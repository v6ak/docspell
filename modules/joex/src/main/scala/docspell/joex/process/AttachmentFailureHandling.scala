/*
 * Copyright 2020 Eike K. & Contributors
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

package docspell.joex.process

import cats.effect._
import cats.implicits._
import fs2.io.file.{Files, Path}

import docspell.addons.{AddonResult, AddonTriggerType}
import docspell.backend.joex.AddonOps
import docspell.common.syntax.all.PathOps
import docspell.joex.addon.GenericItemAddonTask
import docspell.joex.process.AttachmentFailureHandling.{onlyErrors, onlySuccesses}
import docspell.scheduler.Context
import docspell.store.Store
import docspell.store.records.RAttachment

class AttachmentFailureHandling[F[_]: Async: Files](ops: AddonOps[F], store: Store[F]) {

  def attemptTraverseAttachmentsWithFallback[O](
      o: Any,
      ctx: Context[F, _],
      itemData: ItemData
  )(
      f: RAttachment => F[O]
  ): F[(Vector[O], Seq[Throwable])] =
    itemData.attachments
      .traverse(
        attemptAttachmentWithReplacements(o, itemData, ctx)(f)
      )
      .map(all => (onlySuccesses(all), onlyErrors(all)))

  private def attemptAttachmentLastTime[O](
      o: Any
  )(f: RAttachment => F[O]): RAttachment => F[Either[Throwable, O]] =
    ra =>
      f(ra).attempt
        .map(
          _.leftMap(e =>
            new RuntimeException(
              s"Error when processing attachment ${ra.name} in ${o.getClass.getName}: ${e.getMessage}; replacement by addon(s) didn't make it successful.",
              e
            )
          )
        )

  private def genAlternativeAttachment(itemData: ItemData, ctx: Context[F, _])(
      attachment: RAttachment
  ) = {
    val collective = itemData.item.cid
    val failedAttachmentFile = Path("item") / "originals" / attachment.id.id
    val addonRunFs = GenericItemAddonTask.addonResults(
      ops,
      store,
      AddonTriggerType.FailedAttachment,
      Set.empty
    )(
      collective = collective,
      data = itemData,
      maybeMeta = none, // ctx.args.meta.some
      env = Map(
        "FAILED_ATTACHMENT_FILE" -> failedAttachmentFile.toString,
        "FAILED_ATTACHMENT_NAME" -> attachment.name.getOrElse("")
      )
    )

    addonRunFs
      .run(ctx.unit)
      .flatMap(addonTasks =>
        addonTasks
          .map(processAddonResult(ctx, attachment, _))
          .findM(_.map(_.isDefined)) // stop when first addon provides an alternative
      )
      .flatMap(_.getOrElse(none[String].pure[F]))
      .flatMap(
        _.fold(false.pure[F])(replaceAttachment(ctx, attachment, _))
      )
  }

  private def replaceAttachment(
      ctx: Context[F, _],
      attachment: RAttachment,
      newFile: String
  ) =
    Path(newFile).readAll
      .through(store.fileRepo.overwrite(attachment.fileId))
      .compile
      .lastOrError
      .attempt
      .flatMap {
        case Left(e) =>
          ctx.logger.error(e)(s"Failed to store alternative file $newFile") *>
            e.asLeft[Unit].pure[F]
        case Right(res) =>
          ctx.logger.info(s"Alternative file stored: $res") *>
            ().asRight[Throwable].pure[F]
      }
      .map {
        case Left(e: Throwable) => throw e
        case Right(_)           => true
      }

  private def processAddonResult(
      ctx: Context[F, _],
      attachment: RAttachment,
      addonResultF: F[AddonOps.ExecResult]
  ) =
    addonResultF.flatMap(er =>
      er.combined.addonResult match {
        case AddonResult.Success(addonOut) =>
          ctx.logger.info(
            s"Addon that handles failures has provided ${if (addonOut.attachmentFix.isDefined) "an"
              else "no"} alternative for failed attachment ${attachment.name}: $addonOut"
          ) *>
            addonOut.attachmentFix.pure[F]
        case failure =>
          throw new RuntimeException(
            s"Addon that handles failures has failed for attachment ${attachment.name}: $failure"
          )
      }
    )

  private def attemptAttachmentWithReplacements[O](
      o: Any,
      itemData: ItemData,
      ctx: Context[F, _]
  )(f: RAttachment => F[O]): RAttachment => F[Either[Throwable, O]] =
    ra =>
      f(ra).attempt
        .map(
          _.leftMap(e =>
            new RuntimeException(
              s"Error when processing attachment ${ra.name} in ${o.getClass.getName}: ${e.getMessage}",
              e
            )
          )
        )
        .flatMap {
          case Left(e) =>
            ctx.logger.warn(
              s"Attachment failed to process, will try to recover using addons: $e"
            ) *>
              genAlternativeAttachment(itemData, ctx)(ra).flatMap {
                case false =>
                  ctx.logger.warn(
                    s"Attachment failed to process, no addon has provided any alternative: $e"
                  ) *>
                    (e: Throwable).asLeft[O].pure[F]
                case true =>
                  ctx.logger.warn(
                    s"Attachment failed to process, some addon has provided an alternative"
                  ) *>
                    attemptAttachmentLastTime(o)(f)(ra)
              }
          case Right(res) => res.asRight[Throwable].pure[F]
        }

}

object AttachmentFailureHandling {

  def attemptTraverseAttachments[F[_]: Sync, O](o: Any, item: ItemData)(
      f: RAttachment => F[O]
  ): F[(Vector[O], Seq[Throwable])] =
    item.attachments
      .traverse(attemptAttachment(o)(f))
      .map(all => (onlySuccesses(all), onlyErrors(all)))

  def traverseAttachmentsFailsafe[F[_]: Sync, O](
      actionName: String,
      ctx: Context[F, _],
      o: Any,
      item: ItemData
  )(
      f: RAttachment => F[O]
  ): F[Vector[O]] = {
    def reportErrors(errs: Seq[Throwable]): F[Unit] = errs match {
      case Seq() => ().pure[F]
      case Seq(e) =>
        ctx.logger.error(e)(
          s"$actionName failed, continuing without it."
        )
      case manyErrors =>
        ctx.logger.error(
          s"$actionName failed for multiple (${manyErrors.size}) attachments, continuing without them:\n" + manyErrors
            .mkString("\n")
        )
    }

    for {
      _ <- ctx.logger.info(
        s"$actionName for ${item.attachments.size} files…"
      )
      (results, errs) <- attemptTraverseAttachments(o, item)(f)
      _ <- reportErrors(errs)
    } yield results
  }

  /** Essentially like function.attempt, but with extra details when it fails.
    *
    * @param o
    *   The object that processes the attachments. We use it just for extracting its class
    *   name in case of failure.
    * @param f
    *   function to decorate
    */
  def attemptAttachment[F[_]: Sync, O](
      o: Any
  )(f: RAttachment => F[O]): RAttachment => F[Either[Throwable, O]] =
    ra =>
      f(ra).attempt
        .map(
          _.leftMap(e =>
            new RuntimeException(
              s"Error when processing attachment ${ra.name} in ${o.getClass.getName}: ${e.getMessage}",
              e
            )
          )
        )
  def onlyErrors[T](s: Seq[Either[Throwable, T]]): List[Throwable] =
    s.flatMap(_.left.toOption).toList
  def onlySuccesses[T](s: Vector[Either[Throwable, T]]): Vector[T] = s.flatMap(_.toOption)
}
