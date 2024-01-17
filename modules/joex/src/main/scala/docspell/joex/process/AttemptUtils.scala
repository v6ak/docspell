/*
 * Copyright 2020 Eike K. & Contributors
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

package docspell.joex.process

import cats.effect._
import cats.implicits._

import docspell.scheduler.Context
import docspell.store.records.RAttachment

object AttemptUtils {
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
