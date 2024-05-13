/*
 * Copyright 2020 Eike K. & Contributors
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

package docspell.joex.process

import cats.effect._
import cats.implicits._
import fs2.io.file.Files

import docspell.addons.AddonTriggerType
import docspell.analysis.TextAnalyser
import docspell.backend.joex.AddonOps
import docspell.backend.ops.OItem
import docspell.common.ProcessItemArgs
import docspell.ftsclient.FtsClient
import docspell.joex.Config
import docspell.joex.analysis.RegexNerFile
import docspell.scheduler.Task
import docspell.store.Store

object ProcessItem {

  def apply[F[_]: Async: Files](
      cfg: Config,
      itemOps: OItem[F],
      fts: FtsClient[F],
      analyser: TextAnalyser[F],
      regexNer: RegexNerFile[F],
      addonOps: AddonOps[F],
      store: Store[F]
  )(item: ItemData): Task[F, ProcessItemArgs, ItemData] = {
    implicit val afh: AttachmentFailureHandling[F] =
      new AttachmentFailureHandling(addonOps, store)
    ExtractArchive(store)(item)
      .flatMap(Task.setProgress(20))
      .flatMap(
        processAttachments0(cfg, afh, fts, analyser, regexNer, store, (40, 60, 80))
      )
      .flatMap(LinkProposal.onlyNew[F](store))
      .flatMap(SetGivenData.onlyNew[F](itemOps))
      .flatMap(Task.setProgress(99))
      .flatMap(RemoveEmptyItem(itemOps))
      .flatMap(RunAddons(addonOps, store, AddonTriggerType.FinalProcessItem))
  }

  def processAttachments[F[_]: Async: Files](
      cfg: Config,
      afh: AttachmentFailureHandling[F],
      fts: FtsClient[F],
      analyser: TextAnalyser[F],
      regexNer: RegexNerFile[F],
      store: Store[F]
  )(item: ItemData): Task[F, ProcessItemArgs, ItemData] =
    processAttachments0[F](cfg, afh, fts, analyser, regexNer, store, (30, 60, 90))(item)

  def analysisOnly[F[_]: Async: Files](
      cfg: Config,
      analyser: TextAnalyser[F],
      regexNer: RegexNerFile[F],
      store: Store[F]
  )(item: ItemData): Task[F, ProcessItemArgs, ItemData] =
    TextAnalysis[F](cfg.textAnalysis, analyser, regexNer, store)(item)
      .flatMap(FindProposal[F](cfg.textAnalysis, store))
      .flatMap(EvalProposals[F](store))
      .flatMap(CrossCheckProposals[F](store))
      .flatMap(SaveProposals[F](store))

  private def processAttachments0[F[_]: Async: Files](
      cfg: Config,
      afh: AttachmentFailureHandling[F],
      fts: FtsClient[F],
      analyser: TextAnalyser[F],
      regexNer: RegexNerFile[F],
      store: Store[F],
      progress: (Int, Int, Int)
  )(item: ItemData): Task[F, ProcessItemArgs, ItemData] =
    ConvertPdf(afh, cfg.convert, store, item)
      .flatMap(Task.setProgress(progress._1))
      .flatMap(TextExtraction(afh, cfg.extraction, fts, store))
      .flatMap(AttachmentPreview(cfg.extraction.preview, store))
      .flatMap(AttachmentPageCount(store))
      .flatMap(Task.setProgress(progress._2))
      .flatMap(analysisOnly[F](cfg, analyser, regexNer, store))
      .flatMap(Task.setProgress(progress._3))
      .flatMap(failOnPartialFailure[F])

  def failOnPartialFailure[F[_]: Sync](
      item: ItemData
  ): Task[F, ProcessItemArgs, ItemData] = Task { ctx =>
    ctx.logger.debug(s"item.errors: ${item.errors}")
    item.errors match {
      case Seq()  => item.pure[F]
      case Seq(e) => throw e
      case manyErrors =>
        throw new RuntimeException(
          s"Multiple exceptions (${manyErrors.size}) when processing item:\n" + manyErrors
            .mkString("\n")
        )
    }
  }
}
