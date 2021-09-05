/*
 * Copyright 2020 Docspell Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package docspell.backend.signup

import docspell.common._

final case class ExternalAccount(
    collName: Ident,
    login: Ident,
    source: AccountSource
) {

  def toAccountId: AccountId =
    AccountId(collName, login)
}

object ExternalAccount {
  def apply(accountId: AccountId): ExternalAccount =
    ExternalAccount(accountId.collective, accountId.user, AccountSource.OpenId)

}
