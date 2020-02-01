//===-- lib/semantics/check-select-stmt.cc --------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "check-select-stmt.h"
#include "tools.h"
#include "../common/Fortran.h"
#include "../common/idioms.h"
#include "../parser/message.h"
#include "../parser/tools.h"
#include <list>
#include <optional>
#include <set>
#include <tuple>
#include <variant>

namespace Fortran::semantics {

void SelectStmtChecker::Leave(
    const parser::SelectRankConstruct &selectRankConstruct) {
  const auto &selectRankStmt{
      std::get<parser::Statement<parser::SelectRankStmt>>(
          selectRankConstruct.t)};
  const auto &selectRankStmtSel{
      std::get<parser::Selector>(selectRankStmt.statement.t)};

  // R1149 select-rank-stmt checks
  const Symbol *saveSelSymbol{nullptr};
  if (const auto resolvedSel{ResolveSelector(selectRankStmtSel)}) {
    if (const Symbol * sel{evaluate::UnwrapWholeSymbolDataRef(*resolvedSel)}) {
      if (!IsAssumedRankArray(*sel)) {
        context_.Say(parser::FindSourceLocation(selectRankStmtSel),
            "Selector is not an assumed-rank array variable"_err_en_US);
      } else {
        saveSelSymbol = sel;
      }
    } else {
      context_.Say(parser::FindSourceLocation(selectRankStmtSel),
          "Selector is not an assumed-rank array variable"_err_en_US);
    }
  }

  // R1150 select-rank-case-stmt checks
  auto &rankCaseList{std::get<std::list<parser::SelectRankConstruct::RankCase>>(
      selectRankConstruct.t)};
  bool defaultRankFound{false};
  bool starRankFound{false};
  std::set<std::int64_t> matches;

  for (const auto &rankCase : rankCaseList) {
    const auto &rankCaseStmt{
        std::get<parser::Statement<parser::SelectRankCaseStmt>>(rankCase.t)};
    const auto &rank{
        std::get<parser::SelectRankCaseStmt::Rank>(rankCaseStmt.statement.t)};
    std::visit(
        common::visitors{
            [&](const parser::Default &) {  // C1153
              if (!defaultRankFound) {
                defaultRankFound = true;
              } else {
                context_.Say(rankCaseStmt.source,
                    "Not more than one of the selectors of SELECT RANK "
                    "statement may be default"_err_en_US);
              }
            },
            [&](const parser::Star &) {  // C1153
              if (!starRankFound) {
                starRankFound = true;
              } else {
                context_.Say(rankCaseStmt.source,
                    "Not more than one of the selectors of SELECT RANK "
                    "statement may be '*'"_err_en_US);
              }
              if (IsAllocatableOrPointer(*saveSelSymbol)) {  // C1155
                context_.Say(parser::FindSourceLocation(selectRankStmtSel),
                    "RANK (*) cannot be used when selector is "
                    "POINTER or ALLOCATABLE"_err_en_US);
              }
            },
            [&](const parser::ScalarIntConstantExpr &init) {
              if (auto val{GetIntValue(init)}) {
                if (*val < 0 || *val > common::maxRank) {  // C1151
                  context_.Say(rankCaseStmt.source,
                      "The value of the selector must be "
                      "between zero and %d"_err_en_US,
                      common::maxRank);
                }
                auto resultPair{matches.insert(*val)};
                if (!resultPair.second) {  // C1152
                  context_.Say(rankCaseStmt.source,
                      "Same rank values not allowed more than once"_err_en_US);
                }
              }
            },
        },
        rank.u);
  }
}

const SomeExpr *SelectStmtChecker::ResolveSelector(
    const parser::Selector &selector) {
  return std::visit(
      common::visitors{
          [&](const parser::Expr &expr) { return GetExpr(expr); },
          [&](const parser::Variable &var) { return GetExpr(var); },
      },
      selector.u);
}

}
