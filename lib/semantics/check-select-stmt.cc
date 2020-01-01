//==check-select-stmt.cc - Checker for select-rank
// TODO:select-case
// TODO:select-type
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===--------------------------------------------------------------------------------------===

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
  std::visit(
      common::visitors{
          [](const parser::Expr &) {
            // TODO:1.Checks if selector is an
            // expression ex.Rank(a+b)
          },
          [&](const parser::Variable &variable) {
            if (const Symbol * entity{GetLastName(variable).symbol}) {
              if (!IsAssumedRankArray(*entity)) {  // C1150
                context_.Say(variable.GetSource(),
                    "Selector is not an assumed-rank array variable"_err_en_US);
              }
            }
          },
      },
      selectRankStmtSel.u);
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
                    "statement "
                    "may be default"_err_en_US);
              }
            },
            [&](const parser::Star &) {  // C1153
              if (!starRankFound) {
                starRankFound = true;
              } else {
                context_.Say(rankCaseStmt.source,
                    "Not more than one of the selectors of SELECT RANK "
                    "statement "
                    "may be '*'"_err_en_US);
              }
              std::visit(
                  common::visitors{
                      [](const parser::Expr &) {
                        // TODO:1.Checks if selector is an
                        // expression ex.Rank(a+b)
                      },
                      [&](const parser::Variable &variable) {
                        // C1155 RANK (*) not allowed if the selector has the
                        // ALLOCATABLE or POINTER attribute
                        if (const Symbol *
                            entity{GetLastName(variable).symbol}) {
                          if (IsAllocatableOrPointer(*entity)) {
                            context_.Say(variable.GetSource(),
                                "RANK (*) cannot be used when selector is "
                                "POINTER or ALLOCATABLE"_err_en_US);
                          }
                        }
                      },
                  },
                  selectRankStmtSel.u);
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
            [](auto &) {
              common::die("Illegal value of selector in SELECT RANK case "
                          "statement");
            },
        },
        rank.u);
  }
}

}
