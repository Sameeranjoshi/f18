//==check-select-stmt.cc - Checker for select-case, select-rank
//TODO:select-type
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===--------------------------------------------------------------------------------------===

#include "check-select-stmt.h"
#include "tools.h"
#include "../include/flang/ISO_Fortran_binding.h"
#include "../parser/message.h"
#include "../parser/tools.h"
#include <list>
#include <optional>
#include <tuple>
#include <vector>

namespace Fortran::semantics {

void SelectStmtChecker::Leave(
    const parser::SelectRankConstruct &selectRankConstruct) {
  const auto &selectRankStmt{
      std::get<parser::Statement<parser::SelectRankStmt>>(
          selectRankConstruct.t)};
  const auto &selectRankStmtSel{
      std::get<parser::Selector>(selectRankStmt.statement.t)};
  // R1149 select-rank-stmt checks
  if (std::holds_alternative<parser::Expr>(selectRankStmtSel.u)) {
	  shdkfdlfld
    // TODO:
    // 1.Checks if selector is an expression ex.Rank(a+b)
    //
  } else if (std::holds_alternative<parser::Variable>(selectRankStmtSel.u)) {
    const auto &variable{std::get<parser::Variable>(selectRankStmtSel.u)};
    if (const Symbol * entity{GetLastName(variable).symbol}) {
      if (!IsAssumedRankArray(*entity)) {//C1150
        context_.Say(variable.GetSource(),
            "Selector is not an Assumed-rank array variable"_err_en_US);
      }
    }
  }

  // R1150 select-rank-case-stmt checks
  auto &rankCaseList{std::get<std::list<parser::SelectRankConstruct::RankCase>>(
      selectRankConstruct.t)};
  bool defaultRankFound{false};
  bool starRankFound{false};
  std::vector<std::optional<std::int64_t>> matches;

  for (const auto &rankCase : rankCaseList) {
    const auto &rankCaseStmt{
        std::get<parser::Statement<parser::SelectRankCaseStmt>>(rankCase.t)};
    const auto &rank{
        std::get<parser::SelectRankCaseStmt::Rank>(rankCaseStmt.statement.t)};

    // C1153 At most one RANK DEFAULT select-rank-case-stmt allowed
    if (std::holds_alternative<parser::Default>(rank.u)) {
      if (!defaultRankFound) {
        defaultRankFound = true;
      } else {
        context_.Say(rankCaseStmt.source,
            "Not more than one of the selectors of select rank statement "
            "may be default"_err_en_US);
      }
    }  // C1153 At most one RANK (*) select-rank-case-stmt allowed
    else if (std::holds_alternative<parser::Star>(rank.u)) {
      if (!starRankFound) {
        starRankFound = true;
      } else {
        context_.Say(rankCaseStmt.source,
            "Not more than one of the selectors of select rank statement "
            "may be '*'"_err_en_US);
      }
      if (std::holds_alternative<parser::Expr>(selectRankStmtSel.u)) {
        // TODO:1.Checks if selector is an expression ex.Rank(a+b)
      }
      // C1155 RANK (*) not allowed if the selector has the ALLOCATABLE or
      // POINTER attribute
      else if (std::holds_alternative<parser::Variable>(selectRankStmtSel.u)) {
        const auto &variable{std::get<parser::Variable>(selectRankStmtSel.u)};
        if (const Symbol * entity{GetLastName(variable).symbol}) {
          if (IsAllocatableOrPointer(*entity)) {
            context_.Say(variable.GetSource(),
                "RANK (*) cannot be used when selector has "
                "pointer or allocatable "_err_en_US);
          }
        }
      }
    } else if (const auto &init{
                   std::get_if<parser::ScalarIntConstantExpr>(&rank.u)}) {
      if (const auto val{GetIntValue(*init)}) {
        if ((*val < 0) || (*val > CFI_MAX_RANK)) {//C1151
          context_.Say(rankCaseStmt.source,
              "The value of the selector must be "
              "between zero and %d"_err_en_US,
              CFI_MAX_RANK);
        }
        if (std::find(matches.begin(), matches.end(), val) != matches.end()) {//C1152
          context_.Say(rankCaseStmt.source,
              "Same rank values not allowed more than once "_err_en_US);
        } else {
          matches.push_back(val);
        }
      }
    }
  }
}

}  // namespace Fortran::semantics
