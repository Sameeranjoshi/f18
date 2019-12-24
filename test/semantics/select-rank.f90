!===--- select-rank.f90 - Test select rank constraints -------------------===
!
! Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
! See https://llvm.org/LICENSE.txt for license information.
! SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
!
!===------------------------------------------------------------------------===

program selectRankProg
implicit none
   ! local variable declaration
   integer, DIMENSION(3) :: array

   call CALL_ME(array)
   call CALL_ME2(array)
   call CALL_ME3(array)
   call CALL_ME4(array)
   call CALL_ME5(array)
   call CALL_ME6(array)
   call CALL_ME7(array)
   call CALL_ME8(array)
   call CALL_ME9(array)
   contains

   !No error expected
   subroutine CALL_ME(x)
    integer :: x(..)
    SELECT RANK(x)
    RANK (0)
      print *, "PRINT RANK 0"
    RANK (1)
      print *, "PRINT RANK 1"
    END SELECT
   end

   subroutine CALL_ME9(x)
    integer :: x(..)
    boo: SELECT RANK(x)
    RANK (1+0)
      print *, "PRINT RANK 1"
    END SELECT boo
   end subroutine

   !Error expected
   subroutine CALL_ME2(x)
    integer :: x(..)
    integer :: y(3)
    !ERROR: Selector is not an assumed rank array variable
    SELECT RANK(y)
    RANK (0)
      print *, "PRINT RANK 0"
    RANK (1)
      print *, "PRINT RANK 1"
     END SELECT
   end

   subroutine CALL_ME3(x)
    integer :: x(..)
    SELECT RANK(x)
    !ERROR: The value of the selector must be between zero and 15
    RANK (16)
      print *, "PRINT RANK 0"
    END SELECT
   end subroutine

   subroutine CALL_ME4(x)
    integer :: x(..)
    SELECT RANK(x)
    RANK DEFAULT
      print *, "ok "
    !ERROR: Not more than one of the selectors of select rank statement must be default
    RANK DEFAULT
      print *, "not ok"
    RANK (3)
      print *, "IT'S 3"
    END SELECT
   end subroutine

   subroutine CALL_ME5(x)
    integer :: x(..)
    SELECT RANK(x)
    RANK (0)
      print *, "PRINT RANK 0"
    RANK(1)
      print *, "PRINT RANK 1"
    !ERROR: Same rank values not allowed more than once
    RANK(0)
      print *, "ERROR"
    END SELECT
   end subroutine

   subroutine CALL_ME6(x)
    integer :: x(..)    !ASSUMED RANK ARRAY should be a dummy argument for subrotine
    SELECT RANK(x)
    RANK (3)
      print *, "one"
    !ERROR: The value of the selector must be between zero and 15
    RANK(-1)
      print *, "rank: -ve"
    END SELECT
   end subroutine

   subroutine CALL_ME7(arg)
   integer :: i
   integer, dimension(..), pointer :: arg
   !ERROR: RANK (*) cannot be used when selector has pointer or allocatable
   select RANK(arg)
   RANK (*)
      print *, arg(1:1)
   RANK (1)
      print *, arg
   end select
   end subroutine

   subroutine CALL_ME8(x)
    integer :: x(..)
    SELECT RANK(x)
    Rank(2)
      print *, "Merry Christmas"
    RANK (*)
      print *, "Happy New Year"
    !ERROR: Not more than one of the selectors of select rank statementmust be '*'
    RANK (*)
      print *, "Opps!! Wrong"
    END SELECT
   end subroutine

end program selectRankProg
