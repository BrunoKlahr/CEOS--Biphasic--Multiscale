        !COMPILER-GENERATED INTERFACE MODULE: Thu Nov 19 14:11:06 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SOLVECONSTITUTIVEMODEL__genmod
          INTERFACE 
            SUBROUTINE SOLVECONSTITUTIVEMODEL(ELEMENTLIST,              &
     &ANALYSISSETTINGS,TIME,U,STATUS)
              USE MODNODES
              USE MODELEMENTLIBRARY
              TYPE (CLASSELEMENTSWRAPPER) :: ELEMENTLIST(:)
              TYPE (CLASSANALYSIS) :: ANALYSISSETTINGS
              REAL(KIND=8) :: TIME
              REAL(KIND=8) :: U(:)
              TYPE (CLASSSTATUS) :: STATUS
            END SUBROUTINE SOLVECONSTITUTIVEMODEL
          END INTERFACE 
        END MODULE SOLVECONSTITUTIVEMODEL__genmod
