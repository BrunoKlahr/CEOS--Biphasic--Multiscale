        !COMPILER-GENERATED INTERFACE MODULE: Fri Nov 20 10:47:44 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE EXTERNALFORCEMULTISCALEMINIMALLINEARD1__genmod
          INTERFACE 
            SUBROUTINE EXTERNALFORCEMULTISCALEMINIMALLINEARD1(          &
     &ELEMENTLIST,ANALYSISSETTINGS,LAMBDA_F,LAMBDA_U,FEXT)
              USE MODANALYSIS
              USE MODNODES
              USE MODELEMENTLIBRARY
              TYPE (CLASSELEMENTSWRAPPER) :: ELEMENTLIST(:)
              TYPE (CLASSANALYSIS) :: ANALYSISSETTINGS
              REAL(KIND=8) :: LAMBDA_F(:)
              REAL(KIND=8) :: LAMBDA_U(:)
              REAL(KIND=8) :: FEXT(:)
            END SUBROUTINE EXTERNALFORCEMULTISCALEMINIMALLINEARD1
          END INTERFACE 
        END MODULE EXTERNALFORCEMULTISCALEMINIMALLINEARD1__genmod
