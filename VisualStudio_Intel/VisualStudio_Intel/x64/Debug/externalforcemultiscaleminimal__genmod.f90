        !COMPILER-GENERATED INTERFACE MODULE: Fri Nov 20 10:47:44 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE EXTERNALFORCEMULTISCALEMINIMAL__genmod
          INTERFACE 
            SUBROUTINE EXTERNALFORCEMULTISCALEMINIMAL(ELEMENTLIST,      &
     &ANALYSISSETTINGS,LAMBDA_F,LAMBDA_U,FEXT)
              USE MODANALYSIS
              USE MODNODES
              USE MODELEMENTLIBRARY
              TYPE (CLASSELEMENTSWRAPPER) :: ELEMENTLIST(:)
              TYPE (CLASSANALYSIS) :: ANALYSISSETTINGS
              REAL(KIND=8) :: LAMBDA_F(:)
              REAL(KIND=8) :: LAMBDA_U(:)
              REAL(KIND=8) :: FEXT(:)
            END SUBROUTINE EXTERNALFORCEMULTISCALEMINIMAL
          END INTERFACE 
        END MODULE EXTERNALFORCEMULTISCALEMINIMAL__genmod
