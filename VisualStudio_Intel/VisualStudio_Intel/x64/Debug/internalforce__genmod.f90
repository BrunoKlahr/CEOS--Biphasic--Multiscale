        !COMPILER-GENERATED INTERFACE MODULE: Fri Nov 20 10:47:45 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE INTERNALFORCE__genmod
          INTERFACE 
            SUBROUTINE INTERNALFORCE(ELEMENTLIST,ANALYSISSETTINGS,FINT, &
     &STATUS)
              USE MODANALYSIS
              USE MODNODES
              USE MODELEMENTLIBRARY
              TYPE (CLASSELEMENTSWRAPPER) :: ELEMENTLIST(:)
              TYPE (CLASSANALYSIS) :: ANALYSISSETTINGS
              REAL(KIND=8) :: FINT(:)
              TYPE (CLASSSTATUS) :: STATUS
            END SUBROUTINE INTERNALFORCE
          END INTERFACE 
        END MODULE INTERNALFORCE__genmod
