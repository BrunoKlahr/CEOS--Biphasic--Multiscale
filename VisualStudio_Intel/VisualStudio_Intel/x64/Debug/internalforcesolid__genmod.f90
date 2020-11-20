        !COMPILER-GENERATED INTERFACE MODULE: Fri Nov 20 10:47:44 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE INTERNALFORCESOLID__genmod
          INTERFACE 
            SUBROUTINE INTERNALFORCESOLID(ELEMENTLIST,ANALYSISSETTINGS,P&
     &,FINT,STATUS)
              USE MODANALYSIS
              USE MODNODES
              USE MODELEMENTLIBRARY
              TYPE (CLASSELEMENTSWRAPPER) :: ELEMENTLIST(:)
              TYPE (CLASSANALYSIS) :: ANALYSISSETTINGS
              REAL(KIND=8) :: P(:)
              REAL(KIND=8) :: FINT(:)
              TYPE (CLASSSTATUS) :: STATUS
            END SUBROUTINE INTERNALFORCESOLID
          END INTERFACE 
        END MODULE INTERNALFORCESOLID__genmod
