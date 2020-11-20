        !COMPILER-GENERATED INTERFACE MODULE: Fri Nov 20 10:47:43 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE UPDATEMESHCOORDINATES__genmod
          INTERFACE 
            SUBROUTINE UPDATEMESHCOORDINATES(GLOBALNODESLIST,           &
     &ANALYSISSETTINGS,U)
              USE MODANALYSIS
              USE MODNODES
              TYPE (CLASSNODES) ,POINTER :: GLOBALNODESLIST(:)
              TYPE (CLASSANALYSIS) :: ANALYSISSETTINGS
              REAL(KIND=8) :: U(:)
            END SUBROUTINE UPDATEMESHCOORDINATES
          END INTERFACE 
        END MODULE UPDATEMESHCOORDINATES__genmod
