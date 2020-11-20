        !COMPILER-GENERATED INTERFACE MODULE: Fri Nov 20 10:47:44 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NODEIDFLUIDCONSTRUCTOR__genmod
          INTERFACE 
            SUBROUTINE NODEIDFLUIDCONSTRUCTOR(ELEMENTLIST,              &
     &GLOBALNODESLIST)
              USE MODELEMENTLIBRARY
              TYPE (CLASSELEMENTSWRAPPER) ,POINTER :: ELEMENTLIST(:)
              TYPE (CLASSNODES) ,POINTER :: GLOBALNODESLIST(:)
            END SUBROUTINE NODEIDFLUIDCONSTRUCTOR
          END INTERFACE 
        END MODULE NODEIDFLUIDCONSTRUCTOR__genmod
