        !COMPILER-GENERATED INTERFACE MODULE: Thu Nov 19 14:11:04 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE TANGENTSTIFFNESSMATRIXSOLID__genmod
          INTERFACE 
            SUBROUTINE TANGENTSTIFFNESSMATRIXSOLID(ANALYSISSETTINGS,    &
     &ELEMENTLIST,P,KG)
              USE MODGLOBALSPARSEMATRIX
              USE MODNODES
              USE MODELEMENTLIBRARY
              USE MODANALYSIS
              TYPE (CLASSANALYSIS), INTENT(INOUT) :: ANALYSISSETTINGS
              TYPE (CLASSELEMENTSWRAPPER), INTENT(IN) :: ELEMENTLIST(:)
              REAL(KIND=8) :: P(:)
              TYPE (CLASSGLOBALSPARSEMATRIX), INTENT(IN) :: KG
            END SUBROUTINE TANGENTSTIFFNESSMATRIXSOLID
          END INTERFACE 
        END MODULE TANGENTSTIFFNESSMATRIXSOLID__genmod