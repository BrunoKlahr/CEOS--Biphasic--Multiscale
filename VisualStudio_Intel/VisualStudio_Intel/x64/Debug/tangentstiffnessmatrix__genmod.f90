        !COMPILER-GENERATED INTERFACE MODULE: Thu Nov 19 14:11:03 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE TANGENTSTIFFNESSMATRIX__genmod
          INTERFACE 
            SUBROUTINE TANGENTSTIFFNESSMATRIX(ANALYSISSETTINGS,         &
     &ELEMENTLIST,NDOF,KG)
              USE MODGLOBALSPARSEMATRIX
              USE MODNODES
              USE MODELEMENTLIBRARY
              USE MODANALYSIS
              TYPE (CLASSANALYSIS), INTENT(INOUT) :: ANALYSISSETTINGS
              TYPE (CLASSELEMENTSWRAPPER), INTENT(IN) :: ELEMENTLIST(:)
              INTEGER(KIND=4) :: NDOF
              TYPE (CLASSGLOBALSPARSEMATRIX), INTENT(IN) :: KG
            END SUBROUTINE TANGENTSTIFFNESSMATRIX
          END INTERFACE 
        END MODULE TANGENTSTIFFNESSMATRIX__genmod
