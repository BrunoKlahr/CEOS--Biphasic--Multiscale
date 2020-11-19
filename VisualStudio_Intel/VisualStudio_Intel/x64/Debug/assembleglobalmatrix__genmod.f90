        !COMPILER-GENERATED INTERFACE MODULE: Thu Nov 19 14:11:04 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ASSEMBLEGLOBALMATRIX__genmod
          INTERFACE 
            SUBROUTINE ASSEMBLEGLOBALMATRIX(GM,KE,KG)
              USE MODGLOBALSPARSEMATRIX
              INTEGER(KIND=4), INTENT(IN) :: GM(:)
              REAL(KIND=8), INTENT(IN) :: KE(:,:)
              TYPE (CLASSGLOBALSPARSEMATRIX) :: KG
            END SUBROUTINE ASSEMBLEGLOBALMATRIX
          END INTERFACE 
        END MODULE ASSEMBLEGLOBALMATRIX__genmod
