        !COMPILER-GENERATED INTERFACE MODULE: Fri Nov 20 10:47:45 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ASSEMBLEGLOBALMATRIXUPPERTRIANGULAR__genmod
          INTERFACE 
            SUBROUTINE ASSEMBLEGLOBALMATRIXUPPERTRIANGULAR(GM,KE,KG)
              USE MODGLOBALSPARSEMATRIX
              INTEGER(KIND=4), INTENT(IN) :: GM(:)
              REAL(KIND=8), INTENT(IN) :: KE(:,:)
              TYPE (CLASSGLOBALSPARSEMATRIX) :: KG
            END SUBROUTINE ASSEMBLEGLOBALMATRIXUPPERTRIANGULAR
          END INTERFACE 
        END MODULE ASSEMBLEGLOBALMATRIXUPPERTRIANGULAR__genmod
