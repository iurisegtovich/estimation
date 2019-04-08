MODULE OBJF_MOD
!===============================================================================
! MODULO PARA CALCULO DA FUN��O OBJETIVO DE MINIMOS QUADRADOS PONDERADOS POR VARI�NCIA EXPERIMENTAL
! SCHWAAB E PINTO, AN�LISE DE DADOS EXPERIMENTAIS I
! FUNDAMENTOS DE ESTAT�STICA E ESTIMA��O DE PAR�METROS
! EPAPERS, RIO DE JANEIRO,2007
! SE��O 4.5: A DEFINI�� DA FUN��O OBJETIVO
! EQUA��O 4.75, 5.2
!===============================================================================
  USE GLOBAL_MOD
!===============================================================================
CONTAINS
!===============================================================================
  SUBROUTINE OBJF(FOBJ)
    REAL(8), INTENT(OUT) :: FOBJ ! VALOR DA FUN��O OBJETIVO
    INTEGER :: M !CONTADOR MODELOS
    INTEGER :: E !CONTADOR EXPERIMENTOS
    !ACESSO AS GLOBAIS VALORES DE XE, YE, PARAM, MEMORIA ALOCADA DE YC, EY
    ! AVALIA��O DE CADA MODELO M PARA CADA EXPERIMENTO E
    DO M = 1, NMOD
      DO E=1,NEXP(M)
        !CHAMADA DOS MODELOS PELO PONTEIRO GLOBAL
        CALL PMODELS( M=M, VENT=XE(M)%M(E,:), VSAI=YC(M)%M(E,:), PARS=PARAM, GUESS=YE(M)%M(E,:) )
        ! TODO TRANSFORMAR PMODELS EM FUN��O
      ENDDO
    ENDDO
    ! C�LCULO DA FUN��O OBJETIVO
    FOBJ = 0.D0
    DO M = 1, NMOD
      ! ERROS DAS VARI�VEIS DE SAIDA
      EY(M)%M(:,:) = YC(M)%M(:,:) - YE(M)%M(:,:)
      DO E=1,NEXP(M)
        ! NOTA��O MATRICIAL EQ. 4.75
        FOBJ = FOBJ + DOT_PRODUCT(EY(M)%M(E,:),MATMUL(VARYEINV(M)%M(E,:,:),EY(M)%M(E,:)))
      ENDDO
    ENDDO
  ENDSUBROUTINE
!===============================================================================
ENDMODULE
