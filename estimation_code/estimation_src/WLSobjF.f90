MODULE OBJF_MOD
!===============================================================================
! MODULO PARA CALCULO DA FUNÇÃO OBJETIVO DE MINIMOS QUADRADOS PONDERADOS POR VARIÂNCIA EXPERIMENTAL
! SCHWAAB E PINTO, ANÁLISE DE DADOS EXPERIMENTAIS I
! FUNDAMENTOS DE ESTATÍSTICA E ESTIMAÇÃO DE PARÂMETROS
! EPAPERS, RIO DE JANEIRO,2007
! SEÇÃO 4.5: A DEFINIÇÃ DA FUNÇÃO OBJETIVO
! EQUAÇÃO 4.75, 5.2
!===============================================================================
  USE GLOBAL_MOD
!===============================================================================
CONTAINS
!===============================================================================
  SUBROUTINE OBJF(FOBJ)
    REAL(8), INTENT(OUT) :: FOBJ ! VALOR DA FUNÇÃO OBJETIVO
    INTEGER :: M !CONTADOR MODELOS
    INTEGER :: E !CONTADOR EXPERIMENTOS
    !ACESSO AS GLOBAIS VALORES DE XE, YE, PARAM, MEMORIA ALOCADA DE YC, EY
    ! AVALIAÇÃO DE CADA MODELO M PARA CADA EXPERIMENTO E
    DO M = 1, NMOD
      DO E=1,NEXP(M)
        !CHAMADA DOS MODELOS PELO PONTEIRO GLOBAL
        CALL PMODELS( M=M, VENT=XE(M)%M(E,:), VSAI=YC(M)%M(E,:), PARS=PARAM, GUESS=YE(M)%M(E,:) )
        ! TODO TRANSFORMAR PMODELS EM FUNÇÃO
      ENDDO
    ENDDO
    ! CÁLCULO DA FUNÇÃO OBJETIVO
    FOBJ = 0.D0
    DO M = 1, NMOD
      ! ERROS DAS VARIÁVEIS DE SAIDA
      EY(M)%M(:,:) = YC(M)%M(:,:) - YE(M)%M(:,:)
      DO E=1,NEXP(M)
        ! NOTAÇÃO MATRICIAL EQ. 4.75
        FOBJ = FOBJ + DOT_PRODUCT(EY(M)%M(E,:),MATMUL(VARYEINV(M)%M(E,:,:),EY(M)%M(E,:)))
      ENDDO
    ENDDO
  ENDSUBROUTINE
!===============================================================================
ENDMODULE
