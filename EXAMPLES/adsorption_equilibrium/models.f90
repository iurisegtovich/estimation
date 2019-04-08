MODULE MODELS_MOD
!===============================================================================
  CONTAINS
!===============================================================================
  SUBROUTINE MODELS(M,VARENT, VARSAI, PARAM, GUESS)
    INTEGER :: M
    REAL(8), INTENT(IN) :: VARENT(:)
    REAL(8), INTENT(OUT) :: VARSAI(:)
    REAL(8), INTENT(IN) :: GUESS(:)
    REAL(8) :: PARAM(:)
    SELECT CASE(M)
    CASE(1)
      CALL MODEL_A(VARENT, VARSAI, PARAM, GUESS)
    ENDSELECT
  ENDSUBROUTINE
!===============================================================================
  SUBROUTINE MODEL_A(VARENT, VARSAI, PARAM, GUESS)
    REAL(8), INTENT(IN) :: VARENT(:)
    REAL(8), INTENT(OUT) :: VARSAI(:)
    REAL(8), INTENT(IN) :: GUESS(:)
    REAL(8) :: PARAM(:)
    !MODELO
    VARSAI(1)=PARAM(1)* PARAM(2)* VARENT(1) /(1+ PARAM(2)*VARENT(1))
  ENDSUBROUTINE
!===============================================================================
  SUBROUTINE RES(T,Y,YPRIME,DELTA,IRES,RPAR,IPAR)
    !FUNÇÃO RESIDUO PARA O SOLVER ALGEBRICO DIFERENCIAL DASSL
    DOUBLE PRECISION T, Y(*), YPRIME(*), DELTA(*), RPAR(*)
    INTEGER IRES,IPAR(*)
  ENDSUBROUTINE
!===============================================================================
  SUBROUTINE JAC(T,Y,YPRIME,PD,CJ,RPAR,IPAR)
    !FUNÇÃO JACOBIANA PARA O SOLVER ALGEBRICO DIFERENCIAL DASSL
!O FORNECIMENTO DE UMA SUBROTINA PARA CALCULO DA JACOBIANA E NECESSARIO
!POIS A DASSL REQUER LINKAR ESSA ROTINA AO EXECUTÁVEL INDEPENDENTEMENTE DA INTENÇÃO DE USO
!QUE É DEFINIDA EM UMA DAS VARIÁVEIS INFO DELA
  real(8) T, Y(*), PD(:,:), CJ, YPRIME(*), RPAR(*)
  INTEGER IPAR(*)
!NO CASO DE USAR INFO INDICANDO QUE ELA NÃO VAI SER CHAMADA
!ELA DEVE SER DECLARADA AQUI MAS PODE FICAR SEM IMPLEMENTAÇÃO
  END SUBROUTINE
!===============================================================================
ENDMODULE
