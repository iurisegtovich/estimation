MODULE GLOBAL_MOD
!===============================================================================
! MÓDULO COM INTERFACES PARA AS FUNÇÕES ESTATÍSTICAS (GSL),
! DEFINIÇÃO DE ESTRUTURAS DE LISTA DE ARRAYS 2D (MATRIZES) E LISTA DE ARRAYS 3D
! DEFINIÇÃO DE VARIÁVEIS GLOBAIS PARA O CÓDIGO ESTIMATION
! DEFINIÇÃO DE FUNÇÕES AUXILIARES DE CONVERSÃO OU FORMATAÇÃO
! DEFINIÇÃO DE INTERFACE DE MODELOS E PONTEIRO GLOBAL
!===============================================================================
  USE ISO_FORTRAN_ENV, ONLY: INPUT_UNIT, OUTPUT_UNIT, REAL64
!===============================================================================
  INTERFACE !INVERSA DA CHI QUADRADO (ENTRA PROBABILIDADE, SAI VALOR DE CHI QUADRADO LIMITE)
    REAL(C_DOUBLE) FUNCTION DCHIIN(A,B) BIND(C,NAME='gsl_cdf_chisq_Pinv')
      ! TODO insert test here
      USE,INTRINSIC :: ISO_C_BINDING
      REAL(C_DOUBLE),VALUE :: A,B
    ENDFUNCTION
  ENDINTERFACE
!===============================================================================
  INTERFACE !DISTRIBUIÇÃO DE PROBABILIDADES DA CHI QUADRADO (ENTRA VALOR DE CHI QUADRADO TESTE E SAI PROBABILIDADE)
    REAL(C_DOUBLE) FUNCTION DCHIDF(A,B) BIND(C,NAME='gsl_cdf_chisq_P')
      ! TODO insert test here
      USE,INTRINSIC :: ISO_C_BINDING
      REAL(C_DOUBLE),VALUE :: A,B
    ENDFUNCTION
  ENDINTERFACE
!===============================================================================
  INTERFACE !INVERSA DA T STUDENT QUADRADO (ENTRA PROBABILIDADE, SAI VALOR DE T STUDENT LIMITE)
    REAL(C_DOUBLE) FUNCTION DTIN(A,B) BIND(C,NAME='gsl_cdf_tdist_Pinv')
      !DTIN( 0.975d0, 13.0d0)
      !>>> 2.1603686564627922
      !DTIN( 1.0d0-0.975d0, 13.0d0)
      !>>>-2.1603686564627926
      USE,INTRINSIC :: ISO_C_BINDING
      REAL(C_DOUBLE),VALUE :: A,B
    ENDFUNCTION
  ENDINTERFACE
!===============================================================================
  INTERFACE !INVERSA DA FISHER QUADRADO (ENTRA PROBABILIDADE, SAI VALOR DE FISHER LIMITE)
    REAL(C_DOUBLE) FUNCTION DFIN(A,B,C) BIND(C,NAME='gsl_cdf_fdist_P')
      ! TODO insert test here
      USE,INTRINSIC :: ISO_C_BINDING
      REAL(C_DOUBLE),VALUE :: A,B,C
    ENDFUNCTION
  ENDINTERFACE
!===============================================================================
  INTERFACE ! INTERFACE PARA A FUNÇÃO MODELOS DEFININDO O QUE ENTRA E O QUE SAI
    SUBROUTINE IMODELS(M,VENT, VSAI, PARS, GUESS)
      INTEGER, INTENT(IN) :: M !INDICE DE SELECAO DE MODELOS
      REAL(8), INTENT(IN) :: VENT(:) !VETOR DE VARIAVEIS DE ENTRADA
      REAL(8), INTENT(OUT) :: VSAI(:) !VETOR DE VARIAVEIS DE SAIDA
      REAL(8), INTENT(IN) :: PARS(:) !VETOR DE PARAMETROS
      REAL(8), INTENT(IN) :: GUESS(:) !VETOR DE ESTIMATIVAS INICIAIS PARA METODO ITERATIVO
    ENDSUBROUTINE
    ! TODO TRANSFORMAR PMODELS EM FUNÇÃO
  ENDINTERFACE
!===============================================================================
  PROCEDURE(IMODELS), POINTER :: PMODELS => NULL() !PONTEIRO GLOBAL PARA A FUNÇAO MODELOS PARTICULAR DO CASO EM ESTUDO
!===============================================================================
  INTEGER :: NMOD !NUMERO DE MODELOS / BLOCOS DE DADOS EXPERIMENTAIS DIFERENTES
  INTEGER :: NPAR !NUMERO TOTAL DE PARÂMETROS ENVOLVIDOS NOS MODELOS
  INTEGER, ALLOCATABLE :: NENT(:) !NUMERO DE VARIAVEIS DE ENTRADA POR MODELO / BLOCO DE DADOS EXPERIMENTAIS
  INTEGER, ALLOCATABLE :: NSAI(:) !NUMERO DE VARIAVEIS DE SAIDA POR MODELO / BLOCO DE DADOS EXPERIMENTAIS
  INTEGER, ALLOCATABLE :: NEXP(:) !NUMERO DE CONDIÇOES EXPERIMENTAIS POR MODELO / BLOCO DE DADOS EXPERIMENTAIS
  TYPE M2_LIST !TIPIFICAÇÃO DE ELEMENTO PARA LISTA DE MATRIZES
    REAL(8), ALLOCATABLE :: M(:,:) !MATRIZ
  END TYPE
  TYPE(M2_LIST), ALLOCATABLE :: XE(:) !LISTA DE NMOD MATRIZES DE NEXP X NENT DADOS EXPERIMENTAIS DE VARIAVEIS DE ENTRADA
  TYPE M3_LIST !TIPIFICAÇÃO DE ELEMENTO PARA LISTA DE ARRAY DE TRÊS DIMENSÕES
    REAL(8), ALLOCATABLE :: M(:,:,:) !ARRAY DE TRÊS DIMENSÕES
  END TYPE
  TYPE(M2_LIST), ALLOCATABLE :: YE(:) !LISTA DE NMOD MATRIZES DE NEXP X NSAI DADOS EXPERIMENTAIS DE VARIAVEIS DE SAIDA
  TYPE(M2_LIST), ALLOCATABLE :: YC(:) !LISTA DE NMOD MATRIZES DE NEXP X NSAI VALORES CALCULADOS DE VARIAVEIS DE SAIDA
  TYPE(M3_LIST), ALLOCATABLE :: VARYE(:) !LISTA DE NMOD ARRAYS DE NEXP X NSAI X NSAI: NEXP COVARIANCIAS DE VARIAVEIS DE SAIDA (NSAI X NSAI)
  TYPE(M3_LIST), ALLOCATABLE :: VARYEINV(:) !LISTA DE NMOD ARRAYS DE NEXP X NSAI X NSAI: NEXP INVERSAS DAS COVARIANCIAS DE VARIAVEIS DE SAIDA (NSAI X NSAI)
  REAL(8), ALLOCATABLE :: PARAM(:) !LISTA DE NPAR PARÂMETROS
  TYPE(M2_LIST), ALLOCATABLE :: EY(:) !ERRO DAS VARIÁVEIS DE SAÍDA (CALCULADO - EXPERIMENTAL)
!===============================================================================
CONTAINS
!===============================================================================
  FUNCTION INT_TO_CHAR(I) RESULT(C)
    !TRIM(LINT_TO_CHAR(HUGE(1)))
    !TRIM(LINT_TO_CHAR(-HUGE(1)))
    !TRIM(LINT_TO_CHAR(0))
    !>>>2147483647 
    !>>>-2147483647
    !>>>0          
    INTEGER, INTENT(IN) :: I
    CHARACTER(LEN=11) :: C
    WRITE(C,'(I11.1)') I
  ENDFUNCTION
!===============================================================================
  FUNCTION LINT_TO_CHAR(I) RESULT(C)
    !LINT_TO_CHAR(HUGE(1))
    !LINT_TO_CHAR(-HUGE(1))
    !LINT_TO_CHAR(0)
    !>>> 2147483647
    !>>>-2147483647
    !>>> 0000000000
    INTEGER, INTENT(IN) :: I
    CHARACTER(LEN=11) :: C
    WRITE(C,'(I11.10)') I
  ENDFUNCTION
!===============================================================================
  FUNCTION SCI_TO_CHAR(R) RESULT(C)
    !SCI_TO_CHAR(HUGE(1.d0))
    !SCI_TO_CHAR(-HUGE(1.d0))
    !SCI_TO_CHAR(TINY(1.d0))
    !SCI_TO_CHAR(-TINY(1.d0))
    !>>> 0.1798E+309
    !>>>-0.1798E+309
    !>>> 0.2225E-307
    !>>>-0.2225E-307
    REAL(8), INTENT(IN) :: R
    CHARACTER(LEN=12) :: C
    WRITE(C,'(E12.4E3)') R
  ENDFUNCTION
!===============================================================================
  FUNCTION DSCI_TO_CHAR(R) RESULT(C)
    !DSCI_TO_CHAR(HUGE(1.d0))
    !DSCI_TO_CHAR(-HUGE(1.d0))
    !DSCI_TO_CHAR(TINY(1.d0))
    !DSCI_TO_CHAR(-TINY(1.d0))
    !>>> 0.17976931348623157E+309
    !>>>-0.17976931348623157E+309
    !>>> 0.22250738585072014E-307
    !>>>-0.22250738585072014E-307
    REAL(8), INTENT(IN) :: R
    !            +13
    CHARACTER(LEN=25) :: C
    !         +13+13
    WRITE(C,'(E25.17E3)') R
  ENDFUNCTION
!===============================================================================
  FUNCTION PERCENT_TO_CHAR(R) RESULT(C)
    REAL(8), INTENT(IN) :: R
    CHARACTER(LEN=25) :: C1, C
    WRITE(C1,'(F4.1)') R*100.D0
    WRITE(C,'(A)') TRIM(C1)//" %"
  ENDFUNCTION
!===============================================================================
  SUBROUTINE CCWRITE(NUM,MESSAGESTR)
    INTEGER, INTENT(IN) :: NUM(:)
    INTEGER :: I
    INTEGER :: N
    CHARACTER(LEN=*), INTENT(IN) :: MESSAGESTR
    N=SIZE(NUM)
    DO I = 1,N
      WRITE(NUM(I),'(A)') MESSAGESTR
    ENDDO
  ENDSUBROUTINE
!===============================================================================
  FUNCTION ZEROS(N) RESULT(Z)
    INTEGER, INTENT(IN) :: N
    REAL(8), ALLOCATABLE :: Z(:)
    INTEGER :: J
    Z=[(0.D0,J=1,N)]
  ENDFUNCTION
!===============================================================================
ENDMODULE

