!===============================================================================
MODULE ESTIMATION_MOD
!    ESTIMAÇÃO DE PARÂMETROS
!    UTILIZANDO A METODOLOGIA DE FUNÇÃO OBJETIVO MÍNIMOS QUADRADOS PONDERADOS PO
!    R VARIÂNVIA EXPERIMENTAL UTILIZA UM MÉTODO HÍBRIDO PARA A MINIMIZAÇÃO DA FU
!    NÇÃO OBJETIVO. A MINIMZAÇÃO INICIA COM O MÉTODO DO SWARM DE PARTÍCULAS (KE
!    NNEDY & EBERHART, PROC IEEE INTERNATIONAL CONFERENCE ON NEURAL NETWORKS, PE
!    RTH, AUSTRALIA, PP. 1942-1948 E A MELHOR SOLUÇÃO ENCONTRADA É USADA COMO ES
!    TIMATIVA INICAL PARA UM MÉTODO
!    DO TIPO GAUSS-NEWTON (ANDERSON ET AL.,AICHE J.,1978,24(1),20-29) COM ACELER
!    ADOR DE LAW E BAILEY (LAW & BAILEY,CHEM.ENG.SCI.,1963,18,189-202)
!    ANÁLISE ESTATÍSTICA DE TESTES T-STUDENT, CHI^2 E FISHER COMO DESC
!    RITO POR  SCHWAAB, M. PINTO, J. C. ANÁLISE DE DADOS EXPERIMENTAIS I FUNDAM
!    ENTOS DE ESTATÍSTICA E ESTIMAÇÃO DE PARÂMETROS.EPAPERS, RIO DE JANEIRO,2007
!    UTILIZAÇÃO DE FUNÇÕES ESTATÍSTICAS DA BIBLIOTECA GSL
!    UTILIZAÇÃO DE FUNÇÕES DE GEREAÇÃO DE NUMERO ALEATORIO (SEQUENCIA PSEUDO ALEATORIA) DA BIBLIOTECA ZIGURATT
!    UTILIZAÇÃO DE ALGORITMO DE RESOLUÇÃO DE SISTEMAS DE EQUAÇÃO ALGEBRICO DIFERENCIAL DASSL
!===============================================================================
  USE GLOBAL_MOD
  USE GAUSSNEWTON_MOD
  USE SWARM_MOD
!===============================================================================
CONTAINS
!===============================================================================
  SUBROUTINE ESTIMATION(MODELS)
    !CONFIGURAÇÃO DE MODELO
    PROCEDURE(IMODELS) :: MODELS
    INTEGER :: DADOS_OPT_FNUM
    INTEGER :: DADOSEXP_FNUM
    INTEGER :: REPORT_FNUM
    !CONTADORES
    INTEGER :: I
    INTEGER :: J
    INTEGER :: K
    INTEGER :: M
    INTEGER :: II
    INTEGER :: JJ
    !SWARM
    INTEGER :: FLAG_REGCONF  ! ATIVA ARQUIVO SWARM PARA REGIÃO DE CONFIANÇA
    INTEGER :: NIT_SWARM  ! NÚMERO MÁXIMO DE ITERAÇÕES
    INTEGER :: NPS    ! NÚMERO DE PARTÍCULAS DO SWARM
    REAL(8) :: NPREG  ! NÚMERO DE PONTOS NA REGIÃO DE CONFIANÇA
    INTEGER :: FLAG_STATUS  ! VERIFICA LEITURA DO ARQUIVO SAIDA DO SWARM
    REAL(8), ALLOCATABLE :: PLMIN(:)  ! LIMITE INFERIOR DOS PARÂMETROS
    REAL(8), ALLOCATABLE :: PLMAX(:)  ! LIMITE SUPERIOR DOS PARÂMETROS
    REAL(8), ALLOCATABLE :: PP(:)    ! VALOR DOS PARAMETROS LIDOS DO ARQUIVO POPULACAO SWARM
    REAL(8), ALLOCATABLE :: PMIN(:)    ! VALOR MÍNIMO DOS PARAMETROS SEGUNDO SWARM
    REAL(8), ALLOCATABLE :: PMAX(:)    ! VALOR MÁXIMO DOS PARAMETROS SEGUNDO SWARM
    INTEGER :: REGCONF_FNUM
    INTEGER :: SAIDA_SWARM_POPULACAO_FNUM
    !GAUSS NEWTON
    INTEGER :: NIT_GAUSSNEWTON  ! NÚMERO MÁXIMO DE ITERAÇÕES
    INTEGER :: IT    ! NÚMERO ITERAÇÕES REALIZADAS PELO GAUSS NEWTON
    REAL(8) :: FTOL  ! TOLERÂNCIA DA FUNÇÃO OBJETIVO
    REAL(8) :: ALPHA_LB_TOL  ! TOLERÂNCIA DO PASSO
    REAL(8), ALLOCATABLE :: DP(:)    ! PERTURBAÇÃO ABSOLUTA NOS PARÂMETROS
    INTEGER :: GRAFICO_GAUSSNEWTON_FNUM
    !ESTATISTICA
    REAL(8) :: ALPHA  ! NÍVEL DE CONFIANÇA PARA TESTES ESTATÍSTICOS
    INTEGER :: GL    ! GRAUS DE LIBERDADE
    REAL(8) :: TS    ! LIMITE DE CONFANÇA DE ACORDO COM T-STUDENT
    REAL(8) :: FOBJ  ! VALOR DA FUNÇÃO OBJETIVO MEMORIA LOCAL
    REAL(8) :: F    ! VALOR DA FUNÇÃO OBJETIVO LIDA DO ARQUIVO POPULACAO SWARM
    REAL(8) :: OBJF_FISHER  ! VALOR DA FUNÇÃO OBJETIVO LIMITE PARA REGIÃO DE CONFIANÇA
    REAL(8), ALLOCATABLE :: COV(:,:)  ! MATRIZ DE COVARIÂNCIA DOS PARÂMETROS
    REAL(8), ALLOCATABLE :: COR(:,:)  ! MATRIZ DE CORRELAÇÃO DOS PARÂMETROS
    REAL(8), ALLOCATABLE :: SIGMA(:)  ! VETOR COM OS DESVIOS PADRÕES DOS PARÂMETROS
    TYPE(M3_LIST), ALLOCATABLE :: PRED(:)  ! MATRIZ DE COVARIÂNCIA DE PREDIÇÃO
    REAL(8) :: PABS    ! PROBABILIDADE ABSOLUTA DE CADA MODELO
    REAL(8) :: CHI2MAX! VALOR DO CHI-QUADRADO MAXIMO PARA TESTE DE FOBJPARA UM DADO ALPHA E GL
    REAL(8) :: CHI2MIN    ! VALOR DO CHI-QUADRADO MINIMO PARA TESTE DE FOBJPARA UM DADO ALPHA E GL
    !FORMATAÇÃO
    CHARACTER(LEN=200) :: FORMAT_STR
    INTEGER :: NCOL
!-------------------------------------------------------------------------------
    PMODELS=>MODELS
!-------------------------------------------------------------------------------
    CALL SPLASH()
!-------------------------------------------------------------------------------
    CALL LEITURA_EXP()
!-------------------------------------------------------------------------------
    CALL LEITURA_OPT()
!-------------------------------------------------------------------------------
    CALL ALLOCATION()
!-------------------------------------------------------------------------------
    CALL PRESTATISTICS()
!-------------------------------------------------------------------------------
    CALL SWARM(FLAG_REGCONF,NIT_SWARM,NPS,PLMIN,PLMAX,FOBJ,GL)
    CALL GRAFICO_SWARM()
!-------------------------------------------------------------------------------
    CALL POSTSTATISTICS()
    CALL FILTRO_REGCONF()
!-------------------------------------------------------------------------------
    CALL GAUSSNEWTON(NIT_GAUSSNEWTON,FTOL,ALPHA_LB_TOL,PLMIN,PLMAX,DP,IT,FOBJ,COV,PRED)
!-------------------------------------------------------------------------------
    CALL POSTSTATISTICS()
    CALL FILTRO_REGCONF()
!-------------------------------------------------------------------------------
    CALL REPORT()
!-------------------------------------------------------------------------------
    CALL MEMFREE()
!===============================================================================
  CONTAINS
!===============================================================================
    SUBROUTINE SPLASH()
    PRINT*,"                                                                        "
    PRINT*,"========================================================================"
    PRINT*,"                                                                        "
    PRINT*," PARAMETER                                                              "
    PRINT*,"                                                                        "
    PRINT*," O))))) O))) O))))) O))) O)O)    O) O)    O))))) O)))   O)    O)     O) "
    PRINT*," O))  O))  O)  O)    O)  O) O)) O)) O))     O)    O)  O)  O)  O)O)   O) "
    PRINT*," O))   O))     O)    O)  O)  O))))) O)))    O)    O) O)    O) O) O)  O) "
    PRINT*," O)))))  O))   O)    O)  O)   O) O) O  O)   O)    O) O)    O) O)  O) O) "
    PRINT*," O))       O)) O)    O)  O)      O) O)))))  O)    O) O)    O) O)   O)O) "
    PRINT*," O))   O))  O) O)    O)  O)      O) O)   O) O)    O)  O)  O)  O)     O) "
    PRINT*," O))))) O))O)  O)   O))) O)      O) O)    O)O)   O)))   O)    O)     O) "
    PRINT*,"                                                                        "
    PRINT*,"                                                  FOR NON-LINEAR MODELS "
    PRINT*,"                                                                        "
    PRINT*," EXP > WLSOBJF > SWARM > GAUSSNEWTON > COR > T-STUDENT > CHI^2 > FISHER "
    PRINT*,"                                                                        "
    PRINT*,"========================================================================"
    PRINT*,"                                                                        "
    ENDSUBROUTINE
!===============================================================================
    SUBROUTINE LEITURA_EXP()
      OPEN(NEWUNIT=DADOSEXP_FNUM,FILE='INPUT/DADOSEXP.DAT',STATUS='OLD',ACTION='READ')
      ! LEITURA DA ESTRUTURA E VALORES DOS DADOS EXPERIMENTAIS
      !NUMERO DE MODELOS
      READ(DADOSEXP_FNUM,*) !CABEÇALHO
      READ(DADOSEXP_FNUM,*) NMOD
      PRINT*, 'NMOD: ', NMOD
      !ALOCA MEMORIA
      ALLOCATE(NENT(NMOD))
      ALLOCATE(NSAI(NMOD))
      ALLOCATE(NEXP(NMOD))
      ALLOCATE(XE(NMOD))
      ALLOCATE(YE(NMOD))
      ALLOCATE(YC(NMOD))
      ALLOCATE(VARYE(NMOD))
      ALLOCATE(VARYEINV(NMOD))
      !-------------------------------------------------------------------------------
        DO M = 1, NMOD !VALOR DADO NO CODIGO FONTE PRINCIPAL, QUE ALOCA OS MODELOS
          READ(DADOSEXP_FNUM,*) !CABEÇALHO
          READ(DADOSEXP_FNUM,*) NEXP(M), NENT(M), NSAI(M) ! NÚMERO DE EXPERIMENTOS, DE VARIÁVEIS DE ENTRADA E SAÍDA
          PRINT*, 'NEXP, NENT, NSAI: ', NEXP, NENT, NSAI
          ! ALOCANDO AS VARIÁVEIS DE ENTRADA E SAÍDA
          ALLOCATE(XE(M)%M(NEXP(M),NENT(M)))
          ALLOCATE(YE(M)%M(NEXP(M),NSAI(M)))
          ALLOCATE(YC(M)%M(NEXP(M),NSAI(M)))
          ALLOCATE(VARYE(M)%M(NEXP(M),NSAI(M),NSAI(M)))
          ALLOCATE(VARYEINV(M)%M(NEXP(M),NSAI(M),NSAI(M)))
          ! ZERANDO AS MATRIZES DOS ERROS EXPERIMENTAIS E SUAS INVERSAS
          VARYE(M)%M(:,:,:)    = 0.D0
          VARYEINV(M)%M(:,:,:) = 0.D0
          ! LEITURA DOS DADOS EXPERIMENTAIS
          READ(DADOSEXP_FNUM,*) !CABEÇALHO DAS VARIÁVEIS
          PRINT*, ('XE(',INT_TO_CHAR(I),')',I=1,NENT(M)), ('  YE(',INT_TO_CHAR(I),')','  VARY(',INT_TO_CHAR(I),')',I=1,NSAI(M))
          DO K=1,NEXP(M)
            !MÉTODO DE LEITURACOM LOOP IMPLICITO
            READ(DADOSEXP_FNUM,*) (XE(M)%M(K,I),I=1,NENT(M)), (YE(M)%M(K,I),VARYE(M)%M(K,I,I),I=1,NSAI(M))
            PRINT*, (XE(M)%M(K,I),I=1,NENT(M)), (YE(M)%M(K,I),VARYE(M)%M(K,I,I),I=1,NSAI(M))
          ENDDO
        ENDDO
      CLOSE(DADOSEXP_FNUM)
      ! INVERSAo DA MATRIZ DOS ERROS EXPERIMENTAIS EM Y
      DO M = 1, NMOD
        DO K=1,NEXP(M)
          CALL INVERSAOMATRICIAL(NSAI(M),VARYE(M)%M(K,:,:),VARYEINV(M)%M(K,:,:))
        ENDDO
      ENDDO
    ENDSUBROUTINE
!===============================================================================
    SUBROUTINE LEITURA_OPT()
      ! ABRE ARQUIVO COM DADOS RELATIVOS A BUSCA DO MÍNIMO DA FUNÇÃO OBJETIVO
      OPEN(NEWUNIT=DADOS_OPT_FNUM,FILE='INPUT/DADOSOPT.DAT',STATUS='OLD',ACTION='READ')
      !PARAMETROS
      READ(DADOS_OPT_FNUM,*)  !CABEÇALHO
      READ(DADOS_OPT_FNUM,*)  !CABEÇALHO
      READ(DADOS_OPT_FNUM,*) NPAR ! NÚMEROS DE PARÂMETROS DO MODELO
      PRINT*, "NPAR: ", NPAR
      ! ALOCAÇÃO DO VETOR DE PARÂMETROS E AS MATRIZES DE COVARIÂNCIA E DE CORRELAÇÃO
      ALLOCATE(PARAM(NPAR))
      ALLOCATE(SIGMA(NPAR))
      ALLOCATE(COV(NPAR,NPAR))
      ALLOCATE(COR(NPAR,NPAR))
      ALLOCATE(PLMIN(NPAR))
      ALLOCATE(PLMAX(NPAR))
      ALLOCATE(DP(NPAR))
      ! ALOCANDO AS MATRIZES COM A FAIXA VÁLIDA E PERTURBAÇÃO DOS PARAMETROS
      ! PONTO DE TESTE DOS PARÂMETROS
      READ(DADOS_OPT_FNUM,*)  !CABEÇALHO
      READ(DADOS_OPT_FNUM,*) (PARAM(I),I=1,NPAR)
      PRINT*, "PARAM: ", PARAM
      ! FAIXA VÁLIDA PARA OS PARÂMETROS
      READ(DADOS_OPT_FNUM,*)  !CABEÇALHO
      PRINT*, "PLMIN,PLMAX"
      DO I=1,NPAR
        READ(DADOS_OPT_FNUM,*) PLMIN(I), PLMAX(I)
        PRINT*, PLMIN(I), PLMAX(I)
      ENDDO
      !SWARM
      READ(DADOS_OPT_FNUM,*)  !CABEÇALHO
      READ(DADOS_OPT_FNUM,*)  !CABEÇALHO
      READ(DADOS_OPT_FNUM,*) NIT_SWARM  ! NÚMERO MÁXIMO DE ITERAÇÕES
      PRINT*, "NIT_SWARM: ", NIT_SWARM
      READ(DADOS_OPT_FNUM,*)  !CABEÇALHO
      READ(DADOS_OPT_FNUM,*) NPS
      PRINT*, "NPS: ", NPS
      !GAUSSNEWTON
      READ(DADOS_OPT_FNUM,*)  !CABEÇALHO
      READ(DADOS_OPT_FNUM,*)  !CABEÇALHO
      READ(DADOS_OPT_FNUM,*) NIT_GAUSSNEWTON
      PRINT*, "NIT_GAUSSNEWTON: ", NIT_GAUSSNEWTON
      READ(DADOS_OPT_FNUM,*)  !CABEÇALHO
      READ(DADOS_OPT_FNUM,*) FTOL    ! TOLERÂNCIA NA FUNÇÃO OBJETIVO
            PRINT*, "FTOL: ", FTOL
      READ(DADOS_OPT_FNUM,*)  !CABEÇALHO
      READ(DADOS_OPT_FNUM,*) ALPHA_LB_TOL  ! TOLERÂNCIA NO PASSO
      PRINT*, "ALPHA_LB_TOL: ", ALPHA_LB_TOL
      ! PERTURBAÇÃO NOS PARÂMETROS PARA O CÁLCULO DAS DERIVADAS

      READ(DADOS_OPT_FNUM,*)  !CABEÇALHO
      READ(DADOS_OPT_FNUM,*) (DP(I), I=1,NPAR)
      PRINT*, "DP: ", DP
      !CONFIANÇA
      READ(DADOS_OPT_FNUM,*)  !CABEÇALHO
      READ(DADOS_OPT_FNUM,*)  !CABEÇALHO
      READ(DADOS_OPT_FNUM,*) ALPHA    ! NÍVEL DE CONFIANÇA
      PRINT*, "ALPHA: ", ALPHA
      CLOSE(DADOS_OPT_FNUM)
    ENDSUBROUTINE
!===============================================================================
    SUBROUTINE PRESTATISTICS()
    ! SEÇÃO 4.7 SCHWAAB, M., PINTO, J. C., ANÁLISE DE DADOS EXPERIMENTAIS I
      !VARIÁVEL PARA CÁLCULO DOS LIMITES DE CONFIANÇA
      GL = 0
      DO M = 1, NMOD
        GL = GL + NEXP(M)*NSAI(M)
      ENDDO
      GL = GL - NPAR
      !STUDENT
      TS = DTIN( (1.D0+ALPHA)/2 , DFLOAT(GL) )
      !EXERCICIO SUGERIDO #2 DO CAPITULO 5 E SOLUÇÃO; EQ 4.88;
      CHI2MAX = DCHIIN((1.D0+ALPHA)/2.D0, DFLOAT(GL))
      CHI2MIN = DCHIIN((1.D0-ALPHA)/2.D0, DFLOAT(GL))
    ENDSUBROUTINE
!===============================================================================
    SUBROUTINE POSTSTATISTICS()
    ! SEÇÃO 4.7 E CAP 5 SCHWAAB, M., PINTO, J. C., ANÁLISE DE DADOS EXPERIMENTAIS I
      !VARIÁVEL PARA CÁLCULO DOS LIMITES DE CONFIANÇA
      REAL(8) :: NUM
      REAL(8) :: DEN1
      REAL(8) :: DEN2
      INTEGER :: I
      INTEGER :: J
      REAL(8) :: Y_CALC_AVE
      REAL(8) :: Y_EXP_AVE
      REAL(8) :: CORREL
      INTEGER :: NYT
      WRITE(*,'(/,A)') 'RESULTADOS'
    ! SEÇÃO 4.7 SCHWAAB, M., PINTO, J. C., ANÁLISE DE DADOS EXPERIMENTAIS I
      ! CÁLCULO DO VETOR DE DESVIOS PADRÕES DE CADA PARÂMETRO
      FORALL(I=1:NPAR)
        SIGMA(I) = DSQRT( COV(I,I) )
      ENDFORALL
      ! CÁLCULO DA MATRIZ DE CORRELAÇÃO DOS PARÂMETROS
      FORALL(I=1:NPAR,J=1:NPAR)
        COR(I,J)=  COV(I,J)/DSQRT(COV(I,I)*COV(J,J))
      ENDFORALL
      !VARIÁVEL PARA CÁLCULO DOS LIMITES DE CONFIANÇA
      !EXERCICIO SUGERIDO #2 DO CAPITULO 5 E SOLUÇÃO; EQ 4.88;
      PABS = 1.D0 - DCHIDF(FOBJ, DFLOAT(GL))
      !< VALOR DE DENSIDADE DE PROBABILIDADE PARA O VALOR DA FOBJ NO TESTE CHI2 COM NGL DO PROBLEMA E CONFIANÇA ESPECIFICADA
      !< O CHI2MIN, É AQUELE PARA O QUAL A CHI2 DÁ PABSMIN=(1-ALPHA)/2 (2.5% SE O ALPHA DE 95%)
      !< O CHI2MAX, É AQUELE PARA O QUAL A CHI2 DÁ PABSMAX=(1+ALPHA)/2 (97.5% SE O ALPHA DE 95%)
      !< O VALOR DE PABS DEVE ESTAR ENTRE ESSES VALORES PABSMIN E PABSMAX SE O MODELO FOR CAPAZ DE REPRESENTAR OS DADOS ADEQUADAMENTE.
      !< DISCUSSÃO CONTINUA NO VOLUME 2 DO LIVRO DE SCHWAAB E PINTO.
      ! SELEÇÃO DOS PONTOS PARA ANÁLISE DA REGIÃO/INTERVALO DE CONFIANÇA
      OBJF_FISHER = FOBJ * (1.D0 + DFLOAT(NPAR) / DFLOAT(GL) * DFIN(ALPHA, DFLOAT(NPAR), DFLOAT(GL))) !EQ 5.40
      !SEÇÃO 5.7 "SCHWAAB, M., PINTO, J. C., ANÁLISE DE DADOS EXPERIMENTAIS I"
      !-------------------------------------------------------------------------
      !CORRELAÇÃO CALC VS EXP, EQ 4.89
      !-------------------------------------------------------------------------
      NUM=0.D0
      DEN1=0.D0
      DEN2=0.D0
      NYT = 0
      Y_EXP_AVE=0.D0
      Y_CALC_AVE=0.D0
      DO I = 1,NMOD
        DO K = 1, NSAI(I)
          DO J = 1, NEXP(I)
            Y_EXP_AVE = Y_EXP_AVE + YE(I)%M(J,K)
            Y_CALC_AVE = Y_CALC_AVE + YC(I)%M(J,K)
            NYT=NYT+1
          ENDDO
        ENDDO
      ENDDO
      Y_EXP_AVE=Y_EXP_AVE/NYT
      Y_CALC_AVE=Y_CALC_AVE/NYT

      DO I = 1,NMOD
        DO J = 1, NEXP(I)
          DO K = 1, NSAI(I)
            NUM=NUM+(YE(I)%M(J,K)-Y_EXP_AVE)*(YC(I)%M(J,K)-Y_CALC_AVE)
            DEN1=DEN1+(YE(I)%M(J,K)-Y_EXP_AVE)**2
            DEN2=DEN2+(YC(I)%M(J,K)-Y_CALC_AVE)**2
          ENDDO
        ENDDO
      ENDDO
      CORREL = NUM/DSQRT(DEN1*DEN2)
      WRITE(*,'(/,A)') 'COEFICIENTE DE CORRELAÇÃO LINEAR ENTRE VALORES CALCULADOS E VALORES MEDIDOS: '
      WRITE(*,'(A,A)') 'CORREL = ', SCI_TO_CHAR(CORREL)
    ENDSUBROUTINE
!===============================================================================
    SUBROUTINE FILTRO_REGCONF()
      ! REGIÃO DE CONFIANÇA COM OS DADOS DO SWARM
      IF (FLAG_REGCONF .EQ. 1) THEN
        PRINT*, ' '
        PRINT*, 'FILTRANDO RESULTADOS PARA A REGIAO DE CONFIANCA FISHER'
        PRINT*, '(',FOBJ, '< F <', OBJF_FISHER,')'
        PRINT*, ' '
        DO M = 1, NMOD
          FORMAT_STR = '('//TRIM(INT_TO_CHAR(NPAR))//'('//'"PP("'//',I4.1,'//'")"'//',A),2(A),'
          FORMAT_STR = TRIM(FORMAT_STR)//TRIM(INT_TO_CHAR(NEXP(M)*NSAI(M)))//'('//'"YC("'//',I4.1,'//'","'//',I4.1,'//'")"'//',A)'//')'
          WRITE (REGCONF_FNUM,FORMAT_STR) (II, CHAR(9),II=1,NPAR), 'FOBJ', CHAR(9), ((II,JJ,CHAR(9),II=1,NEXP(M)),JJ=1,NSAI(M))
        ENDDO
        ! ABRE ARQUIVO COM OS DADOS DO SWARM DE PARTÍCULAS
        OPEN (NEWUNIT=SAIDA_SWARM_POPULACAO_FNUM,FILE='OUTPUT/SAIDA_SWARM_POPULACAO.DAT',STATUS='OLD',ACTION='READ')
        ! ALOCA E INICIALIZA VETORES PARA OS PARÂMETROS
        PMAX = PARAM  ! VALOR MÁXIMO DOS PARAMETROS NA REGIÃO DE CONFIANÇA
        PMIN = PARAM  ! VALOR MÍNIMO DOS PARAMETROS NA REGIÃO DE CONFIANÇA
        NPREG =  0.    ! NÚMERO DE PONTOS NA REGIÃO DE CONFIANÇA
        DO !LOOP SWARM POPULACAO
          ! LE O DADO GERADO PELO SWARM DE PARTÍCULAS
          READ (SAIDA_SWARM_POPULACAO_FNUM,*,IOSTAT = FLAG_STATUS) K, K, F, PP(1:NPAR)
          ! CONFERE SE CHEGOU AO FINAL DO ARQUIVO
          IF(FLAG_STATUS /= 0) THEN
            EXIT
          ENDIF
          ! TESTA O PONTO, SE ESTIVER NA REGIÃO DE CONFIANÇA E SALVA O VALOR
          IF (F  .LT. OBJF_FISHER) THEN
            NCOL = 0
            DO M = 1, NMOD
              NCOL = NCOL + NEXP(M)*NSAI(M)
            ENDDO
            NCOL = NPAR + 1
            FORMAT_STR = '('//TRIM(INT_TO_CHAR(NCOL))//'(E12.6,A)'//')'
            WRITE (REGCONF_FNUM,FORMAT_STR) (PP(II),",",II=1,NPAR), F," "
            FORALL(J=1:NPAR, PP(J)  .GT. PMAX(J))
              PMAX(J) = PP(J)
            ENDFORALL
            FORALL(J=1:NPAR, PP(J)  .LT. PMIN(J))
              PMIN(J) = PP(J)
            ENDFORALL
            NPREG = NPREG + 1.D0
          ENDIF
        ENDDO !LOOP SWARM POPULACAO
        CLOSE(SAIDA_SWARM_POPULACAO_FNUM)
        !RESULTADO
        WRITE(*,"('NUMERO DE PONTOS NA REGIAO DE CONFIANCA FISHER: ',F0.0)") NPREG
        WRITE(*,"(/,T1,'PAR(I)',T10,'PMIN(I)',T30,' < ','PARAM(I)',T50,' < ','PMAX(I)')")
        DO I=1,NPAR
          WRITE(*,"(T1,I0,T10,ES16.5E4,T30,' < ',ES16.5E4,T50,' < ',ES16.5E4)") I, PMIN(I), PARAM(I), PMAX(I)
        ENDDO
      ENDIF
    ENDSUBROUTINE
!===============================================================================
    SUBROUTINE REPORT()
      OPEN(NEWUNIT=REPORT_FNUM,FILE='OUTPUT/REPORT.DAT',STATUS='REPLACE',ACTION='WRITE')
      ! INICIA O ARQUIVO COM A SAIDA DOS RESULTADOS DE CADA MODELO
      WRITE(REPORT_FNUM, '(A)') 'RESULTADOS:'
      WRITE(REPORT_FNUM, '(A)') 
      WRITE(REPORT_FNUM,'(A,I4)') 'NUMERO DE EXPERIMENTOS :', NEXP
      WRITE(REPORT_FNUM, '(A)') 
      WRITE(REPORT_FNUM,'(A,I4)') 'NUMERO DE VARIÁVEIS DE ENTRADA :', NENT
      WRITE(REPORT_FNUM, '(A)') 
      WRITE(REPORT_FNUM,'(A,I4)') 'NUMERO DE VARIÁVEIS DE SAIDA :', NSAI
      WRITE(REPORT_FNUM, '(A)') 
      WRITE(REPORT_FNUM,'(A,I3)') 'NÚMERO DE ITERAÇÕES NO GAUSSNEWTON :', IT
      WRITE(REPORT_FNUM, '(A)') 
      WRITE(REPORT_FNUM,'(A,E12.6)') 'FUNÇÃO OBJETIVO : ', FOBJ
      WRITE(REPORT_FNUM, '(A)')
      WRITE(REPORT_FNUM,'(A,E12.6)') 'FUNÇÃO OBJETIVO LIMITE FISHER : ', OBJF_FISHER
      WRITE(REPORT_FNUM, '(A)')      
      ! PARÂMETROS
      WRITE(REPORT_FNUM,*) 'LIMITES DE CONFIANÇA DO TSTUDENT E DO FISHER'
      WRITE(REPORT_FNUM,*) '                  PARAM       ','VARIANCIA       ','LI_TSTUDENT       ','LS_TSTUDENT       '
      IF (FLAG_REGCONF .EQ. 1) THEN
        WRITE(REPORT_FNUM,*) '                                            ','LIFISHER       ','LSFISHER      '
      ENDIF
      DO I=1,NPAR
        WRITE(REPORT_FNUM,"(3X,'PARAM(',I2.2,') =',4(2X,E12.6))") I, PARAM(I),  SIGMA(I)**2., PARAM(I) - TS*SIGMA(I), PARAM(I) + TS*SIGMA(I)
        IF (FLAG_REGCONF .EQ. 1) THEN
          WRITE(REPORT_FNUM,"(30X,2(2X,E12.6))") PMIN(I), PMAX(I)
        ENDIF
      ENDDO
      WRITE(REPORT_FNUM, '(A)') 
      WRITE(REPORT_FNUM,*) 'MATRIZ DE COVARIÂNCIA DOS PARÂMETROS'
      FORMAT_STR = '('//'1X,'//TRIM(INT_TO_CHAR(NPAR))//'(T5,'//TRIM(INT_TO_CHAR(NPAR))//'(2X,E12.6),/)'//')'
      WRITE(REPORT_FNUM,FORMAT_STR) COV(1:NPAR,1:NPAR)
      WRITE(REPORT_FNUM, '(A)')
      WRITE(REPORT_FNUM,*) 'MATRIZ DE CORRELAÇÃO DOS PARÂMETROS'
      FORMAT_STR = '('//'1X,'//TRIM(INT_TO_CHAR(NPAR))//'(T5,'//TRIM(INT_TO_CHAR(NPAR))//'(2X,E12.6),/)'//')'
      WRITE(REPORT_FNUM,FORMAT_STR) COR(1:NPAR,1:NPAR)
      ! ARQUIVO PARA CONSTRUÇÃO DE GRAFICOS
      OPEN (NEWUNIT=GRAFICO_GAUSSNEWTON_FNUM,FILE='OUTPUT/GRAFICO_GAUSSNEWTON.DAT',STATUS='REPLACE',ACTION='WRITE')
      DO M = 1, NMOD
        FORMAT_STR = '('//'1X,'//'"EXP,",'//TRIM(INT_TO_CHAR(NENT(M)))//'(7X,'//'"XE("'//'I2.2,'//'"),"'//'),'
        FORMAT_STR = TRIM(FORMAT_STR)//&
        TRIM(INT_TO_CHAR(NSAI(M)+1))//'(7X,'//'"YE("'//',I2.2,'//'"),"'//'7X,'//'"SIGY("'//',I2.2,'//'"),"'//',7X,'//'"YC("'//',I2.2,'//'"),"'//',7X,'//'"YL("'//',I2.2,'//'"),"'//',7X,'//'"YU("'//',I2.2,'//'"),"'//')'//')'
        WRITE(GRAFICO_GAUSSNEWTON_FNUM,FORMAT_STR) (I, I=1,NENT(M)), (I,I,I,I,I, I=1,NSAI(M))
        FORMAT_STR = '('//'2X,I4,'//'",",'//TRIM(INT_TO_CHAR(NENT(M)+5*NSAI(M)))//'(1X,E12.6,'//'","'//')'//')'
        DO I=1,NEXP(M)
          WRITE(GRAFICO_GAUSSNEWTON_FNUM,FORMAT_STR) I, XE(M)%M(I,1:NENT(M)),(YE(M)%M(I,J), PRED(M)%M(I,J,J), YC(M)%M(I,J), YC(M)%M(I,J)-TS*DSQRT(PRED(M)%M(I,J,J)), YC(M)%M(I,J)+TS*DSQRT(PRED(M)%M(I,J,J)), J=1,NSAI(M))
        ENDDO
      ENDDO
      CLOSE(GRAFICO_GAUSSNEWTON_FNUM)
      IF (FLAG_REGCONF .EQ. 1) THEN
        CLOSE(REGCONF_FNUM)
      ENDIF
      WRITE(REPORT_FNUM, '(A)') 
      WRITE(REPORT_FNUM,*) 'XIMIN   FOBJ       CHI2MAX'
      WRITE(REPORT_FNUM,'(3(A))') DSCI_TO_CHAR(CHI2MIN), DSCI_TO_CHAR(FOBJ), DSCI_TO_CHAR(CHI2MAX)
      WRITE(REPORT_FNUM,*) 'PABSMIN   PABS       PABSMAX'
      WRITE(REPORT_FNUM,'(3(A))') DSCI_TO_CHAR(ALPHA/2.D0), DSCI_TO_CHAR(PABS), DSCI_TO_CHAR(1.D0-ALPHA/2.D0)
      WRITE(REPORT_FNUM, *)

      WRITE(*, '(A)') 
      WRITE(*,*) 'XIMIN   FOBJ       CHI2MAX'
      WRITE(*,'(3(A))') SCI_TO_CHAR(CHI2MIN), SCI_TO_CHAR(FOBJ), SCI_TO_CHAR(CHI2MAX)
      WRITE(*,*) 'PABSMIN   PABS       PABSMAX'
      WRITE(*,'(3(A))') SCI_TO_CHAR(ALPHA/2.D0), SCI_TO_CHAR(PABS), SCI_TO_CHAR(1.D0-ALPHA/2.D0)
      WRITE(*,*)

    ENDSUBROUTINE
!===============================================================================
    SUBROUTINE ALLOCATION()
      FLAG_REGCONF=1
      OPEN (NEWUNIT=REGCONF_FNUM,FILE='OUTPUT/SAIDA_SWARM_REGCONF_FISHER.DAT',STATUS='REPLACE',ACTION='WRITE')
      ALLOCATE(EY(NMOD))
      DO M = 1, NMOD
        ALLOCATE(EY(M)%M(NEXP(M),NSAI(M)))
      END DO
      ALLOCATE(PP(NPAR))
      ALLOCATE(PMIN(NPAR))
      ALLOCATE(PMAX(NPAR))
    ENDSUBROUTINE
!===============================================================================
    SUBROUTINE MEMFREE()
      DEALLOCATE(PARAM)
      DEALLOCATE(PLMIN)
      DEALLOCATE(PLMAX)
      DEALLOCATE(DP)
      DEALLOCATE(SIGMA)
      DEALLOCATE(COV)
      DEALLOCATE(COR)
      DO M = 1, NMOD
        DEALLOCATE(XE(M)%M)
        DEALLOCATE(YE(M)%M,YC(M)%M)
        DEALLOCATE(VARYE(M)%M,VARYEINV(M)%M)
      ENDDO
      DEALLOCATE(XE)
      DEALLOCATE(YE)
      DEALLOCATE(YC)
      DEALLOCATE(VARYE)
      DEALLOCATE(VARYEINV)
      DO M = 1, NMOD
        DEALLOCATE(PRED(M)%M)
      ENDDO
      DEALLOCATE(PRED)
      DEALLOCATE(NENT)
      DEALLOCATE(NSAI)
      DEALLOCATE(NEXP)
      DEALLOCATE(EY)
      DEALLOCATE(PMIN)
      DEALLOCATE(PMAX)
    ENDSUBROUTINE
!===============================================================================
    SUBROUTINE GRAFICO_SWARM()
      INTEGER :: GRAFICO_SWARM_FNUM
      !CHAMA FUNÇÃO OBJETIVO PARA OBTER OS VALORES DE VARIAVEL DE SAIDA OTIMOS PARA GRAFICO
      CALL OBJF(FOBJ)
      OPEN (NEWUNIT=GRAFICO_SWARM_FNUM,FILE='OUTPUT/GRAFICO_SWARM.DAT',STATUS='REPLACE',ACTION='WRITE')
      DO M = 1, NMOD
        !CABEÇALHO
        WRITE(GRAFICO_SWARM_FNUM,"("//"' EXP, ',"//INT_TO_CHAR(NENT(M))//"('XE(',A,'), '),"//INT_TO_CHAR(NSAI(M  ))//"('YE(',A,'), ','YC(',A,'), '),"//")") &
        (TRIM(ADJUSTL(INT_TO_CHAR(I))), I=1,NENT(M)), (TRIM(ADJUSTL(INT_TO_CHAR(I))),TRIM(ADJUSTL(INT_TO_CHAR(I))), I=1,NSAI(M))
        DO I=1,NEXP(M)
          !VALORES
          WRITE(GRAFICO_SWARM_FNUM,"("//"A,', '"//INT_TO_CHAR(NENT(M))//"(A,', '),"//INT_TO_CHAR(NSAI(M))//"(A,', ',A,', '),"//")") &
          LINT_TO_CHAR(I), (DSCI_TO_CHAR(XE(M)%M(I,J)),J=1,NENT(M)), (DSCI_TO_CHAR(YE(M)%M(I,J)), DSCI_TO_CHAR(YC(M)%M(I,J)), J=1,NSAI(M))
        ENDDO
      ENDDO
      CLOSE(GRAFICO_SWARM_FNUM)
    ENDSUBROUTINE
!===============================================================================
  ENDSUBROUTINE
!===============================================================================
ENDMODULE
