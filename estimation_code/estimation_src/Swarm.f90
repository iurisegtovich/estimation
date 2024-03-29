MODULE SWARM_MOD
!===============================================================================
! M�DULO DO M�TODO DO PARTICLE SWARM OPTIMIZATION (PSO)
! (KENNEDY E EBERHART, 1995, IN: PROC. IEEE INTERNATIONAL CONFERENCE ON NEURAL NETWORKS, PERTH, AUSTRALIA, PP. 1942-1948)
! SCHWAAB E PINTO, 
! FUNDAMENTOS DE ESTAT�STICA E ESTIMA��O DE PAR�METROS
! EPAPERS, RIO DE JANEIRO,2007
! SE��O 5.6.4
  USE GLOBAL_MOD
  USE OBJF_MOD
  USE ZIGGURAT_MOD
!===============================================================================
  CONTAINS
!===============================================================================
  SUBROUTINE SWARM(FLAG_REGCONF,NIT,NPS,PLMIN,PLMAX,FOBJ,GL)
  !IMPLEMENTA��O DO METODO PSO
  INTEGER, INTENT(IN) :: FLAG_REGCONF  ! DEFINE SE CRIA ARQUIVO PARA REGI�O DE CONFIAN�A
  INTEGER, INTENT(IN) :: NPS      ! N�MERO DE PART�CULAS NO SWARM
  INTEGER, INTENT(IN) :: NIT    ! N�MERO DE ITERA��ES
  REAL(8), INTENT(IN) :: PLMIN(NPAR)  ! LIMITES INFERIOR E SUPEIOR DO INTERVALO DE BUSCA DOS PAR�METROS
  REAL(8), INTENT(IN) :: PLMAX(NPAR)  ! LIMITES INFERIOR E SUPEIOR DO INTERVALO DE BUSCA DOS PAR�METROS
  REAL(8), INTENT(OUT) :: FOBJ      ! VALOR FINAL DA FUN��O OBJETIVO
  INTEGER, INTENT(IN) :: GL !NUMERO DE GRAUS DE LIBERDADE PARA O TESTE DE CHI QUADRADO
  !CALIBRA��O DO SWARM SEGUNDO EXEMPLO 5.10
  REAL(8), PARAMETER :: WO = 0.75   ! VALOR INICIAL E FINAL DO FATOR DE IN�RCIA
  REAL(8), PARAMETER :: WF = 0.75   ! VALOR INICIAL E FINAL DO FATOR DE IN�RCIA
  REAL(8), PARAMETER :: C1 = 1.5   ! VALOR DAS PONDERA��ES DA CONTRIBUI��O INDIVIDUAL E GLOBAL
  REAL(8), PARAMETER :: C2 = 1.5   ! VALOR DAS PONDERA��ES DA CONTRIBUI��O INDIVIDUAL E GLOBAL
  REAL(8) :: W        ! VALOR DO FATOR DE IN�RCIA VARI�VEL AO LONGO DA BUSCA
  INTEGER :: I
  INTEGER :: J
  INTEGER :: IT    ! CONTADOR DAS ITERA��ES
  INTEGER :: POS(1)  ! ARRAY1D DE UM ELEMENTO PARA SALVAR O �NDICE DA POSI��O DO MENOR VALOR DE UM OUTRO ARRAY 1D
  REAL(8) :: P(NPS,NPAR)    ! MATRIZ COM OS VALORES DOS PAR�METROS DE CADA PART�CULA
  REAL(8) :: PVEL(NPS,NPAR)  ! MATRIZ COM OS VALORES DAS VELOCIDADES DE CADA PART�CULA
  REAL(8) :: VMAX(NPAR)    ! VETOR COM AS VELOCIDADES M�XIMAS EM CADA DIRE��O
  REAL(8) :: PPT(NPS,NPAR)  ! MATRIZ COM OS VALORES DOS PAR�METROS �TIMOS DE CADA PART�CULA
  REAL(8) :: POTM(NPAR)    ! VETOR COM OS PAR�METROS �TIMOS DO CONJUNTO DE PART�CULAS
  REAL(8) :: F(NPS)      ! VETOR COM OS VALORES DAS FUN��ES OBJETIVO DA ITERA��O ATUAL DE CADA PART�CULA
  REAL(8) :: FPT(NPS)    ! VETOR COM OS VALORES �TIMOS PARTICULARES DAS FUN��ES OBJETIVO DE CADA PART�CULA
  REAL(8) :: FOTM !VALORES OTIMOS NA POPULA��O DA FUNCAO OBJETIVO AO LONGO DAS ITERA��ES
  INTEGER :: SAIDA_SWARM_POPULACAO_FNUM
  !-----------------------------------------------------------------------------
  CALL ZIGSET( TIME()**2 ) !INICIALIZA��O DO GERADOR DE N�MEROS ALEAT�RIOS
  !SPLASH
  WRITE(*,"(/,'SWARM:')")
  !CABE�ALHO
  WRITE(*,"(T1,'ITERATION',T20,'OBJF', T30, 'PABS',T70,"//INT_TO_CHAR(NPAR)//"('PARAM(',I0,')',5X))") (I,I=1,NPAR)
  !INICIO
  IF (NPS .GT. 0) THEN
    FOTM = HUGE(1.D0)
    FPT = HUGE(1.D0)
    ! DEFINE A VELOCIDADE M�XIMA COMO A QUE TRANSVERSA A REGIAO DE BUSCA INTEIRA EM UM UNICO PASSO
    VMAX = (PLMAX(:) - PLMIN(:))/2.D0
    ! GERANDO SWARM INICIAL E VELOCIDADES INICIAIS
    DO I=1,NPS
      DO J=1,NPAR
        !POSI��ES ALEATORIAMENTE (UNIFORME) ENTRE OS LIMITES INFERIOR E SUPERIOR
        P(I,J) = PLMIN(J) + UNI()*(PLMAX(J) - PLMIN(J))
        !VELOCIDADES ALEATORIAMENTE (UNIFORME) ENTRE VMAX E -VMAX
        PVEL(I,J) = VMAX(J)*(2.D0*UNI() - 1.D0)
      ENDDO
    ENDDO
      ! 1 PART�CULA PODE TER O VALOR TESTE DO ARQUIVO DE DADOS OPT
        DO J=1,NPAR
          P(1,J) = PARAM(J)
        ENDDO
      !ABRIR ARQUIVO DE REGIAO DE CONFIAN�A
      IF(FLAG_REGCONF .EQ. 1) THEN
        OPEN (NEWUNIT=SAIDA_SWARM_POPULACAO_FNUM,FILE='OUTPUT/SAIDA_SWARM_POPULACAO.DAT',STATUS='REPLACE',ACTION='WRITE')
        write(SAIDA_SWARM_POPULACAO_FNUM,*) '(IT),(I),F(I),P(I,:)'
        !TODO: writear os YCs aqui, para poder ter eles no filtrado, para poder fazer nuvems de Y
      ENDIF
      !INICIO DAS ITERA��S
      DO IT = 1,NIT
        DO I=1,NPS
          !PASSA VALORES DE PARAMETOR DA PARTICULA PARA A MEMORIA GLOBAL. TODO: FAZER PASSAGEM POR ARGUMENTO
          PARAM(:) = P(I,:)
          !CALCULA FUN��O OBJETIVO PARA TAIS PAR�METROS
          ! TODO: FAZER UMA FUNCTION FUN��O OBJETIVO
          CALL OBJF(F(I))
        ENDDO
        IF (FLAG_REGCONF .EQ. 1) THEN
          DO I=1,NPS
            !ARQUIVO  QUE VAI SER PARSED, FORMATA��O ESTRITA, SEM CABE�ALHO
            WRITE(SAIDA_SWARM_POPULACAO_FNUM,"("//INT_TO_CHAR(1+1+1+NPAR)//"(A,', '))") &
            LINT_TO_CHAR(IT),LINT_TO_CHAR(I),DSCI_TO_CHAR(F(I)),(DSCI_TO_CHAR(P(I,J)), J=1,NPAR)
          ENDDO
        ENDIF
        !PROCURAR MENOR OBJF DA ITERA��O ATUAL
        POS=MINLOC(F)
        ! VERIFICA SE A MENOR OBJF  DESTA ITERA��O � MENOR QUE O �TIMO GLOBAL ATUAL
        IF (F(POS(1)) .LT. FOTM) THEN
          ! ATUALIZA O VALOR �TIMA DA FUN��O OBJETIVO
          FOTM=F(POS(1))
          ! ATUALIZA A POSI��O �TIMA DOS PARAMETROS
          POTM=P(POS(1),:)
          ! ESCREVE O NOVO �TIMO GLOBAL NA TELA
          WRITE(*,"("//INT_TO_CHAR(1+1+1)//"(A,', '),"//INT_TO_CHAR(NPAR)//"(A,', ')"//")") &
          INT_TO_CHAR(IT), SCI_TO_CHAR(FOTM), SCI_TO_CHAR(1.D0 - DCHIDF(FOTM,DFLOAT(GL))), (SCI_TO_CHAR(POTM(J)),J=1,NPAR)
        ENDIF
        ! VERIFICA SE A MENOR FOBJ DE CADA PARTICULA DIMINUIU
        FORALL(I=1:NPS,F(I) .LT. FPT(I))
          !ATUALIZA O MINIMO PARTICULAR DE CADA PARTICULA
          FPT(I) = F(I)
          PPT(I,:) = P(I,:)
        ENDFORALL
        ! DEFINE EVOLU��O PARA W, EQUA��O 5.33
        W = WO + (WF-WO) * DFLOAT(IT-1) / DFLOAT(NIT-1)
        !ATUALIZA��O DO SWARM
        DO I=1,NPS
          DO J=1,NPAR
            !ATUALIZA��O DA VELOCIDADE, EQUA��O 5.31
            !CONTRIBUI��O NA DIRE��O ATERIOR +
            !CONTRIBUI��O NA DIRE��O DO MELHOR INDIVIDUAL +
            !CONTRIBUI��O NA DIRE��O DO MELHOR GLOBAL
            PVEL(I,J) = W*PVEL(I,J) + C1*UNI()*(PPT(I,J) - P(I,J)) + C2*UNI()*(POTM(J) - P(I,J))
            ! CONTROLE DE VELOCIDADE MAXIMA
            IF( ABS(PVEL(I,J)) .GT. VMAX(J) ) THEN
              PVEL(I,J) = SIGN(VMAX(J),PVEL(I,J))
            ENDIF
            ! ATUALIZA POSI��O, EQUA��O 5.32
            P(I,J) = P(I,J) + PVEL(I,J)
          ENDDO
        ENDDO
        ! VERIFICA SE OS LIMITES DE BUSCA FORAM ULTRAPASSADOS
        ! POSICIONA A PARTIULA NA FRONTEIRA E REDUZ A VELOCIDADE PARA A PROXIMA ITERA��O
        ! LIMITE INFERIOR
        FORALL(I=1:NPS, J=1:NPAR, P(I,J) .LT. PLMIN(J))
          P(I,J) = PLMIN(J)
          PVEL(I,J) = - PVEL(I,J)/2.D0
        ENDFORALL
        !LIMITE SUPERIOR
        FORALL(I=1:NPS,J=1:NPAR, P(I,J) .GT. PLMAX(J))
          P(I,J) = PLMAX(J)
          PVEL(I,J) = - PVEL(I,J)/2.D0
        ENDFORALL
      ENDDO
      !FIM DAS ITERA��ES: 
      !-------------------------------------------------------------------------
      !ENCERRAMENTO
      FOBJ = FOTM
      PARAM = POTM

      CALL OBJF(F(1)) ! chama a fobj �ltima vez p garantir que o �ltimo YC seja do �timo e o grafico_swarm sair legal

      IF(FLAG_REGCONF .EQ. 1) THEN
        CLOSE(SAIDA_SWARM_POPULACAO_FNUM)
      ENDIF
    ELSE
      WRITE(*,'(A)') 'N�MERO DE PART�CULAS NO SWARM MENOR OU IGUAL A ZERO, SWARM IGNORADO'
    ENDIF
  ENDSUBROUTINE
ENDMODULE
