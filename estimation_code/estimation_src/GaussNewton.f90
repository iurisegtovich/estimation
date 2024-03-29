MODULE GAUSSNEWTON_MOD
!  SCHWAAB, M. PINTO, J. C. AN�LISE DE DADOS EXPERIMENTAIS I FUNDAM
!  ENTOS DE ESTAT�STICA E ESTIMA��O DE PAR�METROS.EPAPERS, RIO DE JANEIRO,2007
!  SE��O 5.3 M�TODO DE GAUSS-NEWTON
!  GAUSS-NEWTON (ANDERSON ET AL.,AICHE J.,1978,24(1),20-29)
!  ACELERADOR DE LAW E BAILEY (LAW & BAILEY,CHEM.ENG.SCI.,1963,18,189-202)
  USE GLOBAL_MOD
  USE OBJF_MOD
!===============================================================================
  CONTAINS
!===============================================================================
  SUBROUTINE GAUSSNEWTON(NIT,FTOL,ALPHA_LB_TOL,PLMIN,PLMAX,DP,IT,FOBJ,COVPAR,COVPRED)
    !MINIMIZA��O DA FUN�AO OBJETIVO USANDO O METODO DE NEWTON COM
    !METODO DE ANALISE DE CONVERGENCIA DE LAW E BAILEY
    !USANDO A APROXIMA�AO DE GAUSS PARA A MATRIZ HESSIANA (GAUSS-NEWTON)
    !ANDERSON ET AL.,AICHE J.,1978,24(1),20-29
    !LAW & BAILEY 1963, SE��O (E)
    INTEGER :: I
    INTEGER :: K
    INTEGER :: M
    REAL(8) :: AUX
    INTEGER, INTENT(IN) :: NIT   ! N�MERO M�XIMO DE ITERA��ES
    !LAW & BAILEY
    REAL(8), PARAMETER :: R_LB = 2.    ! FATOR DE REDU��O NA LIMITA��O DO PASSO DOS PARAMETROS, FIG 6, BLOCO 15
    REAL(8), PARAMETER :: BETA_LB = 0.25   ! PAR�METRO DE CRIT�RIO DE ACELERE��O LAW & BAILEY, FIG6, BLOCO 14, EQ 27,
    REAL(8) :: ALPHA_LB    ! FATOR DE LIMITA��O DE PASSO, COME�A COMO 1 E DIMINUI COMO ALPHA_LB/2
    !GAUSSNEWTON
    REAL(8), INTENT(IN) :: FTOL    ! TOLER�NCIA RELATIVA PARA A FUN��O OBJETIVO
    REAL(8), INTENT(IN) :: ALPHA_LB_TOL  ! TOLER�NCIA ABSOLUTA NO PASSO NOS PARAMETROS
    REAL(8), INTENT(IN) :: PLMIN(NPAR)  ! FAIXA V�LIDA DOS PAR�METROS: MINIMO
    REAL(8), INTENT(IN) :: PLMAX(NPAR)  ! FAIXA V�LIDA DOS PAR�METROS: MAXIMO
    REAL(8), INTENT(IN) :: DP(NPAR)      ! PERTURBA��O RELATIVA NOS PAR�METROS
    INTEGER, INTENT(OUT) :: IT        ! N�MERO ITERA��ES REALIZADAS
    REAL(8), INTENT(OUT) :: FOBJ      ! VALOR DA FUN��O OBJETIVO
    REAL(8), INTENT(OUT) :: COVPAR(NPAR,NPAR)  ! MATRIZ DE COVARI�NCIA DOS PAR�METROS
    TYPE(M3_LIST), ALLOCATABLE, INTENT(OUT) :: COVPRED(:) !(NEXP,NSAI,NSAI)! MATRIZ DE COVARI�NCIA DE PREDI��O
    TYPE(M3_LIST), ALLOCATABLE :: DYCDP(:) !(NEXP,NSAI,NPAR)! DERIVADAS DA SA�DA DO MODELO EM RELA��O AOS PAR�METROS
                                         !MATRIZ DE SENSITIVIDADES "B" EQ 4.78, EXEMPLO 5.1
                                         !DERIVADAS DE YC EM RELA��O A PAR
    TYPE(M2_LIST), ALLOCATABLE :: EY(:) !(NEXP,NSAI)! DESVIOS DAS VARI�VEIS DE SA�DA: ERRO YC-YE
    REAL(8) T(NPAR,NPAR) !     ! 0.5D0 VEZES A APROXIMA��O DE GAUSS PARA A MATRIZ HESSIANA DA FUN��O OBJETIVO EM RELA��O AOS PARAMETROS (D2FDP2), EXEMPLO 5.1 VS EQ 5.12
    REAL(8) TINV(NPAR,NPAR)      ! INVERSA DE T
    REAL(8) U(NPAR)          ! VETOR 0.5D0 VEZES GRADIENTE DA FUN��O OBJETIVO EM RELA��O AOS PARAMETROS (DFDP), EXEMPLO 5.1 VS EQ 5.12
    REAL(8) STEPP(NPAR)    ! INCREMENTO NOS PAR�METROS
    REAL(8) FOBJN  ! VALOR ATUALIZADO DA FUN��O OBJETIVO
!    REAL(8) PARAMN  ! VALOR ATUALIZADO DOS APRAMETROS !TODO S� VAI SER �TIL DPS QUE O OBJF RECEBER OS PARAM VIA ARG
    REAL(8) DFL    ! INCREMENTO TOTAL NA FUN��O OBJETIVO CONSIDERANDO LINEAR E SEM RESTRI�OES DE LAW & BAILEY (FLIN(P+STEPP) - F(P)), DT, EQ 22
    REAL(8) DF    ! DELTA DA FUN��O OBJETIVO REAL (F(P+STEPP) - F(P))
    INTEGER :: SAIDA_GAUSSNEWTON_FNUM !NUMERO DE ARQUIVO PARA O HISTORICO DO GAUSS NEWTON
    CHARACTER(LEN=200) :: FORMATSTR
    !SPLASH
    WRITE(*,"(/,'GAUSS-NEWTON:')")
    !ALOCA��O DE MEMORIA LOCAL
    ALLOCATE(COVPRED(NMOD))
    ALLOCATE(EY(NMOD))
    ALLOCATE(DYCDP(NMOD))
    DO M = 1, NMOD
      ALLOCATE(COVPRED(M)%M(NEXP(M),NSAI(M),NSAI(M)))
      COVPRED(M)%M=0.d0
      ALLOCATE(EY(M)%M(NEXP(M),NSAI(M)))
      EY(M)%M=0.d0
      ALLOCATE(DYCDP(M)%M(NEXP(M),NSAI(M),NPAR))
      DYCDP(M)%M=0.d0
    ENDDO
    !INICIALIZA��O ARQUIVO
    ! ARQUIO DE SAIDA DOS DADOS OBTIDOS AO LONGO DAS ITERA��ES
    OPEN (NEWUNIT=SAIDA_GAUSSNEWTON_FNUM,FILE='OUTPUT/SAIDA_GAUSSNEWTON.DAT',STATUS='REPLACE',ACTION='WRITE')
    !---------------------------------------------------------------------------
    ! INICIO DAS ITERA��ES
    !---------------------------------------------------------------------------
    if (NIT==0) then
    print*, 'pulando gn'
    return
    endif
    print*, 'iniciando gn'

    IT=0
    ! C�LCULO DA FUN��O OBJETIVO
    CALL OBJF(FOBJ)
    !LOOP DO METODO DE NEWTON
    DO !GAUSSNEWTON
      ! ESCREVE OS RESULTADOS INTERMEDI�RIOS
      FORMATSTR="(/,2X,'ITERACAO ',I4)"
      WRITE(*,FORMATSTR)                      IT
      WRITE(SAIDA_GAUSSNEWTON_FNUM,FORMATSTR) IT
      ! ADICIONA UMA ITERA��O
      IT=IT+1
      ! INICIALIZA��O DO PARAMETRO DE ALW & BAILEY PARA LIMITA��O DO PASSO
      ALPHA_LB=1.D0
      ! C�LCULO DOS DESVIOS DAS VARI�VEIS (CALCULADO - EXPERIMENTAL, CONVEN��O DE SINAIS INVERSA AO LIVRO, )
      DO M = 1, NMOD
        EY(M)%M(:,:)=YC(M)%M(:,:)-YE(M)%M(:,:)
      ENDDO
      ! C�LCULO DAS PRIMEIRAS DERIVADAS DO MODELO
      CALL JACOBIAN(DP,DYCDP)
      !-------------------------------------------------------------------------
      ! APROXIMA��O DE GAUSS
      !-------------------------------------------------------------------------
      ! C�LCULO DA MATRIZ T[NPAR,NPAR]
      ! HALPHA/2 = BT VY^-1 B, EQ. 4.77
      T=0.D0
      DO M = 1, NMOD
        DO K=1,NEXP(M)
          T = T + MATMUL(TRANSPOSE(DYCDP(M)%M(K,:,:)),MATMUL(VARYEINV(M)%M(K,:,:),DYCDP(M)%M(K,:,:)))
        ENDDO
      ENDDO
      CALL INVERSAOMATRICIAL(NPAR,T,TINV)
      !-------------------------------------------------------------------------
      ! METODO DE NEWTON
      !-------------------------------------------------------------------------
      ! C�LCULO DO VETOR U[NPAR]
      !                        B^T                    VY^-1               YI^E-FI^0, EXEMPLO 5.1; FI^0 � O VALOR CALCULADO COM UMA ESTIMATIVA DE PARAMETRO
      U=0.D0
      DO M = 1, NMOD
        DO K=1,NEXP(M)
          U = U + MATMUL(TRANSPOSE(DYCDP(M)%M(K,:,:)),MATMUL(VARYEINV(M)%M(K,:,:),EY(M)%M(K,:)))
        ENDDO
      ENDDO
      ! C�LCULO DO INCREMENTO NOS PAR�METROS STEPP[NPAR]
      !PASSO, EXEMPLO 5.1, EQ 5.12 DO METODO DE NEWTON
      STEPP = -MATMUL(TINV,U)


      if (useLB) then
      !-------------------------------------------------------------------------
      ! M�TODO LAW & BAILEY PARA VERIFICAR CONVERGENCIA E ACELARA��O
      !-------------------------------------------------------------------------
      ! CALCULO DA VARIA��O DA FUN��O OBJETIVO LINEARIZADA DT, EQ 22, CALCULADO COM BASE  NAS EQ 21, 18, 16, 15
      DFL=0.D0
      DO M = 1, NMOD
        DO K=1,NEXP(M)
          !OBTEM ESTIMATIVA DE FOBJ COM OS YCALC NOVOS
          DFL=DFL + DOT_PRODUCT(EY(M)%M(K,:),MATMUL(VARYEINV(M)%M(K,:,:),MATMUL(DYCDP(M)%M(K,:,:),STEPP(:))))
          !CONFERIDO COM MODELO REGRESS�O LINEAR (ESPERADA = REALIZADA)
        ENDDO
      ENDDO
    
      !CORRIGE A DIRE��O DA BUSCA SE NECESS�RIO: LAW & BAILEY, SE��O (E), CRITERIO 1
      IF (DFL > 0.D0) THEN
        DFL=-DFL
        STEPP=-STEPP
      ENDIF
      
      !REPORT PASSO NOS PAR�METROS
      WRITE(*,"(2X,"//"'VARIACAO DOS PARAMETROS :',"//TRIM(INT_TO_CHAR(NPAR))//"(/,'PAR',I0.0,2X,E12.6,' + (',E12.6,') = ',E12.6))") &
        (I, PARAM(I), STEPP(I), PARAM(I)+STEPP(I), I=1,NPAR)
      WRITE(SAIDA_GAUSSNEWTON_FNUM,"(2X,"//"'VARIACAO DOS PARAMETROS :',"//TRIM(INT_TO_CHAR(NPAR))//"(/,'PAR',I0.0,2X,E12.6,' + (',E12.6,') = ',E12.6))") &
        (I, PARAM(I), STEPP(I), PARAM(I)+STEPP(I), I=1,NPAR)

      !REPORT VARIA��O ESPERADA NA OBFJ
      WRITE(*,"(2X,'VARIACAO ESPERADA DA FOBJ : ',2X,E12.6,' + (',E12.6,') = ',E12.6)") &
        FOBJ,DFL,FOBJ+DFL
      WRITE(SAIDA_GAUSSNEWTON_FNUM,"(2X,'VARIACAO ESPERADA DA FOBJ : ',2X,E12.6,' + (',E12.6,') = ',E12.6)") &
        FOBJ,DFL,FOBJ+DFL

      !TESTE DE RESPEITO AOS LIMITES IMPOSTOS NOS PAR�METROS
      I = 1
      DO
        AUX = PARAM(I) + ALPHA_LB*STEPP(I)
        IF((AUX > PLMIN(I)) .AND. (AUX < PLMAX(I))) THEN
          I = I + 1
        ELSE
          ALPHA_LB = ALPHA_LB/R_LB
          IF (ALPHA_LB < ALPHA_LB_TOL) THEN
            WRITE(*,*) '(AUX > LIM(I,1)) .AND. (AUX < LIM(I,2)) RETURNS FALSE'
            WRITE(*,*) 'STEPP NA GAUSSNEWTON MUITO GRANDE'
            WRITE(*,*) 'UM LIMITE DOS PARAMETROS ESTA SENDO ULTRAPASSADO'
            STOP
          ENDIF
        ENDIF
        IF (I > NPAR) EXIT
      ENDDO
      
      !CORRE��O DOS VALORES DAS VARI�VEIS DE BUSCA
      !ALPHA_LB COME�A COM 1.
      PARAM = PARAM + ALPHA_LB*STEPP
      !VERIFICA SE CHEGOU EM UM M�NIMO
      CALL OBJF(FOBJN)
      FORMATSTR = "(2X,'VARIACAO REALIZADA DA FOBJ : ',2X,E12.6,' + (',E12.6,') = ',E12.6)"
      WRITE(*,FORMATSTR) FOBJ,FOBJN-FOBJ,FOBJN
      WRITE(SAIDA_GAUSSNEWTON_FNUM,FORMATSTR) FOBJ,FOBJN-FOBJ,FOBJN
      IF ( (DABS(FOBJN-FOBJ)/FOBJN < FTOL)) THEN
        !OBTIDA CONVERGENCIA (CRITERIO RELATIVO) NA FOBJ
        FOBJ=FOBJN
        EXIT
      ENDIF
      ! CONTROLE DE CONVERG�NCIA DE LAW&BAILEY
      DO !LAW&BAILEY
        FORMATSTR = "(2X,'ALPHA_LB, FOBJN:',2X,E12.6,E12.6)"
        WRITE(SAIDA_GAUSSNEWTON_FNUM,FORMATSTR) ALPHA_LB, FOBJN
        ! CALCULA A VARIA��O REAL NA FUN��O OBJETIVO
        DF=FOBJN-FOBJ
        !VERIFICA SE HA CONVERG�NCIA PARA UM M�NIMO
        !FOBJ DIMINUI   !CRITERIO DE L&B
        !LAW & BAILEY 1963, CRITERIO 2, EQ (25), CONVE��O DE SINAIS INVERTIDA
        IF (DF .LT. 0.D0) THEN
          !'DF .LT. 0.D0, FUN��O OBJETIVO DIMINUINDO'
          !LAW & BAILEY 1963, CRITERIO 3, EQ (27), CONVE��O DE SINAIS INVERTIDA; FIG 6, BLOCO 14
          IF(DF-BETA_LB*(2.D0*ALPHA_LB-ALPHA_LB**2)*DFL .LT. 0.D0) THEN
            !'(DF-BETA_LB*(2.D0*ALPHA_LB-ALPHA_LB**2)*DFL < 0.D0), PASSA NO CRITERIO DE ACELERA��O DE LB'
            !ATUALIZA VALOR ACEITO DA FUNCAO OBJETIVO
            FOBJ=FOBJN
            !SAI DO LAW & BAILEY
            EXIT 
          ELSE
            WRITE(*,'(A)') 'CRITERIO DE LB ATIVADO (ACELERA��O)'
          ENDIF
        ELSE
          WRITE(*,'(A)') 'CRITERIO DE LB ATIVADO (FREIO)'
        ENDIF
        
        !CRIT�RIO DE LAW&BAILEY ATIVADO
        !REBOBINA ! TODO: EM VEZ DE REBOBINAR, ADIAR APLICA��O DA ATUALIZA��O DO PARAMETRO
        PARAM = PARAM - ALPHA_LB*STEPP
        !REDUZ-SE O TAMANHO DO PASSO E CALCULA-SE OS NOVOS VALORES
        ALPHA_LB=ALPHA_LB/R_LB
        WRITE(*,"(2X,'REDUCAO DE PASSO (ALPHA_LB) =',E12.6,5X,'NOVA FUNCAO OBJETIVO =',E12.6)") &
        ALPHA_LB, FOBJN
        ! 1 -> 1/2 -> 1/2/2 -> ...
        IF (ALPHA_LB .LT. ALPHA_LB_TOL) THEN
          WRITE(*,*) '(ALPHA_LB < ALPHA_LB_TOL):'
          WRITE(*,*) 'METODO LAW & BAILEY NAO CONVERGIU'
          STOP
        ENDIF
        !AVAN�A
        PARAM = PARAM + ALPHA_LB*STEPP
        ! RECALCULAR FUN��O OBJETIVO
        CALL OBJF(FOBJN)
      ENDDO !LAW&BAILEY

      else !not useLB
        PARAM = PARAM + STEPP
	
        CALL OBJF(FOBJN)

	print*, PARAM, '-->', FOBJN

        IF ( (DABS(FOBJN-FOBJ)/FOBJN < FTOL)) THEN
          !OBTIDA CONVERGENCIA (CRITERIO RELATIVO) NA FOBJ
          FOBJ=FOBJN
          EXIT
        ENDIF
	FOBJ=FOBJN
      endif !useLB

      IF (IT .GE. NIT) THEN
        WRITE(*,*) '(IT .GE. NIT)'
        WRITE(*,*) 'O NUMERO MAXIMO DE ITERACOES NA GAUSSNEWTON/LAWBAILEY FOI EXCEDIDO'
        !STOP
        EXIT !DO GAUSSNEWTON
      ENDIF
    ENDDO !GAUSSNEWTON
      !-------------------------------------------------------------------------
      ! C�LCULO DA MATRIZES DE COVARI�NCIA DOS PARAMETROS E DE PREDI��O
      ! RECALCULAR USANDO O STEP NOS PAR�METROS RESTRITO
      !-------------------------------------------------------------------------
      CALL JACOBIAN(DP,DYCDP)!TODO APROVEITAR CALCULOS ACIMA OU FUNCIONALIZAR E REUTILIZAR IMPLEMENTA��O
      T=0.D0!TODO APROVEITAR CALCULOS ACIMA OU FUNCIONALIZAR E REUTILIZAR IMPLEMENTA��O
      DO M = 1, NMOD!TODO APROVEITAR CALCULOS ACIMA OU FUNCIONALIZAR E REUTILIZAR IMPLEMENTA��O
        DO K=1,NEXP(M)!TODO APROVEITAR CALCULOS ACIMA OU FUNCIONALIZAR E REUTILIZAR IMPLEMENTA��O
        !EXEMPLO 5.1V ! TODO DE NOVO ISSO AQUI!TODO APROVEITAR CALCULOS ACIMA OU FUNCIONALIZAR E REUTILIZAR IMPLEMENTA��O
          T = T + MATMUL(TRANSPOSE(DYCDP(M)%M(K,:,:)),MATMUL(VARYEINV(M)%M(K,:,:),DYCDP(M)%M(K,:,:)))!TODO APROVEITAR CALCULOS ACIMA OU FUNCIONALIZAR E REUTILIZAR IMPLEMENTA��O
        ENDDO!TODO APROVEITAR CALCULOS ACIMA OU FUNCIONALIZAR E REUTILIZAR IMPLEMENTA��O
      ENDDO!TODO APROVEITAR CALCULOS ACIMA OU FUNCIONALIZAR E REUTILIZAR IMPLEMENTA��O
      ! C�LCULO DA INVERSA DE T[NPAR,NPAR]
      CALL INVERSAOMATRICIAL(NPAR,T,TINV) !TODO APROVEITAR CALCULOS ACIMA OU FUNCIONALIZAR E REUTILIZAR IMPLEMENTA��O
    ! MATRIZ DE COVARI�NCIA DOS PAR�METROS
    !VALPHA, EQ. 4.82
      COVPAR = TINV
    ! MATRIZ DE COVARIANCIA DE PREDI��O, VY, EQ 4.56
      DO M = 1, NMOD
        DO K=1,NEXP(M)
          COVPRED(M)%M(K,:,:)=MATMUL(DYCDP(M)%M(K,:,:),MATMUL(COVPAR,TRANSPOSE(DYCDP(M)%M(K,:,:))))
        ENDDO
      ENDDO
    !---------------------------------------------------------------------------
    !ENCERRAMENTO
    !---------------------------------------------------------------------------
    ! FECHAR ARQUIVO DE SAIDA DOS DADOS
    CLOSE (SAIDA_GAUSSNEWTON_FNUM)
    ! LIBERA��O MEMORIA
    DO M = 1, NMOD
      DEALLOCATE(DYCDP(M)%M)
      DEALLOCATE(EY(M)%M)
    ENDDO
    DEALLOCATE(DYCDP)
    DEALLOCATE(EY)
  ENDSUBROUTINE
!===============================================================================
  SUBROUTINE JACOBIAN(DP,DYCDP)
    ! C�LCULO DAS DERIVADAS DO MODELO COM RELA��O AOS PAR�METROS  !
    REAL(8), INTENT(IN) :: DP(NPAR)    ! PERTURBA��ES PARA O C�LCULO DAS DERIVADAS
    TYPE(M3_LIST), INTENT(INOUT) :: DYCDP(:) ! DERIVADAS EM RELA��O A PAR�METROS
    REAL(8), ALLOCATABLE :: AUX1(:) ! MEMORIA AUXILIAR, ALOCA��O AUTOM�TICA
    REAL(8), ALLOCATABLE :: AUX2(:) ! MEMORIA AUXILIAR, ALOCA��O AUTOM�TICA
    REAL(8) :: PARD(NPAR)  !PARAMETROS COM PERTURBA��O
    ! VARI�VEIS LOCAIS
    INTEGER :: I
    INTEGER :: K
    INTEGER :: M
    ! INICIALIZANDO O VETOR COM OS PAR�METROS PERTURBADOS
    PARD=PARAM
    DO M = 1, NMOD
      DO K=1,NEXP(M)
        DO I=1,NPAR
          ! CALCULA VARIAVEIS DE SAIDA DO MODELO COM PARAMETROS PERTURBADOS
          PARD(I)=PARAM(I)+DP(I)
          AUX1=ZEROS(NSAI(M))
          CALL PMODELS(M=M,VENT=XE(M)%M(K,:),VSAI=AUX1,PARS=PARD,GUESS=YE(M)%M(K,:))
          PARD(I)=PARAM(I)-DP(I)
          AUX2=ZEROS(NSAI(M))
          CALL PMODELS(M=M,VENT=XE(M)%M(K,:),VSAI=AUX2,PARS=PARD,GUESS=YE(M)%M(K,:))
          !RESTAURA VALORES CENTRAIS PARA O VETOR DE PARAMETROS PERTURBADO
          PARD(I)=PARAM(I)
          ! CALCULA A DERIVADA DO MODELO COM RELA��O A CADA PAR�METRO
          DYCDP(M)%M(K,:,I)=(AUX1-AUX2)/(2.D0*DP(I))
        ENDDO
      ENDDO
    ENDDO
  ENDSUBROUTINE
!===============================================================================
  SUBROUTINE INVERSAOMATRICIAL(ND,M,INVM)
  ! C�LCULO DA INVERSA DE UMA MATRIZ
  ! USANDO O M�TODO DE ELIMINA��O DE GAUSS COM PIVOTA��O PARCIAL
  INTEGER, INTENT(IN)  :: ND      ! DIMENS�O DAS MATRIZES
  REAL(8), INTENT(IN)  :: M(:,:)  ! MATRIZ ORIGINAL
  REAL(8), INTENT(OUT) :: INVM(:,:)  ! MATRIZ INVERTIDA
  INTEGER :: I
  INTEGER :: J
  REAL(8) :: A(ND,2*ND)
  REAL(8) :: B(2*ND)
  REAL(8) :: AUX
  ! INICIALIZANDO A MATRIZ A <-- [M]
    A = 0.D0
    A(1:ND,1:ND) = M
    FORALL (I=1:ND)
      A(I,I+ND) = 1.D0
    ENDFORALL
  ! INICIO DO PROCEDIMENTO DE INVERS�O
    DO I = 1,ND
      AUX = A(I,I)
      DO J = I+1,ND
        IF (DABS(A(J,I)) <= DABS(AUX)) THEN
          CYCLE
        ENDIF
        B(:) = A(J,:)
        A(J,:) = A(I,:)
        A(I,:) = B(:)
        AUX = A(I,I)
      ENDDO
      ! VERIFICA A POSSIBILIDADE DA MATRIZ SER N�O INVERS�VEL
      IF (DABS(AUX) < 1.D-10) THEN
        WRITE(*,*) '(DABS(AUX) < 1.D-10): MATRIZ NAO INVERSIVEL'
      ENDIF
      ! CONTINUA
      A(I,:) = A(I,:)/AUX
      DO J = 1,ND
        IF (I == J) THEN
          CYCLE
        ENDIF
        AUX = A(J,I)
        A(J,:) = A(J,:) - AUX*A(I,:)
      ENDDO
    ENDDO
  ! RESULTADO MATRIZ INVERSA: A --> [INVM]
    INVM = A(:,ND+1:2*ND)
  ENDSUBROUTINE
!===============================================================================
ENDMODULE
