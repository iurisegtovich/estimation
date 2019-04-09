module modelo_mod
  use global_mod
  !-----------------------------------------------------------
  contains
  !-----------------------------------------------------------
  
  subroutine modelo(m,varent, varsai, param, guess)
    integer :: m
    real(8), intent(in) :: varent(:)
    real(8), intent(out) :: varsai(:)
    real(8), intent(in) :: guess(:)
    real(8) :: param(:)
    select case(m)
    case(1)
      call modelo_A(varent, varsai, param, guess)
    case(2)
!      call modelo_B(varent, varsai, param, guess)
    endselect
  endsubroutine
  !-----------------------------------------------------------
  
  subroutine modelo_A(varent, varsai, param, guess)
    real(8), intent(in) :: varent(:)
    real(8), intent(out) :: varsai(:)
    real(8), intent(in) :: guess(:)
    real(8) :: param(:)
    !modelo
    real(8) :: Psat_w_ice, T, PSATpar(3), DT, RT, iRT
    T = varent(1)
    PSATpar(:) = param(:)
    DT = T-273.16d0
    RT = (1.d0 - T/273.16d0)
    iRT = (273.16d0/T - 1.d0)
    Psat_w_ice = 611.657*dexp(PSATpar(1)*(RT)*(dabs(RT))**(PSATpar(2)-1.d0)+PSATpar(3)*(iRT))
    varsai(1) = Psat_w_ice
  endsubroutine
  subroutine modelo2(varent, varsai, param, guess)
    real(8), intent(in) :: varent(:)
    real(8), intent(out) :: varsai(:)
    real(8), intent(in) :: guess(:)
    real(8) :: param(:)
    !DECLARAÇÃO DE VARIÁVEIS PARA A DASSL
    EXTERNAL RES, JAC
    INTEGER NEQ, INFO(15), IDID, LRW, LIW, IWORK(22), IPAR(1)
    DOUBLE PRECISION T, Y(2), YPRIME(2), TOUT, RTOL, ATOL, RWORK(62), RPAR(3)
    !DECLARAÇÃO DE VARIÁVEIS DO PROBLEMA
    DOUBLE PRECISION H, FO, ST, KV, FI
    DOUBLE PRECISION HPRIME,FOPRIME
    DOUBLE PRECISION DT, TEND
    !VARIÁVEIS AUXILIARES DO PROGRAMA
    character( len = 99 ) :: path
    INTEGER :: pathFnum, saidaFnum
    !FORMULAÇÃO DO PROBLEMA
    
    !ATRIBUINDO VALORES ÀS CONSTANTES
    ST=1.D0        !ÁREA TRANSVERSAL DO TANQUE
    KV=1.D0        !CONSTANTE DA VÁLVULA
    
    !DEFININDO A CONDIÇÃO DE CONTORNO
    FI=1.D0        !VAZÃO DE ENTRADA
    
    !DEFININDO CONDIÇÕES INICIAIS DO PROCESSO
    H =0.D0        !ALTURA DE LÍQUIDO NO TANQUE
    FO=0.D0        !VAZÃO DE SAÍDA
    
    !DEFININDO ESTIMATIVAS INICIAIS PARA O MÉTODO NUMÉRICO
    HPRIME=FI/ST   !DERIVADA DE H EM RELAÇÃO AO TEMPO
    FOPRIME=KV*FI/ST   !DERIVADA DE F0 EM RELAÇÃO AO TEMPO
    
    !INICIANDO OS PARÂMETROS DA DASSL
    T=0.D0         !TEMPO ATUAL (TEMPO INICIAL NESSE CASO)
    DT=1.D-1       !TAMANHO DO PASSO NO TEMPO
    TEND=20.D0        !DURAÇÃO DA SIMULAÇÃO DO ENCHIMENTO DE TANQUE
    Y(1)=H         !PASSAGEM DA CONDIÇAO INICIAL PARA UM VETOR LEGIVEL PELA DASSL
    Y(2)=FO        !PASSAGEM DA CONDIÇAO INICIAL PARA UM VETOR LEGIVEL PELA DASSL
    YPRIME(1)=HPRIME     !PASSAGEM DA CONDIÇAO INICIAL PARA UM VETOR LEGIVEL PELA DASSL   
    YPRIME(2)=FOPRIME    !PASSAGEM DA CONDIÇAO INICIAL PARA UM VETOR LEGIVEL PELA DASSL
    TOUT=T+DT      !TEMPO FINAL DDO PRIMEIRO PASSO DA INTEGRAÇÃO NO TEMPO
    INFO=0         !VETOR CONTENDO OPÇOES DA DASSL. DEFAULT=0
    RTOL=1.D-6     !TOLERÂNCIA RELATIVA
    ATOL=1.D-8     !TOLERÂNCIA ABSOLUTA
    NEQ=2          !NÚMERO DE EQUAÇÕES
    LIW=22         !O CÁLCULO DESSE VALOR É ENSINADO NO CÓDIGO DA DASSL
    LRW=62         !O CÁLCULO DESSE VALOR É ENSINADO NO CÓDIGO DA DASSL
    !PASSAGEM DE VARIÁVEIS PARA A SUBROTINA RES()
    RPAR(1)=ST
    RPAR(2)=KV
    RPAR(3)=FI
    
    !O PROGRAMA PRECISA DETECTAR O CAMINHO RELATIVO DESEJADO PARA ESCREVER O ARQUIVO DE SAÍDA
    open( newunit = pathFnum, file = 'PUT "path.txt" HERE', action = 'write', status = 'replace')
    close( pathFnum )
    open( newunit = pathFnum, file = 'path.txt', action = 'read', status = 'old')
    read( pathFnum, * ) path
    close( pathFnum )
    
    !ABRINDO ARQUIVOS PARA GRAVAÇÃO DE DADOS
    open( newunit = saidaFnum , file = trim(path)//'SAIDA.TXT', action = 'write', status = 'replace')
    WRITE(saidaFnum,*) 'TEMPO   H     FO   dHdt     dFOdt'
    WRITE(*,*)         'TEMPO   H     FO   dHdt     dFOdt'
    WRITE(saidaFnum,"(5(F6.3, 1x))") T,Y(:) !ESCREVE NO ARQUIVO DE SAÍDA O TEMPO, A ALTURA E A VAZÃO DE SAÍDA
    WRITE(*,"(5(F6.3, 1x))") T,Y(:)
    
    !ROTINA DE CHAMADA DA DASSL PARA INTEGRAÇÃO DO SISTEMA DE DAEs, EM PASSOS DE TEMPO DT, ATÉ O TEMPO FINAL TEND
    DO WHILE (T < TEND)
    CALL DDASSL (RES, NEQ, T, Y, YPRIME, TOUT, INFO, RTOL, ATOL, IDID, RWORK, LRW, IWORK, LIW, RPAR, IPAR, JAC) !INTEGRA O O SISTEMA
    TOUT=T+DT
    WRITE(saidaFnum,"(5(F6.3, 1x))") T,Y(:), YPRIME(:)
    WRITE(*,"(5(F6.3, 1x))") T,Y(:), YPRIME(:)
    ENDDO
    close(saidaFnum)
  
  endsubroutine
  
  subroutine modelo3(varent, varsai, param, guess)
    real(8), intent(in) :: varent(:)
    real(8), intent(out) :: varsai(:)
    real(8), intent(in) :: guess(:)
    real(8) :: param(:)
  
REAL(8) :: qmax, k, V, Ci, m, Ceq, a, b, c, d
  !Dados do sistema
  qmax = Param(1)
  k = Param(2)
  V = VarEnt(1)
  Ci = VarEnt(2)
  m = VarEnt(3)
  a=k*V
  b=(m*qmax*k+V-k*V*Ci)
  c=(-V*Ci)
  d=b**2-4.d0*a*c
  if (d > 0.) then
    Ceq = (-b+DSQRT(d))/(2.d0*a)
  else
    Ceq=-100.d0
  endif
  VarSai(1) = Ceq
  
  endsubroutine
  
endmodule
