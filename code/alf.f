C
       FUNCTION ALPHAS(Q2)
       IMPLICIT DOUBLE PRECISION (A - Z)
       INTEGER NF, K, IIP
       DIMENSION LAMBDA (3:5), Q2THR (2)
       COMMON /IPART/ IIP
*...HEAVY QUARK THRESHOLDS
*       IF(IIP.EQ.0) THEN
C *** GRV     
*         Q2THR(1) = 2.25D0
*         Q2THR(2) = 20.25D0
*         LAMBDA(3)=0.248D0
*         LAMBDA(4)=0.200D0
*         LAMBDA(5)=0.131D0
C
C *** GRV-98:    LO
C         Q2THR(1) = 1.96D0
C         Q2THR(2) = 20.25D0
C         LAMBDA(3)=0.204D0
C         LAMBDA(4)=0.175D0
C         LAMBDA(5)=0.132D0
C *** GRV-98:     NLO
c         Q2THR(1) = 1.96D0
c         Q2THR(2) = 20.25D0
c         LAMBDA(3)=0.2994D0
c         LAMBDA(4)=0.246D0
c         LAMBDA(5)=0.1677D0
C
C
*       ELSE IF(IIP.EQ.1) THEN 
C *** CTEQ2M
C         Q2THR(1) = 2.56D0
C         Q2THR(1) = 2.24D0
C         Q2THR(1) = 1.D0
C         Q2THR(2) = 25.D0
C         LAMBDA(3)=0.000D0
C         LAMBDA(4)=0.213D0
C         LAMBDA(5)=0.139D0
C *** CTEQ4M
*         Q2THR(1) = 0.D0
*         Q2THR(2) = 25.D0
*         LAMBDA(3)=0.000D0
*         LAMBDA(4)=0.298D0
*         LAMBDA(5)=0.202D0
C *** CTEQ5M
         Q2THR(1) = 0.D0
         Q2THR(2) = 20.25D0
         LAMBDA(3)=0.000D0
         LAMBDA(4)=0.326D0
         LAMBDA(5)=0.226D0
C *** 5L
C         LAMBDA(4)=0.192D0
C         LAMBDA(5)=0.146D0
C
*       ELSE IF(IIP.EQ.2) THEN 
C *** MRS-A'
*         Q2THR(1) = 2.6D0
*         Q2THR(1) = 2.24D0
*         Q2THR(1) = 1.D0
*         Q2THR(2) = 30.D0
*         LAMBDA(3)=0.000D0
*         LAMBDA(4)=0.231D0
*         LAMBDA(5)=0.151D0
C
*       ENDIF
C
       NF = 3
       DO 10 K = 1, 2
       IF (Q2 .GE. Q2THR (K)) THEN
          NF = NF + 1
       ELSE
          GO TO 20
       END IF
  10   CONTINUE
  20   B0 = 11.D0- 2.D0/3.D0* NF
       B0S = B0 * B0
       B1 = 102.D0- 38.D0/3.D0* NF
*
*       b1 = 0.D0
*
       LAM2 = LAMBDA (NF) * LAMBDA (NF)
       LQ2 = DLOG (Q2 / LAM2)
       ALPHAS = 1.D0/ (B0 * LQ2) * (1.D0- B1 / B0S * DLOG (LQ2) / LQ2)
       RETURN
       END
