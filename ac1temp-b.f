C**********************************************************************
C copyright H K D H Bhadeshia, University of Cambridge
C November 1994
C
C Based on paper by Gavard, Bhadeshia, MacKay and Suzuki
C Materials Science and Technology, 1995, submitted
C
C Program predicts the Ac1 temperature of steel as a function of
C  the chemical composition and the heating rate
C  It does not give detailed error bars,
C  which are discussed in the original paper.
C
C To use the output from neural network analysis
C  to predict austenite formation in steels
C
C The coefficients, in the form of weights are stored in a file
C  called AC1 in unit 4
C
C IPAUSE allows a pause if the value is set to 1
C X contains normalized values of the input variables
C W1 and W2 contain the weights
C THETA1 and THETA2 are the biases
C Y is the output (normalized)
C AMIN and AMAX contain the minimum and maximum unnormalized values of the
C input variables and of the output
C
      SUBROUTINE MAP_NEURAL_AC1TEMP(AW,W1,W2,THETA1,THETA2,
     &AMIN,AMAX,IMAX,Y,HT,IERR,IN,IHID,ERROR)
C
      IMPLICIT NONE
      DOUBLE PRECISION  W1(4,22),W2(4),THETA1(4),H(4),ERROR,
     &AMIN(23),AMAX(23),AW(22),X(22),HT(20),Y(20),A,THETA2
      INTEGER IMAX,IERR(20),I,J,IN,IHID,II
C
C Remember to set matrix dimensions according to number of inputs
C   and number of hidden units. Currently set at 100 and 10 respectively.
C
      ERROR = 40D0
C
C
C
C Read a set of unnormalized inputs
C
C
C
         DO 222 I=1,IN
           X(I)=AW(I)
222      CONTINUE

C
C
C One of the variables, in this case the heating rate HT, is to be
C  varied, so at first normalize IN-1 variables


          DO 1 I=1,IN-1
            X(I) = ((X(I) -AMIN(I))/(AMAX(I)-AMIN(I)))-0.5D+00
 1         CONTINUE

          DO 20 II=1,IMAX
          HT(II) = 10.0D+00**(II)/1000.0D+00
          X(IN) = ((HT(II) -AMIN(IN))/(AMAX(IN)-AMIN(IN)))-0.5D+00
C
C IERR = 0  implies that all inputs are within the range of the
C  training dataset for the neural network. Thus, the 95% confidence
C  error bars amount to about +- 11%
C
C IERR = 1 implies that some inputs are out of the range of the
C  training dataset for the neural network. THhe 95% confidence
C  error bars MAY then be greater than +-11%. See Gavard, Bhadeshia, MacKay
C  and Suzuki for details
C
      IERR(II)=0
      DO 11 J=1,IN
         IF(X(J) .LT. -0.5D+00 .OR. X(J) .GT. 0.5D+00) THEN
           IERR(II)=1
         ENDIF
11    CONTINUE
C
C Read all weights, and theta parameters
C
C
C
C Do calculation of the normalized output value for the given set of
C  normalized input
C
C First equation
C
       DO 6 I=1,IHID
          A=0.0D+00
C
               DO 5 J=1,IN
                 A=A+W1(I,J)*X(J)
5              CONTINUE
C
          H(I)=DTANH(A+THETA1(I))
6      CONTINUE
C
C Second equation
C
       A=0.0D+00
       DO 7 I=1,IHID
          A=A+W2(I)*H(I)
7      CONTINUE
C
       Y(II) = A+THETA2
       Y(II)=(Y(II)+0.5D+00)*(AMAX(IN+1)-AMIN(IN+1))+AMIN(IN+1)
C
C Prevent temperature from acquiring a negative value
C
       IF(Y(II) .LT. 0.0D+00)Y(II)=0.0D+00
C
C
C
 20    CONTINUE
C
C
       RETURN
       END
