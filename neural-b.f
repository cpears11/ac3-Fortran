C**********************************************************************
C Copyright H K D H Bhadeshia, University of Cambridge.
C November 1994; Modified March 1999.
C
C Based on paper by Gavard, Bhadeshia, MacKay and Suzuki
C Materials Science and Technology, 1995, submitted
C
C Program predicts the Ac1 and Ac3 temperatures
C  of steel as a function of
C  the chemical composition and the heating rate
C  It does not give detailed error bars,
C  which are discussed in the original paper.
C Program predicts the Ac3 temperature of steel as a function of
C  the chemical composition and the heating rate
C  It does not give detailed error bars,
C  which are discussed in the original paper.
C
C To use the output from neural network analysis
C  to predict austenite formation in steels.
C
C The coefficients, in the form of weights are stored in a file
C  called AC3 in unit 4
C To use the output from neural network analysis
C  to predict austenite formation in steels.
C
C The coefficients, in the form of weights are stored in a files
C  called AC1 and AC3.
C
C X contains normalized values of the input variables.
C W1 and W2 contain the weights.
C THETA1 and THETA2 are the biases.
C Y is the output (normalized).
C AMIN and AMAX contain the minimum and maximum unnormalized values of the
C input variables and of the output.
C IN is the number of inputs.
C IHID is the number of hidden units.
C IMAX is the number of heating rates calculated = 5.
C
      PROGRAM MAP_NEURAL_STEEL
C      
      IMPLICIT NONE
C
      DOUBLE PRECISION THETA2, THETB2, ERROR
      DOUBLE PRECISION AMIN(23),AMAX(23),AW(23),HT(20),THETA1(4)
      DOUBLE PRECISION THETB1(2),W1(4,22),W1B(2,22),W2(4),W2B(2),Y(20) 
C
      INTEGER IERR(20),I,IHID,IMAX,IN
C
      IMAX = 5
C
      OPEN(UNIT=10, FILE='ACINPUT')
      DO 100 I=1,22
         READ(10,*) AW(I)
  100 CONTINUE
C
      CALL WEIGH(AMIN,AMAX,THETA1,THETA2,W1,W2,IN,IHID)
      CALL WRITO(AW)
      CALL MAP_NEURAL_AC1TEMP(AW,W1,W2,THETA1,THETA2,
     &                      AMIN,AMAX,IMAX,Y,HT,IERR,IN,IHID,ERROR)
      WRITE(*,1)
      DO 110 I=1,IMAX,1
         IF(IERR(I) .EQ. 0) THEN
           WRITE(*,2) Y(I), ERROR, HT(I)
         ELSE
           WRITE(*,3) Y(I), ERROR, HT(I)
         ENDIF
  110 CONTINUE
C
      CALL WEIGHB(AMIN,AMAX,THETB1,THETB2,W1B,W2B,IN,IHID)
      CALL MAP_NEURAL_AC3TEMP(AW,W1B,W2B,THETB1,THETB2,
     &                      AMIN,AMAX,IMAX,Y,HT,IERR,IN,IHID,ERROR)
C
      WRITE(*,4)
      DO 120 I=1,IMAX,1
         IF(IERR(I) .EQ. 0) THEN
           WRITE(*,2) Y(I), ERROR, HT(I)
         ELSE
           WRITE(*,3) Y(I), ERROR, HT(I)
         ENDIF
  120 CONTINUE
      STOP
    1 FORMAT(///13X'   Ac1 / C       +- Error / C',
     &            '    Heating Rate / C/s')
    2 FORMAT(17X,F6.0,10X,F6.0,10X,F10.4)
    3 FORMAT(17X,F6.0,10X,'>'F5.0,10X,F10.4)
    4 FORMAT(///13X'   Ac3 / C       +- Error / C',
     &            '    Heating Rate / C/s')
      END


C********************************************************************** 
C
C To read the weights obtained from the neural network
C representation of weld metal Charpy toughness.
C H K D H Bhadeshia, University of Cambridge.
C
      SUBROUTINE WEIGH(AMIN,AMAX,THETA1,THETA2,W1,W2,IN,IHID)
      IMPLICIT NONE
      DOUBLE PRECISION W1(4,22), W2(4),THETA1(4)
      DOUBLE PRECISION AMIN(23),AMAX(23)
      DOUBLE PRECISION THETA2
C 
      INTEGER I, IDUM, IHID, IN, J
C
C Set number of inputs
C
      IN=22
C
C Set number of hidden units
C
      IHID=4
C
      OPEN(UNIT=1, FILE='AC1')
      DO 100 I=1,IN+1
         READ(1,*) IDUM, AMIN(I),AMAX(I)
  100 CONTINUE
C
      DO 120 I=1,IHID
         READ(1,*) THETA1(I)
c        WRITE(*,91) THETA1(I)
         DO 110 J=1,IN
            READ(1,*) W1(I,J)
  110    CONTINUE
  120 CONTINUE
C
      READ(1,*) THETA2
      DO 130 I=1,IHID
         READ(1,*) W2(I)
  130 CONTINUE
      RETURN
      END


C*************************************************************************
C Printing for the neural network representation of weld metal
C  toughness.
C H K D H Bhadeshia, University of Cambridge
C
      SUBROUTINE WRITO(A)
C
      IMPLICIT NONE
      DOUBLE PRECISION A(100)

      WRITE(*,5)

      WRITE(*,4) A(1),A(2),A(3),A(4), A(5),A(6),A(7),A(8),A(9),
     &   A(10),A(11),A(12),A(13),A(14),A(15),
     &   A(16),A(17),A(18),A(19),A(20),
     &   A(21)



C
C View input variables

5      FORMAT(//////
     &18X,'**      Austenite Formation Temperatures    **           '//
     &14X,' EUDIL (Univ. Lille I, France) &',
     &' Darwin College (Cambridge) '//
     &20X,'    Gavard, Bhadeshia, MacKay and Suzuki'///)

4      FORMAT(
     &10X,' Carbon      ',F6.3,'  wt.%',
     &9X,' Silicon           ',F6.3,'  wt.%'/
     &10X,' Manganese   ',F6.3,'  wt.%',9X,
     &' Sulphur           ',F6.3,'  wt.%'/
     &10X,' Phosphorus  ',F6.3,'  wt.%',
     &9X,' Copper            ',F6.3,'  wt.%'/
     &10X,' Nickel      ',F6.3,'  wt.%',9X,
     &' Chromium          ',F6.3,'  wt.%'/
     &10X,' Molybdenum  ',F6.3,'  wt.%   ',6X,
     &' Niobium            ',F6.4,' wt.%   '/
     &10X,' Vanadium    ',F6.3,'  wt.%   ',9X/
     &10X,' Titanium    ',F6.3,'  wt.%',
     &9X,' Aluminium         ',F6.3,'  wt.%'/
     &10X,' Boron        ',F6.4,' wt.%',9X,
     &' Tungsten          ',F6.3,'  wt.%'/
     &10X,' Arsenic     ',F6.3,'  wt.%',
     &9X,' Tin               ',F6.3,'  wt.%'/
     &10X,' Zirconium   ',F6.3,'  wt.%',9X,
     &' Cobalt            ',F6.3,'  wt.%'/
     &10X,' Nitrogen     ',F6.4,' wt.%   ',6X,
     &' Oxygen             ',F6.4,' wt.%   '//
     &//)

      RETURN
      END



C********************************************************************
C To read the weights obtained from the neural network
C representation of weld metal Charpy toughness
C H K D H Bhadeshia, University of Cambridge
C
C
      SUBROUTINE WEIGHB(AMIN,AMAX,THETB1,THETB2,W1B,W2B,IN,IHID)
      IMPLICIT NONE
      DOUBLE PRECISION W1B(2,22), W2B(2),THETB1(2)
      DOUBLE PRECISION AMIN(23),AMAX(23)
      DOUBLE PRECISION THETB2
C
      INTEGER I, IDUM, IHID, IN, J
C
C Set number of inputs
C
      IN=22
C
C Set number of hidden units
C
      IHID=2
C
      OPEN(UNIT=2, FILE='AC3')
      DO 100 I=1,IN+1
         READ(2,*) IDUM, AMIN(I),AMAX(I)
  100 CONTINUE
      DO 120 I=1,IHID
         READ(2,*) THETB1(I)
         DO 110 J=1,IN
            READ(2,*) W1B(I,J)
  110    CONTINUE
  120 CONTINUE
      READ(2,*) THETB2
      DO 130 I=1,IHID
         READ(2,*) W2B(I)
  130 CONTINUE
      RETURN
      END



