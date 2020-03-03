
      IMPLICIT NONE

      INTEGER           NTMP
      PARAMETER        (NTMP=2501)

      DOUBLE PRECISION  SCALE
      DOUBLE PRECISION  V(3)
      DOUBLE PRECISION  UX(3)
      DOUBLE PRECISION  UY(3)
      DOUBLE PRECISION  UZ(3)
      DOUBLE PRECISION  Z0, Z1, Z2
      DOUBLE PRECISION  X, Y
      DOUBLE PRECISION  C(0:3)

      REAL*4                HT(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                ALB(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                G(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                H(-NTMP:NTMP)
      REAL*4                DN(-NTMP:NTMP,NTMP)

      INTEGER            I, I1
      INTEGER            J, J1
      INTEGER            IC, IL
      INTEGER            JC, JL
      INTEGER            K, L
      INTEGER            QSZ
      INTEGER            Q, Q0, Q1

      CHARACTER*6        MAPNM
      CHARACTER*72       MAPFILE
      CHARACTER*72       INFILE
      CHARACTER*72       OUTFILE
      CHARACTER*5000     LINE

      LOGICAL            GUSE(-NTMP:NTMP,-NTMP:NTMP)

      WRITE(6,*) 'Input mapname'
      READ(5,FMT='(A6)') MAPNM

      MAPFILE='MAPFILES/'//MAPNM//'.MAP'
      CALL READ_MAP(MAPFILE,NTMP,QSZ,SCALE,V,UX,UY,UZ,HT,ALB)

      WRITE(6,*) 'Input center'
      READ(5,*) IC, JC

      WRITE(6,*) 'Input left'
      READ(5,*) IL, JL

      Z1=0
      Z2=0
      Q0=2*MAX(ABS(IC-IL), ABS(JC-JL))
      DO I=-Q0,Q0
      DO J=-Q0,Q0
        G(I,J)=0.E0
        GUSE(I,J)=.FALSE.
        IF((I+IC-QSZ-1.GE.-QSZ).AND.(I+IC-QSZ.LE.QSZ).AND.
     .      (J+JC-QSZ.GE.-QSZ).AND.(J+JC-QSZ.LE.QSZ)) THEN
          G(I,J)=HT(I+IC-QSZ-1,J+JC-QSZ)-HT(I+IC-QSZ,J+JC-QSZ)
          Z1=MIN(G(I,J),Z1)
          Z2=MAX(G(I,J),Z2)
          GUSE(I,J)=.TRUE.
        ENDIF
      ENDDO
      ENDDO
      DO I=-Q0,Q0
      DO J=-Q0,Q0
      IF(GUSE(I,J)) THEN
        G(I,J)=REAL((G(I,J)-Z1)/(Z2-Z1))
      ENDIF
      ENDDO
      ENDDO

      Z0=2*SQRT(1.D0*((IC-IL)**2+(JC-JL)**2))
      Q1=NINT(Z0)

      Z1= 1.D10
      Z2=-1.D10
      DO K=-Q1,Q1
        X=IC+K*(IC-IL)/Z0-0.5
        Y=JC+K*(JC-JL)/Z0
        I=NINT(X)
        J=NINT(Y)
        G(I-IC,J-JC)=1
        I=INT(X)
        J=INT(Y)
        X=X-I
        Y=Y-J
        L=-1
        DO J1=0,1
        DO I1=0,1
          L=L+1
          C(L)=HT(I+I1-QSZ, J+J1-QSZ)
        ENDDO
        ENDDO
        C(3)=C(0)-C(1)-C(2)+C(3)
        C(1)=C(1)-C(0)
        C(2)=C(2)-C(0)
        H(K)=REAL(C(0)+C(1)*X+C(2)*Y+C(3)*X*Y) 
        Z1=MIN(Z1,H(K))
        Z2=MAX(Z2,H(K))
      ENDDO
      K=INT(5*(Z2-Z1))+1

      Q=MAX(Q0,Q1)

      DO I=-Q,Q
        DO J=1,2*Q0+20+K
          DN(I,J)=0
        ENDDO
      ENDDO

      DO I=-Q0,Q0
      DO J=1,2*Q0+1
        DN(I,J)=G(I,J-Q0-1)
      ENDDO
      ENDDO

      DO I=-Q1,Q1
        J=NINT(5*(Z2-H(I)))+2*Q0+10+1
        DN(I,J)=1
      ENDDO

      INFILE='TEMPFILE.GRAY'
      OPEN(UNIT=10, FILE=INFILE, ACCESS='DIRECT',
     .     RECL=2*Q+1, STATUS='UNKNOWN')
        DO J=1,2*Q0+20+K
          DO I=-Q,Q
            LINE(I+Q+1:I+Q+1)=CHAR(NINT(255*DN(I,J)))
          ENDDO
          WRITE(10,REC=J) LINE(1:2*Q+1)
        ENDDO
      CLOSE(UNIT=10)

      OUTFILE='TEMPFILE.pgm'

      CALL RAW2PGM(INFILE,OUTFILE,2*Q+1,2*Q0+20+K)
      OPEN(UNIT=63, FILE=INFILE, STATUS='OLD')
      CLOSE(UNIT=63, STATUS='DELETE')

      STOP
      END


