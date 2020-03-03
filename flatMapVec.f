c  Version 1.0 - 1 January 2012 - Eric Palmer (from Gaskell)
c	This outputs a bigmap as a simple csv textfile, grid format
c  gfortran flatMap.f -O2 -o ~/bin/t.flatMap
c  Vers 1.1 - 23 Aug 2015 -- Output in radius. Used vector to calculate radius

      IMPLICIT NONE

      INTEGER               NTMP
      PARAMETER            (NTMP=5000)

      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      V(3)
      DOUBLE PRECISION      UX(3)
      DOUBLE PRECISION      UY(3)
      DOUBLE PRECISION      UZ(3)

      REAL*4                HT0(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                AL0(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                HT1(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                AL1(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                mVal, mX, mY
      REAL*4                minVal, minX, minY
      real                  radius

      INTEGER               QSZ
      INTEGER               QSZLESS
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               zeros

      CHARACTER*6           MAP0
      CHARACTER*6           MAP1
      CHARACTER*72          LMRKFILE

      WRITE(6,*) 'Input map name [6 char only]'
      READ(5,FMT='(A6)') MAP0

      LMRKFILE='MAPFILES/'//MAP0//'.MAP'
      CALL READ_MAP(LMRKFILE,NTMP,QSZ,SCALE,V,UX,UY,UZ,HT0,AL0)
      radius = sqrt (v(1)**2 + v(2)**2 + v(3)**2)

      LMRKFILE=MAP0//'.TXT'
      OPEN(UNIT=10,FILE=LMRKFILE)

      write (*,*) "# Units : meters"
      radius = radius * 1000

      write (*,*) "# QSize: ", QSZ
      write (*,*) "# Scale: ", SCALE * 1000
      write (*,*) "# Radius: ", radius

      DO J=-QSZ,QSZ
        DO I=-QSZ,QSZ
          if (HT0(I, J) .EQ. 0) then
            zeros = zeros+1
          endif
          Ht0(i,j) = Ht0(i,j) * 1000 * scale + radius
        ENDDO
      ENDDO

      mVal = -99999
      minVal = 99999
      zeros=0
      DO J=-QSZ,QSZ
        DO I=-QSZ,QSZ
          if (mVal .lt. HT0(I, J)) then
            mVal = HT0(I, J)
            mX = I
            mY = J
          endif
          if (minVal .gt. HT0(I, J)) then
            minVal = HT0(I, J)
            minX = I
            minY = J
          endif
        ENDDO
      ENDDO
      write (*,*) "# Max Val ", mVal, " (", mX, ", ", mY, ")"
      write (*,*) "# Min Val ", minVal, " (", minX, ", ", minY, ")"
      write (*,*) "# Zeros:  ", zeros

      DO J=-QSZ,QSZ
        write(10,240) ( HT0(I,J), I=-QSZ,QSZ )
      ENDDO
 240  format (f14.5, 2001(", ", f14.5))


      STOP
      END

c   ................................................
      SUBROUTINE READ_MAP(LMRKFILE,NTMP,QSZ,SCALE,V,UX,UY,UZ,HT,ALB)
c   ................................................

      IMPLICIT NONE
      
      INTEGER               NTMP
      INTEGER               QSZ
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               K0
      INTEGER               IX(24)
      INTEGER               JX(24)
      INTEGER               NREC

      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      HSCALE
      DOUBLE PRECISION      V(3)
      DOUBLE PRECISION      UX(3)
      DOUBLE PRECISION      UY(3)
      DOUBLE PRECISION      UZ(3)

      REAL*4                HT(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                ALB(-NTMP:NTMP,-NTMP:NTMP)

      CHARACTER*72          BLINE
      CHARACTER*72          LMRKFILE

      CHARACTER*1           CH1
      CHARACTER*2           CH2, CH2F
      CHARACTER*4           CH4, CH4F
      INTEGER*2             IX2
      REAL*4                RL4
      EQUIVALENCE          (IX2,CH2)
      EQUIVALENCE          (RL4,CH4)

      CHARACTER*2           C2
      INTEGER*2             I2
      EQUIVALENCE          (I2,C2)
      LOGICAL               LFLAG

      c2='69'
      LFLAG=.TRUE.
      if(i2.eq.13881) LFLAG=.FALSE.

      OPEN(UNIT=20, FILE=LMRKFILE, ACCESS='DIRECT',
     .     RECL=72, status='OLD')

c       Read is the scale fo the bigmap
        READ(20,REC=1) BLINE
        CH4f=BLINE(7:10)
        call flip(4,lflag,ch4f,ch4)
        SCALE=RL4

c       Read is the size of the bigmap
        QSZ=ICHAR(BLINE(11:11))
     .     +ICHAR(BLINE(12:12))*256

c       Loop over K four times (for values for V, and U[XYZ])
c       Load up V, UX, UY and UZ
        DO K=1,3
          CH4f=BLINE(12+4*K:15+4*K)
          call flip(4,lflag,ch4f,ch4)
          V(K)=RL4
          CH4f=BLINE(24+4*K:27+4*K)
          call flip(4,lflag,ch4f,ch4)
          UX(K)=RL4
          CH4f=BLINE(36+4*K:39+4*K)
          call flip(4,lflag,ch4f,ch4)
          UY(K)=RL4
          CH4f=BLINE(48+4*K:51+4*K)
          call flip(4,lflag,ch4f,ch4)
          UZ(K)=RL4
        ENDDO


c       Load the H Scale
        CH4f=BLINE(64:67)
        call flip(4,lflag,ch4f,ch4)
        HSCALE=RL4

c       Clear the memory
        DO I=-QSZ,QSZ
          DO J=-QSZ,QSZ
            ALB(I,J)=0
            HT(I,J)=0
          ENDDO
        ENDDO
        
c       Read stuff in, 24 bytes at a time
        NREC=1        
        K=0
        DO J=-QSZ,QSZ
          DO I=-QSZ,QSZ
            K=K+1
            IX(K)=I
            JX(K)=J
            IF(K.EQ.24) THEN
              NREC=NREC+1
              READ(20,REC=NREC) BLINE
              DO K=1,24
                CH1=BLINE(3*K:3*K)
                IF(ICHAR(CH1).NE.0) THEN
                  CH2f=BLINE(3*K-2:3*K-1)
                  call flip(2,lflag,ch2f,ch2)
                  HT(IX(K),JX(K))=HSCALE*IX2
                  ALB(IX(K),JX(K))=.01*ICHAR(CH1)
                ENDIF
              ENDDO
              K=0
            ENDIF
          enddo
        enddo

c       Read the remainder of a 24 byte line
        IF(K.NE.0) THEN
          K0=K
          NREC=NREC+1
          READ(20,REC=NREC) BLINE
          DO K=1,K0
            CH1=BLINE(3*K:3*K)
            CH2f=BLINE(3*K-2:3*K-1)
            call flip(2,lflag,ch2f,ch2)
            HT(IX(K),JX(K))=HSCALE*IX2
            ALB(IX(K),JX(K))=.01*ICHAR(CH1)
          ENDDO
        ENDIF

      CLOSE(UNIT=20)
      
      RETURN
      END     

c   ................................................
      SUBROUTINE WRITE_MAP(LMRKFILE,NTMP,QSZ,SCALE,
     .                     V,UX,UY,UZ,HT,ALB)
c   ................................................

      IMPLICIT NONE
      
      INTEGER               NTMP
      INTEGER               QSZ
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               NREC

      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      HSCALE
      DOUBLE PRECISION      V(3)
      DOUBLE PRECISION      VSIG(3)
      DOUBLE PRECISION      UX(3)
      DOUBLE PRECISION      UY(3)
      DOUBLE PRECISION      UZ(3)
      DOUBLE PRECISION      Z1

      REAL*4                HT(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                ALB(-NTMP:NTMP,-NTMP:NTMP)

      CHARACTER*72          BLINE
      CHARACTER*72          LMRKFILE

      CHARACTER*1           CH1
      CHARACTER*2           CH2, CH2F
      CHARACTER*4           CH4, CH4F
      INTEGER*2             IX2
      REAL*4                RL4
      EQUIVALENCE          (IX2,CH2)
      EQUIVALENCE          (RL4,CH4)

      CHARACTER*2           C2
      INTEGER*2             I2
      EQUIVALENCE          (I2,C2)
      LOGICAL               LFLAG

      c2='69'
      LFLAG=.TRUE.
      if(i2.eq.13881) LFLAG=.FALSE.

      OPEN(UNIT=10,FILE=LMRKFILE,ACCESS='DIRECT',
     .     RECL=72,STATUS='UNKNOWN')
        DO K=1,72
          BLINE(K:K)=CHAR(0)
        ENDDO
        BLINE(1:6)='UNUSED'
        RL4=SCALE
        call flip(4,lflag,ch4,ch4f)
        BLINE(7:10)=CH4f
        BLINE(11:11)=CHAR(QSZ-256*(qsz/256))  !!!*
        BLINE(12:12)=CHAR(qsz/256)
        RL4=50
        call flip(4,lflag,ch4,ch4f)
        BLINE(68:71)=CH4f
        BLINE(13:13)=CHAR(50)
        BLINE(14:14)=CHAR(50)
        BLINE(15:15)=CHAR(50)
        DO K=1,3
          RL4=V(K)
          call flip(4,lflag,ch4,ch4f)
          BLINE(12+4*K:15+4*K)=CH4f
          RL4=UX(K)
          call flip(4,lflag,ch4,ch4f)
          BLINE(24+4*K:27+4*K)=CH4f
          RL4=UY(K)
          call flip(4,lflag,ch4,ch4f)
          BLINE(36+4*K:39+4*K)=CH4f
          RL4=UZ(K)
          call flip(4,lflag,ch4,ch4f)
          BLINE(48+4*K:51+4*K)=CH4f
        ENDDO
        z1=1.0
        do j=-QSZ,QSZ
        do i=-QSZ,QSZ
        if(alb(i,j).gt.0.005) then
          z1=max(z1,ABS(ht(i,j)))
        endif
        enddo
        enddo
        HSCALE=z1/30000
        RL4=HSCALE
        call flip(4,lflag,ch4,ch4f)
        BLINE(64:67)=CH4f
        NREC=1
        WRITE(10,REC=NREC) BLINE
        K=0
        do j=-QSZ,QSZ
        do i=-QSZ,QSZ
          K=K+1
          IX2=NINT(HT(I,J)/HSCALE)
          CH1=CHAR(NINT(100*ALB(I,J)))
          call flip(2,lflag,ch2,ch2f)
          BLINE(3*K-2:3*K-1)=CH2f
          BLINE(3*K:3*K)=CH1
          IF(K.EQ.24) THEN
            NREC=NREC+1
            write(10, REC=NREC) BLINE
            DO K=1,72
              BLINE(K:K)=CHAR(0)
            ENDDO
            K=0
          ENDIF
        enddo
        enddo
        IF(K.NE.0) THEN
          NREC=NREC+1
          write(10, REC=NREC) BLINE
        ENDIF
      CLOSE(UNIT=10)

      RETURN
      END     

c   ..................................................
      subroutine flip(n,lflag,ch1,ch2)
c   ..................................................

      integer*4        n, i
      character*(*)    ch1, ch2
      logical          lflag

      if(lflag) then
        do i=1,n
          ch2(i:i)=ch1(n-i+1:n-i+1)
        enddo
      else
        ch2=ch1
      endif
      
      return
      end
      
