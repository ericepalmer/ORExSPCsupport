C     Eric E. Palmer, 17 Sep 2015

c  Notes: reads in a list of maplets.  Builds terrain


      IMPLICIT NONE
      
      INTEGER               NTMP
      PARAMETER            (NTMP=128)
      INTEGER               BTMP
      PARAMETER            (BTMP=2001)
      INTEGER               QSZ

      DOUBLE PRECISION      S0
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      S1, holdRes, currRes, currVect, currQ
      DOUBLE PRECISION      V(3)
      DOUBLE PRECISION      UX(3)
      DOUBLE PRECISION      UY(3)
      DOUBLE PRECISION      UZ(3)
      DOUBLE PRECISION      V1(3)
      DOUBLE PRECISION      W(3)
      DOUBLE PRECISION      U1X(3)
      DOUBLE PRECISION      U1Y(3)
      DOUBLE PRECISION      U1Z(3)
      DOUBLE PRECISION      AX(0:3)
      DOUBLE PRECISION      AY(0:3)
      DOUBLE PRECISION      AZ(0:3)
      DOUBLE PRECISION      Z1
      DOUBLE PRECISION      Z2
      DOUBLE PRECISION      Z3
      DOUBLE PRECISION      GMIN, GMAX


      REAL*4                HT(-BTMP:BTMP,-BTMP:BTMP)
      REAL*4                ZHT(-BTMP:BTMP,-BTMP:BTMP)
      REAL*4                NH(-BTMP:BTMP,-BTMP:BTMP)
      REAL*4                ZH(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                AL(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                GD(-BTMP:BTMP,-BTMP:BTMP)
      real*4                vect
      real*4                normal

      INTEGER               I
      INTEGER               I0
      INTEGER               I1
      INTEGER               J
      INTEGER               J0
      INTEGER               J1
      INTEGER               IMIN
      INTEGER               IMAX
      INTEGER               JMIN
      INTEGER               JMAX
      INTEGER               K
      INTEGER               K0
      integer               argc
      integer               xMin, xMax, yMin, yMax
      integer               cnt

      CHARACTER*6           NAME
      CHARACTER*6           currName
      CHARACTER*6          listName
      CHARACTER*6           BIGMAP
      CHARACTER*72           search
      CHARACTER*72          OUTFILE
      CHARACTER*72          INFILE
      CHARACTER*72          LMRKFILE
      CHARACTER*72          LMKFILE
      character*5000        tline
      character*10000       cline
      character*80          val

      LOGICAL               EX
      LOGICAL               FOUND
      LOGICAL               HUSE(-BTMP:BTMP,-BTMP:BTMP)
      LOGICAL               GUSE(-BTMP:BTMP,-BTMP:BTMP)

      FOUND=.FALSE.

C  You can use command line or get prompted
      argc = IArgC()
      do i=1, argc
         call getArg (i, val)
      enddo

      if (argc .EQ. 2) then
        call getArg (1, val)
        bigmap = val
        if (bigmap .EQ. '-h') then
          write (*,*) "metaBigmap <BIGMAP> <maplet_list>"
          stop
        endif
        call getArg (2, val)
        search = val
      else
        write(6,*) 'input 6 character map name'
        read(5,fmt='(a6)') BIGMAP
        write(6,*) 'Filename of a lsit of maplets'
        read(5,fmt='(a6)') search
      endif

      LMRKFILE='./MAPFILES/'//BIGMAP//'.MAP'
      CALL READ_MAP(LMRKFILE,BTMP,QSZ,S0,V,UX,UY,UZ,HT,ZHT)
      write (*,*) LMRKFILE, QSZ, S0*1000, 'm'

C     Init memory space
      DO I=-QSZ-1,QSZ+1
      DO J=-QSZ-1,QSZ+1
        HUSE(I,J)=.FALSE.
      ENDDO
      ENDDO

      DO  I= -QSZ,QSZ
        DO J = -QSZ,QSZ
          NH(I,J)=0
          IF(zHT(I,J).GT.(0.005)) THEN
            HUSE(I,J)=.TRUE.
          ENDIF
        ENDDO
      ENDDO
      gMax = 0
      gMin = 999999


C  Loop through  the list of files
      open (79, file="meta.txt")
      open (78, file="resolutions.txt")
      open (77, file="position.json")
      write (77,*) '{"content": ['

      cnt = 0
      holdRes = 0
      open (unit=10, file=search, STATUS='OLD')
100    continue
        READ(10,FMT='(A6)') currName
        IF(currName(1:3).NE.'END') THEN
          LMKFILE='MAPFILES/'//currName//'.MAP'
          INQUIRE(FILE=LMKFILE, EXIST=EX)
          IF(.NOT.EX) GO TO 100


C     Find map-wide min/max
      do j=-qsz,qsz
      do i=-qsz,qsz
        gd(i,j)=(ht(i-1,j)-ht(i+1,j))/2
        if(huse(i+1,j).and.huse(i-1,j)) then
          gd(i,j)=(ht(i-1,j)-ht(i+1,j))/2
          guse(i,j)=.true.
          gmin=MIN(gmin, gd(i,j))
          gmax=MAX(gmax, gd(i,j))
        endif
      enddo
      enddo

C     Scale it
      do j=-qsz,qsz
      do i=-qsz,qsz
      if(guse(i,j)) then
        gd(i,j)=(gd(i,j)-gmin)/(gmax-gmin)/2
      endif
      enddo
      enddo


C     Calculate the image
      infile= 'view.dat'
      open(unit=12, file=infile, access='direct',
     .     recl=2*QSZ+1, status='UNKNOWN')
        do j=1,2*QSZ+1
          do i=1,2*QSZ+1
            if(guse(i-qsz-1,j-qsz-1)) then
              cline(i:i)=char(nint(1+254*gd(i-qsz-1,j-qsz-1)))
            else
              cline(i:i)=char(0)
            endif
C            NH(I,J) = gd(i-qsz-1,j-qsz-1)*254+1
            NH(i-qsz-1, j-qsz-1) = gd(i-qsz-1,j-qsz-1)*254+1
          enddo
          write(12,rec=j) cline(1:2*qsz+1)
        enddo
      close(unit=12)


      OPEN(UNIT=69,FILE='LMRKLIST.TXT')

C       Loop to run through all the landmarks
10      CONTINUE
        READ(69,FMT='(A6)') NAME
        IF(NAME(1:3).EQ.'END') GO TO 30
        IF(NAME(1:1).EQ.'X') GO TO 10

        LMRKFILE='./MAPFILES/'//NAME//'.MAP'
        INQUIRE(FILE=LMRKFILE, EXIST=EX)
        IF(.NOT.EX) GO TO 10
        CALL READ_HEADER(LMRKFILE,K0,S1,V1,U1X,U1Y,U1Z)

        if (name .NE. currName) goto 10   ! skip the files we don't want

c       Only execute this if it is found
        FOUND=.TRUE.
        CALL VSUB(V,V1,W)
        IF(VNORM(W).GT.3*(S0*QSZ+S1*K0)) GO TO 10

        xMin = QSZ
        yMin = QSZ
        xMax = -QSZ
        yMax = -QSZ

C       Read the landmark and mark it up
        CALL READ_MAP(LMRKFILE,NTMP,K0,S1,V1,U1X,U1Y,U1Z,ZH,AL)
        currRes = S1
        currVect = sqrt (v1(1)**2 + v1(2)**2 + v1(3)**2)
        currQ = K0
        DO i0=-K0,K0-1
        DO j0=-K0,K0-1
          K=-1
          IMAX=-QSZ-1
          IMIN= QSZ+1
          JMAX=-QSZ-1
          JMIN= QSZ+1
          do j1=0,1
          do i1=0,1
            K=K+1
            I=I0+I1
            J=J0+J1
            IF(AL(I,J).LT.(0.005)) GO TO 20
            w(1)=V1(1)-V(1)
            w(2)=V1(2)-V(2)
            w(3)=V1(3)-V(3)
            w(1)=w(1)+S1*(j*u1x(1)+i*u1y(1)+ZH(I,J)*u1z(1))
            w(2)=w(2)+S1*(j*u1x(2)+i*u1y(2)+ZH(I,J)*u1z(2))
            w(3)=w(3)+S1*(j*u1x(3)+i*u1y(3)+ZH(I,J)*u1z(3))
            AX(K)=VDOT(w,ux)/S0
            AY(K)=VDOT(w,uy)/S0
            AZ(K)=VDOT(w,uz)/S0
            IMAX=MAX(IMAX,NINT(AY(K)-0.5))
            IMIN=MIN(IMIN,NINT(AY(K)+0.5))
            JMAX=MAX(JMAX,NINT(AX(K)-0.5))
            JMIN=MIN(JMIN,NINT(AX(K)+0.5))
          enddo
          enddo
          IF((IMAX.LT.-QSZ).OR.(IMIN.GT.QSZ).OR.
     .       (JMAX.LT.-QSZ).OR.(JMIN.GT.QSZ)) GO TO 20
          AX(3)=AX(0)-AX(1)-AX(2)+AX(3)
          AX(1)=AX(1)-AX(0)
          AX(2)=AX(2)-AX(0)
          AY(3)=AY(0)-AY(1)-AY(2)+AY(3)
          AY(1)=AY(1)-AY(0)
          AY(2)=AY(2)-AY(0)
          AZ(3)=AZ(0)-AZ(1)-AZ(2)+AZ(3)
          AZ(1)=AZ(1)-AZ(0)
          AZ(2)=AZ(2)-AZ(0)
          IMIN=MAX(IMIN,-QSZ)
          IMAX=MIN(IMAX, QSZ)
          JMIN=MAX(JMIN,-QSZ)
          JMAX=MIN(JMAX, QSZ)

          xMin = MIN (xMin, IMIN)     ! track the pixel boundaries of the landmark
          yMin = MIN (yMin, JMIN)
          xMax = MAX (xMax, IMAX)
          yMax = MAX (yMax, JMAX)

          DO I=IMIN,IMAX
          DO J=JMIN,JMAX
            Z1=AX(2)*AY(3)-AX(3)*AY(2)
            Z2=AX(2)*AY(1)-AX(1)*AY(2)+AX(0)*AY(3)-AX(3)*AY(0)
     .        +AX(3)*I-AY(3)*J      
            Z3=AX(0)*AY(1)-AX(1)*AY(0)+AX(1)*I-AY(1)*J
            IF(Z2**2.LE.4*Z1*Z3) GO TO 15

            NH(I,J)= NH(I, J) +128
15          CONTINUE
          ENDDO
          ENDDO          
20        CONTINUE
        ENDDO
        ENDDO
        GO TO 10
30    CONTINUE
      CLOSE(UNIT=69)

        IF(.NOT. FOUND) then
          WRITE(6,*) search, " was not found"
          GOTO 100
        endif

      xMin = xMin + QSZ
      yMin = yMin + QSZ
      xMax = xMax + QSZ
      yMax = yMax + QSZ
      cnt = cnt + 1

      if (holdRes .NE. currRes) then
         holdRes = currRes
         write (78,fmt='(f9.8)') currRes 
c         write(line,fmt='(i10)') npx
      endif

      if (cnt .NE. 1) write (77,*) ", "
      write (77,*) '{ "title": "', currName, '", ',
     +     '"minX": ', xMin, ', ',
     +     '"maxX": ', xMax, ', ',
     +     '"minY": ', yMin, ', ',
     +     '"maxY": ', yMax, ', ',
     +     '"guid": ', cnt, ', ',
     +     '"res": ', currRes * 1000, ', ',
     +     '"Q": ', currQ, ', ',
     +     '"vect": ', currVect * 1000, '}'
      write (79, *) currName, xMin, xMax, yMin, yMax, cnt, currRes*1000,
     +      currQ, currVect*1000

c      CALL READ_MAP(LMRKFILE,BTMP,QSZ,S0,V,UX,UY,UZ,HT,ZHT)

      GOTO 100
      ENDIF
      close (unit=10)
      write (77,*) "] }"
      close (unit=77)
      close (unit=78)
      close (unit=79)

      STOP
      END

c   ................................................
      SUBROUTINE READ_HEADER(LMRKFILE,QSZ,SCALE,V,UX,UY,UZ)
c   ................................................

      IMPLICIT NONE
      
      INTEGER               QSZ
      INTEGER               K
      INTEGER               IX(24)
      INTEGER               JX(24)
      INTEGER               NREC

      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      V(3)
      DOUBLE PRECISION      UX(3)
      DOUBLE PRECISION      UY(3)
      DOUBLE PRECISION      UZ(3)

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
        READ(20,REC=1) BLINE
        CH4f=BLINE(7:10)
        call flip(4,lflag,ch4f,ch4)
        SCALE=RL4
        QSZ=ICHAR(BLINE(11:11))
     .     +ICHAR(BLINE(12:12))*256
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
      CLOSE(UNIT=20)
      
      RETURN
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

        READ(20,REC=1) BLINE
        CH4f=BLINE(7:10)
        call flip(4,lflag,ch4f,ch4)
        SCALE=RL4
        QSZ=ICHAR(BLINE(11:11))
     .     +ICHAR(BLINE(12:12))*256
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
        CH4f=BLINE(64:67)
        call flip(4,lflag,ch4f,ch4)
        HSCALE=RL4

        DO I=-QSZ,QSZ
        DO J=-QSZ,QSZ
          ALB(I,J)=0
          HT(I,J)=0
        ENDDO
        ENDDO
        
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
      
c  ..............................................
      subroutine raw2pgm(infile,outfile,npx,nln)
c  ..............................................

      implicit none
      
      integer*4       npx, nln, k, k1, k2
      character*10    line
      character*50    header
      character*72    infile, outfile
      character*5000  cline

      
      header(1:2)='P5'
      header(3:3)=char(10)
      header(4:5)='#.'
      header(6:6)=char(10)
      k=7
      write(line,fmt='(i10)') npx
      do k1=1,10
      if(line(k1:k1).ne.' ') then
        header(k:k+10-k1) = line(k1:10)
        k=k+11-k1
        go to 10
      endif
      enddo
10    continue
      header(k:k)=' '
      k=k+1
      write(line,fmt='(i10)') nln
      do k1=1,10
      if(line(k1:k1).ne.' ') then
        header(k:k+10-k1) = line(k1:10)
        k=k+11-k1
        go to 20
      endif
      enddo
20    continue
      header(k:k)=char(10)
      k=k+1
      header(k:k+2)='255'
      k=k+3
      header(k:k)=char(10)
      
      open(unit=20, file=outfile, recl=1, access='direct', 
     .     status='unknown')
      open(unit=10, file=infile, recl=npx, access='direct', 
     .     status='old')
        do k1=1,k
          write(20,rec=k1) header(k1:k1)
        enddo
        do k1=1,nln
          read(10,rec=k1) cline(1:npx)
          do k2=1,npx
            write(20,rec=k+npx*(k1-1)+k2) cline(k2:k2)
          enddo
        enddo
      close(unit=10)
      close(unit=20)
      
      return
      end

