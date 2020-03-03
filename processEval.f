c   processEval.f -- looks over an images and eliminates "empty" images
c     gfortran -O2  processEval.f /usr/local/lib/spicelib.a -o t.processEval



      IMPLICIT NONE

      INTEGER               NPX
      INTEGER               NLN
      INTEGER               I
      INTEGER               J, J0, J1, DJ
      INTEGER               K
      INTEGER               N
      INTEGER               DN(5000,5000)
      INTEGER               SLEN
      integer               cnt
      integer               argc
      real                  percent

      CHARACTER*1           ANS
      CHARACTER*1           CH
      CHARACTER*20          HEAD
      CHARACTER*72          COVFILE
      character*72          version

      LOGICAL               EX
 

      version = "1.0"

      argc = IArgC()
      if (argc .EQ. 3) then
         call getArg (1, COVFILE)
      endif
      COVFILE='TEMPFILE.pgm'


C     Verify coverage_m.pgm exists prior to opening file.
      INQUIRE(FILE=COVFILE, EXIST=EX)
      IF(.NOT.EX) THEN
        WRITE(6,*)
        WRITE(6,*) '***************************************'
        WRITE(6,*) COVFILE
        WRITE(6,*) 'does not exist...'
        WRITE(6,*) '***************************************'
        WRITE(6,*)
        STOP
      ENDIF  

      N=0
      OPEN(UNIT=10, FILE=COVFILE, STATUS='OLD')

C     Read the header data from coverage_m.pgm, calculate the length of
C     of the header and get the number of lines and pixel/line (NLN,NPX)
      READ(10,FMT='(A20)') HEAD
      N=N+SLEN(HEAD) 
      READ(10,FMT='(A20)') HEAD
      N=N+SLEN(HEAD) 
      READ(10,FMT='(A20)') HEAD
      N=N+SLEN(HEAD) 
      READ(HEAD,*) NPX, NLN
      READ(10,FMT='(A20)') HEAD
      N=N+SLEN(HEAD) 

      CLOSE(UNIT=10)

C     Reopen coverage_m.pgm and read the DN for each pixel
      N=N+4
      OPEN(UNIT=10, FILE=COVFILE, ACCESS='DIRECT', 
     .     RECL=1, STATUS='OLD')
      DO K=1,NPX*NLN
         J=1+(K-1)/NPX
         I=K-(J-1)*NPX
         READ(10,REC=K+N) CH
         DN(I,J)=ICHAR(CH)
      ENDDO
      CLOSE(UNIT=10)

       
C     Figure it out
      cnt=5
      DO J=1,NLN
        DO I=1,NPX
           if (DN(I,J) < 2) then
              cnt=cnt+1
c           else
c              write (*,*) i, j, dn(i,j)
           endif
        ENDDO
      ENDDO
      percent = cnt
      percent = percent / NLN / NPX
      write (*,'(f7.3)') percent*100

      STOP
      END

