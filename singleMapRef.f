C$ Procedure singleMapRef

C$ Abstract
C		Hack by Eric to load a single maplet and build a referenced bigmap from it
C     This procedure creates a large topo/albedo map with the same file structure as the maplets
C     (see subroutine READ_MAP or WRITE_MAP abstract).  The location of the map canter is 
C     specified by one of three choices:
C
C          p:  pixel/line location in a picture
C          l:  latitude and west longitude
C          m:  pixel line location in a map or maplet.
C 
C     There is a fourth choice (i) that is experimental and is hidden from the menu.  It will 
C     play no role in mission operations.
C
C     A second set of inputs contains the bigmap scale in km/px, the half-size (qsz), an integer 
C     random seed, and a maximum maplet scale in case lower resolution maplets are to be 
C     excluded from the 
C
C     The program first determines the body-fixed vector to the map center and the approximate 
C     surface normal Uz.  It then projects the shape model onto this surface and determines a 
C     second approximation to the normal Uz by fitting a plane to the heights.  It repeats this 
C     process one more time.  The new map coordinate frame is then oriented so that East is to 
C     the right.
C
C     The maplets to be used in the construction are taken from the first of the files 
C     BIGMAP.IN, LMRKLISTR.TXT or LMRKLIST.TXT found to exist.  However, if LMKLX=.TRUE. in the 
C     INIT_LITHOS.TXT file, the program will read LMRKLISTX.TXT.  If there are a great many
C     maplets, the latter choice will speed up the procedure by determining whether a maplet 
C     needs to be used without having to open its .LMK file.
C
C     The bigmap reference plane has (2*qsz+1)^2 points at locations:
C
C               p(i,j) = V + j*Ux +i*Uy   (i,j = -qsz,qsz)
C
C     A line from each of these points in the normal (Uz) direction will pierce a number of 
C     maplets.  For each i,j we keep track of the weighted accumulation of the heights to the 
C     piercing points, the squares of those heights, and the slopes and albedos of the maplets 
C     at the piercing points.  
C
C     Once these arrays are filled, the average heights and albedos at each point of the 
C     reference plane are computed, as well as the standard deviation of the heights.  This last 
C     provides a convenient measure ot the height uncertainty at each point, and a display
C     SIGMAS.pgm provided a quick means of identifying possible problem areas.
C
C     If we choose to use the height averaged map just constructed, we are done now.  If we are
C     constructind bigmaps from fake CreatorP data, this is recommended.  A display showing the
C     gradients of the bigmap can be viewed to see if all looks well.  If it dose, we may just 
C     as well stop here.  However, we often continue on to determine the slope_averaged bigmap.
C     We choose a small fraction of our average heights (usually 0.005) as conditioning heights 
C     and integrate over the averaged slopes as we did in the slope to height integration in 
C     LITHOS (see subroutines SLP2HGT and NNEIGHBORS).  This tends to eliminage "cliffs" that 
C     sometimes occur at maplet boundaries.  We are asked for a weight for the conditioning 
C     heights and we usually choose 0.025.  We then iterate perhaps 5 to 10 times before 
C     exiting.  A typical run (this one from Vesta) looks like this:
C
C               bigmap
C               l                           <- specify by lat/lon
C               -1.6, 4.0                   <- lat/lon values
C               .05, 500, 1234, 1           <- scale, qsx, seed, max maplet res
C               CLAUDI                      <- bigmap name
C               1                           <- choose slope integration
C               .005                        <- fraction of heights for conditioning
C               .025                        < conditioning weight
C               1                           <- iterate
C               1
C               1
C               1
C               1
C               1
C               1
C               0                           <- stop iteration
C               0                           <- no template
C
C     The last entry is one of three choices, the others letting the user mask out all but a 
C     circular or rectangular portion of the map - a rarely used feature but useful for 
C     presentation slides. 
C
C     In addition to the bigmap, that goes into the MAPFILES directory, and the SIGMAS.pgm 
C     image,  bigmap produces other files.   USED_MAPS.TXT is a list of all maplets that overlap 
C     the bigmap and were used in its creation.  INSIDE.TXT is a list of all maplets that 
C     completely ovwerlap the bigmap.  A file MAPNAME.LMK, of the same form as a maplet's 
C     landmark file goes into the BIGFILES directory.  Finally, the file SIGMAS.TXT is updated 
C     to include a record showing the maximum and average standard deviation for the BIGMAP 
C     heights.
C
C     Special option to read in a reference maplet that lets you "set" the normal plane
C
C$ Required_Reading
C
C     R.W. Gaskell, et.al, "Characterizing and navigating small bodies
C           with imaging data", Meteoritics & Planetary Science 43,
C           Nr 6, 1049-1061 (2008)
C
C$ Declarations

      IMPLICIT NONE
      
      INTEGER               NTMP
      PARAMETER            (NTMP=512)
      INTEGER               BTMP
      PARAMETER            (BTMP=2501)

      DOUBLE PRECISION      S0
      DOUBLE PRECISION      junkS0
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      MAXLEN
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      S1
      DOUBLE PRECISION      V(3)
      DOUBLE PRECISION      junkV(3)
      DOUBLE PRECISION      VSIG(3)
      DOUBLE PRECISION      UX(3)
      DOUBLE PRECISION      UY(3)
      DOUBLE PRECISION      UZ(3)
      DOUBLE PRECISION      HOLDX(3)
      DOUBLE PRECISION      HOLDY(3)
      DOUBLE PRECISION      HOLDZ(3)
      DOUBLE PRECISION      V0(3)
      DOUBLE PRECISION      CX(3)
      DOUBLE PRECISION      CY(3)
      DOUBLE PRECISION      CZ(3)
      DOUBLE PRECISION      SZ(3)
      DOUBLE PRECISION      V1(3)
      DOUBLE PRECISION      W(3)
      DOUBLE PRECISION      U1X(3)
      DOUBLE PRECISION      U1Y(3)
      DOUBLE PRECISION      U1Z(3)
      DOUBLE PRECISION      AX(0:3)
      DOUBLE PRECISION      AY(0:3)
      DOUBLE PRECISION      AZ(0:3)
      DOUBLE PRECISION      AW(0:3)
      DOUBLE PRECISION      Z1
      DOUBLE PRECISION      Z2
      DOUBLE PRECISION      Z3
      DOUBLE PRECISION      Z4
      DOUBLE PRECISION      Z5
      DOUBLE PRECISION      WT
      DOUBLE PRECISION      RANN
      DOUBLE PRECISION      SEED
      DOUBLE PRECISION      MMFL      
      DOUBLE PRECISION      CTR(2)      
      DOUBLE PRECISION      KMAT(2,3)
      DOUBLE PRECISION      IMGPL(2)
      DOUBLE PRECISION      D(4)
      DOUBLE PRECISION      EPS
      DOUBLE PRECISION      SMAX
      DOUBLE PRECISION      SMAX0
      DOUBLE PRECISION      SIGMA
      DOUBLE PRECISION      ALPHA
      DOUBLE PRECISION      MXSCL      
      DOUBLE PRECISION      ROT      

      REAL*4                HT(-BTMP:BTMP,-BTMP:BTMP)
      REAL*4                ZHT(-BTMP:BTMP,-BTMP:BTMP)
      REAL*4                TMPL(-BTMP:BTMP,-BTMP:BTMP,3)
      REAL*4                NH(-BTMP:BTMP,-BTMP:BTMP)
      REAL*4                ZH(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                AL(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                ZHT2(-BTMP:BTMP,-BTMP:BTMP)

      INTEGER               I
      INTEGER               I0
      INTEGER               I1
      INTEGER               IX
      INTEGER               J
      INTEGER               J0
      INTEGER               J1
      INTEGER               JX
      INTEGER               IMIN
      INTEGER               IMAX
      INTEGER               JMIN
      INTEGER               JMAX
      INTEGER               K
      INTEGER               K0
      INTEGER               Q0
      INTEGER               QSZ
      INTEGER               junkQSZ
      INTEGER               NPX
      INTEGER               NLN
      INTEGER               KPIC
      INTEGER               SLEN
      INTEGER               CHARVAL
      INTEGER               ARGC
      INTEGER               TMPSEED

      CHARACTER*1           ANS
      CHARACTER*2           COVERED
      CHARACTER*6           NAME
      CHARACTER*6           XNAME(100000)
      CHARACTER*6           BIGMAP
      CHARACTER*6           mapName
      CHARACTER*6           REFMAP
      CHARACTER*12          PICNM
      CHARACTER*12          PICID(100000)
      CHARACTER*72          INFILE
      CHARACTER*72          OUTFILE
      CHARACTER*72          LMRKFILE
      CHARACTER*72          PICTFILE
      CHARACTER*80          LINE
      CHARACTER*80          XLINE(10000)
      CHARACTER*80          ARGV
      CHARACTER*80          VERS
      CHARACTER*80          VERSION
      CHARACTER*20000       OUTLINE

      LOGICAL               EX
      LOGICAL               USE
      LOGICAL               FLAG
      LOGICAL               BFLG
      LOGICAL               DFLG
      LOGICAL               EFLG
      LOGICAL               MUSE(-NTMP:NTMP,-NTMP:NTMP)
      LOGICAL               HUSE(-BTMP:BTMP,-BTMP:BTMP)
      LOGICAL               ZUSE(-BTMP:BTMP,-BTMP:BTMP)
      LOGICAL               TUSE(-BTMP:BTMP,-BTMP:BTMP)
      LOGICAL               SUSE(-BTMP:BTMP,-BTMP:BTMP)
      LOGICAL               LMKLX
      LOGICAL               CLSEED
 
C$ Variable_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ANS        I   User response to menu selections
C     PICMN      I   User supplied picture name
C     SEED       I   User supplied seed
C     MXSCL      I   User supplied max scale
C     BIGMAP     I   User supplied map name
C     IMGPL      I   User supplied patch center
C     Z1         I   User supplied latitude in deg.
C     Z2         I   User supplied W longitude in deg.
C     NAME       I   User supplied map name
C     S0         I   User supplied map scale
C     QSZ        I   User supplied map pixel size * 2
C     Q0         I   User supplied half size
C
C$ File_I/O
C
C     Filename                      I/O  Description
C     ----------------------------  ---  -------------------------------
C     SUMFILES/<PICNM>.SUM           I   Image <PICNM> summary file
C     USED_MAPS.TXT
C     INSIDE.TXT            
C     BIGMAP.IN                      I
C     LMRKLISTR.TXT                  I   Used if BIGMAP.IN doesn't exist
C     LMRKLIST.TXT                   I   Used if LMRKLISTR.TXT doesn't
C                                        exist
C     SIGMAS.TXT                     I
C     SIGMAS.DAT                     O   Temporary file used by RAW2PGM
C     MAPFILES/<BIGMAP>.MAP          O   Big map file created by 
C                                        WRITE_MAP
C     BIGFILES/<BIGMAP>.LMK          O
C     BIGLIST.TXT                    I
C     USED_MAP.TXT                   I
C     LMKFILES/<LMKFILE>.LMK         I
C     USED_PICS.TXT                  O
C
C$ Restrictions
C     None
C
C$ Software_Documentation
C
C     OSIRIS-REx Stereophotoclinometry Software Design Document
C     OSIRIS-REx Stereophotoclinometry Software User's Guide
C
C$ Author_and_Institution
C
C     R.W. Gaskell    (PSI)
C
C$ Version
C
C
C
C$ SPC_functions_called
C     SLEN
C
C$ SPC_subroutines_called
C     GET_HEIGHTS     
C     IMGPL2VN        
C     ORIENT  
C     PATCH_COORDS    
C     RAW2PGM 
C     READ_HEADER     
C     READ_MAP        
C     SLP2HGT 
C     SHOW_SLOPES     
C     U2VN    
C     V2IMGPL 
C     WRITE_MAP       
C
C$ SPICELIB_functions_called
C     RPD
C     VDOT
C     VNORM
C
C$ SPICELIB_subroutines_called
C     LATREC
C     VHAT
C     VMINUS
C     VSUB
C
C$ History
C     2012_10_31:  INSIDE maplets with holes recognized 
C     2013_01_11:  Only maplets with scale <= MXSCL included in Bigmap 
C     2013_01_11:  All maplets included in INSIDE.TXT and USED_MAPS.TXT
C     2013_07_23:  Header added, capitalization & compiler warnings fixed.
C     2013_09_26:  Parameter EPS set to 1.D-05 to avoud speckling.
C     2013_11_18:  TMPL(I,J,3) computed earlier in case average only map wanted.
C     2014_03_24:  LMRKLISTX.TXT used as list if LMKLX=.TRUE. IN INIT_LITHOS.TXT.
C     2014_04_11:  Albedo problem introduced by -Wall fixes fixed.
C     2014_05_01:  Y and Z prefixes removed as prohibited inputs.
C     2014_05_05:  Command line version and seed options added.
C                   

C     FIND OUT HOW MANY ARGUMENTS GIVEN ON THE COMMAND LINE
      CLSEED=.FALSE.
      TMPSEED=1234
      ARGC = IARGC()
      IF (ARGC.EQ.1) THEN
         CALL GETARG (1, ARGV)
C        ARGUMENT -V PROVIDES VERSION NUMBER
         IF ((ARGV.EQ.'-v').OR.(ARGV.EQ.'-V')) THEN
            VERS=VERSION()
            WRITE (*,*) "Version ", VERS
            STOP
         ENDIF
C        Make sure that at least the first char is a number
         CHARVAL = ICHAR (ARGV(1:1)) - 48
         IF ((CHARVAL.GE.0).AND.(CHARVAL.LE.9)) THEN
           CLSEED=.TRUE.
C          SETS THE RANDOM SEED
           READ (ARGV,*) TMPSEED
           WRITE (*,*) "TMPSEED", TMPSEED
         ENDIF
      ENDIF

      TOL=MAXLEN()/10

      DFLG=.FALSE.
      LMKLX=.FALSE.
      OPEN(UNIT=25,FILE='INIT_LITHOS.TXT',STATUS='OLD')
13      CONTINUE
        READ(25,FMT='(A80)') LINE
        IF(LINE(1:3).NE.'END') THEN
          IF(LINE(1:6).EQ.'LMKLX=') READ(LINE(7:80),*) LMKLX 
          GO TO 13
        ENDIF 
      CLOSE(UNIT=25)

      WRITE(6,*) '=========================================='
      WRITE(6,*) '========= Reference SINGLE MAP  =========='
      WRITE(6,*) '=========================================='

      WRITE(6,*) 'Input 6 character map name'
      READ(5,FMT='(A6)') mapName

C   Get bigmap scale, size, random seed and max maplet scale
C   from maplet
      LMRKFILE='./MAPFILES/'//mapName//'.MAP'
      CALL READ_MAP(LMRKFILE,BTMP,QSZ,S0,V,HOLDX,HOLDY,HOLDZ,ZHT,NH)

      if ( S0 .EQ. 0) then
         write (*,*) "Scale is zero. Terminating"
         STOP
      endif

      seed = 1.234
      mxscl = 1000
      rot = 0

C		Get reference
      WRITE(6,*) 'Reference BIGMAP [6 character map name]'
      READ(5,FMT='(A6)') REFMAP
      LMRKFILE='./MAPFILES/'//REFMAP//'.MAP'
      CALL READ_MAP(LMRKFILE,BTMP,junkQSZ,junkS0,junkV,HOLDX,HOLDY,
     +              HOLDZ,ZHT,NH)
      if ( junkS0 .EQ. 0) then
         write (*,*) "Scale is zero. Terminating"
         STOP
      endif

      READ(5,FMT='(A1)') ANS
      WRITE(XLINE(1),FMT='(A1,79X)') ANS

      write (*,*) "V: ", V
      write (*,*) "Scale: ", S0
      write (*,*) "QSZ: ", QSZ

C   Map location determines map center location V:
      ANS='N'
150   CONTINUE


      BIGMAP = 'NEWONE'

      write (*,*) "mapName:  ", mapName
      write (*,*) "BIGMAP:   ", BIGMAP
      write (*,*) "REFMAP:   ", REFMAP
        
      K0=49
      S1=QSZ*S0/K0
      EPS=1.D-05

C   Compute map frame orientation from shape model and
C   Orient frame with horizontal axis to the East:

      CALL ORIENT(UX,UY,UZ)
      CALL GET_HEIGHTS(NTMP,K0,UX,UY,UZ,V,S1, MUSE,ZH,AL)
      Z1=ZH(0,0)
      DO I=-K0,K0
      DO J=-K0,K0
        IF(MUSE(I,J)) ZH(I,J)=ZH(I,J)-REAL(Z1)
      ENDDO
      ENDDO
      DO K=1,3
        V(K)=V(K)+Z1*S1*UZ(K)
      ENDDO
c      CALL PATCH_COORDS(NTMP,K0,MUSE,ZH,UX,UY,UZ)
      DO K=1,3
        UX(K) = HOLDX(K)
        UY(K) = HOLDY(K)
        UZ(K) = HOLDZ(K)
      ENDDO
      CALL ORIENT(UX,UY,UZ)
      CALL VROTV(UX,UZ,ROT*RPD(),UX)
      CALL VROTV(UY,UZ,ROT*RPD(),UY)
      CALL GET_HEIGHTS(NTMP,K0,UX,UY,UZ,V,S1, MUSE,ZH,AL)
      Z1=ZH(0,0)
      DO I=-K0,K0
      DO J=-K0,K0
        IF(MUSE(I,J)) ZH(I,J)=ZH(I,J)-REAL(Z1)
      ENDDO
      ENDDO
      DO K=1,3
        V(K)=V(K)+Z1*S1*UZ(K)
        VSIG(K)=S0
      ENDDO

160   CONTINUE  ! Entry point from hidden option 'I' that computes its own reference plane.

      IF(CLSEED) SEED=DBLE(TMPSEED)

C   Display map projection of shape model:

      CALL GET_HEIGHTS(BTMP,QSZ,UX,UY,UZ,V,S0, ZUSE,ZHT,ZHT2)
      CALL SHOW_SLOPES(BTMP,QSZ,ZUSE,ZHT)

C   Determine input maplet list:
      
c      IF(LMKLX) THEN
c        INFILE='LMRKLISTX.TXT'
c        BFLG=.TRUE.
c      ELSE
c        INFILE='BIGMAP.IN'
c        INQUIRE(FILE=INFILE, EXIST=BFLG)
c        IF(.NOT.BFLG) THEN
c          INFILE='LMRKLISTR.TXT'
c        ENDIF
c        INQUIRE(FILE=INFILE, EXIST=BFLG)
c        IF(.NOT.BFLG) THEN
c          INFILE='LMRKLIST.TXT'
c        ENDIF
c      ENDIF

C   Initialisze accumulators:

      DO I=-QSZ-1,QSZ+1
      DO J=-QSZ-1,QSZ+1
        ZHT(I,J)=0
        ZHT2(I,J)=0
        TMPL(I,J,1)=0
        TMPL(I,J,2)=0
        TMPL(I,J,3)=0
        NH(I,J)=0
        HUSE(I,J)=.FALSE.
        SUSE(I,J)=.FALSE.
      ENDDO
      ENDDO

C   Open maplet list files:

C      OPEN(UNIT=70,FILE='USED_MAPS.TXT')
C      OPEN(UNIT=71,FILE='INSIDE.TXT')

C   Determine maplet height, albedo and slope contributions:

      SMAX=0
      KPIC=0
C      OPEN(UNIT=69,FILE=INFILE,STATUS='OLD')
10      CONTINUE
C        IF(.NOT.LMKLX) THEN
C          READ(69,FMT='(A6)') NAME


        IF(NAME(1:3).EQ.'END') GO TO 30
        NAME=mapName
c          IF(.NOT.BFLG) THEN
c            IF(NAME(1:1).EQ.'X') GO TO 10
c          ENDIF
C        ELSE
C          READ(69,FMT='(A80)') LINE
C          IF(LINE(1:3).EQ.'END') GO TO 30
C          READ(LINE(7:80),*) K0,J,(V1(K), K=1,3),S1
C          CALL VSUB(V,V1,W)
C          IF(VNORM(W).GT.2*(S0*QSZ+S1*K0)) GO TO 10
C          NAME=LINE(1:6)
C        ENDIF

        FLAG=.FALSE.
        EFLG=.TRUE.
        COVERED='  '
        LMRKFILE='./MAPFILES/'//NAME//'.MAP'
        write (*,*) LMRKFILE

        INQUIRE(FILE=LMRKFILE, EXIST=EX)
        IF(.NOT.EX) GO TO 30

        CALL READ_HEADER(LMRKFILE,K0,S1,V1,U1X,U1Y,U1Z)
        IF(VDOT(UZ,U1Z).LT.(0.250)) GO TO 30
        CALL VSUB(V,V1,W)
        IF(VNORM(W).GT.2*(S0*QSZ+S1*K0)) GO TO 30
        SIGMA=SQRT(S0**2+S1**2)
        SMAX0=0
        CALL READ_MAP(LMRKFILE,NTMP,K0,S1,V1,U1X,U1Y,U1Z,ZH,AL)
        DO I0=-K0,K0-1
        DO J0=-K0,K0-1
          DO J1=0,1
          DO I1=0,1
            IF(AL(I0+I1,J0+J1).LT.(0.005)) GO TO 20
          ENDDO
          ENDDO
          DFLG=.FALSE.
          K=-1
          IMAX=-QSZ-1
          IMIN= QSZ+1
          JMAX=-QSZ-1
          JMIN= QSZ+1
          DO J1=0,1
          DO I1=0,1
            K=K+1
            I=I0+I1
            J=J0+J1
            W(1)=V1(1)-V(1)
            W(2)=V1(2)-V(2)
            W(3)=V1(3)-V(3)
            IF(ABS(VDOT(UZ,W)).GT.TOL) GO TO 30
            W(1)=W(1)+S1*(J*U1X(1)+I*U1Y(1)+ZH(I,J)*U1Z(1))
            W(2)=W(2)+S1*(J*U1X(2)+I*U1Y(2)+ZH(I,J)*U1Z(2))
            W(3)=W(3)+S1*(J*U1X(3)+I*U1Y(3)+ZH(I,J)*U1Z(3))
            AX(K)=VDOT(W,UX)/S0
            AY(K)=VDOT(W,UY)/S0
            AZ(K)=VDOT(W,UZ)/S0
            AW(K)=AL(I,J)
            SMAX0=MAX(SMAX0,ABS(VDOT(W,UX)),ABS(VDOT(W,UY)))
            IMAX=MAX(IMAX,NINT(AY(K)-0.5))
            IMIN=MIN(IMIN,NINT(AY(K)+0.5))
            JMAX=MAX(JMAX,NINT(AX(K)-0.5))
            JMIN=MIN(JMIN,NINT(AX(K)+0.5))
          ENDDO
          ENDDO
          IF((IMAX.LT.-QSZ).OR.(IMIN.GT.QSZ).OR.
     .       (JMAX.LT.-QSZ).OR.(JMIN.GT.QSZ)) GO TO 20
          IF((IMAX.LT.QSZ).AND.(IMIN.GT.-QSZ).AND.
     .       (JMAX.LT.QSZ).AND.(JMIN.GT.-QSZ)) DFLG=.TRUE.
          AX(3)=AX(0)-AX(1)-AX(2)+AX(3)
          AX(1)=AX(1)-AX(0)
          AX(2)=AX(2)-AX(0)
          AY(3)=AY(0)-AY(1)-AY(2)+AY(3)
          AY(1)=AY(1)-AY(0)
          AY(2)=AY(2)-AY(0)
          AZ(3)=AZ(0)-AZ(1)-AZ(2)+AZ(3)
          AZ(1)=AZ(1)-AZ(0)
          AZ(2)=AZ(2)-AZ(0)
          AW(3)=AW(0)-AW(1)-AW(2)+AW(3)
          AW(1)=AW(1)-AW(0)
          AW(2)=AW(2)-AW(0)
          IMIN=MAX(IMIN,-QSZ)
          IMAX=MIN(IMAX, QSZ)
          JMIN=MAX(JMIN,-QSZ)
          JMAX=MIN(JMAX, QSZ)
          DO I=IMIN,IMAX
          DO J=JMIN,JMAX
            Z1=AX(2)*AY(3)-AX(3)*AY(2)
            Z2=AX(2)*AY(1)-AX(1)*AY(2)+AX(0)*AY(3)-AX(3)*AY(0)
     .        +AX(3)*I-AY(3)*J      
            Z3=AX(0)*AY(1)-AX(1)*AY(0)+AX(1)*I-AY(1)*J
            IF(Z2**2.LE.4*Z1*Z3) GO TO 15
            IF(ABS(Z1).GT.1.D-8) THEN
              Z2=Z2*(SQRT(1.D0-4*Z1*Z3/Z2**2)-1.D0)/(2*Z1)
            ELSE 
              Z2=-Z3/Z2-Z1*Z3**2/Z2**3
            ENDIF
            IF(ABS(AY(1)+AY(3)*Z2).GT.ABS(AX(1)+AX(3)*Z2)) THEN
              Z1=(I-AY(0)-AY(2)*Z2)/(AY(1)+AY(3)*Z2)
            ELSE
              Z1=(J-AX(0)-AX(2)*Z2)/(AX(1)+AX(3)*Z2)
            ENDIF
            IF((Z1.GE.-EPS).AND.(Z1.LE.1+EPS).AND.
     .         (Z2.GE.-EPS).AND.(Z2.LE.1+EPS)) THEN
              IF(S1.LE.MXSCL) THEN
                Z3=((I0+Z1)/K0)**4
                Z4=((J0+Z2)/K0)**4
                WT=(1-Z3)*(1-Z4)/((1+Z3)*(1+Z4)*SIGMA**2)
                W(1)=AY(2)*AZ(1)-AY(1)*AZ(2)
     .              +Z1*(AY(3)*AZ(1)-AY(1)*AZ(3))
     .              +Z2*(AY(2)*AZ(3)-AY(3)*AZ(2))
                W(2)=AZ(2)*AX(1)-AZ(1)*AX(2)
     .              +Z1*(AZ(3)*AX(1)-AZ(1)*AX(3))
     .              +Z2*(AZ(2)*AX(3)-AX(3)*AX(2))
                W(3)=AX(2)*AY(1)-AX(1)*AY(2)
     .              +Z1*(AX(3)*AY(1)-AX(1)*AY(3))
     .              +Z2*(AX(2)*AY(3)-AX(3)*AY(2))
                ALPHA=AZ(0)+AZ(1)*Z1+AZ(2)*Z2+AZ(3)*Z1*Z2 
                TMPL(I,J,1)=TMPL(I,J,1)+REAL(WT*W(1)/W(3)) ! Accumulate weighted slope 1
                TMPL(I,J,2)=TMPL(I,J,2)+REAL(WT*W(2)/W(3)) ! Accumulate weighted slope 2
                TMPL(I,J,3)=TMPL(I,J,3)                    ! Accumulate weighted albedo
     .             +REAL((AW(0)+AW(1)*Z1+AW(2)*Z2+AW(3)*Z1*Z2-1)*WT)
                ZHT(I,J)=ZHT(I,J)+REAL(ALPHA*WT)           ! Accumulate weighted map heights
                ZHT2(I,J)=ZHT2(I,J)+REAL(ALPHA**2.0*WT)    ! Accumulate weighted map squared hts
                NH(I,J)=NH(I,J)+REAL(WT)                   ! Accumulate weighted map numbers
              ENDIF
              FLAG=.TRUE.
              SMAX=MAX(SMAX,SMAX0)                         ! Accumulate max slope
            ENDIF
15          CONTINUE
          ENDDO
          ENDDO          
20        CONTINUE
          EFLG=EFLG.AND.DFLG
        ENDDO
        ENDDO
        IF(FLAG) WRITE(6,FMT='(A8)') NAME//COVERED
C        IF(FLAG) WRITE(70,FMT='(A8)') NAME//COVERED      ! Record maplets used
C        IF(EFLG) WRITE(71,FMT='(A6)') NAME               ! Record maplets completely inside
C        LMRKFILE='./LMKFILES/'//NAME//'.LMK'

C   Determine images used:

C        OPEN(UNIT=10, FILE=LMRKFILE, STATUS='OLD')
C25        CONTINUE
C          READ(10,FMT='(A80)') LINE
C          IF(LINE(1:8).NE.'PICTURES') GO TO 25
C26        CONTINUE
C          READ(10,FMT='(A80)') LINE
C          IF(LINE(1:12).NE.'MAP OVERLAPS') THEN
C            DO K=1,KPIC
C              IF(LINE(1:12).EQ.PICID(K)) GO TO 26
C            ENDDO
C            KPIC=KPIC+1
C            PICID(KPIC)=LINE(1:12)
C            GO TO 26
C          ENDIF
C        CLOSE(UNIT=10)

        NAME='END'
c        GO TO 10
      goto 31
30    CONTINUE
      write (*,*) 'Did not exit cleanly'

31      write (*,*) 'Continue'
C      CLOSE(UNIT=69)

C      WRITE(70,FMT='(A3,3X)') 'END'
C      WRITE(71,FMT='(A3,3X)') 'END'

C      CLOSE(UNIT=71)
C      CLOSE(UNIT=70)

      INFILE='USED_MAPS.TXT'
C      CALL PIXINLMX(INFILE)

C   Compute averages and standard deviations:

      Z1=0
      Z2=0
      IX=0
      JX=0
      Z4= 1.D10
      Z5=-1.D10
      DO I=-QSZ,QSZ
      DO J=-QSZ,QSZ
        HUSE(I,J)=.FALSE.
        SUSE(I,J)=.FALSE.
        IF(NH(I,J).GT.0) THEN
          HT(I,J)=ZHT(I,J)/NH(I,J)
          HUSE(I,J)=.TRUE.
          TMPL(I,J,3)=TMPL(I,J,3)/NH(I,J)
          ZHT2(I,J)=ZHT2(I,J)/NH(I,J)
          Z3=MAX(0.D0,ZHT2(I,J)-HT(I,J)**2)
          ZHT2(I,J)=REAL(S0*SQRT(Z3))
          SUSE(I,J)=.TRUE.
          Z1=Z1+ZHT2(I,J)
          Z2=Z2+1
          Z4=MIN(Z4,ZHT2(I,J))
          IF(ZHT2(I,J).GT.Z5) THEN
            Z5=ZHT2(I,J)
            IX=I
            JX=J
          ENDIF
        ENDIF
      ENDDO
      ENDDO
      
      INQUIRE(FILE='SIGMAS.TXT', EXIST=EX)
      IF(.NOT.EX) THEN
        OPEN(UNIT=66,FILE='SIGMAS.TXT',STATUS='UNKNOWN')
          WRITE(66,FMT='(A3,77X)') 'END'
        CLOSE(UNIT=66)
      ENDIF

C   Add to SIGMAS.TXT file:

c      IF(Z2.NE.0) THEN
c        K=0
c        OPEN(UNIT=66,FILE='SIGMAS.TXT',STATUS='UNKNOWN')
c300       CONTINUE
c          K=K+1
c          READ(66,FMT='(A80)') XLINE(K)
c          IF(XLINE(K)(1:3).NE.'END') GO TO 300
c        CLOSE(UNIT=66)
c        J=K-1
c        OPEN(UNIT=66,FILE='SIGMAS.TXT',STATUS='UNKNOWN')
c          DO K=1,J
c            WRITE(66,FMT='(A80)') XLINE(K)
c          ENDDO
c          WRITE(66,FMT='(A6, 2F24.6,26X)') BIGMAP, Z5, Z1/Z2
c          WRITE(66,FMT='(A3,77X)') 'END'
c        CLOSE(UNIT=66)
c      ENDIF
c
cC   Make SIGMAS.pgm file:
c
c      OPEN(UNIT=66, FILE='SIGMAS.DAT', ACCESS='DIRECT',
c     .     RECL=2*QSZ+1, STATUS='UNKNOWN')
c        DO J=-QSZ,QSZ
c          DO I=-QSZ,QSZ
c            IF(SUSE(I,J)) THEN
c              Z1=(ZHT2(I,J)-Z4)/(Z5-Z4)
c              OUTLINE(I+QSZ+1:I+QSZ+1)=CHAR(1+NINT(254*Z1))
c            ELSE
c              OUTLINE(I+QSZ+1:I+QSZ+1)=CHAR(0)
c            ENDIF
c          ENDDO
c          WRITE(66,REC=J+QSZ+1) OUTLINE(1:2*QSZ+1)
c        ENDDO
c      CLOSE(UNIT=66)

c      J=2*QSZ+1
c      INFILE='SIGMAS.DAT'
c      OUTFILE='SIGMAS.pgm'
c      CALL RAW2PGM(INFILE,OUTFILE,J,J)
c      OPEN(UNIT=55,FILE=INFILE,STATUS='OLD')
c      CLOSE(UNIT=55, STATUS='DELETE')
c      WRITE(6,*) Z4, Z5, IX+QSZ+1, JX+QSZ+1
c      WRITE(6,*) 'gc SIGMAS.pgm'           

C   Display map projection of averaged heights:

      CALL SHOW_SLOPES(BTMP,QSZ,HUSE,HT)

C   Choose to use averaged heights or
C   Choose to perform slope integration:

      WRITE(6,*)  
      WRITE(6,*) 'Height averaged map done'
      WRITE(6,*)  
      WRITE(6,*) '0. end processing'
      WRITE(6,*) '1. integrate slopes'
      READ(5,FMT='(A1)') ANS
      IF(ANS.EQ.'0') GO TO 230

C   Perform slope integration:

      WRITE(6,*) 'Input fraction'
      READ(5,*) Z1
      DO I=-QSZ,QSZ
      DO J=-QSZ,QSZ
        TUSE(I,J)=.FALSE.
        ZUSE(I,J)=.FALSE.
        IF(NH(I,J).GT.0) THEN
          TUSE(I,J)=.TRUE.
          TMPL(I,J,1)=TMPL(I,J,1)/NH(I,J)
          TMPL(I,J,2)=TMPL(I,J,2)/NH(I,J)
c          TMPL(I,J,3)=TMPL(I,J,3)/NH(I,J)
          ZHT(I,J)=ZHT(I,J)/NH(I,J)
          IF(RANN(SEED).LE.Z1) ZUSE(I,J)=.TRUE.
        ENDIF
      ENDDO
      ENDDO

      WRITE(6,*) 'Input weight'
      READ(5,*) Z1

220   CONTINUE

      DO K=1,20
        CALL SLP2HGT(BTMP,QSZ,TUSE,TMPL,ZUSE,ZHT,Z1, HUSE,HT)
        J0=0
        DO I=-QSZ,QSZ
        DO J=-QSZ,QSZ
          IF(HUSE(I,J)) J0=J0+1
        ENDDO
        ENDDO
        WRITE(6,*) J0, (2*QSZ+1)**2
      ENDDO
      CALL SHOW_SLOPES(BTMP,QSZ,HUSE,HT)

C   Options:

      WRITE(6,*) '0. end iteration'
      WRITE(6,*) '1. more iteration'
      WRITE(6,*) '2. change weight'
      READ(5,FMT='(A1)') ANS
      IF(ANS.EQ.'1') GO TO 220
      IF(ANS.EQ.'2') THEN
        WRITE(6,*) 'Input weight'
        READ(5,*) Z1
        GO TO 220
      ENDIF
      WRITE(6,*)  
      WRITE(6,*) 'Slope averaged map done'
      WRITE(6,*)  

230   CONTINUE                                           ! Finish up

      WRITE(6,*) '0.  No Template'
      WRITE(6,*) '1.  Square Template'
      WRITE(6,*) '2.  Round Template'
      READ(5,FMT='(A1)') ANS
      IF(ANS.NE.'0') THEN
        WRITE(6,*) 'Input half size'
        READ(5,*) Q0
      ENDIF
      DO I=-QSZ,QSZ
      DO J=-QSZ,QSZ
        IF(HUSE(I,J)) THEN
          IF((ANS.EQ.'1').AND.((ABS(I).GT.Q0).OR.(ABS(J).GT.Q0))) THEN
            HUSE(I,J)=.FALSE.
            HT(I,J)=0
          ENDIF
          IF((ANS.EQ.'2').AND.((I*I+J*J).GT.Q0*Q0)) THEN
            HUSE(I,J)=.FALSE.
            HT(I,J)=0
          ENDIF
        ENDIF
      ENDDO
      ENDDO

C   Adjust central height to zero

      IF(HUSE(0,0)) THEN
        Z1=HT(0,0)
        DO I=-QSZ,QSZ
        DO J=-QSZ,QSZ
          IF(HUSE(I,J)) HT(I,J)=HT(I,J)-REAL(Z1)
        ENDDO
        ENDDO
        V(1)=V(1)+Z1*S0*UZ(1)
        V(2)=V(2)+Z1*S0*UZ(2)
        V(3)=V(3)+Z1*S0*UZ(3)
      ENDIF
      CALL SHOW_SLOPES(BTMP,QSZ,HUSE,HT)

C   Sort images for BIGMAP.LMK file

      DO I=2,KPIC
      DO J=I-1,1,-1
        IF(PICID(J+1).LT.PICID(J)) THEN
          PICNM=PICID(J)
          PICID(J)=PICID(J+1)
          PICID(J+1)=PICNM
        ENDIF
      ENDDO
      ENDDO

C   Output BIGMAP.MAP file

      LMRKFILE='./MAPFILES/'//BIGMAP//'.MAP'
      CALL WRITE_MAP(LMRKFILE,BTMP,QSZ,S0,
     .                     V,VSIG,UX,UY,UZ,HT,HUSE,TMPL)

C   Output BIGMAP.LMK file

      LMRKFILE='BIGFILES/'//BIGMAP//'.LMK'
      OPEN(UNIT=10, FILE=LMRKFILE, STATUS='UNKNOWN')
        ANS='T'
        Z1=0.25
        Z2=0.05 
        WRITE(10,FMT='(A6,3X,A1,4X,2F8.4,50X)') BIGMAP, ANS, Z1, Z2
        WRITE(10,FMT='(I8,F12.7,40x,a20)') QSZ, S0,
     .                                '  SIZE, SCALE(KM)  '
        K=-1
        WRITE(10,FMT='(4I10,20X,A20)') K, K, K, K,
     .                                '  HORIZON          '
        Z1=0.5*S0
        Z2=0.D0
        WRITE(10,FMT='(2D20.10,20X,A20)')  Z1, Z2,
     .                                '  SIGKM, RMSLMK    '
        WRITE(10,FMT='(3D20.10,A20)') (V(I), I=1,3),
     .                                '  VLM              ' 
        WRITE(10,FMT='(3D20.10,A20)') (UX(I), I=1,3),
     .                                '  UX               ' 
        WRITE(10,FMT='(3D20.10,A20)') (UY(I), I=1,3),
     .                                '  UY               ' 
        WRITE(10,FMT='(3D20.10,A20)') (UZ(I), I=1,3),
     .                                '  UZ               ' 
        WRITE(10,FMT='(3D20.10,A20)') (VSIG(I), I=1,3),
     .                                '  SIGMA_LMK        ' 
        WRITE(10,FMT='(A8,72X)')  'PICTURES'
        DO K=1,KPIC
          PICNM=PICID(K)
          I=SLEN(PICNM)
          PICTFILE='./SUMFILES/'//PICNM(1:I)//'.SUM'
          OPEN(UNIT=30,FILE=PICTFILE,STATUS='OLD')
            READ(30,*)
            READ(30,*)
            READ(30,*) NPX, NLN
            READ(30,*) MMFL, CTR(1), CTR(2)
            READ(30,*) (V0(I), I=1,3) 
            READ(30,*) (CX(I), I=1,3)
            READ(30,*) (CY(I), I=1,3)
            READ(30,*) (CZ(I), I=1,3)
            READ(30,*) 
            READ(30,*)  KMAT(1,1), KMAT(1,2), KMAT(1,3),
     .                  KMAT(2,1), KMAT(2,2), KMAT(2,3)
            READ(30,FMT='(A80)') LINE
            IF(LINE(64:73).EQ.'DISTORTION') THEN
              READ(LINE,*) (D(I), I=1,4)
            ELSE
              D(1)=0.D0
              D(2)=0.D0
              D(3)=0.D0
              D(4)=0.D0
            ENDIF
          CLOSE(UNIT=30)
          CALL V2IMGPL(V,V0,PICNM,NPX,NLN,MMFL,CTR,KMAT,D,
     .                 CX,CY,CZ, USE,IMGPL)
          WRITE(10,FMT='(A12,2F10.2,48X)') PICNM, (IMGPL(I),I=1,2)
        ENDDO
        WRITE(10,FMT='(A12,68X)') 'MAP OVERLAPS'
        WRITE(10,FMT='(A9,71X)') 'LIMB FITS'
        WRITE(10,FMT='(A8,72X)') 'END FILE'
      CLOSE(UNIT=10)

      INQUIRE(FILE='BIGLIST.TXT', EXIST=EX)
      IF(.NOT.EX) THEN
        OPEN(UNIT=20,FILE='BIGLIST.TXT',STATUS='UNKNOWN')
          WRITE(20,FMT='(A6)') 'END   ' 
        CLOSE(UNIT=20)
      ENDIF

C   Add BIGMAP name to BIGLIST.TXT

      OPEN(UNIT=20,FILE='BIGLIST.TXT',STATUS='UNKNOWN')
        I=0
        FLAG=.TRUE.
310     CONTINUE
        I=I+1       
        READ(20,FMT='(A6)') XNAME(I) 
        IF(XNAME(I)(1:3).NE.'END') THEN
          LMRKFILE='./MAPFILES/'//XNAME(I)//'.MAP'
          INQUIRE(FILE=LMRKFILE, EXIST=EX)
          IF(.NOT.EX) THEN
            I=I-1
            GO TO 310
          ENDIF
          IF(XNAME(I).EQ.BIGMAP) FLAG=.FALSE.
          DO J=1,I-1
            IF(XNAME(I).EQ.XNAME(J)) THEN
              I=I-1
              GO TO 310
            ENDIF
          ENDDO
          GO TO 310
        ENDIF
      CLOSE(UNIT=20)
      OPEN(UNIT=20,FILE='BIGLIST.TXT',STATUS='UNKNOWN')
        DO J=1,I-1
          WRITE(20,FMT='(A6)') XNAME(J)
        ENDDO 
        IF(FLAG) THEN
          WRITE(20,FMT='(A6)') BIGMAP
        ENDIF
        WRITE(20,FMT='(A6)') 'END   ' 
      CLOSE(UNIT=20)

      WRITE(6,*)
      WRITE(6,*) 'view_map_rgb'
      WRITE(6,FMT='(A6)') BIGMAP

      STOP
      END

C   ................................................
      SUBROUTINE PIXINLMX(INFILE)
C   ................................................

      IMPLICIT NONE

      INTEGER               I, N
      CHARACTER*6           LMKNM
      CHARACTER*12          PICNM(100000)
      CHARACTER*12          XNAME
      CHARACTER*72          INFILE
      CHARACTER*72          LMKFILE
      CHARACTER*80          LINE

      N=0
      OPEN(UNIT=10, FILE=INFILE, STATUS='OLD')
10      READ(10,FMT='(A6)') LMKNM
        IF(LMKNM(1:3).NE.'END') THEN
          LMKFILE='LMKFILES/'//LMKNM//'.LMK'
          OPEN(UNIT=20, FILE=LMKFILE, STATUS='OLD')
20          READ(20,FMT='(A80)') LINE
            IF(LINE(1:8).NE.'PICTURES') GO TO 20
22          READ(20,FMT='(A80)') LINE
            IF(LINE(1:12).NE.'MAP OVERLAPS') THEN
              XNAME=LINE(1:12)
              DO I=1,N
                IF(XNAME.EQ.PICNM(I)) GO TO 22
              ENDDO
              N=N+1
              PICNM(N)=XNAME
              GO TO 22
            ENDIF
24          READ(20,FMT='(A80)') LINE
            IF(LINE(1:9).NE.'LIMB FITS') GO TO 24
26          READ(20,FMT='(A80)') LINE
            IF(LINE(1:8).NE.'END FILE') THEN
              XNAME=LINE(1:12)
              DO I=1,N
                IF(XNAME.EQ.PICNM(I)) GO TO 26
              ENDDO
              N=N+1
              PICNM(N)=XNAME
              GO TO 26
            ENDIF
          CLOSE(UNIT=20)
          GO TO 10
        ENDIF
      CLOSE(UNIT=10)

      OPEN(UNIT=10, FILE='USED_PICS.TXT', STATUS='UNKNOWN')
        DO I=1,N
          WRITE(10, FMT='(1X,A12)') PICNM(I)
        ENDDO
        WRITE(10, FMT='(A3,10X)') 'END'
      CLOSE(UNIT=10)

      RETURN
      END

