C$ Abstract
C
C   This subroutine determines which maplets and maps include a given surface
C   point.  A surface point can be specified by its image-space location in an
C   image, by its latitude and longitude or by its coordinates in a map or 
C   maplet.    
C
C$ Disclaimer
C
C
C$ Required_Reading
C
C     R.W. Gaskell, et.al, "Characterizing and navigating small bodies
C           with imaging data", Meteoritics & Planetary Science 43,
C           Nr 6, 1049-1061 (2008)
C
C$ Declarations
     
      IMPLICIT NONE
            
      INTEGER           BTMP
      PARAMETER        (BTMP=2501)
      
      DOUBLE PRECISION  RPD
      DOUBLE PRECISION  VDOT
      DOUBLE PRECISION  V(3)
      DOUBLE PRECISION  IMGPL(2)
      DOUBLE PRECISION  UX(3)
      DOUBLE PRECISION  UY(3)
      DOUBLE PRECISION  UZ(3)
      DOUBLE PRECISION  W(3)
      DOUBLE PRECISION  SCALE
      DOUBLE PRECISION  Z0
      DOUBLE PRECISION  Z1
      DOUBLE PRECISION  Z2
      DOUBLE PRECISION  Z3
      
      REAL*4            ZH(-BTMP:BTMP,-BTMP:BTMP)
      REAL*4            AL(-BTMP:BTMP,-BTMP:BTMP)

      INTEGER           I
      INTEGER           J
      INTEGER           QSZ

      CHARACTER*1       ANS
      CHARACTER*6       LMKNM
      CHARACTER*12      PICNM
      CHARACTER*72      INFILE
      CHARACTER*72      LMRKFILE
      
      LOGICAL           USE
      LOGICAL           EX
 
C$ Variable_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     None
C
C$ File_I/O
C
C     Filename                      I/O  Description
C     ----------------------------  ---  -------------------------------
C     BIGLIST.TXT                    I   File listing the big maps.
C     LMRKLIST.TXT                   I   File listing the landmark maps
C     LMRKLISTO.TXT                  I   File listing nearby landmark maps
C
C$ Restrictions
C
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
C     None
C
C$ SPC_subroutines_called
C     IMGPL2VN
C     READ_HEADER
C     U2VN
c
C$ SPICELIB_functions_called
C     RPD
C     VDOT
C
C$ SPICELIB_subroutines_called
C     LATREC
C     RECLAT
C     VSUB
C
C$ CALLED_BY_SPC_PROGRAMS
C     LITHOS
C      

      WRITE(6,*) 'pixlinpic (p), ltdlng (l), map p/l (m), quit (q)?'
      READ(5,FMT='(A1)') ANS
      IF ((ANS.EQ.'Q').OR.(ANS.EQ.'q')) THEN
        STOP
      ENDIF
      IF ((ANS.EQ.'P').OR.(ANS.EQ.'p')) THEN
        WRITE(6,*) 'input picture name'
        READ(5,FMT='(A12)') PICNM
        WRITE(6,*) 'input px,ln'
        READ(5,*) IMGPL(1), IMGPL(2)  
        CALL IMGPL2VN(PICNM,IMGPL,USE,V,UZ)
        IF(.NOT.USE) THEN
          WRITE(6,*) 'Point not found'
          RETURN
        ENDIF
      ENDIF
      IF ((ANS.EQ.'L').OR.(ANS.EQ.'l')) THEN
        WRITE(6,*) 'Input ltd, wlng (deg)'
        READ(5,*) Z1, Z2
        CALL LATREC(1.d0,-Z2*RPD(),Z1*RPD(), W)
        CALL U2VN(W,V,UZ)
      ENDIF
      IF ((ANS.EQ.'M').OR.(ANS.EQ.'m')) THEN
        WRITE(6,*) 'Input 6 character map name'
        READ(5,FMT='(A6)') LMKNM
        LMRKFILE='MAPFILES/'//LMKNM//'.MAP'
        CALL READ_MAP(LMRKFILE,BTMP,QSZ,SCALE,V,UX,UY,UZ,ZH,AL)
        DO I=-QSZ,QSZ
        DO J=-QSZ,QSZ
          IF(AL(I,J).LE.(0.005)) ZH(I,J)=0
        ENDDO
        ENDDO
        WRITE(6,*) 'Input patch center (map p/l)'
        READ(5,*) Z1, Z2
        Z1=Z1-QSZ
        Z2=Z2-QSZ
        Z0=0
        IF((ABS(Z1).LE.QSZ).AND.(ABS(Z2).LE.QSZ)) THEN
          Z0=ZH(NINT(Z1),NINT(Z2))
        ENDIF
        V(1)=V(1)+SCALE*(Z1*UY(1)+Z2*UX(1)+Z0*UZ(1))
        V(2)=V(2)+SCALE*(Z1*UY(2)+Z2*UX(2)+Z0*UZ(2))
        V(3)=V(3)+SCALE*(Z1*UY(3)+Z2*UX(3)+Z0*UZ(3))
      ENDIF
      CALL RECLAT(v,Z1,Z2,Z3)
      Z1=Z1*1000
      Z3=Z3/RPD()
      Z2=-Z2/RPD()
      IF(Z2.LT.0) Z2=360+Z2
      WRITE(6,*)
      WRITE(6,FMT='(1X,A14,3F12.6)') 'Lat/Lon/Rad = ', Z3, Z2, Z1

      WRITE(6,*)
      WRITE(6,*) 'BIGMAPS'
      OPEN(UNIT=10,FILE='BIGLIST.TXT',STATUS='UNKNOWN')
10     CONTINUE       
        READ(10,FMT='(A6)') LMKNM
        IF(LMKNM(1:3).NE.'END') THEN
          LMRKFILE='MAPFILES/'//LMKNM//'.MAP'
          INQUIRE(FILE=LMRKFILE, EXIST=EX)
          IF(.NOT.EX) GO TO 10
          CALL READ_HEADER(LMRKFILE,QSZ,SCALE,W,UX,UY,UZ)
          Z1=QSZ*SCALE
          CALL VSUB(V,W,W)
          IF(ABS(VDOT(W,UX)).GT.Z1) GO TO 10
          IF(ABS(VDOT(W,UY)).GT.Z1) GO TO 10
          IF(ABS(VDOT(W,UZ)).GT.Z1) GO TO 10
          Z1=QSZ+VDOT(W,UY)/SCALE
          Z2=QSZ+VDOT(W,UX)/SCALE
          WRITE(6,FMT='(A6,3F12.6)') LMKNM, SCALE, Z1, Z2
          GO TO 10
        ENDIF
      CLOSE(UNIT=10)

      RETURN
      END


