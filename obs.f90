!Credit to Lu Yi {yilu@westlake.edu.cn}
PROGRAM obs
    IMPLICIT NONE
    
    INTEGER::N,M,I,J
    INTEGER::NunZero, Nzero
    REAL,ALLOCATABLE::LAT(:),LON(:),RAIN(:)
    REAL,ALLOCATABLE::READIN(:,:)
    REAL::MISSING_R
    CHARACTER(LEN=100)::INFILE
    CHARACTER(LEN=100)::DATE_N, SUFFIX, FILENAME, FMT_INFO, FMT_EACH
    LOGICAL::CONNECTED
 
! SET THE INITIAL VALUE
   I=1
   J=1
   M=3                     !set the number of type, 3:lat,lon,rain
   MISSING_R  = -888888.   !set the number    
   NunZero    = 0
   Nzero      = 0
    
   WRITE(*,*)'PLEAS WRITE THE INPUT FILENAME'
   READ(*,*)INFILE
   
   !WRITE(*,*)'PLEAS WRITE THE NUMBER OF THE STATIONS'
   !READ(*,*)N       !set the total number of stations
   N = 285684
    WRITE(*,*)'PLEAS WRITE THE OUTPUT FILENAME:ob.rain.yyyymmddhh.xxh'
    READ(*,*) FILENAME
    
    WRITE(*,*)'PLEAS WRITE THE END DATE TIME:2014-07-22_05:59:59'
    READ(*,*) DATE_N
 
!  1.FORMAT
!------------
   FMT_INFO = '(A12, 1X, A19, 1X, I6, 2(F12.3,2X), F8.1, 1X, A5)'
   FMT_EACH = '(F12.3, F12.3, I4, F12.3)'
   
   
! 2. OPEN A FILE FOR OBSERVATIONS INPUT AND OUTPUT
! --------------------------------------
! 2.1 INPUT FILE 
! ---------------

   
   open(10,FILE=INFILE)
   ALLOCATE(READIN(M,N))
   
   DO J=1,N
    READ(10,*)(READIN(I,J),I=1,M)
   END DO 
   
   !OPEN(20,FILE='READIN.TXT')
   !DO J=1,N
       !DO I=1,M
        !WRITE(20,"(F15.8)",ADVANCE='NO') READIN(I,J)
      ! END DO
        !WRITE(20,*)
   ! END DO 
   
   ALLOCATE(LAT(N))
   ALLOCATE(LON(N))
   ALLOCATE(RAIN(N))
   
   DO I=1,N
       LAT(I)=READIN(1,I)
       LON(I)=READIN(2,I)
       RAIN(I)=READIN(3,I)
       IF (RAIN(I)==0.0) THEN 
          ! RAIN(I)= MISSING_R 
           Nzero  = Nzero +1 
       END IF
       !write(*,*)LAT(I)
   END DO 
NunZero = N - Nzero
WRITE(*,*)"The number of NunZero is ", NunZero
WRITE(*,*)"The number of Nzero is ", Nzero

! 2.2 OUTPUT FILE 
! ---------------

    !INQUIRE(UNIT=999, OPENED=CONNECTED)
    !IF (.NOT. CONNECTED) THEN
    OPEN(UNIT=999, FILE=FILENAME, STATUS='REPLACE')
    !END IF 
    REWIND (UNIT=999)
    
    
! 3. FILE HEADER AND DATA
! -----------------------

! 3.1 TOTAL NUMBER OF OBSERVATIONS CONTAINED IN FILE
! ---------------------------------------------------
   WRITE(UNIT = 999, FMT = '((A,I7),A)', ADVANCE = 'no')  &      
   "TOTAL =", N,","

! 3.2 MISSING VALUE FLAG 
! ------------------------

   WRITE(UNIT = 999, FMT = '((A, F8.0), A)') "MISS. =", MISSING_R,","

! 3.3 FORMAT 
! ------------

   WRITE(UNIT = 999, FMT = '(A)')  & 
      "INFO  = PLATFORM, DATE, LEVELS, LATITUDE, LONGITUDE, ELEVATION, ID."
   WRITE(UNIT = 999, FMT = '(A)' ) &
      "EACH  = HEIGHT, RAINFALL DATA, QC, ERROR"

! 3.4 DATA 
! --------

   WRITE (UNIT = 999, FMT = '(A)') &
   "#------------------------------------------------------------------------------#"
   DO I=1,N
         !IF (RAIN(I)==0.0)CYCLE
         WRITE(UNIT = 999, FMT = TRIM(FMT_INFO))   & 
             "FM-129 RAIN ", DATE_N, 1, LAT(I), LON(I),99.9, "12345"
         WRITE(UNIT = 999, FMT = TRIM(FMT_EACH))   &
             99.9, RAIN(I), 88, 2.000
   END DO   

   CLOSE(999)
   
STOP
END 

   

    
    
    
    
    
    
    
