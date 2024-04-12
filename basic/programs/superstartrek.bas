OPTION EXPLICIT 
OPTION DEFAULT NONE 
OPTION BASE 1

' Get random integer number from 1 to 8
FUNCTION RND1TO8() AS INTEGER
  LOCAL INTEGER RR = Int(Rnd()*7.98+1.01)
  RND1TO8=RR 
END FUNCTION

SUB SPLASHSCREEN
LOCAL STRING XX$, X0$, READY$

'STRINGS FOR THE FOLLOWING BRIEF TEXT
If STARBASES_TOTAL = 1 Then
  XX$=""
  X0$=" IS "
Else
  XX$="S"
  X0$=" ARE "
EndIf

'BRIEF TEXT
PRINT:PRINT:PRINT:PRINT:PRINT:PRINT:PRINT:PRINT:PRINT:PRINT:PRINT
PRINT "                                    ,------*------,"
PRINT "                    ,-------------   '---  ------'"
PRINT "                     '-------- --'      / /"
PRINT "                         ,---' '-------/ /--,"
PRINT "                          '----------------'":PRINT
PRINT "                    THE USS ENTERPRISE --- NCC-1701"
PRINT :PRINT :PRINT :PRINT :PRINT
PRINT "YOUR ORDERS ARE AS FOLLOWS:"
PRINT "   DESTROY THE ";KLINGONS_TOTAL;" KLINGON WARSHIPS WHICH HAVE INVADED"
PRINT "   THE GALAXY BEFORE THEY CAN ATTACK FEDERATION HEADQUARTERS"
PRINT "   ON STARDATE"; STARDATE_STARTING + STARDATES_TO_COMPLETE ;", THIS GIVES YOU"; STARDATES_TO_COMPLETE ;" DAYS. THERE";X0$
PRINT "  "; STARBASES_TOTAL ;" STARBASE";XX$;" IN THE GALAXY FOR RESUPPLYING YOUR SHIP"
PRINT : PRINT
INPUT "PRESS KEY IF READY TO ACCEPT COMMAND";READY$
PRINT

END SUB


SUB PUTOBJECT(QX AS INTEGER, QY AS INTEGER, OBJECT AS INTEGER)
LOCAL INTEGER X, Y, FOUND
FOUND=0
DO WHILE FOUND=0
  X = RND1TO8()+((QX-1)*8)
  Y = RND1TO8()+((QY-1)*8)
  IF GALAXY(X,Y) = 0 THEN
    FOUND=1
    GALAXY(X,Y) = OBJECT
    PUTX = X : PUTY = Y
  ENDIF 
LOOP
END SUB

SUB PUTOBJECTS(QX AS INTEGER, QY AS INTEGER, COUNT AS INTEGER, OBJECT AS INTEGER) 
  LOCAL INTEGER COUNTER
  COUNTER = COUNT
  DO WHILE COUNTER > 0
    COUNTER = COUNTER - 1
    PUTOBJECT QX, QY, OBJECT
  LOOP
END SUB

FUNCTION COUNTINQUADRANT(QX AS INTEGER, QY AS INTEGER, OBJECT AS INTEGER) AS INTEGER
  LOCAL INTEGER X, Y, RESULT
  RESULT = 0
  FOR Y = 1 TO 8
    FOR X = 1 TO 8
      IF GALAXY(X+(QX-1)*8,Y+(QY-1)*8) = OBJECT THEN RESULT = RESULT + 1
    NEXT X
  NEXT Y
  COUNTINQUADRANT = RESULT
END FUNCTION

FUNCTION DRAWOBJECT$(OBJECT AS INTEGER)
  LOCAL RESULT$
  RESULT$=FORMAT$(OBJECT)
  IF OBJECT = 0 THEN RESULT$=" " ' EMPTY SPACE
  IF OBJECT = 1 THEN RESULT$="K" ' KLINGON
  IF OBJECT = 2 THEN RESULT$="!" ' STARBASE
  IF OBJECT = 3 THEN RESULT$="*" ' STAR 

  IF OBJECT = 9 THEN RESULT$="@" ' ENETRPRIZE 

  IF OBJECT < 0 THEN RESULT$="~" 'format$(OBJECT - 10) ' path 

  DRAWOBJECT$ = RESULT$
END FUNCTION

SUB PRINTSYSINFO(SYSID AS INTEGER)
  LOCAL SYSM$(8) LENGTH 50
  SYSM$(1) = "       STARDATE           " + FORMAT$(INT(STARDATE_CURRENT*10)*.1)
  SYSM$(2) = "       CONDITION          " '+ CC$
  SYSM$(3) = "       QUADRANT           " + FORMAT$(QUADRANTX)+","+FORMAT$(QUADRANTY)
  SYSM$(4) = "       SECTOR             " + FORMAT$(ENTERPRISE(1))+","+FORMAT$(ENTERPRISE(2))
  SYSM$(5) = "       PHOTON TORPEDOES   " + FORMAT$(Int(ENTERPRISE(5)))
  SYSM$(6) = "       TOTAL ENERGY       " + FORMAT$(Int(ENTERPRISE(3) + ENTERPRISE(4)))
  SYSM$(7) = "       SHIELDS            " + FORMAT$(Int(ENTERPRISE(4)))
  SYSM$(8) = "       KLINGONS REMAINING " + FORMAT$(Int(0))

  PRINT SYSM$(SYSID)
END SUB

SUB SRS
  LOCAL INTEGER X, Y, I
  LOCAL INTEGER QX = QUADRANTX
  LOCAL INTEGER QY = QUADRANTY
  PRINT:PRINT:PRINT

  PRINT "  X: |";
  FOR I = 1 TO 8
    PRINT " "+FORMAT$(I+(QX-1)*8, "%02g")+"|";
  NEXT I
  PRINT
  PRINT "  +------------------------------------"
  PRINT "  |####################################"
  PRINT "Y |# +-1- -2- -3- -4- -5- -6- -7- -8- #"
  FOR Y = 1 TO 8
    PRINT FORMAT$(Y+(QY-1)*8, "%02g")+"|# "+FORMAT$(Y)+"";
    FOR X = 1 TO 8
      PRINT " "+DRAWOBJECT$(GALAXY(X+(QX-1)*8,Y+(QY-1)*8))+" .";
    NEXT X
    PRINT "#";
    PRINTSYSINFO Y
    'PRINT
  NEXT Y
  PRINT "  |####################################"
  PRINT:PRINT:PRINT
END SUB

SUB LRS
  LOCAL INTEGER X, Y, I, S, B, K, LQX, LQY 
  LOCAL STRING FILLER$, HLP$
  LOCAL INTEGER QX = QUADRANTX
  LOCAL INTEGER QY = QUADRANTY
  PRINT:PRINT:PRINT

  PRINT "RADAR DATA:"
  PRINT "   ";
  FOR I = -1 TO 1
    PRINT CHOICE((QX+I)>0 AND (QX+I)<9, FORMAT$(QX+I,"   %1g  "),"   X  ");
  NEXT I
  PRINT
  PRINT "   +-----+-----+-----+"

  FOR Y = -1 TO 1
   PRINT CHOICE((QY+Y)>0 AND (QY+Y)<9, FORMAT$(QY+Y," %1g ")," X ");
   FOR X = -1 TO 1
    LQX = QX + X
    LQY = QY + Y

    IF X=0 AND Y=0 THEN
      FILLER$="="
    ELSE 
      FILLER$=" "
    ENDIF 

     PRINT "|"+FILLER$;
     IF (LQX > 0) AND (LQY > 0) AND (LQX < 9) AND (LQY < 9)  THEN
      K = COUNTINQUADRANT(LQX, LQY, 1)
      B = COUNTINQUADRANT(LQX, LQY, 2)
      S = COUNTINQUADRANT(LQX, LQY, 3)
      PRINT FORMAT$(K)+FORMAT$(B)+FORMAT$(S)+ FILLER$;
    ELSE
      PRINT "XXX ";
    ENDIF
   NEXT X
   PRINT "|"
   PRINT "   +-----+-----+-----+"
  NEXT Y

  PRINT
  INPUT "HELP NEEDED? ('YES' TO PROVIDE)"; HLP$
  IF UCASE$(HLP$) <> "YES" THEN RETURN

  PRINT
  PRINT "*:LONG RANGE SCAN:*" 
  PRINT
  PRINT "RADAR SHOWS DATA ABOUT QUADRANTS DIRECTLY AROUND ENTERPRISE."
  PRINT "ENTERPRIZE IS IN CETER OF RADAR, DEPICTED BY '=NNN=' SIGN"
  PRINT
  PRINT "RADAR DATA IS SHOWN IN NUMBERS,"
  PRINT "      FORMATEED AS FOLLOWING:          K B S"
  PRINT "                                       | | |"
  PRINT "     NUMBER OR 'KLINGONS' >------------+ | |"
  PRINT "     NUMBER OR 'STAR BASES' >------------+ |"
  PRINT "     NUMBER OR 'STARS' >-------------------+"
  PRINT
  PRINT "FOR EXAMPLE: '123' MEANS THAT THERE ARE:"
  PRINT "    '1' KLINGON, '2' STAR BASES, AND '3' STARS IN QUADRANT"
  PRINT:PRINT:PRINT

END SUB

SUB NAV
  LOCAL FLOAT DIRECTION, X, Y 
  local INTEGER SPEED, I, PENALTY

  PRINT
  PRINT "*:NAVIGATION:*"
  PRINT "     COURSE IS IN A CIRCULAR NUMERICAL      6  7  8  "
  PRINT "     VECTOR ARRANGEMENT AS SHOWN.            \ | /   "
  PRINT "     INTEGER AND REAL VALUES MAY BE           \|/    "
  PRINT "     USED.  (THUS COURSE 1.5 IS HALF-     5 ---*--- 1"
  PRINT "     WAY BETWEEN 1 AND 2)                     /|\    "
  PRINT "                                             / | \   "
  PRINT "                                            4  3  2  "
  PRINT
  
  DO
    INPUT "COURSE (1-8)"; DIRECTION
    IF DIRECTION < 1 OR DIRECTION > 8 THEN PRINT "ENSIGN CHEKOV REPORTS,  'INCORRECT COURSE DATA, SIR!'"
  LOOP UNTIL DIRECTION >= 1 AND DIRECTION <= 8
  DIRECTION = (DIRECTION - 1) * 0.785398

  DO
    INPUT "WARP FACTOR (1-63)"; SPEED
    IF SPEED < 1 OR SPEED > 63 THEN PRINT "ENSIGN CHEKOV REPORTS, 'WARP FACTOR MUST BE BETWEEN 1 AND 63, CAPTAIN!'"
  LOOP UNTIL SPEED >= 1 AND SPEED <= 63

  FOR X = 1 to 64
    FOR Y = 1 to 64
      IF GALAXY(X,Y) < 0 THEN GALAXY(X,Y) = 0
    NEXT Y
  NEXT X
   
  X = ENTERPRISE(1)
  Y = ENTERPRISE(2)
  'PRINT "ENTERPRIZE TRAVERSE THROUGH FOLLOWING SECTORS:"
  'PRINT 0 ": ("+ FORMAT$(ENTERPRISE(1), "%02g")  + "," + FORMAT$(ENTERPRISE(2), "%02g") + ")" 
  LOCAL INTEGER TOX, TOY
  FOR I = 1 TO SPEED

    TOX = CINT(ENTERPRISE(1) + I * COS(DIRECTION))
    TOY = CINT(ENTERPRISE(2) + I * SIN(DIRECTION))
    
    'PRINT I ": (" + FORMAT$(TOX, "%02g")  + "," + FORMAT$(TOY, "%02g") + ")";
    
    IF TOX > 64 OR TOY > 64 OR TOX < 1 OR TOY < 1 THEN
      PRINT " < THE NAVIGATION IS OUT OF GALAXY BOUNDARIES!"

      PRINT "LT. UHURA REPORTS MESSAGE FROM STARFLEET COMMAND:"
      PRINT "  'PERMISSION TO ATTEMPT CROSSING OF GALACTIC PERIMETER"
      PRINT "  IS HEREBY *DENIED*.  SHUT DOWN YOUR ENGINES.'"
      PRINT "CHIEF ENGINEER SCOTT REPORTS  'WARP ENGINES SHUT DOWN"
      PRINT "  AT GALACTIC PERIMETER': (" + FORMAT$(X, "%02g")  + "," + FORMAT$(Y, "%02g") + ")
      EXIT FOR
    ENDIF

    IF GALAXY(TOX,TOY) > 0 THEN 
      'PRINT " < THE '"+ DRAWOBJECT$(GALAXY(TOX,TOY))+"' AT THIS SECTOR BLOCKED NAVIGATION!"
      'PRINT "              ENETERPRIZE WILL STOP AT PREVIOUS SECTOR: (" + FORMAT$(X, "%02g")  + "," + FORMAT$(Y, "%02g") + ")
      'PRINT
      
      PENALTY = (I * 10) + RND1TO8()
      PRINT
      PRINT "DUE TO BAD NAVIGATION, ENTERPRIZE PERFROM EMERGENCY STOP!"
      PRINT "ENGINES SHUT DOWN AT SECTOR: (" + FORMAT$(X, "%02g")  + "," + FORMAT$(Y, "%02g") + ")"
      PRINT "CHIEF ENGINEER SCOTT REPORTS  'ADDITIONAL ENERGY CONSUMPTION FOR EMERGENCY STOP IS: "+ FORMAT$(PENALTY) + " UNITS'"

      EXIT FOR    
    ENDIF

    X = TOX : Y = TOY
    IF GALAXY(X, Y)  = 0 THEN GALAXY(X, Y) = (I * -1) - 1
    'PRINT

  NEXT I

  ' SUBSTARCT ENERGY
  ENTERPRISE(3) = ENTERPRISE(3) - I - 10


  GALAXY(ENTERPRISE(1), ENTERPRISE(2)) = -1
  ENTERPRISE(1) = CINT(X): ENTERPRISE(2) = CINT(Y)
  
  GALAXY(ENTERPRISE(1), ENTERPRISE(2)) = 9

  QUADRANTX = INT((ENTERPRISE(1)-1) \ 8) +1 
  QUADRANTY = INT((ENTERPRISE(2)-1) \ 8) +1 
END SUB

SUB PRINTGALAXY
  LOCAL INTEGER I, X, Y
  
  PRINT
  FOR I = 1 TO 64+9
    PRINT "-";
  NEXT I
  PRINT
  
  FOR Y = 1 TO 64
    PRINT "|";
    FOR X = 1 TO 64
      PRINT  DRAWOBJECT$(GALAXY(X,Y));
      IF X MOD 8 = 0 THEN PRINT "|";
    NEXT X
    PRINT 
    IF Y MOD 8 = 0 THEN 
      FOR I = 1 TO 64+9
        PRINT "-";
      NEXT I
      PRINT
    ENDIF
  NEXT Y
END SUB

SUB PLACEENTERPRISE
  ' DETERMENING ENETRPRIZE POSITION
  RANDOMIZE EPOCH(NOW) 
  PUTX = 0: PUTY = 0
  PUTOBJECT RND1TO8(), RND1TO8(), 9 '9 - ENTERPRISE
  ENTERPRISE(1) = PUTX
  ENTERPRISE(2) = PUTY

  QUADRANTX = INT((ENTERPRISE(1)-1) \ 8) +1 
  QUADRANTY = INT((ENTERPRISE(2)-1) \ 8) +1 

  ENTERPRISE(3) = 3000 ' ENERGY
  ENTERPRISE(4) = 0    ' SHIELDS
  ENTERPRISE(5) = 10   ' TORPEDOES

END SUB

SUB GENERATEGALAXY
  LOCAL INTEGER QX, QY, X, Y, KLINGONS_IN_QUADRANT, STARBASES_IN_QUADRANT, STARS_IN_QUADRANT
  LOCAL FLOAT R1

  RANDOMIZE EPOCH(NOW) 
  FOR QY = 1 TO 8
    FOR QX = 1 TO 8
      KLINGONS_IN_QUADRANT = 0
      R1=Rnd(1)
      If R1>.80 Then KLINGONS_IN_QUADRANT=1
      If R1>.95 Then KLINGONS_IN_QUADRANT=2
      If R1>.98 Then KLINGONS_IN_QUADRANT=3

      STARBASES_IN_QUADRANT = 0
      If Rnd(1)>.96 Then STARBASES_IN_QUADRANT=1

      STARS_IN_QUADRANT = RND1TO8()

      PUTOBJECTS QX, QY, KLINGONS_IN_QUADRANT, 1
      PUTOBJECTS QX, QY, STARBASES_IN_QUADRANT, 2
      PUTOBJECTS QX, QY, STARS_IN_QUADRANT, 3

      KLINGONS_TOTAL = KLINGONS_TOTAL + KLINGONS_IN_QUADRANT
      STARBASES_TOTAL = STARBASES_TOTAL + STARBASES_IN_QUADRANT
      STARS_TOTAL = STARS_TOTAL + STARS_IN_QUADRANT      
    NEXT QX
  NEXT QY

  IF STARBASES_TOTAL < 1 THEN
    X = RND1TO8()
    Y = RND1TO8()

    ' 1 = KLINGONS
    IF COUNTINQUADRANT(X, Y, 1) < 2 THEN 
      PUTOBJECTS X, Y, 1, 1
      KLINGONS_TOTAL = KLINGONS_TOTAL + 1
    ENDIF
    ' 2 = STARBASES
    PUTOBJECTS X, Y, 1, 2
    STARBASES_TOTAL = 1
  ENDIF

  STARDATE_CURRENT      = INT(RND()*20+20)*100
  STARDATE_STARTING     = STARDATE_CURRENT
  STARDATES_TO_COMPLETE = 25 + INT(RND()*10)

  IF KLINGONS_TOTAL > STARDATES_TO_COMPLETE THEN STARDATES_TO_COMPLETE=KLINGONS_TOTAL+1

END SUB

FUNCTION QUADRANTNAME$(QX AS INTEGER, QY AS INTEGER) AS STRING
  LOCAL Q(2,8) AS STRING
  LOCAL INTEGER X, Y 

  Q$(1,1)="ANTARES"
  Q$(1,2)="RIGEL"
  Q$(1,3)="PROCYON"
  Q$(1,4)="VEGA"
  Q$(1,5)="CANOPUS"
  Q$(1,6)="ALTAIR"
  Q$(1,7)="SAGITTARIUS"
  Q$(1,8)="POLLUX"

  Q$(2,1)="SIRIUS"
  Q$(2,2)="DENEB"
  Q$(2,3)="CAPELLA"
  Q$(2,4)="BETELGEUSE"
  Q$(2,5)="ALDEBARAN"
  Q$(2,5)="REGULUS"
  Q$(2,7)="ARCTURUS"
  Q$(2,8)="SPICA"

  X = CHOICE((QX / 4) > 1, 2, 1)
  Y = QY


  QUADRANTNAME$ = Q$(X,Y) +" "+ FORMAT$(QX - (X-1))

END FUNCTION




SUB ENTERQUADRANT
  LOCAL STRING Q$ = QUADRANTNAME$(QUADRANTX, QUADRANTY) 

  If STARDATE_CURRENT=STARDATE_STARTING Then
    Print "YOUR MISSION BEGINS WITH YOUR STARSHIP LOCATED"
    Print "IN THE GALACTIC QUADRANT, '";Q$;"'."
  Else
    Print "NOW ENTERING ";Q$;" QUADRANT . . ."
  EndIf
END SUB

FUNCTION DISTANCE(X1 AS INTEGER, Y1 AS INTEGER, X2 AS INTEGER, Y2 AS INTEGER) AS INTEGER
  DISTANCE = INTC(SQR((X1-X2)^2 + (Y1-Y2)^2))
END FUNCTION





'########################### PROGRAM START
SUB MAIN
  ' GLOBAL TECHNICAL VARIABLES
  DIM PUTX AS INTEGER, PUTY AS INTEGER
  DIM QUADRNTS(8,8) AS INTEGER

  ' GLOBAL GAME VARIABLES
  DIM INTEGER  GALAXY(64,64)
  DIM INTEGER ENTERPRISE(10)  ' X,Y, [<SYSTEMS STATUS>] 
  DIM INTEGER QUADRANTX, QUADRANTY
  DIM STRING  COMMAND

  DIM INTEGER KLINGONS_TOTAL  = 0
  DIM INTEGER STARBASES_TOTAL = 0
  DIM INTEGER STARS_TOTAL     = 0
  DIM INTEGER STARDATE_CURRENT      = 0
  DIM INTEGER STARDATE_STARTING     = 0
  DIM INTEGER STARDATES_TO_COMPLETE = 0


  GENERATEGALAXY
  PLACEENTERPRISE
  SPLASHSCREEN
  ENTERQUADRANT
  SRS
  


  MAINLOOP:
    INPUT "COMMAND, SIR"; COMMAND$
      SELECT CASE UCASE$(COMMAND$)
        CASE "NAV"
          NAV
          SRS
        CASE "SRS"
          SRS
        CASE "LRS"
          LRS
        CASE "PHA"
          PRINT "NOT IMPLEMENTED"
        CASE "TOR"
        PRINT "NOT IMPLEMENTED"
        CASE "SHE"
          PRINT "NOT IMPLEMENTED"
        CASE "DAM"
          PRINT "NOT IMPLEMENTED"
        CASE "COM"
          PRINT "NOT IMPLEMENTED"
        CASE "XXX"
          PRINT "NOT IMPLEMENTED"
        CASE "MAP"
          PRINTGALAXY
        CASE Else
          PRINT "ENTER ONE OF THE FOLLOWING:"
          PRINT "  NAV  (TO SET COURSE)"
          PRINT "  SRS  (FOR SHORT RANGE SENSOR SCAN)"
          PRINT "  LRS  (FOR LONG RANGE SENSOR SCAN)"
          PRINT "  PHA  (TO FIRE PHASERS)"
          PRINT "  TOR  (TO FIRE PHOTON TORPEDOES)"
          PRINT "  SHE  (TO RAISE OR LOWER SHIELDS)"
          PRINT "  DAM  (FOR DAMAGE CONTROL REPORTS)"
          PRINT "  COM  (TO CALL ON LIBRARY-COMPUTER)"
          PRINT "  XXX  (TO RESIGN YOUR COMMAND)"
          PRINT
      END SELECT
  GOTO MAINLOOP

END SUB

MAIN
