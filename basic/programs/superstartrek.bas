' Get random integer number from 1 to 8
FUNCTION RND1TO8(R)
  LOCAL RR = Int(Rnd(R)*7.98+1.01)
  RND1TO8=RR 
END FUNCTION

SUB PUTOBJECT(QX, QY, OBJECT)
LOCAL X, Y, FOUND
FOUND=0
DO WHILE FOUND=0
  X = RND1TO8(1)+((QX-1)*8)
  Y = RND1TO8(1)+((QY-1)*8)
  IF GALAXY(X,Y) = 0 THEN
    FOUND=1
    GALAXY(X,Y) = OBJECT
    PUTX = X : PUTY = Y
  ENDIF 
LOOP
END SUB

SUB PUTOBJECTS(QX, QY, COUNT, OBJECT)
  LOCAL COUNTER
  COUNTER = COUNT
  DO WHILE COUNTER > 0
    COUNTER = COUNTER - 1
    PUTOBJECT QX, QY, OBJECT
  LOOP
END SUB

FUNCTION COUNTINQUADRANT(QX, QY, OBJECT)
  LOCAL X, Y, RESULT
  RESULT = 0
  FOR Y = 1 TO 8
    FOR X = 1 TO 8
      IF GALAXY(X+(QX-1)*8,Y+(QY-1)*8) = OBJECT THEN RESULT = RESULT + 1
    NEXT X
  NEXT Y
  COUNTINQUADRANT = RESULT
END FUNCTION

FUNCTION DRAWOBJECT$(OBJECT)
  LOCAL RESULT$
  RESULT$=FORMAT$(OBJECT)
  IF OBJECT = 0 THEN RESULT$=" " ' EMPTY SPACE
  IF OBJECT = 1 THEN RESULT$="K" ' KLINGON
  IF OBJECT = 2 THEN RESULT$="!" ' STARBASE
  IF OBJECT = 3 THEN RESULT$="*" ' STAR 

  IF OBJECT = 9 THEN RESULT$="@" ' ENETRPRIZE 

  DRAWOBJECT$ = RESULT$
END FUNCTION

SUB PRINTSYSINFO(SYSID)
  LOCAL SYSM$(8) LENGTH 50
  SYSM$(1) = "       STARDATE           "+FORMAT$(Int(T*10)*.1)
  SYSM$(2) = "       CONDITION          "+CC$
  SYSM$(3) = "       QUADRANT           "+FORMAT$(QUADRANTX)+","+FORMAT$(QUADRANTY)
  SYSM$(4) = "       SECTOR             "+FORMAT$(ENTERPRISE(1))+","+FORMAT$(ENTERPRISE(2))
  SYSM$(5) = "       PHOTON TORPEDOES   "+FORMAT$(Int(P))
  SYSM$(6) = "       TOTAL ENERGY       "+FORMAT$(Int(E+S))
  SYSM$(7) = "       SHIELDS            "+FORMAT$(Int(S))
  SYSM$(8) = "       KLINGONS REMAINING "+FORMAT$(Int(K9))

  PRINT SYSM$(SYSID)
END SUB

SUB SRS
  LOCAL X, Y, I
  LOCAL QX = QUADRANTX
  LOCAL QY = QUADRANTY
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
  LOCAL X, Y, S, B, K, LQX, LQY FILLER$, HLP$
  LOCAL QX = QUADRANTX
  LOCAL QY = QUADRANTY
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
  LOCAL DIRECTION, SPEED, X, Y
  INPUT "DIRECTION"; DIRECTION
  INPUT "SPEED"; SPEED
  DIRECTION = (DIRECTION - 1)* .785

  X = (ENTERPRISE(1) + SPEED * COS(DIRECTION))
  Y = (ENTERPRISE(2) + SPEED * SIN(DIRECTION))

  GALAXY(ENTERPRISE(1), ENTERPRISE(2)) = 0
  ENTERPRISE(1) = CINT(X): ENTERPRISE(2) = CINT(Y)
  GALAXY(ENTERPRISE(1), ENTERPRISE(2)) = 9

  QUADRANTX = INT((ENTERPRISE(1)-1) \ 8) +1 
  QUADRANTY = INT((ENTERPRISE(2)-1) \ 8) +1 

END SUB

SUB PRINTGALAXY
  LOCAL I, X, Y
  
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
END SUB

SUB GENERATEGALAXY
  LOCAL QX, QY, X, Y

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
    IF COUNTINQUADRANT(X, Y, 1) < 1 THEN 
      PUTOBJECTS X, Y, 1, 1
      KLINGONS_TOTAL = KLINGONS_TOTAL + 1
    ENDIF
    ' 2 = STARBASES
    PUTOBJECTS X, Y, 1, 2
    STARBASES_TOTAL = 1
  ENDIF

END SUB

'########################### PROGRAM START

' GLOBAL TECHNICAL VARIABLES
DIM PUTX, PUTY AS INTEGER
DIM QUADRNTS(8,8) AS INTEGER

' GLOBAL GAME VARIABLES
DIM GALAXY(64,64) AS INTEGER
DIM ENTERPRISE(10) AS INTEGER ' X,Y, [<SYSTEMS STATUS>] 
DIM QUADRANTX, QUADRANTY AS INTEGER

KLINGONS_TOTAL = 0
STARBASES_TOTAL = 0
STARS_TOTAL = 0



GENERATEGALAXY
PLACEENTERPRISE

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
      CASE "GAL"
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

