
TIMER = 0
' Get random integer number from 1 to 8
Function RND1TO8(R)
  LOCAL RR = Int(Rnd(R)*7.98+1.01)
  RND1TO8=RR 
End Function

SUB FINDEMPTY(QX, QY, OBJECT)
LOCAL X, Y, FOUND
FOUND=0
DO WHILE FOUND=0
  X = RND1TO8(1)+((QX-1)*8)
  Y = RND1TO8(1)+((QY-1)*8)
  IF GALAXY(X,Y) = 0 THEN
    FOUND=1
    GALAXY(X,Y) = OBJECT
  ENDIF 
LOOP
END SUB

FUNCTION COUNTINQUADRANT(QX, QY, OBJECT)
  LOCAL X, Y, RESULT
  RESULT = 0
  FOR Y = 1 TO 8
    FOR X = 1 TO 8
      IF GALAXY(X+(QX-1)*8,Y+(QY-1)*8) = OBJECT THEN RESULT = RESULT + 1
  COUNTINQUADRANT = RESULT
END FUNCTION

SUB PLACEOBJECT(QX, QY, COUNT, OBJECT)
  LOCAL COUNTER
  COUNTER = COUNT
  DO WHILE COUNTER > 0
    COUNTER = COUNTER - 1
    FINDEMPTY QX, QY, OBJECT
  LOOP
END SUB

FUNCTION DRAWOBJECT$(OBJECT)
  LOCAL RESULT$
  RESULT$=FORMAT$(OBJECT)
  IF OBJECT = 0 THEN RESULT$=" " ' KLINGON
  IF OBJECT = 1 THEN RESULT$="K" ' KLINGON
  IF OBJECT = 2 THEN RESULT$="!" ' STARBASE
  IF OBJECT = 3 THEN RESULT$="*" ' STAR 
  DRAWOBJECT$ = RESULT$
END FUNCTION


' GENERATING GALAXY
DIM GALAXY(64,64)
DIM QUADRNTS(8,8)

KLINGONS_TOTAL = 0
STARBASES_TOTAL = 0
STARS_TOTAL = 0

SEED = VAL(RIGHT$(TIME$, 2)) + VAL(MID$(TIME$,4,2))*100 + VAL(LEFT$(TIME$, 2))*10000
RANDOMIZE SEED

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

    PLACEOBJECT QX, QY, KLINGONS_IN_QUADRANT, 1
    PLACEOBJECT QX, QY, STARBASES_IN_QUADRANT, 2
    PLACEOBJECT QX, QY, STARS_IN_QUADRANT, 3

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
    PLACEOBJECT X, Y, 1, 1
    KLINGONS_TOTAL = KLINGONS_TOTAL + 1
  ENDIF
  ' 2 = STARBASES
  PLACEOBJECT X, Y, 1, 2
  STARBASES_TOTAL = 1
ENDIF








PRINT
FOR I = 1 TO 64+9
  PRINT "-";
NEXT I
PRINT
FOR Y = 1 TO 64
PRINT "|";
FOR X = 1 TO 64
'PRINT "("X","Y")="GALAXY(X,Y)" ";
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

PRINT "KLINGONS: " KLINGONS_TOTAL
PRINT "STARBASES: " STARBASES_TOTAL
PRINT "STARS: " STARS_TOTAL