10 Rem SUPER STARTREK - MAY 16,1978 - REQUIRES 24K MEMORY
30 Rem
40 Rem ****        **** STAR TREK ****        ****
50 Rem **** SIMULATION OF A MISSION OF THE STARSHIP ENTERPRISE,
60 Rem **** AS SEEN ON THE STAR TREK TV SHOW.
70 Rem **** ORIGIONAL PROGRAM BY MIKE MAYFIELD, MODIFIED VERSION
80 Rem **** PUBLISHED IN DEC'S "101 BASIC GAMES", BY DAVE AHL.
90 Rem **** MODIFICATIONS TO THE LATTER (PLUS DEBUGGING) BY BOB
100 Rem *** LEEDOM - APRIL & DECEMBER 1974,
110 Rem *** WITH A LITTLE HELP FROM HIS FRIENDS . . .
120 Rem *** COMMENTS, EPITHETS, AND SUGGESTIONS SOLICITED --
130 Rem *** SEND TO:  R. C. LEEDOM
140 Rem ***           WESTINGHOUSE DEFENSE & ELECTRONICS SYSTEMS CNTR.
150 Rem ***           BOX 746, M.S. 338
160 Rem ***           BALTIMORE, MD  21203
170 Rem ***
180 Rem *** CONVERTED TO MICROSOFT 8 K BASIC 3/16/78 BY JOHN GORDERS
190 Rem *** LINE NUMBERS FROM VERSION STREK7 OF 1/12/75 PRESERVED AS
200 Rem *** MUCH AS POSSIBLE WHILE USING MULTIPLE STATEMENTS PER LINE
205 Rem *** SOME LINES ARE LONGER THAN 72 CHARACTERS; THIS WAS DONE
210 Rem *** BY USING "?" INSTEAD OF "PRINT" WHEN ENTERING LINES
215 Rem ***
220 Print :Print :Print :Print :Print :Print :Print :Print :Print :Print :Print
221 Print "                                    ,------*------,"
222 Print "                    ,-------------   '---  ------'"
223 Print "                     '-------- --'      / /"
224 Print "                         ,---' '-------/ /--,"
225 Print "                          '----------------'":Print
226 Print "                    THE USS ENTERPRISE --- NCC-1701"
227 Print :Print :Print :Print :Print

    Rem
    Function FND(D): FND=Sqr((K(I,1)-S1)^2+(K(I,2)-S2)^2):End Function

    Rem Get random integer number from 1 to 8
    Function FNR(R): FNR=Int(Rnd(R)*7.98+1.01):End Function


    Rem G(8,8)= GALAXY (elements: KBS number K:Klingon, B:Bases, S:Stars)
    Rem Z(8,8)= EXPLORED GALAXY (elements: 0= not known, KBS= see above)
    Rem C(9,2)= DIRECTION DELTA, C({1-9},1):Y, C({1-9},2):X
    Rem K(3,3)= KLINGONS IN QUADRANT K(#,X), K(#,Y), K(#, LIFE)
    Rem  NN(3)= ???
    Rem   D(8)= DAMAGE PER SYSTEM
    Dim G(8,8),C(9,2),K(3,3),NN(3),Z(8,8),D(8)

    Rem SET DEFAULTS
    Rem ZZ$= EMPTY ROW OF SECTOR
    Rem A1$= AVAILABLE COMMANDS
    Rem   T= CURRRENT STARDATE
    Rem  T0= STARTING STARDATE
    Rem  T9= STARDATES TO COMPLETE MISSION
    Rem  D0= DOCKED FLAG. 1:DOCKED, 0:NOT DOCKED
    Rem   E= CURRENT ENERGY
    Rem  E0= STARTING ENERGY
    Rem   P= CURRENT (P)HOTON TORPEDOES
    Rem  P0= STARTING (P)HOTON TORPEDOES
    Rem   S=
    Rem  S9= TOTAL?
    Rem  K9= TOTAL (K)LINGONS
    Rem  B9= TOTAL STAR (B)ASES
    ZZ$="                         "
    A1$="NAVSRSLRSPHATORSHEDAMCOMXXX"
    T=Int(Rnd(1)*20+20)*100
    T0=T
    T9=25+Int(Rnd(1)*10)
    D0=0
    E=3000
    E0=E
    P=10
    P0=P
    S9=200
    S=0
    B9=2
    K9=0

    Rem INITIALIZE ENTERPRIZE'S POSITION
    Rem Q1,Q2= ENTERPRIZE COORDINATES QUADRANT (?Q1:X, Q2:Y?)
    Rem S1,S2= ENTERPRIZE COORDINATES SECTOR (?S1:X, S2:Y?)
    Q1=FNR(1)
    Q2=FNR(1)
    S1=FNR(1)
    S2=FNR(1)

530 For I=1 To 9
      C(I,1)=0
      C(I,2)=0
    Next I
    C(2,1)=-1
    C(3,1)=-1
    C(4,1)=-1
    C(4,2)=-1
    C(5,2)=-1
    C(6,2)=-1
    C(1,2)=1
    C(2,2)=1
    C(6,1)=1
    C(7,1)=1
    C(8,1)=1
    C(8,2)=1
    C(9,2)=1

670 For I=1 To 8
      D(I)=0
    Next I

810 Rem SETUP WHAT EXHISTS IN THE GALAXY . . .
815 Rem K3= # KLINGONS  B3= # STARBASES  S3 = # STARS
    Rem K9= Total (K)LINGONS
    Rem B9= TOTAL STAR (B)ASES
820  For I=1 To 8
       For J=1 To 8
         K3=0
         Z(I,J)=0
         R1=Rnd(1)
          If R1>.80 Then K3=1
          If R1>.95 Then K3=2
          If R1>.98 Then K3=3
         K9=K9+K3
         B3=0
         If Rnd(1)>.96 Then B3=1
         B9=B9+B3

         G(I,J)=K3*100+B3*10+FNR(1)
       Next J
     Next I

     If K9>T9 Then T9=K9+1

     If B9=0 Then
       If G(Q1,Q2)<200 Then
         G(Q1,Q2)=G(Q1,Q2)+100
         K9=K9+1
       EndIf
       B9=1
       G(Q1,Q2)=G(Q1,Q2)+10
       Q1=FNR(1)
       Q2=FNR(1)
     EndIf

     Rem SAVE TOTAL KLINGONS COUNT FOR THE FUTURE EFFICIENCY CALCULATION
     K7=K9

     Rem STRINGS FOR THE FOLLOWING BRIEF TEXT
     If B9=1 Then
       XX$=""
       X0$=" IS "
     Else
       XX$="S"
       X0$=" ARE "
     EndIf

1230 Print "YOUR ORDERS ARE AS FOLLOWS:"
1240 Print "   DESTROY THE ";K9;" KLINGON WARSHIPS WHICH HAVE INVADED"
1252 Print "   THE GALAXY BEFORE THEY CAN ATTACK FEDERATION HEADQUARTERS"
1260 Print "   ON STARDATE";T0+T9;", THIS GIVES YOU";T9;" DAYS. THERE";X0$
1272 Print "  ";B9;" STARBASE";XX$;" IN THE GALAXY FOR RESUPPLYING YOUR SHIP"
     Print : Print
     Input "PRESS KEY IF READY TO ACCEPT COMMAND";READY$
1280 Print
1300 I=Rnd(1)

1310 Rem HERE ANY TIME NEW QUADRANT ENTERED
1320 Z4=Q1:Z5=Q2:K3=0:B3=0:S3=0:G5=0:D4=.5*Rnd(1):Z(Q1,Q2)=G(Q1,Q2)
1390 If Q1<1 Or Q1>8 Or Q2<1 Or Q2>8 Then GoTo 1600
1430 GoSub 9030:Print

     Rem
     If T0=T Then
       Print "YOUR MISSION BEGINS WITH YOUR STARSHIP LOCATED"
       Print "IN THE GALACTIC QUADRANT, '";G2$;"'."
     Else
       Print "NOW ENTERING ";G2$;" QUADRANT . . ."
     EndIf

1500 Print
     K3=Int(G(Q1,Q2)*.01)      'KLINGONS
     B3=Int(G(Q1,Q2)*.1)-10*K3 'BASES
     S3=G(Q1,Q2)-100*K3-10*B3  'STARS

     If K3>0 Then
       Print "COMBAT AREA      CONDITION RED"
      If S<200 Then
       Print "   SHIELDS DANGEROUSLY LOW"
      EndIf
     EndIf


1590 For I=1 To 3:K(I,1)=0:K(I,2)=0:Next I

1600 For I=1 To 3:K(I,3)=0:Next I
     Q$=ZZ$+ZZ$+ZZ$+ZZ$+ZZ$+ZZ$+ZZ$+Left$(ZZ$,17) 'CLEAR QADRANT DATA

1660 Rem POSITION ENTERPRISE IN QUADRANT, THEN PLACE "K3" KLINGONS, &
1670 Rem "B3" STARBASES, & "S3" STARS ELSEWHERE.
1680 aa$="<*>"
     Z1=S1:Z2=S2
     InsertObjectToQuadrant aa$, Z1, Z2

     If K3>0 Then
       For I=1 To K3
         FindEmptyPlaceInQuadrant
         aa$="+K+"
         Z1=R1:Z2=R2
         InsertObjectToQuadrant aa$, Z1, Z2
         K(I,1)=R1:K(I,2)=R2    'COORDINATES
         K(I,3)=S9*(0.5+Rnd(1)) 'HEALTH
       Next I
     EndIf

     If B3>0 Then
       FindEmptyPlaceInQuadrant
       aa$=">!<"
       Z1=R1:Z2=R2
       B4=R1:B5=R2
       InsertObjectToQuadrant aa$, Z1, Z2
     EndIf

     For I=1 To S3
       FindEmptyPlaceInQuadrant
       aa$=" * "
       Z1=R1:Z2=R2
       InsertObjectToQuadrant aa$, Z1, Z2
     Next I

1980 GoSub 6430

Rem check for low energy, entrypoint for command prompt
1990 If (S+E<10) And (E<10 Or D(7)<0) Then
       Print
       Print "** FATAL ERROR **   YOU'VE JUST STRANDED YOUR SHIP IN "
       Print "SPACE"
       Print "YOU HAVE INSUFFICIENT MANEUVERING ENERGY,";
       Print " AND SHIELD CONTROL"
       Print "IS PRESENTLY INCAPABLE OF CROSS";
       Print "-CIRCUITING TO ENGINE ROOM!!"
       GoTo 6220
     EndIf


2060 Input "COMMAND";aa$
     Select Case UCase$(aa$)
       Case "NAV"
         GoTo 2300
       Case "SRS"
         GoTo 1980
       Case "LRS"
         GoTo 4000
       Case "PHA"
         GoTo 4260
       Case "TOR"
         GoTo 4700
       Case "SHE"
         GoTo 5530
       Case "DAM"
         GoTo 5690
       Case "COM"
         GoTo 7290
       Case "XXX"
         GoTo 6270
       Case Else
         Print "ENTER ONE OF THE FOLLOWING:"
         Print "  NAV  (TO SET COURSE)"
         Print "  SRS  (FOR SHORT RANGE SENSOR SCAN)"
         Print "  LRS  (FOR LONG RANGE SENSOR SCAN)"
         Print "  PHA  (TO FIRE PHASERS)"
         Print "  TOR  (TO FIRE PHOTON TORPEDOES)"
         Print "  SHE  (TO RAISE OR LOWER SHIELDS)"
         Print "  DAM  (FOR DAMAGE CONTROL REPORTS)"
         Print "  COM  (TO CALL ON LIBRARY-COMPUTER)"
         Print "  XXX  (TO RESIGN YOUR COMMAND)"
         Print
         GoTo 1990
       End Select




2290 Rem COURSE CONTROL BEGINS HERE
2300 Input "COURSE (0-9)";C1:If C1=9 Then C1=1
2310 If C1>=1 And C1<9 Then GoTo 2350
2330 Print "   LT. SULU REPORTS, 'INCORRECT COURSE DATA, SIR!'":GoTo 1990
2350 XX$="8":If D(1)<0 Then XX$="0.2"
2360 Print "WARP FACTOR (0-";XX$;")";:Input W1:If D(1)<0 And W1>0.2 Then GoTo 2470
2380 If (W1>0 And W1<=8) Then GoTo 2490
2390 If W1=0 Then 1990
2420 Print "   CHIEF ENGINEER SCOTT REPORTS 'THE ENGINES WON'T TAKE";
2430 Print " WARP ";W1;"!'":GoTo 1990
2470 Print "WARP ENGINES ARE DAMAGED.  MAXIUM SPEED = WARP 0.2":GoTo 1990
2490 N=Int(W1*8+.5):If E-N>=0 Then GoTo 2590
2500 Print "ENGINEERING REPORTS   'INSUFFICIENT ENERGY AVAILABLE"
2510 Print "                       FOR MANEUVERING AT WARP";W1;"!'"
2530 If S<N-E Or D(7)<0 Then GoTo 1990
2550 Print "DEFLECTOR CONTROL ROOM ACKNOWLEDGES";S;" UNITS OF ENERGY"
2560 Print "                         PRESENTLY DEPLOYED TO SHIELDS."
2570 GoTo 1990
2580 Rem KLINGONS MOVE/FIRE ON MOVING STARSHIP . . .
2590 For I=1 To K3:If K(I,3)=0 Then GoTo 2700
2610 aa$="   ":Z1=K(I,1):Z2=K(I,2):GoSub 8670:GoSub 8590
2660 K(I,1)=Z1:K(I,2)=Z2:aa$="+K+":GoSub 8670
2700 Next I: GoSub 6000:D1=0:D6=W1:If W1>=1 Then D6=1
2770 For I=1 To 8:If D(I)>=0 Then GoTo 2880
2790 D(I)=D(I)+D6:If D(I)>-.1 And D(I)<0 Then D(I)=-.1:GoTo 2880
2800 If D(I)<0 Then GoTo 2880
2810 If D1<>1 Then D1=1:Print "DAMAGE CONTROL REPORT:  ";
2840 Print Tab(8);:R1=I:GoSub 8790:Print G2$;" REPAIR COMPLETED."
2880 Next I: If Rnd(1)>.2 Then GoTo 3070
2910 R1=FNR(1):If Rnd(1)>=.6 Then GoTo 3000
2930 D(R1)=D(R1)-(Rnd(1)*5+1):Print "DAMAGE CONTROL REPORT:  ";
2960 GoSub 8790: Print G2$;" DAMAGED":Print :GoTo 3070
3000 D(R1)=D(R1)+Rnd(1)*3+1:Print "DAMAGE CONTROL REPORT:  ";
3030 GoSub 8790: Print G2$;" STATE OF REPAIR IMPROVED":Print
3060 Rem BEGIN MOVING STARSHIP
3070 aa$="   ":Z1=Int(S1):Z2=Int(S2):GoSub 8670
3110 CC1=Fix(C1): Print C1: Print CC1
     X1=C(CC1,1)+(C(CC1+1,1)-C(CC1,1))*(C1-Int(C1)):X=S1:Y=S2
3140 X2=C(CC1,2)+(C(CC1+1,2)-C(CC1,2))*(C1-Int(C1)):Q4=Q1:Q5=Q2
3170 For I=1 To N:S1=S1+X1:S2=S2+X2
If S1<1 Or S1>=9 Or S2<1 Or S2>=9 Then GoTo 3500
3240 S8=Int(S1)*24+Int(S2)*3-26:If Mid$(Q$,S8,2)="  " Then GoTo 3360
3320 S1=Int(S1-X1):S2=Int(S2-X2):Print "WARP ENGINES SHUT DOWN AT ";
3350 Print "SECTOR";S1;",";S2;"DUE TO BAD NAVAGATION":GoTo 3370
3360 Next I: S1=Int(S1):S2=Int(S2)
3370 aa$="<*>":Z1=Int(S1):Z2=Int(S2):GoSub 8670:GoSub 3910:T8=1
3430 If W1<1 Then T8=.1*Int(10*W1)
3450 T=T+T8:If T>T0+T9 Then GoTo 6220
3470 Rem SEE IF DOCKED, THEN GET COMMAND
3480 GoTo 1980
3490 Rem EXCEEDED QUADRANT LIMITS
3500 X=8*Q1+X+N*X1:Y=8*Q2+Y+N*X2:Q1=Int(X/8):Q2=Int(Y/8):S1=Int(X-Q1*8)
3550 S2=Int(Y-Q2*8):If S1=0 Then Q1=Q1-1:S1=8
3590 If S2=0 Then Q2=Q2-1:S2=8
3620 X5=0:If Q1<1 Then X5=1:Q1=1:S1=1
3670 If Q1>8 Then X5=1:Q1=8:S1=8
3710 If Q2<1 Then X5=1:Q2=1:S2=1
3750 If Q2>8 Then X5=1:Q2=8:S2=8
3790 If X5=0 Then GoTo 3860
3800 Print "LT. UHURA REPORTS MESSAGE FROM STARFLEET COMMAND:"
3810 Print "  'PERMISSION TO ATTEMPT CROSSING OF GALACTIC PERIMETER"
3820 Print "  IS HEREBY *DENIED*.  SHUT DOWN YOUR ENGINES.'"
3830 Print "CHIEF ENGINEER SCOTT REPORTS  'WARP ENGINES SHUT DOWN"
3840 Print "  AT SECTOR";S1;",";S2;"OF QUADRANT";Q1;",";Q2;".'"
3850 If T>T0+T9 Then GoTo 6220
3860 If 8*Q1+Q2=8*Q4+Q5 Then GoTo 3370
3870 T=T+1:GoSub 3910:GoTo 1320
3900 Rem MANEUVER ENERGY S/R **
3910 E=E-N-10:If E>=0 Then Return
3930 Print "SHIELD CONTROL SUPPLIES ENERGY TO COMPLETE THE MANEUVER."
3940 S=S+E:E=0:If S<=0 Then S=0
3980 Return
3990 Rem LONG RANGE SENSOR SCAN CODE
4000 If D(3)<0 Then Print "LONG RANGE SENSORS ARE INOPERABLE":GoTo 1990
4030 Print "LONG RANGE SCAN FOR QUADRANT";Q1;",";Q2
4040 O1$="-------------------":Print O1$
4060 For I=Q1-1 To Q1+1:NN(1)=-1:NN(2)=-2:NN(3)=-3:For J=Q2-1 To Q2+1
4120 If I>0 And I<9 And J>0 And J<9 Then NN(J-Q2+2)=G(I,J):Z(I,J)=G(I,J)
4180 Next J: For L=1 To 3:Print ": ";:If NN(L)<0 Then Print "*** ";:GoTo 4230
4210 Print Right$(Str$(NN(L)+1000),3);" ";
4230 Next L: Print ":":Print O1$:Next I:GoTo 1990
4250 Rem PHASER CONTROL CODE BEGINS HERE
4260 If D(4)<0 Then Print "PHASERS INOPERATIVE":GoTo 1990
4265 If K3>0 Then GoTo 4330
4270 Print "SCIENCE OFFICER SPOCK REPORTS  'SENSORS SHOW NO ENEMY SHIPS"
4280 Print "                                IN THIS QUADRANT'":GoTo 1990
4330 If D(8)<0 Then Print "COMPUTER FAILURE HAMPERS ACCURACY"
4350 Print "PHASERS LOCKED ON TARGET;  ";
4360 Print "ENERGY AVAILABLE =";E;" UNITS"
4370 Input " NUMBER OF UNITS TO FIRE";X:If X<=0 Then 1990
4400 If E-X<0 Then GoTo 4360
4410 E=E-X:If D(7)<0 Then X=X*Rnd(1)
4450 H1=Int(X/K3):For I=1 To 3:If K(I,3)<=0 Then GoTo 4670
4480 H=Int((H1/FND(0))*(Rnd(1)+2)):If H>.15*K(I,3) Then GoTo 4530
4500 Print "SENSORS SHOW NO DAMAGE TO ENEMY AT ";K(I,1);",";K(I,2):GoTo 4670
4530 K(I,3)=K(I,3)-H:Print H;" UNIT HIT ON KLINGON AT SECTOR";K(I,1);",";
4550 Print K(I,2):If K(I,3)<=0 Then Print "*** KLINGON DESTROYED ***":GoTo 4580
4560 Print "   (SENSORS SHOW";K(I,3);" UNITS REMAINING)":GoTo 4670
4580 K3=K3-1:K9=K9-1:Z1=K(I,1):Z2=K(I,2):aa$="   ":GoSub 8670
4650 K(I,3)=0:G(Q1,Q2)=G(Q1,Q2)-100:Z(Q1,Q2)=G(Q1,Q2):If K9<=0 Then GoTo 6370
4670 Next I: GoSub 6000:GoTo 1990
4690 Rem PHOTON TORPEDO CODE BEGINS HERE
4700 If P<=0 Then Print "ALL PHOTON TORPEDOES EXPENDED":GoTo 1990
4730 If D(5)<0 Then Print "PHOTON TUBES ARE NOT OPERATIONAL":GoTo 1990
4760 Input "PHOTON TORPEDO COURSE (1-9)";C1:If C1=9 Then C1=1
4780 If C1>=1 And C1<9 Then GoTo 4850
4790 Print "ENSIGN CHEKOV REPORTS,  'INCORRECT COURSE DATA, SIR!'"
4800 GoTo 1990
4850 X1=C(C1,1)+(C(C1+1,1)-C(C1,1))*(C1-Int(C1)):E=E-2:P=P-1
4860 X2=C(C1,2)+(C(C1+1,2)-C(C1,2))*(C1-Int(C1)):X=S1:Y=S2
4910 Print "TORPEDO TRACK:"
4920 X=X+X1:Y=Y+X2:X3=Int(X+.5):Y3=Int(Y+.5)
4960 If X3<1 Or X3>8 Or Y3<1 Or Y3>8 Then GoTo 5490
5000 Print "               ";X3;",";Y3:aa$="   ":Z1=X:Z2=Y:GoSub 8830
5050 If Z3<>0 Then GoTo 4920
5060 aa$="+K+":Z1=X:Z2=Y:GoSub 8830:If Z3=0 Then GoTo 5210
5110 Print "*** KLINGON DESTROYED ***":K3=K3-1:K9=K9-1:If K9<=0 Then GoTo 6370
5150 For I=1 To 3:If X3=K(I,1) And Y3=K(I,2) Then GoTo 5190
5180 Next I: I=3
5190 K(I,3)=0:GoTo 5430
5210 aa$=" * ":Z1=X:Z2=Y:GoSub 8830:If Z3=0 Then GoTo 5280
5260 Print "STAR AT";X3;",";Y3;"ABSORBED TORPEDO ENERGY.":GoSub 6000:GoTo 1990
5280 aa$=">!<":Z1=X:Z2=Y:GoSub 8830:If Z3=0 Then GoTo 4760
5330 Print "*** STARBASE DESTROYED ***":B3=B3-1:B9=B9-1
5360 If B9>0 Or K9>T-T0-T9 Then GoTo 5400
5370 Print "THAT DOES IT, CAPTAIN!!  YOU ARE HEREBY RELIEVED OF COMMAND"
5380 Print "AND SENTENCED TO 99 STARDATES AT HARD LABOR ON CYGNUS 12!!"
5390 GoTo 6270
5400 Print "STARFLEET COMMAND REVIEWING YOUR RECORD TO CONSIDER"
5410 Print "COURT MARTIAL!":D0=0
5430 Z1=X:Z2=Y:aa$="   ":GoSub 8670
5470 G(Q1,Q2)=K3*100+B3*10+S3:Z(Q1,Q2)=G(Q1,Q2):GoSub 6000:GoTo 1990
5490 Print "TORPEDO MISSED":GoSub 6000:GoTo 1990
5520 Rem SHIELD CONTROL
5530 If D(7)<0 Then Print "SHIELD CONTROL INOPERABLE":GoTo 1990
5560 Print "ENERGY AVAILABLE =";E+S;:Input " NUMBER OF UNITS TO SHIELDS";X
5580 If X<0 Or S=X Then Print "<SHIELDS UNCHANGED>":GoTo 1990
5590 If X<=E+S Then GoTo 5630
5600 Print "SHIELD CONTROL REPORTS  'THIS IS NOT THE FEDERATION TREASURY.'"
5610 Print "<SHIELDS UNCHANGED>":GoTo 1990
5630 E=E+S-X:S=X:Print "DEFLECTOR CONTROL ROOM REPORT:"
5660 Print "  'SHIELDS NOW AT";Int(S);" UNITS PER YOUR COMMAND.'":GoTo 1990
5680 Rem DAMAGE CONTROL
5690 If D(6)>=0 Then 5910
5700 Print "DAMAGE CONTROL REPORT NOT AVAILABLE":If D0=0 Then GoTo 1990
5720 D3=0:For I=1 To 8:If D(I)<0 Then D3=D3+.1
5890 Next I: T=T+D3+.1
