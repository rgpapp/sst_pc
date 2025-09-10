5 Randomize Timer
10  ' SUPER STARTREK - MAY 16,1978 - REQUIRES 24K MEMORY (AT LEAST)
20  '
30  ' ***        **** STAR TREK ****      ****
40  ' *** SIMULATION OF A MISSION OF THE STARSHIP ENTERPRISE,
50  ' *** AS SEEN ON THE STAR TREK TV SHOW.
60  ' *** ORIGINAL PROGRAM BY MIKE MAYFIELD, MODIFIED VERSION
70  ' *** PUBLISHED IN DEC'S "101 BASIC GAMES", BY DAVE AHL.
80  ' *** MODIFICATIONS TO THE LATTER (PLUS DEBUGGING) BY BOB
90  ' *** LEEDOM - APRIL & DECEMBER 1974,
100 ' *** WITH A LITTLE HELP FROM HIS FRIENDS . . .
110 ' *** COMMENTS, EPHITETS, AND SUGGESTIONS SOLICITED --
120 ' *** SEND TO: R.C. LEEDOM
130 ' ***          WESTINGHOSE DEFENSE & ELECTRONICS SYSTEMS CNIR
140 ' ***          BOX 746, M.S. 338
150 ' ***          BALTIMORE, MD 21203
160 ' ***
170 ' *** CONVERTED TO MICROSOFT 8 K BASIC 3/16/78 BY JOHN BORDERS
180 ' *** LINE NUMBERS FROM VERSION TREK7 OF 1/12/75 PRESERVED AS
190 ' *** MUCH AS POSSIBLE WHILE USING MULTIPLE STATEMENTS PER LINE
200 ' ***
210 ' *** MODIFIED TO RUN ON GRANT SEARLE'S 9-CHIP Z80 COMPUTER
220 ' *** AND DERIVATIVES 04-AUG-2018 BY N.KENDRICK
230 ' *** (LINKER3000-AT-GMAIL.COM)
240 ' ***
250 ' *** ADAPTED TO MSBASIC V5.0+ INTEGER ROUNDING ON 18/8/18 BY FEILIPU
260 ' ***
    ' ***  Adapted for the PicoCalc by Ron Lauzon, 3/29/2025
    '
270 Dim G(8,8)
    Dim C(9,2)
    Dim K(3,3)
    Dim N1(3)
    Dim Z(8,8)
    Dim D(8)
290 CLS : Font 7: Color RGB(green)
300 Print "THE USS ENTERPRISE --- NCC-1701"
310 Print
330 Print "                  ,------*------,"
340 Print "  ,-------------   '---  ------'"
350 Print "   '-------- --'      / /"
360 Print "       ,---' '-------/ /--,"
370 Print "        '----------------'"
410 Z9$="                         "
430 T=Int(Rnd*20+20)*100
    T0=T
    T9=25+Int(Rnd*10)
    D0=0
    E=3000
    E0=E
440 P=10
    P0=P
    S9=200
    S=0
    B9=0
    K9=0
    x4$=""
450 Function FND(d)
    fnd=Sqr((K(I,1)-S1)^2+(K(I,2)-S2)^2)
    End Function
460 Function FNR(R)
    fnr=Int(Rnd*R*7.98+1.01)
    End Function
470 Q1=FNR(1)
    Q2=FNR(1)
    S1=FNR(1)
    S2=FNR(1)
480 For I=1 To 9
    C(I,1)=0
    C(I,2)=0
    Next I
490 C(3,1)=-1
    C(2,1)=-1
    C(4,1)=-1
    C(4,2)=-1
    C(5,2)=-1
    C(6,2)=-1
500 C(1,2)=1
    C(2,2)=1
    C(6,1)=1
    C(7,1)=1
    C(8,1)=1
    C(8,2)=1
    C(9,2)=1
510 For I=1 To 8
    D(I)=0
    Next I
520 A1$="NAVSRSLRSPHATORSHEDAMCOMXXXSTA"
530 For I=1 To 8
    For J=1 To 8
    K3=0
    Z(I,J)=0
    R1=Rnd
540 If R1>.98 Then K3=3:K9=K9+3:GoTo 570
550 If R1>.95 Then K3=2:K9=K9+2:GoTo 570
560 If R1>.80 Then K3=1:K9=K9+1
570 B3=0
    If Rnd>.96 Then B3=1:B9=B9+1
580 G(I,J)=K3*100+B3*10+FNR(1)
    Next J
    Next I
    If K9>T9 Then T9=K9+1
590 If B9<>0 Then GoTo 620
600 If G(Q1,Q2)<200 Then G(Q1,Q2)=G(Q1,Q2)+100:K9=K9+1
610 B9=1
    G(Q1,Q2)=G(Q1,Q2)+10
    Q1=FNR(1)
    Q2=FNR(1)
620 K7=K9
    Print
630 Print "YOUR ORDERS ARE AS FOLLOWS:"
640 Print "--------------------------"
650 Print "DESTROY THE";K9;" KLINGON WARSHIPS WHICH HAVE INVADED"
660 Print "THE GALAXY BEFORE THEY CAN ATTACK FEDERATION"
670 Print "HEADQUARTERS ON STARDATE";T0+T9;". THIS GIVES YOU";T9
680 Print "DAYS. THERE'S ";B9;" STARBASES"
700 Print "IN THE GALAXY FOR RESUPPLYING YOUR SHIP."
710 Print
    Print "PRESS Y TO ACCEPT COMMAND";
720 Input I5$:Print
730 If Left$(I5$,1)="Y" Or Left$(I5$,1)="y" Then GoTo 760
740 GoTo 710
760 Z4=Q1
    Z5=Q2
    K3=0
    B3=0
    S3=0
    G5=0
    D4=.5*Rnd
    Z(Q1,Q2)=G(Q1,Q2)
770 If Q1<1 Or Q1>8 Or Q2<1 Or Q2>8 Then GoTo 890
780 GoSub 4250
    Print
    If T0<>T Then GoTo 810
790 Print "YOUR MISSION BEGINS WITH YOUR STARSHIP LOCATED"
800 Print "IN THE GALACTIC QUADRANT, '";G2$;"'.":GoTo 820
810 Print "NOW ENTERING ";G2$;" QUADRANT . . ."
820 Print
    K3=Int(G(Q1,Q2)*.01)
    B3=Int(G(Q1,Q2)*.1)-10*K3
830 S3=G(Q1,Q2)-100*K3-10*B3
    If K3=0 Then GoTo 880
840 Print Tab(5);
    Color RGB(black), RGB(red)
    Print " COMBAT AREA      CONDITION RED ";
    Color RGB(green), RGB(black)
    Print
850 If S>200 Then Print :GoTo 880
860 Print Tab(5);
    Color RGB(black), RGB(red)
    Print "    SHIELDS DANGEROUSLY LOW     ";
    Color RGB(green), RGB(black)
    Print
870 Print
880 For I=1 To 3
    K(I,1)=0
    K(I,2)=0
    Next I
890 For I=1 To 3
    K(I,3)=0
    Next I
    Q$=Z9$+Z9$+Z9$+Z9$+Z9$+Z9$+Z9$+Left$(Z9$,17)
900 a2$="<E>"
    Z1=S1
    Z2=S2
    GoSub 4080
    If K3<1 Then GoTo 930
910 For I=1 To K3
    GoSub 4050
    a2$="+K+"
    Z1=R1
    Z2=R2
920 GoSub 4080
    K(I,1)=R1
    K(I,2)=R2
    K(I,3)=S9*(.5+Rnd)
    Next I
930 If B3<1 Then GoTo 950
940 GoSub 4050
    a2$=">B<"
    Z1=R1
    B4=R1
    Z2=R2
    B5=R2
    GoSub 4080
950 For I=1 To S3
    GoSub 4050
    a2$=" * "
    Z1=R1
    Z2=R2
    GoSub 4080
    Next I
960 GoSub 3020
970 If S+E>10 Then If E>10 Or D(7)=0 Then GoTo 1030
980 '
     Print
     Print Tab(10);
     Color RGB(black), RGB(red)
     Print  "** FATAL ERROR **";
     Color RGB(green), RGB(black)
1000 Print
1005 Print "YOU'VE JUST STRANDED YOUR SHIP IN SPACE."
1010 Print "YOU HAVE INSUFFICIENT MANEUVERING ENERGY,"
1015 Print "AND SHIELD CONTROL IS PRESENTLY INCAPABLE OF"
1020 Print "CROSS-CIRCUITING TO ENGINE ROOM!!"
     Print
     GoTo 2890
1030 Print
     Input "COMMAND";a2$:Print :a2$=UCase$(a2$)
1040 For I=1 To 10
     If Left$(a2$,3)<>Mid$(A1$,3*I-2,3) Then GoTo 1060
1050 On I GoTo 1170,960,1840,1930,2150,2500,2590,3360,2920,3180
1060 Next I
     Print "ENTER ONE OF THE FOLLOWING:"
1070 Print "--------------------------"
1080 Print "  NAV  (TO SET COURSE)"
1090 Print "  SRS  (FOR SHORT RANGE SENSOR SCAN)"
     Print "  STA  (FOR STATUS)"
1100 Print "  LRS  (FOR LONG RANGE SENSOR SCAN)"
1110 Print "  PHA  (TO FIRE PHASERS)"
1120 Print "  TOR  (TO FIRE PHOTON TORPEDOES)"
1130 Print "  SHE  (TO RAISE OR LOWER SHIELDS)"
1140 Print "  DAM  (FOR DAMAGE CONTROL REPORTS)"
1150 Print "  COM  (TO CALL ON LIBRARY-COMPUTER)"
1160 Print "  XXX  (TO RESIGN YOUR COMMAND)"
     Print
     GoTo 970
1170 Input "COURSE (1-9)";C1:Print
     If c1=9 Then c1=1
1180 If C1>=1 And C1<9 Then GoTo 1200
1190 Print "   LT. SULU: 'INCORRECT COURSE DATA, SIR!'"
     GoTo 970
1200 x4$="8"
     If D(1)<0 Then x4$="0.9"
1210 Print "WARP FACTOR (0-";x4$;")";
     Input W1:Print
1220 If D(1)<0 And W1>.9 Then GoTo 1270
1230 If W1>0 And W1<=8 Then GoTo 1280
1240 If W1=0 Then GoTo 970
1250 Print "CHIEF ENGINEER SCOTT: 'THE ENGINES WON'T TAKE";
1260 Print " WARP";W1;"!'"
     GoTo 970
1270 Color RGB(red)
     Print "WARP ENGINES ARE DAMAGED. ";
     Color RGB(reen)
     Print "MAXIUM SPEED = WARP 0.9"
     GoTo 970
1280 N=Int(W1*8+.5)
     If E-N>=0 Then GoTo 1350
1290 Print "ENGINEERING: 'INSUFFICIENT ENERGY AVAILABLE"
1300 Print "              FOR MANEUVERING AT WARP";W1;"!'"
1310 If S<N-E Or D(7)<0 Then GoTo 970
1320 Print "DEFLECTOR CONTROL ROOM: '";S;" UNITS OF ENERGY"
1330 Print "              PRESENTLY DEPLOYED TO SHIELDS.'"
1340 GoTo 970
1350 For I=1 To K3
     If K(I,3)=0 Then GoTo 1380
1360 a2$="   "
     Z1=K(I,1)
     Z2=K(I,2)
     GoSub 4080
     GoSub 4050
1370 K(I,1)=Z1
     K(I,2)=Z2
     a2$="+K+"
     GoSub 4080
1380 Next I
     GoSub 2750
     D1=0
     D6=W1
     If W1>=1 Then D6=1
1390 For I=1 To 8
     If D(I)>=0 Then GoTo 1440
1400 D(I)=D(I)+D6
     If D(I)>-.1 And D(I)<0 Then D(I)=-.1:GoTo 1440
1410 If D(I)<0 Then GoTo 1440
1420 If D1<>1 Then D1=1:Print "DAMAGE CONTROL REPORT:"
1430 Print Tab(8);
     R1=I
     GoSub 4130
     Print G2$;" REPAIR COMPLETED."
1440 Next I
     If Rnd>.2 Then GoTo 1500
1450 R1=FNR(1)
     If Rnd>=.6 Then GoTo 1480
1460 D(R1)=D(R1)-(Rnd*5+1)
     Print "DAMAGE CONTROL REPORT:"
1470 GoSub 4130
     Color RGB(red)
     Print "    *** ";G2$;" DAMAGED ***"
     Color RGB(green)
     Print
     GoTo 1500
1480 D(R1)=D(R1)+Rnd*3+1
     Print "DAMAGE CONTROL REPORT:"
1490 GoSub 4130
     Print G2$;" STATE OF REPAIR IMPROVED"
     Print
1500 a2$="   "
     Z1=Int(S1)
     Z2=Int(S2)
     GoSub 4080
1510 X1=C(Int(C1),1)+(C(Int(C1)+1,1)-C(Int(C1),1))*(C1-Int(C1))
     X=S1
     Y=S2
1520 X2=C(Int(C1),2)+(C(Int(C1)+1,2)-C(Int(C1),2))*(C1-Int(C1))
     Q4=Q1
     Q5=Q2
1530 For I=1 To N
     S1=S1+X1:S2=S2+X2
1540 If S1<1 Or S1>=9 Or S2<1 Or S2>=9 Then GoTo 1630
1550 S8=Int(S1)*24+Int(S2)*3-26
     If Mid$(Q$,S8,2)="  "Then GoTo 1580
1560 S1=Int(S1-X1)
     S2=Int(S2-X2)
     Color RGB(yellow)
     Print "WARP ENGINES SHUT DOWN AT ";
1570 Print "SECTOR";S1;",";S2
     Print "DUE TO BAD NAVIGATION":Print
     Color RGB(green)
     GoTo 1590
1580 Next I
     S1=Int(S1)
     S2=Int(S2)
1590 a2$="<E>"
     Z1=Int(S1)
     Z2=Int(S2)
     GoSub 4080
     GoSub 1800
     T8=1
1600 If W1<1 Then T8=.1*Int(10*W1)
1610 T=T+T8
     If T>T0+T9 Then GoTo 2890
1620 GoTo 960
1630 X=8*Q1+X+N*X1
     Y=8*Q2+Y+N*X2
     Q1=Int(X/8)
     Q2=Int(Y/8)
     S1=Int(X-Q1*8)
1640 S2=Int(Y-Q2*8)
     If S1=0 Then Q1=Q1-1:S1=8
1650 If S2=0 Then Q2=Q2-1:S2=8
1660 X5=0
     If Q1<1 Then X5=1:Q1=1:S1=1
1670 If Q1>8 Then X5=1:Q1=8:S1=8
1680 If Q2<1 Then X5=1:Q2=1:S2=1
1690 If Q2>8 Then X5=1:Q2=8:S2=8
1700 If X5=0 Then GoTo 1780
1710 Print "LT. UHURA: MESSAGE FROM STARFLEET COMMAND --"
1720 Print "'PERMISSION TO ATTEMPT CROSSING OF GALACTIC PERIMETER"
1730 Print "IS HEREBY *DENIED*. SHUT DOWN YOUR ENGINES.'"
1740 Print "CHIEF ENGINEER SCOTT: 'WARP ENGINES SHUT DOWN"
1750 Print "AT SECTOR";S1;",";S2;" OF QUADRANT";
1760 Print Q1;",";Q2;".'"
1770 If T>T0+T9 Then GoTo 2890
1780 If 8*Q1+Q2=8*Q4+Q5 Then GoTo 1590
1790 T=T+1
     GoSub 1800
     GoTo 760
1800 E=E-N-10
     If E>=0 Then Return
1810 Print "SHIELD CONTROL SUPPLIES ENERGY TO COMPLETE THE MANEUVER."
1820 S=S+E
     E=0
     If S<=0 Then S=0
1830 Return
1840 If D(3)<0 Then Colour RGB(red):Print "LONG RANGE SENSORS ARE INOPERABLE.":Colour RGB(green):GoTo 970
1850 Print "LONG RANGE SCAN FOR QUADRANT";Q1;",";Q2
     Print
1860 O1$="   +-----+-----+-----+"
     Print O1$
1870 For I=Q1-1 To Q1+1
     N1(1)=-1
     N1(2)=-2
     N1(3)=-3
     For J=Q2-1 To Q2+1
1880 If I>0 And I<9 And J>0 And J<9 Then N1(J-Q2+2)=G(I,J):Z(I,J)=G(I,J)
1890 Next J
     Print Tab(4);
     For L=1 To 3
     Print "| ";
1900 If N1(L)<0 Then Print "*** ";:GoTo 1920
     If N1(L)>=10 Then Color RGB(cyan)
     If N1(L)>=100 Then Color RGB(red)
     If L=2 And I=Q1 Then Color RGB(white)
1910 Print Right$(Str$(N1(L)+1000),3);" ";
     Color RGB(green)
1920 Next L
     Print "|"
     Print O1$
     Next I
     GoTo 970
1930 If D(4)<0 Then Colour RGB(red):Print "PHASERS INOPERATIVE.":Colour RGB(green):GoTo 970
1940 If K3>0 Then GoTo 1970
1950 Print "SCIENCE OFFICER SPOCK: 'SENSORS SHOW NO ENEMY SHIPS"
1960 Print "                        IN THIS QUADRANT'":GoTo 970
1970 If D(8)<0 Then Color RGB(yellow):Print "COMPUTER FAILURE HAMPERS ACCURACY."
     Color RGB(green)
1980 Print "PHASERS LOCKED ON TARGET."
1990 Print "ENERGY AVAILABLE =";E;" UNITS"
2000 Input "NUMBER OF UNITS TO FIRE";X:Print
     If X<=0 Then GoTo 970
2010 If E-X<0 Then GoTo 1990
2020 E=E-X
     If D(7)<0 Then X=X*Rnd
2030 H1=Int(X/K3)
     For I=1 To 3
  If K(I,3)<=0 Then GoTo 2140
2040 H=Int((H1/FND(0))*(Rnd+2))
     If H>.15*K(I,3)Then GoTo 2070
2050 Print "SENSORS SHOW NO DAMAGE TO ENEMY AT";K(I,1);
2060 Print ",";K(I,2);"."
     GoTo 2140
2070 K(I,3)=K(I,3)-H
     Print H;" UNIT HIT ON KLINGON AT SECTOR";
2080 Print K(I,1);",";
2090 Print K(I,2);"."
     If K(I,3)<=0 Then Print
2100 Color RGB(yellow)
     Print "       *** KLINGON DESTROYED ***";
     Color RGB(green)
     Print
     GoTo 2120
2110 Print " (SENSORS SHOW";K(I,3);" UNITS REMAINING)"
     GoTo 2140
2120 K3=K3-1
     K9=K9-1
     Z1=K(I,1)
     Z2=K(I,2)
     a2$="   "
     GoSub 4080
2130 K(I,3)=0
     G(Q1,Q2)=G(Q1,Q2)-100
     Z(Q1,Q2)=G(Q1,Q2)
     If K9<=0 Then GoTo 2990
2140 Next I
     GoSub 2750
     GoTo 970
2150 If P<=0 Then Color RGB(yellow):Print "ALL PHOTON TORPEDOES EXPENDED.":Color RGB(green):GoTo 970
2160 If D(5)<0 Then Color RGB(red):Print "PHOTON TUBES ARE NOT OPERATIONAL.":Color RGB(green):GoTo 970
2170 Input "PHOTON TORPEDO COURSE (1-9)";C1:Print
     If c1=9 Then c1=1
2180 If C1>=1 And C1<9 Then GoTo 2210
2190 Print "ENSIGN CHEKOV: 'INCORRECT COURSE DATA, SIR!'"
2200 GoTo 970
2210 X1=C(Int(C1),1)+(C(Int(C1)+1,1)-C(Int(C1),1))*(C1-Int(C1))
     E=E-2
     P=P-1
2220 X2=C(Int(C1),2)+(C(Int(C1)+1,2)-C(Int(C1),2))*(C1-Int(C1))
     X=S1
     Y=S2
2230 Print "TORPEDO TRACK:"
2240 X=X+X1
     Y=Y+X2
     X3=Int(X+.5)
     Y3=Int(Y+.5)
2250 If X3<1 Or X3>8 Or Y3<1 Or Y3>8 Then GoTo 2490
2260 Print "               ";X3;",";Y3
     a2$="   "
     Z1=X
     Z2=Y
2270 GoSub 4220
2280 If Z3<>0 Then GoTo 2240
2290 a2$="+K+"
     Z1=X
     Z2=Y
     GoSub 4220
     If Z3=0 Then GoTo 2350
2300 Print
     Color RGB(yellow)
     Print "       *** KLINGON DESTROYED ***";
     Color RGB(green)
     Print
2310 Print
     K3=K3-1
     K9=K9-1
     If K9<=0 Then GoTo 2990
2320 For I=1 To 3
     If X3=K(I,1)And Y3=K(I,2)Then GoTo 2340
2330 Next I
     I=3
2340 K(I,3)=0
     GoTo 2470
2350 a2$=" * "
     Z1=X
     Z2=Y
     GoSub 4220
     If Z3=0 Then GoTo 2380
2360 Print "STAR AT";X3;",";Y3;" ABSORBED TORPEDO ENERGY."
     GoSub 2750
2370 GoTo 970
2380 a2$=">!<"
     Z1=X
     Z2=Y
     GoSub 4220
     If Z3=0 Then GoTo 2170
2390 Color RGB(red)
     Print "    *** STARBASE DESTROYED ***";
     Color RGB(green)
     Print
2400 B3=B3-1
     B9=B9-1
2410 If B9>0 Or K9>T-T0-T9 Then GoTo 2450
2420 Print "THAT DOES IT, CAPTAIN! YOU ARE HEREBY RELIEVED OF COMMAND"
2430 Print "AND SENTENCED TO 99 STARDATES AT HARD LABOR ON CYGNUS 12!"
2440 GoTo 2920
2450 Print "STARFLEET COMMAND REVIEWING YOUR RECORD TO CONSIDER"
2460 Print "COURT MARTIAL!"
     D0=0
2470 Z1=X
     Z2=Y
     a2$="   "
     GoSub 4080
2480 G(Q1,Q2)=K3*100+B3*10+S3
     Z(Q1,Q2)=G(Q1,Q2)
     GoSub 2750
     GoTo 970
2490 Print "   TORPEDO MISSED."
     Print
     GoSub 2750
     GoTo 970
2500 If D(7)<0 Then Color RGB(red):Print "SHIELD CONTROL INOPERABLE.":Color RGB(green):GoTo 970
2510 Print "ENERGY AVAILABLE =";E+S
     Input "NUMBER OF UNITS TO SHIELDS";X:Print
2520 If X<0 Or S=X Then Print "<SHIELDS UNCHANGED>":GoTo 970
2530 If X<=E+S Then GoTo 2560
2540 Print "SHIELD CONTROL: 'THIS IS NOT THE FEDERATION TREASURY.'"
2550 Print "<SHIELDS UNCHANGED>"
     GoTo 970
2560 E=E+S-X
     S=X
     Print "DEFLECTOR CONTROL ROOM:"
2570 Print "  'SHIELDS NOW AT";Int(S);" UNITS PER YOUR COMMAND.'"
2580 GoTo 970
2590 If D(6)>=0 Then GoTo 2700
2600 Color RGB(red)
     Print " DAMAGE CONTROL REPORT NOT AVAILABLE."
     Color RGB(green)
     If D0=0 Then GoTo 970
2610 D3=0
     For I=1 To 8
     If D(I)<0 Then D3=D3+.1
2620 Next I
     If D3=0 Then GoTo 970
2630 Print
     D3=D3+D4
     If D3>=1 Then D3=.9
2640 Print "TECHNICIANS STANDING BY TO EFFECT REPAIRS TO YOUR"
2650 Print "SHIP. ESTIMATED TIME TO REPAIR:";.01*Int(100*D3);" STARDATES."
2660 Input "WILL YOU AUTHORIZE THE REPAIR ORDER (Y/N)";a2$:Print :a2$=UCase$(a2$)
2670 If a2$<>"Y"Then GoTo 970
2680 For I=1 To 8
     If D(I)<0 Then D(I)=0
2690 Next I
     T=T+D3+.1
2700 Print
     Print " DEVICE             STATE OF REPAIR"
2710 Print " ------             ---------------"
     For R1=1 To 8
2720 GoSub 4130
     If D(R1)<0 Then Color RGB(red)
     Print " ";G2$;Left$(Z9$,25-Len(G2$));Int(D(R1)*100)*.01
     Color RGB(green)
2730 Next R1
     Print
     If D0<>0 Then GoTo 2610
2740 GoTo 970
2750 If K3<=0 Then Return
2760 If D0<>0 Then Print "STARBASE SHIELDS PROTECT THE ENTERPRISE." : Return
2780 For I=1 To K3
     If K(I,3)<=0 Then GoTo 2880
2790 H=Int((K(I,3)/FND(1))*(2+Rnd))
2800 S=S-H
     K(I,3)=K(I,3)/(3+Rnd(0))
2810 Colour RGB(red)
     Print
     Print H;" UNIT HIT ON ENTERPRISE FROM SECTOR";
2820 Print K(I,1);",";K(I,2);"."
     Colour RGB(green)
2830 If S<=0 Then GoTo 2900
2840 Print " <SHIELDS DOWN TO";S;" UNITS>":Print
     If H<20 Then GoTo 2880
2850 If Rnd>.6 Or H/S<=.02 Then GoTo 2880
2860 R1=FNR(1):D(R1)=D(R1)-H/S-.5*Rnd:GoSub 4130
2870 Print "DAMAGE CONTROL:"
     Color RGB(red)
     Print "   *** ";G2$;" DAMAGED BY THE HIT ***"
     Color RGB(green)
2880 Next I
     Print
     Return
2890 Print :Print "IT IS STARDATE";T;"."
     Print
     GoTo 2920
2900 Print :Print "THE ENTERPRISE HAS BEEN DESTROYED. THE FEDERATION ";
2910 Print "WILL BE CONQUERED."
     GoTo 2890
2920 Print "THERE WERE";K9;" KLINGON BATTLE CRUISERS LEFT AT"
2930 Print "THE END OF YOUR MISSION."
2940 Print
     Print
     If B9=0 Then GoTo 2980
2950 Print "THE FEDERATION IS IN NEED OF A NEW STARSHIP COMMANDER"
2960 Print "FOR A SIMILAR MISSION -- IF THERE IS A VOLUNTEER,"
2970 Input "LET HIM STEP FORWARD AND ENTER 'AYE'";a2$:Print
     If a2$="AYE" Then GoTo 290
2980 Print
     Print "BACK TO SYSTEM."
     End
2990 Print :Print "CONGRATULATIONS, CAPTAIN! THE LAST KLINGON BATTLE"
3000 Print "CRUISER MENACING THE FEDERATION HAS BEEN DESTROYED."
     Print
3010 Print "YOUR EFFICIENCY RATING IS";1000*(K7/(T-T0))^2
     GoTo 2940
3020 For I=S1-1 To S1+1
     For J=S2-1 To S2+1
3030 If Int(I+.5)<1 Or Int(I+.5)>8 Then GoTo 3060
3040 If Int(J+.5)<1 Or Int(J+.5)>8 Then GoTo 3060
3050 a2$=">B<"
     Z1=I
     Z2=J
     GoSub 4220
     If Z3=1 Then GoTo 3070
3060 Next J
     Next I
     D0=0
     GoTo 3090
3070 D0=1
     c4$="DOCKED"
     E=E0
     P=P0
3080 Print "SHIELDS DROPPED FOR DOCKING PURPOSES."
     S=0:Print
     GoTo 3110
3090 If K3>0 Then c4$="*RED*":GoTo 3110
3100 c4$="GREEN"
     If E<E0*.1 Then c4$="YELLOW"
3110 If D(2)>=0 Then GoTo 3130
3120 Color RGB(red)
     Print
     Print "    *** SHORT RANGE SENSORS ARE OUT ***"
     Print
     Color RGB(green)
     Return
3130 O1$="   +--1---2---3---4---5---6---7---8-+"
     Print O1$
3140 For I=1 To 8
     Print I;" |";
3150 For J=(I-1)*24+1 To (I-1)*24+22 Step 3
     If Mid$(Q$,J,3)=" * " Then Color RGB(gold)
     If Mid$(Q$,J,3)="<E>" Then Color RGB(white)
     If Mid$(Q$,J,3)="+K+" Then Color RGB(red)
     If Mid$(Q$,J,3)=">B<" Then Color RGB(cyan)
     Print " ";Mid$(Q$,J,3);
     Color RGB(green)
3160 Next J
     Print "|";I
     Next I
     Print O1$
     Return
3180 Print "STARDATE          ";Int(T*10)*.1
3200 Print "CONDITION          ";
     Select Case C4$
     Case "GREEN"
       Color RGB(lightgrey)
     Case "*RED*"
       Color RGB(BLACK),RGB(red)
     Case "YELLOW"
       Color RGB(yellow)
     Case "DOCKED"
       Color RGB(cyan)
     End Select
     Print C4$;: Color RGB(green),RGB(black):Print
3240 Print "QUADRANT          ";Q1;",";Q2
3260 Print "SECTOR            ";S1;",";S2
3280 Print "PHOTON TORPEDOES  ";:Print Int(P)
3300 Print "TOTAL ENERGY      ";:Print Int(E+S)
3320 Print "SHIELDS           ";:Print Int(S)
3340 Print "KLINGONS REMAINING";:Print Int(K9)
     GoTo 970
3360 If D(8)<0 Then Print "COMPUTER DISABLED.":GoTo 970
3370 Input "COMPUTER ACTIVE AND AWAITING COMMAND";A:Print
     If A<0 Then GoTo 970
3380 Print
     H8=1
     On A+1 GoTo 3480,3630,3740,4010,3800,3470
3390 Print "FUNCTIONS AVAILABLE FROM LIBRARY-COMPUTER:"
3400 Print "-----------------------------------------"
     Print
3410 Print "   0 = CUMULATIVE GALTIC RECORD"
3420 Print "   1 = STATUS REPORT"
3430 Print "   2 = PHOTON TORPEDO DATA"
3440 Print "   3 = STARBASE NAV DATA"
3450 Print "   4 = DIRECTION/DISTANCE CALCULATOR"
3460 Print "   5 = GALAXY 'REGION NAME' MAP"
     Print
     GoTo 3370
3470 H8=0
     G5=1
     Print "                        THE GALAXY"
     GoTo 3530
3480 '
3490 '
3500 Print "      ";
3510 Print "COMPUTER RECORD OF GALAXY FOR QUADRANT";Q1;",";Q2
3520 Print
3530 Print "      1     2     3     4     5     6     7     8"
3540 O1$="   +-----+-----+-----+-----+-----+-----+-----+-----+"
3550 Print O1$
     For I=1 To 8
     Print I;" ";
     If H8=0 Then GoTo 3600
3560 For J=1 To 8
     Print "| ";
     If Z(I,J)=0 Then Print "*** ";:GoTo 3580
     If Z(i,j)>=10 Then Color RGB(cyan)
     If Z(i,j)>=100 Then Color RGB(red)
     If (i=q1 And j=q2) Then Color RGB(white)
3570 Print Right$(Str$(Z(I,J)+1000),3);" ";
     Color RGB(green)
3580 If J=8 Then Print "|"
3590 Next J
     GoTo 3620
3600 Z4=I
     Z5=1
     GoSub 4250
     J0=Int(15-.5*Len(G2$))
     Print Tab(J0);G2$;
3610 Z5=5
     GoSub 4250
     J0=Int(39-.5*Len(G2$))
     Print Tab(J0);G2$
3620 Print O1$
     Next I
     Print
     GoTo 970
3630 Print " STATUS REPORT:"
     Print " -------------"
     x4$=""
3640 If K9>1 Then x4$="S"
3650 Print K9;" KLINGON";x4$;" LEFT."
3660 Print " MISSION MUST BE COMPLETED IN";.1*Int((T0+T9-T)*10);
3670 Print " STARDATES."
3680 If B9<1 Then GoTo 3720
3690 Print " THE FEDERATION IS MAINTAINING";B9;" STARBASES"
3700 Print " IN THE GALAXY."
3710 GoTo 2590
3720 Print "YOUR STUPIDITY HAS LEFT YOU ON YOUR OWN IN"
3730 Print "THE GALAXY -- YOU HAVE NO STARBASES LEFT!"
     GoTo 2590
3740 If K3<=0 Then GoTo 1950
3750 x4$=""
     If K3>1 Then x4$="S"
3760 Print "FROM ENTERPRISE TO KLINGON BATTLE CRUSER";x4$
3770 H8=0
     For I=1 To 3:If K(I,3)<=0 Then GoTo 4000
3780 W1=K(I,1)
     X=K(I,2)
3790 C1=S1
     A=S2
     GoTo 3850
3800 Print "DIRECTION/DISTANCE CALCULATOR:"
3810 Print "YOU ARE AT QUADRANT ";Q1;",";Q2;" SECTOR ";
3820 Print S1;",";S2;"."
3830 Input "PLEASE ENTER INITIAL COORDINATES (Y,X)";C1,A
3840 Input "FINAL COORDINATES (Y,X)";W1,X:Print
3850 X=X-A
     A=C1-W1
     If X<0 Then GoTo 3930
3860 If A<0 Then GoTo 3950
3870 If X>0 Then GoTo 3890
3880 If A=0 Then C1=5:GoTo 3900
3890 C1=1
3900 If Abs(A)<=Abs(X)Then GoTo 3920
3910 Print "DIRECTION =";C1+(((Abs(A)-Abs(X))+Abs(A))/Abs(A))
     GoTo 3990
3920 Print "DIRECTION =";C1+(Abs(A)/Abs(X))
     GoTo 3990
3930 If A>0 Then C1=3:GoTo 3960
3940 If X<>0 Then C1=5:GoTo 3900
3950 C1=7
3960 If Abs(A)>=Abs(X)Then GoTo 3980
3970 Print "DIRECTION =";C1+(((Abs(X)-Abs(A))+Abs(X))/Abs(X))
     GoTo 3990
3980 Print "DIRECTION =";C1+(Abs(X)/Abs(A))
3990 Print "DISTANCE =";Sqr(X^2+A^2)
     If H8=1 Then GoTo 970
4000 Next I
     GoTo 970
4010 If B3<>0 Then Print "FROM ENTERPRISE TO STARBASE:"
4020 W1=B4
     X=B5
     GoTo 3790
4030 Print "MR. SPOCK: 'SENSORS SHOW NO STARBASES IN THIS QUADRANT.'";
4040 GoTo 970
4050 R1=FNR(1)
     R2=FNR(1)
     a2$="   "
     Z1=R1
     Z2=R2
     GoSub 4220
4060 If Z3=0 Then GoTo 4050
4070 Return
4080 S8=Int(Z2-.5)*3+Int(Z1-.5)*24+1
4090 If Len(a2$)<>3 Then Print "ERROR":End
4100 If S8=1 Then Q$=a2$+Right$(Q$,189):Return
4110 If S8=190 Then Q$=Left$(Q$,189)+a2$:Return
4120 Q$=Left$(Q$,S8-1)+a2$+Right$(Q$,190-S8):Return
4130 On R1 GoTo 4140,4150,4160,4170,4180,4190,4200,4210
4140 G2$="WARP ENGINES":Return
4150 G2$="SHORT RANGE SENSORS":Return
4160 G2$="LONG RANGE SENSORS":Return
4170 G2$="PHASER CONTROL":Return
4180 G2$="PHOTON TUBES":Return
4190 G2$="DAMAGE CONTROL":Return
4200 G2$="SHIELD CONTROL":Return
4210 G2$="LIBRARY-COMPUTER":Return
4220 Z1=Int(Z1+.5)
     Z2=Int(Z2+.5)
     S8=(Z2-1)*3+(Z1-1)*24+1
     Z3=0
4230 If Mid$(Q$,S8,3)<>a2$ Then Return
4240 Z3=1
     Return
4250 If Z5<=4 Then On Z4 GoTo 4270,4280,4290,4300,4310,4320,4330,4340
4260 GoTo 4350
4270 G2$="ANTARES":GoTo 4440
4280 G2$="RIGEL":GoTo 4440
4290 G2$="PROCYON":GoTo 4440
4300 G2$="VEGA":GoTo 4440
4310 G2$="CANOPUS":GoTo 4440
4320 G2$="ALTAIR":GoTo 4440
4330 G2$="SAGITTARIUS":GoTo 4440
4340 G2$="POLLUX":GoTo 4440
4350 On Z4 GoTo 4360,4370,4380,4390,4400,4410,4420,4430
4360 G2$="SIRIUS":GoTo 4440
4370 G2$="DENEB":GoTo 4440
4380 G2$="CAPELLA":GoTo 4440
4390 G2$="BETELGEUSE":GoTo 4440
4400 G2$="ALDEBARAN":GoTo 4440
4410 G2$="REGULUS":GoTo 4440
4420 G2$="ARCTURUS":GoTo 4440
4430 G2$="SPICA"
4440 If G5<>1 Then On Z5 GoTo 4460,4470,4480,4490,4460,4470,4480,4490
4450 Return
4460 G2$=G2$+" I":Return
4470 G2$=G2$+" II":Return
4480 G2$=G2$+" III":Return
4490 G2$=G2$+" IV":Return
4500 End
