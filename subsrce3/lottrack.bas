        REM *************************************************************~
            *                                                           *~
            *  L       OOO   TTTTT  TTTTT  RRRR     A     CCC   K   K   *~
            *  L      O   O    T      T    R   R   A A   C   C  K  K    *~
            *  L      O   O    T      T    RRRR   AAAAA  C      KKK     *~
            *  L      O   O    T      T    R   R  A   A  C   C  K  K    *~
            *  LLLLL   OOO     T      T    R   R  A   A   CCC   K   K   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * LOTTRACK - Writes a record into the LOTMVMNT file.        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/19/83 ! ORIGINAL                                 ! HES *~
            * 02/??/87 ! Many changes to support expanded lot trac! KAB *~
            * 05/06/87 ! Bug fix - mismatch between write & format! LDJ *~
            *          !   statement on line 10280.               !     *~
            *************************************************************

             sub "LOTTRACK"                                              ~
                 (fflag$,          /* FROM FLAG  H=INVNTORY,V=VEN,C=CUS*/~
                  fpart$,          /* PART BEING MOVED                 */~
                  flot$,           /* FROM LOT                         */~
                  fstore$,         /* FROM STORE                       */~
                  fmisc$,          /* FROM WHATEVER                    */~
                  tflag$,          /* TO FLAG H=INVNTORY, V=VEN, C=CUS */~
                  tpart$,          /* TO PART IF BLANK THEN SET TO FROM*/~
                  tlot$,           /* TO LOT                           */~
                  tstore$,         /* TO STORE                         */~
                  tmisc$,          /* TO WHATEVER                      */~
                  quantity,        /* QUANTITY MOVED                   */~
                  #1,              /* 'HNYMASTR' FILE                  */~
                  #2)              /* 'SYSFILE2' FILE                  */

        dim                                                              ~
            fflag$1,                     /* FROM TYPE H=HNY,V=VEN,C=CUS*/~
            flot$6,                      /* LOT MOVING FROM            */~
            fmisc$9,                     /* FROM WHATEVER              */~
            fpart$25,                    /* PART BEING MOVED           */~
            fstore$3,                    /* STORE MOVING FROM          */~
            tflag$1,                     /* TO TYPE (H=HNY,V=VEN,C=CUS)*/~
            tlot$6,                      /* LOT MOVING TO              */~
            tmisc$9,                     /* TO WHATEVER                */~
            tpart$25,                    /* NEW PART (USUALLY SAME AS F*/~
            tstore$3,                    /* STORE MOVING TO            */~
            readkey$160,                 /* KEY FOR READ               */~
            sysflags$(2)1                /* SYSTEM FLAGS               */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L01462
            cms2v$ = "04.19.03 05/29/87 Patch Release                   "
L01462: REM *************************************************************

            if sysflags$() = " " then str(sysflags$()) = "XX"

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * #2  ! SYSFILE2 ! SYSTEM CONTROL FILE                      *~
            * #3  ! LOTMVMNT ! LOT MOVEMENT FILE                        *~
            * #4  ! LOTMVDTL ! LOT MOVEMENT DETAILS                     *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #3,  "LOTMVMNT",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  160,                                  ~
                        keypos = 1,    keylen = 88,                      ~
                        alt key  1, keypos =  45, keylen =  88

            select #4,  "LOTMVDTL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  200,                                  ~
                        keypos = 1,    keylen = 96,                      ~
                        alt key  1, keypos =  97, keylen =  96

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            if quantity = 0 then L65000

            if sysfile2% <> 0% then L09100
            call "OPENCHCK" (#2, sysfile2%, 0%, 0%, " ")
L09100:        if sysfile2% < 0% then L65000

            if sysflags$(1) <> "X" then L09180
               call "READ100" (#2, "SWITCHS.HNY", f1%)
                  if f1% = 0% then L65000
               get #2, using L09160, sysflags$(1), sysflags$(2)
L09160:            FMT POS(92), CH(1), XX(1), CH(1)

L09180:     if sysflags$(1) <> "Y" then L65000

            if hnymastr% <> 0% then L09220
            call "OPENCHCK" (#1, hnymastr%, 0%, 0%, " ")
L09220:        if hnymastr% < 0% then L65000

            if tpart$ = " " then tpart$ = fpart$

            if fflag$ <> "H" then L09270
               if flot$ <> " " then L09450 else L65000
L09270:     if tflag$ <> "H" then L09300
               if tlot$ <> " " then L09450 else L65000

L09300:     if pos("HX" = fflag$) <> 0% then L09370
            if pos("HX" = tflag$) <> 0% then L09390
            if fflag$ <> "J" or tflag$ <> "J" then L65000
               temp1$ = str(flot$,,6) & str(fstore$,,3)
               temp2$ = str(tlot$,,6) & str(tstore$,,3)
                   if temp1$ = temp2$ then L09390 else L09370

L09370:     readkey$ = fpart$:call "READ100" (#1, readkey$, f1%)
               goto L09400
L09390:     readkey$ = tpart$:call "READ100" (#1, readkey$, f1%)
L09400:        if f1% = 0% then L65000
            get #1, using L09420, temp$
L09420:         FMT POS(130), CH(1)
            if temp$ <> "Y" then L65000

L09450:     if lottrack% <> 0% then L09470
            call "OPENCHCK" (#3, lottrack%, 0%, 1%, " ")
L09470:        if lottrack% < 0% then L65000

        REM *************************************************************~
            *            S A V E   S T U F F   P A S S E D              *~
            *                                                           *~
            * SAVES THE RECORD IF NOT ALL READY THERE.                  *~
            *************************************************************

            put readkey$, using L35040,                                   ~
                                fflag$, fpart$, flot$, fstore$, fmisc$,  ~
                                tflag$, tpart$, tlot$, tstore$, tmisc$,  ~
                                fflag$, fpart$, flot$, fstore$, fmisc$
            moved1, moved2 = 0:temp$ = " "

            call "READ101" (#3, str(readkey$,1,88), forward%)
               if forward% = 0% then L10180
                   get #3, using L10150, moved1, temp$
L10150:                FMT POS(133), PD(14,4), CH(6)
                   delete #3

L10180:     call "READ101" (#3, str(readkey$,45,88), backward%)
               if backward% = 0% then L10230
                   get #3, using L10150, moved2, temp$
                   delete #3

L10230:     moved = quantity + moved1 - moved2

            if moved = 0 then L65000
            if moved < 0 then reverse_it

            write #3, using L10290, str(readkey$,1,132), moved,           ~
                                                      temp$, date, " "
L10290:           FMT CH(132), PD(14,4), 2*CH(6), CH(8)
            goto L11000

        reverse_it
            write #3, using L10350, str(readkey$,45,88),                  ~
                      str(readkey$,45,44), -moved, temp$, date, " "
L10350:           FMT CH(88), CH(44), PD(14,4), 2*CH(6), CH(8)
            goto L11000

L11000: REM *************************************************************~
            *            S A V E   S T U F F   P A S S E D              *~
            *                                                           *~
            * SAVES THE DETAIL IF REQUESTED.                            *~
            *************************************************************

            if sysflags$(2) <> "D" then L65000

            if lotdetail% <> 0% then L11100
            call "OPENCHCK" (#4, lotdetail%, 0%, 1%, " ")
L11100:        if lotdetail% < 0% then L65000

            init(hex(00)) temp$

L11140:     call "GETDTTM" addr(str(temp$,2,7))

            write #4, using L11200, str(readkey$, 1,88), str(temp$,1,8),  ~
                                   str(readkey$,45,88), str(temp$,1,8),  ~
                                   quantity, eod goto L11140

L11200:           FMT CH(88), CH(8), CH(88), CH(8), PD(14,4)

            goto L65000

        REM *************************************************************~
            *  FORMAT OF KEYS                                           *~
            *************************************************************

L35040:     FMT                    /* 'LOTMVMNT'  LOT MOVEMENT FILE    */~
                  CH(1),           /* FROM FLAG  H=INVNTORY,V=VEN,C=CUS*/~
                  CH(25),          /* PART BEING MOVED                 */~
                  CH(6),           /* FROM LOT                         */~
                  CH(3),           /* FROM STORE                       */~
                  CH(9),           /* FROM MISC.                       */~
                  CH(1),           /* TO FLAG H=INVNTORY, V=VEN, C=CUS */~
                  CH(25),          /* TO PART                          */~
                  CH(6),           /* TO LOT                           */~
                  CH(3),           /* TO STORE                         */~
                  CH(9),           /* TO MISC.                         */~
                  CH(1),           /* FROM FLAG  H=INVNTORY,V=VEN,C=CUS*/~
                  CH(25),          /* PART BEING MOVED                 */~
                  CH(6),           /* FROM LOT                         */~
                  CH(3),           /* FROM STORE                       */~
                  CH(9)            /* FROM MISC.                       */~

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            *************************************************************

            end
