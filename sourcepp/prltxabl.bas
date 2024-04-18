        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   RRRR   L      TTTTT  X   X   AAA   BBBB   L       *~
            *  P   P  R   R  L        T     X X   A   A  B   B  L       *~
            *  PPPP   RRRR   L        T      X    AAAAA  BBBB   L       *~
            *  P      R   R  L        T     X X   A   A  B   B  L       *~
            *  P      R   R  LLLLL    T    X   X  A   A  BBBB   LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLTXABL - BUILDS A TABLE THAT IS USED TO DETERMINE WHAT  *~
            *            EARNINGS ARE SUBJECT TO WHAT DEDUCTIONS.       *~
            *            PRIMARILY FOR TAX DEDUCTION, IE DEDUCTIONS THAT*~
            *            ARE CALCULATED FROM AN ADJUSTED GROSS PAY OR   *~
            *            HOURS.                                         *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/07/83 ! ORIGINAL                                 ! HES *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**


        dim                                                              ~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            deduc$(80)6,                 /* METHOD OF DEDUCTION        */~
            descr$30,                    /* DEDUCTION DESCRIPTION      */~
            descr$(80)30,                /* DEDUCTION DESCRIPTION      */~
            earntype$12,                 /* ENTER EARNINGS TYPE        */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            fac$(80)1,                   /* FAC'S FOR TABULAR INPUT    */~
            flag$(80)1,                  /* TABULAR INPUT VARIABLE     */~
            line2$79,                    /* Screen line #2             */~
            infomsg$79,                  /* INFO  MESSAGE              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            temp$(80)6,                  /* METHOD OF DEDUCTION        */~
            title$(2)64                  /* TABULAR PF KEY TITLES      */~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "

            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! PRLDDT   ! input & list payroll deduc def table     *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            select #02, "PRLDDT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  400,                                  ~
                        keypos =    1, keylen =   6                      ~


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("LINKING TO DATA BASE TO FLAG EARNINGS AS TAX~
        ~ EXEMPT")

            call "OPENFILE" (#01, "SHARE", f2%(01), rslt$(01), axd$(01))
            call "OPENFILE" (#02, "SHARE", f2%(02), rslt$(02), axd$(02))
            if f2%(1) + f2%(2) <> 0 then L65000

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

            title$(1) =   "(ENTER)Next Earntype  (4)Previous     (13)Inst~
        ~ructions  "
            title$(2) =   "(1)Start Over         (5)Next  (15)Prnt Scrn (~
        ~16)Save It"

            str(line2$,62) = "PRLTXABL: " & str(cms2v$,,8)

        REM *************************************************************~
            *                  I N P U T   H E A D E R                  *~
            *                                                           *~
            * INPUTS JOURNAL ENTRY MNEMONIC AND ENTRY DESCRIPTION.      *~
            *************************************************************

        inputmode
            init(" ") flag$(), deduc$(), errormsg$, infomsg$

            for fieldnr% = 1 to 1
L10110:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then L65000
                      if keyhit% <>  0 then       L10110
                gosub'150(fieldnr%)
                      if errormsg$ <> " " then L10110
                next fieldnr%

        REM *************************************************************~
            *               E D I T   L I N E   I T E M S               *~
            *                                                           *~
            * EDITS LINE ITEMS, GOES AND SAVES DATA TO THE FILE, ETC.   *~
            *************************************************************

            line% = 0

L11090:     gosub'213(0%)
                  if keyhit%  =  0 then       inputmode
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  4 then       line% = 0
                  if keyhit%  =  5 and maxlines% > 40 then line% = 40
                  if keyhit%  = 16 then       datasave
                  goto L11090

                goto L11090

        REM *************************************************************~
            *                    S A V E   D A T A                      *~
            *                                                           *~
            * WRITES USERS INPUT TO DISK.                               *~
            *************************************************************

        datasave

            REM OUT WITH THE OLD....
                gosub L31000   /* AND IN WITH THE NEW */
                gosub L30000   /* SHOW'EM WHAT THEY GOT NOW */
                goto inputmode

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *              L O A D   U P  T H E   D A T A               *~
            *                                                           *~
            * SEARCHES SYSFILE2 FOR PREVIOUS ENTRY, THEN GETS ALL OTHER *~
            * DEDUCTIONS, SETTING FLAG TO BLANK FOR THOSE NOT PREVIOUSLY*~
            * SPECIFIED AS BEING EXEMPT.                                *~
            *************************************************************

            REM SEE IF ALLREADY IN SYSFILE2.
                deduc$(), ddt$ = " "
                call "READ100" (#1, "PRLTXABL" & earntype$, f1%(1))
                      if f1%(1) = 0 then L30150
                get #1, using L30130 , earntype$, deduc$()
L30130:                 FMT XX(08), CH(12), 80*CH(6)
                init ("N") flag$()
L30150:         search deduc$() = "      " to cursor%() step 6
                if cursor%(1) = 0 then set_facs  /* FULL */
                u3% = (cursor%(1) + 5)/6
                str(flag$(), u3%) = " "

            REM NOW SCAN DDT FILE AND SET THE FLAG IF IN BOTH PLACES
                call "SHOSTAT" ("Scanning Deductions...")

            REM PLOW ROUTINE TO LOAD DATA FROM LINE ITEMS.
L30240:         call "READ102" (#2, ddt$, f1%(2%))
                     if f1%(2) = 0 then set_facs
                get #2, using L30270 , ddt$, exemptflag$, descr$
L30270:            FMT CH(6), XX(145), CH(1), XX(198), CH(30) /* MTHD */
                if exemptflag$ <> "Y" then L30240
                search deduc$() = str(ddt$,,6) to cursor%() step 6
                if cursor%(1) = 0 then L30330
                descr$( (cursor%(1)+5)/6 ) = descr$
                goto L30240
L30330:         deduc$(u3%) = ddt$
                descr$(u3%) = descr$
                u3% = u3% + 1
                if u3% > 80 then set_facs
                goto L30240

        set_facs
                maxlines% = u3%
                init (hex(81)) fac$()
                str(fac$(), u3%) =  all(hex(8c))
        return

L31000: REM *************************************************************~
            *            W R I T E   D A T A   T O   F I L E            *~
            *                                                           *~
            * WRITE DATA TO FILE, AFTER HAVING OLD DELETED BY L. 19000  *~
            *************************************************************

            REM SEE IF ALLREADY IN SYSFILE2.
                t% = 0
                temp$() = " "

                  for u3% = 1 to maxlines%
                     if flag$(u3%) <> "N" then L31090
                        t% = t% + 1
                        temp$(t%) = deduc$(u3%)
L31090:           next u3%

                call "READ101" (#1, "PRLTXABL" & earntype$, f1%(1))
                if f1%(1) = 1 then delete #1
                if temp$() = " " then L31155
                write #1, using L31140,"PRLTXABL", earntype$, temp$()
L31140:              FMT CH(8), CH(12), 80*CH(6)
L31155: return

        REM *************************************************************~
            *         G E T   E A R N   T Y P E   C O D E               *~
            *                                                           *~
            * GETS NAME OF THIS EARNTYPE IN QUESTION.  NOTE THAT THE    *~
            * ONLY RESTRICTION IS THAT IT BE NON-BLANK.                 *~
            *************************************************************

            deffn'201(fieldnr%)
                  init(hex(8c))    lfac$()
                  on fieldnr% gosub L40150          /* METHOD OF DEDUCTN*/
                  goto L40220

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                         lfac$(fieldnr%) = hex(80)
                      return
L40150:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                         lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                         lfac$(fieldnr%) = hex(82)
                      return

L40220:     accept                                                       ~
               at (01,02),                                               ~
                  "Input the desired earnings type",                     ~
               at (01,67),                                               ~
                  "DATE:",                                               ~
               at (01,73), fac(hex(8c)),      date$             , ch(08),~
               at (02,02), fac(hex(ac)),      line2$            , ch(79),~
               at (04,02), fac(hex(94)),      errormsg$         , ch(79),~
               at (06,02),                                               ~
                  "Earnings type",                                       ~
               at (06,30), fac(   lfac$( 1)), earntype$         , ch(12),~
               at (21,02), fac(hex(a4)),     infomsg$           , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40580
                  call "MANUAL" ("PRLTXABL")
                  goto L40220

L40580:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

        REM *************************************************************~
            *           T A B U L A R   M O D E   S C R E E N           *~
            *                                                           *~
            * HANDLES THE INPUT OF FIELDS IN TABLE, IN EITHER INPUT,    *~
            * EDIT, INSERT, OR DELETE STYLE.                            *~
            *************************************************************

            deffn'213(fieldnr%)                    /* EDIT MODE        */

            infomsg$ = "Put an 'N' by the deduction to indicate it's NOT ~
        ~to be taken from this Earntype"

L41090:     accept                                                       ~
               at (01,02), fac(hex(8c)),     title$(1)          , ch(62),~
               at (02,02), fac(hex(8c)),     title$(2)          , ch(62),~
               at (03,02), fac(hex(94)),     errormsg$          , ch(60),~
               at (04,02), fac(hex(a4)),     infomsg$           , ch(79),~
                                                                         ~
               at (01,64), "! For Earntype: "                           ,~
               at (02,64), "!"                                          ,~
               at (03,64), "+---------------"                           ,~
               at (02,66), fac(hex(84)), earntype$,               ch(12),~
                                                                         ~
               at (05,02),  fac(hex(84)),  deduc$(line%+ 1),      ch(06),~
               at (06,02),  fac(hex(84)),  deduc$(line%+ 3),      ch(06),~
               at (07,02),  fac(hex(84)),  deduc$(line%+ 5),      ch(06),~
               at (08,02),  fac(hex(84)),  deduc$(line%+ 7),      ch(06),~
               at (09,02),  fac(hex(84)),  deduc$(line%+ 9),      ch(06),~
               at (10,02),  fac(hex(84)),  deduc$(line%+11),      ch(06),~
               at (11,02),  fac(hex(84)),  deduc$(line%+13),      ch(06),~
               at (12,02),  fac(hex(84)),  deduc$(line%+15),      ch(06),~
               at (13,02),  fac(hex(84)),  deduc$(line%+17),      ch(06),~
               at (14,02),  fac(hex(84)),  deduc$(line%+19),      ch(06),~
               at (15,02),  fac(hex(84)),  deduc$(line%+21),      ch(06),~
               at (16,02),  fac(hex(84)),  deduc$(line%+23),      ch(06),~
               at (17,02),  fac(hex(84)),  deduc$(line%+25),      ch(06),~
               at (18,02),  fac(hex(84)),  deduc$(line%+27),      ch(06),~
               at (19,02),  fac(hex(84)),  deduc$(line%+29),      ch(06),~
               at (20,02),  fac(hex(84)),  deduc$(line%+31),      ch(06),~
               at (21,02),  fac(hex(84)),  deduc$(line%+33),      ch(06),~
               at (22,02),  fac(hex(84)),  deduc$(line%+35),      ch(06),~
               at (23,02),  fac(hex(84)),  deduc$(line%+37),      ch(06),~
               at (24,02),  fac(hex(84)),  deduc$(line%+39),      ch(06),~
                                                                         ~
               at (05,42),  fac(hex(84)),  deduc$(line%+ 2),      ch(06),~
               at (06,42),  fac(hex(84)),  deduc$(line%+ 4),      ch(06),~
               at (07,42),  fac(hex(84)),  deduc$(line%+ 6),      ch(06),~
               at (08,42),  fac(hex(84)),  deduc$(line%+ 8),      ch(06),~
               at (09,42),  fac(hex(84)),  deduc$(line%+10),      ch(06),~
               at (10,42),  fac(hex(84)),  deduc$(line%+12),      ch(06),~
               at (11,42),  fac(hex(84)),  deduc$(line%+14),      ch(06),~
               at (12,42),  fac(hex(84)),  deduc$(line%+16),      ch(06),~
               at (13,42),  fac(hex(84)),  deduc$(line%+18),      ch(06),~
               at (14,42),  fac(hex(84)),  deduc$(line%+20),      ch(06),~
               at (15,42),  fac(hex(84)),  deduc$(line%+22),      ch(06),~
               at (16,42),  fac(hex(84)),  deduc$(line%+24),      ch(06),~
               at (17,42),  fac(hex(84)),  deduc$(line%+26),      ch(06),~
               at (18,42),  fac(hex(84)),  deduc$(line%+28),      ch(06),~
               at (19,42),  fac(hex(84)),  deduc$(line%+30),      ch(06),~
               at (20,42),  fac(hex(84)),  deduc$(line%+32),      ch(06),~
               at (21,42),  fac(hex(84)),  deduc$(line%+34),      ch(06),~
               at (22,42),  fac(hex(84)),  deduc$(line%+36),      ch(06),~
               at (23,42),  fac(hex(84)),  deduc$(line%+38),      ch(06),~
               at (24,42),  fac(hex(84)),  deduc$(line%+40),      ch(06),~
                                                                         ~
               at (05,09),  fac(hex(8c)),  descr$(line%+ 1),      ch(25),~
               at (06,09),  fac(hex(8c)),  descr$(line%+ 3),      ch(25),~
               at (07,09),  fac(hex(8c)),  descr$(line%+ 5),      ch(25),~
               at (08,09),  fac(hex(8c)),  descr$(line%+ 7),      ch(25),~
               at (09,09),  fac(hex(8c)),  descr$(line%+ 9),      ch(25),~
               at (10,09),  fac(hex(8c)),  descr$(line%+11),      ch(25),~
               at (11,09),  fac(hex(8c)),  descr$(line%+13),      ch(25),~
               at (12,09),  fac(hex(8c)),  descr$(line%+15),      ch(25),~
               at (13,09),  fac(hex(8c)),  descr$(line%+17),      ch(25),~
               at (14,09),  fac(hex(8c)),  descr$(line%+19),      ch(25),~
               at (15,09),  fac(hex(8c)),  descr$(line%+21),      ch(25),~
               at (16,09),  fac(hex(8c)),  descr$(line%+23),      ch(25),~
               at (17,09),  fac(hex(8c)),  descr$(line%+25),      ch(25),~
               at (18,09),  fac(hex(8c)),  descr$(line%+27),      ch(25),~
               at (19,09),  fac(hex(8c)),  descr$(line%+29),      ch(25),~
               at (20,09),  fac(hex(8c)),  descr$(line%+31),      ch(25),~
               at (21,09),  fac(hex(8c)),  descr$(line%+33),      ch(25),~
               at (22,09),  fac(hex(8c)),  descr$(line%+35),      ch(25),~
               at (23,09),  fac(hex(8c)),  descr$(line%+37),      ch(25),~
               at (24,09),  fac(hex(8c)),  descr$(line%+39),      ch(25),~
                                                                         ~
               at (05,49),  fac(hex(8c)),  descr$(line%+ 2),      ch(25),~
               at (06,49),  fac(hex(8c)),  descr$(line%+ 4),      ch(25),~
               at (07,49),  fac(hex(8c)),  descr$(line%+ 6),      ch(25),~
               at (08,49),  fac(hex(8c)),  descr$(line%+ 8),      ch(25),~
               at (09,49),  fac(hex(8c)),  descr$(line%+10),      ch(25),~
               at (10,49),  fac(hex(8c)),  descr$(line%+12),      ch(25),~
               at (11,49),  fac(hex(8c)),  descr$(line%+14),      ch(25),~
               at (12,49),  fac(hex(8c)),  descr$(line%+16),      ch(25),~
               at (13,49),  fac(hex(8c)),  descr$(line%+18),      ch(25),~
               at (14,49),  fac(hex(8c)),  descr$(line%+20),      ch(25),~
               at (15,49),  fac(hex(8c)),  descr$(line%+22),      ch(25),~
               at (16,49),  fac(hex(8c)),  descr$(line%+24),      ch(25),~
               at (17,49),  fac(hex(8c)),  descr$(line%+26),      ch(25),~
               at (18,49),  fac(hex(8c)),  descr$(line%+28),      ch(25),~
               at (19,49),  fac(hex(8c)),  descr$(line%+30),      ch(25),~
               at (20,49),  fac(hex(8c)),  descr$(line%+32),      ch(25),~
               at (21,49),  fac(hex(8c)),  descr$(line%+34),      ch(25),~
               at (22,49),  fac(hex(8c)),  descr$(line%+36),      ch(25),~
               at (23,49),  fac(hex(8c)),  descr$(line%+38),      ch(25),~
               at (24,49),  fac(hex(8c)),  descr$(line%+40),      ch(25),~
                                                                         ~
               at (05,35), fac(fac$(line%+ 1)), flag$ (line%+ 1), ch(01),~
               at (06,35), fac(fac$(line%+ 3)), flag$ (line%+ 3), ch(01),~
               at (07,35), fac(fac$(line%+ 5)), flag$ (line%+ 5), ch(01),~
               at (08,35), fac(fac$(line%+ 7)), flag$ (line%+ 7), ch(01),~
               at (09,35), fac(fac$(line%+ 9)), flag$ (line%+ 9), ch(01),~
               at (10,35), fac(fac$(line%+11)), flag$ (line%+11), ch(01),~
               at (11,35), fac(fac$(line%+13)), flag$ (line%+13), ch(01),~
               at (12,35), fac(fac$(line%+15)), flag$ (line%+15), ch(01),~
               at (13,35), fac(fac$(line%+17)), flag$ (line%+17), ch(01),~
               at (14,35), fac(fac$(line%+19)), flag$ (line%+19), ch(01),~
               at (15,35), fac(fac$(line%+21)), flag$ (line%+21), ch(01),~
               at (16,35), fac(fac$(line%+23)), flag$ (line%+23), ch(01),~
               at (17,35), fac(fac$(line%+25)), flag$ (line%+25), ch(01),~
               at (18,35), fac(fac$(line%+27)), flag$ (line%+27), ch(01),~
               at (19,35), fac(fac$(line%+29)), flag$ (line%+29), ch(01),~
               at (20,35), fac(fac$(line%+31)), flag$ (line%+31), ch(01),~
               at (21,35), fac(fac$(line%+33)), flag$ (line%+33), ch(01),~
               at (22,35), fac(fac$(line%+35)), flag$ (line%+35), ch(01),~
               at (23,35), fac(fac$(line%+37)), flag$ (line%+37), ch(01),~
               at (24,35), fac(fac$(line%+39)), flag$ (line%+39), ch(01),~
                                                                         ~
               at (05,75), fac(fac$(line%+ 2)), flag$ (line%+ 2), ch(01),~
               at (06,75), fac(fac$(line%+ 4)), flag$ (line%+ 4), ch(01),~
               at (07,75), fac(fac$(line%+ 6)), flag$ (line%+ 6), ch(01),~
               at (08,75), fac(fac$(line%+ 8)), flag$ (line%+ 8), ch(01),~
               at (09,75), fac(fac$(line%+10)), flag$ (line%+10), ch(01),~
               at (10,75), fac(fac$(line%+12)), flag$ (line%+12), ch(01),~
               at (11,75), fac(fac$(line%+14)), flag$ (line%+14), ch(01),~
               at (12,75), fac(fac$(line%+16)), flag$ (line%+16), ch(01),~
               at (13,75), fac(fac$(line%+18)), flag$ (line%+18), ch(01),~
               at (14,75), fac(fac$(line%+20)), flag$ (line%+20), ch(01),~
               at (15,75), fac(fac$(line%+22)), flag$ (line%+22), ch(01),~
               at (16,75), fac(fac$(line%+24)), flag$ (line%+24), ch(01),~
               at (17,75), fac(fac$(line%+26)), flag$ (line%+26), ch(01),~
               at (18,75), fac(fac$(line%+28)), flag$ (line%+28), ch(01),~
               at (19,75), fac(fac$(line%+30)), flag$ (line%+30), ch(01),~
               at (20,75), fac(fac$(line%+32)), flag$ (line%+32), ch(01),~
               at (21,75), fac(fac$(line%+34)), flag$ (line%+34), ch(01),~
               at (22,75), fac(fac$(line%+36)), flag$ (line%+36), ch(01),~
               at (23,75), fac(fac$(line%+38)), flag$ (line%+38), ch(01),~
               at (24,75), fac(fac$(line%+40)), flag$ (line%+40), ch(01),~
                                                                         ~
               keys(hex(000104050d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L42530
                  call "MANUAL" ("PRLTXABL")
                  goto L41090

L42530:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41090

        REM *************************************************************~
            *              T E S T   H E A D E R   D A T A              *~
            *                                                           *~
            * HEADER DATA TEST, LOAD ANY PREVIOUS SPECIFICATIONS.       *~
            * ENTRY NAME IS OUT IN THE BUFFER OR NOT.                   *~
            *************************************************************

            deffn'150(fieldnr%)
                  errormsg$, infomsg$ = " "
                  on fieldnr% gosub L50120          /* EARNTYPE         */
                  return

L50120:     REM LOAD ANY STUFF THAT WAS ALLREADY ENTERED.
                if earntype$ = " " then L50160
                gosub L30000
                return
L50160:            return clear all
                   goto inputmode

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL OPEN FILES, DISPLAYS MESSAGE, AND SEES IF ANY  *~
            * DOCUMENTS ARE IN THE BUFFER FOR THIS USER, GENERATING AN  *~
            * APPROPRIATE RETURN CODE IF THERE ARE.                     *~
            *************************************************************

            call "SHOSTAT" ("DATA BASE INTEGRITY CHECK IN PROCESS")

            end f1%(9)
