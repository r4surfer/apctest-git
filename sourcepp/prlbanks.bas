        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   RRRR   L      BBBB    AAA   N   N  K   K   SSS    *~
            *  P   P  R   R  L      B   B  A   A  NN  N  K  K   S       *~
            *  PPPP   RRRR   L      BBBB   AAAAA  N N N  KKK     SSS    *~
            *  P      R   R  L      B   B  A   A  N  NN  K  K       S   *~
            *  P      R   R  LLLLL  BBBB   A   A  N   N  K   K   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLBANKS - Add, change, delete, print (via GETCODE)       *~
            *            Payroll Banks used for direct deposits.        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/13/86 ! Original                                 ! LDJ *~
            * 08/26/86 ! Added fields                             ! HES *~
            * 01/01/89 ! Add direct deposit prenotifications      ! ERN *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            addr$(3)30,                  /* Bank Address Lines         */~
            bank$4,                      /* Bank Code                  */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            name$30,                     /* Bank Name                  */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            route$9, route_old$9,        /* Bank Routing Number        */~
            userid$3                     /* Current User Id            */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! PRLBANKF ! Payroll bank information file            *~
            * #2  ! SYSFILE2 ! System File                              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "PRLBANKF",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize =  256,                                 ~
                         keypos =    1, keylen =   4,                    ~
                         alt key 1, keypos = 5, keylen = 30, dup

            select #2,  "SYSFILE2",                                      ~
                        varc, indexed, recsize = 500,                    ~
                        keypos = 1, keylen = 20


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  0%, 0%, 100%, " ")
            call "OPENCHCK" (#2,  0%, 0%,   0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "READ100" (#2, "PRLDD.INFORMATION", tape%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$="(4)Previous Field" : pf5$=" ": pf16$="(16)Exit Program"
            pf12$ = " "
            errormsg$, inpmessage$, bank$, name$, addr$(), route$ = " "
            route_old$ = " "

            for fieldnr% = 1 to  3
L10310:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10430
L10330:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10410
L10360:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10330
                         if fieldnr% = 1% then L10310
                         goto L10360
L10410:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10330
L10430:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10330
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            inpmessage$ = "Position Cursor And ENTER To Edit Value"
            pf4$  = " "
            pf5$  = " " : pf12$ = "(12)Delete"
            pf16$ = "(16)Save Data"
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 12 then       datakill
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       edtpg1
            fieldnr% = cursor%(1) - 6
            if fieldnr% < 2 or fieldnr% >  6 then edtpg1
            if fieldnr% < 6 then fieldnr% = 2
            if fieldnr% = 6 then fieldnr% = 3
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf4$, pf5$, pf16$ = " "
L11230:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11230
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11230
            goto edtpg1

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto inputmode

        datakill
            key% = 2%
            call "ASKUSER" (key%, "***ARE YOU SURE?***",                 ~
                           "Press RETURN to DELETE this Bank",           ~
                           "-OR-", "Press PF1 to CANCEL Delete.")
            if key% = 1% then edtpg1
            if key% <> 0% then datakill
            call "DELETE" (#1, bank$, 4%)
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20120,         /* Bank Code        */~
                                    L20160,         /* Bank Name        */~
                                    L20190          /* Bank Routing No. */
                     return
L20120:     REM Bank Code                             BANK$
                inpmessage$ = "Return with a ? or blank to find "        ~
                            & "existing banks."
                return
L20160:     REM Bank Name
                inpmessage$ = "Enter Bank Name And Address"
                return
L20190:     REM Bank Routing Number
                inpmessage$ = "Enter Banks Automated Clearing House (ACH)~
        ~ Route Number, Plus Check Digit."
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            get #1 using L35035,                                          ~
               bank$,       /* direct deposit bank code                */~
               name$,       /* bank description                        */~
               addr$(),     /* bank Address                            */~
               route$       /* bank route number                       */

               route_old$ = route$
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            put #1 using L35035,                                          ~
               bank$,       /* direct deposit bank code                */~
               name$,       /* bank description                        */~
               addr$(),     /* bank Address                            */~
               route$,      /* bank route number                       */~
                " "         /* Filler (Internal, unused space)         */
            if bank% = 0% then write   #1
            if bank% = 1% then rewrite #1
            if bank% = 0% or tape% = 0% or route$ = route_old$           ~
                                                              then return
                call "PRLDSPNO" (bank$)
                return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35035: FMT                 /* FILE: PRLBANKF                          */~
            CH(4),          /* direct deposit bank code                */~
            CH(30),         /* bank description                        */~
            3*CH(30),       /* bank address                            */~
            CH(9),          /* bank route number                       */~
            CH(123)         /* Filler (Internal, unused space)         */~

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  str(line2$,62%) = "PRLBANKS: " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40200,         /* Bank Code        */~
                                    L40170,         /* Bank Name        */~
                                    L40200          /* Bank Routing No. */
                  goto L40270

L40170:           REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40200:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40270:     accept                                                       ~
               at (01,02),                                               ~
                  "Maintain Payroll Direct Deposit Banks",               ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Bank Code",                                  ~
               at (06,30), fac(lfac$( 1)), bank$                , ch(04),~
                                                                         ~
               at (08,02), "Bank Name",                                  ~
               at (08,30), fac(lfac$( 2)), name$                , ch(30),~
               at (09,02), " Address (Line One)",                        ~
               at (09,30), fac(lfac$( 2)), addr$(1)             , ch(30),~
               at (10,02), " Address (Line Two)",                        ~
               at (10,30), fac(lfac$( 2)), addr$(2)             , ch(30),~
               at (11,02), " City, State, Zip",                          ~
               at (11,30), fac(lfac$( 2)), str(addr$(3),,19)    , ch(19),~
               at (11,50), fac(lfac$( 2)), str(addr$(3),20,2)   , ch(02),~
               at (11,56), fac(lfac$( 2)), str(addr$(3),22,5)   , ch(05),~
               at (11,62), fac(lfac$( 2)), str(addr$(3),27,4)   , ch(04),~
               at (12,02), "ACH Routing No.",                            ~
               at (12,30), fac(lfac$( 3)), str(route$,,8)       , ch(08),~
               at (12,39), fac(lfac$( 3)), str(route$,9)        , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,20), fac(hex(8c)), pf12$                          ,~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(000104050c0d0f10)),                              ~
               key (keyhit%)

               if keyhit% <> 13 then L40690
                  call "MANUAL" ("PRLBANKS")
                  goto L40270

L40690:        if keyhit% <> 15 then L40730
                  call "PRNTSCRN"
                  goto L40270

L40730:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50120,         /* Bank Code        */~
                                    L50240,         /* Bank Name        */~
                                    L50430          /* Bank Routing No. */
                  return
L50120:     REM Bank Code                             BANK$
                if bank$ <> " " and bank$ <> "?" then L50180
                call "GETCODE" (#1, bank$, name$, 0%, 1.0, bank%)
                if bank%  = 1% then L50180
                     errormsg$ = "Bank Not On File / Not Selected"
                     return
L50180:         call "READ101" (#1, bank$, bank%)
                if bank% = 0% then return
                    gosub dataload
                    fieldnr% = 99%
                    return

L50240:     REM Bank Name                             NAME$
                if name$ = " " then L50320
                if addr$(1) = " " then L50340
                if str(addr$(3),,19) = " " then L50360
                if str(addr$(3),20,2) = " " then L50380
                if pos(str(addr$(3),20,2) < "A") <> 0% then L50411
                if pos(str(addr$(3),20,2) > "Z") <> 0% then L50411
                if str(addr$(3),22,5) = " " then L50400
                str(addr$(3),20,2) = and all(hex(5f))
                return
L50320:         errormsg$ = "Name Can't Be Blank!"
                return
L50340:         errormsg$ = "Address Can't Be Blank."
                return
L50360:         errormsg$ = "City Can't Be Blank."
                return
L50380:         errormsg$ = "State Can't Be Blank."
                return
L50400:         errormsg$ = "Zip Code Can't Be Blank."
                return
L50411:         errormsg$ = "State Must Be Uppercase."
                return

L50430:     REM Bank Routing Number
                check = 0
                for i% = 1 to 8
                     convert str(route$,i%,1) to nbr, data goto L50640
                     on i% goto L50500,, L50490, L50500,, L50490, L50500
                     check = check + (nbr*7) : goto L50510
L50490:              check = check +  nbr    : goto L50510
L50500:              check = check + (nbr*3)
L50510:         next i%
                check = 10 - mod(check+10,10)
                if check = 10 then check = 0
                convert check to temp$, pic(0)
                if str(route$,9,1) = " " then L50620
                if str(route$,9,1) = temp$ then L50620
                     errormsg$ = "Incorrect Check Digit (Pos 9): " &     ~
                                                          str(route$,9,1)
                     return
L50620:         str(route$,9,1) = temp$
                return
L50640:         errormsg$ = "Route Must Be 9 Numbers."
                return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
