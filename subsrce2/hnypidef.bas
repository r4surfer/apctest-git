        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  PPPP   IIIII  DDDD   EEEEE  FFFFF   *~
            *  H   H  NN  N  Y   Y  P   P    I    D   D  E      F       *~
            *  HHHHH  N N N   YYY   PPPP     I    D   D  EEEE   FFFF    *~
            *  H   H  N  NN    Y    P        I    D   D  E      F       *~
            *  H   H  N   N    Y    P      IIIII  DDDD   EEEEE  F       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYPIDEF - This subroutine allows setting and changing of *~
            *            the switches governing the behavior of a       *~
            *            Physical Inventory Count Session.              *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/13/85 ! ORIGINAL                                 ! LDJ *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "HNYPIDEF" (#1,              /* HNYPISYS File              */~
                        session_nbr$,    /* Count Session Number       */~
                        session_date$,   /* Planned count date         */~
                        description$,    /* Session notes/name         */~
                        part_req$,       /* Part numbers required flag */~
                        print$,          /* Print sheets or tickets ?  */~
                        hnyvar$,         /* Update QTY-ON-HAND Var Flag*/~
                        glvar$,          /* Update G/L Variance Flag   */~
                        keyhit%)         /* PF Key passed back;        */~
                                         /*    0 = save data           */~
                                         /*    1 = start over          */~
                                         /*    4 = prior screen        */~
                                         /*   16 = exit, no save       */

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            description$30,              /* Session notes/name         */~
            error$79,                    /* Error message              */~
            errormsg$79,                 /* Error message              */~
            glvar$1,                     /* Update G/L Variance Flag   */~
            hnyvar$1,                    /* Update QTY-ON-HAND Var Flag*/~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Input message              */~
            lfac$(20)1,                  /* Field attribute characters */~
            line2$79,                    /* Second Header line on scrns*/~
            part_req$1,                  /* Part numbers required flag */~
            pfkeys$8,                    /* Valid PF Keys List         */~
            pfs1$(2)79,                  /* Screen 1 PF Key Literals   */~
            print$1,                     /* Print sheets or tickets ?  */~
            session_date$8,              /* Planned count date         */~
            session_nbr$2                /* Count Session Number       */~

        dim f2%(01),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(01),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(01)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(01)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! HNYPISYS ! Physical Inventory System Session Contro *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *


            call "OPENFILE" (#1,  "SHARE", f2%(1 ), rslt$(1 ), axd$(1 ))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            call "DATEFMT" (session_date$)
            date$ = date
            call "DATEFMT" (date$)
            str(line2$,63%) = "HNYPIDEF:" & str(cms2v$,,8%)
            if session_nbr$ > " " then L09150
               new% = 1%
               f1%(1) = 0%
L09150:     pfs1$(1) =                                                   ~
        "(1)Start Over                   (4)Previous field              (~
        ~15)Print Screen"
            pfs1$(2) =                                                   ~
        "                                                               (~
        ~16)Exit"

            if session_nbr$ = " " then pfkeys$ = hex(0001040d0f10)       ~
            else                       pfkeys$ = hex(000104050d0f10)

            init(" ") errormsg$

            if glvar$ > " " then editmode

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

            if session_nbr$>" " then first_field%=4% else first_field%=1%

            for fieldnr% = first_field% to 7%
                gosub'051(fieldnr%)
                      if enabled% = 0% then       L10175
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then       L65000
                      if keyhit% <>  4 then       L10140
L10132:                  fieldnr% = max(first_field%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 0% then L10132
                         goto L10120
L10140:               if keyhit%  = 16 then keyhit% = 32%
                      if keyhit%  = 32 then       L65000
                      if keyhit% <>  0 then       L10120
                gosub'151(fieldnr%)
                     if errormsg$ <> " " then L10120
L10175:     next fieldnr%
            if new% = 0% then L65000

        REM *************************************************************~
            *                     E D I T   M O D E                     *~
            *************************************************************
        editmode
            inpmessage$ = "TO MODIFY DISPLAYED VALUES POSITION CURSOR OR ~
        ~TAB TO DESIRED FIELD"
            if new% = 1% then pfs1$(1) =                                 ~
        "(1)Start Over                                                  (~
        ~15)Print Screen"                                                 ~
            else              pfs1$(1) =                                 ~
        "(1)Start Over                   (4)Previous Screen             (~
        ~15)Print Screen"
            if new% = 1% then                                            ~
            pfs1$(2) =                                                   ~
        "                                                               (~
        ~16)Save Data"                                                    ~
            else pfs1$(2) =                                              ~
        "                                (5)First Screen"

L11130:     gosub'101(0%)
                  if keyhit%  =  1 then       L65000
             gosub'151(0%)
                  if errormsg$ <> " " then L11130
                  if keyhit%  =  4 then       L65000
                  if keyhit%  =  5 then       L65000
                  if keyhit%  = 16 then       L65000
             goto L11130

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                inpmessage$ = " "
                on fieldnr% gosub L20100,           /* Count Session    */~
                                  L20191,           /* Description      */~
                                  L20200,           /* Plan Count Date  */~
                                  L20400,           /* P/N's Required   */~
                                  L20500,           /* Sheets or Tickets*/~
                                  L20600,           /* Update Inventory */~
                                  L20700            /* Update G/L       */
                return

L20100:     REM DEFAULT/ENABLE FOR COUNT SESSION NUMBER
                if new% = 1% and session_nbr$=" " then enabled% = 1%
                inpmessage$ = "Enter the Count Session to Edit"
                return
L20191:     REM DEFAULT/ENABLE FOR COUNT SESSION DESCRIPTION
                if new% = 1% and session_nbr$=" " then enabled% = 1%
                inpmessage$ = "Enter or modify the Count Session Notes"
                return
L20200:     REM DEFAULT/ENABLE FOR PLANNED COUNT DATE
                if new% = 1% and session_nbr$=" " then enabled% = 1%
                inpmessage$ = "Enter or modify the Planned Count Date "
                return
L20400:     REM DEFAULT/ENABLE FOR PART NUMBERS REQUIRED
                enabled% = 1%
                if part_req$ = " " then part_req$ = "N"
                inpmessage$ = "Enter Y to force Entry of Part Numbers whe~
        ~n Entering Count Quantities"
                return
L20500:     REM DEFAULT/ENABLE FOR PRINT SHEETS OR TICKETS ?
                enabled% = 1%
                inpmessage$ = "Enter C to Print Count Sheets or T to Prin~
        ~t Tickets"
                if print$ = " " then print$ = "C"
                return
L20600:     REM DEFAULT/ENABLE FOR UPDATE QTY-ON-HAND WITH VAR
                enabled% = 1%
                if hnyvar$ = " " then hnyvar$ = "Y"
                return
L20700:     REM DEFAULT/ENABLE FOR UPDATE GENERAL LEDGER WITH
                enabled% = 1%
                if glvar$ = " " then glvar$ = "Y"
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  if fieldnr% = 0% then init(hex(81)) lfac$()            ~
                                   else init(hex(8c)) lfac$()
                  if new%=0% then lfac$(1), lfac$(2), lfac$(3) = hex(8c) ~
                             else lfac$(1) = hex(8c)

                  on fieldnr% gosub L40180,         /* Count Session Num*/~
                                    L40180,         /* Count Date       */~
                                    L40180,         /* Description      */~
                                    L40180,         /* Part #'s Req ?   */~
                                    L40180,         /* Sheets or Tickets*/~
                                    L40180,         /* Post HNY Variance*/~
                                    L40180          /* Post G/L Variance*/
                goto L40295

L40180:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return


L40295: accept                                                           ~
               at (01,02),                                               ~
        "Define Physical Inventory Session Behavior",                    ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
        "COUNT SESSION NUMBER       :",                                  ~
               at (06,32), fac(lfac$(01)),        session_nbr$  , ch(02),~
               at (07,02),                                               ~
        "PLANNED COUNT DATE         :",                                  ~
               at (07,32), fac(lfac$(02)),        session_date$ , ch(08),~
               at (08,02),                                               ~
        "SESSION NOTES/NAME         :",                                  ~
               at (08,32), fac(lfac$(03)),        description$  , ch(30),~
               at (10,02),                                               ~
        "PART NUMBERS REQUIRED IN COUNT ENTRY (Y/N)?:",                  ~
               at (10,48), fac(lfac$(04)),        part_req$     , ch(01),~
               at (11,02),                                               ~
        "PRINT COUNT ITEMS ON COUNT SHEETS OR INDIVIDUAL TICKETS (C/T)?:"~
        ,                                                                ~
               at (11,67), fac(lfac$(05)),        print$        , ch(01),~
               at (12,02),                                               ~
        "UPDATE QUANTITY-ON-HAND WITH VARIANCE QTY (Y/N)?:",             ~
               at (12,53), fac(lfac$(06)),        hnyvar$       , ch(01),~
               at (13,02),                                               ~
        "UPDATE GENERAL LEDGER WITH VARIANCE AMT   (Y/N)?:",             ~
               at (13,53), fac(lfac$(07)),        glvar$        , ch(01),~
               at (15,02),                                               ~
        "The above behavior switches control the behavior of certain func~
        ~tions within",                                                   ~
               at (16,02),                                               ~
        "the Physical Inventory Sub-Module for the selected Count Session~
        ~.",                                                              ~
               at (21,02), fac(hex(a4)), inpmessage$,             ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), fac(hex(8c)), pfs1$(1),                ch(79),~
               at (24,02), fac(hex(8c)), pfs1$(2),                ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L40770
                  call "MANUAL" ("HNYPIDEF")
                  goto L40295

L40770:        if keyhit% <> 15 then L40810
                  call "PRNTSCRN"
                  goto L40295

L40810:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return
                  u3% = u3%

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
              errormsg$ = " "
              if fieldnr% > 0% then L50200
              for fieldnr% = 1% to 7%
                  on fieldnr% gosub L50290,         /* Count Session    */~
                                    L50490,         /* Plan Count Date  */~
                                    L50561,         /* Description      */~
                                    L50570,         /* P/N's Required   */~
                                    L50620,         /* Sheets or Tickets*/~
                                    L50670,         /* Update Inventory */~
                                    L50720          /* Update G/L       */
              next fieldnr%
              return

L50200:       on fieldnr% gosub L50290,             /* Count Session    */~
                                L50490,             /* Plan Count Date  */~
                                L50561,             /* Description      */~
                                L50570,             /* P/N's Required   */~
                                L50620,             /* Sheets or Tickets*/~
                                L50670,             /* Update Inventory */~
                                L50720              /* Update G/L       */
              return

L50290:     REM TEST DATA FOR COUNT SESSION NUMBER
                if new% = 0% then return
                if f1%(1) = 1% then return
                description$ = hex(06) & "Select Session to Edit"
                call "GETCODE" (#1,session_nbr$,description$,0%,.3,f1%(1))
                if f1%(1) = 1% then L50380
                errormsg$ = "COUNT SESSION MUST ALREADY EXIST!"
                return
L50380:         get #1 using L50410, session_date$,description$,part_req$,~
                                    print$, hnyvar$, glvar$
                call "DATEFMT" (session_date$)
L50410:         FMT CH(6), POS(9), CH(30), POS(343), 4*CH(1)
                fieldnr% = 99%
                return
L50490:     REM TEST DATA FOR COUNT SESSION DATE
                if new% = 0% then return
                call "DATEOK" (session_date$, date%, error$)
                if error$ = " " then return
                errormsg$ = error$
                return
                date% = date%
L50561:     REM TEST DATA FOR COUNT SESSION DESCRIPTION
        /*      IF DESCRIPTION$ > " " THEN RETURN
                ERRORMSG$ = "COUNT SESSION DESCRIPTION / NOTES CANNOT BE ~
        BLANK" */
                return
L50570:     REM TEST DATA FOR PART NUMBERS REQUIRED
                if part_req$ = "Y" or part_req$ = "N" then return
                errormsg$ = "Part Numbers Required Must be Y or N"
                return
L50620:     REM TEST DATA FOR PRINT SHEETS OR TICKETS ?
                if print$ = "C" or print$ = "T" then return
                errormsg$ = "Print Selection Switch Must be C or T"
                return
L50670:     REM TEST DATA FOR UPDATE QTY-ON-HAND WITH VAR
                if hnyvar$ = "Y" or hnyvar$ = "N" then return
                errormsg$ = "Inventory Variance Switch Must be Y or N"
                return
L50720:     REM TEST DATA FOR UPDATE GENERAL LEDGER WITH
                if glvar$ = "Y" or glvar$ = "N" then return
                errormsg$="General Ledger Variance Switch Must be Y or N"
                return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "DATUNFMT" (session_date$)
            end
