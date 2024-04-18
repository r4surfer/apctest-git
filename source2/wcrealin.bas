        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  W   W   CCC   RRRR   EEEEE    A    L      IIIII  N   N   *~
            *  W   W  C   C  R   R  E       A A   L        I    NN  N   *~
            *  W   W  C      RRRR   EEEE   AAAAA  L        I    N N N   *~
            *  W W W  C   C  R   R  E      A   A  L        I    N  NN   *~
            *   W W    CCC   R   R  EEEEE  A   A  LLLLl  IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * WCREALIN - Program multiplies the WC Units in WCMASTER and*~
            *            the Units used in ALL Routes by the factor     *~
            *            entered.  Also Adjusts WCOUTS and Job Status   *~
            *            Records in JBSTATUS.                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/08/87 ! Original                                 ! HDC *~
            * 01/06/89 ! Corrected FMT at ln 32090 & changed ref  ! MJB *~
            *          !  to FMT at ln 33660                      !     *~
            * 03/27/89 ! Added display of workcenter description, !     *~
            *          ! removed PF4 display on mod to field 1    !     *~
            * 04/04/89 ! Last WC # actually displayed in heading  ! MLJ *~
            * 04/16/91 ! PRR 11070.  Added a warning message for  ! SID *~
            *          !     an exclusive use of WCMASTR file.    !     *~
            * 06/20/91 ! Added 'ALLFREE' and removed 'ROUND'      ! SID *~
            *          !  function for a more precision display   !     *~
            *          !  of the minutes and hours.               !     *~
            * 03/18/94 ! Added WCDFLTS - resets setup & run units.! MLJ *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            altplowkey$34,               /* Alt Route Plowkey          */~
            awc0$4,                      /* Alt WC                     */~
            awc1$4,                      /* Alt. WC 1                  */~
            awc2$4,                      /* Alt. WC 2                  */~
            awc3$4,                      /* Alt. WC 3                  */~
            currunit$50,                 /* Cur WC unit definition     */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lastwc$4,                    /* Last WC Fixed              */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            mulfact$6,                   /* Multiply By                */~
            newunit$50,                  /* New WC unit definition     */~
            oldwcdaily$6,                /* Old WC Units Per Day       */~
            oldwcperday$(7)5,            /* Old Mon-Sun Units          */~
            oldwcperday%(7),             /*                            */~
            newwcdaily$6,                /* New WC Units Per Day       */~
            newwcperday$(7)5,            /* New Mon-Sun Units          */~
            newwcperday%(7),             /*                            */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            rteplowkey$34,               /* Plowkey for Routes         */~
            titl$37,                     /* Screen column titles       */~
            userid$3,                    /* Current User Id            */~
            wcamount%(980),              /* Capacity for Each Day      */~
            wc$4,                        /* WC Code for Routing        */~
            wc1$4,                       /* 1st Concurrent             */~
            wc2$4,                       /* 2nd Concurrent             */~
            wc3$4,                       /* 3rd Concurrent             */~
            wcdescr$30,                  /* WC Description             */~
            wcplowkey$27,                /* WC PLOWKEY                 */~
            wcreadkey$5,                 /* Plowkey                    */~
            wcperday%(7)                 /* Mon- Sunday Units          */~

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! WCMASTR  ! Workcenter Master File                   *~
            * # 2 ! RTEMASTR ! Production routing master file           *~
            * # 3 ! RTEALTRS ! Alternate Routing file                   *~
            * # 4 ! WCOUT    ! WORK CENTER USAGE CROSS REFERENCE        *~
            * # 5 ! JBSTATUS ! Production JOB Actuals                   *~
            * # 6 ! WCDFLTS  ! Work Center Defaults File                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "WCMASTR",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =   5,                     ~
                         alt key 1, keypos =    1, keylen =   6

            select # 2, "RTEMASTR",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    5, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  35          ~

            select # 3, "RTEALTRS",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  34

            select # 4, "WCOUT",                                         ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 68,                                   ~
                         keypos = 9, keylen = 23,                        ~
                         alt key 1, keypos = 1, keylen = 27

            select # 5, "JBSTATUS",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 200,                                  ~
                         keypos = 1, keylen = 12,                        ~
                         alt key 1, keypos = 21, keylen = 44,            ~
                             key 2, keypos = 29, keylen = 36

            select # 6, "WCDFLTS",                                       ~
                        varc,     indexed,  recsize =   100,             ~
                        keypos =    1, keylen =   5

L02400:     keyhit% = 2%
            call "ASKUSER" (keyhit%, "W A R N I N G",                    ~
            "This program requires an exclusive use of SIX files.",      ~
            "A file possession conflict condition may occur if this ",   ~
            "condition is not honored.  Press <RETURN> to continue or "  ~
            & "PF16 to exit.")
            if keyhit% = 16% then L65000
            if keyhit% <> 0% then L02400

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (# 1, "IO   ", f2%( 1), rslt$( 1), " ")
            call "OPENFILE" (# 2, "IO   ", f2%( 2), rslt$( 2), " ")
            call "OPENFILE" (# 3, "IO   ", f2%( 3), rslt$( 3), " ")
            call "OPENFILE" (# 4, "IO   ", f2%( 4), rslt$( 4), " ")
            call "OPENFILE" (# 5, "IO   ", f2%( 5), rslt$( 5), " ")
            call "OPENFILE" (# 6, "IO   ", f2%( 6), rslt$( 6), " ")

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

            titl$ = "Current      New    Conversion Factor"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$ = " " : pf5$ = " " : pf16$ = "(16)Exit Program"
            gosub L29000

            for fieldnr% = 1 to  2
                if fieldnr% > 1 then pf4$ = "(4)Previous Field"
                if fieldnr% > 1 then pf16$ = " "
L10120:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10240
L10140:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10220
L10170:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10140
                         if fieldnr% = 1% then L10120
                         goto L10170
L10220:               if keyhit% = 16 and fieldnr% = 1 then exit_program
                      if keyhit% <>  0 then       L10140
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10140
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            pf4$  = " "
            pf5$  = " "
            pf16$ = "(16)Adjust Files"
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editpg1
            if cursor%(1) <> 6 and cursor%(1) <> 9 then editpg1
            if cursor%(1) = 6 then fieldnr% = 1
            if cursor%(1) = 9 then fieldnr% = 2
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
                  pf4$, pf5$ = " " : pf16$ = "(16)Exit Program"
L11220:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11220
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11220
            goto editpg1

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            lastwc$ = wctochng$
            if mulfact = 1 then inputmode
            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20120,         /* Work Center        */    ~
                              L20150          /* New 24 hr units    */
            return
L20120: REM Def/Enable WC To Change                WCTOCHNG$
            pf4$ = " "
            inpmessage$ = "Input WC Code For Work Center Desired"
            return
L20150: REM Def/Enable For Mult Factor             NEWWCDAILY$
            if newwcdaily$ = " " then newwcdaily$ = oldwcdaily$
            inpmessage$ = "Enter the new units for a 24 hour period."
            return

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, wctochng$, wcdescr$, hrs$, ~
                      oldwcdaily$, oldwcperday$(), newwcdaily$, min$,    ~
                      newwcperday$(), mulfact$, currunit$, newunit$
            call "ALLFREE"
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
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
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput

            call "ASKUSER" (keyhit%,"CAUTION",  "You Will Reset The Work ~
        ~Center Units in The WCMASTR File","Also RUN - SETUP - Concurrent ~
        ~Units Factors In Routes Where This W/C is Used","Press PF16 To Co~
        ~ntinue Or Any Other PF Key To START OVER.")
            if keyhit% <> 16 then startover
            call "SHOSTAT" ("Fixing W/C Master File For: " & wctochng$)

            call "READ101" (#1,wcreadkey$,f1%(1))
            if f1%(1) = 0 then return
            get #1, using L31540, wcperday%(), wcamount%(), wcdaily
L31540:         FMT POS(37), 7*BI(2), POS(60), 980*BI(2), BI(2)

        REM First Fix WCDAILY
            wcdaily = min(round(wcdaily*mulfact,0),65000)

        REM Now Fix WCPERDAY
            for i% = 1% to 7%
                wcperday%(i%) = min(round(wcperday%(i%)*mulfact,0),65000)
            next i%

        REM Now Fix WCAMOUNT
            for i% = 1% to 980%
                wcamount%(i%) = min(round(wcamount%(i%)*mulfact,0),65000)
            next i%

        REM Put Back New Data

            put #1, using L31540, wcperday%(), wcamount%(), wcdaily
            rewrite #1
*          GOTO FIX_RTEMASTR

        REM Adjust Setup & Run Units In WCDFLTS...
            call "READ101" (#6, wcreadkey$, f1%(6%))
                if f1%(6%) <> 1% then L32000
            call "SHOSTAT" ("Adjusting Work Center Defaults File")
            get #6 using L31810, wcd_su, wcd_ru
L31810:         FMT POS(19), BI(4), PD(14,4)
            wcd_su = round(wcd_su * mulfact, 0)
            wcd_ru = round(wcd_ru * mulfact, 4)
            put #6 using L31810, wcd_su, wcd_ru
            rewrite #6

L32000: REM Go Fix RTE
*        FIX_RTEMASTR
            call "SHOSTAT" ("Adjusting Route Master File")
            rteplowkey$ = all(hex(00))
            call "PLOWNXT1" (#2,rteplowkey$, 0%, f1%(2))
L32050:          if f1%(2) = 0 then fix_alternate

            get #2, using L32090, wc$, mq, su, run, wc1$, nm1, wc2$, nm2, ~
                    wc3$, nm3
L32090:     FMT POS(1), CH(4), POS(36), BI(4), BI(2), PD(14,4), CH(4),   ~
                PD(7,4), CH(4), PD(7,4), CH(4), PD(7,4)

        REM Test For And Change Primary WC
            if wc$ <> wctochng$ then L32240
               su =  round(su * mulfact, 0)
               run = round(run * mulfact, 4)
            if wc1$ = " " then L32410
               nm1 = round(nm1/mulfact, 4)
            if wc2$ = " " then L32410
               nm2 = round(nm2/mulfact, 4)
            if wc3$ = " " then L32410
               nm3 = round(nm3/mulfact, 4)
            goto L32410

L32240: REM Test 1st Concurrent
            if wc1$ = " " then L32450
            if wc1$ <> wctochng$ then L32300
               nm1 = round(nm1 * mulfact, 4)
            goto L32410

L32300: REM Test 2nd Concurrent
            if wc2$ = " " then L32450
            if wc2$ <> wctochng$ then L32360
               nm2 = round(nm2 * mulfact, 4)
            goto L32410

L32360: REM Test 3rd Concurrent
            if wc3$ = " " then L32450
            if wc3$ <> wctochng$ then L32450
               nm3 = round(nm3 * mulfact,4)

L32410: REM Put Back Fixed Route
            put #2, using L32090, wc$, mq, su, run, wc1$, nm1, wc2$, nm2, ~
                    wc3$, nm3
            rewrite #2
L32450:     call "READNXT1" (#2, f1%(2))
            goto L32050


        fix_alternate
            call "SHOSTAT" ("Adjusting Route Alternates File")
            altplowkey$ = all(hex(00))
            call "PLOWNXT1" (#3,altplowkey$, 0%, f1%(3))
L32540:          if f1%(3) = 0 then fix_wcout

            get #3, using L32580, awc0$, su, run, awc1$, nm1, awc2$, nm2, ~
                    awc3$, nm3
L32580:     FMT POS(35), CH(4), POS(43), BI(2), PD(14,4), CH(4), PD(7,4),~
                    CH(4), PD(7,4), CH(4), PD(7,4)

        REM Test For And Change Primary WC
            if awc0$ <> wctochng$ then L32730
               run = round(run * mulfact, 4)
               su = round(su * mulfact, 0)
            if awc1$ = " " then L32900
               nm1 = round(nm1 / mulfact,4)
            if awc2$ = " " then L32900
               nm2 = round(nm2 / mulfact,4)
            if awc3$ = " " then L32900
               nm3 = round(nm3 / mulfact,4)
            goto L32900

L32730: REM Test 1st Concurrent
            if awc1$ = " " then L32940
            if awc1$ <> wctochng$ then L32790
               nm1 = round(nm1 * mulfact,4)
            goto L32900

L32790: REM Test 2nd Concurrent
            if awc2$ = " " then L32940
            if awc2$ <> wctochng$ then L32850
               nm2 = round(nm2 * mulfact,4)
            goto L32900

L32850: REM Test 3rd Concurrent
            if awc3$ = " " then L32940
            if awc3$ <> wctochng$ then L32940
               nm3 = round(nm3 * mulfact,4)

L32900: REM Put Back Fixed Alt Route
            put #3, using L32580, awc0$, su, run, awc1$, nm1, awc2$, nm2, ~
                    awc3$, nm3
            rewrite #3
L32940:     call "READNXT1" (#3, f1%(3))
            goto L32540


        fix_wcout
            call "SHOSTAT" ("Fixing WCOUT's For " & wctochng$)
        REM Plow on Work Center...
            wcplowkey$ = all(hex(00))
            str(wcplowkey$,,4) = wctochng$
            call "PLOWAL1" (#4, wcplowkey$, 1%, 4%, f1%(4))
L33060:         if f1%(4) = 0 then fix_jbstatus
            get #4, using L33080, wc$, setup, run
L33080:         FMT CH(4), POS(32), 2*BI(4)
            if wc$ <> wctochng$ then fix_jbstatus

        REM Multiply Run and Setup By MULFACT
            run = round(run * mulfact, 0)
            setup = round(setup * mulfact, 0)

        REM Write It Back...
            put #4, using L33170, setup, run
L33170:         FMT POS(32), 2*BI(4)
            rewrite #4
            call "READNXT1" (#4, f1%(4))
            goto L33060


        fix_jbstatus
            call "SHOSTAT" ("Fixing Job Details From " & wctochng$)
        REM Plow on Work Center...
            wcplowkey$ = all(hex(00))
            str(wcplowkey$,,4) = wctochng$
            call "PLOWAL1" (#5, wcplowkey$, 2%, 4%, f1%(5))
L33560:         if f1%(5) = 0 then return
            get #5, using L33580, wc$, setup, run
L33580:         FMT POS(13), CH(4), POS(91), 2*BI(4)
            if wc$ <> wctochng$ then return

        REM Multiply Run and Setup By MULFACT
            run = round(run * mulfact, 0)
            setup = round(setup * mulfact, 0)

        REM Write It Back...
            put #5, using L33670, setup, run
L33670:         FMT POS(91), 2*BI(4)
            rewrite #5
            call "READNXT1" (#5, f1%(5))
            goto L33560


        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

        FMT                 /* FILE: WCMASTR                           */~
            CH(1),          /* General purpose status indicator        */~
            CH(4),          /* Work Center code                        */~
            CH(1),          /* space taker                             */~
            CH(30),         /* work centre description                 */~
            7*BI(2),        /* normal units worked per day             */~
            CH(3),          /* frequency of PM's for a given work cente*/~
            CH(6),          /* date of last prev maintenance in a work */~
             980*BI(2),     /* amount of wc capacity available or used */~
            BI(2),          /* Number Of WC Units In 24 Hours (converti*/~
            CH(3)           /* Filler (Internal, unused space)         */~

        FMT                 /* FILE: RTEMASTR                          */~
            CH(4),          /* Work Center code                        */~
            CH(25),         /* Part Number                             */~
            CH(3),          /* The specific routing for multiple altern*/~
            CH(3),          /* Route sequence number                   */~
            BI(4),          /* Move / queue time in days               */~
            BI(2),          /* Set up time in hours                    */~
            PD(14,4),       /* Run time in hours per part              */~
            CH(4),          /* Concurrent work center # 1              */~
            PD(7,4),        /* Concurrent work center # 1 normalizer   */~
            CH(4),          /* Concurrent work center # 2              */~
            PD(7,4),        /* Concurrent work center # 2 normalizer   */~
            CH(4),          /* Concurrent work center # 3              */~
            PD(7,4),        /* Concurrent work center # 3 Normalizer   */~
            PD(7,6),        /* Alternate work center shift differential*/~
            CH(1),          /* Generic option field                    */~
            BI(1),          /* Yield expected thru a given RTE step    */~
            PD(14,7),       /* Yield expected thru a given RTE step    */~
            CH(4),          /* closing step number in route / workcente*/~
            BI(1),          /* Percentage of cost incurred thru a given*/~
            CH(04),         /* Type/Classification Code                */~
            CH(30),         /* Activity to be performed                */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(16)          /* Filler (Internal, unused space)         */~

        FMT                 /* FILE: RTEALTRS                          */~
            CH(25),         /* Part Number                             */~
            CH(3),          /* The specific routing for multiple altern*/~
            CH(3),          /* Route sequence number                   */~
            CH(3),          /* Alternate Route sequence                */~
            CH(4),          /* Primary alternate work center           */~
            BI(4),          /* Alternate Move Queue time               */~
            BI(2),          /* Alternate work center set up time       */~
            PD(14,4),       /* Alternate work center run time          */~
            CH(4),          /* Concurrent work center # 1              */~
            PD(7,4),        /* Concurrent work center # 1 normalizer   */~
            CH(4),          /* Concurrent work center # 2              */~
            PD(7,4),        /* Concurrent work center # 2 normalizer   */~
            CH(4),          /* Concurrent work center # 3              */~
            PD(7,4),        /* Concurrent work center # 3 Normalizer   */~
            PD(7,6),        /* Alternate work center shift differential*/~
            CH(1),          /* Generic option field                    */~
            CH(69)          /* Filler (Internal, unused space)         */~

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              str(line2$,62%) = "WCREALIN: " & str(cms2v$,,8%)
              str(line2$,01,21) = "Last W/C Fixed : " & lastwc$
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40160,         /* Work Center       */   ~
                                L40170          /* New 24hr units    */
              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40170:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02),                                               ~
                  "Adjust Work Center & Route Units",                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Work Center",                                ~
               at (06,30), fac(lfac$( 1)), wctochng$            , ch(04),~
               at (06,38), fac(hex(8c)),wcdescr$                , ch(30),~
               at (08,30), fac(hex(ac)), titl$                  , ch(37),~
                                                                         ~
               at (09,02), "Units In A 24 Hr. Period: ",                 ~
               at (09,29), fac(hex(8c)), oldwcdaily$            , ch(06),~
               at (09,40), fac(lfac$( 2)), newwcdaily$          , ch(06),~
               at (09,50), fac(hex(8c)), mulfact$               , ch(06),~
               at (10,20), "Monday ",                                    ~
               at (10,30), fac(hex(8c)),oldwcperday$(1)         , ch(05),~
               at (10,41), fac(hex(8c)),newwcperday$(1)         , ch(05),~
               at (11,20), "Tuesday",                                    ~
               at (11,30), fac(hex(8c)),oldwcperday$(2)         , ch(05),~
               at (11,41), fac(hex(8c)),newwcperday$(2)         , ch(05),~
               at (12,20), "Wednesday",                                  ~
               at (12,30), fac(hex(8c)),oldwcperday$(3)         , ch(05),~
               at (12,41), fac(hex(8c)),newwcperday$(3)         , ch(05),~
               at (13,20), "Thursday",                                   ~
               at (13,30), fac(hex(8c)),oldwcperday$(4)         , ch(05),~
               at (13,41), fac(hex(8c)),newwcperday$(4)         , ch(05),~
               at (14,20), "Friday ",                                    ~
               at (14,30), fac(hex(8c)),oldwcperday$(5)         , ch(05),~
               at (14,41), fac(hex(8c)),newwcperday$(5)         , ch(05),~
               at (15,20), "Saturday",                                   ~
               at (15,30), fac(hex(8c)),oldwcperday$(6)         , ch(05),~
               at (15,41), fac(hex(8c)),newwcperday$(6)         , ch(05),~
               at (16,20), "Sunday ",                                    ~
               at (16,30), fac(hex(8c)),oldwcperday$(7)         , ch(05),~
               at (16,41), fac(hex(8c)),newwcperday$(7)         , ch(05),~
                                                                         ~
               at (18,02), "Current W/C unit = ",                        ~
               at (18,25), fac(hex(8c)), currunit$              , ch(50),~
               at (19,02), "New W/C unit = ",                            ~
               at (19,25), fac(hex(8c)), newunit$               , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$,                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$,                          ~
                                                                         ~
               keys(hex(000104050d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L40640
                  call "MANUAL" ("WCREALIN")
                  goto L40190

L40640:        if keyhit% <> 15 then L40680
                  call "PRNTSCRN"
                  goto L40190

L40680:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50200,         /* Work Centere      */     ~
                              L50400          /* New 24 hr units   */
            return

L50200: REM Test for WC To Change                 WCTOCHNG$
            call "GETCODE" (#1, wctochng$, wcdescr$, 0%, 0, f1%(1))
L50220:         if f1%(1) = 1 then L50260
            errormsg$ = "Invalid Work Center"
            return
        REM Read Data for Input/Edit Screen
L50260:     wcreadkey$ = wctochng$ & " "
            call "READ100" (#1,wcreadkey$,f1%(1))
               if f1%(1) = 0 then L50220
            get #1, using L50300, wcperday%(), wcdaily
L50300:     FMT XX(36), 7*BI(2), XX(1969), BI(2)
            oldwcdaily = wcdaily
            convert oldwcdaily to oldwcdaily$,pic(######)
            for j% = 1% to 7%
                oldwcperday%(j%) = wcperday%(j%)
                convert oldwcperday%(j%) to oldwcperday$(j%),pic(#####)
            next j%

            hrs = 24 / oldwcdaily
            mins = hrs * 60
            call "CONVERT" (hrs, -2.4, hrs$)
            call "CONVERT" (mins, -2.4, min$)
            currunit$ = hrs$ & " Hours (" & min$ & " minutes)"

            if newwcdaily$ = " " then return
*          FIELDNR% = 2%

L50400: REM Test For New 24 hr units
            call "NUMTEST" (newwcdaily$, 0, 65000, errormsg$, -0.01,     ~
                            newunits)
            if errormsg$ <> " " then return
            mulfact = round(newunits / oldwcdaily, 4)
            for j% = 1% to 7%
                newwcperday%(j%) = round(oldwcperday%(j%) * mulfact, 0)
                convert newwcperday%(j%) to newwcperday$(j%), pic(#####)
            next j%
            call "CONVERT" (mulfact, 2.4, mulfact$)

            hrs = 24 / newunits
            mins = hrs * 60
            call "CONVERT" (hrs, -2.4, hrs$)
            call "CONVERT" (mins, -2.4, min$)
            newunit$ = hrs$ & " Hours (" & min$ & " minutes)"

            return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
