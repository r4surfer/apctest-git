        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB    QQQ   V   V   AAA   L      U   U  EEEEE   *~
            *    J    B   B  Q   Q  V   V  A   A  L      U   U  E       *~
            *    J    BBBB   Q   Q  V   V  AAAAA  L      U   U  EEEE    *~
            *  J J    B   B  Q Q Q   V V   A   A  L      U   U  E       *~
            *   J     BBBB    QQQ     V    A   A  LLLLL   UUU   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBQVALUE - Displays contents of JBVALUE2.  Routine may be *~
            *            called for Labor, Miscellaneous, or Work Center*~
            *            cost details.                                  *~
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
            * 07/07/87 ! Original                                 ! RN2 *~
            * 05/01/88 ! Added Tab Stops On Line Items            ! MDE *~
            * 03/28/89 ! Removed reference to Cost Report on PF14 ! MJB *~
            *          !  - it was never there and still isn't!   !     *~
            * 05/15/93 ! Access to core ledgers/Closing Adj       ! KAB *~
            * 02/27/95 ! PRR 13268 - JBVALUE2 Records now corrctly! RJH *~
            *          !  have Posting Date and transaction date. !     *~
            *          !  Screens now display posting date & trans!     *~
            *          !  date.                                   !     *~
            * 02/28/95 ! PRR 13236,13237,13238-now display job    ! RJH *~
            *          !  part number in screen header line 2 as  !     *~
            *          !  room allows.                            !     *~
            * 01/18/96 ! Fixed GET of UserID.                     ! JDH *~
            * 09/05/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "JBQVALUE"   (job$,          /* Job to display data for    */~
                          type$,         /* Record Type (L, M, or W)   */~
                          #2,            /* SYSFILE2                   */~
                          #10, #11,      /* JBMASTR2, JBVALUE2         */~
                          #12, #4  )     /* JBMATER2, HNYMASTR         */

        dim                                                              ~
            amts(12),                    /* Work Array                 */~
            bhdr$(3)20,                  /* Cost Bucket Headers        */~
            bucket_amts$(12)12,          /* Cost Bucket Values         */~
            bucket_descrs$(12)20,        /* Cost Bucket Descriptions   */~
            bucket_ids$(12)10,           /* Cost Bucket IDs            */~
            code$12,                     /* Code for Range Test        */~
            codes$(4)12, codes_save$48,  /* Code Range to Display      */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dates$(4)10, dates_save$40,  /* Date Range to Display      */~
            dfac$1,                      /* Display FACs               */~
            display$(13)183,             /* Display Lines              */~
            errormsg$79,                 /* Error message              */~
            hdr1$79, hdr2$79,            /* Display Headings           */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            job$8, job_descr$30,         /* Job Number                 */~
            last$79,                     /* Last Display Line          */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line1$79,                    /* 1st Line of Screen Header  */~
            line2$79,                    /* 2nd Line of Screen Header  */~
            part$25,                     /* Job Part Number            */~
            pf$(3)79, pfkeys$20,         /* PF Descriptors & Keys      */~
            plowkey$50, plowkey1$50,     /* File Plow Key, Totals Plow */~
            range$12,                    /* Code Range Descriptor      */~
            set$8, set_descr$30,         /* Current Cost Set           */~
                   set_id$4,             /*                            */~
            trans_date$6,                /* Transaction Date           */~
            type$1                       /* Record Type to Display     */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 4 ! HNYMASTR ! Parts Master File                        *~
            * #10 ! JBMASTR2 ! Production job master file               *~
            * #11 ! JBVALUE2 ! Production job value added detail file   *~
            * #12 ! JBMATER2 ! Production Job Materials Ledger          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date : call "DATEFMT" (date$)

*       * Get Job Description
            call "READ100" (#10, job$, f1%(10))
*          CALL "GETCODE" (#10, JOB$, JOB_DESCR$, 0%, .30, F1%(10))
            if f1%(10) = 0% then exit_program
            get #10 using L09105, job_descr$, part$
L09105:        FMT POS(9), CH(30), POS(58), CH(25)
            str(line2$,,61) = "Job:" & job$ & "(" & job_descr$ & "):"  & ~
                                 part$
            str(line2$,62) = "JBQVALUE: " & str(cms2v$,,8)

*       * Get Cost Set and Bucket Info
            set$ = " "
            call "STCSETID" (3%, #2, set$, set_id$, bucket_ids$(),       ~
                             bucket_descrs$(), set_descr$)
            bhdr$(1) = "Bucket ID"
            bhdr$(2) = "Bucket Description"
            bhdr$(3) = "      Amount"

            dates$(1) = "ALL"         :  dates$(2) = " "
            dates$(3) = all(hex(00))  :  dates$(4) = all(hex(ff))

            codes$(1) = "ALL"         :  codes$(2) = " "
            codes$(3) = all(hex(00))  :  codes$(4) = all(hex(ff))

*       * Do Set-ups Specific to Record Type
            on pos("LWMCXYZF" = type$) goto L09320, L09440, L09560, L09600,      ~
                                            L09640, L09680, L09720, L09760
            goto exit_program  /* Invalid Type Passed In */

L09320
*         Type 'L'- LABOR
            line1$ = "Job Labor Transactions Display"
            range$ = "Employees"
            code%  = 12%
            hdr1$  = " Trans.                Work  Activity La"  &       ~
                     "bor                         Transaction"
            hdr2$  = "  Date   Employee     Center   Code   Cl"  &       ~
                     "ass    Rate/Hour      Hours     Value  "
                  /* "MM/DD/YY EEEEEEEEEEEE  WWWW    AAAA    C"
                     "CCC   ########## ########## ###########"  */
            goto L09950

L09440
*         Type 'W'- Work Center Costs
            line1$ = "Job Work Center Costs Display"
            range$ = "Work Centers"
            code%  = 4%
            hdr1$  = "  Post    Work   Activity    Trans.  Use"  &       ~
                     "r                           Transaction"
            hdr2$  = "  Date   Center    Code      Date     ID"  &       ~
                     "       Rate/Hour      Hours     Value  "
                  /* "MM/DD/YY  WWWW     AAAA    MM/DD/YY   UU"
                     "U     ########## ########## ###########"  */
            goto L09950

L09560
*         Type 'M'- Miscellaneous Cost Details
            line1$ = "Job Miscellaneous Cost Details Display"
            goto L09850

L09600
*         Type 'C'- Closing Adjustments
            line1$ = "Job Closing Adjustments Details Display"
            goto L09850

L09640
*         Type 'X'- Core Indirect Material Value
            line1$ = "Job Core Indirect Material Value Display"
            goto L09772

L09680
*         Type 'Y'- Core Credit Transactions
            line1$ = "Job Core Credit Transactions Display"
            goto L09772

L09720
*         Type 'Z'- Core Closing Adjustments Display
            line1$ = "Job Core Closing Adjustments Display"
            goto L09850

L09760
*         Type 'F'- Core Credit Memo Details Display
            line1$ = "Job Core Credit Memo Details Display"

L09772
*       * Core 'Zoned Text' Header
            hdr1$  = "  Post     Posting Text:                "  &       ~
                     "               Trans. User  Transaction"
            hdr2$  = "  Date     Code Part Code               "  &       ~
                     "   Quantity    Date    ID       Value  "
                  /* "MM/DD/YY   C??: PPPPPPPPPPPPPPPPPPPPPPPP"
                     "PQQQQQQQQQQ  MM/DD/YY  UUU  ###########"  */

            goto L09920

*       * Continue with misc-type display
L09850:     hdr1$  = "  Post                                  "  &       ~
                     "               Trans. User  Transaction"
            hdr2$  = "  Date     Posting Text                 "  &       ~
                     "               Date    ID       Value  "
                  /* "MM/DD/YY   TTTTTTTTTTTTTTTTTTTTTTTTTTTTT"
                     "TTTTTTTTTTT  MM/DD/YY  UUU  ###########"  */

L09920
*       ** And Range Stuff

            range$ = " "
            code%  = 1%
            goto L09950

L09950
*       *** Continue with non-type specific initializations
            str(line1$,66) = "Date: " & date$
            gosub totals
            goto  first_screen

        REM *************************************************************~
            *       S C R E E N   C O N T R O L   L O G I C             *~
            *-----------------------------------------------------------*~
            * Handles display screens (Input & Displays).               *~
            *************************************************************

        main_display
            inpmessage$ = "Position Cursor and Press PF-12 to see" &     ~
                          " Breakdown of Transaction Value."
            gosub'101(0%)                /* Display Screen             */
                errormsg$   = " "
                if keyhit%  =  2% then first_screen
                if keyhit%  =  5% then next_screen
                if keyhit%  =  8% then get_ranges
                if keyhit%  = 12% then cost_breakdown
                if keyhit%  = 16% then exit_program
                                  goto main_display

        get_ranges
            inpmessage$ = "Enter Ranges to display and press RETURN."
            dates_save$ = str(dates$())
            codes_save$ = str(codes$())
L10230:     gosub'101(1%)               /* Display & Accept Screen     */
                if keyhit%  =  1% then reset_ranges
                if keyhit% <>  0% then L10230
            gosub'151                   /* Edit Field for Valid Entry  */
                if errormsg$ <> " " then L10230
                gosub totals
                goto  first_screen

            reset_ranges
                str(dates$()) = dates_save$
                str(codes$()) = codes_save$
                goto main_display


        cost_breakdown
            inpmessage$ = "Press RETURN to return to Main Display."
            line% = cursor%(1) - 5%
            if line% < 1% or line% > l% then main_display
            gosub'102
            goto main_display


        first_screen
            plowkey$ = str(job$,,8) & str(type$,,1) & dates$(3)
            first%   = 1%
            gosub load_data
            goto  main_display

        next_screen
            first% = 0%
            gosub load_data
            goto  main_display

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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        load_data
            l%, last% = 0%
            str(last$,,20) = " "
            init(" ") display$()

          load_loop
            call "PLOWNEXT" (#11, plowkey$, 9%, f1%(11))
            if f1%(11) = 1% then L30170
                last% = 1%
                str(last$,,20) = "** End of Display **"
                goto exit_load

L30170:     get #11 using L30180, trans_date$
L30180:         FMT POS(24), CH(6)
            if trans_date$ > dates$(4) then load_loop

            if pos("LW" = type$) = 0% then L30280
                if type$ = "L" then get #11 using L30240, code$           ~
                               else get #11 using L30250, code$
L30240:              FMT POS(181), CH(12)
L30250:              FMT POS(177), CH( 4)
                if code$ < codes$(3) or code$ > codes$(4) then load_loop

L30280:     if l% < 13% then L30320
                str(plowkey$,,23) = addc all(hex(ff))  /* Back Up One */
                goto exit_load

L30320:     l% = l% + 1%
            get #11 using L30340, trans_amt
L30340:         FMT POS(33), PD(14,4)
            call "CONVERT" (trans_amt, 2.2, str(display$(l%),69,11))
            get #11 using L30370, str(display$(l%),80,104) /* Costs     */
L30370:         FMT POS(33), CH(104)
            str(display$(l%),,8) = trans_date$
            call "DATEFMT" (str(display$(l%),,8))
            on pos("LWMCXYZF" = type$) goto L30420, L30620, L30741, L30741,  ~
                                            L30741, L30741, L30741, L30741

L30420:   /* Labor   */
            get #11 using L30480, str(display$(l%),24, 4), /* WC        */~
                                 str(display$(l%),10,12), /* Employee  */~
                                 str(display$(l%),40, 4), /* Lbr Class */~
                                 str(display$(l%),32, 4), /* Activity  */~
                                 rate, hours
L30480:         FMT POS(177), CH(4), CH(12), POS(205), CH(4), POS(215),  ~
                    CH(4), 2*PD(14,4)
            call "CONVERT" (rate , 4.4, str(display$(l%),47,10))
            call "CONVERT" (hours, 2.2, str(display$(l%),58,10))
            goto load_loop

L30620:   /* WC      */
            get #11 using L30680, str(display$(l%),  , 6), /* Post Date */~
                                 str(display$(l%),39, 3), /* User ID   */~
                                 str(display$(l%),11, 4), /* WC Code   */~
                                 str(display$(l%),20, 4), /* Activity  */~
                                 rate, hours
L30680:         FMT POS(10), CH(6), POS(30), CH(3), POS(177), CH(4),     ~
                    POS(215), CH(4), 2*PD(14,4)
            str(display$(l%),28,6) = trans_date$
            call "DATEFMT" (str(display$(l%),,8))
            call "DATEFMT" (str(display$(l%),28,8))
            call "CONVERT" (rate , 4.4, str(display$(l%),47,10))
            call "CONVERT" (hours, 2.2, str(display$(l%),58,10))
            goto load_loop

L30741:   /* Misc & Misc Type */
            get #11 using L30745, str(display$(l%),  , 6), /* Post Date */~
                                 str(display$(l%),64, 3), /* User ID   */~
                                 str(display$(l%),12,40)  /* Text      */
L30745:         FMT POS(10), CH(6), POS(30), CH(3), POS(137), CH(40)
            str(display$(l%),54,6) = trans_date$
            call "DATEFMT" (str(display$(l%),,8))
            call "DATEFMT" (str(display$(l%),54, 8))
            goto load_loop

          exit_load
            str(display$(13),68,1) = hex(a4)
            if pos("LW" = type$) <> 0% then                              ~
                                         str(display$(13),57,1) = hex(a4)
            return

        totals  /* Sum value for display criteria  */
            count%, units, total = 0
            plowkey1$ = str(job$,,8) & str(type$,,1) & dates$(3)

          totals_loop
            call "PLOWNEXT" (#11, plowkey1$, 9%, f1%(11))
            if f1%(11) = 1% then L30950
                put last$ using L30880, count%
L30880:              FMT POS(25), "Totals: ", PIC(#####), " Transactions"
                if pos("LW" = type$) <> 0% then                          ~
                            call "CONVERT" (units, 2.2, str(last$,58,10))~
                                else str(last$,58,10) = " "
                call "CONVERT" (total, 2.2, str(last$,69,11))
                return

L30950:     get #11 using L30960, trans_date$
L30960:         FMT POS(24), CH(6)
            if trans_date$ > dates$(4) then totals_loop

            if pos("LW" = type$) = 0% then L31060
                if type$ = "L" then get #11 using L31020, code$, hours    ~
                               else get #11 using L31030, code$, hours
L31020:              FMT POS(181), CH(12), POS(227), PD(14,4)
L31030:              FMT POS(177), CH( 4), POS(227), PD(14,4)
                if code$ < codes$(3) or code$ > codes$(4) then totals_loop

L31060:     count% = count% + 1%
            get #11 using L31080, trans_amt
L31080:         FMT POS(33), PD(14,4)
            units = units + hours
            total = total + trans_amt
            goto totals_loop

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(edit%) /* EDIT%  0% = Display  1% = Get Ranges       */
            on edit% + 1% goto L40090, L40270

L40090
*        Set up for Display
            init(hex(84))  lfac$()
            dfac$ = hex(86)
            if pos("LW" = type$) = 0% then lfac$(2) = hex(9c)
            pf$(1) = "( 2)First Screen !(8)Display Ranges     " &        ~
                     "                     ! (13)Instructions"
            pf$(2) = "( 5)Next Screen  ! Trans. Dates  MM/DD/Y" &        ~
                     "Y    to MM/DD/YY     ! (15)Print Screen"
            pf$(3) = "                 ! RRRRRRRRRRRR x       " &        ~
                     "     to x            ! (16)Exit Display"
            str(pf$(3),20,12) = range$
            if pos("LW" = type$) = 0% then str(pf$(3),46,2) = " "
            pfkeys$  = hex(ff02ffff05ffff08ffffff0c0dff0f10)
            if first% = 0% then L40230
                str(pf$(1),,16) = " "  :  str(pfkeys$,2,1) = hex(ff)
L40230:     if last%  = 0% then L40410
                str(pf$(2),,16) = " "  :  str(pfkeys$,5,1) = hex(ff)
                goto L40410

L40270
*        Set up for Range Entry
            dfac$ = hex(8c) : init(hex(81)) lfac$()
            if pos("LW" = type$) = 0% then lfac$(2) = hex(9c)
            pf$(1) = "( 1)Exit Changes !Enter Values, 'ALL', '" &        ~
                     "FIRST', or 'LAST'... ! (13)Instructions"
            pf$(2) = "                 ! Trans. Dates  MM/DD/Y" &        ~
                     "Y    to MM/DD/YY     ! (15)Print Screen"
            pf$(3) = "                 ! RRRRRRRRRRRR x       " &        ~
                     "     to x            ! (16)Exit Display"
            str(pf$(3),20,12) = range$
            if pos("LW" = type$) = 0% then str(pf$(3),46,2) = " "
            pfkeys$ = hex(01ffffffffffffffffffffff0d0e0f1000)


L40410:     accept                                                       ~
               at (01,02), fac(hex(8c)), line1$                 , ch(79),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), fac(hex(8c)), hdr1$                  , ch(79),~
               at (05,02), fac(hex(ac)), hdr2$                  , ch(79),~
                                                                         ~
               at (06,02), fac(dfac$  ), str(display$( 1),,79)  , ch(79),~
               at (07,02), fac(dfac$  ), str(display$( 2),,79)  , ch(79),~
               at (08,02), fac(dfac$  ), str(display$( 3),,79)  , ch(79),~
               at (09,02), fac(dfac$  ), str(display$( 4),,79)  , ch(79),~
               at (10,02), fac(dfac$  ), str(display$( 5),,79)  , ch(79),~
               at (11,02), fac(dfac$  ), str(display$( 6),,79)  , ch(79),~
               at (12,02), fac(dfac$  ), str(display$( 7),,79)  , ch(79),~
               at (13,02), fac(dfac$  ), str(display$( 8),,79)  , ch(79),~
               at (14,02), fac(dfac$  ), str(display$( 9),,79)  , ch(79),~
               at (15,02), fac(dfac$  ), str(display$(10),,79)  , ch(79),~
               at (16,02), fac(dfac$  ), str(display$(11),,79)  , ch(79),~
               at (17,02), fac(dfac$  ), str(display$(12),,79)  , ch(79),~
               at (18,02), fac(dfac$  ), str(display$(13),,79)  , ch(79),~
               at (19,02), fac(dfac$  ), last$                  , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)), pf$(1)                 , ch(79),~
               at (23,02), fac(hex(8c)), pf$(2)                 , ch(79),~
               at (24,02), fac(hex(8c)), pf$(3)                 , ch(79),~
               at (23,34), fac(lfac$(1)), dates$(1)                     ,~
               at (23,50), fac(lfac$(1)), dates$(2)                     ,~
               at (24,34), fac(lfac$(2)), str(codes$(1),,code%)         ,~
               at (24,50), fac(lfac$(2)), str(codes$(2),,code%)         ,~
                     keys(pfkeys$), key (keyhit%)

            if keyhit% <> 13 then L40780
                call "MANUAL" ("JBQVALUE")
                goto L40410

L40780:     if keyhit% <> 15 then L40820
                call "PRNTSCRN"
                goto L40410

L40820:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Cost Breakdown Screen.                                    *~
            *************************************************************

        deffn'102
            init (" ") bucket_amts$()
            get display$(line%) using L41090, amts()
L41090:         FMT POS(88), 12*PD(14,4)
            for b% = 1% to 12%
                if amts(b%) <> 0 then                                    ~
                     call "CONVERT" (amts(b%), 4.4, bucket_amts$(b%))
            next b%

L41150:     accept                                                       ~
               at (01,02), fac(hex(8c)), line1$                 , ch(79),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), fac(hex(8c)), hdr1$                  , ch(79),~
               at (05,02), fac(hex(ac)), hdr2$                  , ch(79),~
               at (06,02), fac(hex(84)), str(display$(line%),,79),       ~
                                                                         ~
               at (08,17), fac(hex(ac)), bhdr$(1)               , ch(10),~
               at (08,30), fac(hex(ac)), bhdr$(2)               , ch(20),~
               at (08,52), fac(hex(ac)), bhdr$(3)               , ch(12),~
                                                                         ~
               at (09,17), fac(hex(84)), bucket_ids$   ( 1)     , ch(20),~
               at (10,17), fac(hex(84)), bucket_ids$   ( 2)     , ch(20),~
               at (11,17), fac(hex(84)), bucket_ids$   ( 3)     , ch(20),~
               at (12,17), fac(hex(84)), bucket_ids$   ( 4)     , ch(20),~
               at (13,17), fac(hex(84)), bucket_ids$   ( 5)     , ch(20),~
               at (14,17), fac(hex(84)), bucket_ids$   ( 6)     , ch(20),~
               at (15,17), fac(hex(84)), bucket_ids$   ( 7)     , ch(20),~
               at (16,17), fac(hex(84)), bucket_ids$   ( 8)     , ch(20),~
               at (17,17), fac(hex(84)), bucket_ids$   ( 9)     , ch(20),~
               at (18,17), fac(hex(84)), bucket_ids$   (10)     , ch(20),~
               at (19,17), fac(hex(84)), bucket_ids$   (11)     , ch(20),~
               at (20,17), fac(hex(84)), bucket_ids$   (12)     , ch(20),~
                                                                         ~
               at (09,30), fac(hex(84)), bucket_descrs$( 1)     , ch(20),~
               at (10,30), fac(hex(84)), bucket_descrs$( 2)     , ch(20),~
               at (11,30), fac(hex(84)), bucket_descrs$( 3)     , ch(20),~
               at (12,30), fac(hex(84)), bucket_descrs$( 4)     , ch(20),~
               at (13,30), fac(hex(84)), bucket_descrs$( 5)     , ch(20),~
               at (14,30), fac(hex(84)), bucket_descrs$( 6)     , ch(20),~
               at (15,30), fac(hex(84)), bucket_descrs$( 7)     , ch(20),~
               at (16,30), fac(hex(84)), bucket_descrs$( 8)     , ch(20),~
               at (17,30), fac(hex(84)), bucket_descrs$( 9)     , ch(20),~
               at (18,30), fac(hex(84)), bucket_descrs$(10)     , ch(20),~
               at (19,30), fac(hex(84)), bucket_descrs$(11)     , ch(20),~
               at (20,30), fac(hex(84)), bucket_descrs$(12)     , ch(20),~
                                                                         ~
               at (09,52), fac(hex(84)), bucket_amts$  ( 1)     , ch(12),~
               at (10,52), fac(hex(84)), bucket_amts$  ( 2)     , ch(12),~
               at (11,52), fac(hex(84)), bucket_amts$  ( 3)     , ch(12),~
               at (12,52), fac(hex(84)), bucket_amts$  ( 4)     , ch(12),~
               at (13,52), fac(hex(84)), bucket_amts$  ( 5)     , ch(12),~
               at (14,52), fac(hex(84)), bucket_amts$  ( 6)     , ch(12),~
               at (15,52), fac(hex(84)), bucket_amts$  ( 7)     , ch(12),~
               at (16,52), fac(hex(84)), bucket_amts$  ( 8)     , ch(12),~
               at (17,52), fac(hex(84)), bucket_amts$  ( 9)     , ch(12),~
               at (18,52), fac(hex(84)), bucket_amts$  (10)     , ch(12),~
               at (19,52), fac(hex(84)), bucket_amts$  (11)     , ch(12),~
               at (20,52), fac(hex(84)), bucket_amts$  (12)     , ch(12),~
                                                                         ~
               at (22,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,65), "(13)Instructions",                           ~
               at (24,65), "(15)Print Screen",                           ~
                     keys(hex(000d0f10)), key(keyhit%)

            if keyhit% <> 13 then L41760
                call "MANUAL" ("JBQVALUE")
                goto L41150

L41760:     if keyhit% <> 15 then return
                call "PRNTSCRN"
                goto L41150


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test Ranges.                                              *~
            *************************************************************

        deffn'151
            errormsg$ = " "

*        First Test Date Ranges
            if len(dates$(1)) <> 6% and len(dates$(1)) <> 10% then L50140
                call "DATEOKC" (dates$(1), u3%, errormsg$)
                if errormsg$ <> " " then return
L50140:     if len(dates$(2)) <> 6% and len(dates$(2)) <> 10% then L50180
                call "DATEOKC" (dates$(2), u3%, errormsg$)
                if errormsg$ <> " " then return
                call "DATUFMTC" (dates$(1))
                call "DATUFMTC" (dates$(2))
L50180:     call "TESTRNGE" (dates$(1), dates$(2), dates$(3), dates$(4), ~
                             errormsg$)
                call "DATFMTC" (dates$(1))
                call "DATFMTC" (dates$(2))
            if errormsg$ <> " " then return

*        Test Code Ranges
            if pos("LW" = type$) = 0% then return
            call "TESTRNGE" (codes$(1), codes$(2), codes$(3), codes$(4), ~
                             errormsg$)
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
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            end
