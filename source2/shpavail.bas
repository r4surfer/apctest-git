        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   H   H  PPPP    AAA   V   V   AAA   IIIII  L       *~
            *  S      H   H  P   P  A   A  V   V  A   A    I    L       *~
            *   SSS   HHHHH  PPPP   AAAAA  V   V  AAAAA    I    L       *~
            *      S  H   H  P      A   A   V V   A   A    I    L       *~
            *   SSS   H   H  P      A   A    V    A   A  IIIII  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SHPAVAIL - SELECTS (FROM BCKLINES) ORDERS THAT FALL WITHIN*~
            *            THE OPERATOR-ENTERED RANGES OF VALUES AND THEN *~
            *            PRINTS A REPORT OF ORDERS THAT ARE AVAILABLE   *~
            *            FOR SCHEDULING.                                *~
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
            * 09/12/86 ! Original                                 ! JIM *~
            * 04/16/87 ! Corrected SLCTSORT key length param      ! JIM *~
            * 09/20/88 ! Added flexibility to sort key.           ! JIM *~
            * 09/22/89 ! Added Export order indicator.            ! JDH *~
            * 05/24/90 ! Chngd column literal 'Available' to 'Not ! JDH *~
            *          !  Shipped or Scheduled', added sort optns.!     *~
            *          !  Now includes qty < 1 and > 0.           !     *~
            * 03/26/92 ! Corrected image statement ln 61900       ! MJB *~
            * 10/20/92 ! Removed FACs from page zero.             ! JIM *~
            * 11/03/93 ! Added Customer's shipping Priority Code. ! MLJ *~
            * 08/23/96 ! Century date conversion                  ! DER *~
            *          ! Modified to handle internal fmt dates    !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            bck_ship$6,                  /* Ship date from BCKLINES    */~
            bckmastr_key$25,             /* Key to BCKMASTR            */~
            blankdate$8,                 /* blank unfmt date           */~
            brk_cust$9,                  /* Controls break on Customer */~
            brk_name$30,                 /* Name of customer for breaks*/~
            brk_regn$9,                  /* Controls break on Region   */~
            brk_sord$16,                 /* Controls break on SO #     */~
            brk_stor$3,                  /* Controls break on Store    */~
            company_name$60,             /* Company name from COMPNAME */~
            cursor%(2),                  /* Cursor location for edit   */~
            cust_code$9,                 /* Key to CUSTOMER            */~
            cust_dest$30,                /* Ship to City/State/Zip     */~
            cust_name$30,                /* Name of Customer           */~
            date$8,                      /* Date for screen display    */~
            earl_date$8,                 /* Earliest BCKLINES date     */~
            edtmessage$79,               /* Edit screen message        */~
            eodsw$1,                     /* Indicates end of data      */~
            errormsg$79,                 /* Error message              */~
            export$1,                    /* Export order flag          */~
            from_date$6,                 /* from date intl fmt         */~
            from_regn$9,                 /* Low region for selection   */~
            from_stor$3,                 /* Low store for selection    */~
            hdr$40,                      /* ASKUSER constant           */~
            high_date$10,                /* High date for capture      */~
            high_regn$9,                 /* High region for capture    */~
            high_stor$3,                 /* High store for capture     */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            late_date$8,                 /* Latest BCKLINES date       */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            low_date$10,                 /* Low date for capture       */~
            low_regn$9,                  /* Low region for capture     */~
            low_stor$3,                  /* Low store for capture      */~
            msg$(3)79,                   /* ASKUSER constants          */~
            one_time$1,                  /* Controls one-time logic    */~
            date_hdr$40,                 /* Edited dates               */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            regn_code$9,                 /* Region code from CUSTOMER  */~
            regn_name$30,                /* Region name from GENCODES  */~
            rptid$6,                     /* Report ID                  */~
            scode$1,                     /* Shipping Priority Code     */~
            shipcode$1,                  /* Shipping Priority Code     */~
            skipsw$1,                    /* Controls print skipping    */~
            sord_nbr$16,                 /* Sales Order number         */~
            sort_code$1,                 /* Sort Order for report      */~
            sortkey$44,                  /* Key to WORKFILE            */~
            stor_code$3,                 /* Store code from CUSTOMER   */~
            stor_name$32,                /* Store name from BCKMASTR   */~
            sub_hdr$80,                  /* Report title & dates       */~
            time$8,                      /* Time of day stamp          */~
            to_date$6,                   /* to date intl fmt           */~
            to_regn$9,                   /* End region for selection   */~
            to_stor$3,                   /* End store for selection    */~
            userid$3,                    /* Current User Id            */~
            work_fil$8,                  /* Name of WORKFILE           */~
            work_lib$8,                  /* Library of WORKFILE        */~
            work_vol$6                   /* Volume of WORKFILE         */

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
            * #3  ! BCKLINES ! BACK LOG LINE ITEM FILE                  *~
            * #4  ! BCKMASTR ! BACKLOG MASTER FILE (GET STORE NUMBER)   *~
            * #6  ! CUSTOMER ! CUSTOMER MASTER FILE                     *~
            * #7  ! STORNAME ! STORE INFORMATION FILE                   *~
            * #8  ! GENCODES ! GENERAL CODES FILE                       *~
            * #9  ! WORKFILE ! Temporary System Workfile                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #3,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19                      ~

            select #4,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key 1,  keypos = 26,   keylen =  16

            select #6,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key 1,  keypos = 10,   keylen =  30,         ~
                            key 2,  keypos = 424,  keylen =   9,         ~
                            key 3,  keypos = 771,  keylen =   9,         ~
                            key 4,  keypos = 780,  keylen =   9

            select #7,  "STORNAME", varc, indexed, recsize = 300,        ~
                     keypos = 1, keylen = 3
            select #8,  "GENCODES", varc, indexed, recsize = 128,        ~
                     keypos = 1, keylen = 24
            select #9,  "WORKFILE",                                      ~
                        consec,  recsize =  184

            call "SHOSTAT" ("Opening Files, One Moment Please")

                rslt$(3 ) = "REQUIRED"
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            get rslt$(3) using L02450, bck_recs%
L02450:         FMT POS(17), BI(4)

                rslt$(4 ) = "REQUIRED"
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))
                rslt$(6 ) = "REQUIRED"
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ), 0%, rslt$(6 ))
                rslt$(7 ) = "REQUIRED"
            call "OPENCHCK" (#7,  fs%(7 ), f2%(7 ), 0%, rslt$(7 ))
                rslt$(8 ) = "REQUIRED"
            call "OPENCHCK" (#8,  fs%(8 ), f2%(8 ), 0%, rslt$(8 ))

            if min(fs%()) < 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            if bck_recs% > 0% then L09140
L09070:         comp% = 2%
                hdr$ = "*** CANCEL REQUEST ***"
                msg$(1) = "There is no Sales Order data to analyze"
                msg$(3) = "Press RETURN to cancel program"
                call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
                if comp% <> 0% then L09070
                goto exit_program /* NOTHING TO DO; DON'T DO ANYTHING */
L09140:     bck_recs% = max(50%, (bck_recs% * .3))
            call "COMPNAME" (12%, company_name$, comp%)
            rptid$ = "SHP001"
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$="(4)Previous Field" : pf5$=" ": pf16$="(16)Exit Program"
            init(" ") errormsg$, inpmessage$, high_date$, high_regn$,    ~
                high_stor$, sort_code$
            low_date$, low_regn$, low_stor$ = "ALL"
            sub_hdr$ = "ORDERS AVAILABLE FOR SCHEDULING REPORT:"

            for fieldnr% = 1 to  4
L10140:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10260
L10160:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10240
L10190:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10160
                         if fieldnr% = 1% then L10140
                         goto L10190
L10240:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10160
L10260:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10160
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            inpmessage$ = edtmessage$
            pf4$  = " "
            pf5$  = " "
            pf16$ = "(16)Print Data"
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       edtpg1
L11150:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  5 then edtpg1
            if fieldnr% = 4 then edtpg1
            if fieldnr% = 5 then fieldnr% = 4
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf4$, pf5$, pf16$ = " "
L11200:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11200
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11200
                  if fieldnr% = 4 then fieldnr% = 5
                  if cursor%(1) - 5% <> fieldnr% then L11150
            goto edtpg1

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "SHOSTAT" ("Records are being selected " &              ~
                "... Please stand by")
            plowkey$ = xor plowkey$
            nbr_recs% = 0% : one_time$, eodsw$ = "0"
            call "WORKOPN2" (#9, "OUTPT", bck_recs%, f2%(9))
            call "GETNAMES" addr(#9, work_fil$, work_lib$, work_vol$)
        plow_thru_bcklines
            call "PLOWNEXT" (#3, plowkey$, 0%, f1%(3))
            if f1%(3) = 0% then sort_work_file         /* END OF FILE */
            get #3 using L13230, sord_nbr$
L13230:         FMT POS(10), CH(16)
            if one_time$ = "1" then L13270
                gosub master_reset
                one_time$ = "1"
L13270:     if sord_nbr$ <> brk_sord$ then gosub test_bckmastr
            get #3 using L13320, cust_code$, bck_qopn, bck_qsch,          ~
                           bck_qpreinv, bck_uprc, bck_disc, bck_ship$

        REM RECORD LAYOUT FOR FILE 'BCKLINES' ***************************
L13320:         FMT  CH(9),               /* Customer code             */~
                     POS(109), PD(14, 4), /* Open Order Quantity       */~
                     POS(117), PD(14, 4), /* Scheduled Ship Quantity   */~
                     POS(133), PD(14, 4), /* Preinvoiced Ship Quantity */~
                     POS(141), PD(14, 4), /* Unit Price at Stocking UOM*/~
                     POS(173), PD(14, 4), /* Line item discount percent*/~
                     POS(212), CH(6)      /* Scheduled Ship Date       */

            if bck_ship$ < from_date$ then plow_thru_bcklines
            if bck_ship$ > to_date$ then plow_thru_bcklines
            bck_qopn = bck_qopn - bck_qsch - bck_qpreinv
            if bck_qopn <= 0 then plow_thru_bcklines
            nbr_lines% = nbr_lines% + 1%
            temp = (bck_qopn * bck_uprc)
            disc = (temp * bck_disc) / 100
            availdollars = availdollars + temp - disc
            if bck_ship$ < earl_date$ then earl_date$ = bck_ship$
            if bck_ship$ > late_date$ then late_date$ = bck_ship$
            goto plow_thru_bcklines

        test_bckmastr
            if nbr_lines% = 0% then goto master_reset
            bckmastr_key$ = str(cust_code$,,9) & brk_sord$
            call "READ100" (#4, bckmastr_key$, f1%(4))
            if f1%(4) = 0% then goto master_reset
            get #4 using L13600, cust_name$, cust_dest$, stor_code$,      ~
                                export$, odisc

            cust_dest$ = str(cust_dest$,,15) & str(cust_dest$, 18)
            if export$ = "Y" then export$ = "X" else export$ = " "

        REM RECORD LAYOUT FOR FILE 'BCKMASTR' ***************************
L13600:         FMT  POS(42), CH(30),    /* Ship-to Name               */~
                     XX(120), CH(30),    /* Ship-to City/State/Zip     */~
                     POS(803), CH(3),    /* Store code                 */~
                     POS(857), CH(1),    /* Export Flag                */~
                     POS(859), PD(14,4)  /* Order discount percent     */

            if stor_code$ < from_stor$ then goto master_reset
            if stor_code$ > to_stor$ then goto master_reset
            call "READ100" (#6, cust_code$, f1%(6))
            if f1%(6) = 0% then goto master_reset
            get #6 using L14000, regn_code$, shipcode$
L14000:         FMT  POS(603), CH(9), /* Shipping Region code          */~
                     POS(733), CH(1)  /* Shipping Priority Code        */
            if shipcode$ = " " then shipcode$ = "3"
            if regn_code$ < from_regn$ then goto master_reset
            if regn_code$ > to_regn$ then goto master_reset
            sortkey$ = " "
            str(sortkey$,  1,  3) = stor_code$
            if sort_code$ <> "1" then L14042
                str(sortkey$,  4,  9) = regn_code$
                str(sortkey$, 13,  9) = cust_code$
                str(sortkey$, 22,  6) = bck_ship$
                str(sortkey$, 28, 16) = brk_sord$
                goto L14070
L14042:     if sort_code$ <> "2" then L14052
L14044:         str(sortkey$,  4,  9) = regn_code$
                str(sortkey$, 13,  9) = cust_code$
                str(sortkey$, 22, 16) = brk_sord$
                goto L14070
L14052:     if sort_code$ <> "3" then L14062
                str(sortkey$,  4,  9) = regn_code$
                str(sortkey$, 13,  6) = bck_ship$
                str(sortkey$, 19,  9) = cust_code$
                str(sortkey$, 28, 16) = brk_sord$
                goto L14070
L14062:     if sort_code$ <> "4" then L14044        /* Shouldn't happen */
                str(sortkey$,  4,  6) = bck_ship$
                str(sortkey$, 10,  9) = cust_code$
                str(sortkey$, 19, 16) = brk_sord$

L14070:     amt = round(availdollars - ((availdollars * odisc) / 100), 2)
            write #9, using L14120, sortkey$, cust_name$, cust_dest$,     ~
                earl_date$, late_date$, amt, nbr_lines%, bck_ship$,      ~
                brk_sord$, export$, regn_code$, cust_code$, shipcode$
L14120:         FMT CH(44), CH(30), CH(30), CH(6), CH(6), 2*PD(14, 4),   ~
                    CH(6), CH(16), CH(1), CH(9), CH(9), CH(1)
            nbr_recs% = nbr_recs% + 1%
        master_reset
            if eodsw$ = "1" then return
            brk_sord$ = sord_nbr$
            init (" ") sortkey$, cust_name$, cust_dest$, stor_code$,     ~
                regn_code$, shipcode$

            late_date$ = blankdate$
            earl_date$ = blankdate$ xor hex(FFFFFFFFFFFF)
            availdollars, nbr_lines% = 0
            return

        sort_work_file
            eodsw$ = "1"
            gosub test_bckmastr
            if nbr_recs% > 0% then sort_continue
L14280:         comp% = 2%
                hdr$ = "*** NULL SET SELECTED ***"
                msg$(1) = "There are no records that satisfy your" &     ~
                     " criteria"
                msg$(3) = "Press RETURN to continue"
                call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
                if comp% <> 0% then L14280
                goto inputmode
        sort_continue
            call "SLCTSORT" (#9, len(str(sortkey$)))

        REM PRINT/DISPLAY LOGIC BEGINS HERE *****************************
            date_hdr$ = "FOR ALL DATES"
            if low_date$ = "ALL" then goto L14450
            if low_date$ = "FIRST" and high_date$ = "LAST" then L14450
                date_hdr$ = "ORDERS DATED FROM " & low_date$ & " TO " &  ~
                     high_date$
L14450:     call "FMTTITLE" (sub_hdr$, date_hdr$, 2%)
            one_time$, eodsw$ = "0"
            call "SHOSTAT" ("Orders Available for Scheduling Report " &  ~
                "in Progress")
            page_nbr% = 0% : line_nbr% = 99%
            t1_nbr%, t1_amt, t2_nbr%, t2_amt, t3_nbr%, t3_amt, rt_nbr%,  ~
                rt_amt, nbr_lines%, availdollars = 0

            select printer(134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            time$ = " " : call "TIME" (time$)

        REM PRINT 'PAGE ZERO' -- THE SELECTION SCREEN *******************
            pagesw% = 1%
            gosub page_0_heading
            print skip (4)
L14586:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then goto L14601
                str(i$(), i%, 1%) = " "
                goto L14586
L14601:     print using L62510, "    ---- SELECTION CRITERIA ----"
            print
            for n% = 6% to 19%
                print using L62510, i$(n%)
            next n%
            pagesw% = 0%

        read_sorted_workfile
            read #9 using L14120, sortkey$, cust_name$, cust_dest$,       ~
                earl_date$, late_date$, availdollars, nbr_lines%,        ~
                bck_ship$, sord_nbr$, export$, regn_code$, cust_code$,   ~
                scode$, eod goto report_total
            get sortkey$ using L15000, stor_code$
L15000:         FMT CH(3)
            if one_time$ = "1" then data_print_setup
                gosub cust_reset
                gosub regn_reset
                gosub store_reset
                one_time$ = "1"

        data_print_setup
            skipsw$ = "0"
            if stor_code$ <> brk_stor$ then gosub store_total
            if regn_code$ <> brk_regn$ then gosub region_total
            if cust_code$ <> brk_cust$ then gosub customer_total
            if skipsw$ = "0" then L15140
                print : line_nbr% = line_nbr% + 1%
L15140:     if line_nbr% > 56% then gosub page_heading
            call "DATEFMT" (earl_date$)
            call "DATEFMT" (late_date$)
            print using L61900, regn_code$, scode$, cust_code$,           ~
                cust_name$, cust_dest$, export$, sord_nbr$, earl_date$,  ~
                late_date$, availdollars, nbr_lines%
            line_nbr% = line_nbr% + 1%
            t1_nbr% = t1_nbr% + 1%
            t1_amt  = t1_amt  + availdollars
            goto read_sorted_workfile

        page_heading
            page_nbr% = page_nbr% + 1%  :  line_nbr% = 9%
        page_0_heading
            print page
            print using L60040, date$, time$, company_name$, "-" & rptid$
            print using L60070, sub_hdr$, page_nbr%
            if pagesw% <> 0% then return
            print
            print using L62200, stor_code$, stor_name$
            print
            print using L61000, "#"
            print using L61300, "#"
            print using L61600
            print
            return

        report_total
            eodsw$ = "1"
            gosub store_total
            if line_nbr% > 56 then gosub page_heading
            print using L62900
            print using L63600, rt_nbr%, rt_amt
            print
            print using L63800                        /* END OF REPORT */
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)

        REM SCRATCH THE WORK FILE HOLDING CURRENT LINE ITEMS ************
            close #9
            call "SCRATCH" addr("F", work_fil$, work_lib$, work_vol$,    ~
                "B", " ", comp%)
            goto inputmode

        store_total
            gosub region_total
            if line_nbr% > 56 then gosub page_heading
            if pos("4" = sort_code$) = 0% then L16218
                print using L62300
                line_nbr% = line_nbr% + 1%
L16218:     print
            print using L63400, brk_stor$, stor_name$, t3_nbr%, t3_amt
            line_nbr% = line_nbr% + 2%
            rt_nbr% = rt_nbr% + t3_nbr%
            rt_amt  = rt_amt  + t3_amt
            t3_nbr%, t3_amt = 0
        store_reset
            if eodsw$ = "1" then return
            brk_stor$ = stor_code$
            call "DESCRIBE" (#7, stor_code$, stor_name$, 1%, f1%(7))
            if f1%(7) = 0% then stor_name$ = "(Unknown Store Code)"
            gosub page_heading
            return

        region_total
            gosub customer_total
            if line_nbr% > 56 then gosub page_heading
            if sort_code$ = "4" then L17150
            print using L62900
            print using L63200, brk_regn$, regn_name$, t2_nbr%, t2_amt
            print using L62300
            line_nbr% = line_nbr% + 3%
L17150:     t3_nbr% = t3_nbr% + t2_nbr%
            t3_amt  = t3_amt  + t2_amt
            t2_nbr%, t2_amt = 0
        regn_reset
            if eodsw$ = "1" then return
            brk_regn$ = regn_code$
            gencodes_key$ = str("SHPREGION",,9) & regn_code$
            call "DESCRIBE" (#8, gencodes_key$, regn_name$, 0%, f1%(8))
            if f1%(8) = 0% then regn_name$ = "Region code not in file"
            return

        customer_total
            if pos("34" = sort_code$) <> 0% then L17320
            skipsw$ = "1"
            if line_nbr% > 56 then gosub page_heading
            print using L62900
            print using L63110, brk_cust$, brk_name$, t1_nbr%, t1_amt
            line_nbr% = line_nbr% + 2%
L17320:     t2_nbr% = t2_nbr% + t1_nbr%
            t2_amt  = t2_amt  + t1_amt
            t1_nbr%, t1_amt = 0
        cust_reset
            if eodsw$ = "1" then return
            brk_cust$ = cust_code$
            brk_name$ = cust_name$
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20120,         /* Required Ship Dat*/~
                                    L20160,         /* Store Codes      */~
                                    L20190,         /* Region Codes     */~
                                    L20221          /* Sort Code        */
                     return
L20120:     REM Required Ship Dates                   SHIP_DATE$
            inpmessage$= "Enter a range of required SHIP DATES, 'FIRST'"&~
                ", 'LAST', OR 'ALL'"
                return
L20160:     REM Store Codes                           STOR_CODE$
            inpmessage$= "Enter a range of STORE CODES, or 'ALL'"
                return
L20190:     REM Region Codes                          REGN_CODE$
            inpmessage$= "Enter a range of SHIPPING REGION CODES, " &    ~
                "'FIRST', 'LAST', OR 'ALL'"
                return
L20221:     inpmessage$= "Enter code for sort order: 1-4 see key above "&~
                "for help."
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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  str(line2$,62%) = "SHPAVAIL: " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40200,         /* Required Ship Dat*/~
                                    L40200,         /* Store Codes      */~
                                    L40200,         /* Region Codes     */~
                                    L40230          /* Sort Code        */
                  if errormsg$ > " " and fieldnr% > 0% then              ~
                     lfac$(fieldnr%) = or hex(10)
                  goto L40270

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40200:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L40230:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40270:     accept                                                       ~
               at (01,02),                                               ~
                  "Orders Available for Scheduling Report",              ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Required Ship Dates",                        ~
               at (06,25), fac(lfac$( 1)), low_date$            , ch(10),~
               at (06,52), fac(lfac$( 1)), high_date$           , ch(10),~
                                                                         ~
               at (07,02), "Store Codes",                                ~
               at (07,25), fac(lfac$( 2)), low_stor$            , ch(03),~
               at (07,52), fac(lfac$( 2)), high_stor$           , ch(03),~
                                                                         ~
               at (08,02), "Shipping Regions",                           ~
               at (08,25), fac(lfac$( 3)), low_regn$            , ch(09),~
               at (08,52), fac(lfac$( 3)), high_regn$           , ch(09),~
                                                                         ~
               at (12,06), "Possible Sorts:",                            ~
               at (13,08), "1 - Store/Region/Customer/Ship Date/Order #",~
               at (14,08), "2 - Store/Region/Customer/Order Number     ",~
               at (15,08), "3 - Store/Region/Ship Date/Customer/Order #",~
               at (16,08), "4 - Store/Ship Date/Customer/Order Number  ",~
                                                                         ~
               at (10,02), "Sort Order (1 to 4)",                        ~
               at (10,25), fac(lfac$( 4)), sort_code$           , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(000104050d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L40640
                  call "MANUAL" ("SHPAVAIL")
                  goto L40270

L40640:        if keyhit% <> 15 then L40680
                  call "PRNTSCRN"
                  goto L40270

L40680:        if fieldnr% > 0% then return
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
                  on fieldnr% gosub L50130,         /* Required Ship Dat*/~
                                    L50320,         /* Store Codes      */~
                                    L50380,         /* Region Codes     */~
                                    L50440          /* Sort Code        */
                  return

L50130: REM Required Ship Dates                   SHIP_DATE$
            from_date$   = " "
            to_date$     = " "
            if low_date$ = " "     or ~
               low_date$ = "ALL"   or ~
               low_date$ = "FIRST" then from_date$ = blankdate$
            if from_date$  = blankdate$ then goto chk_date_high

            call "DATEOKC" (low_date$, from_date%, errormsg$)
            if errormsg$ <> " " then return
            call "DATUFMTC" (low_date$)
            from_date$ = low_date$
            call "DATFMTC"  (low_date$)

chk_date_high
            if low_date$  = "ALL"  or   ~
               high_date$ = " "    or   ~
               high_date$ = "LAST" then ~
                 to_date$ = hex(FFFFFFFFFFFF)
            if to_date$   = hex(FFFFFFFFFFFF) then ~
               goto chk_date_good

            call "DATEOKC" (high_date$, to_date%, errormsg$)
            if errormsg$ <> " " then return
            call "DATUFMTC" (high_date$)
            to_date$ = high_date$
            call "DATFMTC"  (high_date$)

chk_date_good
            if from_date$ > to_date$ then errormsg$ =                    ~
                "The FROM Date must be EARLIER than the TO Date"
            return

L50320: REM Store Codes                           STOR_CODE$
            call "TESTRNGE" (low_stor$, high_stor$, from_stor$, to_stor$,~
                errormsg$)
            from_stor$ = addc (hex(01))
                return

L50380: REM Region Codes                          REGN_CODE$
            call "TESTRNGE" (low_regn$, high_regn$, from_regn$, to_regn$,~
                errormsg$)
            from_regn$ = addc (hex(01))
                return

L50440: REM Sort Code                             SORT_CODE$
            if pos("1234" = sort_code$) <> 0% then return
            errormsg$ = "Sort code must be '1', '2', '3', or '4'."
            return

        REM *************************************************************~
            *             I M A G E   S T A T E M E N T S               *~
            *************************************************************

L60040: %RUN DATE: ######## @ ########      #############################~
        ~###############################                      SHPAVAIL####~
        ~###
L60070: %                          ######################################~
        ~##########################################                PAGE: #~
        ~###
L61000: %SHIPPING  P SHIP TO   SHIP TO                    SHIP TO        ~
        ~              X                  -- SHIP DATES ---  VALUE -NOT-  ~
        ~#OF
L61300: % REGION   R CUSTOMER  CUSTOMER NAME              CUSTOMER LOCATI~
        ~ON            P SALES ORDER #    EARLIEST   LATEST SHIPPED/SCHLD ~
        ~LNS
L61600: %--------- - --------- -------------------------- ---------------~
        ~------------- - ---------------- -------- -------- ------------- ~
        ~---
L61900: %######### # ######### ########################## ###############~
        ~############# # ################ ######## ########-##,###,###.## ~
        ~###
L62200: %     STORE: ### ################################
L62300: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~---
L62510: %                          ######################################~
        ~##########################################
L62900: %                                                                ~
        ~                                                   -------------
L63110: %   * CUSTOMER ######### ############################# ###,###,##~
        ~# ORDERS                                          -##,###,###.##
L63200: %  ** REGION ######### ##############################  ###,###,##~
        ~# ORDERS                                          -##,###,###.##
L63400: % *** STORE  ### ################################      ###,###,##~
        ~# ORDERS                                          -##,###,###.##
L63600: %**** REPORT TOTALS                                    ###,###,##~
        ~# ORDERS                                          -##,###,###.##
L63800: %                                                        ** END O~
        ~F REPORT **

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
