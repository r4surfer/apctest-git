        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  DDDD   EEEEE  M   M  DDDD   TTTTT  L      RRRR   PPPP    *~
            *  D   D  E      MM MM  D   D    T    L      R   R  P   P   *~
            *  D   D  EEEE   M M M  D   D    T    L      RRRR   PPPP    *~
            *  D   D  E      M   M  D   D    T    L      R   R  P       *~
            *  DDDD   EEEEE  M   M  DDDD     T    LLLLL  R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DEMDTLRP - Demand Detail Report                           *~
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
            * 05/09/88 ! Original                                 ! TLJ *~
            * 04/09/91 ! (PRR 11687) Corrected Testing Logic for  ! RJB *~
            *          !      the Priority Range on the Screen and!     *~
            *          !      during report generation.           !     *~
            *          ! (PRR 11727) Corrected reseting of page # !     *~
            *          !      to 1 when printing over 99 pages,   !     *~
            *          !      changed format line to handle ##### !     *~
            *          ! (New Stand.) Added Call to ALLFREE       !     *~
            * 06/24/91 ! QC-FIXES Removed 'ALLFREE' not necessary ! RJB *~
            * 06/09/92 ! Pick up a little speed                   ! KAB *~
            * 08/04/92 ! Added extra DEMMASTR channel for PLOWCODE! JDH *~
            * 10/15/92 ! Fixed single demand range.               ! JDH *~
            * 08/05/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            company$60,                  /* Company Name               */~
            complete$10,                 /* Planned Completion Date    */~
            complete1$10,                /* Planned Completion Date    */~
            cpl$8,                       /* Comp Date, TESTRANGE       */~
            cpl1$8,                      /* Comp Date, TESTRANGE       */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9,                   /* Customer Code              */~
            cuscode1$9,                  /* Customer Code              */~
            cus$9,                       /* Customer Code, TESTRANGE   */~
            cus1$9,                      /* Customer Code, TESTRANGE   */~
            date$8,                      /* Date for screen display    */~
            delreq$10,                   /* Delivery Date Required     */~
            delreq1$10,                  /* Delivery Date Required     */~
            del$8,                       /* Delivery Date, TESTRANGE   */~
            del1$8,                      /* Delivery Date, TESTRANGE   */~
            demcode$16,                  /* Demand Code                */~
            demcode1$16,                 /* Demand Code                */~
            dem$16,                      /* Demand Code, TESTRANGE     */~
            dem1$16,                     /* Demand Code, TESTRANGE     */~
            descr$50,                    /* Description for PLOW       */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            p$(6)1,                      /* Planned Status             */~
            p$14,                        /* Planned Status             */~
            part$25,                     /* Part Range                 */~
            part1$25,                    /* Part Range                 */~
            pt$25,                       /* Part Range, TESTRANGE      */~
            pt1$25,                      /* Part Range, TESTRANGE      */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            priority$1,                  /* Priority Range Start       */~
            priority1$1,                 /* Priority Range End         */~
            quan$10,                     /* Quantity                   */~
            quan1$10,                    /* Quantity                   */~
            rec_status$1,                /* Record's Status            */~
            rec_type$1,                  /* Record's Type              */~
            rec_prior$1,                 /* Record's Priority          */~
            rec_req$8,                   /* Record's Req. Delivery Date*/~
            rec_dem$16,                  /* Record's Demand Code       */~
            rec_line$3,                  /* Record's Demand Line       */~
            rec_part$25,                 /* Record's Part Number       */~
            rec_quan$10,                 /* Record's Quantity          */~
            rec_wc$4,                    /* Record's Work Center       */~
            rec_bom$3,                   /* Record's BOM ID            */~
            rec_rte$3,                   /* Record's Route ID          */~
            rec_lp$8,                    /* Record's Last Planned Date */~
            rec_pcd$8,                   /* Record's Planned Comp Date */~
            rec_cus$9,                   /* Record's Customer          */~
            sort$1,                      /* Sort By                    */~
            sortdescr$35,                /* Sort By Description        */~
            sortstr$150,                 /* Sort Parameter String      */~
            t$(10)1,                     /* Demand Type                */~
            t$18,                        /* Demand Type                */~
            to$(10)2,                    /* To for ranges in ACCEPT    */~
            userid$3,                    /* Current User Id            */~
            wc$5,                        /* Work Center                */~
            wc1$5,                       /* Work Center                */~
            wctr$5,                      /* Work Center, TESTRANGE     */~
            wctr1$5                      /* Work Center, TESTRANGE     */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

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
            * # 1 ! DEMMASTR ! Demand Master file                       *~
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 3 ! DEMMASTR ! Demand Master file for CODECODE          *~
            * # 4 ! WCMASTR  ! WORK CENTER MASTER FILE                  *~
            * # 5 ! CUSTOMER ! Customer Master File                     *~
            * # 8 ! WORKFILE ! Workfile for Sorting Report              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select  #1, "DEMMASTR",                                      ~
                        varc,     indexed,  recsize =  123,              ~
                        keypos =   2, keylen =  27 ,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28,         ~
                            key  3, keypos =   29, keylen =  25

            select  #2, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select  #3, "DEMMASTR",                                      ~
                        varc,     indexed,  recsize =  123,              ~
                        keypos =   2, keylen =  27 ,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28,         ~
                            key  3, keypos =   29, keylen =  25

            select #4,  "WCMASTR",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos = 2  , keylen = 5,                       ~
                         alt key 1, keypos = 1, keylen = 6

            select #5,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #8, "WORKFILE", consec, recsize = 123

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (# 1, "SPLIN", f2%( 1), rslt$( 1), " ")
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 0%, rslt$( 5))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "DEMDTLRP: " & str(cms2v$,,8)

            if f2%(1) <> 0% then exit_program
            recs% = val(str(rslt$(1),17,4), 4)
            if recs% = 0% then exit_program
            recs% = max(100%, recs%/3%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 11%
L10100:         gosub'051(fieldnr%)    /* Default / Enables */
                      if enabled% = 0% then L10330
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 7% then L10290
                          if t$() = "          " and fieldnr% <= 2%      ~
                                                 then t$() = " XXXXX XXX"
                          if p$() = "          " and fieldnr% <= 9%      ~
                                                     then p$() = all("X")
                          for i% = fieldnr% to 12%
                             gosub'151(i%)
                             if errormsg$<>" " then L10120
                          next i%
                          goto editpg1
L10290:               if keyhit%<>5% and keyhit%<>6% then L10320
                        gosub'051(fieldnr%)
                        gosub'151(fieldnr%)
                        goto L10120
L10320:               if keyhit% <> 0% then       L10120
L10330:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       generate_report
                  if keyhit% <> 0% then editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% then fieldnr% = 1%
            if fieldnr% > 11% then fieldnr% = 11%
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)     /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit% <> 5% and keyhit% <> 6% then L11220
                     gosub'051(fieldnr%)
                     goto L11180
L11220:           if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11180
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *                 P R I N T   R E P O R T                   *~
            *-----------------------------------------------------------*~
            * Prints the Report.                                        *~
            *************************************************************

        generate_report:
            call "SHOSTAT" ("Generating Demand Detail Report")
            convert sort$ to sort%
            call "WORKOPEN" ( #8, "OUTPT", recs%, f2%(8) )
            runtime$ = " "
            call "TIME" (runtime$)
            call "COMPNAME" (12%, company$, u3%)
            something_printed% = 0%
            line% = 99%
            page% = 0%
            wrecs% = 0%

            init (hex(00)) plowkey$
            call "PLOWALTS" (#1, plowkey$, 0%, 0%, f1%(1))
            goto L12520

        read_record:
            call "READNEXT" (#1, f1%(1))
L12520:        if f1%(1) = 0% then print_report

            if str(key(#1, 1%),,16) < dem$  then read_record
            if str(key(#1, 1%),,16) > dem1$ then read_record

            get #1 using L12620,rec_status$,rec_type$,rec_prior$,rec_req$,~
                             rec_dem$,rec_line$,rec_part$,rec_quan$,     ~
                             rec_wc$,rec_bom$,rec_rte$,rec_lp$,          ~
                             rec_pcd$,rec_cus$

L12620:     FMT              CH(1), CH(1), CH(1), CH(6),                 ~
                             CH(16), CH(3),CH(25),CH(10),                ~
                             CH(4), CH(3), CH(3), XX(3), CH(6),          ~
                             CH(6), CH(9)

            convert rec_quan$ to rec_quan
            convert rec_quan  to rec_quan$, pic(#######.##)

            gosub testdata
            if failed% = 1% then read_record

            write #8, using L12790, rec_status$,rec_type$,                ~
                             rec_prior$,rec_req$,                        ~
                             rec_dem$,rec_line$,rec_part$,rec_quan$,     ~
                             rec_wc$,rec_bom$,rec_rte$,rec_lp$,          ~
                             rec_pcd$,rec_cus$

L12790:     FMT              CH(1), CH(1),                               ~
                             CH(1), CH(6),                               ~
                             CH(16), CH(3),CH(25),CH(10),                ~
                             CH(4), CH(3), CH(3), XX(3), CH(6),          ~
                             CH(6), CH(9)
            wrecs% = wrecs% + 1%
            goto read_record

        testdata:
            failed% = 1%

*       ** TEST_STATUS:
            if rec_status$ <> " " then L13070
               status% = 1%
               goto L13100
L13070:     convert rec_status$ to status%
            if status% >= 6% then status% = status% - 3
            if rec_status$ = "1" then status% = 2%
L13100:     if str(p$(),status%,1) = " " then return

*       ** TEST_TYPE:
            convert rec_type$ to type%
            if str(t$(),type%+1,1) = " " then return

*       ** TEST_PRIORITY:
            if priority1$ <> " " then L13190
                if rec_prior$ = priority$ then L13230 else return
L13190:     if rec_prior$ >= priority$ and rec_prior$ <= priority1$      ~
                                                              then L13230
            return

L13230
*       ** TEST_REQ_DEL:
            if rec_req$ < del$ or rec_req$ > del1$ then return

*       ** TEST_DEMAND_CODE:
*          IF REC_DEM$ < DEM$ OR REC_DEM$ > DEM1$ THEN RETURN
            /* Done Above */
*       ** TEST_PART:
            if rec_part$ < pt$ or rec_part$ > pt1$ then return

*       ** TEST_WC:
            if rec_wc$ < wctr$ or rec_wc$ > wctr1$ then return

*       ** TEST_PCD:
            if rec_pcd$ < cpl$ or rec_pcd$ > cpl1$ then return

*       ** TEST_CUSTOMER:
            if rec_cus$ < cus$ or rec_cus$ > cus1$ then return

*       ** TEST_QUANTITY:
            if quan$ = "ALL" then L13450
            if rec_quan$ < quan$ or rec_quan$ > quan1$ then return

L13450
*       ** ALL OK FINE:
            failed% = 0%
            return

*       ** Report Generation ***
        print_report:
            select printer(134)
            call "SETPRNT" ("DEM001", " ", wrecs%, 0%)
            if wrecs% = 0% then exit_report

            init (" ") sortstr$
            call "GETNAMES" addr(#8,  str(sortstr$,  1, 8),              ~
                                      str(sortstr$,  9, 8),              ~
                                      str(sortstr$, 17, 6))
            str(sortstr$, 23, 22) = str(sortstr$, 1, 22)
            close #8
            on sort% goto L15680,         /* Demand Code/Line           */~
                          L15550,         /* Date Required              */~
                          L15480,         /* Part Code                  */~
                          L15410,         /* Planned Completion Date    */~
                          L15340,         /* Customer                   */~
                          L15270,         /* Quantity                   */~
                          L15200          /* Priority                   */

L15200
*       **  Priority
             str(sortstr$, 45, 4) = "0003"
             str(sortstr$, 49, 3) = "001"
             str(sortstr$, 52, 1) = "C"
             str(sortstr$, 53, 1) = "A"
             goto L15610

L15270
*       **  Quantity
             str(sortstr$, 45, 4) = "0054"
             str(sortstr$, 49, 3) = "010"
             str(sortstr$, 52, 1) = "C"
             str(sortstr$, 53, 1) = "A"
             goto L15610

L15340
*       **  Customer
             str(sortstr$, 45, 4) = "0089"
             str(sortstr$, 49, 3) = "009"
             str(sortstr$, 52, 1) = "C"
             str(sortstr$, 53, 1) = "A"
             goto L15610

L15410
*       **  Planned Completion Date
             str(sortstr$, 45, 4) = "0083"
             str(sortstr$, 49, 3) = "006"
             str(sortstr$, 52, 1) = "C"
             str(sortstr$, 53, 1) = "A"
             goto L15610

L15480
*       **  Part Code
             str(sortstr$, 45, 4) = "0029"
             str(sortstr$, 49, 3) = "025"
             str(sortstr$, 52, 1) = "C"
             str(sortstr$, 53, 1) = "A"
             goto L15610

L15550
*       **  Date Required
             str(sortstr$, 45, 4) = "0004"
             str(sortstr$, 49, 3) = "006"
             str(sortstr$, 52, 1) = "C"
             str(sortstr$, 53, 1) = "A"

L15610
*       ** Secondary Sort for Dups
             str(sortstr$, 54, 4) = "0010"
             str(sortstr$, 58, 3) = "019"
             str(sortstr$, 61, 1) = "C"
             str(sortstr$, 62, 1) = "A"
             goto L15740

L15680
*       ** Demand Code/Line
             str(sortstr$, 45, 4) = "0010"
             str(sortstr$, 49, 3) = "019"
             str(sortstr$, 52, 1) = "C"
             str(sortstr$, 53, 1) = "A"

L15740
*       ** Now Sort and Reopen
            call "SORTCALL" addr(sortstr$, ret%)
               if ret% = 0% then L15790
                  return clear all
                  goto exit_report
L15790:     call "WORKOPN2" (#8, "INPUT", 0%, f2%(8))

        next_line:
            call "READNEXT" (#8, f1%(8))
               if f1%(8) = 0% then exit_report
            get #8 using L16090,                                          ~
                             rec_status$,rec_type$,rec_prior$,rec_req$,  ~
                             rec_dem$,rec_line$,rec_part$,rec_quan$,     ~
                             rec_wc$,rec_bom$,rec_rte$,rec_lp$,          ~
                             rec_pcd$,rec_cus$

L16090:     FMT              CH(1), CH(1), CH(1), CH(6),                 ~
                             CH(16), CH(3),CH(25),CH(10),                ~
                             CH(4), CH(3), CH(3), XX(3), CH(6),          ~
                             CH(6), CH(9)

            line% = line% + 1%
            if line% > 56% then gosub print_headings
            if rec_status$ = "8" or rec_status$ = "9" then plnd$ = "OK"
            if rec_status$ = "6" or rec_status$ = "7" then plnd$ = "LATE"
            if rec_status$ = " " or rec_status$ = "1" then plnd$ = "NOT"
            if rec_status$ = " " or rec_status$ = "6" or                 ~
               rec_status$ = "8" then aprvd$ = "N" else aprvd$ = "Y"
            call "STRING" addr("RJ", rec_quan$, 10%)
            call "DATEFMT" (rec_req$)
            call "DATEFMT" (rec_pcd$)
            call "DATEFMT" (rec_lp$)
            print     using L19220, rec_dem$, rec_line$, rec_part$,       ~
                             rec_quan$, rec_req$, rec_pcd$, rec_lp$,     ~
                             plnd$, aprvd$, rec_type$, rec_prior$,       ~
                             rec_cus$, rec_bom$, rec_rte$, rec_wc$
            goto next_line

        exit_report:
            if something_printed% = 1% then L17090
               u3% = 2%
               call "ASKUSER" (u3%, "* * * NOTE * * *",                  ~
                                  "No report printed.",                  ~
                                  "No DEMANDS EXIST Meeting the "       &~
                                         "Specified Criteria.",          ~
                                  "Press PF(1) to STARTOVER or RETURN " &~
                                         "to EDIT criteria." )
                goto L17140
L17090:     runtime$ = " " : call "TIME" (runtime$)
            print skip(58% - line%)
            print "* * * END OF REPORT - "; runtime$ ; " * * *"
            u3% = 1%
L17140:     close printer
            call "SETPRNT" ("DEM001", " ", 0%, 1%)
            call "FILEBGON" (#8)
            errormsg$ = " "
            if u3% = 1% then goto inputmode else goto editpg1

        print_headings:
            if page% = 0% then gosub print_page_zero
            something_printed% = 1%
            page% = page% + 1%
            line% = 6%
            gosub print_master_headings
            print
            print using L19130
            print using L19150
            print using L19180
            return

        print_master_headings:
            convert page% to page$, pic(#####)
            call "STRING" addr("RJ", page$, 2%)
            print page
            print using L19050, date$, company$
            print using L19090, runtime$, page$
            return

        print_page_zero:
            init (" ") t$, p$
            comma$ = " "
            for i% = 1% to 9%
              if str(t$(),i%+1%,1) = " " then L18580
                convert i% to i$, pic(#)
                t$ = t$ & comma$ & i$
                comma$ = ","
L18580:     next i%
            comma$ = " "
            for i% = 1% to 6%
              if str(p$(),i%,1) = " " then L18650
                convert i% to i$, pic(#)
                p$ = p$ & comma$ & i$
                comma$ = ","
L18650:     next i%

            gosub print_master_headings
            print skip(18)
            print using L19260
            print using L19280
            print
            print using L19310, demcode$, to$(1), demcode1$
            print using L19330, t$
            print using L19350, wc$, to$(2), wc1$
            print using L19370, priority$, to$(3), priority1$
            print using L19390, part$, to$(4), part1$
            print using L19410, delreq$, to$(5), delreq1$
            print using L19430, complete$, to$(6), complete1$
            print using L19450, cuscode$, to$(7), cuscode1$
            print using L19470, p$
            print using L19490, quan$, to$(8), quan1$
            print using L19510, sort$, sortdescr$
            return

        REM *************************************************************~
            *                P R I N T   F O R M A T S                  *~
            *-----------------------------------------------------------*~
            * Formats and image statements for report output.           *~
            *************************************************************
L19050: %DATE: ########                      ############################~
        ~################################                     DEMDTLRP-DEM~
        ~001

L19090: %TIME: ########                                       DEMAND DETA~
        ~IL REPORT                                                PAGE: ##~
        ~###

L19130: %                                                          DELIVE~
        ~RY  PLANNED    LAST
L19150: %DEMAND           LNE PART                        QUANTITY REQUIR~
        ~ED COMPLETION PLANNED  STATUS APPR TYPE PRIOR CUSTOMER  BOM RTE W~
        ~C
L19180: %---------------- --- ------------------------- ---------- ------~
        ~-- ---------- -------- ------ ---- ---- ----- --------- --- --- -~
        ~---

L19220: %################ ### ######################### ########## ######~
        ~## ########   ######## ####    #    #     #   ######### ### ### #~
        ~###

L19260: %                                              DEMAND DETAIL REPO~
        ~RT CRITERIA
L19280: %                                              ------------------~
        ~-----------

L19310: %                                   DEMAND CODE              ####~
        ~############ ## ################
L19330: %                                   DEMAND TYPE            ######~
        ~###########
L19350: %                                   WORK CENTER              ####~
        ~ ## ####
L19370: %                                   PRIORITY                 # ##~
        ~ #
L19390: %                                   PART RANGE               ####~
        ~##################### ## #########################
L19410: %                                   DELIVERY DATE REQUIRED   ####~
        ~###### ## ##########
L19430: %                                   PLANNED COMPLETION DATE  ####~
        ~###### ## ##########
L19450: %                                   CUSTOMER CODE            ####~
        ~#####  ## #########
L19470: %                                   PLANNED STATUS         ######~
        ~########
L19490: %                                   QUANTITY                 ####~
        ~###### ## ##########
L19510: %                                   SORT BY                  #   ~
        ~     ##################################

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20230,         /* Demand Code            */~
                              L20290,         /* Demand Type            */~
                              L20320,         /* Work Center            */~
                              L20410,         /* Priority               */~
                              L20480,         /* Part Range             */~
                              L20540,         /* Delivery Date Req      */~
                              L20600,         /* Plnd Comp Date         */~
                              L20670,         /* Customer Code          */~
                              L20730,         /* Planned Status         */~
                              L20760,         /* Quantity               */~
                              L20810          /* Sort By                */
            return


L20230
*        Def/Enable Demand Code                 DEMCODE$
            to$(1) = "TO"
            if demcode$ <> " " or demcode1$ <> " " then return
            demcode$ = "ALL"
            return

L20290
*        Def/Enable Demand Type                 T$()
            if keyhit% = 6% then t$() = " "
            if keyhit% = 5% then t$() = " XXXXX XXX"
            return

L20320
*        Def/Enable Work Center                 WC$
            if str(t$(),10,1) <> " " then L20360
               enabled% = 0%
               goto L20380
L20360:     to$(2) = "TO"
            if wc$ <> " " or wc1$ <> " " then return
L20380:     wc$ = "ALL"
            return

L20410
*        Def/Enable Priority                    PRIORITY$
            to$(3) = "TO"
            if priority$ <> " " or priority1$ <> " " then return
            priority$ = "A"
            priority1$ = "Z"
            return

L20480
*        Def/Enable Part Range                  PART$
            to$(4) = "TO"
            if part$ <> " " or part1$ <> " " then return
            part$ = "ALL"
            return

L20540
*        Def/Enable Delivery Date Required      DELREQ$
            to$(5) = "TO"
            if (delreq$  <> " " and delreq$  <> blankdate$) or ~
               (delreq1$ <> " " and delreq1$ <> blankdate$) then return
            delreq$ = "ALL"
            return

L20600
*        Def/Enable Planned Completion Date     COMPLETE$
            to$(6) = "TO"
            if (complete$  <> " " and complete$  <> blankdate$) or ~
               (complete1$ <> " " and complete1$ <> blankdate$) then return
            complete$ = "ALL"
            return


L20670
*        Def/Enable Customer Code               CUSCODE$
            to$(7) = "TO"
            if cuscode$ <> " " or cuscode1$ <> " " then return
            cuscode$ = "ALL"
            return

L20730
*        Def/Enable Planned Status              STATUS$
            if keyhit% = 6% then p$() = all(" ")
            if keyhit% = 5% then p$() = all("X")
            return

L20760
*        Def/Enable Quantity                    QUAN$
            to$(8) = "TO"
            if quan$ = " " then quan$ = "ALL"
            return

L20810
*        Def/Enable Sort By                     SORT$
            if sort$ <> " " then L20850
              sort$ = "1"
              sortdescr$ = "(Sort by DEMAND CODE)"
L20850:     return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Demand Code.                                           ",~
         "1-Net Sale,2-Non Net,3-Requis,4-FCST,5-Above FCST,7-PROC/JMP,8-~
        ~PROC,9-Prev.Main"                                               ,~
         "Enter Work Center Range.                                     ",~
         "Enter Priority, Must Be Between 'A' and 'Z'.                 ",~
         "Enter Part Range.                                            ",~
         "Enter Requested Delivery Date Required.                      ",~
         "Enter Planned Completion Date.                               ",~
         "Enter Customer Code Range.                                   ",~
         "Planned/Approved, 1-NOT/NOT,2-NOT/APR,3-LATE/NOT,4-LATE/APR,5-O~
        ~K/NOT, 6-OK/APR",                                                ~
          "Enter Quantity Range.",                                       ~
         "1-Demand, 2-Del Req, 3-Part, 4-Plned Compl, 5-Customer, 6-Quant~
        ~ity, 7-Priority"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sort$,                     ~
                      complete$, cuscode$, delreq$, demcode$,            ~
                      part$, wc$, priority$, p$(), t$(), to$(), quan$,   ~
                      complete1$, cuscode1$, delreq1$, demcode1$,        ~
                      part1$, wc1$, priority1$, sort$, quan1$,           ~
                      sortdescr$
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
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

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40260,         /* Demand Code       */   ~
                                L40260,         /* Demand Type       */   ~
                                L40260,         /* Work Center       */   ~
                                L40260,         /* Priority          */   ~
                                L40260,         /* Part Range        */   ~
                                L40260,         /* Delivery Date Req */   ~
                                L40260,         /* Plnd Comp Date    */   ~
                                L40260,         /* Customer Code     */   ~
                                L40260,         /* Planned Status    */   ~
                                L40260,         /* Quantity          */   ~
                                L40260          /* Sort by           */
              goto L40290

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40260:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40290:     if edit% = 2% and str(t$(),10,1)=" " then lfac$(3) = hex(8c)
L40300:     accept                                                       ~
               at (01,02),                                               ~
                  "Detailed Report for Demands",                         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (06,02), "Demand Code",                                ~
               at (06,27), fac(lfac$( 1)), demcode$             , ch(16),~
               at (06,44), fac(hex(8c)), to$(1)                 , ch(02),~
               at (06,47), fac(lfac$( 1)), demcode1$            , ch(16),~
                                                                         ~
               at (07,02), "Demand Type",                                ~
               at (07,27), "1",                                          ~
               at (07,29), fac(lfac$( 2)), t$(2)                , ch(01),~
               at (07,32), "2",                                          ~
               at (07,34), fac(lfac$( 2)), t$(3)                , ch(01),~
               at (07,37), "3",                                          ~
               at (07,39), fac(lfac$( 2)), t$(4)                , ch(01),~
               at (07,42), "4",                                          ~
               at (07,44), fac(lfac$( 2)), t$(5)                , ch(01),~
               at (07,47), "5",                                          ~
               at (07,49), fac(lfac$( 2)), t$(6)                , ch(01),~
               at (07,52), "7",                                          ~
               at (07,54), fac(lfac$( 2)), t$(8)                , ch(01),~
               at (07,57), "8",                                          ~
               at (07,59), fac(lfac$( 2)), t$(9)                , ch(01),~
               at (07,62), "9",                                          ~
               at (07,64), fac(lfac$( 2)), t$(10)               , ch(01),~
                                                                         ~
               at (08,02), "Work Center",                                ~
               at (08,27), fac(lfac$( 3)), wc$                  , ch(05),~
               at (08,33), fac(hex(8c)), to$(2)                 , ch(02),~
               at (08,36), fac(lfac$( 3)), wc1$                 , ch(05),~
                                                                         ~
               at (09,02), "Priority",                                   ~
               at (09,27), fac(lfac$( 4)), priority$            , ch(01),~
               at (09,29), fac(hex(8c)), to$(3)                 , ch(02),~
               at (09,32), fac(lfac$( 4)), priority1$           , ch(01),~
                                                                         ~
               at (10,02), "Part Range",                                 ~
               at (10,27), fac(lfac$( 5)), part$                , ch(25),~
               at (10,53), fac(hex(8c)), to$(4)                 , ch(02),~
               at (10,56), fac(lfac$( 5)), part1$               , ch(25),~
                                                                         ~
               at (11,02), "Delivery Date Required",                     ~
               at (11,27), fac(lfac$( 6)), delreq$              , ch(10),~
               at (11,38), fac(hex(8c)), to$(5)                 , ch(02),~
               at (11,41), fac(lfac$( 6)), delreq1$             , ch(10),~
                                                                         ~
               at (12,02), "Planned Completion Date",                    ~
               at (12,27), fac(lfac$( 7)), complete$            , ch(10),~
               at (12,38), fac(hex(8c)), to$(6)                 , ch(02),~
               at (12,41), fac(lfac$( 7)), complete1$           , ch(10),~
                                                                         ~
               at (13,02), "Customer Code",                              ~
               at (13,27), fac(lfac$( 8)), cuscode$             , ch(09),~
               at (13,37), fac(hex(8c)), to$(7)                 , ch(02),~
               at (13,40), fac(lfac$( 8)), cuscode1$            , ch(09),~
                                                                         ~
               at (14,02), "Planned Status",                             ~
               at (14,27), "1",                                          ~
               at (14,29), fac(lfac$(09)), p$(1)                , ch(01),~
               at (14,32), "2",                                          ~
               at (14,34), fac(lfac$(09)), p$(2)                , ch(01),~
               at (14,37), "3",                                          ~
               at (14,39), fac(lfac$(09)), p$(3)                , ch(01),~
               at (14,42), "4",                                          ~
               at (14,44), fac(lfac$(09)), p$(4)                , ch(01),~
               at (14,47), "5",                                          ~
               at (14,49), fac(lfac$(09)), p$(5)                , ch(01),~
               at (14,52), "6",                                          ~
               at (14,54), fac(lfac$(09)), p$(6)                , ch(01),~
                                                                         ~
               at (15,02), "Quantity",                                   ~
               at (15,27), fac(lfac$(10)), quan$                , ch(10),~
               at (15,38), fac(hex(8c)), to$(8)                 , ch(02),~
               at (15,41), fac(lfac$(10)), quan1$               , ch(10),~
                                                                         ~
               at (16,02), "Sort By",                                    ~
               at (16,27), fac(lfac$(11)), sort$                , ch(01),~
               at (16,41), fac(hex(8c)), sortdescr$             , ch(35),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L41280
                  call "MANUAL" ("DEMDTLRP") : goto L40300

L41280:        if keyhit% <> 15 then L41310
                  call "PRNTSCRN" : goto L40300

L41310:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41500     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (5)Mark All            " &        ~
                     "  (7)Proceed to Edit   (15)Print Screen"
            pf$(3) = "                 (6)UnMark All          " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04050607ffffffffff0dff0f1000)
            if fieldnr% = 1% then L41470
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
                goto L41471
L41470:     str(pf$(1),18,24) = " " : str(pfkeys$,04,1) = hex(ff)
L41471:     if fieldnr% = 2% or fieldnr% = 9% then L41480
                str(pf$(2),18,24) = " " : str(pfkeys$,05,1) = hex(ff)
                str(pf$(3),18,24) = " " : str(pfkeys$,06,1) = hex(ff)
L41480:     return

L41500: if fieldnr% > 0% then L41590  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L41590:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (5)Mark All            " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (6)UnMark All          " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffff0506ffffffffffff0dff0fff00)
            if fieldnr% = 2% or fieldnr% = 9% then L41670
                str(pf$(2),18,24) = " " : str(pfkeys$,05,1) = hex(ff)
                str(pf$(3),18,24) = " " : str(pfkeys$,06,1) = hex(ff)
L41670:     return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50220,         /* Demand Code            */~
                              L50510,         /* Demand Type            */~
                              L50570,         /* Work Center            */~
                              L50740,         /* Priority               */~
                              L50900,         /* Part Range             */~
                              L51190,         /* Delivery Date Req      */~
                              L51450,         /* Plnd Comp Date         */~
                              L51710,         /* Customer Code          */~
                              L51900,         /* Planned Status         */~
                              L51960,         /* Planned Status         */~
                              L52210          /* Sort By                */
            return

L50220
*        Test for Demand Code                  DEMCODE$
            plowkey$ = " "
            if str(demcode$,1,1) <> "?" then L50280
               call "PLOWCODE" (#3, plowkey$, descr$, -16%, -1.0, f1%(3))
               if f1%(3) = 1% then demcode$ = str(plowkey$,,16)
               plowkey$ = " "
L50280:     if str(demcode1$,1,1) <> "?" then L50300
               call "PLOWCODE" (#3, plowkey$, descr$, -16%, -1.0, f1%(3))
               if f1%(3) = 1% then demcode1$ = str(plowkey$,,16)
L50300:     if str(demcode$,,1)<>"?" and str(demcode1$,,1)<>"?" then L50340
               errormsg$ = "Enter DEMAND RANGE"
               return
L50340:     if demcode$ = " " and demcode1$ <> " " then demcode$ = "FIRST"
            call "TESTRNGE" (demcode$, demcode1$, dem$, dem1$, errormsg$)
            if errormsg$ <> " " then L50490
            if demcode$ <> demcode1$ then L50480   /*****/
               demcode1$ = " "
               plowkey$ = demcode$
               call "PLOWCODE" (#3, plowkey$, descr$, -16%, -1.0, f1%(3))
               if f1%(3) = 1% then L50450
                 errormsg$ = "DEMAND does not exist.  Enter a RANGE or "&~
                                                    "an EXISTING DEMAND."
                 goto L50490
L50450:     demcode$ = str(plowkey$,1,16)
            call "TESTRNGE" (demcode$, demcode1$, dem$, dem1$, errormsg$)
            demcode1$ = " "
L50480:     if demcode1$ = " " then to$(1) = " "
L50490:     return

L50510
*        Test for Demand Type                  T$()
            for j% = 1% to 9%
              if str(t$(),j%,1) <> " " then str(t$(),j%,1) = "X"
            next j%
            return

L50570
*        Test for Work Center                  WC$
            if wc$ = " " and wc1$ <> " " then wc$ = "FIRST"
            call "TESTRNGE" (wc$, wc1$, wctr$, wctr1$, errormsg$)
            if errormsg$ <> " " then L50720
            if wc$ <> wc1$ then L50710   /*****/
               wc1$ = " "
               plowkey$ = str(wc$,1,4)
               call "PLOWCODE" (#4, plowkey$, descr$,  0%, 0.30, f1%(4))
               if f1%(4) = 1% then L50680
                 errormsg$ = "Enter a RANGE or an existing WORK CENTER."
                 goto L50720
L50680:     wc$ = str(plowkey$,1,4)
            call "TESTRNGE" (wc$, wc1$, wctr$, wctr1$, errormsg$)
            wc1$ = " "
L50710:     if wc1$ = " " then to$(2) = " "
L50720:     return

L50740
*        Test for Priority                     PRIORITY$
            if priority$ <> " " then L50750
            if priority1$ <> " " then L50750
               priority$ = "A" : priority1$ = "Z" : to$(3) = "TO"
               /* Just Like Default/Enable would do it */
L50750:     if priority$ = " " then priority$ = "A"
            if priority$ >= "A" and priority$ <= "Z" then L50780
                errormsg$ = "Prioritys MUST BE 'A' through 'Z'." : return
L50780:     if priority1$ <> " " and priority1$ <> priority$ then L50800
                priority1$, to$(3) = " " : return
L50800:     if priority1$ >= "A" and priority1$ <= "Z" then L50820
                 errormsg$ = "Priority MUST BE 'A' through 'Z'." : return
L50820:     if priority1$ >= priority$ then return
                 errormsg$ = "Ending Priority CANNOT BE less the Start."
            return

L50900
*        Test for Part Range                   PART$
            plowkey$ = " "
            if str(part$,1,1) <> "?" then L50960
               call "PLOWCODE" (#3, plowkey$, descr$, -25%, -3.0, f1%(3))
               if f1%(3) = 1% then part$ = str(plowkey$,,25)
               plowkey$ = " "
L50960:     if str(part1$,1,1) <> "?" then L50990
               call "PLOWCODE" (#3, plowkey$, descr$, -25%, -3.0, f1%(3))
               if f1%(3) = 1% then part1$ = str(plowkey$,,25)
L50990:     if str(part$,,1) <> "?" and str(part1$,,1) <> "?" then L51020
               errormsg$ = "Enter PART RANGE"
               return
L51020:     if part$ = " " and part1$ <> " " then part$ = "FIRST"
            call "TESTRNGE" (part$, part1$, pt$, pt1$, errormsg$)
            if errormsg$ <> " " then L51170
            if part$ <> part1$ then L51160
               part1$ = " "
               plowkey$ = str(part$,,25)
               call "PLOWCODE" (#3, plowkey$, descr$, -25%, -3.0, f1%(3))
               if f1%(3) = 1% then L51130
                  errormsg$ = "A DEMAND for this PART does not exist.  "&~
                                 "Enter a RANGE or a PART with a DEMAND."
                  goto L51170
L51130:        part$ = str(plowkey$,1,25)
               call "TESTRNGE" (part$, part1$, pt$, pt1$, errormsg$)
               part1$ = " "
L51160:     if part1$ = " " then to$(4) = " "
L51170:     return

L51190
*        Test for Delivery Date Required       DELREQ$
            if (delreq$  = " " or delreq$  = blankdate$) and ~
               (delreq1$ = " " or delreq1$ = blankdate$) then delreq$ = "ALL"
            if delreq$ = "ALL" or delreq$ = "FIRST" or delreq$ ="LAST"   ~
                                                               then L51250
            call "DATEOKC" (delreq$, date%, errormsg$)
            if errormsg$ <> " " then L51430
L51250:     if delreq1$ = "ALL" or delreq1$ = "FIRST" or delreq1$ ="LAST"~
                          or delreq1$ = blankdate$ or delreq1$ = " " then L51290
            call "DATEOKC" (delreq1$, date1%, errormsg$)
            if errormsg$ <> " " then L51430
L51290:     if delreq$<>"ALL" and delreq$<>"FIRST" and delreq$<>"LAST"   ~
                                          then call "DATUFMTC" (delreq$)
            if delreq1$<>"ALL" and delreq1$<>"FIRST"     and delreq1$<>"LAST"~
                               and delreq1$ = blankdate$ and delreq1$<>" "   ~
                                      then  call "DATUFMTC" (delreq1$)
            call "TESTRNGE" (delreq$, delreq1$, del$, del1$, errormsg$)
            if delreq1$="FIRST" then                                     ~
                       errormsg$="FROM must be less than or equal to TO."
            if delreq$ = delreq1$ then delreq1$ = " "
            if delreq$ <> "ALL" and delreq$ <> "FIRST" and delreq$ <> "LAST"   ~
                                            then call "DATFMTC" (delreq$)
            if delreq1$ <> "ALL" and delreq1$ <> "FIRST" and delreq1$ <> "LAST"~
                                 and delreq1$ <> " " and delreq1$ <> blankdate$~
                                    then call "DATFMTC" (delreq1$)
            if errormsg$ <> " " then L51430
            if delreq1$ = " " then to$(5) = " "
L51430:     return

L51450
*        Test for Planned Completion Date      COMPLETE$
            if (complete$  = " " or complete$  = blankdate$) and ~
               (complete1$ = " " or complete1$ = blankdate$) ~
                               then complete$ = "ALL"
            if complete$="ALL" or complete$="FIRST" or complete$="LAST"  ~
                                                               then L51510
            call "DATEOKC" (complete$, date%, errormsg$)
            if errormsg$ <> " " then L51690
L51510:     if complete1$ = "ALL"      or complete1$ = "FIRST" or ~
               complete1$ = blankdate$ or complete1$ = "LAST"  or ~
               complete1$ = " " then L51550
            call "DATEOKC" (complete1$, date1%, errormsg$)
            if errormsg$ <> " " then L51690
L51550:     if complete$<>"ALL" and complete$<>"FIRST" and               ~
                       complete$<>"LAST" then call "DATUFMTC" (complete$)
            if complete1$ <> "ALL" and complete1$ <> "FIRST" and ~
               complete1$ <> " "   and complete1$ <> "LAST"  and ~
               complete1$ <> blankdate$ then call "DATUFMTC"(complete1$)
            call "TESTRNGE" (complete$,complete1$,cpl$,cpl1$,errormsg$)
            if complete1$="FIRST" then                                   ~
                       errormsg$="FROM must be less than or equal to TO."
            if complete$ = complete1$ then complete1$ = " "
            if complete$<>"ALL" and complete$<>"FIRST" and               ~
                       complete$<>"LAST" then call "DATFMTC" (complete$)
            if complete1$<>"ALL" and complete1$<>"FIRST" and complete1$<>~
               " " and complete1$<>"LAST" then call "DATFMTC" (complete1$)
            if errormsg$ <> " " then L51690
            if complete1$ = " " then to$(6) = " "
L51690:     return

L51710
*        Test for Customer Code                CUSCODE$
            plowkey$ = " "
            if str(cuscode$,1,1) <> "?" then L51722
               call "PLOWCODE" (#5, plowkey$, descr$, 0%, 0.3, f1%(5))
               if f1%(5) = 1% then cuscode$ = str(plowkey$,,9)
               plowkey$ = " "
L51722:     if str(cuscode1$,1,1) <> "?" then L51728
               call "PLOWCODE" (#5, plowkey$, descr$, 0%, 0.3, f1%(5))
               if f1%(5) = 1% then cuscode1$ = str(plowkey$,,9)
L51728:     if str(cuscode$,,1)<>"?" and str(cuscode1$,,1)<>"?"          ~
                                                               then L51740
               errormsg$ = "Enter CUSTOMER RANGE"
               return
L51740:     if cuscode$ = " " and cuscode1$ <> " " then cuscode$ = "FIRST"
            call "TESTRNGE" (cuscode$, cuscode1$, cus$, cus1$, errormsg$)
            if errormsg$ <> " " then L51880
            if cuscode$ <> cuscode1$ then L51870
               cuscode1$ = " "
               plowkey$ = str(cuscode$,,9)
               call "PLOWCODE" (#5, plowkey$, descr$, 0%, 0.3, f1%(5))
               if f1%(5) = 1% then L51840
                  errormsg$ = "Enter a RANGE or an existing CUSTOMER CODE"
                  goto L51880
L51840:        cuscode$ = str(plowkey$,1,9)
               call "TESTRNGE" (cuscode$,cuscode1$,cus$,cus1$,errormsg$)
               cuscode1$ = " "
L51870:     if cuscode1$ = " " then to$(7) = " "
L51880:     return

L51900
*        Test for Planned Status               P$()
            for j% = 1% to 6%
              if str(p$(),j%,1) <> " " then str(p$(),j%,1) = "X"
            next j%
            return

L51960
*        Test for Quantity                    QUAN$
            if quan$ = " " and quan1$ = " " then quan$ = "ALL"
            if quan1$ = quan$ or quan$ = "ALL" then quan1$ = " "
            if quan$ = "ALL" then L52180
            convert quan$ to quan, data goto L52140
            if quan <= 9999999.99 then L52040
L52020:        errormsg$ = "Quantity may not be GREATER THAN 9999999.99"
               goto L52190
L52040:     if quan < 0 then L52080
            if quan1$ = " " then L52170
              convert quan1$ to quan1,data goto L52140
              if quan1 > 9999999.99 then L52020
L52080:       if quan >= 0 and quan1 >= 0 then L52110
                errormsg$ = "Quantity may NOT be NEGATIVE."
                goto L52190
L52110:       if quan1 > quan then L52160
                errormsg$="TO must be GREATER THAN FROM."
                goto L52190
L52140:     errormsg$ = "Enter Numeric Values."
            goto L52190
L52160:     convert quan1 to quan1$, pic(#######.##)
L52170:     convert quan to quan$, pic(#######.##)
L52180:     if quan1$ = " " then to$(8) = " "
L52190:     return

L52210
*        Test for Sort By                      SORT$
            if sort$ = " " then sort$ = "1"
            if sort$ <= "7" and sort$ >= "1" then L52260
                errormsg$ = "Enter 1, 2, 3, 4, 5, 6 or 7"
                goto L52340
L52260:     if sort$ = "1" then sortdescr$ = "(Sort By DEMAND CODE)"
            if sort$ = "2" then sortdescr$ = "(Sort By DELIVERY DATE)"
            if sort$ = "3" then sortdescr$ = "(Sort By PART CODE)"
            if sort$ = "4" then sortdescr$ = "(Sort By PLANNED COMPLETION~
        ~ DATE)"
            if sort$ = "5" then sortdescr$ = "(Sort By CUSTOMER CODE)"
            if sort$ = "6" then sortdescr$ = "(Sort By QUANTITY)"
            if sort$ = "7" then sortdescr$ = "(Sort By PRIORITY)"
L52340:     return

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
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
