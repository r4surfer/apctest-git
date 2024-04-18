        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB   V   V   AAA   RRRR   RRRR   PPPP   TTTTT   *~
            *    J    B   B  V   V  A   A  R   R  R   R  P   P    T     *~
            *    J    BBBB   V   V  AAAAA  RRRR   RRRR   PPPP     T     *~
            *  J J    B   B   V V   A   A  R   R  R   R  P        T     *~
            *   J     BBBB     V    A   A  R   R  R   R  P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBVARRPT - Prints Variance report based on material       *~
            *            purchases, material usage and price, labor     *~
            *            efficency or rate, work center efficency       *~
            *            or rate, or total job.                         *~
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
            * 12/11/85 ! ORIGINAL                                 ! RAC *~
            * 06/24/87 ! Std Cost record lth mods, reformat files ! JIM *~
            * 02/11/88 ! PRR 5884. Fixed date comparisons, etc.   ! JIM *~
            * 06/03/88 ! BOM per unit/fixed overage inversion     ! KAB *~
            * 10/04/88 ! Added line 30942                         ! RJM *~
            *          !   Fixed Std This Level costs from STCHNY !     *~
            *          !   being divided by the Qty Completed from!     *~
            *          !   the Job.  Also fixed problem where IF  !     *~
            *          !   Total by Part and Job not closed then  !     *~
            *          !   STCHNY not read and previous STD used. !     *~
            *          !   (17207)                                !     *~
            * 12/15/89 ! Corrected Range Testing                  ! MJB *~
            * 06/04/91 !(PRR 11382, 11383, 11583) MAJOR OVERHAUL  ! RJB *~
            *          !   1. Rewrote 'REPORT_TITLES' it will now !     *~
            *          !      honor all report parameters         !     *~
            *          !   2. Massive changes to 'READ_HNYMASTR'  !     *~
            *          !      it will now honor all parameters    !     *~
            *          !   3. Fixed 'TEST DATE RANGE' it now works!     *~
            *          !   4. Chged all 2 place values to 4 places!     *~
            *          !   5. Chged Print statements to handle #4 !     *~
            *          !   6. Fixed Calculation of Actual Prices  !     *~
            *          !   7. Removed any redundent code I found  !     *~
            *          !   8. Fixed anything that was broke that I!     *~
            *          !      stumbled across.                    !     *~
            *          !   9. Added call to 'ALLFREE' (new stand) !     *~
            * 07/12/91 !QC-FIXES Corrected and/or rewrote all the ! RJB *~
            *          !   control break-points for printing. This!     *~
            *          !   included the adding of two new varibles!     *~
            *          !   (HDRPRNTED$ and JOBPRNTED$)            !     *~
            * 08/14/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            hdrprnted$1,                 /* Sub-Heading Printed (Y/N)  */~
            jobprnted$1,                 /* Job Total Printed  (Y/N)   */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bom$(2)3,                    /* BOM number                 */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            cq$12,                       /* Completed Quantity         */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            desc$(7)32,                  /* Description                */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            empwc$4,                     /* Employee Work Center       */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            factor(4),                                                   ~
            firstdate$10,                /* first date                 */~
            firstjob$8,                  /* first job                  */~
            lojob$8,                     /* first job for plow         */~
            firstpart$25,                /* first part                 */~
            lopart$25,                   /* first part for plow        */~
            firstvendor$9,               /* first vendor               */~
            lovendor$9,                  /* first vendor for plow      */~
            hdrdate$45,                  /* Report date                */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            jbdate$6,                    /* Posting date               */~
            jbtime$8,                    /* Posting time               */~
            jbvtype$1,                   /* JBVALUE2 record type       */~
            jobnr$8,                     /* Job Number                 */~
            jobvar(5,3),                 /* Job Totals                 */~
            jb$(4)13,                    /* PRINT JOB TOTALS           */~
            labcost(12),                 /* Labor cost breakdown       */~
            labor$(4)4,                  /* Labor standard class codes */~
            labor_class$4,               /* labor class                */~
            labor_class1$4,              /* labor class                */~
            lastdate$10,                 /* last date                  */~
            lastjob$8,                   /* last job                   */~
            hijob$8,                     /* last job for plow          */~
            lastpart$25,                 /* last part                  */~
            hipart$25,                   /* last part for plow         */~
            lastvendor$9,                /* last vendor                */~
            hivendor$9,                  /* last vendor for plow       */~
            lbad(100),                   /* Labor class actual dollars */~
            lbau(100),                   /* Labor class actual units   */~
            lbcl$(100)4,                 /* Labor classes to report    */~
            lbsp(100),                   /* Labor class standard price */~
            lbsu(100),                   /* Labor class standard units */~
            ld1$(2)33,                   /* Sub heading description    */~
            ld2$(2)33,                   /* Sub heading description    */~
            ld3$(2)33,                   /* Sub heading description    */~
            ld4$(2)33,                   /* Sub heading description    */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* Screen Line #2             */~
            look%(2),                    /* Search position            */~
            misc(12),                                                    ~
            partnr$(2)25,                /* part number                */~
            print19$19,                  /*                            */~
            print25$25,                  /*                            */~
            print34$34,                  /*                            */~
            range1$36,                   /*                            */~
            range2$32,                   /*                            */~
            range3$75,                   /*                            */~
            rates(6), lbrates(6),                                        ~
            report1$60,                  /*                            */~
            report2$75,                  /*                            */~
            rptlevel$1,                  /* report level               */~
            rptsort$1,                   /* report sort                */~
            rpttot(3),                   /* report variance totls      */~
            rpttype$1,                   /* report type                */~
            rptvar$1,                    /* report variance            */~
            rte$(2)3,                    /* Route/standard cost number */~
            searchkey$50,                /* plow for report by         */~
            screen1$26,                  /* Screen display field       */~
            screen3$20,                  /* Screen display field       */~
            screen4$17,                  /* Screen display field       */~
            scrtitle$(8)27,              /* Screen Title Fields        */~
            stdcost(2),                  /* Part Standard cost         */~
            stdplow$50,                  /* plow for standard cost     */~
            tagnr$19,                                                    ~
            total$6,                     /* print message for rpt level*/~
            totvar(3),                   /* Total Variances            */~
            type$(5)15,                  /* Print field                */~
            var1(3),                     /* Standard cost detail       */~
            var2(3),                     /* Variance cost detail       */~
            workcenter$4,                /* Work center code           */~
            workkey$50,                  /* plow for workfile          */~
            wrk1key$50                   /* plow for workfile          */~

        dim ap$13,                       /* Actual price               */~
            au$13,                       /* Actual units               */~
            qdv$13,                      /* Quantity dollar variance   */~
            pv$13,                       /* Price variance             */~
            sp$13,                       /* Standard price             */~
            su$13,                       /* Standard units             */~
            tv$13                        /* Total Variance             */~

        dim                                                              ~
            act$(5)4,                    /* Activity Code              */~
            bom$3,                       /* Bill of Materials          */~
            bucket%(6),                  /* Standard Cost Bucket       */~
            compplowkey$31,              /* BOM Plow Key               */~
            lmult(4),                    /* Labor Class Multiplier     */~
            mkr$2,                       /* BOM Marker                 */~
            norm(5),                     /* Concurrent WC Factor       */~
            oldcompplowkey$(101)31,      /* BOM Plow Key Holder        */~
            palevel%(101),               /* Phantom Assy Level         */~
            paok$(101)1,                 /* Phantom Assy O.K.?         */~
            part$25,                     /* Part Code                  */~
            pbs%(1),                     /* Pick By Step Search Rec.   */~
            pbs$5,                       /* Pick By Step               */~
            pbs$(500)5,                  /* Pick By Steps              */~
            pbshold%(101),               /* Pick By Steps              */~
            phfact(101),                 /* Phantom Multiplier         */~
            plowkey$100,                 /* Miscellaneous Read/Plow Key*/~
            readkey$100,                 /* Miscellaneous Read/Plow Key*/~
            rtestep$(500)160,            /* Route Steps                */~
            wc$(5)4,                     /* Work Center                */~
            yield(500)                   /* Cumulative Net Yield       */~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
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
            * #01 ! SYSFILE2 ! Caelus Management System General Informa *~
            * #02 ! RTEMASTR ! Standard & alt work center routings      *~
            * #03 ! CATEGORY ! Inventory Category File                  *~
            * #04 ! JBMATER2 ! Production job material used detail file *~
            * #05 ! JBVALUE2 ! Production job value added detail file   *~
            * #06 ! JBMASTR2 ! Production job master file               *~
            * #07 ! JBCROSS2 ! Cross reference of RTE & BOM planned for *~
            * #08 ! HNYMASTR ! Inventory Master File                    *~
            * #09 ! HNYPROC  ! Inventory where procurred from detail fi *~
            * #10 ! VENDOR   ! Vendor Master File                       *~
            * #11 ! WCMASTR  ! Work Center Master                       *~
            * #12 ! BOMMASTR ! Bill of Materials Master File            *~
            * #13 ! WORKFILE ! Work File for Job Standards              *~
            * #14 ! WORKFILE ! Work File for Job Costs                  *~
            * #15 ! STCnnnnH ! Std Cost Set- Inventory Stds (STCHNY)    *~
            * #16 ! STCnnnnD ! Standard Cost Details (STCDETAL)         *~
            * #17 ! STCnnnnW ! Standard Cost Workcenter/Act (STCWCACT)  *~
            * #18 ! STCnnnnL ! Standard Costing Labor Stds (STCLABOR)   *~
            *-----+----------+------------------------------------------*~
            *  where 'nnnn' is a unique cost set identifier generated   *~
            *  by the call to STCFOPEN.                                 *~
            *************************************************************~

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #02, "RTEMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 400,                                  ~
                         keypos =   5, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 35

            select #03, "CATEGORY",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 4

            select #04, "JBMATER2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  400,                                  ~
                        keypos =    1, keylen =  22,                     ~
                        alt key 1, keypos = 23, keylen = 48              ~

            select #05, "JBVALUE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =  23                      ~

            select #06, "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1300,                                 ~
                        keypos =    1, keylen =   8                      ~

            select #07, "JBCROSS2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   94,                                  ~
                        keypos =   29, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  47,         ~
                            key  2, keypos =   48, keylen =  47          ~

            select #08, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #09, "HNYPROC",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  134,                                  ~
                        keypos =   32, keylen =  40,                     ~
                        alt key  1, keypos =    7, keylen =  65,         ~
                            key  2, keypos =    1, keylen =  40, dup,    ~
                            key  3, keypos =   41, keylen =  31, dup     ~

            select #10, "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  600,                                  ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos = 10, keylen = 30, dup

            select #11, "WCMASTR",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6

            select #12, "BOMMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos =  26, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 56

            select #13, "WORKFILE", varc, indexed, recsize = 50,         ~
                         keypos =  1, keylen = 26                        ~

            select #14, "WORKFILE", varc, indexed, recsize = 43,         ~
                         keypos =  1, keylen = 28                        ~

            select #15, "STCHNY",                                        ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos = 1,    keylen = 25

            select #16, "STCDETAL",                                      ~
                        varc,     indexed,  recsize =  340,              ~
                        keypos = 1,    keylen = 28

            select #17, "STCWCACT",                                      ~
                        varc,     indexed,  recsize =  381,              ~
                        keypos =  1,   keylen = 8

            select #18, "STCLABOR",                                      ~
                        varc,     indexed,  recsize =  323,              ~
                        keypos =  1,   keylen = 4

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#01, "SHARE", f2%(01), rslt$(01), axd$(01))
            call "OPENFILE" (#02, "SHARE", f2%(02), rslt$(02), axd$(02))
            call "OPENFILE" (#03, "SHARE", f2%(03), rslt$(03), axd$(03))
            call "OPENFILE" (#04, "SHARE", f2%(04), rslt$(04), axd$(04))
            call "OPENFILE" (#05, "SHARE", f2%(05), rslt$(05), axd$(05))
            call "OPENFILE" (#06, "SHARE", f2%(06), rslt$(06), axd$(06))
            call "OPENFILE" (#07, "SHARE", f2%(07), rslt$(07), axd$(07))
            call "OPENFILE" (#08, "SHARE", f2%(08), rslt$(08), axd$(08))
            call "OPENFILE" (#09, "SHARE", f2%(09), rslt$(09), axd$(09))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#11, "SHARE", f2%(11), rslt$(11), axd$(11))
            call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))
            call "STCFOPEN" (" ", "SSSS  ", #01, errormsg$,              ~
                #15, #16, #17, #18, #15, #15)

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date : call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Press RETURN for f~
        ~ull screen Edit."

            type$(1) = "For Material"
            type$(2) = "For Labor"
            type$(3) = "For Work Center"
            type$(4) = "For Miscel."
            type$(5) = " "

            ld1$(1) = "COMPONENT PART            DATE"
            ld1$(2) = "COMPONENT PART                "
            ld2$(1) = "CLASS W/C EMPLOYEE CODE   DATE"
            ld2$(2) = "LABOR CLASS                   "
            ld3$(1) = "WORK CENTER               DATE"
            ld3$(2) = "WORK CENTER                   "
            ld4$(1) = "OVERHEAD & MISC           DATE"
            ld4$(2) = "OVERHEAD & MISC               "

            scrtitle$(1) = "Select Report Type"
            scrtitle$(2) = "Select Variance Type"
            scrtitle$(3) = "Select Total By"
            scrtitle$(4) = "Select Report Level"
            scrtitle$(5) = "Select Date Range"
            scrtitle$(6) = "Select Part Range"
            scrtitle$(7) = "Select Job Range"
            scrtitle$(8) = "Select Part Category Range"

            str(line2$,62,18) = "JBVARRPT: " & str(cms2v$,1,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            call "ALLFREE"
            init(" ") errormsg$, inpmessage$, rpttype$, rptvar$,         ~
                      rptsort$, rptlevel$, firstdate$, lastdate$,        ~
                      firstpart$, lastpart$, firstjob$, lastjob$,        ~
                      firstvendor$, lastvendor$, lopart$, hipart$,       ~
                      lojob$, hijob$, lovendor$, hivendor$
            init(hex(ff)) lbcl$()
            mat lbau = zer
            mat lbad = zer
            mat lbsp = zer
            mat lbsu = zer
            mat rpttot = zer
            mat jobvar = zer
            mat totvar = zer

            for fieldnr% = 1 to  4
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10180
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10180:     next fieldnr%

            for fieldnr% = 1 to  4
                gosub'052(fieldnr%)
                      if enabled% = 0 then L10280
L10230:         gosub'102(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  0 then       L10230
                gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10230
L10280:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        edtpg1
L11070:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  5 then       edtpg2
                  if keyhit%  = 16 then       print_report
                  if keyhit% <>  0 then       L11070
            fieldnr% = 1

L11150:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11150
            gosub'155(fieldnr%)
                  if errormsg$ <> " " then L11150
            goto L11070

        edtpg2
L11230:     gosub'112(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  4 then       edtpg1
                  if keyhit%  = 16 then       print_report
                  if keyhit% <>  0 then       L11230
            fieldnr% = 1

L11310:     gosub'122(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11310
            gosub'156(fieldnr%)
                  if errormsg$ <> " " then L11310
            goto L11230

        REM *************************************************************~
            *            B E G I N  R E P O R T                         *~
            *                                                           *~
            * Intializes report parameters and processes report         *~
            *************************************************************

        print_report

        REM Main Logic
            call "SHOSTAT" ("Printing Variance Report")
            hdrdate$ = " " : call "DATE" addr("HD", hdrdate$)
            gosub report_titles
            if rptvar% = 4 then L12100
            call "WORKOPEN" (#13, "SHARE", 500%, f1%(13))
            call "WORKOPEN" (#14, "SHARE", 500%, f1%(14))
L12100:     call "DATUFMTC" (firstdate$)
            call "DATUFMTC" (lastdate$)
            on rptsort% gosub L12200, L12250, L12300, L12350, L12420
L12115:     call "PLOWALTS" (#n%, plowkey$, akey%, break%, f1%(n%))
                if f1%(n%) = 0 then L12180
            gosub plow_detail
            if range% = 0 then L12180
            if rptvar% = 4 then gosub job_total
            gosub print_sort_total
            goto L12115
L12180:     gosub print_totals
            goto inputmode

L12200: REM Initialize plow for sort by part code
            n% = 8       /* HNYMASTR */
            akey%, break% = 0%
            init (hex(00)) plowkey$
            str(plowkey$,,25) = str(lopart$)
            return

L12250: REM Initialize plow for sort by job code
            n% = 6       /* JBMASTR2 */
            akey%, break% = 0%
            init (hex(00)) plowkey$
            str(plowkey$,,8) = str(lojob$)
            return

L12300: REM Initialize plow to sort by Vendor
            n% = 10       /* VENDOR */
            akey%, break% = 0%
            init (hex(00)) plowkey$
            str(plowkey$,,9) = str(lovendor$)
            return

L12350: REM Initialize plow to sort by Part Category
            n% = 3         /* CATEGORY */
            akey%, break% = 0%
            init (hex(00)) plowkey$
            str(plowkey$,,4) = str(lovendor$,,4)
            return

L12420: REM Initialize plow to sort by Labor Type
            n% = 6         /* JBMASTR2 */
            akey%, break% = 0%
            init (hex(00)) plowkey$
            str(plowkey$,,25) = str(lojob$)
            return

        report_titles   /* Initialize Report Titles */
            on rpttype% gosub L13160, L13270, L13360, L13450, L13540
            on rptsort% gosub L13630, L13850, L14050, L14270, L14490
            if firstdate$ <> "ALL" then L13060
                firstdate$ = "19010101" : lastdate$ = "29991231"
                call "DATFMTC" (firstdate$) : call "DATFMTC" (lastdate$)
                range1$ = "DATE RANGE: ALL" : goto L13090
L13060:     range1$ = "DATE RANGE: " & firstdate$
            if lastdate$ = firstdate$ or lastdate$ = " " ~
                                      or lastdate$ = blankdate$ then L13090
                range1$ = range1$ & " to " & lastdate$
L13090:     call "STRING" addr("RJ", range1$, 32%)
            call "STRING" addr("RJ", range3$, 75%)
            pageno% = 0
            line% = 1000
            if rptlevel% = 1% then total$ = " TOTAL" else total$ = " "
            return

L13160: REM Report title for Material report
            on rptvar% goto L13180, L13200, L13220, L13240
L13180:     report1$ = "Material Price Variance Report"
            return
L13200:     report1$ = "Material Usage Variance Report"
            return
L13220:     report1$ = "Material Total Variance Report"
            return
L13240:     report1$ = "Purchase Price Variance Report"
            return

L13270: REM Report title for Labor report
            on rptvar% goto L13290, L13310, L13330
L13290:     report1$ = "Labor Rate Variance Report"
            return
L13310:     report1$ = "Labor Efficiency Variance Report"
            return
L13330:     report1$ = "Labor Total Variance Report"
            return

L13360: REM Report title for Work Center report
            on rptvar% goto L13380, L13400, L13420
L13380:     report1$ = "Work Center Rate Variance Report"
            return
L13400:     report1$ = "Work Center Efficiency Variance Report"
            return
L13420:     report1$ = "Work Center Total Variance Report"
            return

L13450: REM Report title for MISCELLANEOUS report
            on rptvar% goto L13470, L13490, L13510
L13470:     report1$ = "Miscellaneous Rate Variance Report"
            return
L13490:     report1$ = "Miscellaneous Efficiency Variance Report"
            return
L13510:     report1$ = "Miscellaneous Total Variance Report"
            return

L13540: REM Report title for Job report
            on rptvar% goto L13560, L13580, L13600
L13560:     report1$ = "Job Price/Rate Variance Report"
            return
L13580:     report1$ = "Job Usage/Efficiency Variance Report"
            return
L13600:     report1$ = "Job Total Variance Report"
            return

L13630: REM Report title for sort by Part
            if rptlevel% = 1% then report2$ = "Detail for"               ~
                              else report2$ = "Summary for"
            if firstpart$ <> "ALL" then L13680
                report2$ = report2$ & " All Part Numbers" : goto L13710
L13680:     report2$ = report2$ & " Part Number " & firstpart$
            if lastpart$ = firstpart$ or lastpart$ = " " then L13710
                report2$ = report2$ & " to " & lastpart$
L13710:     range2$ = " "
            if rptvar% = 4% then L13780
                if firstjob$ <> "ALL" then L13750
                     range2$ = "JOB RANGE: ALL" : goto L13780
L13750:         range2$ = "JOB RANGE: " & firstjob$
                if lastjob$ = firstjob$ or lastjob$ = " " then L13780
                     range2$ = range2$ & " to "  &  lastjob$
L13780:     if firstvendor$ <> "ALL" then L13800
                range3$ = "PART CATEGORY RANGE: ALL" : goto L13830
L13800:     range3$ = "PART CATEGORY RANGE: " & firstvendor$
            if lastvendor$ = firstvendor$ or lastvendor$ = " " then L13830
                range3$ = range3$ & " to " & lastvendor$
L13830:     return

L13850: REM Report title for sort by Job
            if rptlevel% = 1% then report2$ = "Detail for"               ~
                              else report2$ = "Summary for"
            if firstjob$ <> "ALL" then L13900
                report2$ = report2$ & " All Job Numbers" : goto L13930
L13900:     report2$ = report2$ & " Job Number " & firstjob$
            if lastjob$ = firstjob$ or lastjob$ = " " then L13930
                report2$ = report2$ & " to " & lastjob$
L13930:     if firstpart$ <> "ALL" then L13950
                range2$ = "PART RANGE: ALL" : goto L13980
L13950:     range2$ = "PART RANGE: " & firstpart$
            if lastpart$ = firstpart$ or lastpart$ = " " then L13980
                range2$ = range2$ & " TO " & lastpart$
L13980:     if firstvendor$ <> "ALL" then L14000
                range3$ = "PART CATEGORY RANGE: ALL" : goto L14030
L14000:     range3$ = "PART CATEGORY RANGE: " &  firstvendor$
            if lastvendor$ = firstvendor$ or lastvendor$ = " " then L14030
                range3$ = range3$ & " TO " & lastvendor$
L14030:     return

L14050: REM Report title for sort by Vendor
            if rptlevel% = 1% then report2$ = "Detail for"               ~
                              else report2$ = "Summary for"
            if firstvendor$ <> "ALL" then L14100
                report2$ = report2$ & " All Vendor Codes" : goto L14130
L14100:     report2$ = report2$ & " Vendor Code " & firstvendor$
            if lastvendor$ = firstvendor$ or lastvendor$ = " " then L14130
                report2$ = report2$ & " to " & lastvendor$
L14130:     range2$ = " "
            if rptvar% = 4% then L14200
                if firstjob$ <> "ALL" then L14180
                     range2$ = "JOB RANGE: ALL" : goto L14200
                range2$ = "JOB RANGE: " & firstjob$
L14180:         if lastjob$ = firstjob$ or lastjob$ = " " then L14200
                     range2$ = range2$ & " to "  &  lastjob$
L14200:     if firstpart$ <> "ALL" then L14220
                range3$ = "PART RANGE: ALL" : goto L14250
L14220:     range3$ = "PART RANGE: " & firstpart$
            if lastpart$ = firstpart$ or lastpart$ = " " then L14250
                range3$ = range3$ & " to " & lastpart$
L14250:     return

L14270: REM Report title for sort by PART CATEGORY
            if rptlevel% = 1% then report2$ = "Detail for"               ~
                              else report2$ = "Summary for"
            if firstvendor$ <> "ALL" then L14320
                report2$ = report2$ & " All Part Categories" : goto L14350
L14320:     report2$ = report2$ & " Part Caterory " & firstvendor$
            if lastvendor$ = firstvendor$ or lastvendor$ = " " then L14350
                report2$ = report2$ & " To " & lastvendor$
L14350:     range2$ = " "
            if rptvar% = 4% then L14420
                if firstjob$ <> "ALL" then L14390
                     range2$ = "JOB RANGE: ALL" : goto L14420
L14390:         range2$ = "JOB RANGE: " & firstjob$
                if lastjob$ = firstjob$ or lastjob$ = " " then L14420
                     range2$ = range2$ & " TO " & lastjob$
L14420:     if firstpart$ <> "ALL" then L14440
                range3$ = "PART RANGE: ALL" : goto L14470
L14440:     range3$ = "PART RANGE: " & firstpart$
            if lastpart$ = firstpart$ or lastpart$ = " " then L14470
                range3$ = range3$ & " TO " & lastpart$
L14470:     return

L14490: REM Report title for sort by Labor Type
            if rptlevel% = 1% then report2$ = "Detail for"               ~
                              else report2$ = "Summary for"
            if firstvendor$ <> "ALL" then L14540
                report2$ = report2$ & " All Labor Types" : goto L14570
L14540:     report2$ = report2$ & " Labor Type " & firstvendor$
            if lastvendor$ = firstvendor$ or lastvendor$ = " " then L14570
                report2$ = report2$ & " To " & lastvendor$
L14570:     if firstjob$ <> "ALL" then L14590
                range2$ = "JOB RANGE: ALL" : goto L14620
L14590:     range2$ = "JOB RANGE: " & firstjob$
            if lastjob$ = firstjob$ or lastjob$ = " " then L14620
                range2$ = range2$ & " TO " & lastjob$
L14620:     if firstpart$ <> "ALL" then L14640
                range3$ = "PART RANGE: ALL" : goto L14670
L14640:     range3$ = "PART RANGE: " & firstpart$
            if lastpart$ = firstpart$ or lastpart$ = " " then L14670
                range3$ = range3$ & " TO " & lastpart$
L14670:     return

        REM *************************************************************~
            *          Checks for range on primary file, sets plow for  *~
            *       /* detail file, and gatthers accumulative costs     *~
            *       /* for variance comparisions                        *~
            *************************************************************

        plow_detail
            range% = 1% /* First, test for out of range */
            on rptsort% gosub L15790, L15830, L15870, L15910, L15950
            if range% = 0% then return
            gosub read_hnymastr
            if rptvar% = 4 then L15720
            /* Get standards for report type; material, labor, WC, Job  */
            call "DELETE" (#13, " ", 0%)
            call "DELETE" (#14, " ", 0%)
            if dolpt <> 0 then gosub misc_costs
            gosub route_costs
            gosub bom_costs
L15720:     gosub L17560       /* print detail */
L15730:     if rptsort% = 2% or rptsort% = 5% then return
            if rptvar% = 4% then L15755
            on rptsort% gosub L17178, L15730, L17300, L17178, L15730
            goto L15720
L15755:     if rptsort% = 4 then gosub L17350 else return
            goto L15720

L15790: REM Test for out of range for part number
            if str(plowkey$,,25) > hipart$ then range% = 0
            return

L15830: REM Test for out of range for job number
            if str(plowkey$,,8) > hijob$ then range% = 0
            return

L15870: REM Test for out of range for Vendor
            if str(plowkey$,,9) > hivendor$ then range% = 0
            return

L15910: REM Test for out of range for Part category
            if str(plowkey$,,4) > str(hivendor$,,4) then range% = 0
            return

L15950: REM Test for out of range for labor type
            if str(plowkey$,2,4) > str(hivendor$,,4) then range% = 0
            return

        REM *************************************************************~
            * DERIVE ROUTE COSTS.                                       *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        route_costs
            call "STCRTE"                /*                            */~
                     (partnr$(1),        /* PART NEEDED                */~
                      rte$(1),           /* THE WC ROUTE TO USE        */~
                      bom$(1),           /* WHICH BOM TO USE           */~
                      rtestep$(),        /* THE DERIVED ROUTE          */~
                      pbs$(),            /* PICK-BY-STEPS              */~
                      rte%,              /* # OF ELEMENTS              */~
                      #12,               /* BOMMASTR                   */~
                      #02,               /* RTEMASTR                   */~
                      #15)               /* STCHNY                     */

            if rte% > 0% then L16110
               rte$(1) = " ":errors% = errors% + 1%:return
L16110:     for i% = 1% to rte%
                get rtestep$(i%) using L16145, wc$(1), setup%, run,       ~
                                 wc$(2), norm(2), wc$(3), norm(3),       ~
                                 wc$(4), norm(4), yield(i%), act$(),     ~
                                 phfactor
L16145:             FMT CH(4), POS(40), BI(2), PD(14,4), POS(50), CH(4), ~
                        PD(7,4), CH(4), PD(7,4), CH(4), PD(7,4),         ~
                        POS(80), PD(14,7), POS(94), CH(4),               ~
                        POS(136), 4*CH(4), PD(14,4)
                if rpttype% = 1% then L16260
                wcfactor = 24
                call "READ100" (#11, wc$(1), f1%(11))
                   if f1%(11) = 0% then L16200
                get #11 using L16190, wcfactor
L16190:             FMT XX(2019), BI(2)
L16200:         norm(1), norm(5) = 1 : wc$(5) = wc$(1)
                asrq = (100 * phfactor)/yield(i%)
                run = (24 * run) / wcfactor
                for step% = 1% to 4%
                   gosub step_cost
                next step%
                if act$(5) = " " then L16260
                step% = 5% : asrq = 1 : run = (24 * setup%) / wcfactor
                gosub step_cost
L16260:     next i%
            return

        REM *************************************************************~
            * DERIVE COST OF A PARTICULAR STEP.                         *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        step_cost
            if wc$(step%) = " " and act$(step%) = " " then return
            readkey$ = str(wc$(step%),,4) & act$(step%)
            call "READ100" (#17, readkey$, f1%(17))
               if f1%(17) <> 0% then L16390
            str(readkey$,1,4) = " "
            if readkey$ = " " then return
            call "READ100" (#17, readkey$, f1%(17))
               if f1%(17) <> 0% then L16390
            readkey$ = wc$(step%)
            if readkey$ = " " then return
            call "READ100" (#17, readkey$, f1%(17))
               if f1%(17) = 0% then return
L16390:     get #17 using L16395, rates(), bucket%(), labor$(), lmult()
L16395:         FMT POS(9), 6*PD(14,4), 6*BI(1), 4*CH(4), 4*PD(14,4)
            rtefactor = run * asrq * norm(step%)
            perdl, perfx = 0 : if rtefactor = 0 then L16404
            perfx = ((rates(1) + rates(2)) / stdrun) / rtefactor
            perdl = (rates(3) + rates(4)) / rtefactor
L16404:     var1(1) = perfx + perdl + rates(5) + rates(6)
            var1(2) = rtefactor
            var1(3) = var1(1) * var1(2)
            str(workkey$,,26) = "W" & str(wc$(step%),,4)
            call "READ101" (#13, workkey$, f1%(13))
            if f1%(13) = 0 then gosub L16557 else gosub L16563
            for j% = 1% to 4%
                if labor$(j%) = " " then goto L16545
                if rptsort% <> 5% then L16435
                    if labor$(j%) < lovendor$ or                         ~
                                    labor$(j%) > hivendor$ then L16545
L16435:         call "READ100" (#18, str(labor$(j%)), f1%(18))
                if f1%(18) = 0% then goto L16545
                     get #18 using L16450, lbrates()
L16450:                   FMT /* File #18- STCLABOR */ POS(5), 6*PD(14,4)
                     rtefactor = run * asrq
                     perdl, perfx = 0 : if rtefactor = 0 then L16470
                     perfx = (lbrates(1) / stdrun) / rtefactor
                     perdl = lbrates(3) / rtefactor
L16470:              var1(1) = perfx + perdl + lbrates(5)
                     var1(2) = rtefactor * lmult(j%)
                     var1(3) = var1(1) * var1(2)
                     str(workkey$,,26) = "L" & str(labor$(j%))
                     call "READ101" (#13, workkey$, f1%(13))
                     if f1%(13) = 0 then gosub L16557 else gosub L16563
                     perdl, perfx = 0 : if rtefactor = 0 then L16515
                     perfx = (lbrates(2) / stdrun) / rtefactor
                     perdl = lbrates(4) / rtefactor
L16515:              var1(1) = perfx + perdl + lbrates(6)
                     var1(2) = rtefactor * lmult(j%)
                     var1(3) = var1(1) * var1(2)
                     str(workkey$,,26) = "O" & str(labor$(j%))
                     call "READ101" (#13, workkey$, f1%(13))
                     if f1%(13) = 0 then gosub L16557 else gosub L16563
L16545:     next j%
            return

L16557:     write #13 using L37190, workkey$, var1(1), var1(2), var1(3)
            return

L16563:     gosub L31050 /* Get workfile1 record */
            var3 = var3 + var1(3)
            var2 = var2 + var1(2)
            if var2 <> 0 then var1 = (var3 / var2) else var1 = 0
            rewrite #13 using L37190, workkey$, var1, var2, var3
            return

        REM *************************************************************~
            * DERIVE COSTS INHERITED FROM LOWER LEVEL ASSEMBLIES.       *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        bom_costs

           ti%  = 1% : phfact(1%) = 1
           palevel%, palevel%(ti%) = 0%:paok$(ti%) = "Y"
           compplowkey$ = str(partnr$(1),1,25) & str(bom$(1),1,3) & "  0"
L16655:    call "PLOWNEXT" (#12, compplowkey$, 28%, f1%(12))
                if f1%(12%) <> 0% then L16690
                   bom$(1) = " " : errors% = errors% + 1%: return
L16675:    call "PLOWNEXT" (#12, compplowkey$, 28%, f1%(12))
                if f1%(12%) =  0% then return
L16690:    get #12, using L16695, part$, qu, tu, addon, fu, mkr$, pbs$
L16695:       FMT CH(25), XX(31), 4*PD(14,4), CH(2), XX(8), CH(4)
           if mkr$ = "RE" or mkr$ = "TL" then L16675
           if ti% = 1% or paok$(ti%) = "Y" then L16740
              pbs% = pbshold%(ti% - 1%)
              goto L16780
L16740:    if rte% <= 0% then L16750
           if pbs$ <> " " then L16755
L16750:       pbs% = 1%:goto L16780
L16755:    put str(pbs$,5) using L16760, palevel%(ti%)
L16760:        FMT BI(1)
           search str(pbs$(),1%, 5%*rte%) = str(pbs$,,5) to pbs%() step 5
           pbs% = min(max(1%, int((pbs%(1) + 4%)/5%)), rte%)
L16780:    qtyu  = phfact(ti%) * (qu*tu + fu)
           if rte% > 0% then qtyu  = (100 * qtyu) / yield(pbs%)
           qtyu  = qtyu + addon
           if qtyu <= 0 then L16675
           if mkr$ = "PH" or mkr$ = "PA" then goto  L16890

        REM ADD IN COSTS HERE
           call "READ100"(#15, part$, f1%(15))
              if f1%(15) = 0% then L16675
L16835:    get #15, using L16840, partprice
L16840:        FMT POS(52), PD(14,4)
            var1(1) = partprice
            var1(2) = qtyu
            var1(3) = partprice * qtyu
            str(workkey$,1,26) = "M" & part$
            call "READ101" (#13, workkey$, f1%(13))
                if f1%(13) = 0 then gosub L16557 else gosub L16563
           goto L16675

L16890: REM * * * PHANTOM LOOP LOGIC * * *

             call "READ100"(#15, part$, f1%(15))
                if f1%(15) = 0% then L16675
             get #15, using L16915, bom$
L16915:         FMT POS(38), CH(3)
             if bom$ = " " or ti% > 99% then L16835
             oldcompplowkey$(ti%) = compplowkey$
             pbshold%(ti%) = pbs%
             ti% = ti% + 1%
             if mkr$ <> "PA" or paok$(ti% - 1%) <> "Y" then L16965
                palevel%, palevel%(ti%) = palevel% + 1%
                paok$(ti%) = "Y"
L16965:      phfact(ti%) = phfact(ti%-1%) * qu * tu
             compplowkey$ = str(part$,1,25) & str(bom$,1,3) & "  0"
             gosub L16655
             paok$(ti%) = " "
             ti% = ti% - 1%
             compplowkey$ = oldcompplowkey$(ti%)
             goto L16675

        REM *************************************************************~
            * Accumulate and Store Misc. Costs.                         *~
            *************************************************************

        misc_costs
            str(workkey$,1,14) = "OMISC"
            call "READ101" (#13, workkey$, f1%(13))
            if f1%(13) = 1% then L17055
                write #13 using L37190,workkey$,dolpt,stdrun,(stdrun*dolpt)
                return
L17055:     gosub L31050
            var3 = var3 + (stdrun * dolpt)
            var2 = var2 + stdrun
            if var2 = 0 then var1 = 0 else var1 = (var3 / var2)
            rewrite #13 using L37190, workkey$, var1, var2, var3
            return

        read_hnymastr
            hdrprnted$, jobprnted$ = "N"
            init (hex(00)) searchkey$
            on rptsort% goto L17170, L17220, L17300, L17330, L17220

L17170: REM Read HNYMASTR for sort by part
            r% = 1%
            gosub L30450
            str(searchkey$,1,25) = partnr$(r%)
            if rptvar% = 4% then return
L17178:         call "PLOWALTS" (#7, searchkey$, 1%, 25%, f1%(7))
                     if f1%(7) = 0% then error_mode else gosub L30760
                if jobnr$ < lojob$ or jobnr$ > hijob$ then L17178
                call "READ100" (#6, jobnr$, f1%(6))
                     if f1%(6) = 0% then L17178 else gosub L30400
                if compqty = 0 or datestart$ > lastdate$ then L17178
                if datend$ = " " or datend$ = blankdate$ then L17196
                if datend$ < firstdate$ then L17178
L17196:     return

L17220: REM Read HNYMASTR for sort by job
            gosub L30400 /* Read JBMASTR2 */
            if jobnr$ > hijob$ or compqty = 0 then error_mode
            if partnr$(1) < lopart$ or partnr$(1) >hipart$ then error_mode
            if datestart$ > lastdate$ then error_mode
            if datend$ = " " or datend$ = blankdate$ then L17240
            if datend$ < firstdate$ then error_mode
L17240:     call "READ100" (#08, partnr$(1), f1%(8))
                if f1%(8) = 0 then error_mode
            r% = 1%
            gosub L30450 /* Get BOM & RTE from STCHNY */
            return

L17300: REM Read HNYMASTR for sort by Vendor
            return

L17330: REM Read HNYMASTR for sort by part category
            str(searchkey$,1,4) = str(plowkey$,1,4)
L17350:     call "REDALT0" (#08, searchkey$, 2%, f1%(8))
                if f1%(8) = 0 then L17500
            goto L17370
L17364:     call "READNEXT" (#08, f1%(8))
                if f1%(8) = 0 then L17500
                if key(#08,2) <> str(plowkey$,1,4) then L17500
L17370:     if key(#08,0) < lopart$ or key(#08,0) > hipart$ then L17364
            goto  L17170 /* Get HNYMASTR */

        error_mode
            if rptsort% = 4% then L17364
L17500:     return clear
            return

L17560: REM *************************************************************~
            *                P R I N T  D E T A I L                     *~
            *                                                           *~
            * Accumulates detail and prints if detail print select else *~
            * prints summary record.                                    *~
            *************************************************************


            init (hex(00)) readkey$, workkey$
            if rptvar% = 4 then L19720
            on rpttype% goto L17620, L18240, L18820, L19225, L19680

L17620: REM Build pointer in workfile2
            t% = 1%
            seqnr% = 0
            str(readkey$,,8) = jobnr$
L17645:     call "PLOWNEXT" (#04, readkey$, 8%, f1%(4))
                if f1%(4) = 0 then L17690
            gosub L30190 /* Get JBMATER2 record */
            str(workkey$,,25) = partnr$(2)
            seqnr% = seqnr% + 1
            write #14 using L37230, str(workkey$,,25), seqnr%, jbdate$,   ~
                      jbtime$, jbvtype$
            goto L17645

L17690: REM Plow standard cost workfile and check for pointer in workfile2
            init (hex(00)) readkey$, workkey$
            str(readkey$,,1) = "M"
L17705:     call "PLOWNEXT" (#13, readkey$, 1%, f1%(13))
                if f1%(13) = 0 then L17845
            gosub L30880 /* Get WORKFILE 1 record */
            mat var2 = zer
            workkey$ = str(readkey$,2,25) & hex(000000)
L17730:     call "PLOWNXT1" (#14, workkey$, 25%, f1%(14))
                if f1%(14) = 0 then L17795
            gosub L31010 /* Get WORKFILE2 */
            wrk1key$ = str(jobnr$) & str(jbdate$) & str(jbtime$)
            delete #14
            call "READ100" (#04, wrk1key$, f1%(4))
                if f1%(4) = 0 then L17730
            gosub L30190 /* Get JBMATER2 record */
            if rptlevel% = 1 then gosub mat_dtl_prt /* print record */
            var2(2) = var2(2) + qty
            var2(3) = var2(3) + (qty * matcost)
            goto L17730

L17795:     if var2(2) = 0 then var2(1) = 0 else var2(1) = var2(3)/var2(2)
            unitpricevar = var1(1) - var2(1)
            pricevar = unitpricevar * var2(2)
            qtyvar = (var1(2) * compqty) - var2(2)
            qtydolvar = qtyvar * var1(1)
            gosub dtl_var /* print variance */
            goto L17705

L17845: REM Plow Workfile2 for materials not in BOM
L17850:     init (hex(00)) readkey$, workkey$
            mat var1 = zer
            call "PLOWNXT1" (#14, readkey$, 0%, f1%(14))
                if f1%(14) = 0 then L18175
            mat var2 = zer
            goto L17890
L17880:     call "PLOWNXT1" (#14, readkey$, 25%, f1%(14))
                if f1%(14) = 0 then L17945
L17890:     gosub L31010 /* Get Workfile 2 */
            workkey$ = str(jobnr$) & str(jbdate$) & str(jbtime$)
            delete #14
            call "READ100" (#04, workkey$, f1%(4))
                if f1%(4) = 0 then L17880
            gosub L30190 /* Get JBMATER2 record */
            if rptlevel% = 1 then gosub mat_dtl_prt /* print record */
            var2(2) = var2(2) + qty
            var2(3) = var2(3) + (qty * matcost)
            goto L17880

L17945:     var2(1) = 0
            if var2(2) = 0 and var2(3) <> 0 then var2(2) = 1
            if var2(2) = 0 then L17960
            var2(1) = var2(3) / var2(2)
L17960:     unitpricevar = - var2(1)
            pricevar = unitpricevar * var2(2)
            qtyvar = - var2(2)
            qtydolvar = var1(1) * qtyvar
            gosub dtl_var /* print variance */
            goto L17850

L18175: REM print summary of material costs for JOBNR
            gosub job_total /* print summary */
            return

L18240: REM Build workfile2 for direct Labor costs from JBVALUE2
            t% = 2%
            init (hex(00)) readkey$, workkey$
            str(readkey$,,9) = str(jobnr$) & "L"
L18265:     call "PLOWNEXT" (#05, readkey$, 9%, f1%(5))
                if f1%(5) = 0 then L18315
            gosub L30240 /* Get JBVALUE2 record */
            if rptsort% <> 5% then L18280
            if labor_class1$ < lovendor$ or                              ~
                               labor_class1$ > hivendor$ then L18265
L18280:     if labcost = 0 then L18265
            str(workkey$,,4) = labor_class1$
            seqnr% = seqnr% + 1
            write #14 using L37230, str(workkey$,,4), seqnr%, jbdate$,    ~
                      jbtime$, jbvtype$
            goto L18265

L18315: REM Plow standard cost workfile and check for pointer in workfile2
            init (hex(00)) readkey$, workkey$
            str(readkey$,,1) = "L"
L18330:     call "PLOWNEXT" (#13, readkey$, 1%, f1%(13))
                if f1%(13) = 0 then L18485
            gosub L30880 /* Get WORKFILE 1 record */
            mat var2 = zer
            init (hex(00)) workkey$
            labor_class$, str(workkey$,,4) = str(readkey$,2,4)
L18355:     call "PLOWNXT1" (#14, workkey$, 4%, f1%(14))
                if f1%(14) = 0 then L18425
            gosub L31010 /* Get WORKFILE 2 record */
            wrk1key$ = str(jobnr$) & "L" & str(jbdate$) & str(jbtime$)
            delete #14
            call "READ100" (#05, wrk1key$, f1%(5))
                if f1%(5) = 0 then L18355
            gosub L30240 /* Get JBVALUE2 record */
            print25$ = str(labor_class$)&"  "&str(empwc$)&" "& empcode$
            if rptlevel% = 1 then gosub lab_dtl_prt /* print record */
            var2(2) = var2(2) + hrswork
            var2(3) = var2(3) + labcost
            goto L18355

L18425:     var2(1) = 0
            if var2(2) = 0 and var2(3) <> 0 then var2(2) = 1
            if var2(2) <> 0 then var2(1) = var2(3) / var2(2)
            unitpricevar = var1(1) - var2(1)
            pricevar = unitpricevar * var2(2)
            qtyvar   = (var1(2) * compqty) - var2(2)
            qtydolvar = qtyvar * var1(1)
            print25$ = labor_class$
            gosub dtl_var /* print variance */
            goto L18330

L18485: REM Plow Workfile2 for Labor classes not in STD
L18490:     init (hex(00)) readkey$, workkey$
            mat var1 = zer
            call "PLOWNXT1" (#14, readkey$, 0%, f1%(14))
                if f1%(14) = 0 then L18644
            mat var2 = zer
            goto L18530
L18520:     call "PLOWNXT1" (#14, readkey$, 4%, f1%(14))
                if f1%(14) = 0 then L18590
L18530:     gosub L31010 /* Get WORKFILE 2 record */
            workkey$ = str(jobnr$) & "L" & str(jbdate$) & str(jbtime$)
            delete #14
            call "READ100" (#05, workkey$, f1%(5))
                if f1%(5) = 0 then L18520
            gosub L30240 /* Get JBVALUE2 record */
            print25$ = str(labor_class1$)&"  "&str(empwc$)&" "& empcode$
            if rptlevel% = 1 then gosub lab_dtl_prt /* print record */
            var2(2) = var2(2) + hrswork
            var2(3) = var2(3) + labcost
            goto L18520

L18590:     var2(1) = 0
            if var2(2) = 0 and var2(3) <> 0 then var2(2) = 1
            if var2(2) <> 0 then var2(1) = var2(3) / var2(2)
            unitpricevar = - var2(1)
            pricevar = unitpricevar * var2(2)
            qtyvar = - var2(2)
            qtydolvar = var1(1) * qtyvar
            print25$ = labor_class1$
            gosub dtl_var /* print variance */
            goto L18490

L18644: REM summarize Labor class costs
            gosub job_total
            return

L18800: REM print summary of labor costs for JOBNR
            gosub L18175  /* print summary */
            return

L18820: REM Build workfile2 for Work Center costs from JBVALUE2
            t% = 3%
            init (hex(00)) readkey$, workkey$
            str(readkey$,,9) = str(jobnr$) & "W"
L18845:     call "PLOWNEXT" (#05, readkey$, 9%, f1%(5))
                if f1%(5) = 0 then L18895
            gosub L30240 /* Get JBVALUE2 record */
            str(workkey$,,4) = workcenter$
            seqnr% = seqnr% + 1
            write #14 using L37230, str(workkey$,,25), seqnr%, jbdate$,   ~
                      jbtime$, jbvtype$
            goto L18845

L18895: REM Plow standard cost workfile and check for pointer in workfile2
            init (hex(00)) readkey$, workkey$
            str(readkey$,,1) = "W"
L18910:     call "PLOWNEXT" (#13, readkey$, 1%, f1%(13))
                if f1%(13) = 0 then L19060
            gosub L30880 /* Get WORKFILE 1 record */
            mat var2 = zer
            print19$, str(workkey$,,4) = str(readkey$,2,4)
L18935:     call "PLOWNXT1" (#14, workkey$, 4%, f1%(14))
                if f1%(14) = 0 then L19005
            gosub L31010 /* Get WORKFILE 2 record */
            wrk1key$ = str(jobnr$) & "W" & str(jbdate$) & str(jbtime$)
            delete #14
            call "READ100" (#05, wrk1key$, f1%(5))
                if f1%(5) = 0 then L18935
            gosub L30240 /* Get JBVALUE2 record */
            print19$ = workcenter$
            if rptlevel% = 1 then gosub wc_dtl_prt /* print record */
            var2(2) = var2(2) + hrswork
            var2(3) = var2(3) + labcost
            goto L18935

L19005:     var2(1) = 0
            if var2(2) = 0 and var2(3) <> 0 then var2(2) = 1
            if var2(2) <> 0 then var2(1) = var2(3) / var2(2)
            unitpricevar = var1(1) - var2(1)
            pricevar = unitpricevar * var2(2)
            qtyvar   = (var1(2) * compqty) - var2(2)
            qtydolvar = qtyvar * var1(1)
            gosub dtl_var /* print variance */
            goto L18910

L19060:  REM Plow Workfile 2 for costs not in Standard
            init (hex(00)) readkey$, workkey$
            mat var1 = zer
            call "PLOWNXT1" (#14, workkey$, 0%, f1%(14))
            mat var2 = zer
                if f1%(14) = 0 then L18800
            goto L19105
L19095:     call "PLOWNXT1" (#14, workkey$, 4%, f1%(14))
                if f1%(14) = 0 then L19165
L19105:     gosub L31010 /* Get Workfile2 */
            wrk1key$ = str(jobnr$)& "W" & str(jbdate$)&str(jbtime$)
            delete #14
            call "READ100" (#05, wrk1key$, f1%(5))
                if f1%(5) = 0 then L19095
            gosub L30240 /* Get JBVALUE2 record */
            print19$ = workcenter$
            if rptlevel% = 1 then gosub wc_dtl_prt /* print record */
            var2(2) = var2(2) + hrswork
            var2(3) = var2(3) + labcost
            goto L19095

L19165:     var2(1) = 0
            if var2(2) = 0 and var2(3) <> 0 then var2(2) = 1
            if var2(2) <> 0 then var2(1) = var2(3) / var2(2)
            unitpricevar = var1(1) - var2(1)
            pricevar = unitpricevar * var2(2)
            qtyvar   = (var1(2) * compqty) - var2(2)
            qtydolvar = qtyvar * var1(1)
            work_center$ = workcenter$
            gosub dtl_var /* print variance */
            goto L19060

L19225: REM Build workfile2 for indirect Miscellaneous costs from JBVALUE2
            t% = 4%
            init (hex(00)) readkey$, workkey$
            str(readkey$,,8) = jobnr$
L19235:     call "PLOWNEXT" (#05, readkey$, 8%, f1%(5))
                if f1%(5) = 0 then L19257
            gosub L30240 /* GET JBVALUE2 RECORD */
            if jbvtype$ = "W" then goto L19235
            if jbvtype$ = "M" then goto L19244
            if laboh = 0 then L19235
            str(workkey$,1,26) = labor_class1$
            goto L19247
L19244:     str(workkey$,1,26) = "MISC"
L19247:     seqnr% = seqnr% + 1
            write #14 using L37230, str(workkey$,,25), seqnr%, jbdate$,   ~
                      jbtime$, jbvtype$
            goto L19235

L19257: REM Get plow workfile2 for Work Center cost detail
            init (hex(00)) readkey$, workkey$, wrk1key$
            str(readkey$,,1) = "O"
L19264:     mat var1 = zer :  mat var2 = zer
            call "PLOWNEXT" (#13, readkey$, 1%, f1%(13))
                if f1%(13) = 0 then L19351
            gosub L30880 /* Get WORKFILE 1 record */
            init (hex(00)) workkey$
            print19$, str(workkey$,,4) = str(readkey$,2,4)
L19283:     call "PLOWNXT1" (#14, workkey$, 4%, f1%(14))
                if f1%(14) = 0 then L19323
            gosub L31010 /* Get WORKFILE 2 record */
            wrk1key$ = str(jobnr$)&str(jbvtype$)&str(jbdate$)&str(jbtime$)
            delete #14
            call "READ100" (#05, wrk1key$, f1%(5))
                if f1%(5) = 0 then L19283
            gosub L30240 /* Get JBVALUE2 record */
            workcenter$ = str(readkey$,2,4)
            if labor_class1$ = " " then labor_class1$ = "FREE"
            workcenter$ = labor_class1$
            print19$ = workcenter$
            wcdate$ = workdate$
            wccost = laboh
            if jbvtype$ <> "M" then wchrs = hrswork else wchrs = 0
            if rptlevel% = 1 then gosub wc_dtl_prt /* print record */
            if jbvtype$ <> "M" then var2(2) = var2(2) + hrswork
            var2(3) = var2(3) + laboh
            goto L19283

L19323:     if var2(2) = 0 and var2(3) <> 0 then var2(2) = compqty
            if var2(2) <> 0 then var2(1) = var2(3) / var2(2)
            unitpricevar = var1(1) - var2(1)
            pricevar = unitpricevar * var2(2)
        REM VAR1(2) = VAR1(2) / (MAKEQTY*COMPQTY/MAKEQTY)
        REM QTYVAR = (VAR1(2)*(MAKEQTY*COMPQTY/MAKEQTY))-VAR2(2)
            qtyvar = (var1(2)* compqty) - var2(2)
            qtydolvar = qtyvar * var1(1)
            gosub dtl_var /* print variance */
            goto L19264

L19349: REM Get workfile2 for Work Center indirect costs not in STD
L19351:     init (hex(00)) readkey$, workkey$
            mat var1 = zer
            call "PLOWNXT1" (#14, workkey$, 0%, f1%(14))
            mat var2 = zer
                if f1%(14) = 0 then L19429
            goto L19369
            workcenter$, str(workkey$,,4) = str(readkey$,2,4)
L19365:     call "PLOWNXT1" (#14, workkey$, 4%, f1%(14))
                if f1%(14) = 0 then L19405
L19369:     gosub L31010 /* Get WORKFILE 2 record */
            readkey$ = str(jobnr$)&str(jbvtype$)&str(jbdate$)&str(jbtime$)
            delete #14
            call "READ100" (#05, readkey$, f1%(5))
                if f1%(5) = 0 then L19365
            gosub L30240 /* Get JBVALUE2 record */
            if jbvtype$ = "L" and f1%(18) = 0 then L19349
            wccost = laboh
            if labor_class1$ = " " then labor_class1$ = "FREE"
            workcenter$ = labor_class1$
            print19$ = workcenter$
            if workcenter$ = " " then print19$ = "DIRECT & MISC COSTS"
            wcdate$ = workdate$
            wccost = laboh
            if jbvtype$ <> "M" then wchrs = hrswork else wchrs = 0
            if rptlevel% = 1 then gosub wc_dtl_prt /* print record */
            if jbvtype$ <> "M" then var2(2) = var2(2) + hrswork
            var2(3) = var2(3) + laboh
            goto L19365

L19405:     if jbvtype$ <> "M" then L19409
            var2(1) = 0
            var2(2) = compqty
L19409:     if var2(2) <> 0 then var2(1) = var2(3) / var2(2)
            unitpricevar = var1(1) - var2(1)
            pricevar = unitpricevar * var2(2)
            var1(2) = var1(2) / compqty
            qtyvar   = (var1(2)*compqty)-var2(2)
            qtydolvar = qtyvar * var1(1)
            gosub dtl_var /* print variance */
            goto L19349

L19429: REM print summary of overhead costs for JOBNR
            gosub job_total /* print summary */
            return

L19680: REM Print All Job detail
            gosub L17620 /* Print Material detail */
            if rptsort% = 2% then hdrprnted$ = "N"
            gosub L18240 /* Print Labor detail    */
            if rptsort% = 2% then hdrprnted$ = "N"
            gosub L18820 /* Print Work Ctr detail */
            if rptsort% = 2% then hdrprnted$ = "N"
            gosub L19225 /* Print Miscellaneous detail */
            t% = 5%
            gosub job_total /* Print Job totals      */
            return

L19720: REM Print Material Purchase Detail
            init (hex(00)) readkey$, workkey$, searchkey$
            t% = 1%
            if rptsort% = 4 then L19960
            on rptsort% gosub L19880, L19910, L19925
L19745:     call "PLOWALTS" (#09, readkey$, akey1%, break2%, f1%(9%))
                if f1%(9%) = 0 then return
            if firstpart$ = "ALL" then L19757
            if str(readkey$,10,25) > lastpart$ then L19745
L19757:     mat var1 = zer
            mat var2 = zer
            goto L19775
L19765:     call "PLOWALTS" (#09, readkey$, akey1%, break1%, f1%(9%))
                if f1%(9%) = 0 then L19820
L19775:     gosub L30700 /* Get HNYPROC record */
            if recdate$ < firstdate$ or recdate$ > lastdate$ then L19810
            if rptlevel% = 1 then gosub pur_dtl /* Print detail */
            var2(3) = var2(3) + (matcost * qty)
            var1(3) = var1(3) + (matstd * qty)
            var2(2) = var2(2) + qty
L19810:     goto L19765

L19820:     var2(1), var1(1) = 0
            if var2(2) = 0 then L19870
            var1(2) = var2(2)
            var1(1) = var1(3) / var2(2)
            var2(1) = var2(3) / var2(2)
            unitpricevar = var1(1) - var2(1)
            pricevar = unitpricevar * var2(2)
            qtyvar   = (var1(2) * compqty) - var2(2)
            qtydolvar = qtyvar * var1(1)
            gosub dtl_var /* Print Variance */
L19870:     if rptsort% = 3 then L19745 else return

L19880: REM Initialize Plow by Part
            str(readkey$,,25) = str(plowkey$,,25)
            akey1% = 1
            break1% , break2% = 25%
            return

L19910: REM Initialize Plow by Job
            return

L19925: REM Initialize Plow by Vendor
            str(readkey$,,34) = str(plowkey$,,9) & str(lopart$)
            akey1% = 0
            break1% = 34%
            break2% = 9%
            return

L19960: REM Initialize Plow by Part Cat
            str(searchkey$,1,4) = str(plowkey$,1,4)
            akey1% = 1
            break1% , break2% = 25%
            call "REDALT0" (#08, searchkey$, 2%, f1%(8))
                if f1%(8) = 0 then L19989
            goto L19976
L19970:     call "READNEXT" (#08, f1%(8))
                if f1%(8) = 0 then L19989
                if key(#08,2) <> str(plowkey$,1,4) then L19989
L19976:     if key(#08,0) < lopart$ or key(#08,0) > hipart$ then L19970
            init (hex(00)) readkey$
            str(readkey$,,25), partnr$(1) = key(#08,0)
            gosub L19745
            goto L19970
L19989:     return clear
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* report type      */~
                                    L20200,         /* report variance  */~
                                    L20300,         /* report sort      */~
                                    L20400          /* report level     */
                     return
L20100:     REM DEFAULT/ENABLE FOR report type
                return
L20200:     REM DEFAULT/ENABLE FOR report variance
                return
L20300:     REM DEFAULT/ENABLE FOR report sort
                return
L20400:     REM DEFAULT/ENABLE FOR report level
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 2 OF INPUT. *~
            *************************************************************

            deffn'052(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L21100,         /* Date Range       */~
                                    L21300,         /* Part Range       */~
                                    L21500,         /* Job Range        */~
                                    L21700          /* Vendor Range     */
                     return

L21100:     REM DEFAULT/ENABLE FOR Date Range
                firstdate$ = "ALL"
                return

L21300:     REM DEFAULT/ENABLE FOR Part Range
                firstpart$ = "ALL"
                return

L21500:     REM DEFAULT/ENABLE FOR Job Range
                if rptvar$ <> "4" then L21540
                enabled% = 0
                return
L21540:         firstjob$ = "ALL"
                return

L21700:     REM DEFAULT/ENABLE FOR first vendor
                firstvendor$ = "ALL"
                return

        REM *************************************************************~
            *                 P R I N T  R O U T I N E S                *~
            *                                                           *~
            *                                                           *~
            *************************************************************

        mat_dtl_prt
            if line% > 57 or hdrprnted$ = "N" then gosub sub_heading
            convert qty to qty$, pic (-#####.####)
            convert matcost to matcost$, pic (-#####.####)
            if partnr$(2)= "DIRECT & MISC COSTS" then qty$= "        N/A"
            prtdate$ = jbdate$
            call "DATEFMT" (prtdate$)
            on rptvar% gosub L22090, L22120, L22145, L22145
            line% = line% + 1
            return
L22090:     print using L22150, partnr$(2), prtdate$, matcost$, qty$
            return
L22120:     print using L22125, partnr$(2), prtdate$, qty$
L22125: % ######################### ######## ###########
            return
L22145:     print using L22150, partnr$(2), prtdate$, matcost$, qty$
L22150: % ######################### ######## ###########   ###########
            return

        lab_dtl_prt
            if line% > 57 or hdrprnted$ = "N" then gosub sub_heading
            convert hrswork to hrswork$, pic (-#####.####)
            convert earnrate to labcost$, pic (-#####.####)
            if print25$="DIRECT & MISC COSTS" then hrswork$="        N/A"
            prtdate$ = workdate$
            call "DATEFMT" (prtdate$)
            on rptvar% gosub L22220, L22245, L22270, L22270
            line% = line% + 1
            return
L22220:     print using L22275, print25$, prtdate$, labcost$, hrswork$
            return
L22245:     print using L22250, print25$, prtdate$, hrswork$
L22250: % ######################### ######## ###########
            return
L22270:     print using L22275, print25$, prtdate$, labcost$, hrswork$
L22275: % ######################### ######## ###########   ###########
            return

        wc_dtl_prt
            if line% > 57 or hdrprnted$ = "N" then gosub sub_heading
            convert wchrs to wchrs$, pic (-#####.####)
            if wchrs = 0 then rate = 1 else rate = wchrs
            convert (wccost / rate) to wccost$, pic (-#####.####)
            if print19$ = "DIRECT & MISC COSTS" then wchrs$= "        N/A"
            prtdate$ = wcdate$
            call "DATEFMT" (prtdate$)
            on rptvar% gosub L22345, L22375, L22405, L22405
            line% = line% + 1
            return
L22345:     print using L22415, print19$, prtdate$, wccost$, wchrs$
            return
L22375:     print using L22385, print19$, prtdate$, wchrs$
L22385: % ###################       ######## ###########
            return
L22405:     print using L22415, print19$, prtdate$, wccost$, wchrs$
L22415: % ###################       ######## ###########   ###########
            return

        pur_dtl
            if line% > 57 or hdrprnted$ = "N" then gosub sub_heading
            convert matstd to matstd$, pic (-#####.####)
            convert qty to qty$, pic (-#####.####)
            convert matcost to matcost$, pic (-#####.####)
            convert (matstd - matcost) to purvar$, pic (-#####.####)
            convert ((matstd - matcost)*qty) to totvar$, pic (-#####.####)
            prtdate$ = recdate$
            call "DATEFMT" (prtdate$)
            print using L22576, str(partnr$(1)) & prtdate$, matcost$,     ~
                               matstd$, purvar$, qty$, totvar$
L22576: % ################################## ############# ##############~
        ~#      #############     #############                  #########~
        ~####
            line% = line% + 1
            return

        dtl_var
            totcnt% = totcnt% + 1
            if str(print25$,,4) = " " then str(print25$,,4) = "FREE"
            if line% > 57 or hdrprnted$ = "N" then gosub sub_heading
            convert unitpricevar to upv$, pic(-#####.####)
            convert (round(pricevar,4)) to pv$, pic(-#######.####)
            convert qtyvar to qv$, pic(-#######.####)
            convert (round(qtydolvar,4)) to qdv$, pic(-#######.####)
            convert var1(1) to sp$, pic(-#####.####)
            convert var2(1) to ap$, pic(-#####.####)
            if var1(1) = 0 then convert var1(1) to su$, pic(-#####.####) ~
                 else convert var1(2)*compqty to su$,pic(-#####.####)
            convert var2(2) to au$, pic(-#####.####)
            if t% = 1% then print34$ = partnr$(2) & total$
            if t% = 2% then print34$ = print25$ & total$
            if t% = 3% then print34$ = print19$ & total$
            if t% = 4% then print34$ = print19$ & total$
            if rptvar% = 4% then print34$ = partnr$(1) & total$
            if rptlevel% = 1 then print
            on rptvar% gosub L23040, L23070, L23085, L23040
            if rptlevel% = 1 then print
            if rptlevel%=1 then line% = line% + 3% else line% = line% + 1%
            jobvar(t%, 1%) = jobvar(t%, 1%) + round(pricevar,4)
            jobvar(t%, 2%) = jobvar(t%, 2%) + round(qtydolvar,4)
            jobvar(t%, 3%) = jobvar(t%, 3%) + round(qtydolvar+pricevar,4)
            jobvar(5%, 1%) = jobvar(5%, 1%) + round(pricevar,4)
            jobvar(5%, 2%) = jobvar(5%, 2%) + round(qtydolvar,4)
            jobvar(5%, 3%) = jobvar(5%, 3%) + round(qtydolvar+pricevar,4)
            if rptsort% = 5% then gosub accum_labor_class
            print19$, print25$, print34$ = " "
            return
L23040:     print using L23045, print34$, ap$, sp$, upv$, au$, pv$
L23045: % ################################## ############# ##############~
        ~#      #############     #############                ###########~
        ~##
            return

L23070:     print using L23071, print34$, au$, su$, qv$, sp$, qdv$
L23071: % ################################## #############          #####~
        ~########## #############       #############          ###########~
        ~##
            return

L23085:     convert(round(pricevar+qtydolvar,4)) to tv$,pic(-#######.####)
L23090:     print using L23100, print34$, ap$, au$, sp$, su$, pv$, qdv$,tv$
            return
L23100: % ################################## ############# ############# ~
        ~############ ############# ############# ############# ##########~
        ~###

        job_total
            jobprnted$ = "Y"
            if rptvar% = 4% then L23979
            if line% > 58% or hdrprnted$ = "N" then gosub sub_heading
            if rptsort% <> 2% then hdrprnted$ = "N"
            convert jobvar(t%, 1%) to jb$(1), pic(-#######.####)
            convert jobvar(t%, 2%) to jb$(2), pic(-#######.####)
            convert jobvar(t%, 3%) to jb$(3), pic(-#######.####)
            if rptvar% = 3% then print using L23986, jobnr$, type$(t%),   ~
                    jb$(1), jb$(2), jb$(3) else                          ~
              print using L23986, jobnr$, type$(t%), " ", " ", jb$(rptvar%)
            print
            line% = line% + 2%
            if line% < 51% then print using L24090 else line% = 60%
            line% = line% + 1%
            if t% = 5% then L23982
L23979:     totvar(1%) = totvar(1%) + round(jobvar(t%, 1%),4)
            totvar(2%) = totvar(2%) + round(jobvar(t%, 2%),4)
            totvar(3%) = totvar(3%) + round(jobvar(t%, 3%),4)
L23982:     jobvar(t%, 1%), jobvar(t%, 2%), jobvar(t%, 3%) = 0
            if rptvar% = 4% then  convert jobvar(t%, 1%) to              ~
                    jb$(4), pic(-#######.####)
            return
L23986: % JOB ######## TOTAL ################                            ~
        ~                          ############# ############# ###########~
        ~##

        heading
            hdrprnted$ = "N"
            select printer (134)
            pageno% = pageno% + 1
            print page
            print using L24035, report1$, hdrdate$, pageno%
            print using L24050, report2$, range1$
            print using L24075, range2$, range3$
            print using L24090
            line% = 4%
            if rptvar% = 4% then L24118
            return
L24035: % REPORT: JBVARRPT        #######################################~
        ~        ################################################  PAGE ##~
        ~###
L24050: %                         #######################################~
        ~############################### #################################~
        ~###
L24075: % #################################                       #######~
        ~#################################################################~
        ~###
L24090: % +--------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--+
        % !                                                              ~
        ~                                                                 ~
        ~  !
L24118:     if rptlevel% = 1 then print using L24320,                     ~
               "   PURCHASED PART       DATE   " else                    ~
               print using L24320, "   PURCHASED PART              "
            print using L24090
            line% = line% + 2%
            return

        sub_heading
            if line% > 51% then gosub heading
            if hdrprnted$ = "Y" then return
            hdrprnted$ = "Y"
            if rptvar% = 4 then return
            if rpttype% = 5% and t% > 1% and line% > 4% then L24162
            call "DATEOK" (datestart$, 0%, " ")
            call "DATEOK" (   datend$, 0%, " ")
            call "DATEOK" (  datplst$, 0%, " ")
            call "DATEOK" ( datplend$, 0%, " ")
            convert compqty to cq$, pic(-###########)
            convert makeqty to mq$, pic(-###########)
            print using L24255, jobnr$, partnr$(1), desc$(1), mq$
            print using L24266, datplst$, datplend$, datestart$, datend$, ~
                               cq$
            print using L24090
            line% = line% + 3%
L24162:     on t% gosub L24165, L24215, L24235, L24248, L24525
            line% = line% + 2%
            return
L24165:     if rptvar% = 1 then print using L24320, ld1$(rptlevel%)
            if rptvar% = 2 then print using L24410, ld1$(rptlevel%)
            if rptvar% = 3 then print using L24270, ld1$(rptlevel%)
            print using L24090
            return
L24215:     if rptvar% = 1 then print using L24470, ld2$(rptlevel%)
            if rptvar% = 2 then print using L24500, ld2$(rptlevel%)
            if rptvar% = 3 then print using L24440, ld2$(rptlevel%)
            print using L24090
            return
L24235:     if rptvar% = 1 then print using L24470, ld3$(rptlevel%)
            if rptvar% = 2 then print using L24500, ld3$(rptlevel%)
            if rptvar% = 3 then print using L24440, ld3$(rptlevel%)
            print using L24090
            return
L24248:     if rptvar% = 1 then print using L24470, ld4$(rptlevel%)
            if rptvar% = 2 then print using L24500, ld4$(rptlevel%)
            if rptvar% = 3 then print using L24440, ld4$(rptlevel%)
            print using L24090
            return
L24255: % ! JOB: ########  PART: #########################  DESCRIPTION: ~
        ~##############################    QUANTITY TO MAKE: #############~
        ~  !
L24266: % ! PLAN START DATE: ######## PLAN END DATE: ######## ACT START D~
        ~ATE: ######## ACT END DATE: ########  QTY COMPLETE: #############~
        ~  !
L24270: % ! ##############################  ACTUAL PRICE  ACTUAL USAGE   ~
        ~ STD PRICE    STD USAGE       PRICE VAR     USAGE VAR    TOTAL VA~
        ~R !
L24320: % ! ##############################  ACTUAL PRICE     STD PRICE   ~
        ~    PRICE VARIANCE      ACTUAL USAGE                TOTAL VARIANC~
        ~E !
L24410: % ! ##############################  ACTUAL USAGE         STANDARD~
        ~ USAGE    USAGE VARIANCE    STANDARD PRICE         TOTAL VARIANCE~
        ~  !
L24440: % ! ##############################   ACTUAL RATE  ACTUAL USAGE   ~
        ~  STD RATE    STD USAGE        RATE VAR     EFFIC VAR    TOTAL VA~
        ~R !
L24470: % ! ##############################   ACTUAL RATE      STD RATE   ~
        ~     RATE VARIANCE      ACTUAL USAGE                TOTAL VARIANC~
        ~E !
L24500: % ! ##############################  ACTUAL USAGE         STANDARD~
        ~ USAGE    EFFIC VARIANCE     STANDARD RATE          TOTAL VARIANC~
        ~E !
L24525:     line% = line% - 2%
            return

        print_sort_total
            if jobprnted$ = "N" then return
            jobprnted$ = "N"
            if rptsort% = 2% or rptsort% = 5% then L25130
            if rptvar% = 4 and rptsort% = 1% then L25130
            hdrprnted$ = "Y"
            if line% > 57 then gosub sub_heading
            convert totvar(1%) to jb$(1), pic(-#######.####)
            convert totvar(2%) to jb$(2), pic(-#######.####)
            convert totvar(3%) to jb$(3), pic(-#######.####)
            convert totvar(1%) to jb$(4), pic(-#######.####)
            print
            if rptsort% = 1 then print34$ = "PART " & plowkey$ & " TOTAL"
            if rptsort% = 3 then print34$ = "VENDOR "& plowkey$ & " TOTAL"
            if rptsort% = 4 then print34$ = "PART CATEGORY " &           ~
                                            plowkey$ & " TOTAL"
            if rptvar% = 3% then print using L25190, print34$,            ~
                    jb$(1), jb$(2), jb$(3) else                          ~
              print using L25190, print34$, " ", " ", jb$(rptvar%)
            print
            line% = line% + 3
            if line% < 51% then print using L24090 else line% = 60%
            line% = line% + 1
L25130:     rpttot(1%) = rpttot(1%) + totvar(1%)
            rpttot(2%) = rpttot(2%) + totvar(2%)
            rpttot(3%) = rpttot(3%) + totvar(3%)
            mat totvar = zer
            rpttot% = rpttot% + 1
            totcnt% = 0
            return
L25190: % ##################################                             ~
        ~                          ############# ############# ###########~
        ~##
            return

        print_totals
            if rptsort% = 5% then gosub L26000
            if line% > 57 then gosub sub_heading
            convert rpttot(1%) to jb$(1), pic(-#######.####)
            convert rpttot(2%) to jb$(2), pic(-#######.####)
            convert rpttot(3%) to jb$(3), pic(-#######.####)
            convert rpttot(1%) to jb$(4), pic(-#######.####)
            print
            print34$ = "REPORT TOTALS"
            if rptvar% = 3% then print using L25710, print34$,            ~
                    jb$(1), jb$(2), jb$(3) else                          ~
              print using L25710, print34$, " ", " ", jb$(rptvar%)
            print
            print using L24090
            mat rpttot = zer
            close printer
            if rptvar% = 4% then L25700
            call "FILEBGON" (#13)
            call "FILEBGON" (#14)
            rpttot% = 0%
L25700:     return
L25710: % ##################################                             ~
        ~                          ############# ############# ###########~
        ~##

L26000: REM LABOR_CLASS_TOTALS
            if cnt% = 0 then return
            if line% > 57 then gosub sub_heading
            print
            print using L26190
            print
            line% = line% + 3
            for i% = 1 to cnt%
                if line% > 57 then gosub sub_heading
                convert lbau(i%) to au$, pic (-#####.####)
                if lbau(i%) = 0 then ap$ = "        .00" else            ~
                convert (lbad(i%)/lbau(i%)) to ap$, pic (-#####.####)
                if lbau(i%) = 0 then upv$ = "        .00" else           ~
                convert (lbsp(i%) - (lbad(i%)/lbau(i%))) to upv$,        ~
                        pic (-#######.####)
                if lbau(i%) = 0 then  pv$ = "        .00" else           ~
                convert ((lbsp(i%) - (lbad(i%)/lbau(i%))) * lbau(i%))    ~
                        to pv$, pic (-#######.####)
                convert lbsp(i%) to sp$, pic (-#####.####)
                convert (lbsu(i%) - lbau(i%)) to qv$, pic (-#######.##)
                convert ((lbsu(i%) - lbau(i%)) * lbsp(i%)) to qdv$,      ~
                        pic (-#######.####)
                convert lbsu(i%) to su$, pic (-#####.####)
                convert pv$ to pv
                convert qdv$ to qdv
                convert (round(pv + qdv,2)) to tv$, pic (-#######.####)
                if lbcl$(i%) = hex(eeeeeeee) then print34$ =             ~
                   "DIRECT & MISC COSTS" else print34$ = lbcl$(i%)
                on rptvar% gosub L23040, L23070, L23090
                line% = line% + 1
            next i%
            cnt% = 0
            return
L26190: % TOTAL BY LABOR CLASS:


        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *               G E T   S T A T E M E N T S                 *~
            *************************************************************

        REM Get STCWCACT record
            get #17 using L35240, rates(), labor$(), factor()
            perdl, perfx = 0 : if rtefactor = 0 then L30130
            perfx = ((rates(1) + rates(2)) / stdrun) / rtefactor
            perdl = (rates(3) + rates(4)) / rtefactor
L30130:     var1(1) = perfx + perdl + rates(5) + rates(6)
            var1(2) = rtefactor
            var1(3) = var1(1) * var1(2)
            str(workkey$,,26) = "W" & str(workcenter$)
            return

L30190: REM GET JBMATER2 RECORD
            get #04 using L35300, jbdate$, jbtime$, partnr$(2), qty,      ~
                   matcost, desc$(2)
            if qty = 0 then mqty = 1 else mqty = qty
            matcost = matcost / mqty
            return

L30240: REM GET JBVALUE2 RECORD
            get #05 using L35535, jbvtype$, jbdate$, jbtime$, labcost,    ~
                labcost(), desc$(7), empwc$, empcode$, labor_class1$,    ~
                earnrate, hrswork
            if jbvtype$ <> "M" then L30280
                earnrate = stdrun
                laboh = labcost
                labor_class1$ = "MISC"
                return
L30280:     workdate$, wcdate$ = jbdate$
            workcenter$ = empwc$
        REM Recalculate EARNRATE & OVERHEAD costs
            if hrswork = 0 then hrswork = 1
            laboh = 0
            call "READ100" (#18, str(labor_class1$), f1%(18))
            if f1%(18) = 0% then return
                get #18 using L30350, lbrbkt%, ohdbkt%
L30350:              FMT /* File #18- STCLABOR */ POS(59), 2*BI(1)
                if lbrbkt% > 0% then L30358
                     earnrate, labcost = 0
                     goto L30362
L30358:         earnrate = labcost(lbrbkt%) / hrswork
                labcost  = labcost(lbrbkt%)
L30362:         if ohdbkt% > 0% then L30368
                     laboh = 0
                     goto L30370
L30368:         laboh    = labcost(ohdbkt%)
L30370:         return

L30400: REM GET JBMASTR2 RECORD
            get #06 using L35800, jobnr$, desc$(6), partnr$(1), makeqty,  ~
                   compqty, datestart$, datend$, datplst$, datplend$
            return

L30450: REM Get HNYMASTR, TOTAL STD. COST, BOM, & ROUTE
            get #08 using L36110, partnr$(r%), desc$(r%)
            call "STCCOSTS" (partnr$(r%), " ", #01, 1%, stdcost(r%))
            if rptvar% = 4% then return
            stdplow$ = partnr$(r%)
            bom$(r%), rte$(r%) = " "
            dolpt, stdrun = 0
            call "READ100" (#15, stdplow$, f1%(15))
            if f1%(15) = 0% then return
            get #15 using L30550, bom$(r%), rte$(r%), stdrun, misc()
L30550:         FMT /* File #15- STCnnnnH (STCHNY) */ POS(38), 2*CH(3),  ~
                     PD(14,4), POS(252), 12*PD(14,4)
            dolpt = misc( 1) + misc( 2) +  misc( 3) + misc( 4) + misc( 5)~
                + misc( 6) + misc( 7) +  misc( 8) + misc( 9) + misc(10) +~
                misc(11) + misc(12)
            if stdrun = 0 then stdrun = 1
            return

L30700: REM GET HNYPROC RECORD
            get #09 using L36565, recdate$, partnr$(1), qty, matcost,     ~
                matstd
            makeqty = qty
            return

L30760: REM GET JBCROSS2 RECORD
            get #07 using L36035, tagnr$
            jobnr$ = str(tagnr$,12,8)
            return

        REM GET VENDOR RECORD
            return

L30880: REM GET WORKFILE 1 RECORD
            get #13 using L37190, wrk1key$, var1(1), var1(2), var1(3)
            if t% = 1% then partnr$(2) = str(wrk1key$,2,25)
            if t% = 2% then labor_class$ = str(wrk1key$,2,4)
            if t% = 3% then work_center$ = str(wrk1key$,2,4)
            if t% = 4% then work_center$ = str(wrk1key$,2,4)
*           IF STR(WRK1KEY$,2,4) <> "MISC" THEN 30990
*               IF VAR1(3) <> 0 THEN VAR1(1) = VAR1(3) / COMPQTY
*               VAR1(2) = 1
            return

L31010: REM GET WORKFILE 2 RECORD
            get #14 using L37230, wrk1key$, seqnr%, jbdate$, jbtime$,     ~
                jbvtype$
            return

L31050: REM GET WORKFILE 1 RECORD for duplicate standard record
            get #13 using L37190, wrk1key$, var1, var2, var3
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

L35240: FMT                 /* File #17- STCWCACT                      */~
            POS(9), 6*PD(14,4), /* Standard rates                      */~
            POS(63), 4*CH(4),   /* Labor class codes                   */~
            POS(79), 4*PD(14,4) /* Factors                             */

L35300: FMT                 /* File #04- JBMATER2                      */~
            POS(9), CH(6),  /* system (clock) date from the computer   */~
            CH(8),          /* Time from the system clock              */~
            CH(25),         /* Part code                               */~
            POS(71), PD(14,4), /* Quantity in packed decimal form      */~
            PD(14,4),       /* Total per unit INVENTORY cost           */~
            POS(287), CH(40)/* Posting text                            */

L35535: FMT                 /* File #05- JBVALUE2                      */~
            POS(9), CH(1),  /* Record type 'L', 'W' or 'M'             */~
            CH(6),          /* system (clock) date from the computer   */~
            CH(8),          /* Time from the system clock              */~
            POS(33),PD(14,4),/* Total labor cost                       */~
            12*PD(14,4),    /* Labor cost breakdown                    */~
            CH(40),         /* Posting text                            */~
            CH(4),          /* Work Center code                        */~
            CH(12),         /* Employee code                           */~
            POS(205),CH(4), /* Labor class                             */~
            POS(219),PD(14,4), /* Earnings rate per labor unit         */~
            PD(14,4)        /* Total labor hours worked                */

L35800: FMT                 /* File #06- JBMASTR2                      */~
            CH(8),          /* Production job code                     */~
            CH(30),         /* Description of production job           */~
            POS(58), CH(25),/* Part code                               */~
            PD(14,4),       /* Quantity to make                        */~
            PD(14,4),       /* Quantity completed to date              */~
            POS(147), CH(6),/* Date production job actually started    */~
            CH(6),          /* Date production job actually ended      */~
            POS(168), CH(6),/* Date production job planned to start    */~
            CH(6)           /* Date production job planned to be comple*/

L36035: FMT                 /* File #07- JBCROSS2                      */~
            POS(29), CH(19) /* Tag number in level 2 planning          */

L36110: FMT                 /* File #08- HNYMASTR                      */~
            CH(25),         /* Part Number                             */~
            CH(32)          /* part number description                 */

L36565: FMT                 /* File #09- HNYPROC                       */~
            CH(6),          /* Date quantity received in HNYPROC       */~
            CH(25),         /* Part code                               */~
            POS(72), PD(14,4),/* Quantity in packed decimal form       */~
            PD(14,4),       /* Inventory cost per unit                 */~
            POS(110), PD(14,4) /* Current STD cost per unit            */

L37190: FMT                 /* File #13- WORKFILE 1                    */~
            CH(26),         /* Key                                     */~
            3*PD(14,4)      /* PRICE, UNITS, TOTAL                     */

L37230: FMT                 /* File #14- WORKFILE 2 Actual detail      */~
            CH(25),         /* Key                                     */~
            BI(3),          /* sequence number                         */~
            CH(6),          /* Date                                    */~
            CH(8),          /* Time                                    */~
            CH(1)           /* JBVALUE record type                     */

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  str(line2$,1,50) = "Input Selection Criteria, Screen 1"
                  on fieldnr% gosub L40100,         /* report type      */~
                                    L40100,         /* report variance  */~
                                    L40100,         /* report sort      */~
                                    L40100          /* report level     */
                     goto L40120

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40100:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40120:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Variance Reports",                              ~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,10), fac(hex(a4)), scrtitle$(1)           , ch(27),~
               at (06,44), fac(hex(a4)), scrtitle$(2)           , ch(27),~
               at (07,12), "Report Type:",                               ~
               at (07,25), fac(lfac$( 1)), rpttype$             , ch(01),~
               at (07,46), "Variance Type:",                             ~
               at (07,63), fac(lfac$( 2)), rptvar$              , ch(01),~
               at (08,14), "1. Material",                                ~
               at (08,48), "1. Rate/Price Variance",                     ~
               at (09,14), "2. Labor",                                   ~
               at (09,48), "2. Efficiency/Usage Variance",               ~
               at (10,14), "3. Work Center",                             ~
               at (10,48), "3. Total Variance",                          ~
               at (11,14), "4. Miscellaneous",                           ~
               at (11,48), "4. Purchase Variance",                       ~
               at (12,14), "5. Job",                                     ~
               at (14,10), fac(hex(a4)), scrtitle$(3)           , ch(27),~
               at (14,44), fac(hex(a4)), scrtitle$(4)           , ch(27),~
               at (15,12), "Total By:",                                  ~
               at (15,23), fac(lfac$( 3)), rptsort$             , ch(01),~
               at (15,46), "Report Level:",                              ~
               at (15,60), fac(lfac$( 4)), rptlevel$            , ch(01),~
               at (16,14), "1. By Part",                                 ~
               at (16,48), "1. Detail",                                  ~
               at (17,14), "2. By Job",                                  ~
               at (17,48), "2. Summary",                                 ~
               at (18,14), "3. By Vendor",                               ~
               at (19,14), "4. By Part Category",                        ~
               at (20,14), "5. By Labor Type",                           ~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$,             ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40525
                  call "MANUAL" ("JBVARRPT")
                  goto L40120

L40525:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40120

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   2      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'102(fieldnr%)
                  screen1$ = "Select Part Category Range"
                  screen3$ = "Beginning Category:"
                  screen4$ = "Ending Category:"
                  if rptsort$ <> "3" then L41140
                  screen1$ = "Select Vendor Range"
                  screen3$ = "Beginning Vendor:"
                  screen4$ = "Ending Vendor:"
L41140:           if rptsort$ <> "5" then L41190
                  screen1$ = "Select Labor Type Range"
                  screen3$ = "Beginning Labor Type:"
                  screen4$ = "Ending Labor Type:"

L41190:           init(hex(84)) lfac$()
                  str(line2$,1,50) = "Input Selection Criteria, Screen 2"
                  on fieldnr% gosub L41330,         /* Date Range       */~
                                    L41330,         /* Part Range       */~
                                    L41330,         /* Job Range        */~
                                    L41330          /* Vendor Range     */
                     goto L41400

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41330:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41400:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Variance Reports",                              ~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,03), fac(hex(a4)), scrtitle$(5)           , ch(27),~
               at (06,34), fac(hex(a4)), scrtitle$(6)           , ch(27),~
               at (07,05), "Beginning Date:",                            ~
               at (07,21), fac(lfac$( 1)), firstdate$           , ch(10),~
               at (07,36), "Beginning Part:",                            ~
               at (07,52), fac(lfac$( 2)), firstpart$           , ch(25),~
               at (08,05), "Ending Date:",                               ~
               at (08,21), fac(lfac$( 1)), lastdate$            , ch(10),~
               at (08,36), "Ending Part:",                               ~
               at (08,52), fac(lfac$( 2)), lastpart$            , ch(25),~
               at (11,03), fac(hex(a4)), scrtitle$(7)           , ch(27),~
               at (11,34), fac(hex(a4)), screen1$,                       ~
               at (12,05), "Beginning Job:",                             ~
               at (12,20), fac(lfac$( 3)), firstjob$            , ch(08),~
               at (12,36), fac(hex(8c)), screen3$,                       ~
               at (12,58), fac(lfac$( 4)), firstvendor$         , ch(09),~
               at (13,05), "Ending Job:",                                ~
               at (13,20), fac(lfac$( 3)), lastjob$             , ch(08),~
               at (13,36), fac(hex(8c)), screen4$,                       ~
               at (13,58), fac(lfac$( 4)), lastvendor$          , ch(09),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41860
                  call "MANUAL" ("JBVARRPT")
                  goto L41400

L41860:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41400

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  str(line2$,1,50) = "Edit Selection Criteria, Screen 1"
                  on fieldnr% gosub L42100,         /* report type      */~
                                    L42100,         /* report variance  */~
                                    L42100,         /* report sort      */~
                                    L42100          /* report level     */
                     goto L42120

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L42100:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L42120:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Variance Reports",                              ~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,10), fac(hex(a4)), scrtitle$(1)           , ch(27),~
               at (06,44), fac(hex(a4)), scrtitle$(2)           , ch(27),~
               at (07,12), "Report Type:",                               ~
               at (07,25), fac(lfac$( 1)), rpttype$             , ch(01),~
               at (07,46), "Variance Type:",                             ~
               at (07,63), fac(lfac$( 1)), rptvar$              , ch(01),~
               at (08,14), "1. Material",                                ~
               at (08,48), "1. Rate/Price Variance",                     ~
               at (09,14), "2. Labor",                                   ~
               at (09,48), "2. Efficiency/Usage Variance",               ~
               at (10,14), "3. Work Center",                             ~
               at (10,48), "3. Total Variance",                          ~
               at (11,14), "4. Miscellaneous",                           ~
               at (11,48), "4. Purchase Variance",                       ~
               at (12,14), "5. Job",                                     ~
               at (14,10), fac(hex(a4)), scrtitle$(3)           , ch(27),~
               at (14,44), fac(hex(a4)), scrtitle$(4)           , ch(27),~
               at (15,12), "Total By:",                                  ~
               at (15,23), fac(lfac$( 1)), rptsort$             , ch(01),~
               at (15,46), "Report Level:",                              ~
               at (15,60), fac(lfac$( 1)), rptlevel$            , ch(01),~
               at (16,14), "1. By Part",                                 ~
               at (16,48), "1. Detail",                                  ~
               at (17,14), "2. By Job",                                  ~
               at (17,48), "2. Summary",                                 ~
               at (18,14), "3. By Vendor",                               ~
               at (19,14), "4. By Part Category",                        ~
               at (20,14), "5. By Labor Type",                           ~
                                                                         ~
               at (21,02), fac(hex(a4)), edtmessage$,             ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (23,20), "(5)Next Page",                               ~
               at (24,65),                                               ~
                  "(16)PRINT REPORT",                                    ~
                                                                         ~
               keys(hex(0001050d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L42535
                  call "MANUAL" ("JBVARRPT")
                  goto L42120

L42535:        if keyhit% <> 15 then L42555
                  call "PRNTSCRN"
                  goto L42120

L42555:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   2       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 2 OF DOCUMENT.                    *~
            *************************************************************

            deffn'112(fieldnr%)
                  str(line2$,1,50) = "Edit Selection Criteria, Screen 2"
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L43210,         /* Date Range       */~
                                    L43210,         /* Part Range       */~
                                    L43210,         /* Job Range        */~
                                    L43210          /* Vendor Range     */
                     goto L43262
            deffn'122(fieldnr%)
                  init(hex(84)) lfac$()
                  for fieldnr% = 1 to 2
                      on fieldnr% gosub L43210,                           ~
                                        L43210
                  next fieldnr%
                     goto L43262

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L43210:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L43262:           screen1$ = "Select Part Category Range"
                  screen3$ = "Beginning Category:"
                  screen4$ = "Ending Category:"
                  if rptsort$ <> "3" then L43272
                  screen1$ = "Select Vendor Range"
                  screen3$ = "Beginning Vendor:"
                  screen4$ = "Ending Vendor:"
L43272:           if rptsort$ <> "5" then L43280
                  screen1$ = "Select Labor Type Range"
                  screen3$ = "Beginning Labor Type:"
                  screen4$ = "Ending Labor Type:"

L43280:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Variance Reports",                              ~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,03), fac(hex(a4)), scrtitle$(5)           , ch(27),~
               at (06,34), fac(hex(a4)), scrtitle$(6)           , ch(27),~
               at (07,05), "Beginning Date:",                            ~
               at (07,21), fac(lfac$( 1)), firstdate$           , ch(10),~
               at (07,36), "Beginning Part:",                            ~
               at (07,52), fac(lfac$( 2)), firstpart$           , ch(25),~
               at (08,05), "Ending Date:",                               ~
               at (08,21), fac(lfac$( 1)), lastdate$            , ch(10),~
               at (08,36), "Ending Part:",                               ~
               at (08,52), fac(lfac$( 2)), lastpart$            , ch(25),~
               at (11,03), fac(hex(a4)), scrtitle$(7)           , ch(27),~
               at (11,34), fac(hex(a4)), screen1$,                       ~
               at (12,05), "Beginning Job:",                             ~
               at (12,20), fac(lfac$( 2)), firstjob$            , ch(08),~
               at (12,36), fac(hex(8c)), screen3$,                       ~
                                                                         ~
               at (12,58), fac(lfac$( 2)), firstvendor$         , ch(09),~
               at (13,05), "Ending Job:",                                ~
               at (13,20), fac(lfac$( 2)), lastjob$             , ch(08),~
               at (13,36), fac(hex(8c)), screen4$,                       ~
               at (13,58), fac(lfac$( 2)), lastvendor$          , ch(09),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
                                                                         ~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,20),                                               ~
                  "(4)Previous Page",                                    ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)PRINT REPORT",                                    ~
                                                                         ~
               keys(hex(0001040d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L43830
                  call "MANUAL" ("JBVARRPT")
                  goto L43280

L43830:        if keyhit% <> 15 then L43870
                  call "PRNTSCRN"
                  goto L43280

L43870:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* report type      */~
                                    L50200,         /* report variance  */~
                                    L50300,         /* report sort      */~
                                    L50400          /* report level     */
                     return
            deffn'155(fieldnr%)
                  errormsg$ = " "
                              gosub L50100          /* report type      */
                              if errormsg$ <> " " then return
                              gosub L50200          /* report variance  */
                              if errormsg$ <> " " then return
                              gosub L50300          /* report sort      */
                              if errormsg$ <> " " then return
                              gosub L50400          /* report level     */
                     return

L50100:     REM TEST DATA FOR report type
                rpttype% = pos("12345"=rpttype$)
                if rpttype% = 0 then errormsg$ = "Invalid Report Type: " ~
                   & rpttype$
                return
L50200:     REM TEST DATA FOR report variance
                rptvar% = pos("1234"=rptvar$)
                if rptvar% <> 0 then L50245
                errormsg$ = "Invalid Variance Type: " & rptvar$
                return
L50245:         if rptvar% = 4 and rpttype% <> 1 then errormsg$ =        ~
                   "Purchase Variance not valid for Report Type: "       ~
                   & rpttype$
                return
L50300:     REM TEST DATA FOR report sort
                rptsort% = pos("12345"=rptsort$)
                if rptsort% <> 0 then L50345
                errormsg$ = "Invalid Sort Order: " & rptsort$
                return
L50345:         if rptsort% = 3 and rptvar% <> 4  then errormsg$ =       ~
                   "Sort by Vendor not valid for Variance Type: "        ~
                   & rptvar$
                if rptsort% = 5 and rpttype% <> 2 then errormsg$ =       ~
                   "Sort by Labor Type not valid for Report Type: "      ~
                   & rpttype$
                if rptsort% = 2 and rptvar% = 4 then errormsg$ =         ~
                   "Sort by Job not valid for Variance Type: " & rptvar$
                return
L50400:     REM TEST DATA FOR report level
                rptlevel% = pos("12"=rptlevel$)
                if rptlevel% = 0 then errormsg$ =                        ~
                   "Invalid Report Level: " & rptlevel$
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51100,         /* Date Range       */~
                                    L51300,         /* Part Range       */~
                                    L51500,         /* Job Range        */~
                                    L51700          /* Vendor Range     */
                     return

            deffn'156(fieldnr%)
                errormsg$ = " "
                    gosub L51100          /* Date Range       */
                        if errormsg$ <> " " then return
                    gosub L51300          /* Part Range       */
                        if errormsg$ <> " " then return
                    gosub L51500          /* Job Range        */
                        if errormsg$ <> " " then return
                    gosub L51700          /* Vendor Range     */
                        if errormsg$ <> " " then return
                    return

L51100: REM TEST DATA FOR Date Range
            if firstdate$ <> "ALL" then L51130
                lastdate$ = " " : return
L51130:     if firstdate$ <> " " and firstdate$ <> blankdate$ then L51150
                errormsg$ = " Starting Date CANNOT be Blank." : return
L51150:     call "DATEOKC" (firstdate$, firstdate%, errormsg$)
                if errormsg$ <> " " then return
            if lastdate$ <> " " and lastdate$ <> blankdate$ then L51190
                lastdate$ = firstdate$ : return
L51190:     call "DATEOKC" (lastdate$, lastdate%, errormsg$)
                if errormsg$ <> " " then return
            if lastdate% < firstdate% then errormsg$ =                   ~
                "Ending Date Must Be Later Than Starting Date"
            return

L51300:     REM TEST DATA FOR Part Range
                call "TESTRNGE" (firstpart$, lastpart$,                  ~
                                 lopart$,    hipart$,    errormsg$)
                return

L51500:     REM TEST DATA FOR Job Range
                call "TESTRNGE" (firstjob$,  lastjob$,                   ~
                                 lojob$,     hijob$,     errormsg$)
                return

L51700:     REM TEST DATA FOR Vendor Range
                call "TESTRNGE" (firstvendor$,  lastvendor$,             ~
                                 lovendor$,     hivendor$, errormsg$)
                return


        accum_labor_class

            if str(print34$,,19) = "DIRECT & MISC COSTS" then            ~
                str(print34$,,4) = hex(eeeeeeee)
            search lbcl$() = str(print34$,,4) to look%() step 4
            if look%(1) = 0% then L55090
            p% = (look%(1) + 3%)/4%
            lbau(p%) = lbau(p%) + var2(2)
            lbad(p%) = lbad(p%) + var2(3)
            lbsu(p%) = lbsu(p%) + (var1(2) * compqty)
            return

L55090:     cnt% = cnt% + 1
            lbcl$(cnt%) = str(print34$,,4)
            lbau(cnt%) = var2(2)
            lbad(cnt%) = var2(3)
            lbsp(cnt%) = var1(1)
            lbsu(cnt%) = var1(2) * compqty
            return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("Closing Files, One Moment Please")
            end
