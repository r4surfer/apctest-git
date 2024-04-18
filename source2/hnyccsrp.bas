        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y   CCC    CCC    SSS   RRRR   PPPP    *~
            *  H   H  NN  N  Y   Y  C   C  C   C  S      R   R  P   P   *~
            *  HHHHH  N N N   YYY   C      C       SSS   RRRR   PPPP    *~
            *  H   H  N  NN    Y    C   C  C   C      S  R   R  P       *~
            *  H   H  N   N    Y     CCC    CCC    SSS   R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYCCSRP - This program creates a report of the Cycle Cnt *~
            *            Detail File and may be run either before or    *~
            *            after a session has been closed.  That is befor*~
            *            or after Physical Inventory Posting.           *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/19/92 ! Original                                 ! RJ1 *~
            * 01/12/93 ! Page 0 Facs fix                          ! RJH *~
            * 04/26/93 ! Force Variance % to 0 if unit varianc = 0! RJH *~
            * 08/08/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr_m(4),                  /* Descr Map For PlowCode     */~
            descr$42,                    /* Descr For PlowCode         */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            header$(3)79,                /* For PLOWCODE Call          */~
            i$(24)80,                    /* Screen Image               */~
            inc(2),                      /* Plowcode Arguments         */~
            inc$(2)16,                   /* Plowcode Arguments         */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /* Read Key                   */~
            rptid$6,                     /* Report ID                  */~
            rpttitle$60,                 /* Report Title               */~
            rpttitle2$60,                /* Report Title Second Line   */~
            session$12,                  /* Session Name               */~
            sortnum1$1,                  /* Sort Order                 */~
            sortnum2$1,                  /* Sort Order  no. 2          */~
            sortnum3$1,                  /* Sort Order  no. 3          */~
            sortordmsg$20,               /* Accept Screen Message      */~
            sortmsg1$1,                  /* Sort Order Message         */~
            sortmsg2$1,                  /* Sort Order Message         */~
            sortmsg3$1,                  /* Sort Order Message         */~
            testkey$99,                  /* Test Plowkey               */~
            time$8,                      /* System Time                */~
            userid$3                     /* Current User Id            */~


            dim     /* ** Cycle Count Variables **                     */~
            abcclass$1,                  /* ABC Class                  */~
            actflag$1,                   /* Active Session Flag        */~
            arraycode$(6)1,              /* Master Array Selection Code*/~
            arrayhdr1$(6)44,             /* Master Array Header 1      */~
            arrayhdr2$(6)44,             /* Master Array Header 2      */~
            arrayhdr3$(6)44,             /* Master Array Header 3      */~
            arraylen%(6),                /* Master Array Varibl Length */~
            arrayprntlen%(6),            /* Master Array Print  Length */~
            arrayindx%(6),               /* Master Array Sort Index    */~
            arraysub(6),                 /* Master Array Sub Total     */~
            arraysub$(6)18,              /* Master Array Sub Total Desc*/~
            arraytot(6),                 /* Master Array Totals        */~
            arrayval$(6)44,              /* Master Array Variable Value*/~
            arrayvar$(6)11,              /* Master Array Variable Name */~
            bohqty$10,                   /* Book On Hand Quantity      */~
            bohqtysub$10,                /* Book On Hand Quantity SubT */~
            bohvalsub$10,                /* Book On Hand Value SubTotal*/~
            bohqtytot$10,                /* Book On Hand Quantity Total*/~
            bohvaltot$10,                /* Book On Hand Value Total   */~
            ccgroup$6,                   /* Cycle Count Group Code     */~
            cntby$12,                    /* User Count By              */~
            cntdate$10,                  /* Date PI Started            */~
            cntdltamsub$12,              /* Sub total  Qnty. Delta (-) */~
            cntdltapsub$12,              /* Sub total  Qnty. Delta (+) */~
            cntdltatsub$12,              /* Count Tol. Variance Subtot */~
            cntdltamtot$12,              /* Total  Qnty. Delta (-)     */~
            cntdltaptot$12,              /* Total  Qnty. Delta (+)     */~
            cntdltattot$12,              /* Count Tol. Variance Total  */~
            cntperiod$3,                 /* Count Period               */~
            cnttlernper$10,              /* Count Tolerance in Percent */~
            cnttlernqty$10,              /* Count Tolerance Quantity   */~
            columnhead1$130,             /* Column Header #1           */~
            columnhead2$130,             /* Column Header #2           */~
            columnhead3$130,             /* Column Header #3           */~
            costvar$10,                  /* Value Variance             */~
            counttot$5,                  /* Tolerance Hit Count Total  */~
            enterby$3,                   /* Entry User ID              */~
            lastcntdate$10,              /* Date Part was Last Counted */~
            lastval1$44,                 /* Previous 1st Value in PrKey*/~
            lastval2$44,                 /* Previous 2nd Value in PrKey*/~
            lot$6,                       /* Lot Number of Part         */~
            part$25,                     /* Part Number                */~
            partcntsub$12,               /* Sub total of Parts Counted */~
            partcnttot$12,               /* Total of Parts Counted     */~
            partstrlot$44,               /* Part/Store/Lot Key         */~
            percent$10,                  /* Percentage value           */~
            printkey$99,                 /* Print Format Variable      */~
            printtoltype$1,              /* Print Flag wrt Tolerance   */~
            qty$10,                      /* PI Quantity Count          */~
            sessiondesc$30,              /* Session Description        */~
            sessiondate$8,               /* Session Date               */~
            sourceflag$1,                /* Flag How Part Came to Sessn*/~
            store$3,                     /* Store/warehouse            */~
            tempval$99,                  /* Temporary Variable         */~
            tolhit$1,                    /* Hit Condition wrt Part& Tol*/~
            tolhitsub$12,                /* Sub Total  Tolerance Hits  */~
            tolhittot$12,                /* Total  Tolerance Hits      */~
            unitvar$10,                  /* Unit Variance              */~
            valdltamsub$12,              /* Sub total Value  Delta (-) */~
            valdltapsub$12,              /* Sub total Value  Delta (+) */~
            valdltamtot$12,              /* Total Value  Delta (-)     */~
            valdltaptot$12,              /* Total Value  Delta (+)     */~
            valdltatsub$12,              /* Sub Total ABS Value  Delta */~
            valdltattot$12,              /* Total ABS  Value  Delta    */~
            varcount$4,                  /* Variance Reason Code Count */~
            varprcnt$10,                 /* Variance Percentage fm BOH */~
            varreason$6,                 /* Variance Reason Code       */~
            varreason$(99)6,             /* Variance Reason Code Array */~
            varrsncount(99),             /* Variance Reason Code Count */~
            varrsnprint$(99)6,           /* Variance Reason Print Array*/~
            work2key$99                  /* Workfile 2 Key             */

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
            * #01 ! HNYMASTR ! Inventory Master File                    *~
            * #02 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * #03 ! HNYCCMST ! Cycle Count Master File                  *~
            * #04 ! HNYCCDTL ! Cycle Count Session Detail File          *~
            * #06 ! HNYCCSYS ! Cycle Count System Control File          *~
            * #52 ! WORKFIL2 ! Work File for Report Sort and Printing   *~
            * #55 ! DUMMY    ! For PLOWCODE Extended Arguments          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup     ~

            select #02, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            select #03, "HNYCCMST",                                      ~
                        varc,     indexed,  recsize = 796,               ~
                        keypos =    1,  keylen = 44,                     ~
                        alt key  1, keypos =   45, keylen =   6, dup,    ~
                            key  2, keypos =   81, keylen =   7, dup,    ~
                            key  3, keypos =   73, keylen =  15, dup     ~

            select #04, "HNYCCDTL",                                      ~
                        varc,     indexed,  recsize =  436,              ~
                        keypos =    1, keylen =  57,                     ~
                        alt key  1, keypos =   13, keylen =  45, dup,    ~
                            key  2, keypos =   14, keylen =  44, dup

            select #06, "HNYCCSYS",                                      ~
                        varc,     indexed,  recsize =  249,              ~
                        keypos =    1, keylen =  12,                     ~
                        alt key  1, keypos =   43, keylen =  13, dup,    ~
                            key  2, keypos =   56, keylen =  10

            select #52, "WORKFIL2",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos = 1,    keylen =68                        ~

            select #55, "DUMMY",                                         ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  42

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            rec% = val(str(rslt$(03%),17,4),4) / 2%
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
            call "OPENCHCK" (#06, fs%(06), f2%(06), 0%, rslt$(06))

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
            call "COMPNAME" (12%, company$, ret%)
            ret% = ret%
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "             Cycle Count Session Result Rep" &  ~
                        "ort                           "

            rptid$ = "HNY044"
            sortordmsg$  = "Sort Criteria"
            sortmsg1$  = "1"
            sortmsg2$  = "2"
            sortmsg3$  = "3"

            str(line2$,62) = "HNYCCSRP: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  3%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles EDIT MODE for range selection screen.             *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       extract_data
                  if keyhit% <>  0% then       editpg1
            if cursor%(1%) <> 4% then L11120
                fieldnr% = 1%  :  goto  L11130
L11120:     if cursor%(1%) <>  7% then L11122
                fieldnr% = 2%  :  goto  L11130
L11122:     if cursor%(1%)  = 15% then fieldnr% = 3%                     ~
                                  else fieldnr% = 0%

L11130:     if fieldnr% < 1% or fieldnr% >  3% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *           E X T R A C T   R E P O R T   D A T A           *~
            *-----------------------------------------------------------*~
            * Data Extraction section for report.                       *~
            *************************************************************
        extract_data

            gosub initialize_sort_array
            gosub set_array_index /*Set the Array Index for Report Sort*/

            call "SHOSTAT"  ("Building The Report Work File")

            call "WORKOPEN" (#52, "IO", rec%, f2%(52))

            work2key$ = " "
            str(plowkey$,13) = all(hex(00))
            testkey$ = str(plowkey$,,12)
            str(testkey$,13) = all(hex(ff))

            call "READ104" (#04, plowkey$, f1%(04))   /* HNYCCDTL */
            if f1%(04) = 0% then  goto generate_report else  L13800

        loop_session  /* HNYCCDTL Session Selection Loop */
            call "READNEXT" (#04, f1%(04))
            if  f1%(04) = 0% then  goto generate_report

*        *** TEMP ****
*          KEY$ = KEY(#04, 0%)
*        *** TEMP ****
            if str(key(#4),,12) > testkey$ then goto generate_report
L13800:     get #04 using L14350, actflag$, part$, store$, lot$, qty,     ~
                                 unitvar, cnttlernper, cnttlernqty,      ~
                                 cntperiod$, varreason$

            gosub set_rpttitle2

            /* Calc the Tolerance Hit Condition */
            bohqty = qty - unitvar
            if unitvar <> 0.0 then L13827
                varprcnt = 0.0
                goto L13834
L13827:     if bohqty = 0.0 then varprcnt = 9999.99                      ~
                            else varprcnt = 100 * (unitvar /  bohqty)
L13834:     if abs(varprcnt) <= cnttlernper  or                          ~
               abs(unitvar)  <= cnttlernqty                              ~
                    then tolhit$ = "Y"   else tolhit$ = "N"

            if printtoltype$ = "B" then L13855
            if printtoltype$ = "I" and tolhit$ = "Y" then L13855
            if printtoltype$ = "O" and tolhit$ = "N" then L13855
            goto  loop_session

L13855:     call "CONVERT" (varprcnt     ,-2.2, varprcnt$)
            call "CONVERT" (bohqty       , 2.2, bohqty$)
            call "CONVERT" (cnttlernqty  , 2.2, cnttlernqty$)
            call "CONVERT" (cnttlernper  , 2.2, cnttlernper$)
            if tolhit$ = "N" and varreason$ = " "                        ~
                then varreason$ = "______"

            readkey$ = str(part$) & str(store$) & lot$
            call "READ100" (#03, readkey$, f1%(03))
            get #03  using L14470, ccgroup$, abcclass$

            gosub make_workfile2_key

            put  #52   using L14484,                                      ~
                          str(work2key$,,70), tolhit$, bohqty$, varprcnt$
            write #52, eod goto loop_session

            goto loop_session
            /* ** END of CC Session method of Selection ** */

            /* HNYCCDTL */
L14350:     FMT POS(13), CH(1), CH(25), CH(3), CH(16), POS(68), PD(14,4),~
                POS(84), PD(14,4), POS(100), PD(14,4), PD(14,4),         ~
                POS(122), CH(3), POS(131), CH(6)

            /* HNYMASTR */
            FMT POS(90), CH(4), POS(180), CH(3)            /* HNYMASTR */

            /* HNYCCMST */
L14470:     FMT POS(45), CH(6), POS(127), CH(1)

            /* WORKFIL2 */
L14484:     FMT CH(70), CH(1), CH(10), CH(10)

        make_workfile2_key /* Set Work File & Key from Sort Array Index */
            partstrlot$ = str(part$) & str(store$) & str(lot$)
            arrayval$(1%) = abcclass$ : arrayval$(2%) = ccgroup$
            arrayval$(3%) = partstrlot$ : arrayval$(4%) = cntperiod$
            arrayval$(5%) = varreason$  : arrayval$(6%) = varprcnt$

            work2key$ = arrayval$(arrayindx%(1%))
            pos% = 1%
            len% = arraylen%(arrayindx%(1%))

            for n% = 2% to 6%
            pos% = pos% + len%
            work2key$ = str(work2key$,,(pos% - 1%)) &                    ~
                                               arrayval$(arrayindx%(n%))
            len% = arraylen%(arrayindx%(n%))
            next n%
            return

        initialize_sort_array
            /* Initialize  Report sort order array and other report    */
            /* variables.                                              */

            arraycode$(1) = "A"      : arrayvar$(1) = "ABCCLASS$"
            arrayhdr1$(1) = "ABC"    : arrayhdr2$(1) = "CLS"
            arrayhdr3$(1) = "---"    : arrayval$(1) = abcclass$
            arraylen%(1)  =  1%      : arraysub(1) = 0 : arraytot(1) = 0
            arrayprntlen%(1) = 3%    : arraysub$(1) = "ABC CLASS        :"

            arraycode$(2) = "G"      : arrayvar$(2) = "CCGROUP$"
            arrayhdr1$(2) = "CYCLE " : arrayhdr2$(2) = "GROUP "
            arrayhdr3$(2) = "------" : arrayval$(2) = ccgroup$
            arraylen%(2) = 6%        : arraysub(2) = 0 : arraytot(2) = 0
            arrayprntlen%(2) = 6%    : arraysub$(2) = "CYCLE COUNT GROUP:"

            arraycode$(3) = "P"      : arrayvar$(3) = "PARTSTRLOT$"
            arrayhdr1$(3) = " "      : arrayval$(3) = partstrlot$
            arrayhdr2$(3) = "PART NUMBER               STR LOT   "
            arrayhdr3$(3) = "------------------------- --- ------"
            arraylen%(3) = 44%   :  arraysub(3) = 0  :  arraytot(3) = 0
            arrayprntlen%(3) = 36%   : arraysub$(3) = "PART NAME        :"

            arraycode$(4) = "C"      : arrayvar$(4) = "CNTPERIOD$"
            arrayhdr1$(4) = "CNT"    : arrayval$(4) = cntperiod$
            arrayhdr2$(4) = "PRD"
            arrayhdr3$(4) = "---"
            arraylen%(4)  =  3%  :  arraysub(4) = 0  :  arraytot(4) = 0
            arrayprntlen%(4) = 3%    : arraysub$(4) = "COUNT PERIOD     :"

            arraycode$(5) = "R"      : arrayvar$(5) = "VARREASON$"
            arrayhdr1$(5) = "VAR   " : arrayhdr2$(5) = "REASON"
            arrayhdr3$(5) = "------" : arrayval$(5) = varreason$
            arraylen%(5)  =  6%      : arraysub(5) = 0 : arraytot(5) = 0
            arrayprntlen%(5) = 6%

            arraycode$(6) = "V"      : arrayvar$(6) = "VARPRCNT$"
            arrayhdr1$(6) = "  VAR"  : arrayhdr2$(6) = "PRCNT"
            arrayhdr3$(6) = "-----"  : arrayval$(6) = varprcnt$
            arraylen%(6)  =  5%      : arraysub(6) = 0 : arraytot(6) = 0
            arrayprntlen%(6) = 5%    : arraysub$(6) = "COUNT TOLERNCE % :"

            return      /* END of INTITALIZE_SORT_ARRAY Sub  */

        set_array_index    /* Set the Array Index for Report Sorting */
            i% = 3%
            for n% = 1% to 6%
                p% = pos("AGPCRV" = arraycode$(n%))
                if sortnum1$ <> arraycode$(n%) then L19320
                     arrayindx%(1%) = p%  : goto L19370
L19320:         if sortnum2$ <> arraycode$(n%) then L19340
                     arrayindx%(2%) = p%  : goto L19370
L19340:         if sortnum3$ <> arraycode$(n%) then L19360
                     arrayindx%(3%) = p%  : goto L19370
L19360:         i% = i% + 1%  :  arrayindx%(i%) = p%
L19370:     next n%
            return

        set_rpttitle2
            rpttitle2$ = "                         (        ) IS"
            str(rpttitle2$,14,12) = testkey$
            str(rpttitle2$,27, 8) = sessiondate$
            if actflag$ <> "C" then L19450
                str(rpttitle2$,40,  ) = "CLOSED"     : return
L19450:     if actflag$ <> "A" then L19470
                str(rpttitle2$,40,  ) = "ACTIVE"     : return
L19470:     if actflag$ <> "P" then L19490
                str(rpttitle2$,40,  ) = "PRE-ACTIVE" : return
L19490:         str(rpttitle2$,37,  ) = "HAS NO ACTIVITY FLAG SETTING"
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Session Name/Date      */~
                              L20300,         /* Sort Order             */~
                              L20400          /* Print Choice           */
            return
L20100: REM Def/Enable Session Name            SESSION$


            return

L20300: REM Def/Enable Sort Order                  SORTNUM1$


            return

L20400: REM Def/Enable Print Choice                PRINTTOLTYPE$


            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Session Name then Session Date                         ",~
         "Enter Sort Order                                             ",~
         "Enter Print Choice                                           "


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, lastval1$, printoneline$,  ~
                      printtoltype$, session$, sessiondesc$,             ~
                      sessiondate$, sortnum1$, sortnum2$, sortnum3$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
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
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************
        generate_report
            if work2key$ = " " then goto nothing_to_report
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("RPTID", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            limit% = 0%  /* Variance Reason Array Stopper */
            mat varrsncount = zer
            init(" ") varreason$()

            if lcntr% > 56% then gosub page_head

            call "SHOSTAT" ("Printing Report Now")
            gosub init_totals
            gosub init_subtotals
            gosub set_column_headers
            if pcntr% = 1% then gosub column_head

*         ** Start Report Loop **

            plowkey$ = all(hex(00))
            call "READ102" (#52, plowkey$, f1%(52))
            if f1%(52) <> 0% then L30250
                print
                print "                          NO RECORDS TO REPORT ON"
                goto end_report

        report_loop
            call "READNEXT" (#52, f1%(52))
            if f1%(52) = 0% then goto print_report_totals

L30250:     get #52  using L14484, work2key$, tolhit$, bohqty$, varprcnt$
            gosub set_printkey  /* Set PrintKey Format */

            readkey$ = str(session$) & str(actflag$) & str(part$)        ~
                       & str(store$) & lot$
            call "READ100" (#04, readkey$, f1%(04))
            if f1%(04) <> 0% then L30360
                call "SHOSTAT" ("KEY DID NOT MATCH HNYCCDTL")
                goto report_loop
L30360:     get #04 using  L35030, sourceflag$, cntdate$, enterby$,   qty,~
                 unitcost , unitvar , costvar , cnttlernper ,            ~
                 cnttlernqty , lastcntdate$, varreason$, cntby$

*          IF COSTSOURCE$ = "S" THEN GOSUB SET_HNYQUAN_COST

            call "CONVERT" (qty        , 2.2, qty$)
            call "CONVERT" (unitvar    , 2.2, unitvar$)
            call "CONVERT" (costvar    , 2.2, costvar$)
            call "CONVERT" (cnttlernper, 2.2, cnttlernper$)
            call "CONVERT" (cnttlernqty, 2.2, cnttlernqty$)
            call "DATEFMT" (lastcntdate$)

            if actflag$ = " " then actflag$ = "A"

            lcntr% = lcntr% + 1%
            if lcntr% < 57 then L30545
                gosub page_head   :   gosub column_head

L30545:     gosub set_totals

            print using L60200, printkey$, str(unitvar$,2,9),             ~
                              str(costvar$,2,9),str(bohqty$,2,9),        ~
                              str(qty$,2,9), tolhit$,                    ~
                              str(cnttlernper$,5,5),str(cnttlernqty$,5,5)~
                              ,lastcntdate$

            printoneline$ = "Y"
            goto report_loop

        init_subtotals
            partcntsub, cntdltapsub, cntdltamsub,valdltapsub,valdltamsub,~
                partvalsub, tolhitsub  =  0.0
            recordsub%  =  0%

            return

        init_totals
            partcnttot, cntdltaptot,cntdltamtot, valdltaptot,valdltamtot,~
                partvaltot, tolhittot  =  0.0
            recordtot%  =  0%

            return

        set_totals
            partcntsub = partcntsub + qty
            partcnttot = partcnttot + qty
            if unitvar < 0 then L32290
                cntdltapsub = cntdltapsub + unitvar
                cntdltaptot = cntdltaptot + unitvar
                valdltapsub = valdltapsub + costvar
                valdltaptot = valdltaptot + costvar
                goto L32322
L32290:     cntdltamsub = cntdltamsub + unitvar
            cntdltamtot = cntdltamtot + unitvar
            valdltamsub = valdltamsub + costvar
            valdltamtot = valdltamtot + costvar

L32322:     partvalsub = partvalsub + (qty * unitcost)
            partvaltot = partvaltot + (qty * unitcost)

            if tolhit$ <> "Y" then L32360
                tolhitsub  = tolhitsub  + 1.0
                tolhittot  = tolhittot  + 1.0
L32360:     recordsub% = recordsub% + 1%
            recordtot% = recordtot% + 1%
            gosub set_varreason_totals
            return

        print_subtotals
            call "CONVERT" (partcntsub ,-2.2, partcntsub$)
            call "CONVERT" (tolhitsub  ,-0.01, tolhitsub$)
            if recordsub% <> 0% then L33015
                percent$ = " N/A"  :  goto  L33025
L33015:     percent = 100 * (tolhitsub  / recordsub%)
            call "CONVERT" (percent    ,-2.1, percent$)
L33025:     call "CONVERT" (cntdltapsub,-2.2, cntdltapsub$)
            call "CONVERT" (cntdltamsub,-2.2, cntdltamsub$)
            cntdltatsub = cntdltamsub + cntdltapsub
            call "CONVERT" (cntdltatsub,-2.2, cntdltatsub$)
            bohqtysub = partcntsub - cntdltatsub
            call "CONVERT" (bohqtysub  ,-2.2, bohqtysub$)
            call "CONVERT" (valdltapsub,-2.2, valdltapsub$)
            call "CONVERT" (valdltamsub,-2.2, valdltamsub$)
            valdltatsub = valdltamsub + valdltapsub
            call "CONVERT" (valdltatsub,-2.2, valdltatsub$)
            bohvalsub = partvalsub - valdltatsub
            call "CONVERT" (bohvalsub  ,-2.2, bohvalsub$)

            print using L60250, partcntsub$, tolhitsub$, percent$

            if bohqtysub <> 0 then L33054
                percent$ = " N/A"  :  goto L33060
L33054:     percent = 100 * (cntdltatsub / bohqtysub)
            call "CONVERT" (percent    ,-2.1, percent$)

L33060:     print using L60290, cntdltapsub$, cntdltamsub$, cntdltatsub$, ~
                               bohqtysub$, percent$
            if bohvalsub <> 0 then   L33074
                percent$ = " N/A"  :  goto L33080
L33074:     percent = 100 * (valdltatsub / bohvalsub)
            call "CONVERT" (percent    ,-2.1, percent$)

L33080:     print using L60330, valdltapsub$, valdltamsub$, valdltatsub$, ~
                               bohvalsub$, percent$
            print
            lcntr% = lcntr% + 4%

            return

        print_report_totals
            gosub print_subtotals
            call "CONVERT" (partcnttot ,-2.2, partcnttot$)
            call "CONVERT" (tolhittot  ,-0.01, tolhittot$)
            if recordtot% <> 0% then L33215
                percent$ = " N/A"  :  goto  L33225
L33215:     percent = 100 * (tolhittot  / recordtot%)
            call "CONVERT" (percent    ,-2.1, percent$)
L33225:     call "CONVERT" (cntdltaptot,-2.2, cntdltaptot$)
            call "CONVERT" (cntdltamtot,-2.2, cntdltamtot$)
            cntdltattot = cntdltamtot + cntdltaptot
            call "CONVERT" (cntdltattot,-2.2, cntdltattot$)
            bohqtytot = partcnttot - cntdltattot
            call "CONVERT" (bohqtytot  ,-2.2, bohqtytot$)
            call "CONVERT" (valdltaptot,-2.2, valdltaptot$)
            call "CONVERT" (valdltamtot,-2.2, valdltamtot$)
            valdltattot = valdltamtot + valdltaptot
            call "CONVERT" (valdltattot,-2.2, valdltattot$)
            bohvaltot = partvaltot - valdltattot
            call "CONVERT" (bohvaltot  ,-2.2, bohvaltot$)

            print using L60370, partcnttot$, tolhittot$, percent$

            if bohqtytot <> 0 then L33310
                percent$ = " N/A"  :  goto L33325
L33310:     percent = 100 * (cntdltattot / bohqtytot)
            call "CONVERT" (percent    ,-2.1, percent$)

L33325:     print using L60290, cntdltaptot$, cntdltamtot$, cntdltattot$, ~
                               bohqtytot$, percent$
            if bohvaltot <> 0 then   L33345
                percent$ = " N/A"  :  goto L33360
L33345:     percent = 100 * (valdltattot / bohvaltot)
            call "CONVERT" (percent    ,-2.1, percent$)

L33360:     print using L60330, valdltaptot$, valdltamtot$, valdltattot$, ~
                               bohvaltot$, percent$
            print
            lcntr% = lcntr% + 4%
            gosub print_varreason_totals
            goto end_report

        set_varreason_totals
            hit% = 0%
            if limit% = 0 then L33435
            for n% = 1 to limit%     /* Limit is Zero to start */
                if varreason$(n%) <> varreason$ then L33425
                     varrsncount(n%)  =  varrsncount(n%)  + 1
                     hit% = 1%
                     n% = limit%
L33425:     next n%

L33435:     if hit% = 1% then return
                if limit% < 99% then limit% = limit% + 1%
                varrsncount(limit%) = 1
                varreason$(limit%)  =  varreason$
           return

        print_varreason_totals
            if actflag$ = "P" then return
            mat varrsnprint$ = dsort(varreason$)
            gosub page_head
            counttot = 0.0
            print skip(3)
            print using L60410
            print
            print using L60440
            print using L60470
            for n% =  limit% to 1%  step -1%
                varreason$ = varrsnprint$(n%)
                for i% = 1% to limit%
                    if varreason$ <> varreason$(i%) then L33502
                    count = varrsncount(i%) :  i% = limit%
L33502:         next i%
                if varreason$ <= " " then L33507
                call "CONVERT" (count,  0.01, varcount$)
                print using L60500, varreason$ , varcount$
                counttot = counttot + count
L33507:     next n%
            call "CONVERT" (counttot,  0.01, counttot$)
            print using L60470
            print using L60500 , " TOTAL", counttot$
            return

        set_printkey /* Parse the HNYCCMST Key & Re-String for Printing */
            i% = arrayindx%(1%)
            len% = arraylen%(i%)  :  pos% = 1%
            prntlen% = arrayprntlen%(i%)  :  prntpos% = 1%
            printkey$ = str(work2key$,1,len%)
            if printkey$ <> lastval1$ then L33556
                printkey$ = " " : goto L33595

L33556:         if printoneline$ = "Y" then gosub print_subtotals
                gosub init_subtotals
                lastval1$ = printkey$
            if arrayvar$(i%) <> "PARTSTRLOT$" then L33595
                store$ = str(printkey$,26,3) : part$ = str(printkey$,,25)
                lot$ = str(printkey$,29,6)
                str(printkey$,  ,25) = part$ : str(printkey$,26,1) = " "
                str(printkey$,27, 3) = store$ : str(printkey$,30,1) = " "
                str(printkey$,31, 6) = lot$

L33595:     for n% = 2% to 6%
                i% = arrayindx%(n%)
                pos% = pos% + len%  :  len% = arraylen%(i%)
                prntpos% = prntpos% + prntlen%
                prntlen% = arrayprntlen%(i%)
                tempval$ =  str(work2key$,pos%,len%)

                if n% <> 2%  then L33665
                if printkey$ <> " "  then L33660
                if tempval$ <> lastval2$ then L33660
                     tempval$ = " " : goto L33665
L33660:         lastval2$ = tempval$

L33665:         if arrayvar$(i%) <> "PARTSTRLOT$" then L33700
                   store$ = str(tempval$,26,3) : part$ = str(tempval$,,25)
                   lot$ = str(tempval$,29,6)
                   str(tempval$,  ,25) = part$  : str(tempval$,26,1) = " "
                   str(tempval$,27, 3) = store$ : str(tempval$,30,1) = " "
                   str(tempval$,31, 6) = lot$

L33700:         printkey$ = str(printkey$,,prntpos% + n% - 2% ) & tempval$
            next n%
            partstrlot$ = str(part$) & str(store$) & lot$

            return

        set_column_headers
            i% = arrayindx%(1%)
            columnhead1$ = arrayhdr1$(i%) : columnhead2$ = arrayhdr2$(i%)
            columnhead3$ = arrayhdr3$(i%)
            lentot% = arrayprntlen%(i%) + 1%
            for n% = 2% to 6%
                i% = arrayindx%(n%)
                columnhead1$ = str(columnhead1$,,lentot%) & arrayhdr1$(i%)
                columnhead2$ = str(columnhead2$,,lentot%) & arrayhdr2$(i%)
                columnhead3$ = str(columnhead3$,,lentot%) & arrayhdr3$(i%)
                lentot% = lentot% + arrayprntlen%(i%) + 1%
                next n%

                columnhead1$ = str(columnhead1$,,64) & "  VARIANCE  VARI"~
                               &  "ANCE      BOOK  PHYSICAL T   TOL   TO"~
                               &  "L LAST"
                columnhead2$ = str(columnhead2$,,64) & "  QUANTITY     V"~
                               &  "ALUE  QUANTITY     COUNT H PRCNT  QUA"~
                               &  "N CNT DATE"
                columnhead3$ = str(columnhead3$,,64) & " --------- -----"~
                               &  "---- --------- --------- - ----- ----"~
                               &  "- --------"
                return

        nothing_to_report
L33855:     keyhit% = 2%
            call "ASKUSER" (keyhit%, "**** No record found ****",        ~
                            "Press PF1 to Start Over",                   ~
                                 " --- OR ---",                          ~
                            "Press PF16 to Exit Program")
            if keyhit% = 1%  then goto L33945 /* Head back to Input Mode */
            if keyhit% = 16% then goto exit_program
            goto L33855   /* Loop Until Valid */

        end_report                /* Report Ending Routine */
            time$ = " "  :  call "TIME" (time$)
            print skip(2)
            print using L64990, time$     /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
L33945:     call "FILEBGON" (#52) /* Zap Workfile2 */
            goto inputmode

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "HNYCCSRP", rptid$
            print using L60110, rpttitle$, pcntr%
            print using L60134, rpttitle2$
            print
            if pcntr% = 0% then gosub print_params
            lcntr% = 4%
            return


        column_head
            print columnhead1$
            print columnhead2$
            print columnhead3$
            lcntr% = lcntr% + 3%
            return

        print_params           /* Print Page Zero */
L34510:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L34550
                str(i$(), i%, 1%) = hex(20)
                goto L34510
L34550:     print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 4% to 19% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            gosub page_head
            return


        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: HNYCCDTL                          */~
            POS(58),                                                     ~
            CH(1),          /* Flag Show How Part Got Into Session     */~
            CH(6),          /* Count Date                              */~
            CH(3),          /* Item Entered By                         */~
            PD(14,4),       /* Number of Units Counted                 */~
            PD(14,4),       /* Unit Value                              */~
            PD(14,4),       /* Unit Variance                           */~
            PD(14,4),       /* Value Variance                          */~
            PD(14,4),       /* Count Tolerance Percentage              */~
            PD(14,4),       /* Count Tolerance Quantity                */~
            CH(6),          /* Last Count Date                         */~
            POS(131),       /*                                         */~
            CH(6),          /* Variance Reason Code                    */~
            CH(12)          /* Item Counted By                         */~

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40095,         /* Session Name/Date */   ~
                                L40095,         /* Sort Order        */   ~
                                L40095          /* Print Choice      */

              goto L40110

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40095:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40110:     accept                                                       ~
               at (01,02),                                               ~
                  "Cycle Count Session Results Report",                  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Session Name        ",                       ~
               at (04,23), fac(lfac$( 1)),   session$           , ch(12),~
               at (04,40), fac(hex(8c)), sessiondesc$           , ch(30),~
                                                                         ~
               at (06,23), fac(hex(ac)), sortmsg1$              , ch( 1),~
               at (06,26), fac(hex(ac)), sortmsg2$              , ch( 1),~
               at (06,29), fac(hex(ac)), sortmsg3$              , ch( 1),~
               at (06,41), fac(hex(ac)), sortordmsg$            , ch(20),~
                                                                         ~
               at (07,02), "Sort Order Criteria ",                       ~
               at (07,23), fac(lfac$( 2)),   sortnum1$          , ch(01),~
               at (07,26), fac(lfac$( 2)),   sortnum2$          , ch(01),~
               at (07,29), fac(lfac$( 2)),   sortnum3$          , ch(01),~
                                                                         ~
                                                                         ~
               at (07,41), "A = ABC Class",                              ~
               at (08,41), "C = Count Period",                           ~
               at (09,41), "G = Group    ",                              ~
               at (10,41), "P = Part/Store/Lot",                         ~
               at (11,41), "R = Variance Reason Code",                   ~
               at (12,41), "V = Variance Percent",                       ~
                                                                         ~
               at (15,02), "Print Criteria      ",                       ~
               at (15,23), fac(lfac$( 3)), printtoltype$        , ch(01),~
                                                                         ~
               at (15,41), "I = Within Tolerance Only",                  ~
               at (16,41), "O = Out of Tolerance Only",                  ~
               at (17,41), "B = Include Both         ",                  ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L41140
                  call "MANUAL" ("HNYCCSRP") : goto L40110

L41140:        if keyhit% <> 15 then L41170
                  call "PRNTSCRN" : goto L40110

L41170:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41360     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L41330
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L41340
L41330:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41340:     return

L41360: if fieldnr% > 0% then L41450  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L41450:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* Session Name/Date      */~
                              L50300,         /* Sort Order             */~
                              L50400          /* Print Choice           */

            return
L50100: REM Test for Session Name                 SESSION$
            if session$ = " " or session$ = "?"                          ~
                then plowkey$ = all(hex(20))  else plowkey$ = session$

            mat descr_m = zer : mat inc = zer : init(" ") inc$()
            header$(3) = hex(80) & "Select Session Name"
            header$(1) = "  Session Name   Description"

            descr_m(01) =     1.12  : descr_m(02) = 0001.0
            descr_m(03) =    13.30  : descr_m(04) = 0016.0

            call "PLOWCODE" (#06, plowkey$, descr$, 9000%, 0.42, f1%(06),~
                             header$(), 0, 0, inc(), inc$(), "D", " ",   ~
                             #55, descr_m())

            if f1%(06) = 1% then L50284
                errormsg$ = "Session Not On File" : return
L50284:     session$ = plowkey$
*          CALL "DESCRIBE" (#06, PLOWKEY$, SESSIONDESC$, 0%, F1%(06))
            call "READ100" (#06, session$, f1%(06))
            get #06 using L50289, sessiondesc$, sessiondate$
            call "DATEFMT" (sessiondate$)
L50289:     FMT POS(13), CH(30), POS(66), CH(6)
            return

L50300: REM Test for Sort Order           SORTNUM1$, SORTNUM2$, SORTNUM3$
            if (pos("ACGPRV" = sortnum1$)) = 0% then L50350
            if (pos("ACGPRV" = sortnum2$)) = 0% then L50350
            if (pos("ACGPRV" = sortnum3$)) = 0% then L50350
            if sortnum2$ = sortnum1$ or sortnum3$ = sortnum2$            ~
                                     or sortnum3$ = sortnum1$            ~
                then  errormsg$ = "Can not use same criteria twice."
                return
L50350:     errormsg$ = "'A', 'C', 'G', 'P', 'R', or 'V' Please."
                return

L50400: REM Test for Print Choice                 PRINTTOLTYPE$
            if (pos("IOB" = printtoltype$)) = 0%                         ~
                then errormsg$ = "'I', 'O', or 'B' Please"
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:######

*       * Header Line 2
L60110: %                                     ###########################~
        ~#################################                     PAGE: ####

*       * Header Line 3
L60134: %                                     ###########################~
        ~#################################

*       * Column Header Lines
        %################################################################~
        ~#################################################################~
        ~#

*       * Summary Report Line
L60200:  %###############################################################~
        ~# ######### ######### ######### ######### # ##### ##### ########


*       * Sub Total Line 1
L60250: %    ** SUBTOTALS   QUAN OF PARTS: ##########  QUAN IN TOL: #####~
        ~#####  PERCENT: #####


*       * Sub Total Line 2
L60290: %       QUAN VAR (+): ##########  QUAN VAR (-): ##########  QUAN ~
        ~VAR(ABS): ##########  BOH QUAN: ########## PERCENT: #####

*       * Sub Total Line 3
L60330: %      VALUE VAR (+): ########## VALUE VAR (-): ########## VALUE ~
        ~VAR(ABS): ########## BOH VALUE: ########## PERCENT: #####

*       * Totals Line 1
L60370: %   *** TOTALS      QUAN OF PARTS: ##########  QUAN IN TOL: #####~
        ~#####  PERCENT: #####

*       * Variance Reason Totals
L60410: %    --------- VARIANCE REASON TOTALS ---------

*       * Variance Reason Totals  Header Line 1
L60440: %               VARIANCE       COUNT

*       * Variance Reason Totals  Header Line 2
L60470: %               --------       -----

*       * Variance Reason Line
L60500: %                ######        #####

        %** Report Title for page 0
        %############################################################

L64990:          %                         * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   @   ########   * * * * * * * * * *

        REM THISPROGRAMWASGENERATEDBYGENRPPGMAPROPRIETRYPRODUCTOFCAELUS**~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
