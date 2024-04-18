        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y   AAA   BBBB    CCC                  *~
            *  H   H  NN  N  Y   Y  A   A  B   B  C   C                 *~
            *  HHHHH  N N N   YYY   AAAAA  BBBB   C                     *~
            *  H   H  N  NN    Y    A   A  B   B  C   C                 *~
            *  H   H  N   N    Y    A   A  BBBB    CCC                  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYABC   - Sequences Inventory for ABC Classification.    *~
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
            * 08/06/87 ! Original - Rewrite of former.            ! KAB *~
            * 06/23/92 ! Cycle Count Project Integration          ! RJH *~
            *          ! PRR 10900 - Force low cost parts to 'C'  !     *~
            *          !             or 'D' Class                 !     *~
            *          ! PRR 12282 - Range selection options      !     *~
            *          ! Insure Integer Arrays Indices & STR Fnctn!     *~
            * 09/16/92 ! ASKUSER to confirme ABC update to File   ! RJH *~
            *          ! Gross Inventory Value %age to include D's!     *~
            *          ! and Total w/ D's Displayed               !     *~
            * 09/28/92 ! Calc. by Future Activity to Use Workfile.! RJH *~
            *          !  Total & Unit Value on Report to 2 places!     *~
            *          !  'D' Screen Vars set from (WHEN%) Arrays !     *~
            * 04/08/93 ! PRR 12768 Online documentation accessible! JIM *~
            *          !    from all screens (nixed HNYABCXX's).  !     *~
            * 04/12/93 ! PRR 10647 OnHand/Past/Fut Usage col hdr. ! JIM *~
            * 04/12/93 ! Standard Page 0 FAC removal method.      ! JIM *~
            * 05/06/93 ! My turn. Past usage now has option to    ! JDH *~
            *          !   pick trans type. Also, archived files  !     *~
            *          !   are searched for and used.             !     *~
            *          ! Loosened date restrictions for past usage!     *~
            *          ! PRR 12874. Open Workfiles w/better sizes.!     *~
            * 05/17/93 ! Classification now considers parts with  ! JDH *~
            *          !   no activity (future, current, or past).!     *~
            * 02/03/95 ! Workfile Key changed frm 44 to 25.       ! RJH *~
            * 08/07/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            abclockflag$1,               /* Lock Flag 'Y' = No update  */~
            arch_beg$6,                  /* Archive file starting date */~
            arch_end$6,                  /* Archive file ending date   */~
            archname$8,                  /* Archive file name HNYDYYYY */~
            begin$(3%)10,                /* Date Range                 */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            breakl$(4%)11,               /* Display Break Point        */~
            breakh$(4%)11,               /* Display Break Point        */~
            breaktotal$(3%)11,           /* Display Break Total        */~
            breakl(3%,3%),               /* Break Point                */~
            breakh(3%,3%),               /* Break Point                */~
            breaktotal(3%,3%),           /* Break Total                */~
            ccgroup$6,                   /* Cycle Count Group Name     */~
            class$1,                     /* ABC Classification         */~
            classify_mode$1,             /* Selection                  */~
            classold$1,                  /* Previous ABC Class         */~
            cnttol_a$10,                 /* Count Tolerance for Class A*/~
            cnttol_b$10,                 /* Count Tolerance for Class B*/~
            cnttol_c$10,                 /* Count Tolerance for Class C*/~
            cnttol_d$10,                 /* Count Tolerance for Class D*/~
            company$60,                  /* Company Name               */~
            cost$10,                     /* Report Unit Value          */~
            count$10,                    /* Item Counter               */~
            cursor%(2%),                 /* Cursor location for edit   */~
            dash$1,                      /* Cosmetics                  */~
            date$8,                      /* Date for screen display    */~
            day1$10,                     /* Fisrt Day                  */~
            dcntr$10,                    /* Count of 'D' Parts         */~
            dpct$1,                      /* Display Percent sign       */~
            dpct$(6%)4,                  /* Display Percent            */~
            edtmessage$79,               /* Edit screen message        */~
            end$(3%)10,                  /* Date Range                 */~
            errormsg$79,                 /* Error message              */~
            ext$12,                      /* Report Ext. Value          */~
            fac$1,                       /* Cosmetics                  */~
            fac1$1,                      /* Cosmetics                  */~
            fmccgroup$6,                 /* FROM Cycle Count Group Name*/~
            hdr$(2%)79,                  /* Table Headers              */~
            header$79,                   /* Range Selection Sub Header */~
            hiccgroup$6,                 /* High Cycle Count Group Name*/~
            i0$(24%)80,                  /* Screen Zero Image          */~
            i1$(24%)80,                  /* Screen One Image           */~
            i3$(24%)80,                  /* Screen Three Image         */~
            items%(3%,3%),               /* Items in each Class        */~
            items$(3%)10,                /* Display Items per Class    */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            loc%(2),                     /* SEARCHIN'                  */~
            loccgroup$6,                 /* Low Cycle Count Group Name */~
            onhand$10, colhdr$10,        /* Report On Hand             */~
            part$25,                     /* Part Code                  */~
            partdescr$32,                /* Part Description           */~
            partstrlot$44,               /* Part/Store/Lot             */~
            past_what$(14%)1,            /* What type of past data     */~
            pct%(3%),                    /* ABC Per Cents              */~
            pct$(3%)2,                   /* ABC Per Cents              */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pf16$16,                     /* PF Range Select Literal    */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            r$(24%)80,                   /* Range Selectn Screen Image */~
            rangeline$44,                /* Screen Range Header        */~
            rank$1,                      /* Comparison Basis           */~
            reason$7,                    /* Reason for exclusion       */~
            savefile$1,                  /* Set ABC to which file      */~
            scrn_0_msg$79,               /* Screen 0 Line text         */~
            sign_switch$15,              /* Screen Message             */~
            temp1$10,                    /* Temporary Variabe          */~
            temp2$10,                    /* Temporary Variabe          */~
            tempclass$10,                /* Temporary Variabe          */~
            tempval$79,                  /* Get Code  Variabe          */~
            tran$2,                      /* Transaction type           */~
            ttemp$10,                    /* Temporary Variabe          */~
            tttemp$10,                   /* Temporary Variabe          */~
            test$1,                      /* test                       */~
            testdate$(2%)8,              /* Date Range                 */~
            threshold_c$10,              /* Threshold Value,force to C */~
            threshold_d$10,              /* Threshold Value,force to C */~
            time$8,                      /* Report Time                */~
            toccgroup$6,                 /* TO Cycle Count Group Name  */~
            today$8,                     /* Date Range                 */~
            totalitems%(3%),             /* Current Total for Period   */~
            totalitems$10,               /* Current Total for Period   */~
            total_d_items%(3%),          /* Current Total for D Items  */~
            total_d_dollars(3%),         /* Current Total for D Items  */~
            total_d_dollar$11,           /* Current Total for D Items  */~
            totaldollars(3%),            /* Current Total for Period   */~
            totaldollar$11,              /* Current Total for Period   */~
            total$10,                    /* Totals descriptor          */~
            totdollar$11,                /* Current Total              */~
            totdollar(3%),               /* Current Total              */~
            totitems$10,                 /* Current Total              */~
            totitems%(3%),               /* Current Total              */~
            type$1,                      /* Unit or Gross              */~
            userid$3,                    /* Current User Id            */~
            when$1,                      /* What Period                */~
            workfile$(3%)36,             /* Work File Status           */~
            xstatus$2                    /* ABC 'X' Status condition   */~

        dim f2%(64%),                    /* = 0 if the file is open    */~
            f1%(64%),                    /* = 1 if READ was successful */~
            fs%(64%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64%)20                 /* Text from file opening     */

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
            * # 1 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * # 2 ! PIPOUT   ! Planned inventory use detail rec         *~
            * # 3 ! HNYDETAL ! INVENTORY DETAILS                        *~
            * # 4 ! HNYMASTR ! Inventory Master File                    *~
            * # 5 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 9 ! HNYDYYYY ! Inventory Details Archived File          *~
            * #10 ! ARCHVREC ! Archived History File                    *~
            * #11 ! WORKFIL1 ! Temporary System Workfile                *~
            * #12 ! WORKFIL2 ! Temporary System Workfile                *~
            * #13 ! WORKFIL3 ! Temporary System Workfile                *~
            * #23 ! HNYCCMST ! Cycle Count Master File                  *~
            * #25 ! HNYCCGRP ! Cycle Count Group File                   *~
            * #27 ! STORNAME ! Store Information File                   *~
            * #29 ! CATEGORY ! Inventory Category Code File             *~
            * #51 ! WORKFL51 ! Temporary System Work File               *~
            * #52 ! WORKFL52 ! Temporary System Work File               *~
            * #53 ! WORKFIL3 ! List of Archived HNYDETAL files          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            select # 2, "PIPOUT",                                        ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select # 3, "HNYDETAL",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =   43, keylen =   6, dup,    ~
                            key  2, keypos =   49, keylen =   2, dup     ~

            select # 4, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   43, keylen =   9, dup,    ~
                            key  2, keypos =   49, keylen =   4, dup     ~

            select # 5, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select # 9, "HNYDYYYY",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =   43, keylen =   6, dup,    ~
                            key  2, keypos =   49, keylen =   2, dup

            select #10, "ARCHVREC",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  30

            select #11, "WORKFIL1",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =   10, keylen =  25,                     ~
                        alt key  1, keypos =   60, keylen =  7, dup,     ~
                            key  2, keypos =   68, keylen =  7, dup,     ~
                            key  3, keypos =    1, keylen =  34          ~

            select #12, "WORKFIL2",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =   10, keylen =  25,                     ~
                        alt key  1, keypos =   60, keylen =  7, dup,     ~
                            key  2, keypos =   68, keylen =  7, dup,     ~
                            key  3, keypos =    1, keylen =  34          ~

            select #13, "WORKFIL3",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =   10, keylen =  25,                     ~
                        alt key  1, keypos =   60, keylen =  7, dup,     ~
                            key  2, keypos =   68, keylen =  7, dup,     ~
                            key  3, keypos =    1, keylen =  34          ~

            select #23, "HNYCCMST",                                      ~
                        varc,     indexed,  recsize =  796,              ~
                        keypos =    1, keylen =  44,                     ~
                        alt key  3, keypos =   73, keylen =  15, dup,    ~
                            key  2, keypos =   81, keylen =   7, dup,    ~
                            key  1, keypos =   45, keylen =   6, dup     ~

            select #25, "HNYCCGRP",                                      ~
                        varc,     indexed,  recsize =  616,              ~
                        keypos =    1, keylen =   6                      ~

            select #27, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3

            select #29, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos  =   1, keylen = 4

            select #51, "WORKFL51",                                      ~
                        varc,     indexed,  recsize =   44,              ~
                        keypos  =   1, keylen = 44

            select #52, "WORKFL52",                                      ~
                        varc,     indexed,  recsize =   44,              ~
                        keypos  =   1, keylen = 25

            select #53, "WORKFL53",                                      ~
                        varc,     indexed,  recsize =  08,               ~
                        keypos =  1,   keylen = 08

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1%), f2%( 1%), 0%, rslt$( 1%))
                get str(rslt$(1%),17%,4%) using L03052, q_size%
                q_size% = max(q_size%, 200%)
            call "OPENCHCK" (# 2, fs%( 2%), f2%( 2%), 0%, rslt$( 2%))
                get str(rslt$(2%),17%,4%) using L03052, p_size%
                p_size% = max(p_size%, 200%)
            call "OPENCHCK" (# 3, fs%( 3%), f2%( 3%), 0%, rslt$( 3%))
                get str(rslt$(3%),17%,4%) using L03052, d_size%
                d_size% = max(d_size%, 200%)
            call "OPENCHCK" (# 4, fs%( 4%), f2%( 4%), 0%, rslt$( 4%))
                get str(rslt$(4%),17%,4%) using L03052, m_size%
L03052:              FMT BI(4)
                m_size% = max(m_size%, 200%)
            call "OPENCHCK" (# 5, fs%( 5%), f2%( 5%), 0%, rslt$( 5%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
                get str(rslt$(10%),17%,4%) using L03052, a_size%
                a_size% = max(a_size%, 200%)
            call "OPENCHCK" (#23, fs%(23%), f2%(23%), 0%, rslt$(23%))
                get str(rslt$(23%),17%,4%) using L03052, c_size%
                c_size% = max(c_size%, 200%)
            call "OPENCHCK" (#25, fs%(25%), f2%(25%), 0%, rslt$(25%))
            call "OPENCHCK" (#27, fs%(27%), f2%(27%), 0%, rslt$(27%))
            call "OPENCHCK" (#29, fs%(29%), f2%(29%), 0%, rslt$(29%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            today$, date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            rangeline$ = "                Group Range   From      To"
            scrn_0_msg$ = "Calculate the ABC Classification from a"  &   ~
                          " Range of Parts that are:"
            str(line2$,62%) = "  HNYABC: " & str(cms2v$,,8%)

            call "COMPNAME" (2%, company$, ret%)
            gosub set_cc_sys_vars
            goto inputmode

        set_screen_1_variables
            call "READ100" (#5, "MONTHS OPEN", f1%(5%))
                if f1%(5%) = 0% then L65000
            get #5, using L09190, day1$
L09190:         FMT POS(33), CH(6)

            testdate$(1%) = "19010101"
            call "DATECONV" (testdate$(1%))

            call "DATE" addr("G+", day1$, 489%, testdate$(2%), ret%)

            begin$(1%) = today$
            end$  (1%) = testdate$(2%)
            call "DATFMTC" (begin$(1%))
            call "DATFMTC" (end$  (1%))

            if begin$(3%) <> " " and begin$(3%) <> blankdate$ then L09420
                begin$(3%) = testdate$(1%)
                call "DATE" addr("G+" , today$, -1%, end$(3%), ret%)
                call "DATFMTC" (begin$(3%))
                call "DATFMTC" (end$  (3%))

L09420:     hdr$(1%) = "               Percentage         Breakpoints    ~
        ~  Number     Gross Inventory"
            hdr$(2%) = "Classification  of Total       High           Low~
        ~  of Items         Value     "

            plowkey$ = "CYCLE COUNT:05.00.00"
            call "READ100" (#5, plowkey$, f1%(5%))
                if f1%(5%)  = 0% then input_mode_1
            get #5, using L09476, temp1$, temp2$, type$, rank$, pct$(1%),  ~
                pct$(2%), pct$(3%), when$, str(past_what$())
L09476:     FMT POS(22), 2*CH(6), 2*CH(1), 3*CH(2), CH(1), XX(62), CH(14)

            when% = pos("FCP" = when$)
            begin$(when%) = temp1$
            end$(when%)   = temp2$
            call "DATFMTC" (begin$(when%))
            call "DATFMTC" (end$  (when%))
            return

        set_cc_sys_vars
            sysvarsok% = 0%
            plowkey$ = "CYCLE COUNT:05.00.00"
            call "READ100" (#5, plowkey$, f1%(5%))
                if f1%(5%)  = 0% then return
            sysvarsok% = 1%
            get #5, using L09700, temp1$, temp2$, type$, rank$, pct$(1%),  ~
                pct$(2%), pct$(3%), when$, cnttol_a$, cnttol_b$,         ~
                cnttol_c$, cnttol_d$, threshold_c$, threshold_d$,        ~
                str(past_what$())
L09700:     FMT POS(22), 2*CH(6), 2*CH(1), 3*CH(2), CH(1), XX(2),        ~
                6*CH(10), CH(14)
            return

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            goto input_mode_2    /* Screen 0 */

        input_mode_1   /* Screen 1 - Main ABC Selection Screen */
            for fieldnr% = 1% to  6%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10170
L10152:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10152
L10170:               if keyhit% <> 10% then       L10210
                         if fieldnr% <> 4% then L10130
L10174:                  gosub select_trans
                         if keyhit% =  1% then gosub startover
                         if keyhit% = 16% then goto L10130
                         if keyhit% <> 0% then goto L10174
L10210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <>  0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%
            goto editpg1

        input_mode_2   /* Screen 0 - Initial Screen */
            for fieldnr% = 1% to  3%
L10300:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10420
L10320:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10400
L10350:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L10320
                         if fieldnr% = 1% then L10300
                         goto L10350
L10400:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10320
L10420:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10320
            next fieldnr%
            goto editpg2

        input_mode_3  /* Cycle Count Selection Screen */
            for fieldnr% = 1% to  6%
L10480:         gosub'053(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10600
L10500:         gosub'103(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10580
L10530:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'053(fieldnr%)
                         if enabled% = 1% then L10500
                         if fieldnr% = 1% then L10480
                         goto L10530
L10580:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 8% then L10590
                          pf16$ = hex(84) & "(16)RETURN"
                          gosub select_ranges
                          goto L10500
L10590:               if keyhit% <> 0% then       L10500
L10600:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10500
            next fieldnr%
            goto editpg3

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0% : gosub loadscreen
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 11% or fieldnr% > 13% then L11130
               fieldnr% = 6% : goto L11140
L11130:     if fieldnr% < 1% or fieldnr% >  7% then editpg1
            if fieldnr% = 3% then editpg1
            if fieldnr% > 3% then fieldnr% = fieldnr% - 1%
            if fieldnr% = 5% then fieldnr% = 4%
            if fieldnr% > 5% then fieldnr% = fieldnr% - 1%
L11140:     if fieldnr% = lastfieldnr% then editpg1
            gosub loadscreen
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <> 10% then goto L11190
L11184:              gosub select_trans
                     if keyhit% =  1% then gosub startover
                     if keyhit% = 16% then goto L11170
                     if keyhit% <> 0% then goto L11184
L11190:           if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM Let's try to show something

        loadscreen

            fac$, fac1$ = hex(84)
            breakl$(), breakh$(), breaktotal$(), items$(), total$, dash$,~
            str(workfile$(when%),1%,1%), totalitems$, totaldollar$,      ~
            dpct$, dpct$(), total_d_dollar$, totdollar$, dcntr$,         ~
            totitems$   = " "
            dpctt = 0
            if workfile$(when%) = " " then return
            totdollar = totdollar(when%)
            if when% = 2% then L12150
               temp1$ = begin$(when%)
               call "DATUFMTC" (temp1$)
               temp2$ = end$(when%)
               call "DATUFMTC" (temp2$)
            if str(workfile$(when%),2%,6%)<> str(temp1$,1%,6%) then return
            if str(workfile$(when%),8%,6%)<> str(temp2$,1%,6%) then return
L12150:     if str(workfile$(when%),14%,1%) <> type$ then return
            if str(workfile$(when%),15%,1%) <> rank$ then return
            if str(workfile$(when%),16%,2%) <> pct$(1%) then return
            if str(workfile$(when%),18%,2%) <> pct$(2%) then return
            if str(workfile$(when%),22%,1%) <> when$ then return
            if str(workfile$(when%),23%,14%) <> str(past_what$())        ~
                                                        then return
            for i% = 1% to 3%
            if breakl(when%,i%) < 1e8 then                               ~
                call "CONVERT" (breakl(when%,i%), 2.2, breakl$(i%)) else ~
                call "CONVERT" (breakl(when%,i%), 0.2, breakl$(i%))
            if breakh(when%,i%) < 1e8 then                               ~
                call "CONVERT" (breakh(when%,i%), 2.2, breakh$(i%)) else ~
                call "CONVERT" (breakh(when%,i%), 0.2, breakh$(i%))
            if breaktotal(when%,i%) < 1e8 then                           ~
              call "CONVERT" (breaktotal(when%,i%), 2.2, breaktotal$(i%))~
              else                                                       ~
              call "CONVERT" (breaktotal(when%,i%), 0.2, breaktotal$(i%))
            convert items%(when%,i%) to items$(i%), pic(##########)
            if totdollar = 0 then L12240
              dpct = round(100*breaktotal(when%,i%)/totdollar,0)
              dpctt = dpctt + dpct
              convert dpct to dpct$(i%), pic(-###)
L12240:     next i%
              convert dpctt to dpct$(5%), pic(-###)
              if totdollar(when%) = 0 then dpct = 0 else                 ~
               dpct = round(100*total_d_dollars(when%)/totdollar(when%),0)
              dpctt = dpctt + dpct
              convert dpct to dpct$(4%), pic(-###)
            str(workfile$(when%),1%,1%) = "X"
            dash$ = "-"
            if totaldollars(when%) = 0 then L12260
               convert dpctt to dpct$(6%), pic(-###)
               dpct$ = "%"
               fac1$ = hex(a4)

L12260:     breakh$(4%) = threshold_d$
            call "CONVERT" (lo_d_value, 2.2, breakl$(4%))
            call "RJUSTIFY" (breakh$(4))
            convert totalitems%(when%) to totalitems$, pic(##########)
            convert totitems%(when%) to totitems$, pic(##########)
            convert total_d_dollars(when%) to total_d_dollar$,           ~
                                                      pic(###########)
            convert totdollar(when%) to totdollar$,  pic(###########)
            convert total_d_items%(when%) to dcntr$, pic(##########)
            call "CONVERT" (totaldollars(when%), 2.2, totaldollar$)
            fac$ = hex(a4)
            total$ = "* Totals *"

            return


        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       what_screen_now
                  if keyhit% <>  0% then       editpg2
L12400:     fieldnr% = cursor%(1%) -  9%
            if fieldnr% > 1% then fieldnr% = fieldnr% - 1%
            if fieldnr% < 1% or fieldnr% >  3% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L12450:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12450
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12450
                  lastfieldnr% = fieldnr%
            goto L12400

        editpg3
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then       editpg2
                  if keyhit%  =  8% then       cycle_count_range
                  if keyhit%  = 16% then       set_abc_screen
                  if keyhit% <>  0% then       editpg3
L12594:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% > 6% then fieldnr% = fieldnr% - 1%

            if fieldnr% < 1% or fieldnr% >  6% then editpg3
            if fieldnr% = lastfieldnr% then    editpg3
            gosub'053(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg3
L12660:     gosub'103(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12660
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12660
                  lastfieldnr% = fieldnr%
            goto L12594

        what_screen_now
            if classify_mode$ = "C" then goto input_mode_3
            pf16$ = hex(84) & "(16)Select Parms"
            selectiontype% = 3%   /* Store Field Disabled */
            gosub select_ranges
            if count% = 0% then editpg2   /* Nothing in Range Selection */
            goto set_abc_screen

        set_abc_screen
            gosub set_screen_1_variables
            goto input_mode_1

        cycle_count_range
            pf16$ = hex(84) & "(16)RETURN"
            selectiontype% = 1%   /* Store Field Enabled */
            gosub select_ranges
            if count% = 0% then editpg3
            goto  editpg3

        select_ranges
            /* Call Range Selection Subroutine and make Workfile of   */
            /* desired Part/Store/Lots to classify.                   */
            count% = 0%
            call "FILEBGON" (#51)
            f_size% = q_size% / 2%
            call "WORKOPEN" (#51, "IO", f_size%, f2%(51%))
            header$ = "Manage ABC Classification for Cycle Counting"
            call "HNYCCRNG" ("N", selectiontype%, #04, #01, #29, #27,    ~
                             #23, #51, count%, header$, pf16$, r$() )

            if count% = 0% then L13090
                fmccgroup$ = "RANGE" : toccgroup$ = " " :  fieldnr% = 2%
                return

L13090:     aukey% = 0%
            call "ASKUSER" (aukey%, " ", "No Records Selected that Meet" ~
                            & " the Range Criteria",                     ~
                            "Press Anykey to Return", " ")
            return

        make_group_workfile
            /* Make workfile of desired Part/Store/Lots to classifly    */
            /*   from Cycle Count Groups.                               */
            call "SHOSTAT" ("Compiling Part Population")
            call "FILEBGON" (#51)
            f_size% = c_size% / 2%
            call "WORKOPEN" (#51, "IO", f_size%, f2%(51%))

            if fmccgroup$ = "ALL"  or  fmccgroup$ =  "FIRST"             ~
                  then plowkey$ =  all(hex(00))                          ~
                  else plowkey$ =  fmccgroup$

            call "REDALT4" (#23, plowkey$, 1%, f1%(23%))
            goto L13300

        loop_group    /* CC Group Selection Loop */
            call "READNEXT" (#23, f1%(23))
L13300:     if  f1%(23%) = 0% then  return
            if fmccgroup$ <> "ALL"  and                                  ~
               key(#23, 1%) > hiccgroup$ then return
            get #23 using L13520, partstrlot$, ccgroup$

            write  #51, partstrlot$
            goto loop_group
            /* ** END of CC Group Method of Selection ** */

            /* HNYCCMST File */
L13520:     FMT CH(44),CH(6)


        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************
        datasave
            if fmccgroup$ <> "RANGE" then gosub make_group_workfile
            if str(workfile$(when%),1%,1%) <> " " then L19184
            temp1$, temp2$ = " "
            if when% = 2% then L19130
               temp1$ = begin$(when%)
               call "DATUFMTC" (temp1$)
               temp2$ = end$(when%)
               call "DATUFMTC" (temp2$)
L19130:     if workfile$(when%) = " " then L19160
            if when% = 2% then L19170
               if str(workfile$(when%),23%,14%) <> str(past_what$())     ~
                     then L19160
               if str(workfile$(when%),2%,6%) <> str(temp1$,1%,6%)       ~
                     then L19160
               if str(workfile$(when%),8%,6%)  = str(temp2$,1%,6%)       ~
                     then L19170
L19160:     gosub dataload
            if totalitems%(when%) + total_d_items%(when%) > 0% then L19170
               errormsg$ = "Warning - No Items Matching the Criteria Were~
        ~ Found."
               workfile$(when%) = " "
               goto editpg1
L19170:     gosub calculate
            goto  editpg1

L19184:     aukey% = 0%
            call "ASKUSER" (aukey%, "CONFIRM", "Master File(s) ABC "     ~
                            & "Classification Will Be Updated"    ,      ~
                            "Press Return to Continue",                  ~
                            "Press PF1 to Abort")
            if aukey% = 1% then editpg1
            if aukey% <> 0% then L19184
            gosub dataput
            goto  inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Unit cost/Extension    */~
                              L20200,         /* Gross or Count         */~
                              L20300,         /* What Period            */~
                              L20330,         /* What Type of Past      */~
                              L20400,         /* Date Range             */~
                              L20600          /* ABC Percents           */
            return

L20100: REM Def/Enable Unit Value/Extension
            return

L20200: REM Def/Enable Gross $ or Count
            return

L20300: REM Def/Enable What Period
            return

L20330: REM Def/Enable What Type of Past           PAST_WHAT$
            if when% <> 3% then enabled% = 0%
            if str(past_what$()) <> " " then return
                past_what$( 4%) = "X"
                past_what$( 9%) = "X"
                past_what$(13%) = "X"
            return

L20400: REM Def/Enable Date Range                  BEGIN$(1)
            if when% <> 2% then L20440
               enabled% = 0%
               return
L20440:     if when% = 3% then L20530     /* Past */

               if begin$(1%) <> " " and begin$(1%) <> blankdate$ then return
                  begin$(1%) = today$
                  end$  (1%) = testdate$(2%)
                  call "DATFMTC" (begin$(1%))
                  call "DATFMTC" (end$  (1%))
                  return

L20530:        if begin$(3%) <> " " and begin$(3%) <> blankdate$ then return
                  begin$(3%) = testdate$(1%)
                  call "DATE" addr("G+" , today$, -1%, end$(3%), ret%)
                  call "DATFMTC" (begin$(3%))
                  call "DATFMTC" (end$  (3%))
                  return

L20600: REM Def/Enable ABC Percents
            return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L21100,         /* Selection              */~
                              L21130,         /* Maximum C Value        */~
                              L21190          /* Maximum D Value        */
            return
L21100: REM Def/Enable Selection                   CLASSIFY_MODE$
            return

L21130: REM Def/Enable Maximum C Value             THRESHOLD_C$
            if threshold_c$ = " " then  threshold_c$  = "0.0"


            return

L21190: REM Def/Enable Maximum D Value             THRESHOLD_D$
            if threshold_d$ = " " then  threshold_d$  = "-0.01"


            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  3  of Input. *~
            *************************************************************

        deffn'053(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L22150,         /* Cycle Count Group      */~
                              L22190,         /* Cnt Tol. Class A       */~
                              L22220,         /* Cnt Tol. Class B       */~
                              L22250,         /* Cnt Tol. Class C       */~
                              L22280,         /* Cnt Tol. Class D       */~
                              L22370          /* Save ABC Class in      */
            return
L22150: REM Def/Enable Cycle Count Group Name      FMCCGROUP$
            if fmccgroup$ = " " then  fmccgroup$ = "ALL"
            return

L22190: REM Def/Enable Count Tolerance for Class A CNTTOL_A$
            if cnttol_a$ = " " then cnttol_a$ = "0"
            return

L22220: REM Def/Enable Count Tolerance for Class B CNTTOL_B$
            if cnttol_b$ = " " then cnttol_b$ = "0"
            return

L22250: REM Def/Enable Count Tolerance for Class C CNTTOL_C$
            if cnttol_c$ = " " then cnttol_c$ = "0"
            return

L22280: REM Def/Enable Count Tolerance for Class D CNTTOL_D$
            if cnttol_d$ = " " then cnttol_d$ = "0"
            return


L22370: REM Def/Enable Save ABC Class in           SAVEFILE$
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            if scrnr% = 2% then restore line = scrn2_msg, fieldnr%
            if scrnr% = 3% then restore line = scrn3_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter 'E' for Extension, 'U' for Unit Value                  ",~
         "Enter 'I' for Number of Items, 'G' for Gross Value           ",~
         "Enter 'F' for Future, 'C' for Current, 'P' for Past          ",~
         "Select Transaction Types by placing or removing 'X's. PF(10) to~
        ~ see a list.",                                                   ~
         "Enter Date Range                                             ",~
         "Enter Percents for 'A' Class and 'B' Class. 'C' is Calculated."

        scrn2_msg  :  data                                               ~
         "Enter Base Population Selection('A' = All,'C' = Cycle Count) ",~
         "Enter Threshold Value to Force Part to 'C' Class             ",~
         "Enter Threshold Value to Force Part to 'D' Class             "

        scrn3_msg  :  data                                               ~
         "Enter Cycle Count Group Name or PF(8) to Select by Range     ",~
         "Enter Counting percentage tolerance for Class A              ",~
         "Enter Counting percentage tolerance for Class B              ",~
         "Enter Counting percentage tolerance for Class C              ",~
         "Enter Counting percentage tolerance for Class D              ",~
         "Enter File(s) to Save ABC Class('B' = Both,'C' = Cycle Count)"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, workfile$(), breakh$(),    ~
                      type$, rank$, when$, pct$(), past_what$(),         ~
                      breakl$(), breaktotal$(), items$(), totalitems$,   ~
                      totaldollar$, total$, dash$, dpct$, dpct$()

            fac$, fac1$ = hex(84)
            when% = 2%
            mat pct% = zer
            mat totalitems% = zer
            mat total_d_items% = zer
            mat totaldollars = zer

            init(" ") errormsg$, inpmessage$, classify_mode$,            ~
                      fmccgroup$, toccgroup$,savefile$, test$, dcntr$,   ~
                      totitems$, totdollar$, total_d_dollar$

            totdollar = 0.0
            dcntr%            = 0%
            mat total_d_dollars  = zer

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
        startover_2
            return clear all
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            time$ = " "  :  call "TIME" (time$)
            file% = 10% + when%
            on when% goto L30064, L30066, L30068
L30064:         size% = q_size% / 2% : goto L30070
L30066:         size% = p_size% / 2% : goto L30070
L30068:         size% = d_size% / 2%
L30070:     call "SHOSTAT" ("Now Analyzing the Parts Files for"          ~
                            & " ABC Assignment")

            call "FILEBGON" (#52)
            call "WORKOPEN" (#52, "IO", f_size%/4%, f2%(52%))

            call "FILEBGON" (#file%)
            call "WORKOPEN" (#file%, "IO", size%, f2%(file%))
            totalitems%(when%), totaldollars(when%) = 0

            on when% gosub L30200, L30400, L30600
            if totalitems%(when%) + total_d_items%(when%) > 0% then L30160
               call "FILEBGON" (#file%):workfile$(when%) = " "
               return
L30160:     call "WORKOPN2" (#file%, "IO", size%, f2%(file%))
            return

L30200: REM Build Workfile for Future Activity

            call "DATE" addr("G-", day1$, temp1$, begin%, ret%)
            call "DATE" addr("G-", day1$, temp2$, end%  , ret%)
                 begin% = begin% + 1%
                 end%   = end%   + 1%

            init (hex(00)) plowkey$
L30234:     call "PLOWNEXT" (#51, plowkey$, 0%, f1%(51%))
               if f1%(51%) = 0% then return
            gosub check_abc_class
            if xstatus$ <> "OK" then L30234       /* Then get next one */

            lastsum, count, ext, cost, qty = 0
            class$ = " "
            put str(plowkey$,26%) using L30261, begin%, " "
L30261:         FMT BI(4), CH(10)
L30270:     call "PLOWALTS" (#2, plowkey$, 1%, 25%, f1%(2%))
               if f1%(2%) = 0% then L30320
            get #2 using L30285, temp%, onhand
L30285:         FMT POS(45), BI(4), POS(57), PD(14,4)
            if temp% > end% then L30320
            if onhand <= 0 then L30270  /* SKIP BYPRODUCTS */
               qty = qty + onhand
               goto L30270

L30320:     if qty < 0 then L30325
            call "STCCOSTS" (str(plowkey$,,25%), " ", #5, 1%, cost)
            ext = qty * cost
            gosub build_workwhen_file
L30325:     init (hex(ff)) str(plowkey$,26%)
            goto L30234

L30400: REM Build Workfile for Current value
            init (hex(00)) plowkey$
L30412:     call "PLOWNEXT" (#51, plowkey$, 0%, f1%(51%))
               if f1%(51%) = 0% then return
            gosub check_abc_class
            if xstatus$ <> "OK" then L30412       /* Then get next one */

            lastsum, count, ext, cost, qty = 0
            class$ = " "
            call "READ100" (#1, plowkey$, f1%(1%))
               if f1%(1%) = 0% then L30412
            get #1, using L30490, onhand, stdcost
L30490:         FMT POS(69), PD(14,4), POS(117), PD(14,4)
            qty = qty + onhand
            ext = (onhand*stdcost)
            lastsum = lastsum + stdcost
            count = count + 1

            if count = 0 then L30550
                if qty <> 0 then cost = ext/qty
                if cost <= 0 then class$ = "*"
                if cost <= 0 then cost = lastsum/count
L30550:     gosub build_workwhen_file
            goto L30412           /* Get next record */

L30600: REM Build Workfile for Past Usage

            init (hex(00)) plowkey$
L30620:     call "PLOWNEXT" (#51, plowkey$, 0%, f1%(51%))
L30625:        if f1%(51%) = 0% then return
            plowkey$ = key(#51)
            gosub check_abc_class
            if xstatus$ <> "OK" then L30620       /* Then get next one */

            df% = 3%
            lastsum, count, ext, qty = 0
            class$ = " "
            init (hex(00)) str(plowkey$,35%)

L30685:     call "PLOWNEXT" (#df%, plowkey$, 34%, f1%(df%)) /* HNYDETAL */
               if f1%(df%) = 0% then set_archive_for_look
            get #df% using L30700, ttemp$, tran$, onhand, cost
L30700:         FMT POS(43), CH(6), CH(2), 2*PD(14,4)
            mat loc% = zer
            search "IAICIPIQISIWJBJCJKJRJTPORTVT" = str(tran$,,2%) to    ~
                loc%() step 2%
            if loc%(1%) = 0% then L30685
            loc%(1%) = loc%(1%) / 2% + 1%
            if past_what$(loc%(1%)) = " " then L30685
            if ttemp$ < temp1$ then L30685
            if ttemp$ > temp2$ then L30685
*          IF COST  <= 0      THEN 30685
            if loc%(1%) = 4% or loc%(1%) =  6% or    /* IQs, IWs, JKs */ ~
               loc%(1%) = 9% or loc%(1%) = 13% then  /* & RTs are nor-*/ ~
               onhand = -onhand  /* mally neg(-). So, we change them. */
            qty = qty + onhand
            ext = ext + (cost*onhand)
            lastsum = lastsum + cost
            count = count + 1
            goto L30685

        done_looking_for_qtys
            if qty <> 0 then L30732
                if count <> 0 then cost = lastsum/count else cost = 0
                goto L30734
L30732:     cost = ext/qty
L30734:     gosub build_workwhen_file
            call "READNEXT" (#51, f1%(51))
            goto L30625

        set_archive_for_look
            /* Set up for reading through archived files.         */
            if nmbr_archives% = 0% then done_looking_for_qtys
                if df% <> 9% then L30760
                     close #09 : goto L30766
L30760:         temp$ = hex(0000000000000000)
                call "READ102" (#53, temp$, f1%(53%))
                     goto L30768
L30766:         call "READNEXT" (#53, f1%(53%))
L30768:         if f1%(53%) = 0% then done_looking_for_qtys
                     get #53, archname$
                     call "PUTPRNAM" addr(#09, archname$)
                     f2%(9%) = 1%
                     call "OPENCHCK" (#09, fs%(9%), f2%(9%), 0%, " ")
                     if f2%(9%) <> 0% then L30766
                          df% = 9%
                          plowkey$ = key(#51)
                          init (hex(00)) str(plowkey$,35%)
                          goto L30685

        check_abc_class
            xstatus$ = " "
            if classify_mode$ = "C" then L30850
                call "READ100" (#4, str(plowkey$,,25%), f1%(4%))
                if f1%(4%) = 0% then return        /* Shouldn't happen */
                get #4 using L30830, class$, abclockflag$
L30830:             FMT POS(111), CH(1), POS(118), CH(1)
                goto L30872

L30850:     call "READ100" (#23, plowkey$, f1%(23%))
            if f1%(23%) = 0% then return            /* Shouldn't happen */
            get #23 using L30865, abclockflag$, class$
L30865:         FMT POS(126), CH(1), CH(1)
            if class$ < hex(20) then class$ = " "
L30872:     p% = pos(" ABCD" = class$)
            if p% <> 0%  then goto L30878
                gosub write_x_part :  goto L30882
L30878:     if abclockflag$ <> "Y"  or p% = 1%  then L30883
                gosub write_lock_part
L30882:         xstatus$ = "X"  :  goto L30885
L30883:     xstatus$ = "OK"
L30885:     classold$ = class$
            return

        build_workwhen_file
            temp1 = 9999999999.999  - ext
            temp2 = 99999999.99999  - cost

            call "READ101" (#file%, str(plowkey$,,25%), f1%)
               if f1% = 0% then L30976
            get #file% using L30928, onhand, tempext, stdcost
L30928:         FMT POS(35), 3*PD(14,4)
            divisor = abs(qty) + abs(onhand)
            if divisor = 0 then  L30939   /* No adjustment to cost value */
            stdcost = (abs(stdcost*onhand) + abs(cost*qty)) / divisor
L30939:     if stdcost < threshold_d then  L30940 else  L30942
L30940:         total_d_dollars(when%) = total_d_dollars(when%) + ext
                goto L30943
L30942:         totaldollars(when%) = totaldollars(when%) + ext
L30943:     temp2 = 99999999.99999  - stdcost
            onhand = onhand + qty
            temp1 = temp1 - tempext : tempext = tempext + ext
            put #file% using L30956, onhand, tempext, stdcost, " ", temp2,~
                                    temp1
L30956:        FMT POS(35), 3*PD(14,4), CH(1), PD(14,5), PD(14,3)

            rewrite #file%
            return

L30976:     write #file% using L30983, " ", str(plowkey$,,25%), qty, ext, ~
                                      cost, " ", temp2, temp1, classold$
L30983:        FMT CH(9), CH(25), 3*PD(14,4), CH(1), PD(14,5), PD(14,3), ~
                   CH(1)
            if cost < threshold_d then L30986 else L30990
L30986:         total_d_items%(when%) = total_d_items%(when%) + 1%
                total_d_dollars(when%) = total_d_dollars(when%) + ext
                return

L30990:         totalitems%(when%) = totalitems%(when%) + 1%
                totaldollars(when%) = totaldollars(when%) + ext
            return

        REM *************************************************************~
            *       C A C U L A T E   A B C   C L A S S E S             *~
            *-----------------------------------------------------------*~
            * CLASSIFY/ SORT MAIM AND MANGLE THE DATA                   *~
            *************************************************************
            /* Variable classification:                                 */
            /*           RANK% - 1% --- Assign ABC by Item Number       */
            /*           RANK% - 2% --- Assign ABC by Gross Value       */
            /*           TYPE% - 1% --- Sort order by Unit Cost         */
            /*           TYPE% - 2% --- Sort order by Extended Value    */
            /* ******************************************************** */
        calculate
            file% = 10% + when%
            call "SHOSTAT"("Classifying Parts Found into ABC Categories")

            init (hex(00)) plowkey$
            call "PLOWAL1" (#file%, plowkey$, type%, 0%, f1%)
               if f1% = 0% then L31580
            class$ = "A"
            i% = 1%
            break% = 0%  :  dcntr% = 0%
            lo_d_value = threshold_d
            breakh(when%,1%), breakh(when%,2%), breakh(when%,3%) = 0
            breakl(when%,1%), breakl(when%,2%), breakl(when%,3%) = 0
            items%(when%,1%), items%(when%,2%), items%(when%,3%) = 0
            breaktotal(when%,1%), breaktotal(when%,2%),                  ~
            breaktotal(when%,3%) = 0

            breakoff = (pct%(1%) * totaldollars(when%))/100
            if rank% = 1% then                                           ~
                breakoff = round((pct%(1%) * totalitems%(when%))/100, 0)
            goto L31290

L31270:     call "READNXT1" (#file%, f1%)
               if f1% = 0% then L31580
L31290:     get #file% using L31300, ext, cost, ttemp$, tttemp$
L31300:         FMT POS(43), 2*PD(14,4), XX(1), 2*CH(8)
            tempclass$ = " "

            if cost <= threshold_d then goto force_to_d
            if cost <= threshold_c then goto force_to_c

L31310:     if class$ = "C" then L31450

            if rank% = 1% then L31430
               if breaktotal(when%,i%) + (ext/2) <= breakoff then L31450
L31350:           if class$ = "A" then class$ = "B" else class$ = "C"
                  i% = i% + 1%
                  breakoff = pct%(i%) * totaldollars(when%)
                  if rank% = 1% then                                     ~
                               breakoff = pct%(i%) * totalitems%(when%)
                     breakoff = breakoff/100
                  if rank% = 1% then breakoff = round(breakoff, 0)
                  goto L31310
L31430:        if items%(when%,i%) + 1% > breakoff then L31350

L31450:     if break% = i% then L31490
               if type% = 1% then breakh(when%,i%) = cost                ~
                             else breakh(when%,i%) = ext
               break% = i%
L31490:     gosub set_calc_tots
            if tempclass$ <> " " then L31530
                tempclass$ = class$
L31530:     put #file% using L31540, tempclass$, ttemp$
L31540:         FMT CH(1), CH(8)
            rewrite #file%
            goto L31270

L31580:     put str(workfile$(when%)) using L31600, "X", temp1$, temp2$,  ~
                type$, rank$, pct$(1), pct$(2), pct$(3), when$,          ~
                str(past_what$())
L31600:     FMT CH(1), 2*CH(6), 2*CH(1), 3*CH(2), CH(1), CH(14)
            gosub set_d_totals
            return

        force_to_c
            iold% = i%  :  i% = 3%
            gosub set_calc_tots
            i% = iold%
            tempclass$ = "C"
            goto L31530            /* Re-Write to File */

        force_to_d
            tempclass$ = "D"
            dcntr% = dcntr% + 1%
            if cost < lo_d_value  then  lo_d_value = cost
            goto L31530

        set_calc_tots

            breaktotal(when%,i%) = breaktotal(when%,i%) + ext
            items%(when%,i%) = items%(when%,i%) + 1%
            if type% = 1% then breakl(when%,i%) = cost                   ~
                          else breakl(when%,i%) = ext
            if type% = 2% then ttemp$ = tttemp$
            return

        set_d_totals
            breakh$(4) = threshold_d$
            call "RJUSTIFY" (breakh$(4))
            call "CONVERT" (lo_d_value, 2.2, breakl$(4))
            totalitems%(when%) = items%(when%, 1%) + items%(when%, 2%) + ~
                                   items%(when%, 3%)
            total_d_items%(when%) =  dcntr%
            totitems%(when%) = totalitems%(when%) + total_d_items%(when%)
            totdollar(when%)= totaldollars(when%) + total_d_dollars(when%)
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            init(hex(00)) plowkey$
            file% = 10% + when%
            call "SHOSTAT" ("Updating 'ABC' Classifications")

L32100:     call "PLOWNEXT" (#file%, str(plowkey$,,25%), 0%, f1%)
               if f1% <> 1% then L32190
            get #file%, using L32120, class$
L32120:        FMT CH(1)

            if classify_mode$ <> "C" then L32160/*Reg.Physical Inventory */
                gosub put_hnyccmst             /*Else Cycle Count       */
                if savefile$ = "C" then L32100  /*Only Update Cycle Count*/
L32160:         gosub put_hnymaster            /* or Both CC & PI       */
                goto L32100

            /* Reset Cycle Count System Variables */
L32190:     plowkey$ = "CYCLE COUNT"
            call "DELETE" (#5, plowkey$, 11%)
            write #5, using L32230, "CYCLE COUNT:05.00.00",               ~
                str(workfile$(when%)), cnttol_a$, cnttol_b$, cnttol_c$,  ~
                cnttol_d$, threshold_c$, threshold_d$, str(past_what$()),~
                " ", " "
L32230:         FMT CH(20), CH(24), 6*CH(10), CH(14), CH(198), CH(184)
            return

        put_hnymaster
            call "READ101" (#4, str(plowkey$,,25%), f1%(4%))
            if f1%(4%) = 0% then return
            get #4 using L32300, classold$, abclockflag$
L32300:         FMT POS(111), CH(1), POS(118), CH(1)
            if classold$ < hex(20) then classold$ = " "
            p% = pos(" ABCD" = classold$)
            if p% = 0%  then return
            if abclockflag$ = "Y"  and p% <> 1%  then return

            put #4, using L32340, class$
L32340:         FMT POS(111), CH(1)
            rewrite #4
            return

        put_hnyccmst
            init (hex(00))  str(plowkey$,26%)
L32400:     call "PLOWNEXT" (#51, plowkey$, 25%, f1%(51%))
            if f1%(51%) = 0 then return                /* Back for More */
            call "READ101" (#23, plowkey$, f1%(23%))
            if f1%(23%) = 0 then L32400 /* Get Another of same part type */
            get #23 using L32450, abclockflag$, classold$
L32450:         FMT POS(126), CH(1), CH(1)
            if classold$ < hex(20) then classold$ = " "
            p% = pos(" ABCD" = classold$)
            if p%  = 0%  then L32400
            if abclockflag$ = "Y" and p% <> 1% then L32400

            p% = pos("ABCD" = class$)
            on p% goto L32510, L32520, L32530, L32540
L32510:     cnt_tol = cnttol_a  :  goto L32560
L32520:     cnt_tol = cnttol_b  :  goto L32560
L32530:     cnt_tol = cnttol_c  :  goto L32560
L32540:     cnt_tol = cnttol_d  :  goto L32560

L32560:     put #23 using L32570, cnt_tol, class$, classold$,userid$,today$
L32570:         FMT POS(57), PD(14,4), POS(127), CH(1), CH(1), CH(3),CH(6)
            rewrite #23
            goto L32400

        write_lock_part
            call "READ100" (#52, str(plowkey$,,25%), f1%(52%))
            if f1%(52%) <> 0 then return
            put #52 using L32670, str(plowkey$,,25%), "L"
            write #52
            return
L32670:     FMT CH(25), CH(1)

        write_x_part
            call "READ100" (#52, str(plowkey$,,25%), f1%(52%))
            if f1%(52%) <> 0 then return
            put #52 using L32670, str(plowkey$,,25%), "X"
            write #52
            return

        REM *************************************************************~
            *                P R I N T   R E P O R T                    *~
            *-----------------------------------------------------------*~
            * PRINT THE APPROPRIATE WORKFILE                            *~
            *************************************************************
        print_report
            call "SHOSTAT" ("Printing Report")

            select printer (134)
            call "SETPRNT" ("HNY029", " ", size%, 0%)

            rptstotal, rptttotal, rptsitems%, rpttitems%, page%,         ~
            count% = 0%
            line% = 999%

            file% = when% + 10%

            for class% = 1% to 4%
                class$, plowkey$ = bin(64% + class%)
                init (hex(00)) str(plowkey$,2%)
L33190:         call "PLOWALTS" (#file%, plowkey$, 3%, 1%, f1%)
                   if f1% = 0% then L33360
                get #file% using L33220, part$, onhand, ext, cost, ttemp$,~
                                        classold$
L33220:             FMT POS(10), CH(25), 3*PD(14,4), CH(1), XX(16), CH(1)

                call "DESCRIBE" (#4, part$, partdescr$, 0%, f1%(4%))
                call "CONVERT" (onhand, 2.2, onhand$)
                call "CONVERT" (cost, 4.4, cost$)
                call "CONVERT" (ext, 2.2, ext$)
                if line% > 55% then gosub print_header
                count% = count% + 1%
                rptsitems% = rptsitems% + 1%
                rptstotal = rptstotal + ext
                convert count% to count$, pic(######)
                print using L34170, count$, part$, partdescr$, class$,    ~
                                  classold$, onhand$, cost$, ttemp$, ext$
                line% = line% + 1%
                goto L33190

L33360:         part$ = "* SUBTOTAL FOR CLASS X *"
                str(part$,22%,1%) = class$
                put partdescr$ using L33390, class$, rptsitems%
L33390: %CLASS # CONTAINS ###### ITEMS
                call "CONVERT" (rptstotal, 2.2, ext$)
                if line% > 55% then gosub print_header
                print
                print using L34170, " ", part$, partdescr$, " ", " ", " ",~
                                   " ", " ", ext$
                print
                line% = line% + 3%
                rpttitems% = rpttitems% + rptsitems%
                rptttotal  = rptttotal  + rptstotal
                rptstotal, rptsitems% = 0
            next class%

                part$ = "* * * * TOTAL * * * *"
                put partdescr$ using L33540, rpttitems%
L33540: %###### ITEMS COUNTED
                call "CONVERT" (rptttotal, 2.2, ext$)
                print using L34170, " ", part$, partdescr$, " ", " ", " ",~
                                   " ", " ", ext$
                gosub print_xcluded_parts

                time$ = " "  :  call "TIME" (time$)
                print
                print using L34080, time$

                close printer
                call "SETPRNT" (" ", " ", 0%, 1%)
                return

        print_xcluded_parts
            init (hex(00)) plowkey$  :  count = 0
            gosub print_main_header
            gosub print_exclude_header
            call "READ104" (#52, plowkey$, f1%(52))
L33860:     if f1%(52) = 0% then L33915
            count = count + 1 :  line% = line% + 1%
            if line% < 57% then L33870
                gosub print_main_header
                gosub print_exclude_header
L33870:     get #52 using L32670, part$, class$
            if class$ = "X" then reason$ = "'X' ABC"                     ~
                            else reason$ = "LOCKED "
            call "DESCRIBE" (#4, part$, partdescr$, 0%, f1%(4%))
            print using L33960, part$, partdescr$, reason$
            call "READNEXT" (#52, f1%(52%))
            goto L33860

L33915:     call "CONVERT" (count, 0.01, count$)
            print
            print using L33955, count$
            return
        print_exclude_header
            print
            print using L33990
            print using L33970
            print using L33980
            line% = line% + 4%
            return

L33955: %           * * * * * * *  ##########  EXCLUDED PARTS        * * ~
        ~* * * * *
L33960: %            ########################  ##########################~
        ~#####  #######
L33970: %            PART NUMBER               DESCRIPTION               ~
        ~       REASON
L33980: %            ------------------------  --------------------------~
        ~-----  -------
L33990: %                     PARTS EXCLUDED FROM ABC PROCESS

L34000: %######## ########                   ############################~
        ~################################                     HNYABC  :HNY~
        ~029

L34040: %                                                    A B C  CLASS~
        ~IFICATION LISTING                                        PAGE:  #~
        ~###

L34080:         %                           * * * * * * * * *   E N D   O~
        ~ F   R E P O R T   @   ########   * * * * * * * * *

L34110: % ITEM NO.  PART CODE                  DESCRIPTION               ~
        ~        CLASS  OLD    ##########  UNIT VALUE    TOTAL VALUE

L34140: % --------  -------------------------  --------------------------~
        ~------  -----  -----  ----------  ----------   ------------

L34170: %  ######   #########################  ##########################~
        ~######    #      #    ##########  ########## # ############

        %INVENTORY IS SORTED BY UNIT VALUE
        %INVENTORY IS SORTED BY GROSS VALUE

        %CLASSIFICATION IS BY NUMBER OF ITEMS
        %CLASSIFICATION IS BY GROSS VALUE

        %ASIGNMENT RANGE IS FROM GROUP ###### TO ######

        %THE BASIS FOR CLASSIFICATION IS FUTURE ACTIVITY FROM ######## TO~
        ~ ########
        %THE BASIS FOR CLASSIFICATION IS CURRENT VALUE

        %THE BASIS FOR CLASSIFICATION IS PAST USAGE FROM ######## TO ####~
        ~####

L34330: %               PERCENTAGE         BREAKPOINTS       NUMBER     G~
        ~ROSS INVENTORY
L34350: %CLASSIFICATION  OF TOTAL       HIGH           LOW  OF ITEMS     ~
        ~    VALUE
L34370: %--------------  -------- ----------   ----------- ----------  --~
        ~-------- ------
L34390: %  '#' ITEMS       ## %  ########### - ########### ########## ###~
        ~######## #### %
L34402: %  '#' ITEMS             ########### - ########### ########## ###~
        ~######## #### %
L34410: %                                      ----------- ----------  --~
        ~-------- ------
L34430: %                                   * SUBTOTALS *  ########## ###~
        ~######## #### %

L34460: %                                      * TOTALS *  ########## ###~
        ~######## #### %

        print_main_header
            print page
            print using L34000, date$, time$, company$
            print using L34040, page%
            print
            line% = 3%
            return

        print_header
            if page% = 0% then gosub L34530
            page% = page% + 1%
L34530:     gosub  print_main_header
            if page% = 0% then print_cover
            print using L34110, colhdr$
            print using L34140
            line% = line% + 3%
            return

        print_cover
            gosub  print_screen_0
            gosub  print_screen_1
            if classify_mode$ <> "C"  then L34760
                gosub  print_screen_3
                if fmccgroup$ <> "RANGE"  then L34770
                gosub print_main_header
L34760:     gosub  print_range_selection
L34770:     gosub  print_classification_breakdown
            return

        print_classification_breakdown
            print skip(2)
            print tab(26);
            print "-----------------------   ABC Classification Breakdown~
        ~   -----------------------"
            print
            print tab(26);
            print using L34330
            print tab(26);
            print using L34350
            print tab(26);
            print using L34370
            print tab(26);
            print using L34390, "A", pct$(1%), breakh$(1%), breakl$(1%),  ~
                                items$(1%), breaktotal$(1%), dpct$(1%)
            print tab(26);
            print using L34390, "B", pct$(2%), breakh$(2%), breakl$(2%),  ~
                                items$(2%), breaktotal$(2%), dpct$(2%)
            print tab(26);
            print using L34390, "C", pct$(3%), breakh$(3%), breakl$(3%),  ~
                                items$(3%), breaktotal$(3%), dpct$(3%)
            print tab(26);
            print using L34410
            print tab(26);
            print using L34430, totalitems$, totaldollar$, dpct$(5%)

            print skip(1)
            print tab(26);
            print using L34402, "D", breakh$(4%), breakl$(4%), dcntr$,    ~
                               total_d_dollar$ ,dpct$(4%)

            print tab(26);
            print using L34410
            print tab(26);
            print using L34460, totitems$, totdollar$, dpct$(6%)
            return

        print_screen_0
L35132:     i% = pos(str(i0$()) > hex(7f))
            if i% = 0% then L35150
                str(i0$(), i%, 1%) = hex(20)
                goto L35132
L35150:     print skip(2)
            print tab(26);
            print "----------------------------   Selection  Parameters  ~
        ~ -------------------------"
            print
            for x% = 6% to 13% : print tab(26); i0$(x%) : next x%
            return


        print_range_selection
L36002:     i% = pos(str(r$()) > hex(7f))
            if i% = 0% then L36020
                str(r$(), i%, 1%) = hex(20)
                goto L36002
L36020:     print skip(2)
            print tab(26);
            print "-------------------------  Range Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 20% : print tab(26); r$(x%) : next x%
            return

        print_screen_1
L36102:     i% = pos(str(i1$()) > hex(7f))
            if i% = 0% then L36120
                str(i1$(), i%, 1%) = hex(20)
                goto L36102
L36120:     print skip(2)
            print tab(26);
            print "-------------------------   Classifications Parameters~
        ~   -----------------------"
            print
            for x% = 4% to 10% : print tab(26); i1$(x%) : next x%
            return

        print_screen_3
L36202:     i% = pos(str(i3$()) > hex(7f))
            if i% = 0% then L36220
                str(i3$(), i%, 1%) = hex(20)
                goto L36202
L36220:     print skip(2)
            print tab(26);
            print "-------------------------    Cycle Count Parameters   ~
        ~--------------------------"
            print
            for x% = 6% to 15% : print tab(26); i3$(x%) : next x%
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              str(line2$,,61%) = " "
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40190,         /* Unit Cost/extension  */~
                                L40190,         /* Gross$/Count         */~
                                L40190,         /* What Period          */~
                                L40190,         /* Type of Past Data    */~
                                L40190,         /* Date Range           */~
                                L40200          /* ABC Percents         */
              if when% <> 3% then lfac$(4%) = hex(9c)
              goto L40220

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40190:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40200:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40220:     accept                                                       ~
               at (01,02),                                               ~
                  "Managing ABC Classifications for Cycle Counting",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "  Base ABC Classifications on Unit Value (U) ~
        ~or Extension (E):",                                              ~
               at (04,66), fac(lfac$( 1%)), type$               , ch(01),~
                                                                         ~
               at (05,02), "Partition Inventory by Number of Items (I) or~
        ~ Gross Value (G):",                                              ~
               at (05,66), fac(lfac$( 2%)), rank$               , ch(01),~
                                                                         ~
               at (06,02), "   - Inventory Values and Extensions will be ~
        ~Calculated from -",                                              ~
               at (07,03), "       Future Activity (F), Current Value (C)~
        ~, Past Data (P):",                                               ~
               at (07,66), fac(lfac$( 3%)), when$               , ch(01),~
                                                                         ~
               at (08,11), "Trans Type: x IA  x IC  x IP  x IQ  x IS  x I~
        ~W  x JB",                                                        ~
               at (09,11), "            x JC  x JK  x JR  x JT  x PO  x R~
        ~T  x VT",                                                        ~
               at (08,23), fac(lfac$( 4%)), past_what$(1%)      , ch(01),~
               at (08,29), fac(lfac$( 4%)), past_what$(2%)      , ch(01),~
               at (08,35), fac(lfac$( 4%)), past_what$(3%)      , ch(01),~
               at (08,41), fac(lfac$( 4%)), past_what$(4%)      , ch(01),~
               at (08,47), fac(lfac$( 4%)), past_what$(5%)      , ch(01),~
               at (08,53), fac(lfac$( 4%)), past_what$(6%)      , ch(01),~
               at (08,59), fac(lfac$( 4%)), past_what$(7%)      , ch(01),~
               at (09,23), fac(lfac$( 4%)), past_what$(8%)      , ch(01),~
               at (09,29), fac(lfac$( 4%)), past_what$(9%)      , ch(01),~
               at (09,35), fac(lfac$( 4%)), past_what$(10%)     , ch(01),~
               at (09,41), fac(lfac$( 4%)), past_what$(11%)     , ch(01),~
               at (09,47), fac(lfac$( 4%)), past_what$(12%)     , ch(01),~
               at (09,53), fac(lfac$( 4%)), past_what$(13%)     , ch(01),~
               at (09,59), fac(lfac$( 4%)), past_what$(14%)     , ch(01),~
                                                                         ~
               at (10,02), "         Date Range - From:",                ~
               at (10,30), fac(lfac$( 5%)), begin$(when%)       , ch(10),~
               at (10,41), "to",                                         ~
               at (10,44), fac(lfac$( 5%)), end$  (when%)       , ch(10),~
                                                                         ~
               at (12,02), fac(hex(8c)), hdr$(1%)               , ch(79),~
               at (13,02), fac(hex(ac)), hdr$(2%)               , ch(79),~
                                                                         ~
               at (14,04), "'A' Items",                                  ~
               at (15,04), "'B' Items",                                  ~
               at (16,04), "'C' Items",                                  ~
               at (18,04), "'D' Items",                                  ~
                                                                         ~
               at (14,20), fac(lfac$(6%)), pct$(1%)             , ch(02),~
               at (15,20), fac(lfac$(6%)), pct$(2%)             , ch(02),~
               at (16,20), fac(lfac$(7%)), pct$(3%)             , ch(02),~
                                                                         ~
               at (14,23), "%",                                          ~
               at (15,23), "%",                                          ~
               at (16,23), "%",                                          ~
                                                                         ~
               at (14,26), fac(hex(84)), breakh$(1%)            , ch(11),~
               at (15,26), fac(hex(84)), breakh$(2%)            , ch(11),~
               at (16,26), fac(hex(84)), breakh$(3%)            , ch(11),~
               at (18,26), fac(hex(84)), breakh$(4%)            , ch(11),~
                                                                         ~
               at (14,38), fac(hex(8c)), dash$                  , ch(01),~
               at (15,38), fac(hex(8c)), dash$                  , ch(01),~
               at (16,38), fac(hex(8c)), dash$                  , ch(01),~
               at (18,38), fac(hex(8c)), dash$                  , ch(01),~
                                                                         ~
               at (14,40), fac(hex(84)), breakl$(1%)            , ch(11),~
               at (15,40), fac(hex(84)), breakl$(2%)            , ch(11),~
               at (16,40), fac(hex(84)), breakl$(3%)            , ch(11),~
               at (17,40), fac(hex(8c)), total$                 , ch(10),~
               at (18,40), fac(hex(84)), breakl$(4%)            , ch(11),~
               at (19,40), fac(hex(8c)), total$                 , ch(10),~
                                                                         ~
               at (14,52), fac(hex(84)), items$(1%)             , ch(10),~
               at (15,52), fac(hex(84)), items$(2%)             , ch(10),~
               at (16,52), fac(fac$   ), items$(3%)             , ch(10),~
               at (17,52), fac(hex(84)), totalitems$            , ch(10),~
               at (18,52), fac(fac$   ), dcntr$                 , ch(10),~
               at (19,52), fac(hex(84)), totitems$              , ch(10),~
                                                                         ~
               at (14,63), fac(hex(84)), breaktotal$(1%)        , ch(11),~
               at (15,63), fac(hex(84)), breaktotal$(2%)        , ch(11),~
               at (16,63), fac(fac$   ), breaktotal$(3%)        , ch(11),~
               at (17,63), fac(hex(84)), totaldollar$           , ch(11),~
               at (18,63), fac(fac$   ), total_d_dollar$        , ch(11),~
               at (19,63), fac(hex(84)), totdollar$             , ch(11),~
                                                                         ~
               at (14,75), fac(hex(84)), dpct$(1%)              , ch(04),~
               at (15,75), fac(hex(84)), dpct$(2%)              , ch(04),~
               at (16,75), fac(fac1$  ), dpct$(3%)              , ch(04),~
               at (17,75), fac(hex(84)), dpct$(5%)              , ch(04),~
               at (18,75), fac(fac1$  ), dpct$(4%)              , ch(04),~
               at (19,75), fac(hex(84)), dpct$(6%)              , ch(04),~
                                                                         ~
               at (14,80), fac(hex(8c)), dpct$                  , ch(01),~
               at (15,80), fac(hex(8c)), dpct$                  , ch(01),~
               at (16,80), fac(hex(8c)), dpct$                  , ch(01),~
               at (17,80), fac(hex(8c)), dpct$                  , ch(01),~
               at (18,80), fac(hex(8c)), dpct$                  , ch(01),~
               at (19,80), fac(hex(8c)), dpct$                  , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(84)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               if keyhit% <> 13% then L41011
                  call "MANUAL" ("HNYABC  ") : goto L40220

L41011:        if keyhit% <> 14% then L41020
                  gosub print_report : goto L40220

L41020:        if keyhit% <> 15% then L41050
                  call "PRNTSCRN" : goto L40220

L41050:        close ws
               call "SCREEN" addr ("C", u3%, "I", i1$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41240     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                 (4)Previous Field      " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffff0affff0dff0f1000)
            if fieldnr% = 1% then L41200
                str(pf$(3%),64%)  = " "  :  str(pfkeys$,16%,1%) = hex(ff)
L41200:     if fieldnr% > 2% then L41220
                str(pf$(2%),18%,26%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L41220:     return

L41240: if fieldnr% > 0% then L41390  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            if str(workfile$(when%),1%,1%) <> " " then L41350
                str(pf$(3%),64%)    = "(16)Calc. ABC"
                return
L41350:     str(pf$(3%),46%,16%) = "(14)Print Report"
            str(pfkeys$,14%,1%) = hex(0e)
            return

L41390:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffff0affff0dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   0                   *~
            *-----------------------------------------------------------*~
            * Initial Input Screen                                      *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'050(2%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L41640,         /* Selection         */   ~
                                L42550,         /* Max Value for C's */   ~
                                L42550          /* Max Value for D's */
              goto L41670

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L41640:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41670:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage ABC Classifications for Cycle Counting",       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), fac(hex(8c)), scrn_0_msg$            , ch(79),~
               at (07,13), "A = Based on ALL the Parts",                 ~
               at (08,13), "C = Based on the Cycle Count Population",    ~
                                                                         ~
               at (10,02), "Base Population Selection",                  ~
               at (10,31), fac(lfac$( 1%)), classify_mode$      , ch(01),~
                                                                         ~
               at (12,02), "Minimum Value for B Parts",                  ~
               at (12,31), fac(lfac$( 2%)), threshold_c$        , ch(10),~
                                                                         ~
               at (13,02), "Minimum Value for C Parts",                  ~
               at (13,31), fac(lfac$( 3%)), threshold_d$        , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(84)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L41930
                  call "MANUAL" ("HNYABC  ") : goto L41670

L41930:        if keyhit% <> 15% then L41960
                  call "PRNTSCRN" : goto L41670

L41960:        close ws
               call "SCREEN" addr ("C", u3%, "I", i0$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L42150     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                 (4)Previous Field      " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then return
                str(pf$(3%),64%)  = " "  :  str(pfkeys$,16%,1%) = hex(ff)
            return

L42150: if fieldnr% > 0% then L42240  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Select Range"
            if classify_mode$ <> "C" then L42220
                str(pf$(3%),64%)    = "(16)Cycle Parms"
L42220:     pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L42240:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
              gosub'050(3%, fieldnr%)
              gosub set_pf3
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42540,         /* Cycle Count Group */   ~
                                L42550,         /* Cnt Tol. Class A  */   ~
                                L42550,         /* Cnt Tol. Class B  */   ~
                                L42550,         /* Cnt Tol. Class C  */   ~
                                L42550,         /* Cnt Tol. Class D  */   ~
                                L42540          /* Save ABC Class in */
              goto L42570

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42540:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L42550:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42570:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage ABC Classifications for Cycle Counting",       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), fac(hex(ac)), rangeline$             , ch(47),~
                                                                         ~
               at (07,02), "Cycle Count Group Range",                    ~
               at (07,32), fac(lfac$( 1%)), fmccgroup$          , ch(06),~
               at (07,42), fac(lfac$( 1%)), toccgroup$          , ch(06),~
                                                                         ~
               at (08,02), "Count Tolerance % for Class A",              ~
               at (08,33), fac(lfac$( 2%)), cnttol_a$           , ch(10),~
                                                                         ~
               at (09,02), "Count Tolerance % for Class B",              ~
               at (09,33), fac(lfac$( 3%)), cnttol_b$           , ch(10),~
                                                                         ~
               at (10,02), "Count Tolerance % for Class C",              ~
               at (10,33), fac(lfac$( 4%)), cnttol_c$           , ch(10),~
                                                                         ~
               at (11,02), "Count Tolerance % for Class D",              ~
               at (11,33), fac(lfac$( 5%)), cnttol_d$           , ch(10),~
                                                                         ~
               at (13,02), "File to Set ABC Class in",                   ~
               at (13,33), fac(lfac$( 6%)), savefile$           , ch(01),~
               at (13,37), "C = Cycle Count Master File",                ~
               at (14,37), "B = Both Parts Master File and",             ~
               at (15,37), "     the Cycle Count Master File",           ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(84)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L42950
                  call "MANUAL" ("HNYABC  ") : goto L42570

L42950:        if keyhit% <> 15% then L42980
                  call "PRNTSCRN" : goto L42570

L42980:        close ws
               call "SCREEN" addr ("C", u3%, "I", i3$(), cursor%())
               return

        set_pf3
        if edit% = 2% then L43170     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                 (4)Previous Field      " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                 (8)Select Ranges       " &       ~
                     "                                       "
            pfkeys$ = hex(01ffff04ffffff08ffffffff0dff0fff00)
                if fmccgroup$ = "ALL" then  return
                str(pf$(3%),18%,26%) = " " : str(pfkeys$, 8%,1%) = hex(ff)
            if fieldnr% > 2% then L43150
                str(pf$(2%),18%,26%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L43150:     return

L43170: if fieldnr% > 0% then L43260  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                 (4)Previous Screen     " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)ABC Parms   "
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            return
L43260:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1 A                 *~
            *-----------------------------------------------------------*~
            * Selection for Transation Types.                           *~
            *************************************************************

        select_trans
              gosub set_pf1a
              /* FACs already set from regular screen */
              sign_switch$ = "(Sign Switched)"
              str(line2$,,61%) = "List of Transaction Types for Past Data~
        ~ Calculations"
              inpmessage$ =  "Select Transaction Types by placing or remo~
        ~ving 'X's."

L44120:     accept                                                       ~
               at (01,02),                                               ~
                  "Managing ABC Classifications for Cycle Counting",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Transaction Types:",                         ~
                                                                         ~
               at (04,22), fac(lfac$( 4%)), past_what$(1%)      , ch(01),~
               at (04,24), "(IA) = Inventory Additions",                 ~
               at (05,22), fac(lfac$( 4%)), past_what$(2%)      , ch(01),~
               at (05,24), "(IC) = Cycle Count Adjustments",             ~
               at (06,22), fac(lfac$( 4%)), past_what$(3%)      , ch(01),~
               at (06,24), "(IP) = Physical Inventory Adjustments",      ~
               at (07,22), fac(lfac$( 4%)), past_what$(4%)      , ch(01),~
               at (07,24), "(IQ) = Inventory to Projects",               ~
               at (07,53), fac(hex(84)), sign_switch$           , ch(15),~
               at (08,22), fac(lfac$( 4%)), past_what$(5%)      , ch(01),~
               at (08,24), "(IS) = Inter-Store Movements",               ~
               at (09,22), fac(lfac$( 4%)), past_what$(6%)      , ch(01),~
               at (09,24), "(IW) = Inventory Withdrawals",               ~
               at (09,53), fac(hex(84)), sign_switch$           , ch(15),~
               at (10,22), fac(lfac$( 4%)), past_what$(7%)      , ch(01),~
               at (10,24), "(JB) = Job By-Products",                     ~
               at (11,22), fac(lfac$( 4%)), past_what$(8%)      , ch(01),~
               at (11,24), "(JC) = Job Completions",                     ~
               at (12,22), fac(lfac$( 4%)), past_what$(9%)      , ch(01),~
               at (12,24), "(JK) = Inventory to Jobs",                   ~
               at (12,49), fac(hex(84)), sign_switch$           , ch(15),~
               at (13,22), fac(lfac$( 4%)), past_what$(10%)     , ch(01),~
               at (13,24), "(JR) = Movement to/from Rework Jobs",        ~
               at (14,22), fac(lfac$( 4%)), past_what$(11%)     , ch(01),~
               at (14,24), "(JT) = Job Scrap",                           ~
               at (15,22), fac(lfac$( 4%)), past_what$(12%)     , ch(01),~
               at (15,24), "(PO) = Purchase Order Receipts",             ~
               at (16,22), fac(lfac$( 4%)), past_what$(13%)     , ch(01),~
               at (16,24), "(RT) = Order Shipments",                     ~
               at (16,47), fac(hex(84)), sign_switch$           , ch(15),~
               at (17,22), fac(lfac$( 4%)), past_what$(14%)     , ch(01),~
               at (17,24), "(VT) = A/P Receipts",                        ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(84)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               if keyhit% <> 13% then L44600
                  call "MANUAL" ("HNYABC  ") : goto L44120

L44600:        if keyhit% <> 15% then L44630
                  call "PRNTSCRN" : goto L44120

L44630:        return

        set_pf1a
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Return      "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* Unit cost/extension    */~
                              L50200,         /* Gross $/ Count         */~
                              L50300,         /* What Period            */~
                              L50360,         /* Type of Past Data      */~
                              L50400,         /* Date Range             */~
                              L50800          /* ABC Percents           */
            return

L50100: REM Test for Unit cost/ Extension
            type% = pos("UE" = type$)
            if type% > 0% then return
               errormsg$ = "Enter 'U' or 'E'."
               return

L50200: REM Test for Gross $/ Count
            rank% = pos("IG" = rank$)
            if rank% > 0% then return
               errormsg$ = "Enter 'I' or 'G'."
               return

L50300: REM Test for What Period
            when% = pos("FCP" = when$)
            if when% = 0% then goto L50330
                on when% goto L50322, L50323, L50324
L50322:         colhdr$ = "FUTURE USE" : return
L50323:         colhdr$ = "   ON HAND" : return
L50324:         colhdr$ = "  PAST QTY" : return
L50330:     errormsg$ = "Enter 'F', 'C', or 'P'."
            return

L50360: REM Test for Type of Past Data            PAST_WHAT$()
            if str(past_what$()) <> " " then return
                errormsg$ = "You must select at least one Transaction "& ~
                            "Type."
                return

L50400: REM Test for Date Range                   BEGIN$(1)
            if when% = 2% then return

               call "DATEOKC" (begin$(when%), begin%, errormsg$)
                  if errormsg$ <> " " then return
               call "DATEOKC" (end$  (when%), end%,   errormsg$)
                  if errormsg$ <> " " then return
               if end% >= begin% then L50520
                  errormsg$ = "Beginning Date Must Be On or Before Ending~
        ~ Date"
                  return

L50520:        temp1$ = begin$(when%)
               temp2$ = end$  (when%)
               call "DATUFMTC" (temp1$)
               call "DATUFMTC" (temp2$)

               if when% = 3% then L50690

               if temp1$ >= today$       then L50620
                  errormsg$ = "Beginning Date Must Be On or After Today"
                  return
L50620:        if temp2$ <= testdate$(2%) then return
                  errormsg$ = "Ending Date Must Be On or Before: "
                  temp1$ = testdate$(2%)
                  call "DATFMTC" (temp1$)
                  errormsg$ = errormsg$ & temp1$
                  return

L50690:        if temp1$ >= testdate$(1%) then L50750
                  errormsg$ = "Beginning Date Must Be On or After: "
                  temp1$ = testdate$(1%)
                  call "DATFMTC" (temp1$)
                  errormsg$ = errormsg$ & temp1$
                  return
L50750:        if temp2$ < today$ then look_for_archives
                  errormsg$ = "Ending Date Must Be Prior to Today"
                  return

L50800: REM Test for ABC Percents
            convert pct$(1%) to pct%(1%), data goto L50830
            convert pct%(1%) to pct$(1%), pic(##)
            if pct%(1%) >= 0 and pct%(1%) <= 100 then L50850
L50830:        errormsg$ = "Invalid Entry for 'A' Percent."
               return
L50850:     convert pct$(2%) to pct%(2%), data goto L50870
            convert pct%(2%) to pct$(2%), pic(##)
            if pct%(2%) >= 0 and pct%(2%) <= 100 - pct%(1%) then L50890
L50870:        errormsg$ = "Invalid Entry for 'B' Percent."
               return
L50890:     pct%(3%) = 100% - (pct%(1%) + pct%(2%))
            convert pct%(3%) to pct$(3%), pic(##)
            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51100,         /* Selection              */~
                              L51160,         /* Max Value for C's      */~
                              L51220          /* Max Value for D's      */
            return
L51100: REM Test for Selection                    CLASSIFY_MODE$
            p% = pos("AC" = classify_mode$)
            if p% <> 0% then return
                errormsg$ = "Select 'A' or 'C' for Assignment Method"
            return

L51160: REM Test for Maximum Value for C Parts    THRESHOLD_C$
            convert threshold_c$ to threshold_c  , data goto number_error
            if threshold_c >= 0 then return
                errormsg$ = "Value must be positive"
            return

L51220: REM Test for Maximum Value for D Parts    THRESHOLD_D$
            convert threshold_d$ to threshold_d , data goto number_error
            if threshold_d < threshold_c  then return
               errormsg$ = "'D' Threshold Must Be Less Than 'C' Threshold"


            return

        number_error
            errormsg$ = "Non-Numeric Entry"
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3.                      *~
            *************************************************************

        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L52150,         /* Cycle Count Group      */~
                              L52430,         /* Cnt Tol. Class A       */~
                              L52490,         /* Cnt Tol. Class B       */~
                              L52530,         /* Cnt Tol. Class C       */~
                              L52570,         /* Cnt Tol. Class D       */~
                              L52800          /* Save ABC Class in      */
            return
L52150: REM Test for Cycle Count Group Name       FMCCGROUP$
            if fmccgroup$ = " " and toccgroup$ = " " then doto% = 1%     ~
                                                     else doto% = 0%
            if fmccgroup$ = "ALL" then L52260
            if fmccgroup$ = "FIRST" then L52270
                tempval$ = hex(0684) & "Select FROM Cycle Count Group"
                call "GETCODE" (#25, fmccgroup$, tempval$, 0%, 0, f1%(5%))
                if fmccgroup$ <> "?" then L52250
L52230:         errormsg$ = "'?' is Not a Valid Range" :  return

L52250:     if toccgroup$ <> " "  or doto% = 1% then  L52270
L52260:         toccgroup$ = fmccgroup$     :   goto  L52320
L52270:     if toccgroup$ = "LAST" then  L52320
                tempval$ = hex(0684) & "Select TO Cycle Count Group"
                call "GETCODE" (#25, toccgroup$, tempval$, 0%, 0, f1%(5%))
                if toccgroup$ <> "?" then L52320 else L52230

L52320:     call "TESTRNGE"                                              ~
                  (fmccgroup$          , toccgroup$          ,           ~
                   loccgroup$          , hiccgroup$          ,           ~
                   errormsg$)

            if fmccgroup$ = "FIRST" then  loccgroup$ =  all(hex(20))
            if fmccgroup$ = "ALL" or toccgroup$ = "LAST" then            ~
                                                hiccgroup$ = all(hex(ff))

            return

L52430: REM Test for Count Tolerance for Class A  CNTTOL_A$
            convert cnttol_a$ to cnttol_a , data goto number_error
            if cnttol_a <= 100  then return
L52460:         errormsg$ = "Can Not Be Greater Than 100 Percent"
            return

L52490: REM Test for Count Tolerance for Class B  CNTTOL_B$
            convert cnttol_b$ to cnttol_b , data goto number_error
            if cnttol_b <= 100  then return  else L52460 /* Error Msg */

L52530: REM Test for Count Tolerance for Class C  CNTTOL_C$
            convert cnttol_c$ to cnttol_c , data goto number_error
            if cnttol_c <= 100  then return  else L52460 /* Error Msg */

L52570: REM Test for Count Tolerance for Class D  CNTTOL_D$
            convert cnttol_d$ to cnttol_d , data goto number_error
            if cnttol_d <= 100  then return  else L52460 /* Error Msg */


L52800: REM Test for Save ABC Class in            SAVEFILE$
            p% = pos("BC" = savefile$ )
            if p% <> 0% then return
                errormsg$ = "Select 'C' or 'B' to Set File Method"
            return

        REM *************************************************************~
            *             M I S C   R O U T I N E S                     *~
            *************************************************************

        look_for_archives
            call "SHOSTAT" ("Looking for Archived files...")
            call "FILEBGON" (#53)
            call "WORKOPEN" (#53, "IO", a_size%, f2%(53%))
            plowkey$ = "HNYDETAL" : nmbr_archives% = 0%
L60090:     call "PLOWNEXT" (#10, plowkey$, 8%, f1%(10%))
                if f1%(10%) = 0% then return

            get #10 using L60140, archname$, arch_beg$, arch_end$,        ~
                                 arch_recs%
L60140:          FMT POS(9), CH(8), POS(31), 2*CH(6), POS(46), BI(4)
            call "READ100" (#53, archname$, f1%(53%))
                if f1%(53%) = 1% then L60090  /* Already got this one */
            if arch_recs% = 0% then L60090    /* No Records */
            if (temp1$ >= arch_beg$ and temp1$ <= arch_end$) or          ~
               (temp2$ >= arch_beg$ and temp2$ <= arch_end$)             ~
                then got_an_archive
            if (arch_beg$ >= temp1$ and arch_beg$ <= temp2$) or          ~
               (arch_end$ >= temp1$ and arch_end$ <= temp2$)             ~
                then got_an_archive
            goto L60090 /* Archive file not in range of dates */
        got_an_archive
            call "PUTPRNAM" addr(#09, archname$)
            call "OPENFILE"(#09, "VALID", f2%(9%), rslt$(9%), " ")
                if f2%(9%) = 0% then write_archive_file_name

L60300:     u3% = 2%
            call "ASKUSER" (u3%, "*** ARCHIVED FILE MISSING ***",        ~
                "Archived file " & archname$ & " is not on disk!",       ~
             "Press PF1 to ABORT Processing  -or-  PF2 to Reenter Dates",~
                "-or-  PF16 to Continue Without File.")
            if u3% = 1% then startover_2
            if u3% <>  2% then L60360
                errormsg$ = "Please Re-Enter Date Range."
                return
L60360:     if u3% <> 16% then L60300

        write_archive_file_name
            write #53, archname$, eod goto L60090
            nmbr_archives% = nmbr_archives% + 1%
            goto L60090

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
            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" (#11)
            call "FILEBGON" (#12)
            call "FILEBGON" (#13)
            call "FILEBGON" (#51)
            call "FILEBGON" (#52)
            call "FILEBGON" (#53)
            end
