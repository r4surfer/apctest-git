        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y  L       CCC   BBBB   RRRR   PPPP    *~
            *  H   H  NN  N  Y   Y  L      C   C  B   B  R   R  P   P   *~
            *  HHHHH  N N N   YYY   L      C      BBBB   RRRR   PPPP    *~
            *  H   H  N  NN    Y    L      C   C  B   B  R   R  P       *~
            *  H   H  N   N    Y    LLLLL   CCC   BBBB   R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYLCBRP - This report will print the contents of the     *~
            *            HNYLOCNS file in one of 5 sort orders and      *~
            *            attempt to compare the quantities on hand at   *~
            *            the location with the quantities on hand       *~
            *            according to the accounting system (HNYQUAN).  *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/02/90 ! Original                                 ! JEF *~
            * 09/24/91 ! Re-Designed (Jeff K./Sid W.) Thanks Jeff ! SID *~
            * 11/07/91 ! PRR 12158  Open a WorkFile via WORKOPEN  ! SID *~
            *          !     rather than OPENCHCK.                !     *~
            * 01/08/92 ! Re-Designed processing to use two new    ! JBK *~
            *          ! workfiles rather then arrays for data to !     *~
            *          ! avoid array overflow.                    !     *~
            * 11/17/92 ! PRR 12435 - Open Work files to 20% of    ! RJH *~
            *          !  Location File.                          !     *~
            * 01/12/93 ! Page 0 Facs fix                          ! RJH *~
            * 06/21/93 ! PRR 12864.  Create workfile for more Recs! JDH *~
            * 06/25/93 ! Added part & store TESTRNGE channels.    ! JDH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            col_head1$132,               /* Report Column Heading 1    */~
            col_head2$132,               /* Report Column Heading 2    */~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            curr_part$25,                /* Current part in tables     */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            findkey$99,                  /* Miscellaneous Read/Plow Key*/~
            fm_temp_store$3,             /* Temp Store                 */~
            fmlot$16,                    /* Lot Number                 */~
            fmpart$25,                   /* Part Number                */~
            fmstore$3,                   /* Store Number               */~
            hilot$16,                    /* Lot Number                 */~
            hipart$25,                   /* Part Number                */~
            histore$3,                   /* Store Number               */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            l_table$42,                  /* Store Location keys        */~
            lfac$(5)1,                   /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lockey$99,                   /* Plowkey for Location file  */~
            lolot$16,                    /* Lot Number                 */~
            lopart$25,                   /* Part Number                */~
            lostore$3,                   /* Store Number               */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            prev$42,                     /*                            */~
            printopt$1,                  /* Print Option               */~
            println$132,                 /*                            */~
            quankey$99,                  /* Plowkey to HNYQUAN         */~
            q_table$44,                  /* Storage for HNYQUAN Keys   */~
            rpttitle$60,                 /* Report Title               */~
            sortseq$1,                   /* Sorting Seq                */~
            to_temp_store$3,             /* Temp Store                 */~
            time$8,                      /* System Time                */~
            tolot$16,                    /* Lot Number                 */~
            topart$25,                   /* Part Number                */~
            tostore$3,                   /* Store Number               */~
            userid$3,                    /* Current User Id            */~
            workkey$43                   /* Variable to Write to #04   */~

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
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
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
            * #01 ! HNYLOCNS ! Stock location master file               *~
            * #02 ! STORNAME ! Store Master File                        *~
            * #03 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * #04 ! WORKFILE ! Temporary Work File.                     *~
            * #05 ! HNYMASTR ! Inventory Master File                    *~
            * #06 ! WORKQUAN ! Temporary Work File for HNYQUAN File     *~
            * #07 ! WORKLOCN ! Temporary Work File for HNYLOCNS File    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYLOCNS",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  4, keypos =  590, keylen =  42,         ~
                            key  3, keypos =  527, keylen =  42,         ~
                            key  2, keypos =  485, keylen =  42,         ~
                            key  1, keypos =  443, keylen =  42          ~

            select #02, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos = 1, keylen = 3

            select #03, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            select #04, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =   71,              ~
                        keypos =    1, keylen =   43

            select #05, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos = 1, keylen = 25,                         ~
                        alternate key 1, keypos = 102, keylen =  9, dup, ~
                                  key 2, keypos =  90, keylen =  4, dup, ~
                                  key 3, keypos =  26, keylen = 32, dup

            select #06, "WORKQUAN",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =    1, keylen =   44

            select #07, "WORKLOCN",                                      ~
                        varc,     indexed,  recsize =  54,               ~
                        keypos =    1, keylen =   42

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(1%), f2%(1%), 0%, rslt$(1%))
               record% = val(str(rslt$(1%),17%,4%),4%) / 3
               record% = max(100%, record%)
            call "OPENCHCK" (#02, fs%(2%), f2%(2%), 0%, rslt$(2%))
            call "OPENCHCK" (#03, fs%(3%), f2%(3%), 0%, rslt$(3%))
            call "WORKOPEN" (#04, "IO   ", record%, f2%(4%))/* WorkFile */
            call "OPENCHCK" (#05, fs%(5%), f2%(5%), 0%, rslt$(5%))
            call "WORKOPEN" (#06, "IO   ", record%, f2%(6%))/* WorkQuan */
            call "WORKOPEN" (#07, "IO   ", record%, f2%(7%))/* WorkLocn */

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, 0%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "Warehouse/Location Accounting Balance Report"
            call "STRING" addr("CT", company$,  60%, company$)
            call "STRING" addr("CT", rpttitle$, 60%, rpttitle$)
            str(columnttl$, 1) = "Beginning Code"
            str(columnttl$,27) = "Ending Code"

            str(line2$,62) = "HNYLCBRP: " & str(cms2v$,,8)


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  5%
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
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% = 4% then editpg1
            if fieldnr% = 5% then fieldnr% = 4%
            if fieldnr% = 11% then fieldnr% = 5%
            if fieldnr% < 1% or fieldnr% > 5% then editpg1
            if fieldnr% = lastfieldnr% then editpg1
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
        REM GOSUB SETUP_COLUM_HEADING
            call "SHOSTAT" ("Now Generating Report....")
            goto generate_report

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Part Number            */~
                              L20200,         /* Store Number           */~
                              L20300,         /* Lot Number             */~
                              L20500,         /* Sorting Seq            */~
                              L20600          /* Print Option           */
            return
L20100: REM Def/Enable Part Number                 FMPART$
            if fmpart$             = " " then                            ~
               fmpart$             = "ALL"
            return

L20200: REM Def/Enable Store Number                FMSTORE$
            if fmstore$            = " " then                            ~
               fmstore$            = "ALL"
            return

L20300: REM Def/Enable Lot Number                  FMLOT$
            if fmlot$              = " " then                            ~
               fmlot$              = "ALL"
            return

L20500: REM Def/Enable Sorting Seq                 FMSORTSEQ$
            if sortseq$            = " " then                            ~
               sortseq$            = "1"
            return

L20600: REM Def/Enable Print Option                FMPRNTOPT$
            if printopt$           = " " then                            ~
               printopt$           = "A"
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
         "Enter Part Number Range, 'ALL' or '?'.                       ",~
         "Enter Store Number Range, 'ALL' or '?'.                      ",~
         "Enter Lot Range or 'ALL'.                                    ",~
         "Enter Sorting Sequence: '1', '2', '3', '4', or '5'.          ",~
         "Enter Printing Option: 'A', 'B', or 'C'.                     "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      fmlot$, fmpart$, printopt$,                        ~
                      sortseq$, fmstore$, hilot$,                        ~
                      hipart$, histore$, lolot$, lopart$,                ~
                      lostore$, tolot$, topart$, tostore$,               ~
                      plowkey$, quankey$, lockey$, curr_part$,           ~
                      fm_temp_store$, to_temp_store$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
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
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("HNY040", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            convert sortseq$ to sortseq%
            gosub set_col_heading
            plowkey$ = str(lopart$) & hex(00)

        read_hnymastr
*        Read HNYMASTR for a Part Number to Check for the Balance !!!

            call "PLOWNEXT" (#05, plowkey$, 0%, f1%(05))
                if f1%(05) = 0% then let_her_rip

            if str(plowkey$, 1, 25) > str(hipart$) then let_her_rip

            quankey$, lockey$  = str(plowkey$, 1, 25) & hex(00)

*        Process HNYQUAN Records

            q_cnt%, l_cnt% = 0

*        Read HNYQUAN and Place Data in WORKQUAN
        read_quan
            call "PLOWALTS" (#3, quankey$, 0%, 25%, f1%(3))
              if f1%(3) = 0 then read_loc

            if str(quankey$, 26,  3) <  fm_temp_store$                   ~
               or str(quankey$, 26,  3) >  to_temp_store$ then read_quan
            if str(quankey$, 29,  6) <  lolot$                           ~
               or str(quankey$, 29,  6) >  hilot$ then read_quan

            get #3 using L30630, oh_quan
L30630:         FMT POS(69), PD(14,4)

            q_cnt% = q_cnt% + 1%
            write #6, using L30670, quankey$, oh_quan, 0, 0%
L30670:         FMT CH(44), 2*PD(14,4), BI(4)
            goto read_quan

*        Read HNYLOCNS and Place Data in Tables

        read_loc
            call "PLOWALTS" (#1, lockey$, 3%, 25%, f1%(1))
            if f1%(1) = 0 then read_workquan

*        Check For User Input Ranges
            if str(lockey$, 26,  3) <  fm_temp_store$                    ~
               or str(lockey$, 26,  3) >  to_temp_store$ then read_loc
            if str(lockey$, 37,  6) <  lolot$                            ~
               or str(lockey$, 37,  6) >  hilot$ then read_loc

            get #1 using L30870, oh_loc
L30870:         FMT POS(573), PD(14,4)

            l_cnt% = l_cnt% + 1%
            pointer1% = 0%
            findkey$ = str(lockey$,1,28) & str(lockey$,37,6)

            call "READ101" (#6, findkey$, f1%(6))
                if f1%(6) = 0% then write_worklocn

            get #6, using L31060, oh_lotloc, q_access%
L31060:         FMT POS(53), PD(14,4), BI(4)
                     oh_lotloc = oh_lotloc + oh_loc
                     q_access% = q_access% + 1%
                     pointer1% = 1%
            put #6, using L31060, oh_lotloc, q_access%

            rewrite #6

        write_worklocn

            write #7, using L31170, lockey$, oh_loc, pointer1%
L31170:         FMT CH(42), PD(14,4), BI(4)

            goto read_loc

*        Search the WORKQUAN records with no location references
        read_workquan

            if printopt$ = "B" then  L32000      /* Skip if in bal only */
            if q_cnt% = 0% then L32000           /* No HNYQUAN data     */

            init (hex(00)) quankey$

L31270:     call "PLOWNEXT" (#6, quankey$, 0%, f1%(6))
                if f1%(6) = 0% then read_worklocn

                get #6, using L31310, q_table$, oh_quan, oh_lotloc,       ~
                                     q_access%
L31310:              FMT CH(44), 2*PD(14,4), BI(4)
                if q_access% <> 0% then L31270

*        Entry in WORKQUAN does not have any matching HNYLOCNS records
*        Check to see if it should be printed on the report

                if oh_quan = 0 then L31270       /* Exclude zero values */

*        OK, format the data for the workfile based on the sort sequence

                init (" ")  workkey$

                on sortseq% goto L31460, L31550, L31640, L31730, L31810

*        Store, Location, Part, lot
L31460:         put workkey$, using L31510, str(q_table$,26, 3),          ~
                                           " ",                          ~
                                           str(q_table$, 1,25),          ~
                                           str(q_table$,29, 6)

L31510:              FMT CH(03), CH(08), CH(25), CH(06)
                goto L31880

*        Store, part, lot, location
L31550:         put workkey$, using L31600, str(q_table$,26, 3),          ~
                                           str(q_table$, 1,25),          ~
                                           str(q_table$,29, 6),          ~
                                           " "

L31600:              FMT CH(03), CH(25), CH(06), CH(08)
                goto L31880

*        Store, part, location, lot
L31640:         put workkey$, using L31690, str(q_table$,26, 3),          ~
                                           str(q_table$, 1,25),          ~
                                           " ",                          ~
                                           str(q_table$,29, 6)

L31690:              FMT CH(03), CH(25), CH(08), CH(06)
                goto L31880

*        Part, store, location, lot
L31730:         put workkey$, using L31770, str(q_table$, 1,28),          ~
                                           " ",                          ~
                                           str(q_table$,29, 6)

L31770:              FMT CH(28), CH(08), CH(06)
                goto L31880

*        Store, lot, part, location
L31810:         put workkey$, using L31860, str(q_table$,26, 3),          ~
                                           str(q_table$,29, 6),          ~
                                           str(q_table$, 1,25),          ~
                                           " "

L31860:              FMT CH(03), CH(06), CH(25), CH(08)

L31880:         put workkey$, using L31890, "Q"
L31890:              FMT POS(43), CH(1)

                put #4, using L31930, workkey$, oh_quan, 0, oh_lotloc,    ~
                                     q_access%
L31930:              FMT CH(43), 3*PD(14,4), BI(4)

                write #4

            goto L31270

        REM Check WORKLOCN records and format for the workfile
L32000: read_worklocn

            if l_cnt% = 0% then clean_up_workfiles

                init (hex(00))  lockey$
L32050:         oh_quan, oh_loc, oh_lotloc, q_access%, pointer1% = 0

                call "PLOWNEXT" (#7, lockey$, 0%, f1%(7))
                     if f1%(7) = 0% then clean_up_workfiles

                get #7, using L32090, l_table$, oh_loc, pointer1%
L32090:              FMT CH(42), PD(14,4), BI(4)
                     if pointer1% = 0% then oh_lotloc = oh_loc
                     if pointer1% = 0% then L32150

                findkey$ = str(lockey$, 1,28) & str(lockey$,37, 6)
                call "READ100" (#6, findkey$, f1%(6))
                     if f1%(6) = 0 then L32150
                get #6, using L32135, oh_quan, oh_lotloc, q_access%
L32135:              FMT POS(45), 2*PD(14,4), BI(4)

L32150
*        Lets test the print options
                if printopt$ = "A" then L32260
                if printopt$ <> "B" then L32230

*        Option 'B', In balance only
                if oh_lotloc <> oh_quan then L32050
                goto L32260

L32230
*        Option 'C', Out of balance only
                if oh_lotloc = oh_quan then L32050

L32260
*        Found data, format WORKKEY and write WORKFILE
                init (" ") workkey$

                on sortseq% goto L32330, L32410, L32490, L32560, L32620

*        Store, Location, Part, Lot
L32330:         put workkey$, using L32370, str(l_table$,26, 3),          ~
                                           str(l_table$,29, 8),          ~
                                           str(l_table$, 1,25),          ~
                                           str(l_table$,37, 6)
L32370:              FMT CH(03), CH(08), CH(25), CH(06)
                goto L32690

*        Store, Part, Lot, Location
L32410:         put workkey$, using L32450, str(l_table$,26, 3),          ~
                                           str(l_table$, 1,25),          ~
                                           str(l_table$,37, 6),          ~
                                           str(l_table$,29, 8)
L32450:              FMT CH(03), CH(25), CH(06), CH(08)
                goto L32690

*        Store, Part, Location, Lot
L32490:         put workkey$, using L32530, str(l_table$,26, 3),          ~
                                           str(l_table$, 1,25),          ~
                                           str(l_table$,29, 8),          ~
                                           str(l_table$,37, 6)
L32530:              FMT CH(03), CH(25), CH(08), CH(06)
                goto L32690

L32560
*        Part, Store, Location, Lot
                put workkey$, using L32580, str(l_table$, 1,42)
L32580:              FMT CH(42)
                goto L32690

*        Store, Lot, Part, Location
L32620:         put workkey$, using L32660, str(l_table$,26, 3),          ~
                                           str(l_table$,37, 6),          ~
                                           str(l_table$, 1,25),          ~
                                           str(l_table$,29, 8)
L32660:              FMT CH(03), CH(06), CH(25), CH(08)

*        Put a 'L' to identify Location record
L32690:         put workkey$, using L32700, "L"
L32700:              FMT POS(43), CH(1)

                put #4, using L32740, workkey$, oh_quan, oh_loc,          ~
                                     oh_lotloc, q_access%
L32740:              FMT CH(43), 3*PD(14,4), BI(4)

                write #4

            goto L32050

*        Clean-up the workfiles and go back for next part number
        clean_up_workfiles
            init (hex(00)) quankey$, lockey$
            call "DELETE" (#6, quankey$, 0%)
            call "DELETE" (#7, lockey$,  0%)
            goto read_hnymastr

        let_her_rip
*        Set Up Break Keys
           on sortseq% goto L32870, L32885, L32900, L32915, L32930
L32870:        b1% =  3% : b2% =  8% : b3% = 25% : b4% =  6%
               b5% =  1% : b6% =  7% : b7% = 16% : b8% = 42%
                     goto L32945
L32885:        b1% =  3% : b2% = 25% : b3% =  6% : b4% =  8%
               b5% =  1% : b6% =  7% : b7% = 33% : b8% = 40%
                     goto L32945
L32900:        b1% =  3% : b2% = 25% : b3% =  8% : b4% =  6%
               b5% =  1% : b6% =  7% : b7% = 33% : b8% = 42%
                     goto L32945
L32915:        b1% = 25% : b2% =  3% : b3% =  8% : b4% =  6%
               b5% =  1% : b6% = 27% : b7% = 33% : b8% = 42%
                     goto L32945
L32930:        b1% =  3% : b2% =  6% : b3% = 25% : b4% =  8%
               b5% =  1% : b6% =  7% : b7% = 14% : b8% = 40%

L32945
*        Printing Starts Here !!!!!
            init(" ") workkey$, println$
            init(hex(00))  prev$
            oh_quan, oh_loc, oh_lotloc, q_access% = 0
            plowkey$ = hex(00)
L32980:     call "PLOWNXT1" (#04, plowkey$, 0%, f1%(04))
                if f1%(04) = 0% then end_report
                get #04 using L33020, workkey$, oh_quan, oh_loc,          ~
                                     oh_lotloc, q_access%
L33020:                       FMT    CH(43), 3*PD(14,4), BI(4)

            if lcntr% < 55% then L33100
               gosub page_head
               init(" ")  println$  :  init(hex(00)) prev$

L33100:     gosub'201 (1%            , b1%, b5%)
            gosub'201 (1% + b1%      , b2%, b6%)
            gosub'201 (1% + b1% + b2%, b3%, b7%)
            str(println$, b8%, b4%) =                                    ~
                str(workkey$,1+b1%+b2%+b3%,b4%)

           if q_access% = 0%                                             ~
             then str(println$,91) = "No Accounting Quantity Found."
           if q_access% = 1% and (oh_quan = oh_lotloc)                   ~
             then str(println$,91) = "Single Location, In Balance."
           if q_access% = 1% and (oh_quan <> oh_lotloc)                  ~
             then str(println$,91) = "Single Location, Out of Balance."
           if q_access% > 1% and (oh_quan = oh_lotloc)                   ~
             then str(println$,91) = "Multiple Locations, In Balance."
           if q_access% > 1% and (oh_quan <> oh_lotloc)                  ~
             then str(println$,91) = "Multiple Locations, Out of Balance."
           if str(workkey$,43,1) = "Q"                                   ~
             then str(println$,91) = "No Location Quantity Found."

*        Convert Quantity to the appropriate fields
            call "CONVERT" (oh_loc,    2.2, str(println$, 49, 12))
            call "CONVERT" (oh_lotloc, 2.2, str(println$, 62, 12))
            call "CONVERT" (oh_quan,   2.2, str(println$, 77, 12))

            print println$
            print skip(1)
            lcntr% = lcntr% + 2%

            prev$ = str(workkey$,1,42)
            delete #04
            init(" ") println$, workkey$
            goto L32980 /* Get Next Record */

        end_report                /* Report Ending Routine */
            time$ = " "  :  call "TIME" (time$)
            if pcntr% = -1% then gosub page_head
            print skip(2)
            print using L64990, time$      /* End of report line */
            close printer
            call "SETPRNT" ("HNY040", " ", 0%, 1%)
            goto inputmode

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            if pcntr% = 0% then gosub print_params
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "HNYLCBRP"
            print using L60110, rpttitle$, pcntr%
            print
            print col_head1$
            print col_head2$
            print using L64996
            lcntr% = 6%
            return

        print_params           /* Print Page Zero */
            print page
            print using L64980, rpttitle$
L34541:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L34545
                str(i$(), i%, 1%) = hex(20)
                goto L34541
L34545:     print using L60070, date$, time$, company$, "HNYLCBRP"
            print using L60110, rpttitle$, pcntr%
            print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 20% : print tab(25); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            pcntr% = pcntr% + 1%
            return

        deffn'201 (c0%, c1%, c2%)
          if str(prev$,    c0%, c1%) = str(workkey$, c0%, c1%) then return
             str(println$, c2%, c1%) = str(workkey$, c0%, c1%)
             init (hex(00))  str(prev$, c0% + c1%)
             return

        set_col_heading
            on sortseq% goto L34790, L34810, L34830, L34850, L34870

L34790:     str(col_head2$,1,) = "STORE LOCATION PART                  " ~
                              &  "    LOT"       : goto L35000
L34810:     str(col_head2$,1,) = "STORE PART                      LOT  " ~
                              &  "  LOCATION"    : goto L35000
L34830:     str(col_head2$,1,) = "STORE PART                      LOCAT" ~
                              &  "ION LOT"       : goto L35000
L34850:     str(col_head2$,1,) = "PART                      STORE LOCAT" ~
                              &  "ION LOT"       : goto L35000
L34870:     str(col_head2$,1,) = "STORE LOT    PART                    " ~
                              &  "  LOCATION"

L35000:     str(col_head1$,55,34) = "QTY AT     LOCATION PART/STORE/LOT"
            str(col_head2$,49,40) =                                      ~
               "    LOCATION        TOTAL ACCTG QUANTITY"
            str(col_head2$,91,8)  = "COMMENTS"
        return

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
              on fieldnr% gosub L40100,         /* Part Number       */   ~
                                L40100,         /* Store Number      */   ~
                                L40100,         /* Lot Number        */   ~
                                L40105,         /* Sorting Seq       */   ~
                                L40100          /* Print Option      */
              goto L40115

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40100:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40105:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40115:     accept                                                       ~
               at (01,02),                                               ~
                  "Report on Inventory vs Location Quantities",          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,30), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (06,02), "Part Range",                                 ~
               at (06,30), fac(lfac$( 1)), fmpart$              , ch(25),~
               at (06,56), fac(lfac$( 1)), topart$              , ch(25),~
                                                                         ~
               at (07,02), "Store Range",                                ~
               at (07,30), fac(lfac$( 2)), fmstore$             , ch(03),~
               at (07,56), fac(lfac$( 2)), tostore$             , ch(03),~
                                                                         ~
               at (08,02), "Lot Range",                                  ~
               at (08,30), fac(lfac$( 3)), fmlot$               , ch(06),~
               at (08,56), fac(lfac$( 3)), tolot$               , ch(06),~
                                                                         ~
               at (10,02), "Sorting Sequence",                           ~
               at (10,20), fac(lfac$( 4)), sortseq$             , ch(01),~
               at (10,25), "1. Store / Location / Part / Lot",           ~
               at (11,25), "2. Store / Part / Lot / Location",           ~
               at (12,25), "3. Store / Part / Location / Lot",           ~
               at (13,25), "4. Part / Store / Location / Lot",           ~
               at (14,25), "5. Store / Lot / Part / Location",           ~
                                                                         ~
               at (16,02), "Printing Option",                            ~
               at (16,20), fac(lfac$( 5)), printopt$            , ch(01),~
               at (16,25), "A. All",                                     ~
               at (17,25), "B. In Balance Only",                         ~
               at (18,25), "C. Out Of Balance Only",                     ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40340
                  call "MANUAL" ("HNYLCBRP") : goto L40115

L40340:        if keyhit% <> 15 then L40355
                  call "PRNTSCRN" : goto L40115

L40355:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40450     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40435
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L40440
L40435:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40440:     return

L40450: if fieldnr% > 0% then L40495  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40495:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,         /* Part Number            */~
                              L50200,         /* Store Number           */~
                              L50300,         /* Lot Number             */~
                              L50500,         /* Sorting Seq            */~
                              L50600          /* Print Option           */
            return
L50100: REM Test for Part Number                  FMPART$
            call "TESTRNGE"                                              ~
                  (fmpart$            , topart$              ,           ~
                   lopart$            , hipart$              ,           ~
                   errormsg$, #05)
            return

L50200: REM Test for Store Number                 FMSTORE$
            call "TESTRNGE"                                              ~
                  (fmstore$            , tostore$            ,           ~
                   lostore$            , histore$            ,           ~
                   errormsg$, #02)
            if errormsg$ <> " " then return
            if fmstore$ = "ALL" and tostore$ = " " then L50258
               fm_temp_store$ = fmstore$ : to_temp_store$ = tostore$
               return
L50258:        fm_temp_store$ = lostore$ : to_temp_store$ = histore$
               return

L50300: REM Test for Lot Number                   FMLOT$
            call "TESTRNGE"                                              ~
                  (fmlot$              , tolot$              ,           ~
                   lolot$              , hilot$              ,           ~
                   errormsg$)
            return

L50500: REM Test for Sorting Seq                  SORTSEQ$
          if pos("12345" = sortseq$) <> 0% then return
           errormsg$ = "Sorting Option Must be '1', '2', '3', '4' or '5'."
          return

L50600: REM Test for Print Option                 PRINTOPT$
          if pos("ABC" = printopt$) <> 0% then return
           errormsg$ = "Printing Option Must be 'A', 'B' or 'C'."
          return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ########   ########                 ########################~
        ~####################################                ########: HNY~
        ~040
*       * Header Line 2
L60110: %                                        ########################~
        ~####################################                    PAGE:   #~
        ~###
        %** Report Title for page 0
L64980: %############################################################

L64990: %                              * * * * * * * * * *   E N D   O F ~
        ~  R E P O R T   @  ########   * * * * * * * * * *

L64996: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~---

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
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")
            call "FILEBGON" (#4)
            call "FILEBGON" (#6)
            call "FILEBGON" (#7)
            end
