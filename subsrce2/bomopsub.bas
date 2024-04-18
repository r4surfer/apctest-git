        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  BBBB    OOO   M   M   OOO   PPPP    SSS   U   U  BBBB    *~
            *  B   B  O   O  MM MM  O   O  P   P  S      U   U  B   B   *~
            *  BBBB   O   O  M M M  O   O  PPPP    SSS   U   U  BBBB    *~
            *  B   B  O   O  M   M  O   O  P          S  U   U  B   B   *~
            *  BBBB    OOO   M   M   OOO   P       SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMOPSUB - Select options in Bill of Materials, review    *~
            *            options selected, reselect, etc.  Adjust cost  *~
            *            by reselecting.  Adjust selling price or       *~
            *            margin outright.                               *~
            *                                                           *~
            *             @@@@    @@@@@@@    @@@@    Danger!   This     *~
            *               @@   @@@@@@@@@   @@      routine may be     *~
            *                 @@ @   @   @ @@        hazardous to your  *~
            *                    @@@@@@@@@           sanity.  No less   *~
            *                     @@...@@            than four          *~
            *                  @@  @...@ @@          '7 loops - you     *~
            *                @@    @@@@@    @@       break it - you     *~
            *             @@@@               @@@@    own it.  Amen      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/20/93 ! Total rewrite to allow partial           ! WPH *~
            *          ! reselection of options.                  !     *~
            *          !  Note for all you historians, this       !     *~
            *          !  rewrite spelled the end of AUTO-REPLACE.!     *~
            * 06/04/96 ! PRR 13611.  Fix page handling for lists. ! JDH *~
            *          !   I fixed it, but I don't own it.        !     *~
            * 09/04/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

             sub "BOMOPSUB"                                              ~
                 (clearflg%,       /* CLEAR FOR INPUT FLAG 0=NO, 1=YES */~
                  assypart$,       /* PART TO BUILD                    */~
                  demand$,         /* DEMAND CODE & LINE               */~
                  effdatef$,       /* EFFECTIVE DATE                   */~
                  inbomid$,        /* Specific Parent Bom Id.          */~
                  cuscode$,        /* Customer to find prices for      */~
                  custype$,        /* Customer Type Code               */~
                  partcat$,        /* Part Category Code               */~
                  pcode$,          /* Price code passed in             */~
                  pricedate$,      /* Pricing Date (fmt/unfmt)         */~
                  dollars(),       /* Old/New Cost, Old/New Price      */~
                  #4,              /* 'HNYMASTR' INVENTORY MASTER FILE */~
                  #5,              /* 'BOMMASTR' BILL OF MATERIALS FILE*/~
                  #7,              /* 'BOMSPEC' STORES SELECTED OPTIONS*/~
                  #10,             /* 'SYSFILE2' SYSTEM CONTROL FILE   */~
                  #11,             /* 'ENGMASTR' ENGINEERING MASTER FIL*/~
                  #8,              /* 'BOMSPHDR' Header to BOMSPEC File*/~
                  returncode%)     /*  See legend below                */


        REM Return Codes Defined - 0 = All OK                            ~
                                   1 = Invalid Parent Passed In          ~
                                   2 = Parent passed in not generic      ~
                                   3 = Calendar or effect. date error    ~
                                   4 = User aborted without selections   ~

        REM Note 1 All pricing and costing of generic parts is now       ~
                   performed here in BOMOPSUB rather than in the callers.~
                   Callers can only manually edit.  Cost & price are     ~
                   determined here and passed back to the caller as      ~
                   dollars(2) and dollars(4) respectively.  If BOMOPSUB  ~
                   is called again later to review the config (clearflg% ~
                   = 0) then the price of the config (determined prev-   ~
                   iously) is passed back in as dollars(3) and the cost  ~
                   is recalculated (BCKxxxxx programs do not currently   ~
                   retain the cost (they should).  Subsequent edits      ~
                   result in recalculation/adjustment of cost & price.   ~
                                                                         ~
            Note 2 If PCODE$ is passed in as numeric, we adjust the std. ~
                   cost and calculate the selling price by applying a    ~
                   profit margin to the cost.  If PCODE$ is passed in as ~
                   alpha, we look up the base price and then add in the  ~
                   price of each top level replacement part selected to  ~
                   arrive at an adjusted price.  Cost is calculated the  ~
                   same way in each case by obtaining the standard cost, ~
                   subtracting out the cost for the option part in the   ~
                   bill (extended), and adding back in the cost of the   ~
                   replacement part(s) selected.  Non stocked parts and  ~
                   "don't use anything" both have zero cost.             ~
                                                                         ~
            Note 3 Since the user can manually adjust the selling price, ~
                   all this pricing work can get blown out of the water  ~
                   and un-auditable if the user tweeks the price.        ~
                   Therefore if the price does not equal what we came    ~
                   up with through our look-up and auto-adjustment, we   ~
                   show message on the screen that it was set 'manually'.~
                                                                         ~
            Note 4 The BOM version should also only be selectable here   ~
                   in BOMOPSUB - the user should not be allowed to edit  ~
                   it in the caller as this could put the BOM out of     ~
                   synch with the set of replacements selected here.

        dim                                                              ~
            adj$10,                      /* Price Adjustment Amount    */~
            alt$(100)1,                  /* ACCEPTS ALTERNATE SELECTION*/~
            altdesc$(100)32,             /* DESCRIPTION OF ALTERNATE PA*/~
            altpart$(100)25,             /* ALTERNATE PARTS            */~
            assy$25,                     /* SUB ASSEMBLY PART NUMBER   */~
            assypart$25,                 /* ASSEMBLY PART NUMBER       */~
            assypartkey$28,              /* ASSEMBLY PART NUMBER - BOM */~
            assypartdescr$34,            /* ASSEMBLY PART DESCRIPTION  */~
            base_price$8,                /* Base Selling Price         */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bom$(490)3,                  /* BOM LIST                   */~
            bomid$3,                     /* WHICH ALT BOM?             */~
            bommkr$2,                    /* BOM MARKER                 */~
            catcode$4,                   /* Component Category code    */~
            cprice$8,                    /* Component Price            */~
            cbomid$3,                    /* COMPONENTS SPECIFIC BOM    */~
            cdesc$34,                    /* Component Description      */~
            column%(30),                 /* CURSOR LOCATION FOR DISPLAY*/~
            colhead$(3)32,               /* Screen column headers      */~
            colheader$79,                /* Screen column header       */~
            confirmation$75,             /* Message that opts selected */~
            config_descr$60,             /* Description of saved config*/~
            component$25,                /* COMPONENT PART NUMBER      */~
            componentkey$28,             /* COMPONENT PART NUMBER - BOM*/~
            cost$10,                     /* Adjusted Standard Cost/unit*/~
            cursor%(2),                  /* CURSOR LOCATION FOR DISPLAY*/~
            currtype$1,                  /* Currency Type code         */~
            currency$4,                  /* Currency code              */~
            date$8,                      /* CALENDAR DATE FOR SHOW OFF */~
            demand$19,                   /* DEMAND CODE & LINE         */~
            descr$79,                    /* Descrip line for PLOWCODE  */~
            dflag$(100)1,                /* Default selection flag     */~
            descr_map(8),                /* Plowcode Arg               */~
            dollars(4),                  /* Oldstd, NewStd, OldPric,New*/~
            edtmessage$79,               /* Edit screen message        */~
            effdatef$8,                  /* EFFECTIVITY DATE FORMATTED */~
            effdate$8,                   /* EFFECTIVITY DATE           */~
            errormsg$79,                 /* ERROR MESSAGE TEXT INFO    */~
            err$79,                      /* Error text                 */~
            ext(30),                     /* QUANTITY EXTENTION ARRAY   */~
            ext$8,                       /* Component quantity         */~
            extprice$10,                 /* Component price extended   */~
            fixed$10,                    /* Bom Fixed quantity per run */~
            over$10,                     /* Bom Added overage quantity */~
            hdr$(3)79,                   /* Plowcode headers           */~
            headline$(3)79,              /* Screen Header Lines        */~
            header$79,                   /* Header passed to ASKSTRNG  */~
            hnymastr_record$(100)9,      /* 900 byte HNYMASTR record   */~
            i$(24)80,                    /* Screen Image               */~
            id$3,                        /* SUB ASSEMBLY BOM ID.       */~
            ignore$1,                    /* Tell planning to ignore    */~
            inbomid$3,                   /* ASSYPARTS SPECIFIC BOM     */~
            incl_excl$(1)1, incl_excl(1),/* Plowcode Arg               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line$(30)79,                 /* TEXT TO DISPLAY ON SCREEN  */~
            line2$79,                    /* Screen Line #2             */~
            list$(1000)79,               /* Screen List lines          */~
            list_head$79,                /* Column headers for list    */~
            m$(30)1,                     /* BOM MARKERS FOR 30 LEVELS  */~
            marker$2,                    /* BOM Marker                 */~
            margin$10,                   /* Profit margin per unit     */~
            message1$79,                 /* Text passed to ASKSTRNG    */~
            message2$79,                 /* Text passed to ASKSTRNG    */~
            old_demand$19,               /* SO/line of old config      */~
            old_date$8,                  /* Date of old config order   */~
            op$1,                        /* OPTION FLAG IN BOMMASTR    */~
            optn$1,                      /* Option flag                */~
            opart$25,                    /* Option Parts for Review    */~
            origpart$25,                 /* Original part              */~
            p$(30)31,                    /* READ KEYS FOR 30 LEVELS    */~
            parent$28,                   /* Parent Part and BOM id     */~
            pf19_prompt$32,              /* Prompt for Price audit list*/~
            part$25,                     /* DUMMY ARGUMENT PART #      */~
            partkey$28,                  /* DUMMY ARGUMENT PART # - BOM*/~
            passy$25,                    /* SUB ASSEMBLY PART FOR PRINT*/~
            pseq$3,                      /* Seq. number in parents BOM */~
            pseq2$3,                     /* Seq. number in parents BOM */~
            pbom$3,                      /* ALT BOM FOR PRINT          */~
            pcomp$25,                    /* COMPONENT FOR PRINT        */~
            pdesc$34,                    /* Descript for current P$()  */~
            percent$6,                   /* Margin as a percent        */~
            pdescr$(2)32,                /* DESCRIPTION OF PARTS FOR PR*/~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* ALL PURPOSE READ KEY       */~
            prepl$25,                    /* COMPONENT REPLACEMENT FOR P*/~
            price$10,                    /* Selling Price per unit     */~
            price_msg$20,                /* Tells how price determined */~
            qflag$(100)1,                /* Prompt for qty?  Y/N flag  */~
            qty(4),                      /* Component Quantities       */~
            quan$10,                     /* Component Quantity         */~
            readkey$99,                  /* KEY FOR PLOW ROUTINES      */~
            readkey2$99,                 /* KEY FOR PLOW ROUTINES      */~
            record$255,                  /* Record read from file      */~
            ref_key$31,                  /* SELECTED OPTION            */~
            ref_descr$34,                /* Descrip of selected opton  */~
            rpart$25,                    /* Repl.  Parts for Review    */~
            rdesc$32,                    /* Repl.  Part Descriptions   */~
            rep_part$25,                 /* SELECTED OPTION            */~
            savekey$99,                  /* Save plowkey for later use */~
            scrn$(15)79,                 /* Screen Lines for Explosion */~
            selected_part$25,            /* Part selected for re-select*/~
            save_seq$3,                  /* Hold a seqeuence #         */~
            seq$3,                       /* SUB ASSEMBLY SEQUENCE NUMBE*/~
            sq$(30)3,                    /* Seq. number in parents BOM */~
            size$10,                     /* Component Size (times used)*/~
            s_type$3,                    /* Selected Part's part type  */~
            temp$79,                     /* Temporary Variable         */~
            toggle$23,                   /* Toggle key prompt          */~
            total$10,                    /* Total of something         */~
            type$3,                      /* Part Type                  */~
            uom$4,                       /* Unit of Measure            */~
            uomdescr$34,                 /* Unit of Measure Description*/~
            userid$3,                    /* Current User Id            */~
            what$20                      /* What is ASKSTRNG obtaining */

        dim sum(12),                     /* Folded costs  from STCCOST */~
            bom(12),                     /* BOM costs     from STCCOST */~
            rte(12),                     /* Rout costs    from STCCOST */~
            dtl(12)                      /* Detail costs  from STCCOST */~

        REM If you need to explode bigger BOMs, just boost the size of   ~
            the following arrays.  However this thing will get to be a   ~
            segment 2 hog if you go much farther.  No other changes need ~
            be made as there is logic to sense the dimension of the array

            dim                                                          ~
            active$(1000)1,              /* Tracks free slots in PLIST$*/~
            plist$(2,1000)32,            /* List of parts & descrips   */~
            indent%(1000),               /* Indent level for parts     */~
            marker$(1000)2,              /* BOM marker                 */~
            optn$(1000)1,                /* Component is option flag   */~
            ebom$(1000)3,                /* Effective BOM for parts    */~
            plus$(1000)1,                /* Indicates levels below     */~
            seq$(1000)3,                 /* Component sequence number  */~
            tptr%(1000),                 /* Temp pointers to new parts */~
            qty$(1000)10,                /* Extended comp. qty/parent  */~
            type$(1000)3,                /* Part type for options flag */~
            rbom$(1000)3,                /* Replacement parts BOM      */~
            rlist$(2,1000)32,            /* List of replacement parts  */~
            rtype$(1000)3,               /* Part type for replacement  */~
            idx%(1000),                  /* Position index             */~
            rqty$(1000)10                /* Ext. comp repl qty/parent  */

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
            * #1  ! HNYOPTNS ! BOM Options File                         *~
            * #6  ! WORK2    ! Workfile for net change inserts          *~
            * #9  ! GENCODES ! General Codes file                       *~
            * #64 ! DUMMY    ! Dummy File for PLOWCODE arg              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "HNYOPTNS",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 54

            select #6, "WORK2",                                          ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 57, keylen = 94

            select #9, "GENCODES",                                       ~
                        varc, indexed,                                   ~
                        recsize = 128,                                   ~
                        keypos = 1, keylen = 24

            select #64, "DUMMY",                                         ~
                        varc, indexed,                                   ~
                        recsize = 4,  keypos = 1, keylen = 2

            get #4, str(hnymastr_record$()) /* Save in-coming HNYMASTR */
            if fs%(1%) <> 0% then L02350
            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 0%, rslt$(1%))
L02350:     if fs%(9%) <> 0% then L09000
            call "OPENCHCK" (#9, fs%(9%), f2%(9%), 0%, rslt$(9%))


L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Do setup chores, validation, and initialize variables.    *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

*        Normal Set-up Stuff
            call "EXTRACT" addr("ID", userid$)
            date$ = date

            call "PIPINDEX" (#10, date$, today%, idxerr%)
                if idxerr% = 0% then L09060
                returncode% = 3%
                goto exit_program

L09060:     call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            confirmation$ = "All Replacements for Options in the Bill o"&~
                            "f Materials Have Been Selected."

            str(line2$,62) = "BOMOPSUB: " & str(cms2v$,,8)

*        Special Setup for Option Selection
            show_all% = 1%  /* 1 = don't show all, 2 = do show all */
            restart% = 0%
            force_manual% = 0%  /* 0 = honor default selections */
            ignore$, err$ = " "
            returncode% = 0%   /* All OK until we say different */
            colheader$   = "   1 2 3 4 5 6 7 8 9 10                    "&~
                           "                       Quantity Mk O"

            d% = dim(plist$(),2) /* D% = array size; used in Explosion*/

*        Check Validity of Parent Part number
           call "DESCRIBE" (#4, assypart$, assypartdescr$, 0%, f1%(4))
               if f1%(4) <> 0%  then L09160
               returncode% = 1%     /* Can't read HNYMASTR for this part*/
               goto  exit_program

*        Get Parent Part Type And STD Cost. Note- #4 Already Read Above
L09160:     get #4, using L09165, type$
L09165:     FMT POS(180), CH(3)
            if type$ = "000" then L09185
                returncode% = 2%       /* Non-Generics can't have optns*/
                goto exit_program
L09185:     call "STCCOSTS" (assypart$, " ", #10, 1%, stccost)

*        Quick test to see if part has ANY bill of materials on file
            readkey$ = assypart$
            call "PLOWNEXT" (#5, readkey$, 25%, f1%(5))
                if f1%(5) <> 0% then L09270
                returncode% = 1%     /* Wooden Nickel -No Bill on File */
                goto exit_program

L09270
*        Convert date passed in to a PIP subscript
            call "DATEOK" (effdatef$, i%, errormsg$)
            if errormsg$ = " " then L09320
               returncode% = 3%     /* Fatal Calendar Problem */
               goto exit_program

        REM We're going to assume that the caller caught any serious     ~
            planning calendar problem, and that if the date passed in    ~
            is beyond the calendar, we will use the BOM version effective~
            on the last date of the calendar.

L09320:     effdate$ = effdatef$
            call "DATUNFMT" (effdate$)
            call "PIPINDEX" (#10, effdate$, dateindex%, idxerr%)
            if idxerr% = 0% then L09400
               dateindex% = 490%  /* last day of calendar */

L09400: REM *************************************************************~
            *  L O A D   P R E V I O U S   C O N F I G U R A T I O N    *~
            *   If we find one, ask user to load it or select new one.  *~
            *************************************************************

            if clearflg% = 0% then check_bomspec
                                         /* we are editing an old config*/
            init (hex(00)) readkey2$
            str(readkey2$,1,25) = assypart$
            str(readkey2$,26,9) = cuscode$

            call "PLOWNEXT" (#8, readkey2$, 34%, f1%(8)) /* part + cust */
                if f1%(8) = 1% then L09510

            str(readkey2$,26, ) = " "

            call "PLOWNEXT" (#8, readkey2$, 25%, f1%(8)) /* part only */
                if f1%(8) = 1% then L09623
            goto check_bomspec

*       Customer/Part Specific
L09510:     ask% = 2%
            call "ASKUSER"(ask%,"* * * RE-SELL OLD CONFIGURATION? * * *",~
            "This Customer Has Purchased This Generic Part Before.",     ~
            "Press PF 9 to Select And Load a Previous Configuration, or",~
            "Press PF 16 To Select a New Set of Option Replacements.")
            if ask% = 16% then check_bomspec
            if ask% <> 9% then L09510
            str(readkey2$,35,) = hex(00)
            descr$ = hex(06) & "Previous Configurations sold to " &      ~
                  cuscode$ & " for Part: " & assypart$
            init(" ") hdr$()
            hdr$(1) = "  S.O. Number    Line    Date  / Configuration " &~
                      "Description"
            mat incl_excl = zer
            descr_map(1%) = 35.19 : descr_map(2%) = 1
            descr_map(3%) = 54.061 : descr_map(4%) = 24
            descr_map(5%) = 60.60 : descr_map(6%) = 1008
            call "PLOWCODE" (#8, readkey2$, descr$, 9034%, 0.1, f1%(8),  ~
                  hdr$(), 0,0, incl_excl(), incl_excl$(), "D", " ",      ~
                  #64, descr_map())
                if f1%(8) = 0% then check_bomspec
            old_demand$ = str(readkey2$,35,19)
            gosub load_previous_config
            goto L09730
*       Not customer-specific
L09623:     ask% = 2%
            call "ASKUSER"(ask%,                                         ~
            "* * * LOAD EXISTING CONFIGURATION? * * *",                  ~
            "Previous Configurations Are Available for this Part",       ~
            "Press PF 9 to Select And Load a Previous Configuration, or",~
            "Press PF 16 To Select a New Set of Option Replacements.")
            if ask% = 16% then check_bomspec
            if ask% <> 9% then L09623
            str(readkey2$,35,) = hex(00)
            descr$ = hex(06) & "Previous Configurations on file " &      ~
                              "for Part: " & assypart$
            init(" ") hdr$()
            hdr$(1) = "  Date         Configuration Description"

            mat incl_excl = zer
            descr_map(1%) = 54.061: descr_map(2%) = 1
            descr_map(3%) = 60.60  : descr_map(4%) = 10
            descr_map(5%) =  0    : descr_map(6%) = 0
            call "PLOWCODE" (#8, readkey2$, descr$, 9025%, 0.1, f1%(8),  ~
                  hdr$(), 0,0, incl_excl(), incl_excl$(), "D", " ",      ~
                  #64, descr_map())
                if f1%(8) = 0% then check_bomspec
            old_demand$ = str(readkey2$,35,19)
            gosub load_previous_config

L09730:     assypartkey$ = str(assypart$,,25) & str(inbomid$,,3)
            gosub set_screen_headers
            dollars(1), dollars(2) = stccost
            gosub data_load_cost_loop
            gosub price_lookup
            goto menu_screen_control  /* options are already selected */

        REM *************************************************************~
            * A R E   O P T I O N S   A L R E A D Y   S E L E C T E D ? *~
            *   If so, proceed to the menu, else go ahead & select.     *~
            *************************************************************

        check_bomspec
            init (hex(00)) readkey2$
            dollars(1), dollars(2) = stccost
            if clearflg% = 1% then select_options /* force it */

            str(readkey2$,,19) = demand$
            call "PLOWALTS" (#7, readkey2$, 1%, 19%, f1%(7))
                if f1%(7) = 1% then L09877
                   inbomid$ = " "  /* forces user to select BOMID */
                   goto select_options  /* no BOMSPEC records found */
L09877:     get #7 using L09880, inbomid$
L09880:         FMT POS(51), CH(3)

            assypartkey$ = str(assypart$,,25) & str(inbomid$,,3)

            gosub set_screen_headers
            gosub data_load_cost_loop
            gosub price_lookup /* just to see if it is the same */
            gosub load_price_list : blt_price = tot /* built up price */
            dollars(4) = dollars(3)  /* set price to price passed in */
            goto menu_screen_control  /* options are already selected */

        set_screen_headers

*        Set up the screen headers showing SO#, part#, etc.
            str(headline$(1%),1,19) = "Option Processing: "
            str(headline$(1%),65%) = "Today: " & str(date$,,8)

            str(headline$(2%),1,) = "SO: " & str(demand$,1,16) &         ~
                             " Line: " & str(demand$,17,3)
            str(headline$(2%),62%) = "BOMOPSUB: " & str(cms2v$,,8)

            str(headline$(3%),1,) = "Part: " & str(assypart$,,) &        ~
                             "Desc: " &  str(assypartdescr$,,) &         ~
                             "BOM: " &  str(inbomid$,,)
            return

        REM *************************************************************~
            *             S E L E C T   O P T I O N S                   *~
            *-----------------------------------------------------------*~
            * If options haven't been selected previously, this is      *~
            * where we step through the bill, selecting based on        *~
            * defaults, if any, and manually selecting the rest.  If    *~
            * quantity is to be selected, we drop into the quantity     *~
            * screen.  We end up at the menu when all are selected.     *~
            *************************************************************

        select_options
            gosub determine_top_bom
            assypartkey$ = str(assypart$,,25) & str(inbomid$,,3)
            gosub clear_for_input
            gosub set_screen_headers
            gosub initialize_variables
            if pcode$ > hex(3a) then gosub price_lookup /*gosub if alpha*/
            l%, p%, pl%, options% = 0%
            mat column% = zer
            init(" ") line$(), p$(), m$()

*        All set, Here we go...
            gosub'7(assypartkey$, 1)

*        Ok, we're back - all options have been selected
            if pcode$ < hex(3a) then gosub price_lookup /*gosub if numbr*/
            goto menu_screen_control  /* all done */

        price_lookup
            disc = disc
            currtype$, currency$ = " "

            call "CPRASSGN" (cuscode$, custype$, assypart$, partcat$,    ~
                pcode$, pricedate$, currtype$, currency$, dollars(2),    ~
                  1     , #10, #04, dollars(4), disc, errormsg$)
            if dollars(4) = -1 then dollars(4) = 0
            base, blt_price, cpr_price = dollars(4)
            call "CONVERT"(base, 2.2, base_price$)

            return


        REM *************************************************************~
            *    S C R E E N    C O N T R O L    S E C T I O N S        *~
            *-----------------------------------------------------------*~
            *  Controller loops etc. for the menu and various selection *~
            *  view, and reselection screens -in the order user sees 'em*~
            *************************************************************

        selection_screen_control /* to select replacement part */

            gosub load_replacements_list /*get list defined in HNYOPTNS*/

            if options% = 1% then process_the_selection /* no list   */
                           /* found so use the component in the bill */

            if dflag% = 0% then L10160   /* no default selection avail. */
                ref_key$ = altpart$(dflag%) /* we run with the default */
                goto process_the_selection

L10160:     c% = 0%
            call "DESCRIBE" (#4, p$(pl%), pdesc$, 1%, f1%(4))

L10170:     gosub selection_screen   /* Show 'em the screen  */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then gosub startover
                  if keyhit%  =  3% then gosub startover
                  if keyhit%  <> 4% then  L10230
                          c% = max(1%, c% - 10%)
                          goto L10170
L10230:           if keyhit%  <> 5% then  L10260
                          c% = min(alt% - 10%, c% + 10%)
                          goto L10170
L10260:           if keyhit% <> 16% then L10270
                          returncode% = 4%
                          goto exit_program
L10270:           if keyhit% <>  0% then L10170

*        Figure which one they selected
                if cursor%(1%) < 10% then L10170
                if cursor%(1%) > 19% then L10170
                pick% = (cursor%(1%) - 9%) + c%
                if altpart$(pick%) = " " then L10170
                ignore$ = " "
                if altpart$(pick%) = "** USE ANY STOCK PART **" then     ~
                                           goto pick_a_part
                if altpart$(pick%) = "** USE ANY NON-STOCK **" then      ~
                                           goto enter_anything
                ref_key$ = altpart$(pick%)

        process_the_selection
                gosub set_component
                if ref_key$ = "** DON'T USE ANYTHING **" then L10362
                if dflag% > 0% then save_the_selection
                if pick% > 0% then L10351  /* picked from list */
                   goto save_the_selection /* run with the bom qty */

L10351:         if qflag$(pick%) <> "Y" then save_the_selection

                if str(ref_key$,,25)<>                                   ~
                               "** DON'T USE ANYTHING **" then L10390
L10362:            quan$ = "0"
                   size$ = "1"
                   convert quan$ to qn, data goto L10372
L10372:            convert size$ to sz, data goto L10380
L10380:            goto save_the_selection

L10390:         gosub quantity_screen_control
                   if keyhit% = 4% then L10170   /* goto previous screen */
                   if keyhit% <> 16% then save_the_selection
                      returncode% = 4%  /* User aborted back to S.O. */
                      goto exit_program

        save_the_selection   /* ...to the BOMSPECT file */
            if str(ref_key$,,25) <>"** DON'T USE ANYTHING **" then L10438
                ref_key$ = " "
                ignore$ = "Y"
L10438:     newextqty = qn * sz + over
            newextqty = round((newextqty * ext(pl%)) + fixed ,2)
            gosub'200(component$, str(ref_key$,,25), extqty, newextqty)
            write #7, using L21330, component$, assy$, id$, seq$, demand$,~
                          l%, column%(l%), str(ref_key$,,25), qn, sz,    ~
                          bomid$, ignore$, newextqty, extqty, pseq2$, " "
            bomspec_is_empty% = 0%
            get #7 using L10454, savekey$  /* used when reselecting */
L10454:         FMT POS(26), CH(54)

            return

        pick_a_part
            k% = 0%
            part$ = " "
            header$ = "ENTER OR SELECT A PART NUMBER"
            message1$ = "Enter a Valid Part Number or Leave the Field"
            message2$ = "Blank and Press Return to Pick From List."
            what$ ="Part Number"

            call "ASKSTRNG" (k%, header$, message1$, message2$, what$,   ~
                             part$, 25%, #4, f1%(4))
            if f1%(4) <> 1% then L10170
            if k% = 1% then L10170
            ignore$ = " "
            get #4, using L10514, type$
L10514:        FMT POS(180), CH(3)
            if type$ > "000" and type$ < "200" then ignore$ = "Y"
            ref_key$ = part$
            goto process_the_selection

        enter_anything
            k% = 0%
            part$ = " "
            header$ = "ENTER ANY NON-STOCK PART NUMBER"
            message1$ = "Enter any Non-Stock Part Number and Press Return"
            message2$ = "to Select it as an Option Replacement"
            what$ ="Non-Stock Part Number"
            call "ASKSTRNG" (k%, header$, message1$, message2$, what$,   ~
                             part$, 25%)
            if k% = 1% then L10170
            if part$ = " " then enter_anything
            ref_key$ = part$
            call "READ100" (#4, ref_key$, f1%(4))
               if f1%(4) = 0% then ignore$ = "Y"
            goto process_the_selection

        REM *************************************************************~
            *        Q U A N T I T Y   S C R E E N   C O N T R O L      *~
            * Handles edit of quantity for selected replacement.        *~
            *************************************************************

        quantity_screen_control
            ref_descr$ = "Non-Stock Part"
            call "CONVERT" (qn, 2.2, quan$)
            call "CONVERT" (sz, 2.2, size$)
            call "READ100" (#4, str(ref_key$,,25), f1%(4))
               if f1%(4) = 0% then L11090
               get #4, using L11074, ref_descr$, uom$
L11074:           FMT POS(26), CH(32), POS(74), CH(4)
            call "DESCRIBE" (#9, "UOM      " & uom$, uomdescr$,1%, f1%(9))
L11090:     call "PUTPAREN"(ref_descr$)
L11100:     gosub quantity_screen      /* Display / Accept  */
                if keyhit% <>  0% then L11100

            errormsg$ = " "
            call "NUMTEST"(quan$, 1, 999999999, errormsg$,-2.2, qn)
                if errormsg$ <> " " then L11100
            call "NUMTEST"(size$, 1, 999999999, errormsg$,-2.2, sz)
                if errormsg$ <> " " then L11100

            return

        REM *************************************************************~
            *        M A I N   M E N U   C O N T R O L                  *~
            * We end up here after all options have been selected.      *~
            * User can branch to review or explode functions and/or     *~
            * twiddle with the selling price or margin.  Since data     *~
            * was saved during '7, PF 16 exits back to caller from here.*~
            *************************************************************

        menu_screen_control
            fieldnr% = 1%
            errormsg$ = " "

            margin = dollars(4) - dollars(2)
            gosub do_converts
L12070:     gosub'103(fieldnr%)    /* Display / Accept  */
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  2% then gosub startover
                if keyhit%  =  3% then gosub startover
                if keyhit%  =  9% then gosub review_list_control
                if keyhit%  = 10% then goto explode_bom_control
                if keyhit% <> 11% then L12120
                     fieldnr% = 2%
                     goto L12241
L12120:         if keyhit% <> 12% then L12130
                     fieldnr% = 3%
                     goto L12241
L12130:         if keyhit%  = 14% then gosub print_them
                if keyhit% = 16% and fieldnr% = 1% then exit_program
                if keyhit% = 17% then gosub header_file_control
                if keyhit% = 18% then gosub print_cost_report
                if keyhit% = 19% then gosub price_audit_control
                goto L12070

L12241:     gosub'103(fieldnr%)    /* Display / Accept  */
                if fieldnr% = 3% then L12280
                call "NUMTEST" (percent$, 0, 9e9, errormsg$,-2.2, percent)
                     if errormsg$ <> " " then L12241
                     if percent < 100 and percent > 0 then L12257
                        errormsg$ = "Margin must be less than 100% and" &~
                        " greater than 0."
                        goto L12241
L12257:              fieldnr% = 1%
                     percent = percent / 100
                     dollars(4) = dollars(2) / (1 - percent)
                     margin = dollars(4) - dollars(2)
                     gosub do_converts
                     goto L12070

L12280:         call "NUMTEST" (price$, 0, 9e9, errormsg$,-2.2,dollars(4))
                     if errormsg$ <> " " then L12241
                     fieldnr% = 1%
                     margin = dollars(4) - dollars(2)
                     gosub do_converts
                     goto L12070

        do_converts
            call "CONVERT" (dollars(2), 2.2, cost$)
            call "CONVERT" (dollars(4), 2.2, price$)
            call "CONVERT" (margin, 2.2, margin$)
            if dollars(4) <> 0 then L12345
               percent = 0
               goto L12346

L12345:     percent =  (margin/dollars(4)) * 100
L12346:     call "CONVERT" (percent, 2.2, percent$)
            return

        REM *************************************************************~
            *        R E V I E W   S E L E C T I O N S   M A D E        *~
            * Show the list of replacements that have been made.        *~
            *************************************************************

        review_list_control

            gosub load_bomspec  /* load replacement records to array*/
            c% = 0%
L13070:     str(headline$(1%),20, 25) = "Review Replacements List"
            list_head$ = "Option Part               Replacement Part   "&~
                         "       Replacement Description    "
            str(list_head$,26,1) = hex(ac)
            str(list_head$,52,1) = hex(ac)

L13080:     gosub list_screen
                if keyhit%  <> 4% then  L13100
                     c% = max(0%, c% - 14%)
                     goto L13080
L13100:         if keyhit%  <> 5% then  L13115
                     c% = min(r% - 14%, c% + 14%)
                     goto L13080
L13115:         if keyhit% <> 16% then L13070
                     keyhit% = 0%
                     return

        REM *************************************************************~
            *        P R I C E   A U D I T   L I S T   C O N T R O L    *~
            * Show how the price was built up by adding top components. *~
            *************************************************************

        price_audit_control

            gosub load_price_list  /* load data into list array */
            c% = 0%
L13300:     str(headline$(1%),20, 29) = "Review Price Build-up Details"
            list_head$ = "Part Number               Description        "&~
                         "         Price      Qty  Extension"


            str(list_head$,26,1) = hex(ac)
            str(list_head$,51,1) = hex(ac)
            str(list_head$,60,1) = hex(ac)
            str(list_head$,69,1) = hex(ac)

L13360:     gosub list_screen
                if keyhit%  <> 4% then  L13400
                     c% = max(0%, c% - 14%)
                     goto L13360
L13400:         if keyhit%  <> 5% then  L13430
                     c% = min(r% - 14%, c% + 14%)
                     goto L13360
L13430:         if keyhit% <> 16% then L13300
                     keyhit% = 0%
                     return

        REM *************************************************************~
            *        E X P L O D E   B I L L  O F  M A T E R I A L      *~
            * Allow user to drill down any path into the bill, review   *~
            * replacements selected, AND RESELECT AT ANY POINT.         *~
            *************************************************************

        explode_bom_control
             gosub first_screenload

        do_explode
             gosub plow_comps
             gosub insert_in_index_array

        newscreen
             gosub make_screen_lines

        redisplay
L14120:      gosub display_explosion_screen
                if keyhit% = 1% then  gosub  startover
                if keyhit% <> 16% then L14150
                     keyhit% = 0%
                     goto menu_screen_control
L14150:         if keyhit% <> 8% then L14180
                     t% = 1% + mod(t%,2%)  /* toggle it */
                     goto newscreen
L14180:         if keyhit% <> 2%  then  L14210
                     top% = 1%
                     goto newscreen
L14210:         if keyhit% <> 3%  then  L14240
                     top% = max(max% - 14%, 1%)
                     goto newscreen
L14240:         if keyhit% <> 4%  then  L14270
                     top% = max(1%, top% - 14%)
                     goto newscreen
L14270:         if keyhit% <> 5%  then  L14300
                     top% = min(max% - 14%, top% + 15%)
                     goto newscreen
L14300:         if keyhit% <> 6%  then  L14330
                     top% = max(1%, top% - 1%)
                     goto newscreen
L14330:         if keyhit% <> 7%  then  L14370
                     top% = min(max%, top% + 1%)
                     goto newscreen

L14370:         if keyhit% <> 9% then L14410
                     show_all% = 1% + mod(show_all%,2%) /* toggle it */
                     goto explode_bom_control

L14410:         if keyhit% <> 10%  then  L14440
                     goto explode_bom_control

L14440:         line% = cursor%(1%) - 4%
                if line% < 1% or line% > 15% then L14120
                c% = top% + line% - 1%

                if keyhit% <> 0% then L14630
                if rlist$(1%, idx%(c%)) = " " then L14550
                     if rbom$(idx%(c%)) = " " then redisplay /* no bom */
                     if plus$(idx%(c%)) <> "-" then L14590
                     gosub collapse_index_array
                     goto newscreen

L14550:         if ebom$(idx%(c%)) = " " then redisplay /* no bom */
                if plus$(idx%(c%)) <> "-" then L14590
                     gosub collapse_index_array
                     goto newscreen
L14590:         if plus$(idx%(c%)) <> "+" then redisplay
                     if indent%(idx%(c%)) < 10% then L14600
                     k% = 2%
                     call "ASKUSER"(k%, "* * * LIMIT REACHED * * *",     ~
                      "This routine cannot display an explosion deeper", ~
                      "than 10 levels.  You've hit this limit, sorry.",  ~
                      "Press any key to acknowledge.")
            /* note - no big deal if limit exceeded, screen just wraps */
                goto redisplay

L14600:         gosub set_key
                     goto do_explode

L14630:         if keyhit% <> 12% then redisplay
                     if optn$(idx%(c%)) = " " then redisplay
                     goto reselect_control

        REM *************************************************************~
            *  O P T I O N   S E L E C T I O N   L O O P                *~
            *-----------------------------------------------------------*~
            * Explodes down through the bill, examining each part to    *~
            * see if it's an option or generic. L% is a pseudo- level   *~
            * and will differ from PL% (actual level) when phantoms     *~
            * are encountered.  When we find an option part we GOSUB    *~
            * up to the screen control section to display the list of   *~
            * replacements (or run with the a default).  Once a         *~
            * replacement is made we are back here to continue explode. *~
            *************************************************************~

        deffn'7(partkey$, extqty)
            pl% = pl% + 1%           /* PL% is actual level      */
            l% = max(1, pl% - p%)    /* L% is the 'Pseudo-Level' */
            p$(pl%) = partkey$
            ext(pl%) = extqty
            if extqty <= 0 then L15388  /* end */

L15172:     call "PLOWNEXT" (#5, p$(pl%), 28%, f1%(5))
                if f1%(5) = 0% then L15388

*        Load From BOMMASTR
            if str(p$(pl%),29,3) = "  0" then L15172 /* skip header */
                get #5, using L15228, component$,assy$,id$,seq$,qn,sz,    ~
                                     fixed,over,bommkr$,op$,cbomid$
L15228:         FMT 2*CH(25),2*CH(3),4*PD(14,4),CH(2),CH(1),CH(3)
            sq$(pl%) = seq$
            m$(pl%) = bommkr$
            call "DESCRIBE"(#4, component$, cdesc$, 1%, f1%(4))
                if f1%(4) = 0% then L15172 /* non-stocked part */
            get #4, using L15276, type$
L15276:         FMT XX(179), CH(3)
            if op$ <> "Y" and type$ <> "000" then L15172 /* Move On */
            gosub'190(component$)  /* Find effect. BOM for Component*/
            componentkey$ = str(component$,,25) & str(bomid$,,3)
            extqty = qn * sz + over
            extqty = round((extqty*ext(pl%)) + fixed , 2)
            if op$ <> "Y" then L15348  /* Option is somewhere below.*/
                  column%(l%)=column%(l%)+1% /* Count Of Hits This Levl*/
                  pseq2$ = " " /* Parent Seq # used when multiple optns */
                  if pl% > 1% then pseq2$ = sq$(pl% - 1%) /* per parent */
                  gosub  selection_screen_control
                  if pl% = 1% then gosub add_price
L15348:     if m$(pl%) = "P" then p% = p% + 1%
            if pl% < 30% then gosub'7(componentkey$,extqty)
                                          /* Do Comps If Not At Bottom */
            goto L15172

L15388
*        End routine gracefully
            pl% = pl% - 1%
            if pl% = 0% then goto L15420/* Avoid M$(PL%) blow-up */
                if m$(pl%) = "P" then p% = p% - 1%
L15420:     l% = max(1, pl% - p%)   /* L% Is The 'Pseudo-Level'*/
            return /* Eventualy This Will Return Completely Out Of '7  */

        set_component
            gosub'190(ref_key$)
            componentkey$ = str(ref_key$,,25) & str(bomid$,,3)
            return

        REM *************************************************************~
            *  D A T A   L O A D  C O S T  A D J.  L O O P              *~
            *-----------------------------------------------------------*~
            * Used when loading an existing configuration from BOMSPEC  *~
            * This logic recalculates the cost of the configuration.    *~
            * Much like the original selection loop '7 except the       *~
            * selections are already available out in BOMSPEC.          *~
            * We adjust the cost of the top level part by subtracting   *~
            * out the cost of the option part in the bill (extended by  *~
            * the bill quantities) and add in the cost of the           *~
            * replacement part (again, extended).                       *~
            *************************************************************~

        data_load_cost_loop
            gosub initialize_variables
            pl% = 0%
            gosub loop_setup

            gosub'10(assypartkey$, 1)
            return

        deffn'10(partkey$, extqty)
            pl% = pl% + 1%           /* PL% is actual level      */
            l% = max(1, pl% - p%)    /* L% is the 'Pseudo-Level' */
            p$(pl%) = partkey$
            ext(pl%) = extqty
            if extqty <= 0 then L15884  /* end */

L15652:     call "PLOWNEXT" (#5, p$(pl%), 28%, f1%(5))
                if f1%(5) = 0% then L15884

*        Load From BOMMASTR
            if str(p$(pl%),29,3) = "  0" then L15652 /* skip header */
                get #5, using L15708, component$,assy$,id$,seq$,qn,sz,    ~
                                     fixed,over,bommkr$,op$,cbomid$
L15708:         FMT 2*CH(25),2*CH(3),4*PD(14,4),CH(2),CH(1),CH(3)
            sq$(pl%) = seq$
            m$(pl%) = bommkr$
            call "DESCRIBE"(#4, component$, cdesc$, 1%, f1%(4))
                if f1%(4) = 0% then L15652 /* non-stocked part */
            get #4, using L15756, type$
L15756:         FMT XX(179), CH(3)
            if op$ <> "Y" and type$ <> "000" then L15652 /* Move On */
            gosub'190(component$)  /* Find effect. BOM for Component*/
            componentkey$ = str(component$,,25) & str(bomid$,,3)
            extqty = qn * sz + over
            extqty = round((extqty*ext(pl%)) + fixed, 2)
            if op$ <> "Y" then L15850  /* Option is somewhere below.*/
                  column%(l%)=column%(l%)+1% /* Count Of Hits This Levl*/

                /* read from BOMSPEC to bring in the replacement */
                  init(hex(00)) readkey$
                  str(readkey$,1,28) =  str(p$(pl%),,28)
                  str(readkey$,29,3) =  str(seq$,,)
                  str(readkey$,32,19) =  str(demand$,,19)
                  put readkey$, using L15831, l%, column%(l%) - 1%
L15831:              FMT POS(51), BI(2), BI(2)
            /* use a plow in the event that previous re-selects have  */
            /* mucked up the sequence numbers                         */
L15836:        call "PLOWNEXT" (#7, readkey$, 52%, f1%(7))
                   if f1%(7) = 0% then halt_dataload
                   get #7, using L15840, origpart$,column%(l%), ref_key$, ~
                       qn, sz, cbomid$, extqty, oldext, pseq2$
L15840:            FMT CH(25), POS(78), BI(2), CH(25), 2*PD(14,4), CH(3),~
                       XX(1), 2*PD(14,4), CH(3)
               if pl% = 1% then L15844
               if pseq2$ <> sq$(pl% - 1%) then L15836
L15844:        gosub'200(origpart$, ref_key$, oldext, extqty)
               gosub'190(ref_key$)
               componentkey$ = str(ref_key$,,25) & str(bomid$,,3)
L15850:     if m$(pl%) = "P" then p% = p% + 1%
            if pl% < 30% then gosub'10(componentkey$,extqty)
                                          /* Do Comps If Not At Bottom */
            goto L15652

L15884
*        End routine gracefully
            pl% = pl% - 1%
            if pl% = 0% then goto L15916/* Avoid M$(PL%) blow-up */
                if m$(pl%) = "P" then p% = p% - 1%
L15916:     l% = max(1, pl% - p%)   /* L% Is The 'Pseudo-Level'*/
            return /* Eventualy This Will Return Completely Out Of '10 */

        halt_dataload
            k% = 2%
            call "ASKUSER" (k%, "* * *  DATA LOADING ERROR * * *",       ~
                "Cannot proceed with loading previous option selections",~
                 "From the menu, START OVER and reselect all options.",  ~
                 "Press any Key to Acknowledge.")
            gosub price_lookup  /* get a price so no divide by 0 later */
            goto menu_screen_control

        REM *************************************************************~
            *    D E T E R M I N E  B O M  T O   U S E                  *~
            *-----------------------------------------------------------*~
            * We figure out which bill version to use for the top-level *~
            * item based on data passed in, or ask user for help.       *~
            *************************************************************

        determine_top_bom

*        Is the BOM passed in any good?
            if clearflg% = 1% then inbomid$ = " "
            if inbomid$ = " " then L16310
            init(hex(00)) plowkey$   /* this code prob. never executes */
            str(plowkey$,,28) = str(assypart$,,25) & str(inbomid$,,)
            call "PLOWNEXT" (#5, plowkey$, 28%, f1%(5))
                if f1%(5) = 0% then L16310
                bomid$ = inbomid$
                goto  L16730              /* off to the races */

L16310
*        Get the Effectivity Array - we'll need it soon
            init(hex(00)) readkey$
            str(readkey$,1%,29%) = str(assypart$,1%,25%) & "1" & "001"
            call "READ100" (#11, readkey$, f1%(11%))   /* engmastr */
                 if f1%(11%) = 1% then L16380
                 returncode% = 3%
                 goto no_effective_bom
L16380:     get #11, using L16390, bom$()
L16390:       FMT POS(30), 490 * CH(3)

*        Ask the user for a BOM
            if effdatef$ = " " or effdatef$ = blankdate$ then L16450
               str(temp$,,) ="or Press PF 9 to use the BOM effective " & ~
                              "on: " &  str(effdatef$,,)
L16450:     k% = 2%
            call "ASKUSER" (k%, " * * * WHICH BILL OF MATERIALS FOR OPT" ~
                                & "IONS? * * * ",                        ~
                "Press RETURN to use the effective Bill of Materials",   ~
                "or Press PF 8 to Select the Bill Version. ", temp$)

            if k% = 0% or k% = 8% or k% = 9% then L16500
                goto L16450   /* Hey, press a key I can deal with */

L16500:     if temp$ = " " then L16580
            if k% <> 9% then L16580

*        Get the BOM for the date passed in (probably the ship date)
            gosub'190(assypart$)
            goto L16730

*        Get the user to pick a BOM from the list
L16580:    if k% <> 8% then L16700
            readkey$ = str(assypart$,,25%) & str(inbomid$,,3%)
            hdr$()="  Listed Below Are The Existing BOMs For Part: " &   ~
                                                             assypart$
            errormsg$ = hex(06) & "Select Bill Of Materials"
            call "PLOWCODE" (#5, readkey$, errormsg$, 2025%, .30,        ~
                                                     f1%(5%),  hdr$(), 3)
            if f1%(5%) <> 0% then L16670
                goto L16450 /* lets try this again */
L16670:     bomid$ = str(readkey$,26%,3%)
            goto L16730

L16700
*       Looks like we run with the effective bill for todays date
            bomid$ = bom$(today%)
            dateindex% = today%
L16730:     if bomid$ = " " then no_effective_bom
            inbomid$ = bomid$
            return

        no_effective_bom
            h% = 2%
L16770:     call "ASKUSER" (h%, " * * * WARNING * * * ",                 ~
            "No BOM is set effective for " & effdatef$ & ".",            ~
            "Press RETURN to Select a BOM from the list of all BOMS, ",  ~
            "or Press PF 16 to Cancel Option Selection and Exit.")
            if h% = 0% or h% = 16% then L16830
              goto L16770
L16830:     if h% <> 0% then L16840
                k% = 8%
                goto L16580
L16840:     returncode% = 4%
            goto exit_program

        REM *************************************************************~
            *        H E A D E R   F I L E   C O N T R O L              *~
            *-----------------------------------------------------------*~
            * Here we save a header record for this particular config.  *~
            * We can then allow the user to choose to load previous     *~
            * configuration in lieu of doing selecting replacements.    *~
            *************************************************************

        header_file_control

            call "REDALT0" (#8, demand$, 1%, f1%(8))
               if f1%(8) = 0% then L17150  /* to input mode */
            get #8, using L17110, old_demand$, old_date$, config_descr$
L17110:        FMT POS(35), CH(19), CH(6), CH(60)
            call "DATEFMT" (old_date$)
            goto L17280  /* to input mode */

L17150
*       Input mode
            old_date$ = date$
            old_demand$ = demand$
            config_descr$ = " "
L17190:     edit% = 0%
L17200:     gosub header_screen
                if keyhit% <> 1% then L17240
                   errormsg$ = " "
                   return
L17240:         if keyhit% <> 0% then L17200
            gosub test_header_data
            if errormsg$ <> " " then L17200

L17280
*       Edit mode for header_file screen
            errormsg$ = " "
            edit% = 1%
L17310:     gosub header_screen
                if keyhit% <> 1% then L17350
                   errormsg$ = " "
                   return
L17350:         if keyhit% = 0% then L17190
                if keyhit% <> 12% then L17370
                if f1%(8) = 0% then L17370
                   gosub delete_header_record
                   if f1%(8) = 0% then return
L17370:         if keyhit% <> 16% then L17310
            gosub test_header_data
            if errormsg$ <> " " then L17200

            call "DATUNFMT" (old_date$)
            if f1%(8) = 0% then L17520

*       Update old BOMSPHDR record
            call "REDALT1" (#8, demand$, 1%, f1%(8))
            put #8, using L17470, old_date$, config_descr$
L17470:        FMT POS(54), CH(6), CH(60)
            rewrite #8
            return

*       Write new BOMSPHDR record
L17520:     put #8, using L21460,  assypart$, cuscode$, demand$,          ~
                                  old_date$, config_descr$, " "
            write #8
            return

        delete_header_record
            k% = 2%
            call "ASKUSER"(k%,"* * * CONTINUE? * * *",                   ~
                 "Please confirm your Intention to Delete the Header",   ~
                 "Press PF 24 to Delete the Configuration Header or",    ~
                 "Press any other key to abort this action.")

            if k% <> 24% then return
            call "REDALT1" (#8, demand$, 1%, f1%(8))
            delete #8
            f1%(8) = 0%
            return

        REM *************************************************************~
            *      G E T   E F F E C T I V E   B O M                    *~
            *-----------------------------------------------------------*~
            * Gets the current BOM for passed part based on dateindex%  *~
            *************************************************************

        deffn'190(part$)
            if ref_key$ <> "** DON'T USE ANYTHING **" then L18090
              bomid$, cbomid$ = " "
              return
L18090:     if ref_key$ <> "** USE ANY NON-STOCK ** " then L18110
              bomid$, cbomid$ = " "
              return
L18110:     bomid$ = cbomid$
            if part$ <> assypart$ and cbomid$ <> " " then return
            if dateindex% = 0% then return  /* OFF PROD CALENDER? */
                readkey2$ = " "
                readkey2$ = str(part$,,25) & "1" & " "
                    call "PLOWNEXT" (#11, readkey2$, 26%, f1%(11))
                    if f1%(11) <> 1% then return
                       get #11, using L18180, str(bom$())
L18180:                         FMT XX(29), CH(1470)
                    if str(readkey2$,,25) <> str(part$,,25) then return
                    bomid$ = bom$(dateindex%)
                    return

        REM *************************************************************~
            *    P R I N T   T H E   S E L E C T E D   O P T I O N S    *~
            *-----------------------------------------------------------*~
            * Dumps all records from the BOMSPEC File for this demand.  *~
            *************************************************************

        print_them

            call "SHOSTAT" ("Printing List Of Options Selected")
            line1% = 1000: page% = 0
            gosub L19160
            print using L19670            /* PRINT TAG LINE WHEN DONE. */
            close printer
        return

L19160:     readkey2$ = all(hex(00))
            str(readkey2$,,19) = demand$

L19190:     call "PLOWALTS" (#7, readkey2$, 1%, 19%, f1%(7))
                     if f1%(7) = 0 then return
            get #7, using L19240, pcomp$, passy$, pbom$, prepl$
            if prepl$ = " " then prepl$ = "**NOTHING USED**"

L19240:     FMT CH(25),                  /* COMPONENT PART NUMBER      */~
                CH(25),                  /* ASSEMBLY PART NUMBER       */~
                CH(3),                   /* BOM STRUCTURE ID           */~
                XX(3),                   /* BOM SEQUENCE NUMBER        */~
                XX(19),                  /* DEMAND CODE & LINE         */~
                XX(4),                   /* ADDITIONAL SEQ TO STOP DUPS*/~
                CH(25)                   /* REPLACMENT PART            */~

*        Routine to print the record just loaded.
            gosub L19440              /* PAGE CONTROL SUBROUTINE    */
            pdescr$() = " "
            call "DESCRIBE"(#4, pcomp$, pdescr$(1), 0%, f1%(4))
            print using L19730, passy$, pbom$, pcomp$, pdescr$(1)
            pdescr$() = " "
            call "DESCRIBE"(#4, passy$, pdescr$(1), 0%, f1%(4))
            call "DESCRIBE"(#4, prepl$, pdescr$(2), 0%, f1%(4))
            print using L19730, pdescr$(1), " ", prepl$, pdescr$(2)
            print using L19670
            goto L19190

L19440
*        Page control routine.
            select printer(134)
            line1% = line1% + 3
            if line1% < 60 then return
                   print page
                   page% = page% + 1
                   print using L19620, page%, assypart$, assypartdescr$,  ~
                                      date$
                   print using L19640, str(demand$,,16), str(demand$,17), ~
                                      str(assypartkey$, 26, 3), effdatef$
                   print
                   print using L19670
                   print using L19690
                   print using L19710
                   print using L19670
                   line1% = 7
            return

L19620: %PAGE#####   O P T I O N   S E L E C T I O N S   F O R: #########~
        ~################# ##################################     ########
L19640: %DEMAND CODE ################  LINE ###   USING BOM ### EFFECTIVE~
        ~ ON ########

L19670: %+---------------------------------+----+------------------------~
        ~---+---------------------------------+
L19690: %!ASSEMBLY PART                    !BOM ! COMPONENT PART         ~
        ~   !COMPONENT PART DESCRIPTION       !
L19710: %!ASSEMBLY PART DESCRIPTION        !    ! REPLACEMENT PART       ~
        ~   !REPLACEMENT PART DESCRIPTION     !
L19730: %!################################ !### !########################~
        ~## !################################ !


        REM *************************************************************~
            *     C H E C K   F O R   O P T I O N S  L I S T            *~
            *-----------------------------------------------------------*~
            * Checks to see if this component has a list of replacement *~
            * parts.  If so, we load them into ALTPART$(). If no list   *~
            * is found, then we fill the hole with the component itself.*~
            * Note that if a list is found, the component cannot be     *~
            * selected to replace itself unless it is on its own list.  *~
            *************************************************************

        load_replacements_list

            dflag%, alt% = 0%
            alt$(), altpart$(), altdesc$(), dflag$(), qflag$() = " "

*        Check for assembly-specific options first...
            init(hex(00)) readkey$
            anypart% = 0%  /* flags whether any stock part allowed */
            anynsp% = 0%    /* flags whether any non-stock part allowed */
            alt% = 1%  /* counter for replacements in the list */
            str(readkey$,,28) = p$(pl%)
            str(readkey$,29,26) = str(component$) & hex(00)
            call "PLOWNEXT" (#1, readkey$, 53%, f1%(1%))
                if f1%(1%) <> 0% then L20380

*        Ok, so we gotta look for part-specific options...
            str(readkey$,,28) = " "
            call "PLOWNEXT" (#1, readkey$, 53%, f1%(1%))
                if f1%(1) <> 0% then L20380
            altpart$(alt%) = component$
            call "DESCRIBE" (#4,altpart$(alt%),altdesc$(alt%),0%,f1%(4))
            options% = 1%          /* set flag to show no list found */
            return

L20360:     call "PLOWNEXT" (#1, readkey$, 53%, f1%(1%))
                if f1%(1%) = 0% then return

*        Ok, we got a list so load it
L20380:     options% = 2%          /* set flag to show we got a list */
            get #1, using L20382, altpart$(alt%), dflag$(alt%),qflag$(alt%)
L20382:          FMT POS(55), CH(25), POS(87), CH(1), CH(1)

            if altpart$(alt%) <> "** USE ANY STOCK PART **" then L20395
                 if anypart% = 1% then L20360  /* duplicate so skip it */
                 anypart% = 1%
                 dflag$(alt%) = "N"  /* just to make sure */
                 goto L20401
L20395:     if altpart$(alt%) <> "** USE ANY NON-STOCK **" then L20401
                 if anynsp% = 1% then L20360  /* duplicate so skip it */
                 anynsp% = 1%
                 dflag$(alt%) = "N"  /* just to make sure */

L20401:     if dflag$(alt%) <> "Y" then dflag$(alt%) = "N"
            if qflag$(alt%) <> "Y" then qflag$(alt%) = "N"

            if force_manual% = 1% then L20420 /* use no defaults */
            if anypart% + anynsp% > 0% then L20422 /* skip describe */
            if dflag$(alt%) = "Y" then dflag% = alt%

L20420:     call "DESCRIBE" (#4,altpart$(alt%),altdesc$(alt%),0%,f1%(4))
                 if dflag% <> 0% then return /*got a default so lets go*/
L20422:     alt% = min(alt%+1%,100%)
            goto L20360

        REM *************************************************************~
            *          S A V E   O P T I O N   S E L E C T E D          *~
            *                                                           *~
            * Saves the replacement part for the current option part    *~
            * in the BOMSPEC file.                                      *~
            *************************************************************



        REM *************************************************************~
            *          A D J U S T   S T A N D A R D   C O S T          *~
            *-----------------------------------------------------------*~
            * Backs out the standard cost for one part and adds in the  *~
            * standard cost of the second part.                         *~
            *************************************************************

        deffn'200(oldpart$, newpart$, oldqty, newqty)
            if oldpart$ = newpart$ then return

*        Back out the old...
            if dollars(2) = -1 then return /* Already Screwed */
            call "READ100" (#4, oldpart$, f1%(4))
                if f1%(4) <> 0 then L21180
                cstccost   =  0        /* Non-stocked part */
                goto L21190
L21180:     call "STCCOSTS" (oldpart$, " ", #10, 1%, cstccost)
L21190:     dollars(2) = dollars(2) - (oldqty*(cstccost))
            if newpart$ = " " then return

*        And add in new standard cost
            call "READ100" (#4, newpart$, f1%(4))
            if f1%(4) <> 0% then L21270
                cstccost   =  0        /* Non-stocked part */
                goto L21280
L21270:     call "STCCOSTS" (newpart$, " ", #10, 1%, cstccost)
L21280:     dollars(2) = dollars(2) + (newqty*(cstccost))
            return



        REM *************************************************************~
            *             F I L E    F O R M A T S                      *~
            * --------------------------------------------------------- *~
            *                                                           *~
            *                                                           *~
            *************************************************************

*        Format for BOMSPEC File
L21330:     FMT CH(25),                  /* Component (optn part in BOM*/~
                CH(25),                  /* Assembly part # -(parent)  */~
                CH(3),                   /* BOM Structure Id (parent)  */~
                CH(3),                   /* BOM Seq # (parent)         */~
                CH(19),                  /* Demand code and line       */~
                BI(2),                   /* Level (row)                */~
                BI(2),                   /* Column (n'th option in row)*/~
                CH(25),                  /* Replacement part (selected)*/~
                PD(14,4),                /* Quantity used (of repl part*/~
                PD(14,4),                /* Size (times used (ditto)   */~
                CH(3),                   /* BOM ID of replacement      */~
                CH(1),                   /* Flag for planning          */~
                PD(14,4),                /* Extended Quantity of repl  */~
                PD(14,4),                /* Extended Quantity of compnt*/~
                CH(3),                   /* BOM seq # of parent        */~
                CH(7)                    /* Filler                     */~

*        Format for BOMSPHDR File
L21460:     FMT CH(25),       /*  1 - 25    Generic top-level parent   */~
                CH(09),       /* 26 - 34    Customer Code              */~
                CH(19),       /* 35 - 53    Demand Code and Line       */~
                CH(06),       /* 54 - 59    Date of order              */~
                CH(60),       /* 60 - 119   Text Description of Config */~
                CH(31)        /* 120 - 150  Filler                     */~

        REM *************************************************************~
            *   C L E A R  A N Y T H I N G   L A Y I N G   A R O U N D  *~
            *-----------------------------------------------------------*~
            * Clears everything in BOMSPEC for this demand/line.        *~
            *************************************************************

        clear_for_input
            init (hex(00)) readkey2$
            bomspec_is_empty% = 1%     /* 0 means it has records */
            str(readkey2$,,19) = str(demand$,,)
*       Clear the BOMSPHDR record for this config if it exists
            call "REDALT1" (#8, readkey2$, 1%, f1%(8))
                if f1%(8%) = 0% then L21670
            delete #8
*       Then loop through BOMSPEC to clear out records
L21670:     call "PLOWAL1" (#7, readkey2$, 1%, 19%, f1%(7))
                if f1%(7%) = 0% then return
                delete #7
            goto L21670


        REM *************************************************************~
            *     A D J U S T   S E L L I N G   P R I C E               *~
            *-----------------------------------------------------------*~
            * If the price code passed in is numeric, we calculate the  *~
            * cost and then apply the margin up in PRICE_LOOKUP.  If    *~
            * the price code is alpha, we take the price returned from  *~
            * CPRASSGN in PRICE_LOOKUP and we add to it the prices of   *~
            * the top level option replacements selected.  When in      *~
            * review mode (CLEARFLAG = 0) we run with the price passed  *~
            * in.  When re-selecting options, if the item reselected    *~
            * is a top level item, we subtract its price out, and add   *~
            * in the price of the new replacement.                      *~
            *                                                           *~
            *************************************************************

        add_price
            if pcode$ < hex(3a) then return  /* numberic is cost plus */
            cprice = 0
            call "READ100" (#4, ref_key$, f1%(4))
               if f1%(4) = 0% then return  /* can't have a price */
               get #4, using L22102, catcode$
L22102:        FMT POS(90), CH(4)

            disc = disc
            currtype$, currency$ = " "

            call "CPRASSGN" (cuscode$, custype$, ref_key$,   catcode$,   ~
                pcode$, pricedate$, currtype$, currency$, dollars(2),    ~
                  1     , #10, #04, cprice, disc, errormsg$)
            if cprice = -1 then cprice = 0
            dollars(4) = dollars(4) + (cprice * qn * sz)
            blt_price = blt_price + (cprice * qn * sz)
            return

        subtract_price

            if pcode$ < hex(3a) then return  /* numberic is cost plus */
            cprice = 0
            call "READ100" (#4, component$, f1%(4))
               if f1%(4) = 0% then return  /* can't have a price */
               get #4, using L22360, catcode$
L22360:        FMT POS(90), CH(4)

            disc = disc
            currtype$, currency$ = " "

            call "CPRASSGN" (cuscode$, custype$, component$, catcode$,   ~
                pcode$, pricedate$, currtype$, currency$, dollars(2),    ~
                  1     , #10, #04, cprice, disc, errormsg$)
            if cprice = -1 then cprice = 0
            dollars(4) = dollars(4) - (cprice * qn * sz)
            blt_price = blt_price - (cprice * qn * sz)
            return

        REM *************************************************************~
            *         P R I N T   C O S T   R E P O R T                 *~
            *-----------------------------------------------------------*~
            * Call the subroutine to print the cost audit report for    *~
            * this generic part with options selected.                  *~
            *                                                           *~
            *************************************************************

        print_cost_report

           call "BOMOPCST"                                               ~
                 (demand$,               /* Sales Order + Line #       */~
                 "Y",                    /* Explode only generics      */~
                 "N",                    /* Print Part descriptions    */~
                 assypart$,              /* Top level part number      */~
                 effdate$,               /* Due date                   */~
                 #4,                     /* HNYMASTR                   */~
                 #5,                     /* BOMMASTR                   */~
                 #7,                     /* BOMSPEC                    */~
                 #10,                    /* SYSFILE2                   */~
                 #11,                    /* ENGMASTR                   */~
                 u3%)                    /* Return Code                */

            return

        REM *************************************************************~
            *               E X P L O S I O N   S T U F F               *~
            *-----------------------------------------------------------*~
            * Routines for handling PF 10 display, drill down, etc.     *~
            * Note that this logic is from BOMBRWSB and the handling of *~
            * the arrays is better documented there.  This version of   *~
            * the logic is complicated by ability to reselect options.  *~
            * These routines are called from EXPLODE_BOM_CONTROL section*~
            *************************************************************

        REM *************************************************************~
            *      I N I T I A L I Z E  D I S P L A Y                   *~
            *************************************************************

        first_screenload   /* initialize the display              */

            init(hex(00)) plowkey$, plist$()
            init(" ") ebom$(), type$(), plus$(), optn$(), rtype$(),      ~
                      marker$(), seq$(), rlist$(), rbom$(),rqty$(), qty$()
            mat indent% = con
            init(hex(ff)) active$()
            mat tptr% = zer
            mat idx%  = zer
            str(plowkey$,,28) = str(assypartkey$, ,28)

            top% = 1%  /* Tracks which array element is top of screen */
            max% = 0%  /* Maximum elements in big parts array */
            c% = 0%    /* Current position in index array */
            t% = 1%    /* part/descrip toggle set to show part number */
            indent_level% = 1%   /* level of components just loaded */

            return

        REM *************************************************************~
            *               L O A D   C O M P O N E N T S               *~
            *************************************************************

        plow_comps
            new% = 0%
            oldmax% = max%

        next_component
L25370:     call "PLOWNEXT" (#5, plowkey$, 28%, f1%(5%))
            if f1%(5%) <> 1% then return

            get #5, using L25410, temp$, seq$
L25410:         FMT CH(25), POS(54), CH(3)

            if seq$ <> "  0" then L25480
                get #5, using L25450, batch
L25450:         FMT POS(107), PD(14,4)
                goto L25370               /* skip header records */

L25480:     get #5, using L25490, temp$,seq$,qty(),marker$,optn$, cbomid$
L25490:         FMT CH(25), POS(54), CH(3), 4*PD(14,4),CH(2),CH(1), CH(3)

            call "READ100" (#4, str(temp$,1,25), f1%(4%)) /* hnymastr*/
                 if f1%(4%) <> 1% then L25560
                   get #4, using L25540,  type$
L25540:               FMT POS(180), CH(3)

L25560:     if show_all% = 2% then L25610
            if type$ =  "000" then L25610
            if optn$ =  "Y" then L25610
            goto next_component

L25610:     new% = new% + 1%
            max% = max% + 1%
            if max% > d% then abort_message     /* exceeded array size */

            a% = pos(str(active$()) = hex(ff)) /* -find a hole in array */
            plist$(1%, a%) = str(temp$,1,25)   /* -put it in the hole   */
            tptr%(new%) = a%                   /* -record where it is   */
            active$(a%) = " "                  /* -mark hole as filled  */
            temp$ = " "
            ebom$(a%) = cbomid$
            type$(a%) = type$
            indent%(a%) = indent_level%
            marker$(a%) = marker$
            seq$(a%) = seq$
            if optn$ = "Y" then optn$(a%) = optn$
            total =  (batch * ((qty(1%)* qty(2%)) + qty(4%)))/*+QTY(3)*/
            call "CONVERT"(total, 0.4, qty$(a%))

            gosub check_for_bom    /* find effective bom if exists  */
            call "DESCRIBE" (#4, plist$(1%,a%), plist$(2%,a%),0%, f1%(4%))
            if f1%(4) = 0% then plist$(2%,a%) = "* * Non-Stocked Part * *"

*        Load option replacement from BOMSPEC
            if optn$ <> "Y" then next_component
            init(hex(00)) readkey$
            str(readkey$,1,28) = str(plowkey$,1,28)
            str(readkey$,29,3) = str(seq$,,)
            str(readkey$,32,19) = str(demand$,,)
            seqnr% = 0%
            put readkey$ using L25900, indent_level%, seqnr%
L25900:        FMT POS(51), BI(2), BI(2)
L25910:     call "PLOWNEXT" (#7, readkey$, 52%, f1%(7%))
               if f1%(7%) = 0% then next_component
            get #7, using L25931, rlist$(1%,a%), qn, sz, rbom$(a%), pseq2$
L25931:        FMT POS(80), CH(25), 2*PD(14,4), CH(3), POS(141), CH(3)
            if c% = 0% then L25939
            if indent%(idx%(c%)) = 1% then L25939
            for up% = max(1%, c%) to 1% step -1   /* why not just c%? */
              if plus$(idx%(up%)) <> "-" then L25937
              if indent%(idx%(up%)) <= indent%(idx%(c%)) then L25938
L25937:     next up%
L25938:       if pseq2$ <> seq$(idx%(up%)) then L25910
L25939:     quan = qn * sz
            call "CONVERT"(quan, 0.4, rqty$(a%))
            if rlist$(1%,a%) <> " " then L25971
               rlist$(1%,a%) = "* * Nothing * *"
               rlist$(2%,a%) = "("&"Replaces " & plist$(1%,a%) & ")"
               rtype$(a%) = "NSP"
               goto L26040

L25971:     call "DESCRIBE" (#4, rlist$(1%,a%), rlist$(2%,a%),0%, f1%(4%))
            if f1%(4) = 0% then rlist$(2%,a%) = "* * Non-Stocked Part * *"
            gosub check_for_repl_bom   /* find effective bom if exists  */
            rlist$(2%,a%) = "* * Non-Stocked Part * *"
            rtype$(a%) = "NSP"
            call "READ100" (#4, rlist$(1%,a%), f1%(4%))
            if f1%(4) = 0% then L26040
            get #4, using L26030, rlist$(2%,a%),   rtype$(a%)
L26030:         FMT POS(26), CH(32),   POS(180), CH(3)

L26040:     optn$(a%) = "R" /* to flag replacement made */

            goto next_component

        check_for_bom /* does component have effective bom?*/
            if ebom$(a%) <> " " then L26170 /*using 'bill to use' instead*/
            init(hex(00)) readkey$
            str(readkey$,1%,29%) = str(plist$(1%,a%),1%,25%)& "1" & "001"
            call "READ100" (#11, readkey$, f1%(11%))  /* engmastr */
                 if f1%(11%) <> 1% then return
                   get #11, using L26150,  bom$()
L26150:               FMT POS(30), 490 * CH(3)
            ebom$(a%) = bom$(dateindex%)
L26170:     plus$(a%) = "+"
            return

        check_for_repl_bom /* does replacement have effective bom?*/
            if rbom$(a%) <> " " then L26280 /*using bill from BOMSPEC */
            init(hex(00)) readkey$
            str(readkey$,1%,29%) = str(rlist$(1%,a%),1%,25%)& "1" & "001"
            call "READ100" (#11, readkey$, f1%(11%))  /* engmastr */
                 if f1%(11%) <> 1% then return
                   get #11, using L26260,  bom$()
L26260:               FMT POS(30), 490 * CH(3)
            rbom$(a%) = bom$(dateindex%)
L26280:     plus$(a%) = "+"
            return

        REM *************************************************************~
            *                S E T  P L O W  K E Y                      *~
            *************************************************************

        set_key
            init (hex(00)) plowkey$
            if rlist$(1%, idx%(c%)) <> " " then L26450

            str(plowkey$, 1%, 25%) = plist$(1%, idx%(c%))
            str(plowkey$,26%,  3%) = ebom$(idx%(c%))
            goto L26480

L26450:     str(plowkey$, 1%, 25%) = rlist$(1%, idx%(c%))
            str(plowkey$,26%,  3%) = rbom$(idx%(c%))

L26480:     indent_level% = indent%(idx%(c%)) + 1% /* in another level */
            plus$(idx%(c%)) = "-"
            return

        REM *************************************************************~
            *            I N S E R T   I N D E X   A R R A Y            *~
            *************************************************************

        insert_in_index_array

            if c% = 0% then L26740  /* skip the down/up moves first time */

            call"MXST4PT"addr(idx%(d%-(oldmax%-c%)),idx%(c%+1%),         ~
                                 oldmax%-c%)
            call "MXST4PT" addr(idx%(c%+1%+new%),idx%(d%-(oldmax%-c%)),  ~
                                 oldmax%-c%)

L26740:     for i% = 1% to new%
                 idx%(c% + i%) = tptr%(i%)
            next i%

            mat tptr% = zer

            return

        abort_message
            k% = 2%
            call "ASKUSER" (k%, " * * * ARRAY OVERFLOW * * *",           ~
                      "Too many components for this display to handle.", ~
                      "Don't drill down so far, or have your SA change", ~
                      "the array size.  Press any key to continue.")
            return clear all
            goto menu_screen_control

        REM *************************************************************~
            *       S E T  U P    T H E   S C R E E N   L I N E S       *~
            *************************************************************

        make_screen_lines
             init(hex(20)) scrn$() :  init(hex(8c)) lfac$()
             temp$ = " "
             for i% = 1% to min(max%+1%-top%,15%)

                j% =  top% +i% -1%   /* where we are */

                if rlist$(1%,idx%(j%)) <> " " then L27140
                if ebom$(idx%(j%)) <> " " then                           ~
                            str(temp$,1,1) = plus$(idx%(j%))
                goto L27170

L27140:         if rbom$(idx%(j%)) <> " " then                           ~
                            str(temp$,1,1) = plus$(idx%(j%))

L27170:         str(temp$, 2%, 3%) =  seq$(idx%(j%))
                str(temp$, 5%, 1%) =  hex(29)          /* the ')' */
                if type$(idx%(j%)) = "000" then str(temp$,6%,1%) = hex(84)
                str(temp$, 7%,32%) =  plist$(t%, idx%(j%))

               /* do the swap of replacement part */
                if rlist$(t%, idx%(j%)) <> " " then                      ~
                          str(temp$, 7%,32%) =  rlist$(t%, idx%(j%))
                if rtype$(idx%(j%)) = "000" then str(temp$,6%,1%) =hex(84)
                str(scrn$(i%),indent%(idx%(j%))* 2% -1%,79%)= str(temp$,,)
                str(scrn$(i%),64%,1%) =  hex(8c)
                str(scrn$(i%),65%,10%) =  qty$(idx%(j%))
                if rlist$(t%, idx%(j%)) <> " " then                      ~
                          str(scrn$(i%),65%,10%) =  rqty$(idx%(j%))
                str(scrn$(i%),76%,02%) =  marker$(idx%(j%))
                str(scrn$(i%),78%,1%)= hex(84)
                str(scrn$(i%),79%,01%) =  optn$(idx%(j%))
                temp$ = " "
                if plus$(idx%(j%)) <> " " or optn$(idx%(j%)) <> " " then ~
                    lfac$(i%) = hex(8e)  /* set active line as tab stop */
             next i%
             return

        REM *************************************************************~
            *        C O L L A P S E   I N D E X   A R R A Y            *~
            *************************************************************

        collapse_index_array
*        Note that IDX%(C%) is the parent, INDENT%(IDX%(C%) is its level
            plus$(idx%(c%)) = "+"
            oldmax% = max%
            del% = 0%
*        Here we count the suckers and clear each one out
            for s% = 1% to max%-c%
                 if show_all% = 2% then L27590
                    if idx%(c% + s%) = 0% then L27710
L27590:          if indent%(idx%(c% + s%)) <= indent%(idx%(c%))          ~
                        then hit_bottom
                 del% = del% + 1%
                 active$(idx%(c% + s%)) = hex(ff)  /* reclaim space */
                 if plus$(idx%(c% + s%)) = "-" then                      ~
                                    plus$(idx%(c% + s%)) = "+"
                 ebom$(idx%(c% + s%)) = " "
                 plist$(1, idx%(c% + s%)) = " "  /* ? */
                 plist$(2, idx%(c% + s%)) = " "
                 rlist$(1, idx%(c% + s%)) = " "
                 rlist$(2, idx%(c% + s%)) = " "
L27710:     next s%

        hit_bottom
*        Here we close up the gap in the index
            call "MXST4PT" addr(idx%(c%+1%),idx%(c% + del% + 1%),        ~
                                 max%-c%-del%)
            max% = oldmax% - del%
            return

        REM *************************************************************~
            *        E N D   O F   E X P L O S I O N   S T U F F        *~
            *************************************************************

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$, cost$, margin$, price$
            margin, dollars(4), calc_cost = 0
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
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
            restart% = 1%
            return clear all
            clearflg% = 1%
            force_manual% = 0%
            if keyhit% = 3% then force_manual% = 1%
            dollars(1), dollars(2) = stccost
            dollars(4) = dollars(3)
            call "FILEBGON" (#6)
            if keyhit% > 1% then select_options
            gosub clear_for_input
            returncode% = 4%
            goto exit_program

        REM *************************************************************~
            *    L O A D   R E C O R D S   F R O M   B O M S P E C      *~
            *-----------------------------------------------------------*~
            * Read records into array for Review List Screen.           *~
            *************************************************************

        load_bomspec
            init(hex(00)) list$(), opart$, rpart$, rdesc$
            r% = 0%
            plowkey$ = all(hex(00))
            str(plowkey$,,19) = str(demand$,,)

L30130:     call "PLOWALTS" (#7, plowkey$, 1%, 19%, f1%(7))
                if f1%(7) = 0% then return
            r% = r% + 1%
            get #7, using L30151, opart$, rpart$
L30151:        FMT CH(25), POS(80), CH(25)
            if rpart$ <>" " then L30290
               rpart$ = "** DON'T USE ANYTHING **"
               rdesc$ = " "
               goto L30292
L30290:     call "DESCRIBE"(#4, rpart$, rdesc$, 0%, f1%(4))
            if f1%(4) = 0% then rdesc$ = "* * Non-Stocked Part * *"
L30292:     str(list$(r%),1,25) = str(opart$,,)
            str(list$(r%),27,25) = str(rpart$,,)
            str(list$(r%),53,27) = str(rdesc$,1,27)
            goto L30130  /* read next record */

        REM *************************************************************~
            *    L O A D   P R E V I O U S   C O N F I G U R A T I O N  *~
            *-----------------------------------------------------------*~
            * Create a new set of BOMSPEC records from a previous set.  *~
            *************************************************************

        load_previous_config

            f% = 0%  /* so we can grab parent bomid out of first record */
            plowkey$ = all(hex(00))
            str(plowkey$,,19) = str(old_demand$,,)

L30470:     call "PLOWALTS" (#7, plowkey$, 1%, 19%, f1%(7))
                if f1%(7) = 0% then return

            get #7, using L30510, record$
L30510:        FMT CH(150)
            if f% = 0% then inbomid$ = str(record$,51,3)
            str(record$,57,19) = str(demand$,,)
            put #7, using L30510, record$
            write #7
            f% = 1%
            goto L30470  /* read next record */


        REM *************************************************************~
            *    L O A D   P R I C E   A U D I T  I N F O               *~
            *-----------------------------------------------------------*~
            * Read records into array for Review List Screen.           *~
            *************************************************************

        load_price_list
            init(hex(00)) list$()
            tot = base
            r% = 1%
            plowkey$ = all(hex(00))
            str(plowkey$,,19) = str(demand$,,)
            str(list$(1),1,25) = str(assypart$,,)
            str(list$(1),27,  ) =  "(Base at Price Code " & pcode$ & ")"

            str(list$(1),52,8) = str(base_price$,,)
            str(list$(1),65,4) = "1.00"
            str(list$(1),72,8) = str(base_price$,,)

L30720:     call "PLOWALTS" (#7, plowkey$, 1%, 19%, f1%(7))
                if f1%(7) = 0% then L30900
            if str(plowkey$,20,2) <> hex(0001) then L30720

            r% = r% + 1%
            get #7, using L30760, rpart$, ext
L30760:        FMT POS(80), CH(25), POS(125), PD(14,4)
            if rpart$ <>" " then L30810
               rpart$ = "** DON'T USE ANYTHING **"
               rdesc$ = " "
               ext, cprice, extprice = 0
               goto L30829
L30810:     call "DESCRIBE"(#4, rpart$, rdesc$, 0%, f1%(4))
            if f1%(4) = 0% then rdesc$ = "* * Non-Stocked Part * *"
            disc = disc
            currtype$, currency$ = " "
            call "STCCOSTS" ( rpart$, " ", #10, 1%, cstccost)
            call "CPRASSGN" (cuscode$, custype$, rpart$,    partcat$,    ~
                pcode$, pricedate$, currtype$, currency$, cstccost,      ~
                  1     , #10, #04, cprice,     disc, errormsg$)
            if cprice = -1 then cprice = 0
            extprice = ext * cprice
L30829:     call "CONVERT" (ext, 2.2, ext$)
            call "CONVERT" (cprice, 2.2, cprice$)
            call "CONVERT" (extprice, 2.2, extprice$)
            tot = tot + extprice
            str(list$(r%),1,25) = str(rpart$,,)
            str(list$(r%),27,25) = str(rdesc$,1,25)
            str(list$(r%),52, 8) = str(cprice$,,)
            str(list$(r%),61, 8) = str(ext$,,)
            str(list$(r%),70,10) = str(extprice$,,)
            goto L30720  /* read next record */

L30900:     call "CONVERT" (tot, 2.2, total$)
            str(list$(r% + 1%),45, ) = "Total Built Up Price = "
            str(list$(r% + 1%),70,10) = total$

            if dollars(4) = tot then return
            adj = dollars(4) - tot

            call "CONVERT" (adj, 2.2, adj$)
            str(list$(r% + 2%),45, ) = "Manual Adjustment    = "
            str(list$(r% + 2%),70,10) = adj$

            str(list$(r% + 3%),45, ) = "Total Adjusted Price = "
            str(list$(r% + 3%),70,10) = price$
            return

        REM *************************************************************~
            *      R E S E L E C T I O N   C O N T R O L L E R          *~
            *-----------------------------------------------------------*~
            * This is the messy stuff. At this point, the user has      *~
            * selected a replacement part.  They want to reselect       *~
            * this option.  First we have to delete BOMSPEC records     *~
            * for any and all dependent selections at lower bill levels *~
            * and then redisplay the options list and let them choose   *~
            * the new replacement from the list.  So far fairly easy.   *~
            * Things get hairy if the new replacement they choose       *~
            * happens to have a bill and is a generic part.  In such    *~
            * cases we must restart the selection process from that     *~
            * new replacement part.  However, because of the X and Y    *~
            * coordinates in the BOMSPEC we have to re-trace the whole  *~
            * selection process to the point of the new replacement to  *~
            * load the COLUMN%() array.  Then, as the user selects new  *~
            * options below the new replacement, we need to shift       *~
            * the coordinates for existing BOMSPEC records to make room *~
            * in the COLUMN%() series just before we insert each new    *~
            * selection into BOMSPEC. We continue to select options     *~
            * as normal until we get back up to the restart point.      *~
            * And oh, by the way... be sure to make the proper          *~
            * adjustments to quantities, costs, prices as we go. Enjoy! *~
            *************************************************************

        reselect_control
            if optn$(idx%(c%)) <> "R" then explode_bom_control
            init(hex(00)) plowkey$
            up% = 0%

*       Is the parent of this replacement the top level part?  Else...
            if indent%(idx%(c%)) <> 1% then L31150
            str(plowkey$,1,25) = str(assypart$,1,25)
            str(plowkey$,26,3) = str(inbomid$,1,3)
            goto L31290

L31150
*       Serious trickery to get the parent of the selected option part
            for up% =  max(1%, c% - 1%) to 1% step -1
               if plus$(idx%(up%)) <> "-" then L31190
               if indent%(idx%(up%)) < indent%(idx%(c%)) then L31220
L31190:     next up%

*        Now set up the key to read the right record out of BOMSPEC
L31220:     str(plowkey$,1,25) = str(plist$(1%, idx%(up%)),1,25)
            str(plowkey$,26,3) = str(ebom$(idx%(up%)),1,3)

            if rlist$(1%, idx%(up%)) = " "  then L31290
                str(plowkey$,1,25) = str(rlist$(1%, idx%(up%)),1,25)
                str(plowkey$,26,3) = str(rbom$(idx%(up%)),1,3)

L31290:     str(plowkey$,29,3) = str(seq$(idx%(c%)),1,3)
            str(plowkey$,32,19) = str(demand$,1,19)
            put plowkey$, using L31320, indent%(idx%(c%))
L31320:        FMT POS(51), BI(2)

*       Now, clear out the BOMSPEC record for this replacement
L31350:     call "PLOWNXT1" (#7, plowkey$, 52%, f1%(7))
               if f1%(7) = 0% then halt_reselect /* rats!, missed it */
               get #7, using L31390, rep_part$,parent$, pseq$, row%, col%,~
                       selected_part$, qn, sz, cbomid$, ext1, ext2, pseq2$
L31390:           FMT CH(25), CH(28), CH(3), POS(76),  2*BI(2),          ~
                       CH(25), 2*PD(14,4), CH(3), XX(1), 2*PD(14,4), CH(3)
            if up% = 0% then L31410
            if idx%(up%) = 0% then L31410
            if pseq2$ <> seq$(idx%(up%)) then L31350
L31410:     delete #7

*       Adjust selling price if top level component
            if indent%(idx%(c%)) <> 1% then L31420
            component$ = selected_part$
            gosub subtract_price

L31420
*       Reset the quantity, adjust cost, get the BOM ID and misc.
            save_seq$ = pseq2$  /* PSEQ2$ to be reset after '8 */
            b_qn, b_sz = 1 /* Just in case. Should always get a hit */
            call "READ100" (#5, parent$ & pseq$, f1%(5))
                if f1%(5) = 0% then L31462
            get #5, using L31460, b_qn, b_sz   /* Bom quantities */
L31460:         FMT POS(57), 2*PD(14,4)       /* for use later. */

L31462:     call "READ100" (#4, selected_part$, f1%(4))
                if f1%(4) = 0% then L31488  /* non-stocked part */
            get #4, using L31465, s_type$
L31465:         FMT POS(180),CH(3)
            if s_type$ <> "000" then L31488 /* back out the standard now*/
                calc_cost = 0
        REM Since this is a generic part, we can't just back out the     ~
            standard cost.  Instead, we add up the cost of all its       ~
            components, along with the value added by it and generics    ~
            below it and then back out this total calculated cost.
                call "STCCOSTS" (selected_part$, " ", #10, 3%, cstccost, ~
                                 sum(), bom(), rte(), dtl())
                mat rte = rte + dtl : value_added = 0
                for i% = 1% to 12%
                     value_added = value_added + rte(i%)
                next i%
                calc_cost = (ext1*value_added)
                goto L31492

*       Back the cost of this part out of the total standard cost
L31488:     gosub'200(selected_part$, rep_part$, ext1, ext2)

*       Determine the bill of materials version for this part
L31492:     gosub'190(selected_part$)
            init(hex(00)) componentkey$, savekey$

            componentkey$ = str(selected_part$,,25) & str(bomid$,,3)
            savekey$ =  componentkey$

*       Now, delete dependent selections below the part if there are any
            if str(componentkey$,26,3) = " " then L31620 /* no bom so    */
                                                        /* nothing below*/
            gosub loop_setup
            pl% = indent%(idx%(c%))
            sq$(pl%) = pseq$
            gosub'8(componentkey$, ext1)      /* go into deletion loop */
            str(componentkey$,,28) = str(savekey$,,28)
            init(hex(00)) savekey$
            if s_type$ <> "000" then L31620

*       Back out calculated cost of this generic replacement
            dollars(2) = dollars(2) - calc_cost
            cstccost = 0
*        Add in standard cost of old part
            call "READ100" (#4, rep_part$, f1%(4))
            if f1%(4) = 0 then L31613
            call "STCCOSTS" (rep_part$, " ", #10, 1%, cstccost)
L31613:     dollars(2) = dollars(2) + (ext2*cstccost)

L31620
*       Now, get ready and then go show the user the list of options
            qn = b_qn
            sz = b_sz
            pseq2$ = save_seq$
            pl% = row%
            ext(pl%) = ext2 / qn * sz
            extqty = ext2
            l%  = pl%               /*  L% = MAX(1, PL% - P%) */
            column%(l%) = col%
            component$  = rep_part$
            p$(pl%), assy$ = str(parent$,1,25)
            id$ = str(parent$,26,3)
            seq$ = pseq$
            force_manual% = 1%  /* Let 'em pick, since they selected one*/
            gosub selection_screen_control
            if str(parent$,,25) <> assypart$ then L31720
            gosub add_price
L31720:     force_manual% = 0%  /* Use defaults below, if there are any */

*       Test to see if their selection implies additional selections
            if str(componentkey$,26,3) = " " then end_reselect
            call "READ100" (#4, str(componentkey$,,25), f1%(4))
                if f1%(4) = 0% then end_reselect
            get #4, using L31780, type$
L31780:         FMT XX(179), CH(3)
            if type$ <> "000" then end_reselect

        REM Some fun begins right here.  SAVEKEY$ holds BOMSPEC key for  ~
            the replacement just selected.  We now reselect below it.    ~
            We have to start at the very top and re-process all the      ~
            selections leading up to the point at which reselection begin~
            in order to pre-load the coordinates.  When we reach the     ~
            starting point, we indicate it by setting RA% = 1%.  We then ~
            reselect below that point, and end when we rise back up.

            gosub loop_setup
            pl% = 0%
            componentkey$ = " "
            ra% = 0%       /* set Reselect Active flag to off  */
            gosub'9(assypartkey$, 1)  /* go into reselection loop */

        end_reselect
            if pcode$ < hex(3a) then gosub price_lookup /*gosub if numbr*/
            goto explode_bom_control

        loop_setup
            l%, p%, options% = 0%
            mat column% = zer
            init(" ") line$(), p$(), m$(), sq$()
            return

        halt_reselect
            k% = 2%
            call "ASKUSER" (k%, "* * *  UNABLE TO CONTINUE * * *",       ~
                 "Cannot proceed with reselection from this point",      ~
                 "From the menu, press PF 1 and start selection over",   ~
                 "Press any Key to Acknowledge.")
            goto menu_screen_control

        REM *************************************************************~
            *   D E L E T I O N    L O O P   F O R   R E S E L E C T    *~
            * Here we clear out the BOMSPEC records for the options     *~
            * below the point at which they have chosen to reselect.    *~
            *************************************************************

        deffn'8(partkey$, extqty)
            pl% = pl% + 1%           /* PL% is actual level      */
            l% = max(1, pl% - p%)    /* L% is the 'Pseudo-Level' */
            p$(pl%) = partkey$
            ext(pl%) = extqty
            if extqty <= 0 then L32680

L32220:     call "PLOWNEXT" (#5, p$(pl%), 28%, f1%(5))
                if f1%(5) = 0% then L32680

*        Load From BOMMASTR
                if str(p$(pl%),29,3) = "  0" then L32220 /* skip header */
                get #5, using L32290, component$,assy$,id$,seq$,qn,sz,    ~
                                     fixed,over,bommkr$,op$,cbomid$
L32290:         FMT 2*CH(25),2*CH(3),4*PD(14,4),CH(2),CH(1),CH(3)
                sq$(pl%) = seq$
                m$(pl%) = bommkr$
                call "READ100"(#4, component$, f1%(4))
                     if f1%(4) = 0% then L32220 /* non-stocked part */
                get #4, using L32350, type$
L32350:         FMT XX(179), CH(3)
                extqty = qn * sz + over
                extqty = round((extqty*ext(pl%)) + fixed, 2)
                if op$ = "Y" or type$ = "000" then L32370
               /* add in the cost of this component */
                call "STCCOSTS" (component$, " ", #10, 1%, cstccost)
                calc_cost = calc_cost + (extqty*cstccost)
                goto L32220 /* Move On */

L32370:         gosub'190(component$)  /* Find effect. BOM for Component*/
                componentkey$ = str(component$,,25) & str(bomid$,,3)

                if op$ = "Y" then L32410
            /* determine the value added for this part and include it */
                     gosub tally_value_added
                     goto L32630  /* Option is somewhere below this 000.*/

L32410:        column%(l%)=column%(l%)+1% /* Count Of Hits This Levl*/
               init(hex(00)) plowkey$

                  str(plowkey$,1,28) = str(p$(pl%),,28)
                  str(plowkey$,29,3) = str(seq$,,3)
                  str(plowkey$,32,19) = str(demand$,1,19)
                  put plowkey$, using L32480, pl%
L32480:              FMT POS(51), BI(2)

L32500:           call "PLOWNXT1" (#7, plowkey$, 52%, f1%(7))
                     if f1%(7%) = 0% then L32630  /*  what gives ?? */
                  get #7, using L32540, origpart$, component$, qn, sz ,   ~
                           cbomid$, pseq2$
L32540:               FMT CH(25), POS(80), CH(25), 2*PD(14,4), CH(3),    ~
                          POS(141), CH(3)
                  if pl% = 1% then L32560
                  if pseq2$ <> sq$(pl% - 1%) then L32500

L32560:           delete #7
                  extqty = qn * sz  /* + OVER */
                  extqty = round((extqty*ext(pl%)) /* + FIXED */, 2)
                  gosub tally_value_added
                  gosub'190(component$)  /* Find effect. BOM for Comp. */
                  componentkey$ = str(component$,,25) & str(bomid$,,3)

L32630:         if m$(pl%) = "P" then p% = p% + 1%
                if pl% < 30% then gosub'8(componentkey$, extqty)
                                          /* Do Comps If Not At Bottom */
                goto L32220 /* plow for next component at this level */

L32680:         REM END ROUTINE GRACEFULLY.
                    pl% = pl% - 1%
                    if pl% = 0% then goto L32720/* Avoid M$(PL%) blow-up */
                        if m$(pl%) = "P" then p% = p% - 1%
L32720:             l% = max(1, pl% - p%)   /* L% Is The 'Pseudo-Level'*/
            return /* Eventually This Will Return Completely Out Of '8  */

        tally_value_added
            call "STCCOSTS" (component$, " ", #10, 3%, cstccost,         ~
                                      sum(), bom(), rte(), dtl())
            mat rte = rte + dtl : value_added = 0
                for i% = 1% to 12%
                     value_added = value_added + rte(i%)
                next i%
            calc_cost = calc_cost + (extqty*value_added)
            return

        REM *************************************************************~
            *  R E S E L E C T I O N   L O O P   F O R   R E S E L E C T*~
            * Here we prompt the user to reselect options at and below  *~
            * the chosen point in the BOM.                              *~
            *************************************************************

        deffn'9(partkey$, extqty)
            pl% = pl% + 1%           /* PL% is actual level      */
            l% = max(1, pl% - p%)    /* L% is the 'Pseudo-Level' */
            p$(pl%) = partkey$
            ext(pl%) = extqty
            if extqty <= 0 then L33370  /* end */

L32890:     call "PLOWNEXT" (#5, p$(pl%), 28%, f1%(5))
                if f1%(5) = 0% then L33370

*        Load From BOMMASTR
                if str(p$(pl%),29,3) = "  0" then L32890 /* skip header */
                get #5, using L32960, component$,assy$,id$,seq$,qn,sz,    ~
                                     fixed,over,bommkr$,op$,cbomid$
L32960:         FMT 2*CH(25),2*CH(3),4*PD(14,4),CH(2),CH(1),CH(3)
                sq$(pl%) = seq$
                m$(pl%) = bommkr$
                call "DESCRIBE"(#4, component$, cdesc$, 1%, f1%(4))
                     if f1%(4) = 0% then L32890 /* non-stocked part */
                get #4, using L33020, type$
L33020:         FMT XX(179), CH(3)
                if op$ <> "Y" and type$ <> "000" then L32890 /* Move On */
                gosub'190(component$)  /* Find effect. BOM for Component*/
                componentkey$ = str(component$,,25) & str(bomid$,,3)
                extqty = qn * sz + over
                extqty = round((extqty*ext(pl%)) + fixed, 2)
                if op$ <> "Y" then L33320  /* Option is somewhere below.*/
                  column%(l%)=column%(l%)+1% /* Count Of Hits This Levl*/

                  if ra% = 1% then L33290 /* prompt for selection if we */
                                         /* are actively reselecting...*/
                /* else we read from BOMSPEC and load the selection.   */
                  init(hex(00)) readkey$
                  str(readkey$,1,28) =  str(p$(pl%),,28)
                  str(readkey$,29,3) =  str(seq$,,)
                  str(readkey$,32,19) =  str(demand$,,19)
                  put readkey$, using L33190, l%, column%(l%) - 1%
L33190:              FMT POS(51), BI(2), BI(2)

            /* We use a plow in the event that previous re-selects  */
            /* have mucked up the sequence numbers.                 */

L33220:        call "PLOWNEXT" (#7, readkey$, 52%, f1%(7))
               if f1%(7%) = 0% then halt_reselect
                     get #7, using L33240, column%(l%), ref_key$, pseq2$
L33240:                   FMT POS(78), BI(2), CH(25), POS(141), CH(3)
               if pl% = 1% then L33250
               if pseq2$ <> sq$(pl% - 1%) then L33220

L33250:              gosub set_component
               if str(readkey$,1,54) <> str(savekey$,1,54) then L33320
                    ra% = 1%  /* we are at the reselection point */
                    stopping_point% = pl%
                    goto L33320

L33290:         gosub  shift_coordinates
                pseq2$ = " "
                if pl% > 1% then pseq2$ = sq$(pl% - 1%)
                gosub  selection_screen_control

L33320:         if m$(pl%) = "P" then p% = p% + 1%
                if pl% < 30% then gosub'9(componentkey$,extqty)
                                          /* Do Comps If Not At Bottom */
                goto L32890

L33370:         REM END ROUTINE GRACEFULLY.
                    pl% = pl% - 1%
                    if ra% = 0%  then L33440
                    if pl% <> stopping_point% then L33440
                          return clear all
                          goto end_reselect /* we're done */

L33440:             if pl% = 0% then goto L33460/* Avoid M$(PL%) blow-up */
                        if m$(pl%) = "P" then p% = p% - 1%
L33460:             l% = max(1, pl% - p%)   /* L% Is The 'Pseudo-Level'*/
            return /* Eventualy This Will Return Completely Out Of '9  */


        REM *************************************************************~
            *   S H I F T    T H E   C O O R D I N A T E S              *~
            * Whenever we insert new selections in the BOMSPEC file     *~
            * as a result of the reselection process, we have to slide  *~
            * the column% seq numbers over to insure that there is      *~
            * room in the sequence so the new part lines up properly    *~
            * with the previous BOMSPEC records.  We do this by reading *~
            * BOMSPEC, write a record to workfile #6, then delete from  *~
            * BOMSPEC, read next record, etc.  When this is done, we    *~
            * then read from the workfile, increment the sequence and   *~
            * write to BOMSPEC.  When done, we call delete on the WF.   *~
            *************************************************************

        shift_coordinates
            rf% = 0%      /* records found */
            init(hex(00)) plowkey$
            str(plowkey$,1,19) = str(demand$,,)
            put plowkey$, using L33680, pl%, column%(pl%) - 1%
L33680:         FMT POS(20), BI(2), BI(2)

L33700:     call "PLOWAL1" (#7, plowkey$, 1%, 21%, f1%(7))
                if f1%(7) <> 0% then L33750
                if rf% = 0% then return
                goto read_wf

L33750:     get #7, using L33760, record$
L33760:         FMT CH(150)
            delete #7

            if f2%(6) = 0% then L33820
               call "WORKOPEN" (#6, "SHARE", 100%, f2%(6))

L33820:     write #6, using L33760, record$
            rf% = rf% + 1%
            goto L33700

        read_wf  /* restore records to BOMSPEC from workfile */
            init (hex(00)) plowkey$
                call "PLOWNEXT" (#6, plowkey$, 0%, f1%(6))
                goto L33880
L33870:     call "READNEXT" (#6, f1%(6))
L33880:         if f1%(6) = 0% then L33970
                get #6, using L33760, record$
                     get record$, using L33910, seq%
L33910:                  FMT POS(78), BI(2)
                     seq% = seq% + 1%  /* increment counter as we go */
                     put record$, using L33910, seq%
                write #7, using L33760, record$
            goto L33870

L33970:     call "DELETE" (#6, " ", 0%)
            return

        REM *************************************************************~
            *   O P T I O N   S E L E C T I O N   S C R E E N           *~
            *-----------------------------------------------------------*~
            * See the list of replacements and pick one.                *~
            *************************************************************

        selection_screen
              gosub set_pf1
              str(headline$(1%),20, 25) = "Select Option Replacement"
              colhead$(1%) = "Replacement Parts"
              colhead$(2%) = "Descriptions"

L40110:     accept                                                       ~
               at (01,02), fac(hex(8c)), headline$(1%)          , ch(79),~
               at (02,02), fac(hex(8c)), headline$(2%)          , ch(79),~
               at (03,02), fac(hex(ac)), headline$(3%)          , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Parent: ",                                   ~
               at (05,10), fac(hex(8c)), p$(pl%)                , ch(25),~
               at (05,36), fac(hex(8c)), pdesc$                 , ch(34),~
                                                                         ~
               at (05,73), "BOM: ",                                      ~
               at (05,78), fac(hex(8c)), id$                    , ch(03),~
                                                                         ~
               at (06,02), "Option: ",                                   ~
               at (06,10), fac(hex(8c)), component$             , ch(25),~
               at (06,36), fac(hex(8c)), cdesc$                 , ch(34),~
                                                                         ~
               at (08,03), "List of Option Replacements:",               ~
               at (09,06), fac(hex(ac)), colhead$(1)            , ch(25),~
               at (09,32), fac(hex(ac)), colhead$(2)            , ch(32),~
                                                                         ~
               at (10,06), fac(hex(8e)), altpart$(c% + 01%)     , ch(25),~
               at (11,06), fac(hex(8e)), altpart$(c% + 02%)     , ch(25),~
               at (12,06), fac(hex(8e)), altpart$(c% + 03%)     , ch(25),~
               at (13,06), fac(hex(8e)), altpart$(c% + 04%)     , ch(25),~
               at (14,06), fac(hex(8e)), altpart$(c% + 05%)     , ch(25),~
               at (15,06), fac(hex(8e)), altpart$(c% + 06%)     , ch(25),~
               at (16,06), fac(hex(8e)), altpart$(c% + 07%)     , ch(25),~
               at (17,06), fac(hex(8e)), altpart$(c% + 08%)     , ch(25),~
               at (18,06), fac(hex(8e)), altpart$(c% + 09%)     , ch(25),~
               at (19,06), fac(hex(8e)), altpart$(c% + 10%)     , ch(25),~
                                                                         ~
               at (10,32), fac(hex(8c)), altdesc$(c% + 01%)     , ch(32),~
               at (11,32), fac(hex(8c)), altdesc$(c% + 02%)     , ch(32),~
               at (12,32), fac(hex(8c)), altdesc$(c% + 03%)     , ch(32),~
               at (13,32), fac(hex(8c)), altdesc$(c% + 04%)     , ch(32),~
               at (14,32), fac(hex(8c)), altdesc$(c% + 05%)     , ch(32),~
               at (15,32), fac(hex(8c)), altdesc$(c% + 06%)     , ch(32),~
               at (16,32), fac(hex(8c)), altdesc$(c% + 07%)     , ch(32),~
               at (17,32), fac(hex(8c)), altdesc$(c% + 08%)     , ch(32),~
               at (18,32), fac(hex(8c)), altdesc$(c% + 09%)     , ch(32),~
               at (19,32), fac(hex(8c)), altdesc$(c% + 10%)     , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40550
                  call "MANUAL" ("BOMOPSUB") : goto L40110

L40550:        if keyhit% <> 15% then L40580
                  call "PRNTSCRN" : goto L40110

L40580:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1

            inpmessage$ = "Position the Cursor and Press RETURN to "  &  ~
                          "Select the Replacement."

            pf$(1) = "(1)Start Over (Undo Selections & Return " &        ~
                     "to S.O.)               (13)Instructions"
            pf$(2) = "(2)Restart Selection Process, Honoring D" &        ~
                     "efaults                (15)Print Screen"
            pf$(3) = "(3)Restart Selection Process, Pick All M" &        ~
                     "anually (4)Prev (5)Nxt (16)Exit to S.O."
            pfkeys$ = hex(0102030405ffffffffffffff0dff0f1000)
            if c% > 0% then L40730
                str(pf$(3),49,07)  = " "  :  str(pfkeys$,4,1) = hex(ff)
L40730:     if c% + 10% <= alt% then L40745
                str(pf$(3),57,07) = " "  :  str(pfkeys$, 5,1) = hex(ff)
L40745:     if bomspec_is_empty% = 1% then return
            str(pf$(3),64,16)  = " "  :  str(pfkeys$,16,1) = hex(ff)
            return


        REM *************************************************************~
            *  A C C E P T   O R   C H A N G E   Q U A N T I T Y        *~
            *-----------------------------------------------------------*~
            * Review and accept or edit quantity and times used fields. *~
            *************************************************************

        quantity_screen

            call "CONVERT"(fixed, 2.2, fixed$)
            call "CONVERT"(over, 2.2, over$)

            gosub set_pf2
            str(headline$(1%),20, 27) = "Accept/Edit Replacement Quantity"
            colhead$() = " "
L41050:     accept                                                       ~
               at (01,02), fac(hex(8c)), headline$(1%)          , ch(79),~
               at (02,02), fac(hex(8c)), headline$(2%)          , ch(79),~
               at (03,02), fac(hex(ac)), headline$(3%)          , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Parent: ",                                   ~
               at (05,10), fac(hex(8c)), p$(pl%)                , ch(25),~
               at (05,36), fac(hex(8c)), pdesc$                 , ch(34),~
                                                                         ~
               at (05,73), "BOM: ",                                      ~
               at (05,78), fac(hex(8c)), id$                    , ch(03),~
                                                                         ~
               at (06,02), "Option: ",                                   ~
               at (06,10), fac(hex(8c)), component$             , ch(25),~
               at (06,36), fac(hex(8c)), cdesc$                 , ch(34),~
                                                                         ~
               at (08,03), "Selected Replacement Part:",                 ~
                                                                         ~
               at (10,06), fac(hex(8c)), str(ref_key$,,25)      , ch(25),~
               at (11,08), fac(hex(8c)), ref_descr$             , ch(34),~
                                                                         ~
               at (10,30), "Quantity:"                          ,        ~
               at (10,40), fac(hex(82)), quan$                  , ch(10),~
               at (10,55), "Times Used:"                        ,        ~
               at (10,68), fac(hex(82)), size$                  , ch(10),~
                                                                         ~
               at (13,25), "Added Overage:",                             ~
               at (13,40), fac(hex(8c)), over$                  , ch(10),~
               at (13,52), "Fixed Qty/Run:",                             ~
               at (13,68), fac(hex(8c)), fixed$                 , ch(10),~
                                                                         ~
               at (15,25), "Unit of Measure:",                           ~
               at (15,42), fac(hex(8c)), uom$                   , ch(04),~
               at (15,47), fac(hex(8c)), uomdescr$              , ch(34),~
                                                                         ~
               at (17,06), "Note: Defaults for quantity and factors are",~
               at (17,50), "from the Option part shown ",                ~
               at (18,06), "above as it appears in the Bill for the ",   ~
               at (18,46), "Parent part shown above.",                   ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L41560
                  call "MANUAL" ("BOMOPSUB") : goto L41050

L41560:        if keyhit% <> 15% then L41610
                  call "PRNTSCRN" : goto L41050

L41610:        return

        set_pf2

            inpmessage$ = "Accept the Default Quantities From BOM o"  &  ~
                          "r Edit Them; Press RETURN to continue."

            pf$(1) = "                (4)Previous Screen      " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(ffffff04ffffffffffffffff0dff0fff00)

            call "PLOWALTS" (#7, demand$, 1%, 19%, f1%(7))
                 if f1%(7%) = 0% then return
            str(pf$(3),64,16)  = " "  :  str(pfkeys$,16,1) = hex(ff)
            return

        REM *************************************************************~
            *               M A I N   M E N U   S C R E E N             *~
            *-----------------------------------------------------------*~
            * The center of the universe.  Congradulations, you made it!*~
            *************************************************************

        deffn'103(fieldnr%)
            init(hex(8c)) lfac$() /* monetary fields dim, protected*/

            on fieldnr% gosub  L42030,   /* Show the menu           */    ~
                               L42040,   /* Edit Profit Margin %    */    ~
                               L42050    /* Edit Selling Price      */
            goto L42060

L42030:              init(hex(84)) lfac$() /* all bright, protected*/
                     return

L42040:              init(hex(82)) lfac$(2) /* enable percent field only*/
                     return

L42050:              init(hex(82)) lfac$(3) /* enable price field only */
                     return

L42060:     colhead$(), price_msg$ = " "
            str(headline$(1%),20, 29) = "Menu                         "
            gosub set_pf3
            keyhit% = 0%

L42070:     accept                                                       ~
               at (01,02), fac(hex(8c)), headline$(1%)          , ch(79),~
               at (02,02), fac(hex(8c)), headline$(2%)          , ch(79),~
               at (03,02), fac(hex(ac)), headline$(3%)          , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,03), fac(hex(84)), confirmation$          , ch(75),~
                                                                         ~
               at (08,20), "( 9) Review Replacements List           ",   ~
               at (09,20), "(10) Review BOM as Selected (& Reselect)",   ~
               at (10,20), "(11) Edit Profit Margin                 ",   ~
               at (11,20), "(12) Edit Selling Price                 ",   ~
               at (12,20), "(14) Print List of Options Selected    ",    ~
               at (13,20), "(16) Save Selections & Return to S.O.  ",    ~
               at (14,20), "(17) Save Configuration for Future Resale",  ~
               at (15,20), "(18) Print Cost Audit Report",               ~
               at (16,20), fac(hex(8c)) , pf19_prompt$          , ch(32),~
                                                                         ~
               at (18,12), "Unit Cost with Options Selected: ",          ~
               at (18,56), fac(lfac$(1)), cost$                 , ch(10),~
                                                                         ~
               at (19,12), "Profit Margin:                      ",       ~
               at (19,56), fac(hex(8c)) , margin$               , ch(10),~
               at (19,68), fac(lfac$(2)), percent$              , ch(06),~
               at (19,75), "%"                                  ,        ~
               at (20,12), "Selling Price Per Unit"              ,       ~
               at (20,35), fac(hex(8c)) , price_msg$            , ch(20),~
               at (20,56), fac(lfac$(3)), price$                , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L42400
                  call "MANUAL" ("BOMOPSUB") : goto L42070

L42400:        if keyhit% <> 15% then L42430
                  call "PRNTSCRN" : goto L42070

L42430:        return

        set_pf3

            inpmessage$, pf19_prompt$ = " "

            if pcode$ < hex(3a) then L42490
            price_msg$ = "(Built Up From Base)"
            if dollars(4) <> blt_price then price_msg$ = "(Manually Set)"
            goto L42500

L42490:     price_msg$ = "(Per Price Code " & pcode$ & ")"
            if dollars(4) <> cpr_price then price_msg$ = "(Manually Set)"

L42500:     pf$(1) = "(1)Startover (Undo Selections & Return t" &        ~
                     "o Sales Order)         (13)Instructions"
            pf$(2) = "(2)Restart Selection Process, Honoring D" &        ~
                     "efaults                (15)Print Screen"
            pf$(3) = "(3)Restart Selection Process, Pick All M" &        ~
                     "anually                (16)Save Data   "
            pfkeys$ = hex(010203ffffffffff090a0b0c0d0e0f101112ff00)

*          IF PRICE_MSG$ = "(Manually Set)" THEN RETURN
            if pcode$ < hex(3a) then return
            pf19_prompt$ = "(19) Show Price Build-up Details"
            str(pfkeys$,19,1) = hex(13)

            return

        REM *************************************************************~
            *   L I S T   S C R E E N                                   *~
            *-----------------------------------------------------------*~
            * Multi-purpose screen for review of list of options        *~
            * selected and for review of price build-up audit.          *~
            *************************************************************

        list_screen

              gosub set_pf4


L43120:     accept                                                       ~
               at (01,02), fac(hex(8c)), headline$(1%)          , ch(79),~
               at (02,02), fac(hex(8c)), headline$(2%)          , ch(79),~
               at (03,02), fac(hex(ac)), headline$(3%)          , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)), list_head$             , ch(79),~
                                                                         ~
               at (06,02), fac(hex(8c)),    list$(c% + 01%)     , ch(79),~
               at (07,02), fac(hex(8c)),    list$(c% + 02%)     , ch(79),~
               at (08,02), fac(hex(8c)),    list$(c% + 03%)     , ch(79),~
               at (09,02), fac(hex(8c)),    list$(c% + 04%)     , ch(79),~
               at (10,02), fac(hex(8c)),    list$(c% + 05%)     , ch(79),~
               at (11,02), fac(hex(8c)),    list$(c% + 06%)     , ch(79),~
               at (12,02), fac(hex(8c)),    list$(c% + 07%)     , ch(79),~
               at (13,02), fac(hex(8c)),    list$(c% + 08%)     , ch(79),~
               at (14,02), fac(hex(8c)),    list$(c% + 09%)     , ch(79),~
               at (15,02), fac(hex(8c)),    list$(c% + 10%)     , ch(79),~
               at (16,02), fac(hex(8c)),    list$(c% + 11%)     , ch(79),~
               at (17,02), fac(hex(8c)),    list$(c% + 12%)     , ch(79),~
               at (18,02), fac(hex(8c)),    list$(c% + 13%)     , ch(79),~
               at (19,02), fac(hex(8c)),    list$(c% + 14%)     , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L43630
                  call "MANUAL" ("BOMOPSUB") : goto L43120

L43630:        if keyhit% <> 15% then L43650
                  call "PRNTSCRN" : goto L43120
L43650:        return

        set_pf4

            inpmessage$ = "Press PF 16 to Return to the Menu"

            pf$(1) = "                                        " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(4)Previous   (5)Next                   " &        ~
                     "                       (16)To Menu     "
            pfkeys$ = hex(ffffff0405ffffffffffffff0dff0f1000)
            if c% > 0% then L43860
                str(pf$(3),1,12)  = " "  :  str(pfkeys$,4,1) = hex(ff)
L43860:     if c% < (r% - 14%) then L43870
                str(pf$(3),14,12)  = " "  :  str(pfkeys$,5,1) = hex(ff)
L43870:     return

        REM *************************************************************~
            * E X P L O D E   T H E   B I L L   O F   M A T E R I A L S *~
            *-----------------------------------------------------------*~
            * Allow the user to drill down the bill and reselect options*~
            *************************************************************

        display_explosion_screen

              gosub set_pf5

*            INIT(HEX(8C)) LFAC$()

L44085:     accept                                                       ~
               at (01,02), fac(hex(8c)), headline$(1%)          , ch(79),~
               at (02,02), fac(hex(8c)), headline$(2%)          , ch(79),~
               at (03,02), fac(hex(ac)), headline$(3%)          , ch(79),~
                                                                         ~
               at (04,02), fac(hex(ac)), colheader$             , ch(79),~
               at (05,02), fac(lfac$( 1%)), scrn$(       1%)    , ch(79),~
               at (06,02), fac(lfac$( 2%)), scrn$(       2%)    , ch(79),~
               at (07,02), fac(lfac$( 3%)), scrn$(       3%)    , ch(79),~
               at (08,02), fac(lfac$( 4%)), scrn$(       4%)    , ch(79),~
               at (09,02), fac(lfac$( 5%)), scrn$(       5%)    , ch(79),~
               at (10,02), fac(lfac$( 6%)), scrn$(       6%)    , ch(79),~
               at (11,02), fac(lfac$( 7%)), scrn$(       7%)    , ch(79),~
               at (12,02), fac(lfac$( 8%)), scrn$(       8%)    , ch(79),~
               at (13,02), fac(lfac$( 9%)), scrn$(       9%)    , ch(79),~
               at (14,02), fac(lfac$(10%)), scrn$(      10%)    , ch(79),~
               at (15,02), fac(lfac$(11%)), scrn$(      11%)    , ch(79),~
               at (16,02), fac(lfac$(12%)), scrn$(      12%)    , ch(79),~
               at (17,02), fac(lfac$(13%)), scrn$(      13%)    , ch(79),~
               at (18,02), fac(lfac$(14%)), scrn$(      14%)    , ch(79),~
               at (19,02), fac(lfac$(15%)), scrn$(      15%)    , ch(79),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L44405
                  call "MANUAL" ("BOMOPSUB") : goto L44085

L44405:        if keyhit% <> 15% then L44435
                  call "PRNTSCRN" : goto L44085

L44435:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf5

            inpmessage$ = "Cursor & RETURN at '+' to Explode or at '" &  ~
                          "-' to Collapse; PF 12 to Reselect."

            toggle$ = hex(84) & "(9)See All Components" & hex(8c)
            if show_all% = 1% then L44495
            toggle$ = hex(84) & "(9)See Options Only  " & hex(8c)

                                     /*  Input Mode             */
L44495:     pf$(1%)= "(1)Start Over                           " &        ~
                     "(8)Toggle Part/Descrip (13)Instructions"
            pf$(2%)= "(2)First  (4)Prev. Screen  (6)Up One   "  &        ~
                             toggle$ &     " (15)Print Screen"
            pf$(3%)= "(3)Last   (5)Next Screen   (7)Down One (" &        ~
                     "10)Collapse All        (16)To Menu     "
            pfkeys$ = hex(0102030405060708090aff0c0dff0f1000)

            if top% > 1% then L44605
                str(pf$(2),1,38)  = " "  :  str(pfkeys$,2,1) = hex(ff)
                str(pfkeys$,4,1) = hex(ff): str(pfkeys$,6,1) = hex(ff)
L44605:     if top% + 15% <= max%  then L44635
                str(pf$(3),1,38)  = " "  :  str(pfkeys$,3,1) = hex(ff)
                str(pfkeys$,5,1) = hex(ff): str(pfkeys$,7,1) = hex(ff)
L44635:     if pos(str(plus$()) = "-") <> 0% then L44655 /* collapsable?*/
                str(pf$(3),40,16) = " "  :  str(pfkeys$,10,1) = hex(ff)
L44655:     return


        REM *************************************************************~
            *          H E A D E R   F I L E   S C R E E N              *~
            *-----------------------------------------------------------*~
            * Input, review, and deletion of header file records        *~
            *************************************************************

        header_screen

              gosub set_pf6

L45180:     accept                                                       ~
               at (01,02), fac(hex(8c)), headline$(1%)          , ch(79),~
               at (02,02), fac(hex(8c)), headline$(2%)          , ch(79),~
               at (03,02), fac(hex(ac)), headline$(3%)          , ch(79),~
               at (05,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Order #/Line:",                              ~
               at (06,16), fac(hex(84)),  old_demand$           , ch(19),~
               at (07,02), "Order Date:",                                ~
               at (07,16), fac(lfac$(1)), old_date$             , ch(08),~
               at (08,02), "Description:",                               ~
               at (08,16), fac(lfac$(1)), config_descr$         , ch(60),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L45490
                  call "MANUAL" ("BOMOPSUB") : goto L45180

L45490:        if keyhit% <> 15% then L45540
                  call "PRNTSCRN" : goto L45180

L45540:        return

        set_pf6
            if edit% = 1% then L45990
            str(headline$(1%),20, 27) = "Input Configuration Info   "
            lfac$(1) = hex(81)
            inpmessage$ = "Enter or Change Date and Description for " &  ~
                          "this Configuration, then press RETURN."

                                     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f00ff)

            return

*       Edit Mode
L45990:     str(headline$(1%),20, 27) = "Review Configuration Info  "
            lfac$(1) = hex(8d)
            inpmessage$ = "Press RETURN to Enable Date and Descripti" &  ~
                          "on Fields for Edit."

                                     /*  Edit  Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                  (12)Delete Configurati" &        ~
                     "on Header              (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffff0c0dff0f1000)

            return
        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        test_header_data
            errormsg$ = " "
            od% = od%
            if old_date$ = " " or old_date$ = blankdate$ then return
            call "DATEOK"(old_date$, od%, errormsg$)
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            if returncode% = 0% then L65161
               mat dollars = zer
L65161:     call "FILEBGON" (#6)
            put #4, str(hnymastr_record$()) /* Restore Incoming HNYMASTR*/
            end
