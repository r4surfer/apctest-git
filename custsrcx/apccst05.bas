        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - APCCST05                             *~
            *  Creation Date     - 07/21/95                             *~
            *  Last Modified Date- 10/31/05                             *~
            *  Description       - Program to Explode the Cost of       *~
            *                      Materials Associated with a MFG      *~
            *                      Product and Create a Detailed        *~
            *                      Analysis Report.                     *~
            *                                                           *~
            *  Special Notes     - Hidden PF(9) Key Turns on the Debug  *~
            *                      for each Phase of Cost Calculation.  *~
            *                                                           *~
            *  Special Hook Area - Line No. 61950 = 3122080006040586794 *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *  Subroutines - (APCCST0B) - New Subroutine to Calculate   *~
            *                             the Material Requirements      ~
            *                             Associated with the Product   *~
            *                             and their costs. The Assoc.   *~
            *                             price is also Calculated.     *~
            *                                                           *~
            *                 (APCCST5B) - Sub to Calculate Labor       *~
            *                 (APCCST0B) - Sub to Calc and Consolidate  *~
            *                 (APCCST1B) - Sub to Calc MFG Raw Materials*~
            *                 (APCDCT2B) - Sub Calc Glass/Screen Matl's *~
            *                 (APCCST3B) - Sub to Calc Locks Matl's     *~
            *                 (APCCST4B) - Sub to Calc Hardware/Package *~
            *                 (APCCST9B) - Sub Used by all for Matl's   *~
            *                 (APCPRSUB) - Primary Pricing Calc Routine *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/02/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 10/02/95 ! Mod to Add Wood Surround and Factory Mull! RHH *~
            *          !   to Report.                             !     *~
            * 06/06/96 ! Mod to Add New Debug Screen Utility for  ! RHH *~
            *          !   Debugging Process.                     !     *~
            * 11/08/97 ! Mod to Add New File for Labor Subroutine ! RHH *~
            *          !   (APCCST5B) File = APCPLNDP             !     *~
	    * 02/13/98 ! Y2K Compliance				  ! DJD *~
            * 10/31/05 1 (AWD001) CR347 Mod for sub part          ! CMG *~
            * 01/11/06 ! (PAR000) CR347 mods for sub part         ! CMG *~
            *02/13/2019! CR-1894  Increase EMP DEPT size to 3     ! DES *~
            *************************************************************

        dim                              /* Report Variables           */~
            calc$1, calc_d$30,           /* Report Selection Code      */~
            bg_mod$25, bg_mod_d$32,      /* Beginning Model Code all   */~
            ed_mod$25, ed_mod_d$32,      /* Ending MOdel Code          */~
            store1$9, store1_d$30,       /* Customer Code              */~
            ss$(15%)40, prt_hdr$9,       /* Screen Descriptions        */~
            apc_key$25,                  /* GET PARTS KEY              */~
            adj_mat$6,                   /* ADJUSTMENT AMOUNT          */~
            apc_scr$120,                 /*                            */~
            apc_prt$60,                  /*                            */~
            apc_sze$20,                  /*                            */~
            part_desc$32,                /*                            */~
            mod$3                        /*                            */

        dim                              /* (Program) - Variables      */~
            title$40,date$8,hdr$(10%)9,  /* REPORT TITLE               */~
            runtime$8, code$3,           /* REPORT RUN TIME            */~
            readkey$50, desc$30,         /* Generic Key                */~
            cursor%(2%), tot(20%),       /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim                              /* Costing Variables          */~
            part$25,                     /* MFG Part Number            */~
/*PAR000*/  partno1$20,                  /* MFG Sub Part               */~
            tmp$(7%,50%)25,              /* Store all Raw Mat'l Part No*/~
            tmc(7%,50%),                 /* Assoc. Cut Inches In Dec.  */~
            tmct(7%,50%),                /* Assoc. Total Cost Raw Mat'l*/~
            tmu%(7%,50%),                /* Assoc. Unit of Measure     */~
            tmd$(7%,50%)32,              /* Assoc. Raw Mat'l Desc      */~
            tmuc(7%,50%),                /* Assoc. Raw Mat'l Unit Cost */~
            tmsi(7%,50%),                /* Assoc. Scrap Inches Decimal*/~
            tmsc(7%,50%),                /* Assoc. Scrap Mat'l Cost    */~
            tmeq$(7%,50%)3,              /* Assoc. Calc Type & Eq. No. */~
            tmph$(7%,50%)5,              /* Assoc. Phantom for Calc    */~
            tcnt%(7%),                   /* Assoc. Count for Each Type */~
            lb_typ$1, lb_typ_d$30,       /* LAB TYPE (S)tandard (A)ctua*/~
            lab(10%),                    /* Breakdown of Labor Cost    */~
            avg_pay(15%),                /* Avg Hourly Pay by Dept     */~
            uph(15%),                    /* Avg Units Per Manhour Dept */~
            tc(25%),                     /* Total Cost's               */~
            tt(25%),                     /* Cost Total Buckets         */~
            rm_mat(20%),                 /* Total Vinyl,Misc, Mat      */~
            rm_mats(20%),                /* Total Vinyl,Misc, Mat Scrap*/~
            apc_err%(20%), err$(20%)20,  /* Error Flag for Each Modul  */~
            apc_err$20,                  /* Print in Product Header    */~
            pc(36%)                      /* Calc Dealer Price Catalog  */

        dim f2%(20%),                    /* = 0 if the file is open    */~
            f1%(20%),                    /* = 1 if READ was successful */~
            fs%(20%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(20%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = " Costing Detail Analysis Report Utility "
            pname$ = "APCCST05 - Rev: R6.04"

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
            * #01 ! APCCUTEQ ! Saw Optimization Cross-Reference File    *~
            * #02 ! HNYMASTR ! Part Master File                         *~
            * #03 ! HNYQUAN  ! Inventory Quantities Master File         *~
            * #04 ! GENCODES ! Master Code Table File                   *~
            * #05 ! AMTBOMCD ! Master Equation File                     *~
            * #06 ! AMTBOMIF ! Master Part Validity File                *~
            * #07 ! APCEMPLY ! Employee Master File                     *~
            * #08 ! APCEQUAT ! Equation an Parts Cross Reference File   *~
            * #09 ! APCCSTHP ! Hardware and Packaging Costing Components*~
            * #10 ! APCCSTLR ! Departments Average Hourly Rates         *~
            * #11 ! CPRPRICE ! Master System Price File                 *~
            * #12 ! CUSTOMER ! Master Customer File                     *~
            * #13 ! APCPCMST ! Pricing Definition file                  *~
            * #14 ! APCSKUNO ! Home Center's Skuno File                 *~
            * #15 ! APCPCMSK ! Pricing Key Definition File              *~
            * #16 ! APCPCMSD ! Pricing Master Calc Definition File      *~
            * #17 ! APCCSTEX ! APC COSTING EXCEPTION FILE               *~
            * #18 ! APCSTOCK ! APC STOCK MASTER FILE                    *~
            * #19 ! APCPLNDP ! PLANNING MASTER DEPARTMENT FILE          *~
            * #20 ! APCCSTWK ! Costing Report Work File                 *~
            * #21 ! AWDPCMST ! Pricing Definition file     CR1894       *~            
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCCUTEQ",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    2, keylen =   7,                     ~
                        alt key  1, keypos  =     1, keylen =  8

            select #2,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  1,   keylen =  25,                     ~
                        alt key  1, keypos  =   102, keylen =  9, dup,   ~
                            key  2, keypos  =    90, keylen =  4, dup,   ~
                            key  3, keypos  =    26, keylen = 32, dup

            select #3,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =   650,             ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =  24

            select #5,  "AMTBOMCD",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42

            select #6,  "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32                      ~

            select #7,  "APCEMPLY",                                      ~
                        varc,     indexed,  recsize =  1024,             ~
                        keypos =    7, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =  11, dup,    ~
                            key  2, keypos  =  12, keylen =  26, dup

            select #8,  "APCEQUAT",                                      ~
                        varc,     indexed,  recsize =   16,              ~
                        keypos =    1, keylen =   8

            select #9,  "APCCSTHP",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  20

            select #10, "APCCSTLR",                                      ~
                        varc,     indexed,  recsize =  102,              ~
                        keypos =    1, keylen =  3

            select #11, "CPRPRICE"                                       ~
                        varc,     indexed,  recsize = 700,               ~
                        keypos = 1,    keylen =  47

            select #12, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =  1,   keylen =  9,                      ~
                        alt key  1, keypos  =    10, keylen = 30, dup,   ~
                            key  2, keypos  =   424, keylen =  9, dup,   ~
                            key  3, keypos  =   771, keylen =  9, dup,   ~
                            key  4, keypos  =   780, keylen =  9, dup

            select #13, "APCPCMST"                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 9,    keylen =  40,                     ~
                        alt key  1, keypos  =     1, keylen = 8

            select #14, "APCSKUNO"                                       ~
                        varc,     indexed,  recsize =  73,               ~
                        keypos = 1,    keylen =  28,                     ~
                        alt key  1, keypos  =    29, keylen = 28, dup

            select #15, "APCPCMSK"                                       ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos = 1,    keylen =   5

            select #16, "APCPCMSD"                                       ~
/*AWD001*/              varc,     indexed,  recsize =  768,              ~
                        keypos = 1,    keylen =   9

            select #17,  "APCCSTEX",                                     ~
                        varc,     indexed,  recsize = 1100,              ~
                        keypos =    1, keylen =    9

            select #18, "APCSTOCK",                                      ~
                        varc,     indexed,  recsize =  70,               ~
                        keypos =    1, keylen =  32,                     ~
                        alt key  1, keypos =   8, keylen = 32

            select #19, "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =    32,             ~
                        keypos =   11, keylen =  12,                     ~
                        alt key  1, keypos  =   9, keylen =  14,         ~
                            key  2, keypos  =   4, keylen =  12,         ~
                            key  3, keypos  =   1, keylen =  15

            select #20, "APCCSTWK",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    1, keylen =  25

            select #21, "AWDPCMST",                                      ~
	                varc,     indexed,  recsize =   128,                 ~
		            keypos =    9, keylen =  53,                         ~
		            alt key  1, keypos  =  1, keylen =  8
                
            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),  0%, rslt$(6%))
            call "OPENCHCK" (#7, fs%(7%), f2%(7%),  0%, rslt$(7%))
            call "OPENCHCK" (#8, fs%(8%), f2%(8%),  0%, rslt$(8%))
            call "OPENCHCK" (#9, fs%(9%), f2%(9%),  0%, rslt$(9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%),  0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%),  0%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%),  0%, rslt$(12%))
            call "OPENCHCK" (#13, fs%(13%), f2%(13%),  0%, rslt$(13%))
            call "OPENCHCK" (#14, fs%(14%), f2%(14%),  0%, rslt$(14%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%),  0%, rslt$(15%))
            call "OPENCHCK" (#16, fs%(16%), f2%(16%),  0%, rslt$(16%))
            call "OPENCHCK" (#17, fs%(17%), f2%(17%),  0%, rslt$(17%))
            call "OPENCHCK" (#18, fs%(18%), f2%(18%),  0%, rslt$(18%))
            call "OPENCHCK" (#19, fs%(19%), f2%(19%),  0%, rslt$(19%))
            call "OPENCHCK" (#21, fs%(21%), f2%(21%), 50%, rslt$(21%))            

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            ss$( 1%) = "****************************************"
            ss$( 2%) = "*  Costing Analysis Report Selections  *"
            ss$( 3%) = "*  (1) = All Areas                     *"
            ss$( 4%) = "*  (2) = Direct/Indirect Labor (Only)  *"
            ss$( 5%) = "*  (3) = Raw Materials (Lineals Only)  *"
            ss$( 6%) = "*  (4) = Raw Material (Glass Only)     *"
            ss$( 7%) = "*  (5) = Raw Materials (Screen Only)   *"
            ss$( 8%) = "*  (6) = Raw Materials (Locks Only)    *"
            ss$( 9%) = "*  (7) = Hardware Only                 *"
            ss$(10%) = "*  (8) = Packaging Only                *"
            ss$(11%) = "*  (9) = Pricing Info Only             *"
            ss$(12%) = "****************************************"

            hdr$(1%) = "Materials" : err$(1% ) = "(Error) In Labor Cal"
            hdr$(2%) = "Glass Mat" : err$(2% ) = "(Error) in Material "
            hdr$(3%) = "Screen Mt" : err$(3% ) = "(Error) in Glass    "
            hdr$(4%) = "Lock Matl" : err$(4% ) = "(Error) in Screen   "
            hdr$(5%) = "Hardware " : err$(5% ) = "(Error) in Locks    "
            hdr$(6%) = "Packaging" : err$(6% ) = "(Error) in Hardware "
            hdr$(10%)= "**Labor**" : err$(7% ) = "(Error) in Packaging"
                                     err$(8% ) = "(Error) in Pricing  "
                                     err$(9% ) = "                    "
                                     err$(10%) = "                    "
                                     err$(11%) = "                    "
                                     err$(12%) = "(Err) CSTM Material "
                                     err$(13%) = "                    "
                                     err$(14%) = "                    "
                                     err$(15%) = "                    "
                                     err$(16%) = "(Err) CSTM Hardware "
                                     err$(17%) = "(Err) CSTM Packaging"
                                     err$(18%) = "                    "
                                     err$(19%) = "                    "
                                     err$(20%) = "                    "
        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 6%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
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
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg1
L11140:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 6% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11190:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11190
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11190
                  lastfieldnr% = fieldnr%
            goto L11140

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_report
            if len(bg_mod$) > 10% then goto L19180
               mode% = 1% : gosub open_work
               mode% = 3% : gosub open_work
               gosub select_printer
               gosub generate_report
               close printer
               gosub delete_work
               goto L19210
L19180:     gosub select_printer
            gosub gen_rpt
            close printer
L19210: return clear all
        goto inputmode

        select_printer
            runtime$ = " "
            call "TIME" (runtime$)
            select printer (134)
            pageno% = 0%
            lcntr% = 99%
        return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return

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
         "Enter an Valid report Selection?                             ",~
         "Enter a Beginning Model Product Code or (ALL)?               ",~
         "Enter a Ending Model Product Code?                           ",~
         "Enter a Valid Terms Vinyl Discount Percent in Decimal?       ",~
         "Enter an (S)tandard or (A)ctual for the Costimg Method to Use",~
         "Enter a Valid Customer Code? Optional only used for Pricing. "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, readkey$, lb_typ$, desc$,  ~
                      lb_typ_d$, bg_mod$, bg_mod_d$, ed_mod$, ed_mod_d$, ~
                      calc$, calc_d$, store1$, store1_d$,                ~
                      adj_mat$, apc_err$
            debug% = 0%
        return

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
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
        REM DATALOAD

        REM RETURN

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT

        REM RETURN

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub set_pf1

              gosub'050(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40210,         /* Report Selection Code*/~
                                L40210,         /* Beginning Model Code */~
                                L40210,         /* Ending Model Code    */~
                                L40210,         /* Adjustment Amount    */~
                                L40210,         /* (S)td or (A)ctual Lab*/~
                                L40210          /* 1st Customer Code    */
              goto L40240

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40210:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40240:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Rpt Selection    :",                         ~
               at (03,21), fac(lfac$(1%)), calc$                , ch(01),~
               at (03,48), fac(hex(84)), calc_d$                , ch(30),~
                                                                         ~
               at (04,02), "Beg Model Code   :",                         ~
               at (04,21), fac(lfac$(2%)), bg_mod$              , ch(25),~
               at (04,48), fac(hex(84)), bg_mod_d$              , ch(32),~
                                                                         ~
               at (05,02), "End Model Code   :",                         ~
               at (05,21), fac(lfac$(3%)), ed_mod$              , ch(25),~
               at (05,48), fac(hex(84)), ed_mod_d$              , ch(32),~
                                                                         ~
               at (06,02), "Adjustment Amount:",                         ~
               at (06,21), fac(lfac$(4%)), adj_mat$             , ch(06),~
                                                                         ~
               at (07,02), "Lab-(S)td,(A)ct  :",                         ~
               at (07,21), fac(lfac$(5%)), lb_typ$              , ch(01),~
               at (07,48), fac(hex(84)), lb_typ_d$              , ch(30),~
                                                                         ~
               at (08,02), "Prc-Cust Code    :",                         ~
               at (08,21), fac(lfac$(6%)), store1$              , ch(06),~
               at (08,48), fac(hex(84)), store1_d$              , ch(30),~
                                                                         ~
               at (09,21), fac(hex(84)), ss$( 1%)               , ch(40),~
               at (10,21), fac(hex(84)), ss$( 2%)               , ch(40),~
               at (11,21), fac(hex(84)), ss$( 3%)               , ch(40),~
               at (12,21), fac(hex(84)), ss$( 4%)               , ch(40),~
               at (13,21), fac(hex(84)), ss$( 5%)               , ch(40),~
               at (14,21), fac(hex(84)), ss$( 6%)               , ch(40),~
               at (15,21), fac(hex(84)), ss$( 7%)               , ch(40),~
               at (16,21), fac(hex(84)), ss$( 8%)               , ch(40),~
               at (17,21), fac(hex(84)), ss$( 9%)               , ch(40),~
               at (18,21), fac(hex(84)), ss$(10%)               , ch(40),~
               at (19,21), fac(hex(84)), ss$(11%)               , ch(40),~
               at (20,21), fac(hex(84)), ss$(12%)               , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9% then goto L40770
                  debug% = 1%

L40770:        if keyhit% <> 15 then goto L40810
                  call "PRNTSCRN"
                  goto L40240

L40810:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41000     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                   (4)Previous Field    " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L40960
               str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40960:     if fieldnr% > 1% then L40980
               str(pf$(2%),20%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40980: return

L41000: if fieldnr% > 0% then L41120  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2%)= "                   (9)Debug Display     " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffff09ffffffff0e0f1000)
            if userid$ = "RHH" then return
               str(pf$(2%),20%,18%) = " " : str(pfkeys$,9%,1%) = hex(ff)
        return

L41120:     pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50160,         /* Report Selection Code */ ~
                              L50300,         /* Beg Model Code        */ ~
                              L50550,         /* End Model Code        */ ~
                              L50810,         /* Adjustment Amount     */ ~
                              L50930,         /* (S)tandard,(A)ctual   */ ~
                              L51050          /* Store One Code        */
            return

L50160: REM Report Selection                      CALC$, CALC_D$
            if calc$ <> " " then goto L50200
               calc$ = "1"

L50200:     convert calc$ to calc%, data goto L50210
L50210:
            if calc% < 1% or calc% > 9% then goto L50260
               calc_d$ = str(ss$(2% + calc%),10%,30%)
               cc% = calc% - 1%
        return
L50260:     errormsg$ = "(Error) - Invalid Report Selection Cose ??"
            calc$, calc_d$ = " "
        return

L50300: REM Beginning Mode Code              BG_MOD$, BG_MOD_D$
            if bg_mod$ <> " " then goto L50350
L50320:        bg_mod$ = "ALL"
               bg_mod_d$  = "(ALL) Model Codes in Table"
               return
L50350:     if str(bg_mod$,1%,1%) = "A" then goto L50320
            x% = len(bg_mod$)
            if x% > 3% then goto L50430
               code$ = str(bg_mod$,1%,3%)
               gosub check_model
               if code% = 0% then goto L50480
               bg_mod_d$ = desc$
               return
L50430:    part$ = bg_mod$
           gosub get_descript
        REM IF ERR% <> 0% THEN GOTO 50510
           bg_mod_d$ = part_desc$
        return
L50480:     errormsg$ = "(Error) - Invalid Model Code?"
            bg_mod$, bg_mod_d$ = " "
        return
            errormsg$ = "(Error) - Invalid MFG Part Number?"
            bg_mod$, bg_mod_d$ = " "
        return

L50550: REM Ending                           ED_MOD$, ED_MOD_D$
            if ed_mod$ <> " " then goto L50600
L50570:        ed_mod$ = bg_mod$
               ed_mod_d$  = bg_mod_d$
               return
L50600:     if str(ed_mod$,1%,1%) = "A" then goto L50570
            x% = len(ed_mod$)
            if x% > 3% then goto L50680
               code$ = str(ed_mod$,1%,3%)
               gosub check_model
               if code% = 0% then goto L50740
               ed_mod_d$ = desc$
               return
L50680:    ed_mod$ = bg_mod$
           part$ = ed_mod$
           gosub get_descript
        REM IF ERR% <> 0% THEN GOTO 50770
           ed_mod_d$ = part_desc$
        return
L50740:     errormsg$ = "(Error) - Invalid Model Code?"
            ed_mod$, ed_mod_d$ = " "
        return
            errormsg$ = "(Error) - Invalid MFG Part Number?"
            ed_mod$, ed_mod_d$ = " "
        return

L50810: REM Adjustment Amount                 ADJ_MAT$
            if adj_mat$ <> " " then goto L50840
L50830:        adj_mat$ = "0.0"
L50840:     convert adj_mat$ to adj_mat, data goto L50830

            convert adj_mat to adj_mat$, pic(##.##-)

        return
          errormsg$="(Error) - Invalid Adjustment Amount?"
          adj_mat$ = " "
        return

L50930: REM Standard / Actual Costing Method      LB_TYP$, LB_TYP_D$
            if lb_typ$ <> " " then goto L50970
               lb_typ$ = "A"

L50970:     if lb_typ$ <> "S" and lb_typ$ <> "A" then goto L51010
               if lb_typ$ = "S" then lb_typ_d$ ="Standard Costing Method"
               if lb_typ$ = "A" then lb_typ_d$ ="Actual Costing Method"
        return
L51010:     errormsg$ = "(Error) - Invalid Costing Method Selected??"
            lb_typ$, lb_typ_d$ = " "
        return

L51050: REM 1st Customer Code                STORE1$, STORE1_D$
            if store1$ <> " " then goto L51080
               store1$ = "EM0100 "
L51080:     read #12,key = store1$, using L51090,store1_d$, eod goto L51110
L51090:       FMT POS(10), CH(30)
        return
L51110:     errormsg$ = "(Error) - Invalid Store (1) Customer Code?"
            store1$, store1_d$ = " "
        return

        check_model
           code% = 0%
           readkey$ = " "
           str(readkey$,1%,9%)    = "MODEL    "
           str(readkey$,10%,15%)  = code$
           read #4,key = readkey$, using L51210, desc$, eod goto L51230
L51210:        FMT POS(25), CH(30)
           code% = 1%
L51230: return

        get_descript                          /* (AMTBOMIF) - VALIDITY */
             err% = 0%
             part_desc$ = " "
             call "APCDESCR" (part$, apc_scr$, apc_prt$, apc_sze$,       ~
                                                               #6, err% )
             str(part_desc$,1%,16%) = str(apc_prt$,1%,16%)
             str(part_desc$,17%,16%)= str(apc_sze$,1%,16%)
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+
L55060: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!
L55080: %! ######## @ ########                        ###################~
        ~#####################                     (APCCST05) Page: #### !

L55110: %! Beginning Part  : #########################                   ~
        ~                  Rpt Select : # ############################## !
L55130: %!     Description : (################################)          ~
        ~               Adjustment Amt: ######                           !
L55150: %! Ending    Part  : #########################                   ~
        ~                  (S)td,(A)ct: # ############################## !
L55170: %!     Description : (################################)          ~
        ~                  Customer   : ###### ######################### !

L55200: %! Analysis Part   : #########################  #################~
        ~###############   Error Msg  : ####################             !

L55230: %!Proc Type!Cnt!Raw Mat'l Part !<--- Raw Mat'l Description ----->~
        ~!T/Eq!Phantom!Mat'l Inch!Scr Inch!UnitCost!Availabl!Tot Cost!Unt!

L55260: %!#########!###!###############!#################################~
        ~!#/##! ##### ! ###.#### !###.####!##.#####!###.####!###.####! # !

L55290: %!---------!---!---------------!---------------------------------~
        ~!----!-------!----------!--------!--------!--------!--------!---!

L55320: %!MFG Pay/UPH: ##.##-/##.####- ! Dir.: $###.####- ! Mat'l Pay/UPH~
        ~: ##.##-/##.####- ! Indir: $###.####- ! Tot Dir.   [$####.####-]!

L55350: %!Gl1 Pay/UPH: ##.##-/##.####- ! Dir.: $###.####- ! Stage Pay/UPH~
        ~: ##.##-/##.####- ! Indir: $###.####- ! Tot Indir. [$####.####-]!

L55380: %!Gl2 Pay/UPH: ##.##-/##.####- ! Dir.: $###.####- ! Load  Pay/UPH~
        ~: ##.##-/##.####- ! Indir: $###.####- ! Tot Overh. [$####.####-]!

L55410: %!*****************************!Total: $###.####- !**************~
        ~******************! Total: $###.####- ! Tot Lab/Ovr[$####.####-]!

L55440: %!Scr Pay/UPH: ##.##-/##.####- !        --------- !**************~
        ~******************!        ---------- !            -------------!

L55470: %!Mat Vinyl Cost : $###.####-  ! Vinyl Scrap Cost : $###.####-   ~
        ~! Total Vinyl : $###.####- ! Vinyl Adj  :$###.####-             !

L55500: %!    Misc. Cost : $###.####-  ! Misc. Scrap Cost : $###.####-   ~
        ~! Total Misc  : $###.####- ! Cost Adj   :$###.####-             !

L55530: %!                             !                                 ~
        ~! Total W/F   : $###.####- ! Freight Adj:$###.####-             !

L55560: %!                 ----------  !                    ----------   ~
        ~!              ----------  !             ----------             !

L55590: %!Tot Mat'l Cost : $###.####-  ! Total Scrap Cost : $###.####-   ~
        ~! Total Cost  : $###.####- ! Total Adj's:$###.####-[$####.####-]!

L55620: %!                                                               ~
        ~! Calc Price[$####.####-]  !      Total Prod. Cost [$####.####-]!

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_header
           if lcntr% <> 99% then print using L55040
           print page
           pageno% = pageno% + 1%
           print using L55040
           print using L55080, date$, runtime$, title$, pageno%
           print using L55110, bg_mod$, calc$, calc_d$
           print using L55130, bg_mod_d$, adj_mat$
           print using L55150, ed_mod$, lb_typ$, lb_typ_d$
           print using L55170, ed_mod_d$, store1$, str(store1_d$,1%,25%)
           print using L55200, part$, part_desc$, apc_err$
           print using L55060
           print using L55230
           lcntr% = 9%
           prt_hdr$ = hdr$(k%)
        return

        prt_dtl_1
              k% = 1% : gosub print_header
              cnt% = 0%
              for k% = 1% to 6%              /* Process Types 1 thru 6 */
                for i% = 1% to tcnt%(k%)     /* Detail Raw Materials   */
                  cnt% = cnt% + 1%
                  cost = round(tmct(k%,i%) + tmsc(k%,i%), 4)
                  if i% = 1% then prt_hdr$ = hdr$(k%) else prt_hdr$ = " "
                  if lcntr% > 58% then gosub print_header
                  print using L55290
                  print using L55260, prt_hdr$, cnt%,                     ~
                                     str(tmp$(k%,i%),1%,15%),tmd$(k%,i%),~
                                     str(tmeq$(k%,i%),1%,1%),            ~
                                     str(tmeq$(k%,i%),2%,2%),            ~
                                     tmph$(k%,i%),                       ~
                                     tmc(k%,i%), tmsi(k%,i%),            ~
                                     tmuc(k%,i%), 0.0, cost, tmu%(k%,i%)
                  lcntr% = lcntr% + 2%
                next i%
              next k%
              gosub prt_dtl_2
        return

        prt_dtl_2
           mat tot = zer
                                             /* Total Mat'l Vinyl Cost */
           tot(1%) = round(rm_mat(1%) + rm_mat(4%) + rm_mat(7%) +        ~
                           rm_mat(10%) + rm_mat(13%) + rm_mat(16%), 4)
                                             /* Total Vinyl Scrap Cost */
           tot(2%) = round(rm_mats(1%) + rm_mats(4%) + rm_mats(7%), 4)
                                             /* Total Mat'l Vinyl Cost */
                                             /* Including Vinyl Scrap  */
           tot(4%) = round(tc(1%) + tc(4%) + tc(7%) + tc(10%) + tc(13%) +~
                           tc(16%), 4)
                                             /* Total Vinyl Adjustment */
                                             /* Amount of Mat'l (-)    */
           tot(3%) = round( tc(20%) * (-1.0), 4)
                                             /* Total Mat'l Misc. Cost */
           tot(5%) = round(rm_mat(2%) + rm_mat(5%) + rm_mat(8%) +        ~
                           rm_mat(11%) + rm_mat(14%) + rm_mat(17%), 4)
                                             /* Total Misc. Scrap Cost */
           tot(6%) = round(rm_mats(2%) + rm_mats(5%) + rm_mats(8%), 4)
                                             /* Total Mat'l Misc. Cost */
                                             /* Including Misc. Scrap  */
           tot(8%) = round(tc(2%) + tc(5%) + tc(8%) + tc(11%) + tc(14%) +~
                           tc(17%), 4)
           tot(7%) = round(adj_mat, 4)       /*Dollar Amount Adjustment*/
                                             /* Total Vinyl and Misc.  */
                                             /* Mat'l Cost             */
           tot(9%) = round(rm_mat(3%) + rm_mat(6%) + rm_mat(9%) +        ~
                           rm_mat(12%) + rm_mat(15%) + rm_mat(18%), 4)
                                             /* Total Vinyl and Misc.  */
                                             /* Scrap Cost             */
           tot(10%) = round(rm_mats(3%) + rm_mats(6%) + rm_mats(9%), 4)
                                             /* Total Vinyl and Misc.  */
                                             /* (Mat'l + Scrap) Cost   */
           w_f_amt = round(tc(21%), 4)       /* Calc of Wood Surround  */
           tot(12%)= round(tc(3%) + tc(6%) + tc(9%) + tc(12%) + tc(15%) +~
                           tc(18%) + tc(21%), 4)
                                              /* Calc Freight Amount   */
           frt_amt = round(tc(19%), 4)
                                              /* Total of all Adj Amt's*/
                                              /* Vinyl, Cost, and Frt  */
           tot(11%)= round(tot(3%) + tot(7%) + frt_amt, 4)
                                              /* Total Material Cost   */
                                              /* Includes Vinyl,Misc., */
                                              /* Scrap and all Adj's   */
           tot(15%) = round( tot(12%) + tot(11%), 4)
                                              /* Total All Direct Labor*/
           tot(13%)= round(lab(1%) + lab(2%) + lab(3%), 4%)
                                              /* Total all Indirect Lab*/
           if lb_typ$ = "S" then lab(4%) = lab(7%)
           tot(14%)= round(lab(4%) + lab(5%) + lab(6%), 4)
                                              /* Total MFG Cost include*/
                                              /* Vinyl,Misc.,Scrap and */
                                              /* Direct,Indirect, and  */
                                              /* Overhead Labor        */
           tot(16%) = round(tot(15%) + lab(10%), 4)
           k% = 10%
           if lcntr% > 50% then gosub print_header
           print using L55060
           print using L55320, avg_pay(1%), uph(1%), lab(1%),             ~
                              avg_pay(7%), uph(7%), lab(4%), tot(13%)

           print using L55350, avg_pay(4%), uph(4%), lab(2%),             ~
                              avg_pay(8%), uph(8%), lab(5%), tot(14%)

           print using L55380, avg_pay(5%), uph(5%), lab(3%),             ~
                              avg_pay(9%), uph(9%), lab(6%), lab(9%)

           print using L55440, avg_pay(6%), uph(6%)
           print using L55410, tot(13%), tot(14%), lab(10%)
        REM - End of Labor
           print using L55060
           print using L55470, tot(1%), tot(2%), tot(4%), tot(3%)
           print using L55500, tot(5%), tot(6%), tot(8%), tot(7%)
           print using L55530, w_f_amt, frt_amt
           print using L55560
           print using L55590, tot(9%),tot(10%),tot(12%),tot(11%),tot(15%)
           print using L55060
           print using L55620, price, tot(16%)
           print using L55040
        return

        generate_report
           call "SHOSTAT" ("Selection MFG Parts")
           gosub get_parts
           title$ = "***** APC Costing Analysis Report ******"
           call "SHOSTAT" ("Calculating Product Costs")
           apc_key$ = " "
           read #20,key > apc_key$, using L61370, part$, part_desc$,      ~
                                         adj_mat$, eod goto generate_done
           goto L61450
        generate_nxt
           read #20, using L61370, part$, part_desc$, adj_mat$,           ~
                                                   eod goto generate_done
L61370:       FMT POS(26), CH(25), CH(32), CH(4)
           adj_mat = 0.0
           convert adj_mat$ to adj_mat, data goto L61400
L61400:
           if str(part$,1%,3%) = "312" then                              ~
                                       adj_mat = round(adj_mat/10.0, 2)  ~
                             else adj_mat = round(adj_mat / 100.0 , 2)
           convert adj_mat to adj_mat$, pic(##.##-)
L61450:    gosub calc_cost
              gosub prt_dtl_1
           goto generate_nxt
        generate_done
        return

        gen_rpt
           title$ = "***** APC Costing Analysis Report ******"
           call "SHOSTAT" ("Calculating Product Costs")
           part$ = bg_mod$
           part_desc$ = bg_mod_d$
           gosub calc_cost
           gosub prt_dtl_1
        return

        get_parts
           readkey$ = " "
           str(readkey$,1%,9%) = "COST 0000"
           read #4,key > readkey$, using L61680, readkey$, desc$,         ~
                                                   eod goto get_done
           goto L61690
        get_nxt
           read #4, using L61680, readkey$, desc$, eod goto get_done
L61680:        FMT CH(24), CH(30)
L61690:    if str(readkey$,1%,9%) <> "COST 0000" then goto get_done
           mod$ = str(desc$,1%,3%)
           if str(bg_mod$,1%,3%) = "ALL" then goto L61740
              if mod$ < str(bg_mod$,1%,3%) then goto get_nxt
              if mod$ > str(ed_mod$,1%,3%) then goto get_nxt
L61740:          part$ = str(desc$,1%,25%)
                 adj_mat$ = str(desc$,27%,4%)
                 gosub get_descript
             apc_key$ = " "
             str(apc_key$,1%,4%)  = str(part$,1%,4%)
             str(apc_key$,5%,7%)  = str(part$,13%,7%)
             str(apc_key$,12%,2%) = str(part$,7%,2%)
             str(apc_key$,14%,2%) = str(part$,5%,2%)
             str(apc_key$,16%,4%) = str(part$,9%,4%)

             write #20, using L61860, apc_key$, part$, part_desc$,        ~
                            adj_mat$, "              ", eod goto get_nxt
L61860:           FMT CH(25), CH(25), CH(32), CH(4), CH(14)
             goto get_nxt
        get_done
        return

        calc_cost
           if debug% = 0% then goto L61970
*       RHH
              cc% = 99%
              part$ = "3212080006040586794"
*       RHH
L61970:    call "APCCST0B" ( cc%,        /* CALCULATION METHOD         */~
                             part$,      /* MFG Part Number            */~
/* PAR000 */                 partno1$,  /* MFG Sub part number        */~
                             adj_mat,    /* Cost Adjustment Dollars    */~
                             tmp$(),     /* Raw Mat'l Part Numbers     */~
                             tmc(),      /* Raw Mat'l Cut Inches in Dec*/~
                             tmct(),     /* Raw Mat'l Costs            */~
                             tmu%(),     /* Raw Mat'l Calc Unit of Meas*/~
                             tmd$(),     /* Raw Mat'l Descriptions     */~
                             tmuc(),     /* Raw Mat'l Unit Cost        */~
                             tmsi(),     /* Raw Mat'l Scrap Inches Dec */~
                             tmsc(),     /* Raw Mat'l Scrap Cost       */~
                             tmeq$(),    /* Calc Type and Equation No. */~
                             tmph$(),    /* Phantom Number             */~
                             tcnt%(),    /* Raw Mat'l Type Counts      */~
                             lb_typ$,    /* Labor Type (A) or (S)tand  */~
                             lab(),      /* Labor Costs (1 thru 10)    */~
                             avg_pay(),  /* Avg Hourly Pay by Dept     */~
                             uph(),      /* Avg Units Per Manhour Dept */~
                             tc(),       /* Material Costs (1 thru 18) */~
                             tt(),       /* Total Cost Buckets         */~
                             rm_mat(),   /* Material Costs (1 thru 10) */~
                             rm_mats(),  /* Mat'l Scrap Costs(1 thru 9)*/~
                             store1$,    /* Customer Code for Pricing  */~
                             pc(),       /* 35 Prices                  */~
                             price,      /* Calc. Price for Customer   */~
                             #1,         /*   (APCCUTEQ)               */~
                             #2,         /*   (HNYMASTR)               */~
                             #3,         /*   (HNYQUAN )               */~
                             #4,         /*   (GENCDSIN)               */~
                             #5,         /*   (AMTBOMCD)               */~
                             #7,         /*   (APCEMPLY)               */~
                             #8,         /*   (APCEQUAT)               */~
                             #9,         /*   (APCCSTHP)               */~
                             #10,        /*   (APCCSTLR)               */~
                             #11,        /*   (CPRPRICE)               */~
                             #12,        /*   (CUSTOMER)               */~
                             #13,        /*   (APCPCMST)               */~
                             #21,        /*   (AWDPCMST)               */~
                             #14,        /*   (APCSKUNO)               */~
                             #15,        /*   (APCPCMSK)               */~
                             #16,        /*   (APCPCMSD)               */~
                             #17,        /*   (APCCSTEX)               */~
                             #18,        /*   (APCSTOCK)               */~
                             #19,        /*   (APCPLNDP)               */~
                             apc_err%()) /* 0% = Ok, Non Zero Error    */
            apc_err$ = "**** No Errors *****"
            for i% = 1% to 20%
              if apc_err%(i%) = 0% then goto L62440
                 apc_err$ = err$(apc_err%(i%))
L62440:     next i%
        return

        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#20,mode$, 500%, f2%)
            if f2% <> 0% then goto L62550
        return
L62550:     call "SHOSTAT" ("Error - Cannot Open (APCCSTWK)") : stop
        return
        delete_work
            call "FILEBGON" (#20)
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
