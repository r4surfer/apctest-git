        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - APCCST06                             *~
            *  Creation Date     - 08/01/95                             *~
            *  Last Modified Date- 10/31/05                             *~
            *  Description       - Program to Create a report with      *~
            *                      the inventory Evaluation using       *~
            *                      the Subroutine to obtain the         *~
            *                      material, labor, and Overhead Cost   *~
            *                      Associated with Finished Goods.      *~
            *                                                           *~
            *  Special Notes     - Hidden PF(9) Key Turns on the Debug  *~
            *                      for each Phase of Cost Calculation.  *~
            *                                                           *~
            *  Special Hook Area - Line No. 61980 = 3212080006040586794 *~
            *                                                           *~
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
            * 08/08/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 09/15/95 ! Mod to use the new routine (APCCTS0B) to ! RHH *~
            *          !   Calcualte the Freight Amount and the   !     *~
            *          !   Vinyl Discount Amount.                 !     *~
            * 06/06/96 ! Mod to Add the New Debug Subroutine, Use ! RHH *~
            *          !   DEBUG% and CC% to turn on Debug.       !     *~
            * 11/08/97 ! Mod to Add tNew File fo Labor APCPLNDP   ! RHH *~
            *          !                                          !     *~
            * 03/13/98 ! y2k compliance                           ! DJD *~
            * 10/31/05 1 (AWD001) CR347 Mod for sub part          ! CMG *~
            * 01/11/06 ! (PAR000) CR347 mods for sub part         ! CMG *~
            *02/13/2019! CR-1894  Increase EMP DEPT size to 3     ! DES *~
            *************************************************************

        dim                              /* Report Variables           */~
            hh$30, apc_key$30,           /* Plowcode Header            */~
            bg_mod$25, bg_mod_d$32,      /* Beginning Model Code all   */~
            ed_mod$25, ed_mod_d$32,      /* Ending MOdel Code          */~
            adj_mat$6,                   /* Adj Amt                    */~
            store1$3, store1_d$30,       /* Customer Code              */~
            typ$1, typ_d$30,             /* Rpt Type 0=Non-Zero,1=All  */~
            apc_scr$120,                 /*                            */~
            apc_prt$60,                  /*                            */~
            apc_sze$20,                  /*                            */~
            part_desc$32, prt_part$32,   /* Calculated Part Description*/~
            save_mod$3, mod$3, mod_desc$32   /* Model Code             */

        dim                              /* (Program) - Variables      */~
            title$40,date$8, cnt$10,     /* REPORT TITLE               */~
            runtime$8,                   /* REPORT RUN TIME            */~
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
            apc$   = "  Inventory Evaluation Report Utility   "
            pname$ = "APCCST06 - Rev: R6.04"

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
            * #17 ! APCCSTEX ! APC EXCEPTION DEFINITION FILE            *~
            * #18 ! APCSTOCK ! APC STOCK MASTER FILE                    *~
            * #19 ! APCPLNDP ! MASTER DEPARTMENT FILE                   *~
            * #20 ! STORNAME ! MASTER STORE DEFINITION FILE             *~
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

            select #20, "STORNAME",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos = 1, keylen = 3

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
            call "OPENCHCK" (#17, fs%(17%), f2%(17%),100%, rslt$(17%))
            call "OPENCHCK" (#18, fs%(18%), f2%(18%),100%, rslt$(18%))
            call "OPENCHCK" (#19, fs%(19%), f2%(19%),100%, rslt$(19%))
            call "OPENCHCK" (#20, fs%(20%), f2%(20%),  0%, rslt$(20%))
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

            err$(1% ) = "(Error) In Labor Cal"
            err$(2% ) = "(Error) in Material "
            err$(3% ) = "(Error) in Glass    "
            err$(4% ) = "(Error) in Screen   "
            err$(5% ) = "(Error) in Locks    "
            err$(6% ) = "(Error) in Hardware "
            err$(7% ) = "(Error) in Packaging"
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
            cc% = 0%

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
            gosub select_printer
            gosub gen_rpt
            close printer
        return clear all
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
         "Enter a Beginning Part Number or (ALL)?                      ",~
         "Enter a Ending Part Number?                                  ",~
         "Enter a Valid Vinyl Disc. %, Freight %, and Adjustment Amt?  ",~
         "Enter an (S)tandard or (A)ctual for the Costimg Method to Use",~
         "Enter a Valid Store Code for Finished Goods Inventory?       ",~
         "Enter Report Type? 0 = All Non-Zero, 1 = All Finished Goods. "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, readkey$, lb_typ$, desc$,  ~
                      lb_typ_d$, bg_mod$, bg_mod_d$, ed_mod$, ed_mod_d$, ~
                      hh$, store1$, store1_d$, adj_mat$,                 ~
                      apc_err$, mod$, save_mod$, typ$, typ_d$
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
              on fieldnr% gosub L40210,         /* Beginning Model Code */~
                                L40210,         /* Ending Model Code    */~
                                L40210,         /* Terms Vinyl Discount */~
                                L40210,         /* (S)td or (A)ctual Lab*/~
                                L40210,         /* MFG Store Number     */~
                                L40210          /* Report Type          */
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
               at (03,02), "Beginning Part No:",                         ~
               at (03,21), fac(lfac$(1%)), bg_mod$              , ch(25),~
               at (03,48), fac(hex(84)), bg_mod_d$              , ch(32),~
                                                                         ~
               at (04,02), "Ending Part No.  :",                         ~
               at (04,21), fac(lfac$(2%)), ed_mod$              , ch(25),~
               at (04,48), fac(hex(84)), ed_mod_d$              , ch(32),~
                                                                         ~
               at (05,02), "Adjustment Amount:",                         ~
               at (05,21), fac(lfac$(3%)), adj_mat$             , ch(06),~
                                                                         ~
               at (06,02), "Lab-(S)td,(A)ct  :",                         ~
               at (06,21), fac(lfac$(4%)), lb_typ$              , ch(01),~
               at (06,48), fac(hex(84)), lb_typ_d$              , ch(30),~
                                                                         ~
               at (07,02), "MFG'S Store Code :",                         ~
               at (07,21), fac(lfac$(5%)), store1$              , ch(03),~
               at (07,48), fac(hex(84)), store1_d$              , ch(30),~
                                                                         ~
               at (08,02), "Report Type (0/1):",                         ~
               at (08,21), fac(lfac$(6%)), typ$                 , ch(01),~
               at (08,48), fac(hex(84)), typ_d$                 , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9% then goto L40610
                  debug% = 1%

L40610:        if keyhit% <> 15 then goto L40650
                  call "PRNTSCRN"
                  goto L40240

L40650:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40840     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                   (4)Previous Field    " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L40800
               str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40800:     if fieldnr% > 1% then L40820
               str(pf$(2%),20%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40820: return

L40840: if fieldnr% > 0% then L40940  /*  Edit Mode - Select Fld */
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

L40940:     pf$(1%)= "(1)Start Over                           " &        ~
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
            on fieldnr% gosub L50160,         /* Beg Model Code        */ ~
                              L50390,         /* End Model Code        */ ~
                              L50630,         /* Terms Vinyl Discount  */ ~
                              L50750,         /* (S)tandard,(A)ctual   */ ~
                              L50870,         /* Finished Goods Store  */ ~
                              L51000          /* Report Type           */
            return

L50160: REM Beginning Mode Code              BG_MOD$, BG_MOD_D$
            if str(bg_mod$,1%,1%) <> "A" then goto L50220
               bg_mod$ = "ALL"
               bg_mod_d$ = "(ALL) MFG Part Numbers"
               return

L50220:     if bg_mod$ <> " " then goto L50260
               hh$ = hex(06) & "Select a MFG Part Number"
               call "PLOWCODE" (#2, bg_mod$, hh$, 0%, .30, f1%(2%))

L50260:        read #2,key = bg_mod$, using L50280, bg_mod_d$,            ~
                                                           eod goto L50350
L50280:           FMT POS(26), CH(30)
            part$ = bg_mod$
            gosub get_descript
            if err% <> 0% then goto L50350
            bg_mod_d$ = part_desc$
            if len(bg_mod$) < 19 then goto L50350
        return
L50350:     errormsg$ = "(Error) - Invalid Beginning MFG Part Number?"
            bg_mod$, bg_mod_d$ = " "
        return

L50390: REM Ending                           ED_MOD$, ED_MOD_D$
            if str(bg_mod$,1%,1%) <> "A" then goto L50450
               ed_mod$ = "ALL"
               ed_mod_d$ = "(ALL) MFG Part Numbers"
               return

L50450:     if ed_mod$ <> " " then goto L50490
               hh$ = hex(06) & "Select a MFG Part Number"
               call "PLOWCODE" (#2, ed_mod$, hh$, 0%, .30, f1%(2%))

L50490:        read #2,key = ed_mod$, using L50510, ed_mod_d$,            ~
                                                           eod goto L50590
L50510:           FMT POS(26), CH(30)
            part$ = ed_mod$
            gosub get_descript
            if err% <> 0% then goto L50590
            ed_mod_d$ = part_desc$
            if bg_mod$ > ed_mod$ then goto L50590
            if len(ed_mod$) < 19 then goto L50590
        return
L50590:     errormsg$ = "(Error) - Invalid Ending MFG Part Number?"
            ed_mod$, ed_mod_d$ = " "
        return

L50630: REM Adjustment Amount                 ADJ_MAT$
            if adj_mat$ <> " " then goto L50660
               adj_mat$ = "0.0"
L50660:     convert adj_mat$ to adj_mat, data goto L50710

            convert adj_mat to adj_mat$, pic(##.##-)

        return
L50710:   errormsg$="(Error) - Invalid Adjustment Amount?"
          adj_mat$ = " "
        return

L50750: REM Standard / Actual Costing Method      LB_TYP$, LB_TYP_D$
            if lb_typ$ <> " " then goto L50790
               lb_typ$ = "A"

L50790:     if lb_typ$ <> "S" and lb_typ$ <> "A" then goto L50830
               if lb_typ$ = "S" then lb_typ_d$ ="Standard Costing Method"
               if lb_typ$ = "A" then lb_typ_d$ ="Actual Costing Method"
        return
L50830:     errormsg$ = "(Error) - Invalid Costing Method Selected??"
            lb_typ$, lb_typ_d$ = " "
        return

L50870: REM Finished Goods Store             STORE1$, STORE1_D$
            if store1$ <> " " then goto L50900
               store1$ = "300"
L50900:     read #20,key = store1$, using L50910,store1_d$, eod goto L50960
L50910:       FMT POS(4), CH(30)
            convert store1$ to store1%, data goto L50930
L50930:
            if store1% < 300% then goto L50960
        return
L50960:     errormsg$ = "(Error) - Invalid Finished Goods Store Number?"
            store1$, store1_d$ = " "
        return

L51000: REM Report Type                       TYP$, TYP_D$
             if typ$ <> " " then goto L51030
                typ$ = "0"
L51030:      if typ$ <> "0" and typ$ <> "1" then goto L51070
                if typ$ = "0" then typ_d$ = "All Non-Zero Finished Goods"~
                              else typ_d$ = "All Finished Goods         "
        return
L51070:      errormsg$ = "(Error) - Invalid Report Type?"
             typ$, typ_d$ = " "
        return

        get_descript                          /* (AMTBOMIF) - VALIDITY */
             part_desc$ = " "
	     /* this is for debugging olnly!!! */
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
        ~#####################                                Page: #### !

L55110: %! Beg Part : #########################                          ~
        ~                                             Adj Pct  : ####### !
L55130: %! End Part : #########################                          ~
        ~                                                                !
L55150: %! Store    : ### ##############################                 ~
        ~                                                                !

L55180: %!<- Part Number and Description->!On - Hand ! Material !Dir/Ind ~
        ~Lb! Overhead!Tot Material!Total Labor !Tot Overhead! Total Cost !

L55210: %!################################!          !          !        ~
        ~  !         !            !            !            !            !

L55240: %!################################!##,###.##-!##,###.##-!##,###.#~
        ~#-!#,###.##-!####,###.##-!####,###.##-!####,###.##-!####,###.##-!

L55270: %!################################!          !          !        ~
        ~  !         !####,###.##-!####,###.##-!####,###.##-!####,###.##-!

L55300: %!--------------------------------!----------!----------!--------~
        ~--!---------!------------!------------!------------!------------!

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_header
           if lcntr% <> 99% then print using L55040
           print page
           pageno% = pageno% + 1%
           print using L55040
           print using L55080, date$, runtime$, title$, pageno%
           print using L55110, bg_mod$, adj_mat$
           print using L55130, ed_mod$
           print using L55150, store1$, store1_d$
           print using L55060
           print using L55180
           lcntr% = 7%
        return

        prt_dtl_1
           mat tot = zer
           labor, overhead, tot_mat, tot_lab = 0.0
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
                                             /* (Mat'l + Scrap) Cost  */
           tot(12%)= round(tc(3%) + tc(6%) + tc(9%) + tc(12%) + tc(15%) +~
                           tc(18%), 4)
                                              /* Calc Freight Amount   */
           frt_amt = round( tc(19%), 4)
           w_f_amt = round( tc(21%), 4)       /* WOOD SURR/FACT MULL   */
                                              /* Total of all Adj Amt's*/
                                              /* Vinyl, Cost, and Frt  */
           tot(11%)= round(tot(3%) + tot(7%) + frt_amt + w_f_amt, 4)
                                              /* Total Material Cost   */
                                              /* Includes Vinyl,Misc., */
                                              /* Scrap and all Adj's   */
           tot(15%) = round( tot(12%) + tot(11%), 4)
                                              /* Total All Direct Labor*/
           tot(13%)= round(lab(1%) + lab(2%) + lab(3%), 4%)
                                              /* Total all Indirect Lab*/
           if lb_typ$ = "S" then lab(4%) = lab(7%)
           tot(14%)= round(lab(4%) + lab(5%) + lab(6%), 4)
           if x_err% = 0% then goto L60730
              labor, overhead = 0.0
              tot_mat = t_mat
              goto L60760

L60730:    tot_mat  = round(tot(15%), 2)            /* Material        */
           labor    = round(tot(13%) + tot(14%), 2) /* Direct/Indirect */
           overhead = round(lab(9%), 2)             /* Overhead        */
L60760:    tot_lab  = round(labor + overhead, 2)    /* Total Lab/OverH */
                                              /* Total MFG Cost include*/
                                              /* Vinyl,Misc.,Scrap and */
                                              /* Direct,Indirect, and  */
                                              /* Overhead Labor        */
           tot(16%) = round(tot_mat + tot_lab, 2)
           utot_mat = round(tot_mat  * inv_on_hand, 2) /* Total Mat'l  */
           utot_lab = round(labor    * inv_on_hand, 2) /* Total Labor  */
           utot_ovr = round(overhead * inv_on_hand, 2) /* Total Overhea*/
           utot_cost = round(tot(16%) * inv_on_hand, 2) /* Total Cost  */

           gosub get_descript
           prt_part$ = "*** " & part$ & " **"
           if tot_mat > .01 then goto L60920
              str(part_desc$,13%,20%) = "(Error) No Material "

L60920:    if tot_lab > .01 then goto L60960
              if x_err% <> 0% then goto L60970
              str(part_desc$,13%,20%) = "(Error) No Labor    "

L60960:    if err_flag% <> 0% then str(part_desc$,13%,20%) = apc_err$
L60970:    if lcntr% > 57% then gosub print_header
              print using L55300
              print using L55210, prt_part$
              print using L55240, part_desc$, inv_on_hand, tot_mat,       ~
                                 labor, overhead, utot_mat, utot_lab,    ~
                                 utot_ovr, utot_cost
              lcntr% = lcntr% + 3%
                                                       /* Model Totals */
              mtot_mat  = round(mtot_mat  + utot_mat, 2)
              mtot_lab  = round(mtot_lab  + utot_lab, 2)
              mtot_ovr  = round(mtot_ovr  + utot_ovr, 2)
              mtot_cost = round(mtot_cost + utot_cost, 2)
                                                       /* Grand Totals */
              gtot_mat  = round(gtot_mat  + utot_mat, 2)
              gtot_lab  = round(gtot_lab  + utot_lab, 2)
              gtot_ovr  = round(gtot_ovr  + utot_ovr, 2)
              gtot_cost = round(gtot_cost + utot_cost, 2)
        return

        prt_model
           if lcntr% > 57% then gosub print_header
           mod_desc$ = "Subtotal for Product ( "&save_mod$&" )"
           print using L55300
           print using L55270, mod_desc$, mtot_mat, mtot_lab, mtot_ovr,   ~
                              mtot_cost
           save_mod$ = mod$
           mtot_mat, mtot_lab, mtot_ovr, mtot_cost = 0.0
           lcntr% = lcntr% + 2%
        return

        prt_totals
           if lcntr% > 57% then gosub print_header
           mod_desc$ = "********* Grand Totals *********"
           print using L55300
           print using L55270, mod_desc$, gtot_mat, gtot_lab, gtot_ovr,   ~
                              gtot_cost
           print using L55040
           save_mod$ = mod$
           gtot_mat, gtot_lab, gtot_ovr, gtot_cost = 0.0
        return

        gen_rpt
           cnt% = 0%
           price = 0.0
           mtot_mat, mtot_lab, mtot_ovr, mtot_cost = 0.0
           gtot_mat, gtot_lab, gtot_ovr, gtot_cost = 0.0
           title$ = "APC Inventory Evaluation Report-Standard"
           if lb_typ$ = "A" then str(title$,33%,8%) = "  Actual"
           apc_key$ = " "
           if str(bg_mod$,1%,3%) = "ALL" then goto L61480
              apc_key$ = bg_mod$
L61480:    read #2,key > apc_key$, using L61530,apc_key$,eod goto gen_done
           save_mod$ = str(apc_key$,1%,3%)
           goto L61540
        gen_next
           read #2,key > apc_key$, using L61530,apc_key$,eod goto gen_done
L61530:       FMT CH(25)
L61540:    cnt% = cnt% + 1%
           part$ = apc_key$
           mod$ = str(part$,1%,3%)
           if str(bg_mod$,1%,3%) = "ALL" then goto L61590
              if part$ > ed_mod$ then goto gen_done
L61590:    if len(part$) < 19 then goto gen_next
           if mod(cnt%,10%) <> 0% then goto L61650
              cnt$ = "[ XXXXX ]"
              convert cnt% to str(cnt$,3%,5%), pic(#####)

              print at(02,35);hex(84);cnt$
L61650:    gosub get_onhand
           if typ$ = "0" and inv_on_hand <= 0.0 then goto gen_next
           if save_mod$ = mod$ then goto L61690
              gosub prt_model
L61690:    gosub calc_cost
           gosub prt_dtl_1
           goto gen_next

        gen_done
           gosub prt_model
           gosub prt_totals
        return

        get_onhand
          readkey$ = all(hex(00)) : inv_on_hand = 0.0
          str(readkey$,1%,25%) = part$
          read #3,key > readkey$, using L61860, readkey$, on_hand,        ~
                                                     eod goto L61910
          goto L61870
        get_onhand_nxt
          read #3, using L61860, readkey$, on_hand, eod goto L61910
L61860:       FMT POS(17), CH(44), POS(69), PD(14,4)
L61870:   if str(readkey$,1%,25%) <> part$ then goto L61910
          if str(readkey$,26%,3%) <> store1$ then goto get_onhand_nxt
             inv_on_hand = inv_on_hand + on_hand
             goto get_onhand_nxt
L61910:   inv_on_hand = round(inv_on_hand, 2)
        return

        calc_cost
           if debug% = 0% then goto L62000
*       RHH
              cc% = 99%
              part$ = "3212080006040586794"
*       RHH
L62000:    call "APCCST0B" ( cc%,        /* Calc Method - 0% = All Area*/~
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
                             "EM0100",   /* Customer Code for Pricing  */~
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
                             #21,        /*   (AWDPCMST)   CR1894      */~
                             #14,        /*   (APCSKUNO)               */~
                             #15,        /*   (APCPCMSK)               */~
                             #16,        /*   (APCPCMSD)               */~
                             #17,        /*   (APCCSTEX)               */~
                             #18,        /*   (APCSTOCK)               */~
                             #19,        /*   (APCPLNDP)               */~
                             apc_err%()) /* 0% = Ok, Non Zero Error    */
            err_flag% = 0%
            apc_err$ = "**** No Errors *****"
            gosub check_nocost
            if x_err% <> 0% then return
            for i% = 1% to 20%
              if apc_err%(i%) = 0% then goto L62510
                 apc_err$ = err$(apc_err%(i%))
                 err_flag% = 1%
L62510:     next i%
        return

        check_nocost
           x_err% = 0%
           init(" ") readkey$
           str(readkey$,1%,9%)   = "COST NONE"
           str(readkey$,10%,15%) = str(part$,1%,3%)
           read #4,key = readkey$, eod goto L62620
              x_err% = 1%                           /* TREAT AS A PART */
              gosub compute_cost
L62620: return

        compute_cost
           t_mat = 0.0
           mod$ = str(part$,1%,3%)
           sale_1, sale_2, sale_3 = 0.0
           readkey$ = " "
           str(readkey$,1%,9%)   = "COST SALE"
L62700:    str(readkey$,10%,15%) = mod$
           read #4,key = readkey$, using L62720, desc$, eod goto L62880
L62720:       FMT POS(25), CH(30)
           convert str(desc$,1%,8%)  to sale_1, data goto L62740
L62740:
           convert str(desc$,11%,8%) to sale_2, data goto L62760
L62760:
           convert str(desc$,22%,8%) to sale_3, data goto L62780
L62780:
           sale_1 = sale_1 / 100.0
           sale_2 = sale_2 / 100.0
                                                  /* Costing Error     */
           t_mat = ( pc(1%) * .50 ) * sale_1      /* Calc based on the */
                                                  /* Catalog Price     */
           if pc(1%) < .01 then t_mat  = sale_3
           if t_mat < .01 then t_mat = sale_3
           mod$ = str(part$,1%,3%)
        return
L62880:    mod$ = "000"
           goto L62700

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
