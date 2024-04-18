        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN02                             *~
            *  Creation Date     - 02/19/03                             *~
            *  Last Modified Date- 10/30/2014                           *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New program to build an Appian       *~
            *                      Extract file.                        *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *  Spec. Comm (Screen 1) -                                  *~
            *                         PF(10) Edit Change Load Data and  *~
            *                                Information.               *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/19/02 ! New Program for (EWS) - Last Mod Date    ! CMG *~
            * 12/10/04 ! (AWD001) Mod for EOD test.               ! RHH *~  
            * 10/03/05 ! (AWD002) double height for bay/bow units ! CMG *~
            *          !   when calculating cubic inches          !     *~
            * 03/16/06 ! (AWD003) - modification for North East   ! CMG *~
            * 07/06/06 ! (AWD004) - mod to add ship_to in download! CMG *~
            * 09/06/06 ! (AWD005) - mod to better calc mull size  ! CMG *~
            * 06/27/08 ! (AWD006) - mod for howship data          ! CMG *~
            *03/26/2011! (AWD007) - mod to use customer address   ! CMG *~
            *          !    for M2O orders                        !     *~
            *11/03/2011! (AWD008) - mod to remove recalc cubic unit!CMG *~
            *10/30/2014! (AWD009) - mod to add delivery days for  ! CMG *~
            *          !     Penske                               !     *~
            *09/06/2017! (CR1104) - fix addresses for cross dock  ! RDB *~
            *04/16/2021! CR2819 Remove STK and M2O Address logic  ! RDB *~
            *************************************************************

        dim                              /* (APCPLNOR) - FILE          */~
            or_due$8,                    /* S.O. Due Date/Delivery Date*/~
            or_region$2,                 /* Customer Region Code       */~
            or_route$5,                  /* Customer Route Code        */~
            or_zip$9,                    /* Customer Zip Code          */~
            or_drop$2,                   /* Customer Drop Number       */~
            or_cuscode$9,                /* Customer Number            */~
            or_po$16,                    /* Customer P.O. Number       */~
            or_so$8,                     /* Customer S.O. Number       */~
            or_status$2,                 /* Current S.O. Stat PLAN STAT*/~
            or_dte$8,                    /* Date Assoc. with Stat Chg  */~
            or_inv$8,                    /* APC Invoice Assoc. with SO */~
            or_chk$8,                    /* APC Check Assoc. with Invoi*/~
            or_sls$4,                    /* APC Salesman Code          */~
            or_fob$2, how_ship$11,       /* Cust Delivery Cde PLAN DEL?*/~
            or_hows$2,                   /* Spec. Instr (PLAN HOWS)    */~
            or_load$5,                   /* Load N. Sched./Assigned    */~
            or_date$8,                   /* Date S.O. Created          */~
            or_chg$8,                    /* Date S.O. Last Modified    */~
            or_userid$3,                 /* S.O. Last Modified By User */~
            or_bol$8,                    /* Date B.O.L. Created        */~
            or_text$4,                   /* S.O. Header Text Id        */~
                                         /* Actual No. Delivery Days   */~
            or_special$10, or_rec$170,   /* Special Product Flags      */~
            or_fil$3, or_key$51,         /* Filler Area                */~
            sc_key$10,                   /* SC Readkey                 */~
            sc_key1$27,                  /* S.O. Alt 1 Load Key        */~
            sc_part$25,                  /* SC Part Number             */~
            sc_drop_seq$5,               /* S.O. Line Item Drop Seq No */~
            wood$3                       /* Wood Surround Flags        */


        dim                              /* (Program) - Variables      */~
            filename$8,                  /* Used By EWDOPEN            */~
            blankdate$6,                 /* Null Date test             */~ 
            x$8,                         /* Format Due Date            */~
            cnt$28, page$16, line1$65,   /* Screen Display             */~
            mode$5,                      /* Open Work Routine          */~
            hdr$45, msg$(3%)79,          /* Askuser - Var's            */~
            date$8,                      /* REPORT TITLE               */~
            readkey$50, desc$30,         /* Generic Key                */~
            bckmst_key$25,               /* BCKMASTR Readkey           */~
            name$30, address$(2%)30,     /* BCKMASTR Ship To Name      */~
            cust_address$(2%)30,         /* Customer Address (AWD007)  */~
            custcity$18,                 /* Customer City (CR1104)     */~
            custst$2,                    /* Customer State (CR1104)    */~
            custzip$9,                   /* Customer Zip (CR1104)      */~
            userwhoenter$3,              /* User who entered order (AWD007)*/~
            address1$30,                 /*  and Address               */~
            city$25,                     /* City                       */~
            state$2,                     /* State                      */~
            zip$9,                       /* Zip Code                   */~    
            cutoff$2,                    /* Customer Cutoff Code       */~
            deliver$2,                   /* Num of Days to Deliver (AWD009) */~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */

        dim tot_units$12,                /* Total Units FOR Criteria   */~
            tot_make$12,                 /* Total Makes for Criteria   */~
            tot_pull$12,                 /* Total Pulls for Criteria   */~
            tot_product$12,              /* Total Product(Units)       */~
            tot_prod_value$12,           /* Total S.O. Net Value       */~
            pg_tot_unit$10,              /* Total Page Units           */~
            pg_tot_value$10,             /* Total Page Net Value       */~
            h1$3, h2$10, h3$5, h4$9,     /* Screen Headers             */~
            h5$25, h6$8, h7$6, h8$5,     /* Screen Headers             */~
            c1$(9000%)1,                 /* Include/Exclude            */~
            c2$(9000%)10,                /* Dispatch Date              */~
            c3$(9000%)5,                 /* Route Codes                */~
            c4$(9000%)9,                 /* Customer Code              */~
            c5$(9000%)25,                /* Customer Name              */~
            c6$(9000%)8,                 /* Sales Order Number         */~
            c7$(9000%)7,                 /* Loading Units              */~
            c8$(9000%)30,                /* Address 1                  */~
            c9$(9000%)25,                /* City                       */~
            c10$(9000%)2,                /* State                      */~
            c11$(9000%)9,                /* Zip Code                   */~
            c12$(9000%)2,                /* Cutoff                     */~
            c13$(9000%)5,                /* Load Number                */~
            c14$(9000%)7,                /* Gross Open Amount          */~
            c15$(9000%)30,               /* Address 2                  */~
            c16$(9000%)16,               /* Purchase Order Number      */~
            c17$(9000%)10,               /* Total Qty                  */~
/*AWD004*/  c18$(9000%)3,                /* Ship To                    */~
/*AWD009*/  c19$(9000%)2,                /* Deliver (AWD009)           */~
            exc_cust$(900%)9,            /* Customers to Exclude       */~
            exc_how$(100%)2,             /* How Ship to Exclude        */~
            genkey$24                    /* Gencodes Readkey           */


        dim bg_ld_dte$10,                /* Screen Beg Load Date       */~
            ed_ld_dte$10,                /* Screen End Load Date       */~
            app_ld_key$16,               /* Appian Read Key 1%         */~
            bg_ld$5,                     /* Screen Beg Load Number     */~
            ed_ld$5,                     /* Screen End Load Number     */~
            bg_ld_d$30,                  /* Screen Beg Load Desc       */~
            ed_ld_d$30,                  /* Screen End Load Desc       */~
            bg_tme$4,                    /* Screen Beg Start Time      */~
            ed_tme$4,                    /* Screen End Start Time      */~
            scr_ear_dte$10               /* Screen Earliest Dispatch   */

        dim comma$1,                     /* Comma for EWDAPPSN File    */~
            file$8,                      /* File Name                  */~
            library$8,                   /* Library Name = APCDATA     */~
            volume$6                     /* DISK VOLUME = CARLOS       */

        dim schema$8                     /* SCHEMA          (AWD002)   */

        dim ship_to$3                    /* Customer Ship To (AWD003)  */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Appian File Extract Utility  "
            pname$ = "AWDPLN02 - Rev: R6.04"

        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNOR ! (NEW) S.O. Header Histroy                *~
            * #2  ! GENCODES ! Master Code Tables File                  *~
            * #3  ! APCPLNSC ! Planning Master Schedule File            *~
            * #4  ! CUSTOMER ! Customer Master Schedule File            *~
            * #5  ! APCPLNLD ! Load Master File                         *~
            * #6  ! AWDPLNLD ! Appian Load Master File                  *~
            * #7  ! AWDAPPSN ! File to sent to PC                       *~
            * #8  ! APCPLNWK ! Work File                                *~
            * #9  ! BCKMASTR ! Sales Order Master- Headers              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos = 1,    keylen = 51,                      ~
                        alt key 1, keypos = 27, keylen = 25,             ~
                            key 2, keypos = 70, keylen =  8, dup,        ~
                            key 3, keypos = 78, keylen =  8, dup,        ~
                            key 4, keypos = 52, keylen =  8,             ~
                            key 5, keypos = 36, keylen = 16, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24

            select #3,  "APCPLNSC",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 24,   keylen = 10,                      ~
                        alt key 1, keypos =  7, keylen = 27,             ~
                            key 2, keypos =  1, keylen = 33

            select #4,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos = 1,    keylen =  9,                      ~
                        alt key 1, keypos = 10, keylen = 30, dup,        ~
                            key 2, keypos =424, keylen =  9, dup,        ~
                            key 3, keypos =771, keylen =  9, dup,        ~
                            key 4, keypos =780, keylen =  9, dup,        ~
                            key 5, keypos = 1049, keylen = 9, dup

            select #5,  "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 11,   keylen =  5,                      ~
                        alt key 1, keypos =  3, keylen = 13,             ~
                            key 2, keypos =  1, keylen = 15

            select #6,  "AWDAPPLD",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   12, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =  16,         ~
                            key  2, keypos =    2, keylen =  15,         ~
                            key  3, keypos =   17, keylen =  15

            select #7, "AWDAPPSN",                                       ~
                        varc,     indexed, recsize = 256,                ~
                        keypos = 1,    keylen = 50

            select #8,  "APCPLNWK",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =  1,   keylen = 64


            select #9,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            call "SHOSTAT" ("Initialization")


            filename$ = "APCPLNOR" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSC" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "CUSTOMER" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNLD" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDAPPLD" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
REM            call "OPENCHCK" (#7, fs%(7%), f2%(7%), 500%, rslt$(7%))

            filename$ = "BCKMASTR" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
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
            call "DATUFMTC" (blankdate$)
            h1$ = "I/E"
            h2$ = "Dsptch Dte"
            h8$ = "Load "
            h3$ = "Route"
            h4$ = "Customer "
            h5$ = "<---- Customer Name ---->"
            h6$ = "<-S.O.->"
            h7$ = "LUnits"

* (AWD003) Next 3 lines
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #2, schema_err%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 5%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
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
                  if keyhit%  =  9% then gosub load_analysis
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg1
L11150:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 5% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11200:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11200
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11200
                  lastfieldnr% = fieldnr%
            goto L11150

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        load_analysis
            call "SHOSTAT" ("Loading Appian Load Data")

            gosub open_file
            init(" ") tot_units$, tot_make$, tot_pull$, tot_product$,    ~
                      tot_prod_value$, pg_tot_unit$, pg_tot_value$, cnt$,~
                      or_key$, sc_key1$, c1$(), c2$(), c3$(), c4$(),     ~
                      c5$(), c6$(), c7$(), c8$(), c9$(), c10$(), c11$(), ~
                      c12$(), c13$(), c14$(), c15$(), c16$(), c17$(),    ~
                      c18$(), c19$()

            tt = 0.0                             /* Total Run Net $$$  */
            cnt% = 0%
            cnt$ = "Records Scanned [ xxxxxxxx ]"
            tot_units    = 0.0 : tot_make%      = 0%  : tot_pull% = 0%
            tot_product% = 0%  : tot_prod_value = 0.0
            pg_tot_unit  = 0.0 : pg_tot_value   = 0.0
            val_max%     = 0%  : cc%            = 0%
            gosub load_exclude_customers
            gosub load_exclude_hows
            gosub scan_appian_loads

            gosub'102(1%, 1%)                   /* Select Sales Orders */
            keyhit% = 0%

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
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
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
         "Enter a Valid Beginning/Ending Load Date ?                   ",~
         "Enter a Valid Beginning Load Number ?                        ",~
         "Enter a Valid Ending    Load Number ?                        ",~
         "Enter a Valid Beginning Start Time, Military Format HHMM  ?  ",~
         "Enter a Valid Ending    Start Time, Military Format HHMM  ?  "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28280
                inpmessage$ = edtmessage$
                return

L28280
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Non-Blank Character in Include/Exclude?              "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") or_due$, or_region$, or_route$, or_zip$, or_drop$, ~
                      or_cuscode$, or_po$, or_so$, or_status$, or_dte$,  ~
                      or_inv$, or_chk$, or_sls$, or_fob$, how_ship$,     ~
                      or_hows$, or_load$, or_date$, or_chg$, or_userid$, ~
                      or_bol$, or_text$, or_special$, or_rec$, or_fil$,  ~
                      or_key$, sc_key1$, sc_drop_seq$,        x$, cnt$,  ~
                      page$, line1$, mode$, hdr$, msg$(), readkey$, zip$,~
                      desc$, edtmessage$, errormsg$, i$(), name$, city$, ~
                      inpmessage$, lfac$(), pf$(), pfkeys$, tot_units$,  ~
                      tot_make$, tot_pull$, tot_product$, tot_prod_value$,~
                      pg_tot_unit$, pg_tot_value$, c1$(), c2$(), c3$(),  ~
                      c4$(), c5$(), c6$(), c7$(), bg_ld_dte$, ed_ld_dte$,~
                      app_ld_key$, bg_ld$, ed_ld$, bg_ld_d$, ed_ld_d$,   ~
                      scr_ear_dte$, address$(), address1$, state$, cutoff$,~
                      c8$(), c9$(), c10$(), c11$(), c12$(), sc_key$,     ~
                      bg_tme$, ed_tme$, c13$(), c14$(), c15$(), c16$(),  ~
                      c17$(), c18$(), cust_address$(), deliver$, c19$()

/* (AWD003)  */
            init(" ") ship_to$


            tt, or_cost = 0.0                    /* Total Run Net $$$  */
            analysis% = 0%                       /* Set Default to S.O.*/
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
        dataload                         /* (APCPLNOR) - File          */
            read #1,key 4% = or_so$, eod goto L30400
            or_days% = 0%
        get #1, using L35040  ,                                           ~
            or_due$,                     /* S.O. Due Date/Delivery Date*/~
            or_region$,                  /* Customer Region Code       */~
            or_route$,                   /* Customer Route Code        */~
            or_zip$,                     /* Customer Zip Code          */~
            or_drop$,                    /* Customer Drop Number       */~
            or_cuscode$,                 /* Customer Number            */~
            or_po$,                      /* Customer P.O. Number       */~
            or_so$,                      /* Customer S.O. Number       */~
            or_status$,                  /* Current S.O. Stat PLAN STAT*/~
            or_dte$,                     /* Date Assoc. with Stat Chg  */~
            or_inv$,                     /* APC Invoice Assoc. with SO */~
            or_chk$,                     /* APC Check Assoc. with Invoi*/~
            or_sls$,                     /* APC Salesman Code          */~
            or_fob$,                     /* Cust Delivery Cde PLAN DEL?*/~
            or_hows$,                    /* Spec. Instr (PLAN HOWS)    */~
            or_load$,                    /* Load N. Sched./Assigned    */~
            or_units,                    /* Total Loading Units S.O.   */~
            or_value,                    /* Total Price S.O. (Net)     */~
            or_cost,                     /* Total Cost S.O.            */~
            or_mak%,                     /* Total Make Quantity S.O.   */~
            or_pul%,                     /* Total Pull Quantity S.O.   */~
            or_date$,                    /* Date S.O. Created          */~
            or_chg$,                     /* Date S.O. Last Modified    */~
            or_userid$,                  /* S.O. Last Modified By User */~
            or_bol$,                     /* Date B.O.L. Created        */~
            or_text$,                    /* S.O. Header Text Id        */~
            or_days%,                    /* Actual No. Delivery Days   */~
            or_special$,                 /* Special Products Flags     */~
            or_fil$                      /* Filler Area                */
            x$ = or_due$
            call "DATEFMT" (x$)
L30400: return

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
                                         /* APCPLNOR - New File Layout */
L35040:     FMT CH(08),                  /* S.O. Due Date/Delivery Date*/~
                CH(02),                  /* Customer Region Code       */~
                CH(05),                  /* Customer Route Code        */~
                CH(09),                  /* Customer Zip Code          */~
                CH(02),                  /* Customer Drop Number       */~
                CH(09),                  /* Customer Number            */~
                CH(16),                  /* Customer P.O. Number       */~
                CH(08),                  /* Customer S.O. Number       */~
                CH(02),                  /* Current S.O. Stat PLAN STAT*/~
                CH(08),                  /* Date Assoc. with Stat Chg  */~
                CH(08),                  /* APC Invoice Assoc. with SO */~
                CH(08),                  /* APC Check Assoc. with Invoi*/~
                CH(04),                  /* APC Salesman Code          */~
                CH(02),                  /* Cust Delivery Cde PLAN DEL?*/~
                CH(02),                  /* Spec. Instr (PLAN HOWS)    */~
                CH(05),                  /* Load N. Sched./Assigned    */~
                PD(14,4),                /* Total Loading Units S.O.   */~
                PD(14,4),                /* Total Price S.O. (Net)     */~
                PD(14,4),                /* Total Cost S.O.            */~
                BI(2),                   /* Total Make Quantity S.O.   */~
                BI(2),                   /* Total Pull Quantity S.O.   */~
                CH(08),                  /* Date S.O. Created          */~
                CH(08),                  /* Date S.O. Last Modified    */~
                CH(03),                  /* S.O. Last Modified By User */~
                CH(08),                  /* Date B.O.L. Created        */~
                CH(04),                  /* S.O. Header Text Id        */~
                BI(2),                   /* Actual No. Delivery Days   */~
                CH(10),                  /* Special Product Flags      */~
                CH(01)                   /* Filler Area                */


                                         /* APCPLNSC - File            */
        FMT CH(6),                       /* Delivery/Production Date   */~
            CH(5),                       /* Sched Load No. Def ='99999'*/~
            CH(2),                       /* Customer Drop No. Def='00  */~
            CH(5),                       /* Sched Drop Seq. P.O./S.O.  */~
            CH(5),                       /* Customer Product Sort Seq. */~
            CH(25),                      /* MFG Part Number            */~
            CH(9),                       /* Customer Code              */~
            CH(8),                       /* Customer S.O. Number       */~
            CH(2),                       /* S.O. Line Item No.         */~
            BI(2),                       /* Total Line Item Quantity   */~
            BI(2),                       /* Total Make Qty Line Item   */~
            BI(2),                       /* Total Pull Qty Line Item   */~
            PD(14,4),                    /* Total Line Item Price(Net) */~
            PD(14,4),                    /* Total Line Item Cost       */~
            PD(14,4),                    /* Total Line Item Units      */~
            CH(4),                       /* Line Item Text Id          */~
            CH(1),                       /* Inventory Updated (Y or N) */~
            CH(5),                       /* Parent Load No. Def='99999'*/~
            CH(2),                       /* Sched Line Item Status Code*/~
            CH(6),                       /* Sched Line Item Status Date*/~
            CH(10),                      /* Special Product Flags      */~
            CH(3)                         /* Filler Area                */


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
              on fieldnr% gosub L40200,         /* Beg/End Dispatch Dte */~
                                L40200,         /* Beg     Load Number  */~
                                L40200,         /* End     Load Number  */~ 
                                L40200,         /* Beg Start Time       */~
                                L40200          /* End Start Time       */

              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Beg Earliest Dispatch:",                     ~
               at (03,25), fac(lfac$(1%)), bg_ld_dte$           , ch(10),~
                                                                         ~
               at (03,40), "End Earliest Dispatch:",                     ~
               at (03,63), fac(lfac$(1%)), ed_ld_dte$           , ch(10),~
                                                                         ~
               at (04,02), "Begin Load Number    :",                     ~
               at (04,25), fac(lfac$(2%)), bg_ld$               , ch(05),~
               at (04,35), fac(hex(84)),   bg_ld_d$             , ch(30),~
                                                                         ~
               at (05,02), "Ending Load Number   :",                     ~
               at (05,25), fac(lfac$(3%)), ed_ld$               , ch(05),~
               at (05,35), fac(hex(84)),   ed_ld_d$             , ch(30),~
                                                                         ~
               at (06,02), "Begin Start Time     :",                     ~
               at (06,25), fac(lfac$(4%)), bg_tme$              , ch(04),~
                                                                         ~
               at (07,02), "Ending Start Time    :",                     ~
               at (07,25), fac(lfac$(5%)), ed_tme$              , ch(04),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 15 then goto L40790
                  call "PRNTSCRN"


L40790:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40990     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            return

L40990: if fieldnr% > 0% then L41100  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(9)Appian Load Analysis                 " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffff09ffffffffff0f1000)
            return
L41100:
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *          D i s p l a y   A n a l y s i s   D a t a        *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
L42025:       gosub set_pf2
              gosub'060(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              lfac$(fieldnr%) = hex(81)                  /* Upper Only */

            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), line1$                 , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(50),~
               at (02,60), fac(hex(8c)), page$                  , ch(16),~
                                                                         ~
               at (03,02), "Beg/End Dis Dte:  ",                         ~
               at (03,20), fac(hex(84)), bg_ld_dte$             , ch(10),~
               at (03,31), fac(hex(84)), ed_ld_dte$             , ch(10),~
                                                                         ~
               at (04,02), "Beg/End LD Num :  ",                         ~
               at (04,20), fac(hex(84)), bg_ld$                 , ch(05),~
               at (04,31), fac(hex(84)), ed_ld$                 , ch(05),~
                                                                         ~
               at (05,02), "Beg/End Time   :  ",                         ~
               at (05,20), fac(hex(84)), bg_tme$                , ch(04),~
               at (05,31), fac(hex(84)), ed_tme$                , ch(04),~
                                                                         ~
               at (03,42), "Tot Loading Units:",                         ~
               at (03,61), fac(hex(84)), tot_units$             , ch(12),~
                                                                         ~
               at (04,42), "Tot Pull Units   :",                         ~
               at (04,61), fac(hex(84)), tot_pull$              , ch(12),~
                                                                         ~
               at (05,42), "Tot Make Units   :",                         ~
               at (05,61), fac(hex(84)), tot_make$              , ch(12),~
                                                                         ~
               at (06,42), "Tot Product Units:",                         ~
               at (06,61), fac(hex(84)), tot_product$           , ch(12),~
                                                                         ~
               at (07,42), "Tot Dollar Value.:",                         ~
               at (07,61), fac(hex(84)), tot_prod_value$        , ch(12),~
                                                                         ~
               at (09,02), fac(hex(a4)), h1$                    , ch(03),~
               at (09,06), fac(hex(a4)), h2$                    , ch(10),~
               at (09,17), fac(hex(a4)), h8$                    , ch(05),~
               at (09,23), fac(hex(a4)), h3$                    , ch(05),~
               at (09,29), fac(hex(a4)), h4$                    , ch(09),~
               at (09,39), fac(hex(a4)), h5$                    , ch(25),~
               at (09,65), fac(hex(a4)), h6$                    , ch(08),~
               at (09,74), fac(hex(a4)), h7$                    , ch(06),~
                                                                         ~
               at (10,03), fac(lfac$(1%)), c1$( 1% + kk%)       , ch(01),~
               at (10,06), fac(hex(84)),   c2$( 1% + kk%)       , ch(10),~
               at (10,17), fac(hex(84)),  c13$( 1% + kk%)       , ch(05),~
               at (10,23), fac(hex(84)),   c3$( 1% + kk%)       , ch(05),~
               at (10,29), fac(hex(84)),   c4$( 1% + kk%)       , ch(09),~
               at (10,39), fac(hex(84)),   c5$( 1% + kk%)       , ch(25),~
               at (10,65), fac(hex(84)),   c6$( 1% + kk%)       , ch(08),~
               at (10,74), fac(hex(84)),str(c7$(1% + kk%),2%,6%), ch(06),~
                                                                         ~
               at (11,03), fac(lfac$(1%)), c1$( 2% + kk%)       , ch(01),~
               at (11,06), fac(hex(84)),   c2$( 2% + kk%)       , ch(10),~
               at (11,17), fac(hex(84)),  c13$( 2% + kk%)       , ch(05),~
               at (11,23), fac(hex(84)),   c3$( 2% + kk%)       , ch(05),~
               at (11,29), fac(hex(84)),   c4$( 2% + kk%)       , ch(09),~
               at (11,39), fac(hex(84)),   c5$( 2% + kk%)       , ch(25),~
               at (11,65), fac(hex(84)),   c6$( 2% + kk%)       , ch(08),~
               at (11,74), fac(hex(84)),str(c7$(2% + kk%),2%,6%), ch(06),~
                                                                         ~
               at (12,03), fac(lfac$(1%)), c1$( 3% + kk%)       , ch(01),~
               at (12,06), fac(hex(84)),   c2$( 3% + kk%)       , ch(10),~
               at (12,17), fac(hex(84)),  c13$( 3% + kk%)       , ch(05),~
               at (12,23), fac(hex(84)),   c3$( 3% + kk%)       , ch(05),~
               at (12,29), fac(hex(84)),   c4$( 3% + kk%)       , ch(09),~
               at (12,39), fac(hex(84)),   c5$( 3% + kk%)       , ch(25),~
               at (12,65), fac(hex(84)),   c6$( 3% + kk%)       , ch(08),~
               at (12,74), fac(hex(84)),str(c7$(3% + kk%),2%,6%), ch(06),~
                                                                         ~
               at (13,03), fac(lfac$(1%)), c1$( 4% + kk%)       , ch(01),~
               at (13,06), fac(hex(84)),   c2$( 4% + kk%)       , ch(10),~
               at (13,17), fac(hex(84)),  c13$( 4% + kk%)       , ch(05),~
               at (13,23), fac(hex(84)),   c3$( 4% + kk%)       , ch(05),~
               at (13,29), fac(hex(84)),   c4$( 4% + kk%)       , ch(09),~
               at (13,39), fac(hex(84)),   c5$( 4% + kk%)       , ch(25),~
               at (13,65), fac(hex(84)),   c6$( 4% + kk%)       , ch(08),~
               at (13,74), fac(hex(84)),str(c7$(4% + kk%),2%,6%), ch(06),~
                                                                         ~
               at (14,03), fac(lfac$(1%)), c1$( 5% + kk%)       , ch(01),~
               at (14,06), fac(hex(84)),   c2$( 5% + kk%)       , ch(10),~
               at (14,17), fac(hex(84)),  c13$( 5% + kk%)       , ch(05),~
               at (14,23), fac(hex(84)),   c3$( 5% + kk%)       , ch(05),~
               at (14,29), fac(hex(84)),   c4$( 5% + kk%)       , ch(09),~
               at (14,39), fac(hex(84)),   c5$( 5% + kk%)       , ch(25),~
               at (14,65), fac(hex(84)),   c6$( 5% + kk%)       , ch(08),~
               at (14,74), fac(hex(84)),str(c7$(5% + kk%),2%,6%), ch(06),~
                                                                         ~
               at (15,03), fac(lfac$(1%)), c1$( 6% + kk%)       , ch(01),~
               at (15,06), fac(hex(84)),   c2$( 6% + kk%)       , ch(10),~
               at (15,17), fac(hex(84)),  c13$( 6% + kk%)       , ch(05),~
               at (15,23), fac(hex(84)),   c3$( 6% + kk%)       , ch(05),~
               at (15,29), fac(hex(84)),   c4$( 6% + kk%)       , ch(09),~
               at (15,39), fac(hex(84)),   c5$( 6% + kk%)       , ch(25),~
               at (15,65), fac(hex(84)),   c6$( 6% + kk%)       , ch(08),~
               at (15,74), fac(hex(84)),str(c7$(6% + kk%),2%,6%), ch(06),~
                                                                         ~
               at (16,03), fac(lfac$(1%)), c1$( 7% + kk%)       , ch(01),~
               at (16,06), fac(hex(84)),   c2$( 7% + kk%)       , ch(10),~
               at (16,17), fac(hex(84)),  c13$( 7% + kk%)       , ch(05),~
               at (16,23), fac(hex(84)),   c3$( 7% + kk%)       , ch(05),~
               at (16,29), fac(hex(84)),   c4$( 7% + kk%)       , ch(09),~
               at (16,39), fac(hex(84)),   c5$( 7% + kk%)       , ch(25),~
               at (16,65), fac(hex(84)),   c6$( 7% + kk%)       , ch(08),~
               at (16,74), fac(hex(84)),str(c7$(7% + kk%),2%,6%), ch(06),~
                                                                         ~
               at (17,03), fac(lfac$(1%)), c1$( 8% + kk%)       , ch(01),~
               at (17,06), fac(hex(84)),   c2$( 8% + kk%)       , ch(10),~
               at (17,17), fac(hex(84)),  c13$( 8% + kk%)       , ch(05),~
               at (17,23), fac(hex(84)),   c3$( 8% + kk%)       , ch(05),~
               at (17,29), fac(hex(84)),   c4$( 8% + kk%)       , ch(09),~
               at (17,39), fac(hex(84)),   c5$( 8% + kk%)       , ch(25),~
               at (17,65), fac(hex(84)),   c6$( 8% + kk%)       , ch(08),~
               at (17,74), fac(hex(84)),   c7$( 8% + kk%)       , ch(06),~
                                                                         ~
               at (18,03), fac(lfac$(1%)), c1$( 9% + kk%)       , ch(01),~
               at (18,06), fac(hex(84)),   c2$( 9% + kk%)       , ch(10),~
               at (18,17), fac(hex(84)),  c13$( 9% + kk%)       , ch(05),~
               at (18,23), fac(hex(84)),   c3$( 9% + kk%)       , ch(05),~
               at (18,29), fac(hex(84)),   c4$( 9% + kk%)       , ch(09),~
               at (18,39), fac(hex(84)),   c5$( 9% + kk%)       , ch(25),~
               at (18,65), fac(hex(84)),   c6$( 9% + kk%)       , ch(08),~
               at (18,74), fac(hex(84)),str(c7$(9% + kk%),2%,6%), ch(06),~
                                                                         ~
               at (19,03), fac(lfac$(1%)), c1$(10% + kk%)       , ch(01),~
               at (19,06), fac(hex(84)),   c2$(10% + kk%)       , ch(10),~
               at (19,17), fac(hex(84)),  c13$(10% + kk%)       , ch(05),~
               at (19,23), fac(hex(84)),   c3$(10% + kk%)       , ch(05),~
               at (19,29), fac(hex(84)),   c4$(10% + kk%)       , ch(09),~
               at (19,39), fac(hex(84)),   c5$(10% + kk%)       , ch(25),~
               at (19,65), fac(hex(84)),   c6$(10% + kk%)       , ch(08),~
               at (19,74), fac(hex(a4)),str(c7$(10% + kk%),2%,6%), ch(06),~
                                                                         ~
               at (20,71), fac(hex(84)),   pg_tot_unit$         , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 1% then goto L42830            /* Startover*/
                  gosub startover
                  goto L42025

L42830:        if keyhit% <> 2% then goto L42850            /* First    */
L42835:           kk% = 0%
                  goto L42025

L42850:        if keyhit% <> 3% then goto L42875            /* Last      */
L42855:           x% = int(val_max% / 10%)
                  kk% = (x%*10%)
                  goto L42025

L42875:        if keyhit% <> 4% then goto L42905            /* Previous */
                  if kk% < 11% then goto L42835
                  kk% = kk% - 10%
                  if kk% <= 1% then goto L42835
                  goto L42025

L42905:        if keyhit% <> 5% then goto L42930            /* Next     */
                  kk% = kk% + 10%
                  if kk% < val_max% then goto L42025
                  goto L42855

L42930:
        REM - Analysis  = 0%
                                   /* Include S.O. 'Non-Blank'         */
                                   /* Skip 'Blank' Process 'Non-Blank' */
               if keyhit% <> 9%  then goto L43070
                  gosub process_data
                  goto L43190
                                   /* Exclude S.O. 'Non-Blank'         */
                                   /* Skip 'Non-Blank', Process 'Blank'*/
L43070:        if keyhit% <> 10% then goto L43085
                  gosub process_data
                  goto L43190
L43085: 
               if keyhit% <> 15 then goto L43180
                  call "PRNTSCRN"
                  goto L42025

L43180:        if keyhit% <> 16% then goto L42025

L43190:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf2
            line1$ =                                                     ~
                  "(New) Appian Build Order File  - Display"             ~

            page$ ="Page: xxx of xxx"
            x1% = (kk% / 10%) + 1%
            x2% = (val_max% / 10%) + 1%
            if x2% < 1% then x2% = 1%
            convert x1% to str(page$,7%,3%), pic(###)
            convert x2% to str(page$,14%,3%), pic(###)

            pg_tot_unit = 0.0 : pg_tot_value = 0.0
            for n% = 1% to 10%
                y1 = 0% : x1 = 0.0
                convert c7$(n% + kk%) to y1, data goto L43305
L43305:
REM             convert c14$(n% + kk%) to x1,  data goto L43315
REM L43315
                pg_tot_unit  = pg_tot_unit + y1
                pg_tot_value = pg_tot_value + x1
            next n%
            convert pg_tot_unit to pg_tot_unit$, pic(######.##)
            convert pg_tot_value to pg_tot_value$, pic(######.##-)

            pf$(1%) = "(1)Start Over  (4)Previous              " &       ~
                      "                                       "
            pf$(2%) = "(2)First       (5)Next       (9)Include " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(3)Last                      (10)Exclude" &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(0102030405ffffff090aff0cffff0f1000)
            gosub check_screen
        return

        check_screen
            if analysis% = 0% then gosub no_delete
            if analysis% = 1% then gosub no_i_e
            if str(or_load$,1%,3%) = "N/A" then gosub no_i_e
            if val_max% > 10% then goto L43450
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L43450:      if kk% >= 10% then goto L43465
                gosub no_first
                gosub no_prev
L43465:      if (kk% + 10%) <= val_max% then goto L43475
                gosub no_last
L43475:      if kk% <= (val_max% - 10%) then goto L43485
                gosub no_next
L43485: return
        no_delete
            str(pf$(1%),46%,16%)= " " : str(pfkeys$,11%,1%) = hex(ff)
            str(pf$(2%),46%,16%)= " " : str(pfkeys$,12%,1%) = hex(ff)
            str(pf$(3%),16%,14%)= " " : str(pfkeys$, 7%,1%) = hex(ff)
        return
        no_i_e
            str(pf$(2%),30%,11%)= " " : str(pfkeys$, 9%,1%) = hex(ff)
            str(pf$(3%),30%,11%)= " " : str(pfkeys$,10%,1%) = hex(ff)
        return
        no_first
            str(pf$(2%),1%,10%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
            str(pf$(2%),16%,12%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last
            str(pf$(3%),1%,10%)  = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
            str(pf$(1%),16%,12%) = " " : str(pfkeys$,4%,1%) = hex(ff)
        return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50150,         /* Beg/End Load Date     */~
                              L50500,         /* Beg     Load Number   */~
                              L50900,         /* End     Load Number   */~
                              L51000,         /* Beg Start Time        */~
                              L51100          /* End Start Time        */

            return

L50150: REM Beg/End   Load Date                   BG_LD_DTE$, ED_LD_DTE$
            if bg_ld_dte$ > " " then goto L50190                                 
               bg_ld_dte$ = date$                                                

L50190:     date% = 0%
            call "DATEOKC" (bg_ld_dte$, date%, errormsg$)                        
            if date% = 0% then goto date_error
            bg_dte$ = bg_ld_dte$
            call "DATUFMTC" (bg_dte$)                                         
        REM Ending Due/Delivery Date              ED_DUE$, ED_DTE$
            if len(ed_ld_dte$) < 6 then ed_ld_dte$ = bg_ld_dte$
            date% = 0%
            call "DATEOKC" (ed_ld_dte$, date%, errormsg$)
            if date% = 0% then goto date_error
            ed_dte$ = ed_ld_dte$
            call "DATUFMTC" (ed_dte$)                                         
            if bg_dte$ > ed_dte$ then goto L50330
        return
L50330:     errormsg$ = "(Error) - Invalid Load Date Range??"
        date_error
            gosub error_prompt
            init(" ") bg_ld_dte$, bg_dte$, ed_ld_dte$, ed_dte$
        return

L50500: REM Beg     Load Number                   BG_LD$         
            if bg_ld$ <> " " then goto L50510
               bg_ld$ = "N/A  "
REM               ed_ld$ = "N/A  "
               bg_ld_d$ = "Not Applicable at This Time"
               ed_ld_d$ = "Not Applicable at This Time"
               return
L50510:     if str(bg_ld$,1%,5%) = "N/A  " then return
            convert bg_ld$ to bg_ld%, data goto L50550

            convert bg_ld% to bg_ld$, pic(00000)
            goto L50590
L50550:     convert str(bg_ld$,2%,4%) to bg_ld%, data goto load_error

            convert bg_ld% to str(bg_ld$,2%,4%), pic(0000)
L50590:     init(" ") ld$
            ld$ = bg_ld$
            gosub lookup_load_desc
            bg_ld_d$ = load_d$
        return
        load_error
          errormsg$="(Error) - Invalid Load Number, Not Defined?"
          gosub error_prompt
          init(" ") bg_ld$, ed_ld$              
        return

L50900: REM End     Load Number                   ED_LD$         
            if ed_ld$ <> " " and bg_ld$ <> "N/A" then goto L50910
L50980:        bg_ld$ = "N/A  "
               ed_ld$ = "N/A  "
               bg_ld_d$ = "Not Applicable at This Time"
               ed_ld_d$ = "Not Applicable at This Time"
               return
L50910:     if str(ed_ld$,1%,5%) = "N/A  " then goto L50980
            convert ed_ld$ to ed_ld%, data goto L50950

            convert ed_ld% to ed_ld$, pic(00000)
            goto L50990
L50950:     convert str(ed_ld$,2%,4%) to ed_ld%, data goto load_error

            convert ed_ld% to str(ed_ld$,2%,4%), pic(0000)
L50990:     init(" ") ld$
            ld$ = ed_ld$
            gosub lookup_load_desc
            ed_ld_d$ = load_d$
        return

L51000: REM Verify Appian Load Start Time             BG_TME$   
            if bg_tme$ <> " " then goto L51010
               bg_tme$ = "N/A"
               return
L51010:
            hh%, mm% = 0%
            convert str(bg_tme$,1%,2%) to hh%, data goto L51090

            convert str(bg_tme$,3%,4%) to mm%, data goto L51090

            if mm% > 60% or mm% < 0% then goto L51090
            if hh% > 24% or hh% < 0% then goto L51090

        return
L51090:    errormsg$ = "(Error) - Invalid Appian Load Beginning Time?"
           gosub error_prompt
           init(" ") bg_tme$        
        return

L51100: REM Verify Appian Load End Time              ED_TME$
            if ed_tme$ <> " " and bg_tme$ <> "N/A" then goto L51110
L51150:        ed_tme$ = "N/A"
               bg_tme$ = "N/A"
               return
L51110:     if ed_tme$ = "N/A" then goto L51150
            hh%, mm% = 0%
            convert str(ed_tme$,1%,2%) to hh%, data goto L51190

            convert str(ed_tme$,3%,4%) to mm%, data goto L51190

            if mm% > 60% or mm% < 0% then goto L51190
            if hh% > 24% or hh% < 0% then goto L51190

        return
L51190:    errormsg$ = "(Error) - Invalid Appian Load Ending Time?"
           gosub error_prompt
           init(" ") ed_tme$        
        return

        lookup_load_desc
REM - LOOK-UP LOAD
            read #5,key = ld$, using L51220, load_d$, eod goto load_error
L51220:        FMT POS(16), CH(30)
        return


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************


        scan_appian_loads
            str(app_ld_key$,1%,16%) = all(hex(00))
            str(app_ld_key$,1%,1%) = "0"
        scan_appian_nxt
            read #6,key 1% > app_ld_key$, using L61120, app_ld_key$,       ~
                                                 eod goto scan_appian_done
L61120:         FMT CH(16)

REM                if str(app_ld_key$,1%,1%) <> "0" then goto scan_appian_nxt

                if str(app_ld_key$,2%,6%) > ed_dte$                        ~
                                                then goto scan_appian_done
                if str(bg_ld$,1%,3%) <> "N/A" and                       ~
                   str(app_ld_key$,12%,5%) < bg_ld$ then goto scan_appian_nxt
                if str(ed_ld$,1%,3%) <> "N/A" and                       ~
                   str(app_ld_key$,12%,5%) > ed_ld$ then goto scan_appian_nxt


                if str(bg_tme$,1%,3%) <> "N/A" and                       ~
                   str(app_ld_key$,8%,4%) < bg_tme$ then goto scan_appian_nxt
                if str(ed_tme$,1%,3%) <> "N/A" and                       ~
                   str(app_ld_key$,8%,4%) > ed_tme$ then goto scan_appian_nxt

                                                         /* Prime Pump   */
            init(" ") sc_key1$, sc_part$
            str(sc_key1$,1%,5%) = str(app_ld_key$,12%,5%)
        scan_orders_nxt


          read #3,key 1% > sc_key1$, using L61130, sc_key1$,               ~
                                            eod goto scan_appian_nxt
L61130:        FMT POS(7), CH(27)                    

          cnt% = cnt% + 1%
          if mod(cnt%,100%) <> 0% then goto L61155
             convert cnt% to str(cnt$,19%,8%), pic(########)
             print at(02,02);hex(84);cnt$;

L61155:      if str(sc_key1$,1%,5%) <> str(app_ld_key$,12%,5%) then      ~
                                                goto scan_appian_nxt

             if str(or_so$,1%,8%) = str(sc_key1$,18%,8%) then goto scan_orders_nxt   
             or_so$ = str(sc_key1$,18%,8%)
             gosub dataload                    /* Lookup OR Information */
                                               /* Planned to Loaded Stat*/
             if or_status$ < "03" or or_status$ > "16"                    ~
                                               then goto scan_orders_nxt


             for i% = 1% to cnt_max%
                if str(or_cuscode$,1%,9%) = str(exc_cust$(i%),1%,9%) then ~
                                                     goto scan_orders_nxt
             next i%

             for i% = 1% to cnt_max_how%
                if str(or_hows$,1%,9%) = str(exc_cust$(i%),1%,9%) then ~
                                                     goto scan_orders_nxt
             next i%

             if str(or_route$,1%,5%) = " " then goto scan_orders_nxt

             cc% = cc% + 1%
             c1$(cc%) = " "
             x% = 0%
             convert str(sc_key1$,6%,5%) to x%, data goto L61280
L61280:
             scr_ear_dte$ = str(app_ld_key$,2%,6%)
             call "DATFMTC" (scr_ear_dte$)
             c2$(cc%) = scr_ear_dte$
             c3$(cc%) = or_route$                        /* Route Code */
             c4$(cc%) = or_cuscode$                      /* Customer   */
             or_cuscode$ = c4$(cc%)
             gosub lookup_customer
             gosub lookup_bckmastr
/* (AWD004)  */
/* (AWD006) */
             ship_to$ = "   "
             if str(how_ship$, 1, 2) = "88" then ship_to$ = "999"             

REM             if str(ship_to$,1%,3%) <> "999" then str(ship_to$,1%,3%) = "   "
/* (AWD006) - end */
             str(c18$(cc%),1%,3%) = ship_to$

             c5$(cc%) = desc$                            /* Cust Name  */
             c6$(cc%) = or_so$                           /* Sales Order*/
/* (AWD008) */
REM put lookup deliver after setting c5$ and c6$ not to mess up customer name and info
             gosub lookup_deliver                        /* (AWD009) */
             gosub lookup_cutoff
REM             or_units = 0.00
/*@@@*/      gosub scan_order_detail
             convert or_units to c7$(cc%), pic(####.00)    /* S.O. Units */
             convert or_value to c14$(cc%), pic(####.00)   /* S.O. Units */
             c8$(cc%) = address$(1%)                       /* Address    */
             c15$(cc%) = address$(2%)                       /* Address    */
             c9$(cc%) = city$                              /* City       */
             c10$(cc%) = state$                            /* State      */
             c11$(cc%) = zip$                              /* Zip        */
             c12$(cc%) = cutoff$                           /* Cutoff Code*/
             c13$(cc%) = or_load$                          /* Load Num   */
             c16$(cc%) = or_po$                            /* PO Number  */
             c19$(cc%) = deliver$                          /* (AWD009) deliver */
                                                      /* Screen Totals */

             tot_units      = round(tot_units + or_units, 2)
             tot_make%      = tot_make%    + or_mak%
             tot_pull%      = tot_pull%    + or_pul%
             tot_product%   = tot_product% + (or_mak% + or_pul%)

             or_product% = 0%
             or_product% = (or_mak% + or_pul%)
/*@@@*/      or_product% = des_cnt%
/*@@@*/      convert or_product% to c17$(cc%), pic(#######.00)

             tot_prod_value = round(tot_prod_value + or_value, 2)
REM             convert (x% + 1%) to str(sc_key1$,6%,5%), pic(00000)
             goto scan_orders_nxt
        scan_appian_done
           val_max% = cc%
           convert tot_units to tot_units$, pic(########.##-)
           convert tot_make% to tot_make$, pic(############)
           convert tot_pull% to tot_pull$, pic(############)
           convert tot_product% to tot_product$, pic(############)
           convert tot_prod_value to tot_prod_value$, pic(########.##-)
           kk% = 0%
        return


        lookup_customer
           desc$, cust_address$() = " "              /* (AWD007) */ /*(AWD009)*/
           read #4,key = or_cuscode$, using L61450, desc$, cust_address$(), ~
                                custcity$, custst$, custzip$, ~
                                cutoff$, deliver$, eod goto L61455
L61450:       FMT POS(253), CH(25), XX(5), 2*CH(30), ~
                  POS(403), CH(18), CH(02), POS(424), CH(09), ~
                        POS(860), CH(02),      ~
                        POS(880), CH(02)            /* CR1104 city st zip */
L61455: return

        lookup_deliver
            init(" ")genkey$, desc$
            p%, deliver% = 0%
            desc$ = "00-One   Wk-(00)-Days"
            str(genkey$,1%,9%)   = "PLAN DELV"
            str(genkey$,10%,15%) = deliver$
            read #2,key = genkey$, using L63750 , desc$, eod goto noDeliver

            p% = 0%
            p% = pos(desc$ = "(")
            if p% = 0% then goto noDeliver
            convert str(desc$,(p%+1%),2%) to deliver%, data goto noDeliver

            convert deliver% to deliver$, pic(00)
        return
        noDeliver
          deliver$ = "00"
        return



        lookup_cutoff
            init(" ")genkey$, desc$
            p%, cutoff% = 0%
            desc$ = "0-(XXX)-UnAssigned"
            str(genkey$,1%,9%)   = "PLAN CUTO"
            str(genkey$,10%,15%) = cutoff$
            read #2,key = genkey$, using L63750 , desc$, eod goto noCutoff

            convert str(desc$,1%,1%) to cutoff%, data goto noCutoff

            convert cutoff% to cutoff$, pic(00)
        return
        noCutoff
          cutoff$ = "00"
        return

        lookup_bckmastr
           bckmst_key$, name$, address$(), address1$ = " "
           city$, state$, zip$ = " "
           str(bckmst_key$,1%,9%) = or_cuscode$  
           str(bckmst_key$,10%,16%) = or_so$           
           read #9, key = bckmst_key$, using L61460, name$, address$(),     ~
                                       address1$, how_ship$, userwhoenter$, ~
                                       ship_to$,            /*(AWD007)*/    ~
                                       /* (AWD006) (AWD003) */~
                                        eod goto no_bckmstr
L61460:       FMT POS(42), CH(30), 2*CH(30), POS(192), CH(30), POS(422), ~
                  CH(02) , POS(836), CH(03), POS(908), CH(03)

/* (AWD007) */
/* CR2819       if userwhoenter$ = "M2O" then address$() = cust_address$()  */
/* CR2819       if userwhoenter$ = "STK" then address$() = cust_address$()  */
            
            for k% = 1% to 30%
               if str(name$,k%,1%) = "," then ~
                   str(name$,k%,1%) = " "
            next k%
/* CR1104 + */
            if or_cuscode$ = "AT0215" or or_cuscode$ = "AT0216"  ~
               then goto L61475 
/* CR1104 - */
           call "SPCESMSH" (address1$,1%,30%)
           p%, len% = 0%
           len% = len(address1$)
REM           p% = 1%
            for k% = 1% to 60%
                if str(address$(),k%,1%) = "," then ~
                   str(address$(),k%,1%) = " "
            next k%
            for k% = 1% to 30%
                if str(address1$,k%,1%) = "," then ~
                   str(address1$,k%,1%) = " "
            next k%


           for k% = 1% to len%
              p% = pos("0123456789" = str(address1$,k%,1%))
              if p% <> 0% then goto L61470
           next k%

L61470:    zip$   = str(address1$,k%,len%)
           state$ = str(address1$,k%-3%,2%)
           city$  = str(address1$,1%,k%-4%)
           return  
           
L61475:          /* CR 1104 get address for cross dock customer  */ 
           address$() = cust_address$()
           city$      = custcity$
           state$     = custst$
           zip$       = custzip$
        
        no_bckmstr
        return


        process_data
            call "SHOSTAT" ("Processing Include/Exclude Data")
            comma$ = "|"
            gosub write_header
            for i% = 1% to val_max%
                                              /* Include '*' Skip ' '  */
REM                if keyhit% =  9% and c1$(i%) = " " then goto L62295
                if keyhit% =  9% and c1$(i%) <> "X" then goto L62295
                                              /* Include ' ' Skip '*'  */
REM                if keyhit% = 10% and c1$(i%) <> " " then goto L62295
                if keyhit% = 10% and c1$(i%) = "X" then goto L62295

                   gosub write_upload_file
L62295:     next i%
        return

        write_upload_file

            write #7, using L63400,                                        ~
                      c4$(i%),  comma$,              /* customer code   */ ~
                      c6$(i%),  comma$,              /* sales order     */ ~
                      c5$(i%),  comma$,              /* customer name   */ ~
                      c8$(i%),  comma$,              /* customer addr1  */ ~
                      c15$(i%), comma$,              /* customer addr2  */ ~
                      c9$(i%),  comma$,              /* customer city   */ ~
                      c10$(i%), comma$,              /* customer state  */ ~
                      c11$(i%), comma$,              /* customer zip    */ ~
                      c12$(i%), comma$,              /* customer cutoff */ ~
                      c14$(i%), comma$,              /* SO Value        */ ~
                      c7$(i%),  comma$,              /* SO load units   */ ~
                      c13$(i%), comma$,              /* sales order load*/ ~
                      c16$(i%), comma$,              /* customer PO     */ ~
                      c17$(i%), comma$,              /* SO QTY          */ ~
/*(AWD004)*/          c18$(i%), comma$,              /* SO shipid       */ ~
                      c19$(i%), comma$,              /* customer deliver*/ ~
                                                           eod goto L63405


L63400:               FMT CH(9),  CH(1), CH(8),  CH(1),       ~
                          CH(25), CH(1), CH(30), CH(1),       ~
                          CH(30), CH(1), CH(25), CH(1),       ~
                          CH(2),  CH(1), CH(9),  CH(1),       ~
                          CH(2),  CH(1), CH(14), CH(1),       ~
                          CH(14), CH(1), CH(5),  CH(1),       ~
                          CH(16), CH(1), CH(10), CH(1),       ~
                          CH(3),  CH(1), CH(02), CH(1), CH(36)
        return
L63405:
                                                             /* (AWD001)  */
            errormsg$ = "(Error) Sales Order Skipped -->" & c6$(i%)
            gosub error_prompt
        return

        write_header
            write #7, using L63410, "Acct#", comma$, "Order#", comma$,     ~
                      "Name", comma$, "Address", comma$, "Address2",       ~
                      comma$, "City", comma$, "State", comma$, "Zip",      ~
                      comma$, "Cutoff", comma$, "Gross Open Amount",       ~
                      comma$, "Load Units", comma$, "Load Num", comma$,    ~
/*(AWD004)*/          "PO Num", comma$, "Tot Qty", comma$, "Ship To", comma$,~
                      "Deliver Days", comma$,  " "

L63410:               FMT CH(5), CH(1), CH(6), CH(1), CH(4), CH(1), CH(7),  ~
                          CH(1), CH(8), CH(1), CH(4), CH(1), CH(5), CH(1),  ~
                          CH(3), CH(1), CH(6), CH(1), CH(17), CH(1), CH(10), ~
                          CH(1), CH(8), CH(1), CH(6), CH(1), CH(7), CH(1),  ~ 
                          CH(7), CH(1), CH(12), CH(1), CH(24)
        return


        load_exclude_customers
            cnt% = 0%
            init(" ") genkey$
            str(genkey$,1%,9%) = "EWDAPPEXC"
        exclude_nxt
            read #2, key > genkey$, using L63550, genkey$,               ~
                                              eod goto exclude_done
L63550:                  FMT CH(24)

            if str(genkey$,1%,9%) <> "EWDAPPEXC" then goto exclude_done
            cnt% = cnt% + 1%
            str(exc_cust$(cnt%),1%,9%) = str(genkey$,10%,9%)
            goto exclude_nxt
        exclude_done
            cnt_max% = cnt%
        return

        load_exclude_hows
            cnt% = 0%
            init(" ") genkey$
            str(genkey$,1%,9%) = "EWDAPPHOW"
        exclude_how_nxt
            read #2, key > genkey$, using L63550, genkey$,               ~
                                              eod goto exclude_how_done

            if str(genkey$,1%,9%) <> "EWDAPPHOW" then goto exclude_how_done
            cnt% = cnt% + 1%
            str(exc_how$(cnt%),1%,2%) = str(genkey$,10%,2%)
            goto exclude_how_nxt
        exclude_how_done
            cnt_max_how% = cnt%
        return

        scan_order_detail
             init(" ") sc_key$
             sc_tqty% = 0%
	           des_cnt% = 0%
             str(sc_key$,1%,8%) = or_so$
        scan_order_nxt
             read #3, key > sc_key$, using L63720, sc_key$, sc_part$,    ~
                                    sc_tqty%, eod goto order_detail_done

L63720:                  FMT POS(24), CH(10), CH(25), POS(68), BI(2)

             if str(sc_key$,1%,8%) <> or_so$ then goto order_detail_done
/* (AWD008) */
            if len(sc_part$) < 19% then goto scan_order_nxt
            des_cnt% = des_cnt% + sc_tqty%
REM             gosub calc_cubic
             goto scan_order_nxt
        order_detail_done
        return
        calc_cubic
            sc_units = 0.0 : fact = 0.0 : a1% = 0% : a2% = 0%
            sc% = 0%
            fact = 3.25                     /* Set Default!!           */
            if len(sc_part$) < 19% then return
            des_cnt% = des_cnt% + sc_tqty%
            str(genkey$,1%,9%)   = "SYS CODES"
            str(genkey$,10%,15%) = str(sc_part$,1%,3%)
            read #2,key = genkey$, using L63750 , desc$, eod goto calc_fact

L63750:     FMT POS(25), CH(30)          /* GENCODES - Tables          */
            convert str(desc$,21%,5%) to fact, data goto calc_fact

        calc_fact                                /* Convert Width      */
            convert str(sc_part$,13%,4%) to a1%,data goto calc_width
        calc_width                               /* Convert Height     */
            convert str(sc_part$,17%,3%) to a2%,data goto calc_height
        calc_height                              /* Always Round Up    */
            a1% = a1% + 10%
            a2% = a2% + 10% 
                                                 /* Is this TSO, BSO, FGO */
                                                 /* If so take half height*/
            convert str(sc_part$,11%,1%) to sc%, data goto no_screen
        no_screen
            if sc% = 4% or sc% = 5% or sc% = 6% then a2% = round(a2% / 2%, 0)

/* (AWD002) - double if 9 for bay/bow */
            if str(sc_part$,1%,1%) = "9" then a2% = round(a2% * 2%, 0)

            unit_i%  = int(a1%/10) * int(a2%/10)
                                                 /* Is this Wood Surround */
            gosub lookup_mull 
/*(AWD005)*/

            if cubic_mull% = 1% then fact = fact + wd_sze


            fact     = round(fact,2)        /* Calc Line Item Units    */
                                            /* Divide by 144 to turn inches */
                                            /* back to feet                 */
REM            sc_units = round( ((fact * unit_i%)/144.0) * sc_tqty%,2)
            sc_units = round( ((fact * unit_i%)/1728.0) * sc_tqty%,2)
            or_units = round( or_units + sc_units, 2) /* Tot S.O. Units*/
        return

        lookup_mull
            cubic_mull% = 0% : wood% = 0%
            init(" ") genkey$, wood$
            if str(sc_part$,1%,1%) = "9" then return
            if len(sc_part$) < 22 then return
            if len(sc_part$) = 22 then wood$ = str(sc_part$,20%,3%)      ~
                                  else wood$ = str(sc_part$,23%,3%)
            convert wood$ to wood%, data goto L63780 

            if wood% > 1% and wood% < 81% then return

L63780:     if wood$ = "000" then return
               str(genkey$,1%,9%) = "APC WOOD "
               str(genkey$,10%,3%) = wood$
               read #2,key = genkey$,using L63750 , desc$,                ~
                                                eod goto lookup_mull_done


               gosub check_app_mull
        lookup_mull_done
        return
        check_app_mull
            cubic_mull% = 0%                                                       
            wd_sze = 4.00                    /* (AWD005) */
            init(" ") readkey$, desc$                                      
            str(genkey$,1%,9%) = "AWD WOOD "
            str(genkey$,10%,3%) = wood$
	    read #2,key = genkey$, using L63750 , desc$,                  ~
                                             eod goto no_app_mull

                  convert str(desc$,25%,6%) to wd_sze, data goto invalid_size
invalid_size:

            cubic_mull% = 1%
        no_app_mull
        return


        open_error
           comp% = 2%
           hdr$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        
        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_file
            init(" ") library$, volume$, file$
            library$        = "APPIAN  "
* (AWD003) - Next two lines
            if schema% = 1% then volume$         = "CARLO2"
            if schema% = 2% then volume$         = "NE2"

            file$   = "AWDAPPSN"
            call "OPENFILE" (#7, "IO   ", f2%(7%), rslt$(7%), axd$ )
            if f2%(7%) <> 0% then goto L63800
               gosub file_exists         
               if comp% <> 16% then goto exit_program
                  call "FILEBGON" (#7)

L63800:    open nodisplay #7, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return
       

        file_exists
          comp% = 2%                                   
          hdr$ = "*** APPIAN Order File Exists **"
          msg$(1%) = "        The File (AWDAPPSN) Already Exists.      "
          msg$(2%) = "       A P P I A N   S E N D   F I L E           "
          msg$(3%) = "Press <RETURN> To Exit Prog, or PF(16) to Delete."
          call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return


        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
