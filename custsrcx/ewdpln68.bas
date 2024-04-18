        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDPLN68                             *~
            *  Creation Date     - 02/15/99                             *~
            *  Last Mod Date     - 07/18/2013                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - History Sales Order Data Lookup      *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/15/99 ! (New) Program                            ! RHH *~
            * 04/16/99 ! (EWD001) - Mod to Warranty ID Lookup     ! RHH *~
            *          !    find S.O. Line Item.                  !     *~
            * 01/09/06 ! (PAR000) CR347 Mods for new part number  ! RHH *~
            *06/26/2007! (PAR001) Mod for 2005 split              ! DES *~
            *07/18/2013! (AWD002) mod to pass dummy dimensions to ! CMG *~
            *          !   awdpartn                               !     *~
            *04/12/2016! SR73678  mod to add file ARVHIST.        ! PWW *~
            *          !                                          !     *~
            *04/02/2017! remove all files except ARVHIST          !     *~
            *************************************************************

        dim                                                              ~
            rhh$80, cnt$18,              /* For Debug                  */~
            scr_sel$1, scr_sel_d$30,     /* History Lookup Selection   */~
            sl$(5%)24,                   /* Screen Display Text        */~
            se$(5%)16, se_d$(5%)30,      /* Screen Selection Data      */~
            se%(5%),                     /* Length of Selection Data   */~
            ss$(20%)40,                  /* Screen Text                */~
                                         /* (EWDHIST) Master S.O. Hist */~
            hs_rec$256,                  /* Record                     */~
            hs_key$32,                   /* Alt Key (1) SO,LN,YYYY     */~
            hs_job$16,                   /* Customer Job Name          */~
            hs_cust$9,                   /* Customer Code              */~
            hs_po$16,                    /* Customer P.O. Number       */~
            hs_ln$3,                     /* Sales Order Line Item      */~
            hs_yr$4,                     /* History Data Year (YYYY)   */~
            hs_load$5,                   /* Customer Load No.          */~
            hs_so$8,                     /* Customer Sales Order       */~
                                         /* hs_ln$                     */~
                                         /* hs_yr$                     */~
            hs_hdrt$4,                   /* Header Text Id             */~
            hs_linet$4,                  /* Line Item Text Id          */~
                                         /* hs_cust$                   */~
            hs_order$6,                  /* S.O. Order Date            */~
                                         /* hs_so$                     */~
                                         /* hs_ln$                     */~
            hs_ship$6,                   /* Sales Order Ship Date      */~
            hs_quote$10,                 /* EWD Quote Number           */~
            hs_part$25,                  /* MFG Part Number            */~
                                         /* hs_oqty% - Order Ln Qty    */~
                                         /* hs_sqty% - Shipped Ln Qty  */~
                                         /* hs_hdr_disc                */~
                                         /* hs_ln_disc                 */~
                                         /* hs_uprice - Ln Unit Price  */~
                                         /* hs_eprice - Ln Extnd Price */~
            hs_cat$4,                    /* Ln Item Category Code      */~
            hs_prc$1,                    /* Ln Item Price Code         */~
            hs_rgn$4,                    /* Region Code                */~
            hs_sls$4,                    /* Salesman Code              */~
            hs_inv$8,                    /* EWD Invoice Number         */~
            hs_chk$8,                    /* Customer Check No.         */~
            hs_due$6,                    /* Customer Due Date          */~
            hs_sub_part$20,              /* Sub Part Number    (PAR000)*/~
            hs_info_part$9,              /* Info Part Number   (PAR000)*/~
            hs_filler$13,                /* Filler Area        (PAR000)*/~
            warr_rec$32, warr_key$15,    /* Warranty Record            */~
            warranty$8, warranty_ln$3,   /* Save Warranty Info (EWD001)*/~
            warranty_so$8,               /* S.O. Assoc with Warranty   */~
            warr$(100%)50,               /* Warranty Numbers   (EWD001)*/~
            sav_warr$11,                 /* For Load Warranty ids      */~
            warr_1$11, warr_2$4,         /* Column Headers     (EWD001)*/~
                                         /* Lookup Customer Data       */~
            cust_key$9,                  /* Customer Primary Key       */~
            cust_name$30,                /* Customer Name              */~
            cust_city$18,                /* Customer City              */~
            cust_st$2,                   /* Customer State             */~
            cust_zip$9,                  /* Customer Zip Code          */~
                                         /*                            */~
            textid$4,                    /* S.O. HEADER TEXT ID        */~
            txtid$4,                     /* Argument                   */~
            text_rec$(3%)70,             /* TEXT RECORD BUFFER         */~
            text_key$11,                 /* TEXT KEY                   */~
            text$64,                     /* TEXT DEFINITION AREA       */~
            sav_txt$9,                   /* CHECK TEXT KEY             */~
            header$4,                    /* Store Header Text Id       */~
                                         /*                            */~
            tst_date$10, tst_dte$10,     /* Check Begin Dates          */~
            tst_date1$10, tst_dte1$10,   /* Check End Dates            */~
            tst_key$50,                  /* Use for Warranty           */~
            sav_key$50, tst_ky$50,       /* Check Line Items           */~
            sav_yr$4, tst_yr$4,          /* Check Year (YYYY)          */~
                                         /* Display Sales headers      */~
            h1$9, h2$16, h3$8, h4$10,    /* Col Headers                */~
            h5$8, h6$8, h7$5,            /* Col Headers                */~
            title$20,                    /*                            */~
                                         /*                            */~
            readkey$50, desc$32,         /* GENCODES Lookup            */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            dsp_msg$79,                  /* Screen Display Message     */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim cc$(504%)76,                 /* Lookup Display             */~
            c1$(504%)1,                  /* Lookup Selection           */~
            dt$(100%)120,                /* Detail Display     (PAR000)*/~
            hh$(15%)41, dth$(15%)25      /* Detail Display             */


        dim f2%(64%),                    /* = 0 if the file is open    */~
            f1%(64%),                    /* = 1 if READ was successful */~
            fs%(64%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "  Sales Order History Look-Up Utility   "
            pname$ = "EWDPLN68 - Rev: R7.00"

        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

          REM ***********************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! CUSTOMER ! Customer Master                          *~
            * #2  ! EWDHIST  ! (New) Master Sales Order History File    *~
            * #3  ! EWDTEXTH ! (New) Master History Text for S.O.       *~
            * #4  ! EWDWARR  ! (New) Master Warranty File Cross-Ref     *~
            * #5  ! GENCODES ! Master Table File                        *~
            * #10 ! EWDHIST  ! Use for Warranty Look Up                 *~
            * #63 ! BCKSUBPT ! New Sub Part Number File         (PAR000)*~
            *-----!----------!------------------------------------------*~
            * #18 ! AWDHIST  ! (New) Master Sales Order History File    *~
            * #20 ! AWDWARR  ! (New) Master Warranty File Cross-Ref     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key 1,  keypos = 10,   keylen =  30,         ~
                            key 2,  keypos = 424,  keylen =   9,         ~
                            key 3,  keypos = 771,  keylen =   9,         ~
                            key 4,  keypos = 780,  keylen =   9
                                                   /* (PAR000)          */
            select #2,  "EWDHIST",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   17, keylen =  32,                     ~
                        alt key  1, keypos =   54, keylen =  15,         ~
                            key  2, keypos =   49, keylen =  20,         ~
                            key  3, keypos =   77, keylen =  26,         ~
                            key  4, keypos =    1, keylen =  25, dup,    ~
                            key  5, keypos =  193, keylen =   8, dup
                                                   /* (PAR000)          */

            select #3, "EWDTEXTH",                                       ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen =  11
/*SR73678 + */
            select #24, "AWDTEXTH",                                       ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen =  11

            select #25, "ARVTEXTH",                                       ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen =  11
/*SR73678 - */
            select #4, "EWDWARR",                                        ~
                         varc,    indexed,  recsize =   32,              ~
                         keypos =    1, keylen =  15,                    ~
                         alt key 1, keypos =  16, keylen = 15, dup

            select #5,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24
                                                   /* (PAR000)          */
            select #10, "EWDHIST",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   17, keylen =  32,                     ~
                        alt key  1, keypos =   54, keylen =  15,         ~
                            key  2, keypos =   49, keylen =  20,         ~
                            key  3, keypos =   77, keylen =  26,         ~
                            key  4, keypos =    1, keylen =  25, dup,    ~
                            key  5, keypos =  193, keylen =   8, dup

            select #63, "BCKSUBPT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =  12, keylen =  11, dup,     ~
                            key  2, keypos =  23, keylen =  45, dup

                                                   /* (PAR000)          */
                                                   /* (PAR001)          */
            select #18,  "AWDHIST",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   17, keylen =  32,                     ~
                        alt key  1, keypos =   54, keylen =  15,         ~
                            key  2, keypos =   49, keylen =  20,         ~
                            key  3, keypos =   77, keylen =  26,         ~
                            key  4, keypos =    1, keylen =  25, dup,    ~
                            key  5, keypos =  193, keylen =   8, dup
/*SR73678 + */
            select #19,  "ARVHIST",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   17, keylen =  32,                     ~
                        alt key  1, keypos =   54, keylen =  15,         ~
                            key  2, keypos =   49, keylen =  20,         ~
                            key  3, keypos =   77, keylen =  26,         ~
                            key  4, keypos =    1, keylen =  25, dup,    ~
                            key  5, keypos =  193, keylen =   8, dup
/*SR73678 - */
            select #20, "AWDWARR",                                       ~
                         varc,    indexed,  recsize =   32,              ~
                         keypos =    1, keylen =  15,                    ~
                         alt key 1, keypos =  16, keylen = 15, dup
/*SR73678 + */
            select #21, "ARVWARR",                                       ~
                         varc,    indexed,  recsize =   32,              ~
                         keypos =    1, keylen =  15,                    ~
                         alt key 1, keypos =  16, keylen = 15, dup
/*SR73678 - */
            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 10%, rslt$(1%))
REM            CALL "OPENCHCK" (#2, FS%(2%), F2%(2%), 10%, RSLT$(2%))
REM            CALL "OPENCHCK" (#3, FS%(3%), F2%(3%), 10%, RSLT$(3%))
/*SR73678 + */
REM            CALL "OPENCHCK" (#24, FS%(24%), F2%(24%), 10%, RSLT$(24%))
            call "OPENCHCK" (#25, fs%(25%), f2%(25%), 10%, rslt$(25%))
/*SR73678 - */
REM            CALL "OPENCHCK" (#4, FS%(4%), F2%(4%), 10%, RSLT$(4%))
            CALL "OPENCHCK" (#5, fs%(5%), f2%(5%), 10%, rslt$(5%))

REM            CALL "OPENCHCK" (#10, FS%(10%), F2%(10%), 10%, RSLT$(10%))

                                                   /* (PAR000)          */
            call "OPENCHCK" (#63, fs%(63%), f2%(63%), 10%, rslt$(63%))
                                                   /* (PAR000)          */
                                                   /* (PAR001)          */
REM            CALL "OPENCHCK" (#18, FS%(18%), F2%(18%), 18%, RSLT$(18%))
/*SR73678*/ call "OPENCHCK" (#19, fs%(19%), f2%(19%), 19%, rslt$(19%))
REM            CALL "OPENCHCK" (#20, FS%(20%), F2%(20%), 20%, RSLT$(20%))
/*SR73678*/ call "OPENCHCK" (#21, fs%(21%), f2%(21%), 21%, rslt$(21%))
            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   2%
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
                  if keyhit%  = 10% then gosub process_data
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 2% then editpg1
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
            *             P R O C E S S   D A T A                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        process_data                             /* (EWDHIST)          */
            gosub lookup_history
            if hit% = 0% then goto L19000

            gosub display_history_orders

        return clear all
        goto inputmode

L19000: return clear all
        errormsg$ = "No Sales Orders Found ? ? ?"
        goto editpg1

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
         "Enter a History Lookup Selection ( 1 thru 7)?                ",~
         "Enter the Applicable S.O. History Look-Up Information?       "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, scr_sel$, scr_sel_d$,      ~
                      ss$(), sl$(), se$(), se_d$(), cc$(), c1$(), dt$(), ~
                      hs_job$, hs_cust$, hs_po$, hs_ln$, hs_yr$, hs_po$, ~
                      hs_load$, hs_so$, hs_hdrt$, hs_linet$, hs_order$,  ~
                      hs_ship$, hs_quote$, hs_part$, hs_cat$, hs_prc$,   ~
                      hs_rgn$, hs_sls$, hs_inv$, hs_chk$, hs_due$,       ~
                      hs_filler$, readkey$, desc$, warr_rec$, text_rec$(),~
                      textid$, txtid$, text_key$, text$, sav_txt$,       ~
                      hh$(), dth$(), warranty$, warranty_ln$, warranty_so$,~
                      hs_sub_part$, hs_info_part$
                                                         /* (PAR000)   */
                                                         /* (EWD001)   */
            f1% = 0%                                     /* (PAR001)   */


            ss$( 1%)= "****************************************"
            ss$( 2%)= "* Sale Order History Lookup Selections *"
            ss$( 3%)= "* ------------------------------------ *"
            ss$( 4%)= "* (1) Customer / Purchase Order        *"
            ss$( 5%)= "* (2) Warranty Number                  *"
            ss$( 6%)= "* (3) Sales Order                      *"
            ss$( 7%)= "* (4) Load / Sales Order               *"
            ss$( 8%)= "* (5) Customer / Order Date            *"
            ss$( 9%)= "* (6) Job Name / Customer              *"
            ss$(10%)= "* (7) Invoice Number                   *"
            ss$(11%)= "****************************************"

            mat se% = zer

            hh$(1%) = "Customer: xxxxxxxxx Sales Order: xxxxxxxx"
            hh$(2%) = "P.O. No.: xxxxxxxxxxxxxxxx "
            hh$(3%) = "Job Name: xxxxxxxxxxxxxxxx "
            hh$(4%) = "Quote No: xxxxxxxxxx       "
            hh$(5%) = "Salesman: xxxx Region: xxxx"

                                                   /* Length (22) Right */
            hh$(6%) = "Order Date: xxxxxxxxxx"     /* Start Pos (13)    */
            hh$(7%) = "Ship Date : xxxxxxxxxx"
            hh$(8%) = "Due Date  : xxxxxxxxxx"
            hh$(9%) = "Invoice No: xxxxxxxx  "
            hh$(10%)= "Check No. : xxxxxxxx  "
                                                    /* dt$() Positions */
            dth$(1%) = "Ln "                        /* (1 for 3  )     */
            dth$(2%) = "<----- Part Number ----->"  /* (6 for 25 )     */
            dth$(3%) = "Comment "                   /* (33 for 8 )     */
            dth$(5%) = "Ord Qty"                    /* (43 for 7 )     */
            dth$(6%) = "Shp Qty"                    /* (52 for 7 )     */
            dth$(7%) = "Ext. Price"                 /* (61 for 10)     */
            dth$(8%) = "Text"                       /* (72 for 4 )     */

            warr_1$  = "Warranty Id"                /* (EWD001)        */
            warr_2$  = "Lin"                        /*                 */
            warr_3$  = "Year"                       /* (EWD001)        */
        return

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

        REM dataload
        REM return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT

        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                       /* (EWDHIST) New Master History */
        REM FMT      CH(16),             /* Customer Job Name          */~
        REM          CH(9),              /* Customer Code              */~
        REM          CH(16),             /* Customer P.O. Number       */~
        REM          CH(3),              /* Sales Order Line Item      */~
        REM          CH(4),              /* History Data Year (YYYY)   */~
        REM          CH(5),              /* EWD Load Number            */~
        REM          CH(8),              /* Customer Sales Order       */~
        REM          CH(3),              /* hs_ln$                     */~
        REM          CH(4),              /* hs_yr$                     */~
        REM          CH(4),              /* Header Text Id             */~
        REM          CH(4),              /* Line Item Text Id          */~
        REM          CH(9),              /* hs_cust$                   */~
        REM          PD(11,1),           /* S.O. Order Date            */~
        REM          CH(8),              /* hs_so$                     */~
        REM          CH(3),              /* hs_ln$                     */~
        REM          PD(11,1),           /* Sales Order Ship Date      */~
        REM          CH(10),             /* EWD Quote Number           */~
        REM          CH(25),             /* MFG Part Number            */~
        REM          BI(2),              /* hs_oqty% - Order Ln Qty    */~
        REM          BI(2),              /* hs_sqty% - Shipped Ln Qty  */~
        REM          PD(15,4),           /* Header Discount Pcnt       */~
        REM          PD(14,4),           /* Line Item Discount Pcnt    */~
        REM          PD(14,4),           /* hs_uprice - Ln Unit Price  */~
        REM          PD(14,4),           /* hs_eprice - Ln Extnd Price */~
        REM          CH(4),              /* Ln Item Category Code      */~
        REM          CH(1),              /* Ln Item Price Code         */~
        REM          CH(4),              /* Region Code                */~
        REM          CH(4),              /* Salesman Code              */~
        REM          CH(8),              /* EWD Invoice Number         */~
        REM          CH(8),              /* Customer Check No.         */~
        REM          PD(11,1),           /* S.O. Due Date              */~
        REM          CH(20),             /* New Sub Part Number        */~
        REM          CH(09),             /* Information Part Number    */~
        REM          CH(13)              /* Filler Area                */
                                         /* (EWDHIST)                  */
                                         /* (PAR000)                   */

                                         /* (EWDWARR) - New File       */
        REM FMT      CH(8),              /* Warranty Id                */~
        REM          CH(3),              /* Line Item                  */~
        REM          CH(4),              /* Warranty Year              */~
        REM          CH(8),              /* Customer Sales Order       */~
        REM          CH(3),              /* Sales Order Line Item      */~
        REM          CH(4),              /* Sales Order Year           */~
        REM          CH(2)               /* Filler                     */

        REM FMT CH(1024)                 /* (EWDTEXTH) New History     */
        REM                              /*    Text Master. Same Format*/
        REM                              /*    asCaelus (TXTFILE)      */



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
              on fieldnr% gosub L40160,          /* Lookup Selection   */~
                                L40160           /* Sel (1)            */
              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Sales Order Lookup Code:",                   ~
               at (03,27), fac(lfac$(1%)), scr_sel$             , ch(01),~
               at (03,45), fac(hex(84)),   scr_sel_d$           , ch(30),~
                                                                         ~
               at (04,02), fac(hex(84)),   sl$(1%)              , CH(24),~
               at (04,27), fac(lfac$(2%)), se$(1%)              , ch(16),~
               at (04,45), fac(hex(84)),   se_d$(1%)            , ch(30),~
                                                                         ~
               at (05,02), fac(hex(84)),   sl$(2%)              , ch(24),~
               at (05,27), fac(lfac$(2%)), se$(2%)              , ch(16),~
               at (05,45), fac(hex(84)),   se_d$(2%)            , ch(30),~
                                                                         ~
               at (09,21), fac(hex(84)),   ss$(1%)              , ch(40),~
               at (10,21), fac(hex(84)),   ss$(2%)              , ch(40),~
               at (11,21), fac(hex(84)),   ss$(3%)              , ch(40),~
               at (12,21), fac(hex(84)),   ss$(4%)              , ch(40),~
               at (13,21), fac(hex(84)),   ss$(5%)              , ch(40),~
               at (14,21), fac(hex(84)),   ss$(6%)              , ch(40),~
               at (15,21), fac(hex(84)),   ss$(7%)              , ch(40),~
               at (16,21), fac(hex(84)),   ss$(8%)              , ch(40),~
               at (17,21), fac(hex(84)),   ss$(9%)              , ch(40),~
               at (18,21), fac(hex(84)),   ss$(10%)             , ch(40),~
               at (19,21), fac(hex(84)),   ss$(11%)             , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40420
                  call "PRNTSCRN"
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1) = "(1)Start Over     (4)Previous Field     " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(2%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1%),19%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (10)Select Data        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffff0affffff0e0f1000)
            return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return


        REM *************************************************************~
            *      D I S P L A Y   S A L E S   H E A D E R   D A T A    *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_history_orders
            k% = 0%
L41000:     gosub set_pf2
            accept                                                       ~
               at (01,02), fac(hex(84)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(84)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(84)), title$                 , ch(20),~
               at (02,64), fac(hex(84)), pageno$                , ch(14),~
                                                                         ~
               at (03,02), "Sales Order Lookup Code:",                   ~
               at (03,27), fac(hex(84)),   scr_sel$             , ch(01),~
               at (03,45), fac(hex(84)),   scr_sel_d$           , ch(30),~
                                                                         ~
               at (04,02), fac(hex(84)),   sl$(1%)              , CH(24),~
               at (04,27), fac(hex(84)),   se$(1%)              , ch(16),~
               at (04,45), fac(hex(84)),   se_d$(1%)            , ch(30),~
                                                                         ~
               at (05,02), fac(hex(84)),   sl$(2%)              , ch(24),~
               at (05,27), fac(hex(84)),   se$(2%)              , ch(16),~
               at (05,45), fac(hex(84)),   se_d$(2%)            , ch(30),~
                                                                         ~
               at (06,04), fac(hex(a4))  , h1$                  , ch(12),~
               at (06,16), fac(hex(a4))  , h2$                  , ch(16),~
               at (06,33), fac(hex(a4))  , h3$                  , ch(12),~
               at (06,43), fac(hex(a4))  , h4$                  , ch(10),~
               at (06,55), fac(hex(a4))  , h5$                  , ch(08),~
               at (06,65), fac(hex(a4))  , h6$                  , ch(08),~
               at (06,75), fac(hex(a4))  , h7$                  , ch(05),~
                                                                         ~
               at (07,02), fac(hex(81))  , c1$(k% + 1%)         , ch(01),~
               at (07,04), fac(hex(84))  , cc$(k% + 1%)         , ch(76),~
                                                                         ~
               at (08,02), fac(hex(81))  , c1$(k% + 2%)         , ch(01),~
               at (08,04), fac(hex(84))  , cc$(k% + 2%)         , ch(76),~
                                                                         ~
               at (09,02), fac(hex(81))  , c1$(k% + 3%)         , ch(01),~
               at (09,04), fac(hex(84))  , cc$(k% + 3%)         , ch(76),~
                                                                         ~
               at (10,02), fac(hex(81))  , c1$(k% + 4%)         , ch(01),~
               at (10,04), fac(hex(84))  , cc$(k% + 4%)         , ch(76),~
                                                                         ~
               at (11,02), fac(hex(81))  , c1$(k% + 5%)         , ch(01),~
               at (11,04), fac(hex(84))  , cc$(k% + 5%)         , ch(76),~
                                                                         ~
               at (12,02), fac(hex(81))  , c1$(k% + 6%)         , ch(01),~
               at (12,04), fac(hex(84))  , cc$(k% + 6%)         , ch(76),~
                                                                         ~
               at (13,02), fac(hex(81))  , c1$(k% + 7%)         , ch(01),~
               at (13,04), fac(hex(84))  , cc$(k% + 7%)         , ch(76),~
                                                                         ~
               at (14,02), fac(hex(81))  , c1$(k% + 8%)         , ch(01),~
               at (14,04), fac(hex(84))  , cc$(k% + 8%)         , ch(76),~
                                                                         ~
               at (15,02), fac(hex(81))  , c1$(k% + 9%)         , ch(01),~
               at (15,04), fac(hex(84))  , cc$(k% + 9%)         , ch(76),~
                                                                         ~
               at (16,02), fac(hex(81))  , c1$(k% + 10%)        , ch(01),~
               at (16,04), fac(hex(84))  , cc$(k% + 10%)        , ch(76),~
                                                                         ~
               at (17,02), fac(hex(81))  , c1$(k% + 11%)        , ch(01),~
               at (17,04), fac(hex(84))  , cc$(k% + 11%)        , ch(76),~
                                                                         ~
               at (18,02), fac(hex(81))  , c1$(k% + 12%)        , ch(01),~
               at (18,04), fac(hex(84))  , cc$(k% + 12%)        , ch(76),~
                                                                         ~
               at (19,02), fac(hex(81))  , c1$(k% + 13%)        , ch(01),~
               at (19,04), fac(hex(84))  , cc$(k% + 13%)        , ch(76),~
                                                                         ~
               at (20,02), fac(hex(81))  , c1$(k% + 14%)        , ch(01),~
               at (20,04), fac(hex(84))  , cc$(k% + 14%)        , ch(76),~
                                                                         ~
               at (21,02), fac(hex(a4)),   dsp_msg$             , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L41040             /* First    */
L41020:           k% = 0%
                  goto L41000

L41040:        if keyhit% <> 3% then goto L41080             /* Last      */
L41060:           x% = int(val_max% / 14%)
                  k% = (x%*14%)
                  if (k% + 1%) > val_max% then k% = k% - 14%
                  goto L41000

L41080:        if keyhit% <> 4% then goto L41100             /* Previous */
                  if k% < 15% then goto L41020
                  k% = k% - 14%
                  if k% <= 1% then goto L41020
                  goto L41000

L41100:        if keyhit% <> 5% then goto L41150             /* Next     */
                  k% = k% + 14%
                  if k% < val_max% then goto L41000
                  goto L41060

L41150:        if keyhit% <> 15 then goto L41155
                  call "PRNTSCRN"
                  goto L41000

L41155:        if keyhit% <> 0% then goto L41200
                  for i% = 1% to hit%
                      if c1$(i%) <> " " then goto L41160
                  next i%
                  goto L41000

L41160:           gosub Load_detail
                  gosub display_history_detail
                  goto L41000

L41200:        if keyhit% <> 16 then goto L41000

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return clear all
        goto inputmode

        set_pf2
            init(" ") h1$, h2$, h3$, h4$, h5$, h6$, h7$, c1$(), dt$()
            dsp_msg$=                                                     ~
             "Use 'X' to Display Detail Information, followed by <Return>?"


            title$ = " Matches Found [xxx]"
            convert hit% to str(title$,17%,3%), pic(###)
            if hit% = 500% then str(title$,1%,1%) = "*"

            pageno$ = "Page: XX of XX"             /* k% = Array Values */
            h1$ = "Customer "
            h2$ = "<Purchase Order>"
            h3$ = "<-S.O.->"
            h4$ = "Order Date"
            h5$ = "Invoice "
            h6$ = "Check No"
            h7$ = "Load"

            val_max% = hit%
            if val_max% > (504% - 14%) then val_max% = 504% - 14%
                                                        /* Display Max */
            x = val_max%
            yy% = ( val_max% / 14% )
            if mod(x,14) <> 0 then yy% = yy% + 1%

            xx% = (k% / 14%) +1%
            if xx% > yy% then xx% = yy%

            convert xx% to str(pageno$,7%,2%), pic(##) /* Current Page*/

            convert yy% to str(pageno$,13%,2%), pic(##)/* Total Pages */

            pf$(1) = "(2)First           (5)Next              " &        ~
                     "                                       "
            pf$(2) = "(3)Last                                 " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(4)Previous                             " &        ~
                     "                       (16)Exit Display"
            pfkeys$ = hex(ff02030405ffffffffffffffffff0f1000)
            gosub check_screen
            return

        check_screen
            if val_max% > 14% then goto L41860
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L41860:      if k% >= 14% then goto L41870
                gosub no_first
                gosub no_prev
L41870:      if (k% + 14%) <= val_max% then goto L41880
                gosub no_last
L41880:      if k% <= (val_max% - 14%) then goto L41900
                gosub no_next
L41900: return
        no_first
            str(pf$(1%),1%, 9%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
            str(pf$(1%),20%, 9%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last
            str(pf$(2%),1%,9%)   = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
            str(pf$(3%),1%,12%)  = " " : str(pfkeys$,4%,1%) = hex(ff)
        return


         REM *************************************************************~
            *      D I S P L A Y   S A L E S   D E T A I L   D A T A    *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_history_detail
            k% = 0%
L42000:     gosub set_pf3
            accept                                                       ~
               at (01,02), fac(hex(84)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(84)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(84)), title$                 , ch(20),~
               at (02,64), fac(hex(84)), pageno$                , ch(14),~
                                                                         ~
               at (03,02), fac(hex(84)),   hh$(1%)              , ch(41),~
               at (04,02), fac(hex(84)),   hh$(2%)              , ch(38),~
               at (05,02), fac(hex(84)),   hh$(3%)              , ch(38),~
               at (06,02), fac(hex(84)),   hh$(4%)              , CH(38),~
               at (07,02), fac(hex(84)),   hh$(5%)              , ch(38),~
                                                                         ~
               at (03,58), fac(hex(84)),   hh$(6%)              , ch(22),~
               at (04,58), fac(hex(84)),   hh$(7%)              , ch(22),~
               at (05,58), fac(hex(84)),   hh$(8%)              , ch(22),~
               at (06,58), fac(hex(84)),   hh$(9%)              , CH(22),~
               at (07,58), fac(hex(84)),   hh$(10%)             , ch(22),~
                                                                         ~
               at (08,02), fac(hex(a4))  , dth$(1%)             , ch(03),~
               at (08,07), fac(hex(a4))  , dth$(2%)             , ch(25),~
               at (08,34), fac(hex(a4))  , dth$(3%)             , ch(08),~
               at (08,44), fac(hex(a4))  , dth$(5%)             , ch(07),~
               at (08,53), fac(hex(a4))  , dth$(6%)             , ch(07),~
               at (08,62), fac(hex(a4))  , dth$(7%)             , ch(10),~
               at (08,73), fac(hex(a4))  , dth$(8%)             , ch(04),~
                                                                         ~
               at (09,02), fac(hex(84))  , dt$(k% + 1%)         , ch(79),~
                                                                         ~
               at (10,02), fac(hex(84))  , dt$(k% + 2%)         , ch(79),~
                                                                         ~
               at (11,02), fac(hex(84))  , dt$(k% + 3%)         , ch(79),~
                                                                         ~
               at (12,02), fac(hex(84))  , dt$(k% + 4%)         , ch(79),~
                                                                         ~
               at (13,02), fac(hex(84))  , dt$(k% + 5%)         , ch(79),~
                                                                         ~
               at (14,02), fac(hex(84))  , dt$(k% + 6%)         , ch(79),~
                                                                         ~
               at (15,02), fac(hex(84))  , dt$(k% + 7%)         , ch(79),~
                                                                         ~
               at (16,02), fac(hex(84))  , dt$(k% + 8%)         , ch(79),~
                                                                         ~
               at (17,02), fac(hex(84))  , dt$(k% + 9%)         , ch(79),~
                                                                         ~
               at (18,02), fac(hex(84))  , dt$(k% + 10%)        , ch(79),~
                                                                         ~
               at (19,02), fac(hex(84))  , dt$(k% + 11%)        , ch(79),~
                                                                         ~
               at (20,02), fac(hex(84))  , dt$(k% + 12%)        , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   dsp_msg$             , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L42040             /* First    */
L42020:           k% = 0%
                  goto L42000

L42040:        if keyhit% <> 3% then goto L42080             /* Last      */
L42060:           x% = int(val_max% / 12%)
                  k% = (x%*12%)
                  if (k% + 1%) > val_max% then k% = k% - 12%
                  goto L42000

L42080:        if keyhit% <> 4% then goto L42100             /* Previous */
                  if k% < 13% then goto L42020
                  k% = k% - 12%
                  if k% <= 1% then goto L42020
                  goto L42000

L42100:        if keyhit% <> 5% then goto L42150             /* Next     */
                  k% = k% + 12%
                  if k% < val_max% then goto L42000
                  goto L42060

L42150:        if keyhit% <> 15 then goto L42155
                  call "PRNTSCRN"
                  goto L42000

L42155:        if keyhit% <> 8% then goto L42160
                  txtid$ = header$
                  convert headerch$ to ff%, data goto L42156 /*SR73678 */
L42156
                  gosub lookup_text
                  goto L42000
                                                   /* (PAR000)         */
L42160:        if keyhit% <> 9% then goto L42162
                  goto L42180
                                                   /* (PAR000)         */
L42162:        if keyhit% <> 10% then goto L42165
                  goto L42180
                                                   /* (PAR000)         */

L42165:        if keyhit% <> 16% then goto L42170
                  goto L42180

L42170:        if keyhit% = 0% then goto L42180
                  goto L42000

L42180:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            if keyhit% = 16% then return           /* Finished         */
               fieldnr% = cursor%(1%) - 8%         /* Check for Line   */
               if fieldnr% < 1% then goto L42000   /* Item Text        */
                  line% = k% + fieldnr%
                  if keyhit% = 9% then goto L42190
                                                   /* (PAR000)         */
                  if keyhit% = 10% then goto L42200
                                                   /* (PAR000)         */
                     txtid$ = str(dt$(line%),80%,4%)
                     convert str(dt$(line%),115%,2%) to ff%,  ~
                                           data goto L42181 /*SR73678 */
L42181
                     gosub lookup_text
                     goto L42000
L42190:        gosub load_warranty_ids
               goto L42000
                                                  /* (PAR000)           */
L42200:        gosub load_part_explosion
               goto L42000
                                                  /* (PAR000)           */
        return

        set_pf3
            line% = 0%
            init(" ") dsp_msg$
                                                   /* (PAR00) - Correction */
            dsp_msg$=                                                     ~
     "Pos Cursor to Display Ln Info? <Return>=Text, <PF9>=Warranty No, <P10>=Part No"

        REM "Position cursor to line with Text 'Yes' and Press <Return> to Display?"

            pageno$ = "Page: XX of XX"             /* k% = Array Values */
            val_max% = dtl%
            if val_max% > (100% - 12%) then val_max% = 100% - 12%
                                                        /* Display Max */
            x = val_max%
            yy% = ( val_max% / 12% )
            if mod(x,12) <> 0 then yy% = yy% + 1%

            xx% = (k% / 12%) +1%
            if xx% > yy% then xx% = yy%

            convert xx% to str(pageno$,7%,2%), pic(##) /* Current Page*/

            convert yy% to str(pageno$,13%,2%), pic(##)/* Total Pages */
                                                       /* (PAR000)      */
            pf$(1) = "(2)First           (5)Next              " &        ~
                     "(10)New Part Explosion                 "
            pf$(2) = "(3)Last            (8)Header Text       " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(4)Previous        (9)Warranty No's     " &        ~
                     "                       (16)Exit Display"
            pfkeys$ = hex(ff02030405ffff08090affffffff0f1000)
                                                       /* (PAR000)      */
            if header% = 0% then str(pf$(2%),20%,15%) = " "
            if header% = 0% then str(pfkeys$,8%,1%) = hex(ff)

            gosub check_screen_dt
            return

        check_screen_dt
            if val_max% > 12% then goto L42860
               gosub no_first_dt
               gosub no_next_dt
               gosub no_last_dt
               gosub no_prev_dt
               return
L42860:      if k% >= 12% then goto L42870
                gosub no_first_dt
                gosub no_prev_dt
L42870:      if (k% + 12%) <= val_max% then goto L42880
                gosub no_last_dt
L42880:      if k% <= (val_max% - 12%) then goto L42900
                gosub no_next_dt
L42900: return
        no_first_dt
            str(pf$(1%),1%, 9%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next_dt
            str(pf$(1%),20%, 9%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last_dt
            str(pf$(2%),1%,9%)   = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev_dt
            str(pf$(3%),1%,12%)  = " " : str(pfkeys$,4%,1%) = hex(ff)
        return

        REM *************************************************************~
            *          D I S P L A Y   W A R R A N T Y   I D ' S        *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_warranty
            jj% = 0%
L43000:     gosub set_pf4
            accept                                                       ~
               at (01,02), fac(hex(84)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(84)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(84)), title$                 , ch(20),~
               at (02,64), fac(hex(84)), pageno$                , ch(14),~
                                                                         ~
               at (03,02), fac(hex(84)),   hh$(1%)              , ch(41),~
               at (04,02), fac(hex(84)),   hh$(2%)              , ch(38),~
               at (05,02), fac(hex(84)),   hh$(3%)              , ch(38),~
               at (06,02), fac(hex(84)),   hh$(4%)              , CH(38),~
               at (07,02), fac(hex(84)),   hh$(5%)              , ch(38),~
                                                                         ~
               at (03,58), fac(hex(84)),   hh$(6%)              , ch(22),~
               at (04,58), fac(hex(84)),   hh$(7%)              , ch(22),~
               at (05,58), fac(hex(84)),   hh$(8%)              , ch(22),~
               at (06,58), fac(hex(84)),   hh$(9%)              , CH(22),~
               at (07,58), fac(hex(84)),   hh$(10%)             , ch(22),~
                                                                         ~
               at (08,02), fac(hex(a4))  , warr_1$              , ch(11),~
               at (08,16), fac(hex(a4))  , warr_2$              , ch(03),~
               at (08,21), fac(hex(a4))  , warr_3$              , ch(04),~
                                                                         ~
               at (09,02), fac(hex(84))  , warr$(jj% + 1%)      , ch(50),~
                                                                         ~
               at (10,02), fac(hex(84))  , warr$(jj% + 2%)      , ch(50),~
                                                                         ~
               at (11,02), fac(hex(84))  , warr$(jj% + 3%)      , ch(50),~
                                                                         ~
               at (12,02), fac(hex(84))  , warr$(jj% + 4%)      , ch(50),~
                                                                         ~
               at (13,02), fac(hex(84))  , warr$(jj% + 5%)      , ch(50),~
                                                                         ~
               at (14,02), fac(hex(84))  , warr$(jj% + 6%)      , ch(50),~
                                                                         ~
               at (15,02), fac(hex(84))  , warr$(jj% + 7%)      , ch(50),~
                                                                         ~
               at (16,02), fac(hex(84))  , warr$(jj% + 8%)      , ch(50),~
                                                                         ~
               at (17,02), fac(hex(84))  , warr$(jj% + 9%)      , ch(50),~
                                                                         ~
               at (18,02), fac(hex(84))  , warr$(jj% + 10%)     , ch(50),~
                                                                         ~
               at (19,02), fac(hex(84))  , warr$(jj% + 11%)     , ch(50),~
                                                                         ~
               at (20,02), fac(hex(84))  , warr$(jj% + 12%)     , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   dsp_msg$             , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L43040             /* First    */
L43020:           jj% = 0%
                  goto L43000

L43040:        if keyhit% <> 3% then goto L43080             /* Last      */
L43060:           x% = int(val_max% / 12%)
                  jj% = (x%*12%)
                  if (jj% + 1%) > val_max% then jj% = jj% - 12%
                  goto L43000

L43080:        if keyhit% <> 4% then goto L43100             /* Previous */
                  if jj% < 13% then goto L43020
                  jj% = jj% - 12%
                  if jj% <= 1% then goto L43020
                  goto L43000

L43100:        if keyhit% <> 5% then goto L43150             /* Next     */
                  jj% = jj% + 12%
                  if jj% < val_max% then goto L43000
                  goto L43060

L43150:        if keyhit% <> 15 then goto L43160
                  call "PRNTSCRN"
                  goto L43000

L43160:        if keyhit% <> 16% then goto L43170
                  goto L43180

L43170:        if keyhit% = 0% then goto L43180
                  goto L43000

L43180:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf4
            line% = 0%
            init(" ") dsp_msg$
            dsp_msg$ = "Press <Return> to Continue?"

            pageno$ = "Page: XX of XX"             /* k% = Array Values */
            val_max% = warr%
            if val_max% > (100% - 12%) then val_max% = 100% - 12%
                                                        /* Display Max */
            x = val_max%
            yy% = ( val_max% / 12% )
            if mod(x,12) <> 0 then yy% = yy% + 1%

            xx% = (k% / 12%) +1%
            if xx% > yy% then xx% = yy%

            convert xx% to str(pageno$,7%,2%), pic(##) /* Current Page*/

            convert yy% to str(pageno$,13%,2%), pic(##)/* Total Pages */

            pf$(1) = "(2)First           (5)Next              " &        ~
                     "                                       "
            pf$(2) = "(3)Last                                 " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(4)Previous                             " &        ~
                     "                       (16)Exit Display"
            pfkeys$ = hex(ff02030405ffff08ffffffffffff0f1000)
            sav_k% = k%
            k% = jj%
            gosub check_screen_dt
            jj% = k%
            k% = sav_k%
            return

       REM *************************************************************~
            *                     E D I T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            init(" ") errormsg$
            on fieldnr% gosub L50000,                 /* scr_sel$     */  ~
                              L50100                  /* se$(1%)      */
                                                      /* se$(2%)      */
            return

L50000: Rem Special Selection Code                    scr_sel$
            sel% = 0%
            init(" ") scr_sel_d$, sl$(), se$()
            if scr_sel$ <> " " then goto L50010
               scr_sel$ = "1"

L50010:     convert scr_sel$ to sel%, data goto L50020

L50020:     if sel% < 1% or sel% > 7% then goto L50090
               scr_sel_d$ = str(ss$(sel% + 3%),7%,30%)

            if sel% <> 1% then goto L50030
               sl$(1%) = "Customer Code          :"
               sl$(2%) = "P.O. Number            :"
               goto L50080

L50030:     if sel% <> 2% then goto L50040
               sl$(1%) = "Warranty Number        :"
               sl$(2%) = "                       :"
               goto L50080

L50040:     if sel% <> 3% then goto L50050
               sl$(1%) = "Sales Order Number     :"
               sl$(2%) = "                       :"
               goto L50080

L50050:     if sel% <> 4% then goto L50060
               sl$(1%) = "Load Number            :"
               sl$(2%) = "Sales Order Number     :"
               goto L50080

L50060:     if sel% <> 5% then goto L50070
               sl$(1%) = "Customer Code          :"
               sl$(2%) = "Order Date  Beg/End    :"
               goto L50080

L50070:     if sel% <> 6% then goto L50075
               sl$(1%) = "Job Name               :"
               sl$(2%) = "Customer Code          :"
               goto L50080

L50075:     if sel% <> 7% then goto L50090
               sl$(1%) = "Invoice Number         :"
               sl$(2%) = "                       :"
L50080:
        return
L50090:     init(" ") scr_sel$, scr_sel_d$
            errormsg$ = "(Error) Invalid History Lookup Selection?"
            gosub error_prompt
        return

L50100: Rem Edit Lookup Data
            init(" ") se_d$(), cust_key$, hs_key$
            hs_key$ = all(hex(00))

            se%(1%) = len(se$(1%))                   /* Store Both   */
            se%(2%) = len(se$(2%))                   /* Lengths      */
            on sel% goto L50200,L50300, L50400, L50500, L50600, L50700, ~
                         L50800

L50200: Rem Cutomer/P.O.                    se$(1%), se$(2%), se$(3%)
               if se%(1%) <> 6% then goto L50210     /* Partial      */
                  cust_key$ = se$(1%)
                  gosub lookup_customer              /* Exact        */
                  se_d$(1%) = cust_name$
                  if len(se_d$(1%)) < 10 then goto L51000 /* (Error) */
                  goto L50220
L50210:        if se%(1%) < 2% then goto L51000      /* (2) Minimum  */
                  se_d$(1%) = "Check Customers (Partial)     "

L50220:        if str(se$(2%),1%,1%) = " " then se%(2%) = 0%
                  se_d$(2%) = "Check Purchase Orders(Partial)"

          kk% = 0%                                /* Primary Key     */
          str(hs_key$,1%,9%)   = str(se$(1%),1%,se%(1%))
          if se%(1%) = 6% and se%(2%) <> 0% then  /* Set P.O. Data   */~
                       str(hs_key$,10%,16%) = str(se$(2%),1%,se%(2%))
        return                                    /* (EWDHIST)       */

L50300: Rem Warranty No.                    se$(1%), se$(2%)
               if se%(1%) < 6% then goto L51060   /* (6) Minimum     */
               if se%(1%) = 8% then                                    ~
                                   se_d$(1%) = "Check Warranty Id's"   ~
                    else se_d$(1%) = "Check Warranty Id's (Partial) "

        kk%       = 0%                            /* Primary Key     */
        se$(2%)   = " "
        se_d$(2%) = "Not Applicable"
        str(hs_key$,1%,8%) = str(se$(1%),1%,se%(1%))
        return                                    /* (EWDWARR)       */

L50400: Rem Sales Order                     se$(1%), se$(2%)
               if se%(1%) < 6% then goto L51010   /* (6) Minimum     */
                  if se%(1%) = 8% then                                 ~
                                      se_d$(1%) = "Check Sales Orders" ~
                     else se_d$(1%) = "Check Sales Orders (Partial)  "

        kk%       = 1%                            /* Alt Key (1)     */
        se$(2%)   = " "
        se_d$(2%) = "Not Applicable"
        str(hs_key$,1%,8%) = str(se$(1%),1%,se%(1%))
        return

L50500: Rem Load No./Sales Order            se$(1%), se$(2%)
               if se%(1%) <> 5% then goto L51030   /* (5) Minimum    */
                  se_d$(1%) = "Check Loads"

               if str(se$(2%),1%,1%) = " "then se%(2%) = 0%
               if se%(2%) = 8% then                                    ~
                                      se_d$(2%) = "Check Sales Orders" ~
                     else se_d$(2%) = "Check Sales Orders (Partial)  "

        kk% = 2%                                  /* Alt Key (2)     */
        str(hs_key$,1%,5%) = str(se$(1%),1%,5%)
        if se%(1%) = 5% and se%(2%) <> 0% then                         ~
                           str(hs_key$,6%,8%) = str(se$(2%),1%,se%(2%))
        return

L50600: Rem Cutomer/Order Date              se$(1%), se$(2%)
               if se%(1%) <> 6% then goto L51070     /* Must be Exact*/
                  cust_key$ = se$(1%)
                  gosub lookup_customer              /* Exact        */
                  se_d$(1%) = cust_name$
                  if len(se_d$(1%)) < 10 then goto L51000 /* (Error) */

               init(" ") tst_date$, tst_date1$, tst_dte$, tst_dte1$
               p% = pos(se$(2%) = "/")
               if p% = 0% then p% = 10%
               tst_date$ = str(se$(2%),1%,p% - 1%)
               if p% <> 10% then tst_date1$ = str(se$(2%),p% + 1%)     ~
                            else tst_date1$  = tst_date$

               date% = 0%
               call "DATEOKC" (tst_date$, date%, errormsg$)
               if date% = 0% then goto L51020          /* (Error)    */
               tst_dte$ = tst_date$                    /* Begin Date */
               call "DATUFMTC" (tst_dte$)
               str(se$(2%),1%,16%) = tst_date$ & "        "

               date% = 0%
               init(" ") errormsg$
               call "DATEOKC" (tst_date1$, date%, errormsg$)
               if date% = 0% then goto L51025          /* (Error)    */
               tst_dte1$ = tst_date1$                  /* End Date   */
               call "DATUFMTC" (tst_dte1$)

               se_d$(2%) = "End Date: " &tst_date1$ & "          "


        kk% = 3%                                  /* Alt Key (3)     */
        str(hs_key$,1%,9%)  = str(se$(1%),1%,se%(1%))
        str(hs_key$,10%,6%) = str(tst_dte$,1%,6%)

        return

L50700: Rem Job Name/Customer               se$(1%), se$(2%)
               if se%(1%) < 5% then goto L51040   /* (5) Minimum     */
                  se_d$(1%) = "Check Job Name (Partial)      "

               if se%(2%) <> 6% then goto L50710     /* Partial      */
                  cust_key$ = se$(2%)
                  gosub lookup_customer              /* Exact        */
                  se_d$(2%) = cust_name$
                  if len(se_d$(2%)) < 10 then goto L51000 /* (Error) */
                  goto L50720

L50710:        if str(se$(2%),1%,1%) = " " then se%(2%) = 0%
                  se_d$(2%) = "Check Customers (Partial)     "

L50720: kk% = 4%                                  /* Alt Key (4)     */
        str(hs_key$,1%,16%) = str(se$(1%),1%,se%(1%))
        if se%(1%) = 16% and se%(2%) <> 0% then                        ~
                         str(hs_key$,17%,9%) = str(se$(2%),1%,se%(2%))
        return

L50800: Rem Invoice Number                       se$(1%), se$(2%)
               if se%(1%) < 6% then goto L51080   /* (6) Minimum     */
                  if se%(1%) = 8% then                                 ~
                                 se_d$(1%) = "Check Customer Invoices" ~
                  else se_d$(1%) = "Check Customer Invoices (Partial)  "

        kk% = 5%                                  /* Alt Key (5)     */
        str(hs_key$,1%,8%) = str(se$(1%),1%,se%(1%))

        return

L51000:     errormsg$ = "(Error) Cust. Code (2) for History Look-Up?"
            goto L52000
L51010:     errormsg$ = "(Error) S. O. Code (6) for History Look-Up?"
            goto L52000
L51020:     errormsg$ = "(Error) Begin S.O. Date for History Look-Up?"
            goto L52000
L51025:     errormsg$ = "(Error) End S.O. Date for History Look-Up?"
            goto L52000
L51030:     errormsg$ = "(Error) Load Number (5) for History Look-Up?"
            goto L52000
L51040:     errormsg$ = "(Error) Job Number Min (5) for History Look-Up?"
            goto L52000
L51060:     errormsg$ = "(Error) Warranty No. (6) for History Look-Up?"
            goto L52000
L51070:     errormsg$ = "(Error) Cust. Code (6) for History Look-Up?"
            goto L52000
L51080:     errormsg$ = "(Error) Invoice No.Min (6) for History Look-Up?"

L52000:     init(" ") se$(), se_d$()
            gosub error_prompt
        return

        lookup_customer                            /* Pass Key In    */
            init(" ") cust_name$, cust_city$, cust_st$, cust_zip$
            read #1,key = cust_key$, using L52010, cust_name$,         ~
                                  cust_city$, cust_st$, cust_zip$,     ~
                                  eod goto lookup_cust_done
L52010:        FMT POS(253), CH(30), POS(403), CH(18), CH(2), XX(1),   ~
                      CH(9)
        lookup_cust_done

        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        deffn'099(textid$)
            txt% = 0%
            if textid$ = hex(00000000) or textid$ = hex(ffffffff)        ~
                                           or textid$ = " " then return
            txt% = 1%
        return

        lookup_text                           /* Look Up Text Id       */
            init(" ") text_key$, sav_txt$
            ta% = 0%
            gosub'099(txtid$)
            if txt% = 0% then return

            text_key$ = all(hex(00))
            str(text_key$,1%,1%) = "M"
            str(text_key$,2%,3%) = "   "
            str(text_key$,5%,4%) = txtid$
            str(text_key$,9%,1%) = "1"
            sav_txt$ = str(text_key$,1%,9%)
/*SR73678 Read from #ff% instead of #3   */

            read #25,key > text_key$, using L60000, text_key$,         ~
                                             eod goto lookup_again
L60000:        FMT CH(11)
            if sav_txt$ <> str(text_key$,1%,9%) then                   ~
                                                 goto lookup_again

               get #25, using L60010, text$, text_rec$()
L60010:           FMT CH(64), 3*CH(70)

           init(" ") hdr$, msg$()
           rhh$ =" ----------------------------------------------------"
           comp% = 2%
           hdr$     = "*******  H e a d e r   T e x t   *******"
           if line% <> 0% then                                          ~
              hdr$  = "****  L i n e   I t e m   T e x t   ****"

           msg$(1%) = "Ln (1) " & text_rec$(1%) & rhh$
           msg$(2%) = "Ln (2) " & text_rec$(2%) & rhh$
           msg$(3%) = "Ln (3) " & text_rec$(3%) & rhh$
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

/*SR73678 + */
        lookup_again
           ta% = ta% + 1%
           if ta% > 1% then goto lookup_text_done
REM           FF% = 3%
           goto lookup_text
/*SR73678 - */

        lookup_text_done
           errormsg$ = "(Error) - No Text Found ? ? ?"
           gosub error_prompt
        return

       lookup_history                  /* Sales Order header info    */
            call "SHOSTAT" ("Searching History Data")
            cnt$ = "Checked [xxxxxxxx]"
            hit% = 0% : cnt% = 0%
REM            F1% = 2%                  /* (PAR001) */
            init(" ") hs_rec$, sav_key$, sav_yr$, cc$(), c1$()

            if sel% = 2% then                                          ~
               read #21,key >= hs_key$, using L61000, warr_rec$,         ~
                                          eod goto lookup_history_done ~
                         else                                          ~
               read #19, key kk% >= hs_key$, using L61005, hs_rec$,      ~
                                          eod goto lookup_history_done
L61000:        FMT CH(32)
L61005:        FMT CH(256)

            goto L61010
       lookup_history_nxt

            if sel% = 2% then                                          ~
               read #21,using L61000,warr_rec$,eod goto lookup_history_done~
                         else                                             ~
               read #19,using L61005, hs_rec$, eod goto lookup_history_done

L61010:     cnt% = cnt% + 1%
            if mod(cnt%,100%) <> 0 then goto L61015
               convert cnt% to str(cnt$,10%,8%), pic(########)
               print at(02,31);hex(84);cnt$;

L61015:     gosub test_hist
            if test% = 88% then goto lookup_history_nxt

            if test% = 99% then goto lookup_history_done
               if sav_key$ = tst_ky$ and sav_yr$ = tst_yr$ then         ~
                                                goto lookup_history_nxt
                  init(" ") sav_key$, sav_yr$
                  sav_key$ = tst_ky$
                  sav_yr$  = tst_yr$
                  hit% = hit% + 1%
                  if hit% < 505% then goto L61020
                     hit% = 504%              /* (504) = 37 Pages    */
                     goto lookup_2013_done

                                              /* Customer Code       */
L61020:           str(cc$(hit%),1%,9%)   = str(hs_rec$,17%,9%)
                                              /* Purchase Order Code */
                  str(cc$(hit%),13%,16%) = str(hs_rec$,26%,16%)
                                              /* Sales Order Code    */
                  str(cc$(hit%),30%,8%)  = str(hs_rec$,54%,8%)
                                              /* Order Date / Year   */
                  str(cc$(hit%),40%,10%) = str(hs_rec$,86%,6%)
                  call "DATFMTC" ( str(cc$(hit%),40%,10%) )
                                              /* Invoice Number      */
                  str(cc$(hit%),52%,8%)  = str(hs_rec$,193%,8%)
                                              /* Check Number        */
                  str(cc$(hit%),62%,8%)  = str(hs_rec$,201%,8%)
                                              /* Load Number         */
                  str(cc$(hit%),72%,5%)  = str(hs_rec$,49%,5%)

REM                  F1% = 2%
               goto lookup_history_nxt

        lookup_history_done
/* PAR001 */
REM            F1% = 18%
            init(" ") hs_rec$
            if sel% = 2% then                                          ~
               read #21,key >= hs_key$, using L61000, warr_rec$,         ~
                                          eod goto lookup_2005_done ~
                         else                                          ~
               read #19, key kk% >= hs_key$, using L61005, hs_rec$,      ~
                                          eod goto lookup_2005_done
            goto L61110
lookup_2005_nxt
/* PAR001 */

            if sel% = 2% then                                          ~
               read #21,using L61000,warr_rec$,eod goto lookup_2005_done~
                         else                                             ~
               read #19,using L61005, hs_rec$, eod goto lookup_2005_done

L61110:     cnt% = cnt% + 1%
            if mod(cnt%,100%) <> 0 then goto L61115
               convert cnt% to str(cnt$,10%,8%), pic(########)
               print at(02,31);hex(84);cnt$;

L61115:     gosub test_hist
            if test% = 88% then goto lookup_2005_nxt

            if test% = 99% then goto lookup_2005_done
               if sav_key$ = tst_ky$ and sav_yr$ = tst_yr$ then         ~
                                                goto lookup_2005_nxt
                  init(" ") sav_key$, sav_yr$
                  sav_key$ = tst_ky$
                  sav_yr$  = tst_yr$
                  hit% = hit% + 1%
                  if hit% < 505% then goto L61120
                     hit% = 504%              /* (504) = 37 Pages    */
                     goto lookup_2013_done

                                              /* Customer Code       */
L61120:           str(cc$(hit%),1%,9%)   = str(hs_rec$,17%,9%)
                                              /* Purchase Order Code */
                  str(cc$(hit%),13%,16%) = str(hs_rec$,26%,16%)
                                              /* Sales Order Code    */
                  str(cc$(hit%),30%,8%)  = str(hs_rec$,54%,8%)
                                              /* Order Date / Year   */
                  str(cc$(hit%),40%,10%) = str(hs_rec$,86%,6%)
                  call "DATFMTC" ( str(cc$(hit%),40%,10%) )
                                              /* Invoice Number      */
                  str(cc$(hit%),52%,8%)  = str(hs_rec$,193%,8%)
                                              /* Check Number        */
                  str(cc$(hit%),62%,8%)  = str(hs_rec$,201%,8%)
                                              /* Load Number         */
                  str(cc$(hit%),72%,5%)  = str(hs_rec$,49%,5%)

REM                  F1% = 18%
               goto lookup_2005_nxt

lookup_2005_done
/*SR73678 + */
REM            F1% = 19%
            init(" ") hs_rec$
            if sel% = 2% then                                          ~
               read #21,key >= hs_key$, using L61000, warr_rec$,         ~
                                          eod goto lookup_2013_done ~
                         else                                          ~
               read #19, key kk% >= hs_key$, using L61005, hs_rec$,      ~
                                          eod goto lookup_2013_done
            goto L61210
lookup_2013_nxt
/* PAR001 */

            if sel% = 2% then                                          ~
               read #21,using L61000,warr_rec$,eod goto lookup_2013_done~
                         else                                             ~
               read #19,using L61005, hs_rec$, eod goto lookup_2013_done

L61210:     cnt% = cnt% + 1%
            if mod(cnt%,100%) <> 0 then goto L61215
               convert cnt% to str(cnt$,10%,8%), pic(########)
               print at(02,31);hex(84);cnt$;

L61215:     gosub test_hist
            if test% = 88% then goto lookup_2013_nxt

            if test% = 99% then goto lookup_2013_done
               if sav_key$ = tst_ky$ and sav_yr$ = tst_yr$ then         ~
                                                goto lookup_2013_nxt
                  init(" ") sav_key$, sav_yr$
                  sav_key$ = tst_ky$
                  sav_yr$  = tst_yr$
                  hit% = hit% + 1%
                  if hit% < 505% then goto L61220
                     hit% = 504%              /* (504) = 37 Pages    */
                     goto lookup_2013_done

                                              /* Customer Code       */
L61220:           str(cc$(hit%),1%,9%)   = str(hs_rec$,17%,9%)
                                              /* Purchase Order Code */
                  str(cc$(hit%),13%,16%) = str(hs_rec$,26%,16%)
                                              /* Sales Order Code    */
                  str(cc$(hit%),30%,8%)  = str(hs_rec$,54%,8%)
                                              /* Order Date / Year   */
                  str(cc$(hit%),40%,10%) = str(hs_rec$,86%,6%)
                  call "DATFMTC" ( str(cc$(hit%),40%,10%) )
                                              /* Invoice Number      */
                  str(cc$(hit%),52%,8%)  = str(hs_rec$,193%,8%)
                                              /* Check Number        */
                  str(cc$(hit%),62%,8%)  = str(hs_rec$,201%,8%)
                                              /* Load Number         */
                  str(cc$(hit%),72%,5%)  = str(hs_rec$,49%,5%)

REM                  F1% = 19%
               goto lookup_2013_nxt

lookup_2013_done

/*SR73678 - */
        return

        test_hist
           init(" ") tst_ky$, tst_yr$, tst_key$
           on sel% goto Lsel1, Lsel2, Lsel3, Lsel4, Lsel5, Lsel6, Lsel7
Lsel1:                                 /* Check Customer, P.O. Year  */
           if str(hs_key$,1%,se%(1%)) <> str(hs_rec$,17%,se%(1%)) then ~
                                                  goto test_hist_done
           if se%(2%) = 0% then goto Lsel1a
           if str(se$(2%),1%,se%(2%)) <> str(hs_rec$,26%,se%(2%)) then ~
                                                   goto test_hist_skip
Lsel1a:       tst_ky$ = str(hs_rec$,17%,25%)
              goto Lselx
                                       /* Check Warranty Number      */
Lsel2:     if str(hs_key$,1%,se%(1%)) <> str(warr_rec$,1%,se%(1%)) then~
                                                   goto test_hist_done
                                       /* Lookup in (EWDHIST)        */
              init(" ") hs_rec$
              str(tst_key$,1%,8%) = str(warr_rec$,16%,8%)
              read #19,key 1% >= tst_key$, using L61005, hs_rec$,       ~
                                                eod goto test_hist_done

              if str(tst_key$,1%,8%) <> str(hs_rec$,54%,8%) then       ~
                                                    goto test_hist_done
                 tst_ky$ = str(hs_rec$,54%,8%)
                 warranty$    = str(warr_rec$,1%,8%)     /* (EWD001) */
                 warranty_so$ = str(hs_rec$,54%,8%)
                 warranty_ln$ = str(warr_rec$,9%,3%)
                                                         /* (EWD001) */
                 goto Lselx
                                       /* Check Customer S.O.        */
Lsel3:     if str(hs_key$,1%,se%(1%)) <> str(hs_rec$,54%,se%(1%)) then ~
                                                   goto test_hist_done
              tst_ky$ = str(hs_rec$,54%,8%)
              goto Lselx
                                       /* Check Load Customer S.O.   */
Lsel4:     if str(hs_key$,1%,5%) <> str(hs_rec$,49%,5%) then           ~
                                                    goto test_hist_done
           if se%(2%) = 0% then goto Lse14a
           if str(se$(2%),1%,se%(2%)) <> str(hs_rec$,54%,se%(2%)) then ~
                                                   goto test_hist_skip
Lse14a:       tst_ky$ = str(hs_rec$,49%,13%)
              goto Lselx
                                       /* Check Customer / Order Date*/
Lsel5:     if str(hs_key$,1%,se%(1%)) <> str(hs_rec$,77%,se%(1%)) then ~
                                                   goto test_hist_done
                                                   /* Check End Date */
           if str(hs_rec$,86%,6%) > str(tst_dte1$,1%,6%) then          ~
                                                   goto test_hist_done

              tst_ky$ = str(hs_rec$,77%,23%)
              goto Lselx
                                       /* Check Job No / Customer    */
Lsel6:     if str(hs_key$,1%,se%(1%)) <> str(hs_rec$,1%,se%(1%)) then  ~
                                                  goto test_hist_done
           if se%(2%) = 0% then goto Lsel6a
           if str(se$(2%),1%,se%(2%)) <> str(hs_rec$,17%,se%(2%)) then ~
                                                  goto test_hist_skip
Lsel6a:       tst_ky$ = str(hs_rec$,1%,25%)
              goto Lselx
                                       /* Invoice Number              */
Lsel7:     if str(hs_key$,1%,se%(1%)) <> str(hs_rec$,193%,se%(1%)) then ~
                                                  goto test_hist_done
              tst_ky$ = str(hs_rec$,193%,8%)

Lselx:     test% = 0%
           tst_yr$ = str(hs_rec$,45%,4%)
        return

        test_hist_done
           test% = 99%                               /* Finished     */
        return

        test_hist_skip
           test% = 88%
        return

        load_detail
           init(" ") hs_key$, tst_yr$, sav_key$, header$
           dtl% = 0%
           str(hs_key$,1%,8%) = str(cc$(i%),30%,8%)
           sav_key$ = hs_key$
           tst_yr$ = str(cc$(i%),46%,4%)
         load_detail_next
REM    /* PAR001 */
REM    FF% = 2%
REM    FF$ = "03"   /* FF$ = "03"                 SR73678 */
REM    IF TST_YR$ >= "2005" THEN  FF% = 18%
REM    IF TST_YR$ >= "2005" THEN  FF$ = "24"/*=24 SR73678 */
REM    IF TST_YR$ >= "2013" THEN  FF% = 19%     /*SR73678 */
REM    IF TST_YR$ >= "2013" THEN  FF$ = "25"/*=25 SR73678 */

           read #19,key 1% > hs_key$, using L62000, hs_rec$,            ~
                                              eod goto load_detail_done
L62000:       FMT CH(256)
           hs_key$ = str(hs_rec$,54%,15%)
           if str(hs_key$,1%,8%) <> str(sav_key$,1%,8%) then            ~
                                                  goto load_detail_done
           if str(hs_key$,12%,4%) <> tst_yr$ then                       ~
                                                  goto load_detail_next
           dtl% = dtl% + 1%
           if dtl% > 1% then goto L62010
                                                       /* Load Header  */
                                                       /* (PAR000)     */
              title$  = "Load No.: xxxxx"
              str(title$,11%,5%)   = str(hs_rec$,49%,5%)  /* Load No.  */
              str(hh$(1%),11%,9%)  = str(hs_rec$,17%,9%)  /* Customer  */
              str(hh$(1%),34%,8%)  = str(hs_rec$,54%,8%)  /* Sales Ord */
              str(hh$(2%),11%,16%) = str(hs_rec$,26%,16%) /* P.O. No.  */
              str(hh$(3%),11%,16%) = str(hs_rec$,1%,16%)  /* Job Name  */
              str(hh$(4%),11%,10%) = str(hs_rec$,109,10%) /* Quote No. */
              str(hh$(5%),11%,4%)  = str(hs_rec$,189%,4%) /* Sls Code  */
              str(hh$(5%),24%,4%)  = str(hs_rec$,185%,4%) /* Region Cd */

                                                       /* (PAR000)     */
              init(" ") tst_date$                      /* Order Date   */
              tst_date$ = str(hs_rec$,86%,6%)
              call "DATFMTC" (tst_date$)
              str(hh$(6%),13%,10%) = tst_date$

              init(" ") tst_date$                       /* Ship Date   */
              tst_date$ = str(hs_rec$,103%,6%)
              call "DATFMTC" (tst_date$)
              str(hh$(7%),13%,10%) = tst_date$

              init(" ") tst_date$                       /* Due Date    */
              tst_date$ = str(hs_rec$,209%,6%)
              call "DATFMTC" (tst_date$)
              str(hh$(8%),13%,10%) = tst_date$
                                                        /* Invoice No. */
              str(hh$(9%),13%,8%)  = str(hs_rec$,193%,8%)
                                                        /* Check No.   */
              str(hh$(10%),13%,8%) = str(hs_rec$,201%,8%)

              header% = 0%
              txtid$ = str(hs_rec$,69%,4%)
              gosub'099(txtid$)
              if txt% <> 0% then header% = 1%
              if txt% <> 0% then header$ = txtid$
              if txt% <> 0% then headerch$ = ff$   /*SR73678 */

L62010:                                           /* Format Detail   */
                                                  /* (PAR000)        */
            str(dt$(dtl%),1%,3%)   = str(hs_rec$,42%,3%)   /* Line Item*/
            str(dt$(dtl%),6%,25%)  = str(hs_rec$,119%,25%) /* Part No. */
            str(dt$(dtl%),33%,8%)  = "        "            /* Blank    */

            get str(hs_rec$,144%,2%), using L62020, ord%
L62020:       FMT BI(2)
            convert ord% to str(dt$(dtl%),45%,4%), pic(####)
                                                            /* Ord Qty  */
            get str(hs_rec$,146%,2%), using L62020, ord%
            convert ord% to str(dt$(dtl%),54%,4%), pic(####)
                                                            /* Shp Qty  */
            get str(hs_rec$,172%,8%), using L62030, ord
L62030:       FMT PD(14,4)
            convert ord to str(dt$(dtl%),61%,10%), pic(##,###.##-)
                                                            /* Ext Price*/
            str(dt$(dtl%),73%,3%) = "No "
            txtid$ = str(hs_rec$,73%,4%)
            gosub'099(txtid$)
            if txt% <> 0% then str(dt$(dtl%),73%,3%) = "Yes"

            if txt% <> 0% then str(dt$(dtl%),80%,4%) = txtid$
            ff$ = "25"
            str(dt$(dtl%),115%,2%) = ff$   /*SR73678 file channel # of text  */
                                                            /* Store Id  */
                                                            /* Line Text */
            if warranty_so$ = str(hs_rec$,54%,8%) and                      ~
               warranty_ln$ = str(hs_rec$,62%,3%) then                     ~
               str(dt$(dtl%),77%,2%) = "**"
                                                            /* (EWD001)  */
                                                            /* (PAR000)  */
                                                            /* Sub Part  */
            str(dt$(dtl%),85%,20%) = str(hs_rec$,215%,20%)
                                                            /* Info Part */
            str(dt$(dtl%),105%,9%) = str(hs_rec$,235%,9%)

                                                            /* (PAR000)  */

            goto load_detail_next
        load_detail_done

        return

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                 /* (EWD001) - New    */
        load_warranty_ids
            warr% = 0%
            init(" ") warr_key$, sav_warr$, warr$()
            str(warr_key$,1%,8%) = str(hh$(1%),34%,8%)
            str(warr_key$,9%,3%) = str(dt$(line%),1%,3%)
            sav_warr$ = str(warr_key$,1%,11%)
       /* PAR001 */
REM       FF% = 4%
REM           IF STR(HH$(6%),19%,10%) >= "2005" THEN FF% = 20%
REM           IF STR(HH$(6%),19%,10%) >= "2013" THEN FF% = 21%  /*SR73678 */
REM    IF STR(WARR_REC$,12,4) = "2005" THEN  FF% = 20%

            read #21,key 1% >= warr_key$, using L63000, warr_rec$,         ~
                                        eod goto load_warranty_done
L63000:        FMT CH(32)
            goto L63010
        load_warranty_nxt
            read #21, using L63000, warr_rec$, eod goto load_warranty_done
L63010:     warr_key$ = str(warr_rec$,16%,15%)
 
            if str(warr_key$,1%,11%) <> sav_warr$ then                   ~
                                                 goto load_warranty_done
            warr% = warr% + 1%
            if warr% > 100% then warr% = 100%
            str(warr$(warr%),1%,8%)  = str(warr_rec$,1%,8%)
            str(warr$(warr%),15%,3%) = str(warr_rec$,9%,3%)
            str(warr$(warr%),20%,4%) = str(warr_rec$,12%,4%)
            goto load_warranty_nxt
        load_warranty_done
            if warr% = 0% then return
               gosub display_warranty
        return
                                                 /* (EWD001) - New    */
                                                 /* (PAR000)          */
        load_part_explosion
            init(" ") hs_ln$, hs_so$, hs_part$, hs_sub_part$, hs_info_part$
            dim1es, dim2es, dim3es = 0.00        /* (AWD002)          */
                                                 /* Explosion Switch  */
            switch% = 0%                         /* 0% = History      */
                                                 /* 1% = Caelus       */
                                                 /* S.O. Line Item    */
            hs_ln$        = str(dt$(line%),1%,3%)
                                                 /* Sales Order No.   */
            hs_so$        = str(hh$(1%),34%,8%)
                                                 /* Part Number       */
            hs_part$      = str(dt$(line%),6%,25%)
                                                 /* Sub Part Number   */
            hs_sub_part$  = str(dt$(line%),85%,20%)
                                                 /* Infor Part Number */
            hs_info_part$ = str(dt$(line%),105%,9%)

            call "AWDPARTN" ( switch%,       /* Current Planning Tables*/~
                              hs_ln$,        /* S. O. Line Item   (In )*/~
                              hs_so$,        /* Sales Order No.   (In )*/~
                              hs_part$,      /* Old Part Number   (In )*/~
                              hs_sub_part$,  /* Sub Part Number   (Hst)*/~
                              hs_info_part$, /* Infor Part Number (Hst)*/~
                              dim1es,        /* (AWD002)               */~
                              dim2es,        /* (AWD002)               */~
                              dim3es,        /* (AWD002)               */~
                              #63,           /* New Sub Part Number    */~
                              #5 )           /* FILE = (GENCODES)      */

        return
                                                 /* (PAR000)         */
        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            end

