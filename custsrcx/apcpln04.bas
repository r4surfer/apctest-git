        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLN04 - Subroutine (APCPLN7B)     *~
            *  Creation Date     - 05/01/96                             *~
            *  Last Modified Date- 04/05/05                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Modified By       - Christie M. Gregory                  *~
            *                                                           *~
            *  Description       - Utility Program used to Define Loads.*~
            *                      The Production Week and Production   *~
            *                      Day are Used to Determine the Date   *~
            *                      to Start Planning the Load.          *~
            *                                                           *~
            *  Code Tables Used  - (PLAN REGN)                          *~
            *                                                           *~
            *  Subroutine Used   - AWDPLN0B - Calc and Edit Production  *~
            *                                 Year, Week, and Day       *~
            *                      APCPLN5B - Create (APCPLNOR) Data    *~
            *                      APCPLN6B - Create (APCPLNSC) and     *~
            *                                 (APCPLNSD) Data           *~
            *                                                           *~
            *  Special Comments  - Load Number is Assigned from Store   *~
            *                      (000). Three Types of Load Numbers   *~
            *                      Regular Load Number (00000).         *~
            *                      Alfha Load Number   (A0000) Inserts  *~
            *                      Stock Load Number   (S0000) Stock    *~
            *                      Also Stock Sales Order Number is     *~
            *                      assigned from store (000)-(S0000000) *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/12/96 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 04/29/97 ! Mod to Stock, Change P.O. Assignment     ! RHH *~
            *          !   Use Date and Time for P.O. Number      !     *~
            * 07/15/97 ! Corr Problem with Deleting Stock Loads.  ! RHH *~
            * 11/13/97 ! Mod for Upgrade to new Release R6.04.03  ! RHH *~
            * 03/14/98 ! Y2K                                      ! LDJ *~
            * 04/22/98 ! Several Y2K fixes                        ! ERN *~
            * 08/14/98 ! (EWD002) - Mods to Update Daily Sales    ! RHH *~
            *          !    Analysis                              !     *~
            *          ! (EWD003) - New File for Special Schedule !     *~
            *          !   (EWDSCHED). Tempered Glass & Grid      !     *~
            * 09/22/98 ! (EWD004) - For Freeze codes 90 - 98      ! RHH *~       
            * 08/18/99 ! (EWD005) - Mod to replace 'OPENCHCK'     ! RHH *~
            *          !            with new 'EWDOPEN' Subroutine !     *~
            *          !            to improve HP speed.          !     *~
            * 07/23/01 ! (EWD006) - Mod to allow creating Stock   ! CMG *~
            *          !            loads that will be for Dallas !     *~
            *          !            stock.                        !     *~    
            * 12/30/01 ! (EWD007) - Mod for year 2002.            ! CMG *~
            * 11/22/02 ! (EWD008) - Mods for Effective Units on   ! CMG *~
            *          !              display screen.             !     *~
            * 02/20/03 ! (EWD009) - Mods to add new Appian Date   ! CMG *~
            * 03/25/03 ! (EWD010) - Mods to add 'P' Load Number   ! CMG *~
            * 04/05/05 ! (AWD011) - Mod to the reclen of APCPLNSD ! CMG *~
	    * 01/15/07 ! (AWD012) - mod to fix AWDAPPLD ld desc   ! CMG *~
            * 10/17/17 ! (CR1174) - New Shipment Block and format ! RDB *~
            *          !            on APCPLNLD of filler space   !     *~
            * 01/23/19 ! (CR1876) - New Load Date Entry for P load! RDB *~
            * 04/09/19 ! (CR1979) - New G & L loads               ! DES *~
            *************************************************************

        dim                              /* (APCPLNLD) - FILE          */~
            filename$8,                  /* Use with EWDOPEN - EWD005  */~            
            ld_region$2, ld_region_d$30, /* Load Region Code (Primary) */~
            ld_wk_dte$10, sc_wk_dte$10,  /* Calculated Production Date       (Y2K, LDJ) */~
            ld_load$5,                   /* Load Number (0),(A),(S)    */~
            ld_desc$30,                  /* Load Description           */~
            ld_dts1$10,                  /* Scheduled Production Date        (Y2K, LDJ) */~
            ld_dts2$10,                  /* Scheduled Completion Date        (Y2K, LDJ) */~
            ld_dts3$10,                  /* Scheduled Load Date              (Y2K, LDJ) */~
            ld_dtp1$10,                  /* Planned Production Date          (Y2K, LDJ) */~
            ld_dtp2$10,                  /* Planned Completion Date          (Y2K, LDJ) */~
            ld_dtp3$10,                  /* Planned Load Date                (Y2K, LDJ) */~
            ld_status$2,                 /* Current Status of Load     */~
            ld_date$10,                  /* Current Status Date              (Y2K, LDJ) */~
            ld_yr$2,                     /* Scheduled Production Year  */~
            ld_year$4,                   /*   ""           ""     ""         (Y2K, LDJ) */~
            ld_wk$2,                     /* Scheduled Production Week  */~
            ld_day$1, sv_day$1,          /* Scheduled Production Day   */~
            ld_sister$2,                 /* Sister Compay Code   EWD006*/~
            ld_sis_desc$10,              /* Sister Description   EWD006*/~
/*CR1174*/  ld_userid$3,                 /* User ID Staging/Shipping   */~
/*CR1174*/  ld_time_stage$4,             /* Time Staging/Shipping      */~
/*CR1174*/  ld_awd$3,                    /* AWD Default                */~
/*CR1174*/  ld_shp_blk$3,                /* Shipment Block             */~
/*CR1174*/  ld_fil$5,                    /* Filler Area                */~
            tot_value$12,                /* Total Dollar Value OF Load */~
            tot_units$12,                /* Total Units for Load       */~
            tot_pull$5,                  /* Total Pulls for Load       */~
            tot_make$5,                  /* Total Makes for Load       */~
            tot_product$5,               /* Total Product for Load     */~
            tot_drops$3                  /* Total Number of Drops      */

        dim                              /* (Program) - Variables      */~
            date$10, dateme$8,dateme2$8, /* dates                      */~
            or_so$8, sav_so$8,           /* Sales Order Number         */~
            or_no$(5%)8, or_po$16,       /* Cust,S.O.,Inv,Chk, P.O.    */~
            sc_key1$27,                  /* (APCPLNSC) - ALT (1) Key   */~
            store$3, sav_drop_seq$5,     /* Beg/End Region Codes       */~
            l1$8, l2$8, l3$7,            /* Use for Assigning Load No's*/~
            lx$5, tx$1,                  /* P Load Number    (EWD010)  */~
            l4$5,                        /* P Load Number    (EWD010)  */~
            l6$5,                        /* G Load Number    (CR1979)  */~
            l7$5,                        /* L Load Number    (CR1979)  */~
            cnt$10,                      /* Use for Screen Print       */~
            sc$(8%)30,                   /* Load Messages    (EWD010)  */~
            scr$(5%)50,                  /* Header Display Screen      */~
            readkey$50, desc$30,         /* Generic Key                */~
            prd_dte$6, jdate1$7,         /* For Production Week Calc         (Y2K, LDJ) */~
            or_rec$170, sc_rec$128,      /* (APCPLNOR) and (APCPLNSC)  */~
            hdr$40, msg$(3%)79,          /* Askuser - Prompts          */~
            x1$10, x2$10,                /* Use for Date Edits               (Y2K, LDJ) */~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(20%),                    /* = 0 if the file is open    */~
            f1%(20%),                    /* = 1 if READ was successful */~
            fs%(20%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(20%)20                 /* Text from file opening     */

        dim                              /* Subroutine - Variables     */~
            cur_yr$2,  prv_yr$2,         /* Current Year               */~
            cur_wk$2,  cur_dy$1,         /* Current Prod. Week an Day  */~
            cur_dte$6, cur_date$10,      /* Prod Week Date Form/Unform       (Y2K, LDJ) */~
            ent_yr$2,                    /* Julian Year and Day YYDDD  */~
            ent_wk$2,  ent_dy$1,         /* Entry Prod. Week an Day    */~
            ent_dte$6, ent_date$10       /* Prod Week Date Form/Unform       (Y2K, LDJ) */

        dim                              /* Variables - Delete Routine */~
            dt_key$23, ad_key$33,        /* 'DT' and 'AD' Primary Keys */~
            dt_bar$18, dt_st$2,          /* Detail Barcode             */~
            dt_dept$3, dt_yr$2,dt_year$4,/* Detail Dept and Prod Year        (Y2K, LDJ) */~
            dt_wk$2, dt_day$2,           /* Prod Week and Day          */~
            dt_shft$2, dt_proc$2,        /* Detail Shift/Process Code  */~
            pl_unts(7%), pl_unta%(7%)    /* Reamining Cap./Units Planed*/

* PAR000
        dim stk_part$45, stk_desc$30,    /* Stock Part No/Description  */~
            stk_qty$4, stk_time$8,       /* Stock Quantity             */~
            stk_load$5,                  /* Stock Load Number          */~
            stk_so$8, sto$8,             /* Stock Sales Order No       */~
            stk$(99%)25, qty$(99%)4,     /* Store Stock Part Numbers   */~
/*PAR000*/  stk_sub$(99%)20,             /* Store Stock Sub Part Num   */~
/*PAR000*/  stk_desc$(99%)30,            /* Store Stock Descriptions   */~
            upccode$20, stk_dte$6        /* UPC Code , DUE DATE        */

        dim fix_key$23, fix_rec$256,     /* Fix Key and Rec (APCPLNDT) */~
            fix_so$8,                    /* Fix S.O. Number            */~
            fix_dte$10, fix_date$10,     /* Fix New Production Date          (Y2K, LDJ) */~
            fix_load$5,                  /* Fix Old Load No.           */~
            fix_dte1$10                  /* Fix Old Production Date          (Y2K, LDJ) */

        dim                              /*      (EWD008)              */~
            ef_part$25,                  /*  Part Number from 'SC'     */~
            pl_unte%(7%)                 /*  Remaining Effective Units */

        dim ld_app_dte$10,               /* Appian Load Date     EWD009*/~
            ap_ear_tme$4,                /* Appian Earlies Dispatch Tme*/~
            ap_start_dte$10,             /* Appian Truck Start Date    */~
            ap_start_tme$4,              /* Appian Truck Start Time    */~
            ap_end_dte$10,               /* Appian Truck End Date      */~
            ap_end_tme$4,                /* Appian Truck End Time      */~
            ap_truck$5,                  /* Appian Truck Number        */~
            ap_flag$1,                   /* Have Appian been uploaded  */~
            ap_up_dte$10,                /* Appian Upload Date         */~
            ap_type$1,                   /* Appian Load or Not         */~
            ap_desc$30,                  /* Appian Load Description    */~
            ap_filler$45,                /* Filler Area                */~
            ap_blank$50,                 /* Blank Field Info           */~
            store_key$18                 /* STORE (000)         (EWD010)*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "Production Loads and Stock Scheduling   "
            pname$ = "APCPLN04 - Rev: R6.04"

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
            * #1  ! APCPLNLD ! (NEW) Load Master File                   *~
            * #2  ! APCPLNOR ! (NEW) S.O. Header Histroy                *~
            * #3  ! GENCODES ! Master Code Tables File                  *~
            * #4  ! APCPLNSC ! Planning Master Schedule File            *~
            * #5  ! STORNAME ! Master System Store File                 *~
            * #6  ! APCPLNUC ! Planning Master Capacity File            *~
            * #7  ! APCPLNDT ! Planning Master Detail File              *~
            * #8  ! APCPLNAD ! Production Scanning Detail File          *~
/*PAR000*/  * #9  ! INVMASTR ! Inventory Master File                    *~
            * #10 ! CUSTOMER ! Customer Master File                     *~
            * #11 ! BCKMASTR ! S.O. Header Detail                       *~
            * #12 ! BCKLINES ! S.O. Line Item Detail                    *~
            * #13 ! APCPULLS ! Stock Pulls Inventory                    *~
/*PAR000*/  * #14 ! INVQUAN  ! Inventory Quantities                     *~
            * #15 ! APCPLNDP ! New Planning Dept Spec's                 *~
            * #16 ! APCPLNSD ! New Planning Sched Dept Detail           *~
            * #17 ! APCPLNSA ! Daily Sales Analysis       (EWD002)      *~
            * #18 ! EWDSCHED ! Special Schedule Analysis  (EWD003)      *~
            * #63 ! BCKSUBPT ! Sub Part File                 (PAR000)   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 11,   keylen =  5,                      ~
                        alt key 1, keypos =  3, keylen = 13,             ~
                            key 2, keypos =  1, keylen = 15

            select #2,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos = 1,    keylen = 51,                      ~
                        alt key 1, keypos = 27, keylen = 25,             ~
                            key 2, keypos = 70, keylen =  8, dup,        ~
                            key 3, keypos = 78, keylen =  8, dup,        ~
                            key 4, keypos = 52, keylen =  8,             ~
                            key 5, keypos = 36, keylen = 16, dup

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24

            select #4,  "APCPLNSC",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 24,   keylen = 10,                      ~
                        alt key 1, keypos =  7, keylen = 27,             ~
                            key 2, keypos =  1, keylen = 33

            select #5,  "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3

            select #6,  "APCPLNUC",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 11,                      ~
                        alt key 1, keypos =  7, keylen = 11

            select #7,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  24,  keylen =  23,                     ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup

            select #8,  "APCPLNAD",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =  19,  keylen =  33,                     ~
                        alt key 1, keypos =  1, keylen = 33


* PAR000
            select #9,  "INVMASTR",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =  45,                     ~
                        alt key  1, keypos =  122, keylen =   9, dup,    ~
                            key  2, keypos =  110, keylen =   4, dup,    ~
                            key  3, keypos =   46, keylen =  32, dup

            select #10,  "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #11, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #12, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #13,  "APCPULLS",                                     ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =   10, keylen =   10,                    ~
                        alt key  1, keypos =    1, keylen =  19,         ~
                            key  2, keypos =   20, keylen =  25, dup

* PAR000
            select #14, "INVQUAN",                                       ~
                        varc,     indexed,  recsize =  768,              ~
                        keypos =   17, keylen =  64,                     ~
                        alt key  1, keypos =    1, keylen =  64


            select #15,  "APCPLNDP",                                     ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos =   11, keylen =   12,                    ~
                        alt key  1, keypos =    9, keylen =  14,         ~
                            key  2, keypos =    4, keylen =  12,         ~
                            key  3, keypos =    1, keylen =  15
/* (AWD011) - Mod to key and reclen */
            select #16, "APCPLNSD",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  23

                                                    /* (EWD001)-Begin */   
            select #17, "APCPLNSA",                                      ~
                        varc,     indexed,  recsize = 32,                ~
                        keypos = 11,   keylen = 17,                      ~
                        alt key  1, keypos =     1, keylen = 19, dup
                                                    /* (EWD001)-End   */
            select #18, "EWDSCHED",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  38,                     ~
                        alt key  1, keypos =   16, keylen =  23

                                                             /* (EWD009) */
             select #19, "AWDAPPLD"                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   12, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =  16,         ~
                            key  2, keypos =    2, keylen =  15,         ~
                            key  3, keypos =   17, keylen =  15

                                                    /* (EWD010) Begin */
            select #20, "SYSFILE2"                                        ~
                       varc, indexed, recsize = 500,                     ~
                       keypos = 1, keylen = 20
                                                    /* (EWD010) End   */

/* (PAR000)  */

            select #63, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~ 
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup   

            call "SHOSTAT" ("Opening Files, One moment Please?")
                                                         /* (EWD005)   */
            filename$ = "APCPLNLD" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNOR" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSC" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "STORNAME" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNUC" : call "EWDOPEN" (#6, filename$, err%) 
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDT" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNAD" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
* PAR000
            filename$ = "INVMASTR" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "CUSTOMER" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKMASTR" : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#12, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPULLS" : call "EWDOPEN" (#13, filename$, err%)
            if err% <> 0% then gosub open_error
* PAR000
            filename$ = "INVQUAN"  : call "EWDOPEN" (#14, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDP" : call "EWDOPEN" (#15, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSD" : call "EWDOPEN" (#16, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSA" : call "EWDOPEN" (#17, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDSCHED" : call "EWDOPEN" (#18, filename$, err%)
            if err% <> 0% then gosub open_error
                                                         /* (EWD005)   */
                                                         /* (EWD009)   */
            call "OPENCHCK" (#19, fs%(19%), f2%(19%),500%, rslt$(19%))

                                                         /* (EWD010)   */
            filename$ = "SYSFILE2" : call "EWDOPEN" (#20, filename$, err%)
            if err% <> 0% then gosub open_error
* PAR000
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
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

            sc$(1%) = "Load Number Already Assigned  "
            sc$(2%) = "Assign A Regular Load Number  "
            sc$(3%) = "Assign A Stock Load Number    "
            sc$(4%) = "Assign A Alpha Load Number    "
            sc$(5%) = "Assign Production Load Number "         /*  (EWD010) */
            sc$(6%) = "Assign Goose-neck Load Number "         /*  (CR1979) */
            sc$(7%) = "Assign Local Load Number      "         /*  (CR1979) */
            sc$(8%) = "                              "         /*  (EWD010) */

            scr$(1%)="**************************************************"
            scr$(2%)="*(New) Load Definition/Analysis Utility(APCPLN04)*"
            scr$(3%)="**************************************************"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 11%        /* (EWD009)          */
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 10% then goto inputmode_a
                      if keyhit%  = 14% then goto inputmode_b
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
                  if keyhit%  = 11% then goto inputmode_a
REM                  if keyhit%  = 12% then gosub delete_load
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11150:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% > 11% then editpg1    /* (EWD009) */
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
            *       I N P U T   M O D E   F O R   S T O C K             *~
            *************************************************************

        inputmode_a
            for fieldnr% = 1% to  2%
L12060:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L12190
L12080:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L12160
L12110:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L12080
                         if fieldnr% = 1% then L12060
                         goto L12110
L12160:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% = 14% then gosub process_stock
                      if keyhit% <> 0% then       L12080
L12190:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12080
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   F O R   S T O C K              *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 10% then gosub update_stock
                  if keyhit%  = 14% then gosub process_stock
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg2
L13120:     fieldnr% = cursor%(1%) - 14%
            if fieldnr% < 1% or fieldnr% > 2% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L13170:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13170
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13170
                  lastfieldnr% = fieldnr%
            goto L13120


        REM *************************************************************~
            *       P l a n   F i x   S c r e e n                       *~
            *************************************************************

        inputmode_b
            gosub initialize_variables
            for fieldnr% = 1% to  2%
L14060:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L14190
L14080:         gosub'103(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L14160
L14110:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L14080
                         if fieldnr% = 1% then L14060
                         goto L14110
L14160:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L14080
L14190:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L14080
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   F O R   P l a n   F i x        *~
            *************************************************************

        editpg3
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub process_fix
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg3
L15120:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% > 2% then editpg3
            if fieldnr% = lastfieldnr% then    editpg3
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg3
L15170:     gosub'103(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L15170
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L15170
                  lastfieldnr% = fieldnr%
            goto L15120

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        process_stock
           if stk_max% = 0% then goto L19130
              call "SHOSTAT" ("Processing Stock load ("&stk_load$&")")
              gosub update_stock             /* Clear Out Last Entries */
              gosub assign_order             /* Assign Stock S.O. No.  */
              gosub build_stock              /* Build (APCPLNOR) and   */
              gosub stock_message            /* (APCPLNSC),(APCPLNSD)  */
L19130: return clear all
        goto inputmode

        stock_message
           convert cnt% to cnt$, pic(00000)
           comp% = 2%
           hdr$ = "********* Stock Scheduling *********"
           msg$(1%) = "Number of Stock Windows Scheduled( "&cnt$&" )"
           msg$(2%) = "for Load [ "&stk_load$&" ] Sales Order [ "&stk_so$~
        &" ]"
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return clear all
        goto inputmode

        process_fix
           call "SHOSTAT" ("CorrectinG Sales Order ( "& fix_so$ &" )")
        stop " CHECK FIX " : close ws

           init(" ") fix_key$, fix_rec$
           str(fix_key$,1%,8%) = fix_so$
        process_fix_nxt
           read #7,hold,key > fix_key$, using L19370, fix_rec$,           ~
                                               eod goto process_fix_done
L19370:       FMT CH(256)
           fix_key$ = str(fix_rec$,24%,23%)
           if str(fix_key$,1%,8%) <> fix_so$ then goto L19470
              delete #7

              str(fix_rec$,47%,6%) = str(fix_date$,1%,6%)
              put #7, using L19370, fix_rec$
              write #7, eod goto L19490
              goto process_fix_nxt
        process_fix_done
L19470: return clear all
        goto inputmode_b
L19490:     errormsg$ = "(Error - Correcting Barcode ------> "& fix_key$
            gosub error_prompt
            goto L19470

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            if opn% = 0% then goto L20120
               if edit% = 2% then return
               if str(ld_load$,1%,3%) = "N/A" then ld_load$ = " "
               if len(ld_load$) = 5 then enabled% = 0%
L20120: return

        deffn'052(fieldnr%)
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
                                                             /*  (EWD006)  */
        scrn1_msg  :  data                                               ~
         "Enter a Valid Load Number and Sister Company Code or ' '=Reg,'A'=Forecast.'S'=Stock?",~
         "Enter the Applicable Description for Load?                   ",~
         "Enter a Valid Production Week and Year?                      ",~
         "Enter a Valid Starting Load Production Day?                  ",~
         "Enter the Applicable Region Code for Load?                   ",~
         "Enter a Valid Schedule Production Date?                      ",~
         "Enter a Valid Schedule Completion Date?                      ",~
         "Enter a Valid Schedule Load Date?                            ",~
         "Enter a Valid Appian Date?                                   ",~
         "Enter a Valid Appian Time, Military Format HHMM              " /* (EWD009) */

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28310
                inpmessage$ = edtmessage$
                return

L28310
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid Manufactured Part Number for Stock (19 Digits)?",~
         "Enter the Appropriate Quantity for Production of Stock?      "

        deffn'070(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28450
                inpmessage$ = edtmessage$
                return

L28450
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn3_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn3_msg  :  data                                               ~
         "Enter the Customer Sales Order for Production Date Change?   ",~
         "Enter the (New) Production Date for the Sales Order?         "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, readkey$, desc$,ld_region$,~
                      ld_wk_dte$, ld_desc$, ld_dts1$, ld_dts2$,          ~
                      ld_dts3$, ld_dtp1$, ld_dtp2$, ld_dtp3$, ld_status$,~
                      ld_date$, ld_wk$, ld_day$, ld_fil$, store$,        ~
                      tot_value$, tot_units$, tot_pull$, tot_make$,      ~
                      tot_product$, tot_drops$, ld_yr$, ld_year$,prd_dte$,jdate1$,      /* (Y2K, LDJ) */~
                      sc_wk_dte$, ld_region_d$, dt_key$, ad_key$, dt_yr$,dt_year$,      /* (Y2K, LDJ) */~
                      dt_wk$, dt_day$, dt_shft$, dt_proc$, dt_dept$,     ~
                      dt_bar$, stk$(), qty$(), or_no$(), stk_dte$,       ~
                      fix_rec$, fix_key$, fix_so$, fix_dte$, fix_load$,  ~
                      fix_dte1$, ld_sister$, ld_sis_desc$, ld_app_dte$,  ~
                      ap_ear_tme$, ap_start_dte$, ap_start_tme$,         ~
                      ap_end_dte$,  ap_end_tme$, ap_truck$, ap_flag$,    ~
                      ap_up_dte$, ap_type$, ap_desc$, ap_filler$,        ~
                      ap_blank$, stk_sub$(), stk_desc$, stk_desc$(),     ~
                      ld_shp_blk$

            if opn% = 0% then ld_load$ = " "
            assign% = 7%                                   /*  (EWD010)  */
            ld_sister$ = "00"                        /*  (EWD006)  */
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
        dataload
            rec% = 0%
            read #1,key = ld_load$, eod goto L30380

        get #1, using L35040,                                             ~
            ld_region$,                  /* Load Region Code (Primary) */~
            ld_wk_dte$,                  /* Calculated Production Date */~
            ld_load$,                    /* Load Number (0),(A),(S)    */~
            ld_desc$,                    /* Load Description           */~
            ld_dts1$,                    /* Scheduled Production Date  */~
            ld_dts2$,                    /* Scheduled Completion Date  */~
            ld_dts3$,                    /* Scheduled Load Date        */~
            ld_dtp1$,                    /* Planned Production Date    */~
            ld_dtp2$,                    /* Planned Completion Date    */~
            ld_dtp3$,                    /* Planned Load Date          */~
            ld_status$,                  /* Current Status of Load     */~
            ld_date$,                    /* Current Status Date        */~
            ld_yr$,                      /* Current Production Year    */~
            ld_wk$,                      /* Scheduled Production Week  */~
            ld_day$,                     /* Scheduled Production Day   */~
            ld_sister$,                  /* Sister Code       EWD006   */~
            ld_userid$,                  /* User ID in Scanning        */~
            ld_time_stage$,              /* Time staging               */~
            ld_awd$,                     /* Default AWD                */~
            ld_shp_blk$,                 /* Shipment Block             */~
            ld_fil$                      /* Filler Area                */

            stk_dte$ = str(ld_dts3$,1%,6%)
            call "DATFMTC" (ld_wk_dte$)                                         /* (Y2K, LDJ) */
            call "DATFMTC" (ld_dts1$)                                           /* (Y2K, LDJ) */
            call "DATFMTC" (ld_dts2$)                                           /* (Y2K, LDJ) */
            call "DATFMTC" (ld_dts3$)                                           /* (Y2K, LDJ) */
            call "DATFMTC" (ld_dtp1$)                                           /* (Y2K, LDJ) */
            call "DATFMTC" (ld_dtp2$)                                           /* (Y2K, LDJ) */
            call "DATFMTC" (ld_dtp3$)                                           /* (Y2K, LDJ) */
            convert val(ld_yr$,2) to ld_year$, pic(0000)                        /* (Y2K, LDJ) */
            gosub L50480                  /* Load Production Week Data  */
            rec% = 1%
            gosub load_appld                            /*  (EWD009)    */
            gosub scan_load
            gosub L51100        /* CR1174 missing description on get */    
            stk% = 0%

L30380: return

        load_appld                                      /*  (EWD009)  */
            read #19,key = ld_load$, eod goto no_appld

            get #19, using L30550, ld_app_dte$, ap_ear_tme$


            call "DATFMTC" (ld_app_dte$)                      
        no_appld
        return                                          /*  (EWD009)  */

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        delete_load
        dataput
            if assign% > 0% and assign% < 7% then gosub assign_load
            read #1,hold,key = ld_load$, eod goto L31210
               if keyhit% <> 12% then goto L31190
                  if ld_status$ < "11" then goto L31150
                     errormsg$ = "Cannot 'DELETE LOAD' Product Complete?"
                     gosub error_prompt
                     goto dataput_done
L31150:           delete #1
                  gosub delete_detail
                  goto dataput_done

L31190:        delete #1                           /* Delete Load       */

L31210:     call "DATUFMTC" (ld_wk_dte$)                                        /* (Y2K, LDJ) */
            call "DATUFMTC" (ld_dts1$)                                          /* (Y2K, LDJ) */
            call "DATUFMTC" (ld_dts2$)                                          /* (Y2K, LDJ) */
            call "DATUFMTC" (ld_dts3$)                                          /* (Y2K, LDJ) */
            call "DATUFMTC" (ld_dtp1$)                                          /* (Y2K, LDJ) */
            call "DATUFMTC" (ld_dtp2$)                                          /* (Y2K, LDJ) */
            call "DATUFMTC" (ld_dtp3$)                                          /* (Y2K, LDJ) */
            convert ld_year$ to ld_year%                                        /* (Y2K, LDJ) */
            ld_yr$ = bin(ld_year%,2)                                            /* (Y2K, LDJ) */
            ld_status$ = "02"
            str(ld_date$,1%,6%) = date
            str(ld_date$,7%,2%) = "  "

        put #1, using L35040,                                             ~
            ld_region$,                  /* Load Region Code (Primary) */~
            ld_wk_dte$,                  /* Calculated Production Date */~
            ld_load$,                    /* Load Number (0),(A),(S)    */~
            ld_desc$,                    /* Load Description           */~
            ld_dts1$,                    /* Scheduled Production Date  */~
            ld_dts2$,                    /* Scheduled Completion Date  */~
            ld_dts3$,                    /* Scheduled Load Date        */~
            ld_dtp1$,                    /* Planned Production Date    */~
            ld_dtp2$,                    /* Planned Completion Date    */~
            ld_dtp3$,                    /* Planned Load Date          */~
            ld_status$,                  /* Current Status of Load     */~
            ld_date$,                    /* Current Status Date        */~
            ld_yr$,                      /* Current Production Year    */~
            ld_wk$,                      /* Scheduled Production Week  */~
            ld_day$,                     /* Scheduled Production Day   */~
            ld_sister$,                  /* Sister Code       EWD006   */~
            ld_userid$,                  /* User ID in Scanning        */~
            ld_time_stage$,              /* Time staging               */~
            ld_awd$,                     /* Default AWD                */~
            ld_shp_blk$,                 /* Shipment Block             */~
            ld_fil$                      /* Filler Area                */
            
            write #1, eod goto dataput_done

            gosub put_appld                        /*  (EWD009)        */
        dataput_done
            return clear all
            goto inputmode

        put_appld                                        /*  (EWD009)  */
            ap_type$ = "N"
            str(ap_desc$,1%,30%) = str(ld_desc$,1%,30%)
            call "DATUFMTC" (ld_app_dte$)
            call "DATUFMTC" (ap_start_dte$)
            call "DATUFMTC" (ap_end_dte$)
            call "DATUFMTC" (ap_up_dte$)
            ap_start_tme$, ap_end_tme$ = "9999"
            ap_flag$ = "0"
            ap_truck$ = "99999"

            read #19,hold, key = ld_load$, eod goto no_appld_put
                 get #19, using L30560,                                 ~
                          ap_flag$,      /* Appian Uploaded      EWD009*/~
                          ap_blank$,     /* Appian Load Date           */~
                          ap_blank$,     /* Appian Earlies Dispatch Tme*/~
                          ld_load$,      /* Load Number                */~
                          ap_start_dte$, /* Appian Truck Start Date    */~
                          ap_start_tme$, /* Appian Truck Start Time    */~
                          ld_load$,      /* Load Number                */~
                          ap_end_dte$,   /* Appian Truck End Date      */~
                          ap_end_tme$,   /* Appian Truck End Time      */~
                          ap_truck$,     /* Appian Truck Number        */~
                          ap_up_dte$,    /* Appian Upload Date         */~
/* (AWD012)  */           ap_blank$,     /* Appian Load Description    */~
                          ap_type$,      /* Is this an Appian Load     */~
                          ap_filler$     /* Appian Filler Area         */

                 delete #19

        no_appld_put            
            put #19, using L30560,                                       ~
            ap_flag$,                    /* Appian Uploaded      EWD009*/~
            ld_app_dte$,                 /* Appian Load Date           */~
            ap_ear_tme$,                 /* Appian Earlies Dispatch Tme*/~
            ld_load$,                    /* Load Number                */~
            ap_start_dte$,               /* Appian Truck Start Date    */~
            ap_start_tme$,               /* Appian Truck Start Time    */~
            ld_load$,                    /* Load Number                */~
            ap_end_dte$,                 /* Appian Truck End Date      */~
            ap_end_tme$,                 /* Appian Truck End Time      */~
            ap_truck$,                   /* Appian Truck Number        */~
            ap_up_dte$,                  /* Appian Upload Date         */~
            ap_desc$,                    /* Appian Load Description    */~
            ap_type$,                    /* Is this an Appian Load     */~
            ap_filler$                   /* Appian Filler Area         */

            write #19, eod goto appld_done            

        appld_done
        return                                          /*  (EWD009)  */

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* (APCPLNLD) - FILE          */
L35040: FMT CH(02),                      /* Load Region Code (Primary) */~
            CH(08),                      /* Calculated Production Date */~
            CH(05),                      /* Load Number (0),(A),(S)    */~
            CH(30),                      /* Load Description           */~
            CH(08),                      /* Scheduled Production Date  */~
            CH(08),                      /* Scheduled Completion Date  */~
            CH(08),                      /* Scheduled Load Date        */~
            CH(08),                      /* Planned Production Date    */~
            CH(08),                      /* Planned Completion Date    */~
            CH(08),                      /* Planned Load Date          */~
            CH(02),                      /* Current Status of Load     */~
            CH(08),                      /* Current Status Date        */~
            CH(02),                      /* Scheduled Production Year  */~
            CH(02),                      /* Scheduled Production Week  */~
            CH(01),                      /* Scheduled Production Day   */~
            CH(02),                      /* Sister Code        EWD006  */~
            CH(03),                      /* User ID in Scanning        */~
            CH(04),                      /* Time staging               */~
            CH(03),                      /* Default AWD                */~
            CH(03),                      /* Shipment Block CR1174      */~
            CH(05)                       /* Filler Area        EWD006  */


                                         /* (APCPLNOR) - FILE          */
L35220: FMT CH(170)                      /* Sales Order Record   - #2  */

                                         /* (APCPLNSC) - FILE          */
L35250: FMT CH(128)                      /* Schedule Record      - #4  */

                                         /* (INVMASTR) - FILE          */
L35280:     FMT POS(46),                 /* Skip                       */~
                CH(32),                  /* Part Description           */~
                POS(586),                /* Skip to UPC Code           */~
                CH(20)                   /* UPC Bar Code               */

L30550: FMT XX(1), CH(6), CH(4)          /* (EWDAPPLD) - File  (EWD009)*/

L30560: FMT                              /* (EWDAPPLD) - File  (EWD009)*/~
            CH(1),                       /* Appian Upload Flag         */~
            CH(6),                       /* Appian Earliest Upload Date*/~
            CH(4),                       /* Earliest Dispatch Time     */~
            CH(5),                       /* Load Number                */~
            CH(6),                       /* Appian Load Start Date     */~
            CH(4),                       /* Appian Load Start Time     */~
            CH(5),                       /* Load Number                */~
            CH(6),                       /* Appian Load End Date       */~
            CH(4),                       /* Appian Load End Time       */~
            CH(5),                       /* Appian Truck Number        */~
            CH(6),                       /* Appian Upload Date         */~
            CH(30),                      /* Appian Load Description    */~
            CH(1),                       /* Appian Type                */~
            CH(45)                       /* Filler                     */


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L40070:       gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40230,         /* Load Number       */   ~
                                L40220,         /* Load Description  */   ~
                                L40240,         /* Production Week   */   ~
                                L40240,         /* Production Day    */   ~
                                L40230,         /* Shipment Block    */   ~
                                L40230,         /* Load Region       */   ~
                                L40230,         /* Production Dates  */   ~
                                L40230,         /* Completion Dates  */   ~
                                L40230,         /* Load Dates        */   ~
                                L40230,         /* Appian Dte  EWD009*/   ~
                                L40230          /* Appian Tme  EWD009*/   

              goto L40260

L40220:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40230:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40240:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40260:     accept                                                       ~
               at (01,16), fac(hex(84)), scr$(1%)               , ch(50),~
               at (02,16), fac(hex(84)), scr$(2%)               , ch(50),~
               at (03,16), fac(hex(84)), scr$(3%)               , ch(50),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Load Number     :",                          ~
               at (05,20), fac(lfac$(1%)), ld_load$             , ch(05),~
               at (05,30), fac(hex(84)), sc$(assign% + 1%)      , ch(30),~
/*EWD006*/     at (05,65), fac(lfac$(1%)), ld_sister$           , ch(02),~
/*EWD006*/     at (05,68), fac(hex(84)), ld_sis_desc$           , ch(10),~
                                                                         ~
               at (06,02), "Load Description:",                          ~
               at (06,20), fac(lfac$(2%)), ld_desc$             , ch(30),~
                                                                         ~
               at (07,02), "Production Week :",                          ~
               at (07,20), fac(lfac$(3%)), ld_wk$               , ch(02),~
               at (07,30), fac(hex(84)), sc_wk_dte$             , ch(10),       /* (Y2K, LDJ) */~
               at (07,42), fac(lfac$(3%)), ld_year$             , ch(04),       /* (Y2K, LDJ) */~
                                                                         ~
               at (08,02), "Production Day  :",                          ~
               at (08,20), fac(lfac$(4%)), ld_day$              , ch(01),~
               at (08,30), fac(hex(84)), ld_wk_dte$             , ch(10),       /* (Y2K, LDJ) */~
                                                                         ~
               at (09,02), "Shipment Block  :",                                 /* CR1174 */ ~
               at (09,20), fac(lfac$(5%)), ld_shp_blk$          , ch(03),~
                                                                         ~                                                                        
               at (10,02), "Load Region Code:",                          ~
               at (10,20), fac(lfac$(6%)), ld_region$           , ch(02),~
               at (10,30), fac(hex(84)), ld_region_d$           , ch(30),~
                                                                         ~
               at (11,02), "Production Date (Scheduled):",               ~
               at (11,35), fac(lfac$(7%)), ld_dts1$             , ch(10),       /* (Y2K, LDJ) */~
               at (11,50), "(Planned):",                                 ~
               at (11,65), fac(hex(84)),   ld_dtp1$             , ch(10),       /* (Y2K, LDJ) */~
                                                                         ~
               at (12,02), "Completion Date (Scheduled):",               ~
               at (12,35), fac(lfac$(8%)), ld_dts2$             , ch(10),       /* (Y2K, LDJ) */~
               at (12,50), "(Planned):",                                 ~
               at (12,65), fac(hex(84)),   ld_dtp2$             , ch(10),       /* (Y2K, LDJ) */~
                                                                         ~
               at (13,02), "Load Date       (Scheduled):",               ~
               at (13,35), fac(lfac$(9%)), ld_dts3$             , ch(10),       /* (Y2K, LDJ) */~
               at (13,50), "(Planned):",                                 ~
               at (13,65), fac(hex(84)),   ld_dtp3$             , ch(10),       /* (Y2K, LDJ) */~
                                                                         ~
/*EWD009*/     at (14,02), "Earliest Dispatch Date     :",               ~
/*EWD009*/     at (14,35), fac(lfac$(10%)), ld_app_dte$         , ch(10),~
                                                                         ~
/*EWD009*/     at (15,02), "Earliest Dispatch Time     :",               ~
/*EWD009*/     at (15,35), fac(lfac$(11%)), ap_ear_tme$         , ch(04),~
                                                                         ~
               at (17,02), "Load Dollar Val:",                           ~
               at (17,20), fac(hex(84)),   tot_value$           , ch(12),~
               at (17,40), "Load Tot Units :",                           ~
               at (17,60), fac(hex(84)),   tot_units$           , ch(12),~
               at (18,02), "Load Tot Pull  :",                           ~
               at (18,27), fac(hex(84)),   tot_pull$            , ch(05),~
               at (18,40), "Load Tot Make  :",                           ~
               at (18,67), fac(hex(84)),   tot_make$            , ch(05),~
               at (19,02), "Load Tot Product",                           ~
               at (19,27), fac(hex(84)),   tot_product$         , ch(05),~
               at (19,40), "Load Tot Drops :",                           ~
               at (19,69), fac(hex(84)),   tot_drops$           , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40970
                  call "PRNTSCRN"
                  goto L40070

L40970:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41160     /*  Input Mode             */
            pf$(1) = "(1)Start Over          (14)Fix Data     " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffff0affffff0e0f1000)
            if fieldnr% = 1% then L41120
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L41120:     if fieldnr% > 1% then L41131
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L41131:     if userid$ = "RHH" or  userid$ = "CEP" then return
                str(pf$(1%),23%,16%) = " " : str(pfkeys$,14%,1%) = hex(ff)
            return

L41160: if fieldnr% > 0% then L41320  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over    (11)Schedule Stock     " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Load   "
            pfkeys$ = hex(01ffffffffffffffffff0bffffff0f1000)
            if str(ld_load$,1%,1%) = "S" then goto L41260
                str(pf$(1%),18,20) = " " : str(pfkeys$,11%,1%) = hex(ff)
L41260:     if rec% = 1% then goto L41290
                str(pf$(2%),18,20) = " " : str(pfkeys$,12%,1%) = hex(ff)
                return
L41290:     if ld_status$ < "04" then return
               str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
            return
L41320:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Input and Edit Screen for Stock                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42160,         /* MFG Part Number   */   ~
                                L42170          /* MFG Quantity      */
              goto L42190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L42170:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Load Number     :",                          ~
               at (05,20), fac(hex(84)),   ld_load$             , ch(05),~
               at (05,30), fac(hex(84)), sc$(assign% + 1%)      , ch(30),~
                                                                         ~
               at (06,02), "Load Description:",                          ~
               at (06,20), fac(hex(84)),   ld_desc$             , ch(30),~
                                                                         ~
               at (07,02), "Production Week :",                          ~
               at (07,20), fac(hex(84)),   ld_wk$               , ch(02),~
               at (07,30), fac(hex(84)), sc_wk_dte$             , ch(10),       /* (Y2K, LDJ) */~
               at (07,42), fac(hex(84)),   ld_year$             , ch(04),       /* (Y2K, LDJ) */~
                                                                         ~
               at (08,02), "Production Day  :",                          ~
               at (08,20), fac(hex(84)),   ld_day$              , ch(01),~
               at (08,30), fac(hex(84)), ld_wk_dte$             , ch(10),       /* (Y2K, LDJ) */~
                                                                         ~
               at (09,02), "Shipment Block  :",                                 /* CR1174 */ ~
               at (09,20), fac(hex(84)),    ld_shp_blk$         , ch(03),~
                                                                         ~                             
               at (10,02), "Load Region Code:",                          ~
               at (10,20), fac(hex(84)),   ld_region$           , ch(02),~
               at (10,30), fac(hex(84)), ld_region_d$           , ch(30),~
                                                                         ~
               at (11,02), "Production Date (Scheduled):",               ~
               at (11,35), fac(hex(84)),   ld_dts1$             , ch(10),       /* (Y2K, LDJ) */~
               at (11,50), "(Planned):",                                 ~
               at (11,65), fac(hex(84)),   ld_dtp1$             , ch(10),       /* (Y2K, LDJ) */~
                                                                         ~
               at (12,02), "Completion Date (Scheduled):",               ~
               at (12,35), fac(hex(84)),   ld_dts2$             , ch(10),       /* (Y2K, LDJ) */~
               at (12,50), "(Planned):",                                 ~
               at (12,65), fac(hex(84)),   ld_dtp2$             , ch(10),       /* (Y2K, LDJ) */~
                                                                         ~
               at (13,02), "Load Date       (Scheduled):",               ~
               at (13,35), fac(hex(84)),   ld_dts3$             , ch(10),       /* (Y2K, LDJ) */~
               at (13,50), "(Planned):",                                 ~
               at (13,65), fac(hex(84)),   ld_dtp3$             , ch(10),       /* (Y2K, LDJ) */~
                                                                         ~
               at (15,02), "Stock Part No.  :",                          ~
/*PAR000*/     at (15,20), fac(lfac$(1%)), str(stk_part$, 1%,25%), ch(25),~
/*PAR000*/     at (15,47), fac(lfac$(1%)), str(stk_part$,26%,20%), ch(20),~
/*PAR000*/     at (16,27), fac(hex(84)), stk_desc$              , ch(30),~
                                                                         ~
               at (16,02), "Stock Quantity  :",                          ~
               at (16,20), fac(lfac$(2%)), stk_qty$             , ch(04),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L42790
                  call "PRNTSCRN"
                  goto L42190

L42790:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L43000     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                      (14)Process Stock"
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L42940
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L42940:     if fieldnr% > 1% then L42960
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L42960:     if stk_max% <> 0% then return
                str(pf$(1%),64%) = " " : str(pfkeys$,14%,1%) = hex(ff)
            return

L43000: if fieldnr% > 0% then L43100  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over       (10)Update Stock    " &       ~
                      "                      (14)Process Stock"
            pf$(2%) = "                                        " &       ~
                      "                      (15)Print Screen "
            pf$(3%) = "                                        " &       ~
                      "                      (16)Exit Program "
            pfkeys$ = hex(01ffffffffffffffff0a0b0c0d0e0f1000)

            return
L43100:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *     Planning Fix Screen                                   *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
L44050:       gosub'070(1%, fieldnr%)
              gosub set_pf3
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L44150,         /* Sales Order       */   ~
                                L44140          /* Production Date   */

              goto L44180

L44140:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L44150:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L44180:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Sales Order         :",                      ~
               at (05,25), fac(lfac$(1%)), fix_so$              , ch(08),~
                                                                         ~
               at (06,02), "New Production Date :",                      ~
               at (06,25), fac(lfac$(2%)), fix_dte$             , ch(10),       /* (Y2K, LDJ) */~
                                                                         ~
               at (07,02), "Current Load Number :",                      ~
               at (07,25), fac(hex(84)),   fix_load$            , ch(05),~
                                                                         ~
               at (08,02), "Old Production Date :",                      ~
               at (08,25), fac(hex(84)),   fix_dte1$            , ch(10),       /* (Y2K, LDJ) */~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L44480
                  call "PRNTSCRN"
                  goto L44050

L44480:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
        if edit% = 2% then L44670     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffff0affffffff0f1000)
            if fieldnr% = 1% then L44630
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L44630:     if fieldnr% > 1% then L44650
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L44650:     return

L44670: if fieldnr% > 0% then L44760  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Save Change "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L44760:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
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
            on fieldnr% gosub L50180,         /* Scheduled Prod Load No*/ ~
                              L50450,         /* Load Description      */ ~
                              L50480,         /* Production Week       */ ~
                              L50820,         /* Production Day        */ ~
                              L51950,         /* Shipment Block        */ ~
                              L51100,         /* Load Region Code      */ ~
                              L51260,         /* Production Dates      */ ~
                              L51450,         /* Completion Dates      */ ~
                              L51640,         /* Load Dates            */ ~
                              L51800,         /* Appian Load Dte EWD009*/ ~
                              L51900          /* Appian Load Tme EWD009*/ 

            return

L50180: REM Scheduled Production load             LD_LOAD$
            if edit% <> 2% then assign% = 0%
            if len(ld_load$) = 5 then goto L50260
               if len(ld_load$) > 1 then goto L50260
               assign% = 1%
               if str(ld_load$,1%,1%) = "A" then assign% = 3%
               if str(ld_load$,1%,1%) = "S" then assign% = 2%
                                               /*   (EWD010)  @@@    */
               if str(ld_load$,1%,1%) = "P" then assign% = 4%
               if str(ld_load$,1%,1%) = "G" then assign% = 5%
               if str(ld_load$,1%,1%) = "L" then assign% = 6%
               gosub validate_sister           /*  (EWD006)  */
               return
L50260:     convert ld_load$ to ld_load%, data goto L50300

            convert ld_load% to ld_load$, pic(00000)
            goto L50340
L50300:     convert str(ld_load$,2%,4%) to ld_load%, data goto L50400

            convert ld_load% to str(ld_load$,2%,4%), pic(0000)

L50340:     if edit% = 2% then return
               gosub dataload
               if rec% = 0% then goto L50400
               fieldnr% = 11%
               assign% = 0%
               gosub validate_sister           /*  (EWD006)  */
        return
L50400:     errormsg$ = "(Error) - Load Number not on File??"
            gosub error_prompt
            init(" ") ld_load$
        return

L50450: REM Load Description                      LD_DESC$
            return

L50480: REM Scheduled Production Week             LD_WK$
           init(" ") cur_yr$, cur_wk$, cur_dy$, cur_dte$, cur_date$,    ~
                     ent_yr$, ent_wk$, ent_dy$, ent_dte$, ent_date$,    ~
                     sc_wk_dte$
           
           REM ld_year = 2020                                                       /* (Y2K, LDJ) */
           dateme$ = date
           call "EWDMDY" (dateme$,1%,mm%,mm$,dd%,dd$,yy%,yy$)
           ld_year = yy%
           if ld_year$ = " " then Call_AWDPLN0B                                 /* (Y2K, LDJ) */
           call "NUMTEST" (ld_year$,1998,2399,errormsg$,0,ld_year)              /* (Y2K, LDJ) */
           if errormsg$ = " " then Call_AWDPLN0B                                /* (Y2K, LDJ) */
              errormsg$ = "(Error) - Invalid Production Year!"                  /* (Y2K, LDJ) */
              gosub error_prompt                                                /* (Y2K, LDJ) */
              return                                                            /* (Y2K, LDJ) */
              
Call_AWDPLN0B:                                                                  /* (Y2K, LDJ) */
           ld_yr$ = bin(ld_year,2)                                              /* (Y2K, LDJ) */
           if ld_yr$ <> " "  then ent_yr$ = ld_yr$
           if ld_wk$ <> " "  then ent_wk$ = ld_wk$
           if ld_day$ <> " " then ent_dy$ = ld_day$
           call "AWDPLN0B" ( cur_yr$,    /* Current Production Year    */~
                             cur_wk$,    /* Current Production Week    */~
                             cur_dy$,    /* Current Production Day     */~
                             cur_dte$,   /* Current Production Date(6) */~
                             cur_date$,  /* Current Production Date(8) */~
                             ent_yr$,    /* Entry Production Year (IN) */~
                             ent_wk$,    /* Entry Prod Week       (IN) */~
                             ent_dy$,    /* Entry Production Day (OPT) */~
                             ent_dte$,   /* Entry Production Date (6)  */~
                             ent_date$,  /* Entry Production Date *8)  */~
                             prv_yr$,    /* Previous Year              */~
                             #3,         /* Gencodes                   */~
                             pl_e%    )  /* 0% = No, 1% = Found        */

            if pl_e% <> 0% then goto L50770

                                                                                /* (Y2K, LDJ) */
            ld_yr$     = ent_yr$
            convert val(ld_yr$,2) to ld_year$,pic(0000)                         /* (Y2K, LDJ) */
            ld_wk$     = ent_wk$
            sv_day$    = ent_dy$
            sc_wk_dte$ = ent_dte$
            call "DATFMTC" (sc_wk_dte$)                                         /* (Y2K, LDJ) */       
        return
L50770:    errormsg$ = "(Error) - Invalid Production Week (1 thru 52)?"
           gosub error_prompt
           init(" ") sc_wk_dte$, ld_wk$, ld_day$, ld_year$, sv_day$             /* (Y2K, LDJ) */
        return

L50820: REM Production Day                        LD_DAY$
           if ld_day$ <> " " then goto L50860
              ld_day$ = sv_day$

L50860:    convert ld_day$ to x%, data goto L51050

           if x% < 1% or x% > 7% then goto L51050

           ld_wk_dte$ = sc_wk_dte$
           call "DATUFMTC" (ld_wk_dte$)                                         /* (Y2K, LDJ) */
           call "DATE" addr("GJ", str(ld_wk_dte$,,6%), str(jdate1$,,5%), x%)    /* (Y2K, LDJ) */
                                               /* Convert to Julian DTE*/
           call "DATJULCV" (jdate1$)  /* Convert to ASCII String */             /* (Y2K, LDJ) */                                    
           convert str(jdate1$,5%,3%) to j1%, data goto L50950                  /* (Y2K, LDJ) */
L50950:
           convert ld_day$ to jj%, data goto L51050
                                               /* Current Production */
           j1% = j1% + (jj% - 1%)             /* Day Julian Date    */
           convert j1% to str(jdate1$,5%,3%), pic(000)                          /* (Y2K, LDJ) */
           if j1% > 365% then gosub new_year   /*  (EWD007)  */
           call "DATJULCV" (jdate1$)  /* Convert back to Packed Value  */       /* (Y2K, LDJ) */
           
                                               /* Begin with Production*/
                                               /* Date and   */
           call "DATE" addr("JG", str(jdate1$,,5%), str(prd_dte$,,6%), x%)      /* (Y2K, LDJ) */ 
           ld_wk_dte$ = prd_dte$
           call "DATFMTC" (ld_wk_dte$)                                          /* (Y2K, LDJ) */
        return
L51050:    errormsg$ = "(Error) - Invalid Production Day (1 thru 7)?"
           gosub error_prompt
           init(" ") ld_day$, jdate1$, prd_dte$, ld_wk_dte$
        return
        new_year                                     /*   (EWD007) - BEG  */
           convert str(jdate1$,1%,4%) to yy%, data goto L50960

L50960:    yy% = yy% + 1%

           j1% = j1% - 365%

           convert yy% to str(jdate1$,1%,4%), pic(0000)

           convert j1% to str(jdate1$,5%,3%), pic(000)
        return                                       /*   (EWD007)  -  END */

L51100: REM Verify Region Code                    LD_REGION$
           init(" ") readkey$, desc$, ld_region_d$
           if ld_region$ <> " " then goto L51150
              ld_region$ = "01"

L51150:    str(readkey$,1%,9%)   = "PLAN REGN"
           str(readkey$,10%,15%) = ld_region$
           read #3,key = readkey$, using L51180, desc$, eod goto L51210
L51180:       FMT POS(25), CH(30)
           ld_region_d$ = desc$
        return
L51210:    errormsg$ = "(Error) - Invalid Region Code?"
           gosub error_prompt
           init(" ") ld_region$, ld_region_d$
        return

L51260: REM Verify Scheduled/Production Date      LD_DTS1$, LD_DTP1$
           init(" ") x1$, x2$
           if ld_dts1$ <> " " then goto L51310
              ld_dts1$ = ld_wk_dte$
           date% = 0%
L51310:    call "DATEOKC" (ld_dts1$, date%, errormsg$)                      /* (Y2K, LDJ) */
           if date% = 0% then goto L51400
              ld_dtp1$ = ld_dts1$
              x1$ = ld_dts1$
              call "DATUFMTC" (x1$)                                         /* (Y2K, LDJ) */
              x2$ = ld_wk_dte$
              call "DATUFMTC" (x2$)                                         /* (Y2K, LDJ) */
              if str(x1$,1%,6%) < str(x2$,1%,6%) then goto L51400
        return
L51400:    errormsg$ = "(Error) - Invalid Production Date for Week?"
           gosub error_prompt
           init(" ") ld_dts1$, ld_dtp1$
        return

L51450: REM Verify Scheduled/Completion Date      LD_DTS2$, LD_DTP2$
           init(" ") x1$, x2$
           if ld_dts2$ <> " " then goto L51490
              ld_dts2$ = ld_wk_dte$
L51490:    date% = 0%
           call "DATEOKC" (ld_dts2$, date%, errormsg$)                      /* (Y2K, LDJ) */
           if date% = 0% then goto L51590
              ld_dtp2$ = ld_dts2$
              x1$ = ld_dts1$
              call "DATUFMTC" (x1$)                                         /* (Y2K, LDJ) */
              x2$ = ld_dts2$
              call "DATUFMTC" (x2$)                                         /* (Y2K, LDJ) */                
              if str(x1$,1%,6%) > str(x2$,1%,6%) then goto L51590
        return
L51590:    errormsg$ = "(Error) - Invalid Completion Date for Week?"
           gosub error_prompt
           init(" ") ld_dts2$, ld_dtp2$
        return

L51640: REM Verify Scheduled/Load Date            LD_DTS3$, LD_DTP3$
           init(" ") x1$, x2$
           if ld_dts3$ <> " " then goto L51680
              ld_dts3$ = " "   /* ld_wk_dte$  CR1876 */
L51680:    date% = 0%
           call "DATEOKC" (ld_dts3$, date%, errormsg$)                      /* (Y2K, LDJ) */
           if date% = 0% then goto L51780
              ld_dtp3$ = ld_dts3$
              x1$ = ld_dts2$
              call "DATUFMTC" (x1$)                                         /* (Y2K, LDJ) */
              x2$ = ld_dts3$
              call "DATUFMTC" (x2$)                                         /* (Y2K, LDJ) */
              if str(x1$,1%,6%) > str(x2$,1%,6%) then goto L51780
        return
L51780:    errormsg$ = "(Error) - Invalid Load Date for Week?"
           gosub error_prompt
           init(" ") ld_dts3$, ld_dtp3$
        return

L51800: REM Verify Appian Load Date               LD_APP_DTE$    /*  (EWD009)  */
           init(" ") x1$, x2$
           if ld_app_dte$ <> " " then goto L51820
              ld_app_dte$ = ld_wk_dte$
L51820:    date% = 0%
           call "DATEOKC" (ld_app_dte$, date%, errormsg$)
           if date% = 0% then goto L51890
        return
L51890:    errormsg$ = "(Error) - Invalid Appian Load Date?"
           gosub error_prompt
           init(" ") ld_app_dte$        
        return

L51900: REM Verify Appian Load Time               AP_EAR_TME$    /*  (EWD009)  */
            hh%, mm% = 0%
            convert str(ap_ear_tme$,1%,2%) to hh%, data goto L51990

            convert str(ap_ear_tme$,3%,4%) to mm%, data goto L51990

            if mm% > 60% or mm% < 0% then goto L51990
            if hh% > 24% or hh% < 0% then goto L51990

        return
L51990:    errormsg$ = "(Error) - Invalid Appian Load Time?"
           gosub error_prompt
           init(" ") ap_ear_tme$        
        return

        validate_sister
           if str(ld_load$,1%,1%) <> "S" then ld_sister$ = "00"
           if rec% <> 0% and ld_sister$ = " " then ld_sister$ = "00"
           init(" ") readkey$, desc$
           str(readkey$,1%,9%)   = "PLNSISTER"
           str(readkey$,10%,15%) = ld_sister$
           read #3,key = readkey$, using L51180, desc$, eod goto sis_err

           ld_sis_desc$ = str(desc$,1%,10%)
        return
        sis_err
           errormsg$ = "(ERROR) - Sister Company Code not defined in 'PLNSISTER'."
           gosub error_prompt
           init(" ") ld_sister$, ld_sis_desc$
        return

L51950: REM Shipment Block   SHP_BLK$
        return           

        REM *************************************************************~
            *              E D I T   F O R   S T O C K                  *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L52100,         /* Stock part Number     */ ~
                              L52280          /* Stock Quantity to Make*/
            return

L52100: REM Stock Part Number                     STK_PART$
            init(" ") stk_desc$, upccode$
            if stk_part$ <> " " then goto L52160
               stk_desc$ = hex(06) & "Select a Valid Stock Part"
               call "GETCODE" (#9,stk_part$, stk_desc$, 0%, 1.32, f1%(9))

L52160:     if len(stk_part$) < 19 then goto L52210
            read #9,key = stk_part$, using L35280, stk_desc$, upccode$,   ~
                                                  eod goto L52210
            if len(upccode$) <> 12 then goto L52230
        return
L52210:     errormsg$ = "(Error) - Invalid MFG Stock Part Number?"
            goto L52240
L52230:        errormsg$ = "(Error) - No UPC Code on File?"
L52240:        gosub error_prompt
               init(" ") stk_part$, stk_desc$, upccode$
        return

L52280: REM Stock Part Quantity                   STK_QTY$
           if stk_qty$ <> " " then goto L52310
              stk_qty$ = "1"
L52310:    convert stk_qty$ to stk_qty%, data goto L52360

           if stk_qty% < 1% or stk_qty% > 9999% then goto L52360
              convert stk_qty% to stk_qty$, pic(0000)
        return
L52360:    errormsg$ = "(Error) - Invalid Stock Quantity?"
           gosub error_prompt
           init(" ") stk_qty$
        return

        update_stock
            stk_load$ = ld_load$
            if stk% = 99% then goto L52590
               if len(stk_part$) < 19 then goto L52530
               call "SHOSTAT" ("Updating Stock for ("&stk_load$&")")
               stk% = stk% + 1%
/*PAR000*/     stk$(stk%)     = str(stk_part$, 1%,25%)
/*PAR000*/     stk_sub$(stk%) = str(stk_part$,26%,20%)
/*PAR000*/     stk_desc$(stk%)= stk_desc$
               qty$(stk%) = stk_qty$
               stk_max% = stk%
               convert stk_qty$ to rhh%, data goto L52510
L52510:
               cnt% = cnt% + rhh%
L52530:        init(" ") stk_part$, stk_desc$, upccode$, stk_qty$
               if keyhit% <> 10% then goto L52580
        return clear all
        goto inputmode_a

L52580: return
L52590:     errormsg$ = "(Error) - MAX Stock Line Items (99)?"
            gosub error_prompt
            init(" ") stk_part$, stk_desc$, upccode$, stk_qty$
        return

        REM *************************************************************~
            *              E D I T   F O R   F i x                      *~
            *************************************************************

        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L53100,         /* Customer S.O.         */ ~
                              L53280          /* New Production Date   */
            return

L53100: REM Fix Sales Order                       FIX_SO$
            init(" ") fix_load$, fix_dte1$, fix_dte$, fix_key$, fix_rec$
            if fix_so$ <> " " then goto L53152
               goto L53210

L53152:     str(fix_key$,1%,8%) = fix_so$
            read #7,key > fix_key$, using L53190, fix_rec$,               ~
                                                          eod goto L53210
L53190:        FMT CH(256)
            if str(fix_rec$,24%,8%) <> fix_so$ then goto L53210
               fix_load$ = str(fix_rec$,1%,5%)
               fix_dte1$ = str(fix_rec$,47%,6%)
               call "DATFMTC" (fix_dte1$)                                       /* (Y2K, LDJ) */

        return
L53210:     errormsg$ = "(Error) - Invalid Customer Sales Order No.?"
            gosub error_prompt
            init(" ") fix_so$, fix_dte$, fix_load$, fix_dte1$
        return

L53280: REM New Production Date                   FIX_DTE$, FIX_DATE$
           if fix_dte$ <> " " then goto L53310
              goto L53360
L53310:    call "DATEOKC" (fix_dte$, date%, errormsg$)                          /* (Y2K, LDJ) */
           if date% = 0% then goto L53360
           fix_date$ = fix_dte$
           call "DATUFMTC" (fix_date$)                                          /* (Y2K, LDJ) */
        return
L53360:    errormsg$ = "(Error) - Invalid Date Entered?"
           gosub error_prompt
           init(" ") fix_dte$, fix_date$
        return
 
        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        scan_load
          call "SHOSTAT" ("Scanning S.O. for Load")

          init(" ") sc_key1$, tot_value$, tot_units$, tot_pull$,         ~
                    tot_make$, tot_product$, tot_drops$, sav_drop_seq$
          cnt$ = "[ XXXXX ]"
          cnt% = 0% : or_cost = 0.0
          tot_value = 0.0 : tot_units    = 0.0 : tot_pull%  = 0%
          tot_make% = 0%  : tot_product% = 0.0 : tot_drops% = 0%
          str(sc_key1$,1%,5%) = ld_load$
        scan_load_nxt
          read #4,key 1% > sc_key1$, using L60170, sc_key1$,              ~
                                                eod goto scan_load_done
L60170:         FMT POS(7), CH(27)
          cnt% = cnt% + 1%
          if mod(cnt%,100%) <> 0% then goto L60230
             convert cnt% to str(cnt$,3%,5%), pic(#####)
             print at(04,36);hex(84);cnt$;

L60230:   if str(sc_key1$,1%,5%) <> ld_load$ then goto scan_load_done
          if sav_drop_seq$ <> " " then goto L60260
             goto L60270
L60260:   if sav_drop_seq$ = str(sc_key1$,6%,5%) then goto scan_load_nxt
L60270:      sav_drop_seq$ = str(sc_key1$,6%,5%)

             or_so$ = str(sc_key1$,18%,8%)
        REM - Load Analysis
             convert str(sc_key1$,11%,2%) to sc_drop%, data goto L60320
L60320:
             if sc_drop% > tot_drops% then tot_drops% = sc_drop%
             read #2,key 4% = or_so$, using L60370, or_units, or_value,   ~
                                   or_cost, or_mak%, or_pull%,           ~
                                   eod goto scan_load_nxt
L60370:        FMT POS(99),3*PD(14,4),2*BI(2)
                                                      /* Screen Totals */
             tot_value      = round(tot_value + or_value, 2)
             tot_units      = round(tot_units + or_units, 2)
             tot_pull%      = tot_pull% + or_pull%
             tot_make%      = tot_make% + or_mak%
             tot_product%   = tot_product% + (or_pull% + or_mak%)
             goto scan_load_nxt
        scan_load_done
           convert tot_value to tot_value$, pic(#########.##)
           convert tot_units to tot_units$, pic(#########.##)
           convert tot_pull% to tot_pull$,  pic(#####)
           convert tot_make% to tot_make$,  pic(#####)
           convert tot_product% to tot_product$, pic(#####)
           convert tot_drops% to tot_drops$, pic(###)
        return

        assign_load                /* L1$ = Regular load           0%  */
           ld_load$ = " "          /* L2$ = Forecast Load (Alpha)  2%  */
           store$ = "000"          /* L3$ = Stock Load             1%  */
           err% = 1%
           read #5,hold,key = store$, using L60600, l1$, l3$, l2$,        ~
                                                        eod goto L60960
L60600:       FMT POS(154), CH(8), POS(194), CH(7), CH(8)
/* add G & L loads (5 & 6) @@@ */
           on assign% goto L60630, L60720, L60810, L60900, L60900, L60900 

L60630:       err% = 2%                 /* Assign Standard Load Number */
              convert l1$ to l1%, data goto L60960

              l1% = l1% + 1
              convert l1% to l1$, pic(00000000)          /* SIZE = 8 */
              if l1$ = "00099999" then l1$ = "00000001"
              ld_load$ = str(l1$,4%,5%)
              goto L60910

L60720:       err% = 3%                    /* ASSIGN Stock Load Number */
              convert str(l3$,2%,6%) to l3%, data goto L60960

              l3% = l3% + 1%
              str(l3$,1%,1%) = "S"
              convert l3% to str(l3$,2%,6%), pic(000000)
              ld_load$ = "S" & str(l3$,4%,4%)
              goto L60910

L60810:       err% = 4%                    /* Assign Alpha Load Number */
              convert str(l2$,2%,7%) to l2%, data goto L60960

              l2% = l2% + 1%
              convert l2% to str(l2$,2%,7%), pic(0000000)

              str(l2$,1%,1%) = "A"
              if l2$ = "A0009999" then l2$ = "A0000001"
              ld_load$ = "A" & str(l2$,5%,4%)

L60910:       put #5, using L60600, l1$, l3$, l2$
              rewrite #5
         err% = 0%
         gosub load_prompt
        return
L60960:    if err% = 1% then                                             ~
              errormsg$ = "(Error) - Unable TO Read Store(000)?"
           if err% = 2% then                                             ~
              errormsg$ = "(Error) - Unable Assign Regular Load Number?"
           if err% = 3% then                                             ~
              errormsg$ = "(Error) - Unable Assign Stock Load Number?"
           if err% = 4% then                                             ~
              errormsg$ = "(Error) - Unable Assign Alpha Load Number?"
           if err% = 5% then                                             ~
              errormsg$ = "(Error) - Unable to Read 'P' Load Store(000)?"
           if err% = 6% then                                             ~
              errormsg$ = "(Error) - Unable to Read 'G' Load Store(000)?"
           if err% = 7% then                                             ~
              errormsg$ = "(Error) - Unable to Read 'L' Load Store(000)?"
           if err% = 8% then                                             ~
              errormsg$ = "(Error) - Unable Assign Production Load Number?"
        ld_load$ = " "
           gosub error_prompt
        return
                                                            /*  (EWD010) -- BEG  */
L60900:     err% = 7%
            store_key$ = "DEFAULTS.STORE.000"
            read #20,hold,key = store_key$, using L60100, l4$, l6$, l7$, ~
                                                  eod goto L60960
L60100:  FMT POS(322), CH(5), POS(362), CH(5), POS(382), CH(5)
              
/* CR1979 add G & L loads */
	      err% = 8%
	      tx$ = "P"
	      lx$ = l4$

              if assign% <> 5% then goto not_g
	      tx$ = "G"
	      lx$ = l6$

not_g
              if assign% <> 6% then goto not_l
	      tx$ = "L"
	      lx$ = l7$

not_l

              convert str(lx$,2%,7%) to lx%, data goto L60960 

              lx% = lx% + 1%
              convert lx% to str(lx$,2%,7%), pic(0000)

/*            str(l4$,1%,1%) = "P" */
              str(lx$,1%,1%) = tx$                     
              if lx$ = "P9999" then lx$ = "P0001"
              if lx$ = "G9999" then lx$ = "G0001"
              if lx$ = "L9999" then lx$ = "L0001"
              ld_load$ = tx$ & str(lx$,2%,4%)

              if tx$ = "P" then l4$ = lx$ 
              if tx$ = "G" then l6$ = lx$ 
              if tx$ = "L" then l7$ = lx$ 

              put #20, using L60100, l4$, l6$, l7$
              rewrite #20
         err% = 0%
         gosub load_prompt
        return
                                                            /*  (EWD010) -- END  */

        delete_detail
            call "SHOSTAT" ("Re-Setting S.O. Information")

            init(" ") sav_so$, sc_key1$
            str(sc_key1$,1%,5%) = ld_load$
        delete_detail_nxt                        /* (APCPLNSC) - File  */
            read #4,hold,key 1% > sc_key1$, using L35250, sc_rec$,        ~
                                            eod goto delete_detail_done
           if str(sc_rec$,7%,5%) <> ld_load$ then goto delete_detail_done
               sc_key1$ = str(sc_rec$,7%,27%)    /* Can't be Complete  */
               if str(sc_rec$,110%,2%) > "10" and                        ~
                  str(ld_load$,1%,1%) <> "S" then goto delete_detail_nxt
               delete #4
                                                 /* Delete Stock Data  */
            if str(ld_load$,1%,1%) = "S" then goto L61330

            str(sc_rec$,7%,5%)   = "00000"       /* Reset Load Number  */
            str(sc_rec$,12%,5%)  = "00000"       /* Reset Drop Seq.    */
            str(sc_rec$,17%,2%)  = "00"          /* Reset Drop No.     */
            str(sc_rec$,105%,5%) = "00000"       /* Reset Parent Load  */
            str(sc_rec$,110%,2%) = "01"          /* Reste Status Code  */
            str(sc_rec$,112%,6%) = date          /* Set Status Date    */
            put #4, using L35250, sc_rec$
            write #4, eod goto L61450

L61330:     if sav_so$ <> " " then goto L61360
               goto L61370

L61360:     if sav_so$ = str(sc_rec$,24%,8%) then goto delete_detail_nxt
L61370:        sav_so$ = str(sc_rec$,24%,8%)
               gosub delete_header

            goto delete_detail_nxt
        delete_detail_done
            gosub delete_dtl

        return
L61450:     errormsg$="(Error)-Deleting (SC) Line Item Detail-"&sc_key1$
            gosub error_prompt
            goto delete_detail_nxt

        delete_header                             /* (APCPLNOR) - File */
            read #2,hold,key 4% = sav_so$, using L35220, or_rec$,         ~
                                             eod goto delete_header_done
               delete #2                          /* Delete Stock Data */
            if str(sav_so$,1%,1%) = "S" then goto delete_header_done

            str(or_rec$,25%,2%) = "00"            /* Reset Drop Number */
            str(or_rec$,60%,2%) = "01"            /* Reset Status Code */
            str(or_rec$,62%,8%) = date            /* Set Status Date   */
            str(or_rec$,94%,5%) = "00000"         /* Reset Load Number */

            put #2, using L35220, or_rec$
               write #2, eod goto delete_header_done
        delete_header_done
        return

        delete_dtl                                 /*(APCPLNDT) -File  */
            call "SHOSTAT" ("Re-Setting Capacity Information")
            init(" ") dt_key$, dt_bar$, dt_dept$, dt_proc$, dt_shft$,    ~
                      dt_yr$, dt_wk$, dt_day$, dt_st$, ef_part$
            str(dt_key$,1%,5%) = ld_load$          /* Set Load Number  */
        delete_dtl_nxt
            read #7,hold,key 3% > dt_key$, using L61750, dt_key$, dt_bar$,~
                                  dt_dept$, dt_proc$, dt_st$, dt_shft$,  ~
                                  dt_upmh, ef_part$, dt_yr$, dt_wk$, dt_day$,~
                                  eod goto delete_dtl_done
L61750:        FMT CH(23), CH(18), CH(3), CH(2), POS(64), CH(2),         ~
                   POS(104), CH(2), POS(181), PD(14,4), CH(25),          ~
                   POS(230), 3*CH(2)
            if ld_load$ <> str(dt_key$,1%,5%) then goto delete_dtl_done
            if dt_st$ > "10" and str(dt_key$,1%,1%) <> "S" then          ~
                                                      goto delete_dtl_nxt
               delete #7
               gosub update_capacity
               gosub delete_audit
               goto delete_dtl_nxt
        delete_dtl_done
        return

        update_capacity                           /* (APCPLNUC) - File */
            if dt_dept$ = "095" or dt_dept$ = "100" or dt_dept$ = "102"  ~
                                or dt_dept$ = "104" then return
            init(" ") pl_key$
            str(pl_key$,1%,2%)  = dt_yr$
            str(pl_key$,3%,2%)  = dt_wk$
            str(pl_key$,5%,2%)  = dt_shft$
            str(pl_key$,7%,3%)  = dt_dept$
            str(pl_key$,10%,2%) = dt_proc$         /*   (EWD008)       */
            read #6,hold,key = pl_key$, using L61980, pl_unts(),          ~
                                pl_unta%(), pl_unte%(),                   ~
                                             eod goto update_capacity_done
L61980:        FMT POS(31), 7*PD(14,4), 7*BI(2), 7*BI(2)

            gosub calc_eff                         /*   (EWD008)       */
            dt_day% = 1%
            convert dt_day$ to dt_day%, data goto L62010
L62010:
            pl_unts(dt_day%)  = round(pl_unts(dt_day%) + dt_upmh, 2)
            pl_unta%(dt_day%) = pl_unta%(dt_day%) - 1%
            pl_unte%(dt_day%) = round(pl_unte%(dt_day%) - ef_unit,0)
            if pl_unte%(dt_day%) < 0% then pl_unte%(dt_day%) = 0%
                                                   /*   (EWD008)       */
            put #6, using L61980, pl_unts(), pl_unta%(), pl_unte%()
            rewrite #6
        update_capacity_done
        return

        delete_audit                              /* (APCPLNAD) - File */
            init(" ") ad_key$
            str(ad_key$,1%,18%) = dt_bar$
        delete_audit_nxt
            read #8,hold,key 1% > ad_key$, using L62150, ad_key$,         ~
                                                        eod goto L62190
L62150:        FMT CH(33)
            if str(ad_key$,1%,18%) <> dt_bar$ then return
               delete #8
               goto delete_audit_nxt
L62190: return

        load_prompt
           comp% = 2%
           hdr$ = "*****"& sc$(assign% + 1%) &"*****"
           msg$(1%) = "                                           "
           msg$(2%) = "** Load Number Assigned (" & ld_load$ & ") **"
           goto L62320
        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
L62320:    msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        assign_order               /* Get the next Stock Sales Order   */
           store$ = "000"          /* from Store'000'.                 */
           read #5,hold,key = store$, using L62390, sto$, eod goto L62490
L62390:       FMT POS(178), CH(8)
           stk_so$ = sto$
           convert str(sto$,2%,7%) to sto%, data goto L62490

           sto% = sto% + 1%
           str(sto$,1%,1%) = "S"
           convert sto% to str(sto$,2%,7%), pic(0000000)
           put #5,using L62390,sto$
           rewrite #5
        return
L62490:    errormsg$ = "(Error) - Invalid Stock Sales Order"
           gosub error_prompt
           init(" ") stk_so$
        return

        build_stock
                                         /* (EWD004) - Not change for  */ 
           init(" ") stk_time$, or_po$   /*            Stock Orders    */
           call "TIME" (stk_time$)
           or_no$(1%) = "999999999"               /* CUSTOMER CODE     */
           or_no$(2%) = stk_so$                   /* Sales Order Number*/
           or_no$(3%) = "999999999"               /* S.O. Invoice No.  */
           or_no$(4%) = "999999999"               /* S.O. Check No.    */
           dateme$ = date
           call "DATFMTC" (dateme$, dateme%, dateme2$)
           dateme% = dateme%
           str(or_po$,1%,8%)  = dateme2$          /* Use Current Date  */
           str(or_po$,9%,8%)  = stk_time$         /* Use Current Time  */

           err% = 0%                     /* OPT% = 1% Add              */
        call "APCPLN5B"  (  1%,          /* Option Code                */~
                          stk_dte$,      /* Stock Scheduled Load Date  */~
                          or_no$(),      /* Customer Number            */~
                                         /* Sales Order Number         */~
                                         /* APC Inv. for S.O.          */~
                                         /* APC Chk Assoc. with S.O.   */~
                          or_po$,        /* Customer P.O. Number       */~
                          "02",          /* Curr S.O. Status(PLAN STAT)*/~
                          date,          /* Date Assoc. with Stat Chg. */~
                          "0000",        /* APC Salesman Code          */~
                          "01",          /* Customer Delivery(PLAN DEL?*/~
                          "00",          /* Special Instr (PLAN HOWS)  */~
                          stk_load$,     /* Load Number Sched/Assigned */~
                          date,          /* Date S.O. Created          */~
                          date,          /* Date S.O. Last Changed     */~
                          userid$,       /* S.O. Last Modified By      */~
                          "      ",      /* Date B.O.L. Created        */~
                          "    ",        /* S.O. Header Text Id        */~
                          stk$(),        /* Stock Part Numbers         */~
/*PAR000*/                stk_sub$(),    /* Stock Subpart numbers      */~
/*PAR000*/                stk_desc$(),   /* Stock Description          */~
                          qty$(),        /* Assoc. Stock Quantity      */~
                          #10,           /* CUSTOMER - Master File     */~
                          #2,            /* APCPLNOR - APC Header File */~
                          #4,            /* APCPLNSC - APC Schedule    */~
                          #11,           /* BCKMASTR - S.O. HEADER     */~
                          #12,           /* BCKLINES - S.O. DETAIL     */~
                          #3,            /* GENCODES - TABLES          */~
                          #13,           /* APCPULLS - APC Pull Stock  */~
                          #14,           /* INVQUAN  - Inventory Qty   */~
                          #15,           /* APCPLNDP - Plan Dept Master*/~
                          #16,           /* APCPLNSD - Plan S.O. Sched */~
                          #9,            /* INVMASTR - PART MASTER     */~
                          #17,           /* APCPLNSA Daily Sales-EWD002*/~
                          #18,           /* EWDSCHED Spec Sched(EWD003)*/~
                          #63,           /* BCKSUBPT                   */~
                          err% )         /* 0 = OK, 1 = ERROR          */

        return

        open_error                                    /* (EWD005)      */
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return
                                                      /* (EWD005)      */

        calc_eff                                 /* (EWD008)           */
           ef_unit = 0.00
           tqty% = 1%
           ef_err% = 0%

           call "APCPLNEF" ( ef_part$,   /* Part Number                */~
                             tqty%,      /* Number of planning units IN*/~
                             ef_unit,    /* Number of effective unitOUT*/~
                             ef_err%,    /* Error Code              OUT*/~
                             #3)         /* FILE = (GENCODES)          */

           if ef_err% <> 0% then ef_unit = 0.00

        return                                   /* (EWD008)           */
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end

