        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLN03                             *~
            *  Creation Date     - 05/01/96                             *~
            *  Last Modified Date- 03/11/2009                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Last Modified By  - Christie M. Gregory                  *~
            *                                                           *~
            *  Description       - New Build Loads Utility Program.     *~
            *                      Utilizing Region, Route, and Zip     *~
            *                      Code to Assign Sales Orders to a     *~
            *                      'Defined Load'. Two report formats,  *~
            *                      (1) Detail Analysis by S.O.          *~
            *                      (2) Summary Analysis for each Dept.  *~
            *                          for S.O.s or a Load.             *~
            *                                                           *~
            *  Code Tables Used  - (PLAN REGN), (PLAN RTE ), (PLAN SUPP)*~
            *                                                           *~
            *  Subroutine Used   - APCPLN1B, APCPLN7B                   *~
            *                                                           *~
            *  Spec. Comm (Screen 1) - PF(8) Includes all S.O.'s that   *~
            *                                are not assigned, Unless   *~
            *                                to the Specified Load.     *~
            *                          PF(9) Includes Only those S.O's  *~
            *                                assigned to Specified Load.*~
            *                         PF(10) Edit Change Load Data and  *~
            *                                Information.               *~
            *                         PF(14) Print Detail Report from   *~
            *                                Selection Criteria.       .*~
            *                         PF(15) Print Summary Report from  *~
            *                                Selection Criteria.        *~
            *             (Screen 2) - PF(8) Display Product Analysis   *~
            *                                by Departments Based on    *~
            *                                Selection Criteria Either  *~
            *                                All S.O's or Load Data.    *~
            *                          PF(9) Include - Skip the Blanks  *~
            *                                and Process the'Non-Blanks'*~
            *                                into the Load.             *~
            *                         PF(10) Exclude - Skip the 'Non-   *~
            *                                Blanks' and Process all the*~
            *                                Blanks into the Load.      *~
            *                         PF(13) Print Summary Report from  *~
            *                                Selection Criteria.       .*~
            *                         PF(14) Print Detail Report from   *~
            *                                Selection Criteria.        *~
            *             (Screen 2)--PF(7)  Re-Sequence Load by Route  *~
            *              'Loads'           Code and Assign New Drop   *~
            *                                Numbers to Customers.      *~
            *                         PF(8)  Display Product Analysis   *~
            *                                by Departments Based on    *~
            *                                Selection Criteria Either  *~
            *                                All S.O's or Load Data.    *~
            *                         PF(11) Re-Sequence Load and Assign*~
            *                                New Drop Numbers to        *~
            *                                Customers. Clean-Up Seq/s  *~
            *                         PF(12) Delete Sales Orders From   *~
            *                                Load. Skip all 'Blanks' and*~
            *                                Delete all Sales Orders    *~
            *                                from Load with 'Non-Blank' *~
            *                                Selection.                 *~
            *                         PF(13) Print Summary Report from  *~
            *                                Selection Criteria.       .*~
            *                         PF(14) Print Detail Report from   *~
            *                                Selection Criteria.        *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/09/96 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 10/24/96 ! Mod to Assign Load Number to Stock Pulls ! RHH *~
            *          !   New Sub UPDATE_PULL_LOAD. PT_LOAD$ set !     *~
            *          !   to 'CHold' When Status = '99'          !     *~
            * 11/13/97 ! Mod for Upgrade to new Release R6.04.03  ! RHH *~
            * 03/09/98 ! Y2K Changes                              ! LDJ *~
            * 09/22/98 ! (EWD001) Mod for New Status Codes 90-98  ! RHH *~
            *          !   90% = Tempered Glass Freeze cannot Plan!     *~
            *          !   91% = Special liting Freeze cannot Plan!     *~
            * 08/18/99 ! (EWD002) - Mod to replace 'OPENCHCK'     ! RHH *~
            *          !            with new 'EWDOPEN' Subroutine !     *~
            *          !            to improve HP speed.          !     *~
            * 05/24/01 ! (EWD003) - Mod to add delete off load or ! CMG *~
            *          !            delete entire order.          !     *~
            * 07/13/01 ! (EWD004) - Mod for Special Shapes Stock  ! RHH *~
            *          !            pulls. New Shape Stock Dept   !     *~
            *          !            '104'                         !     *~
            * 11/22/02 ! (EWD005) - Mods for Effective Units on   ! CMG *~
            *          !              display screen.             !     *~
            * 06/16/03 ! (EWD006) - Mods for Appian Loads         ! CMG *~
            * 10/27/03 ! (EWD007) - Mods for Oracle               ! CMG *~
            * 03/15/04 ! (EWD008) - Mods for New Surge System     ! CMG *~
            * 04/05/05 ! (AWD009) - Mod to APCPLNSD               ! CMG *~
            * 05/30/06 ! (AWD010) - Mod not to allow delete order ! CMG *~
              *          !   on status >= 18 BOL cut                !     *~
            * 05/30/06 ! (AWD011) - Mod not to allow over 99 drops! CMG *~
            * 05/30/06 ! (AWD012) - Mod to log deleted Orders     ! CMG *~
            *03/11/2009! (AWD013) - mod to delete ewdsched        ! CMG *~
            *10/02/2009! (AWD014) - Eliminate ability to can ord  ! DES *~
            *06/03/2011! (AWD015) - add orcl usr & pswd lookup    ! CMG *~
            *11/23/2015! (SR70889) - mod to show X-dock orders    ! CMG *~
            *05/05/2018! CR Proj  - Trigger Atlas on order remove ! RDB *~
            *10/30/2019! CR2317   - Add drop number to display    ! RDB *~
            *************************************************************

        dim                              /* (APCPLNOR) - FILE          */~
            or_due$8,                    /* S.O. Due Date/Delivery Date*/~
            or_region$2, prt_region$2,   /* Customer Region Code       */~
            or_route$5,  prt_route$5,    /* Customer Route Code        */~
            or_zip$9,                    /* Customer Zip Code          */~
            or_drop$2,                   /* Customer Drop Number       */~
            or_cuscode$9,                /* Customer Number            */~
            or_po$16,                    /* Customer P.O. Number       */~
            or_so$8,                     /* Customer S.O. Number       */~
            or_status$2, pt_load$5,      /* Current S.O. Stat PLAN STAT*/~
            or_dte$8,                    /* Date Assoc. with Stat Chg  */~
            or_inv$8,                    /* APC Invoice Assoc. with SO */~
            or_chk$8,                    /* APC Check Assoc. with Invoi*/~
            or_sls$4,                    /* APC Salesman Code          */~
            or_fob$2, how_ship$11,       /* Cust Delivery Cde PLAN DEL?*/~
            or_hows$2, sc_load$5,        /* Spec. Instr (PLAN HOWS)    */~
            or_load$5, or_load_d$30,     /* Load N. Sched./Assigned    */~
            sav_load$5, prt_load$5,      /* Total Loading Units S.O.   */~
                                         /* Total Price S.O. (Net)     */~
                                         /* Total Cost S.O.            */~
                                         /* Total Make Quantity S.O.   */~
                                         /* Total Pull Quantity S.O.   */~
            or_date$8,                   /* Date S.O. Created          */~
            or_chg$8,                    /* Date S.O. Last Modified    */~
            or_userid$3,                 /* S.O. Last Modified By User */~
            or_bol$8,                    /* Date B.O.L. Created        */~
            or_text$4,                   /* S.O. Header Text Id        */~
                                         /* Actual No. Delivery Days   */~
            or_special$10, or_rec$170,   /* Special Product Flags      */~
            or_fil$3, or_key$51,         /* Filler Area                */~
            sc_key$10, sc_so$8,          /* S. O. Line Items Primary Ky*/~
            sc_key1$27, sc_rec$128,      /* S.O. Alt 1 Load Key        */~
            sc_drop_seq$5, sc_line$2,    /* S.O. Line Item Drop Seq No */~
            sc_part$25,                  /* MFG Part Number            */~
            sav_drop_seq$5,              /* Save Drop Seq for Scan Load*/~
            sc_special$10, sd_key$23     /* Schedule Detail            */

        dim                              /* (Delete) Detail Variables  */~
            dt_key$23, dt_dept$3,        /* Primary Key and Department */~
            dt_proc$2, dt_shft$2,        /* Detail Process and Shift   */~
            dt_yr$2, dt_wk$2, dt_day$2,  /* Detail Year, Week, Day     */~
            pl_unts(7%), dt_seq$5,       /* Capacity Key and Capacity  */~
            pl_unta%(7%), ad_key$33,     /* Planned Units and Audit Key*/~
            pl_unte%(7%),                /* Effective Units    (EWD005)*/~
            dt_bar$18, dt_st$2,          /* Barcode                    */~
            dt_so$8,  dt_line$2,         /* Sales order, Line from BARCODE */~
            hows$100                     /* Special Howships - No Upmh */

        dim                              /* (Program) - Variables      */~
            filename$8,                  /* Used By EWDOPEN    (EWD002)*/~
            blankdate$6,                 /* Null Date test                   (Y2K, LDJ) */~
            pull_key$10, pull_rec$64,    /* Stock Pull Rec and Prim Key*/~
            bg_due$10, bg_dte$10,        /* Beg/End S.O. Due Date            (Y2K, LDJ) */~
            ed_due$10, ed_dte$10,        /*                                  (Y2K, LDJ) */~
            bg_region$2, ed_region$2,    /* Beg/End Region Codes       */~
            bg_route$5, ed_route$5,      /* Beg/End Route Codes        */~
            bg_zip$9, ed_zip$9,          /* Beg/End Zip Codes          */~
            x$8, sav_due$8, scr$35,      /* Format Due Date            */~
            cnt$28, page$16, line1$65,   /* Screen Display             */~
            mode$5, wrk$64,              /* Open Work Routine          */~
            hdr$45, msg$(3%)79,          /* Askuser - Var's            */~
            title$40, date$8,            /* REPORT TITLE               */~
            runtime$8,                   /* REPORT RUN TIME            */~
            readkey$50, desc$30,         /* Generic Key                */~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim                              /* (SR70889)                   */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4,            /* Additional Info Fields     */~
            subpart$20,                  /* Subpart Number             */~
            infopart$20,                 /* Info Part Number           */~
            mfgplant$1,                  /* (SR70889) Mfg Plant        */~
            invplant$1                   /* (SR70889) Inv Plant        */


        dim f2%(25%),                    /* = 0 if the file is open    */~
            f1%(25%),                    /* = 1 if READ was successful */~
            fs%(25%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(25%)20                 /* Text from file opening     */

        dim tot_units$12,                /* Total Units FOR Criteria   */~
            tot_make$12,                 /* Total Makes for Criteria   */~
            tot_pull$12,                 /* Total Pulls for Criteria   */~
            tot_product$12,              /* Total Product(Units)       */~
            tot_prod_value$12,           /* Total S.O. Net Value       */~
            pg_tot_unit$6,               /* Total Page Units           */~
            pg_tot_value$10,             /* Total Page Net Value       */~
            h1$3, h2$5, h3$5, h4$9,      /* Screen Headers             */~
            h5$25, h6$8, h7$6, h8$10,    /* Screen Headers             */~
            h9$1,                        /* Cross Dock (SR70889)       */~
            h10$2,                       /* Drop number CR2317         */~
            c1$(5000%)1,                 /* Include/Exclude            */~
            c2$(5000%)5,                 /* Sequence Number            */~
            c3$(5000%)5,                 /* Route Codes                */~
            c4$(5000%)9,                 /* Customer Code              */~
            c5$(5000%)25,                /* Customer Name              */~
            c6$(5000%)8,                 /* Sales Order Number         */~
            c7$(5000%)6,                 /* Loading Units              */~
            c8$(5000%)10,                /* S.O. Net Value             */~
            c9$(5000%)10,                /* Cross-Dock (SR70889)       */~
            c10$(5000%)2,                /* Drop number CR2317         */~
            cc$(48%)3, dd$(48)30,        /* Analysis Screen  ('103)    */~
            nn%(48%,5%), nt%(5%),        /* Analysis Screen  ('103)    */~
            tt(48%),                     /* Sales Totals by Department */~
            sp$(48%)3,                   /* Save Support Departments   */~
            txt$(192%)37, fld4$4,        /* Analysis Screen            */~
            sav_break$4, break$4,        /* Report Break - Region/Rte  */~
            fld1$20, fld2$6, fld3$4      /*                            */

        dim pp$(48%,192%)3, pp$1,        /* Save Products for Each Dept*/~
            pm%(48%), np%(48%,192%),     /* Store Count, 192 = Max Prod*/~
            sav_model$3, pl_key$12,      /*                            */~
            sav_key$7                    /* Load Products              */

        dim mst_key$25,                  /* BCKMASTR Delete Key        */~
            lne_key$19,                  /* BCKLINES Delete Key        */~
            bck_so$16                    /* BCKMASTR Sales Order Number*/

        dim                              /*      (EWD005)              */~
            ef_part$25,                  /*  Part Number from 'SC'     */~
            ap_type$1                    /*  Appian Load Type  (EWD006)*/

        dim                              /*  (EWD007)                  */~
            bar_key$18,                  /* New Barcode File Readkey   */~
            server$25,                   /* Connection String          */~
            user$25,                     /* User Name to Connect       */~
            pass$25,                     /* Password to Connect        */~
            dt_rec$256,                  /* sudo apcplndt rec for DB   */~
            stmt1$250,                   /* First Query String         */~
            stmt2$250,                   /* Second Query String        */~
            stmt3$250,                   /* Second Query String        */~
            stmt4$250,                   /* Second Query String        */~
            stmt5$250,                   /* Second Query String        */~
            stmt6$250,                   /* Second Query String        */~
            stmt7$250,                   /* Second Query String        */~
            stmt8$250,                   /* Second Query String        */~
            error$256,                   /* Error String               */~
            field$256,                   /* Query Return Field         */~
            name$50                      /* Field Name Returned        */


        dim awdplndl_rec$128,            /* (AWD012) AWDPLNDL record   */~
            awdplndl_key$23,             /* (AWD012) AWDPLNDL Readkey  */~
            time$6,                      /* (AWD012) Time Deleted      */~
            awdplndl_flag$1,             /* (AWD012) cancel Flag       */~
            awdplndl_status$2,           /* (AWD012) save status       */~
            awdplndl_load$5              /* (AWD012) save load         */

        dim ewdsched_key$16,             /* (AWD013) readkey           */~
            ewdsched_sav$16              /* (AWD013) save readkey      */

        dim atlstatus$2,                /* Status code        PR Proj  */~
            pgmname$10,                 /* Program Name       PR Proj  */~
            prev_so_line$10,            /* Prev read on DT             */~
            schema$8                    /* Schema             PR Proj  */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Scheduling Load Analysis Utility  "
            pname$ = "APCPLN03 - Rev: R6.05 10/31/2019"

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
            * #1  ! APCPLNOR ! (NEW) S.O. Header History                *~
            * #2  ! GENCODES ! Master Code Tables File                  *~
            * #3  ! APCPLNSC ! Planning Master Schedule File            *~
            * #4  ! CUSTOMER ! Customer Master Schedule File            *~
            * #5  ! APCPLNLD ! Load Master File                         *~
            * #6  ! APCPLNSD ! Scheduling Department Detail             *~
            * #7  ! APCPLNWK ! Scheduling Work File                     *~
            * #8  ! APCPLNDP ! Planning Master Department File          *~
            * #9  ! APCPLNDT ! Planning Master Detail File              *~
            * #10 ! APCPULLS ! APC PULL FROM INVENTORY MASTER FILE      *~
            * #11 ! APCPLNAD ! Production Scanning Audit File           *~
            * #15 ! APCPLNUC ! Planning Master Unit Capacity            *~
            * #16 ! BCKMASTR ! Backlog master file               EWD003 *~
            * #17 ! BCKLINES ! Back Log Line Item File           EWD003 *~
            * #18 ! AWDPLNLD ! Appian Load Number                       *~
            * #19 ! AWDBARDE ! Warranties to Delete in Oracle    EWD007 *~
            * #20 ! AWDPLNDL ! Log Deleted orders                AWD012 *~
            * #21 ! HLDSCHED ! Special Temp Glass and Liting     AWD013 *~
            * #22 ! SYSFILE2 ! Caelus Management System General Informa *~
/*CRProj*/  * #23 ! PGORLNTR ! PlyGem ATLaS Trigger Remote Order Line Fi*~
            * #63 ! BCKSUBPT ! Sub Part File        (SR70889)           *~
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
/* (AWD009) - Mod to key and reclen */
            select #6,  "APCPLNSD",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =  1,   keylen = 23

            select #7,  "APCPLNWK",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =  1,   keylen = 64

            select #8,  "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =   32,              ~
                        keypos = 11,   keylen = 12,                      ~
                        alt key 1, keypos =  9, keylen = 14,             ~
                            key 2, keypos =  4, keylen = 12,             ~
                            key 3, keypos =  1, keylen = 15

            select #9,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  24,  keylen =  23,                     ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup

            select #10,  "APCPULLS",                                     ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =   10, keylen =   10,                    ~
                        alt key  1, keypos =    1, keylen =  19,         ~
                            key  2, keypos =   20, keylen =  25, dup

            select #11,  "APCPLNAD",                                     ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =   19, keylen =   33,                    ~
                        alt key  1, keypos =    1, keylen =  33

            select #13, "DTHOLDFI",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  24,  keylen =  23
/*
REM                     alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup
*/
            select #15,  "APCPLNUC",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =   11,                    ~
                        alt key  1, keypos =    7, keylen =  11

                                                        /* (EWD003)    */
            select #16, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #17, "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19
                                                        /* (EWD003)    */

                                                        /*  (EWD006)   */
             select #18, "AWDAPPLD"                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   12, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =  16,         ~
                            key  2, keypos =    2, keylen =  15,         ~
                            key  3, keypos =   17, keylen =  15
                                                        /*  (EWD006)   */
                                                        /*  (EWD007)   */

            select #19, "AWDBARDE"                                      ~
                        varc,     indexed, recsize = 18,                ~
                        keypos =    1, keylen = 18
                                                        /*  (EWD007)   */

                                                        /*  (AWD012)   */

            select #20, "AWDPLNDL"                                       ~
                        varc,     indexed, recsize = 128,                ~
                        keypos =    1, keylen = 23,                      ~
                        alt key  1, keypos =   16, keylen =  8, dup

                                                        /*  (AWD012)   */

/* (AWD013) */
            select #21, "HLDSCHED",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  40,                     ~
                        alt key  1, keypos =   16, keylen =  25,         ~
                            key  2, keypos =   27, keylen =  16


            select #22,  "SYSFILE2",                                     ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #23, "PGORLNTR",                    /* PC Proj  */    ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   21, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  64,         ~
                            key  2, keypos =   54, keylen =  11, dup
                        
/* (SR70889) */
            select #63, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup


            call "SHOSTAT" ("Initialization")

                                                        /* (EWD002)    */


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
            filename$ = "APCPLNSD" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "APCPLNDP" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDT" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPULLS" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNAD" : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error
              call "OPENCHCK" (#13, fs%(13%), f2%(13%), 100%, rslt$(13%))
            filename$ = "APCPLNUC" : call "EWDOPEN" (#15, filename$, err%)
            if err% <> 0% then gosub open_error
                                                        /* (EWD002)    */

                                                        /* (EWD003)    */
            filename$ = "BCKMASTR" : call "EWDOPEN" (#16, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#17, filename$, err%)
            if err% <> 0% then gosub open_error
                                                       /* (EWD003)     */


            filename$ = "AWDAPPLD" : call "EWDOPEN" (#18, filename$, err%)
            if err% <> 0% then gosub open_error
                                                       /* (EWD006)     */

REM            call "OPENCHCK" (#19, fs%(19%), f2%(18%), 0%, rslt$(19%))
REM  Note Do I really want to delete??  What if program fails??
REM  For Now I am going to say not to delete but I will leave code if I
REM  need to put it back in.

REM                   if fs%(19%) <> 1% then goto L03080
REM                     call "FILEBGON" (#19)
REM  L03080:
              call "OPENCHCK" (#19, fs%(19%), f2%(19%), 100%, rslt$(19%))

/* (AWD020) */
              call "OPENCHCK" (#20, fs%(20%), f2%(20%), 100%, rslt$(20%))

/*(AWD013) */
            filename$ = "HLDSCHED" : call "EWDOPEN" (#21, filename$, err%)
            if err% <> 0% then gosub open_error

/*(AWD015) */
            filename$ = "SYSFILE2" : call "EWDOPEN" (#22, filename$, err%)
            if err% <> 0% then gosub open_error
/* CR Proj */            
            filename$ = "PGORLNTR" : call "EWDOPEN" (#23, filename$, err%)
            if err% <> 0% then gosub open_error

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
            call "DATUFMTC" (blankdate$)                                        /* (Y2K, LDJ) */
            h1$ = "I/E"
            h2$ = "Seq. "
            h3$ = "Route"
            h4$ = "Customer "
            h5$ = "<--- Customer Name --->"
            h6$ = "<-S.O.->"
            h7$ = "LUnits"
            h8$ = "Net Value "
            h9$ = "C"                  /* (SR70889) */
            h10$ = "DP"                /* CR2317 drop number */ 
            gosub load_dept                                              ~

        REM - Pos of the '*' indicates those codes that do not deduct
        REM   from Capacity in (APCPLNUC)
        REM   1, 2, 3, 9, 11, 12, 20, 21, 25
            hows$ = "***     * **       **   *                         "

            init(" ") error$, field$, name$
                   
            schema_err%, schema% = 0%                           /* PR Proj */
            init(" ") schema$                                   /* PR Proj */
            call "SCHEMA" (schema$, schema%, #2, schema_err%)   /* PR Proj */ 

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
L10200:               if keyhit% = 10% then gosub build_change
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
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
                  if keyhit%  =  8% then gosub so_analysis
                  if keyhit%  =  9% then gosub load_analysis
                  if keyhit%  = 10% then gosub build_change
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 15% then gosub print_summary
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

        print_summary
            sel% = 1%
            goto L19130

        print_report
            sel% = 0%

L19130:     tot_units = 0.0    : tot_make% = 0%       : tot_pull% = 0%
            tot_product% = 0%  : tot_prod_value = 0.0
            pg_tot_unit  = 0.0 : pg_tot_value   = 0.0
            gosub select_printer
            gosub generate_report

        close printer
        return clear all
        goto inputmode

        select_printer
            load% = 1%                           /* Load No. Specified */
            if str(or_load$,1%,3%) = "N/A" then load% = 0%
            if sel% <> 0% then goto L19380        /* Detail Reports     */
               call "SHOSTAT" ( "Creating Detail Analysis Report")
               if load% <> 0% then goto L19330    /* No Load No. Specif.*/
                  if analysis% = 0% then                                 ~
                      title$ = "Sales Order(Only) Detail Analysis Report"~
                 else title$ = "Sales Order(Only) Detail Analysis Report"
                  goto select_summary
L19330:        if analysis% = 0% then            /* Load No. Specified */~
                      title$ = "Sales Orders/Load Detail Analysis Report"~
                 else title$ = "***Load (Only) Detail Analysis Report***"
               goto select_summary
                                                 /* Summary Reports    */
L19380:     call "SHOSTAT" ( "Creating Summary Report")
            if load% <> 0% then goto L19440       /* No Load No. Specif.*/
               if analysis% = 0% then                                    ~
                      title$ = "***Sales Orders (Only) Summary Report***"~
                 else title$ = "***Sales Orders (Only) Summary Report***"
                  goto select_summary
L19440:        if analysis% = 0% then            /* Load No. Specified */~
                      title$ = "Sales Orders/Load Summary Report        "~
                 else title$ = "****** Load (Only) Summary Report ******"
        select_summary
            runtime$ = " "
            call "TIME" (runtime$)
            select printer (134)
            pageno% = 0%
            lcntr% = 99%
            mat nn% = zer                        /* Windows,Sashs,Parts*/
            mat nt% = zer                        /* Screen Totals      */
            mat np% = zer                        /* Product Totals     */
            mat tt  = zer                        /* Dept's Net Dollars */
            tt = 0.0                             /* Total Run Net $$$  */
        return

        so_analysis
            call "SHOSTAT" ("Loading S.O. Analysis Data")
            analysis% = 0%
            goto L19680
        load_analysis
            call "SHOSTAT" ("Loading Truck Load Data")
            analysis% = 1%

L19680:     init(" ") tot_units$, tot_make$, tot_pull$, tot_product$,    ~
                      tot_prod_value$, pg_tot_unit$, pg_tot_value$, cnt$,~
                      or_key$, sc_key$, sc_key1$, c1$(), c2$(), c3$(),   ~
                      c4$(), c5$(), c6$(), c7$(), c8$(), c9$(), c10$()
            mat nn% = zer                        /* Windows,Sashs,Parts*/
            mat nt% = zer                        /* Screen Totals      */
            mat np% = zer                        /* Product Totals     */
            mat tt  = zer                        /* Dept's Net Dollars */
            tt = 0.0                             /* Total Run Net $$$  */
            delete_flag% = 0%                    /* Delete From Load   */
            drop_seq_max% = 0%                 /* Last Drop Seq Loaded */
            cnt% = 0%
            cnt$ = "Records Scanned [ xxxxxxxx ]"
            tot_units    = 0.0 : tot_make%      = 0%  : tot_pull% = 0%
            tot_product% = 0%  : tot_prod_value = 0.0
            pg_tot_unit  = 0.0 : pg_tot_value   = 0.0
            val_max%     = 0%  : cc%            = 0%
            on (analysis% + 1%) gosub scan_orders, scan_loads

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
         "Enter a Valid Beginning/Ending Due Date/?                    ",~
         "Enter a Valid Beginning/Ending Region Code or 'Al' = All?    ",~
         "Enter a Valid Beginning/Ending Route Code or 'AL' = All?     ",~
         "Enter a Valid Beginning/Ending Zip Code or 'ALL' = All?      ",~
         "Enter a Valid Load Number to Build or <Return> for Analysis? "

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
         "Enter a Non-Blank Character or New Insert Seq. Number?       "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, readkey$, bg_due$, bg_dte$,~
                      bg_region$, ed_region$, bg_route$, ed_route$,      ~
                      bg_zip$, ed_zip$, c1$(), c2$(), hdr$, msg$(),      ~
                      c3$(), c4$(), c5$(), c6$(), c7$(), c8$(), c9$(),   ~
                      c10$(),                                            ~
                      tot_units$, tot_make$, tot_pull$, tot_product$,    ~
                      tot_prod_value$, pg_tot_unit$, pg_tot_value$,      ~
                      or_load$, or_load_d$, sc_load$, ed_due$, ed_dte$,  ~
                      ap_type$, flag$, pgm$, so_inv$, item_no$, flds$(), ~
                      bcksubpt_rec$, info_flds$(), subpart$, infopart$,  ~
                      mfgplant$, invplant$

            mat nn% = zer                        /* Windows,Sashs,Parts*/
            mat nt% = zer                        /* Screen Totals      */
            mat np% = zer                        /* Product Totals     */
            mat tt  = zer                        /* Dept's Net Dollars */
            tt = 0.0                             /* Total Run Net $$$  */
            analysis% = 0%                       /* Set Default to S.O.*/

            call "ALLFREE"
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
            tot_units      = round(tot_units + or_units, 2)
            tot_make%      = tot_make%    + or_mak%
            tot_pull%      = tot_pull%    + or_pul%
            tot_product%   = tot_product% + (or_mak% + or_pul%)
            tot_prod_value = round(tot_prod_value + or_value, 2)
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

                                         /* APCPLNOR - New File Layout */
L35350:     FMT CH(170)                  /* Total Record               */

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

                                         /* APCPLNSC - File            */
L35620: FMT CH(128)                      /* Total Record               */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L40070:       gosub set_pf1

              gosub'050(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40200,         /* Beg/End Due Date     */~
                                L40200,         /* Beg/End Region Code  */~
                                L40200,         /* Beg/End Route Code   */~
                                L40200,         /* Beg/End Zip Code     */~
                                L40200          /* Load Number          */
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
               at (03,02), "Begin S.O. Due Date  :",                     ~
               at (03,25), fac(lfac$(1%)), bg_due$              , ch(10),       /* (Y2K, LDJ) */~
                                                                         ~
               at (03,40), "Ending S.O. Due Date :",                     ~
               at (03,63), fac(lfac$(1%)), ed_due$              , ch(10),       /* (Y2K, LDJ) */~
                                                                         ~
               at (04,02), "Begin Region Code    :",                     ~
               at (04,25), fac(lfac$(2%)), bg_region$           , ch(02),~
                                                                         ~
               at (04,40), "Ending Region Code   :",                     ~
               at (04,63), fac(lfac$(2%)), ed_region$           , ch(02),~
                                                                         ~
               at (05,02), "Begin Route Code     :",                     ~
               at (05,25), fac(lfac$(3%)), bg_route$            , ch(05),~
                                                                         ~
               at (05,40), "Ending Route Code    :",                     ~
               at (05,63), fac(lfac$(3%)), ed_route$            , ch(05),~
                                                                         ~
               at (06,02), "Begin Zip Code       :",                     ~
               at (06,25), fac(lfac$(4%)), bg_zip$              , ch(09),~
                                                                         ~
               at (06,40), "Ending Zip Code      :",                     ~
               at (06,63), fac(lfac$(4%)), ed_zip$              , ch(09),~
                                                                         ~
               at (07,02), "Schedule Load Number :",                     ~
               at (07,25), fac(lfac$(5%)), or_load$             , ch(05),~
               at (07,40), fac(hex(84)),   or_load_d$           , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 6% then goto L40700
                  table% = 12%                     /* Region Codes     */
                  gosub display_codes
                  goto L40070

L40700:        if keyhit% <> 7% then goto L40750
                  table% = 16%                     /* Route Codes      */
                  gosub display_codes
                  goto L40070

L40750: REM    IF KEYHIT% <> 15 THEN GOTO 40790
        REM       CALL "PRNTSCRN"
        REM       GOTO 40070

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40990     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &       ~
                      "                                       "
            pf$(2%) = "                 (6)Region Codes        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                 (7)Route Codes         " &       ~
                      "(10)Bld/Chg Loads      (16)Exit Program"
            pfkeys$ = hex(01ffff04ff0607ffff0affffffff0f1000)
            if fieldnr% = 1% then L40950
               str(pf$(3%),40%,20%) = " " : str(pfkeys$,10%,1%) = hex(ff)
               str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40950:     if fieldnr% > 1% then L40970
               str(pf$(1%),18%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40970:     return

L40990: if fieldnr% > 0% then L41100  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "(8)S.O. Analysis       (14)Prt DTL Rpt "
            pf$(2%) = "                 (6)Region Codes        " &       ~
                      "(9)Load Analysis       (15)Prt Sum Rpt "
            pf$(3%) = "                 (7)Route Codes         " &       ~
                      "(10)Bld/Chg Loads      (16)Exit Program"
            pfkeys$ = hex(01ffffffff060708090affffff0e0f1000)
            if ap_type$ <> "Y" then goto not_app
               str(pf$(1%),40%,18%) = " " : str(pfkeys$,8%,1%) = hex(ff)

not_app:
        if or_load$ <> "99999" then goto L41095
               str(pf$(1%),40%,18%) = " " : str(pfkeys$,8%,1%) = hex(ff)
L41095:
            if len(or_load$) = 5 then return
               str(pf$(2%),40%,18%) = " " : str(pfkeys$,9%,1%) = hex(ff)
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
            *         S T A T U S   D I S P L A Y   S C R E E N         *~
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
               at (03,02), "Beg/End Due Date:",                          ~
               at (03,20), fac(hex(84)), bg_due$                , ch(10),       /* (Y2K, LDJ) */~
               at (03,31), fac(hex(84)), ed_due$                , ch(10),       /* (Y2K, LDJ) */~
                                                                         ~
               at (03,42), "Tot Loading Units:",                                /* (Y2K, LDJ) */~
               at (03,61), fac(hex(84)), tot_units$             , ch(12),       /* (Y2K, LDJ) */~
                                                                         ~
               at (04,02), "Beg/End Region  :",                          ~
               at (04,20), fac(hex(84)), bg_region$             , ch(02),~
               at (04,31), fac(hex(84)), ed_region$             , ch(02),       /* (Y2K, LDJ) */~
                                                                         ~
               at (04,42), "Tot Pull Units   :",                                /* (Y2K, LDJ) */~
               at (04,61), fac(hex(84)), tot_pull$              , ch(12),       /* (Y2K, LDJ) */~
                                                                         ~
               at (05,02), "Beg/End Route   :",                          ~
               at (05,20), fac(hex(84)), bg_route$              , ch(05),~
               at (05,31), fac(hex(84)), ed_route$              , ch(05),       /* (Y2K, LDJ) */~
                                                                         ~
               at (05,42), "Tot Make Units   :",                                /* (Y2K, LDJ) */~
               at (05,61), fac(hex(84)), tot_make$              , ch(12),       /* (Y2K, LDJ) */~
                                                                         ~
               at (06,02), "Beg/End Zip Code:",                          ~
               at (06,20), fac(hex(84)), bg_zip$                , ch(09),~
               at (06,31), fac(hex(84)), ed_zip$                , ch(09),       /* (Y2K, LDJ) */~
                                                                         ~
               at (06,42), "Tot Product Units:",                                /* (Y2K, LDJ) */~
               at (06,61), fac(hex(84)), tot_product$           , ch(12),       /* (Y2K, LDJ) */~
                                                                         ~
               at (07,02), "Schedule Load No:",                          ~
               at (07,20), fac(hex(84)), or_load$               , ch(05),~
                                                                         ~
               at (07,42), "Tot Dollar Value.:",                                /* (Y2K, LDJ) */~
               at (07,61), fac(hex(84)), tot_prod_value$        , ch(12),       /* (Y2K, LDJ) */~
                                                                         ~
               at (09,02), fac(hex(a4)), h1$                    , ch(03),~
               at (09,06), fac(hex(a4)), h2$                    , ch(05),~
               at (09,12), fac(hex(a4)), h10$                   , ch(02),~
               at (09,15), fac(hex(a4)), h3$                    , ch(05),~
               at (09,21), fac(hex(a4)), h9$                    , ch(01),~
               at (09,23), fac(hex(a4)), h4$                    , ch(07),~
               at (09,31), fac(hex(a4)), h5$                    , ch(23),~
               at (09,55), fac(hex(a4)), h6$                    , ch(08),~
               at (09,64), fac(hex(a4)), h7$                    , ch(06),~
               at (09,71), fac(hex(a4)), h8$                    , ch(10),~
                                                                         ~
               at (10,03), fac(lfac$(1%)), c1$( 1% + kk%)       , ch(01),~
               at (10,06), fac(lfac$(1%)), c2$( 1% + kk%)       , ch(05),~
               at (10,12), fac(hex(84)),  c10$( 1% + kk%)       , ch(02),~
               at (10,15), fac(hex(84)),   c3$( 1% + kk%)       , ch(05),~
               at (10,21), fac(hex(84)),   c9$( 1% + kk%)       , ch(01),~
               at (10,23), fac(hex(84)),   c4$( 1% + kk%)       , ch(07),~
               at (10,31), fac(hex(84)),   c5$( 1% + kk%)       , ch(23),~
               at (10,55), fac(hex(84)),   c6$( 1% + kk%)       , ch(08),~
               at (10,64), fac(hex(84)),   c7$( 1% + kk%)       , ch(06),~
               at (10,71), fac(hex(84)),   c8$( 1% + kk%)       , ch(10),~
                                                                         ~
               at (11,03), fac(lfac$(1%)), c1$( 2% + kk%)       , ch(01),~
               at (11,06), fac(lfac$(1%)), c2$( 2% + kk%)       , ch(05),~
               at (11,12), fac(hex(84)),  c10$( 2% + kk%)       , ch(02),~
               at (11,15), fac(hex(84)),   c3$( 2% + kk%)       , ch(05),~
               at (11,21), fac(hex(84)),   c9$( 2% + kk%)       , ch(01),~
               at (11,23), fac(hex(84)),   c4$( 2% + kk%)       , ch(07),~
               at (11,31), fac(hex(84)),   c5$( 2% + kk%)       , ch(23),~
               at (11,55), fac(hex(84)),   c6$( 2% + kk%)       , ch(08),~
               at (11,64), fac(hex(84)),   c7$( 2% + kk%)       , ch(06),~
               at (11,71), fac(hex(84)),   c8$( 2% + kk%)       , ch(10),~
                                                                         ~
               at (12,03), fac(lfac$(1%)), c1$( 3% + kk%)       , ch(01),~
               at (12,06), fac(lfac$(1%)), c2$( 3% + kk%)       , ch(05),~
               at (12,12), fac(hex(84)),  c10$( 3% + kk%)       , ch(02),~
               at (12,15), fac(hex(84)),   c3$( 3% + kk%)       , ch(05),~
               at (12,21), fac(hex(84)),   c9$( 3% + kk%)       , ch(01),~
               at (12,23), fac(hex(84)),   c4$( 3% + kk%)       , ch(07),~
               at (12,31), fac(hex(84)),   c5$( 3% + kk%)       , ch(23),~
               at (12,55), fac(hex(84)),   c6$( 3% + kk%)       , ch(08),~
               at (12,64), fac(hex(84)),   c7$( 3% + kk%)       , ch(06),~
               at (12,71), fac(hex(84)),   c8$( 3% + kk%)       , ch(10),~
                                                                         ~
               at (13,03), fac(lfac$(1%)), c1$( 4% + kk%)       , ch(01),~
               at (13,06), fac(lfac$(1%)), c2$( 4% + kk%)       , ch(05),~
               at (13,12), fac(hex(84)),  c10$( 4% + kk%)       , ch(02),~
               at (13,15), fac(hex(84)),   c3$( 4% + kk%)       , ch(05),~
               at (13,21), fac(hex(84)),   c9$( 4% + kk%)       , ch(01),~
               at (13,23), fac(hex(84)),   c4$( 4% + kk%)       , ch(07),~
               at (13,31), fac(hex(84)),   c5$( 4% + kk%)       , ch(23),~
               at (13,55), fac(hex(84)),   c6$( 4% + kk%)       , ch(08),~
               at (13,64), fac(hex(84)),   c7$( 4% + kk%)       , ch(06),~
               at (13,71), fac(hex(84)),   c8$( 4% + kk%)       , ch(10),~
                                                                         ~
               at (14,03), fac(lfac$(1%)), c1$( 5% + kk%)       , ch(01),~
               at (14,06), fac(lfac$(1%)), c2$( 5% + kk%)       , ch(05),~
               at (14,12), fac(hex(84)),  c10$( 5% + kk%)       , ch(02),~               
               at (14,15), fac(hex(84)),   c3$( 5% + kk%)       , ch(05),~
               at (14,21), fac(hex(84)),   c9$( 5% + kk%)       , ch(01),~
               at (14,23), fac(hex(84)),   c4$( 5% + kk%)       , ch(07),~
               at (14,31), fac(hex(84)),   c5$( 5% + kk%)       , ch(23),~
               at (14,55), fac(hex(84)),   c6$( 5% + kk%)       , ch(08),~
               at (14,64), fac(hex(84)),   c7$( 5% + kk%)       , ch(06),~
               at (14,71), fac(hex(84)),   c8$( 5% + kk%)       , ch(10),~
                                                                         ~
               at (15,03), fac(lfac$(1%)), c1$( 6% + kk%)       , ch(01),~
               at (15,06), fac(lfac$(1%)), c2$( 6% + kk%)       , ch(05),~
               at (15,12), fac(hex(84)),  c10$( 6% + kk%)       , ch(02),~               
               at (15,15), fac(hex(84)),   c3$( 6% + kk%)       , ch(05),~
               at (15,21), fac(hex(84)),   c9$( 6% + kk%)       , ch(01),~
               at (15,23), fac(hex(84)),   c4$( 6% + kk%)       , ch(07),~
               at (15,31), fac(hex(84)),   c5$( 6% + kk%)       , ch(23),~
               at (15,55), fac(hex(84)),   c6$( 6% + kk%)       , ch(08),~
               at (15,64), fac(hex(84)),   c7$( 6% + kk%)       , ch(06),~
               at (15,71), fac(hex(84)),   c8$( 6% + kk%)       , ch(10),~
                                                                         ~
               at (16,03), fac(lfac$(1%)), c1$( 7% + kk%)       , ch(01),~
               at (16,06), fac(lfac$(1%)), c2$( 7% + kk%)       , ch(05),~
               at (16,12), fac(hex(84)),  c10$( 7% + kk%)       , ch(02),~               
               at (16,15), fac(hex(84)),   c3$( 7% + kk%)       , ch(05),~
               at (16,21), fac(hex(84)),   c9$( 7% + kk%)       , ch(01),~
               at (16,23), fac(hex(84)),   c4$( 7% + kk%)       , ch(07),~
               at (16,31), fac(hex(84)),   c5$( 7% + kk%)       , ch(23),~
               at (16,55), fac(hex(84)),   c6$( 7% + kk%)       , ch(08),~
               at (16,64), fac(hex(84)),   c7$( 7% + kk%)       , ch(06),~
               at (16,71), fac(hex(84)),   c8$( 7% + kk%)       , ch(10),~
                                                                         ~
               at (17,03), fac(lfac$(1%)), c1$( 8% + kk%)       , ch(01),~
               at (17,06), fac(lfac$(1%)), c2$( 8% + kk%)       , ch(05),~
               at (17,12), fac(hex(84)),  c10$( 8% + kk%)       , ch(02),~
               at (17,15), fac(hex(84)),   c3$( 8% + kk%)       , ch(05),~
               at (17,21), fac(hex(84)),   c9$( 8% + kk%)       , ch(01),~
               at (17,23), fac(hex(84)),   c4$( 8% + kk%)       , ch(07),~
               at (17,31), fac(hex(84)),   c5$( 8% + kk%)       , ch(23),~
               at (17,55), fac(hex(84)),   c6$( 8% + kk%)       , ch(08),~
               at (17,64), fac(hex(84)),   c7$( 8% + kk%)       , ch(06),~
               at (17,71), fac(hex(84)),   c8$( 8% + kk%)       , ch(10),~
                                                                         ~
               at (18,03), fac(lfac$(1%)), c1$( 9% + kk%)       , ch(01),~
               at (18,06), fac(lfac$(1%)), c2$( 9% + kk%)       , ch(05),~
               at (18,12), fac(hex(84)),  c10$( 9% + kk%)       , ch(02),~               
               at (18,15), fac(hex(84)),   c3$( 9% + kk%)       , ch(05),~
               at (18,21), fac(hex(84)),   c9$( 9% + kk%)       , ch(01),~
               at (18,23), fac(hex(84)),   c4$( 9% + kk%)       , ch(07),~
               at (18,31), fac(hex(84)),   c5$( 9% + kk%)       , ch(23),~
               at (18,55), fac(hex(84)),   c6$( 9% + kk%)       , ch(08),~
               at (18,64), fac(hex(84)),   c7$( 9% + kk%)       , ch(06),~
               at (18,71), fac(hex(84)),   c8$( 9% + kk%)       , ch(10),~
                                                                         ~
               at (19,03), fac(lfac$(1%)), c1$(10% + kk%)       , ch(01),~
               at (19,06), fac(lfac$(1%)), c2$(10% + kk%)       , ch(05),~
               at (19,12), fac(hex(84)),  c10$(10% + kk%)       , ch(02),~               
               at (19,15), fac(hex(84)),   c3$(10% + kk%)       , ch(05),~
               at (19,21), fac(hex(84)),   c9$(10% + kk%)       , ch(01),~
               at (19,23), fac(hex(84)),   c4$(10% + kk%)       , ch(07),~
               at (19,31), fac(hex(84)),   c5$(10% + kk%)       , ch(23),~
               at (19,55), fac(hex(84)),   c6$(10% + kk%)       , ch(08),~
               at (19,64), fac(hex(a4)),   c7$(10% + kk%)       , ch(06),~
               at (19,71), fac(hex(a4)),   c8$(10% + kk%)       , ch(10),~
                                                                         ~
               at (20,64), fac(hex(84)),   pg_tot_unit$         , ch(06),~
               at (20,71), fac(hex(84)),   pg_tot_value$        , ch(10),~
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

L42930:        if keyhit% <> 7% then goto L42950            /* Re-Seq   */
                  gosub re_seq_route
                  goto L43190

L42950:        if keyhit% <> 8% then goto L43045
L42955:           dept% = 0%
                  jj% = 0%
                  gosub'103(1%)                /* Detail by Department */
                                               /* Check Dept. Select   */
                  lin% = cursor%(1%)           /* Check Prod For Dept  */
                  col% = cursor%(2%)
                  dept% = lin% - 9%
                  if col% > 40% then dept% = dept% + 12%
                  keyhit% = 0%
                  if dept% < 1% then goto L43045
                     if x1% = 2% then dept% = dept% + 24%
                     pp_max% = 0%
                     gosub'103(1%)
                     goto L42955

        REM - Analysis  = 0%
                                   /* Include S.O. 'Non-Blank'         */
                                   /* Skip 'Blank' Process 'Non-Blank' */
L43045:        if keyhit% <> 9%  then goto L43070
                  gosub process_data
                  goto L43190
                                   /* Exclude S.O. 'Non-Blank'         */
                                   /* Skip 'Non-Blank', Process 'Blank'*/
L43070:        if keyhit% <> 10% then goto L43085
                  gosub process_data
                  goto L43190
L43085: REM - ANALYSIS% = 1%       /* Re-Sequence Load Assign Drops    */
               if keyhit% <> 11% then goto L43110
                  gosub re_seq_load
                  goto L43190
                                   /* Delete S.O. 'Non-Blank' from Load*/
L43110:        if keyhit% <> 12% then goto L43130
                  gosub delete_orders
                  goto L43190

L43130:        if keyhit% <> 13% then goto L43145
                  gosub print_summary

L43145:        if keyhit% <> 14% then goto L43160
                  gosub print_report

L43160:        if keyhit% <> 15 then goto L43180
                  call "PRNTSCRN"
                  goto L42025

L43180:        if keyhit% <> 16% then goto L42025

L43190:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf2
            if analysis% = 0% then                                       ~
            line1$ =                                                     ~
                  "(New) Scheduling S.O. Analysis - Display"             ~
                              else                                       ~
            line1$ =                                                     ~
                  "(New) Scheduling Load Analysis - Display"

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
                convert c8$(n% + kk%) to x1,  data goto L43315
L43315:
                pg_tot_unit  = pg_tot_unit + y1
                pg_tot_value = pg_tot_value + x1
            next n%
            convert pg_tot_unit to pg_tot_unit$, pic(###.##)
            convert pg_tot_value to pg_tot_value$, pic(######.##-)

            pf$(1%) = "(1)Start Over  (4)Previous   (8)Dept Ana" &       ~
                      "lysis (11)Re-Seq Load  (14)Prt Dtl Rpt "
            pf$(2%) = "(2)First       (5)Next       (9)Include " &       ~
                      "      (12)Delete SO's  (15)Print Screen"
            pf$(3%) = "(3)Last        (7)Re-Seq RTE (10)Exclude" &       ~
                      "      (13)Prt Sum Rpt  (16)Exit Program"
            pfkeys$ = hex(0102030405ff0708090a0b0c0d0e0f1000)
            gosub check_screen
        return

        check_screen
            if analysis% = 0% then gosub no_delete
            if analysis% = 1% then gosub no_i_e
            if str(or_load$,1%,3%) = "N/A" then gosub no_i_e
            if ap_type$ = "Y" then gosub ap_load_scr
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
        ap_load_scr
            str(pf$(1%),46%,17%) = " " : str(pfkeys$,11%,1%) = hex(ff)
            str(pf$(3%),15%,16%) = " " : str(pfkeys$,7%,1%) = hex(ff)
        return


        REM *************************************************************~
            *                S U M M A R Y   S C R E E N                *~
            *************************************************************

        deffn'103(fieldnr%)
L45050:     gosub set_pf3
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), line1$                 , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(50),~
               at (02,60), fac(hex(8c)), page$                  , ch(16),~
                                                                         ~
               at (03,02), "Beg/End Due Date:",                          ~
               at (03,20), fac(hex(84)), bg_due$                , ch(10),       /* (Y2K, LDJ) */~
               at (03,31), fac(hex(84)), ed_due$                , ch(10),       /* (Y2K, LDJ) */~
                                                                         ~
               at (03,42), "Tot Loading Units:",                                /* (Y2K, LDJ) */~
               at (03,61), fac(hex(84)), tot_units$             , ch(12),~
                                                                         ~
               at (04,02), "Beg/End Region  :",                          ~
               at (04,20), fac(hex(84)), bg_region$             , ch(02),~
               at (04,31), fac(hex(84)), ed_region$             , ch(02),       /* (Y2K, LDJ) */~
                                                                         ~
               at (04,42), "Tot Pull Units   :",                                /* (Y2K, LDJ) */~
               at (04,61), fac(hex(84)), tot_pull$              , ch(12),       /* (Y2K, LDJ) */~
                                                                         ~
               at (05,02), "Beg/End Route   :",                          ~
               at (05,20), fac(hex(84)), bg_route$              , ch(05),~
               at (05,31), fac(hex(84)), ed_route$              , ch(05),       /* (Y2K, LDJ) */~
                                                                         ~
               at (05,42), "Tot Make Units   :",                                /* (Y2K, LDJ) */~
               at (05,61), fac(hex(84)), tot_make$              , ch(12),       /* (Y2K, LDJ) */~
                                                                         ~
               at (06,02), "Beg/End Zip Code:",                          ~
               at (06,20), fac(hex(84)), bg_zip$                , ch(09),~
               at (06,31), fac(hex(84)), ed_zip$                , ch(09),       /* (Y2K, LDJ) */~
                                                                         ~
               at (06,42), "Tot Product Units:",                                /* (Y2K, LDJ) */~
               at (06,61), fac(hex(84)), tot_product$           , ch(12),       /* (Y2K, LDJ) */~
                                                                         ~
               at (07,02), "Schedule Load No:",                          ~
               at (07,20), fac(hex(84)), or_load$               , ch(05),~
                                                                         ~
               at (07,42), "Tot Dollar Value.:",                                /* (Y2K, LDJ) */~
               at (07,61), fac(hex(84)), tot_prod_value$        , ch(12),~
                                                                         ~
               at (08,02), fac(hex(84)), scr$                   , ch(35),~
               at (08,38), fac(lfac$(1%)),pp$                   , ch(01),~
                                                                         ~
               at (09,02), fac(hex(a4)), fld1$                  , ch(20),~
               at (09,23), fac(hex(a4)), fld2$                  , ch(06),~
               at (09,30), fac(hex(a4)), fld3$                  , ch(04),~
               at (09,35), fac(hex(a4)), fld4$                  , ch(04),~
                                                                         ~
               at (09,40), fac(hex(a4)), fld1$                  , ch(20),~
               at (09,61), fac(hex(a4)), fld2$                  , ch(06),~
               at (09,68), fac(hex(a4)), fld3$                  , ch(04),~
               at (09,73), fac(hex(a4)), fld4$                  , ch(04),~
                                                                         ~
               at (10,02), fac(hex(84)), txt$(1% + jj%)         , ch(37),~
               at (10,40), fac(hex(84)), txt$(13% + jj%)        , ch(37),~
                                                                         ~
               at (11,02), fac(hex(84)), txt$(2% + jj%)         , ch(37),~
               at (11,40), fac(hex(84)), txt$(14% + jj%)        , ch(37),~
                                                                         ~
               at (12,02), fac(hex(84)), txt$(3% + jj%)         , ch(37),~
               at (12,40), fac(hex(84)), txt$(15% + jj%)        , ch(37),~
                                                                         ~
               at (13,02), fac(hex(84)), txt$(4% + jj%)         , ch(37),~
               at (13,40), fac(hex(84)), txt$(16% + jj%)        , ch(37),~
                                                                         ~
               at (14,02), fac(hex(84)), txt$(5% + jj%)         , ch(37),~
               at (14,40), fac(hex(84)), txt$(17% + jj%)        , ch(37),~
                                                                         ~
               at (15,02), fac(hex(84)), txt$(6% + jj%)         , ch(37),~
               at (15,40), fac(hex(84)), txt$(18% + jj%)        , ch(37),~
                                                                         ~
               at (16,02), fac(hex(84)), txt$(7% + jj%)         , ch(37),~
               at (16,40), fac(hex(84)), txt$(19% + jj%)        , ch(37),~
                                                                         ~
               at (17,02), fac(hex(84)), txt$(8% + jj%)         , ch(37),~
               at (17,40), fac(hex(84)), txt$(20% + jj%)        , ch(37),~
                                                                         ~
               at (18,02), fac(hex(84)), txt$(9% + jj%)         , ch(37),~
               at (18,40), fac(hex(84)), txt$(21% + jj%)        , ch(37),~
                                                                         ~
               at (19,02), fac(hex(84)), txt$(10% + jj%)        , ch(37),~
               at (19,40), fac(hex(84)), txt$(22% + jj%)        , ch(37),~
                                                                         ~
               at (20,02), fac(hex(84)), txt$(11% + jj%)        , ch(37),~
               at (20,40), fac(hex(84)), txt$(23% + jj%)        , ch(37),~
                                                                         ~
               at (21,02), fac(hex(84)), txt$(12% + jj%)        , ch(37),~
               at (21,40), fac(hex(84)), txt$(24% + jj%)        , ch(37),~
                                                                         ~
               at (22,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L46070            /* First    */
L46040:           jj% = 0%
                  goto L45050

L46070:        if keyhit% <> 3% then goto L46120            /* Last      */
L46080:           x% = int(pp_max% / 24%)
                  jj% = (x%*24%)
                  goto L45050

L46120:        if keyhit% <> 4% then goto L46180            /* Previous */
                  if jj% < 25% then goto L46040
                  jj% = jj% - 24%
                  if jj% <= 1% then goto L46040
                  goto L45050

L46180:        if keyhit% <> 5% then goto L46230            /* Next     */
                  jj% = jj% + 24%
                  if jj% < pp_max% then goto L45050
                  goto L46080

L46230:        if keyhit% <> 15 then goto L46270
                  call "PRNTSCRN"
                  goto L45050

L46270:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
          lfac$(1%) = hex(84)                  /* Position Cusrsor to  */
          if dept% <> 0% then goto set_pf4     /* Middle of Screen     */
            lfac$(1%) = hex(81)                /* Only for Product     */
            pp_max% = dept_max%                /* Detail Display       */
            fld1$ = "<Dept's Description>"                     /* (20) */
            fld2$ = " Wind."                                   /* ( 6) */
            fld3$ = "Sash"                                     /* ( 4) */
            fld4$ = "Part"                                     /* ( 4) */

            if analysis% = 0% then                                       ~
            line1$ =                                                     ~
                 "(New)Scheduling S.O. Analysis-Department"              ~
                              else                                       ~
            line1$ =                                                     ~
                 "(New)Scheduling Load Analysis-Department"

                                                   /* (1) = Windows    */
            init(" ") txt$(), inpmessage$          /* (2) = Sashs      */
               for i% = 1% to pp_max%              /* (3) = Parts      */
                 txt$(i%) = str(dd$(i%),1%,20%)
                 convert nn%(i%,1%) to str(txt$(i%),22%,6%), pic(######)
                 convert nn%(i%,2%) to str(txt$(i%),29%,4%), pic(####)
                 convert nn%(i%,3%) to str(txt$(i%),34%,4%), pic(####)
               next i%

           inpmessage$ = " Product Analysis: Windows (          ) " &    ~
                                            "Sashs (      ) "       &    ~
                                            "Parts (      ) "
           convert nt%(1%) to str(inpmessage$,29%,10%), pic(##########)

           convert nt%(2%) to str(inpmessage$,48%, 6%), pic(######)

           convert nt%(3%) to str(inpmessage$,63%, 6%), pic(######)
            scr$ = "Select Dept & <Return> for Detail"
           page$ ="Page: xxx of xxx"
           x1% = (jj% / 24%) + 1%
           x2% = (pp_max% / 24%) + 1%
           if x2% < 1% then x2% = 1%
           convert x1% to str(page$,7%,3%), pic(###)
           convert x2% to str(page$,14%,3%), pic(###)

            pf$(1%) = "(2)First           (4)Previous          " &       ~
                      "                       (15)Print Screen"
            pf$(2%) = "(3)Last            (5)Next              " &       ~
                      "                       (16)Exit Screen "
            pfkeys$ = hex(ff02030405ffffffffffffffffff0f1000)
            gosub check_scr
        return

        set_pf4
          if pp_max% <> 0% then goto L47040        /* Skip Load of Data */

            fld1$ = "Mod <-Description-->"                     /* (20) */
            fld2$ = "Makes "                                   /* ( 6) */
            fld3$ = "    "                                     /* ( 4) */
            fld4$ = "    "                                     /* ( 4) */
                                                           /* (EWD004) */
            if cc$(dept%) = "102" or cc$(dept%) = "104" then             ~
               fld2$ = "Pulls "                                /* ( 6) */

            line1$ =                                                     ~
              "(New)Product Display - (" & dd$(dept%) & ") Department"
            init(" ") txt$(), inpmessage$, scr$
            pp_max% = pm%(dept%)
               for i% = 1% to pp_max%
                 sav_model$ = pp$(dept%,i%)
                 gosub lookup_model
                 str(txt$(i%),1%,3%)  = sav_model$
                 str(txt$(i%),5%,16%) = desc$

             convert np%(dept%,i%) to str(txt$(i%),22%,6%), pic(######)
               next i%

L47040:    inpmessage$ = " Product Analysis: Press <Return> to Continue?"
           page$ ="Page: xxx of xxx"
           x1% = (jj% / 24%) + 1%
           x2% = (pp_max% / 24%) + 1%
           if x2% < 1% then x2% = 1%
           convert x1% to str(page$,7%,3%), pic(###)
           convert x2% to str(page$,14%,3%), pic(###)

            pf$(1%) = "(2)First           (4)Previous          " &       ~
                      "                       (15)Print Screen"
            pf$(2%) = "(3)Last            (5)Next              " &       ~
                      "                       (16)Exit Screen "
            pfkeys$ = hex(ff02030405ffffffffffffffffff0f1000)
            gosub check_scr
        return

        check_scr
            if pp_max% > 24% then goto L47270
               gosub no_fst
               gosub no_nxt
               gosub no_lst
               gosub no_prv
               return
L47270:      if jj% >= 24% then goto L47300
                gosub no_fst
                gosub no_prv
L47300:      if (jj% + 24%) <= pp_max% then goto L47320
                gosub no_lst
L47320:      if jj% <= (pp_max% - 24%) then goto L47340
                gosub no_nxt
L47340: return
        no_fst
            str(pf$(1%),1%, 9%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_nxt
            str(pf$(2%),20%, 9%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_lst
            str(pf$(2%),1%,9%)   = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prv
            str(pf$(1%),20%,12%) = " " : str(pfkeys$,4%,1%) = hex(ff)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50150,         /* Beg/End Due Date      */ ~
                              L50390,         /* Beg/End Region Code   */ ~
                              L50630,         /* Beg/End Route Code    */ ~
                              L50860,         /* Beg/End Zip Code      */ ~
                              L51060          /* Schedule Load Number  */
            return

L50150: REM Beginning Due/Delivery Date           BG_DUE$, BG_DTE$
            if bg_due$ > " " then goto L50190                                   /* (Y2K, LDJ) */
               bg_due$ = date$                                                  /* (Y2K, LDJ) */

L50190:     date% = 0%
            call "DATEOKC" (bg_due$, date%, errormsg$)                          /* (Y2K, LDJ) */
            if date% = 0% then goto date_error
            bg_dte$ = bg_due$
            call "DATUFMTC" (bg_dte$)                                           /* (Y2K, LDJ) */
        REM Ending Due/Delivery Date              ED_DUE$, ED_DTE$
            if len(ed_due$) < 6 then ed_due$ = bg_due$
            date% = 0%
            call "DATEOKC" (ed_due$, date%, errormsg$)                          /* (Y2K, LDJ) */
            if date% = 0% then goto date_error
            ed_dte$ = ed_due$
            call "DATUFMTC" (ed_dte$)                                           /* (Y2K, LDJ) */
            if bg_dte$ > ed_dte$ then goto L50330
        return
L50330:     errormsg$ = "(Error) - Invalid Due Date Range??"
        date_error
            gosub error_prompt
            init(" ") bg_due$, bg_dte$, ed_due$, ed_dte$
        return

L50390: REM Beg/End Region Codes                  BG_REGION$, ED_REGION$
            if bg_region$ <> " " then goto L50440
L50410:        bg_region$ = "AL"
               ed_region$ = bg_region$
               return
L50440:     if str(bg_region$,1%,1%) = "A" then goto L50410
            init(" ")readkey$
            str(readkey$,1%,9%)   = "PLAN REGN"
            str(readkey$,10%,15%) = bg_region$
            read #2,key = readkey$, eod goto region_error
        REM END Region Code
            if len(ed_region$) < 2 then ed_region$ = bg_region$
            init(" ") readkey$
            str(readkey$,1%,9%)   = "PLAN REGN"
            str(readkey$,10%,15%) = ed_region$
            read #2,key = readkey$, eod goto region_error
            if bg_region$ > ed_region$ then goto region_error
        return
        region_error
          errormsg$="(Error) - Invalid Beg/End Region Code??"
          gosub error_prompt
          init(" ") bg_region$, ed_region$
        return

L50630: REM Beg/End Route Codes                   BG_ROUTE$, ED_ROUTE$
            if bg_route$ <> " " then goto L50680
L50650:        bg_route$ = "ALL  "
               ed_route$ = bg_route$
               return
L50680:     if str(bg_route$,1%,1%) = "A" then goto L50650
            init(" ") readkey$
            str(readkey$,1%,9%)   = "PLAN RTE "
            str(readkey$,10%,15%) = bg_route$
            read #2,key = readkey$, eod goto route_error
            if len(ed_route$) < 3 then ed_route$ = bg_route$
            init(" ") readkey$
            str(readkey$,1%,9%)   = "PLAN RTE "
            str(readkey$,10%,15%) = ed_route$
            read #2,key = readkey$, eod goto route_error
            if bg_route$ > ed_route$ then goto route_error
        return
        route_error
          errormsg$="(Error) - Invalid Beg/End Route Code ?"
          gosub error_prompt
          init(" ") bg_route$, ed_route$
        return

L50860: REM Beg/End Zip Code                      BG_ZIP$, ED_ZIP$
            if bg_zip$ <> " " then goto L50910
L50880:        bg_zip$ = "ALL--Zips"
               ed_zip$ = bg_zip$
               return
L50910:     if str(bg_zip$,1%,3%) = "ALL" then goto L50880
            convert bg_zip$ to x1%, data goto zip_error

        REM Ending Zip Code
            if len(ed_zip$) < 5  then ed_zip$ = bg_zip$
            convert ed_zip$ to x2%, data goto zip_error

            if x1% > x2% then goto zip_error
        return
        zip_error
          errormsg$="(Error) - Invalid Beg/End Zip Range?"
          gosub error_prompt
          init(" ") bg_zip$, ed_zip$
        return

L51060: REM Schedule Load Number                  OR_LOAD$, OR_LOAD_D$
            if or_load$ <> " " then goto L51110
               or_load$ = "N/A  "
               or_load_d$ = "Not Applicable at This Time"
               return
L51110:     convert or_load$ to or_load%, data goto L51150

            convert or_load% to or_load$, pic(00000)
            goto L51190
L51150:     convert str(or_load$,2%,4%) to or_load%, data goto load_error

            convert or_load% to str(or_load$,2%,4%), pic(0000)

L51190: REM - LOOK-UP LOAD
            read #5,key = or_load$, using L51220, or_load_d$,             ~
                                                 eod goto load_error
L51220:        FMT POS(16), CH(30)

            gosub lookup_app_load
        return
        load_error
          errormsg$="(Error) - Invalid Load Number, Not Defined?"
          gosub error_prompt
          init(" ") or_load$, or_load_d$
        return

        display_codes
            call "APCPLN1B" (table%, #2)
        return

        lookup_app_load                                      /*  (EWD006)  */
            init(" ") ap_type$
            read #18, key = or_load$, using L51800, ap_type$,             ~
                                                  eod goto no_app_load
L51800:                 FMT POS(83), CH(2)
        no_app_load
        return                                               /*  (EWD006)  */

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55080: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------~
        ~--+

L55110: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------~
        ~--!

                                           /* Report Detail - Header   */
L55150: %!######## ########                            ##################~
        ~######################                                 APCPLN03:~
        ~  !
L55180: %!User Id: ###                                                   ~
        ~                                                     Page: #####~
        ~  !
L55210: %!Beg Due Date: ##########                 Beg Region: ##        ~
        ~             Beg Route: #####                 Beg Zip: #########~
        ~  !                                                                        /* (Y2K, LDJ) */
L55240: %!End Due Date: ##########                 End Region: ##        ~
        ~             End Route: #####                 End Zip: #########~
        ~  !                                                                    /* (Y2K, LDJ) */
L55270: %!Region: ##    Route: #####   Load No.: #####                   ~
        ~                                                                ~
        ~  !
                                           /* Report Detail - Col. Hdr */
L55310: %!Due Date!RC!Route!Customer !<- Customer Name -->!Sales Or!    U~
        ~nits!Make!Pull!SO Net Amt! Load!Tp!Di!Sp!Wd!Sa!Ds!Up!Pt!How Ship~
        ~  !
                                           /* Report Detail - Detail   */
L55350: %!--------!--!-----!---------!--------------------!--------!-----~
        ~----!----!----!----------!-----!--!--!--!--!--!--!--!--!--------~
        ~--!
L55380: %!########!##!#####!#########!####################!########!   ##~
        ~#.##!####!####!######.##-!#####!# !# !# !# !# !# !# !# !########~
        ~##!
                                           /* Report Detail - Totals   */
L55420: %!Total Makes : ############                                     ~
        ~  Total Pulls : ############        Total Product : ############~
        ~  !
L55450: %!Total Units : ############                                     ~
        ~                                    Total Value   : ############~
        ~  !
                                           /* Report Summary- Heading  */
L55490: %+---------------------------------------------------------------~
        ~---------------+
L55510: %!######## ########     ######################################## ~
        ~     APCPLN03: !
L55530: %!User Id: ###                                                   ~
        ~   Page: ##### !
L55550: %!Beg Due Date: ########## Beg Region: ##  Beg Route: #####  Beg ~
        ~Zip: ######### !                                                       /* (Y2K, LDJ) */
L55570: %!End Due Date: ########## End Region: ##  End Route: #####  End ~
        ~Zip: ######### !                                                       /* (Y2K, LDJ) */
L55590: %!Region: ##    Route: #####   Load No.: #####                   ~
        ~               !
                                           /* Report Detail - Col. Hdr */
L55620: %!---------------------------------------------------------------~
        ~---------------!


L55660: %!Dpt!<------ Description --------->!Windows!Sash!Part!< Make>!Pu~
        ~ll! Net Sales  !

L55690: %!---!------------------------------!-------!----!----!-------!--~
        ~--!------------!

L55720: %!###!##############################!#######!####!####!#######!##~
        ~##!########.##-!

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_hdr_sum
           if sc_load$ = "N/A" then prt_load$ = sc_load$                 ~
                               else prt_load$ = or_load$
           prt_region$ = or_region$
           prt_route$  = or_route$
           if analysis% = 0% then goto L60060
              prt_region$ = "NN"
              prt_route$  = " N/A "
L60060:    runtime$ = " "
           call "TIME" (runtime$)
           if lcntr% <> 99% then print using L55490
           print page
           pageno% = pageno% + 1%
           print using L55490
           print using L55510, date$, runtime$, title$
           print using L55530, userid$, pageno%
           print using L55550, bg_due$, bg_region$, bg_route$, bg_zip$
           print using L55570, ed_due$, ed_region$, ed_route$, ed_zip$
           print using L55590, prt_region$, prt_route$, prt_load$
           print using L55620
           print using L55660
           lcntr% = 8%
        return

        print_dtl_sum
           gosub print_hdr_sum
           for i% = 1% to dept_max%
               print using L55690
               print using L55720, cc$(i%), dd$(i%), nn%(i%,1%),          ~
                                  nn%(i%,2%), nn%(i%,3%), nn%(i%,4%),    ~
                                  nn%(i%,5%), tt(i%)
           next i%
           print using L55620
           print using L55720, "***","<----- ANALYSIS TOTALS ------>",    ~
                       nt%(1%), nt%(2%), nt%(3%), nt%(4%), nt%(5%), tt
           print using L55490
           mat nn% = zer
           mat nt% = zer
           mat tt  = zer
           tt = 0.0
        return

        print_header
           if sc_load$ = "N/A" then prt_load$ = sc_load$                 ~
                               else prt_load$ = or_load$
           prt_region$ = or_region$
           prt_route$  = or_route$
           if analysis% = 0% then goto L60270
              prt_region$ = "NN"
              prt_route$  = " N/A "
L60270:    runtime$ = " "
           call "TIME" (runtime$)
           if lcntr% <> 99% then print using L55080
           print page
           pageno% = pageno% + 1%
           print using L55080
           print using L55150, date$, runtime$, title$
           print using L55180, userid$, pageno%
           print using L55210, bg_due$, bg_region$, bg_route$, bg_zip$
           print using L55240, ed_due$, ed_region$, ed_route$, ed_zip$
           print using L55270, prt_region$, prt_route$, prt_load$
           print using L55110
           print using L55310
           lcntr% = 8%
        return

        print_detail
           if lcntr% > 57% then gosub print_header
              pt_load$ = or_load$
              if or_status$ = "99" then pt_load$ = "CHold"
                                                  /* (EWD001) - Freeze */
              if or_status$ > "89" and or_status$ < "99" then            ~
                                        pt_load$ = "Freez"
                                                  /* (EWD001) -        */
              if sav_due$ = x$ then goto L60385
                 sav_due$ = x$

L60385:       print using L55350
              print using L55380, x$      , or_region$, or_route$,       ~
                          or_cuscode$, str(desc$,1%,20%),or_so$,or_units,~
                                 or_mak%, or_pul%, or_value, pt_load$,   ~
                          str(or_special$,1%,1%), str(or_special$,2%,1%),~
                          str(or_special$,3%,1%), str(or_special$,4%,1%),~
                          str(or_special$,5%,1%), str(or_special$,6%,1%),~
                          str(or_special$,7%,1%), str(or_special$,8%,1%),~
                          how_ship$
              lcntr% = lcntr% + 2%
        return

        print_totals
           if lcntr% > 55% then gosub print_header

           convert tot_units to tot_units$, pic(#########.##)
           convert tot_make% to tot_make$,  pic(############)
           convert tot_pull% to tot_pull$, pic(############)
           convert tot_product% to tot_product$,      pic(############)
           convert tot_prod_value to tot_prod_value$, pic(#########.##)

           print using L55110
           print using L55420, tot_make$, tot_pull$, tot_product$
           print using L55450, tot_units$, tot_prod_value$
                                             /* (EWD001) - Fix       */
           tot_units = 0.0 : tot_make% = 0% : tot_pull% = 0%
           tot_product% = 0% : tot_prod_value = 0.0
        return

        generate_report                               /* S.O. Analysis */
            init(" ") or_key$, sc_load$, sav_due$, x$, sav_break$,break$
            sc_load$ = or_load$
            if analysis% = 1% then goto gen_rpt       /* Load Analysis */

            cnt% = 0%
            cnt$ = "Records Scanned [ xxxxxxxx ]"
            str(or_key$,1%,8%) = bg_dte$
            if bg_region$ = "AL" then goto generate_nxt
               str(or_key$,9%,2%) = bg_region$
            if str(bg_route$,1%,3%) = "ALL" then goto generate_nxt
               str(or_key$,11%,5%) = bg_route$
            if str(bg_zip$,1%,3%) = "ALL" then goto generate_nxt
               str(or_key$,16%,9%) = bg_zip$
        generate_nxt                              /* (APCPLNOR) - File */
          read #1,key > or_key$, using L60615, or_key$, or_so$,or_status$,~
                             or_hows$, or_load$, eod goto generate_done
L60615:         FMT CH(51), CH(8), CH(02), POS(92), CH(2), CH(5)
          cnt% = cnt% + 1%
          if mod(cnt%,100%) <> 0% then goto L60645
             convert cnt% to str(cnt$,19%,8%), pic(########)
             print at(02,02);hex(84);cnt$;

L60645:   if str(or_key$,1%,8%) > ed_dte$ then goto generate_done
          if str(bg_region$,1%,2%) = "AL" then goto L60670
             if str(or_key$,9%,2%) < bg_region$ or                       ~
                str(or_key$,9%,2%) > ed_region$ then goto generate_nxt

L60670:   if str(bg_route$,1%,3%) = "ALL" then goto L60690
             if str(or_key$,11%,5%) < bg_route$ or                       ~
                str(or_key$,11%,5%) > ed_route$ then goto generate_nxt

L60690:   if str(bg_zip$,1%,3%) = "ALL" then goto L60715
             if str(or_key$,16%,9%) < bg_zip$ or                         ~
                str(or_key$,16%,9%) > ed_zip$ then goto generate_nxt

                                            /* Otherwise Analysis% = 0%*/
L60715:      convert or_status$ to or_status%, data goto L60720
L60720:
                                            /* (EWD001) - Freeze       */
             if or_status% < 3% then goto L60725
                if or_status% < 90% then goto generate_nxt
                                            /* (EWD001) - Freeze       */

L60725:      if or_status% <> 2% then goto L60765
                                            /* No Assigned Load Data   */
                if str(sc_load$,1%,3%) = "N/A" then goto generate_nxt
                                            /* Load Must Match         */
                if or_load$ <> sc_load$ then goto generate_nxt
                                            /* Set Break Key           */
L60765:      or_region$ = str(or_key$,9%,2%)
             or_route$  = str(or_key$,11%,5%)
             str(break$,1%,2%) = or_region$
             str(break$,3%,2%) = str(or_route$,1%,2%)
             if len(sav_break$) <  4 then sav_break$ = break$
             if sel% <> 0% then generate_sum
                if sav_break$ = break$ then goto L60815
                   gosub print_totals
                   sav_break$ = break$
                   gosub print_header
L60815:         gosub dataload              /* (APCPLNOR) - File       */
                gosub lookup_how_ship
                gosub lookup_customer
                gosub print_detail
                goto generate_nxt
        generate_sum
             if sav_break$ = break$ then goto L60860
                gosub print_dtl_sum
                sav_break$ = break$
L60860:      gosub load_line_item
             goto generate_nxt
        generate_done
             if sel% = 0% then gosub print_totals                        ~
                          else gosub print_dtl_sum
             if sel% = 0% then print using L55080   /* (EWD001) - Fix  */
        return

        gen_rpt
            init(" ") sc_key1$, sav_due$, x$, sav_drop_seq$
            str(sc_key1$,1%,5%) = sc_load$
            cnt% = 0%
            cnt$ = "Records Scanned [ xxxxxxxx ]"
        gen_nxt                                   /* (APCPLNSC) - File */
          read #3,key 1% > sc_key1$, using L60935, sc_key1$,              ~
                                                        eod goto gen_done
L60935:         FMT POS(7), CH(27)
          cnt% = cnt% + 1%
          if mod(cnt%,100%) <> 0% then goto L60965
             convert cnt% to str(cnt$,19%,8%), pic(########)
             print at(02,02);hex(84);cnt$;

L60965:   if str(sc_key1$,1%,5%) <> sc_load$ then goto gen_done
          if sav_drop_seq$ <> " " then goto L60980
             goto L60985
L60980:   if sav_drop_seq$ = str(sc_key1$,6%,5%) then goto gen_nxt
L60985:      sav_drop_seq$ = str(sc_key1$,6%,5%)
                                                  /* Get info from 1st */
             or_so$ = str(sc_key1$,18%,8%)        /* Line of S.O.      */
             if sel% <> 0% then gen_sum
                gosub dataload                    /* (APCPLNOR) - File */
                gosub lookup_how_ship
                gosub lookup_customer
                gosub print_detail
                goto gen_nxt
        gen_sum
             gosub load_line_item                 /* Load Detail for   */
             goto gen_nxt                         /* All of S.O.       */
        gen_done
          if sel% = 0% then gosub print_totals                           ~
                       else gosub print_dtl_sum
          if sel% = 0% then print using L55080    /* (EWD001) - Fix    */
        return

        scan_orders
            str(or_key$,1%,8%) = bg_dte$
            if bg_region$ = "AL" then goto scan_orders_nxt
               str(or_key$,9%,2%) = bg_region$
            if str(bg_route$,1%,3%) = "ALL" then goto scan_orders_nxt
               str(or_key$,11%,5%) = bg_route$
            if str(bg_zip$,1%,3%) = "ALL" then goto scan_orders_nxt
               str(or_key$,16%,9%) = bg_zip$
        scan_orders_nxt
          read #1,key > or_key$, using L61125, or_key$, or_so$,           ~
                        or_status$, sc_load$, eod goto scan_orders_done
L61125:         FMT CH(51), CH(8), CH(02), POS(94), CH(5)
          cnt% = cnt% + 1%
          if mod(cnt%,100%) <> 0% then goto L61155
             convert cnt% to str(cnt$,19%,8%), pic(########)
             print at(02,02);hex(84);cnt$;

L61155:   if str(or_key$,1%,8%) > ed_dte$ then goto scan_orders_done
          if str(bg_region$,1%,2%) = "AL" then goto L61175
             if str(or_key$,9%,2%) < bg_region$ or                       ~
                str(or_key$,9%,2%) > ed_region$ then goto scan_orders_nxt
L61175:   if str(bg_route$,1%,3%) = "ALL" then goto L61190
             if str(or_key$,11%,5%) < bg_route$ or                       ~
                str(or_key$,11%,5%) > ed_route$ then goto scan_orders_nxt
L61190:   if str(bg_zip$,1%,3%) = "ALL" then goto L61210
             if str(or_key$,16%,9%) < bg_zip$ or                         ~
                str(or_key$,16%,9%) > ed_zip$ then goto scan_orders_nxt
        REM - Load Analysis
                                         /* (EWD001) - Cannot assign    */
                                         /*            Frozen Sales Ords*/
L61210:      if or_status$ <> "00" and or_status$ <> "01" and            ~
                or_status$ <> "02" then  goto scan_orders_nxt
             sc_drop_seq$ = "00000"
             if or_status$ <> "02" then goto L61240
                if sc_load$ <> or_load$ then goto scan_orders_nxt

L61240:      get #1, using L61250, or_units, or_value, or_cost, or_mak%,  ~
                                                               or_pul%
L61250:         FMT POS(99), 3*PD(14,4), 2*BI(2)
             gosub load_line_item                  /* Get Drop Seq No. */
             cc% = cc% + 1%
             c1$(cc%) = " "
             x% = 0%
             convert sc_drop_seq$ to x%, data goto L61280
L61280:
             if x% > drop_seq_max% then drop_seq_max% = x%

             if sc_drop_seq$ <> "00000" then c1$(cc%) = "*"
             c2$(cc%) = sc_drop_seq$                     /* Seq. Number*/
             c3$(cc%) = str(or_key$,11%,5%)              /* Route Code */
             c4$(cc%) = str(or_key$,27%,9%)              /* Customer   */
             or_cuscode$ = c4$(cc%)
             gosub lookup_customer
             c5$(cc%) = desc$                            /* Cust Name  */
             c6$(cc%) = or_so$                           /* Sales Order*/
             convert or_units to c7$(cc%), pic(###.##)   /* S.O. Units */

             convert or_value to c8$(cc%), pic(######.##-) /* S.O. Net */
                                                      /* Screen Totals */

             if xdock% = 1% then c9$(cc%) = "C"          /* (SR70889) */
             c10$(cc%) = str(or_key$,25%,2%)             /* CR2317 */
             tot_units      = round(tot_units + or_units, 2)
             tot_make%      = tot_make%    + or_mak%
             tot_pull%      = tot_pull%    + or_pul%
             tot_product%   = tot_product% + (or_mak% + or_pul%)
             tot_prod_value = round(tot_prod_value + or_value, 2)
             goto scan_orders_nxt
        scan_orders_done
           val_max% = cc%
           convert tot_units to tot_units$, pic(########.##-)
           convert tot_make% to tot_make$, pic(############)
           convert tot_pull% to tot_pull$, pic(############)
           convert tot_product% to tot_product$, pic(############)
           convert tot_prod_value to tot_prod_value$, pic(########.##-)
           kk% = 0%
        return

        lookup_customer
           desc$ = " "
           read #4,key = or_cuscode$, using L61450, desc$,eod goto L61455
L61450:       FMT POS(253), CH(25)
L61455: return

        load_line_item                            /* (APCPLNSC) - File */
           init(" ") sc_key$                      /* Totals for 'All'  */
           xdock% = 0%                            /* (SR70889) */
           str(sc_key$,1%,8%) = or_so$            /* Line Items on the */
        load_line_next                            /* Sales Order.      */
           read #3,key > sc_key$, using L61495, sc_key$,                  ~
                                               eod goto load_line_done
L61495:       FMT POS(24), CH(10)
           if str(sc_key$,1%,8%) <> or_so$ then return
              sc_so$   = str(sc_key$,1%,8%)
              sc_line$ = str(sc_key$,9%,2%)
              get #3, using L61525, sc_drop_seq$, sc_mqty%, sc_pqty%,     ~
                                     sc_pqty1%, dollars, sc_special$
L61525:         FMT POS(12), CH(5), POS(70), 3*BI(2), PD(14,4), POS(118),~
                    CH(10)

/* (SR70889) */
              init(" ") so_inv$, item_no$
              so_inv$  = str(sc_key$,1%,8%)
              item_no$ = str(sc_key$,9%,2%)
              gosub lookup_sub_part
              if invplant$ <> mfgplant$ then xdock% = 1%
/* (SR70889\) */
              gosub scan_dtl                      /* Line Item Totals  */
              goto load_line_next                 /* Assoc. Dept/Prod  */
        load_line_done
        return

        scan_dtl                            /* Department Detail for a */
                                            /* Sales Order Line Item   */
           tt% = 1%                                        /* Window   */
           if str(sc_special$,10%,1%) = "Y" then tt% = 2%  /* Sash     */
           if str(sc_special$,8%,1%)  = "Y" then tt% = 3%  /* Part     */
           init(" ") sd_key$
           str(sd_key$,1%,8%) = sc_so$      /* (APCPLNSD) - Sched File */
           str(sd_key$,9%,2%) = sc_line$
        scan_dtl_nxt
           read #6,key > sd_key$,using L61610, sd_key$, eod goto L61875
L61610:       FMT CH(23)
           if str(sd_key$,1%,8%) <> sc_so$ then goto scan_dtl_done
           if str(sd_key$,9%,2%) <> sc_line$ then goto scan_dtl_done
                                                   /* (AWD009) - change POS */
              sc_dept$ = str(sd_key$,12%,3%)       /* Only Look at 1st */
              str(sd_key$,15%,2%) = "99"           /* Skip the Others  */
              sav_model$ = str(sd_key$,19%,3%)     /* Store Model Code */
              for i% = 1% to dept_max%             /* Dept Subsript(I%)*/
                  if cc$(i%) = sc_dept$ then goto L61655
              next i%
L61655:       for s% = 1% to sp_max%               /* Support Subscript*/
                  if sp$(s%) = sc_dept$ then goto L61685       /* (S%)  */
              next s%
              s% = 0%
        REM - Department Totals                    /* Make Totals For  */
                                                   /* All Departments  */
L61685:
        REM if i% = m102% or i% = m104% then goto L61810
            if i% = m102% then goto L61810
                                          /* Window, Sash, Part Totals */
                 nn%(i%,tt%) = nn%(i%,tt%) + sc_mqty%
                                          /* Department Make Totals    */
                 nn%(i%,4%)  = nn%(i%,4%)  + sc_mqty%
                                          /* Totals for all Dept's for */
                                          /* Windows,Sash's, and Part's*/
                 nt%(tt%)    = nt%(tt%) + sc_mqty%
                 if s% <> 0% then goto L61810    /* Skip Support Dept's */
                                          /* Dept Display Totals       */
                    nt%(4%)     = nt%(4%)  + sc_mqty%
                    nn%(i%,5%)  = nn%(i%,5%)  + sc_pqty% + sc_pqty1%
                    tt(i%)      = round(tt(i%) + dollars, 2)
                                          /* Dept Display Screen Totals*/
                    nt%(5%)     = nt%(5%)  + (sc_pqty% + sc_pqty1%)
                    tt = round(tt + dollars, 2)
                 if sc_pqty% = 0% then goto L61785
                                          /* Finished Inventory Totals */
                    nn%(m102%,tt%) = nn%(m102%,tt%) + sc_pqty%
                    nn%(m102%,5% ) = nn%(m102%,5% ) + sc_pqty%
L61785:          if sc_pqty1% = 0% then goto L61810
                                          /* Shape Inventory Totals    */
                                          /* (EWD004)                  */
                    nn%(m104%,tt%) = nn%(m104%,tt%) + sc_pqty1%
                    nn%(m104%,5% ) = nn%(m104%,5% ) + sc_pqty1%

L61810: REM - Product Totals by Department
              for k% = 1% to pm%(i%)
                if pp$(i%,k%) = sav_model$ then goto L61840
              next k%
                np%(m100%,k%) = np%(m100%,k%) + sc_mqty%
                goto scan_dtl_nxt
L61840:       np%(i%,k%) = np%(i%,k%) + sc_mqty%        /* Count Makes */
              if s% <> 0% then goto scan_dtl_nxt        /* Skip Support*/
                 if sc_pqty%  <> 0% then                /* Pull Stock  */~
                                np%(m102%,k%) = np%(m102%,k%) + sc_pqty%
                                                        /* (EWD004)    */
                 if sc_pqty1% <> 0% then                /* Pull Shapes */~
                                np%(m104%,k%) = np%(m104%,k%) + sc_pqty1%
              goto scan_dtl_nxt
L61875: scan_dtl_done
        return

        scan_loads                                /* (APCPLNSC) - File */
            init(" ") sc_key1$, sav_drop_seq$     /* Note - All Status */
            str(sc_key1$,1%,5%) = or_load$        /* Codes Assoc. with */
        scan_loads_nxt                            /* S.O. Are Retrieved*/
          init(" ") or_route$, or_cuscode$, or_so$
          or_units = 0.0 : or_value = 0.0 : or_cost = 0.0
          or_mak%  = 0%  : or_pul%  = 0%
          read #3,key 1% > sc_key1$, using L61935, sc_key1$,              ~
                                                eod goto scan_loads_done
L61935:         FMT POS(7), CH(27)
          cnt% = cnt% + 1%
          if mod(cnt%,100%) <> 0% then goto L61965
             convert cnt% to str(cnt$,19%,8%), pic(########)
             print at(02,02);hex(84);cnt$;

L61965:   if str(sc_key1$,1%,5%) <> or_load$ then goto scan_loads_done
          if sav_drop_seq$ = str(sc_key1$,6%,5%) then goto scan_loads_nxt
             sav_drop_seq$ = str(sc_key1$,6%,5%)

             or_so$   = str(sc_key1$,18%,8%)

             gosub load_line_item                /* Do Detail Analysis */

        REM - Load Analysis
             read #1,key 4% = or_so$, using L62025, or_route$, or_drop$,     ~
                                   or_cuscode$, or_units, or_value, or_cost, ~
                                   or_mak%, or_pul%, eod goto L62030
L62025:        FMT POS(11),CH(5),POS(25),CH(02),CH(9),POS(99),3*PD(14,4),2*BI(2)
L62030:      cc% = cc% + 1%
             convert (cc%*10%) to sc_drop_seq$, pic(00000)

             c1$(cc%) = " "
             c2$(cc%) = sc_drop_seq$                     /* Seq. Number*/
             c3$(cc%) = or_route$                        /* Route Code */
             c4$(cc%) = or_cuscode$                      /* Customer   */
             gosub lookup_customer
             c5$(cc%) = desc$                            /* Cust Name  */
             c6$(cc%) = or_so$                           /* Sales Order*/
             convert or_units to c7$(cc%), pic(###.##)   /* S.O. Units */

             convert or_value to c8$(cc%), pic(######.##-) /* S.O. Net */
                                                      /* Screen Totals */
             if xdock% = 1% then c9$(cc%) = "C"       /* add with CR2317 here */
             c10$(cc%) = or_drop$                     /* CR2317 */                                                      
             tot_units      = round(tot_units + or_units, 2)
             tot_make%      = tot_make%    + or_mak%
             tot_pull%      = tot_pull%    + or_pul%
             tot_product%   = tot_product% + (or_mak% + or_pul%)
             tot_prod_value = round(tot_prod_value + or_value, 2)
             goto scan_loads_nxt
        scan_loads_done
           val_max% = cc%
           convert tot_units to tot_units$, pic(########.##-)
           convert tot_make% to tot_make$, pic(############)
           convert tot_pull% to tot_pull$, pic(############)
           convert tot_product% to tot_product$, pic(############)
           convert tot_prod_value to tot_prod_value$, pic(########.##-)
           kk% = 0%
           drop_seq_max% = (cc% * 10%)
        return

        process_data
            call "SHOSTAT" ("Processing Include/Exclude Data")
            delete_flag% = 0%
            cnt% = drop_seq_max%
            or_status$ = "02"
            str(or_date$,1%,6%) = date
            str(or_date$,7%,2%) = "  "
            or_dte$ = or_date$
            or_drop$ = "00"
            for i% = 1% to val_max%
                                              /* Include '*' Skip ' '  */
                if keyhit% =  9% and c1$(i%) = " " then goto L62295
                                              /* Include ' ' Skip '*'  */
                if keyhit% = 10% and c1$(i%) <> " " then goto L62295

                   sc_drop_seq$ = c2$(i%)        /* Selected Drop Seq. */
                   if sc_drop_seq$ <> "00000" then goto L62285
                      cnt% = cnt% + 10%
                      convert cnt% to sc_drop_seq$, pic(00000)

L62285:            or_so$ = c6$(i%)              /* Selected S.O.      */
                   gosub update_header
L62295:     next i%
        return

        update_header
            read #1,hold,key 4% = or_so$,using L35350, or_rec$,           ~
                                                           eod goto L62410
               if delete_flag% = 0% then goto L62355
           /* (AWD010)  */
          if str(or_rec$,60%,2%) > "19" then goto delete_error

                  if str(or_rec$,60%,2%) < "03" then goto L62355
                     gosub delete_dtl
                     if keyhit% = 32% then gosub delete_bck /* (EWD003)  */
                     if keyhit% = 32% then gosub delete_ewdsched /* (AWD013)*/
                     read #1,hold,key 4% = or_so$,using L35350, or_rec$,  ~
                                                           eod goto L62410

L62355:        delete #1
            if keyhit% = 7% or keyhit% = 11% then  /* RE-SEQUENCE */     ~
                                        or_status$ = str(or_rec$,60%,2%)

REM - Don not rewrite 'OR' or 'SC' if cancel order!!
/*(AWD012) - save data */
            awdplndl_status$ = str(or_rec$,60%,2%)
            awdplndl_load$   = str(or_rec$,94%,5%)


            if keyhit% = 32% then goto L62405           /* (EWD003)    */
            str(or_rec$,25%,2%) = or_drop$              /* Drop Number */
            str(or_rec$,94%,5%) = or_load$              /* Load Number */
            str(or_rec$,60%,2%) = or_status$
            str(or_rec$,62%,8%) = or_dte$
            put #1, using L35350, or_rec$
               write #1, eod goto L62415
L62405:     gosub update_detail
L62410: return
L62415:     errormsg$ = "(Error) - Updating Header (APCPLNOR) " &or_key$
            gosub error_prompt
            init(" ") errormsg$
            goto L62405

        update_detail
            init(" ") sc_key$
            str(sc_key$,1%,8%) = or_so$
        update_detail_nxt
            read #3,hold,key > sc_key$, using L35620, sc_rec$,            ~
                                                     eod goto L62535
            if str(sc_rec$,24%,8%) <> or_so$ then return
               sc_key$ = str(sc_rec$,24%,10%)
               delete #3

            if keyhit% = 32% then goto L62530           /* (EWD003)    */
            str(sc_rec$,7%,5%)   = or_load$      /* Curr Assigned Load */
            str(sc_rec$,12%,5%)  = sc_drop_seq$  /* Sched Drop Seq.    */
            str(sc_rec$,17%,2%)  = or_drop$      /* Sched Drop No.     */
            str(sc_rec$,105%,5%) = or_load$      /* Orig. Parent Load  */
            str(sc_rec$,110%,2%) = or_status$    /* Status For Select  */
            str(sc_rec$,112%,6%) = str(or_dte$,1%,6%)    /* Stat Date  */
            put #3, using L35620, sc_rec$
REM            gosub update_dtholdfl_sc
            write #3, eod goto L62540
L62530:     gosub update_pull_load

            gosub update_surge                       /*  (EWD008)  */
            goto update_detail_nxt
L62535: return
L62540:     errormsg$ = "(Error) - Updating Line (APCPLNSC) " &sc_key$
            gosub error_prompt
            init(" ") errormsg$
            goto update_detail_nxt

        delete_orders
            call "SHOSTAT" ("Deleting Selected Sales Orders")

            gosub delete_load_order                /* (EWD003)         */

            delete_flag% = 1%                      /* Set the Delete   */
            or_status$ = "01"                      /* Flag, Do Not Del */
            str(or_date$,1%,6%) = date             /* S.O. with Status */
            str(or_date$,7%,2%) = "  "             /* Greater than '02'*/
            or_dte$             = or_date$
            or_drop$            = "00"
            sc_drop_seq$        = "00000"
            or_load$            = "99999"
/*(AWD012) */
            init(" ") awdplndl_status$, awdplndl_load$
            for i% = 1% to val_max%
                                              /* Delete  '*' Skip ' '  */
                if c1$(i%) = " " then goto L62645
                   or_so$ = c6$(i%)              /* Selected S.O.      */
                                                 /* (EWD003)           */
                   or_cuscode$ = c4$(i%)         /* Selected Customer  */
                   gosub update_header

/* (AWD012) - log deleted order */
                   gosub update_awdplndl


L62645:     next i%
            delete_flag% = 0%
            gosub check_warranty_file
        return


        check_warranty_file                      /* (EWD007)           */
             init(" ") bar_key$
             read #19, hold, key > bar_key$, eod goto no_oracle_update

                gosub oracle_connect
                     if oci_err% >= 0% then goto L63250
                        gosub oracle_no_connect
                        return
        check_warranty_next
             read #19, hold, eod goto no_oracle_update

L63250:         get #19, using L62650, bar_key$
                gosub update_oracle
                  delete #19
                  goto check_warranty_next
        no_oracle_update
        return
                                                  /*   (EWD007)          */


        re_seq_load
            call "SHOSTAT" ("Assign Drops/Re-Seq Load")

            str(or_date$,1%,6%) = date
            str(or_date$,7%,2%) = "  "
            or_dte$ = or_date$
            or_drop$            = "00"          /* Load was Previously */
            for i% = 1% to val_max%             /* loaded by selection */
                sc_drop_seq$ = c2$(i%)          /* From Input Screen   */
                or_so$       = c6$(i%)          /* Selected S.O.       */
                gosub update_detail             /* Update Load for all */
            next i%                             /* Changes Made in     */
            cc% = 0% : val_max% = 0%            /* (APCPLNSC) - File   */
            init(" ") c2$(), c4$(), c6$(), sc_key1$, sav_drop_seq$,      ~
                      sav_cuscode$
            str(sc_key1$,1%,5%) = or_load$      /* Clean-Up Seq No's in*/
        REM - Start of Loop                     /* Specified Load      */
L62750:     read #3,key 1% > sc_key1$,using L62760, sc_key1$, sc_cuscode$,~
                                                   eod goto L62810
L62760:        FMT POS(7), CH(27), XX(25), CH(9)
            if str(sc_key1$,1%,5%) <> or_load$ then goto L62810
            if sav_drop_seq$ = str(sc_key1$,6%,5%) then goto L62750
               sav_drop_seq$ = str(sc_key1$,6%,5%)
                                                /* Clean-Up Sort Order */
               cc% = cc% + 1%                   /* In Memory, New No's */
               convert (cc% * 10%) to c2$(cc%), pic(00000)
               c4$(cc%) = sc_cuscode$
               c6$(cc%) = str(sc_key1$,18%,8%)
               goto L62750
L62810: REM - END of Loop
            val_max% = cc%                      /* Assign New Drops and*/
            or_drop% = 0%                       /* Update all S.O's on */
            for i% = 1% to val_max%             /* Specified Load.     */
                sc_drop_seq$ = c2$(i%)
                or_so$       = c6$(i%)
                if sav_cuscode$ = c4$(i%) then goto L62865
                   sav_cuscode$ = c4$(i%)
                   or_drop% = or_drop% + 1%
           /*(AWD011)*/
           if or_drop% > 99% then or_drop% = 99%

                   convert or_drop% to or_drop$, pic(00)

L62865:         gosub update_header
            next i%
        return

        re_seq_route
            call "SHOSTAT" ("Assign Drops/Re-Seq Load by Route")
            mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work
            cnt% = 0%
            str(or_date$,1%,6%) = date
            str(or_date$,7%,2%) = "  "
            or_dte$ = or_date$
            or_drop$            = "00"          /* New Load            */
            for i% = 1% to val_max%             /* loaded by selection */
                cnt% = cnt% + 1%
                init(" ") wrk$
                str(wrk$,1%,5%)  = c3$(i%)      /* Route Code          */
                str(wrk$,6%,9%)  = c4$(i%)      /* Customer Code       */
                str(wrk$,15%,8%) = c6$(i%)      /* S.O. Number         */
                convert cnt% to str(wrk$,23%,5%), pic(00000)
REM     gosub update_dtholdfl_sc  /* AWD000 */
                write #7, using L62970, wrk$, eod goto L62975
L62970:            FMT CH(64)
L62975:     next i%                             /* Changes Made in     */
            cc% = 0% : val_max% = 0%            /* (APCPLNSC) - File   */
            init(" ") wrk$, c2$(), c4$(), c6$(), sav_cuscode$
        REM - Start of Loop                     /* Specified Load      */
L62995:     read #7,key > wrk$, using L62970, wrk$, eod goto L63030
               FMT CH(5), CH(9), CH(8)
               cc% = cc% + 1%                   /* In Memory, New No's */
               convert (cc% * 10%) to c2$(cc%), pic(00000)
               c4$(cc%) = str(wrk$,6%,9%)
               c6$(cc%) = str(wrk$,15%,9%)
               goto L62995
L63030: REM - END of Loop
            val_max% = cc%                      /* Assign New Drops and*/
            or_drop% = 0%                       /* Update all S.O's on */
            for i% = 1% to val_max%             /* Specified Load.     */
                sc_drop_seq$ = c2$(i%)
                or_so$       = c6$(i%)
                if sav_cuscode$ = c4$(i%) then goto L63085
                   sav_cuscode$ = c4$(i%)
                   or_drop% = or_drop% + 1%
                   convert or_drop% to or_drop$, pic(00)

L63085:         gosub update_header
            next i%
            gosub delete_work
        return


        build_change
         sav_load$ = or_load$
         opn% = 1% : err% = 0%
         call "APCPLN7B" (opn%,          /* Value = 1% Skip Open       */~
                          sav_load$,     /* Load Number Build/Change   */~
                          err% )         /* 0 = OK, 1 = ERROR          */
        return

        load_dept
            call "SHOSTAT" ("Loading Department Data")
                                                 /* Load Departments   */
            init(" ") readkey$, cc$(), dd$()     /* for a Product Anal-*/
            i% = 0% : m102% = 1% : m104% = 1%    /* ysis by Department */
                                                 /* (EWD004)           */
            dept_max% = 1%
            str(readkey$,1%,9%) = "PLAN DEPT"
        load_dept_nxt
            read #2,key > readkey$, using L63205, readkey$, desc$,        ~
                                                 eod goto load_dept_done
L63205:        FMT CH(24), CH(20)
            if str(readkey$,1%,9%) <> "PLAN DEPT" then                   ~
                                                  goto load_dept_done
            i% = i% + 1%
            if i% > 48% then i% = 48%          /* Max Set to 48 Dept's */
            cc$(i%) = str(readkey$,10%,3%)     /* Set Dept Code Value  */
            dd$(i%) = desc$                    /* Set Dept Description */
            if cc$(i%) = "100" then m100% = i% /* Overflow             */
            if cc$(i%) = "102" then m102% = i% /* Set Finished Goods   */
            if cc$(i%) = "104" then m104% = i% /* Set Special ShapeStock*/
                                               /* (EWD004)             */
            goto load_dept_nxt
        load_dept_done
           dept_max% = i%                      /* Set Max Departments  */
           gosub load_product                  /* Load Prod for Dept's */
           gosub load_support                  /* Load Support Dept's  */
        return

        load_product
           init(" ") pp$()
           for i% = 1% to dept_max%            /* Now Find Max Prod. in*/
             init(" ") pl_key$, sav_key$       /* each Department      */
             pp% = 0%
             str(pl_key$,1%,3%) = cc$(i%)      /* Department Code      */
             str(pl_key$,4%,2%) = "01"         /* Process Code         */
             str(pl_key$,6%,2%) = "01"         /* Shift Code           */
             sav_key$ = str(pl_key$,1%,7%)
        load_prod_nxt
             read #8,key > pl_key$, using L63345, pl_key$,eod goto L63400
L63345:         FMT POS(11), CH(12)
             if sav_key$ <> str(pl_key$,1%,7%) then goto L63400
                if pp% = 0% then goto L63380
                for kk% = 1% to pp%
                    if pp$(i%,kk%) = str(pl_key$,8%,3%) then             ~
                                                      goto load_prod_nxt
                next kk%
L63380:         pp% = pp% + 1%
                if pp% > 192% then pp% = 192%
                pp$(i%,pp%) = str(pl_key$,8%,3%)
             goto load_prod_nxt
L63400:      if pp% = 0% then pp% = 1%
             pm%(i%) = pp%       /* Save Max Products for a Department */
           next i%
        return

        lookup_model
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "MODEL    "
            str(readkey$,10%,15%) = sav_model$
            read #2,key = readkey$,using L63450, desc$, eod goto L63455
L63450:        FMT POS(25), CH(20)
L63455: return
        lookup_how_ship
            init(" ") readkey$, desc$, how_ship$
            if or_hows$ = "00" then return           /* Skip Our Truck */
            str(readkey$,1%,9%)   = "PLAN HOWS"
            str(readkey$,10%,15%) = or_hows$
            read #2,key = readkey$,using L63490, desc$, eod goto L63505
L63490:        FMT POS(25), CH(20)
            str(how_ship$,1%,3%) = or_hows$ & "-"
            str(how_ship$,4%,8%) = desc$
L63505: return

        load_support
           init(" ") sp$(), readkey$ : ss% = 0%
           str(readkey$,1%,9%) = "PLAN SUPP"
        load_supp_nxt
             read #2,key > readkey$, using L63545, readkey$,              ~
                                                  eod goto load_supp_done
L63545:         FMT CH(24)
             if str(readkey$,1%,9%) <> "PLAN SUPP" then                  ~
                                                      goto load_supp_done
                ss% = ss% + 1%
                sp$(ss%) = str(readkey$,10%,3%)
             goto load_supp_nxt
        load_supp_done
           sp_max% = ss%               /* Save Max Support Departments */
        return
                                                           /* (EWD002)  */
        open_error
           comp% = 2%
           hdr$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                           /* (EWD002)  */

                                                           /* (EWD003)  */
        delete_load_order
           gosub oracle_connect
           comp% = 2%
           hdr$  = "**Delete Order off Load or Cancel Order**"
           msg$(1%) = " - To Delete order off LOAD Press F6 - - - "
           msg$(2%) = "                                           "
           msg$(3%) = "Press Enter To Continue.                   "
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
           if comp% <> 6% then goto delete_order_no
/* <AWD014> Eliminate ability to cancel orders in planning */
REM        msg$(2%) = " - To Cancel ENTIRE Order Press PF32 !! - "
REM        msg$(3%) = "Press Enter or PF32 To Continue."
REM        call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
REM
REM        if comp% <> 32% then return
REM           call "EWDPLB58" (switch%)
REM           if switch% = 0% then goto delete_order_no  /*Did not type 'YES'*/
REM           keyhit% = 32%
            call "SHOSTAT" ("Deleting Order off Load.      ")
/* </AWD014>  */
        return
        delete_order_no
        return clear all
        goto inputmode
                                                           /* (EWD003)  */

        /* (AWD010) */
    delete_error
           comp% = 2%
           hdr$  = "**Error Can Not Delete Sales Order **"
           msg$(1%) = " - Order already in Invoicing         - - "
           msg$(2%) = or_so$
           msg$(3%) = "Press Enter.                              "
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return                   /* return to where update_header called */
    /* (AWD010) */

        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#7,mode$, 500%, f2%)
            if f2% <> 0% then goto L63680
        return
L63680:     errormsg$ = "(Error) Cannot Open Work File?? "
            gosub error_prompt
            init(" ") errormsg$
        return
        delete_work
            call "FILEBGON" (#7)
        return

        update_pull_load
            init(" ") pull_key$
            str(pull_key$,1%,10%) = str(sc_rec$,24%,10%)
            read #10,hold,key = pull_key$, using L63745, pull_rec$,       ~
                                                        eod goto L63770
L63745:        FMT CH(64)
            delete #10                                  /* (EWD004)    */

            if keyhit% = 32% then return                /* (EWD003)    */
            str(pull_rec$,51%,5%) = str(sc_rec$,7%,5%)
            put #10, using L63745, pull_rec$
            write #10, eod goto L63775
L63770: return
L63775:     errormsg$ = "(Error) - Updating Stock Pull Record - " &      ~
                                                       pull_key$
            gosub error_prompt
            init(" ") errormsg$
        return

        update_surge                                     /*  (EWD008)  */
              init(" ") sc_part$
              inv_qty% = 0%
              get sc_rec$ using L63800, sc_part$, inv_qty%

L63800:           FMT POS(34), CH(25), POS(72), BI(2)

              if inv_qty% <= 0% then return

              call "AWDSURGE" (sc_part$, inv_qty%)

        return                                           /* (EWD008)  */


        delete_dtl                                 /*(APCPLNDT) -File  */
            or_hows% = 50%
            or_hows$ = str(or_rec$,92%,2%)
            convert or_hows$ to or_hows%, data goto L63830
            atlstatus$ = "98"                          /* PR Proj */
            pgmname$   = "APCPLN03"                    /* PR Proj */
            init (" ") prev_so_line$ 
L63830:
            init(" ") dt_key$, dt_bar$, dt_dept$, dt_proc$, dt_shft$,     ~
                      ef_part$
            str(dt_key$,1%,8%) = or_so$            /* Sales Order      */
        delete_dtl_nxt
REM  ???    read #9,hold,key > dt_key$, using L63865, dt_key$, dt_st$
            read #9,key > dt_key$, using L63865, dt_key$, dt_st$,    ~
                             dt_shft$, dt_upmh, ef_part$, dt_yr$, dt_wk$, ~
                             dt_day$, eod goto delete_dtl_done
                                                    /*   (EWD005)        */
L63865:        FMT POS(24), CH(23), POS(64), CH(2), POS(104), CH(2),     ~
                   POS(181), PD(14,4), CH(25), POS(230), 3*CH(2)
            if or_so$ <> str(dt_key$,1%,8%) then goto delete_dtl_done
/* ???????? */
            read #9, key = dt_key$, using L63866, dt_rec$,          ~
               eod goto delete_dtl_done
L63866:     FMT  CH(256)
            gosub delete_dtholdfl
            if schema% = 2% then gosub atlas_trigger  /* CR Proj */     
            read #9, hold, key = dt_key$, using L63866, dt_rec$,          ~
               eod goto delete_dtl_done
               delete #9
               dt_bar$  = str(dt_key$,1%,18%)
               dt_dept$ = str(dt_key$,19%,3%)
               dt_proc$ = str(dt_key$,22%,2%)
               gosub update_capacity
               gosub delete_audit
               gosub update_barcode                    /*   (EWD007)   */
               goto delete_dtl_nxt
        delete_dtl_done
        return

        atlas_trigger
          dt_so$   = str(dt_rec$,24%,8%)
          dt_line$ = str(dt_rec$,32%,2%)
          dt_cust$ = str(dt_rec$,124%,9%)  
          if prev_so_line$ = str(dt_rec$,24%,10%) then goto L63900
          call "APCORLNS" (dt_cust$, dt_so$, dt_line$, atlstatus$, ~
                           pgmname$, #23, error%)
          error% = 0%
          prev_so_line$ = str(dt_rec$,24%,10%)
L63900:        
        return
        
        update_capacity                           /* (APCPLNUC) - File */
            if dt_dept$ = "095" or dt_dept$ = "100" or dt_dept$ = "102"  ~
                                or dt_dept$ = "104" then return
                                                  /* (EWD004)          */
            if str(hows$,or_hows%,1%) = "*" then return /*Skip Capacity*/

            init(" ") pl_key$
            str(pl_key$,1%,2%)  = dt_yr$
            str(pl_key$,3%,2%)  = dt_wk$
            str(pl_key$,5%,2%)  = dt_shft$
            str(pl_key$,7%,3%)  = dt_dept$
            str(pl_key$,10%,2%) = dt_proc$
            read #15,hold,key = pl_key$, using L63995, pl_unts(),         ~
                                pl_unta%(), pl_unte%(),                   ~
                                             eod goto update_capacity_done
                                                           /*  (EWD005)  */
L63995:        FMT POS(31), 7*PD(14,4), 7*BI(2), 7*BI(2)
            gosub calc_eff                           /*   (EWD005)       */
            dt_day% = 1%
            convert dt_day$ to dt_day%, data goto L64010
L64010:
            pl_unts(dt_day%)  = round(pl_unts(dt_day%) + dt_upmh, 2)
            pl_unta%(dt_day%) = pl_unta%(dt_day%) - 1%
            pl_unte%(dt_day%) = round(pl_unte%(dt_day%) - ef_unit,0)
            if pl_unte%(dt_day%) < 0% then pl_unte%(dt_day%) = 0%
                                                    /*   (EWD005)       */
            put #15, using L63995, pl_unts(), pl_unta%(), pl_unte%()
            rewrite #15
        update_capacity_done
        return

        delete_audit                              /* (APCPLNAD) - File */
            init(" ") ad_key$
            str(ad_key$,1%,10%) = str(dt_bar$,1%,10%)
        delete_audit_nxt
            read #11,hold,key 1% > ad_key$, using L64080, ad_key$,        ~
                                                        eod goto L64100
L64080:        FMT CH(33)
            if str(ad_key$,1%,18%) <> dt_bar$ then return
               delete #11
               goto delete_audit_nxt
L64100: return

        update_barcode                                   /*  (EWD007)   */
             if str(bar_key$,1%,18%) = str(dt_bar$,1%,18%) then return

             init(" ")bar_key$
             str(bar_key$,1%,18%) = str(dt_bar$,1%,18%)

             read #19, hold, key = bar_key$, eod goto no_barcode
                  delete #19
        no_barcode

             put #19, using L62650, bar_key$

L62650:           FMT CH(18)

             write #19


        return                                           /*  (EWD007)   */

        delete_bck
            init(" ") mst_key$, bck_so$
            str(mst_key$,1%,9%) = or_cuscode$
            str(mst_key$,10%,16%) = or_so$

            read #16,hold,key = mst_key$, using L64110, bck_so$,          ~
                                                       eod goto L64130

L64110:         FMT XX(9), CH(16)
                 delete #16

L64130:     gosub delete_lne
        return

        delete_lne
            init(" ") lne_key$
            str(lne_key$,1%,16%) = bck_so$

        delete_lne_next
            read #17,hold key > lne_key$, using L64120, lne_key$,         ~
                                                       eod goto delete_lne_done


L64120:         FMT XX(9), CH(19)

               if str(lne_key$,1%,16%) > bck_so$ then goto delete_lne_done
               if str(lne_key$,1%,16%) <> bck_so$ then goto delete_lne_next
                  delete #17

                     goto delete_lne_next
        delete_lne_done
        return

/* (AWD013) */
        delete_ewdsched
             init(" ") ewdsched_key$, ewdsched_sav$
             str(ewdsched_key$,1%,8%) = or_so$
             str(ewdsched_sav$,1%,8%) = ewdsched_key$

         ewdsched_next
             read #21, hold, key 2% > ewdsched_key$, using ewdsched_fmt, ~
                               ewdsched_key$, eod goto ewdsched_done

ewdsched_fmt:         FMT POS(27), CH(16)

                  if str(ewdsched_key$,1%,8%) <> str(ewdsched_sav$,1%,8%) ~
                                          then goto ewdsched_done

                      delete #21

                      goto ewdsched_next
        ewdsched_done
        return
      /* (/AWD013) */

        calc_eff                                 /* (EWD005)           */
           ef_unit = 0.00
           tqty% = 1%
           ef_err% = 0%


           call "APCPLNEF" ( ef_part$,   /* Part Number                */~
                             tqty%,      /* Number of planning units IN*/~
                             ef_unit,    /* Number of effective unitOUT*/~
                             ef_err%,    /* Error Code              OUT*/~
                             #3)         /* FILE = (GENCODES)          */

           if ef_err% <> 0% then ef_unit = 0.00

        return                                   /* (EWD005)           */

        update_dtholdfl_sc
    return
    /* don't update with apcplnsc, keep code in case it changes */
        init(" ") dt_rec$
        put zero$,using zerofmt, 0.00
zerofmt:    FMT PD(14,4)
            str(dt_rec$,01,05) = str(sc_rec$,07,05)   /* Load         */
            str(dt_rec$,06,05) = str(sc_rec$,12,05)   /* Drop Seq     */
            str(dt_rec$,11,02) = str(sc_rec$,17,02)   /* Drop No      */
            str(dt_rec$,13,05) = str(sc_rec$,19,05)   /* Cust Sort    */
            str(dt_rec$,18,02) = str(sc_rec$,32,02)   /* Line Item    */
            str(dt_rec$,20,04) = str(sc_rec$,07,05)   /* Item No      */
            str(dt_rec$,24,08) = str(sc_rec$,24,08)   /* Sale         */
            str(dt_rec$,32,02) = str(sc_rec$,32,02)   /* Line Item    */
            str(dt_rec$,34,04) = str(sc_rec$,07,05)   /* Item No      */
            str(dt_rec$,38,04) = "0000"               /* Sale         */
            str(dt_rec$,42,03) = str(sc_rec$,07,05)   /* Dept         */
            str(dt_rec$,45,02) = str(sc_rec$,07,05)   /* Proc         */
            str(dt_rec$,47,06) = str(sc_rec$,01,06)   /* Prod Date    */
            str(dt_rec$,53,06) = str(sc_rec$,01,06)   /* Date Stamp   */
            str(dt_rec$,59,03) = str(sc_rec$,07,05)   /* Dept         */
            str(dt_rec$,62,02) = str(sc_rec$,07,05)   /* Proc         */
            str(dt_rec$,64,02) = str(sc_rec$,110,2)   /* Status       */
            str(dt_rec$,66,30) = "                  " /* Index        */
            str(dt_rec$,96,08) = "        "           /* Ref          */
            str(dt_rec$,104,02) = "00"                /* PLAN SHFT    */
            str(dt_rec$,106,05) = str(sc_rec$,19,05)   /* Sort         */
            str(dt_rec$,111,05) = "00000"              /* Prod Seq No  */
            str(dt_rec$,116,05) = "00000"              /* Time         */
            str(dt_rec$,124,09) = str(sc_rec$,59,09)   /* Cust         */
            str(dt_rec$,133,08) = str(sc_rec$,24,08)   /* Sale         */
            str(dt_rec$,141,08) = zero$                /* Mat Cost     */
            str(dt_rec$,149,08) = zero$                /* Labor Cost   */
            str(dt_rec$,157,08) = zero$                /* Overhead     */
            str(dt_rec$,165,08) = zero$                /* Freight      */
            str(dt_rec$,173,08) = zero$                /* Vinyl Disc   */
            str(dt_rec$,181,08) = zero$                /* UPMH         */
            str(dt_rec$,189,25) = str(sc_rec$,34,25)   /* Part         */
            str(dt_rec$,214,01) = "0"                  /* Sash         */
            str(dt_rec$,215,01) = "N"                  /* Mfg Part     */
            str(dt_rec$,216,01) = "0"                  /* Sample       */
            str(dt_rec$,217,03) = "000"                /* Wood Surround*/
            str(dt_rec$,220,10) = str(sc_rec$,118,10)   /* Special      */
            str(dt_rec$,230,02) = "0000"               /* PROD YEAR    */
            str(dt_rec$,232,02) = "00"                 /* Prod Week    */
            str(dt_rec$,234,02) = "00"                 /* Prod Day     */
            str(dt_rec$,236,04) = str(sc_rec$,100,4)   /* Text (BI)    */
            str(dt_rec$,240,01) = "0"                  /* Group Code   */
            str(dt_rec$,241,02) = "00"                 /* Config Code  */
            str(dt_rec$,243,02) = "00"                 /* Private Lbl C*/
            str(dt_rec$,245,12) = "            "       /* Filler       */
/* <AWD014> */
        check_dtholdfl
           read #13, hold, key > dt_key$, eod goto empty_holdfl
           comp% = 2%
           hdr$  = "***** (Warning) (Warning) *****"
           msg$(1%) = " - - - - W a r n i n g - - - -"
           msg$(2%) = "Unprocessed data from last run!"
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
empty_holdfl: return
/* </AWD014> */

        update_dtholdfl
        dt_bar$ = str(dt_rec$,24,18)
        dt_key$ = str(dt_rec$,24,23)
        read #13, hold, key = dt_key$, eod goto skip_delete
            delete #13
skip_delete: write #13, using L63866, dt_rec$
        dt_bar$ = str(dt_rec$,24,18)
            return

        delete_dtholdfl
        dt_bar$ = str(dt_rec$,24,18)
        dt_key$ = str(dt_rec$,24,23)
        read #13, hold, key = dt_key$, eod goto notfnd:
            delete #13

/* update order_master */
notfnd:
            init(" ") stmt1$, stmt2$
            stmt1$  = "UPDATE MSSQL.ORDERMASTER SET STATE = 'unplan', " & ~
                      " datestatechange = current_date " &                ~
                  " WHERE salesorder = '" & str(dt_bar$,1,8) & "'"

            gosub oracle_flush
            gosub oracle_exec
            if oci_err% <> 0% then goto sqlerr
        stmt1$ = "COMMIT"
            gosub oracle_flush
            gosub oracle_exec
            return

sqlerr:
            gosub oracle_error1
            return

        update_apcplndt
        des_cnt = 0
            dt_key$ = "000000000000000000000"

    read_loop
        read #13, hold, key > dt_key$, hold, using L63866, dt_rec$,    ~
                     eod goto end_update
        dt_key$ = str(dt_rec$,24,23)
            dt_part$ = str(dt_rec$,189%,25%)
        dt_load$ = str(dt_rec$,1,5)
        dt_drop_seq$ = str(dt_rec$,6,5)
        dt_drop$ = str(dt_rec$,11,2)
        dt_cus_sort$ = str(dt_rec$,13,5)
        dt_ln_item$ = str(dt_rec$,18,2)
        dt_item_no$ = str(dt_rec$,20,4)
        dt_so_line$ = str(dt_rec$,24,10)
        dt_bar$ = str(dt_rec$,24,18)
            dt_dept$ = str(dt_rec$,42,3)
        dt_proc$ = str(dt_rec$,45,2)
            dt_date$ = str(dt_rec$,47,6)
            dt_dte$ = str(dt_rec$,53,6)
REM     dt_date$ = pln_dte$
REM     dt_dte$  = pln_dte$
            dt_special$ = str(dt_rec$,220,10)
        dt_st$ = str(dt_rec$,64,2)
        dt_index$ = str(dt_rec$,66,30)
        dt_shft$ = str(dt_rec$,104,2)
        dt_sort$ = str(dt_rec$,106,5)
        dt_time$ = str(dt_rec$,116,8)
        dt_cust$ = str(dt_rec$,124,9)
REM         dt_txt$  = str(dt_rec$,236,4)    /* Line Item Txt*/
            dt_sku$  = str(dt_rec$,245,7)    /* Line Item Txt*/
REM         dt_txt$  = str(dt_rec$,236,4)
        /*--------------------------------*/
            get str(dt_rec$,236,4), using bin4, dt_txt%
bin4:       FMT BI(4)
            convert dt_txt% to dt_txt$, pic (0000000000)
            dt_prv$    = str(dt_rec$,243,2)
            dt_group$  = str(dt_rec$,240,1)
            dt_config$ = str(dt_rec$,241,2)
            sc$        = str(dt_rec$,214,1)
            dt_ref$    = str(dt_rec$,096,8)
        /*--------------------------------*/
             a,b,c,d,e = 0.00

              get str(dt_rec$,133%,56%), using L32140, dt_sale, a, b, c,  ~
                                                      d, e, dt_upmh
L32140:     FMT 7*PD(14,4)
            convert dt_sale to dt_sale$, pic (########.####-)
            convert dt_upmh to dt_upmh$, pic (########.####-)
            get dt_date$, using L68020, dt_date%
L68020:     FMT PD(11,1)
            convert dt_date% to date1$, pic (00000000)
            get dt_dte$, using L68020, dt_dte%
            convert dt_dte% to date2$, pic (00000000)
            bg_dte$ = ent_dte$
            pln_day$ = ent_dy$
            gosub calc_date
        bg_yr% = dt_date% / 10000%
        convert bg_yr% to tmp_yr$, pic (####)
            get dt_rec$, using dtyr, bg_yr%
dtyr:       FMT POS(230), BI(2)
            bg_wk$ = str(dt_rec$,232,2)
            dt_seq$ = str(dt_rec$,111,5)
            pln_day$ = str(dt_rec$,234,2)
REM     convert bg_yr$  to bg_yr%, data goto skip_year
skip_year:  convert bg_yr%  to bg_yr$, pic (00)
            if str(dt_sale$,3,1) = "#" then dt_sale$ = "0.00"

            init(" ") stmt1$, stmt2$, stmt3$, stmt4$, stmt5$, stmt6$, ~
                      stmt7$, stmt8$
            stmt1$  = "DELETE FROM DTS_PLAN_DETAIL WHERE " & ~
              "BARCODE = '" & dt_bar$ & "' AND PLAN_DEPARTMENT = '" & ~
           dt_dept$ & "' AND PROCESS_CODE = '" & dt_proc$ & "'"

            gosub oracle_flush
            gosub oracle_exec

            if oci_err% <> 0% then gosub oracle_error1

REM                    ....+....1....+....2....+....3....+....4....+....5

          stmt1$    = "INSERT INTO DTS_PLAN_DETAIL (BARCODE, " & ~
              "PLAN_DEPARTMENT, PROCESS_CODE, LOAD_NUMBER, " & ~
                      "DROP_SEQUENCE, PROD_DROP_NO, PRODUCTION_DATE, " & ~
              "CUST_SORT_CODE, PROD_ITEM_NUMBER, SO_LINE_ITEM, " & ~
              "SCANNED_DATE, PRODUCTION_DEPARTMENT, " & ~
                      "PRODUCTION_PROCESS, PRODUCTION_STATUS"

          stmt2$    = ", SORT_INDEX, WARRANTY_REMAKE_NUMBER, " & ~
              "SCHED_SHIFT_CODE, PRODUCTION_SORT, " & ~
              "PRODUCTION_SEQUENCE, SCANNED_TIME, " & ~
              "CUSTOMER_CODE, SALES_ORDER_PRICE, " & ~
              "MATERIAL_COST, LABOR_COST, LABOR_OVERHEAD, " & ~
                  "FREIGHT_COST, VINYL_DISCOUNT, " & ~
              "UNIT_PER_MAN_HOUR, MFG_PART_NUMBER,"

          stmt3$    = " SASH, MFG_PART, SAMPLE, WOOD_SURROUND_CODE, " & ~
              "SPECIAL_PRODUCTS, PRODUCTION_YEAR, " & ~
              "PRODUCTION_WEEK, PRODUCTION_DAY, " & ~
              "PRODUCTION_TEXT, GROUP_CODE, CONFIG_CODE, " & ~
              "PRIVATE_LABEL_CODE, SKU) VALUES ('" & ~
                      dt_bar$ & "','" & dt_dept$ & "','" & dt_proc$ & ~
              "','" & dt_load$ & "','" & dt_drop_seq$ &      ~
                      "','" & dt_drop$ & "',TO_DATE("

          stmt4$    = "'" & date1$ & "','yyyymmdd'),'" & dt_cus_sort$ & ~
                      "','" & dt_item_no$  & "','" & dt_ln_item$ &    ~
                      "',TO_DATE('" & date1$ & "','yyyymmdd'),'" &    ~
                      dt_dept$ & "','" & dt_proc$ & "','" & dt_st$ & "',"
          stmt5$ =    "'" & dt_index$ & "','" & dt_ref$ & "','" & ~
                      dt_shft$ & "','" & dt_sort$ & "','" & dt_seq$ & "','" & ~
                      dt_time$ & "','" & dt_cust$ & "'," & dt_sale$ & ~
                      ",0.00,0.00,0.00,0.00,0.00," & dt_upmh$ &  ~
                      ",'" & dt_part$ & "'"
          stmt6$ =    ",'" & sc$ & "','" & ~
                      str(dt_rec$,215,1) & "','"  & ~
                      str(dt_rec$,216,1) & "','" &  ~
                      str(dt_rec$,217,3) & "','" & dt_special$ & ~
                      "'," & tmp_yr$ & ",'" & bg_wk$ & "','" & ~
                      pln_day$ & "'," &  ~
              dt_txt$ & ",'" &  ~
                      dt_group$ & "','" & dt_config$ & "','" & ~
                      dt_prv$ & "','" & dt_sku$ & "')"

            init(" ") stmt7$, stmt8$

            gosub oracle_flush
            gosub oracle_exec1

            if oci_err% <> 0% then gosub oracle_error1

            delete #13

            des_cnt = des_cnt + 1
        if des_cnt < 500 then goto read_loop
        des_cnt = 0
        stmt1$ = "COMMIT"
            gosub oracle_flush
            gosub oracle_exec
          /* check bckupdte */
              goto read_loop
end_update:

        stmt1$ = "COMMIT"
            gosub oracle_flush
            gosub oracle_exec
        return

     calc_date
           julian_days% = 365%              /* Julian Days in Year     */
           if leap_yr% = 1% then julian_days% = 366%   /* Leap Year    */
                                            /* PLN_DAY$ = Input  Day   */
                                            /* PLN_DATE = Output Format*/
                                          /* Specified Production Week */
           call "DATE" addr("GJ", str(bg_dte$,,6%), str(jdate1$,,5%), x%)       /* (Y2K, LDJ) */
                                          /* Convert to Julian Date Fmt*/
           call "DATJULCV" (jdate1$)    /* Convert from Packed to Ascii*/       /* (Y2K, LDJ) */
           convert str(jdate1$,5%,3%) to j1%, data goto L52480                  /* (Y2K, LDJ) */
L52480:
           convert pln_day$ to dd%, data goto L52630
                                          /* Calculate the Date Assoc. */
           j1% = j1% + (dd% - 1%)         /* with Specified Prod Day   */
           if j1% <= julian_days% then goto L52570
                                                       /*  (EWD011)    */
REM              convert (bg_yr% + 1%) to str(jdate1$,1%,4%), pic(0000)            /* (Y2K, LDJ) */
              convert bg_yr% to str(jdate1$,1%,4%), pic(0000)

              j1% = j1% - julian_days%

L52570:    convert j1% to str(jdate1$,5%,3%), pic(000)                          /* (Y2K, LDJ) */
           call "DATJULCV" (jdate1$)    /* Convert back to Packed */            /* (Y2K, LDJ) */
                                          /* Begin with Production*/
                                           /* Date and   */
           call "DATE" addr("JG", str(jdate1$,,5%), str(pln_dte$,,6%), x%)      /* (Y2K, LDJ) */
           pln_date$ = pln_dte$
           call "DATFMTC" (pln_date$)                                           /* (Y2K, LDJ) */
        return
L52630:    errormsg$ = "(Error) - Invalid Production Day (1 thru 7)?"
           init(" ") pln_date$, pln_dte$, pln_day$
        return

        oracle_exec1
            oci_err% = 0%
            call "EXEC1" (stmt1$, stmt2$, stmt3$, stmt4$, stmt5$, ~
                          stmt6$, stmt7$, stmt8$, oci_err%)
            init(" ") stmt1$, stmt2$, stmt3$, stmt4$, stmt5$, ~
                      stmt6$, stmt7$, stmt8$

        return

        oracle_error1
REM            oci_err% = 0%
            init(" " ) error$
            call "ERROR" (error$)

            call "SHOSTAT" ("ERROR --> " & error$)

        return

                                                    /* (EWD007)       */

        update_oracle
            gosub oracle_flush
            init(" ") stmt1$, stmt2$
            str(stmt1$,1%,40%)  = "UPDATE MSSQL.WARRANTY SET WARRANTYID = '"
            str(stmt1$,41%,20%) = " ' WHERE BARCODE = '"
            str(stmt1$,61%,20%) = str(bar_key$,1%,18%) & "'"
            gosub oracle_exec
        return

        oracle_connect
            init(" ") user$, pass$, server$
REM            user$   = "MSSQL"
REM            pass$   = "MSSQL"
            gosub get_user_pswd       /* (AWD015) */
            oci_err% = 0%
            call "CONNECT" (user$, pass$, server$, oci_err%)
        return


        oracle_flush
            oci_err% = 0%
            call "FLUSH" (oci_err%)
        return

        oracle_exec
            oci_err% = 0%
            call "EXEC" (stmt1$, stmt2$, oci_err%)
        return

        oracle_no_connect
           comp% = 2%
           hdr$ = "***************************************"
           msg$(1%) = " - - - You are NOT connect to Oracle - - - "
           msg$(2%) = "      Contact Systems For Support.         "
           msg$(3%) = "      Press <Return> to Exit??             "
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                    /*  (EWD007)      */


/* (AWD012) - Begin */

        update_awdplndl
             init(" ") awdplndl_rec$, awdplndl_key$, time$, awdplndl_flag$
             time$ = time
             awdplndl_flag$ = "0"
             if keyhit% = 32% then awdplndl_flag$ = "1"

             str(awdplndl_key$,1%,6%)   = date
             str(awdplndl_key$,7%,6%)   = time$
             str(awdplndl_key$,13%,3%)  = userid$
             str(awdplndl_key$,16%,8%)  = or_so$

             read #20, hold, key = awdplndl_key$, eod goto no_awdplndl_rec

                      delete #20

no_awdplndl_rec:

             str(awdplndl_rec$,1%,23%)  = str(awdplndl_key$,1%,23%)
             str(awdplndl_rec$,24%,1%)  = awdplndl_flag$
             str(awdplndl_rec$,25%,2%)  = awdplndl_status$    /* Status */
             str(awdplndl_rec$,27%,5%)  = awdplndl_load$      /* Load   */
             str(awdplndl_rec$,32%,97%) = " "                 /* Filler */

             write #20, using AWDPLNDL_FMT1, awdplndl_rec$,            ~
                                   eod goto AWDPLNDL_WRITE_ERR
AWDPLNDL_FMT1:           FMT CH(128)


         return
         AWDPLNDL_WRITE_ERR
             errormsg$ = "ERROR - Error writing AWDPLNDL record !! "
             gosub error_prompt

             return
/* (AWD012) - end */

/* (AWD015) beg */
        get_user_pswd
            call "READ100" (#22, "ORACLE PASSWORD", f1%(22%))   /* SYSFILE2 */
            if f1%(22%) <> 0% then get #22 using ORCL_PSWD, user$, pass$
ORCL_PSWD:         FMT POS(21), CH(50), POS(50)

        return

/* (AWD015) END */

/* (SR70889) */

        lookup_sub_part                              /* (AWD005) - BEG */
             init(" ") flag$, pgm$, bcksubpt_rec$, flds$(), info_flds$(), ~
                       subpart$, infopart$, mfgplant$, invplant$
            flag$ = "0"                  /* Sales Order Info         */
            pgm$  = "1"


            convert so_inv$ to so_inv%, data goto convert_alpha

            convert so_inv% to so_inv$, pic(00000000)

            goto order_converted

convert_alpha:
            convert str(so_inv$,2%,7%) to so_inv%, data goto sub_part1
sub_part1:
            convert so_inv% to str(so_inv$,2%,7%), pic(0000000)


order_converted:
            convert item_no$ to item_no%, data goto sub_part2
sub_part2:

            convert item_no% to item_no$, pic(###)


        call "AWDBKSUB"   (flag$,        /* Flag 0=SalesOrder 1=Invoice*/~
                          pgm$,          /* Calling Program 0=BCKUPDTE */~
                                         /* 1=Any Other 2=Delete       */~
                                         /* 3=Invoice                  */~
                          so_inv$,       /* SO or Invoice Num to lookup*/~
                          item_no$,      /* Item Number                */~
                          bcksubpt_rec$, /* Record If BCKUPDTE then    */~
                                         /* pass in else pass out      */~
                          flds$(),       /* Part Number Fields         */~
                          info_flds$(),  /* Information Fields         */~
                          #63,           /* BCKSUBPT File              */~
                          suberr1%)      /* Error Code                 */


            subpart$  = str(bcksubpt_rec$,48%,20%)
            infopart$ = str(bcksubpt_rec$,132%,20%)
            invplant$ = str(infopart$,5%,1%)  /* (SR70889) */
            mfgplant$ = str(infopart$,7%,1%)  /* (SR70889) */

            if suberr1% = 0% then return
            str(bcksubpt_rec$,48%,20%) = "00000000000000000000"
            str(bcksubpt_rec$,132%,20%) = "00000000000000000000"
            subpart$ = str(bcksubpt_rec$,48%,20%)
            infopart$ = str(bcksubpt_rec$,132%,20%)

            errormsg$ = "AWDBKSUB ERROR = "&so_inv$ & " Line= " & item_no$
            gosub error_prompt
            suberr1% = 0%

        return                                       /* (AWD005) - end */

/* (SR70889\) */
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("Updating Database, Please wait!")
            gosub oracle_connect
               if oci_err% >= 0% then goto oracle_connected
                  gosub oracle_no_connect
                  end
oracle_connected:
            gosub update_apcplndt
        close #13
REM         call "FILEBGON" (#13)            /* Department Capacities   */
            call "SHOSTAT" ("One Moment Please")

            end
