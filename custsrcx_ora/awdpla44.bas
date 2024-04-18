        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLA44                             *~
            *  Creation Date     - 10/02/96                             *~
            *  Last Modified Date- 02/28/07                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Modified By       - Christie M. Gregory                  *~
            *  Description       - Staging, Loading, and Shipping       *~
            *                      Utility Program.                     *~
            *                                                           *~
            *                                                           *~
            *  Code Tables Used  - (PLAN SHFT) - Shift Codes            *~
            *                      (PLAN DEPT) - Department Codes       *~
            *                      (PLAN PROC) - Planning Process Codes *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/02/96 ! Mods to Switch (APCTRACK) to New Planning! RHH *~
            *          !   System.                                !     *~
            * 04/16/97 ! Mods to add Password Control to the use  ! RHH *~
            *          !   PF(9)Update. New Sub. (APCPASSW)       !     *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 06/05/98 ! (EWD001) Fix scr_drop$ to work with the  ! RHH *~
            *          !   selection Data.                        !     *~
            * 06/08/98 ! (EWD002) Mod's to save entries that are  ! RHH *~
            *          !   removed from load. New file (EWDBOLRM).!     *~
            * 06/11/98 ! (EWD003) Mod to ad New Selection for     ! RHH *~
            *          !   backorders. Remove (APCPLC44) No Longer!     *~
            *          !   used by program.                       !     *~
            * 10/01/98 ! (EWD004) Mod for new sort for Drivers Log! RHH *~
            * 10/20/98 ! (EWD005) Mod for new Glass Warranty      ! RHH *~
            *          !   Do not print on the delivery_ticket.   !     *~
            *          !   glass warranty line items.             !     *~
            * 11/05/98 ! (EWD006) Mod for private Label and Series! RHH *~
            *          !                                          !     *~
            * 12/13/99 ! (EWD007) Mod for not updating APCPLNSC for CMG *~
            *          !          records with same barcode but   !     *~
            *          !          with different departments.     !     *~
            *          !                                          !     *~
            * 12/16/99 ! (EWD008) Mod to replace openchecks with  ! CMG *~
            *          !          "EWDOPENS".  Also add open_error!     *~
            *          !          routine.                        !     *~
            *          !                                          !     *~
            * 07/18/00 ! (EWD009) Mod to delete label records when! RHH *~
            *          !          product is removed or back      !     *~
            *          !          ordered from load.              !     *~
            *          !          New Routine 'remove_label'      !     *~
            * 04/10/01 ! (EWD010) Mod to Add Planning Sort        ! CMG *~
            *          !           Sequence to Screen.            !     *~
            * 06/06/01 ! (EWD011) Mod to add Error Msg to screen  ! CMG *~
            *          !           when there is no BCKMASTR      !     *~
            *          !           record because of order being  !     *~
            *          !           canceled incorrectly.          !     *~
            * 06/28/01 ! (EWD012) Mod to take away backorder from ! CMG *~
            *          !           load and put in new program    !     *~
            *          !           EWDPLN79 that allows scanning  !     *~
            *          !           of barcode from prod lines.    !     *~
            *          !           Also add sub EWDPLA44.         !     *~
            * 07/13/01 ! (EWD013) Mod for Special Shapes and the  ! RHH *~
            *          !           department '104'               !     *~
            * 04/25/02 ! (EWD014) Mod to change customer name on  ! CMG *~
            *          !           driver's report to the order   !     *~
            *          !           customer's ship-to name.       !     *~
            * 04/17/02 ! (EWD015) Mod change company name         ! TLM *~
            * 02/10/03 ! (EWD016) Mod to add Window Wizard Line No! CMG *~
            * 05/07/03 ! (EWD017) Mod to allow numeric or alpha   ! CMG *~
            *          !          load numbers.                   !     *~
            * 10/02/03 ! (EWD018) Mod to add new status option.   ! CMG *~
            * 05/02/05 ! (AWD019) Mod for new delivery ticket     ! CMG *~
            * 12/05/05 ! (AWD020) Mod to add load totals to end   ! CMG *~
            *          !    of delivery ticket                    !     *~
            * 03/03/06 ! (PAR001) Mod for New Sub Part Number in  ! RHH *~
            *          !    the label file.                       !     *~
            * 01/24/07 ! (AWD021) mod to who and when closed load ! CMG *~
            * 02/28/07 ! (AWD022) do not close load if so is enter! CMG *~
            * 08/10/09 ! (AWD023) drop warranties from report     ! DES *~
            *03/30/2011! (AWD024) mod for address on M2O Orders   ! CMG *~
            *06/03/2011! (AWD025) - add orcl usr & pswd lookup    ! CMG *~
            *10/14/2014! (AWD026) mod for Warning message when    ! PWW *~
            *          !     generating POD without BOL           !     *~
            *09/10/2015! SR68443  Disable PF9 & PF14 if user is   ! PWW *~
            *          !      not in apcmaster. Fixed bug with    !     *~
            *          !      barcode/part column heading.        !     *~
            *11/12/2015! SR70531  Disable PF9 & PF14 if user id is! PWW *~
            *          !      "SHP".                              !     *~
            *07/11/2016! SR75119  Remove Phone num,Delv Dte & Re- ! PWW *~
            *          !      marks. Add Seq. Num.                !     *~
            *08/31/2016! (CR456) DC Center status 15 RGA          ! CMG *~
            *09/30/2016!(SR77673) mod to show rga product on      ! CMG *~
            *          !    selection 1 & 2 for shipping to track !     *~
            *03/20/2017! (CR905) add room location to bol         !     *~
            *05/05/2018! CR Proj - Trigger Atlas on order remove  ! RDB *~
            *03/25/2021! CR2797  - Add SKU to front of desc       ! RDB *~
			*04/18/2023! CR3300 Add new RGA 300 bldg with new dept! RDB *~
            *************************************************************

        dim                              /* FILE = APCPLNOR            */~
            or_key4$8,                   /* Alt 4 S.O.                 */~
            or_po$16, or_drop$2          /* Customer P.O. Number       */

        dim                              /* FILE = APCPLNSC            */~
            sc_rec$128,                  /* Record Format              */~
            sc_key$10,                   /* Primary Key                */~
            sc_sav$10,                   /* Save readkey  EWD012       */~
            pload$5,                     /* Parent Load   EWD012       */~
            sc_key1$27                   /* Alt Key 1                  */

        dim                              /* FILE = APCPLNDT            */~
            dt_rec$256,                  /* Record Format              */~
            dt_key3$23,                  /* Alt Key                    */~
            dt_bar$18,                   /* Barcode                    */~
            dt_st$2,                     /* Scann Status               */~
            dt_proc$2,                   /* Production Process Code    */~
            dt_dept$3,                   /* Department                 */~
            dt_drop$2,                   /* Drop                       */~
            dt_part$25,                  /* part                       */~
            dt_cust$9, sav_cust$9,       /* Customer Code (EWD004)     */~
            dt_so$8,                     /* Sales Order Number         */~
            dt_item$2                    /* Line Item                  */

/* CR2797 */
        dim tmp_bar$18, lowes_key$23, lowes_sku$9
        
        dim                              /* (New) - (APCPLNLD) - File  */~
            ld_load$5, sav_load$5,       /* Load Number                */~
            ld_desc$30, apc_desc$30,     /* Load Description           */~
            ld_dtp1$8, ld_dte1$6,        /* Load Production Date (Plan)*/~
            ld_dtp2$8, ld_dte2$6,        /* Load Completion Date (Plan)*/~
            ld_dtp3$8, ld_dte3$6         /* Load Load Date       (Plan)*/

        dim                              /* (Program Variables)        */~
            cust_desc$30,                /* CUSTOMER NAME (EWD004)     */~
            cust_addr$(6%)30,            /* Ship To Address (EWD004)   */~
            bill_desc$(3%)30,            /* Bill to name and address   */~
            bill_city$18,                /* Bill To City               */~
            bill_state$2,                /* Bill to State              */~
            bill_zip$9,                  /* Bill To Zip Code           */~
            bck_key$25,                  /* BCKMASTR Primary           */~
            bck_terms$20,                /* Customer Terms             */~
            bck_fob$20,                  /* Customer FOB               */~
            bck_job$20, bck_cl$2,        /* Job Name                   */~
            bck_ship$10,                 /* ship Date                  */~
            bck_phone$14,                /* Customer Phone Number      */~
            bck_ln_key$19,               /* BCKLINES                   */~
            sav_ln_key$19,               /* BCKLINES           (AWD019)*/~
            bck_ln$2,                    /* Line item                  */~
            bck_ww_ln$2,                 /* WW Line item       (EWD016)*/~
            bck_part$25,                 /* Part Number                */~
            bck_desc$32,                 /* Part Descrip               */~
            bck_d$11,                    /* Line Item Desc             */~
            ship_to$30,                  /* Ship-To Address    (EWD014)*/~
            ship_to_city$30,             /* Ship City, St, Zip (EWD014)*/~
            s_23m$3,                     /* Model Code         (EWD006)*/~
            s_23$8,                      /* Series name        (EWD006)*/~
            s_so$8,                      /* Sales order Code   (EWD006)*/~
            s_ln$3,                      /* Sales Order Line Item      */~
            s_prv$30,                    /* Private Label Name (EWD006)*/~
            s_1$2,                       /* Private Label Code (EWD006)*/~
            bck_tst$18,                  /* Build width and Height     */~
            bck_wd$9,                    /* Line item Width            */~
            bck_ht$9,                    /* Line item Height           */~
            bck_po$16,                   /* P.O. Number                */~
            bck_mull$3,                  /* Save Mull Code             */~
            qty(6%), qty$(6%)10,         /* BCKLINES - Quantities      */~
            tt(4%),                      /* Detail Totals              */~
            tot$(4%)4,                   /* Customer Totals (EWD004)   */~
            sav_so$8,                    /* SAVE SALES ORDER NUMBER    */~
            o$(2500%)1,                  /* Update Flag                */~
            d$(2500%)2,                  /* Customer Drop Codes        */~
            b$(2500%)25, bb$(4000%)18,   /* Part Number abd Barcodes   */~
            bo$(2500%)1,                 /* BackOrder Code (EWD012)    */~
            c$(2500%)9,                  /* Customer Numbers           */~
            s$(2500%)8,                  /* Sales Order Numbers        */~
/*EWD010*/  st$(2500%)5,                 /* Planning Sort Sequence     */~
            m$(2500%)4,                  /* Make / Pull                */~
            s1$(3%)50,                   /* Update Screen Text         */~
/*EWD010*/  hdr$(7%)25,                  /* Update Column Headings     */~
            scr$(11%)30, scr1$(20%)40,   /* Screen Text Messages       */~
            stat_msg$(4%)39,             /* Status Messages            */~
            scan$5,                      /* Scanned Record Count       */~
            i_max$5,                     /* Number of Records Selected */~
            scr_code$1,                  /* Screen Code (0) or (1)     */~
            scr_load$5,                  /* Screen Load Number         */~
            scr_drop$2,                  /* Customer Drop Number       */~
            scr_so$8,                    /* Screen Sales order (AWD020)*/~
            scr_msg$30,                  /* Screen Message Process Type*/~
            scr_msg1$30,                 /* Screen Message Completion  */~
            company$40,                  /* For Report Company Name    */~
            print_title$40,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            readkey$24,                  /* GEN CODES KEY              */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(25%),                    /* = 0 if the file is open    */~
            f1%(25%),                    /* = 1 if READ was successful */~
            fs%(25%),                    /* = 1 if file open, -1 if it */~
            rslt$(25%)20                 /* Text from file opening     */

        dim                                                              ~
            hdr$40,                      /* ASKUSER HEADER             */~
            msg$(3%)79                   /* ASKUSER TEXT               */

        dim                              /* ( APCTRKWK)                */~
            wrk_key$60, mode$5,          /* Load Number                */~
            wrk_key1$60, sav_key1$33,    /* Second Sort (EWD004)       */~
            sav_po$16,                   /* Save P.O. Number (EWD004)  */~
            wrk_rec$140, sav_rec$200,    /* Load Description           */~
            wrk_qty$4, seq$5,            /* Scheduled Production Date  */~
            city$18,                     /* Planned Production Date    */~
            state$2,                     /* Scheduled Completion Date  */~
            zip$9,                       /* Planned Completion Date    */~
            cus_contact$20,              /* Customer Contact           */~
            cus_phone$10,                /* Customer Phone             */~
            count$3                      /* Scheduled Load Date        */

        dim oradesc_key$14,              /* ORADESCR File Key          */~
            savorad_key$14,              /* Save Read Key              */~
            ora_config$1,                /* Configuration or Not       */~
            oradesc_key1$14,             /* ORADESCR Key 1             */~
            savorad_key1$14,             /* Save Read Key              */~
            part_desc$250,               /* Oracle Part Description    */~
            window$1,                    /* Window flag                */~
            part$1,                      /* Part Flag                  */~
            scrn$1,                      /* Screen Flag                */~
            size_desc$25,                /* Size Desc                  */~
            prev_ww$3,                   /* Previous Printed WW Line   */~
            config_qty$7,                /* Caelus Line and Qty        */~
            shp_qty$3,                   /* Ship Qty                   */~
            prt_wind$3,                  /* Print Window Qty           */~
            prt_scrn$2,                  /* Print Screen Qty           */~
            prt_part$2,                  /* Print Part   Qty           */~
            sav_config$1,                /* Save Configuration         */~
            sav_po1$16,                  /* Save Purchase Order        */~
            prt_line$2,                  /* Formatted Print Line Num   */~
            sav_drop$2,                  /* Save Drop Number           */~
            drop_tt(4%),                 /* Drop Totals                */~
            drop_tot$(4%)4,              /* Drop Totals                */~
/*AWD020*/  load_tt(4%),                 /* Load Totals                */~
/*AWD020*/  load_tot$(4%)4,              /* Load Totals                */~
            awdtrail_key$20,             /* AWDTRAIL Readkey           */~
            awdtrail$8,                  /* Trailer Number             */~
/*SR75119*/ sequences$(500%)5            /* Seq nums                   */

        dim userwhoenter$3,              /* UserWhoEnteredOrder(AWD024)*/~
            cust_address$(3%)30          /* Customer Address (AWD024)  */

        dim txt_key$11,                  /* (CR905) Text File Key      */~
            sve_txt$11,                  /* (CR905) save txt key       */~
            txtid$4,                     /* (CR905) text id            */~
            txt$70,                      /* (CR905) Text               */~
            nameorderby$60,              /* (CR905) Name Order By      */~
            roomLoc$30                   /* (CR905) Room Location      */

        dim                              /*  (AWD019)                  */~
            server$25,                   /* Connection String          */~
            user$25,                     /* User Name to Connect       */~
            pass$25                      /* Password to Connect        */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(Staging/Loading) Tracking & Update Util"
            pname$ = "AWDPLA44 - Rev: R6.04 06/18/06"

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
            * #1  ! APCPULLS ! APC Pull From Inventory Master File      *~
            * #2  ! APCPLNDT ! Production Master Detail (Old APCPIECE)  *~
            * #3  ! APCPLNOR ! Planning S.O. Header History-Old APCORDER*~
            * #4  ! APCPLNSC ! Planning Master Schedule-Old APCLINES    *~
            * #5  ! GENCODES ! Master System Table File                 *~
            * #6  ! APCPLNAD ! Scanning Audit File                      *~
            * #7  ! BCKMASTR ! Sales Order Header File        (EWD004)  *~
            * #8  ! BCKLINES ! Sales Order Detail Lines       (EWD004)  *~
            * #9  ! EWDDELIV ! Newd Delivery Sort for Drivers (EWD004)  *~
            * #10 ! EWDBOLRM ! Line Items Removed from Load   (EWD002)  *~
            * #11 ! CUSTOMER ! CUSTOMER MASTER FILE                     *~
            * #13 ! EWDBOLBK ! Cross-Reference file for Back Orders     *~
            * #12 ! EWDPRDLB ! New Production Labels(EWD009) (PAR001)   *~
            * #14 ! APCTRKWK ! DRIVER REPORT WORK FILE                  *~
            * #15 ! APCPLNLD ! Planning Load Master - Old APCMAST       *~
            * #16 ! ORADESCR ! Oracle Description and Info   (AWD019)   *~
            * #17 ! AWDTRAIL ! Track Trailer and Load Assigned  (AWD019)*~
            * #18 ! SYSFILE2 ! Caelus Management System General Informa *~
            * #19 ! TXTFILE  ! Text File   (CR905)                      *~                    
/*CRProj*/  * #23 ! PGORLNTR ! PlyGem ATLaS Trigger Remote Order Line Fi*~            
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "APCPULLS",                                     ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =   10, keylen =   10,                    ~
                        alt key  1, keypos =    1, keylen =  19,         ~
                            key  2, keypos =   20, keylen =  25, dup

            select #2,   "APCPLNDT",                                     ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   24, keylen =   23,                    ~
                        alt key  1, keypos =   47, keylen =  57,         ~
                            key  2, keypos =   53, keylen =  51,         ~
                            key  3, keypos =    1, keylen =  23, dup,    ~
                            key  4, keypos =   96, keylen =   8, dup

            select #3,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize = 170,               ~
                        keypos =    1, keylen =   51,                    ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos =   70, keylen =   8, dup,    ~
                            key  3, keypos =   78, keylen =   8, dup,    ~
                            key  4, keypos =   52, keylen =   8,         ~
                            key  5, keypos =   36, keylen =  16, dup

            select #4, "APCPLNSC",                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   24, keylen =   10,                    ~
                        alt key  1, keypos =    7, keylen =  27,         ~
                            key  2, keypos =    1, keylen =  33

            select #5, "GENCODES",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #6,   "APCPLNAD",                                     ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =   19, keylen =   33,                    ~
                        alt key  1, keypos =    1, keylen =  33
                                                     /* (EWD004)       */
            select #7,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #8,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19



            select #9,  "EWDDELIV",                                      ~
                        varc,     indexed,  recsize =   200,             ~
                        keypos =    1, keylen =  60
                                                     /* (EWD002) Begin */
            select #10, "EWDBOLRM",                                      ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos =    1, keylen =   16,                    ~
                        alt key  1, keypos =    6, keylen =  11
                                                     /* (EWD002) End   */
            select #11, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup
                                                    /* (EWD009) - New   */

/*PAR001*/
            select #12, "EWDPRDLB",                                      ~
                        varc,     indexed,  recsize =  1024,             ~
                        keypos =    1, keylen =   35,                    ~
                        alt key  1, keypos = 278, keylen =  23
                                                    /* (EWD009) - File  */

                                                    /* (EWD012) - New   */
            select #13, "EWDBOLBK",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    7, keylen =   16,                    ~
                        alt key  1, keypos =   12, keylen =  11,         ~
                            key  2, keypos =    1, keylen =  22,         ~
                            key  3, keypos =    2, keylen =  21
                                                    /* (EWD012) - File  */

            select #14, "APCTRKWK",                                      ~
                        varc,     indexed,  recsize =   200,             ~
                        keypos =    1, keylen =  60

            select #15, "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =   11, keylen =  5,                      ~
                        alt key  1, keypos =    3, keylen =  13,         ~
                            key  2, keypos =    1, keylen =  15


            select #16, "ORADESCR",                                      ~
                        varc,     indexed,  recsize =  1024,             ~
                        keypos =    1, keylen =  14,                     ~
                        alt key 1, keypos = 15, keylen = 14   /* (AWD019) */

                                                          /* (AWD019)   */
            select #17, "AWDTRAIL",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =  20,                     ~
                        alt key  1, keypos =   21, keylen =  20
                                                          /* (AWD019)   */

           select #18,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20
/*(CR905)*/
           select #19,  "TXTCUST",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11

            select #23, "PGORLNTR",                    /* PC Proj  */    ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   21, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  64,         ~
                            key  2, keypos =   54, keylen =  11, dup
						
/* (EWD008)  BEGIN */
            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, fs%(1%), f2%(1%),500%, rslt$(1%))
            filename$ = "APCPLNDT" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNOR" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSC" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNAD" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
                                                  /* (EWD004) - Begin */
            call "OPENCHCK" (#7, fs%(7%), f2%(7%),100%, rslt$(7%))
            filename$ = "BCKLINES" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
                                                  /* (EWD002) - Begin */
            call "OPENCHCK" (#10, fs%(10%), f2%(10%),100%, rslt$(10%))
                                                  /* (EWD002) - End   */
            filename$ = "CUSTOMER" : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error
                                                  /* (EWD009) - Begin */
            filename$ = "EWDPRDLB" : call "EWDOPEN" (#12, filename$, err%)
                                                  /* (EWD009) - End   */
                                                  /* (EWD012) - Begin */
            call "OPENCHCK" (#13, fs%(13%), f2%(13%),100%, rslt$(13%))
                                                  /* (EWD012) - End   */
            filename$ = "APCPLNLD" : call "EWDOPEN" (#15, filename$, err%)
            if err% <> 0% then gosub open_error

REM FILENAME$ = "ORADESCR"  CALL "EWDOPEN" (#16, FILENAME$, ERR%)
REM IF ERR% <> 0% THEN GOSUB OPEN_ERROR            /* (AWD019) */
            call "OPENCHCK" (#16, fs%(16%), f2%(16%),100%, rslt$(16%))

REM FILENAME$ = "AWDTRAIL"  CALL "EWDOPEN" (#17, FILENAME$, ERR%)
REM IF ERR% <> 0% THEN GOSUB OPEN_ERROR            /* (AWD019) */
            call "OPENCHCK" (#17, fs%(17%), f2%(17%),100%, rslt$(17%))

/* (EWD008)  END */
/*(AWD028) */
            filename$ = "SYSFILE2" : call "EWDOPEN" (#18, filename$, err%)
            if err% <> 0% then gosub open_error
/* (CR905) */
            filename$ = "TXTFILE" : call "EWDOPEN" (#19, filename$, err%)
            if err% <> 0% then gosub open_error
/* CR Proj */            
            filename$ = "PGORLNTR" : call "EWDOPEN" (#23, filename$, err%)
            if err% <> 0% then gosub open_error
			
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
            max_pieces% = 2500%               /* Max Windows on a Load */
            ora_connect% = 0%                 /* Connection Flag       */
/* CR3300 */            
        dim schema$8                     /* Schema                     */
			
        schema_err%, schema% = 0%
        init(" ") schema$
        call "SCHEMA" (schema$, schema%, #5, schema_err%)                                
/* CR3300 */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            call "ALLFREE"
            gosub initialize_variables
            gosub screen_message_1

            for fieldnr% = 1% to  4%            /* (AWD020) */
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
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  9% then gosub report
                  if keyhit%  = 10% then gosub process_data
                  if keyhit%  = 11% then gosub delivery_ticket
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then gosub select_data
                  if keyhit% <>  0% then       editpg1
L11140:     fieldnr% = cursor%(1%) - 5%              /* (AWD020) */
            if fieldnr% < 1% or fieldnr% > 4% then editpg1
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
            *      E D I T   M O D E   F O R   L O A D   D E T A I L    *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        process_data
            sel% = 1%
            goto L11340
        select_data
            sel% = 0%
L11340:     if scr_code% <> 4% then goto L11450   /* Update Inventory  */
L11350:        gosub'103(0%, 0%)                  /* Check List Screen */
               if keyhit% = 1% then gosub startover
               if keyhit% = 16% then gosub exit_program
               if keyhit% <> 9% then goto L11350

               gosub update_inventory

L11450:     call "SHOSTAT" ("Loading Data For Load")
/*SR68443*/ gosub screen_message_2
            gosub dataload                    /* Load All S.O. In Load */

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)            /* Display Screen - No Entry  */
                  if keyhit%  =  2% then i% = 1%
                  if keyhit%  =  9% then gosub begin_process_a /* SPEC */
                  if keyhit%  = 14% then gosub begin_process
                  if keyhit%  = 16% then goto inputmode
                  if keyhit% <>  0% then       editpg2
L11560:
            if cursor%(1%) > 10% then fieldnr% = 1%

            if fieldnr% < 1% or fieldnr% > 1% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L11630:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then goto editpg2
                  if keyhit% <>  0% then L11630
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11630
                  lastfieldnr% = fieldnr%
            goto L11560

        REM *************************************************************~
            *             P R O C E S S   D A T A                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************
        begin_process_a                         /* Special Process     */
            call "APCPASSW"("APCPLA44", userid$, check%)
                if check% <> 0% then goto L19220

            for i% = 1% to i_max%
                if o$(i%) = " " then goto L19130 /* Update Blank Selects*/
                   o$(i%) = " "                 /* Skip All No-Blanks  */
                   goto L19140                   /* with One Key Stroke */
L19130:         o$(i%) = "X"
L19140:     next i%

        begin_process                      /* Staging, Loading, Remove */

            inc% = scr_code%        
            on inc% gosub update_staging, update_loading, closeout_load,~
                          backorder_load, backorder_load, backorder_load, ~
						  backorder_load
                                                          /* (EWD018) */
                                           /* (EWD003) - Mod Backorder */
            gosub update_load_info
L19220: return clear all
        goto inputmode

        print_report
            gosub generate_report
        return clear all
        goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
         return

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

        scrn1_msg  :  data                                               ~
         "Enter a Valid Code Selection.                                ",~
         "Enter a Valid Load Number.                                   ",~
         "Enter a Valid Starting Drop Number or Leave Blank.           ",~
         "Enter a Valid Sales Order Number or Leave Blank.             "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28260
                inpmessage$ = edtmessage$
                return

L28260
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Non-Blank Character for Data to be Updated.          "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, scr_code$, scr_msg$,       ~
                      scr_code$, scr_msg1$, ld_load$, ld_dtp1$, ld_dte1$,~
                      ld_dtp2$, ld_dte2$, ld_dtp3$, ld_dte3$, ld_desc$,  ~
                      apc_desc$, sav_load$, scr_drop$, sc_key$, scr_load$,~
                      scr_so$
                      /*(AWD020)*/
            counter% = 0%
        return

        REM *************************************************************~
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
            *   L O A D   D A T A   F O R   S C R E E N   A R R A Y S   *~
            *-----------------------------------------------------------*~
            * Loads data for Screen Arrays-O$(),B$(),C$(),S$(),D$(),M$()*~
            *************************************************************
            dataload
               init(" ") o$(), b$(), c$(), s$(), d$(), m$(), bb$(), st$(), ~
                         dt_key3$, bo$()
               i%, i_max%, scan% = 0%
               str(dt_key3$,1%,5%)  = scr_load$
               read #2,key 3% > dt_key3$, using L35040, dt_rec$,          ~
                                                   eod goto dataload_done
               goto L30160
           next_dataload
               read #2, using L35040, dt_rec$, eod goto dataload_done

L30160:       if scr_load$ <> str(dt_rec$,1%,5%) then goto dataload_done
                 dt_dept$ = str(dt_rec$,42%,3%)     /* Department Code */
                 gosub check_support
                 if supp% = 1% then goto next_dataload
                 dt_proc$ = str(dt_rec$,45%,2%)   /* Prd Proc Cde (CR456) */
                                                            /* (SR77673) */

                 dt_st$   = str(dt_rec$,64%,2%)     /* Current Status  */
                 dt_drop$ = str(dt_rec$,11%,2%)   /* No. of Drops Last */
                 dt_so$   = str(dt_rec$,24%,8%)   /*(AWD020) SO Number */
                 if scr_drop% = 0% then goto L30170 /* (EWD001) Fix    */
                    if scr_drop$ <> dt_drop$ then goto next_dataload
L30170:                                             /* (EWD001)        */
                                                    /* (AWD020)        */
                 if scr_so$ <> " " and (scr_so$ <> dt_so$)               ~
                                            then goto next_dataload

                 scan%  = scan% + 1%
                                                   
REM SCR_CODE% = 5% still used in BACKORDER EWDPLN79                                                    
REM                                1       2       3       4       5
                 on scr_code% goto L30250, L30270, L30270, L30270, L30280, L30280, L30280
                                                      /* (EWD003)       */
L30250: REM       IF DT_ST$ < "14" THEN GOTO L30290   /* NOT STAGED     */
/* (CR456) */
/* (SR77673) */   if dt_st$ < "14" or dt_st$ = "26" or dt_st$ = "32"  ~
                                                        then goto L30290
                     goto next_dataload               /* Yes Staged     */
L30270:              REM IF DT_ST$ < "16" THEN GOTO L30290   /*NOT LOADED*/
/* (CR456) */
/* (SR77673) */   if dt_st$ < "16" or dt_st$ = "26" or dt_st$ = "32"  ~
                                                      then goto L30290
                     goto next_dataload               /* Yes Loaded     */
                                                      /* RGA   (EWD018) */
L30280:           if dt_st$ <= "16" or dt_st$ = "26" then goto L30290
                     goto next_dataload               /*       (EWD018) */
L30290:        i% = i% + 1%
               if i% > max_pieces% then i% = max_pieces%
        REM - Put Into Screen Arrays for Selection
               gosub check_backorder                  /* (EWD012)     */
               if str(pload$,1%,1%) = "A" then bo$(i%) = "@"  /*EWD012*/
               b$(i%) = str(dt_rec$,189%,25%)         /* Part Number  */
               c$(i%) = str(dt_rec$,124%,9%)          /* Customer     */
               s$(i%) = str(dt_rec$,24%,8%)           /* Sales Order  */
               d$(i%) = str(dt_rec$,11%,2%)           /* Drop Number  */
               bb$(i%)= str(dt_rec$,24%,18%)          /* Barcode      */
               st$(i%)= str(dt_rec$,111%,5%)   /*Planning Sort-EWD010 */
               if sel% = 1% then b$(i%) = str(dt_rec$,24%,18%)
               m$(i%) = "Make"
               if dt_dept$ = "102" then m$(i%) = "PStk"
               if dt_dept$ = "104" then m$(i%) = "PShp"   /* (EWD013)  */
               if dt_st$   = "26"  then m$(i%) = "Trns"   /* (CR456) */
               if dt_st$   = "32"  then m$(i%) = "RGA "   /* (CR456) */
               if dt_st$   = "14"  then m$(i%) = "Stge"   /* (CR456) */
               if dt_st$   = "16"  then m$(i%) = "Load"   /* (CR456) */
               goto next_dataload
        dataload_done
           i_max% = i%                       /* Total Pieces Found for */
           i%     = 1%                       /* Selection Made.        */
           convert scan% to scan$, pic(00000)  /* Total Pieces on Load */

           convert i_max% to i_max$, pic(00000)

           if scr_code% = 1% then                                        ~
              stat_msg$(1) = "Scheduled = "&scan$&" Not Staged = "&i_max$
           if scr_code% = 2% then                                        ~
              stat_msg$(1) = "Scheduled = "&scan$&" Not Loaded = "&i_max$
           if scr_code% = 3% then                                        ~
             stat_msg$(1) = "Scheduled = "&scan$&" Unscheduled = "&i_max$
           if scr_code% = 5% then                                        ~		   
             stat_msg$(1) = "Scheduled = "&scan$&" Unscheduled = "&i_max$
/* CR3300 */
           if scr_code% = 7% then                                        ~
             stat_msg$(1) = "Scheduled = "&scan$&" Unscheduled = "&i_max$			 
        return

        check_backorder
           init(" ") sc_key$, sc_sav$, pload$
           str(sc_key$,1%,8%) = str(dt_rec$,24%,8%)
           str(sc_key$,9%,2%) = str(dt_rec$,32%,2%)

           read #4, key = sc_key$, eod goto backorder_done

             get #4, using L30300, pload$
L30300:         FMT POS(105), CH(05)
        str(sc_sav$,1%,10%) = str(sc_key$,1%,10%)

        backorder_done
        return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Update Store Data and Part Data                           *~
            *************************************************************

        REM DATAPUT

        REM RETURN

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35040:     FMT CH(256)                  /* (APCPLNDT) - FILE          */


L35080:     FMT CH(128)                  /* (APCPLNSC) - FILE          */

            FMT CH(128)                  /* (APCPLNLD) - FILE          */


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
              on fieldnr% gosub L40095,         /* Screen Code       */   ~
                                L40090,         /* Load Number       */   ~
                                L40095,         /* Drop Number       */   ~
                                L40090          /* SO Number (AWD020)*/

              goto L40105

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40090:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40095:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40105:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (04,02), "Prod. Date: ",                               ~
               at (04,15), fac(hex(84)), ld_dtp1$               , ch(08),~
               at (04,25), "Comp. Date: ",                               ~
               at (04,38), fac(hex(84)), ld_dtp2$               , ch(08),~
               at (04,48), "Load. Date: ",                               ~
               at (04,61), fac(hex(84)), ld_dtp3$               , ch(08),~
               at (04,70), "Drops: ",                                    ~
               at (04,77), fac(hex(84)), dt_drop$               , ch(02),~
                                                                         ~
               at (06,02), "Update Code :",                              ~
               at (06,20), fac(lfac$(1%)), scr_code$            , ch(01),~
               at (06,40), fac(hex(84)), scr_msg$               , ch(30),~
               at (07,02), "Load Number :",                              ~
               at (07,20), fac(lfac$(2%)), scr_load$            , ch(05),~
               at (07,40), fac(hex(84)), apc_desc$              , ch(30),~
               at (08,02), "Drop Number :",                              ~
               at (08,20), fac(lfac$(3%)), scr_drop$            , ch(02),~
                                                                         ~
/*AWD020*/     at (09,02), "Sales Order :",                              ~
/*AWD020*/     at (09,20), fac(lfac$(4%)), scr_so$              , ch(08),~
                                                                         ~
               at (10,26), fac(hex(84)), scr$(1%)               , ch(30),~
               at (11,26), fac(hex(84)), scr$(2%)               , ch(30),~
               at (12,26), fac(hex(84)), scr$(3%)               , ch(30),~
               at (13,26), fac(hex(84)), scr$(4%)               , ch(30),~
               at (14,26), fac(hex(84)), scr$(5%)               , ch(30),~
               at (15,26), fac(hex(84)), scr$(6%)               , ch(30),~
               at (16,26), fac(hex(84)), scr$(7%)               , ch(30),~
               at (17,26), fac(hex(84)), scr$(8%)               , ch(30),~
               at (18,26), fac(hex(84)), scr$(9%)               , ch(30),~
               at (19,26), fac(hex(84)), scr$(10%)              , ch(30),~
/* CR3300 */ ~			   
               at (20,26), fac(hex(84)), scr$(11%)              , ch(30),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L40340
                  call "PRNTSCRN"
                  goto L40105

L40340:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40435     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40415
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40415:     if fieldnr% > 1% then L40425
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40425:     return

L40435: if fieldnr% > 0% then L40480  /*  Edit Mode - Select Fld */
/*SR68443*/ gosub screen_message_2
            pf$(1%)= "(1)Start Over    (9)Driver's Report     " &        ~
                     "                       (14)Print Report"
            pf$(2%)= "                 (10)Select (Barcode)   " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                 (11)Delivery Tickets   " &        ~
                     "                       (16)Select Data "
            pfkeys$ = hex(01ffffffffffffff090a0bffff0e0f1000)
            return
L40480:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Criteria for Edit Screen                                  *~
            *************************************************************

        deffn'102(fieldnr%, edit%)

L41080:       gosub'060(1%, fieldnr%)                /* Data Prompt   */
              gosub set_pf2
              if fieldnr% > 0% then init(hex(81)) lfac$()                ~
                               else init(hex(86)) lfac$()
              goto L41190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41190:     accept                                                       ~
               at (01,16), fac(hex(84)), s1$(1%)                , ch(50),~
               at (02,16), fac(hex(84)), s1$(2%)                , ch(50),~
               at (03,16), fac(hex(84)), s1$(3%)                , ch(50),~
                                                                         ~
               at (04,02), "Prod. Date: ",                               ~
               at (04,15), fac(hex(84)), ld_dtp1$               , ch(08),~
               at (04,25), "Comp. Date: ",                               ~
               at (04,38), fac(hex(84)), ld_dtp2$               , ch(08),~
               at (04,48), "Load. Date: ",                               ~
               at (04,61), fac(hex(84)), ld_dtp3$               , ch(08),~
               at (04,70), "Drops: ",                                    ~
               at (04,77), fac(hex(84)), dt_drop$               , ch(02),~
               at (05,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Update Code    :",                           ~
               at (06,20), fac(hex(84)), scr_code$              , ch(01),~
               at (06,40), fac(hex(84)), scr_msg$               , ch(30),~
                                                                         ~
               at (07,02), "Load Number    :",                           ~
               at (07,20), fac(hex(84)), scr_load$              , ch(05),~
               at (07,40), fac(hex(84)), apc_desc$              , ch(30),~
               at (08,02), "Drop Number    :",                           ~
               at (08,20), fac(hex(84)), scr_drop$              , ch(02),~
               at (08,40), fac(hex(84)), stat_msg$(1%),           ch(39),~
                                                                         ~
               at (10,02), fac(hex(ac)), hdr$(1%)               , ch(06),~
               at (10,10), fac(hex(ac)), hdr$(2%)               , ch(24),~
               at (10,36), fac(hex(ac)), hdr$(3%)               , ch(09),~
               at (10,47), fac(hex(ac)), hdr$(4%)               , ch(09),~
               at (10,58), fac(hex(ac)), hdr$(5%)               , ch(04),~
               at (10,64), fac(hex(ac)), hdr$(6%)               , ch(04),~
               at (10,71), fac(hex(ac)), hdr$(7%)               , ch(06),~
               at (11,04), fac(lfac$(1%)), o$(i%)               , ch(01),~
/*EWD012*/     at (11,08), fac(hex(84)),  bo$(i%)               , ch(01),~
               at (11,10), fac(hex(84)),   b$(i%)               , ch(25),~
               at (11,36), fac(hex(84)),   c$(i%)               , ch(09),~
               at (11,47), fac(hex(84)),   s$(i%)               , ch(08),~
               at (11,59), fac(hex(84)),   d$(i%)               , ch(02),~
               at (11,64), fac(hex(84)),   m$(i%)               , ch(04),~
/*EWD010*/     at (11,71), fac(hex(84)),  st$(i%)               , ch(05),~
               at (12,04), fac(lfac$(1%)), o$(i%+1%)            , ch(01),~
/*EWD012*/     at (12,08), fac(hex(84)),  bo$(i%+1%)            , ch(01),~
               at (12,10), fac(hex(84)),   b$(i%+1%)            , ch(25),~
               at (12,36), fac(hex(84)),   c$(i%+1%)            , ch(09),~
               at (12,47), fac(hex(84)),   s$(i%+1%)            , ch(08),~
               at (12,59), fac(hex(84)),   d$(i%+1%)            , ch(02),~
               at (12,64), fac(hex(84)),   m$(i%+1%)            , ch(04),~
/*EWD010*/     at (12,71), fac(hex(84)),  st$(i%+1%)            , ch(05),~
               at (13,04), fac(lfac$(1%)), o$(i%+2%)            , ch(01),~
/*EWD012*/     at (13,08), fac(hex(84)),  bo$(i%+2%)            , ch(01),~
               at (13,10), fac(hex(84)),   b$(i%+2%)            , ch(25),~
               at (13,36), fac(hex(84)),   c$(i%+2%)            , ch(09),~
               at (13,47), fac(hex(84)),   s$(i%+2%)            , ch(08),~
               at (13,59), fac(hex(84)),   d$(i%+2%)            , ch(02),~
               at (13,64), fac(hex(84)),   m$(i%+2%)            , ch(04),~
/*EWD010*/     at (13,71), fac(hex(84)),  st$(i%+2%)            , ch(05),~
               at (14,04), fac(lfac$(1%)), o$(i%+3%)            , ch(01),~
/*EWD012*/     at (14,08), fac(hex(84)),  bo$(i%+3%)            , ch(01),~
               at (14,10), fac(hex(84)),   b$(i%+3%)            , ch(25),~
               at (14,36), fac(hex(84)),   c$(i%+3%)            , ch(09),~
               at (14,47), fac(hex(84)),   s$(i%+3%)            , ch(08),~
               at (14,59), fac(hex(84)),   d$(i%+3%)            , ch(02),~
               at (14,64), fac(hex(84)),   m$(i%+3%)            , ch(04),~
/*EWD010*/     at (14,71), fac(hex(84)),  st$(i%+3%)            , ch(05),~
               at (15,04), fac(lfac$(1%)), o$(i%+4%)            , ch(01),~
/*EWD012*/     at (15,08), fac(hex(84)),  bo$(i%+4%)            , ch(01),~
               at (15,10), fac(hex(84)),   b$(i%+4%)            , ch(25),~
               at (15,36), fac(hex(84)),   c$(i%+4%)            , ch(09),~
               at (15,47), fac(hex(84)),   s$(i%+4%)            , ch(08),~
               at (15,59), fac(hex(84)),   d$(i%+4%)            , ch(02),~
               at (15,64), fac(hex(84)),   m$(i%+4%)            , ch(04),~
/*EWD010*/     at (15,71), fac(hex(84)),  st$(i%+4%)            , ch(05),~
               at (16,04), fac(lfac$(1%)), o$(i%+5%)            , ch(01),~
/*EWD012*/     at (16,08), fac(hex(84)),  bo$(i%+5%)            , ch(01),~
               at (16,10), fac(hex(84)),   b$(i%+5%)            , ch(25),~
               at (16,36), fac(hex(84)),   c$(i%+5%)            , ch(09),~
               at (16,47), fac(hex(84)),   s$(i%+5%)            , ch(08),~
               at (16,59), fac(hex(84)),   d$(i%+5%)            , ch(02),~
               at (16,64), fac(hex(84)),   m$(i%+5%)            , ch(04),~
/*EWD010*/     at (16,71), fac(hex(84)),  st$(i%+5%)            , ch(05),~
               at (17,04), fac(lfac$(1%)), o$(i%+6%)            , ch(01),~
/*EWD012*/     at (17,08), fac(hex(84)),  bo$(i%+6%)            , ch(01),~
               at (17,10), fac(hex(84)),   b$(i%+6%)            , ch(25),~
               at (17,36), fac(hex(84)),   c$(i%+6%)            , ch(09),~
               at (17,47), fac(hex(84)),   s$(i%+6%)            , ch(08),~
               at (17,59), fac(hex(84)),   d$(i%+6%)            , ch(02),~
               at (17,64), fac(hex(84)),   m$(i%+6%)            , ch(04),~
/*EWD010*/     at (17,71), fac(hex(84)),  st$(i%+6%)            , ch(05),~
               at (18,04), fac(lfac$(1%)), o$(i%+7%)            , ch(01),~
/*EWD012*/     at (18,08), fac(hex(84)),  bo$(i%+7%)            , ch(01),~
               at (18,10), fac(hex(84)),   b$(i%+7%)            , ch(25),~
               at (18,36), fac(hex(84)),   c$(i%+7%)            , ch(09),~
               at (18,47), fac(hex(84)),   s$(i%+7%)            , ch(08),~
               at (18,59), fac(hex(84)),   d$(i%+7%)            , ch(02),~
               at (18,64), fac(hex(84)),   m$(i%+7%)            , ch(04),~
/*EWD010*/     at (18,71), fac(hex(84)),  st$(i%+7%)            , ch(05),~
               at (19,04), fac(lfac$(1%)), o$(i%+8%)            , ch(01),~
/*EWD012*/     at (19,08), fac(hex(84)),  bo$(i%+8%)            , ch(01),~
               at (19,10), fac(hex(84)),   b$(i%+8%)            , ch(25),~
               at (19,36), fac(hex(84)),   c$(i%+8%)            , ch(09),~
               at (19,47), fac(hex(84)),   s$(i%+8%)            , ch(08),~
               at (19,59), fac(hex(84)),   d$(i%+8%)            , ch(02),~
               at (19,64), fac(hex(84)),   m$(i%+8%)            , ch(04),~
/*EWD010*/     at (19,71), fac(hex(84)),  st$(i%+8%)            , ch(05),~
               at (20,04), fac(lfac$(1%)), o$(i%+9%)            , ch(01),~
/*EWD012*/     at (20,08), fac(hex(84)),  bo$(i%+9%)            , ch(01),~
               at (20,10), fac(hex(84)),   b$(i%+9%)            , ch(25),~
               at (20,36), fac(hex(84)),   c$(i%+9%)            , ch(09),~
               at (20,47), fac(hex(84)),   s$(i%+9%)            , ch(08),~
               at (20,59), fac(hex(84)),   d$(i%+9%)            , ch(02),~
               at (20,64), fac(hex(84)),   m$(i%+9%)            , ch(04),~
/*EWD010*/     at (20,71), fac(hex(84)),  st$(i%+9%)            , ch(05),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L42240
                  call "PRNTSCRN"
                  goto L41080

L42240:        if keyhit% <> 3% then goto L42290
                  x% = (int(i_max%/10) * 10%) + 1%
                  i% = x%
                  goto L41080

L42290:        if keyhit% <> 4% then goto L42340
                  x% = i% - 10%
                  i% = x%
                  goto L41080

L42340:        if keyhit% <> 5% then goto L42390
                  x% = i% + 10%
                  i% = x%
                  goto L41080

L42390:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf2
        if edit% = 2% then L42470     /*  Input Mode             */
            return

L42470: if fieldnr% > 0% then L42620  /*  Edit Mode - Select Fld */
/*SR68443 + */
            gosub check_apcmaster
/*          if apcmaster% = 0% then goto skip_apcmaster          */
/*SR70531*/ if apcmaster% = 0% and userid$ <> "SHP" then goto skip_apcmaster
            pf$(1%)= "                 (4)Previous            " &        ~
                     "                                       "
            pf$(2%)= "(2)First         (5)Next                " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "(3)Last                                 " &        ~
                     "                       (16)Exit Load   "
            pfkeys$ = hex(ff02030405ffffffffffffffffff0f1000)
            goto skip_apcmaster2

        skip_apcmaster
/*SR68443 - */
            pf$(1%)= "                 (4)Previous            " &        ~
                     "                       (14)Update Data "
            pf$(2%)= "(2)First         (5)Next                " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "(3)Last          (9)Update Total Load   " &        ~
                     "                       (16)Exit Load   "
            pfkeys$ = hex(ff02030405ffffff09ffffffff0e0f1000)
        skip_apcmaster2                                  /*SR68443 */
            if (i% + 10%) <= i_max% then goto L42580     /* Check Last */
               str(pf$(3%),1%,10%) = " "  : str(pfkeys$,3%,1%) = hex(ff)
               str(pf$(2%),18%,12%) = " " : str(pfkeys$,5%,1%) = hex(ff)
L42580:     if i% > 1% then goto L42600                  /* Check Prev */
               str(pf$(1%),18%,12%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L42600:     if scr_code% < 3% then return
               str(pf$(3%),18%,21%) = " " : str(pfkeys$,9%,1%) = hex(ff)
            return
L42620:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *          C H E C K   L I S T S C R E E N                  *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
            gosub set_pf3
L43060:     accept                                                       ~
               at (01,02),                                               ~
                  "*** Check List Screen for 'Updating Inventory' ***",  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
                                                                         ~
               at (03,21), fac(hex(84)), scr1$(1%)              , ch(40),~
               at (04,21), fac(hex(84)), scr1$(2%)              , ch(40),~
               at (05,21), fac(hex(84)), scr1$(3%)              , ch(40),~
               at (06,21), fac(hex(84)), scr1$(4%)              , ch(40),~
               at (07,21), fac(hex(84)), scr1$(5%)              , ch(40),~
               at (08,21), fac(hex(84)), scr1$(6%)              , ch(40),~
               at (09,21), fac(hex(84)), scr1$(7%)              , ch(40),~
               at (10,21), fac(hex(84)), scr1$(8%)              , ch(40),~
               at (11,21), fac(hex(84)), scr1$(9%)              , ch(40),~
               at (12,21), fac(hex(84)), scr1$(10%)             , ch(40),~
               at (13,21), fac(hex(84)), scr1$(11%)             , ch(40),~
               at (14,21), fac(hex(84)), scr1$(12%)             , ch(40),~
               at (15,21), fac(hex(84)), scr1$(13%)             , ch(40),~
               at (16,21), fac(hex(84)), scr1$(14%)             , ch(40),~
               at (17,21), fac(hex(84)), scr1$(15%)             , ch(40),~
               at (18,21), fac(hex(84)), scr1$(16%)             , ch(40),~
               at (19,21), fac(hex(84)), scr1$(17%)             , ch(40),~
               at (20,21), fac(hex(84)), scr1$(18%)             , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L43420
                  call "PRNTSCRN"
                  goto L43060

L43420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
            gosub screen_message_3
            inpmessage$ =                                                ~
               "Verify All Check List Criteria Before Continuing,PF(9)? "
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                   (9)Continue Inventory" &        ~
                     " Update                (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffff09ffffffffff0f1000)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50140,         /* Screen Code           */ ~
                              L50330,         /* Load Number           */ ~
                              L50550,         /* Drop Number           */ ~
                              L50750          /* Sales Order (AWD020)  */

            return

L50140: REM Screen Code                           SCR_CODE$
            if scr_code$ <> " " then goto L50170
               scr_code$ = "1"
L50170:     scr_code% = 0%
            convert scr_code$ to scr_code%, data goto L50270

            if scr_code% < 1% or scr_code% > 7% then goto L50270
			if scr_code% = 6% then goto L50270             /* CR3300   */
                                                           /* (EWD012) */
                                                           /* (EWD018) */
               if scr_code% = 1% then scr_msg$ = "Update Staging Data  "
               if scr_code% = 2% then scr_msg$ = "Update Loading Data  "
               if scr_code% = 3% then scr_msg$ = "Remove from Load     "
               if scr_code% = 4% then scr_msg$ = "Update Inventory     "
               if scr_code% = 5% then scr_msg$ = "Update RGA         "
/* CR3300 */
               if scr_code% = 7% then scr_msg$ = "Update 300 RGA     "
			   if scr_code% = 7% and schema% <> 1 then goto L50271
                                                  /* (EWD003)           */
        return
L50270:     errormsg$ = "(Error) - Invalid Update Selection(1 thru 5 and 7)?"
            gosub error_prompt
            init(" ") scr_msg$, scr_code$
            scr_code% = 0%
        return
L50271:     errormsg$ = "(Error) - Option not valid at your location?"
            gosub error_prompt
            init(" ") scr_msg$, scr_code$
            scr_code% = 0%
        return
		
L50330: REM Screen Load Number                    SCR_LOAD$
            if scr_load$ <> " " then goto L50360
               goto L50490
L50360:     scr_load% = 0%                                   /*  (EWD017) */
REM            if pos("AS" = str(scr_load$,1%,1%)) > 0 then goto L50420
               convert scr_load$ to scr_load%, data goto L50420

               convert scr_load% to scr_load$, pic(00000)
               goto L50450
L50420:     convert str(scr_load$,2%,4%) to scr_load%, data goto L50490

            convert scr_load% to str(scr_load$,2%,4%), pic(0000)
L50450:     ld_load$ = scr_load$            /* Remove GOSUB STOCK_LOAD */
            gosub lookup_load
            if apc% = 0% then goto L50490
        return
L50490:     errormsg$ = "(Error) - Invalid Load Number?"
            gosub error_prompt
            init(" ") ld_dtp1$, ld_dte1$, ld_dtp2$, ld_dte2$, ld_dtp3$,  ~
                      ld_dte3$, apc_desc$, scr_load$, sav_load$, ld_load$
        return

L50550: REM Screen Drop Number                     SCR_DROP$
            scr_drop% = 0%
            gosub screen_message_2
            if scr_drop$ <> " " then goto L50590
               return
L50590:     init(" ") sc_key1$
            str(sc_key1$,1%,5%) = ld_load$
L50610:     read #4,key 1% > sc_key1$, using L50630, sc_key1$,            ~
                                                           eod goto L50670
L50630:        FMT XX(6), CH(27)
            if str(sc_key1$,1%,5%) <> scr_load$ then goto L50670
            if str(sc_key1$,11%,2%)  <> scr_drop$ then goto L50610
            convert scr_drop$ to scr_drop%, data goto L50670
                                                  /* (EWD001) Fix    */
        return
L50670:     errormsg$ = "(Error) - Invalid Drop Number For Load?"
            gosub error_prompt
            init(" ") scr_drop$ : scr_drop% = 0%    /* (EWD001)        */
        return

L50750: REM Screen Sales Order                   SCR_SO$    /* (AWD020) - BEG*/
            if scr_so$ <> " " then goto L50790
               return
L50790:     init(" ") sc_key1$
            str(sc_key1$,1%,5%) = ld_load$
L50710:     read #4,key 1% > sc_key1$, using L50630, sc_key1$,            ~
                                                           eod goto L50670

            if str(sc_key1$,1%,5%) <> scr_load$ then goto L50770
            if str(sc_key1$,18%,8%)  <> scr_so$ then goto L50710
REM                 call "SHOSTAT" ("SO " & str(sc_key1$,18%,8%)) stop

        return
L50770:     errormsg$ = "(Error) - Invalid Sales Order Number For Load?"
            gosub error_prompt
            init(" ") scr_so$                 /* (AWD020)  - END   */




        deffn'152(fieldnr%)
            errormsg$ = " "
            gosub L50780                      /* Update Flag           */

            return

L50780: REM Update Flag                           O$()
        return

        lookup_load                          /* (APCPLNLD) - Load File */
          if sav_load$ = ld_load$ then return
            apc% = 0%
            read #15,key = ld_load$, using L50860, ld_desc$, ld_dtp1$,    ~
                           ld_dtp2$, ld_dtp3$, eod goto lookup_load_done
L50860:       FMT POS(16), CH(30), POS(70), 3*CH(8)
            ld_dte1$ = str(ld_dtp1$,1%,6%)   /* Planned Prod. Unformat */
            ld_dte2$ = str(ld_dtp2$,1%,6%)   /* Planned Comp. Unformat */
            ld_dte3$ = str(ld_dtp3$,1%,6%)   /* Planned Load. Unformat */
            call "DATEFMT" (ld_dtp1$)        /* Planned Prod. Date     */
            call "DATEFMT" (ld_dtp2$)        /* Planned Comp. Date     */
            call "DATEFMT" (ld_dtp3$)        /* Planned Load  Date     */
            sav_load$ = ld_load$
            apc_desc$ = ld_desc$
            apc% = 1%
        lookup_load_done
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55000: % ######## ########                                   ###########~
        ~#################################                     Page: ####

L55010: % Load No: #####         Drop No.      (##)        D E L I V E R ~
        ~Y   T I C K E T                           Ship Date : ##########

L55015: % Purchase Order: ################      Job Name: ###############~
        ~#####                                     Sales Order: ########

L55020: %     Customer No.      Phone Number                       Terms ~
        ~                   FOB                        Trailer

L55030: %      #########       ##############                ############~
        ~########     ####################             ########


REM L55025 %     Phone Number               Sales Order       Job Name      ~
        ~            Trailer

REM L55035 %     ##############              ########         ##############~
        ~##          #####

L55040: %     Sold To: ##############################                    ~
        ~     Ship To: ##############################

L55050: %              ##############################                    ~
        ~              ##############################

L55060: %              ##############################                    ~
        ~              ##############################

L55070: %              ##################, ## #########                  ~
        ~              ##################, ## #########


L55080: % <LN>  UT  SC  PT <                  Description                ~
        ~                            >  <          Size         >  <CONFIG>

L55090: % ----  --  --  --  ---------------------------------------------~
        ~-----------------------------  -------------------------  --------

L55100: %  ##  ###  ##  ##  #############################################~
        ~#############################  #########################   #######

L55105: %  ##  ###  ##  ##     ##########################################~
        ~#############################  #########################   #######

L55125: %  ##  ###  ##  ##        #######################################~
        ~#############################  #########################   #######

L55135: %                   Room/Location: ##############################
        ~


L55110: %       ==  ==  ==

L55115: % Purchase Order: ################          Job Name: ###########~
        ~#########          Sales Order: ########


L55120: %      ### ### ###                                               ~
        ~                                                           XX####XX

L55130: % Drop (##) Total:

L55140: % Load (#####) Total:

L55150: %     #### ### ###                                               ~
        ~                                                            XX###XX




REM     NORANDEX PRINT STATEMENTS

NOR_L55080: % <LN>  UT  SC  PT  <          Size         > <                  ~
            ~Description                                            >  <CONFIG>

NOR_L55090: % ----  --  --  --  -------------------------  ------------------~
            ~--------------------------------------------------------  --------

NOR_L55100: % ###   ###  ##  ##  #########################  ###################~
            ~#######################################################   #######

NOR_L55105: % ###   ###  ##  ##  #########################     ################~
            ~#######################################################   #######

NOR_L55125: % ###   ###  ##  ##  #########################        #############~
            ~#######################################################   #######

NOR_L55135: %                    Room/Location: ##############################



        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        screen_message_1
            init(" ") scr$()
               scr$( 1%) =  "******************************"
               scr$( 2%) =  "*      Update Selections     *"
               scr$( 3%) =  "*                            *"
               scr$( 4%) =  "* (1) - Update Staging       *"
               scr$( 5%) =  "* (2) - Update Loading       *"
               scr$( 6%) =  "* (3) - Remove from Load     *"
               scr$( 7%) =  "* (4) - Update Inventory     *"
REM            scr$( 8%) =  "* (5) - Backorder From Load  *"
/*(EWD012)*/   scr$( 8%) =  "* (5) - Update RGA           *"
/*(EWD018)*/   scr$( 9%) =  "* (7) - Update 300 RGA       *"
/* CR3300 */   scr$(10%) =  "*                            *"
               scr$(11%) =  "******************************"
        return

        screen_message_2
            init(" ") o$(), b$(), c$(), s$(), d$(), m$(), s1$(), hdr$()
            s1$(1%)= "**************************************************"
            s1$(2%)= "*         x                            x         *"
            s1$(3%)= "**************************************************"

            if scr_code% = 1% then str(s1$(2%),11%,30%) =                ~
                                   " Update Staging Data for Load "
            if scr_code% = 2% then str(s1$(2%),11%,30%) =                ~
                                   " Update Loading Data for Load "
            if scr_code% = 3% then str(s1$(2%),11%,30%) =                ~
                                   " Remove from Loading Schedule"
/* (CR456) */
            if scr_code% = 5% then str(s1$(2%),11%,30%) =                ~
                                   " Update RGA Data for Load    "
/* CR3300 */
            if scr_code% = 7% then str(s1$(2%),11%,30%) =                ~
                                   " Update 300 RGA Data for Load"
								   
            hdr$(1%) = "Update"
            hdr$(2%) = "<---- Part Number ----->"
            hdr$(3%) = "Customer "
            hdr$(4%) = "Sales Ord"
            hdr$(5%) = "Drop"
            hdr$(6%) = "M/P "
            hdr$(7%) = "Seq No"                       /* (EWD010) */
            if sel% = 1% then hdr$(2%) = "<--- Barcode Number --->"
        return

        update_staging
        update_loading
        closeout_load
        backorder_load                       /* (EWD003) - Backorder */
            if inc% = 1% then call "SHOSTAT" ("Updating Staging Data")
            if inc% = 2% then call "SHOSTAT" ("Updating Loading Data")
            if inc% = 3% then call "SHOSTAT" ("Removing From Load "  )
            if inc% = 5% then call "SHOSTAT" ("Update RGA Status  "  )
			   if inc% = 6% then call "SHOSTAT" ("Update RGA Status  "  )
                                             /* (EWD003) -           */
/* CR3300 */											 
               if inc% = 7% then call "SHOSTAT" ("Update 300 RGA Status" )
                                    /* Only Update Parts Selected */
                                    /* Update Based on Status of  */
            for i% = 1% to i_max%   /* (14)-Staged, (16)-Loaded   */
                if o$(i%) = " " then goto L60260
                   dt_bar$ = bb$(i%)
                                           /* (PAR001)              */
                                           /* Pass - (1) (APCPLNDT) */
                   call% = 1%              /* (EWD012)              */
                   pass% = 1%
                   call "EWDPLA44" (call%,  /* 1 = APCPA44 2=EWDPLN79   */ ~
                                    pass%,  /* 1 = Updte St 2= Updte Hdr*/ ~
                                    scr_code%, /* Screen Code Selection */ ~
                                    scr_load$, /* Load Number           */ ~
                                    dt_bar$,   /* Bar Code Number       */ ~
                                    userid$,   /* UserID                */ ~
                                    " ",       /* Authorized ID         */ ~
                                    " ",       /* Area Code             */ ~
                                    " ",       /* Found Code            */ ~
                                    " ",       /* Reason Code           */ ~
                                    " ",       /* Text Area             */ ~
                                    #2,        /* APCPLNDT              */ ~
                                    #3,        /* APCPLNOR              */ ~
                                    #4,        /* APCPLNSC              */ ~
                                    #5,        /* GENCODES              */ ~
                                    #6,        /* APCPLNAD              */ ~
                                    #10,       /* EWDBOLRM              */ ~
                                    #12,       /* EWDPRDLB (PAR001)     */ ~
                                    #13,       /* EWDBOLBK              */ ~
                                    #23,       /* PGORLNTR              */ ~ 									
                                    err%       /* Error Code            */ )
L60260:     next i%
                                           /* Pass - (2) (APCPLNSC) */
                   call% = 1%              /* (EWD012)              */
                   pass% = 2%
                   call "EWDPLA44" (call%,  /* 1 = APCPA44 2=EWDPLN79   */ ~
                                    pass%,  /* 1 = Updte St 2= Updte Hdr*/ ~
                                    scr_code%, /* Screen Code Selection */ ~
                                    scr_load$, /* Load Number           */ ~
                                    dt_bar$,   /* Bar Code Number       */ ~
                                    userid$,   /* UserID                */ ~
                                    " ",       /* Authorized ID         */ ~
                                    " ",       /* Area Code             */ ~
                                    " ",       /* Found Code            */ ~
                                    " ",       /* Reason Code           */ ~
                                    " ",       /* Text Area             */ ~
                                    #2,        /* APCPLNDT              */ ~
                                    #3,        /* APCPLNOR              */ ~
                                    #4,        /* APCPLNSC              */ ~
                                    #5,        /* GENCODES              */ ~
                                    #6,        /* APCPLNAD              */ ~
                                    #10,       /* EWDBOLRM              */ ~
                                    #12,       /* EWDPRDLB              */ ~
                                    #13,       /* EWDBOLBK              */ ~
                                    #23,       /* PGORLNTR              */ ~								
                                    err%       /* Error Code            */ )
        return




        delete_pull_stock
          pull% = 0%
          call "APCPULSB" (2%, " ", " ", " ", dt_so$, dt_item$, " ", " ",     ~
                               0%, 0%, #1, pull%)
        return

        update_load_info                 /* Check to see if Staged or  */
            gosub dataload               /* Load   /* CHECK COMPLETE   */
            if i_max% <> 0% then return
            if str(scr_load$,1%,1%) = "S" then return /*SKIP STOCK LOAD*/
            if str(scr_so$,1%,1%) <> " " then return   /* (AWD022)*/

/* (AWD021) */
            read #15,hold,key = scr_load$, eod goto L60965
               put #15, using L60945, dt_st$, date, userid$, time, "AWD"
L60945:        FMT POS(94), CH(2), CH(6), POS(111), CH(03), CH(04), CH(03)
            rewrite #15
        return
            errormsg$ = "(Error) - Updating Load Status ("&scr_load$&")"
L60965:     gosub error_prompt
            init(" ") errormsg$
        return


        REM *************************************************************~
            *           B O L   R E P O R T   L O G                     *~
            *************************************************************

                                                   /* REPORT HEADER */
L61010: %!######## ########   ########################################   ~
        ~     APCPLA44: !

L61025: %!User Id: ###        ########################################   ~
        ~   Page: ##### !
                                                   /* COLUMN 1 HEADER */
L61040: %!Load Number: #####                                             ~
        ~               !

L61055: %!<- P.O. Number >!Sls  Ord!Dp! Qty!<Cust>!<--- Customer Name ---~
        ~>!Bol Date! Who!
L61065: %+---------------------------------------------------------------~
        ~---------------+

L61080: %!                                                               ~
        ~               !
L61090: %!---------------------------------------------------------------~
        ~---------------!
L61100: %!----------------!--------!--!----!------!----------------------~
        ~-!--------!----!
L61110: %!################!########!##!####!######!######################~
        ~#!__/__/__!____!
        REM            X

L61130: %!BOL'S TOTAL:[ ### ] ADJUSTMENTS: __________ = TOTAL: __________~
        ~               !

        select_printer
            page_no% = 0%
            lcnt%    = 99%
            call "FMTTITLE" (print_title$, " ", 12%)
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCLDT", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCLDT", " ", 0%, 1%)
        return

        generate_report                           /* (EWD004) - Begin */
            init(" ") sav_so$, sc_key1$, sav_cust$
            call "SHOSTAT" ("Creating BOL'S Report Log ")
            print_title$ = "BOL'S Work Sheet Report Log "
            gosub select_printer

            gosub delivery_sort
            count% = 0% : wrk_qty% = 0%
            init(" ") wrk_key$
            read #9,key > wrk_key$, using L62255, wrk_key$, wrk_rec$, ~
                                                eod goto generate_done
            str(sav_rec$,1%,60%)   = wrk_key$
            str(sav_rec$,61%,140%) = wrk_rec$
            goto L61275
        generate_next
            read #9, using L62255, wrk_key$, wrk_rec$,                ~
                                               eod goto generate_done
L61275:     REM if str(sav_rec$,1%,21%) <> str(wrk_key$,1%,21%) then      ~
                                               goto L61280

               if str(sav_rec$,1%,37%) <> str(wrk_key$,1%,37%) then      ~
                                               goto L61280
               wrk_qty% = wrk_qty% + 1%
               goto generate_next

L61280:     convert wrk_qty% to wrk_qty$, pic(####)
            gosub print_detail

            str(sav_rec$,1%,60%)    = wrk_key$
            str(sav_rec$,61%,140%)  = wrk_rec$
            wrk_qty% = 0%
            goto L61275
        generate_done
            convert wrk_qty% to wrk_qty$, pic(####)
            gosub print_detail

            gosub print_totals
            gosub close_printer
            gosub delete_work
        return

        lookup_customer
            if sav_cust$ = dt_cust$ then return
               sav_cust$ = dt_cust$
                                                    /* Ship To Info   */
            init(" ") cust_desc$, cust_address$()         /* (AWD024) */
            read #11,key = dt_cust$, using L61405, cust_desc$, cust_address$(),~
                                      city$, state$, zip$, cus_contact$, ~
                                      cus_phone$, eod goto L61415
L61405:        FMT POS(10), CH(30), POS(253), 3*CH(30), POS(403), CH(18),~
                   CH(2), XX(1), CH(9), POS(433), CH(20), CH(10)
        return

        lookup_customer_billto
            init(" ") bill_desc$(), bill_city$, bill_state$, bill_zip$
                                                     /* Bill To Info   */
            read #11,key = dt_cust$, using L61410, bill_desc$(),         ~
                                     bill_city$, bill_state$, bill_zip$, ~
                                                   eod goto L61415
L61410:        FMT POS(40), 3*CH(30), POS(190), CH(18), CH(2), CH(9)

L61415: return

        lookup_po                                 /* (APCPLNOR) - File */
            init(" ") or_key4$, or_po$, or_drop$
            or_key4$ = str(dt_bar$,1%,8%)
/*          read #3,key 4% = or_key4$, using L61450, or_drop$, or_po$,    ~
                                             eod goto L61455  */
/*AWD026*/  read #3,key 4% = or_key4$, using L61450, or_drop$, or_po$,    ~
                                       or_status$, eod goto L61455
/*L61450:        FMT POS(25), CH(2), POS(36), CH(16)*/
L61450:        FMT POS(25), CH(2), POS(36), CH(16), POS(60), CH(2)
L61455: return

        print_header
          if lcnt% <> 99% then print using L61065
          page_no% = page_no% + 1%
          print page
          print using L61065
          print using L61010, date$, rpt_time$, company$
          print using L61025, userid$, print_title$, page_no%
          print using L61080
          print using L61040, scr_load$
          print using L61090
          print using L61055
          lcnt% = 7%
        return

        print_detail
          if lcnt% > 35% then gosub print_header
          print using L61100
          print using L61110, str(sav_rec$,120%,16%), str(sav_rec$,166%,8%),~
                              str(sav_rec$,6%,2%), wrk_qty$,               ~
                              str(sav_rec$,8%,6%), str(sav_rec$,61%,23%)
          
          lcnt% = lcnt% + 2%
          count% = count% + 1%
         return

        print_totals
          convert count% to count$, pic(###)

          if lcnt% > 35% then gosub print_header
          print using L61090
          print using L61130, count$
          print using L61065
        return

        delivery_ticket                         /* (EWD004) - Begin */
/*AWD026*/  warn_flag = 0%

            if ora_connect% = 0% then gosub connect
            init(" ") sav_so$, sc_key1$, sav_cust$, sav_po$, sav_po1$, ~
                      sav_drop$
            gosub select_printer
            gosub lookup_trailer

            call "SHOSTAT" ("Sorting Load Data")

            gosub delivery_sort
/*AWD026 + */
            if warn_flag = 0% then goto skip_warn_mess
               gosub warn_mess
               if comp% = 8 then goto skip_warn_mess
                  return clear all
                  goto inputmode
        skip_warn_mess
/*AWD026 - */
            mat tt = zer
            mat drop_tt = zer
            mat load_tt = zer                 /* (AWD020) */

            call "SHOSTAT" ( "Creating Delivery Tickets for Load ("& ~
                                       scr_load$ &")" ) 
            init(" ") wrk_key$, wrk_rec$, sav_rec$, sav_cust$,      ~
                      sav_po$, sav_po1$, sav_drop$
            read #9,key > wrk_key$, using L62255, wrk_key$, wrk_rec$, ~
                                         eod goto delivery_ticket_done
            str(sav_rec$,1%,60%)   = wrk_key$
            str(sav_rec$,61%,140%) = wrk_rec$
            sav_cust$ = str(wrk_key$,8%,9%)
            sav_po1$  = str(wrk_key$,17%,16%)
            sav_drop$ = str(wrk_key$,6%,2%)
            goto L61290
        delivery_ticket_nxt
            read #9,key > wrk_key$, using L62255, wrk_key$, wrk_rec$, ~
                                         eod goto delivery_ticket_done

                                     /* Notes - All barcodes are in file */
                                     /* counter is changed every time    */
                                     /* have a new drop and every time   */
                                     /* have a new po number.  So 1 - 21 */
                                     /* on key is through counter        */
                              /* wrk key = load(5), drop(2), customer(9) */
                              /* po(16), counter(5), bar(18), dept(3)    */
                              /* proc(2)                                 */
L61290:    if str(sav_rec$,1%,37%) <> str(wrk_key$,1%,37%) then      ~
                                               goto L61300
               goto delivery_ticket_nxt

L61300:
REM            call "SHOSTAT" (" LOOK UP DELIVERY DETAIL ")   stop
            /* Note may be good place to ensure ora desc is in file */
REM            if str(sav_rec$,166%,8%) <> "02099945" then goto not_so
REM                call "SHOSTAT" (" LOOK UP DELIVERY DETAIL ")    stop
REM not_so
            init(" ") oradesc_key$
            str(oradesc_key$,1%,8%) = str(sav_rec$,166%,8%)
            gosub check_oradesc               /* check for sales order */
            gosub lookup_job_name             /* job and fob           */

REM IF SAV_PO1$ <> STR(WRK_KEY$,17%,16%) THEN GOSUB PO_HEADER
REM IF SAV_DROP$ <> STR(WRK_KEY$,7%,2%) THEN GOSUB DELIVERY_TOTALS
REM SAV_PO1$  = STR(WRK_KEY$,17%,16%)

            gosub lookup_delivery_detail        /* Print Lines */



            if sav_drop$ <> str(wrk_key$,6%,2%) then                  ~
                                                gosub delivery_totals
            if sav_po1$ <> str(wrk_key$,17%,16%) then                  ~
                                                gosub delivery_sub_totals

            str(sav_rec$,1%,60%)    = wrk_key$
            str(sav_rec$,61%,140%)  = wrk_rec$
            sav_cust$ = str(wrk_key$,8%,9%)
            sav_drop$ = str(wrk_key$,6%,2%)
            sav_po1$  = str(wrk_key$,17%,16%)
            goto L61290
        delivery_ticket_done
            init(" ") oradesc_key$
            mat tt = zer
            str(oradesc_key$,1%,8%) = str(sav_rec$,166%,8%)
            gosub check_oradesc               /* check for sales order */
            gosub lookup_job_name

            gosub lookup_delivery_detail
            gosub delivery_totals
            gosub load_totals                 /* (AWD020) */
            gosub close_printer
            gosub delete_work
            if ora_connect% <> 0% then gosub disconnect

        return clear all
        goto inputmode

        delivery_header
          gosub lookup_delivery_header


          call "STRING" addr ("CT", bck_terms$, 20%)
          call "STRING" addr ("CT", bck_fob$, 20%)
          call "STRING" addr ("CT", awdtrail$, 8%)
          dt_so$   = str(sav_rec$,166%,8%)
          bck_po$  = str(sav_rec$,120%,16%)


          page_no% = page_no% + 1%
          print page
          print                                          /*  (EWD015)  */
          print using L55000, date$, rpt_time$, company$, page_no%
          print                                   /* Delivery Ticket */
          print using L55010, scr_load$, or_drop$, bck_ship$
          print using L55015, bck_po$, bck_job$, dt_so$
          print
          print using L55020                     /* Customer Info   */
          print using L55030, dt_cust$, bck_phone$, bck_terms$,    ~
                              bck_fob$, awdtrail$
          print
          print using L55040, bill_desc$(1%), cust_addr$(1%)
          print using L55050, bill_desc$(2%), cust_addr$(2%)
          print using L55060, bill_desc$(3%), cust_addr$(3%)
          print using L55070, bill_city$, bill_state$, bill_zip$,      ~
                              city$, state$, zip$
          print

          if str(dt_cust$,1%,2%) = "NO" or                      ~
                 str(dt_cust$,1%,2%) = "RE"                    ~
                                   then gosub print_col_nor    ~
          else gosub print_col_all


          lcnt% = 16%
        return

        print_col_all
          print using L55080                     /* Column Headings   */
          print using L55090
        return

        print_col_nor
          print using nor_L55080                 /* Column Headings   */
          print using nor_L55090
        return


        po_header
           if (lcnt% + 4%) >= 40% then goto delivery_header     ~
           else gosub delivery_sub_totals




           dt_so$   = str(sav_rec$,166%,8%)
           bck_po$  = str(sav_rec$,120%,16%)

           print using L55115, bck_po$, bck_job$, dt_so$
           print

           lcnt% = lcnt% + 2%

        return


        delivery_detail
          init(" ") prt_wind$, prt_scrn$, prt_part$, prt_line$

                                                 /* (EWD005)    */
                                        /*  (EWD016) Add WW Ln */
          if lcnt% > 35% then gosub delivery_header
          call "STRING" addr ("RJ", size_desc$, len(str(size_desc$)))
          call "STRING" addr ("RJ", config_qty$, len(str(config_qty$)))

          if prev_ww$ <> " " and                              ~
                 (prev_ww$ <> str(oradesc_key1$,9%,3%)) then print

          if prev_ww$ <> " " and                              ~
                 (prev_ww$ <> str(oradesc_key1$,9%,3%))       ~
                 then lcnt% = lcnt% + 1%


          if prev_ww$ <> str(oradesc_key1$,9%,3%) then sav_config$ = " "
          if sav_config$ = "1" and window$ = "1" then window% = 0%

          if window% <> 0% then convert window% to prt_wind$, pic(###)
          if scrn%   <> 0% then convert   scrn% to prt_scrn$, pic(##)
          if part%   <> 0% then convert   part% to prt_part$, pic(##)

          prt_line% = 0%
          convert str(oradesc_key1$,10%,2%) to prt_line%, data goto prt_lne_err

prt_lne_err:

          convert prt_line% to prt_line$, pic(00)


          if str(dt_cust$,1%,2%) = "NO" or                       ~
                 str(dt_cust$,1%,2%) = "RE"                      ~
                                     then gosub print_detail_nor ~
          else gosub print_detail_all


REM          lcnt% = lcnt% + 1%

          prev_ww$    = str(oradesc_key1$,9%,3%)
          if sav_config$ = " " then sav_config$ = ora_config$
          gosub add_qty
        return


REM PRINT_DETAIL_ALL
          if prev_ww$ = str(oradesc_key1$,9%,3%) then              ~
             print using L55105, " ",                              ~
/*Configuration*/                prt_wind$, prt_scrn$, prt_part$,  ~
                                 str(part_desc$,1%,71%),size_desc$,~
                                 config_qty$                       ~
          else                                                     ~
             print using L55100, prt_line$,                        ~
/*NotConfiguration*/             prt_wind$, prt_scrn$, prt_part$,  ~
                                 str(part_desc$,1%,74%),size_desc$,~
                                 config_qty$


          if prev_ww$ = str(oradesc_key1$,9%,3%) then              ~
             print using L55105, " ",                              ~
/*Configuration*/                " ", " ", " ",                    ~
                                 str(part_desc$,72%,71%)," ",      ~
                                 " "                               ~
          else                                                     ~
             print using L55100, " ",                              ~
/*NotConfiguration*/             " ", " ", " ",                    ~
                                 str(part_desc$,75%,74%)," ",      ~
                                 " "


        return


        print_detail_all
          if prev_ww$ = str(oradesc_key1$,9%,3%) then              ~
                                gosub print_all_config             ~
             else               gosub print_all_not_config

        return

        print_all_config
REM  Do not indent screens
         if prt_scrn$ <> " " then goto print_all_not_config
         print using L55105, " ",                              ~
/*Configuration*/            prt_wind$, prt_scrn$, prt_part$,  ~
                             str(part_desc$,1%,71%),size_desc$,~
                             config_qty$

          lcnt% = lcnt% + 1%
REM IF STR(PART_DESC$,72%,71%) = " " THEN RETURN
REM Rule must have at least 20 characters to print on second line
          bgPos%, edPos% = 0%
          bgPos% = 72%    :    edPos% = 71%
          if str(part_desc$,92%,51%) <> " " then gosub print_second

          return
          if roomLoc$ <= " " then return
            print using L55135, roomLoc$
            lcnt% = lcnt% + 1%
        return
        print_second
          print using L55125, " ",                               ~
/*Configuration*/             " ", " ", " ",                     ~
                              str(part_desc$,bgPos%,edPos%)," ", ~
                              " "
          lcnt% = lcnt% + 1%
        return



        print_all_not_config
             print using L55100, prt_line$,                        ~
/*NotConfiguration*/             prt_wind$, prt_scrn$, prt_part$,  ~
                                 str(part_desc$,1%,74%),size_desc$,~
                                 config_qty$
          lcnt% = lcnt% + 1%
REM IF STR(PART_DESC$,75%,74%) = " " THEN RETURN
REM Rule must have at least 20 characters to print on second line
          bgPos%, edPos% = 0%
          bgPos% = 75%    :    edPos% = 71%
          if str(part_desc$,95%,51%) <> " " then gosub print_second

REM PRINT USING L55125, " ",  /*NOTCONFIGURATION*/ " ", " ", " ",      ~
    STR(PART_DESC$,75%,71%)," ", " "
REM LCNT% = LCNT% + 1%
 
          if roomLoc$ <= " " then return
            print using NOR_L55135, roomLoc$
            lcnt% = lcnt% + 1%
    
        return



REM PRINT_DETAIL_NOR
          if prev_ww$ = str(oradesc_key1$,9%,3%) then              ~
             print using NOR_L55105, " ",                          ~
/*Configuration*/                prt_wind$, prt_scrn$, prt_part$,  ~
                                 size_desc$,str(part_desc$,1%,71%),~
                                 config_qty$                       ~
          else                                                     ~
             print using NOR_L55100, prt_line$,                    ~
/*NotConfiguration*/             prt_wind$, prt_scrn$, prt_part$,  ~
                                 size_desc$,                       ~
                                 str(part_desc$,1%,74%), config_qty$


          if prev_ww$ = str(oradesc_key1$,9%,3%) then              ~
             print using NOR_L55105, " ",                          ~
/*Configuration*/                " ", " ", " ",                    ~
                                 " ",str(part_desc$,72%,71%),      ~
                                 " "                               ~
          else                                                     ~
             print using NOR_L55100, " ",                          ~
/*NotConfiguration*/             " ", " ", " ",                    ~
                                 " ",                              ~
                                 str(part_desc$,75%,74%), " "

        return



        print_detail_nor
          if prev_ww$ = str(oradesc_key1$,9%,3%) then              ~
                                gosub print_nor_config             ~
             else               gosub print_nor_not_config

        return

        print_nor_config
REM  Do not indent screens
          if prt_scrn$ <> " " then goto print_nor_not_config
             print using NOR_L55105, " ",                          ~
/*Configuration*/                prt_wind$, prt_scrn$, prt_part$,  ~
                                 size_desc$,str(part_desc$,1%,71%),~
                                 config_qty$

          lcnt% = lcnt% + 1%
REM IF STR(PART_DESC$,72,71%) = " " THEN RETURN
REM Rule must have at least 20 characters to print on second line
          bgPos%, edPos% = 0%
          bgPos% = 72%    :    edPos% = 71%
          if str(part_desc$,92%,51%) <> " " then gosub print_second_nor


REM  PRINT USING NOR_L55125, " ",                          ~
/*CONFIGURATION*/ " ", " ", " ",                    ~
                  " ", STR(PART_DESC$,72%,71%),     ~
                  " "
          lcnt% = lcnt% + 1%
          if roomLoc$ <= " " then return
            print using NOR_L55135, roomLoc$
            lcnt% = lcnt% + 1%
        return


        print_nor_not_config
          print using NOR_L55100, prt_line$,                    ~
/*NotConfiguration*/          prt_wind$, prt_scrn$, prt_part$,  ~
                              size_desc$,str(part_desc$,1%,74%),~
                              config_qty$
          lcnt% = lcnt% + 1%
REM IF STR(PART_DESC$,75%,74%) = " " THEN RETURN
REM Rule must have at least 20 characters to print on second line
          bgPos%, edPos% = 0%
          bgPos% = 75%    :    edPos% = 71%
          if str(part_desc$,95%,54%) = " " then gosub print_second_nor

          if sav_config$ <> "0" then return
          if roomLoc$ <= " " then return
            print using NOR_L55135, roomLoc$
            lcnt% = lcnt% + 1%
        return
        print_second_nor
          print using NOR_L55125, " ",                            ~
/*NotConfiguration*/          " ", " ", " ",                      ~
                              " ", str(part_desc$,bgPos%,edPos%), ~
                              " "

          lcnt% = lcnt% + 1%
        return


        add_qty

           cmg% = 0%
           tt(1%) = tt(1%) + window%
           tt(2%) = tt(2%) + scrn%
           tt(3%) = tt(3%) + part%

           convert str(config_qty$,5%,3%) to cmg%, data goto no_add

                 tt(4%) = tt(4%) + cmg%
no_add:

           drop_tt(1%) = drop_tt(1%) + window%
           drop_tt(2%) = drop_tt(2%) + scrn%
           drop_tt(3%) = drop_tt(3%) + part%
           drop_tt(4%) = drop_tt(4%) + cmg%

/*(AWD020) - Load Totals */
           load_tt(1%) = load_tt(1%) + window%
           load_tt(2%) = load_tt(2%) + scrn%
           load_tt(3%) = load_tt(3%) + part%
           load_tt(4%) = load_tt(4%) + cmg%

        return



        delivery_sub_totals
          init(" ") tot$()
          for kk% = 1% to 4%
              convert int(tt(kk%)) to tot$(kk%), pic(####)

          next kk%
                                                /* (EWD005)     */
          if (lcnt% + 6%) > 40% then gosub delivery_header

REM K% = 40% - LCNT%
REM IF K% < 1% THEN GOTO L61310
REM FOR KK% = 1% TO K%
REM    PRINT
REM NEXT KK%

REM L61310
REM PRINT
          print using L55110
          print using L55120, str(tot$(1%),2%,3%), ~
                              str(tot$(2%),2%,3%), ~
                              str(tot$(3%),2%,3%), ~
                              str(tot$(4%),1%,4%)
          print

REM  PRINT USING L55130, " "             /* (EWD005)    */
REM  PRINT USING L55140, " "
REM  PRINT
REM  PRINT USING L55150                  /* (EWD005)    */
        lcnt% = lcnt% + 3%

          lcnt% = 99%
          mat tt = zer
          page_no% = 0%
        return

        delivery_totals

REM       call "SHOSTAT" (" DELIVERY TOTAL " )   stop
          gosub delivery_sub_totals
          init(" ") drop_tot$()
          for kk% = 1% to 4%
              convert int(drop_tt(kk%)) to drop_tot$(kk%), pic(####)

          next kk%
                                                /* (EWD005)     */
REM          if lcnt% > 60% then gosub delivery_header

          k% = 40% - lcnt%
          if k% < 1% then goto L61310
          for kk% = 1% to k%
                print
          next kk%

L61310:
          print
          print using L55130, or_drop$
          print using L55110
          print using L55120, str(drop_tot$(1%),2%,3%), ~
                              str(drop_tot$(2%),2%,3%), ~
                              str(drop_tot$(3%),2%,3%), ~
                              str(drop_tot$(4%),2%,3%)
          print
        REM  print using L55130, " "             /* (EWD005)    */
        REM  print using L55140, " "
        REM  print
        REM  print using L55150                  /* (EWD005)    */
        REM automatically make sav_po1$ = wrk_key so will not print sub_totals
          sav_po1$ = str(wrk_key$,17%,16%)
          lcnt% = 99%
          mat drop_tt = zer
          page_no% = 0%
        return

/* (AWD020) - Add load totals to the bottom */
        load_totals
          init(" ") load_tot$()
          for kk% = 1% to 4%
              convert int(load_tt(kk%)) to load_tot$(kk%), pic(####)

          next kk%

          if (lcnt% + 6%) > 40% then gosub delivery_header

          print using L55140, scr_load$
          print using L55110
          print using L55150, str(load_tot$(1%),2%,3%), ~
                              str(load_tot$(2%),2%,3%), ~
                              str(load_tot$(3%),2%,3%), ~
                              str(load_tot$(4%),2%,3%)
          print
           lcnt% = lcnt% + 4%

          lcnt% = 99%
          mat load_tt = zer
          page_no% = 0%
        return

        lookup_delivery_header                   /* (BCKMASTR)        */
          city$      = str(sav_rec$,91%,18%)
          state$     = str(sav_rec$,109%,2%)
          zip$       = str(sav_rec$,111%,9%)
          cus_phone$ = str(sav_rec$,156%,10%)

          dt_cust$   = str(sav_rec$,8%,9%)
          dt_so$     = str(sav_rec$,166%,8%)
          or_po$     = str(sav_rec$,120%,16%)
          or_drop$   = str(sav_rec$,6%,2%)

          gosub lookup_customer_billto
          init(" ") bck_key$, bck_terms$, bck_ship$,                     ~
                    bck_phone$, cust_addr$(), userwhoenter$
          str(bck_key$,1%,9%)   = dt_cust$
          str(bck_key$,10%,16%) = dt_so$
          read #7,key = bck_key$, using L61600, cust_addr$(), bck_terms$,~
                                  bck_ship$, userwhoenter$, eod goto cancel_order
                                           /* (AWD024) */
L61600:      FMT POS(42), 6*CH(30), POS(402), CH(20), POS(824), CH(6), ~
                 POS(836), CH(03)

          city$  = str(cust_addr$(6%),01%,18%)
          state$ = str(cust_addr$(6%),19%,02%)
          zip$   = str(cust_addr$(6%),21%,09%)
          bck_phone$ = "(" & str(cus_phone$,1%,3%) & ") " &             ~
                     str(cus_phone$,4%,3%) & "-" & str(cus_phone$,7%,4%)
          call "DATFMTC" (bck_ship$)
          if userwhoenter$ = "M2O" then goto replace_address
          if userwhoenter$ = "STK" then goto replace_address
        return
        replace_address
           init(" ") sav_cust$
           gosub lookup_customer
           cust_addr$(1%) = cust_address$(1%)
           cust_addr$(2%) = cust_address$(2%)
           cust_addr$(3%) = cust_address$(3%)
        return

        cancel_order                                 /* (EWD011)  */
           comp% = 2%
           hdr$ = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - -  Canceled Order !! - - - - - - - "
           msg$(2%) = " Please Note CUSTOMER & SO : " & dt_cust$ & dt_so$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        lookup_job_name                               /* (BCKMASTR)   */
          init(" ") bck_key$, bck_job$, bck_fob$
          str(bck_key$,1%,9%)  = str(sav_rec$,8%,9%)
          str(bck_key$,10%,8%) = str(sav_rec$,166%,8%)

          read #7,key = bck_key$, using L61605, bck_fob$, bck_job$,   ~
                                                   eod goto L61608
L61605:      FMT POS(442), CH(20), POS(619), CH(20)
L61608:   return

        lookup_delivery_detail                        /* (BCKLINES)   */
                                                          /* (AWD019) */
          init(" ") bck_ln_key$, dt_so$, dt_cust$, prev_ww$, sav_ln_key$,~
                    sav_config$, txtid$
          dt_so$   = str(sav_rec$,166%,8%)
          dt_cust$ = str(sav_rec$,8%,9%)
          bck_po$  = str(sav_rec$,120%,16%)
          str(bck_ln_key$,1%,16%) = dt_so$
                                               /* (EWD016) - Get WW Ln */
L61610:   read #8,key > bck_ln_key$, using L61620, bck_ln_key$,        ~
                        bck_part$, bck_desc$, qty(), txtid$, s_1$,     ~
                        bck_ww_ln$, eod goto L61700

L61620:     FMT POS(10), CH(19), POS(32), CH(25), CH(32), POS(93),     ~
                 6*PD(14,4), /*(CR905)*/ POS(242), CH(04),            ~
                 POS(282), CH(2), POS(285), CH(2)

          if dt_so$ <> str(bck_ln_key$,1%,8%) then goto L61700

             gosub glass_warranty                     /* (EWD005)      */
             if glass_warranty% = 1% then goto L61610
                                                      /* (EWD005)      */
             init(" ") bck_tst$, bck_wd$, bck_ht$, bck_d$, bck_mull$
REM MAT TT = ZER
             bck_ln%, bck_ww_ln% = 0%                 /*  (EWD016)     */
             bck_ln$  = str(bck_ln_key$,18%,2%)       /* Line Item No. */
             convert bck_ln$ to bck_ln%, data goto L61625

L61625:      convert bck_ln% to bck_ln$, pic(00)
                                                      /*  (EWD016) - Beg */
             convert bck_ww_ln$ to bck_ww_ln%, data goto L61629

L61629:      if bck_ww_ln% = 0% then bck_ww_ln% = bck_ln%

             convert bck_ww_ln% to bck_ww_ln$, pic(00)

                                                      /*  (EWD016) - End */


REM CALL "SHOSTAT" (" LOOK UP ORACLE DESCRIPTION ")  STOP
             str(sav_ln_key$,1%,19%) = bck_ln_key$
             gosub lookup_text                        /* (CR905) */
             gosub lookup_oradesc                     /* (AWD019) */



             gosub delivery_detail                 /* Line Item Detail */
             goto L61610

L61700: return

REM BUILD_DESCRIPTION                             /* (EWD006) MODS */
            s_23% = 0%
            s_23m$ = str(bck_part$,1%,3%)
            s_so$  = dt_so$
            s_ln$  = bck_ln$
            init(" ") s_prv$                          /* s_1$ Passed In  */
            prv% = 1%                                 /* Use BCKLINES    */
            call "APCPRZSB" (prv%, s_1$, dt_cust$, s_23m$, s_so$,          ~
                                   s_ln$, s_prv$, s_23$, s_23%,            ~
                                   #11, #5, #2, #8, x_er% )
            if x_er% <> 0% then return
               str(bck_d$,1%,8%) = s_23$

        return                                        /* (EWD006) Mods */

REM LOOKUP_COLOR
            init(" ") readkey$, bck_cl$
            str(readkey$,1%,9%) = "COLOR    "
            str(readkey$,10%,15%) = str(bck_part$,4%,1%)
            read #5,key = readkey$, using L61705, bck_cl$, eod goto L61708
L61705:        FMT POS(25), CH(2)
L61708: return

        stock_error
          comp% = 2%
          hdr$ = "****** UPDATE ERROR *****"
          msg$(1) = "Cannot Update Load ("&scr_load$&")"
          msg$(3) = "Press RETURN to Re-Try or PF(16) to Exit."
          call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
          if str(scr_load$,1%,1%) = "S" then return
        return
/*<AWD026> + */
        warn_mess
          comp% = 2%
          hdr$ = "****** WARNING!!    *****"
          msg$(1) = "BOL will print WITHOUT creating invoice data!"
          msg$(2) = "                                             "
          msg$(3) = "Press PF(8) to acknowledge or RETURN to Cancel BOL."
          call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return
/*AWD026> - */
        update_inventory
            if str(scr_load$,1%,1%) <> "S" then goto L61715
               gosub stock_error
               goto L61725

L61715:     gosub clear_pipouts
/* PAR000 */
/* 1 for call by load */
            call "APCPLB44" ("1", scr_load$, " ", #4 )
L61725: return clear all
        goto inputmode

        clear_pipouts                                    /* FOR A LOAD */
            call "SHOSTAT" ("Scan and Clear Pull's From Stock")
            init(" ") sc_key1$                     /* APCPLNSC - Driver*/
            str(sc_key1$,1%,5%) = scr_load$        /* Uodate Inventory */
            read #4,key 1% > sc_key1$, using L35080, sc_rec$,             ~
                                                     eod goto clear_done
            goto L61790
        next_pipout
            read #4, using L35080, sc_rec$, eod goto clear_done

L61790:     if scr_load$ <> str(sc_rec$,7%,5%) then goto clear_done
               dt_so$   = str(sc_rec$,24%,8%)
               dt_item$ = str(sc_rec$,32%,2%)

               gosub delete_pull_stock
               goto next_pipout
        clear_done
        return

        REM *************************************************************~
            *        D R I V E R ' S   R E P O R T   L O G              *~
            *************************************************************

L61885: %!######## ########                       #######################~
        ~#################                                     APCPLA44: !

L61900: %!User Id: ###                            #######################~
        ~#################                                   Page: ##### !
                                                   /* COLUMN 1 HEADER */
L61915: %!Load Number: #####                                             ~
        ~                                                                !
L61925: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+
L61935: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!
        %!                                                               ~
        ~                                                                !

/*SR75119 + */
L61960: %!No.!Dp!<- P.O. Number >!<Cust>!<--- Customer's Name -->!Sls  Ord~
        ~! Qty!<--------  S e q u e n c e  N u m b e r s  ------------->!

L61975: %!###!##!################!######!########################!########~
        ~!####! ##### ##### ##### ##### ##### ##### ##### ##### #####   !

L61990: %!---!--!----------------!------!------------------------!--------~
        ~!----!---------------------------------------------------------!

L62005: %!   !  !                !      !################## , ##          ~
        ~!    ! ##### ##### ##### ##### ##### ##### ##### ##### #####   !
N62005: %!   !  !                !      !                                 ~
        ~!    ! ##### ##### ##### ##### ##### ##### ##### ##### #####   !
N62006: %!                  ##### ##### ##### ##### ##### ##### ##### ####~
        ~# ##### ##### ##### ##### ##### ##### ##### ##### ##### #####  !

        glass_warranty                               /* (EWD005) - Begin*/
            glass_warranty% = 0%
            if len(bck_part$) > 18% then return
            if str(bck_part$,5%,4%) <> "WARR" then return
               glass_warranty% = 1%
        return                                       /* (EWD005) - End  */
                                                     /* (EWD004) - Begin*/
        delivery_sort                                /* Delivery (1)    */
            mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work
            sav_po$ = "xyz"
            init(" ") dt_key3$, sav_cust$            /* (EWD004)        */
            str(dt_key3$,1%,5%) = scr_load$          /* (APCPLNDT)      */
            read #2,key 3% > dt_key3$, using L35040, dt_rec$,             ~
                                              eod goto delivery_sort_done
            goto L62085
        delivery_sort_nxt
            read #2, using L35040, dt_rec$, eod goto delivery_sort_done

L62085:     if scr_load$ <> str(dt_rec$,1%,5%) then goto delivery_sort_done
               dt_dept$ = str(dt_rec$,42%,3%)
               dt_proc$ = str(dt_rec$,45%,2%)       /* (EWD004)        */
               dt_so$   = str(dt_rec$,24%,8%)
               dt_part$ = str(dt_rec$,189,25)
/*AWD023*/  if str(dt_part$,5,4) = "WARR" then goto delivery_sort_nxt
               if dt_dept$ = "095" or dt_dept$ = "102" or                ~
                                      dt_dept$ = "104" then goto L62120
                                                    /* (EWD013)        */
               gosub check_support
               if supp% = 1% then goto delivery_sort_nxt

L62120:        init(" ") wrk_key$, wrk_rec$
               dt_bar$  = str(dt_rec$,24%,18%)
               dt_cust$ = str(dt_rec$,124%,9%)
               gosub lookup_po
               if scr_drop$ <> " " and scr_drop$ <> or_drop$             ~
                                            then goto delivery_sort_nxt

               if scr_so$ <> " " and scr_so$ <> dt_so$  /*(AWD020)*/     ~
                                            then goto delivery_sort_nxt
/*AWD026*/     if or_status$ >= "18" then goto no_warn_mess
/*AWD026*/     warn_flag = 1%

        no_warn_mess
               str(wrk_key$,1%,5%)   = scr_load$       /* Load Number  */
               str(wrk_key$,6%,2%)   = or_drop$        /* Drop Number  */
               str(wrk_key$,8%,9%)   = dt_cust$        /* Customer Code*/
REM  STR(WRK_KEY$,17%,5%)  = "99999"      /* SORT COUNTER */
REM  STR(WRK_KEY$,22%,16%) = OR_PO$       /* P.O. NUMBER  */
/* TO DO - should 99999 be before or after po?
/* (AWD019) make by po not customer */
               str(wrk_key$,17%,16%) = or_po$          /* P.O. Number  */
               str(wrk_key$,33%,5%)  = "99999"         /* Sort Counter */
               str(wrk_key$,38%,18%) = dt_bar$         /* Barcode      */
               str(wrk_key$,56%,3%)  = dt_dept$        /* Department Cd*/
               str(wrk_key$,59%,2%)  = dt_proc$        /* Process Code */

               gosub lookup_customer
               str(wrk_rec$,1%,30%)  = cust_desc$      /* NAME  61,30  */
               str(wrk_rec$,31%,18%) = city$           /* CITY  91,18  */
               str(wrk_rec$,49%,2%)  = state$          /* STATE 109,2  */
               str(wrk_rec$,51%,9%)  = zip$            /* ZIP   111,9  */
               str(wrk_rec$,60%,16%) = or_po$          /* PO No.120,16 */
               str(wrk_rec$,76%,20%) = cus_contact$    /* Contact136,20*/
               str(wrk_rec$,96%,10%) = cus_phone$      /* Phone  156,10*/
               str(wrk_rec$,106%,8%) = str(dt_bar$,1%,8%) /* S.O.166,8 */
/*SR75119 + */
               str(wrk_rec$,114%,5%) = str(dt_rec$,111%,5%) /*Seq num  */
               str(wrk_rec$,119%,22%)= " "                  /* Filler  */
/*SR75119 - */
                                                       /* (APCTRKWK)   */
               put #14, using L62255, wrk_key$, wrk_rec$
L62255:          FMT CH(60), CH(140)
               write #14, eod goto L62260
               goto delivery_sort_nxt
L62260:             errormsg$ = "Error Sorting Driver's Report"
                    gosub error_prompt
                    goto delivery_sort_nxt

        delivery_sort_done                          /* Reverse Sort (2) */
                                                    /* (APCTRKWK)       */
REM CALL "SHOSTAT" (" DELIVERY SORT DONE " )  STOP
               init(" ") wrk_key$, wrk_rec$, sav_key1$, sav_po$
        delivery_sort_1
               read #14,key > wrk_key$, using L62255, wrk_key$, wrk_rec$,~
                                            eod goto delivery_sort_1_done
               init(" ") wrk_key1$        /* (AWD019) Changes to Delivery */
               if sav_key1$ = str(wrk_key$,1%,7%) then goto L62270
                  seq% = 99999%                     /* Load, Drop, Cust,  */
                  sav_key1$ = str(wrk_key$,1%,7%)  /* (AWD019) */

L62270:        if sav_po$ = str(wrk_key$,17%,16%) then goto L62280
                  sav_po$ = str(wrk_key$,17%,16%)   /* Subtract when    */
                  seq% = seq% - 2%                  /* P.O. Changes     */
                  convert seq% to seq$, pic(00000)

L62280:        str(wrk_key1$,1%,16%) = str(wrk_key$,1%,16%)
               str(wrk_key1$,17%,5%) = seq$
               str(wrk_key1$,22%,39%) = str(wrk_key$,22%,39%)
                                                    /* (EWDDELIV)       */
               put #9, using L62255, wrk_key1$, wrk_rec$
               write #9, eod goto L62290
               goto delivery_sort_1
L62290:           errormsg$ = "(Error) Sorting in Reverse Order"
                  gosub error_prompt
                  goto delivery_sort_1
        delivery_sort_1_done

        return

        report                                       /* DRIVER'S REPORT */
            call "SHOSTAT" ("Building Driver Work File")
            gosub delivery_sort                      /* (EWD004)        */
            count% = 0% : wrk_qty% = 0%
            call "SHOSTAT" ("Creating Driver's Report ")
            print_title$ = "Driver's Delivery Report Log "
            gosub select_printer
            init(" ") wrk_key$, sequences$()
            wrk_key$ = all(hex(00))
            read #14,key > wrk_key$, using L62255, wrk_key$, wrk_rec$,   ~
                                                        eod goto gen_done
            str(sav_rec$,1%,60%)   = wrk_key$
            str(sav_rec$,61%,140%) = wrk_rec$
            goto L62325
        gen_next
            read #14, using L62255, wrk_key$, wrk_rec$,                  ~
                                                        eod goto gen_done
L62325:     if str(sav_rec$,1%,37%) <> str(wrk_key$,1%,37%) then         ~
                                                        goto L62330

               wrk_qty% = wrk_qty% + 1%
/*SR75119 */   sequences$(wrk_qty%) = str(wrk_rec$,114%,5%)/* Seq num  */
               goto gen_next

L62330:     convert wrk_qty% to wrk_qty$, pic(####)
            count% = count% + 1%
            convert count% to count$, pic(###)
            gosub lookup_sold_to_name                     /*  (EWD014)  */
            gosub print_driver_detail

            str(sav_rec$,1%,60%)   = wrk_key$
            str(sav_rec$,61%,140%) = wrk_rec$
            wrk_qty% = 0%
            init(" ") sequences$()
            goto L62325                      /* Record already loaded */

        gen_done

            convert wrk_qty% to wrk_qty$, pic(####)
            count% = count% + 1%
            convert count% to count$, pic(###)
            gosub lookup_sold_to_name                     /*  (EWD014)  */
            gosub print_driver_detail

            gosub delete_work
            print using L61925
            gosub close_printer
        return clear all
        goto inputmode

        lookup_sold_to_name                               /* (EWD014)   */
          init(" ") bck_key$, ship_to$, ship_to_city$
          str(bck_key$,1%,9%)   = str(sav_rec$,8%,6%)
          str(bck_key$,10%,16%) = str(sav_rec$,166%,8%)
          read #7,key = bck_key$, using L62335, ship_to$, ship_to_city$,~
                                                       eod goto no_order
L62335:      FMT POS(42),CH(30), POS(192),CH(30)
          return
          no_order
             ship_to$ = str(sav_rec$,61%,24%)
             str(ship_to_city$,1%,17%) = str(sav_rec$,91%,17%)
             str(ship_to_city$,19%,2%) =  str(sav_rec$,109%,2%)
             str(ship_to_city$,22%,9%) = str(sav_rec$,111%,9%)
          return

        print_head
           if lcnt% <> 99% then print using L61925
           page_no% = page_no% + 1%
           print page
           print using L61925
           print using L61885, date$, rpt_time$, company$
           print using L61900, userid$, print_title$, page_no%
           print using L61915, scr_load$
           print using L61935
           print using L61960
           lcnt% = 6%
        return

        print_driver_detail

            if lcnt% > 56% then gosub print_head
            print using L61990

            seq_j% = 0%

            print using L61975, count$, str(sav_rec$,6%,2%),             ~
                               str(sav_rec$,120%,16%),                   ~
                               str(sav_rec$,8%,6%), ship_to$,            ~
                               str(sav_rec$,166%,8%), wrk_qty$,          ~
/*SR75119                      STR(SAV_REC$,136%,20%)                  */~
/*SR75119                      STR(SAV_REC$,156%,10%)                  */~
                               sequences$(seq_j%+1),                     ~
                               sequences$(seq_j%+2),                     ~
                               sequences$(seq_j%+3),                     ~
                               sequences$(seq_j%+4),                     ~
                               sequences$(seq_j%+5),                     ~
                               sequences$(seq_j%+6),                     ~
                               sequences$(seq_j%+7),                     ~
                               sequences$(seq_j%+8),                     ~
                               sequences$(seq_j%+9)

REM str(sav_rec$,8%,6%) str(sav_rec$,61%,24%)

REM PRINT USING L62005, STR(SAV_REC$,91%,18%),                    ~
REM                     STR(SAV_REC$,109%,2%),                     ~
REM                     STR(SAV_REC$,111%,9%)

            seq_j% = 9%
            print using L62005, str(ship_to_city$,1%,17%),                ~
                               str(ship_to_city$,19%,2%),                ~
/*SR75119                      str(ship_to_city$,22%,9%),              */~
/*SR75119                      str(sav_rec$,156%,10%)                  */~
                               sequences$(seq_j%+1),                     ~
                               sequences$(seq_j%+2),                     ~
                               sequences$(seq_j%+3),                     ~
                               sequences$(seq_j%+4),                     ~
                               sequences$(seq_j%+5),                     ~
                               sequences$(seq_j%+6),                     ~
                               sequences$(seq_j%+7),                     ~
                               sequences$(seq_j%+8),                     ~
                               sequences$(seq_j%+9)

            lcnt% = lcnt% + 3%

            wrk_qty% = wrk_qty% - 18%
            if wrk_qty% <= 0% then goto print_driver_detail_end
            seq_lines% = (wrk_qty%/9%)
            if MOD(wrk_qty%,9%) <> 0% then seq_lines% = seq_lines% + 1%
            seq_j% = 18%
            for seq_i% = 1 to seq_lines% step 1%
                if lcnt% <= 56% then goto no_head
                gosub print_head
                print using L61990
              no_head

                print using N62005, sequences$(seq_j%+1),          ~
                                    sequences$(seq_j%+2),          ~
                                    sequences$(seq_j%+3),          ~
                                    sequences$(seq_j%+4),          ~
                                    sequences$(seq_j%+5),          ~
                                    sequences$(seq_j%+6),          ~
                                    sequences$(seq_j%+7),          ~
                                    sequences$(seq_j%+8),          ~
                                    sequences$(seq_j%+9)
                lcnt% = lcnt% + 1%

                seq_j% = seq_j% + 9%

            next seq_i%

        print_driver_detail_end
        return
                                                   /* (EWD004) - End   */
        screen_message_3
            init(" ") scr1$()
               scr1$(1%)  =  "****************************************"
               scr1$(2%)  =  "* Check List for 'Inventory Update'    *"
               scr1$(3%)  =  "*                                      *"
               scr1$(4%)  =  "* (A) - Has Staging been Updated       *"
               scr1$(5%)  =  "*       Complete? or (Selection '1')   *"
               scr1$(6%)  =  "* (B) - Has Loading been Updated       *"
               scr1$(7%)  =  "*       Complete? or (Selection '2')   *"
               scr1$(8%)  =  "* (C) - Note: If Items are to be       *"
               scr1$(9%)  =  "*         Removed from 'Load'?         *"
               scr1$(10%) =  "*       Selection 3 Must be run before *"
               scr1$(11%) =  "*       Selections 2 or 5 are run and  *"
               scr1$(12%) =  "*       the load Flagged as Complete?  *"
               scr1$(13%) =  "* (D) - Only Run Selection '4' When    *"
               scr1$(14%) =  "*       the Criteria for (A) thru (C)  *"
               scr1$(15%) =  "*       has been met.                  *"
               scr1$(16%) =  "* Special Note: Must be Run before     *"
               scr1$(17%) =  "*       Processing Bill of Ladings???  *"
               scr1$(18%) =  "****************************************"
        return

        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"
            f2% = 0%
            call "WORKOPN2" (#9, mode$, 500%, f2%)
            if f2% <> 0% then goto L62680
            f2% = 0%
            call "WORKOPN2" (#14,mode$, 500%, f2%)
            if f2% <> 0% then goto L62690
        return
L62680:     errormsg$ = "Error - Cannot Open (EWDDELIV)"
            gosub error_prompt
        return

L62690:     errormsg$ = "Error - Cannot Open (APCTRKWK)"
            gosub error_prompt
        return
        delete_work
            call "FILEBGON" (#9)
            call "FILEBGON" (#14)
        return

        error_prompt
           comp% = 2%
           hdr$ = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        check_support
           supp% = 0%
           if dt_dept$ = "102" or dt_dept$ = "104" then                  ~
                                                  goto check_support_done
                                                       /* (EWD013)     */
           init(" ") readkey$
           str(readkey$,1%,9%)   = "PLAN SUPP"
           str(readkey$,10%,15%) = dt_dept$
           read #5,key = readkey$, eod goto check_support_done
           supp% = 1%
        check_support_done
        return
/*SR68443 = */
        check_apcmaster
           apcmaster% = 1%
           init(" ") readkey$
           str(readkey$,1%,9%)   = "APCMASTER"
           str(readkey$,10%,11%) = "APCPLA44" & userid$
           read #5,key = readkey$, eod goto check_apcmaster_done
           apcmaster% = 0%
        check_apcmaster_done
        return
/*SR68443 - */
        disp_quantities

           return
           if counter% > 10% then return
              counter% = counter% + 1%

           convert qty(1%) to qty$(1%), pic(####)  /* order Quantity */
           convert qty(2%) to qty$(2%), pic(####)  /* Ship Quantity  */
           convert qty(3%) to qty$(3%), pic(####)  /* Open Quantity  */
           convert qty(4%) to qty$(4%), pic(####)  /* Schedule Qty   */
           convert qty(5%) to qty$(5%), pic(####)  /* Allocated Qty  */
           convert qty(6%) to qty$(6%), pic(####)  /* Pre-Invoice Qty*/
           tst_qty = 0.00
           convert tst_qty to tst_qty$, pic(####)  /* Delivery Ticket*/
           comp% = 2%
           hdr$ = "SO= " & dt_so$ & " Cus= " & dt_cust$
           hdr$ = hdr$ &" load= " &scr_load$
           msg$(1%) = "Ord = "&qty$(1%)&"  Shp = "&qty$(2%)
           msg$(2%) = "Opn = "&qty$(3%)&"  Sch = "&qty$(4%)
           msg$(3%) = "All = "&qty$(5%)&"  Pre = "&qty$(6%)&" t1= "&tst_qty$
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

/* (EWD008) */
        open_error
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
            err% = 0%
        return
/* (EWD008) */


        lookup_oradesc
            init(" ") oradesc_key$, oradesc_key1$, ora_config$, part_desc$,~
                      window$, part$, scrn$, size_desc$, savorad_key$,     ~
                      savorad_key1$, config_qty$, shp_qty$, prt_wind$,     ~
                      prt_scrn$, prt_part$
            order_qty, sub_qty, tot_order, tot_sub, tot_lines = 0.00
            window%, scrn%, part%, shp_qty% = 0%


            str(oradesc_key$,1%,8%)  = str(bck_ln_key$,1%,8%)
            str(oradesc_key$,9%,3%)  = str(bck_ln_key$,17%,3%)
            str(savorad_key$,1%,11%) = str(oradesc_key$,1%,11%)
        lookup_oradesc_next
            read #16, key > oradesc_key$, using L63000, oradesc_key$,      ~
                          oradesc_key1$, part_desc$, window$, scrn$, part$,~
                          size_desc$, order_qty, sub_qty, ora_config$,     ~
                          eod goto no_oradesc

L63000:               FMT CH(14), CH(14), POS(29), CH(250), CH(01), CH(01),~
                          CH(01), CH(25), PD(14,4), PD(14,4),              ~
                          POS(362), CH(01)

                  if str(oradesc_key$,1%,11%) <> str(savorad_key$,1%,11%)  ~
                       then goto no_oradesc       /* 1 - 11 = Sales Order */
                                                  /* and caelus line      */
/* CR2797 start */                                                  
                  gosub read_sku         /* add SKU to front of description */
                  if lowes_stock% = 0% then goto L63005
                  str(part_desc$,15%,250%) = part_desc$
                  str(part_desc$,1%,14%) = "SKU " & lowes_sku$
                                
L63005:
/* CR2797 end */
                  shp_qty% = int(qty(2%)) + int(qty(6%))

                  if window$ = "1" then window% = shp_qty%

                  if scrn$   = "1" then scrn% = shp_qty%

                  if part$   = "1" then part% = shp_qty%

                  if size_desc$ < " " then str(size_desc$,1%,25%) = " "


                  if ora_config$ = "0" then                       ~
                               convert shp_qty% to shp_qty$,pic(000)
                  if ora_config$ = "0" then                      ~
                     config_qty$ = str(oradesc_key$,10%,2%) & "-" & shp_qty$


                  if ora_config$ = "0" then return        /* Not Config   */
REM IF ORA_CONFIG$ = "2" THEN RETURN          /* FACTORY PREP */
                                                        /* WW Line Num    */
REM CALL "SHOSTAT" ( " I AM HERE  "  )     STOP
                     savorad_key1$ = str(oradesc_key1$,1%,14%)
                                                        /* Caelus Lne Num */
                     savorad_key$ = str(oradesc_key$,1%,14%)
                     gosub get_total_config_count

                     bck_ln_key$ = sav_ln_key$      /* Reset BCKLINES key */
                                         /* Go Get Line 1 for the Config  */
                     goto lookup_oradesc_next
        return
        no_oradesc
        return

/* CR2797 new read */
        read_sku
           lowes_stock% = 0%
           init(" ") lowes_key$, tmp_bar$
           str(lowes_key$,1%, 8%) = str(bck_ln_key$,1%,8%)
           str(lowes_key$,9%, 2%) = bck_ln$
           str(lowes_key$,11%,13%) = "0000000000000"

           read #2,key >= lowes_key$, using LSEQ2, tmp_bar$, lowes_sku$,   ~
                                                   eod goto LW0
LSEQ2:     FMT POS(24), CH(18), POS(245), CH(09)
           if str(tmp_bar$,1%,8%) <> str(bck_ln_key$,1%,8%) then goto LW0
           if str(tmp_bar$,9%,2%) <> bck_ln$                then goto LW0
           
           if lowes_sku$ > "00000"  then lowes_stock% = 1%

           if lowes_sku$ = "88910"  then lowes_stock% = 0%
           if lowes_sku$ = "2910"   then lowes_stock% = 0%
           if lowes_sku$ = "231061" then lowes_stock% = 0%
           if lowes_sku$ = "197556" then lowes_stock% = 0%
           if lowes_sku$ = "36277"  then lowes_stock% = 0%

LW0:    return

        
        check_oradesc
            if str(sav_rec$,166%,8%) <> "02107602" then goto no_stop
                  call "SHOSTAT" (" SO NUMBER --> " & str(sav_rec$,166%,8%))
                  stop
no_stop
             read #16, key > oradesc_key$, using L63010, oradesc_key$,      ~
                          eod goto build_desc
L63010:             FMT CH(14)

                    if str(oradesc_key$,1%,8%) <> str(sav_rec$,166%,8%)     ~
                            then goto build_desc


        return
        build_desc
            error% = 0%
REM            gosub connect
            call "ORADESCR" (str(sav_rec$,166%,8%),   /* S.O. No.         */~
                             userid$,         /* User Who Entered Order   */~
                             error%,          /* Error Flag from File Open*/~
                             #16,             /* File ORADESC             */~
                             #8,              /* BCKLINES                 */~
                             #5   )           /* GENCODES                 */
REM            gosub disconnect
        return

        connect

          init(" ") user$, pass$, server$
REM            user$   = "MSSQL"
REM            pass$   = "MSSQL"
          gosub get_user_pswd       /* (AWD028) */

          oci_err% = 0%
          call "CONNECT" (user$, pass$, server$, oci_err%)
          if oci_err% >= 0% then ora_connect% = 1%
          if oci_err% >= 0% then return
             gosub oracle_connect_error
             return clear all
             goto inputmode
       return

/* (AWD028) beg */
        get_user_pswd
            call "READ100" (#18, "ORACLE PASSWORD", f1%(18%))   /* SYSFILE2 */
            if f1%(18%) <> 0% then get #18 using ORCL_PSWD, user$, pass$
ORCL_PSWD:         FMT POS(21), CH(50), POS(50)

        return

/* (AWD028) END */

       disconnect
            call "DISCNNCT" (oci_err%)
            ora_connect% = 0%
       return

        oracle_connect_error
           comp% = 2%
           hdr$ = "***************************************"
           msg$(1%) = " - - - You are NOT connect to Oracle - - - "
           msg$(2%) = "      Contact Systems For Support.         "
           msg$(3%) = "      Press <Return> to Exit??             "
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return


        get_total_config_count

             read #16, using L63020, oradesc_key$, oradesc_key1$,           ~
                                     window$, scrn$, part$, order_qty,      ~
                                     sub_qty, eod goto get_tot_done

L63020:                  FMT CH(14), CH(14), POS(279), CH(01), CH(01),      ~
                             CH(01), POS(307), PD(14,4), PD(14,4)


                  if str(oradesc_key1$,1%,11%) <> str(savorad_key1$,1%,11%)    ~
                       then goto get_tot_done   /* Key 1 - WW Line Number */

                       if window$ <> "1" then goto get_total_config_count
                          gosub lookup_lines_qty

                  tot_order = tot_order + order_qty
                  tot_sub   = tot_sub   + qty(2%) + qty(6%)
                  tot_lines = tot_lines + 1         /* Number of Lines in   */
                                                    /* the configuration    */

                     goto get_total_config_count

        return
        get_tot_done
             config_qty = 0.00

             config_qty = tot_sub / tot_lines
             if ora_config$ <> "2" then                     ~
                          window% = window% + int(config_qty)

                                       /* Reset everything back */
             oradesc_key1$ = str(savorad_key1$,1%,14%)
             oradesc_key$ = str(savorad_key$,1%,14%)
             gosub delivery_detail
        return

        lookup_lines_qty

           init(" ") bck_ln_key$
           str(bck_ln_key$,1%,16%) = str(oradesc_key$,1%,8%)
           str(bck_ln_key$,17%,3%) = str(oradesc_key$,9%,3%)

           read #8, key = bck_ln_key$, using L64020, qty(),                 ~
                                                    eod goto lines_qty_done
L64020:             FMT POS(93), 6*PD(14,4)

        lines_qty_done

        return

        lookup_trailer
          init(" ") awdtrail_key$
          str(awdtrail_key$,1%,1%) = "1"         /* Open Trailer  */
          str(awdtrail_key$,2%,5%) = scr_load$   /* Load Number   */
        trailer_next
          read #17, key 1% > awdtrail_key$, using L64050, awdtrail_key$, ~
                                         eod goto trailer_done
L64050:           FMT POS(21), CH(20)

                  if str(awdtrail_key$,1%,1%) <> "1" then goto trailer_done
                    if str(awdtrail_key$,2%,5%) <> scr_load$      ~
                                                     then goto trailer_next
                  awdtrail$ = str(awdtrail_key$,7%,8%)

        trailer_done
        return
/*(CR905)*/
        lookup_text
           init(" ") nameorderby$, roomLoc$
           txt_key$, txt$, sve_txt$ = all(hex(00))
           p% = 0%
           str(txt_key$,1%,1%) = "M"
           str(txt_key$,2%,3%) = "   "
           str(txt_key$,5%,4%) = txtid$
           str(txt_key$,9%,1%) = "1"
           sve_txt$ = txt_key$
           read #19,key > txt_key$, eod goto noLneTxt
              get #19, using TXT_FMT, txt_key$, txt$
TXT_FMT:         FMT CH(11), POS(64), XX(70), XX(70), CH(70)
           if str(sve_txt$,1%,9%) <> str(txt_key$,1%,9%) then goto noLneTxt

             p% = pos(txt$ = "/")

             if p% = 0% then roomLoc$ = str(txt$,1%,30%)
             if p% = 0% then return

             nameorderby$ = str(txt$,1%,(p%-1%))
             roomLoc$     = str(txt$,(p%+1%),30%)


        noLneTxt
        return
/*(CR905-)*/
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end


