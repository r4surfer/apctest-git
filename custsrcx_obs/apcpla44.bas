        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLA44                             *~
            *  Creation Date     - 10/02/96                             *~
            *  Last Modified Date- 01/24/07                             *~
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
            * 01/24/07 ! (AWD021) mod to who and when closed load ! CMG *~
            *09/15/2016! (CR456) DC Center remove 15 RGA          ! CMG *~
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
            dt_dept$3,                   /* Department                 */~
            dt_proc$2,                   /* process Code (EWD004)      */~
            dt_drop$2,                   /* Drop                       */~
            dt_cust$9, sav_cust$9,       /* Customer Code (EWD004)     */~
            dt_so$8,                     /* Sales Order Number         */~
            dt_item$2                    /* Line Item                  */

        dim                              /* (New) - (APCPLNLD) - File  */~
            ld_load$5, sav_load$5,       /* Load Number                */~
            ld_desc$30, apc_desc$30,     /* Load Description           */~
            ld_dtp1$8, ld_dte1$6,        /* Load Production Date (Plan)*/~
            ld_dtp2$8, ld_dte2$6,        /* Load Completion Date (Plan)*/~
            ld_dtp3$8, ld_dte3$6         /* Load Load Date       (Plan)*/

        dim                              /* (Program Variables)        */~
            cust_desc$30,                /* CUSTOMER NAME (EWD004)     */~
            cust_addr$(3%)30,            /* Ship To Address (EWD004)   */~
            bill_desc$(3%)30,            /* Bill to name and address   */~
            bill_city$18,                /* Bill To City               */~
            bill_state$2,                /* Bill to State              */~
            bill_zip$9,                  /* Bill To Zip Code           */~
            bck_key$25,                  /* BCKMASTR Primary           */~
            bck_terms$20,                /* Customer Terms             */~
            bck_job$20, bck_cl$2,        /* Job Name                   */~
            bck_ship$10,                 /* ship Date                  */~
            bck_phone$14,                /* Customer Phone Number      */~
            bck_ln_key$19,               /* BCKLINES                   */~
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
            bck_sash$1,                  /* Sash code                  */~
            bck_mull$3,                  /* Save Mull Code             */~
            qty(6%), qty$(6%)10,         /* BCKLINES - Quantities      */~
            tt(4%), tt$(4%)4,            /* Detail Totals              */~
            tot(4%), tot$(4%)4,          /* Customer Totals (EWD004)   */~
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
            scr$(10%)30, scr1$(20%)40,   /* Screen Text Messages       */~
            stat_msg$(4%)39,             /* Status Messages            */~
            scan$5,                      /* Scanned Record Count       */~
            i_max$5,                     /* Number of Records Selected */~
            scr_code$1,                  /* Screen Code (0) or (1)     */~
            scr_load$5,                  /* Screen Load Number         */~
            scr_drop$2,                  /* Customer Drop Number       */~
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

        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
            rslt$(15%)20                 /* Text from file opening     */

        dim                                                              ~
            hdr$40,                      /* ASKUSER HEADER             */~
            msg$(3%)79                   /* ASKUSER TEXT               */

        dim                              /* ( APCTRKWK)                */~
            wrk_key$60, mode$5,          /* Load Number                */~
            wrk_key1$60, sav_key1$16,    /* Second Sort (EWD004)       */~
            sav_po$16,                   /* Save P.O. Number (EWD004)  */~
            wrk_rec$140, sav_rec$200,    /* Load Description           */~
            wrk_qty$4, seq$5,            /* Scheduled Production Date  */~
            city$18,                     /* Planned Production Date    */~
            state$2,                     /* Scheduled Completion Date  */~
            zip$9,                       /* Planned Completion Date    */~
            cus_contact$20,              /* Customer Contact           */~
            cus_phone$10,                /* Customer Phone             */~
            count$3                      /* Scheduled Load Date        */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(Staging/Loading) Tracking & Update Util"
            pname$ = "APCPLA44 - Rev: R6.04"

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
            * #12 ! EWDPRDLB ! New Production Labels          (EWD009)  *~
            * #14 ! APCTRKWK ! DRIVER REPORT WORK FILE                  *~
            * #15 ! APCPLNLD ! Planning Load Master - Old APCMAST       *~
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
            select #12, "EWDPRDLB",                                      ~
                        varc,     indexed,  recsize =  640,              ~
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

/* (EWD008)  END */
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

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            gosub screen_message_1

            for fieldnr% = 1% to  3%
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
L11140:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 3% then editpg1
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
L11340:     if scr_code% <> 4% then goto L11450
L11350:        gosub'103(0%, 0%)                  /* Check List Screen */
               if keyhit% = 1% then gosub startover
               if keyhit% = 16% then gosub exit_program
               if keyhit% <> 9% then goto L11350

               gosub update_inventory

L11450:     call "SHOSTAT" ("Loading Data For Load")
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
                          backorder_load, backorder_load
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
         "Enter a Valid Starting Drop Number or Leave Blank.           "

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
                      apc_desc$, sav_load$, scr_drop$, sc_key$, scr_load$
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
                 dt_st$   = str(dt_rec$,64%,2%)     /* Current Status  */
                 dt_drop$ = str(dt_rec$,11%,2%)   /* No. of Drops Last */
                 if scr_drop% = 0% then goto L30170 /* (EWD001) Fix    */
                    if scr_drop$ <> dt_drop$ then goto next_dataload
L30170:                                             /* (EWD001)        */
                 scan%  = scan% + 1%
                                                    /* (EWD018)        */
                 on scr_code% goto L30250, L30270, L30270,L30270, L30280
                                                      /* (EWD003)       */
L30250:           if dt_st$ < "14" then goto L30290   /* Not Staged     */
                     goto next_dataload               /* Yes Staged     */
L30270:           if dt_st$ < "16" then goto L30290   /* Not Loaded     */
                     goto next_dataload               /* Yes Loaded     */
L30280:           if dt_st$ <= "16" then goto L30290  /* RGA   (EWD018) */
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
               st$(i%)= str(dt_rec$,111%,5%)          /* Planning Sort - EWD010 */
               if sel% = 1% then b$(i%) = str(dt_rec$,24%,18%)
               m$(i%) = "Make"
               if dt_dept$ = "102" then m$(i%) = "PStk"
               if dt_dept$ = "104" then m$(i%) = "PShp"   /* (EWD013)  */
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
                                L40095          /* Drop Number       */

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
            pf$(1%)= "                 (4)Previous            " &        ~
                     "                       (14)Update Data "
            pf$(2%)= "(2)First         (5)Next                " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "(3)Last          (9)Update Total Load   " &        ~
                     "                       (16)Exit Load   "
            pfkeys$ = hex(ff02030405ffffff09ffffffff0e0f1000)
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
                              L50550          /* Drop Number           */

            return

L50140: REM Screen Code                           SCR_CODE$
            if scr_code$ <> " " then goto L50170
               scr_code$ = "1"
L50170:     scr_code% = 0%
            convert scr_code$ to scr_code%, data goto L50270

REM  IF SCR_CODE% < 1% OR SCR_CODE% > 5% THEN GOTO L50270  (CR456)
                                                           /* (EWD012) */
                                                           /* (EWD018) */
             if scr_code% < 1% or scr_code% > 4% then goto L50270
               if scr_code% = 1% then scr_msg$ = "Update Staging Data. "
               if scr_code% = 2% then scr_msg$ = "Update Loading Data. "
               if scr_code% = 3% then scr_msg$ = "Remove from Load.    "
               if scr_code% = 4% then scr_msg$ = "Update Inventory.    "

REM IF SCR_CODE% = 5% THEN SCR_MSG$ = "UPDATE RGA.          "  (CR456)
                                                  /* (EWD003)           */
        return
L50270:     errormsg$ = "(Error) - Invalid Update Selection(1 thru 5)?"
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

L55010: % Load No: #####                                   D E L I V E R ~
        ~Y   T I C K E T

L55020: %     Drop No.    Ship Date      Customer No.                    ~
        ~Terms                                             Phone Number

L55030: %      (##)      ##########       #########                      ~
        ~###################                               ##############

L55040: %     Sold To: ##############################                    ~
        ~     Ship To: ##############################

L55050: %              ##############################                    ~
        ~              ##############################

L55060: %              ##############################                    ~
        ~              ##############################

L55070: %              ##################, ## #########                  ~
        ~              ##################, ## #########


L55080: % <- P.O. Number > <WW> <Ln>  <- Description >  < Width >  <Heigh~
        ~t>   Wind   Part     Fact/Mull <---- Job Name ---->  Sales Order

L55090: % ---------------- ---- ----  ----------------  ---------  -------~
        ~-   ----   ----     --------- --------------------  ------------

L55100: % ################ ##   ##    ################  #########  ######~
        ~##   ####   ####        ###    ####################   ########

L55110: %                                                                ~
        ~     ====   ====

L55120: %                                                                ~
        ~     ####   ####




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
REM            scr$( 8%) =  "* (5) - Update RGA           *"  /* (CR456) */
               scr$( 8%) =  "*                            *"  /* (CR456) */
/*(EWD018)*/   scr$( 9%) =  "*                            *"
               scr$(10%) =  "******************************"
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
            if scr_code% = 5% then str(s1$(2%),11%,30%) =                ~
                                   " Update RGA Data for Load    "

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
                                             /* (EWD003) -           */
                                    /* Only Update Parts Selected */
                                    /* Update Based on Status of  */
            for i% = 1% to i_max%   /* (14)-Staged, (16)-Loaded   */
                if o$(i%) = " " then goto L60260
                   dt_bar$ = bb$(i%)
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
                                    #12,       /* EWDPRDLB              */ ~
                                    #13,       /* EWDBOLBK              */ ~
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
                                    err%       /* Error Code            */ )
        return




        delete_pull_stock
          pull% = 0%
          call "APCPULSB" (2%, " ", " ", dt_so$, dt_item$, " ", " ",     ~
                               0%, 0%, #1, pull%)
        return

        update_load_info                 /* Check to see if Staged or  */
            gosub dataload               /* Load   /* CHECK COMPLETE   */
            if i_max% <> 0% then return
            if str(scr_load$,1%,1%) = "S" then return /*SKIP STOCK LOAD*/

            read #15,hold,key = scr_load$, eod goto L60965
               put #15, using L60945, dt_st$, date, userid$, time, "APC"
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
L61275:     if str(sav_rec$,1%,21%) <> str(wrk_key$,1%,21%) then      ~
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
            init(" ") cust_desc$
            read #11,key = dt_cust$, using L61405, cust_desc$,           ~
                                      city$, state$, zip$, cus_contact$, ~
                                      cus_phone$, eod goto L61415
L61405:        FMT POS(10), CH(30), POS(403), CH(18), CH(2), XX(1),      ~
                   CH(9), POS(433), CH(20), CH(10)
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
            read #3,key 4% = or_key4$, using L61450, or_drop$, or_po$,    ~
                                             eod goto L61455
L61450:        FMT POS(25), CH(2), POS(36), CH(16)
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
          if lcnt% > 56% then gosub print_header
          print using L61100
          print using L61110, str(sav_rec$,120%,16%), str(sav_rec$,166%,8%),~
                              str(sav_rec$,6%,2%), wrk_qty$,               ~
                              str(sav_rec$,8%,6%), str(sav_rec$,61%,23%)
          lcnt% = lcnt% + 2%
          count% = count% + 1%
         return

        print_totals
          convert count% to count$, pic(###)

          if lcnt% > 57% then gosub print_header
          print using L61090
          print using L61130, count$
          print using L61065
        return

        delivery_ticket                         /* (EWD004) - Begin */
            init(" ") sav_so$, sc_key1$, sav_cust$
            gosub select_printer

            call "SHOSTAT" ("Sorting Load Data")

            gosub delivery_sort
            call "SHOSTAT" ( "Creating Delivery Tickets for Load ("& ~
                                       scr_load$ &")" )
            mat tot = zer

            init(" ") wrk_key$, wrk_rec$, sav_rec$, sav_cust$
            read #9,key > wrk_key$, using L62255, wrk_key$, wrk_rec$, ~
                                         eod goto delivery_ticket_done
            str(sav_rec$,1%,60%)   = wrk_key$
            str(sav_rec$,61%,140%) = wrk_rec$
            sav_cust$ = str(wrk_key$,8%,9%)
            goto L61290
        delivery_ticket_nxt
            read #9,key > wrk_key$, using L62255, wrk_key$, wrk_rec$, ~
                                         eod goto delivery_ticket_done
L61290:     if str(sav_rec$,1%,21%) <> str(wrk_key$,1%,21%) then      ~
                                               goto L61300
               goto delivery_ticket_nxt

L61300:     gosub lookup_delivery_detail

            if sav_cust$ <> str(wrk_key$,8%,9%) then                  ~
                                                gosub delivery_totals
            str(sav_rec$,1%,60%)    = wrk_key$
            str(sav_rec$,61%,140%)  = wrk_rec$
            sav_cust$ = str(wrk_key$,8%,9%)
            goto L61290
        delivery_ticket_done
            gosub lookup_delivery_detail
            gosub delivery_totals
            gosub close_printer
            gosub delete_work

        return clear all
        goto inputmode

        delivery_header
          gosub lookup_delivery_header

          page_no% = page_no% + 1%
          print page
          print
          print using L55000, date$, rpt_time$, company$, page_no%    /*  (EWD015)  */
          print
          print using L55010, scr_load$          /* Delivery Ticket */
          print
          print using L55020                     /* Customer Info   */
          print using L55030, or_drop$, bck_ship$, dt_cust$, bck_terms$,~
                              bck_phone$
          print
          print using L55040, bill_desc$(1%), cust_addr$(1%)
          print using L55050, bill_desc$(2%), cust_addr$(2%)
          print using L55060, bill_desc$(3%), cust_addr$(3%)
          print using L55070, bill_city$, bill_state$, bill_zip$,      ~
                              city$, state$, zip$
          print
          print using L55080                     /* Column Headings   */
          print using L55090
          lcnt% = 15%
        return

        delivery_detail
          for kk% = 1% to 4%
              convert tt(kk%) to tt$(kk%), pic(####)

          next kk%
                                                 /* (EWD005)    */
          if lcnt% > 31% then gosub delivery_header   /*  (EWD016) Add WW Ln */
          print using L55100, bck_po$, bck_ww_ln$, bck_ln$, bck_d$,        ~
                              bck_wd$, bck_ht$, tt$(1%), tt$(2%),          ~
                              bck_mull$, bck_job$, dt_so$
          lcnt% = lcnt% + 1%
        return

        delivery_totals
          for kk% = 1% to 2%
              convert tot(kk%) to tot$(kk%), pic(####)

          next kk%
                                                /* (EWD005)     */
          if lcnt% > 31% then gosub delivery_header
             k% = 31% - lcnt%
             if k% < 1% then goto L61310
                for kk% = 1% to k%
                    print
                next kk%

L61310:   print
          print using L55110
          print using L55120, tot$(1%), tot$(2%)
          print
        REM  print using L55130, " "             /* (EWD005)    */
        REM  print using L55140, " "
        REM  print
        REM  print using L55150                  /* (EWD005)    */
          lcnt% = 99%
          mat tot = zer
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
                    bck_phone$, cust_addr$()
          str(bck_key$,1%,9%)   = dt_cust$
          str(bck_key$,10%,16%) = dt_so$
          read #7,key = bck_key$, using L61600, cust_addr$(), bck_terms$,~
                                  bck_ship$, eod goto cancel_order
L61600:      FMT POS(42), 3*CH(30), POS(402), CH(20), POS(824), CH(6)
          bck_phone$ = "(" & str(cus_phone$,1%,3%) & ") " &             ~
                     str(cus_phone$,4%,3%) & "-" & str(cus_phone$,7%,4%)
          call "DATFMTC" (bck_ship$)
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
          init(" ") bck_key$, bck_job$
          str(bck_key$,1%,9%)   = dt_cust$
          str(bck_key$,10%,16%) = dt_so$
          read #7,key = bck_key$, using L61605, bck_job$, eod goto L61608
L61605:      FMT POS(619), CH(20)
L61608:   return

        lookup_delivery_detail                        /* (BCKLINES)   */
          init(" ") bck_ln_key$, dt_so$, dt_cust$
          dt_so$   = str(sav_rec$,166%,8%)
          dt_cust$ = str(sav_rec$,8%,9%)
          bck_po$  = str(sav_rec$,120%,16%)
          str(bck_ln_key$,1%,16%) = dt_so$
                                               /* (EWD016) - Get WW Ln */
L61610:   read #8,key > bck_ln_key$, using L61620, bck_ln_key$,        ~
                        bck_part$, bck_desc$, qty(), s_1$, bck_ww_ln$, ~
                                                        eod goto L61700

L61620:     FMT POS(10), CH(19), POS(32), CH(25), CH(32), POS(93),     ~
                 6*PD(14,4), POS(282), CH(2), POS(285), CH(2)
          if dt_so$ <> str(bck_ln_key$,1%,8%) then goto L61700
             gosub glass_warranty                     /* (EWD005)      */
             if glass_warranty% = 1% then goto L61610
                                                      /* (EWD005)      */
             init(" ") bck_tst$, bck_wd$, bck_ht$, bck_d$, bck_mull$
             mat tt = zer
             bck_ln%, bck_ww_ln% = 0%                 /*  (EWD016)     */
             bck_ln$  = str(bck_ln_key$,18%,2%)       /* Line Item No. */
             convert bck_ln$ to bck_ln%, data goto L61625

L61625:      convert bck_ln% to bck_ln$, pic(00)
                                                      /*  (EWD016) - Beg */
             convert bck_ww_ln$ to bck_ww_ln%, data goto L61629

L61629:      if bck_ww_ln% = 0% then bck_ww_ln% = bck_ln%

             convert bck_ww_ln% to bck_ww_ln$, pic(00)

                                                      /*  (EWD016) - End */


             bck_d$   = str(bck_desc$,1%,11%)         /* MFG Descript  */
             gosub build_description                  /* (EWD006)      */
             gosub lookup_color
             str(bck_d$,9%,3%) = " " & bck_cl$

/*(EWD016)*/ bck_tst$ = str(bck_desc$,17%,16%)        /* Width/Height  */
             p% = pos(bck_tst$ = "X")
             bck_wd$ = str(bck_tst$,1%,p% - 1%)       /* Save Width    */
             bck_ht$ = str(bck_tst$,p% + 1%,7%)       /* Save Height   */
             tst_qty = qty(2%) + qty(6%)              /* Tot Shipped   */

             gosub disp_quantities                    /* Debug         */

             bck_sash$ = str(bck_part$,11%,1%)        /* Save Screen   */
             bck_mull$ = "999" : x% = 0%              /* Test for Mull */
             if len(bck_Part$) > 19 then bck_mull$ = str(bck_part$,20%,3%)
             if len(bck_part$) > 22 then bck_mull$ = str(bck_part$,23%,3%)
             convert bck_mull$ to x%, data goto L61630
L61630:
             if x% = 0% then bck_mull$ = "YES" else bck_mull$ = "   "

             gosub lookup_job_name                    /* (BCKMASTR)    */
             if len(bck_part$) > 18 then goto L61660
                init(" ") bck_wd$, bck_ht$
                tt(2%)  = tt(2%)  + tst_qty           /* Parts         */
                tot(2%) = tot(2%) + tst_qty
                goto L61690

L61660:      tt(1%)  = tt(1%)  + tst_qty              /* Std Window    */
             tot(1%) = tot(1%) + tst_qty

L61690:      gosub delivery_detail                 /* Line Item Detail */
             goto L61610

L61700: return

        build_description                             /* (EWD006) Mods */
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

        lookup_color
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

        update_inventory
            if str(scr_load$,1%,1%) <> "S" then goto L61715
               gosub stock_error
               goto L61725

L61715:     gosub clear_pipouts
            call "APCPLB44" ( scr_load$, #4 )
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

L61960: %!No.!Dp!<- P.O. Number >!<Cust>!<--- Customer's Name -->!Sls  Ord~
        ~! Qty!<----- Contact ------>!Phone Number!Delv Dte! Remarks    !

L61975: %!###!##!################!######!########################!########~
        ~!####! #################### ! ########## !__/__/__!            !

L61990: %!---!--!----------------!------!------------------------!--------~
        ~!----!----------------------!------------!--------!------------!

L62005: %!   !  !                !      !################## , ## #########~
        ~!    !                      !            !        !            !

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
               if dt_dept$ = "095" or dt_dept$ = "102" or                ~
                                      dt_dept$ = "104" then goto L62120
                                                    /* (EWD013)        */
               gosub check_support
               if supp% = 1% then goto delivery_sort_nxt

L62120:        init(" ") wrk_key$, wrk_rec$
               dt_bar$  = str(dt_rec$,24%,18%)
               dt_cust$ = str(dt_rec$,124%,9%)
               gosub lookup_po
               if scr_drop$ <> " " and scr_drop$ <> or_drop$            ~
                                     then goto delivery_sort_nxt

               str(wrk_key$,1%,5%)   = scr_load$       /* Load Number  */
               str(wrk_key$,6%,2%)   = or_drop$        /* Drop Number  */
               str(wrk_key$,8%,9%)   = dt_cust$        /* Customer Code*/
               str(wrK_key$,17%,5%)  = "99999"         /* Sort Counter */
               str(wrk_key$,22%,16%) = or_po$          /* P.O. Number  */
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
               str(wrk_rec$,106%,8%) = str(dt_bar$,1%,8%) /* S.O.   166,8 */
               str(wrk_rec$,114%,27%)= " "                  /* Filler  */
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
               init(" ") wrk_key$, wrk_rec$, sav_key1$, sav_po$
        delivery_sort_1
               read #14,key > wrk_key$, using L62255, wrk_key$, wrk_rec$,~
                                            eod goto delivery_sort_1_done
               init(" ") wrk_key1$
               if sav_key1$ = str(wrk_key$,1%,16%) then goto L62270
                  seq% = 99999%                     /* Load, Drop, Cust */
                  sav_key1$ = str(wrk_key$,1%,16%)

L62270:        if sav_po$ = str(wrk_key$,22%,16%) then goto L62280
                  sav_po$ = str(wrk_key$,22%,16%)   /* Subtract when    */
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
            init(" ") wrk_key$
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
               goto gen_next

L62330:     convert wrk_qty% to wrk_qty$, pic(####)
            count% = count% + 1%
            convert count% to count$, pic(###)
            gosub lookup_sold_to_name                     /*  (EWD014)  */
            gosub print_driver_detail

            str(sav_rec$,1%,60%)   = wrk_key$
            str(sav_rec$,61%,140%) = wrk_rec$
            wrk_qty% = 0%
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
          read #7,key = bck_key$, using L62335, ship_to$, ship_to_city$, eod goto no_order
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
            print using L61975, count$, str(sav_rec$,6%,2%),             ~
                               str(sav_rec$,120%,16%),                   ~
                               str(sav_rec$,8%,6%), ship_to$,            ~
                               str(sav_rec$,166%,8%), wrk_qty$,          ~
                               str(sav_rec$,136%,20%),                   ~
                               str(sav_rec$,156%,10%)

REM                               str(sav_rec$,8%,6%) str(sav_rec$,61%,24%)

REM            print using L62005, str(sav_rec$,91%,18%),                    ~
REM                               str(sav_rec$,109%,2%),                     ~
REM                               str(sav_rec$,111%,9%)
            print using L62005, str(ship_to_city$,1%,17%),                ~
                               str(ship_to_city$,19%,2%),                ~
                               str(ship_to_city$,22%,9%)
            lcnt% = lcnt% + 3%
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


        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end


