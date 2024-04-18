        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLN5B                             *~
            *  Creation Date     - 05/01/96                             *~
            *  Last Modified Date- 03/17/2010                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Create and Update S.O. Header Record *~
            *                      for Scheduling and Planning. Use this*~
            *                      routine also for Updating the Status *~
            *                      and the date of status change.       *~
            *                      Default - OR_REGION$ = "00"          *~
            *                                OR_ROUTE$  = "00000"       *~
            *                                                           *~
            *  Programs Calling Sub - BCKUPDTE - S.O. Background Task   *~
            *                         APCPLN04 - Load Build & Stock Bld *~
            *                         APCEDIPO - EDI S.O. Create Sub.   *~
            *                         APCEDIQT - Called by JBPOST2 Sub. *~
            *                         JBPOST2  - VIQS System            *~
            *                                                           *~
            *  Subroutines Called - APCPLN6B - Master Scheduling Routine*~
            *                                                           *~
            *  Option Code Definitions - ( OPT% 1% thru 10% )           *~
            *  @(1) = S.O. Added   - STAT  = "00"           - BCKUPDTE  *~
            *         S.O. Assigned- STAT  = "02"           - APCPLN03  *~
            *         Load/Drop Assigned                                *~
            *  @(2) = S.O. Planned - STAT  = "03"           - APCPLN06  *~
            *  @(3) = Status Change- STAT  = "04" thru "16" - ? ? ? ?   *~
            *  @(4) = B.O.L Created- STAT  = "18"           - ? ? ? ?   *~
            *  @(5) = S.O. Invoiced- STAT  = "20"           - ARIUPDTE  *~
            *  @(6) = Invoice Sent - STAT  = "22"           - ? ? ? ?   *~
            *  @(7) = S.O. Paid    - STAT  = "24"           - CRCUPDTE  *~
            *  @(8) = S.O. Changed - STAT  = "01"           - BCKUPDTE  *~
            *  @(9) = S.O. Deleted - STAT  = No Change      - BCKUPDTE  *~
            *  @(10)= Update Delivery Days                  - ? ? ? ?   *~
            *                                                           *~
            *  Special Comments-(1)Schedule Data will only be Updated   *~
            *                      for OPT% = 1%,8%,9% No Others.       *~
            *                      Schedule Data will only change for   *~
            *                      OPT% = 8%, as long as the Status     *~
            *                      Code is not Greater than '01'.       *~
            *                                                           *~
            *                   (2)Once Status Code Greater than '01'   *~
            *                      Which means the S.O. has been        *~
            *                      assigned to a specific load. In Order*~
            *                      to make any changes the S.O. Must 1st*~
            *                      be removed from the load via planning*~
            *                      and then Deleted using S.O. entry.   *~
            *                                                           *~
            *                   (3)Special Status Codes                 *~
            *                      99% = Credit Hold - Can change and   *~
            *                                Delete S.O.                *~
            *                      90% = Tempered Freeze - Can Change,  *~
            *                                but cannot Delete S.O.     *~
            *                      91% = Special Liting Freeze- Can     *~
            *                                change S.O., but cannot    *~
            *                                Sales order.               *~
            *                      92%-98% = Specials                   *~
            *                                                           *~
            *       OR_SPECIAL$  - (1%,1%) = Tempered Glass             *~
            *       (Y/N)          (2%,1%) = Diamond Grid               *~
            *                      (3%,1%) = Special Liting             *~
            *                      (4%,1%) = Wood Surround              *~
            *                      (5%,1%) = Sample Product             *~
            *                      (6%,1%) = Display Product            *~
            *                      (7%,1%) = UPS Sales Order            *~
            *                      (8%,1%) = Parts                      *~
            *                      (9%,1%) = Cottage/Oriel              *~
            *                      (10,1%) = Sash Top/Bot/Fixed         *~
            *                                                           *~
            *       STK$(99%)    - MFG Part No. (25)                    *~
            *       QTY$(99%)    - Quantity for MFG Part No.            *~
            *                                                           *~
            *       Error Codes  - 2% Unable to Add Sales Order         *~
            *                    - 4% Unable to Update Planned S.O.     *~
            *                    - 6% Unable to Update Invoiced S.O.    *~
            *                    - 8% Unable to Update Paid S.O.        *~
            *                    -10% Unable to Update Changed S.O.     *~
            *                    -12% Unable to Delete S.O.             *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/01/96 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 01/24/97 ! Mod to Process Scheduled Stock for New   ! RHH *~
            *          !   Planning System. STK$() MFG Part No.   !     *~
            *          !   QTY$() MFG Quantity                    !     *~
            * 03/20/97 ! Mod to Add New Subroutine (PO_DELETE)    ! RHH *~
            *          !   to resolve Delete Problem with S.Orders!     *~
            * 04/25/97 ! Check Error Codes                        ! RHH *~
            * 10/13/97 ! Mod to Change and Delete to Process      ! RHH *~
            *          !   Regardless of Status Codes.            !     *~
            * 11/13/97 ! Mod for release Upgrade to R6.04.03      ! RHH *~
            * 01/20/98 ! Mod Update for New Wood Surround Codes   ! RHH *~
            * 09/22/98 ! (EWD001) Mod for special status codes    ! RHH *~
            *          !   90% - 98% Reseved for Special Orders.  !     *~
            *          !   90% = Tempered Glass Freeze            !     *~
            *          !   91% = Special Liting Freeze            !     *~
            * 10/15/98 ! (EWD002) - Mod to prevent changing       ! RHH *~
            *          !   planned Orders and Froze Orders        !     *~
            * 08/21/00 ! (EWD003) - Mods for new howship code for ! RHH *~
            *          !   Renovator Product tracking Code=35/UPS !     *~
            * 01/10/02 ! (EWD004) - Mods for New "UPS" howship    ! RHH *~
            *          !   codes.                                 !     *~  
            * 10/24/03 ! (EWD005) - Mod to fix credit hold bug.   ! CMG *~
            *          !  if order is resubmitted assumes must be !     *~
            *          !  status '01' when could be '99'          !     *~
            * 04/26/06 ! (AWD006) - Mod not to look up by PO when ! CMG *~
            *          !    alpha Orders                          !     *~
            *03/08/2010! (AWD007) - add file kanbanpt             ! CMG *~
            *03/17/2010! (AWD008) - add file hldsched             ! CMG *~
			*10/27/2022! (CR3186) Add prefix field to apcplnor    ! RDB *~			
            *************************************************************

        sub "APCPLN5B"   (opt%,          /* Option Code                */~
                          or_due$,       /* S.O. Due Date/Delivery Date*/~
                          or_no$(),      /* Customer Number            */~
                                         /* Customer S.O. Number       */~
                                         /* APC Invoice Assoc. with SO */~
                                         /* APC Check Assoc. with Invoi*/~
                          or_po$,        /* Customer P.O. Number       */~
                          or_status$,    /* Current S.O. Stat PLAN STAT*/~
                          or_dte$,       /* Date Assoc. with Stat Chg  */~
                          or_sls$,       /* APC Salesman Code          */~
                          or_fob$,       /* Cust Delivery Cde PLAN DEL?*/~
                          or_hows$,      /* Spec. Instr (PLAN HOWS)    */~
                          or_load$,      /* Load N. Sched./Assigned    */~
                          or_date$,      /* Date S.O. Created          */~
                          or_chg$,       /* Date S.O. Last Modified    */~
                          or_userid$,    /* S.O. Last Modified By User */~
                          or_bol$,       /* Date B.O.L. Created        */~
                          or_text$,      /* S.O. Header Text Id        */~
                          stk$(),        /* MFG Part No.               */~
/*PAR000*/                stk_sub$(),    /* MFG Sub Part No            */~
/*PAR000*/                stk_desc$(),   /* Stock Description          */~
                          qty$(),        /* MFG Quantity               */~
/*CR3186*/                prefix_so$,    /* New prefix of sales order  */~								  
                          schema%,       /* User Schema                */~
                          #1,            /* CUSTOMER - Customer Master */~
                          #2,            /* APCPLNOR - APC Header Hist */~
                          #3,            /* APCPLNSC -                 */~
                          #4,            /* BCKMASTR                   */~
                          #5,            /* BCKLINES                   */~
                          #6,            /* GENCODES                   */~
                          #7,            /* APCPULLS                   */~
                          #8,            /* HNYQUAN                    */~
                          #9,            /* APCPLNDP                   */~
                          #10,           /* APCPLNSD                   */~
                          #11,           /* HNYMASTR -                 */~
                          #12,           /* APCPLNSA Daily Sales-EWD002*/~
                          #13,           /* EWDSCHED Spec Sched(EWD002)*/~
                          #63,           /* BCKSUBPT    PAR000         */~
                          #14,           /* KANBANPT  (AWD007)         */~
                          #15,           /* HLDSCHED  (AWD008)         */~
                          err% )         /* 0 = OK, NOT 0 = ERROR      */

        dim                              /* APCPLNOR - File            */~
            or_no$(5%)9,                 /* CUST,S.O.,INV,CHK          */~
            or_due$8,                    /* S.O. Due Date/Delivery Date*/~
            or_region$2,                 /* Customer Region Code       */~
            or_route$5,                  /* Customer Route Code        */~
            or_zip$9,                    /* Customer Zip Code          */~
            or_drop$2,                   /* Customer Drop Number       */~
            or_cuscode$9,                /* Customer Number            */~
            or_po$16, sav_po$16,         /* Customer P.O. Number       */~
            or_so$8,                     /* Customer S.O. Number       */~
            or_status$2, sav_status$2,   /* Current S.O. Stat PLAN STAT*/~
            or_dte$8,                    /* Date Assoc. with Stat Chg  */~
            or_inv$8,                    /* APC Invoice Assoc. with SO */~
            or_chk$8,                    /* APC Check Assoc. with Invoi*/~
            or_sls$4,                    /* APC Salesman Code          */~
            or_fob$2,                    /* Cust Delivery Cde PLAN DEL?*/~
            or_hows$2,                   /* Spec. Instr (PLAN HOWS)    */~
            or_load$5,                   /* Load N. Sched./Assigned    */~
                                         /* Total Loading Units S.O.   */~
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
            or_special$10,               /* Special Product Flags      */~
            or_fil$1,                    /* Filler Area                */~
            or_key4$8, or_key1$25,       /* Alt Key 4 = Sales Order No.*/~
            or_rec$170,                  /* Header Record              */~
            stk_flag$1,                  /* Process Stock (Y)es or (N)o*/~
            stk$(99%)25, qty$(99%)4,     /* Part (25), Qty (4)         */~
/*PAR000*/  stk_sub$(99%)20,             /* Part (20)                  */~
/*PAR000*/  stk_desc$(99%)30             /* Stock Desck                */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Planing Sales Order Header Info   "
            pname$ = "APCPLN5B - Rev: R6.04"

        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! CUSTOMER ! Customer Master File                     *~
            * #2  ! APCPLNOR ! Planning S.O. Header History             *~
            * #3  ! APCPLNSC ! Planning Master Schedule File S.O by Line*~
            * #4  ! BCKMASTR ! S.O. Header File                         *~
            * #5  ! BCKLINES ! S.O. Detail File                         *~
            * #6  ! GENCODES ! Master Table Database                    *~
            * #7  ! APCPULLS ! APC Pull from Inventory (INTERIUM)       *~
            * #8  ! INVQUAN  ! Inventory Quantities Master Detail       *~
            * #9  ! APCPLNDP ! Planning Master Department File          *~
            * #10 ! APCPLNSD ! Planning S.O. Scheduling Dept. Detail    *~
            * #11 ! INVMASTR ! MASTER INVENTORY FILE                    *~
            * #63 ! BCKSUBPT ! Sub Part File                 (PAR000)   *~
            *************************************************************~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            init(" ") or_cuscode$, or_so$, or_inv$, or_chk$, sav_po$
REM STR(OR_CUSCODE$,1%,9%) = STR(OR_NO$(1%),1%,6%)
            or_cuscode$ = or_no$(1%)
            or_so$      = or_no$(2%)
            or_inv$     = or_no$(3%)
            or_chk$     = or_no$(4%)

            init(" ") or_key4$, or_rec$, or_fil$, sav_status$, stk_flag$
            or_units = 0.0 : or_value = 0.0 : or_cost  = 0.0
            or_mak%  = 0%  : or_pul%  = 0%  : or_days% = 0%
                                    /* (EWD001) - Cannot Delete Codes */
                                    /*            90% thru 98%        */
                                    /*            99% = Can Delete    */   
            sav_status$ = or_status$
            stk_flag$ = "N"
            if str(or_so$,1%,1%) = "S" then stk_flag$ = "Y"
                                            /* EWD002                 */
            if or_status$ < "03" then goto L02000
            if or_status$ > "89" then goto L02000
               err% = 4%
               goto L02150 
                                            /* EWD002 - Prevent Mod's */
                                            /*   of Planned Orders    */

*       RHH


        REM *************************************************************~
            *     G E T   C U S T O M E R   A N D   S A L E S           *~
            *                 O R D E R   D A T A                       *~
            *************************************************************

L02000:     err% = 1%                    /* Begin By Setting Error Code*/
            if opt% < 1% or opt% > 10% then goto L02150
            gosub or_load               /* Load all Customer Info      */

            if or% = 0% and opt% = 9% then end

            if or% = 0% then opt% = 1%  /* Must Be a New Sales Order   */
            on opt% gosub so_add, so_planned, so_change, bol_created,    ~
                          so_inv, inv_sent, so_paid, so_saved,           ~
                          so_deleted, so_delivery


L02150: end                             /* Terminate Subroutine        */

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* APCPLNOR - New File Layout */
L02210:     FMT CH(08),                  /* S.O. Due Date/Delivery Date*/~
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
                CH(08),                  /* APC Check Assoc. with Inv. */~
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

L02510:     FMT CH(170)                  /* APCPLNOR - Record          */

        REM *************************************************************~
            *       U P D A T E   R O U T I N E S   F O R   G R O U P   *~
            *************************************************************

        so_add                           /* OPT% = (1) Stat = '01'     */
            or_special$ = "NNNNNNNNNN"
            or_dte$  = date    & "  "
            or_date$ = or_dte$ & "  "
            or_chg$  = or_dte$ & "  "
            or_drop$ = "00"
            gosub load_customer
            gosub or_load                /* Lock Sales Order if Exists */
            if or% = 0% then goto L02690   /* SAV_STATUS$ = 99 CREDIT HLD*/
               delete #2
               gosub po_delete

L02690: put #2, using L02210   ,                                           ~
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
            prefix_so$                   /* Filler Area                */

            write #2, eod goto L03040
            err% = 0%
            gosub update_schedule
        return
L03040:     err% = 2%                            /* Unabel to Add S.O. */
        return

        so_planned                       /* OPT% = (3) Stat = '03'     */
            str(or_rec$,25%,2%) = or_drop$           /* Cust Drop No   */
            str(or_rec$,60%,2%) = or_status$         /* Status         */
            str(or_rec$,62%,8%) = or_dte$            /* Date of Status */
            str(or_rec$,94%,5%) = or_load$           /* Load Assigned  */
               delete #2
               gosub po_delete

            put #2, using L02510  , or_rec$
            write #2, eod goto L03190
            err% = 0%
        return
L03190:     err% = 4%                      /* Unable to Update Planned */
        return

        so_change                        /* OPT% = (3) Stat = "04'-'16'*/
            put #2, using L02510  , or_status$, or_dte$
               FMT POS(60), CH(2), CH(8)
            rewrite #2
            err% = 0%
        return

        bol_created                      /* OPT% = (4) Stat = '18'     */
            put #2, using L03310  , or_status$, or_dte$, or_bol$
L03310:        FMT POS(60), CH(2), CH(8), POS(146), CH(8)
            rewrite #2
            err% = 0%
        return

        so_inv                           /* OPT% = (5) Stat = '20'     */
            str(or_rec$,60%,2%) = or_status$
            str(or_rec$,62%,8%) = or_dte$               /* Post Date   */
            str(or_rec$,70%,8%) = or_inv$               /* Invoice No  */
               delete #2
               gosub po_delete
            put #2, using L02510  , or_rec$
            write #2, eod goto L03460
            err% = 0%
        return
L03460:     err% = 6%                      /* Unable to Update Invoiced*/
        return

        inv_sent                         /* OPT% = (6) Stat = '22'     */
            put #2, using L03510  , or_status$, or_dte$
L03510:        FMT POS(60), CH(2), CH(8)
            rewrite #2
            err% = 0%
        return

        so_paid                          /* OPT% = (7) Stat = '24'     */
            str(or_rec$,60%,2%) = or_status$
            str(or_rec$,62%,8%) = or_dte$               /* Post Date   */
            str(or_rec$,78%,8%) = or_chk$               /* Check No.   */
               delete #2
               gosub po_delete

            put #2, using L02510  , or_rec$
            write #2, eod goto L03670
            err% = 0%
        return
L03670:     err% = 8%                      /* Unable to Update Paid    */
        return

        so_saved                         /* OPT% = (8) Stat = 'No Chg' */
            gosub load_customer
            gosub or_load                /* Lock Sales Order if Exists */
            or_status$ = str(or_rec$,60%,2%)
            or_drop$   = str(or_rec$,25%,2%)
            or_load$   = str(or_rec$,94%,5%)
            or_dte$    = or_chg$
            chg% = 100%
            convert or_status$ to chg%, data goto L04050 /* Error No Chg */
                                                       /* Alpha Code   */
            or_status$ = sav_status$            /* (EWD001) Change     */
REM         if chg% = 0% then or_status$ = "01" /* (EWD001) Must be Chg*/
                                                /* (EWD005)  */
            if chg% = 0% and sav_status$ <> "99"                        ~
                         then or_status$ = "01" /* (EWD005)            */


               delete #2
               gosub po_delete

               str(or_rec$,1%,8%)   = or_due$
               str(or_rec$,9%,2%)   = or_region$
               str(or_rec$,11%,5%)  = or_route$
               str(or_rec$,16%,9%)  = or_zip$
               str(or_rec$,36%,16%) = or_po$
               str(or_rec$,60%,2%)  = or_status$
               str(or_rec$,62%,8%)  = or_dte$
               str(or_rec$,86%,4%)  = or_sls$
               str(or_rec$,90%,2%)  = or_fob$
               str(or_rec$,92%,2%)  = or_hows$
               str(or_rec$,94%,5%)  = or_load$
               str(or_rec$,135%,8%) = or_chg$
               str(or_rec$,143%,3%) = or_userid$
               str(or_rec$,154%,4%) = or_text$
			   str(or_rec$,170%,1%) = prefix_so$
            put #2, using L02510, or_rec$
            write #2, eod goto L04050
            err% = 0%
            gosub update_schedule
        return
L04050:     err% = 10%                     /* Unable to Update Changed */
        return

        so_deleted                       /* OPT% = (9) Stat = 'N/A'    */
            or_status$ = str(or_rec$,60%,2%)
            chg% = 100%
            convert or_status$ to chg%, data goto L04210
                                       /* (EWD001) Can Delete Credit   */
                                       /*  but not Special Sales orders*/
            if chg% > 89% and chg% < 99% then goto L04210 /* Error     */
               delete #2
               gosub po_delete

            err% = 0%
            gosub update_schedule
        return
L04210:     err% = 12%                 /* Cannot Delete Special orders */
        return

        so_delivery                      /* OPT% = (10) Stat = 'N/A'   */
            put #2, using L04260 , or_days%             /* Delivery Days */
L04260:        FMT POS(158), BI(2)
            rewrite #2
            err% = 0%
        return

        or_load                                       /* Based on S.O. */
            init(" ") or_key4$, or_rec$, sav_po$
            or% = 0%
            if stk_flag$ = "Y" then return
            or_key4$ = or_so$
            read #2,hold,key 4% = or_key4$, using L02510  , or_rec$,       ~
                                                         eod goto po_load
            sav_po$ = str(or_rec$,36%,16%)
            or% = 1%
        return

        po_load
/*(AWD006) - do not look up by po if Alpha Sales order*/
            or_so% = 0%
            convert or_so$ to or_so%, data goto L04500
/*(AWD006) - END */
            init(" ") or_key1$, sav_po$
            str(or_key1$,1%,9%)   = or_cuscode$
            str(or_key1$,10%,16%) = or_po$
            read #2,hold,key 1% = or_key1$, using L02510, or_rec$,         ~
                                                        eod goto L04500
            sav_po$ = str(or_rec$,36%,16%)
            or% = 1%
L04500: return

        po_delete                                     /* INSURE PO GONE*/
/*(AWD006) - do not look up by po if Alpha Sales order*/
            or_so% = 0%
            convert or_so$ to or_so%, data goto L04500
/*(AWD006) - END */

            init(" ") or_key1$
            str(or_key1$,1%,9%)   = or_cuscode$
            str(or_key1$,10%,16%) = sav_po$
            read #2,hold,key 1% = or_key1$, using L02510 , or_rec$,        ~
                                                            eod goto L04590
               delete #2
L04590: return

        load_customer
            or_region% = 0%   : or_route% = 0%
            or_region$ = "00" : or_route$ = "00000"
            or_zip$    = "000000000"
            if stk_flag$ = "Y" then return
            read #1,key = or_cuscode$, using L04680 , or_zip$, or_region$, ~
                                               or_route$, eod goto L04760
L04680:        FMT POS(424), CH(9), POS(940), CH(2), POS(980), CH(5)
REM            convert or_region$ to or_region%, data goto L04700
L04700:
            convert or_route$ to or_route%, data goto L04720
L04720:
REM            convert or_region% to or_region$, pic(00)

            convert or_route% to or_route$, pic(00000)
L04760: return

        update_schedule

         call "APCPLN6B" (opt%,          /* 1%, 8%, 9% Are Valid       */~
                          or_due$,       /* S.O. Due Date/Delivery Date*/~
                          or_cuscode$,   /* Customer Code              */~
                          or_so$,        /* Customer Sales Order       */~
                          or_drop$,      /* Customer Drop Number       */~
                          or_load$,      /* Load No. Scheduled/Assigned*/~
                          or_status$,    /* Current S.O. Stat-PLAN STAT*/~
                          or_dte$,       /* Date of Stat Change        */~
                          or_hows$,      /* How Ship Codes             */~
                          stk$(),        /* MFG Part No. (25)          */~
/*PAR000*/                stk_sub$(),    /* MFG Subpart num            */~
/*PAR000*/                stk_desc$(),   /* Stock Description          */~
                          qty$(),        /* MFG Quantity (04)          */~
/*CR3186*/                prefix_so$,    /* Prefix Sales Order         */~						  
                          schema%,       /* schema                     */~
                          #1,            /* CUSTOMER - Customer Master */~
                          #2,            /* APCPLNOR - APC Header Hist */~
                          #3,            /* APCPLNSC -                 */~
                          #4,            /* BCKMASTR                   */~
                          #5,            /* BCKLINES                   */~
                          #6,            /* GENCODES                   */~
                          #7,            /* APCPULLS                   */~
                          #8,            /* HNYQUAN                    */~
                          #9,            /* APCPLNDP                   */~
                          #10,           /* APCPLNSD                   */~
                          #11,           /* HNYMASTR -                 */~
                          #12,           /* APCPLNSA Daily Sales-EWD002*/~
                          #13,           /* EWDSCHED Spec Sched(EWD002)*/~
                          #63,           /* BCKSUBPT                   */~
                          #14,           /* KANBANPT (AWD007)          */~
                          #15,           /* HLDSCHED (AWD008)          */~
                          err% )         /* 0 = OK, NOT 0 = ERROR       */
        return
