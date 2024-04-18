        REM *************************************************************~
            *                                                           *~
            *  Special Note - Used by Programs (BCKUPDTE) and (JBPOST2) *~
            *                                                           *~
            *  Program Name      - APCPLN6B                             *~
            *  Creation Date     - 05/01/96                             *~
            *  Last Modified Date- 05/17/2021                           *~
            *  Last Modified By  - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Update S.O. Header Record            *~
            *                      for Scheduling and Planning. Also    *~
            *                      Create Planning Master Scheduling    *~
            *                      Line Item Record (APCPLNSC) and      *~
            *                      Detail Record (APCPLNSD).            *~
            *                                                           *~
            *  Code Tables Used  - (SYS CODES)- Currently Used for the  *~
            *                      Customer Sort Code and Unit Factor.  *~
            *                                                           *~
            *       (EWD011)     - (PLANSSHAP)- Models for Stock Shapes *~
            *                                                           *~
            *  Special Comments  - File Not changed when status Greater *~
            *                      than '01' EXCEPT FOR DELETE.         *~
            *                                                           *~
            *                    - Units Per Manhour In (APCPLNDP) File *~
            *                      are Converted to ManHours Per Unit in*~
            *                      (APCPLNSD) File. Note that it is not *~
            *                      Weighted in Accordance with Table    *~
            *                      (PLAN UPMH)                          *~
            *                                                           *~
            *                    - Note - By Definition 'DIRECT' DEPART.*~
            *                             Begin with a '0' AND INDIRECT *~
            *                             Begin with a 'NON-ZERO' VALUE *~
            *                                                           *~
            * --> ( Only 'DIRECT DEPARTMENTS' ARE SCHEDULED PRODUCT )   *~
            *                                                           *~
            *       OR_SPECIAL$  - (1%,1%) = Tempered Glass             *~
            *       (Y/N)          (2%,1%) = Diamond Grid               *~
            *                      (3%,1%) = Special Liting and Glass   *~
            *                      (4%,1%) = Wood Surround              *~
            *                      (5%,1%) = Sample Product             *~
            *                      (6%,1%) = Display Product            *~
            *                      (7%,1%) = UPS Sales Order            *~
            *                      (8%,1%) = Parts                      *~
            *                      (9%,1%) = Cottage/Oriel              *~
            *                      (10,1%) = Sash Top/Bot/Fixed         *~
            *                                                           *~
            *                                                           *~
            * --> (Special)      - (update_daily - Trap Data for Daily  *~
            *                                      Sales Report         *~
            *                                                           *~
            *                    - (analyize_Glass) - Trap Data in      *~
            *                                (EWDSCHED) File. For       *~
            *                                Tempered Glass & Liting    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/01/96 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 07/22/96 ! Mod to only Schedule 'DIRECT' DEPT'S,    ! RHH *~
            *          !  Skip indirects, Dept'S (095) OR GREATER.!     *~
            * 09/27/96 ! Corrected Prob with Dept (000) for Parts ! RHH *~
            *          !  also Dept (056).                        !     *~
            * 10/22/96 ! Mod to Check PULL_FLAG$ to see if Active ! RHH *~
            *          !  for Pulling from Inventory.             !     *~
            *          !  Only "Y" is Valid to Pull from Stock    !     *~
            *          !  all others Skipped.                     !     *~
            * 01/08/97 ! Mod UPS (1,2,3,11) - Display (3,5)       ! RHH *~
            *          !  Sample (2,4,6)                          !     *~
            * 01/24/97 ! Mod for new planning to process STOCK.   ! RHH *~
            *          !   STK$() = MFG Part No.                  !     *~
            *          !   QTY$() = MFG Quantity                  !     *~
            * 02/15/97 ! Mod for only Pulling Mulls for Specified ! RHH *~
            *          !   Mull Codes. Also add New Error Code    !     *~
            *          !   Values for (ERR%).                     !     *~
            * 02/21/97 ! Mod to Turn Off Pulls from Inventory     ! RHH *~
            *          !   when S.O. is for Salvage or Scrap Prod.!     *~
            * 05/12/97 ! Mod to set Special Liting Flag for       ! RHH *~
            *          !   Special Glass also Code (89).          !     *~
            * 07/09/97 ! Mod to 'CHECK_UNITS' SUBROUTINE. THE     ! RHH *~
            *          !   part number for 'SCREEN ONLY' DEPT.    !     *~
            *          !   does not have to start with a '0'      !     *~
            * 11/13/97 ! Mod for Upgrade to new Release R6.04.03  ! RHH *~
            * 01/20/98 ! Mods for New Wood Surround Codes. A00    ! RHH *~
            *          !   - A99 thru Z00 - Z99. The Old Codes    !     *~
            *          !   001 thru 080 are No Longer Valid. Also !     *~
            *          !   New 'PLAN UNIT' CODES.                 !     *~
            *          !      A0 ----> A00 thru A99               !     *~
            *          !      B0 ----> B00 thru B99               !     *~
            *          !      C0 ----> C00 thru C99               !     *~
            *          !      Etc. to Z0 ----> Z00 thru Z99       !     *~
            * 01/20/98 ! Mods for Mull Pulls, New Table(PLAN MULL)! RHH *~
            *          !   Contains the Wood Surround Codes for   !     *~
            *          !   Pulls from Stock.                      !     *~
            *          ! Mods for New Glass which is purchased    !     *~
            *          !   outside. Based on the LITING Codes in  !     *~
            *          !   the New Table (PLANGLOUT).             !     *~
            * 05/28/98 ! (EWD001) - Mod for Group Code to Support ! RHH *~
            *          !   for Configurations                     !     *~
            * 08/14/98 ! (EWD002) - Mods to Update Daily Sales    ! RHH *~
            *          !    Analysis                              !     *~
            *          ! (EWD003) - New File for Special Schedule !     *~
            *          !   (EWDSCHED). Tempered Glass & Grid      !     *~
            * 09/22/98 ! (EWD004) - Mod for special status codes  ! RHH *~
            *          !    90% thru 98% which freeze Sales order.!     *~
            *          !    99% = Credit Hold which can be changed!     *~
            *          !          and deleted.                    !     *~
            * 01/29/99 ! (EWD005) - Mod to Fix Special Problem    ! RHH *~
            *          !                                          !     *~
            * 02/15/99 ! (EWD006) - Special Mod do not pull from  ! RHH *~
            *          !    for store (NO0081), until further     !     *~
            *          !    notice.                               !     *~
            * 01/20/00 ! (EWD007) - Mod for new Marketing program ! RHH *~
            *          !    to support Samples/Displays and       !     *~
            *          !    Literature and apparel                !     *~
            *          !                                          !     *~
            * 08/21/00 ! (EWD008) - Mod for new Howship code for  ! RHH *~
            *          !    Renovator Product and UPS Code = 35.  !     *~
            *          !                                          !     *~
            * 02/05/01 ! (EWD009) - Mod to include 'ZZ' IN SPECIAL! CMG *~
            *          !            liting.                       !     *~
            * 02/05/01 ! (EWD010) - Mod to add two new buckets to ! CMG *~
            *          !             EWDPLN58 - Diamond grid &    !     *~
            *          !             garden window.               !     *~
            * 07/10/01 ! (EWD011) - Mod to Pull Special Shapes    ! RHH *~
            *          !            from Inventory. (16) Products !     *~
            *          !            and (28) Frames. Will Put in  !     *~
            *          !            Dept. 104 when planned        !     *~
            * 01/10/02 ! (EWD012) - Mod for new 'UPS' CODES FOR   ! RHH *~
            *          !            for howship.                  !     *~
            * 02/05/02 ! (EWD013) - Mod to add model 997 to garden! CMG *~
            *          !            window bucket.                !     *~
            * 06/24/02 ! (EWD014) - Mod to activate Liting Codes  ! RHH *~
            *          !            for Special Shapes            !     *~
            * 09/17/02 ! (EWD015) Fix for Special Shapes Grid Code! CMG *~
            * 10/28/02 ! (EWD016) - Mod for new patio door models.! CMG *~
            * 04/11/03 ! (EWD017) - Mod for new calculation on    ! CMG *~
            *          !            loading units. Now using cubic!     *~
            *          !            units or volume.              !     *~
            * 06/13/03 ! (EWD018) - Mod to exclude In Line Mulls  ! CMG *~
            * 09/17/03 ! (EWD019) - Mod to write primary dept to  ! CMG *~
            *          !            EWDSCHED                      !     *~
            * 10/28/03 ! (EWD020) - Mod to exclude stock tempered ! CMG *~
            *          !     from EWDSCHED                        !     *~
            * 11/24/03 ! (EWD021) - Mod for 'A' CELL DEPARTMENT   ! CMG *~
            * 07/21/04 ! (EWD022) - Mod for new inline wood dept  ! CMG *~
            * 04/05/05 ! (AWD023) - Mod for planning order        ! CMG *~
            * 07/28/05 ! (AWD024) - Mod to have grid '58' IN SPC  ! CMG *~
            *          !             liting bucket                !     *~
            * 04/28/06 ! (AWD025) - mod not to put on hold if not ! CMG *~
            *          !          making product                  !     *~
            * 05/23/06 ! (AWD026) - mod to always delete schedule ! CMG *~
            *          !   b/c of the weird way orders are submitt!     *~
            *          !   now with North East                    !     *~
            * 06/20/06 ! (AWD027) - mod to check where product is ! CMG *~
            *          !       going to be mulled                 !     *~
            * 07/05/06 ! (AWD028) - mod to fix non-inline product ! CMG *~
            *07/23/2007! (AWD029) - mod for impact windows        ! CMG *~
            *12/06/2007! (AWD030) - mod for oval shapes           ! CMG *~
            *01/10/2008! (AWD031) - mod for new dept 074          ! CMG *~
            *08/07/2008! (AWD032) - mod for sdl shapes ewdsched   ! CMG *~
            *10/29/2008! (AWD033) - mod to move '89' TEMP CHECK   ! CMG *~
            *          !     after dallas and thermal check       !     *~
            *04/13/2009! (AWD034) - mod for screen code           ! CMG *~
            *08/27/2009! (AWD035) mod to remove "XX" wildcard from! CMG *~
            *          !       TEMPSTOCK table lookup             !     *~
            *03/08/2010! (AWD036) - add kanban look up KANBANPT   ! CMG *~
            *03/16/2010!(AWD037) - check for screens to put on hold!CMG *~
            *06/07/2011!(AWD038) - Mod for Hopper                 ! CMG *~
            *09/20/2011!(AWD039) mod to put WW unitid in ewdsched ! CMG *~
            *10/25/2011!(AWD040) mod for new liting TP & TD       ! CMG *~
            *03/20/2012!(AWD041) mods for the 8900 routings       ! CMG *~
            *08/27/2012!(AWD042) more 8900 routing mods           ! CMG *~
            *03/25/2013!(AWD043) mod for cross dock ntx / nc logic! CMG *~
            *06/11/2013!(AWD044) SGP laminate glass               ! CMG *~
            *07/01/2013!(AWD045) OGO change ecn 2013-032          ! CMG *~
            *07/02/2013!(AWD046) mod to route BBG door to dept 020! CMG *~
            *08/28/2013!(AWD047) mod for thermal products         ! CMG *~
            *03/11/2014!(AWD048) mod 65 series lamni on hold      ! CMG *~
            *06/12/2014!(AWD049) mod to add series & style        ! CMG *~
            *12/03/2014!(AWD050) mod to move to HLDSCHED and obs  ! CMG *~
            *          !   EWDSCHED                               !     *~
            *05/21/2015! (SR65252) turn off lamin models for cutter!CMG *~
            *05/25/2015! (SR64938) ECR-2015 add SGP to model 352  ! CMG *~
            *07/15/2015! (SR67046) turn off dept 006 PVB Order    ! CMG *~
            *08/04/2015! (SR67607) Patio Stock for NTX            ! CMG *~
            *06/08/2016!(SR75267) mod for nailing fin series      ! CMG *~
            *08/05/2016! (CR594) mod Series 35 Egress Model Not   ! CMG *~
            *          !     Lite-Lift                            !     *~
            *12/14/2016! (CR799) Turn on Laminate Order Glass     ! MES *~
            *12/28/2016! (CR591) Turn off paint for screen only   ! CMG *~
            *          !   for colors Almond, Clay, Bronze        !     *~
            *05/23/2017! (CR982) Change for Caelus Hopper         !     *~
            *09/25/2017! (CR1123) 25 & 27 how ship-to in samples  ! RDB *~
            *03/06/2018! (CR1329) mods for TX screen only         ! CMN *~
            *05/17/2018! (CR1339) fix logic stopping temper door  ! RDB *~
            *07/16/2018! (CR1602) cross dock painted              ! CMG *~
            *03/19/2018! (CR1966) mod set THD Stock chk_unit$="02"! CMN *~
            *07/03/2018! (CR2115) mod force series to uppercase   ! CMN *~
            *          !  to check APPLNAIL table                 !     *~
            *07/29/2019! (CR2143) mod to route Plygem fin removal ! CMN *~
            *          !   to dept 054 REMOVNAIL                  !     *~
            *09/16/2019! (CR2235) flex screen screen code options ! CMN *~
            *09/18/2019! (CR2244) flex screen orders on hold      ! CMN *~
            *10/02/2019! (CR2264) Turn off Flex Screen will not   ! CMN *~
            *          !  paint screens, similar to casement      !     *~
            *11/12/2019! (CR2312) TX laminate on hold             ! MES *~
            *04/20/2020! (CR2521) Allow JXE for 101 dept          ! RDB *~
            *04/20/2020! (CR2522) Set painted flag black door scrn! RDB *~
            *04/23/2020! (CR2532) Add new Hopper & Garden Win modl! RDB *~
            *07/08/2020! (CR2599) Black door screen NC not paintng! RDB *~
            *07/22/2020! (CR2603) No Black TX screens 8300dh slide! RDB *~
            *07/22/2020! (CR2601) No Bronze TX screen 312 332     ! RDB *~
            *07/31/2020! (CR2633) Calc lunits for JJJ buyouts     ! RDB *~
            *09/11/2020! (CR2549) Door TX no black paint          ! RDB *~
            *01/26/2021! (CR2763) Add packaging dept TX           ! CMN *~
            *05/17/2021! (CR2825) quality dept                    ! CMN *~
            *08/19/2021! (CR2883) Add NC wrap dept 076            ! RDB *~
            *03/24/2022! (CR3063) Change field sc_cost            ! RDB *~
			*10/27/2022! (CR3186) Add prefix field to apcplnor    ! RDB *~
			*11/07/2022! (CR3193) Planning Paint with sales order ! RDB *~
			*12/21/2022! (CR3215) Screen Patio Door TX paint flag ! RDB *~
            *************************************************************

         sub "APCPLN6B"  (opt%,          /* 1%, 8%, 9% ARE VALID       */~
                          or_due$,       /* S.O. Due Date/Delivery Date*/~
                          sc_cuscode$,   /* Customer Code              */~
                          sc_so$,        /* Customer Sales Order       */~
                          sc_drop$,      /* Customer Drop Number       */~
                          sc_load$,      /* Load No. Scheduled/Assigned*/~
                          sc_st$,        /* Current S.O. Stat-PLAN STAT*/~
                          or_dte$,       /* Date of Stat Change        */~
                          or_hows$,      /* How Ship Codes             */~
                          stk$(),        /* Stock MFG Part No'S        */~
/*PAR000*/                stk_sub$(),    /* Stock MFG Sub Part No      */~
/*PAR000*/                stk_desc$(),   /* Stock Description          */~
                          qty$(),        /* Stock Quantity for Part No.*/~
/*CR3186*/                prefix_so$,    /* New prefix of sales order  */~					  
                          schema%,       /* User schema                */~
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
                          #11,           /* HNYMASTR                   */~
                          #12,           /* APCPLNSA Daily Sales-EWD002*/~
                          #13,           /* EWDSCHED Spec Sched(EWD002)*/~
                          #63,           /* BCKSUBPT        PAR000     */~
                          #14,           /* KANBANPT  (AWD036)         */~
                          #15,           /* HLDSCHED  (AWD037)         */~
                          err% )         /* 0 = OK, Not 0 = Error      */
            
        dim                              /* APCPLNSC - File            */~
            sc_dte$6, or_due$8,          /* Delivery/Production Date   */~
            sc_load$5,                   /* Sched Load No. Def ='99999'*/~
            sc_drop_seq$5,               /* Sched Drop Seq. P.O./S.O.  */~
            sc_drop$2,                   /* Customer Drop No. Def='00  */~
            sc_cus_sort$5,               /* Customer Product Sort Seq. */~
            sc_so$8,                     /* Customer S.O. Number       */~
            sc_line$2,                   /* S.O. Line Item No.         */~
            sc_dept$3,                   /* Primary dept CR3063        */~
/*PAR000*/  seq$3,                       /* SO Line in ### format      */~
            sc_part$25, spec_part$25,    /* MFG Part Number            */~
            spec_subp$20,                /* PAR000 Spec subpart number */~
            sp_part$25,                  /* (EWD011) special Shapes    */~
            sp_subp$20,                  /* PAR000 sub part number     */~
            sc_cuscode$9, sav_part$25,   /* Customer Code              */~
                                         /* Total Line Item Quantity   */~
                                         /* Total Make Qty Line Item   */~
                                         /* Total Pull Qty Line Item   */~
                                         /* Total Line Item Price(Net) */~
                                         /* Total Line Item Cost       */~
                                         /* Total Line Item Units      */~
            sav_subp$20,                 /* PAR000 sav subpart         */~
            sc_txt$4,                    /* Line Item Text Id          */~
            sc_inv$1,                    /* Inventory Updated (Y or N) */~
            sc_pload$5,                  /* Parent Load No. Def='99999'*/~
            sc_st$2,                     /* Sched Line Item Status Code*/~
            sc_ldte$6, or_dte$8,         /* Sched Line Item Status Date*/~
            sc_grp$1,                    /* Configuration Code (EWD001)*/~
            sc_special$10, or_special$10,/* Special Product Flags      */~
            sc_key$10,                   /* Primary Key - S.O. Line It.*/~
            bck_key$25, hny_key$64,      /* BCKMASTR/BCKLINES - KEY    */~
            readkey$50, desc$30,         /* GENCODES GENERIC KEY       */~
            savekey$50,                  /* Save GENCODES Key (AWD041) */~
            sc_rec$128, or_rec$170,      /* DETAIL/HEADER RECORDS      */~
            or_hows$2,                   /* Sales Order How Ship Code  */~
            or_sls$4,                    /* (EWD002) - Salesman Code   */~
            sd_key$23, pl_key$15,        /* Update Keys                */~
            pl_dept$3, pl_model$3,       /* Department and Model Codes */~
            primary_dept$3,              /* Primary Dept Code (EWD019) */~
            pl_proc$2,                   /* Planning Process Code      */~
            upmh$(50%)1,                 /* Unit Codes Flags           */~
            pull_flag$1,                 /* "Y"= Pull from Inventory   */~
            wood$3, wood_scrpt$2,        /* Wood Surround Flags        */~
            chk_unit$2,                  /* PLAN UNIT - Codes For Table*/~
            sc$30,                       /* Values for Screen Field    */~
            stk_flag$1,                  /* Schedule Stock (Y)es,(N)o  */~
            stk$(99%)25, qty$(99%)4,     /* MFG Part and Quantity      */~
/*PAR000*/  stk_sub$(99%)20,             /* Part (20)                  */~
/*PAR000*/  stk_desc$(99%)30,            /* Stock Desck                */~
/*CR1123*/  is_sample$1,                 /* Flag for sample display    */~
/*CR1966*/  sku$10,                      /* Lowes / THD sku number     */~
/*CR2883*/  bck_user_entered$3           /* STK ECAT order             */

        dim                              /* (EWD002) _ Mods            */~
            sa_rec$32,                   /* Daily Sales Analysis Rec   */~
            sa_key$17,                   /* Primary Key                */~
            model$3,                     /* Model Code                 */~
            ty$2,                        /* Glass Type Code            */~
            lt$2,                        /* Liting/Grid Code           */~
            sp_rec$128, sp_key2$23,      /* Special Scheduling Data    */~
            sp_type$1,                   /* Special Analysis Type Code */~
            sp_time$8,                   /* Time Last Change           */~
            sp_cutoff$2, vf$(5%)20,      /* Customer Cut off           */~
            sp_qty$4,                    /* Line Item Quantity         */~
            sp_usr$3,                    /* User Id                    */~
            sp_route$5                   /* customer Route Code        */

        dim                              /* (AWD037)                   */~
            hd_rec$256, hd_key1$25,      /* Special Scheduling Data    */~
            hd_key2$16                   /* Read key                   */

        dim hows$50,                     /* (EWD007) _ Mods            */~
            ss$2,                        /* Sample/Display Code        */~
            lk_config$2,                 /* For Configurations         */~
            lk_prv$2,                    /* Private Label              */~
            lk_po$16,                    /* Customer P.O. Number       */~
            lk_ln$3                      /* S.O. Line Item             */

        dim sd_wd_sort$1                 /* (AWD023) Wood Sort         */

        dim                              /* PAR000                     */~
            invmastr_key$45,             /* INVMASTR Read key          */~
            subp$20,                     /* subpart number             */~
            infp$9,                      /* info part number           */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4             /* Additional Info Fields     */

        dim mfg_plant$1,                 /* Manufacturing plant        */~
            inv_plant$1,                 /* Invoice Plant              */~
/*(AWD027)*/ specialmull$1               /* Special Mull Flag          */

/* (AWD036) */
         dim kanbanptKey1$45             /* KANBANPT readkey key 1     */

         dim unitid$10                   /* WW Unit ID        (AWD039) */

        dim beg_so$1,                    /* Begin Sales Order Number   */~
            painted$30,                  /* Painted Colors             */~
            scrnPaint$30,                /* (CR591) screen painted cl  */~
            sCaseScrn$256,               /* (CR591) screen casement    */~
            sFlexScrn$256,               /* (CR2264) flex screen       */~
            casement(99),                /* Casement Screen            */~
            painted(30)                  /* Painted                    */


        dim width$4,                     /* Width  (AWD044)            */~
            height$3                     /* Height (AWD044)            */


/* (AWD049) */
        dim series$16,                   /* WW Series                  */~
            style$10                     /* WW Style                   */

/* (SR75267) */
         dim table$9,                    /* Table To Read              */~
             genkey$15,                  /* GENCODES Key to Read       */~
             descr1$30,                  /* Description                */~
             codeLen$2,                  /* Code Length                */~
             descr2$30,                  /* Description                */~
             descr3$30                   /* Description                */

         dim                             /* (CR1966) Buyout Dept Route */~
             plygem_buyout$2,            /* PlyGem BuyOut              */~
/*(CR2825)*/ qlty_dept_flag$1            /* Quality dept               */             
             


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Planning Sales Order Schedule Util"
            pname$ = "APCPLN6B - Rev: R6.04"

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
            * #7  ! APCPULLS ! APC Pull from Inventory (Interium)       *~
            * #8  ! INVQUAN  ! Inventory Quantities Master File         *~
            * #9  ! APCPLNDP ! Planning Master Department File          *~
            * #10 ! APCPLNSD ! Planning S.O. Scheduling Dept. Detail    *~
            * #11 ! INVMASTR ! MASTER INVENTORY FILE                    *~
            * #12 ! APCPLNSA ! Daily Sales Analysis (EWD002)            *~
            * #13 ! EWDSCHED ! Special Scheduling Data File (EWD002)    *~
            * #63 ! BCKSUBPT ! Sub Part File                PAR000      *~
            *************************************************************
            
    REM *************************************************************~
        *                I N I T I A L I Z A T I O N                *~
        * --------------------------------------------------------- *~
        * Initializes information necessary for program.            *~
        *************************************************************
/* (AWD041) */
    init(" ") beg_so$, painted$, scrnPaint$, sCaseScrn$, sFlexScrn$
    beg_so$ = str(sc_so$,1%,1%)
    painted$ = "5,I,J,K,L,M,N,O,P"
/* Only paint P-Bronze for certain Series */
    scrnPaint$ = "I,J,K,L,O"        /* (CR1329) add black  CR2522 */
    sCaseScrn$ = "039,040,041,042,043,044,045,046,047,048,049"
    sFlexScrn$ = "0F1,0F2,0F3,0F4,0F5,0F6,0F7,0F8,0F9" /* (CR2264) */

    init(" ") sc_rec$, or_rec$, readkey$, sc_key$, sd_key$
    sc_dte$  = str(or_due$,1%,6%)
    sc_ldte$ = str(or_dte$,1%,6%)
    sc_drop_seq$ = "00000"             /* Default Drop Seq. No.*/
    sc_inv$      = "N"                 /* Default Inventory Upd*/
    sc_pload$    = sc_load$            /* Sched and Parent Eq. */
    or_special$  = "NNNNNNNNNN"        /* Default Specials 'NO'*/
    sc$          = "123456789ABCIJYZDWX"  /* (CR2235)          */
                                       /* Valid Screen Options */
    pl_proc$     = "01"                /* Fabrication Only     */
    stk_flag$ = "N"
    if str(sc_so$,1%,1%) = "S" then stk_flag$ = "Y"
    serr% = 0%                         /* Clear Error Flag     */
    err% = 1%
                                       /* (EWD007)             */
                                       /* 2,3,4,5,6,22,32,33   */
    hows$ = " *****               *         **                 "
    hows% = 0%
    convert or_hows$ to hows%, data goto L01000

    REM *************************************************************~
        *     G E T   C U S T O M E R   A N D   S A L E S           *~
        *                 O R D E R   D A T A                       *~
        *************************************************************


L01000:
        if stk_flag$ <> "Y" then goto L02110
          gosub schedule_stock
        goto exit_sub

L02110:
/* (AWD041) */
REM              GOSUB LOAD_8900_MDLS      /* (SR75267) logic removed */
REM              GOSUB LOAD_65_MDLS        /* (SR75267) logic removed */

/* (AWD026) - always try to delete schedule */
REM IF OPT% = 1% THEN GOTO UPDATE_SO           /* ADD NEW S.O.         */

     gosub del_sched                 /* Delete Planning Data */
                                     /* and Special Glass    */
                                     /* Data in (EWDSCHED)   */
    update_so
      if opt% = 9% then goto exit_sub    /* Delete S.O.          */
      gosub calc_sales_order         /* Same for Add and     */
                                           /* Change               */
    goto exit_sub                      /* Terminate Subroutine */

    REM *************************************************************~
        *               F O R M A T  S T A T E M E N T S            *~
        *************************************************************

L02230: FMT                              /* APCPLNSC - File            */~
            CH(6),                       /* Delivery/Production Date   */~
            CH(5),                       /* Sched Load No. Def ='99999'*/~
            CH(5),                       /* Sched Drop Seq. P.O./S.O.  */~
            CH(2),                       /* Customer Drop No. Def='00  */~
            CH(5),                       /* Customer Product Sort Seq. */~
            CH(8),                       /* Customer S.O. Number       */~
            CH(2),                       /* S.O. Line Item No.         */~
            CH(25),                      /* MFG Part Number            */~
            CH(9),                       /* Customer Code              */~
            BI(2),                       /* Total Line Item Quantity   */~
            BI(2),                       /* Total Make Qty Line Item   */~
            BI(2),                       /* Tot Pull Qty Ln Itm-Finishe*/~
            BI(2),                       /* Tot Pull Qty Ln Itm-Mulling*/~
            PD(14,4),                    /* Total Line Item Price(Net) */~
            CH(5),                       /* Filler CR3063              */~
            CH(3),                       /* Primary dept CR3063        */~
            PD(14,4),                    /* Total Line Item Units      */~
            CH(4),                       /* Line Item Text Id          */~
            CH(1),                       /* Inventory Updated (Y or N) */~
            CH(5),                       /* Parent Load No. Def='99999'*/~
            CH(2),                       /* Sched Line Item Status Code*/~
            CH(6),                       /* Sched Line Item Status Date*/~
            CH(10),                      /* Special Product Flags Y/N  */~
            CH(01)                       /* Configuration Group Code   */
                                         /* (EWD001)                   */
                                         
/* CR3063       PD(14,4)                    Total Line Item Cost       */

L02480:     FMT CH(170)                  /* APCPLNOR - Record          */

L02500:     FMT POS(25), CH(30)          /* GENCODES - Tables          */

        REM *************************************************************~
            *       U P D A T E   R O U T I N E S   F O R   G R O U P   *~
            *************************************************************

    calc_sales_order
       read #2,key 4% = sc_so$, using L02510, sp_route$, lk_po$,  ~
                                sp_usr$, or_special$, eod goto L02560
L02510:     FMT POS(11), CH(5), POS(36), CH(16), POS(143), CH(3),   ~
                POS(160), CH(10)
                                              /* (EWD007)          */

L02560:
       or_units = 0.0 : or_value = 0.0 : or_cost  = 0.0
       or_mak%  = 0%  : or_pul%  = 0%  : ord_disc = 0.0
       init(" ") bck_user_entered$ 
       init(" ") bck_key$
       str(bck_key$,1%,9%)   = sc_cuscode$  /* BCKMASTR - Lookup */
       str(bck_key$,10%,16%) = sc_so$
/* CR2883 add user field */       
       read #4,key = bck_key$, using L02640, bck_user_entered$, ord_disc, ~
                                             qlty_dept_flag$,  ~
                                                  eod goto calc_sales_done
L02640:         FMT POS(836), CH(03), POS(859), PD(14,4), POS(990), CH(01) 

         bck_key$ = all(hex(00))
         str(bck_key$,1%,8%) = sc_so$
    next_s_o                                 /* BCKLINES - Process */
      sc_tqty%  = 0%  : sc_mqty% = 0%  : sc_pqty% = 0%
      sc_pqty1% = 0%  : sc_price = 0.0    /* CR3063 : sc_cost  = 0.0  */
      sc_units = 0.0  : sc_line% = 0%
      read #5,key > bck_key$, using L02750 , bck_key$, sc_part$,   ~
                             ord_qty, ord_price, ln_disc, sc_txt$, ~
                             unitid$, sc_grp$, lk_config$, lk_prv$,~
                             specialmull$, sku$, eod goto calc_sales_done
                                           /* (CR1966) sku */

L02750:         FMT POS(10), CH(19), POS(32), CH(25), POS(93), PD(14,4), ~
                POS(165), 2*PD(14,4), POS(242), CH(4), POS(265), CH(10), ~
                POS(279), CH(1), CH(2), CH(2),  POS(289), CH(1), CH(10)

        if sc_so$ <> str(bck_key$,1%,8%) then goto calc_sales_done
        convert str(bck_key$,17%,3%) to sc_line%, data goto calc_sales_ln

calc_sales_ln:
/* + (CR1966) */
        thdstk%, sku%, buyout%, plygem% = 0%
        if lk_prv$ = "18" or lk_prv$ = "19" then plygem% = 1%
        convert sku$ to sku%, data goto notSku
        if sku% > 0% and lk_prv$ = "19" then thdstk% = 1%

notSku:
        init(" ") model$, width$, height$
        model$ = str(sc_part$,1%,3%)        /* Model Code       */
        width$  = str(sc_part$,13%,4%)
        height$ = str(sc_part$,17%,3%)

        lk_ln$ = str(bck_key$,17%,3%)
        convert sc_line% to sc_line$, pic(00) /* Line Item No. */

        sc_tqty% = int(ord_qty)               /* Ln Item Qty   */
        sc_price =  round(  ord_price * ord_qty, 2)
                                    /* Line Item Discounted Price  */
        discamt  =  round( sc_price * ln_disc * .01, 2)
        sc_price =  round( sc_price - discamt, 2)
                                    /* Calc. Line Item Net Price   */
        discamt  = round(sc_price * ord_disc * .01, 2)
        sc_price = round(sc_price - discamt, 2)
        or_value = round(or_value + sc_price, 2)
        gosub check_screenonl
        gosub check_mullonly
/* CR2633 moved up in code */
        so_inv$  = str(bck_key$,1%, 8%)        /* PAR000 */
        item_no$ = str(bck_key$,17%,3%)        /* PAR000 */
        gosub lookup_subpart                   /* PAR000 */
                                /* S.O. Line Item Values (Net) */
        gosub calc_units        /* Calculate Loading Units/Line*/

         subp$    = str(bcksubpt_rec$,48%,20%)  /* PAR000 */
         infp$    = str(bcksubpt_rec$,132%,9%)  /* PAR001 */
         series$  = str(bcksubpt_rec$,169%,16%) /*(AWD049)*/
         style$   = str(bcksubpt_rec$,185%,10%) /*(AWD049)*/
/* + (CR1966)*/
         plygem_buyout$ = str(bcksubpt_rec$,199%,02%)

         convert plygem_buyout$ to buyout%, data goto badBuyout

badBuyout:
/* - (CR1966)*/
/* + (SR75267) */
        mdl8900% = 0%
        init(" ") table$, genkey$, codeLen$, descr1$, descr2$, descr3$
        generr% = 0%
        table$ = "APPLNAIL"
        genkey$ = series$
        call "UPPERCASE" (genkey$, err%)   /* (CR2115) */
        call "GENREAD" (table$, genkey$, descr1$, codeLen$, descr2$,  ~
                        descr3$, generr%)
          if generr% = 0% then mdl8900% = 1%

/* - (SR75267) */

/* + (CR2143) */
        
        removnail% = 0%
        init(" ") table$, genkey$, codeLen$, descr1$, descr2$, descr3$
        generr% = 0%
        table$ = "REMOVNAIL"
        genkey$ = series$
        call "UPPERCASE" (genkey$, err%)
        call "GENREAD" (table$, genkey$, descr1$, codeLen$, descr2$,   ~
                        descr3$, generr%)
           if generr% = 0% then removnail% = 1%
/* - (CR2143) */
REM  IF SCREENONL% = 1% THEN PAINTED% = 0%   /* (AWD042) */
        scrnpnt%  = 0%                       /* CR2522 */
        gosub check_painted               /* (AWD041) */
        gosub calc_on_hand      /* Calculate Pulls from Inv.   */
        or_mak% = or_mak% + sc_mqty%   /* Total S.O. Make Qty  */
        or_pul% = or_pul% + (sc_pqty% + sc_pqty1%)
                                /* Total S.O. Pull Fin and Mull*/
        gosub prod_analysis     /* Write Unit Record for Each  */
                                /* Applicable Department/Shift */
        gosub so_add            /* Update New Master Scheduler */
        gosub analyize_glass    /* (EWDSCHED)                  */
        gosub analyizeHldsched
        gosub update_marketing
      goto next_s_o
    calc_sales_done
      gosub so_update               /* Update Planning S.O. Header */
      gosub update_daily            /* (EWD002) Update Daily Sales */
    return

    update_marketing                /* (EWD007) Marketing          */
      error% = 0%
                                    /* Not Sample or Literature    */
      if str(hows$,hows%,1%) <> "*" then return

                                     /* (EWD0007) Update Marketing */
      call "EWDPLN6B" (opt%,           /* 1=Add,8=Change,9=Delete    */~
                      lk_prv$,         /* Customer Private Label     */~
                      sc_cuscode$,     /* Customer Code              */~
                      lk_po$,          /* Customer P.O. Number       */~
                      sc_so$,          /* Customer Sales Order No    */~
                      lk_ln$,          /* Sales Order Line Item      */~
                      or_hows$,        /* S.O. Howship Code          */~
                      sc_dte$,         /* S.O. Due Date              */~
                      sc_tqty%,        /* Line Item Quantity         */~
                      ord_price,       /* Line Item Price Unit       */~
                      ln_disc,         /* Line Item Discount Percent */~
                      ord_disc,        /* Oder Discount Percent      */~
                      sc_part$,        /* MFG Part Number            */~
                      #6,              /* GENCODES Master Tables     */~
                      error%)          /* Return Code                */

    return
                                    /* (EWD007) End of Update      */
    update_daily                            /* (EWD002) Daily Sls */
      init(" ") sa_key$, sa_rec$          /* Not Done for Stock */
      if str(sc_so$,1%,1%) = "S" then return /* Skip Stock      */
      str(sa_key$,1%,9%)  = sc_cuscode$
      str(sa_key$,10%,8%) = sc_so$
      read #12,hold,key = sa_key$, using L02800, sa_rec$,         ~
                                         eod goto update_daily_sales
L02800:        FMT CH(32)
        delete #12
        if opt% = 9% then return         /* Delete S.O.         */
    update_daily_sales
      str(sa_rec$,1%,6%)  = date          /* Date of Last Change */
      str(sa_rec$,7%,4%)  = or_sls$
      str(sa_rec$,11%,9%) = sc_cuscode$
      str(sa_rec$,20%,8%) = sc_so$
      str(sa_rec$,28%,2%) = sc_st$        /* 00=New, 01=Change   */
                                          /* Credit and Freeze   */
                                          /* are treated as Chg  */

      if sc_st$ > "01" then str(sa_rec$,28%,2%) = "01"
      str(sa_rec$,30%,3%) = "   "

      put #12, using L02800, sa_rec$
      write #12, eod goto update_daily_done
    update_daily_done
    return

    analyize_glass                 /* Glass Analysis for Tempered */
      sdl_shape% = 0%
      if str(sc_so$,1%,1%) = "S" then goto analyize_glass_done
                                           /* Skip Stock S.O.     */
      init(" ") model$
      model$ = str(sc_part$,1%,3%)        /* Model Code          */
      sp_shapes% = 0%
      gosub check_special_shapes
REM IF SP_SHAPES% = 1% THEN GOTO SCHEDULED_SHAPES
      gosub check_garden                  /* Check Garden Window */
       if gar_window% = 1% then goto schedule_garden
      ty$ = str(sc_part$,5%,2%)           /* Glass Type Code     */
      lt$ = str(sc_part$,7%,2%)           /* Liting Code Grid    */
REM IF TY$ = "89" THEN GOTO SCHEDULE_TEMP
                                           /* Special Glass       */
      if str(model$,1%,1%) = "9" then goto analyize_glass_done
                                          /* Skip Bay/Bow        */
      gosub check_liting                  /* Check Liting        */
      gosub check_diamond_grid            /* Check Diamond Grid (EWD010) */
      gosub check_patio                   /* Check Stock Patio   */
      gosub check_thermal                 /* Thermal Models (AWD029)*/
      gosub check_dallas                  /* Dallas Models (AWD029)*/
      gosub check_hopper                  /* Hopper Parts (AWD038) */

/* (AWD033) have to send because "Q" shape can go to Thermal or Dallas */
      if thermal% = 1% then goto schedule_thermal
      if dallas%  = 1% then goto schedule_dallas
/* (AWD030) BEG  check dallas and thermal before 'Q' */
      if str(lt$,1,1) = "Q" then sp_shapes% = 1%
      if sp_shapes% = 1% and str(subp$,8,1) = "1" then sdl_shape% = 1%
REM CHECK SDL SHAPE BEFORE TEMPERED
      if sdl_shape% = 1% then goto schedule_sdl_shape
      if sp_shapes% = 1% then goto scheduled_shapes
      if hopper% = 1% then goto schedule_hopper

/* (AWD033) tempered check needs to be after shapes */
/*    b/c tempered checks for shapes too           */
      gosub check_temp                    /* Check Tempered Glass*/
/* CR3193 */	  
      if painted% = 1% and                                                 ~
	     (str(sc_so$,1%,1%) = "0" or str(sc_so$,1%,1%) = "1" )             ~
                                   then sp_temp% = 0% /*(CR982)*/
/* (AWD033) -- after dallas and thermal */
/* IF SP_PATIO% = 1% AND SP_LITING% = 0% THENGOTO ANALYIZE_GLASS_DONE  CR1339 */
      if sp_temp% = 1% then goto schedule_temp
      if ty$ = "89" then goto schedule_temp
finish_thermal_checks:
      if sp_liting% = 1% then goto schedule_lit
      if dg_liting% = 1% then goto schedule_grid
      if cross_dock_t% <> 1% then goto noThermalHold
        gosub isDoor
         if door% = 0% then goto noDoorHold
         if numOfPanels% <> 2% then goto noDoorHold /*Only 2Pane  */
         if (width$ = "594" or width$ = "704" or    /*STOCK SIZES */~
             width$ = "954") and height$ = "794" then goto noDoorHold
     gosub schedule_thermal
noDoorHold:
      if painted% = 0% then goto noPaintHold
     gosub schedule_thermal
noPaintHold:
      gosub isLamn
       if lamn% = 0% then goto noThermalHold
     gosub schedule_thermal
noThermalHold:
     goto scheduleHldschedDone              /* Not Applicable      */
    scheduled_shapes
      sp_type$ = "2"                   /* Special Shapes      */
    goto writeHLDSCHED
    schedule_lit
      sp_type$ = "0"                   /* Scheduled Liting    */
    goto writeHLDSCHED
    schedule_temp                          /* (AWD047) */
REM IF CROSS_DOCK_T% = 1% THEN GOTO FINISH_THERMAL_CHECKS
      sp_type$ = "1"                   /* Scheduled Temp      */
    goto writeHLDSCHED
    schedule_grid
      sp_type$ = "3"                   /* Scheduled Diamond Grid */
    goto writeHLDSCHED
    schedule_garden
      sp_type$ = "4"                   /* Scheduled Garden Window */
    goto writeHLDSCHED
    schedule_thermal
      sp_type$ = "6"                   /* Scheduled Thermal Window */
    goto writeHLDSCHED
    schedule_dallas
      sp_type$ = "7"                   /* Scheduled Dallas Window */
    goto writeHLDSCHED
    schedule_sdl_shape                     /* Scheduled SDL Shapes */
       sp_type$ = "8"
    goto writeHLDSCHED
    schedule_hopper
       sp_type$ = "A"
    goto writeHLDSCHED
    schedule_ewdsched                      /* (EWDSCHED) - File   */
      if cross_dock_t% = 1% and sp_type$ <> "6" then goto schedule_thermal
        kanban% = 0%                       /* (AWD036) */
        gosub checkKanbanpt                /* (AWD036) */
        if kanban% = 1% then return        /* (AWD036) */

      init(" ") sp_rec$, sp_key2$, sp_time$
      call "TIME" (sp_time$)
      convert sc_tqty% to sp_qty$, pic(0000)

      str(sp_key2$,1%,9%)  = sc_cuscode$
      str(sp_key2$,10%,8%) = sc_so$
      str(sp_key2$,18%,2%) = sc_line$

      gosub lookup_cust                  /* Get Cut off Day 1-7*/
      if sc_tqty% < 1% then goto analyize_glass_done
      if sc_mqty% < 1% and cross_dock_t% = 0%  then goto analyize_glass_done
      for sp% = 1% to sc_tqty%
         convert sp% to str(sp_key2$,20%,4%), pic(0000)
                                           /* Piece (MFG Window) */
         read #13,hold,key 1% = sp_key2$, using L03000, sp_rec$, ~
                                     eod goto schedule_ewdsched_add
L03000:    FMT CH(128)
           delete #13
                                          /* Released to Planning */
           if str(sp_rec$,7%,1%) = "Z" then goto schedule_ewdsched_add
      goto schedule_ewdsched_chg
      schedule_ewdsched_add            /* No Change After Add*/
        str(sp_rec$,1%,6%)  = date     /* Create Date        */
        str(sp_rec$,7%,1%)  = "0"      /* (PLAN SCH1)        */
        str(sp_rec$,16%,23%)= sp_key2$ /* Cust,S.O.,Line Ite */
        str(sp_rec$,93%,4%) = sc_txt$  /* Line Item Text     */

      schedule_ewdsched_chg            /* Fields Can Change  */
        str(sp_rec$,8%,1%)  = sp_type$ /* (PLAN SCH2)        */
        str(sp_rec$,9%,2%)  = sp_cutoff$/* customer Cutoff   */
        str(sp_rec$,11%,5%) = sp_route$/* Cust Route Code    */
        str(sp_rec$,39%,6%) = sc_dte$  /* Cust Due Date      */
        str(sp_rec$,45%,25%)= sc_part$ /* Part Number        */
        str(sp_rec$,70%,4%) = sp_qty$  /* Line Item Quantity */
        str(sp_rec$,74%,2%) = sc_st$   /* Planning Status    */
        str(sp_rec$,76%,3%) = sp_usr$  /* User Id            */
        str(sp_rec$,79%,6%) = date     /* Last Mod Date      */
        str(sp_rec$,85%,8%) = sp_time$ /* Time Last Mode     */
        str(sp_rec$,97%,3%) = primary_dept$ /* (EWD019)      */
        str(sp_rec$,100%,10%) = unitid$ /* WW Unitid (AWD039)*/
        str(sp_rec$,111%,19%)= " "     /* Filler             */
      write #13, using L03000, sp_rec$, eod goto L03010

L03010:
      next sp%

    analyize_glass_done
    return

    analyizeHldsched
      ritescrn% = 0%
      gosub checkRiteScrn
       if ritescrn% = 1% then gosub createRiteScrn
      sgp%, obs%,lamn% = 0%
      model$ = str(sc_part$,1%,3%)
      gosub checkSGP
/* SR65252  */
REM IF (SGP% = 1% OR OBS% = 1%) OR (MFG_PLANT$ = "1" AND
REM LAMN% = 1% AND NOPVB% = 0%) THEN GOSUB CREATESGP

/*(CR799) (CR2312)*/
REM     if (sgp% = 1% or obs% = 1%) or (mfg_plant$ = "1" and lamn% = 1%) ~
REM                      then gosub createSGP
/*(CR2312)*/
if obs% = 1% or lamn% = 1% then gosub createSGP

/* (AWD050) */
       if str(series$,1%,4%) = "35  " then gosub createLiteLift
/* (CR2244) */
       if str(sc_part$,11%,1%) = "W" or str(sc_part$,11%,1%) = "X"  ~
                                                then gosub createFlexScrn
    return

    createRiteScrn
      sp_type$ = "9"
    goto writeHLDSCHED
    createSGP
      sp_type$ = "B"
    goto writeHLDSCHED
    createLiteLift                                /* (CR594)  */
      if model$ = "E44" then return             /* (CR594)  */
      if str(style$,1%,3%) = "DHE" then return  /* No LiteLift     */
      sp_type$ = "C"                            /* for Egress Model*/
     goto writeHLDSCHED
    createFlexScrn                                         /* (CR2244) */
     sp_type$ = "E"                                /* E = Flex screen  */
     goto writeHLDSCHED
    writeHLDSCHED
/* (AWD050) move code down here */
      if cross_dock_t% = 1% and (sp_type$ <> "6" and sp_type$ <> "4")    ~
                             then goto schedule_thermal /* (AWD047) */

      kanban% = 0%                       /* (AWD036) */
      gosub checkKanbanpt                /* (AWD036) */
      if kanban% = 1% then return        /* (AWD036) */
/* (\AWD050) */
      init(" ") hd_rec$, hd_key1$
      str(hd_key1$,1%,1%)  = "0"
      str(hd_key1$,2%,1%)  = sp_type$
      str(hd_key1$,3%,9%)  = sc_cuscode$
      str(hd_key1$,12%,8%) = sc_so$
      str(hd_key1$,20%,2%) = sc_line$

      init(" ") sp_rec$, sp_key2$, sp_time$
      call "TIME" (sp_time$)
      convert sc_tqty% to sp_qty$, pic(0000)

      str(sp_key2$,1%,9%)  = sc_cuscode$
      str(sp_key2$,10%,8%) = sc_so$
      str(sp_key2$,18%,2%) = sc_line$

      gosub lookup_cust                  /* Get Cut off Day 1-7*/

      if sc_tqty% < 1% then goto scheduleHldschedDone
      if sc_mqty% < 1% and cross_dock_t% = 0% then goto scheduleHldschedDone

      for sp% = 1% to sc_tqty%
        convert sp% to str(hd_key1$,22%,4%), pic(0000)

        read #15,hold,key 1% = hd_key1$, using L03500, hd_rec$, ~
                                     eod goto scheduleHldschedAdd
L03500:  FMT CH(256)

          delete #15
                                          /* Released to Planning */
          if str(hd_rec$,7%,1%) = "Z" then goto scheduleHldschedAdd
        goto scheduleHldschedChg

       scheduleHldschedAdd                    /* No Change After Add*/
         str(hd_rec$,1%,6%)  = date     /* Create Date        */
        str(hd_rec$,7%,1%)  = "0"      /* (PLAN SCH1)        */
        str(hd_rec$,16%,25%)= hd_key1$ /* Cust,S.O.,Line Ite */
        str(hd_rec$,41%,1%) = "0"      /* (PLAN SCH1)        */
        str(hd_rec$,42%,1%) = sp_type$ /* (PLAN SCH2)        */
        str(hd_rec$,97%,4%) = sc_txt$  /* Line Item Text     */
      scheduleHldschedChg                    /* Fields Can Change  */
        str(hd_rec$,8%,1%)  = sp_type$ /* (PLAN SCH2)        */
        str(hd_rec$,9%,2%)  = sp_cutoff$/* customer Cutoff   */
        str(hd_rec$,11%,5%) = sp_route$/* Cust Route Code    */
        str(hd_rec$,43%,6%) = sc_dte$  /* Cust Due Date      */
        str(hd_rec$,49%,25%)= sc_part$ /* Part Number        */
        str(hd_rec$,74%,4%) = sp_qty$  /* Line Item Quantity */
        str(hd_rec$,78%,2%) = sc_st$   /* Planning Status    */
        str(hd_rec$,80%,3%) = sp_usr$  /* User Id            */
        str(hd_rec$,83%,6%) = date     /* Last Mod Date      */
        str(hd_rec$,89%,8%) = sp_time$ /* Time Last Mode     */
        str(hd_rec$,97%,4%) = sc_txt$  /* Line Text          */
        str(hd_rec$,101%,3%) = primary_dept$ /* Department    */
        str(hd_rec$,104%,10%) = unitid$  /*WW Unitid (AWD039)*/
        str(hd_rec$,114%,1%) = " "
        if lamn% = 1% then str(hd_rec$,114%,1%) = "L"
        if sgp% = 1%  then str(hd_rec$,114%,1%) = "S"
        str(hd_rec$,115%,142%)= " "     /* Filler             */
        write #15, using L03500, hd_rec$, eod goto L03510

L03510:
      next sp%
    scheduleHldschedDone
    return
/* (AWD037) */

    check_liting
      sp_liting% = 0%
REM IF LT$ = "97" OR LT$ = "98" OR LT$ = "99" THEN SP_LITING% = 1%
REM IF LT$ = "A0" THEN SP_LITING% = 1%
      if lt$ = "99" then sp_liting% = 1%
      if lt$ = "83" or lt$ = "84" or lt$ = "85" then sp_liting% = 1%
      if lt$ = "86" or lt$ = "87" or lt$ = "ZZ" then sp_liting% = 1%
      if lt$ = "59" then sp_liting% = 1%
    return

    check_diamond_grid
      dg_liting% = 0%
      if lt$ = "97" or lt$ = "98" then dg_liting% = 1%
      if lt$ = "TD" then dg_liting% = 1%
    return

    check_patio                              /* Check Stock Patio  */
      sp_patio% = 0%                       /* Glass              */
      if str(model$,1%,1%) <> "3" then return
      if stock% = 1% then return           /*  (EWD020)          */
                                             /*  (EWD016)          */
REM IF MODEL$ <> "311" AND MODEL$ <> "312" AND MODEL$ <> "313"   ~
REM   AND MODEL$ <> "314" THEN RETURN
REM  IF MODEL$ = "311" OR MODEL$ = "312" THEN GOTO CHECK_PATIO_GLASS
REM  IF MODEL$ = "313" OR MODEL$ = "314" THEN GOTO CHECK_PATIO_GLASS
REM  IF MODEL$ = "332" OR MODEL$ = "333" THEN GOTO CHECK_PATIO_GLASS
REM  IF MODEL$ = "334" OR MODEL$ = "335" THEN GOTO CHECK_PATIO_GLASS
/*(CR1339)*/
      init(" ") readkey$                       /* (CR982) */
      str(readkey$,1%,9%)   = "PLAN DOOR"
      str(readkey$,10%,15%) = model$
      read #6,key = readkey$, eod goto PatioDone
         goto check_patio_glass
    PatioDone
    return
    check_patio_glass
      if mfg_plant$ = "4" then return /* Do not apply to TX SR67607 */
REM IF TY$ = "08" OR TY$ = "13" OR TY$ = "16" THEN SP_PATIO% = 1%
      sp_patio% = 1%
    return
                                             /*  (EWD016)          */
    check_temp                               /* (EWD011)           */
      sp_temp%, stock%, sp_shapes% = 0%
REM DO NOT PUT TEMPERED SHAPES ON HOLD....

      gosub check_special_shapes
       if sp_shapes% = 1% then return
      
      init(" ") readkey$
      str(readkey$,1%,9%)   = "PLAN TEMP"
      str(readkey$,10%,15%) = ty$
      read #6,key = readkey$, eod goto check_temp_done
       gosub check_stock_temp
        if stock% = 1% then return
      sp_temp% = 1%
    check_temp_done
    return
    check_stock_temp
      stock% = 0%
                              /* Cottage & Oriel can not be stock  */
      if str(sc_part$,9%,2%) >= "70" and str(sc_part$,9%,2%) <= "97" then return
      init(" ") readkey$
      str(readkey$,1%,9%)  = "TEMPSTOCK"
      str(readkey$,10%,3%) = str(sc_part$,1%,3%)
      str(readkey$,13%,2%) = str(sc_part$,5%,2%)
/* (AWD035) Remove XX wildcard must lookup by glass code */
REM IF STR(SC_PART$,1%,1%) = "3" THEN STR(READKEY$,13%,2%) = "XX"
      str(readkey$,15%,4%) = str(sc_part$,13%,4%)
      str(readkey$,19%,3%) = str(sc_part$,17%,3%)
      read #6,key = readkey$, eod goto check_stock_done

        stock% = 1%
    check_stock_done
    return

    check_special_shapes
      if cross_dock_t% = 1% then return   /* (AWD047) only check  */
                                          /* Thermal Product      */
                                          /* 2014/03/11 change to */
                                          /* skip if thermal cross*/
                                          /*dock; logic backwards */

      sp_shapes% = 0%
      init(" ") readkey$
      str(readkey$,1%,9%)   = "PLAN SHAP"
      str(readkey$,10%,15%) = model$
      read #6,key = readkey$, eod goto check_special_shapes_done
        sp_shapes% = 1%
    check_special_shapes_done
    return

    check_garden
       gar_window% = 0%
       if str(model$,1%,3%) = "998" then gar_window% = 1%
       if str(model$,1%,3%) = "995" then gar_window% = 1%  /* CR2532 */
       if str(model$,1%,3%) = "997" and pull_flag$ = "N" then gar_window% = 1%
    return

    check_thermal
      thermal% = 0%
      init(" ") readkey$
      str(readkey$,1%,9%)   = "THERMAL  "
      str(readkey$,10%,15%) = model$
      read #6,key = readkey$, eod goto check_thermal_done

        thermal% = 1%
    check_thermal_done
    return

    check_dallas
        dallas% = 0%
        init(" ") readkey$
        str(readkey$,1%,9%)   = "DALLAS   "
        str(readkey$,10%,15%) = model$
        read #6,key = readkey$, eod goto check_dallas_done
           dallas% = 1%
    check_dallas_done
    return

    checkRiteScrn
      riteScrn% = 0%
      init(" ") readkey$
      str(readkey$,1,9)  = "RITESCRN"
      str(readkey$,10,3) = str(sc_part$,1,1) & "**"
      str(readkey$,13,1) = "*"
      str(readkey$,14,1) = str(subp$,4,1)

       read #6,key = readkey$, eod goto noRiteScrn1
         riteScrn% = 1%
    return
    noRiteScrn1
      riteScrn% = 0%
      init(" ") readkey$
      str(readkey$,1,9)  = "RITESCRN"
      str(readkey$,10,3) = str(sc_part$,1,3)
      str(readkey$,13,1) = str(sc_part$,11,1)
      str(readkey$,14,1) = "*"

      read #6,key = readkey$, eod goto noRiteScrn2
        riteScrn% = 1%
    return
    noRiteScrn2
    return
                                     /* (AWD037\) */
/* (AWD044) */
    checkSGP
      partlen% = 0%
      partlen% = len(sc_part$)
      if partlen% < 19% then return

      init(" ") width$, height$
      width$  = str(sc_part$,13%,4%)
      height$ = str(sc_part$,17%,3%)
       sgp%, obs%, lamn% = 0%
      gosub isLamn
      if lamn% = 0% then return

      gosub isObs
      gosub isDoor
      gosub isFixed   /* (AWD049) */
      if door% = 1% and width$ > "0720" and height$ > "800" ~
             then sgp% = 1%
/* SR64938 */
/* 2015-08-15 update to SR64938, model should be A41 not 352 */
      if str(sc_part$,1%,3%) = "A41" then sgp% = 1%  /* 352 Door - Model A41 */

                      /* (AWD049) */
      if fixed% = 0% then return
      if door% = 0% and width$ > "0440" and height$ > "440" then sgp% = 1%
      if door% = 0% and (width$ > "0720" or height$ > "720") then sgp% = 1%
    return

    isLamn
      lamn%, noPVB% = 0%
      init(" ") readkey$
      str(readkey$,1%,9%)   = "PLAN LAMN"
      str(readkey$,10%,15%) = str(sc_part$,5%,2%)
      read #6,key = readkey$, eod goto lamnDone

       lamn% = 1%
    lamnDone
    return /*CR799*/
/* SR65252 LOGIC BYPASSED WITH CR799*/
      IF MFG_PLANT$ <> "1" THEN RETURN
      IF MODEL$ = "E30" OR MODEL$ = "E31" OR MODEL$ = "E34" THEN NOPVB% = 1%
      IF MODEL$ = "E51" OR MODEL$ = "E53" OR MODEL$ = "E56" THEN NOPVB% = 1%
      IF MODEL$ = "E57" OR MODEL$ = "E58" OR MODEL$ = "E59" THEN NOPVB% = 1%
      IF MODEL$ = "E71" OR MODEL$ = "E73" OR MODEL$ = "E76" THEN NOPVB% = 1%
      IF MODEL$ = "E77" OR MODEL$ = "E78" OR MODEL$ = "E79" THEN NOPVB% = 1%
      IF MODEL$ = "S26" OR MODEL$ = "S27" OR MODEL$ = "S36" THEN NOPVB% = 1%
/* TURN OFF DEPT 047*/
      IF MODEL$ = "E51" OR MODEL$ = "E71" OR MODEL$ = "E73" THEN NOPVB% = 1%
      IF MODEL$ = "E81" OR MODEL$ = "E83" THEN NOPVB% = 1%
/*TURN OFF DEPT 047 - SR 67046*/
      IF MFG_PLANT$ = "1" AND STR(SC_PART$,1%,3%) = "E41" THEN NOPVB% = 1%
      IF MFG_PLANT$ = "1" AND STR(SC_PART$,1%,3%) = "S46" THEN NOPVB% = 1%
      IF MFG_PLANT$ = "1" AND STR(SC_PART$,1%,3%) = "S56" THEN NOPVB% = 1%
/* SR65252        */

    isObs
      obs% = 0%
      init(" ") readkey$
      str(readkey$,1%,9%)   = "OBS GED  "
      str(readkey$,10%,15%) = str(sc_part$,5%,2%)
      read #6,key = readkey$, eod goto obsDone

       obs% = 1%
    obsDone
    return

    isDoor
      init(" ") readkey$, desc$
      door% = 0%
      numOfPanels% = 2%
      str(readkey$,1%,9%)   = "PLAN DOOR"
      str(readkey$,10%,15%) = str(sc_part$,1%,3%)
      read #6,key = readkey$,using L02500 , desc$, ~
                               eod goto check_door_done

       convert str(desc$,27%,1%) to numOfPanels%,data goto badPanels

badPanels:
       door% = 1%
    check_door_done
    return
    isFixed
      init(" ") readkey$, desc$
      fixed% = 0%
      str(readkey$,1%,9%)   = "FIXEDLITE"
      str(readkey$,10%,15%) = style$

      read #6,key = readkey$, eod goto check_fixed_done

       fixed% = 1%
    check_fixed_done
    return

    check_hopper                    /* (AWD038) */ 
      hopper% = 0%
      if model$ <> "034" and model$ <> "H01" then return

      hopper% = 1%
      if model$ = "034" then return
      if model$ = "H01" and str(sc_part$,11%,1%) = "4" then return
      if model$ = "H01" and str(sc_part$,11%,1%) = "5" then return
      if model$ = "H01" and str(sc_part$,11%,1%) = "6" then return
      if model$ = "H01" and str(sc_part$,11%,1%) = "7" then return
      if model$ = "H01" and str(sc_part$,17,3%) = " " then return
      hopper% = 0%    /* Not a hopper part to hold */
    return

    lookup_cust
      init(" ") readkey$, desc$
      read #1,key = sc_cuscode$, eod goto L03030
        get #1, using L03020   , vf$()

L03020:  FMT POS(820), 5*CH(20)
       sp_cutoff$ = str(vf$(3%),1%,2%) /* Cust Delivery Code   */

       init(" ") readkey$, desc$
       str(readkey$,1%,9%)   = "PLAN CUTO"
       str(readkey$,10%,15%) = sp_cutoff$
       read #6,key = readkey$, using L03025, desc$, eod goto L03030
L03025:       FMT POS(25), CH(1)
       sp_cutoff$ = "0" & desc$            /* cut off Day 1 thru 7 */
L03030:
    return

    calc_units                          /* Only Manufactured Parts */
      sc_units = 1.0 : fact = 0.0 : a1% = 0% : a2% = 0%
      sc% = 0%
      fact = 3.25                     /* Set Default!!           */
      sc_cus_sort$ = "99999"
      
/* CR2633 */
      if len(sc_part$) < 19% and schema% = 1% then return
      buyout% = 0%
      if schema% = 2% and len(sc_part$) < 19% then gosub checkBuyout
      if schema% = 2% and len(sc_part$) < 19% and buyout% = 0% then return
      if buyout% = 1% then goto buyout_cont   /*  calc_height  */
      
      str(readkey$,1%,9%)   = "SYS CODES"
      str(readkey$,10%,15%) = str(sc_part$,1%,3%)

      read #6,key = readkey$,using L02500 , desc$, eod goto calc_fact
        sc_cus_sort$ = str(desc$,11%,5%)   /* Customer Sort Code */
        convert str(desc$,21%,5%) to fact, data goto calc_fact

    calc_fact                                /* Convert Width      */
      convert str(sc_part$,13%,4%) to a1%, data goto calc_width

    calc_width                               /* Convert Height     */
      convert str(sc_part$,17%,3%) to a2%, data goto calc_height

    calc_height                              /* Always Round Up    */
                                        /* UNIT_I% = United Inches */
                                        /* Is this TSO, BSO, FGO   */
                                        /* If so take half height  */
REM IF STR(SC_PART$,16%,1%) <> "0" THEN A1% = A1% + 10%
REM IF STR(SC_PART$,19%,1%) <> "0" THEN A2% = A2% + 10%

      a1% = a1% + 10%
      a2% = a2% + 10%

REM UNIT_I%  = INT(A1%/10) + INT(A2%/10)

      convert str(sc_part$,11%,1%) to sc%, data goto no_screen
    no_screen
      if sc% = 4% or sc% = 5% then a2% = round(a2% / 2%, 0)
      if sc% = 6% or sc% = 7% then a2% = round(a2% / 2%, 0)
/* double if 9 for bay/bow */
      if str(sc_part$,1%,1%) = "9" then a2% = round(a2% * 2%, 0)
      unit_i% = int(a1%/10) * int(a2%/10)
    
    buyout_cont      /* CR2633 */
      gosub lookup_mull                 /* Is this Wood Surround */
        if cubic_mull% = 1% then fact = fact + wd_sze
        cubic_mull% = 0%
        mull% = 0%
        fact     = round(fact,2)        /* Calc Line Item Units    */
REM SC_UNITS = ROUND( ((FACT * UNIT_I%)/116.0) * SC_TQTY%,2)
                                   /* Divide by 144 to turn inches */
                                   /* back to feet                 */
      sc_units = round( ((fact * unit_i%)/1728.0) * sc_tqty%,2)
        if sc_units = 0.00 then sc_units = 1.00
      or_units = round( or_units + sc_units, 2) /* Tot S.O. Units*/
    return
    
/* CR2633 */
        checkBuyout
           a1% = 0% : a2% = 0%
           sc% = 0%
           if str(sc_part$,1%,3%) <> "JJJ" then return
           
           widthes, heightes = 0.00 
           get str(bcksubpt_rec$,223%,8%) using L62050, widthes 
           get str(bcksubpt_rec$,231%,8%) using L62050, heightes 
L62050:         FMT PD(14,4)  

           if widthes <= 0.00 or heightes <= 0 then return
           buyout% = 1%
           a1% = int(widthes) + 1% 
           a2% = int(heightes) + 1%
           unit_i% = int(a1%) * int(a2%)
        return
      
    so_add
      x% = pos("0123456789ABCDEFGHIJKLMNOPQRSTUVWXY" = sc_grp$)
      if x% = 0% then sc_grp$ = "Z"
                                     /* (EWD001) - End of Mod      */
      sc_key$ = " "
      str(sc_key$,1%,8%) = sc_so$  /* APCPLNSC - Line Item Update*/
      str(sc_key$,9%,2%) = sc_line$
      read #3,hold,key = sc_key$, eod goto line_update

        delete #3                 /* Delete Ln Item, If it Exist*/
    line_update
      put #3, using L02230   ,         /* APCPLNSC-Line Item Update  */~
          sc_dte$,                     /* Delivery/Production Date   */~
          sc_load$,                    /* Sched Load No. Def ='99999'*/~
          sc_drop_seq$,                /* Sched Drop Seq. P.O./S.O.  */~
          sc_drop$,                    /* Customer Drop No. Def='00  */~
          sc_cus_sort$,                /* Customer Product Sort Seq. */~
          sc_so$,                      /* Customer S.O. Number       */~
          sc_line$,                    /* S.O. Line Item No.         */~
          sc_part$,                    /* MFG Part Number            */~
          sc_cuscode$,                 /* Customer Code              */~
          sc_tqty%,                    /* Total Line Item Quantity   */~
          sc_mqty%,                    /* Total Make Qty Line Item   */~
          sc_pqty%,                    /* Tot Pull Qty Ln Itm-Finish */~
          sc_pqty1%,                   /* Tot Pull Qty Ln Itm-Mulling*/~
          sc_price,                    /* Total Line Item Price(Net) */~
          sc_filer$,                   /* 5 byte filler CR3063       */~
          sc_dept$,                    /* Primary dept CR3063        */~
          sc_units,                    /* Total Line Item Units      */~
          sc_txt$,                     /* Line Item Text Id          */~
          sc_inv$,                     /* Inventory Updated (Y or N) */~
          sc_pload$,                   /* Parent Load No. Def='99999'*/~
          sc_st$,                      /* Sched Line Item Status Code*/~
          sc_ldte$,                    /* Sched Line Item Status Date*/~
          sc_special$,                 /* Special Product Flags      */~
          sc_grp$                      /* (EWD001) Wood S/F Group Cod*/

/* CR3063       sc_cost,                  Total Line Item Cost       */

      write #3, eod goto line_err

      err% = 0%
      for n% = 1% to 10%
        if str(sc_special$,n%,1%) = "Y" then str(or_special$,n%,1%) = "Y"
      next n%
    return
    line_err
      serr% = 14%                 /* Unable to Update (APCPLNSC) */
    return

    so_update
      read #2,hold,key 4% = sc_so$, using L02480 , or_rec$,        ~
                                              eod goto so_update_hdr
        delete #2
    so_update_hdr
      or_sls$ = str(or_rec$,86%,4%)         /* (EWD002) Salesman */
      put str(or_rec$,99%,28%),using L03840 , or_units, or_value,  ~
                                          or_cost, or_mak%, or_pul%
L03840:   FMT 3*PD(14,4), 2*BI(2)
      str(or_rec$,160%,10%) = or_special$
      put #2, using L02480 , or_rec$
      write #2, eod goto so_err
      err% = 0%
    return
    so_err
      serr% = 16%                 /* Unable To Update (APCPLNOR) */
    return

    check_pull_on                         /* No Pulls for Scrap or */
      if a_cell% = 1% then return
      pull_flag$ = "N"                   /* Salvage Product.      */
      if or_hows$ = "30" or or_hows$ = "31" then return
      if str(sc_cuscode$,1%,6%) = "NO0081" then return

      init(" ") invmastr_key$
      str(invmastr_key$, 1%,25%) = sc_part$
      str(invmastr_key$,26%,20%) = subp$
      read #11,key = invmastr_key$, using L04000 , pull_flag$, eod goto L04010

        if pull_flag$ = " " then pull_flag$ = "N"
L04000:   FMT POS(606), CH(1)     /* Check to see if Active      */
L04010:
    return                          /* Blank Same as 'Y' (YES)     */
                                    /* Make Blank = 'N' (AWD029) */

    calc_on_hand                    /* Current Available on System */
REM GOSUB CHECK_SPECIAL_SHAPES_STOCK      /* (AWD025)  */
REM IF SHAPES% = 0% AND FRAM% = 0% THEN GOTO CALC_NO_SHAPES
REM SAV_PART$ = SC_PART$
REM SC_PART$  = SPEC_PART$
REM CALC_NO_SHAPES                  /* (EWD011)         */
      gosub check_ne_product
      a_cell% = 0%
      gosub check_a_cell
      if a_cell% = 0% then goto check_no_a_cell
        sav_part$ = sc_part$
        sav_subp$ = subp$
        sc_part$  = spec_part$
        subp$     = spec_subp$
    check_no_a_cell
       if ne_pull% = 1% then goto calc_pull_all
       if ntx_pull% = 1% then goto calc_pull_all
       if thermal_pull% = 1% then goto calc_pull_all
       gosub check_pull_on
        if pull_flag$ = "N" then goto calc_no_pulls
       gosub calc_onhand                     /* Check Inventory  */
        if on_hand% = 0% then goto calc_no_pulls

       call "APCPULSB" (0%, sc_part$, subp$, " ", " ", " ", " ", ~
                        " ", pull%, 0%, #7,xerr% ) /* APCPULLS    */
       if xerr% <> 0% then goto calc_no_pulls
          on_hand% = on_hand% - pull%             /* Qty On-Hand */
          if on_hand% < 1% then goto calc_no_pulls
* CALC_PULL_STOCK                            /* NOT APPLICABLE   */
       if on_hand% >= sc_tqty% then goto calc_pull_all
       sc_pqty% = on_hand%                /* Total Greater    */
       sc_mqty% = sc_tqty% - sc_pqty%     /* than Available   */
    goto calc_update                         /* Partial Pull     */
    calc_pull_all
      sc_pqty% = sc_tqty%                   /* Pull (All) From  */
      sc_mqty% = 0%                         /* Inv.,Make None   */
    calc_update
      gosub write_pull
    goto calc_pull_exit
    calc_no_pulls
      on_hand%  = 0%
      sc_mqty%  = sc_tqty%
      sc_pqty%  = 0%
      sc_pqty1% = 0%
                                               /* (EWD011)         */
REM IF SHAPES% = 0% AND FRAM% = 0% THEN RETURN
      if shape% <> 0% or fram% <> 0% or a_cell% <> 0% then gosub reset_part
    return

    reset_part
      shapes% = 0% : fram% = 0%
      sc_part$ = sav_part$
    return


    calc_pull_exit
      if ne_pull% = 0% then return
      if ntx_pull% = 0% and thermal_pull% = 0% then return
      sc_pqty1% = sc_pqty%            /* stock where applicable*/
      sc_pqty%  = 0%

REM IF SHAPES% = 0% AND FRAM% = 0% THEN RETURN
REM IF NTX_PULL% = 0% THEN RETURN          /* Set the Pull Quantity */
REM SC_PART$  = SAV_PART$                 /* FOR 'SPECAIL SHAPES'  */

    return

    calc_onhand                           /* Obtain from (HNYQUAN) */
      init(" ") hny_key$ : on_hand% = 0%  /* the Physical On-Hand  */
      str(hny_key$, 1%,25%) = sc_part$    /* Quantity in Inventory */
      str(hny_key$,26%,20%) = subp$
    calc_onhand_nxt
      read #8,key > hny_key$, using L04530, hny_key$, onhand,        ~
                                            eod goto calc_onhand_exit
L04530:        FMT POS(17), CH(64), POS(89), PD(14,4)
      if str(hny_key$, 1%,45%) <> str(sc_part$,,25) & str(subp$,,20)~
                                               then goto calc_onhand_exit
REM IF STR(HNY_KEY$,26%,20%) <> SUBP$ THEN GOTO CALC_ONHAND_EXIT
      if str(hny_key$,46%, 3%) <> "300" then goto calc_onhand_nxt
        on_hand% = on_hand% + int(onhand)
    goto calc_onhand_nxt
    calc_onhand_exit
    return

    write_pull                               /*ON_HAND% = The Total*/
      if ne_pull% = 1% then return            /* (AWD025) */
      if ntx_pull% = 1% then return           /* (AWD043) */
      if thermal_pull% = 1% then return       /* (AWD047) */
      if sc_pqty% = 0% then return           /*Avail at Time of Pul*/
      call "APCPULSB" (1%,                /* Selection Add      */~
                       sc_part$,          /* Part Number        */~
/*PAR000*/             subp$,             /* Subpart number     */~
                       sc_cuscode$,       /* Customer Code      */~
                       sc_so$,            /* Sales Order        */~
                       sc_line$,          /* S.O. Line Item No  */~
                       sc_ldte$,          /* Date Created       */~
                       sc_load$,          /* Load Number        */~
                       sc_pqty%,          /* Pull from Stock Qty*/~
                       on_hand%,          /* Current On Hand Qty*/~
                       #7,                /* (INVPULLS) File    */~
                       xerr% )            /* 0% = OK, 1% = Error*/
      if xerr% = 0% then return
        serr% = 12%                      /* Error Adding Pull */
        sc_pqty% = 0% : sc_pqty1% = 0%
        sc_mqty% = sc_tqty%
    return

    delete_pull                              /* Change made to S.O.*/
         if stk_flag$ = "Y" then return
* PAR000
         call "APCPULSB" (2%, " ", " ",  " ", sc_so$, sc_line$, " ",  ~
                              " ", 0%, 0%, #7, xerr% )
    return

    del_sched
      str(sc_key$,1%,8%) = sc_so$         /* Clear Entries in the  */
    del_sched_nxt                         /* Line Item Sched. File */
      read #3,hold,key > sc_key$, using L04910 , sc_key$,          ~
                                              eod goto del_sched_done
L04910:         FMT POS(24), CH(10)
       if str(sc_key$,1%,8%) <> sc_so$ then goto del_sched_done
         sc_line$ = str(sc_key$,9%,2%)

         delete #3

         gosub delete_pull
         if opt% <> 9% then goto L04950
           sc_line% = 0%               /* Need sc_so$, lk_ln$  */
                                       /* or_hows$ & sc_cuscode$*/
           convert sc_line$ to sc_line%, data goto L04950

           convert sc_line% to lk_ln$, pic(###)
         gosub update_marketing

L04950:
    goto del_sched_nxt
    del_sched_done
      init(" ") sc_key$, sd_key$       /* Clear Entries in the  */
      str(sd_key$,1%,8%) = sc_so$      /* APCPLNSD-Sched Detail */
    del_detail_nxt
      read #10,hold,key > sd_key$, using L05030 , sd_key$,         ~
                                      eod goto del_detail_done
L05030:    FMT CH(23)
      if str(sd_key$,1%,8%) <> sc_so$ then goto del_detail_done
        delete #10

    goto del_detail_nxt
    del_detail_done
      gosub del_ewdsched
      gosub delHldSched
    return

    del_ewdsched
      init(" ") sp_key2$
      str(sp_key2$,1%,9%)  = sc_cuscode$
      str(sp_key2$,10%,8%) = sc_so$
L05050:
      read #13,hold,key 1% > sp_key2$, using L05060, sp_rec$,   ~
                                     eod goto del_ewdsched_done
L05060:    FMT CH(128)
        sp_key2$ = str(sp_rec$,16%,23%)
        if str(sp_key2$,1%,9%) <> sc_cuscode$ then goto del_ewdsched_done
        if str(sp_key2$,10%,8%) <> sc_so$ then goto del_ewdsched_done
           delete #13

        goto L05050
    del_ewdsched_done
    return
/*(AWD037) */
    delHldsched
      init(" ") hd_key2$
      str(hd_key2$,1%,8%) = sc_so$
L05550:
      read #15,hold,key 2% > hd_key2$, using L05560, hd_rec$,   ~
                                     eod goto delHldschedDone
L05560:  FMT CH(256)

REM ~~ important only set key to first 10 so when read mult times
REM ~~ the other instances can be found.  the BARCODE is not a unique key
      hd_key2$ = str(hd_rec$,27%,10%)
      if str(hd_key2$,1%,8%) <> sc_so$ then goto delHldschedDone
         delete #15
      goto L05550
    delHldschedDone
    return
/*(AWD037\) */
*======================================================================
*   P R O D     A N A L Y S I S
*======================================================================

    prod_analysis                         /* Analysis of Product   */
      sc_special$ = "NNNNNNNNNN"          /* Associated with each  */
      init(" ") pl_key$                   /* S.O. Line Item.       */
      pl_model$ = str(sc_part$,1%,3%)
      sc_dept$ = "   "                    /* CR3063                */      

      gosub check_fab                   /* Analyize Prod Fab.    */
      sd_wd_sort$ = "9"

      str(pl_key$,1%,3%) = pl_model$
    prod_next                             /* APCPLNDP-Dept Master  */
      read #9,key 3% > pl_key$,using L05190 ,pl_key$,               ~
                                             eod goto prod_done
L05190:        FMT CH(15)
       if str(pl_key$,1%,3%) <> pl_model$ then goto prod_done
* Only the 'FABRICATION' PROCESS '01' IS ANALYIZED
       if str(pl_key$,14%,2%) <> pl_proc$ then goto prod_next
* Only Process 'DIRECT' DEPARTMENTS, SKIP INDIRECT DEPARTMENTS
        pl_dept% = 100%
        pl_dept$ = str(pl_key$,11%,3%)    /* Set Department Code   */
        convert pl_dept$ to pl_dept%, data goto L05270
L05270:
        if a_cell% = 1% and pl_dept$ = "052" then pl_dept$ = "053"
        if a_cell% = 1% and pl_dept$ = "050" then pl_dept$ = "053"

        gosub check_support
REM IF SUPP% <> 1% THEN PRIMARY_DEPT$ = PL_DEPT$
        if stk_flag$ = "Y" and pl_dept% = 95% then goto L05310

REM IF PL_DEPT% > 94% AND PL_DEPT% <> 101% AND PL_DEPT% <> 104% THEN GOTO PROD_NEXT
REM  IF PL_DEPT% = 102% THEN GOTO PROD_NEXT /*(CR982)*/

       if pl_dept% > 94% and pl_dept% < 101% then goto prod_next
/* CR2532 */
       if pl_dept% = 102% and pl_model$ <> "H02" and pl_model$ <> "038" ~
                          and pl_model$ <> "H03" and pl_model$ <> "080" ~
                                         then goto prod_next
       if buyout% = 1% and pl_dept% <> 101% then goto prod_next /* (CR1966) */         
       
       if schema% = 1% and pl_model$ = "H01" then skip_cross_ck
       if schema% = 1% and pl_model$ = "H02" then skip_cross_ck /*(CR982)*/
       if schema% = 1% and pl_model$ = "H03" then skip_cross_ck /* CR2532 */
       if schema% = 1% and pl_model$ = "998" then skip_cross_ck
       if schema% = 1% and pl_model$ = "995" then skip_cross_ck /* CR2532 */
       if schema% = 1% and pl_model$ = "034" then skip_cross_ck
       if schema% = 1% and pl_model$ = "038" then skip_cross_ck /*(CR982)*/
       if schema% = 1% and pl_model$ = "080" then skip_cross_ck /* CR2532 */

REM allow plygem buyout and brands to route to dept 101
       if buyout% = 1% and pl_dept% = 101% then goto skip_cross_ck /* (CR1966) */ 
REM Model JMA -> STYLE SD2, Model JVH -> Style SD2HP, Model JVS -> Style SD2O
       if plygem% = 1% and str(style$,01%,03%) = "SD2" and pl_dept% = 101% ~
                                        then goto skip_cross_ck /* (CR1966) */  
/*CR2521 model JXE */
       if plygem% = 1% and str(style$,01%,03%) = "SHI" and pl_dept% = 101% ~
                                        then goto skip_cross_ck                                       
       if cross_dock% = 0% and pl_dept% = 101% then goto prod_next
       if cross_dock% = 1% and pl_dept% <> 101% then goto prod_next
       if cross_dock_t% = 0% and pl_dept% = 103% then goto prod_next
       if cross_dock_t% = 1% and pl_dept% <> 103% then goto prod_next

skip_cross_ck:

REM IF MDL8900% = 1% AND PAINTED% = 0% AND PL_DEPT% = 104% THEN GOTO PROD_NEXT
REM IF MDL8900% = 1% AND PAINTED% = 0% THEN GOTO NOT_PAINTED
REM IF (MDL8900% = 1% AND BEG_SO$ = "0") AND PL_DEPT% <> 104% THEN GOTO PROD_NEXT
REM IF (MDL8900% = 1% AND BEG_SO$ <> "0") AND PL_DEPT% = 104% THEN GOTO PROD_NEXT
REM PAINTED% CAN BE 0,1,2,3,4,5,6,7,8
REM IF SCHEMA% = 2% THEN GOTO SKIPTXPAINTCHECK
REM IF SCHEMA% = 2% THEN GOTO CHECKTXPAINTCHECK

/* + CR3193 sales order 0 change to allow 1 */	
       if painted% <> 0% and cross_dock% = 1%  and                     ~
	      (beg_so$ = "0" or beg_so$ = "1") /* (CR1602) (CR3193) */     ~
                                               then goto skipPaintCheck
       if schema% = 2% then goto paintCheckTX
         if cross_dock_t% = 1% then goto skipPaintCheck   /* (AWD047) */
	 
         if (beg_so$ = "0" or beg_so$ = "1" or beg_so$ = "A")                 ~
             and painted% <> 0%                                               ~
             and pl_dept% <> 104% then goto prod_next
         if (beg_so$ <> "0" and beg_so$ <> "1") and                           ~
             painted% = 0% and pl_dept% = 104%                                ~
                                                  then goto prod_next
         if (beg_so$ <> "0" and beg_so$ <> "1" and beg_so$ <> "A")            ~
                              and painted% <> 0%                              ~
                              and pl_dept% = 104% then goto prod_next
         if painted% = 0% and pl_dept% = 104% then goto prod_next
         goto skipPaintCheck

paintCheckTX:
         if cross_dock_t% = 1% then goto skipPaintCheck
/* CR2603 */         
         if (beg_so$ = "0" or beg_so$ = "1" or beg_so$ = "B")           ~
		     and painted% <> 0%                                         ~
             and pl_dept$ = "001" and str(sc_part$,1%,3%) = "267"       ~
             and str(sc_part$,4%,1%) = "5"   then goto skipPaintCheck
         if (beg_so$ = "0" or beg_so$ = "1" or beg_so$ = "B")           ~
		     and painted% <> 0%                                         ~
             and pl_dept$ = "001" and str(sc_part$,1%,3%) = "S23"       ~
             and str(sc_part$,4%,1%) = "5"   then goto skipPaintCheck
         if (beg_so$ = "0" or beg_so$ = "1" or beg_so$ = "B")           ~
		     and painted% <> 0%                                         ~
             and pl_dept$ = "001" and str(sc_part$,1%,3%) = "S33"       ~
             and str(sc_part$,4%,1%) = "5"   then goto skipPaintCheck
/* CR2601 */
         if (beg_so$ = "0" or beg_so$ = "1" or beg_so$ = "B")           ~
		     and painted% <> 0%                                         ~
             and pl_dept$ = "001" and str(sc_part$,1%,3%) = "312"       ~ 
             and str(sc_part$,4%,1%) = "P"    then goto skipPaintCheck
         if (beg_so$ = "0" or beg_so$ = "1" or beg_so$ = "B")           ~
		     and painted% <> 0%                                         ~
             and pl_dept$ = "001" and str(sc_part$,1%,3%) = "332"       ~
             and str(sc_part$,4%,1%) = "P"    then goto skipPaintCheck            
/* CR2549 */
         if (beg_so$ = "0" or beg_so$ = "1" or beg_so$ = "B")           ~
             and painted% <> 0%                                         ~
             and pl_dept$ = "001" and str(sc_part$,1%,3%) = "312"       ~ 
             and str(sc_part$,4%,1%) = "5"    then goto skipPaintCheck
         if (beg_so$ = "0" or beg_so$ = "1" or beg_so$ = "B")           ~
		     and painted% <> 0%                                         ~
             and pl_dept$ = "001" and str(sc_part$,1%,3%) = "313"       ~
             and str(sc_part$,4%,1%) = "5"    then goto skipPaintCheck 
             
         if (beg_so$ = "0" or beg_so$ = "1" or beg_so$ = "B")           ~
		                     and painted% <> 0%                         ~
                             and pl_dept% <> 104% then goto prod_next
         if (beg_so$ <> "0" and beg_so$ <> "1")                         ~
		                     and painted% = 0% and pl_dept% = 104%      ~
                                                  then goto prod_next
         if (beg_so$ <> "0" and beg_so$ <> "1" and beg_so$ <> "B")      ~
		                      and painted% <> 0%                        ~
                              and pl_dept% = 104% then goto prod_next
         if painted% = 0% and pl_dept% = 104% then goto prod_next
/* - CR3193 */
/* CR2603 */       
         if (str(sc_part$,1%,3%) = "267" or str(sc_part$,1%,3%) = "219")   ~
             and str(sc_part$,4%,1%) = "5"                                 ~
             and (pl_dept$ = "001" or pl_dept$ = "000") and beg_so$ = "D"  ~
                 then goto prod_next
         if (str(sc_part$,1%,3%) = "S23" or str(sc_part$,1%,3%) = "067")   ~
             and str(sc_part$,4%,1%) = "5"                                 ~
             and (pl_dept$ = "001" or pl_dept$ = "000") and beg_so$ = "D"  ~
                 then goto prod_next
         if (str(sc_part$,1%,3%) = "S33" or str(sc_part$,1%,3%) = "068")   ~
             and str(sc_part$,4%,1%) = "5"                                 ~
             and (pl_dept$ = "001" or pl_dept$ = "000") and beg_so$ = "D"  ~
                 then goto prod_next
/* CR2601 */
         if (str(sc_part$,1%,3%) = "312" or str(sc_part$,1%,3%) = "351")   ~
             and str(sc_part$,4%,1%) = "P"                                 ~
             and (pl_dept$ = "001" or pl_dept$ = "000") and beg_so$ = "D"  ~
                  then goto prod_next
         if (str(sc_part$,1%,3%) = "332" or str(sc_part$,1%,3%) = "361")   ~
            and str(sc_part$,4%,1%) = "P"                                  ~
            and (pl_dept$ = "001" or pl_dept$ = "000") and beg_so$ = "D"   ~
                  then goto prod_next
/* CR2549 */
         if (str(sc_part$,1%,3%) = "312" or str(sc_part$,1%,3%) = "313")   ~
             and str(sc_part$,4%,1%) = "5"                                 ~
             and (pl_dept$ = "001" or pl_dept$ = "000") and beg_so$ = "D"  ~
                  then goto prod_next
         if (str(sc_part$,1%,3%) = "351" or str(sc_part$,1%,3%) = "358"    ~
              or  str(sc_part$,1%,3%) = "390")                             ~
             and str(sc_part$,4%,1%) = "5"                                 ~
             and (pl_dept$ = "001" or pl_dept$ = "000") and beg_so$ = "D"  ~
                  then goto prod_next
                  
         goto skipPaintCheck

skipPaintCheck:
/* (AWD027) - begin */
       if pl_dept$ <> "044" and pl_dept$ <> "054" then goto NOT_WOOD
REM IF SPECIALMULL$ = "C" AND PL_DEPT$ = "044" THEN PROD_NEXT
REM IF SPECIALMULL$ = "C" AND PL_DEPT$ = "054" THEN PROD_NEXT
REM IF STR(SUBP$,8,1) = "1" AND PL_DEPT$ = "044" THEN PROD_NEXT
REM IF STR(SUBP$,8,1) = "1" AND PL_DEPT$ = "054" THEN PROD_NEXT
       if schema% = 2% and wood_scrpt$ <> "00" then goto FIN_REMOV_WOOD
       if removnail% = 0% then goto FIN_REMOV_WOOD /*+ (CR2143) prod to wood*/
                                                   /*surround to fin removal*/
          if str(sc_part$,12%,1%) = "1" then goto FIN_REMOV_WOOD /* No Fin */
          if str(sc_part$,12%,1%) = "2" then goto FIN_REMOV_WOOD /* No Fin */
          if str(sc_part$,12%,1%) = "5" then goto FIN_REMOV_WOOD /* No Fin */
          goto prod_next
          
FIN_REMOV_WOOD:                                                   /* - (CR2143)  */
       if specialmull$ = "C" then goto prod_next    
       if str(subp$,8,1) = "1" then goto prod_next
       if mdl8900% = 0% then goto NOT_WOOD
         if str(sc_part$,12%,1%) = "3" then goto prod_next
         if str(sc_part$,12%,1%) = "4" then goto prod_next
         if str(sc_part$,12%,1%) = "6" then goto prod_next
         if str(sc_part$,12%,1%) = "M" then goto prod_next
         if str(sc_part$,12%,1%) = "O" then goto prod_next

NOT_WOOD:
       gosub isDoor
        if door% = 0% then goto skip_Hardware_Check
       if str(subp$,4,1) = "4" and pl_dept$ <> "020" then prod_next

skip_Hardware_Check:

REM IF (SPECIALMULL$ <> "C" AND STR(SUBP$,8,1) <> "1") AND PL_DEPT$ = "074" THEN PROD_NEXT
       if pl_dept$ <> "074" then goto not_dept_074
         if str(subp$,8,1) = "1" then goto dept_074_prod
         if specialmull$ = "C"   then goto dept_074_prod
REM CHECK FOR LOCK CODES WITH FIN; OPTIONAL FIN IS APPLIED IN THE SDL DEPT
REM PAINTED ORDERS WITH FIN SHOULD NOT BE ROUTED TO SDL DEPT
REM IF MDL8900% = 1% AND BEG_SO$ = "0" THEN GOTO PROD_NEXT

       if mdl8900% = 0% then goto prod_next
/* CR3193 */	   
       if painted% = 1% and (beg_so$ = "0" or beg_so$ = "1") ~
	         then goto prod_next
         if str(sc_part$,12%,1%) = "3" then goto dept_074_prod
         if str(sc_part$,12%,1%) = "4" then goto dept_074_prod
         if str(sc_part$,12%,1%) = "6" then goto dept_074_prod
         if str(sc_part$,12%,1%) = "M" then goto dept_074_prod
         if str(sc_part$,12%,1%) = "O" then goto dept_074_prod
       goto prod_next
not_dept_074:
dept_074_prod:
/* (\AWD041)  */

L05310:
      gosub check_units                 /* Check Special Dept'S .*/
      if chk_unit$ = "00" then goto prod_next /* Model, Unit,    */
                                              /* Dept. Not/Applic*/
      if chk_unit$ <> str(pl_key$,4%,2%) then goto prod_next
                                                /* Not Correct Unit*/
                                                /* Value for Dept  */
      get #9, using L05380 , pl_units, pl_seq%  /* Get Units/HR  */
L05380:      FMT XX(22), PD(14,4), XX(1), BI(1)   /* and Prod Seq. */

      if pl_seq% = 0% then pl_seq% = 255%     /* Set Default End  */

REM IF INLINE% = 1% AND PL_DEPT$ = "044" THEN GOTO PROD_NEXT
REM IF INLINE% = 0% AND PL_DEPT$ = "054" THEN GOTO PROD_NEXT

      sd_wd_sort$ = "9"                 /* (AWD023) - Default */
       if supp% = 0% then sd_wd_sort$ = "0"        /* (AWD023) */
       if supp% = 1% and pl_dept$ = "102" and pl_model$ = "H02"   ~
                          then sd_wd_sort$ = "0"        /* (CR982) */
       if supp% = 1% and pl_dept$ = "102" and pl_model$ = "H03"   ~
                          then sd_wd_sort$ = "0"        /* CR2532 */
       if supp% <> 0% and pl_dept$ = "044" then sd_wd_sort$ = "1"
       if supp% <> 0% and pl_dept$ = "054" then sd_wd_sort$ = "1"
       if supp% <> 0% and pl_dept$ = "074" then sd_wd_sort$ = "1"
       
      if supp% <> 1% then primary_dept$ = pl_dept$   /* (CR2244) */

      str(sd_key$,1% ,8%)   = sc_so$              /* S.O. No. */
      str(sd_key$,9% ,2%)   = sc_line$            /* Line Item*/
      str(sd_key$,11%,1%)   = sd_wd_sort$         /* Plan sort*/
      str(sd_key$,12%,3%)   = pl_dept$            /* Dept.Code*/
      str(sd_key$,15%,2%)   = str(pl_key$,14%,2%) /* Process  */
      str(sd_key$,17%,2%)   = str(pl_key$,9%,2%)  /* Shift    */
      str(sd_key$,19%,3%)   = pl_model$           /* Model    */
      str(sd_key$,22%,2%)   = str(pl_key$,4%,2%)  /* Unit Code*/

      read #10,hold,key = sd_key$, eod goto prod_add
          delete #10                       /* Delete if Exists   */
    prod_add                                 /* APCPLNSD- Sched DTL*/
      if pl_dept$ = "021" then gosub sample_display

      sd_units = round(1.0 / pl_units, 4)  /* Convert to (MHPU), */
                                             /* Man Hours per Unit */
      put #10, using L05530 , sd_key$, sd_units, pl_seq%, " "
L05530:       FMT CH(23), PD(14,4), BI(1), CH(1)

      write #10, eod goto prod_done    /* Only One (1) Entry Per */
/* CR3063 Update primary dept or 104 in apcplnsc*/
      if str(sd_key$, 11%, 1%) = "0"  or  ~
         pl_dept$ = "104"             then sc_dept$ = pl_dept$
      
      goto prod_next                   /* Department Per Shift   */
    prod_done
    return

    check_units
      chk_unit$ = "00"
      if pl_dept$ <> "044" then goto L05660   /* Wood Surround   */
        chk_unit$ = wood_scrpt$               /* Set W/S for Prod*/
      return                                  /* 0% or A0 to Z0  */

                                                /*  (EWD022) - BEG */
L05660:
      if pl_dept$ <> "054" then goto L05665   /* Wood Surround   */
                          /* (CR2143) not wood must have fin removal and TX*/
REM        IF MULL% = 0% AND REMOVNAIL% = 1% AND SCHEMA% = 2% THEN GOTO L05666    
        if wood_scrpt$ = "00" and removnail% = 1% ~
                                 and schema% = 2% then goto L05666    
        chk_unit$ = wood_scrpt$               /* Set W/S for Prod*/
      return                                  /* 0% or A0 to Z0  */
L05665:
      if pl_dept$ <> "074" then goto L05666   /* Wood Surround   */
REM        IF MULL% = 0% THEN GOTO L05666
        if wood_scrpt$ = "00" then goto L05666
         chk_unit$ = wood_scrpt$              /* Set W/S for Prod*/
      return                                  /* 0% or A0 to Z0  */

L05666:
      if pl_dept$ <> "011" then goto L05710   /* UPS Product     */
       if upmh$(23%) = "Y" then chk_unit$ = "23"
       if upmh$(15%) = "Y" and upmh$(23%) = "Y" then chk_unit$ = "15"
      return
L05710:
      if pl_dept$ <> "021" then L05790          /* Sample/Display  */
       if upmh$(3%) = "Y" then chk_unit$ = "03" /*Sample Prod  */
       if upmh$(5%) = "Y" then chk_unit$ = "05" /*Display Prod */
       if upmh$(15%) = "Y" and upmh$(3%) = "Y" then chk_unit$ = "15"
       if upmh$(15%) = "Y" and upmh$(5%) = "Y" then chk_unit$ = "15"
      return
L05790:
      if pl_dept$ <> "000" then goto L05860     /* Screen Only     */
REM IF STR(SC_PART$,1%,1%) <> "0" THEN RETURN
       if upmh$(17%) = "Y" then chk_unit$ = "17" /*Full Screen */
       if upmh$(19%) = "Y" then chk_unit$ = "19" /*Half Screen */
       if upmh$(21%) = "Y" then chk_unit$ = "21" /*With Screen */
       if upmh$(15%) = "Y" then chk_unit$ = "15" /*Screen Part */
      return
L05860:
      if pl_dept$ <> "001" then goto L05910     /* Screen Dept.    */
       if upmh$(17%) = "Y" then chk_unit$ = "17" /*Full Screen */
       if upmh$(19%) = "Y" then chk_unit$ = "19" /*Half Screen */
       if upmh$(21%) = "Y" then chk_unit$ = "21" /*With Screen */
      return
L05910:
      if pl_dept$ <> "056" then goto L05920     /* Patio Screen    */
       if upmh$(17%) = "Y" then chk_unit$ = "17" /*Full Screen */
       if upmh$(19%) = "Y" then chk_unit$ = "19" /*Half Screen */
       if upmh$(21%) = "Y" then chk_unit$ = "21" /*With Screen */
       if upmh$(15%) = "Y" then chk_unit$ = "15" /*Screen Part */
      return
L05920:
/* + (CR1966) */
       if thdstk% <> 1% then goto L05925
         chk_unit$ = "02"
         return
L05925:
/* - (CR1966) */
/* + (CR2763) */
       if pl_dept$ <> "076" then goto L05930    
         if schema% <> 2% then goto L05930                         /*  TX */
          if sp_route$ = "90000" then goto L05930   /* not Builder Direct */
           chk_unit$ = "16"
      return
L05930:
/* CR2883 dept 076 NC white only with Lowes SKU */
      if pl_dept$ <> "076" then goto L05935
       if schema% <> 1% then goto L05935
        if bck_user_entered$ = "ECT" then goto L05935
         if str(sku$,1%,2%) = "  " then goto L05935 
/* CR2802 black laminate */         
          if str(sc_part$,4%,1%) <> "2" and str(sc_part$,4%,1%) <> "4"  ~ 
              then goto L05935 
           if str(sc_cuscode$,1%,2%) <> "LO" then goto L05935
               gosub checkskuTable
               if sossku% = 1%  then goto L05935
/* Sash only skip */
              if str(sc_part$,11%,1%) = "4" or ~
                 str(sc_part$,11%,1%) = "5" or ~
                 str(sc_part$,11%,1%) = "6"  then goto L05935             
               chk_unit$ = "16"
      return
L05935:     
/* (CR2825) */
      if pl_dept$ <> "051" then goto L05940
         if qlty_dept_flag$ <> "1" then goto L05940 /* flag on admin header */
         chk_unit$ = "25"
      return
L05940:      
/* - (CR2763) */
      if pl_dept$ <> "101" and pl_dept$ <> "103" then goto L05990

      if mullonly% = 1% then chk_unit$ = "15"  /*Mull Part */
      if chk_unit$ = "15" then return
      if screenonl% = 0% then goto L05990

      if upmh$(15%) = "Y" then chk_unit$ = "15" /*Screen Part */
      if upmh$(17%) = "Y" then chk_unit$ = "17" /*Full Screen */
      if upmh$(19%) = "Y" then chk_unit$ = "19" /*Half Screen */
      if upmh$(21%) = "Y" then chk_unit$ = "21" /*With Screen */

      if chk_unit$ = "15" or chk_unit$ = "17" then return
      if chk_unit$ = "19" or chk_unit$ = "21" then return
                                              /* Applicable to all */
                                              /* other Departments */
L05990:
      if upmh$(15%) <> "Y" then goto L06030
       chk_unit$ = "15"
      return

L06030:
      if upmh$(09%) <> "Y" then goto L06035
       chk_unit$ = "09"
      return

L06035:
      if upmh$(11%) <> "Y" then goto L06040
       chk_unit$ = "11"
      return

L06040:
      if upmh$(13%) <> "Y" then goto L06045
        chk_unit$ = "13"
      return
/*(AWD045) */
L06045:
      if upmh$(14%) <> "Y" then goto L06050
        chk_unit$ = "14"
      return
L06050:
/*(\AWD045) */
      for i% = 1% to 16%                 /* All Other Departments*/
       if upmh$(i%) <> "Y" then goto L06080
        convert i% to chk_unit$, pic(00)   /* Set Applicable UPMH$ */
        return                       /* Code (1) thru (16)   */
L06080:
      next i%
    return

    check_fab                             /* Analyize Product   */
      init(" ") upmh$()                   /* Fabrication all Dep*/
      inline% = 0%
      if len(sc_part$) > 18 then goto check_fab_screen
L06100:
      upmh$(15%) = "Y"                 /* Set Flag for Parts */
      str(sc_special$,8%,1%) = "Y"
    goto L06350                        /* Check Samp,Disp,UPS*/
    check_fab_screen                         /*Check Screen Options*/
      p% = pos(sc$ = str(sc_part$,11%,1%))
      if p% = 1% then upmh$(19%)  = "Y"   /* Half Screen Option */
      if p% =12% then upmh$(19%)  = "Y"   /* Full Screen Option */
      if p% =14% then upmh$(19%)  = "Y"   /* Half Screen Option */
      if p% =17% then upmh$(19%)  = "Y"   /* Half Screen Option */
      if p% =19% then upmh$(19%)  = "Y"   /* Half Screen Option 'W'(CR2235) */
REM IF P% =19% THEN UPMH$(19%)  = "Y"   /* FULL SCREEN OPTION */
REM IF P% =11% THEN UPMH$(19%)  = "Y"   /* HALF SCREEN OPTION */


      if p% = 2% then upmh$(17%)  = "Y"   /* Full Screen Option */
      if p% =10% then upmh$(17%)  = "Y"   /* Half Screen Option */
      if p% =18% then upmh$(17%)  = "Y"   /* Half Screen Option 'X'(CR2235)*/
REM IF P% =11% THEN UPMH$(17%)  = "Y"   /* HALF SCREEN OPTION */
REM IF P% =16% THEN UPMH$(17%)  = "Y"   /* FULL SCREEN OPTION */

      if p% = 3% then upmh$(21%)  = "Y"   /* N/A Default With Sc*/
      if p% = 8% then upmh$(21%)  = "Y"   /* Screen Only Dept   */
      if p% = 9% then upmh$(21%)  = "Y"   /* With Screen Option */
REM IF P% =13% THEN UPMH$(21%)  = "Y"   /* WITH SCREEN OPTION */
REM IF P% =17% THEN UPMH$(21%)  = "Y"   /* WITH SCREEN OPTION */

      if p% = 13% then goto L06100        /* Screen Part (AWD034)*/
      if p% = 15% then goto L06100        /* Screen Part (AWD034)*/
      if p% = 16% then goto L06100        /* Screen Part (AWD034)*/
REM IF P% = 18% THEN GOTO L06100        /* SCREEN PART        */
REM IF P% = 19% THEN GOTO L06100        /* SCREEN PART        */

      if p% = 4% then upmh$(9%)   = "Y"   /* Top Sash Only      */
      if p% = 5% then upmh$(11%)  = "Y"   /* Bot Sash Only      */
      if p% = 6% then upmh$(13%)  = "Y"   /* Fixed Glass Only   */
      if p% = 7% then upmh$(14%)  = "Y"   /* Operat Glass Only (AWD045)  */
                                                /* Set Sash Flag       */
      if p% = 4% or p% = 5% or p% = 6% then str(sc_special$,10%,1%) = "Y"
      if p% = 7% then str(sc_special$,10%,1%) = "Y" /*(AWD045)*/
                                                /* SET 'SAMPLE' FLAGS  */
L06350:
/*CR1123*/
      is_sample$ = "N"
      if or_hows$ = "25" or or_hows$ = "27" then gosub sample_display
      if is_sample$ = "Y" then upmh$(03%) = "Y"

      if or_hows$ = "02" or or_hows$ = "04" or                 ~
         or_hows$ = "06" or or_hows$ = "22" then upmh$(03%) = "Y"
      if or_hows$ = "38" or or_hows$ = "39" or                 ~
         or_hows$ = "40" OR or_hows$ = "34" then upmh$(03%) = "Y"

      if upmh$(3%) = "Y" then str(sc_special$,5%,1%) = "Y"
                                                /* SET 'DISPLAY' FLAGS  */
      if or_hows$ = "03" or or_hows$ = "05" then upmh$(05%) = "Y"
      if upmh$(5%) = "Y" then str(sc_special$,6%,1%) = "Y"
                                                /* SET 'UPS' FLAGS      */
       if or_hows$ = "01" or or_hows$ = "02" or                 ~
          or_hows$ = "03" or or_hows$ = "11" then upmh$(23%) = "Y"
       if or_hows$ = "13" or or_hows$ = "27" or                 ~
          or_hows$ = "25" or or_hows$ = "35" then upmh$(23%) = "Y"
       if or_hows$ = "36" or or_hows$ = "37" or                 ~
          or_hows$ = "38" or or_hows$ = "39" then upmh$(23%) = "Y"
       if or_hows$ = "40" or or_hows$ = "41" or                 ~
          or_hows$ = "42" or or_hows$ = "43" then upmh$(23%) = "Y"
       if or_hows$ = "44" or or_hows$ = "45" or                ~
          or_hows$ = "46"                    then upmh$(23%) = "Y"

       if upmh$(23%) = "Y" then str(sc_special$,7%,1%) = "Y"
       if upmh$(15%) = "Y" then goto L06500  /* Skip for Parts     */
         gosub check_specials             /* Temp,Diam,Spec Etc.*/
         gosub lookup_mull                /* Wood Surround/Mull */
REM GOSUB CHECK_INLINE               /* (EWD018)           */
         gosub lookup_co_or               /* Cottage/Oriel      */
L06500:
       gosub check_flags                   /* Window Priority is */
    return                                 /* (1) thru (16)      */

    lookup_mull                              /* Flags A0 thru Z0   */
      mull% = 0% : wood_scrpt$ = "00" : cubic_mull% = 0%
      init(" ") readkey$, wood$, spec_part$, sav_part$
      if str(sc_part$,1%,1%) = "9" then return
      if len(sc_part$) < 22 then return
      if len(sc_part$) = 22 then wood$ = str(sc_part$,20%,3%)      ~
                            else wood$ = str(sc_part$,23%,3%)
      convert wood$ to wood%, data goto L06600  /* (EWD007)       */

      if wood% > 1% and wood% < 81% then return
                                                  /* (EWD007)       */
L06600:
      if wood$ = "000" then return
      str(readkey$,1%,9%) = "APC WOOD "
      str(readkey$,10%,3%) = wood$
      read #6,key = readkey$,using L02500 , desc$, eod goto lookup_mull_done

        wood_scrpt$ = str(wood$,1%,1%) & "0"  /* ?0 = Unit Code */
        str(sc_special$,4%,1%) = "Y"       /* Set Wood Surround */
         mull% = 1%                        /* Set Mull Flag     */
                                           /* Skip Cot/Or Wind  */
        gosub check_app_mull
        if len(sc_part$) <> 22 then return /* for Mull Pulls    */
          str(spec_part$,1%,19%) = str(sc_part$,1%,19%)
          str(spec_part$,9%,2%)  = "99"  /* Set Mull Stock Part*/
                                         /* Number for Pulls   */
    lookup_mull_done
      if mull% = 0% then return
      gosub check_stock_mulls
    return

    check_stock_mulls                        /* Not Applicable for */
      mull% = 0%                           /* Cottage/Oriel      */
      init(" ") readkey$, desc$            /* Windows            */
      str(readkey$,1%,9%) = "PLAN MULL"
      str(readkey$,10%,3%) = wood$

      read #6,key = readkey$, eod goto L06840
        mull% = 1%
L06840:
    return
                                                 /* (EWD011)          */
    check_app_mull                           /* (EWD017)          */
      cubic_mull% = 0%
      wd_sze = 4.00
      init(" ") readkey$, desc$
      str(readkey$,1%,9%) = "AWD WOOD "
      str(readkey$,10%,3%) = wood$

      read #6,key = readkey$, using L02500 , desc$,  eod goto no_app_mull

        convert str(desc$,25%,6%) to wd_sze, data goto invalid_size
invalid_size:

        cubic_mull% = 1%
    no_app_mull
    return

    checkskuTable
      init(" ") readkey$
      sossku% = 0%
      str(readkey$,1%,9%) = "SOS SKU"
      str(readkey$,10%,15%) = sku$

      read #6,key = readkey$, eod goto noSku
        sossku% = 1%

noSku: 
    return

    check_inline
      inline% = 0%
      init(" ") readkey$, desc$
      str(readkey$,1%,9%) = "PLANINLIN"
      str(readkey$,10%,3%) = pl_model$
      str(readkey$,13%,3%) = wood$

      read #6,key = readkey$, eod goto not_inline
        inline% = 1%
    not_inline
    return

    check_special_shapes_stock
      sp_part$ = sc_part$
      call "EWDSHAPE" (shapes%, fram%, sp_part$, spec_part$, #6)
    return

    check_a_cell
      sp_part$ = sc_part$
      sp_subp$ = subp$
      call "EWDACELL" (a_cell%, sp_part$, sp_subp$, spec_part$,  ~
                          spec_subp$, #6, #11)
    return

    lookup_co_or                             /* Cottage/Oriel      */
      init(" ") readkey$, desc$
      str(readkey$,1%,9%)   = "HINGE    "
      str(readkey$,10%,15%) = str(sc_part$,9%,2%)
      read #6,key = readkey$,using L02500 ,desc$,                    ~
                                         eod goto lookup_co_or_done
       if str(desc$,1%,2%) = "CO" or str(desc$,1%,2%) = "OR" then   ~
                            upmh$(7%) = "Y" /* Set Cot/Or Flag    */
       if upmh$(7%) = "Y" then str(sc_special$,9%,1%) = "Y"
    lookup_co_or_done
    return

    check_flags                              /* Only Set Flag (1)  */
      for i% = 2% to 16%                   /* When No other Flag */
        if upmh$(i%) <> "Y" then goto L07030 /* is Set Between     */
           upmh$(1%) = " "                 /* (2) and (16) then  */
        return                          /* Set Standard Window*/
L07030:
      next i%
      upmh$(1%) = "Y"                      /* Standard Window Flg*/
    return

    check_specials
      init(" ") readkey$, desc$
      str(readkey$,1%,9%)   = "PLAN TEMP"
      str(readkey$,10%,15%) = str(sc_part$,5%,2%)
      read #6,key = readkey$, eod goto check_spec_glass
         str(sc_special$,1%,1%) = "Y"    /* Set Tempered Flag    */
    check_spec_glass
      init(" ") readkey$, desc$
      str(readkey$,1%,9%)   = "PLANGLOUT"   /* Outside Purchase  */
      str(readkey$,10%,15%) = str(sc_part$,7%,2%)
      read #6,key = readkey$, eod goto check_spec_grid
         str(sc_special$,1%,1%) = "Y"    /* Set Tempered Flag    */

    check_spec_grid                        /* Check Diamond Grid   */
      if str(sc_part$,7%,2%) = "97" or str(sc_part$,7%,2%) = "98"  ~
                                   then str(sc_special$,2%,1%) = "Y"
                                           /* Special Liting       */
      if str(sc_part$,7%,2%) = "99" then                           ~
                                        str(sc_special$,3%,1%) = "Y"
                                           /* Special Glass        */
      if str(sc_part$,5%,2%) = "89" then                           ~
                                        str(sc_special$,3%,1%) = "Y"
/* (AWD040) */                                /* TSO Diamond Grid      */
      if str(sc_part$,5%,2%) = "TD" then                           ~
                                        str(sc_special$,3%,1%) = "Y"
    return

    schedule_stock
      or_units = 0.0 : or_value = 0.0 : or_cost  = 0.0
      or_mak%  = 0%  : or_pul%  = 0%  : ord_disc = 0.0
      for stk% = 1% to 99%
        if len(stk$(stk%)) < 19 then goto L07500
          init(" ") sc_txt$
          sc_tqty% = 0%  : sc_mqty% = 0%  : sc_pqty% = 0%
          sc_pqty1%= 0%  : sc_price = 0.0 : sc_cost  = 0.0
          sc_units = 0.0 : sc_line% = 0%
          sc_part$ = stk$(stk%)
          subp$    = stk_sub$(stk%)  /*PAR000*/
          convert stk% to sc_line$, pic(00)

          convert stk% to seq$, pic(###) /*PAR000*/

          convert qty$(stk%) to sc_mqty%, data goto L07430
L07430:
          sc_tqty% = sc_mqty%
          or_mak%  = or_mak% + sc_mqty%
          gosub calc_units
          gosub prod_analysis
          gosub so_add
          gosub bcksubpt_add /*PAR000*/
      next stk%
L07500:
      gosub so_update
    return
        
    sample_display                               /* (EWD007)         */
      ss% = 0%
      if len(sc_part$) < 20 then goto L08010   /* Quick Test       */
       convert str(sc_part$,20%,3%) to ss%, data goto L08000

      if ss% < 1% or ss% > 80% then goto L08000 /* Not Samp/Disp   */
                                                /*   (EWD015)      */
      if str(sc_part$,7%,2%) > "99" then goto L08000

/*CR1123 */

      goto L08020                              /* Code Found      */

L08000:
       convert str(sc_part$,23%,3%) to ss%, data goto L08010

       if ss% < 1% or ss% > 80% then goto L08010

       goto L08020
L08010:
    return
L08020:
       ss% = ss% + 30%
       convert ss% to ss$, pic(00)

       init(" ") readkey$, desc$
       str(readkey$,1%,9%) = "PLAN UNIT"
       str(readkey$,10%,2%) = ss$
       read #6,key = readkey$,using L02500 , desc$,                ~
                                        eod goto L08030
       is_sample$ = "Y"                       /* CR1123 */
       convert str(desc$,17%,4%) to pl_units, data goto L08030
       pl_units = round(pl_units, 2)
                                              /* load Units per manhour */
L08030:
    return                                /* (EWD007)-End of Changes*/  
    
    check_support                          /*  (EWD019)  - begin    */
      supp% = 0%
      init(" ") readkey$
      str(readkey$,1%,9%)   = "PLAN SUPP"
      str(readkey$,10%,15%) = pl_dept$
      read #6,key = readkey$, eod goto check_support_done
      supp% = 1%
    check_support_done
    return                                /*  (EWD019)  - begin    */

    lookup_subpart                            /* PAR000  */
      init(" ") bcksubpt_rec$, flds$(), info_flds$(), plygem_buyout$
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
                        err1%)         /* Error Code                 */

      if err1% <> 0% then                                          ~
            str(bcksubpt_rec$,48%,20%) = "0000000000000000000000000"

      if err1% = 0% then return

    return                                    /* PAR000 */

    bcksubpt_add
      init(" ") bcksubpt_rec$, flds$(), info_flds$()
      subpterr% = 0%
      flag$ = "0"              /* Call By Sales Order */
      pgm$  = "0"              /* Called By BCKUPDTE  */

      gosub set_bcksubpt
      call "AWDBKSUB" (flag$, pgm$, sc_so$, seq$,           ~
                       bcksubpt_rec$, flds$(), info_flds$(),~
                                          #63, subpterr%    )

    return

    set_bcksubpt
      str(bcksubpt_rec$,1%,8%) = sc_so$        /* Sales Order No    */
      str(bcksubpt_rec$,9%,3%) = seq$          /* Sequence Number   */
      str(bcksubpt_rec$,12%,8%) = "        "   /* No Invoice No Yet */
      str(bcksubpt_rec$,20%,3%) = "   "        /* Invoice Item      */
      str(bcksubpt_rec$,23%,25%) = sc_part$    /* Part Number       */
      str(bcksubpt_rec$,48%,20%) = subp$       /* Sub Part Items    */
      str(bcksubpt_rec$,68%,64%) = stk_desc$(stk%)  /* Part Desc    */
      str(bcksubpt_rec$,132%,7%) = "0000000"   /* Information Part  */
      str(bcksubpt_rec$,139%,2%) = "00"        /* Brand Information */
      str(bcksubpt_rec$,141%,118%) = " "        /* Filler            */
    return
                                             /* PAR000 - end */

    check_ne_product
      ne_pull% = 0%
      ntx_pull% = 0%   /* (AWD043) */
      cross_dock% = 0% /* (AWD043) */
      thermal_pull%, cross_dock_t% = 0% /* (AWD047) */
REM====================
REM  5-7
REM  X
REM  INVOICE, STD MFG & ACT MFG PLANT
REM  (PLANTINV), (PLANTSTD), (PLANTACT)
REM  INV_PLANT$              MFG_PLANT$
REM  1=WELCOME
REM  2=SHELTON
REM  3=THERMAL
REM  4=DALLAS
REM====================
      init(" ") mfg_plant$, inv_plant$
      mfg_plant$ = str(infp$,7%,1%)
      inv_plant$ = str(infp$,5%,1%)

      if mfg_plant$ = inv_plant$ then return
      if schema% <> 1% then goto not_nc_schema
        if inv_plant$= "1" and mfg_plant$ = "2" then ne_pull% = 1%
/* (AWD043) */
        if inv_plant$= "1" and mfg_plant$ = "4" then ntx_pull% = 1%
        if inv_plant$= "1" and mfg_plant$ = "3" then thermal_pull% = 1%

not_nc_schema

      if schema% <> 4% then goto not_devnc_schema
        if inv_plant$= "1" and mfg_plant$ = "2" then ne_pull% = 1%
/* (AWD043) */
        if inv_plant$= "1" and mfg_plant$ = "4" then ntx_pull% = 1%

        if inv_plant$= "1" and mfg_plant$ = "3" then thermal_pull% = 1%

not_devnc_schema

      if schema% <> 2% then goto not_ne_schema
        if inv_plant$= "2" and mfg_plant$ = "1" then ne_pull% = 1%
/* (AWD043) */
        if inv_plant$= "4" and mfg_plant$ = "1" then ntx_pull% = 1%

not_ne_schema
        if ntx_pull% = 1% then cross_dock% = 1%
        if thermal_pull% = 1% then cross_dock_t% = 1%  /* (AWD047) */
    return

/* (AWD036) */
    checkKanbanpt
      kanban% = 0%

      str(kanbanptkey1$,1%,8%)   = str(sc_part$,1%,8%)
      str(kanbanptkey1$,9%,4%)   = "0000"
      str(kanbanptkey1$,13%,12%) = str(sc_part$,13%,12%)
      str(kanbanptkey1$,26%,20%) = "00000000000000000000"
      str(kanbanptkey1$,26%,3%)  = str(subp$,1%,3%)

      read #14, key 1% = kanbanptkey1$, eod goto noKanban

      kanban% = 1%
    noKanban
    return
/* (AWD036) */
    check_screenonl
      screenonl% = 0%
      init(" ") readkey$
      str(readkey$,1%,9%)   = "SCREENONL"
REM STR(READKEY$,10%,15%) = STR(SC_PART$,1%,3%)
      str(readkey$,10%,15%) = model$

      read #6,key = readkey$, eod goto check_screenonlDone

      screenonl% = 1%
    check_screenonlDone
    return

    check_mullonly
      mullonly% = 0%
      init(" ") readkey$
      str(readkey$,1%,9%)   = "PLANMONLY"
REM STR(READKEY$,10%,15%) = STR(SC_PART$,1%,3%)
      str(readkey$,10%,15%) = model$

      read #6,key = readkey$, eod goto check_mullonlyDone
          mullonly% = 1%

    check_mullonlyDone
    return

    exit_sub
      err% = serr%                               /* Set for Errors */
    end


    check_painted
      painted% = 0%
      casement% = 0%
      sFlexScrn% = 0%
      mat painted = zer
      mat casement = zer
      mat sFlexScrn = zer
/* do not paint casement scrn - scrn on inside of window - scrn only model below */
      if str(sc_part$,11%,1%) = "6" then return  /*FGO no sash material*/
      if str(sc_part$,11%,1%) = "7" then return  /*OGO no sash material*/
/* (CR591) */
      search sCaseScrn$ = str(sc_part$,1%,3%) to casement()
      casement% = int(casement())
        if casement% <> 0% then return
        
/* +  (CR2264)  */
      search sFlexScrn$ = str(sc_part$,1%,3%) to sFlexScrn()
      sFlexScrn% = int(sFlexScrn())
        if sFlexScrn% <> 0% then return        
/* -  (CR2264)  */
REM IF SCHEMA% = 2% THEN GOTO SKIPSCREENONLY /*(CR1329)*/
      if schema% = 2% then goto TXPaint
      if screenonl% = 1% then goto checkScreenPaint
skipScreenOnly:
      search painted$ = str(sc_part$,4%,1%) to painted()
      painted% = int(painted())
    return
    
    checkScreenPaint
/* CR2522 remove P and add 5 */
REM      if str(sc_part$,4%,1%) = "P" then gosub checkBronze 
/* CR2599 NC not painted screens for door per this CR */ 
      if str(sc_part$,4%,1%) = "5" and mfg_plant$ <> "1"  ~
             then gosub check_screen_door 

      if painted% <> 0% then return

      search scrnPaint$ = str(sc_part$,4%,1%) to painted()
      painted% = int(painted())
    return
    checkBronze
      if str(series$,1%,4%) = "130 " then painted% = 1%
      if str(series$,1%,4%) = "150 " then painted% = 1%
      if str(series$,1%,4%) = "160 " then painted% = 1%
    return
/* (\AWD041)*/
/* CR2522 Validate by door screen model numbers - CR2599 not needed NC */
    check_screen_door
       if str(sc_part$,1%,3%) = "354" then goto procpaint
       if str(sc_part$,1%,3%) = "353" then goto procpaint 
       if str(sc_part$,1%,3%) = "363" then goto procpaint
       if str(sc_part$,1%,3%) = "396" then goto procpaint
       if str(sc_part$,1%,3%) = "394" then goto procpaint
       if str(sc_part$,1%,3%) = "392" then goto procpaint
       if str(sc_part$,1%,3%) = "390" then goto procpaint
       if str(sc_part$,1%,3%) = "361" then goto procpaint
       if str(sc_part$,1%,3%) = "351" then goto procpaint
       if str(sc_part$,1%,3%) = "368" then goto procpaint
       if str(sc_part$,1%,3%) = "358" then goto procpaint
       return
procpaint:
       painted% = 1%
       scrnpnt% = 1%
    return
/*(CR1329)+*/
    TXPaint
      if screenonl% = 1% then goto TXcheckScreenPaint

/* CR2549 TX doors no black paint */
      if (str(sc_part$,1%,3%) = "312" or str(sc_part$,1%,3%) = "313") and ~
          str(sc_part$,4%,1%) = "5" then goto TXNoBlackPaint
         
      search painted$ = str(sc_part$,4%,1%) to painted()
      painted% = int(painted())
      return
TXNoBlackPaint:
      painted% = 0
    return
    TXcheckScreenPaint
      if str(series$,1%,4%) = "130 " then return
      if str(series$,1%,4%) = "150 " then return
      if str(series$,1%,4%) = "160 " then return
/* CR2601 */
      if str(sc_part$,1%,3%) = "351" and str(sc_part$,4%,1%) = "P" then return
      if str(sc_part$,1%,3%) = "361" and str(sc_part$,4%,1%) = "P" then return
/* CR2603 */ 
      if str(sc_part$,1%,3%) = "219" and str(sc_part$,4%,1%) = "5" then return
      if str(sc_part$,1%,3%) = "067" and str(sc_part$,4%,1%) = "5" then return
      if str(sc_part$,1%,3%) = "068" and str(sc_part$,4%,1%) = "5" then return
/* CR2549 TX not black paint on some screen for doors */
      if str(sc_part$,1%,3%) = "351" and str(sc_part$,4%,1%) = "5" then return
      if str(sc_part$,1%,3%) = "358" and str(sc_part$,4%,1%) = "5" then return
      if str(sc_part$,1%,3%) = "390" and str(sc_part$,4%,1%) = "5" then return
/* CR3215 TX not black paint on screen for doors */	  
      if str(sc_part$,1%,3%) = "361" and str(sc_part$,4%,1%) = "5" then return                              
	  
      search scrnPaint$ = str(sc_part$,4%,1%) to painted()
      painted% = int(painted())

      if str(series$,1%,4%) = "361 " then painted% = 1%
    return
/*(CR1329)-*/
