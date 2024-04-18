      REM   *  Hidden PF Key 32  - Runs APCSCAN2, clock in - out utility*~
            *                                                           *~
            *  Program Name      - APCSCANN                             *~
            *  Creation Date     - 12/23/96                             *~
            *  Last Modified Date- 09/01/2016                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Last Mod By       - Christie Gregory                     *~
            *                                                           *~
            *  Description       - Scanning Utility used inconjunction  *~
            *                      with the new planning system.        *~
            *                                                           *~
            *  Special Notes     - (GL1) Used for Glass Reports & labels*~
            *                      (NGL)                                *~
            *                                                           *~
            *                      (OV?) Userid is for the Glass House  *~
            *                      (ON?) and can only run Selections    *~
            *                            (4) and (5).                   *~
            *                            Remake Reason Codes 26, 28, 30 *~
            *                            are reserved for the Glass     *~
            *                            House and put into a seperate  *~
            *                            remake glass file.             *~
            *                                                           *~
            *                      (SCN) Userid is for the Production   *~
            *                      (NSC) lines. They cannot run Select- *~
            *                            ion (4) Complete Glass.        *~
            *                                                           *~
            *                      (BYP) Userid is for the By-Pass Load-*~
            *                      (BYN) ing. It does not require Load  *~
            *                            number.                        *~
            *                                                           *~
            *                      (RCV) Receive Custom Glass           *~
            *                      (RCN)                                *~
            *                                                           *~
            *           Note     - Userid for Scanning is Required      *~
            *                                                           *~
            *  Code Tables Used  - (PLAN SHFT) - Shift Codes            *~
            *                      (PLAN DEPT) - Department Codes       *~
            *                      (PLAN PROC) - Planning Process Codes *~
            *                      (PLAN REMK) - Glass Remake Reason    *~
            *                                    Codes                  *~
            *                      (PLANGLASS) - Oven Id Code Table     *~
            *                                    for tracking. (EWD012) *~
            *                                                           *~
            *  Special Comments  - All Tables Excep (MODEL) can have    *~
            *                      Alphnumeric values.                  *~
            *                                                           *~
            *                      Uses Table Subroutine (APCPLN1B)     *~
            *                      for Displaying Code table Values.    *~
            *                                                           *~
            *                      Use New Subroutine to Calculate      *~
            *                      Scanned Units for Production Shift   *~
            *                      (APCPLC40).                          *~
            *                                                           *~
            *                      New Subroutine to ADJUST_DATE when   *~
            *                      units are Calculated for 3rd Shift.  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/11/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 02/19/96 ! Mod CALC_SCANNED Units PF(5) Key         ! RHH *~
            * 03/11/96 ! Mod for New File Structure with Part No, ! RHH *~
            *          !   with Sash, Part, Sample, and Wood      !     *~
            *          !   Surround Flags.                        !     *~
            * 08/26/96 ! Mod for Departments '044', '043', and    ! RHH *~
            *          !   '009', Set Staged Flag in "APCDATAC"   !     *~
            *          !   when Scanned Complete.                 !     *~
            * 08/26/96 ! Mod for Departments '044', '043', and    ! RHH *~
            *          !   '009', Set Staged Flag in "APCDATAC"   !     *~
            *          !   when Scanned Complete.                 !     *~
            * 01/15/97 ! Mod correct Staging and Loading Problems ! RHH *~
            * 01/30/97 ! Clean-up Scanning Errors, and Add RGA    ! RHH *~
            *          ! Selection.                               !     *~
            * 02/07/97 ! Mod for Glass and Glass Re-Make Scanning ! RHH *~
            *          !   Also New Routine CALC_GLASS            !     *~
            * 02/13/97 ! Mod to Count Sash's and Parts if they    ! RHH *~
            *          !   are Not a Zero Price. (Sale) Line No   !     *~
            *          !   6600                                   !     *~
            * 02/23/97 ! Mod to Support RGA Subs                  ! JBF *~
            * 04/22/97 ! Mod to Add (Load No) to the screen for   ! RHH *~
            *          !   Shipping. Only Applicable to Shipping. !     *~
            * 04/22/97 ! Mod to Update "ALL" product Scanned by   ! RHH *~
            *          !   by Wood Surround as Stagged Complete.  !     *~
            *          !   Changes at Line 4310 Approx.           !     *~
            * 04/22/97 ! Mod to Adjust the Production Date for    ! RHH *~
            *          !   3rd Shift when PF(5) is used.          !     *~
            * 05/01/97 ! Mod to Add a password to gain Access into! RHH *~
            *          !   Selection (4) for the Glass Oven.      !     *~
            * 06/05/97 ! Mod to Glass remake reason codes. No     ! RHH *~
            *          !   Codes over '20' are Valid.             !     *~
            * 08/21/97 ! Mod to Argument List for (APCPLC40)      ! RHH *~
            *          !   SC_LOAD$ = Load Default = "ALL  "      !     *~
            * 11/17/97 ! Mof for Upgrade to R6.04.03              ! RHH *~
            * 04/02/98 ! Y2K modifications                        ! ERN *~
            * 05/04/98 ! Mod for Glass Re-make Date/Time update   ! RHH *~
            *          !   Two (2) New Glass Re-make analysis subs!     *~
            *          !   (APCPLD45) and (APCPLE45). Change Glass!     *~
            *          !   update for Scheduled and Re-make glass !     *~
            *          !   All Mods flagged with (EWD0001)        !     *~
            *          !   User ID's (scn) and (gls) are used in  !     *~
            *          !   test criteria.                         !     *~
            * 05/11/98 ! Mod for new PF Key for the Printer       ! RHH *~
            *          !   At L05380 allow for Loading to override!     *~
            *          !   Production Scanning.(EWD001)           !     *~
            * 05/26/98 ! (EWD002) - Mod for New Loading (BYP) user! RHH *~
            *          !   Id for By-passes                       !     *~
            * 06/04/98 ! (EWD003) - Mod for making Userid required! RHH *~
            *          !   and test for new reason codes 26,28, 30!     *~
            *          !   which are reserved for the Glass House !     *~
            * 07/01/98 ! (EWD004) - Add Undelivered Goods Options ! RHH *~
            * 07/31/98 ! (EWD005) - New Glass Remake Analysis     !     *~
            *          !   File. Fixes for Tempered Glass and     !     *~
            *          !   Special Liting. (check_specials)       !     *~
            * 11/05/98 ! (EWD006) - Mod to ask for password, RGA  ! RHH *~
            *          !   and Undelivered Goods.                 !     *~
            * 11/23/98 ! (EWD007) - Mod to add Glass Search Prog  ! RHH *~
            *          !   EWDPLN64 to Scanning Program.          !     *~
            * 12/07/98 ! (EWD008) - Re-Make Clean-up              ! RHH *~
            * 01/06/99 ! (EWD009) - Update Completion Date, Time, ! RHH *~
            *          !   Userid when not a Re_make              ! RHH *~
            * 02/15/99 ! (EWD010) - Update (EWDPLNRK) file that   ! RHH *~
            *          !   Glass in Slot is Complete.             !     *~
            * 03/24/99 ! (EWD011) - Mods for Trauma Center        ! RHH *~
            * 04/16/99 ! (EWD012) - Mod for new OV? Glass Id      ! RHH *~
            *          !            mods to (APCPLD45) needed to  !     *~
            *          !            add shift code to arguments   !     *~
            *          !            New field in (APCPLNGR) remake!     *~
            *          !            file at Pos. (65%,1%) Oven Code!    *~
            * 06/08/99 ! (EWD013) - Mod to fix problem with Erron.! RHH *~
            *          !            glass showing during analysis.!     *~
            *          !            Completed glass Should not    !     *~
            *          !            show under scheduled glass for!     *~
            *          !            PF(8). Shifts (2) and (3) are !     *~
            *          !            turned off in Trauma Center   !     *~
            * 06/09/99 ! (EWD014) - Mod to set Up new flag to turn! RHH *~
            *          !            off the trauma center.        !     *~
            *          !            trauma_center$ = (Y)es or (N)o!     *~
            * 07/16/99 ! (EWD015) - Mod to set Up new Program for ! RHH *~
            *          !            printing single labels and    !     *~
            *          !            glass Re-Make Labels.         !     *~
            * 08/18/99 ! (EWD016) - Mod to replace 'OPENCHEK'     ! RHH *~
            *          !            with new 'EWDOPEN' Subroutine !     *~
            *          !            to improve HP speed.          !     *~
            * 12/16/99 ! (EWD017) - Mod for new reason code '40'  ! RHH *~
            *          !            Only valid for glass house    !     *~
            *          !            Also glass house will not be  !     *~
            *          !            able to use Production reason !     *~
            *          !            codes                         !     *~
            * 12/20/99 ! (EWD018) - Mod To glass for new 12 digit ! RHH *~
            *          !            Barcode. Glass Barcode (9) +  !     *~
            *          !            Re-Make Number (3) = 12       !     *~
            * 01/28/00 ! (EWD019) - Mod's for Glass Repair Station! RHH *~
            * 05/24/00 ! (EWD020) - Mod to loading to force 'Stop'!     *~
            * 11/20/00 ! (EWD021) - Mod to loading to force stop  ! CMG *~
            *          !            if not been to staging        !     *~
            * 04/23/01 ! (EWD022) - Mod to turn on hidden key 32  ! CMG *~
            *          !            to run APCSCAN2 program.      !     *~
            * 06/29/01 ! (EWD023) - Mod to turn on a key to allow ! CMG *~
            *          !            scanning of BACKORDERS.       !     *~
            * 10/18/01 ! (EWD024) - Mod to turn on Glass Detail   ! CMG *~
            *          !            Audit for Remakes (EWDPLNGT). !     *~
            * 06/24/02 ! (EWD025) - Mod to activate Liting Codes  ! CMG *~
            *          !            for Special Shapes            !     *~
            * 01/17/03 ! (EWD026) - Mod to automatically stage FGO! CMG *~
            * 03/12/03 ! (EWD027) Mod to allow numeric or alpha   ! CMG *~
            *          !          load numbers.                   !     *~
            * 06/17/03 ! (EWD028) - Mod to take out auto staging  ! CMG *~
            *          !            for Wood Surround and Fix Gls !     *~
            * 07/16/03 ! (AWD029) - Mod to Complete window based  ! RHH *~
            *          !            Production Department Product !     *~
            *          !            planned in.                   !     *~
            * 09/15/03 ! (AWD030) - Mod to Clean-up Code for Glass! RHH *~
            *          !            Remakes. No Longer any repair !     *~
            *          !            codes. 1 - 8 = Production,    !     *~
            *          !            11-18 = Glass department Only !     *~
            * 09/22/03 ! (AWD031) - Mod to Turn on Tempered Glass ! CMG *~
            *          !            Scanning.                     !     *~
            * 04/12/04 ! (AWD032) - Mod for Lean to give ability  ! CMG *~
            *          !            to glass house to scan in     !     *~
            *          !            remakes as production line    !     *~
            * 04/12/04 ! (AWD033) - Mod to scann glass panels     ! CMG *~
            *          !            received in production lines  !     *~
            * 06/01/04 ! (AWD034) - Mod to allow users to scan    ! CMG *~
            *          !            prairie grid.                 !     *~
            * 06/01/04 ! (AWD035) - Mod to allow new scan status  ! CMG *~
            *          !            '11' and '13' for wood surround!    *~
            * 06/24/04 ! (AWD036) - Mod to put APCPLN40 on option ! CMG *~
            *          !                 4 on scanning screen     !     *~
            * 08/12/04 ! (AWD037) - Mod to put a password for opt ! CMG *~
            *          !            2; wood receive               !     *~
            * 09/16/04 ! (AWD038) - Mod to make user 'RCV' same   ! CMG *~
            *          !            as 'OV'.                      !     *~
            * 09/24/04 ! (AWD039) - Mod for Special Shapes Remake ! RHH *~
            * 10/26/04 ! (AWD040) - Mods to try to figure out  why!     *~
            * 12/10/04 ! (AWD041) - Mods to add new in house rmk cd!CMG *~
            * 12/22/04 ! (AWD042) - Mods to the new grid code 59  ! CMG *~
            * 01/24/05 ! (AWD043) - Mods to add department on the ! CMG *~
            *          !        when scanning in a remake         !     *~
            * 07/28/05 ! (AWD044) - Mod to add drywall to wood lbl! CMG *~
            * 08/22/05 ! (AWD045) - mod to combine RF glass remake! CMG *~
            *          !      scanning into this scan program     !     *~
            * 09/19/05 ! (AWD046) - mod to add glass status code  ! CMG *~
            *          !      to screen when scanning remakes     !     *~
            * 09/21/05 ! (AWD047) - mod to add receiving glass to ! CMG *~
            *          !      rf scanning screens                 !     *~
            * 01/01/06 ! (PAR000) CR347 Mod for new sub part      ! CMG *~
            * 02/10/06 ! (AWD048) mod for special lits            ! CMG *~
            * 03/03/06 ! (PAR001) Mod for the new production label! RHH *~
            *          !    changes. Size Change 640 to 1024      !     *~
            * 03/23/06 ! (PAR002) Mod for NE Scanning             ! RHH *~
            *          !    New Id's NGL, NSC, BYN, ON1, ON2, ON3 !     *~
            *          !    RCN                                   !     *~
            * 04/10/06 ! (PAR003) Mod to scanning so that NE could! RHH *~
            *          !    print labels in Wood Surround.        !     *~
            * 05/12/06 ! (AWD049) fix dup status 12 record check  ! DES *~
            * 07/11/06 ! (AWD050) fix for cross dock mull 560 Series!CMG*~
            * 09/07/06 ! (AWD051) autocomplete dept 44/54 added   ! DES *~
            * 10/02/06 ! (AWD052) mod to add getcode for remake rsn!CMG *~
            * 10/31/06 ! (AWD053) mod to allow remake reasons up  ! CMG *~
            *          !      to 10                               !     *~
            * 05/03/07 ! (AWD054) mod for 18 mm                   ! CMG *~
            * 09/04/07 ! (AWD055) mod to display brand            ! CMG *~
            * 01/07/08 ! (AWD056) mod for bullnose casing wood lb ! CMG *~
            * 01/07/08 ! (AWD057) Mod for support dept 074        ! CMG *~
            *09/08/2008! (AWD058) Mod for baybow infor for oracle ! DES *~
            *02/16/2009! (AWD059) mod for new brand viewpoint     ! CMG *~
            *03/24/2009! (AWD060) mod for WS label                ! DES *~
            *03/31/2009! (AWD061) mod for receiving glass only    ! CMG *~
            *          !  receive remakes not original            !     *~
            *05/21/2009! (AWD062) mod for Florida Mull Clip       ! DES *~
            *03/13/2010! (AWD063) mod to allow patio to be scanned! CMG *~
            *          !           complete and remake gls        !     *~
            *03/13/2010! (AWD064) mod to prevent blank status inDT! CMG *~
            *06/03/2010! (AWD065) mod to prevent 5/32 or 3/16 gls ! CMG *~
            *          !        processing as stock gls           !     *~
            *06/12/2012! (AWD066) mod for tempered glass          ! CMG *~
            *12/30/2013! (AWD067) mod for new brand Norandex      ! MES *~
            *07/28/2014! (AWD068) mod to lookup series and style  ! CMG *~
            *04/23/2015! (IM8022) mod for glass remakes           ! CMG *~
            *04/06/2015!(SR73802) mod for New brand eXtreme       ! MES *~
            *09/01/2016! (CR456) DC Center status 15 RGA          ! CMG *~
            *01/17/2017!(SR79102) Remove codes of GlassRMk codes        *~
            *                       add rmk code for APCPLA45     ! MES *~
            *07/13/2017! (CR1017) Add Patio Hardware Display Info ! RDB *~
            *                     dept 20 23                      !     *~
            *10/04/2018! CR1716   Add dept 064 as support, see 054! RDB *~
            *01/30/2019! CR1911   New brand for display           ! RDB *~
            *08/27/2019! CR2180  TX Remake Now chg for barcodes   ! RDB *~
            *01/07/2019! CR2375  By Customer Code show brand as **! RDB *~
			*02/03/2023! CR3199  Add new Lowe's stock label print ! RDB *~
            *************************************************************


        dim hdr$47, scr$(15%)40,         /* ASKUSER Header             */~
            filename$8, key13$19,        /* Use with EWDOPEN - EWD016  */~
            trauma_center$1,             /* (EWD014) Sitch on/off      */~
            her$(20%)50,                 /* Error Text Display         */~
            rf_her$(40%)20,              /* RF Err Text Dis (AWD045)   */~
            scr_sel$1, scr_sel_d$30,     /* Screen Scanning Option     */~
            rep_id$(10%)3,               /* Valid Repair Id's  (EWD019)*/~
            rep_code$(30%)2,             /* Vaild Reason code's(EWD019)*/~
            scr_id$3, msg$(3%)79, hh$40, /* Scanning User Id           */~
            scr_id_d$30,                 /* User Id Name               */~
            scr_dept$3, scr_dept_d$30,   /* Product Line / Dept Code   */~
            sav_scr_dept$3,              /* Save Dept          (EWD025)*/~
            scr_shft$2, scr_shft_d$30,   /* Screen Shift Entry         */~
            scr_proc$2, scr_proc_d$30,   /* Product / Dept Process Code*/~
            scr_load$5, scr_load_d$30,   /* Production Load and Descrip*/~
            sc_load$5, ed_load$5,        /* Used by (APCPLC40)         */~
            readkey$24, desc$30,         /* GENERIC KEY                */~
            table$9, code$3,             /* TABLE LOOKUP NAME AND CODE */~
            pfkeys$40,                   /* PF KEYS                    */~
            xx$(7%)50,                   /* Screen Display area Text   */~
            gl$(7%)50,                   /* 'Glass' Message            */~
            ld$(7%)50,                   /* 'Load' Message             */~
            st$(7%)50,                   /* 'Stage' Message Test       */~
            pd$(7%)50,                   /* 'Prod' Message             */~
            ps$(7%)50,                   /* Scan 'COMP'lete Text Screen*/~
            ee$(7%)50,                   /* 'STOP' Message Error Text  */~
            rf_ee$(7%)20,                /* Stop for RF     (AWD045)   */~
            ad_rec$64, ad_rec1$64,       /* Prod Scanning Audit File   */~
            ad_time$8, ad_key$33,        /* Time Stamp                 */~
            ad_dept$3, ad_proc$2,        /* Aduit Dept and Process     */~
            dt_rec$256, dt_dept$3,       /* Production Detail          */~
            dt_key$23, sav_key$23,       /* (APCPLNDT)-Tracking File   */~
            dt_load$5,                   /* Scanning Load Number - Only*/~
            sav_load$5,                  /* Scanning Load Number - Only*/~
            ap$2,                        /* Save 'AM' or 'PM' of Time  */~
            jdate$7,                     /* Save Todays Julian Date    */~
            err$(40%)50,                 /* Defined Error Messages     */~
            err$1,                       /* Print Error Code   (PAR003)*/~
            rf_err$(40%)20,              /* RF Err Msg         (AWD045)*/~
            barcode$18,                  /* Scanned Barcode            */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dateout$8,                   /* Time Extract For Screen    */~
            inp_text$(5%)79,             /* Input Prompt Text          */~
            rf_inp_text$(3%)20,          /* RF input text      (AWD045)*/~
            fld$(3%)30,                  /* Field Text                 */~
            rf_fld$(4%)20,               /* RF Field Text      (AWD045)*/~
            errormsg$79,                 /* Error message              */~
            txrmkmsg$79,                 /* Authorization error for TX */~
            rf_errormsg$20,              /* RF Error Message   (AWD045)*/~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            rf_inpmessage$20,            /* Information Mess   (AWD045)*/~
            prevcode$18,                 /* Previous Bar Code Entered  */~
            lfac$(4%)1,                  /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            rf_pf$(3%)20,                /* RF PF Screen Literal (AWD045)*/~
            scrn_title$40, title$40,     /* Screen Description         */~
            wandchar$1,                  /* Wand Character - Scanner   */~
            userid$3,                    /* Current User Id            */~
            hold_dept$3,                 /* AWD051                     */~
/*CR2180*/  schema$8                     /* (PAR003) Schema Switch     */

        dim rlib$8,                      /* Run library for link call  */~
            rvol$6,                      /* Run volume for link call   */~
            run$8                        /* Program to Run (EWD007)    */

        dim                              /* (APCPLNGR) - File          */~
/*PAR000*/  rm_key$12, rm_rec$(4%)128,   /* Re-Make Key and Record     */~
            rm_bar$12, rm_key2$33,    /* Remake Glass Barcode  (EWD018)*/~
            rm_bar1$12,                  /* Remake Barcode     (AWD046)*/~
            rm_txt$25, sav_dte$6,        /* Glass Count Text           */~
            rm_reason$2, reason_d$30,    /* Glass Remake Reason Code   */~
            rm_rm_dte$6, rm_shft$4,      /* Date Re-make Scan (EWD001) */~
            rm2bar$15,                   /* Remake Now barcode CR2180  */~
            calc_time$8, calc_dte$6,     /* Use for Re-make Calc(EWD001*/~
            bar_wand$1, reason_wand$1,   /* Barcode Wand Characters    */~
            rm_part$25, rm_gls$2, rm_lit$2, /* Remake MFG (EWD005)     */~
/*PAR000*/  rm_subpart$20,               /* Sub part number            */~
            rm_rmk_flag$1,        /* Remake Flag 0=rmk 1= prod(SR79102)*/~
            glstype$20,                  /* (IM8022)                   */~
            trk$1, check_trk$1,          /* (EWD012) For ovens         */~
            sav_userid$3,                /* (EWD012) Temporary         */~
            rma_rec$64,                  /* (APCPLNGA) - (EWD005)      */~
                                         /* (APCPLNDT)                 */~
            dt_bar$18, dt_st$2,          /* Bar Code Value             */~
            dt_txt$4,text$(2%)70,        /* Detail Text Id             */~
            gt_rec$200, gt_part$25,      /* (EWD011) Trauma Record     */~
            dt_cust$9,                   /* (EWD011) Get Customer      */~
            p_mod$(306%)3, sc_dte$6,     /* Department Models          */~
            p_unt%(306%,3%), tt_unit$24, /* Scanned Units each Prod    */~
            p_unts%(306%,3%),            /* Sampl Units Only           */~
            p_untss%(306%,3%),           /* Charge Sashs Only          */~
            p_untpp%(306%,3%),           /* Charge Parts Only          */~
            p_val(306%,3%),              /* Scanned Units Dollar Val   */~
            p_mrp(6%,3%)                 /* MFG Costs                  */

        dim                              /* (EWD024)                   */~
            tt_in$8, tt_out$8,           /* Start and Complete Times   */~
            tt_hr$3, tt_mn$2             /* Calculated Hours & Minutes */


        dim                              /* (AWD029)                   */~
            supp_dept$(20%)3             /* Support Departments        */

REM        dim                              /* (AWD031)                   */~
            SP_KEY$35,                   /* EWDSCHED READKEY           */~
            SP_STATUS_DTE$6,             /* SPECIAL ANALYSIS STATUS DTE*/~
            SP_STATUS$1,                 /* SPECIAL ANALYSIS PROCESS CD*/~
            SP_TYPE$1,                   /* ANALYSIS TYPE PROCESS CODE */~
            SP_TYPE1$1,                  /* ANALYSIS TYPE PROCESS CODE1*/~
            SP_ROUTE$5,                  /* ROUTE CODE                 */~
            SP_CUTOFF$2,                 /* CUT OFF DAY 1 THRU 7       */~
            SP_CUST$9,                   /* CUSTOMER CODE              */~
            SP_SO$8,                     /* SALES ORDER NUMBER         */~
            SP_LN$2,                     /* S.O. LINE ITEM NO.         */~
            SP_LN_ITEM$4,                /* LINE ITEM PIECE            */~
            SP_DUE$6,                    /* SALES ORDER DUE DATE       */~
            SP_PART$25,                  /* PART NUMBER                */~
            SP_QTY$4,                    /* LINE ITEM QUANTITY         */~
            SP_QTY1$4,                   /* LINE ITEM QUANTITY 1      */~
            SP_STAT$2,                   /* (PLAN STAT) PLANNING       */~
            SP_USR$3,                    /* LAST MOD USER ID           */~
            SP_DTE$6,                    /* LAST MOD DATE              */~
            SP_TIME$8,                   /* LAST MOD TIME              */~
            SP_TEXT$4,                   /* LINE ITEM TEXT ID          */~
            SP_PRIMARY$3,                /* PRIMARY DEPT               */~
            SP_FIL$17                    /* FILLER AREA                */


        dim hldso$8,                     /* SALES ORDER NUMBER         */~
            hldln$2,                     /* S.O. LINE ITEM NO.         */~
            hldlnitem$4,                 /* LINE ITEM PIECE            */~
            hldKey$16,                   /* HLDSCHED Key (IM8022)      */~
            hldLamn$1,                   /* Laminate Flag (IM8022)     */~
            savHld$16,                   /* Save HLDSCHED Key          */~
            hldRec$256,                  /* HLDSCHED Rec               */~
            hldType$1,                   /* Hld Type                   */~
            hldStatus$1,                 /* Hld Status                 */~
            hldscrmkKey$28,              /* HLDSCRMK Key 2             */~
            hldscrmkRec$256,             /* HLDSCRMK Rec               */~
            hldbar$9,                    /* remake barcode             */~
            hldnum$3,                    /* remake number              */~
            hldtime$8,                   /* Remake Time                */~
            hldrmkStatus$1,              /* HldRemake Status           */~
            hldrmkType$1                 /* HldRemake Type             */


        dim lb_key$35, lb_key1$23,       /* Record Key from LB         */~
            lb_rec$(4%)256,              /* LB Record Data     (PAR001)*/~
            testdate$10,                 /* Calulate Day               */~
            day$1,                       /* Day of the Week            */~
            hinge$12,                    /* Hinge Description          */~
            sav_part$25,                 /* Part Number                */~
            arg1$5,                      /* Load                       */~
            arg2$7,                      /* WS Code                    */~
            arg3$7,                      /* Family Number              */~
            arg4$7,                      /* Production Day             */~
            arg5$12,                     /* Hinge Code                 */~
            arg6$7,                      /* Drywall                    */~
            arg7$4,                      /* WS label           (AWD060)*/~
            arg8$20,                     /* sub part           (AWD062)*/~
            arg9$20,                     /* Series             (AWD068)*/~
            arg10$20,                    /* Open               (AWD068)*/~
            arg11$20,                    /* Open               (AWD068)*/~
            arg12$20                     /* Open               (AWD068)*/

         dim rm_dept_n$3,                /* New Department No  (AWD043)*/~
             dept_wand$1,                /* Screen Character   (AWD043)*/~
             dept_d$30,                  /* Department Desc    (AWD043)*/~
             rm_status$1,                /* Remake Status      (AWD046)*/~
             rm_status_d$(11%)30,        /* Reamke Status Desc (AWD046)*/~
             gls_status$20               /* Glass Status Desc  (AWD046)*/


        dim drywall$7                    /* Drywall Flag       (AWD044) */


        dim plowkey$100                  /* Plowcode key        (AWD052)*/

                                         /* (AWD050)                   */
        dim                                                              ~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4,            /* Additional Info Fields     */~
            specialmull$1,               /* Special Mull Flag          */~
            subpart$20                   /* Subpart                    */

        dim brand$(99,5)40,              /* Brand Display  (AWD055)    */~
            brand$2                      /* Brand from apcplndt (AWD055)*/

/* (AWD068) */
        dim series$16,                   /* WW Series                  */~
            style$10

                                         /* (AWD050)                   */
        dim f2%(50%),                    /* = 0 if the file is open    */~
            fs%(50%), axd$4,             /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(50%)20                 /* Text from file opening     */
           
/* (CR1017) */ 
        dim  dt_date$6,~
             dt_seq$5,~
             dt_part$25,~
             dt_partpos4$1,~
             dt_partpos11$1,~
             p_descr$30,~
             scr_line1$38,~
             scr_line2$38,~
             scr_line3$38,~
             scr_line4$38,~
             scr_line5$38,~
             scr_line6$38

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
            * #1  ! APCPLNDT ! (NEW) Planning Tracking File             *~
            * #2  ! GENCODES ! Master Code Table File                   *~
            * #3  ! APCPLNGR ! (New) Glass Remake File                  *~
            * #4  ! APCPLNDP ! Master Department File                   *~
            * #5  ! APCPLNAD ! (New) Planning Master Audit File         *~
            * #6  ! USERCLMS ! Caelus Master User Def. (USERLCMS)       *~
            * #7  ! APCPLNLD ! Planning/Scheduling Load Master File     *~
            * #8  ! EWDPLNGT ! (New) Glass Re-Make Trauma Ctr (EWD011)  *~
            * #9  ! APCPLNGA ! New Re-Make Glass Audit (EWD005)         *~
            * #10 ! EWDPLNRK ! New Glass Rack Data Base (EWD010)        *~
            * #11 ! HLDSCHED ! Hold Schedule file              (IM8022) *~
            * #13 ! BCKLINES ! Sales Order Lne Items       (AWD060)     *~
            * #15 ! TXTFILE  ! New Master Text File                     *~
            * #18 ! HLDSCRMK ! Special Temp Glass Remake        (IM8022)*~
            * #19 ! CUSTOMER ! Customer Master File             (AWD031)*~
            * #20 ! APCPLNGV ! Glass Receiving File             (AWD033)*~
            * #21 ! EWDPRDLB ! New Production Labels-No File Chg(PAR001)*~
            * #22 ! AWDSCHGL ! Schedule Glass File              (IM8022)*~
            * #42 ! AWDBAYBW ! New BAYBOW file for complaints   (AWD058)*~
            * #63 ! BCKSUBPT ! Sub Part File                    (PAR000)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 24,   keylen = 23,                      ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,    keylen = 24

            select #3,  "APCPLNGR",                                      ~
/*IM8022*/              varc,     indexed,  recsize = 512,               ~
                        keypos = 22,   keylen = 12,                      ~
                        alt key 1, keypos =  7, keylen = 27,             ~
                            key 2, keypos =  1, keylen = 33,             ~
                            key 3, keypos = 13, keylen = 21

            select #4,  "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =   32,              ~
                        keypos = 11,   keylen = 12,                      ~
                        alt key 1, keypos =  9, keylen = 14,             ~
                            key 2, keypos =  4, keylen = 12,             ~
                            key 3, keypos =  1, keylen = 15

            select #5,  "APCPLNAD",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos = 19,   keylen = 33,                      ~
                        alt key 1, keypos =  1, keylen = 33

            select #6,  "USERLCMS",                                     ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   3,                     ~
                        alt key  1, keypos =   4, keylen =  30, dup

            select #7,  "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 11,   keylen =  5,                      ~
                        alt key 1, keypos =  3, keylen = 13,             ~
                            key 2, keypos =  1, keylen = 15
                                                   /* (EWD011) - Begin */
            select #8,  "EWDPLNGT",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos = 1,    keylen = 29,                      ~
                        alt key 1, keypos = 18, keylen = 12,             ~
                            key 2, keypos =  4, keylen = 26
                              /* (EWD024) - Change EWDPLNGT Key Layout */
                                                   /* (EWD011) - End   */
                                                   /* (EWD005) - Begin */
            select #9,  "APCPLNGA",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =  1,   keylen = 18,                      ~
                        alt key 1, keypos =  7, keylen = 12
                                                   /* (EWD005) - End   */

                                                   /* (EWD010) - Begin */
            select #10, "EWDPLNRK",                                      ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  9,                      ~
                        alt key  1, keypos = 10, keylen =  14

            select #11, "HLDSCHED",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  40,                     ~
                        alt key  1, keypos =   16, keylen =  25,         ~
                            key  2, keypos =   27, keylen =  16

            select #13, "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #15, "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =   1, keylen =  11
                                                    /* (EWD010)/(EWD011) */
REM            select #18,  "EWDSCRMK",                                     ~
REM                        varc,     indexed,  recsize =  128,              ~
REM                        keypos = 1,   keylen = 50,                       ~
REM                        alt key 1, keypos =  16, keylen = 35

            select #18,  "HLDSCRMK",                                     ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,   keylen = 52,                       ~
                        alt key 1, keypos =  16, keylen = 37,            ~
                            key 2, keypos =  27, keylen = 28


            select #19,  "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos = 1,    keylen =  9,                      ~
                        alt key 1, keypos = 10, keylen = 30, dup,        ~
                            key 2, keypos =424, keylen =  9, dup,        ~
                            key 3, keypos =771, keylen =  9, dup,        ~
                            key 4, keypos =780, keylen =  9, dup,        ~
                            key 5, keypos = 1049, keylen = 9, dup
                                                   /* (AWD031) - End   */
                                                   /* (AWD033) - Begin */

            select #20,  "APCPLNGV",                                     ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =  1,   keylen = 18,                      ~
                        alt key 1, keypos =  7, keylen = 12
                                                   /* (AWD033) - End   */
                                                   /* (PAR001)         */
            select #21, "EWDPRDLB",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =   35,                    ~
                        alt key  1, keypos = 278, keylen =  23
                                                    /* (PAR001)         */

/*AWD058*/  select #42, "AWDBAYBW",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =   29

/*PAR000*/
            select #63, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup


            call "SHOSTAT" ("Opening Files, One moment Please?")
                                                         /* (EWD0016)   */
            filename$ = "APCPLNDT" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNGR" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDP" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNAD" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error


            filename$ = "APCPLNLD" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
                                                      /*  (EWD024)      */
                                                      /*  Add OPENCHCK  */
REM            call "OPENCHCK" (#8, fs%(8%), f2%(8%),500%, rslt$(8%))
REM            filename$ = "EWDPLNGT"  call "EWDOPEN" (#8, filename$, err%)
REM         if err% <> 0% then gosub open_error

            call "OPENCHCK" (#9, fs%(9%), f2%(9%),500%, rslt$(9%))
REM            filename$ = "APCPLNGA"  call "EWDOPEN" (#9, filename$, err%)
REM            if err% <> 0% then gosub open_error
            filename$ = "EWDPLNRK" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "HLDSCHED" : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#13, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "TXTFILE" : call "EWDOPEN" (#15, filename$, err%)
            if err% <> 0% then gosub open_error

            call "OPENOLIB" (#6, "SHARE", f2%(6%), rslt$(6%), axd$)
                                                         /* (EWD016)   */

            filename$ = "HLDSCRMK" : call "EWDOPEN" (#18, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "CUSTOMER" : call "EWDOPEN" (#19, filename$, err%)
            if err% <> 0% then gosub open_error
                                                         /* (AWD031)   */

             call "OPENCHCK" (#20, fs%(20%), f2%(20%),500%, rslt$(20%))

            filename$ = "EWDPRDLB" : call "EWDOPEN" (#21, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDBAYBW" : call "EWDOPEN" (#42, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
            if err% <> 0% then gosub open_error

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            dim apc$40, pname$21
            apc$   = "* (EWD) Planning Master Scan Utility **"
            pname$ = "APCSCANN - Rev: R7.00"


            dim rf_apc$20, rf_pname$8                       /* (AWD045)  */
            rf_apc$   = "Appian Mst Scan Util"              /* (AWD045)  */
            rf_pname$ = "AWDRFSCN"                          /* (AWD045)  */

            trauma_center$ = "N"                           /* (EWD014) */



            scr$(1%) = "****************************************"
            scr$(2%) = "*       SCANNING SELECTION CODES       *"
            scr$(3%) = "* ------------------------------------ *"
            scr$(4%) = "* (1) - Production Scanning     <xxx>  *"
            scr$(5%) = "* (2) - Wood Surround Receiving <xxx>  *"
            scr$(6%) = "* (3) - Available                      *"
            scr$(7%) = "* (4) - Glass Scanning-Table    <110>  *"
            scr$(8%) = "* (5) - Glass Remake Scanning   <112>  *"
REM   SCR$(9%) = "* (6) - RGA / RG SCANNING       <114>  *" /* (CR456) */
            scr$(9%) = "*                                      *"
            scr$(10%)= "* (7) - Log Maint/Undelivered   <116>  *"
            scr$(11%)= "* (8) - Holding Area Invent.    <116>  *"
            scr$(12%)= "****************************************"

            err_check% = 0%              /* AWD051                     */
            b_max% = 10%                     /* SET NUMBER OF TIMES TO */
                                             /* RING BELL ON SCREEN    */
            call "EXTRACT" addr("ID", userid$)
REM            IF USERID$ = "CMG" THEN USERID$ = "OV1"
REM            IF USERID$ = "CG1" THEN USERID$ = "GF2"
            date$ = date
            call "DATEFMT" (date$)
                                               /* Screen Default Msg   */
            gl$(1%) = " GGGGG    L          AAAAA     SSSSS     SSSSS "
            gl$(2%) = "G     G   L         A     A   S     S   S     S"
            gl$(3%) = "G         L         A     A     S         S    "
            gl$(4%) = "G         L         AAAAAAA       S         S  "
            gl$(5%) = "G   GGG   L         A     A         S         S"
            gl$(6%) = "G     G   L         A     A   S     S   S     S"
            gl$(7%) = " GGGGG    LLLLLLL   A     A    SSSSS     SSSSS "

            st$(1%) = " SSSSS    TTTTTTT    AAAAA     GGGGG    EEEEEE "
            st$(2%) = "S     S      T      A     A   G     G   E      "
            st$(3%) = "  S          T      A     A   G         E      "
            st$(4%) = "    S        T      AAAAAAA   G   GGG   EEEE   "
            st$(5%) = "      S      T      A     A   G     G   E      "
            st$(6%) = "S     S      T      A     A   G     G   E      "
            st$(7%) = " SSSSS       T      A     A    GGGGGG   EEEEEE "

            ld$(1%) = "     L          OOOOO     AAAAA    DDDDDD      "
            ld$(2%) = "     L         O     O   A     A   D     D     "
            ld$(3%) = "     L         O     O   A     A   D     D     "
            ld$(4%) = "     L         O     O   AAAAAAA   D     D     "
            ld$(5%) = "     L         O     O   A     A   D     D     "
            ld$(6%) = "     L         O     O   A     A   D     D     "
            ld$(7%) = "     LLLLLLL    OOOOO    A     A   DDDDDD      "

            pd$(1%) = "    PPPPPPP    RRRRRR     OOOOO    DDDDDD      "
            pd$(2%) = "    P      P   R     R   O     O   D     D     "
            pd$(3%) = "    P      P   R     R   O     O   D     D     "
            pd$(4%) = "    PPPPPPP    RRRRRR    O     O   D     D     "
            pd$(5%) = "    P          R   R     O     O   D     D     "
            pd$(6%) = "    P          R    R    O     O   D     D     "
            pd$(7%) = "    P          R     R    OOOOO    DDDDDD      "

                                               /* Scanned Complete Msg */
            ps$(1%) = " CCCCC     OOOOO    M      M   PPPPPPP         "
            ps$(2%) = "C     C   O     O   MM    MM   P      P        "
            ps$(3%) = "C         O     O   M M  M M   P      P        "
            ps$(4%) = "C         O     O   M  MM  M   PPPPPPP    [][] "
            ps$(5%) = "C         O     O   M      M   P          [][] "
            ps$(6%) = "C     C   O     O   M      M   P               "
            ps$(7%) = " CCCCC     OOOOO    M      M   P               "
                                               /* Scanned Error Msg    */
            ee$(1%) = "      SSSSS    TTTTTTT    OOOOO    PPPPPP     "
            ee$(2%) = "     S     S      T      O     O   P     P    "
            ee$(3%) = "       S          T      O     O   P     P    "
            ee$(4%) = "         S        T      O     O   PPPPPP     "
            ee$(5%) = "           S      T      O     O   P          "
            ee$(6%) = "     S     S      T      O     O   P      [][]"
            ee$(7%) = "      SSSSS       T       OOOOO    P      [][]"


            rf_ee$(1%) = "  SSS TTT OOO PPP   "                /* (AWD045) */
            rf_ee$(2%) = "  S    T  O O P P   "
            rf_ee$(3%) = "  SSS  T  O O PPP   "
            rf_ee$(4%) = "     S T  O O P     "
            rf_ee$(5%) = "     S T  O O P     "
            rf_ee$(6%) = "  SSSS T  OOO P     "


/* (AWD055) */
            REM            1234567890123456789012345678901234567890
            brand$(1,1) = "EEEE  L    L    IIIII   SSS  OOO  N   N"
            brand$(1,2) = "E     L    L      I    S    O   O NN  N"
            brand$(1,3) = "EEE   L    L      I     SS  O   O N N N"
            brand$(1,4) = "E     L    L      I       S O   O N  NN"
            brand$(1,5) = "EEEE  LLLL LLLL IIIII   SSS  OOO  N   N"

            brand$(2,1) = "PPP   RRR   OO  V   V IIIII EEEEE W   W"
            brand$(2,2) = "P  P  R  R O  O V   V   I   E     W   W"
            brand$(2,3) = "PPP   RRR  O  O V   V   I   EEE   W W W"
            brand$(2,4) = "P     R  R O  O  V V    I   E      W W "
            brand$(2,5) = "P     R  R  OO    V   IIIII EEEEE  W W "

            brand$(3,1) = "W   W EEEE  AA  L      OOO   CCC  K   K"
            brand$(3,2) = "W   W E    A  A L     O   O C   C K  KK"
            brand$(3,3) = "W W W EEE  AAAA L     O   O C     KKK  "
            brand$(3,4) = " W W  E    A  A L     O   O C   C K  KK"
            brand$(3,5) = " W W  EEEE A  A LLLLL  OOO   CCC  K   K"

            brand$(4,1) = " AAA  L    L   "
            brand$(4,2) = "A   A L    L   "
            brand$(4,3) = "AAAAA L    L   "
            brand$(4,4) = "A   A L    L   "
            brand$(4,5) = "A   A LLLL LLLL"


            brand$(5,1) = "BBBB   AA  Y  Y     / BBBB   OOO  W   W"
            brand$(5,2) = "B   B A  A Y  Y    /  B   B O   O W   W"
            brand$(5,3) = "BBBB  AAAA  YY    /   BBBB  O   O W W W"
            brand$(5,4) = "B   B A  A  YY   /    B   B O   O W WWW"
            brand$(5,5) = "BBBB  A  A  YY  /     BBBB   OOO   W W "


            brand$(6,1) = "RRR   EEEE Y  Y N   N  DDD   SSS"
            brand$(6,2) = "R  R  E    Y  Y NN  N  D  D S   "
            brand$(6,3) = "RRR   EEE   YY  N N N  D  D  SS "
            brand$(6,4) = "R  R  E     YY  N  NN  D  D    S"
            brand$(6,5) = "R  R  EEEE  YY  N   N  DDD   SSS"

            brand$(7,1) = "L     AA  N   N  SSS III N   N  GGG"
            brand$(7,2) = "L    A  A NN  N S     I  NN  N G   "
            brand$(7,3) = "L    AAAA N N N  SS   I  N N N G GG"
            brand$(7,4) = "L    A  A N  NN    S  I  N  NN G  G"
            brand$(7,5) = "LLLL A  A N   N  SSS III N   N  GGG"

            brand$(8,1) = " AAA  TTT RRR  III U  U M   M"
            brand$(8,2) = "A   A  T  R  R  I  U  U MM MM"
            brand$(8,3) = "AAAAA  T  RRR   I  U  U M M M"
            brand$(8,4) = "A   A  T  R  R  I  U  U M   M"
            brand$(8,5) = "A   A  T  R  R III  UU  M   M"

            brand$(9,1) = "RRR  EEEE L    III  AA  BBB  III L  "
            brand$(9,2) = "R  R E    L     I  A  A B  B  I  L  "
            brand$(9,3) = "RRR  EEE  L     I  AAAA BBB   I  L  "
            brand$(9,4) = "R  R E    L     I  A  A B  B  I  L  "
            brand$(9,5) = "R  R EEEE LLLL III A  A BBB  III LLL"

            brand$(10,1) = "H  H U  U TTT TTT III N   N  GGG "
            brand$(10,2) = "H  H U  U  T   T   I  NN  N G    "
            brand$(10,3) = "HHHH U  U  T   T   I  N N N G GGG"
            brand$(10,4) = "H  H U  U  T   T   I  N  NN G   G"
            brand$(10,5) = "H  H  UU   T   T  III N   N  GGG "

            brand$(11,1) = "M   M  OOOO DDD  EEEE RRR   N   N"
            brand$(11,2) = "MM MM O   O D  D E    R  R  NN  N"
            brand$(11,3) = "M M M O   O D  D EEE  RRR   N N N"
            brand$(11,4) = "M   M O   O D  D E    R  R  N  NN"
            brand$(11,5) = "M   M  OOO  DDD  EEEE R   R N   N"

/* (AWD059) */
            brand$(13,1) = "V   V III EEEE W   W PPP  N  N TTT"
            brand$(13,2) = "V   V  I  E    W   W P  P N  N  T "
            brand$(13,3) = "V   V  I  EEE  W W W PPP  NN N  T "
            brand$(13,4) = " V V   I  E    W W W P    N NN  T "
            brand$(13,5) = "  V   III EEEE WW WW P    N  N  T "
/* (AWD059) */
/* (AWD067)*/
REM         call "SHOSTAT" ("Norandex scanning") stop
            brand$(14,1) = "N   N  OOO  RRR   N   N DDD  X   X"
            brand$(14,2) = "NN  N O   O R  R  NN  N D  D  X X "
            brand$(14,3) = "N N N O   O RRR   N N N D  D   X  "
            brand$(14,4) = "N  NN O   O R  R  N  NN D  D  X X "
            brand$(14,5) = "N   N  OOO  R   R N   N DDD  X   X"
/* (AWD067) */

/* (SR73802)*/
            brand$(16,1) = "EEEE X   X TTT RRR   EEEE M   M EEEE"
            brand$(16,2) = "E     X X   T  R  R  E    MM MM E   "
            brand$(16,3) = "EEE    X    T  RRR   EEE  M M M EEE "
            brand$(16,4) = "E     X X   T  R  R  E    M   M E   "
            brand$(16,5) = "EEEE X   X  T  R   R EEEE M   M EEEE"
/* (SR73802) */
/* CR1911 */
            brand$(17,1) = " AA  M   M    CCC  L     AA   SSS  SSS" 
            brand$(17,2) = "A  A MM MM   C   C L    A  A S    S   "
            brand$(17,3) = "AAAA M M M   C     L    AAAA  SS   SS "
            brand$(17,4) = "A  A M   M   C   C L    A  A    S    S"
            brand$(17,5) = "A  A M   M    CCC  LLLL A  A  SSS  SSS"
            
            brand$(18,1) = "PPPP L     Y   Y  GGGG   EEEE M   M"
            brand$(18,2) = "P  P L      Y Y  G       E    MM MM"
            brand$(18,3) = "PPP  L       Y   G   GGG EEE  M M M"
            brand$(18,4) = "P    L       Y    G   G  E    M   M"
            brand$(18,5) = "P    LLLLL   Y     GGG   EEEE M   M"
            
            brand$(19,1) = "H  H  OOO  M   M EEEE   DD   EEEE PPPP"
            brand$(19,2) = "H  H O   O MM MM E      D D  E    P  P"
            brand$(19,3) = "HHHH O   O M M M EEE    D  D EEE  PPP "
            brand$(19,4) = "H  H O   O M   M E      D D  E    P   "
            brand$(19,5) = "H  H  OOO  M   M EEEE   DD   EEEE P   "
                          
            brand$(20,1) = "W   W N   N W   W    N   N  AA  TTT III"
            brand$(20,2) = "W   W NN  N W   W    NN  N A  A  T   I "
            brand$(20,3) = "W W W N N N W W W    N N N AAAA  T   I "
            brand$(20,4) = "W W W N  NN W W W    N  NN A  A  T   I "
            brand$(20,5) = "WW WW N   N WW WW    N   N A  A  T  III"
            
/* CR2375 */            
            brand$(21,1) = "***************"
            brand$(21,2) = "***************"
            brand$(21,3) = "***************"
            brand$(21,4) = "***************"
            brand$(21,5) = "***************"

            
            brand$(98,1) = "EEEEE RRRR  RRRR   OOO  RRRR "
            brand$(98,2) = "E     R   R R   R O   O R   R"
            brand$(98,3) = "EEEE  RRRR  RRRR  O   O RRRR "
            brand$(98,4) = "E     R   R R   R O   O R   R"
            brand$(98,5) = "EEEEE R   R R   R  OOO  R   R"

            brand$(99,1) = " "
            brand$(99,2) = " "
            brand$(99,3) = " "
            brand$(99,4) = " "
            brand$(99,5) = " "





            inp_text$(1%)="Scan Barcode or Manually Enter Barcode Number"
            inp_text$(3%)="Scan Glass Remake <Reason> Barcode?"
            inp_text$(5%)="Scan Glass Remake <Department> Barcode?"
                                                        /*  (AWD043)   */

            err$(1%)="(Error) Barcode Not on File, or Invalid?          "
            err$(2%)="(Error) Barcode Not on File for Department?       "
            err$(3%)="(Error) Barcode on File for Different Department? "
            err$(4%)="(Error) Barcode Not Valid for Department/Process? "
            err$(5%)="(Error) Barcode Has Already Been Scanned Complete?"
            err$(6%)="(Error) Updating Production Department (XXX)?     "
            err$(7%)="(Error) While Updating Barcode Audit ---------->  "

            err$(8%)="(Error) Updating Receive, Dept (XXX) Not Complete "
REM ERR$(8%)="(ERROR) UPDATING STAGING, DEPT (XXX) NOT COMPLETE "
REM ERR$(9%)="(ERROR) UPDATING LOADING, PRODUCT NOT STAGED?     "
            err$(9%)="(Error) Updating Received Wood Surround Prod?     "
           err$(10%)="(Error) Glass Barcode not on file, or Invalid?    "
           err$(11%)="(Error) Glass Barcode Not Complete Cannot Re-Make?"
           err$(12%)="(Error) Updating Glass, Cannot Update Glass? ? ?  "


           err$(13%)="(Error) Product already Staged? ? ?               "
           err$(14%)="(Error) Product already Loaded? ? ? ?             "


           err$(15%)="(Error) Invalid Reason Code for Glass Re-Make?    "
           err$(16%)="(Error) Invalid Load No.? Not Valid for Load?     "
           err$(17%)="(Error) Shift Code Not Valid for <Time of Day>?   "
           err$(18%)="(Error) Glass Barcode Not Scheduled?              "
REM ERR$(19%)="(ERROR) TEMPERED/SPECIAL LITING - NOT RE-MAKE SCAN"
           err$(19%)="(Error) Special Glass/Liting - Not Re-Make Scan"
           err$(20%)="(Error) Updating Glass? Glass Not Found?          "
           err$(21%)="(Error) Invalid Scanning User Id?                 "
           err$(22%)="(Error) Updating Glass Audit File?                "
           err$(23%)="(Error) Glass Barcode Already Complete?           "
           err$(24%)="(Error) Update HLDSCHED?                          "
                                                            /* (AWD031) */
           err$(25%)="(Error) Updating Glass Received File?             "
                                                            /* (AWD033) */
           err$(26%)="(Error) Invalid Scanning Department ?             "
                                                            /* (AWD043) */
           
            her$(1%) = "     I n v a l i d   B a r c o d e   N o .     "
            her$(2%) = "   N o t   O n   F i l e   F o r   D e p t.    "
            her$(3%) = "      O n   F i l e   F o r   D e p t.         "
            her$(4%) = "N o t  V a l i d  F o r  D e p t  P r o c e s s"
            her$(5%) = " P r o d u c t   A l r e a d y   S c a n n e d "
            her$(6%) = "      E r r o r   U p d a t i n g              "
            her$(7%) = "E r r o r   U p d a t i n g   A u d i t        "
            her$(8%) = " P r o d u c t   N o t  S c a n n e d  By - XXX"
            her$(9%) = "     P r o d u c t   N o t   S t a g e d       "
            her$(10%) = "   G l a s s   B a r c o d e   I n v a l i d   "
            her$(11%) = "   G l a s s   N o t   C o m p l e t e d       "
            her$(12%) = "  U n a b l e   T o   U p d a t e   G l a s s  "
            her$(13%) = " P r o d u c t   A l r e a d y   S t a g e d   "
            her$(14%) = " P r o d u c t   A l r e a d y   L o a d e d   "
            her$(15%) = " G l a s s   R e a s o n   C o d e   Invalid?  "
            her$(16%) = "     I n v a l i d   L o a d   N u m b e r     "
            her$(17%) = "      I n v a l i d   S h i f t   C o d e      "
            her$(18%) = "   G l a s s   N o t   S c h e d u l e d       "
            her$(19%) = "T e m p e r e d / S p e c i a l  Invalid Re-Make?"


                                                              /* (AWD045) - BEG */
            rf_err$(1%)="Brcd Not on File   "
            rf_err$(2%)="Brcd Not Dept      "
            rf_err$(3%)="Brcd Diff Dept     "
            rf_err$(4%)="Brcd Not Valid Dept"
            rf_err$(5%)="Brcd Already Cmplte"
            rf_err$(6%)="Update Prd Dpt(XXX)"
            rf_err$(7%)="Update Brcd Audit  "
            rf_err$(8%)="Update Stg Dpt(XXX)"
            rf_err$(9%)="Update Load Not Stg"
           rf_err$(10%)="Glass Brcd not file"
           rf_err$(11%)="Glass Brcd Not Cmpl"
           rf_err$(12%)="Cannot Update Glass"
           rf_err$(13%)="Product already Stg"
           rf_err$(14%)="Prd already Loaded"
           rf_err$(15%)="Reason Cde Glass RM"
           rf_err$(16%)="Invalid Load No."
           rf_err$(17%)="Shift Code Not Vali"
           rf_err$(18%)="Glass Brcd Not Sch"
           rf_err$(19%)="Tempered/Special Lt"
           rf_err$(20%)="Glass Not Found"
           rf_err$(21%)="Invalid Scann User"
           rf_err$(22%)="Update Glass Audit"
           rf_err$(23%)="Glass Brcd Complet"
           rf_err$(24%)="Not Valid App Labe"
           rf_err$(25%)="Ship Lbl not Prod"
           rf_err$(26%)="Updt Prod Dept(XXX)"
           rf_err$(27%)="Updt Dept(XXX)App"
           rf_err$(28%)="App Lbl Not On File"
           rf_err$(29%)="Not Stg No App Lbl"
           rf_err$(30%)="InvalidDepartment?"

            rf_her$(1%) = "Invalid Barcode No   "
            rf_her$(2%) = "Not On File For Dept "
            rf_her$(3%) = "On File For Dept     "
            rf_her$(4%) = "Not Valid For Dept   "
            rf_her$(5%) = "Prod Already Scanned "
            rf_her$(6%) = "Error  Updating      "
            rf_her$(7%) = "Error Updating Audit "
            rf_her$(8%) = "Prod Not Scan By XXX "
            rf_her$(9%) = "Product Not Staged   "
            rf_her$(10%) = "Glass Barcode Invalid"
            rf_her$(11%) = "Glass Not Completed  "
            rf_her$(12%) = "Not To Update Glass  "
            rf_her$(13%) = "Prod Already Staged  "
            rf_her$(14%) = "Prod Already Loaded  "
            rf_her$(15%) = "Invalid Reason Code? "
            rf_her$(16%) = "Invalid Load Number  "
            rf_her$(17%) = "Invalid Shift Code   "
            rf_her$(18%) = "Glass Not Scheduled  "
            rf_her$(19%) = "Temp/Spec    Re-Make?"
            rf_her$(24%) = "Invalid Appian Bacode"
            rf_her$(25%) = "Ship Does Match Prod "
            rf_her$(26%) = "Updating Product Dept"
            rf_her$(27%) = "Updating Appian Label"
            rf_her$(28%) = "Appian Lbl Not On File"
            rf_her$(29%) = "NOT STAGED-NO LABEL  "
                                                   /* (AWD045)    - END  */

                                                   /* (EWD019)           */
            rep_id$( 1%) = "RB1"   : rep_code$( 1%) = "50"
            rep_id$( 2%) = "RG1"   : rep_code$( 2%) = "51"
            rep_id$( 3%) = "RB2"   : rep_code$( 3%) = "52"
            rep_id$( 4%) = "RG2"   : rep_code$( 4%) = "53"
            rep_id$( 5%) = "RB3"   : rep_code$( 5%) = "54"
            rep_id$( 6%) = "RG3"   : rep_code$( 6%) = "55"
                                     rep_code$( 7%) = "56"
                                     rep_code$( 8%) = "57"
                                     rep_code$( 9%) = "58"
                                     rep_code$(10%) = "59"
                                     rep_code$(11%) = "60"
                                     rep_code$(12%) = "61"
                                     rep_code$(13%) = "62"
                                     rep_code$(14%) = "63"
                                     rep_code$(15%) = "  "
                                     rep_code$(16%) = "65"
                                     rep_code$(17%) = "66"
                                     rep_code$(18%) = "67"
                                     rep_code$(19%) = "68"
                                     rep_code$(20%) = "69"
                                     rep_code$(21%) = "70"
                                     rep_code$(22%) = "71"
                                     rep_code$(23%) = "72"
                                     rep_code$(24%) = "73"
                                     rep_code$(25%) = "74"
                                     rep_code$(26%) = "75"
                                                   /* (EWD019)           */
                                                   /* (AWD029) Support   */
            gosub load_support
                                                   /* (AWD029)           */

            rf_scan% = 0%                          /* (AWD045)      */

        initialize
            if str(userid$,1%,2%) = "GF" then goto rf_initialize
                                                         /* (PAR002)   */
            if str(userid$,1%,2%) = "GN" then goto rf_initialize

                                                           /* (AWD045) */
            edit% = 0%
            glass_receiving% = 0%                        /*  (AWD033)  */
            init(" ") scr_shft$, scr_shft_d$, scr_proc$, scr_proc_d$,    ~
                    scr_dept$, scr_dept_d$, dt_st$, prevcode$, tt_unit$, ~
                      scr_sel$, scr_sel_d$, scr_id$, rm_reason$, rm_bar$,~
                      reason_d$, scrn_title$, title$, fld$(3%),          ~
                      scr_id_d$, scr_load$, scr_load_d$, sav_scr_dept$,  ~
                      day$, rm_dept_n$, dept_wand$, dept_d$, rm_status$, ~
                      rm_status_d$(), gls_status$, rm2bar$
                                         /* (AWD043) */
REM Remake Description RM_STATUS$ + 1 ; because status starts at 0
            rm_status% = -1%                     /*(AWD052)*/
            lookup_dept% = 0%                    /*(AWD052)*/
            rm_status_d$(1%) = "Scanned in for Remake "
            rm_status_d$(2%) = "Glass in Remake Batch "
            rm_status_d$(3%) = "Glass Completed       "
            rm_status_d$(4%) = " "
            rm_status_d$(5%) = "Glass Received in Line"
            rm_status_d$(6%) = " "
            rm_status_d$(7%) = " "
            rm_status_d$(8%) = " "
            rm_status_d$(9%) = " "
            rm_status_d$(10%) = "Tempered Being Ordered"
            rm_status_d$(11%) = "Glass Not On File "


                                                          /* (EWD026)  */
            scr_proc$ = "01"           /* Set Default to Manufacturing */
            tt_unit% = 0%
            brand% = 99%                      /* (AWD055) */
            err%    = 0%
            call "SCHEMA" (schema$,      /* What switch 1-NC 2-NE      */~
                           schema%,      /* Schema                     */~
                           #2,           /* GENCODES                   */~
                           err% )        /* error                      */
                           
        main
            gosub mainmenu
            init(" ") errormsg$
            if keyhit% = 16% then exit_program

               gosub check_selection

               if code% = 0% then goto initialize

               gosub check_dept
               if code% = 0% then goto initialize
               gosub check_shift
               if code% = 0% then goto initialize
               gosub check_process
               if code% = 0% then goto initialize
               gosub check_user
               if code% = 0% then goto initialize
               gosub check_load
               if code% = 0% then goto initialize
            edit% = 1%

            pass%, wood_rmk% = 0%                       /* (AWD037)       */
            if scr_sel% = 2% and keyhit% = 14% then gosub check_pass
                if pass% <> 0% then goto main           /*  (AWD037)      */
            if keyhit% = 14% then gosub prod_scan
         goto main

         rf_initialize                                /* (AWD045) - beg */
            rf_scan% = 1%
            edit% = 0%
            init(" ") scr_shft$, scr_shft_d$, scr_proc$, scr_proc_d$,    ~
                    scr_dept$, scr_dept_d$, dt_st$, prevcode$, tt_unit$, ~
                      scr_id$, scr_sel$, scr_sel_d$, rm_reason$, rm_bar$,~
                      reason_d$, scrn_title$, title$, fld$(3%),          ~
                      scr_id_d$, scr_load$, scr_load_d$, sav_scr_dept$,  ~
                      scr_dept$, scr_shft$, rm_bar1$, rm_dept_n$,        ~
                      dept_wand$, rm_status$

            scr_proc$ = "01"           /* Set Default to Manufacturing */
            tt_unit% = 0%

            rf_inp_text$(1%)="Glass Remakes  "
            rf_inp_text$(2%)="Glass Receiving"

        rf_main
            err% = 0%
            if str(userid$,1%,3%) <> "GF1" then goto NOT_REMAKE_USR
                                                     /* (PAR002)  */
            if str(userid$,1%,3%) <> "GN1" then goto NOT_REMAKE_USR
               scr_sel$ = "5"
               scr_dept$ = "112"
NOT_REMAKE_USR

            if str(userid$,1%,3%) <> "GF2" then goto NOT_REMAKE_RECV
                                                     /* (PAR002)  */
            if str(userid$,1%,3%) <> "GN2" then goto NOT_REMAKE_RECV
               scr_sel$ = "1"
NOT_REMAKE_RECV

            gosub rf_mainmenu
            init(" ") rf_errormsg$
            if keyhit% = 2% then exit_program
            if keyhit% = 3% then gosub prod_scan
               gosub check_selection
               if code% = 0% then goto initialize
               gosub check_dept
               if code% = 0% then goto initialize
               gosub check_shift
               if code% = 0% then goto initialize
               gosub check_process
               if code% = 0% then goto initialize
               gosub check_user
               if code% = 0% then goto initialize
               gosub L11960       /* for load */


            edit% = 1%
                                 /*  (AWD045) - Change Function Keys  */
            if keyhit% = 1% then gosub prod_scan
         goto rf_main

         mainmenu                                /* Main Scanning Menu */
            gosub set_screen_1
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,11), "Scanning Selection    :",                    ~
               at (04,35), fac(lfac$(1%)), scr_sel$             , ch(01),~
               at (04,42), fac(hex(84)),scr_sel_d$              , ch(30),~
                                                                         ~
               at (05,11), "Scanning Department   :",                    ~
               at (05,35), fac(lfac$(1%)), scr_dept$            , ch(03),~
               at (05,42), fac(hex(84)),scr_dept_d$             , ch(30),~
                                                                         ~
               at (06,11), "Scanning Shift Code   :",                    ~
               at (06,35), fac(lfac$(1%)), scr_shft$            , ch(02),~
               at (06,42), fac(hex(84)),scr_shft_d$             , ch(30),~
                                                                         ~
               at (07,11), "Scanning User Id.     :",                    ~
               at (07,35), fac(lfac$(1%)), scr_id$              , ch(03),~
               at (07,42), fac(hex(84)),scr_id_d$               , ch(30),~
                                                                         ~
               at (08,11), "Loading, Load Number  :",                    ~
               at (08,35), fac(lfac$(1%)), scr_load$            , ch(05),~
               at (08,42), fac(hex(84)),scr_load_d$             , ch(30),~
                                                                         ~
               at (09,21), fac(hex(84)), scr$(1%)               , ch(40),~
               at (10,21), fac(hex(84)), scr$(2%)               , ch(40),~
               at (11,21), fac(hex(84)), scr$(3%)               , ch(40),~
               at (12,21), fac(hex(84)), scr$(4%)               , ch(40),~
               at (13,21), fac(hex(84)), scr$(5%)               , ch(40),~
               at (14,21), fac(hex(84)), scr$(6%)               , ch(40),~
               at (15,21), fac(hex(84)), scr$(7%)               , ch(40),~
               at (16,21), fac(hex(84)), scr$(8%)               , ch(40),~
               at (17,21), fac(hex(84)), scr$(9%)               , ch(40),~
               at (18,21), fac(hex(84)), scr$(10%)              , ch(40),~
               at (19,21), fac(hex(84)), scr$(11%)              , ch(40),~
               at (20,21), fac(hex(84)), scr$(12%)              , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               errormsg$ = " "

               if keyhit% <> 1% then goto L03880     /* Print Que        */
                  run$ = "ILPMAN"
                  gosub Run_Program
                  goto mainmenu

L03880:        if keyhit% <> 4% then L03890          /* Report/Utility   */
                  gosub utility_scan
                  goto mainmenu

L03890:        if keyhit% <> 6% then L03900          /* DEPARTMENT CODES */
                  table% = 1%
                  gosub display_codes
                  goto mainmenu

L03900:        if keyhit% <> 7% then L03940          /* DEPARTMENT CODES */
                  run$ = "SCRSCANN"
                  gosub Run_Program
                  goto mainmenu

L03940:        
               /* if keyhit% <> 9% then L03990           SHIFT CODES      */
               /*   table% = 4%                                           */
               /*   gosub display_codes                                   */
               /*   goto mainmenu                                         */
               if keyhit% <> 9% then L03990
			        run$ = "EWDPLN50"
					gosub Run_Program
					goto mainmenu
					
L03990:        if keyhit% <> 10% then L04000         /* Glass Analysis   */
                                                     /* (EWD012) - Track */
                  glstype% = 1%                      /* (IM8022) Annealed */
                  if len(scr_dept$) < 3                                    ~
                      then call "APCPLD45" (1%, "000",     scr_shft$,      ~
                                                        glstype%, #3, #2 ) ~
                      else call "APCPLD45" (0%, scr_dept$, scr_shft$,      ~
                                                         glstype%, #3, #2 )
                                                     /* (EWD012) -       */
                  goto mainmenu
                                                     /* (EWD007) Begin   */
L04000:        if keyhit% <> 11% then goto L04015    /* Glass Search     */
                  run$ = "EWDPLN64"
                  gosub Run_Program
                  goto mainmenu
                                                     /* (EWD007) End     */
L04015:        if keyhit% <> 12% then L04010         /* (EWD022) Begin   */
                  pass% = 0%
                  call "APCPASSW" ("EWDPLN79", " ", pass%)
                  if pass% <> 0% then goto initialize

                  run$ = "EWDPLN79"
                  gosub Run_Program
                  goto mainmenu                    /* (EWD022) End      */

L04010:        if keyhit% <> 13% then L04030         /* Glass Analysis   */
                                                     /* (AWD031) - Track */
                  glstype% = 2%                      /* (IM8022) Tempered*/
                  if len(scr_dept$) < 3                                    ~
                      then call "APCPLD45" (1%, "000",     scr_shft$,      ~
                                                          glstype%, #3, #2)~
                      else call "APCPLD45" (0%, scr_dept$, scr_shft$,      ~
                                                          glstype%, #3, #2)
                                                     /* (AWD031) -       */
                  goto mainmenu


L04030:        if keyhit% <> 15% then L04020
                  call "PRNTSCRN"
                  goto mainmenu

L04020:        if keyhit% <> 32% then L04070
                  pass% = 0%
                  call "APCPASSW" ("APCSCAN2", " ", pass%)
                  if pass% <> 0% then goto initialize

                  run$ = "APCSCAN2"
                  gosub Run_Program
                  goto mainmenu

L04070:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_screen_1
            lfac$(1%) = hex(81)
            init(" ") dateout$, scrn_title$
            title$ = " S c a n n i n g   S e l c t i o n s "
            call "TIME" (dateout$)
            inpmessage$ = "Enter a Valid Scanning, Department, Shift Sele~
        ~ction, Userid Required?"
            pf$(1%) = "(1)Print Que         ( 6)Departments    " &       ~
                      "(11)Glass Search       (14)Scanning    "
            pf$(2%) = "(4)Rpt/Utility       ( 9)Lowes Sleeve   " &       ~
                      "(12)BackOrder Scan     (15)Print Screen"
/*AWD031*/  pf$(3%) = "(7)Screen Scanning   (10)Glass Analysis " &       ~
                      "(13)Temp Glass         (16)Exit Program"
            pfkeys$ = hex(01ffff04ff0607ff090a0b0c0d0e0f102000)
        return                                  /* (EWD022) - Add 12 Key */

                                                /* (AWD045)    -  BEG    */
        rf_mainmenu                                /* Main Scanning Menu */
            gosub rf_set_screen_1
                                                 /* Staging Printer    */
                                                 /* (AWD032) - 6/26/03 */
            accept                                                       ~
               at (01,02), fac(hex(8c)), rf_pname$              , ch(08),~
               at (01,12), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), rf_errormsg$           , ch(20),~
                                                                         ~
               at (03,02), "Sel: ",                                      ~
               at (03,08), fac(lfac$(1%)), scr_sel$             , ch(01),~
                                                                         ~
               at (04,02), "Dept:",                                      ~
               at (04,08), fac(lfac$(1%)), scr_dept$            , ch(03),~
                                                                         ~
               at (05,02), "Shft:",                                      ~
               at (05,08), fac(lfac$(1%)), scr_shft$            , ch(02),~
                                                                         ~
               at (06,02), "User:",                                      ~
               at (06,08), fac(lfac$(1%)), scr_id$              , ch(03),~
                                                                         ~
               at (07,02), "Load:",                                      ~
               at (07,08), fac(lfac$(1%)), scr_load$            , ch(05),~
                                                                         ~
               at (08,02), fac(hex(8c)),   rf_pf$(1%)           , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               rf_errormsg$ = " "

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        rf_set_screen_1
            lfac$(1%) = hex(81)
            init(" ") dateout$, scrn_title$
            title$ = " S c a n n i n g   S e l c t i o n s "
            call "TIME" (dateout$)
            rf_inpmessage$ = "Enter Dept,Shft,ID?"
            rf_pf$(1%) = "(1)Scn (2)Ext       "
            pfkeys$ = hex(0102ffffffffffffffffffffffffffffff00)
        return                                  /* (EWD022) - Add 12 Key */
                                                /* (AWD045)  -  END      */



        prod_scan                                      /* Scann Products */
                                                       /* (EWD013)       */
            if scr_id$ = "GLS" or scr_id$ = "SCN" or scr_id$ = "BYP" then  ~
                                                       goto L04400
            if str(scr_id$,1%,2%) = "OV" then goto L04400
                                                      /* (EWD013) Cleanup*/
            if str(scr_id$,1%,3%) = "RCV" then goto L04400
                                                      /* (PAR002)        */
            if scr_id$ = "NGL" or scr_id$ = "NSC" or scr_id$ = "BYN" then  ~
                                                       goto L04400
            if str(scr_id$,1%,2%) = "ON" then goto L04400

            if str(scr_id$,1%,3%) = "RCN" then goto L04400

                                                      /* (PAR002)        */
                                                      /* (AWD038) */

            if scr_sel% < 6% then goto L04300          /* (EWD004)-Begin */
                                                       /* (EWD006) -     */
               call "APCPASSW" ("APCSCANN", scr_id$, pass%)
                   if pass% <> 0% then goto L04390
REM               IF SCR_SEL% = 6% THEN GOTO RGA_SCAN
               if scr_sel% = 6% then goto main         /* (CR456) */
                                                       /* (EWD006) -     */
               if scr_sel% = 7% then call "EWDUGLGB" ("shop", scr_id$, " ")
               if scr_sel% = 8% then call "EWDUGQTB" ("shop", scr_id$)
               return clear all
               goto main
                                                       /* (EWD004) - End */
L04300:     if scr_sel% <> 4% then goto L04320         /* Only for Glass */
               if scr_id$ = "TRM" then goto L04400

            call "APCPASSW"("APCSCANN", userid$, pass%)/* (EWD012) - OV  */
                if pass% <> 0% then goto L04390
/* CR2220 + */
L04320:     txrmkmsg$ = "No Authorization to access Glass Remake Scanning"
            pass% = 0%
            if schema% = 2% and scr_sel% = 5% then   ~
            call "APCPASSW"("APCSCAN5", userid$, pass%)   /* CR2220 */
                if pass% <> 0% then errormsg$ = txrmkmsg$
                if pass% <> 0% then goto L04390
            init(" ") txrmkmsg$
/* CR2220 - */               
            fieldnr% = 1%
            init (" ") barcode$, wandchar$, xx$()
            rep_id% = 0%                     /* (EWD019) - Exception     */
            for jj% = 1% to 10%              /* Repair Id                */
               if scr_id$ = rep_id$(jj%) then rep_id% = 1%
            next jj%
            if scr_sel% <> 5% then rep_id% = 0%

            if scr_sel% = 6% then goto main         /* (CR456) */
REM         IF SCR_SEL% = 6% THEN GOTO RGA_SCAN     /* (EWD006) NOT USED */
            if scr_sel% > 3% then goto glass_scan   /* (EWD013) Glass    */
                                                    /*   but less than 6 */
                                                    /* (AWD047) */
            if rf_scan% = 1% and scr_sel% = 1% then gosub scanned_received

            gosub'100(fieldnr%)
            errormsg$ = " "
            if keyhit% <> 16% then goto prod_scan_next
L04390:        return clear all
               goto initialize

L04400:        return clear all                      /* (EWD013)      */
               err% = 21%
               gosub err_scrn
               goto initialize

        prod_scan_next
REM CHECK_SHIPPING is now to check wood surround received product!!!!
REM UPDATE_SHIPPING is now to update wood surround received product!!!!
            init(" ") scr_line1$, scr_line2$, scr_line3$, scr_line4$, ~
                      scr_line5$, scr_line6$          /* (CR1017) */
            err_check% = 0%              /* AWD051                     */
/*AWD051*/  if scr_dept$ = "044" and scr_sel% = 1% then goto auto_prod_scan
/*AWD051*/  if scr_dept$ = "054" and scr_sel% = 1% then goto auto_prod_scan
/*CR1716*/  if scr_dept$ = "064" and scr_sel% = 1% then goto auto_prod_scan
/*AWD057*/  if scr_dept$ = "074" and scr_sel% = 1% then goto auto_prod_scan

            if scr_sel% = 1% then gosub check_data
            if scr_sel% = 2% then gosub check_shipping     /* (AWD040)  */
                                                           /* (AWD040)  */
REM            if scr_sel% <> 1% and scr_sel% <> 2% then goto initialize
            if scr_sel% > 3% then goto initialize

            if check% = 0% then goto prod_scan

REM only use of display of brand NO UPDATE
            if scr_id$ = "DSP" then goto prod_scan    /* (AWD055) */

            if scr_sel% = 1% then gosub update_track
            
            if scr_sel% = 2% then gosub update_shipping   /* (AWD040)  */
        REM - Special Mod for Wood Surround Scanning ( Update Staging)

            goto prod_scan

REM         +---------------------------------------------------------------+
REM         | if dept = 44 or 54 the complete both departments. AWD051      |
REM         +---------------------------------------------------------------+
REM         |             EWDHIST Read                                      |
REM         |             EWDHIST Read                                      |
REM         +---------------------------------------------------------------+
auto_prod_scan:
            hold_dept$ = scr_dept$
            err_check% = 1%
            scr_dept$ = "044"
            gosub check_data
            if check% <> 0% then gosub update_track
            if check% <> 0% then goto dept_found

REM            err_check% = 0%
            scr_dept$ = "054"
            gosub check_data
            if check% <> 0% then gosub update_track
            if check% <> 0% then goto dept_found
            if err_check% = 0% and errormsg$ <> " " then goto dept_found

/* CR1716 */
            scr_dept$ = "064"
            gosub check_data
            if check% <> 0% then gosub update_track
            if check% <> 0% then goto dept_found

            if errormsg$ <> " " then gosub err_scrn
            
/* (AWD057) */            
REM            err_check% = 0%
            scr_dept$ = "074"
            gosub check_data
            if check% <> 0% then gosub update_track
            if check% <> 0% then goto dept_found

            if errormsg$ <> " " then gosub err_scrn

dept_found:
/* (/AWD057) */
            err_check% = 0%
            scr_dept$ = hold_dept$

            goto prod_scan



            if scr_sel% <> 1% then goto prod_scan
REM         if scr_dept$ <> "044" then goto prod_scan       /* (EWD026) */
                                                            /* (EWD028) */
REM         if scr_dept$ <> "044" and scr_dept$ <> "033" then goto prod_scan
REM         if scr_dept$ <> "033" then goto prod_scan       /* (EWD028) */

            goto prod_scan                                  /* (EWD028) */

               sav_scr_dept$ = scr_dept$                    /* (EWD026) */
               scr_sel% = 2%            /* Change to Staging Selection */
               gosub stagged            /* Set Flags for Stagging      */
               gosub update_shipping    /* Update all Dept's as Stagged*/
               gosub prod               /* Re-Set Flags for Production */
               scr_sel%  =  1%          /* Re-Set Screen Selection     */
               scr_dept$ = "044"        /* Re-Set Department Code      */
               if sav_scr_dept$ = "033" then scr_dept$ = "033"  /* (EWD026) */
               tt_unit% = tt_unit% - 1% /* Adjust Units from Stagging  */
            goto prod_scan              /* Unit was already updated by */
                                        /* Wood Surround Dept.         */
        rga_scan
            call "APCRGA23" ( #2,                  /* GENCODES Channel */~
                              #5,                  /* APCPLNAD Channel */~
                              #1,                  /* APCPLNDT Channel */~
/*PAR000*/                    #63 )                /* BCKSUBPT CHANNEL */

        return

        glass_scan                                   /* Scan Products */
            fieldnr% = 1%
            init (" ") rm_bar$,rm_reason$,xx$(),bar_wand$,reason_wand$, ~
                       rm_dept_n$, dept_wand$, dept_d$, rm_status$, rm2bar$

        glass_scan_2
            init (" ") xx$()
REM         CONVERT FIELDNR% TO CMG$, PIC(#)
REM         CALL "SHOSTAT" (" FIELDNR% " & CMG$)  STOP

            if rf_scan% = 0% and schema% = 1% then gosub'200(fieldnr%)
/* CR2180 */
            if rf_scan% = 0% and schema% = 2% then gosub'300(fieldnr%)
            if rf_scan% = 1% and glass_receiving% <> 1%              ~
                                               then gosub'201(fieldnr%)
/*(AWD047) */
            if rf_scan% = 1% and glass_receiving% =  1%              ~
                                               then gosub'202(fieldnr%)
            errormsg$ = " "
            rf_errormsg$ = " "                    /* (AWD045) */
            if received% <> 1% or keyhit% <> 16% then goto not_rec_pan
               received% = 0%
               goto prod_scan
not_rec_pan:
                                                           /* (AWD045) */
            if keyhit% <> 16% and rf_scan% = 0% then goto glass_scan_nxt
            if keyhit% <> 2%  and rf_scan% = 1% then goto glass_scan_nxt
               return clear all
               goto initialize
        glass_scan_nxt
            if fieldnr% = 5% then goto L04810
            if scr_sel% <> 5% then goto L04810
               gosub lookup_main_dept                 /* (AWD046) */
               fieldnr% = 3%
               if errormsg$ <> " " then goto glass_scan
               if rm_reason$ <> " " then fieldnr% = 5%
                                               /* (AWD043) */
                                               /* Only do on fieldnr% = 3% */
                                               /* so only looking up once  */
REM            IF FIELDNR% = 5% THEN GOSUB LOOKUP_MAIN_DEPT
               goto glass_scan_2
L04810:
            gosub check_glass
            if check% = 0% then goto glass_scan
                                                        /*  (AWD033)  */
            if glass_receiving% = 1% then gosub check_dept_glass
            if err% <> 0% then goto glass_scan
                                                        /*  (AWD033)  */
            gosub update_glass
            goto glass_scan

        check_data                              /* Scheduled Data      */
            err% = 0%
                                                /*            (AWD029) */
            if supp_flag% = 1% then goto L04900 /* Override not Valid  */
                                                /* Only Valid for Prod */
               if scr_sel% = 1% then goto check_data_override

L04900:     check% = 0% : dt_st% = 0%           /* (APCPLNDT) - FILE   */

            init(" ") dt_key$, errormsg$, dt_bar$, dt_rec$, sav_key$
            str(dt_key$,1%,18%) = barcode$      /* Set Barcode Value   */
            str(dt_key$,19%,3%) = scr_dept$     /* Set Department Code */
            str(dt_key$,22%,2%) = scr_proc$     /* Set Process Code    */
            read #1,key = dt_key$, using L04940 , dt_st$, eod goto L05020
L04940:        FMT POS(64), CH(2)

            convert dt_st$ to dt_st%, data goto L05020

            dt_bar$ = barcode$
REM            if dt_st% > 11% then goto L05070     /* Already Scanned    */
            if dt_st% > 11% and dt_st% <> 32% then goto L05070  
               check% = 1%
               sav_key$ = dt_key$
        return
L05020:     gosub check_errors : gosub err_scrn
        return
L05070:     err% = 5% :  gosub err_scrn
        return


REM     CHECK_REMAKE                            /* REMAKE Prod (AWD044)*/
            err% = 0%
            check% = 0% : dt_st% = 0%           /* (APCPLNDT) - FILE   */

            init(" ") dt_key$, errormsg$, dt_bar$, dt_rec$, sav_key$
            str(dt_key$,1%,18%) = barcode$      /* Set Barcode Value   */
            str(dt_key$,19%,3%) = scr_dept$     /* Set Department Code */
            str(dt_key$,22%,2%) = scr_proc$     /* Set Process Code    */
            read #1,key = dt_key$, using L04940 , dt_st$, eod goto RMK_DONE

            convert dt_st$ to dt_st%, data goto BAD_STATUS_RMK

            dt_bar$ = barcode$
            if dt_st% > 17% then goto L05070     /* bol cut   */
               check% = 1%
               sav_key$ = dt_key$
        return
RMK_DONE:
            gosub check_errors : gosub err_scrn
        return
BAD_STATUS_RMK:
            err% = 5% :  gosub err_scrn
        return
                                                /* (AWD029)            */

        check_shipping                          /* Staging/Loading     */
          err% = 0% : hit% = 0%
          check% = 0% : dt_st% = 0%           /* (APCPLNDT) - FILE   */
          init(" ") dt_key$, errormsg$, dt_bar$, dt_rec$, dt_dept$,    ~
                    dt_load$
          str(dt_key$,1%,18%) = barcode$      /* Set Barcode Value   */
        check_shipping_nxt
          read #1,key > dt_key$, using L05220 ,dt_load$,dt_key$, dt_st$, ~
                                              eod goto L05430
L05220:        FMT CH(5), POS(24), CH(23), POS(64), CH(2)
            if barcode$ <> str(dt_key$,1%,18%) then goto L05430
              dt_dept$ = str(dt_key$,19%,3%)
              convert dt_st$ to dt_st%, data goto L05580

            if dt_dept$ = "001" then goto check_shipping_nxt
            for x% = 1% to supp_max%
               if dt_dept$ = supp_dept$(x%) then goto check_shipping_nxt
            next x%

            if dt_dept$ = "102" and dt_st% < 12% then goto L05360 /* PULL STK*/

            if dt_dept$ = "104" and dt_st% < 12% then goto L05360 /* PULL MUL*/

REM If primary department is not complete, complete it and then change
REM status to 13
               if dt_st% < 12% then goto L05450      /* Staging Test  */
                  if dt_st% > 13% then goto L05640   /* (AWD035)   */
L05360:
                  str(sav_key$,1%,23%) = dt_key$        /* (AWD044) */
                  hit% = hit% + 1%
               goto check_shipping_nxt

                                 /* NOT USED OPTION 3 -- BEG */
               if userid$ = "BYP" then goto L05400      /* (EWD002)      */
               if scr_load$ <> dt_load$ then goto L05720   /* Check Load */
                                         /* Per Keith Barnes remove Test */
                   /* (EWD021) Per Keith Barnes & Kyle Green to add Test */
               if dt_st% < 14% then goto L05450         /* Loading Test  */
L05400:           if dt_st% > 14% then goto L05640      /* (EWD001)      */
                  hit% = hit% + 1%                      /* (EWD002)      */
               goto check_shipping_nxt
                                 /* NOT USED OPTION 3 -- END */

L05430:     if hit% <> 0% then check% = 1% else goto L05580
        return
L05450:
REM IF SCR_SEL% <> 2% THEN GOSUB ERROR_DISPLAY   /* (EWD020)   */
REM If product is not complete, then complete it.
           upd_st$ = "12"
           str(sav_key$,1%,18%) = barcode$
           str(sav_key$,19%,3%) = str(dt_key$,19%,3%)        /* dept */
           str(sav_key$,22%,2%) = str(dt_key$,22%,2%)        /* Proc */
           err_check% = 0%              /* (AWD051)  */

           gosub update_track
           upd_st$ = "13"
           check% = 1%
        return
L05580:    check% = 0%
           err% = 1% : gosub err_scrn
REM IF SCR_SEL% <> 2% THEN GOSUB ERROR_DISPLAY   /* (EWD020)   */
        return
L05640:    check% = 0%
           if scr_sel% = 2% then err% = 13% else err% = 14%
           gosub err_scrn
REM IF SCR_SEL% <> 2% THEN GOSUB ERROR_DISPLAY   /* (EWD020)   */
        return
L05720:    check% = 0%
           err% = 16% : gosub err_scrn
REM IF SCR_SEL% <> 2% THEN GOSUB ERROR_DISPLAY   /* (EWD020)   */
        return

        check_glass                        /* (EWD013) - Check Routine */
REM  CALL "SHOSTAT" (" HERE AT CHECK GLASS " )  STOP
          check% = 0% : cnt% = 0%        /*    Note - Only (1) Glass */
          tempered% = 0%                       /*     (AWD031)       */
          laminate% = 0%                       /*     (IM8022)       */
          init(" ") rm_key$, sav_st$     /*    record per Barcode    */
REM INIT(" ") RM_STATUS$           /*            (AWD046)      */
REM RM_STATUS$ = "Z"
          rm_len% = 0%
          rm_len% = len(rm_bar$)         /* Quick Fix   03/16/2000   */
          rm_len% = 12%                  /* Only allow 12 digit      */

          if rm_len% < 11% then str(rm_key$,1%,9%) = rm_bar$           ~
                           else str(rm_key$,1%,12%) = rm_bar$
                                           /* (EWD018) = Primary Key   */

        check_glass_nxt
          gosub check_glass_file              /*  (AWD031) */
          if glass% <> 1% then goto L05980    /*  (AWD031) */
          cnt% = cnt% + 1%

          if cnt% > 1% and rm_len% > 11% then goto L05980
          rm_key$ = str(rm_rec$(),22%,12%)   /* Glass Barcode & Rm No*/
REM RM_STATUS$ = STR(RM_REC$(),13%,1%)        /* (AWD046) */
 
                                               /* (EWD018)             */
          if rm_len% < 11% and str(rm_key$,1%,9%) <> rm_bar$ then goto L05980

          if rm_len% > 11% and str(rm_key$,1%,12%) <> rm_bar$ then goto L05980
                                               /* (EWD018)             */
            sav_st$ = str(rm_rec$(),13%,1%)
            if scr_sel% = 5% then goto create_remake
                                               /* (EWD013) Glass Table */
            if str(rm_rec$(),13%,1%) = "1" then goto L05960
            if glass_receiving% = 1% then goto L05960  /*  (AWD033) */
        goto check_glass_nxt      /* Glass Not Scheduled  */
        create_remake                          /* Check Completed Glass*/
            if rep_id% <> 1% then goto NoRemakeRepair
             if str(rm_rec$(),13%,1%) = "1" then goto L05900
             if str(rm_rec$(),13%,1%) = "2" then goto check_glass_nxt
NoRemakeRepair:
                                               /*  (AWD033)            */
REM IF STR(RM_REC$(),13%,1%) <> "2" THEN GOTO CHECK_GLASS_NXT
REM 2 = complete; 4 = received
            if str(rm_rec$(),13%,1%) <> "2" and                       ~
               str(rm_rec$(),13%,1%) <> "4" then goto check_glass_nxt
L05900:                                   /* (EWD019) Repair Exception */

            if str(rm_bar$,1%,1%) = "Z" then goto L06110
                                             /* Cannot remake 'Z' Barcode */
            gosub check_reason               /* Can Only Re-Make Comp*/
             if code% = 0% then goto L06150
            gosub check_specials             /* (EWD005) - Spec/Temp */
             if code% = 0% then goto L06160
            gosub check_dept_rmk             /* (AWD043) */
             if code% = 0% then goto L06180   /* (AWD043) */
            dept_d$ = str(desc$,1%,30%)      /* (AWD043) */
L05960:     check% = 1%                        /* Valid Scan of Glass  */
        return
L05980:     if cnt% > 0% then goto L06040
               err% = 10% : goto L06170        /* Not on File/Invalid  */

L06040:     if scr_sel% = 5% then goto L06100
               if sav_st$ = "2" then err% = 23%                         ~
                                else err% = 18%
            goto L06170                        /* Glass Not Scheduled  */
L06100:     err% = 11% : goto L06170           /* Glass Not Completed  */
L06110:     err% = 10% : goto L06170           /* (Z)  Is Invalid      */
L06150:     err% = 15% : goto L06170           /* Invalid Reason Code  */
L06160:     err% = 19% : goto L06170           /* Invalid Temp/Spec    */
L06180:     err% = 26%
L06170:     gosub err_scrn
            check% = 0%
        return

        check_glass_file                             /*  (AWD031)    */
          glass% = 0%
          tempered% = 0%
          laminate% = 0%
          if rm_len% < 11% then                                            ~
             read #3,key > rm_key$, using L05850 , rm_rec$(), eod goto no_glass~
          else                                             ~
             read #3,key = rm_key$, using L05850 , rm_rec$(), eod goto no_glass
L05850:        FMT 4*CH(128)                          /* (PAR000) */

/* (IM8022) */
REM IF STR(RM_REC$(),22%,9%) <> RM_BAR$ THEN GOTO CHECK_TEMP
               if str(rm_rec$(),22%,12%) <> rm_bar$ then goto no_glass
               glstype$ = str(rm_rec$(),335%,20%)
               if str(glstype$,1%,8%) = "TEMPERED" then tempered% = 1%
               if str(glstype$,9%,5%) = "LAMIN" then laminate% = 1%
          glass% = 1%
        no_glass
        return

        update_glass                            /* (APCPLNGR) - File   */
          init(" ") errormsg$, dateout$
          call "TIME" (dateout$)
          read #3,hold,key = rm_key$, using L06260, rm_rec$(), eod goto L06440
L06260:        FMT 4*CH(128)                          /*(IM8022)*/

            delete #3
            set_flag% = 0%

            if scr_sel% = 5% then goto L06340
L06280:
            str(rm_rec$(),7%,6%)  = date    /* Glass Scan Date        */
            str(rm_rec$(),13%,1%) = "2"     /* Glass Scanned Complete */
                                            /* Glass Received         */
            if glass_receiving% = 1% then str(rm_rec$(),13%,1%) = "4"
            if trauma_center$ <> "Y" then goto no_trauma_org
            init(" ") gt_rec$
            str(gt_rec$,1%,1%)  = "0"            /* Status = '0' Open  */
            str(gt_rec$,2%,2%)  = "00"           /* Area = Originator  */
            str(gt_rec$,4%,6%)  = str(rm_rec$(),52%,6%)
            str(gt_rec$,10%,8%) = str(rm_rec$(),44%,8%)
            if str(rm_rec$(),32%,2%) = "00" and scr_sel% = 4% then      ~
               gosub update_glass_trauma_org

            no_trauma_org
              if str(rm_rec$(),61%,4%) = "0000" then gosub calc_remake  ~
                                     else goto L06320

              goto L06420

L06320:                                                 /*  (AWD033)   */
            if glass_receiving% = 1% then goto L06420
              str(rm_rec$(),44%,8%)  = time     /* Completed Time    */
              str(rm_rec$(),52%,6%)  = date     /* Completed Date    */
              str(rm_rec$(),58%,3%)  = scr_id$  /* Completed User id */
              str(rm_rec$(),61%,4%)  = "    "   /* (EWD013)     */
            goto L06420                       /* (EWD009) - End        */

L06340:     str(rm_rec$(),7%,6%)   = date    /* Glass Scan Date        */
            str(rm_rec$(),13%,1%)  = "0"     /* Re-Schedule as Re-Make */
            if tempered% = 1% then str(rm_rec$(),13%,1%)  = "9"
            if laminate% = 1% then str(rm_rec$(),13%,1%)  = "9" /*(IM8022)*/
            str(rm_rec$(),34%,2%)  = rm_reason$ /* Re-Make Reason Code */
                                             /* (EWD019) Exception     */
            if rep_id% = 1% then goto L06280
                                             /* (EWD019)               */
            rm_num% = 1%
            convert str(rm_rec$(),32%,2%) to rm_num%, data goto L06390
L06390:
            convert (rm_num% + 1%) to str(rm_rec$(),32%,2%), pic(00)
            str(rm_rec$(),44%,8%)  = time     /* (EWD001) Set Re-make  */
            str(rm_rec$(),52%,6%)  = date     /* Date/Time Stamp       */
            str(rm_rec$(),58%,3%)  = scr_id$  /* User id               */
            str(rm_rec$(),61%,4%)  = "0000"   /* Total Hrs/Mins        */
                                              /* (EWD001) Clock Starts */
            str(rm_rec$(),249%,3%) = rm_dept_n$ /* (AWD043) */
                                              /* (AWD031) */
                                              /* (IM8022) */
            str(rm_rec$(),356%,1%) = rm_rmk_flag$ /*(SR79102) */
            if tempered% = 1% or laminate% = 1% then gosub update_glass_sched
            if laminate% = 1% and hldsch% = 0% then str(rm_rec$(),13%,1%)  = "0"
            if temp_stock% = 1% then goto L06420
            if tempered% = 1% and dt_rec% <> 1% then goto L06430
            if laminate% = 1% and dt_rec% <> 1% then goto L06430  /*(IM8022)*/

L06420:     str(rm_rec$(),14%,8%) = dateout$  /* Time of Status Change */
            str(rm_rec$(),36%,6%) = date      /* Date of Status Change */
            str(rm_rec$(),42%,2%) = scr_shft$ /* Scanning Shift Code   */
            str(rm_rec$(),252%,3%) = str(rm_rec$(),31,3%)   /*REMAKE NO*/
                                              /* (EWD012) Begin        */
            if str(userid$,1%,2%) = "OV" then                           ~
               str(rm_rec$(),65%,1%) = str(userid$,3%,1%) /* 1-9,A-Z   */
                                              /* (PAR002)              */
            if str(userid$,1%,2%) = "ON" then                           ~
               str(rm_rec$(),65%,1%) = str(userid$,3%,1%) /* 1-3       */

            if str(userid$,1%,3%) = "GLS" then                          ~
               str(rm_rec$(),65%,1%) = str(scr_shft$,2%,1%)  /*Temporary*/
                                              /* (PAR002)              */
            if str(userid$,1%,3%) = "NGL" then                          ~
               str(rm_rec$(),65%,1%) = str(scr_shft$,2%,1%)  /*Temporary*/
                                              /* (PAR002)              */
                                              /* (EWD012) for Tracking */
            put #3, using L06260, rm_rec$()
            write #3, eod goto L06430
               tt_unit% = tt_unit% + 1%          /* Calc Scanned Units */


            if trauma_center$ <> "Y" then goto no_trauma
               if set_flag% = 1% then gosub update_ewdplngt   /* (EWD024) */
               if str(rm_rec$(),32%,2%) = "00" and scr_sel% = 4% then   ~
                      gosub update_ewdplngt                /* (EWD024) */
               if scr_sel% = 5% then                                    ~
                      gosub update_glass_trauma            /* (EWD024) */

            no_trauma

            if set_flag% = 1% then gosub update_glass_audit
                                                         /*  (AWD033)  */
            if glass_receiving% = 1% then                     ~
                               gosub calc_remake
            if glass_receiving% = 1% then                     ~
                               gosub update_glass_audit_rec

                                                 /* (EWD005)           */
            gosub update_glass_rack              /* (EWD010)           */


                                                 /* (EWD011)           */
                                                /* (EWD014) New Switch */
            gosub ok_scrn
        return
L06430:     err% = 12%
            goto L06450

L06440: err% = 20%
L06450: gosub err_scrn
        return
                                                 /* (EWD010) - Begin   */
        update_glass_rack
            if str(rm_rec$(),13%,1%) <> "2" then return
            read #10,hold,key = str(rm_rec$(),22%,9%),                   ~
                                          eod goto update_glass_rack_done
            put #10, using L06500, "1"
L06500:       FMT POS(58), CH(1)

            rewrite #10
        update_glass_rack_done
        return                                   /* (EWD010) - Done    */

        update_glass_trauma                      /* (EWD011) - Begin   */
          if str(rm_rec$(),13%,1%) <> "0" then return
                                                 /* Only Re-Makes      */
                                                 /* All Dept's and all */
                                                 /* Shifts             */
REM IF STR(RM_REC$(),42%,2%) = "03" THEN RETURN
REM IF STR(RM_REC$(),42%,2%) = "02" THEN RETURN

          init(" ") gt_rec$
          str(gt_rec$,1%,1%)  = "0"            /* Status = '0' Open  */
          str(gt_rec$,2%,2%)  = "00"           /* Area = Originator  */
          str(gt_rec$,4%,6%)  = date           /* Date Scanned (In)  */
          str(gt_rec$,10%,8%) = time           /* Time Scanned (In)  */
        update_glass_trauma_org
          str(gt_rec$,18%,9%) = str(rm_rec$(),22%,9%)
                                                 /* Re-Make Barcode     */
          str(gt_rec$,27%,3%) = str(rm_rec$(),31%,3%)
                                                 /* Glass Re-Make Number*/
                                                 /* Change place        */
                                                 /* of re-make num & model  */
          str(gt_rec$,30%,3%) = str(rm_rec$(),249%,3%)
                                                 /* Production Dept Code*/
          str(gt_rec$,33%,3%) = "TOP"
          if str(rm_rec$(),111%,1%) = "B" then str(gt_rec$,33%,3%) = "BOT"
                                                 /* View - Top or Bot   */
          gt_part$ = str(rm_rec$(),125%,25%)   /* MFG Part Number     */
          init(" ") code$, table$
          code$ = str(gt_part$,4%,1%)          /* Color Code          */
          table$ = "COLOR    "
          gosub check_code
          str(gt_rec$,36%,6%)     = str(desc$,6%,6%)
                                                 /* Color Long Form     */
          str(gt_rec$,42%,7%)     = str(rm_rec$(),234%,7%)
                                                 /* Muttin Code         */
          str(gt_rec$,49%,9%)     = str(rm_rec$(),85%,9%)
                                                 /* Cut Width           */
          str(gt_rec$,58%,9%)     = str(rm_rec$(),94%,8%)
                                                 /* Cut Height          */
          init(" ") code$, table$, dt_key$
          code$  = str(gt_part$,5%,2%)
          table$ = "GLASS    "
          gosub check_code
          str(gt_rec$,67%,4%)     = str(desc$,1%,3%) & " "
                                                 /* Glass Type Code     */
          str(gt_rec$,71%,5%)     = str(rm_rec$(),242%,5%)
                                                 /* Production Seq No.  */
          str(gt_rec$,76%,8%)     = str(rm_rec$(),163%,8%)
                                                 /* Sales Order No.     */
          str(dt_key$,1%,8%)      = str(gt_rec$,76%,8%)
          read #1,key > dt_key$, using Lxxx00, dt_key$, dt_cust$,       ~
                                                          eod goto Lxxx10
Lxxx00:        FMT POS(24), CH(23), POS(124), CH(9)
            if str(gt_rec$,76%,8%) <> str(dt_key$,1%,8%) then goto Lxxx10

            str(gt_rec$,84%,9%)  = dt_cust$
                                                 /* Customer Code       */
Lxxx10:     str(gt_rec$,93%,10%)    = str(rm_rec$(),1%,6%) & "    "
            call "DATFMTC" ( str(gt_rec$,93%,10%) )
                                                 /* Formatted Prod Dte  */
            str(gt_rec$,103%,3%)    = str(rm_rec$(),72%,3%)
                                                 /* Model Code          */
            str(gt_rec$,106%,1%)    = "N"
                                                 /* Label Printed (N)o  */
            str(gt_rec$,107%,13%)   = str(rm_rec$(),150%,13%)
                                                 /* Window Width/Height */
            str(gt_rec$,120%,2%)    = str(rm_rec$(),42%,2%)
                                                 /* Shift Code of Scan  */
            str(gt_rec$,122%,3%)    = str(rm_rec$(),58%,3%)
                                                 /* User Id of Requester*/
            init(" ") rm_subpart$
            rm_subpart$ = str(rm_rec$(),255%,20%)        /* (PAR000)  */
/* (AWD054) */
REM IF STR(RM_SUBPART$,1%,1%) = "2" THEN P% = 1%
                                                 /* field1$  (PAR000) */
            if str(rm_subpart$,1%,1%) = "2" and             ~
                     str(rm_subpart$,1%,1%) = "3" then p% = 1%
            if str(rm_subpart$,1%,1%) = "2" and             ~
                 str(rm_subpart$,1%,1%) = "1" then str(gt_rec$,125%,1%) = "E"
                                                /* field1$  (PAR000) */
            if p% <> 0% then str(gt_rec$,125%,1%) = "C"
                                                 /*Test/Set Contour Grid*/
            dt_txt$ = str(rm_rec$(),112%,4%)
            nbr_line% = 0%
            init(" ") text$()
            call "APCPLTXT" (#15, dt_txt$, text$(), nbr_line%)
            str(gt_rec$,126%,40%)   = str(text$(2%),1%,40%)
                                                 /* Glass Text          */
            str(gt_rec$,166%,6%)    = "      "
                                                 /* Date Completed      */
            str(gt_rec$,172%,8%)    = "        "
                                                 /* Time Completed      */
            str(gt_rec$,180%,2%)    = "00"
                                                 /* shift Completed     */
            str(gt_rec$,182%,3%)    = "XXX"
                                                 /* User Who Completed  */
            str(gt_rec$,185%,6%)    = "000:00"
                                                 /* Total hhh-mm        */
            str(gt_rec$,191%,4%)    = str(rm_rec$(),171%,4%)
                                                 /* Spacer Thickness    */
            str(gt_rec$,195%,6%)    = "      "

        dataput_gt                               /* (EWD024)            */
                                                 /* Filler Area         */
            write #8, using L07300, gt_rec$, eod goto L07310
L07300:         FMT CH(200)
L07310:     gosub remake_display
        return

        update_glass_audit_rec
/* Do not update orginal only remakes (AWD061) */
           if str(rm_rec$(),32%,2%) = "00" then return
           init (" ") rma_rec$
           str(rma_rec$,1%,6%)   = date                   /* Date     */
           str(rma_rec$,7%,9%)   = str(rm_rec$(),22%,9%)  /* Bar Code */
           str(rma_rec$,16%,3%)  = str(rm_rec$(),31%,3%)  /* Rmk No.  */
           str(rma_rec$,19%,8%)  = time                   /* Rmk Time */
           str(rma_rec$,27%,3%)  = scr_id$                /* User Id  */
           str(rma_rec$,34%,3%)  = scr_dept$              /* Dept     */
           str(rma_rec$,37%,2%)  = str(rm_rec$(),34%,2%)  /* Reason Cd*/
           convert r_hrs% to str(rma_rec$,30%,2%), pic(00)
           convert r_min% to str(rma_rec$,32%,2%), pic(00)


           read #20, hold, key 1% = rm_key$, using L07320, rma_rec$,     ~
                                                         eod goto not_received
L07320:         FMT CH(64)
           convert r_hrs% to str(rma_rec$,39%,2%), pic(00)
           convert r_min% to str(rma_rec$,41%,2%), pic(00)

            delete #20
not_received:
           write #20, using L06510, rma_rec$, eod goto L06545

        return
L06545:     err% = 25%
            gosub err_scrn
        return                                   /*  (AWD033)  -  END  */


        update_glass_audit                       /* (EWD005) - Begin   */
           set_flag% = 0%
           init(" ") rma_rec$
           str(rma_rec$,1%,6%)   = str(rm_rec$(),52%,6%)  /* Date     */
           str(rma_rec$,7%,9%)   = str(rm_rec$(),22%,9%)  /* Bar Code */
           str(rma_rec$,16%,3%)  = str(rm_rec$(),31%,3%)  /* Rmk No.  */
           str(rma_rec$,19%,8%)  = str(rm_rec$(),44%,8%)  /* Rmk Time */
           str(rma_rec$,27%,3%)  = str(rm_rec$(),58%,3%)  /* User Id  */
           str(rma_rec$,30%,4%)  = str(rm_rec$(),61%,4%)  /* Completed*/
           str(rma_rec$,34%,3%)  = str(rm_rec$(),249%,3%) /* Dept     */
           str(rma_rec$,37%,2%)  = str(rm_rec$(),34%,2%)  /* Reason Cd*/
           str(rma_rec$,39%,26%) = " "
                                                       /*  (AWD031)          */
           write #9%, using L06510, rma_rec$, eod goto L06520
L06510:         FMT CH(64)

REM  GOSUB UPDATE_EWDPLNGT
        return
L06520:     err% = 22%
            gosub err_scrn
        return

        update_glass_sched                       /* (AWD031) - Begin   */
          updteCnt% = 0%
          gosub check_temp_stock
          if temp_stock% = 1% then return
          gosub lookup_dt
            if dt_rec% <> 1% then return
          gosub findHldsched                              /* (IM8022) */

REM INIT(" ") SP_STATUS_DTE$, SP_STATUS$, SP_TYPE$, SP_DUE$,    ~
REM SP_PART$, SP_QTY$, SP_QTY1$, SP_USR$, SP_DTE$,    ~
REM SP_KEY$, SP_PRIMARY$, SP_TYPE1$, SP_TIME$
REM SP_QTY1% = 0
REM GOSUB LOOKUP_CUST
REM SP_STATUS_DTE$ = DATE
REM SP_STATUS$     = "0"
REM SP_TYPE$       = "5"
REM SP_DUE$        = " "
REM SP_PART$       = STR(RM_REC$(),125%,25%)
REM SP_QTY$        = "0001"
REM SP_USR$        = USERID$
REM SP_DTE$        = DATE
REM CALL "TIME" (SP_TIME$)
REM SP_PRIMARY$    = STR(RM_REC$(),249%,3%)
REM STR(SP_KEY$,1%,9%)  = SP_CUST$
REM STR(SP_KEY$,10%,8%) = SP_SO$
REM STR(SP_KEY$,18%,2%) = SP_LN$
REM STR(SP_KEY$,20%,4%) = SP_LN_ITEM$
REM STR(SP_KEY$,24%,9%) = STR(RM_REC$(),22%,9%)
REM STR(SP_KEY$,33%,3%) = STR(RM_REC$(),31%,3%)
REM
REM READ #18, HOLD, KEY 1% = SP_KEY$, USING L07340, SP_TYPE1$,   ~
REM                           SP_QTY1$, EOD GOTO NO_EWDSCHED
REM L07340            FMT POS(8), CH(1), POS(82), CH(4)
REM IF SP_TYPE1$ <> "5" THEN GOTO NOT_REMAKE
REM
REM CONVERT SP_QTY1$ TO SP_QTY1%, DATA GOTO L07330
REM L07330 SP_QTY1% = SP_QTY1% + 1%
REM        CONVERT SP_QTY1% TO SP_QTY$, PIC(0000)
REM
REM NOT_REMAKE
REM        DELETE #18
REM NO_EWDSCHED
REM
REM
REM PUT #18, USING L07350, SP_STATUS_DTE$,  /* SPEC ANAL ST     */~
REM                        SP_STATUS$,      /* (PLAN SCH1)      */~
REM                        SP_TYPE$,        /* (PLAN SCH2)      */~
REM                        SP_CUTOFF$,      /* CUT OFF DAY 1-7  */~
REM                        SP_ROUTE$,       /* ROUTE CODE       */~
REM                        SP_CUST$,        /* CUSTOMER CODE    */~
REM                        SP_SO$,          /* SALES ORDER NO.  */~
REM                        SP_LN$,          /* S.O. LINE ITEM NO*/~
REM                        SP_LN_ITEM$,     /* LINE ITEM PIECE  */~
REM                        STR(RM_REC$(),22%,9%),/* BAR CODE    */~
REM                        STR(RM_REC$(),31%,3%),/* RMK NO.     */~
REM                        SP_DUE$,         /* SALES ORDER DUE  */~
REM                        SP_PART$,        /* PART NUMBER      */~
REM                        SP_QTY$,         /* LINE ITEM QTY    */~
REM                        SP_STAT$,        /* (PLAN STAT) PLANN*/~
REM                        SP_USR$,         /* LAST MOD USER ID */~
REM                        SP_DTE$,         /* LAST MOD DATE    */~
REM                        SP_TIME$,        /* LAST MOD TIME    */~
REM                        SP_TEXT$,        /* LINE ITEM TEXT ID*/~
REM                        SP_PRIMARY$,     /* PRIMARY DEPT     */~
REM                        SP_FIL$          /* FILLER AREA      */
REM
REM L07350     FMT CH(6), CH(1), CH(1), CH(2), CH(5), CH(9), CH(8), CH(2),  ~
REM                CH(4), CH(9), CH(3), CH(6), CH(25), CH(4), CH(2), CH(3), ~
REM                CH(6), CH(8), CH(4), CH(3), CH(17)
REM
REM            WRITE #18, EOD GOTO L07360
REM
        return

L07360:     err% = 24%
            gosub err_scrn
        return

        findHldsched
          gosub lookupHldSched
          if hldsch% = 0% and updteCnt% = 0% then goto L07360

        return


        lookupHldSched                                   /* #11 HLDSCHED */
          hldsch% = 0%                /* Read look for Tempered or Lamin */
          init(" ") hldKey$, savHld$, hldType$, hldStatus$, hldrmkStatus$,  ~
                    hldrmkType$, hldLamn$
          str(hldKey$,1%,8%)  = hldso$
          str(hldKey$,9%,2%)  = hldln$
          str(hldKey$,11%,4%) = hldlnitem$
          savHld$ = hldKey$
hldSchedNext:
          read #11, key 2% > hldKey$, using HLDFMT, hldRec$, eod goto noHldKey
HLDFMT:       FMT CH(256)

             hldKey$ = str(hldRec$,27%,16%)
             if str(hldKey$,1%,14%) <> str(savHld$,1%,14%) then goto noHldKey

             hldStatus$ = str(hldRec$,7%,1)
             hldType$   = str(hldRec$,8%,1)
             hldLamn$   = str(hldRec$,114%,1)

             if hldStatus$ = "0" then goto hldSchedNext /* 0 means never ordered*/
                                  /* Only 1 -> tempered and B -> laminate    */
             if hldType$ <> "1" and hldType$ <> "B" then goto hldSchedNext
             hldrmkStatus$ = "0"
             hldrmkType$   = "5"
REM order lamin if sgp - hldLamn$ = "S" or obs% = 1%
             if hldType$ = "B" and (hldLamn$ = "L" and obs% = 0%)            ~
                                                       then goto hldSchedNext
             if hldType$   = "B" then hldrmkType$ = "D"

             gosub writeHldSched       /* Now check and write remake file */
             hldsch% = 1%
             goto hldSchedNext         /* Loop look for other records */
        noHldKey
        return


        writeHldSched                                    /* #18 - HLDSCRMK */
          init(" ") hldscrmkKey$, hldbar$, hldnum$, hldscrmkRec$
          hldbar$ = str(rm_rec$(),22%,9%)
          hldnum$ = str(rm_rec$(),31%,3%)

/* Key Sales Order, Ln, Ln_item, Glass Bar and Num; then status and type */
          str(hldscrmkKey$,1%,14%) = str(hldRec$,27%,14%)
          str(hldscrmkKey$,15%,9%) = hldbar$
          str(hldscrmkKey$,24%,3%) = hldnum$
          str(hldscrmkKey$,27%,1%) = hldrmkStatus$
          str(hldscrmkKey$,28%,1%) = hldrmkType$

          read #18, key 2% = hldscrmkKey$, eod goto noscrmkRec

            goto L07360   /* ERROR RECORD SHOULD NOT BE THERE FOR SAME BARCODE*/

        noscrmkRec
          init(" ") hldtime$
          call "TIME" (hldtime$)
          str(hldscrmkRec$,1%,40%)   = str(hldRec$,1%,40%)
          str(hldscrmkRec$,41%,9%)   = hldbar$
          str(hldscrmkRec$,50%,3%)   = hldnum$
          str(hldscrmkRec$,53%,204%) = str(hldRec$,41%,204%)

          str(hldscrmkRec$,1%,6%)    = date
          str(hldscrmkRec$,7%,1%)    = hldrmkStatus$ /* Status */
          str(hldscrmkRec$,8%,1%)    = hldrmkType$   /* Type */
          str(hldscrmkRec$,16%,1%)   = hldrmkStatus$ /* Status */
          str(hldscrmkRec$,17%,1%)   = hldrmkType$   /* Type */
          str(hldscrmkRec$,53%,1%)   = hldrmkStatus$ /* Status */
          str(hldscrmkRec$,54%,1%)   = hldrmkType$   /* Type */
          str(hldscrmkRec$,86%,4%)   = "0001"        /* Qty    */
          str(hldscrmkRec$,92%,3%)   = userid$
          str(hldscrmkRec$,95%,6%)   = date
          str(hldscrmkRec$,101%,8%)  = hldtime$


          put #18, using HLDSCRMK1, hldscrmkRec$
HLDSCRMK1:     FMT CH(256)
          write #18
        return


        lookup_dt
           dt_rec% = 0%
           init(" ") dt_key$, dt_bar$, hldso$, hldln$, hldlnitem$
           str(dt_key$,1%,8%) = str(rm_rec$(),22%,8%)

           read #1, key 4% = dt_key$, using L07370, dt_key$, eod goto no_dt

L07370:            FMT POS(96), CH(8)
            if str(dt_key$,1%,8%) <> str(rm_rec$(),22%,8%) then goto no_dt

              get #1, using L07380, dt_bar$

L07380:       FMT POS(24), CH(18), POS(64), CH(2), POS(124), CH(9)

              hldso$      = str(dt_bar$,1%,8%)
              hldln$      = str(dt_bar$,9%,2%)
              hldlnitem$  = str(dt_bar$,11%,4%)
           dt_rec% = 1%
        no_dt
        return

REM  LOOKUP_CUST
REM  INIT(" ") READKEY$, DESC$, VF$()
REM  READ #19,KEY = SP_CUST$, EOD GOTO NO_CUST
REM  GET #19, USING L07390   , VF$()
REM
REM L07390 FMT POS(820), 10*CH(20)
REM  SP_CUTOFF$ = STR(VF$(3%),1%,2%) /* CUST DELIVERY CODE   */
REM  SP_ROUTE$ =  STR(VF$(9%),1%,5%)
REM
REM  INIT(" ") READKEY$, DESC$
REM  STR(READKEY$,1%,9%)   = "PLAN CUTO"
REM  STR(READKEY$,10%,15%) = SP_CUTOFF$
REM  READ #2,KEY = READKEY$, USING L12070, DESC$, EOD GOTO NO_CUST
REM
REM  SP_CUTOFF$ = "0" & STR(DESC$,1%,1%)     /* CUT OFF DAY 1 THRU 7 */
REM  NO_CUST
REM  RETURN

        check_temp_stock
          if tempered% = 0% then return
          temp_stock% = 0%
          init(" ") readkey$, desc$, rm_part$, rm_subpart$

          rm_part$ = str(rm_rec$(),125%,25%)
          rm_subpart$ = str(rm_rec$(),255%,20%)             /* (AWD065)  */

          str(readkey$, 1%,9%)  = "TEMPSTOCK"
          str(readkey$,10%,3%) = str(rm_part$,1%,3%)
          str(readkey$,13%,2%) = str(rm_part$,5%,2%)
          str(readkey$,15%,4%) = str(rm_part$,13%,4%)
          str(readkey$,19%,3%) = str(rm_part$,17%,3%)

          read #2,key = readkey$, eod goto not_stock

             temp_stock% = 1%
             str(rm_rec$(),13%,1%)  = "0"
        not_stock
        return

        calc_remake
           c_hrs% = 0% : c_min% = 0% : days% = 0%
           r_hrs% = 0% : r_min% = 0% : tot_min% = 0%
           ct_min% = 0% : rt_min% = 0% : ret% = 0%
           calc_time$  = time
           calc_dte$   = date
           rm_rm_dte$  = str(rm_rec$(),52%,6%)
           call "DATE" addr("G-",rm_rm_dte$,calc_dte$,days%,ret%)

           convert str(calc_time$,1%,2%) to c_hrs%, data goto L06525

L06525:    convert str(calc_time$,3%,2%) to c_min%, data goto L06530

L06530:    if str(calc_time$,5%,2%) > "30" then c_min% = c_min% + 1%
           convert str(rm_rec$(),44%,2%) to r_hrs%, data goto L06535

L06535:    convert str(rm_rec$(),46%,2%) to r_min%, data goto L06540

L06540:    if str(rm_rec$(),48%,2%) > "30" then r_min% = r_min% + 1%
                              /* Convert to total Minutes - complete */
           ct_min% = ( (c_hrs% + (24% * days%)) * 60%) + c_min%
                              /* Convert to Total Minutes - Re-make  */
           rt_min% = ( r_hrs% * 60% ) + r_min%

           tot_min% = (ct_min% - rt_min%)
           r_hrs% = int(tot_min%/60%)
           r_min% = tot_min% - (60% * r_hrs%)
           if glass_receiving% <> 1% then                            ~
                         convert r_hrs% to str(rm_rec$(),61%,2%), pic(00)
           if glass_receiving% <> 1% then                            ~
                         convert r_min% to str(rm_rec$(),63%,2%), pic(00)

           set_flag% = 1%          /* Set Flag to Update */
        return

        update_track                            /* (APCPLNDT) - File   */
     
/* (AWD064) */
           if upd_st$ = " " then goto corruptWrite
           if scr_id$ = " " then goto corruptWrite

           init(" ") errormsg$, prevcode$, dateout$
           call "TIME" (dateout$)
           dt_key$ = sav_key$

           read #1,hold,key = dt_key$, using L06640 , dt_rec$,            ~
                                                     eod goto L06670
L06640:        FMT CH(256)
           
             delete #1

L06670:    dt_dept$ = str(dt_rec$,59%,3%)
           str(dt_rec$,53%,6%)  = date       /* Product Scan Date     */
           str(dt_rec$,64%,2%)  = upd_st$    /* Product Scanned       */
           str(dt_rec$,104%,2%) = scr_shft$  /*                       */
           str(dt_rec$,116%,8%) = dateout$   /* Time Product Scanned  */
/*AWD058*/ call "AWDCOMSB" (dt_rec$,#42,scr_dept$,scr_id$)
           get str(dt_rec$,133%,8), using L06720 , dt_sale /* PRICE     */
L06720:         FMT PD(14,4)
           put #1, using L06640 , dt_rec$
           write #1, eod goto L06850
           
           if dt_sale <> 0.0 then goto L06790     /* Count Charged Items*/
           if str(dt_rec$,215%,1%) = "Y" then goto L06800 /* Skip Parts */
           if str(dt_rec$,214%,1%) <> "0" then goto L06800 /* Skip Sashs*/
           
L06790:    tt_unit% = tt_unit% + 1%               /* Calc Scanned Units */
L06800:    ad_dept$ = str(dt_rec$,42%,3%)         /* Scanning Department*/
           ad_proc$ = str(dt_rec$,45%,2%)         /* Scanning Process   */
           brand$ = str(dt_rec$,243%,2%)                    /* (AWD055) */
/* CR2375 */
           gosub check_cust_brand
           if lblbrand% = 1% then brand$ = "21"
/* (CR1017) +*/          
           prevcode$ = dt_bar$
           If ad_dept$ <> "020" and ad_dept$ <> "023" then goto L068005
           
           init (" ") dt_date$, dt_seq$, dt_part$, dt_partpos4$, ~
                      dt_partpos11$, p_descr$ 
           dt_date$ = str(dt_rec$,47%,6%)                    
           dt_seq$  = str(dt_rec$,111%,5%)                  
           dt_dept$ = str(dt_rec$,59%,3%)                  
           dt_partpos4$  = str(dt_rec$,192%,1%)
           dt_partpos11$ = str(dt_rec$,199%,1%)
           dt_part$ = str(dt_rec$,189%,25%)
           
           init(" ") so_inv$, item_no$
           so_inv$  = str(barcode$,1%,8%)
           item_no$ = str(barcode$,9%,2%)
           gosub lookup_sub_part
           date$ = dt_date$
           call "DATEFMT" (date$)
           scr_line1$ = "ProdDate: " & date$
           scr_line2$ = "Seq     : " & dt_seq$
           scr_line3$ = "Dept    : " & dt_dept$
           
           gosub find_part4_descr
           scr_line4$ = "Fram Clr: " & p_descr$
           
           gosub find_part11_descr  
           if len(dt_part$) < 19 then p_descr$ = dt_part$
           scr_line5$ = "Screen  : " & p_descr$
           
           gosub find_subpart4_descr  
           scr_line6$ = "Hardware: " & p_descr$
 /* (CR1017) - */
 
L068005: 
         gosub update_audit
         gosub ok_scrn
         
/* (AWD055)*/
         convert brand$ to brand%, data goto no_brand
        return
no_brand:
         brand% = 98%
        return
/*(/AWD055)*/
L06850:  err% = 6% : gosub err_scrn
         str(errormsg$,41%,3%) = str(dt_rec$,42%,3%)
        return

REM     UPDATE_REMAKE                              /*  (AWD044)           */
                                                   /* (APCPLNDT) - File   */
            init(" ") errormsg$, prevcode$, dateout$
            call "TIME" (dateout$)
            dt_key$ = sav_key$
            read #1,hold,key = dt_key$, using L06640 , dt_rec$,            ~
                                                     eod goto NO_DT_REMAKE
               delete #1

NO_DT_REMAKE:
            str(dt_rec$,53%,6%)  = date       /* Product Scan Date     */
            str(dt_rec$,64%,2%)  = upd_st$    /* Product Scanned       */
            str(dt_rec$,104%,2%) = scr_shft$  /*                       */
            str(dt_rec$,116%,8%) = dateout$   /* Time Product Scanned  */
            get str(dt_rec$,133%,8), using L06720 , dt_sale /* PRICE     */

/*AWD058*/  call "AWDCOMSB" (dt_rec$,#42,scr_dept$,scr_id$)
            put #1, using L06640 , dt_rec$
            write #1, eod goto BAD_RMK_WRITE
            prevcode$ = dt_bar$
            if dt_sale <> 0.0 then goto  RMK_CNT    /* Count Charged Items*/
            if str(dt_rec$,215%,1%) = "Y" then goto NO_RMK_CNT /* Skip Parts */
            if str(dt_rec$,214%,1%) <> "0" then goto NO_RMK_CNT /* Skip Sashs*/
RMK_CNT:
               tt_unit% = tt_unit% + 1%            /* Calc Scanned Units */
NO_RMK_CNT:
        ad_dept$ = str(dt_rec$,42%,3%)             /* Scanning Department*/
        ad_proc$ = str(dt_rec$,45%,2%)             /* Scanning Process   */
        gosub update_audit
        gosub ok_scrn
        return
BAD_RMK_WRITE:
           err% = 6% : gosub err_scrn
           str(errormsg$,41%,3%) = str(dt_rec$,42%,3%)
        return

        update_shipping                         /* (APCPLNDT) - File   */
/* (AWD064) */
            if upd_st$ = " " then goto corruptWrite
            if scr_id$ = " " then goto corruptWrite


            init(" ") errormsg$, prevcode$, dateout$, dt_key$, dt_rec$,~
                      sav_load$, sav_part$
            hit% = 0%
            dt_sale = 0.00
            call "TIME" (dateout$)
REM STR(DT_KEY$,1%,18%) = BARCODE$        /* (AWD044)  */
            str(dt_key$,1%,23%)  =  sav_key$         /* (AWD044)  */

            read #1,hold,key = dt_key$, using L06990 , dt_rec$,            ~
                                                     eod goto L07210
L06990:        FMT CH(256)
REM            dt_key$ = str(dt_rec$,24%,23%)
REM            if str(dt_key$,1%,18%) <> barcode$ then goto L07120

                                                              /*  (AWD035) */
REM DO I WANT TO UPDATE THE SUPPORT DEPTS????   Don't think so.
REM               DT_DEPT$ = STR(DT_KEY$,19%,3%)
REM               IF DT_DEPT$ = "001" THEN GOTO UPDATE_SHIPPING_NXT
REM               FOR X% = 1% TO SUPP_MAX%
REM                   IF DT_DEPT$ = SUPP_DEPT$(X%) THEN GOTO UPDATE_SHIPPING_NXT
REM               NEXT X%
/* Do not update dept 044 to 13 b/c will make it look complete!! */
REM               IF STR(DT_KEY$,19%,3%) = "044" THEN GOTO UPDATE_SHIPPING_NXT

               delete #1
L07210:

               get str(dt_rec$,133%,8), using L06720 , dt_sale /* PRICE  */
               str(dt_rec$,53%,6%) = date     /* Product Scan Date     */
               str(dt_rec$,64%,2%) = upd_st$  /* Product Scanned       */
               str(dt_rec$,116%,8%)= dateout$ /* Time Product Scanned  */
/*AWD058*/     call "AWDCOMSB" (dt_rec$,#42,scr_dept$,scr_id$)
               put #1, using L06990 , dt_rec$
               write #1, eod goto L07200

               sav_load$ = str(dt_rec$,1%,5%)
               sav_part$ = str(dt_rec$,189%,25%)
               ad_dept$ = str(dt_rec$,42%,3%)      /* Scanning Department*/
               ad_proc$ = str(dt_rec$,45%,2%)      /* Scanning Process   */
               gosub update_audit

               gosub lookup_prd_lb                    /*  (AWD035)  */
               gosub lookup_hinge
               gosub LOOKUP_DRYWALL                   /* (AWD044)   */
               prevcode$ = barcode$           /* Calc Scanned Units */
/* (AWD050) */
               init(" ") so_inv$, item_no$
               so_inv$  = str(barcode$,1%,8%)
               item_no$ = str(barcode$,9%,2%)

               gosub lookup_sub_part

/* (AWD050) */
               gosub print_wood_label                 /*  (AWD035)  */
               gosub received_ok_scrn
               if dt_sale <> 0.00 then tt_unit% = tt_unit% + 1%


REM L07120
REM               PREVCODE$ = BARCODE$
REM               TT_UNIT% = TT_UNIT% + 1%         /* CALC SCANNED UNITS */
REM DON'T UPDATE AUDIT WITH DEPT 106 OR 108 B/C NOT STAGING OR LOADING!
REM               AD_DEPT$ = "106"                     /* STAGING        */
REM               IF SCR_SEL% = 3% THEN AD_DEPT$ = "108" /* LOADING      */
REM               AD_PROC$ = "01"
REM               GOSUB UPDATE_AUDIT
REM               GOSUB LOOKUP_PRD_LB                    /*  (AWD035)  */
REM               GOSUB LOOKUP_HINGE
REM               GOSUB LOOKUP_DRYWALL                   /* (AWD044)   */
REM               GOSUB PRINT_WOOD_LABEL                 /*  (AWD035)  */
REM        GOSUB RECEIVED_OK_SCRN

        return
L07200:    err% = 9% : gosub err_scrn
        return

        lookup_prd_lb                                 /*  (AWD035) -  BEG */
            init(" ") lb_key1$, lb_key$, lb_rec$()
            str(lb_key1$,1%,18%) = barcode$
            str(lb_key1$,19%,3%) = ad_dept$
            str(lb_key1$,22%,2%) = ad_proc$
                                                                 /* (PAR001) */
            read #21, key 1% = lb_key1$, using L35040, lb_rec$(),             ~
                                                       eod goto prd_lb_done
L35040:          FMT 4*CH(256)                                   /* (PAR001) */
            lb_key$  = str(lb_rec$(),1%,35%)
            lb_key1$ = str(lb_rec$(),278%,23%)                   /* (PAR001) */
             gosub calculate_day
        prd_lb_done
        return

        calculate_day
            day% = 9%
            testdate$ =  str(lb_key$,1%,6%)
            call "DATUFMTC" (testdate$)
            call "DAY" addr(str(testdate$,,6%), day%)

            day% = day% - 1%
            if day% = 0% then day% = 7%

            convert day% to day$, pic(#)
        return

        lookup_hinge
            init(" ") table$, code$
            table$ = "HINGE    "
REM CODE$  = STR(DT_REC$,197,2%)
            code$  = str(sav_part$,9%,2%)
            gosub check_code
            if code% <> 1% then return
            p% = 0%
            p% = pos(desc$ = "-")
            hinge$ = str(desc$,1%, p%-1%)

        return

/*CR2375 */
        check_cust_brand
          init(" ") readkey$, desc$
          lblbrand% = 0%
          str(readkey$,1%,9%)   = "LBLBRAND "
          str(readkey$,10%,15%) = str(dt_rec$,124%,9%)
          read #2,key = readkey$, using L12070, desc$, eod goto cust_brand_done
              lblbrand% = 1%
        cust_brand_done
        return


        LOOKUP_DRYWALL                          /* (AWD044)  */
          init(" ") table$, code$, drywall$
          table$ = "PLAN DRYW"
          code$  = str(sav_part$,1%,3%)            /* MODEL */
          gosub check_code
          if code% <> 1% then return
          drywall$ = "DRYWALL"
        return                                 /*  (AWD044)  */

        print_wood_label
            init(" ") arg1$, arg2$, arg3$, arg4$, arg5$, arg6$, arg7$, arg8$, ~
                      arg9$, arg10$, arg11$, arg12$
            arg1$ = sav_load$
            arg2$ = str(lb_rec$(),594%,6%)       /* (PAR001)  */
/* (AWD050) */
/* (AWD056) */
REM IF SPECIALMULL$ = "C" THEN ARG2$ = " N/A  "
                                                 /* (PAR001)  */
            arg3$ = str(prevcode$,6%,3%) & "-" & str(lb_rec$(),634%,2%)
            arg4$ = "DAY " & day$
            arg5$ = hinge$
            arg6$ = drywall$                     /* (AWD044)  */

        /* <AWD062> */
            arg8$ = subpart$                  /* (PAR001)  */
        /* <AWD060> */
/* KEY = SO$16 & SEQ$3  AWD060 */
              init(" ") arg7$
            str(key13$,1,16)  = str(barcode$,1%,8%)
            str(key13$,17,1)  = " "
            str(key13$,18,2) = str(barcode$,9%,2%)
              if str(key13$,18,1) = "0" then str(key13$,18,1) = " "

            read #13, key = key13$, using bnklines, group$,        ~
                                    config$, eod goto no_ws_data
bnklines:   FMT POS(279), CH(1), CH(2)
            if group$ <= "0" then goto no_ws_data
            if str(config$,2,1) <= "0" then goto no_ws_data
            arg7$ = str(config$,2,1) & "," & group$
no_ws_data:
        /* </AWD060> */
/*(AWD056)*/
            if specialmull$ = "C" then arg6$ = "CASING"
            if str(subpart$,1,1) = "4" then arg6$ = "SDL"
            if specialmull$ = "C" and str(subpart$,1,1) = "SDL" ~
                            then arg6$ = "CAS/SDL"
                                                 /* (PAR003)  */
/* (AWD068) */
            arg9$ = series$

            err%, been_here% = 0%
            call "AWDPLB05" (been_here%, arg1$, arg2$, arg3$,        ~
                             arg4$, arg5$, arg6$, arg7$, arg8$,      ~
                             arg9$, arg10$, arg11$, arg12$, #2, err%)

                if err% <> 0% then gosub print_error
                                                 /* Finished   */
            been_here% = 1%
            call "AWDPLB05" (been_here%, arg1$, arg2$, arg3$,       ~
                             arg4$, arg5$, arg6$, arg7$, arg8$,     ~
                             arg9$, arg10$, arg11$, arg12$, #2, 99%)

                if err% <> 0% then gosub print_error
        return
                                                 /* (PAR003)   */
        print_error
            if rf_scan% = 1% then goto print_e2  /* (PAR003)   */
            hdr$     = "***** Label Printing Error *****"
            msg$(1%) = "ERROR OCCURRED WHILE PRINTING LABEL !!!"
            msg$(2%) = "Return Code (AWDPLB05) = "
            msg$(3%) = "Press <ENTER> to Continue, <PF16> to Exit"

            convert err% to str(msg$(2%),26%,2%), pic(#0)
print_e1:   k% = 2%
            call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
                if k%  =  0% then return
                if k% <> 16% then goto print_e1
            return clear all
        goto initialize
print_e2:                                        /* (PAR003)   */
            convert err% to err$, pic(#)

            rf_errormsg$ = "Label Error = (" & err$ & ")"
            gosub'500(fieldnr%)
            return clear all
        goto initialize                          /* (PAR003)   */
                                                 /* (AWD035)END*/
        update_audit
/*(AWD064)*/
            if upd_st$ = " " then goto corruptWrite
            if scr_id$ = " " then goto corruptWrite

                                                           /* (AWD035) */
                /* Don't update more than once for Efficiency Purposes */
            audit% = 0%

REM   SCR_SEL OF 1 = CAME FROM UPDATE_TRACK
REM   SCR_SEL OF 2 = CAME FROM UPDATE_SHIPPING

/* AWD049   IF UPD_ST$ = "12" AND SCR_SEL% = 1% THEN */
            if upd_st$ = "12" then gosub check_previous_audit
            if audit% <> 0% then return

            init(" ") ad_rec$, ad_time$, ad_key$, ad_rec1$
            call "TIME" (ad_time$)
            str(ad_rec$,1%,18%) = barcode$                /* Barcode   */
            str(ad_rec$,19%,6%) = date                    /* Scan Date */
            str(ad_rec$,25%,3%) = ad_dept$                /* Department*/
            str(ad_rec$,28%,2%) = ad_proc$                /* Process   */
            str(ad_rec$,30%,2%) = scr_shft$               /* Shift Code*/
            str(ad_rec$,32%,2%) = upd_st$                 /* Status    */
            str(ad_rec$,34%,18%)= barcode$                /* Barcode   */
            str(ad_rec$,52%,8%) = ad_time$                /* Time Stamp*/
            str(ad_rec$,60%,3%) = scr_id$                 /* User Id   */
            str(ad_rec$,63%,2%) = "  "                    /* Filler    */
            ad_key$ = str(ad_rec$,19%,33%)
            read #5,hold,key = ad_key$, using L07430,ad_rec1$,eod goto L07420

               delete #5
L07420:     put #5, using L07430 , ad_rec$
L07430:       FMT CH(64)
            write #5, eod goto L07460
        return
L07460:    err% = 7% : gosub err_scrn
        return


        check_previous_audit
           audit% = 0%
           ad_key$, ad_rec1$ =  all(hex(00))
           str(ad_key$,1%,18%) = barcode$
        check_audit_nxt
            read #5, key 1% > ad_key$, using L07430, ad_rec1$,           ~
                                                   eod goto no_prev_audit

              if str(ad_rec1$,1%,18%) <> str(barcode$,1%,18%)            ~
                                                   then goto no_prev_audit
               str(ad_key$,1%,33%) = str(ad_rec1$,1%,33%)
               if str(ad_rec1$,25%,3%) <> scr_dept$ then goto check_audit_nxt
               if str(ad_key$,32%,2%) <> "12" then goto check_audit_nxt

            audit% = 1%           /* There has already been a '12' */
        no_prev_audit
        return



        update_ewdplngt
          gt_load% = 0%
REM IF STR(RM_REC$(),32%,2%) = "00" THEN                ~
REM GOSUB UPDATE_GLASS_TRAUMA_ORG
          gosub dataload_gt
          if gt_load% = 0% then return  /* Could Not Load */
          str(gt_rec$,1%,1%)   = "1"   /* Closed         */
          str(gt_rec$,2%,2%)   = "01"
          str(gt_rec$,166%,6%) = date  /* Finished       */
          str(gt_rec$,172%,8%) = time  /* Finished Time  */
          str(gt_rec$,180%,2%) = scr_shft$ /* Finish Shft*/
          str(gt_rec$,182%,3%) = userid$   /* Area-User  */
          gosub calc_time
          gosub dataput_gt
        return

        dataload_gt
          read #8, hold, key 1% = rm_key$, using L07300, gt_rec$,         ~
                                                   eod goto no_dataload_gt

            delete #8
            gt_load% = 1%
        return
        no_dataload_gt
           init(" ") gt_rec$
           str(gt_rec$,1%,1%)  = "0"            /* Status = '0' Open  */
           str(gt_rec$,2%,2%)  = "00"           /* Area = Originator  */
           str(gt_rec$,4%,6%)  = str(rm_rec$(),52%,6%)
           str(gt_rec$,10%,8%) = str(rm_rec$(),44%,8%)
           gosub update_glass_trauma_org
        goto dataload_gt
        return

        calc_time
          init(" ") tt_in$, tt_out$
          tt_in$  = str(gt_rec$,10%,8%)          /* Start Time  */
          tt_out$ = str(gt_rec$,172%,8%)         /* Finish Time */

          convert str(tt_in$,1%,2%) to tt_hr1%, data goto L22010
L22010:
          convert str(tt_in$,3%,2%) to tt_mn1%, data goto L22020
L22020:
          convert str(tt_in$,5%,4%) to tt_sc1%, data goto L22025
L22025:
          convert str(tt_out$,1%,2%) to tt_hr2%, data goto L22030
L22030:
          convert str(tt_out$,3%,2%) to tt_mn2%, data goto L22040
L22040:
          convert str(tt_out$,5%,4%) to tt_sc2%, data goto L22045
L22045:
          if tt_sc1% > 3000% then tt_mn1% = tt_mn1% + 1%
          if tt_sc2% > 3000% then tt_mn2% = tt_mn2% + 1%

                                             /* Convert start and   */
          tt1% = (tt_hr1% * 60%) + tt_mn1%   /* Finish time to      */
          tt2% = (tt_hr2% * 60%) + tt_mn2%   /* Minutes             */

          if tt1% > tt2% then tt2% = tt2% + 1440% /* Correct with    */
                                             /* Minutes for (1) day  */
          tt3% = tt2% - tt1%                 /* Total Minutes to     */
                                             /* Complete Conversion  */
          tt_hr3% = int(tt3%/60%)            /* Total Minutes        */
          tt_mn3% = mod(tt3%,60%)

          convert tt_hr3% to tt_hr$, pic(000)/* Total Hours          */

          convert tt_mn3% to tt_mn$, pic(00) /* Total Minutes        */

          str(gt_rec$,185%,6%)  = tt_hr$ & ":" & tt_mn$
        return


        ok_scrn
        REM *************************************************************~
            * Display This Screen If Barcode is Scanned And No          *~
            * Errors Occur.                                             *~
            *************************************************************
        if rf_scan% = 1% then return                  /* (AWD045)    */
           init(" ") hdr$, errormsg$
           hdr$ = "Load No: XXXXX  Customer Code: XXXXXXXXX"
           str(hdr$,10%,5%) = str(dt_rec$,1%,5%)
           str(hdr$,32%,9%) = str(dt_rec$,124%,9%)
           print at(03,02);hex(84);errormsg$;
           print at(03,17);hex(84);hdr$;
           print at(07,17);hex(84);ps$(1%);
           print at(08,17);hex(84);ps$(2%);
           print at(09,17);hex(84);ps$(3%);
           print at(10,17);hex(84);ps$(4%);
           print at(11,17);hex(84);ps$(5%);
           print at(12,17);hex(84);ps$(6%);
           print at(13,17);hex(84);ps$(7%);
           for i% = 1% to b_max%
               print at(13,75);bell;
           next i%
        REM CALL "PAUSE" ADDR(100%)
        return

        received_ok_scrn
        REM *************************************************************~
            * Display This Screen If Barcode is Scanned And No          *~
            * Errors Occur.                                             *~
            *************************************************************

            init(" ") hdr$, errormsg$
            hdr$ = "Load No: XXXXX  Customer Code: XXXXXXXXX"
            str(hdr$,10%,5%) = str(dt_rec$,1%,5%)
            str(hdr$,32%,9%) = str(dt_rec$,124%,9%)
            print at(03,02);hex(84);errormsg$;
            print at(03,17);hex(84);hdr$;
            print at(07,17);hex(84);ps$(1%);
            print at(08,17);hex(84);ps$(2%);
            print at(09,17);hex(84);ps$(3%);
            print at(10,17);hex(84);ps$(4%);
            print at(11,17);hex(84);ps$(5%);
            print at(12,17);hex(84);ps$(6%);
            print at(13,17);hex(84);ps$(7%);
            for i% = 1% to b_max%
                print at(13,75);bell;
            next i%
        REM CALL "PAUSE" ADDR(100%)

        return

        startover                              /* (AWD045) */
         if rf_scan% = 1% then goto rf_startover
          u3% = 2%
          call "STARTOVR" (u3%)
          if u3% = 1% then return
          errormsg$ = " "
          edit% = 0%
        return clear all
        goto main

        rf_startover                      /* (AWD045) */
          rf_errormsg$ = " "
          edit% = 0%
        return clear all
        goto rf_main                      /* (AWD045) */

        exit_program
          call "SHOSTAT" ("One Moment Please")
        end

        REM *************************************************************~
            *       Production, Staging, Loading Display Screen         *~
            *************************************************************

        deffn'100(fieldnr%)
L07950:     gosub set_screen_2
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), scrn_title$            , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,29), fac(hex(84)), tt_unit$               , ch(24),~
                                                                         ~
               at (05,02), fac(hex(84)), fld$(1%)               , ch(25),~
               at (05,30), fac(lfac$(1%)), barcode$             , ch(18),~
               at (05,50), fac(lfac$(2%)), wandchar$            , ch(01),~
                                                                         ~
               at (05,52), fac(hex(84)), fld$(2%)               , ch(04),~
               at (05,57), fac(hex(84)),   prevcode$            , ch(18),~
                                                                         ~
/*(AWD055)*/   at (07,02), fac(hex(84)), brand$(brand%,1%)      , ch(40),~
/*(CR1017)*/   at (07,43), fac(hex(84)), scr_line1$             , ch(38),~
/*(AWD055)*/   at (08,02), fac(hex(84)), brand$(brand%,2%)      , ch(40),~
/*(CR1017)*/   at (08,43), fac(hex(84)), scr_line2$             , ch(38),~
/*(AWD055)*/   at (09,02), fac(hex(84)), brand$(brand%,3%)      , ch(40),~
/*(CR1017)*/   at (09,43), fac(hex(84)), scr_line3$             , ch(38),~
/*(AWD055)*/   at (10,02), fac(hex(84)), brand$(brand%,4%)      , ch(40),~
/*(CR1017)*/   at (10,43), fac(hex(84)), scr_line4$             , ch(38),~
/*(AWD055)*/   at (11,02), fac(hex(84)), brand$(brand%,5%)      , ch(40),~
/*(CR1017)*/   at (11,43), fac(hex(84)), scr_line5$             , ch(38),~
/*(CR1017)*/   at (12,43), fac(hex(84)), scr_line6$             , ch(38),~
                                                                         ~
               at (14,16), fac(hex(84)), xx$(1%)                , ch(50),~
               at (15,16), fac(hex(84)), xx$(2%)                , ch(50),~
               at (16,16), fac(hex(84)), xx$(3%)                , ch(50),~
               at (17,16), fac(hex(84)), xx$(4%)                , ch(50),~
               at (18,16), fac(hex(84)), xx$(5%)                , ch(50),~
               at (19,16), fac(hex(84)), xx$(6%)                , ch(50),~
               at (20,16), fac(hex(84)), xx$(7%)                , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 1% then L08300
                  gosub startover

L08300:        if keyhit% <> 5% then L08320          /* Calculate Units  */
                  gosub calc_scanned
                  goto L07950

L08320:        if keyhit% <> 7% then L08325          /*  (AWD033)  */
                  gosub scanned_received
                  received% = 1%
                                                     /*  (AWD033)  */

L08325:        if keyhit% <> 8% then L08335          /*  (AWD035)  */
                  gosub wood_remake
                  tt_unit% = 0%
                                                     /*  (AWD035)  */

L08335:        if keyhit% <> 10% then goto L08330
                                                     /* (EWD012) - Track */
                  glstype% = 1%                      /* (IM8022)         */
                  call "APCPLD45" (0%, scr_dept$, scr_shft$, glstype%, #3, #2)
                  goto L07950

                                                     /* (EWD007) -       */
L08330:        if keyhit% <> 11% then goto L08340    /* Glass Lookup     */
                  run$ = "EWDPLN64"
                  gosub Run_Program
                  goto L07950

L08340:        if keyhit% <> 13% then goto L08350    /* (AWD031) */

                  glstype% = 2%                      /* (IM8022)         */
                  call "APCPLD45" (0%, scr_dept$, scr_shft$, glstype%, #3, #2)
                  goto L07950                         /* (AWD031) */
                                                     /* (EWD007) - End   */
L08350:        if keyhit% <> 15% then L08380
                  call "PRNTSCRN"
                  goto L07950

L08380:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_screen_2
            if scr_sel% = 1% then copy pd$() to xx$()
REM            if scr_sel% = 2% then copy st$() to xx$()   /*  (AWD035)  */
REM            if scr_sel% = 3% then copy ld$() to xx$()   /*(AWD035)-Not Use*/
            if scr_sel% = 2% then gosub set_received_data  /*(AWD035) */

            tt_unit$ = "Scanned Units [ XXXXXX ]"
            convert tt_unit% to str(tt_unit$,17%,6%), pic(######)

            init(" ") dateout$
            inpmessage$ = inp_text$(fieldnr%)
            call "TIME" (dateout$)
            lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
            fld$(1%)      = "Barcode Number To Scan  :"
            fld$(2%)      = "Prv:"
            pf$(1%) = "(1)Startover           (7)Receive Glass " &       ~
                      "(11)Glass Search                       "
            pf$(2%) = "                       (8)Wood Remake   " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(5)Calc Units        (10)Glass Analysis " &       ~
                      "(13)Temp Glass         (16)Exit Screen "
            pfkeys$ = hex(01ffffff050807ffff0a0bff0dff0f1000)
                                                                /* (AWD047) */
            if scr_sel% = 2% then str(pf$(1%),24%,18%) = " "
            if scr_sel% = 2% then str(pfkeys$,7%,2%) = hex(ff)

            if scr_sel% = 2% then return

            str(pf$(2%),24%,15%) = " " : str(pfkeys$,11%,2%) = hex(ff)
        return                                           /*  (AWD033)   */

        set_received_data                                /*  (AWD035)   */
          if wood_rmk% = 0% then goto set_up_screen
            xx$(1%) = " "
            xx$(2%) = " "
           xx$(3%) = "Scan Barcode to update status to 11 for Production Remake"
            xx$(4%) = " "
            xx$(5%) = " "
            xx$(6%) = " "
            xx$(7%) = " "
          return

set_up_screen:
REM            gosub calculate_day
            xx$(1%) = " "
            xx$(2%) = "Load    : " & sav_load$
            xx$(3%) = "WS Code : " & str(lb_rec$(),594%,6%) /* (PAR001) */
                                                         /* (PAR001)    */
            xx$(4%) = "Family #: " & str(prevcode$,6%,3%) & " - " & str(lb_rec$(),634%,2%)
            xx$(5%) = "Prd Day : " & day$
            xx$(6%) = "Hinge   : " & hinge$
            xx$(7%) = "Drywall : " & drywall$            /* (AWD044)    */
        return                                           /*  (AWD035)   */

        REM *************************************************************~
            *        Glass and Glass Re-Make Scanning Screen            *~
            *************************************************************

                                                       /* (EWD018)      */
        deffn'200(fieldnr%)                         /* Chg to 12 Digits */

L08700:     gosub set_screen_3


            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), scrn_title$            , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,29), fac(hex(84)), tt_unit$               , ch(24),~
                                                                         ~
               at (05,02), fac(hex(84)), fld$(1%)               , ch(25),~
               at (05,30), fac(lfac$(1%)), rm_bar$              , ch(12),~
               at (05,50), fac(lfac$(2%)), bar_wand$            , ch(01),~
                                                                         ~
               at (06,02), fac(hex(84)), fld$(2%)               , ch(25),~
               at (06,30), fac(lfac$(3%)), rm_reason$           , ch(02),~
               at (06,35), fac(lfac$(4%)), reason_wand$         , ch(01),~
               at (06,40), fac(hex(84)),   reason_d$            , ch(25),~
                                                                         ~
/*AWD043*/     at (07,02), fac(hex(84)), fld$(3%)               , ch(25),~
/*AWD043*/     at (07,30), fac(lfac$(5%)), rm_dept_n$           , ch(03),~
/*AWD043*/     at (07,35), fac(lfac$(6%)), dept_wand$           , ch(01),~
/*AWD043*/     at (07,40), fac(hex(84)),   dept_d$              , ch(25),~
                                                                         ~
/*AWD046*/     at (08,02), fac(hex(84)), gls_status$            , ch(20),~
/*AWD046*/     at (08,30), fac(hex(84)), rm_status$             , ch(03),~
/*AWD046*/     at (08,35), fac(hex(84)), rm_status_d$(rm_status%), ch(30),~
                                                                         ~
               at (14,16), fac(hex(84)), xx$(1%)                , ch(50),~
               at (15,16), fac(hex(84)), xx$(2%)                , ch(50),~
               at (16,16), fac(hex(84)), xx$(3%)                , ch(50),~
               at (17,16), fac(hex(84)), xx$(4%)                , ch(50),~
               at (18,16), fac(hex(84)), xx$(5%)                , ch(50),~
               at (19,16), fac(hex(84)), xx$(6%)                , ch(50),~
               at (20,16), fac(hex(84)), xx$(7%)                , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 1% then L09070
                  gosub startover

L09070:        if keyhit% <> 5% then L09080          /* Calculate Units  */
                  gosub calc_scanned
                  goto L08700

L09080:        if keyhit% <> 6% then L09085
                  gosub get_rmk_reasons
                  goto L08700

L09085:        if keyhit% <> 10% then goto L09090    /* Glass analysis   */
                                                     /* (EWD012) - Track */
                  glstype% = 1%                      /* (IM8022)         */
                  call "APCPLD45" (0%, scr_dept$, scr_shft$, glstype%, #3, #2)
                  goto L08700
                                                     /* (EWD007) Begin   */
L09090:        if keyhit% <> 11% then goto L09100    /* Glass Search     */
                  run$ = "EWDPLN64"
                  gosub Run_Program
                  goto L08700
                                                     /* (EWD007) - End   */

L09100:        if keyhit% <> 13% then goto L09120    /* Glass analysis   */
                                                     /* (AWD031) - Track */
                  glstype% = 2%                      /* (IM8022)         */
                  call "APCPLD45" (0%, scr_dept$, scr_shft$, glstype%, #3, #2)
                  goto L08700
                                                     /* (AWD031)         */
                                                     /* (EWD015) Begin   */
L09120:        if keyhit% <> 14% then goto L09110    /* Glass Search     */
                  run$ = "EWDPLN72"
                  gosub Run_Program
                  goto L08700
                                                     /* (EWD015) - End   */

L09110:        if keyhit% <> 15% then L09150
                  call "PRNTSCRN"
                  goto L08700

L09150:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_screen_3
            copy gl$() to xx$()

            tt_unit$ = "Scanned Units [ XXXXXX ]"
            convert tt_unit% to str(tt_unit$,17%,6%), pic(######)

            init(" ") dateout$
            inpmessage$ = inp_text$(fieldnr%)
            call "TIME" (dateout$)
                                           /* (EWD013)- Set up Screen */
            fld$(1%)      = "Glass Barcode to Scan   :"
            fld$(2%)      = "Glass Remake Reason Code:"
            if scr_sel% = 4% then init(" ") fld$(2%)         /*  (AWD033)  */
            if glass_receiving% = 1% then init(" ") fld$(2%)
            if glass_receiving% = 1% then                                ~
                            fld$(1%) = "Glass Barcode to Receive:"
                                                                 /*  (AWD043) */
            if scr_sel% = 5% then fld$(3%) = "Remake Department       :"

            pf$(1%) = "(1)Startover                            " &       ~
                      "(11)Glass Search       (14)Print Labels"
            pf$(2%) = "                     (6)Remake Reasons  " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(5)Calc Units        (10)Glass Analysis " &       ~
                      "(13)Temp Glass         (16)Exit Screen "
            pfkeys$ = hex(01ffffff0506ffffff0a0bff0d0e0f1000)
            if scr_sel% = 5% then gosub set_rm_status_d  /* (AWD046) */
                                                /* (EWD015)    */
            if scr_sel% = 4% then goto L09200
               str(pf$(1%),64%) = " " : str(pfkeys$,14%,1%) = hex(ff)
                                                /* (EWD015)    */
L09200:        if fieldnr% <> 1% then goto L09430
                                              /* Fieldnr% = 1% & 2% */
                                              /* Barcode            */
               init(" ") rm_reason$, reason_wand$, reason_d$
               lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
               lfac$(3%) = hex(84) : lfac$(4%) = hex(84)
               lfac$(5%) = hex(84) : lfac$(6%) = hex(84)
        return                                     /*  (AWD043)  */
                                                   /* Remake Reason */
L09430:     if fieldnr% <> 3% then goto L09450  /* Fieldnr% = 3% & 4% */
            lfac$(1%) = hex(84) : lfac$(2%) = hex(84)
            lfac$(3%) = hex(81) : lfac$(4%) = hex(99)
            if rm_status$ <> "2" and rm_status$ <> "4"             ~
                                         then lfac$(3%) = hex(84)
            if rm_status$ <> "2" and rm_status$ <> "4"             ~
                                          then lfac$(4%) = hex(84)
            lfac$(5%) = hex(84) : lfac$(6%) = hex(84)
        return
                                                 /* Department */
L09450:     lfac$(1%) = hex(84) : lfac$(2%) = hex(84)
            lfac$(3%) = hex(84) : lfac$(4%) = hex(84)
            lfac$(5%) = hex(81) : lfac$(6%) = hex(99)
        return

        set_rm_status_d                            /* (AWD046) */
              if lookup_dept% = 0% then return    /* (AWD052) glass not found*/


              convert rm_status$ to rm_status%, data goto bad_status1

bad_status1:
              rm_status% = rm_status% + 1%
              gls_status$ = "Glass Status  "

        return                                     /* (AWD046) */

        REM *************************************************************~
            *        Glass and Glass Re-Make Scanning Screen   CR2180   *~
            *************************************************************
        deffn'300(fieldnr%)                         /* Chg to 15 Digits */

L09700:     gosub set_screen_B3


            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), scrn_title$            , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,29), fac(hex(84)), tt_unit$               , ch(24),~
                                                                         ~
               at (05,02), fac(hex(84)), fld$(1%)               , ch(25),~
               at (05,30), fac(lfac$(1%)), rm2bar$              , ch(15),~
               at (05,50), fac(lfac$(2%)), bar_wand$            , ch(01),~
                                                                         ~
               at (06,02), fac(hex(84)), fld$(2%)               , ch(25),~
               at (06,30), fac(lfac$(3%)), rm_reason$           , ch(02),~
               at (06,35), fac(lfac$(4%)), reason_wand$         , ch(01),~
               at (06,40), fac(hex(84)),   reason_d$            , ch(25),~
                                                                         ~
/*AWD043*/     at (07,02), fac(hex(84)), fld$(3%)               , ch(25),~
/*AWD043*/     at (07,30), fac(lfac$(5%)), rm_dept_n$           , ch(03),~
/*AWD043*/     at (07,35), fac(lfac$(6%)), dept_wand$           , ch(01),~
/*AWD043*/     at (07,40), fac(hex(84)),   dept_d$              , ch(25),~
                                                                         ~
/*AWD046*/     at (08,02), fac(hex(84)), gls_status$            , ch(20),~
/*AWD046*/     at (08,30), fac(hex(84)), rm_status$             , ch(03),~
/*AWD046*/     at (08,35), fac(hex(84)), rm_status_d$(rm_status%), ch(30),~
                                                                         ~
               at (14,16), fac(hex(84)), xx$(1%)                , ch(50),~
               at (15,16), fac(hex(84)), xx$(2%)                , ch(50),~
               at (16,16), fac(hex(84)), xx$(3%)                , ch(50),~
               at (17,16), fac(hex(84)), xx$(4%)                , ch(50),~
               at (18,16), fac(hex(84)), xx$(5%)                , ch(50),~
               at (19,16), fac(hex(84)), xx$(6%)                , ch(50),~
               at (20,16), fac(hex(84)), xx$(7%)                , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               if str(rm2bar$,1%,1%) = "." then  ~
                   rm_bar$ = str(rm2bar$,4%,12%) ~
               else                              ~
                   rm_bar$ = rm2bar$

               if keyhit% <> 1% then L09770
                  gosub startover

L09770:        if keyhit% <> 5% then L09780          /* Calculate Units  */
                  gosub calc_scanned
                  goto L09700

L09780:        if keyhit% <> 6% then L09785
                  gosub get_rmk_reasons
                  goto L09700

L09785:        if keyhit% <> 10% then goto L09790    /* Glass analysis   */
                                                     /* (EWD012) - Track */
                  glstype% = 1%                      /* (IM8022)         */
                  call "APCPLD45" (0%, scr_dept$, scr_shft$, glstype%, #3, #2)
                  goto L09700
                                                     /* (EWD007) Begin   */
L09790:        if keyhit% <> 11% then goto L09791    /* Glass Search     */
                  run$ = "EWDPLN64"
                  gosub Run_Program
                  goto L09700
                                                     /* (EWD007) - End   */

L09791:        if keyhit% <> 13% then goto L09792    /* Glass analysis   */
                                                     /* (AWD031) - Track */
                  glstype% = 2%                      /* (IM8022)         */
                  call "APCPLD45" (0%, scr_dept$, scr_shft$, glstype%, #3, #2)
                  goto L09700
                                                     /* (AWD031)         */
                                                     /* (EWD015) Begin   */
L09792:        if keyhit% <> 14% then goto L09793    /* Glass Search     */
                  run$ = "EWDPLN72"
                  gosub Run_Program
                  goto L09700
                                                     /* (EWD015) - End   */

L09793:        if keyhit% <> 15% then L09794
                  call "PRNTSCRN"
                  goto L09700

L09794:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_screen_B3
            copy gl$() to xx$()

            tt_unit$ = "Scanned Units [ XXXXXX ]"
            convert tt_unit% to str(tt_unit$,17%,6%), pic(######)

            init(" ") dateout$
            inpmessage$ = inp_text$(fieldnr%)
            call "TIME" (dateout$)
                                           /* (EWD013)- Set up Screen */
            fld$(1%)      = "Glass Barcode to Scan   :"
            fld$(2%)      = "Glass Remake Reason Code:"
            if scr_sel% = 4% then init(" ") fld$(2%)         /*  (AWD033)  */
            if glass_receiving% = 1% then init(" ") fld$(2%)
            if glass_receiving% = 1% then                                ~
                            fld$(1%) = "Glass Barcode to Receive:"
                                                                 /*  (AWD043) */
            if scr_sel% = 5% then fld$(3%) = "Remake Department       :"

            pf$(1%) = "(1)Startover                            " &       ~
                      "(11)Glass Search       (14)Print Labels"
            pf$(2%) = "                     (6)Remake Reasons  " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(5)Calc Units        (10)Glass Analysis " &       ~
                      "(13)Temp Glass         (16)Exit Screen "
            pfkeys$ = hex(01ffffff0506ffffff0a0bff0d0e0f1000)
            if scr_sel% = 5% then gosub set_rm_Bstatus_d  /* (AWD046) */
                                                /* (EWD015)    */
            if scr_sel% = 4% then goto L09800
               str(pf$(1%),64%) = " " : str(pfkeys$,14%,1%) = hex(ff)
                                                /* (EWD015)    */
L09800:        if fieldnr% <> 1% then goto L09830
                                              /* Fieldnr% = 1% & 2% */
                                              /* Barcode            */
               init(" ") rm_reason$, reason_wand$, reason_d$
               lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
               lfac$(3%) = hex(84) : lfac$(4%) = hex(84)
               lfac$(5%) = hex(84) : lfac$(6%) = hex(84)
        return                                     /*  (AWD043)  */
                                                   /* Remake Reason */
L09830:     if fieldnr% <> 3% then goto L09850  /* Fieldnr% = 3% & 4% */
            lfac$(1%) = hex(84) : lfac$(2%) = hex(84)
            lfac$(3%) = hex(81) : lfac$(4%) = hex(99)
            if rm_status$ <> "2" and rm_status$ <> "4"             ~
                                         then lfac$(3%) = hex(84)
            if rm_status$ <> "2" and rm_status$ <> "4"             ~
                                          then lfac$(4%) = hex(84)
            lfac$(5%) = hex(84) : lfac$(6%) = hex(84)
        return
                                                 /* Department */
L09850:     lfac$(1%) = hex(84) : lfac$(2%) = hex(84)
            lfac$(3%) = hex(84) : lfac$(4%) = hex(84)
            lfac$(5%) = hex(81) : lfac$(6%) = hex(99)
        return

        set_rm_Bstatus_d                            /* (AWD046) */
              if lookup_dept% = 0% then return    /* (AWD052) glass not found*/


              convert rm_status$ to rm_status%, data goto bad_status2

bad_status2:
              rm_status% = rm_status% + 1%
              gls_status$ = "Glass Status  "

        return                                     /* (AWD046) */
                                                      /* (AWD045) -  BEG */
        REM *************************************************************~
            *        Glass and Glass Re-Make Scanning Screen            *~
            *************************************************************
        deffn'201(fieldnr%)
            gosub rf_set_screen_3
            accept                                                       ~
               at (01,02), fac(hex(8c)), rf_pname$              , ch(21),~
               at (01,12), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), rf_errormsg$           , ch(20),~
                                                                         ~
               at (03,02), fac(hex(84)), tt_unit$               , ch(24),~
                                                                         ~
               at (04,02), fac(hex(84)), rf_fld$(1%)            , ch(07),~
               at (04,09), fac(lfac$(1%)), rm_bar$              , ch(09),~
               at (04,19), fac(lfac$(2%)), bar_wand$            , ch(01),~
                                                                         ~
               at (05,02), fac(hex(84)), rf_fld$(2%)            , ch(25),~
               at (05,09), fac(lfac$(3%)), rm_reason$           , ch(02),~
               at (05,19), fac(lfac$(4%)), reason_wand$         , ch(01),~
                                                                         ~
               at (06,02), fac(hex(84)), rf_fld$(3%)            , ch(25),~
               at (06,09), fac(lfac$(5%)), rm_dept_n$           , ch(03),~
               at (06,19), fac(lfac$(6%)), dept_wand$           , ch(01),~
                                                                         ~
               at (07,02), fac(hex(a4)),   rf_inpmessage$       , ch(20),~
               at (08,02), fac(hex(8c)),   rf_pf$(1%)           , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 1% then L09075
                  gosub startover

L09075:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        rf_set_screen_3
            rf_inp_text$(1%) = "Scan GLS Barco"
            rf_inp_text$(3%) = "Enter Rmk Num "
            rf_inp_text$(5%) = "Department    "

            tt_unit$ = "Scn[ XXX ] St[ X ]"
            convert tt_unit% to str(tt_unit$,6%,3%), pic(###)

            str(tt_unit$,16%,1%) = rm_status$

            init(" ") dateout$
            inpmessage$ = rf_inp_text$(fieldnr%)
            call "TIME" (dateout$)

            rf_fld$(1%)      = "Glass: "                  /* (AWD045) */
            rf_fld$(2%)      = "RsnCd: "                  /* (AWD045) */
            rf_fld$(3%)      = "Depar: "                  /* (AWD045) */
            if rm_reason$ = " " then rm_reason$ = "90"
            if scr_sel% = 4% then init(" ") fld$(2%)

            rf_pf$(1%) = "(1)StartOver(2)Exit"
            pfkeys$ = hex(0102ffffffffffffffffffffffffffff00)

            if fieldnr% <> 1% then goto L09435
                                              /* Fieldnr% = 1% & 2% */
                                              /* Barcode            */
               init(" ") rm_reason$, reason_wand$, reason_d$
               lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
               lfac$(3%) = hex(84) : lfac$(4%) = hex(84)
               lfac$(5%) = hex(84) : lfac$(6%) = hex(84)
        return                                     /*  (AWD043)  */
                                                   /* Remake Reason */
L09435:     if fieldnr% <> 3% then goto L09455  /* Fieldnr% = 3% & 4% */
            lfac$(1%) = hex(84) : lfac$(2%) = hex(84)
            lfac$(3%) = hex(81) : lfac$(4%) = hex(99)
                                                    /* (AWD046)   */
            if rm_status$ <> "2" and rm_status$ <> "4"             ~
                                         then lfac$(3%) = hex(84)
            if rm_status$ <> "2" and rm_status$ <> "4"             ~
                                          then lfac$(4%) = hex(84)

            lfac$(5%) = hex(84) : lfac$(6%) = hex(84)
        return
                                                   /* Department */
L09455:     lfac$(1%) = hex(84) : lfac$(2%) = hex(84)
            lfac$(3%) = hex(84) : lfac$(4%) = hex(84)
            lfac$(5%) = hex(81) : lfac$(6%) = hex(99)
        return



                                                      /* (AWD047) -  BEG */
        REM *************************************************************~
            *        Glass and Glass Re-Make Scanning Screen            *~
            *************************************************************
        deffn'202(fieldnr%)
            gosub rf_set_screen_4
            accept                                                       ~
               at (01,02), fac(hex(8c)), rf_pname$              , ch(21),~
               at (01,12), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), rf_errormsg$           , ch(20),~
                                                                         ~
               at (03,02), fac(hex(84)), tt_unit$               , ch(24),~
                                                                         ~
               at (04,02), fac(hex(84)), rf_fld$(1%)            , ch(05),~
               at (04,08), fac(lfac$(1%)), rm_bar$              , ch(12),~
               at (05,02), fac(lfac$(2%)), bar_wand$            , ch(01),~
                                                                         ~
               at (07,02), fac(hex(a4)),   rf_inpmessage$       , ch(20),~
               at (08,02), fac(hex(8c)),   rf_pf$(1%)           , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 1% then L09180
                  gosub startover

L09180:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        rf_set_screen_4
            rf_inpmessage$ = "Scan Bcd to Receive"
            rf_inp_text$(1%) = "Scan GLS Recei"
            tt_unit$ = "Scn[ XXX ] St[ X ]"
            convert tt_unit% to str(tt_unit$,6%,3%), pic(###)
            init(" ") dateout$
            inpmessage$ = rf_inp_text$(fieldnr%)
            call "TIME" (dateout$)

            rf_fld$(1%)      = "RCV: "
            rf_pf$(1%) = "(1)StartOver(2)Exit"
            pfkeys$ = hex(0102ffffffffffffffffffffffffffff00)
            init(" ") rm_reason$, reason_wand$, reason_d$
            lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
            lfac$(3%) = hex(84) : lfac$(4%) = hex(84)
            lfac$(5%) = hex(84) : lfac$(6%) = hex(84)
        return
                                                /* (AWD047) - END */



                                                /*  (AWD045) - END */

        utility_scan
REM           rpt% = 0%                            /* (AWD036)  */
REM           call "APCPLB40" (rpt%)               /* (AWD036)  */
                  run$ = "APCPLN40"                /* (AWD036)  */
                  gosub Run_Program                /* (AWD036)  */
        return

        calc_scanned
           if scr_sel% = 4% or scr_sel% = 5% then goto calc_glass

           yy% = 1%                      /* For Specified Shift        */
           convert scr_shft$ to yy%, data goto L09570
L09570:
           tt_unit% = 0%                 /* Clear Current Units        */
           sc_dte$ = date                /* Todays Date                */
           gosub adjust_date
           sc_load$ = "ALL  "            /* Default All Loads          */
           ed_load$ = "ALL  "            /* Default All Loads          */
           p_screen% = 1%                /* No Screen Display          */
           p_scan% = 1%                  /* Scanned Products Only      */
           if scr_sel% = 2% then p_scan% = 2%      /* (AWD045)         */
           p_flg% = 0% : p_max% = 0%     /* Load Products Array        */
           call "APCPLC40" ( scr_dept$,  /* Specified Department Code  */~
                             sc_dte$,    /* Specified Production Date  */~
                                         /* for a Production Day-Begin */~
                             scr_shft$,  /* Shift Code or (AL) = All   */~
                             sc_load$,   /* Production Load or (ALL)   */~
                             ed_load$,   /* Ending Load Number         */~
                             p_mod$(),   /* Department Products        */~
                             p_unt%(),   /* Product Units              */~
                             p_unts%(),  /* Sample Units Only          */~
                             p_untss%(), /* Charge Sashs Only          */~
                             p_untpp%(), /* Charge Parts Only          */~
                             p_val(),    /* Product Dollar Value       */~
                             p_mrp(),    /* (5) Costing Buckets        */~
                             p_max%,     /* Max Number of Products     */~
                             p_flg%,     /* 0% = Load, 1% = No Load    */~
                             p_scan%,    /* 0%=Not Scanned, 1%=Scanned */~
                             p_screen%,  /* 0% = Yes, 1% = No - Display*/~
                             #4,         /* (APCPLNDP) Master Dept.    */~
                             #1,         /* (APCPLNDT) Prod. Tracking  */~
                             #5,         /* (APCPLNAD) Prod. Tracking  */~
                             #2 )        /* (GENCDSIN) Master Code Tab */

           for kk% = 1% to p_max%
               tt_unit% = tt_unit% + p_unt%(kk%,yy%)
           next kk%
           if err% <> 0% then goto L09930
        return
L09930:     err% = 17% : gosub err_scrn
        return

        adjust_date
            err% = 0%
            if yy% <> 3% then return          /* Only Adjust 3rd Shift */
               j1% = 0% : hr% = 0% : j2% = 0% : leap% = 0% : x% = 0%
               init(" ") ad_time$, jdate$, ap$
               call "TIME" (ad_time$)
               convert str(ad_time$,1%,2%) to hr%, data goto L10060
L10060:
               ap$ = str(ad_time$,7%,2%)      /* Store AM or PM        */
               if ap$ = "PM" then return      /* Today is Curr Prod DTE*/
               if hr% > 6% then err% = 17%    /* Shift is not Valid    */
               if err% <> 0% then return      /* Set Error Flag        */

               call "DATE" addr("GJ", sc_dte$, jdate$, x%) /*Get Julian*/
               call "DATJULCV" (jdate$)            /* jdate$ = YYYYDDD  */
               convert str(jdate$,1%,4%) to j1%, data goto L10130/*Year */
L10130:
               convert str(jdate$,5%,3%) to j2%, data goto L10150/* Day */
L10150:
               j2% = j2% - 1%                 /* Subtract 1 Day        */
                                              /* Check Previous Year   */
               if mod(j1%-1%,4) = 0 then leap% = 1% /* For Leap Year   */
               if j2% > 0% then goto L10240
                  j2% = 365%                  /* Set Date for Prev Year*/
                  if leap% = 1% then j2% = 366%
                  convert (j1% - 1%) to str(jdate$,1%,4%), pic(0000)

L10240:        convert j2% to str(jdate$,5%,3%), pic(000)
               CALL "DATJULCV" (jdate$)    /* Packed in the saddle again */
               call "DATE" addr("JG",jdate$, sc_dte$, x%)
                                           /* Adjusted Production Date   */
        return

        calc_glass                         /* (EWD011) Fix So that Units */
            tt_unit% = 0% : cnt% = 0%      /* are Calculated By Shift    */
            init(" ") rm_key2$, rm_txt$, sav_dte$, trk$, sav_userid$

            sav_userid$ = userid$          /* (EWD012) - save user       */
            if userid$ = "GLS" then                                        ~
               userid$ = "OV" & str(scr_shft$,3%,1%)
                                           /* (PAR002)                   */
            if userid$ = "NGL" then                                        ~
               userid$ = "ON" & str(scr_shft$,3%,1%)

            check_trk$ = str(userid$,3%,1%) /* (EWD012) Save Track Code  */
            yy% = 1%                       /* For Specified Shift        */
            convert scr_shft$ to yy%, data goto L10250
L10250:
            sc_dte$ = date                  /* Todays Date                */
            gosub adjust_date
            sav_dte$ = sc_dte$
            rm_txt$ = "Units Checked = [XXXXXX]"
            call "SHOSTAT" ("Calculating Glass Units")
            str(rm_key2$,1%,6%) = sav_dte$
        calc_glass_nxt
            read #3,key 1% > rm_key2$, using L10390, rm_key2$, rm_shft$, ~
                                           trk$, eod goto calc_glass_done
L10390:        FMT POS(7), CH(27), POS(42), CH(2), POS(65), CH(1)
            cnt% = cnt% + 1%
            if mod(cnt%,50%) <> 0 then goto L10450
               convert cnt% to str(rm_txt$,18%,6%), pic(######)

               print at(04,29);rm_txt$;
L10450:     if str(rm_key2$,1%,6%) <> sav_dte$ then goto calc_glass_done
            if str(rm_key2$,7%,1%) <> "2" then goto calc_glass_nxt
            if scr_shft$ <> rm_shft$ then goto calc_glass_nxt
            if check_trk$ <> trk$ then goto calc_glass_nxt
                                                  /* (EWD011) By shift */
               tt_unit% = tt_unit% + 1%
               goto calc_glass_nxt
        calc_glass_done
            cnt% = 0%
            userid$ = sav_userid$                 /* (EWD012)          */
        return

        err_scrn                    /* Display this Message for Errors */
        if err_check% = 1% then goto skip_error
            errormsg$ = err$(err%)
                                                   /*  (AWD045)  */
            if rf_scan% = 1% then goto rf_err_scrn


            print at(03,02);hex(84);"                  ";
            print at(03,17);hex(84);her$(err%);
            print at(07,17);hex(84);ee$(1%);
            print at(08,17);hex(84);ee$(2%);
            print at(09,17);hex(84);ee$(3%);
            print at(10,17);hex(84);ee$(4%);
            print at(11,17);hex(84);ee$(5%);
            print at(12,17);hex(84);ee$(6%);
            print at(13,17);hex(84);ee$(7%);
            for i% = 1% to b_max%
                print at(13,75);bell;
            next i%
        REM CALL "PAUSE" ADDR(100%)
            if err% = 10% or err% = 11% then gosub gls_error_prompt

skip_error: err% = 0%
        return

        rf_err_scrn
            rf_errormsg$ = rf_err$(err%)
            gosub'500(fieldnr%)
        return



        gls_error_prompt
           comp% = 2%

           hh$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = "E r r o r - - See Glass Supervisor -  Status:  " & ~
                           rm_status_d$(rm_status%+1%)
           msg$(2%) = errormsg$
           msg$(3%) = "Press PF10 To Continue."
           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%))
           if comp% <> 10% then goto gls_error_prompt
       return

        check_selection

           init(" ") upd_st$
           code% = 0%
           convert scr_sel$ to scr_sel%, data goto L11060

           if scr_sel% < 1% or scr_sel% > 8% then goto L11060

              if scr_sel% = 3% then goto L11060             /* (AWD035) */
                                   /* Only used by glass Dept (EWD001) */
                                                     /* (PAR002)       */
              if userid$ <> "SCN" and userid$ <> "NSC" then goto L11050

                 if scr_sel% = 4% then goto L11065
                                                     /* (EWD012) OV1   */
L11050:       if userid$ = "GLS" then goto L11052    /* Temporary      */
              if userid$ = "NGL" then goto L11052    /* (PAR002)       */

                                                     /*  (AWD038)      */
REM              if str(userid$,1%,2%) <> "OV" then goto L11055
              if str(userid$,1%,2%) <> "OV" and userid$ <> "RCV" and    ~
                 str(userid$,1%,2%) <> "ON" and userid$ <> "RCN"        ~
                                                        then goto L11055

L11052:          if scr_sel% < 4% then goto L11068
                 if scr_sel% > 5% then goto L11068   /* (EWD012) - Fix */
                                                     /* (EWD001) - End */
L11055:       scr_sel_d$ = str(scr$(3% + scr_sel%),9%,30%)
              if scr_sel% = 1% then str(scr_sel_d$,26%,3%) = scr_dept$
                                                     /* (AWD044) */
              if scr_sel% = 2% then str(scr_sel_d$,26%,3%) = scr_dept$
              scrn_title$ = scr_sel_d$
              if scr_sel% = 3% then str(scrn_title$,29%,11%) =           ~
                                                      "Load= "&scr_load$
              code% = 1%                           /* (EWD004) - Begin */
REM        on scr_sel% goto prod, stagged, loaded, glass, gls_rmk, rga,     ~
                               log_maint, hold_area

           on scr_sel% goto prod, wood_rec,  prod, glass, gls_rmk, rga,  ~
                               log_maint, hold_area

           goto L11060

        prod
              upd_st$ = "12" : upd_st% = 12%    /* Production Complete */
              return

        wood_rec
              upd_st$ = "13" : upd_st% = 13%   /* Received into Wood Surround*/
              return

        stagged
              upd_st$ = "14" : scr_dept$ = "106"/* Staged Complete    */
              upd_st% = 14%
              return

        REM loaded
        REM      upd_st$ = "16"
        REM      scr_dept$ = "108"/* Loaded              */
        REM      upd_st% = 16%
        REM      return
        glass
              upd_st$ = " 2" : scr_dept$ = "110"/* Completed Glass     */
              upd_st% = 2%                      /* (EWD013) Completed  */
              return
        gls_rmk
              upd_st$ = " 1" : scr_dept$ = "112"/* Scheduled Re-make   */
              upd_st% = 1%                      /* (EWD013) Scheduled  */
           return
        rga
              upd_st$ = "50" : scr_dept$ = "114"  /* RGA Scanning      */
              upd_st% = 50%
           return
        log_maint
              upd_st$ = "90" : scr_dept$ = "116"  /* Log Maint-Undelv  */
              upd_st% = 90%
           return
        hold_area
              upd_st$ = "91" : scr_dept$ = "116"  /* Hold Area-Undelv  */
              upd_st% = 91%
        return
                                                  /* (EWD004) - End    */
L11060:    if rf_scan% = 1% then goto L11071      /* (AWD045)   */
           errormsg$ = "(Error) - Invalid Scanning Selection?"
           goto L11070                            /* (EWD001)          */
L11065:    if rf_scan% = 1% then goto L11080      /* (AWD045)   */
           errormsg$ = "(Error) - Only Valid for Glass Scanning?"
           goto L11070
L11068:    if rf_scan% = 1% then goto L11085      /* (AWD045)   */
           errormsg$ = "(Error) - Only Valid for Production Scanning"
L11070:    gosub error_prompt                     /* (EWD001)          */
           code% = 0%
        return

                                                  /* (AWD045)   */
L11071:    rf_errormsg$ = "Invalid Selection"
           goto L11090
L11080:    rf_errormsg$ = "Only Valid for Gls Scn"
           goto L11090
L11085:    rf_errormsg$ = "Only Valid for Prd Scn"

L11090:    gosub'500(fieldnr%)
        return
                                                     /* (AWD045) - END */

                                  /* (AWD030)                           */
        check_reason              /* Codes "01" TO "08" Vinyl Production*/
           table$ = "PLAN REMK"   /*       "11" to "18" Glass House     */
                                  /*       No Longer any repair         */
           init(" ") code$
           code$  = rm_reason$  : rm_reason% = 99%
           rm_rmk_flag$ = "1"      /*(SR79102)*/
                                  /* (AWD039) Special Shapes Remakes    */
                                  /* C0 to C6  Codes                    */
           if str(rm_rec$(),249%,3%) = "043" and str(rm_reason$,1%,1%) = "C" ~
                                  then goto L11175

           convert rm_reason$ to rm_reason%, data goto L11150

L11150:    if userid$ = "GLS" then goto L11152   /* Temporary           */
           if userid$ = "NGL" then goto L11152   /* (PAR002)            */
                                                 /*   (AWD038)          */
REM           IF STR(USERID$,1%,2%) <> "OV" THEN GOTO L11160
                                                     /* (AWD045)  */
                                                     /* (PAR002)  */
           if str(userid$,1%,2%) <> "OV" and str(userid$,1%,2%) <> "GF"  ~
                   and userid$ <> "RCV"  and str(userid$,1%,2%) <> "ON"  ~
                   and userid$ <> "RCN"  and str(userid$,1%,2%) <> "GN"  ~
                                                         then goto L11160
                                                     /* (PAR002)  */
                                                 /* Valid for Glass house */
                                                 /*      (EWD032)         */
L11152:                                         /* Glass Remakes    */
REM            if (rm_reason% > 10% and rm_reason% < 19%) or              ~
                                    rm_reason% = 20% then goto L11170
REM (SR79102)  if (rm_reason% > 10% and rm_reason% < 24%) then goto L11170
        if (rm_reason% > 10% and rm_reason% < 21%) then goto L11155
/* (SR79102)IF RM_REASON% = 45% THEN GOTO L11170
            IF RM_REASON% = 52% THEN GOTO L11170
            IF RM_REASON% = 58% THEN GOTO L11170
            IF RM_REASON% = 59% THEN GOTO L11170
            IF RM_REASON% = 76% THEN GOTO L11170
            IF RM_REASON% = 77% THEN GOTO L11170
            IF RM_REASON% = 78% THEN GOTO L11170
*/
                                                      /* (AWD041) */
               if rm_reason% > 89% and rm_reason% < 100% then goto L11155
L11155:        rm_rmk_flag$ = "0"      /*(SR79102) glass indicator*/
            if rm_rmk_flag$ = "0" then goto L11170

              rm_reason$ = "99"                  /* Not Valid for Glass */


                                                 /* Check Production    */
L11160:    REM IF RM_REASON% > 10% THEN RM_REASON$ = "99"
REM valid external reason codes are 1-10, 21, 23, 32, 45, 52, 58, 59, 76, 77
           if rm_reason% <= 10% then goto L11170
           if rm_reason% = 21% then goto L11170
           if rm_reason% = 22% then goto L11170
           if rm_reason% = 23% then goto L11170
           if rm_reason% = 32% then goto L11170
           if rm_reason% = 45% then goto L11170
           if rm_reason% = 52% then goto L11170
           if rm_reason% = 58% then goto L11170
           if rm_reason% = 59% then goto L11170
           if rm_reason% = 76% then goto L11170
           if rm_reason% = 77% then goto L11170
           if rm_reason% = 78% then goto L11170


           rm_reason$ = "99"

                                                 /* Valid 1 thru 8      */
L11170:    if scr_sel% = 5% and rm_reason% = 0% then rm_reason$ = "99"
                                                 /* (EWD008) - Not Valid*/
L11175:       gosub check_code                   /* (AWD038)            */
              if rm_reason$ = "99" then code% = 0%
              if code% = 0% then goto L11220
                 reason_d$ = desc$
        return
L11220:    errormsg$ = "(Error) - Invalid Glass Reason Code for Re-Makes?"
           gosub error_prompt
           code% = 0% : check% = 0%
        return                                   /* (AWD030)           */

                                                 /* (EWD005) - Begin   */
        check_specials                           /* Tempered Glass and */
REM TEMPERED% = 0%                        /*     (AWD031)       */
REM LAMINATE% = 0%                        /*     (IM8022)       */
          rm_part$ = str(rm_rec$(),125%,25%)    /* Special Lighting   */
          rm_gls$  = str(rm_part$,5%,2%)        /* Glass Type         */
          rm_lit$  = str(rm_part$,7%,2%)        /* Liting             */
          if glstype$ <> " " then goto checkOBS
          table$ = "PLAN TEMP"
          code$  = rm_gls$                      /* Check Tempered Gl  */
          gosub check_code
           if code% = 1% then tempered% = 1%
/* (AWD063) */
/* Allow all patio remake scanning */
REM IF STR(RM_PART$,1%,1%) = "3" THEN CODE% = 0%
REM IF CODE% = 1% THEN TEMPERED% = 1%     /*    (AWD031)        */
REM IF CODE% = 1% AND FT% = 16% THEN TEMPERED% = 1%     /*(AWD066)*/
/*(IM8022) + */
          table$ = "PLAN LAMN"
          code$  = rm_gls$                      /* Check Tempered Gl  */
          gosub check_code
           if code% = 1% then laminate% = 1%
/*(IM8022) - */
checkOBS:
          obs% = 0%
          table$ = "OBS GED"
          code$  = rm_gls$                      /* Check Tempered Gl  */
          gosub check_code
           if code% = 1% then obs% = 1%

REM IF TEMPERED% = 1% THEN CODE% = 0%
/*(AWD063) */
REM IF STR(RM_PART$,1%,1%) = "3" THEN GOTO CHECK_PATIO_TEMP
REM IF STR(RM_REC$(),249%,3%) = "104" OR            ~
REM STR(RM_REC$(),249%,3%) = "043" THEN TEMPERED% = 0%
REM IF TEMPERED% = 0% THEN CODE% = 1%
REM IF CODE% = 1% THEN GOTO L11250

        check_spec_lit:                        /* Check Special Lit  */
          if rm_gls$ = "89" or rm_gls$ = "99" then goto L11260

          if rm_lit$ = "59" then goto L11260 
          if rm_lit$ = "83" or rm_lit$ = "84" then goto L11260
          if rm_lit$ = "85" or rm_lit$ = "86" then goto L11260
          if rm_lit$ = "87" or rm_lit$ = "99" then goto L11260
           code% = 1%
        return
           errormsg$ = "(Error) - Cannot Scan Tempered Glass??"
           goto L11270

L11260:    errormsg$ = "(Error) - Cannot Scan Special Liting??"
L11270:    gosub error_prompt
           code% = 0% : check% = 0%
        return
        check_patio_temp                                 /* (AWD031) */
             if str(rm_part$,1%,3%) = "315" then return
             if str(rm_part$,1%,3%) = "316" then return
             if str(rm_part$,1%,3%) = "335" then return
             if str(rm_part$,1%,3%) = "336" then return
             tempered% = 0%
        return                                           /* (AWD031) */

                                                   /* (EWD005) - End   */
        check_shift
           table$ = "PLAN SHFT"
           code$  = scr_shft$
           gosub check_code
           if code% = 0% then goto L11330
              scr_shft_d$ = desc$
        return
L11330:    if rf_scan% = 1% then goto L11335
           errormsg$ = "(Error) - Invalid Shift Selection??"
           gosub error_prompt
        return
L11335:    rf_errormsg$ = "Invalid Shift"         /* (AWD045)   */
           gosub'500(fieldnr%)
        return

        check_process
           table$ = "PLAN PROC"
           code$  = scr_proc$
           gosub check_code
           if code% = 0% then goto L11440
              scr_proc_d$ = desc$
        return
L11440:    if rf_scan% = 1% then goto L11445
           errormsg$ = "(Error) - Invalid Process Selection??"
           gosub error_prompt
        return
L11445:    rf_errormsg$ = "Invalid Process"       /* (AWD045)  */
           gosub'500(fieldnr%)
        return

        check_dept
           init(" ") desc$
           supp_flag% = 0%
           if scr_dept$ > "095" and scr_dept$ <> "101" then goto L11610
           if scr_sel% <> 1% and scr_sel% <> 2% then goto L11580
           if scr_sel% = 2% then scr_dept$ = "044"
           table$ = "PLAN DEPT"
           code$  = scr_dept$
           gosub check_code
           if code% = 0% then goto L11580
              scr_dept_d$ = desc$

            for i% = 1% to supp_max%             /* Set Flag for Support */
                if scr_dept$ = supp_dept$(i%) then supp_flag% = 1%
            next i%
        return
L11580:    if rf_scan% = 1% then goto L11585
           errormsg$ = "(Error) - Invalid Department Selection??"
           gosub error_prompt
        return
L11585:    rf_errormsg$ = "Invalid Department"          /* (AWD045) */
           gosub'500(fieldnr%)
        return

L11610:    if scr_dept$ = "106" then scr_dept_d$ = "Staging Department"
           if scr_dept$ = "108" then scr_dept_d$ = "Loading Department"
           if scr_dept$ = "110" then scr_dept_d$ = "Glass Department"
           if scr_dept$ = "112" then scr_dept_d$="Glass Remake Department"
           if scr_dept$ = "114" then scr_dept_d$="RGA / RG Scanning      "
           if scr_dept$ = "116" then scr_dept_d$="Undelivered Goods Areas"
           if len(scr_dept_d$) < 3 then goto L11580
           code% = 1%
        return

        check_user
           code% = 0%
           if scr_id$ = "XXX" then goto L11770
           init(" ") scr_id_d$
           read #6,key = scr_id$, using L11750, scr_id_d$,eod goto L11770
L11750:       FMT POS(4), CH(30)
           code% = 1%
        return
L11770:    if rf_scan% = 1% then goto L11775
           errormsg$ = "(Error) - Invalid User ID, 'ID' is Required?"
           gosub error_prompt
           init(" ") scr_id$, scr_id_d$
        return
L11775:    rf_errormsg$ = "Invalid User ID"              /* (AWD045)  */
           gosub'500(fieldnr%)
           init(" ") scr_id$, scr_id_d$
        return

        check_load
           code% = 0%
           if scr_dept$ <> "108" then goto L11960
           if userid$ = "BYP" then goto L11960  /* (EWD002) - 05/26/98 */
                                                /*  (EWD027)  - BEG    */
              convert scr_load$ to scr_load%, data goto L11930

              convert scr_load% to scr_load$, pic(00000)
              goto L11940
                                               /* Alpha or Stock Loads */
L11930:       convert str(scr_load$,2%,4%) to scr_load%, data goto L11920

              convert scr_load% to str(scr_load$,2%,4%), pic(0000)
L11940:
                                                /*  (EWD027)  - END    */
              read #7,key = scr_load$, using L11890, scr_load_d$,         ~
                                                    eod goto L11920
L11890:          FMT POS(16), CH(30)
              code% = 1%
        return
L11920:   errormsg$ = "(Error) - Invalid Load Number. Load No. Required?"
          gosub error_prompt
          init(" ") scr_load$, scr_load_d$
        return
L11960:   code% = 1%
          scr_load$ = "00000"
          scr_load_d$ = "N/A - Not Applicable"
        return

        check_code
           code% = 0%
           init(" ") readkey$, desc$
           str(readkey$,1%,9%)   = table$
           str(readkey$,10%,15%) = code$
           read #2,key = readkey$, using L12070, desc$, eod goto L12090
L12070:       FMT POS(25), CH(30)
           code% = 1%
L12090: return

        display_codes
           call "APCPLN1B" ( table%, #2)
        return

        check_errors
           init(" ") dt_key$
           str(dt_key$,1%,18%) = barcode$
           read #1,key > dt_key$, using L12190,dt_key$,eod goto L12270
L12190:      FMT POS(24), CH(23)
           if barcode$ <> str(dt_key$,1%,18%) then goto L12270
           str(dt_key$,19%,3%) = scr_dept$
           str(dt_key$,22%,2%) = " "
           read #1,key > dt_key$, using L12190, dt_key$, eod goto L12290
           if str(dt_key$,19%,3%) <> scr_dept$ then goto L12310
           err% = 4%                 /* Not Valid for Dept and Process */
        return
L12270:    err% = 1% : return                  /* Barcode Not on File? */
L12290:    err% = 2% : return                  /* Not on file for Dept */
L12310:    err% = 3% : return                  /* On File for Dept ??  */
        return

        remake_display
           return                      /* (EWD024) - Turn off Disp  */
           
           
           remake_cnt% = 0%
           init(" ") gt_key$
           gt_key$ = all(hex(00))
           str(gt_key$,1%,3%) = "001"
        remake_display_nxt
           read #8,key > gt_key$, using L13000, gt_key$,                 ~
                                             eod goto remake_display_done
L13000:       FMT CH(26)
           if str(gt_key$,1%,3%) <> "001" then goto remake_display_done
              remake_cnt% = remake_cnt% + 1%
              goto remake_display_nxt
        remake_display_done

           comp% = 2%
           hh$  = "********* Re-Make Glass Status *********"
           msg$(1%) = "Y o u r   R e - M a k e  G l a s s   W i l l "
           msg$(2%) = "b e   r e a d y   i n  [ xxxx ] m i n u t e s"
           msg$(3%) = "Press Any Key To Continue."
           convert (remake_cnt% * 7%) to str(msg$(2%),26%,4%), pic(####)

           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%))
        return

        error_prompt
           if rf_scan% = 1% then return
           comp% = 2%
           hh$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_error
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return

        Run_Program:
           return% = 0% : comp% = 0%
           init(" ") rlib$, rvol$
           call "PROCLINK" (run$, rlib$, rvol$, return%, comp%)
        return

        error_display
           comp% = 2%
           hh$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press PF(10) Key, To Continue."
           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%))
           if comp% <> 10% then goto error_display
        return

        check_data_override

            err% = 0%
            check% = 0% : dt_st% = 0%           /* (APCPLNDT) - FILE   */
            init(" ") dt_key$, errormsg$, dt_bar$, dt_rec$, sav_key$
            str(dt_key$,1%,18%) = barcode$      /* Set Barcode Value   */
L14000:
            read #1,key > dt_key$, using L14050 , dt_key$, dt_st$,       ~
                                                          eod goto L15020
L14050:        FMT POS(24), CH(23), POS(64), CH(2)

                                                /* Check for Barcode     */
            if str(dt_key$,1%,18%) <> barcode$ then goto L15020
                                                /* Check for Dept already*/
                                                /* Logged In             */
            if scr_dept$ = str(dt_key$,19%,3%) then goto L15000

            for i% = 1% to supp_max%            /* Skip Support Depts    */
                if str(dt_key$,19%,3%) = supp_dept$(i%) then goto L14000
            next i%
                                                /* Production Dept       */
L15000:     convert dt_st$ to dt_st%, data goto L15020

            dt_bar$ = barcode$
            if dt_st% > 11% then goto L15070     /* Already Scanned */
               check% = 1%
               sav_key$ = dt_key$

        return
L15020:     gosub check_errors : gosub err_scrn
        return
L15070:     err% = 5% :  gosub err_scrn
        return
                                                /* (AWD029)              */

                                                /* Support departments */
                                                /* less than '090'     */
        load_support
            init(" ") supp_dept$()
            supp_max% = 0%
            readkey$ = all(hex(00))
            str(readkey$,1%,9%) = "PLAN SUPP"
            read #2,key > readkey$, using L16000, readkey$,            ~
                                                 eod goto load_support_done
            goto L16010
        load_support_next
            read #2, using L16000, readkey$, eod goto load_support_done
L16000:         FMT CH(24)
L16010:     if str(readkey$,1%,9%) <> "PLAN SUPP" then goto load_support_done
               if str(readkey$,10%,3%) > "090" then load_support_done
                  supp_max% = supp_max% + 1%
                  supp_dept$(supp_max%) = str(readkey$,10%,3%)
                                                       /* Load Dept   */
               goto load_support_next
        load_support_done
        return

        scanned_received
           glass_receiving% = 1%
           received% = 1%
           gosub glass_scan
           glass_receiving% = 0%
           if rf_scan% <> 1% then return
           if keyhit% = 2% then return
        goto scanned_received
        return

        check_dept_glass
           err% = 0%
           if str(rm_rec$(),249%,3%) = scr_dept$ then return

           table$ = "GLASSCROS"
           code$  = scr_dept$
           gosub check_code
           for x% = 1% to 27% step 4%
               if str(desc$,x%,3%) = str(rm_rec$(),249%,3%) then return
           next x%

           err% = 2% :  gosub err_scrn
        return

       wood_remake
          init (" ") barcode$, wandchar$, xx$()
          tt_unit% = 0%
          wood_rmk% = 1%

          gosub'100(fieldnr%)
          errormsg$ = " "
          if keyhit% <> 16% then goto wood_remake_next
             wood_rmk% = 0%
       return
       wood_remake_next
          upd_st$ = "11"
          gosub check_shipping
          if check% = 0% then goto prod_scan
          gosub update_shipping
       goto wood_remake
       return

       check_pass
             call "APCPASSW" ("APCSCANN", "WD", pass%)
       return


       check_dept_rmk                                 /*  (AWD043)  */
           table$ = "PLAN DEPT"
           code$  = rm_dept_n$
           gosub check_code

       return        /*  (AWD043) */
       
       find_part11_descr               /* (CR1017) */
           p_descr$ = "NA - Not applicable"
           init(" ") readkey$, desc$
           str(readkey$,1%,9%)   = "SCREEN"
           str(readkey$,10%,15%) = dt_partpos11$
           read #2,key = readkey$, using L17000, desc$, eod goto L17050
L17000:       FMT POS(25), CH(30)
           p_descr$ = desc$
L17050: return

        find_part4_descr               /* (CR1017) */
           p_descr$ = "NA - Not applicable"
           init(" ") readkey$, desc$
           str(readkey$,1%,9%)   = "COLOR"
           str(readkey$,10%,15%) = dt_partpos4$
           read #2,key = readkey$, using L17055, desc$, eod goto L17075
L17055:       FMT POS(25), CH(30)
           p_descr$ = desc$
L17075: return

       find_subpart4_descr               /* (CR1017) */
           p_descr$ = "NA - Not applicable"
           init(" ") readkey$, desc$
           str(readkey$,1%,9%)   = "HARDWARE"
           str(readkey$,10%,15%) = str(subpart$,4%,1%)
           read #2,key = readkey$, using L18000, desc$, eod goto L18050
L18000:       FMT POS(25), CH(30)
           p_descr$ = desc$
L18050: return

       lookup_main_dept                                    /*  (AWD043)  */
           lookup_dept% = 0%                               /* (AWD046)   */
           init(" ") rm_dept_n$, rm_status$, rm_key$
           rm_key$ = str(rm_bar$,1%,12%)
           rm_len% = 0%
           rm_len% = len(rm_bar$)
           rm_status% = 10%

           if rm_len% < 12% then                                           ~
              read #3,key > rm_key$, using L20000, rm_status$, rm_key$,    ~
                                       rm_dept_n$, eod goto not_main         ~
           else                                                            ~
              read #3,key = rm_key$, using L20000 , rm_status$, rm_key$,   ~
                                     rm_dept_n$, eod goto not_main

L20000:         FMT POS(13), CH(01), POS(22), CH(12), POS(249), CH(03)

REM  CALL "SHOSTAT" (" GLASS KEY " & RM_KEY$ )  STOP
REM  ACCEPT 2 = COMPLETE AND 4 = RECEIVED

              if rm_status$ <> "2" and rm_status$ <> "4" then goto not_completed
              lookup_dept% = 1%
              if rm_len% = 12% then return

              if str(rm_bar$,1%,9%) <> str(rm_key$,1%,9%) then goto not_main
              rm_bar$ = str(rm_key$,1%,12%)
        return
        not_completed
REM ACCEPT 2 = COMPLETE AND 4 = RECEIVED
         convert rm_status$ to rm_status%, data goto bad_status

bad_status:

         err% = 11%
         gosub err_scrn
        return
        not_main
         err% = 10%
         gosub err_scrn
        return


REM CHECK_TEMP_DEPT
REM INIT(" ") RM_DEPT_N$, RM_STATUS$, RM_KEY$
REM RM_KEY$ = STR(RM_BAR$,1%,12%)
REM IF RM_LEN% < 12% THEN                                           ~
REM    READ #16,KEY > RM_KEY$, USING L20000, RM_STATUS$, RM_KEY$,   ~
REM                           RM_DEPT_N$, EOD GOTO NOT_MAIN         ~
REM ELSE                                                            ~
REM    READ #16, KEY = RM_KEY$ USING L20000, RM_STATUS$, RM_KEY$,   ~
REM                                  RM_DEPT_N$, EOD GOTO NOT_MAIN
REM
REM    CALL "SHOSTAT" (" TEMP KEY " & RM_KEY$ )  STOP
REM    ACCEPT 2 = COMPLETE AND 4 = RECEIVED
REM
REM   IF RM_STATUS$ <> "2" AND RM_STATUS$ <> "4"                ~
REM      THEN GOTO NOT_COMPLETED
REM  LOOKUP_DEPT% = 1%
REM  IF RM_LEN% = 12% THEN RETURN
REM
REM  IF STR(RM_BAR$,1%,9%) <> STR(RM_KEY$,1%,9%)                ~
REM          THEN GOTO BAD_RMK_BARCODE
REM
REM  ACCEPT 2 = COMPLETE AND 4 = RECEIVED
REM
REM  IF RM_STATUS$ <> "2" AND RM_STATUS$ <> "4"                ~
REM             THEN ERR% = 11%
REM
REM  RM_BAR$ = STR(RM_KEY$,1%,12%)
REM
REM  IF ERR% <> 0% THEN GOSUB ERR_SCRN
REM  RETURN
REM  BAD_RMK_BARCODE
REM  ERR% = 10% :  GOSUB ERR_SCRN
REM  INIT(" ") RM_STATUS$, RM_BAR1$, RM_DEPT_N$
REM  RETURN                                              /* (AWD043)   */
                                                         /* (AWD046)   */


/*  (AWD045) - Numerious Mods shorten messages - error scrn for RF  */
        deffn'500(fieldnr%)
            gosub set_error
            accept                                                       ~
               at (01,02), fac(hex(94)), rf_errormsg$           , ch(20),~
                                                                         ~
               at (02,02), fac(hex(84)), rf_ee$(1%)             , ch(20),~
               at (03,02), fac(hex(84)), rf_ee$(2%)             , ch(20),~
               at (04,02), fac(hex(84)), rf_ee$(3%)             , ch(20),~
               at (05,02), fac(hex(84)), rf_ee$(4%)             , ch(20),~
               at (06,02), fac(hex(84)), rf_ee$(5%)             , ch(20),~
               at (07,02), fac(hex(84)), rf_ee$(6%)             , ch(20),~
                                                                         ~
               at (07,02), fac(hex(a4)),   rf_inpmessage$       , ch(20),~
               at (08,02), fac(hex(8c)),   pf$(1%)              , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_error
            inpmessage$ = "PF<04> to Continue"
            pf$(1%) =     "(04) Continue     "
            pfkeys$ = hex(ffffff04ffffffffffffffffffffffffff)
        return
/*  (AWD045) - Numerious Mods shorten messages  */


/* (AWD050) */

        lookup_sub_part
          init(" ") bcksubpt_rec$, flds$(), info_flds$(), specialmull$, ~
                    subpart$, series$, style$

          flag$ = "0"                  /* Sales Order Info         */
          pgm$  = "1"
          err1% = 0%

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

            if err1% <> 0% then                                ~
                    str(bcksubpt_rec$,48%,20%) = "00000000000000000000"

            init(" ") specialmull$, subpart$
            specialmull$ = str(bcksubpt_rec$,152%,1%)
            subpart$     = str(bcksubpt_rec$,48%,20%)
            series$  = str(bcksubpt_rec$,169%,16%) /*(AWD068)*/
            style$   = str(bcksubpt_rec$,185%,10%) /*(AWD068)*/
        return

        get_rmk_reasons
          init(" ") plowkey$

          plowkey$ = "PLAN REMK"
          call "PLOWCODE" (#2, plowkey$, " ", 9%, .30, f1%(2))
        return

/* (AWD064) */
        corruptWrite
          errormsg$ = "(ERROR) Can't Update APCPLNDT no Status/User!"
          gosub error_display
          goto exit_program



