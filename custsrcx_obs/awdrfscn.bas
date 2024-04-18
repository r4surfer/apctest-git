        REM *************************************************************~
            *  Hidden PF Key 32  - Runs APCSCAN2, clock in - out utility*~
            *                                                           *~            
            *  Program Name      - AWDRFSCN                             *~
            *  Creation Date     - 12/23/96                             *~
            *  Last Modified Date- 01/09/08                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Last Mod By       - Christie Gregory                     *~
            *                                                           *~            
            *  Description       - Scanning Utility used inconjunction  *~
            *                      with the new planning system.        *~
            *                                                           *~
            *  Note              - Same program as AWDSCANN for shipping*~
            *                      with a modified screen for the RF    *~
            *                      scanners.                            *~
            *                                                           *~
            *  Special Notes     - (GLS) Userid is for the Glass House  *~
            *                            and can only run Selections    *~
            *                            (4) and (5).                   *~
            *                            Remake Reason Codes 26, 28, 30 *~
            *                            are reserved for the Glass     *~
            *                            House and put into a seperate  *~
            *                            remake glass file.             *~
            *                      (SCN) Userid is for the Production   *~
            *                            lines. They cannot run Select- *~
            *                            ion (4) Complete Glass.        *~
            *                      (BYP) Userid is for the By-Pass Load-*~
            *                            ing. It does not require Load  *~
            *                            number.                        *~
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
            * 05/19/03 ! (AWD028) - Mod to Load scanning for the  ! RHH *~
            *          !            New shipping Label.           !     *~
            * 06/05/03 ! (AWD029) - Mod to turn on Screen Id 'BY?'! RHH *~
            *          !            for User Id 'APN' for Appian. !     *~
            * 06/16/03 ! (AWD030) - Mod to Add Staging of Appian  ! RHH *~
            *          !            loads. When product is Staged !     *~
            *          !            print Appian Shipping label   !     *~
            * 06/18/03 ! (AWD031) - Mod to Add Date and Time when ! RHH *~
            *          !            label printed (Staged)        !     *~
            * 06/26/03 ! (AWD032) - Mod for Pinter Selection for  ! RHH *~
            *          !            Staging. (A or B)             !     *~
            * 07/16/03 ! (AWD033) - Mod To Update Production for  ! RHH *~
            *          !            Complete during Stagging. When!     *~
            *          !            product not completed by Dept.!     *~ 
            *          !            Also, no label don't stage.   !     *~
            * 07/30/03 ! (AWD034) - Mod to turn on 2nd Staging    ! RHH *~
            *          !            for shipping.                 !     *~
            * 03/01/04 ! (AWD035) - New program for RF scanning   ! CMG *~
            *          !            numerious mods                !     *~
            * 04/19/04 ! (AWD036) - Mod to be able to scan and    ! CMG *~
            *          !            re-print Appian Labels        !     *~
            * 06/04/04 ! (AWD037) - Mod to be able to stage prod  ! CMG *~
            *          !            that has '13' status          !     *~
            * 07/21/04 ! (AWD038) Mod to automatically update dept! CMG *~
            *          !            '054' just like '044' when    !     *~
            *          !            staging.                      !     *~
            * 08/23/04 ! (AWD039) Mod NOT to automatically complet! CMG *~
            *          !            department '044'.             !     *~
            * 03/22/05 ! (AWD040) Mod to add ability to scan in   ! CMG *~
            *          !   missing from buggy remakes.            !     *~
            * 01/09/08 ! (AWD041) mod for new dept 074            ! CMG *~
            *************************************************************

        dim hdr$47,                      /* ASKUSER Header             */~
            filename$8,                  /* Use with EWDOPEN - EWD016  */~            
            trauma_center$1,             /* (EWD014) Sitch on/off      */~
            her$(30%)50,                 /* Error Text Display         */~
            scr_sel$1, scr_sel_d$30,     /* Screen Scanning Option     */~
            rep_id$(10%)3,               /* Valid Repair Id's  (EWD019)*/~
            rep_code$(30%)2,             /* Vaild Reason code's(EWD019)*/~
            scr_id$3, msg$(3%)20, hh$40, /* Scanning User Id           */~
            scr_id_d$30,                 /* User Id Name               */~
            scr_dept$3, scr_dept_d$30,   /* Product Line / Dept Code   */~
            scr_prt$1,                   /* Staging Printer A,B(AWD032)*/~ 
            sav_scr_dept$3,              /* Save Dept          (EWD025)*/~
            scr_shft$2, scr_shft_d$30,   /* Screen Shift Entry         */~
            scr_proc$2, scr_proc_d$30,   /* Product / Dept Process Code*/~
            scr_load$5, scr_load_d$30,   /* Production Load and Descrip*/~
            readkey$24, desc$30,         /* GENERIC KEY                */~
            table$9, code$3,             /* TABLE LOOKUP NAME AND CODE */~
            pfkeys$40,                   /* PF KEYS                    */~
            ee$(7%)50,                   /* 'STOP' Message Error Text  */~
            ad_rec$64, ad_rec1$64,       /* Prod Scanning Audit File   */~
            ad_time$8, ad_key$33,        /* Time Stamp                 */~
            ad_dept$3, ad_proc$2,        /* Aduit Dept and Process     */~
            dt_rec$256, dt_dept$3,       /* Production Detail          */~
            dt_key$23, sav_key$23,       /* (APCPLNDT)-Tracking File   */~
            override_key$23,             /* Override Scanning  (AWD033)*/~
            dt_load$5,                   /* Scanning Load Number - Only*/~
            err$(40%)50,                 /* Defined Error Messages     */~
            barcode$18,                  /* Scanned Barcode            */~
            barcode_shp$20,              /* Shipping Barcode   (AWD028)*/~
            awd_app_key0$20,             /* Label Primary Key  (AWD028)*/~
            rec$(3%)200,                 /* Appian Label Record(AWD030)*/~ 
            app_scan_tme$4,              /* Appian Scan Time   (awd028)*/~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dateout$8,                   /* Time Extract For Screen    */~
            inp_text$(5%)20,             /* Input Prompt Text          */~
            fld$(4%)30,                  /* Field Text                 */~
            errormsg$20,                 /* Error message              */~
            i$(24%)85,                   /* Screen Image               */~
            inpmessage$20,               /* Informational Message      */~
            prevcode$18,                 /* Previous Bar Code Entered  */~
            lfac$(4%)1,                  /* Field Attribute Characters */~
            pf$(3%)20,                   /* PF Screen Literals         */~
            scrn_title$40, title$40,     /* Screen Description         */~
            wandchar$1,                  /* Wand Character - Scanner   */~
            wandchar_shp$1,              /* Wand Character - Appian    */~
            userid$3                     /* Current User Id            */


        dim                              /* (APCPLNGR) - File          */~
            rm_key$12, rm_rec$256,       /* Re-Make Key and Record     */~
            rm_bar$12,                /* Remake Glass Barcode  (EWD018)*/~
                                         /*                    (AWD040)*/~
            rm_bar1$9,                   /* To scan missing from buggy */~
            rm_reason$2, reason_d$30,    /* Glass Remake Reason Code   */~
            rm_rm_dte$6,                 /* Date Re-make Scan (EWD001) */~
            calc_time$8, calc_dte$6,     /* Use for Re-make Calc(EWD001*/~    
            bar_wand$1, reason_wand$1,   /* Barcode Wand Characters    */~
            rm_part$25, rm_gls$2, rm_lit$2, /* Remake MFG (EWD005)     */~
            rma_rec$64,                  /* (APCPLNGA) - (EWD005)      */~
                                         /* (APCPLNDT)                 */~
            dt_bar$18, dt_st$2,          /* Bar Code Value             */~
            dt_txt$4,text$(2%)70,        /* Detail Text Id             */~
            gt_rec$200, gt_part$25, lk$1,/* (EWD011) Trauma Record     */~
            dt_cust$9,                   /* (EWD011) Get Customer      */~
            tt_unit$24                   /* Scanned Units each Prod    */

        dim                              /* (EWD024)                   */~
            tt_in$8, tt_out$8,           /* Start and Complete Times   */~
            tt_hr$3, tt_mn$2             /* Calculated Hours & Minutes */

        dim                              /* (AWD040)                   */~
            sp_key$35,                   /* EWDSCHED Readkey           */~
            sp_status_dte$6,             /* Special Analysis Status Dte*/~
            sp_status$1,                 /* Special Analysis Process Cd*/~
            sp_type$1,                   /* Analysis Type Process Code */~
            sp_type1$1,                  /* Analysis Type Process Code1*/~
            sp_route$5,                  /* Route code                 */~
            sp_cutoff$2,                 /* cut off Day 1 thru 7       */~
            sp_cust$9,                   /* Customer Code              */~
            sp_so$8,                     /* Sales order number         */~
            sp_ln$2,                     /* S.O. Line item No.         */~
            sp_ln_item$4,                /* Line Item Piece            */~
            sp_due$6,                    /* Sales order Due Date       */~
            sp_part$25,                  /* Part Number                */~
            sp_qty$4,                    /* line Item Quantity         */~
            sp_qty1$4,                   /* line Item Quantity 1      */~
            sp_stat$2,                   /* (PLAN STAT) Planning       */~
            sp_usr$3,                    /* Last Mod User Id           */~
            sp_dte$6,                    /* Last Mod Date              */~
            sp_time$8,                   /* Last Mod Time              */~
            sp_text$4,                   /* Line Item Text Id          */~
            sp_primary$3,                /* Primary Dept               */~
            sp_fil$17,                   /* Filler Area                */~
            vf$(10%)20                   /* Variable Fields            */

         dim rm_dept_n$3,                /* New Department No  (AWD040)*/~
             dept_wand$1                 /* Screen Character   (AWD040)*/ 

        dim f2%(45%),                    /* = 0 if the file is open    */~
            fs%(45%), axd$4,             /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(45%)20                 /* Text from file opening     */

            mat f2% = con
            mat fs% = con

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
            * #8  ! EWDPLNGT ! (New) Glass Re-Make Trauma Ctr (Remove)  *~
            * #9  ! APCPLNGA ! New Re-Make Glass Audit (EWD005)         *~
            * #10 ! EWDPLNRK ! New Glass Rack Data Base (EWD010)        *~
            * #12 ! AWDAPPLS ! New Appian Shipping Label File   (AWD028)*~
            * #15 ! TXTFILE  ! New Master Text File                     *~     
            * #42 ! AWDBAYBW ! New File for Complaints                  *~
            * #63 ! BCKSUBPT ! New Sub Part Number File         (PAR000)*~
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
                        varc,     indexed,  recsize = 256,               ~
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
        REM    select #8,  "EWDPLNGT",                                      ~
        REM                varc,     indexed,  recsize =  200,              ~
        REM                keypos = 1,    keylen = 29,                      ~
        REM                alt key 1, keypos = 18, keylen = 12,             ~
        REM                    key 2, keypos =  4, keylen = 26
        REM                   /* (EWD024) - Change EWDPLNGT Key Layout */
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
                                                   /* (AWD028)         */
                                                   /* (AWD030)         */
            select #12,  "AWDAPPLS",                                     ~
                        varc,     indexed,  recsize = 600,               ~
                        keypos =  1,   keylen = 20,                      ~
                        alt key 1, keypos = 21, keylen = 34, dup,        ~
                            key 2, keypos = 23, keylen = 32, dup,        ~
                            key 3, keypos = 56, keylen = 10, dup
                                                   /* (AWD028)         */
                                                   /* (AWD030)         */
                                                   /* (AWD032)         */
            select #14, "EWDPRDLB",                                      ~
                        varc,     indexed,  recsize =  640,              ~
                        keypos =    1, keylen =   35,                    ~
                        alt key  1, keypos = 278, keylen =  23 
                                                   

            select #15, "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =   1, keylen =  11
                                                    /* (EWD010)/(EWD011) */

                                                    /* (AWD040) - Begin */ 

            select #16,  "AWDPLNGR",                                     ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 22,   keylen = 12,                      ~
                        alt key 1, keypos =  7, keylen = 27,             ~
                            key 2, keypos =  1, keylen = 33,             ~
                            key 3, keypos = 13, keylen = 21
                            
            select #17,  "AWDPLNGA",                                     ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =  1,   keylen = 18,                      ~
                        alt key 1, keypos =  7, keylen = 12

            select #18,  "EWDSCRMK",                                     ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,   keylen = 50,                       ~
                        alt key 1, keypos =  16, keylen = 35        
                        
            select #19,  "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos = 1,    keylen =  9,                      ~
                        alt key 1, keypos = 10, keylen = 30, dup,        ~
                            key 2, keypos =424, keylen =  9, dup,        ~
                            key 3, keypos =771, keylen =  9, dup,        ~
                            key 4, keypos =780, keylen =  9, dup,        ~
                            key 5, keypos = 1049, keylen = 9, dup  

                                                   /* (AWD040) - End   */
            select #42, "AWDBAYBW",                                      ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =    1, keylen =  29                      

            select #63, "BCKSUBPT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =  12, keylen =  11, dup,     ~
                            key  2, keypos =  23, keylen =  45, dup


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
            filename$ = "APCPLNGA" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDPLNRK" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error
                                                      /* (AWD028)       */
            filename$ = "AWDAPPLS" : call "EWDOPEN" (#12, filename$, err%)
            if err% <> 0% then gosub open_error

                                                      /* (AWD032)       */
            filename$ = "EWDPRDLB" : call "EWDOPEN" (#14, filename$, err%)
            if err% <> 0% then gosub open_error

                                                      /* (AWD028)       */
            filename$ = "TXTFILE" : call "EWDOPEN" (#15, filename$, err%)
            if err% <> 0% then gosub open_error

            call "OPENOLIB" (#6, "SHARE", f2%(6%), rslt$(6%), axd$)
                                                         /* (EWD016)   */

                                                         /* (AWD040)   */
            filename$ = "AWDPLNGR" : call "EWDOPEN" (#16, filename$, err%)
            if err% <> 0% then gosub open_error                      
            call "OPENCHCK" (#17, fs%(17%), f2%(17%),500%, rslt$(17%))  
            filename$ = "EWDSCRMK" : call "EWDOPEN" (#18, filename$, err%)
            if err% <> 0% then gosub open_error           
            filename$ = "CUSTOMER" : call "EWDOPEN" (#19, filename$, err%)
            if err% <> 0% then gosub open_error            
            filename$ = "AWDBAYBW" : call "EWDOPEN" (#42, filename$, err%)
            if err% <> 0% then gosub open_error 

            filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
            if err% <> 0% then gosub open_error           
                                             
                                                         /* (AWD040)   */

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            dim apc$40, pname$8 
            apc$   = "** (AWD) Appian Master Scan Utility ***"
            pname$ = "AWDRFSCN"                

            trauma_center$ = "N"                           /* (EWD014) */
                                                           /* (AWD028) */


            b_max% =  2%                     /* SET NUMBER OF TIMES TO */
                                             /* RING BELL ON SCREEN    */
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            ee$(1%) = "  SSS TTT OOO PPP   "
            ee$(2%) = "  S    T  O O P P   "
            ee$(3%) = "  SSS  T  O O PPP   "
            ee$(4%) = "     S T  O O P     "
            ee$(5%) = "     S T  O O P     "
            ee$(6%) = "  SSSS T  OOO P     "


/*  (AWD035) - All Error Messages shortened  */

            err$(1%)="Brcd Not on File   "
            err$(2%)="Brcd Not Dept      "
            err$(3%)="Brcd Diff Dept     "
            err$(4%)="Brcd Not Valid Dept"
            err$(5%)="Brcd Already Cmplte"
            err$(6%)="Update Prd Dpt(XXX)"
            err$(7%)="Update Brcd Audit  "
            err$(8%)="Update Stg Dpt(XXX)"
            err$(9%)="Update Load Not Stg"
           err$(10%)="Glass Brcd not file"
           err$(11%)="Glass Brcd Not Cmpl"
           err$(12%)="Cannot Update Glass"
           err$(13%)="Product already Stg"
           err$(14%)="Prd already Loaded"
           err$(15%)="Reason Cde Glass RM"
           err$(16%)="Invalid Load No."
           err$(17%)="Shift Code Not Vali"
           err$(18%)="Glass Brcd Not Sch"
           err$(19%)="Tempered/Special Lt"
           err$(20%)="Glass Not Found"
           err$(21%)="Invalid Scann User"
           err$(22%)="Update Glass Audit"
           err$(23%)="Glass Brcd Complet"
           err$(24%)="Not Valid App Labe"
           err$(25%)="Ship Lbl not Prod"
           err$(26%)="Updt Prod Dept(XXX)"
           err$(27%)="Updt Dept(XXX)App"
           err$(28%)="App Lbl Not On File"
           err$(29%)="Not Stg No App Lbl"
           err$(30%)="InvalidDepartment?"
                                                            /* (AWD040) */
                                                       /* (AWD030)     */
                                                       /* (AWD033)     */   

            her$(1%) = "Invalid Barcode No   "
            her$(2%) = "Not On File For Dept "
            her$(3%) = "On File For Dept     "
            her$(4%) = "Not Valid For Dept   "   
            her$(5%) = "Prod Already Scanned "
            her$(6%) = "Error  Updating      "
            her$(7%) = "Error Updating Audit "
            her$(8%) = "Prod Not Scan By XXX "
            her$(9%) = "Product Not Staged   "
            her$(10%) = "Glass Barcode Invalid"
            her$(11%) = "Glass Not Completed  "
            her$(12%) = "Not To Update Glass  "
            her$(13%) = "Prod Already Staged  "
            her$(14%) = "Prod Already Loaded  "
            her$(15%) = "Invalid Reason Code? "
            her$(16%) = "Invalid Load Number  "
            her$(17%) = "Invalid Shift Code   "
            her$(18%) = "Glass Not Scheduled  "
            her$(19%) = "Temp/Spec    Re-Make?"
                                                   /* (AWD028)           */
            her$(24%) = "Invalid Appian Bacode"
            her$(25%) = "Ship Does Match Prod "
                                                   /* (AWD030)           */
            her$(26%) = "Updating Product Dept"
            her$(27%) = "Updating Appian Label" 
            her$(28%) = "Appian Lbl Not On File"
            her$(29%) = "NOT STAGED-NO LABEL  "
                                                   /* (AWD030)           */
                                                   /* (AWD033)           */ 

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
                                     rep_code$(10%) = "59 "
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

        initialize
            edit% = 0%
            init(" ") scr_shft$, scr_shft_d$, scr_proc$, scr_proc_d$,    ~
                    scr_dept$, scr_dept_d$, dt_st$, prevcode$, tt_unit$, ~
                      scr_id$, scr_sel$, scr_sel_d$, rm_reason$, rm_bar$,~
                      reason_d$, scrn_title$, title$, fld$(3%),          ~
                      scr_id_d$, scr_load$, scr_load_d$, sav_scr_dept$,  ~
                      scr_prt$, scr_dept$, scr_shft$, rm_bar1$, rm_dept_n$,~
                      dept_wand$  
                                                          /* (EWD026)  */
            scr_proc$ = "01"           /* Set Default to Manufacturing */
            tt_unit% = 0%

            inp_text$(1%)="Scan Shp Barco"
            inp_text$(2%)="Scan Prd Barco"
        main

            if str(userid$,1%,2%) <> "RF" then goto not_staging
               scr_sel$ = "2"
               scr_dept$ = "106"

not_staging

            if str(userid$,1%,2%) <> "SF" then goto not_shipping
               scr_sel$ = "3"
               scr_dept$ = "108"
not_shipping


                                                /* (AWD040) - BEG */
            if str(userid$,1%,2%) <> "GF" then goto not_glass
               scr_sel$ = "5"
               scr_dept$ = "112"
not_glass
                                                /* (AWD040) - END */
            gosub mainmenu

            init(" ") errormsg$
            if keyhit% = 2% then exit_program       /*  (AWD035) - Change Function Keys  */
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
               gosub check_load
               if code% = 0% then goto initialize
            edit% = 1%
            if keyhit% = 1% then gosub prod_scan    /*  (AWD035) - Change Function Keys  */
            if keyhit% = 3% then gosub prod_scan    /*  (AWD036) - Mod to Reprint Label  */
         goto main

         mainmenu                                /* Main Scanning Menu */
            gosub set_screen_1
                                                 /* Staging Printer    */
                                                 /* (AWD032) - 6/26/03 */
/*  (AWD035) - Mod to shorten screen  */
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(08),~
               at (01,12), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(20),~
                                                                         ~
               at (03,02), "Sel: ",                                      ~
               at (03,08), fac(lfac$(1%)), scr_sel$             , ch(01),~
                                                                         ~
               at (04,02), "Dept:",                                      ~
               at (04,08), fac(lfac$(1%)), scr_dept$            , ch(03),~
               at (05,12), fac(lfac$(1%)), scr_prt$             , ch(01),~
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
               at (08,02), fac(hex(8c)),   pf$(1%)              , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               errormsg$ = " "

/*  (AWD035) - Mod take out all function key except 1% and 2%  */
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_screen_1
            lfac$(1%) = hex(81)
            init(" ") dateout$, scrn_title$
            title$ = " S c a n n i n g   S e l c t i o n s "
            call "TIME" (dateout$)
            inpmessage$ = "Enter a Valid Scanning, Department, Shift Sele~
        ~ction, Userid Required?"
            pf$(1%) = "(1)Scn (2)Ext (3)Prt"
            if scr_sel% <> 3% then goto L04200
               pf$(1%) = "(1)Scn (2)Ext       "
               pfkeys$ = hex(0102ffffffffffffffffffffffffffffff00)
L04200:
            pfkeys$ = hex(010203ffffffffffffffffffffffffffff00)
        return                                  /* (EWD022) - Add 12 Key */

        prod_scan                                      /* Scann Products */
                                                       /* (EWD013)       */
            if keyhit% <> 3% then goto not_reprint     /*  (AWD036)      */
               gosub reprint_label
REM               return                                  /*  (AWD036)      */
not_reprint:                                           /*  (AWD036)      */

            if scr_id$ = "GLS" or scr_id$ = "SCN" or scr_id$ = "BYP" then  ~
                                                       goto L04400
                                                       /* (AWD029)       */

            if str(scr_id$,1%,2%) = "OV" then goto L04400
                                                      /* (EWD013) Cleanup*/

            if scr_sel% < 6% then goto L04300          /* (EWD004)-Begin */
                                                       /* (EWD006) -     */
               call "APCPASSW" ("APCSCANN", scr_id$, pass%)
                   if pass% <> 0% then goto L04390
               if scr_sel% = 6% then goto rga_scan
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

L04320:     fieldnr% = 1%
                                             /* (AWD028)                 */
            init (" ") barcode_shp$, wandchar_shp$, awd_app_key0$
            init (" ") barcode$, wandchar$, xx$()
            rep_id% = 0%                     /* (EWD019) - Exception     */
            for jj% = 1% to 10%              /* Repair Id                */
               if scr_id$ = rep_id$(jj%) then rep_id% = 1%
            next jj%
            if scr_sel% <> 5% then rep_id% = 0%
                                             /* (EWD019)                 */
            if scr_sel% = 6% then goto rga_scan     /* (EWD006) Not Used */   
            if scr_sel% > 3% then goto glass_scan   /* (EWD013) Glass    */
                                                    /*   but less than 6 */ 
                                                    /* (AWD028)          */
        prod_scan_2
            init(" ") barcode$, wandchar$, xx$()
                                                    /* (AWD030)          */
            if scr_sel% = 2% then gosub'150(fieldnr%) 
            if scr_sel% = 3% then gosub'100(fieldnr%)
                                                    /* (AWD030)          */
            if keyhit% = 3% then goto prod_scan
            errormsg$ = " "           /*  (AWD035) - Change Function Keys */
            if keyhit% <> 2% then goto prod_scan_next
L04390:        return clear all
               goto initialize

L04400:        return clear all                      /* (EWD013)      */
               err% = 21%
               gosub err_scrn
               goto initialize

        prod_scan_next
                                                      /* (AWD030)       */
            if scr_sel% = 2% then goto L04500         /* Staging        */
                                                      /* (AWD030)       */ 
            if fieldnr% = 2% then goto L04450
               gosub check_shipping_appian
               if check% = 0% then goto prod_scan
               fieldnr% = 2% 
               goto prod_scan_2
L04450: 
            if scr_sel% = 3% then gosub check_appian_production
            if check% = 0% then goto prod_scan                                      
                                                      /* (AWD028)       */
L04500:                                               /* (AWD030)       */

            if scr_sel% = 1% then gosub check_data                       ~
                             else gosub check_shipping
            if check% = 0% then goto prod_scan

                                                      /* First          */ 
            if scr_sel% = 1% then gosub update_track                     ~
                             else gosub update_shipping
                                                      /* Second         */
            if scr_sel% = 3% then gosub update_shipping_appian
                                                      /* (AWD028)       */

        REM - Special Mod for Wood Surround Scanning ( Update Staging)
            if scr_sel% <> 1% then goto prod_scan
REM            if scr_dept$ <> "044" then goto prod_scan    /* (EWD026) */

                                                            /* (AWD038) BEG */
REM         if scr_dept$ <> "044" and scr_dept$ <> "033" then goto prod_scan   
            if scr_dept$ = "044" and scr_dept$ = "033" then goto update_supp
  
            if scr_dept$ = "054" then goto update_supp
/*(AWD041) */
            if scr_dept$ = "074" then goto update_supp

                    goto prod_scan
update_supp:
                                                            /* (AWD038) END */

               sav_scr_dept$ = scr_dept$                   /*  (EWD026) */                                         
               scr_sel% = 2%            /* Change to Staging Selection */
               gosub stagged            /* Set Flags for Stagging      */
               gosub update_shipping    /* Update all Dept's as Stagged*/
               gosub prod               /* Re-Set Flags for Production */
               scr_sel%  =  1%          /* Re-Set Screen Selection     */
               scr_dept$ = "044"        /* Re-Set Department Code      */
               if sav_scr_dept$ = "033" then scr_dept$ = "033"  /* (EWD026) */
               if sav_scr_dept$ = "054" then scr_dept$ = "054"  /* (AWD038) */
/*(AWD041)*/
               if sav_scr_dept$ = "074" then scr_dept$ = "074"

               tt_unit% = tt_unit% - 1% /* Adjust Units from Stagging  */
            goto prod_scan              /* Unit was already updated by */
                                        /* Wood Surround Dept.         */
        rga_scan
            call "APCRGA23" ( #2,                  /* GENCODES Channel */~
                              #5,                  /* APCPLNAD Channel */~
                              #1 )                 /* APCPLNDT Channel */

        return

        reprint_label
               err% = 0%
               keyhit% = 3%
               init(" ") barcode$, wandchar$, xx$()
               gosub'150(fieldnr%)                     /*  (AWD036)      */
               if keyhit% = 2% then return
               gosub lookup_barcode
               if errormsg$ <> " " then goto reprint_label
               if keyhit% = 2% then return
                  gosub print_label /*  (AWD036)      */
                  goto reprint_label
 

        return
        lookup_barcode
            init(" ") dt_key$
            str(dt_key$,1%,18%) = barcode$      /* Set Barcode Value   */
        check_barcode_nxt
            read #1,key > dt_key$, using L05220 ,dt_load$,dt_key$, dt_st$, ~
                                                eod goto bar_done

                 if str(dt_key$,19%,3%) = "001" then goto check_barcode_nxt
                 if dt_st$ < "14" then goto barcode_error


        bar_done
        return
        barcode_error
            err% = 29% : gosub err_scrn
        return
        

        glass_scan                                   /* Scan Products */
            fieldnr% = 1%
            pass% = 1%
            init (" ") rm_bar$,rm_reason$,xx$(),bar_wand$,reason_wand$, ~
                       rm_bar1$
        glass_scan_2
            init (" ") xx$(), rm_reason$, reason_wand$
            gosub'200(fieldnr%)
            errormsg$ = " "    /*  (AWD035) - Change Function Keys */
            if keyhit% <> 2% then goto glass_scan_nxt
               return clear all
               goto initialize
        glass_scan_nxt
            if fieldnr% = 5% then goto L04810
            if scr_sel% <> 5% then goto L04810
               pass% = pass% + 1%
               fieldnr% = 3%
               if pass% > 2% and err% = 0% then fieldnr% = 5%
                                               /* (AWD040) */
                                               /* Only do on fieldnr% = 3% */
                                               /* so only looking up once  */
               if fieldnr% = 5% then gosub lookup_main_dept
               goto glass_scan_2
L04810:     gosub check_glass
            if check% = 0% then goto glass_scan
            gosub update_glass
            goto glass_scan

        check_data                              /* Scheduled Data      */
            err% = 0%
            check% = 0% : dt_st% = 0%           /* (APCPLNDT) - FILE   */
            init(" ") dt_key$, errormsg$, dt_bar$, dt_rec$, sav_key$
            str(dt_key$,1%,18%) = barcode$      /* Set Barcode Value   */
            str(dt_key$,19%,3%) = scr_dept$     /* Set Department Code */
            str(dt_key$,22%,2%) = scr_proc$     /* Set Process Code    */
            read #1,key = dt_key$, using L04940 , dt_st$, eod goto L05020
L04940:        FMT POS(64), CH(2)
            convert dt_st$ to dt_st%, data goto L05020

            dt_bar$ = barcode$
            if dt_st% > 11% then goto L05070     /* Already Scanned    */
               check% = 1%
               sav_key$ = dt_key$
        return
L05020:     gosub check_errors : gosub err_scrn
        return
L05070:     err% = 5% :  gosub err_scrn
        return
                                                /* (AWD028)            */
        check_appian_production
            err% = 0%
            check% = 0%
            if str(barcode_shp$,1%,18%) <> barcode$ then goto LAPP_2
               check% = 1%  
        return

        check_shipping_appian
            err% = 0% 
            check% = 0%
            init(" ") awd_app_key0$
            awd_app_key0$ = barcode_shp$
            read #12, key 0% = awd_app_key0$, eod goto LAPP_1
              check% = 1% 

        return
LAPP_1:     check% = 0%
            err% = 24%
            errormsg$ = err$(err%)               /*  (AWD035) take out error */
            gosub err_scrn                       /*  (AWD035) take out error */
REM         gosub error_display                  /*  (AWD035) take out error */
        return
LAPP_2:     check% = 0%
            err% = 25%
            errormsg$ = err$(err%)               /*  (AWD035) take out error */
            gosub err_scrn                       /*  (AWD035) take out error */
REM         gosub error_display                  /*  (AWD035) take out error */
        return
                                                /* (AWD028) (AWD029)   */
                                                /* (AWD033)            */                                    
        check_shipping                          /* Staging/Loading     */
            override% = 0%
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
               if scr_sel% <> 2% then goto L05380
                  if dt_dept$ = "102" and dt_st% < 12% then /* PULL STK*/~
                                                  goto L05360
                  if dt_dept$ = "104" and dt_st% < 12% then /* PULL MUL*/~
                                                  goto L05360

                                                        /* (AWD033)      */
                                                        /* Not Completed */
                                                        /* by Department */
                  if dt_st% < 12% then goto L05450      /* Staging Test  */
                                                        /* (AWD033)      */  
                                                        /* (AWD037)      */
REM               if dt_st% > 12% then goto L05640
                  if dt_st% > 13% then goto L05640
L05360:              hit% = hit% + 1%                   /* Ready to Stag */
                     goto check_shipping_nxt
L05380:        if userid$ = "BYP" then goto L05400      /* (EWD002)      */
                                                        /* (AWD029)      */
               if userid$ = "APN" and str(scr_id$,1%,2%) = "BY"           ~
                                                         then goto L05400

               if userid$ = "SF1" and str(scr_id$,1%,2%) = "BY" then      ~
                                                             goto L05400
               if userid$ = "RF1" and str(scr_id$,1%,2%) = "BY" then      ~
                                                             goto L05400

               if scr_load$ <> dt_load$ then goto L05720   /* Check Load */
                                         /* Per Keith Barnes remove Test */
                   /* (EWD021) Per Keith Barnes & Kyle Green to add Test */
               if dt_st% < 14% then goto L05450         /* Loading Test  */
                                                        /* (RHHTEST)     */
                                                        /* (AWD030)      */

L05400: 
          if dt_st% > 15% then goto L05640              /* (EWD001)      */


                                                        /* (AWD030)      */               
                                                        /* (RHHTEST)     */
                  hit% = hit% + 1%                      /* Ready to Load */
                  goto check_shipping_nxt
L05430:     if hit% <> 0% then check% = 1% else goto L05580
        return
L05450:     check% = 0%
            if scr_sel% <> 2% then goto L05530
                                                         /* (AWD033)   */
               override% = 1%
               if dt_dept$ = "044" then goto L05520      /* (AWD039)   */
               init(" ") override_key$
               str(override_key$,1%,18%) = barcode$
               str(override_key$,19%,3%) = dt_dept$
               str(override_key$,22%,2%) = scr_proc$
               gosub update_track_override
               if override% > 1% then return        /* (Error) occurred */
                                                    /* Exit routine     */
                                           
                                                    /* Now the product */
                                                    /* can be staged   */ 
               goto check_shipping                  /* Normal Exit for */
                                                    /* Staging         */
                                                    /* (AWD033)        */ 
L05520:        err% = 8%                            /* (AWD039)        */
               str(err$(8%),18%,3%) = dt_dept$ 
               goto L05560
                                      /* (Above no Used)    (AWD033) */                    
L05530:     err% = 9%
L05560:     gosub err_scrn
REM            if scr_sel% <> 2% then gosub error_display   /* (EWD020)   */
        return
L05580:     check% = 0%
            err% = 1% : gosub err_scrn
REM            if scr_sel% <> 2% then gosub error_display   /* (EWD020)   */  
        return
L05640:     check% = 0%
            if scr_sel% = 2% then err% = 13% else err% = 14%
            gosub err_scrn
REM            if scr_sel% <> 2% then gosub error_display   /* (EWD020)   */
        return
L05720:     check% = 0%
            err% = 16% : gosub err_scrn
REM            if scr_sel% <> 2% then gosub error_display   /* (EWD020)   */
        return

        check_glass                        /* (EWD013) - Check Routine */ 
            check% = 0% : cnt% = 0%        /*    Note - Only (1) Glass */
            init(" ") rm_key$, sav_st$     /*    record per Barcode    */
            rm_len% = 0%
            rm_len% = len(rm_bar1$)
                                           /* Quick Fix   03/16/2000   */
REM            rm_len% = 12%                  /* Only allow 12 digit      */

                               /*                          */  
            if rm_len% < 11% then str(rm_key$,1%,9%) = rm_bar1$           ~
                             else str(rm_key$,1%,12%) = rm_bar1$
                                           /* (EWD018) = Primary Key   */
   
        check_glass_nxt
REM FT% = File Tempered  FR% = File Remake        
            ft% = 0%                            /*  (AWD040) */
            fr% = 0%                            /*  (AWD040) */
            gosub check_glass_file              /*  (AWD040) */
            if glass% <> 1% then goto L05980    /*  (AWD040) */


            cnt% = cnt% + 1%

            if cnt% > 1% and rm_len% > 11% then goto L05980
            rm_key$ = str(rm_rec$,22%,12%)     /* Glass Barcode & Rm No*/
                                               /* (EWD018)             */
            if rm_len% < 11% and str(rm_key$,1%,9%) <> rm_bar1$ then      ~
                                                       goto L05980
        
            if rm_len% > 11% and str(rm_key$,1%,12%) <> rm_bar1$ then     ~
                                                       goto L05980
                                               /* (EWD018)             */
               sav_st$ = str(rm_rec$,13%,1%) 
               if scr_sel% = 5% then goto create_remake
                                               /* (EWD013) Glass Table */ 
                  if str(rm_rec$,13%,1%) = "1" then goto L05960
                     goto check_glass_nxt      /* Glass Not Scheduled  */

        create_remake                          /* Check Completed Glass*/
                                               /* (EWD019) - Repair    */
            if rep_id% = 1% and str(rm_rec$,13%,1%) = "1"                ~
                                                       then goto L05900 
            if rep_id% = 1% and str(rm_rec$,13%,1%) = "2"                ~
                                             then goto check_glass_nxt
                                               /* (EWD019) - Exception */
               if str(rm_rec$,13%,1%) <> "2" then goto check_glass_nxt
L05900:                                   /* (EWD019) Repair Exception */

                  if str(rm_bar$,1%,1%) = "Z" then goto L06110
                                          /* Cannot remake 'Z' Barcode */    
                  gosub check_reason           /* Can Only Re-Make Comp*/
                  if code% = 0% then goto L06150
                  gosub check_specials         /* (EWD005) - Spec/Temp */
                  if code% = 0% then goto L06160
                  gosub check_dept_rmk          /* (AWD040) */
                  if code% = 0% then goto L06180   /* (AWD040) */

L05960:     check% = 1%                        /* Valid Scan of Glass  */
        return
L05980:     if cnt% > 0% then goto L06040
               err% = 10% : goto L06170        /* Not on File/Invalid  */

L06040:     if scr_sel% = 5% then goto L06100
               if sav_st$ = "2" then err% = 23%                         ~
                                else err% = 18%
               goto L06170                     /* Glass Not Scheduled  */

L06100:     err% = 11% : goto L06170           /* Glass Not Completed  */

L06110:     err% = 10% : goto L06170           /* (Z)  Is Invalid      */

L06150:     err% = 15% : goto L06170           /* Invalid Reason Code  */

L06160:     err% = 19% : goto L06170           /* Invalid Temp/Spec    */

L06180:     err% = 30%                         /* Invalid Rmk Depart   */

L06170:     gosub err_scrn          
            check% = 0%
        return



        check_glass_file                             /*  (AWD040)    */
             glass% = 0%
             tempered% = 0%
             if rm_len% < 11% then                                            ~
            read #3,key > rm_key$, using L05850 , rm_rec$, eod goto check_temp~
                             else                                             ~
            read #3,key = rm_key$, using L05850 , rm_rec$, eod goto check_temp
L05850:        FMT CH(256)

               if rm_len% = 12% then found_gls
                   
                   if str(rm_rec$,22%,9%) <> rm_bar1$ then goto check_temp

found_gls:
               glass% = 1%
               ft% = 3%
               fr% = 9%
               return
        check_temp

             if rm_len% < 11% then                                            ~        
            read #16,key > rm_key$, using L05850 , rm_rec$, eod goto no_glass ~
                             else                                             ~
            read #16,key = rm_key$, using L05850 , rm_rec$, eod goto no_glass
            

               if rm_len% = 12% then goto found_temp
                       if str(rm_rec$,22%,9%) <> rm_bar1$ then goto no_glass
found_temp:
               glass% = 1%
               tempered% = 1%
               ft% = 16%
               fr% = 17%
        no_glass
        return                                       /*  (AWD040)    */


        update_glass_sched                       /* (AWD040) - Begin   */
           gosub check_temp_stock
            if temp_stock% = 1% then return             
           init(" ") sp_status_dte$, sp_status$, sp_type$, sp_due$,    ~
                     sp_part$, sp_qty$, sp_qty1$, sp_usr$, sp_dte$,    ~
                     sp_key$, sp_primary$, sp_type1$, sp_time$
           sp_qty1% = 0
           gosub lookup_dt
           if dt_rec% <> 1% then return
           gosub lookup_cust
           sp_status_dte$ = date
           sp_status$     = "0"
           sp_type$       = "5"
           sp_due$        = " "
           sp_part$       = str(rm_rec$,125%,25%)
           sp_qty$        = "0001"
           sp_usr$        = userid$
           sp_dte$        = date
           call "TIME" (sp_time$)
           sp_primary$    = str(rm_rec$,249%,3%) 
           str(sp_key$,1%,9%)  = sp_cust$
           str(sp_key$,10%,8%) = sp_so$
           str(sp_key$,18%,2%) = sp_ln$
           str(sp_key$,20%,4%) = sp_ln_item$
           str(sp_key$,24%,9%) = str(rm_rec$,22%,9%)
           str(sp_key$,33%,3%) = str(rm_rec$,31%,3%)
           
           read #18, hold, key 1% = sp_key$, using L07340, sp_type1$,   ~
                                      sp_qty1$, eod goto no_ewdsched
L07340:            FMT POS(8), CH(1), POS(82), CH(4)           
                  if sp_type1$ <> "5" then goto not_remake
                  
                  convert sp_qty1$ to sp_qty1%, data goto L07330
L07330:              sp_qty1% = sp_qty1% + 1%
                     convert sp_qty1% to sp_qty$, pic(0000)
                                       
not_remake          
                 delete #18                  
no_ewdsched           
        
        
           put #18, using L07350, sp_status_dte$,  /* Spec Anal St     */~
                                  sp_status$,      /* (PLAN SCH1)      */~
                                  sp_type$,        /* (PLAN SCH2)      */~
                                  sp_cutoff$,      /* Cut Off Day 1-7  */~
                                  sp_route$,       /* Route code       */~
                                  sp_cust$,        /* Customer Code    */~
                                  sp_so$,          /* Sales order No.  */~
                                  sp_ln$,          /* S.O. Line item No*/~
                                  sp_ln_item$,     /* Line Item Piece  */~
                                  str(rm_rec$,22%,9%),/* Bar Code      */~
                                  str(rm_rec$,31%,3%),/* Rmk No.       */~
                                  sp_due$,         /* Sales order Due  */~
                                  sp_part$,        /* Part Number      */~
                                  sp_qty$,         /* line Item Qty    */~
                                  sp_stat$,        /* (PLAN STAT) Plann*/~
                                  sp_usr$,         /* Last Mod User Id */~
                                  sp_dte$,         /* Last Mod Date    */~
                                  sp_time$,        /* Last Mod Time    */~
                                  sp_text$,        /* Line Item Text Id*/~
                                  sp_primary$,     /* Primary Dept     */~
                                  sp_fil$          /* Filler Area      */    
                                  
L07350:     FMT CH(6), CH(1), CH(1), CH(2), CH(5), CH(9), CH(8), CH(2),  ~
                CH(4), CH(9), CH(3), CH(6), CH(25), CH(4), CH(2), CH(3), ~
                CH(6), CH(8), CH(4), CH(3), CH(17) 

            write #18, eod goto L07360

        return
L07360:     err% = 24%
            gosub err_scrn
        return                                       
        
        lookup_dt
             dt_rec% = 0%
             init(" ") dt_key$, dt_bar$, sp_cust$, sp_so$, sp_ln$,sp_ln_item$,~
                       dt_st$, sp_stat$
             str(dt_key$,1%,8%) = str(rm_rec$,22%,8%)
             read #1, key 4% = dt_key$, using L07370, dt_key$, eod goto no_dt
             
L07370:            FMT POS(96), CH(8)
                if str(dt_key$,1%,8%) <> str(rm_rec$,22%,8%) then goto no_dt
                
                get #1, using L07380, dt_bar$, dt_st$, sp_cust$
                
L07380:             FMT POS(24), CH(18), POS(64), CH(2), POS(124), CH(9)
                
                sp_so$ = str(dt_bar$,1%,8%)
                sp_ln$ = str(dt_bar$,9%,2%)
                sp_ln_item$ = str(dt_bar$,11%,4%)
                sp_stat$ = dt_st$
             dt_rec% = 1%                        
        no_dt
        return
        
        lookup_cust
           init(" ") readkey$, desc$, vf$()
           read #19,key = sp_cust$, eod goto no_cust
              get #19, using L07390   , vf$()

L07390:        FMT POS(820), 10*CH(20)
           sp_cutoff$ = str(vf$(3%),1%,2%) /* Cust Delivery Code   */
           sp_route$ =  str(vf$(9%),1%,5%)

           init(" ") readkey$, desc$             
           str(readkey$,1%,9%)   = "PLAN CUTO"
           str(readkey$,10%,15%) = sp_cutoff$
           read #2,key = readkey$, using L12070, desc$, eod goto no_cust

           sp_cutoff$ = "0" & str(desc$,1%,1%)     /* cut off Day 1 thru 7 */
        no_cust
        return

        check_temp_stock                                
            temp_stock% = 0%
            init(" ") readkey$, desc$, rm_part$
            rm_part$ = str(rm_rec$,125%,25%)
            str(readkey$, 1%,9%)  = "TEMPSTOCK"
            str(readkey$,10%,3%) = str(rm_part$,1%,3%)
            str(readkey$,13%,2%) = str(rm_part$,5%,2%)
            str(readkey$,15%,4%) = str(rm_part$,13%,4%)
            str(readkey$,19%,3%) = str(rm_part$,17%,3%)

            read #2,key = readkey$, eod goto not_stock

               temp_stock% = 1%
               str(rm_rec$,13%,1%)  = "0"
        not_stock
        return
                                                     /* (AWD040) - End    */




        update_glass                            /* (APCPLNGR) - File   */
            init(" ") errormsg$, dateout$       /* (EWD014) - Valid Rec*/
            call "TIME" (dateout$)
            read #ft%,hold,key = rm_key$, using L06260 , rm_rec$,          ~
                                                       eod goto L06440
L06260:        FMT CH(256)
               delete #ft%
               set_flag% = 0%                /* (EWD005) - For Audit   */

            if scr_sel% = 5% then goto L06340
L06280:                                      /* (EWD019) - Exception   */
               str(rm_rec$,7%,6%)  = date    /* Glass Scan Date        */
               str(rm_rec$,13%,1%) = "2"     /* Glass Scanned Complete */
                                             /* (EWD001) Close Re-make */
                                             /* (EWD009) Update DTM    */
            if trauma_center$ <> "Y" then goto no_trauma_org
            init(" ") gt_rec$
            str(gt_rec$,1%,1%)  = "0"            /* Status = '0' Open  */
            str(gt_rec$,2%,2%)  = "00"           /* Area = Originator  */
            str(gt_rec$,4%,6%)  = str(rm_rec$,52%,6%) 
            str(gt_rec$,10%,8%) = str(rm_rec$,44%,8%)
            if str(rm_rec$,32%,2%) = "00" and scr_sel% = 4% then        ~
                      gosub update_glass_trauma_org   /* (EWD024)      */

            no_trauma_org
               if str(rm_rec$,61%,4%) = "0000" then gosub calc_remake    ~
                                               else goto L06320 
               goto L06420

L06320:     str(rm_rec$,44%,8%)  = time       /* Completed Time        */
            str(rm_rec$,52%,6%)  = date       /* Completed Date        */
            str(rm_rec$,58%,3%)  = scr_id$    /* Completed User id     */
            str(rm_rec$,61%,4%)  = "    "     /* (EWD013)              */
            goto L06420                       /* (EWD009) - End        */

L06340:     str(rm_rec$,7%,6%)   = date      /* Glass Scan Date        */
            str(rm_rec$,13%,1%)  = "0"       /* Re-Schedule as Re-Make */
            if tempered% = 1% then str(rm_rec$,13%,1%)  = "9"  /* (AWD040)  */
            str(rm_rec$,34%,2%)  = rm_reason$   /* Re-Make Reason Code */
                                             /* (EWD019) Exception     */
            if rep_id% = 1% then goto L06280
                                             /* (EWD019)               */
            rm_num% = 1%
            convert str(rm_rec$,32%,2%) to rm_num%, data goto L06390
L06390:
            convert (rm_num% + 1%) to str(rm_rec$,32%,2%), pic(00)
            str(rm_rec$,44%,8%)  = time       /* (EWD001) Set Re-make  */
            str(rm_rec$,52%,6%)  = date       /* Date/Time Stamp       */
            str(rm_rec$,58%,3%)  = scr_id$    /* User id               */
            str(rm_rec$,61%,4%)  = "0000"     /* Total Hrs/Mins        */
                                              /* (EWD001) Clock Starts */  
            str(rm_rec$,249%,3%) = rm_dept_n$   /* (AWD040) */

                                              /* (AWD040) */
            if tempered% = 1% then gosub update_glass_sched
            if tempered% = 1% and dt_rec% <> 1% then goto L06430

L06420:     str(rm_rec$,14%,8%) = dateout$    /* Time of Status Change */
            str(rm_rec$,36%,6%) = date        /* Date of Status Change */
            str(rm_rec$,42%,2%) = scr_shft$   /* Scanning Shift Code   */
            str(rm_rec$,252%,3%) = str(rm_rec$,31,3%) /* RE-MAKE NO.   */
                                              /* (EWD012) Begin        */
            if str(userid$,1%,2%) = "OV" then                           ~
               str(rm_rec$,65%,1%) = str(userid$,3%,1%) /* 1-9,A-Z     */

            if str(userid$,1%,3%) = "GLS" then                          ~
               str(rm_rec$,65%,1%) = str(scr_shft$,2%,1%)/* Temporary  */
                                              /* (EWD012) for Tracking */
            put #ft%, using L06260, rm_rec$
            write #ft%, eod goto L06430
               tt_unit% = tt_unit% + 1%          /* Calc Scanned Units */

            
            if trauma_center$ <> "Y" then goto no_trauma
               if set_flag% = 1% then gosub update_ewdplngt   /* (EWD024) */
               if str(rm_rec$,32%,2%) = "00" and scr_sel% = 4% then     ~
                      gosub update_ewdplngt                /* (EWD024) */
               if scr_sel% = 5% then                                    ~
                      gosub update_glass_trauma            /* (EWD024) */

            no_trauma

            if set_flag% = 1% then gosub update_glass_audit

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
            if str(rm_rec$,13%,1%) <> "2" then return
            read #10,hold,key = str(rm_rec$,22%,9%),                     ~
                                          eod goto update_glass_rack_done
            put #10, using L06500, "1"
L06500:       FMT POS(58), CH(1)
 
            rewrite #10
        update_glass_rack_done
        return                                   /* (EWD010) - Done    */

        update_glass_trauma                      /* (EWD011) - Begin   */
            if str(rm_rec$,13%,1%) <> "0" then return
                                                 /* Only Re-Makes      */
                                                 /* All Dept's and all */
                                                 /* Shifts             */
                                                 /* (EWD013) Shift(2,3)*/
                                                 /*    Not turned on   */
                                                 /* (EWD024) Turn on   */
                                                 /*    All Shifts      */
REM         if str(rm_rec$,42%,2%) = "03" then return
REM         if str(rm_rec$,42%,2%) = "02" then return
        
            init(" ") gt_rec$
            str(gt_rec$,1%,1%)  = "0"            /* Status = '0' Open  */
            str(gt_rec$,2%,2%)  = "00"           /* Area = Originator  */
            str(gt_rec$,4%,6%)  = date           /* Date Scanned (In)  */
            str(gt_rec$,10%,8%) = time           /* Time Scanned (In)  */  
        update_glass_trauma_org
            str(gt_rec$,18%,9%) = str(rm_rec$,22%,9%)
                                                 /* Re-Make Barcode     */
            str(gt_rec$,27%,3%) = str(rm_rec$,31%,3%)
                                                 /* Glass Re-Make Number*/ 
                                                 /* (EWD024) - Change place */
                                                 /* of re-make num & model  */

            str(gt_rec$,30%,3%) = str(rm_rec$,249%,3%)
                                                 /* Production Dept Code*/
            str(gt_rec$,33%,3%) = "TOP"
            if str(rm_rec$,111%,1%) = "B" then str(gt_rec$,33%,3%) = "BOT"
                                                 /* View - Top or Bot   */
            gt_part$ = str(rm_rec$,125%,25%)     /* MFG Part Number     */
            init(" ") code$, table$
            code$ = str(gt_part$,4%,1%)          /* Color Code          */
            table$ = "COLOR    "
            gosub check_code
            str(gt_rec$,36%,6%)     = str(desc$,6%,6%)
                                                 /* Color Long Form     */
            str(gt_rec$,42%,7%)     = str(rm_rec$,234%,7%)
                                                 /* Muttin Code         */
            str(gt_rec$,49%,9%)     = str(rm_rec$,85%,9%)
                                                 /* Cut Width           */ 
            str(gt_rec$,58%,9%)     = str(rm_rec$,94%,8%)   
                                                 /* Cut Height          */
            init(" ") code$, table$, dt_key$
            code$  = str(gt_part$,5%,2%)
            table$ = "GLASS    "
            gosub check_code
            str(gt_rec$,67%,4%)     = str(desc$,1%,3%) & " "
                                                 /* Glass Type Code     */
            str(gt_rec$,71%,5%)     = str(rm_rec$,242%,5%)
                                                 /* Production Seq No.  */ 
            str(gt_rec$,76%,8%)     = str(rm_rec$,163%,8%)
                                                 /* Sales Order No.     */
            str(dt_key$,1%,8%)      = str(gt_rec$,76%,8%)
            read #1,key > dt_key$, using Lxxx00, dt_key$, dt_cust$,       ~
                                                          eod goto Lxxx10
Lxxx00:        FMT POS(24), CH(23), POS(124), CH(9)
            if str(gt_rec$,76%,8%) <> str(dt_key$,1%,8%) then goto Lxxx10        
               str(gt_rec$,84%,9%)  = dt_cust$ 
                                                 /* Customer Code       */
Lxxx10:     str(gt_rec$,93%,10%)    = str(rm_rec$,1%,6%) & "    "
            call "DATFMTC" ( str(gt_rec$,93%,10%) )
                                                 /* Formatted Prod Dte  */           

            str(gt_rec$,103%,3%)    = str(rm_rec$,72%,3%)
                                                 /* Model Code          */

            str(gt_rec$,106%,1%)    = "N"  
                                                 /* Label Printed (N)o  */
            str(gt_rec$,107%,13%)   = str(rm_rec$,150%,13%)
                                                 /* Window Width/Height */
            str(gt_rec$,120%,2%)    = str(rm_rec$,42%,2%)
                                                 /* Shift Code of Scan  */
            str(gt_rec$,122%,3%)    = str(rm_rec$,58%,3%)
                                                 /* User Id of Requester*/
            lk$ = str(gt_part$,12%,1%) 
            p%  = pos("0123456789" = lk$ )
            if p% = 0% then str(gt_rec$,125%,1%) = "C"
                                                 /*Test/Set Contour Grid*/

            dt_txt$ = str(rm_rec$,112%,4%)  
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
            str(gt_rec$,191%,4%)    = str(rm_rec$,171%,4%)
                                                 /* Spacer Thickness    */
            str(gt_rec$,195%,6%)    = "      " 

        dataput_gt                               /* (EWD024)            */
                                                 /* Filler Area         */   
            write #8, using L07300, gt_rec$, eod goto L07310
L07300:         FMT CH(200)       
L07310:     gosub remake_display         
        return
                                                 /* (EWD011) - End     */

        update_glass_audit                       /* (EWD005) - Begin   */
            set_flag% = 0%
            init(" ") rma_rec$
            str(rma_rec$,1%,6%)   = str(rm_rec$,52%,6%)    /* Date     */
            str(rma_rec$,7%,9%)   = str(rm_rec$,22%,9%)    /* Bar Code */
            str(rma_rec$,16%,3%)  = str(rm_rec$,31%,3%)    /* Rmk No.  */
            str(rma_rec$,19%,8%)  = str(rm_rec$,44%,8%)    /* Rmk Time */
            str(rma_rec$,27%,3%)  = str(rm_rec$,58%,3%)    /* User Id  */
            str(rma_rec$,30%,4%)  = str(rm_rec$,61%,4%)    /* Completed*/
            str(rma_rec$,34%,3%)  = str(rm_rec$,249%,3%)   /* Dept     */
            str(rma_rec$,37%,2%)  = str(rm_rec$,34%,2%)    /* Reason Cd*/
            str(rma_rec$,39%,26%) = " "

                                                       /*  (AWD040)      */
            write #fr%, using L06510, rma_rec$, eod goto L06520
L06510:         FMT CH(64)

REM            gosub update_ewdplngt                          /*  (EWD024)  */
        return
L06520:     err% = 22%                                     /* (EWD013) */
            gosub err_scrn
        return 
                                                  /* (EWD005) - End    */
        calc_remake                   /* (EWD001) - New Subroutine */
            c_hrs% = 0% : c_min% = 0% : days% = 0%
            r_hrs% = 0% : r_min% = 0% : tot_min% = 0%
            ct_min% = 0% : rt_min% = 0% : ret% = 0%   
            calc_time$  = time
            calc_dte$   = date
            rm_rm_dte$  = str(rm_rec$,52%,6%)
            call "DATE" addr("G-",rm_rm_dte$,calc_dte$,days%,ret%)

            convert str(calc_time$,1%,2%) to c_hrs%, data goto L06525
        
L06525:     convert str(calc_time$,3%,2%) to c_min%, data goto L06530

L06530:     if str(calc_time$,5%,2%) > "30" then c_min% = c_min% + 1%
            convert str(rm_rec$,44%,2%) to r_hrs%, data goto L06535

L06535:     convert str(rm_rec$,46%,2%) to r_min%, data goto L06540

L06540:     if str(rm_rec$,48%,2%) > "30" then r_min% = r_min% + 1%
                              /* Convert to total Minutes - complete */
            ct_min% = ( (c_hrs% + (24% * days%)) * 60%) + c_min%
                              /* Convert to Total Minutes - Re-make  */
            rt_min% = ( r_hrs% * 60% ) + r_min%

            tot_min% = (ct_min% - rt_min%)
            r_hrs% = int(tot_min%/60%)
            r_min% = tot_min% - (60% * r_hrs%)
            convert r_hrs% to str(rm_rec$,61%,2%), pic(00)
            convert r_min% to str(rm_rec$,63%,2%), pic(00)

            set_flag% = 1%          /* (EWD005) - Set Flag to Update */
        return                      /* (EWD001) - End Subroutine     */

        update_track                            /* (APCPLNDT) - File   */
            init(" ") errormsg$, prevcode$, dateout$
            call "TIME" (dateout$)
            dt_key$ = sav_key$
            read #1,hold,key = dt_key$, using L06640 , dt_rec$,            ~
                                                     eod goto L06670
L06640:        FMT CH(256)
               delete #1

L06670:     str(dt_rec$,53%,6%)  = date       /* Product Scan Date     */
            str(dt_rec$,64%,2%)  = upd_st$    /* Product Scanned       */
            str(dt_rec$,104%,2%) = scr_shft$  /*                       */
            str(dt_rec$,116%,8%) = dateout$   /* Time Product Scanned  */
            get str(dt_rec$,133%,8), using L06720 , dt_sale /* PRICE     */
L06720:         FMT PD(14,4)
            put #1, using L06640 , dt_rec$
            write #1, eod goto L06850
            prevcode$ = dt_bar$
            if dt_sale <> 0.0 then goto L06790     /* Count Charged Items*/
            if str(dt_rec$,215%,1%) = "Y" then goto L06800 /* Skip Parts */
            if str(dt_rec$,214%,1%) <> "0" then goto L06800 /* Skip Sashs*/
L06790:        tt_unit% = tt_unit% + 1%          /* Calc Scanned Units */
L06800: ad_dept$ = str(dt_rec$,42%,3%)           /* Scanning Department*/
        ad_proc$ = str(dt_rec$,45%,2% )          /* Scanning Process   */
	call "AWDCOMSB" (dt_rec$,#42,scr_dept$,scr_id$)
        gosub update_audit
        gosub ok_scrn
        return
L06850:    err% = 6% 
           str(err$(6%),41%,3%)  = str(dt_rec$,42%,3%)
           gosub err_scrn
           str(errormsg$,41%,3%) = str(dt_rec$,42%,3%)
        return

        update_shipping                         /* (APCPLNDT) - File   */
                                                /* (AWD033) - Label    */
            if scr_sel% = 2% then gosub print_label
            if scr_sel% = 2% and errormsg$ <> " " then return
                                                /* No Label Exit       */
                                                /* (AWD033)            */ 
            init(" ") errormsg$, prevcode$, dateout$, dt_key$, dt_rec$
            call "TIME" (dateout$)
            str(dt_key$,1%,18%) = barcode$
        update_shipping_nxt
            read #1,hold,key > dt_key$, using L06990 , dt_rec$,            ~
                                                     eod goto L07200
L06990:        FMT CH(256)
            dt_key$ = str(dt_rec$,24%,23%)
            if str(dt_key$,1%,18%) <> barcode$ then goto L07120
                                              /* (RHHTEST)             */
               delete #1
               str(dt_rec$,53%,6%) = date     /* Product Scan Date     */
               str(dt_rec$,64%,2%) = upd_st$  /* Product Scanned       */
               str(dt_rec$,116%,8%)= dateout$ /* Time Product Scanned  */
               put #1, using L06990 , dt_rec$
               write #1, eod goto L07200
               ad_dept$ = str(dt_rec$,42%,3%)    /* Scanning Department*/
               ad_proc$ = str(dt_rec$,45%,2% )   /* Scanning Process   */
               gosub update_audit
	       call "AWDCOMSB" (dt_rec$,#42,scr_dept$,scr_id$)

                                               /* (RHHTEST)            */
               goto update_shipping_nxt
L07120: prevcode$ = barcode$
            tt_unit% = tt_unit% + 1%             /* Calc Scanned Units */
            ad_dept$ = "106"                     /* Staging            */
            if scr_sel% = 3% then ad_dept$ = "108" /* Loading          */
            ad_proc$ = "01"
            gosub update_audit
        gosub ok_scrn
                                                 /* (AWD030)           */
        REM if scr_sel% = 2% then gosub print_label

                                                 /* (AWD030)           */ 
        return
L07200:    err% = 26% : gosub err_scrn
        return
REM L07210     err% = 29% : gosub err_scrn
        return
                                                 /* (AWD033)          */
        update_shipping_appian
           init(" ") awd_app_key0$, errormsg$, rec$(), calc_time$, app_scan_tme$

           calc_time$ = time                     /* Military - HHMMSSXX */ 
           app_scan_tme$ = str(calc_time$,1%,4%)
           awd_app_key0$ = barcode_shp$
           read #12,hold,key 0% = awd_app_key0$, using LAPP_3, rec$(), eod goto LAPP_4
LAPP_3:       FMT 3*CH(200)
           delete #12

           str(rec$(),522%,6%) = date            /* Scan Date          */
           str(rec$(),528%,4%) = app_scan_tme$   /* Scan Time-Military */
           put #12, using LAPP_3, rec$()
           write #12, eod goto LAPP_4
           gosub ok_scrn
        return
LAPP_4:    err% = 27% : gosub err_scrn
        
        return
                                                 /* (AWD028)        */ 

                                                 /* (AWD031)        */
        update_staging_appian
           init(" ") awd_app_key0$, errormsg$, calc_time$, app_scan_tme$

           calc_time$ = time                     /* Military - HHMMSSXX */ 
           app_scan_tme$ = str(calc_time$,1%,4%)
           awd_app_key0$ = str(rec$(),1%,20%)
           init(" ") rec$()
           read #12,hold,key 0% = awd_app_key0$, using LAPP_5, rec$(),  ~
                                                 eod goto LAPP_6
LAPP_5:       FMT 3*CH(200)
           delete #12

           str(rec$(),579%,6%) = date            /* Scan Date          */
           str(rec$(),585%,4%) = app_scan_tme$   /* Scan Time-Military */
           put #12, using LAPP_5, rec$()
           write #12, eod goto LAPP_6

        return
LAPP_6:    err% = 27% : gosub err_scrn
        
        return
                                                 /* (AWD031) - (AWD033)*/
        update_audit                             /* Mod for Staging    */
 
            init(" ") ad_rec$, ad_time$, ad_key$, ad_rec1$
            call "TIME" (ad_time$)
            str(ad_rec$,1%,18%) = barcode$                /* Barcode   */
            str(ad_rec$,19%,6%) = date                    /* Scan Date */
            str(ad_rec$,25%,3%) = ad_dept$                /* Department*/
            str(ad_rec$,28%,2%) = ad_proc$                /* Process   */
            str(ad_rec$,30%,2%) = scr_shft$               /* Shift Code*/
            str(ad_rec$,32%,2%) = upd_st$                 /* Status    */
                                                          /* (AWD033)  */
            if override% = 1% then str(ad_rec$,32%,2%) = "12"
                                                          /* (AWD033)  */
            str(ad_rec$,34%,18%)= barcode$                /* Barcode   */
            str(ad_rec$,52%,8%) = ad_time$                /* Time Stamp*/
            str(ad_rec$,60%,3%) = scr_id$                 /* User Id   */
                                                          /* (AWD033)  */
                                                          /* Staging ID*/
                                                          /* (AWD033)  */
            str(ad_rec$,63%,2%) = "  "                    /* Filler    */
            ad_key$ = str(ad_rec$,19%,33%)
            read #5,hold,key = ad_key$, using L07430,ad_rec1$,eod goto L07420
               delete #5
L07420:     put #5, using L07430 , ad_rec$
L07430:       FMT CH(64)
            write #5, eod goto L07460
        return
L07460:    err% = 7% : gosub err_scrn
           override% = override% + 1%                     /* (AWD033)  */
        return

        update_ewdplngt
            gt_load% = 0%
REM	    if str(rm_rec$,32%,2%) = "00" then                ~
REM                                gosub update_glass_trauma_org
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
            str(gt_rec$,4%,6%)  = str(rm_rec$,52%,6%) 
            str(gt_rec$,10%,8%) = str(rm_rec$,44%,8%)
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

            if tt1% > tt2% then tt2% = tt2% + 1440% /* Correct with 
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
        return                                    /*  (AWD035) - Take out OK screen */


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
            CALL "PAUSE" ADDR(50%)
        return

        startover
            u3% = 2%
REM            call "STARTOVR" (u3%)
REM            if u3% = 1% then return
            errormsg$ = " "
            edit% = 0%
        return clear all
        goto main

        exit_program
            call "SHOSTAT" ("One Moment Please")
        end

        REM *************************************************************~
            *       Production, Staging, Loading Display Screen         *~
            *************************************************************

                                                 /* (AWD028)            */
                                                 /* (AWD035) - Numerious Mods to Screen */
        deffn'100(fieldnr%)
            gosub set_screen_2
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(08),~
               at (01,12), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(20),~
                                                                         ~
               at (04,02), fac(hex(84)), tt_unit$               , ch(20),~
                                                                         ~
               at (05,02), fac(hex(84)), fld$(1%)               , ch(20),~
               at (05,02), fac(lfac$(1%)), barcode_shp$         , ch(20),~
               at (05,25), fac(lfac$(2%)), wandchar_shp$        , ch(01),~
                                                                         ~
               at (06,02), fac(lfac$(3%)), barcode$             , ch(18),~
               at (06,25), fac(lfac$(4%)), wandchar$            , ch(01),~
                                                                         ~
                                                                         ~
               at (07,02), fac(hex(a4)),   inpmessage$          , ch(20),~
               at (08,02), fac(hex(8c)),   pf$(1%)              , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 1% then L07500
                  gosub startover
L07500:
/*  (AWD035)  -  Take out function keys  */
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_screen_2
/*  (AWD035) - Numerious Mods shorten messages  */
            if scr_sel% = 1% then copy pd$() to xx$()
            if scr_sel% = 2% then copy st$() to xx$()
            if scr_sel% = 3% then copy ld$() to xx$()

            tt_unit$ = "Scanned[ XXXXXX ]"
            convert tt_unit% to str(tt_unit$,10%,6%), pic(######)

            init(" ") dateout$
            inpmessage$ = inp_text$(fieldnr%)
            call "TIME" (dateout$)
                                                      /* (AWD028)       */                 
            fld$(1%)      = "Ship & Prod Barcode:"
                                                      /* (AWD028)       */

            pf$(1%) = "(1)StartOver(2)Exit"
            pfkeys$ = hex(0102ffffffffffffffffffffffffffff00)
                                                      /* (AWD028)       */
            if fieldnr% <> 1% then goto L08400
               init(" ") barcode$, wandchar$
               lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
               lfac$(3%) = hex(84) : lfac$(4%) = hex(84)
        return
L08400:     lfac$(1%) = hex(84) : lfac$(2%) = hex(84)
            lfac$(3%) = hex(81) : lfac$(4%) = hex(99)
                                                      /* (AWD028)       */ 
        return
                                                      /* (AWD030)       */
                                                      /* (AWD035) Mod to shorten screen */
        deffn'150(fieldnr%)
            gosub set_screen_2a
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(08),~
               at (01,12), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(20),~
                                                                         ~
               at (03,02), fac(hex(84)), tt_unit$               , ch(24),~
                                                                         ~
               at (04,02), fac(hex(84)), fld$(1%)               , ch(25),~
               at (05,02), fac(lfac$(1%)), barcode$             , ch(18),~
               at (05,21), fac(lfac$(2%)), wandchar$            , ch(01),~
                                                                         ~
               at (07,02), fac(hex(a4)),   inpmessage$          , ch(20),~
               at (08,02), fac(hex(8c)),   pf$(1%)              , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 1% then L08500
                  gosub startover
/*  (AWD035)  -  Take out function keys  */

L08500:
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_screen_2a
/*  (AWD035) - Numerious Mods shorten messages  */
            if scr_sel% = 1% then copy pd$() to xx$()
            if scr_sel% = 2% then copy st$() to xx$()
            if scr_sel% = 3% then copy ld$() to xx$()

            tt_unit$ = "Scanned[ XXXXXX ]"
            convert tt_unit% to str(tt_unit$,10%,6%), pic(######)

            init(" ") dateout$
            inpmessage$ = inp_text$(2%)
            call "TIME" (dateout$)
            lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
            fld$(1%)      = "Barcode To Scan  :"
            if keyhit% = 3% then fld$(1) = "Barcode to RE-Print:"
            fld$(2%)      = "Prv:"
            pf$(1%) = "(1)StOv(2)Ex(3)Re-Pr"
            pfkeys$ = hex(010203ffffffffffffffffffffffffff00)
        return
                                                      /* (awd030)       */

        REM *************************************************************~
            *        Glass and Glass Re-Make Scanning Screen            *~
            *************************************************************
                                                       /* (EWD018)      */
        deffn'200(fieldnr%)                         /* Chg to 12 Digits */
            gosub set_screen_3
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,12), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(20),~
                                                                         ~
               at (03,02), fac(hex(84)), tt_unit$               , ch(24),~
                                                                         ~
               at (04,02), fac(hex(84)), fld$(1%)               , ch(07),~
               at (04,09), fac(lfac$(1%)), rm_bar1$             , ch(09),~
               at (04,19), fac(lfac$(2%)), bar_wand$            , ch(01),~
                                                                         ~
               at (05,02), fac(hex(84)), fld$(2%)               , ch(25),~
               at (05,09), fac(lfac$(3%)), rm_reason$           , ch(02),~
               at (05,19), fac(lfac$(4%)), reason_wand$         , ch(01),~
                                                                         ~
/*AWD040*/     at (06,02), fac(hex(84)), fld$(3%)               , ch(25),~
/*AWD040*/     at (06,09), fac(lfac$(5%)), rm_dept_n$           , ch(03),~
/*AWD040*/     at (06,19), fac(lfac$(6%)), dept_wand$           , ch(01),~
                                                                         ~
               at (07,02), fac(hex(a4)),   inpmessage$          , ch(20),~
               at (08,02), fac(hex(8c)),   pf$(1%)              , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 1% then L09070
                  gosub startover
/*  (AWD040)  -  Take out function keys  */

L09070:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_screen_3
            inp_text$(1%) = "Scan GLS Barco"
            inp_text$(3%) = "Enter Rmk Num "
            inp_text$(5%) = "Department    "
/*  (AWD040) - Numerious Mods shorten messages  */

            tt_unit$ = "Scanned[ XXXXXX ]"
            convert tt_unit% to str(tt_unit$,10%,6%), pic(######)

            init(" ") dateout$
            inpmessage$ = inp_text$(fieldnr%)
            call "TIME" (dateout$)
                                           /* (EWD013)- Set up Screen */
            fld$(1%)      = "Glass: "                  /* (AWD040) */
            fld$(2%)      = "RsnCd: "                  /* (AWD040) */
            fld$(3%)      = "Depar: "                  /* (AWD040) */
            if rm_reason$ = " " then rm_reason$ = "90" 
            if scr_sel% = 4% then init(" ") fld$(2%)

            pf$(1%) = "(1)StartOver(2)Exit"
            pfkeys$ = hex(0102ffffffffffffffffffffffffffff00)
                                                /* (EWD015)    */
        return

REM        utility_scan
REM           rpt% = 0%
REM           call "APCPLB40" (rpt%)
REM        return



        err_scrn                    /* Display this Message for Errors */
            errormsg$ = err$(err%)
REM            print at(03,02);hex(84);"                  ";
REM            print at(03,02);hex(84);her$(err%);
REM            print at(04,02);hex(84);ee$(1%);
REM            print at(05,02);hex(84);ee$(2%);
REM            print at(06,02);hex(84);ee$(3%);
REM            print at(07,02);hex(84);ee$(4%);
REM            print at(08,02);hex(84);ee$(5%);
REM            print at(09,02);hex(84);ee$(6%);
REM            print at(10,02);hex(84);ee$(7%);
REM            for i% = 1% to b_max%
REM                print at(13,19);bell;
REM            next i%
         gosub'500(0%)
         err% = 0%
REM      call "PAUSE" ADDR(100%)
        return

/*  (AWD035) - Numerious Mods shorten messages  */
        deffn'500(fieldnr%)
            gosub set_error
            accept                                                       ~
               at (01,02), fac(hex(94)), errormsg$              , ch(20),~
                                                                         ~
               at (02,02), fac(hex(84)), ee$(1%)                , ch(20),~
               at (03,02), fac(hex(84)), ee$(2%)                , ch(20),~
               at (04,02), fac(hex(84)), ee$(3%)                , ch(20),~
               at (05,02), fac(hex(84)), ee$(4%)                , ch(20),~
               at (06,02), fac(hex(84)), ee$(5%)                , ch(20),~
               at (07,02), fac(hex(84)), ee$(6%)                , ch(20),~
                                                                         ~
               at (07,02), fac(hex(a4)),   inpmessage$          , ch(20),~
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
/*  (AWD035) - Numerious Mods shorten messages  */

                                                      /* (awd030)       */        

        check_selection

           init(" ") upd_st$
           code% = 0%
           convert scr_sel$ to scr_sel%, data goto L11060

           if scr_sel% < 1% or scr_sel% > 8% then goto L11060
                                                     /* (AWD028)       */
                                                     /* (AWD030)       */
                                                     /* (AWD040)       */
              if scr_sel% <> 3% and scr_sel% <> 2% and scr_sel% <> 5%  ~
                             then goto L11062
                                                     /* (AWD030)       */
                                                     /* (AWD028)       */
                                   /* Only used by glass Dept (EWD001) */
              if userid$ <> "SCN" then goto L11050
                 if scr_sel% = 4% then goto L11065
                                                     /* (EWD012) OV1   */   
L11050:       if userid$ = "GLS" then goto L11052    /* Temporary      */ 
              if str(userid$,1%,2%) <> "OV" then goto L11055

L11052:          if scr_sel% < 4% then goto L11068
                 if scr_sel% > 5% then goto L11068   /* (EWD012) - Fix */
                                                     /* (EWD001) - End */ 
L11055:       
              
              
                                                     /* (AWD032)       */
              if scr_sel% = 2% and scr_prt$ = "A" then                  ~
                       str(scrn_title$,30%,11%) = "Printer (A)"

              if scr_sel% = 2% and scr_prt$ = "B" then                  ~
                       str(scrn_title$,30%,11%) = "Printer (B)"
                                                     /* (AWD032)       */
    
              if scr_sel% = 3% then str(scrn_title$,29%,11%) =           ~
                                                      "Load= "&scr_load$
              code% = 1%                           /* (EWD004) - Begin */
        on scr_sel% goto prod, stagged, loaded, glass, gls_rmk, rga,     ~
                               log_maint, hold_area

           goto L11060

        prod
              upd_st$ = "12" : upd_st% = 12%    /* Production Complete */
              return
        stagged
              upd_st$ = "14" : scr_dept$ = "106"/* Staged Complete    */
              upd_st% = 14%
              return
        loaded
              upd_st$ = "16" : scr_dept$ = "108"/* Loaded              */
              upd_st% = 16%
              return
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
/*  (AWD035) - Numerious Mods shorten messages  */
                                                  /* (EWD004) - End    */
L11060:    errormsg$ = "(Error) Scan Sel?"
           goto L11070                            /* (EWD001)          */
                                                  /* (AWD028)          */
L11062:    errormsg$ = "(Error) Appian Lbl"
           goto L11070
                                                  /* (AWD028)          */ 
L11065:    errormsg$ = "(Error) Glass Scan?"
           goto L11070
L11068:    errormsg$ = "(Error) Product Scan"
L11070:    REM    gosub error_prompt                     /* (EWD001)          */
           code% = 0%
        return

        check_reason              /* Codes "01" TO "08" Vinyl Production*/
           table$ = "PLAN REMK"   /*       "11" to "18" Glass House     */
                                  /*       No Longer any repair         */
           init(" ") code$ 
           code$  = rm_reason$  : rm_reason% = 99%
                                  /* (AWD039) Special Shapes Remakes    */
                                  /* C0 to C6  Codes                    */
           if str(rm_rec$,249%,3%) = "043" and str(rm_reason$,1%,1%) = "C" ~
                                  then goto L11175 

           convert rm_reason$ to rm_reason%, data goto L11150 

L11150:    if userid$ = "GLS" then goto L11152   /* Temporary           */
           if userid$ = "CMG" then goto L11152   /* Temporary           */     
                                                 /*   (AWD038)          */
REM           if str(userid$,1%,2%) <> "OV" then goto L11160
           if str(userid$,1%,2%) <> "OV" and str(userid$,1%,2%) <> "GF"  ~
                   and str(userid$,1%,3%) <> "RCV" then goto L11160
                                                 /* Valid for Glass house */
                                                 /*      (EWD032)         */
L11152:       if (rm_reason% > 10% and rm_reason% < 19%) or              ~
                                    rm_reason% = 20% then goto L11170
                                                      /* (AWD041) */
               if rm_reason% > 89% and rm_reason% < 100% then goto L11170
             
              rm_reason$ = "99"                  /* Not Valid for Glass */ 
                                                
                                                 /* Check Production    */
L11160:    if rm_reason% > 8% then rm_reason$ = "99"
                                                 /* Valid 1 thru 8      */
L11170:    if scr_sel% = 5% and rm_reason% = 0% then rm_reason$ = "99" 
                                                 /* (EWD008) - Not Valid*/
L11175:       gosub check_code                   /* (AWD038)            */
              if rm_reason$ = "99" then code% = 0%
              if code% = 0% then goto L11220
                 reason_d$ = desc$
        return
L11220:    errormsg$ = "Reason Cde Glass RM"
           code% = 0% : check% = 0%
        return                                   /* (AWD030)           */

                                                 /* (EWD005) - Begin   */ 
        check_specials                           /* Tempered Glass and */
           rm_part$ = str(rm_rec$,125%,25%)      /* Special Lighting   */
           rm_gls$  = str(rm_part$,5%,2%)        /* Glass Type         */
           rm_lit$  = str(rm_part$,7%,2%)        /* Liting             */               
           table$ = "PLAN TEMP"
           code$  = rm_gls$                      /* Check Tempered Gl  */
           gosub check_code
           if code% = 0% then goto check_spec_lit
           if code% = 1% then tempered% = 1%     /*    (AWD040)        */
           if tempered% = 1% then code% = 0%
           if str(rm_part$,1%,1%) = "3" then goto check_patio_temp
           if str(rm_rec$,249%,3%) = "104" or            ~
              str(rm_rec$,249%,3%) = "043" then tempered% = 0%
           if tempered% = 0% then code% = 1%   
           if code% = 1% then goto L11250              
                                                 /*    (AWD040)        */


check_spec_lit:                                    /* Check Special Lit  */
           if rm_gls$ = "89" or rm_gls$ = "99" then goto L11260

           if rm_lit$ = "83" or rm_lit$ = "84" or rm_lit$ = "85" then    ~
                                                  goto L11260 
           if rm_lit$ = "86" or rm_lit$ = "87" or rm_lit$ = "88" or      ~
                                rm_lit$ = "99" then goto L11260
                                                    /*  (EWD025)          */
REM           if rm_lit$ = "A0" then goto L11260    /* (EWDRHH)           */ 
           code% = 1% 
        return
L11250:    errormsg$ = "Can't Scn Temp Glass??"
           goto L11270

L11260:    errormsg$ = "Can't Scn SpecLiting??"
L11270:    REM    gosub error_prompt
           code% = 0% : check% = 0%
        return    
                                                   /* (EWD005) - End   */

        check_patio_temp                                 /* (AWD040) */
             if str(rm_part$,1%,3%) = "315" then return
             if str(rm_part$,1%,3%) = "316" then return
             if str(rm_part$,1%,3%) = "335" then return
             if str(rm_part$,1%,3%) = "336" then return
             tempered% = 0%
        return                                           /* (AWD040) */



        check_shift
           table$ = "PLAN SHFT"
           code$  = scr_shft$
           gosub check_code
           if code% = 0% then goto L11330
              scr_shft_d$ = desc$
        return
L11330:    errormsg$ = "(Error) Shift Sel??"
REM              gosub error_prompt
        return

        check_process
           table$ = "PLAN PROC"
           code$  = scr_proc$
           gosub check_code
           if code% = 0% then goto L11440
              scr_proc_d$ = desc$
        return
L11440:    errormsg$ = "(Error) Process Sel??"
           REM    gosub error_prompt
        return

        check_dept
           init(" ") desc$
           if scr_dept$ > "095" then goto L11610
           if scr_sel% <> 1% then goto L11580
           table$ = "PLAN DEPT"
           code$  = scr_dept$
           gosub check_code
           if code% = 0% then goto L11580
              scr_dept_d$ = desc$
        return
L11580:    errormsg$ = "(Error) Depart Sel??"
           REM    gosub error_prompt
        return
L11610:   
                                                /* (AWD032) - Printer */
           if scr_prt$ = " " then scr_prt$ = "A"
           if scr_prt$ <> "A" and scr_prt$ <> "B" then scr_prt$ = "A"

           if scr_dept$ = "108" then scr_prt$ = " "
                                             
           if scr_dept$ = "106" and scr_prt$ = "A" then                 ~
                          scr_dept_d$ = "Staging Department (Printer=A)" ~
                     else scr_dept_d$ = "Staging Department (Printer=B)"
                                                /* (AWD032) - Printer */

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
           if scr_id$ = "XXX" then goto L11770    /* (EWD003) - 06/04/98 */       
           init(" ") scr_id_d$
           read #6,key = scr_id$, using L11750, scr_id_d$,eod goto L11770
L11750:       FMT POS(4), CH(30)
           code% = 1%
        return
L11770:    errormsg$ = "(Error) Invalid User?"
           REM    gosub error_prompt
           init(" ") scr_id$, scr_id_d$
        return

        check_load
           code% = 0%
           if scr_dept$ <> "108" then goto L11960
           if userid$ = "BYP" then goto L11960  /* (EWD002) - 05/26/98 */
                                                /* (AWD029) - 06/05/03 */
           if userid$ = "APN" and str(scr_id$,1%,2%) = "BY" then        ~
                                                            goto L11960
           if userid$ = "SF1" and str(scr_id$,1%,2%) = "BY" then        ~
                                                            goto L11960
           if userid$ = "RF1" and str(scr_id$,1%,2%) = "BY" then        ~
                                                            goto L11960
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
L11920:   errormsg$ = "(Error) Inva Ld Num?"
          REM    gosub error_prompt
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

REM        display_codes
REM           call "APCPLN1B" ( table%, #2)
REM        return

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
                                                      /* (EWD007) - Mods */ 
/*  (AWD035) - Take out Error will not fit on screen  */
REM        error_prompt
REM           comp% = 2%
REM           hh$  = "******* (Error) (Error) (Error)  *******"
REM           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
REM           msg$(2%) = errormsg$
REM           msg$(3%) = "Press Any Key To Continue."
REM           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%))
REM        return
                                                      /* (EWD007) - Mods */ 
        open_error                                    /* (EWD016)        */
            errormsg$ = "(Open Error) - File = " & filename$
            REM    gosub error_prompt
        return
                                                      /* (EWD016)        */

        error_display
           return                                   /*  (AWD035)  */
           comp% = 2%
           hh$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press PF(10) Key, To Continue."
           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%))
           if comp% <> 10% then goto error_display
        return
                                                    /* (AWD030)   */
        
        print_label                                 /* (AWD033)   */
            been_here% = 0%
                                                    /* (AWD034)   */
            switch%    = 1%                         /* Staging    */
                                                    /* (AWD032)   */
            if scr_prt$ = "A" then switch% = 1%     /* MFGSTAG    */
            if scr_prt$ = "B" then switch% = 2%     /* MFGSHIP    */
                                                    /* (AWD032)   */

                                                    /* (AWD034)   */
            awd_app_key0$ = all(hex(00))
            str(awd_app_key0$,1%,18%) = barcode$    /* Barcode          */ 
            
            read #12,key 0% > awd_app_key0$, using L14000, rec$(),        ~
                                            eod goto L14100
L14000:        FMT 3*CH(200)

            if barcode$ <> str(rec$(),1%,18%) then goto L14100

            err% = 0%
            call "AWDPLA05" (switch%, been_here%, rec$(), #2, #14, #63, err%)
                if err% <> 0% then gosub print_error
                                                 /* Finished   */
            call "AWDPLA05" (switch%, been_here%, rec$(), #2, #14, #63, 99%)
                if err% <> 0% then gosub print_error

            gosub update_staging_appian
                                                 /* (AWD031)   */
        return
L14100:    err% = 28% 
           gosub err_scrn                        /*  (AWD035)  */
REM           gosub error_display                   /* (AWD031)   */
        return

        print_error  
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
                                                /* (AWD030)   */

                                           /* (AWD033)                */
                                           /* Almost same as update Trk*/
                                           /* Update product complete  */
                                           /* by Staging              */
        update_track_override              /*     (APCPLNDT) - File   */
            init(" ") errormsg$, prevcode$, dateout$
            call "TIME" (dateout$)
            dt_key$ = override_key$
            read #1,hold,key = dt_key$, using L14200 , dt_rec$,            ~
                                                     eod goto L14250
L14200:        FMT CH(256)

               delete #1

L14250:     str(dt_rec$,53%,6%)  = date       /* Product Scan Date     */
            str(dt_rec$,64%,2%)  = "12"       /* Product Complete-Wired*/
            str(dt_rec$,104%,2%) = scr_shft$  /*                       */
            str(dt_rec$,116%,8%) = dateout$   /* Time Product Scanned  */
            get str(dt_rec$,133%,8), using L14300 , dt_sale /* PRICE   */
L14300:         FMT PD(14,4)
            put #1, using L06640 , dt_rec$
            write #1, eod goto L14450
            prevcode$ = dt_bar$
            if dt_sale <> 0.0 then goto L14350   /* Count Charged Items*/
            if str(dt_rec$,215%,1%) = "Y" then goto L14400 /* Skip Parts */
            if str(dt_rec$,214%,1%) <> "0" then goto L14400 /* Skip Sashs*/
L14350:     
        REM    tt_unit% = tt_unit% + 1%          /* Calc Scanned Units */
                                                 /* (AWD033)           */
L14400: ad_dept$ = str(dt_rec$,42%,3%)           /* Scanning Department*/
        ad_proc$ = str(dt_rec$,45%,2% )          /* Scanning Process   */
        gosub update_audit                       /* (Also Changed)     */
        gosub ok_scrn
        return
L14450:    err% = 6% 

           str(err$(6%),41%,3%)  = str(dt_key$,19%,3%)
           gosub err_scrn
           str(errormsg$,41%,3%) = str(dt_rec$,19%,3%)
           override% = override% + 1%
        return
                                                 /* (AWD033)          */

       lookup_main_dept                                      /*  (AWD040)  */
             init(" ") rm_dept_n$, rm_rec$, rm_key$
             rm_key$ = rm_bar1$
             read #3,key > rm_key$, using L20000 , rm_rec$, eod goto not_main
                            


L20000:                 FMT CH(256)

                     if str(rm_rec$,22%,9%) <> rm_bar1$ then goto not_main

                            rm_dept_n$ = str(rm_rec$,249%,3%)
        not_main
        return                                              /*  (AWD040)  */
                                        
        
       check_dept_rmk                                 /*  (AWD040)  */
           table$ = "PLAN DEPT"
           code$  = rm_dept_n$
           gosub check_code

       return                                          /*  (AWD040) */


        
        
