        REM *************************************************************~
            *  Hidden PF Key 32  - Runs APCSCAN2, clock in - out utility*~
            *                                                           *~
            *  Program Name      - AWDSCANN                             *~
            *  Creation Date     - 12/23/96                             *~
            *  Last Modified Date- 11/16/2016                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Last Mod By       - Christie Gregory                     *~
            *                                                           *~
            *  Description       - Scanning Utility used inconjunction  *~
            *                      with the new planning system.        *~
            *                                                           *~
            *  Special Notes     - (APN) Userid is for Staging and      *~
            *                      (NAP) Loading.                       *~
            *                                                           *~
            *                      (BYP) Userid is for the By-Pass Load-*~
            *                      (BYN) ing. It does not require Load  *~
            *                            number.                        *~
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
            *                                                  (AWD037) *~
            *                      New Subroutine (AWDPLA05) to print   *~
            *                      New shipping label.                  *~
            *                                                  (AWD037) *~
            *                                                           *~
            *                                                  (AWD051) *~
            *                      New Database File (AWDPLNCD) Save    *~
            *                      Cross Docking Warranty ID and        *~
            *                      from 1st label scanned and Production*~
            *                      Barcode from the 2nd Label Scanned.  *~
            *                      45 Digit Part Number Must Agree.     *~
            *                                                  (AWD051) *~
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
            *          !   by Wood Surround as Staged Complete.   !     *~
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
            *          !            Complete during Staging. When !     *~
            *          !            product not completed by Dept.!     *~
            *          !            Also, no label don't stage.   !     *~
            * 07/30/03 ! (AWD034) - Mod to turn on 2nd Staging    ! RHH *~
            *          !            for shipping.                 !     *~
            * 09/22/03 ! (AWD035) - Mod to Turn on Tempered Glass ! CMG *~
            *          !            Analysis.                     !     *~
            * 06/04/04 ! (AWD036) - Mod to be able to stage prod  ! CMG *~
            *          !            that has '13' status          !     *~
            * 06/22/04 ! (AWD037) - Mod to the New shipping Label ! RHH *~
            *          !            increase the size of the Prod-!     *~
            *          !            uction Seq. Number.           !     *~
            * 06/24/04 ! (AWD038) - Mod to put APCPLN40 on option ! CMG *~
            *          !                 4 on scanning screen     !     *~
            * 06/30/04 ! (AWD039) - Mod to add 'T' switch to use  ! RHH *~
            *          !            testing labels.               !     *~
            * 07/16/04 ! (AWD040) Mod to increase the size of the ! RHH *~
            *          !            drop number on the Shipping   !     *~
            *          !            label.                        !     *~
            * 07/21/04 ! (AWD041) Mod to automatically update dept! CMG *~
            *          !            '054' just like '044' when    !     *~
            *          !            staging.                      !     *~
            * 08/23/04 ! (AWD042) Mod NOT to automatically complet! CMG *~
            *          !            department '044'.             !     *~
            * 04/18/05 ! (AWD043) Mod to Scanning for the new     ! RHH *~
            *          !            trailer file. Only Loading    !     *~
            *          !            affected.                     !     *~
            * 04/25/05 !          Add Second Control Code to Chg  ! RHH *~
            *          !          Current Drop                    !     *~
            * 05/16/05 !          Mod to not allow override       ! RHH *~
            *          !          backwards. Only forward ie.     !     *~
            *          !          Drops 10, 9, 8, 7, 6, etc.      !     *~
            * 05/25/05 ! (AWD044) Mod to scanning for UPS - Does  ! RHH *~
            *          !            have a trailer.               !     *~
            * 05/25/05 ! (AWD045) Mods for RF Scanners            ! CMG *~
            * 08/01/05 ! (AWD046) Mod to change the override      ! RHH *~
            *          !          Controls. PF(25) = PF(11) and   !     *~
            *          !          PF(30) = PF(17)                 !     *~
            * 08/19/05 ! (AWD047) Correct By-Pass problem when    ! RHH *~
            *          !          status is '15'.                 !     *~
            * 09/19/05 ! (AWD048) Mod to allow status 15 to go    ! CMG *~
            *          !  through staging and get a new label     !     *~
            * 12/28/05 ! (AWD049) Mof for two new printers in     ! RHH *~
            *                     Shipping.                       !     *~
            * 01/01/06 ! (PAR000) CR347 AWDPLA05                  ! RHH *~
            * 03/03/06 ! (PAR001) Mod for the new production label! RHH *~
            *          !    changes. Size Change 640 to 1024      !     *~
            * 03/24/06 ! (PAR002) Mod to Staging scanning for the ! RHH *~
            *          !    new Cross-docking procedure for North !     *~
            *          !    East. Noth Eas Userid's Added. NAP    !     *~
            *          !    and BYN                               !     *~
            * 04/18/06 ! (PAR003) Mod to Cross Docking to Verify  ! RHH *~
            *          !    the Sales Order and Line Item are the !     *~
            *          !    Same.                                 !     *~
            * 04/20/06 ! (PAR004) Mod for the new file size change! RHH *~
            *          !    to APCPLNWT. File size changed to 128 !     *~
            *          !    Mod to add new Printer code 'Z'       ! RHH *~
            *          !    for the North East But Print Here at  !     *~
            *          !    Atrium.                               !     *~
            * 04/21/06 ! (AWD050) mod for rf and sf login         ! CMG *~
            * 05/26/06 ! (AWD051) Mod for the new Cross Dock Data ! RHH *~
            *          !    file update. Update for only Cross    !     *~
            *          !    Docked line item windows.             !     *~
            * 05/31/06 ! (AWD052) Mod for spec test for Dept '054'! RHH *~
            *          !    Same Staging test as Dept '044'.      !     *~
            * 06/09/06 !    Final Testing  - Completed and 'OK'   ! RHH *~
            * 06/15/06 ! (AWD053) Mod to test ofr stock orders in ! RHH *~
            *          !    department '102'.                     !     *~
            * 09/25/06 ! (AWD054) Mod to the file AWDAPPLS Change ! RHH *~
            *          !    the File size for Job Name and Room   !     *~
            *          !    location                              !     *~
            * 10/18/06 ! (AWD055) mod to fix loading problems     ! CMG *~
            *05/23/2007! (AWD056) fix error with rf reprints      ! DES *~
            * 01/07/08 ! (AWD057) mods for new support dept 074   ! CMG *~
            *05/13/2008! (AWD058) mods for prod scan stat=11      ! DES *~
            *09/08/2008! (AWD059) mods complaints                 ! DES *~
            *10/29/2009! (AWD060) add new printer option          ! DES *~
            *03/13/2010! (AWD061) mod to prevent blank status inDT! CMG *~
            *04/15/2013! (AWD063) mod for NTX rf scanners         ! CMG *~
            *03/23/2015! (AWD064) Added new Printer               ! PWW *~
            *05/25/2015! (SR64968) - Mod To Update Production for ! PWW *~
            *          !            Complete during Staging. When !     *~
            *          !            product not completed by Dept.!     *~
            *09/11/2015! (SR68443) - Mod To restrict scanning user! PWW *~
            *          !            ID to new GENCODES table      !     *~
            *          !            AWDSCANN.                     !     *~
            *10/06/2015! SR69112 Mod To give a summary count of   ! PWW *~
            *          !         different status in the pipe-    !     *~
            *          !         line. PF4 will produce a summary !     *~
            *          !         screen via a new sub APCPLD40.   !     *~
            *10/28/2015! SR70180 Mod To add new CrossDock Printer.! PWW *~
            *01/12/2016! (SR70699) mod to cross-dock scanning     ! CMG *~
            *01/19/2016! (SR70701) mod for loading cross-dock     ! CMG *~
            *02/03/2016! (SR72504) mod for Owner Plnt & Crs-Dck   ! CMG *~
            *04/29/2016! (SR74525) mod to fix random things like, ! PWW *~
            *  /  /    !           multi prints, label print for  !     *~
            *  /  /    !           nonstaged units.  Also per Craig,    *~
            *  /  /    !           Boules; certain error messages !     *~
            *  /  /    !           should not be overridden so eaisly.  *~
            *10/05/2016! (CR456) mod for DC Center                ! CMG *~
            *11/16/2016! (CR780) mod to print label in order scan ! CMG *~
            *11/16/2016! (CR781) mod to prt Cart lbls from RF Scn ! CMG *~
            *11/28/2016! (CR780) mod to correct key offset for    ! PWW *~
            *  /  /    !          key #2 in AWDCART. Fix for CR780!     *~
            *12/14/2016! SR78815 mod to delete cart records after ! PWW *~
            *  /  /    !         labels are printed for cart. And !     *~
            *  /  /    !         fixed an error message.          !     *~
            *01/02/2017! Additional Logging                       !     *~
            *07/10/2017! CR1014 New printers for staging in NTX   ! RDB *~
            *08/17/2017! SR81640 Fix J option printer for DC      ! RDB *~
            *09/21/2017! CR890 Add truck load start date & time to! RDB *~
            *          !       AWDAPPLD. And Err msg chg per CMG. !     *~
            *10/13/2017! CR1170 Restrict TX sample scanning.      ! RDB *~
            *10/13/2017! CR1166 Move CART Appian label print from ! RDB *~
            *          !        DC to main plant printing         !     *~
            *10/23/2017! CR1166 Stop printing duplicate           ! RDB *~
            *10/24/2017! SR82415 Free hold on awdappld            ! RDB *~
            *10/23/2017! CR1191 CF1 user reprint option & validate! RDB *~
            *11/09/2017! CR1194 Autocomplete at NC Cart in-transit! RDB *~
            *12/06/2017! CR1123 Add tracking audit chgs           ! RDB *~
            *12/27/2017! CR1207 Tracking carts on trucks          ! RDB *~
            *02/12/2018! CR1207 Add formatting on the cart number ! RDB *~
            *04/01/2018! CR1356 TX checks for correct SKU label   ! RDB *~
            *05/25/2018! SR84903 Write audit cart stage status 12 ! CMG *~ 
            *06/18/2018! CR1567 Limit Staging printer & set default!RDB *~
            *06/28/2018! SR85412 add C and F                      ! RDB *~
            *09/14/2018! CR1698 Restrict TX IDs from Cart Scanning! RDB *~
            *10/04/2018! CR1715 Allow reprint from w/n Cart Scan  ! RDB *~
            *10/04/2018! CR1716 Add dept 064 support dept, like 054!RDB *~
            *10/04/2018! CR1717 Stop cart scan if appian not exist !RDB *~
            *11/08/2018! CR1791 Add back stops for some support    !RDB *~
            *          !        depts and send msg for Tech Pickups!    *~           
            *12/10/2018! CR1810 Add printers Q,R,S,U for TX eol    !RDB *~   
            *01/30/2019! CR1911 New brand for display              !RDB *~ 
            *02/14/2019! CR1918 Use PlyGem PO on label when found  !RDB *~   
            *02/21/2019! CR1936 Remove Branding question Y/N       !RDB *~  
            *07/16/2019! CR2128 Add Ship Block to cartscan skip    !RDB *~  
            *07/19/2019! CR2130 Allow 300 building staging to ptr  !RDB *~   
            *01/19/2021! CR2493 New NC Paint Receiving Option      !RDB *~  
            *05/19/2020! CR2565 Stop scanning wrong load           !RDB *~                  
            *04/23/2020! CR2532 Add new garden window model        !RDB *~   
            *01/19/2021! CR2713 Add PP3 ID to only scan D orders   !RDB *~ 
            *          ! Add ship blocks 3CC and CVP               !RDB *~   
            *03/01/2021! CR2782 Change staging message on Wood Surr!RDB *~  
            *03/17/2021! CR2794 Remove debug statement for PP3 msg !RDB *~   
            *04/12/2021! CR2815 Block CF1 from Cart to Stage       !RDB *~  
            *05/10/2021! CR2825 Add quality check to cart scan     !RDB *~  
            *08/18/2021! CR2883 New shrink wrap Lowes dept         !RDB *~       
            *12/01/2021! CR2960 Black laminate Mocksville location !RDB *~   
            *11/10/2022! CR3194 Check Sales Order first character  !RDB *~			
            *************************************************************

        dim hdr$47, scr$(15%)40,         /* ASKUSER Header             */~
            filename$8,                  /* Use with EWDOPEN - EWD016  */~
            her$(40%)50,                 /* Error Text Display         */~
            scr_sel$1, scr_sel_d$30,     /* Screen Scanning Option     */~
            rep_id$(10%)3,               /* Valid Repair Id's  (EWD019)*/~
            rep_code$(30%)2,             /* Vaild Reason code's(EWD019)*/~
            scr_id$3, msg$(3%)79, hh$40, /* Scanning User Id           */~
            scr_id_d$30,                 /* User Id Name               */~
            scr_dept$3, scr_dept_d$30,   /* Product Line / Dept Code   */~
            scr_prt$1,                   /* Staging Printer A,B(AWD032)*/~
            sav_scr_dept$3,              /* Save Dept          (EWD025)*/~
            scr_shft$2, scr_shft_d$30,   /* Screen Shift Entry         */~
            scr_proc$2, scr_proc_d$30,   /* Product / Dept Process Code*/~
            scr_load$5, scr_load_d$30,   /* Production Load and Descrip*/~
            sc_load$5, ed_load$5,        /* Used by (APCPLC40)         */~
            readkey$24, desc$30,         /* GENERIC KEY                */~
            table$9, code$3,             /* TABLE LOOKUP NAME AND CODE */~
            pfkeys$40,                   /* PF KEYS                    */~
            xx$(7%)50,                   /* Screen Display area Text   */~
            cr$(7%)50,                   /* 'Glass' Message            */~
            ld$(7%)50,                   /* 'Load' Message             */~
            st$(7%)50,                   /* 'Stage' Message Test       */~
            rc$(7%)50,                   /* Receive Paint CR2493       */~
            ps$(7%)50,                   /* Scan 'COMP'lete Text Screen*/~
            ee$(7%)50,                   /* 'STOP' Message Error Text  */~
            rf_ee$(7%)20,                /* Stop for RF     (AWD045)   */~
            rf_msg$(7%)20,               /* Display RF message CR1207  */~
            ad_rec$64, ad_rec1$64,       /* Prod Scanning Audit File   */~
            ad_time$8, ad_key$33,        /* Time Stamp                 */~
            ad_dept$3, ad_proc$2,        /* Aduit Dept and Process     */~
            log_rec$256,                 /* Log Rec                    */~
            modulename$20,               /* what called update audit   */~
            adCrSt$2, adCrDept$3,        /* AD Status & Dept(CR456)    */~
            sav_dept$3,                  /* Dept pass to RFID print    */~
            dt_rec$256, dt_dept$3,       /* Production Detail          */~
            dt_key$23, dt_key3$23,       /* (APCPLNDT)-Tracking File   */~
            dt_part$25,                  /* (APCPLNDT) Part Number     */~
            override_key$23,             /* Override Scanning  (AWD033)*/~
            dt_load$5,                   /* Scanning Load Number - Only*/~
            ap$2,                        /* Save 'AM' or 'PM' of Time  */~
            jdate$7,                     /* Save Todays Julian Date    */~
            err$(100%)55,                /* Defined Error Messages     */~
            err$1,                       /* Label Error Code   (PAR004)*/~
            rf_err$(100%)20,             /* RF Err Msg         (AWD045)*/~
            barcode$18,                  /* Scanned Barcode            */~
            barcode_shp$20,              /* Shipping Barcode   (AWD028)*/~
            awd_app_key0$20,             /* Label Primary Key  (AWD028)*/~
            rec$(4%)256,                 /* Appian Label Record(AWD054)*/~
            app_scan_tme$4,              /* Appian Scan Time   (awd028)*/~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dateout$8,                   /* Time Extract For Screen    */~
            inp_text$(2%)79,             /* Input Prompt Text          */~
            rf_inp_text$(10%)20,         /* RF input text      (AWD045)*/~
            fld$(4%)30,                  /* Field Text                 */~
            rf_fld$(4%)20,               /* RF Field Text      (AWD045)*/~
            errormsg$79,                 /* Error message              */~
            rf_errormsg$20,              /* RF Error Message   (AWD045)*/~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            rf_inpmessage$20,            /* Information Mess   (AWD045)*/~
            prevcode$18,                 /* Previous Bar Code Entered  */~
            lfac$(4%)1,                  /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            rf_pf$(3%)20,                /* RF PF Screen Literal(AWD045)*/~
            scrn_title$40, title$40,     /* Screen Description         */~
            wandchar$1,                  /* Wand Character - Scanner   */~
            wandchar_shp$1,              /* Wand Character - Appian    */~
            /*logmsg$256,                   Wand Character - Appian    */~
            userid$3,                    /* Current User Id            */~
            howship$2,                   /* How ship - check tech pick */~
            ld_ship_block$3,             /* Ship Block number for load */~
            ld_app_rec$128,              /* (awdappld) New Load File  CR890 */~
            ld_app_key0$5,               /* Load No. Primary key  CR890     */~
			prefix_so$1                  /* Prefix sales order              */

                                         /* (PAR002)                   */
        dim cross_warranty1$8,           /* Warranty Id Cross Docking  */~
            cross_warranty$8,            /* " "         for Cross      */~
            cross_warranty2$8,           /* Cross Dock Warranty(AWD051)*/~
            cross_rec$128,               /* Warranty Record    (PAR004)*/~
            cross_rec1$128,              /* Cross Dock Record  (AWD051)*/~
            warranty_key$18,             /* Alt 2 Warr Key-S.O. Line It*/~
            cross_key$23,                /* Key of Scanned Record      */~
            cross_dock_key1$23,          /* Save Detail Key            */~
            cross_dock_dept$3,           /* Save Dept 1st label(AWD053)*/~
            cross_part1$25,              /* Save Part Number           */~
            cross_part$25,               /* Save Part Number fro Cross */~
            cross_sub_part$20,           /* Save Sub Part Number       */~
            cross_sub_info$20            /* Save Sub Info      (PAR004)*/
                                         /* (PAR002)                   */

        dim rlib$8,                      /* Run library for link call  */~
            rvol$6,                      /* Run volume for link call   */~
            run$8                        /* Program to Run (EWD007)    */

        dim                              /* (APCPLNGR) - File          */~
            rm_bar$12,                /* Remake Glass Barcode  (EWD018)*/~
            rm_reason$2, reason_d$30,    /* Glass Remake Reason Code   */~
            calc_time$8,                 /* Use for Re-make Calc(EWD001*/~
                                         /* (APCPLNDT)                 */~
            dt_bar$18, dt_st$2,          /* Bar Code Value             */~
            dt_proc$2,                   /* DT Process code (CR456)    */~
            sav_bar$18,                  /* (SR70699) sav_bar          */~
            dt_sku$9, dt_brand$2,        /* CR1356                     */~
            sav_sku$9,                   /* CR1356 save from read past */~
            dt_cust$9,                   /* CR2128 Customer code       */~
            p_mod$(306%)3, sc_dte$6,     /* Department Models          */~
/*SR69112   p_unt%(306%,3%), tt_unit$24,    Scanned Units each Prod    */~
/*SR69112*/ p_unt%(306%,3%), tt_unit$33, /* Drop Units Load/NoLoad     */~
            p_unts%(306%,3%),            /* Sampl Units Only           */~
            p_untss%(306%,3%),           /* Charge Sashs Only          */~
            p_untpp%(306%,3%),           /* Charge Parts Only          */~
            p_val(306%,3%),              /* Scanned Units Dollar Val   */~
            p_mrp(6%,3%)                 /* MFG Costs                  */

        dim                              /* (AWD043)                   */~
            barcode_trailer$8,           /* Barcode for Trailer Scan   */~
            wandchar_trailer$1,          /* Wand Character Trailer     */~
            tr_key0$20,                  /* Trailer File Primary Key   */~
            tr_rec$128,                  /* Trailer Record             */~
            drop_txt1$18,                /* Current Drop Number        */~
            drop_txt2$18,                /* Scanned Drop Number        */~
            drop$2,                      /* Store Drop Number          */~
            dd$(100%)2,                  /* Store Drops                */~
            dd_ord$(100%)3,              /* Load Order and Status      */~
            test_drop$2,                 /* Use to check Drops         */~
            sav_drop$2, scan_drop$,      /* Save Curr and Scan drop No */~
            ed_drop$2,                   /* Current Drop SR69112       */~
            hld_drop$2,                  /* Hold Drop for Cart Scan    */~
            cartDrp$2                    /* Current drop for Cart Scan */
                                         /* (AWD043)                   */
                                         /* (PAR004)                   */

        dim awd_app_key2$32,             /* (AWD055)                   */~
            schema$8                     /* (SR72504)                  */

        dim                                                              ~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4             /* Additional Info Fields     */
                                         /* (PAR004)                   */
  
        dim                              /* (CR456) Cart Variables     */~
            cartKey$35,                  /* Cart Key                   */~
            cartKey2$18,                 /* Cart Barcode;BrcdeOnly1Cart*/~
            cartNum$8,                   /* Cart Num                   */~
            cartSt$1,                    /* Cart Status                */~
            cartBar$18,                  /* Cart Barcode               */~
            cartDept$3,                  /* Cart Department            */~
            cartAdSt$2,                  /* Cart AD Prod Status        */~
            cartLd$5,                    /* Cart Load                  */~
            errCart$8,                   /* Err Cart Number            */~
            cartDest$10,                 /* Cart Dest                  */~
            cartRec$256,                 /* Cart Rec                   */~
            cartTime$8,                  /* Cart Time                  */~
            cartSeq$8,                   /* Cart Scan Seq    (CR780)   */~
            ad_bar$18
  
        dim upd_st$2, cartsaveDest$10
        
        dim scr_truck$8,                 /* Screen truck number        */~
            cur_truck$8,                 /* Current truck number       */~
            scr_cart$8,                  /* Screen cart number         */~
            cnt$8,                       /* Screen counter             */~
            cnt2$8,                      /* Total carts on Truck       */~
            tk_rec$75,                   /* Record length              */~
            tk_key$31,                   /* Key of tracking file       */~
            tk_key2$9                    /* Key by cart nbr and status */
/* CR1356 */        
        dim xr_key$16,                   /* Sku cross reference key-sku */~
            xr_upc$20,                   /* Sku cross ref UPC           */~
            brand$(99,5)40,              /* Brand Display  (CR1356)     */~
            brand_ok$1,                  /* Flag brand validation       */~
            YNFlag$1,                    /* Flag request to user        */~
            fldx$(2%)30,                 /* Screen display title        */~
            up$(7%)50,                   /* 'UPC Stage' Message         */~
            scrn_upc_title$40,           /* Screen Description          */~
            scrn_YN_title$40,            /* Screen Decription           */~
            uinpmessage$79,              /* Informational Message       */~
            scn_upc$16                   /* Scanned UPC on brand label  */
        
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
            * #4  ! APCPLNDP ! Master Department File                   *~
            * #5  ! APCPLNAD ! (New) Planning Master Audit File         *~
            * #6  ! USERCLMS ! Caelus Master User Def. (USERLCMS)       *~
            * #7  ! APCPLNLD ! Planning/Scheduling Load Master File     *~
            * #8  ! APCPLNWT ! Warranty ID Cross-Ref to S.O.    (PAR002)*~
            *     !          !                                  (PAR004)*~
            * #9  ! AWDPLNCD ! Cross Docing Warranty ID Cross-Ref(AWD051)*~
            * #11 ! AWDTRAIL ! Track Trailer and Load Assigned  (AWD043)*~
            * #12 ! AWDAPPLS ! New Appian Shipping Label File   (AWD028)*~
            * #14 ! EWDPRDLB ! New Production Labels-No File Chg(PAR001)*~
            * #17 ! AWDAPPLD ! Appian Load File                 (CR890) *~
            * #18 ! AWDTKCRT ! Tracking carts on trucks         (CR1207)*~
            * #19 ! AWDSKUXR ! Sku Cross Reference              (CR1356)*~
            * #20 ! BCKMASTR ! S.O. HEADER FILE                         *~
            * #42 ! AWDBAYBW ! New file for complaints          (AWD059)*~
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
                                                   /* (PAR002)         */
                                                   /* (PAR004)         */
            select #8,  "APCPLNWT",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,   keylen =  8,                       ~
                        alt key 1, keypos =  9, keylen = 10, dup,        ~
                            key 2, keypos =  9, keylen = 18

                                                   /* (PAR004)         */
                                                   /* (PAR002)         */
                                                   /* (AWD051)         */
            select #9,  "AWDPLNCD",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,   keylen =  8,                       ~
                        alt key 1, keypos =  9, keylen = 10, dup,        ~
                            key 2, keypos =  9, keylen = 18,             ~
                            key 3, keypos = 92, keylen =  8, dup

                                                   /* (AWD051)         */
                                                   /* (AWD043)         */
            select #11,  "AWDTRAIL",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =  20,                     ~
                        alt key  1, keypos =   21, keylen =  20
                                                   /* (AWD043)         */

                                                   /* (AWD028)         */
                                                   /* (AWD030)         */
                                                   /* (AWD054)         */
            select #12,  "AWDAPPLS",                                     ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =   20,                    ~
                        alt key 1,  keypos = 21,   keylen =  34, dup ,   ~
                            key 2,  keypos = 23,   keylen =  32, dup ,   ~
                            key 3,  keypos = 56,   keylen =  10, dup

                                                   /* (AWD054)         */
                                                   /* (AWD028)         */
                                                   /* (AWD030)         */
                                                   /* (AWD032)         */
                                                   /* (PAR001)         */
            select #14, "EWDPRDLB",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =   35,                    ~
                        alt key  1, keypos = 278, keylen =  23
                                                    /* (PAR001)         */
/* (CR780) change keys */
            select #15,  "AWDCART",             /*(CR456) */             ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    7, keylen =  35,                     ~
                        alt key  1, keypos =    1, keylen =  41,         ~
/*CR780                     key  2, keypos =   16, keylen =  18,      */ ~
                            key  2, keypos =   24, keylen =  18,         ~
                            key  3, keypos =   44, keylen =   5, dup


            select #16,  "AWDPLNAD",                                     ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 19,   keylen = 33,                      ~
                        alt key 1, keypos =  1, keylen = 33
/* CR890 */                        
            select #17, "AWDAPPLD",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   12, keylen =    5,                    ~
                        alt key 1,  keypos =  1,   keylen =  16,         ~
                            key 2,  keypos =  2,   keylen =  15,         ~
                            key 3,  keypos = 17,   keylen =  15
  
/* CR1207 */                        
            select #18, "AWDTKCRT",                                      ~
                        varc,     indexed,  recsize =  75,               ~
                        keypos =   1, keylen =    31,                    ~
                        alt key 1, keypos = 9,   keylen = 9, dup
                        
/* CR1356 */            
            select #19,  "AWDSKUXR",                                     ~
                        varc,     indexed,  recsize = 256,              ~
                        keypos =    1, keylen =  16,                    ~
                        alt key  1, keypos =  17, keylen =  20,         ~
                            key  2, keypos =  37, keylen =  45, dup
/*CR1918*/
            select #20, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup
                        
/*AWD059*/  select #42, "AWDBAYBW",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =   29
                                                        /* (PAR000)     */
            select #63, "BCKSUBPT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =  12, keylen =  11, dup,     ~
                            key  2, keypos =  23, keylen =  45, dup

                                                       /* (PAR000)     */
            call "SHOSTAT" ("Opening Files, One moment Please?")
                                                         /* (EWD0016)   */
            filename$ = "APCPLNDT" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDP" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNAD" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNLD" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
/*PAR002*/  filename$ = "APCPLNWT" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
/*AWD051*/  call "OPENCHCK" (#9, fs%(9%), f2%(9%),100%, rslt$(9%))
/*AWD043*/  filename$ = "AWDTRAIL" : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error
/*AWD028*/  call "OPENCHCK" (#12, fs%(12%), f2%(12%),500%, rslt$(12%))
        REM FILENAME$ = "AWDAPPLS"
        REM    call "EWDOPEN" (#12, filename$, err%)
        REM    if err% <> 0% then gosub open_error
/*AWD032*/  filename$ = "EWDPRDLB" : call "EWDOPEN" (#14, filename$, err%)
            if err% <> 0% then gosub open_error
            call "OPENCHCK" (#15, fs%(15%), f2%(15%),100%, rslt$(15%))
            call "OPENCHCK" (#16, fs%(16%), f2%(16%),500%, rslt$(16%))
/* CR890 */ filename$ = "AWDAPPLD" : call "EWDOPEN" (#17, filename$, err%)
            if err% <> 0% then gosub open_error
            call "OPENOLIB" (#6, "SHARE", f2%(6%), rslt$(6%), axd$)
            call "OPENCHCK" (#42, fs%(42%), f2%(42%),500%, rslt$(42%))
/*PAR000*/  filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
            if err% <> 0% then gosub open_error
/* CR1207 */ call "OPENCHCK" (#18, fs%(18%), f2%(18%),500%, rslt$(18%))
/* CR1356 */ call "OPENCHCK" (#19, fs%(19%), f2%(19%),0%, rslt$(19%))
/* CR1918 */ filename$ = "BCKMASTR" : call "EWDOPEN" (#20, filename$, err%)
             if err% <> 0% then gosub open_error
        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
                                                            /* (AWD047)   */
        dim apc$40, pname$21
            apc$   = "** (AWD) Appian Master Scan Utility ***"
            pname$ = "AWDSCANN - 09/25/2006"                /* (AWD054)   */
                                                            /* (AWD052)   */
                                                            /* (AWD051)   */
                                                            /* (PAR002)   */
                                                            /* (PAR004)   */
        dim rf_apc$20, rf_pname$8
            rf_apc$   = "Appian Mst Scan Util"
            rf_pname$ = "AWDSCANN"                          /* (PAR002)   */

                                                             /* (SR72504) */
        init(" ") schema$    : schema% = 0%
        call "SCHEMA" (schema$,             /* What switch 1-NC 2-NE    */~
                       schema%,             /* Schema                   */~
                       #2,                  /* GENCODES                 */~
                       err% )               /* error                    */
                                                          /* (\SR72504) */
                                                          


        wood_rmk% = 0%
                                                             /* (AWD028) */
        scr$(1%) = "****************************************"
        scr$(2%) = "*       SCANNING SELECTION CODES       *"
        scr$(3%) = "* ------------------------------------ *"
        scr$(4%) = "*         Appian Loading Only          *"
        scr$(5%) = "* (1) - Cart Scan            <105>     *" /* (CR456) */
        scr$(6%) = "* (2) - Staging Scann (Label)<106>     *"
        scr$(7%) = "* (3) - Appian Load Scanning <108>     *"
        scr$(8%) = "* (4) - Paint Receiving      <106>     *" /* CR2493 */
        scr$(9%) = "*                                      *"
        scr$(10%)= "*                                      *"
        scr$(11%)= "*                                      *"
        scr$(12%)= "****************************************"

        b_max% = 10%        /* SET NUMBER OF TIMES TO RING BELL ON SCREEN*/
        call "EXTRACT" addr("ID", userid$)
        date$ = date
        call "DATEFMT" (date$)
                                         /* (CR456) Screen Default Msg   */
        cr$(1%) = " CCCCC     AAAAA    RRRRRR    TTTTTTT          "
        cr$(2%) = "C     C   A     A   R     R      T             "
        cr$(3%) = "C         A     A   R     R      T             "
        cr$(4%) = "C         AAAAAAA   RRRRRR       T             "
        cr$(5%) = "C         A     A   R     R      T             "
        cr$(6%) = "C     C   A     A   R     R      T         [][]"
        cr$(7%) = " CCCCC    A     A   R     R      T         [][]"

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

        rc$(1%) = "  RRRR   EEEEEE  CCCCC  V         V EEEEEE     "
        rc$(2%) = "  R   R  E      C     C  V       V  E          "
        rc$(3%) = "  R    R E      C         V     V   E          "
        rc$(4%) = "  R   R  EEEE   C         V     V   EEEE       "
        rc$(5%) = "  RRRR   E      C          V   V    E          "
        rc$(6%) = "  R   R  E      C     C     V V     E          "
        rc$(7%) = "  R    R EEEEEE  CCCCC       V      EEEEEE     "
                                                            
        up$(1%) = "     U      U  PPPPPP    CCCCC                 "
        up$(2%) = "     U      U  P     P  C     C                "
        up$(3%) = "     U      U  P     P  C                      "
        up$(4%) = "     U      U  PPPPPP   C                      "
        up$(5%) = "     U      U  P        C                      "
        up$(6%) = "     U      U  P        C     C                "
        up$(7%) = "      UUUUUU   P         CCCCC                 "    
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


        rf_ee$(1%) = "  SSS TTT OOO PPP   "                  /* (AWD045) */
        rf_ee$(2%) = "  S    T  O O P P   "
        rf_ee$(3%) = "  SSS  T  O O PPP   "
        rf_ee$(4%) = "     S T  O O P     "
        rf_ee$(5%) = "     S T  O O P     "
        rf_ee$(6%) = "  SSSS T  OOO P     "
        

/* (CR1356) */
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

            brand$(13,1) = "V   V III EEEE W   W PPP  N  N TTT"
            brand$(13,2) = "V   V  I  E    W   W P  P N  N  T "
            brand$(13,3) = "V   V  I  EEE  W W W PPP  NN N  T "
            brand$(13,4) = " V V   I  E    W W W P    N NN  T "
            brand$(13,5) = "  V   III EEEE WW WW P    N  N  T "

REM         call "SHOSTAT" ("Norandex scanning") stop
            brand$(14,1) = "N   N  OOO  RRR   N   N DDD  X   X"
            brand$(14,2) = "NN  N O   O R  R  NN  N D  D  X X "
            brand$(14,3) = "N N N O   O RRR   N N N D  D   X  "
            brand$(14,4) = "N  NN O   O R  R  N  NN D  D  X X "
            brand$(14,5) = "N   N  OOO  R   R N   N DDD  X   X"

            brand$(16,1) = "EEEE X   X TTT RRR   EEEE M   M EEEE"
            brand$(16,2) = "E     X X   T  R  R  E    MM MM E   "
            brand$(16,3) = "EEE    X    T  RRR   EEE  M M M EEE "
            brand$(16,4) = "E     X X   T  R  R  E    M   M E   "
            brand$(16,5) = "EEEE X   X  T  R   R EEEE M   M EEEE"
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
        
        rf_msg$(1%)= "  Truck Active      "                   /* CR1207 */
        rf_msg$(2%)= "F1 to Close Truck   "
        rf_msg$(3%)= "Or F4 to Continue   "
        rf_msg$(4%)= "    current truck   "
                        
        inp_text$(01%)="Scan Barcode or Manually Enter Barcode Number"
        inp_text$(02%)="Scan Production Barcode or Manuall Enter No.?"
        err$(01%)="(Error) Barcode Not on File, or Invalid?          "
        err$(02%)="(Error) Barcode Not on File for Department?       "
        err$(03%)="(Error) Barcode on File for Different Department? "
        err$(04%)="(Error) Barcode Not Valid for Department/Process? "
        err$(05%)="(Error) Barcode Has Already Been Scanned Complete?"
        err$(06%)="(Error) Updating Production Department (XXX)?     "
        err$(07%)="(Error) While Updating Barcode Audit ---------->  "
        err$(08%)="(Error) Updating Staging, Dept (XXX) Not Complete?"
        err$(09%)="(Error) Updating Loading, Product Not Staged?     "
        err$(10%)="(Error) Glass Barcode not on file, or Invalid?    "
        err$(11%)="(Error) Glass Barcode Not Complete Cannot Re-Make?"
        err$(12%)="(Error) Updating Glass, Cannot Update Glass? ? ?  "
        err$(13%)="(Error) Product already Staged? ? ?               "
        err$(14%)="(Error) Product already Loaded? ? ? ?             "
        err$(15%)="(Error) Invalid Reason Code for Glass Re-Make?    "
        err$(16%)="(Error) Invalid Load No.? Not Valid for Load?     "
        err$(17%)="(Error) Shift Code Not Valid for <Time of Day>?   "
        err$(18%)="(Error) Glass Barcode Not Scheduled?              "
        err$(19%)="(Error) Tempered/Special Liting - Not Re-Make Scan"
        err$(20%)="(Error) Updating Glass? Glass Not Found?          "
        err$(21%)="(Error) Invalid Scanning User Id?                 "
        err$(22%)="(Error) Updating Glass Audit File?                "
        err$(23%)="(Error) Glass Barcode Already Complete?           "
        err$(24%)="(Error) Not Valid Appian Shipping Label Barcode?  "
        err$(25%)="(Error) Shipping Label does not match Production? "
        err$(26%)="(Error) Updating Production Department (XXX)?     "
        err$(27%)="(Error) Updating Production Department(XXX)Appian?"
        err$(28%)="(Error) Appian Label Not On File?                 "
        err$(29%)="(Error) Not Staged, No Appian Label printed?      "
        err$(30%)="(Error) Load Complete - All Staged Product Loaded "
        err$(31%)="(Error) Scanned Drop, out of Drop Sequence?       "
        err$(32%)="(Error) Cannot go Back. Drop has been Closed?     "
        err$(33%)="(Error) Warranty Update. Could Not Find Warr Rec  "
        err$(34%)="(Error) APCPLNDT Update. Could not Update Warranty"
        err$(35%)="(Error) Cross Docking. 2nd Label not Correct one  "
        err$(36%)="(Error) Cross Dock Part Number Validation (FAILED)"
        err$(37%)="(Error) Barcode Exists on cart XXXX? "     /* (CR456) */
        err$(38%)="(Error) Invalid Cart Number XXXX? "        /* (CR456) */
        err$(39%)="(Error) Invalid Cart Destination? "        /* (CR456) */
        err$(40%)="(Error) Need to goto Samples?      "       /* (CR456) */
        err$(41%)="(Error) Need to goto RGA?          "       /* (CR456) */
        err$(42%)="(Error) Need to goto Wood Surround?"       /* (CR456) */
        err$(43%)="(Error) Need to goto Casing/SDL  ? "       /* (CR456) */
        err$(44%)="(Error) Need to goto Bay/Bow?      "       /* (CR456) */
        err$(45%)="(Error) Not Complete Dept XXX?     "       /* (CR456) */
        err$(46%)="(Error) Product in Transit?        "       /* SR78815 */
        /* CR1356 */
        err$(47%)="(Error) Invalid SKU Label, Does not Match Barcode" 
        err$(48%)="(Message) Tech Pickup Window !!!!"         /* CR1791 */
        err$(49%)="(Error) Need to Ship from 300 plant"       /* CR2128 */
        err$(50%)="(Error) Barcode Must Be D for Paint"       /* CR2713 */
        err$(51%)="(Error) Paint Ship from 300 plant  "       /* CR2713 */
        err$(52%)="Need to go Quality "                       /* CR2825  */  
        err$(53%)="Need to go Shrink Wrap"                    /* CR2883 */
        
        rf_scan% = 0%
        rf_err$(01%)="Brcd Not on File   "
        rf_err$(02%)="Brcd Not Dept      "
        rf_err$(03%)="Brcd Diff Dept     "
        rf_err$(04%)="Brcd Not Valid Dept"
        rf_err$(05%)="Brcd Already Cmplte"
        rf_err$(06%)="Update Prd Dpt(XXX)"
        rf_err$(07%)="Update Brcd Audit  "
        rf_err$(08%)="Update Stg Dpt(XXX)"
        rf_err$(09%)="Update Load Not Stg"
        rf_err$(10%)="Glass Brcd not file"
        rf_err$(11%)="Glass Brcd Not Cmpl"
        rf_err$(12%)="Cannot Update Glass"
        rf_err$(13%)="Product already Stg"
        rf_err$(14%)="Prd already Loaded "
        rf_err$(15%)="Reason Cde Glass RM"
        rf_err$(16%)="Invalid Load Num   "
        rf_err$(17%)="Shift Code Invalid "
        rf_err$(18%)="Glass Brcd Not Schd"
        rf_err$(19%)="Tempered/Special Lt"
        rf_err$(20%)="Glass Not Found    "
        rf_err$(21%)="Invalid Scann User "
        rf_err$(22%)="Update Glass Audit "
        rf_err$(23%)="Glass Brcd Complet "
        rf_err$(24%)="Invalid App Label  "
        rf_err$(25%)="Ship Lbl not Prod  "
        rf_err$(26%)="Updt Prod Dept(XXX)"
        rf_err$(27%)="Updt Dept(XXX)App  "
        rf_err$(28%)="App Lbl Not On File"
        rf_err$(29%)="Not Stg No App Lbl "
        rf_err$(30%)="Load Complete      "
        rf_err$(31%)="Drop Scn, out Seq  "
        rf_err$(32%)="Cannot go Back     "
        rf_err$(33%)="Warranty Update.   "
        rf_err$(34%)="APCPLNDT Update.   "
        rf_err$(35%)="Cross Dock Error   "
        rf_err$(36%)="Part Val (FAILED)  "
        rf_err$(37%)="Brcde on cart XXXX "                    /* (CR456) */
        rf_err$(38%)="Invalid Cart XXXX  "                    /* (CR456) */
        rf_err$(39%)="Invalid Cart Dest  "                    /* (CR456) */
        rf_err$(40%)="Need to go Samples?"                    /* (CR456) */
        rf_err$(41%)="Need to go RGA?    "                    /* (CR456) */
        rf_err$(42%)="Need to go WoodDpt?"                    /* (CR456) */
        rf_err$(43%)="Need to go CaseSDL?"                    /* (CR456) */
        rf_err$(44%)="Need to go Bay/Bow?"                    /* (CR456) */
        rf_err$(45%)="Not Comp Dept XXX? "                    /* (CR456) */
        rf_err$(46%)="Product in Transit?"                    /* SR78815 */
        rf_err$(47%)="No shft, ptr or user"                   /* CR1191  */
        rf_err$(48%)="!!! Tech Pickup !!!"                    /* CR1791  */
        rf_err$(49%)="300 Ship window    "                    /* CR2128  */
        rf_err$(50%)="Must Be D Paint Brc"                    /* CR2713  */
        rf_err$(51%)="Paint Stage only   "                    /* CR2713  */  
        rf_err$(52%)="Need to go Quality "                    /* CR2825  */  
        rf_err$(53%)="Need to go Shr Wrap"                    /* CR2883  */

        her$(01%) = "     I n v a l i d   B a r c o d e   N o .     "
        her$(02%) = "   N o t   O n   F i l e   F o r   D e p t.    "
        her$(03%) = "      O n   F i l e   F o r   D e p t.         "
        her$(04%) = "N o t  V a l i d  F o r  D e p t  P r o c e s s"
        her$(05%) = " P r o d u c t   A l r e a d y   S c a n n e d "
        her$(06%) = "      E r r o r   U p d a t i n g              "
        her$(07%) = "E r r o r   U p d a t i n g   A u d i t        "
        her$(08%) = " P r o d u c t   N o t  S c a n n e d  By - XXX"
        her$(09%) = "     P r o d u c t   N o t   S t a g e d       "
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
        her$(24%) = "  I n v a l i d   A p p i a n    B a c o d e   "
        her$(25%) = "S h i p p i n g   D o e s   M a t c h   P r o d"
        her$(26%) = "U p d a t i n g   P r o d u c t i o n   D e p t"
        her$(27%) = "   U p d a t i n g   A p p i a n   L a b e l   "
        her$(28%) = "A p p i a n   L a b e l   N o t   O n   F i l e"
        her$(29%) = "     N O T   S T A G E D - N O   L A B E L     "
        her$(30%) = "A L L  S t a g e d   P r o d u c t  L o a d e d"
        her$(31%) = "    N o t   I n   D r o p   S e q u e n c e    "
        her$(32%) = "C A N N O T  G O  B A C K  D R O P  C L O S E D"
        her$(33%)=" W A R R A N T Y  U P D A T E  N O T   F O U N D "
        her$(34%)="APCPLNDT - W A R R A N T Y   N O T  U P D A T E D"
        her$(35%)="   C R O S S   D O C K - W R O N G   L A B E L   "
        her$(36%)="P A R T   V A L I D A T I O N - ( F A I L E D )  "

        rep_id$(01%) = "RB1"   : rep_code$( 1%) = "50"
        rep_id$(02%) = "RG1"   : rep_code$( 2%) = "51"
        rep_id$(03%) = "RB2"   : rep_code$( 3%) = "52"
        rep_id$(04%) = "RG2"   : rep_code$( 4%) = "53"
        rep_id$(05%) = "RB3"   : rep_code$( 5%) = "54"
        rep_id$(06%) = "RG3"   : rep_code$( 6%) = "55"
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
      
        cartsaveDest$ = "   "               /* CR2960 */   
        
      initialize
   
                                                  /* (PAR002) RF Testing */
        if str(userid$,1%,2%) = "TF" then str(userid$,1%,2%) = "RF"
        if str(userid$,1%,2%) = "ZF" then str(userid$,1%,2%) = "SF"

                                                  /* (PAR002) RF Testing */
        if str(userid$,1%,2%) = "CF" then goto rf_initialize /* (CR456) */
        if str(userid$,1%,2%) = "MF" then goto rf_initialize /* CR2960  */
        if str(userid$,1%,2%) = "RF" or str(userid$,1%,2%) = "SF"    ~
                           then goto rf_initialize
                                                             /* (AWD063) */
        if str(userid$,1%,3%) = "TR2" or str(userid$,1%,3%) = "TS2"  ~
                           then goto rf_initialize

        if str(userid$,1%,2%) = "PF" then goto rf_initialize /* CR2493 */
        
        if str(userid$,1%,2%) = "PP" then goto rf_initialize /* CR2713 */
        
        edit% = 0%
        init(" ") scr_shft$, scr_shft_d$, scr_proc$, scr_proc_d$,         ~
                  scr_dept$, scr_dept_d$, dt_st$, prevcode$, tt_unit$,    ~
                  scr_sel$, scr_sel_d$, scr_id$, rm_reason$, rm_bar$,     ~
                  reason_d$, scrn_title$, title$, fld$(3%), scr_id_d$,    ~
                  scr_load$, scr_load_d$, sav_scr_dept$, scr_prt$,        ~
                  dt_proc$, modulename$
                                                            /* (EWD026)  */
        scr_proc$ = "01"             /* Set Default to Manufacturing */
        tt_unit%  = 0%
        tt_unit2% = 0%
        apcpld40_call_flg% = 0%                             /*SR69112*/
        cartseq% = 0%                                       /* (CR780) */
REM------------------------------------------------------------------------
REM             M A I N                                                   -
REM------------------------------------------------------------------------
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
                                                    /* (AWD043)          */
        REM       GOSUB CHECK_LOAD
        REM       IF CODE% = 0% THEN GOTO INITIALIZE
                                                    /* (AWD043)          */
           edit% = 1%
           brand% = 99%                             /* CR1356 */
           if keyhit% = 14% then gosub prod_scan
      goto main



      rf_initialize                                 /* (AWD045) - beg */
        rf_scan% = 1%
        edit% = 0%
        cartseq% = 0%                             /* (CR780) */
        init(" ") scr_shft$, scr_shft_d$, scr_proc$, scr_proc_d$,      ~
                  scr_dept$, scr_dept_d$, dt_st$, prevcode$, tt_unit$, ~
                  scr_sel$, scr_sel_d$, scr_id$, rm_reason$, rm_bar$,  ~
                  reason_d$, scrn_title$, title$, fld$(3%),            ~
                  scr_id_d$, scr_load$, scr_load_d$, sav_scr_dept$,    ~
                  scr_prt$, dt_proc$, modulename$
                                                            /* (EWD026)  */
        scr_proc$ = "01"             /* Set Default to Manufacturing */
        tt_unit%  = 0%
        tt_unit2% = 0%
        apcpld40_call_flg% = 0%                            /*SR69112 */

        rf_inp_text$(1%)="Scan Shp Barco"
        rf_inp_text$(2%)="Scan Prd Barco"
        rf_inp_text$(3%)="Scan Trailer  "
        rf_inp_text$(4%)="Cart Trailer  "                  /*(CR456) */
        rf_inp_text$(5%)="Cart Tracking "                  /* CR1207 */
REM------------------------------------------------------------------------
REM             R F   M A I N                                             -
REM------------------------------------------------------------------------
        rf_main
                                                             /* (CR456) */
          if str(userid$,1%,2%) <> "CF" and str(userid$,1%,2%) <> "MF" ~
                then goto not_carting                        /* CR2960 */ 
          scr_sel$ = "1"
          scr_dept$ = "105"
not_carting:
                                                             /* (AWD063) */
                                                             /* CR2713 */
          if str(userid$,1%,2%) <> "RF" and userid$ <> "TR2"  and  ~
             str(userid$,1%,2%) <> "PP"   then goto not_staging
          scr_sel$ = "2"
          scr_dept$ = "106"

not_staging:
                                                             /* (AWD063) */
          if str(userid$,1%,2%) <> "SF" and userid$ <> "TS2"    ~
                                           then goto not_shipping
          scr_sel$ = "3"
          scr_dept$ = "108"
not_shipping
/* CR2493 */
          if str(userid$,1%,2%) <> "PF" then goto not_painting
          if schema% <> 1% then goto not_painting    
          
          scr_sel$ = "4"
          scr_dept$ = "106"
not_painting

          gosub rf_mainmenu
          init(" ") rf_errormsg$
          init(" ") errormsg$                              /* <AWD056> */
          if keyhit% = 2% then exit_program
          if keyhit% = 3% then gosub prod_scan
             gosub check_selection                  /* Validate Screen */
             if code% = 0% then goto initialize
             gosub check_dept
             if code% = 0% then goto initialize
             gosub check_shift
             if code% = 0% then goto initialize
             gosub check_process
             if code% = 0% then goto initialize
             gosub check_user
             if code% = 0% then goto initialize
             edit% = 1%
          if keyhit% = 1% then gosub prod_scan
          if keyhit% = 3% then gosub prod_scan
          if keyhit% = 4% then gosub cart_tracking      /* CR1207 */
        goto rf_main


REM------------------------------------------------------------------------
REM       B E G I N N I N G    O F    S C R E E N S                       -
REM------------------------------------------------------------------------

        mainmenu                                  /* Main Scanning Menu */
          gosub set_screen_1
                                                   /* Staging Printer    */
                                                   /* Remove Load Entry  */
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
               at (05,40), fac(lfac$(1%)), scr_prt$             , ch(01),~
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

               if keyhit% <> 1% then goto L03880         /* Print Que    */
                  run$ = "ILPMAN"
                  gosub Run_Program
                  goto mainmenu

L03880:        if keyhit% <> 4% then L03890          /* Report/Utility   */
                  gosub utility_scan
                  goto mainmenu

L03890:        if keyhit% <> 6% then L03940          /* DEPARTMENT CODES */
                  table% = 1%
                  gosub display_codes
                  goto mainmenu

L03940:        if keyhit% <> 9% then L04030          /* SHIFT CODES      */
                  table% = 4%
                  gosub display_codes
                  goto mainmenu

L04030:        if keyhit% <> 12% then L04010         /* (EWD022) Begin   */
                  pass% = 0%
                  call "APCPASSW" ("EWDPLN79", " ", pass%)
                  if pass% <> 0% then goto initialize

                  run$ = "EWDPLN79"
                  gosub Run_Program
                  goto mainmenu                     /* (EWD022) End      */

L04010:        if keyhit% <> 15% then L04020
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
                     "                       (14)Scanning    "
          pf$(2%) = "(4)Rpt/Utility       ( 9)Shift Codes    " &       ~
                    "(12)BackOrder Scan     (15)Print Screen"
          pf$(3%) = "                                        " &       ~
                    "                       (16)Exit Program"
          pfkeys$ = hex(01ffff04ff06ffff09ffff0cff0e0f102000)
        return                                   /* (EWD022) - Add 12 Key*/

        rf_mainmenu                                    /* (AWD045) - BEG */
          gosub rf_set_screen_1
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
               at (05,12), fac(lfac$(1%)), scr_prt$             , ch(01),~
                                                                         ~
               at (05,02), "Shft:",                                      ~
               at (05,08), fac(lfac$(1%)), scr_shft$            , ch(02),~
                                                                         ~
               at (06,02), "User:",                                      ~
               at (06,08), fac(lfac$(1%)), scr_id$              , ch(03),~
                                                                         ~
               at (08,02), fac(hex(8c)),   rf_pf$(1%)           , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               rf_errormsg$ = " "
               errormsg$    = " "                            /* <AWD056> */

/*  (AWD045) - Mod take out all function key except 1% and 2%  */

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return
        rf_set_screen_1
          lfac$(1%) = hex(81)
          init(" ") dateout$, scrn_title$ 
          call "TIME" (dateout$)
          rf_inpmessage$ = "Enter Dept,Shft,ID?"
          if scr_sel$ <> "1" then goto L04080        /* CR1207 */

          rf_pf$(1%) = "1)Scn2)Ext3)Prt4)Trk"        /* CR1207 */
             pfkeys$ = hex(01020304ffffffffffffffffffffffffff00)
          return
L04080:
          rf_pf$(1%) = "(1)Scn (2)Ext (3)Prt"
             pfkeys$ = hex(010203ffffffffffffffffffffffffffff00)
        return                                         /* (AWD045) - END */

        REM        Staging, Loading Display Screen
        deffn'100(fieldnr%)
L07950:   gosub set_screen_2
          accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,26), fac(hex(a4)), scr_load_d$            , ch(30),~
               at (02,21), fac(hex(a4)), scrn_title$            , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), fac(hex(84)), drop_txt1$             , ch(18),~
               at (04,29), fac(hex(84)), tt_unit$               , ch(33),~
               at (04,62), fac(hex(84)), drop_txt2$             , ch(18),~
                                                                         ~
               at (05,02), fac(hex(84)), fld$(1%)               , ch(23),~
               at (05,27), fac(lfac$(1%)), barcode_shp$         , ch(20),~
               at (05,50), fac(lfac$(2%)), wandchar_shp$        , ch(01),~
                                                                         ~
               at (06,02), fac(hex(84)), fld$(2%)               , ch(23),~
               at (06,27), fac(lfac$(3%)), barcode$             , ch(18),~
               at (06,50), fac(lfac$(4%)), wandchar$            , ch(01),~
                                                                         ~
               at (06,52), fac(hex(84)), fld$(3%)               , ch(04),~
               at (06,57), fac(hex(84)),   prevcode$            , ch(18),~
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

               if keyhit% <> 1% then L08290
                  gosub startover

                                  /*       GOTO SKIP_PWWW      TEMP PWWW */
                                                            /* SR69112 + */
L08290:        if keyhit% <> 4% then L08300      /* Calculate Drop Units */
                                 /*        GOTO SKIP_PWWW      TEMP PWWW */
                  gosub calc_dropped
                  goto L07950
                                                            /* SR69112 - */
                                                  /*           SKIP_PWWW */

L08300:        if keyhit% <> 5% then L08360          /* Calculate Units  */
                  gosub calc_scanned
                  goto L07950
                                                     /* (EWD007) - End   */
L08360:        if keyhit% <> 15% then L08380
                  call "PRNTSCRN"
                  goto L07950

L08380:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return
        set_screen_2
          if scr_sel% = 1% then copy cr$() to xx$()
          if scr_sel% = 2% then copy st$() to xx$()
          if scr_sel% = 3% then copy ld$() to xx$()
          if scr_sel% = 4% then copy rc$() to xx$()                 /* CR2493 */
          
          str(scrn_title$,29%,11%) = "Load= "&scr_load$

          drop_txt1$ = "[Current Drop: XX]"   /* Current Load Drop (18)*/
          drop_txt2$ = "[Scanned Drop: YY]"   /* Scanned load Drop (18)*/

          str(drop_txt1$,16%,2%) = sav_drop$
          str(drop_txt2$,16%,2%) = scan_drop$

          tt_unit$ = "Scanned Units [ XXXXXX ]"
          convert tt_unit% to str(tt_unit$,17%,6%), pic(######)
                                                             /*SR69112 + */
          if apcpld40_call_flg% <> 1%  then goto skip_drop
             tt_unit$ = "Loaded [ XXX ] Not Loaded [ XXX ]"
             convert tt_unit% to str(tt_unit$,10%,3%), pic(###)
             convert tt_unit2% to str(tt_unit$,29%,3%), pic(###)

        skip_drop
                                                             /*SR69112 - */
          init(" ") dateout$
          inpmessage$ = inp_text$(fieldnr%)
          call "TIME" (dateout$)
                                                       /* (AWD028)       */
          fld$(1%)      = "Ship Barcode To Scan :"
          fld$(2%)      = "Prod Barcode To Scan :"
          fld$(3%)      = "Prv:"
                                                       /* (AWD028)       */

          pf$(1%) = "(1)Startover                            " &       ~
                     "                                       " /*SR69112*/
          pf$(2%) = "(4)Drop Units                           " &       ~
                      "                       (15)Print Screen"
          pf$(3%) = "(5)Calc Units                           " &       ~
                     "                       (16)Exit Screen "
          pfkeys$ = hex(01ffff0405ffffffffffffffffff0f1000)
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

        deffn'101(fieldnr%)
          gosub rf_set_screen_2
          accept                                                       ~
               at (01,02), fac(hex(8c)), rf_pname$              , ch(08),~
               at (01,12), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), rf_errormsg$           , ch(20),~
                                                                         ~
               at (03,02), fac(hex(84)), tt_unit$               , ch(24),~
                                                                         ~
               at (04,02), fac(hex(84)), fld$(1%)               , ch(20),~
               at (05,02), fac(lfac$(1%)), barcode_shp$         , ch(20),~
               at (05,24), fac(lfac$(2%)), wandchar_shp$        , ch(01),~
                                                                         ~
               at (06,02), fac(lfac$(3%)), barcode$             , ch(18),~
               at (06,22), fac(lfac$(4%)), wandchar$            , ch(01),~
                                                                         ~
                                                                         ~
               at (07,02), fac(hex(a4)),   rf_inpmessage$       , ch(20),~
               at (08,02), fac(hex(8c)),   rf_pf$(1%)           , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 1% then L08310
                  gosub startover

L08310:
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return
        rf_set_screen_2
         if scr_sel% = 1% then copy cr$() to xx$()
         if scr_sel% = 2% then copy st$() to xx$()
         if scr_sel% = 3% then copy ld$() to xx$()
         if scr_sel% = 4% then copy rc$() to xx$()                 /* CR2493 */
         
         tt_unit$ = "Scanned[ XXXXXX ]"
         convert tt_unit% to str(tt_unit$,10%,6%), pic(######)

         init(" ") dateout$
         rf_inpmessage$ = rf_inp_text$(fieldnr%)
         call "TIME" (dateout$)

         fld$(1%)      = "Ship & Prod Barcode:"

         rf_pf$(1%) = "(1)StartOver(2)Exit"
         pfkeys$ = hex(0102ffffffffffffffffffffffffffff00)
         if fieldnr% <> 1% then goto L08410
            init(" ") barcode$, wandchar$
            lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
            lfac$(3%) = hex(84) : lfac$(4%) = hex(84)
        return
L08410:     lfac$(1%) = hex(84) : lfac$(2%) = hex(84)
            lfac$(3%) = hex(81) : lfac$(4%) = hex(99)
        return

        deffn'150(fieldnr%)
L08450:   gosub set_screen_2a
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
/* CR1356 */                                                             ~
               at (07,02), fac(hex(84)), brand$(brand%,1%)      , ch(40),~
               at (07,43), fac(hex(84)), scr_line1$             , ch(38),~
               at (08,02), fac(hex(84)), brand$(brand%,2%)      , ch(40),~
               at (08,43), fac(hex(84)), scr_line2$             , ch(38),~
               at (09,02), fac(hex(84)), brand$(brand%,3%)      , ch(40),~
               at (09,43), fac(hex(84)), scr_line3$             , ch(38),~
               at (10,02), fac(hex(84)), brand$(brand%,4%)      , ch(40),~
               at (10,43), fac(hex(84)), scr_line4$             , ch(38),~
               at (11,02), fac(hex(84)), brand$(brand%,5%)      , ch(40),~
               at (11,43), fac(hex(84)), scr_line5$             , ch(38),~
               at (12,43), fac(hex(84)), scr_line6$             , ch(38),~
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

               if keyhit% <> 1% then L08490
                  gosub startover
/*             GOTO SKIP_PWWW2     TEMP PWWW */
/*SR69112 + */
L08490:        if keyhit% <> 4% then L08500      /* Calculate Drop Units */
/*             GOTO SKIP_PWWW2     TEMP PWWW */
                  gosub calc_dropped
                  goto L08450
/*SR69112 - */
/*           SKIP_PWWW2   */
L08500:        if keyhit% <> 5% then L08570          /* Calculate Units  */
                  gosub calc_scanned
                  goto L08450

L08570:        if keyhit% <> 8% then L08590          /* Calculate Units  */
                  gosub wood_remake
                  tt_unit% = 0%

L08590:        if keyhit% <> 10% then L08560
                  barcode$ = cross_key$
                  crossdock_bypass% = 1%
                  goto L08580

L08560:        if keyhit% <> 14% then L08565                  /* (CR456) */
                  cartPrint% = 1%                /* Print Labels On cart */
                  gosub printCartLabel
                  goto L08450

L08565:        if keyhit% <> 15% then L08580
                  call "PRNTSCRN"
                  goto L08450

L08580:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return
        set_screen_2a
          if scr_sel% = 1% then copy cr$() to xx$()
          if scr_sel% = 2% then copy st$() to xx$()
          if scr_sel% = 3% then copy ld$() to xx$()
          if scr_sel% = 4% then copy rc$() to xx$()                 /* CR2493 */
          
/* (CR456) */
          if scr_sel% <> 1% then goto noCartSel
             str(scrn_title$,30%,11%) = cartNum$
noCartSel:

          tt_unit$ = "Scanned Units [ XXXXXX ]"
          convert tt_unit% to str(tt_unit$,17%,6%), pic(######)
          init(" ") dateout$
          inpmessage$ = inp_text$(fieldnr%)
          call "TIME" (dateout$)
          lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
          fld$(1%)      = "Barcode Number To Scan  :"
          fld$(2%)      = "Prv:"
                                                      /* (PAR002)        */
          if cross_dock% = 1% then                                     ~
             inpmessage$="Scan Second Barcode or Enter Barcode Number Manually"
          if cross_dock% = 1% then                                     ~
              fld$(1%)   = "2nd Barcode No. to Scan :"
                                                      /* (PAR002)        */
          pf$(1%) = "(1)Startover                            " &       ~
                    "                       (14)Stage Cart  "
          pf$(2%) = "                       (8)Production Sca" &       ~
                    "n                      (15)Print Screen"
          pf$(3%) = "(5)Calc Units          (10)ByPass Cross " &       ~
                    "Dock                   (16)Exit Screen "
          pfkeys$ = hex(01ffffff05ffff08ff0affffff0e0f1000)
/*(CR456)*/
          if scr_dept$ = "105" then gosub changeExit
          if scr_dept$ <> "106" then gosub removeCartSelection
          if cross_dock% = 0% then str(pf$(3%),22%,25%) = " "
          if cross_dock% = 0% then str(pfkeys$,10%,1%) = hex(ff)
        return
        removeCartSelection
          str(pf$(1%),64%,16%) = " "
          str(pfkeys$,14%,1%) = hex(ff)
        return
        changeExit
          str(pf$(3%),64%,15%) = "(16)Next Cart  "
        return

        deffn'151(fieldnr%)
L08520:   gosub rf_set_screen_2a
          accept                                                       ~
               at (01,02), fac(hex(8c)), rf_pname$              , ch(08),~
               at (01,12), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), rf_errormsg$           , ch(20),~
                                                                         ~
               at (03,02), fac(hex(84)), tt_unit$               , ch(20),~
                                                                         ~
               at (04,02), fac(hex(84)), rf_fld$(1%)            , ch(20),~
               at (05,02), fac(lfac$(1%)), barcode$             , ch(18),~
               at (05,21), fac(lfac$(2%)), wandchar$            , ch(01),~
                                                                         ~
               at (07,02), fac(hex(a4)),   rf_inpmessage$       , ch(20),~
               at (08,02), fac(hex(8c)),   rf_pf$(1%)           , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 1% then L08510
                  gosub startover
L08510:                                                      /* (CR781) */
               if keyhit% <> 4% then L08515
                  cartPrint% = 1%                /* Print Labels On cart */
                  gosub printCartLabel
                  goto L08520
L08515:
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return
        rf_set_screen_2a
/*  (AWD045) - Numerious Mods shorten messages  */
          if scr_sel% = 1% then copy cr$() to xx$()
          if scr_sel% = 2% then copy st$() to xx$()
          if scr_sel% = 3% then copy ld$() to xx$()
          if scr_sel% = 4% then copy rc$() to xx$()                 /* CR2493 */

          tt_unit$ = "Scanned[ XXXXXX ]"
          convert tt_unit% to str(tt_unit$,10%,6%), pic(######)

          init(" ") dateout$
          rf_inpmessage$ = rf_inp_text$(2%)
          call "TIME" (dateout$)
          lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
          rf_fld$(1%)      = "Barcode To Scan  :"
                                                      /* (PAR002)        */
          if cross_dock% = 1% then                                     ~
             rf_inpmessage$= "2nd Barcode   "
          if cross_dock% = 1% then                                     ~
             rf_fld$(1%)   = "2nd Barcode Scan :"
                                                     /* (PAR002)        */
                                                     /* (CR456) */
/*          if userid$ = "CF1" then goto rf_set_screen_2a_cart  CR1191 */
          if keyhit% = 3% then rf_fld$(1%) = "Barcode to RE-Print:"
            rf_fld$(2%)      = "Prv:"
            rf_pf$(1%) = "1)StOv2)Ex3)ReP4)Crt"
            pfkeys$ = hex(01020304ffffffffffffffffffffffff00)
        return
                                                             /* <AWD058> */
        deffn'152(fieldnr%)
LX8450:
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
/* CR1356 */                                                             ~
               at (07,02), fac(hex(84)), brand$(brand%,1%)      , ch(40),~
               at (07,43), fac(hex(84)), scr_line1$             , ch(38),~
               at (08,02), fac(hex(84)), brand$(brand%,2%)      , ch(40),~
               at (08,43), fac(hex(84)), scr_line2$             , ch(38),~
               at (09,02), fac(hex(84)), brand$(brand%,3%)      , ch(40),~
               at (09,43), fac(hex(84)), scr_line3$             , ch(38),~
               at (10,02), fac(hex(84)), brand$(brand%,4%)      , ch(40),~
               at (10,43), fac(hex(84)), scr_line4$             , ch(38),~
               at (11,02), fac(hex(84)), brand$(brand%,5%)      , ch(40),~
               at (11,43), fac(hex(84)), scr_line5$             , ch(38),~
               at (12,43), fac(hex(84)), scr_line6$             , ch(38),~
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

               if keyhit% <> 1% then LX8500
                  gosub startover

LX8500:        if keyhit% <> 5% then LX8560          /* Calculate Units  */
                  gosub calc_scanned
                  goto LX8450

LX8560:        if keyhit% <> 15% then LX8580
                  call "PRNTSCRN"
                  goto LX8450

LX8580:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return
                                                      /* </AWD058>      */
        REM New Cart Scan Screen
        deffn'155(fieldnr%)
          fld%, fieldnr% = 0%
          init(" ") cartNum$, cartDest$, wandchar$
LC8600:   gosub set_Cart_screen
          if fieldnr% > 0% then init(hex(8c)) lfac$()                 ~
                           else init(hex(86)) lfac$()
          if fieldnr% <> 1% then nextCartField
             lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
             lfac$(3%) = hex(84) : lfac$(4%) = hex(84)
             goto cartScreen
nextCartField:
             lfac$(1%) = hex(84) : lfac$(2%) = hex(84)
             lfac$(3%) = hex(81) : lfac$(4%) = hex(99)

cartScreen:
          accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), scrn_title$            , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(84)), fld$(1%)               , ch(24),~
               at (05,30), fac(lfac$(1%)), cartNum$             , ch(08),~
               at (05,40), fac(lfac$(2%)), wandchar$            , ch(01),~
                                                                         ~
               at (06,02), fac(hex(84)), fld$(2%)               , ch(24),~
               at (06,30), fac(lfac$(3%)), cartDest$            , ch(10),~
               at (06,40), fac(lfac$(4%)), wandchar$            , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if fieldnr% = 1% and keyhit% <> 16% then goto LC8600

               if keyhit% <> 1% then LC8610
                  gosub startover

LC8610:        if keyhit% <> 15% then LC8620
                  call "PRNTSCRN"
                  goto LC8600

LC8620:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return
        set_Cart_screen
          cartDest$ = "195"   /* Default */
          if userid$ = "MF1" then cartDest$ = "MCK"
          if cartsaveDest$ = "MCK" then cartDest$ = cartsaveDest$  /* CR2960 */
          fieldnr% = fieldnr% + 1%
          inpmessage$ = inp_text$(fieldnr%)
          fld$(1%) = "Cart Barcode to Scan:"
          fld$(2%) = "Cart Destination    :"
          pf$(1%) = "(1)Startover                            " &       ~
                     "                                       "
          pf$(2%) = "                                        " &       ~
                    "                       (15)Print Screen"
          pf$(3%) = "                                        " &       ~
                    "                       (16)Exit Screen "
          pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
        return

        REM New Cart RF Scan Screen
        deffn'156(fieldnr%)
          fld%, fieldnr% = 0%
          init(" ") cartNum$, cartDest$, wandchar$
LC8700:   gosub rf_Cart_set_screen
          if fieldnr% > 0% then init(hex(8c)) lfac$()                 ~
                           else init(hex(86)) lfac$()
          if fieldnr% <> 1% then nextRFCartField
             lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
             lfac$(3%) = hex(84) : lfac$(4%) = hex(84)
             goto cartRFScreen
nextRFCartField:
             lfac$(1%) = hex(84) : lfac$(2%) = hex(84)
             lfac$(3%) = hex(81) : lfac$(4%) = hex(99)

cartRFScreen:
          accept                                                       ~
               at (01,02), fac(hex(8c)), rf_pname$              , ch(08),~
               at (01,12), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), rf_errormsg$           , ch(20),~
                                                                         ~
               at (03,02), fac(hex(84)), rf_fld$(1%)            , ch(20),~
               at (04,02), fac(lfac$(1%)), cartNum$             , ch(08),~
               at (04,12), fac(lfac$(2%)), wandchar$            , ch(01),~
                                                                         ~
               at (05,02), fac(hex(84)), rf_fld$(2%)            , ch(20),~
               at (06,02), fac(lfac$(3%)), cartDest$            , ch(10),~
               at (06,12), fac(lfac$(4%)), wandchar$            , ch(01),~
                                                                         ~
                                                                         ~
               at (07,02), fac(hex(a4)),   rf_inpmessage$       , ch(20),~
               at (08,02), fac(hex(8c)),   rf_pf$(1%)           , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               fld% = fld% + 1%
               if fld% = 1% and keyhit% <> 2% then goto LC8700

               if keyhit% <> 1% then LC8720
                  gosub startover

LC8720:
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return
        rf_Cart_set_screen
          cartDest$ = "195"   /* Default */
          if userid$ = "MF1" then cartDest$ = "MCK"  /* CR2960 */
          if cartsaveDest$ = "MCK" then cartDest$ = cartsaveDest$  /* CR2960 */            
          fieldnr% = fieldnr% + 1%
          rf_inpmessage$ = rf_inp_text$(4%)
          rf_fld$(1%) = "Cart to Scan:     "
          rf_fld$(2%) = "Cart Dest   :     "
          rf_pf$(1%) = "(1)StOv(2)Ex       "
          pfkeys$ = hex(0102ffffffffffffffffffffffffffff00)
        return


        REM New Trailer Scan Screen
        deffn'175(fieldnr%)
L08600:   gosub set_screen_2b
          accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), scrn_title$            , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(84)), fld$(1%)               , ch(24),~
               at (05,30), fac(lfac$(1%)), barcode_trailer$     , ch(08),~
               at (05,40), fac(lfac$(2%)), wandchar_trailer$    , ch(01),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 1% then L08610
                  gosub startover

L08610:        if keyhit% <> 15% then L08620
                  call "PRNTSCRN"
                  goto L08600

L08620:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return
        set_screen_2b
          init(" ") dateout$
          inpmessage$ = inp_text$(fieldnr%)
          call "TIME" (dateout$)

          fld$(1%) = "Trailer Barcode to Scan:"             /* (CR456) */
          if cartPrint% = 1% then fld$(1%) = "Cart Barcodes to Stage :"
          pf$(1%) = "(1)Startover                            " &       ~
                    "                                       "
          pf$(2%) = "                                        " &       ~
                    "                       (15)Print Screen"
          pf$(3%) = "                                        " &       ~
                    "                       (16)Exit Screen "
          pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)

          init(" ") barcode_trailer$, wandchar_trailer$
          lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
        return

        REM New Trailer Scan Screen
        deffn'176(fieldnr%)
          if userid$ = "CF1" then goto endCartPrnt          /* CR2815 */
          if userid$ = "MF1" then goto endCartPrnt          /* CR2960 */
          gosub rf_set_screen_2b
          accept                                                       ~
               at (01,02), fac(hex(8c)), rf_pname$              , ch(08),~
               at (01,12), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), rf_errormsg$           , ch(20),~
                                                                         ~
               at (03,02), fac(hex(84)), rf_fld$(1%)            , ch(20),~
               at (04,02), fac(lfac$(1%)), barcode_trailer$     , ch(08),~
               at (04,12), fac(lfac$(2%)), wandchar_trailer$    , ch(01),~
                                                                         ~
                                                                         ~
               at (07,02), fac(hex(a4)),   rf_inpmessage$       , ch(20),~
               at (08,02), fac(hex(8c)),   rf_pf$(1%)           , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)



               if keyhit% <> 1% then L08525
                  gosub startover

L08525:
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return
        rf_set_screen_2b
          init(" ") dateout$
          rf_inpmessage$ = rf_inp_text$(3%)            /* (CR781) */
          if cartPrint% = 1% then rf_inpmessage$ = rf_inp_text$(4%)
          call "TIME" (dateout$)
          lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
          rf_fld$(1%)      = "Trailer To Scan  :"           /* (CR456) */
                                                              /* (CR781) */
          if cartPrint% = 1% then rf_fld$(1%) = "Cart to Stage    :"
          if keyhit% = 3% then rf_fld$(1%) = "Barcode to RE-Print:"

          rf_fld$(2%)      = "Prv:"
          rf_pf$(1%) = "(1)StOv(2)Ex(3)Re-Pr"
          pfkeys$ = hex(010203ffffffffffffffffffffffffff00)


          init(" ") barcode_trailer$, wandchar_trailer$
          lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
        return

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
               at (08,02), fac(hex(8c)),   rf_pf$(1%)           , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_error
          if cart% = 1% then goto cart_msg
          rf_inpmessage$ = "PF<04> to Continue"
          rf_pf$(1%)     = "(04) Continue     "
          pfkeys$        = hex(ffffff04ffffffffffffffffffffffffff)
        return
        cart_msg
          rf_pf$(1%)     = "(02)Del(04)Cont   "
          rf_inpmessage$ = "PF<04>Cont,Del Any"
          pfkeys$ = hex(0102030405060708090a0b0c0d0e0fffff)
        return


/*  (AWD045) - Numerious Mods shorten messages  */

REM------------------------------------------------------------------------
REM       E N D    O F    S C R E E N S                                   -
REM------------------------------------------------------------------------

REM------------------------------------------------------------------------
REM       B E G I N N I N G    O F    R O U T I N E S                     -
REM------------------------------------------------------------------------

        check_selection
          init(" ") upd_st$
          code% = 0%
                
/* CR2493 */
          if str(userid$,1%,2%) <> "PF"  then goto L00500
          if schema% <> 1 then goto L11060
          if scr_sel$ <> "4" then goto L11065
L00500:
/* (CR456) removed old code & changed scr_sel_d$ from scr$(3+ to scr$(4+ */
          convert scr_sel$ to scr_sel%, data goto L11060
/* CR1698 */
          if scr_sel$ = "1" then scr_dept$ = "105"
          if scr_sel$ = "2" then scr_dept$ = "106"
          if scr_sel$ = "3" then scr_dept$ = "108"
          if scr_sel$ = "4" then scr_dept$ = "106"  /* CR2493 */
          
          scr_sel_d$ = str(scr$(4% + scr_sel%),9%,30%)
          str(scr_sel_d$,23%,3%) = scr_dept$
          scrn_title$ = scr_sel_d$
/* CR1567 CR2493 */
          if scr_sel% <> 2% and scr_sel% <> 4% then goto noPrtSel
          p% = 0%
          p% = pos("CFHIJTV" = scr_prt$) 
          if p% = 0% and schema% = 1% and scr_sel$ = "2" then scr_prt$ = "H"
/* CR2493 */
          if p% = 0% and schema% = 1% and scr_sel$ = "4" then scr_prt$ = "H"
            if scr_prt$ = "A" then str(scrn_title$,30%,11%) = "Printer (A)"
            if scr_prt$ = "B" then str(scrn_title$,30%,11%) = "Printer (B)"
            if scr_prt$ = "T" then str(scrn_title$,30%,11%) = "TestPrinter"
            if scr_prt$ = "C" then str(scrn_title$,30%,11%) = "Printer (C)"
            if scr_prt$ = "D" then str(scrn_title$,30%,11%) = "Printer (D)"
            if scr_prt$ = "E" then str(scrn_title$,30%,11%) = "Printer (E)"
            if scr_prt$ = "Z" then str(scrn_title$,30%,11%) = "NE PRT- (Z)"
/*(CR456)*/ if scr_prt$ = "H" then str(scrn_title$,30%,11%) = "DC PRT- (H)"
/*(CR456)*/ if scr_prt$ = "I" then str(scrn_title$,30%,11%) = "DC PRT- (I)"
/*(CR456)*/ if scr_prt$ = "J" then str(scrn_title$,30%,11%) = "DC PRT- (J)"
/* CR1014*/ if scr_prt$ = "K" then str(scrn_title$,30%,11%) = "NE PRT- (K)"
/* CR1014*/ if scr_prt$ = "L" then str(scrn_title$,30%,11%) = "NE PRT- (L)"
/* CR1014*/ if scr_prt$ = "M" then str(scrn_title$,30%,11%) = "NE PRT- (M)"
/* CR1014*/ if scr_prt$ = "N" then str(scrn_title$,30%,11%) = "NE PRT- (N)"
/* CR1014*/ if scr_prt$ = "P" then str(scrn_title$,30%,11%) = "NE PRT- (P)"
/* CR1810*/ if scr_prt$ = "Q" then str(scrn_title$,30%,11%) = "NE PRT- (Q)"
/* CR1810*/ if scr_prt$ = "R" then str(scrn_title$,30%,11%) = "NE PRT- (R)"
/* CR1810*/ if scr_prt$ = "S" then str(scrn_title$,30%,11%) = "NE PRT- (S)"
/* CR1810*/ if scr_prt$ = "U" then str(scrn_title$,30%,11%) = "NE PRT- (U)"
/* CR2130*/ if scr_prt$ = "V" then str(scrn_title$,30%,11%) = "300 SHIP(V)"
/* CR2493*/ if scr_prt$ = "H" and scr_sel$ = "4"   ~
                              then str(scrn_title$,30%,11%) = "DC PAINT(H)"

noPrtSel:

            code% = 1%                           /* (EWD004) - Begin */
REM ON SCR_SEL% GOTO PROD, STAGED, LOADED, GLASS, GLS_RMK, RGA, LOG_MAINT,~
       HOLD_AREA
/* CR 2493 */
          on scr_sel% goto cart, staged, loaded, painted

          goto L11060

        cart
/* CR1698 */
          if schema% = 2% then gosub L11055
                
          upd_st$ = "26" : scr_dept$ = "105"       /* Cart Scan          */
          upd_st% = 26%
        return
        
        staged
/* CR1698 */
          if schema% = 2% and scr_dept$ = "105" then gosub L11057
                
          upd_st$ = "14" : scr_dept$ = "106"       /* Staged Complete    */
          upd_st% = 14%
        return
        
        loaded
          upd_st$ = "16" : scr_dept$ = "108"      /* Loaded              */
          upd_st% = 16%
        return
        
/* CR2493 */
        painted   
           if schema% = 2% then gosub L11059
           upd_st$ = "14" : scr_dept$ = "106"    /* Paint Received      */
           upd_st%   = 14%
        return
        
L11055:
          if rf_scan% = 1% then ~
             rf_errormsg$ = "Invalid Sel"   ~
          else  ~
             errormsg$ = "(Error) - Invalid Scanning Selection?" 
          code% = 0%
        return

L11057:
          if rf_scan% = 1% then ~
             rf_errormsg$ = "Invalid Dept"   ~
          else  ~
             errormsg$ = "(Error) - Invalid Scanning Department?" 
          code% = 0%
        return          
  
L11059:
          if rf_scan% = 1% then ~
             rf_errormsg$ = "Invalid Sel"   ~
          else  ~
             errormsg$ = "(Error) - Invalid Scanning Selection?" 
          code% = 0%
        return
        
L11060:   if rf_scan% = 1% then goto L11071               /* (AWD045)   */
          errormsg$ = "(Error) - Invalid Scanning Selection?"
          goto L11070                              /* (EWD001)          */
                                                  /* (AWD028)          */
          if rf_scan% = 1% then goto L11075               /* (AWD045)   */
          errormsg$ = "(Error) - Only Valid for Appian Shipping Label"
          goto L11070
                                                    /* (AWD028)          */
          if rf_scan% = 1% then goto L11080               /* (AWD045)   */
          errormsg$ = "(Error) - Only Valid for Glass Scanning?"
          goto L11070
          
          if rf_scan% = 1% then goto L11085               /* (AWD045)   */
          errormsg$ = "(Error) - Only Valid for Production Scanning"
/* CR2493 */
L11065:   if rf_scan% = 1% then L11087
          errormsg$ = "(Error) - Cannot change SEL with PF ID"

L11070:   gosub error_prompt                       /* (EWD001)          */
          code% = 0%
        return
                                                           /* (AWD045)   */
L11071:   rf_errormsg$ = "Invalid Selection"
          goto L11090
L11075:   rf_errormsg$ = "Only Valid for Shp Lbl"
          goto L11090
L11080:   rf_errormsg$ = "Only Valid for Gls Scn"
          goto L11090
L11085:   rf_errormsg$ = "Only Valid for Prd Scn"
          goto L11090
L11087:   rf_errormsg$ = "Cannot Change SEL     "

L11090:   gosub'500(fieldnr%)
        return
                                                     /* (EWD005) - End   */
        check_shift
          table$ = "PLAN SHFT"
          code$  = scr_shft$
          gosub check_code
          if code% = 0% then goto L11330
             scr_shft_d$ = desc$
        return
L11330:   if rf_scan% = 1% then goto L11335
          errormsg$ = "(Error) - Invalid Shift Selection??"
          gosub error_prompt
        return
L11335:   rf_errormsg$ = "Invalid Shift"
          gosub'500(fieldnr%)
        return

        check_process
          table$ = "PLAN PROC"
          code$  = scr_proc$
          gosub check_code
          if code% = 0% then goto L11440
             scr_proc_d$ = desc$
        return
L11440:   if rf_scan% = 1% then goto L11445
          errormsg$ = "(Error) - Invalid Process Selection??"
          gosub error_prompt
        return
L11445:   rf_errormsg$ = "Invalid Process"
          gosub'500(fieldnr%)
        return

        check_dept
          init(" ") desc$
          if scr_dept$ > "095" then goto L11610
          if scr_dept$ < "100" then goto L11580              /* (CR456) */
        return
L11580:   if rf_scan% = 1% then goto L11585
          errormsg$ = "(Error) - Invalid Department Selection??"
          gosub error_prompt
        return
L11585:   rf_errormsg$ = "Invalid Department"
          gosub'500(fieldnr%)
        return

L11610:
          p% = 0%
          p% = pos("ABT" = scr_prt$)
/* CR1567 set default for NC cart scanning */
          if schema% = 1% and scr_sel$ = "1" and p% = 0%  then scr_prt$ = "A"
/* CR2493 */          
          if scr_dept$ <> "106" then goto noPrtScrnMsg
/* (CR456) + */
          p% = 0%
          p% = pos("ABCDEFGTZHIJKLMNPQRSUV" = scr_prt$)
          if p% = 0% then scr_prt$ = "A"
          
/* CR1567 +  set staging to default to H */
          if p% = 0% and schema% = 1% then scr_prt$ = "H" 
          p% = 0%
          p% = pos("CFHIJTV"= scr_prt$)
          if schema% = 1% and scr_sel$ = "2" and p% = 0% then scr_prt$ = "H"
          if schema% = 1% and scr_sel$ = "4" and p% = 0% then scr_prt$ = "H"
/* CR1567 - */

/* (CR456) - */

          if scr_prt$ = "A" then scr_dept_d$ = "Carting Department ~
                                                     ~(Printer=A)"
          if scr_prt$ = "B" then scr_dept_d$ = "Carting Department ~
                                                   ~(Printer=B)"
          if scr_prt$ = "C" then scr_dept_d$ = "Staging Department ~
                                                  ~(Printer=C)"
                                                        /*AWD060*/
          if scr_prt$ = "D" then scr_dept_d$ = "Staging Department ~
                                                  ~(Printer=D)"
          if scr_prt$ = "E" then scr_dept_d$ = "Staging Department ~
                                                 ~(Printer=E)"
                                                       /*AWD064*/
          if scr_prt$ = "F" then scr_dept_d$ = "UPS Department (Pri~
                                                  ~nter=F)    "
                                                      /*SR70180*/
          if scr_prt$ = "G" then scr_dept_d$ = "Cross Dock Dept(Pri~
                                                   ~nter=G)    "
          if scr_prt$ = "T" then scr_dept_d$ = "Staging Department ~
                                                   ~(TESTPRINT)"
          if scr_prt$ = "Z" then scr_dept_d$ = "Staging North East ~
                                                   ~(Printer=Z)"
/*CR456+*/
          if scr_prt$ = "H" then scr_dept_d$ = "DC Center (Printer=H)"
          if scr_prt$ = "I" then scr_dept_d$ = "DC Center (Printer=I)"
          if scr_prt$ = "J" then scr_dept_d$ = "DC Center (Printer=J)"
/*CR456-*/  
/*CR1014+*/
          if scr_prt$ = "K" then scr_dept_d$ = "Staging Department ~
                                                   ~(Printer=K)"
          if scr_prt$ = "L" then scr_dept_d$ = "Staging Department ~
                                                   ~(Printer=L)"
          if scr_prt$ = "M" then scr_dept_d$ = "Staging Department ~
                                                   ~(Printer=M)"
          if scr_prt$ = "N" then scr_dept_d$ = "Staging Department ~
                                                   ~(Printer=N)"
          if scr_prt$ = "P" then scr_dept_d$ = "Staging Department ~
                                                   ~(Printer=P)"
/*CR1014-*/
/*CR1810*/
          if scr_prt$ = "Q" then scr_dept_d$ = "Staging Department ~
                                                   ~(Printer=Q)"
          if scr_prt$ = "R" then scr_dept_d$ = "Staging Department ~
                                                   ~(Printer=R)"
          if scr_prt$ = "S" then scr_dept_d$ = "Staging Department ~
                                                   ~(Printer=S)"
          if scr_prt$ = "U" then scr_dept_d$ = "Staging Department ~
                                                   ~(Printer=U)"
/*CR2130 */
          if scr_prt$ = "V" then scr_dept_d$ = "Staging 300 Building"
                                             
noPrtScrnMsg:

          if scr_dept$ = "105" then scr_dept_d$ = "Cart Transport    "
          if scr_dept$ = "108" then scr_dept_d$ = "Loading Department"
          if len(scr_dept_d$) < 3 then goto L11580
            code% = 1%
        return

        check_user
          code% = 0%
          if scr_id$ = "XXX" then goto L11770    /* (EWD003) - 06/04/98 */
/*SR68443 + */
          table$ = "AWDSCANN"
          code$  = scr_id$
          gosub check_code
          if code% <> 1% then goto L11770
          init(" ") scr_id_d$
/*SR68443 -READ #6,KEY = SCR_ID$, USING L11750, SCR_ID_D$,EOD GOTO L11770*/
REM L11750        FMT POS(4), CH(30)
           code% = 1%
        return
L11770:   if rf_scan% = 1% then goto L11775
          errormsg$ = "(Error) - Invalid User ID, 'ID' is Required?"
          gosub error_prompt
          init(" ") scr_id$, scr_id_d$
        return
L11775:   rf_errormsg$ = "Invalid User ID"
          gosub'500(fieldnr%)
          init(" ") scr_id$, scr_id_d$
        return

        check_load
          code% = 0%
          convert scr_load$ to scr_load%, data goto L11930

          convert scr_load% to scr_load$, pic(00000)

          goto L11940
                                                 /* Alpha or Stock Loads */
L11930:   convert str(scr_load$,2%,4%) to scr_load%, data goto L11920

          convert scr_load% to str(scr_load$,2%,4%), pic(0000)
L11940:

          read #7,key = scr_load$, using L11890, scr_load_d$,         ~
                                                    eod goto L11920
L11890:          FMT POS(16), CH(30)
            code% = 1%
        return
                                                /* This should Not Occur */
L11920:   if rf_scan% = 1% then goto L11925
          errormsg$ = "(Error) - Invalid Load Number. Load No. Required?"
          init(" ") scr_load$, scr_load_d$
        return
L11925:   rf_errormsg$ = "Invalid Load Number"
          init(" ") scr_load$, scr_load_d$
        return
        
        get_block

          read #7,key = dt_load$, using L12000, ld_ship_block$,         ~
                                                    eod goto L12009
L12000:          FMT POS(121), CH(3)
L12009:       
        return
                                                   /* (AWD043)           */
        check_code
          code% = 0%
          init(" ") readkey$, desc$
          str(readkey$,1%,9%)   = table$
          str(readkey$,10%,15%) = code$
          read #2,key = readkey$, using L12070, desc$, eod goto L12090
L12070:       FMT POS(25), CH(30)
          code% = 1%
L12090: return


REM------------------------------------------------------------------------
REM       B E G I N N I N G    O F    S C A N N I N G (AWD043             -
REM------------------------------------------------------------------------

        prod_scan                                    /* Scann Products   */
          cross_dock% = 0%                         /* (PAR002)         */
          updatedCrossDock% = 0%                   /* (SR70699)        */
                                                     /* (AWD045)         */
/*CR1191 */ if scr_id$=" " or scr_prt$=" " or scr_shft$=" " then goto L04105
   
          if keyhit% <> 3% or rf_scan% <> 1% then goto not_reprint
/* (AWD063) */

          if str(userid$,1%,2%) <> "RF" and userid$ <> "TR2" and           ~
             str(userid$,1%,2%) <> "CF" and str(userid$,1%,2%) <> "PF" and ~
             str(userid$,1%,2%) <> "PP" and str(userid$,1%,2%) <> "MF"     ~
                then goto not_reprint  /* CR1191  CR2493 CR2713 CR2960 */
                
             gosub reprint_label

not_reprint

          by_pass% = 0% : by_ups% = 0%             /* (AWD044)         */
          if userid$ = "BYP" then by_pass% = 1%
          if userid$ = "BYN" then by_pass% = 1%    /* (PAR002)         */

          if userid$ = "APN" and str(scr_id$,1%,2%) = "BY"              ~
                                                        then by_pass% = 1%
          if userid$ = "APT" and str(scr_id$,1%,2%) = "BY"              ~
                                                        then by_pass% = 1%
                                                       /* (PAR002)       */
                                                       /* (AWD044)       */
/* CMN - I believe checks for UPS so no trailer scan                     */
/*       probably need the same for Cart                                 */

          if userid$ = "APN" and str(scr_id$,1%,3%) = "BY6"             ~
                                                       then by_ups% = 1%
          if userid$ = "APT" and str(scr_id$,1%,3%) = "BY6"             ~
                                                       then by_ups% = 1%
                                                      /* (PAR002)        */
                                                      /* (AWD044)        */
                                                         /* (AWD045)     */
/* (AWD050) - begin */
          if str(userid$,1,2) = "SF" and                      ~
                      str(scr_id$,1%,2%) = "BY" then by_pass% = 1%
          if str(userid$,1,2) = "RF" and                      ~
                       str(scr_id$,1%,2%) = "BY" then by_pass% = 1%
/* (AWD050) - end */
/* (AWD063) */
          if userid$ = "TR2" and                      ~
                      str(scr_id$,1%,2%) = "BY" then by_pass% = 1%
          if userid$ = "TS2" and                      ~
                       str(scr_id$,1%,2%) = "BY" then by_pass% = 1%

/* CMN - TO DO IS this needed? anymore                                   */
/*   Yes to elimiate users who need to use APCSCANN from using AWDSCANN  */
                                                               /*(PAR002)*/
          if scr_id$ = "GLS" or scr_id$ = "SCN" or scr_id$ = "BYP" or   ~
             str(scr_id$,1%,2%) = "OV" or scr_id$ = "BYN" or            ~
             scr_id$ = "NSC" or str(scr_id$,1%,2%) = "ON" then goto L04100
                                                          /* (PAR002)    */
         goto prod_scan_2                 /*  STAGING  */
         
L04100: return clear all                            /* Invalid Screen  */
          err% = 21%                                /* Id.             */
          gosub err_scrn  
        goto initialize  
L04105: return clear all                            /* Missing Screen CR1191 */
          err% = 47%                                /* Id, Prtr or shift */
          gosub err_scrn   
        goto initialize   

REM------------------------------------------------------------------------
REM       S T A G I N G                                                   -
REM------------------------------------------------------------------------
        prod_scan_2                             /* !!!!  CART_SCAN !!!!! */

           if scr_sel% = 1% then goto cart_scan /* (CR456) !! CART_SCAN */
           if scr_sel% = 3% then goto prod_scan_3  /* LOAD SCAN         */
           init(" ") barcode$, wandchar$, xx$()    /* Staging Scan      */
           fieldnr% = 1%
           crossdock_bypass% = 0%
           cartPrint% = 0%   
/*SR74525   IF UPDATEDCROSSDOCK% <> 1% THEN GOSUB PROD_SCAN_2_INPUT      */
           if updatedCrossDock% <> 1% then goto prod_scan_2_input /*SR74525*/
              barcode$ = sav_bar$
           brand% = 99%
           goto prd_scn_2_updCrsDck
prod_scan_2_input:
           if rf_scan% <> 1% then gosub'150(fieldnr%)  /*ScanProdLabel  */
           if rf_scan%  = 1% then gosub'151(fieldnr%)
prd_scn_2_updCrsDck:
           errormsg$, rf_errormsg$ = " "
           if rf_scan%  = 1% and keyhit% = 2% then goto L04200
           if rf_scan% <> 1% and keyhit% = 16% then goto L04200
           if rf_scan%  = 1% and keyhit% = 3% then goto prod_scan

        goto prod_scan_2a
L04200: return clear all
        goto initialize
        prod_scan_2a
                                                   /* (PAR002)         */
          if cross_dock% = 1% then goto L04210     /* Check 2nd Label  */
          if cross_dock% = 2% then goto prod_scan         /* Error     */
                                                   /* (PAR002)         */
/* CR2713 */
/* user ID starting PP from 300 building staging paint can scan D barcodes */
          if str(userid$,1%,2%) = "PP" and str(barcode$,1%,1%) <> "D"   ~
               then goto invalid_paint_scan_300
                                                           /*(SR70701)*/
          init(" ") sav_bar$
          updatedCrossDock% = 0%
                                                            /* (SR72504) */
          if schema% = 1% and str(barcode$,1%,1%) = "A" then          ~
                                                   goto notUpdateCrossShip
          if schema% = 2% and str(barcode$,1%,1%) = "B" then          ~
                                                   goto notUpdateCrossShip
                                                           /* (\SR72504) */
          if str(barcode$,1%,1%) <> "A" and str(barcode$,1%,1%) <> "B" ~
                                              then goto notUpdateCrossShip
             updatedCrossDock% = 1%
             sav_bar$ = barcode$
/* CR3194 */
             prefix_so$ = "1"
             if str(barcode$,2%,1%) = "9" then prefix_so$ = "0"			 
             str(barcode$,1%,1%) = prefix_so$

notUpdateCrossShip:
/* CR2493 */ 
          if schema% = 1% and str(barcode$,1%,1%) = "D" and ~
             scr_sel$ = "4" then gosub reset_barcode_paint
                                                           /*(\SR70701)*/
          gosub check_shipping                  /* !! CHECK_SHIPPING   */
          if check% = 0% then goto prod_scan    /* err occurred - loop */
                                                /* (PAR002)            */
                                                /* (PAR004)            */
L04210:   if keyhit% = 10% and crossdock_bypass% = 1% then skipCrossCheck
          gosub check_cross_dock                /* !! CHECK_CROSS_DOCK */
          if cross_dock% = 1% then goto prod_scan_2    /* 2nd label    */


          if cross_dock% = 2% then goto prod_scan      /* Error        */
                                                /* No Error Continue   */
skipCrossCheck:
          gosub update_shipping                 /* !!   UPDATE_SHIPPING*/

        goto prod_scan                      /* Loop Back to Beginning */

/* CR2713 */
        invalid_paint_scan_300
          err% = 50%
          gosub err_scrn
          goto prod_scan
        return
        
        
/* CR1356 */
        no_brand 
           brand% = 99%
        return

REM------------------------------------------------------------------------
REM       C A R T   S C A N N I N G                                       -
REM------------------------------------------------------------------------
        cart_scan
          init(" ") cartNum$, cartDest$, wandchar$, errormsg$, rf_errormsg$, ~
                    cartSt$, cartLd$, cartSeq$ /* (CR780) */
          tt_unit% = 0%
          fieldnr% = 1%
          cartPrint% = 0%
          cartSt$ = "A"
          init(" ") prevcode$     /*  CR1191 reset on new cart nbr  */
/* Scan Cart Number */

        cartInput                                   /* Re-input on error */
          err%, cartseq% = 0%                    /* (CR780) */
          if rf_scan% <> 1% then gosub'155(fieldnr%)   /* Scan Cart      */
          if rf_scan%  = 1% then gosub'156(fieldnr%)   /* Scan Cart      */
          if rf_scan%  = 1% and keyhit% = 2% then goto l04320
          if rf_scan% <> 1% and keyhit% = 16% then goto l04320
          gosub formatCart
          gosub validateDestination
          if err% > 0% then cartInput           /* Loop Back to show Err */

/* Check if cart exist and ask to delete  */
          gosub cartCheck
                        /* If cart% = 0% then means deleted inactive cart*/
                        /* scan Cart Again only if active                */
REM          IF CARTDEL% = 1% AND CART% <> 0% THEN GOTO CART_SCAN
REM          IF CARTEXISTS% = 1% AND CART% = 0% THEN GOTO CART_SCAN
cartNxtBar:                                         /* Scan Next Barcode */
                    /* Scan Barcodes to cart; check production status etc*/
                    /* Scan Prod Label                                   */
          init(" ") barcode$, wandchar$,  xx$()   /* prevcode$, CR1191 Prv not displaying */
          if rf_scan% <> 1% then gosub'150(fieldnr%)    /*Barcode Scan   */
          if rf_scan%  = 1% then gosub'151(fieldnr%)
          rprt_flg% = 0%
          if rf_scan%  = 1% and keyhit% = 2% then goto cart_scan
          if rf_scan%  = 1% and keyhit% = 3% then gosub reprint_label /*CR1698*/
          if rprt_flg% = 1% then goto cartNxtBar
          if rf_scan% <> 1% and keyhit% = 16% then goto cart_scan

          gosub check_shipping
            if check% = 0% then goto cartNxtBar   /* err occurred - loop */
          barcode_shp$ = " "            
          gosub CartUpdate
          if err% <> 0% then goto cartNxtBar
          gosub update_shipping
          goto cartNxtBar                              /* LOOP         */

L04320:
          call "ALLFREE"
          return clear all
          goto initialize
          
        printCartLabel
          fieldnr% = 1%
          init(" ") cartNum$, wandchar$, errormsg$, rf_errormsg$
          fieldnr% = 1%
          labelprint% = 0%
        cartPrintInput                              /* Re-input on error */
          err% = 0%
          if rf_scan% <> 1% then gosub'175(fieldnr%)   /* Scan Print Cart*/
          if rf_scan%  = 1% then gosub'176(fieldnr%)   /* Scan Print Cart*/
          if rf_scan%  = 1% and keyhit% = 2% then goto endCartPrnt
          if rf_scan% <> 1% and keyhit% = 16% then goto endCartPrnt

          cartNum$ = barcode_trailer$
          gosub formatCart
          if err% > 0% then cartPrintInput
          init(" ") cartKey$, cartBar$
          cartKey$ = cartNum$
nxtCartRecCheck:
          gosub cartRecCheck
          if cart% = 0% then goto noCartRec                /* No Cart Found */
          barcode$ = str(cartKey$,18%,18%)
          gosub update_shipping
           if err% <> 0% then goto nxtCartRecCheck     /* no add to count*/
             labelprint% = labelprint% + 1%            /* on error       */
          goto nxtCartRecCheck
noCartRec:

REM F LABELPRINT% = 0% THEN RETURN
         if labelprint% = 0% then goto skipcartprintmsg
         gosub cartPrintMsg
/* an attempt to delete carts after printing labels */
REM GOSUB UPDATECARTSTATUS
skipcartprintmsg:

         gosub CartDel2                        /* SR78815 */
REM         CARTPRINT% = 0%
         call "ALLFREE"

         goto cartPrintInput
endCartPrnt:
        return clear all
        goto initialize

REM------------------------------------------------------------------------
REM       L O A D I N G                                                   -
REM------------------------------------------------------------------------
        prod_scan_3
/*(SR70699)*/
            init(" ") sav_bar$
            updatedCrossDock% = 0%
                                                    /* (AWD044)          */
            if by_ups% = 1% then goto prod_scan_3b

            fieldnr% = 1%
            init(" ") barcode_trailer$, wandchar_trailer$
                                                         /* (AWD045)     */
            if rf_scan% <> 1% then gosub'175(fieldnr%)   /* Scan Trailer */
            if rf_scan%  = 1% then gosub'176(fieldnr%)   /* Scan Trailer */
            if rf_scan%  = 1% and keyhit% = 2% then goto L04220
            if rf_scan% <> 1% and keyhit% = 16% then goto L04200
                      goto prod_scan_3a
L04220:        return clear all
               goto initialize
        prod_scan_3a
            gosub check_trailer
            if trailer%=1% then gosub set_load_start
                                                    /* (AWD044)          */
        prod_scan_3b
            if by_ups% = 1% then trailer% = 1%

            if trailer% = 0% then goto prod_scan_3

        prod_scan_4
            fieldnr% = 1%
            init(" ") barcode_shp$, wandchar_shp$, awd_app_key0$
            init(" ") barcode$, wandchar$, xx$()
        prod_scan_4a
                                                           /* (AWD045)   */
               if rf_scan% <> 1% then gosub'100(fieldnr%)  /* Shipping1st*/
               if rf_scan%  = 1% then gosub'101(fieldnr%)  /* Shipping1st*/
                                                              /*(SR70699)*/
               init(" ") sav_bar$
               updatedCrossDock% = 0%
               if schema% = 1% and str(barcode$,1%,1%) = "A" then           ~
                                                   goto notUpdatedCrossDock
               if schema% = 2% and str(barcode$,1%,1%) = "B" then           ~
                                                   goto notUpdatedCrossDock
/* CR2493 CR3194 */                                              
               if schema% = 1%                    and   ~
                  str(barcode$,1%,1%) =  "D"      and   ~
                  ((str(barcode_shp$,1%,1%) = "0"  or   ~
                    str(barcode_shp$,1%,1%) = "1") or   ~				  
                   str(barcode_shp$,1%,1%) = "A")    then goto L04225
                                              
               if str(barcode$,1%,1%) <> "A" and str(barcode$,1%,1%) <> "B" ~
                                              then goto notUpdatedCrossDock

                 updatedCrossDock% = 1%
L04225:
                 sav_bar$ = barcode$
                 str(barcode$,1%,1%) = str(barcode_shp$,1%,1%)  /* CR2493 fix */
                                                                /* SR100475 */

notUpdatedCrossDock:
                                                             /*(\SR70699)*/
               errormsg$, rf_errormsg$ = " "
               if rf_scan%  = 1% and keyhit% = 2% then goto L04230
               if rf_scan% <> 1% and keyhit% = 16% then goto L04230
                      goto prod_scan_4b
L04230:           return clear all
                  goto initialize

        prod_scan_4b
            if fieldnr% = 2% then goto prod_scan_4d
               gosub check_shipping_appian
               if check% = 0% then goto prod_scan_4
                                                    /* Check for override*/
               gosub check_drop_override
               if check% = 0% then goto prod_scan_4 /* No Override       */
               fieldnr% = 2%
               goto prod_scan_4a

        prod_scan_4d
            gosub check_appian_production
            if check% = 0% then goto prod_scan_4

            gosub check_shipping
            if check% = 0% then goto prod_scan_4

               gosub update_shipping                    /* First         */
               gosub update_shipping_appian             /* Second        */
            if by_pass% = 1% then goto prod_scan        /* Startover     */
               goto prod_scan_4

REM    ************** End of Scanning   (AWD043) ***************
/* CR1207 +   Area with tracking cart screen and process added  */
REM------------------------------------------------------------------------
REM      Cart Tracking                                     CR1207         -
REM------------------------------------------------------------------------
        cart_tracking
          init(" ") scr_truck$, cnt$, cnt2$
          cnt% = 0%
   
          gosub rf_scr_cart_track1
          
          if keyhit% = 2% then return
          
          if scr_truck$ = " " then return
          gosub act_truck
      
          gosub rf_cart_track2
          
        return
        
        rf_scr_cart_track1
          cnt%  = 0%
          cnt3% = 0%
       
          gosub rf_set_tracking
          accept                                                         ~
               at (01,02), fac(hex(8c)), rf_pname$              , ch(08),~
               at (01,12), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), rf_errormsg$           , ch(20),~
                                                                         ~
               at (03,02), "Truck #: ",                                  ~
               at (03,12), fac(lfac$(1%)), scr_truck$           , ch(08),~
                                                                         ~
               at (08,02), fac(hex(8c)),   rf_pf$(1%)           , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               rf_errormsg$ = " "
               errormsg$    = " "                         
               
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return
        
        rf_set_tracking
          lfac$(1%) = hex(81)
          init(" ") dateout$, scrn_title$
          call "TIME" (dateout$)
          rf_inpmessage$ = "Scan Truck Nbr"
          rf_pf$(1%) = "(1)Scn (2)Ext"
             pfkeys$ = hex(0102ffffffffffffffffffffffffffffff00)
        return                                       
  
        rf_cart_track2
          init(" ") scr_cart$
          gosub rf_scr_cart_track2
          
          if keyhit% = 1% then goto cart_tracking     /* change truck # */
          if keyhit% = 2% then return                 /* exit           */
          
          if scr_truck$ = scr_cart$ then goto rf_cart_track2
          if scr_cart$ = " " then rf_cart_track2
          
          gosub rf_write_tracking
          if w_okay% = 1% then goto L05000
          cartNum$ = scr_cart$                        /* close if cart on */
          cur_truck$  = scr_truck$                    /* do not close current */
          gosub close_cart                            /* another truck    */
           
          rf_errormsg$ = "Successful " & scr_cart$
          srchCart$ = scr_cart$
          cnt3% = 0%   /* reset for new scanned cart */
          gosub CntWindows
          cnt% = cnt% + cnt3%
          convert cnt% to cnt$, pic(00000000)
          cnt2% = cnt2% + 1                          /* truck total count */
          convert cnt2% to cnt2$, pic(00000000)
          init(" ") scr_cart$
          call "ALLFREE"
          goto L05500
L05000:
          rf_errormsg$ = "Error Tracking"    

L05500:   goto rf_cart_track2
          
        return
        
        rf_scr_cart_track2
          gosub rf_set_tracking2
          accept                                                         ~
               at (01,02), fac(hex(8c)), rf_pname$              , ch(08),~
               at (01,12), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), rf_errormsg$           , ch(20),~
                                                                          ~
               at (03,02), "Truck # ",                                   ~
               at (03,12), fac(hex(8c)), scr_truck$             , ch(08),~
                                                                         ~
               at (04,02), "Cart #: ",                                   ~
               at (04,12), fac(lfac$(1%)), scr_cart$            , ch(08),~
                                                                         ~    
               at (05,03), "Trk Pcs  ",                                  ~
               at (05,12), fac(hex(8C)), cnt$                   , ch(08),~
                                                                         ~
               at (06,03), "Trk Carts",                                  ~
               at (06,12), fac(hex(8C)), cnt2$                  , ch(08),~
                                                                         ~                                                                         
               at (08,02), fac(hex(8c)),   rf_pf$(1%)           , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               rf_errormsg$ = " "
               errormsg$    = " "                         
               
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return
          
        rf_set_tracking2
          lfac$(1%) = hex(81)
          init(" ") dateout$, scrn_title$
          call "TIME" (dateout$)
          rf_inpmessage$ = "Scan Cart Nbr"
          rf_pf$(1%) = "(1)Chg Truck  (2)Ext"
             pfkeys$ = hex(0102ffffffffffffffffffffffffffffff00)
        return     

        rf_write_tracking      
           init(" ") tk_key$, tk_rec$, dateout$
           
           cartNum% = 0%             /* zero left fill numeric cart number */
           convert scr_cart$ to cartNum%, data goto L05005
           convert cartNum% to scr_cart$, pic(00000000)
L05005:           
           w_okay% = 1%
           str(tk_key$,1%,8%) = scr_truck$
           str(tk_key$,9%,8%) = scr_cart$
           
           read #18, hold, key > tk_key$, using L05010, tk_rec$, ~
                  eod goto write_tracking
L05010:      FMT CH(75)
           if str(tk_rec$,1%,8%) <> scr_truck$ or ~
              str(tk_rec$,9%,8%) <> scr_cart$ then goto write_tracking
           
           if str(tk_rec$,17%,1%) <> "A" then goto write_tracking 
           
           delete #18
           
        write_tracking
            str(tk_rec$,1%,8%) = scr_truck$        /* truck number */
            str(tk_rec$,9%,8%) = scr_cart$         /* cart number  */
            str(tk_rec$,17%,1%) = "A"              /* status       */       
            str(tk_rec$,18%,6%) = date             /* scan date    */
            call "TIME" (dateout$)
            str(tk_rec$,24%,8%) = dateout$         /* scan time    */
            str(tk_rec$,32%,6%) = " "              /* close date   */
            str(tk_rec$,38%,8%) = " "              /* close time   */
          
            put #18, using L05010, tk_rec$
            
            write #18
            
            w_okay% = 0% 

        return
REM ------------------------------------------------------------------
REM - Check if a truck has at least 1 active record plus count carts -    
REM ------------------------------------------------------------------
        act_truck
          ask_response% = 1%
          cnt2% = 0%
          cnt3% = 0%
          init(" ") tk_key$
                    
          str(tk_key$,1%,8%) = scr_truck$
          str(tk_key$,9%,8%) = "        "
 
        check_truck_nxt
          read #18, key > tk_key$, using L05100, tk_key$, eod goto L05175
          
L05100:      FMT CH(31)

          
          if str(tk_key$,1%,8%) <> scr_truck$ then goto L05175
          if str(tk_key$,17%,1%) <> "A" then goto check_truck_nxt
          
          if ask_response% = 1% then gosub truck_active
                    
          cnt2% = cnt2% + 1                        /* truck total count */
          srchCart$ = str(tk_key$,9%,8%) 
          gosub CntWindows 
          goto check_truck_nxt
L05175:    
          cnt% = cnt3%                   
          convert cnt3% to cnt$, pic(00000000)
          convert cnt2% to cnt2$, pic(00000000)
        return

        
REM ----------------------------------------------------------------
REM - Count the windows on the cart                        CR1207  -    
REM ----------------------------------------------------------------
        CntWindows

          str(cartKey$,1%,8%)  = srchCart$
          str(cartKey$,9%,1%)  = "A"
          str(cartKey$,10%,8%) = "00000000"
          
        CntWinNxt
        
          read #15, key > cartKey$, using CRTFMT, cartKey$, eod goto L05180
          
          if str(cartKey$,1%,8%) <> srchCart$ then goto L05180
          if str(cartKey$,9%,1%) <> "A" then goto L05180
          
          cnt3% = cnt3% + 1
               
        goto CntWinNxt
L05180:
        return

REM ----------------------------------------------------------------
REM - Ask if truck to be closed or continue loading        CR1207  -    
REM ----------------------------------------------------------------
        truck_active
          gosub trail_scr1
          ask_response% = 0%
          if keyhit% = 4% then return
          
          gosub close_truck
          cnt2% = -1%    /* Deleted truck account for initial count of 1 */
          call "ALLFREE"
        return
          
        trail_scr1        
          gosub set_active_scr
          accept                                                       ~
               at (01,02), fac(hex(94)), rf_errormsg$           , ch(20),~
                                                                         ~
               at (02,02), fac(hex(8c)), rf_msg$(1%)            , ch(20),~
               at (03,02), fac(hex(8c)), rf_msg$(2%)            , ch(20),~
               at (04,02), fac(hex(8c)), rf_msg$(3%)            , ch(20),~
               at (05,02), fac(hex(8c)), rf_msg$(4%)            , ch(20),~
                                                                         ~
               at (07,02), fac(hex(a4)),   rf_inpmessage$       , ch(20),~
               at (08,02), fac(hex(8c)),   rf_pf$(1%)           , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_active_scr
          lfac$(1%) = hex(81)
          rf_inpmessage$ = "Truck " & scr_truck$
          rf_pf$(1%)     = "1) Close 4) Continue"
          pfkeys$        = hex(01ffff04ffffffffffffffffffffffffff)
        return
        
        close_truck
          ask_response% = 0%
          init(" ") dateout$, tk_key$
          
          call "TIME" (dateout$)
          str(tk_key$,1%,8%) = scr_truck$
          str(tk_key$,9%,8%) = " " 
          str(tk_key$,17%,1%) = "A"
          
        close_nxt_truck  
          read #18, hold, key > tk_key$, using L05200, tk_rec$, eod goto L05250
                    
L05200:      FMT CH(75)

          tk_key$ = str(tk_rec$,1%,31%)
          if str(tk_rec$,1%,8%) <> scr_truck$ then goto L05250
          if str(tk_rec$,17%,1%) <> "A" then goto close_nxt_truck
          
          delete #18
          
          str(tk_rec$,17%,1%) = "C" 
          str(tk_key$,17%,1%) = "C"
          str(tk_rec$,32%,8%) = date
          str(tk_rec$,38%,8%) = dateout$
          
          put #18, using L05200, tk_rec$
          write #18, eod goto L05250          
          goto close_nxt_truck 
          
L05250:   return

        close_cart
          init(" ") dateout$, tk_key2$
          call "TIME" (dateout$)
                    
          str(tk_key2$,1%,8%) = cartNum$
          str(tk_key2$,9%,1%) = " "
          
        close_nxt_cart 
          read #18, hold, key 1% > tk_key2$, using L05275, tk_rec$, ~
                  eod goto L05300
                    
L05275:      FMT CH(75)

          tk_key2$ = str(tk_rec$,9%,9%)                    
          if str(tk_rec$,9%,8%) <> cartNum$ then goto L05300
          if str(tk_rec$,17%,1%) > "A" then goto L05300
          if str(tk_rec$,1%,8%) = cur_truck$ then goto close_nxt_cart
          
          delete #18 
          
          str(tk_rec$,17%,1%) = "C" 
          str(tk_key2$,9%,1%) = "C"
          str(tk_rec$,32%,8%) = date
          str(tk_rec$,38%,8%) = dateout$ 
           
          put #18, using L05275, tk_rec$
          write #18, eod goto L05300      
          goto close_nxt_cart
          
L05300:   return

/* CR1207 - End of major area of change for tracking carts */        
REM ***************** CHECK_SHIPPING             ***************
/* (AWD028) (AWD029) (AWD033) (AWD043) */
REM ***************** make sure status is correct **************

        check_shipping                            /* Staging/Loading     */
            override% = 0%
            err%   = 0%
            hit%   = 0%
            check% = 0%
            dt_st% = 0%           /* (APCPLNDT) - FILE   */
/* CR1356 add brand and sku */
            init(" ") dt_key$, errormsg$, dt_bar$, dt_rec$, dt_dept$,     ~
                      dt_load$, rf_errormsg$, cross_warranty1$,           ~
                      cross_part1$, cross_warranty$, dt_brand$, dt_sku$,  ~
                      dt_cust$

            str(dt_key$,1%,18%) = barcode$        /* Set Barcode Value   */
        check_shipping_nxt
            read #1,key > dt_key$, using L05220 ,dt_load$, hld_drop$, dt_key$,~
                      dt_st$, cross_warranty1$, dt_cust$, cross_part1$,       ~
                      dt_brand$, dt_sku$,  ~
                    eod goto L05430                             
/* CR1356 - note check will read past barcode */
/* CR1717  add drop field in read     CR2128 add customer        */
L05220:        FMT CH(5), POS(11), CH(02), POS(24), CH(23), POS(64), CH(2), ~
                   POS(96), CH(8), POS(124), CH(09),                        ~
                   POS(189), CH(25), POS(243), CH(02), CH(09)

            if barcode$ <> str(dt_key$,1%,18%) then goto L05430
               dt_bar$  = str(dt_key$,1%,18%)             /* (PAR002)    */
               dt_dept$ = str(dt_key$,19%,3%)
               dt_proc$ = str(dt_key$,22%,2%)                /* (CR456)  */
               cartLd$  = dt_load$                           /* (CR456)  */
               cartDrp$ = hld_drop$                          /* CR1698  */
               dt_part$ = cross_part1$
               convert dt_st$ to dt_st%, data goto L05580

               if dt_dept$ = "001" then goto check_shipping_nxt
REM               IF SCR_SEL% <> 2% THEN GOTO L05380

/* CR1717 */
/* Cart Scanning only; if shipping label not exist, display error then  */
/* return for next scan                                                 */
               init(" ") howship$ 
               if scr_sel% = 1% then gosub validate_appian_exist
               if scr_sel% = 1% and check% = 0% then return 
               if scr_sel% = 1% and howship$ = "09" then goto cartTechPick
                          /* if cart shp label not found, return CR1791 */  
/* 2128 */
REM              if scr_sel% = 1% and dt_cust$ = "CV0999" then goto cart300Ship
REM CR2565               scr_load$ = dt_load$
               init(" ") ld_ship_block$
               gosub get_block
               if scr_sel% = 1% and ld_ship_block$ = "300"  ~
                          then goto cart300Ship
/* CR2713 add new ship block checks  */
               if scr_sel% = 1% and ld_ship_block$ = "3CC"  ~
                          then goto cart3CCShip
               if scr_sel% = 1% and ld_ship_block$ = "CVP"  ~
                          then goto cartCVPShip
               
REM ---------------
REM  skip below logic & check loading logic when scr_sel% = 3%
REM ---------------
               if scr_sel% = 3% then goto L05380      /* (CR456) Loading */
/* cross_warranty1$ Warrant Number from DT if crossDock should be 'S'num */

                  cross_key$      = dt_key$
                  cross_warranty$ = cross_warranty1$
/* Dont think need 105 here should have been caught putting on cart      */

REM CR2493           if dt_dept$ = "102" and dt_st% < 12% then  /* PULL STK*/~
                                                  goto L05360

/*SR64968 IF DT_DEPT$ = "104" AND DT_ST% < 12% THEN PULL MUL GOTO L05360 */
/*SR64968 IF DT_DEPT$ = "101" AND DT_ST% < 12% THEN PULLCROS GOTO L05360 */
                                                        /* (AWD033)      */
                                                        /* Not Completed */
                                                        /* by Department */
/* If dt_st% < 12% with normal staging then autocomplete dept 044 & 054  */
                  if dt_st% < 12% then goto L05450      /* Staging Test  */
                                                        /* (AWD033)      */
                                                        /* (AWD036)      */
REM               IF DT_ST% > 12% THEN GOTO L05640
/* allow repair (rga) product to go back through staging                 */
REM                  IF DT_ST% = 15% THEN GOTO L05360      /* (AWD048)   */
                  if dt_st% = 26% then goto L05360  /*(CR456)CART - hit% */
                  if dt_st% = 32% then goto L05360  /*(CR456)RGA  - hit% */
                  if scr_sel% = 1% and str(barcode$,1%,1%) = "C"   ~
                            then goto L05510
                            
                  if wood_rmk% = 1% then goto L05360           /* AWD058 */
                  if dt_st% > 13% then goto L05640

L05360:              hit% = hit% + 1%                   /* Ready to Stag */
                     sav_sku$ = dt_sku$                 /* CR1356 */
                     convert dt_brand$ to brand%, data gosub no_brand
                     goto check_shipping_nxt    /* END OF SCR_SEL% <> 2% */
L05380:                                    /* BEGINNING OF SCR_SEL% = 3% */
                                                        /* (AWD043)      */
                                                         /* (AWD044)     */
               if userid$ = "APN" and str(scr_id$,1%,3%) = "BY6" then     ~
                  scr_load$ = dt_load$
               if userid$ = "APT" and str(scr_id$,1%,3%) = "BY6" then     ~
                  scr_load$ = dt_load$
                                                         /* (AWD044)     */

               if scr_load$ <> dt_load$ then goto L05720   /* Check Load */
                                         /* Per Keith Barnes remove Test */
                   /* (EWD021) Per Keith Barnes & Kyle Green to add Test */
                                                        /* Must be Staged*/
/*  wood_rmk% is set to 1% when pf8 is hit                               */
/*  I thought this was to scan back to status 11 for production remake?  */

               if wood_rmk% = 1% then goto L05420    /* AWD058 */
/* If dt_st% < 14% auto complete dept.  will this stay on for dc center? */
/*     No b/c product will be status 12 need to by-pass dt_st% < 14% chk */
/*     for cart scanning                                                 */

                 if scr_sel% = 1% then goto L05420           /* (CR456)  */
                 if dt_st% < 14% then goto L05450            /* Load Test*/
                                                             /* (AWD030) */
                 if dt_st% > 15% then goto L05640            /*err%EWD001*/
                                                             /* (AWD030) */
L05420:           hit% = hit% + 1%                           /*ReadyToLoa*/
                  goto check_shipping_nxt
L05430:     if hit% <> 0% then check% = 1% else goto L05580
        return
L05450:     check% = 0%                    /* scr_sel% - 2 = staging     */
            isSupp% = 0%
                 /* only auto complete if scr_sel% = 2%! staging not cart*/
            if cartDest$ = "300" then return               /* (CR456)    */
            if scr_sel% = 1% then gosub L05510   /* check support dept CR1791 */
            if isSupp% = 1% then return         /* no error continue  */
            if scr_sel% = 3% then goto L05530      /* err% = 9%  CR1194 */
            if schema% = 2% and dt_dept$ = "021"  ~
                   then goto L05510         /* CR1170 (AWD033)   */
                   
               override% = 1%
               if dt_dept$ = "044" then goto L05525        /* (AWD042) CR2782 */
               if dt_dept$ = "054" then goto L05520        /* (AWD052)   */
               if dt_dept$ = "064" then goto L05520        /* CR1716 */ 
               if dt_dept$ = "051" then goto L05522        /* CR2825 */
               if dt_dept$ = "076" then goto L05523        /* CR2883 */
               
                 
REM               IF DT_DEPT$ = "074" THEN GOTO L05520     /* (AWD057)   */

               init(" ") override_key$
               str(override_key$,1%,18%) = barcode$
               str(override_key$,19%,3%) = dt_dept$
               str(override_key$,22%,2%) = scr_proc$
               gosub update_track_override
               if override% > 1% then return         /* (Error) occurred */
                                                     /* Exit routine     */
                                       /* Now the product can be staged  */
               goto check_shipping     /* Loop Next Dept                 */
                                       /* Normal Exit for Staging        */
L05520:        err% = 8%                              /* (AWD042)        */
               str(err$(8%),33%,3%) = dt_dept$
               str(rf_err$(8%),16%,3%) = dt_dept$
               goto L05560
/* CR2782 */              
L05525:        err% = 42%
/* CR2794 removed debug statement */
               str(rf_err$(8%),16%,3%) = dt_dept$
               goto L05560
/* CR2825 */               
L05522:        err% = 52%
               goto L05560
               
/* CR2883 */
L05523:        err% = 53%
               goto L05560               
               
L05530:     err% = 9%
L05560:     gosub err_scrn
            if scr_sel% <> 2% then gosub error_display     /* (EWD020)   */
        return
L05580:     check% = 0%
            err% = 1% : gosub err_scrn
            if scr_sel% <> 2% then gosub error_display     /* (EWD020)   */
        return
L05640:     check% = 0%
            if scr_sel% = 2% then err% = 13% else err% = 14%
/*CR23493*/ if scr_sel% = 4% then err% = 13% else err% = 14%
/*SR78815*/ if scr_sel% = 3% and dt_st% = 26% then err% = 46%
            gosub err_scrn
/* (EWD020) CR2493  */
            if scr_sel% <> 2% and scr_sel% <> 4% then gosub error_display 
        return
L05720:     check% = 0%
            err% = 16% : gosub err_scrn
            if scr_sel% <> 2% then gosub error_display     /* (EWD020)   */
        return

REM -------
REM  cart error
REM  added error messages for cart scanning
REM -------
L05510:
          isSupp% = 1%

          if dt_dept$ = "021" then goto cartSampErr
          if dt_dept$ = "032" then goto cartRgaErr
          if dt_dept$ = "044" then goto cartWoodErr
          if dt_dept$ = "051" then goto cartQCErr
          if dt_dept$ = "076" then goto cartWPErr        /* CR2883 */
          if dt_dept$ = "074" then goto cartCasingErr
          if str(barcode$,1%,1%) = "C" then goto cartBayErr
     /* autocomplete stop CR1791     goto cartDeptErr      */
          isSupp% = 0%
          
        return

cartSampErr:
          err% = 40%
          gosub err_scrn
          gosub error_display
        return
cartRgaErr:
          err% = 41%
          gosub err_scrn
          gosub error_display
        return
cartWoodErr:
          err% = 42%
          gosub err_scrn
          gosub error_display
        return
cartCasingErr:
          err% = 43%
          gosub err_scrn
          gosub error_display
        return
cartBayErr:
          err% = 44%
          gosub err_scrn
          gosub error_display
        return
cartDeptErr:
          err% = 45%
          str(err$(45%),27%,3%) = dt_dept$
          str(rf_err$(45%),15%,3%) = dt_dept$
          gosub err_scrn
          gosub error_display
        return
        
/* CR1791 */         
cartTechPick:
         err% = 48%
         gosub err_scrn
         gosub error_display
         check% = 0%
       return
  
/* CR2128 */
cart300Ship:
          err% = 49%
          gosub err_scrn
          gosub error_display
          check% = 0%
        return 
  
/* CR2713 */
cart3CCShip:
          err% = 51%
          gosub err_scrn
          gosub error_display
          check% = 0%
        return 
  
/* CR2713 */
cartCVPShip:
          err% = 51%
          gosub err_scrn
          gosub error_display
          check% = 0%
        return 

/* CR2825 */
cartQCErr:
          err% = 52%
          gosub err_scrn
          gosub error_display
          check% = 0%
        return 
        
/* CR2883 */
cartWPErr:
          err% = 53%
          gosub err_scrn
          gosub error_display
          check% = 0%
        return 
        
REM **************************************************************************
REM * Validate shipping exists for Cart Scanning ONLY.  If not exit, display *
REM * error message and goto next label scan.    CR1717                      *
REM ************************************************************************** 
        validate_appian_exist
          str(barcode_shp$,1%,18%) = barcode$     /* check if shipping label  */
          str(barcode_shp$,19%,2%) = cartDrp$     /* was created, if not then */
          gosub check_shipping_appian             /* exit after error  CR1717 */
          barcode_shp$ = " "                      /* reset */
        return
    
REM ***************** UPDATE_SHIPPING            ***************
        update_shipping                           /* (APCPLNDT) - File   */
                                                  /* (AWD033) - Label    */
                                                             /* (AWD061) */
            override% = 0%
            if upd_st$ = " " then goto corruptWrite
            if scr_id$ = " " then goto corruptWrite

REM         IF SCR_SEL% = 2% THEN GOSUB PRINT_LABEL

/*AWD058  if scr_sel% = 2% and wood_rmk% = 0% then gosub print_label  */

          brand_ok$ = "Y"
          if schema% = 2% and rf_scan% = 0% then gosub check_brand_label
          if brand_ok$ = "N" then return
          
/* After starting */
          if scr_sel% = 2% and wood_rmk% = 0% and cart% <> 1% ~     
              then gosub print_label              /*  CR1166  */  
              
          if scr_sel% = 4% then gosub print_label              /*  CR2493  */ 
              
            if scr_sel% = 1% and upd_st$ = "26" and wood_rmk% = 0%   ~
                 then gosub print_label     /* CR1166 */
                  
            if (scr_sel% = 2% or scr_sel% = 4%) and err% = 28% then goto L07210
                                                  /* No Label Exit       */
                                                  /* (AWD033)            */
            init(" ") errormsg$, prevcode$, dateout$, dt_key$, dt_rec$,  ~
                      rf_errormsg$
            call "TIME" (dateout$)
            str(dt_key$,1%,18%) = barcode$
        update_shipping_nxt
            read #1,hold,key > dt_key$, using L06990 , dt_rec$,            ~
                                                     eod goto L07200
L06990:        FMT CH(256)
            dt_key$ = str(dt_rec$,24%,23%)
            if str(dt_key$,1%,18%) <> barcode$ then goto L07120
               current_drop$ = str(dt_rec$,11%,12%)            /*SR69112 */
               current_load$ = str(dt_rec$,1%, 5%)             /*SR69112 */
               delete #1
               str(dt_rec$,53%,6%) = date            /* Product Scan Date*/
/* Do not allow completed units to update status for NC cart scan   */
/* CR1191 */   if scr_sel% = 2% and cart% = 1% and  ~    
                  str(dt_rec$,64%,2%) = "16" then goto L06991
                 str(dt_rec$,64%,2%) = upd_st$         /* Product Scanned  */
L06991:
               str(dt_rec$,116%,8%)= dateout$        /* TimeProdScanned  */
               put #1, using L06990 , dt_rec$
               write #1, eod goto L07200
               ad_dept$ = str(dt_rec$,42%,3%)      /* Scanning Department*/
               ad_proc$ = str(dt_rec$,45%,2%)      /* Scanning Process   */
               modulename$ = "UPDATE_SHIPPING"
               gosub update_audit
/*AWD059*/     call "AWDCOMSB" (dt_rec$,#42,scr_dept$,scr_id$)

               goto update_shipping_nxt
L07120:     prevcode$ = barcode$
/*No matter the selection all 1 to tt_unit%                            */
            tt_unit% = tt_unit% + 1%             /* Calc Scanned Units */

            if scr_sel% <> 1% then goto skip_cart  /* (CR456)          + */
              ad_dept$ = "105"                     /* Cart Scanning      */
              goto creatAudit

skip_cart:
REM              if scr_sel% <> 2% then goto skip_stag  /* (CR456)           -*/
              if scr_sel% <> 2% and scr_sel% <> 4% then goto skip_stag  
                                                   /* (CR456)  CR2493  -*/
              ad_dept$ = "106"                     /* Staging            */
              goto creatAudit                      /* (CR456)            */
skip_stag:                                         /* (CR456)            */
                                                             /*SR69112 + */
            if scr_sel% <> 3% then goto skip_load
               ad_dept$ = "108"                    /* Loading            */
               tt_unit2% = tt_unit2% - 1%          /*Total NOT loaded    */
skip_load:
                                                             /*SR69112 - */
creatAudit:
            ad_proc$ = "01"
            modulename$ = "UPDT_SHP-AFTSCRSEL"
            gosub update_audit
                                                            /* (SR70699) */
            if cartPrint% = 1% then return                  /*  (CR456)  */
            if updatedCrossDock% = 0% then gosub ok_scrn
                                                   /* (AWD030)           */
        REM IF SCR_SEL% = 2% THEN GOSUB PRINT_LABEL

                                                   /* (AWD030)           */
        return
L07200:    err% = 26% : gosub err_scrn
        return
L07210:    err% = 29% : gosub err_scrn
        return                                     /* (AWD031) - (AWD033)*/
        
REM *************** TX Brand check routines ********************
REM * CR1356 - if TX Staging and not cart (TX does not cart),  *
REM *   verify the order SKU if found.  Set flag if the SKU and*
REM *   UPC match in file.  Help user to get the correc brand  *
REM *   label on the window.                                   *
REM ************************************************************ 
        check_brand_label 
        
           if scr_sel% <> 2% then goto L07220  /* routine only for staging */
           if cart% = 1% then goto L07220    /* routine not for cart scanning */

           brand_ok$ = "Y"                           
           init(" ") errormsg$ 
L07215:    YNFlag$ = "Y"                 /* CR1936 remove question */
           goto L07218                   /* CR1936 skip down to continue */
           /*  gosub scr_yn_sku             CR1936 */       

           if YNFlag$ = "Y" or YNFlag$ = "N" then goto L07218           
           errormsg$ = "You must answer with Y or N"
           goto L07215
           
/* CR1936  L07218:    if YNFlag$  =  "N" then goto L07222  */
L07218:
/*  it must be yes */
           if sav_sku$ = " "  or sav_sku$ = "0" then goto L07220 

           gosub read_sos_sku              /* if DT SKU found, skip process */
           if sosexits% = 1% then goto L07220
           
           if YNFlag$  =  "Y" then gosub scan_upc 
     
L07220:    return 

L07222:    brand_ok$ = "N"   
           gosub display_msg
           return
           
        scr_yn_sku
          gosub set_YN_screen
          accept                                                         ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), scrn_YN_title$         , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Enter Y or N :"                             ,~
               at (05,20), fac(lfac$(1%)), YNFlag$              , ch(01),~
                                                                         ~
               at (07,02), fac(hex(84)), brand$(brand%,1%)      , ch(40),~
               at (07,43), fac(hex(84)), scr_line1$             , ch(38),~
               at (08,02), fac(hex(84)), brand$(brand%,2%)      , ch(40),~
               at (08,43), fac(hex(84)), scr_line2$             , ch(38),~
               at (09,02), fac(hex(84)), brand$(brand%,3%)      , ch(40),~
               at (09,43), fac(hex(84)), scr_line3$             , ch(38),~
               at (10,02), fac(hex(84)), brand$(brand%,4%)      , ch(40),~
               at (10,43), fac(hex(84)), scr_line4$             , ch(38),~
               at (11,02), fac(hex(84)), brand$(brand%,5%)      , ch(40),~
               at (11,43), fac(hex(84)), scr_line5$             , ch(38),~
               at (12,43), fac(hex(84)), scr_line6$             , ch(38),~
                                                                         ~
               at (21,02), fac(hex(a4)),   uinpmessage$         , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return
        set_YN_screen

          init(" ") dateout$, YNFlag$, scrn_YN_title$
          uinpmessage$ = "A Brand Label is Required, Please Verify"
          call "TIME" (dateout$)
                                                    
          scrn_YN_title$ = "Brand Label Verification of Window"

          pf$(1%) = "Y or N then Enter                       " &       ~
                     "                                       " 
          pf$(2%) = "                                        "
          pf$(3%) = "                                        " &       ~
                     "                                       "
          pfkeys$ = hex(ffffffffffffffffffffffffffffff1000)

          lfac$(1%) = hex(81)
         
        return

REM ***********************************************************
REM *  CR1356 - Scan the UPC being used on window to confirm  *
REM *  that the correct brand label is used.                  *
REM ***********************************************************
        scan_upc
          init(" ") errormsg$
L07230:   gosub set_upc_screen
          accept                                                         ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,26), fac(hex(a4)), scr_load_d$            , ch(30),~
               at (02,21), fac(hex(a4)), scrn_upc_title$        , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(84)), fldx$(1%)              , ch(23),~
               at (05,27), fac(lfac$(1%)), scn_upc$             , ch(15),~
                                                                         ~
               at (05,52), fac(hex(84)), fldx$(2%)              , ch(04),~
               at (05,57), fac(hex(84)), uprevcode$             , ch(15),~
                                                                         ~
               at (07,02), fac(hex(84)), brand$(brand%,1%)      , ch(40),~
               at (07,43), fac(hex(84)), scr_line1$             , ch(38),~
               at (08,02), fac(hex(84)), brand$(brand%,2%)      , ch(40),~
               at (08,43), fac(hex(84)), scr_line2$             , ch(38),~
               at (09,02), fac(hex(84)), brand$(brand%,3%)      , ch(40),~
               at (09,43), fac(hex(84)), scr_line3$             , ch(38),~
               at (10,02), fac(hex(84)), brand$(brand%,4%)      , ch(40),~
               at (10,43), fac(hex(84)), scr_line4$             , ch(38),~
               at (11,02), fac(hex(84)), brand$(brand%,5%)      , ch(40),~
               at (11,43), fac(hex(84)), scr_line5$             , ch(38),~
               at (12,43), fac(hex(84)), scr_line6$             , ch(38),~
                                                                         ~
               at (14,16), fac(hex(84)), up$(1%)                , ch(50),~
               at (15,16), fac(hex(84)), up$(2%)                , ch(50),~
               at (16,16), fac(hex(84)), up$(3%)                , ch(50),~
               at (17,16), fac(hex(84)), up$(4%)                , ch(50),~
               at (18,16), fac(hex(84)), up$(5%)                , ch(50),~
               at (19,16), fac(hex(84)), up$(6%)                , ch(50),~
               at (20,16), fac(hex(84)), up$(7%)                , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   uinpmessage$         , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 1% then goto L07235
               brand_ok$ = "N"
               gosub startover
L07235:
               if keyhit% <> 16% then goto L07240
               brand_ok$ = "N"
               goto L07275
L07240:
               gosub read_UPC_confirm
               uprevcode$ = scn_upc$
               if brand_ok$ = "Y" then goto L07275
               
               init(" ") errormsg$                  
               err% = 47%                  
               gosub err_scrn
               errormsg$ = err$(47%) 
/* Testing only & " Should be SKU " & sav_sku$ & " UPC " & str(xr_upc$,5%,16%)*/ 
               goto L07230            

L07275:        init(" ") errormsg$ 
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

        return
        set_upc_screen

          init(" ") dateout$, scn_upc$
          uinpmessage$ = "Scan SKU UPC barcode   "
          call "TIME" (dateout$)
                                                    
          scrn_upc_title$ = "Staging SKU Confirmation by UPC"
          fldx$(1%)      = "UPC Barcode To Scan :"
          fldx$(2%)      = "Prv:"

          pf$(1%) = "(1)Startover                            " &       ~
                     "                                       " 
          pf$(2%) = "                                        "
          pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Screen "
          pfkeys$ = hex(01ffffffffffffffffffffffffffff1000)

          lfac$(1%) = hex(81) : lfac$(2%) = hex(81)
                                                      
        return

REM **************************************************************
REM *  CR1356 - Check sku from DT to match UPC and scanned UPC   *
REM **************************************************************        
        read_UPC_confirm
            
             init(" ") xr_key$ 
             brand_ok$ = "N"             
             str(xr_key$,1%,4%) = "X_LO"
             str(xr_key$,5%,12%) = sav_sku$
             read #19, key = xr_key$, using L07290, xr_key$, xr_upc$,     ~
                                                   eod goto L07295
L07290:    FMT ch(16), ch(20)

             if str(xr_upc$,5%,len(xr_upc$)) = scn_upc$ then brand_ok$ = "Y"
             goto L07299
L07295:    
             init(" ") xr_key$  
             str(xr_key$,1%,4%) = "X_MB"
             str(xr_key$,5%,12%) = sav_sku$
             read #19, key = xr_key$, using L07290, xr_key$, xr_upc$,      ~
                                                   eod goto L07297   
             if str(xr_upc$,5%,len(xr_upc$)) = scn_upc$ then brand_ok$ = "Y"
             goto L07299
L07297:     
             init(" ") readkey$
             str(readkey$,1%,9%)   = "PATIOSKU "
             str(readkey$,10%,15%) = sav_sku$
             read #2,key = readkey$, using L07310, desc$, eod goto L07299
               str(xr_upc$,5%,16%) = str(desc$, 1%, 20%)
               if str(xr_upc$,5%,len(xr_upc$)) = scn_upc$ then brand_ok$ = "Y"      
L07299:
        return
   
REM **************************************************************
REM *  CR1356 - Response was NO by user, ask to change label     *
REM **************************************************************        
        display_msg 
           hdr$     = "**** Brand Label not Verified ****"
           msg$(1%) = "Please replace the brand label then "
           msg$(2%) = "restart by scanning the barcode  "
           msg$(3%) = "Press <ENTER> to Continue"
L07300:
           brand_ok$ = "N"   
           k% = 2%       
           call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
              if k%  =  0% then return
              goto L07300
        return            

REM **************************************************************
REM *  CR1356 - Check for SOS SKU, no SKU check needed if found*
REM **************************************************************        
        read_sos_sku 
          sosexits% = 0%
          init(" ") readkey$
          str(readkey$,1%,9%)   = "SOS SKU"
          str(readkey$,10%,15%) = sav_sku$
          read #2,key = readkey$, using L07310, desc$, eod goto L07320
L07310:       FMT POS(25), CH(30)
            sosexits% = 1%        
L07320:   return
        
REM *************** New Cross Docking Routine   ***************
        check_cross_dock
            cross_err% = 0%                      /* Cross Dock Situation */
            if dt_dept$ = "102" then return
REM DO NOT CROSS DOCK HOPPER OR GARDEN WINDOWS
            if str(dt_part$,1%,3%) = "998" and dt_dept$ = "101" then return
/* CR2532 */
            if str(dt_part$,1%,3%) = "995" and dt_dept$ = "101" then return
            
            if str(dt_part$,1%,3%) = "H01" and dt_dept$ = "101" then return

            if cross_dock% = 1% then goto check_cross_dock_1
                                            /* 1st Test for Cross Dock   */
            if str(cross_warranty$,1%,1%) <> "S" and cross_dock% = 0% then ~
                                                     cross_dock% = 3%
                                            /* Not a Cross Dock          */
            if cross_dock% > 2% then goto check_cross_dock_done
                                            /* (AWD051)                  */
                                            /* (AWD053)                  */
                                            /* 1st Barcode Label Scanned */
               init(" ") cross_dock_key1$, barcode$, wandchar$,           ~
                         cross_warranty2$, cross_dock_dept$
               cross_dock% = 1%                 /* Set Cross Dock Flag   */
               cross_dock_key1$  = cross_key$   /* SaveBarcodeOf1stLabel */
               cross_warranty2$  = cross_warranty$     /*Save1stLabelWarr*/
               cross_dock_dept$  = dt_dept$      /* Save 1st label Dept  */
                                                 /* (AWD053)             */
                                                 /* (AWD051)             */
               prevcode$         = str(cross_key$,1%,18%)

               goto check_cross_dock_done

        check_cross_dock_1
                                                 /* (AWD053)             */
                                                 /* Check for Stock      */
               if cross_dock_dept$ = "102" and str(barcode$,1%,1%) = "S" ~
                                              then goto check_cross_dock_2
               if cross_dock_dept$ = "101" and str(barcode$,1%,1%) = "S" ~
                                              then goto check_cross_dock_2

                                             /* (PAR003) Test 2nd Label  */
               if str(barcode$,1%,1%) = "D" then goto skip_CheckPainted
               if str(cross_dock_key1$,2%,9%) <> str(barcode$,2%,9%)     ~
                                              then goto CROSS_DOCK_ERR3
skip_CheckPainted:
                                                 /* (AWD053)             */
        check_cross_dock_2
                                                 /* (PAR003)             */
               gosub find_warranty               /* FindWarr Id 2nd Label*/
               if cross_err% = 1% then goto CROSS_DOCK_ERR1
               if cross_err% = 2% then goto CROSS_DOCK_ERR4
                                                 /* Correct (APCPLNDT)   */
                                                 /* Update1stLbl with the*/
                                                 /* Correct Warranty     */
               dt_key$ = cross_dock_key1$
               read #1,hold,key 0% = dt_key$, using L14520, dt_rec$,     ~
                                                  eod goto CROSS_DOCK_ERR2
                  delete #1
                                            /* Warranty 'From' 2nd Label */
               str(dt_rec$,96%,8%) = cross_warranty$
               write #1, using L14520, dt_rec$, eod goto CROSS_DOCK_ERR2
               read #1, key 0% = dt_key$, using L14520, dt_rec$,         ~
                                                   eod goto CROSS_DOCK_ERR2
               dt_load$ = str(dt_rec$,1%,5%)
               dt_dept$ = str(dt_rec$,42%,3%)
               dt_st$   = str(dt_rec$,64%,2%)
               barcode$ = str(dt_key$,1%,18%)
               cross_dock% = 3%                  /*Process Completed - Ok*/
        check_cross_dock_done
        return
  
CROSS_DOCK_ERR1:
        err% = 33%                               /*(APCPLNWT) Update Error*/
        cross_dock% = 2%                         /* Error Code            */
        gosub err_scrn
        return
CROSS_DOCK_ERR2:
        err% = 34%                               /* (APCPLNDT)Update_error*/
        cross_dock% = 2%                         /* Error Code            */
        gosub err_scrn
        return
CROSS_DOCK_ERR3:                                 /* (PAR003)              */
        err% = 35%                               /* Cross Dock Error      */
        cross_dock% = 2%                         /* Wrong Label Scanned   */
        gosub err_scrn
        return
CROSS_DOCK_ERR4:
        err% = 36%                               /* Part Number Validation*/
        cross_dock% = 2%                         /* Error Code            */
        gosub err_scrn
        return


 REM *************** UPDATE_CROSS_DOCK ****************************
        update_cross_dock                          /* (AWDPLNCD)          */
          init(" ") cross_rec1$                    /* Shouldn't Exist     */
                                                   /* 2nd ProductionBarcod*/
                                                   /* Scanned.            */
          read #9,hold,key 0% = cross_warranty2$, using L16000,           ~
                                          cross_rec1$, eod goto L16100

              delete #9
L16100:

              cross_rec1$ = cross_rec$        /* 45 Digit Part Number    */
                                              /* Info and New Info Fields*/
                                              /* Replace Warranty Id with*/
                                              /* the Warranty Id from the*/
                                              /* 1st Label Scanned       */
              str(cross_rec1$,1%,8%)  = cross_warranty2$
                                              /* Replace the Production  */
                                              /* Barcode with the Product*/
                                              /* Barcode from the 2nd Lbl*/
                                              /* Scanned                 */
              str(cross_rec1$,9%,18%) = warranty_key$
                                              /* New Cross Dock Record   */
                                              /* Lnk AWDPLNCD to APCPLNWT*/
                                              /* Warranty ID from the    */
                                              /* 2nd Label Scanned       */
              str(cross_rec1$,92%,8%)   = cross_warranty$  /*In APCPLNWT */
                                              /* Production Barcode from */
                                              /* the 1st label Scanned   */
              str(cross_rec1$,100%,18%) = cross_dock_key1$  /*In APCPLNWT*/
              write #9, using L16000, cross_rec1$, eod goto L16110

         return
L16110:
          errormsg$ = "(Error)- Updating (AWDPLNCD) - " & cross_warranty2$
          gosub error_prompt
        return                                     /* (AWD051)           */
                                                             /*(CR456) + */
REM *************************  CARTCHECK Logic **********************
        formatCart
          cartNum% = 0%
          convert cartNum$ to cartNum%, data goto cartErr

          convert cartNum% to cartNum$, pic(00000000)
        return
cartErr:
         err% = 38%                   /* Barcode Already Exist on Cart */
         str(err$(38%),29%,4%) = str(errCart$,5%,4%)            /* CR890 */
         str(rf_err$(38%),14%,4%) = str(errCart$,5%,4%)         /* CR890 */
         gosub err_scrn
        return

        validateDestination
          if cartDest$ = " " then goto DestErr

          if cartDest$ = "195" then return
          if cartDest$ = "300" then return
          if cartDest$ = "MCK" then cartsaveDest$ = "MCK"  /* CR2960 */
          if cartDest$ = "MCK" then return    /* CR2960 */

DestErr:
          err% = 39%                            /* Destination Error     */
          gosub err_scrn
        return

        cartCheck
          cart% = 0%
          init(" ") cartKey$, cartBar$
          cartKey$ = cartNum$
          gosub cartRecCheck
          if cart% = 1% then gosub CartDel
        return

        cartRecCheck
          cart% = 0%     /* CR1207 removed hold here for read only */
          read #15, key > cartKey$, using CRTFMT, cartKey$,         ~
                                                         eod goto noRecCart
CRTFMT:             FMT POS(07), CH(35)

          if str(cartKey$,1%,8%) <> cartNum$ then goto noRecCart
            cartBar$ = str(cartKey$,18%,18%)
            cart% = 1%
          modulename$ = "cartRecCheck"
          gosub update_audit2
        noRecCart
          modulename$ = "noRecCart"
          gosub update_audit2
        return


        cartDel
          init(" ") error$
          errormsg$    = "Cart Already Exists! Press PF<10> to Continue, ~
                          ~any key to delete."
          rf_errormsg$ = "Cart Exists!!    "
          if rf_scan% = 0%  then gosub error_cart_display
          if rf_scan% <> 0% then gosub rf_cart_err_scrn

          modulename$ = "cartDel"
          gosub update_audit2

          if comp% = 10% then cartNoDel

          gosub cartDel2

        return
        cartNoDel
          comp% = 0%
          init(" ") errormsg$, rf_errormsg$
          call "ALLFREE"
        return

/*SR78815 + */
        cartDel2
          init(" ") error$
          cartKey$ = cartNum$
        cartDel2Nxt
          read #15, hold, key > cartKey$, using CRTFMT, cartKey$,         ~
                                                        eod goto noCartDel2

           if str(cartKey$,1%,8%) <> cartNum$ then goto noCartDel2

              delete #15


           if cartPrint% = 1% then goto cartDel2Nxt

           gosub updateProdStatus
           goto cartDel2Nxt
        noCartDel2
          cart% = 0%
          init(" ") errormsg$, rf_errormsg$
          init(" ") cur_truck$                  /* CR1207 set to no truck */
          gosub close_cart                      /* CR1207 close cart tracking */
          call "ALLFREE"
        return
/*SR78815 - */

        cartUpdate
           init(" ") cartKey2$
           cartKey2$ = barcode$
           gosub cartBarcode
             if cartbar% = 1% then goto cartBarErr
           init(" ") cartKey$
           cartseq% = cartseq% + 1%
           convert cartseq% to cartSeq$, pic(00000000)  /* (CR780) */

           str(cartKey$,1%,8%)   = cartNum$
           str(cartKey$,9%,1%)   = cartSt$
           str(cartKey$,10%,8%)  = cartSeq$          /* (CR780) */
           str(cartKey$,18%,18%) = barcode$
           gosub writeCart
        return
cartBarErr:
           err% = 37%                   /* Barcode Already Exist on Cart */
           str(err$(37%),32%,4%) = str(errCart$,5%,4%)
           str(rf_err$(37%),15%,4%) = str(errCart$,5%,4%)    /* CR890 */
           gosub err_scrn
        return

        cartBarcode
           init(" ") errCart$
           cartbar% = 0%
           read #15, key 2% = cartKey2$, using CARTFMT, errCart$,         ~
                                                      eod goto noCartBar
CARTFMT:           FMT POS(07), CH(08)

             cartbar% = 1%
        noCartBar
        return

        writeCart
          read #15, hold, key = cartKey$, eod goto writeCart1

             delete #15

        writeCart1
           init(" ") cartTime$
           call "TIME" (cartTime$)

           put #15, using CARTFMT1, date,          /* System Date      */~
                                   cartNum$,        /* Cart Number      */~
                                   cartSt$,         /* Cart Status      */~
                                   cartSeq$,        /* Cart Seq (CR780) */~
                                   barcode$,        /* DT Barcode       */~
                                   cartDest$,       /* Cart Destination */~
                                   cartLd$,         /* Cart Load Number */~
                                   scr_id$,         /* Screen User ID   */~
                                   userid$,         /* Unix User ID     */~
                                   cartTime$       /* Cart Scan Time   */


CARTFMT1:   FMT CH(06), CH(08), CH(01), CH(08), CH(18), CH(10), CH(05), ~
                CH(03), CH(03), CH(08)


           write #15

        return

REM        updateCartStatus
REM          init(" ") cartKey$, cartRec$
REM          cartKey$ = cartNum$
REM          cartSt$="I"          /* Make Cart Status variable Inactive */
REM          gosub cartRecUpdate
REM          cartSt$="A"          /* Set Status variable back to Active */
REM        return

REM     cartRecUpdate
          cart% = 0%
        cartRecUpdNxt
          read #15, hold, key > cartKey$, using CRTUPDFMT, cartRec$,     ~
                                               eod goto noCartRecUpd
CRTUPDFMT:      FMT CH(256)

          cartKey$ = str(cartRec$,7%,35%)
          if str(cartKey$,1%,8%) <> cartNum$ then goto noCartRecUpd
          str(cartRec$,15%,1%) = cartSt$

          delete #15

          put #15, using CRTUPDFMT, cartRec$

          write #15

          goto cartRecUpdNxt
        noCartRecUpd
        return


        updateProdStatus
           init(" ") cartDept$, adCrSt$
           str(dt_key$,1%,18%) = cartBar$

           read #1, hold, key > dt_key$, using L14200 , dt_rec$,           ~
                                                    eod goto noupdDTSt 

             dt_key$ = str(dt_rec$,24%,23%)
             if str(dt_key$,1%,18%) <> cartBar$ then goto noupdDTSt
             cartDept$ = str(dt_key$,19%,3%)
             adCrSt$   = str(dt_rec$,64%,2%)

             if adCrSt$ <> "26" then goto updateProdStatus

             modulename$ = "updateProdStatus"
             gosub update_audit2

             gosub findADSt
             gosub updDTPrdSt
             goto updateProdStatus
        noupdDTSt
        return

        findADSt
         modulename$ = "findADSt"
         gosub update_audit2

         init(" ") ad_key$, cartAdSt$, adCrSt$, adCrDept$
         ad_key$ = all(hex(00))
         str(ad_key$,1%,18%) = cartBar$
        updateAdStNxt
          read #5,hold,key 1% > ad_key$, using L07430, ad_rec$,          ~
                                                   eod goto noCartAdStUpdt

            ad_bar$ = str(ad_rec$,1%,18%)
            ad_key$ = str(ad_rec$,1%,33%)
            if str(ad_key$,1%,18%) <> cartBar$ then goto noCartAdStUpdt
             adCrDept$ = str(ad_rec$,25%,3%)
               if adCrDept$ = "105" then goto deleteAdSt
               if adCrDept$ = "106" then goto deleteAdSt
               if adCrDept$ = "108" then goto deleteAdSt
               adCrSt$   = str(ad_rec$,32%,2%)
REM               IF ADCRST$ > "18" THEN GOTO DELETEADST

REM obtain highest status besides depart 105,106,108
             if adCrDept$ <> cartDept$ then goto updateAdStNxt
             if cartAdSt$ < adCrSt$ then cartAdSt$ = adCrSt$


             goto updateAdStNxt


deleteAdSt:
            delete #5

            goto updateAdStNxt
        noCartAdStUpdt
        return

        updDTPrdSt
           modulename$ = "updDTPrdSt"
           gosub update_audit2

           call "TIME" (dateout$)
           str(dt_key$,1%,18%) = cartBar$
           str(dt_key$,19%,3%) = cartDept$
           str(dt_key$,22%,2%) = scr_proc$

           read #1,hold,key = dt_key$, using L14200 , dt_rec$,            ~
                                                    eod goto noupdDTPrdSt
              delete #1

           str(dt_rec$,53%,6%)  = date          /* Product Scan Date     */
           str(dt_rec$,64%,2%)  = cartAdSt$     /* Product Complete-Wired*/
           str(dt_rec$,104%,2%) = scr_shft$     /* Screen Shift          */
           str(dt_rec$,116%,8%) = dateout$      /* Time Product Scanned  */

           put #1, using L14310 , dt_rec$

           write #1

        noupdDTPrdSt
        return

                                                             /*(CR456) - */

        check_trailer                                  /* (AWD043)       */
           init(" ") scr_load$, sav_drop$, scan_drop$
           trailer%  = 0%
           new_drop% = 0%
           init(" ") tr_key0$

           str(tr_key0$,1%,1%) = "0"             /* Look for Open Trailer*/
           str(tr_key0$,2%,8%) = barcode_trailer$   /*Trailer No Scanned */

           read #11, key 0% > tr_key0$, using L11800, tr_rec$, eod goto L11810
L11800:         FMT CH(128)

           if str(tr_rec$,1%,9%) <> str(tr_key0$,1%,9%) then goto L11810
             scr_load$ = str(tr_rec$,10%,5%)
             gosub check_load
               if code% = 0% then goto L11820    /*Invalid Load Number   */
             sav_drop$  = "XX"                   /* Init Save Drop No.   */
             scan_drop$ = "YY"                   /* Init Current ScanDrop*/
             trailer%  = 1%                      /* Valid Trailer & Load */
             current_load$ = scr_load$           /* SR69112              */

        return
L11810:     errormsg$ = "(Error)-Invalid Trailer Number?"
L11820:     if rf_scan% = 1% then goto L11830
            gosub error_prompt
            init(" ") scr_load$, sav_drop$
        return
L11830:     rf_errormsg$ = "Invalid Trailer Num?"
            gosub'500(fieldnr%)
            init(" ") scr_load$, sav_drop$
        return                                         /* (AWD043)       */
                                                       /* (AWD028)       */
        check_appian_production
           err% = 0%
           check% = 0%
           if str(barcode_shp$,1%,18%) <> barcode$ then goto LAPP_2
              check% = 1%
        return
        
REM ************** check appian label; called from loading
        check_shipping_appian                             /* (AWD043)    */
           err%   = 0%
           check% = 0%
           sav_drop% = 0%                                 /* Drop Changed*/
           init(" ") awd_app_key0$
           awd_app_key0$ = barcode_shp$
           read #12, key 0% = awd_app_key0$, using LAPP_FMT, howship$,  ~
                   eod goto LAPP_1
LAPP_FMT:    FMT POS(488), CH(2)              /* add howship CR1791 */

             check% = 1%
             scan_drop$   = str(barcode_shp$,19%,2%) /* From Scanned Brcd*/
                                                     /* (AWD044          */
              if by_ups% = 1% then scan_drop$ = "99" /* Treat as '99'    */

              if scan_drop$ = "99" then return       /* Skip '99' Drops  */
                                                     /* Init Drop Sort   */
              if sav_drop$ = "XX" then gosub count_drops
                                                     /* Save for 1st Drop*/
              if sav_drop$ = "XX" then sav_drop$ = scan_drop$
                                                    /* Verify CurrentDrop*/
              if sav_drop$ = scan_drop$ then sav_drop% = 1%
                                                      /* (AWD043)        */
        return
LAPP_1:     check% = 0%
            err% = 24%
            gosub err_scrn
            gosub error_display
        return
LAPP_2:     check% = 0%
            err% = 25%
            gosub err_scrn
            gosub error_display
        return
                                                         /* (AWD033)     */
REM ************** update shipping label with date and time loaded
        update_shipping_appian
           init(" ") awd_app_key0$, errormsg$, rec$(), calc_time$,        ~
                     app_scan_tme$, rf_errormsg$

           calc_time$ = time                      /* Military - HHMMSSXX */
           app_scan_tme$ = str(calc_time$,1%,4%)
           awd_app_key0$ = barcode_shp$
           read #12,hold,key 0% = awd_app_key0$, using LAPP_3, rec$(),    ~
                                                            eod goto LAPP_4
LAPP_3:       FMT 4*CH(256)                        /* (AWD054)           */

           delete #12

           str(rec$(),522%,6%) = date              /* Scan Date          */
           str(rec$(),528%,4%) = app_scan_tme$     /* Scan Time-Military */
           put #12, using LAPP_3, rec$()
           write #12, eod goto LAPP_4

           gosub ok_scrn
        return
LAPP_4:    err% = 27%
           gosub err_scrn
        return

REM ************** update shipping label with date and time staged
        update_staging_appian
           init(" ") awd_app_key0$, errormsg$, calc_time$,                ~
                     app_scan_tme$, rf_errormsg$

           calc_time$ = time                      /* Military - HHMMSSXX */
           app_scan_tme$ = str(calc_time$,1%,4%)
           awd_app_key0$ = str(rec$(),1%,20%)
           init(" ") rec$()
           read #12,hold,key 0% = awd_app_key0$, using LAPP_3, rec$(),    ~
                                                            eod goto LAPP_6
              delete #12

           str(rec$(),579%,6%) = date              /* Scan Date          */
           str(rec$(),585%,4%) = app_scan_tme$     /* Scan Time-Military */
           put #12, using LAPP_3, rec$()
           write #12, eod goto LAPP_6
        return
LAPP_6:    err% = 27%
           gosub err_scrn
        return

        update_audit                               /* Mod for Staging    */
          if upd_st$ = " " then goto corruptWrite
          if scr_id$ = " " then goto corruptWrite

          init(" ") ad_rec$, ad_time$, ad_key$, ad_rec1$
          call "TIME" (ad_time$)
          str(ad_rec$,1%,18%) = barcode$                    /* Barcode   */
          str(ad_rec$,19%,6%) = date                        /* Scan Date */
          str(ad_rec$,25%,3%) = ad_dept$                    /* Department*/
          str(ad_rec$,28%,2%) = ad_proc$                    /* Process   */
          str(ad_rec$,30%,2%) = scr_shft$                   /* Shift Code*/
          str(ad_rec$,32%,2%) = upd_st$                     /* Status    */
                                                            /* (AWD033)  */
          if override% = 1% and modulename$ = "UPDATE_TRACK_OVERRID"    ~
             then str(ad_rec$,32%,2%) = "12"
                                                            /* (AWD033)  */
          str(ad_rec$,34%,18%)= barcode$                    /* Barcode   */
          str(ad_rec$,52%,8%) = ad_time$                    /* Time Stamp*/
          str(ad_rec$,60%,3%) = scr_id$                     /* User Id   */
                                                            /* (AWD033)  */
                                                            /* Staging ID*/
                                                            /* (AWD033)  */
          str(ad_rec$,63%,2%) = "  "                        /* Filler    */

          ad_key$ = str(ad_rec$,19%,33%)
          read #5,hold,key = ad_key$, using L07430,ad_rec1$,eod goto L07420

             delete #5

L07420:     put #5, using L07430 , ad_rec$
L07430:       FMT CH(64)
            write #5, eod goto L07460


            gosub update_audit2
        return
L07460:   err% = 7%
          gosub err_scrn
          override% = override% + 1%                        /* (AWD033)  */
        return

        update_audit2
          if barcode$ = " " then return
          if ad_dept$ = " " then return

          init(" ") log_rec$, ad_time$, ad_key$, ad_rec1$
          call "TIME" (ad_time$)
          str(log_rec$,1%,18%) = barcode$                  /* Barcode   */
          str(log_rec$,19%,6%) = date                      /* Scan Date */
          str(log_rec$,25%,3%) = ad_dept$                  /* Department*/
          str(log_rec$,28%,2%) = ad_proc$                  /* Process   */
          str(log_rec$,30%,2%) = scr_shft$                 /* Shift Code*/
          str(log_rec$,32%,2%) = upd_st$                   /* Status    */
          str(log_rec$,194%,2%) = upd_st$                  /* CR1123 Tracking */
           
          if override% = 1% and modulename$ = "UPDATE_TRACK_OVERRID"    ~
             then str(log_rec$,32%,2%) = "12"
          str(log_rec$,196%,7%) = " AFTER "                /* CR1123 Tracking */
          str(log_rec$,203%,2%) = str(log_rec$,32%,2%)

          str(log_rec$,34%,18%)= barcode$                  /* Barcode   */
          str(log_rec$,52%,8%) = ad_time$                  /* Time Stamp*/
          str(log_rec$,60%,3%) = scr_id$                   /* User Id   */
          str(log_rec$,63%,3%) = userid$
          convert keyhit% to str(log_rec$,66%,2%), pic(00)
          str(log_rec$,68%,8%) = cartNum$
          str(log_rec$,76%,20%) = modulename$
          str(log_rec$,96%,1%) = scr_sel$
          str(log_rec$,97%,2%) = scr_shft$
          str(log_rec$,99%,3%) = scr_dept$
          str(log_rec$,102%,40%) = scrn_title$
          convert override% to str(log_rec$,142%,2%), pic(00)
          convert comp% to str(log_rec$,148%,2%), pic(00)
          convert cartPrint% to str(log_rec$,150%,2%), pic(00)


          str(log_rec$,156%,3%) = cartDept$
          str(log_rec$,159%,35%) = cartKey$


          ad_key$ = str(ad_rec$,19%,33%)
          read #16,hold,key = ad_key$, using LOG_FMT,log_rec$,       ~
                                           eod goto noLogRec
             str(log_rec$,205%,8%) = " DltAdd "              /* CR1123 */
             delete #16

noLogRec:     put #16, using LOG_FMT , log_rec$

LOG_FMT:       FMT CH(256)
            write #16, eod goto LogWriteErr

        return
LogWriteErr:
REM ERR% = 7%
REM GOSUB ERR_SCRN
REM OVERRIDE% = OVERRIDE% + 1%     
        return
                                             /* Almost same as update Trk*/
                                             /* Update product complete  */
                                              /* by Staging              */
        update_track_override                 /*     (APCPLNDT) - File   */
           init(" ") errormsg$, prevcode$, dateout$, rf_errormsg$
           errormsg$ = " "                                   /* <AWD056> */
           call "TIME" (dateout$)
           dt_key$ = override_key$
           read #1,hold,key = dt_key$, using L14200 , dt_rec$,            ~
                                                            eod goto L14250
L14200:        FMT CH(256)

              delete #1

L14250:    str(dt_rec$,53%,6%)  = date          /* Product Scan Date     */
           str(dt_rec$,64%,2%)  = "12"          /* Product Complete-Wired*/
           str(dt_rec$,104%,2%) = scr_shft$     /*                       */
           str(dt_rec$,116%,8%) = dateout$      /* Time Product Scanned  */
           get str(dt_rec$,133%,8), using L14300 , dt_sale    /* PRICE   */
L14300:       FMT PD(14,4)

           put #1, using L14310 , dt_rec$
L14310:         FMT ch(256)
           write #1, eod goto L14450
           prevcode$ = dt_bar$
           dt_brand$ = str(dt_rec$,243%,2%)        /* CR1356 */
           if dt_sale <> 0.0 then goto L14350      /* Count Charged Items*/
           if str(dt_rec$,215%,1%) = "Y" then goto L14400   /* Skip Parts*/
           if str(dt_rec$,214%,1%) <> "0" then goto L14400  /* Skip Sashs*/
L14350:
        REM    TT_UNIT% = TT_UNIT% + 1%            /* CALC SCANNED UNITS */
                                                   /* (AWD033)           */
L14400:    ad_dept$ = str(dt_rec$,42%,3%)          /* Scanning Department*/
           ad_proc$ = str(dt_rec$,45%,2%)          /* Scanning Process   */
           modulename$ = "UPDATE_TRACK_OVERRID"
           gosub update_audit                      /* (Also Changed)     */
           call "AWDCOMSB" (dt_rec$,#42,scr_dept$,scr_id$)   /* (AWD059) */
                                                            /* (SR70699) */
           if updatedCrossDock% = 0% then gosub ok_scrn
        return
L14450:    err% = 6%
           str(err$(6%),41%,3%)  = str(dt_key$,19%,3%)
           gosub err_scrn
           str(errormsg$,41%,3%) = str(dt_rec$,19%,3%)
           override% = override% + 1%
        return

        check_drop_override
           check% = 1%
           if scan_drop$ = "99" then return         /* Skip Check        */
                                                    /* (AWD047)          */
                                                    /* Shouldn't happen  */
                                                    /* for Status '15'   */
           if curr_drop% = 0% then goto OVER_1      /* Load Complete     */
                                                    /* Not an Override   */
           if by_pass% = 1% then return             /* Skip Test         */

REM   convert new_drop% to rhh$,pic(###)
REM   call "SHOSTAT" ("New = " & rhh$ & "  " & str(dd_ord$(new_drop%),1%,2%) &~
                     "  Scan= " & scan_drop$ )
REM   stop
                                                    /* Check for Current */
                                                    /* Drop Override     */
           if new_drop% <> 0% and                                     ~
              str(dd_ord$(new_drop%),1%,2%)  = scan_drop$ then return

           if str(dd_ord$(curr_drop%),1%,2%) = scan_drop$ then return

              gosub count_drops
              if curr_drop% = 0% then goto OVER_1     /* Load Complete   */
              if str(dd_ord$(curr_drop%),1%,2%) <> scan_drop$ then     ~
                                                           goto OVER_2

              sav_drop$ = scan_drop$         /* Switch to next open drop */
              check% = 1%
           return
OVER_1:    check% = 0%
           err% = 30%
           gosub err_scrn
           gosub error_display
        return
OVER_2:    check% = 0%                               /* Not Correct Drop */
           err% = 31%                                /* Not in Drop Seq  */
OVER_2A:
           gosub err_scrn
   /*SR74525  GOSUB ERROR_DISPLAY_OVERRIDE                               */

           if rf_scan% <> 1% then gosub error_display_override  /*SR74525*/
                                                          /* (AWD046)    */
                                            /* Change from 25% to 11%    */
           if comp% <> 11% then goto OVER_3             /* Override KEY  */
              check% = 1%                               /* Allow Override*/
              sav_drop$ = str(dd_ord$(curr_drop%),1%,2%)
OVER_3:
                                                          /* (AWD046)    */
                                             /* Change from 30% to 17%   */
           if comp% <> 17% then return
                                                     /* All the change   */
                                                     /* of the Current   */
              gosub find_new_drop                    /* drop Seq.        */
                                              /* Test Override Condition */
                                              /* cannot go backward once */
                                              /* drop has been closed    */
              if save_new_drop% = 0% then goto OVER_4
                 if new_drop% > save_new_drop% then goto OVER_4
                    new_drop% = save_new_drop%
                    check% = 0%
                    err% = 32%
                    goto OVER_2A
OVER_4:
              if new_drop% <> 0% then                     ~
                               sav_drop$ = str(dd_ord$(new_drop%),1%,2%)
              if new_drop% <> 0% then check% = 1%
        return

        count_drops                                   /* (AWDAPPLS)      */
           init(" ") awd_app_key2$, drop$, dd$(), dd_ord$()
           ll% = 0%
           str(awd_app_key2$,1%,5%) = scr_load$            /* Set Load   */
           read #12,key 2% > awd_app_key2$, using L14500,              ~
                                    awd_app_key2$, eod goto count_drops_done

L14500:       FMT POS(23), CH(32)
           goto L14510
        count_drops_next
           read #12, using L14500, awd_app_key2$, eod goto count_drops_done

L14510:
           if str(awd_app_key2$,1%,5%) <> scr_load$ then goto count_drops_done
              drop$ = str(awd_app_key2$,6%,2%)
                                                       /* Skip Drop '99' */
              if drop$ = "99" then goto count_drops_next
              if ll% <> 0% then goto L14515
                 goto L14518
L14515:
              if dd$(ll%) = drop$ then goto count_drops_next
L14518:          ll% = ll% + 1%
                 dd$(ll%) = drop$
           goto count_drops_next
        count_drops_done
           drop_max%  = ll%                            /* Last drop      */
           curr_drop% = 0%                             /* Sub Script of  */
                                                       /* Current Open   */
                                                       /* Drop by Priority*/
           xx%       = drop_max% + 1%

           for i% = 1% to drop_max%
               str(dd_ord$(i%),1%,2%) = dd$(xx% - i%)
               str(dd_ord$(i%),3%,1%) = "O"            /* Drop Open      */
               test_drop$ = str(dd_ord$(i%),1%,2%)
               gosub check_drop
                                                       /* Special - Test */
               if i% < new_drop% then drop_open% = 0%
                                                       /*Force Close Drop*/
                                                       /* Drop Closed    */
                                                       /* All loaded     */
               if drop_open% = 0% then str(dd_ord$(i%),3%,1%) = "C"
               if curr_drop% = 0% and drop_open% = 1% then curr_drop% = i%
                                                       /* Special - Test */
               if curr_drop% < new_drop% then curr_drop% = new_drop%
                                                       /*ChangeCurr_drop%*/
           next i%
                                                   /* if Drop_open% = 0% */
                                                   /*Then all drop loaded*/
        return

        check_drop                                          /* (APCPLNDT)*/
           init(" ") dt_key3$
           drop_open% = 0%                    /* Drop Closed all Loaded  */
           str(dt_key3$,1%,5%) = scr_load$
           read #1,key 3% > dt_key3$, using L14520, dt_rec$,              ~
                                                   eod goto check_drop_done

L14520:       FMT CH(256)
           goto L14530
        check_drop_next
           read #1, using L14520, dt_rec$, eod goto check_drop_done

L14530:    dt_key3$ = str(dt_rec$,1%,23%)
           if str(dt_key3$,1%,5%) <> scr_load$ then goto check_drop_done
              if str(dt_key3$,11%,2%) <> test_drop$ then goto check_drop_next
                 if str(dt_rec$,64%,2%) = "14" then drop_open% = 1%
                                                     /* (AWD047)         */
/* (AWD055)    do not count status 15 as open on status 14  */
REM              IF STR(DT_REC$,64%,2%) = "15" THEN DROP_OPEN% = 1%
                                                     /* (AWD047)         */

                 if drop_open% = 1% then goto check_drop_done
           goto check_drop_next
        check_drop_done       /* One Item in Staging then Drop Still Open*/
        return

        find_new_drop
          save_new_drop% = 0%
          save_new_drop% = new_drop%
          for i% = 1% to drop_max%
              if str(dd_ord$(i%),1%,2%) = scan_drop$ and                  ~
                 str(dd_ord$(i%),3%,1%) = "O" then new_drop% = i%
          next i%
       REM    CONVERT NEW_DROP% TO RHH$, PIC(###)
       REM    CALL "SHOSTAT" ("NEW= " & RHH$ & "  " & STR(DD_ORD$(NEW_DROP%),1%,2%) )
       REM    STOP
                               /* The scan_drop$ must be an open drop    */
        return                                           /* (AWD043)     */


                                                             /* (AWD061) */
        corruptWrite
           errormsg$ = "(ERROR) Can't Update APCPLNDT no Status/User!"
           gosub error_display
           goto exit_program


        find_warranty                              /* (APCPLNWT)         */
                                                   /* (AWD051)-(AWDPLNCD)*/
           init (" ") warranty_key$, cross_warranty$
                                                   /* (PAR004)2ND Scanned*/
           warranty_key$ = barcode$                /*  Production Barcode*/
           read #8,key 2% = warranty_key$, using L16000, cross_rec$,      ~
                                                           eod goto L16020
L16000:       FMT CH(128)
                                              /* Save Warranty ID Found  */
              cross_warranty$ = str(cross_rec$,1%,8%)
                          /* (PAR004) Verify the MFG Part and Sub Part   */
              so_inv$  = str(cross_dock_key1$,1%,8%)    /* Sales Order   */
              item_no$ = str(cross_dock_key1$,9%,2%)    /* Line Item No. */
              gosub lookup_sub_part
                                                   /* Verify Part Number */
              if cross_part$ <> str(cross_rec$,27%,25%) then goto L16025

                                                   /* (AWD053)           */
                                              /* Check for Stock an Skip */
              if cross_dock_dept$ = "102" and str(barcode$,1%,1%) = "S"    ~
                                              then goto L16010

                                              /* Verify sub Part         */
              if cross_sub_part$ <> str(cross_rec$,52%,20%) then goto L16025
L16010:
                                              /* Correct MFG Part        */
                                           /* Read Warranty again locked */
              read #8,hold,key 0% = cross_warranty$, using L16000,        ~
                                              cross_rec$, eod goto L16020

                 delete #8
        /* Replace the Production Barcode with 1st label Scanned's Barcode*/
              str(cross_rec$,9%,18%) = str(cross_dock_key1$,1%,18%)
              write #8, using L16000, cross_rec$, eod goto L16020
                                                   /* (AWD051)           */
                           /* Now Update New Warranty database (AWDPLNCD)*/
              gosub update_cross_dock
                                                   /* (AWD051)           */
        return
L16020:    cross_err%  = 1%                        /* Warranty Error     */
        return
L16025:    cross_err%  = 2%                   /* Part Number Validation  */
        return

        wood_remake                                /*  <AWD058>          */
          init (" ") barcode$, wandchar$, xx$()
          xx$(3) = "Scan Barcode to update status to 11 for Production Remake"
          tt_unit% = 0%
          wood_rmk% = 1%

          gosub'152(fieldnr%)
          errormsg$ = " "

          if keyhit% <> 16% then goto wood_remake_next
             wood_rmk% = 0%
          return
        wood_remake_next
           upd_st$ = "11"
           gosub check_shipping
REM IF CHECK% = 0% THEN GOTO PROD_SCAN_2
             if check% = 0% then goto wood_remake
           upd_st$ = "11"
           gosub update_shipping
           goto wood_remake
        return                                     /*  </AWD058>         */

        lookup_barcode
           init(" ") dt_key$, sav_dept$
           str(dt_key$,1%,18%) = barcode$         /* Set Barcode Value   */
        check_barcode_nxt
           read #1,key > dt_key$, using L05220 ,dt_load$, hld_drop$,     ~
                       dt_key$, dt_st$, ~
                                                       eod goto bar_done

              if str(dt_key$,19%,3%) = "001" then goto check_barcode_nxt
              if scr_sel$ = "4" then goto bar_done
              if dt_st$ < "14" and dt_st$ <> "26" then goto barcode_error
              sav_dept$ = str(dt_key$,19%,3%)              /* CR2713 */

        bar_done
        return
        barcode_error
           err% = 29%
           gosub err_scrn
           gosub error_display                               /* (SR74525)*/
        return                                               /* (AWD045) */

        lookup_sub_part
           init(" ") bcksubpt_rec$, flds$(), info_flds$(), cross_part$,   ~
                     cross_sub_part$, cross_sub_info$
           flag$ = "0"                       /* Sales Order Info         */
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

           call "AWDBKSUB"   (flag$,      /* Flag 0=SalesOrder 1=Invoice*/~
                              pgm$,       /* Calling Program 0=BCKUPDTE */~
                                          /* 1=Any Other 2=Delete       */~
                                          /* 3=Invoice                  */~
                              so_inv$,    /* SO or Invoice Num to lookup*/~
                              item_no$,   /* Item Number                */~
                              bcksubpt_rec$, /* Record If BCKUPDTE then */~
                                          /* pass in else pass out      */~
                              flds$(),    /* Part Number Fields         */~
                              info_flds$(), /* Information Fields       */~
                              #63,        /* BCKSUBPT File              */~
                              err1%)      /* Error Code                 */

            if err1% <> 0% then                             ~
                    str(bcksubpt_rec$,48%,20%) = "00000               "

              cross_part$     = str(bcksubpt_rec$,23%,25%)

              cross_sub_part$ = str(bcksubpt_rec$,48%,20%)

              cross_sub_info$ = str(bcksubpt_rec$,132%,20%)
              if err1% = 0% then return

       return


/*SR69112 +**Created new subroutine to calculate number of units scanned**~
           **loaded. Also calculate number of units scanned staged but  **~
           **not loaded.All the above for the currect drop only.       ***/

        calc_dropped

        /*         APCPLD40_CALL_FLG% = 1%       ACTIVATE LATER IF NEEDED*/
           yy% = 1%                        /* For Specified Shift        */
           convert scr_shft$ to yy%, data goto N09570
N09570:
  /*         CONVERT CURRENT_DROP$ TO CUR_DROP%, DATA GOTO N09571        */
           convert scan_drop$ to cur_drop%, data goto N09571
N09571:
  /*         CUR_DROP% = CUR_DROP% - 1                                   */
           convert cur_drop% to current_drop$, pic(00)

           tt_unit% = 0%                   /* Clear Current Units        */
           sc_dte$ = date                  /* Todays Date                */
           gosub adjust_date
           sc_load$ = current_load$        /* Current Load               */
           ed_drop$ = current_drop$        /* Current Drop               */
           p_screen% = 0%                  /* No Screen Display          */
           p_scan% = 1%                    /* Scanned Products Only      */
           p_flg% = 1% : p_max% = 0%       /* Load Products Array        */
/*pwww*/   call "APCPLD40" ( scr_dept$,   /* Specified Department Code  */~
                             sc_dte$,     /* Specified Production Date  */~
                                          /* for a Production Day-Begin */~
                             scr_shft$,   /* Shift Code or (AL) = All   */~
                             sc_load$,    /* Current Production Load    */~
                             ed_drop$,    /* Current Drop               */~
                             barcode_trailer$, /* Trailer Number        */~
                             p_mod$(),    /* Department Products        */~
                             p_unt%(),    /* Product Units              */~
                             p_unts%(),   /* Sample Units Only          */~
                             p_untss%(),  /* Charge Sashs Only          */~
                             p_untpp%(),  /* Charge Parts Only          */~
                             p_val(),     /* Product Dollar Value       */~
                             p_mrp(),     /* (5) Costing Buckets        */~
                             p_max%,      /* Max Number of Products     */~
                             p_flg%,      /* 0% = Load, 1% = No Load    */~
                             p_scan%,     /* 0%=Not Scanned, 1%=Scanned */~
                             p_screen%,   /* 0% = Yes, 1% = No - Display*/~
                             #4,          /* (APCPLNDP) Master Dept.    */~
                             #1,          /* (APCPLNDT) Prod. Tracking  */~
                             #5,          /* (APCPLNAD) Prod. Tracking  */~
                             #2 )         /* (GENCDSIN) Master Code Tab */

                        REM        FOR KK% = 1% TO P_MAX%
                        REM            TT_UNIT% = TT_UNIT% + P_UNT%(KK%,YY%)
                        REM        NEXT KK%
                        REM        IF ERR% <> 0% THEN GOTO N09930
                        REM        TT_UNIT2% = P_UNT%(1%,1%)
                        REM        TT_UNIT% = P_UNT%(1%,2%)
        return
/* N09930: */     err% = 17% : gosub err_scrn
        return
                                                             /*SR69112 - */

        adjust_date
            err% = 0%
            if yy% <> 3% then return            /* Only Adjust 3rd Shift */
               j1% = 0%
               hr% = 0%
               j2% = 0%
               leap% = 0%
               x% = 0%
               init(" ") ad_time$, jdate$, ap$
               call "TIME" (ad_time$)
               convert str(ad_time$,1%,2%) to hr%, data goto L10060
L10060:
               ap$ = str(ad_time$,7%,2%)        /* Store AM or PM        */
               if ap$ = "PM" then return        /* Today is Curr Prod DTE*/
               if hr% > 6% then err% = 17%      /* Shift is not Valid    */
               if err% <> 0% then return        /* Set Error Flag        */

               call "DATE" addr("GJ", sc_dte$, jdate$, x%)   /*Get Julian*/
               call "DATJULCV" (jdate$)             /* jdate$ = YYYYDDD  */
               convert str(jdate$,1%,4%) to j1%, data goto L10130 /*Year */
L10130:
               convert str(jdate$,5%,3%) to j2%, data goto L10150 /* Day */
L10150:
               j2% = j2% - 1%                   /* Subtract 1 Day        */
                                                /* Check Previous Year   */
               if mod(j1%-1%,4) = 0 then leap% = 1%   /* For Leap Year   */
               if j2% > 0% then goto L10240
                  j2% = 365%                    /* Set Date for Prev Year*/
                  if leap% = 1% then j2% = 366%
                  convert (j1% - 1%) to str(jdate$,1%,4%), pic(0000)

L10240:        convert j2% to str(jdate$,5%,3%), pic(000)
               CALL "DATJULCV" (jdate$)    /* Packed in the saddle again */
               call "DATE" addr("JG",jdate$, sc_dte$, x%)
                                           /* Adjusted Production Date   */
        return



        utility_scan
REM           RPT% = 0%                                     /* (AWD038)  */
REM           CALL "APCPLB40" (RPT%)                        /* (AWD038)  */
                  run$ = "APCPLN40"                         /* (AWD038)  */
                  gosub Run_Program                         /* (AWD038)  */
        return

        calc_scanned

           apcpld40_call_flg% = 0%                             /*SR69112 */
           yy% = 1%                        /* For Specified Shift        */
           convert scr_shft$ to yy%, data goto L09570
L09570:
           tt_unit% = 0%                   /* Clear Current Units        */
           sc_dte$ = date                  /* Todays Date                */
           gosub adjust_date
           sc_load$ = "ALL  "              /* Default All Loads          */
           ed_load$ = "ALL  "              /* Default All Loads          */
           p_screen% = 1%                  /* No Screen Display          */
           p_scan% = 1%                    /* Scanned Products Only      */
           p_flg% = 0% : p_max% = 0%       /* Load Products Array        */

           call "APCPLC40" ( scr_dept$,   /* Specified Department Code  */~
                             sc_dte$,     /* Specified Production Date  */~
                                          /* for a Production Day-Begin */~
                             scr_shft$,   /* Shift Code or (AL) = All   */~
                             sc_load$,    /* Production Load or (ALL)   */~
                             ed_load$,    /* Ending Load Number         */~
                             p_mod$(),    /* Department Products        */~
                             p_unt%(),    /* Product Units              */~
                             p_unts%(),   /* Sample Units Only          */~
                             p_untss%(),  /* Charge Sashs Only          */~
                             p_untpp%(),  /* Charge Parts Only          */~
                             p_val(),     /* Product Dollar Value       */~
                             p_mrp(),     /* (5) Costing Buckets        */~
                             p_max%,      /* Max Number of Products     */~
                             p_flg%,      /* 0% = Load, 1% = No Load    */~
                             p_scan%,     /* 0%=Not Scanned, 1%=Scanned */~
                             p_screen%,   /* 0% = Yes, 1% = No - Display*/~
                             #4,          /* (APCPLNDP) Master Dept.    */~
                             #1,          /* (APCPLNDT) Prod. Tracking  */~
                             #5,          /* (APCPLNAD) Prod. Tracking  */~
                             #2 )         /* (GENCDSIN) Master Code Tab */

           for kk% = 1% to p_max%
               tt_unit% = tt_unit% + p_unt%(kk%,yy%)
           next kk%
           if err% <> 0% then goto L09930
           tt_unit% = p_unt%(1%,1%)                                 /*pww*/
        return
L09930:     err% = 17% : gosub err_scrn
        return



       REM ---------------L A B E L    P R I N T I N G --------------

        print_label                                        /* (AWD033)   */
           been_here% = 0%
                                                           /* (AWD034)   */
           switch%    = 1%                                 /* Staging    */
           if scr_sel$ = "2" and schema% = 1% then switch% = 10%   /* CR1567 */
/* CR2493 using same printer letters as staging */
           if scr_sel$ = "4" and schema% = 1% then switch% = 10%   
                                                           /* (AWD032)   */
           if scr_prt$ = "A" then switch% = 1%             /* MFGSTAG    */
           if scr_prt$ = "B" then switch% = 2%             /* MFGSHIP    */
                                                           /* (AWD039)   */
           if scr_prt$ = "T" then switch% = 3%             /* MFGTEST    */
                                                           /* (AWD049)   */
           if scr_prt$ = "C" then switch% = 4%             /* MFGRGA     */
           if scr_prt$ = "D" then switch% = 5%             /* MFGUPS     */
                                                           /* (PAR004)   */
           if scr_prt$ = "E" then switch% = 7%             /* AWD060     */
           if scr_prt$ = "F" then switch% = 8%             /* AWD064     */
           if scr_prt$ = "G" then switch% = 9%             /* SR70180    */
           if scr_prt$ = "Z" then switch% = 6%             /* MFGNEA     */
           if scr_prt$ = "H" then switch% = 10%            /* MFGDCZA    */
           if scr_prt$ = "I" then switch% = 11%            /* MFGDCZB    */
           
           if scr_prt$ = "J" then switch% = 12%            /* MFGDCZC    */
           
/* CR1014 */
           if scr_prt$ = "K" and schema% = 2% then switch% = 13% /* NESTGK   */
           if scr_prt$ = "L" and schema% = 2% then switch% = 14% /* NESTGL   */
           if scr_prt$ = "M" and schema% = 2% then switch% = 15% /* NESTGM   */
           if scr_prt$ = "N" and schema% = 2% then switch% = 16% /* NESTGN   */
           if scr_prt$ = "P" and schema% = 2% then switch% = 17% /* NESTGP   */

/* CR1810 */
           if scr_prt$ = "Q" and schema% = 2% then switch% = 18% /* NESTGQ   */
           if scr_prt$ = "R" and schema% = 2% then switch% = 19% /* NESTGR   */
           if scr_prt$ = "S" and schema% = 2% then switch% = 20% /* NESTGS   */
           if scr_prt$ = "U" and schema% = 2% then switch% = 21% /* NESTGU   */
/* CR2130 */
           if scr_prt$ = "V" and schema% = 1% then switch% = 22% /* MFG300V  */
     
           awd_app_key0$ = all(hex(00))
           str(awd_app_key0$,1%,18%) = barcode$            /* Barcode    */

           read #12,key 0% > awd_app_key0$, using LAPP_3, rec$(),        ~
                                           eod goto L14100
             
           if barcode$ <> str(rec$(),1%,18%) then goto L14100
           if dt_dept$ <> " " then sav_dept$ = dt_dept$    /* CR2713     */   
                                                           /* (AWD037)   */
                                                           /* (AWD040)   */
                                                           /* (PAR000)   */
                                                           /* (PAR001)   */
           err% = 0%
/***  CR2713  ***/
           if str(barcode$,1%,1%) = "D" and str(userid$,1%,2%) = "PP" then ~
                goto L14150            
REM                    and ld_ship_block$ = "3CC" then ~
 
         
/* CR1918 add #20 parameter */   
           call "AWDPLA05" (switch%, been_here%, rec$(), #2, #14,#63,#20, err%)
               if err% <> 0% then gosub print_error
                                                           /* Finished   */
           call "AWDPLA05" (switch%, been_here%, rec$(), #2, #14,#63,#20, 99%)
               if err% <> 0% then gosub print_error
                                                           /* (PAR001)   */
                                                           /* (PAR000)   */
                                                           /* (AWD037)   */
                                                           /* (AWD040)   */
           gosub update_staging_appian   
                                                           /* (AWD031)   */
        return
L14100:    err% = 28% : gosub err_scrn
           gosub error_display                             /* (AWD031)   */
        return

/* CR2713 */
L14150:
           call "EWDRFPRT" (barcode$, sav_dept$, #2, #14, err%)  
           init (" ") sav_dept$
           if err% <> 0% then gosub print_error 
           gosub update_staging_appian   
        return
        
        
        print_error
           if rf_scan% = 1% then goto print_e2             /* (PAR002)   */
           hdr$     = "***** Label Printing Error *****"
           msg$(1%) = "ERROR OCCURRED WHILE PRINTING LABEL !!!"
           msg$(2%) = "Return Code (AWDPLA05) = "
           msg$(3%) = "Press <ENTER> to Continue, <PF16> to Exit"

           convert err% to str(msg$(2%),26%,2%), pic(#0)
print_e1:
           k% = 2%
           call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
              if k%  =  0% then return
              if k% <> 16% then goto print_e1
        return clear all
        goto initialize
print_e2:                                                   /* (PAR002)  */
           convert err% to err$, pic(#0)
           rf_errormsg$ = "Label Error = (" & err$ & ")"
           gosub'500(fieldnr%)
           return clear all
           goto initialize
                                                           /* (AWD030)   */
        reprint_label                                      /*  (AWD045)  */
              err% = 0%
REM              keyhit% = 3%                                   /* <AWD056> */
              if scr_sel% <> 4% then keyhit% = 3%          /* CR2493 */
              init(" ") barcode$, wandchar$, xx$(), errormsg$, rf_errormsg$
              if rf_scan% = 1%  then gosub'151(fieldnr%)
              if rf_scan% <> 1% then gosub'150(fieldnr%)
              if keyhit% = 2% then return
                                                            /* (SR72504) */
                                          /* Logic is reverse if reprint */
/* CR3194 */
              prefix_so$ = "1"
              if str(barcode$,2%,1%) = "9" then prefix_so$ = "0"
              if schema% = 2% and str(barcode$,1%,1%) = "A" then    ~
                                    str(barcode$,1%,1%) = prefix_so$

              if schema% = 1% and str(barcode$,1%,1%) = "B" then    ~
                                    str(barcode$,1%,1%) = prefix_so$
                                                           /* (\SR72504) */
                                                           
              if schema% = 1% and str(barcode$,1%,1%) = "D" and ~
                 scr_sel$ = "4"  then gosub reset_barcode_paint     /* CR2493 */
                  
              gosub lookup_barcode
              if errormsg$ <> " " or rf_errormsg$ <> " "             ~
                                           then goto reprint_label
              if keyhit% = 2% then return
                 gosub print_label
                 rprt_flg% = 1%                  /* CR1698 */
                 if scr_sel% = 4% then goto paintprtd  /* CR2493 */
                 goto reprint_label
paintprtd:
              tt_unit% = tt_unit% + 1% 
        return
        
/* CR2493 Reset the barcode to 0 for NC or A for TX */
        reset_barcode_paint

/* CR3194 */
        prefix_so$ = "1"
        if str(barcode$,2%,1%) = "9" then prefix_so$ = "0"
        str(barcode$,1%,1%) = prefix_so$
		
        init(" ") dt_key$
        str(dt_key$,1%,18%) = barcode$         /* Set Barcode Value   */
        reset_barcode_nxt
           read #1,key > dt_key$, using L14175 , dt_key$, eod goto not0_done
L14175:        FMT POS(24), CH(23)
              
              if str(dt_key$,1%,18%) <> barcode$  then goto not0_done              
              if str(dt_key$,19%,3%) = "001" then goto reset_barcode_nxt
           return
not0_done:
           str(barcode$,1%,1%) = "A"
          
           init(" ") dt_key$
           str(dt_key$,1%,18%) = barcode$         /* Set Barcode Value   */
        reset_Dbarcode_nxt
           read #1,key > dt_key$, using L14175 , dt_key$, eod goto no_bar_fnd

              if str(dt_key$,1%,18%) <> barcode$  then goto no_bar_fnd                    
              if str(dt_key$,19%,3%) = "001" then goto reset_Dbarcode_nxt    
no_bar_fnd:
        return
        
REM     *************************************************************~
        * Display This Screen If Barcode is Scanned And No          *~
        * Errors Occur.                                             *~
        *************************************************************
        ok_scrn
        if rf_scan%  = 1% then return
            init(" ") hdr$, errormsg$, rf_errormsg$
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

        err_scrn                      /* Display this Message for Errors */
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
            for io% = 1% to 3%                              /* (SR74525) */
              for i% = 1% to b_max%
                  print at(13,75);bell;
              next i%
              CALL "PAUSE" ADDR(50%)
            next io%
        return


        startover
          if rf_scan%  = 1% then goto rf_startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            errormsg$, rf_errormsg$ = " "
            edit% = 0%
        return clear all
        if rf_scan% <> 1% then goto main
rf_startover:
        if rf_scan%  = 1% then goto rf_main



        display_codes
           call "APCPLN1B" ( table%, #2)
        return

REM------------------------------------------------------------------------
REM       E N D    O F    R O U T I N E S                                 -
REM------------------------------------------------------------------------

REM------------------------------------------------------------------------
REM       E R R O R    R O U T I N E S                                    -
REM------------------------------------------------------------------------

        error_prompt
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
                                             /* (AWD045) - Can't display */
           if rf_scan% = 1% then return
           comp% = 2%
           hh$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press PF(10) Key, To Continue."
           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%))
           if comp% <> 10% then goto error_display
           brand% = 99%
        return

        rf_err_scrn
            rf_errormsg$ = rf_err$(err%)
            gosub'500(fieldnr%)
        return

        error_display_override
           comp% = 2%
           hh$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - O v e r r i d e   - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press PF(**) Key, To Override and Continue."
           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%))
                                                   /* Hidden = 11% & 17% */
                                                   /* (SR74525)          */
           if comp% <> 10% and comp% <> 11% and comp% <> 17% then         ~
                                 goto error_display_override
                                                             /* (AWD046) */
        return

        cartPrintMsg                                       /* (CR456) + */
           if rf_scan% = 1% then return
/* CR1166   */
/*           comp% = 2%   */
/*           hh$  = "********  (C A R T    P R I N T)  ********"   */
/*           msg$(1%) = " - - - - - - - - P r i n t - - - - - - - - " */
/*           msg$(2%) = "  Printed XXXX Shipping Labels  "  */
/*           convert labelprint% to str(msg$(2%),11%,4%), pic(0000) */
/*           msg$(3%) = "Press Any Key To Continue." */
/*           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%)) */
           comp% = 2%
           hh$  = "*****  (C A R T    S C A N N E D)  *****"
           msg$(1%) = " - - - - - - - - - - - - - - - - "
           msg$(2%) = "  Product Staged XXXX Shipping Labels  "
           convert labelprint% to str(msg$(2%),18%,4%), pic(0000)
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%))
/* CR1166 */
        return

        rf_cart_err_scrn
          comp% = 0%
          gosub'500(fieldnr%)
          if keyhit% = 4% then comp% = 10%
        return

        error_cart_display
           if rf_scan% = 1% then return
           comp% = 2%
           hh$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press PF(10) Key, To Continue."
           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%))
        return                                             /* (CR456) - */
REM--------------------------------------------------
REM Set the start date and time for the load - CR890
REM--------------------------------------------------
        set_load_start                             /* Update (AWDAPPLD)  */
            init(" ") ld_app_key0$, ld_app_rec$
            ld_app_key0$ = str(tr_rec$,10%,5%)
            read #17,hold,key 0% = ld_app_key0$, using L51000, ld_app_rec$,~
                                                      eod goto L51200
L51000:        FMT CH(128)  
            
            if str(ld_app_rec$,93%,4%) > " " then goto L51550 
              /* SR82415 clear hold on file */
            delete #17
            
            str(ld_app_rec$,87%,6%) = date 
            calc_time$ = time                      /* Military - HHMMSSXX */
            app_scan_tme$ = str(calc_time$,1%,4%)
            str(ld_app_rec$,93%,4%) = app_scan_tme$  

            write #17 using L51000, ld_app_rec$, eod goto L51500          

L51200:     return
        
L51500:     errormsg$ = "(Error) - Unable to set load start date and time"
            gosub error_prompt     
        return
        
L51550:  call "ALLFREE"
        return
        

REM------------------------------------------------------------------------
REM       E N D    O F    E R R O R    R O U T I N E S                    -
REM------------------------------------------------------------------------


REM------------------------------------------------------------------------
REM       E X I T    P R O G R A M                                        -
REM------------------------------------------------------------------------

        exit_program
            call "SHOSTAT" ("One Moment Please")
        end






















