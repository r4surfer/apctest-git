        REM *************************************************************~ 
            *                                                           *~  
            *  Program Name      - APCPLA42                             *~
            *  Creation Date     - 09/23/96                             *~
            *  Last Modified Date- 09/02/2011                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - This Program Creates Cut Sheets by   *~
            *                      Departments.                         *~
            *                                                           *~
            *  Code Tables Used  - (MODEL    ) - Model Codes            *~
            *                      (PLAN SHFT) - Shift Codes            *~
            *                      (PLAN DEPT) - Department Codes       *~
            *                      (PLAN PROC) - Planning Process Codes *~
            *                      (PLAN UNIT) - Planning Units Codes   *~
            *                      (PLAN DATE) - Production Date Codes  *~
            *                      (PLAN SASH) - Valid Sash Models      *~
            *                      (PLAN TUBE) - Valid Tube Wind Models *~
            *                      (APCCUTRPT) - Valid Equations for    *~
            *                                    Special Reports        *~
            *                      (APCCUTDPT) - Special Name for Dept  *~
            *                                                           *~
            *          Welders -   (APCWELD  ) - Wegoma/Sampson Welders *~
            *                      (APCWELDFR) - Frame Double Hung      *~
            *                      (APCWELDSH) - Sash Double Hung       *~
            *                      (APCWEG700) - Welder Vinyl Prime     *~
            *                      (APCSAMPSA) - Sampson Frames         *~
            *                      (APCSAMPSB) - Sampson Sash's         *~
            *                      (EWDSAMPSF) - Sampson Frames (EWD006)*~
            *                      (EWDSAMPSS) - Sampson Sash's (EWD006)*~
            *                      (APCSAMPSC) - Sampson Sash Dept=048  *~
            *                      (EWDWEGOMA) - Slider Model           *~
            *                                                           *~
            *  Special Comments  - All Tables Excep (MODEL) can have    *~
            *                      Alphanumeric values.                 *~
            *                                                           *~
            *                      Uses Table Subroutine (APCPLN1B)     *~
            *                      for Displaying Code table Values.    *~
            *-----------------------------------------------------------*~
            *  New Program   Old Program   <------ Departments -------> *~
            *-----------------------------------------------------------*~
            *  (LINEALMATE)        Updated (PAR000)                     *~
            *  (APCPLO42)                  008, Sash      @SAW05@       *~
            *  (APCPLP42)                  008, Frame     @SAW06@       *~
            *  (APCPLS42)                  047,           @SAW07@       *~
            *  (AWDPLF42)    (EWD033)      027 Frame/Sash @SAW027@      *~
            *  (AWDPLG42)    (EWD034)      007 Frame/Sash @SAW007@      *~
            *  (AWDPLU42)    (AWD051)      053 Fram/Sash  @SAW053@      *~
            *  (AWDPLI42)    (EWD036)      028 Frame/Sash @SAW028@      *~
            *  (AWDPLJ42)    (EWD038)      017 450 Brick Mold Frame/Sash*~
            *                                  @SAW017@                 *~
            *  (AWDPLK42)    (EWD038)      018 450 Cont Head Brick Mold *~
            *                                  Frame/Sash @SAW018@      *~
            *  (AWDPLL42)    (PAR001)      006 Frame OBSOLETE           *~  
            *                                   @SAW006@ NE             *~
            *  (AWDPLM42)    (PAR001)      006 Frame OBSOLETE NE        *~
            *  (AWDPLT42)    (AWD050)      006 Frame/Sash @SAW006@      *~
            *  (AWDPLV42)                  025 130 CHS    @SAW025@      *~
            *  (AWDPLO42)    (PAR002)      026 Frame/Sash @SAW026@      *~
            *                                                           *~
            *  (AWDPLB42)    (EWD028)      052 Frame/Sash @SAW052@      *~
            *  (AWDPLD42)    (EWD029)      049 Frame/Sash @SAW049@      *~
            *  (AWDPLA42)    (EWD028)      050 Frame/Sash @SAW050@      *~
            *  (AWDPLE42)    (EWD032)      051 Frame/Sash @SAW051@      *~
            *  (AWDPLC42)    (EWD030)      002 Frame/Sash @SAW002@      *~
            *  (APCPLR42)                  048 @SAW15@,@SAW16@,@SAW17@  *~
            *                                  @SAW18@,@SAW32@ Bilco    *~
            *                                                           *~
            *  (APCPLW42)    (EWD009)      036,S-@SAW09@,F-@SAW12@      *~
            *                                  H-@SAW13@                *~
            *  (APCPLC42)    (APCCUTSB)    Old Catch All  @SAWS@        *~
            *  (EWDPLG42)    (EWD???)      TSO/BSO   File @SAW28@       *~
            *                              (PAR001) Remove Dept=006     *~
            *  (APCPLB42)    (APCCUTBS)    033,048 (Sashs)              *~
            *                              @SASH@,@SASH1@,@SASH2@,      *~
            *                              @SASH3@                      *~
            *-----------------------------------------------------------*~
            *  (WELDMATE)          Updated (PAR000)                     *~
            *                              (PAR001) Remove Dept=006     *~
            *  (APCPLD42)    (APCCUTWL)    033 @WELDF1@ thru @WELDF4@   *~
            *                                  @WELDS1@ thru @WELDS4@   *~
            *  (APCPLV42)    (EWD007)      025, 026, 036 (Sash) Sampson *~
            *                                  @WELDE1@ thru @WELDE4@   *~
            *                                  @WELDR1@ thru @WELDR4@   *~
            *                                  @WELDS1@ thru @WELDS4@   *~
            *  (APCPLT42                   048 @SAMPD1@ thru @SAMPD8@   *~
            *-----------------------------------------------------------*~
            *  (Reports)           Updated (PAR000)                     *~
            *  (APCPLE42)                  002,018,022,042,043,050,051  *~
            *                              (print_rpt_3)                *~
            *                                                           *~
            *  (APCPLF42)                  (PAR001) 017,033,048,049,052 *~
            *                              Specials and -TUBE Winding   *~
            *                              (print_rpt_4)                *~
            *                                                           *~
            *                              (PAR001) 006= North East     *~
            *  (AWDPLN42)                  006                          *~
            *                              Specials and -TUBE Winding   *~
            *                              (print_rpt_4a)               *~
            *                                                           *~
            *  (APCPLG42)    (PAR002)      007,025,026,027,028,036,     *~
            *                              (print_rpt_5)                *~
            *                                                           *~
            *  (APCPLH42)                  008,009,047                  *~
            *                              (print_rpt_6)                *~
            *                                                           *~
            *  (AWDPLH42)    (EWD035)      New Special Shapes Bending   *~
            *                              Report. 043                  *~
            *                                                           *~
            *  (APCPLI42)                  Calc Tube for Specific Models*~
            *-----------------------------------------------------------*~
            *  (Removed Subroutines) Updated (PAR000)                   *~
            *-----------------------------------------------------------*~
            *  (APCPLZ42)                (EWDPLH42)                     *~
            *  (APCPLJ42)                (APCPLL42)                     *~
            *  (APCPLM42)                (APCPLK42)                     *~
            *  (EWDPLB42)                (EWDPLA42)                     *~
            *  (EWDPLD42)                (EWDPLE42)                     *~
            *  (EWDPLF42)                (APCPLX42)                     *~
            *  (APCPLQ42)                (APCPLU42)                     *~
            *  (EWDPLC42)                (APCPL042)                     *~
            *  (APCPL142)                (APCPLY42)                     *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/11/91 ! New Program for (APC) - LAST MOD DATE    ! RHH *~
            * 04/01/92 ! Modify Vinyl DH Sort (UPDATE_RPT_4)      ! RHH *~
            * 05/21/92 ! Modification for Adding Seq No. to Cut   ! RHH *~
            *          ! Sheets. Add Cot/Or Calc to Prod (6)      !     *~
            * 08/15/92 ! Modification for Passing Data to New     ! RHH *~
            *          ! Saw Optimization Software. Using Routines!     *~
            *          ! ( APCCUTLD - ?00 Series Load Cut Definiti!     *~
            *          ! ( APCCUTCC - ?00 Calculate Cuts for Wind !     *~
            *          ! ( APCCUTSB - Ex. APCCUTSB = ?00 Series   !     *~
            *          ! ( APCCUTXB - 300 Series and Special Prods!     *~
            *          ! ( APCCUT6B - 600 Series Cut Sheet        !     *~
            *          ! ( APCCUT7B - 700 Series Cut Sheet        !     *~
            *          ! ( APCCUT8B - 800 Series Cut Sheet        !     *~
            *          ! ( APCCUTBS - 600 Series Build Sash File  !     *~
            *          ! ( APCCUTTB - CALC TURNS AND WINDS        !     *~
            * 01/07/93 ! Modification Change the Sort of the Sash ! RHH *~
            *          ! File to Match the Daily Consolidation.   !     *~
            * 08/01/93 ! Mod To (APCCUTLD) and (APCCUTCC) to      ! RHH *~
            *          ! Support Costing of Misc. Parts and Grid/ !     *~
            *          ! Liting.                                  !     *~
            * 09/14/93 ! Mod To Check for Special Reporting       ! RHH *~
            *          !   Options.                               !     *~
            * 11/02/93 ! Mod To Seperate Double Hung and New      ! RHH *~
            *          !   Construction into Seperate Reports.    !     *~
            *          !   Cutting and Tube Winding               !     *~
            * 11/17/93 ! Mod To 'APCCUTTB' Routine because of     ! RHH *~
            *          !   Winding Chart Change                   !     *~
            * 01/20/94 ! Mod To Change 600 and 700 Series to by   ! RHH *~
            *          !   Load. Special Sort for 700 Series      !     *~
            *          !   SORT_PROD_7 -No Longer Valid 09/13/1999!     *~
            * 04/14/94 ! Mod FOR NEW CONSTRUCTION (A) AND VINYL   ! RHH *~
            *          !   SLIDER AND PICTURE (B) 'CHECK_SPECIAL' !     *~
            * 06/02/94 ! Mod to (APCCUTTB) for Tempered Glass and ! RHH *~
            *          !   for Cottage/Oriel windows              !     *~
            * 03/03/95 ! Mod to Skip Models that will be cut on   ! RHH *~
            *          !   the new Wegoma Saws. (CHECK_WEGOMA)    !     *~
            * 03/13/95 ! Mod New Version to Print reports for MFG ! RHH *~
            *          !   Departments.                           !     *~
            * 04/27/95 ! Mod Add Three (3) New Departments        ! RHH *~
            *          !                                          !     *~
            * 09/07/95 ! Mod to W_MOD$() Valid Twin/Tripl for     ! RHH *~
            *          !   WOOD/SURROUND Factory Mull             !     *~
            * 11/15/95 ! Mod Create Cut Sheets for Stock Product. ! RHH  ~
            * 01/02/96 ! Mod for New Department Definitions in    ! RHH *~
            *          ! (PLAN DEPT) Table and Verify against the !     *~
            *          ! new Scanning File (APCPLNTK).            !     *~
            * 09/16/96 ! Mod to Convert to the New Planning System! RHH *~
            * 06/12/97 ! Mods for the new Family of Windows. New  ! RHH *~
            *          !  Sub (APCPLJ42) For new Bilco saws. Also !     *~
            *          !  mod for dept's (049) and (052) for the  !     *~
            *          !  new sampson welders.                    !     *~
            * 06/16/97 ! Mods for the new Cut Sheet reports. New  ! RHH *~
            *          !  reports key off the equation data in    !     *~
            *          !  the report file (APCCUTRPT) This table  !     *~
            *          !  contains the applicable equations.      !     *~
            * 06/19/97 ! Mods for the new Sampson Welders. There  ! RHH *~
            *          !  is a different bridge file format for   !     *~
            *          !  Sampson. New Subroutine (APCPLK42)      !     *~
            * 06/24/97 ! Mods for the new Wegoma Welders. There   ! RHH *~
            *          !  is a different bridge file format, new  !     *~
            *          !  Wegoma saws. New Subroutine (APCPLL42)  !     *~
            * 09/09/97 ! Mods for the new Sampson Sash Saws for   ! RHH *~
            *          !  the new Family Products. New Subroutine !     *~
            *          !  (APCPLM42).                             !     *~
            * 09/10/97 ! Mods W_F$, W_1$, W_2$, with Fin, 1 Lock  ! RHH *~
            *          !  2 Lock Code Variables. Also add Dept    !     *~
            *          !  052 New Family to Tube Winding.         !     *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 01/21/98 ! Mods to Linealmate Subroutines to Assign ! RHH *~
            *          !  the Schedule number for batches. Uses   !     *~
            *          !  the table (PLANSCHED). Also turn on Two !     *~
            *          !  new Sub's (APCPLO42) and (APCPLP42)     !     *~
            *          !  for the Casement Department.            !     *~
            * 03/02/98 ! Mods for Two New Tables (PLAN SASH) and  ! RHH *~
            *          !  (PLAN TUBE) control the Models associated     *~
            *          !  with each of the processes. The Sash    !     *~
            *          !  machine and the Tube Winding Report.    !     *~
            * 06/02/98 ! (EWD001) Mod for new Wegoma Saws for Dept! RHH *~
            *          !  048 Sliders.                            !     *~
            * 09/11/98 ! (EWD002) Mod for new Linealmate for Dept ! RHH *~
            *          !  048 Sliders (APCPLR42)                  !     *~
            *          !  047 Picture Windows (APCPLS42)          !     *~
            * 09/12/98 ! (EWD003) Mod to enable special reports   ! RHH *~
            *          !  for all departments.                    !     *~
            * 10/05/98 ! Clean Up Files for Linealmate            ! RHH *~
            * 11/17/98 ! (EWD004) Mod for Dept 048-Sliders and the! RHH *~
            *          !  new Sampson Welder for Sash's.          !     *~
            *          !  Subroutine (APCPLT42)                   !     *~
            * 12/16/98 ! (EWD005) Mod to turn on reports for 7's  ! RHH *~
            *          !   Series.                                !     *~
            * 05/05/99 ! (EWD006) Mod to turn on Sampson Fram and ! RHH *~
            *          !   sash saws for Dept (2) New 211 Line    !     *~
            *          !   (APCPLU42)                             !     *~
            * 05/06/99 ! (EWD007) Mod to turn on Sampson Sash     ! RHH *~
            *          !   welder for Department (036) 712 Line   !     *~
            * 05/26/99 ! (EWD008) Mod to turn on Bilco Positioners! RHH *~
            *          !   for the new 211 Line                   !     *~
            * 06/21/99 ! (EWD009) Mod for Optimizing Wegoma Saws  ! RHH *~
            *          !   in 712 Line. (APCPLW42) Dept = 36      !     *~
            * 06/21/99 ! (EWD010) Mod for Optimizing Sampson Saws ! RHH *~
            *          !   in 211 Line. (APCPLX42) Dept =  2      !     *~
            * 09/13/99 ! (EWD011) Mod to Remove sort_prod_7       ! RHH *~
            *          !   and fix (APCPLG42) for Dept = 036%     !     *~
            * 09/23/99 ! (EWD012) Mod Switch the Dept 002 Lineal- ! RHH *~
            *          !   mate to be the same as Dept 049        !     *~
            *          !   (APCPLM42)                             !     *~
            * 11/09/99 ! (EWD013) Mod For Sash in (APCPLU42)      ! RHH *~
            * 01/06/00 ! (EWD014) Mods for new Dept's new version ! RHH *~
            *          !   of (APCPLR42) Obsolete (APCPLQ42)      !     *~
            * 01/20/00 ! (EWD015) Mods for Samp/Disp/Literature   ! RHH *~
            * 01/28/00 ! (EWDXXX) Mods to fix Double Stack Problem! RHH *~ 
            *          !   in (APLPLV42) subroutine.              !     *~
            * 04/24/00 ! (EWD015) Continued Mods for Samples new  ! RHH *~
            *          !   code '027' Parts range (12 thru 26)    !     *~
            * 04/25/00 ! (EWD016) Mods for Dept 007 - Bilco       ! RHH *~
            * 06/11/00 ! (EWD017) Mods for new Dept 007 - Joseph  ! RHH *~
            *          !   (APCPLZ42) New Sub                     !     *~
            * 08/01/00 ! (EWD018) Mods for Department 005 and New ! RHH *~
            *          !   Welders. use Sub (APCPL042)            !     *~
            *          !   Note- Sash Quad Stack, Frame Double Stack!   *~
            * 04/27/01 ! (EWD019) Mods for Department 002 and the ! RHH *~
            *          !   215 Line. New Sash Welder Single Stack !     *~
            *          !   (APCPL142)                             !     *~
            * 08/16/01 ! (EWD020) Continued Mods for Samples new  ! CMG *~
            *          !   code '036' Parts range (12 thru 35)    !     *~
            * 02/20/02 ! (EWD021) Modification for saw and weld   ! RHH *~
            *          !   files for the new 413 line.            !     *~
            * 09/17/02 ! (EWD022) Fix for Special Shapes Grid Code! CMG *~
            * 11/07/02 ! (EWD023) Modification for saw, weld, and ! CMG *~
            *          !   samp files for new 500 cont head/sill  !     *~
            * 04/01/03 ! (EWD024) Modification for new dept '025' ! CMG *~
            *          !          and '026'                       !     *~
            * 05/07/03 ! (EWD025) Mod to allow numeric or alpha   ! CMG *~
            *          !          load numbers.                   !     *~
            * 07/08/03 ! (EWD026) Mods for TSO/BOS Linealmate. Opt! CMG *~
            *          !          '2' saw files.                  !     *~
            * 10/27/03 ! (EWD027) Modification for new dept '027' ! CMG *~
            *          !          and '028'                       !     *~
            * 01/29/04 ! (EWD028) Mods for New Flex lines.        ! RHH *~
            *          !          Dept's 050 and 052              !     *~
            * 02/16/04 ! (EWD029) Mods for new Flex Lines.        ! RHH *~
            *          !          Dept 049. 413                   !     *~
            * 03/11/04 ! (EWD030) Mods for new Flex Lines.        ! RHH *~
            *          !          Dept 002. 211 Additional Mod for!     *~
            *          !          changing the machine code.      !     *~
            * 03/15/04 ! (EWD031) Mod to correct Locks problem    ! RHH *~
            *          !                                          !     *~
            * 04/16/04 ! (EWD032) Mod for new Flex line.          ! RHH *~
            *          !          Dept 051 with Product moved     !     *~
            *          !          from Dept 050. Still have       !     *~
            *          !          Dept 050.                       !     *~
            * 05/03/04 ! (EWD033) Mod for new Flex Line           ! RHH *~
            *          !          New flex format to resolve Sash !     *~
            *          !          Welder file format problems.    !     *~
            *          !          for Dept 27, Brickmould         !     *~
            * 06/21/04 ! (EWD034) Mod for new Flex Line           ! RHH *~
            *          !          convert the 215 Line to new     !     *~
            *          !          Weldmate and Linealmate format  !     *~
            *          !          Dept 007                        !     *~
            * 06/21/04 ! (EWD035) Mod to add the Special Shapes   ! RHH *~
            *          !          Bending report for Dept 043     ! RHH *~
            *          !          (AWDPLH2)                       !     *~
            * 01/17/05 ! (EWD036) Mod for new Flex Line           ! RHH *~
            *          !          convert department 028 to the   !     *~
            *          !          new weldmate and linealmate     !     *~
            *          !          format. Dept 028                !     *~
            * 02/03/05 ! (EWD037) Mod to Sub awdpli42 for dept.   ! RHH *~
            *          !          028 to set weld qty for triples !     *~
            *          !          to three,                       !     *~
            * 12/16/05 ! (EWD038) Mod for New Brick Mold Dept's   ! RHH *~
            *          !          (017) 450 Brick Mold and        !     *~
            *          !          (018) 450 Cont. Head Brick Mold !     *~
            * 01/15/06 ! (PAR000) CR347 Mod for New Part Number an! RHH *~
            *          !          cleanup old code. Also Add array!     *~
            *          !          for Subs with 1% = 1% Lock,     !     *~
            *          !          2% = 2 Locks, 3% = With Fin     !     *~
            * 04/17/06 ! (PAR001) Mod for new NE Dept 006. Code   ! RHH *~
            *          !          Linealmate. AWDPLL42, AWDPLM42. !     *~
            *          !          Cut Sheet Report and Tube       !     *~
            *          !          Winding. AWDPLN42               !     *~
            *          !          Also the new Sub Part Number and!     *~
            *          !          information Fields have been    !     *~
            *          !          added to (APCCUTWK) & (APCCUTW2)!     *~
            *          !         Used by Subs (AWDPLN42)(APCPLF42)!     *~
            * 05/10/06 ! (PAR002) Mods for new Department 026     ! RHH *~
            *          !          New Sub (AWDPLO42). Also Modify !     *~
            *          !          (APCPLG42) for Cut Sheet Report !     *~
            * 05/23/06 ! (PAR003) Mod for New Special report for  ! RHH *~
            *          !          Sash's. Option = 10             !     *~
            * 10/21/06 ! (EWD039) Change of format                ! DES *~
            * 05/05/08 ! (AWD040) mod for new report, slow moving ! CMG *~
            *          !           raw materials                  !     *~
            * 01/13/09 ! (AWD041) mod to sort by seq instead of   ! CMG *~
            *          !     sort in APCPLNDT                     !     *~
            *03/16/2009! (AWD042) mod for new florida depts - 004,! CMG *~
            *          !    014, 019, 020                         !     *~
            *10/01/2009! (AWD043) add customer to work file       ! CMG *~
            *04/21/2010! (AWD044) mod for dept 005 - 267C line    ! CMG *~
            *08/04/2010! (AWD045) mod for turn on sash only for   ! CMG *~
            *          !      027 and 028                         !     *~
            *09/02/2011! (AWD046) mod for new slider file         ! CMG *~
            *03/07/2013! (AWD047) mod for NTX cut sheet           ! CMG *~
            *08/05/2013! (AWD048) mod for shapes dimensions       ! CMG *~
            *06/03/2014! (AWD049) mod to add bar code to wk file  ! PWW *~
            *06/19/2014! (AWD050) mod for series 35 dept 006      ! MES *~
            *08/27/2014! (AWD051) mod for series 5700 dept 053    ! MES *~
            *10/15/2014! (AWD052) mod for NTX linealmate          ! MES *~
            *02/09/2015! (AWD053) mod for NTX Dept 015 & 016      ! MES *~
            *04/02/2015! (IM7733) mod for CHS 130 dept 025        ! MES *~
            *06/06/2015! (SR65694) mod for NTX DH line Dept 045   ! MES *~
            *10/14/2015! (SR65695) mod for NTX 120 line Dept 012  ! MES *~
            *03/20/2016! (SR73216) mod to add subpt to AWDPLH42.  ! PWW *~
            *04/15/2016! (SR73949) mod to add TX dept 057         ! MES *~ 
            *05/22/2018! (CR1482)  mod to add TX dept 031,041,070 ! MES *~
            *12/28/2018! (CR1722) mod to add TX dept 071          ! MES *~
            *05/03/2019! (CR2005)  turn of Dept 071 linealmate    ! MES *~
            *12/04/2019! (CR2355) mod to hsl based on height      ! MES *~
            *04/20/2020! (CR2503) mod to CHS locks width          ! MES *~
            *07/30/2021!  CR2803 new Sturtz saw dept 053          ! RDB *~
            *01/07/2022!  CR2985 new Sturtz saw dept 007          ! RDB *~
            *06/12/2023!  CR3333 Allow dept 002 TSO/BSO           ! RDB *~
            *************************************************************

        dim                              /* FILE = AMTBOMCD            */~
            model$3,                     /* Model Code                 */~
            size$4, mode$5,              /* Linealmate Batch Size Wind */~
            sched$3,                     /* Linealmate Starting Sched  */~
            mm$2,                        /* Product Line Code of Part  */~
            partno$25                    /* Part Number                */

        dim                              /* FILE = APCPLNDT            */~
            pd_dept$3,                   /* Tracking Department Code   */~
            sav_dept$3,                  /* Save Department    (EWD026)*/~
            save_part$25,                /* Part Number                */~
            dt_rec$256,                  /* Production Detail          */~
            dt_key1$57,                  /* Primary Alt Key            */~
            dt_load$5,                   /* Production Load Number     */~
            dt_part$25,                  /* Production Part Number     */~
            dt_txt$4,                    /* S.O. Line Item Text Id     */~
            dt_bar$18,                   /* Production Barcode         */~
            dt_ref$8,                    /* Warranty Number            */~
            dt_sort$5,                   /* Production Sort Code       */~
            dt_shft$2,                   /* Production Shift Code      */~
            dt_sash$1,                   /* Prod Sash Flag 0,1,2,3,4   */~
            dt_prt$1,                    /* Part (Y)es, or (N)o        */~
            dt_samp$1,                   /* 0=NA, 1=SAMP, 2=DISP       */~
            dt_seq$5                     /* Department Seq. Number     */

        dim                              /* FILE - (APCCUTWK)          */~
            wrk_key$5,                   /* PRIMARY KEY                */~
            wrk_key1$51,                 /* ALT KEY (1)                */~
            wrk_rec$200,                 /* WORK RECORD                */~
            wd$7,                        /* Actual Width               */~
            ht$6,                        /* Actual Height              */~
            sav_cl$1,                    /* COLOR                      */~
            sav_size$13,                 /* COLOR                      */~
            total$5                      /* TOTAL FOR TYPE             */


        dim                              /* FILE-(APCCUTW1) 600 Series */~
            w_f$25,                      /* Save With Fin Codes        */~
            w_1$25,                      /* Save (1) Lock Codes        */~
            w_2$25,                      /* Save (2) Lock Codes        */~
            lk_fn$(5%)30,                /* For Subs 1,2,Fin   (PAR000)*/~
            sort_key$30,                 /* Tube Winding Report Key    */~
            c_o$2, flg$1,                /* COTTAGE/ORIEL              */~
            t_t$4,                       /* TWIN/TRIPLE                */~
            t_b$3,                       /* TSO, BSO, FSO              */~
            locks$3,                     /* NUMBER OF LOCKS            */~
            fin$3                        /* WITH FIN YES, NO           */

        dim                              /* (Program Variables)        */~
            lit_flg$1,                   /* Literature Flag    (EWD014)*/~
            hdr$40, msg$(3%)79,          /* ASKUSER                    */~
            sqq$5,                       /* Sequence Number,HIGH TO LOW*/~
            spec$4,                      /* SCREEN - BSO,TSO,FGO       */~
            hnge$20,                     /* Description of Hinge       */~
            sze$30,                      /* Save Eights                */~
            sz$100,                      /* Save Sixteenths            */~
            wind$2,                      /* NUMBER OF WINDS            */~
/*AWD040*/  option$2, scr$(15%)60,       /* Select(1) thru (10)(PAR003)*/~
            opt_desc$30,                 /* Selection Description      */~
            wd1$8,                       /* CALC (1)                   */~
            wd2$8,                       /* CALC (2)                   */~
            wd3$9,                       /* CALC (3)                   */~
            wd4$8,                       /* CALC (4)                   */~
            wd5$8,                       /* CALC (5)                   */~
            wd6$8,                       /* CALC (6)                   */~
            wd7$8,                       /* CALC (7)                   */~
            wd8$8,                       /* CALC (8)                   */~
            wd9$8,                       /* CALC (9)                   */~
            wd10$8,                      /* CALC (10)                  */~
            wd11$8,                      /* CALC (11)                  */~
            gs$2,                        /* GLASS CODE                 */~
            readkey$30,                  /* GENCODES Look-Up Key       */~
            prod_dte$8,                  /* Screen Completion Date FORM*/~
            scr_dte$8, comp_dte$8,       /* Prod. and Comp. Dates      */~
            scr_dte1$8,                  /* Screen PROD. Date Unform   */~
            scr_dept$3, scr_shft$2,      /* Production Department      */~
            scr_prod$1,                  /* Screen Product Line or All */~
            scr_msg$30,                  /* Department Description     */~
            scr_msg1$30, scr_msg2$30,    /* Shift Description          */~
            scr_load$5, scr_desc$30,     /* Specified Load Number      */~
            ld_load$5, ld_desc$30,       /* New Load Data              */~
            scr_type$1,                  /* Use Linealmate (EWD014)    */~
            descr$30,                    /* Use for GENCODES Look-Up   */~
            fld$(11%)4,                  /* Save (11) Field Values     */~
            filename$8,                  /* Used by EWDOPEN            */~
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
            f1%(15%)                     /* = 1 if READ was successful */

                                         /* = 1 if file open, -1 if it */
                                         /*   doesn't exist, or 0 if   */
                                         /*   not yet checked (OPENCHCK*/
                                         /* Text from file opening     */

                                         /* (PAR001)                   */
        dim                                                              ~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            partstyle$10,                /*     style                  */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4,            /* Additional Info Fields     */~
            dt_sub_part$20,              /* New Sub Part No.           */~
            dt_sub_info$20               /* New Sub Info Fields (9)+11 */~
                                         /* (PAR001)                   */

        dim dt_cust$9                    /* Customer (AWD043)          */
        
        dim schema$8                     /* Schema   (AWD047)          */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21                   /* (PAR001)          */
                                                   /* (PAR003)          */
            apc$   = "Depart. Production Cut Sheets - 05/23/06"
            pname$ = "APCPLA42 - Rev: 01.05"

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
            * #1  ! APCCUTWK ! Work File                                *~
            * #2  ! APCPLNDT ! Production Master Detail File            *~
            * #3  ! GENCODES ! Master System Table File                 *~
            * #4  ! AWDSPECB ! Special Shapes Bending Database (EWD035) *~
            * #5  ! APCPLNLD ! Master LOAD INFO FILE                    *~
            * #6  ! AMTBOMIF ! Master EQUATION FILE                     *~
            * #7  ! HNYMASTR ! MASTER INVENTORY FILE            (PAR001)*~
            * #8  ! APCCUTW2 ! Work File For (600) SERIES               *~
            * #9  ! TEXTFILE ! MASTER TEXT FILE                         *~
            * #10 ! BCKLINES ! SALES ORDER LINE ITEMS                   *~
            * #11 ! APCCUTEQ ! SAW CROSS REF FILE                       *~
            * #12 ! AMTBOMCD ! MASTER EQUATION FILE                     *~
            * #63 ! BCKSUBPT ! New Sub Part Number File         (PAR001)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCCUTWK",                                      ~
                        varc,     indexed,  recsize =   256,             ~
                        keypos =    1, keylen =    5,                    ~
                        alt key  1, keypos =    6, keylen = 51, dup

            select #2,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   24, keylen =   23,                    ~
                        alt key  1, keypos =   47, keylen = 57,          ~
                            key  2, keypos =   53, keylen = 51,          ~
                            key  3, keypos =    1, keylen = 23, dup,     ~
                            key  4, keypos =   96, keylen =  8, dup

            select #3, "GENCODES",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24
                                                       /* (EWD035)     */
            select #4,  "AWDSPECB",                                      ~
                        varc,     indexed,  recsize =   256,             ~
                        keypos =    1, keylen =   36,                    ~
                        alt key  1, keypos =  140, keylen = 23
                                                       /* (EWD035)     */

            select #5,   "APCPLNLD",                                     ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   11, keylen =    5,                    ~
                        alt key  1, keypos =    3, keylen = 13,          ~
                            key  2, keypos =    1, keylen = 15


            select #6,  "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32                      ~


            select #7,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup
                                                   /* (PAR002) Chg Size */
            select #8,  "APCCUTW2",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   1, keylen =  25
                                                   /* (PAR002) Chg Size */
            select #9,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =   1, keylen =  11

            select #10, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #11, "APCCUTEQ",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    2, keylen =   7,                     ~
                        alt key  1, keypos  =     1, keylen =  8

            select #12, "AMTBOMCD",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42
                                                        /* (PAR001)     */
            select #63, "BCKSUBPT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =  12, keylen =  11, dup,     ~
                            key  2, keypos =  23, keylen =  45, dup

                                                       /* (PAR001)     */

            call "SHOSTAT" ("Opening Files, One Moment Please")
                                                       /* (EWD011)     */
            filename$ = "APCPLNDT" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
                                                      /* (EWD035)      */
            filename$ = "AWDSPECB" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
                                                      /* (EWD035)      */
            filename$ = "APCPLNLD" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AMTBOMIF" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "HNYMASTR" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "TXTFILE" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCCUTEQ" : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AMTBOMCD" : call "EWDOPEN" (#12, filename$, err%)
            if err% <> 0% then gosub open_error
                                                          /* (EWD011)  */
                                                          /* (PAR001)  */
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
            if err% <> 0% then gosub open_error
                                                          /* (PAR001)  */
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

        REM - NEAREST 8TH INCH
           sze$ = "1/81/43/81/25/83/47/8         "

        REM - NEAREST 16TH OF AN INCH
           sz$ = " 1/16 1/8  3/16 1/4  5/16 3/8  7/16 1/2  9/16 5/8 11/16~
        ~ 3/4 13/16 7/8 15/16     "

        REM   - OLD DEPARTMENT       NEW DEPARTMENT
        REM     --------------       --------------
        REM          1                    007
        REM          2                    006  North East
        REM          3                    028, 048
        REM          4                    033
        REM          5                    022
        REM          6                    008
        REM          7                    047
        REM          8                    009
        REM          9                    043
        REM          10                   036
        REM          11                   042
        REM          12                   046
        REM          13                   044
        REM          14                   000
        REM                               011 - UPS
        REM                               021 - SAMPLES

            scr$( 1%) = "******************************~
                        ~******************************"
            scr$( 2%) = "*(1) = Standard Cut Sheet Rpt ~
                        ~ (11) = Slow Moving Lineals  *"
            scr$( 3%) = "*(2) = Create Linealmate Data ~
                        ~                             *"
            scr$( 4%) = "*(3) = Create SashMate Data   ~
                        ~                             *"
            scr$( 5%) = "*(4) = Create Wegoma Welder   ~
                        ~                             *"
            scr$( 6%) = "*(5) = Glaze Bead Report      ~
                        ~                             *"
            scr$( 7%) = "*(6) = Re-Bar Report          ~
                        ~                             *"
            scr$( 8%) = "*(7) = Frame Snap In Report   ~
                        ~                             *"
            scr$( 9%) = "*(8) = Department Defined     ~
                        ~                             *"
            scr$(10%) = "*(9) = TSO/BSO LinealMate     ~
                        ~                             *"  /* (EWD026) */
            scr$(11%) = "*(10)= (New) Sash Report      ~
                        ~                             *"  /* (PAR003) */

                                       /* (EWD038) 12-14-05 Re-Checked */
                                       /* (PAR000) 01-15-2006          */
            init(" ") lk_fn$()
            w_f$ = "346MO"             /* Lock Codes that are With Fin */
            w_1$ = "13HM"              /* Lock Codes for (1) Lock      */
            w_2$ = "24JO"              /* Lock Codes for (2) Locks     */
            lk_fn$(1%) = w_1$          /* Codes for 1 Lock             */
            lk_fn$(2%) = w_2$          /* Codes for 2 Locks            */
            lk_fn$(3%) = w_f$          /* Codes for with Fin           */
                                       /* (Par000) 01-15-2006          */
                                       
/* (AWD047) */

            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #3, schema_err%)                                         


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            max_fld% = 6%
            for fieldnr% = 1% to  max_fld%
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
                  if keyhit%  = 16% then gosub begin_process
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > max_fld% then editpg1
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

        begin_process
            call "SHOSTAT" ("Creating "& scr_msg$)
            gosub create_report
            gosub delete_work
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
         "Enter the Production and Completion Date for Cut Sheet?      ",~
         "Enter a Valid Department Code Selection?                     ",~
         "Enter a Valid Shift Code Selection or (AA) = All?            ",~
         "Enter a Specific Load Number or Blank = N/A?                 ",~
         "Enter the Applicable Report Option, (1 thru 10)?             ",~
         "Enter the Batch Size and the Starting Schedule Number?       "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, scr_dte$, scr_dte1$,       ~
                      comp_dte$, scr_dept$, scr_msg$, scr_shft$,         ~
                      scr_msg1$, scr_load$, scr_desc$, option$, size$,   ~
                      sched$, opt_desc$, scr_msg2$        /* (PAR003)   */

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
        REM DATALOAD
        REM RETURN
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

                                         /* APCPLNDT - Load Detail Rec */
L35050:     FMT CH(256)                  /* Planning Detail Record     */

            FMT CH(01),                  /* TXN Type ( 1 thru 2 )      */~
                CH(03),                  /* Model Code                 */~
                CH(02),                  /* Primary Code               */~
                CH(02),                  /* Hinge Code                 */~
                CH(01),                  /* CLMR Applicable            */~
                CH(01),                  /* WALLWIDT Applicable        */~
                CH(04),                  /* Phantom Designator         */~
                CH(02)                   /* Filler Area                */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************
                                         /* (PAR003) New Option=10      */
        deffn'101(fieldnr%, edit%)
L40070:       gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40210,         /* Prod/Comp Dates   */   ~
                                L40220,         /* Production Dept   */   ~
                                L40210,         /* Production Shift  */   ~
                                L40210,         /* Specified Load    */   ~
                                L40220,         /* Report Options    */   ~
                                L40210          /* Batch Size,Sched  */

              goto L40240

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40210:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40220:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40240:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "MFG Production Date       :",                ~
               at (04,30), fac(lfac$(1%)), scr_dte$             , ch(08),~
               at (04,40), "Completion Date :",                          ~
               at (04,60), fac(lfac$(1%)), comp_dte$            , ch(08),~
               at (05,02), "Production Department Code:",                ~
               at (05,30), fac(lfac$(2%)), scr_dept$            , ch(03),~
               at (05,41), fac(hex(84)), scr_msg$               , ch(30),~
               at (06,02), "Production Shift Code     :",                ~
               at (06,30), fac(lfac$(3%)), scr_shft$            , ch(02),~
               at (06,41), fac(hex(84)), scr_msg1$              , ch(30),~
               at (07,02), "Specific Load (Only)      :",                ~
               at (07,30), fac(lfac$(4%)), scr_load$            , ch(05),~
               at (07,41), fac(hex(84)), scr_desc$              , ch(30),~
               at (08,02), "Cut Sheet Report Option   :",                ~
               at (08,30), fac(lfac$(5%)), option$              , ch(02),~
               at (08,41), fac(hex(84)), opt_desc$              , ch(30),~
               at (09,02), fac(hex(84)), scr_msg2$              , ch(27),~
               at (09,41), fac(lfac$(6%)), size$                , ch(04),~
               at (09,51), fac(lfac$(6%)), sched$               , ch(03),~
                                                                         ~
               at (10,12), fac(hex(84)), scr$(1%)               , ch(60),~
               at (11,12), fac(hex(84)), scr$(2%)               , ch(60),~
               at (12,12), fac(hex(84)), scr$(3%)               , ch(60),~
               at (13,12), fac(hex(84)), scr$(4%)               , ch(60),~
               at (14,12), fac(hex(84)), scr$(5%)               , ch(60),~
               at (15,12), fac(hex(84)), scr$(6%)               , ch(60),~
               at (16,12), fac(hex(84)), scr$(7%)               , ch(60),~
               at (17,12), fac(hex(84)), scr$(8%)               , ch(60),~
               at (18,12), fac(hex(84)), scr$(9%)               , ch(60),~
               at (19,12), fac(hex(84)), scr$(10%)              , ch(60),~
               at (20,12), fac(hex(84)), scr$(11%)              , ch(60),~
                                                                        ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 10% then goto L40730
                  gosub display_codes
                  goto L40070

L40730:        if keyhit% <> 15% then goto L40770
                  call "PRNTSCRN"
                  goto L40070

L40770:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40960     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &       ~
                      "                                       "
            pf$(2%) = "                 (10)Display Departments" &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffff0affffffff0f1000)
            if fieldnr% = 1% then L40920
                str(pf$(3%),64%)  = " "  :  str(pfkeys$,16%,1%) = hex(ff)
L40920:     if fieldnr% > 1% then L40940
                str(pf$(1%),18%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40940:     return

L40960: if fieldnr% > 0% then L41050  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                                       "
            pf$(2%) = "                 (10)Display Departments" &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Print Data  "
            pfkeys$ = hex(01ffffffffffffffff0affffffff0f1000)
            return
L41050:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
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
            on fieldnr% gosub L50160,         /* Prod. and Comp. Dates */ ~
                              L50400,         /* Prod. Dept Code       */ ~
                              L50580,         /* Production/Completion */ ~
                              L50720,         /* Load Number           */ ~
                              L50970,         /* 1-Rpt,2-Opt,3-SASH,4-W*/ ~
                              L51250          /* Batch Size, Start Schd*/
            return

L50160: REM Production Date                       SCR_DTE$, SCR_DTE1$
            date% = 0%
            call "DATEOK" (scr_dte$, date%, errormsg$ )
            if errormsg$ <> " " then goto L50360
               scr_dte1$ = scr_dte$
               call "DATUNFMT" (scr_dte1$)
               rem dt_key1$ = " "                 /* (APCPLNDT) - File    */
               dt_key1$ = all(hex(00))
               str(dt_key1$,1%,6%) = str(scr_dte1$,1%,6%)
               read #2,key 1% > dt_key1$, using L50260, dt_key1$,         ~
                                                        eod goto L50350
L50260:           FMT POS(47), CH(57)
               if str(dt_key1$,1%,6%) <> str(scr_dte1$,1%,6%) then       ~
                                                        goto L50350
               if comp_dte$ <> " " then goto L50310
                  comp_dte$ = scr_dte$
L50310:        date% = 0%
               call "DATEOK" (comp_dte$, date%, errormsg$ )
               if errormsg$ <> " " then goto L50360
        return
L50350:       errormsg$ = "No Data on File for Specified Production Date~
                          ~ this is a test ."
L50360:       init(" ") scr_dte1$, scr_dte$, comp_dte$
              gosub error_prompt
        return

L50400: REM Production Department Code            SCR_DEPT$, SCR_MSG$
            scr_dept% = 0%
            if scr_dept$ <> " " then goto L50440
               goto L50530
L50440:     convert scr_dept$ to scr_dept%, data goto L50480

            convert scr_dept% to scr_dept$,pic(000)

L50480:     pd_dept$ = scr_dept$
            gosub lookup_dept
            if code% = 0% then goto L50530
               dept% = scr_dept%
        return
L50530:     errormsg$ = "(Error) Invlaid Department Code Selection?"
            init(" ") scr_dept$, scr_msg$, pd_dept$
            gosub error_prompt
        return

L50580: REM Production Shift Code                 SCR_SHFT$, SCR_MSG1$
           if scr_shft$ <> " " then goto L50630
L50600:       scr_shft$ = "AA"                    /* EQUALS PRODUCTION */
              scr_msg1$ = "(AA) = All Shifts"
              return
L50630:    if str(scr_shft$,1%,1%) = "A" then goto L50600
              gosub lookup_shift
              if code% = 0% then goto L50670
        return
L50670:     errormsg$ = "(Error) - Invalid Shift Code Selection?"
            init(" ") scr_shft$, scr_msg1$
            gosub error_prompt
        return

L50720: REM Load Number
            if scr_load$ <> " " then goto L50770
L50740:        scr_load$ = "N/A  "
               scr_desc$ = "N/A = Not Applicable"
               return
L50770:     if str(scr_load$,1%,1%) = "N" then goto L50740
            scr_load% = 0%
                                                    /*  (EWD025)  */
REM            if pos("AS" = str(scr_load$,1%,1%)) > 0 then goto L50840
               convert scr_load$ to scr_load%, data goto L50840

               convert scr_load% to scr_load$, pic(00000)
               goto L50870
L50840:     convert str(scr_load$,2%,4%) to scr_load%, data goto L50920

            convert scr_load% to str(scr_load$,2%,4%), pic(0000)
L50870:     ld_load$ = scr_load$            /* Remove GOSUB STOCK_LOAD */
            gosub lookup_load
            if apc% = 0% then goto L50920
            scr_desc$ = ld_desc$
        return
L50920:     errormsg$ = "(Error) - Invalid Load Number Selection?"
            gosub error_prompt
            init(" ") scr_load$, scr_desc$, ld_load$, ld_desc$
        return

L50970: REM Report Options                            /* (PAR003)      */
            if option$ <> " " then goto L51010        /* 1 Thru 10     */
               option$ = "1"

L51010:     convert option$ to option%, data goto L51160

                                                  /* (EWD026)          */
                                                  /* (PAR003)          */
            if option% < 1% or option% > 11% then goto L51160
                                                  /* (EWD003) Special  */
                                                  /* (AWD040) */
                                                  /* Reports all Depts */

            if option% < 11% then opt_desc$ = str(scr$(option% + 1%),2%,28%)
            if option% > 10% then opt_desc$ = ~
                       str(scr$((option% + 1%) - 10%),32%,28%)

            if option% = 2% then scr_msg2$="Batch Size, Schedule Start:" ~
                            else init(" ") size$, sched$, scr_msg2$
            if option% = 2% then return
                                                  /* (EWD026)           */
            if option% = 9% then scr_msg2$="Batch Size, Schedule Start:" ~
                            else init(" ") size$, sched$, scr_msg2$
            if option% = 9% then return
               if edit% = 1% then  fieldnr% = 6%
        return
L51160:     errormsg$ = "(Error)-Invalid Report Selection, (1 thru 11)?"
            init(" ") option$, opt_desc$, size$, sched$
            gosub error_prompt
        return

L51250: REM Linealmate Batch/Sched            /* Size and Start Values */
                                              /*  (EWD026)             */
            if option$ <> "2" and option$ <> "9" then return
            size% = 500%
            gosub lookup_schedule
            convert size$ to size%, data goto L51300
L51300:
            convert size% to size$, pic(0000)

            convert sched$ to sched%, data goto L51390

            convert sched% to sched$, pic(000)

            if sched% < 100% then goto L51390
        return
L51390:     errormsg$ = "(Error) Invalid Batch or Schedule Size?"
            init(" ") size$, sched$
            gosub error_prompt
        return

        open_error                                    /* (EWD011)        */
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return
                                                      /* (EWD011)        */
        error_prompt
           comp% = 2%
           hdr$ = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        display_codes
            table% = 1%
            call "APCPLN1B" (table%, #3)
        return

        lookup_dept
            code% = 0%
            if pd_dept$ = "066" then goto L51660           /* (EWD015) */

            init(" ") readkey$, scr_msg$
            str(readkey$,1%,9%)   = "PLAN DEPT"
            str(readkey$,10%,15%) = pd_dept$
            read #3,key = readkey$, using L51640, scr_msg$, eod goto L51660
L51640:        FMT POS(25), CH(30)
            code% = 1%
L51660: return

        lookup_tso_bso                                    /*  (EWD026)   */
            code% = 0%

            init(" ") readkey$, scr_prod$, descr$
            str(readkey$,1%,9%)   = "PLNTSOBSO"
            str(readkey$,10%,15%) = str(dt_rec$,42%,3%)   /* Department  */
            read #3,key = readkey$, using L51640, descr$, eod goto L51670

            scr_prod$ = str(descr$,1%,1%)
            sav_dept$ = str(dt_rec$,42%,3%)
REM            tso_bso% = 1%
            code% = 1%
L51670: return

        lookup_shift
            code% = 0%
            init(" ") readkey$, scr_msg1$
            str(readkey$,1%,9%)   = "PLAN SHFT"
            str(readkey$,10%,15%) = scr_shft$
            read #3,key = readkey$, using L51640, scr_msg1$,eod goto L51750
            code% = 1%
L51750: return

        lookup_load                          /* (APCPLNLD) - Load File */
           apc% = 0%
           read #5,key = ld_load$, using L51810, ld_desc$,                ~
                                                eod goto lookup_load_done
L51810:       FMT POS(16), CH(30)
            apc% = 1%
        lookup_load_done
        return

        lookup_schedule
            init(" ") readkey$, sched$
            str(readkey$,1%,9%)   = "PLANSCHED"
            str(readkey$,10%,15%) = "LINEALMA"
            read #3,key = readkey$, using L51910, sched$,eod goto L51920
L51910:       FMT POS(25), CH(3)
L51920: return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        convert_fields
            init(" ") fld$(), wd$, ht$, spec$, t_b$
            s_width, s_height, s_clmr = 0.0          /* Decimal Values */
            lit_flg$ = "N"                           /* (EWD015)       */
            if str(dt_part$,1%,3%) = "003" then lit_flg$ = "Y"
                                                     /* (EWD015)       */
            partno$ = dt_part$
            fld$(1%) = str(partno$,1%,3%)              /* Model Number */
            fld$(2%) = str(partno$,4%,1%)              /* Color        */
         if dt_prt$ = "Y" then goto L60440              /*Component Part*/
            fld$(3%) = str(partno$,5%,2%)              /* Glass        */
            fld$(4%) = str(partno$,7%,2%)              /* Liting       */
            fld$(5%) = str(partno$,9%,2%)              /* Hinge        */
            fld$(6%) = str(partno$,11%,1%)             /* Screen       */
            fld$(7%) = str(partno$,12%,1%)             /* Locks        */
            fld$(8%) = str(partno$,13%,4%)             /* Width        */
            fld$(9%) = str(partno$,17%,3%)             /* Height       */
            fld$(10%)= str(partno$,20%,3%)             /* CLMR         */
            fld$(11%)= str(partno$,23%,3%)             /* WALLWIDT     */
            if lit_flg$ = "Y" then goto L60445         /* (EWD015)     */

            gosub std_wd_ht                     /* ACTUAL WIDTH/HEIGHT */
            gosub lookup_hinge
            gosub check_sash
               a1, a2 = 0.0
               convert str(fld$(8%),1%,3%) to a1, data goto L60250
L60250:
               convert str(fld$(8%),4%,1%) to a2, data goto L60270
L60270:
               s_width = a1 + (a2/8.0)
               a1, a2 = 0.0
               convert str(fld$(9%),1%,2%) to a1, data goto L60310
L60310:
               convert str(fld$(9%),3%,1%) to a2, data goto L60330
L60330:
               s_height = a1 + (a2/8.0)
               a1, a2 = 0.0
            if len(partno$) < 22 then return
               convert str(fld$(10%),1%,2%) to a1, data goto L60430

               convert str(fld$(10%),3%,1%) to a2, data goto L60400
L60400:
               s_clmr = a1 + (a2/8.0)
               if s_clmr <= 8.0 then s_clmr = 0.0
L60430: return
L60440:     wd$ = "COMPONE"
            ht$ = "NT PRT"
        return
L60445:     wd$ = str(dt_part$,13%,4%)
            ht$ = str(dt_part$,17%,3%)
        return                                        /* (EWD015)      */

        lookup_hinge                                  /* Look Up Hinge */
            init(" ") readkey$, c_o$, hnge$
            str(readkey$,1%,9%)   = "HINGE    "
            str(readkey$,10%,15%) = fld$(5%)
            read #3,key = readkey$, using L60530, descr$, eod goto L60580
L60530:        FMT POS(25), CH(30)
            p% = pos(descr$ = "-")
            hnge$ = str(descr$,p%+2%,4%)
            if str(descr$,1%,2%) = "CO" or str(descr$,1%,2%) = "OR" then ~
                                                c_o$ = str(descr$,1%,2%)
L60580: return

        check_sash                                    /* Look Up Screen*/
            dt_sash$ = str(dt_rec$,214%,1%)
            if dt_sash$ = "1" then t_b$ = "TSO"
            if dt_sash$ = "2" then t_b$ = "BSO"
            if dt_sash$ = "3" then t_b$ = "FGO"
        return

        std_wd_ht              /* CONVERT STANDARD WIDTH AND HEIGHT */
                              /* F0%       - FRACT. NEW PART WIDTH    */
                              /* F1%       - FRACT. NEW PART HEIGHT   */
                              /* WD$   - REPLACEMENT WIDTH & FRACT (7)*/
                              /* HT$   - REPLACEMENT HEIGHT & FRACT(6)*/
           str(wd$,1%,3%)  = str(partno$,13%,3%)    /* WIDTH PART (3) */
           if str(wd$,1%,1%) = "0" then str(wd$,1%,1%) = " "
           str(ht$,1%,2%) = str(partno$,17%,2%)     /* HEIGTH PART (2)*/

           f0%, f1% = 0%                            /* SET FRACTIONS  */
           convert str(partno$,16%,1%) to f0%, data goto L60820 /* WIDTH */

           convert str(partno$,19%,1%) to f1%, data goto L60820 /* HEIGH */

           goto L60830
L60820:      f0%, f1% = 8%
L60830:    if f0% = 0% then f0% = 9%
           if f1% = 0% then f1% = 9%

           str(wd$,4%,1%) = " "          /* Build Width with Fraction */
           str(wd$,5%,3%) = str(sze$,(f0%*3%) - 2%, 3%)

           str(ht$,3%,1%) = " "          /* Build Height with Fraction */
           str(ht$,4%,3%) = str(sze$,(f1%*3%) - 2%, 3%)
        return

        create_report                           /* For All Departments */
            mode% = 1% : gosub open_work
            wrk_seq% = 0%
            init(" ") save_part$, sav_cl$, sav_size$, total$, dt_key1$,   ~
                      sav_dept$
            dt_key1$ = all(hex(00))
            str(dt_key1$,1%,6%) = str(scr_dte1$,1%,6%)  /* Prod Date   */
            read #2,key 1% > dt_key1$, using L35050, dt_rec$,             ~
                                                    eod goto scan_done
            goto L61040
        scan_next
            read #2, using L35050, dt_rec$, eod goto scan_done

L61040:     dt_load$ = str(dt_rec$,1%,5%)
            dt_key1$ = str(dt_rec$,47%,57%)
            dt_shft$ = str(dt_rec$,104%,2%)
            if str(dt_key1$,1%,6%) <> str(scr_dte1$,1%,6%) then          ~
                                                            goto scan_done
            if str(scr_load$,1%,1%) = "N" then goto L61120
               if scr_load$ <> dt_load$ then goto scan_next
                  goto L61130
L61120:     if pos("A" = str(dt_load$,1%,1%)) > 0% then goto scan_next
L61130:     if scr_dept$ <> str(dt_rec$,42%,3%) then goto scan_next
            if scr_shft$ = "AA" then goto L61170
               if scr_shft$ <> dt_shft$ then goto scan_next

L61170:        dt_part$ = str(dt_rec$,189%,25%)         /* Part Number */
                                                        /* (EWD015)    */
                                                        /* (EWD020)    */
             if option$ <> "2" and option$ <> "9" then goto L61210

               if scr_dept$ = "017" or scr_dept$ = "018" then goto L61210
               if scr_dept$ = "007" then goto L61210
/* (AWD045) */
               if scr_dept$ = "002" then goto L61210         /* CR3333 */
               if scr_dept$ = "027" then goto L61210
               if scr_dept$ = "028" then goto L61210
               if scr_dept$ = "048" then goto L61210
               if scr_dept$ = "036" then goto L61210
               if scr_dept$ = "053" then goto L61210   /* CR2803 */

               if scr_dept$ = "017" and str(dt_part$,1,3) = "449" and ~
                     str(dt_part$,11,1) = "5" then goto L61210


               gosub lookup_tso_bso
                  if code% <> 1% then goto L61210
               if option$ = "9" then gosub check_tso_bso                ~
                  else gosub check_not_tso_bso
               if check_tso_bso% = 0% then scan_next
L61210:
               gosub check_samples
               if ss% > 11% and ss% < 29% then goto scan_next
                                                        /* Skip Parts  */
                                                        /* (EWD015)    */

               dt_cust$ = str(dt_rec$,124%,9%)          /* (AWD043)    */
               dt_txt$  = str(dt_rec$,236%,4%)          /* Text Id     */
               dt_bar$  = str(dt_rec$,24%,18%)          /* Set Barcode */
               model$   = str(dt_part$,1%,3%)           /* Model Code  */
               mm$      = str(model$,1%,2%)             /* 2 Digit Prod*/
               dt_sort$ = str(dt_rec$,106%,5%)          /* Prod Sort   */
               scr_prod$= str(model$,1%,1%)             /* Product Line*/
               dt_ref$  = str(dt_rec$,96%,8%)           /* Warranty No.*/
               dt_sash$ = str(dt_rec$,214%,1%)          /* 0, 1, 2, 3  */
               dt_prt$  = str(dt_rec$,215%,1%)          /* (Y)es, (N)o */
               dt_samp$ = str(dt_rec$,216%,1%)          /* 0=NO,1=SAMP */
                                                        /* 2 = DISPLAY */
               dt_seq$  = str(dt_rec$,111%,5%)          /* Low/High Seq*/
               seq% = 0%
               convert dt_seq$ to seq%, data goto L61320
L61320:
               sqq% = 99999% - seq%
               convert sqq% to sqq$, pic(00000)         /* High/Low Seq*/


               gosub convert_fields
               init(" ") wd1$, wd2$, wd3$, wd4$, wd5$, wd6$, wd7$,       ~
                         wd8$, wd9$, wd10$, wd11$
               gosub update_rpt

            goto scan_next
        scan_done
            mode% = 3% : gosub open_work

            opt% = pos("2349" = option$)
            if opt% <> 0% then goto optimize
            gosub print_rpt
        return

                                                        /* (EWD026)    */
        check_tso_bso
             check_tso_bso% = 0%
             if str(dt_rec$,42%,3%) = sav_dept$ then goto tso_bso
              gosub lookup_tso_bso                  /* one dept at a time*/
                  if code% <> 1% then return
tso_bso:
                  p% = 0%
                  p% = pos("45" = str(dt_part$,11%,1%))    /* TSO/BSO     */
                  if p% = 0% then return
              check_tso_bso% = 1%
         return

         check_not_tso_bso
              check_tso_bso% = 0%
                  p% = 0%
                  p% = pos("45" = str(dt_part$,11%,1%))    /* TSO/BSO     */
                  if p% <> 0% then return
              check_tso_bso% = 1%
         return
                                                              /* (EWD026)    */

                                                      /* (EWD015)       */
        check_samples
            ss% = 0%
            if len(dt_part$) < 20 then goto LS2      /* Quick Test      */
            if str(dt_part$,1%,1%) = "9" then goto LS2 /* Bay/Bow       */
            convert str(dt_part$,20%,3%) to ss%, data goto LS1

            if ss% < 1% or ss% > 80% then goto LS1   /* Not Samp/Disp   */
                                                     /*   (EWD022)      */
            if str(dt_part$,7%,2%) > "99" then goto LS1

        return                                       /* Code Found      */
LS1:        convert str(dt_part$,23%,3%) to ss%, data goto LS2

            if ss% < 1% or ss% > 80% then goto LS2
                                                     /* Code Found      */
        return
LS2:        ss% = 0%
        return
                                                       /* (EWD015)      */
        optimize                         /* Build Saw Optimization Data */
            if dept% = 66% then return                 /* Literature    */

            count% = 0%
            if schema% = 1% and option$ = "2" 						~
            	then goto L61710      				    /* LINEALMATE    */
                                                       /*  (EWD026)     */
                                                       /* (AWD052)      */
            if schema% = 2% and option$ = "2" 			/*MEG*/			~
            	then goto LNTXlineal      			   /* LINEALMATE    */                                           
            if option$ = "9" then goto L62310          /* LINEALMATE    */
                                                       /* TSO/BSO       */
            if option$ = "4" then goto L62530          /* WEGOMA WELDER */

                                                       /* option$ = 3   */
                                                       /* Sash Files    */
                                                       /* (PAR001)      */
                                                       /* Remove Dept=6 */
            
            if dept% <> 33% and dept% <> 48% then goto L62890

                              scr_prod$ = "6"
                              call "APCPLB42" ( count%,   /* No. Sash's*/~
                                                scr_dept$,/* DEPARTMENT*/~
                                                scr_prod$,/* PROD LINE */~
                                                lk_fn$(), /* 1,2 Lk Fin*/~
                                                #1,       /* APCCUTWK  */~
                                                #3,       /* GENCODES  */~
                                                #11,      /* APCCUTEQ  */~
                                                #12,      /* AMTBOMCD  */~
                                                #6 )      /* AMTBOMIF  */
               return

L61710:     if dept% = 23% or dept% = 42% then goto L62890 /* NOT APPLIC*/

               scr_prod$ = "6"
               if dept% = 7% then scr_prod$ = "2" /* (EWD016)  CR2985   */
REM               IF DEPT% = 8% OR DEPT% = 47% THEN GOTO L62220
               if dept% = 47% then goto L62220

               if dept% = 8% then goto L62226

               if dept% = 9% then scr_prod$ = "9"
                                                   /* (PAR001)          */
REM               if dept% = 6% then goto L61800      /* NE dept 006       */
                                                   /* (PAR001)          */
                                                   /* (EWD034)          */
               if dept% = 6% then goto L61810      /* (AWD050)          */
                                                                  
               if dept% = 7% then goto L61940      /* CR2985            */
                                                   /* (EWD017) 06/12/00 */
                                                   /* (EWD034)          */
                                                   /* (EWD038)          */
               if dept% = 17% then goto L61780

               if dept% = 19% then goto L61780

               if dept% = 18% then goto L61790
                                                   /* (EWD038)          */
                                                   /* (PAR002)          */
               if dept% = 26% then goto L61820
                                                   /* (PAR002)          */

               if dept% = 27% then scr_prod$ = "5" /* (EWD027)          */
                                                   /* (EWD033)Linealmate*/
               if dept% = 27% then goto L61720     /* (EWD027) 10/27/03 */
                                                   /* (EWD033)          */

                                                   /* (EWD036) Flex     */
               if dept% = 28% then scr_prod$ = "5" /* (EWD027)          */
               if dept% = 28% then goto L61720     /* (EWD027) 10/27/03 */
                                                   /* (EWD036) Flex     */

               if dept% = 49% or dept% = 52% then goto L61900
               if dept% = 5% then goto L61900     /* (AWD044) */
                                                  /* (EWD012) 09/23/99 */
               if dept% = 50% then goto L61950    /* (EWD021) 02/06/02 */

               if dept% = 51% then goto L61960    /* (EWD023) 11/07/02 */
/* CR283 */
              if dept% = 53% then scr_prod$ = "5" /* (EWD027)          */               
              if dept% = 53% then goto L61940  /*   (AWD051) 08/27/14 */
              
REM              if dept% = 25% then goto L61770   /* (IM7733) 04/02/14 */

               if dept% = 2%  then goto L61910
                                                  /* (EWD001) - Begin  */
               if dept% = 48% then goto L62300    /* Wegoma Saws       */
                                                  /* (EWD001) - End    */
               if dept% = 36% then goto L62625    /* (EWD039) - Dept 36*/
REM            if dept% = 36% then goto L61890    /* (EWD009) - Dept 36*/

/* (AWD042) florida windows */
REM               if dept% = 4% or dept% = 14% then goto L62635
REM               if dept% = 19% or dept% = 20% then goto L62635
/* (/AWD042) florida window */
             if dept% = 14% then goto L62635
                                                  /* Wegoma Saws       */
                                                  /* (EWD012)          */
                                                  /* (PAR000) ???      */
                                                  /* Bilco Positioners */
               call "APCPLC42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
        return                                            /* (EWD012)  */

                                                          /* (EWD017)  */
L61720:                                                   /* (EWD033)  */
        if dept% <> 27% then goto L61730
               scr_prod$ = "5"                            /* (PAR000)  */
                                                          /* Dept = 027*/
               call "AWDPLF42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
               return
                                                          /* (EWD033)  */

L61730:                                                   /* (EWD034)  */
                                                          /*Joseph Saws*/
        if dept% <> 7% then goto L61740
               scr_prod$ = "2"                            /* (PAR000)  */
                                                          /* Dept = 007*/
               call "AWDPLG42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
               return
                                                          /* (EWD034)  */
                                                          /* (EWD036)  */
L61740:
                                                          /* (EWD037)  */
               scr_prod$ = "5"                            /* (PAR000)  */
                                                          /* Dept = 028*/
               call "AWDPLI42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
        return                                            /* (EWD012)  */
                                                          /* (EWD017)  */
                                                          /* (EWD036)  */

                                                          
REM L61770                                            /* (IM7733) Dept = 025*/
REM               scr_prod$ = "4"                            /* (PAR000)  */
REM               call "AWDPLV42" ( size%,                   /* BATCH SIZE*/~
REM                                 sched%,                  /* Start Sch */~
REM                                 scr_dte$,                /* Prod Date */~
REM                                 scr_dept$,               /* DEPARTMENT*/~
REM                                 scr_prod$,               /* Prod Line */~
REM                                 scr_load$,               /* Prod Load */~
REM                                 lk_fn$(),                /* 1,2 Lk Fin*/~
REM                                 #1,                      /* APCCUTWK  */~
REM                                 #3,                      /* GENCODES  */~
REM                                 #11,                     /* APCCUTEQ  */~
REM                                 #12,                     /* AMTBOMCD  */~
REM                                 #6 )                     /* AMTBOMIF  */
REM        return                                                                                         
                                                          
                                                          /* (EWD038)  */
L61780:                                                   /* Dept = 017*/
               scr_prod$ = "4"                            /* (PAR000)  */
               call "AWDPLJ42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
        return                                            /* (EWD038)  */

L61790:                                                   /* Dept = 018*/
               scr_prod$ = "4"                            /* (PAR000)  */
               call "AWDPLK42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
        return                                            /* (EWD038)  */

                                                          /* (EWD038)  */

                                                          /* (PAR001)  */
REM L61800:                                                   /* Dept = 006*/
               scr_prod$ = "5"                            /* (PAR001)  */
               call "AWDPLL42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */

               scr_prod$ = "5"                            /* (PAR001)  */
               call "AWDPLM42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
        return                                            /* (PAR001)  */

                                                          /* (PAR002)  */

L61810:                                                   /*(AWD050)*/
                                                          /* Dept = 006*/
               scr_prod$ = "E"                            /* (PAR001)  */
               call "AWDPLT42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
                                                  
        return                                                  
L61820:                                                   /* Dept = 026*/
               scr_prod$ = "7"                            /* (PAR002)  */
               call "AWDPLO42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
        return

REM L61890: REM - Sash, Frame, Header for Wegoma Saws         /* (EWD009)  */
               scr_prod$ = "7"                            /* Dept = 36 */
               sav_sched% = sched%                        /* (PAR000)  */
               for ii% = 1% to 3%
                   if ii% = 1% then scr_type$ = "1"
                   if ii% = 2% then scr_type$ = "2"
                   if ii% = 3% then scr_type$ = "3"
                   call "APCPLW42" ( size%,               /* BATCH SIZE*/~
                                     sched%,              /* Start Sch */~
                                     scr_dte$,            /* Prod Date */~
                                     scr_dept$,           /* DEPARTMENT*/~
                                     scr_prod$,           /* Prod Line */~
                                     scr_load$,           /* Prod Load */~
                                     scr_type$,           /* Sash 1st  */~
                                     lk_fn$(),            /* 1,2 Lk Fin*/~
                                     #1,                  /* APCCUTWK  */~
                                     #3,                  /* GENCODES  */~
                                     #11,                 /* APCCUTEQ  */~
                                     #12,                 /* AMTBOMCD  */~
                                     #6 )                 /* AMTBOMIF  */
               next ii%

        return
                                                       /* (EWD009) End */
        REM - Frame Saws are Enabled ( 01/21/98)          /* New Family*/
L61900:        scr_prod$ = "4"
               sav_sched% = sched%
                                                          /* (PAR000)  */
               if dept% <> 52% then goto L61905           /* (EWD028)  */

               call "AWDPLB42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
               return

L61905:                                                   /* (EWD028)  */
                                                          /* (PAR000)  */
               call "AWDPLD42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
               return

        REM - Sash Saws are Enabled ( 01/21/98 )          /* New Family*/
L61910:        scr_prod$ = "2"
               sched% = sav_sched%
                                                          /* (PAR000)  */
               call "AWDPLC42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
               return
                                                          /* (EWD030)  */

        REM - Frame and Sash Saw Enabled ( 02/06/02)      /* (EWD021)  */
 
L61940:                                                   /* (CR2985)  */
        if dept% <> 53% and dept% <> 7% then goto L61950
               scr_prod$ = "5"                            /* (PAR000)  */
                                                          /* Dept = 053*/
               partstyle$ =  str(bcksubpt_rec$,185%, 10%)
               call "AWDPLX42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 partstyle$,              /* Bcksubpt style */~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
               return
                                                          /* ((AWD051))  */  
                                                                       
L61950:        scr_prod$ = "4"                            /* New 411   */
               sav_sched% = sched%
                                                          /* (PAR000)  */
               call "AWDPLA42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */

               return

        REM - Frame and Sash Saw Enabled ( 02/06/02)      /* (EWD023)  */
L61960:        scr_prod$ = "4"                            /* New 450   */
               sav_sched% = sched%
                                                          /* (PAR000)  */
               call "AWDPLE42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */


               return                                     /* (RHHTEST) */

        REM - Sash Saws are Enabled ( 01/21/98 )          /* Casement  */
L62220:        scr_prod$ = "8"                            /* (PAR000)  */
               sched% = sav_sched%
               if dept% = 47% then goto L62225
               call "APCPLO42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */


        REM - Frame Saws are Enabled ( 01/21/98 )         /* Casement  */
               sched% = sav_sched%                        /* (PAR000)  */
               call "APCPLP42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
        return

L62225:                                                   /* (EWD002)  */
                                                          /* (PAR000)  */
        REM - Bilco Positioner Saw   ( 09/18/98 )         /* Casement  */
               sched% = sav_sched%                        /* Dept = 047*/
               call "APCPLS42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
        return


L62226:
REM               call "SHOSTAT" (" I AM HERE " )  stop

               call "AWDPLP42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */

            return


                                         /* Special Wegoma Saws(EWD001)*/
L62300:
            scr_prod$ = "4"
                                         /* (PAR000)                   */
            sched% = sav_sched%          /* (EWD002) - Begin 09/18/98  */
                                         /* (EWD014) - New Prod's      */
           for ii% = 1% to 6%            /* (EWD027) - New Prod's      */
               if ii% = 1% then scr_type$ = "A"
               if ii% = 2% then scr_type$ = "B"
               if ii% = 3% then scr_type$ = "C"
               if ii% = 4% then scr_type$ = "D"
               if ii% = 5% then scr_type$ = "E"           /*  (EWD027) */
               if ii% = 6% then scr_type$ = "F"           /*  (AWD046) */
               call "APCPLR42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 scr_type$,               /* A,B,C, D  */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
           next ii%
                                         /* (PAR000) ???? Below for 48 */

REM            call "SHOSTAT" ("I AM HERE at BUILD Sash Weld ")  stop

               call "AWDPLS42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */

           return


L62310:                                  /* (EWD026) - For TSO/BSO     */
                                         /* (PAR000)                   */
               call "EWDPLG42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */


        return                           /* (EWD002) - End 09/11/98    */
                                         /* (EWD014) - End 01/06/00    */

                                         /* Special for Wegoma Welders */
L62530:     scr_prod$ = "6"
            if dept% = 36% then scr_prod$ = "7"
            if dept% = 25% then scr_prod$ = "5"         /*  (EWD027)   */
                                                        /*  (PAR001)   */
                                                        /*Remove dept=6 */
            if dept% = 33% then goto L62610
                                         /* (EWD016) Mods for Dept 007 */
                                                      /* (PAR000)      */
                                                      /* (PAR002)      */
                                                      /* Rm - Dept 26  */
            if dept% = 25% or dept% = 36% then goto L62620
            if dept% = 25% then goto L62620

                                         /* (EWD004) - Sampson Welder  */
                                         /* (PAR000)                   */
            if dept% = 48% then goto L62630
                                                      /* (EWD038)     */
                                                      /* (PAR000) Dept*/
                                                      /* 17,18,27,28  */
            if dept% = 17% or dept% = 18% or dept% = 27% or ~
               dept% = 28% then goto L62890           /* N/A          */

            if dept% = 49% or dept% = 52% then goto L62890   /* N/A   */

            if dept% = 50% then goto L62890           /* N/A          */

            if dept% = 51% then goto L62890           /* N/A          */

            if dept% = 2%  then goto L62890           /* N/A          */

            if dept% = 7%  then goto L62890           /* N/A          */

            if dept% = 5% then goto L62890            /* N/A          */

        return

L62610:                       call "APCPLD42" ( count%,   /* No. Sash's*/~
                                                scr_dept$,/* DEPARTMENT*/~
                                                scr_prod$,/* PROD LINE */~
                                                lk_fn$(), /* 1,2 Lk Fin*/~
                                                #1,       /* APCCUTWK  */~
                                                #3,       /* GENCODES  */~
                                                #11,      /* APCCUTEQ  */~
                                                #12,      /* AMTBOMCD  */~
                                                #6 )      /* AMTBOMIF  */

        return
                                            /* (EWD007) Sampson Welder */
L62620: REM - Sampson Sash Welder Enabled ( 05/06/99)                  */
                                            /* (PAR000)   Dept = 25,36 */
               sav_sched% = sched%
                              call "APCPLV42" ( count%,   /* No. Sash's*/~
                                                scr_dept$,/* DEPARTMENT*/~
                                                scr_prod$,/* PROD LINE */~
                                                lk_fn$(), /* 1,2 Lk Fin*/~
                                                #1,       /* APCCUTWK  */~
                                                #3,       /* GENCODES  */~
                                                #11,      /* APCCUTEQ  */~
                                                #12,      /* AMTBOMCD  */~
                                                #6 )      /* AMTBOMIF  */
                                                          /* (EWD006)   */
                                                     /* (EWD007) - End */


        return

L62625: REM - Sampson Sash Welder Enabled ( 05/06/99)
                             /* (PAR000)   Dept = 36    */

               scr_prod$ = "7"                            /* Dept = 36 */
               sav_sched% = sched%                        /* (PAR000)  */
                   call "APCPLX42" ( size%,               /* BATCH SIZE*/~
                                     sched%,              /* Start Sch */~
                                     scr_dte$,            /* Prod Date */~
                                     scr_dept$,           /* DEPARTMENT*/~
                                     scr_prod$,           /* Prod Line */~
                                     scr_load$,           /* Prod Load */~
                                     lk_fn$(),            /* 1,2 Lk Fin*/~
                                     #1,                  /* APCCUTWK  */~
                                     #3,                  /* GENCODES  */~
                                     #11,                 /* APCCUTEQ  */~
                                     #12,                 /* AMTBOMCD  */~
                                     #6 )                 /* AMTBOMIF  */
               return


L62630:                           /* (EWD004) Sampson Welder SASH'S    */
                                  /* (PAR000)                          */
                              call "APCPLT42" ( count%,   /* No. Sash's*/~
                                                scr_dept$,/* DEPARTMENT*/~
                                                scr_prod$,/* PROD LINE */~
                                                lk_fn$(), /* 1,2 Lk Fin*/~
                                                #1,       /* APCCUTWK  */~
                                                #3,       /* GENCODES  */~
                                                #11,      /* APCCUTEQ  */~
                                                #12,      /* AMTBOMCD  */~
                                                #6 )      /* AMTBOMIF  */
        return

/* (AWD042) florida windows departments 004, 014, 019, 020 */
L62635:

               scr_prod$ = "E"                        /* Florida Windows */
               call "AWDPLR42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
        return



/* (/AWD042) florida windows */
L62890: return                                  /* Not Applicable - Exit */

        update_rpt
                                                /* (PAR001)            */
                                              /* MFG Part and Sub Part */
            so_inv$  = str(dt_rec$,24%,8%)           /* Sales Order    */
            item_no$ = str(dt_rec$,32%,2%)           /* Line Item No.  */
            gosub lookup_sub_part                    /* (PAR001)       */

            init(" ") wrk_key$, wrk_key1$, wrk_rec$
            wrk_seq% = wrk_seq% + 1%                 /* Assign Seq No. */
            convert wrk_seq% to wrk_key$, pic(00000) /* File Being Read*/
                                                     /* Date Load Seq. */
/* (AWD041) */
REM            str(wrk_key1$,1%,5%)  = dt_sort$         /* Product Sort*/
            str(wrk_key1$,1%,5%)  = dt_seq$          /* Product Seq    */
            str(wrk_key1$,6%,3%)  = model$           /* Model Code     */
            str(wrk_key1$,9%,1%)  = fld$(2%)         /* Color Code     */
            str(wrk_key1$,10%,4%) = fld$(8%)         /* Window Width   */
            str(wrk_key1$,14%,3%) = fld$(9%)         /* Window Height  */
                                                     /* (EWD011) remove*/
                                                     /* sort_prod_7    */
            gosub get_info                           /* Additional Info*/
            str(wrk_rec$,1%,8%)  = dt_ref$           /* Warranty No.   */
            str(wrk_rec$,9%,5%)  = dt_seq$           /* Sequence No.   */
            str(wrk_rec$,14%,2%) = c_o$              /* Cottage/Oriel  */
            str(wrk_rec$,16%,4%) = t_t$              /* Twin/Triple    */
            str(wrk_rec$,20%,3%) = t_b$              /* TSO,BSO,FSO    */
            str(wrk_rec$,23%,3%) = locks$            /* Locks          */
            str(wrk_rec$,26%,3%) = fin$              /* With Fin       */
            str(wrk_rec$,29%,5%) = dt_load$          /* Load Number    */
            str(wrk_rec$,34%,4%) = dt_txt$           /* Text Id        */
            str(wrk_rec$,38%,25%)= dt_part$          /* Part Number    */
            str(wrk_rec$,63%,7%) = wd$               /* WIDTH - WINDOW */
            str(wrk_rec$,70%,7%) = ht$               /* HEIGHT- WINDOW */
            str(wrk_rec$,77%,1%) = dt_samp$          /* SAMPLE FLAGS   */
            str(wrk_rec$,78%,3%) = str(dt_rec$,42%,3%) /* Department   */
                                                     /* (PAR001)       */
            str(wrk_rec$,81%,20%)  = dt_sub_part$    /* Sub Part No.   */
            str(wrk_rec$,101%,20%) = dt_sub_info$    /* New Info Fields*/
            str(wrk_rec$,121%,9%)  = dt_cust$        /* (AWD043)    */

/* (AWD048) */
            convert dim1es to str(wrk_rec$,130%,8%), pic(000.000)            
            
            convert dim2es to str(wrk_rec$,139%,8%), pic(000.000)
/*(AWD049)*/str(wrk_rec$,147%,18%) = dt_bar$
/*(AWD049)*/str(wrk_rec$,165%,2%) = dt_shft$
/*          str(wrk_rec$,147%,55%) = " "                Filler         */
            str(wrk_rec$,167%,34%) = " "             /*<AWD049>        */
                                                     /* (PAR001)       */
                                                     /* Cut Records    */
            put #1, using L63190, wrk_key$, wrk_key1$, wrk_rec$
L63190:        FMT CH(5), CH(51), CH(200)
            write #1, eod goto L63280
            if option$ <> "1" then return
            if tube% = 0% then return
                                                     /* Tube Winding   */
            gosub sort_product
            write #8, eod goto L63270
L63270: return
L63280:  call "SHOSTAT" ("(Error) Updating Work File for Selection?")
        return

        calc_tube
           init(" ") readkey$ : tube% = 0%    /* Check for Valid Model */
           str(readkey$,1%,9%)   = "PLAN TUBE"
           str(readkey$,10%,15%) = model$
           read #3,key = readkey$, eod goto L63430

                 gs$  = str(dt_part$,5%,2%)
                 flg$ = str(c_o$,1%,1%)
                 call "APCPLI42" (1%, gs$, s_height, s_width, w1$,       ~
                                          w2$, w3$, model$, wind$, flg$ )
                 wd3$ = w1$ : wd4$ = w2$ : wd5$ = w3$
                 tube% = 1%
L63430: return

/*(CR2355)*/
        get_slider
            slider% = 0%
            readkey$ = "GLASS07  " & str(dt_part$,1%,3%)
            read #3,key = readkey$, eod goto L63435
            slider% = 1%
                                         
L63435: return
/*CR2503*/
        get_contHS
            cont_qty% = 0%
            
            str(readkey$,1%,9%)   = "PLAN CONT"
            str(readkey$,10%,15%) = str(dt_part$,1%,3%)
            read #3,key = readkey$, using L63536, descr$, eod goto L63440
            convert str(descr$,30%,1%) to cont_qty%, data goto L63440   
L63536:        FMT POS(25), CH(30)

L63440: return
            
        get_info
            tube% = 0%
            init(" ") t_t$, locks$, fin$
            if dt_prt$ = "Y" then return                   /* Part     */
               t_t$ = "SNGL"                               /* Single   */
               if hnge$ = "TWIN" then t_t$ = hnge$         /* Double   */
               if hnge$ = "TRPL" then t_t$ = hnge$         /* Triple   */
               if pos(w_f$ = str(fld$(7%),1%,1%)) <> 0 then              ~
                  fin$ = "YES"                             /* With Fin */
               locks$ = "(0)"                         /* LOCKS - O,1,2 */
               if pos(w_1$ = str(fld$(7%),1%,1%)) <> 0 then              ~
                                                     locks$ = "(1)"
               if pos(w_2$ = str(fld$(7%),1%,1%)) <> 0 then              ~
                                                     locks$ = "(2)"
                                                   /* DOUBLE HUNG      */
                                                   /* NEW CONSTRUCTION */
                                                   /* SLIDERS/PICTURE  */
/* (AWD040) */
              if str(fld$(7),1,1) <> "5" and str(fld$(7),1,1) <> "6"  ~
                                        then goto not_with_lock
                 locks$ = "(1)"
/*  (CR2355) */
                 gosub get_slider
                 gosub get_contHS /*CR2503*/
                 
                 if slider% =0% and fld$(8%) > "0271" then locks$ = "(2)"
                 if slider% =1% and fld$(9%) > "271"  then locks$ = "(2)"
/* CR2503 27.25 * 2 + 0.5 (mull) = 0550 or 55.0*/
/* CR2503 27.25 * 3 + 0.5 (mull) + 0.5 (mull) = 0826 or 82.75 */
                 if cont_qty% = 2% and fld$(8%) <= "0550" then locks$ = "(1)"
                 if cont_qty% = 3% and fld$(8%) <= "0826" then locks$ = "(1)"
not_with_lock:

REM               if dept% = 6% or dept% = 28% or dept% = 33% or dept% = 48%~
                                                     then gosub calc_tube
               if dept% = 6% or dept% = 33% or dept% = 48% then gosub calc_tube
               if dept% = 49% then gosub calc_tube /* New Family       */
               if dept% = 52% then gosub calc_tube /* NEW FAMILY       */
                                                   /* (EWD038)         */
               if dept% = 17% then gosub calc_tube /* 450 Brick Mold   */

        return

        print_rpt
            if schema% <> 1% then goto print_ntx    /* (AWD047)        */
                                                    /* (EWD003) for    */
                                                    /* (PAR003)        */
                                                    /* Special Reports */
            if option% > 4% then goto print_rpt_special
                                                    /* (PAR003)        */
                                                    /* (PAR000)        */
            if dept% = 2% or dept% = 23% or dept% = 42% or dept% = 43% or ~
               dept% = 50%                then goto print_rpt_3
                                                    /* (PAR000)        */
            if dept% = 51% or dept% = 18% then goto print_rpt_3
                                                    /* (PAR000)        */
                                                    /* (PAR001) 6%=NE  */
            if dept% = 17% or dept% = 33% or dept% = 48% or               ~
               dept% = 49% or dept% = 52% then goto print_rpt_4

            if dept% = 5% then goto print_rpt_4     /* (AWD044) */

                                                    /* (PAR000)        */
                                                    /* (PAR002)        */
            if dept% = 7% or dept% = 25% or dept% = 26% or dept% = 27% or ~
               dept% = 28% or dept% = 36% or dept% = 53%                 ~
                then goto print_rpt_5               /* (AWD051)        */
                                                    /* (PAR002)        */
                                                    /* (PAR000)        */
            if dept% = 6% or dept% = 19%                                  ~
                         then goto print_rpt_5     /* (AWD050)        */ 
            if dept% = 8% or dept% = 9% or dept% = 47% then               ~
                                               goto print_rpt_6

        print_rpt_3
            if dept% = 23% then spec% = 0%        /* VINYL PATIO      */ 
            if dept% = 42% then spec% = 1%        /* HINGED PATIO     */
            if dept% = 43% then spec% = 2%        /* SPECIAL PRODUCTS */
            if dept% = 2%  then spec% = 3%        /* Dept 211         */
            if dept% = 50% then spec% = 4%        /* Cont. Head/Sill  */
                                                  /* (EWD038)         */
            if dept% = 51% then spec% = 5%        /* 500 Cont Head    */
            if dept% = 18% then spec% = 6%        /* 450 Cont Brick Mold*/
            if dept% = 14% then spec% = 7%
                                                  /* (EWD038)         */

                                                  /* (EWD038)         */
            call "APCPLE42" (scr_dte$, prod_dte$, spec%, #1, #3, #6,     ~
                                                       #7, #9, #11, #12 )

                                                  /* (EWD035)         */

REM            if dept% <> 43% then return
            if schema% = 1% and dept% <> 43% then return
 
            call "AWDPLH42" (scr_dte$,      /* Production Date           */ ~
                             comp_dte$,     /* Completion Date           */ ~
                             scr_shft$,     /* Shift Selection           */ ~
                             scr_load$,     /* Screen Load Selection     */ ~
                             scr_dept$,     /* Screen Dept               */ ~
                             #63,           /* bcksubpt <SR73216>        */ ~
                             #4,            /* (AWDSPECB) Shapes Bending */ ~
                             #3,            /* (GENCODES) Master Tables  */ ~
                             #9 )           /* (TXTFILE ) Text File      */

                                                  /* (EWD035)         */
            return
        print_rpt_4 
REM            if dept% =  6% then spec% = 0%        /* (PAR001) 6%=NE   */
                                                     /*(AWD050)          */
            if dept% = 48% then spec% = 1%
            if dept% = 33% then spec% = 2%
            if dept% = 49% then spec% = 3%   
            if dept% = 52% then spec% = 4%
                                                  /* (EWD038)          */
            if dept% = 17% then spec% = 9%
                                                  /* (EWD038)          */
            if dept% = 5% then spec% = 11%        /* (AWD044)          */
        print_rpt_special                         /* (EWD003)          */
            if option$ = "11" then goto print_slow_lineals   /* (AWD040) */
            if option$ = "5" then spec% = 5%
            if option$ = "6" then spec% = 6%      /* (PAR001) 6%=NE    */
            if option$ = "7" then spec% = 7%
            if option$ = "8" then spec% = 8%      /* (EWD003) End      */
            if option$ = "10" then spec% = 10%    /* (PAR003)          */

REM            if dept% = 6% then goto print_rpt_4a  /* (PAR001)(AWD050)*/

            call "APCPLF42" (scr_dte$, prod_dte$, spec%, scr_dept$,      ~
                                       #1, #8, #3, #6, #7, #9, #11, #12 )
            return
                                                 /* (EWD003)           */
REM       print_rpt_4a                             /* (PAR001) (AWD050)    */
REM            spec% = 0%                           /* Dept 006 Cut Sheets */
REM            call "AWDPLN42" (scr_dte$, prod_dte$, spec%, scr_dept$,      ~
REM                                    #1, #8, #3, #6, #7, #9, #11, #12 )
REM            return
                                                 /* (PAR001)            */

        print_slow_lineals                       /* (AWD040)            */
            call "SHOSTAT" ("PRINT SLOW LINEALS")
            call "AWDPLQ42" (scr_dte$, prod_dte$, spec%, scr_dept$,      ~
                                       #1,    /* APCCUTWK */             ~
                                       #3,    /* GENCODES */             ~
                                       #6,    /* AMTBOMIF */             ~
                                       #7,    /* HNYMASTR */             ~
                                       #11,   /* APCCUTEQ */             ~
                                       #12 )  /* AMTBOMCD */
               return

        print_rpt_5                              /* (PAR002)            */
            if dept% =  7% then spec% = 9%         /* CR2985            */
            if dept% = 36% then spec% = 2%
            if dept% = 25% then spec% = 3%         /*  (EWD024)         */
            if dept% = 26% then spec% = 4%         /*  (PAR002)         */

            if dept% = 27% then spec% = 5%         /*  (EWD027)         */
            if dept% = 28% then spec% = 6%         /*  (EWD027)         */
            if dept% =  6% then spec% = 7%         /*  (AWD050)         */
            if dept% = 19% then spec% = 8%         /*  (AWD050)         */
            if dept% = 53% then spec% = 9%         /*  (AWD051)   2803      */
            call "APCPLG42" (scr_dte$, prod_dte$, spec%, #1, #8, #3, #6, ~
                                            #7,#9, #11, #12 )
            return
        print_rpt_6
            call "APCPLH42" (scr_dte$, prod_dte$, #1, #8, #3, #6, #7, #9,~
                                                  #11, #12 )
        return
        
        print_ntx

            call "NTXLIN01" (scr_dte$, prod_dte$, spec%, scr_dept$,      ~
                             #1, #3, #6, #7, #9, #11, #12 )

            if dept% = 2% or dept% = 3% then goto NTX_Shapes                                                          
        return  
        NTX_Shapes
REM            spec% = 2%
REM            call "APCPLE42" (scr_dte$, prod_dte$, spec%, #1, #3, #6,     ~
                                                       #7, #9, #11, #12 )  
                                                             
            call "AWDPLH42" (scr_dte$,      /* Production Date           */ ~
                             comp_dte$,     /* Completion Date           */ ~
                             scr_shft$,     /* Shift Selection           */ ~
                             scr_load$,     /* Screen Load Selection     */ ~
                             scr_dept$,     /* Screen Dept               */ ~
                             #63,           /* bcksubpt <SR73216>        */ ~
                             #4,            /* (AWDSPECB) Shapes Bending */ ~
                             #3,            /* (GENCODES) Master Tables  */ ~
                             #9 )           /* (TXTFILE ) Text File      */     
        return                                           

        sort_product                            /* Tube Winding Report */
            if dt_prt$ = "Y" then w1$, w2$, w3$ = " "
            sort_key$ = all(hex(00))
                                                          /* (PAR001)  */
            str(sort_key$,1%,5%) = wrk_key$               /* SORT CODE */
            put #8, using L64130, sort_key$,dt_seq$, dt_load$, dt_part$,  ~
                                 dt_ref$, w1$, w2$, w3$, dt_sub_part$,    ~
                                 dt_sub_info$, " "
L64130:          FMT CH(25), CH(5), CH(5), CH(25), CH(8), 3*CH(3), CH(20),~
                     CH(20), CH(11)
                                                          /* (PAR001)  */
        return

        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#1,mode$, 500%, f2%)
            if f2% <> 0% then goto L64580
            call "WORKOPN2" (#8,mode$, 500%, f2%)
            if f2% <> 0% then goto L64600
        return
L64580:     call "SHOSTAT" ("ERROR - CANNOT OPEN (APCCUTWK)") : stop
        return
L64600:     call "SHOSTAT" ("ERROR - CANNOT OPEN (APCCUTW2)") : stop
        return
        delete_work
            call "FILEBGON" (#1)
            call "FILEBGON" (#8)
        return
                                                         /* (PAR001)    */
        lookup_sub_part
            init(" ") bcksubpt_rec$, flds$(), info_flds$(),           ~
                      dt_sub_part$, dt_sub_info$
            dim1es, dim2es, dim3es = 0.00       /* (AWD048)          */
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

/* (AWD048) */          
            get bcksubpt_rec$ using dimFMT, dim1es, dim2es
dimFMT:               FMT POS(153), PD(14,4), PD(14,4)
             
            if err1% = 0% then goto no_subpart_error             
               str(bcksubpt_rec$,48%,20%) = "00000000000000000000"
               str(bcksubpt_rec$,132%,20%) = "00000000000000000000"
               
no_subpart_error:
               dt_sub_part$ = str(bcksubpt_rec$,48%,20%)

               dt_sub_info$ = str(bcksubpt_rec$,132%,20%)

               if err1% = 0% then return

            return

                                                         /* (PAR001)    */
LNTXLineal:
		if dept% = 59% or dept% = 58% then goto L65000    /*(AWD052)*/
		if dept% = 057% then goto L65000    			  /*(SR73949)*/
		if dept% = 015% then goto L65010                  /*(AWD053)*/
		if dept% = 016% then goto L65020                  /*(AWD053)*/
		if dept% = 045% then goto L65030                  /*(SR65694)*/	
		if dept% = 012% then goto L65040                  /*(SR65695)*/	
        if dept% = 031% then goto L65050                  /*(CR1482) */
        if dept% = 041% then goto L65050                  /*(CR1482) */
REM     if dept% = 070% then goto L65050           /*(CR1482) (CR2005)*/
        if dept% = 071% then goto L65055                  /*(CR1722) */        
		
		return
L65000:                                                   /* (AWD052)  */
REM        if dept% <> 57% or dept% <> 58% then goto L61950
               scr_prod$ = "5"                            /* (PAR000)  */
                                                          /* Dept = 058,059*/
               call "NTXPLB42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
               return
                                                          /* ((AWD052))  */
                                                          
L65010:                                                   /* (AWD053)  */
               scr_prod$ = "5"                            /* (PAR000)  */
                                                          /* Dept = 015*/
               call "NTXPLC42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
               return
                                                          /* ((AWD053))  */

L65020:                                                   /* (AWD053)  */
               scr_prod$ = "5"                            /* (PAR000)  */
                                                          /* Dept = 015*/
               call "NTXPLD42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
               return
                                                          /* ((AWD053))  */

L65030:                                                   /* (SR65694) */
               scr_prod$ = "5"                            /* (PAR000)  */
                                                          /* Dept = 015*/
               call "NTXPLE42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
               return
                                                          /* ((SR65694))  */
                                                          
L65040:                                                   /* (SR65695) */
               scr_prod$ = "5"                            /* (PAR000)  */
                                                          /* Dept = 012*/
               call "NTXPLF42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
               return
                                                          /* ((SR65695))  */
                                                                            

L65050:                                                   /* (CR1482) */
               scr_prod$ = "3"                            /* (PAR000)  */
                                                          /* Dept = 012*/
               call "NTXPLG42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
               return
                                                          /* ((CR1482))  */  
L65055:                                                   /* (CR1722) */
               scr_prod$ = "5"                            /* (PAR000)  */
                                                          /* Dept = 012*/
               call "NTXPLH42" ( size%,                   /* BATCH SIZE*/~
                                 sched%,                  /* Start Sch */~
                                 scr_dte$,                /* Prod Date */~
                                 scr_dept$,               /* DEPARTMENT*/~
                                 scr_prod$,               /* Prod Line */~
                                 scr_load$,               /* Prod Load */~
                                 lk_fn$(),                /* 1,2 Lk Fin*/~
                                 #1,                      /* APCCUTWK  */~
                                 #3,                      /* GENCODES  */~
                                 #11,                     /* APCCUTEQ  */~
                                 #12,                     /* AMTBOMCD  */~
                                 #6 )                     /* AMTBOMIF  */
               return
                                                          /* ((CR1722))  */                                                          
        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")
        end
