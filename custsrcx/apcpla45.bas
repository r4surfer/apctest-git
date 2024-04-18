        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLA45                             *~
            *  Creation Date     - 11/18/98                             *~
            *  Last Modified Date- 02/14/2020                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - This Program Creates and Updates     *~
            *                      the 'Glass Database' for the new     *~
            *                      Glass Remake System. This Utility    *~
            *                      also Explodes the Glass on the       *~
            *                      Screen.                              *~
            * x$ = bin(35%,1)      STUFF Pound symbol into X$           *~
            *                                                           *~
            *  File Naming - Description                                *~
            *  -----------   -----------------------------------------  *~
            *    Work        <APCPLNWK> - Used for Sorting Glass        *~
            *    Labels      MMDD@<Dpt> or <ALL> - Regular Labels       *~
            *                MMDD@<RMK> - For All Remake Labels         *~
            *                MMDD@<RM1> - For Remakes Loads     (EWD030)*~
            *                MMDD@<RM2> - No Longer Used        (EWD030)*~
            *                MMDD@<TMP> - Special and Tempered Glass    *~
            *                                                           *~
            *    Batches     @GLSGED@ = GED, @GLSBIL@ = Bilco - Regular *~
            *    (All )      @RMKGDK@ = GED, @RMKBLK@ = Bilco - Remake  *~
            *    (Loads)     @RMKGD1@ = GED, @RMKBL1@ = Bilco - Remake  *~
            *    (Not Used)  @RMKGD2@ = GED, @RMKBL2@ = Bilco - Remake  *~
            *                                                           *~
            *New(APCPLC45) - Subroutine for Creating Glass File Using   *~
            *   (APCGS5SB)   the GED Sort. The file is created in       *~
            *                the Bilco Glass Format and Processed by    *~
            *                the Bilco Glass Optimizer.                 *~
            *                                                           *~
            *New(APCPLB45) - Subroutine for Creating Glass File which   *~
            *   (APCGS4SB)   will be Processed by the GED Glass         *~
            *                Optimizer. All Quantities are One (1) in   *~
            *                Item Record.                               *~
            *New(APCPLD45) - Subroutine for Analyzing both Scheduled    *~
            *                and completed glass. Also checks Re-Make   *~
            *                Glass.                                     *~
            *                Item Record.                               *~
            *                                                           *~
            *NEW(APCPLE45) - Subroutine to create 2nd Windowmate file   *~
            *                @GLSBLB@. This batch file contains Depart. *~
            *                defined in GLASS12 Table.                  *~
            *                                                   (EWD058)*~
            *NEW(APCPLF45) - Subroutine to created Special Shape Glass  *~
            *                Bridge file @SHAPE2@ for PMC Glass         *~
            *                Cutter. When data entered through Selection*~
            *                (5) from Menu.                             *~
            *                                                           *~
            *NEW(APCPLG45) - Subroutine to create Special Shape Glass   *~
            *                Bridge file @SHAPE2@ for PMC and @SHAPE3@  *~
            *                for Custom Glass. Data Created from Part   *~
            *                number when regular glass is run.          *~
            *                                                           *~
            *NEW(EWDCALSS) - Subroutine to Calculate the Special Shape  *~
            *                Glass cut sizes. Has all of the Special    *~
            *                Shape Equations. Special adjustment for    *~
            *                Brick Mull.                                *~
            *                                                           *~
            *                                                   (EWD058)*~
            *New(EWDPLN72) - New Program and Subroutine for printing,   *~
            *                only Glass and remake labels on the Zebra  *~
            *                printers. Uses subroutine (EWDPLA72) to    *~
            *                print labels. Subroutine (EWDPLD72) used   *~
            *                for Special Shape Glass Labels.            *~
            *                                                           *~
            *                                                   (EWD059)*~
            *New(EWDCALSL) - Subroutine to Calculate the Special Shape  *~
            *                Lineal cus sizes and the Angles. All the   *~
            *                equations for shapes. New database         *~
            *                'AWDSPECB' Shape Fields 'BHLR12' 'ABCD'    *~
            *                                                           *~
            * GED Glass Tables                                          *~
            *            (GED XXX  ) - GED Glass System Model Codes     *~
            *            (GED X0   ) - GED GLASS MODELS                 *~
            *                                                           *~
            *            (GED X1   ) - BILCO GLASS MODELS (GED SORT)    *~
            *                                                           *~
            *            (GED 000  ) - GED Spacer Id's Table            *~
            *                          Field #2 Header, No Decimal      *~
            *            (GED 001  ) - GED Glass Type Code Table        *~
            *                          (00-29) - Single Strength        *~
            *                          (50-79) - Double Strength        *~
            *                           Note - DS Pos (16) Double (Only)*~
            *            (GED 002  ) -  GED Glass Thickness & Spacer    *~
            *                           Pos ( 1 - 6) = Thickness        *~
            *                           Pos ( 9 - 6) = Single Strength  *~
            *                           Pos (17 - 6) = Double Strength  *~
            *                           Note - Decimal Included         *~
            *            (GED 003  ) -  GED Glass Liting/Grid Cross Ref *~
            *                           Pos (1 - 10) Follwed by "-" and *~
            *                                    Long Description       *~
            *            (GED 004  ) - GED Width and Height Adjustment  *~
            *                          Pos (1 - 6) = Width Adjustment   *~
            *                          Pos (8 - 6) = Height Adjustment  *~
            *            (GED 005  ) - GED Glass Special Shape Codes    *~
            *                          Note - Default Code              *~
            *                           "1-STD" = Standard Rectangle    *~
            * System Glass Tables                                       *~
            *            (GLASS01  ) - GLASS QUANTITIES FOR TOP/BOT     *~
            *            (GLASS02  ) - TABLE FOR TOP GLASS ONLY         *~
            *            (GLASS03  ) - TABLE FOR TWO BOTTOMS            *~
            *            (GLASS04  ) - Table for no Glass Required      *~
            *            (GLASS05  ) - TABLE FOR '8' SERIES WITH GLASS  *~
            *  Not Used->(GLASS06  ) - TABLE TO SKIP NO DETAIL LABELS   *~
            *  Not Used->(GLASS07  ) - TABLE FOR SLIDERS                *~
            *            (GLASS08  ) - TABLE - SPECIAL SHAPES NO GLASS  *~
            *            (GLASS09  ) - TABLE - DEPT'S EXCLUDE FROM BILCO*~
            *            (GLASS10  ) - TABLE - CLMR Specified Top-Bot   *~
            *            (PLAN TEMP) - All Valid Temered Glass Codes    *~
            *            (PLAN DBLE) - All Double Strength Glass Codes  *~
            *            (PLANGLOUT) - Skip Glass, Purchased Outside    *~
            *                          for the specified 'LITING' Codes.*~
            *            (PLANARGON) - Check Table for Glass Codes with *~
            *                          Argon Gas. Table only has the    *~
            *                          Glass Codes with Argon Gas. (G1) *~
            *                          is the GED Code for Argon.       *~
            *            (PLANKRYPT) - Check Table for Glass Codes with *~
            *                          KRYPTON Gas. Table only has the  *~
            *                          Glass Codes with KRYPTON Gas.(G2)*~
            *                          is the GED Code for KRYPTON.     *~
            *            (PLAN REMK) - Reason Codes for Remake Glass    *~
            *  (EWD028)  (GLASS11  ) - Table with Department and Table  *~
            *                          Number. Bilco Glass Only         *~
            *                                                           *~
            *  (EWD029)  (GLASS12  ) - Table with Department and Spacer *~
            *                          in Decimal format. If in table   *~
            *                          will place in the (B) file       *~
            *                                                           *~
            *  (EWD031)  (PLNCONFIG) - Table with Special Shape         *~
            *                          Configuration Codes              *~
            *                                                           *~
            *            (PLNCNFIG ) - Table with Prompt Text for each  *~
            *                          Screen field.                    *~
            *                                                           *~
            *            (PLN GLASS) - Special Shapes Glass Code with   *~
            *                          Sandwich in the description      *~
            *                                                           *~
            *            (PLNCNFIGM) - MMM=Model,CC=Special Shape Config*~
            *                          SS=Sequence or occurence         *~
            *                          Used to find Specified Config.   *~
            *                          associated with model            *~
            *                                                           *~
            *  (EWD042)  (SHPHFOFF ) - P.PPPP=Offset amount of frame to *~
            *                          deduct from radius and height of *~
            *                          HALF ROUND ONLY.                 *~
            *                          G.GGGG=Offset amount of glazing  *~
            *                          bead to deduct from radius and   *~
            *                          height of HALF ROUND ONLY.       *~
            *                                                           *~
            *  (EWD031)   sh_flags(10%)3                                *~
            *                          (1%,1%) = Data Entered Y or N    *~
            *                          (2%,1%) = Field Entered W-H-R-D  *~
            *                                    Y-N                    *~
            *                          (3%,1%) = Field Calculated Value *~
            *                                    Width, Height, Radius  *~
            *                                    Diameter,              *~
            *                                                           *~
            *  (EWD058)   (PLAN SHAP)  Special Shape Model Codes        *~
            *                          <MMM> = Model                    *~
            *                                                           *~
            *             (SHAPCROSS)  Shape Cross Reference with 25    *~
            *                          digit part number. <LH>          *~
            *                          L = 1st Digit of Liting Code     *~
            *                          H = 2nd Digit of Hinge Code      *~
            *                          Description = xx Shape Code      *~
            *                                                           *~
            *             (PLN SSORT)  Shape Glass Sort Code            *~
            *                          1<GC> GC = Glass Code            *~
            *                          2<C>  C  = Color Code            *~
            *                          3<L>  L  = Lock Code             *~
            *                          Descr = "00 to 99"               *~
            *                          "00" = Default Sort Seq.         *~
            *                                                           *~
            *             (SHAP CUST)  Special Shape Codes for Sending  *~
            *                          information to Custom. The Data  *~
            *                          for Shape Code will be in        *~
            *                          @SHAPE3@                         *~
            *                                                           *~
            *             (PLN SSGRID) Special Shapes Adjustments for   *~
            *                          Grid. Used in conjunction with   *~
            *                          AUTO LISP Grid alignment Program *~
            *                                                           *~
            *             (PLN FACE)   Special Shape Glass Facing table *~
            *                          used put the Facing Left or Right*~
            *                          on the Shapes Glass label. It is *~
            *                          based on Shape Code and Glass    *~
            *                          code.                            *~
            *                                                           *~
            *  (EWD070)   (SHAPESCAN)  Special Shapes Receiving Custom  *~
            *                          Glass Time Windows. 0 thru 9     *~
            *                                                           *~
            *  (EWD070)   (PLANGLASS)  Glass Tracking Codes. Added two  *~
            *                          Codes to track Custom Glass      *~
            *                          '7' Custom Glass, '8' Custom     *~
            *                          Remake Glass.                    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *  Top Glass Type  Phantom      Bot Glass Type  Phantom     *~
            *  - Standard        2008       - Standard        3008      *~
            *  - Std Oriel       2203       - Std Oriel       3203      *~
            *  - Std Cottage     2103       - Std Cottage     3103      *~
            *  - Picture         2008                                   *~
            *  - H.S & Patio     2008       - H.S. Patio      3008      *~
            *                               - Hopper          3008      *~
            *  - 1/3-1/3-1/3     2108       - 1/3-1/3-1/3     3108      *~
            *  - 1/4-1/2-1/4     2008       - 1/4-1/2-1/4     3008      *~
            *  - Spec. Oriel     20xx       - Spec. Oriel     30xx      *~
            *  - Spec. Cottage   20xx       - Spec. Cottage   30xx      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * RM_FLAG% DEFINITIONS                                      *~
            *    production                     = 0%                    *~
            *    tempered                       = 2%                    *~
            *    production remakes             = 1%                    *~
            *    tempered remakes               = 3%                    *~
            *    laminate                       = 4%                    *~
            *-----------------------------------------------------------*~
            * TEMPERED SPTYPE% DEFINITIONS                              *~
            *    single strength                spType% = 1%            *~
            *    double strength                spType% = 2%            *~
            *    tempered double strength       spType% = 3%            *~
            *    SDL Single strength            spType% = 4%            *~
            *    SDL Double strength            spType% = 5%            *~
            *    SDL Temp Double strength       spType% = 6%            *~
            *    Triple Strength > 25SqFt 3/16  spType% = 7%            *~
            *    Door 5/32 Triple Strength      spType% = 8%            *~
            *    Laminate Glass                 spType% = 9%            *~
            *    Laminate Strengthened Glass    spType% =10%            *~
            *    Single Glazed Tempered         spType% =11%            *~
            *    Laminate 5/32                  spType% =12%            *~
            *    Laminate Strengthened 5/32     spType% =13%            *~
            *    Laminate 3/16                  spType% =14%            *~
            *    Laminate Strengthened 3/16     spType% =15%            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/18/96 ! New Program for (APC) - LAST MOD DATE    ! RHH *~
            *          !   Contains all the Modification that are !     *~
            *          !   in APCGLS05, and the following         !     *~
            *          ! Special Mod to Create a New Sort order   !     *~
            *          ! Uses Priority Codes 1% to 50%. Dept,Glass!     *~
            *          ! Strength, and Spacer are the Elements.   !     *~
            * 10/06/96 ! New Planning Version of Glass            ! RHH *~
            * 01/03/97 ! Mode for UPS and Back Orders Modes to    ! RHH *~
            *          !   RM_NUM$. GOSUB LOOKUP_SO               !     *~
            * 01/28/97 ! Add new Barcode field to Label           ! JBF *~
            * 02/12/97 ! Mode for Change GLASS_DTE$ to Glass Prod ! RHH *~
            *          !   Date. Date Glass is Made and Completed !     *~
            * 04/08/97 ! Mods to change the sort for remake glass ! RHH *~
            *          !   Disabled Sort Code line (60240) also   !     *~
            *          !   Removed Seq No. from Sort 60256,60272  !     *~
            * 06/04/97 ! Mods to add two (2) new Departments to   ! RHH *~
            *          !   the sort. (049) and (052).             !     *~
            * 06/12/97 ! Mods to also change error code to '51'   ! RHH *~
            *          !   and "E" this affects both Subs.        !     *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 01/14/98 ! Mod to (APCPLB45) and (APCPLC45) to Add  ! RHH *~
            *          !   new Subroutine Called 'CHECK_GLASS'.   !     *~
            *          !   New Tables (PLANGLOUT) and (PLANARGON) !     *~
            *          !   LITING Codes and Glass Argon Gass Codes!     *~
            * 02/02/98 ! Mod to Skip Liting Code = 88% at         ! RHH *~
            *          !   BUILD_GED_KEY                          !     *~
            * 02/26/98 ! Mod to correct Label TEXT$() for Glass   ! RHH *~
            *          !   Label Printing.                        !     *~
            * 03/16/98 ! Mods to fix Sandwich problem in BILCO    ! RHH *~
            *          !   glass 'APCPLC45'. Also created new     !     *~
            *          !   report for Special Tempered Glass      !     *~
            * 03/24/98 ! Y2K                                      ! LDJ *~
            * 04/07/98 ! Correction made to GEN_RPT1 to fix Lookup! RHH *~
            *          !   must use read next logic because of Dup!     *~
            * 04/10/98 ! Mod to use New Table with the Constant   ! RHH *~
            *          !   values used to calculate the height    !     *~
            *          !   adjustment for Specified CLMR.         !     *~
            * 06/04/98 ! (EWD001) Mod to Split Remake Glass into  ! RHH *~
            *          !   Two (2) Groups. Remakes and Glass House!     *~
            * 06/18/98 ! (EWD002) Mod to Stock- Clear (01) Glass  ! RHH *~
            *          !   glass only. Type (01).                 !     *~
            * 07/27/98 ! (EWD003) Fix Special Liting Problem put  ! RHH *~
            *          !   liting codes between (83% - 88%) at the!     *~
            *          !   with sort code '049'. Also mods for    !     *~
            *          !   special Tempered Glass Report. Create  !     *~
            *          !   labels for Tempered Glass 'MMDD@TMP'   !     *~
            *          !   Mods to Sub (gen_rpt1).                !     *~
            * 10/16/98 ! (EWD004) Mod to create new sort sequence ! RHH *~
            * 12/07/98 ! (EWD005) Mods for Glass Re-make System   ! RHH *~
            *          !   put (IND) on glass label for Indy's    !     *~
            * 01/11/99 ! (EWD006) Differnt Label File Names       ! RHH *~
            *          !   Mod to Purge to Save Stock             !     *~
            * 02/23/99 ! (EWD007) Mod to add New Departments to   ! RHH *~
            *          !   glass sort.                            !     *~
            * 03/25/99 ! (EWD008) Mod to add New Special Glass    ! RHH *~
            *          !   utility (EWDPLN66) to Eliminate Manual !     *~
            *          !   labels.                                !     *~
            * 04/06/99 ! (EWD009) Mod for new special Liting Code ! RHH *~
            *          !   assoc. with new products.  Temporary   !     *~
            *          !   put at the end. Code 'A0'              !     *~
            * 04/12/99 ! (EWD010) Mod to (APCPLD45) for new glass ! RHH *~
            *          !   oven tracking. Using Id (OV?)          !     *~
            * 04/27/99 ! (EWD011) Mod for Glass Code "E7"         ! RHH *~
            * 07/19/99 ! (EWD012) Mod to print Glass Re-Make Label! RHH *~
            *          !   on Zebra Printer. Use remake_label% to !     *~
            *          !   turn-off old label format.             !     *~
            * 08/11/99 ! (EWD013) Mod to Correct problem with new ! RHH *~
            *          !   model (121). Problem is that no sort   !     *~
            *          !   code was found. Alos mod was made to   !     *~
            *          !   (APCPLB45) to support Grid Bumpers.    !     *~
            * 09/21/99 ! (EWD014) Mod to EWD012 to print all glass! RHH *~
            *          !   labels on Zebra printers.              !     *~
            * 09/27/99 ! (EWD015) Mod to Fix probelem with Cot/Or ! RHH *~
            *          !   when Wood Surround is present.         !     *~
            * 11/09/99 ! (EWD016) Mod to Fix problem with Double  ! RHH *~
            *          !   strength glass for new 'Alpha' Codes   !     *~
            * 12/17/99 ! (EWD017) Mod to Add products to Glass    ! RHH *~
            *          !   sort for new product lines.            !     *~
            * 01/21/00 ! (EWD018) Mods for Samp/Disp/Lit          ! RHH *~
            *          !   lit_flg$                               !     *~
            * 03/29/00 ! (EWD019) Mods to Glass Sort for Dept 007 ! RHH *~
            * 04/24/00 !   Additional Mods to (EWD018) for new    ! RHH *~
            *          !   sample code (027) parts (012 thru 026) ! RHH *~
            * 05/23/00 ! (EWD020) Mods for New Glass codes to     ! RHH *~
            *          !   rebuild the Sandwich.                  !     *~
            * 06/29/00 ! (EWD021) Mods for Stock Glass            ! RHH *~
            * 07/12/00 ! (EWD022) Take out glass type '04'        ! CMG *~
            * 08/21/00 ! (EWD023) Add Dept 36 stock glass         ! RHH *~
            * 08/31/00 ! (EWD024) Mod to make all glass double    ! RHH *~
            *          !          strength for department 047     !     *~
            *          !          and Department 033              !     *~
            * 09/29/00 ! (EWD025) Mod to Exclude depts from the   ! RHH *~
            *          !          Bilco Machine. (GLASS09) Table  !     *~
            * 09/29/00 ! (EWD026) Mod to make stock glass model   ! RHH *~
            *          !          Specific. New Table (GLASS-GED) !     *~
            *          !          Stock Size  and Model Code      !     *~
            * 12/06/00 ! (EWD027) Mods for Special glass codes    ! RHH *~
            *          !          set-up special sandwich.        !     *~
            * 02/20/01 ! (EWD028) Mods for Hegla Table Changes    ! RHH *~
            *          !          New Glass Table (GLASS11  )     !     *~
            * 05/04/01 ! (EWD029) Mods for running two copies of  ! RHH *~
            *          !          WINDOWMATE for the Bilco cutters!     *~
            *          !          new glass table (GLASS12  )     !     *~
            *          !          New Subroutine (APCPLE45)       !     *~
            * 06/21/01 ! (EWD030) Mods to Remake glass system to  ! RHH *~
            *          !          build batch based on load       !     *~
            *          !          priority.                       !     *~
            * 08/20/01 ! (EWD031) Mods for Phase (2) of Speacial  ! RHH *~
            *          !          Shapes. Screen entry of Shapes  !     *~
            *          !          Calculate and create a batch.   !     *~
            * 08/20/01 ! (EWD032) Mods to change the glass sort.  ! RHH *~
            *          !          also modify the order of tempered!    *~
            * 10/08/01 ! (EWD033) Mods to add lookup and edit     ! CMG *~
            *          !          screen for special shapes       !     *~
            * 11/30/01 ! (EWD034) Mode to change value from .414308!RHH *~
            *          !          to .413                         !     *~
            * 12/10/01 ! (EWD035) Mode for New 411 and Dept 50    ! RHH *~
            * 12/11/01 ! (EWD036) Mods for New 411 and Dept 50    ! CMG *~
            * 03/28/02 ! (EWD037) Mods for 'GLASS-GED' to only    ! CMG *~
            *          !            control BILCO                 !     *~
            * 05/22/02 ! (EWD038) Mods for New 441 and 451.       ! CMG *~
            * 06/01/02 ! (EWD039) Mod change company name         ! CMG *~
            * 06/24/02 ! (EWD040) Mod to turn on Special Shape    ! RHH *~
            *          !          liting codes.                   !     *~
            * 09/17/02 ! (EWD041) Fix for Special Shapes Grid Code! CMG *~
            * 09/23/02 ! (EWD042) Mod for New Half Round Dry Bend ! CMG *~
            *          !          Equations.                      !     *~
            * 11/01/02 ! (EWD043) Mod for New Sun Clean Glass Sort! CMG *~
            * 11/06/02 ! (EWD044) Mod for new patio door models.  ! CMG *~
            * 11/06/02 ! (EWD045) Mod for New Cont. Head & Sill   ! CMG *~
            * 12/02/02 ! (EWD046) Mod for Dept 51 Glass Sort.     ! CMG *~
            * 12/02/02 ! (EWD047) Mod to increase array size from ! CMG *~
            *          !          100 to 200.                     !     *~
            * 01/22/03 ! (EWD048) Mod to add glass sort for dept  ! CMG *~
            *          !          '025' & '026' and add new lookup!     *~
            *          !          for cont. head/sill products    !     *~
            * 02/17/03 ! (EWD049) Mod for new slider models 120 & ! CMG *~
            *          !          130.                            !     *~
            * 03/06/03 ! (EWD050) Mod for new grid code 58.  Prair! CMG *~
            *          !          on top and clear on bottom.     !     *~
            * 05/07/03 ! (EWD051) Mod to allow numeric or alpha   ! CMG *~
            *          !          load numbers.                   !     *~
            * 05/23/03 ! (EWD052) Mod for new 3/4' Grid.  Put 'W' ! CMG *~
            *          !          on Gls Label.                   !     *~
            * 09/15/03 ! (EWD053) Mod to cleanup sort for remake  ! RHH *~
            *          !          Glass. Production glass 1st,    !     *~
            *          !          (1-8), 2nd Glass House (11-18)  !     *~
            * 09/17/03 ! (EWD054) Mod not to exclude glass from   ! CMG *~
            *          !          department '104'.               !     *~
            * 09/15/03 ! (EWD055) Mod to for tempered procedures. ! CMG *~
            * 11/18/03 ! (EWD056) Mod to turn on all Special Shape! RHH *~
            *          !          equations and change S.S Labels !     *~
            * 12/08/03 ! (EWD057) Mod to for sort changes.        ! CMG *~
            * 02/16/04 ! (EWD058) Final changes for Custom Glass  ! RHH *~
            *          !          Special change to 'EWDCALSS' for!     *~
            *          !          Birck Mull Models (553,554)     !     *~
            * 03/31/04 ! (EWD059) New database and Calc for the   ! RHH *~
            *          !          Special Shapes bending report   !     *~
            *          !          New Sub 'EWDCALSL' Calc the     !     *~
            *          !          Lineals and Angles for Bending  !     *~
            * 05/11/04 ! (EWD060) Mod to turn on Special Shapes   ! RHH *~
            *          !          with all other Glass. Also,     !     *~
            *          !          assign Glass barcode to Special !     *~
            *          !          and update the Glass Database   !     *~
            *          !                                          !     *~
            * 05/18/04 ! (EWD061) Mod for vinyl line remake reason! CMG *~
            *          !          code that can be used in the    !     *~
            *          !          glass department.               !     *~
            * 05/24/04 ! (EWD062) Mod for Prairie Grid Calculation! CMG *~
            * 06/01/04 ! (EWD063) Mod to put specials back on the ! CMG *~
            *          !          rack labels-so they have to be  !     *~
            *          !          in the glass database           !     *~
            * 06/22/04 ! (EWD064) Mod to add additional comments  ! CMG *~
            *          !             to GLSGED file               !     *~
            * 07/29/04 ! (EWD065) Mod to correct the 'OB' Glass   ! RHH *~
            *          !          problem                         !     *~
            * 08/06/04 ! (EWD066) Mode to APCPLG45 to change the  ! RHH *~
            *          !          format of the production date   !     *~
            *          !          in the Custom Glass bridge file.!     *~
            * 08/06/04 ! (EWD067) Mode for remake glass to add a  ! RHH *~
            *          !          'T' Selection for Triples. When !     *~
            *          !          there will be no glass Calc.    !     *~
            *          !          for Radius. 'Y' only calcs.     !     *~
            *          !          Radius .'N' Calcs Everything.   !     *~
            *          !          (RHH-SPECIAL) Marked            !     *~
            * 08/06/04 ! (EWD068) Mod to 'APCPLG45' To not send   ! RHH *~
            *          !          Triple Eyebrow Glass to Custom  !     *~
            *          !          Grid Codes beginning with 'T,U' !     *~
            * 08/10/04 !          Add all special Shape changes to! RHH *~
            *          !          the current production version  !     *~
            * 08/10/04 ! (EWD069) Mod to record length of @SHAPE3@! RHH *~
            *          !          Add three new fields to spread  !     *~
            *          !          sheet for remake and Pricing    !     *~
            *          !          add new file @SHAPE4@ for remake!     *~
            *          !          glass.                          !     *~
            * 09/14/04 ! (EWD070) Mods for receiving Special Glass! RHH *~
            *          !          and chages to the Glass reports.!     *~
            *          !          Special Shape remake glass.     !     *~
            * 09/14/04 ! (EWD071) Mods for new Special Shapes re- ! RHH *~
            *          !          Make glass processing.          !     *~
            * 11/08/04 ! (EWD072) Mode for New Custom Database    ! RHH *~
            *          !          file with pricing information.  !     *~
            *          !          Mod to APCPLG45 11-03-04        !     *~
            * 12/10/04 ! (AWD073) Mod to new in house rmk codes   ! CMG *~
            * 12/10/04 ! (AWD074) Mod to put back selection to only!CMG *~
            *          !    pull vinyl line remakes               !     *~
            * 02/07/05 ! (AWD075) Mod for pre-cut glass           ! CMG *~
            * 03/03/05 ! (AWD076) Mod for stock glass             ! CMG *~
            * 03/29/05 ! (AWD077) Mod to Change Glass Calc for    ! RHH *~
            *          !          Ellipticals for Brick Mold Model!     *~
            *          !          558, 559. Also turn on New Half !     *~
            *          !          grid codes HE, HF, HG           !     *~
            * 03/30/05 ! (AWD078) Precut can not be dept 033      ! CMG *~
            * 05/16/05 ! (AWD079) Mods to rearrange gls sort to so! CMG *~
            *          !   DS is sorted together to put in batchs !     *~
            *          !   together                               !     *~
            * 05/19/05 ! (AWD080) Mod to add UPS to remake label  ! CMG *~
            * 06/26/05 ! (AWD081) Mod to add flag to combine or   ! CMG *~
            *          !     split DS batches                     !     *~
            * 10/17/05 ! (AWD082) Mod to Fix Remake Number Problem! RHH *~
            *          !     for Special Shapes.                  !     *~
            * 11/02/05 ! (AWD083) CR347 Mod for sub part number   ! CMG *~
            * 11/08/05 ! (AWD084) Mod for new DS over OBS gls code! CMG *~
            * 01/31/06 ! (AWD085) CR347 Add Sales Order Line to   ! RHH *~
            *          !     Special Shapes Label File            !     *~
            * 03/19/06 ! (AWD086) Mods for Krypton                ! CMG *~
            * 03/19/06 ! (AWD087) MOD FOR 104 NOW NE CROSS DOCK DEPT!CMG*~
            * 03/19/06 ! (AWD088) mods for NC sort and NE sort    ! CMG *~
            * 03/30/06 ! (AWD089) mods for new dept, dept 55      ! CMG *~
            * 05/22/06 ! (AWD090) mods for tempered glass         ! DES *~
            * 11/08/06 ! (AWD091) mods hub (for shapes)           ! DES *~
            * 02/25/07 ! (AWD092) mod for sp25 and new casement   ! CMG *~
            * 03/17/07 ! (AWD093) mod for hgl code for triples    ! CMG *~
            * 03/29/07 ! (AWD094) Mod for grid color              ! CMG *~
            * 03/29/07 ! (AWD095) mod for sample codes range      ! CMG *~
            * 05/03/07 ! (AWD096) mod for 18 mm grid              ! CMG *~
            * 11/07/07 ! (AWD097) mods for double obscure         ! CMG *~
            * 01/10/08 ! (AWD098) mods for casing/bullnose shapes ! CMG *~
            * 03/05/08 ! (AWD099) mods for SDL                    ! CMG *~
            * 04/23/08 ! (AWD100) mod for slider sort             ! CMG *~
            * 06/23/08 ! (AWD101) mod for shape sq footage        ! CMG *~
            * 07/02/08 ! (AWD102) mod for Infini-Lite SDL File    ! CMG *~
            * 02/04/09 ! (AWD103) mod for stock pre-cut glass -   ! CMG *~
            *                    to turn on function again        !     *~
            * 02/23/09 ! (AWD104) mod for 333 door two tops/one bo! CMG *~
            * 02/26/09 ! (AWD105) change sort to hex for up to 255! CMG *~
            *          !     and add flordia window sort          !     *~
            *05/21/2009! (AWD106) mod to flordia in own remake fil! CMG *~
            *07/16/2009! (AWD107) mod for florida dble str checks ! CMG *~
            *09/30/2009! (AWD108) mod to put 'BLINDS' on LBL      ! CMG *~
            *10/27/2009! (AWD109) mods for ultra intercept        ! CMG *~
            *03/10/2010! (AWD110) mods for patio kanban lookup    ! CMG *~
            *04/01/2010! (AWD111) mods for stock & custom patio   ! CMG *~
            *02/14/2011! (AWD112) mods for intercept types        ! CMG *~
            *03/29/2011! (AWD113) mod for casement models         ! CMG *~
            *10/26/2011! (AWD114) mod for liting TD & TP          ! CMG *~
            *11/16/2011! (AWD115) mod for new tempered process    ! CMG *~
            *12/08/2011! (AWD116) mod for different cutbacks      ! CMG *~
            *11/19/2012! (AWD117) mod for triple pane             ! CMG *~ 
            *11/19/2012! (AWD118) mod for valance                 ! CMG *~
            *02/27/2013! (AWD119) add sub_part for shps - foam    ! CMG *~
            *07/01/2013! (AWD120) OGO change ecn 2013-032         ! CMG *~
            *07/22/2013! (AWD121) mod for new shape dimensions    ! CMG *~
            *09/22/2014! (AWD122) mod for strengthened lamni      ! CMG *~
            *02/26/2015! (AWD123) mod for @GLSLAM@ file           ! CMG *~
            *04/10/2015! (IM7733) 130 CHS Twin & Triple           ! CMG *~
            *04/10/2015! (IM8011) mod tempered gls sort for SP09  ! CMG *~
            *05/18/2015! (IM8022) mod for glass remakes           ! CMG *~
            *06/04/2015! (SR64679) large ntx windows made duraseal! CMG *~
            *01/06/2016! (SR67154) Energy Star 2016 Changes       ! CMG *~
            *01/30/2017! (CR503) Mods for GED calc values         ! CMN *~
            *02/25/2017! (CR838) mod for 8 ft door                ! CMN *~
            *11/29/2017! (CR1173) mod for STC Larson Window       ! CMN *~
            *02/26/2019! (CR1767) modify to set up department 058 ! CMN *~
            *          !   as tt/bb                               !     *~
            *          ! (CR1938) modify to remove TT/BB for depts! CMN *~
            *          !    029 & 071 & 072                       !     *~
            *04/05/2019! (CR1974) remove ds override logic        ! CMN *~
            *04/16/2019! (CR1987) mod for Triple, Quad, STC glass ! CMN *~
            *04/16/2019! (CR1990) mod add ultra lookup removed    ! CMN *~
            *          !   from DT                                !     *~
            *06/03/2019! (CR2055) Remove TX Duralite Logic        ! CMN *~
            *07/03/2019! (CR2109) changes for Series 130 Operable ! CMN *~
            *          !          Shapes glass                    ! CMN *~
            *09/12/2019! (CR2232) Update Series 1100 PW dept from ! CMN *~
            *          !    dept 072 to 027                       !     *~
            *10/24/2019! (CR2303) changes for Series 150 Operable ! CMN *~
            *10/24/2019! (CR2305) changes for lowe barcode        ! CMN *~
            *10/30/2019! (CR2260) NC STC changes schema=1%        ! CMN *~
            *02/14/2020! (CR2411) TX dept 058 TB to TT/BB         ! CMN *~
            *02/15/2021! (CR2773) precut tempered                 ! CMN *~
            *03/11/2021! (CR2790) precut tempered label batch     ! CMN *~
            *03/10/2022! CR3046 Add glstype$ set to INDY process  ! RDB *~
			*09/30/2022! CR3166 GED file changes for Erdman       ! RDB *~
            *02/24/2023! CR3266 Change on large doors spacer      ! RDB *~
			*03/06/2023! CR3272 Restrict new large doors to model ! RDB *~
            *************************************************************

        dim bck_rec$256, bck_key$19,     /* FILE = AMTBOMCD            */~
            view$3, mode$5,              /* TOP/BOT                    */~
            model$3,                     /* Special Glass Models       */~
            tab$(10%)9,                  /* Glass Table Names          */~
            mod$(10%,999%)3,             /* STORE MODELS ASSOC (EWD047)*/~
            mod%(10%),                   /* NO. ENTRIES EACH TABLE     */~
            ws$(200%)9,                  /* Stock Sizes Width (EWD026) */~
            hs$(200%)8,                  /* Stock Sizes Height(EWD026) */~
            md$(200%)3,                  /* Stock Model Codes (EWD026) */~
            stk_desc$30, stock$9,        /* Stock Description  W & H   */~
            desc$32, sh_desc$128,        /* Description an Shape Warr  */~
            phantom$25, tmp_desc$30      /* Phantom Designator         */

        dim                              /* FILE = APCPLNDT            */~
            hub_key$29,type$15,          /* HUB key for apcplz45       */~
            hubcl_key$12,                /* key for EWDHUBCL           */~
            sh_hub1$10,                  /* HUB1                       */~
            sh_hub2$10,                  /* HUB1                       */~
            sh_hub3$10,                  /* HUB1                       */~
            ld_key$5,                    /* LOAD NUMBER                */~
            save_part$25,                /* PART NUMBER                */~
            dt_rec$256,                  /* Detail Record              */~
            dt_key$23,                   /* Primary Key - Load Number  */~
            dt_key1$57,                  /* Alt - Bar Code Key         */~
            dt_load$5, rm_ky$33,         /* Production Load No         */~
            dt_bar$18, rm_bar$9,         /* Production Barcode         */~
            dt_date$6, rm_num$3,         /* Production Date Planned    */~
            dt_ref$8,  rm_key$33,        /* New Warranty Number        */~
            dt_shft$2, rm_st$1,          /* Production Shift Code      */~
            dt_seq$5,                    /* Department Sequence No.    */~
            dt_cust$9,                   /* Customer Code              */~
            dt_part$25, dt_desc$32,      /* Part Number                */~
            dt_sash$1, or_hows$2,        /* Sash 0=No,1=Top,2=Bot,3=FGO*/~
            dt_prt$1,                    /* Part (Y)es or (N)o         */~
            dt_samp$1,                   /* Sample 0=No,1=Samp,2-Disp  */~
            dt_wood$3,                   /* W/S 000=N/A,Code Value     */~
            dt_special$10,               /* Special Flags              */~
            dt_txt$4,text$(2%)70,txt$40, /* Detail Text Id             */~
            dt_gls$1,                    /* Glass Yes or No            */~
            dt_dept$3,                   /* Plan Department            */~
            hg$2,                        /* HINGE CODE                 */~
            hgl$15,                      /* Hinge Code Left Discript   */~
            hgr$15,                      /* Hinge Code Right Descript  */~
            ty$2,                        /* Glass Type Code            */~
            ty_s$3,                      /* Glass Short Description    */~
            ty_l$30,                     /* Glass Long Description     */~
            cl$1, cl_s$2, cl_l$6,        /* Glass Code                 */~
            lt$2,                        /* Liting Code                */~
            l_lt$6,                      /* Liting Left Descr          */~
            r_lt$6,                      /* Liting Right Descr         */~
            sc$1,                        /* Screen Code                */~
            lk$1                         /* Lock Code                  */

        dim                              /* FILE - (APCPLNWK)          */~
            lab_key$62,                  /* (AWD112) readkey           */~
            lab_ged$66,                  /* GED Primary Key            */~
/*AWD083*/  lab_rec$(2%)223,             /* Label Rec Alt Key 1 (1-38) */~
            lab_fil$9,                   /* Label Filler Area          */~
            wd$7,                        /* Actual Width               */~
            ht$6,                        /* Actual Height              */~
            filler$24                    /* RECORD FILLER              */

        dim                              /* FILE - LABEL PRINT FILES   */~
            filename$8,                  /* File Name for Open         */~
            ff$8,                        /* Label Print File Name      */~
            library$8,                   /* Library Name = 'APCDATA'   */~
            volume$6,                    /* Volume Name = 'CARLOS'     */~
            a1$2, a2$7, a3$8, a4$10,     /*                            */~
            a5$3, a6$8, a7$10, a8$2,     /*                            */~
            a9$5, a10$16, a11$3, a12$3,  /*                            */~
            a13$40, a14$9,  a15$3, a16$6,/*                            */~
            a17$1, a18$9, a19$9,a20$9,   /*                            */~
            a21$9, a22$9, a23$3, a24$2,  /*                            */~
            a25$40,a26$2,a27$            /* Special Text for Shapes    */

                                         /* (EWD012)/(EWD014)          */
        dim gl_key1$12, gl_key$32,       /* Alt Key (1)                */~
            gl_process$1,                /* Glass Process Flag         */~
            gl_sort$5,                   /* Glass Sort Code            */~
            gl_num$3                     /* Glass Remake Number        */~
                                         /* (EWD012)/(EWD014)          */

        dim                              /* File - (APCPLNGR)          */~
            rm_dept$3, rm_seq$5,         /* Glass Remake Reports       */~
            rm_model$3, rm_load$5,       /*                            */~
            rm_gls$2, rm_mut$8,          /*                            */~
            rm_wd$7, rm_ht$6, rm_so$8,   /*                            */~
            rm_reason$2, rm_tot$10,      /*                            */~
            cust_tot$10,                 /* (EWD070) Received Cust Gls */~
            rm_reason_d$30,              /*                            */~
            title$30, company$25,        /*                            */~
            rpt_time$8, rm_cnt$37,       /*                            */~
/*AWD083*/  rm_rec$(2%)256               /*                            */

        dim                              /* (Program Variables)        */~
            lit_flg$1,                   /* Literature        (EWD018) */~
            size$4,                      /* Batch File Size            */~
            bg_date$10, bg_dte$10,       /* Beginning Prod Date        */~
            ed_date$10, ed_dte$10,       /* Ending Production Dte      */~
            rp_sel$1, rp_sel_d$30,       /* Glass Report Selections    */~
            sav_mod$3,                   /* Product Line Model         */~
            glass_dte$8,                 /* Glass Completion Date Form */~
            glass_dte1$8,                /* Glass Completion Date Unfor*/~
            sze$30,                      /* Save Eights                */~
            sz$100,                      /* Save Sixteenths            */~
            wd1$9,                       /* Calculated Width           */~
            wd2$9,                       /* CLMR FOR SCREEN            */~
            ht1$8,                       /* Calculated Height          */~
            readkey$30,                  /* GENCODESLook-Up Key        */~
            scr$(10%)40, rpt$(10%)40,    /* Screen Text Messages       */~
            scr_temp$(10%)70,            /* Screen Text Messages EWD054*/~
            scr_dte$8,                   /* Glass Production Date FORM */~
            scr_dte1$8,                  /* Glass Prod. Date Unform    */~
            dd$9, dd1$3, num$,           /* Label Day of Week          */~
            scr_sel$1,                   /* Glass Process Selection    */~
            scr_dept$3,                  /* Glass Department Selection */~
            scr_shft$2, scr_shft_d$30,   /* Glass Department Selection */~
            scr_msg$30,                  /* Screen - Report Selection  */~
            scr_msg1$30,                 /* Screen - Product Line      */~
            scr_load$5, scr_desc$30,     /* Screen - Load Number       */~
            descr$30,   stk_so$8,        /* Use for GENCODES Look-Up   */~
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

        dim hub_width$8, mdl$3

        dim specialmull$1                /* Casing/BUllnose null (AWD098)*/

        dim f2%(64%),                    /* = 0 if the file is open    */~
            f1%(64%),                    /* = 1 if READ was successful */~
            fs%(64%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64%)20                 /* Text from file opening     */


        dim                              /* FILE - @GLSGED@ - File     */~
            hdr$40,                      /* Askuser Header             */~
            msg$(3%)79,                  /* Askuser Messages           */~
            file$20, bat$3,              /* Name of Batch file         */~
            cut$(10%)55,                 /* TOP A AN B, BOT A AN B     */~
            txt$(4%)50, note$(10%)50,    /* Screen '102Header Text     */~
            tty$2,                       /* GED Glass Type Code        */~
            temp$1,skip$1,               /* Blank No, * = Yes          */~
            temp1$1,                     /* Temp Flag 1        (EWD055)*/~
            sandwich$10,                 /* Glass Sandwich             */~
            spacer$6,                    /* Sort Spacer Thickness      */~
            space_d$10,                  /* Spacer Id                  */~
            t_k$6,                       /* Thickness                  */~
            s_s$6,                       /* Single Strength Spacer     */~
            s_d$6,                       /* Double Strength Spacer     */~
            s_5_32$6,                    /* 5/32 Spacer (AWD111)       */~
            s_3_16$6,                    /* 3/16 Spacer (AWD111)       */~
            s_lam$6,                     /* Laminate Spacer (IG)
            pt_k$6,                      /* PlyGem SS Thickness (CR1987)*/~
            w_adj$6, h_adj$6,            /* Width and Height Adjustment*/~
            muttin$8, lits$1, mut$8,     /* Muttin Code Vert/Horiz     */~
            ged_cnt$5,                   /* Counter for Batches        */~
            t_err$(10%)25,               /* Error Text Message         */~
            so$8, ged$(10%)20,           /* Sales Order Number         */~
            so_ln$2,                     /* Sales Order Lne (AWD085)   */~
            width_d$8,                   /* Save Width Calc in Decimal */~
            height_d$8,                  /* Save Height Calc Decimal   */~
            ged_bilco$1,ged_desc$20,     /* 0 = GED, 1 = Bilco         */~
            g_b$1, pg_dte$6, pg_dte1$6,  /* 0 = GED ONLY,1 = BILCO ONLY*/~
            pg_dte2$6, rm_flag$1,        /* Remake Purge Date          */~
/*AWD105*/  ss$(999%)11, ss$2, xx$2,     /* Sort Code Array (EWD017)   */~
            sizetype$(999%)2,            /* Size Type                  */~
            intercept$2,                 /* (AWD112) Intercept Type    */~
/*AWD105*/  srt$10,                       /* three digit sort          */~
            sort$11,                     /* Used for Sorting           */~
            ss_temp$(999%)11,            /* Used For sort Temp (EWD055)*/~
            temp_rack$1,                 /* Temp Rack '0' '1'  (EWD055)*/~
            index$4                      /* Sort index                 */

        dim rlib$8,                      /* Run library for link call  */~
            rvol$6,                      /* Run volume for link call   */~
            run$8,                       /* Program to Run (EWD007)    */~
            axd$4                        /*                            */

                                         /* (EWD031)                   */
        dim                              /* FILE - (Special Shapes)    */~
            tt$(10%)25, ttt$(10%)8,      /* Screen Prompt Text         */~
            sh_qty$4, sh_batch$20,       /* Window Quantity for Shapes */~
            sh$(10%)10, sh(10%),         /* Entered Data Conv to Decima*/~
            shc$(10%)10, shc(10%),       /* Calculated Sizes Dec to Fra*/~
            shd(10%),                    /* Shape Part No Sizes Decimal*/~
            sh_cnt$4,                    /* Batch Counter              */~
                /* (CR503 CHANGE bil$(300%,30%)20 to bil$(300%,45%)20  */~
            bil$(300%,50%)20,            /* 300 Bilco Shape Records    */~
            sh_sandwich$20,              /* Shapes BILCO Sandwich      */~
            sh_model$3, sh_model_d$30,   /* Model Code                 */~
            sh_glass$2, sh_glass_d$30,   /* Glass Code and Sandwich    */~
            sh_config$2, sh_config_d$30, /* Configuration Code         */~
            sh_config_seq$2,             /* Config Code Sequence       */~
            sh_cnfig$1, sh_cnfig_d$30,   /* Config Input Text for Promp*/~
            sh_flags$(10%)3,             /* Prompt Data(YN) Type(WHRDN)*/~
            sh_num$7, sh_txt$18,         /* Warranty Id Decimal-Text   */~
            sh_grid_flag$1,              /* Grid Only Flg(Y/N) (EWD070)*/~
            sh_fields$7, sh_bridge$7,    /* (EWD056)                   */~
            sh_entry$7,                  /* (EWD056) - Entered Fields  */~
            sh_hub$10,                   /* (EWD056) - Hub adjustment  */~
            sh_face$4,                   /* (EWD056) - Glass Facing cod*/~
            sh_position$7,               /* (EWD056) - Position of Valu*/~
            shape_cross$2,               /* (EWD056) - Cross Ref Code  */~
            sh_lt$2,                     /* (EWD056) - Liting Code     */~
            shape_counter$4,             /* (EWd056) - shape Counter   */~
            sh_codes$8, sh_key$50,       /* (EWD056) - Part No Un Pack */~
            sh_rec$256                   /* (EWD056) - Label Rec and Key*/

        dim                                                              ~
            ged_shape$10,                /* Ged Shape Code     (CR503) */~
            ged_fields$10,               /* Data Entry Values  (CR503) */~
            ged_shc(10%),                /* Calculated Values  (CR503) */~
            gs$2                         /* Argon Gas                  */



        dim                              /* Analysis Screen   (EWD033) */~
            dt$(100%)79,                 /* Analysis Display           */~
            cc$(100%)1,                  /* Selection                  */~
            rec$4,                       /* Batch Lookup Counter       */~
            scr_title$45,                /* Screen Title               */~
            time$8,                      /* Screen Time                */~
            dsp_msg$79,                  /* Screen Display Message     */~
            h1$4, h2$3, h3$2, h4$2,      /* Summary Screen Display     */~
            h5$9, h6$9, h7$9, h8$9,      /*                            */~
            h9$9, h10$                   /*                            */


        dim                              /* Special Shapes    (EWD070) */~
            scr_wind$1, scr_wind_d$30,   /* S.S Receiving Custom Window*/~
            beg_wind$4, end_wind$4,      /* Beg/End Recv. Scanning Time*/~
            scan_time$4, scan_date$      /* Time Glass Scanned         */

                                         /* (EWD070)                   */

        dim ds_batch$1, reschedBatch$1   /* DS Batch Flag  (AWD081)    */

        dim                              /* (AWD083)                   */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4,            /* Additional Info Fields     */~
            sub_part$20,                 /* Sub part number            */~
            new_part$45,                 /* Entire Part Number         */~
            sav_part1$45,                /* Entire Save Part Number    */~
            wrk_part$45,                 /* Entire Save Part Number    */~
            field1$1,                    /* New Part Field 1 GRDTYPE   */~
            field2$1,                    /* New Part Field 2 GRDSIZE   */~
            field3$1,                    /* New Part Field 3 GRDCOLOR  */~
            field4$1,                    /* New Part Field 4 HARDWARE  */~
            field5$1,                    /* New Part Field 5 FOAM      */~
            field6$1,                    /* New Part Field 6 Casing    */~
            field7$1,                    /* New Part Field 7 Sample Col*/~
            field8$1,                    /* New Part Field 8 OGrid Type*/~
            field9$1,                    /* New Part Field 9 OGrid Size*/~
            field10$1,                   /* New Part Field10 OGrid Colo*/~
            field11$1,                   /* New Part Field11 IGrid Colo*/~
            series$16,                   /* Series Code                */~
            style$10                     /* Style Code                 */

        dim schema$8                     /* Schema (AWD088)            */

        dim logmsg$256                /* for LOGFILE subroutine */

/* (AWD110) */
        dim kanbanptKey1$45,            /* KANBANPT readkey key 1     */~
            patio$1                     /* Patio Stock Flag (AWD111)  */
                                        /* 0=NotPatio 1=Door 2=StockSz*/
        dim showMessage$100             /* Shostat Message */

/* (AWD115) */
        dim awdschgl_key0$9,            /* AWDSCHGL Key 0             */~
            gl_order$5,                 /* Order Number               */~
            gl_order_seq$5,             /* Order Sequence Number      */~
            gl_sandwich$20,             /* IG Sandwich Code           */~
            gl_intercept$2,             /* Intercept                  */~
            gl_spc_type$3,              /* Spacer Type                */~
            gl_car_rack$3,              /* Cardinal Rack              */~
            gl_car_gls_type$(2%)30,     /* Cardinal Glass Type        */~
            gl_spc_desc$10,             /* Spacer Desc                */~
            gl_spc_thick$10,            /* Spacer Thickness           */~
            gl_wd1$10,                  /* Glass Width Fraction       */~
            gl_ht1$10,                  /* Glass Height Fraction      */~
            gl_barcode$18,              /* Barcode Number             */~
            gl_err$3,                   /* Error Code                 */~
            gl_ord_gls$1,               /* Order Glass 0 or 1         */~
            gl_gls$2                    /* Gls Code                   */
/* (AWD116) */
        dim interdesc$(99%)5,           /* Intercept Desc             */~
            interoffset$7,              /* Offset by intercept        */~
            interadj$7,                 /* Adjustment by intercept    */~
            interoffset(99%),           /* Offset by intercept        */~
            interadj(99)                /* W & H addit adj by intercep*/

        dim reverse_sort$5

/* (AWD121) */
        dim dim1es$10, dim2es$, dim3es$

/* (AWD122) */
        dim hldkey2$16, hld_lam$1
/* (IM8022) */
        dim glstype$20

        dim table$9,                    /* Table To Read              */~
            genkey$15,                  /* GENCODES Key to Read       */~
            descr1$30,                  /* Description                */~
            codeLen$2,                  /* Code Length                */~
            descr2$30,                  /* Description                */~
            descr3$30                   /* Description                */

        dim dt_prv$2,                   /* APCPLNDT Brand    (CR1987) */~
            lowebarcode$10,             /* Lowe barcode               */~
            ig$2,                       /* IG Code                    */~
            igcode$(3%)3                /* IG Code 1 - 3              */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$41, pname$32, apc1$41                   /* (EWD055) */
                                                            /* (EWD060) */
                                                            /* (EWD066) */
                                                            /* (EWD068) */
                                                            /* (EWD072) */
                                                            /* (AWD077) */
                                                            /* (AWD082) */
            apc$   = "(New)Planning Glass Processing Utility   "
            pname$ = "APCPLA45 - Rev: A1.00 - 11/02/05"
            apc1$  = "(New)Tempered Glass Processing Utility   "
            axd$   = " "

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
            * #1  ! AMTBOMCD ! Master Equation File                     *~
            * #2  ! APCPLNDT ! Master Planning Detail File (APCPIECE)   *~
            * #3  ! GENCODES ! Master System Table File                 *~
            * #4  ! APCEQUAT ! Equation and Part Cross Reference File   *~
            * #5  ! APCPLNLD ! Planning/Scheduling Load Master File     *~
            * #6  ! APCPLNWK ! Glass Work File (Labels)                 *~
            * #7  ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * #8  ! APCPLNGR ! Glass Sched/Remake File                  *~
            * #9  ! TEXTFILE ! MASTER TEXT FILE                         *~
            * #10 ! @GLSGED@ ! Glass Batch File for GED Glass System    *~
            * #11 ! @GLSBLA@ ! Glass Batch File (A)for Bilco Sys(EWD029)*~
            * #12 ! @RMKGD1@ ! Remake Batch File for GED Glass Prd      *~
            * #13 ! @RMKBL1@ ! Remake Batch File for Bilco Glass Prd    *~
            * #14 ! APCPLNOR ! Planning S.O. Header History             *~
            * #15 ! @GLSBLB@ ! Blass Batch File (B)for Bilco Sys(EWD029)*~
            * #16 ! @RMKGD2@ ! Remake Batch GED Glass House (EWD001)    *~
            * #17 ! @RMKBL2@ ! Remake Batch Bilco Glass House (EWD001)  *~
            * #18 ! APCPLNDP ! Planning Master Dept File      (EWD003)  *~
            * #19 ! BCKLINES ! S.O. Line Item Detail Information        *~
            * #20 ! APCPLNSA ! sales Analysis Control File              *~
            * #21 ! @RMKGDK@ ! Remake Batch File for GED Glass Both     *~
            * #22 ! @RMKBLK@ ! Remake Batch File for Bilco Glass Both   *~
            * #23 ! @SHAPE3@ ! Special Shape Glass-for Custom   (EWD056)*~
            * #24 ! AWDSPECB ! Special Shapes Bending Database  (EWD059)*~
            * #25 ! EWDGLSXX ! Glass Label Master DATABASE      (EWD056)*~
            * #26 ! @SHAPE2@ ! Special Shape Glass-Bilco        (EWD056)*~
            * #27 ! EWDGLSWW ! Special Shape Sort Work File     (EWD056)*~
            * #28 ! AWDPLNGR ! Glass Sched/Remake Tempered File (EWD055)*~
            * #29 ! @GLSTMP@ ! Glass Batch File for GED Glass   (EWD055)*~
            * #30 ! @RMKTMP@ ! Rmke Batch File for GED Glass    (EWD055)*~
            * #31 ! @SHAPE4@ ! Special Shape Glass Remake Custom(EWD069)*~
            * #32 ! AWDGLSCT ! Custom Glass Detail Data File    (EWD072)*~
            * #33 ! @RMKGD5@ ! Rmk Batch file vinyl lne codes   (AWD074)*~
            * #34 ! @RMKBL5@ ! Bilco File for vinyl line rmks   (AWD074)*~
            * #35 ! @GLSPRE@ ! Glass Batch File for PreCut Glass(AWD075)*~
            * #36 ! @RMKBLT@ ! Bilco File for tempered glass    (AWD076)*~
            * #37 ! @GLSBLT@ ! Bilco File for tempered glass    (AWD076)*~
            * #38 ! @SHAPE5@ ! File for Infini-Lite order file  (AWD102)*~
            * #42 ! HUBCALFL ! Hub formula file                 (AWD091)*~
            * #44 ! @RMKFLG@ ! Remake Batch File Florida GED    (AWD106)*~
            * #45 ! @RMKFLB@ ! Remake Batch File Florida Bilco  (AWD106)*~
            * #46 ! @GLSVLA@ ! Valance GED Glass File                   *~
            * #47 ! @GLSVLB@ ! Valance Bilco (PMC) Glass File           *~
            * #48 ! @RMKVLA@ ! Remake Valance GED Glass File            *~
            * #49 ! @RMKVLB@ ! Remake Valance Bilco (PMC) Glass File    *~
            * #50 ! KANBANPT ! Kanban File                              *~
            * #51 ! @PATIO@  ! Patio Glass File                         *~
            * #52 ! @CUSPAT@ ! Custom Patio File                        *~
            * #53 ! @RMKPAT@ ! Remake Patio Glass File                  *~
            * #54 ! @RMKPTC@ ! Remake Custom Patio File                 *~
            * #55 ! AWDSCHGL ! EWDPLN58 GLASS ORDER and Order sequence  *~
            * #56 ! @GLSLAM@ ! Laminate Glass file             (AWD0123)*~
            * #63 ! BCKSUBPT ! Subpart file                             *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "AMTBOMCD",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42

            select #2,   "APCPLNDT",                                     ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   24, keylen =   23,                    ~
                        alt key  1, keypos =   47, keylen =  57,         ~
                            key  2, keypos  =  53, keylen =  51,         ~
                            key  3, keypos  =   1, keylen =  23, dup,    ~
                            key  4, keypos  =  96, keylen =   8, dup

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #4,  "APCEQUAT",                                      ~
                        varc,     indexed,  recsize =   16,              ~
                        keypos =    1, keylen =   8

            select #5,   "APCPLNLD",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   11, keylen =    5,                    ~
                        alt key  1, keypos =    3, keylen =  13,         ~
                            key  2, keypos  =   1, keylen =  15

REM            select #6,  "APCPLNWK",                                   ~
                        varc,     indexed,  recsize =   256,             ~
                        keypos =    1, keylen =   66
/*(AWD083) Record size change (AWD112) keylen & record size change */
REM            select #6,  "APCPLNWK",                                   ~
                        varc,     indexed,  recsize =   384,             ~
                        keypos =    1, keylen =   66

            select #6,  "APCPLNWK",                                      ~
                        varc,     indexed,  recsize =   574,             ~
                        keypos =    1, keylen =  128

            select #7,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  1,   keylen =  25,                     ~
                        alt key  1, keypos  =   102, keylen =  9, dup,   ~
                            key  2, keypos  =    90, keylen =  4, dup,   ~
                            key  3, keypos  =    26, keylen = 32, dup

            select #8,  "APCPLNGR",                                      ~
/*AWD083*/              varc,     indexed,  recsize =  512,              ~
                        keypos = 22,   keylen =  12,                     ~
                        alt key  1, keypos  =     7, keylen = 27,        ~
                            key  2, keypos  =     1, keylen = 33,        ~
                            key  3, keypos  =    13, keylen = 21

            select #9,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =   1, keylen =  11

            select #10, "@GLSGED@", consec, recsize = 384     /*(EWD064)*/
                                                /* (EWD029)          */
            select #11, "@GLSBLA@", consec, recsize = 165

* !!! CMG was 220
            select #12, "@RMKGD1@", consec, recsize = 384

            select #13, "@RMKBL1@", consec, recsize = 165

            select #14, "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =   170,             ~
                        keypos =    1, keylen =  51,                     ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos  =  70, keylen =   8, dup,    ~
                            key  3, keypos  =  78, keylen =   8, dup,    ~
                            key  4, keypos  =  52, keylen =   8,         ~
                            key  5, keypos  =  36, keylen =   16, dup
                                                /* (EWD029)           */
            select #15, "@GLSBLB@", consec, recsize = 165

* !!! CMG was 220
            select #16, "@RMKGD2@", consec, recsize = 384 /* (EWD001) */

            select #17, "@RMKBL2@", consec, recsize = 165 /* (EWD001) */

            select #18, "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =    32,             ~
                        keypos =   11, keylen =  12,                     ~
                        alt key  1, keypos =    9, keylen =  14,         ~
                            key  2, keypos  =   4, keylen =  12,         ~
                            key  3, keypos  =   1, keylen =  15

            select #19, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos = 10,   keylen =  19

            select #20, "APCPLNSA",                                      ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos = 11,   keylen =  17,                     ~
                        alt key  1, keypos  =     1, keylen = 19, dup
                                                       /* (EWD006)     */
* !!! CMG
            select #21, "@RMKGDK@", consec, recsize = 384

            select #22, "@RMKBLK@", consec, recsize = 165
                                                       /* (EWD006)     */
                                                       /* (EWD012)     */
                                                       /* (EWD056)     */

                                                      /* (RHH-SPECIAL) */
                                                      /* (EWD067)      */
                                                  /* Chg Record Length */
                                                  /* to add Gls Barcode*/
                                                  /* 170 to 180        */
                                                  /* (EWD069) Chg Rec  */
                                                  /* Fr 180 to 200     */
            select #23, "@SHAPE3@", consec, recsize = 200

                                                  /* (EWD069) Glass Rmk*/
            select #31, "@SHAPE4@", consec, recsize = 200
                                                  /* (EWD069) Remake Gls*/
                                                  /* (EWD069)          */
                                                  /* (RHH-SPECIAL) */
                                                  /* (EWD059)      */
                                                  /* (EWD067)      */

            select #24, "AWDSPECB",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =  1,   keylen =  36,                     ~
                        alt key  1, keypos  =   140, keylen = 23
                                                       /* (EWD059)     */

            select #25, "EWDGLSXX",                                       ~
                        varc,     indexed,  recsize = 384, /* (CR2305) */ ~
                        keypos =  1,   keylen =  32,                      ~
                        alt key  1, keypos  =    33, keylen = 12
                                                       /* (EWD012) End */
                                                       /* (EWD014)     */
                                                       /* (EWD056)     */
            select #26, "@SHAPE2@", consec, recsize = 165


                                                        /* (EWD056)     */
                                     /* (CR503) changed from 512 to 1024*/
            select #27, "EWDGLSWW",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos = 257,   keylen =  50
                                                       /* (EWD056)     */

            select #28,  "AWDPLNGR",                   /* (EWD055) */    ~
/*AWD083*/              varc,     indexed,  recsize =  384,              ~
                        keypos = 22,   keylen =  12,                     ~
                        alt key  1, keypos  =     7, keylen = 27,        ~
                            key  2, keypos  =     1, keylen = 33,        ~
                            key  3, keypos  =    13, keylen = 21

* !!! CMG was 220 now 384
            select #29, "@GLSTMP@", consec, recsize = 384

            select #30, "@RMKTMP@", consec, recsize = 384
                                                       /* (EWD072)      */
            select #32, "AWDGLSCT",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    1, keylen =   23,                    ~
                        alt key  1, keypos =   12, keylen =  12,         ~
                            key  2, keypos  = 154, keylen =  29
                                                       /* (EWD072)      */


                                                       /* (AWD074) - BEGIN */
* !!! CMG was 220
            select #33, "@RMKGD5@", consec, recsize = 384

            select #34, "@RMKBL5@", consec, recsize = 165

                                                    /* (AWD074) - END   */

            select #35, "@GLSPRE@", consec, recsize = 384    /* (AWD075) */

            select #36, "@RMKBLT@", consec, recsize = 165    /* (AWD076) */

            select #37, "@GLSBLT@", consec, recsize = 165    /* (AWD076) */

/* (AWD102) add infini-lite file */
            select #38, "@SHAPE5@", consec, recsize = 200

/* <AWD091> */
            select #42, "HUBCALFL",                                      ~
                        varc,  indexed,  recsize = 512,                  ~
                        keypos =    1, keylen =  29

            select #43, "EWDHUBCL",                                      ~
                         varc,  indexed,  recsize = 64,                  ~
                         keypos =    1, keylen =  12
/* </AWD091> */

/* (AWD106) - Remake FLG - GED -- FLB - Bilco/Hegla */
            select #44, "@RMKFLG@", consec, recsize = 384

            select #45, "@RMKFLB@", consec, recsize = 165

/* (AWD106) */


/* (AWD109) - Remake FLG - GED -- FLB - Bilco/Hegla */
            select #46, "@GLSVLA@", consec, recsize = 384

            select #47, "@GLSVLB@", consec, recsize = 165

            select #48, "@RMKVLA@", consec, recsize = 384

            select #49, "@RMKVLB@", consec, recsize = 165

/* (AWD109) */
/* (AWD110) add kanbanpt */
            select #50, "KANBANPT",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =  1,    keylen = 10,                     ~
                        alt key  1, keypos =   11, keylen =  45
/*(AWD111)*/
            select #51, "@PATIO@", consec, recsize = 384
/*(AWD111)*/
            select #52, "@CUSPAT@", consec, recsize = 384


/*(AWD111)*/
            select #53, "@RMKPAT@", consec, recsize = 384
/*(AWD111)*/
            select #54, "@RMKPTC@", consec, recsize = 384

/* (AWD115) */
            select #55, "AWDSCHGL",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =   11, keylen =   9,                     ~
                        alt key  1, keypos =    1, keylen =  19

            select #56, "@GLSLAM@", consec, recsize = 384   /* (AWD123) */

            select #57, "HLDSCHED",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  40,                     ~
                        alt key  1, keypos =   16, keylen =  25,         ~
                            key  2, keypos =   27, keylen =  16


                                                        /* (AWD083)     */
            select #63, "BCKSUBPT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =  12, keylen =  11, dup,     ~
                            key  2, keypos =  23, keylen =  45, dup

                                                       /* (AWD083)     */


            call "SHOSTAT" ("Opening Files, One Moment Please")
                                                   /* (EWD014)         */

REM            call "OPENCHCK" (#6, fs%(6%), f2%(6%),500%, rslt$(6%))

            filename$ = "AMTBOMCD" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDT" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),500%, rslt$(4%))

            filename$ = "APCPLNLD" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "HNYMASTR" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            call "OPENCHCK" (#8, fs%(8%), f2%(8%),500%, rslt$(8%))

            filename$ = "TXTFILE" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNOR" : call "EWDOPEN" (#14, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDP" : call "EWDOPEN" (#18, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#19, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSA" : call "EWDOPEN" (#20, filename$, err%)
            if err% <> 0% then gosub open_error
                                                          /* (EWD056)    */
            call "OPENCHCK" (#25, fs%(25%), f2%(25%),500%, rslt$(25%))
                                                          /* (EWD014)    */

                                                          /* (EWD055)    */
            call "OPENCHCK" (#28, fs%(28%), f2%(28%),500%, rslt$(28%))

                                                          /* (EWD059)    */
            call "OPENCHCK" (#24, fs%(24%), f2%(24%),500%, rslt$(24%))
                                                          /* (EWD072)    */
            call "OPENCHCK" (#32, fs%(32%), f2%(32%),500%, rslt$(32%))

/* <AWD091> */
            call "OPENCHCK" (#42, fs%(42%), f2%(42%),500%, rslt$(42%))
            call "OPENCHCK" (#43, fs%(43%), f2%(43%),500%, rslt$(43%))
            call "FILEBGON" addr(#43%)
            call "OPENFILE" (#43%,"OUTPT",f2%(43%), rslt$(43%), axd$)
/* </AWD091> */

/* (AWD110) */
            filename$ = "KANBANPT" : call "EWDOPEN" (#50, filename$, err%)
            if err% <> 0% then gosub open_error

/* (AWD115) */
            filename$ = "AWDSCHGL" : call "EWDOPEN" (#55, filename$, err%)
            if err% <> 0% then gosub open_error


            filename$ = "HLDSCHED" : call "EWDOPEN" (#57, filename$, err%)
            if err% <> 0% then gosub open_error
                                                           /* (AWD083)  */
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
            if err% <> 0% then gosub open_error
                                                           /* (AWD083)  */

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************



            call "FILEBGON" (#27)


            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
                                               /* (EWD005) - Begin */
                                               /* (EWD030)   Begin */

            scr$(1%) = "****************************************"
            scr$(2%) = "* (1) - Calc. and Explode Glass Sizes  *"
            scr$(3%) = "* (2) - Create Glass Labels/Batches    *"
            scr$(4%) = "* (3) - Proc ReMake Glass (All)        *"
            scr$(5%) = "* (4) - Create Gls Laminate Production *"
            scr$(6%) = "* (5) - Special Shape Glass (Bilco)    *"
            scr$(7%) = "* (6) - Proc INDY Glass                *"
            scr$(8%) = "* (7) - Proc ReMake Laminate           *"
            scr$(9%) = "* (8) - Create Valance Gls Lbls/Batches*"
            scr$(10%)= "* (9) - Create Valance Remakes         *"
                                               /* (EWD005) - End   */
                                               /* (AWD074) Change Screen */

                                               /* (EWD030)   End   */
            rpt$(1%) = "****************************************"
            rpt$(2%) = "*       New Glass Reporting            *"
            rpt$(3%) = "*      ----------------------          *"
            rpt$(4%) = "* (1) - Scanned Glass Re-Make's        *"
            rpt$(5%) = "* (2) - Scheduled Glass Re-Make's      *"
            rpt$(6%) = "* (3) - Scheduled Glass                *"
            rpt$(7%) = "* (4) - Comp Glass Report-Uses Scan Dte*"
            rpt$(8%) = "* (5) - Comp Re-Make Glass-Uses Scan DT*"
            rpt$(9%) = "*                                      *"
            rpt$(10%)= "****************************************"

                                               /* (EWD055)   Begin */
                                             /* (AWD109) change scr*/
                                            /* (CR2773) precut temp*/
            scr_temp$(1%) = "******************New Tempered Glas~
                            ~s Processing************************"
            scr_temp$(2%) = "*1-                               *~
                            ~                                  *"
            scr_temp$(3%) = "*2-Create Tempered Glass Lbls/Btch*~
                            ~B-Create Laminate Glass           *"
            scr_temp$(4%) = "*3-Proc Tempered Re-Make Gls (All)*~
                            ~C-Create Laminate Remake          *"
            scr_temp$(5%) = "*4-Proc Precut tempered           *~
                            ~                                  *"
            scr_temp$(6%) = "*5-Create Stock Patio Batch       *~
                            ~                                  *"
            scr_temp$(7%) = "*6-Create Precut Temp Rmk         *~
                            ~                                  *"
            scr_temp$(8%) = "*7-Create Re-make Stock Batch     *~
                            ~                                  *"
                                               /* (EWD055) - End   */


        REM - NEAREST 8TH INCH
           sze$ = "1/81/43/81/25/83/47/8         "

        REM - NEAREST 16TH OF AN INCH
           sz$ = " 1/16 1/8  3/16 1/4  5/16 3/8  7/16 1/2  9/16 5/8 11/16~
        ~ 3/4 13/16 7/8 15/16     "

                        /* (GLASS01)  - GLASS QUANTITIES FOR TOP/BOT    */
                        /* (GLASS02)  - TABLE FOR TOP GLASS ONLY        */
                        /* (GLASS03)  - TABLE FOR TWO BOTTOMS           */
                        /* (GLASS04)  - Table for no Glass Required     */
                        /* (GLASS05)  - TABLE FOR '8' SERIES WITH GLASS */
                        /* (GLASS06)  - TABLE TO SKIP NO DETAIL LABELS  */
                        /* (GLASS07)  - TABLE FOR SLIDERS               */
                        /* (GLASS08)  - TABLE - SPECIAL SHAPES NO GLASS */
                        /* (GLASS09)  - TABLE - Bilco Glass Exclude Depts*/

            tab$(1%) = "GLASS01  "   : tab$(6%)  = "GLASS06  "
            tab$(2%) = "GLASS02  "   : tab$(7%)  = "GLASS07  "
            tab$(3%) = "GLASS03  "   : tab$(8%)  = "GLASS08  "
            tab$(4%) = "GLASS04  "   : tab$(9%)  = "GLASS09  "
            tab$(5%) = "GLASS05  "
            tab_max% = 9%

            gosub load_tables                /* Load all Glass Tables  */

            t_err$(1%) = "Err-1 No Equation Found  "
            t_err$(2%) = "Err-2 C.M.Rail Calc Error"
            t_err$(3%) = "Err-3 C.M.Rail Entered   "
            t_err$(4%) = "Err-4 GED Overall Thick  "
            t_err$(5%) = "Err-5 GED Sandwich Find  "
            t_err$(6%) = "Err-6 GED Spacer Descr.  "
            t_err$(7%) = "Err-7 GED No. Lits Error "
            t_err$(8%) = "Err-8 GED Hinge Code Err "
            t_err$(9%) = "Err-9 GED Muttin Error   "
            t_err$(10%) = " "

            ged$( 1%) = "OV Thick =XXXXXX    "
            ged$( 2%) = "Sandwich =XXXXXXXXXX"
            ged$( 3%) = "Spacer D.=XXXXXXXXXX"
            ged$( 4%) = "No. Lits =X         "
            ged$( 5%) = "Muttin Cd=XXXXXXXX  "

            ged$( 6%) = "OV Thick =XXXXXX    "
            ged$( 7%) = "Sandwich =XXXXXXXXXX"
            ged$( 8%) = "Spacer D.=XXXXXXXXXX"
            ged$( 9%) = "No. Lits =X         "
            ged$(10%) = "Muttin Cd=XXXXXXXX  "


            gosub load_sort
            gosub load_interdesc

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            for fieldnr% = 1% to  7%
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
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% = 14% and fieldnr% = 1% then inputmode_r
                      if keyhit% <> 0% then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%
        goto editpg1
        inputmode_a
            txt$(2%) =                                                   ~
                    "*(Calculate) Glass Cuts for Manufactured Products*"
            for fieldnr% = 1% to  1%
L10330:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10450
L10350:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% then goto inputmode
                      if keyhit% <>  4% then       L10440
L10380:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10350
                         if fieldnr% = 1% then L10330
                         goto L10380
L10440:               if keyhit% <> 0% then       L10350
L10450:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10350
            next fieldnr%
            goto editpg2

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
            if fieldnr% < 1% or fieldnr% > 7% then editpg1
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

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub calc_data
                  if keyhit%  = 16% then goto  inputmode
                  if keyhit% <>  0% then       editpg2
L11310:     fieldnr% = cursor%(1%) - 7%
            if fieldnr% < 1% or fieldnr% > 1% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L11360:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11360
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11360
                  lastfieldnr% = fieldnr%
            goto L11310


        REM *************************************************************~
            *       I N P U T   M O D E   F O R   R E P O R T S         *~
            *************************************************************

        inputmode_r
            gosub initialize_variables
            txt$(2%) =                                                   ~
                    "* (Report) Current Glass Status for MFG Products *"
                                           /* (EWD070)          */
            for fieldnr% = 1% to  3%
L12300:         gosub'053(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L12420
L12320:         gosub'103(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% then goto inputmode
                      if keyhit% <>  4% then       L12410
L12360:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'053(fieldnr%)
                         if enabled% = 1% then L12320
                         if fieldnr% = 1% then L12300
                         goto L12360
L12410:               if keyhit% <> 0% then       L12320
L12420:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12320
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   F O R   R E P O R T S          *~
            *************************************************************

        editpg3
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then goto  inputmode
                  if keyhit% <>  0% then       editpg3
L14110:     fieldnr% = cursor%(1%) - 7%
                                        /* (EWD070) Chg Edit           */
            if fieldnr% < 1% or fieldnr% > 3% then editpg3
            if fieldnr% = lastfieldnr% then    editpg3
            gosub'053(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg3
L14160:     gosub'103(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L14160
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L14160
                  lastfieldnr% = fieldnr%
            goto L14110
                                                       /* (EWD070)     */
                                                    /* (EWD031) - Begin*/
        REM *************************************************************~
            * I N P U T   M O D E   F O R   S P E C I A L   S H A P E S *~
            *************************************************************

        inputmode_special_shapes
            rec% = 0%                                 /*  (EWD033)   */
            txt$(2%) =                                                   ~
                    "* Calculate Cut Glass for Special Shapes         *"
            for fieldnr% = 1% to  12%
L16300:         gosub'055(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L16420
L16320:         gosub'105(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% then gosub process_shapes
                      if keyhit% <>  4% then       L16410
L16360:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'055(fieldnr%)
                         if enabled% = 1% then L16320
                         if fieldnr% = 1% then L16300
                         goto L16360
L16410:               if keyhit% <> 0% then       L16320
L16420:         gosub'155(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L16320
            next fieldnr%

        REM *************************************************************~
            *  E D I T   M O D E   F O R   S P E C I A L   S H A P E S  *~
            *************************************************************

        editpg5
            lastfieldnr% = 0%
            gosub'105(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub process_shapes
                  if keyhit% <>  0% then       editpg5
L16510:     fieldnr% = cursor%(1%) - 7%
            if fieldnr% < 1% or fieldnr% > 12% then editpg5
            if fieldnr% = lastfieldnr% then    editpg5
            gosub'055(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg5
L16560:     gosub'105(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L16560
            gosub'155(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L16560
                  lastfieldnr% = fieldnr%
            goto L16510
                                                      /* (EWD031) - End */

                                                    /* (EWD055) - Begin */
        REM *************************************************************~
            *       I N P U T   M O D E   T E M P E R E D               *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry tempered screens.     *~
            *************************************************************

        inputmode_tempered
            gosub initialize_variables_temp
            for fieldnr% = 1% to  7%
L16600:         gosub'056(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L16720
L16620:         gosub'106(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L16700
L16650:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'056(fieldnr%)
                         if enabled% = 1% then L16620
                         if fieldnr% = 1% then L16600
                         goto L16650
L16700:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% = 14% and fieldnr% = 1% then inputmode_r
                      if keyhit% <> 0% then       L16620
L16720:         gosub'156(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L16620
            next fieldnr%
            goto editpg6

        REM *************************************************************~
            *  E D I T   M O D E   F O R   T E M P E R E D              *~
            *************************************************************

        editpg6
            lastfieldnr% = 0%
            gosub'106(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub process_tempered
                  if keyhit% <>  0% then       editpg5
L16810:     fieldnr% = cursor%(1%) - 7%
            if fieldnr% < 1% or fieldnr% > 12% then editpg6
            if fieldnr% = lastfieldnr% then    editpg5
            gosub'056(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg6
L16860:     gosub'106(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L16860
            gosub'156(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L16860
                  lastfieldnr% = fieldnr%
            goto L16810
                                                      /* (EWD055) - End */

        REM *************************************************************~
            *             P R O C E S S   D A T A                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        process_tempered                           /*  (EWD055)    */
            shape_remake% = 0%
            tempScrn% = 1%
            if scr_sel% = 12% then lamnScrn% = 1%
            if scr_sel% = 4% then temppreScrn% = 1%   /* (CR2773) */
            goto tmpScrn
        begin_process
            shape_remake% = 0%                     /* (EWD071)     */
            if scr_sel% = 4% then lamnScrn% = 1%   /* Production-Annealed*/
            if scr_sel% = 7% then lamnScrn% = 1%   /* Remakes   -Annealed*/
tmpScrn:
             if scr_sel% <> 1% then goto L19110
                return clear all
                goto inputmode_a

L19110:      call "SHOSTAT" ("Creating "& scr_msg$)
                                               /* (EWD005) - Begin    */

             if tempScrn% = 1% then goto temperedChoice  /* (AWD011) */
REM             IF LAMNSCRN% = 1% THEN GOTO TEMPEREDCHOICE  /* (AWD123) */

             if scr_sel% = 2% then gosub create_data
             if scr_sel% = 4% then gosub create_data
             if scr_sel% = 6% then gosub create_data
             if scr_sel% = 8% then gosub create_data

             if scr_sel% = 3% then gosub create_rmk_data
             if scr_sel% = 7% then gosub create_rmk_data
             if scr_sel% = 9% then gosub create_rmk_data

               goto create_batch

temperedChoice:
         if scr_sel% = 2% then gosub create_data
         if scr_sel% = 4% then gosub create_data      /* (CR2773) */
         if scr_sel% = 5% then gosub create_data
         if scr_sel% = 12% then gosub create_data     /* Option - B      */
                                                      /*Laminate (AWD123)*/
         if scr_sel% = 3% then gosub create_rmk_data
         if scr_sel% = 6% then gosub create_rmk_data   /* (CR2773)    */
         if scr_sel% = 7% then gosub create_rmk_data   /* Stock Patio */
         if scr_sel% = 13% then gosub create_rmk_data  /* Temp Lam Rmk */


REM CALL "SHOSTAT" ("HERE AT END OF CREATE DATA " )  STOP
        create_batch                           /* Create Glass Batches */
                                               /* (EWD056) Shapes      */
                                               /* NTX Shapes           */
REM --------------------------------------------------------------------
REM      S H A P E S
REM --------------------------------------------------------------------
        if scr_dept$ = "043" and schema% = 1% then goto create_batch_done
        if (scr_dept$ = "002" or scr_dept$ = "003") and schema% = 2%   ~
                                 then goto create_batch_done  /*(CR2109)*/
        if scr_dept$ = "064" and schema% = 2% then goto create_batch_done
                                               /* (EWD056) Batches     */
            gosub batch_header
            if keyhit% = 1% then gosub startover
            if keyhit% = 16% then gosub startover
            if keyhit% = 14% then goto L19120
            if keyhit% = 10% then goto L19130
               goto create_batch

L19120:      gl_sort% = 0%
             gosub print_labels
             goto L19150

L19130:      gosub prompt_no_labels
             if comp% <> 0% then goto L19190

L19150:      gosub prompt_user
             if comp% <> 0%  then goto L19190
                gosub create_glass
                gosub prompt_user_done
L19190:      gosub delete_work
             keyhit% = 0%
        return clear all
        goto inputmode

        create_batch_done                         /* (EWD056)        */
        return clear all                          /* Process Shapes  */
                                                  /* (EWD056)        */
                                                  /* Process Shapes  */
             gosub process_shapes_online

        goto inputmode

        prompt_user
            comp% = 2%
            hdr$ = "** Glass Optimizer Batches **"
            if ged_bilco$ = "0" then                                     ~
             msg$(1%)="Do you wish to Create Both GED & Bilco Glass     "
            if ged_bilco$ = "1" then                                     ~
             msg$(1%)="Do you wish to Create GED Glass Batches Only?    "
            if ged_bilco$ = "2" then                                     ~
             msg$(1%)="Do you wish to Create Bilco Glass Batches Only?  "
            msg$(2%)="             O P T I M I Z A T I O N             "
            msg$(3%)="Press <RETURN> To Create ,  Any (PF) Key To Exit."
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        prompt_user_done
            comp% = 2%
            hdr$ = "** Glass Optimizer Batches **"
            msg$(1) = "Transmit File Name (@GLSGED@) Number of Batches"  ~
                     & " Equals ("&bat$&")"

            msg$(2) = "             O P T I M I Z A T I O N             "
            msg$(3) = "Press <RETURN> or Any (PF) Key To Continue....   "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        prompt_user_done_shape
            comp% = 2%
            hdr$ = "** Glass Optimizer Batches **"
            msg$(1) = "Transmit File Name (@SHAPE2@) Number of Batches"  ~
                     & " Equals ("&bat$&")"

            msg$(2) = "             O P T I M I Z A T I O N             "
            msg$(3) = "Press <RETURN> or Any (PF) Key To Continue....   "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return


        prompt_no_data
            comp% = 2%
            hdr$ = "***** Glass Processing *****"
          msg$(1%)= "( No Data ) Found on System for Specified Selection"
          msg$(2%)= "                   G l a s s                     "
          msg$(3%)= "Press <RETURN> or Any (PF) Key To Continue....   "
          call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return clear all
        goto inputmode

        prompt_no_shape_data
            comp% = 2%
            hdr$ = "***** Shapes Processing *****"
          msg$(1%)= "( No Data ) Found on System for Shapes Selection"
          msg$(2%)= "                   G l a s s                     "
          msg$(3%)= "Press <RETURN> or Any (PF) Key To Continue....   "
          call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return clear all
        goto inputmode

        prompt_no_labels
            comp% = 2%
            hdr$ = "***** Glass Processing *****"
          msg$(1%)= "(No Glass Labels Will be Created !!!) for Selection"
          msg$(2%)= "                   G l a s s                     "
          msg$(3%)= "Press <RETURN> To Continue, Any (PF) Key To Exit."
          call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        print_report
            gosub select_printer
            gosub gen_rpt                           /* (EWD005)        */
            gosub close_printer
        return clear all
        goto inputmode

        select_printer
            pageno% = 0%
            lcnt%    = 99%
            title$ = rp_sel_d$                        /* (EWD070)   */
            call "FMTTITLE" (title$, " ", 12%)
            date$ = date  :  call "DATEFMT" (date$)
            company$ = "Atrium Windows and Doors"     /*  (EWD070)  */
            call "SETPRNT" ("APCGLS", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCGLS", " ", 0%, 1%)
        return
                                               /* (EWD031) - Begin    */
        process_shapes                         /* (EWD056)            */
            call "SHOSTAT" ("Creating "& scr_msg$)
            if sh_cnt% < 1% then goto L19900

               gosub batch_header
               if keyhit% = 1% then gosub startover
               if keyhit% = 16% then gosub startover
               if keyhit% = 14% then goto L19200
               if keyhit% = 10% then goto L19210
               goto process_shapes

L19200:        gl_sort% = 0%                   /* (EWD056)            */
               gosub print_labels_sh
               goto L19230

L19210:        comp% = 0%
               gosub prompt_no_labels
               if comp% <> 0% then goto L19890

L19230:        gosub prompt_user
               if comp% <> 0%  then goto L19890

               scr_dte$ = date            /* Today's Date              */
               date% = 0%
               call "DATEOK" (scr_dte$, date%, errormsg$ )
               scr_dte1$ = scr_dte$
               call "DATUNFMT" (scr_dte1$)

                                          /* ( Bilco Glass Cutter )    */
            call "APCPLF45" (size%,       /* Specified Batch Size      */~
                             glass_dte$,  /* Glass Production Date     */~
                             scr_sel$,    /* Screen Selection          */~
                             scr_dte$,    /* Planned Production Date   */~
                             scr_dte1$,   /* Planned Production Unforma*/~
                             file$,       /* Name of Optimized File    */~
                             bat$,        /* Number of Batches Created */~
                             bil$(),      /* Window Cut Instructions   */~
                             sh_max%,     /* Number of Windows in Batch*/~
                             #3)          /* (GENCODES) TABLES         */

            gosub prompt_user_done_shape

L19890:     keyhit% = 0%

L19900: return clear all
        goto inputmode                    /* (EWD056)                 */
                                          /* (EWD031)                 */


        process_shapes_online            /* (EWD056)                  */
                                         /* (EWD058)                  */
                                         /* (EWD071)                  */
         if shape_remake% = 0% then                                  ~
          call "SHOSTAT" ("(" & shape_counter$ & ") S.S. Labels-Bridge") ~
          else                                                     ~
          call "SHOSTAT" ("(" & shape_counter$ & ") S.S. Remake Labels-  ~
                          ~Bridge")
                                         /* (EWD071)                 */

               gosub batch_header
               if keyhit% = 1% then gosub startover
               if keyhit% = 16% then gosub startover
               if keyhit% = 14% then goto L20000 /* Process an Create*/
                                                 /* Label Data       */
               if keyhit% = 10% then goto L20010 /* Process an do Not*/
                                                 /* Create Labels    */
               goto process_shapes_online

L20000:        gl_sort% = 0%                   /* (EWD056)            */
                                               /* (EWD071)            */
               if shape_remake% = 0% then goto L20005
                  gosub special_create_glass
                                               /* Create Remake Glass */
                                               /* Labels     (EWD071) */

                   goto L20030                 /* Create Glass Bridge */
                                               /* (EWD070)            */

L20005:                                        /* Move Labels from the*/
                                               /* sorted work file to */
                                               /* Database. EWDGLSXX  */
               gosub print_labels_online
               goto L20030                     /* scr_dte1$           */

L20010:        comp% = 0%
               gosub prompt_no_labels
               if comp% <> 0% then goto L20090


L20030:        gosub prompt_user
               if comp% <> 0%  then goto L20090
                                          /* (AWD085)                  */
                                          /* (EWD066)                  */
                                          /* Create Special Shapes     */
                                          /* Bridge File. @SHAPE2@     */
                                          /* ( Bilco Glass Cutter )    */
            call "APCPLG45" (size%,       /* Specified Batch Size      */~
                             scr_sel$,    /* Screen Selection          */~
                             scr_dte1$,   /* Production Date unform    */~
                             file$,       /* Name of Optimized File    */~
                             bat$,        /* Number of Batches Created */~
                             sh_max%,     /* Number of Windows in Batch*/~
                             sh_grid_flag$,/* Grid Only Flag   (EWD070)*/~
                             schema%,     /* Schema                    */~
                             #3,          /* (GENCODES) TABLES         */~
                             #26,         /* (@SHAPE2@) In House       */~
                             #25,         /* (EWDGLSXX) Label Data Base*/~
                             #8,          /* (APCPLNGR) glass file     */~
                             #23,         /* (@SHAPE3@) Custom File    */~
                             #31,         /* (@SHAPE4@) Custom Remake  */~
                             #32,         /* (AWDGLSCT)CustData(EWD072)*/~
                             #2,          /* (APCPLNDT)Planning(EWD072)*/~
                             #6,          /* (APCPLNWK) Label Detail Fl*/~
                             #63,         /* (BCKSUBpt)SubPart(AWD085) */~
                             #43,         /* (EWDHUBCL) hub calc file  */~
/*(AWD102)*/                 #38 )        /* (@SHAPE5@)Infini-Lite     */


            gosub prompt_user_done_shape

                                          /* (GED Glass Cutter (CR503))*/
            call "APCPLH45" (size%,       /* Specified Batch Size      */~
                             scr_sel$,    /* Screen Selection          */~
                             glass_dte$,  /* Glass Production Date     */~
                             scr_dte$,    /* Planned Production Date   */~
                             scr_dte1$,   /* Planned Production Unforma*/~
                             file$,       /* Name of Optimized File    */~
                             bat$,        /* Number of Batches Created */~
                             #3,          /* (GENCODES) TABLES         */~
                             #27)         /* Work File                 */


L20090:     keyhit% = 0%                  /* Scratch the Sorted work   */
                                          /* File                      */
                                          /* (EWD071)                  */
                                          /* No Work File Created      */
            if shape_remake% = 1% then return

            gosub delete_work_shape
            gosub delete_work

        return                            /* (EWD056)                  */



        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            goto L28000

        deffn'052(fieldnr%)
            goto L28000

        deffn'053(fieldnr%)
            goto L28000

        deffn'055(fieldnr%)                        /* (EWD031)         */
L28000:     enabled% = 1%
        return

        deffn'056(fieldnr%)                        /* (EWD030)         */
            goto L28000

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
         "Enter 1=Calc,2=Lab/Bat,3=ReMke(All),4=RMake(Lds),5=Shapes,     ~
         ~6=INDY?",                                                      ~
         "Enter Machine Selection, 0=Both, 1=GED, 2=Bilco? Def. = '0'? ",~
         "Enter a Valid Planning Production Date?                      ",~
         "Enter a Valid Department Selection or All?                   ",~
         "Enter a Valid Shift Selection or 'AA' = All Shifts?          ",~
         "Enter the Completion Date for the Glass?                     ",~
         "Enter a Production Load Number?                              "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28290
                inpmessage$ = edtmessage$
                return

L28290
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid Manufacture Part Number?                       "

          deffn'070(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28420
                inpmessage$ = edtmessage$
                return

L28420
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn3_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return
                                                    /* (EWD070)        */
        scrn3_msg  :  data                                               ~
         "Enter a Beginning and Ending Production Date for Report?     ",~
         "Enter Sel. 1=Scan'd Rmk,2=Sch'd Rmk,3=Sch'd,4=Comp Gls Rpt Dte,~
         ~5=Comp Rmk Gls Dte?",                                          ~
         "Enter Valid Dept Code or (ALL) and Scanning Window Code (Specia~
         ~l Shapes)?"

                                                    /* (EWD070)        */

        deffn'090(scrnr%, fieldnr%)                 /* (EWD031)        */
            if fieldnr% <> 0% then L28620
                inpmessage$ = edtmessage$
                return

L28620
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn5_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn5_msg  :  data                                               ~
         "Enter a Valid Shape Quantity?                                ",~
         "Enter a Valid Model Code?                                    ",~
         "Enter a Valid Glass Code?                                    ",~
         "Enter a Valid Special Shape Configuration Code with Seq. No.?",~
         "Enter Valid Data for (          )?                           ",~
         "Enter Valid Data for (          )?                           ",~
         "Enter Valid Data for (          )?                           ",~
         "Enter Valid Data for (          )?                           ",~
         "Enter Valid Data for (          )?                           ",~
         "Enter Valid Data for (          )?                           ",~
         "Enter Valid Data for (          )?                           ",~
         "Enter Valid Data for Label Text?                             "


        deffn'100(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28700
                inpmessage$ = edtmessage$
                return

L28700
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn6_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn6_msg  :  data                                               ~
         "Enter 1=Calc,2=Lab/Bat,3=ReMake(All)                         ",~
         "Enter Machine Selection, 0=Both, 1=GED, 2=Bilco? Def. = '0'? ",~
         "Enter a Valid Planning Production Date?                      ",~
         "Enter a Valid Department Selection or All?                   ",~
         "Enter a Valid Shift Selection or 'AA' = All Shifts?          ",~
         "Enter the Completion Date for the Glass?                     ",~
         "Enter a Production Load Number?                              "

      REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$, scr_dte$, scr_dte1$,       ~
                      dt_key1$, scr_sel$, size$, scr_msg$, scr_dept$,    ~
                      scr_msg1$, scr_shft$, scr_shft_d$, glass_dte$,     ~
                      glass_dte1$, ld_key$, scr_load$, scr_desc$,        ~
                      ged_bilco$, ged_desc$, save_part$, dt_key$,        ~
                      dt_desc$, dt_part$, dt_desc$, cut$(), ged$(),      ~
                      bg_date$, bg_dte$, ed_date$, ed_dte$, rp_sel$,     ~
                      rp_sel_d$, gl_sort$, rm_load$, sort$,  ~
                      sh$(), shc$(), tt$(), bil$(), sh_model$, ttt$(),   ~
                      sh_config$, sh_model_d$, sh_config_d$,             ~
                      sh_cnt$, sh_cnfig$, sh_cnfig_d$, sh_flags$(),      ~
                      sh_glass$, sh_glass_d$, sh_config_seq$, sh_txt$,   ~
                      new_part$, dim1es$, dim2es$, dim3es$, glsSrch$

            init(" ") srt$                               /* (AWD105) */

            init(" ") ds_batch$, reschedBatch$           /* (AWD081) */
            init(" ") specialmull$                       /* (AWD097) */
                                                       /* (EWD012)     */
            txt$(1%) =                                                   ~
                "**************************************************"
            txt$(2%) =                                                   ~
                "*   (New) Glass Processing and Batch Creation    *"
            txt$(3%) =                                                   ~
                "*       ( E W D ) G l a s s   S y s t e m        *"
            txt$(4%) =                                                   ~
                "**************************************************"

            note$( 1%) =                                                 ~
                "Note: Please note that the 'Batch Name' will be   "
            note$( 2%) =                                                 ~
                "      used and necessary when you are ready to    "
            note$( 3%) =                                                 ~
                "      print the applicable glass labels. This     "
            note$( 4%) =                                                 ~
                "      'Batch Name' needs to be unique and should  "
            note$( 5%) =                                                 ~
                "      be descriptive for the applicable Glass     "
            note$( 6%) =                                                 ~
                "      production run. When printing 'Glass Labels'"
            note$( 7%) =                                                 ~
                "      the 'Production Date' and 'Batch Name' will "
            note$( 8%) =                                                 ~
                "      be required.                                "
            note$( 9%) =                                                 ~
                "                                                  "
            note$(10%) =                                                 ~
                "                                                  "
            ff$      = "MMDD@DPT"
            volume$  = "CARLOS"
            library$ = "APCDATA "
            mat sh  = zer                               /* (EWD031)    */
            mat shc = zer                               /* (EWD031)    */

            sh_qty% = 0%
            sh_qty$ = "    "
            sh_cnt% = 0%
            sh_config_seq$ = "01"
            tempScrn% = 0%                             /*  (EWD055)    */
            lamnScrn% = 0%                             /* (AWD123)     */
            purge_temp% = 0%                           /*  (EWD055)    */
            temppreScrn% = 0%
        return

        initialize_variables_temp                      /*  (EWD055)    */
            init(" ") errormsg$, inpmessage$, scr_dte$, scr_dte1$,       ~
                      dt_key1$, scr_sel$, size$, scr_msg$, scr_dept$,    ~
                      scr_msg1$, scr_shft$, scr_shft_d$, glass_dte$,     ~
                      glass_dte1$, ld_key$, scr_load$, scr_desc$,        ~
                      ged_bilco$, ged_desc$, save_part$, dt_key$,        ~
                      dt_desc$, dt_part$, dt_desc$, cut$(), ged$(),      ~
                      bg_date$, bg_dte$, ed_date$, ed_dte$, rp_sel$,     ~
                      rp_sel_d$, gl_sort$, rm_load$, sort$,  ~
                      sh$(), shc$(), tt$(), bil$(), sh_model$, ttt$(),   ~
                      file$, bat$, dim1es$, dim2es$, dim3es$, glsSrch$

            tempScrn% = 0%
            lamnScrn% = 0%                              /* (AWD123)   */
            purge_temp% = 0%
            temppreScrn% = 0%
        return                                         /*  (EWD055)    */

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            if tempScrn% = 1% then goto startover_temp     /* (EWD055) */
            return clear all
            goto inputmode
            startover_temp
            return clear all                               /* (EWD055) */
            goto inputmode_tempered                        /* (EWD055) */

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            model$   = str(dt_rec$,189%,3%)     /* Model/Product       */
            dt_load$ = str(dt_rec$,1%,5%)       /* Production Load No. */
            dt_bar$  = str(dt_rec$,24%,18%)     /* Barcode Value       */
            dt_dept$ = str(dt_rec$,42%,3%)      /* Prod Dept. No.      */

REM            goto not_order

           if str(dt_bar$,1,10) <> "0927208501" then goto not_order
REM  AND STR(DT_BAR$,1,10) <> "0844863601" THEN GOTO NOT_ORDER

            if dt_dept$ = "000" then goto not_order
            if dt_dept$ = "001" then goto not_order
              call "SHOSTAT"("ORDER -->  " & dt_bar$)
                stop

not_order:


REM            if dt_dept$ <> "043" then goto notDept

REM CALL "SHOSTAT"("DEPT -->  " & DT_DEPT$)  STOP
REM NOTDEPT
            dt_date$ = str(dt_rec$,47%,6%)      /* Production Date     */
            dt_ref$  = str(dt_rec$,96%,8%)      /* Warranty Number     */
            ref% = 0%
REM GOSUB CHECK_REF
REM IF REF% = 1% THEN GOSUB TRACK_SEQ
            dt_shft$ = str(dt_rec$,104%,2%)     /* Shift Code          */
            dt_seq$  = str(dt_rec$,111%,5%)     /* Dept Seq. No.       */
            dt_cust$ = str(dt_rec$,124%,9%)     /* Customer Code       */
            dt_part$ = str(dt_rec$,189%,25%)    /* Part Number         */
            lt$ = str(dt_part$,7%,2%)           /* (AWD118) */
            dt_sash$ = str(dt_rec$,214%,1%)     /* Sash 1=T,2=B,3=FGO  */
            dt_prt$  = str(dt_rec$,215%,1%)     /* MFG Part Y or N     */
            dt_samp$ = str(dt_rec$,216%,1%)     /* Sample Y or N       */
            dt_wood$ = str(dt_rec$,217%,3%)     /* Wood Surround Code  */
            dt_special$ = str(dt_rec$,220%,10%) /* Special Prod Flags  */
            dt_txt$  = str(dt_rec$,236%,4%)     /* Line Item Text Id   */
            dt_gls$ = "Y"
            calsl% = 0%
            if str(dt_part$,1,3) = "B70" or str(dt_part$,1,3) = "B71" ~
                                             then calsl% = 1%
            if str(dt_part$,5%,2%) = "00" then dt_gls$ = "N"
/*(AWD109) */
REM ULTRA$ = STR(DT_REC$,254%,1%)                 /* (CR1990) */


            so_inv$  = str(dt_bar$,1%,8%)        /* (AWD083) */
            item_no$ = str(dt_bar$,9%,2%)        /* (AWD083) */
            gosub lookup_sub_part                /* (AWD083) */
            sub_part$ = str(bcksubpt_rec$,48%,20%)
            new_part$ = str(dt_part$,1%,25%) & str(sub_part$,1%,20%)
            series$ = str(bcksubpt_rec$,169%,16%)
            style$  = str(bcksubpt_rec$,185%,10%)
            if series$ = " " or str(series$,1%,1%) = "0" then          ~
                                                      gosub lookupSeries
            specialmull$ = str(bcksubpt_rec$,152%,1%)  /* (AWD097) */
/* (AWD111) */
REM  CALL "SHOSTAT" ("SPECIAL MULL" & SPECIALMULL$ & " SEQ " & DT_SEQ$)

            sdl% = 0%
            if str(sub_part$,8,1) = "1" and str(sub_part$,9,1) <> "2" ~
                                      then sdl% = 1%
/* (AWD111\) */
/* (CR1987) */
            dt_prv$ = str(dt_rec$,243%,2%)
        return
        check_ref
REM         if dt_ref$ = "22569929" then ref% = 1%
REM      if dt_ref$ = "22561998" then ref% = 1%
REM      if dt_ref$ = "22562073" then ref% = 1%
REM      if dt_ref$ = "22562039" then ref% = 1%
REM      if dt_ref$ = "22561970" then ref% = 1%
REM      if dt_ref$ = "22561971" then ref% = 1%
REM      if dt_ref$ = "22561969" then ref% = 1%
REM      if dt_ref$ = "22561990" then ref% = 1%
REM      if dt_ref$ = "22561972" then ref% = 1%
REM      if dt_ref$ = "22565364" then ref% = 1%
REM      if dt_ref$ = "22562037" then ref% = 1%
REM      if dt_ref$ = "22562038" then ref% = 1%
        return


        track_seq
          call "SHOSTAT" (" REF -->  " & dt_ref$ & " DT_DEPT --> " & ~
                           dt_dept$)
          stop
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

                                                   /* (EWD012) - Begin */
                                                   /* (EWD014)         */
                                                   /* (EWDGLSXX) Same  */
L35050:    FMT CH(01),                             /* Glass Process Cd */~
               CH(06),                             /* Glass Prod Date  */~
               CH(20),                             /* Unique Batch ID  */~
               CH(05),                             /* sort Number      */~
               CH(09),                             /* Glass Barcode    */~
               CH(03),                             /* Glass Re-Make No */~
               CH(02),                             /* Glass Type       */~
               CH(07),                             /* Grid/Liting      */~
               CH(08),                             /* Sales Order      */~
               CH(10),                             /* Calc Width       */~
               CH(03),                             /* Model Code       */~
               CH(10),                             /* Calc Height      */~
               CH(02),                             /* Shift Code       */~
               CH(05),                             /* Seq No Production*/~
               CH(07),                             /* Actual Window Wd */~
               CH(02),                             /* Color - Lock     */~
               CH(06),                             /* Actual Window  Hg*/~
               CH(03),                             /* Depart Code      */~
               CH(40),                             /* Text Line (1)    */~
               CH(40),                             /* Text Line (2)    */~
               CH(03),                             /* Top or Bot       */~
               CH(06),                             /* Color            */~
               CH(01),                             /* Contor Grid      */~
               CH(03),                             /* Print Re-Make No */~
               CH(01),                             /* Stock Flag (Spec)*/~
               CH(09),                             /* Base    (EWD031) */~
               CH(09),                             /* Left    (EWD031) */~
               CH(09),                             /* Right   (EWD031) */~
               CH(09),                             /* Top     (EWD031) */~
               CH(09),                             /* S1      (EWD031) */~
               CH(03),                             /* Config  (EWD031) */~
               CH(02),                             /* C Seq   (EWD031) */~
               CH(01),                             /* Screen Code      */~
               CH(02),                             /* Intercept(AWD112)*/~
               CH(10),                             /* (CR2305) Sandwich*/~
               CH(20)                              /* (CR2305) Lowe Num*/

                                                   /* (EWD012) - End   */
                                                   /* (EWD014)         */

                                                   /* (EWD056)         */
                                                   /* (EWDGLSWW) Work  */
L35060:    FMT CH(01),                             /* Glass Process Cd */~
               CH(06),                             /* Glass Prod Date  */~
               CH(20),                             /* Unique Batch ID  */~
               CH(05),                             /* sort Number      */~
               CH(09),                             /* Glass Barcode    */~
               CH(03),                             /* Glass Re-Make No */~
               CH(02),                             /* Glass Type       */~
               CH(07),                             /* Grid/Liting      */~
               CH(08),                             /* Sales Order      */~
               CH(10),                             /* Calc Width       */~
               CH(03),                             /* Model Code       */~
               CH(10),                             /* Calc Height      */~
               CH(02),                             /* Shift Code       */~
               CH(05),                             /* Seq No Production*/~
               CH(07),                             /* Actual Window Wd */~
               CH(02),                             /* Color an LockCode*/~
               CH(06),                             /* Actual Window  Hg*/~
               CH(03),                             /* Depart Code      */~
               CH(40),                             /* Text Line (1)    */~
               CH(40),                             /* Text Line (2)    */~
               CH(03),                             /* Top or Bot       */~
               CH(06),                             /* Color            */~
               CH(01),                             /* Contor Grid      */~
               CH(03),                             /* Print Re-Make No */~
               CH(01),                             /* Stock Flag (Spec)*/~
               CH(09),                             /* Base    (EWD031) */~
               CH(09),                             /* Left    (EWD031) */~
               CH(09),                             /* Right   (EWD031) */~
               CH(09),                             /* Top     (EWD031) */~
               CH(09),                             /* S1      (EWD031) */~
               CH(03),                             /* Config  (EWD031) */~
               CH(02),                             /* C Seq   (EWD031) */~
               CH(01),                             /* Screen Code      */~
               CH(02),                             /* Filler Area      */~
               CH(50),                             /* Shape Glass Sort */~
               CH(03),                             /* Filler  (CR503)  */~
               CH(20),                      /* bil$() fields 01 (CR503)*/~
               CH(20),                      /* bil$() fields 02 (CR503)*/~
               CH(20),                      /* bil$() fields 03 (CR503)*/~
               CH(20),                      /* bil$() fields 04 (CR503)*/~
               CH(20),                      /* bil$() fields 05 (CR503)*/~
               CH(20),                      /* bil$() fields 06 (CR503)*/~
               CH(20),                      /* bil$() fields 07 (CR503)*/~
               CH(20),                      /* bil$() fields 08 (CR503)*/~
               CH(20),                      /* bil$() fields 09 (CR503)*/~
               CH(20),                      /* bil$() fields 10 (CR503)*/~
               CH(20),                      /* bil$() fields 11 (CR503)*/~
               CH(20),                      /* bil$() fields 12 (CR503)*/~
               CH(20),                      /* bil$() fields 13 (CR503)*/~
               CH(20),                      /* bil$() fields 14 (CR503)*/~
               CH(20),                      /* bil$() fields 15 (CR503)*/~
               CH(25)                              /* Part Number      */

                                                   /* Key              */
                                                   /* (EWD056) - End   */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L40070:       gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                 ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40220,         /* Process Selection */   ~
                                L40220,         /* 0=Both,1=GED,2=Bil*/   ~
                                L40210,         /* Production Date   */   ~
                                L40210,         /* Department Code   */   ~
                                L40210,         /* Shift Code        */   ~
                                L40200,         /* Glass Production  */   ~
                                L40210          /* Load Number       */
              goto L40240

L40200:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40210:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40220:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40240:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(32),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,20), fac(hex(a4)), apc$                   , ch(41),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Process Selection(1-7):",                    ~
               at (04,26), fac(lfac$(1%)), scr_sel$             , ch(01),~
               at (04,30), fac(lfac$(1%)), size$                , ch(04),~
/*(AWD081)*/   at (04,35), fac(lfac$(1%)), ds_batch$            , ch(01),~
               at (04,40), fac(hex(84)), scr_msg$               , ch(30),~
               at (05,02), "Machine-Both,GED,Bilco:",                    ~
               at (05,26), fac(lfac$(2%)), ged_bilco$           , ch(01),~
               at (05,35), fac(lfac$(2%)), reschedBatch$        , ch(01),~
               at (05,40), fac(hex(84)), ged_desc$              , ch(20),~
               at (06,02), "Planned Prod. Date    :",                    ~
               at (06,26), fac(lfac$(3%)), scr_dte$             , ch(08),~
               at (07,02), "Department Code, (A)ll:",                    ~
               at (07,26), fac(lfac$(4%)), scr_dept$            , ch(03),~
               at (07,40), fac(hex(84)), scr_msg1$              , ch(30),~
               at (08,02), "Shift Code or AA = All:",                    ~
               at (08,26), fac(lfac$(5%)), scr_shft$            , ch(02),~
               at (08,40), fac(hex(84)), scr_shft_d$            , ch(30),~
               at (09,02), "Glass Production Date :",                    ~
               at (09,26), fac(lfac$(6%)), glass_dte$           , ch(08),~
               at (10,02), "Load Number or Blank  :",                    ~
               at (10,26), fac(lfac$(7%)), scr_load$            , ch(05),~
               at (10,40), fac(hex(84)),   scr_desc$            , ch(30),~
                                                                         ~
               at (11,21), fac(hex(84)), scr$(1%)               , ch(40),~
               at (12,21), fac(hex(84)), scr$(2%)               , ch(40),~
               at (13,21), fac(hex(84)), scr$(3%)               , ch(40),~
               at (14,21), fac(hex(84)), scr$(4%)               , ch(40),~
               at (15,21), fac(hex(84)), scr$(5%)               , ch(40),~
               at (16,21), fac(hex(84)), scr$(6%)               , ch(40),~
               at (17,21), fac(hex(84)), scr$(7%)               , ch(40),~
               at (18,21), fac(hex(84)), scr$(8%)               , ch(40),~
               at (19,21), fac(hex(84)), scr$(9%)               , ch(40),~
               at (20,21), fac(hex(84)), scr$(10%)              , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 5% then goto L40550  /*  (EWD055)        */
                  goto inputmode_tempered


                                                  /* (EWD008) - Begin */
L40550:        if keyhit% <> 7% then goto L40600
                  run$ = "EWDPLN66"
                  gosub run_program
                  goto L40070
                                                  /* (EWD008) - End   */
                                                  /* (EWD005) - Begin */
L40600:        if keyhit% <> 9% then goto L40700
                  run$ = "EWDPLN64"
                  gosub run_program
                  goto L40070
                                                  /* (EWD005) - End   */
L40700:        if keyhit% <> 10% then goto L40730
                  glstype% = 1%                         /* (IM8022)     */
                  gosub analysis
                  goto L40070

L40730:        if keyhit% <> 12% then goto L40770
                  gosub purge_data
                  goto L40070

L40770:        if keyhit% <> 15% then goto L40810
                  call "PRNTSCRN"
                  goto L40240

L40810:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1                       /* (EWD005) Mod - PF(9)    */
                                      /* (EWD055) Mod - PF(5)    */
        if edit% = 2% then L41000     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (5)Tempered Glass      " &       ~
                      " (9)Glass Search       (14)Print Report"
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "(10)Glass Analysis     (15)Print Screen"
            pf$(3%) = "                 (7)Special Glass       " &       ~
                      "(12)Glass Purge        (16)Exit Program"
            pfkeys$ = hex(01ffff0405ff07ff090aff0cff0e0f1000)
            if fieldnr% = 1% then L40960
                str(pf$(1%),64%) = " "  :  str(pfkeys$,14%,1%) = hex(ff)
                str(pf$(3%),64%) = " "  :  str(pfkeys$,16%,1%) = hex(ff)
                str(pf$(3%),18%,18%) = " " : str(pfkeys$,7%,1%) = hex(ff)
                                                /* (EWD008)           */
L40960:     if fieldnr% > 1% then L40980
                str(pf$(2%),18%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40980:     return

L41000: if fieldnr% > 0% then L41090  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      " (9)Glass Search                       "
            pf$(2%) = "                                        " &       ~
                      "(10)Glass Analysis     (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "(12)Glass Purge        (16)Process Data"
            pfkeys$ = hex(01ffffffffffffff090aff0cffff0f1000)
            return
L41090:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Calculate Sizes for Screen Display                        *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42080          /* Stock Part Number */

              goto L42095

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42080:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42095:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(32),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (03,16), fac(hex(84)), txt$(1%)               , ch(50),~
               at (04,16), fac(hex(84)), txt$(2%)               , ch(50),~
               at (05,16), fac(hex(84)), txt$(3%)               , ch(50),~
               at (06,16), fac(hex(84)), txt$(4%)               , ch(50),~
                                                                         ~
               at (07,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (08,02), "Manufactured Window :",                      ~
               at (08,25), fac(lfac$(1%)), dt_part$             , ch(25),~
               at (08,52), fac(lfac$(1%)), dim1es$              , ch(10),~
               at (08,64), fac(lfac$(1%)), dim2es$              , ch(10),~
               at (09,12), fac(hex(84)), dt_desc$               , ch(32),~
                                                                         ~
               at (10,02), fac(hex(84)), cut$(1%)               , ch(55),~
               at (11,02), fac(hex(84)), cut$(2%)               , ch(55),~
               at (12,02), fac(hex(84)), cut$(3%)               , ch(55),~
               at (13,02), fac(hex(84)), cut$(4%)               , ch(55),~
               at (14,02), fac(hex(84)), cut$(5%)               , ch(55),~
                                                                         ~
               at (10,58), fac(hex(84)), ged$(1%)               , ch(20),~
               at (11,58), fac(hex(84)), ged$(2%)               , ch(20),~
               at (12,58), fac(hex(84)), ged$(3%)               , ch(20),~
               at (13,58), fac(hex(84)), ged$(4%)               , ch(20),~
               at (14,58), fac(hex(84)), ged$(5%)               , ch(20),~
                                                                         ~
               at (16,02), fac(hex(84)), cut$(6%)               , ch(55),~
               at (17,02), fac(hex(84)), cut$(7%)               , ch(55),~
               at (18,02), fac(hex(84)), cut$(8%)               , ch(55),~
               at (19,02), fac(hex(84)), cut$(9%)               , ch(55),~
               at (20,02), fac(hex(84)), cut$(10%)              , ch(55),~
                                                                         ~
               at (16,58), fac(hex(84)), ged$(6%)               , ch(20),~
               at (17,58), fac(hex(84)), ged$(7%)               , ch(20),~
               at (18,58), fac(hex(84)), ged$(8%)               , ch(20),~
               at (19,58), fac(hex(84)), ged$(9%)               , ch(20),~
               at (20,58), fac(hex(84)), ged$(10%)              , ch(20),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L42345
                  call "PRNTSCRN"
                  goto L42095

L42345:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L42420     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Screen "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return

L42420: if fieldnr% > 0% then L42465  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (14)Calculate   "
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Screen "
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L42465:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Optimizer Batch Name(s)                                   *~
            *************************************************************

        batch_header
            gosub set_pf3
L43080:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(32),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
                                                                         ~
               at (03,16), fac(hex(84)), txt$(1%)               , ch(50),~
               at (04,16), fac(hex(84)), txt$(2%)               , ch(50),~
               at (05,16), fac(hex(84)), txt$(3%)               , ch(50),~
               at (06,16), fac(hex(84)), txt$(4%)               , ch(50),~
               at (08,02), "Batch Description :",                        ~
               at (08,25), fac(hex(81)), file$                  , ch(20),~
                                                                         ~
               at (10,16), fac(hex(84)), note$( 1%)             , ch(50),~
               at (11,16), fac(hex(84)), note$( 2%)             , ch(50),~
               at (12,16), fac(hex(84)), note$( 3%)             , ch(50),~
               at (13,16), fac(hex(84)), note$( 4%)             , ch(50),~
               at (14,16), fac(hex(84)), note$( 5%)             , ch(50),~
               at (15,16), fac(hex(84)), note$( 6%)             , ch(50),~
               at (16,16), fac(hex(84)), note$( 7%)             , ch(50),~
               at (17,16), fac(hex(84)), note$( 8%)             , ch(50),~
               at (18,16), fac(hex(84)), note$( 9%)             , ch(50),~
               at (19,16), fac(hex(84)), note$(10%)             , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L43310
                  call "PRNTSCRN"
                  goto L43080

L43310:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
               txt$(2%) =                                                ~
                    "*  (Optimization) Batch Processing Description   *"
            inpmessage$ = "Enter Description for Optimization Batches"
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (14)Process Data"
            pf$(2%) = "                  (10) Process Data (No " &       ~
                      "Labels)                (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffff0affffff0e0f1000)
            if scr_sel% = 2% or scr_sel% = 6% then return
               str(pf$(2%),18%,32%) = " "           /* Not Applicable */
               str(pfkeys$,10%,1%)  = hex(ff)       /* for Re-Makes   */

            return



        REM *************************************************************~
            *    A N A L Y S I S   A N D   P U R G E   U T I L I T Y    *~
            *************************************************************

        analysis
                                            /* (EWD010) -Oven Tracking*/
            call "APCPLD45" (1%,            /* Called From (APCPLA45) */ ~
                             "000",         /* Dept Not Applicable    */ ~
                             scr_shft$,     /* Shift Selection        */ ~
                             glstype%,      /* 1-Anneal 2-Temp (I8022)*/ ~
/*(EWD055)*/                 #8,            /* File (APCPLNGR)        */ ~
                             #3    )        /* (GENCODES              */
                                            /* (EWD010) - Tracking    */
        return

        purge_data
                                               /* Only Purge Completed */
                                               /* Glass Two (2) Weeks  */
                                               /* Old, No Re-Makes     */

            init(" ") rm_ky$, pg_dte$, pg_dte1$, rm_cnt$, pg_dte2$,      ~
                      rm_flag$
            rm_ky$ = all(hex(00))
            rm_cnt$ = "Checked [XXXXXXXX] Deleted [xxxxxxxx]"
            cnt% = 0% : cnt1% = 0%
            call "SHOSTAT" ("Purging Glass Data")
            pg_dte$ = date : err% = 0%
            call "DATE" addr("G+",pg_dte$,-30%,pg_dte1$,err%)
            call "DATE" addr("G+",pg_dte$,-7%,pg_dte2$,err%)
        REM  Purge Override
        REM  rhh$ = "11/27/98"
        REM  call "DATUNFMT" (rhh$)
        REM  pg_dte2$ = str(rhh$,1%,6%)

        REM str(rm_ky$,1%,6%) = pg_dte1$        /* (EWD005) Keep 30 days*/
                                                /* of stock glass       */
        purge_nxt
/* (EWD055) */
            read #8,hold,key 1% > rm_ky$, using L44250, rm_ky$, rm_flag$, ~
                                                      eod goto purge_done
L44250:        FMT POS(7), CH(27), POS(163), CH(1)
            cnt% = cnt% + 1%
            if mod(cnt%,50%) <> 0% then goto L44320
               convert cnt% to str(rm_cnt$,10%,8%), pic(########)

               convert cnt1% to str(rm_cnt$,29%,8%), pic(########)

               print at(03,22);hex(84);rm_cnt$;

L44320:     if str(rm_ky$,1%,6%) > pg_dte2$ then goto purge_done
                                         /* Save Stock for 30 Days   */
               if rm_flag$ = "S" and str(rm_ky$,1%,6%) > pg_dte1$ then ~
                                                 goto purge_nxt
/* (EWD055) */
               delete #8                   /* Delete all Prior to Date */
               cnt1% = cnt1% + 1%
               goto purge_nxt
        purge_done
        return

        REM *************************************************************~
            *               S C R E E N   P A G E   4                   *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
              gosub'070(1%, fieldnr%)
              gosub set_pf5
              if fieldnr% > 0% then init(hex(8c)) lfac$()                 ~
                               else init(hex(86)) lfac$()
                                                /* (EWD070)          */
              on fieldnr% gosub L45150,         /* Beg/End Prod Date */   ~
                                L45160,         /* Report Type       */   ~
                                L45160          /* Depart Sel/Window */

              goto L45180

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L45150:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L45160:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L45180:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(32),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (03,16), fac(hex(84)), txt$(1%)               , ch(50),~
               at (04,16), fac(hex(84)), txt$(2%)               , ch(50),~
               at (05,16), fac(hex(84)), txt$(3%)               , ch(50),~
               at (06,16), fac(hex(84)), txt$(4%)               , ch(50),~
                                                                         ~
               at (07,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (08,02), "Beginning Prod Date :",                      ~
               at (08,25), fac(lfac$(1%)), bg_date$             , ch(10),~
                                                                         ~
               at (08,40), "Ending Prod. Date   :",                      ~
               at (08,66), fac(lfac$(1%)), ed_date$             , ch(10),~
                                                                         ~
               at (09,02), "Report Type         :",                      ~
               at (09,25), fac(lfac$(2%)), rp_sel$              , ch(01),~
               at (09,40), fac(hex(84)), rp_sel_d$              , ch(30),~
                                                                         ~
               at (10,02), "Dept Code & Time Win:",                      ~
               at (10,25), fac(lfac$(3%)), scr_dept$            , ch(03),~
               at (10,30), fac(lfac$(3%)), scr_wind$            , ch(01),~
               at (10,40), fac(hex(84)), scr_msg1$              , ch(30),~
                                                                         ~
               at (11,21), fac(hex(84)), rpt$(1%)               , ch(40),~
               at (12,21), fac(hex(84)), rpt$(2%)               , ch(40),~
               at (13,21), fac(hex(84)), rpt$(3%)               , ch(40),~
               at (14,21), fac(hex(84)), rpt$(4%)               , ch(40),~
               at (15,21), fac(hex(84)), rpt$(5%)               , ch(40),~
               at (16,21), fac(hex(84)), rpt$(6%)               , ch(40),~
               at (17,21), fac(hex(84)), rpt$(7%)               , ch(40),~
               at (18,21), fac(hex(84)), rpt$(8%)               , ch(40),~
               at (19,21), fac(hex(84)), rpt$(9%)               , ch(40),~
               at (20,21), fac(hex(84)), rpt$(10%)              , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L45600
                  call "PRNTSCRN"
                  goto L45180

L45600:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf5
        if edit% = 2% then L45750     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Screen "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return

L45750: if fieldnr% > 0% then L45840  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (14)Print Report"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Screen "
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L45840:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return


        REM *************************************************************~
            *               S C R E E N   P A G E   5   (EWD031)        *~
            *************************************************************

                                                /* (EWD071)         */
        deffn'105(fieldnr%, edit%)
              gosub'090(1%, fieldnr%)
              gosub set_pf7
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L47160,         /* shape Quantity   */   ~
                                L47155,         /* Model Code       */   ~
                                L47155,         /* Glass Code       */   ~
                                L47160,         /* Shapes Code      */   ~
                                L47160,         /* Base Entry       */   ~
                                L47160,         /* Left Entry       */   ~
                                L47160,         /* Right Entry      */   ~
                                L47160,         /* Top Entry        */   ~
                                L47160,         /* S1 Entry         */   ~
                                L47160,         /* S2 Entry         */   ~
                                L47155,         /* Nest Entry       */   ~
                                L47150          /* Label Text       */


              goto L47180

L47150:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L47155:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L47160:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L47180:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(32),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
                                                                         ~
               at (03,16), fac(hex(84)), txt$(1%)               , ch(50),~
               at (04,16), fac(hex(84)), txt$(2%)               , ch(50),~
               at (05,16), fac(hex(84)), txt$(3%)               , ch(50),~
               at (06,16), fac(hex(84)), txt$(4%)               , ch(50),~
                                                                         ~
               at (07,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (08,02), "Special Shape Quantity               ",      ~
               at (08,36), fac(lfac$(1%)), sh_qty$              , ch(04),~
                                                                         ~
               at (08,60), fac(hex(84)), sh_batch$              , ch(20),~
                                                                         ~
               at (09,02), "Model Code                           ",      ~
               at (09,36), fac(lfac$(2%)), sh_model$            , ch(03),~
               at (09,50), fac(hex(85))  , sh_model_d$          , ch(30),~
                                                                         ~
               at (10,02), "Special Shape Glass Code             ",      ~
               at (10,36), fac(lfac$(3%)), sh_glass$            , ch(02),~
               at (10,50), fac(hex(85))  , sh_glass_d$          , ch(30),~
                                                                         ~
               at (11,02), "Shape Configuration Code             ",      ~
               at (11,36), fac(lfac$(4%)), sh_config$           , ch(02),~
               at (11,40), fac(lfac$(4%)), sh_config_seq$       , ch(02),~
               at (11,50), fac(hex(85)), sh_config_d$           , ch(30),~
                                                                         ~
               at (12,02), " Base   ",                                   ~
               at (12,10), fac(hex(85)), tt$(1%)                , ch(25),~
               at (12,36), fac(lfac$(5%)), sh$(1%)              , ch(09),~
               at (12,50), "Calculated",                                 ~
               at (12,61), fac(hex(85)), ttt$(1%)               , ch(08),~
               at (12,70), fac(hex(85)), shc$(1%)               , ch(09),~
                                                                         ~
               at (13,02), " Left   ",                                   ~
               at (13,10), fac(hex(85)), tt$(2%)                , ch(25),~
               at (13,36), fac(lfac$(6%)), sh$(2%)              , ch(09),~
               at (13,50), "Calculated",                                 ~
               at (13,61), fac(hex(85)), ttt$(2%)               , ch(08),~
               at (13,70), fac(hex(85)), shc$(2%)               , ch(09),~
                                                                         ~
               at (14,02), " Right  ",                                   ~
               at (14,10), fac(hex(85)), tt$(3%)                , ch(25),~
               at (14,36), fac(lfac$(7%)), sh$(3%)              , ch(09),~
               at (14,50), "Calculated",                                 ~
               at (14,61), fac(hex(85)), ttt$(3%)               , ch(08),~
               at (14,70), fac(hex(85)), shc$(3%)               , ch(09),~
                                                                         ~
               at (15,02), " Top    ",                                   ~
               at (15,10), fac(hex(85)), tt$(4%)                , ch(25),~
               at (15,36), fac(lfac$(8%)), sh$(4%)              , ch(09),~
               at (15,50), "Calculated",                                 ~
               at (15,61), fac(hex(85)), ttt$(4%)               , ch(08),~
               at (15,70), fac(hex(85)), shc$(4%)               , ch(09),~
                                                                         ~
               at (16,02), " S1     ",                                   ~
               at (16,10), fac(hex(85)), tt$(5%)                , ch(25),~
               at (16,36), fac(lfac$(9%)), sh$(5%)              , ch(09),~
               at (16,50), "Calculated",                                 ~
               at (16,61), fac(hex(85)), ttt$(5%)               , ch(08),~
               at (16,70), fac(hex(85)), shc$(5%)               , ch(09),~
                                                                         ~
               at (17,02), " S2     ",                                   ~
               at (17,10), fac(hex(85)), tt$(6%)                , ch(25),~
               at (17,36), fac(lfac$(10%)), sh$(6%)             , ch(09),~
               at (17,50), "Calculated",                                 ~
               at (17,61), fac(hex(85)), ttt$(6%)               , ch(08),~
               at (17,70), fac(hex(85)), shc$(6%)               , ch(09),~
                                                                         ~
               at (18,02), " Nest N ",                                   ~
               at (18,10), fac(hex(85)), tt$(7%)                , ch(25),~
               at (18,36), fac(lfac$(11%)), sh$(7%)             , ch(01),~
               at (18,50), "Calc Size",                                  ~
               at (18,70), fac(hex(85)), shc$(7%)               , ch(09),~
                                                                         ~
               at (19,02), "Label Text                           ",      ~
               at (19,36), fac(lfac$(12%)), sh_txt$             , ch(18),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
                                                           /* (EWD071)   */
               if keyhit% <> 3%  then goto L47200
                  gosub special_shape_remake
                                                           /* (EWD071)   */
L47200:        if keyhit% <> 10% then goto L47400          /*  (EWD033)  */
                   goto display_analysis

L47400:        if keyhit% <> 12% then goto L47500
                  gosub save_shape_data

L47500:        if keyhit% <> 15% then goto L47600
                  call "PRNTSCRN"
                  goto L47180

L47600:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf7
           if fieldnr% < 4% then goto set_pf7_skip
           if fieldnr% > 11% then goto set_pf7_skip

              rr% = fieldnr% - 4%
             if str(sh_flags$(rr%),2%,1%) = "W" then                     ~
                str(inpmessage$,23%,10%) = "Width     "
             if str(sh_flags$(rr%),2%,1%) = "H" then                     ~
                str(inpmessage$,23%,10%) = "Height    "
             if str(sh_flags$(rr%),2%,1%) = "R" then                     ~
                str(inpmessage$,23%,10%) = "Radius    "
             if str(sh_flags$(rr%),2%,1%) = "N" then                     ~
                str(inpmessage$,23%,10%) = "Not Applic"

             if str(sh_flags$(rr%),2%,1%) = "L" then                     ~
                str(inpmessage$,23%,10%)  = "Leg Height"
             if str(sh_flags$(rr%),2%,1%) = "S" then                     ~
                str(inpmessage$,23%,10%)  = "Side Leg  "
             if str(sh_flags$(rr%),2%,1%) = "T" then                     ~
                str(inpmessage$,23%,10%)  = "Top  Leg  "
             if str(sh_flags$(rr%),2%,1%) = "X" then                     ~
                str(inpmessage$,23%,10%)  = "SlegHeight"

        set_pf7_skip
            txt$(1%) =                                                   ~
                "**************************************************"
            txt$(2%) =                                                   ~
                "*       Special Shapes Glass Entry Screen        *"
            txt$(3%) =                                                   ~
                "*       ( E W D ) G l a s s   S y s t e m        *"
            txt$(4%) =                                                   ~
                "**************************************************"

        sh_batch$ = "Batch Count: [####]"
        convert sh_cnt% to str(sh_batch$,15%,4%), pic(####)
        if sh_error% <> 0% then gosub L47845            /* Batch Limit */
                                      /* (EWD033)                */
                                      /* (EWD071)                */
        if edit% = 2% then L47750     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                 (4) Previous Field     " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(3)Process Rmks (10) Select Shape Data  " &       ~
                      "                       (16)Process Glas"
            pfkeys$ = hex(01ff0304ffffffffff0affffffff0f1000)
                                                          /*  (EWD033)  */
            if sh_cnt% > 0% then goto L47610
               str(pf$(3%),16%,24%) = " " : str(pfkeys$,10%,1%) = hex(ff)

L47610:     if fieldnr% > 1% then L47620
                str(pf$(2%),18%,20%) = " " : str(pfkeys$,4%,1%) = hex(ff)
                return
L47620:     str(pf$(3%),64%)     = " " :  str(pfkeys$,16%,1%) = hex(ff)
            str(pf$(3%),1%,15%)  = " " :  str(pfkeys$,03%,1%) = hex(ff)
           return
                                                         /* (EWD071)    */
L47750: if fieldnr% > 0% then L47840  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                   (12)Save Calculation"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Process Glas"
            pfkeys$ = hex(01ffffffffffffffffffff0cffff0f1000)
            return
L47840:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

L47845:     errormsg$ = "(Error) Max records in Batch reached !!!!!"
            gosub error_prompt
        return
                                                    /* (EWD031)         */



        REM *************************************************************~
            *           D I S P L A Y   S U M M A R Y   S C R E E N     *~
            *-----------------------------------------------------------*~
            * Display Screen                                    EWD033 *~
            *************************************************************

        display_analysis
            k% = 0%
L50000:     gosub set_pf_analysis
            accept                                                       ~
               at (01,64), fac(hex(84)), pageno$                , ch(14),~
               at (02,64), fac(hex(84)), "Date:"                        ,~
               at (02,69), fac(hex(84)), date$                  , ch(10),~
               at (03,64), fac(hex(84)), "Time:"                        ,~
               at (03,69), fac(hex(84)), time$                  , ch(10),~
                                                                         ~
               at (02,18), fac(hex(a4)), scr_title$             , ch(45),~
                                                                         ~
               at (05,04), fac(hex(a4))  , h1$                  , ch(04),~
               at (05,09), fac(hex(a4))  , h2$                  , ch(03),~
               at (05,13), fac(hex(a4))  , h3$                  , ch(02),~
               at (05,16), fac(hex(a4))  , h4$                  , ch(02),~
               at (05,19), fac(hex(a4))  , h5$                  , ch(09),~
               at (05,29), fac(hex(a4))  , h6$                  , ch(09),~
               at (05,39), fac(hex(a4))  , h7$                  , ch(09),~
               at (05,49), fac(hex(a4))  , h8$                  , ch(09),~
               at (05,59), fac(hex(a4))  , h9$                  , ch(09),~
               at (05,69), fac(hex(a4))  , h10$                 , ch(09),~
                                                                         ~
               at (06,02), fac(hex(81))  , cc$(k% + 1%)         , ch(01),~
               at (06,04), fac(hex(84))  , dt$(k% + 1%)         , ch(77),~
                                                                         ~
               at (07,02), fac(hex(81))  , cc$(k% + 2%)         , ch(01),~
               at (07,04), fac(hex(84))  , dt$(k% + 2%)         , ch(77),~
                                                                         ~
               at (08,02), fac(hex(81))  , cc$(k% + 3%)         , ch(01),~
               at (08,04), fac(hex(84))  , dt$(k% + 3%)         , ch(77),~
                                                                         ~
               at (09,02), fac(hex(81))  , cc$(k% + 4%)         , ch(01),~
               at (09,04), fac(hex(84))  , dt$(k% + 4%)         , ch(77),~
                                                                         ~
               at (10,02), fac(hex(81))  , cc$(k% + 5%)         , ch(01),~
               at (10,04), fac(hex(84))  , dt$(k% + 5%)         , ch(77),~
                                                                         ~
               at (11,02), fac(hex(81))  , cc$(k% + 6%)         , ch(01),~
               at (11,04), fac(hex(84))  , dt$(k% + 6%)         , ch(77),~
                                                                         ~
               at (12,02), fac(hex(81))  , cc$(k% + 7%)         , ch(01),~
               at (12,04), fac(hex(84))  , dt$(k% + 7%)         , ch(77),~
                                                                         ~
               at (13,02), fac(hex(81))  , cc$(k% + 8%)         , ch(01),~
               at (13,04), fac(hex(84))  , dt$(k% + 8%)         , ch(77),~
                                                                         ~
               at (14,02), fac(hex(81))  , cc$(k% + 9%)         , ch(01),~
               at (14,04), fac(hex(84))  , dt$(k% + 9%)         , ch(77),~
                                                                         ~
               at (15,02), fac(hex(81))  , cc$(k% + 10%)        , ch(01),~
               at (15,04), fac(hex(84))  , dt$(k% + 10%)        , ch(77),~
                                                                         ~
               at (16,02), fac(hex(81))  , cc$(k% + 11%)        , ch(01),~
               at (16,04), fac(hex(84))  , dt$(k% + 11%)        , ch(77),~
                                                                         ~
               at (17,02), fac(hex(81))  , cc$(k% + 12%)        , ch(01),~
               at (17,04), fac(hex(84))  , dt$(k% + 12%)        , ch(77),~
                                                                         ~
               at (18,02), fac(hex(81))  , cc$(k% + 13%)        , ch(01),~
               at (18,04), fac(hex(84))  , dt$(k% + 13%)        , ch(77),~
                                                                         ~
               at (19,02), fac(hex(81))  , cc$(k% + 14%)        , ch(01),~
               at (19,04), fac(hex(84))  , dt$(k% + 14%)        , ch(77),~
                                                                         ~
               at (21,02), fac(hex(a4)),   dsp_msg$             , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L50020            /* First    */
L50010:           k% = 0%
                  goto L50000

L50020:        if keyhit% <> 3% then goto L50030           /* Last      */
L50040:           x% = int(val_max% / 14%)
                  k% = (x%*14%)
                  if (k% + 1%) > val_max% then k% = k% - 14%
                  goto L50000

L50030:        if keyhit% <> 4% then goto L50050           /* Previous */
                  if k% < 15% then goto L50010
                  k% = k% - 14%
                  if k% <= 1% then goto L50010
                  goto L50000

L50050:        if keyhit% <> 5% then goto L50060           /* Next     */
                  k% = k% + 14%
                  if k% < val_max% then goto L50000
                  goto L50040

L50060:        if keyhit% <> 15% then goto L50070
                  call "PRNTSCRN"
                  goto L50000

L50070:        if keyhit% <> 0% then goto L50075
                  gosub find_record

L50075:        if keyhit% <> 16% then goto L50000

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return clear all
        goto inputmode_special_shapes


        set_pf_analysis
            init(" ") h1$, h2$, h3$, h4$, h5$, h6$, h7$, h8$, dt$(),      ~
                      cc$(), rec$, scr_title$, rpt_time$, dsp_msg$
            dsp_msg$=                                                     ~
             "Use 'X' to Display Detail Information, followed by <Return>?"
            str(dsp_msg$,62%,15%) = "Total [ xxxxx ]"
            convert sh_cnt% to str(dsp_msg$,70%,5%), pic(#####)


            init(" ") time$
            call "TIME" (time$)

            scr_title$ = "Summary Display   -   Special Shapes Batch"

            pageno$ = "Page: XX of XX"             /* k% = Array Values */
            h1$  = "Qty "
            h2$  = "Mod"
            h3$  = "GL"
            h4$  = "CF"
            h5$  = "   Base   "
            h6$  = "   Left   "
            h7$  = "   Right  "
            h8$  = "   Top    "
            h9$  = "    S1    "
            h10$ = "    S2    "

            val_max% = sh_cnt%
            gosub load_screen
            if val_max% > (100% - 14%) then val_max% = 100% - 14%
                                                        /* Display Max */
            x = val_max%
            yy% = ( val_max% / 14% )
            if mod(x,14) <> 0 then yy% = yy% + 1%

            xx% = (k% / 14%) +1%
            if xx% > yy% then xx% = yy%

            convert xx% to str(pageno$,7%,2%), pic(##) /* Current Page*/

            convert yy% to str(pageno$,13%,2%), pic(##)/* Total Pages */


            pf$(1) = "(2)First           (5)Next              " &        ~
                     "                                       "

            pf$(2) = "(3)Last                                 " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(4)Previous                             " &        ~
                     "                       (16)Exit Display"
            pfkeys$ = hex(ff02030405ffffffffffffffffff0f1000)

            gosub check_screen
        return

        load_screen
           for i% = 1% to val_max%
               str(dt$(i%),1%,4%)  = str(bil$(i%,5%),1%,4%)
               str(dt$(i%),6%,3%)  = str(bil$(i%,1%),1%,3%)
               str(dt$(i%),10%,2%) = str(bil$(i%,20%),1%,2%)
               str(dt$(i%),13%,2%) = str(bil$(i%,2%),2%,2%)
               str(dt$(i%),16%,9%) = str(bil$(i%,6%),1%,9%)
               str(dt$(i%),26%,9%) = str(bil$(i%,8%),1%,9%)
               str(dt$(i%),36%,9%) = str(bil$(i%,10%),1%,9%)
               str(dt$(i%),46%,9%) = str(bil$(i%,12%),1%,9%)
               str(dt$(i%),56%,9%) = str(bil$(i%,14%),1%,9%)
               str(dt$(i%),66%,9%) = str(bil$(i%,16%),1%,9%)
            next i%
        return

        check_screen
            if val_max% > 14% then goto L50090
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L50090:      if k% >= 14% then goto L50100
                gosub no_first
                gosub no_prev
L50100:      if (k% + 14%) <= val_max% then goto L50110
                gosub no_last
L50110:      if k% <= (val_max% - 14%) then goto L50120
                gosub no_next
L50120: return
        no_first
            str(pf$(1%),1%, 9%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
            str(pf$(1%),20%, 9%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last
            str(pf$(2%),1%,9%)   = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
            str(pf$(3%),1%,12%)  = " " : str(pfkeys$,4%,1%) = hex(ff)
        return                                        /*  (EWD033) - END */


        REM *************************************************************~
            *               S C R E E N   P A G E   6                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen For Tempered Glass         *~
            *************************************************************

        deffn'106(fieldnr%, edit%)
L48070:       gosub'100(1%, fieldnr%)
              gosub set_pf8
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L48210,         /* Process Selection */   ~
                                L48220,         /* 0=Both,1=GED,2=Bil*/   ~
                                L48210,         /* Production Date   */   ~
                                L48210,         /* Department Code   */   ~
                                L48210,         /* Shift Code        */   ~
                                L48200,         /* Glass Production  */   ~
                                L48210          /* Load Number       */
              goto L48240

L48200:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L48210:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L48220:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L48240:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(32),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,20), fac(hex(a4)), apc1$                  , ch(41),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Process Selection(1-6):",                    ~
               at (04,26), fac(lfac$(1%)), scr_sel$             , ch(01),~
               at (04,30), fac(lfac$(1%)), size$                , ch(04),~
               at (04,40), fac(hex(84)), scr_msg$               , ch(30),~
               at (05,02), "Machine-Both,GED,Bilco:",                    ~
               at (05,26), fac(lfac$(2%)), ged_bilco$           , ch(01),~
               at (05,40), fac(hex(84)), ged_desc$              , ch(20),~
               at (06,02), "Planned Prod. Date    :",                    ~
               at (06,26), fac(lfac$(3%)), scr_dte$             , ch(08),~
               at (07,02), "Department Code, (A)ll:",                    ~
               at (07,26), fac(lfac$(4%)), scr_dept$            , ch(03),~
               at (07,40), fac(hex(84)), scr_msg1$              , ch(30),~
               at (08,02), "Shift Code or AA = All:",                    ~
               at (08,26), fac(lfac$(5%)), scr_shft$            , ch(02),~
               at (08,40), fac(hex(84)), scr_shft_d$            , ch(30),~
               at (09,02), "Glass Production Date :",                    ~
               at (09,26), fac(lfac$(6%)), glass_dte$           , ch(08),~
               at (10,02), "Load Number or Blank  :",                    ~
               at (10,26), fac(lfac$(7%)), scr_load$            , ch(05),~
               at (10,40), fac(hex(84)),   scr_desc$            , ch(30),~
                                                                         ~
               at (11,06), fac(hex(84)), scr_temp$(1%)          , ch(70),~
               at (12,06), fac(hex(84)), scr_temp$(2%)          , ch(70),~
               at (13,06), fac(hex(84)), scr_temp$(3%)          , ch(70),~
               at (14,06), fac(hex(84)), scr_temp$(4%)          , ch(70),~
               at (15,06), fac(hex(84)), scr_temp$(5%)          , ch(70),~
               at (16,06), fac(hex(84)), scr_temp$(6%)          , ch(70),~
               at (17,06), fac(hex(84)), scr_temp$(7%)          , ch(70),~
               at (18,06), fac(hex(84)), scr_temp$(8%)          , ch(70),~
               at (19,06), fac(hex(84)), scr_temp$(9%)          , ch(70),~
               at (20,06), fac(hex(84)), scr_temp$(10%)         , ch(70),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 10% then goto L48730
                  glstype% = 2%                         /* (IM8022)      */
                  gosub analysis
                  goto L48070

L48730:        if keyhit% <> 12% then goto L48770
                  purge_temp% = 1%
                  gosub purge_data
                  goto L48070

L48770:        if keyhit% <> 15% then goto L48810
                  call "PRNTSCRN"
                  goto L48240

L48810:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf8
        if edit% = 2% then L49000     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (14)Print Report"
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "(10)Glass Analysis     (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "(12)Glass Purge        (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffff0Aff0cff0e0f1000)
            if fieldnr% = 1% then L48960
                str(pf$(1%),64%) = " "  :  str(pfkeys$,14%,1%) = hex(ff)
                str(pf$(3%),64%) = " "  :  str(pfkeys$,16%,1%) = hex(ff)
                str(pf$(3%),18%,18%) = " " : str(pfkeys$,7%,1%) = hex(ff)
L48960:     if fieldnr% > 1% then L48980
                str(pf$(2%),18%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L48980:     return

L49000: if fieldnr% > 0% then L49090  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "(12)Glass Purge        (16)Process Data"
            pfkeys$ = hex(01ffffffffffffffffffff0cffff0f1000)
            return
L49090:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50170,         /* Glass Process Selectio*/ ~
                              L50470,         /* Mach 0=Both,1=GED,2=Bi*/ ~
                              L50660,         /* Planned Production Dat*/ ~
                              L50940,         /* Department Code or All*/ ~
                              L51090,         /* Shift Code or AA = All*/ ~
                              L51220,         /* Glass Completion DTE  */ ~
                              L51400          /* Load Number           */
            return

L50170: REM Process Selection                     SCR_SEL$
            if size$ <> " " then goto L50200
               size$ = "0100"                    /* Default Batch Size */
L50200:     convert size$ to size%, data goto L50420

            convert size% to size$, pic(####)

            if size% > 200% then goto L50420

/* (AWD081) - BEGIN  */
            if ds_batch$ <> " " then goto L50250
               ds_batch$ = "1"
L50250:

            if ds_batch$ <> "0" and ds_batch$ <> "1" then goto L50430

/* (AWD081)  -   END */

            scr_sel% = 0%
            if scr_sel$ <> " " then goto L50280
               scr_sel$ = "1"
L50280:     convert scr_sel$ to scr_sel%, data goto L50380
                                                 /* (EWD005) 12/02/98 */
                                                 /* (AWD074)  */
/*(AWD109) */
            if scr_sel% < 1% or scr_sel% > 9% then goto L50380

            scr_msg$ = str(scr$(scr_sel% + 1%),9%,30%)
            if scr_sel% <> 1% then return
               init(" ") scr_sel$, scr_msg$, ged_bilco$, ged_desc$,      ~
                         size$, scr_dte$, scr_dept$, scr_msg1$,          ~
                         scr_shft_d$, glass_dte$, scr_load$, scr_desc$,  ~
                         ds_batch$
               gosub begin_process
        return
L50380:     errormsg$ = "(Error)-Invalid Process Selection (1,2,3,4,5,6,7,~
            ~8,9)?"
            gosub error_prompt
            init(" ") scr_sel$, size$, scr_msg$, ds_batch$
        return
L50420:     errormsg$ = "(Error) - Invalid Size Specification."
            gosub error_prompt
            init(" ") scr_sel$, size$, scr_msg$, ds_batch$
        return
L50430:     errormsg$ = "(Error) - Invalid DS Batch Specification."
            gosub error_prompt
            init(" ") scr_sel$, size$, scr_msg$, ds_batch$
        return

L50470: REM Machine Flag                          GED_BILCO$
           init(" ") ged_desc$
           if ged_bilco$ <> " " then goto L50510
              ged_bilco$ = "0"                     /* Default to 'GED' */
L50510:    if ged_bilco$ = "0" then ged_desc$ = "Both GED & Bilco  "
           if ged_bilco$ = "1" then ged_desc$ = "GED Glass Machine "
           if ged_bilco$ = "2" then ged_desc$ = "Bilco Glass Machine"
           if len(ged_desc$) < 5 then goto L50610
        REM IF SCR_SEL% = 3% AND GED_BILCO$ = "0" THEN GOTO 50610

           if reschedBatch$ <> " " then goto L50520
              reschedBatch$ = "0"

L50520:    if reschedBatch$ <> "0" and reschedBatch$ <> "1" then        ~
                                                              goto L50640
                                                   /* (EWD005)         */
           if scr_sel% = 2% or scr_sel% = 6% then return
           if scr_sel% = 8% then return     /* (AWD109)*/
           if scr_sel% = 4% then return     /* (IM8022)*/

              init(" ") scr_dte$, scr_dept$, scr_msg1$, scr_shft$,       ~
                        scr_shft_d$, glass_dte$, scr_load$, scr_desc$

/* (AWD0106) take out load entry for option 4*/
              if scr_sel% = 5% then goto L50630    /* (EWD031)         */
              gosub begin_process
        return
L50610:    errormsg$= "(Error)-Invalid Glass Machine Selection (0,1,2)?"
           gosub error_prompt
           init(" ") ged_bilco$, ged_desc$
        return
L50640:    errormsg$= "(Error)-Invalid Re-Sechedule Batch Selection (0,1)?"
           gosub error_prompt
           init(" ") reschedBatch$
        return
L50620: return clear all                           /* (EWD030)         */
        goto inputmode

L50630: return clear all                           /* (EWD031)         */
        goto inputmode_special_shapes
                                                   /* (EWD031)         */
L50660: REM Production Date                       SCR_DTE$, SCR_DTE1$
           date% = 0%
           call "DATEOK" (scr_dte$, date%, errormsg$ )
           if errormsg$ <> " " then return
              scr_dte1$ = scr_dte$
              call "DATUNFMT" (scr_dte1$)
              dt_key1$ = all(hex(00))
              str(dt_key1$,1%,6%) = str(scr_dte1$,1%,6%)
              read #2,key 1% > dt_key1$, using L50760, dt_key1$,        ~
                                                      eod goto L50890
L50760:         FMT POS(47), CH(57)
              if str(scr_dte1$,1%,6%) <> str(dt_key1$,1%,6%) then       ~
                                                              goto L50890
        REM Remake Production Date
           call "DATE" addr("GD",str(scr_dte1$,1%,6%),dd$,date%)
           dd1$ = dd$ : num$ = "7"
           if dd1$ = "MON" then num$ = "1"
           if dd1$ = "TUE" then num$ = "2"
           if dd1$ = "WED" then num$ = "3"
           if dd1$ = "THU" then num$ = "4"
           if dd1$ = "FRI" then num$ = "5"
           if dd1$ = "SAT" then num$ = "6"

        return
L50890:    errormsg$ = "No Data on File for Specified Production Date"
           gosub error_prompt
           init(" ") scr_dte$, scr_dte1$
        return

L50940: REM Department Selection                  SCR_DEPT$
            if scr_dept$ <> " " then goto L50990
               scr_dept$ = "ALL"
               scr_msg1$ = "(A)LL Process all Dept's"
               goto L51020
L50990:     gosub lookup_dept
            if dept% = 0% then goto L51040

L51020: REM FF$   = STR(SCR_DTE1$,3%,4%) & "@" & SCR_DEPT$

            ff$ = str(scr_dte$,1,2) & str(scr_dte$,4,2) & "@" & scr_dept$
        return
L51040:     errormsg$ = "(Error) - Invalid Department Selection?"
            gosub error_prompt
            init(" ") scr_dept$, scr_msg1$
        return

L51090: REM Shift Selection                       SCR_SHFT$
            if scr_shft$ <> " " then goto L51140
               scr_shft$ = "AA"
               scr_shft_d$ = "(AA) Process all Shift's"
               return
L51140:     gosub lookup_shift
            if shft% = 0% then goto L51170
        return
L51170:     errormsg$ = "(Error) - Invalid Shift Selection?"
            gosub error_prompt
            init(" ") scr_shft$, scr_shft_d$
        return

L51220: REM Glass Production Date                 GLASS_DTE$,GLASS_DTE1$
           if scr_sel% = 2% then goto L51260
           if scr_sel% = 6% then goto L51260
           if scr_sel% = 8% then goto L51260 /* (AWD109)*/
           if scr_sel% = 4% then goto L51260 /* (IM8022)*/
                                                  /* (EWD005)          */
              init(" ") scr_dept$, scr_msg1$, scr_load$, glass_dte$
              return
L51260:    date% = 0%
           if glass_dte$ <> " " then goto L51300
              goto L51350

L51300:    call "DATEOK" (glass_dte$, date%, errormsg$ )
           if errormsg$ <> " " then return
              glass_dte1$ = glass_dte$
              call "DATUNFMT" (glass_dte1$)
        return
L51350:    errormsg$ = "(Error) - Invalid Glass Production Date??"
           gosub error_prompt
           init(" ") glass_dte$,glass_dte1$
        return

L51400: REM LOAD NUMBER
           if scr_sel% <> 1% then goto L51450
              init(" ") scr_dept$, scr_msg1$, scr_load$, glass_dte$,     ~
                        scr_desc$, stk_so$
              return
L51450:    if scr_load$ <> " " then goto L51500
L51460:       if scr_sel$ = "6" then goto L51570   /* (EWD005) Req.   */
              scr_load$ = "ALL  "
              scr_desc$ = "(ALL) - Loads"
              return

L51500:    if str(scr_load$,1%,3%) = "ALL" then goto L51460
           ld_key$ = all(hex(00))
           ld_key$ = scr_load$
           read #5,key = ld_key$, using L51540, scr_desc$, eod goto L51560
L51540:       FMT POS(16), CH(30)
        return
L51560:    errormsg$ = "(Error) - Invalid Load Number Entered?"
           gosub error_prompt
           init(" ") scr_load$, scr_desc$, stk_so$
        return                                    /* (EWD005) - Begin */
L51570:    errormsg$ = "(Error) - Valid Load Number is Required?"
           gosub error_prompt
           init(" ") scr_load$, scr_desc$, stk_so$
        return
                                                  /* (EWd005) - End   */
        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51670          /* Manufactured Part     */

            return

L51670: REM Manufactured Part                     DT_PART$
            init(" ") cut$(), ged$(), dt_desc$, model$, dt_dept$
            new_part$ = dt_part$
            if dt_part$ <> " " then goto L51750

               dt_desc$ = " "
               dt_desc$ = hex(06) & "Select a Valid Stock Part"
               call "GETCODE" (#7,dt_part$, dt_desc$, 0%, 1.32, f1%(7))

L51750:     dt_desc$ = "( Non-Stock Part )"
            read #7,key = dt_part$, using L51770,dt_desc$, eod goto L51790
L51770:        FMT POS(26), CH(32)

L51790:     if len(dt_part$) < 19 then goto L51850
               gosub format_screen
               model$ = str(dt_part$,1%,3%)
REM !!IF STR(MODEL$,1%,1%) = "8" THEN DT_DEPT$ = "008"
/* (AWD113) */
               if str(model$,1%,1%) = "8" then gosub check_casement_model
               if str(model$,1%,1%) = "A" then gosub check_casement_model
               if str(model$,1%,1%) = "D" then gosub check_casement_model
/* (\AWD113) */
               if str(model$,1%,1%) = "9" then goto L51850

/* (AWD121) */
               dim1es, dim2es, dim3es = 0.00
               convert dim1es$ to dim1es, data goto bad_dim1
bad_dim1:
               convert dim2es$ to dim2es, data goto bad_dim2
bad_dim2:
               convert dim3es$ to dim3es, data goto bad_dim3
bad_dim3:

               convert dim1es to dim1es$, pic(######0.00)
               convert dim2es to dim2es$, pic(######0.00)
               convert dim3es to dim3es$, pic(######0.00)

        return
L51850:     errormsg$ = "(Error) - Invalid Part Number for Calculation?"
            gosub error_prompt
            init(" ") dt_part$, dt_desc$
        return
/* (AWD113) */
        check_casement_model
          for i% = 1% to mod%(5%)         /* Check Valid 800's */
           if mod$(5%,i%) = model$ then goto casement_model_found
          next i%
        return
        casement_model_found
          dt_dept$ = "008"
        return
/* (\AWD113) */

        format_screen
              cut$(1%) =                                                 ~
                 "Top(1) Wid =            Hght =          Clmr-         "
              cut$(2%) =                                                 ~
                 "   (2) Wid =            Hght =          Clmr-         "
              cut$(3%) =                                                 ~
                 "   (3) Wid =            Hght =          Clmr-         "
              cut$(4%) =                                                 ~
                 "   (4) Wid =            Hght =          Clmr-         "
              cut$(5%) =                                                 ~
                 "   (5) Wid =            Hght =          Clmr-         "
              cut$(6%) =                                                 ~
                 "Bot(1) Wid =            Hght =          Clmr-         "
              cut$(7%) =                                                 ~
                 "   (2) Wid =            Hght =          Clmr-         "
              cut$(8%) =                                                 ~
                 "   (3) Wid =            Hght =          Clmr-         "
              cut$(9%) =                                                 ~
                 "   (4) Wid =            Hght =          Clmr-         "
              cut$(10%) =                                                ~
                 "   (5) Wid =            Hght =          Clmr-         "
                                                /* Width  = Pos(14,9) */
                                                /* Height = Pos(32,8) */
                                                /* Clmr   = Pos(46,9) */
            ged$( 1%) = "OV Thick =XXXXXX    "
            ged$( 2%) = "Sandwich =XXXXXX    "
            ged$( 3%) = "Spacer D.=XXXXXXXXXX"
            ged$( 4%) = "No. Lits =X         "
            ged$( 5%) = "Muttin Cd=XXXXXXXX  "

            ged$( 6%) = "OV Thick =XXXXXX    "
            ged$( 7%) = "Sandwich =XXXXXX    "
            ged$( 8%) = "Spacer D.=XXXXXXXXXX"
            ged$( 9%) = "No. Lits =X         "
            ged$(10%) = "Muttin Cd=XXXXXXXX  "

        return

        lookup_dept
           init(" ") readkey$ : dept% = 0%
           if  scr_dept$ = "066" then goto L52350        /* (EWD018)   */

           str(readkey$,1%,9%)   = "PLAN DEPT"
           str(readkey$,10%,15%) = scr_dept$
           read #3,key = readkey$, using L52330, scr_msg1$,eod goto L52350
L52330:        FMT POS(25), CH(30)
           dept% = 1%
L52350: return
                                                         /* (EWD070)  */
        lookup_scan_window
           init(" ") readkey$, scr_wind_d$, beg_wind$, end_wind$
           scr_wind%, beg_wind%, end_wind% = 0%

           str(readkey$,1%,9%)   = "SHAPESCAN"
           str(readkey$,10%,15%) = scr_wind$
           read #3,key = readkey$, using L52330, scr_wind_d$,             ~
                                                           eod goto L52360
              beg_wind$ = str(scr_wind_d$,1%,4%)
              end_wind$ = str(scr_wind_d$,6,4%)

              convert beg_wind$ to beg_wind%, data goto L52360

              convert end_wind$ to end_wind%, data goto L52360

              scr_msg1$ = "Scan Window =" & str(scr_wind_d$,11%,17%)

           scr_wind% = 1%
L52360: return

        check_scan_window
           init(" ") scan_time$, scan_date$
           scan_wind% = 0%
           scan_time% = 0%

           scan_date$ = str(rm_rec$(),7%,6%)
                                                      /* Out of Range  */
           if scan_date$ > str(ed_dte$,1%,6%) then return

           scan_time$ = str(rm_rec$(),14%,2%) & str(rm_rec$(),17%,2%)

           convert scan_time$ to scan_time%, data goto L52365
L52365:
           if str(rm_rec$(),20%,2%) = "PM" then                         ~
                                          scan_time% = scan_time% + 1200%
                                                      /* Special Test  */
                                                      /* for Scan Wind */
                                                      /* (4)           */

                                                      /* Adjustment for*/
                                                      /* after Midnight*/
                                                      /* before 01:00AM*/
           if str(rm_rec$(),20%,2%) = "AM" and scan_time% > 1159% then   ~
                                          scan_time% = scan_time% - 1200%

          if scr_wind$ = "4" then goto L52380

           if scan_time% < beg_wind% then goto L52370 /* Not in Window */

           if scan_time% > end_wind% then goto L52370 /* Not in Window */

              scan_wind% = 1%                         /* In window     */
L52370:

        return
L52380:

          if scan_date$ <> str(bg_dte$,1%,6%) then goto L52385
                                                     /* Check begin    */
                                                     /* Time Window    */
             if scan_time% < beg_wind% then return   /* Not in Window  */

             goto L52390                             /* Within Window  */
L52385:
                                                     /* Check End      */
                                                     /* Time Window    */
          if scan_date$ <> str(ed_dte$,1%,6%) then return
             if scan_time% > end_wind% then return   /* not in Window  */


L52390:                                              /* Within Window */
              scan_wind% = 1%
        return
                                                         /* (EWD070)  */
        lookup_shift
           init(" ") readkey$ : shft% = 0%
           str(readkey$,1%,9%)   = "PLAN SHFT"
           str(readkey$,10%,15%) = scr_shft$
          read #3,key = readkey$, using L52420, scr_shft_d$,eod goto L52440
L52420:        FMT POS(25), CH(30)
           shft% = 1%
L52440: return

        check_bilco                              /* (EWD025) - 09/29/00*/
            g_b$ = "2"                           /* 0 = GED Only       */
                                                 /* 1 = Bilco only     */
                                                 /* 2 = Both GED/Bilco */
            for i% = 1% to mod%(9%)              /* (GLASS09) Exclude  */
                                                 /* Departments-Exception*/
                if dt_dept$ = mod$(9%,i%) then goto L52540
            next i%
        return
L52540:     g_b$ = "0"                           /* GED  Only          */
        return                                   /* (EWD025)           */

        deffn'153(fieldnr%)
            errormsg$ = " "
                                              /* (EWD070)              */
            on fieldnr% gosub L53060,         /* Begin/End Prod Date   */ ~
                              L53490,         /* Report Selection      */ ~
                              L53700          /* Dept Sel and Time Wind*/
                                              /* (EWD070)              */
            return

L53060: REM Production Date                       BG_DATE$, BG_DTE$
           if bg_date$ <> " " then goto L53100
              bg_date$ = date$

L53100:    date% = 0%
           call "DATEOKC" (bg_date$, date%, errormsg$ )
           if errormsg$ <> " " then return
              bg_dte$ = bg_date$
              call "DATUFMTC" (bg_dte$)
        REM Production Date                       ED_DATE$, ED_DTE$
           date% = 0%
           if ed_date$ <> " " then goto L53270
              ed_date$ = bg_date$

L53270:    call "DATEOKC" (ed_date$, date%, errormsg$ )
           if errormsg$ <> " " then return
              ed_dte$ = ed_date$
              call "DATUFMTC" (ed_dte$)
              if ed_dte$ < bg_dte$ then goto L53440
        return
           errormsg$ = "(Error) No Data for Specified Production Date?"
           gosub error_prompt
           init(" ") bg_date$, bg_dte$, ed_date$, ed_dte$
        return
L53440:    errormsg$ = "(Error) Invalid Ending Production Date?      "
           gosub error_prompt
           init(" ") bg_date$, bg_dte$, ed_date$, ed_dte$
        return

L53490: REM Report Selection                        RP_SEL$, RP_SEL_D$
            init(" ") rp_sel_d$
            if rp_sel$ <> " " then goto L53540
               rp_sel$ = "1"

L53540:     if rp_sel$ = "1" then rp_sel_d$ = "Scanned Glass Re-Make's  "
            if rp_sel$ = "2" then rp_sel_d$ = "Scheduled Glass Re-Make's"
            if rp_sel$ = "3" then rp_sel_d$ = "Scheduled Glass          "
            if rp_sel$ = "4" then rp_sel_d$ = "Completed Glass          "
            if rp_sel$ = "5" then rp_sel_d$ = "Completed Glass Re-Make's"
            if len(rp_sel_d$) < 5 then goto L53610
        return
L53610:    errormsg$ = "(Error) Invalid Report Selection (1 Thru 5)? "
           gosub error_prompt
           init(" ") rp_sel$, rp_sel_d$
        return
                                              /* (EWD070)              */

L53700: REM Department Selection                  SCR_DEPT$
            if scr_dept$ <> " " then goto L53710
               scr_dept$ = "ALL"
               scr_msg1$ = "(A)LL Process all Dept's"
               scr_wind$ = "0"
               goto L53720
L53710:     gosub lookup_dept
            if scr_wind$ = " "then scr_wind$ = "0"

            if dept% = 0% then goto L53740
/* NTX Shapes */
REM IF SCR_DEPT$ <> "043" THEN GOTO L53720
               if scr_dept$ <> "043" and schema% = 1% then goto L53720
               if (scr_dept$ <> "002" and scr_dept$ <> "003") and ~
                     schema% = 2% then goto L53720

               gosub lookup_scan_window
               if scr_wind% = 0% then goto L53750

               if rp_sel$ = "1" then                              ~
                               rp_sel_d$ = "Scanned Glass Re-Make's       "
               if rp_sel$ = "2" then                              ~
                               rp_sel_d$ = "Custom Glass Sched Re-Make's  "
               if rp_sel$ = "3" then                              ~
                               rp_sel_d$ = "Custom Glass Scheduled        "
               if rp_sel$ = "4" then                              ~
                               rp_sel_d$ = "Custom Glass Received         "
               if rp_sel$ = "5" then                              ~
                               rp_sel_d$ = "Completed Glass Re-Make's     "
L53720:
        return
L53740:     errormsg$ = "(Error) - Invalid Department Selection?"
            gosub error_prompt
            init(" ") scr_dept$, scr_msg1$, scr_wind$, scr_wind_d$
        return
L53750:     errormsg$ = "(Error) - Invalid Special Shapes Scanning Window?"
            gosub error_prompt
            init(" ") scr_dept$, scr_msg1$, scr_wind$, scr_wind_d$
        return

                                              /* (EWD070)              */

        deffn'155(fieldnr%)
            errormsg$ = " "
              on fieldnr% gosub L54550,         /* Quantity         */   ~
                                L54600,         /* Model Code       */   ~
                                L54647,         /* Glass Code       */   ~
                                L54650,         /* Shapes Code      */   ~
                                L54700,         /* Base Entry       */   ~
                                L54750,         /* Left Entry       */   ~
                                L54800,         /* Right Entry      */   ~
                                L54850,         /* Top Entry        */   ~
                                L54900,         /* S1 Entry         */   ~
                                L54950,         /* S2 Entry         */   ~
                                L54990,         /* Nest Entry       */   ~
                                L54995          /* Label Text       */

            return
L54550: Rem Cut Quantity                        sh_qty$
           convert sh_qty$ to sh_qty%, data goto L54560

           convert sh_qty% to sh_qty$, pic(####)

           if sh_qty% = 0% then goto L54560

        return
L54560: errormsg$ = "(Error) - Invalid Quantity?"
        gosub error_prompt
        init(" ") sh_qty$
        return

L54600: Rem Model Code                          sh_model$
                                         /* Check Valid Special */
                                         /* Shapes Models       */
           gosub lookup_shape
           if check% = 0% then goto L54645

                                         /* Get Model Description */
           init(" ") readkey$, desc$, sh_model_d$
           str(readkey$, 1%,9%)  = "MODEL    "
           str(readkey$,10%,15%) = sh_model$
           read #3,key = readkey$, using L52330, desc$,eod goto L54645

           sh_model_d$ = desc$
        return
L54645:    errormsg$ =  "Error - Invalid Special Shape Model?"
           gosub error_prompt
           init(" ") sh_model$, sh_model_d$
        return

L54647: Rem Special Shape Glass Code                    sh_glass$
           init(" ") readkey$, desc$, sh_glass_d$
                                         /* S.S Glass Sandwich */
           sh_glass% = 0%
           convert sh_glass$ to sh_glass%, data goto L54648

           convert sh_glass% to sh_glass$, pic(00)
L54648:
           gosub lookup_shape_glass
           if check% = 0% then goto L54649

        return
L54649:    errormsg$ =  "Error - Invalid Special Shape Glass Code?"
           gosub error_prompt
           init(" ") sh_glass$, sh_glass_d$
        return

L54650: Rem Shapes Code                         sh_config$
                                         /* 1st Get Shape Description */
           gosub load_shape_desc
           if check% = 0% then goto L54695
                                         /* 2nd Get Field Prompts     */
           gosub load_shape_prompts
           if check% = 0% then goto L54698
                                         /* 3rd Verify Shape get Desc*/
           gosub verify_model_shape
           if check% = 0% then goto L54695

        return
L54695:    errormsg$ = "(Error) - Invalid Special Shapes Config Code?"
           gosub error_prompt
           init(" ") sh_config$, sh_config_d$, sh_config_seq$
           sh_config_seq$ = "01"
        return
L54698:    errormsg$ = "(Error) - Invalid Config Data Text?"
           init(" ") sh_config$, sh_config_d$, tt$(), ttt$()
        return
                                            /* sh$(?) = Input Value      */
                                            /* shc$(?)= Calculated Value */
L54700: Rem Base Entry                      sh$(1%), shc$(1%)
        sh% = 1%
        gosub convert_decimal

        if str(sh_flags$(sh%),1%,1%) = "Y" and sh(sh%) < 1.0 then        ~
                                                              goto L54710
        return
L54710:    errormsg$ = "(Error) - Invalid " & tt$(sh%) & " Size?"
           gosub error_prompt
           init(" ") sh$(sh%), shc$(sh%)
        return


L54750: Rem Left Entry                          sh$(2%), shc$(2%)
        sh% = 2%
        gosub convert_decimal

        if str(sh_flags$(sh%),1%,1%) = "Y" and sh(sh%) < 1.0 then        ~
                                                              goto L54710
        return

L54800: Rem Right Entry                         sh$(3%), shc$(3%)

        sh% = 3%
        gosub convert_decimal

        if str(sh_flags$(sh%),1%,1%) = "Y" and sh(sh%) < 1.0 then        ~
                                                               goto L54710
        return

L54850: Rem Top Entry                           sh$(4%), shc$(4%)
        sh% = 4%
        gosub convert_decimal

        if str(sh_flags$(sh%),1%,1%) = "Y" and sh(sh%) < 1.0 then        ~
                                                               goto L54710
        return

L54900: Rem S1 Entry                            sh$(5%), shc$(5%)
        sh% = 5%
        gosub convert_decimal

        if str(sh_flags$(sh%),1%,1%) = "Y" and sh(sh%) < 1.0 then        ~
                                                               goto L54710
        return

L54950: Rem S2 Entry                            sh$(6%), shc$(6%)
        sh% = 6%
        gosub convert_decimal

        if str(sh_flags$(sh%),1%,1%) = "Y" and sh(sh%) < 1.0 then        ~
                                                               goto L54710
        return

L54990: Rem Nest Entry                          sh$(7%), shc$(7%)
        sh% = 7%
                                                /* (RHH-SPECIAL)      */
                                                /* (EWD067)           */

        REM   sh$(sh%)  = "N        "
        REM   shc$(sh%) = "(N)o     "

        if str(sh$(sh%),1%,1%) = "Y"  then shc$(sh%) = "(Y)es   "
        if str(sh$(sh%),1%,1%) = "N"  then shc$(sh%) = "(N)o    "
        if str(sh$(sh%),1%,1%) = "T"  then shc$(sh%) = "(T)rp   "

        str(sh_entry$,7%,1%) = "N"

        if str(sh$(sh%),1%,1%) = "Y" then str(sh_entry$,7%,1%) = "Y"
        if str(sh$(sh%),1%,1%) = "T" then str(sh_entry$,7%,1%) = "T"
                                               /* (RHH-SPECIAL)       */
                                               /* (EWD067)            */

        process% = 2%                    /* From Data Entry            */
        gosub calculate_special_shape
                                         /* (EWD056)                   */
        return

L54995: Rem Label Text                          sh_txt$
        return

        convert_shape
                                        /* Converting the dimensions  */
                                        /* from the Part Number.      */
                                        /* Created by window Wizard   */
            a1, a2, shd(1%), shd(2%), shd(3%), shd(4%) = 0.0
            if str(sh_code$,2%,1%) = "?" then goto CS2
            convert str(dt_part$,13%,3%) to a1, data goto CS1
CS1:
            convert str(dt_part$,16%,1%) to a2, data goto CS2
CS2:
            shd(1%) = a1 + (a2/8.0)           /* Decimal Width    */

            a1 = 0.0 : a2 = 0.0
            if str(sh_code$,4%,1%) = "?" then goto CS4
            convert str(dt_part$,17%,2%) to a1, data goto CS3
CS3:
            convert str(dt_part$,19%,1%) to a2, data goto CS4
CS4:
            shd(2%) = a1 + (a2/8.0)           /* Decimal Height   */


            a1 = 0.0 : a2 = 0.0
            if str(sh_code$,6%,1%) = "?" then goto CS6
REM            convert str(dt_part$,20%,2%) to a1, data goto CS5
REM CS5
REM            convert str(dt_part$,22%,1%) to a2, data goto CS6
CS6:
REM            shd(3%) = a1 + (a2/8.0)        /* Decimal Leg Height*/
               shd(3%) = dim1es               /* (AWD121) */

                                              /* Fourth dimention is */
                                              /* in the 1st three digits*/
                                              /* of the Glass Text   */
            a1 = 0.0 : a2 = 0.0
            if str(sh_code$,8%,1%) = "?" then CS8
REM            convert str(txt$,1%,2%) to a1, data goto CS7
REM CS7
REM            convert str(txt$,3%,1%) to a2, data goto CS8
CS8:
REM            shd(4%) = a1 + (a2/8.0)        /* Decimal Leg2 Height*/
               shd(4%) = dim2es

        return

        lookup_cross                              /* (EWD056)            */
           init(" ") readkey$, desc$, sh_config$, sh_config_seq$, sh_codes$
           check% = 0%
           operableshape% = 0%
                                                 /* For Elipticles      */
           if str(shape_cross$,1%,1%) = "C" then                         ~
                                             str(shape_cross$,2%,1%) = "0"
                                                 /* For Cirlces         */
           if str(shape_cross$,1%,1%) = "E" then                         ~
                                             str(shape_cross$,2%,1%) = "0"
                                                 /* For Octagons        */
           if str(shape_cross$,1%,1%) = "I" then                         ~
                                             str(shape_cross$,2%,1%) = "0"

           gosub lookup_specials                 /* Check for Octagon   */
                                                 /* and Cirles Until WW */
                                                 /* Fixed (Left Out)    */

           str(readkey$, 1%,9%)  = "SHAPCROSS"
           str(readkey$,10%,15%) = shape_cross$
           read #3,key = readkey$, using L52330, desc$,               ~
                                       eod goto lookup_cross_done

           sh_config$     = str(desc$,1%,2%)     /* Shape Code           */
           sh_config_seq$ = str(desc$,3%,2%)     /* Sequence code        */
                                                 /* Unpack Codes         */
           str(sh_codes$,1%,1%) = str(desc$,6%,1%)
           str(sh_codes$,2%,1%) = str(desc$,8%,1%)
           str(sh_codes$,3%,1%) = str(desc$,10%,1%)
           str(sh_codes$,4%,1%) = str(desc$,12%,1%)

           check% = 1%
        lookup_cross_done
           init(" ") rhh$
           convert check% to rhh$, pic(##)
        return                                  /* (EWD056)            */

        lookup_specials                         /* Octagon/Circles     */
           init(" ") readkey$, desc$
           str(readkey$,1%,9%) = "SHAPEXTRA"
           str(readkey$,10%,15%) = sh_model$
           read #3,key = readkey$, using L52330, desc$,               ~
                                    eod goto lookup_specials_done
                shape_cross$ = str(desc$,1%,2%)
                if str(shape_cross$,1%,1%) = "S" or     /* (CR2303) */ ~
                   str(shape_cross$,1%,1%) = "R" then operableshape% = 1%

        lookup_specials_done

        return


        lookup_shape                            /* (EWD056)            */
           init(" ") readkey$
           check% = 0%
           str(readkey$,1%,9%)   = "PLAN SHAP"
           str(readkey$,10%,15%) = sh_model$
           read #3,key = readkey$, eod goto lookup_shape_done
              check% = 1%
        lookup_shape_done

        return                                  /* (EWD056)            */

        lookup_shape_glass                      /* (EWD056)            */
           init(" ") readkey$, desc$, sh_glass_d$
           check% = 0%
           str(readkey$, 1%,9%)  = "PLN GLASS"
           str(readkey$,10%,15%) = sh_glass$
           read #3,key = readkey$, using L52330, desc$,               ~
                                       eod goto lookup_shape_glass_done

           sh_glass_d$ = desc$
REM SH_SANDWICH$= STR(DESC$,1%,12%)
           sh_sandwich$= str(desc$,1%,20%)
           check% = 1%
           return
        lookup_shape_glass_done
           sh_sandwich$ = "Error - "& sh_glass$ /* Not Defined         */
           sh_glass_d$  = sh_sandwich$
           check% = 1%
        return                                  /* (EWD056)            */

        load_shape_desc                         /* (EWD056)            */
           init(" ") readkey$, desc$
           check% = 0%
           str(readkey$,1%,9%)   = "PLNCONFIG"  /* Shape Description */
           str(readkey$,10%,15%) = sh_config$
           read #3,key = readkey$, using L52330, desc$,               ~
                                         eod goto load_shape_desc_done
           sh_config_d$ = desc$
           check% = 1%
        load_shape_desc_done

        return                                  /* (EWD056)          */

        check_shape_glass_sort                  /* (EWD056)           */
           init(" ") readkey$, desc$
           str(readkey$,1%,9%)   = "PLN SSORT"  /* Shape Sorting     */
           str(readkey$,10%,15%) = "1" & str(dt_part$,5%,2%)
           read #3,key = readkey$, using L52330, desc$,               ~
                                         eod goto check_shape_glass_done
           sh_sort$ = str(desc$,1%,2%)
        check_shape_glass_done

        return                                  /* (EWD056)          */

        check_shape_grid_sort                   /* (EWD056)          */
           init(" ") readkey$, desc$
           str(readkey$,1%,9%)   = "PLN SSORT"  /* Shape Sorting     */
                                                /* Check Color 1st   */
           str(readkey$,10%,15%) = "2" & str(dt_part$,4%,1%)
           read #3,key = readkey$, using L52330, desc$,               ~
                                         eod goto check_shape_grid_done
           sh_sort$ = str(desc$,1%,2%)
        goto check_shape_grid_done1
                                                /* Check Screen 2nd  */
        check_shape_grid_done
           init(" ") readkey$, desc$
           str(readkey$,1%,9%) = "PLN SSORT"
           str(readkey$,10%,15%) ="3" & str(dt_part$,12%,1%)
           read #3,key = readkey$, using L52330, desc$,               ~
                                         eod goto check_shape_grid_done1
               sh_sort$ = str(desc$,1%,2%)
        check_shape_grid_done1

        return                                  /* (EWD056)          */

        check_shape_temp                        /* (EWD056)           */
           init(" ") readkey$, desc$
           str(readkey$,1%,9%)   = "PLAN TEMP"  /* Check Tempered     */
           str(readkey$,10%,15%) = str(dt_part$,5%,2%)
           read #3,key = readkey$, using L52330, desc$,               ~
                                         eod goto check_shape_temp_done
           sh_sort$ = "10"                      /* Put Temp at the end */
        check_shape_temp_done

        return                                  /* (EWD056)          */

        load_shape_prompts       /* (EWD056)                         */
           init(" ") sh_bridge$, sh_entry$
           check% = 0%
                                 /* 2nd Load Field Prompts           */
                                 /* Text with input criteria         */
                                 /* Flags 1%,1% = Input Y or N       */
                                 /*       2%,1% = What is Entered    */
                                 /*       3%,1% = What is Calculated */
           for i% = 1% to 7%     /* Seven Possible Field Prompts     */
               init(" ") readkey$, desc$
               str(readkey$,1%,9%) = "PLNCNFIG "
               str(readkey$,10%,15%) = sh_config$
               convert i% to str(readkey$,12%,1%), pic(0)

               read #3,key = readkey$, using L52330,desc$,             ~
                                        eod goto load_shape_prompts_done

               tt$(i%) = str(desc$,1%,25%) /* Field Prompt Description*/
               sh_flags$(i%) = str(desc$,28%,3%)
                                           /* Data input Flags 1, 2,3 */
                                           /* (1) Input Data Y or N   */
                                           /* (2) Input Data Type     */
                                           /* (3) Calc Dat Out        */
                                           /* Set Calculate Field     */
             if str(sh_flags$(i%),3%,1%) = "W" then                     ~
                ttt$(i%) = "Width   "
             if str(sh_flags$(i%),3%,1%) = "H" then                     ~
                ttt$(i%) = "Height  "
             if str(sh_flags$(i%),3%,1%) = "R" then                     ~
                ttt$(i%) = "Radius  "
             if str(sh_flags$(i%),3%,1%) = "N" then                     ~
                ttt$(i%) = " (N/A)  "
             if str(sh_flags$(i%),3%,1%) = "L" then                     ~
                ttt$(i%) = "Leg Hght"
             if str(sh_flags$(i%),3%,1%) = "S" then                     ~
                ttt$(i%) = "LSideLeg"
             if str(sh_flags$(i%),3%,1%) = "T" then                     ~
                ttt$(i%) = "Top  Leg"
             if str(sh_flags$(i%),3%,1%) = "X" then                     ~
                ttt$(i%) = "SlegHght"
             if str(sh_flags$(i%),3%,1%) = "Z" then                     ~
                ttt$(i%) = "RSideLeg"

                                  /* Save Calc Fields for Bridge File */
             str(sh_bridge$,i%,1%) = str(sh_flags$(i%),3%,1%)
                                  /* Save Entry Fields for Calc       */
             str(sh_entry$,i%,1%)  = str(sh_flags$(i%),2%,1%)

           next i%
             check% = 1%
             str(sh_entry$,7%,1%) = "N"          /* (RHH-SPECIAL)      */
                                                 /* (EWD067)           */

        load_shape_prompts_done

        return                   /* (EWD056)                           */

        verify_model_shape                       /* (EWD056)            */
           init(" ") readkey$, desc$
           check% = 0%
           str(readkey$,1%,9%)   = "PLNCNFIGM"   /* Configuration table */
           str(readkey$,10%,3%)  = sh_model$     /* Special Shape Model */
           str(readkey$,13%,2%)  = sh_config$    /* Shape Code          */
           str(readkey$,15%,2%)  = sh_config_seq$/* Shape seq. No. When */
                                                 /* Same Shape two Names*/
           read #3,key = readkey$, using L52330, desc$,                  ~
                                   eod goto verify_model_shape_done

           sh_config_d$ = desc$                  /* Get Shape Descript  */
                                                 /* Convert to Integer  */
           convert sh_config$ to sh_config%, data goto                   ~
                                                   verify_model_shape_done


           check% = 1%
        verify_model_shape_done

        return                                   /* (EWD056)            */
                                                 /* (AWD077)            */
        calculate_special_shape
        if calsl% = 1% then goto calc_calsl
        if scr_sel% <> 5% then goto SP_1
           if sh_config% = 64% and sh_config_seq$ = "02"             ~
                                               then shape_cross$ = "C0"


sp_1:

REM init(" ") cmgwidth$, cmgheight$, cmgdim1es$
REM call "SHOSTAT" ("HERE AT EWDCALSS ->" & dt_seq$ & " " & dt_part$)
REM stop

REM convert width to cmgwidth$, pic(###0.0000)
REM convert height to cmgheight$, pic(###0.0000)
REM convert dim1es to cmgdim1es$, pic(###0.0000)

REM call "SHOSTAT" ("W-H-D ->" & cmgwidth$ & "-" & cmgheight$ & "-" & cmgdim1es$)
REM stop

        call "EWDCALSS"   (sh_config%,   /* Shape Code                 */~
                          sh_model$,     /* Model Code                 */~
                          sh(),          /* Data Entry Values          */~
                          shc(),         /* Calculated Values          */~
                          sh_fields$,    /* Label Field Flags          */~
                          sh_position$,  /* Label Value Position       */~
                          sh_entry$,     /* Data Entry Field Name      */~
                          dt_bar$,       /* For Debug                  */~
                          shape_cross$,  /* 'SHAPCROSS' Code           */~
/*(CR503)*/               ged_shape$,    /* GED Shape Code             */~
/*(CR503)*/               ged_fields$,   /* GED Shape Field Entries    */~
/*(CR503)*/               ged_shc(),     /* GED Shape Calculated Value */~
/*(CR2109)*/              width,         /* Bottom Sash Width          */~
/*(CR2109)*/              height,        /* Bottom Sash Height         */~
/*(CR2109)*/              operableshape%,/* Operable Shape Flag        */~
                          #3,            /* GENCODES File              */~
                          err% )         /* Error Code 0 = Ok, 0 <> err*/

           for k% = 1% to 7%
               gosub convert_fraction

           next k%

REM SH_HUB$ = "          "

           if err% <> 0% then goto print_error

calc_calsl:
REM   CALL "SHOSTAT" ("EWDCALSL")   STOP
           if operableshape% = 1% then return       /* (CR2303) */
           str(sh_face$,1%,2%) = sh_config$
           str(sh_face$,3%,2%) = sh_glass$
                                                     /* (EWD059)        */
                                                     /* Sub 'EWDCALSL'  */
           call "EWDCALSL"   (scr_sel%,      /* Screen Selection        */~
                              sh_model$,     /* Model Code              */~
                              sh_config%,    /* Shape Code              */~
                              dt_rec$,       /* APCPLNDT Record         */~
                              sh(),          /* Data Entry Values       */~
                              sh_entry$,     /* Name of Data Field Ente */~
                              shape_cross$,  /* 'SHAPCROSS' Code        */~
                              specialmull$,  /* Casing/Bullnose (AWD097 */~
                              sub_part$,     /* Subpart Number  (AWD119)*/~
                              series$,       /* Series                  */~
                              #3,            /* GENCODES File           */~
                              #24,           /* AWDSPECB -  Bending Data*/~
                              err% )         /* Error Code0=Ok, 0<> err */
                                                     /* (EWD059)        */
           if err% = 0% then return
print_error:  if err% = 1% then                                          ~
                 errormsg$ = "Error Calulating the Width for (" &        ~
                  sh_config$ & ") " & dt_bar$

              if err% = 2% then                                          ~
                 errormsg$ = "Error Calculating the Height for (" &      ~
                  sh_config$ & ") " & dt_bar$

              if err% = 3% then                                          ~
                 errormsg$ = "Error Calculating the Radius for (" &      ~
                  sh_config$ & ") " & dt_bar$

              if err% = 4% then                                          ~
                 errormsg$ = "Error Calculating the Leg for (" &         ~
                  sh_config$ & ") " & dt_bar$

              if err% = 5% then                                          ~
                 errormsg$ = "Error No Data in (SHPHFOFF ) for Product - "~
                  & sh_model$

              if err% = 6% then                                          ~
                 errormsg$ = "Error No Equation for Shape (" &           ~
                  sh_config$ & ") "

              if err% = 7% then                                         ~
                 errormsg$ = "Error Updating 'AWDSPECB' for Shape ("    ~
                  & sh_config$ & ") "

              if err% = 8% then                                         ~
                 errormsg$ = "Error Calculating the Diagonal for ("     ~
                   & sh_config$ & ") " & dt_bar$

              gosub error_prompt
        return

        convert_decimal
            sh(sh%) = 0.0
            if str(sh_flags$(sh%),1%,1%) = "N" then goto convert_decimal_1

            convert sh$(sh%) to sh(sh%), data goto convert_decimal_1
        convert_decimal_1:

            sh(sh%) = round(sh(sh%), 5)
            convert sh(sh%) to sh$(sh%), pic(###.####-)

            if sh(sh%) < 1.0 then sh$(sh%) = "         "
        return

        convert_fraction

           wd1$ = "         "
           if shc(k%) < 1.0 then goto convert_fraction_done

           calc = shc(k%)                      /* Convert size   (3) */
           gosub convert_sixteen
           convert a% to str(wd1$,1%,3%), pic(###)

           if b% = 0% then goto convert_fraction_done
                                                 /* Check For Fraction */
              str(wd1$,5%,5%) = str(sz$,(b%*5%) - 4%, 5%)
        convert_fraction_done
           shc$(k%) = wd1$
        return

        initialize_shapes
            init(" ") tt$(), sh_model$, sh_model_d$, sh_config$,          ~
               sh_config_d$, sh_cnfig$, sh_cnfig_d$, sh_flags$(), sh$(),  ~
               shc$(), ttt$(), sh_glass$, sh_glass_d$, sh_config_seq$,    ~
               sh_sandwich$, sh_txt$, sh_face$, sh_fields$, sh_bridge$,   ~
               sh_position$, sh_entry$, shape_cross$, sh_codes$,          ~
               ged_shape$, ged_fields$ /* (CR0503) */
            sh_hub$  = "    "  /* AWD091 */
            sh_hub1$ = "    "
            sh_hub2$ = "    "
            sh_hub3$ = "    "

            mat sh  = zer
            mat shc = zer
            mat ged_shc = zer                /* (CR503) */
            sh_qty% = 0%
            sh_qty$ = "    "
            sh_config_seq$ = "01"
        return

        save_shape_data
          if rec% = 299% then goto save_edited_shape
            sh_error% = 0%
            sh_cnt%   = sh_cnt% + 1%
            sh_rec%   = 0%
            sh_rec%   = sh_cnt%
            convert sh_cnt% to sh_cnt$, pic(0000)

            goto save_new_shape
/*  (EWD033)  Change all sh_cnt% when loading array to sh_rec%, so would */
/*  not save over last saved shape and would not mess up counter!!       */
        save_edited_shape
           convert sh_cnt$ to sh_rec%, data goto L55030 /*ShouldNotHappen*/
L55030:

        save_new_shape
            if process% = 1% then sh_rec% = 1%

            bil$(sh_rec%,1%) = sh_model$             /* Model Code       */
            convert sh_config% to str(bil$(sh_rec%,2%),1%,2%), pic(00)
                                                     /* Configuration Cod*/
            bil$(sh_rec%,3%) = sh_config_seq$        /* Config Seq. Code */
            bil$(sh_rec%,4%) = sh_cnt$               /* Sequence Number  */
            bil$(sh_rec%,5%) = sh_qty$               /* Shape Quantity   */
            bil$(sh_rec%,6%) = sh$(1%)               /* Entered - Base   */
            bil$(sh_rec%,7%) = shc$(1%)              /* Calc-Base Fractio*/
            bil$(sh_rec%,8%) = sh$(2%)               /* Entered - Left   */
            bil$(sh_rec%,9%) = shc$(2%)              /* Calc-Left Fractio*/
            bil$(sh_rec%,10%)= sh$(3%)               /* Entered - right  */
            bil$(sh_rec%,11%)= shc$(3%)              /* Calc-right Fracti*/
            bil$(sh_rec%,12%)= sh$(4%)               /* Entered - Top    */
            bil$(sh_rec%,13%)= shc$(4%)              /* Calc-Top Fraction*/
            bil$(sh_rec%,14%)= sh$(5%)               /* Entered - S1     */
            bil$(sh_rec%,15%)= shc$(5%)              /* Calc-S1 Fraction */
            bil$(sh_rec%,16%)= sh$(6%)               /* Entered - S2     */
            bil$(sh_rec%,17%)= shc$(6%)              /* Calc-S2 Fraction */
            bil$(sh_rec%,18%)= sh$(7%)               /* Entered - Edgewor*/
            bil$(sh_rec%,19%)= shc$(7%)              /* Calc-Edgework Y-N*/
            bil$(sh_rec%,20%)= sh_glass$             /* Shape Glass Code */
            bil$(sh_rec%,21%)= sh_sandwich$          /* Shape Sandwich   */
            bil$(sh_rec%,22%)= sh_config_d$          /* Shape Description*/
            bil$(sh_rec%,23%)= sh_txt$               /* Label Text       */
            bil$(sh_rec%,24%)= sh_hub$               /* Window Hub Adjust*/
            bil$(sh_rec%,25%)= sh_face$              /* Glass Facing Code*/
            bil$(sh_rec%,26%)= sh_fields$            /* Print fields     */
            bil$(sh_rec%,27%)= sh_bridge$            /* Bridge File Field*/
            bil$(sh_rec%,28%)= sh_position$          /* Value Position   */
            bil$(sh_rec%,29%)= sh_entry$             /* Data Entry Fields*/
            bil$(sh_rec%,30%)= "         "           /*                  */
                                                     /* (CR503) +        */

REM CALL "SHOSTAT" ("BUILD SHAPE GED KEY " ) STOP
            gosub build_ged_key
            bil$(sh_rec%,31%)= ged_shape$            /* GED Shape Code   */
            bil$(sh_rec%,32%)= ged_fields$           /* GED Fields       */

                                                     /* Shape Dim Width  */
            convert ged_shc(01%) to bil$(sh_rec%,33%), pic(###0.000)
                                                     /* Shape Dim Height */
            convert ged_shc(02%) to bil$(sh_rec%,34%), pic(###0.000)
                                                     /* Shape Dim A      */
            convert ged_shc(03%) to bil$(sh_rec%,35%), pic(###0.000)
                                                     /* Shape Dim B      */
            convert ged_shc(04%) to bil$(sh_rec%,36%), pic(###0.000)
                                                     /* Shape Dim C      */
            convert ged_shc(05%) to bil$(sh_rec%,37%), pic(###0.000)
                                                     /* Shape Dim D      */
            convert ged_shc(06%) to bil$(sh_rec%,38%), pic(###0.000)
                                                     /* Shape Dim E      */
            convert ged_shc(07%) to bil$(sh_rec%,39%), pic(###0.000)
                                                     /* Shape Dim F      */
            convert ged_shc(08%) to bil$(sh_rec%,40%), pic(###0.000)

            bil$(sh_rec%,41%)= t_k$                  /* Thickness        */
            bil$(sh_rec%,42%)= sandwich$             /* Glass Sandwich   */
            bil$(sh_rec%,43%)= space_d$              /* Spacer Desc      */
            bil$(sh_rec%,44%)= gs$                   /* Gas              */
            bil$(sh_rec%,45%)= interoffset$          /* Offset / Undercut*/
            bil$(sh_rec%,46%)= "         "           /*                  */
            bil$(sh_rec%,47%)= "         "           /*                  */
            bil$(sh_rec%,48%)= "         "           /*                  */
            bil$(sh_rec%,49%)= "         "           /*                  */
            bil$(sh_rec%,50%)= "         "           /*                  */
                                                     /* (CR503) -        */


            gosub initialize_shapes

            if process% = 1% then return

               sh_max% = sh_cnt%
               if sh_max% > 299% then sh_error% = 1% /* Max Limit        */
               if sh_max% > 299% then sh_max% = 299% /* Set to Limit     */
        return clear all
        goto inputmode_special_shapes

        load_shapes_screen                   /* (EWD033)  - Begin  */
            convert rec% to rec$, pic(0000)

            sh_model$   = bil$(rec%,1%)      /* Model Code         */
            sh_config$  = str(bil$(rec%,2%),1%,2%) /* Configuration Code */
            sh_config_seq$ = bil$(rec%,3%)   /* Config Seq. Code   */
            sh_cnt$     = bil$(rec%,4%)      /* Sequence Number    */
            sh_qty$     = bil$(rec%,5%)      /* Shape Quantity     */
            sh$(1%)     = bil$(rec%,6%)      /* Entered - Base     */
            shc$(1%)    = bil$(rec%,7%)      /* Calc-Base Fraction */
            sh$(2%)     = bil$(rec%,8%)      /* Entered - Left     */
            shc$(2%)    = bil$(rec%,9%)      /* Calc-Left Fraction */
            sh$(3%)     = bil$(rec%,10%)     /* Entered - right    */
            shc$(3%)    = bil$(rec%,11%)     /* Calc-right Fraction*/
            sh$(4%)     = bil$(rec%,12%)     /* Entered - Top      */
            shc$(4%)    = bil$(rec%,13%)     /* Calc-Top Fraction  */
            sh$(5%)     = bil$(rec%,14%)     /* Entered - S1       */
            shc$(5%)    = bil$(rec%,15%)     /* Calc-S1 Fraction   */
            sh$(6%)     = bil$(rec%,16%)     /* Entered - S2       */
            shc$(6%)    = bil$(rec%,17%)     /* Calc-S2 Fraction   */
            sh$(7%)     = bil$(rec%,18%)     /* Entered - Edgework */
            shc$(7%)    = bil$(rec%,19%)     /* Calc-Edgework Y-N  */
            sh_glass$   = bil$(rec%,20%)     /* Shape Glass Code   */
            sh_sandwich$=bil$(rec%,21%)      /* Shape Sandwich     */
            sh_config_d$= bil$(rec%,22%)     /* Shape Description  */
            sh_txt$     = bil$(rec%,23%)     /* Label Text         */
            sh_hub$     = bil$(rec%,24%)     /* Hub AWD091         */

            rec%        = 299%
        return clear all
        goto editpg5

        find_record
          gosub initialize_shapes
          for rec% = 1% to val_max%
              if cc$(rec%) = " " then goto no_record
                 gosub load_shapes_screen
                 rec% = val_max%
no_record:
          next rec%
        return                               /* (EWD033)  - End    */


        deffn'156(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L56170,         /* Glass Process Selectio*/ ~
                              L56470,         /* Mach 0=Both,1=GED,2=Bi*/ ~
                              L56660,         /* Planned Production Dat*/ ~
                              L56940,         /* Department Code or All*/ ~
                              L57090,         /* Shift Code or AA = All*/ ~
                              L57220,         /* Glass Completion DTE  */ ~
                              L57400          /* Load Number           */
            return

L56170: REM Process Selection                     SCR_SEL$
            if size$ <> " " then goto L56200
               size$ = "0050"                    /* Default Batch Size */
L56200:     convert size$ to size%, data goto L56420

            convert size% to size$, pic(####)

            if size% > 200% then goto L56420
            scr_sel% = 0%
            if scr_sel$ <> " " then goto L56280
               scr_sel$ = "2"
L56280:     convert scr_sel$ to scr_sel%, data goto L56250


L56250:     if scr_sel$ = "B" then scr_sel% = 12% /*(AWD123)*/
            if scr_sel$ = "C" then scr_sel% = 13% /*(IM8022)*/
            if scr_sel% = 0% then goto L56380
                                                 /* (EWD005) 12/02/98 */
REM            IF SCR_SEL% < 2% OR SCR_SEL% > 3% THEN GOTO L50380
            if scr_sel% = 2% or scr_sel% = 3% then goto good_sel
            if scr_sel% = 4% then goto good_sel  /*(CR2773) */
            if scr_sel% = 5% then goto good_sel  /*(AWD111) */
            if scr_sel% = 6% then goto good_sel  /*(CR2773) */
            if scr_sel% = 7% then goto good_sel  /*(AWD111) */
            if scr_sel% = 12% then goto good_sel  /* (AWD123) */
            if scr_sel% = 13% then goto good_sel  /* (IM8022) */
                 goto L50380
good_sel:

            scr_msg$ = str(scr_temp$(scr_sel% + 1%),4%,31%)
            if scr_sel% > 10% then             ~
               scr_msg$ = str(scr_temp$(scr_sel% - 9%),38%,31%)
            if scr_sel% <> 1% then return
               init(" ") scr_sel$, scr_msg$, ged_bilco$, ged_desc$,      ~
                         size$, scr_dte$, scr_dept$, scr_msg1$,          ~
                         scr_shft_d$, glass_dte$, scr_load$, scr_desc$
               gosub process_tempered
        return
L56380:     errormsg$ = "(Error)-Invalid Process Selection (2,3,8,9)?"
            gosub error_prompt
            init(" ") scr_sel$, size$, scr_msg$
        return
L56420:     errormsg$ = "(Error) - Invalid Size Specification."
            gosub error_prompt
            init(" ") scr_sel$, size$, scr_msg$
        return

L56470: REM Machine Flag                          GED_BILCO$
           init(" ") ged_desc$
           if ged_bilco$ <> " " then goto L56510
              ged_bilco$ = "1"                     /* Default to 'GED' */
L56510:    if ged_bilco$ = "0" then ged_desc$ = "Both GED & Bilco  "
           if ged_bilco$ = "1" then ged_desc$ = "GED Glass Machine "
           if ged_bilco$ = "2" then ged_desc$ = "Bilco Glass Machine"
           if len(ged_desc$) < 5 then goto L56610


           if scr_sel% = 2% or scr_sel% = 8% then return
           if scr_sel% = 4% then return    /* (CR2773) */
           if scr_sel% = 5% then return    /* (AWD111) Stock Patio */
           if scr_sel% = 12% then return   /* (AWD123) laminate    */
              init(" ") scr_dte$, scr_dept$, scr_msg1$, scr_shft$,       ~
                        scr_shft_d$, glass_dte$, scr_load$, scr_desc$
              gosub process_tempered
        return
L56610:    errormsg$= "(Error)-Invalid Glass Machine Selection (0,1,2)?"
           gosub error_prompt
           init(" ") ged_bilco$, ged_desc$
        return
        return clear all
        goto inputmode_tempered


L56660: REM Production Date                       SCR_DTE$, SCR_DTE1$
           date% = 0%
           call "DATEOK" (scr_dte$, date%, errormsg$ )
           if errormsg$ <> " " then return
              scr_dte1$ = scr_dte$
              call "DATUNFMT" (scr_dte1$)
              dt_key1$ = all(hex(00))
              str(dt_key1$,1%,6%) = str(scr_dte1$,1%,6%)
              read #2,key 1% > dt_key1$, using L50760, dt_key1$,          ~
                                                      eod goto L56890

              if str(scr_dte1$,1%,6%) <> str(dt_key1$,1%,6%) then         ~
                                                              goto L56890
        REM Remake Production Date
           call "DATE" addr("GD",str(scr_dte1$,1%,6%),dd$,date%)
           dd1$ = dd$ : num$ = "7"
           if dd1$ = "MON" then num$ = "1"
           if dd1$ = "TUE" then num$ = "2"
           if dd1$ = "WED" then num$ = "3"
           if dd1$ = "THU" then num$ = "4"
           if dd1$ = "FRI" then num$ = "5"
           if dd1$ = "SAT" then num$ = "6"

        return
L56890:    errormsg$ = "No Data on File for Specified Production Date"
           gosub error_prompt
           init(" ") scr_dte$, scr_dte1$
        return

L56940: REM Department Selection                  SCR_DEPT$
            if scr_dept$ <> " " then goto L56990
               scr_dept$ = "ALL"
               scr_msg1$ = "(A)LL Process all Dept's"
               goto L57020
L56990:     gosub lookup_dept
            if dept% = 0% then goto L57040

L57020: REM FF$   = STR(SCR_DTE1$,3%,4%) & "@" & SCR_DEPT$
            ff$ = str(scr_dte$,1,2) & str(scr_dte$,4,2) & "@" & scr_dept$
        return
L57040:     errormsg$ = "(Error) - Invalid Department Selection?"
            gosub error_prompt
            init(" ") scr_dept$, scr_msg1$
        return

L57090: REM Shift Selection                       SCR_SHFT$
            if scr_shft$ <> " " then goto L57140
               scr_shft$ = "AA"
               scr_shft_d$ = "(AA) Process all Shift's"
               return
L57140:     gosub lookup_shift
            if shft% = 0% then goto L57170
        return
L57170:     errormsg$ = "(Error) - Invalid Shift Selection?"
            gosub error_prompt
            init(" ") scr_shft$, scr_shft_d$
        return

L57220: REM Glass Production Date                 GLASS_DTE$,GLASS_DTE1$
           if scr_sel% = 2% then goto L57260
           if scr_sel% = 8% then goto L57260
           if scr_sel% = 4% then goto L57260   /* (CR2773) */
           if scr_sel% = 5% then goto L57260   /* (AWD111) */
           if scr_sel% = 6% then goto L57260   /* (CR2773) */
           if scr_sel% = 12% then goto L57260   /* (AWD113) */

              init(" ") scr_dept$, scr_msg1$, scr_load$, glass_dte$
              return
L57260:    date% = 0%
           if glass_dte$ <> " " then goto L57300
              goto L57350

L57300:    call "DATEOK" (glass_dte$, date%, errormsg$ )
           if errormsg$ <> " " then return
              glass_dte1$ = glass_dte$
              call "DATUNFMT" (glass_dte1$)
        return
L57350:    errormsg$ = "(Error) - Invalid Glass Production Date??"
           gosub error_prompt
           init(" ") glass_dte$,glass_dte1$
        return

L57400: REM LOAD NUMBER
           if scr_sel% <> 1% then goto L57450
              init(" ") scr_dept$, scr_msg1$, scr_load$, glass_dte$,     ~
                        scr_desc$, stk_so$
              return
L57450:    if scr_load$ <> " " then goto L57500
L57460:
              scr_load$ = "ALL  "
              scr_desc$ = "(ALL) - Loads"
              return

L57500:    if str(scr_load$,1%,3%) = "ALL" then goto L57460
           ld_key$ = all(hex(00))
           ld_key$ = scr_load$
           read #5,key = ld_key$, using L51540, scr_desc$, eod goto L57560

        return
L57560:    errormsg$ = "(Error) - Invalid Load Number Entered?"
           gosub error_prompt
           init(" ") scr_load$, scr_desc$, stk_so$
        return
           errormsg$ = "(Error) - Valid Load Number is Required?"
           gosub error_prompt
           init(" ") scr_load$, scr_desc$, stk_so$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~--------------+
L55060: %!---------------------------------------------------------------~
        ~--------------!
L55080: %!######## @ ########        #########################           ~
        ~    Page: ### !
L55100: %!Begin Date: ##########                                         ~
        ~              !

L55120: %!End Date  : ##########  ##############################         ~
        ~              !

        REM - Detail
L55150: %! Barcode !Dpt!SeqNo!Mod!Load !Gl! Grid   !Width  !Height! S. O.~
        ~  !Reason Desc!
L55170: %!#########!###!#####!###!#####!##!########!#######!######!######~
        ~##!###########!
L55190: %!---------!---!-----!---!-----!--!--------!-------!------!------~
        ~--!-----------!

L55220: %!Report Totals : ##########                                     ~
        ~              !
                                     /* (EWD070) Special Shapes Formats */
                                     /* Total and Signature Line        */
L55300: %!Custom Rec: ####### Total: ####### Signature: ________________ ~
        ~Date:   /  /  !
                                     /* Header Line                     */
L55310: %!Begin Date: ##########                  Scanning Time Window : ~
        ~ #### To #### !

                                     /* (EWD070)                        */

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************
        lookup_hows                              /* (AWD080)    */
            init(" ") or_hows$
            read #14,key 4% = so$, using L60021, or_hows$, eod goto L60060
L60021:        FMT POS(92), CH(2)

        return                                  /* (AWD080)   */

        lookup_so
REM            IF STR(RM_NUM$,3%,1%) <> "0" THEN RETURN  /* SKIP RE-MAKES */
REM            INIT(" ") OR_HOWS$
REM            READ #14,KEY 4% = SO$, USING L60021, OR_HOWS$,           ~
                                                          EOD GOTO L60060
REM L60021        FMT POS(92), CH(2)
                                                 /* Set for Backorders */
            if or_hows$ = "21" then str(rm_num$,1%,1%)= "9"
                                                 /* Set for UPS S.O.'s */
REM         IF OR_HOWS$ = "01" OR OR_HOWS$ = "02" OR OR_HOWS$ = "03" OR  ~
               OR_HOWS$ = "11" OR OR_HOWS$ = "25" THEN                   ~
               STR(RM_NUM$,1%,1%) = "0"
            if or_hows$ = "04" or or_hows$ = "06" then rm_num$ = "SAM"
            if or_hows$ = "05" then rm_num$ = "DIS"
/*(AWD083) changed to array */
            if str(lab_rec$(),69%,1%) = "4" then rm_num$ = "TSO"
            if str(lab_rec$(),69%,1%) = "5" then rm_num$ = "BSO"
            if str(lab_rec$(),69%,1%) = "6" then rm_num$ = "FGO"
/*(AWD020)*/ if str(lab_rec$(),69%,1%) = "7" then rm_num$ = "OGO"
                                            /* (EWD005) - Indy Labels */
            if or_hows$ = "20" then rm_num$ = "IND"
                                            /* (EWD005)               */
        return
L60060:     or_hows$ = "EE"
        return

        create_rmk_data                    /* (EWD006) for Labels      */
            mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work
            init(" ") ff$, scr_dte$
            init(hex(00)) rm_key$
                                           /* Set All Dates to todays  */
            glass_dte$ = date$             /* Date for re-make runs    */
            scr_dte$   = date$
            scr_dte1$  = scr_dte$          /* Production Date is 'Now' */
            call "DATUNFMT" (scr_dte1$)

            str(ff$,1%,2%) = str(date$,1%,2%)
            str(ff$,3%,2%) = str(date$,4%,2%)
            str(ff$,5%,4%) = "@RMK"
            rm_flag% = 1%                  /* (EWD005) Set for Re-makes*/
            if tempScrn% = 1% then rm_flag% = 3%           /* (EWD055) */
            if lamnScrn% = 1% then rm_flag% = 5%           /* (IM8022) */
            ged_cnt% = 0%
            str(rm_key$,1%,1%) = "0"       /* Only Status '0' Remakes  */

        create_rmk_nxt
/* (EWD055) */
/*(AWD083) changed lab_rec to array and changed fmt size */
            read #8,key 3% > rm_key$, using L60123, lab_ged$, lab_rec$(), ~
                                                  eod goto create_rmk_done
L60123:        FMT CH(66), 2*CH(223)    /* (AWD112) */


            rm_key$    = str(lab_ged$,13%,21%)
            rm_st$     = str(rm_key$,1%,1%)
            rm_reason$ = str(lab_ged$,34%,2%)     /* (AWD083)*/
            rm_dept$   = str(lab_rec$(),183%,3%)  /* (EWD071) Department */
            glstype$   = str(lab_rec$(),269%,20%) /* (IM8022) */
            if rm_st$ <> "0" then goto create_rmk_done
                                                /* (EWD005) Selections */
                                                /* (EWD053) Remake Sort*/
                                                /* (EWD071) Check '043'*/
                                                /* Only process Shapes */

            valance% = 0%                      /* (AWD118) */

            init(" ") model$, ty$, sub_part$, dt_part$
            dt_part$ = str(lab_rec$(),59%,25%)
            model$ = str(dt_part$,1%,3%)
            ty$ = str(lab_rec$(),257%,2%)
            if ty$ <= " " then ty$ = str(lab_rec$(),63%,2%)  /* POS 323 */
/* (AWD117) */
/* (AWD115) */
            dt_seq$ = str(lab_rec$(),176%,5%)
            init(" ") awdschgl_key0$
            awdschgl_key0$ = str(rm_key$,10%,9%)
            gosub checkAWDSCHGL
            if awdschgl% = 1% then ty$ = gl_gls$
/* (\AWD115) */
REM sub_part is 257% - 66% = 191%
            sub_part$ = str(lab_rec$(),191%,20%)
            gosub lookup_intercept               /* (AWD112) */
            lt$ = str(dt_part$,7%,2%)
            gosub check_valance

            init(" ") view$                      /* (AWD116) */
            if str(rm_key$,18,1) < "5" then view$ = "TOP"               ~
                                       else view$ = "BOT"
            gosub get_ged_adjust                 /* (AWD116) */
            str(lab_rec$(),141%,6%) = w_adj$
            str(lab_rec$(),147%,6%) = h_adj$
            str(lab_rec$(),243%,7%) = interoffset$
            str(lab_rec$(),250%,7%) = interadj$

            if tempScrn% = 1% then goto temperedRmkSelection
               if str(glsType$,1%,8%) = "TEMPERED" then goto create_rmk_nxt
                               /* Skip the Shapes Dept if not selection 5*/
/* NC & NTX Shapes */
               if scr_sel$ = "5" and rm_dept$ <> "043" and schema% = 1% ~
                                       then goto create_rmk_nxt
               if scr_sel$ = "5" and                                    ~
                              (rm_dept$ <> "002" and rm_dept$ <> "003") ~
                                 and schema% = 2% then goto create_rmk_nxt
               if scr_sel$ <> "5" and rm_dept$ = "043" and schema% = 1% ~
                               then goto create_rmk_nxt
               if scr_sel$ <> "5" and                                   ~
                                (rm_dept$ = "002" or rm_dept$ = "003")  ~
                                and schema% = 2% then goto create_rmk_nxt

/* (AWD118) */
/* VALANCE */
               if scr_sel$ = "9" and valance% <> 1% then                ~
                                                     goto create_rmk_nxt
               if scr_sel$ <> "9" and valance% = 1% then                ~
                                                     goto create_rmk_nxt

/* Annealed Laminate */
               if scr_sel$ = "7" and                                    ~
                               str(glstype$,1%,13%) <> "ANNEALEDLAMIN"  ~
                                                 then goto create_rmk_nxt
               if scr_sel$ <> "7" and                                   ~
                                str(glstype$,1%,13%) = "ANNEALEDLAMIN"  ~
                                                 then goto create_rmk_nxt


/*(/AWD109)*/

/* Everything else scr_sel% = 3% */
               goto remakeHit          /* Everything else */
temperedRmkSelection:
               if str(glsType$,1%,8%) = "ANNEALED" then goto create_rmk_nxt

               init(" ") ty$, dt_part$
               ty$ = str(lab_rec$(),257%,2%)
               if ty$ = " " then ty$ = str(lab_rec$(),63%,2%)
               dt_part$ = str(lab_rec$(),59%,25%)

               gosub lookup_temp         /* 0=NotPatio 1=Door 2=StockSize*/
               if scr_sel% = 7% and patio$ <> "2" then goto create_rmk_nxt
               if patio$ = "2" and scr_sel% <> 7 then goto create_rmk_nxt

               if scr_sel% = 13% and                                      ~
                                 str(glstype$,1%,13%) <> "TEMPEREDLAMIN"  ~
                                                 then goto create_rmk_nxt
               if scr_sel% <> 13% and                                     ~
                                  str(glstype$,1%,13%) = "TEMPEREDLAMIN"  ~
                                                 then goto create_rmk_nxt

               if scr_sel% = 6% and stktmp% = 1% then goto remakeHit

               if scr_sel% = 6% and                                      ~
                                str(glstype$,1%,13%) <> "TEMPEREDSTK"    ~
                                                then goto create_rmk_nxt
               if scr_sel% <> 6% and                                     ~
                                 str(glstype$,1%,13%) = "TEMPEREDSTK"    ~
                                                then goto create_rmk_nxt


REM            IF SCR_SEL% = 11% AND PATIO$ <> "1" THEN GOTO CREATE_RMK_NXT
REM            IF PATIO$ = "1" AND SCR_SEL% <> 11 THEN GOTO CREATE_RMK_NXT


/* (AWD112) */
remakeHit:
               str(lab_rec$(),233%,1%) = "0"    /* Unused Flag for Ultra */

                                                /* (EWD071)            */
                      /* (AWD074) - Make Default 1 for reason flag!!!!!*/
               rm_reason% = 0% : reason_flag% = 1% /* Default Production*/
               convert rm_reason$ to rm_reason%, data goto L60125
L60125:                                            /* Glass House Last  */
                                                   /* (EWD061)          */
             if tempScrn% = 1% then goto L60130  /* (AWD112) */
              if rm_reason% >= 11% and rm_reason% <= 18% then             ~
                                                          reason_flag% = 0%
              if rm_reason% =  20% then reason_flag% = 0%
              if rm_reason% > 89% and rm_reason% <= 100% then             ~
                                                          reason_flag% = 0%

               if scr_sel$ = "3" then goto L60130
                                                /* (EWD071) Same as '3' */
               if scr_sel$ = "5" then goto L60130
                                                /* (EWD030)             */

                                                 /* (EWD030)           */
L60130:
/* CR3166 */
            sandwich$ = str(lab_rec$(),115%,10%)
            gosub lookup_lowebarcode  
            str(lab_rec$(),290%,4%) = lowebarcode$
			nbr_line% = 0%
            init(" ") text$()
            call "APCPLTXT" (#9, dt_txt$, text$(), nbr_line%)
			str(lab_rec$(),294%,40%) = str(text$(2%),1%,40%)  
			gosub lookup_hows
			str(lab_rec$(),334%,2%) = or_hows$			
			
            gosub remake_key
                                                 /* Note (APCPLNGR) is */
                                                 /* not changed at all */
                                                 /* (EWD005) - Done    */

            write #6, using L60124, lab_key$, lab_ged$, lab_rec$(),   ~
                                                            eod goto L60156
L60124:        FMT CH(62), CH(66), 2*CH(223)
            goto create_rmk_nxt                      /* (AWD112) */
        create_rmk_done
            if ged_cnt% = 0% then gosub prompt_no_data
        return
L60156:     call "SHOSTAT" ("Err Remake Update Key ---> "& rm_key$)
               stop
            call "SHOSTAT" ("Sales Order No. -> "&str(lab_rec$(),97%,8%))
               stop
            close ws
        return

        remake_key                           /* (EWD005) Build Glass   */
/* (AWD112) set up new lab_key$ */
            str(lab_key$,1%,2%) = intercept$
            str(lab_key$,3%,15%) = "000000000000000"
/* (\AWD112)  */

            ged_cnt% = ged_cnt% + 1%         /* work Record            */
            convert ged_cnt% to ged_cnt$, pic(#####)

            str(lab_ged$,1%,2%)  = "00"      /* Production Sort Code   */
            if reason_flag% = 1% then str(lab_ged$,1%,2%) = "01"
                                             /* Production 1st         */
                                             /* Glass House 2nd        */
                                             /* (EWD053)               */
            str(lab_ged$,3%,6%)  = str(lab_rec$(),156%,6%) /* Spacer Thil*/
            str(lab_ged$,9%,5%)  = "00000"     /* Seq No. not Applicable */
            str(lab_ged$,14%,3%) = str(lab_rec$(),6%,3%)   /* Model Code */
            str(lab_ged$,17%,5%) = "00000"     /* Seq No. not Applicable */
            str(lab_ged$,22%,7%) = str(lab_rec$(),84%,7%)  /* Window Wid */
            str(lab_ged$,29%,6%) = str(lab_rec$(),91%,6%)  /* Window High*/
            str(lab_ged$,35%,2%) = str(lab_rec$(),11%,2%)  /* Glass Type */
            str(lab_ged$,37%,1%) = str(lab_rec$(),9%,1%)   /* Color Code */
            str(lab_ged$,38%,1%) = "0"                     /* 0=TOP,1=BOT*/
            if str(lab_rec$(),45%,1%) = "B" then str(lab_ged$,38%,1%) = "1"
            str(lab_ged$,39%,6%) = str(lab_rec$(),162%,6%) /* Overall Thi*/
            str(lab_ged$,45%,8%) = str(lab_rec$(),168%,8%) /* Muttin     */
            str(lab_ged$,53%,9%) = str(rm_key$,10%,9%)   /* Glass Bar  */
            str(lab_ged$,62%,5%) = ged_cnt$              /* Record Cnt */
            if tempScrn% = 1% then gosub remake_key_temp  /*  (EWD055)  */
            dt_dept$ = str(lab_rec$(),183%,3%)
            str(lab_rec$(),191%,20%) = sub_part$


            gosub check_bilco
            if ged_bilco$ = "0" then g_b$ ="2"/* Set for Both Bilco/GED*/
            str(lab_rec$(),189%,1%) = g_b$      /* Re-Set Bidge Flag     */
            str(lab_rec$(),190%,1%) = " "       /* Clear INDY Flag       */
        return                                  /* if it is set          */
        remake_key_temp                         /* (EWD055) - Beg        */
             convert str(lab_rec$(),125%,7%) to width, data goto L60150
L60150:
             convert str(lab_rec$(),133%,6%) to height, data goto L60155
L60155:
             space_d$ = str(lab_rec$(),105%,10%)
             dt_dept$  = str(lab_rec$(),183%,3%)
             init(" ") dt_part$
             dt_part$ = str(lab_rec$(),59%,25%)

             gosub checkTempGed
             gosub get_sort_temp

             str(lab_key$,8%,5%) = srt$              /* (AWD112) */
             str(lab_ged$,1%,2%) = "00"
             str(lab_ged$,3%,6%)  = str(lab_rec$(),156%,6%)/*SpacerThknes*/
             str(lab_ged$,9%,5%)  = "00000"

             str(lab_ged$,14%,3%) = str(lab_rec$(),59%,3%)  /* Model Code*/
             str(lab_ged$,17%,5%) = "00000"        /* Sequence Number    */
             str(lab_ged$,22%,7%) = str(lab_rec$(),125%,7%)/*WindowWidth */
             str(lab_ged$,29%,6%) = str(lab_rec$(),133%,6%)/*WindowHeight*/
             str(lab_ged$,35%,2%) = str(lab_rec$(),63%,2%)/*GlassType Cde*/
             str(lab_ged$,37%,1%) = str(lab_rec$(),62%,1%)/* Color Code  */
             str(lab_ged$,38%,1%) = "0"            /* 0 = TOP, 1 = BOT   */
             if str(lab_rec$(),45%,1%) = "B" then                         ~
                                                 str(lab_ged$,38%,1%) = "1"




             str(lab_rec$(),154%,2%)  = ss$
             str(lab_rec$(),141%,6%) = w_adj$
             str(lab_rec$(),147%,6%) = h_adj$
             str(lab_rec$(),243%,7%) = interoffset$
             str(lab_rec$(),250%,7%) = interadj$
             str(lab_rec$(),257%,2%) = ty$

        return                                /* (EWD055)  - END       */


        create_data                           /* Create Glass Cut Data */
            call "SHOSTAT" ("Calculating Glass Cuts")
            mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work

            gl_sort% = 0%                     /* (EWD056)              */
            work_shape% = 0%                  /* (EWD056)              */
            shape_counter% = 0%               /* (EWD056)              */
            shapeBatch% = 0%

            ged_cnt% = 0%
            rm_num$  = "000"                  /* (EWD005) All Glass    */
                                              /* Starts at '000'       */
            rm_flag% = 0%                     /* (EWD005) Pass to Subs */
            if tempScrn% = 1% then rm_flag% = 2%
            if lamnScrn% = 1% then rm_flag% = 4%      /* (AWD123) */
            init(" ") filler$, sav_mod$       /* '0%' = Not Re-make Run*/
            dt_key1$ = all(hex(00))
            str(dt_key1$,1%,6%) = str(scr_dte1$,1%,6%)    /* Prod. Date */
            read #2,key 1% > dt_key1$, using L60330, dt_rec$,             ~
                                                     eod goto create_next
            goto L60339
        create_next
            if scr_sel% <> 1% then goto L60327      /* Screen Explosion */
               if ct% <> 0% then return
                  errormsg$ = "(Error) - Unable to Calculate Glass?"
                  gosub error_prompt
                  return

L60327:     read #2, using L60330, dt_rec$, eod goto create_done
L60330:        FMT CH(256)


L60339:
REM ===============
REM test readkey for scr_dte1$
REM ===============
            dt_date$ = str(dt_rec$,47%,6%)      /* Production Date     */
            if dt_date$ <> str(scr_dte1$,1%,6%) then goto create_done
REM =================
REM test screen selections
REM =================
            if scr_shft$ = "AA" then goto L60378        /* All Shifts   */
               if scr_shft$ <> dt_shft$ then goto create_next
L60378:     dt_load$ = str(dt_rec$,1%,5%)       /* Production Load No. */
            if str(scr_load$,1%,3%) = "ALL" then goto L60387 /*All Loads*/
               if dt_load$ <> scr_load$ then goto create_next
               goto L60390                           /* Skip Alpha Loads */
L60387:     if str(dt_load$,1%,1%) = "A" then goto create_next

L60390:
REM =================
REM test department checks
REM =================
            dt_dept$ = str(dt_rec$,42%,3%)
            if scr_dept$ = "ALL" then goto L60363      /* All Depart's */
               if dt_dept$ <> scr_dept$ then goto create_next

               if scr_dept$ = "043" and schema% = 1% then shapeBatch% = 1%
               if scr_dept$ = "002" and schema% = 2% then shapeBatch% = 1%
               if scr_dept$ = "003" and schema% = 2% then shapeBatch% = 1%
               if scr_dept$ = "064" and schema% = 2% then shapeBatch% = 1%
               goto L60369
L60363:     gosub check_support
            if supp% = 1% then goto create_next        /* Skip Support */
                                                       /* Skip Shapes  */
                                                       /* Department   */

            if dt_dept$ = "043" and schema% = 1% then goto create_next
            if (dt_dept$ = "002" or dt_dept$ = "003") and schema% = 2% ~
                    then goto create_next
                                                       /* (AWD087) */
L60369:

REM CALL "SHOSTAT" ("HERE IN CREATE DATA ") STOP

            if  dt_dept$ = "009" or str(dt_rec$,42%,3%) = "046"          ~
                              then goto create_next /* BAY/BOW & MULLS */
            if dt_dept$ = "101" then goto create_next
            if dt_dept$ = "102" then goto create_next
            if dt_dept$ = "103" then goto create_next
            if dt_dept$ = "104" then goto create_next
/* (AWD110) */

            sh_hub$  = "    "  /* AWD091 */
            sh_hub1$ = "    "  /* AWD091 */
            sh_hub2$ = "    "  /* AWD091 */
            sh_hub3$ = "    "  /* AWD091 */
            gosub dataload
            if len(dt_part$) < 19 then goto create_next

            kanban% = 0%                                 /* (AWD110) */
            if dt_dept$ = "023" and schema% = 1% then gosub checkKanbanpt
            if dt_dept$ = "020" and schema% = 1% then gosub checkKanbanpt
            if dt_dept$ = "033" and schema% = 1% then gosub checkKanbanpt
/*(CR976)*/ if dt_dept$ = "041" and schema% = 2% then gosub checkKanbanpt
            if kanban% = 1% then goto create_next        /* (AWD110) */

            if dt_prt$ = "Y" then goto create_next     /* Skip Parts   */
                                                       /* No Glass N/A */
            if dt_gls$ = "N" and calsl% = 0% then goto create_next
                                                       /* (EWD018)     */
            gosub check_samples                        /* Skip parts   */
/* (AWD095) */
            if ss% > 11% and ss% < 29% then goto create_next
/* (AWD118) */
/* Tempered Glass Can be tempered */
REM            IF TEMPERED% =  1% THEN GOTO NOTEMPVALCHECK
            gosub check_valance
              if scr_sel$ = "2" and valance% = 1% then goto create_next
              if scr_sel$ = "8" and valance% <> 1% then goto create_next

noTempValCheck:
/* (\AWD118) */
REM CALL "SHOSTAT" ("HERE IN CREATE DATA ") STOP
            ty$ = str(dt_part$,5%,2%)                  /* Glass type   */
REM =================================
REM Had to move glass check further down because of
REM different glass configurations in a window
REM =================================
REM**   GOSUB LOOKUP_TEMP
REM (AWD115)  assume will process
REM**   PROCESS% = 1%
REM**   IF TEMPERED% =  1% THEN GOSUB PRODTEMPEREDLOGIC
REM**   IF TEMPERED% <> 1% THEN GOSUB PRODLOGIC
REM**   IF PROCESS% = 0% THEN GOTO CREATE_NEXT
            gosub lookup_ultra                       /* (CR1990) */

        calc_data
            gosub hub_calc
            if scr_sel% = 1% then                                       ~
                            call "SHOSTAT" ("Calculating Glass Cuts")
            init(" ") view$
            t1% = 1% : ct% = 0%                        /* No. of Tops  */
            b1% = 1%                                   /* No. of Bots  */
            gosub check_thickness
            for i% = 1% to mod%(4%)            /* No Glass Required    */
              if mod$(4%,i%) = model$ then goto create_next
            next i%
            if dt_dept$ <> "008" then goto L60438  /*Check Casement Dept*/
               for i% = 1% to mod%(5%)         /* Check Valid 800's */
REM IF MOD$(5%,I%) = MODEL$ THEN GOTO L60435      /* (CR838) */
                   if mod$(5%,i%) = model$ then goto L60438
               next i%
               goto create_next
L60438:
            gosub isPatio                         /* (CR838)           */
            gosub case_glass                     /*Multiple Lits 800's*/
            gosub convert_fields                 /* Top Glass 1st     */
/* (AWD122) */
            gosub lookup_hldsched
REM GOSUB CHECK_PATIO              /* NUMBER OF PANNELS */  /* (CR838) */
            gosub check_cont_head                 /*  (EWD048)         */
            if sc$ = "5" then t1% = 0%            /* No Top Glass      */
            if sc$ = "7" then t1% = 0%            /* NoTopGlass(AWD120)*/
                                                  /* Process TOP Glass */
            if t1% = 0% then goto L60516          /*No Top Glass Needed*/
               view$ = "TOP" :  cal% = 1%         /* Process All Top   */
               gosub lookup_phantom               /* Glass             */
               gosub check_for_shape
/* NC & NTX Shapes */
                  if scr_dept$ <> "043" and schema% = 1% then goto L60475
                  if (scr_dept$ <> "002" and scr_dept$ <> "003") ~
                       and schema% = 2% then goto L60475
                  if mod(shape_counter%,10) = 0 then                    ~
                    call "SHOSTAT" ("Shapes Found ----> " & shape_counter$)
                                                  /* (EWD060)          */
                  if calsl% = 1% then create_next
L60475:
               ct% = 0%                           /* Init Cut Counter  */
REM (AWD115)
               for k% = 1% to t1%
REM *******
                  ct% = ct% + 1%
                  if scr_sel% = 1% then goto noTypeTopCheck
REM (AWD115)  assume will process
                  process% = 1%
                  init(" ") awdschgl_key0$
                  awdschgl_key0$ = dt_ref$
                  convert (ct% - 1%) to str(awdschgl_key0$,9%,1%), pic(#)
                  gosub checkAWDSCHGL
                  if awdschgl% = 1% then ty$ = gl_gls$
                  gosub lookup_temp
                  gosub checkScrnSel                 /* (CR2773) */

                  if process% = 0% then goto L60515
REM *******
noTypeTopCheck:
                  gosub update_work
L60515:        next k%
                                                  /* Now See IF Top    */
L60514:        for i% = 1% to mod%(2%)            /* Glass Only        */
                   if mod$(2%,i%) = model$ then goto create_next
               next i%
                                              /* Special-Top Sash Only */
               if sc$ = "4" or sc$ ="6" then goto create_next
                                                  /* Process BOT Glass */
L60516:        if b1% = 0% then goto create_next  /* No Bottom Glass   */
                  view$ = "BOT" : cal% = 1%       /* Process Bottom Gls*/
                  if sc$ = "5" then goto L60534
                  if sc$ = "7" then goto L60534   /*(AWD120) */
                     for i% = 1% to mod%(3%)   /* Check Two Bottoms */
                         if mod$(3%,i%) = model$ then b1% = b1% + 1%
                     next i%
                                                   /* TWO (2) BOTTOMS*/
                                                   /*  (EWD044)      */
L60534:
REM IF MODEL$ = "312" OR MODEL$ = "332" THEN GOTO PATIOBOTTOM
REM IF MODEL$ = "378" OR MODEL$ = "388" THEN GOTO PATIOBOTTOM
REM GOTO L60540
REM PATIOBOTTOM
REM IF HG$ = "42" OR HG$ = "37" THEN B1% = 2%
REM L60540
                  gosub lookup_phantom
/*+(CR2109)*/     gosub check_for_shape  /* NTX Operable Shapes */
                    if schema% = 1% then goto L60480  /* Do not show for NC */
                    if scr_dept$ <> "064" and schema% = 2% then goto L60480
                    if mod(shape_counter%,10) = 0 then                    ~
                      call "SHOSTAT" ("Shapes Found ----> " & shape_counter$)
                      if calsl% = 1% then create_next
L60480:
/*-(CR2109)*/
                  ct% = 5%
REM (AWD115)

               for k% = 1% to b1%        /* Multiple Bottoms Gls */
REM *******
                  ct% = ct% + 1%
                  if scr_sel% = 1% then goto noTypeBotCheck
REM (AWD115)  assume will process
                  process% = 1%
                  init(" ") awdschgl_key0$
                  awdschgl_key0$ = dt_ref$
                  convert (ct% - 1%) to str(awdschgl_key0$,9%,1%), pic(#)
                  gosub checkAWDSCHGL
                  if awdschgl% = 1% then ty$ = gl_gls$
                  gosub lookup_temp
                  gosub checkScrnSel                 /* (CR2773) */


REM        IF LAMINATE% =  1% THEN GOSUB PRODLAMINATELOGIC /* (AWD123) */
REM        IF TEMPERED% =  1% THEN GOSUB PRODTEMPEREDLOGIC
REM        IF TEMPERED% <> 1% AND LAMINATE% <> 1% THEN GOSUB PRODLOGIC
REM        TY$ = STR(DT_PART$,5%,2%)

                  if process% = 0% then goto L60517
REM *******
noTypeBotCheck:
                  gosub update_work
L60517:        next k%
            goto create_next
        create_done
            if ged_cnt% = 0% then gosub prompt_no_data
            if shapeBatch% = 1% and shape_counter% = 0% then   ~
                                            gosub prompt_no_shape_data
        return
/* (CR2773) */
        checkScrnSel
          if tempScrn% <> 0% then goto tempScrnSel
             if scr_sel% = 2% then gosub prodLogic
             if scr_sel% = 4% then gosub prodLaminateLogic
             if scr_sel% = 6% then gosub prodLogic          /* CR3046 */
             if scr_sel% = 8% then gosub prodLogic
        return
        tempScrnSel
             if scr_sel% = 2% then gosub prodTemperedLogic
             if scr_sel% = 4% then gosub prodTemperedStockLogic
             if scr_sel% = 5% then gosub prodTemperedLogic
             if scr_sel% = 12% then gosub prodlaminatetemperedlogic /* B=12% */
        return
/* (AWD115) */
        prodLogic
REM temp1$ will not have * if gls code is 89 or topTemp% or botTemp% is 0
          if temp1$ = "*" then process% = 0%
/* (AWD123) */
          if gls_lamn% <> 0% then process% = 0%
          if stktmp% <> 0% then process% = 0%
          if process% = 1% then glsType$ = "ANNEALED"
        return
        prodTemperedLogic
           if temp1$ = " " then process% = 0%
REM !! SCR_SEL% 5% = Stock Patio
REM !! stockPatio% 0 = not stock
REM !! stockPatio% 1 = stock
           if scr_sel% = 5% and (door% = 0% or stockPatio% = 0%)     ~
                                                then process% = 0%
           if scr_sel% <> 5% and (door% <> 0% and stockPatio% <> 0%) ~
                                                then process% = 0%
/* (AWD123) */
            if stktmp% <> 0% then process% = 0%
            if gls_lamn% <> 0% then process% = 0%
            if process% = 1% then glsType$ = "TEMPERED"
        return
/* (\AWD115) */
/* (AWD123) */
        prodLaminateLogic
          if gls_lamn% = 0% then process% = 0%
          if temp1$ = "*" then process% = 0%
          if stktmp% <> 0% then process% = 0%
          if process% = 1% then glsType$ = "ANNEALEDLAMIN"
        return
/* (IM8022) */
        prodLaminateTemperedLogic
          if gls_lamn% = 0% then process% = 0%
          if temp1$ = " " then process% = 0%
          if stktmp% <> 0% then process% = 0%
          if process% = 1% then glsType$ = "TEMPEREDLAMIM"
        return
/* (\AWD123) */
/* (CR2773)  */
        prodTemperedStockLogic
          if temp1$ = " " then process% = 0%
          if temp$ = " "  then process% = 0%
          if gls_lamn% <> 0% then process% = 0%
          if door% <> 0% then process% = 0%
          

          if scr_sel% <> 4% and stktmp% <> 1% then process% = 0%
          if scr_sel% = 4% and stktmp% = 0% then process% = 0%
          if process% = 1% then glsType$ = "TEMPEREDSTK"
        return
/* (\CR2773) */


                                                      /* (EWD018)       */
        check_samples
            ss% = 0%
            if len(dt_part$) < 20 then goto LS2      /* Quick Test      */
            if str(dt_part$,1%,1%) = "9" then goto LS2 /* Bay/Bow       */
            convert str(dt_part$,20%,3%) to ss%, data goto LS1

            if ss% < 1% or ss% > 80% then goto LS1   /* Not Samp/Disp   */

                                                     /*   (EWD041)      */
            if str(dt_part$,7%,2%) > "99" then goto LS1

        return                                       /* Code Found      */
LS1:        convert str(dt_part$,23%,3%) to ss%, data goto LS2

            if ss% < 1% or ss% > 80% then goto LS2
                                                     /* Code Found      */
        return
LS2:        ss% = 0%
        return
                                                       /* (EWD018)      */

        update_work
            init(" ") lab_key$, lab_ged$, lab_rec$()
            ged_cnt% = ged_cnt% + 1%
            convert ged_cnt% to ged_cnt$, pic(#####)
            if err% <> 0% then goto create_error_rec

            gosub calc_wd                     /* Ctr line Meeting Rail*/
            gosub calc_wd_ht      /* Calculated Width/Height Long Form */
/* (AWD117) */
            tripane% = 0%
            gosub lookup_triplepane
            if tripane% <> 0% and intercept% <> 4% then intercept% = 4%
            if tripane% <> 0% and intercept$ <> "04" then intercept$ = "04"
            gosub build_ged_key                 /* Sets spacer */
            if sgp% = 0% then goto notSGP
              sandwich$ = str(sandwich$,1%,5%) & "L" & str(sandwich$,6%,4%)

notSGP:
            gosub check_stock               /* Check to see If Stock   */
            gosub check_precut              /*  (AWD075)               */

            if err% <> 0% then goto create_error_rec
            if scr_sel% <> 1% then goto L60657
               str(cut$(ct%),14%,9%) = wd1$              /* For Screen */
               str(cut$(ct%),32%,8%) = ht1$
               str(cut$(ct%),46%,9%) = wd2$
               if ct% <> 1% then goto L60636
                  str(ged$(1%),11%,6%) = t_k$
                  str(ged$(2%),11%,10%)= sandwich$
                  str(ged$(3%),11%,10%)= spacer$
                  str(ged$(4%),11%,1%) = lits$
                  str(ged$(5%),11%,8%) = muttin$
L60636:        if ct% <> 6% then goto L60654
                  str(ged$(6%),11%,6%) = t_k$
                  str(ged$(7%),11%,10%)= sandwich$
                  str(ged$(8%),11%,10%)= spacer$
                  str(ged$(9%),11%,1%) = lits$
                  str(ged$(10%),11%,8%)= muttin$
L60654:        return
L60657:     gosub update_record                  /* ONLY VALID LABELS */
        return

        create_error_rec
            if scr_sel% <> 1% then goto L60678
               errormsg$ = t_err$(err%)
               if err% = 1% then errormsg$ = t_err$(err%) & "  " & ~
                                                phantom$ & "  " & hgl$
               return
L60678:     str(lab_rec$(),1%,1%)  = "E"
            str(lab_rec$(),2%,5%)  = "99999"
            str(lab_rec$(),7%,25%) = dt_part$
            convert err% to str(lab_rec$(),32%,1%), pic(#)
            sort% = 999%
            convert sort% to srt$,pic(00000)        /* (AWD112) */
                                                    /* (EWD057)        */
/* (AWD105) */
            str(lab_key$,8%,5%)  = srt$             /* (AWD112) */
            str(lab_ged$,1%,2%)  = "00"             /* (AWD112) */
            str(lab_ged$,3%,6%)  = "ERROR*"
            str(lab_ged$,9%,5%)  = "*" & model$ & "*"
            str(lab_ged$,14%,2%) = ty$
            str(lab_ged$,16%,1%) = cl$
            str(lab_ged$,17%,5%) = dt_seq$
            str(lab_ged$,22%,5%) = ged_cnt$
            str(lab_ged$,27%,8%) = so$
            str(lab_ged$,35%,3%) = dt_dept$
            str(lab_ged$,38%,25%)= " "
            init(" ") wd1$, ht1$, wd2$
            goto L60927                             /* UPDATE BUFFER    */

        update_record
            gosub check_bilco                 /* Check for Bridge File */
            str(lab_rec$(),1%,5%)   = dt_load$           /* Load No.    */
            str(lab_rec$(),6%,3%)   = model$             /* Product Code*/
            str(lab_rec$(),9%,2%)   = cl$ & " "          /* Color Code  */
            str(lab_rec$(),11%,2%)  = ty_s$              /* Glass Type  */
            if str(view$,1%,1%)   = "T" then             /* Liting Descr*/~
                                       str(lab_rec$(),13%,6%) = l_lt$     ~
                                  else str(lab_rec$(),13%,6%) = r_lt$
            str(lab_rec$(),19%,9%)  = wd1$                /* Calc Width  */
            str(lab_rec$(),28%,8%)  = ht1$                /* Calc Height */
            str(lab_rec$(),36%,9%)  = wd2$                /* CLMR Width  */
            str(lab_rec$(),45%,1%)  = str(view$,1%,1%)    /* Top/Bot     */
            str(lab_rec$(),46%,4%)  = dt_txt$
            str(lab_rec$(),50%,9%)  = lab_fil$
            str(lab_rec$(),59%,25%) = dt_part$
            str(lab_rec$(),84%,7%)  = wd$                 /* WINDOW WD   */
            str(lab_rec$(),91%,6%)  = ht$                 /* WINDOW HT   */

            str(lab_rec$(),97%,8%)   = so$          /*Save Sales Order No*/
/* PATIO$ 0=NotPatio 1=Door 2=StockSize*/                  /* (EWD055)   */
            str(lab_rec$(),105%,10%) = space_d$     /* Store Spacer Desc */
            str(lab_rec$(),115%,10%) = sandwich$    /* Glass Sandwich Typ*/
            str(lab_rec$(),125%,8%)  = width_d$     /* Calc Width Decimal*/
            str(lab_rec$(),133%,8%)  = height_d$    /* Calc Height Decim */
            str(lab_rec$(),141%,6%)  = w_adj$       /* Width Adjustment  */
            str(lab_rec$(),147%,6%)  = h_adj$       /* Height Adjustment */
            str(lab_rec$(),153%,1%)  = temp$        /* Tempered Flag '*' */
            str(lab_rec$(),156%,6%)  = spacer$      /**Glass Spacer Size */
            str(lab_rec$(),162%,6%)  = t_k$         /**Glass Overall Thic*/
            str(lab_rec$(),168%,8%)  = muttin$      /**Glass Muttin Code */
            str(lab_rec$(),168%,8%)  = str(lab_ged$,45%,8%)
                                                    /* Note - Seq No.    */
                                                    /* could have leading*/
                                                    /* W, X, Y, or Z Sort*/
                                                    /* (EWD070)-Note     */
            str(lab_rec$(),181%,2%)  = dt_shft$     /* Dept Shift Code   */
            str(lab_rec$(),183%,3%)  = dt_dept$     /* Department Code   */
            if rm_num$ = "000" then str(rm_num$,1%,1%) = num$
                                           /* Set Production day of week */
            str(lab_rec$(),186%,3%)  = rm_num$      /* Remake Number     */
            str(lab_rec$(),189%,1%)  = g_b$         /* 0=GED,1=Bilco Only*/


REM CALL "SHOSTAT" ("LAB_REC1 189 -> " & STR(LAB_REC$(),189%,1%))  STOP
REM Do not use 190; it is reserved for INDY processing
            str(lab_rec$(),191%,20%) = sub_part$    /* (AWD112)          */
                                         /* (AWD112) Reserved for GLS SRT*/
            str(lab_rec$(),211%,22%) = " "
            str(lab_rec$(),233%,1%) = ultra$        /* (AWD109) ultra    */
            str(lab_rec$(),234%,1%) = patio$        /* (AWD111) patio    */
            str(lab_rec$(),235%,6%) = str(dt_rec$,47%,6%) /*Prod DT Date */
            str(lab_rec$(),241%,2%) = intercept$    /* (AWD112)          */
                                                    /* Rec Length (318)  */

                                                    /* (EWD005) Indy Flag*/
/* PATIO$ 0=NotPatio 1=Door 2=StockSize*/           /* (EWD055)          */
/* (AWD111) this is for indy's only!! */
               if tempScrn% <> 1% and scr_sel$ = "6"        ~
                  then str(lab_rec$(),190%,1%) = "1"
                                                  /* Place Spec at End */
/* (AWD112) */
               if sort% >= 900% then goto L60912
               goto L60879

REM !!!! This is wrong
               call "SHOSTAT" ("I AM HERE THIS IS WRONG") : stop
REM !               SS$ = STR(LAB_GED$,1%,2%)
REM !               SRT$ = SS$
REM !               GOTO L60912
               srt$ = str(lab_key$,8%,5%)
               goto L60912
L60879:
               if lt% > 82% and lt% < 88% then sort% = 951%  /* Diamond */
               if lt% > 96% and lt% <= 99% then sort% = 951%
               if lt$ = "V0" then sort% = 951%      /*(AWD118) custom val*/
/*(AWD114) */                                             /* TSO Diamond */
               if lt$ = "TD" then sort% = 951%
               if lt$ = "RZ" then sort% = 951%
               if lt$ = "SZ" then sort% = 951%

               if lt% = 59% then sort% = 951%
               convert sort% to srt$,pic(00000)        /* (AWD112) */

               if lt% > 82% and lt% < 88% and tempScrn% = 0% then        ~
                                                               goto L60915
               if lt% > 96% and lt% <= 99% and tempScrn% = 0% then       ~
                                                               goto L60915
/* (AWD118) custom val */
               if lt$ = "V0" and tempScrn% = 0% then goto L60915
               if lt% = 59% and tempScrn% = 0% then goto L60915
/*(AWD114)*/   if lt$ = "TD" and tempScrn% = 0% then goto L60915
               if lt$ = "RZ" and tempScrn% = 0% then goto L60915
               if lt$ = "SZ" and tempScrn% = 0% then goto L60915

               if lt% > 82% and lt% < 88% and tempScrn% = 1% then      ~
                                                             goto L60912
               if lt% > 96% and lt% <= 99% and tempScrn% = 1% then    ~
                                                             goto L60912
/* (AWD118) custom val */
               if lt$ > "V0" and tempScrn% = 1% then goto L60912
               if lt% = 59% and tempScrn% = 1% then goto L60912
               if lt$ = "TD" and tempScrn% = 1% then goto L60912
                                                  /*  (EWD063) */
/* (AWD115) */
               if awdschgl% = 1% and temp$ = "*" then goto L60912
/* (AWD123) */ if awdschgl% = 1% and lamnScrn% = 1% then goto L60912

               if skip$ = "*" or temp$ = "*" then goto L60888

REM               goto L60909
               goto L60912

L60888:     if temp$ <> "*" then goto L60900
               if tempScrn% = 1% then goto L60900


               sort% = 904%
               if dt_dept$ = "023" then sort% = 903%
               if dt_dept$ = "020" then sort% = 903%
               if dt_dept$ = "042" then sort% = 902%
/* (IM7733) */
REM               IF DT_DEPT$ = "025" THEN SORT% = 901%
/* NTX Shapes */
REM               IF DT_DEPT$ = "043" THEN SORT% = 900%
               if dt_dept$ = "043" and schema% = 1% then sort% = 900%
               if dt_dept$ = "002" and schema% = 2% then sort% = 900%
               if dt_dept$ = "003" and schema% = 2% then sort% = 900%
               convert sort% to srt$, pic(00000)
               str(lab_key$,1%,2%) = "98"
               str(lab_key$,8%,5%) = srt$
               str(lab_ged$,9%,1%) = "W"
               intercept% = 98%
               intercept$ = "98"
               goto L60918

L60900:
/* (AWD112) */
               if skip$ <> "*" then goto not_skip
                  sort% = 951%
                  convert sort% to srt$, pic(00000)
                  str(lab_key$,8%,5%) = srt$
                  str(lab_ged$,9%,1%) = "Y"

not_skip:
/* (\AWD112) */                                              /* (EWD055) */
               if temp$ = "*" and tempScrn% = 1% then goto L60912
               goto L60918

L60912:
/* (AWD123) */
            if (tempScrn% = 1% or lamnScrn% = 1%) and patio$ <> "2"     ~
                                              then goto update_record_temp
            gosub get_sort

/* (AWD105) change from ss to srt$ */
L60915:
            str(lab_key$,8%,5%) = srt$              /* (AWD112)          */
                                                  /* Final Sort Code & */
L60918:
* !!!!  What is this used for????
            str(lab_rec$(),154%,2%)  = srt$
REM            STR(LAB_REC$(),176%,5%)  = STR(LAB_GED$,9%,5%) /*DEPTSEQMOD*/
/* (AWD117) */
            str(lab_rec$(),176%,5%)  = dt_seq$

/* PATIO$ 0=NotPatio 1=Door 2=StockSize*/               /* (EWD055)      */
/* (AWD112) */
            str(lab_rec$(),211%,11%) = ss$(sort%)
            str(lab_rec$(),243%,76%) = " "      /*Filler 318 total length*/
/* (\AWD112) */
L60927:
/* (AWD116)*/
            convert interoffset(intercept%) to interoffset$, pic(0.00000)

            if interadj(intercept%) < 0.00 then     ~
             convert interadj(intercept%) to interadj$, pic(-0.0000) ~
            else                                                     ~
             convert interadj(intercept%) to interadj$, pic(0.00000)

            str(lab_rec$(),243%,7%) = interoffset$
            str(lab_rec$(),250%,7%) = interadj$
            str(lab_rec$(),257%,2%) = ty$
            str(lab_rec$(),259%,10%) = style$

            str(lab_rec$(),269%,20%) = glstype$         /* (IM8022) */
            if valance% = 0% then str(lab_rec$(),289%,1%) = "0"
            if valance% = 1% then str(lab_rec$(),289%,1%) = "1"
			
/* CR3166 */
            sandwich$ = str(lab_rec$(),115%,10%)
            gosub lookup_lowebarcode  
            str(lab_rec$(),290%,4%) = lowebarcode$
			nbr_line% = 0%
            init(" ") text$()
            call "APCPLTXT" (#9, dt_txt$, text$(), nbr_line%)
			str(lab_rec$(),294%,40%) = str(text$(2%),1%,40%)  
			gosub lookup_hows
			str(lab_rec$(),334%,2%) = or_hows$

REM CALL "SHOSTAT" ("LAB_REC1 189 -> " & STR(LAB_REC$(),189%,1%))  STOP
REM CALL "SHOSTAT" ("DEPT SEQ " & STR(LAB_REC$(),176%,5%))  STOP
REM IF DT_BAR$ = "070038110100010001" THEN GOSUB SHOWRECORD

            write #6, using L60930, lab_key$, lab_ged$, lab_rec$(), ~
                                                      eod goto L60936
L60930:     FMT CH(62), CH(66), 2*CH(223)      /* Rec Len = 384     */
        return                           /* (AWD112) new rec len = 512 */
L60936:     call "SHOSTAT" ("Err Write Lab (GED Key) -> "&               ~
                                 str(lab_ged$,1%,30%) ) : stop
            call "SHOSTAT" ("Sales Order No. -> "&str(dt_rec$,24%,8%) )
            stop
            close ws
        return

        showRecord

          call "SHOSTAT" ("SO lab_rec " & str(lab_rec$(),97%,8%) )
          stop
        return

        update_record_temp
          gosub get_sort_temp
        goto L60927

        convert_fields
            if new_part$ = sav_part1$ then return
                  sav_part1$ = new_part$

            init(" ") model$, cl$, ty$, lt$, hg$, sc$, lk$
            init(" ") field1$, field2$, field3$, field4$, field5$, ~
               field6$, field7$, field8$, field9$, field10$, field11$,~
/*(AWD112)*/   intercept$

            s_width = 0.0 : s_height = 0.0 : s_clmr = 0.0
            sh_hub$  = "    "
            sh_hub1$ = "    "
            sh_hub2$ = "    "
            sh_hub3$ = "    "

            lit_flg$ = "N"                             /* (EWD018)     */
            if str(dt_part$,1%,3%) = "003" then lit_flg$ = "Y"
                                                       /* (EWD018)     */
            model$    = str(dt_part$,1%,3%)            /* Model Number */
            cl$       = str(dt_part$,4%,1%)            /* Color        */
            ty$       = str(dt_part$,5%,2%)            /* Glass        */
            lt$       = str(dt_part$,7%,2%)            /* Liting       */
            hg$       = str(dt_part$,9%,2%)            /* Hinge        */
            sc$       = str(dt_part$,11%,1%)           /* Screen       */
            lk$       = str(dt_part$,12%,1%)           /* Locks        */
            field1$   = str(new_part$,26%,1%)          /* Grid Type    */
            field2$   = str(new_part$,27%,1%)          /* Grid Size    */
            field3$   = str(new_part$,28%,1%)          /* Grid Color   */
            field4$   = str(new_part$,29%,1%)          /* Hardware     */
            field5$   = str(new_part$,30%,1%)          /* Foam         */
            field6$   = str(new_part$,31%,1%)          /* Casing       */
            field7$   = str(new_part$,32%,1%)          /* Sample Color */
            field8$   = str(new_part$,33%,1%)          /* Grd Type     */
            field9$   = str(new_part$,34%,1%)          /* Grd Size     */
            field10$  = str(new_part$,35%,1%)          /* OutSide GrdCo*/
            field11$  = str(new_part$,36%,1%)          /* Inside GrdCol*/

            if lit_flg$ = "N" then goto L60950         /* (EWD018)     */
               wd$ = str(dt_part$,13%,4%)
               ht$ = str(dt_part$,17%,3%)
               return
                                                       /* (EWD018)     */
L60950:     gosub std_wd_ht                        /* WD$ - Width Prt  */
                                                   /* HT$ - Height Prt */
            a1 = 0.0 : a2 = 0.0
            convert str(dt_part$,13%,3%) to a1, data goto L61005
L61005:
            convert str(dt_part$,16%,1%) to a2, data goto L61011
L61011:
            s_width = a1 + (a2/8.0)                /* Decimal Width    */
            a1 = 0.0 : a2 = 0.0
            convert str(dt_part$,17%,2%) to a1, data goto L61023
L61023:
            convert str(dt_part$,19%,1%) to a2, data goto L61029
L61029:
            s_height = a1 + (a2/8.0)               /* Decimal Height   */
            gosub lookup_color
            gosub lookup_glass
            gosub lookup_grid
            gosub lookup_hinge
            gosub lookup_intercept      /* (AWD112) assign intercept type*/

            gosub lookup_double
            gosub lookup_triple                           /* (CR1987) */
            gosub lookup_quad                             /* (CR1987) */
            gosub lookup_lamn
            gosub lookup_stc           /* (CR1173)
            a1 = 0.0 : a2 = 0.0
            if len(dt_part$) < 22 then return
                                          /* (EWD015) - 09/27/99 Begin */
               convert str(dt_part$,20%,2%) to a1, data goto L61050

               convert str(dt_part$,22%,1%) to a2, data goto L61045
L61045:
               s_clmr = a1 + (a2/8.0)
L61050:     if s_clmr <= 8.0 then s_clmr = 0.0
                                          /* (EWD015) Wood Surround */
        return                            /*          End           */

hub_calc:
            /* AWD091 */
REM        LOGMSG$ = STR(DT_PART$,1,3) & "|" & WIDTH_D$ & "|"
        sh_hub1$ = "    "
        sh_hub2$ = "    "
        sh_hub3$ = "    "
        if str(dt_part$,1,08) <> "B422B4H2" then goto no_grid
        sh_hub3$ = "    "

no_grid:
        so_ln% = 0%
        wrk_part$ = dt_part$
        bck_key$ = str(dt_rec$,24,8)
        des_mdl$ = str(dt_part$,1,3)
        so_ln% = 0%
REM        LOGMSG$ = LOGMSG$ & "$" & WRK_PART$ & "$"
REM        LOGMSG$ = LOGMSG$ & "(" & STR(DT_REC$,32,2) & ")"
        convert str(dt_rec$,32,2) to so_ln%, data goto BCK_FMT
BCK_FMT:    FMT CH(256)
        mull2$ = str(wrk_part$,23,3)
        if mull2$ = "   " then mull2$ = str(wrk_part$,20,3)
        if mull2$ < "   " then mull2$ = "   "
/* find previous line item */
REM        LOGMSG$ = LOGMSG$ & "X" & MULL$ & "/" & MULL2$ & "X"

        a1 = 0.0 : a2 = 0.0
        convert str(dt_part$,13%,3%) to a1, data goto A61005
A61005:
        convert str(dt_part$,16%,1%) to a2, data goto A61011
A61011:
        hld_width = 0.0
        hld_width = a1 + (a2/8.0)                /* Decimal Width    */
        trp_sw = 0.0
so_loop:
        so_ln% = so_ln% - 1%
        if so_ln% < 1 then goto skip_hub_calc
        convert so_ln% to str(bck_key$,17,3), pic (###)
        logmsg$ = logmsg$ & "=" & str(bck_key$,17,3)
        read #19, key = bck_key$, using BCK_FMT, bck_rec$,   ~
                                        eod goto skip_hub_calc
         if des_mdl$ = str(bck_rec$,32,3) then goto so_loop
REM      DES_MDL$ = STR(BCK_REC$,32,3)
         des_part$ = str(bck_rec$,32,25)
         hubm = 0
         convert str(des_part$,8,1) to hubm, data goto so_cont
so_cont:
         wrk_part$ = des_part$
         mull$ = str(bck_rec$,54,3)
            if mull$ = "   " then mull$ = str(bck_rec$,51,3)
            if mull$ < "   " then mull$ = "   "
        /* part = 32 for 25 */
         a1 = 0.0 : a2 = 0.0
         convert str(bck_rec$,45%,2%) to a1, data goto S61005
S61005:
         convert str(bck_rec$,47%,1%) to a2, data goto S61010
S61010:
         tmp_width = a1 + (a2/8.0)                /* Decimal Width    */
         convert tmp_width to tmp_width$, pic (##0.0)
         if mull$ <> mull2$ then goto so_loop

/*    +------------------------------------------------------+
        | grid 99 is not defined, can't calc.                  +
        | if mull D05/C03/D17 it is a triple and needs to go   +
        | back 2 parts instead of 1.                           +
        +------------------------------------------------------+ */
         if str(des_part$,7,2) = "99" then goto skip_hub_calc
         if mull$ = "D05" or mull$ = "C03" or mull$ = "D17" then          ~
                                                               goto S61011
         goto S61012

S61011:
         trp_sw = trp_sw + 1.0
         if trp_sw < 2.0 then goto so_loop

S61012:
REM         LOGMSG$ = LOGMSG$ & ">>" & DES_PART$
REM         LOGMSG$ = LOGMSG$ & "<<" & STR(DT_PART$,13,4)
REM         IF TMP_WIDTH < (HLD_WIDTH / 3.1) THEN GOTO SO_LOOP

         des_mdl$ = str(bck_rec$,32,3)  /* ??? */
/* find previous line item */

         readkey$ = "SHPHFOFF                      "
         str(readkey$,10%,3%) = str(dt_part$,1,3)
REM         str(readkey$,10%,3%) = str(des_part$,1,3)
REM      hub_width = 0.0000
         hub1 = 0.0000
         hub2 = 0.0000
         hub3 = 0.0000

REM        LOGMSG$ = LOGMSG$ & STR(DES_PART$,1,3) & "|"
        read #3, key = readkey$, using L61013, hub_width$,         ~
                                        eod goto skip_hub_calc


L61013:     FMT POS(22), CH(08)
        init(" ") key$
        str(key$,1,9)  = "MULLCODES"
        str(key$,10,3) = mull$
REM        LOGMSG$ = LOGMSG$ & "=" & MULL$
        read #3,key = key$, using L61014, type$,            ~
                                        eod goto skip_hub_calc

         v = 0
         for l = 1 to 30
         if str(type$,l,1) = " " then v=1
            if v=1 then str(type$,l,1) = " "
         next l

        init(" ") key$, desc$, lite$
        str(key$,1,9)  = "LITING   "
REM      STR(KEY$,10,3) = STR(WRK_PART$,7,2)
REM      HLD_LITE$ = STR(WRK_PART$,7,2)
REM      STR(KEY$,10,3) = STR(BCK_REC$,38,2)
        str(key$,10,3) = str(dt_part$,7,2)
        hld_lite$ = str(bck_rec$,38,2)
REM        LOGMSG$ = LOGMSG$ & "+" & HLD_LITE$
        read #3,key = key$, using L61014, desc$,            ~
                                        eod goto skip_hub_calc
             tmp_desc$ = desc$
         lite$ = "  "
         v = 0
         for l = 1 to 30
         if str(desc$,l,1) = "V" then v = l
         if v <> 0 then l = 31
         next l
         if v <> 0 then lite$ = str(desc$,v-1,2)
REM      IF STR(DESC$,1,7) = "SPECIAL" THEN LITE$ = "SPECIAL"

        hubs = 0
        str(key$,1,9)  = "CUSTLITES"
REM          STR(KEY$,10,3) = STR(WRK_PART$,7,2)
REM          LOGMSG$ = LOGMSG$ & "+" & STR(WRK_PART$,7,2)
        str(key$,10,3) = str(dt_part$,7,2)
REM        LOGMSG$ = LOGMSG$ & "+" & STR(DT_PART$,7,2)
        read #3,key = key$, using L61014, desc$,            ~
                                        eod goto L61014
        hubs = 0
REM          CONVERT STR(DESC$,30%,1%) TO HUBS, DATA GOTO SKIP_HUB_CALC
        convert str(desc$,30%,1%) to hubs, data goto skip_conv

REM          IF HUBS = 0 THEN GOTO SKIP_HUB_CALC

skip_conv:
         /* as per Chad, use parent grid to determin formula key */
REM          CONVERT HUBS TO STR(LITE$,3,1), PIC (0)
        convert hubm to str(lite$,3,1), pic (0) /* use parent hub */
REM        LOGMSG$ = LOGMSG$ & "+" & STR(DESC$,30,1)
REM        LOGMSG$ = LOGMSG$ & "/" & STR(DES_PART$,8,1)

        str(lite$,4,1) = "H"
/* if TRIPLE & hinge contains 1/3, then 3 */
        if str(type$,1,6) <> "TRIPLE" then goto L61014
REM          STR(LITE$,3,1) = "3"
        if str(lite$,3,1) <> "1" and str(lite$,3,1) <> "2" then  ~
                                             str(lite$,3,1) = "3"
        str(key$,1,9)  = "HINGE    "
        str(key$,10,3) = str(wrk_part$,7,2)
REM MMM      STR(KEY$,10,3) = STR(DT_PART$,7,2)
REM         LOGMSG$ = LOGMSG$ & "*" & KEY$
        read #3,key = key$, using L61014, desc$,            ~
                                        eod goto L61014
REM          HUBS = 3
        for l = 1 to 30
REM     IF STR(DESC$,L,3) = "1/3" THEN GOTO L61014
           if str(desc$,l,3) = "1/3" then hubs = 3
        next l
REM          GOTO SKIP_HUB_CALC

/*-------------- lite$ = "2v3h" format, vert & hub */
L61014:     FMT POS(025), CH(30)
/* build key ---------------*/
/* 1-24,*/
REM test_skip
        if hubs = 0 then goto skip_hub_calc
        init(" ") hub_key$
REM     STR(HUB_KEY$,1,4) = STR(WRK_PART$,1,3)
        str(hub_key$,1,4)  = des_mdl$
        str(hub_key$,5,15) = type$
        str(hub_key$,20,10)  = lite$

/************************************************************************/
REM convert str(hub_key$,1,3) to mdl%, data goto skip_hub_calc
        mdl$ = str(hub_key$,1,3)
        if mdl$ > "520" and mdl$ < "529" then                       ~
                                       tmp_width = (tmp_width - 0.5) / 2.0
        if mdl$ > "530" and mdl$ < "539" then                       ~
                                       tmp_width = (tmp_width - 1.0) / 3.0
        convert tmp_width to tmp_width$, pic (##0.000)
        logmsg$ = logmsg$ & "//" & hub_width$ & "/" & tmp_width$
        hub_width$ = tmp_width$  /* parent width  */
        convert hubm to hubm$, pic (#0)
REM       LOGMSG$ = LOGMSG$ & "==" & HUBM$ & "=="
REM skip_0
/* add hubm to call list */
        call "APCPLZ45" (hub_key$, hub_width$, hub1, hub2, hub3, #42, ~
                 hubm,err%)
/* as per Chad, use min for determining # hubs to calc */
        convert hubs to hubs$, pic (#0)
        convert hubm to hubm$, pic (#0)

REM       LOGMSG$ = LOGMSG$ & HUBS$ & "/" & HUBM$
REM IF HUBM < HUBS AND HUBM > 0.0 THEN HUBS = HUBM

        if hubs < hubm and hubs > 0.0 then hubm = hubs
        if hubs > 2 then goto skip_3
        if hubs > 1 then goto skip_2
        hub2 = 0.00
skip_2:
        hub3 = 0.00
skip_3:

REM CALL "LOGFILE" (LOGMSG$)
REM LOGMSG$ = "=== "  & DES_MDL$ & "/" & MODEL$
REM LOGMSG$ = LOGMSG$ & "|" & HUB_KEY$ & "|" & HUB_WIDTH$ & "=" & WIDTH_D$
REM CALL "LOGFILE" (LOGMSG$)
REM INIT(" ") LOGMSG$
REM STR(LOGMSG$,1,24) = HUB_KEY$
REM STR(LOGMSG$,25,3) = " | "
REM STR(LOGMSG$,28,8) = HUB_WIDTH$
REM STR(LOGMSG$,36,3) = " = "

         convert hub1  to str(logmsg$,39,10), pic (0000.000-)
         convert hub2  to str(logmsg$,49,10), pic (0000.000-)
         convert hub3  to str(logmsg$,59,10), pic (0000.000-)

REM         HUBCL_KEY$ = DT_BAR$
REM        STR(LOGMSG$,69,12) = STR(HUBCL_KEY$,1,12)
REM        STR(LOGMSG$,82,01) = "/"
REM        STR(LOGMSG$,83,25) = DT_PART$
REM        STR(LOGMSG$,109,01) = "/"
REM        STR(LOGMSG$,110,05) = TMP_WIDTH$

           if hub1 = 0.0 then goto skip_hub_calc
            sh_hub1$ = "NONE"
            sh_hub2$ = "NONE"
            sh_hub3$ = "NONE"
           if hub1 > 0 then                          ~
            convert hub1  to sh_hub1$, pic (0000.000-)
           if hub2 > 0 then                          ~
            convert hub2  to sh_hub2$, pic (0000.000-)
           if hub3 > 0 then                          ~
           convert hub3  to sh_hub3$, pic (0000.000-)
REM        LOGMSG$ = LOGMSG$ & "-" & SH_HUB1$
REM        LOGMSG$ = LOGMSG$ & "-" & SH_HUB2$
REM        LOGMSG$ = LOGMSG$ & "-" & SH_HUB3$
REM        call "LOGFILE" (logmsg$)
skip_hub_calc:

        gosub overide_hub
REM        LOGMSG$ = DT_PART$ & " >>> "
REM        LOGMSG$ = LOGMSG$ & "-" & SH_HUB1$
REM        LOGMSG$ = LOGMSG$ & "-" & SH_HUB2$
REM        LOGMSG$ = LOGMSG$ & "-" & SH_HUB3$
REM            call "LOGFILE" (logmsg$)
REM     HUBCL_KEY$ = RM_BAR$

        hubcl_key$ = str(dt_bar$,1,10)
        gosub write_new_file
        sh_hub$ = sh_hub1$
     return
/*
skip_hub_calc:
        gosub overide_hub
REM         LOGMSG$ = LOGMSG$ & "@@@@@"
REM        CALL "LOGFILE" (LOGMSG$)
    return
*/
overide_hub:
REM          HLD_LITE$ = STR(WRK_PART$,7,2)

             hld_lite$ = str(dt_part$,7,2)
          if hld_lite$ <> "A0" and hld_lite$ <> "C0" and          ~
             hld_lite$ <> "D0" and hld_lite$ <> "E0" and          ~
             hld_lite$ <> "F0" and hld_lite$ <> "H0" and          ~
             hld_lite$ <> "I0" and hld_lite$ <> "J0" and          ~
             hld_lite$ <> "K0" and hld_lite$ <> "L0" and          ~
             hld_lite$ <> "M0" and hld_lite$ <> "N0" and          ~
             hld_lite$ <> "P0" then goto skip_none
            sh_hub1$ = "NONE"
            sh_hub2$ = "NONE"
            sh_hub3$ = "NONE"
            goto hard_coded

skip_none:   /* apply "EQUAL" */
          if hld_lite$ <> "E1" and hld_lite$ <> "E2" and          ~
             hld_lite$ <> "I1" and hld_lite$ <> "I2"              ~
             then goto skip_equal
            sh_hub1$ = "EQUAL"
            sh_hub2$ = "EQUAL"
            sh_hub3$ = "EQUAL"
            goto hard_coded

skip_equal:  /* apply "STAR" */
          if hld_lite$ <> "H1" and hld_lite$ <> "HE" and          ~
             hld_lite$ <> "C1" and hld_lite$ <> "C4" and          ~
             hld_lite$ <> "D6" and hld_lite$ <> "DH"              ~
             then goto hard_coded
            sh_hub1$ = "STAR"
            sh_hub2$ = "STAR"
            sh_hub3$ = "STAR"
hard_coded: return

write_new_file
REM     LOGMSG$ = ">>   " & HUB_KEY$ & "|" & HUBCL_KEY$ & ", " & SH_HUB1$
REM      call "LOGFILE" (logmsg$)

            read #43,hold,key = hubcl_key$, eod goto L63390
               delete #43
L63390:     write #43,using hubcalfl, hubcl_key$,sh_hub1$,sh_hub2$,      ~
                                      sh_hub3$," "
hubcalfl:   FMT CH(12), 3*CH(10), CH(20)
      return



        lookup_reason
            init(" ") readkey$, rm_reason_d$
            str(readkey$,1%,9%)   = "PLAN REMK"
            str(readkey$,10%,15%) = rm_reason$
            read #3,key = readkey$, using L61107, rm_reason_d$,           ~
                                                           eod goto L61110
L61107:        FMT POS(25), CH(30)
L61110: return

        check_patio                              /* Note - GLASS01   */
           gosub case_glass
                                                 /* One Top, One Bot */
                                                 /*    (EWD044)      */
            check_patio_mod% = 0%    /*UsedToDetermineIfOneOfTestedModels*/
            if model$ <> "312" and model$ <> "332" then goto L61115
               check_patio_mod% = 1%
               if hg$ <> "42" then goto L61115
                  t1% = 1% : b1% = 2%
                                                 /* Two Tops, One Bot*/
                                                 /*    (EWD044)      */
                                                 /* (AWD104)         */
L61115:     if model$ <> "313" then goto L61120
               check_patio_mod% = 1%
               t1% = 1% : b1% = 1%
                                                 /* Two Tops, Two Bots*/
                                                 /*    (EWD044)      */
L61120:     if model$ <> "314" and model$ <> "334" then goto L61125
               check_patio_mod% = 1%
               t1% = 2% : b1% = 0%

L61125:     if model$ <> "333" then goto L61135
               check_patio_mod% = 1%
               t1% = 2% : b1% = 1%

L61135:     if model$ <> "378" and model$ <> "388" then goto L61138
               check_patio_mod% = 1%
               if hg$ <> "42" then goto L61138
                  t1% = 1% : b1% = 2%
L61138:

            if check_patio_mod% = 0% then return
REM if one of tested models and tso or bso set quantity to 1
               if sc$ = "4" then t1% = 1%
               if sc$ = "5" then b1% = 1%
        return

        lookup_color                                  /* Look Up Color */
            init(" ") descr$, readkey$, cl_l$, cl_s$
            str(readkey$,1%,9%)   = "COLOR    "
            str(readkey$,10%,15%) = cl$
            read #3,key = readkey$,using L61131, descr$,eod goto L61140
L61131:        FMT POS(25), CH(30)
            cl_l$ = str(descr$,6%,6%)                 /* Long Descript */
            cl_s$ = str(descr$,1%,2%)                 /* Short Descript*/
L61140: return
        lookup_grdcolor                               /* Look Up Color */
            init(" ") descr$, readkey$, cl_l$, cl_s$
            str(readkey$,1%,9%)   = "GRDCOLOR"
            str(readkey$,10%,15%) = str(lab_rec$(),193%,1%)
            read #3,key = readkey$,using L61131, descr$,               ~
                                                    eod goto grdcolor_done

            p% = 0%
            p% = pos(descr$ = "-")
            cl_l$ = str(descr$,(p%+2%),6%)            /* Long Descript */
            cl_s$ = str(descr$,1%,2%)                 /* Short Descript*/
        grdcolor_done
        return

        lookup_glass                                 /* Look Up GLASS  */
            init(" ") readkey$, descr$, ty_s$, ty_l$
            ty% = 0%
            str(readkey$,1%,9%)   = "GLASS    "
            str(readkey$,10%,15%) = ty$
            read #3,key = readkey$, using L61131, descr$, eod goto L61179
            p% = pos(descr$ = "-")
            if p% = 0% then p% = 4%
            ty_l$ = descr$                            /* Long Descript */
            ty_s$ = str(descr$,1%,p%-2%)              /* Short Descript*/
            convert ty$ to ty%, data goto L61179

L61179: return

        lookup_grid                                   /* Look Up Grid  */
            lt% = 0%
            init(" ") descr$, readkey$, l_lt$, r_lt$
            str(readkey$,1%,9%)   = "LITING   "
            str(readkey$,10%,15%) = lt$
            read #3,key = readkey$,using L61131, descr$,eod goto L61221
            p% = pos(descr$ = "-")
            if p% = 0% then p% = 4%
            l_lt$ = str(descr$,1%,p%-2%) & "   "  /* LEFT MUTTIN - TOP */
            r_lt$ = str(descr$,p%+2%,6%) & "   "  /* RIGHT MUTTIN - BOT*/
            convert lt$ to lt%,data goto L61221

        return
L61221:
REM NO IDEA WHY THIS WOULD BE SET TO 82%, GRID CODE 82% IS 6HX1V - 6HX1V
REM LT%    = 82%                          /* (EWD040) TURN ON  */
                                                  /*   ALPHA'S         */
REM            IF LT$ = "A0" THEN LT% = 101%         /* (EWD009)       */
        RETURN

        lookup_hinge                                  /* Look Up Hinge */
            init(" ") hgl$, hgr$, descr$, readkey$
            str(readkey$,1%,9%)   = "HINGE    "
            str(readkey$,10%,15%) = hg$
            read #3,key = readkey$,using L61131, descr$,eod goto L61263
            p% = pos(descr$ = "-")
            if p% = 0% then p% = 4%
            hgl$ = str(descr$,1%,p% - 1%)    /* Left Side Description  */
            hgr$ = str(descr$,p% + 2%)       /* Right Side Description */
/* (AWD093) */
            if hg$ >= "30" and hg$ <= "39" then gosub get_hgl_cde
            if hg$ >= "A6" and hg$ <= "B6" then gosub get_hgl_cde
               return
/* (AWD093/) */
            if model$ <> "830" and model$ <> "883" and model$ <> "861"   ~
                                                       then return
               p1% = pos(descr$ = "/")
               if p1% = 0% then return
               hgl$ = str(descr$,p1%-1%,3%)
L61263: return
/* (AWD093) */
        get_hgl_cde
            if model$ = "830" or model$ = "883"  then get_hgl_desc
            if model$ = "861" then get_hgl_desc

            if model$ = "843" or model$ = "833"  then get_hgl_desc
            if model$ = "818" or model$ = "808"  then get_hgl_desc

            if model$ = "D43" or model$ = "D33"  then get_hgl_desc
            if model$ = "D18" or model$ = "D08"  then get_hgl_desc

            if model$ = "JP3" or model$ = "JT3"  then get_hgl_desc
             return
        get_hgl_desc
          p1% = pos(descr$ = "/")
          if p1% = 0% then return
          hgl$ = str(descr$,p1%-1%,3%)
        return
/* (AWD093/) */


        lookup_temp
            door%,stktmp% = 0%           /*(AWD111) */
            patio$ = "0"         /*(AWD111) */
            topTemp%, botTemp% = 0%
            init(" ") readkey$, temp$, temp1$
            str(readkey$,1%,9%) = "PLAN TEMP"
            str(readkey$,10%,15%) = ty$
            read #3,key = readkey$, eod goto L61284

               temp$ = "*"
                                      /* (EWD055) */
               temp1$ = "*"
               if ty$ = "89" then temp1$ = " "

               gosub isPatio
               if door% = 1% then gosub check_patio_temp
               gosub isCustom             
/* (AWD111\)*/
/* NC & NTX Shapes */
               if dt_dept$ = "104" then temp1$ = " "
               if dt_dept$ = "043" and schema% = 1% then temp1$ = " "
               if (dt_dept$ = "002" or dt_dept$ = "003") ~
                    and schema% = 2% then temp1$ = " "

                                      /* (EWD055) */
/* (AWD115) */
               gosub checkTempGed
/* (\AWD115) */
L61284: return
        check_patio_temp                   /* (EWD055) */
/*(AWD111) */
             temp$  = "*"
             temp1$ = "*"
/* (AWD111\) */
          return

/*(AWD111)*/
        isPatio
           door% = 0%
           init(" ") readkey$
           str(readkey$,1,9)  = "PLAN DOOR"
           str(readkey$,10,3) = str(dt_part$,1%,3%)
           read #3, key = readkey$, eod goto notDoor
                door% = 1%
                patio$ = "1"
REM                GOSUB ISCUSTOM             /* (CR2773) */
        notDoor
        return
        isCustom
            stockPatio% = 0%
            stktmp% = 0%
            init(" ") readkey$, desc$
            str(readkey$, 1%,9%)  = "TEMPSTOCK"
            str(readkey$,10%,3%) = str(dt_part$,1%,3%)
            str(readkey$,13%,2%) = str(dt_part$,5%,2%)
            str(readkey$,15%,4%) = str(dt_part$,13%,4%)
            str(readkey$,19%,3%) = str(dt_part$,17%,3%)

            read #3,key = readkey$, using L52330, desc$,eod goto not_stock

               if door% = 0% then goto notTempDoor          /* (CR2773) */
               if door% = 0% then goto notTempDoor
                 patio$ = "2"
                 stockPatio% = 1%
        return
        notTempDoor
          stktmp% = 1%                   /* (CR2773) */
        return
/*(AWD111\) */

        lookup_double
            gls_double% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%) = "PLAN DBLE"
            str(readkey$,10%,15%) = ty$
            read #3,key = readkey$, eod goto L61308
               gls_double% = 1%
L61308: return

        lookup_triple
            gls_trip% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%) = "PLAN TRIP"
            str(readkey$,10%,15%) = ty$
            read #3,key = readkey$, eod goto L61310
               gls_trip% = 1%
L61310: return

        lookup_quad
            gls_quad% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%) = "PLAN QUAD"
            str(readkey$,10%,15%) = ty$
            read #3,key = readkey$, eod goto L61312
               gls_quad% = 1%
L61312: return

        lookup_lamn
            gls_lamn% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%) = "PLAN LAMN"
            str(readkey$,10%,15%) = ty$
            read #3,key = readkey$, eod goto L61315

               gls_lamn% = 1%
               gls_double% = 1%
L61315: return

        check_cont_head
            conthead% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%) = "PLAN CONT"
            str(readkey$,10%,15%) = model$
            read #3,key = readkey$, eod goto not_cont
               conthead% = 1
REM  GOSUB CASE_GLASS               /* (CR838) */
        not_cont
        return

        lookup_stc                 /* (CR1173) */
            stc% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%) = "PLAN STC "
            str(readkey$,10%,15%) = ty$
            read #3,key = readkey$, eod goto notSTC
               stc% = 1%
               gls_double% = 1%
        notSTC
        return

        std_wd_ht /* Convert Standard Width/Height to Fraction in 8'ths*/
                  /* F0% = Width Fraction, F1% = Height Fraction       */
                  /* WD$ = Width & Fraction, HT$ = Height & Fraction   */
           str(wd$,1%,3%) = str(dt_part$,13%,3%)         /* Width  (3) */
           if str(wd$,1%,1%) = "0" then str(wd$,1%,1%) = " "
           str(ht$,1%,2%) = str(dt_part$,17%,2%)         /* Height (2) */
           f0% = 0% : f1% = 0%                      /* Build Fractions */
           convert str(dt_part$,16%,1%) to f0%,data goto L61350 /*Width */

           convert str(dt_part$,19%,1%) to f1%,data goto L61350 /*Height*/

           goto L61353
L61350:      f0% = 8% : f1% = 8%
L61353:    if f0% = 0% then f0% = 9%
           if f1% = 0% then f1% = 9%
           str(wd$,4%,1%) = " "          /* Build Width with Fraction  */
           str(wd$,5%,3%) = str(sze$,(f0%*3%) - 2%, 3%)
           str(ht$,3%,1%) = " "          /* Build Height with Fraction */
           str(ht$,4%,3%) = str(sze$,(f1%*3%) - 2%, 3%)
        return

        calc_wd_ht /* Convert Calculated Width/Height With Fract. 16'th*/
                   /* WD1$ = Width & Fract(9), HT1$ = Height & Fract(8)*/
           if lit_flg$ = "N" then goto L61370
              wd1$ = str(dt_part$,13%,4%)
              ht1$ = str(dt_part$,17%,3%)
              return

L61370:    calc = width                          /* Convert Width  (3) */
           gosub convert_sixteen : wd1$ = "         "
           convert a% to str(wd1$,1%,3%), pic(###)

           if b% = 0% then goto L61401            /* Check For Fraction */
              str(wd1$,5%,5%) = str(sz$,(b%*5%) - 4%, 5%)
L61401:    calc = height                         /* Convert Height (2) */
                                                 /* (EWD058) Brickmold  */
            if model$ = "553" then height = height + .2500
            if model$ = "554" then height = height + .2500
                                                 /* (EWD058)           */


           gosub convert_sixteen : ht1$ = "        "
           convert a% to str(ht1$,1%,2%), pic(##)

           if b% = 0% then goto L61419            /* Check For Fraction */
              str(ht1$,4%,5%) = str(sz$,(b%*5%) - 4%, 5%)
L61419:    init(" ") width_d$, height_d$
           x = round(width,4)
           convert width to width_d$, pic(00#.####)
           x = round(height,4)
           convert height to height_d$, pic(00#.####)
        return

        calc_wd   /* Convert Center Line Meeting Rail to Long Form     */
           init(" ") wd2$                            /* CLMR For Glass */
           if s_clmr < 1 then return
           calc = s_clmr                             /* Decimal CLMR   */
           gosub convert_sixteen
           convert a% to str(wd2$,1%,3%),pic(###)    /* Size (3) CLMR  */

           if b% = 0% then goto L61464                /* Check Fraction */
              str(wd2$,5%,5%) = str(sz$,(b%*5%) - 4%, 5%)
L61464: return

        convert_sixteen
            calc = round( calc, 4 )
            a% = int(calc)
            b% = int((calc - a%) * 10000)
            if b% = 0% then goto L61500                 /****************/
               d% = int(b%/625)                        /* Conversion of*/
               if mod(b%,625) <> 0 then d% = d% + 1%   /* Decimals to  */
                  b% = d%                              /*  Sixteen's   */
                  if b% <> 16% then goto L61500         /****************/
                     a% = a% + 1% : b% = 0%         /* A% = WHOLE PART */
L61500: return

        lookup_phantom                       /* Special Code for Model */
           err% = 0%
           sqFeet = 0.00
           if view$ <> "TOP" then goto L61533 /* 830, 883              */
              phantom$ = "2008"
              if str(hgl$,1%,2%) = "OR"  then phantom$ = "2203"
              if str(hgl$,1%,2%) = "CO"  then phantom$ = "2103"
              if str(hgl$,1%,3%) = "1/4" then phantom$ = "2008"
              if str(hgl$,1%,3%) = "1/3" then phantom$ = "2108"
              goto L61548
L61533:    phantom$ = "3008"
           if str(hgl$,1%,2%) = "OR"  then phantom$ = "3203"
           if str(hgl$,1%,2%) = "CO"  then phantom$ = "3103"
           if str(hgl$,1%,3%) = "1/4" then phantom$ = "3008"
           if str(hgl$,1%,3%) = "1/3" then phantom$ = "3108"
L61548: REM                       /* CAL% 1%=W/H, 2%=W Only, 3%=H Only */
            call "APCCALSB" (cal%,                 /* Calc Type 1%,2%,3*/~
                             dt_part$,             /* Part Number      */~
                             dim1es,               /* (AWD122)         */~
                             dim2es,               /* (AWD122)         */~
                             dim3es,               /* (AWD122)         */~
                             phantom$,             /* Phantom Designato*/~
                             width,                /* Exact width      */~
                             height,               /* Exact Height     */~
                             #1,                   /* AMTBOMCD Equation*/~
                             err% )                /* Error Code 0%-Ok */

/*(AWD111) */
            gosub calcSqFeet
            if err% <> 0% then goto L61584
            if str(hgl$,1%,2%) = "CO" or str(hgl$,1%,2%) = "OR" then     ~
                                            gosub calc_clmr_1

/* SR64679 */
REM GOSUB CHECKINTERCEPT                               /* (CR2055) */
        return
L61584:     err% = 1%                              /* Equation Error   */
        return

        calcSqFeet
            sqFeet = 0.00
            widthRnd%, heightRnd% = 0%
            widthDec, heightDec, widthRndDec, heightRndDec = 0.00
            widthDec     = width
            heightDec    = height
            widthRndDec  = widthDec - int(widthDec)
            heightRndDec = heightDec - int(heightDec)
            if widthRndDec > 0.00 then widthRnd%  = int(widthDec) + 1 ~
                 else  widthRnd% = int(widthDec)
            if heightRndDec > 0.00 then heightRnd% = int(heightDec) + 1 ~
                 else heightRnd% = int(heightDec)

            sqFeet = round((widthRnd%*heightRnd%) / 144.0,4)

        return
/* SR64679 + */
        checkIntercept
          if schema% <> 2% then return         /* Only TX                */
          if gls_lamn% = 1% then goto DuraSeal /* Note TX only uses      */
          if width > 50 and height > 50 then goto DuraSeal  /*DuraSeal   */
          if width > 73 or  height > 73 then goto DuraSeal  /* but TX    */
        return                                 /* know it as spacer 03   */
        DuraSeal
          intercept% = 3%
          convert intercept% to intercept$,pic(00)
        return

/* SR64679 - */
        check_for_shape                            /*  (EWD056) - Begin */
            process% = 1%                         /* Process from Part No*/

            gosub initialize_shapes

            sh_model$ = str(dt_part$,1%,3%)        /* Set Model Code     */
            sh_lt$    = str(dt_part$,7%,2%)        /* Set Liting code    */

            gosub lookup_shape                     /* Verify Shape Model */
            if check% = 0% then goto not_a_shape

            init(" ") shape_cross$
            str(shape_cross$,1%,1%) = str(dt_part$,7%,1%)
                                                   /* 1st Digit Liting   */
            str(shape_cross$,2%,1%) = str(dt_part$,10%,1%)
                                                   /* 2nd Digit Hinge    */
                                                   /* Special Check for  */
                                                   /* Octagon/Full circle*/


            gosub lookup_cross                     /* Get Shape Code     */
            if check% = 0% then goto not_a_shape


            gosub verify_model_shape               /* Verify Model       */
            if check% = 0% then goto not_a_shape


            gosub load_shape_prompts               /* Get Shape Prompts  */
            if check% = 0% then goto not_a_shape

            sh_glass$ = str(dt_part$,5%,2%)

            a27$      = str(dt_part$,11%,1%)       /* Screen Code        */

            gosub lookup_shape_glass               /* Get Sandwich       */
            if check% = 0% then goto not_a_shape

                                                   /* Glass Facing Code  */
            str(sh_face$,1%,2%) = sh_config$       /* Shape Config Code  */
            str(sh_face$,3%,2%) = sh_glass$        /* Glass Code         */

            sh_qty$ = "   1"                       /* Shape Quantity     */
            sh_qty% = 1%

            nbr_line% = 0%
            init(" ") txt$, text$()
            call "APCPLTXT" (#9, dt_txt$, text$(), nbr_line%)
            txt$     = str(text$(2%),1%,40%)       /* Obtain Glass Text  */
                                                   /* From Sales Order   */
                                                   /* Need sh() and sh$()*/
            gosub convert_shape      /* Convert width, height, dim1, dim2*/
                                     /* into shd(1%) - shd(4%)           */

            for kk% = 1% to 6%
                sh(kk%) = 0.0
                if str(sh_entry$,kk%,1%) = "N" then goto Next_kk
                   for ll% = 1% to 4%
                       if str(sh_entry$,kk%,1%) <> str(sh_codes$,ll%,1%) ~
                                                          then goto Next_ll
                          sh(kk%) = shd(ll%)
                          goto Next_kk

Next_ll:           next ll%

Next_kk:           convert sh(kk%) to sh$(kk%), pic(###.####-)

                   if sh(kk%) < 1.0 then sh$(kk%) = "         "

            next kk%


            gosub calculate_special_shape          /* Calculate Glass    */

            if calsl% = 1% then return


            gosub update_work_shape


        not_a_shape

        return                                 /*  (EWD056) - End      */

        calc_clmr_1                            /* Both - Cottage/Oriel */
          if s_clmr < 1.0 then return
            init(" ") readkey$, descr$
            str(readkey$,1%,9%)   = "GLASS10  "
            str(readkey$,10%,15%) = model$
            read #3,key = readkey$, using L61600, descr$,                ~
                                                          eod goto L61620
L61600:        FMT POS(25), CH(30)
            t_clmr = 0.0 : b_clmr = 0.0 : tb_clmr = 0.0
            convert str(descr$,1%,8%) to t_clmr, data goto L61625

            convert str(descr$,12%,8%) to b_clmr, data goto L61625

            convert str(descr$,22%,8%) to tb_clmr, data goto L61615
L61615
            if view$ = "TOP" then                           /* GLASS   */~
               height =((s_height/2.0) - t_clmr)                         ~
                                  - (((s_height/2.0) + tb_clmr) - s_clmr)
            if view$ = "BOT" then                           /* GLASS   */~
                height =((s_height/2.0) - b_clmr)                        ~
                                  + (((s_height/2.0) + tb_clmr) - s_clmr)
L61620: return
L61625:      err% = 3%
        return

        check_stock                    /* Check Stock Clear Glass Only */
           stock% = 0%                          /* (AWD075)  */
           if ged_bilco$ =  "0" then return    /* (AWD076) - 03/03/05  */
           init(" ") stock$, lab_fil$

           if ty$ <> "01" then return
           for i% = 1% to stk_max%
               if ws$(i%) = wd1$ and hs$(i%) = ht1$ and md$(i%) = model$ ~
                                                        then goto L61800
           next i%

        return
L61800:    stock$, str(lab_fil$,1%,5%) = "STOCK"
           if ty$ = "01" then sandwich$ = "IG3CLS3CLS"

                                       /* (EWD021) - End               */
           stock% = 1%                 /* (AWD075)                     */
        return

        create_glass                           /* (EWD014)             */
            if scr_sel% = 0% then return
               if ged_bilco$ <> "0" and ged_bilco$ <> "1" then goto L61896
               pass% = 0%                             /* (AWD074)  */
           create_precut                              /* (AWD086)  */

               call "APCPLB45" (size%,    /* Specified Batch Size      */~
                               scr_sel$,  /* Screen Selection (EWD006) */~
                               glass_dte$,/* Glass Production Date     */~
                               scr_dte$,  /* Planned Production Date   */~
                               scr_dte1$, /* Planned Production Unforma*/~
                               file$,     /* Name of Optimized File    */~
                               bat$,      /* Number of Batches Created */~
                               rm_flag%,  /* Remake Flag               */~
                               ss$(),     /* Sort Code        (EWD064) */~
                               ss_temp$(),/* (AWD111) Tempered Sort    */~
                               pass%,     /* NumberOfTimesCalled(AWD074)*/~
                               ds_batch$, /* DS BATCH         (AWD081) */~
                               tempScrn%, /* (AWD111) Tempered or Not  */~
                               lamnScrn%, /* (AWD123) Laminate or Not  */~
                               schema%,   /* Schema (CR1987)           */~
                               #3,        /* (GENCODES) TABLES         */~
                               #6,        /* (APCPLNWK) Label Detail Fl*/~
/*(EWD055)*/                   #8)        /* (APCPLNGR) Remake Glass   */

REM                             #10,       /* (@GLSGED@) OPTIMIZER FILE */~
REM                             #21,       /* (@RMKGDK@) BOTH (EWD006)  */~
REM                             #12,       /* (@RMKGD1@) PRODUCTION     */~
REM                             #16,       /* (@RMKGD2@) IN HOUSE       */~
REM                             #29,       /* (@GLSTMP@) TEMP GLASS     */~
REM                             #30,       /* (@RMKTMP@) TEMP RE-MAKE   */~
REM                             #33,       /* (@RMKGD5@) VINYL LINE RMK */~
REM                             #35,       /* (@GLSPRE@) PRECUT GLASS   */~
REM                             #44,       /* (@RMKFLG@)  (AWD106)      */~
REM                             #46,       /* (@GLSVLA@)  (AWD109)      */~
REM                             #48,       /* (@RMKVLA@)  (AWD109)      */~
REM                             #51,       /* (@PATIO@)           AWD111*/~
REM                             #52,       /* (@CUSPAT@)          AWD111*/~
REM                             #53,       /* (@RMKPAT@)          AWD111*/~
REM                             #54,       /* (@RMKPTC@)          AWD111*/~
REM                             #56)       /* (@GLSLAM@)  (AWD123)      */


/* (AWD103) take out two passes - only produce GLSGED file not GLSPRE  */
REM               IF PASS% = 0% AND GED_BILCO$ <>  "2" THEN PASS% = 1%
REM               IF PASS% = 1% THEN GOTO CREATE_PRECUT

               if ged_bilco$ = "1" then return
                                          /* (EWD028) Nedd to sort work*/
                                          /*  Using Table Number       */

/* (AWD111) do not need patio in cut file outside purchase */
               if tempScrn% = 1% and scr_sel% = 5% then return
               if tempScrn% = 1% and scr_sel% = 6% then return
               if tempScrn% = 1% then return
/*(AWD111\)*/
                                          /* ( Bilco Glass Cutter )    */
                                          /* (EWD029) New File         */
L61896:

              call "APCPLC45" (size%,    /* Specified Batch Size      */~
                               scr_sel$,  /* Screen Selection (EWD006) */~
                               glass_dte$,/* Glass Production Date     */~
                               scr_dte$,  /* Planned Production Date   */~
                               scr_dte1$, /* Planned Production Unforma*/~
                               file$,     /* Name of Optimized File    */~
                               bat$,      /* Number of Batches Created */~
                               rm_flag%,  /* Remake Flag               */~
                               ss$(),     /* Sort Code        (AWD079) */~
                               ds_batch$, /* DS BATCH         (AWD081) */~
                               reschedBatch$, /* Reschedule Batch      */~
                               tempScrn%, /* (AWD111) Tempered or Not  */~
                               lamnScrn%, /* (AWD123) Laminate or Not  */~
                               #6,        /* (APCPLNWK) Label Detail Fl*/~
                               #8,        /* (APCPLNGR) Remake Glass   */~
                               #3)        /* (GENCODES) TABLES         */


REM                             #11,       /* (@GLSBLA@) OPTIMIZER FILE */~
REM                             #22,       /* (@RMKBLK@) BOTH (EWD006)  */~
REM                             #13,       /* (@RMKBL1@) PRODUCTION     */~
REM                             #17,       /* (@RMKBL2@) IN HOUSE       */~
REM                             #34,       /* (@RMKBL5@) VINYL LINE RMK */~
REM                             SCR_NBR%,  /* SCREEN NUMBER             */~
REM                             #36,       /* (@RMKBLT@) TEMPERED GLASS */~
REM                             #37,       /* (@GLSBLT@) TEMPERED GLASS */~
REM                             #45,       /* (@RMKFLB@)   AWD106       */~
REM                             #47,       /* (@GLSVLB@)   AWD109       */~
REM                             #49  )     /* (@RMKULB@)   AWD109       */
REM                                     /* (AWD075)   ADDED FILE CHANNEL */
REM                                         /* (EWD001) - MOD 06/04/98   */

               if scr_sel$ <> "2" then return /* Only Call for Production*/

               rh% = len(file$)
               str(file$,rh%+1%,2%) = "-B"/* Set for (B) File          */

                                          /* ( Bilco Glass Cutter )    */
                                          /* (EWD029) New (B) File     */
               call "APCPLE45" (size%,    /* Specified Batch Size      */~
                               scr_sel$,  /* Screen Selection (EWD006) */~
                               glass_dte$,/* Glass Production Date     */~
                               scr_dte$,  /* Planned Production Date   */~
                               scr_dte1$, /* Planned Production Unforma*/~
                               file$,     /* Name of Optimized File    */~
                               bat$,      /* Number of Batches Created */~
                               rm_flag%,  /* Remake Flag               */~
                               #6,        /* (APCPLNWK) Label Detail Fl*/~
                               #8,        /* (APCPLNGR) Remake Glass   */~
                               #15,       /* (@GLSBLB@) Optimizer File */~
                               #22,       /* (@RMKBLK@) Both (EWD006)  */~
                               #13,       /* (@RMKBL1@) Production     */~
                               #3,        /* (GENCODES) TABLES         */~
                               #17 )      /* (@RMKBL2@) In House       */

                                          /* (EWD001) - Mod 06/04/98   */
        return


        load_stock                                 /* (AWD076) - BEGIN */
/* (AWD103) not used anymore */
REM             IF GED_BILCO$ = "1" THEN GOSUB LOAD_STOCK_PMC
REM             IF GED_BILCO$ = "2" THEN GOSUB LAOD_STOCK_BILCO

        return

        load_stock_bilco                                  /* (AWD076) */
            init(" ") ws$(), hs$(), md$()
            readkey$ = all(hex(00)) : stk_max% = 0%
            str(readkey$,1%,9%) = "GLASS-GED"          /* (EWD026)     */
            read #3,key > readkey$, using L61965, readkey$, stk_desc$,    ~
                                                 eod goto load_done_bilco
            goto L61968
        load_next_bilco
            read #3, using L61965, readkey$,stk_desc$,                    ~
                                                   eod goto load_done_bilco
L61965:         FMT CH(24), CH(30)
L61968:     if str(readkey$,1%,9%) <> "GLASS-GED" then goto load_done_bilco
               stk_max% = stk_max% + 1%
               ws$(stk_max%) = str(stk_desc$,3%,9%)
               hs$(stk_max%) = str(stk_desc$,15%,8%)
               md$(stk_max%) = str(stk_desc$,25%,3%)   /* (EWD026)     */
                                                       /* Load Model   */
               goto load_next_bilco
        load_done_bilco
        return

        load_stock_pmc                                    /* (AWD076) */
            init(" ") ws$(), hs$(), md$()
            readkey$ = all(hex(00)) : stk_max% = 0%
            str(readkey$,1%,9%) = "GLASS GED"
            read #3,key > readkey$, using L61965, readkey$, stk_desc$,    ~
                                                 eod goto load_done_pmc
            goto L61969
        load_next_pmc
            read #3, using L61965, readkey$,stk_desc$,                    ~
                                                     eod goto load_done_pmc

L61969:     if str(readkey$,1%,9%) <> "GLASS GED" then goto load_done_pmc
               stk_max% = stk_max% + 1%
               ws$(stk_max%) = str(stk_desc$,3%,9%)
               hs$(stk_max%) = str(stk_desc$,15%,8%)
               md$(stk_max%) = str(stk_desc$,25%,3%)

               goto load_next_pmc
        load_done_pmc
        return                                        /*  (AWD076) - END */

        case_glass
          init(" ") table$
          generr%, door8ft% = 0%
          table$ = "GLASS01"
          sc$ = str(dt_part$,11%,1%)
          if door% <> 0% then gosub case_glass_door   /*(CR838)*/
          if door8ft% = 1% then goto convertQty       /*(CR838)*/

          init(" ") genkey$
          str(genkey$,1%,3%) = model$
          str(genkey$,4%,2%) = str(dt_part$,9%,2%)
          gosub genRead
            if genErr% <> 0% then goto L62031

convertQty:
            convert str(descr1$,1%,2%) to t1%, data goto L62013
L62013:
            convert str(descr1$,4%,2%) to b1%, data goto L62019
L62019:
/*(AWD120)*/
REM IF SC$ <> "4" AND SC$ <> "5" AND SC$ <> "6" THEN RETURN

            if sc$ = "4" then goto case_tso
            if sc$ = "5" then goto case_bso
            if sc$ = "6" then goto case_tso
            if sc$ = "7" then goto case_bso
        return
        case_tso
/*(\AWD120)*/
            t1% = 1%
            b1% = 0%
        return
        case_bso
            t1% = 0%
            b1% = 1%
        return
L62031:
            t1% = 1%
            b1% = 1%
        return
        case_glass_door                                 /*(CR838)+*/
           door8ft% = 0%
           init(" ") genkey$
           str(genkey$,1%,3%) = model$
           str(genkey$,4%,2%) = "**"
           str(genkey$,6%,4%) = str(dt_part$,13%,4%)
           gosub genRead
             if genErr% = 0% then door8ft% = 1%
        return                                          /*(CR838)-*/

        load_tables
           call "SHOSTAT" ("Loading Glass Tables")
           mat mod% = zer
           init(" ") mod$()
           for i% = 1% to tab_max%
               readkey$ = " "
               str(readkey$,1%,9%)  = tab$(i%)
L62058:        read #3,key > readkey$,using L62061,readkey$,eod goto L62076
L62061:           FMT CH(24)
               if tab$(i%) <> str(readkey$,1%,9%) then goto L62076
                  mod%(i%) = mod%(i%) + 1%
                  mod$(i%,mod%(i%)) = str(readkey$,10%,3%)
                  goto L62058
L62076:    next i%
        return

        check_thickness                            /* Thickness/Spacer */
           init(" ") readkey$, t_k$, s_s$, s_d$
           init(" ") s_5_32$, s_3_16$, s_lam$ /* (AWD111) */
           str(readkey$,1%,9%)   = "GED 002  "          /* (AWD099) */
           if str(sub_part$,8,1) = "1" and str(sub_part$,9,1) <> "2" ~
                      then str(readkey$,1,9) = "GED 002S"
           str(readkey$,10%,15%) = model$
           read #3,key = readkey$, using L62100, descr$, eod goto L62115
L62100:       FMT POS(25), CH(30)
           t_k$ = str(descr$,1%,6%)          /* Thickness of Spacer    */
           s_s$ = str(descr$,9%,6%)          /* Single Strength Spacer */
           s_d$ = str(descr$,17%,6%)         /* Double Strength Spacer */
           pt_k$ = ".68750"                  /* (CR1987) SS Spacer     */

/*(AWD111) */
           str(readkey$,1%,9%)   = "GED 002B "
           str(readkey$,10%,15%) = model$
           read #3,key = readkey$, using L62100, descr$, eod goto L62116
           s_5_32$ = str(descr$,9%,6%)      /* 5/32 Spacer */
           s_3_16$ = str(descr$,17%,6%)     /* 3/16 Spacer */
           s_lam$  = str(desc$,25%,6%)      /* Laminate (CR1987) */
/*(AWD111\) */
L62116: return
L62115:    t_k$="ERR-01" : s_s$="ERR-01" : s_d$="ERR-01" : err%=4%
        return

        build_ged_key                            /* NOTE - 'GLS SORT'  */
            trpThick%, doorThick% = 0%            /* (AWD0111) */
            strengthened% = 0%
            gosub calc_double                    /*  TABLE NOT USED    */
            gosub check_ged_glass
            gosub checkobsgls
            gosub check_argon
            if str(sub_part$,7,1) = "1" then gs$ = "G1" /* (CR1987)    */
            if tempScrn% = 1% then gosub checkTempGed   /* (AWD111)    */
            spacer$ = s_s$                       /* Single Strength    */
            if double% = 1% then spacer$ = s_d$  /* Double Strength    */
                                                 /* (EWD055)           */
            if temp$ = "*"  then spacer$ = s_d$  /* Double Strength    */
/* (AWD111) */
            if tempScrn% = 0% then goto no_thick_glass
            if door% = 0% and obs% = 1% and sqFeet > 25 then trpThick% = 1%
            if door% = 0% and obs% = 0% and sqFeet > 25 then doorThick% = 1%

            if str(sub_part$,14%,1%) = "5" then doorThick% = 1%
            if str(dt_part$,13%,4%) = "0946" then doorThick% = 1%
            if door% = 0% and trpThick% = 1% then spacer$ = s_3_16$
/* CR3266 */		
/* CR3272 */
     /* if door% = 1% and str(dt_part$,13%,4%) = "1074" then doorThick% = 1% */
           if door% = 0% then goto skip_3panel
		   
		   wide_3door% = 0% 
           if str(dt_part$,13%,4%) = "1397" or   ~
              str(dt_part$,13%,4%) = "1434" then wide_3door% = 1%
		   if wide_3door% = 0% then goto skip_3panel
			  
		   if str(dt_part$,1%,3%) = "313" or str(dt_part$,1%,3%) = "333" or ~
              str(dt_part$,1%,3%) = "373" or str(dt_part$,1%,3%) = "383" or ~
			  str(dt_part$,1%,3%) = "318" or str(dt_part$,1%,3%) = "323" then ~
				  doorThick% = 1%
skip_3panel:
				
            if doorThick% = 1% then spacer$ = s_5_32$

            if gls_lamn% = 0% then goto no_thick_glass
              if door% = 1% then goto checkDoorSizeLamn
               if str(dt_part$,13%,4%) > "0480" then strengthened% = 1%
               if str(dt_part$,17%,3%) > "720"  then strengthened% = 1%
               if strengthened% = 1% and sqFeet > 25 and obs% = 0% ~
                                                then spacer$ = s_5_32$
               if strengthened% = 1% and sqFeet > 25 and obs% = 1% ~
                                                then spacer$ = s_3_16$
                  goto no_thick_glass

checkDoorSizeLamn:
               if str(dt_part$,13%,4%) > "0794" then strengthened% = 1%
               if str(dt_part$,17%,3%) > "794" then strengthened% = 1%
               if strengthened% = 1% and sqFeet > 25 and obs% = 0% ~
                                                then spacer$ = s_5_32$
               if strengthened% = 1% and sqFeet > 25 and obs% = 1% ~
                                                then spacer$ = s_3_16$

                  goto no_thick_glass

/* (AWD111\) */

no_thick_glass:
            if gls_trip% = 1% then spacer$ = s_5_32$           /* (CR1987) */
            if gls_quad% = 1% then spacer$ = s_3_16$           /* (CR1987) */
            if stc% = 1% then spacer$ = s_3_16$  /* (CR1173)           */
                                                 /* Related to Liting  */
            init(" ") lab_key$, lab_ged$, skip$  /* Code Values        */
            sort% = 0%                           /* (AWD112)           */
            str(lab_key$,1%,2%) = intercept$     /* (AWD112)           */
            str(lab_key$,3%,15%) = "000000000000000"
            if lt% > 88% and lt% < 97% then skip$ = "*"

            str(lab_ged$,1%,1%)  = "0"           /* SPECIAL SORT FLAG  */
            str(lab_ged$,2%,1%)  = "0"           /* TOP/BOT FLAG       */
            str(lab_ged$,3%,6%)  = spacer$       /* Spacer Thickness   */
            str(lab_ged$,9%,5%)  = dt_seq$       /* Glass Sort Code    */
/* 0=NotPatio 1=Door 2=StockSize*/               /* (EWD055)           */
/* (AWD115) */
REM !! IF PATIO$ <> "2" AND TEMPERED% = 1% THEN                 ~
                                       STR(LAB_GED$,9%,5%)  = "00000"

        REM (Begin) Floating Sort Area - ( 14 Thru 38 )
            str(lab_ged$,14%,3%) = model$        /* Model Code         */
            str(lab_ged$,17%,5%) = dt_seq$       /* Sequence Number    */
/* (AWD111) sort stock tempered as non-tempered */
/* PATIO$ 0=NotPatio 1=Door 2=StockSize*/
            str(lab_ged$,22%,7%) = wd$           /* Window Width       */
            str(lab_ged$,29%,6%) = ht$           /* Window Height      */
            str(lab_ged$,35%,2%) = ty$           /* Glass Type Code    */
            str(lab_ged$,37%,1%) = cl$           /* Color Code         */
            str(lab_ged$,38%,1%) = "0"           /* 0 = TOP, 1 = BOT   */
            if str(view$,1%,1%) = "B" then str(lab_ged$,38%,1%) = "1"
/* (AWD115) */
            if door% = 1% then gosub chg_sort1                  /* (CR1987) */
            if dt_prv$ <> "18" and dt_prv$ <> "19" then goto noUpdteIGThckns
               if double% = 1% then goto noUpdteIGThckns        /* (CR1987) */
               if gls_trip% = 1% then goto noUpdteIGThckns      /* (CR1987) */
               if gls_quad% = 1% then goto noUpdteIGThckns      /* (CR1987) */
               if temp$ = "*" then goto noUpdteIGThckns
                  t_k$ = pt_k$                                  /* (CR1987) */

noUpdteIGThckns:
        REM (End) of Floating Sort Area
            str(lab_ged$,39%,6%) = t_k$          /* Overall Thickness  */
            so$ = str(dt_bar$,1%,8%)
            so_ln$ = str(dt_bar$,9%,2%)          /* (AWD085) */
            gosub get_spacer_desc                /* SPACE_D$ Descript  */
            gosub get_ged_adjust                 /* W_ADJ, H_ADJ       */
            gosub get_muttin                     /* MUTTIN$, LITS$,    */
                                                 /* VERT%, HORZ%       */
            gosub check_ged_special              /* Check Spec. Shapes */
            str(rm_bar$,1%,8%) = dt_ref$       /* 0-4 = TOP, 5-9 = BOT */
            convert (ct% -1%) to str(rm_bar$,9%,1%), pic(#)

            str(lab_ged$,45%,8%) = muttin$       /* Vertical/Horizontal*/
            str(lab_ged$,53%,9%) = rm_bar$       /* Record Counter     */
                                                 /* (EWD055)           */
/* (AWD111) sort stock tempered as non-tempered */
/* PATIO$  0=NotPatio 1=Door 2=StockSize*/
            str(lab_ged$,62%,5%) = ged_cnt$      /* RECORD COUNTER ??  */

/* (AWD111) sort stock tempered as non-tempered */
/* PATIO$  0=NotPatio 1=Door 2=StockSize*/
            if  patio$ <> "2"  and tempScrn% = 1% then                ~
                                               gosub temp_build_ged_key
                                            /* CORRECTION FOR 'CO' AND */
                                            /* OB GLASS TYPES          */
                                            /* (EWD020) - 05/23/00     */
                                            /*  (EWD055)  */
                                            /* (EWD065)                */
            if ty$ = "02" or ty$ = "14" or ty$ = "G4" then goto L62130

            if ty$ = "G1" or ty$ = "G2" or ty$ = "G3" then goto L62130

/*(AWD084) - New codes C9 and D4 */
            if ty$ = "C9" or ty$ = "D4" then goto L62130

            goto L62120
               IF TY$ = "E7" THEN GOTO L62120

               IF  TY$ = "G5" OR TY$ = "G6" THEN GOTO L62120

               IF TY$ = "G9" OR TY$ = "H0" THEN GOTO L62120
                                            /* (EWD027) - 12/06/00     */
               IF TY$ = "G7" OR TY$ = "G8" THEN GOTO L62120
               IF TY$ = "H1" OR TY$ = "H2" THEN GOTO L62120
                                            /* (EWD027)                */

        return


L62120:
            gosub check_bottom                       /* (EWD055) */
            if spec% <> 1% then return               /* (EWD055) */
            spacer$ = s_s$
            if double% = 1% then spacer$ = s_d$
/* (AWD111) */
            if tempScrn% = 1% and door% = 0% and trpThick% = 1% ~
                  then spacer$ = s_3_16$
            if str(dt_part$,13%,4%) = "0946" then doorThick% = 1%
            if doorThick% = 1% then spacer$ = s_5_32$
/* (AWD111\) */
            if gls_trip% = 1% then spacer$ = s_5_32$           /* (CR1987) */
            if gls_quad% = 1% then spacer$ = s_3_16$           /* (CR1987) */
            if stc% = 1% then spacer$ = s_3_16$  /* (CR1173)  */

            gosub get_spacer_desc                    /* (EWD055) */
            if str(view$,1%,1%) <> "B" then return
            spacer$ = s_d$                           /* (EWD055) */
            sandwich$ = str(desc$,18%,10%)           /* (EWD055) */
/* (AWD111) */
            if tempScrn% = 1% and door% = 0% and trpThick% = 1%  ~
                  then spacer$ = s_3_16$
            if doorThick% = 1% then spacer$ = s_5_32$
/* (AWD111\) */
            if gls_trip% = 1% then spacer$ = s_5_32$           /* (CR1987) */
            if gls_quad% = 1% then spacer$ = s_3_16$           /* (CR1987) */
            if stc% = 1% then spacer$ = s_3_16$  /* (CR1173)  */

            gosub get_spacer_desc                    /* (EWD055) */

            if gls_lamn% = 0% then goto no_LamnGlass
              if door% = 1% then goto checkDoorSizeLamnBot
               if str(dt_part$,13%,4%) > "0480" then strengthened% = 1%
               if str(dt_part$,17%,3%) > "720"  then strengthened% = 1%

               gosub get_spacer_desc
                  goto no_LamnGlass

checkDoorSizeLamnBot:
               if str(dt_part$,13%,4%) > "0794" then strengthened% = 1%
               if str(dt_part$,17%,3%) > "794" then strengthened% = 1%

               gosub get_spacer_desc
                  goto no_LamnGlass
no_LamnGlass:
            return                                   /* (EWD055) */

L62130:                                              /* (EWD065) */
            if str(view$,1%,1%) <> "B" then return
                                                     /* OBS Glass*/
                  if ty$ = "02" then sandwich$ = "IG4OB4CL"
                  if ty$ = "14" then sandwich$ = "IG3CL3CL"

/*(AWD097)*/
                  if ty$ = "G1" then sandwich$ = "IG4OB4LE"
                  if ty$ = "G2" then sandwich$ = "IG4OB4LEAR"
                  if ty$ = "G3" then sandwich$ = "IG4OB4PA"
                  if ty$ = "G4" then sandwich$ = "IG4OB4PAAR"
/*(AWD084) - Next two lines */
                  if ty$ = "C9" then sandwich$ = "IG4OB4PA"
                  if ty$ = "D4" then sandwich$ = "IG4OB4PAAR"
/* (SR67154) */
                  if ty$ = "3F" then sandwich$ = "IG4C34OBAR"
                  if ty$ = "3K" then sandwich$ = "IG4C34OTAR"
                                           /* (EWD027)                 */
        return
        temp_build_ged_key                 /* (AWD115) */
/* 0=NotPatio 1=Door 2=StockSize*/
REM Need to get Order Number and Sequence from AWDSCHGL
          if awdschgl% = 1% then goto setAWDSCHGL_ged_key
           str(lab_ged$,1%,1%)   = "0"           /* SPECIAL SORT FLAG  */
           str(lab_ged$,2%,1%)   = "0"           /* TOP/BOT FLAG       */
           str(lab_ged$,3%,6%)   = spacer$       /* Spacer Thickness   */
           str(lab_ged$,9%,5%)   = "00000"       /* Production Sequence*/
           str(lab_ged$,17%,5%)  = "00000"       /* Production Sequence*/

REM STR(LAB_GED$,53%,8%) = SO$
REM CONVERT CT% TO STR(LAB_GED$,61%,1%), PIC(0)
REM STR(LAB_GED$,62%,5%) = STR(DT_BAR$,9%,2%) & STR(DT_BAR$,12%,3%)
        return
        setAWDSCHGL_ged_key
REM Use this to put awdschgl ordered glass after the non awdschgl
REM ordered gls such as stock etc....
REM Set intercept to "99"
REM !!  STR(LAB_KEY$,1,2) = "99"
        return

                                                     /* (EWD065)   */
        check_bottom
           spec% = 0%
           init(" ") readkey$
           str(readkey$,1%,9%)   = "TEMP GED "
           str(readkey$,10%,15%) = ty$
           read #3,key = readkey$, using L62397, desc$,                ~
                                     eod goto check_bottom_done
           spec% = 1%
        check_bottom_done
           spec% = 0%
           init(" ") readkey$
           str(readkey$,1%,9%)   = "OBS GED  "
           str(readkey$,10%,15%) = ty$
           read #3,key = readkey$, using L62397, desc$,                ~
                                     eod goto check_obs_done
           spec% = 1%
        check_obs_done
        return
                                           /* (EWD020) - 05/23/00      */
        chg_sort1                          /* SPECIAL SORT VINYL PATIO */
           if schema% <> 1% then return    /* Only NC                  */

           reverse_sort%, reverse_seq% = 0%
           reverse_sort% = 99999%
           convert dt_seq$ to reverse_seq%, data goto reverse_bad

reverse_bad:

           reverse_sort% = reverse_sort% - reverse_seq%

           convert reverse_sort% to reverse_sort$, pic(00000)

REM STR(LAB_GED$,9%,5%)  = DT_SEQ$
REM REVERSE_SORT$ = DT_SEQ$
            str(lab_ged$,9%,5%)  = reverse_sort$

            str(lab_ged$,14%,3%) = model$        /* Model Code         */
            str(lab_ged$,17%,7%) = wd$           /* Window Width       */
            str(lab_ged$,24%,6%) = ht$           /* Window Height      */
            str(lab_ged$,30%,2%) = ty$           /* Glass Type Code    */
            str(lab_ged$,32%,1%) = cl$           /* COLOR              */
            str(lab_ged$,33%,1%) = "0"           /* 0 = TOP, 1 = BOT   */
            if str(view$,1%,1%) = "B" then str(lab_ged$,33%,1%) = "1"
REM STR(LAB_GED$,34%,5%) = DT_SEQ$       /* SEQUENCE NUMBER    */
            str(lab_ged$,34%,5%) = reverse_sort$  /* Sequence Number    */
        return

        calc_double                      /* Calc Double Strength Glass */
          double% = 0%
          x = 0.0 : y = 0.0 : z = 0.0
          if lit_flg$ = "Y" then return          /* (EWD018)           */

          if gls_double% = 1% then goto L62346
                                                 /* (EWD024)           */
          if dt_dept$ = "047" then goto L62346
          if dt_dept$ = "033" then goto L62346
/* NTX Shapes */
REM IF DT_DEPT$ = "043" THEN GOTO L62346   /* (EWD058)         */
          if dt_dept$ = "043" and schema% = 1% then goto L62346
          if dt_dept$ = "002" and schema% = 2% then goto L62346
          if dt_dept$ = "003" and schema% = 2% then goto L62346
REM IF DT_DEPT$ = "019" THEN GOTO L62346   /* (AWD105)        */
                                                 /* (EWD024)           */
REM WD1$ & HT1$ are glass size not window size
          convert str(wd1$,1%,3%) to x, data goto L62313
L62313:
          convert str(ht1$,1%,2%) to y, data goto L62319
L62319:
          if str(wd1$,7%,1%) = "/" then x = x + 1.0
          if str(ht1$,6%,1%) = "/" then y = y + 1.0
          z = (x * y)/ 144.0
          if z > 20.0 then goto L62346               /* Square Feet     */
          if x > 60.0 then goto L62346               /* Width Greater   */
          if y > 60.0 then goto L62346               /* Height Greater  */
          if (x + y) > 100.0 then goto L62346        /* Tot United Inch */
/* (AWD107) */
          if model$ = "F21" or model$ = "F31" then goto ck_fl_sh
          if model$ = "F41" or model$ = "F51" then goto ck_fl_sh

          if model$ = "S11" or model$ = "S13" then goto ck_fl_2sl
          if model$ = "S15" or model$ = "S17" then goto ck_fl_2sl

          if model$ = "S12" or model$ = "S14" then goto ck_fl_2sl
          if model$ = "S16" or model$ = "S18" then goto ck_fl_3sl
/* (\AWD107) */

/*(AWD092)*/
          if model$ <> "831" and model$ <> "841" then return
             if x < 31 or x > 36 then return   /* width hasToBeBtwn31&36*/
             if y < 54 or y > 66 then return   /* heigh hasToBeBtwn54&66*/
                double% = 1%
/*(AWD092/)*/
        return
L62346:
          double% = 1%
        return

/* (AWD107) */
        ck_fl_sh
/* Oriel different; Cottage not offered yet */
            if hgl$ = "OR" then goto fl_sh_or
            if hgl$ = "CO" then return

            if x >= 38 and y >= 55 then goto L62346
        return
fl_sh_or:

           if x >= 32 and y >= 46 then goto L62346
       return
       ck_fl_2sl
           if x >= 39 and y = 56 then goto L62346

           if x >= 57 and y >= 42 then goto L62346
       return
       ck_fl_3sl
           if hgl$ = "1/3" then goto ck_fl_3sl_1_3
           value = 0.00

/* -0.4545 is slop of angle  */
/* Addition value is offset  */

           value = ((-0.4545 * x) + 82.724)

           if y >= value then goto L62346
       return
ck_fl_3sl_1_3:

/* -0.5555 is slop of angle  */

           value = ((-0.5555 * x) + 108.8835)

           if y >= value then goto L62346

        return

        check_ged_glass                            /* Thickness/Spacer */
REM           call "SHOSTAT" ("CHECK GED GLASS " ) stop
           init(" ") readkey$, sandwich$, tty$
           tty$ = ty$ : tty% = 0%
           convert tty$ to tty%, data goto L62367
L62367:
           if double% = 0% then goto L62388
              if tty% = 0% then goto L62388
/* (CR1974) */
REM IF TTY% > 50% THEN GOTO L62388
REM TTY% = TTY% + 50%
REM CONVERT TTY% TO TTY$, PIC(00)

L62388:    str(readkey$,1%,9%)   = "GED 001  "
           str(readkey$,10%,15%) = tty$
           read #3,key = readkey$, using L62397, desc$, eod goto L62412
L62397:       FMT POS(25), CH(30)
           sandwich$ = str(desc$,1%,10%)
           tty$ = ty$
                                                 /* (EWD055) */
           if tty% = 89% then temp$ = "*" /* SPECIAL GLASS AT END */
                                                 /* (EWD016) Begin*/
/* (CR1974) */
           if double% = 1% and tty% > 0% and tty% < 50% then                ~
                                                      gosub convertSandwichDS
           if tty% > 0% then return
           if double% = 0% then return
           if gls_trip% = 1% then return                    /* (CR1987) */
           if gls_quad% = 1% then return                    /* (CR1987) */
           if stc% = 1% then return                         /* (CR1987) */
           if gls_lamn% = 1% then return                    /* (CR1987) */
                                                 /* Must be Alpha */
/* (SR67154) */                       /* Do not check Triple Pane */
REM L62400
           if str(sandwich$,1%,2%) <> "IG" then return
             gosub convertSandwichDS /* (CR1974) */
REM DD% = POS(SANDWICH$ = "3")
REM IF DD% = 0% THEN RETURN
REM STR(SANDWICH$,DD%,1%) = "4"
REM GOTO L62400                        /* (EWD016) END  */
/* (CR1974) */
REM IF STR(SANDWICH$,3%,1%) = "3" THEN STR(SANDWICH$,3%,1%) = "4"
REM IF STR(SANDWICH$,6%,1%) = "3" THEN STR(SANDWICH$,6%,1%) = "4"
/* (SR67154) */
        return
L62412:   sandwich$ = "ERR-02 ***" : err% = 5%
        return
/* + (CR1974) */
        convertSandwichDS
          if str(sandwich$,3%,1%) = "3" then str(sandwich$,3%,1%) = "4"
          if str(sandwich$,6%,1%) = "3" then str(sandwich$,6%,1%) = "4"
        return
/* - (CR1974) */
/*(AWD111) */
        checkTempGed
/* (AWD115) */
REM set default to 1% becuase this is only checked when tempered
           topTemp%, botTemp% = 1%
           init(" ") readkey$, desc$
           str(readkey$,1,9)  = "TEMP GED"
           str(readkey$,10,2) = ty$

           read #3, key = readkey$, using L62397, desc$, eod goto noTempGed

              if view$ = "TOP" then sandwich$ = str(desc$,1%,10%)
              if view$ = "BOT" then sandwich$ = str(desc$,18%,10%)
/* (AWD115) */
              convert str(desc$,15%,1%) to topTemp%, data goto topTempErr
topTempErr:

              convert str(desc$,30%,1%) to botTemp%, data goto botTempErr
botTempErr:
/* (\AWD115) */
              if topTemp% = 0% or botTemp% = 0% then temp1$ = " "
       return
       noTempGed
           init(" ") readkey$, desc$
           str(readkey$,1,9)  = "GED 001 "
           str(readkey$,10,2) = ty$

           read #3, key = readkey$, using L62397, desc$, eod goto  noGED001

              sandwich$ = str(desc$,1%,10%)
        noGED001
        return
/*(AWD111\) */


        get_spacer_desc                      /* Get Spacer Description */
           init(" ") space_d$, readkey$
           str(readkey$,1%,9%)   = "GED 000  "
           str(readkey$,10%,15%) = str(spacer$,2%,5%)  /* Skip Decimal */
           read #3,key = readkey$, using L62436, space_d$, eod goto L62442
L62436:       FMT POS(25), CH(10)
        return
L62442:    space_d$ = "ERR-03 ***" : err% = 6%
        return

        get_ged_adjust                       /* Get Width/Height Adj   */
           w_adj, h_adj = 0.0                          /*-O = No Adj.  */
/* (AWD116) */
           interadj = 0.00
           interadj = interadj(intercept%)

           init(" ") w_adj$, h_adj$, readkey$          /*-T = Top Adj. */
           str(readkey$,1%,9%)   = "GED 004  "         /*-B = Bot Adj. */
           str(readkey$,10%,15%) = model$ & "-" & str(view$,1%,1%)
           read #3,key = readkey$, using L62469, desc$, eod goto L62499
L62469:       FMT POS(25), CH(30)
                                                   /* Adjustment Found */
              w_adj$ = str(desc$,1%,6%) : h_adj$ = str(desc$,8%,6%)
              convert w_adj$ to w_adj, data goto L62481
L62481:
/* (AWD116) */
              w_adj = w_adj + interadj

              convert h_adj$ to h_adj, data goto L62487

L62487:
/* (AWD116) */
               h_adj = h_adj + interadj

           if w_adj < 0.0 then convert w_adj to w_adj$, pic(-0.###)      ~
                          else convert w_adj to w_adj$, pic(0.####)
           if h_adj < 0.0 then convert h_adj to h_adj$, pic(-0.###)      ~
                          else convert h_adj to h_adj$, pic(0.####)

L62499:
/* (AWD116) */
          convert interoffset(intercept%) to interoffset$, pic(0.00000)

          if interadj(intercept%) < 0.00 then     ~
            convert interadj(intercept%) to interadj$, pic(-0.0000) ~
          else                                                      ~
            convert interadj(intercept%) to interadj$, pic(0.00000)
        return

        get_muttin
            vert% = 0% : horz% = 0% : er% = 0%
            if str(dt_part$,7%,2%) <> "00" then goto L62523
               muttin$ = "        " : lits$ = "0"
               return

L62523:     call "APCGSLIT" ( dt_part$,         /* MFG Part Number     */~
                              muttin$,          /* Grid Vert/Horiz Code*/~
                              lits$,            /* No. of Lits         */~
                              str(view$,1%,1%), /* T or B              */~
                              vert%,            /* Number of Verticals */~
                              horz%,            /* Number of Horizontal*/~
                              #3,               /* (GENCODES)          */~
                              er% )             /* Error Code          */
            if er% = 0% then return
               if er% = 1% then err% = 7%       /* GED Lits Error      */
               if er% = 2% then err% = 8%       /* GED Hinge Error     */
               if er% = 3% then err% = 9%       /* GED Muttin Error    */
        return

        check_ged_special
            for i% = 1% to mod%(8%)
               if model$ <> mod$(8%,i%) then goto L62586
               sort% = 900%
               convert sort% to srt$, pic(00000)
               str(lab_key$,8%,5%) = srt$

               i% = mod%(8%) + 1%              /* Exit Loop        */
                                               /* (EWd017)         */
L62586:     next i%
        return

        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#6,mode$, 500%, f2%)
            if f2% <> 0% then goto L62619
        return
L62619:     call "SHOSTAT" ("Error - Cannot Open (APCPLNWK)") : stop
        return
        delete_work
            call "FILEBGON" (#6)
        return

        open_work_shape
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#27,mode$, 500%, f2%)
            if f2% <> 0% then goto L62630
        return
L62630:     call "SHOSTAT" ("Error - Cannot Open (EWDGLSWW)") : stop
        return
        delete_work_shape
            call "FILEBGON" (#27)
        return

        get_sort                                 /* (EWD004) - Begin    */
            vv$ = str(view$,1%,1%)
            str(sort$,1%,3%) = dt_dept$
            str(sort$,4%,2%) = "SS"
REM IF DOUBLE% = 1% THEN STR(SORT$,4%,2%) = "DS"  /* (CR1987) */
            if double% = 1% then str(sort$,4%,2%) = "DD"        /* (CR1987) */
            if gls_trip% = 1% then str(sort$,4%,2%) = "TT"      /* (CR1987) */
            if gls_quad% = 1% then str(sort$,4%,2%) = "QQ"      /* (CR1987) */
            if schema% = 1% and stc% = 1% then str(sort$,4%,2%) = "DT" /*(CR2260) */
            str(sort$,6%,4%) = str(space_d$,1%,4%)
            str(sort$,10%,2%) = "TB"
            if schema% = 2% then goto check_NTX_TT_BB

            if dt_dept$ <> "048" then goto L62673   /* New Construction */

REM IF STR(SORT$,4,6) = "DSSP17" THEN GOTO L62760   /* (CR1987) */
               if str(sort$,4,6) = "DDSP17" then goto L62760
               if str(sort$,4,6) = "SSSP19" then goto L62760
REM IF STR(SORT$,4,6) = "DSSP11" THEN GOTO L62760  /* (CR1987) */
               if str(sort$,4,6) = "DDSP11" then goto L62760
               if str(sort$,4,6) = "SSSP13" then goto L62760
                  goto L62664


REM GOTO L62760                        /* (AWD100) */

                                                    /* (EWD013)         */
               if str(model$,1%,1%) <> "7" and str(model$,1%,1%) <> "1"  ~
                                                         then goto L62760
               if str(model$,1%,1%) = "7" then goto L62664
               if str(model$,1%,3%) = "121" or str(model$,1%,3%) = "131" ~
                                                         then goto L62664
                                                    /*  (EWD049)        */
               if str(model$,1%,3%) = "120" or str(model$,1%,3%) = "130" ~
                                                         then goto L62664
               if str(model$,1%,3%) = "126" or str(model$,1%,3%) = "136" ~
                                                         then goto L62664
               if str(model$,1%,3%) = "127" or str(model$,1%,3%) = "137" ~
                                                         then goto L62664

                  goto L62760                     /* Uses 'TB' for Sort */
                                                    /* (EWD013)         */
L62664:           str(sort$,10%,2%) = "TT"
                  if vv$ = "B" then str(sort$,10%,2%) = "BB"
                  goto L62760
L62673:     if dt_dept$ = "028" then goto L62664    /* Welded Sash      */
            if dt_dept$ = "027" then goto L62664    /* Welded Sash      */
            if dt_dept$ = "064" then goto L62664    /* New Welded Sash  */
            if dt_dept$ = "019" then goto L62664
            if dt_dept$ = "025" then goto L62664    /* (IM7733) */
            if dt_dept$ = "053" then goto L62664
REM Mod for BSOs not to be TT BB now TB and spacer is SP13
            bso% = 0%
            if str(dt_part$,11,1) = "4" then bso% = 1%
            if str(dt_part$,11,1) = "5" then bso% = 1%
            if str(dt_part$,11,1) = "6" then bso% = 1%
            if str(dt_part$,11,1) = "7" then bso% = 1%
                                                    /* Vinyl Prime 712  */
REM IF DT_DEPT$ <> "036" THEN GOTO NOTBSORT
            if dt_dept$ = "036" then goto L62664

REM Windows still TTBB sort
REM IF BSO% = 0% THEN GOTO L62664
REM IF BSO% = 1% AND STR(SPACE_D$,1%,4%) <> "SP13" THEN GOTO L62664
REM keep sort as TB
REM NOTBSORT
            if dt_dept$ = "005" then goto L62664    /* Low End Single Hg*/
            if dt_dept$ = "007" then goto L62664    /* (EWD019)         */
            if dt_dept$ = "026" then goto L62664    /* (EWD048)         */
            if dt_dept$ = "049" then goto L62664
            if dt_dept$ = "053" then goto L62664
            if dt_dept$ = "006" then goto L62664


REM            if dt_dept$ <> "023" then goto L62694   /* Vinyl Patio    */
                                                       /* Vinyl Patio    */
            if dt_dept$ <> "023" and dt_dept$ <> "020" then goto L62694
               i% = 3%                              /* (AWD079)         */
               str(sort$,10%,2%) = "TT"
               if vv$ = "T" then goto L62775
                 i% = 4%
                 str(sort$,10%,2%) = "BB"
               goto L62775

L62694:     if dt_dept$ <> "042" then goto L62760   /* Hinged Patio     */
               i% = 93%                             /* (EWD017)         */
               goto L62775

L62760:
/* (AWD099) */
            index% = 1%
/* (AWD112) */
            if schema% <> 1% then goto skip_sdl_check
            if sdl% = 1% and                                 ~
                   (dt_dept$ <> "008" and dt_dept$ <> "047") ~
                    then index% = sdl_index%

REM IF SDL% = 1% AND                                 ~
REM (DT_DEPT$ <> "008" AND DT_DEPT$ <> "047"  ~
REM AND DT_DEPT$ <> "019")                   ~
REM THEN INDEX% = 103%

/* (AWD117) */
            if tripane% = 1% then index% = tpl_index%

skip_sdl_check:

            if gls_lamn% = 1% then index% = lamn_index%
            if stc% = 1% then index% = stc_index%   /* (CR1173) */

            for i% = index% to ss_max%
                if ss$(i%) = sort$ then goto L62775
            next i%

            init(" ") errormsg$, index$
            convert index% to index$, pic(0000)
            errormsg$ = "Sorting Error " & sort$ & " BAR " & dt_bar$ &   ~
                                                         " INDEX " & index$
            gosub error_prompt
            i% = 999%
            str(lab_rec$(),7%,25%) = "Sorting Error with Spacer"

L62775:
            init(" ") srt$                            /* (AWD105) */
            sort% = 0%                                /* (AWD105) */
            sort% = i%                                /* (AWD105) */
/* (AWD112) */
            convert sort% to srt$, pic(00000)
        return
            for i% = 73% to ss_max%
                if ss$(i%) = sort$ then goto L62785
            next i%
            i% = ZZ%
            str(lab_rec$(),7%,25%) = "Sorting Error with Spacer"

L62785:     convert i% to ss$, pic(00)
        return                                      /*  (EWD043)   - BEG */

check_NTX_TT_BB:
/* (CR1938) BEG */
REM IF DT_DEPT$ = "029" THEN STR(SORT$,10%,2%) = "TT"
REM IF DT_DEPT$ = "029" AND VV$ = "B" THEN STR(SORT$,10%,2%) = "BB"
REM IF DT_DEPT$ = "071" THEN STR(SORT$,10%,2%) = "TT"
REM IF DT_DEPT$ = "071" AND VV$ = "B" THEN STR(SORT$,10%,2%) = "BB"
/* (CR1938) END */

/* (CR1767) */ /* (CR2411) */
        if dt_dept$ = "058" then str(sort$,10%,2%) = "TT"
        if dt_dept$ = "058" and vv$ = "B" then str(sort$,10%,2%) = "BB"
        if dt_dept$ = "059" then str(sort$,10%,2%) = "TT"
        if dt_dept$ = "059" and vv$ = "B" then str(sort$,10%,2%) = "BB"
        if dt_dept$ = "029" then str(sort$,10%,2%) = "TT"
        if dt_dept$ = "029" and vv$ = "B" then str(sort$,10%,2%) = "BB"
        if dt_dept$ = "070" then str(sort$,10%,2%) = "TT"
        if dt_dept$ = "070" and vv$ = "B" then str(sort$,10%,2%) = "BB"
        if dt_dept$ = "071" then str(sort$,10%,2%) = "TT"
        if dt_dept$ = "071" and vv$ = "B" then str(sort$,10%,2%) = "BB"
/*+ (CR2232) */
        if dt_dept$ = "027" then str(sort$,10%,2%) = "TT"
        if dt_dept$ = "027" and vv$ = "B" then str(sort$,10%,2%) = "BB"
/*- (CR2232) */
        goto L62760

        get_sort_temp                               /* (EWD055) - BEG    */

            gosub check_temp_stock
            if temp_stock% = 1% then goto L62790

            gosub getTempType
            temp_rack$ = "1"
            gosub check_width_height
            if spType% = 998% then i% = 998%
            if spType% = 999% then i% = 999%
            if spType% = 998% or spType% = 999% then goto L62790

            convert spType% to str(sort$,1,3), pic(000)
            str(sort$,4,1) = temp_rack$
            str(sort$,5,4) = space_d$
REM STR(SORT$,9,2) = "DS"        /* (CR1987) */
            str(sort$,9,2) = "DD"                           /* (CR2260) */
REM IF SCHEMA% = 1% AND STC% = 1% THEN STR(SORT$,9,2) = "DT"

            for i% = 1% to ss_max%
                if ss_temp$(i%) = sort$ then goto L62790
            next i%
REM CALL "SHOSTAT" ("SORTING ERR")  STOP
/* (AWD112) */
            init(" ") errormsg$                                  /*+(CR1987)*/
            errormsg$ = "Sorting Error " & sort$ & " BAR " & dt_bar$ &   ~
                                                         " INDEX " & index$
            gosub error_prompt                                   /*-(CR1987)*/
            i% = 999%
            str(lab_rec$(),7%,25%) = "Sorting Error with Spacer"

L62790:
            init(" ") srt$
            sort% = 0%
            sort% = i%
            if awdschgl% = 1% then goto get_sort_temp_awdschgl
REM!!  call "SHOSTAT" (" Here are SORT TEMP ")
REM!!  stop

/* (AWD112) */

            convert sort% to srt$, pic(00000)
REM !! put a '9' to force at end
            str(lab_key$,8%,5%) = srt$
REM Don't override SO number for remakes, causing remakes to
REM show blank for so
/* (IM8022) */
REM IF RM_FLAG% <> 3% THEN STR(LAB_REC$(),97%,8%) =  STR(RM_BAR$,1%,8%)
REM IF RM_FLAG% <> 3% THEN STR(LAB_REC$(),190%,1%) = STR(RM_BAR$,9%,1%)
REM !!!!  What is this used for????
            str(lab_rec$(),154%,2%)  = srt$
REM Don't override seq number for remakes, causing remakes to
REM show zero for seq
REM         IF RM_FLAG% <> 3% THEN                         ~
REM         STR(LAB_REC$(),176%,5%)  = STR(LAB_GED$,9%,5%)  /*DEPTSEQMOD */
/* (AWD117) */
            str(lab_rec$(),176%,5%)  = dt_seq$
            str(lab_rec$(),211%,11%) = ss_temp$(sort%)

REM STR(LAB_REC$(),243%,76%) = " "
        return
/* (AWD115) */
        get_sort_temp_awdschgl
REM !! put a '99999' to force at end
REM !!         call "SHOSTAT" (" Here are SORT TEMP AWDSCHGL ")
REM !!         stop
          str(lab_key$,3%,5%)  = "99999"
          str(lab_key$,8%,5%)  = gl_order$
          str(lab_key$,13%,5%) = gl_order_seq$

REM !!!!  What is this used for????
           str(lab_rec$(),154%,2%)  = srt$
REM STR(LAB_REC$(),176%,5%)  = STR(LAB_GED$,9%,5%) /*DEPTSEQMOD*/
/* (AWD117) */
            str(lab_rec$(),176%,5%)  = dt_seq$

            str(lab_rec$(),211%,11%) = ss_temp$(sort%)
REM STR(LAB_REC$(),243%,76%) = " "

        return

        check_width_height
             temp_rack$ = "1"         /* (AWD111) */

             if width <= 38.0 and height <= 68.0 then temp_rack$ = "0"
             if width <= 68.0 and height <= 38.0 then temp_rack$ = "0"

             if width <= 6.0 and height <= 12.0 then temp_rack$ = "1"
             if width <= 12.0 and height <= 6.0 then temp_rack$ = "1"
/* (AWD109) */
/* (AWD112) */
        return

             if ultra$ <> "1" then return
                   if temp_rack$ = "0" then temp_rack$ = "2"
                   if temp_rack$ = "1" then temp_rack$ = "3"

        return
        check_temp_stock
            if lamnScrn% = 1% then return    /* (AWD123) */
            temp_stock% = 0%
            init(" ") readkey$, desc$
            str(readkey$, 1%,9%)  = "TEMPSTOCK"
            str(readkey$,10%,3%) = str(dt_part$,1%,3%)
            str(readkey$,13%,2%) = str(dt_part$,5%,2%)
            str(readkey$,15%,4%) = str(dt_part$,13%,4%)
            str(readkey$,19%,3%) = str(dt_part$,17%,3%)

            read #3,key = readkey$, using L52330, desc$,eod goto not_stock

               temp_stock% = 1%
/* (AWD109) ultra changes */
               i% = 991%
               sort% = 991%
               if ultra$ = "1" then i% = 992%
               if ultra$ = "1" then sort% = 992%
               ss$ = srt$
        not_stock
        return
                                                 /* (EWD055) - End       */
/*(AWD111) */
        getTempType
          spType% = 1%
          if double% = 1% then spType% = 2%
          if temp$ = "*" then spType% = 3%
          if sdl% = 1% then spType% = 4%
          if sdl% = 1% and double% = 1% then spType% = 5%
          if sdl% = 1% and temp$ = "*" then spType% = 6%
          if trpThick% = 1% then spType% = 7%
          if doorThick% = 1% then spType% = 8%
          if ty$ = "AZ" then spType% = 11%
          if stc% = 1% then spType% = 16%

          /* do not look at spTemp on Patio stock or custom */
          if scr_sel% = 5% or scr_sel% = 6% then return

/* (AWD112) */
REM  !!IF TY$ = "89" THEN SPTYPE% = 998%
/* (AWD123) */
REM ===============
REM     Laminate Glass                 spType% = 9%
REM     Laminate Strengthened Glass    spType% =10%
REM     Laminate 5/32                  spType% =12%
REM     Laminate Strengthened 5/32     spType% =13%
REM     Laminate 3/16                  spType% =14%
REM     Laminate Strengthened 3/16     spType% =15%
REM ===============
          if gls_lamn% = 0% then goto notLamn
             spType% = 9%
             if strengthened% = 1% then spType% = 10%
             if obs% = 0% and sqFeet > 25 then spType% = 12%
             if obs% = 0% and sqFeet > 25 and strengthened% = 1% then ~
                                               spType% = 13%

             if obs% = 1% and sqFeet > 25 then spType% = 14%
             if obs% = 1% and sqFeet > 25 and strengthened% = 1% then ~
                                               spType% = 15%
notLamn:
          if awdschgl% = 0% and ty$ = "89" then spType% = 998%
REM  !!if spTemp% = 0% then spType% = 998%
        return
/* (AWD111\) */
                                                 /* (EWD004) - End       */
        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_error                                    /* (EWD014)        */
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return
                                                      /* (EWD014)        */
        check_support
           if str(dt_dept$,1%,3%) = "104" then return /* (EWD054)        */
           supp% = 0%
           init(" ") readkey$
           str(readkey$,1%,9%)   = "PLAN SUPP"
           str(readkey$,10%,15%) = dt_dept$
           read #3,key = readkey$, eod goto check_support_done
           supp% = 1%
        check_support_done
        return

        build_label                        /* (EWD017)                 */
            init(" ") a18$, a19$, a20$, a21$, a22$, a23$, a24$, a25$, a27$

            if str(lab_ged$,1%,2%) = "ZZ" then goto error_label
            a1$ = ty$                      /* (02)     Glass Type      */
            a2$ = mut$                     /* (06)     Grid/Liting Vode*/
            a3$ = so$                      /* (08)     S.O. Number     */
            a4$ = wd1$ & " "               /* (09)     Calc_width      */
            a5$ = model$                   /* (03)     Model Code      */
            a6$ = scr_dte$                 /* (08)     Production Date */
            a7$ = ht1$ & "  "              /* (09)     Calc Height     */
            a8$ = dt_shft$                 /* (02)     Shift Code      */
            a9$ = dt_seq$                  /* (05)     Dept Seq No.    */
            a10$= wd$ & "X" & ht$          /* (15)     Growth          */
            a11$= rm_num$                  /* (03)     Remake Counter  */
            a12$= dt_dept$                 /* (03)     Department Code */
            a13$= txt$                     /* (40)     40 Char. Text   */
            a14$= rm_bar$                  /* (09)     Glass Barcode   */
            a15$= view$                    /* (03)     View Top/Bot    */
            a16$= cl_l$                    /* (06)     Window Color    */

/* remove @@@ */ sh_hub$ = str(sh_rec$,150%,10%)
              a25$ = sh_hub$                 /* ??????                 */
            a26$= cl$ & lk$                /* (02)     Color,Lock Code */
        REM A17$= LK$                      /* (01)     Contour Grid    */
            a27$= str(dt_part$,11%,1%)     /* (01)     Screen Code     */

REM     A26$= CL$ & LK$                /* (02)     Color,Lock Code */
REM     A17$= LK$                      /* (01)     Contour Grid    */
REM     A27$= STR(DT_PART$,11%,1%)     /* (01)     Screen Code     */

                                           /* Record Length = 167 ??   */
        return
                                           /* (EWD056) Begin           */
        build_label_sh                     /* (EWD031)                 */
            if process% = 1% then kk% = 1%

            a1$ = str(bil$(kk%,20%),1%,2%) /* (02)     Glass Type      */
            a2$ = "      "                 /* (06)     Grid/Liting Vode*/
            a3$ = "99999999"               /* (08)     S.O. Number     */
            a4$ = "         "              /* (09)     Calc_width      */
            a5$ = str(bil$(kk%,1%),1%,3%)  /* (03)     Model Code      */
            a6$ = scr_dte$                 /* (08)     Production Date */
            a7$ = "         "              /* (09)     Calc Height     */
            a8$ = "01"                     /* (02)     Shift Code      */
            a9$ = "0" & str(bil$(kk%,4%),1%,4%)  /* (05)Dept Seq No.   */
            a10$= "               "        /* (15)     Growth          */
            a11$= "000"                    /* (03)     Remake Counter  */
REM A12$= "043"                 /* (03)     DEPARTMENT CODE */
REM IF SCHEMA% = 1% THEN A12$ = "043"
REM IF SCHEMA% = 2% THEN A12$ = DT_DEPT$
            a12$= dt_dept$
            a13$= "@SEQ" & str(bil$(kk%,23%),1%,18%)
                                           /* (40)     40 Char. Text   */
        REM A14$= SH_NUM$                  /* (09)     Glass Barcode   */
            a15$= "TOP"                    /* (03)     View Top/Bot    */
            a16$= "      "                 /* (06)     Window Color    */
            a17$= " "                      /* (01)     Contour Grid    */
                                           /* Record Length = 203 ??   */
            a18$= str(bil$(kk%,7%),1%,9%)  /* (09)     Base            */
            a19$= str(bil$(kk%,9%),1%,9%)  /* (09)     left            */
            a20$= str(bil$(kk%,11%),1%,9%) /* (09)     Right           */
            a21$= str(bil$(kk%,13%),1%,9%) /* (09)     Top             */
            a22$= str(bil$(kk%,15%),1%,9%) /* (09)     S1              */
            a23$= "0" & str(bil$(kk%,2%),1%,2%)  /*(03)ConfigurationCode*/
            a24$= str(bil$(kk%,3%),1%,2%)  /* (02)   Config Seq        */
                                           /* Put new fields in text   */
                                           /* Line (2) for Label       */
                                           /* (10)   Window Hub Adjust */

/* remove @@@ */ sh_hub$ = str(sh_rec$,150%,10%)
            str(a25$,1%,10%) = sh_hub$
                                           /* (04)   Glass Faceing Code*/
            str(a25$,11%,4%) = str(bil$(kk%,25%),1%,4%)
                                           /* (07)   Print Field Flags */
            str(a25$,15%,7%) = str(bil$(kk%,26%),1%,7%)
                                           /* (07)   Bridge File Flags */
            str(a25$,22%,7%) = str(bil$(kk%,27%),1%,7%)
                                           /* (07)   Positon Value     */
            str(a25$,29%,7%) = str(bil$(kk%,28%),1%,7%)
                                           /* (07)   Data Entered      */
            str(a25$,36%,7%) = str(bil$(kk%,29%),1%,7%)

            if process% = 2% then return

               a2$ = sh_lt$ & "     "      /* Grid                    */

               so$ = str(dt_bar$,1%,8%)

                                           /* (AWD085)                */
               so_ln$ = str(dt_bar$,9%,2%) /* Save Line Item          */

               a3$ = so$
               a4$ = wd1$ & " "
               a5$ = model$
               a7$ = ht1$ & " "
               a8$ = dt_shft$
               a9$ = dt_seq$
               a10$ = wd$ & "X" & ht$
               a11$ = rm_num$              /* (EWD070)                 */
               a12$ = dt_dept$
               a13$ = txt$

               a15$ = view$
               a16$ = cl_l$
               a26$ = cl$ & lk$
               a27$ = str(dt_part$,11%,1%) /* Save Screen Code         */
                                           /* Note a14$ will have 'P'  */
                                           /* in Barcode Temporary     */
                                           /* Because of two labels.   */

                                           /* Record Length = 253      */

        return                             /* (EWD056) END             */

        error_label
            a1$ = "**"                     /* (02)     Glass Type      */
            a2$ = "ERR=  "                 /* (06)     Grid/Liting Vode*/
            str(a2$,6%,1%) = str(lab_rec$(),32%,1%)  /* Set Error Code */
            a3$ = str(lab_ged$,27%,8%)     /* (08)     S.O. Number     */
            a4$ = "*********"              /* (09)     Calc_width      */
            a5$ = str(lab_rec$(),7%,3%)    /* (03)     Model Code      */
            a6$ = "********"               /* (08)     Production Date */
            a7$ = "*********"              /* (09)     Calc Height     */
            a8$ = "**"                     /* (02)     Shift Code      */
            a9$ = str(lab_ged$,17%,5%)     /* (05)     Dept Seq No.    */
            a10$= "***************"        /* (15)     Growth          */
            a11$= "ERR"                    /* (03)     Remake Counter  */
            a12$= str(lab_ged$,35%,3%)     /* (03)     Department Code */
                                           /* (40)     40 Char. Text   */
            a13$= "MODEL = " & str(lab_rec$(),7%,25%) & " ******"
            a14$= "000000000"              /* (09)     Glass Barcode   */
            a15$= "***"                    /* (03)     View Top/Bot    */
            a16$= "ERROR*"                 /* (06)     Window Color    */
            a17$= " "                      /* (01)     Contour Grid    */
                                           /* Record Length = 155 ??   */
        return

        print_labels
            call "SHOSTAT" ("Printing Glass Labels")
                                              /* (EWD012) - New Version */
                                              /* (EWD014) -             */
            init(" ") lab_key$, lab_ged$, lab_rec$()      /* (AWD112)   */
            lab_key$ = all(hex(00))                       /* (AWD112)   */
            lab_ged$ = all(hex(00))

            read #6,key > lab_key$, using L63015, lab_key$, lab_ged$,     ~
                               lab_rec$(),  eod goto print_done
                    goto print_labels_first
        print_labels_nxt            /* (AWD112) make this a read next */
            read #6,using L63015, lab_key$, lab_ged$, lab_rec$(),     ~
                                                   eod goto print_done
L63015:        FMT CH(62), CH(66), 2*CH(223)     /*(AWD112) */

print_labels_first:
/* (AWD115) */
            awdschgl% = 0%
            if str(lab_key$,3%,5%) = "99999" then awdschgl% = 1%
/* (\AWD115) */
            cl$ = str(lab_rec$(),9%,1%)
REM GOSUB LOOKUP_COLOR

            gosub lookup_grdcolor
            view$ = "TOP"
            if str(lab_rec$(),45%,1%) = "B" then view$ = "BOT"
            l_lt$    = str(lab_rec$(),13%,6%)  /* Top=Left,Bot=Right   */
            so$      = str(lab_rec$(),97%,8%)
                                               /* (EWD055) */
            patio$ = str(lab_rec$(),234%,1%)
/* AWD111 stock is not sorted like custom and tempered glass */
/* 0=NotPatio 1=Door 2=StockSize*/
            if patio$ = "2" then goto L63020
            if awdschgl% = 1% then goto L63020
             if rm_flag% = 3% then goto L63020
REM scr_sel%->3% = remakes scr_sel%->9%=ultra remakes
REM IF TEMPERED% = 1% AND (SCR_SEL% <> 3% AND SCR_SEL% <> 9%) ~
REM THEN SO$ = STR(LAB_GED$,53%,8%)
L63020:
            wd1$     = str(lab_rec$(),19%,9%)
            dt_dept$ = str(lab_rec$(),183%,3%)
            ht1$     = str(lab_rec$(),28%,8%)
            dt_shft$ = str(lab_rec$(),181%,2%)
            dt_seq$  = str(lab_rec$(),176%,5%)       /* (EWD055) */

            rm_num$  = str(lab_rec$(),186%,3%)
            gl_num$  = rm_num$                     /* (EWD014)         */
            gosub lookup_hows                      /* (AWD080)         */
            gosub lookup_ups                       /* (AWD080)         */
            gosub lookup_so


            model$   = str(lab_rec$(),6%,3%)
            dt_txt$  = str(lab_rec$(),46%,4%)
            wd$      = str(lab_rec$(),84%,7%)
            ht$      = str(lab_rec$(),91%,6%)
            lt$      = str(lab_rec$(),65%,2%)
            lk$      = str(lab_rec$(),70%,1%)     /* Test for Contour */
            field1$  = str(lab_rec$(),191%,1%)    /* Gridtype (AWD083)*/
            field2$  = str(lab_rec$(),192%,1%)    /* Gridsize (AWD083)*/
            field3$  = str(lab_rec$(),193%,1%)    /* Gridcolor(AWD083)*/
            field4$  = str(lab_rec$(),194%,1%)    /* hardware (AWD083)*/
            field5$  = str(lab_rec$(),195%,1%)    /* foam     (AWD083)*/
            field6$  = str(lab_rec$(),196%,1%)    /* Casing           */
            field7$  = str(lab_rec$(),197%,1%)    /* Sample Color     */
            field8$  = str(lab_rec$(),198%,1%)    /* Grid Type        */
            field9$  = str(lab_rec$(),199%,1%)    /* Grid Size        */
            field10$ = str(lab_rec$(),200%,1%)    /* OutSide Grid Colo*/
            field11$ = str(lab_rec$(),201%,1%)    /* Inside Grid Color*/

REM P% = POS("0123456789HJPVWXY" = LK$ )    /* Grid.          */
REM IF P% = 0% THEN A17$ = "C"              /* Contour */
REM IF FIELD1$ = "2" THEN A17$ = "C"     /*(AWD083) */
/*(AWD096) */
            a17$ = " "
            if field1$ = "2" and field2$ = "3" then a17$ = "C"
            if field1$ = "2" and field2$ = "1" then a17$ = "E"
            if field8$ = "1" and field9$ = "1" then a17$ = "S"
/*(/AWD096) */
                                                   /*  (EWD052) Put 'W'*/
                                                   /* on Gls Label     */
REM IF CL$ = "G" OR CL$ = "H" THEN A17$ = "W"
REM IF CL$ = "I" OR CL$ = "J" THEN A17$ = "W"  /* 3/4 Grid */

            if field2$ = "2" then a17$ = "W"           /*(AWD083) */
/*(AWD118)*/ if str(lt$,1%,1%) = "V" then a17$ = "V" /* Valance Grid */

            nbr_line% = 0%
            init(" ") txt$, text$()
            call "APCPLTXT" (#9, dt_txt$, text$(), nbr_line%)
            txt$     = str(text$(2%),1%,40%)
            rm_bar$  = str(lab_ged$,53%,9%)

/* 0=NotPatio 1=Door 2=StockSize*/                           /* (EWD055) */
/* (IM8022) */
            goto L63025


REM IF PATIO$ = "2" THEN GOTO L63025
REM IF TEMPERED% <> 1% THEN GOTO NORESETTEMPKEY
/* (AWD115) */
REM IF AWDSCHGL% = 1% THEN GOTO NORESETTEMPKEY
REM IF TEMPERED% = 1% AND (SCR_SEL% = 3% OR SCR_SEL% = 9%)    ~
REM THEN GOTO NORESETTEMPKEY
REM IF TEMPERED% = 1% AND (SCR_SEL% = 4% OR SCR_SEL% = 7%)    ~
REM THEN GOTO NORESETTEMPKEY
REM IF TEMPERED% = 1% AND SCR_SEL% = 11%    ~
REM THEN GOTO NORESETTEMPKEY
REM RM_BAR$ = STR(LAB_REC$(),97%,8%) & STR(LAB_REC$(),190%,1%)


noResetTempKey:
L63025:

            ty$      = str(lab_rec$(),11%,2%)
            stock$   = str(lab_rec$(),50%,9%)
            ultra$     = str(lab_rec$(),233%,1%)
REM !CALL "SHOSTAT" ("LABEL INTERCEPT ")  STOP
            intercept$ = str(lab_rec$(),241%,2%)
            sandwich$ = str(lab_rec$(),115%,10%)   /* (CR2305) */
            gosub lookup_lowebarcode               /* (CR2305) */

            gosub change_muttin
            gosub build_label
            gosub print_lab
            goto print_labels_nxt
        print_done
        return

        print_lab                                  /* (EWD014)         */
           gosub create_label_data                 /* (EWD014)         */

        return
                                                   /* (EWD012) - End   */
        change_muttin
            lt% = 0%
            lt$ = str(lab_rec$(),65%,2%)
            convert lt$ to lt%, data goto L63234
L63234:
            mut$ = " "                         /* SWITCH VERT - HORZ */
            muttin$ = str(lab_ged$,45%,8%)     /*            TO      */
            if len(muttin$) < 5 then goto L63258
               xx% = pos(muttin$ = "x")
               cc% = pos(muttin$ = "C")
               ii% = (cc% - xx%)
            mut$ = str(muttin$,xx%+1%,ii%) & "x" & str(muttin$,1%,xx%-1%)
L63258:     if lt% > 82% then mut$ = l_lt$
            if lt% >= 58% then mut$ = l_lt$     /* (EWD050)           */
            if lt% = 0%  then mut$ = l_lt$     /* (EWD040)           */
            if lt% = 57% then mut$ = "PERIM"
/*(AWD118) */
            if lt$ = "V0" then mut$ = "CUST_VAL"
            if lt$ = "V1" then mut$ = "VALA1"
            if lt$ = "V2" then mut$ = "VALA2"
            if lt$ = "V3" then mut$ = "VALA3"
            if lt$ = "V4" then mut$ = "VALA4"
            if lt$ = "V5" then mut$ = "VALA5"
        return

        gen_rpt
            call "SHOSTAT" ("Creating Glass Report")
                                                   /* (EWD070)         */
            rm_tot% = 0% : rp_sel% = 0% : cust_tot% = 0%
            init(" ") rm_ky$, rm_tot$, cust_tot$
            convert rp_sel$ to rp_sel%, data goto L63282
L63282:                                            /* Set Start Date   */
            if rp_sel% > 3% then str(rm_ky$,1%,6%) = str(bg_dte$,1%,6%)
        gen_rpt_nxt
            if rp_sel% > 3% then goto L63303
               read #8,key 3% > rm_ky$, using L63309, rm_rec$(),          ~
                                                  eod goto gen_rpt_done
               goto L63315
                                                   /* (EWD070)         */
                                                   /* Sel = 4,5 Scan DT*/
L63303:     read #8,key 1% > rm_ky$, using L63309, rm_rec$(),             ~
                                                  eod goto gen_rpt_done
L63309:        FMT 2*CH(256)

L63315:     if rp_sel% < 4% then rm_ky$ = str(rm_rec$(),13%,21%)          ~
                            else rm_ky$ = str(rm_rec$(),7%,27%)

            rm_st$ = str(rm_rec$(),13%,1%)
            if rm_st$ = "2" and rp_sel% < 4% then goto gen_rpt_done
                                                  /* (EWD070)          */
            if scr_dept$ = "ALL" then goto L63320
               if scr_dept$ <> str(rm_rec$(),249%,3%) then                ~
                                            goto gen_rpt_nxt
/* NTX Shapes */
               if scr_dept$ <> "043" and schema% = 1% then goto L63320
               if (scr_dept$ <> "002" and scr_dept$ <> "003")      ~
                      and schema% = 2% then goto L63320

                                                  /* Check Scan Window */
               if rp_sel% <> 4% then goto L63320  /* only for Sel = 4  */
                  gosub check_scan_window
                                                  /* Not a Hit         */
                  if scan_wind% = 0% then goto gen_rpt_nxt

                                                  /* (EWD070)          */
L63320:
            if rm_st$ <> "0" then goto L63342
               if rp_sel% = 1% then goto print_data /* Scanned Re-Make */
                  goto gen_rpt_nxt
L63342:     if rm_st$ <> "1" then goto L63357
               if rp_sel% = 3% then goto print_data /* Scheduled Glass */
               if rp_sel% = 2% and str(rm_rec$(),32%,2%) <> "00" then     ~
                                    goto print_data /* Scheduled Re-Mak*/
                  goto gen_rpt_nxt
L63357:     if rp_sel% = 4% then goto print_data    /* Completed Glass */
            if rp_sel% = 5% and str(rm_rec$(),32%,2%) <> "00" then        ~
                                    goto print_data /* Completed Re-Mak*/
               goto gen_rpt_nxt

        print_data
            if rp_sel% < 4% then goto L63384
               if str(rm_ky$,1%,6%) > str(ed_dte$,1%,6%) then             ~
                                                        goto gen_rpt_done
L63384:        rm_bar$    = str(rm_rec$(),22%,9%)
               rm_dept$   = str(rm_rec$(),249,3%)
               rm_seq$    = str(rm_rec$(),242,5%)
               rm_model$  = str(rm_rec$(),72%,3%)
               rm_load$   = str(rm_rec$(),67%,5%)
               rm_gls$    = str(rm_rec$(),77%,2%)
               rm_mut$    = str(rm_rec$(),234%,8%)
               rm_wd$     = str(rm_rec$(),150%,7%)
               rm_ht$     = str(rm_rec$(),157%,6%)
               rm_reason$ = str(rm_rec$(),34%,2%)
               rm_so$     = str(rm_rec$(),163%,8%)
               gosub lookup_reason
                                           /* (EWD070) Seperate total   */
                                           /* for Glass received from   */
                                           /* Custom                    */
               if str(rm_rec$(),65%,1%) = "7" then                       ~
                                                cust_tot% = cust_tot% + 1%
               if str(rm_rec$(),65%,1%) = "7"                            ~
                                           then str(rm_seq$,1%,1%) = "C" ~
                                           else str(rm_seq$,1%,2%) = "  "
                                           /* Skip for Window Reports   */
                                           /* only used for Custom      */
               if str(rm_rec$(),65%,1%) <> "7" and scr_wind$ <> "0" then ~
                                            goto gen_rpt_nxt
                                           /* Non Custom set to Blank   */
                                           /* (EWD070)                  */
               rm_tot% = rm_tot% + 1%
               gosub print_dtl
               goto gen_rpt_nxt
        gen_rpt_done
            if rm_tot% = 0% then return
            convert rm_tot% to rm_tot$, pic(#######)
                                               /* (EWD070)              */
            convert cust_tot% to cust_tot$, pic(#######)

             if scr_dept$ <> "043" and schema% = 1% then PRT_1
             if scr_dept$ <> "002" and scr_dept$ <> "003"                 ~
                  and schema% = 2% then PRT_1
                                           /* Only Selection 4          */
               if rp_sel% <> 4% then goto prt_1
                  print using L55060
                  print using L55300, cust_tot$, rm_tot$
                  print using L55040
                  return
PRT_1:                                         /* (EWD070)              */
            print using L55060
            print using L55220, rm_tot$
            print using L55040
        return

        print_header
            init(" ") rpt_time$
            call "TIME" (rpt_time$)
            pageno% = pageno% + 1%
            if lcnt% <> 99% then print using L55040
            print page
            print using L55040
            print using L55080, date$, rpt_time$, company$, pageno%
                                               /* (EWD070)              */
            if scr_dept$ <> "043" and schema% = 1% then goto PRT_2
            if scr_dept$ <> "002" and scr_dept$ <> "003"   ~
                  and schema% = 2% then goto PRT_2
               print using L55310, bg_date$, beg_wind$, end_wind$
               print using L55120, ed_date$, title$
               print using L55060
               print using L55150
               print using L55190
               lcnt% = 7%
               return
PRT_2:                                        /* (EWD070)              */

            print using L55100, bg_date$
            print using L55120, ed_date$, title$
            print using L55060
            print using L55150
            print using L55190
            lcnt% = 7%
        return

        print_dtl
            if lcnt% > 58% then gosub print_header
            print using L55170, rm_bar$, rm_dept$, rm_seq$, rm_model$,   ~
                               rm_load$, rm_gls$,  rm_mut$, rm_wd$,      ~
                               rm_ht$, rm_so$, str(rm_reason_d$,1%,11%)
            lcnt% = lcnt% + 1%
        return

        Run_Program:
           return% = 0% : comp% = 0%
           init(" ") rlib$, rvol$
           call "PROCLINK" (run$, rlib$, rvol$, return%, comp%)
        return


        create_label_data                         /* (EWD012) EWDGLSXX */
                                                  /* (EWD014)          */
            gl_sort% = gl_sort% + 1%
            convert gl_sort% to gl_sort$, pic(00000)
            gl_process$ = "2"

            if scr_sel% > 2% and scr_sel% < 6% then gl_process$ = "3"
                                                   /*  (AWD074)     */
            if scr_sel% = 7% then gl_process$ = "3"
                                                  /* (EWD031)          */
            if scr_sel% = 5% then gl_process$ = "5"
                                                  /* (EWD031)          */
                                                  /* (AWD075)  */
            if gl_process$ = "2" and str(stock$,1%,6%) = "STOCK1"     ~
                     then gl_process$ = "8"
                                                  /* (AWD082)         */
            if gl_num$ = "000" then str(gl_num$,1%,1%) = num$
            if a11$    = "000" then str(a11$,1%,1%)    = num$
                                                  /* (AWD082)         */

            if field1$ = "4" then a2$ = "BLINDS"    /* (AWD108) */

/* (AWD109) Regular Production and Remake process*/
            if scr_sel$ = "8" then gl_process$ = "2"
            if scr_sel$ = "9" then gl_process$ = "3"

/*(\AWD109) */
                                                  /* (EWD055)  - BEG   */
            if gl_process$ = "2" and tempScrn% = 1% then gl_process$ = "6"
            if gl_process$ = "3" and tempScrn% = 1% then gl_process$ = "7"
                                                  /* (EWD055)  - END   */
            /* (AWD111) */
/* (CR2790) */            
REM  if (scr_sel% = 5% or scr_sel% = 6%) and tempScrn% = 1%   then gl_process$ = "6"
REM  if (scr_sel% = 7% or scr_sel% = 11%) and tempScrn% = 1%  then gl_process$ = "7"

            if tempScrn% <> 1% then goto notTempLabel
               if scr_sel% = 4% then gl_process$ = "6" /* Tempered Precut */
               if scr_sel% = 5% then gl_process$ = "6" /* Patio           */
               if scr_sel% = 6% then gl_process$ = "7" /* Tempered precut rmk */
               if scr_sel% = 7% then gl_process$ = "7" /* Tempered precut rmk */
notTempLabel:            


            if scr_sel% = 12% and lamnScrn% = 1% then gl_process$ = "9"
/* (IM8022) */
            if scr_sel% = 4%  and lamnScrn% = 1% then gl_process$ = "9"

            init(" ") hubcl_key$
            str(hubcl_key$,1,9)  = a14$
            str(hubcl_key$,10,3) = gl_num$

            init(" ") gl_key1$
            str(gl_key1$,1%,9%)  = a14$
            str(gl_key1$,10%,3%) = gl_num$
            read #25,hold,key 1% = gl_key1$, eod goto L63400
               delete #25

L63400:     put #25, using L35050, gl_process$, /* Glass Process Code  */~
                                   scr_dte1$,   /* Production Date     */~
                                   file$,       /* Batch Name          */~
                                   gl_sort$,    /* Sort Number         */~
                                   a14$,        /* Glass Barcode       */~
                                   gl_num$,     /* Glass Remake Number */~
                                   a1$,         /* Glass Type code     */~
                                   a2$,         /* Grid/Liting Code    */~
                                   a3$,         /* Customer Sales Order*/~
                                   a4$,         /* Calculated Width    */~
                                   a5$,         /* Model Code          */~
                                   a7$,         /* Calculated Height   */~
                                   a8$,         /* shift Code          */~
                                   a9$,         /* Production Seq. No. */~
                                   wd$,         /* Actual Wind Wid     */~
                                   a26$,        /* Color, Lock Code    */~
                                   ht$,         /* Actual Wind Height  */~
                                   a12$,        /* Department Code     */~
                                   a13$,        /* Glass Text Line (1) */~
                                   a25$,        /* Glass Text Line (2) */~
                                   a15$,        /* View Top or Bot     */~
                                   a16$,        /* Color               */~
                                   a17$,        /* Contour Grid        */~
                                   a11$,        /* Re-Make No. Prt Ver */~
                                   " ",         /* Stock Flag          */~
                                   a18$,        /* Base       (EWD031) */~
                                   a19$,        /* Left       (EWD031) */~
                                   a20$,        /* Right      (EWD031) */~
                                   a21$,        /* Top        (EWD031) */~
                                   a22$,        /* S1         (EWD031) */~
                                   a23$,        /* Config     (EWD031) */~
                                   a24$,        /* C Seq      (EWD031) */~
                                   a27$,        /* Screen Code         */~
                                   intercept$,  /* Intercept           */~
                                   lowebarcode$,/* Lowe Number (CR2305)*/~
                                   sandwich$    /* Sandwich (CR2305)   */~

            write #25, eod goto L63500
        return
L63500:     errormsg$ = "(Error)Writing Glass Label-" & gl_key1$
            gosub error_prompt
        return


        assign_barcode                          /* (EWD031)            */
            init(" ") readkey$, sh_desc$
            str(readkey$,1%,9%)  = "PLN GLASS"
            str(readkey$,10%,2%) = "00"         /* Store last Barcode  */
            read #3,hold,key = readkey$, using L63700, sh_desc$,         ~
                                                       eod goto L63710
            delete #3
            sh_num$ = str(sh_desc$,26%,7%)      /* Next Barcode        */
            convert sh_num$ to sh_num%, data goto L63710

            sh_num% = sh_num% + 1%              /* Build Next Barcode  */
            convert sh_num% to str(sh_desc$,26%,7%), pic(0000000)

            write #3, using L63700, sh_desc$, eod goto L63710
L63700:       FMT CH(128)

        return
L63710:     errormsg$ = "(Error) - Unable to assign Shape Barcode"
            gosub error_prompt

        return clear all
        goto inputmode

                                                       /* (EWD031)     */
                                                       /* (EWD056)     */
        print_labels_sh
            call "SHOSTAT" ("Printing Special Shape Glass Labels")
            init(" ") wd$, ht$, scr_dte$, scr_dte1$

            scr_dte$ = date            /* Today's Date              */
            date% = 0%
            call "DATEOK" (scr_dte$, date%, errormsg$ )
            scr_dte1$ = scr_dte$

            call "DATUNFMT" (scr_dte1$)

            gl_num$ = "000"

            for kk% = 1% to sh_max%
                sh_qty% = 1%
                sh_qty$ = str(bil$(kk%,5%), 1%,4%)
                convert sh_qty$ to sh_qty%, data goto L63720
L63720:
                for jj% = 1% to sh_qty%
                    gosub assign_barcode
                    a14$ = "P" & sh_num$ & "0"          /* Glass Barcode */
                                                        /* (EWD056)      */
                    gosub build_label_sh                /* Build Label   */
                    gosub print_lab
                next jj%

            next kk%

        return                                          /* (EWD056)      */


                                                      /* (EWD056)     */
        print_labels_online
            call "SHOSTAT" ("Printing Special Shape Glass Labels")
            init(" ") sh_key$, gl_sort$, sh_rec$

                                               /* Read Sorted Work File  */
        print_labels_online_nxt
            read #27,hold,key > sh_key$, using L63730, sh_rec$, sh_key$, ~
                                          eod goto print_labels_online_done
L63730:        FMT CH(256), CH(50)
            gl_sort% = gl_sort% + 1%
            convert gl_sort% to gl_sort$, pic(00000)

            str(sh_rec$,28%,5%) = gl_sort$    /* Seq. Number Assigned    */
                                              /* After Data Sorted       */
            str(sh_rec$,2%,6%)  = str(scr_dte1$,1%,6%)
                                              /* Todays Date Unformatted */
            str(sh_rec$,8%,20%) = file$       /* Batch Name for Day      */

            init(" ") hubcl_key$
            hubcl_key$  = str(sh_rec$,33,12)

                                              /* Check Primary Key  1st  */
            init(" ") gl_key$, gl_key1$
            gl_key$ = str(sh_rec$,1%,32%)
            read #25,hold,key 0% = gl_key$, eod goto L63735
               delete #25
L63735:
                                              /* Check Secendary Key 2nd */
            str(gl_key1$,1%,9%)  = str(sh_rec$,33%,9%) /* Glass Barcode  */
            str(gl_key1$,10%,3%) = str(sh_rec$,42%,3%) /* Remake Number  */

            read #25,hold,key 1% = gl_key1$, eod goto L63740
               delete #25
L63740:

            str(sh_rec$,150,10) = sh_hub$
/* remove @@@  SH_HUB$ = STR(SH_REC$,150%,10%) */

            put #25, using L63750, sh_rec$      /* Special Shapes Label  */
                                                /* Record. After Sort    */
L63750:        FMT CH(256)
                                           /* Create Sorted Shape Labels */
            write #25, eod goto L63760

            goto print_labels_online_nxt
        print_labels_online_done

        return                                          /* (EWD056)      */
L63760:     errormsg$ = "(Error) Writing Special Shape Glass Label-" &    ~
                                                                  gl_key1$
            gosub error_prompt
        return


        update_work_shape                               /* (EWDGLSWW)    */
            if work_shape% = 99% then goto L63800
               mode% = 1% : gosub open_work_shape
               mode% = 3% : gosub open_work_shape
               work_shape% = 99%
L63800:

            gosub save_shape_data

            gosub build_label_sh

                                               /* (EWd060)           */
        REM GOSUB ASSIGN_BARCODE               /* Shape Glass        */
        REM A14$ = "P" & SH_NUM$ & "0"         /* Tempory            */
                                               /* Assign Glass Barcode */
                                               /* (EWD060)           */
            str(a14$,1%,8%) = dt_ref$
            str(a14$,9%,1%) = "0"
                                               /* (EWD060)           */
            init(" ") sh_key$

            sh_sort$ = "00"                    /* Primary Sort Seq   */

            gosub check_shape_glass_sort

            gosub check_shape_grid_sort

            gosub check_shape_temp             /* Tempered Glass at  */
                                               /* the end            */
/* (AWD101) - begin put sq foot grid only below tempered */
            gosub check_sq_footage
            if sq_ft% = 1% then sh_sort$ = "20"
/* (AWD101) - end */

            str(sh_key$,1%,2%)  = sh_sort$     /* Shape Sort Seq     */
            str(sh_key$,3%,3%)  = dt_dept$     /* Production Dept    */
            str(sh_key$,6%,5%)  = dt_seq$      /* Prod. Sequence No. */
            str(sh_key$,11%,9%) = a14$         /* Shape Barcode No.  */

            gl_sort$    = "00000"
            gl_process$ = "5"
            gl_num$ = a11$                     /* Set Re-Make Number */

            if str(dt_part$,11%,1%) = "4" then a11$ = "TSO"   /* CR2124 */
            if str(dt_part$,11%,1%) = "5" then a11$ = "BSO"   /* CR2124 */
            if str(dt_part$,11%,1%) = "6" then a11$ = "FGO"   /* CR2124 */
            if str(dt_part$,11%,1%) = "7" then a11$ = "OGO"   /* CR2124 */

            if gl_num$ = "000" then str(gl_num$,1%,1%) = num$
            if a11$    = "000" then str(a11$,1%,1%)    = num$
                                               /* (AWD082)           */

            read #27,hold,key = sh_key$, eod goto L63810
               delete #27

L63810:
            put #27, using L35060, gl_process$, /* Glass Process Code  */~
                                   scr_dte1$,   /* Production Date     */~
                                   file$,       /* Batch Name          */~
                                   gl_sort$,    /* Sort Number         */~
                                   a14$,        /* Glass Barcode       */~
                                   gl_num$,     /* Glass Remake Number */~
                                   a1$,         /* Glass Type code     */~
                                   a2$,         /* Grid/Liting Code    */~
                                   a3$,         /* Customer Sales Order*/~
                                   a4$,         /* Calculated Width    */~
                                   a5$,         /* Model Code          */~
                                   a7$,         /* Calculated Height   */~
                                   a8$,         /* shift Code          */~
                                   dt_seq$,     /* Prod. Seq. No. (a9$)*/~
                                   wd$,         /* Actual Wind Wid     */~
                                   a26$,        /* Color and Lock Code */~
                                   ht$,         /* Actual Wind Height  */~
                                   a12$,        /* Department Code     */~
                                   a13$,        /* Glass Text Line (1) */~
                                   a25$,        /* Glass Text Line (2) */~
                                   a15$,        /* View Top or Bot     */~
                                   a16$,        /* Color               */~
                                   a17$,        /* Contour Grid        */~
                                   a11$,        /* Re-Make No. Prt Ver */~
                                   " ",         /* Stock Flag          */~
                                   a18$,        /* Base       (EWD031) */~
                                   a19$,        /* Left       (EWD031) */~
                                   a20$,        /* Right      (EWD031) */~
                                   a21$,        /* Top        (EWD031) */~
                                   a22$,        /* S1         (EWD031) */~
                                   a23$,        /* Config     (EWD031) */~
                                   a24$,        /* C Seq      (EWD031) */~
                                   a27$,        /* Screen Code         */~
                                   so_ln$,      /* SO Lne      (AWD085)*/~
                                   sh_key$,     /* Shape Glass Sort    */~
                                   " ",         /* Filler of CH(03)    */~
                             bil$(sh_rec%,31%), /*  GED Shape Code 01  */~
                             bil$(sh_rec%,32%), /*  GED Fields     02  */~
                             bil$(sh_rec%,33%), /*  Width          03  */~
                             bil$(sh_rec%,34%), /*  Height         04  */~
                             bil$(sh_rec%,35%), /*  DimA           05  */~
                             bil$(sh_rec%,36%), /*  DimB           06  */~
                             bil$(sh_rec%,37%), /*  DimC           07  */~
                             bil$(sh_rec%,38%), /*  DimD           08  */~
                             bil$(sh_rec%,39%), /*  DimE           09  */~
                             bil$(sh_rec%,40%), /*  DimF           10  */~
                             bil$(sh_rec%,41%), /*  t_k$           11  */~
                             bil$(sh_rec%,42%), /*  sandwich$      12  */~
                             bil$(sh_rec%,43%), /*  space_d$       13  */~
                             bil$(sh_rec%,44%), /*  gs$            14  */~
                             bil$(sh_rec%,45%), /*  interoffset$   15  */~
                                   dt_part$     /*  Part Number        */

            write #27, eod goto L63820
            shape_counter% = shape_counter% + 1%
            convert shape_counter% to shape_counter$, pic(0000)

        return
L63820:     errormsg$ = "(Error)Writing Shape Glass Work File-" & sh_key$
            gosub error_prompt
        return

/* (AWD101) - begin */
        check_sq_footage
REM           call "SHOSTAT" (" FINDING WIDTH AND HEIGHT " )  stop
           sq_ft%, p% = 0%
           save_width, save_height = 0.00
           init(" ") sh_entry$, flag$, calc$
           sh_entry$ = str(a25$,36%,5%)
           flag$ = "W"
           p% = pos(sh_entry$ = flag$)
           if p% = 0% then return
               gosub get_size
               gosub save_size
               save_width = a
           flag$ = "H"
           p% = pos(sh_entry$ = flag$)
           if p% = 0% then return
               gosub get_size
               gosub save_size
               save_height = a

               sq_ft = 0.00
               sq_ft = round((save_width * save_height) / 144,4)
               if sq_ft > 25 then sq_ft% = 1%
        return

        get_size
              if p% = 1% then calc$ = a18$
              if p% = 2% then calc$ = a19$
              if p% = 3% then calc$ = a20$
              if p% = 4% then calc$ = a21$
              if p% = 5% then calc$ = a22$
        return

        save_size
REM           call "SHOSTAT" (" CONVERTING SIZES " )  stop
           init(" ") a$, b$, c$, d$
           a, b, c, d = 0.00

           a$ = str(calc$,1,3)
           b$ = str(calc$,4,7)

           convert a$ to a, data goto bad_size

           if b$ = " " then return   /* No Fraction */

REM P% = POS(B$ = "/")
REM IF P% = 1% THEN B$ = STR(B$,1,1)
REM IF P% = 1% THEN C$ = STR(B$,3,1)
REM IF P% = 2% THEN B$ = STR(B$,1,2)
REM IF P% = 2% THEN C$ = STR(B$,4,2)

           c$ = str(b$,2,2)
           d$ = str(b$,5,2)

           convert c$ to c, data goto bad_size

           convert d$ to d, data goto bad_size

           b = round(c/d,4)

           a = a + b

bad_size:
           return

/* (AWD101) - end   */



                                                /* (EWD071)            */
        special_shape_remake

        REM    call "SHOSTAT" ("Process being Developed-Not Turned On?")
        REM    stop
        REM    end

            shape_remake% = 1%
            init(" ") scr_dept$, scr_dte1$
            if schema% = 1% then scr_dept$ = "043"     /* Set for Shapes */
REM FT% = 8%                                /* (IM8022) */
            scr_dte1$ = date

            gosub create_rmk_data

            convert ged_cnt% to shape_counter$, pic(###)

        REM call "SHOSTAT" ("Special Shapes Remakes--- " & shape_counter$)
        REM stop

            gosub process_shapes_online

        return clear all
        goto inputmode


        special_create_glass                   /* (APCPLNWK) Work File */
            lab_key$ = all(hex(00))            /* (AWD112) */
            lab_ged$ = all(hex(00))
            read #6,key > lab_ged$, using L63900, lab_ged$, lab_rec$(), ~
                                                   eod goto create_done
            goto L63910
        special_create_next
            read #6, using L63900, lab_key$, lab_ged$, lab_rec$(),   ~
                                            eod goto special_create_done
L63900:       FMT CH(62), CH(66), 2*CH(223)

L63910:     if str(lab_rec$(),183%,3%) <> "043" and schema% = 1%   ~
                      then goto special_create_next
            if str(lab_rec$(),183%,3%) <> "002" and                ~
               str(lab_rec$(),183%,3%) <> "003"and schema% = 2%    ~
                      then goto special_create_next

               gl_num% = 0%
               gl_num$ = str(lab_rec$(),186%,3%)
               convert str(gl_num$,2%,2%) to gl_num%, data goto L63915
L63915:
               gl_num% = gl_num% - 1%
               if gl_num% < 0% then gl_num% = 0%

               convert gl_num% to str(gl_num$,2%,2%), pic(00)

               gosub special_rmk_label

               goto special_create_next

        special_create_done

        return

        special_rmk_label                    /* Find Previous Glass Label*/
            init(" ") hubcl_key$
            hubcl_key$  = str(sh_rec$,33,12)

            init(" ") gl_key1$, sh_rec$      /* (EWDGLSXX) Glass Labels */
                                             /* Build Glass Key         */
            str(gl_key1$,1%,9%)  = str(lab_ged$,53%,9%)
            str(gl_key1$,10%,3%) = gl_num$
            read #25,hold,key 1% = gl_key1$, using L63920, sh_rec$,       ~
                                                           eod goto L63940
               delete #25

L63920:        FMT CH(256)

               str(sh_rec$,2%,6%)  = str(scr_dte1$,1%,6%)  /*SetProdDate */
               str(sh_rec$,8%,20%) = file$                 /*SetBatchName*/
               str(sh_rec$,42%,3%) = str(lab_rec$(),186%,3%) /*SetRmkNum */
               str(sh_rec$,200%,3%)= str(lab_rec$(),186%,3%)


                                           /* Create remake Shape Labels */
/* remove @@@ */ sh_hub$ = str(sh_rec$,150%,10%)

            write #25, using L63920, sh_rec$, eod goto L63930
        return
L63930:     errormsg$ = "(Error) Writing Special Shape Remake Label-" ~
                                                             & gl_key1$
            gosub error_prompt
        return
L63940:     errormsg$ = "(Error) Reading Special Shape Remake Label-" ~
                                                             & gl_key1$
            gosub error_prompt
        return
                                                /* (EWD071)            */

        check_precut                               /*  (AWD075) -  Beg */
            init(" ") descr$, readkey$, stock$, lab_fil$
REM IF STOCK% = 1% THEN RETURN
            precut% = 0%
            gosub lookup_hinge
            if str(descr$,1%,2%) = "OR" then return
            if str(descr$,1%,2%) = "CO" then return
            if str(dt_part$,9%,2%) >= "70" and         ~
                      str(dt_part$,9%,2%) <= "97" then return

            if dt_dept$ = "033" then return                 /* (AWD078) */


            str(readkey$,1%,9%)   = "GLASSPRE1"
            str(readkey$,10%,3%) = str(dt_part$,1%,3%)       /* Model    */
            str(readkey$,13%,2%) = str(dt_part$,5%,2%)       /* gls code */
            str(readkey$,15%,4%) = str(dt_part$,13%,4%)  /*widthOfWindow */
            str(readkey$,19%,3%) = str(dt_part$,17%,3%)  /*heightOfWindow*/

            read #3,key = readkey$,using L61131, descr$,eod goto no_precut

REM     call "SHOSTAT" (" I am here" & readkey$ )  stop
/*(AWD103) */
REM     STOCK$, STR(LAB_FIL$,1%,6%) = "STOCK1"
                                   /* Note !  1 on end is special  */
/* Change back to stock not stock1 (AWD103) */
                 stock$, str(lab_fil$,1%,6%) = "STOCK"
                 gosub lookup_sandwich
                 precut% = 1%          /* It is precut! */
        no_precut
        return

        lookup_sandwich
            init(" ") descr$, readkey$
            str(readkey$,1%,9%)   = "GLASSPRE2"
            str(readkey$,10%,10%) = sandwich$
            read #3,key = readkey$,using L61131, descr$,                 ~
                                                      eod goto no_sandwich
                  sandwich$ = str(descr$,1%,10%)

        no_sandwich
        return                                       /*  (AWD075) -  Beg */


        lookup_ups                                         /* (AWD080) */
             init(" ") readkey$
             str(readkey$,1%,9%)  = "PLAN UPS "
             str(readkey$,10%,2%) = or_hows$
             read #3, key = readkey$, eod goto no_ups
                   rm_num$ = "UPS"
        no_ups
        return                                             /* (AWD080) */


        lookup_sub_part                              /* (AWD083) - BEG */
             init(" ") flag$, pgm$, bcksubpt_rec$, flds$(), info_flds$(), ~
                       series$, style$
             dim1es, dim2es, dim3es = 0.00        /* (AWD121)          */
               so_inv%, item_no%, suberr1% = 0%

               flag$ = "0"
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

/* (AWD121) */
            get bcksubpt_rec$ using dimFmt, dim1es, dim2es
dimFMT:               FMT POS(153), PD(14,4), PD(14,4)


            if suberr1% = 0% then return



            str(bcksubpt_rec$,48%,20%) = "00000000000000000000"
            errormsg$ = "AWDBKSUB ERROR = "&so_inv$ & " Line= " & item_no$
            gosub error_prompt
            suberr1% = 0%

        return

REM !PACK_SORT_CODE
REM !PUT STR(SRT$,1,2), USING L63510, SORT%

REM !SS$ = SRT$
REM !RETURN

REM !UNPACK_SORT_CODE
REM !GET STR(SRT$,1,2), USING L63510, SORT%
REM ! L63510:  REM !           FMT BI(2)
REM !CONVERT SORT% TO SRT$, PIC(00)
REM !RETURN



/* (AWD110) */
        checkKanbanpt
         kanban% = 0%
                  /* BBG Door Is Always Kanban */
         if str(sub_part$,1%,1%) = "4" then goto setKanban
         str(kanbanptkey1$, 1%,8%)  = str(dt_part$,1,8)
         str(kanbanptkey1$, 9%,4%)  = "0000"
         str(kanbanptkey1$,13%,12%) = str(dt_part$,13,12)
         str(kanbanptkey1$,26%,20%) = "00000000000000000000"
         str(kanbanptkey1$,26%,3%)  = str(sub_part$,1,3)

         read #50, key 1% = kanbanptkey1$, eod goto noKanban

setKanban:
                kanban% = 1%
        noKanban
        return
/* (AWD110) */
/* (AWD112) */
        lookup_intercept
/* (AWD117) */
          if str(sub_part$,17%,1%) <> "0" then goto subpart_intercept
          init(" ") readkey$, descr$, intercept$
          intercept% = 1%
          str(readkey$,1,9)  = "INTERCEPT"
          str(readkey$,10,3) = model$
          str(readkey$,13,2) = ty$ 

           read #3, key = readkey$, using L61131, descr$,  ~
                                   eod goto no_intercept_glass

               convert str(descr$,1,2) to intercept%,               ~
                                          data goto intercept_done
           goto intercept_done

        no_intercept_glass
          str(readkey$,1,9)  = "INTERCEPT"
          str(readkey$,10,3) = model$
          str(readkey$,13,2) = str(ty$,1,1) & "*"

           read #3, key = readkey$, using L61131, descr$,  ~
                                   eod goto no_intercept_all

               convert str(descr$,1,2) to intercept%,               ~
                                             data goto intercept_done
           goto intercept_done

         no_intercept_all
           init(" ") readkey$
           str(readkey$,1,9)  = "INTERCEPT"
           str(readkey$,10,3) = model$
           str(readkey$,13,2) = "**"

           read #3, key = readkey$, using L61131, descr$,  ~
                                   eod goto intercept_done

               convert str(descr$,1,2) to intercept%,               ~
                                              data goto intercept_done

        intercept_done
          convert intercept% to intercept$,pic(00)
        return
/* (AWD117) */
        subpart_intercept
          convert str(sub_part$,17%,1%) to intercept%,             ~
                                       data goto badsubpart_intercept
          goto intercept_done
        badsubpart_intercept
         str(sub_part$,17%,1%) = "0"
         goto lookup_intercept
/* (\AWD117) */
/* (AWD112) end */
        checkAWDSCHGL
            init(" ") gl_order$, gl_order_seq$, gl_sandwich$,            ~
                gl_intercept$, gl_spc_type$, gl_car_rack$,               ~
                gl_car_gls_type$(), gl_spc_desc$, gl_spc_thick$, gl_wd1$,~
                 gl_ht1$, gl_barcode$, gl_err$, gl_ord_gls$

            awdschgl% = 0%
            
            if hldsched% = 0% then return

REM AWDSCHGL_KEY0$ = DT_REF$
REM CONVERT (CT% - 1%) TO STR(AWDSCHGL_KEY0$,9%,1%), PIC(#)

            read #55, key 0% = awdschgl_key0$, eod goto checkAWDSCHGL_DONE

                get #55, using AWDSCHGL_FMT, gl_order$,            ~
                                             gl_order_seq$,        ~
                                             gl_sandwich$,         ~
                                             gl_intercept$,        ~
                                             gl_spc_type$,         ~
                                             gl_car_rack$,         ~
                                             gl_car_gls_type$(),   ~
                                             gl_spc_desc$,         ~
                                             gl_spc_thick$,        ~
                                             gl_wd1$,              ~
                                             gl_ht1$,              ~
                                             gl_barcode$,          ~
                                             gl_err$,              ~
                                             gl_ord_gls$,          ~
                                             gl_gls$

REM !!IF GL_ORD_GLS$ = "0" THEN GOTO CHECKAWDSCHGL_DONE
                  awdschgl% = 1%
                  gls_double% = 1%                    /* Has to be DS */

        checkAWDSCHGL_DONE
        return

AWDSCHGL_FMT:  FMT CH(05), CH(05), XX(09), CH(20), CH(02), CH(03), CH(03),~
                 2*CH(30), CH(10), CH(10), XX(25), CH(10), CH(10), XX(08),~
                   XX(08), CH(18), CH(03), CH(01), CH(02)

/* (AWD116) */
        load_interdesc
          init(" ") readkey$, desc$, interdesc$()
          mat interoffset = zer
          mat interadj = zer
          str(readkey$,1,9) = "INTERDESC"
        interdesc_next
          read #3, key > readkey$, using L61965, readkey$, desc$, ~
                                              eod goto interdesc_done
            if str(readkey$,1,9) <> "INTERDESC" then goto interdesc_done

            intercept% = 99%
            convert str(readkey$,10,2) to intercept%,      ~
                                  data goto interdesc_done

            if intercept% > 99% then goto interdesc_error

           interdesc$(intercept%) = str(desc$,1,5)
           convert str(desc$,12%,8%) to interoffset(intercept%),   ~
                                             data goto interdesc_error

           convert str(desc$,22%,8%) to interadj(intercept%),      ~
                                             data goto interdesc_error
           goto interdesc_next

         interdesc_done
         return
         interdesc_error
          errormsg$ = "INTERDESC ERROR = Error Loading data from INTERDESC"
          gosub error_prompt
         goto exit_program

/* (\AWD116) */
        checkobsgls
           init(" ") readkey$
           obs% = 0%
           str(readkey$,1%,9%)   = "OBS GED  "
           str(readkey$,10%,15%) = ty$
           read #3,key = readkey$, eod goto noobsgls
              obs% = 1%
              double% = 1%
        noobsgls
        return

/* (AWD117) */
        lookup_triplepane
         tripane% = 0%
         init(" ") readkey$
         str(readkey$,1%,9%)   = "PLANTRIPL"
         str(readkey$,10%,15%) = ty$
         read #3,key = readkey$, eod goto notriplepane
              tripane% = 1%
        notriplepane
        return
/* (\AWD117) */

/* (AWD118) */
        check_valance
         valance% = 0%
         init(" ") readkey$
         str(readkey$,1%,9%)   = "VALANCE  "
         str(readkey$,10%,15%) = lt$
         read #3,key = readkey$, eod goto novalance
              valance% = 1%
        novalance
        return
/* (\AWD118) */
        lookupSeries
          call "AWDSERST" (str(dt_part$,1%,3%), series$, style$, #3, err%)

          if err% = 0% then return

          series$, style$ = "UNKNOWN"
         return

/* + (CR1990) */
        lookup_ultra
            init(" ") readkey$, ultra$
            str(readkey$,1%,9%)  = "PLANULTRA"      /* ALL GLASS By Model */
            str(readkey$,10%,5%) = str(dt_part$,1,3) & "**"
            read #3,key = readkey$, eod goto lookup_ultra_gls
               ultra$ = "1"
        return
        lookup_ultra_gls
            str(readkey$,1%,9%)  = "PLANULTRA"    /* Specific Glass */
            str(readkey$,10%,5%) = str(dt_part$,1,3) & str(dt_part$,5,2)
            read #3,key = readkey$, eod goto ultra_done
               ultra$ = "1"
        ultra_done
        return
/* - (CR1990) */

/* (AWD122) */
        lookup_hldsched
           init(" ") hldkey2$
           hldsched%, sgp% = 0%
           str(hldkey2$,1%,10%) = str(dt_bar$,1%,10%)
        hldschedNext
           read #57, key 2% > hldkey2$, using HLDFMT, hldkey2$, hld_lam$, ~
                                                       eod goto noHldsched
HLDFMT:            FMT POS(27), CH(16), POS(114), CH(01)
              if str(hldkey2$,1%,10%) <> str(dt_bar$,1%,10%) then goto ~
                        noHldsched
              hldsched% = 1%
/* hldkey2$,16,1 = sp_type, sp_type = B means laminate */
              if str(hldkey2$,16%,1%) <> "B" then goto hldschedNext
               if hld_lam$ = "S" then sgp% = 1%
               
        noHldsched
        return
/* (\AWD122) */

/* (CR503) + */
        check_argon                      /* Glass Code for Argon */
          init(" ") readkey$, gs$
          str(readkey$,1%,9%)   = "PLANARGON"
          str(readkey$,10%,15%) = ty$
          read #3,key = readkey$, eod goto noArgon
             gs$ = "G1"
        noArgon
        return
/* (CR503) - */

/* (CR838) */
        genRead
          init(" ") codeLen$, descr1$, descr2$, descr3$
          generr% = 0%
          call "GENREAD" (table$, genkey$, descr1$, codeLen$,       ~
                                         descr2$, descr3$, generr%)
        return
/* (CR838) - */
/* + (CR2305) */
        lookup_lowebarcode
REM   CALL "SHOSTAT" ("LOOK UP LOWE " & SANDWICH$ ) STOP
          init(" ") lowebarcode$, ig$, igcode$()
          ig% = 0%
          lowe% = 0%
          lowebarcode$ = "0000"
          igcode$(1%) = str(sandwich$,3%,3%)
          igcode$(2%) = str(sandwich$,6%,3%)
          igcode$(3%) = str(sandwich$,9%,3%)

          table$ = "PLAN SAND"
          for g% = 1% to 3%
            ig$ = str(igcode$(g%),2%,2%)
            genkey$ = ig$
            gosub genRead
            if generr% <> 0% then badIG
            convert str(descr1$,20%,10%) to ig%, data goto badIG

badIG:
            if ig% > lowe% then lowe% = ig%
          next g%

          convert lowe% to lowebarcode$, pic(0000)
        return
/* - (CR2305) */
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" (#6)
            end




load_sort:
* (AWD088) Next 3 lines
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #3, schema_err%)

                                                  /* (EWD004) - Begin    */
                                                  /* (AWD079) - Begin    */
            if schema% <> 1% then goto not_nc_sort        /* (AWD088) */
            ss_max% = 999%
            gosub loadSORTGLASS
            gosub loadSORTTEMP


return

            sdl_index%  = 103%
            lamn_index% = 180%
            tpl_index%  = 150%

REM SPACER 17
            ss$( 1%)  = "008DSSP17TB"
            ss$( 2%)  = "048DSSP17TB"
            ss$( 3%)  = "023DSSP17TT"
            ss$( 4%)  = "023DSSP17BB"
            ss$( 5%)  = "020DSSP17TT"
            ss$( 6%)  = "020DSSP17BB"
            ss$( 7%)  = "033DSSP17TB"
            ss$( 9%)  = "064DSSP17TT"
            ss$(10%)  = "064DSSP17BB"
            ss$(11%)  = "047DSSP17TB"

REM SPACER 19
            ss$(12%)  = "006DSSP19TT"
            ss$(13%)  = "006DSSP19BB"

REM SPACER 15 Green Line Batch
            ss$(20%)  = "055DSSP15TB"
            ss$(21%)  = "033DSSP15TB"
            ss$(22%)  = "008DSSP15TB"
            ss$(23%)  = "064DSSP15TT"
            ss$(24%)  = "064DSSP15BB"
            ss$(25%)  = "047DSSP15TB"


REM SPACER 15 This was moved here to group 002 with 007,049,005&019
REM  -blue line batch
            ss$(30%)  = "005DSSP15TT"
            ss$(31%)  = "005DSSP15BB"
            ss$(32%)  = "049DSSP15TT"
            ss$(33%)  = "049DSSP15BB"

            ss$(34%)  = "002DSSP15TB"
            ss$(35%)  = "014DSSP15TB"
            ss$(36%)  = "017DSSP15TB"
            ss$(37%)  = "018DSSP15TB"



REM SPACER 15 Group 048,036,027,028,053     - handtop batch
            ss$(40%)  = "053DSSP15TT"
            ss$(41%)  = "019DSSP15TT"
            ss$(42%)  = "025DSSP15TT"     /* (IM7733) */
            ss$(43%)  = "048DSSP15TT"
            ss$(44%)  = "036DSSP15TT"
            ss$(45%)  = "027DSSP15TT"
            ss$(46%)  = "028DSSP15TT"

            ss$(47%)  = "053DSSP15BB"
            ss$(48%)  = "019DSSP15BB"
            ss$(49%)  = "025DSSP15BB"     /* (IM7733) */
            ss$(50%)  = "048DSSP15BB"
            ss$(51%)  = "036DSSP15BB"
            ss$(52%)  = "027DSSP15BB"
            ss$(53%)  = "028DSSP15BB"


REM SPACER 11
            ss$(55%)  = "036DSSP11TT"
            ss$(56%)  = "036DSSP11BB"
            ss$(57%)  = "033DSSP11TB"
            ss$(58%)  = "047DSSP11TB"

REM SPACER 25
            ss$(59%)  = "033DSSP25TB"




/* Single Strength SS */
            ss$(60%)  = "008SSSP17TB"
            ss$(61%)  = "048SSSP17TT"
            ss$(62%)  = "048SSSP17BB"
            ss$(63%)  = "064SSSP19TT"
            ss$(64%)  = "064SSSP19BB"
            ss$(65%)  = "064SSSP17TT"
            ss$(66%)  = "064SSSP17BB"
            ss$(67%)  = "049SSSP17TT"
            ss$(68%)  = "049SSSP17BB"
            ss$(69%)  = "014SSSP17TB"
            ss$(70%)  = "002SSSP17TB"
            ss$(71%)  = "055SSSP17TB"
            ss$(75%)  = "053SSSP17TT"
            ss$(76%)  = "053SSSP17BB"
            ss$(80%)  = "008SSSP19TB"
            ss$(81%)  = "048SSSP19TB"
            ss$(82%)  = "017SSSP17TB"
            ss$(83%)  = "018SSSP17TB"
            ss$(84%)  = "019SSSP17TT"
            ss$(85%)  = "019SSSP17BB"
            ss$(86%)  = "025SSSP17TT"     /* (IM7733) */
            ss$(87%)  = "025SSSP17BB"     /* (IM7733) */
            ss$(89%)  = "005SSSP17TT"
            ss$(90%)  = "005SSSP17BB"
            ss$(91%)  = "036SSSP17TT"
            ss$(92%)  = "036SSSP17BB"
            ss$(93%)  = "053SSSP17TT"
            ss$(94%)  = "053SSSP17BB"
REM this is for 716 BSO even though it is really TT BB going to process as
REM TB so awdpln16 will process the BSOs correctly
            ss$(96%)  = "026SSSP17TT"
            ss$(97%)  = "026SSSP17BB"
            ss$(98%)  = "025SSSP17BB"     /* (IM7733) */
            ss$(99%)  = "027SSSP17TT"
            ss$(100%) = "027SSSP17BB"
            ss$(101%) = "028SSSP17TT"
            ss$(102%) = "028SSSP17BB"

/* sdl !!!! if beginning of SDL changes then change */
            ss$(103%) = "017DSSP11TB"
            ss$(104%) = "018DSSP11TB"
            ss$(105%) = "002DSSP11TB"
            ss$(106%) = "048DSSP11TB"
            ss$(107%) = "055DSSP11TB"
            ss$(108%) = "036DSSP11TT"
            ss$(109%) = "036DSSP11BB"
            ss$(110%) = "019DSSP11TT"
            ss$(111%) = "019DSSP11BB"
            ss$(112%) = "025DSSP11TT"     /* (IM7733) */
            ss$(113%) = "025DSSP11BB"     /* (IM7733) */
            ss$(114%) = "027DSSP11TT"
            ss$(115%) = "027DSSP11BB"
            ss$(116%) = "028DSSP11TT"
            ss$(117%) = "028DSSP11BB"
            ss$(118%) = "036SSSP13TT"
            ss$(119%) = "036SSSP13BB"
            ss$(120%) = "047DSSP13TB"
            ss$(121%) = "006DSSP15TT"
            ss$(122%) = "006DSSP15BB"
            ss$(123%) = "006DSSP09TT"
            ss$(124%) = "006DSSP09BB"
            ss$(125%) = "047DSSP09TB"
            ss$(130%) = "014DSSP17TB"
            ss$(131%) = "017SSSP13TB"
            ss$(132%) = "018SSSP13TB"
            ss$(133%) = "002SSSP13TB"
            ss$(134%) = "048SSSP13TB"
            ss$(135%) = "055SSSP13TB"
            ss$(136%) = "019SSSP13TT"
            ss$(137%) = "019SSSP13BB"
            ss$(138%) = "025SSSP13TT"     /* (IM7733) */
            ss$(139%) = "025SSSP13BB"     /* (IM7733) */
            ss$(140%) = "027SSSP13TT"
            ss$(141%) = "027SSSP13BB"
            ss$(142%) = "028SSSP13TT"
            ss$(143%) = "028SSSP13BB"


/* 3P !!!! if beginning of Triple Glaze changes then change index% = 150%*/
            ss$(150%) = "047DSSP15TB"
            ss$(151%) = "014DSSP15TB"
            ss$(152%) = "014SSSP17TB"
            ss$(154%) = "104SSSP19TB"
            ss$(155%) = "104DSSP17TB"
            ss$(156%) = "104SSSP13TB"
            ss$(157%) = "043SSSP19TB"
            ss$(158%) = "043DSSP17TB"
            ss$(159%) = "043SSSP13TB"
            ss$(160%) = "043DSSP11TB"
            ss$(161%) = "043SSSP17TB"
            ss$(162%) = "043DSSP15TB"
            ss$(165%) = "033SSSP15TT"
            ss$(166%) = "033SSSP15BB"
            ss$(167%) = "004DSSP15TT"
            ss$(168%) = "004DSSP15BB"
            ss$(169%) = "014DSSP15TT"
            ss$(170%) = "014DSSP15BB"
            ss$(173%) = "004SSSP17TT"
            ss$(174%) = "004SSSP17BB"
            ss$(175%) = "014SSSP17TT"
            ss$(176%) = "014SSSP17BB"


/* Laminate Glas !! index = 180%  */
            ss$(180%) = "014DSSP19TB"   /* !!! CMG !!! 01-19-2015 */
            ss$(181%) = "014DSSP17TB"
            ss$(182%) = "014DSSP15TB"
            ss$(183%) = "006DSSP13TT"
            ss$(184%) = "006DSSP13BB"
            ss$(185%) = "006DSSP11TT"
            ss$(186%) = "006DSSP11BB"
            ss$(187%) = "043DSSP19TB"
            ss$(188%) = "047DSSP19TB"
            ss$(189%) = "047DSSP13TB"
            ss$(190%) = "047DSSP11TB"
            ss$(191%) = "006DSSP09TT"
            ss$(192%) = "006DSSP09BB"
            ss$(193%) = "047DSSP09TB"  /* !!! CMG !!! 01-19-2015 */


REM *****     Put all Odd Stuff at Bottom
REM
            ss$(900%) = "043Tempered"
REM            SS$(901%) = "025TEMPERED"       /* (IM7733) */
            ss$(902%) = "042Tempered"
            ss$(903%) = "023Tempered"
            ss$(904%) = "997Tempered"
            ss$(950%) = "998Obsecure"
            ss$(951%) = "998Diamond "
            ss$(998%) = "998Stock   "
            ss$(999%) = "999Errors  "



                                               /* (EWD004) - End   */
                                               /* (EWD017) - End   */
                                               /* (EWD055) - BEG   */

REM Beginning of Texas Sort                       (AWD088)

not_nc_sort:

            ss_max% = 999%
            gosub loadSORTGLASS
            gosub loadSORTTEMP


return

            sdl_index%  = 97%
            lamn_index% = 130%
            tpl_index%  = 1%    /* NTX doesn't have triple default to 1%*/

            ss$( 1%)  = "029DSSP09TT"
            ss$( 2%)  = "029DSSP09BB"
            ss$( 3%)  = "030DSSP09TB"
            ss$( 4%)  = "031DSSP09TB"
            ss$( 5%)  = "032DSSP09TB"
            ss$( 6%)  = "033DSSP09TB"
            ss$( 7%)  = "070DSSP09TB"
            ss$( 8%)  = "033DSSP13TB"
            ss$( 9%)  = "071DSSP13TT"
            ss$(10%)  = "071DSSP13BB"
            ss$(11%)  = "033DSSP11TB"
            ss$(12%)  = "030DSSP11TB"
            ss$(13%)  = "031DSSP11TB"
            ss$(14%)  = "032DSSP11TB"
            ss$(15%)  = "004DSSP15TB"
            ss$(17%)  = "070DSSP15TB"
            ss$(18%)  = "012DSSP15TB"
            ss$(19%)  = "013DSSP15TB"
            ss$(20%)  = "045DSSP15TB"
            ss$(21%)  = "033DSSP15TB"
            ss$(25%)  = "057DSSP15TB"
            ss$(26%)  = "058DSSP15TB"
            ss$(27%)  = "059DSSP15TB"
            ss$(33%)  = "015DSSP15TB"
            ss$(34%)  = "016DSSP15TB"
            ss$(35%)  = "071DSSP15TT"
            ss$(36%)  = "071DSSP15TT"
            ss$(37%)  = "070DSSP19TB"
            ss$(38%)  = "033DSSP19TB"
            ss$(39%)  = "071DSSP19TT"
            ss$(40%)  = "071DSSP19BB"
            ss$(44%)  = "040DSSP11TB"
            ss$(45%)  = "041DSSP17TB"
            ss$(46%)  = "045DSSP17TB"

/* Single Strength SS */
            ss$(47%)  = "033SSSP13TB"
            ss$(48%)  = "030SSSP13TB"
            ss$(49%)  = "031SSSP13TB"
            ss$(50%)  = "032SSSP13TB"
            ss$(51%)  = "071SSSP13TT"
            ss$(52%)  = "071SSSP13BB"
            ss$(53%)  = "033SSSP11TB"
            ss$(54%)  = "029SSSP11TT"
            ss$(55%)  = "029SSSP11BB"
            ss$(56%)  = "030SSSP11TB"
            ss$(57%)  = "031SSSP11TB"
            ss$(58%)  = "032SSSP11TB"
            ss$(66%)  = "033SSSP17TB"
            ss$(67%)  = "012SSSP17TB"
            ss$(68%)  = "013SSSP17TB"
            ss$(69%)  = "045SSSP17TB"
            ss$(70%)  = "070SSSP17TB"
            ss$(81%)  = "057SSSP17TB"
            ss$(82%)  = "058SSSP17TB"
            ss$(83%)  = "059SSSP17TB"
            ss$(92%)  = "015SSSP17TB"
            ss$(93%)  = "016SSSP17TB"
            ss$(94%)  = "041SSSP19TB"
            ss$(95%)  = "045SSSP19TB"
            ss$(96%)  = "041SSSP15TB"

/*(AWD099) sdl !!!! if beginning of SDL changes then change index% = 100%*/
            ss$(97%)  = "041DSSP15TB"
            ss$(100%) = "045DSSP11TB"
            ss$(102%) = "015DSSP11TB"
            ss$(103%) = "045DSSP15TB"
            ss$(104%) = "012DSSP15TB"
            ss$(105%) = "013DSSP15TB"
            ss$(106%) = "004DSSP15TB"

            ss$(110%) = "045SSSP13TB"
            ss$(111%) = "015SSSP13TB"
            ss$(113%) = "045SSSP17TB"
            ss$(114%) = "012SSSP17TB"
            ss$(115%) = "013SSSP17TB"

/* Laminate Glas !! index = 130%  */
            ss$(130%) = "041DSSP17TB"
            ss$(131%) = "070DSSP09TB"
            ss$(132%) = "071DSSP13TT"
            ss$(133%) = "071DSSP13BB"
            ss$(134%) = "033DSSP13TB"
            ss$(135%) = "033DSSP09TB"
            ss$(136%) = "033DSSP17TB"
            ss$(137%) = "070DSSP15TB"
            ss$(138%) = "071DSSP11TT"
            ss$(139%) = "071DSSP11BB"
            ss$(140%) = "033DSSP19TB"
            ss$(141%) = "071DSSP19TT"
            ss$(142%) = "071DSSP19BB"


REM *****     Put ALL Odd Stuff at Bottom    *****
            ss$(150%) = "104SSSP17TB"
            ss$(151%) = "104DSSP15TB"
            ss$(152%) = "104SSSP13TB"

            ss$(153%) = "002DSSP17TB"
            ss$(154%) = "002DSSP15TB"
            ss$(155%) = "002DSSP15TB"
            ss$(156%) = "002DSSP13TB"
            ss$(157%) = "002DSSP11TB"
            ss$(158%) = "002DSSP09TB"

            ss$(160%) = "003SSSP17TB"
            ss$(161%) = "003DSSP15TB"
            ss$(162%) = "003SSSP15TB"
            ss$(163%) = "003DSSP13TB"
            ss$(164%) = "003SSSP11TB"
            ss$(165%) = "003DSSP09TB"
            ss$(166%) = "003DSSP17TB"
            ss$(167%) = "003SSSP19TB"
            ss$(168%) = "003SSSP25TB"
            ss$(169%) = "003DSSP25TB"
            ss$(170%) = "003DSSP23TB"
            ss$(171%) = "003DSSP19TB"

/* (/AWD105) */
REM *****     Put all Odd Stuff at Bottom            (EWD043)  END
REM                                                     (EWD057)
            ss$(900%) = "002Tempered"
            ss$(901%) = "003Tempered"
            ss$(902%) = "040Tempered"
            ss$(903%) = "041Tempered"
            ss$(904%) = "997Tempered"
            ss$(950%) = "998Obsecure"
            ss$(951%) = "998Diamond "
            ss$(998%) = "998Stock   "
            ss$(999%) = "999Errors  "

            ss_max% = 999%

REM            GOSUB LOADTEMPSORT
return


        getTempSort
REM             RACK = 0 OR 1 BY RACK SIZE, SPACER, STRENGTH, and Department
            ss_temp$(1%)   = "0010SP09DS"    /* (IM8011) 2 in each group */
            ss_temp$(2%)   = "0010SP11DS"
            ss_temp$(3%)   = "0010SP13DS"
            ss_temp$(4%)   = "0010SP15DS"
            ss_temp$(5%)   = "0010SP17DS"
            ss_temp$(6%)   = "0010SP19DS"
            ss_temp$(7%)   = "0011SP09DS"  /* (IM8011) goes through to   */
            ss_temp$(8%)   = "0011SP11DS"  /* bottom of temp sort...     */
            ss_temp$(9%)   = "0011SP13DS"
            ss_temp$(10%)  = "0011SP15DS"
            ss_temp$(11%)  = "0011SP17DS"
            ss_temp$(12%)  = "0011SP19DS"

            ss_temp$(21%)  = "0020SP09DS"
            ss_temp$(22%)  = "0020SP11DS"
            ss_temp$(23%)  = "0020SP13DS"
            ss_temp$(24%)  = "0020SP15DS"
            ss_temp$(25%)  = "0020SP17DS"
            ss_temp$(26%)  = "0020SP19DS"
            ss_temp$(27%)  = "0021SP09DS"
            ss_temp$(28%)  = "0021SP11DS"
            ss_temp$(29%)  = "0021SP13DS"
            ss_temp$(30%)  = "0021SP15DS"
            ss_temp$(31%)  = "0021SP17DS"
            ss_temp$(32%)  = "0021SP19DS"

            ss_temp$(41%)  = "0030SP09DS"
            ss_temp$(42%)  = "0030SP11DS"
            ss_temp$(43%)  = "0030SP13DS"
            ss_temp$(44%)  = "0030SP15DS"
            ss_temp$(45%)  = "0030SP17DS"
            ss_temp$(46%)  = "0030SP19DS"
            ss_temp$(47%)  = "0031SP09DS"
            ss_temp$(48%)  = "0031SP11DS"
            ss_temp$(49%)  = "0031SP13DS"
            ss_temp$(50%)  = "0031SP15DS"
            ss_temp$(51%)  = "0031SP17DS"
            ss_temp$(52%)  = "0031SP19DS"

            ss_temp$(61%)  = "0040SP09DS"
            ss_temp$(62%)  = "0040SP11DS"
            ss_temp$(63%)  = "0040SP13DS"
            ss_temp$(64%)  = "0040SP15DS"
            ss_temp$(65%)  = "0040SP17DS"
            ss_temp$(66%)  = "0040SP19DS"
            ss_temp$(67%)  = "0041SP09DS"
            ss_temp$(68%)  = "0041SP11DS"
            ss_temp$(69%)  = "0041SP13DS"
            ss_temp$(70%)  = "0041SP15DS"
            ss_temp$(71%)  = "0041SP17DS"
            ss_temp$(72%)  = "0041SP19DS"

            ss_temp$(81%)  = "0050SP09DS"
            ss_temp$(82%)  = "0050SP11DS"
            ss_temp$(83%)  = "0050SP13DS"
            ss_temp$(84%)  = "0050SP15DS"
            ss_temp$(85%)  = "0050SP17DS"
            ss_temp$(86%)  = "0050SP19DS"
            ss_temp$(87%)  = "0051SP09DS"
            ss_temp$(88%)  = "0051SP11DS"
            ss_temp$(89%)  = "0051SP13DS"
            ss_temp$(90%)  = "0051SP15DS"
            ss_temp$(91%)  = "0051SP17DS"
            ss_temp$(92%)  = "0051SP19DS"

            ss_temp$(101%) = "0060SP09DS"
            ss_temp$(102%) = "0060SP11DS"
            ss_temp$(103%) = "0060SP13DS"
            ss_temp$(104%) = "0060SP15DS"
            ss_temp$(105%) = "0060SP17DS"
            ss_temp$(106%) = "0060SP19DS"
            ss_temp$(107%) = "0061SP09DS"
            ss_temp$(108%) = "0061SP11DS"
            ss_temp$(109%) = "0061SP13DS"
            ss_temp$(110%) = "0061SP15DS"
            ss_temp$(111%) = "0061SP17DS"
            ss_temp$(112%) = "0061SP19DS"

            ss_temp$(121%) = "0070SP09DS"
            ss_temp$(122%) = "0070SP11DS"
            ss_temp$(123%) = "0070SP13DS"
            ss_temp$(124%) = "0070SP15DS"
            ss_temp$(125%) = "0070SP17DS"
            ss_temp$(126%) = "0070SP19DS"
            ss_temp$(127%) = "0071SP09DS"
            ss_temp$(128%) = "0071SP11DS"
            ss_temp$(129%) = "0071SP13DS"
            ss_temp$(130%) = "0071SP15DS"
            ss_temp$(131%) = "0071SP17DS"
            ss_temp$(132%) = "0071SP19DS"

            ss_temp$(141%) = "0080SP09DS"
            ss_temp$(142%) = "0080SP11DS"
            ss_temp$(143%) = "0080SP13DS"
            ss_temp$(144%) = "0080SP15DS"
            ss_temp$(145%) = "0080SP17DS"
            ss_temp$(146%) = "0080SP19DS"
            ss_temp$(147%) = "0081SP09DS"
            ss_temp$(148%) = "0081SP11DS"
            ss_temp$(149%) = "0081SP13DS"
            ss_temp$(150%) = "0081SP15DS"
            ss_temp$(151%) = "0081SP17DS"
            ss_temp$(152%) = "0081SP19DS"


            ss_temp$(161%) = "0090SP09DS"
            ss_temp$(162%) = "0090SP11DS"
            ss_temp$(163%) = "0090SP13DS"
            ss_temp$(164%) = "0090SP15DS"
            ss_temp$(165%) = "0090SP17DS"
            ss_temp$(166%) = "0090SP19DS"
            ss_temp$(167%) = "0091SP09DS"
            ss_temp$(168%) = "0091SP11DS"
            ss_temp$(169%) = "0091SP13DS"
            ss_temp$(170%) = "0091SP15DS"
            ss_temp$(171%) = "0091SP17DS"
            ss_temp$(172%) = "0091SP19DS"

            ss_temp$(181%) = "0100SP09DS"
            ss_temp$(182%) = "0100SP11DS"
            ss_temp$(183%) = "0100SP13DS"
            ss_temp$(184%) = "0100SP15DS"
            ss_temp$(185%) = "0100SP17DS"
            ss_temp$(186%) = "0100SP19DS"
            ss_temp$(187%) = "0101SP09DS"
            ss_temp$(188%) = "0101SP11DS"
            ss_temp$(189%) = "0101SP13DS"
            ss_temp$(190%) = "0101SP15DS"
            ss_temp$(191%) = "0101SP17DS"
            ss_temp$(192%) = "0101SP19DS"

            ss_temp$(201%) = "0110SP09DS"
            ss_temp$(202%) = "0110SP11DS"
            ss_temp$(203%) = "0110SP13DS"
            ss_temp$(204%) = "0110SP15DS"
            ss_temp$(205%) = "0110SP17DS"
            ss_temp$(206%) = "0110SP19DS"
            ss_temp$(207%) = "0111SP09DS"
            ss_temp$(208%) = "0111SP11DS"
            ss_temp$(209%) = "0111SP13DS"
            ss_temp$(210%) = "0111SP15DS"
            ss_temp$(211%) = "0111SP17DS"
            ss_temp$(212%) = "0111SP19DS"

            ss_temp$(221%) = "0120SP09DS"
            ss_temp$(222%) = "0120SP11DS"
            ss_temp$(223%) = "0120SP13DS"
            ss_temp$(224%) = "0120SP15DS"
            ss_temp$(225%) = "0120SP17DS"
            ss_temp$(226%) = "0120SP19DS"
            ss_temp$(227%) = "0121SP09DS"
            ss_temp$(228%) = "0121SP11DS"
            ss_temp$(229%) = "0121SP13DS"
            ss_temp$(230%) = "0121SP15DS"
            ss_temp$(231%) = "0121SP17DS"
            ss_temp$(232%) = "0121SP19DS"

            ss_temp$(241%) = "0130SP09DS"
            ss_temp$(242%) = "0130SP11DS"
            ss_temp$(243%) = "0130SP13DS"
            ss_temp$(244%) = "0130SP15DS"
            ss_temp$(245%) = "0130SP17DS"
            ss_temp$(246%) = "0130SP19DS"
            ss_temp$(247%) = "0131SP09DS"
            ss_temp$(248%) = "0131SP11DS"
            ss_temp$(249%) = "0131SP13DS"
            ss_temp$(250%) = "0131SP15DS"
            ss_temp$(251%) = "0131SP17DS"
            ss_temp$(252%) = "0131SP19DS"

            ss_temp$(261%) = "0140SP09DS"
            ss_temp$(262%) = "0140SP11DS"
            ss_temp$(263%) = "0140SP13DS"
            ss_temp$(264%) = "0140SP15DS"
            ss_temp$(265%) = "0140SP17DS"
            ss_temp$(266%) = "0140SP19DS"
            ss_temp$(267%) = "0141SP09DS"
            ss_temp$(268%) = "0141SP11DS"
            ss_temp$(269%) = "0141SP13DS"
            ss_temp$(270%) = "0141SP15DS"
            ss_temp$(271%) = "0141SP17DS"
            ss_temp$(272%) = "0141SP19DS"

            ss_temp$(281%) = "0150SP09DS"
            ss_temp$(282%) = "0150SP11DS"
            ss_temp$(283%) = "0150SP13DS"
            ss_temp$(284%) = "0150SP15DS"
            ss_temp$(284%) = "0150SP17DS"
            ss_temp$(286%) = "0150SP19DS"
            ss_temp$(287%) = "0151SP09DS"
            ss_temp$(288%) = "0151SP11DS"
            ss_temp$(289%) = "0151SP13DS"
            ss_temp$(290%) = "0151SP15DS"
            ss_temp$(291%) = "0151SP17DS"
            ss_temp$(292%) = "0151SP19DS"

            ss_temp$(900%) = "900Tempered"
            ss_temp$(901%) = "901Tempered"
            ss_temp$(902%) = "902Tempered"
            ss_temp$(903%) = "903Tempered"
            ss_temp$(904%) = "904Tempered"
            ss_temp$(950%) = "950Tempered"
            ss_temp$(951%) = "951Tempered"
            ss_temp$(991%) = "991 89STOCK"
            ss_temp$(992%) = "992 STOCK  "
            ss_temp$(999%) = "999EWDSCHGL"
                                               /* (EWD055)  - END  */
        return


        loadSORTGLASS
         sg%, sdl_index%, lamn_index%, tpl_index%, stc_index% = 0%
         init(" ") readkey$, desc$, ss$(), sizetype$()
REM str(readkey$, 1%,9%)  = "SRTGLASS"
         str(readkey$, 1%,9%)  = "SORTGLASS"
        nextSORTGLASS
         read #3,key > readkey$, using L61965, readkey$, desc$,         ~
                                                       eod goto SORTGLASS
REM if str(readkey$,1%,9%) <> "SRTGLASS" then goto SORTGLASS
           if str(readkey$,1%,9%) <> "SORTGLASS" then goto SORTGLASS

           convert str(readkey$,10%,3%) to sg%, data goto SORTGLASS

           ss$(sg%) = str(desc$,1%,11%)
REM IF SUBSTR(SS$(SG%),10%,2%) = "TB" THEN SIZETYPE$(SG%) = "TB"
REM IF SUBSTR(SS$(SG%),10%,2%) = "TT" THEN SIZETYPE$(SG%) = "TT"
REM IF SUBSTR(SS$(SG%),10%,2%) = "BB" THEN SIZETYPE$(SG%) = "TT"

           if str(desc$,28%,3%) = "SDL" then sdl_index% = sg%
           if str(desc$,28%,3%) = "TRP" then tpl_index% = sg%
           if str(desc$,28%,3%) = "LAM" then lamn_index% = sg%
           if str(desc$,28%,3%) = "STC" then stc_index% = sg%  /* (CR1173) */
           goto nextSORTGLASS
        SORTGLASS
        return

        loadSORTTEMP
         sg% = 0%
         init(" ") readkey$, desc$, ss_temp$()
         str(readkey$, 1%,9%)  = "SORTTEMP"
        nextSORTTEMP
         read #3,key > readkey$, using L61965, readkey$, desc$,         ~
                                                        eod goto SORTTEMP

           if str(readkey$,1%,9%) <> "SORTTEMP" then goto SORTTEMP

           convert str(readkey$,10%,3%) to sg%, data goto SORTTEMP

           ss_temp$(sg%) = str(desc$,1%,11%)

           goto nextSORTTEMP
        SORTTEMP
        return
