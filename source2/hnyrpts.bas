        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  RRRRR  PPPP   TTTTT   SSS           *~
            *  H   H  NN  N  Y   Y  R   R  P   P    T    S              *~
            *  HHHHH  N N N   YYY   RRR    PPP      T     SSS           *~
            *  H   H  N  NN    Y    R  R   P        T        S          *~
            *  H   H  N   N    Y    R   R  P        T     SSS           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYRPTS  - Multi purpose parts listing program to create  *~
            *             LOTS!!!!!! of different Inventory reports!    *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/12/85 ! ORIGINAL                                 ! MJB *~
            * 06/09/86 ! Corrected Calls to SETPRNT (added Report ! LDJ *~
            *          !   ID's).                                 !     *~
            * 06/17/86 ! Changed #3-Removed Prices, Added Descr   ! MJB *~
            *    ""    ! Changed #5-Added all other Quantities    !  "  *~
            *    ""    !  changes made for Revenue Project        !  "  *~
            * 11/13/86 ! Added listing with text                  ! ERN *~
            * 02/04/87 ! Changed HNYQUAN Format                   ! KAB *~
            * 05/20/87 ! Rewrite to add Range Functions           ! MJB *~
            * 03/08/88 ! Corrected Part Type & Category Range Test! HES *~
            * 03/29/88 ! PRR-6087 On report with text shouldn't   ! TLJ *~
            *          !  print parts whith all lines deleted     !     *~
            * 05/10/88 ! Fixed Parts Reference List loop          ! HES *~
            * 08/28/88 ! Fixed some Error Exit goto's             ! RJM *~
            * 10/27/88 ! Fixed 1st read on list by Cat Code       ! MJB *~
            *          !       and range end on list w/stc        !     *~
            * 11/30/88 ! Fixed Store range check on report #6     ! MJB *~
            * 04/27/89 ! Fixed Range testing on #7                ! MJB *~
            * 05/25/89 ! Stocking to Pricing conv s/b PD(14,7)    ! GGO *~
            * 02/21/90 ! Fixed printing of default record for     ! LAB *~
            *          ! category code listing, & fixed selection ! LAB *~
            *          ! problem for generic code listing         ! LAB *~
            * 08/13/90 ! Basic 4.3 & SSL Compatibility            ! KAB *~
            * 11/15/90 ! Added Management DMC to HNY013.          ! JDH *~
            * 05/20/91 ! Added GSC Enhancements for               ! JBK *~
            *          !  1.  Allow Part Type Range input - HNY023!     *~
            *          !  2.  Allow Part No., Part Type, and      !     *~
            *          !      Category Code Ranges to be specified!     *~
            *          !      for HNY015                          !     *~
            *          !  3.  Allow Category Code Ranges to be    !     *~
            *          !      specified for HNY014, 016, 027, and !     *~
            *          !      028                                 !     *~
            *          !  4.  Print HNY012 Pan Size with 1 decimal!     *~
            * 05/31/91 ! PRR 11742 Changed Logic for Inactive Days! SID *~
            *          !     11824 Expanded the Image Fields      !     *~
            * 09/13/91 ! Took off line hdrs on Page 0. OK, Maree? ! JDH *~
            * 03/26/92 ! PRR 12186.  HNY017 repair.               ! JDH *~
            *          ! PRR 12195.  HNY028 now honors TO store.  !     *~
            *          ! PRR 12100.  HNY013 added Stocking UOM.   !     *~
            *          ! PRR 11058.  HNY012 added ATC horizon,    !     *~
            *          !   and Buyer & Scheduler Class Codes.     !     *~
            * 11/11/92 ! PRR 11528 - HNY015 print Part totals.    ! RJH *~
            *          ! PRR 11976 - No longer using FAC's to     !     *~
            *          !  control display variables. Using NULL or!     *~
            *          !  valid variabled now.                    !     *~
            *          ! PRR 12446, 12482 - Fix Store range selec-!     *~
            *          !  tion conditionals.                      !     *~
            *          ! Added sub-function to print  *End Report*!     *~
            *          !  with time stamp.                        !     *~
            *          ! Misc. implied Integer conversions fixed. !     *~
            * 01/12/93 ! Page 0 Facs fix.                         ! RJH *~
            * 11/11/92 ! PRR 12771 - HNY028/029 Max/Min Report    ! RJH *~
            *          !  logic changed.  Best Min/Max values now !     *~
            *          !  used instead of First encountered when  !     *~
            *          !  evaluating for multiple Lots.           !     *~
            * 03/15/93 ! PRR 12697 Add Type & Catgy ranges to #1. ! JIM *~
            * 04/19/93 ! All 19 (!) reports now show an ASKUSER   ! JIM *~
            *          !   if nothing was selected for print.     ! JIM *~
            * 03/28/94 ! PRR 12871. HNY027 added BO & OO Qtys.    ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**
        dim                                                              ~
            abc$1,                       /* ABC Category               */~
            adate$6,                     /* Date from detail file      */~
            buyu$4,                      /* Sell  UOM                  */~
            byr1$3,                      /* Buyer code #1              */~
            byr2$3,                      /* Buyer code #2              */~
            catcd$4, catcd2$4,           /* Category Code              */~
            cdate$6,                     /* Date current               */~
            cursor%(2),                  /* Cursor Position            */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            days$3,                      /* Inactive Days Select       */~
            defprt$25,                   /* Check for default record   */~
            desc$32,                     /* Part Description           */~
            desc1$32,                    /* Part Description #1        */~
            desc2$32,                    /* Part Description #2        */~
            disp_rng$(7)22,              /* Display Range String       */~
            dtlline$(50)132,             /* Detail line for Reporting  */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            frcc$4, tocc$4, hicc$4, locc$4,  /* Category Range         */~
            frgc$16,togc$16,higc$16,logc$17, /* Generic  Range         */~
            frpn$25,topn$25,hipn$25,lopn$25, /* Part Number Range      */~
            frpt$3, topt$3, hipt$3, lopt$3,  /* Part Type Range        */~
            frst$3, tost$3, hist$3, lost$3,  /* Store Range            */~
            frvc$9, tovc$9, hivc$9, lovc$9,  /* Category Range         */~
            fromtitl$25,                 /* Range Title From           */~
            gencd$16,                    /* Generic Code               */~
            gendesc$30,                  /* Generic Code Description   */~
            genkey$41,                   /* Alt key to HNYGENER        */~
            mdmc$10,                     /* Management DMC Value       */~
            mdmc_print1$14,              /* Management DMC Print Msg   */~
            mdmc_print2$14,              /* Management DMC Print Msg   */~
            mgtrpt_on$1,                 /* Is Management Reporting on?*/~
            head$40,                     /* Header for ASKUSER         */~
            hi$80,                       /* Top Line for ASKUSER       */~
            hzn$3,                       /* ATC Horizon                */~
            i$(24)80,                    /* Screen Image               */~
            ifac$(7)1,                   /* FAC for range input        */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            inpmsg$(7)79,                /* INPUT MESSAGE for ranges   */~
            lead$10,                     /* Lead Time                  */~
            line2$79,                    /* Screen Line 2              */~
            lo$80,                       /* Bottom Line for ASKUSER    */~
            lot$6,                       /* Lot Number                 */~
            mid$80,                      /* Middle Line for ASKUSER    */~
            min$10,                      /* Minimum Stock Level        */~
            max$10,                      /* Maximum Stock Level        */~
            moq$10,                      /* MOQ                        */~
            name$60,                     /* Company name for header    */~
            oldcode$16,                  /* Prior Generic Code         */~
            oldpart$25,                  /* Previous Part number       */~
            oldstor$3,                   /* Previous store number      */~
            pan$7,                       /* Pan Size                   */~
            part$25,                     /* Part Number                */~
            part1$25,                    /* Part Number #1             */~
            part2$25,                    /* Part Number #2             */~
            pdate$8,                     /* Date for printing          */~
            pf$(3)79,                    /* PF Key Literals            */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            print$(5)9,                  /* Work Variable              */~
            ptype$12,                    /* Part type long             */~
            range$(7)22,                 /* Range Literals             */~
            partkey$50,                  /* Read key for HNYMASTR      */~
            rfac$(7)1,                   /* FAC for Range select       */~
            rptdescr$(20)36,             /* Report description for scr */~
            sellu$4,                     /* Stock UOM                  */~
            spart$25,                    /* Part from HNYDETAL         */~
            spec$4,                      /* Spec/Obso indicator        */~
            stext$40,                    /* Text from HNYDETAL         */~
            stor$3,                      /* Store Number               */~
            suid$3,                      /* User ID   HNYDETAL         */~
            text$40,                     /* Text from HNYDETAL         */~
            textid$4,                    /* Text ID                    */~
            time$8,                      /* Sys Time Stamp             */~
            totitl$25,                   /* Range Title To             */~
            totcost$10,                  /* Total Standard Cost        */~
            tot_msg$41,                  /* Totals Print Message Text  */~
            ttlsel$30,                   /* Report select line         */~
            type$3, type2$3,             /* Part Type                  */~
            uid$3,                       /* User ID   HNYDETAL         */~
            vname$30,                    /* Vendor Name                */~
            vend$9,                      /* Vendor Code                */~
            workkey$60                   /* Key to WORKFILE            */

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20                  /* TEXT FROM FILE OPENING     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.01 07/28/94 CMS Patch Release R6.03.01      "
        REM *************************************************************

            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! HNYMASTR ! Inventory Master File                    *~
            * #2  ! HNYDETAL ! Inventory detail file                    *~
            * #3  ! HNYQUAN  ! Inventory Store Quantity File            *~
            * #4  ! BOMMASTR ! BOM relationship file                    *~
            * #5  ! SYSFILE2 ! System Records File                      *~
            * #6  ! WORKFILE ! Work file for various reports            *~
            * #8  ! HNYGENER ! Generic Description File                 *~
            * #9  ! VENDOR   ! Vendor Master File                       *~
            * #10 ! CATEGORY ! Category Master File                     *~
            * #11 ! HNYALTRS ! Alternate Parts File                     *~
            * #12 ! HNYOPTNS ! Option Parts File                        *~
            * #13 ! TXTFILE  ! System Text File                         *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #2,  "HNYDETAL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =   43, keylen =   6, dup,    ~
                            key  2, keypos =   49, keylen =   2, dup     ~

            select #3,  "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  650,                                  ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =   44

            select #4,  "BOMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56          ~

            select #5,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos  =  1, keylen =  20

            select #6,  "WORKFILE",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  200,                                  ~
                        keypos  =  1, keylen =  60

            select #8,  "HNYGENER",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos =   17, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  41

            select #9,  "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos = 1, keylen =  9,                         ~
                        alt key  1, keypos = 10, keylen = 30, dup

            select #10, "CATEGORY",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 200,                                  ~
                         keypos = 1, keylen = 4

            select #11, "HNYALTRS",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 60,                                   ~
                         keypos = 1, keylen = 33

            select #12, "HNYOPTNS",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 100,                                  ~
                         keypos  =   1, keylen = 54

            select #13, "TXTFILE",                                       ~
                        varc, indexed, recsize = 2024,                   ~
                        keypos = 1, keylen = 11

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *************************************************************

            time$ = " "  :  call "TIME" (time$)
            date$ = date
            cdate$ = date$
            call "DATEFMT" (date$)
            u3% = 0%
            head$ = "ABNORMAL FILE CONDITION ERROR"
            str(line2$,62) = " HNYRPTS: " & str(cms2v$,1,8)
            call "SHOSTAT" ("Opening Files, One Moment Please")
            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

            select printer(134)
            ttlsel$ = "Enter Report Selection"

            init(" ") range$()
            init(hex(00)) defprt$
            range$(1%) = "Part Number"
            range$(2%) = "Part Type"
            range$(3%) = "Store Number"
            range$(4%) = "Part Category"
            range$(5%) = "Vendor Code"
            range$(6%) = "Generic Code"
            range$(7%) = "Number Days Inactive"

            inpmsg$(1%) = "Enter Beginning and Ending Part Numbers    "
            inpmsg$(2%) = "Enter Beginning and Ending Part Type Codes "
            inpmsg$(3%) = "Enter Beginning and Ending Store Codes     "
            inpmsg$(4%) = "Enter Beginning and Ending Category Codes  "
            inpmsg$(5%) = "Enter Beginning and Ending Vendor Codes    "
            inpmsg$(6%) = "Enter Beginning and Ending Generic Codes   "
            inpmsg$(7%) = "Enter Number of Inactive Days to Report    "

            fromtitl$ = "Beginning Code Number"
            totitl$   = "Ending Code Number"

            for i% = 1% to 20%
                str(rptdescr$(i%),,2%) = hex(0b8c)
            next i%

            str(rptdescr$( 1%),3%) = "Parts Listing with Text       "
            str(rptdescr$( 2%),3%) = "Inactive Parts Listing        "
            str(rptdescr$( 3%),3%) = "Parts with Alternates         "
            str(rptdescr$( 4%),3%) = "Parts with Options            "
            str(rptdescr$( 5%),3%) = "Negative Inventory Listing    "
            str(rptdescr$( 6%),3%) = "Parts in Stock at zero cost   "
            str(rptdescr$( 7%),3%) = "Below Minimum Stock Report    "
            str(rptdescr$( 8%),3%) = "Above Maximum Stock Report    "
            str(rptdescr$( 9%),3%) = "Part Number Reference List    "
            str(rptdescr$(10%),3%) = "Master Parts List             "
            str(rptdescr$(11%),3%) = "Parts Listed at Standard Costs"
            str(rptdescr$(12%),3%) = "Alphabetical by Description   "
            str(rptdescr$(13%),3%) = "Parts listed by Generic Code  "
            str(rptdescr$(14%),3%) = "Parts listed by Category Code "
            str(rptdescr$(15%),3%) = "Purch Parts by Primary Vendor "
            str(rptdescr$(16%),3%) = "Part Quantities By Store/Lot  "
            str(rptdescr$(17%),3%) = "Parts Flagged OBSOLETE        "
            str(rptdescr$(18%),3%) = "Mfg parts with no W/U or BOM  "
            str(rptdescr$(19%),3%) = "Purchased parts with no W/U   "
            str(rptdescr$(20%),3%) = "                              "

            call "COMPNAME" (12%, name$, ret%)
            ret% = 0%
            goto L10000     /* Bypass below for now */

        is_management_reporting_on
            /* See if Management Reporting is on */
            init(" ") mdmc_print1$, mdmc_print2$
            call "READ100" (#5, "SWITCHS.GL", f1%(5%))
            if f1%(5%) = 1% then get #5 using L09680, mgtrpt_on$
L09680:         FMT POS(59), CH(1)
            if mgtrpt_on$ <> "Y" then return
                mdmc_print1$ = "MANAGEMENT DMC"
                mdmc_print2$ = "--------------"
                return

L10000: REM *************************************************************~
            *       I N P U T   M O D E  ,  R E P O R T   T Y P E       *~
            *************************************************************

        inputmode
            init(" ") errormsg$, frpn$,topn$, frpt$,topt$, frst$,tost$,  ~
                          days$, frcc$,tocc$, frvc$,tovc$, frgc$,togc$
            inpmessage$ = "Position cursor and press RETURN to "  &      ~
                          "select Desired Report"
L10090:     gosub'101
                if keyhit% =  1% then gosub startover
                if keyhit% = 16% then L65000
                if keyhit% <> 0% then L10090

            rpt% = cursor%(1) - 5%
            if rpt% < 1% or rpt% > 10% then L10090
            if cursor%(2) > 40% then rpt% = rpt% + 10%

        REM Now set RNG% for setting facs
            if rpt% > 16% then gen_rpts
            if rpt% > 15% then L10290
                rng% = 1%
            if rpt% <> 1% then L10230
               rng% =  4%
               goto L12000
L10230:     if rpt% > 13% then L10260
               rng% =  2%
            if rpt% =  2% then rng% = 9%
L10260:     if rpt% <  5% then L12000
            if rpt% >  8% then L10310
               rng% = 10%
L10290:     if rpt% = 16% then rng% = 10%
               goto L12000
L10310:     if rpt% <  9% then L12000
            if rpt% <> 13% then L10350
               rng% =  5%
               goto L12000
L10350:     if rpt% <> 15% then L10380
               rng% =  7%
               goto L12000
L10380:     rng% = 4%

L12000: REM *************************************************************~
            *      I N P U T   M O D E   R A N G E   S E L E C T        *~
            *************************************************************
            init (hex(9c)) rfac$(), ifac$()
            init (hex(8c)) rfac$()
            disp_rng$() = " "
            for fieldnr% = 1% to 7%
L12050:         gosub'052(fieldnr%,1%)
                    if enabled% = 0% then L12190
L12070:         gosub'102(fieldnr%,1%)
                    if keyhit% =  1% then gosub startover
                    if keyhit% <> 4% then L12150
L12100:                 fieldnr% = max(1%, fieldnr% - 1%)
                        gosub'052(fieldnr%,1%)
                        if enabled% = 1% then L12070
                        if fieldnr% = 1% then L12050
                        goto L12100
L12150:             if keyhit% = 16% then L65000
                    if keyhit% <> 0% then L12070
                gosub'152(fieldnr%)
                    if errormsg$ <> " " then L12070
L12190:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   R A N G E   S E L E C T        *~
            *************************************************************
        editpg2
            gosub'102(0%,2%)
                if keyhit% =  1% then gosub startover
                if keyhit% = 16% then gen_rpts
                if keyhit% <> 0% then editpg2
                fieldnr% = cursor%(1) - 6%
                if fieldnr% < 1% or fieldnr% > 7% then editpg2

            gosub'052(fieldnr%,2%)
                if enabled% = 0% then editpg2
L16130:     gosub'102(fieldnr%,2%)
                if keyhit% =  1% then gosub startover
                if keyhit% <> 0% then L16130
            gosub'152(fieldnr%)
                if errormsg$ <> " " then L16130
            goto editpg2

        REM *************************************************************~
            *    S E T   I F A C   F O R   I N P U T / E D I T          *~
            *************************************************************
        deffn'052(fieldnr%,mode%)
            if mode% = 2% then init(hex(8c)) ifac$()
            enabled% = 0%
            on fieldnr% gosub L18100,            /* Part Number Range   */~
                              L18200,            /* Part Type Range     */~
                              L18300,            /* Store Code Range    */~
                              L18400,            /* Part Category Range */~
                              L18500,            /* Vendor Code Range   */~
                              L18600,            /* Generic Desig Range */~
                              L18700             /* Inactive Days       */
                return

L18100
*       * Part Number Range
            if rng% = 9% or rng% = 10% then L18130
            if rng% > 7% then return
L18130
*          IF MODE% = 1% THEN RFAC$(1) = HEX(8C)
            if mode% = 1% then disp_rng$(1%) = range$(1%)
            ifac$(1%) = hex(81)
            if frpn$ = " " then frpn$ = "ALL"
            enabled% = 1%
            return

L18200
*       * Part Type Range
            if rng% = 9% or rng% = 10% then L18230
            if rng% < 2% or rng% > 5% then return
L18230
*          IF MODE% = 1% THEN RFAC$(2) = HEX(8C)
            if mode% = 1% then disp_rng$(2%) = range$(2%)
            ifac$(2%) = hex(81)
            if frpt$ = " " then frpt$ = "ALL"
            enabled% = 1%
            return

L18300
*       * Store Code Range
            if rng% = 10% then L18320
            if rng% <> 3% and rng% <> 8% then return
L18320
*          IF MODE% = 1% THEN RFAC$(3) = HEX(8C)
            if mode% = 1% then disp_rng$(3%) = range$(3%)
            ifac$(3%) = hex(81)
            if frst$ = " " then frst$ = "ALL"
            enabled% = 1%
            return

L18400
*       * Part Category Range
            if rng% = 10% then L18411
            if rng% <> 4% and rng% <> 6% then return
L18411
*          IF MODE% = 1% THEN RFAC$(4) = HEX(8C)
            if mode% = 1% then disp_rng$(4%) = range$(4%)
            ifac$(4%) = hex(81)
            if frcc$ = " " then frcc$ = "ALL"
            enabled% = 1%
            return

L18500
*       * Vendor Code Range
            if rng% <> 7% then return
*          IF MODE% = 1% THEN RFAC$(5) = HEX(8C)
            if mode% = 1% then disp_rng$(5%) = range$(5%)
            ifac$(5%) = hex(81)
            if frvc$ = " " then frvc$ = "ALL"
            enabled% = 1%
            return

L18600
*       * Generic Designator Range
            if rng% <> 5% then return
*          IF MODE% = 1% THEN RFAC$(6) = HEX(8C)
            if mode% = 1% then disp_rng$(6%) = range$(6%)
            ifac$(6%) = hex(81)
            if frgc$ = " " then frgc$ = "ALL"
            enabled% = 1%
            return

L18700
*       * Inactive Days
            if rng% <> 9% then return
*          IF MODE% = 1% THEN RFAC$(7) = HEX(8C)
            if mode% = 1% then disp_rng$(7%) = range$(7%)
            ifac$(7%) = hex(81)
            if days$ = " " then days$ = "0  "
            enabled% = 1%
            return

        REM *************************************************************~
            *             G E N E R A T E   T H E   R E P O R T S       *~
            *************************************************************
        gen_rpts
            selected% = 0%              /* Indicate 'nothing selected' */
            on rpt% gosub   L20000,   /*  1 Parts Listing With Text     */~
                            L20205,   /*  2 Inactive                    */~
                            L20490,   /*  3 Parts with Alternates       */~
                            L20705,   /*  4 Parts with Options          */~
                            L20935,   /*  5 Negative Inventory Report   */~
                            L21140,   /*  6 Parts at zero cost          */~
                            L21495,   /*  7 Parts below MIN Stock Level */~
                            L21495,   /*  8 Parts above MAX Stock Level */~
                            L22040,   /*  9 Part Number Reference List  */~
                            L22340,   /* 10 Master Parts List           */~
                            L22550,   /* 11 Parts Listed at Standard    */~
                            L22735,   /* 12 Alphabetical on Description */~
                            L23110,   /* 13 By Generic Code             */~
                            L23460,   /* 14 By Category Code            */~
                            L23715,   /* 15 Purch By Primary Vendor     */~
                            L24065,   /* 16 Qty on hand                 */~
                            L24375,   /* 17 Obsolete                    */~
                            L24605,   /* 18 Mfg, no W/U or BOM          */~
                            L24815    /* 19 Purch, no W/U               */

                goto inputmode

L20000: REM *************************************************************~
            **  #1        Part Number Listing with Text                **~
            *************************************************************
            call "SHOSTAT" ("Printing Parts Listing with Text")
            call "OPENCHCK" (#1, 0%, f2%(1%), 0%, rslt$(1%))
                if f2%(1%) <> 0 then L64015
            call "OPENCHCK" (#13, 0%, f2%(13%), 0%, rslt$(13%))
            call "SETPRNT" ("HNY035", " ", 1%, 0%)
            pcntr% = -1%  :  gosub print_params
            partkey$ = lopn$
            call "READ102" (#1, partkey$, f1%(1%))
                if f1%(1%) = 0% then L20170
                goto L20080

L20070:     call "READNEXT" (#1, f1%(1%))
                if f1%(1%) = 0% then L20170
L20080:     get #1 using L20085, part$, desc$, catcd$, textid$, type$
L20085:         FMT CH(25), CH(32), POS(90), CH(4), POS(98), CH(4),      ~
                     POS(180), CH(3)
            if part$ > hipn$ then L20170
            if catcd$ < locc$ then L20070
            if catcd$ > hicc$ then L20070
            if type$ < lopt$ then L20070
            if type$ > hipt$ then L20070
            if textid$ = " " or textid$ = hex(00000000) or               ~
                                textid$ = hex(ffffffff) then L20070
*        Test if TEXT lines have been deleted
            textkey$ = "M   " & textid$ & hex(000000)
            call "PLOWNEXT" (#13, textkey$, 8%, f1%(13%))
            if f1%(13%) = 0% then L20070
            str(textkey$,10,2) = hex(ffff)
L20116:     get #13 using L20117, lines%
L20117:         FMT POS(15),     BI(2)
            if lines% <> 0% then L20125
            call "PLOWNEXT" (#13, textkey$, 9%, f1%(13%))
            if f1%(13%) = 0% then L20070 else goto L20116

L20125:     if lcntr% > 53% then gosub page_head
            print using L63190, part$, desc$
            lcntr% = lcntr% + 1%
            selected% = 1%            /* Indicate 'something selected' */
            textstatus% = 0%
L20145:     call "TXTPRINT" (#13, f2%(13), 134%, textid$, "HNY035", 60%, ~
                             lcntr%, 56%, "N", " ", textstatus%)
            if textstatus% = 0% then L20070 else gosub page_head
            goto L20145

L20170:     gosub print_end_report
            if selected% = 0% then gosub null_set_askuser

            close #1
            close #13
            return

L20205: REM *************************************************************~
            **  #2              Inactive Parts Listing                 **~
            *************************************************************
            convert days% to d$, pic(###)
            hi$ = "Printing Parts With NO Activity Within the Last " &   ~
                    d$ & " DAYS"
            call "SHOSTAT" (hi$)
            call "OPENCHCK" (#1, 0%, f2%(1%), 0%, rslt$(1%))
                if f2%(1%) <> 0 then L64015
            call "OPENCHCK" (#2, 0%, f2%(2%), 0%, rslt$(2%))
                if f2%(2%) <> 0 then L64050
                if val(str(rslt$(2),17,4),4) = 0% then L64050
            call "SETPRNT" ("HNY017", " ", 1%, 0%)
            pcntr% = -1%  :  gosub print_params
            workkey$ = lopn$

        read_loop2
            olddiff% = 9999%
            call "PLOWNEXT" (#2, workkey$, 0%, f1%(2%))
                if f1%(2%) = 0% then L20450
            goto L20325

L20315:     call "PLOWNEXT" (#2, workkey$, 25%, f1%(2%))
                if f1%(2%) = 0% then L20372
L20325:     get #2 using L20475, part$, adate$, text$, uid$
            if part$ > hipn$ then L20450
            call "DATE" addr ("G-", adate$, cdate$, diff%, u3%)
            if diff% < days% then next_part
            if olddiff% = 99999% then L20355
            if diff% >= olddiff% then next_lot
L20355:     olddiff% = diff%
            stext$ = text$ : spart$ = part$
            suid$ = uid$   : pdate$ = adate$
            goto next_lot
L20372:     if spart$ = " " then next_part
            call "DESCRIBE" (#1, spart$, desc$, 0%, f1%(1))
            get #1 using L20380, type$
L20380:         FMT POS(180), CH(3)
            if type$ < lopt$ or type$ > hipt$ then next_part
            call "DATEFMT" (pdate$)
            if lcntr% > 58% then gosub page_head
            print using L61640, spart$, desc$, type$, pdate$, suid$, stext$
            spart$ = " "
            lcntr% = lcntr% + 1%
            selected% = 1%            /* Indicate 'something selected' */
            goto read_loop2

        next_part
            spart$ = " "
            str(workkey$,26%,9%) = all(hex(ff))
            goto read_loop2

        next_lot
            str(workkey$,35%,6%) = all(hex(ff))
            goto L20315

L20450:     gosub print_end_report
            if selected% = 0% then gosub null_set_askuser

            close #1 : close #2
            return

L20475: FMT  /* HNYDETAL for report #2      */                           ~
            CH(25), XX(17), CH(6), XX(34), CH(40), CH(3)

L20490: REM *************************************************************~
            **  #3        Part Number With Alternate Parts             **~
            *************************************************************
            call "SHOSTAT" ("Printing Listing of Parts With Alternates")
            call "OPENCHCK" (#1, 0%, f2%(1%), 0%, rslt$(1%))
                if f2%(1%) <> 0 then L64015
            call "OPENCHCK" (#11, 0%, f2%(11%), 0%, rslt$(11%))
                if f2%(11%) <> 0 then L64410
                if val(str(rslt$(11%),17%,4%),4%) = 0% then L64410
            call "SETPRNT" ("HNY025", " ", 1%, 0%)
            pcntr% = -1%  :  gosub print_params
            init(hex(00)) part2$, partkey$
            partkey$ = lopn$
            call "READ102" (#11, partkey$, f1%(11%))
                if f1%(11%) = 0% then L20680
                goto L20590

        read_loop3
L20580:     call "READNEXT" (#11, f1%(11%))
                if f1%(11%) = 0% then L20680
L20590:     get #11 using L20595, part$, part1$
L20595:         FMT CH(25), POS(34), CH(25)
            if part$ > hipn$ then L20680
            call "DESCRIBE" (#1, part$, desc$, 0%, f1%(1%))
                if f1%(1%) = 0% then desc$ = "PART NOT ON FILE"
            get #1 using L20620, type$
L20620:         FMT POS(180), CH(3)
            if type$ < lopt$ or type$ > hipt$ then L20580
            call "DESCRIBE" (#1, part1$, desc1$, 0%, f1%(1%))
                if f1%(1%) = 0% then desc1$ = "ALTERNATE PART NOT ON FILE"
            if lcntr% > 58% then gosub page_head
            if part$=part2$ then print using                             ~
                                     L62680, " ", " ", part1$, desc1$     ~
                    else print using L62680 , part$, desc$, part1$, desc1$
            part2$ = part$
            lcntr% = lcntr% + 1%
            selected% = 1%            /* Indicate 'something selected' */
            goto read_loop3

L20680:     gosub print_end_report
            if selected% = 0% then gosub null_set_askuser

            close #1 : close #11
            return

L20705: REM *************************************************************~
            **  #4        Part Number With Option Parts                **~
            *************************************************************
            call "SHOSTAT" ("Printing Listing of Parts With Options")
            call "OPENCHCK" (#1, 0%, f2%(1%), 0%, rslt$(1%))
                if f2%(1%) <> 0% then L64015
            call "OPENCHCK" (#12, 0%, f2%(12%), 0%, rslt$(12%))
                if f2%(12%) <> 0 then L64470
                if val(str(rslt$(12%),17%,4%),4%) = 0% then L64470
            call "SETPRNT" ("HNY026", " ", 1%, 0%)
            pcntr% = -1%  :  gosub print_params
            init(hex(00)) part2$

        read_loop4
            call "READNEXT" (#12, f1%(12%))
                if f1%(12%) = 0% then L20910
            get #12 using L20790, part$, part1$
L20790:         FMT POS(29), CH(25), POS(55), CH(25)
            if part$ < lopn$ then read_loop4
            if part$ > hipn$ then L20910
            call "DESCRIBE" (#1, part$, desc$, 0%, f1%(1%))
                if f1%(1%) = 0% then desc$ = "PART NOT ON FILE"
            get #1 using L20820, type$
L20820:         FMT POS(180), CH(3)
            if type$ < lopt$ or type$ > hipt$ then read_loop4
            if part1$ <> part$ then L20850
            desc1$ = desc$
            goto L20875

L20850:     if part1$ <> "** DON'T USE ANYTHING **" then L20865
            desc1$ = " "
            goto L20875
L20865:     call "DESCRIBE" (#1, part1$, desc1$, 0%, f1%(1%))
                if f1%(1%) = 0% then desc1$ = "ALTERNATE PART NOT ON FILE"
L20875:     if lcntr% > 58% then gosub page_head
            if part$=part2$ then print using L62680," "," ",part1$, desc1$~
                     else print using L62680, part$, desc$, part1$, desc1$
            lcntr% = lcntr% + 1%
            selected% = 1%            /* Indicate 'something selected' */
            part2$ = part$
            goto read_loop4

L20910:     gosub print_end_report
            if selected% = 0% then gosub null_set_askuser

            close #1 : close #12
            return

L20935: REM *************************************************************~
            **  #5           Negative Inventory Report                 **~
            *************************************************************
            call "SHOSTAT" ("Printing Parts With Negative Inventory Statu~
        ~s")
            call "OPENCHCK" (#1, 0%, f2%(1%), 0%, rslt$(1%))
                if f2%(1) <> 0 then L64015
            call "OPENCHCK" (#3, 0%, f2%(3%), 0%, rslt$(3%))
                if f2%(3) <> 0 then L64110
                if val(str(rslt$(3%),17%,4%),4%) = 0% then L64110
            call "SETPRNT" ("HNY014", " ", 1%, 0%)
            pcntr% = -1%  :  gosub print_params
            partkey$ = lopn$
            call "READ102" (#3, partkey$, f1%(3%))
                if f1%(3%) = 0% then L21100
                goto L21035

        read_loop5
            call "READNEXT" (#3, f1%(3%))
                if f1%(3%) = 0% then L21100
L21035:     get #3 using L21125, part1$, stor$, lot$, qtyoh
            if part1$ > hipn$ then L21100
            if stor$ <= lost$ or stor$ > hist$ then read_loop5
            if qtyoh >= 0 then read_loop5

            call "READ100" (#1, part1$, f1%(1%))
            if f1%(1%) = 1 then L21060
                desc1$ = "PART NOT ON MASTER FILE !"
                goto L21075

L21060:     get #1 using L21065, desc1$, catcd$, type$
L21065:         FMT POS(26), CH(32), POS(90), CH(4), POS(180), CH(3)
            if type$  < lopt$ or type$  > hipt$ then read_loop5
            if catcd$ < locc$ or catcd$ > hicc$ then read_loop5

L21075:     if lcntr% > 58% then gosub page_head
            print using L61180 , part1$, desc1$, catcd$, type$, stor$,    ~
                                lot$, qtyoh
            lcntr% = lcntr% + 1%
            selected% = 1%            /* Indicate 'something selected' */
            goto read_loop5

L21100:     gosub print_end_report
            if selected% = 0% then gosub null_set_askuser

            close #1 : close #3
            return

L21125: FMT  /* HNYQUAN  for report #4      */                           ~
            POS(17), CH(25), CH(3), CH(6), POS(69), PD(14,4)

L21140: REM *************************************************************~
            **  #6           Parts in stock at zero cost               **~
            *************************************************************
            call "SHOSTAT" ("Printing List of Parts in Stock        at Ze~
        ~ro Cost")
            call "OPENCHCK" (#1, 0%, f2%(1%), 0%, rslt$(1%))
                if f2%(1%) <> 0 then L64015
            call "OPENCHCK" (#3, 0%, f2%(3%), 0%, rslt$(3%))
                if f2%(3%) <> 0 then L64110
                if val(str(rslt$(3%),17%,4%),4%) = 0% then L64110
            call "SETPRNT" ("HNY016", " ", 1%, 0%)
            pcntr% = -1%  :  gosub print_params
            recnbr% = max(10%,(val(str(rslt$(3%),17%,4%),4%)))
            call "WORKOPEN" (#6, "IO    ", (recnbr% / 5%), f2%(6%))
            partkey$ = lopn$
            call "READ102" (#3, partkey$, f1%(3%))
                if f1%(3%) = 0% then L21430
                goto L21250

        read_loop6
            call "READNEXT" (#3, f1%(3%))
                if f1%(3%) = 0% then L21335
L21250:     get #3 using L21475, part1$, stor$, lot$, qtyoh, totalcost
            if part1$ > hipn$ then L21335
            if stor$ <= lost$ or stor$ > hist$ then read_loop6
            if totalcost <> 0 then read_loop6

            call "READ100" (#1, part1$, f1%(1%))
                if f1%(1%) = 1 then L21300
                    desc1$ = "PART NOT ON MASTER FILE!"
                    type$ = "   "
                    goto L21315

L21300:     get #1 using L21305, desc1$, catcd$, type$
L21305:         FMT XX(25), CH(32), POS(90), CH(4), POS(180), CH(3)
            if type$  < lopt$ or type$  > hipt$ then read_loop6
            if catcd$ < locc$ or catcd$ > hicc$ then read_loop6

L21315:     write #6 using L21460, part1$, stor$, lot$, desc1$,           ~
                                  type$, qtyoh, totalcost, catcd$
            goto read_loop6

L21335:     init(hex(00)) workkey$
            call "READ104" (#6, workkey$, f1%(6%))
                if f1%(6%) = 0% then L21430
            goto L21375

        print_loop6
            call "READNEXT" (#6, f1%(6%))
                if f1%(6%) = 0% then L21430
L21375:     get #6 using L21460, part1$, stor$, lot$, desc1$, type$,      ~
                                qtyoh, totalcost, catcd$
            if type$ < "500" then ptype$ = "PURCHASED   " else           ~
                                  ptype$ = "MANUFACTURED"
            if type$ < "100" then ptype$ = "GENERIC     "
            if lcntr% > 58% then gosub page_head
            print using L61470 , part1$, desc1$, catcd$, ptype$, stor$,   ~
                                lot$, totalcost, qtyoh
            lcntr% = lcntr% + 1%
            selected% = 1%            /* Indicate 'something selected' */
            goto print_loop6

L21430:     gosub print_end_report
            if selected% = 0% then gosub null_set_askuser

            close #1 : close #3
            call "FILEBGON" (#6)
            return

L21460: FMT  /* WORKFILE for report #6      */                           ~
            CH(25), CH(3), CH(6), XX(26), CH(32), CH(4), 2*PD(14,4), CH(4)

L21475: FMT  /* HNYQUAN  for report #6      */                           ~
            POS(17), CH(25), CH(3), CH(6), POS(69), PD(14,4), POS(117),  ~
            PD(14,4)

L21495: REM *************************************************************~
            **  #7 & #8      Parts MIN & MAX Reports                   **~
            *************************************************************
            if rpt% = 7% then                                            ~
            call "SHOSTAT" ("Printing List of Parts With Quantity On Hand~
        ~ Below the Designated Minimum Level") else                       ~
            call "SHOSTAT" ("Printing List of Parts With Quantity On Hand~
        ~ Above the Designated Maximum Level")
            call "OPENCHCK" (#1, 0%, f2%(1%), 0%, rslt$(1%))
                if f2%(1%) <> 0 then L64015
            call "OPENCHCK" (#3, 0%, f2%(3%), 0%, rslt$(3%))
                if f2%(3%) <> 0 then L64110
                if val(str(rslt$(3),17,4),4) = 0% then L64110
            if rpt% = 7% then  call "SETPRNT" ("HNY027", " ", 1%, 0%)    ~
                          else call "SETPRNT" ("HNY028", " ", 1%, 0%)
            pcntr% = -1%  :  gosub print_params
            recnbr% = max(10%,(val(str(rslt$(3),17,4),4)))
            call "WORKOPEN" (#6, "IO    ", (recnbr% / 4%), f2%(6%))
            oldstor$, part2$ = hex(00) : minm = 9e8 : maxm = 0
            partkey$ = lopn$
            call "READ102" (#3, partkey$, f1%(3%))
                if f1%(3%) = 0% then L21951   /* GoTo END REPORT */
                goto L21607  /* GET the DATA */
        read_loop7
            call "READNEXT" (#3, f1%(3%))
            if f1%(3%) <> 0% then L21607
L21599:         gosub write_work          /* Last Record */
                goto start_print7
L21607:     get #3 using L21983 , part$, stor$, qtyoh, qtybo, qtyoo, min$,~
                                                                    max$
            if part$ = part2$ then L21679   /* Not a New Part from Loop */
                /* We have a new Part to test */
                if part$ > hipn$ then L21599 /*End of Part Range so Print*/
                call "READ100" (#1, part$, f1%(1%))
                    if f1%(1%) = 0% then L21651      /* Shouldn't Happen */
                get #1 using L21635, catcd$, type$
L21635:             FMT POS(90), CH(4), POS(180), CH(3)
                if type$  < lopt$ or type$  > hipt$ then read_loop7
                if catcd$ < locc$ or catcd$ > hicc$ then read_loop7
            /* Test for Valid Store */
L21651:     if stor$ <= lost$ or stor$ > hist$ then read_loop7
            /* Test for 1st time thru. Write the last valid part if not */
            if part2$ <> hex(00) and oldstor$ <> hex(00)                 ~
                                                   then gosub  write_work
                part2$   = part$  :  type2$  = type$
                oldstor$ = stor$  :  catcd2$ = catcd$
                goto L21707
L21679:     /* Test for a New Store (Loop thru Store for Lots )*/
            if oldstor$ = stor$ then L21711       /* Accumulate and Loop */
                if stor$ <= lost$ or stor$ > hist$ then read_loop7
                gosub write_work
                oldstor$ = stor$ : totbo, totoo, totqty = 0 /* Init */
                /* Fall into Accumulation */
            /* Accumulate Lot Quantities for the Store & set Min & Max  */
L21707:     minm = 9e8        :  maxm = 0
L21711:     gosub set_min_max
            totqty = totqty + qtyoh
            totbo  = totbo  + qtybo
            totoo  = totoo  + qtyoo
            goto read_loop7

        set_min_max
            convert min$ to mintemp, data goto L21739
            if mintemp < minm then minm = mintemp
L21739:     convert max$ to maxtemp, data goto L21747
            if maxtemp > maxm then maxm = maxtemp
L21747:     return
        write_work
            if minm = 9e8 then minm = 0
            if maxm = 0   then maxm = 9e8
            if totqty > minm and totqty < maxm then L21779
            call "DESCRIBE" (#1, part2$, desc2$, 0%, f1%(1%))
            write #6 using L21971, oldstor$, part2$, desc2$, totqty,      ~
                                totbo, totoo, minm, maxm, catcd2$, type2$
L21779:     totbo, totoo, totqty = 0
            return

        start_print7
            init(hex(00)) workkey$
            call "READ104" (#6, workkey$, f1%(6%))
                if f1%(6%) = 0% then L21951
            oldstor$ = hex(00)
            goto L21827
        print_loop7
            call "READNEXT" (#6, f1%(6%))
                if f1%(6%) = 0% then L21951
L21827:     get #6 using L21971 , stor$, part$, desc$, qty, bo, oo,       ~
                                 minm, maxm, catcd$, type$
            if stor$ > hist$ then L21847
            if rpt% = 7% and qty < minm then print_min
            if rpt% = 8% and qty > maxm then print_max
L21847:     goto print_loop7

        print_min
            if lcntr% > 58% then gosub page_head
            if stor$ = oldstor$ or oldstor$ = hex(00) then L21871
            print  :  lcntr% = lcntr% + 1%
L21871:     qtysh = minm - qty
            print using L62940 , stor$, part$, desc$, catcd$, type$,      ~
                                bo, oo, qty, minm, qtysh
            oldstor$ = stor$
            lcntr% = lcntr% + 1%
            selected% = 1%            /* Indicate 'something selected' */
            qtysh = 0
            goto print_loop7

        print_max
            if lcntr% > 58% then gosub page_head
            if stor$ = oldstor$ or oldstor$ = hex(00) then L21919
            print  :  lcntr% = lcntr% + 1%
L21919:     qtyov = qty - maxm
            print using L63070 , stor$, part$, desc$, catcd$, type$, qty, ~
                                maxm, qtyov
            oldstor$ = stor$
            lcntr% = lcntr% + 1%
            selected% = 1%            /* Indicate 'something selected' */
            qtyov = 0
            goto print_loop7

L21951:     gosub print_end_report
            if selected% = 0% then gosub null_set_askuser
            close #1 : close #3
            call "FILEBGON" (#6)
            return

L21971: FMT  /* WORKFILE for report #7      */                           ~
            CH(3), CH(25), XX(32), CH(32), 5*PD(14,4), CH(4), CH(3)

L21983: FMT  /* HNYQUAN  for report #7  & #8   */                        ~
            POS(17), CH(25), CH(3), POS(69), 3*PD(14,4), POS(221),       ~
            2*CH(10)

L22040: REM *************************************************************~
            **  #9        Part Number Reference Listing                **~
            *************************************************************
            call "SHOSTAT" ("Printing Reference Part Number Listing")
            call "OPENCHCK" (#1, 0%, f2%(1%), 0%, rslt$(1%))
                if f2%(1%) <> 0 then L64015
            call "SETPRNT" ("HNY011", " ", 1%, 0%)
            pcntr% = -1%  :  gosub print_params
            partkey$ = lopn$
            call "READ102" (#1, partkey$, f1%(1%))
                if f1%(1%) = 0% then L22300
                firstsw% = 1%

        read_loop9
            gosub page_head
            init(" ") dtlline$( )
            p1%, p2% = 0%
            if firstsw% = 1% then L22145
L22130:     if p1% = 50% then L22190
L22135:         call "READNEXT" (#1, f1%(1%))
                    if f1%(1%) = 0% then L22250
L22145:         get #1 using L22150, part1$, desc1$, catcd$, type$
L22150:             FMT CH(25), CH(32), POS(90), CH(4), POS(180), CH(3)
                firstsw% = 0%
                if part1$ > hipn$ then L22250
                if type$ < lopt$ or type$ > hipt$ then L22135
                if catcd$ <= locc$ or catcd$ > hicc$ then L22135
                p1% = p1% + 1%
                put dtlline$(p1%) using L22150, part1$, desc1$
            goto L22130

L22190:     if p2% = 50% then L22250
L22195:         call "READNEXT" (#1, f1%(1%))
                    if f1%(1%) = 0% then L22250
                get #1 using L22150, part2$, desc2$, catcd$, type$
                if part2$ > hipn$ then L22250
                if type$ < lopt$ or type$ > hipt$ then L22195
                if catcd$ <= locc$ or catcd$ > hicc$ then L22195
                p2% = p2% + 1%
                put dtlline$(p2%) using L22235, part2$, desc2$
L22235:             FMT POS(58), CH(25), CH(32)
            goto L22190

L22250:     for k = 1 to 50
                get dtlline$(k) using L22325,part1$,desc1$,part2$,desc2$
                if part1$ = " " then L22270
                print using L60720 , part1$, desc1$, part2$, desc2$
                selected% = 1%        /* Indicate 'something selected' */
L22270:     next k

            if f1%(1%) = 0% then L22300
            if key(#1,0) < hipn$ then read_loop9

L22300:     gosub print_end_report
            if selected% = 0% then gosub null_set_askuser

            close #1
            return

L22325: FMT  /* Detail array report #9 */                                ~
            CH(25), CH(32), CH(25), CH(32)

L22340: REM *************************************************************~
            **  #10              Master Parts List                     **~
            *************************************************************
            call "SHOSTAT" ("Printing Master Inventory Parts Listing")
            call "OPENCHCK" (#1, 0%, f2%(1%), 0%, rslt$(1%))
                if f2%(1%) <> 0 then L64015
            call "SETPRNT" ("HNY012", " ", 1%, 0%)
            pcntr% = -1%  :  gosub print_params
            partkey$ = lopn$
            call "READ102" (#1, partkey$, f1%(1%))
                if f1%(1%) = 0% then L22485
                goto L22420

        read_loop10
            call "READNEXT" (#1, f1%(1%))
                if f1%(1%) = 0% then L22485
L22420:     get #1 using L22525, part1$, desc1$, sellu$, buyu$, conv,     ~
                catcd$, abc$, hzn%, spec$, lead$, type$, moq$, byr1$,    ~
                byr2$, ss, pan
            if part1$ > hipn$ then L22485
            if type$ < lopt$ or type$ > hipt$ then read_loop10
            if catcd$ <= locc$ or catcd$ > hicc$ then read_loop10
            convert moq$ to moq, data gosub L22510
            convert lead$ to lead, data gosub L22515
            convert hzn%  to hzn$, pic(###)
            call "CONVERT" (pan, 0.1, pan$)
            if lcntr% > 58% then gosub page_head
            print using L60910, part1$, desc1$, sellu$, buyu$, conv,      ~
                        catcd$, abc$, spec$, type$, lead, moq, ss, pan$, ~
                        byr1$, byr2$, hzn$
            lcntr% = lcntr% + 1%
            selected% = 1%            /* Indicate 'something selected' */
            goto read_loop10

L22485:     gosub print_end_report
            if selected% = 0% then gosub null_set_askuser

            close #1
            return

L22510:     moq = 0  :  return
L22515:     lead = 0  :  return

L22525: FMT  /* HNYMASTR for report #10 */                               ~
            CH(25), CH(32), XX(16), 2*CH(4), PD(14,7), CH(4), XX(17),    ~
            CH(1) , XX(16), BI(2) , XX(36), CH(4) , CH(10), CH(3), XX(7),~
            CH(10), CH(3), XX(106), CH(3), XX(6), 2*PD(14,4)

L22550: REM *************************************************************~
            **  #11            Parts Listed at Standard Cost           **~
            *************************************************************
            call"SHOSTAT"("Printing Inventory Parts With Standard Costs")
            call "OPENCHCK" (#1, 0%, f2%(1%), 0%, rslt$(1%))
                if f2%(1%) <> 0 then L64015
            call "OPENCHCK" (#5, 0%, f2%(5%), 0%, rslt$(5%))
                if f2%(5%) <> 0 then L64530
            gosub is_management_reporting_on
            call "SETPRNT" ("HNY013", " ", 1%, 0%)
            pcntr% = -1%  :  gosub print_params
            partkey$ = lopn$
            call "READ102" (#1, partkey$, f1%(1%))
                if f1%(1%) = 0% then L22695
                goto L22640

        read_loop11
            call "READNEXT" (#1, f1%(1%))
                if f1%(1%) = 0% then L22695
L22640:     get #1 using L22720, part1$, desc$, uom$, catcd$, type$
            if part1$ > hipn$ then L22695
            if type$ < lopt$ or type$ > hipt$ then read_loop11
            if catcd$ <= locc$ or catcd$ > hicc$ then read_loop11
            call "STCCOSTS" (part1$, " ", #5, 1%, totcost)
            call "CONVERT" (totcost, 4.4, totcost$)
                mdmc$ = " "
                if mgtrpt_on$ <> "Y" then L22670
                call "MDMCCOST" (part1$, " ", #5, mdmc)
                call "CONVERT" (mdmc, 4.4, mdmc$)
L22670:     if lcntr% > 58% then gosub page_head
            print using L61050, part1$, desc$, type$, catcd$, uom$,       ~
                               totcost$, mdmc$
            lcntr% = lcntr% + 1%
            selected% = 1%            /* Indicate 'something selected' */
            goto read_loop11

L22695:     gosub print_end_report
            if selected% = 0% then gosub null_set_askuser

            close #1  :  close #5
            return

L22720: FMT  /* HNYMASTR for report #11 */                               ~
           CH(25), CH(32), POS(74), CH(4), POS(90), CH(4), POS(180), CH(3)

L22735: REM *************************************************************~
            **  #12       Part Number Alphabetical Listing             **~
            *************************************************************
            call "SHOSTAT" ("Printing Master Parts Listing Alphabetical B~
        ~y Description")
            call "OPENCHCK" (#1, 0%, f2%(1%), 0%, rslt$(1%))
                if f2%(1%) <> 0 then L64015
            recnbr% = max(10%,(val(str(rslt$(1%),17%,4%),4%)))
            call "WORKOPEN" (#6, "IO    ", (recnbr% / 2%), f2%(6%))
            call "SETPRNT" ("HNY024", " ", 1%, 0%)
            pcntr% = -1% : gosub print_params
            partkey$ = lopn$ : init(" ") dtlline$()
            call "READ102" (#1, partkey$, f1%(1%))
                if f1%(1%) = 0% then L23050
                goto L22830

        read_loop12
            call "READNEXT" (#1, f1%(1%))
                if f1%(1%) = 0% then L22870
L22830:     get #1 using L22835, part$, desc$, catcd$, type$
L22835:         FMT CH(25), CH(32), POS(90), CH(4), POS(180), CH(3)
            if part$ > hipn$ then L22870
            if type$ < lopt$ or type$ > hipt$ then read_loop12
            if catcd$ <= locc$ or catcd$ > hicc$ then read_loop12
            write #6 using L22955, desc$, part$
            goto read_loop12

L22870:     init(hex(00)) workkey$
            call "READ104" (#6, workkey$, f1%(6%))
                if f1%(6%) = 0% then L64515
            gosub page_head
            get #6 using L22955, desc$, part$
            put dtlline$(1) using L22900, desc$, part$
L22900:         FMT CH(32), CH(25)
            ctr% = 2%
            goto L22935

        read_loop12a
            gosub page_head
            init(" ") dtlline$( )
L22935:     for i = ctr% to 50
                call "READNEXT" (#6, f1%(6%))
                    if f1%(6%) = 0% then L22975
                get #6 using L22955, desc$, part$
L22955:             FMT CH(32), CH(25), XX(3)
                put dtlline$(i) using L22955, desc$, part$
            next i

L22975:     for j = 1 to 50
                call "READNEXT" (#6, f1%(6%))
                    if f1%(6%) = 0% then L23015
                get #6 using L22955, desc$, part$
                put dtlline$(j) using L23000, desc$, part$
L23000:             FMT POS(58), CH(32), CH(25)
            next j

L23015:     for k = 1 to 50
                get dtlline$(k) using L23080,desc1$,part1$,desc2$,part2$
                print using L62550, desc1$, part1$, desc2$, part2$
                selected% = 1%        /* Indicate 'something selected' */
            next k

            if f1%(6%) <> 0% then ctr% = 1%
            if f1%(6%) <> 0% then read_loop12a

L23050:     gosub print_end_report
            if selected% = 0% then gosub null_set_askuser

            close #1
            call "FILEBGON" (#6)
            return

L23080: FMT                 /* ARRAY DTLLINE$()                        */~
            CH(32),         /* 1st part description                    */~
            CH(25),         /* 1st part number                         */~
            CH(32),         /* 2nd part description                    */~
            CH(25)          /* 2nd part number                         */

L23110: REM *************************************************************~
            **  #13          Parts listed by Generic Code              **~
            *************************************************************
            call "SHOSTAT" ("Printing Parts Listing by Generic Code")
            call "OPENCHCK" (#1, 0%, f2%(1%), 0%, rslt$(1%))
                if f2%(1%) <> 0 then L64015
            recnbr% = max(10%,(val(str(rslt$(1%),17%,4%),4%)))
            call "WORKOPEN" (#6, "IO    ", (recnbr% / 2%), f2%(6%))
            call "SETPRNT" ("HNY021", " ", 1%, 0%)
            pcntr% = -1% : gosub print_params
            partkey$ = lopn$
            call "READ102" (#1, partkey$, f1%(1%))
                if f1%(1%) = 0% then L23375
                goto L23200

        read_loop13
            call "READNEXT" (#1, f1%(1%))
                if f1%(1%) = 0% then read_loop13a
L23200:     get #1 using L23405, part$, desc$, gencd$, catcd$,            ~
                                vend$, type$
            if part$ > hipn$ then read_loop13a
            if type$ < lopt$ or type$ > hipt$ then read_loop13
            if gencd$ < logc$ or gencd$ > higc$ then read_loop13
            write #6 using L23425, gencd$, part$, desc$, catcd$,          ~
                                vend$, type$
            goto read_loop13

        read_loop13a
            init(hex(00)) workkey$, oldcode$
            pcntr% = 0%
            call "READ104" (#6, workkey$, f1%(6%))
            goto L23285

L23275:     call "READNEXT" (#6, f1%(6%))
                if f1%(6%) = 0% then L23375
L23285:     get #6 using L23425,gencd$, part$, desc$, catcd$, vend$, type$
            if gencd$ = oldcode$ then L23315
            genkey$ = str(gencd$,1%,16%) & " "
            call "REDALT4" (#8, genkey$, 1%, f1%(8%))
            get #8 using L23445, gendesc$
            if gencd$ = " " then gendesc$ = "NO GENERIC CODE ASSIGNED!"
L23315:     if lcntr% > 58% then gosub page_head
            if gencd$ = oldcode$ or str(oldcode$,1,1)= hex(00) then L23330
            print  :  lcntr% = lcntr% + 1%
L23330:     if gencd$ = oldcode$ then                                    ~
                print using L62050 , " ", " ", part$, desc$, catcd$,      ~
                      vend$, type$                                       ~
                else  print using L62050 , gencd$, gendesc$, part$, desc$,~
                      catcd$, vend$, type$
            oldcode$ = gencd$
            lcntr% = lcntr% + 1%
            selected% = 1%            /* Indicate 'something selected' */
            goto L23275

L23375:     gosub print_end_report
            if selected% = 0% then gosub null_set_askuser

            close #1 : close #8
            call "FILEBGON" (#6)
            return

L23405: FMT  /* HNYMASTR for report #13  */                              ~
                          CH(25), CH(32), CH(16), XX(16), CH(4),         ~
                          XX(8), CH(9), XX(69), CH(3)

L23425: FMT  /* WORKFILE for report #13  */                              ~
                          CH(16), CH(25), XX( 9), CH(32), CH(4),         ~
                          CH(9), CH( 3)

L23445: FMT  /* HNYGENER for report #13  */                              ~
                          XX(41), CH(30)

L23460: REM *************************************************************~
            **  #14          Parts listed by Category Code             **~
            *************************************************************
            call "SHOSTAT" ("Printing Parts Listing by Category Code")
            call "OPENCHCK" (#1, 0%, f2%(1%), 0%, rslt$(1%))
                if f2%(1%) <> 0 then L64015
            call "SETPRNT" ("HNY023", " ", 1%, 0%)
            pcntr% = -1% : gosub print_params
            partkey$ = locc$
            init(hex(00)) oldcode$
            call "REDALT2" (#1, partkey$, 2%, f1%(1%))
                if f1%(1%) = 0% then L23650
                goto L23545

        read_loop14
            call "READNEXT" (#1, f1%(1%))
                if f1%(1%) = 0% then L23650
L23545:     get #1 using L23675, part$, desc$, gencd$, catcd$,            ~
                                vend$, type$
            if catcd$ > hicc$ then L23650
            if part$ < lopn$ or part$ > hipn$ then read_loop14
            if part$ = defprt$ then read_loop14
            if type$ < lopt$ or type$ > hipt$ then read_loop14
*       * Okay fine, so print it
            if catcd$ = oldcode$ then L23590
            if catcd$ = " " then L23585
            call "DESCRIBE" (#10, catcd$, gendesc$, 0%, f1%(10%))
L23585:     if catcd$ = " " then gendesc$ = "NO CATEGORY CODE ASSIGNED!"
L23590:     if lcntr% > 58% then gosub page_head
            if catcd$ = oldcode$ or str(oldcode$,1,1)= hex(00) then L23605
            print  :  lcntr% = lcntr% + 1%
L23605:     if catcd$ = oldcode$ and lcntr% > 5%  then                   ~
                print using L62410 , " ", " ", part$, desc$, gencd$,      ~
                      vend$, type$                                       ~
                else  print using L62410 , catcd$, gendesc$, part$, desc$,~
                      gencd$, vend$, type$
            oldcode$ = catcd$
            lcntr% = lcntr% + 1%
            selected% = 1%            /* Indicate 'something selected' */
            goto read_loop14

L23650:     gosub print_end_report
            if selected% = 0% then gosub null_set_askuser

            close #1 : close #10
            return

L23675: FMT  /* HNYMASTR for report #14  */                              ~
                          CH(25), CH(32), CH(16), XX(16), CH(4),         ~
                          XX(8), CH(9), POS(180), CH(3)

        FMT  /* WORKFILE for report #14  */                              ~
                          CH(4), CH(25), XX(21), CH(32), CH(16),         ~
                          CH(9), CH( 3)

L23715: REM *************************************************************~
            **  #15       Purchased parts by primary vendor code       **~
            *************************************************************
            call "SHOSTAT" ("Printing Purchased Parts Listed by Primary V~
        ~endor Code")
            call "OPENCHCK" (#1, 0%, f2%(1%), 0%, rslt$(1%))
                if f2%(1%) <> 0 then L64015
            recnbr% = max(10%,(val(str(rslt$(1%),17%,4%),4%)))
            call "WORKOPEN" (#6, "IO    ", (recnbr% / 2%), f2%(6%))
            call "SETPRNT" ("HNY022", " ", 1%, 0%)
            pcntr% = -1% : gosub print_params
            partkey$ = lopn$
            call "READ102" (#1, partkey$, f1%(1%))
                if f1%(1%) = 0% then L23980
                goto L23810

        read_loop15
            call "READNEXT" (#1, f1%(1%))
                if f1%(1%) = 0% then read_loop15a
L23810:     get #1 using L24020, part$, desc$, vend$, lead$, type$, moq$, ~
                                byr1$, byr2$, sslevel, pansize
            if part$ > hipn$ then read_loop15a
            if type$ < "200" or type$ > "499" then read_loop15
            if vend$ < lovc$ or vend$ > hivc$ then read_loop15
            write #6 using L24045, vend$, part$, desc$, lead$, moq$,      ~
                                  byr1$, byr2$, sslevel, pansize
            goto read_loop15

        read_loop15a
            init(hex(00)) workkey$, oldcode$
            call "READ104" (#6, workkey$, f1%(6%))
            goto L23885
L23875:     call "READNEXT" (#6, f1%(6%))
                if f1%(6%) = 0% then L23980
L23885:     get #6 using L24045, vend$, part$, desc$, lead$, moq$,        ~
                                byr1$, byr2$, sslevel, pansize
            if vend$ = oldcode$ then L23915
            if vend$ = " " then L23910
            call "DESCRIBE" (#9, vend$, vname$, 0%, f1%(9%))
                if f1%(9%) = 0% then vname$ = "VENDOR CODE NOT ON FILE"
L23910:     if vend$ = " " then vname$ = "NO PRIMARY VENDOR ASSIGNED!"
L23915:     if lcntr% > 58% then gosub page_head
            if vend$ = oldcode$ or str(oldcode$,1,1) = hex(00) then L23930
            print  :  lcntr% = lcntr% + 1%
L23930:     convert moq$ to moq, data gosub L24010
            if vend$ = oldcode$ and lcntr% > 6%  then                    ~
                print using L62250 , " ", " ", part$, desc$, lead$,       ~
                                   byr1$, byr2$, moq, sslevel, pansize   ~
                else  print using L62250 , vend$, vname$, part$, desc$,   ~
                      lead$, byr1$, byr2$, moq, sslevel, pansize
            oldcode$ = vend$
            lcntr% = lcntr% + 1%
            selected% = 1%            /* Indicate 'something selected' */
            goto L23875

L23980:     gosub print_end_report
            if selected% = 0% then gosub null_set_askuser

            close #1 : close #9
            call "FILEBGON" (#6)
            return

L24010:     moq = 0  :  return

L24020: FMT  /* HNYMASTR for report #15  */                              ~
                CH(25), CH(32), POS(102), CH(9), POS(170), CH(10),       ~
                CH(3),  XX(7),  CH(10), CH(3), POS(309), CH(3), XX(6),   ~
                2*PD(14,4)

L24045: FMT  /* WORKFILE for report #15  */                              ~
                          CH( 9), CH(25), XX(16), CH(32), 2*CH(10),      ~
                          2*CH(3), 2*PD(14,4)

L24065: REM *************************************************************~
            **  #16        Quantity on Hand by Store/Lot               **~
            *************************************************************
            call "SHOSTAT" ("Printing Inventory Quantities by Store/Lot")
            call "OPENCHCK" (#1, 0%, f2%(1%), 0%, rslt$(1%))
                if f2%(1%) <> 0 then L64015
            call "OPENCHCK" (#3, 0%, f2%(3%), 0%, rslt$(3%))
                if f2%(3%) <> 0 then L64110
                if val(str(rslt$(3%),17%,4%),4%) = 0% then L64110
            call "SETPRNT" ("HNY015", " ", 1%, 0%)
            pcntr% = -1% : gosub print_params
            recnbr% = max(10%,(val(str(rslt$(3%),17%,4%),4%)))
            call "WORKOPEN" (#6, "IO    ", (recnbr% / 2%), f2%(6%))

        read_loop16
            call "READNEXT" (#3, f1%(3%))
                if f1%(3%) = 0% then L24190
            get #3 using L24360, part1$, stor$, lot$, binloc$, qtyoh,     ~
                                qtybo, qtyonord, qtycom, qtyqc
            if stor$  <= lost$ or  stor$  > hist$ then read_loop16
            if part1$ < lopn$ or  part1$ > hipn$ then read_loop16

            call "READ100" (#1, part1$, f1%(1%))
            if f1%(1%) = 1% then L24164
                desc1$ = "PART NOT ON MASTER FILE !"
                goto L24170

L24164:     get #1 using L24165, desc1$, catcd$, type$
L24165:         FMT POS(26), CH(32), POS(90), CH(4), POS(180), CH(3)
            if type$  < lopt$ or  type$  > hipt$ then read_loop16
            if catcd$ < locc$ or  catcd$ > hicc$ then read_loop16

L24170:     write #6 using L24345, stor$, part1$, lot$, desc1$, qtyoh,    ~
                                  qtybo, qtyonord, qtycom, qtyqc, binloc$
            goto read_loop16

L24190:     init(hex(00)) workkey$
            oldpart$, oldstor$ = " "
            call "READ104" (#6, workkey$, f1%(6%))

            goto L24230

        print_loop16
            call "READNEXT" (#6, f1%(6%))
L24230:         if f1%(6%) <> 0% then L24240
                    gosub print_part_total :  goto L24315
L24240:     get #6 using L24345, stor$, part1$, lot$, desc1$, qtyoh,      ~
                                  qtybo, qtyonord, qtycom, qtyqc, binloc$
            if oldpart$ = part1$ or oldpart$ = " " then L24249
                gosub print_part_total
L24249:     if lcntr% > 58% then gosub page_head
            call "CONVERT" (qtyoh   , 0.2, print$(1%))
            call "CONVERT" (qtybo   , 0.2, print$(2%))
            call "CONVERT" (qtyonord, 0.2, print$(3%))
            call "CONVERT" (qtycom  , 0.2, print$(4%))
            call "CONVERT" (qtyqc   , 0.2, print$(5%))
            if stor$ = oldstor$ or oldstor$ = " " then L24285
                print            :  lcntr% = lcntr% + 1%
                part_cnt% =  0%  :  oldpart$ = " "
L24285:     print using L61330 , stor$, lot$, part1$, desc1$, binloc$,    ~
                print$(1%), print$(2%), print$(3%), print$(4%), print$(5%)
            selected% = 1%            /* Indicate 'something selected' */
            oldstor$  = stor$
            oldpart$  = part1$
            part_cnt% = part_cnt% + 1%
            qtyoh_tot = qtyoh_tot + qtyoh
            qtybo_tot = qtybo_tot + qtybo
            qtyon_tot = qtyon_tot + qtyonord
            qtyco_tot = qtyco_tot + qtycom
            qtyqc_tot = qtyqc_tot + qtyqc
            lcntr%    = lcntr%    + 1%

            goto print_loop16

L24315:     gosub print_end_report
            if selected% = 0% then gosub null_set_askuser

            close #1 : close #3
            call "FILEBGON" (#6)
            return

L24345: FMT  /* WORKFILE for report #16     */                           ~
            CH(3), CH(25), CH(6), XX(26), CH(32), 5*PD(14,4), CH(8)

L24360: FMT  /* HNYQUAN  for report #16     */                           ~
            POS(17), CH(25), CH(3), CH(16), CH(8), POS(69), 5*PD(14,4)

L24375: REM *************************************************************~
            **  #17       Obsolete Part Number  Listing                **~
            *************************************************************
            call "SHOSTAT" ("Printing Obsolete Part Number Listing")
            call "OPENCHCK" (#1, 0%, f2%(1%), 0%, rslt$(1%))
                if f2%(1%) <> 0 then L64015
            call "SETPRNT" ("HNY018", " ", 1%, 0%)
            pcntr% = 0%

        read_loop17
            gosub page_head
            init(" ") dtlline$( )
            for i = 1 to 50
L24440:         call "READNEXT" (#1, f1%(1%))
                    if f1%(1%) = 0% then L24530
                get #1 using L24455, part1$, desc1$, obso$
L24455:             FMT CH(25), CH(32), POS(166), CH(4)
                if obso$ <> "OBSO" then L24440
                put dtlline$(i) using L24470, part1$, desc1$
L24470:             FMT POS(1), CH(25), POS(26), CH(32)
            next i

            for j = 1 to 50
L24490:         call "READNEXT" (#1, f1%(1%))
                    if f1%(1%) = 0% then L24530
                get #1 using L24455, part2$, desc2$, obso$
                if obso$ <> "OBSO" then L24490
                put dtlline$(j) using L24515, part2$, desc2$
L24515:             FMT POS(58), CH(25), POS(83), CH(32)
            next j

L24530:     for k = 1 to 50
                get dtlline$(k) using L24590,part1$,desc1$,part2$,desc2$
                if part1$ = " " then L24560
                print using L60720 , part1$, desc1$, part2$, desc2$
                selected% = 1%        /* Indicate 'something selected' */
            next k

L24560:     if f1%(1) <> 0 then read_loop17
            gosub print_end_report
            if selected% = 0% then gosub null_set_askuser

            close #1
            return

L24590: FMT  /* Detail array report #17 */                               ~
            CH(25), CH(32), CH(25), CH(32)

L24605: REM *************************************************************~
            **  #18       Parts with no W/U or BOM                     **~
            *************************************************************
            call "SHOSTAT" ("Printing List of MFG & Generic Parts That Ha~
        ~ve NO W/U or Bill Of Materials")
            call "OPENCHCK" (#1, 0%, f2%(1%), 0%, rslt$(1%))
                if f2%(1%) <> 0 then L64015
            call "OPENCHCK" (#4, 0%, f2%(4%), 0%, rslt$(4%))
                if f2%(4%) <> 0 then L64170
                if val(str(rslt$(4%),17%,4%),4%) = 0% then L64170
            pcntr% = 0%  :  lcntr% = 99%
            call "SETPRNT" ("HNY019", " ", 1%, 0%)

        read_loop18
            init(" ") desc1$, desc2$, and$, typ$
L24680:     init(hex(00)) workkey$
            call "READNEXT" (#1, f1%(1%))
               if f1%(1%) = 0% then L24790
            get #1 using L24700, part$, desc$, type$
L24700:         FMT CH(25), CH(32), POS(180), CH(3)
            if type$ = "000" then L24715
            if type$ < "500" then read_loop18
L24715:     str(workkey$,1%,25%) = part$
            call "PLOWALTS" (#4, workkey$, 1%, 25%, f1%(4%))
            if f1%(4%) = 0% then desc1$ = "NOT USED"
            init(hex(00)) workkey$
            str(workkey$,1%,25%) = part$
            call "PLOWNEXT" (#4, workkey$, 25%, f1%(4%))
                if f1%(4%) = 0% then desc2$ = "HAS NO BILL OF MATERIALS"
            if desc1$ = " " and desc2$ = " " then L24680
            if desc1$ <> " " and desc2$ <> " " then and$ = "AND"
            if type$ = "000" then typ$ = "YES"
            if lcntr% > 58% then gosub page_head
            print using L61770 , part$, desc$, desc1$, and$, desc2$, typ$
            lcntr% = lcntr% + 1%
            selected% = 1%            /* Indicate 'something selected' */
            goto read_loop18

L24790:     gosub print_end_report
            if selected% = 0% then gosub null_set_askuser

            close #1 : close #4
            return

L24815: REM *************************************************************~
            **  #19       Purchased parts that are not used in BOM     **~
            *************************************************************
            call "SHOSTAT" ("Printing List of Purchased Parts That Are NO~
        ~T Used in a Bill of Materials")
            call "OPENCHCK" (#1, 0%, f2%(1%), 0%, rslt$(1%))
                if f2%(1%) <> 0 then L64015
            call "OPENCHCK" (#4, 0%, f2%(4%), 0%, rslt$(4%))
                if f2%(4) <> 0 then L64170
                if val(str(rslt$(4%),17%,4%),4%) = 0% then L64170
            call "SETPRNT" ("HNY020", " ", 1%, 0%)
            pcntr% = 0%  :  lcntr% = 99%
            init(hex(00)) partkey$
            call "READ102" (#1, partkey$, f1%(1%))
                if f1%(1%) = 0% then L24980
                goto L24920
        read_loop19
            init(" ") text$
            init(hex(00)) workkey$
            call "READNEXT" (#1, f1%(1%))
               if f1%(1%) = 0% then L24980
L24920:     get #1 using L24925, part$, desc$, type$
L24925:         FMT CH(25), CH(32), POS(180), CH(3)
            if type$ < "200" or type$ > "499" then read_loop19
            str(workkey$,1%,25%) = part$
            call "PLOWALTS" (#4, workkey$, 1%, 25%, f1%(4%))
            if f1%(4%) <> 0% then read_loop19
            text$ = "NOT USED IN ANY BILL OF MATERIALS"
            if lcntr% > 58% then gosub page_head
            print using L61900 , part$, desc$, text$
            lcntr% = lcntr% + 1%
            selected% = 1%            /* Indicate 'something selected' */
            goto read_loop19

L24980:     gosub print_end_report
            if selected% = 0% then gosub null_set_askuser

            close #1 : close #4
            return

        print_end_report
            time$ = " "  :  call "TIME" (time$)
            print skip(2)
            print using L63250, time$
            close printer
            return

        print_part_total
            if part_cnt% < 2% then L25180
            call "CONVERT" (qtyoh_tot, 0.2, print$(1%))
            call "CONVERT" (qtybo_tot, 0.2, print$(2%))
            call "CONVERT" (qtyon_tot, 0.2, print$(3%))
            call "CONVERT" (qtyco_tot, 0.2, print$(4%))
            call "CONVERT" (qtyqc_tot, 0.2, print$(5%))
            tot_msg$ = "** PART " & oldpart$ & " TOTAL **"
            call "RJUSTIFY" (tot_msg$)

            print using L61362, tot_msg$  , print$(1%), print$(2%),       ~
                               print$(3%), print$(4%), print$(5%)
            print
            lcntr% = lcntr% + 2%
L25180:     qtyoh_tot, qtybo_tot, qtyon_tot, qtyco_tot, qtyqc_tot = 0.0
            part_cnt% = 0%
            return

        null_set_askuser
            call "ASKUSER" (0%, "*** NULL SET SELECTED ***",             ~
                "There are no parts that match the criteria you entered"&~
                ".", " ", "Press any PF key to acknowledge and continue"&~
                ".")
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************
        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

        REM *************************************************************~
            * Page Headings etc...                                      *~
            *************************************************************

        page_head
            print page
            pcntr% = pcntr% + 1%
            on rpt% gosub   L30280 ,  /*  1 Parts Listing with Text     */~
                            L30370 ,  /*  2 Inactive                    */~
                            L30470 ,  /*  3 Parts with Alternates       */~
                            L30560 ,  /*  4 Parts with Options          */~
                            L30650 ,  /*  5 Negative Inventory Report   */~
                            L30740 ,  /*  6 Parts at zero cost          */~
                            L30840 ,  /*  7 Parts with QTY under MIN    */~
                            L30930 ,  /*  8 Parts with QTY over MAX     */~
                            L31020 ,  /*  9 Part Number Reference List  */~
                            L31100 ,  /* 10 Master Parts List           */~
                            L31200 ,  /* 11 Parts at Standard Cost      */~
                            L31300 ,  /* 12 Alphabetical Listing        */~
                            L31390 ,  /* 13 Parts by generic Code       */~
                            L31480 ,  /* 14 Parts by category code      */~
                            L31570 ,  /* 15 Purch by primary vendor     */~
                            L31670 ,  /* 16 Qty on hand                 */~
                            L31760 ,  /* 17 Obsolete                    */~
                            L31850 ,  /* 18 Mfg parts no W/U or BOM     */~
                            L31940    /* 19 Purch no W/U                */
                return

L30280: REM *****  #1  Parts Listing w/ Text        *****
            print using L63280 , date$, time$, name$, pcntr%
            print using L60590
            print
            if pcntr% = 0% then return
            print using L63140
            print using L63160
            lcntr% = 5%
            return

L30370: REM *****  #2  Inactive Parts Listing        *****
            print using L63280 , date$, time$, name$, pcntr%
            print using L60230 , days%
            print
            if pcntr% = 0% then return
            print using L61550
            print using L61580
            print using L61610
            lcntr% = 6%
            return

L30470: REM *****  #3  Parts with Alternates        *****
            print using L63280 , date$, time$, name$, pcntr%
            print using L60470
            print
            if pcntr% = 0% then return
            print using L62620
            print using L62650
            lcntr% = 5%
            return

L30560: REM *****  #4  Parts with Options           *****
            print using L63280 , date$, time$, name$, pcntr%
            print using L60500
            print
            if pcntr% = 0% then return
            print using L62750
            print using L62780
            lcntr% = 5%
            return

L30650: REM *****  #5  Negative Inventory Listing     *****
            print using L63280 , date$, time$, name$, pcntr%
            print using L60140
            print
            if pcntr% = 0% then return
            print using L61120
            print using L61150
            lcntr% = 5%
            return

L30740: REM *****  #6  Parts in stock at zero cost       *****
            print using L63280 , date$, time$, name$, pcntr%
            print using L60200
            print
            if pcntr% = 0% then return
            print using L61410
            print using L61440
            lcntr% = 6%
            return

L30840: REM *****  #7  Parts under MINimum          *****
            print using L63280 , date$, time$, name$, pcntr%
            print using L60530
            print
            if pcntr% = 0% then return
            print using L62880
            print using L62910
            lcntr% = 5%
            return

L30930: REM *****  #8  Parts over MAXimum           *****
            print using L63280 , date$, time$, name$, pcntr%
            print using L60560
            print
            if pcntr% = 0% then return
            print using L63010
            print using L63040
            lcntr% = 5%
            return

L31020: REM *****  #9 Part Number Reference List     *****
            print using L63280 , date$, time$, name$, pcntr%
            print using L60050
            print
            if pcntr% = 0% then return
            print using L60660
            print using L60690
            return

L31100: REM *****  #10 Master Parts Listing           *****
            print using L63280 , date$, time$, name$, pcntr%
            print using L60080
            print
            if pcntr% = 0% then return
            print using L60790
            print using L60830
            print using L60870
            lcntr% = 6%
            return

L31200: REM *****  #11  Parts list at Standard Cost    *****
            print using L63280 , date$, time$, name$, pcntr%
            print using L60110
            print
            if pcntr% = 0% then return
            print using L60990, mdmc_print1$
            print using L61020, mdmc_print2$
            lcntr% = 5%
            return

L31300: REM *****  #12  Alphabetical Parts Listing   *****
            print using L63280 , date$, time$, name$, pcntr%
            print using L60440
            print
            if pcntr% = 0% then return
            print using L62490
            print using L62520
            lcntr% = 5%
            return

L31390: REM *****  #13  Parts by Generic Code        *****
            print using L63280 , date$, time$, name$, pcntr%
            print using L60350
            print
            if pcntr% = 0% then return
            print using L61970
            print using L62010
            lcntr% = 5%
            return

L31480: REM *****  #14  Parts Listed By Category     *****
            print using L63280 , date$, time$, name$, pcntr%
            print using L60410
            print
            if pcntr% = 0% then return
            print using L62330
            print using L62370
            lcntr% = 5%
            return

L31570: REM *****  #15  Purch parts by primary vendor   *****
            print using L63280 , date$, time$, name$, pcntr%
            print using L60380
            print
            if pcntr% = 0% then return
            print using L62130
            print using L62170
            print using L62210
            lcntr% = 6%
            return

L31670: REM *****  #16  Quantities by Store/Lot           *****
            print using L63280 , date$, time$, name$, pcntr%
            print using L60170
            print
            if pcntr% = 0% then return
            print using L61250
            print using L61290
            lcntr% = 5%
            return

L31760: REM *****  #17  Obsolete Parts Listing        *****
            print using L63280 , date$, time$, name$, pcntr%
            print using L60260
            print
            if pcntr% = 0% then return
            print using L60660
            print using L60690
            lcntr% = 5%
            return

L31850: REM *****  #18  Mfg parts with no W/U or BOM  *****
            print using L63280 , date$, time$, name$, pcntr%
            print using L60290
            print
            if pcntr% = 0% then return
            print using L61710
            print using L61740
            lcntr% = 5%
            return

L31940: REM *****  #19  Purch parts not used         *****
            print using L63280 , date$, time$, name$, pcntr%
            print using L60320
            print
            if pcntr% = 0% then return
            print using L61840
            print using L61870
            lcntr% = 5%
            return


*       *****************************************************************
*           Print Range Selections Screen as Page #0                    *
*       *****************************************************************

        print_params
            gosub page_head
L33085:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L33110
                str(i$(), i%, 1%) = hex(20)
                goto L33085
L33110:     print skip(5)
            print tab(37);
            print "--------------------- Report Selection Parameters ----~
        ~---------"
            for x% = 6% to 17%: print tab(37); i$(x%) : next x%
            print tab(37);
            print "------------------------------------------------------~
        ~---------"
            lcntr% = 99%
            return

        REM *************************************************************~
            *     I N P U T   S C R E E N   R E P O R T   T Y P E       *~
            *************************************************************

        deffn'101
            str(line2$,,60) = " "
            str(rptdescr$(1%),,2) = hex(0b8c)
L40100:     accept                                                       ~
               at (01,02),                                               ~
                  "Part Number List Selections",                         ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), fac(hex(80))  , rptdescr$( 1),                ~
               at (06,02), fac(hex(8e))  , rptdescr$( 1),                ~
               at (07,02), fac(hex(8e))  , rptdescr$( 2),                ~
               at (08,02), fac(hex(8e))  , rptdescr$( 3),                ~
               at (09,02), fac(hex(8e))  , rptdescr$( 4),                ~
               at (10,02), fac(hex(8e))  , rptdescr$( 5),                ~
               at (11,02), fac(hex(8e))  , rptdescr$( 6),                ~
               at (12,02), fac(hex(8e))  , rptdescr$( 7),                ~
               at (13,02), fac(hex(8e))  , rptdescr$( 8),                ~
               at (14,02), fac(hex(8e))  , rptdescr$( 9),                ~
               at (15,02), fac(hex(8e))  , rptdescr$(10),                ~
               at (06,41), fac(hex(8e))  , rptdescr$(11),                ~
               at (07,41), fac(hex(8e))  , rptdescr$(12),                ~
               at (08,41), fac(hex(8e))  , rptdescr$(13),                ~
               at (09,41), fac(hex(8e))  , rptdescr$(14),                ~
               at (10,41), fac(hex(8e))  , rptdescr$(15),                ~
               at (11,41), fac(hex(8e))  , rptdescr$(16),                ~
               at (12,41), fac(hex(8e))  , rptdescr$(17),                ~
               at (13,41), fac(hex(8e))  , rptdescr$(18),                ~
               at (14,41), fac(hex(8e))  , rptdescr$(19),                ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13% then L40920
                  call "MANUAL" ("HNYRPTS")
                  goto L40100

L40920:        if keyhit% <> 15% then L41000
                  call "PRNTSCRN"
                  goto L40100

L41000:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *        I N P U T   M O D E , R A N G E   S E L E C T      *~
            *************************************************************

        deffn'102(fieldnr%,mode%)
            str(line2$,,60) = "Range Selection for " &                   ~
                               str(rptdescr$(rpt%),3)
            if fieldnr% = 0% then inpmessage$ = edtmessage$ else         ~
                                  inpmessage$ = inpmsg$(fieldnr%)
            if fieldnr% > 0% then init(hex(8c)) ifac$()                  ~
                             else init(hex(86)) ifac$()
            gosub setpf2
            on fieldnr% gosub L41250,            /* Part Number Range   */~
                              L41250,            /* Part Type Range     */~
                              L41250,            /* Store Code Range    */~
                              L41250,            /* Part Category Range */~
                              L41250,            /* Vendor Code Range   */~
                              L41250,            /* Generic Desig Range */~
                              L41260             /* Inactive Days       */
            goto L41280

L41250:         ifac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L41260:         ifac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41280:     accept                                                       ~
               at (01,02),                                               ~
                  "Part Number List Selections",                         ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,25), fac(hex(ac))  , fromtitl$,             ch(25),~
               at (06,52), fac(hex(ac))  , totitl$  ,             ch(25),~
               at (07,02), fac(rfac$( 1)), disp_rng$(1%),         ch(22),~
               at (07,25), fac(ifac$( 1)), frpn$    ,             ch(25),~
               at (07,52), fac(ifac$( 1)), topn$    ,             ch(25),~
               at (08,02), fac(rfac$( 2)), disp_rng$(2%),         ch(22),~
               at (08,25), fac(ifac$( 2)), frpt$    ,             ch( 3),~
               at (08,52), fac(ifac$( 2)), topt$    ,             ch( 3),~
               at (09,02), fac(rfac$( 3)), disp_rng$(3%),         ch(22),~
               at (09,25), fac(ifac$( 3)), frst$    ,             ch( 3),~
               at (09,52), fac(ifac$( 3)), tost$    ,             ch( 3),~
               at (10,02), fac(rfac$( 4)), disp_rng$(4%),         ch(22),~
               at (10,25), fac(ifac$( 4)), frcc$    ,             ch( 4),~
               at (10,52), fac(ifac$( 4)), tocc$    ,             ch( 4),~
               at (11,02), fac(rfac$( 5)), disp_rng$(5%),         ch(22),~
               at (11,25), fac(ifac$( 5)), frvc$    ,             ch( 9),~
               at (11,52), fac(ifac$( 5)), tovc$    ,             ch( 9),~
               at (12,02), fac(rfac$( 6)), disp_rng$(6%),         ch(22),~
               at (12,25), fac(ifac$( 6)), frgc$    ,             ch(16),~
               at (12,52), fac(ifac$( 6)), togc$    ,             ch(16),~
               at (13,02), fac(rfac$( 7)), disp_rng$(7%),         ch(22),~
               at (13,25), fac(ifac$( 7)), days$    ,             ch( 3),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 13% then L41710
                  call "MANUAL" ("HNYRPTS")
                  goto L41280

L41710:        if keyhit% <> 15% then L41750
                  call "PRNTSCRN"
                  goto L41280

L41750:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        setpf2
        if mode% = 2% then L41950     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                 (4)Previous Field      " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Program"

            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L41910
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L41910:     if fieldnr% > 1% then L41930
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41930:     return

L41950: if fieldnr% > 0% then L42060  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Print Report"

            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
                                     /*  Edit Mode - Enabled    */
L42060:     pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *************************************************************
        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50120,            /* Part Number Range   */~
                              L50160,            /* Part Type Range     */~
                              L50200,            /* Store Code Range    */~
                              L50260,            /* Part Category Range */~
                              L50330,            /* Vendor Code Range   */~
                              L50400,            /* Generic Desig Range */~
                              L50470             /* Inactive Days       */
                return
L50120: REM Test for Part Number Range
            call "OPENCHCK" (#1, 0%, f2%(1%), 0%, rslt$(1%))
                if f2%(1%) <> 0 then L64015
                if val(str(rslt$(1%),17%,4%),4%) = 0% then L64015
            call "TESTRNGE" (frpn$, topn$, lopn$, hipn$, errormsg$, #1)
            return

L50160: REM Test for Part Type Range
            if frpt$ <> "ALL" then L50170
               frpt$ = "000" : topt$ = "999"
L50170:     call "NUMTEST" (frpt$, 0, 999, errormsg$, 0.0, temp)
                if errormsg$ <> " " then return
            convert temp to frpt$, pic(000)
            if topt$ = " " then topt$ = frpt$
            call "NUMTEST" (topt$, 0, 999, errormsg$, 0.0, temp)
                if errormsg$ <> " " then return
            convert temp to topt$, pic(000)
            if frpt$ <= topt$ then L50182
                errormsg$ = "Invalid Range, Please Re-enter"
                return
L50182:     lopt$ = frpt$ : hipt$ = topt$
            return

L50200: REM Test for Store Range
            call "TESTRNGE" (frst$, tost$, lost$, hist$, errormsg$    )
            return

L50260
*       **  Test Range for Category Code
            call "OPENCHCK" (#10, 0%, f2%(10%), 0%, rslt$(10%))
                if f2%(10%) <> 0 then L64350
                if val(str(rslt$(10%),17%,4%),4%) = 0% then L64350
            call "TESTRNGE" (frcc$, tocc$, locc$, hicc$, errormsg$, #10)
            return

L50330
*       **  Test range for Vendor Code
            call "OPENCHCK" (#9, 0%, f2%(9%), 0%, rslt$(9%))
                if f2%(9%) <> 0 then L64290
                if val(str(rslt$(9%),17%,4%),4%) = 0% then L64290
            call "TESTRNGE" (frvc$, tovc$, lovc$, hivc$, errormsg$, #09)
            return

L50400
*       **  Test range for Generic Code
            call "OPENCHCK" (#8, 0%, f2%(8%), 0%, rslt$(8%))
                if f2%(8%) <> 0 then L64230
                if val(str(rslt$(8%),17%,4%),4%) = 0% then L64230
            call "TESTRNGE" (frgc$, togc$, logc$, higc$, errormsg$, #08)
            logc$ = str(logc$,1,16) /* CODE TO ELIMINATE ADDC PROB. */
            return

L50470
*       **  Test for number of inactive days
            convert days$ to days, data goto L50510
            days% = days
            return
L50510:     errormsg$ = "Invalid Entry for Number of Inactive Days"
            return

        REM *************************************************************~
            *   A N D   N O W   T H E   I M A G E   S T A T E M E N T S *~
            *************************************************************
        REM ***** First the header lines

L60050: %                                                  PART NUMBER RE~
        ~FERENCE LISTING                                   HNYRPTS: HNY011

L60080: %                                                  MASTER INVENTO~
        ~RY PARTS LISTING                                  HNYRPTS: HNY012

L60110: %                                                 PARTS LISTED WI~
        ~TH STANDARD COSTS                                 HNYRPTS: HNY013

L60140: %                                                    NEGATIVE INV~
        ~ENTORY LISTING                                    HNYRPTS: HNY014

L60170: %                                                 INVENTORY QUANT~
        ~ITIES BY STORE/LOT                                HNYRPTS: HNY015

L60200: %                                                    PARTS IN STO~
        ~CK AT ZERO COST                                   HNYRPTS: HNY016

L60230: %                                   PARTS WITH NO INVENTORY ACTIV~
        ~ITY IN THE LAST ### DAYS                          HNYRPTS: HNY017

L60260: %                                                      PARTS FLAG~
        ~GED OBSOLETE                                      HNYRPTS: HNY018

L60290: %                                       MANUFACTURED AND GENERIC ~
        ~PARTS WITH NO W/U OR BOM                          HNYRPTS: HNY019

L60320: %                                          PURCHASED PARTS NOT US~
        ~ED IN ANY BILL OF MATERIALS                       HNYRPTS: HNY020

L60350: %                                              INVENTORY PARTS LI~
        ~STED BY GENERIC CODE                              HNYRPTS: HNY021

L60380: %                                                PURCHASED PARTS ~
        ~BY PRIMARY VENDOR                                 HNYRPTS: HNY022

L60410: %                                                  PARTS LISTED B~
        ~Y CATEGORY CODE                                   HNYRPTS: HNY023

L60440: %                                           PARTS LISTING - ALPHA~
        ~BETICAL BY DESCRIPTION                            HNYRPTS: HNY024

L60470: %                                                       PARTS WIT~
        ~H ALTERNATES                                      HNYRPTS: HNY025

L60500: %                                                         PARTS W~
        ~ITH OPTIONS                                       HNYRPTS: HNY026

L60530: %                                    PARTS IN STOCK WITH QUANTITY~
        ~ LESS THAN DESIGNATED MINIMUM                     HNYRPTS: HNY027

L60560: %                                   PARTS IN STOCK WITH QUANTITY ~
        ~GREATER THAN DESIGNATED MAXIMUM                   HNYRPTS: HNY028

L60590: %                                                    PARTS LISTIN~
        ~G WITH TEXT                                       HNYRPTS: HNY035

        REM *************************************************************~
            **       Now the lines for report #9  &  #17               **~
            *************************************************************

L60660: %PART NUMBER                DESCRIPTION                          ~
        ~      PART NUMBER                DESCRIPTION

L60690: %-------------------------  --------------------------------     ~
        ~      -------------------------  --------------------------------

L60720: %#########################  ################################     ~
        ~      #########################  ################################

        REM *************************************************************~
            **                Lines for report #10                     **~
            *************************************************************

L60790: %                                                              UO~
        ~M    CONVERT CAT AB SPEC PRT LEAD         SAFETY PROCURE BYR SCH ~
        ~ATC

L60830: %PART NUMBER               PART DESCRIPTION                 STK  ~
        ~SELL  FACTOR CODE C OBSO TYP TIME    MOQ   STOCK INCRMNT CLS CLS ~
        ~HZN

L60870: %------------------------- -------------------------------- ---- ~
        ~---- ------- ---- - ---- --- --- ------- ------- ------- --- --- ~
        ~---

L60910:   %######################### ################################ ###~
        ~# #### ####.## #### # #### ### ### ####### ####### ####### ### ##~
        ~# ###

        REM *************************************************************~
            **              Lines for report #11                       **~
            *************************************************************

L60990: %PART NUMBER                DESCRIPTION                          ~
        ~PART TYPE   CATEGORY    STK UOM    TOTAL STD COST  ##############

L61020: %-------------------------  --------------------------------     ~
        ~---------   --------    -------    --------------  ##############

L61050: %#########################  ################################     ~
        ~###         ####        ####           ##########      ##########

        REM *************************************************************~
            **              Lines for report #5                        **~
            *************************************************************

L61120: %PART NUMBER               PART DESCRIPTION                   CAT~
        ~    TYPE   STORE   LOT             QUANTITY

L61150: %------------------------- --------------------------------   ---~
        ~-   ----   ------  ------   ---------------

L61180: %######################### ################################   ###~
        ~#   ###       ###  ######   ##########.####-

        REM *************************************************************~
            **              Lines for report #16                       **~
            *************************************************************

L61250: % STORE/LOT     PART NUMBER               PART DESCRIPTION       ~
        ~          BIN ID.    ON HAND BACKORDER  ON ORDER COMMITTED    IN ~
        ~QC

L61290: % ----- ------  ------------------------- -----------------------~
        ~--------- -------- --------- --------- --------- --------- ------~
        ~---

L61330: %   ### ######  ######################### #######################~
        ~######### ######## ######### ######### ######### ######### ######~
        ~###

L61362: %                                        ########################~
        ~#################  ######### ######### ######### ######### ######~
        ~###

        REM *************************************************************~
            **            Now the lines for report #6                  **~
            *************************************************************

L61410: %PART NUMBER               PART DESCRIPTION                CAT   ~
        ~PART TYPE    STORE  LOT      TOTAL COST   QTY ON HAND

L61440: %------------------------- ------------------------------  ----  ~
        ~------------ -----  ------  -----------  ------------

L61470: %######################### ##############################  ####  ~
        ~############   ###  ######  ######.####  #######.####-


        REM *************************************************************~
            **            Now the lines for report #2                  **~
            *************************************************************

L61550: %                                                                ~
        ~    ----------- LAST TRANSACTION ----------

L61580: %PART NUMBER               PART DESCRIPTION                   TYP~
        ~E   DATE      USER  TRANSACTION DESCRIPTION

L61610: %------------------------- --------------------------------   ---~
        ~-   --------  ----  ----------------------------------------

L61640: %######################### ################################   ###~
        ~    ########  ###   ########################################

        REM *************************************************************~
            **            Now the lines for report #18                 **~
            *************************************************************

L61710: %PART NUMBER                PART DESCRIPTION                     ~
        ~ USAGE?        BILL OF MATERIALS?       GENERIC?

L61740: %-------------------------  --------------------------------     ~
        ~ -------- --- ------------------------    ---

L61770: %#########################  ################################     ~
        ~ ######## ### ########################    ###

        REM *************************************************************~
            **            Now the lines for report #19                 **~
            *************************************************************

L61840: %PART NUMBER                PART DESCRIPTION                     ~
        ~ BILL OF MATERIALS?

L61870: %-------------------------  --------------------------------     ~
        ~ ----------------------------------------

L61900: %#########################  ################################     ~
        ~ ########################################

        REM *************************************************************~
            **            Now the lines for report #13                 **~
            *************************************************************

L61970: %GENERIC CODE     GENERIC CODE DESCRIPTION       PART NUMBER     ~
        ~          PART DESCRIPTION                   CTGY  VENDOR     TYP~
        ~E

L62010: %---------------- ------------------------------ ----------------~
        ~--------- --------------------------------   ----  ---------   --~
        ~-

L62050: %################ ############################## ################~
        ~######### ################################   ####  #########   ##~
        ~#

        REM *************************************************************~
            **            Now the lines for report #15                 **~
            *************************************************************

L62130: %VENDOR                                                          ~
        ~                              LEAD          MINIMUM  SAFETY  PROC~
        ~URE

L62170: % CODE     VENDOR NAME                    PART NUMBER            ~
        ~   DESCRIPTION (PARTIAL)      TIME  BUYERS    ORDER   STOCK  INCR~
        ~MNT

L62210: %--------- ------------------------------ -----------------------~
        ~-- -------------------------  ----  --- ---  ------  ------  ----~
        ~--

L62250: %######### ############################## #######################~
        ~## #########################  ####  ### ###  ######  ######  ####~
        ~##

        REM *************************************************************~
            **            Now the lines for report #14                 **~
            *************************************************************

L62330: %  CTGY   CATEGORY CODE DESCRIPTION      PART NUMBER             ~
        ~  PART DESCRIPTION                  GENERIC CODE     VENDOR    TY~
        ~PE

L62370: %  ----   ------------------------------ ------------------------~
        ~- --------------------------------  ---------------- --------- --~
        ~--

L62410: %  ####   ############################## ########################~
        ~# ################################  ################ ######### ##~
        ~##

        REM *************************************************************~
            **            Now the lines for report #12                 **~
            *************************************************************

L62490: %DESCRIPTION                       PART NUMBER                   ~
        ~DESCRIPTION                       PART NUMBER

L62520: %--------------------------------  -------------------------     ~
        ~--------------------------------  -------------------------

L62550: %################################  #########################     ~
        ~################################  #########################

        REM *************************************************************~
            **            Now the lines for report #3                  **~
            *************************************************************

L62620: %PART NUMBER                PART DESCRIPTION                     ~
        ~ALTERNATE PART NUMBER      ALTERNATE PART DESCRIPTION

L62650: %-------------------------  --------------------------------     ~
        ~-------------------------  --------------------------------

L62680: %#########################  ################################     ~
        ~#########################  ################################

        REM *************************************************************~
            **            Now the lines for report #4                  **~
            *************************************************************

L62750: %PART NUMBER                PART DESCRIPTION                     ~
        ~OPTION PART NUMBER         OPTION PART DESCRIPTION

L62780: %-------------------------  --------------------------------     ~
        ~-------------------------  --------------------------------

        %#########################  ################################     ~
        ~#########################  ################################

        REM *************************************************************~
            **            Now the lines for report #7                  **~
            *************************************************************

L62880: %STR PART NUMBER               PART DESCRIPTION                 C~
        ~AT  TYP   OPEN S.O.    ON ORDER  QTY ON HAND     MINIMUM  QTY UND~
        ~ER

L62910: %--- ------------------------- -------------------------------- -~
        ~--- --- ----------- ----------- ------------ ----------- --------~
        ~--

L62940: %### ######################### ################################ #~
        ~### ### ########.## ########.## ########.##- ########.## #######.~
        ~##

        REM *************************************************************~
            **            Now the lines for report #8                  **~
            *************************************************************

L63010: %STORE PART NUMBER               PART DESCRIPTION                ~
        ~  CAT   TYPE       QTY ON HAND           MAXIMUM          QTY OVE~
        ~R

L63040: %----- ------------------------- --------------------------------~
        ~  ----  ----  ----------------   ---------------   --------------~
        ~-

L63070: % ###  ######################### ################################~
        ~  ####  ###    ##########.####-  ##########.####   ##########.###~
        ~#-

        REM *************************************************************~
            **            Now the lines for report #1                  **~
            *************************************************************

L63140: %PART NUMBER               PART DESCRIPTION                 TEXT

L63160: %------------------------- -------------------------------- -----~
        ~-----------------------------------------------------------------

L63190: %######################### ################################

        REM *************************************************************~
            **            END OF REPORT LINE                           **~
            *************************************************************

L63250: %                                          ********** END OF REPO~
        ~RT  @ ######## **********

L63280: %RUN DATE: ######## @ ########        ###########################~
        ~#################################                    PAGE: ###

        REM *************************************************************~
            **       Error routines for files not open or empty        **~
            *************************************************************
L64015: REM ****** Error for HNYMASTR
            hi$= "The PART MASTER FILE is UNAVAILABLE or does NOT EXIST."
            mid$ = " Report this problem to your SYSTEM ADMINISTRATOR"
            lo$ = "Please press RETURN to EXIT this program"
            call "ASKUSER" (askkey%, head$, hi$, mid$, lo$)
            goto L65000

L64050: REM ****** Error for HNYDETAL
L64055:     askkey% = 2%
            hi$ = "The PART HISTORY FILE does NOT EXIST or is EMPTY."
            mid$ = "Press RETURN to select a different report, "
            lo$ = "OR Press PF16 to EXIT this program."
            call "ASKUSER" (askkey%, head$, hi$, mid$, lo$)
            if askkey% = 16% then L65000
            if askkey% <> 0% then L64055
            if f2%(2%) = 0% then close #2 : close #1
            return clear all
            goto inputmode

L64110: REM ****** Error for HNYQUAN
L64115:     askkey% = 2%
            hi$ = "The PART QUANTITY FILE does NOT EXIST or is EMPTY."
            mid$ = "Press RETURN to select a different report, "
            lo$ = "OR Press PF16 to EXIT this program."
            call "ASKUSER" (askkey%, head$, hi$, mid$, lo$)
            if askkey% = 16% then L65000
            if askkey% <> 0% then L64115
            if f2%(3%) = 0% then close #3 : close #1
            return clear all
            goto inputmode

L64170: REM ****** Error for BOMMASTR
L64175:     askkey% = 2%
            hi$ = "The BILL of MATERIALS FILE does NOT EXIST or is EMPTY."
            mid$ = "Press RETURN to select a different report, "
            lo$ = "OR Press PF16 to EXIT this program."
            call "ASKUSER" (askkey%, head$, hi$, mid$, lo$)
            if askkey% = 16% then L65000
            if askkey% <> 0% then L64175
            if f2%(4%) = 0% then close #4 : close #1
            return clear all
            goto inputmode

L64230: REM ****** Error for HNYGENER
L64235:     askkey% = 2%
            hi$ = "The GENERIC REFERENCE FILE does NOT EXIST or is EMPTY."
            mid$ = "Press RETURN to select a different report, "
            lo$ = "OR Press PF16 to EXIT this program."
            call "ASKUSER" (askkey%, head$, hi$, mid$, lo$)
            if askkey% = 16% then L65000
            if askkey% <> 0% then L64235
            if f2%(8%) = 0% then close #8 : close #1
            return clear all
            goto inputmode

L64290: REM ****** Error for VENDOR
L64295:     askkey% = 2%
            hi$ = "The VENDOR MASTER FILE does NOT EXIST or is EMPTY."
            mid$ = "Press RETURN to select a different report, "
            lo$ = "OR Press PF16 to EXIT this program."
            call "ASKUSER" (askkey%, head$, hi$, mid$, lo$)
            if askkey% = 16% then L65000
            if askkey% <> 0% then L64295
            if f2%(9%) = 0% then close #9 : close #1
            return clear all
            goto inputmode

L64350: REM ****** Error for CATEGORY
L64355:     askkey% = 2%
            hi$ = "The CATEGORY FILE does NOT EXIST or is EMPTY."
            mid$ = "Press RETURN to select a different report, "
            lo$ = "OR Press PF16 to EXIT this program."
            call "ASKUSER" (askkey%, head$, hi$, mid$, lo$)
            if askkey% = 16% then L65000
            if askkey% <> 0% then L64355
            if f2%(10%) = 0% then close #10 : close #1
            return clear all
            goto inputmode

L64410: REM ****** Error for HNYALTRS
L64415:     askkey% = 2%
            hi$ = "The ALTERNATE PART FILE does NOT EXIST or is EMPTY."
            mid$ = "Press RETURN to select a different report, "
            lo$ = "OR Press PF16 to EXIT this program."
            call "ASKUSER" (askkey%, head$, hi$, mid$, lo$)
            if askkey% = 16% then L65000
            if askkey% <> 0% then L64415
            if f2%(11%) = 0% then close #11 : close #1
            return clear all
            goto inputmode

L64470: REM ****** Error for HNYOPTNS
L64475:     askkey% = 2%
            hi$ = "The OPTION PART FILE does NOT EXIST or is EMPTY."
            mid$ = "Press RETURN to select a different report, "
            lo$ = "OR Press PF16 to EXIT this program."
            call "ASKUSER" (askkey%, head$, hi$, mid$, lo$)
            if askkey% = 16% then L65000
            if askkey% <> 0% then L64475
            if f2%(12%) = 0% then close #12 : close #1
L64515:     return clear all
            goto inputmode

L64530: REM ****** Error for SYSFILE2
L64535:     askkey% = 2%
            hi$ = "The System File SYSFILE2 Cannot be Opened!"
            mid$ = "Consult the System Administrator to Correct Problem."
            lo$ = "Press PF16 to EXIT this program."
            call "ASKUSER" (askkey%, head$, hi$, mid$, lo$)
            if askkey% <> 16% then L64535
            if f2%(12%) = 0% then close #12 : close #1
            goto L65000

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *************************************************************

            call "SHOSTAT" ("Closing, One Moment Please")
            call "SETPRNT" (" ", " ", 0%, 1%)
            end
