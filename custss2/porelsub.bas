        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP    OOO   RRRR   EEEEE  L       SSS    U   U  BBBB   *~
            *  P   P  O   O  R   R  E      L      S       U   U  B   B  *~
            *  PPPP   O   O  RRRR   EEEE   L       SSS    U   U  BBBB   *~
            *  P      O   O  R   R  E      L          S   U   U  B   B  *~
            *  P       OOO   R   R  EEEEE  LLLLL   SSS     UUU   BBBB   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PORELSUB - Satisfy Buy Orders for parts, creating 99%     *~
            *            of a VBKLINE to feed into VBKINPUT. The buyer  *~
            *            will make critical decisions such as which     *~
            *            vendor, what to pay, ETC.                      *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1984, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/03/84 ! ORIGINAL                                 ! KEN *~
            * 08/30/84 ! ADDED BUYER/PLANNER VERIFICATION         ! DSH *~
            * 08/10/85 ! GENERAL HOUSKEEPING TYPE MODS            ! HES *~
            * 02/04/86 ! Revised Buyer Validation Logic           ! HES *~
            *          ! Major terminology changes                ! HES *~
            * 04/14/86 ! Part# not showing on screen              ! HES *~
            *          ! ADDED 'GETDEM' TO SCREENS WITH TAG NO. S ! WPH *~
            * 05/09/86 ! Changed calls to Procurement Hist & Price! LDJ *~
            *          !   Catalog.                               !     *~
            * 09/09/86 ! ADDED PF10 Mult. Release function        ! HDC *~
            * 05/19/87 ! Added inventory cost field & PORLSE fmt  ! MJB *~
            * 01/31/89 ! Changed inquiry screen to show part descr! WPH *~
            * 03/29/89 ! Added call to VENDSPSB (via PF8) which   ! RJM *~
            *          !   displays Vendor Information & Buy From !     *~
            *          !   Locations info. (PRR 10733)            !     *~
            * 06/14/89 ! Added code to enable MA and/or DBA to    ! MJB *~
            *          !   process ANY PO Directives on file -    !     *~
            *          !   new PF key 10 added to first screen.   !     *~
            *          ! Changed Enable on Conversion Factor to   !     *~
            *          !   enable if no vendor price info available.    *~
            * 08/02/89 ! Init Cost Distribution with unit prices  ! KAB *~
            * 06/29/90 ! Now allows edit for < 3 character USERIDs! KAB *~
            * 07/13/90 ! Inv Cost now depends on VBK flag.        ! JDH *~
            * 04/16/92 ! PRR 12163, 12188 - Internal Price Per    ! MLJ *~
            *          !   Unit now defaults to 0 same as Price   !     *~
            *          !   Per Unit.                              !     *~
            *          ! PRR 12162, 12190 - Vendor GETCODE header !     *~
            *          !   now includes Vendor # from HNYMASTR.   !     *~
            *          !   PF16 from GETCODE will use this vendor.!     *~
            * 04/17/92 ! PRR 12348 - Fixed PF6 functionality. PF5 !     *~
            *          !   works as stated in documentation.      !     *~
            * 07/02/92 ! Added PIPIN channel in pass to GETDEM    ! WPH *~
            * 10/16/92 ! Added Currency Input/Edit.               ! JDH *~
            * 02/18/93 ! Mods for full multi-currency support.    ! JDH *~
            *          !   Includes 'Defer Costs until PO cut'.   !     *~
            * 02/25/93 ! PRRs 12215,11062 If canceling or ignoring! JDH *~
            *          !   all BOs no need to enter directive data!     *~
            * 03/16/93 ! Added Core Value Coding.  Std costed     ! JBK *~
            *          !   Reman items costed as sum of Reman cost!     *~
            *          !   and Core Cost.                         !     *~
            * 07/23/93 ! PRR 12896. View by Vendor option added.  ! RAN *~
            *          ! View by primary or alternate buyer status.     *~
            *          ! Added options 'S'um to combine, and 'D'e-!     *~
            *          !   summarize.                             !     *~
            *          ! Added option to place directive on hold. !     *~
            *          ! Purchased Jobs Project. (BWs to RWs or   !     *~
            *          !   create RWs)                            !     *~
            * 07/26/93 ! Cleanup & ergonomics.                    ! JDH *~
            * 08/26/93 ! PRR 12895. PIPIN's (and PIPOUTs) created ! JBK *~
            *          !   for ROs and RWs and removed for honored!     *~
            *          !   BOs and BWs.  All Purchase directives  !     *~
            *          !   have PIPs.                             !     *~
            *          ! More Changes for Purchase Jobs Project.  !     *~
            * 12/20/93 ! PRR 13070  Added PF key for Where Used.  ! JDH *~
            * 05/19/94 ! Minor fix on a key for 'READ100' for     ! JBK *~
            *          !   UNIX compatability.                    !     *~
            * 06/24/94 ! Fixed unconditional call to CURRATSB.    ! JDH *~
            *          ! Added logic to honor new setting of 'B'  !     *~
            *          !   for POST_AT_STD_COST$.                 !     *~
            *          ! Fixed display after last advice cutover  !     *~
            *          !   into a directive. Detail was hanging on!     *~
            * 07/07/94 ! Fixed scrn handling of vendor sorted BOs.! JDH *~
            * 07/08/94 ! Added Contract and Job Number fields.    ! LDJ *~
            *          ! Added VPCMASTR file channel to call list !     *~
            * 11/23/94 ! Now Looks for Default Contract ID value. ! LDJ *~
            * 04/07/95 ! Fixed buyer code handling in 50000s.     ! JDH *~
            * 04/17/95 ! PRR 13377. Use this level costs for PJs. ! JDH *~
            * 07/19/95 ! Use this level costs for PJs that are    ! JDH *~
            *          !   Core parts.  Oversight from 04/17/95.  !     *~
            * 07/19/96 ! Changes for the year 2000.               ! DXL *~
            * 07/18/97 ! Tag No back to YYMMDD format             ! DER *~
            * 12/20/99 ! Mod to change vendor's price to allow    ! CMG *~
            *          !     five decimal places.  (EWD0001)      !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

            sub "PORELSUB" (#1,                  /* PIPIN    File UFB  */~
                            #2,                  /* PORLSE   File UFB  */~
                            #3,                  /* VENDOR   File UFB  */~
                            #4,                  /* HNYMASTR File UFB  */~
                            #5,                  /* VENPRICE File UFB  */~
                            #6,                  /* SYSFILE2 File UFB  */~
                            #7,                  /* PIPMASTR File UFB  */~
                            #8,                  /* SFCUM2   File UFB  */~
                            #9,                  /* HNYPROC  File UFB  */~
                            #10,                 /* HNYGENER File UFB  */~
                            #11,                 /* PIPCROSS File UFB  */~
                            #12,  /* not in use yet POADVMAS File UFB  */~
                            #13,                 /* GENCODES File UFB  */~
                            #16,                 /* VPCMASTR File UFB  */~
                            f2%)

        dim                              /* GENERAL PURPOSE VARIABLES  */~
            a_bom(12),                   /* Bill of Material Costs     */~
            a_dtl(12),                   /* This Level Costs           */~
            a_rte(12),                   /* Route Costs                */~
            a_sum(12),                   /* Folded Costs               */~
            basedate$8,                  /* Planning Base date         */~
            bfac$1,                      /* For Late Message           */~
            bfac1$1,                     /* For Late Message           */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bom_id$3,                    /* Pur Job Effective BOM ID   */~
            bomid$3,                     /* Pur Job Effective BOM ID   */~
            buyer$3,                     /* Buyer Code                 */~
            byr$(24)3,                   /* Buyer Codes for Admins     */~
            ven$(24)9,                   /* Vendor Codes that tie      */~
            codes$(100)3,                /* Part/Buyer release codes   */~
            continued$11,                /* Screen Reminder            */~
            contract_type$1,             /* Contract Item Type         */~
            core_inv_flag$1,             /* Core Inv Trans Flag        */~
            curr_on_flag$1,              /* Multi-currency on? Y or N  */~
            currency$4, currvend$4,      /* Currency code              */~
            curr_msg$13,                 /* Screen message for currency*/~
            curr_msg2$21,                /* Screen message for currency*/~
            curr_table$1,                /* Exchange rate table        */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            defer_cost$1,                /* Defer cost until PO creat. */~
            due%(100),                   /* PIPIN day array            */~
            errormsg$79,                 /* Error message              */~
            hdr$(1)79,                   /* PLOWCODE Argument          */~
            header$79,                   /* Screen header              */~
            i$(24)92,                    /* Screen image               */~
            ii$(24)80,                   /* Screen image               */~
            incl(1), incl$(1)1,          /* PLOWCODE Argument          */~
            invcosts$96,                 /* 12 Inventory Buckets, Disp */~
            inpmessage$79,               /* Input message              */~
            item$25,                     /* Contract Specific Item     */~
            lfac$(120)1,                 /* Field attribute characters */~
            misc$79,                     /* Miscellaneous Junk Variable*/~
            msg$(12)79,                  /* Initiial messages          */~
            oldpipcross$19,              /* Demand Tag for PORLSE Line */~
            oldreltag$19,                /* Pip Tag on PORLSE Header   */~
            oldreltag$(100)19,           /* Pip Tag on PORLSE Lines    */~
            old_pj_job$2,                /* Purch Job flag on PORLSE   */~
            pf$(2)79, pfk$32,            /* PF Descrs and Keys         */~
            pobuffrkey$100,              /* Old PORLSE  Key for delete */~
            post_at_std_cost$1,          /* Inv Cost at standard Flag  */~
            pipid$2,                     /* ID of source PIP file      */~
            pipkey$20,                   /* PIPIN Plow Key             */~
            pipcross$150,                /* PIPCROSS Plow key          */~
            piptag$19,                   /* Release PIP Tag            */~
            piptime$8,                   /* PIPOUT System Time         */~
            pj_bom$1,                    /* Purchase Job BOM Flag      */~
            pjfac$(2)1,                  /* Purchase Job FACS          */~
            pj_job$2,                    /* Purchase Job               */~
            pj_lead$10,                  /* Purchase Job Lead Time     */~
            pj_msg$12,                   /* Purchase Job Screen Msg    */~
            plowkey$100,                 /* For plowing                */~
            readkey$100,                 /* Misc Read Key              */~
            readke2$100,                 /* Misc Read Key              */~
            remanpart$99,                /* Reman - Core Part Keys     */~
            rteid$3,                     /* Pur Job Effective Route    */~
            selectmsg$79,                /* G.P. Message string        */~
            serial_no$1,                 /* Pur Job Serial Number Flag */~
            title$79,                    /* Screen title               */~
            testdate$8,                  /* Special Price Test Date    */~
            totalstdcost$10,             /* Total Standard Cost        */~
            usedkey$100,                 /* For plowing                */~
            userid$3,                    /* Current User ID            */~
            vsa_on_sw$,                  /* Vendor Service Advices On? */~
            vsa_pfk$37,                  /* POVRELSB PF Key Prompt     */~
            warnmsg$30                   /* Warning Message            */

        dim                              /* PO ADVICE/REQUIS VARIABLES */~
            contract$16,                 /* Contract #                 */~
            contract_line$4,             /* Contract Line              */~
            costtype$1,                  /* Part's Costing Method      */~
            duedate$8,                   /* Due Date                   */~
            effdate$6,                   /* Date Special Price Effectiv*/~
            expdate$6,                   /* Date Special Price Expires */~
            ext$10,                      /* Line Extension             */~
            factor$10,                   /* Quantity per case          */~
            job$8,                       /* Job Number                 */~
            jobdescr$32,                 /* Job Description            */~
            main_ven$9,                  /* Vendor # from HNYMASTR     */~
            meas$4,                      /* Unit of measure            */~
            orddate$8,                   /* Order date                 */~
            ourprice$10,                 /* Price Per Unit             */~
            part$25,                     /* Part                       */~
            part$(24)25,                 /* Part numbers on inquiry    */~
            partclass$3,                 /* Partclass (Buyer)          */~
            partdescr$34,                /* Part Description           */~
            po$19,                       /* PO (tag number)            */~
            descr$34,                    /* Part Description           */~
            qtyorder$10,                 /* Quantity                   */~
            stkg$4,                      /* Stocking UOM Default       */~
            tempvend$9,                  /* For call to VENDSPSB       */~
            tag$(24)19,                  /* Tag numbers for inquiry    */~
            tagdate$6,                   /* Tag No date, YYMMDD used   */~
            tagdtfull$8,                 /* Tag No date, CCYYMMDD      */~
            tagdttmp$8,                  /* temp/scratch date          */~
            type$3,                      /* Part Type                  */~
            vencode$9,                   /* Vendor                     */~
            vencodedescr$79,             /* Vendor Description         */~
            venpart$25,                  /* Vendor part nbr            */~
            venprice$10,                 /* Price Per Unit             */~
            venorder$10                  /* Quantity in 'CASES'        */~

        dim                              /* PIPIN ARRAYS & VARIABLES   */~
            pipin$(100)19,               /* PIPIN Tag                  */~
            pipdue$(100)8,               /* PIP Due date               */~
            pipord$(100)8,               /* PIP Order date             */~
            pipqty$(100)10,              /* PIP Quantity required      */~
            oldpipstat$(100)1,           /* PIP Status prior run       */~
            pipstat$(100)1,              /* PIP Status this run        */~
            seq$4,                       /* B.O. Sequence Number       */~
            seq$(100)4,                  /* B.O. Sequence Number       */~
            sfac$(100)1,                 /* Field attribute characters */~
            stat$4,                      /* Statutory currency         */~
            tagnr$19                     /* PIPIN Tag number           */

        dim alt$(100)1,                  /* Alternate buyer flags      */~
            holdrel1$1, holdrel$(24)1,   /* Directive Hold/Release     */~
            holdreldesc$12,              /* Hold /Release displ. msg   */~
            keyarray$(3,100)44,          /* Array of Keys to workfile  */~
                                         /* key slot = display page    */~
            oldholdrel$1,                /* 'old' hold/release value   */~
            pippart$25,                  /* PIPOUT part # for SEARCH   */~
            npj_no$8                     /* Purchase Job - Next Job #  */

        dim f1%(64)                      /* = 1 If read was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! PIPIN    ! Planned inventory additions detail       *~
            * # 2 ! PORLSE   ! release control buffer                   *~
            * # 3 ! VENDOR   ! Vendor Master File                       *~
            * # 4 ! HNYMASTR ! Inventory Master File                    *~
            * # 5 ! VENPRICE ! Vendor current prices - all vendors, all *~
            * # 6 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 7 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * # 8 ! SFCUM2   ! Cumulative Sales Forecast File           *~
            * # 9 ! HNYPROC  ! Inventory Procurement History File       *~
            * #10 ! HNYGENER ! Inventory Generic Parts Cross-Reference  *~
            * #11 ! PIPCROSS ! Planned Inventory Position Tag Xref File *~
            * #12 ! POADVMAS ! Purchase Advices (Supplemental) Master   *~
            * #13 ! GENCODES ! General Codes File                       *~
            * #14 ! VENDORBF ! Vendor Buy From file                     *~
            * #15 ! JBMASTR2 ! Shop Floor Jobs Master File              *~
            * #16 ! VPCMASTR ! Vendor Purchase Contracts Master File    *~
            * #20 ! PORELUSR ! User Buy order Release Cross Reference   *~
            * #21 ! DEMMASTR ! MASTER FILE FOR PLANNING DEMANDS         *~
            * #22 ! BOMMASTR ! BILL OF MATERIALS RELATIONSHIP FILE      *~
            * #23 ! ENGMASTR ! ENGINEERING MASTER FILE                  *~
            * #24 ! POPIPXRF ! Buy Order/Purch Directive Cross Ref File *~
            * #25 ! PIPOUT   ! Planned Position Out                     *~
            * #26 ! JBCROSS2 ! Job rte/bom used cross ref.              *~
            * #27 ! JBPIPXRF ! Option part pegging                      *~
            * #28 ! WCOUT    ! Work center cross reference file         *~
            * #40 ! CURMASTR ! Multi-Currency Master file               *~
            * #41 ! COREXREF ! Core Part Cross Reference File           *~
            * #50 ! WORKFILE ! Workfile for Screen Displays             *~
            * #51 ! WORKFILE ! Workfile for Purchased Job PIP's         *~
            *************************************************************

            select #14, "VENDORBF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 15

            select #15, "JBMASTR2",                                      ~
                        varc, indexed, recsize = 1300,                   ~
                        keypos = 1, keylen =  8

            select #20, "PORELUSR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 70,                                    ~
                        keypos =    1, keylen =   6,                     ~
                        alt key 1, keypos =  4, keylen = 6

             select #21, "DEMMASTR",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 123,                                   ~
                        keypos =    2, keylen =  27,                     ~
                        alt key 1, keypos = 10, keylen = 19,             ~
                            key 2, keypos = 1, keylen = 28

            select #22, "BOMMASTR",                                      ~
                         varc,     indexed,  recsize =  150,             ~
                         keypos =  26, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 56

            select #23, "ENGMASTR" ,                                     ~
                        varc,     indexed,  recsize = 2015,              ~
                        keypos = 1, keylen = 29

            select #24, "POPIPXRF",                                      ~
                        varc, indexed, recsize = 100,                    ~
                        keypos = 1, keylen = 58

            select #25, "PIPOUT",                                        ~
                        varc, indexed, recsize = 64,                     ~
                        keypos = 1, keylen = 56,                         ~
                        alt key 1, keypos = 20, keylen = 37

            select #26, "JBCROSS2",                                      ~
                        varc, indexed, recsize = 94,                     ~
                        keypos = 29, keylen = 19,                        ~
                        alt key 1, keypos = 1, keylen = 47,              ~
                            key 2, keypos = 48, keylen = 47

            select #27, "JBPIPXRF",                                      ~
                        varc, indexed, recsize = 63,                     ~
                        keypos = 1, keylen = 63,                         ~
                        alt key 1, keypos = 45, keylen = 19

            select #28, "WCOUT",                                         ~
                        varc, indexed, recsize = 68,                     ~
                        keypos = 9, keylen = 23,                         ~
                        alt key 1, keypos = 1, keylen = 27

            select #40, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            select #41, "COREXREF",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =   26, keylen =  50,                     ~
                        alt key  1, keypos =    1, keylen =  50,         ~
                            key  2, keypos =   76, keylen =  25, dup     ~

            select #50, "WORKFILE",                                      ~
                        varc, indexed, recsize = 194,                    ~
                        keypos = 1, keylen = 19,                         ~
                        alt key 1, keypos = 20, keylen = 44,             ~
                            key 2, keypos = 64, keylen = 23,             ~
                            key 3, keypos = 87, keylen = 28

            select #51, "WORKFIL1",                                      ~
                         varc,     indexed,  recsize =   64,             ~
                         keypos =   1, keylen = 56,                      ~
                         alt key  1, keypos = 20, keylen = 37

            REM Create/open buffer if need be...
            if f2%=0 then L05050
               call "OPENCHCK" (# 2, 0%, f2%, 100%, " ")

            if open14%=0% then call "OPENCHCK" (#14, open14%, 0%, 0%, " ")
            if open15%=0% then call "OPENCHCK" (#15, open15%, 0%, 0%, " ")
L05050:     if open20%=0% then call "OPENCHCK" (#20, open20%, 0%, 0%, " ")
            if open21%=0% then call "OPENCHCK" (#21, open21%, 0%, 0%, " ")
            if open24%=0% then call "OPENCHCK" (#24, open24%, 0%,100%," ")
            if open25%=0% then call "OPENCHCK" (#25, open25%, 0%, 0%, " ")
            if open26%=0% then call "OPENCHCK" (#26, open26%, 0%, 0%, " ")
            if open27%=0% then call "OPENCHCK" (#27, open27%, 0%, 0%, " ")
            if open28%=0% then call "OPENCHCK" (#28, open28%, 0%, 0%, " ")

            call "WORKOPEN" (#50, "IO   ", 1000%, f50%) : f50% = f50%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes Information Necessary For Program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            tagdttmp$ = date
            call "DATEFMT" ( tagdttmp$, 0%, tagdtfull$ )
            tagdate$ = str( tagdtfull$, 3%, 6% )


            date$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)
            bfac1$ = hex(94)

            REM See if operator is an administrator or not
            call "CMSMACHK" ("VBK", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then admok% = 1%
            if lfac$(1) = "Y" or lfac$(2) = "Y" then L09380

            REM Load Up His Release Codes For Cross Checking...
            readkey$ = all(hex(00))
            call "PLOWNEXT" (#20, readkey$, 0%, f1%(20))
                if f1%(20) = 0 then L09380  /* not using feature */

            c%, admin% = 0 : codes$() = " "
            readkey$ = str(userid$) & hex(00000000)
L09220:     call "PLOWNEXT" (#20, readkey$, 3%, f1%(20))
                if f1%(20) = 0 then L09300
            c% = c% + 1
            if c% = 100 then L09300
            get #20, using L09270, codes$(c%), alt$(c%)
L09270:     FMT XX(3), CH(3), POS(40), CH(1)
            goto L09220

L09300:     REM What did we get?
            if codes$() <> " " then L09360
                call "ASKUSER" (0%, "Sorry", "You are not listed as a" & ~
                " valid Buyer", "therefore you may not create" &         ~
                " Purchasing Directives", "Press (RETURN) to exit.")
                goto L65000
L09360:     if codes$() <> "ALL" then L09410

L09380:     REM This guy can do as he pleases...
            admin% = 1

L09410:     readke2$ = "MONTHS OPEN"
            call "READ100" (#6, readke2$, f1%(6))
                if f1%(6)<>0 then L09470
                call "ASKUSER" (0%, "Sorry",                             ~
                            "Can't Find Months Open record in SYSFILE2.",~
                            "Press (RETURN) To Exit.", " ")
                goto L65000
L09470:     get #6, using L09480, basedate$
L09480:         FMT XX(32), CH(6)

            REM Get today in terms of planning calendar...
                call "DATE" addr("G-", basedate$, date, today%, err%)
                today% = today% + 1

            REM Get flag to see if inventory cost is to be at standard
            call "VBKSWTCH" ("POSTDCST", post_at_std_cost$, temp, err%)
            if post_at_std_cost$ = "B" then post_at_std_cost$ = "Y"
            call "VBKSWTCH" ("DEFERCST", defer_cost$      , temp, err%)
            call "VBKSWTCH" ("VSA ACTV", vsa_on_sw$       , temp, err%)
            if vsa_on_sw$ = "Y" then vsa_pfk$ =                          ~
                "(11)Enter/View Vendor Service Advices"                  ~
                                else vsa_pfk$ = " "
            warnmsg$ = "> > > W A R N I N G < < <"

            init (" ") pipkey$, part$, partdescr$, readkey$, main_ven$
            title$="Review Buy Orders and/or Create Purchase Directives"
            str(title$,62%) = "PORELSUB: " & str(cms2v$,,8%)
            msg$( 1)="RETURN - Create new Purchase Directive (manually or~
        ~ from open Buy Orders).   "
            msg$( 2)="PF (2) - Edit Purchase Directive you have already c~
        ~reated for this part.     "
            msg$( 3)="PF (3) - See ALL Open Buy Orders by Order Date.    ~
        ~                          "
            msg$( 4)="PF (4) - See ALL Open Buy Orders by Part Number.   ~
        ~                          "
            msg$( 5)="PF (5) - See ALL Open Buy Orders by Primary Vendor.~
        ~                          "
            msg$( 6)="PF (6) - Manage next Part Number with Open Buy Orde~
        ~rs.                       "
            msg$( 7)="PF (8) - See / Edit Purchase Directives by Part Num~
        ~ber.                      "
            msg$( 8)="PF (9) - See / Edit Purchase Directives by Vendor. ~
        ~                          "
            msg$( 9)="PF(10) - See ALL Purchase Directives on File. (Admi~
        ~nistrators ONLY).         "
            if admok% <> 1% then msg$(9%) = " "
            msg$(10)="PF(12) - See all LATE Buy Orders by Order Date.    ~
        ~                          "
            msg$(11)="PF(14) - Review Procurement History file.          ~
        ~                          "
            msg$(12)="View Advices, Directives under MAIN Buyer Codes ONL~
        ~Y.                        "

            altb%=0%      /* Associated buyer flag.  If set to 0%, only */
                          /* MAIN buyer codes' stuff appears.  If set   */
                          /* to 1%, then alternate buyers stuff appears */
                          /* IN ADDITION to main buyers'.               */
            factor = 1
            header$ = "Ref#  Status  Quantity    Order      Due     Buy O~
        ~rder Tag #"

            if seq$() <> " " then L09820
            for i = 1 to 100
                call "CONVERT" (i, -.001, str(seq$(i),,3%))
                seq$(i) = seq$(i) & ")"
            next i

L09820:     plowkey$ = all(hex(00))              /* If UOM file set up */
            str(plowkey$,10) = "UOM"             /* then edit entries. */
            call "READ100" (#13, plowkey$, uom%)

*        Check for Multi-Currency
            curr_on_flag$ = "N" : stat$, curr_msg$, curr_msg2$ = " "
            readke2$ = "SWITCHS.CUR"
            call "READ100" (#06, readke2$, f1%(6%))
            if f1%(6%) <> 0% then get #06 using L09865, curr_on_flag$,     ~
                                                      stat$, curr_table$
L09865:         FMT POS(21), CH(1), CH(4), POS(28), CH(1)
            if curr_on_flag$ <> "Y" then L09896
                if open40% = 0% then call "OPENCHCK" (#40, open40%, 0%,  ~
                                                                 0%, " ")
                curr_msg$ = "Currency Code"
                curr_msg2$ = "(Always in Statutory)"
                goto L09900

L09896:     stat$ = " "

L09900
*        Check for Core
            plowkey$ = "SWITCHS.COR"
            call "READ100" (#6, plowkey$, core_on%)
                if core_on% <> 1% then L09960
            get #6 using L09925, core_inv_flag$
L09925:             FMT POS(134), CH(1)
                if core_inv_flag$ = "Y" then L09945
                    core_on% = 0%
                    goto L09960
L09945:         if open41% = 0% then call "OPENCHCK" (#41, open41%, 0%,  ~
                                                                 0%, " ")

L09960
*        Check for Purchased Jobs
            plowkey$ = "SWITCHS.SFC"
            call "READ100" (#6, plowkey$, pj_on%)
                if pj_on% <> 1% then L10000
            get #6 using L09982, npj_no$
L09982:         FMT POS(115), CH(8)
            if npj_no$ = " " then pj_on% = 0%
            if pj_on% = 0% then L10000
            if open22%=0% then call "OPENCHCK" (#22, open22%, 0%, 0%, " ")
            if open23%=0% then call "OPENCHCK" (#23, open23%, 0%, 0%, " ")
            call "WORKOPEN" (#51, "IO   ", 1000%, f51%)  :  f51% = f51%
            pj_msg$ = "Purchase Job"

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode

            routine% = 0%                /* Return marker from DATASAVE */
            loaded%  = 0%
            pj_stat%, pj_lead%, bom_check%, kitmode% = 0%
            bom_id$, pj_bom$, pj_job$, pj_lead$, serial_no$, piptag$ = " "

            gosub L12000   /* Check for late Buy Orders */
            gosub initialize
            inpmessage$="Enter Part And/Or Use PF Keys As Indicated."
            call "DESCRIBE" (#4, part$, partdescr$, 1%, f1%(4%))

            for fieldnr% = 1% to 1%
L10130:         gosub'101(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% then L65000
                      if keyhit%  =  2% then see_pod_this_part
                      if keyhit%  =  6% then next_part
                      if keyhit%  =  8% then see_all_pod
                      if keyhit%  = 10% and admok% = 1% then see_all_pod
                      if keyhit%  =  9% then see_all_pod_vendor
                      if keyhit% <>  3% and keyhit% <> 12%  then L10200
                          akey% = 2%  :  goto see_open_pipins
L10200:               if keyhit% <>  4% then L10205
                          akey% = 1%  :  goto see_open_pipins
L10205:               if keyhit% <>  5% then L10211
                          akey% = 3%  :  goto see_open_pipins
L10211:               if keyhit% <>  7% then L10228
                        if altb%=1% then L10224
                        altb%=1%
            msg$(12)="View Advices, Directives under MAIN & ALTERNATE Buy~
        ~er Codes.                 " : goto L10130
L10224:                 altb%=0%
            msg$(12)="View Advices, Directives under MAIN Buyer Codes ONL~
        ~Y.                        " : goto L10130
L10228:               if keyhit%<>  0% then       L10130
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10130
                next fieldnr%

        bo_fast:if maxlines% = 0% then L10310
                   fieldnr% = 14%
                   gosub edit_field
                     /* If we're not satisfying any BO then we  */
                     /* can go straight to editmode for edit or */
                     /* save.  No need to enter directive data. */
                     for i% = 1% to maxlines%
                          if pipstat$(i%) = "R" then inputmode2
                     next i%
                     goto editmode

        inputmode2
L10310:     for fieldnr% = 2% to 13%
                if fieldnr% = 11% then L10365
                gosub'051(fieldnr%)
                      if enabled% = 0% then L10350
L10330:         gosub'111(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit% <>  0% then       L10330
L10350:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10330
L10365:     next fieldnr%

            if totalstdcost > 0 then L10402
            totalstdcost$ = ourprice$
            if currency$ <> stat$ and curr_on_flag$ = "Y" then           ~
                                                      gosub get_curr_cost
            if defer_cost$ <> "Y" then L10390
L10386:         gosub figure_cost
                goto L10398
L10390:     if pj_on% = 1% and pj_job$ = "PJ" then L10386
            if pos("FST" = costtype$) <> 0 and post_at_std_cost$ = "Y"   ~
                then gosub get_std_cost  /* Get Std Cost for Inv Cost */
            gosub init_hnycdist
L10398:     if defer_cost$ <> "Y" then L10402
                totalstdcost = -1 : totalstdcost$ = "*Deferred*"

L10402:     if pj_on% <> 1% or pj_job$ <> "PJ" then L10410
                temp1$ = orddate$  :  temp2$ = duedate$  :  kitmode% = 1%
                call "DATUNFMT" (temp2$)  :  call "DATUNFMT" (temp1$)
                call "PJINPSUB" (part$, partdescr$, vencode$,            ~
                      vencodedescr$, venpart$, oldreltag$, temp1$,       ~
                      temp2$, bom_id$, qtyorder, pj_stat%, kitmode%, #25,~
                      #4, #22, #23, #7, #8, #6, #51)
                if kitmode% = 99% then gosub L29930
L10410:     goto editmode

        get_std_cost  /* Get Std Cost for Inv Cost */
            if pj_job$ = "PJ" then L10422
                if core_on% <> 1% then L10422
                     gosub get_reman_core_std_cost
                     if reman% = 0% then L10422
                     goto L10426
L10422:     call "STCCOSTS" (part$, " ", #6, 3%, temp, a_sum(), a_bom(), ~
                                                       a_rte(), a_dtl())
            if pj_job$ <> "PJ" then L10426
                temp = 0 : for i% = 1 to 12: temp=temp+a_dtl(i%) : next i%
L10426:     call "CONVERT" (temp, 2.4, totalstdcost$)
            return

        next_part
            init (hex(ff)) plowkey$
            str(plowkey$,,25) = part$
L10460:     call "PLOWALTS" (#1,plowkey$,1%,0%,f1%(1))
                if f1%(1)=0 then L10560
            if str(plowkey$,30%,2%)<>"BO" and                            ~
               str(plowkey$,30%,2%)<>"BW" then L10460
            part$ = str(plowkey$,,25)
            gosub initialize
            gosub'151(1%)
              if errormsg$ <> " " then L10460
              if maxlines% = 0% then L10460
              goto bo_fast

L10560:     part$, partdescr$ = " "
            goto inputmode

        init_hnycdist
            call "HNYCDIST" ("I", part$, partdescr$, " ", #6,            ~
                             invcosts$, totalstdcost$, totalstdcost)
            return

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        editmode
            inpmessage$ = "Postion Cursor and press (ENTER) to modify; PF~
        ~-9 to Toggle Hold/Release."
            gosub'111(0%)
                  if keyhit%  = 10% then datasave
                  if keyhit%  = 16% then datasave
                  if keyhit%  = 11% then gosub manage_kit_list
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  9% then goto L11080
                    if holdrel1$="R" then holdrel1$="H" else holdrel1$="R"
L11080:           gosub L11320
                  if keyhit% <> 0% then editmode
            oldfield% = 0%
L11095:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% > 5% and fieldnr% < 11% then                     ~
                                                fieldnr% = fieldnr% + 1%
            if fieldnr% = 3% then editmode
            if fieldnr% = 2% and cursor%(2%) > 30% then fieldnr% = 3%
            if fieldnr% = 5% and cursor%(2%) > 47% then fieldnr% = 6%
            if fieldnr% = 7% and cursor%(2%) > 39% then fieldnr% = 13%
*          IF FIELDNR% = 11% AND CURSOR%(2%) <= 55% THEN FIELDNR% = 11%

            if fieldnr% = 11% and cursor%(2%)  > 56% then fieldnr% = 12%
            if oldfield% = fieldnr% then editmode
            if fieldnr% <  2% then editmode
            if cursor%(1%) - 3% > 11% then fieldnr% = 14%
            if fieldnr% = 14% and maxlines%=0% then editmode
L11155:     if fieldnr% = 14% then inpmessage$ = "'R'-Satisfy, ' '-Lea"& ~
                "ve as BO/BW, 'C'-Cancel" else inpmessage$ = " "
            if fieldnr% <> 11% then L11195
                gosub defer_cost
                if defer% = 1% then L11190
                    call "HNYCDIST" (" ", part$, partdescr$, " ", #6,    ~
                                 invcosts$, totalstdcost$, totalstdcost)
L11190:         goto editmode
L11195:     gosub edit_field
               oldfield% = fieldnr%
               if fieldnr% <> 14% then L11095
                  /* If there's no BO that we're satisfying, then    */
                  /* just go to editmode for edit or save.  If we're */
                  /* satisfying and have not gone through inputmode  */
                  /* we had better go get that done.                 */
                  for i% = 1% to maxlines%
                     if pipstat$(i%) = "R" then L11250
                  next i%
                     goto editmode
L11250:           if vencode$ = " " then goto inputmode2
                     fieldnr%  = 9%
                     goto L11155

        edit_field
          if fieldnr% = 6% and curr_on_flag$ <> "Y" then return
L11280:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                     gosub L11320
                  if keyhit% <>  0 then L11280
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11280
            return

L11320: REM BOUNCE LINEAR PORTION OF SCREEN

                     if keyhit% =  2% then l%=0%
                     if keyhit% =  3% then l%=max(0%,maxlines%-6%)
                     if keyhit% =  4% then l%=max(0%,l%-6%)
                     if keyhit% =  5% then l%=max(0%,min(l%+6%,          ~
                                                         maxlines%-6%))
                     if keyhit% =  6% then l%=max(0%,l%-1%)
                     if keyhit% =  7% then l%=max(0%,min(l%+1%,          ~
                                                         maxlines%-6%))

            return

        defer_cost /* Enter here from edit mode */
L11390:     defer% = 0%
            call "ASKUSER" (defer%, "*** Defer Costs? ***",              ~
                            "Do you wish to defer costs until Purchase"& ~
                            " Order time?", "Press PF1 to defer -or- ",  ~
                            "Press RETURN to enter costs.")
            if defer%  = 0% then figure_cost
            if defer% <> 1% then L11390
                totalstdcost$ = "*Deferred*"
                totalstdcost  = -1 : init (hex(00)) invcosts$
                return
        figure_cost /* Enter here from input mode */
            if totalstdcost > 0 then return
                if pos("FST" = costtype$) = 0 or post_at_std_cost$ <> "Y"~
                                                               then L11480
*              IF PJ_ON% = 1% AND PJ_JOB$ = "PJ" THEN 11480
                     gosub get_std_cost
                     gosub init_hnycdist
                     return
L11480:         if currency$ <> stat$ and curr_on_flag$ = "Y" then L11500
                     totalstdcost$ = ourprice$
                     gosub init_hnycdist
                     return
L11500: get_curr_cost
                call "CURRATSB" (currency$, curr_table$, date$,          ~
                                             cur_factor, temp, errormsg$)
                if errormsg$ = " " then L11535
                     errormsg$ = " "
L11525:              totalstdcost$ = "0.00"
                     return
L11535:         convert ourprice$ to temp, data goto L11525
                temp = temp * cur_factor
                call "CONVERT" (temp, 4.4, totalstdcost$)
                gosub init_hnycdist
                return

L12000: REM *************************************************************~
            * A LITTLE ROUTINE TO SEE IF ANY LATE B.O.s OUT THERE.      *~
            * ADD CHECK TO SEE IF ANY LATE BW's                         *~
            *************************************************************

            bfac$ = hex(9c)
            pipkey$="BO"
L12065:     msg$(10)="PF(12) - See all LATE Buy Orders by Order Date.    ~
        ~                        "
            init (hex(00)) str(pipkey$,3,16)

L12090:     call "PLOWNEXT" (#1, pipkey$, 2%, f1%(1))
                 if f1%(1)=0 then L12230
            get #1, using L12120, part$, i%   /* Order date */
L12120:     FMT CH(25), XX(31), BI(4)
            if i% >= today% then L12230
            gosub initialize
            gosub'151(1%)
                if errormsg$ <> " " then L12090
                if maxlines% = 0% then L12090
            bfac$ = hex(84)
            str(msg$(10%),9%,1%) = bfac1$
            if bfac$ = hex(9c) then str(msg$(10%),9%,1%) = bfac$
            bfac1$ = hex(84) /* Flash it only once */

L12230:     init (" ") part$, partdescr$, readkey$
            if str(pipkey$,,2%) = "BO" then L12237
                if bfac$ = hex(9c) then msg$(10%) = "   "
                return
L12237:     if  bfac$ = hex(84) then return  /*Late BO; enough checking!*/
                pipkey$="BW"
                goto L12065    /* Check for Late BW's        */

        REM *************************************************************~
            *  For Reman Part Determine Standard cost as a sum of the   *~
            *  Reman's Standard Cost and the Implied Core's Std Cost    *~
            *************************************************************
        get_reman_core_std_cost
            init (" ")  remanpart$
            remanpart$ = part$
            call "PLOWALTS" (#41, remanpart$, 0%, 25%, reman%)
                if reman% = 0% then return
                if str(remanpart$,26%,25%) <> " " then L12380
                     reman% = 0%
                     return
L12380:     call "STCCOSTS" (str(remanpart$, 1%,25%), " ", #6, 1%, reman)
            call "STCCOSTS" (str(remanpart$,26%,25%), " ", #6, 1%, core)

            temp = reman + core
            return

        REM *************************************************************~
            *  Manage Kit List for Purchase Job.                        *~
            *************************************************************
        manage_kit_list
            if pj_on% <> 1% or pj_job$ <> "PJ" then return
                temp1$ = orddate$  :  call "DATUNFMT" (temp1$)
                temp2$ = duedate$  :  call "DATUNFMT" (temp2$)
                kitmode% = 2%
                call "PJINPSUB" (part$, partdescr$, vencode$,            ~
                                 vencodedescr$, venpart$, oldreltag$,    ~
                                 temp1$, temp2$, bom_id$, qtyorder,      ~
                                 pj_stat%, kitmode%, #25, #4, #22, #23,  ~
                                 #7, #8, #6, #51)

                if kitmode% = 99% then gosub L29930
                return
        REM *************************************************************~
            *                                                           *~
            *  SCAN PURCHASE DIRECTIVES FOR DIRECT EDIT                 *~
            *                                                           *~
            *************************************************************
        see_pod_this_part
        see_all_pod

            routine% = 1%                /* Return marker from DATASAVE */

            savehit% = keyhit%
            plowkey$ = str(part$,,25%)

L15120:     if savehit% = 2% then L15160
               plowkey$ = all(hex(00))
L15140:           call "PLOWALTS" (#2, plowkey$, 3%, 0%, f1%(2%))
                        if f1%(2%) = 0% then inputmode
L15160:     str(plowkey$,26%) = all(hex(00))
            f1%(2%) = 0% :pass% = 0%

            init (" ") i$()
            i$(1%)="Open Purchase Directives For Part: "&str(plowkey$,,25)
            i$(2%)="Vendor     Order     Due      Quantity  Extension Ven~
        ~dor Part Number"
            if curr_on_flag$ = "Y" then str(i$(2),76%,4%) = "Curr"
            i$(22%)="Position Cursor And Press RETURN To Edit"
            i$(23%)="(1)Start Over         (5)Next      (13)Instructions ~
        ~ (15)Print Screen"
            i$(24%)="(2)First              (6)Next Part                  ~
        ~ (16)Return to Input"
            if savehit% = 2 then str(i$(24%),23%,12%) = " "

L15310:     lll% = 3%
            init (hex(8c)) lfac$()
            lfac$(20%)=hex(ac)
            init(" ") str(i$(),185%,1748%)
               if str(plowkey$,26%,1%) = hex(ff) then L15430
               if f1%(2%) = 0% then L15390

L15360:     call "READNEXT" (#2, f1%(2%))
               goto L15400

L15390:     call "PLOWALTS" (#2, plowkey$, 3%, 25%, f1%(2%))
L15400:         if f1%(2%) <> 0% then L15460
L15410:            f1%(2%) = 0% : init (hex(ff)) str(plowkey$,26%)
                   if lll% > 3% then L15700
L15430:               if savehit% = 8% or savehit% = 10% then L15140
                      goto inputmode

L15460:     get #2, using L15470, buyer$, venpart$, po$
L15470:         FMT XX(1), CH(3), XX(9), CH(25), XX(9), CH(19)
                  if str(venpart$,,25%) <> str(plowkey$,,25%) then L15410
                  if po$ <> " " then L15500
                  if admok% = 1% then L15550
                  if buyer$=userid$ then L15550
                  call "READ100" (#4, venpart$, f1%(4%))
                     if f1%(4%)=0% then L15500
                  get #4 using L15491, vbuyer$
L15491:              FMT POS(200), CH(3)
                  search codes$()=vbuyer$ to cursor%() step 3
                     if cursor%(1)=0% then L15500   /* No Chance        */
                     if alt$((cursor%(1%)+2)/3)=" " then L15550 /* Main */
                  if altb%=0% then L15500 /* Not viewing Alt Buyers     */
                  if alt$((cursor%(1%)+2)/3)="Y" then L15550
                     /* Allow info if alternate buyer code             */

L15500:                 get #2, using L15510, plowkey$
L15510:                     FMT XX(13), CH(34)
                        init (hex(ff)) str(plowkey$,35%)
                        goto L15390

L15550:     get #2, using L15580, holdrel$(lll%),                         ~
                                 byr$(lll%), str(i$(lll%),,9%),          ~
                                 str(i$(lll%),11%,6%),                   ~
                                 str(i$(lll%),80%,3%),                   ~
                                 qty, extn, str(i$(lll%),51%,25%),       ~
                                 str(i$(lll%),20%,6%),                   ~
                                 str(i$(lll%),76%,4%)
            if holdrel$(lll%)="H" then lfac$(lll%-2%)=hex(84)
            if lll%<>22%  then L15594
                if holdrel$(lll%)="H" then lfac$(lll%-2%)=hex(a4)        ~
                   else lfac$(lll%-2%)=hex(ac)
L15580:         FMT CH(1), CH(3), CH(9), XX(25), CH(6), CH(3), XX(19),   ~
                    PD(14,4), PD(14,4), CH(25), CH(6), POS(238), CH(4)

L15594:     if str(i$(lll%),76%,4%) = " " then str(i$(lll%),76%,4%)= stat$
            if curr_on_flag$ <> "Y" then str(i$(lll%),76%,4%) = " "

            call "CONVERT" (qty, 0.2, str(i$(lll%),29%,10%))
            call "CONVERT" (extn,2.2, str(i$(lll%),40%,10%))
            call "DATEFMT" (str(i$(lll%),11%,8%))
            call "DATEFMT" (str(i$(lll%),20%,8%))
*          STR(I$(LLL%),76%,1%) = HEX(9C)
            lll% = lll% + 1%

            if lll% > 20% then L15760
               goto L15360

L15700:     if savehit% <> 2% then L15780
            if pass% <> 0% then L15780
            if lll% > 4% then L15760
               cursor%(1%) = 3%
               goto L15910

L15760:     pass% = 1%

L15780:     gosub inquiry_screen

            if keyhit% =  2% then L15120
            if keyhit% =  5% then L15310
            if keyhit% = 16% then inputmode

            if keyhit% <> 6% then L15890
                if savehit% <> 10% then savehit% = 8%
                init (hex(ff)) str(plowkey$,26%)
                goto L15140

L15890:     if keyhit% <> 0% then L15780

L15910:     fieldnr% = cursor%(1%)
            if fieldnr% < 3% or fieldnr% > lll% - 1% then L15780
            if str(i$(fieldnr%),80%,3%) = " " then L15780
            gosub initialize
            temp$ = str(i$(fieldnr%),11%,8%)
            call "DATUNFMT" (str(temp$,,8%))
*          IF SAVEHIT% = 10% THEN                                       ~
*              POBUFFRKEY$=HOLDREL$(FIELDNR%) & BYR$(FIELDNR%) ELSE     ~
*              POBUFFRKEY$=HOLDREL$(FIELDNR%) & USERID$
                pobuffrkey$=holdrel$(fieldnr%) & byr$(fieldnr%)
            str(pobuffrkey$,  5%,  9%) = str(i$(fieldnr%),,9%)
            str(pobuffrkey$, 14%, 25%) = str(plowkey$,,25%)
            str(pobuffrkey$, 39%,  6%) = str(temp$,,6%)
            str(pobuffrkey$, 45%,  3%) = str(i$(fieldnr%), 80%, 3%)
            init (" ") str(pobuffrkey$,48%)

            call "READ100" (#2, str(pobuffrkey$,,66%), f1%(2%))
                if f1%(2%) = 0% then L15780

            gosub L32000
            goto editmode

        REM *************************************************************~
            *                                                           *~
            *  SCAN PURCHASE DIRECTIVES BY VENDOR FOR DIRECT EDIT       *~
            *                                                           *~
            *************************************************************


        see_all_pod_vendor

            routine% = 2%                /* Return marker from DATASAVE */

            savehit% = keyhit%
L16160:     init (hex(00)) plowkey$

            call "PLOWALTS" (#2, plowkey$, 2%, 0%, f1%(2))
               if f1%(2)=0 then inputmode
            str(plowkey$,10%) = all(hex(00))
            f1%(2) = 0% :pass% = 0%

            init (" ") i$()
            i$(1)="Open Purchase Directives by Vendor"
            i$(2)="Part                         Order       Due       Qua~
        ~ntity  Vendor P/N"
            i$(22)="Position Cursor And Press RETURN To Edit"
            i$(23)="(1)Start Over         (5)Next      (13)Instructions  ~
        ~(15)Print Screen"
            i$(24)="(2)First                                             ~
        ~(16)Return to Input"

L16245:     lll% = 3% : oldven$=" "
            init (hex(8c)) lfac$()
            lfac$(20%)=hex(ac)
            init(" ") str(i$(),161%,1748%): goto L16285

L16260:     call "READNEXT" (#2, f1%(2))
               if f1%(2%)<>0% then L16285
               if f1%(2%)=0% and lll%>3% then L16445
               goto inputmode

L16285:     get #2, using L16290, buyer$, venpart$, po$, podven$, part$
L16290:         FMT XX(1), CH(3), XX(9), CH(25), XX(9), CH(19),          ~
                  POS(5), CH(9), CH(25)
                  if po$ <> " " then L16260
                  if admok%=1% then L16335
                  if buyer$=userid$ then L16335
                  call "READ100" (#4, venpart$, f1%(4%))
                     if f1%(4%)=0% then L16260
                  get #4 using L16308, vbuyer$
L16308:              FMT POS(200), CH(3)
                  search codes$()=vbuyer$ to cursor%() step 3
                     if cursor%(1%)=0% then L16260  /* No Chance        */
                     if alt$((cursor%(1%)+2%)/3)=" " then L16335 /* Main*/
                  if altb%=0% then L16260 /* Not allowing Alt Buyers    */
                  if alt$((cursor%(1%)+2%)/3)<>"Y" then L16260
                     /* Get info if alternate buyer code               */

L16335:     if podven$ = oldven$ then L16360
               lll%=lll%+1% : oldven$ = podven$
               i$(lll%)="Vendor Code : " & podven$
               call "DESCRIBE" (#3,podven$,str(i$(lll%),26%),0%,f1%(3%))
               lll%=lll%+1%
L16360:     get #2, using L16380, holdrel$(lll%),                         ~
                                 byr$(lll%),ven$(lll%),str(i$(lll%),,25),~
                                 str(i$(lll%),28,6), str(i$(lll%),77,3), ~
                                 qty, extn, str(i$(lll%),63,12),         ~
                                 str(i$(lll%),39,6)
            if holdrel$(lll%)="H" then lfac$(lll%-2%)=hex(84)
            if lll%<>22%  then L16395
                if holdrel$(lll%)="H" then lfac$(lll%-2%)=hex(a4)        ~
                   else lfac$(lll%-2%)=hex(ac)
L16380:         FMT CH(1), CH(3), CH(9), CH(25), CH(6), CH(3), XX(19),   ~
                    PD(14,4), PD(14,4), CH(25), CH(6)

L16395:     call "CONVERT" (qty, 0.2, str(i$(lll%),50,10))
*          CALL "CONVERT" (EXTN,2.2, STR(I$(LLL%),63,10))
            call "DATEFMT" (str(i$(lll%),28,8))
            call "DATEFMT" (str(i$(lll%),39,8))
            str(i$(lll%),76,1)=hex(9c)
            lll%=lll%+1%

            if lll% > 20% then L16445
               goto L16260

L16445:     gosub inquiry_screen

            if keyhit% =  2 then L16160
            if keyhit% =  5 then L16245
            if keyhit% = 16 then inputmode

            if keyhit% <> 0 then L16445

            fieldnr%=cursor%(1)
            if fieldnr%<3 or fieldnr%>lll%-1% then L16445
            if str(i$(fieldnr%),77,3)=" " then L16445
            gosub initialize
            temp$ = str(i$(fieldnr%),28,8)
            call "DATUNFMT" (str(temp$,,8))
            pobuffrkey$ = holdrel$(fieldnr%) & byr$(fieldnr%)
            str(pobuffrkey$,  5,  9) = str(ven$(fieldnr%),,9%)
            str(pobuffrkey$, 14, 25) = str(i$(fieldnr%),,25%)
            str(pobuffrkey$, 39,  6) = str(temp$,,6%)
            str(pobuffrkey$, 45,  3) = str(i$(fieldnr%),77%,3%)
            init (" ") str(pobuffrkey$,48)

            call "READ100" (#2, str(pobuffrkey$,,66), f1%(2%))
                if f1%(2%)=0% then L16445
            gosub L32000
            goto editmode

        REM *************************************************************~
            *                                                           *~
            *  SCAN B.O.s FOR YOUR VIEWING ENJOYMENT                    *~
            *                                                           *~
            *************************************************************

        see_open_pipins

            routine% = 3%

            i$(), part$(), descr$, tag$(), oldven$  = " "
            i$(1) = "Review Of Open Buy Orders ("
            if savehit%=12 then i$(1) = "Review Of" & hex(84) & "LATE" & ~
                                                 hex(8c) & "Buy Orders ("
            if akey%=2% then i$(1)=i$(1)&"By Suggested Order Date)"
            if akey%=1% then i$(1)=i$(1)&"In Part Number Order)"
            if akey%=3% then i$(1)=i$(1)&"In Vendor Sort Order)"

            i$( 2)="Part Number (Description)                           O~
        ~rder    Due In    Quantity"
            i$(22) ="(Position Cursor And Press RETURN To Process Buy Ord~
        ~er)"
            i$(23)="(1)Start Over       (2)First  (4)Prev Pge  (13)Instru~
        ~ctions (15)Print Screen   "
            i$(24)="(3)Where Used                 (5)Next Pge  (14)Trace ~
        ~BO     (16)Return to Input"

               if loaded% = 1% then L17530          /* Allow direct view */
                                                   /* if ret'n from edit*/
            call "SHOSTAT" ("Now Loading Advices ...")
            init (hex(00)) plowkey$
            init (hex(8c)) lfac$()
            lfac$(20%)=hex(ac)
            call "DELETE" (#50, plowkey$, 0%) /* Purge workfile        */

            init (hex(00)) plowkey$
            str(plowkey$,,2)="BO"

            lll%=3%

            found%=0%          /* Check if any beasties exist          */
L17410:     call "PLOWNEXT" (#1, plowkey$, 2%, f1%(1))
            if f1%(1)=0 then L17450
            gosub L18190
            goto L17410
L17450:     if str(plowkey$,,2%)="BW" then L17500   /* Verify BW PIPINs */
               init (hex(00)) plowkey$
               str(plowkey$,,2%)="BW"
               goto L17410

L17500:     if found% = 0% then inputmode
            loaded% = 1%
            init (" ")  i$(3%)

L17530:     init (hex(00)) plowkey$ : page% = 1% : lll% = 3%
            call "PLOWALTS" (#50, plowkey$, akey%, 0%, f1%(50%))
            init (hex(00)) keyarray$(1%,page%), keyarray$(2%,page%),     ~
               keyarray$(3%,page%)

            goto L17615
L17590:     lll% = lll% + 1%
                if lll% > 20% then L17820
            call "PLOWALTS" (#50, plowkey$, akey%, 0%, f1%(50%))
L17615:         if f1%(50%) = 0% and lll% = 3% then inputmode
                if f1%(50%) = 0% then L17820
L17630:     get #50 using L17640, vencode$
L17640:         FMT POS(87), CH(9)
            if akey% <> 3% then L17780
                if lll% = 3% then L17700
                if vencode$ = oldven$ then L17780
                if vencode$ = " " then L17780
                if lll% + 3% > 20% then L17812
L17700:         oldven$ = vencode$
                lll% = lll% + 1%
                call "DESCRIBE" (#3, vencode$, vencodedescr$, 1%, f1%(3%))
                i$(lll%) = "Advices for Vendor Code: " & vencode$ &      ~
                     "  -  " & vencodedescr$
                lll% = lll% + 1%
L17780:     get #50 using L17790, tag$(lll%), i$(lll%), part$(lll%)
L17790:         FMT CH(19), POS(115), CH(80), POS(20), CH(25)
            goto L17590

L17812:     str(plowkey$,,28%) =   /* Already been PLOWed, so set a */   ~
                str(plowkey$,,28%) addc all(hex(ff))   /* bit back. */

L17820:     gosub inquiry_screen
            i$(21%) = " "

            if keyhit% <> 5 then L18020

                call "PLOWALTS" (#50, plowkey$, akey%, 0%, f1%(50%))
                     if f1%(50%)=0% then L17820
                init (" ") str(i$(),185%,1748%)
                lll%=3% : page%=page%+1%
                get #50 using L17930, keyarray$(1%,page%),                ~
                     keyarray$(2%,page%), keyarray$(3,page%)
L17930:              FMT POS(20), CH(44), POS(64), CH(23), POS(87), CH(28)
           str(keyarray$(1%,page%),,44%)=str(keyarray$(1%,page%),,44%)   ~
                         addc  all (hex(ff))
           str(keyarray$(2%,page%),,23%)=str(keyarray$(2%,page%),,23%)   ~
                         addc  all (hex(ff))
           str(keyarray$(3%,page%),,28%)=str(keyarray$(3%,page%),,28%)   ~
                         addc  all (hex(ff))
                goto L17630

L18020:     if keyhit% <> 4% then L18120
                if page%=1% then L17820
L18040:         plowkey$=keyarray$(akey%,page%-1%)
                call "PLOWALTS" (#50, plowkey$, akey%, 0%, f1%(50%))
                     if f1%(50%)=0% then L17820
                page%=page%-1%
                init (" ") str(i$(),185%,1748%)
                lll%=3%
                goto L17630

L18120:     if keyhit% <> 2% then L18140
                page%=2% : goto L18040
L18140:     if keyhit% = 16% then inputmode
            if keyhit% =  0% then L18610
            if keyhit% = 3% then where_used
            if keyhit% = 14 then trace_demand
                goto L17820

L18190:     get #1, using L18200, readkey$, j%
L18200:     FMT CH(25), XX(31), BI(4)
            call "READ100" (#4, readkey$, f1%(4))
                if f1%(4) = 0 then L18330
            if admin% = 1% then L18330
            get #4, using L18250, descr$, main_ven$, partclass$
L18250:     FMT POS(26), CH(32), POS(102), CH(9), POS(200), CH(3)
            search codes$() = str(partclass$,,3) to cursor%() step 3
                if cursor%(1) = 0 then return
                if alt$((cursor%(1%)+2%)/3)=" " then L18330   /* Main   */
                if altb%=0% then return  /* Not allowing 'Alt' Buyers  */
                if alt$((cursor%(1%)+2%)/3)<>"Y" then return
                     /* Fall through if buyer marked 'alternate'       */

L18330:     REM all clear, get info...
            if j% < today% or savehit% <> 12% then L18370
                return clear
                goto L17500
L18370:     get #1, using L18390, part$(lll%), i%, tag$(lll%), q
            found%=found%+1%             /* Beastie counter            */
L18390:     FMT CH(25), BI(4), CH(19), PD(14,4)
*          IF ADMIN% <> 1% THEN 17730 /* cause we got descrip at 17620 */
*          CALL "READ100" (#4, PART$(LLL%), F1%(4))
*              IF F1%(4) =  0 THEN 17730

            if f1%(4%)<>0% then get #4, using L18450, descr$, main_ven$
L18450:         FMT POS(26), CH(32), POS(102), CH(9)

            str(i$(lll%),,50) = part$(lll%) & " (" &  descr$ & ")"
            call "DATE" addr("G+",basedate$,i%-1%,str(i$(lll%),61,6),err%)
               call "DATEFMT" (str(i$(lll%),61,8))
            call "DATE" addr("G+",basedate$,j%-1%,str(i$(lll%),52,6),err%)
               call "DATEFMT" (str(i$(lll%),52,8))
            call "CONVERT" (q, 0.2, str(i$(lll%),70%,10%))
            call "READ101" (#50, tag$(lll%), f1%(50))
                if f1%(50%)<>0% then delete #50
            write #50 using L18570, tag$(lll%),part$(lll%),tag$(lll%), j%,~
                tag$(lll%), main_ven$,tag$(lll%), str(i$(lll%),,80%)
L18570:         FMT CH(19), CH(25), CH(19), BI(4), CH(19), CH(9), CH(19),~
                    CH(80)
                return

L18610:     if cursor%(1%) < 3 or cursor%(1%) > lll%-1% then L17820
            if str(i$(cursor%(1%))) = " " then L17820
            if str(i$(cursor%(1%)),,25%) = "Advices for Vendor Code: "   ~
                                                               then L17820
            part$ = part$(cursor%(1%))
            gosub initialize
            gosub'151(1%)
               if errormsg$ = " " then bo_fast
            i$(21) = errormsg$ : part$ = " "
            goto L17820

        trace_demand
            if cursor%(1%) > 2 and cursor%(1%) < lll% then L18740
L18720:        i$(21) = "Please Position Cursor First"
               goto L17820
L18740:     if str(i$(cursor%(1%))) = " " then L18720
                if keyhit% = 3% then L18810 /* Back to WHERE_USED */
            tagnr$ = tag$(cursor%(1%))
            call "GETDEM"(1%, tagnr$, #11, #21, #1, " ", " ", 0%)
            goto L17820

        where_used
            goto trace_demand  /* Just for the line position */
L18810:     usedkey$ = part$(cursor%(1%))
            init (hex(00)) str(usedkey$,26%)
            call "PLOWALTS" (#22, usedkey$, 1%, 25%, f1%(22%))
            if f1%(22%) <> 0% then L18880
                i$(21%)   = "Not found as a component of any Product" &  ~
                            " Structure (BOM)."
                goto L17820
L18880:     misc$ = hex(06) & "Part " & part$(cursor%(1%)) &             ~
                                                " is used in these BOM's"
            hdr$(1%) = "  Assembly Part Code        ID   Part Description"
            incl(1%) = 0
            usedkey$ = part$(cursor%(1%))
            call "PLOWCODE" (#22, usedkey$, misc$, 8025%, 1.32, f1%(21), ~
                             hdr$(), 28, 0, incl(), incl$()," ","Y", #4)
            goto L17820

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave

            save_msg% = 0%
            if abs(qtyorder) < .0001 then L19200

            if orddate$ = " " or orddate$ = blankdate$ then L19160
            if duedate$ = " " or duedate$ = blankdate$ then L19160
            if vencode$ = " " then L19160
            if ourprice$= " " then L19160
               goto L19410
L19160:     errormsg$="Incomplete Information, Please Edit"
            goto editmode


L19200
*        Complete delete of PORLSE Header record, All PIPs and other
*        files directly corrected with the Header must be cleaned up.
*        Other files more directly related to the possible PORLSE lines
*        records will be cleaned up when the lines records are deleted.

            for i% = 1% to maxlines%
                if pipstat$(i%) <> "C" then pipstat$(i%) = " "
            next i%

            call "SHOSTAT" ("Preparing to Delete the Purchase Directive")
            save_msg% = 1%

            if pj_on%     <> 1% then L19410            /* No PIPOUTs */
            if oldreltag$ = " " then L19410            /* No PIPs */
                plowkey$ = str(oldreltag$) & hex(00)
                gosub delete_pipout               /* Delete PIPOUTs */
                tagnr$ = oldreltag$
                gosub delete_jbcross2            /* Delete JBCROSS2 */
                plowkey$ = str(oldreltag$)
                call "DELETE" (#51, plowkey$, 19%)   /* Clean-up WF */

L19410:     if save_msg% <> 0% then L19450
            call "SHOSTAT" ("Preparing to Save the Purchase Directive")
            save_msg% = 1%

L19450:     if pobuffrkey$ <> " " then call "DELETE" (#2,pobuffrkey$,47%)
                if oldreltag$ = " " then L19850         /* No PIPINs */
                     tagnr$ = oldreltag$  :  gosub delete_pipin
                     if abs(qtyorder) < .0001 then L19850

*        If the PORLSE Header Record is to be saved later, it must be
*        checked for a change in status so the PIPOUTs for a Purchased
*        Job can be correctly maintained

*        First Check for conditions where no PIPOUTs exist. (No PJ)
                if pj_on%   <> 1% then L19850            /* No PIPOUTs */
                if pj_stat% =  0% then L19850            /* No PIPOUTs */
                if old_pj_job$ = " " and pj_job$ = " " then L19850

                if old_pj_job$ <> pj_job$ then L19700

*        Was purchased job and is still purchased job, but Kit List may
*        have been changed so old kit list must go away.

                if pj_stat%  = 3% then L19850    /* Leave Kit List Alone */
                if pj_stat% <> 4% then L19850    /* Something wrong */
                     plowkey$ = str(oldreltag$) & hex(00)
                     gosub delete_pipout       /* Delete 'old' Kit List */
                     goto L19850

L19700:         if old_pj_job$ <> " " then L19750

*        Was NOT purchased job and is NOW purchased job, No PIPOUTs Yet
                goto L19850

L19750
*        Was purchased job and is now NOT purchased job, Delete PIPOUTs,
*        JBCROSS2 record and Clean up the workfile
                plowkey$ = str(oldreltag$) & hex(00)
                gosub delete_pipout
                tagnr$ = oldreltag$
                gosub delete_jbcross2
                if pj_stat% = 3% then goto L19850
                     plowkey$ = oldreltag$      /* Clean-up Work File */
                     call "DELETE" (#51, plowkey$, 19%)

L19850:     gosub L31000

            if keyhit% <> 16% then L19930
                if routine% = 1% and savehit% = 8% then goto see_all_pod
                if routine% = 2% then goto see_all_pod_vendor
                if routine% = 3% then goto see_open_pipins
                goto inputmode

L19930:     gosub bo_fast_init
            gosub L30000
            goto bo_fast

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%:inpmessage$ = " "
                  on fieldnr% gosub L20100,         /* PART             */~
                                    L20200,         /* ORDER DATE       */~
                                    L20300,         /* DUE DATE         */~
                                    L20400,         /* VENDOR           */~
                                    L20500,         /* VENDOR PART NO   */~
                                    L21080,         /* Currency Code    */~
                                    L20600,         /* VENDOR SHIP UNIT */~
                                    L20700,         /* CONVERSION FACTOR*/~
                                    L20800,         /* ORDER QUANTITY   */~
                                    L20900,         /* UNIT PRICE       */~
                                         ,         /* Inventory Cost   */~
                                    L21150,         /* Purchase Job     */~
                                    L21300          /* Purchase Contract*/
                  return

L20100: REM DEFAULT/ENABLE FOR PART
            return
L20200: REM DEFAULT/ENABLE FOR ORDER DATE
            if orddate$ = " " or orddate$ = blankdate$ then L20240
               enabled% = 0%
               return
L20240:     orddate$=date$
            return
L20300: REM DEFAULT/ENABLE FOR DUE DATE
            if pj_on% = 1% and pj_stat% > 2% then L20340
            if duedate$ = " " or duedate$ = blankdate$ then return
               enabled% = 0%
               return
L20340:     if duedate$ = " " or duedate$ = blankdate$ then L20360
               enabled% = 0%
               return
L20360:     temp$ = orddate$  :  call "DATUNFMT" (temp$)
            call "DATE" addr("G+", temp$, pj_lead%, duedate$, err%)
                call "DATEFMT" (duedate$)
                return

L20400: REM DEFAULT/ENABLE FOR VENDOR
                enabled%=0%
                return
L20500: REM DEFAULT/ENABLE FOR VENDOR PART NO
            if venpart$<>" " then enabled%=0%
            return
L20600: REM DEFAULT/ENABLE FOR VENDOR SHIP UNIT
            if meas$ = " " then L20640
               enabled%=0%
               return
L20640:     meas$ = stkg$
            return
L20700: REM DEFAULT/ENABLE FOR CONVERSION FACTOR
            if factor <> 0 then L20740
                factor = 1
                goto L20750
L20740:     enabled% = 0%
L20750:     gosub'040
            return
L20800: REM DEFAULT/ENABLE FOR ORDER QUANTITY
            if abs(qtyorder) < .0001 then L20850
               gosub'040
               if pj_on% = 1% and pj_job$ = "PJ" then return
               enabled% = 0%
               return
L20850:     venorder$, qtyorder$ = " "
            return
L20900: REM DEFAULT/ENABLE FOR UNIT PRICE
            if abs(ourprice) < .0000001 then L20950
               gosub'040
               enabled% = 0%
               return
L20950:     venprice$, ourprice$ = " "
            return

        REM DEFAULT/ENABLE FOR INVENTORY COST
            if abs(totalstdcost) < .0001 then L21050
               gosub'040
               enabled% = 0%
               return
L21050:     totalstdcost$ = " "
            return

L21080: REM DEFAULT/ENABLE FOR CURRENCY CODE
            if curr_on_flag$ = "Y" then L21110
                enabled% = 0% : return
L21110:     if currency$ = " " then L21130
                enabled% = 0% : return
L21130:     currency$ = currvend$ : return

L21150: REM DEFAULT/ENABLE FOR PURCHASE JOB
            if pj_on% <> 0% then L21180
                enabled% = 0% : return
L21180:     if bom_check% <> 0% then L21200
                enabled% = 0% : return
L21200:     if pj_stat% <> 0% then L21220
                enabled% = 0% : return
L21220:     if pj_job$ <> " " then return
            if pj_stat% < 3% then return
            pj_job$ = "PJ"
            return

L21300: REM Default/Enable For Purchase Contract
            enabled% = 0%
            if contract$ <> " " then return
            if str(part$,,9%) = "ACTIVITY:" then contract_type$ = "A"    ~
                                            else contract_type$ = "P"
            if contract_type$ = "A" then item$ = str(part$,11%,4%)       ~
                                           else item$ = part$
            call "VPCDEFLT" (vencode$, contract$, contract_line$,        ~
                             contract_type$,item$,orddate$, #16, f1%(16%))
            return

        REM *************************************************************~
            * INITIALIZATION FOR INPUTMODE                              *~
            *************************************************************
        initialize
        init (" ")                                                       ~
            errormsg$,                   /* ERROR MESSAGE              */~
            inpmessage$,                 /* INPUT MESSAGE              */~
            tempvend$,                                                   ~
            factor$,                     /* QUANTITY PER CASE          */~
            meas$,                       /* UNIT OF MEASURE            */~
            ourprice$,                   /* PRICE PER UNIT             */~
            venprice$,                   /* PRICE PER UNIT             */~
            ext$,                        /* LINE EXTENSION             */~
            totalstdcost$,               /* INVENTORY COST             */~
            vencode$,                    /* VENDOR                     */~
            vencodedescr$,               /* VENDOR                     */~
            venpart$,                    /* VENDOR PART NBR            */~
            costtype$,                   /* Part's Costing Method      */~
            currency$, currvend$,        /* Currency Codes             */~
            contract$, contract_line$,   /* Contract ID                */~
            job$, jobdescr$              /* Job                        */

            venprice, ourprice, totalstdcost = 0
            factor = 0
            init (hex(00)) invcosts$
            defer% = 0%

        bo_fast_init:

            init (" ")                                                   ~
            duedate$,                    /* DUE DATE                   */~
            pobuffrkey$,                 /* OLD PORLSE  KEY FOR DELETE */~
            pipin$  (),                  /* PIPIN TAG                  */~
            venorder$,                   /* QUANTITY IN 'CASES'        */~
            qtyorder$,                   /* QUANTITY                   */~
            orddate$,                    /* ORDER DATE                 */~
            pipdue$ (),                  /* PIP DUE DATE               */~
            pipord$ (),                  /* PIP ORDER DATE             */~
            pipqty$ (),                  /* PIP QUANTITY REQUIRED      */~
            pipstat$(),                  /* PIP STATUS THIS RUN        */~
            oldpipcross$,                /* Old Pipcross PIPOUT Tag    */~
            oldpipstat$(),               /* PIP Status Prior Run       */~
            oldreltag$,                  /* Old PD Rel PIP Tag - Header*/~
            oldreltag$(),                /* Old PD Rel PIP Tag - Lines */~
            old_pj_job$,                 /* Purchased Job Flag         */~
            pj_job$,                     /* Purchased Job Flag         */~
            bomid$, rteid$               /* Purchased Job BOM & RTE    */~

            maxlines%, l% = 0%
            qtyorder, venorder = 0
            bom_check%, pj_stat% = 0%

            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1984, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover

            keyhit1% = 2%  /* PUT MSG AREA AT BOTTOM OF SCREEN  */
            call "STARTOVR" (keyhit1%)
                if keyhit1% = 1% then return
                if keyhit1% <> 0% then startover
               REM START OVER            (ENTER)
L29930:            init (hex(00))  plowkey$
                   call "DELETE" (#51, plowkey$, 0%)
                   return clear all
                   goto inputmode

L30000: REM *************************************************************~
            *                                                           *~
            *  LOAD OPEN BUY ORDERS FOR THIS PART                       *~
            *                                                           *~
            *************************************************************

        REM LOAD OPEN BUY ORDERS
            init (hex(00)) readkey$
            str(readkey$,,25) = part$

L30100:     if maxlines%=100% then return
            call "PLOWALTS" (#1,readkey$,1%,25%,f1%(1))
               if f1%(1) = 0 then L30220
                  get #1, using L30140, temp$
L30140:             FMT XX(29), CH(2)
            if str(temp$,,2%)<>"BO" and                                  ~
               str(temp$,,2%)<>"BW" then L30100
            maxlines%=maxlines%+1%
            gosub format_line
            oldreltag$(maxlines%) = pipin$(maxlines%)
            oldpipstat$(maxlines%) = " "
            goto L30100

L30220:     if maxlines% = 0% then return
            inpmessage$ = "Select BO/BWs To Satisfy, If Any. 'R' = Sat"& ~
                          "isfy, 'C' = Cancel, Blank = Ignore."
            return

        format_line

            get #1, using L35030, due%, pipin$(maxlines%), qty, st%
                call "DATE" addr("G+", basedate$, st%-1%,                ~
                                             pipord$(maxlines%), err%)
                call "DATE" addr("G+", basedate$, due%-1%,               ~
                                             pipdue$(maxlines%), err%)
                call "DATEFMT" (pipord$(maxlines%))
                call "DATEFMT" (pipdue$(maxlines%))
                call "CONVERT" (qty, 0.2, pipqty$(maxlines%))
                due%(maxlines%)=due%
            return

L31000: REM *************************************************************~
            *                                                           *~
            *   SAVE RESULTS OF THIS MESS                               *~
            *                                                           *~
            *************************************************************

            if abs(qtyorder) < .0001 then L31500       /*JUST CHECK PIP*/
            call "SHOSTAT" ("Saving Purchase Directive, Ajusting PIP")
            call "DATUNFMT" (duedate$)
            call "DATUNFMT" (orddate$)
            s% = 0%

L31086:     s% = s% + 1%
            convert s% to seq$, pic(000)
            pobuffrkey$ = holdrel1$ & str(userid$,,3%)
            str(pobuffrkey$,  5,  9) = str(vencode$,,9)
            str(pobuffrkey$, 14, 25) = str(part$,,25)
            str(pobuffrkey$, 39,  6) = str(orddate$,,6)
            str(pobuffrkey$, 45,  3) = str(seq$,,3)
            init (hex(00)) str(pobuffrkey$, 48)
            call "PLOWNEXT" (#2, pobuffrkey$, 47%, f1%(2))
               if f1%(2)<>0 then L31086

            if oldreltag$ <> " " then L31171
                init (" ")  oldreltag$
                call "PIPINDEX" (#6, orddate$, ord%, err%)
                convert ord% to str(oldreltag$,3%,3%), pic(###)
                str(oldreltag$,1%, 2%) = "RO"
                str(oldreltag$,6%,14%) = tagdate$ & time
                if pj_on% <> 0% and pj_job$ = "PJ" then                  ~
                                            str(oldreltag$,1%, 2%) = "RW"

L31171:     piptag$ = oldreltag$
            if old_pj_job$ = pj_job$ then L31180
                if old_pj_job$ = "  " then str(piptag$,1%,2%) = "RW"
                if old_pj_job$ = "PJ" then str(piptag$,1%,2%) = "RO"

L31180:     write #2 using L35070, holdrel1$, userid$, vencode$, part$,   ~
                      orddate$, seq$, " ", qtyorder, totcost, venpart$,  ~
                      duedate$, meas$, factor, ourprice, totalstdcost,   ~
                      invcosts$, currency$, piptag$, job$, contract$,    ~
                      contract_line$, " "
*        Now process new PIPIN and PIPOUT's for the Purchase Directive
            if part$ = " " then L31500
            call "READ100" (#7, part$, f1%(7%))
                if f1%(7%) = 0% then L31500
            call "PIPINDEX" (#6, duedate$, due%, err%)
            call "PIPINDEX" (#6, orddate$, ord%, err%)
            write #1 using L31285, part$, due%, piptag$, qtyorder, ord%
L31285:         FMT CH(25), BI(4), CH(19), PD(14,4), BI(4)
            call "PIPFLAGS" (part$, 1%, due%, qtyorder, #7, #8)

*        Now Check Check for PIPOUT's for Purchased Jobs
            if pj_on% <> 1% then L31500
            if pj_job$ <> "PJ" then L31500

                temp1$ = orddate$
                temp2$ = duedate$
                call "PJINPSUB" (part$, " ", " ", " ", " ", oldreltag$,  ~
                                 temp1$, temp2$, bom_id$, qtyorder,      ~
                                 pj_stat%, 3%, #25, #4, #22, #23, #7, #8,~
                                 #6, #51)
*        Create JBCROSS2 Record
            write #26 using L31385, part$, rteid$, piptag$, part$, bomid$,~
                                   piptag$, eod goto L31500
L31385:         FMT CH(25), CH(3), CH(19), CH(25), CH(3), CH(19)

L31500: REM NOW PROCESS 'LINE ITEMS'

            if maxlines% = 0% then L31550

            for i% = 1% to maxlines%
            if pipstat$(i%) = "C" then gosub L31800
            if pipstat$(i%) = "R" then gosub L31700
            if pipstat$(i%) = " " then gosub L31600
            next i%

L31550:     if oldreltag$ = " " then return
            if oldreltag$ = piptag$ then return
                call "JBRETAG" (oldreltag$, piptag$, #26, #7, #28, #1,   ~
                                #25, #11, #27)
            return

L31600: REM LEAVE UNSATISFIED
            if oldpipstat$(i%) = " " then return
                tagnr$ = oldreltag$(i%)
                gosub move_from_popipxrf

            return

L31700: REM RELEASE WITH THIS PURCHASE DIRECTIVE
            if oldpipstat$(i%) = "R" then L31730
                tagnr$ = oldreltag$(i%)
                gosub move_to_popipxrf


L31730:     str(pipin$(i%),,1%)="R"

            write #2 using L35070, holdrel1$, userid$, vencode$, part$,   ~
                           orddate$, seq$, pipin$(i%), 0, 0, " ", " ",   ~
                           " ", 0, 0, totalstdcost, invcosts$, " ",      ~
                           oldreltag$(i%), job$, contract$,              ~
                           contract_line$, " "
            return

L31800: REM CANCEL THIS BOs
            if oldpipstat$(i%) <> "R" then L31835
                plowkey$ = str(oldreltag$(i%)) & "PO"
                call "DELETE" (#24, plowkey$, 21%)
                tagnr$ = oldreltag$(i%)
                gosub move_from_popipxrf

L31835:     tagnr$ = oldreltag$(i%)
            gosub delete_pipin
            plowkey$ = str(oldreltag$(i%)) & hex(00)
            gosub delete_pipout
            tagnr$ = oldreltag$(i%)
            gosub delete_jbcross2

            init (hex(00)) pipcross$
            str(pipcross$,,19%) = oldreltag$(i%)
            call "PLOWAL1" (#11, pipcross$, 1%, 19%, f1%(11%))
            if f1%(11%) = 0% then L31915
            delete #11
            put #11, using L31900, " CANCELLED       "
L31900:         FMT POS(22), CH(17)
            write #11

L31915:     call "READ101" (#50, oldreltag$(i%), f1%(50%))
                if f1%(50%) <> 0% then delete #50

            return

L32000: REM *************************************************************~
            *                                                           *~
            *  LOAD INFO FROM BUFFER                                    *~
            *                                                           *~
            *************************************************************

            get #2, using L32220, holdrel1$,                              ~
                       vencode$, part$, orddate$, qtyorder,              ~
                       venpart$, duedate$, meas$, factor,   ourprice,    ~
                       totalstdcost, invcosts$, currency$, oldreltag$,   ~
                       job$, contract$, contract_line$
            oldholdrel$=holdrel1$
L32220:     FMT CH(1), XX(3), CH(9), CH(25), CH(6), XX(22), PD(14,4),    ~
                     XX(8), CH(25), CH(6), CH(4), PD(14,4), PD(14,7),    ~
                     PD(14,4), CH(96), CH(4), CH(19), CH(08), CH(16),    ~
                     CH(4)
            call "CONVERT" (totalstdcost, 4.4, totalstdcost$)
            if totalstdcost = -1 then totalstdcost$ = "*Deferred*"
            if factor <= 0 or factor > 1000000 then factor = 1
            call "DATEFMT" (duedate$)
            call "DATEFMT" (orddate$)
            call "DESCRIBE" (#3, vencode$, vencodedescr$, 1%, f1%(3%))
            call "DESCRIBE" (#15, job$, jobdescr$, 1%, f1%(15%))
            call "DESCRIBE" (#4, part$, partdescr$, 1%, f1%(4%))
                if f1%(4%) = 1% then L32390
                     partdescr$ = "(* * * NON STOCKED PART * * *)"
                     stkg$ = "EACH"
                     goto L32410
L32390:     get #4, using L32395, stkg$, serial_no$, type$, partclass$,   ~
                                 costtype$
L32395:         FMT POS(74), CH(4), POS(131), CH(1), POS(180), CH(3),    ~
                    POS(200), CH(3), POS(307), CH(1)

L32410:     if pj_on% <> 1% then L32430
                gosub purchase_job_check
                if pj_stat% = 0% then L32430
                     if str(oldreltag$,1%,2%) <> "RO" then L32420
                          pj_stat%    = 1%
                          old_pj_job$ = " "
                          goto L32425
L32420:              if str(oldreltag$,1%,2%) <> "RW" then L32425
                          pj_stat%    = 3%
                          old_pj_job$ = "PJ"
L32425:              pj_job$ = old_pj_job$
L32430:     gosub'040

            maxlines%=0%
            pobuffrkey$=str(pobuffrkey$,,47%) & "R" & hex(00)
L32470:     call "PLOWNEXT" (#2, pobuffrkey$, 47%, f1%(2%))
               if f1%(2%) = 0% then L30000
            if maxlines% > 99% then L30000
            maxlines% = maxlines% + 1%
               get #2, using L32510, pipin$(maxlines%),                   ~
                                    oldreltag$(maxlines%)

L32510:            FMT POS(48), CH(19), POS(242), CH(19)
               if oldreltag$(maxlines%) <> " " then L32620
                      call "READ100" (#1, pipin$(maxlines%), f1%(1%))
                         if f1%(1%) <> 0% then L32580
                            pipin$(maxlines%)       = " "
                            oldreltag$(maxlines%)   = " "
                            maxlines% = maxlines% - 1%
                            goto L32470
L32580:               gosub format_line
                      pipstat$(maxlines%) = "R"
                      oldpipstat$(maxlines%) = " "
                      oldreltag$(maxlines%) = pipin$(maxlines%)
                      goto L32470

L32620:     /* Load Release details from the POPIPXRF file */

            plowkey$ = str(oldreltag$(maxlines%)) & "PI"
            call "PLOWNEXT" (#24, plowkey$, 21%, f1%(24%))
               if f1%(24%) <> 0% then L32670
                  pipin$(maxlines%)       = " "
                  oldreltag$(maxlines%)   = " "
                  maxlines% = maxlines% - 1%
                  goto L32470
L32670:         get #24, using L32680, due%, qty, st%
L32680:             FMT POS(47), BI(4), POS(59), PD(14,4), BI(4)
                call "DATE" addr("G+", basedate$, st%-1%,                ~
                                             pipord$(maxlines%), err%)
                call "DATE" addr("G+", basedate$, due%-1%,               ~
                                             pipdue$(maxlines%), err%)
                call "DATEFMT" (pipord$(maxlines%))
                call "DATEFMT" (pipdue$(maxlines%))
                call "CONVERT" (qty, 0.2, pipqty$(maxlines%))
                due%(maxlines%)=due%
                pipstat$(maxlines%) = "R"
                oldpipstat$(maxlines%) = "R"
            goto L32470


        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

L35030: FMT                      /* FILE: PIPIN                        */~
            XX(25),              /* Part code                          */~
            BI(4),               /* Date in subscript for PIP          */~
            CH(19),              /* Tag number in level 2 planning     */~
            PD(14,4),            /* Quantity of something in packed de */~
            BI(4)                /* Date to start as a date subscript  */~

L35070: FMT                      /* FILE  PORLSE                       */~
            CH(1),               /* STATUS FLAG (RESERVED)             */~
            CH(3),               /* BUYER CODE                         */~
            CH(9),               /* ASSIGNED VENDOR CODE               */~
            CH(25),              /* PART NBR                           */~
            CH(6),               /* ORDER DATE                         */~
            CH(3),               /* SEQUENCE NUMBER                    */~
            CH(19),              /* PO/LINE OR PIPIN TAG NR            */~
            PD(14,4),            /* QUANTITY TO PURCHASE               */~
            PD(14,4),            /* EXTENSION                          */~
            CH(25),              /* VENDOR'S PART NBR                  */~
            CH(6),               /* DUE DATE                           */~
            CH(4),               /* UNIT OF MEASURE                    */~
            PD(14,4),            /* QUANTITY PER CASE                  */~
            PD(14,7),            /* UNIT PRICE                         */~
            PD(14,4),            /* Total Standard Cost                */~
            CH(96),              /* 12 Cost Buckets                    */~
            CH(4),               /* Currency Code                      */~
            CH(19),              /* Header Record PIPTAG               */~
            CH(08),              /* Purchase for Job Code              */~
            CH(16),              /* Purchase Contract ID               */~
            CH(04),              /* Purchase Contract Line             */~
            CH(204)              /* Filler for the rest of it          */

        FMT                      /* FILE PIPCROSS                      */~
            XX(16),              /* DEMAND CODE                        */~
            XX(3),               /* DEMAND LINE                        */~
            XX(19),              /* PIPIN TAG                          */~
            CH(19),              /* PIPOUT TAG                         */~
            XX(6),               /* SYSTEM DATE                        */~
            XX(8),               /* SYSTEM TIME                        */~
            XX(1),               /* PEGGING OPTION                     */~
            XX(25),              /* PART NUMBER                        */~
            XX(8),               /* PROCUREMENT QTY  (  PD(14,4)  )    */~
            XX(8),               /* REQUIREMENT QTY  (  PD(14,4)  )    */~
            XX(1),               /* DEMAND TYPE                        */~
            XX(1),               /* PRIORITY CODE                      */~
            XX(6),               /* PIPIN START DATE                   */~
            XX(6),               /* PIPIN END DATE                     */~
            XX(23)               /* FILLER                             */

L35390: FMT                      /* FILE: POPIPXRF                     */~
            CH(19),              /* Tag number in level 2 planning     */~
            CH(2),               /* ID Code for where from 'PI' or 'PO'*/~
            CH(25),              /* Part code                          */~
            BI(4),               /* Date in/out subscript for PIP      */~
            CH(8),               /* System Time Variable for PIPOUT    */~
            PD(14,4),            /* Quantity of something in packed de */~
            BI(4),               /* Date to start as a date subscript  */~
            CH(3),               /* BOM ID for JBCROSS2 - Header only  */~
            CH(3),               /* Route ID for JBCROSS2 - Header only*/~
            CH(19),              /* PIPCROSS PIPOUT tag - Header only  */~
            CH(5)                /* FILLER                             */

        REM *************************************************************~
            * Miscellaneous Purchase Job and PIP Management Routines.   *~
            *                                                           *~
            *************************************************************

        purchase_job_check
            bomid$, rteid$ = " "
            if pj_on% <> 1% then return
            if serial_no$ = "Y" then return
            if bom_check% <> 0% then return
            if pj_stat% <> 0% then return
            if orddate$ = " " or orddate$ = blankdate$ then return
                temp$ = orddate$  :  call "DATUNFMT" (temp$)
                call "BOMFIND" (part$, temp$, #23, #6, bom_id$)
                     if bom_id$ = " " then L36210
                bomid$ = bom_id$

                readke2$ = str(part$) & str(bom_id$) & "  0"
                call "READ100" (#22, readke2$, f1%(22%))
                     if f1%(22%) = 0% then L36210
                pj_stat% = 1%
                get #22 using L36170, rteid$, pj_bom$
L36170:              FMT POS(87), CH(3), POS(145), CH(1)
                if pj_bom$ <> "Y" then L36210
                pj_stat% = 3%
                pj_job$ = "PJ"
L36210:         bom_check% = 1%
            return

        delete_pipin
            if tagnr$ = " " then return
                call "READ101" (#1, tagnr$, f1%(1%))
                     if f1%(1%) = 0% then L36380
                get #1 using L36350, pippart$, due%, qty
L36350:              FMT CH(25), BI(4), POS(49), PD(14,4)
                delete #1
                call "PIPFLAGS" (pippart$, 1%, due%, -qty, #7, #8)
L36380:     return

        delete_pipout
            if str(plowkey$,1%,19%) = " " then return
L36510:         call "PLOWNXT1" (#25, plowkey$, 19%, f1%(25%))
                     if f1%(25%) = 0% then return
                get #25 using L36525, pippart$, due%, qty
L36525:              FMT POS(20), CH(25), BI(4), POS(57), PD(14,4)
                delete #25
                call "PIPFLAGS" (pippart$, 1%, due%, qty, #7, #8)
                goto L36510

        delete_jbcross2
            if tagnr$ = " " then return
                call "READ101" (#26, tagnr$, f1%(26%))
                     if f1%(26%) = 0% then return
                delete #26
                return

        move_to_popipxrf
            /* PIPCROSS */
            init (hex(00))  pipcross$
            str(pipcross$,1%,19%) = tagnr$
            call "PLOWAL1" (#11, pipcross$, 1%, 19%, f1%(11%))
                if f1%(11%) = 0% then L37010
            get #11 using L36955, oldpipcross$
L36955:         FMT CH(19)
            delete #11
            put #11 using L36980, oldreltag$
L36980:         FMT POS(20), CH(19)
            write #11

L37010:     /* JBCROSS2 */
            rteid$, bomid$ = " "
            call "READ101" (#26, tagnr$, f1%(26%))
                if f1%(26%) = 0% then L37170
            get #26 using L37060, rteid$, bomid$
L37060:         FMT POS(26), CH(3), POS(73), CH(3)
            delete #26

L37170:     /* PIPIN */
            call "READ101" (#1, tagnr$, f1%(1%))
                if f1%(1%) = 0% then L37340
            get #1 using L37210, pippart$, due%, qty, st%
L37210:         FMT CH(25), BI(4), POS(49), PD(14,4), BI(4)
            delete #1
            call "PIPFLAGS" (pippart$, 1%, due%, -qty, #7, #8)

            /* POPIPXRF for PIPIN */
            write #24 using L35390, tagnr$, "PI", pippart$, due%, " ",    ~
                                   qty, st%, bomid$, rteid$,             ~
                                   oldpipcross$, " "

            /* The Screen Display Workfile */
            call "READ101" (#50, tagnr$, f1%(50%))
                if f1%(50%) <> 0% then delete #50

L37340:       /* Retrieve Existing PIPOUTS for "BW" and Deal with Them */
            plowkey$ = str(tagnr$) & hex(00)
L37360:     call "PLOWNXT1" (#25, plowkey$, 19%, f1%(25%))
                if f1%(25%) = 0% then return

            get #25 using L37400, pippart$, due%, piptime$, qty
L37400:         FMT POS(20), CH(25), BI(4), CH(8), PD(14,4)
            delete #25
            call "PIPFLAGS" (pippart$, 1%, due%, qty, #7, #8)

            write #24 using L35390, tagnr$, "PO", pippart$, due%,         ~
                                   piptime$, qty, 0%, " ", " ", " ", " "
            goto L37360

        move_from_popipxrf
            if tagnr$ = " " then return
            plowkey$ = tagnr$
L37530:     call "PLOWNXT1" (#24, plowkey$, 19%, f1%(24%))
                if f1%(24%) = 0% then return
            get #24 using L37560, pipid$
L37560:         FMT POS(20), CH(2)

            if pipid$ = "PI" then L37690          /* PIPIN */
                if pipid$ <> "PO" then L37530     /* PIPOUT */
                     get #24 using L37610, pippart$, due%, piptime$, qty
L37610:                   FMT POS(22), CH(25), BI(4), CH(8), PD(14,4)
                     delete #24
L37625:              piptime$ = time
                     write #25 using L37650, tagnr$, pippart$, due%,      ~
                                            piptime$, qty, eod goto L37625
L37650:                   FMT CH(19), CH(25), BI(4), CH(8), PD(14,4)
                     call "PIPFLAGS" (pippart$, 1%, due%, -qty, #7, #8)
                     goto L37530

L37690
*        Process the PIPIN and update files
            /* PIPIN */
            get #24 using L37730, pippart$, due%, qty, st%, bomid$,       ~
                                 rteid$, oldpipcross$
L37730:         FMT POS(22), CH(25), BI(4), XX(8), PD(14,4), BI(4),      ~
                    CH(3), CH(3), CH(19)
            delete #24
            write #1 using L37770, pippart$, due%, tagnr$, qty, st%
L37770:         FMT CH(25), BI(4), CH(19), PD(14,4), BI(4)
            call "PIPFLAGS" (pippart$, 1%, due%, qty, #7, #8)
            loaded% = 0%               /* Force re-load of BO workfile */

            /* PIPCROSS */
            if oldpipcross$ = " " then L37920
            init (hex(00))  pipcross$
            str(pipcross$,1%,38%) = str(oldpipcross$) & str(oldreltag$)
            call "PLOWNXT1" (#11, pipcross$, 38%, f1%(11%))
                if f1%(11%) = 0% then L37920
            delete #11
            put #11 using L37890, tagnr$
L37890:         FMT POS(20), CH(19)
            write #11

L37920:     /* JBCROSS2 */
            if pj_on% <> 1% then L38000
            if str(tagnr$,2%,1%) <> "W" then L38000
            if bomid$ = " " then L38000
            write #26 using L37980, pippart$, rteid$, tagnr$, pippart$,   ~
                                   bomid$, tagnr$, eod goto L38000
L37980:         FMT CH(25), CH(3), CH(19), CH(25), CH(3), CH(19)

L38000:     goto L37530

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()

                  on fieldnr% gosub L40210          /* PART             */

                     goto L40280

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40210:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40280:     accept                                                       ~
               at (01,02), "Manage Buy Orders And Purchase Directives",  ~
               at (01,66), "Today:", fac(hex(8c)), date$        , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), "Enter Part Number",                          ~
               at (04,20), fac(lfac$( 1)), part$                , ch(25),~
               at (04,46), fac(hex(8c)),   partdescr$           , ch(34),~
               at (05,04), fac(hex(84)) , msg$(12%)             , ch(77),~
               at (06,02), "Then Press Appropriate Key To...",           ~
                                                                         ~
               at (08,04), fac(hex(8c)) , msg$( 1%)             , ch(77),~
               at (09,04), fac(hex(8c)) , msg$( 2%)             , ch(77),~
               at (10,04), fac(hex(8c)) , msg$( 3%)             , ch(77),~
               at (11,04), fac(hex(8c)) , msg$( 4%)             , ch(77),~
               at (12,04), fac(hex(8c)) , msg$( 5%)             , ch(77),~
               at (13,04), fac(hex(8c)) , msg$( 6%)             , ch(77),~
               at (14,04), fac(hex(8c)) , msg$( 7%)             , ch(77),~
               at (15,04), fac(hex(8c)) , msg$( 8%)             , ch(77),~
               at (16,04), fac(hex(8c)) , msg$( 9%)             , ch(77),~
               at (17,04), fac(bfac$)   , msg$(10%)             , ch(77),~
               at (18,04), fac(hex(8c)) , msg$(11%)             , ch(77),~
                                                                         ~
               at (22,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over  (7)Toggle Main/Alternate Buyer",       ~
               at (23,48),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,16), fac(hex(8c)), vsa_pfk$,                       ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(000102030405060708090a0b0c0d0e0f10ff)),          ~
               key (keyhit%)

               if keyhit% <> 11% then L40740
                  call "POVRELSB" (#6,#2,#3,#40,#13,#16,#5,#4,#9)
                  goto L40280

L40740:        if keyhit% <> 14 then L40780
                  call "HNYPRCSB" (" ",part$,0%,#9,#4,#3,#5)
                  goto L40280

L40780:        if keyhit% <> 13 then L40820
                  call "MANUAL" ("PORELSUB")
                  goto L40280

L40820:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40280

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
               gosub set_pf1
               init (hex(8c)) lfac$()  :  init (hex(9c)) pjfac$()
               if holdrel1$ <> " " then L41074
                     holdrel1$ = "R"
L41074:        if holdrel1$ = "R" then holdreldesc$ = "( RELEASED )"
               if holdrel1$ = "H" then holdreldesc$ = "( ON-HOLD )"
               init(hex(8c)) sfac$()
               continued$ = " "
               if pipqty$(l%+7%) <> " " then continued$ = "(continued)"
               if maxlines% <> 100% then str(sfac$(), maxlines% + 1%) =  ~
                                                             all(hex(9c))
               if fieldnr% <> 0% then L41142
                  if maxlines% > 0% then init(hex(86)) str(lfac$(),21%,1%)
L41142:        if pj_on% = 0% then L41150
                     if pj_stat% = 0% then L41150
                          init (hex(8c))  pjfac$()
                          if fieldnr% = 12% then pjfac$(2%) = hex(81)
L41150:           on fieldnr% gosub L41380,         /* PART             */~
                                    L41380,         /* ORDER DATE       */~
                                    L41380,         /* DUE DATE         */~
                                    L41380,         /* VENDOR           */~
                                    L41380,         /* VENDOR PART      */~
                                    L41380,         /* Currency Code    */~
                                    L41380,         /* SHIPPING U/MEAS  */~
                                    L41410,         /* CONVERSION FACTOR*/~
                                    L41410,         /* QUANTITY         */~
                                    L41410,         /* PRICE PER UNIT   */~
                                         ,         /* INVENTORY COST   */~
                                         ,         /* Purchase Job     */~
                                    L41380,         /* Contract & Line  */~
                                    L41280          /* B.O. STATI       */

                     goto L41450

L41280:           REM SET FAC'S FOR BUY ORDER STATUS CODES
                      if maxlines% = 0% then return
                         for i%=1% to maxlines%
                            if pipin$(i%) = " " then L41330
                         lfac$(20%+i%)=hex(81)
L41330:                  next i%
                      return
                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41380:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L41410:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41450:     accept                                                       ~
               at (01,02), "Manage Buy Orders And Purchase Directives",  ~
               at (01,66), "Today:", fac(hex(8c)), date$        , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02),                                               ~
                  "Part Code  ",                                         ~
               at (04,22), fac(hex(84)), part$                  , ch(24),~
               at (04,47), fac(hex(8c)), partdescr$             , ch(34),~
               at (05,02),                                               ~
                  "Order Date",                                          ~
               at (05,22), fac(lfac$( 2%)), orddate$            , ch(08),~
               at (05,51),                                               ~
                  "Directive Status: ",                                  ~
               at (05,69), fac(hex(84)), holdreldesc$           , ch(12),~
               at (05,32),                                               ~
                  "Due Date",                                            ~
               at (05,41), fac(lfac$( 3%)), duedate$            , ch(08),~
               at (06,02), "Purchase for Job",                           ~
               at (06,22), fac(hex(8c)),    job$                , ch(08),~
               at (06,47), fac(hex(8c)),    jobdescr$           , ch(32),~
               at (07,02),                                               ~
                  "Vendor",                                              ~
               at (07,22), fac(lfac$( 4%)), vencode$            , ch(09),~
               at (07,47), fac(hex(8c)),   vencodedescr$        , ch(32),~
               at (08,02),                                               ~
                  "Vendor Part Number",                                  ~
               at (08,22), fac(lfac$( 5%)), venpart$            , ch(25),~
               at (08,48), fac(hex(8c)),   curr_msg$            , ch(13),~
               at (08,62), fac(lfac$( 6%)),currency$            , ch( 4),~
               at (09,02),                                               ~
                  "Vendor Shipping Units",                               ~
               at (09,29), fac(lfac$( 7%)), meas$               , ch( 4),~
               at (09,41), "Purchase Contract:",                         ~
               at (09,60), fac(lfac$(13%)), contract$           , ch(16),~
               at (09,77), fac(lfac$(13%)), contract_line$      , ch(04),~
               at (10,02),                                               ~
                  "Quantity Per Vendor Unit",                            ~
               at (10,30), fac(lfac$( 8%)), factor$             , ch(10),~
               at (10,43), "  Internal",                                 ~
               at (11,02),                                               ~
                  "Quantity To Order",                                   ~
               at (11,30), fac(lfac$( 9%)), venorder$           , ch(10),~
               at (11,43), fac(lfac$( 9%)), qtyorder$           , ch(10),~
               at (12,02),                                               ~
                  "Price Per Unit",                                      ~
               at (12,30), fac(lfac$(10%)), venprice$           , ch(10),~
               at (12,43), fac(lfac$(10%)), ourprice$           , ch(10),~
               at (12,57), "Extension:",                                 ~
               at (12,68), fac(hex(84)),   ext$                 , ch(10),~
                                                                         ~
               at (13,02), "Inventory Cost",                             ~
               at (13,17), fac(hex(8c)),   curr_msg2$           , ch(21),~
               at (13,43), fac(lfac$(11%)), totalstdcost$       , ch(10),~
               at (13,57), fac(pjfac$(1%)), pj_msg$             , ch(12),~
               at (13,71), fac(pjfac$(2%)), pj_job$             , ch(02),~
                                                                         ~
               at (14,02), fac(hex(ac)),   header$              , ch(79),~
                                                                         ~
               at (15,02), fac(sfac$(l%+1%)), seq$(l%+1%)       , ch(04),~
               at (16,02), fac(sfac$(l%+2%)), seq$(l%+2%)       , ch(04),~
               at (17,02), fac(sfac$(l%+3%)), seq$(l%+3%)       , ch(04),~
               at (18,02), fac(sfac$(l%+4%)), seq$(l%+4%)       , ch(04),~
               at (19,02), fac(sfac$(l%+5%)), seq$(l%+5%)       , ch(04),~
               at (20,02), fac(sfac$(l%+6%)), seq$(l%+6%)       , ch(04),~
                                                                         ~
               at (15,11), fac(lfac$(l%+21%)), pipstat$(l%+1%)  , ch( 1),~
               at (16,11), fac(lfac$(l%+22%)), pipstat$(l%+2%)  , ch( 1),~
               at (17,11), fac(lfac$(l%+23%)), pipstat$(l%+3%)  , ch( 1),~
               at (18,11), fac(lfac$(l%+24%)), pipstat$(l%+4%)  , ch( 1),~
               at (19,11), fac(lfac$(l%+25%)), pipstat$(l%+5%)  , ch( 1),~
               at (20,11), fac(lfac$(l%+26%)), pipstat$(l%+6%)  , ch( 1),~
                                                                         ~
               at (15,14), fac(hex(84)),   pipqty$ (l%+1%)      , ch(10),~
               at (16,14), fac(hex(84)),   pipqty$ (l%+2%)      , ch(10),~
               at (17,14), fac(hex(84)),   pipqty$ (l%+3%)      , ch(10),~
               at (18,14), fac(hex(84)),   pipqty$ (l%+4%)      , ch(10),~
               at (19,14), fac(hex(84)),   pipqty$ (l%+5%)      , ch(10),~
               at (20,14), fac(hex(84)),   pipqty$ (l%+6%)      , ch(10),~
                                                                         ~
               at (15,27), fac(hex(8c)),   pipord$ (l%+1%)      , ch( 8),~
               at (16,27), fac(hex(8c)),   pipord$ (l%+2%)      , ch( 8),~
               at (17,27), fac(hex(8c)),   pipord$ (l%+3%)      , ch( 8),~
               at (18,27), fac(hex(8c)),   pipord$ (l%+4%)      , ch( 8),~
               at (19,27), fac(hex(8c)),   pipord$ (l%+5%)      , ch( 8),~
               at (20,27), fac(hex(8c)),   pipord$ (l%+6%)      , ch( 8),~
                                                                         ~
               at (15,37), fac(hex(8c)),   pipdue$ (l%+1%)      , ch( 8),~
               at (16,37), fac(hex(8c)),   pipdue$ (l%+2%)      , ch( 8),~
               at (17,37), fac(hex(8c)),   pipdue$ (l%+3%)      , ch( 8),~
               at (18,37), fac(hex(8c)),   pipdue$ (l%+4%)      , ch( 8),~
               at (19,37), fac(hex(8c)),   pipdue$ (l%+5%)      , ch( 8),~
               at (20,37), fac(hex(8c)),   pipdue$ (l%+6%)      , ch( 8),~
                                                                         ~
               at (15,47), fac(hex(8c)),   pipin$  (l%+1%)      , ch(19),~
               at (16,47), fac(hex(8c)),   pipin$  (l%+2%)      , ch(19),~
               at (17,47), fac(hex(8c)),   pipin$  (l%+3%)      , ch(19),~
               at (18,47), fac(hex(8c)),   pipin$  (l%+4%)      , ch(19),~
               at (19,47), fac(hex(8c)),   pipin$  (l%+5%)      , ch(19),~
               at (20,47), fac(hex(8c)),   pipin$  (l%+6%)      , ch(19),~
               at (21,47), fac(hex(84)),   continued$           , ch(11),~
                                                                         ~
               at (22,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), fac(hex(8c)), pf$(1%),                        ~
               at (24,02), fac(hex(8c)), pf$(2%),                        ~
                                                                         ~
               keys(pfk$),                                               ~
               key (keyhit%)

               if keyhit% <>  8% then L42400
                  tempvend$ = vencode$
                  call "VENDSPSB" (tempvend$, #3, #14)
                  goto L41450

L42400:        if keyhit% <> 14% then L42440
                  call "HNYPRCSB" (vencode$,part$,0%,#9,#4,#3,#5)
                  goto L41450

L42440:        if keyhit% <> 13% then L42480
                  call "MANUAL" ("PORELSUB")
                  goto L41450

L42480:        if keyhit% <> 15% then L42520
                  call "PRNTSCRN"
                  goto L41450

L42520:        close ws
               call "SCREEN" addr("C", 0%, "I", ii$(), cursor%())
               return

        set_pf1
            pf$(1%) = "(1)Start Ovr  (2)First (4)Prev (6)Down  "    &    ~
                      "          (10)Mult Releas (15)Print Scn"
            pf$(2%) = "(8)Vendor     (3)Last  (5)Next (7)Up (11"    &    ~
                      ")Kit List (14)Proc Histry (16)Save Data"
            pfk$ = hex(0102030405060708090a0bff0d0e0f1000)

            if pj_on% = 1% and pj_job$ = "PJ" then return
            str(pf$(2%),38%,12%) = " "  :  str(pfk$,11%,1%) = hex(ff)
            return

        REM *************************************************************~
            *       S T A N D A R D   D I S P L A Y   S C R E E N       *~
            *                                                           *~
            * For inquiry routines.                                     *~
            *************************************************************

        inquiry_screen
            lfac$(20%)=hex(ac)
            accept                                                       ~
                at (01,02), fac(hex(8c)), i$(1%)                , ch(79),~
                at (02,02), fac(hex(a4)), i$(2%)                , ch(79),~
                at (03,02), fac(lfac$( 1)), i$(3%)              , ch(79),~
                at (04,02), fac(lfac$( 2)), i$(4%)              , ch(79),~
                at (05,02), fac(lfac$( 3)), i$(5%)              , ch(79),~
                at (06,02), fac(lfac$( 4)), i$(6%)              , ch(79),~
                at (07,02), fac(lfac$( 5)), i$(7%)              , ch(79),~
                at (08,02), fac(lfac$( 6)), i$(8%)              , ch(79),~
                at (09,02), fac(lfac$( 7)), i$(9%)              , ch(79),~
                at (10,02), fac(lfac$( 8)), i$(10%)             , ch(79),~
                at (11,02), fac(lfac$( 9)), i$(11%)             , ch(79),~
                at (12,02), fac(lfac$(10)), i$(12%)             , ch(79),~
                at (13,02), fac(lfac$(11)), i$(13%)             , ch(79),~
                at (14,02), fac(lfac$(12)), i$(14%)             , ch(79),~
                at (15,02), fac(lfac$(13)), i$(15%)             , ch(79),~
                at (16,02), fac(lfac$(14)), i$(16%)             , ch(79),~
                at (17,02), fac(lfac$(15)), i$(17%)             , ch(79),~
                at (18,02), fac(lfac$(16)), i$(18%)             , ch(79),~
                at (19,02), fac(lfac$(17)), i$(19%)             , ch(79),~
                at (20,02), fac(lfac$(18)), i$(20%)             , ch(79),~
                at (21,02), fac(lfac$(19)), i$(21%)             , ch(79),~
                at (22,02), fac(lfac$(20)), i$(22%)             , ch(79),~
                at (23,02), fac( hex( 8c)), i$(23%)             , ch(79),~
                at (24,02), fac( hex( 8c)), i$(24%)             , ch(79),~
                                                                         ~
                keys(hex(000102030405060d0e0f10)), key(keyhit%)

                if keyhit% <> 1% then L43400
                   gosub startover
                   goto inquiry_screen

L43400:         if keyhit% <> 13% then L43440
                   call "MANUAL" ("PORELSUB")
                   goto inquiry_screen

L43440:         if keyhit% <> 15% then L43480
                   call "PRNTSCRN"
                   goto inquiry_screen

L43480:         if keyhit% <> 0% and keyhit% <> 3% and                   ~
                                               keyhit% <> 14% then return
                   close ws
                   call "SCREEN" addr("C", 0%, "I", ii$(), cursor%())
                   return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50105,         /* PART             */~
                                    L50240,         /* ORDER DATE       */~
                                    L50260,         /* DUE DATE         */~
                                    L50320,         /* VENDOR           */~
                                    L50480,         /* VENDOR PART      */~
                                    L50840,         /* Currency Code    */~
                                    L50490,         /* SHIPPING UNITS   */~
                                    L50535,         /* CONVERSION FACTOR*/~
                                    L50595,         /* QUANTITY         */~
                                    L50710,         /* UNIT PRICE       */~
                                    L50825,         /* INVENTORY COST   */~
                                    L51400,         /* Purchase Job     */~
                                    L51500,         /* Purchase Contract*/~
                                    L51000          /* B.O. STATUS      */
                 return

L50105: REM VALIDATION FOR PART
L50110:     call "DESCRIBE" (#4, part$, partdescr$, 1%, f1%(4))
               if f1%(4) <> 0% then L50170
            call "HNYGREF" (part$, #10, #4, f1%(4))
               if f1%(4) <> 0% then L50110
            call "GETCODE" (#4, part$, partdescr$, 1%, 0, f1%(4))
               if f1%(4) <> 0% then L50160
                if part$ = " " then L50160
                   partdescr$ = "(* * * NON STOCKED PART * * *)"
                   stkg$ = "EACH"
                   goto L50225
L50160:        errormsg$ = hex(00)
               return
L50170:     get #4, using L50175, stkg$, serial_no$, pj_lead$, type$,     ~
                                                    partclass$, costtype$
L50175:         FMT POS(74), CH(4), POS(131), CH(1), POS(170), CH(10),   ~
                    CH(3), POS(200), CH(3), POS(307), CH(1)
            if type$ > "000" then L50191
                errormsg$ = "Build To Option Part, Cannot Be Purchased"
                return
L50191:     pj_lead% = 0%
            if pj_lead$ <> " " then                                      ~
                 convert pj_lead$ to pj_lead%, data goto L50195

L50195:     if admin% = 1 then L50221
            search codes$() = str(partclass$,,3) to cursor%() step 3
                if cursor%(1%) = 0% then L50210
                if alt$((cursor%(1%)+2%)/3%) = " " then L50221 /* Main   */
                if altb%=0% then L50210   /* Not allowing 'Alt' Buyers  */
                if alt$((cursor%(1%)+2%)/3%) = "Y" then L50221
                     /* Fall through if buyer NOT 'alternate'          */
L50210:         errormsg$ = "Sorry, You Are Not Authorized To Create Purc~
        ~hase Directives For This Part."
                return
L50221:     if pj_on% = 1% then gosub purchase_job_check
L50225:     gosub L30000
            return

L50240: REM VALIDATION FOR ORDER DATE
            call "DATEOK" (orddate$, 0%, errormsg$)
                if errormsg$ <> " " then return
            if pj_on% <> 1% then return
            call "DATEOK" (orddate$, 0%, errormsg$)
                temp$ = orddate$
                call "DATUNFMT" (temp$)
            call "DATE" addr("G-", basedate$, temp$, ord%, err%)
                if err% <> 0%  then L50253
                if ord% < 0% or ord% > 489% then L50253
                     gosub purchase_job_check
                     return
L50253:     errormsg$ = "Order Date Is Not Within Planning Calendar"
            return

L50260: REM VALIDATION FOR DUE DATE
            call "DATEOK" (duedate$, 0%, errormsg$)
              if errormsg$ <> " " then return
                temp$ = duedate$
                call "DATUNFMT" (temp$)
            call "DATE" addr("G-", basedate$, temp$, due%, err%)
                if err% <> 0%  then L50305
                if due% < 0% or due% > 489% then L50305
                   return
L50305:     errormsg$ = "Due Date Is Not Within Planning Calendar"
            return

L50320: REM VALIDATION FOR VENDOR
            currvend$ = " "
            if vencode$<>" " then L50435
                call "VPRPSLCT" (part$,vencode$,venpart$,1%," ",#5,#4,#3,~
                                 #9)
                readke2$ = str(part$,,25%) & str(vencode$,,9%) &         ~
                                                      str(venpart$,,25%)
                call "READ100" (#5, readke2$, f1%(5%))
                ourprice = 0
                if f1%(5)=0 then L50435
                     testdate$ = orddate$
                     call "DATUNFMT" (testdate$)
                     if testdate$ = " " or testdate$ = blankdate$ then ~
                          testdate$ = date
                                                  /* (EWD0001) */ 
                    get #5, using L50385, venprice, meas$, factor,       ~
                                          nextprice, effdate$, expdate$, ~
                                          currency$
L50385:                  FMT POS(69), PD(15,5), POS(89), CH(4), PD(14,4),~
                             PD(14,4), CH(6), CH(6), POS(158), CH(4)
                     if effdate$ = " " or effdate$ = blankdate$ then L50415
                     if testdate$ < effdate$ or                          ~
                        testdate$ > expdate$ then L50415
                        venprice = nextprice
L50415:              if abs(factor) < .0001 or factor > 1000000          ~
                                                          then factor = 1
                     ourprice = round(venprice/factor, 7%)
                     gosub'040
                     goto L50474
L50435:     call "READ100" (#4, part$, f1%(4))
                if f1%(4) <> 0% then get #4 using L50445, main_ven$
L50445:             FMT POS(102), CH(9)
            vencodedescr$=hex(06) & "Choose Vendor or PF16 for Part Maste~
        ~r Vendor: " &  main_ven$
            call "GETCODE"(#3, vencode$, vencodedescr$, 1%, 1.3, f1%(3))
                if f1%(3) = 0% then vencode$ = main_ven$
            if vencode$ = " " then errormsg$ = "Vendor Not On File"
                if vencode$ = " " then L50478
L50474:     call "DESCRIBE" (#3, vencode$, vencodedescr$, 1%, f1%(3))
            if f1%(3%) = 1% then get #3 using L50476, currvend$
L50476:       FMT POS(528), CH(4)
            if curr_on_flag$ <> "Y" then currvend$ = " "
L50478:         return

L50480: REM VALIDATION FOR VENDOR PART
            return
L50490: REM VALIDATION FOR SHIPPING UNITS
            if uom% = 0% then return
            plowkey$ = "UOM      " & meas$
            selectmsg$ = hex(06) & "Unit of Measure"
            call "PLOWCODE" (#13, plowkey$, selectmsg$, 9%, 0.30, f1%(13))
            if f1%(13) = 1% then L50525
                errormsg$ = "Invalid Unit of Measure Code."  :  return
L50525:     meas$ = str(plowkey$,10,4)
            return
L50535: REM VALIDATION FOR CONVERSION FACTOR
            if factor$ <> " " then L50555
               temp = factor
               goto L50560
L50555:     convert factor$ to temp, data goto L50585
L50560:        temp = round(temp, 4)
            if temp <= 0 or temp > 1000000 then L50585
               factor = temp
               gosub'040
               return
L50585:     errormsg$ = "Invalid entry for conversion factor"
            return
L50595: REM VALIDATION FOR QUANTITY
            if venorder$ = " " then L50650
               convert venorder$ to temp, data goto L50635
                temp = round(temp, 7)
                if abs(venorder - temp) < .0001 then L50650
            if temp < 0 then L50635
               temp = round(temp*factor,4)
               goto L50655
L50635:     errormsg$ = "Invalid entry for vendor's order quantity"
            return

L50650:     convert qtyorder$ to temp, data goto L50695
L50655:        temp = round(temp,4)
                  if abs(qtyorder - temp) < .0001 then L50680
                     if temp < 0 then L50695

            qtyorder = temp
L50680:     gosub'040
            return

L50695:     errormsg$ = "Invalid entry for internal order quantity"
            return

L50710: REM VALIDATION FOR UNIT PRICE
            if venprice$ = " " then venprice$ = "0"
               convert venprice$ to temp, data goto L50750
                temp = round(temp, 4)
                if abs(venprice - temp) < .0001 then L50763
            if temp < 0 then L50750
               temp = round(temp/factor,7)
               goto L50770
L50750:     errormsg$ = "Invalid entry for vendor's price"
            return

L50763:     if ourprice$ = " " then ourprice$ = "0"
            convert ourprice$ to temp, data goto L50810
L50770:        temp = round(temp,7)
                  if abs(ourprice - temp) < .0000001 then L50795
                     if temp < 0 then L50810

            ourprice = temp
L50795:     gosub'040
            return

L50810:     errormsg$ = "Invalid entry for internal price"
            return

L50825: REM VALIDATION FOR INVENTORY COST
            return

L50840: REM VALIDATION FOR CURRENCY CODE
            if currency$ = " " then currency$ = currvend$
            if currency$ = " " then currency$ = stat$
            if curr_on_flag$ <> "Y" then return  /* Just to make sure */
                call "GETCODE" (#40, currency$, " ", 0%, .3, f1%(40%))
                if f1%(40%) <> 0% then return
                    if currency$ = " " then L50920
                        errormsg$ = "Currency code not found on file."
                        return
L50920:             errormsg$ = "Currency code cannot be blank."
                    return

L51000: REM *************************************************************~
            *                                                           *~
            *  HANDLE UPDATE OF INFORMATION, REACTING TO STATUS CODE    *~
            * RESPONSES                                                 *~
            *                                                           *~
            *************************************************************

            qtyorder = 0
            temp1$=hex(00999999990f)
            temp2$=hex(00999999990f)

            for i%=1% to maxlines%
                if pos(" RC"=pipstat$(i%))<>0% then L51180
                   if str(pipin$(i%),2%,1%)="W" and                      ~
                      pos(" RCSD"=pipstat$(i%))<>0% then L51180
                   errormsg$ = "Invalid Response: " &pipstat$(i%)
                   l% = max(0,min(i%-1, maxlines% - 6%))
                   i% = maxlines%
                   goto L51250
L51180:         if pipstat$(i%)<>"R" and pipstat$(i%)<>"D" and           ~
                   pipstat$(i%)<>"S" then L51250
                convert pipqty$(i%) to q, data goto L51250
                qtyorder = qtyorder + q
                temp$ = pipord$(i%):call "DATUNFMT" (temp$)
                if temp$ < temp1$ then temp1$ = temp$
                temp$ = pipdue$(i%):call "DATUNFMT" (temp$)
                if temp$ < temp2$ then temp2$ = temp$
L51250:     next i%

            if errormsg$<>" " then return
            if factor <> 0 then gosub'040
            if abs(qtyorder) < .0001 then return

        REM IF TEMP1$<DATE THEN TEMP1$=DATE
               if temp1$<>hex(00999999990f) then orddate$=temp1$
            if temp2$ < date then temp2$=date
               if temp2$<>hex(00999999990f) then duedate$=temp2$
            call "DATEOK" (orddate$, 0%, errormsg$)
            call "DATEOK" (duedate$, 0%, errormsg$)
               errormsg$=" "
            return

L51400: REM VALIDATION FOR PURCHASE JOB
            if pj_job$ = "  " or pj_job$ = "PJ" then return
                errormsg$ = "Enter 'PJ' for a 'Purchased Job' or leave Bl~
        ~ank."
            return

L51500: REM Validate Purchase Contract & Line
            if contract$ = " " and contract_line$ = " " then return
            if str(part$,,8%) = "ACTIVITY" then contract_type$ = "A"     ~
                                           else contract_type$ = "P"
            call "VPCPIKME" (vencode$,             /* Vendor           */~
                             contract$,            /* Contract ID      */~
                             contract_line$,       /* Contract Line    */~
                             contract_type$,       /* Contract ItemType*/~
                             part$,                /* Contract Item    */~
                             orddate$,             /* Order by Date    */~
                             #16,                  /* VPCMASTR File    */~
                             #3,                   /* VENDOR   File    */~
                             f1%(16%))             /* Return Status    */
            if f1%(16%) = 0% then                                        ~
               errormsg$="Contract ID must be blank or a valid Contract:"~
                        & " " & contract$
            return

        REM COMMON ALPHA CONVERT SECTION
            deffn'040
            if factor = 0 then factor = 1
            factor   = round(factor  , 4)

            qtyorder = round(qtyorder, 4)
            ourprice = round(ourprice, 7)

            venorder = round(qtyorder/factor, 7)
            venprice = round(ourprice*factor, 5)

            totcost  = round(qtyorder*ourprice, 2)
            vencost  = round(venorder*venprice, 2)

            call "CONVERT" (factor  , 0.4, factor$  )
            call "CONVERT" (qtyorder, 0.2, qtyorder$)
            call "CONVERT" (venorder, 0.7, venorder$)
            call "CONVERT" (ourprice, 2.7, ourprice$)
            call "CONVERT" (venprice, 2.5, venprice$)

            call "CONVERT" (totcost, -2.2, ext$)
                if abs(totcost - vencost) < .01 then return
            str(ext$,,1) = "*"
            return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1984, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            close #50
            call "FILEBGON" (#50)
            call "GETUFBS1" addr (#51, f51%)
                if f51% = 0% then L65190
            close #51
            call "FILEBGON" (#51)
L65190:     end

