        REM *************************************************************~
            *                                                           *~
            *  H   H  N   N  Y   Y  IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  H   H  NN  N  Y   Y    I    NN  N  P   P  U   U    T     *~
            *  HHHHH  N N N   YYY     I    N N N  PPPP   U   U    T     *~
            *  H   H  N  NN    Y      I    N  NN  P      U   U    T     *~
            *  H   H  N   N    Y    IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYINPUT - Add / Change Records (Parts) in the Inventory  *~
            *            Master File (HNYMASTR).  Provides Add/Change/  *~
            *            Delete capability to the Subordinate Part      *~
            *            files such as the Alternate Parts File         *~
            *            (HNYALTRS), and the Generic Part Cross-        *~
            *            Reference File (HNYGENER).                     *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/18/83 ! ORIGINAL                                 ! GLW *~
            * 07/25/83 ! ALLOW GENERIC PARTS- THEIR ALTERNATES    ! GLW *~
            *          !       ARE THE ALLOWABLE OPTIONS          !     *~
            * 09/14/83 ! IND STD GENERIC INSTEAD OF VPN W XREF    ! KAB *~
            * 10/24/83 ! ADDED INPUT OF OPTION LIST               ! HES *~
            * 12/06/83 ! NULL APPENDIX LINKAGE INCLUDED           ! KAB *~
            * 04/11/84 ! ADDED EDIT-ALL CAPABILITY TO EDIT MODE   ! GLW *~
            * 04/16/84 ! ADDED TO UNIT OF ISSUE VENPRICE INPUT    ! HES *~
            * 09/17/85 ! Connected Text sub-module.               ! ERN *~
            * 10/03/85 ! Format changes to VENDOR put in.         ! ERN *~
            * 11/05/85 ! Screen Format Changes, Allowed inquiry   ! LDJ *~
            *          !   on G/L accounts.                       !     *~
            * 11/21/85 ! Rounding on standard costs to 4 places   ! HES *~
            * 02/??/86 ! Added PF 32 to force PIP RESTORE on Data ! KAB *~
            *          !   Save.                                  !     *~
            * 03/10/86 ! Added subroutine CDANPOST in data save.  ! LDJ *~
            * 05/08/86 ! Stripped the Maintenance of the Vendor   ! LDJ *~
            *          !   Price Catalogue Out, screen changes,   !     *~
            *          !   miscellaneous cleanup.                 !     *~
            * 06/17/86 ! Removed prices.  Added Taxable flag,     ! ERN *~
            *          !   Sales Discounts Acct. Removed Approval !     *~
            *          !   Status byte.                           !     *~
            * 09/25/86 ! Removed Options Maintenance Logic        ! HES *~
            * 10/10/86 ! var. ATC.  Several inconveniences (PRR's)! KAB *~
            * 02/02/87 ! Miscellaneous Changes for Support of Lot ! KAB *~
            *          ! Tracking and Serial Number Tracking.     ! LDJ *~
            * 03/12/87 ! Fixed bug re new generic & full screen;  ! ERN *~
            *          ! Added input/edit messages.               !     *~
            * 04/01/87 ! STC Project- Expand record, add fields.  ! ERN *~
            *          !  Also did all lot of clean up.           !     *~
            * 09/01/87 ! Miscellaneous changes for ROP            ! LKM *~
            * 08/04/88 ! HNYMASTR conversion factor to 7 decimals.! JIM *~
            * 09/20/88 ! Added max. % & units over receipt.       ! JIM *~
            * 10/07/88 ! Misc Cleanup. Correct CLASS Description. ! JDH *~
            *          !   Disallow zero conversion factor.       !     *~
            * 10/24/88 ! Now neg. quantity allowed default correct! JDH *~
            * 02/16/89 ! Proj 7890206 Min SO Quantity & Increment.! JIM *~
            * 05/15/89 ! CMS2/CMSI Merge                          ! MJB *~
            *          !  - Removed ALL ROP Code.                 !     *~
            *          !  - Added ASKUSER to include TEXT in COPY.!     *~
            *          !  - Added Test for lot ' ' when trying to !     *~
            *          !    change lot track flag from N to Y.    !     *~
            * 08/16/89 ! Proj. 7880907 - over/under shipments     ! LAB *~
            * 11/02/89 ! ADDED COST TYPE 'B'                      ! LAB *~
            * 04/13/90 ! Fixed testing of Minimum Sales Order Qty.! JDH *~
            * 08/14/90 ! - Changed LEADTIME on screen to CH(3)    ! MJB *~
            *          ! - Correct FMT statement for HNYMASTR to  !     *~
            *          !   full 900 characters.                   !     *~
            *          ! - Added option to select from generic    !     *~
            * 08/24/90 ! Changed UOM conversion literal and GET   ! JDH *~
            *          !  from UOM Conversion file.  Moved DELETE !     *~
            *          !  of HNYMASTR closer to WRITE.            !     *~
            * 03/03/92 ! Fix delete of record when manage defaults! JDH *~
            * 04/20/92 ! PRR 12117.  GETCODE now shows total Desc.! JDH *~
            *          ! PRR 12237.  Hilite PF25 if text exists.  !     *~
            * 07/08/92 ! Added ABC Lock Flag Field to Screen 3    ! RJH *~
            *          !  for Cycle Count Project                 !     *~
            * 11/05/92 ! PRR 12029 - If Part Type defined in      ! RJH *~
            *          !  GENCODE, check for validity W/GETCODE.  !     *~
            * 11/09/92 ! PRR 12120 - Record Creation date, Modifd ! RJH *~
            *          !  date, Modified by added to record and   !     *~
            *          !  displayed on first screen.              !     *~
            *          ! PRR 11866 - ln 32412 & 32510 changed to  !     *~
            *          !  not delete record but to read & rewrite.!     *~
            *          ! PRR 12488 - Added Planning fields - Alloc!     *~
            *          !  ation Mthd & Demand Type to Scrn#3.     !     *~
            *          ! Fix misc. implied integer conversions.   !     *~
            * 11/11/92 ! Added call to ALLFREE.                   ! JIM *~
            * 11/11/92 ! Brought Core Deposit Module to R6.02.03. ! JIM *~
            * 11/11/92 ! Matrix GET, PUT applied to PIPMASTR.     ! JIM *~
            * 05/09/93 ! Correct typo on implied integer Ln 50580 ! RJH *~
            * 05/12/93 ! Added Count Rate (peices/hr) as a field. ! JDH *~
            * 09/29/93 ! PF2 Alternates is hilit if alts on file. ! JDH *~
            * 03/28/94 ! PRR 12826 - Added call to PUTPAREN.      ! MLJ *~
            *          ! PRR 13126 - Now unconditionally updates  !     *~
            *          !   MODDATE$ and MODBY$ at data save.      !     *~
            * 03/30/94 ! Added PFKey prompt and call to ECRINQSB. ! LDJ *~
            * 06/10/94 ! Added Additional Variable Fields for     ! LDJ *~
            *          ! Formula Calculation fields.              !     *~
            *          ! Changed '10X routines to NOT call SCREEN !     *~
            *          ! when in INPUT mode (speed up entry).     !     *~
            *          ! Added Default Formula Name field.        !     *~
            *          ! Corrected defaulting in of Overshipment %!     *~
            * 06/28/94 ! PRR 13244 Init last count date for copied! JDH *~
            *          !   parts.                                 !     *~
            * 06/28/94 ! Add PF9 to view to Precious Metal        ! RJH *~
            *          !   Surcharges on a PM Part.               !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim                                                              ~
            a_descrs$(19)20,             /* Account Descriptors        */~
            a_fnxref$19, a_line$19,      /* Acct/Field # X-Refs        */~
            acct$(19)12,                 /* General Ledger Accounts    */~
            acct_descr$(19)30,           /*  (in seq of record layout) */~
            allocdflt$1,                 /* Planning Allocation Default*/~
            altdesr$(100)34,             /* Alternate Part Descr.      */~
            altpart$(100)25,             /* Alternate Part #s          */~
            altrmsg$79,                  /* Generic Part Message       */~
            atch$10,                     /* ATC Horizon in days        */~
            bin$8,                       /* Bin Location               */~
            bclass$(2)3,                 /* Buyer/planner Classes      */~
            bcdesc$(2)32,                /* Buyer/planner Classes      */~
            category$4,                  /* Inventory Category code    */~
            categorydescr$32,            /* Description of Category    */~
            class$4, class_descr$30,     /* Free (User Definable) Field*/~
            cpart$25,                    /* Part Number for Copy       */~
            conversion$10,               /* Conversion Factor (P==>S)  */~
            costtype$1,                  /* Inventory Costing Method   */~
            costtypedescr$32,            /* Inventory Costing Method   */~
            count_rate$10,               /* Count Rate (peices/hr)     */~
            createdate$8,                /* Date Record was created    */~
            cursor%(2),                  /* Cursor Location            */~
            cyclecat$1,                  /* Cycle Count Category       */~
            cycledate$8,                 /* Last Count Date            */~
            cyclelock$1,                 /* ABC Class Locking Flag     */~
            date$8,                      /* System Calendar Date       */~
            demtypedflt$1,               /* Planning Demand Type Dflt  */~
            description$32,              /* Description                */~
            dflts$(6)150,                /* Defaults                   */~
            draw_ref$16, draw_size$2,    /* Drawing Reference & Size   */~
            ecrpfk$14,                   /* ECR Inquiry PFKey Prompt   */~
            edttran$80,                  /* Map to Field # from Cursor */~
            errormsg$79,                 /* Error message text string  */~
            fac$(25)1,                   /* Field attribute characters */~
            fac25$1,                     /* PF(25) FAC                 */~
            form_calc_flag$1,            /* Formula Calculations Used? */~
            formula_name$16,             /* Default Formula Name       */~
            formula_prompt$20,           /* Default Formula Name Prompt*/~
            gcplowkey$99,                /* GENCODE Plowkey            */~
            gendescr$30,                 /* Generic Part Designator Des*/~
            generic$16,                  /* Generic Part Designator    */~
            genfiller$59,                /* HNYGENER Record Filler     */~
            hdr$(1)79,                   /* PLOWCODE Argument          */~
            i$(24)80,                    /* Screen Image               */~
            incl(1), incl$(1)1,          /* PLOWCODE Argument          */~
            indicator$4,                 /* Special/obsolete Indicator */~
            infomsg$79, inpmessage$79,   /* Informative message text   */~
            junkkey$60,                  /* Used in PLOWNEXT Routine   */~
            lastpart$25,                 /* Last Part Number input.    */~
            leadtime$10, lt_msg$30,      /* Lead Time (Days)           */~
            line2$79,                    /* Screen Underline           */~
            llcode$4,                    /* Low Level Code             */~
            lottrack$1,                  /* Subject to Lot Tracking?   */~
            oldlottrack$1,               /* Saved LOTTRACK Flag        */~
            message$30,                  /* Message for Line Items scrn*/~
            minsoqty$10, minsoinc$10,    /* Min SO Quantity & Increment*/~
            misc$79,                     /* Miscellaneous Junk Variable*/~
            modby$3,                     /* Record Modified By         */~
            moddate$8,                   /* Record Modified Date       */~
            negqty$1,                    /* Can Pools Go Negative      */~
            onfileptype$3,               /* Part Type On File          */~
            ovrrecpct$8, ovrrecqty$10,   /* Max. %, units over receipt */~
            ovrshppct$8, ovrshpqty$10,   /* Max. %, units overshipment */~
            pansize$10,                  /* Mfg/Purchase Increment     */~
            part$25,                     /* Part Number being input.   */~
            partkey$60,                  /* Part Read key              */~
            part_type_chk$1,             /* GENCODE Part type Check Flg*/~
            pfkeys$(4)17,                /* PFKeys enabled in tabular  */~
            pfkey$32,                    /* PFKeys enabled             */~
            pfmsg$(3)79,                 /* PFKeys Prompts             */~
            pip%(490),                   /* Nul matrix for new PIP     */~
            pipstat$1,                   /* PIP Status                 */~
            plowkey$100,                 /* Key for PLOWCODE calls     */~
            pmoq$10,                     /* Minimum Order Quantity     */~
            pm_on$1,                     /* Precious Metal On Flag     */~
            pm_pfk$14,                   /* Precious Metal PF Message  */~
            potency$10,                  /* Standard Potency Factor    */~
            ptype$3,                     /* Part Type (numeric)        */~
            ptypedescr$32,               /* Type Description           */~
            readkey$50,                  /* Multi-purpose Read Key     */~
            safety$10,                   /* Safety Stock               */~
            serial$1,                    /* Serial Numbered Part       */~
            seqnr$8,                     /* Converted no. of Alternates*/~
            set$8,                       /* Cost Set ID                */~
            scr%(3,20), set%(255),       /* Enable arrays              */~
            prcunit$4,                   /* Sales Unit of Measure      */~
            prcunitdescr$32,             /* Sales Unit of Measure Descr*/~
            sopriority$1,                /* Sales Order Plan Priority  */~
            spart$25, sdescr$32,         /* Save part, Description     */~
            stkunit$4,                   /* Stock Unit of Measure      */~
            stkunitdescr$32,             /* Stock U-O-M Description    */~
            switchkey$20,                /* SYSFILE2 Read key          */~
            taxable$1,                   /* Part Taxable Flag          */~
            tdate$6,                     /* Todays date unformated     */~
            testget$(9)100,              /* Test string of HNYINPUT Rec*/~
            testput$(9)100,              /* Test string of HNYINPUT Rec*/~
            texta$(196,1)70,             /* Text Matrix for TXTINSUB   */~
            textid$4,                    /* X-ref to Part Text         */~
            textmsg$79,                  /* Message to TXTINSUB        */~
            tfac$(20,2)1,                /* Fac's for Tabular Screens  */~
            tttle$(4,2)55,               /* Titles for Tabular Screens */~
            txt$4,                       /* For testing TEXTID values  */~
            uom$1, uomconv$1,            /* On-file? (Y/N)             */~
            userid$3,                    /* Current User ID            */~
            vencode$9,                   /* Vendor Code                */~
            vendname$32,                 /* Vendor's Name (Internal)   */~
            vf_pf$2,                     /* Variable Fields PFKeys Flag*/~
            vff$(10)20,                  /* Formula Variables Temp Spac*/~
            vfv$(10)12,                  /* Formula Variables Perm Spac*/~
            vfs$200                      /* Variable Fields            */

        dim f2%(32),                     /* File Status Flags          */~
            fs%(32),                     /* File Status Flags          */~
            f1%(32),                     /* Record-on-file Flags       */~
            rslt$(32)20                  /* Returned from "OPENFILE"   */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            *  #1 ! SYSFILE2 ! System Default Information File          *~
            *  #2 ! HNYMASTR ! Inventory Master File                    *~
            *  #3 ! CATEGORY ! Inventory Category File                  *~
            *  #4 ! GLMAIN   ! General Ledger Main File                 *~
            *  #5 ! HNYALTRS ! Inventory Alternate Parts File           *~
            *  #6 ! VENDOR   ! Vendor Master File                       *~
            *  #7 ! HNYPROC  ! Procurement History File                 *~
            *  #8 ! VENPRICE ! Vendor Price Catalogue File              *~
            *  #9 ! HNYGENER ! Generic Part Cross Reference File        *~
            * #10 ! BOMFRMLA ! BOM Formula Calculations File            *~
            * #12 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #14 ! ENGMASTR ! Engineering Master Filer                 *~
            * #15 ! PIPIN    ! PIPIN File                               *~
            * #16 ! PIPOUT   ! PIPOUT File                              *~
            * #17 ! HNYQUAN  ! Inventory Quantities File                *~
            * #18 ! SFCUM2   ! Cumulative Forecast File                 *~
            * #19 ! TXTFILE  ! System Text File                         *~
            * #20 ! GENCODES ! General Codes File                       *~
            * #21 ! BOMMASTR ! Bill of Materials Master file            *~
            *************************************************************

            select #1,  "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            select  #2, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90, keylen = 4, dup,  ~
                                   key 3, keypos = 26, keylen = 32, dup

            select  #3, "CATEGORY",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 4

            select  #4, "GLMAIN",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 9

            select  #5,  "HNYALTRS",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 60,                                   ~
                         keypos = 1, keylen = 33

            select  #6,  "VENDOR",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 600,                                  ~
                         keypos = 1, keylen = 9,                         ~
                         alt key 1, keypos = 10, keylen = 30, dup

            select  #7, "HNYPROC",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 134,                                  ~
                         keypos =32, keylen = 40,                        ~
                         alternate key 1, keypos = 7, keylen = 65,       ~
                                   key 2, keypos = 1, keylen = 40, dup,  ~
                                   key 3, keypos =41, keylen = 31, dup

            select  #8, "VENPRICE",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 256,                                  ~
                         keypos = 10, keylen = 59,                       ~
                         alternate key 1, keypos = 1, keylen = 34, dup,  ~
                                   key 2, keypos =35, keylen = 34

            select  #9,  "HNYGENER",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 100,                                  ~
                         keypos = 17, keylen = 25,                       ~
                         alternate key 1, keypos = 1, keylen = 41

           select #10, "BOMFRMLA", varc, indexed,                        ~
                       recsize = 2024, keypos = 1   , keylen = 16

           select #12, "PIPMASTR", varc, indexed,                        ~
                       recsize = 2024, keypos = 2   , keylen = 25,       ~
                       alternate key 1, keypos = 1,   keylen = 26

            select #14, "ENGMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos =    1, keylen =  29                      ~

            select #15, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48

            select #16, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37

            select #17, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   650,                                 ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            select #18, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  2024,                                 ~
                        keypos =    1, keylen =  25

            select #19, "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos  = 1, keylen = 11

            select #20, "GENCODES",                                      ~
                        varc, indexed, recsize = 128,                    ~
                        keypos =    1,  keylen = 24

            select #21, "BOMMASTR",                                      ~
                        varc, indexed, recsize = 150,                    ~
                        keypos =   26,  keylen = 31,                     ~
                        alt key  1, keypos =    1, keylen = 56

        call "SHOSTAT"  ("Opening Files, One Moment Please")
            rslt$(1) = "REQUIRED"
            call "OPENCHCK" (#1 , fs%( 1%), f2%( 1%), 0%, rslt$( 1%))
            call "OPENCHCK" (#2 , fs%( 2%), f2%( 2%), 1%, rslt$( 2%))
            call "OPENCHCK" (#5 , fs%( 5%), f2%( 5%), 1%, rslt$( 5%))
            call "OPENCHCK" (#9 , fs%( 9%), f2%( 9%), 1%, rslt$( 9%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%), 1%, rslt$(12%))
            call "OPENCHCK" (#20, fs%(20%), f2%(20%), 0%, rslt$(20%))

            if fs%(1%) < 1% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * SETS DATES, TRANSLATION TABLES, ETC                       *~
            *************************************************************

            switchkey$ = "SWITCHS.BOM"
            call "READ100" (#1, switchkey$, f1%(1%))
            form_calc_flag$ = "N"
            if f1%(1%) = 0% then L09140
            get #1 using L09110, form_calc_flag$
L09110:     FMT POS(24), CH(01)
            if form_calc_flag$ = " " then form_calc_flag$ = "N"
            if form_calc_flag$ = "N" then formula_prompt$ = " "          ~
                else formula_prompt$ = "Default Formula Name"
L09140:     switchkey$ = "SWITCHS.COR"
            call "READ100" (#1, switchkey$, core_track%) /* SYSFILE2 */
            date$, tdate$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr ("ID", userid$)
            pg3% = 17% /* number of real items on page three */
                       /* here for easy changing             */
            str(line2$,62%) = "HNYINPUT: " & str(cms2v$,,8%)
            init(hex(01)) edttran$

            a_fnxref$ = hex(010203040513060708090a0b0c0d0e0f101112)
            a_line$   = hex(060708090a0c0e0f101112130e0f101112130b)
            a_descrs$( 1%) = "Purchasing Source"
            a_descrs$( 2%) = "Work-in-Process Source"
            a_descrs$( 3%) = "Inventory Assets"
            a_descrs$( 4%) = "Cost of Goods Sold"
            a_descrs$( 5%) = "Sales Distribution"
            a_descrs$( 6%) = "Inventory Adjustments"
            a_descrs$( 7%) = "First Variance"
            a_descrs$( 8%) = "Second Variance"
            a_descrs$( 9%) = "Third Variance"
            a_descrs$(10%) = "Fourth Variance"
            a_descrs$(11%) = "Fifth Variance"
            a_descrs$(12%) = "Sixth Variance"
            a_descrs$(13%) = "Seventh Variance"
            a_descrs$(14%) = "Eighth Variance"
            a_descrs$(15%) = "Ninth Variance"
            a_descrs$(16%) = "Tenth Variance"
            a_descrs$(17%) = "Eleventh Variance"
            a_descrs$(18%) = "Twelfth Variance"
            a_descrs$(19%) = "Sales Discounts"

            tttle$(1%,1%) = "(1)Start Over(2)Col 1(4)Line Above(16)Edit M~
        ~ode"
            tttle$(2%,1%) = "(1)Strt Over(2)First(3)Last(4)Prev(5)Next(6)~
        ~Down(7)Up"
            tttle$(2%,2%) = "(11)Insert (12)Delete (15)Print Screen  (16)~
        ~Edit Mode"
            tttle$(3%,1%) = "Supply Requested Items and Press (ENTER),  o~
        ~r PRESS"
            tttle$(3%,2%) = "(1)To Exit Insert Mode (2)Column One (4)Line~
        ~ Above"
            tttle$(4%,1%) = "Press (ENTER) to Delete Flashing Line, or"
            tttle$(4%,2%) = "Press (1) To Exit Delete Mode"

            pfkeys$(1%) = hex(000102040f10ffffffffffffffffffffff)
            pfkeys$(2%) = hex(00010203040506070b0c0f10ffffffffff)
            pfkeys$(3%) = hex(000102040fffffffffffffffffffffffff)
            pfkeys$(4%) = hex(00010fffffffffffffffffffffffffffff)

*        Check For Inventory Defaults
            part$ = all(hex(00))
            call "READ100" (#2, part$, dflts%)
            if dflts% = 1% then get #2, str(dflts$(),,900)

            gosub load_calendar

*        See if UOM and UOMCONV files in place; also Part Class, PartType
            uom$, uomconv$, part_type_chk$ = "N"
            readkey$ = "UOM"
            call "PLOWNEXT" (#20, readkey$, 9%, f1%(20%))
            if f1%(20%) = 1% then uom$ = "Y"

            readkey$ = "UOMCONV"
            call "PLOWNEXT" (#20, readkey$, 9%, f1%(20%))
            if f1%(20%) = 1% then uomconv$ = "Y"

            readkey$ = "PARTCLASS"
            call "PLOWNEXT" (#20, readkey$, 9%, class%)

            readkey$ = "PARTTYPE "
            call "PLOWNEXT" (#20, readkey$, 9%, f1%(20%))
            if f1%(20%) = 1% then   part_type_chk$ = "Y"

        gosub init_enables

*        See if this User is a big cheese or not
            call "CMSMACHK" ("HNY", fac$(1), fac$(2))
            if fac$(1%) = "Y" or fac$(2%) = "Y" then admin% = 1%

*        Check if Precious Metal Surcharge is on
            pm_on$ = "N"  :  pm_pfk$ = " "
            call "READ100" (#01, "SWITCHS.BCK", f1%(01%))
            if f1%(01%) = 1% then get #01 using L09980, pm_on$
L09980:         FMT POS(60), CH(1)
*        Set PF Key prompt for Precious Metal Surcharge
            if pm_on$ <> "Y" then L10000
                pm_pfk$ = "(9)PM Items"

L10000: REM *************************************************************~
            *            M A I N   I N P U T   R O U T I N E            *~
            * --------------------------------------------------------- *~
            * This enters all the header information for a part number. *~
            * If part is on file, go directly to Edit Mode.             *~
            *************************************************************

        inputmode
            description$, vencode$, vendname$, pmoq$, ptype$, generic$,  ~
               stkunit$, prcunit$, bclass$(), conversion$, costtype$,    ~
               category$, taxable$, indicator$, seqnr$, leadtime$,       ~
               categorydescr$, ptypedescr$, altpart$(), errormsg$,       ~
               altdesr$(), draw_size$, draw_ref$, acct$(), acct_descr$(),~
               costtypedescr$, gendescr$, safety$, pansize$, cyclecat$,  ~
               cycledate$, onfileptype$, sopriority$, atch$, altrmsg$,   ~
               stkunitdescr$, prcunitdescr$, bin$, bcdesc$(), lottrack$, ~
               negqty$, serial$, class$, class_descr$, potency$, vfs$,   ~
               lt_msg$, ovrrecpct$, ovrrecqty$, minsoqty$, minsoinc$,    ~
               oldlottrack$, infomsg$, ovrshppct$, ovrshpqty$, ecrpfk$,  ~
               createdate$, moddate$, modby$, testget$(), testput$(),    ~
               formula_name$, vff$(), vfv$()           = " "
            retrieved%, maxlines%, keyhit%, dfltsedit%, alts% = 0%
            textid$ = all(hex(ff))
            call "TXTFUTIL" (#19, f2%(19), "INTL", textid$)
            lastpart$ = part$
            editall%, proceed%, copy%, lastfieldnr%, highfield% = 0%
            mat pip% = zer
            call "ALLFREE"

*        Get the FIRST PAGE here.
            highfield% = 0%
            for fieldnr% = 1% to 14%
L10280:         gosub'051(fieldnr%)
L10290:         gosub'101(fieldnr%, 1%)
                     if keyhit%  =  1% then gosub startover
                     if keyhit%  = 16% then       exit_program
                     if keyhit% <>  4% then L10410
L10330:                   fieldnr% = max(1%, fieldnr% - 1%)
                          if fieldnr% = 1% then inputmode
                               gosub'051(fieldnr%)
                               call "ENABLSUB" ("SET", "HNYINPUT",       ~
                                                 scr%(), set%(), 1%,     ~
                                                 fieldnr%, 1%, enabled%)
                               if enabled% = 0% then L10330
                               goto L10290
L10410:              if keyhit%  =  5% and fieldnr% = 1% then L10280
                     if keyhit%  =  8% then gosub see_procurement_his
                     if keyhit%  =  7% and fieldnr% > 1% then proceed%=1%
                     if keyhit%  =  7% then L10470
                     if keyhit%  =  9% and fieldnr% = 1% then selectgen
                     if keyhit%  = 17% and fieldnr% = 1% then L10470
                     if keyhit% <>  0% then L10290
L10470:         gosub'151(fieldnr%)
                     if errormsg$ <> " " then L10290
                          highfield% = max(highfield%, fieldnr%)
            next fieldnr%

*        Now get the second page.
            highfield%, keyhit% = 0%
L10540:     for fieldnr% = 1% to 20%
                gosub'052(fieldnr%)
L10560:         gosub'102(fieldnr%, 1%)
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <>  4% then L10660
                          keyhit% = 0%
L10590:                   fieldnr% = max(1%, fieldnr% - 1%)
                          if fieldnr% = 1% then L10540
                               gosub'052(fieldnr%)
                               call "ENABLSUB" ("SET", "HNYINPUT",       ~
                                                 scr%(), set%(), 2%,     ~
                                                 fieldnr%, 1%, enabled%)
                               if enabled% = 0% then L10590 else L10560
L10660:              if keyhit%  =  8% then gosub see_procurement_his
                     if keyhit%  =  7% then proceed% = 1%
                     if keyhit%  =  7% then L10710
                     if keyhit%  = 16% then exit_program
                     if keyhit% <>  0% then       L10560
L10710:         gosub'152(fieldnr%)
                     if errormsg$ <> " " then L10560
                          highfield% = max(highfield%, fieldnr%)
            next fieldnr%

*        Now get the third page.
            highfield%, keyhit% = 0%
L10780:     for fieldnr% = 1% to pg3%
                gosub'053(fieldnr%)
L10800:         gosub'103(fieldnr%, 1%)
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <>  4% then L10900
                          keyhit% = 0%
L10830:                   fieldnr% = max(1%, fieldnr% - 1%)
                          if fieldnr% = 1% then L10780
                               gosub'053(fieldnr%)
                               call "ENABLSUB" ("SET", "HNYINPUT",       ~
                                                 scr%(), set%(), 3%,     ~
                                                 fieldnr%, 1%, enabled%)
                               if enabled% = 0% then L10830 else L10800
L10900:              if keyhit%  =  8% then gosub see_procurement_his
                     if keyhit%  =  7% then proceed% = 1%
                     if keyhit%  =  7% then L10950
                     if keyhit%  = 16% then       exit_program
                     if keyhit% <>  0% then       L10800
L10950:         gosub'153(fieldnr%)
                          if errormsg$ = " " then L10970
                             if skipfield = 0 then L10800
L10970:                   highfield% = max(highfield%, fieldnr%)
            next fieldnr%

*        Do Input for Variable Fields
            str(line2$,,60) = "This Part: " & part$
            call "VFINPSUB" ("HNYMASTR", "I", "Manage Parts Master File",~
                             str(line2$,,60), "NN", vfs$, keyhit%)
            if keyhit% = 1% then inputmode

*        Do Input for BOM Formulas Variable Fields
            if form_calc_flag$ <> "Y" then L11130
            str(line2$,,60%) = "This Part: " & part$
            call "VFINPSUB" ("FORMULA ", "I",                            ~
                             "Manage Part Formula Variables",            ~
                             str(line2$,,60%), "NN", str(vff$()), keyhit%)
            if keyhit% = 1% then inputmode
            gosub move_formula_var_in

L11130
*        Get any maintenance required for the Appendix file
            call "HNYAPPND" (3%, part$, u3%)
            message$ = " "
            goto editmode

        selectgen
            init(hex(00)) plowkey$
            call "PLOWCODE" (#9, plowkey$, " ", 16%, -1.25, f1%(9))
            if f1%(9) = 0 then inputmode
            part$ = str(plowkey$,17,25)
            if part$ = " " then inputmode   /* No Part Selected */
            gosub L30000

            goto editmode

        REM *************************************************************~
            *         E D I T   T H E   H E A D E R   P A G E S         *~
            *************************************************************

        editmode
            errormsg$, infomsg$ = " "
            proceed% = 0%

L12040:     lastfieldnr% = 2%
            gosub'101(0%, 2%)
                  if keyhit%  <> 6% then L12065    /* Edit all */
                     editall% = 1%
                     goto L12160
L12065:           if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       editaltrs
                  if keyhit%  =  3% then gosub core_tracking
                  if keyhit%  =  5% then       edit_page_two
                  if keyhit%  =  8% then gosub see_procurement_his
                  if keyhit%  =  9% then gosub view_add_pm_items
                  if keyhit%  = 10% then gosub call_stcmatrx
                  if keyhit%  = 11% then gosub view_ecr_info
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 32% then       datasave
                  if keyhit%  = 25% then gosub edit_text
                  if keyhit% <>  0% and keyhit% <> 29% then L12040
            fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 2% or fieldnr% > 14% then L12040
            if keyhit% <> 29% then L12160
                if admin% <> 1% then L12040
                call "ENABLSUB" ("MODIFY", "HNYINPUT", scr%(), set%(),   ~
                                                   1%, fieldnr%, 0%, 0%)
                goto L12040

L12160:     gosub'101(fieldnr%, 2%)
                  if keyhit%  = 1% then gosub startover
                  if enabled% = 0% and editall% = 0% then L12040
                  if keyhit% <> 0% then       L12160
            if editall% = 0% then lastfieldnr%, lf2% = fieldnr%          ~
                             else lf2% = 14%

            for fieldnr% = lastfieldnr% to lf2%     /* 14% */
                gosub'151(fieldnr%)
                     lastfieldnr% = fieldnr%
                     if errormsg$ <> " " then L12160
            next fieldnr%
            editall% = 0%
            goto L12040

        edit_page_two
L12240:     init(" ") errormsg$, infomsg$
L12245:     lastfieldnr% = 1%
            gosub'102(0%, 2%)
                  if keyhit%  <> 6 then L12270    /* EDIT ALL */
                     editall% = 1%
                     goto L12395
L12270:           if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       editaltrs
                  if keyhit%  =  3 then gosub core_tracking
                  if keyhit%  =  4% then       editmode
                  if keyhit%  =  5% then       edit_page_three
                  if keyhit%  =  5% then L12685
                  if keyhit%  =  8% then gosub see_procurement_his
                  if keyhit%  =  9% then gosub view_add_pm_items
                  if keyhit%  = 10% then gosub call_stcmatrx
                  if keyhit%  = 11% then gosub view_ecr_info
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 32% then       datasave
                  if keyhit%  = 25% then gosub edit_text
                  if keyhit% <>  0% and keyhit% <> 29% then edit_page_two
            fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% > 15% or fieldnr% = 9%          ~
                                                       then edit_page_two
            if fieldnr% < 9% then L12365
                fieldnr% = fieldnr% - 1%
                if cursor%(2) > 40% then fieldnr% = fieldnr% + 6%
L12365:     if keyhit% <> 29% then L12395
                if admin% <> 1% then edit_page_two
                call "ENABLSUB" ("MODIFY", "HNYINPUT", scr%(), set%(),   ~
                                                   2%, fieldnr%, 0%, 0%)
                goto L12240

L12395:     gosub'102(fieldnr%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if enabled% = 0% and editall% = 0% then edit_page_two
            if editall% = 0% then lastfieldnr%, lf2% = fieldnr%          ~
                             else lf2% = 19%

            for fieldnr% = lastfieldnr% to lf2%
                gosub'152(fieldnr%)
                     lastfieldnr% = fieldnr%
                     if errormsg$ <> " " then L12395
            next fieldnr%
            editall% = 0%
            goto L12245

        edit_page_three
L12470:     lastfieldnr% = 1%
            gosub'103(0%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  <> 6% then L12500
                     editall% = 1%
                     goto L12600
L12500:           if keyhit%  =  2% then       editaltrs
                  if keyhit%  =  3 then gosub core_tracking
                  if keyhit%  =  4% then       edit_page_two
                  if keyhit%  =  5% then       edit_appendix
                  if keyhit%  =  8% then gosub see_procurement_his
                  if keyhit%  =  9% then gosub view_add_pm_items
                  if keyhit%  = 10% then gosub call_stcmatrx
                  if keyhit%  = 11% then gosub view_ecr_info
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 32% then       datasave
                  if keyhit%  = 25% then gosub edit_text
                  if keyhit%  = 29% then L12560
                  if keyhit% <>  0% then       L12470
L12560:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > pg3% then L12470
            if keyhit% <> 29% then L12600
                if admin% <> 1% then L12470
                call "ENABLSUB" ("MODIFY", "HNYINPUT", scr%(), set%(),   ~
                                                   3%, fieldnr%, 0%, 0%)
                goto L12470

L12600:     gosub'103(fieldnr%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if enabled% =  0% and editall% = 0% then L12470
                  if keyhit% <>  0% then       L12600
            if editall% = 0% then lastfieldnr%, lf2% = fieldnr%          ~
                             else lf2% = pg3%

            for fieldnr% = lastfieldnr% to lf2%
                gosub'153(fieldnr%)
                     lastfieldnr% = fieldnr%
                     if errormsg$ = " " then L12660
                        if skipfield = 0 then L12600
L12660:     next fieldnr%
            editall% = 0%
            goto L12470

        edit_appendix
L12685:     call "HNYAPPND" (4%, part$, err%)
            if err% <> 99% then L12705
                if keyhit% = 4% then edit_page_three   /* Ignore call */
                if keyhit% = 5% then edit_vf
L12705:     if err%  = 1% then inputmode       /* Branch per pf key    */
            if err% =  4% then edit_page_three
            if err% =  5% then edit_vf
            if err% = 16% then datasave

        edit_vf
            if form_calc_flag$ = "Y" then vf_pf$ = "YY" else vf_pf$ = "YN"
            str(line2$,,60) = "This Part: " & part$
            call "VFINPSUB" ("HNYMASTR", "E", "Manage Parts Master File",~
                             str(line2$,,60), vf_pf$, vfs$, keyhit%)
            if keyhit% =  1% then inputmode
            if keyhit% =  4% then edit_appendix
            if keyhit% =  5% then edit_formula_vf
            if keyhit% = 16% then datasave
                             goto editmode

        edit_formula_vf
            str(line2$,,60) = "This Part: " & part$
            gosub move_formula_var_out
            call "VFINPSUB" ("FORMULA ", "E",                            ~
                             "Manage Part Formula Variables",            ~
                             str(line2$,,60), "YN", str(vff$()), keyhit%)
            if keyhit% =  1% then inputmode
            gosub move_formula_var_in
            if keyhit% =  4% then edit_vf
            if keyhit% = 16% then datasave
                             goto editmode

        call_stcmatrx
            set$ = " "
            call "STCMATRX" (part$, set$, #1, #2)
            return

        core_tracking
            call "CORXRFSB" (part$, 1%) /* Input/Edit current part only */
            return

        REM *************************************************************~
            *          E D I T   A L T E R N A T E   P A R T S          *~
            *-----------------------------------------------------------*~
            * Enters Alternate Parts and checks if on inventory master  *~
            * file.  Will *NOT* let operator enter alternate part that  *~
            * is not on the master file.  Blanks are not allowed in     *~
            * the part field.  A Part can not be entered as it's own    *~
            * alternate part.                                           *~
            *************************************************************

        editaltrs
            message$ = "ALTERNATE PARTS"
            infomsg$ = " "
            line%, currentline%, screenline% = 0

L13150:     gosub'114(0%)
                  if keyhit%  =  0% then       L13320
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then line% = 0%
                  if keyhit%  =  3% then line% = max(0%, maxlines%-15%)
                  if keyhit%  =  4% then line% = max(0%, line% - 15%)
                  if keyhit%  =  5% then line% = min(line% + 15%,        ~
                                                 max(0%, maxlines% - 20%))
                  if keyhit%  =  6% then line% = max(0%, line% - 1%)
                  if keyhit%  =  7% then line% = min(line% + 1%,         ~
                                                 max(0%, maxlines% - 20%))
                  if keyhit%  =  9% then       editmode
                  if keyhit%  = 11% then gosub insertmode
                  if keyhit%  = 12% then gosub deletemode
                  if keyhit%  = 16% then       editmode
                  goto L13150

L13320:     REM Now figure out which field he hit.
                screenline% = max(0%, cursor%(1) - 4%)
                if screenline%  =  0% then L13150
                currentline% = screenline% + line%
                if currentline% > maxlines% then L13150
                fieldnr% = val(str(edttran$,cursor%(2)))
                if fieldnr% = 0 then L13150         /* Bad field        */

L13400:         gosub'114(fieldnr%)      /* Now get field to modify    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  0% then L13400
                gosub'154(fieldnr%)
                      if errormsg$ <> " " then L13400
                goto L13150

        REM *************************************************************~
            *        C O L U M N   O N E ,   L I N E   A B O V E        *~
            *-----------------------------------------------------------*~
            * Column one key and line above key functions handled here. *~
            *************************************************************

        columnone
            c% = currentline%
            init(" ") altpart$(c%), altdesr$(c%), infomsg$, errormsg$
            return

        lineabove
            if currentline% = 1 then return
            c% = currentline%
            on fieldnr% gosub L13630      /* PART NUMBER                */
            return
L13630:                 altpart$(c%) = altpart$(c%-1)
                        altdesr$(c%) = altdesr$(c%-1): return

L13660: REM *************************************************************~
            *              I N S E R T   M O D E   C O D E              *~
            *-----------------------------------------------------------*~
            * Handles insertion of a line item the list of alternates.  *~
            *************************************************************

        insertmode
            if maxlines% = 100 then return   /* Array full, can't*/
            REM Otherwise, set CURRENTLINE%, SCREENLINE%, and copy right
                screenline% = max(0%, cursor%(1%) - 4%)
                if line% + screenline% < maxlines% then L13780
                   screenline% = maxlines% - line% /*To ins at end*/
L13780:         if screenline% <> 20 then L13810    /* Bottom of page  */
                   line% = line% + 1%
                   screenline% = screenline% - 1%
L13810:         currentline%, c% = screenline% + line%

            REM Copy all the elements up one
                if c% >= maxlines% then L13900
                for temp% = maxlines% to c%+1% step -1%
                    altpart$(temp%+1%) = altpart$(temp%)
                    altdesr$(temp%+1%) = altdesr$(temp%)
                    next temp%

L13900:         screenline% = screenline% + 1%
                c%, currentline% = currentline% + 1%

                init(" ") altpart$(c%), altdesr$(c%)

            REM Now input the line, enable cancel out option
                infomsg$ = " "
L13970:         for fieldnr% = 1% to 1%
L13980:             gosub'124(fieldnr%)
                          if keyhit%  =  1% then L14150     /* End insert*/
                          if keyhit% <>  2% then L14030
                                         gosub columnone
                                         goto L13970
L14030:                   if keyhit% <>  4% then L14060
                               gosub lineabove
                               goto L14070
L14060:                   if keyhit% <>  0% then L13980
L14070:             gosub'154(fieldnr%)
                          if errormsg$ <> " " then L13980
                    next fieldnr%

                maxlines% = maxlines% + 1%
                cursor%(1) = min(cursor%(1%) + 1%, 24%)
                goto L13660
L14140:
L14150:     REM This routine aborts insert mode and destroys screenline%
                c% = currentline%
                if currentline% <= maxlines% then gosub L14270

                temp% = maxlines% + 1%
                init(" ") altpart$(temp%), altdesr$(temp%), errormsg$,   ~
                          infomsg$

            if currentline% >= maxlines% and screenline% = 20            ~
               then line% = max(0%, line%- 1%)
            return

L14270:     for temp% = currentline% to maxlines%
                altpart$(temp%) = altpart$(temp%+1%)
                altdesr$(temp%) = altdesr$(temp%+1%)
            next temp%
            return

        REM *************************************************************~
            *              D E L E T E   M O D E   C O D E              *~
            *-----------------------------------------------------------*~
            * Deletes a line item from list of alternate parts.         *~
            *************************************************************

        deletemode
            if maxlines% = 0% then return
            screenline% = cursor%(1%) - 4%
            if screenline% < 1% then return
               currentline% = screenline% + line%
               if currentline% > maxlines% then return

L14460:     gosub'134(screenline%)
                  if keyhit%  =  1% then       return
                  if keyhit% <>  0% then       L14460

            c% = currentline%
            if currentline% < maxlines% then gosub L14140
                                         /* Actually delete line @C%   */
            temp% = maxlines%
            init(" ") altpart$(temp%), altdesr$(temp%), errormsg$,       ~
                      infomsg$
            maxlines% = maxlines% - 1%
            return

        REM *************************************************************~
            *               E D I T   T E X T                           *~
            * --------------------------------------------------------- *~
            * Allow editing of Part Text.                               *~
            *************************************************************
        edit_text
            textmsg$ = "Part " & part$ & ", '" & description$ & "'"
            call "TXTINSUB" (#19, f2%(19), "001", textmsg$, textid$,     ~
                                                            texta$())
            return

        deffn'040(txt$)   /* Highlight PF25 if text on file */
            fac25$ = hex(84) /* Highlight PF(25)Manage Text */
            if txt$ = hex(00000000) or txt$ = hex(ffffffff) or txt$ = " "~
                then fac25$ = hex(8c) /* Dim PF(25)Manage Text */
            return

        REM *************************************************************~
            *            W R I T E   D A T A   T O   F I L E            *~
            *-----------------------------------------------------------*~
            * Updates the Inventory Master & Supporting Part Files.     *~
            *************************************************************

        datasave
          if dfltsedit% = 0% then L19170
            savedefaults% = 0%
            if admin% <> 1% then L19240
L19100:         savedefaults% = 2%
                call "ASKUSER" (savedefaults%, "SAVE DEFAULTS ON FILE?", ~
                     "PRESS PF-16 TO SAVE Defaults on file", "- OR -",   ~
                     "RETURN to use for current session only.")
                if savedefaults% <> 16% and savedefaults% <> 0% then L19100
                if savedefaults%  =  0% then L19240

L19170
*        Delete previous data from files
            junkkey$ =  part$
            if dfltsedit% = 1% then junkkey$ = all(hex(00))
            call "DELETE" (#5, junkkey$, 25%)
            if keyhit% = 32% then call "DELETE" (#12, junkkey$, 25%)

L19240
*        Write updated info to files
            gosub L32000

*        Update screen display variables
            lastpart$ = part$
            if str(part$,,1) = hex(00) then lastpart$ = "Defaults"

*        ...and Return
            goto inputmode

        REM *************************************************************~
            * E N A B L E / D E F A U L T   S E C T I O N   1ST PAGE    *~
            *-----------------------------------------------------------*~
            * Grabs Defaults and Enables if necessary.                  *~
            *************************************************************

        deffn'051(fieldnr%)
            if highfield% >= fieldnr% then return
            on fieldnr%    gosub L20125,            /* Part number      */~
                                 L20165,            /* Description      */~
                                 L20195,            /* Vendor part no.  */~
                                 L20220,            /* Stocking U.O.M.  */~
                                 L20245,            /* Sales Unit of mea*/~
                                 L20270,            /* Conversion       */~
                                 L20345,            /* Category Code    */~
                                 L20385,            /* Vendor Code      */~
                                 L20425,            /* Leadtime         */~
                                 L20455,            /* Type             */~
                                 L20495,            /* MOQ              */~
                                 L20525,            /* Pan Size         */~
                                 L20560,            /* Min SO Quantity  */~
                                 L20600             /* Safety Stock     */
            return

L20125
*        Default for PART NUMBER
            if part$ = " " and keyhit% = 0% then return
                call "READ102" (#2, part$, f1%(2%))
                if f1%(2%) = 1% then L20150
                     part$ = " " : return
L20150:         get #2, part$
                return

L20165
*        Default for DESCRIPTION
            description$ = " "
            if dflts% = 1% then get str(dflts$()) using L20180,description$
L20180:         FMT XX(25), CH(32)
            return

L20195
*        Default for GENERIC DESIGNATOR
            if dflts% = 1% then get str(dflts$()) using L20205, generic$
L20205:         FMT POS(58), CH(16)
            return

L20220
*        Default for STOCKING UNIT OF MEASURE
            if dflts% = 1% then get str(dflts$()) using L20230, stkunit$
L20230:            FMT POS(74), CH(4)
            return

L20245
*        Default for sales unit of measure
            if dflts% = 1% then get str(dflts$()) using L20255, prcunit$
L20255:            FMT POS(78), CH(4)
            return

L20270
*        Default for UOM CONVERSION
            conversion$ = "1"
            if uomconv$ = "Y" then L20310
                if dflts% = 1% then                                      ~
                               get str(dflts$()) using L20295, conversion
L20295:              FMT POS(82), PD(14,7)
                     call "CONVERT" (conversion, -0.7, conversion$)
                     return
L20310:     readkey$ = "UOMCONV  " & str(prcunit$) & "-" & str(stkunit$)
            call "READ100" (#20, readkey$, f1%(20%))
            if f1%(20%) = 0% then return
                get #20 using L20330, conversion$
L20330:              FMT XX(24), CH(10)
                return

L20345
*        Default for CATEGORY CODE
            if dflts% = 1% then get str(dflts$()) using L20355, category$
L20355:         FMT POS(90), CH(4)
            if fs%(3%) = 0% then                                         ~
                  call "OPENCHCK" (#3, fs%(3%), f2%(3%), 0%, rslt$(3%))
            call "DESCRIBE" (#3, category$, categorydescr$, 1%, f1%(3%))
            return

L20385
*        Default for VENDOR CODE
            if dflts% = 1% then get str(dflts$()) using L20395, vencode$
L20395:         FMT POS(102), CH(9)
            if fs%(6%) = 0% then                                         ~
                call "OPENCHCK" (#6, fs%(6%), f2%(6%), 0%, rslt$(6%))
            call "DESCRIBE" (#6, vencode$, vendname$, 1%, f1%(6%))
            return

L20425
*        Default/enable for LEAD TIME
            leadtime$ = "30"
            if dflts% = 1% then get str(dflts$()) using L20440, leadtime$
L20440:         FMT POS(170), CH(10)
            return

L20455
*        Default/enable for TYPE
            ptype$ = "200"
            if dflts% = 1% then get str(dflts$()) using L20470, ptype$
L20470:         FMT POS(180), CH(3)
            infomsg$ ="PART TYPES 000 = GENERIC, 001-199 = NONPLAN,"  &  ~
                      " 200-499 = PURCH, 500-999 = MFG"
            return

L20495
*        Default/enable for MOQ
            pmoq$ = "100"
            if dflts% = 1% then get str(dflts$()) using L20510, pmoq$
L20510:            FMT POS(190), CH(10)
                return

L20525
*        Default/enable for PAN SIZE
            pansize = 0
            if dflts% = 1% then get str(dflts$()) using L20540, pansize
L20540:         FMT POS(326), PD(14,4)
            call "CONVERT" (pansize, -2.2, pansize$)
            return

L20560
*        Default/enable for MIN SO QUANTITY & MIN SO IN
            minsoqty = 0
            if dflts% = 1% then get str(dflts$()) using L20575, minsoqty
L20575:         FMT POS(706), PD(14,4)
            call "CONVERT" (minsoqty, -2.2, minsoqty$)
            minsoinc = 0
            if dflts% = 1% then get str(dflts$()) using L20591, minsoinc
L20591:         FMT POS(714), PD(14,4)
            call "CONVERT" (minsoinc, -2.2, minsoinc$)
            return

L20600
*        Default/enable for SAFETY STOCK
            safety = 0
            if dflts% = 1% then get str(dflts$()) using L20630, safety
L20630:         FMT POS(318), PD(14,4)
            call "CONVERT" (safety, -2.2, safety$)
            return

        REM *************************************************************~
            * E N A B L E / D E F A U L T   S E C T I O N   2ND PAGE    *~
            *-----------------------------------------------------------*~
            * Grabs Defaults and enables if necessary.                  *~
            *************************************************************

        deffn'052(fieldnr%)
            if highfield% >= fieldnr% then return

            if fieldnr% = 1% then gosub L21130      /* Cost Method      */~
                             else gosub L21200      /* Accounts         */
            return

L21130
*        Default for COST TYPE
            costtype$ = " "
            if dflts% = 1% then get str(dflts$()) using L21160, costtype$
L21160:         FMT POS(307), CH(1)
            infomsg$ = "VALID TYPES ARE: R, X, A, B, S, F, L, M, T, P,"
            infomsg$ = infomsg$ & " & Y."
            return

L21200
*        Default for ACCOUNT NUMBERS
            if dflts% = 0% then return
                a% = val(str(a_fnxref$,fieldnr%-1%,1),1)
                acct$(a%) = str(dflts$(), 326% + (a%*9%), 9)
                if fs%(4%) = 0% then                                     ~
                    call "OPENCHCK" (#4, fs%(4%), f2%(4%), 0%, rslt$(4%))
                call "DESCRIBE" (#4, acct$(a%), acct_descr$(a%), 0%, u3%)
                call "GLFMT" (acct$(a%))
                return

        REM *************************************************************~
            * E N A B L E / D E F A U L T   S E C T I O N   3RD PAGE    *~
            *-----------------------------------------------------------*~
            * Grabs defaults and enables if necessary                   *~
            *************************************************************

        deffn'053(fieldnr%)
            if highfield% >= fieldnr% then return
            on fieldnr%    gosub L22240,            /* Bin Number       */~
                                 L22290,            /* Buyer Class      */~
                                 L22340,            /* Planner Class    */~
                                 L22390,            /* Cycle Count Stuff*/~
                                 L22460,            /* Special/Obsolete */~
                                 L22510,            /* Class            */~
                                 L22560,            /* Taxable          */~
                                 L22610,            /* Lot Track        */~
                                 L22670,            /* Serial Number    */~
                                 L22730,            /* Negative Qty.    */~
                                 L22790,            /* Standard Potency */~
                                 L22860,            /* Drawing Size/Ref */~
                                 L22920,            /* Rec > Order %    */~
                                 L23040,            /* S.O. Priority    */~
                                 L23090,            /* ATC Horizon      */~
                                 L23160,            /* Overshipment %   */~
                                 L23242             /* Default Formula  */
                return

L22240
*        Default/enable for BIN NUMBER
            if dflts% = 1% then get str(dflts$()) using L22260, bin$
L22260:         FMT POS(155), CH(8)
            return

L22290
*        Default/enable for BUYER PART CLASS
            if dflts% = 1% then get str(dflts$()) using L22310, bclass$(1)
L22310:         FMT POS(200), CH(3)
            return

L22340
*        Default/enable for PLANNER PART CLASS
            if dflts% = 1% then get str(dflts$()) using L22360, bclass$(2)
L22360:         FMT POS(309), CH(3)
            return

L22390
*        Default/enable for CYCLE COUNT DATE, CATEGORY, LOCK & Rate
            if dflts% = 1% then get str(dflts$()) using L22420,           ~
                           cyclecat$, cycledate$, cyclelock$, count_rate
L22420:              FMT POS(111), CH(1), CH(6), CH(1), PD(14,4)
            call "DATEFMT" (cycledate$)
            call "CONVERT" (count_rate, -2.2, count_rate$)
            return

L22460
*        Default for SPECIAL/OBSOLETE FIELD
            if dflts% = 1% then get str(dflts$()) using L22480, indicator$
L22480:         FMT POS(166), CH(4)
            return

L22510
*        Default for CLASS
            if dflts% = 1% then get str(dflts$()) using L22530, class$
L22530:         FMT POS(133), CH(4)
            return

L22560
*        Default for TAXABLE
            if dflts% = 1% then get str(dflts$()) using L22580, taxable$
L22580:         FMT POS(127), CH(1)
            return

L22610
*        Default for LOT TRACK
            if dflts% = 1% then get str(dflts$()) using L22630, lottrack$
L22630:         FMT POS(130), CH(1)
            if lottrack$ = " " then lottrack$ = "N"
            return

L22670
*        Default for SERIAL NUMBER
            if dflts% = 1% then get str(dflts$()) using L22690, serial$
L22690:         FMT POS(131), CH(1)
            if serial$ = " " then serial$ = "N"
            return

L22730
*        Default for NEGATIVE QUANTITIES
            if dflts% = 1% then get str(dflts$()) using L22750, negqty$
L22750:         FMT POS(132), CH(1)
            if negqty$ = " " then negqty$ = "N"
            return

L22790
*        Default for STANDARD POTENCY
            if dflts% = 1% then get str(dflts$()) using L22810, potency
L22810:         FMT POS(203), PD(14,4)
            if potency = 0 then potency = 1
            call "CONVERT" (potency, -2.2, potency$)
            return

L22860
*        Default for DRAWING SIZE & REFERENCE
            if dflts% = 1% then get str(dflts$()) using L22890,           ~
                                                    draw_size$, draw_ref$
L22890:         FMT POS(137), CH(2), CH(16)
            return

L22920
*        Default for Max. Receipt over Order Percent & Units
            if dflts% = 1% then get str(dflts$()) using L22940, ovrrecpct,~
                ovrrecqty
L22940:         FMT POS(211), PD(14,4), PD(14,4)
            call "CONVERT" (ovrrecpct, -2.4, ovrrecpct$)
            call "CONVERT" (ovrrecqty, -2.2, ovrrecqty$)
            return

L23040
*        Default/enable for SO PLANNING PRIORITY
            if dflts% = 1% then get str(dflts$()) using L23060,           ~
                                    allocdflt$, demtypedflt$, sopriority$
L23060:         FMT POS(242), CH(1), CH(1), POS(334), CH(1)
            return

L23090
*        Default for ATC HORIZON
            atch% = 999%
            if dflts% = 1% then get str(dflts$()) using L23120, atch%
L23120:         FMT POS(128), BI(2)
            convert atch% to atch$, pic(##0)
            return

L23160
*        Default for Overshipment Percent & Units
            if dflts% = 1% then get str(dflts$()) using L23180, ovrshppct,~
                ovrshpqty
L23180:         FMT POS(722), PD(14,4), PD(14,4)
            call "CONVERT" (ovrshppct, -2.4, ovrshppct$)
            call "CONVERT" (ovrshpqty, -2.2, ovrshpqty$)
            return

L23242
*        Default for Default Formula Name
            if dflts% = 1% then get str(dflts$()) using L23248,           ~
                formula_name$
L23248:         FMT POS(858), CH(16)

        REM *************************************************************~
            *        I N P U T   M E S S A G E S  &  E N A B L E S      *~
            * --------------------------------------------------------- *~
            * Sets Input Messages.                                      *~
            *************************************************************

        deffn'050 (sn%, fn%, editall%)
            if editall% <> 1% then L28120
                inpmessage$ = "Enter changes and then press RETURN to" & ~
                              " edit data."
                return

L28120:     if fn% <> 0% then L28170
                inpmessage$ = "To Edit displayed values use PF-6.  Use" &~
                              " PF-32 to Save Data and Balance PIP."
                return

L28170
*        Define the Input Message for the Screen/Field indicated.
            if sn% = 1% then restore line = scrn1_msg, fn%
            if sn% = 2% then restore line = scrn2_msg, fn%
            if sn% = 3% then restore line = scrn3_msg, fn%
            read inpmessage$             /* Read Input Message         */
            return

        scrn1_msg : data                                                 ~
         "Enter Part Number.  Leave blank to list Part Numbers on file.",~
         "Enter Part Description.                                      ",~
         "Enter Generic Part Designator (may be left blank).           ",~
         "Enter Stocking Unit of Measure Code.                         ",~
         "Enter Pricing Unit of Measure Code.                          ",~
         "Enter how many, in Stocking UOM, there are in one Pricing UOM",~
         "Enter Part Category code.                                    ",~
         "Enter Vendor Code of main supplier (may be left blank).      ",~
         "Enter Standard Purchasing Leadtime in number of days.        ",~
         "Enter the Part Type code for this Part.                      ",~
         "Enter Minimum Order Quantity (in Stocking Unit of Measure).  ",~
         "Enter Manufacturing/Purchasing Increment Size.               ",~
         "Enter the Minimum Sales Quantity & Sales Order Increment.    ",~
         "Enter the Safety Stock level for this Part."

        scrn2_msg : data                                                 ~
         "Enter the Costing Method to be used for this Part.           ",~
         "Enter the Purchasing Source Account Number.                  ",~
         "Enter the Work-in-Process Source Account Number.             ",~
         "Enter the Inventory Assets Account Number.                   ",~
         "Enter the Cost of Goods Sold Account Number.                 ",~
         "Enter the Sales Distribution Account Number.                 ",~
         "Enter the Sales Discount Account Number.                     ",~
         "Enter the Inventory Adjustments Account Number.              ",~
         "Enter the Variance Account for the First Cost Bucket.        ",~
         "Enter the Variance Account for the Second Cost Bucket.       ",~
         "Enter the Variance Account for the Third Cost Bucket.        ",~
         "Enter the Variance Account for the Fourth Cost Bucket.       ",~
         "Enter the Variance Account for the Fifth Cost Bucket.        ",~
         "Enter the Variance Account for the Sixth Cost Bucket.        ",~
         "Enter the Variance Account for the Seventh Cost Bucket.      ",~
         "Enter the Variance Account for the Eighth Cost Bucket.       ",~
         "Enter the Variance Account for the Ninth Cost Bucket.        ",~
         "Enter the Variance Account for the Tenth Cost Bucket.        ",~
         "Enter the Variance Account for the Eleventh Cost Bucket.     ",~
         "Enter the Variance Account for the Twelfth Cost Bucket.      "

        scrn3_msg : data                                                 ~
         "Enter the Primary Bin Location for this Part.                ",~
         "Enter the Buyer Part Class code for this Part.               ",~
         "Enter the Production Scheduler Class code for this Part.     ",~
         "Enter ABC Class (A, B, C, D, X, blank), Last Count Date, Lock(Y~
        ~/N) & Count Rate",                                               ~
         "Enter 'SPEC' to indicate special or 'OBSO' if obsoleted part.",~
         "Enter your Part Class code for this part.                    ",~
         "'Y' = always taxed, 'N' = never taxed, ' ' = per Customer.   ",~
         "Enter 'Y' if this part is to be lot tracked, else 'N'.       ",~
         "Enter 'Y' if serial numbers are required for this Part.      ",~
         "Enter 'Y' if the system is to prevent negative inventory lots",~
         "Enter Default Potency Factor. (eg, .50 = 50%, etc...)  Does Not~
        ~ effect On Hand"                                                ,~
         "Enter the Drawing Size and Reference.                        ",~
         "Enter the maximum Receipt over Order Percent & Quantity.     ",~
         "Enter Priority(A-Z), Demand Type(1-2), Allocation(A,C,N,P,Z); B~
        ~lanks OK."                                                      ,~
         "Enter the Available to Commit Horizon (in number of days).   ",~
         "Enter the Overshipment Percent & Quantity.                   ",~
         "Enter the Default BOM Calculation Formula Name if appropriate"

        REM *************************************************************~
            * E N A B L E   S W I T C H   D E F A U L T S               *~
            *************************************************************

        init_enables
*        Define Screen, Field Cross Ref and Field Enable Settings.
            mat set% = con   : mat set% = (99%) * set%
            mat scr% = zer
            scr%(1%, 1%) =  1% : set%( 1%) = 13%   /* Part Code        */
            scr%(1%, 2%) =  2% : set%( 2%) =  2%   /* Part Descr       */
            scr%(1%, 3%) =  3% : set%( 3%) =  2%   /* Generic Part Des */
            scr%(1%, 4%) =  4% : set%( 4%) =  2%   /* UOM- Stocking    */
            scr%(1%, 5%) =  5% : set%( 5%) =  2%   /* UOM- Sales       */
            scr%(1%, 6%) =  6% : set%( 6%) =  2%   /* UOM Conv Factor  */
            scr%(1%, 7%) =  7% : set%( 7%) =  2%   /* Category Code    */
            scr%(1%, 8%) =  8% : set%( 8%) =  2%   /* Vendor Code      */
            scr%(1%, 9%) =  9% : set%( 9%) =  2%   /* Std Purch LT     */
            scr%(1%,10%) = 10% : set%(10%) =  2%   /* Part Type        */
            scr%(1%,11%) = 11% : set%(11%) =  2%   /* Min Order Qty    */
            scr%(1%,12%) = 12% : set%(12%) =  2%   /* Purch Incr       */
            scr%(1%,13%) = 54% : set%(54%) =  2%   /* Min Sales Qty    */
            scr%(1%,14%) = 13% : set%(13%) =  2%   /* Safety Stock     */

            scr%(2%, 1%) = 21% : set%(21%) =  2%   /* Cost Method      */
            scr%(2%, 2%) = 22% : set%(22%) =  2%   /* GL-Purch Source  */
            scr%(2%, 3%) = 36% : set%(36%) =  2%   /*   -WIP Source    */
            scr%(2%, 4%) = 23% : set%(23%) =  2%   /*   -Inventory     */
            scr%(2%, 5%) = 24% : set%(24%) =  2%   /*   -Cost of Sales */
            scr%(2%, 6%) = 25% : set%(25%) =  2%   /*   -Sales         */
            scr%(2%, 7%) = 35% : set%(35%) =  2%   /*   -Sales Discs   */
            scr%(2%, 8%) = 29% : set%(29%) =  2%   /*   -Adjustments   */
            scr%(2%, 9%) = 26% : set%(26%) =  2%   /*   - Variance  1  */
            scr%(2%,10%) = 27% : set%(27%) =  2%   /*   - Variance  2  */
            scr%(2%,11%) = 28% : set%(28%) =  2%   /*   - Variance  3  */
            scr%(2%,12%) = 42% : set%(42%) =  2%   /*   - Variance  4  */
            scr%(2%,13%) = 43% : set%(43%) =  2%   /*   - Variance  5  */
            scr%(2%,14%) = 44% : set%(44%) =  2%   /*   - Variance  6  */
            scr%(2%,15%) = 45% : set%(45%) =  2%   /*   - Variance  7  */
            scr%(2%,16%) = 46% : set%(46%) =  2%   /*   - Variance  8  */
            scr%(2%,17%) = 47% : set%(47%) =  2%   /*   - Variance  9  */
            scr%(2%,18%) = 48% : set%(48%) =  2%   /*   - Variance 10  */
            scr%(2%,19%) = 49% : set%(49%) =  2%   /*   - Variance 11  */
            scr%(2%,20%) = 50% : set%(50%) =  2%   /*   - Variance 12  */

            scr%(3%, 1%) = 30% : set%(30%) =  2%   /* Bin Location     */
            scr%(3%, 2%) = 31% : set%(31%) =  2%   /* Buyer Class Code */
            scr%(3%, 3%) = 32% : set%(32%) =  2%   /* Planner Class Cde*/
            scr%(3%, 4%) = 33% : set%(33%) =  2%   /* Cycle Count Ctgy */
            scr%(3%, 5%) = 34% : set%(34%) =  2%   /* Spcl/Obsolete    */
            scr%(3%, 6%) = 41% : set%(41%) =  2%   /* Class            */
            scr%(3%, 7%) = 16% : set%(16%) =  2%   /* Taxable          */
            scr%(3%, 8%) = 37% : set%(37%) =  2%   /* Lot Track        */
            scr%(3%, 9%) = 38% : set%(38%) =  2%   /* Serial Number    */
            scr%(3%,10%) = 39% : set%(39%) =  2%   /* Neg. Quantity    */
            scr%(3%,11%) = 40% : set%(40%) =  2%   /* Std. Potency     */
            scr%(3%,12%) = 51% : set%(51%) =  2%   /* Drawing Sz/Ref   */
            scr%(3%,13%) = 52% : set%(52%) =  2%   /* Rec > Order %    */
            scr%(3%,14%) = 14% : set%(14%) =  2%   /* SO Planng Prty   */
            scr%(3%,15%) = 15% : set%(15%) =  2%   /* ATC Horizon      */
            scr%(3%,16%) = 55% : set%(55%) =  2%   /* Overshipment     */
            scr%(3%,17%) = 56% : set%(56%) =  2%   /* Def Formula Name */

            call "ENABLSUB" ("INIT", "HNYINPUT", scr%(), set%(),         ~
                                                          0%, 0%, 0%, 0%)
            return

            /* Available, 56% - 255% */

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the user the ability to start over when he wants to *~
            * else return to the menu.  Notice that he has to push 2    *~
            * different buttons to start over--harder to screw it up.   *~
            *************************************************************

        startover
            keyhit1% = 2%  /* Put msg area on bottom of screen  */
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return
                if keyhit1% <> 0% then startover
                     return clear all
                     goto inputmode

L30000: REM *************************************************************~
            *            R E A D   D A T A   F R O M   F I L E          *~
            *-----------------------------------------------------------*~
            * Loads the part from the main file and formats the entries.*~
            * Also loads the Suppprting part files into arrays.         *~
            *************************************************************

            retrieved%, oldpartonfile%, temp%, maxlines%, dfltsedit% = 0%
            if keyhit% <> 17% then L30110
                part$      = all(hex(00)) /* Defaults */
                dfltsedit% = 1%
L30110:     call "READ100" (#2, part$, f1%(2%))
            if f1%(2%) = 0% then return

*        Part already on file
            print at (4,1,80); hex(84); "Part On File, Loading Data"
            oldpartonfile% = 1%

            plowkey$ = part$  /* Test if alts on file */
            call "PLOWNEXT" (#5, plowkey$, 25%, alts%)

            if copy% = 0% then retrieved% = 1%

            /* Capture record for later test for change */
            get #2, testget$()

            get #2, using L31020, description$, generic$, stkunit$,       ~
                        prcunit$, conversion, category$, llcode$,        ~
                        textid$,vencode$,cyclecat$,cycledate$,cyclelock$,~
                        count_rate, taxable$, atch%, lottrack$, serial$, ~
                        negqty$, class$, draw_size$, draw_ref$, bin$,    ~
                        indicator$, leadtime$, ptype$, pmoq$, bclass$(1),~
                        potency, ovrrecpct, ovrrecqty, createdate$,      ~
                        moddate$, modby$, allocdflt$, demtypedflt$,      ~
                        costtype$, bclass$(2), safety, pansize,          ~
                        sopriority$, acct$(), vfs$, minsoqty, minsoinc,  ~
                        ovrshppct, ovrshpqty, vfv$(), formula_name$

            if copy% = 0% then L30290
            cycledate$, moddate$, modby$ = " "
            createdate$ = tdate$
            if textid$ = " " then L30300
            if textid$ = hex(ffffffff) then L30300
            if textid$ = hex(00000000) then L30300
L30250:         ask% = 2%
                call "ASKUSER" (ask%, "***** COPY TEXT? *****",          ~
                     "Press PF-25 to INCLUDE Text in the Copy", "- or-", ~
                     "PF-16 to NOT Copy Text")
                if ask% = 25% then L30275
                if ask% <> 16% then L30250
                     init (hex(ff)) textid$ : goto L30300
L30275:         call "TXTFUTIL" (#19, f2%(19%), "INTL", " ")
                call "TXTFUTIL" (#19, f2%(19%), "COPY", textid$)
                goto L30300
L30290:     call "TXTFUTIL" (#19, f2%(19%), "LOAD", textid$) /*Load Text*/
*        Retrieve descriptions from main files
L30300:     if fs%(3%) = 0% then                                         ~
                     call "OPENCHCK" (#3, fs%(3%), f2%(3%), 0%, rslt$(3%))
            call "DESCRIBE" (#3, category$, categorydescr$, 1%, f1%(3%))
            if fs%(6%) = 0% then                                         ~
                     call "OPENCHCK" (#6, fs%(6%), f2%(6%), 0%, rslt$(6%))
            call "DESCRIBE" (#6, vencode$, vendname$, 1%, f1%(4%))
            readkey$ = "BYCLASSES" & bclass$(1%)
            call "DESCRIBE" (#20, readkey$, bcdesc$(1%), 1%, f1%(20%))
            readkey$ = "PSCLASSES" & bclass$(2%)
            call "DESCRIBE" (#20, readkey$, bcdesc$(2%), 1%, f1%(20%))
            readkey$ = "UOM      " & stkunit$
            call "DESCRIBE" (#20, readkey$, stkunitdescr$, 1%, f1%(20%))
            readkey$ = "UOM      " & prcunit$
            call "DESCRIBE" (#20, readkey$, prcunitdescr$, 1%, f1%(20%))
            call "DATEFMT" (cycledate$)
            readkey$ = "PARTCLASS" & class$
            call "DESCRIBE" (#20, readkey$, class_descr$, 1%, f1%(20%))

           oldlottrack$ = lottrack$
           ptype% = 0% : convert ptype$ to ptype%, data goto L30500
           convert ptype% to ptype$, pic(000)
L30500:    call "HNYTYPE" (ptype%, ptypedescr$, 1%)
           onfileptype$ = ptype$
           atch = atch%
           call "CONVERT" (atch   , -0.001, atch$)
           call "CONVERT" (potency,   -4.2, potency$)
           call "CONVERT" (ovrrecpct, -2.4, ovrrecpct$)
           call "CONVERT" (ovrrecqty, -2.2, ovrrecqty$)
           call "CONVERT" (ovrshpqty, -2.2, ovrshpqty$)
           call "CONVERT" (ovrshppct, -2.4, ovrshppct$)
           call "CONVERT" (count_rate, -2.2, count_rate$)

*         See if part has variable leadtimes.  Inform if so.
            if fs%(14%) = 0% then                                        ~
                call "OPENCHCK" (#14, fs%(14%), f2%(14%), 0%, rslt$(14%))
            call "READ100" (#14, str(part$,,25) & "2001", f1%(14%))
            if f1%(14%) = 1% then lt_msg$ = "Part has variable lead times"

*        Format numbers and Accounts for screen display
            pmoq = 0 : convert pmoq$ to pmoq, data goto L30640
L30640:     call "CONVERT" (pmoq, 2.2, pmoq$)
            call "CONVERT" (safety    , 2.2, safety$)
            call "CONVERT" (pansize   , 2.2, pansize$)
            call "CONVERT" (minsoqty, 2.2, minsoqty$)
            call "CONVERT" (minsoinc, 2.2, minsoinc$)
            call "CONVERT" (conversion, -0.7, conversion$)
            call "DATEFMT" (createdate$)
            call "DATEFMT" (moddate$)
            if fs%(4%) = 0% then                                         ~
                call "OPENCHCK" (#4, fs%(4%), f2%(4%), 0%, rslt$(4%))
            for a% = 1% to 19%
                call "DESCRIBE" (#4, acct$(a%), acct_descr$(a%), 0%, u3%)
                call "GLFMT"    (acct$(a%))
            next a%

            if costtype$ = " " then costtype$ = "R"
            gosub L34210

*        Get Generic Part Description
           call "PLOWALTS" (#9,str(generic$,,16) & hex(00),1%,16%,f1%(9%))
            if f1%(9%) = 0% then L30840
                get #9, using L30820, gendescr$
L30820:              FMT XX(41), CH(30)

L30840:     if copy% = 1% then return

                partkey$ = part$  /* Load Alternate part numbers */
                temp%    = 0%
L30880:         call "PLOWNEXT" (#5, partkey$, 25%, f1%(5%))
                if f1%(5%) = 0% then L30970
                     temp%, maxlines% = temp% + 1%
                     get #5, using L30920, altpart$(temp%)
L30920:                   FMT XX(33), CH(25)
                     call "DESCRIBE" (#2, altpart$(temp%),               ~
                                          altdesr$(temp%), 1%, f1%(2))
                     goto L30880

L30970:     call "HNYAPPND" (1%, part$, err%)
*        Set PF Key prompt for ECR Inquiry (if any ECRs)
            ecrpfk$ = "(11)"
            call "ECRINQSB" ("C",        /* "C" to Check for ECR Info  */~
                                         /*    and return PFKey Prompt */~
                             part$,      /* Part to Do Inquiry/Check on*/~
                             ecrpfk$,    /* IN:  PFKey # to Use        */~
                                         /* OUT: Formatted PFKey Prompt*/~
                                         /*    Will be BLANK if no ECRs*/~
                             #01,        /* SYSFILE2                   */~
                             #02)        /* HNYMASTR                   */

            return

*        Format for Inventory Master File (HNYMASTR)
L31020:     FMT XX(25),          /* Part Number                        */~
                CH(32),          /* Description                        */~
                CH(16),          /* Vendor Part Number                 */~
                2*CH(4),         /* Sales, Pricing UOMs                */~
                PD(14,7),        /* Conversion factor (P==>S)          */~
                CH(4),           /* Category code                      */~
                CH(4),           /* Lower Level Code                   */~
                CH(4),           /* Text ID X-Ref                      */~
                CH(9),           /* Vendor code                        */~
                CH(1),           /* Cycle count category               */~
                CH(6),           /* Last Count date                    */~
                CH(1),           /* ABC Lock Flag                      */~
                PD(14,4),        /* Cycle Count Rate (peices/hr)       */~
                CH(1),           /* Part Taxable Flag                  */~
                BI(2),           /* ATC Horizon days                   */~
                CH(1),           /* Lot Tracking Flag                  */~
                CH(1),           /* Serial Number Flag                 */~
                CH(1),           /* Negative Quantity Flag             */~
                CH(4),           /* Class                              */~
                CH(2), CH(16),   /* Drawing Size and Reference         */~
                CH(8),           /* Primary Bin Location               */~
                XX(3),           /* Filler                             */~
                CH(4),           /* Special/obsolete field             */~
                CH(10),          /* Lead time (days)                   */~
                CH(3), XX(7),    /* Type                               */~
                CH(10),          /* MOQ                                */~
                CH(3),           /* Buyer/planner 1                    */~
                PD(14,4),        /* Standard Potency                   */~
                2*PD(14,4),      /* Max. Receipt over Order % & units  */~
                CH(6),           /* Created Date                       */~
                CH(6),           /* Modified Date                      */~
                CH(3),           /* Last modified By                   */~
                CH(1),           /* Allocation Default Flag            */~
                CH(1),           /* Demand Type Default Flag           */~
                XX(63),          /* Filler                             */~
                CH(1),           /* Cost selection                     */~
                XX(1),           /* Filler                             */~
                CH(3),           /* Buyer/planner 2                    */~
                XX(6),           /* Filler                             */~
                PD(14,4),        /* Safety stock level                 */~
                PD(14,4),        /* MFG/PUR Increment (Pansize)        */~
                CH(1),           /* SO Priority assigned               */~
                19*CH(9),        /* Account Numbers                    */~
                CH(200),         /* Variable Fields                    */~
                2*PD(14,4),      /* Min SO Qty & Increment             */~
                2*PD(14,4),      /* Overshipment % & units             */~
                10*CH(12),       /* Formula Variable Fields            */~
                CH(16)           /* Default Formula Name               */

L32000: REM *************************************************************~
            *         S A V E   D A T A   O N   T H E   D I S K         *~
            *-----------------------------------------------------------*~
            * Writes the data onto the FILE(s). Note how the routine    *~
            * reverses the sequence in the Alternate file.  'To be an   *~
            * alternate of' is reflexive.                               *~
            *************************************************************


            if dfltsedit% = 0% then L32120
                part$ = all(hex(00))
                goto L32300

L32120
*        First Write Alternate Part Numbers
          if maxlines% = 0% then L32190
            for temp% = 1% to maxlines%
                convert temp% to seqnr$, pic(########)
                write #5 using L33470, part$, seqnr$, altpart$(temp%), " "
            next temp%

L32190
*        Next do Generic Xref
            genfiller$ = " "
            call "READ101" (#9, part$, f1%(9%))
            if f1%(9%) = 0% then L32260
                get #9 using L32240, genfiller$
L32240:              FMT XX(71),CH(29)
                delete #9
L32260:         if generic$ = " " then L32300
                write #9,using L32280,generic$,part$,gendescr$,genfiller$
L32280:              FMT CH(16), CH(25), CH(30), CH(29)

L32300
*        Convert Alphas to Numerics
            convert conversion$ to conversion
            convert safety$ to safety
            convert pansize$ to pansize
            convert atch$ to atch%
            convert minsoqty$ to minsoqty
            convert minsoinc$ to minsoinc

            call "DATUNFMT" (createdate$)

            for a% = 1% to 19%
                call "GLUNFMT" (acct$(a%))
            next a%
            call "DATUNFMT" (cycledate$)

            if leadtime$ = " " then leadtime$ = "0"
            if oldpartonfile% = 0% then createdate$ = tdate$
            moddate$ = tdate$
            modby$   = userid$

            junkkey$ = part$
            if dfltsedit% = 1% then junkkey$ = all(hex(00))
            if dfltsedit% = 1% and savedefaults% <> 16% then L32420
            call "READ101" (#2, junkkey$, f1%(2%))

L32420:     put #2 using L33070, part$, description$, generic$,           ~
                    stkunit$, prcunit$, conversion, category$, llcode$,  ~
                    textid$, vencode$, cyclecat$, cycledate$, cyclelock$,~
                    count_rate, taxable$, atch%, lottrack$, serial$,     ~
                    negqty$, class$, draw_size$, draw_ref$, bin$, " ",   ~
                    indicator$, leadtime$, ptype$, pmoq$, bclass$(1%),   ~
                    potency, ovrrecpct, ovrrecqty, createdate$,          ~
                    moddate$, modby$, allocdflt$, demtypedflt$, " ",     ~
                    costtype$, " ", bclass$(2%), " ", safety, pansize,   ~
                    sopriority$, acct$(), vfs$, minsoqty, minsoinc,      ~
                    ovrshppct, ovrshpqty, vfv$(), formula_name$, " "

            if dfltsedit% = 1% and savedefaults% <> 16% then L32570
                if f1%(2%) = 0% then write #2  else  rewrite #2
                call "TXTFUTIL" (#19, f2%(19%), "TOS2", textid$)
                call "HNYAPPND" (2%, part$, err%)
                if dfltsedit% = 1% then L32570
                if oldpartonfile% = 1% then call "CDANPOST" (#2, "C")    ~
                                       else call "CDANPOST" (#2, "A")
L32570:     if dfltsedit% = 1% then get #2, str(dflts$(),,900)
            if dfltsedit% = 1% then dflts% = 1%

*        We do NOT want to create a PIP for generic or nonplan parts
*        If STR(PTYPE$,1,3) < "200" THEN GOTO 31455 & Much Trouble
          if dfltsedit% = 1% then return
            call "READ101" (#12, part$, f1%(12))
            if f1%(12) = 0% then L32660
                if ptype$ = "000" or ptype$ > "199" then L32870
                delete #12 : return
L32660:     if ptype$ > "000" and ptype$ < "200" then return
                pipstat$ = " "
                ppmoq, pcqty = 0
                mat pip% = zer
                init (hex(00)) junkkey$ : str(junkkey$,,25) = part$
                if fs%(17) = 0% then                                     ~
                       call "OPENCHCK" (#17,fs%(17),f2%(17),0%,rslt$(17))
L32730:         call "PLOWNEXT" (#17, junkkey$, 25%, f1%(17))
                if f1%(17) = 0% then L32810
                     if str(junkkey$,26,1) < "0" then L32730
                     if str(junkkey$,26,1) > "9" then L32810
                          get #17 using L32780, q
L32780:                        FMT POS(69), PD(14,4)
                          pcqty = pcqty + q
                          goto L32730
L32810:         for i% = max(1%, today%-1%) to 490%
                     pip%(i%) = pcqty
                next i%
                gosub bal_pip
                goto L32890

L32870:         get #12 using L32880, pipstat$, pcqty
L32880:              FMT CH(1), POS(1987), PD(14,4)
                call "MXFL4GT" addr (#12, 26%, pip%(), 490%)
L32890:         convert pmoq$ to ppmoq, data goto L32910
                leadtime%, atch% = 0%
L32910:         convert leadtime$ to leadtime%, data goto L32920
L32920:         convert ptype$ to ptype%
                convert atch$ to atch%, data goto L32940
L32940:         atch% = atch% +                                          ~
                           (1000% * (max(0%, val(sopriority$, 1) - 64%)))
                put #12 using L32990, pipstat$, part$, pcqty, safety,     ~
                                     ppmoq, pansize, ptype%, leadtime%,  ~
                                     atch%
L32990:              FMT CH(1), CH(25), POS(1987), 4*PD(14,4), 3*BI(2)
                call "MXFL4PT" addr (#12, 26%, pip%(), 490%)
                if f1%(12%) = 0% then write #12 else rewrite #12
                if fs%(18%) = 0% then                                    ~
                   call "OPENCHCK" (#18, fs%(18%), f2%(18%),0%,rslt$(18%))
                call "PIPFLAGS" (part$, 1%, today%, 0, #12, #18)
                return

*        HNYMASTR format
L33070:     FMT CH(25),              /* Part Number                    */~
                CH(32),              /* Description                    */~
                CH(16),              /* Generic Part Number            */~
                2*CH(4),             /* Unit of measures               */~
                PD(14,7),            /* Conversion factor (P==>S)      */~
                CH(4),               /* Category code                  */~
                CH(4),               /* Lower Level Code               */~
                CH(4),               /* Text ID X-Ref                  */~
                CH(9),               /* Vendor code                    */~
                CH(1),               /* Cycle count category           */~
                CH(6),               /* Last count date                */~
                CH(1),               /* ABC Lock Flag                  */~
                PD(14,4),            /* Cycle Count Rate (peices/hr)   */~
                CH(1),               /* Taxable Flag                   */~
                BI(2),               /* ATC Horizon days               */~
                CH(1),               /* Lot Track Flag                 */~
                CH(1),               /* Serial Number Flag             */~
                CH(1),               /* Negative Quantity Flag         */~
                CH(4),               /* Class                          */~
                CH(2), CH(16),       /* Drawing Size and Reference     */~
                CH(8),               /* Primary Bun (sic) Location     */~
                CH(3),               /* Filler                         */~
                CH(4),               /* Special/obsolete field         */~
                CH(10),              /* Lead time                      */~
                CH(10),              /* Type(3) and Filler (7)         */~
                CH(10),              /* MOQ                            */~
                CH(3),               /* Buyer/planner 1                */~
                PD(14,4),            /* Standard Potency               */~
                2*PD(14,4),          /* Max. Receipt over Order % & qty*/~
                CH(6),               /* Created Date                   */~
                CH(6),               /* Modified Date                  */~
                CH(3),               /* Last modified By               */~
                CH(1),               /* Allocation Default Flag        */~
                CH(1),               /* Demand Type Default Flag       */~
                CH(63),              /* Filler                         */~
                CH(1),               /* Cost Type                      */~
                CH(1),               /* Filler                         */~
                CH(3),               /* Buyer/planner 2                */~
                CH(06),              /* Filler                         */~
                PD(14,4),            /* Safety stock                   */~
                PD(14,4),            /* Pan size                       */~
                CH(1),               /* SO Priority assigned           */~
                19*CH(9),            /* G/L Accounts                   */~
                CH(200),             /* Variable Fields                */~
                2*PD(14,4),          /* Min SO Qty & Increment         */~
                2*PD(14,4),          /* Overshipment % & units         */~
                10*CH(12),           /* Formula Variable Fields        */~
                CH(16),              /* Default Formula Name           */~
                CH(027)              /* Filler                         */

*        Format for Alternates File
L33470:     FMT CH(25),              /* Part Number                    */~
                CH(8),               /* Converted sequence no.         */~
                CH(25),              /* Alternate part number          */~
                CH(2)                /* Filler                         */

        bal_pip
            if fs%(15) = 0% then                                         ~
                       call "OPENCHCK" (#15,fs%(15),f2%(15),0%,rslt$(15))
            if fs%(16) = 0% then                                         ~
                       call "OPENCHCK" (#16,fs%(16),f2%(16),0%,rslt$(16))
            init (hex(00)) junkkey$
            str(junkkey$,,25) = str(part$,,25)
L33590:     call "PLOWALTS" (#15, junkkey$, 1%, 25%, f1%(15%))
            if f1%(15%) = 0% then L33680
                get #15 using L33620, u3%, q
L33620:              FMT XX(25), BI(4), XX(19), PD(14,4)
                for i% = u3% to 490%
                     pip%(i%) = pip%(i%) + q
                next i%
                goto L33590

L33680:     init (hex(00)) junkkey$
            str(junkkey$,,25) = str(part$,,25)
L33700:     call "PLOWALTS" (#16, junkkey$, 1%, 25%, f1%(16%))
            if f1%(16%) = 0% then return
                get #16 using L33730, u3%, q
L33730:              FMT XX(19), XX(25), BI(4), XX(8), PD(14,4)
                for i% = u3% to 490%
                     pip%(i%) = pip%(i%) - q
                next i%
                goto L33700

        rem**************************************************************~
            *          l o a d   c a l m a s t r    d a t a             *~
            *-----------------------------------------------------------*~
            * get planning calendar index for today.                    *~
            *************************************************************

        load_calendar
            call "PIPINDEX" (#1, " ", today%, err%)
            if err% = 0% then return

            call "ASKUSER" (0%, "***INVALID PLANNING CALENDAR***",       ~
            "Your Planning/Production Calendar is Missing or Invalid!",  ~
            "Please Press RETURN to Acknowledge this message & EXIT,",   ~
            "Then run the Procedure PROCCAL to correct the situation!")
            goto exit_program


        REM *************************************************************~
            *         D E S C R I B E   C O S T   T Y P E               *~
            *************************************************************

L34210:     costtypedescr$=" "
            if costtype$ ="R" then costtypedescr$="(Actual Value, LIFO)"
            if costtype$ ="X" then costtypedescr$="(Actual LIFO/Adj Act)"
            if costtype$ ="A" then costtypedescr$="(Average Cost)"
            if costtype$ ="B" then costtypedescr$="(Mod. Average Cost)"
            if costtype$ ="S" then costtypedescr$="(Standard LIFO)"
            if costtype$ ="F" then costtypedescr$="(Fixed Standard)"
            if costtype$ ="L" then costtypedescr$="(Last Cost)"
            if costtype$ ="M" then costtypedescr$="(Manual Cost)"
            if costtype$ ="T" then costtypedescr$="(Standard FIFO)"
            if costtype$ ="P" then costtypedescr$="(Actual Value, FIFO)"
            if costtype$ ="Y" then costtypedescr$="(Actual FIFO/Adj Act)"
            return

        REM *************************************************************~
            *      1 S T   P A G E   O F   I N P U T   S C R E E N      *~
            *-----------------------------------------------------------*~
            * Gets all the information on the first page of the display.*~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            gosub'050(1%, fieldnr%, editall%)
            if edit% = 1% then str(line2$,,61)= "Last Part: " & lastpart$~
                          else str(line2$,,61)= " "
            gosub set_pf1
            if fieldnr% = 0% then init(hex(86)) fac$()                   ~
                             else init(hex(8c)) fac$()

            if fieldnr% = 0% and editall% = 0% then L40230
                ff% = 2% : lf% = 15%
                if editall% = 0% then ff%, lf% = fieldnr%
                for field% = ff% to lf%
                     call "ENABLSUB" ("SET", "HNYINPUT", scr%(), set%(), ~
                                             1%, field%, edit%, enabled%)
                     if proceed% = 1% and fieldnr% > 2% then enabled% = 0%
                     if errormsg$ > " " then enabled% = 1%
                     if enabled% > 0% then L40125
                          if editall% = 0% then return else L40200

L40125:           on field%   gosub  L40215,        /* Part             */~
                                     L40215,        /* Description      */~
                                     L40215,        /* Generic Designtr */~
                                     L40210,        /* Stocking U.O.M.  */~
                                     L40210,        /* Sales unit of mea*/~
                                     L40220,        /* Conversion factor*/~
                                     L40215,        /* Category code    */~
                                     L40215,        /* Vendor code      */~
                                     L40220,        /* Leadtime         */~
                                     L40215,        /* Type             */~
                                     L40220,        /* MOQ              */~
                                     L40220,        /* Pan size         */~
                                     L40220,        /* Min SO Quantity  */~
                                     L40220         /* Safety stock     */
L40200:         next field%
                goto L40230
L40210:             fac$(field%) = hex(80) : return     /* Upper/Lower */
L40215:             fac$(field%) = hex(81) : return     /* Upper Only  */
L40220:             fac$(field%) = hex(82) : return     /* Numeric     */

L40230:     accept                                                       ~
               at (01,02), "MANAGE INVENTORY PART MASTER FILE",          ~
               at (01,66), "Today:", fac(hex(8c)), date$        , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(84)), infomsg$,  ch(79),              ~
               at (04,02), fac(hex(94)), errormsg$, ch(79),              ~
               at (05,02), "Part Number",                                ~
               at (05,30), fac(fac$( 1%)), part$                , ch(25),~
               at (06,02), "Part Description",                           ~
               at (06,30), fac(fac$( 2%)), description$         , ch(32),~
               at (07,02), "Generic Part Designation",                   ~
               at (07,30), fac(fac$( 3%)), generic$             , ch(16),~
               at (07,48), fac(hex(8c)), gendescr$              , ch(30),~
               at (08,02), "Unit of Measure: Stocking",                  ~
               at (08,30), fac(fac$( 4%)), stkunit$             , ch(04),~
               at (08,48), fac(hex(8c)) , stkunitdescr$         , ch(32),~
               at (09,02), "                 Pricing",                   ~
               at (09,30), fac(fac$( 5%)), prcunit$             , ch(04),~
               at (09,48), fac(hex(8c)) , prcunitdescr$         , ch(32),~
               at (10,02), "UOM Conversion Factor",                      ~
               at (10,48), "Number of",                                  ~
               at (10,58), fac(hex(8c)) , stkunit$              , ch(04),~
               at (10,63), "per 1",                                      ~
               at (10,69), fac(hex(8c)) , prcunit$              , ch(04),~
               at (10,30), fac(fac$( 6%)), conversion$          , ch(10),~
               at (11,02), "Category Code",                              ~
               at (11,30), fac(fac$( 7%)), category$            , ch(04),~
               at (11,48), fac(hex(8c)),  categorydescr$        , ch(32),~
               at (12,02), "Vendor Code",                                ~
               at (12,30), fac(fac$( 8%)), vencode$             , ch(9), ~
               at (12,48), fac(hex(8c)),  vendname$             , ch(32),~
               at (13,02), "Std Purchase Leadtime: Days",                ~
               at (13,30), fac(fac$( 9%)), leadtime$            , ch(03),~
               at (13,45), fac(hex(8c)) , lt_msg$               , ch(30),~
               at (14,02), "Part Type",                                  ~
               at (14,30), fac(fac$(10%)), ptype$               , ch(03),~
               at (14,48), fac(hex(8c)), ptypedescr$            , ch(32),~
               at (15,02), "Minimum Order Quantity",                     ~
               at (15,30), fac(fac$(11%)), pmoq$                , ch(10),~
               at (16,02), "Mfg/Pur Increment (Pansize)",                ~
               at (16,30), fac(fac$(12%)), pansize$             , ch(10),~
               at (16,58), "Created "    ,                               ~
               at (16,67), fac(hex(8c)) , createdate$           , ch(08),~
               at (17,02), "Min Sale Qty / SO Increment",                ~
               at (17,30), fac(fac$(13%)), minsoqty$            , ch(10),~
               at (17,44), fac(fac$(13%)), minsoinc$            , ch(10),~
               at (17,58), "Modified"    ,                               ~
               at (17,67), fac(hex(8c)) , moddate$              , ch(08),~
               at (18,02), "Safety Stock",                               ~
               at (18,30), fac(fac$(14%)), safety$              , ch(10),~
               at (18,58), "By      "   ,                                ~
               at (18,67), fac(hex(8c)) , modby$                , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg$(1)              , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg$(2)              , ch(79),~
               at (24,02), fac(hex(8c)), pfmsg$(3)              , ch(79),~
                     keys(pfkey$), key(keyhit%)
               errormsg$ = " "

            if keyhit% <> 13% then L40500
                call "MANUAL" ("HNYINPUT")
                goto L40230

L40500:     if keyhit% <> 14% then L40520
                gosub where_used
                goto L40230

L40520:     if keyhit% <> 15% then L40540
                call "PRNTSCRN"
                goto L40230

L40540:     if edit% = 2% or keyhit% <> 3% then L40620
                if fieldnr% <> 3% then L40230
                misc$ = hex(06) & "Select Part to Copy"
                call "GETCODE" (#2, cpart$, misc$, 0%, 3, f1%(2%))
                if f1%(2%) = 0% then L40230
                     savedfsw% = dfltsedit%
                     spart$ = part$ : sdescr$ = description$
                     part$  = cpart$
                     copy%  = 1%
                     gosub L30000
                     copy%  = 0%
                     dfltsedit% = savedfsw%
                     part$ = spart$ : description$ = sdescr$
                     if len(cpart$) = 1% then cpart$ = cpart$ & hex(00)
                     cpart$ = str(cpart$,,len(cpart$)-1) & hex(00)
                     return clear all
                     goto editmode

L40620:     if fieldnr% > 0% and edit% = 1% and editall% = 0% then return
            close ws
            call "SCREEN" addr("C", u%, "I", i$(), cursor%())
            return

        set_pf1
          if edit% = 2% then L40735
            pfmsg$(1%) = "(1)Start Over    (4)Previous Field      " &    ~
                        "  (7)Proceed to Edit   (13)Instructions"
            pfmsg$(2%) = "(3)COPY          (5)Next Part           " &    ~
                        "                       (15)Print Screen"
            pfmsg$(3%) = "(17)Manage Defaults                     " &    ~
                        "  (9)Select Generic    (16)Exit Program"
            pfkey$ = hex(01ff030405ff07ff09ffffff0d0e0f101100)
            if fieldnr%  > 2% then L40700
                str(pfmsg$(1%),18,18) = " " : str(pfkey$, 4,1) = hex(ff)
                str(pfmsg$(1%),43,18) = " " : str(pfkey$, 7,1) = hex(ff)
L40700:     if fieldnr%  = 1% then L40715
                str(pfmsg$(3%),43,18) = " " : str(pfkey$, 9,1) = hex(ff)
                str(pfmsg$(2%),18,18) = " " : str(pfkey$, 5,1) = hex(ff)
                str(pfmsg$(3%), 1,20) = " " : str(pfkey$,17,1) = hex(ff)
L40715:     if fieldnr%  = 3% then L40725
                str(pfmsg$(2%), 1, 8) = " " : str(pfkey$, 3,1) = hex(ff)
L40725:     return

L40735:   if fieldnr% > 0% or editall% = 1% then L40805
            pfmsg$(1%) = " (1)Start Over                          " &    ~
                        "     (10)See Std Costs (13)Instructions"
            pfmsg$(2%) = " (2)Alternate Parts (5)Next Scr         " &    ~
                        "     (14)Where Used    (15)Print Screen"
            pfmsg$(3%) = " (3)Core Tracking   (8)Procurement Histo" &    ~
                        "ry   (25)Part Text     (16/32)Data Save"
            str(pfmsg$(3%),59,1) = hex(8c) /* Hilight PF(25) */
            gosub'040(textid$)
            str(pfmsg$(3%),45,1) = fac25$
            pfkey$ = hex(010203ff0506ff08090a0bff0d0e0f1020191d00)
            if pm_pfk$ = " " then str(pfkey$,09%,1%) = hex(ff)
            if ecrpfk$ = " " then str(pfkey$,11%,1%) = hex(ff)
            if retrieved% = 1% and core_track% = 1% and dfltsedit% <> 1% ~
                then goto L40774
                str(pfmsg$(3%),2%,16%) = " " : str(pfkey$,3%,1%) = hex(ff)
L40774:     str(pfmsg$(2%),32%,14%) = ecrpfk$
            str(pfmsg$(1%),32%,14%) = pm_pfk$
            if dfltsedit% = 0% then L40795
                str(pfmsg$(2%), 2,18) = " " : str(pfkey$, 2,1) = hex(ff)
                str(pfmsg$(2%),32,29) = " " : str(pfkey$,14,1) = hex(ff)
                str(pfmsg$(3%),20,24) = " " : str(pfkey$, 8,1) = hex(ff)
L40795:     if alts% = 0% then str(pfmsg$(2%),1%,1%) = hex(8c)
            if alts% <> 1% then return
                str(pfmsg$(2%), 1%,1%) = hex(84)
                str(pfmsg$(2%),20%,1%) = hex(8c)
                return

L40805:     pfmsg$(1%) = "(1)Start Over                           " &    ~
                        "                       (13)Instructions"
            pfmsg$(2%) = "                                        " &    ~
                        "                       (15)Print Screen"
            pfmsg$(3%) = "                                        " &    ~
                        "                                       "
            pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
            return

        REM *************************************************************~
            * 2 N D   P A G E   O F   I N P U T   M A I N   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gets the second page of input for the screen. Prices,     *~
            * Costs, and that sort of thing.                            *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
            gosub'050(2%, fieldnr%, editall%)
            str(line2$,,61)= "This Part: " & part$
            gosub set_pf2
            if fieldnr% = 0% then init(hex(86)) fac$()                   ~
                             else init(hex(8c)) fac$()

            if fieldnr% = 0% and editall% = 0% then L41125
                ff% = 1% : lf% = 20%
                if editall% = 0% then ff%, lf% = fieldnr%
                for field% = ff% to lf%
                     call "ENABLSUB" ("SET", "HNYINPUT", scr%(), set%(), ~
                                             2%, field%, edit%, enabled%)
                     if proceed%  = 1%  then enabled% = 0%
                     if errormsg$ > " " then enabled% = 1%
                     if enabled%  > 0%  then fac$(field%) = hex(81)
                     if enabled%  > 0%  then L41115
                          if editall% = 0% then return
L41115:         next field%

L41125:     accept                                                       ~
               at (01,02), "MANAGE INVENTORY PART MASTER FILE",          ~
               at (01,66), "Today:", fac(hex(8c)), date$        , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(84)), infomsg$,  ch(79),              ~
               at (04,02), fac(hex(94)), errormsg$, ch(79),              ~
                                                                         ~
               at (05,02), "Cost Method",                                ~
               at (05,30), fac(fac$( 1%)), costtype$            , ch(01),~
               at (05,49), fac(hex(8c)) , costtypedescr$        , ch(32),~
                                                                         ~
               at (06,02), "Source Account- Purchasing",                 ~
               at (06,30), fac(fac$( 2%)), acct$( 1%)           , ch(12),~
               at (06,49), fac(hex(8c)),  acct_descr$( 1%)      , ch(32),~
                                                                         ~
               at (07,02), "Source Account- WIP",                        ~
               at (07,30), fac(fac$( 3%)), acct$( 2%)           , ch(12),~
               at (07,49), fac(hex(8c)),  acct_descr$( 2%)      , ch(32),~
                                                                         ~
               at (08,02), "Inventory Assets Account",                   ~
               at (08,30), fac(fac$( 4%)), acct$( 3%)           , ch(12),~
               at (08,49), fac(hex(8c)),  acct_descr$( 3%)      , ch(32),~
                                                                         ~
               at (09,02), "Cost of Sales Account",                      ~
               at (09,30), fac(fac$( 5%)), acct$( 4%)           , ch(12),~
               at (09,49), fac(hex(8c)),  acct_descr$( 4%)      , ch(32),~
                                                                         ~
               at (10,02), "Sales Distribution",                         ~
               at (10,30), fac(fac$( 6%)), acct$( 5%)           , ch(12),~
               at (10,49), fac(hex(8c)),  acct_descr$( 5%)      , ch(32),~
                                                                         ~
               at (11,02), "Sales Discounts",                            ~
               at (11,30), fac(fac$( 7%)), acct$(19%)           , ch(12),~
               at (11,49), fac(hex(8c)),  acct_descr$(19%)      , ch(32),~
                                                                         ~
               at (12,02), "Inventory Adjustments",                      ~
               at (12,30), fac(fac$( 8%)), acct$( 6%)           , ch(12),~
               at (12,49), fac(hex(8c)),  acct_descr$( 6%)      , ch(32),~
                                                                         ~
               at (13,02), "Inventory Variance Accounts:",               ~
               at (14,02), " 1", at(15,02), " 2",  at (16,02), " 3",     ~
               at (17,02), " 4", at(18,02), " 5",  at (19,02), " 6",     ~
               at (14,42), " 7", at(15,42), " 8",  at (16,42), " 9",     ~
               at (17,42), "10", at(18,42), "11",  at (19,42), "12",     ~
               at (14,05), fac(fac$( 9%)), acct$( 7%)           , ch(12),~
               at (15,05), fac(fac$(10%)), acct$( 8%)           , ch(12),~
               at (16,05), fac(fac$(11%)), acct$( 9%)           , ch(12),~
               at (17,05), fac(fac$(12%)), acct$(10%)           , ch(12),~
               at (18,05), fac(fac$(13%)), acct$(11%)           , ch(12),~
               at (19,05), fac(fac$(14%)), acct$(12%)           , ch(12),~
               at (14,45), fac(fac$(15%)), acct$(13%)           , ch(12),~
               at (15,45), fac(fac$(16%)), acct$(14%)           , ch(12),~
               at (16,45), fac(fac$(17%)), acct$(15%)           , ch(12),~
               at (17,45), fac(fac$(18%)), acct$(16%)           , ch(12),~
               at (18,45), fac(fac$(19%)), acct$(17%)           , ch(12),~
               at (19,45), fac(fac$(20%)), acct$(18%)           , ch(12),~
               at (14,18), fac(hex(8c)) , acct_descr$( 7%)      , ch(23),~
               at (15,18), fac(hex(8c)) , acct_descr$( 8%)      , ch(23),~
               at (16,18), fac(hex(8c)) , acct_descr$( 9%)      , ch(23),~
               at (17,18), fac(hex(8c)) , acct_descr$(10%)      , ch(23),~
               at (18,18), fac(hex(8c)) , acct_descr$(11%)      , ch(23),~
               at (19,18), fac(hex(8c)) , acct_descr$(12%)      , ch(23),~
               at (14,58), fac(hex(8c)) , acct_descr$(13%)      , ch(23),~
               at (15,58), fac(hex(8c)) , acct_descr$(14%)      , ch(23),~
               at (16,58), fac(hex(8c)) , acct_descr$(15%)      , ch(23),~
               at (17,58), fac(hex(8c)) , acct_descr$(16%)      , ch(23),~
               at (18,58), fac(hex(8c)) , acct_descr$(17%)      , ch(23),~
               at (19,58), fac(hex(8c)) , acct_descr$(18%)      , ch(23),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg$(1%)             , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg$(2%)             , ch(79),~
               at (24,02), fac(hex(8c)), pfmsg$(3%)             , ch(79),~
                     keys(pfkey$), key(keyhit%)
               errormsg$ = " "

            if keyhit% <> 13% then L41525
                call "MANUAL" ("HNYINPUT")
                goto L41125

L41525:     if keyhit% <> 14% then L41545
                gosub where_used
                goto L41125

L41545:     if keyhit% <> 15% then L41565
                call "PRNTSCRN"
                goto L41125

L41565:     if fieldnr% > 0% and edit% = 1% and editall% = 0% then return
            close ws
            call "SCREEN" addr("C", u%, "I", i$(), cursor%())
            return

        set_pf2
          if edit% = 2% then L41650
            pfmsg$(1%) = "(1)Start Over    (4)Previous Field      " &    ~
                        "  (7)Proceed to Edit   (13)Instructions"
            pfmsg$(2%) = "                                        " &    ~
                        "                       (15)Print Screen"
            pfmsg$(3%) = "                                        " &    ~
                        "                                       "
            pfkey$ = hex(01ffff04ffff07ffffffffff0dff0fffff00)
            if fieldnr%  > 1% then L41640
                str(pfmsg$(1%),18,18) = " " : str(pfkey$, 4,1) = hex(ff)
L41640:     return

L41650:   if fieldnr% > 0% or editall% = 1% then L41720
            pfmsg$(1%) = " (1)Start Over      (4)Prev Scr         " &    ~
                        "     (10)See Std Costs (13)Instructions"
            pfmsg$(2%) = " (2)Alternate Parts (5)Next Scr         " &    ~
                        "     (14)Where Used    (15)Print Screen"
            pfmsg$(3%) = " (3)Core Tracking   (8)Procurement Histo" &    ~
                        "ry   (25)Part Text     (16/32)Data Save"
            str(pfmsg$(3%),59,1) = hex(8c) /* Hilight PF(25) */
            gosub'040(textid$)
            str(pfmsg$(3%),45,1) = fac25$
            pfkey$ = hex(010203040506ff08090a0bff0d0e0f1020191d00)
            if pm_pfk$ = " " then str(pfkey$, 9%,1%) = hex(ff)
            if ecrpfk$ = " " then str(pfkey$,11%,1%) = hex(ff)
            if retrieved% = 1% and core_track% = 1% and dfltsedit% <> 1% ~
                then goto L41689
                str(pfmsg$(3%),2%,16%) = " " : str(pfkey$,3%,1%) = hex(ff)
L41689:     str(pfmsg$(2%),32%,14%) = ecrpfk$
            str(pfmsg$(1%),32%,14%) = pm_pfk$
            if dfltsedit% = 0% then L41710
                str(pfmsg$(2%), 2,18) = " " : str(pfkey$, 2,1) = hex(ff)
                str(pfmsg$(2%),32,29) = " " : str(pfkey$,14,1) = hex(ff)
                str(pfmsg$(3%),20,24) = " " : str(pfkey$, 8,1) = hex(ff)
L41710:     if alts% = 0% then str(pfmsg$(2%),1%,1%) = hex(8c)
            if alts% <> 1% then return
                str(pfmsg$(2%), 1%,1%) = hex(84)
                str(pfmsg$(2%),20%,1%) = hex(8c)
                return

L41720:     pfmsg$(1%) = "(1)Start Over                           " &    ~
                        "                       (13)Instructions"
            pfmsg$(2%) = "                                        " &    ~
                        "                       (15)Print Screen"
            pfmsg$(3%) = "                                        " &    ~
                        "                                       "
            pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
            return

        REM *************************************************************~
            * 3 R D   P A G E   O F   I N P U T   M A I N   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gets the third page of input for the screen.              *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
            gosub'050(3%, fieldnr%, editall%)
            str(line2$,,61)= "This Part: " & part$
            gosub set_pf3
            if fieldnr% = 0% then init(hex(86)) fac$()                   ~
                             else init(hex(8c)) fac$()

            if fieldnr% = 0% and editall% = 0% then L42420
                ff% = 1% : lf% = pg3%
                if editall% = 0% then ff%, lf% = fieldnr%
                for field% = ff% to lf%
                     call "ENABLSUB" ("SET", "HNYINPUT", scr%(), set%(), ~
                                             3%, field%, edit%, enabled%)
                     if proceed%  = 1%  then enabled% = 0%
                     if field% = 17% and form_calc_flag$ = "N" then      ~
                         enabled% = 0%
                     if errormsg$ > " " then enabled% = 1%
                     if enabled%  > 0%  then L42240
                          if editall% = 0% then return else L42360

L42240:              on field% gosub L42390,        /* Bin Location     */~
                                     L42390,        /* Buyer class      */~
                                     L42390,        /* Planner class    */~
                                     L42390,        /* Cycle count stuff*/~
                                     L42390,        /* Special/obsolete */~
                                     L42390,        /* Class            */~
                                     L42390,        /* Taxable          */~
                                     L42390,        /* Lot Track        */~
                                     L42390,        /* Serial Number    */~
                                     L42390,        /* Neg. Quantity    */~
                                     L42390,        /* Potency          */~
                                     L42380,        /* Drawing Size/Ref */~
                                     L42400,        /* Rec > Order %    */~
                                     L42390,        /* SO Priority      */~
                                     L42400,        /* ATC Horizon      */~
                                     L42400,        /* Overship percent */~
                                     L42390         /* Default Formula  */
L42360:     next field%
            goto L42420
L42380:           fac$(field%) = hex(80) : return       /* Upper/Lower */
L42390:           fac$(field%) = hex(81) : return       /* Upper Only  */
L42400:           fac$(field%) = hex(82) : return       /* Numeric     */

L42420:     accept                                                       ~
               at (01,02), "MANAGE INVENTORY PART MASTER FILE",          ~
               at (01,66), "Today:", fac(hex(8c)), date$        , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$, ch(79),              ~
                                                                         ~
               at (04,02), "Primary Bin Location",                       ~
               at (04,30), fac(fac$( 1%)), bin$                 , ch(08),~
               at (05,02), "Buyer Part Class Code",                      ~
               at (05,30), fac(fac$( 2%)), bclass$(1%)          , ch(03),~
               at (05,49), fac(hex(8c)) , bcdesc$(1%)           , ch(32),~
               at (06,02), "Production Scheduler Class",                 ~
               at (06,30), fac(fac$( 3%)), bclass$(2%)          , ch(03),~
               at (06,49), fac(hex(8c)) , bcdesc$(2%)           , ch(32),~
               at (07,02), "ABC Class/Date",                             ~
               at (07,30), fac(fac$( 4%)), cyclecat$            , ch( 1),~
               at (07,35), fac(fac$( 4%)), cycledate$           , ch( 8),~
               at (07,49), "ABC Lock"            ,                       ~
               at (07,58), fac(fac$( 4%)), cyclelock$           , ch( 1),~
               at (07,60), "Count Rate"          ,                       ~
               at (07,71), fac(fac$( 4%)), count_rate$          , ch(10),~
               at (08,02), "Special/Obsolete Indicator",                 ~
               at (08,30), fac(fac$( 5%)), indicator$           , ch(4), ~
               at (09,02), "Part Class (User Def.)",                     ~
               at (09,30), fac(fac$( 6%)), class$               , ch(04),~
               at (09,49), fac(hex(8c)) , class_descr$          , ch(30),~
               at (10,02), "Part Taxable? (Y/N/ )",                      ~
               at (10,30), fac(fac$( 7%)), taxable$             , ch(01),~
               at (11,02), "Lot Tracking Required?",                     ~
               at (11,30), fac(fac$( 8%)), lottrack$            , ch(01),~
               at (12,02), "Ser. Numbers Required?",                     ~
               at (12,30), fac(fac$( 9%)), serial$              , ch(01),~
               at (13,02), "Prevent Negative Lots?",                     ~
               at (13,30), fac(fac$(10%)), negqty$              , ch(01),~
               at (14,02), "Default Std Potency Factor",                 ~
               at (14,30), fac(fac$(11%)), potency$             , ch(10),~
               at (15,02), "Drawing Size & Reference",                   ~
               at (15,30), fac(fac$(12%)), draw_size$           , ch(02),~
               at (15,40), fac(fac$(12%)), draw_ref$            , ch(16),~
               at (16,02), "Max Receipt ovr Ord % /Unit",                ~
               at (16,30), fac(fac$(13%)), ovrrecpct$           , ch(08),~
               at (16,40), fac(fac$(13%)), ovrrecqty$           , ch(10),~
               at (17,02), "S.O. Planning, Priority",                    ~
               at (17,30), fac(fac$(14%)), sopriority$          , ch(01),~
               at (17,35), "Demand Type"            ,                    ~
               at (17,48), fac(fac$(14%)), demtypedflt$         , ch(01),~
               at (17,53), "Allocation Method"      ,                    ~
               at (17,72), fac(fac$(14%)), allocdflt$           , ch(01),~
               at (18,02), "ATC Horizon in Days",                        ~
               at (18,30), fac(fac$(15%)), atch$                , ch(10),~
               at (19,02), "Allow Overshipment % /Unit",                 ~
               at (19,30), fac(fac$(16%)), ovrshppct$           , ch(08),~
               at (19,40), fac(fac$(16%)), ovrshpqty$           , ch(10),~
               at (20,02), fac(hex(8c)),   formula_prompt$      , ch(20),~
               at (20,30), fac(fac$(17%)), formula_name$        , ch(16),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg$(1%)             , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg$(2%)             , ch(79),~
               at (24,02), fac(hex(8c)), pfmsg$(3%)             , ch(79),~
                                                                         ~
               keys(pfkey$),                                             ~
               key(keyhit%)

               errormsg$ = " "

               if keyhit% <> 13% then L43040
                  call "MANUAL" ("HNYINPUT")
                  goto L42420

L43040:        if keyhit% <> 14% then L43080
                  gosub where_used
                  goto L42420

L43080:        if keyhit% <> 15% then L43120
                  call "PRNTSCRN"
                  goto L42420

L43120:     if fieldnr% > 0% and edit% = 1% and editall% = 0% then return
                close ws
                call "SCREEN" addr("C", u%, "I", i$(), cursor%())
                return

        set_pf3
          if edit% = 2% then L43290
            pfmsg$(1%) = "(1)Start Over    (4)Previous Field      " &    ~
                        "  (7)Proceed to Edit   (13)Instructions"
            pfmsg$(2%) = "                                        " &    ~
                        "                       (15)Print Screen"
            pfmsg$(3%) = "                                        " &    ~
                        "                                       "
            pfkey$ = hex(01ffff04ffff07ffffffffff0dff0fffff00)
            if fieldnr%  > 1% then L43270
                str(pfmsg$(1%),18,18) = " " : str(pfkey$, 4,1) = hex(ff)
L43270:     return

L43290:   if fieldnr% > 0% or editall% = 1% then L43460
            pfmsg$(1%) = " (1)Start Over      (4)Prev Scr         " &    ~
                        "     (10)See Std Costs (13)Instructions"
            pfmsg$(2%) = " (2)Alternate Parts (5)Next Scr         " &    ~
                        "     (14)Where Used    (15)Print Screen"
            pfmsg$(3%) = " (3)Core Tracking   (8)Procurement Histo" &    ~
                        "ry   (25)Part Text     (16/32)Data Save"
            str(pfmsg$(3%),59,1) = hex(8c) /* Hilight PF(25) */
            gosub'040(textid$)
            str(pfmsg$(3%),45,1) = fac25$
            pfkey$ = hex(010203040506ff08090a0bff0d0e0f1020191d00)
            if pm_pfk$ = " " then str(pfkey$,09%,1%) = hex(ff)
            if ecrpfk$ = " " then str(pfkey$,11%,1%) = hex(ff)
            str(pfmsg$(2%),32%,14%) = ecrpfk$
            str(pfmsg$(1%),32%,14%) = pm_pfk$
            if retrieved% = 1% and core_track% = 1% and dfltsedit% <> 1% ~
                then goto L43400
                str(pfmsg$(3%),2%,16%) = " " : str(pfkey$,3%,1%) = hex(ff)
L43400:     if dfltsedit% = 0% then L43440
                str(pfmsg$(2%), 2,18) = " " : str(pfkey$, 2,1) = hex(ff)
                str(pfmsg$(2%),32,29) = " " : str(pfkey$,14,1) = hex(ff)
                str(pfmsg$(3%),20,24) = " " : str(pfkey$, 8,1) = hex(ff)
L43440:     if alts% = 0% then str(pfmsg$(2%),1%,1%) = hex(8c)
            if alts% <> 1% then return
                str(pfmsg$(2%), 1%,1%) = hex(84)
                str(pfmsg$(2%),20%,1%) = hex(8c)
                return

L43460:     pfmsg$(1%) = "(1)Start Over                           " &    ~
                        "                       (13)Instructions"
            pfmsg$(2%) = "                                        " &    ~
                        "                       (15)Print Screen"
            pfmsg$(3%) = "                                        " &    ~
                        "                                       "
            pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
            return

        where_used
            if fs%(21%) = 0% then                                        ~
                call "OPENCHCK" (#21, fs%(21%), f2%(21%), 0%, rslt$(21%))
            plowkey$ = str(part$,,25)
            init (hex(00)) str(plowkey$,26)
            call "PLOWALTS" (#21, plowkey$, 1%, 25%, f1%(21%))
            if f1%(21%) <> 0% then L45790
                errormsg$ = "Not found as a component of any Product" &  ~
                            " Structure (BOM)."
                return
L45790:     misc$ = hex(06) & "Part " & part$ & " is used in these BOM's"
            hdr$(1%) = "  Assembly Part Code        ID   Part Description"
            incl(1%) = 0
            plowkey$ = str(part$,,25)
            call "PLOWCODE" (#21, plowkey$, misc$, 8025%, 1.32, f1%(21), ~
                             hdr$(), 28, 0, incl(), incl$()," ","Y", #2)
            return

        REM *************************************************************~
            *  A L T E R N A T E   P A R T   I N P U T   S C R E E N S  *~
            *-----------------------------------------------------------*~
            * Screen code for input, edit, insert, and delete modes.    *~
            *************************************************************

        deffn'104(fieldnr%)
            screen% = 1%
            goto L46145

        deffn'114(fieldnr%)
            screen% = 2%
            if fieldnr% = 0% then init (hex(86)) tfac$()                 ~
                             else init (hex(8c)) tfac$()
            goto L46150

        deffn'124(fieldnr%)
            screen% = 3%
            goto L46145

        deffn'134(screenline%)
            screen% = 4%
            init(hex(8c)) tfac$()
            for temp% = 1% to 1%
                tfac$(screenline%, temp%) = hex(94)
            next temp%
            goto L46185

*        Set FAC's for input screens.
L46145:     init(hex(84)) tfac$()
L46150:     on fieldnr% gosub  L46170           /* PART NUMBER      */
            goto L46185

                    tfac$(screenline%, fieldnr%) = hex(80)  : return
L46170:             tfac$(screenline%, fieldnr%) = hex(81)  : return
                    tfac$(screenline%, fieldnr%) = hex(82)  : return

L46185:     accept                                                       ~
               at (01,02), fac(hex(8c)), tttle$(screen%, 1%)    , ch(54),~
               at (02,02), fac(hex(8c)), tttle$(screen%, 2%)    , ch(54),~
               at (01,56), fac(hex(84)), part$                  , ch(25),~
               at (02,56), fac(hex(84)), description$           , ch(25),~
               at (03,02), fac(hex(94)), errormsg$              , ch(63),~
               at (04,02), fac(hex(a4)), infomsg$               , ch(79),~
               at (05,02), fac(hex(84)), message$                       ,~
                                                                         ~
         at (05,20), fac(tfac$( 1%,1%)), altpart$(line%+  1%)   , ch(25),~
         at (06,20), fac(tfac$( 2%,1%)), altpart$(line%+  2%)   , ch(25),~
         at (07,20), fac(tfac$( 3%,1%)), altpart$(line%+  3%)   , ch(25),~
         at (08,20), fac(tfac$( 4%,1%)), altpart$(line%+  4%)   , ch(25),~
         at (09,20), fac(tfac$( 5%,1%)), altpart$(line%+  5%)   , ch(25),~
         at (10,20), fac(tfac$( 6%,1%)), altpart$(line%+  6%)   , ch(25),~
         at (11,20), fac(tfac$( 7%,1%)), altpart$(line%+  7%)   , ch(25),~
         at (12,20), fac(tfac$( 8%,1%)), altpart$(line%+  8%)   , ch(25),~
         at (13,20), fac(tfac$( 9%,1%)), altpart$(line%+  9%)   , ch(25),~
         at (14,20), fac(tfac$(10%,1%)), altpart$(line%+ 10%)   , ch(25),~
         at (15,20), fac(tfac$(11%,1%)), altpart$(line%+ 11%)   , ch(25),~
         at (16,20), fac(tfac$(12%,1%)), altpart$(line%+ 12%)   , ch(25),~
         at (17,20), fac(tfac$(13%,1%)), altpart$(line%+ 13%)   , ch(25),~
         at (18,20), fac(tfac$(14%,1%)), altpart$(line%+ 14%)   , ch(25),~
         at (19,20), fac(tfac$(15%,1%)), altpart$(line%+ 15%)   , ch(25),~
         at (20,20), fac(tfac$(16%,1%)), altpart$(line%+ 16%)   , ch(25),~
         at (21,20), fac(tfac$(17%,1%)), altpart$(line%+ 17%)   , ch(25),~
         at (22,20), fac(tfac$(18%,1%)), altpart$(line%+ 18%)   , ch(25),~
         at (23,20), fac(tfac$(19%,1%)), altpart$(line%+ 19%)   , ch(25),~
         at (24,20), fac(tfac$(20%,1%)), altpart$(line%+ 20%)   , ch(25),~
                                                                         ~
         at (05,47), fac(hex(8c))      , altdesr$(line%+  1%)   , ch(34),~
         at (06,47), fac(hex(8c))      , altdesr$(line%+  2%)   , ch(34),~
         at (07,47), fac(hex(8c))      , altdesr$(line%+  3%)   , ch(34),~
         at (08,47), fac(hex(8c))      , altdesr$(line%+  4%)   , ch(34),~
         at (09,47), fac(hex(8c))      , altdesr$(line%+  5%)   , ch(34),~
         at (10,47), fac(hex(8c))      , altdesr$(line%+  6%)   , ch(34),~
         at (11,47), fac(hex(8c))      , altdesr$(line%+  7%)   , ch(34),~
         at (12,47), fac(hex(8c))      , altdesr$(line%+  8%)   , ch(34),~
         at (13,47), fac(hex(8c))      , altdesr$(line%+  9%)   , ch(34),~
         at (14,47), fac(hex(8c))      , altdesr$(line%+ 10%)   , ch(34),~
         at (15,47), fac(hex(8c))      , altdesr$(line%+ 11%)   , ch(34),~
         at (16,47), fac(hex(8c))      , altdesr$(line%+ 12%)   , ch(34),~
         at (17,47), fac(hex(8c))      , altdesr$(line%+ 13%)   , ch(34),~
         at (18,47), fac(hex(8c))      , altdesr$(line%+ 14%)   , ch(34),~
         at (19,47), fac(hex(8c))      , altdesr$(line%+ 15%)   , ch(34),~
         at (20,47), fac(hex(8c))      , altdesr$(line%+ 16%)   , ch(34),~
         at (21,47), fac(hex(8c))      , altdesr$(line%+ 17%)   , ch(34),~
         at (22,47), fac(hex(8c))      , altdesr$(line%+ 18%)   , ch(34),~
         at (23,47), fac(hex(8c))      , altdesr$(line%+ 19%)   , ch(34),~
         at (24,47), fac(hex(8c))      , altdesr$(line%+ 20%)   , ch(34),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)

               if keyhit% <> 15% then L46475
                  call "PRNTSCRN"
                  goto L46185

L46475:        if screen% <> 2% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *      T E S T   D A T A   F O R   F I R S T   P A G E      *~
            *-----------------------------------------------------------*~
            * Tests data on the first page of input for validity, etc.  *~
            * Loads part number entered/selected if already on file.    *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$, infomsg$ = " "
            on fieldnr% gosub        L50130,        /* Part on File     */~
                                     L50235,        /* Description      */~
                                     L50250,        /* Vendor Part #    */~
                                     L50355,        /* Stocking U.O.M.  */~
                                     L50440,        /* Sales unit of mea*/~
                                     L50525,        /* Conversion factor*/~
                                     L50550,        /* Category code    */~
                                     L50610,        /* Vendor code      */~
                                     L50670,        /* Lead Time        */~
                                     L50715,        /* Part Type        */~
                                     L50870,        /* MOQ              */~
                                     L50890,        /* Pansize          */~
                                     L50910,        /* Min SO Quantity  */~
                                     L50935         /* Safety Stock     */
                   return

L50130
*        Look for (and load) the INVENTORY PART
            if part$ <> " " or keyhit% = 17% then L50160
                call "GETCODE" (#2, part$, " ", 0%, 3.32, f1%(2%))
                     if f1%(2%) <> 0% then L50160
                     errormsg$ = hex(00)
                     return
L50160:     if len(part$) > 16% then L50190
            junkkey$ = part$
            call "PLOWALTS" (#9, junkkey$, 1%, 16%, f1%(9%))
            if f1%(9%) = 0% then L50190
                errormsg$ = "Cannot be Generic Cross Reference."
                return
L50190:     gosub L30000
            if dfltsedit% = 1% then part$ = "** Defaults **"
            if keyhit% = 17% then keyhit% = 0%
        REM * * * rem out the following statement to kill auto load * * *
        REM IF OLDPARTONFILE% = 0 THEN PROCEED% = 1%                    *
        REM * * * rem out the above statement to kill auto load * * * * *
            if oldpartonfile% = 0% then return
                return clear all
                goto editmode

L50235
*        DESCRIPTION
            return

L50250
*        GENERIC DESIGNATOR
            gendescr$   = " " : if generic$ = " " then return
            if generic$ <> "?" then L50285
                generic$ = " "
                call "PLOWCODE" (#9, str(generic$,,16), gendescr$, -16%, ~
                                                        -1.30, f1%(9%))
                if f1%(9%) = 0% then return
L50285:     if generic$ <> part$ then L50300
L50290:         errormsg$ = "Can't be the same as an inventory part"
                return
L50300:     call "READ100" (#2, generic$, f1%(2%))
            if f1%(2%) <> 0% then L50290
                junkkey$ =  generic$
                call "PLOWALTS" (#9, junkkey$, 1%, 16%, f1%(9%))
                if f1%(9%) = 1% then get #9 using L50330, gendescr$       ~
                    else gendescr$ = "*New, need to add description*"
L50330:              FMT POS(42), CH(30)
                if gendescr$ = " " then gendescr$ =                      ~
                                     "* On file, description blank *"
            return

L50355
*        Test STOCKING UNIT OF MEASURE
            if uom$ <> "Y" then return
                if stkunit$ <> " " or dfltsedit% = 0% then L50380
                     stkunitdescr$ = " "
                     return
L50380:         readkey$ = "UOM      " & stkunit$
                stkunitdescr$ = hex(06) & "SELECT STOCKING UOM."
                f1%(20%) = -8%
                call "PLOWCODE" (#20, readkey$, stkunitdescr$, 9%, 0.3,  ~
                                                                 f1%(20%))
                if f1%(20%) = 0% then L50425
                     stkunit$ = str(readkey$,10)
                     stkunitdescr$ = "(" & stkunitdescr$ & ")"
                     return
L50425:         errormsg$ = "Unit of Measure not on file: " & stkunit$
                return

L50440
*        Test SALES UNIT OF MEASURE
            if uom$ <> "Y" then return
                if prcunit$ <> " " or dfltsedit% = 0% then L50465
                     prcunitdescr$ = " "
                     return
L50465:         readkey$ = "UOM      " & prcunit$
                prcunitdescr$ = hex(06) & "SELECT SALES UOM."
                f1%(20%) = -9%
                call "PLOWCODE" (#20, readkey$, prcunitdescr$, 9%, 0.3,  ~
                                                                 f1%(20%))
                if f1%(20%) = 0% then L50510
                     prcunit$ = str(readkey$,10)
                     prcunitdescr$ = "(" & prcunitdescr$ & ")"
                     return
L50510:         errormsg$ = "Unit of Measure not on file: " & prcunit$
                return

L50525
*        Test UOM CONVERSION FACTOR
            if conversion$ = " " then conversion$ = "1"
            call "NUMTEST" (conversion$, 1e-7, 9e7, errormsg$, 0.7, temp)
            return

L50550
*        Test INVENTORY CATEGORY CODE (on file or blank)
            categorydescr$ = " "     /* Zap just in case.          */
            if category$   = " " then return
                if fs%(3%) = 0% then                                     ~
                    call "OPENCHCK" (#3, fs%(3%), f2%(3%), 0%, rslt$(3%))
               categorydescr$ = hex(06) & "Select Category Code"
               f1%(3%) = -11%
               call "PLOWCODE" (#3,category$,categorydescr$,0%,.3,f1%(3%))
               if f1%(3%) = 1% then return
                     errormsg$ = "Category not on file: " & category$
                     return

L50610
*        Test VENDOR CODE
            vendname$ = " "
            if vencode$ = " " then return
                if fs%(6%) = 0% then                                     ~
                    call "OPENCHCK" (#6, fs%(6%), f2%(6%), 0%, rslt$(6%))
                vendname$ = hex(06)& "Select VENDOR Code"
                f1%(06) = -12%
                call "PLOWCODE" (#6, vencode$, vendname$,0%, 0.3, f1%(6%))
                    if f1%(6%) <> 1% then L50655
                        call "PUTPAREN" (vendname$)
                        return
L50655:         errormsg$ = "Vendor code not on file: " & vencode$
                return

L50670
*        Test LEAD TIME
            call "NUMTEST" (leadtime$, 0, 999, errormsg$, 0.0, temp)
            return

L50715
*        Test PART TYPE
            if ptype$ <> " " or dfltsedit% = 0% then L50728
                ptypedescr$ = " " : return
L50728:     if ptype$ = "?" and part_type_chk$ = "Y" then L50742
            convert ptype$ to ptype%, data goto L50840
            convert ptype% to ptype$, pic(000)
            if ptype% < 0% or ptype% > 999% then L50840
            if part_type_chk$ <> "Y" then L50745
L50742:         gosub check_gencode_for_part_type
                if errormsg$ <> " " then return else L50750
L50745:     call "HNYTYPE" (ptype%, ptypedescr$, 1%)
L50750:     if onfileptype$ = " " or dfltsedit% = 1% then return
            if onfileptype$ = "000" or onfileptype$ > "199" then L50775
            if ptype$ > "000" and ptype$ < "200" then return
                ptypedescr$ = hex(94) & "(Changed From Non-Planned)"
                return
L50775:     if ptype$ = "000" or ptype$ > "199" then return
            if fs%(15%) = 0% then                                        ~
                call "OPENCHCK" (#15, fs%(15%), f2%(15%), 0%, rslt$(15%))
            call "PLOWALTS" (#15, str(part$,1,25) & hex(00000000), 1%,   ~
                                                           25%, f1%(15%))
            if f1%(15%) <> 0% then L50850
            if fs%(16%) = 0% then                                        ~
                call "OPENCHCK" (#16, fs%(16%), f2%(16%), 0%, rslt$(16%))
            call "PLOWALTS" (#16, str(part$,1,25) & hex(00000000), 1%,   ~
                                                           25%, f1%(16%))
            if f1%(16%)<>0 then L50850
                ptypedescr$ = hex(94) & "(Changed From Planned)"
                return
L50840:     errormsg$ = "Please enter Type as numeric, 1 to 999"
            return
L50850:     errormsg$ = "Part has activity in PIP"
            ptype$ = onfileptype$
            return

L50870
*        Test MOQ for numeric
            call "NUMTEST" (pmoq$, 0, 9e7, errormsg$, -2.2, temp)
            return

L50890
*        Test PANSIZE
            call "NUMTEST" (pansize$, 0, 9e7, errormsg$, -2.2, pansize)
            return

L50910
*        TEST MIN SO QUANTITY &  MIN SO INCREMENT
            call "NUMTEST" (minsoqty$, 0, 9e7, errormsg$, -2.2, minsoqty)
            if errormsg$ <> " " then return
            call "NUMTEST" (minsoinc$, 0, 9e7, errormsg$, -2.2, minsoinc)
            return

L50935
*        TEST SAFETY STOCK
            call "NUMTEST" (safety$, 0, 9e7, errormsg$, -2.2, safety)
            return

        check_gencode_for_part_type
            if ptype$ = "?" then ptype$  =  all(hex(20))
            gcplowkey$ = "PARTTYPE " &  ptype$
            ptypedescr$   = hex(06) & "Select Part Type Code"
            call "PLOWCODE" (#20, gcplowkey$, ptypedescr$,               ~
                                                        9%, .3, f1%(20%))
            if f1%(20%) = 1% then L50975
                 errormsg$ = "Part Type not on File"
                 return
L50975:     ptype$ = str(gcplowkey$, 10%, 3%)
            convert ptype$ to ptype%
            return

        REM *************************************************************~
            *      T E S T   D A T A   O N   S E C O N D   P A G E      *~
            *                                                           *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$, infomsg$ = " "
            if fieldnr% = 1%  then gosub L51110     /* Cost Method      */~
                              else gosub L51180     /* Account Numbers  */
            return

L51110
*        Test for COST METHOD
            if pos("RXPYSTABFLM" = costtype$) = 0% then L51150
            gosub L34210
            return
L51150:         errormsg$ = "Must be R, X, P, Y, S, T, F, A, B, M, or L"
                return

L51180
*        Test ACCOUNT NUMBERS
            a% = val(str(a_fnxref$,fieldnr%-1%,1),1)
            acct_descr$(a%) = " "
            if dfltsedit% = 1% and acct$(a%) = " " then return
            if (a% = 5% or (a% >= 7% and a% <= 19%))                     ~
                                     and acct$(a%) = " " then return
            if fs%(4%) = 0% then                                         ~
                  call "OPENCHCK" (#4, fs%(4%), f2%(4%), 0%, rslt$(4%))
            f1%(4) = -val(str(a_line$,a%,1),1)
            acct_descr$(a%) = hex(06) & "Select "& a_descrs$(a%)& " Acct"
            call "PLOWCODE" (#4, acct$(a%),acct_descr$(a%),0%,0.3,f1%(4%))
            if f1%(4%) <> 0% then return
                errormsg$ = "Account not on file: " & acct$(a%)
                return

        REM *************************************************************~
            *      T E S T   D A T A   O N   T H I R D     P A G E      *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$, infomsg$ = " "
                  skipfield = 0
                  on fieldnr% gosub  L52200,        /* Bin Location     */~
                                     L52230,        /* Buyer Class Code */~
                                     L52230,        /* Planner Class Cod*/~
                                     L52440,        /* Cycle count stuff*/~
                                     L52520,        /* Special/obsolete */~
                                     L52590,        /* Class            */~
                                     L52750,        /* Taxable          */~
                                     L52800,        /* Lot Track        */~
                                     L52850,        /* Serial Number    */~
                                     L52900,        /* Neg. Quantity    */~
                                     L52950,        /* Std. Potency     */~
                                     L53020,        /* Drawing Size/Ref */~
                                     L53050,        /* Rec > Order %    */~
                                     L53130,        /* S.O. Priority    */~
                                     L53190,        /* ATC Horizon      */~
                                     L53231,        /* Overship % & unit*/~
                                     L53236         /* Default Formula  */
                  return

L52200
*        Test BIN LOCATION
            return

L52230
*        Test PLANNER/BUYER CODE
            i% = fieldnr% - 1%
            if bclass$(i%) = " " then return
                readkey$ = "BYCLASSES" & bclass$(i%)
                bcdesc$(i%) = hex(06) & "Select Buyer Part Class"
                if i% = 1 then L52310
                readkey$ = "PSCLASSES" & bclass$(i%)
                bcdesc$(i%) = hex(06) & "Select Scheduler Part Class"
L52310:         f1%(20) = -8%
                call "PLOWCODE" (#20, readkey$, bcdesc$(i%), 9%, 0.3,    ~
                                                                 f1%(20%))
                if f1%(20%) = 0% then L52380
                     bclass$(i%) = str(readkey$,10)
                     call "PUTPAREN" (bcdesc$(i%))
                     return
L52380:         errormsg$ = "Invalid Buyer Part Class: " & bclass$(i%)
                if i% = 1% then return
                     errormsg$ =                                         ~
                            "Invalid Scheduler Part Class: "& bclass$(i%)
                return

L52440
*        Test CYCLE COUNT INFORMATION
            if pos(" ABCDX" = cyclecat$) <> 0% then L52472
             errormsg$ = "Cycle Count Category must be ' ', A, B, C, D, X"
                return
L52472:     if pos("YN" = cyclelock$) <> 0% then L52480
                errormsg$ = "ABC Lock must be 'Y' = Locked or 'N' = Not "
                return
L52480:     call "NUMTEST" (count_rate$, 0, 9e7, errormsg$, 2.2,         ~
                                                             count_rate)
                if errormsg$ <> " " then return
            if cycledate$ = " " then return
                call "DATEOK" (cycledate$, err%, errormsg$)
                return

L52520
*        Test SPECIAL/OBSOLETE CODE
            if indicator$ = " " then return
            if indicator$ = "SPEC" then return
            if indicator$ = "OBSO" then return
                errormsg$ = "Invalid entry for Indicator: " & indicator$
                return

L52590
*        Test for CLASS
            if class% <> 1% then return
                if class$ <> " " or dfltsedit% = 0% then L52640
                     class_descr$ = " "
                     return
L52640:         readkey$ = "PARTCLASS" & class$
                class_descr$ = hex(06) & "SELECT PART CLASS."
                f1%(20) = -10%
                call "PLOWCODE" (#20, readkey$, class_descr$, 9%, 0.3,   ~
                                                                 f1%(20%))
                if f1%(20%) = 0% then L52720
                     class$ = str(readkey$,10)
                     return
L52720:         errormsg$ = "Part Class Code not on file: " & class$
                return

L52750
*        Test for TAXABLE FLAG
            if pos(" YN" = taxable$) <> 0% then return
                errormsg$ = "Enter blank, 'Y' or 'N'."
                return

L52800
*        Test LOT TRACK
            if oldpartonfile% = 1% and                                   ~
                (lottrack$ = "Y" and oldlottrack$ = "N")                 ~
                                 then test_hnyquan
            if pos("YN" = lottrack$) <> 0% then return
                errormsg$ = "Must be 'Y' or 'N'."
                return

L52850
*        Test SERIAL NUMBERS
            if pos("YN" = serial$) <> 0% then return
                errormsg$ = "Must be 'Y' or 'N'."
                return

L52900
*        Test NEGATIVE QUANTITIES
            if pos("YN" = negqty$) <> 0% then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L52950
*        Test STANDARD POTENCY
            call "NUMTEST" (potency$, 0, 9e7, errormsg$, 2.4, potency)
            if errormsg$ <> " " then return
                if potency <= 0 then errormsg$ =                         ~
                            "Default potency must be greater than zero."
                return

L53020
*        Test DRAWING SIZE & REFERENCE
            return

L53050
*        Test Max Receipt over Order Percent & Test Max Receipt over Ord
            call "NUMTEST" (ovrrecpct$, 0, 9e7, errormsg$, 2.4, ovrrecpct)
            call "NUMTEST" (ovrrecqty$, 0, 9e7, errormsg$, 2.2, ovrrecqty)
            return

L53130
*        Test Planning - Priority, Demand Type, Allocation Method
            if sopriority$  = " " then L53164
            if sopriority$ >= "A" and sopriority$ <= "Z" then L53164
                errormsg$ = "Invalid Priority.  Enter ' ' or A-Z."
                return
L53164:     if demtypedflt$ = "1" or demtypedflt$ = "2" then L53171
            if demtypedflt$ = " " then L53171
                errormsg$ = "Invalid Demand Type.  Enter ' ', '1' or '2'."
                cursor%(2%) = 45%
                return
L53171:     if pos(" NACZP" = allocdflt$) > 0% then return
                errormsg$ = "Invalid Allocation Method.  " &             ~
                            "Enter ' ', 'N', 'A', 'P', 'C', or 'Z'."
                cursor%(2%) = 55%
                return

L53190
*        Test ATC HORIZON
            if atch$ = " " then atch$ = "0"
            call "NUMTEST" (atch$, 0, 999, errormsg$, -0.001, atch)
            return

L53231
*        Test Max Receipt Overship Percent & Units
            call "NUMTEST" (ovrshppct$, 0, 9e7, errormsg$, 2.4, ovrshppct)
            call "NUMTEST" (ovrshpqty$, 0, 9e7, errormsg$, 2.2, ovrshpqty)
            return

L53236
*        Test Default Formula Name
            if form_calc_flag$ = "N" or formula_name$ = " " then return
            if fs%(10%) = 0% then                                        ~
                  call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%,rslt$(10%))
            readkey$ = hex(06) & "Select Applicable Formula For this Part"
            f1%(10%) = -20%
            call "PLOWCODE" (#10, formula_name$, readkey$, 0%,.3,f1%(10%))
            if f1%(10%) = 1% then return
            errormsg$="Default Formula Name not on file: " & formula_name$
            return

        test_hnyquan
            init (hex(00)) junkkey$
            str(junkkey$,,16) = " "
            str(junkkey$,17,25) = str(part$)
            if fs%(17%) = 0% then                                        ~
                call "OPENCHCK" (#17, fs%(17%), f2%(17%), 0%, rslt$(17%))
            call "PLOWALTS" (#17, junkkey$, 1%, 41%, f1%(17%))
                if f1%(17%) = 0% then return
L53311:     ask% = 2%
                call "ASKUSER" (ask%, "***** WARNING *****",             ~
                     "Lot 'Blank' is on File For This Part.", " ",       ~
                     "Press RETURN to Continue, PF1 to Set to 'N'.")
                if ask% = 0% then return
                if ask% <> 1% then L53311
                lottrack$ = "N" : return
            errormsg$ = "CANNOT Turn Lot Tracking ON with Lot 'Blank'" & ~
                        " on the HNYQUAN File"
            return


        REM *************************************************************~
            *      T E S T   A L T E R N A T E   P A R T S              *~
            *-----------------------------------------------------------*~
            * Test Alternates - parts entered must be on file.          *~
            *************************************************************

            deffn'154(fieldnr%)
                  errormsg$ = " "
                  c% = currentline%
                  on fieldnr% gosub  L54130         /* PART NUMBER      */
            return

L54130
*        Test ALT PART NUMBER - No Blanks Allowed
            call "GETCODE" (#2, altpart$(c%), altdesr$(c%), 1%, 3,f1%(2%))
            if f1%(2%) <> 0% then L54180
                errormsg$ = "Part Number Not On File: " & altpart$(c%)
                return
L54180:     if altpart$(c%) <> part$ then L54210
                errormsg$ = "Can't be Same As Part Being Edited"
                return
L54210:     if maxlines% < 1% then return
                for x9% = 1% to maxlines% + 1%
                   if x9% = c% then L54270
                     if altpart$(c%) <> altpart$(x9%) then L54270
                          errormsg$ = "Part Number Is Already In List"
                          return
L54270:         next x9%
                return

        see_procurement_his              /* View Procurement History */
            if fs%(8%) = 0% then                                         ~
                    call "OPENCHCK" (#8, fs%(8%), f2%(8%), 0%, rslt$(8%))
            if fs%(7%) = 0% then                                         ~
                    call "OPENCHCK" (#7, fs%(7%), f2%(7%), 0%, rslt$(7%))
            if fs%(6%) = 0% then                                         ~
                    call "OPENCHCK" (#6, fs%(6%), f2%(6%), 0%, rslt$(6%))
            call "HNYPRCSB" (vencode$, part$, 0%, #7, #2, #6, #8)
            return

        view_ecr_info                    /* View ECR History this Part */
            call "ECRINQSB" ("A",        /* "A" - Show ALL ECRs this   */~
                                         /*    part, open or closed.   */~
                             part$,      /* Part to Do Inquiry/Check on*/~
                             ecrpfk$,    /* IN:  PFKey # to Use        */~
                             #01,        /* SYSFILE2                   */~
                             #02)        /* HNYMASTR                   */
            return

        move_formula_var_in              /* Move Formula Variable Fields*/
            for i% = 1% to 10%           /* from working storage to perm*/
                if len(vff$(i%)) < 13% then L55250
                   call "ASKUSER" (keyhit%,  "*** FIELD ENTRY ERROR ***",~
                     "The Formula Variable value you entered, " &        ~
                     vff$(i%) &", is too long!",                         ~
                     "Unless you change it it will be truncated to 12 " &~
                     "characters.",                                      ~
                     "Press RETURN/ENTER to Acknowledge this Warning.")
L55250:         vfv$(i%) = vff$(i%)
            next i%
            return

        move_formula_var_out             /* Move Formula Variable Fields*/
            for i% = 1% to 10%           /* from perm to working storage*/
                vff$(i%) = vfv$(i%)
            next i%
            return

        view_add_pm_items
            if pm_on$ <> "Y" then return
            call "PMPTBLSB"( part$, #2, #20,"S")
            return

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************
        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
