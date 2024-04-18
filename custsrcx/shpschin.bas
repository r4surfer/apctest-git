        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   H   H  PPPP    SSS    CCC   H   H  IIIII  N   N   *~
            *  S      H   H  P   P  S      C   C  H   H    I    NN  N   *~
            *   SSS   HHHHH  PPPP    SSS   C      HHHHH    I    N N N   *~
            *      S  H   H  P          S  C   C  H   H    I    N  NN   *~
            *   SSS   H   H  P       SSS    CCC   H   H  IIIII  N   N   *~
            *                                                           *~
            * --------------------------------------------------------- *~
            * SHPSCHIN - Allows scheduling of an order for shipment and *~
            *            indicating the quantities to ship.             *~
            * ---------------------------------------------------------Q*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            * --------------------------------------------------------- *~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/24/86 ! Original                                 ! ERN *~
            * 05/13/87 ! Std Costing Changes.                     ! ERN *~
            * 11/02/87 ! Add Export Invoice Recognition           ! MJB *~
            * 02/03/88 ! Added check for cancelled orders         ! JDH *~
            * 04/04/88 ! Added Last SO/BOL Msg to first screen    ! MJB *~
            * 04/21/88 ! Added Soft Enables                       ! TLJ *~
            * 10/07/88 ! Added Packing Units, Packing Qty.        ! JIM *~
            * 11/22/88 ! Added RTEALTRS & HNYALTRS for PLNRSUB    ! MJB *~
            * 08/21/89 ! Took out code that disabled fields 4,5,6.! JDH *~
            *          !   Fixed PF4 & PF16 on screen 1.          !     *~
            * 09/21/89 ! Misc. clean up.                          ! JDH *~
            * 09/25/89 ! Over/Under Shipment project              ! LAB *~
            * 11/01/89 ! Over/Under Shipment project              ! JDH *~
            * 03/06/90 ! Changed 85 to 86 on MIN statement for    ! LAB *~
            *          ! next, last, and up PF keys               !     *~
            * 05/16/90 ! Chngd qtys sent to SHPOVRSB; chngd 'Un-  ! JDH *~
            *          !  scheduled' to 'Uncommitted' & is the qty!     *~
            *          !  not shipped or scheduled; part descrip  !     *~
            *          !  now comes from BCKLINES, not HNYMASTR.  !     *~
            *          !  Defaults for printing Pick Lists & BOLs !     *~
            *          !  are now 'Y' & 'N' respectively.         !     *~
            * 01/29/91 ! FSE on first screen mods.                ! JDH *~
            * 06/14/91 ! Added call to ALLFREE.                   ! JDH *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 04/06/92 ! PRR 12301. Can't flag complete line zero.! JDH *~
            * 06/05/92 ! PRR 12469. Honors Export Auto-Apprvl flag! JDH *~
            * 11/03/93 ! Added shipping priority code to display  ! MLJ *~
            *          !  via PF(27).  Key toggles Seq#/Priority &!     *~
            *          !  Uncomitted/Allocated.                   !     *~
            * 01/13/94 ! Added SO/line to filler of SHPLINES      ! WPH *~
            *          !                                          !     *~
            * 11/19/97 ! Moved in RHHSRCE Mods to this version    ! djd *~
            *          !                                          !     *~
            * 04/06/98 ! Y2K modifications                        ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            alloc$(100)10,               /* Qty Allocated              */~
            autoapp$1,                   /* Export Auto-Approvals      */~
            avail(100), avail$(100)10,   /* Qty Avail for Scheduling   */~
            blankdate$6,                 /* PD fmt of a blank date     */~
            bol$3,                       /* Bill of Lading Number      */~
            carrier$6,                   /* Carrier                    */~
            carriername$32,              /* Carrier Name               */~
            committed(100),              /* Already shipped or scheduld*/~
            comp$(100)1,                 /* Completion flag            */~
            crflag$1,                    /* Order Credit Status Flag   */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9,                   /* Ship-to Cutomer Code       */~
            cusname$30,                  /* Ship-to Customer Name      */~
            date$8,                      /* Date for screen display    */~
            def_percent(100),            /* Default Percentages        */~
            def_unit(100),               /* Default Units              */~
            disp$(100)3,                 /* Field to display 3 fields  */~
            edtmessage$79,               /* Edit screen message        */~
            enabled%(25),                /* Array for soft enables     */~
            errormsg$79,                 /* Error message              */~
            expappr$1,                   /* Export Approval Flag       */~
            export$1, expdesc$16,        /* Export Order flag          */~
            field$(100)3,                /* Seq/Priority disp array    */~
            fob$20,                      /* FOB                        */~
            hdr1$03, hdr2$25, hdr3$08,   /* Column Headings            */~
            hdr4$03, hdr5$01, hdr6$10,   /* Column Headings            */~
            hdr6_7$10, hdr7$10, hdr8$10, /* Column Headings            */~
            hdr11$3, hdr1_11$3,          /* Column Heading             */~
            howship$20,                  /* How Ship                   */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            instr$(2)50,                 /* Shipping Instructions      */~
            invnr$8,                     /* Invoice Number             */~
            ivtext$(113,1)70,            /* Invoice Text Array         */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lastbol$3, lastso$16,        /* For screen #1 info         */~
            lastlit$35,                  /* For screen #1 info         */~
            line2$79,                    /* Second Line of Screen Headr*/~
            lotnrs$(30)6,                /* Lot Number Array           */~
            lotqtys(30),                 /* Lot Number Qtys/ Costs     */~
            msg$79,                      /* Message to TXTDSPLY routine*/~
            order(100),                  /* Current order amount       */~
            origschld(100),              /* Original Schedule Qty      */~
            overship$1,                  /* Overshipment allowed flag  */~
            part$(100)25,                /* Part Numbers               */~
            partdesc$(100)32,            /* Part Descriptions          */~
            pf$(3)79, pfkey$22,          /* PF Keys                    */~
            pn_or_desc$(100)25,          /* for PN or Description      */~
            printdate$8,                 /* Document Print Date        */~
            printpick$1, printbol$1,     /* Ship Doc Print Flags       */~
            readkey$100, readkey1$100,   /* Multi-Use Read Keys        */~
            reqd$(100)8,                 /* Required Ship Dates        */~
            schflag$(100)1,              /* Already Scheduled Flag     */~
            schld(100), schld$(100)10,   /* Qty Scheduled              */~
            scr%(1,15), set%(255),       /* Soft Enable Tables         */~
            seqnr$(100)3,                /* Line Item Sequence Numbers */~
            shipcode$(100)3,             /* Shipping Priority array    */~
            shipdflt$1,                  /* Customer ship priority code*/~
            shipdate$8,                  /* Scheduled Ship Date        */~
            shipdatelast$8,              /* Scheduled Ship Date (Last) */~
            so$16,                       /* Sales Order - BOL          */~
            sotext$(113,1)70,            /* Text Array                 */~
            textso$(100)4, textiv$(100)4,/* Line Text IDs              */~
            textso$4, textiv$4,          /* Header Text ID             */~
            toggle$(100)10,              /* Toggle betw AVAIL$/ALLOC$  */~
            txt$(100)1,                  /* Line Item Text Avail Flag  */~
            userid$3                     /* Current User Id            */

        dim over(100),                   /* Tested Overship flag       */~
            pkqty$(100)4, pkqty(100),    /* Package quantity           */~
            pkpak$(100)8, pkpak%(100),   /* Package type, type ok?     */~
            hdr9$4, hdr10$8,             /* Screen headers             */~
            qfac$(20)1,                  /* FACs                       */~
            sfac$(20)1,                  /* FACs                       */~
            paktypname$30                /* Package type name dummy    */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 11/19/97 General Release  Purchase Jobs  "
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
            * #1  ! SHPHDRS  ! Shipment Scheduling / Pre-Invoicing- Hea *~
            * #2  ! SHPLINES ! Shipment Scheduling / Pre-Invoicing- Lin *~
            * #3  ! GENCODES ! General Codes File                       *~
            * #4  ! BCKPRIDX ! SO Document Print Index File             *~
            * #5  ! BCKMASTR ! Backlog master file                      *~
            * #6  ! BCKLINES ! Back Log Line Item File                  *~
            * #7  ! PIPIN    ! Planned inventory additions detail       *~
            * #8  ! PIPOUT   ! Planned inventory use detail record      *~
            * #9  ! BCKBUFFR ! Backlog buffer for SO headers            *~
            * #10 ! DEMMASTR ! Demand Master File                       *~
            * #11 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #12 ! HNYMASTR ! Inventory Master File                    *~
            * #13 ! HNYDETAL ! Inventory detail file                    *~
            * #14 ! RTEMASTR ! Production routing master file           *~
            * #15 ! WCMASTR  ! Workcenter Master File                   *~
            * #16 ! CALMASTR ! Planning Production Calendar File        *~
            * #17 ! BOMMASTR ! BOM relationship file                    *~
            * #18 ! WCOUT    ! Planned work center use detail record    *~
            * #19 ! ENGMASTR ! Engineering Master Filer                 *~
            * #20 ! PIPCROSS ! hard peg cross reference                 *~
            * #21 ! SFMASTR2 ! Sales forecast master file               *~
            * #22 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #23 ! TXTFILE  ! System Text File                         *~
            * #24 ! SYSFILE2 ! System Misc File                         *~
            * #25 ! HNYQUAN  ! Inventory Quantity File                  *~
            * #26 ! HNYPOOL  ! Inventory LIFO/FIFO Pools File           *~
            * #27 ! CUSTOMER ! Customer Master File                     *~
            * #30 ! HNYALTRS ! Alternate Parts                          *~
            * #31 ! RTEALTRS ! Job, PIP X-ref                           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "SHPHDRS",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =  1,   keylen = 28

            select #2,  "SHPLINES",                                      ~
                        varc,     indexed,  recsize = 600,               ~
                        keypos =  10,  keylen = 22

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24                      ~

	    /* CHANGE 1 - RECORD LENGTH CHANGE */
            select #4,  "BCKPRIDX",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =   11, keylen =  29,                     ~
                        alt key  1, keypos =    1, keylen =  39          ~

            select #5,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup     ~

            select #6,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19                      ~

            select #7,  "PIPIN",                                         ~
                        varc,     indexed,  recsize =   60,              ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select #8,  "PIPOUT",                                        ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select #9,  "BCKBUFFR",                                      ~
                        varc,     indexed,  recsize = 1020,              ~
                        keypos =    1, keylen =  10,                     ~
                        alt key  1, keypos =    4, keylen =   7, dup,    ~
                            key  2, keypos =   30, keylen =  16          ~

            select #10, "DEMMASTR",                                      ~
                        varc,     indexed,  recsize =  123,              ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28          ~

            select #11, "PIPMASTR",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select #12, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #13, "HNYDETAL",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =   43, keylen =   6, dup,    ~
                            key  2, keypos =   49, keylen =   2, dup     ~

            select #14, "RTEMASTR",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    5, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  35          ~

            select #15, "WCMASTR",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6          ~

            select #16, "CALMASTR",                                      ~
                        varc,     indexed,  recsize = 1962,              ~
                        keypos =    1, keylen =   2                      ~

            select #17, "BOMMASTR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56          ~

            select #18, "WCOUT",                                         ~
                        varc,     indexed,  recsize =   68,              ~
                        keypos =    1, keylen =  35,                     ~
                        alt key  1, keypos =   20, keylen =  16          ~

            select #19, "ENGMASTR",                                      ~
                        varc,     indexed,  recsize = 2015,              ~
                        keypos =    1, keylen =  29                      ~

            select #20, "PIPCROSS",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  71,                     ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33          ~

            select #21, "SFMASTR2",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  25                      ~

            select #22, "SFCUM2",                                        ~
                        varc,     indexed,  recsize = 1985,              ~
                        keypos =    1, keylen =  25                      ~

            select #23, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11

            select #24, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =    1, keylen =  20

            select #25, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =   44

            select #26, "HNYPOOL",                                       ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen =  38

            select #27, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #30, "HNYALTRS",                                      ~
                        varc,     indexed,  recsize = 60,                ~
                        keypos =    1, keylen =  33

            select #31, "RTEALTRS",                                      ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos =   1, keylen = 34

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 100%, rslt$(1 ))
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 200%, rslt$(2 ))
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ),   0%, rslt$(3 ))
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 100%, rslt$(4 ))
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ),   0%, rslt$(5 ))
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ),   0%, rslt$(6 ))
            call "OPENCHCK" (#7,  fs%(7 ), f2%(7 ),   0%, rslt$(7 ))
            call "OPENCHCK" (#8,  fs%(8 ), f2%(8 ),   0%, rslt$(8 ))
            call "OPENCHCK" (#9,  fs%(9 ), f2%(9 ), 100%, rslt$(9 ))
            call "OPENCHCK" (#10, fs%(10), f2%(10),   0%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11),   0%, rslt$(11))
            call "OPENCHCK" (#12, fs%(12), f2%(12),   0%, rslt$(12))
            call "OPENCHCK" (#13, fs%(13), f2%(13),   0%, rslt$(13))
            call "OPENCHCK" (#14, fs%(14), f2%(14),   0%, rslt$(14))
            call "OPENCHCK" (#15, fs%(15), f2%(15),   0%, rslt$(15))
            call "OPENCHCK" (#16, fs%(16), f2%(16),   0%, rslt$(16))
            call "OPENCHCK" (#17, fs%(17), f2%(17),   0%, rslt$(17))
            call "OPENCHCK" (#18, fs%(18), f2%(18),   0%, rslt$(18))
            call "OPENCHCK" (#19, fs%(19), f2%(19),   0%, rslt$(19))
            call "OPENCHCK" (#20, fs%(20), f2%(20),   0%, rslt$(20))
            call "OPENCHCK" (#21, fs%(21), f2%(21),   0%, rslt$(21))
            call "OPENCHCK" (#22, fs%(22), f2%(22),   0%, rslt$(22))
            call "OPENCHCK" (#24, fs%(24), f2%(24),   0%, rslt$(24))
            call "OPENCHCK" (#25, fs%(25), f2%(25),   0%, rslt$(25))
            call "OPENCHCK" (#26, fs%(26), f2%(26),   0%, rslt$(26))
            call "OPENCHCK" (#27, fs%(27), f2%(27),   0%, rslt$(27))
            call "OPENCHCK" (#30, fs%(30), f2%(30),   0%, rslt$(30))
            call "OPENCHCK" (#31, fs%(31), f2%(31),   0%, rslt$(31))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date : call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)
            call "DATUFMTC" (blankdate$)
            edtmessage$ = "To Modify Displayed Values, Press (RETURN)."
            printpick$ = "Y"  :  printbol$ = "N"
            select printer
            hdr1$, hdr1_11$ = "Seq"
            hdr2$ = "Part Code/Description"
            hdr3$ = "Required"
            hdr4$ = "TCS"
            hdr5$ = " "
            hdr6$ = "Uncommited"
            hdr7$, hdr6_7$ = " Allocated" : toggle% = 1%
            hdr8$ = " Scheduled"
            hdr9$ = "PQty"
            hdr10$= "Packtype"
            hdr11$= "Pty"

            lastbol$, lastso$ = " "

            gosub init_enables

            call "BCKSWTCH" ("BCK", "OVRSHIP ", overship$, temp, u3%)
                temp  = 0    /* Just so I don't get a compile error */
            call "BCKSWTCH" ("BCK", "OVRSHIP%", temp$, sys_def_percent,  ~
                              u3%)
                temp$ = " "  /* Just so I don't get a compile error */
            call "BCKSWTCH" ("BCK", "XAUTOAPP", autoapp$, temp, u3%)

*        See if User is an administator
            call "CMSMACHK" ("ARM", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then admin% = 1%         ~
                                                else admin% = 0%

*        Clear any Sales Order for this User that was in process.
            readkey$ = all(hex(00))  :  str(readkey$,,3) = userid$
            call "READ101" (#9, readkey$, f1%(9))
            if f1%(9) = 0% then L10000
                so$ = key(#9, 2%)
                get #9 using L09480, readkey$
L09480:              FMT XX(10), CH(8)
                u3% = 2%
                if readkey$ = "SHPSCHIN" then L09580
                     pf$(1) = "You did not complete processing Sales" &  ~
                              " Order " & so$
                     pf$(2) = "in the program " & str(readkey$,,8) & "."
                     pf$(3) = "Press (RETURN) to exit this program."
                     call "ASKUSER" (u3%, "IN-PROCESS MESSAGE",          ~
                                     pf$(1), pf$(2), pf$(3))
                     goto exit_program
L09580:         call "ASKUSER" (u3%, "RESTART NOTE",                     ~
                     "Processing for the Sales Order shown below was",   ~
                     "not completed.  Press (RETURN) To Continue", so$)
                delete #9

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, so$, bol$, cuscode$,       ~
                      cusname$, shipdate$, carrier$, carriername$,       ~
                      howship$, fob$, instr$(), seqnr$(), part$(),       ~
                      reqd$(), txt$(), textso$, textso$(), avail$(),     ~
                      alloc$(), schld$(), schflag$(), store$, export$,   ~
                      shipdatelast$, expappr$, partdesc$(), toggle$(),   ~
                      pn_or_desc$(), pkqty$(), pkpak$(), comp$(),disp$(),~
                      shipcode$(), shipdflt$, field$()

            init (hex(ff)) textiv$, textiv$()
            call "TXTFUTIL" (#23, f2%(23), "INTL", textiv$)
            top% = 1% : toggl% = 0%
            mat avail     = zer
            mat schld     = zer
            mat origschld = zer
            mat pkqty     = zer
            mat pkpak%    = zer   : mat over = zer
            mat def_percent = zer : mat def_unit = zer
            call "ALLFREE"

            for fieldnr% = 1% to 2%
L10220:         gosub'051(fieldnr%, 1%)  /* Check Enables, Set Defaults*/
                     if enabled% = 0% then L10330
L10240:         gosub'101(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10310
L10270:                  fieldnr% = max(2%, fieldnr% - 1%)
                         gosub'051(fieldnr%, 1%)
                         if enabled% = 1% then L10240
                         if fieldnr% = 2% then L10220
                         goto L10270
L10310:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10240
L10330:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10240
            next fieldnr%
            goto edit_lines

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edit_header
            inpmessage$ = edtmessage$
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  errormsg$   = " "
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       edit_lines
                  if keyhit%  = 12 then gosub delete_from_schedule
                  if keyhit%  = 16 then gosub datasave
                  if keyhit%  = 26 then gosub display_so_hdr_text
                  if keyhit%  = 28 then gosub capture_inv_header_text
                  if keyhit%  =  0 then L11245
                  if keyhit% <> 29% then edit_header
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 2% or fieldnr% > 9% then edit_header
            if fieldnr% > 7% then fieldnr% = fieldnr% - 1%
                gosub'049(1%, fieldnr%)
                goto edit_header
L11245:     fieldnr% = 2%
            gosub'051(fieldnr%, 2%)    /* Set Input Message           */
L11270:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11270
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11270
                  goto edit_header

        edit_lines
            inpmessage$ = "Press (RETURN) to Edit Scheduled / Package " &~
                "Quantities."
            gosub create_disp
            gosub'102(1%)               /* Display Screen - No Entry   */
                  errormsg$   = " "
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then gosub first
                  if keyhit%  =  3 then gosub last
                  if keyhit%  =  4 then gosub prev
                  if keyhit%  =  5 then gosub nexts
                  if keyhit%  =  6 then gosub down
                  if keyhit%  =  7 then gosub up
                  if keyhit%  =  8 then gosub plnrsub
                  if keyhit%  =  9 then gosub edit_header
                  if keyhit%  = 10 then gosub hnyqdisp
                  if keyhit%  = 11 then gosub toggle_pn_desc
                  if keyhit%  = 22 then gosub flag_complete
                  if keyhit%  = 26 then gosub display_so_line_text
                  if keyhit%  = 28 then gosub capture_inv_line_text
                  if keyhit%  = 16 then gosub datasave
                  if keyhit% <>  0 then       edit_lines
            gosub'052                   /* Set INPMESSAGE$             */
            for z% = top% to min(top% + 14%, maxlines%)
                pkpak%(z%) = 0%         /* Package type not tested     */
            next z%
L11610:     gosub'102(2%)               /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 11 then gosub toggle_pn_desc
                  if keyhit% <>  0 then L11610
            gosub'152                   /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11610
            goto edit_lines


        REM *************************************************************~
            *             M I S C    R O U T I N E S                    *~
            * --------------------------------------------------------- *~
            * Some routines invoked by the PF keys above.               *~
            *************************************************************

        first : top% = 1%                                        : return
        last  : top% = max(1%, min(86%, maxlines%-14%))          : return
        prev  : top% = max(1%, top%-14%)                         : return
        nexts : top% = max(1%, min(86%, top%+14%, maxlines%-14%)): return
        down  : top% = max(1%, top%-1%)                          : return
        up    : top% = max(1%, min(86%, top%+1%, maxlines%-1%))  : return


        display_so_hdr_text
            msg$ = str(line2$,,60)
            call "TXTDSPLY" (#23, f2%(23), "013", msg$, textso$,         ~
                             sotext$())
            return

        display_so_line_text
            gosub calc_line_number : if c% = 0% then return
            msg$ = "Customer: " & cuscode$ & " (" & cusname$ & ")  BOL: "~
                   & so$ & "-" & bol$ & "  Line: " & seqnr$(c%)
            call "TXTDSPLY" (#23, f2%(23), "014", msg$, textso$(c%),     ~
                             sotext$())
            return


        capture_inv_header_text
            msg$ = str(line2$,,60)
            call "TXTINSUB" (#23, f2%(23), "015", msg$, textiv$,         ~
                ivtext$())
            return

        capture_inv_line_text
            gosub calc_line_number : if c% = 0% then return
            msg$ = "Customer: " & cuscode$ & " (" & cusname$ & ")  BOL: "~
                & so$ & "-" & bol$ & "  Line: " & seqnr$(c%)
            call "TXTINSUB" (#23, f2%(23), "016", msg$, textiv$(c%),     ~
                ivtext$())
            return

        calc_line_number
            errormsg$ = " "
            c% = cursor%(1) - 6% + top%
            if c% >= 1% and c% <= maxlines% then return
                c% = 0%
                errormsg$ = "Position Cursor to the Line for Display."
                return

        plnrsub
            gosub calc_line_number : if c% = 0% then return
            call "PLNRSUB" (0%, part$(c%), #10, #11, #12, #14, #15, #16, ~
                            #17, #18, #19, #7 , #8 , #20, #21, #22, #13, ~
                            #31, #30)
            return

        hnyqdisp
            gosub calc_line_number : if c% = 0% then return
            call "HNYQDISP" (part$(c%), #12, #25, #26, #24)
            return

        clear_in_process
            readkey$ = all(hex(00))
            str(readkey$,,3) = userid$
            call "DELETE" (#9, readkey$, 10%)
            return

        delete_from_schedule
*        (1)Reduce scheduled qtys on order line items
*        (2)Delete Header and Lines from Scheduled file
*        (3)Remove in-process status
            u3% = 2%
            call "ASKUSER" (u3%, "DELETE FROM SCHEDULE",                 ~
                            "Enter PF-28 to remove Order from Schedule", ~
                            "-OR-", "(RETURN) to return to edit.")
            if u3% <> 28% then return
                if bol$ = "NEW" then L12950
                     for c% = 1% to maxlines%
                          readkey$ = str(so$) &  str(seqnr$(c%))
                          call "READ101" (#6, readkey$, f1%(6))
                          get #6 using L12840, schld
L12840:                        FMT POS(117), PD(14,4)
                          schld = schld - origschld(c%)
                          put #6 using L12840, schld
                          rewrite #6
                     next c%

                     readkey$ = str(cuscode$) & str(so$) & str(bol$)
                     call "DELETE" (#1, readkey$, 28%)
                     call "TXTFUTIL" (#23, f2%(23), "XOUT", textiv$)
                     readkey$ = str(so$) & str(bol$) & hex(00)
                     call "DELETE" (#2, readkey$, 19%)
                     for c% = 1% to maxlines%
                         call "TXTFUTIL" (#23, f2%(23), "XOUT",          ~
                               textiv$(c%))
                     next c%
                     gosub delete_print_file
L12950:         gosub clear_in_process
                return clear all
                goto inputmode

        toggle_pn_desc
            for i% = 1% to maxlines%
                if toggl% = 0% then pn_or_desc$(i%) = partdesc$(i%)      ~
                               else pn_or_desc$(i%) = part$(i%)
            next i%
            if toggl% = 0% then toggl% = 1% else toggl% = 0%
            return


        flag_complete
            gosub calc_line_number : if c% = 0% then return
            if comp$(c%) = " " then comp$(c%) = "C" else comp$(c%) = " "
            return

        create_disp
            for x% = 1% to 100%
                str(disp$(x%),1,1) = txt$(x%)
                str(disp$(x%),2,1) = comp$(x%)
                str(disp$(x%),3,1) = schflag$(x%)
            next x%
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            * --------------------------------------------------------- *~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            for c% = 1% to maxlines%
                if schld(c%) > 0 then L19130
            next c%
            errormsg$ = "There is nothing scheduled for shipment."
            return

L19130:     return clear all
            gosub save_data
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * --------------------------------------------------------- *~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%, mode%)
              enabled% = 1%
              if fieldnr% = 1% then L20220

                 for i% = 2% to 8%
                 call "ENABLSUB" ("SET", "SHPSCHIN", scr%(), set%(), 1%, ~
                                  i%, mode%, enabled%(i%))
                 next i%
                 inpmessage$ = "Enter Scheduling Header Information."
                 if mode% = 2% then return
                 goto L20270

L20220
*        Sales Order - BOL                     SO$
            inpmessage$ = "Enter Sales Order (and BOL to recall a" &     ~
                          " previously scheduled order)."
            return

L20270
*        Scheduled Ship Date                   SHIPDATE$
            if shipdate$ = " " then shipdate$ = date$

*        Carrier                               CARRIER$

*        How Ship                              HOWSHIP$

*        FOB                                   FOB$

*        Shipping Instructions                 INSTR$(2)

*        Pick List Printing                    PRINTPICK$
            if printpick$ = " " then printpick$ = "Y"

*        BOL Printing                          PRINTBOL$
            if printbol$ = " " then printbol$ = "N"
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            * --------------------------------------------------------- *~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052
            on fieldnr% gosub L21100                /* Scheduled        */
            return

L21100
*        Scheduled Quantities                  SCHLD$
            inpmessage$ = "Enter Changes and then Press (RETURN)."
            return

        REM *************************************************************~
            *         I N I T I A L I Z E   E N A B L E S               *~
            * --------------------------------------------------------- *~
            * Initialize Soft Enable Settings.                          *~
            *************************************************************
        init_enables
*        Define Screen, Field Cross Ref and Field Enable Settings.
            mat set% = con   : mat set% = (99%) * set%
            mat scr% = zer
            scr%(1, 1) =  1% : set%( 1) = 13%      /* Sales Order - BOL*/
            scr%(1, 2) =  2% : set%( 2) =  2%      /* Schdld Ship Date */
            scr%(1, 3) =  3% : set%( 3) =  2%      /* Carrier          */
            scr%(1, 4) =  4% : set%( 4) =  2%      /* How Ship         */
            scr%(1, 5) =  5% : set%( 5) =  2%      /* FOB              */
            scr%(1, 6) =  6% : set%( 6) =  2%      /* Ship Instructs   */
            scr%(1, 7) =  7% : set%( 7) =  2%      /* Pick List Print  */
            scr%(1, 8) =  8% : set%( 8) =  2%      /* BOL Print        */

            call "ENABLSUB" ("INIT", "SHPSCHIN", scr%(), set%(),         ~
                                                         0%, 0%, 0%, 0%)
            return

        REM *************************************************************~
            *           R E S E T   S O F T   E N A B L E S             *~
            * --------------------------------------------------------- *~
            * Allow User to modify enable settings.                     *~
            *************************************************************
        deffn'049(s%, f%)      /* Screen and Field Numbers             */
            if admin% <> 1% then return            /* Not Authorized   */
            call "ENABLSUB" ("MODIFY", "SHPSCHIN", scr%(), set%(),       ~
                              s%, f%, 0%, 0%)
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            * --------------------------------------------------------- *~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
L29300:     u3% = 2%
            call "STARTOVR" (u3%)
            if u3%  = 1% then return
            if u3% <> 0% then L29300
                return clear all              /* Wants to Start Over   */
                gosub clear_in_process
                goto inputmode


        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            * --------------------------------------------------------- *~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        load_data
*        First verify that the Sales Order exists.
            fnd% = 0% : so_saver$ = so$
L30070:     readkey$ = str(so$) & hex(00)
            call "PLOWNEXT" (#6, readkey$, 16%, f1%(6))  /* BCKLINES */
            if f1%(6) = 1% then L30120
                if fnd% <> 0% then goto L30100 /*Been here before?-Error*/
                if len(so$) > 7% then goto L30100 /*Too long?- Error */
                if pos(so$ = ".") <> 0% then goto L30100 /* Dots?- Error*/
                convert so$ to so%, data goto L30100 /*Not Num?- Error */
                convert so% to so$, pic (00000000) /* Zero-fill it */
                fnd% = 1%  /* Indicate 'been here before' */
                goto L30070 /* Try again with zero-filled SO$ */

L30100:         errormsg$ = "Sales Order is not on file."
                so$ = so_saver$
                return

L30120:     get #6 using L30130, cuscode$
L30130:         FMT CH(9)

*        Get default shipping priority for this customer...
            call "READ100" (#27, cuscode$, f1%(27%))
                if f1%(27%) = 0% then L30137
            get #27 using L30136, shipdflt$
L30136:         FMT POS(733), CH(1)
L30137:     if shipdflt$ = " " then shipdflt$ = "3"

            readkey$ = str(cuscode$) & so$
            call "READ100" (#5, readkey$, f1%(5))     /* BCKMASTR */
            if f1%(5) = 0% then L30100  /* Hopefully never happens */
                get #5 using L30180, crflag$
L30180:              FMT POS(875), CH(1)
                if crflag$ = " " then L30240
             if crflag$ = "H" then errormsg$ = "Order is on Credit Hold."
             if crflag$ = "C" then errormsg$ = "Order has been Cancelled"
                     return

L30240
*        Now look at the Bill of Lading
            if bol$ <> " " then L30480
          /* Check that there is nothing scheduled for this order      */
                readkey$ = str(so$) & hex(00)
                call "PLOWNEXT" (#2, readkey$, 16%, f1%(2))
                if f1%(2) = 0% then L30370
                     u3% = 2%
                     call "ASKUSER" (u3%, "CONTINUE???",                 ~
                          "There are BOLs outstanding for this Order.",  ~
                     " ", "Hit PF-16 to continue, RETURN to Start Over")
                     if u3% = 16% then L30370
                          return clear all
                          goto inputmode
L30370:         gosub check_inprocess  :  if errormsg$ <> " " then return
                bol$ = "NEW"
          /* Load defaults and other data from order         */
                get #5 using L30420, cusname$, howship$, fob$, instr$(),  ~
                                    textso$, store$, shipdate$, export$
L30420:              FMT XX(41), CH(30), POS(422), 2*CH(20), 2*CH(50),   ~
                         POS(799), CH(4), CH(3), POS(824), CH(6),        ~
                         POS(857), CH(1)
                if shipdate$ = blankdate$ then shipdate$ = date$                ~
                     else call "DATEFMT" (shipdate$)
                gosub load_lines
                set% = 1%  :  gosub calc_allocated
                return

L30480
*        Bill of Lading Specified - load it up
            get #5 using L30500, cusname$, textso$
L30500:         FMT XX(41), CH(30), POS(799), CH(4)
            readkey$ = str(cuscode$) & str(so$) & bol$
            call "READ100" (#1, readkey$, f1%(1))
            if f1%(1) = 1% then L30560
                errormsg$ = "Bill of Lading is not in scheduling file."
                return
L30560:     get #1 using L30570, shipdate$, invnr$
L30570:         FMT XX(183), CH(6), POS(234), CH(8)
            if shipdate$ = blankdate$ and invnr$ = " " then L30660
                call "DATEFMT" (shipdate$)
                if shipdate$ <> blankdate$ then                          ~
                     errormsg$ = "Bill of Lading Shipped on " & shipdate$
                if invnr$    <> " " then                                 ~
                     errormsg$ = "Bill of Lading Invoiced on Invoice " & ~
                                 invnr$
                return
L30660:     gosub check_inprocess  :  if errormsg$ <> " " then return
            get #1 using L30690, store$, shipdate$, carrier$, howship$,   ~
                         fob$, instr$(), textiv$, export$, expappr$
L30690:         FMT XX(28), CH(3), 2*CH(6), 2*CH(20), 2*CH(50),          ~
                    POS(242), CH(4), CH(1), CH(1)
            call "TXTFUTIL" (#23, f2%(23), "LOAD", textiv$)
            call "DATEFMT" (shipdate$)
            shipdatelast$ = shipdate$
            call "READ100" (#3, "CARRIERS " & str(carrier$), f1%(3))
                if f1%(3) = 1% then get #3 using L30760, carriername$
L30760:              FMT XX(24), CH(30)
            gosub load_lines
            set% = 0%  :  gosub calc_allocated
            return


        load_lines
            if export$ = "Y"                                             ~
                then expdesc$ = "Export" else expdesc$ = "Domestic"
                call "PUTPAREN" (expdesc$)
            maxlines% = 0%
            readkey$  = str(so$) & hex(00)
L30880:     call "PLOWNEXT" (#6, readkey$, 16%, f1%(6))
            if f1%(6) = 0% then return
                c%, maxlines% = maxlines% + 1%
                get #6 using L30930, seqnr$(c%), part$(c%), partdesc$(c%),~
                        invd, opn, schld, pre_inv, reqd$(c%),textso$(c%),~
                        shipcode$(c%)
L30930:              FMT XX(25), CH(3), XX(3), CH(25), CH(32), POS(101), ~
                         3*PD(14,4), POS(133), PD(14,4), POS(212),       ~
                         CH(6), POS(242), CH(4), POS(277), CH(1)
                if shipcode$(c%) = " " then shipcode$(c%) = shipdflt$
                call "DATEFMT" (reqd$(c%))
*              CALL "DESCRIBE" (#12,PART$(C%),PARTDESC$(C%),0%,F1%(12))
                pn_or_desc$(c%) = part$(c%)
                if textso$(c%) <> hex(ffffffff) and textso$(c%) <> " "   ~
                     then txt$(c%) = "T"
                call "READ100" (#12%, part$(c%), f1%(12))
                if f1%(12) = 0% then goto L31000
                get #12%, using L30994, def_percent(c%), def_unit(c%)
L30994:              FMT POS(722), PD(14,4), PD(14,4)
L31000:         if bol$ = "NEW" then L31090
                     readkey1$ = str(so$) & str(bol$) & str(seqnr$(c%))
                     call "READ100" (#2, readkey1$, f1%(2))
                     if f1%(2) = 0% then L31090
                          get #2 using L31050, schld(c%), textiv$(c%),    ~
                               pkqty$(c%), pkpak$(c%), comp$(c%)
L31050:                        FMT POS(32), PD(14,4), POS(460), CH(4),   ~
                                   CH(4), CH(8), CH(1)
                          origschld(c%) = schld(c%)
                          call "TXTFUTIL" (#23, f2%(23), "LOAD",         ~
                                textiv$(c%))
L31090:         if schld <> origschld(c%) then schflag$(c%) = "S"
                order(c%) = opn + invd
                committed(c%) = invd + pre_inv + schld - schld(c%)
                avail(c%) = opn - schld - pre_inv
                call "CONVERT" (avail(c%), 2.2, avail$(c%))
                call "CONVERT" (schld(c%), 2.2, schld$(c%))
                goto L30880


        check_inprocess
*        See if order is already in buffer.  If so can't touch
            call "REDALT0" (#9, so$, 2%, f1%(9))
            if f1%(9) = 0% then L31230
L31200:         errormsg$ = "Order is already being processed."
                return

L31230
*        All Ok-fine.  Flag Order as in-process.
            readkey$ = " "
            str(readkey$, 1, 3) = userid$
            str(readkey$, 4, 7) = all(hex(00))
            str(readkey$,11, 8) = "SHPSCHIN"
            str(readkey$,21,25) = str(cuscode$) & so$
            write #9 using L31310, readkey$, " ", " ", " ", " ",          ~
                                  eod goto L31200
L31310:         FMT CH(45), 3*CH(250), CH(225)
            return


        calc_allocated    /* Determine what has been allocated to the  */
                          /* line items on this order.  Sets Scheduled */
                          /* = ALLOC if SET% = 1%.                     */
          if maxlines% = 0% then return
            call "DATUNFMT" (shipdate$)
            call "PIPINDEX" (#24, shipdate$, shipdate%, u3%)
            call "DATEFMT"  (shipdate$)
            init (" ") alloc$()
            for c% = 1% to maxlines%
                alloc = 0
                readkey$  = str(so$) & str(seqnr$(c%)) & hex(00)
L31460:         call "PLOWNEXT" (#8, readkey$, 19%, f1%(8))
                if f1%(8) = 0% then L31540                    /* PIPOUT */
                     get #8 using L31490, pip%, qty
L31490:                   FMT XX(44), BI(4), XX(8), PD(14,4)
                     if pip% > shipdate% then L31540
                          alloc = alloc + qty
                          goto L31460

L31540:         readkey$  = str(so$) & str(seqnr$(c%))
                call "READ100" (#7, readkey$, f1%(7))        /* PIPIN  */
                if f1%(7) = 0% then L31610
                     get #7 using L31580, pip%, qty
L31580:                   FMT XX(25), BI(4), XX(19), PD(14,4)
                     if pip% <= shipdate% then alloc = alloc - qty

L31610:         call "CONVERT" (alloc, 2.2, alloc$(c%))
                if set% <> 1% then L31650
                     schld(c%) = max(0, min(alloc, avail(c%)))
                     call "CONVERT" (schld(c%), 2.2, schld$(c%))
L31650:     next c%
            return


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            * --------------------------------------------------------- *~
            * (1)Write BOL to Scheduling files.                         *~
            * (2)Adjust Scheduled Quantities on Order line items.       *~
            * (3)Remove order in-process flag.                          *~
            *************************************************************
        save_data
*        See if we need to assign a BOL Number
        if bol$ <> "NEW" then L32200
            readkey$ = str(cuscode$) & so$
            call "READ101" (#5, readkey$, f1%(5))  /* BCKMASTR */
            get #5 using L32130, bol%
L32130:         FMT POS(878), BI(4)
            convert bol% to bol$, pic(##0)
            bol% = bol% + 1%  :  if bol% = 1000% then bol% = 1%
            put #5 using L32130, bol%
            rewrite #5
            call "STRING" addr("LJ", bol$, 3%)

L32200
*        Now write the BOL Header
            call "DATUNFMT" (shipdate$)
            readkey$ = str(cuscode$) & str(so$) & str(bol$)
            call "DELETE" (#1, readkey$, 28%)
            if autoapp$ = "Y" then expappr$ = "Y"
            write #1 using L35060, cuscode$, so$, bol$, store$, shipdate$,~
                                  carrier$, howship$, fob$, instr$(),    ~
                                  " ", " ", 0, 0, 0, " ", textiv$,       ~
                                  export$, expappr$, " "
            call "TXTFUTIL" (#23, f2%(23), "TOS2", textiv$)

*        Now write the lines and adjusted the qty scheduled in BCKLINES
            readkey$ = str(so$) & str(bol$) & hex(00)
            call "DELETE" (#2, readkey$, 19%)
            for c% = 1% to maxlines%
                write #2 using L35270, cuscode$, so$, bol$, seqnr$(c%),   ~
                     schld(c%),  lotnrs$(), lotqtys(), textiv$(c%),      ~
                     pkqty$(c%), pkpak$(c%), comp$(c%),                  ~
                     so$, seqnr$(c%), " "
                call "TXTFUTIL" (#23, f2%(23), "TOS2", textiv$(c%))
                readkey$ = str(so$) &  str(seqnr$(c%))
                call "READ101" (#6, readkey$, f1%(6))
                get #6 using L32410, schld
L32410:              FMT POS(117), PD(14,4)
                schld = schld - origschld(c%) + schld(c%)
                put #6 using L32410, schld
                rewrite #6
            next c%

*        Write the Shipping Documents Print File
            gosub delete_print_file
*          IF EXPORT$ = "Y" AND EXPAPPR$ <> "Y" THEN GOTO 31600
            if printpick$ = "N" then L32560
                printdate$ = shipdate$
                if printpick$ = "I" then printdate$ = " "

		/* CHANGE 2 - ADDED TWO BLANK FIELDS TO THE END OF THE WRITE */
                /* STATEMENT 						     */
                write #4 using L32610, "P", store$, printdate$, "P",      ~
                                      cuscode$, so$, bol$, " ", " ", " "

L32560:     if printbol$ = "N" then L32640
                printdate$ = shipdate$
                if printbol$ = "I" then printdate$ = " "
		/* CHANGE 3 - ADDED TWO BLANK FIELDS TO THEN END OF THE WRITE */
                /* STATEMENT 						     */
                write #4 using L32610, "B", store$, printdate$, "B",      ~
                                      cuscode$, so$, bol$, " ", " ", " "

		/* CHANGE 4 - ADDED CH(20) TO THE END OF THE FMT STMT        */
L32610:         FMT CH(1), CH(3), CH(6), CH(1), CH(9), CH(16), CH(3),    ~
                    CH(6), CH(20)

L32640
*        Lastly, clear the Order In-process flag
            gosub clear_in_process
            lastbol$ = bol$
            lastso$ = so$
            return


        delete_print_file
*        Remove Pick List and BOL print Requests
            readkey$ = "P" & str(cuscode$) & str(so$) & bol$
            call "DELETE" (#4, readkey$, 29%)
            readkey$ = "B" & str(cuscode$) & str(so$) & bol$
            call "DELETE" (#4, readkey$, 29%)
            return


        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            * --------------------------------------------------------- *~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35060: FMT                 /* FILE: SHPHDRS                           */~
            CH(9),          /* Customer Ship-to Address Identifier     */~
            CH(16),         /* Sales order number                      */~
            CH(3),          /* Bill of Lading Number                   */~
            CH(3),          /* Store Code                              */~
            CH(6),          /* Ship Date                               */~
            CH(6),          /* Shipping Carrier Code                   */~
            CH(20),         /* how ship information                    */~
            CH(20),         /* f.o.b. information                      */~
            2*CH(50),       /* Shipping Instructions                   */~
            CH(6),          /* Ship Date                               */~
            CH(20),         /* Freight/ Air Bill Number                */~
            PD(14,4),       /* Number of Cartons                       */~
            PD(14,4),       /* Shipment Weight                         */~
            PD(14,4),       /* freight amount                          */~
            CH(8),          /* Invoice Number                          */~
            CH(4),          /* Text ID                                 */~
            CH(1),          /* Export Order flag                       */~
            CH(1),          /* Export Approval flag                    */~
            CH(53)          /* Filler For Rest of Record               */

L35270: FMT                 /* FILE: SHPLINES                          */~
            CH(9),          /* Customer Ship-to Address Identifier     */~
            CH(16),         /* Sales order number                      */~
            CH(3),          /* Bill of Lading Number                   */~
            CH(3),          /* General purpose sequence number         */~
            PD(14,4),       /* Quantity scheduled or shipped           */~
            30*CH(6),       /* Lot Number                              */~
            30*PD(14,4),    /* Lot Quantities                          */~
            CH(4),          /* Text ID                                 */~
            CH(4),          /* Package quantity                        */~
            CH(8),          /* Package type                            */~
            CH(1),          /* Shipment Completed Flag                 */~
            CH(16),         /* Sales order number                      */~
            CH(3),          /* SO line number                          */~
            CH(105)         /* Filler                                  */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            * --------------------------------------------------------- *~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            lastlit$ = " "
            if lastso$ <> " " then lastlit$ = "Last SO/BOL: " &          ~
                              lastso$ & " / " & lastbol$
            line2$ = "Customer: " & cuscode$ & " (" & cusname$ & ")"
            if store$ <> " " then line2$ = line2$ & "  Store: " & store$
            str(line2$,62) = "SHPSCHIN: " & str(cms2v$,,8%)
            gosub setpf1

            init (hex(86)) lfac$()
            if fieldnr% = 0% then L40360
            init (hex(8c)) lfac$()
            if fieldnr% <> 1% then L40230
                lfac$(1) = hex(81)
                goto L40360
L40230:     for i% = 2% to 8%
                if enabled%(i%) = 1% then lfac$(i%) = hex(81)
                next i%
            if enabled%(6%) = 1% then lfac$(6%) = hex(80)

L40360: accept                                                           ~
            at (01,02), "Shipment Scheduling",                           ~
            at (01,30), fac(hex(8c)),  lastlit$                 , ch(35),~
            at (01,66), "Today:",                                        ~
            at (01,73), fac(hex(8c)),  date$                    , ch(08),~
            at (02,02), fac(hex(ac)),  line2$                   , ch(79),~
            at (04,02), fac(hex(94)),  errormsg$                , ch(79),~
                                                                         ~
            at (06,02), "Sales Order - BOL",                             ~
            at (06,30), fac(lfac$( 1)), so$                     , ch(16),~
            at (06,47), "-",                                             ~
            at (06,49), fac(lfac$( 1)), bol$                    , ch(03),~
                                                                         ~
            at (07,02), "Scheduled Ship Date",                           ~
            at (07,30), fac(lfac$( 2)), shipdate$               , ch(08),~
                                                                         ~
            at (08,02), "Carrier",                                       ~
            at (08,30), fac(lfac$( 3)), carrier$                , ch(06),~
            at (08,49), fac(hex(8c)),   carriername$            , ch(32),~
                                                                         ~
            at (09,02), "How Ship",                                      ~
            at (09,30), fac(lfac$( 4)), howship$                , ch(20),~
                                                                         ~
            at (10,02), "FOB",                                           ~
            at (10,30), fac(lfac$( 5)), fob$                    , ch(20),~
                                                                         ~
            at (11,02), "Shipping Instructions",                         ~
            at (11,30), fac(lfac$( 6)), instr$(1)               , ch(50),~
            at (12,30), fac(lfac$( 6)), instr$(2)               , ch(50),~
                                                                         ~
            at (13,02), "Print Pick List? (Y/N/I)",                      ~
            at (13,30), fac(lfac$( 7)), printpick$              , ch(01),~
                                                                         ~
            at (14,02), "Print BOL? (Y/N/I)",                            ~
            at (14,30), fac(lfac$( 8)), printbol$               , ch(01),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)),   pf$(1)                  , ch(79),~
            at (23,02), fac(hex(8c)),   pf$(2)                  , ch(79),~
            at (24,02), fac(hex(8c)),   pf$(3)                  , ch(79),~
                keys(pfkey$), key(keyhit%)


            if keyhit% <> 13 then L40830
                call "MANUAL" ("SHPSCHIN")
                goto L40360

L40830:     if keyhit% <> 15 then L40870
               call "PRNTSCRN"
               goto L40360

L40870:     if keyhit% <> 10 then L40950
                readkey$ = " "
                errormsg$ = hex(06) & "Select Sales Order To Schedule"
                call "GETCODE"(#5, readkey$, errormsg$, 0%, 0.46, f1%(5))
                if f1%(5) <> 1% then L40360
                     so$ = str(readkey$,10,16) : bol$ = " " : keyhit%=0%
                     return

L40950:     if keyhit% <> 14 then L41030
                readkey$ = " "
                call "GETCODE" (#1, readkey$, " ", 0%, 0.03, f1%(1))
                if f1%(1) <> 1% then L40360
                     so$  = str(readkey$,10,16)
                     bol$ = str(readkey$,26, 3) : keyhit% = 0%
                     return

L41030:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        setpf1
        if edit% = 2% then L41240         /* Input Mode                 */
           pf$(1) = "(1)Start Over                         (10)See Orde"&~
                    "rs on File   (13)Instructions"
           pf$(2) = "                 (4)Previous Field    (14)See BOLs"&~
                    " on File     (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)Exit Program"
           pfkey$ = hex(01ffff04ffffffffff0affff0d0e0f10ffffff00)
           if fieldnr% = 1% then L41190
                str(pf$(1),36,25), str(pf$(2),36,25) = " "
                str(pf$(3),64,16) = " "
                str(pfkey$,10,1), str(pfkey$,14,1) = hex(ff)
                str(pfkey$,16,1) = hex(ff)
L41190:    if fieldnr% > 2% then L41220
                str(pf$(2),18,18) = " "
                str(pfkey$, 4, 1) = hex(ff)
L41220:    return

L41240:  if fieldnr% > 0% then L41350     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "(2)Line Items                (26)Display Order Tex"&~
                    "t            (15)Print Screen"
           pf$(3) = "(12)Delete from Schedule     (28)Manage Inv Header"&~
                    " Text        (16)Save Data   "
           pfkey$ = hex(0102ffffffffffffffffff0c0dff0f10ff1a1c00191d)
           return

                                         /* Edit Mode- Field Enabled   */
L41350:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00ffff)
           return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            * --------------------------------------------------------- *~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(edit%)
            line2$ = "Customer: " & cuscode$ & "  BOL: " & so$ & "-" &   ~
                     bol$ & "  Store: " & store$
            str(line2$, 62) = "SHPSCHIN: " & str(cms2v$,,8%)
            if toggle% <> 0% then L42110
                str(field$())  = str(shipcode$())
                str(toggle$()) = str(avail$())
                goto L42130
L42110:     str(field$())  = str(seqnr$())
            str(toggle$()) = str(alloc$())
L42130:     gosub setpf2
            if errormsg$ <> " " then L42230
                if edit% = 1% then init(hex(86)) lfac$()
                if edit% = 1% then init(hex(86)) qfac$()
                if edit% = 1% then init(hex(84)) sfac$()
                if edit% = 1% then L42230
                     for c% = top% to  min(top% + 14%, maxlines%)
                          lfac$(c%-top%+1%) = hex(82)
                          qfac$(c%-top%+1%) = hex(82)
                          sfac$(c%-top%+1%) = hex(81)
                     next c%

L42230: accept                                                           ~
            at (01,02), "Shipment Scheduling",                           ~
            at (01,66), "Today:",                                        ~
            at (01,73), fac(hex(8c)),  date$                    , ch(08),~
            at (02,02), fac(hex(ac)),  line2$                   , ch(79),~
            at (04,02), fac(hex(94)),  errormsg$                , ch(79),~
                                                                         ~
            at (05,02), fac(hex(ac)), hdr1_11$                  , ch(03),~
            at (05,06), fac(hex(ac)), hdr2$                     , ch(25),~
            at (05,32), fac(hex(ac)), hdr3$                     , ch(08),~
            at (05,41), fac(hex(ac)), hdr4$                     , ch(03),~
            at (05,45), fac(hex(ac)), hdr6_7$                   , ch(10),~
            at (05,56), fac(hex(ac)), hdr8$                     , ch(10),~
            at (05,67), fac(hex(ac)), hdr9$                     , ch(04),~
            at (05,72), fac(hex(ac)), hdr10$                    , ch(08),~
                                                                         ~
            at (06,02), fac(hex(84)),   field$      (top% +  0%), ch(03),~
            at (07,02), fac(hex(84)),   field$      (top% +  1%), ch(03),~
            at (08,02), fac(hex(84)),   field$      (top% +  2%), ch(03),~
            at (09,02), fac(hex(84)),   field$      (top% +  3%), ch(03),~
            at (10,02), fac(hex(84)),   field$      (top% +  4%), ch(03),~
            at (11,02), fac(hex(84)),   field$      (top% +  5%), ch(03),~
            at (12,02), fac(hex(84)),   field$      (top% +  6%), ch(03),~
            at (13,02), fac(hex(84)),   field$      (top% +  7%), ch(03),~
            at (14,02), fac(hex(84)),   field$      (top% +  8%), ch(03),~
            at (15,02), fac(hex(84)),   field$      (top% +  9%), ch(03),~
            at (16,02), fac(hex(84)),   field$      (top% + 10%), ch(03),~
            at (17,02), fac(hex(84)),   field$      (top% + 11%), ch(03),~
            at (18,02), fac(hex(84)),   field$      (top% + 12%), ch(03),~
            at (19,02), fac(hex(84)),   field$      (top% + 13%), ch(03),~
            at (20,02), fac(hex(84)),   field$      (top% + 14%), ch(03),~
                                                                         ~
            at (06,06), fac(hex(84)), pn_or_desc$   (top% +  0%), ch(25),~
            at (07,06), fac(hex(84)), pn_or_desc$   (top% +  1%), ch(25),~
            at (08,06), fac(hex(84)), pn_or_desc$   (top% +  2%), ch(25),~
            at (09,06), fac(hex(84)), pn_or_desc$   (top% +  3%), ch(25),~
            at (10,06), fac(hex(84)), pn_or_desc$   (top% +  4%), ch(25),~
            at (11,06), fac(hex(84)), pn_or_desc$   (top% +  5%), ch(25),~
            at (12,06), fac(hex(84)), pn_or_desc$   (top% +  6%), ch(25),~
            at (13,06), fac(hex(84)), pn_or_desc$   (top% +  7%), ch(25),~
            at (14,06), fac(hex(84)), pn_or_desc$   (top% +  8%), ch(25),~
            at (15,06), fac(hex(84)), pn_or_desc$   (top% +  9%), ch(25),~
            at (16,06), fac(hex(84)), pn_or_desc$   (top% + 10%), ch(25),~
            at (17,06), fac(hex(84)), pn_or_desc$   (top% + 11%), ch(25),~
            at (18,06), fac(hex(84)), pn_or_desc$   (top% + 12%), ch(25),~
            at (19,06), fac(hex(84)), pn_or_desc$   (top% + 13%), ch(25),~
            at (20,06), fac(hex(84)), pn_or_desc$   (top% + 14%), ch(25),~
                                                                         ~
            at (06,32), fac(hex(84)),   reqd$       (top% +  0%), ch(08),~
            at (07,32), fac(hex(84)),   reqd$       (top% +  1%), ch(08),~
            at (08,32), fac(hex(84)),   reqd$       (top% +  2%), ch(08),~
            at (09,32), fac(hex(84)),   reqd$       (top% +  3%), ch(08),~
            at (10,32), fac(hex(84)),   reqd$       (top% +  4%), ch(08),~
            at (11,32), fac(hex(84)),   reqd$       (top% +  5%), ch(08),~
            at (12,32), fac(hex(84)),   reqd$       (top% +  6%), ch(08),~
            at (13,32), fac(hex(84)),   reqd$       (top% +  7%), ch(08),~
            at (14,32), fac(hex(84)),   reqd$       (top% +  8%), ch(08),~
            at (15,32), fac(hex(84)),   reqd$       (top% +  9%), ch(08),~
            at (16,32), fac(hex(84)),   reqd$       (top% + 10%), ch(08),~
            at (17,32), fac(hex(84)),   reqd$       (top% + 11%), ch(08),~
            at (18,32), fac(hex(84)),   reqd$       (top% + 12%), ch(08),~
            at (19,32), fac(hex(84)),   reqd$       (top% + 13%), ch(08),~
            at (20,32), fac(hex(84)),   reqd$       (top% + 14%), ch(08),~
                                                                         ~
            at (06,41), fac(hex(84)),   disp$       (top% +  0%), ch(03),~
            at (07,41), fac(hex(84)),   disp$       (top% +  1%), ch(03),~
            at (08,41), fac(hex(84)),   disp$       (top% +  2%), ch(03),~
            at (09,41), fac(hex(84)),   disp$       (top% +  3%), ch(03),~
            at (10,41), fac(hex(84)),   disp$       (top% +  4%), ch(03),~
            at (11,41), fac(hex(84)),   disp$       (top% +  5%), ch(03),~
            at (12,41), fac(hex(84)),   disp$       (top% +  6%), ch(03),~
            at (13,41), fac(hex(84)),   disp$       (top% +  7%), ch(03),~
            at (14,41), fac(hex(84)),   disp$       (top% +  8%), ch(03),~
            at (15,41), fac(hex(84)),   disp$       (top% +  9%), ch(03),~
            at (16,41), fac(hex(84)),   disp$       (top% + 10%), ch(03),~
            at (17,41), fac(hex(84)),   disp$       (top% + 11%), ch(03),~
            at (18,41), fac(hex(84)),   disp$       (top% + 12%), ch(03),~
            at (19,41), fac(hex(84)),   disp$       (top% + 13%), ch(03),~
            at (20,41), fac(hex(84)),   disp$       (top% + 14%), ch(03),~
                                                                         ~
            at (06,45), fac(hex(84)),   toggle$     (top% +  0%), ch(10),~
            at (07,45), fac(hex(84)),   toggle$     (top% +  1%), ch(10),~
            at (08,45), fac(hex(84)),   toggle$     (top% +  2%), ch(10),~
            at (09,45), fac(hex(84)),   toggle$     (top% +  3%), ch(10),~
            at (10,45), fac(hex(84)),   toggle$     (top% +  4%), ch(10),~
            at (11,45), fac(hex(84)),   toggle$     (top% +  5%), ch(10),~
            at (12,45), fac(hex(84)),   toggle$     (top% +  6%), ch(10),~
            at (13,45), fac(hex(84)),   toggle$     (top% +  7%), ch(10),~
            at (14,45), fac(hex(84)),   toggle$     (top% +  8%), ch(10),~
            at (15,45), fac(hex(84)),   toggle$     (top% +  9%), ch(10),~
            at (16,45), fac(hex(84)),   toggle$     (top% + 10%), ch(10),~
            at (17,45), fac(hex(84)),   toggle$     (top% + 11%), ch(10),~
            at (18,45), fac(hex(84)),   toggle$     (top% + 12%), ch(10),~
            at (19,45), fac(hex(84)),   toggle$     (top% + 13%), ch(10),~
            at (20,45), fac(hex(84)),   toggle$     (top% + 14%), ch(10),~
                                                                         ~
            at (06,56), fac(lfac$( 1)), schld$      (top% +  0%), ch(10),~
            at (07,56), fac(lfac$( 2)), schld$      (top% +  1%), ch(10),~
            at (08,56), fac(lfac$( 3)), schld$      (top% +  2%), ch(10),~
            at (09,56), fac(lfac$( 4)), schld$      (top% +  3%), ch(10),~
            at (10,56), fac(lfac$( 5)), schld$      (top% +  4%), ch(10),~
            at (11,56), fac(lfac$( 6)), schld$      (top% +  5%), ch(10),~
            at (12,56), fac(lfac$( 7)), schld$      (top% +  6%), ch(10),~
            at (13,56), fac(lfac$( 8)), schld$      (top% +  7%), ch(10),~
            at (14,56), fac(lfac$( 9)), schld$      (top% +  8%), ch(10),~
            at (15,56), fac(lfac$(10)), schld$      (top% +  9%), ch(10),~
            at (16,56), fac(lfac$(11)), schld$      (top% + 10%), ch(10),~
            at (17,56), fac(lfac$(12)), schld$      (top% + 11%), ch(10),~
            at (18,56), fac(lfac$(13)), schld$      (top% + 12%), ch(10),~
            at (19,56), fac(lfac$(14)), schld$      (top% + 13%), ch(10),~
            at (20,56), fac(lfac$(15)), schld$      (top% + 14%), ch(10),~
                                                                         ~
            at (06,67), fac(qfac$( 1)), pkqty$      (top% +  0%), ch(04),~
            at (07,67), fac(qfac$( 2)), pkqty$      (top% +  1%), ch(04),~
            at (08,67), fac(qfac$( 3)), pkqty$      (top% +  2%), ch(04),~
            at (09,67), fac(qfac$( 4)), pkqty$      (top% +  3%), ch(04),~
            at (10,67), fac(qfac$( 5)), pkqty$      (top% +  4%), ch(04),~
            at (11,67), fac(qfac$( 6)), pkqty$      (top% +  5%), ch(04),~
            at (12,67), fac(qfac$( 7)), pkqty$      (top% +  6%), ch(04),~
            at (13,67), fac(qfac$( 8)), pkqty$      (top% +  7%), ch(04),~
            at (14,67), fac(qfac$( 9)), pkqty$      (top% +  8%), ch(04),~
            at (15,67), fac(qfac$(10)), pkqty$      (top% +  9%), ch(04),~
            at (16,67), fac(qfac$(11)), pkqty$      (top% + 10%), ch(04),~
            at (17,67), fac(qfac$(12)), pkqty$      (top% + 11%), ch(04),~
            at (18,67), fac(qfac$(13)), pkqty$      (top% + 12%), ch(04),~
            at (19,67), fac(qfac$(14)), pkqty$      (top% + 13%), ch(04),~
            at (20,67), fac(qfac$(15)), pkqty$      (top% + 14%), ch(04),~
                                                                         ~
            at (06,72), fac(sfac$( 1)), pkpak$      (top% +  0%), ch(08),~
            at (07,72), fac(sfac$( 2)), pkpak$      (top% +  1%), ch(08),~
            at (08,72), fac(sfac$( 3)), pkpak$      (top% +  2%), ch(08),~
            at (09,72), fac(sfac$( 4)), pkpak$      (top% +  3%), ch(08),~
            at (10,72), fac(sfac$( 5)), pkpak$      (top% +  4%), ch(08),~
            at (11,72), fac(sfac$( 6)), pkpak$      (top% +  5%), ch(08),~
            at (12,72), fac(sfac$( 7)), pkpak$      (top% +  6%), ch(08),~
            at (13,72), fac(sfac$( 8)), pkpak$      (top% +  7%), ch(08),~
            at (14,72), fac(sfac$( 9)), pkpak$      (top% +  8%), ch(08),~
            at (15,72), fac(sfac$(10)), pkpak$      (top% +  9%), ch(08),~
            at (16,72), fac(sfac$(11)), pkpak$      (top% + 10%), ch(08),~
            at (17,72), fac(sfac$(12)), pkpak$      (top% + 11%), ch(08),~
            at (18,72), fac(sfac$(13)), pkpak$      (top% + 12%), ch(08),~
            at (19,72), fac(sfac$(14)), pkpak$      (top% + 13%), ch(08),~
            at (20,72), fac(sfac$(15)), pkpak$      (top% + 14%), ch(08),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)),   pf$(1)                  , ch(79),~
            at (23,02), fac(hex(8c)),   pf$(2)                  , ch(79),~
            at (24,02), fac(hex(8c)),   pf$(3)                  , ch(79),~
                keys(pfkey$), key(keyhit%)

            if keyhit% <> 14% then L43990
                if errflag% = 0% then L43920
                errormsg$ = " " : errflag% = 0%
L43920:         mat schld = zer : init (" ") schld$()
                for n% = 1% to maxlines%
                     schld$(n%) = alloc$(n%)
                     convert alloc$(n%) to schld(n%), data goto L43960
L43960:         next n%
                return

L43990:     if keyhit% <> 27% then L44100
                if toggle% = 0% then goto L44050
                     toggle% = 0%
                     str(toggle$()) = str(avail$())
                     str(field$())  = str(shipcode$())
                     hdr6_7$ = hdr6$ : hdr1_11$ = hdr11$
                     goto L42230
L44050:         toggle% = 1%
                hdr6_7$ = hdr7$ :  hdr1_11$ = hdr1$
                str(field$())  = str(seqnr$())
                str(toggle$()) = str(alloc$())
                goto L42230

L44100:     if keyhit% <> 13 then L44140
                call "MANUAL" ("SHPSCHIN")
                goto L42230

L44140:     if keyhit% <> 15 then L44180
               call "PRNTSCRN"
               goto L42230

L44180:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        setpf2
        if edit% = 2% then L44380         /* Display Mode               */
           pf$(1) = "(1)S/Over (8)Dsp PIP & ATC (10)Show Invtry (26)Dis"&~
                    "p SO Text    (13)Instructions"
           pf$(2) = "(2)First (4)Prev (6)Down   (11)Tgl PN/Desc (27)Sq/"&~
                    "Pr Ucmt/Aloc (15)Print Screen"
           pf$(3) = "(3)Lst (5)Nxt (7)Up (9)Hdr (22)Flg Complet (28)INV"&~
                    " Line Text   (16)Save Data   "
           pfkey$ = hex(0102040603050708090a0bff0dff0f10161aff001b1c)
           if top% > 1% then L44330
                str(pf$(2),,26) = " "  :  str(pfkey$, 2, 3) = hex(ff)
L44330:    if top% + 14% < maxlines% then L44350
                str(pf$(3),,20) = " "  :  str(pfkey$, 5, 3) = hex(ff)
L44350:    return

                                         /* Edit Mode- Field Enabled   */
L44380:    pf$(1) = "(1)Start Over                             (14)Set "&~
                    "Sched=Alloc  (13)Instructions"
           pf$(2) = "                                          (27)Sq/P"&~
                    "r Ucmt/Aloc  (15)Print Screen"
           pf$(3) = "                          (11)Tgl PN/Dsc          "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffff0bff0d0e0fffffffff001bff)
           return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50180,         /* Sales Order - BOL*/~
                                    L50270          /* All the Rest     */
                  return

L50180
*        Sales Order - BOL                     SO$
            if so$ <> " " then L50220
                errormsg$ = "Sales Order may not be left blank."
                return
L50220: REM SOMETHING
            gosub load_data
            if bol$ = "NEW" or errormsg$ <> " " then return
                return clear all
                goto edit_header

L50270
*        Scheduled Ship Date                   SHIPDATE$
            call "DATEOK" (shipdate$, u3%, errormsg$)
            if errormsg$ <> " " then return
                call "DATUNFMT" (shipdate$)
                if shipdate$ >= date then L50350
                     errormsg$ = "Cannot schedule prior to today."
                     call "DATEFMT" (shipdate$)
                     return
L50350:         call "DATEFMT" (shipdate$)
                if shipdate$ = shipdatelast$ then L50430
                     set%    = 0%
                     if bol$ = "NEW" and edit% = 1% then set% = 1%
                     gosub calc_allocated
                shipdatelast$ = shipdate$

L50430
*        Carrier                               CARRIER$
            readkey$ = "CARRIERS " & carrier$
            carriername$ = hex(06) & "Select Carrier (PF-16 if none)."
            call "PLOWCODE" (#3, readkey$, carriername$, 9%, 0.30, f1%(3))
            if f1%(3) = 1% then L50490
                carriername$ = " " : goto L50520
L50490:     carrier$ = str(readkey$,10)

L50520
*        How Ship                              HOWSHIP$

*        FOB                                   FOB$

*        Shipping Instructions                 INSTR$()

*        Print Pick List                       PRINTPICK$
            if pos("YIN" = printpick$) <> 0% then L50670
                errormsg$ = "Pick List Printing: Y = Print  I = Print " &~
                            "Immediately  N = Don't Print"

L50670
*        Print BOL                             PRINTBOL$
            if pos("YIN" = printbol$) <> 0% then return
                errormsg$ = "BOL Printing: Y = Print  I = Print " &      ~
                            "Immediately  N = Don't Print"
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152
            errormsg$ = " "
            init (hex(8c)) lfac$()
            init (hex(8c)) sfac$()
            init (hex(8c)) qfac$()

*        Scheduled Quantities                  SCHLD$
            for c% = top% to  min(top% + 14%, maxlines%)
                if schld$(c%) = " " then schld$(c%) = "0"
                convert schld$(c%) to schld(c%), data goto L51150
                goto L51180
L51150:              errormsg$ = "Invalid Numeric Entry."
                     lfac$(c% - top% + 1%) = hex(82)
                     return
L51180:         if schld(c%) <= avail(c%) then L51230
                if overship$ = "Y" then goto L51215
                     errormsg$ = "Scheduled Quantity cannot exceed" &    ~
                                 " Uncommitted Quantity."
                     errflag% = 1%
                     lfac$(c% - top% + 1%) = hex(82)
                     return
L51215:          if over(c%) = schld(c%) then L51230
                 tot_ship = committed(c%) + schld(c%)
L51220:          call "SHPOVRSB" (part$(c%), seqnr$(c%), order(c%),      ~
                       tot_ship, def_percent(c%), def_unit(c%),          ~
                       sys_def_percent, u3%)
                 if u3% = 16% then L51230
                 if u3% <> 1% then L51220
                     errormsg$ = "Please reenter scheduled quantity."
                     errflag% = 1%
                     lfac$(c% - top% + 1%) = hex(82)
                     return
L51230:         call "CONVERT" (schld(c%), 2.2, schld$(c%))
                over(c%) = schld(c%)
            next c%

*        Package Quantities                    PKQTY$
            for c% = top% to  min(top% + 14%, maxlines%)
                if pkqty$(c%) = " " then pkqty$(c%) = "0"
                convert pkqty$(c%) to pkqty(c%), data goto L51310
                if pkqty(c%) >= 0 then L51340
                     errormsg$ = "Packing Quantity cannot be negative."
                     goto L51320
L51310:              errormsg$ = "Invalid Numeric Entry."
L51320:              qfac$(c% - top% + 1%) = hex(82)
                     return
L51340:         call "CONVERT" (pkqty(c%), 0.0, pkqty$(c%))
                if pkqty$(c%) = "   0" then pkqty$(c%) = " "
            next c%

*        Package Types                         PKPAK$
            for c% = top% to  min(top% + 14%, maxlines%)
                if pkqty$(c%) = " " and pkpak$(c%) = " " then L51610
                if schld(c%) <> 0 then goto L51450
                if pkpak$(c%) = " " then goto L51610
                     errormsg$ = "Pack Type invalid if zero scheduled."
                     sfac$(c% - top% + 1%) = hex(81)
                     return
L51450:         if pkpak%(c%) = 1% then goto L51560
                if pkpak%(c%) = 2% then goto L51620
                readkey$ = " "
                str(readkey$,1,9) = "PACKUNIT "
                str(readkey$,10,8) = pkpak$(c%)
                call "READ100" (#3, readkey$, f1%(3))
                if f1%(3) = 1% then goto L51610
                     pkpak%(c%) = 1%       /* Package Type not valid */
                     errormsg$ = "Package Type not in table."
                     sfac$(c% - top% + 1%) = hex(81)
                     return
L51560:         readkey$ = "PACKUNIT " & pkpak$(c%)
                call "PLOWCODE" (#3, readkey$, paktypname$, 9%, .3,      ~
                     f1%(3))
                if f1%(3) = 0% then goto L51610
                     pkpak$(c%) = str(readkey$,10)
L51610:         pkpak%(c%) = 2%            /* Package Type ok */
L51620:         next c%
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * Terminates execution (files closed automatically).        *~
            * --------------------------------------------------------- *~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
