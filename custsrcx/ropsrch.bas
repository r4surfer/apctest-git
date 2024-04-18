        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  RRRR    OOO   PPPP    SSS   RRRR    CCC   H   H          *~
            *  R   R  O   O  P   P  S      R   R  C   C  H   H          *~
            *  RRRR   O   O  PPPP    SSS   RRRR   C      HHHHH          *~
            *  R   R  O   O  P          S  R   R  C   C  H   H          *~
            *  R   R   OOO   P       SSS   R   R   CCC   H   H          *~
            *                                                           *~
            *    SPECIAL LINE AT - (4035)                               *~
            *-----------------------------------------------------------*~
            * ROPSRCH  - This Program will Search a specified part range*~
            *            and identify all re-order point intrusions.    *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/21/87 ! Original                                 ! LKM *~
            * 11/24/87 ! Fixed FS95 bug                           ! MJB *~
            * 05/06/88 ! Changed Part Class Range to CH(4)        ! JDH *~
            * 06/08/88 ! Major clean up of non-std items          ! JDH *~
            * 08/17/89 ! Modified for APC to show PO on scr/rpt   ! CLN *~
            * 02/12/98 ! Condition Code of (EWD) Version for      ! RHH *~
            *          ! Upgrade to R6.04.03                      !     *~
            *          ! Change Record Length of BOMSPEC          !     *~
            * 10/12/00 ! Mod to add vendor_code and buyer code    ! CMG *~
            *          !     as part of selection criteria. (EWD001)    *~
            * 09/23/04 ! (AWD002) Mod so ROP will not count alpa  ! CMG *~
            *          !   store as part of on-hand               !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        com planflags$(25)20,                                            ~
            yymmdd$(490)6,                                               ~
            eff$(490)3,                  /* EFF, PLT WORK ARRAY        */~
            oldcompplowkey$(100)31,      /* FOR PHANTOM LOGIC          */~
            pip%(490),                                                   ~
            cumf%(490),                                                  ~
            awca%(490),                  /* CONCURRENT WC AVAILABILITY */~
            awcu1%(490),                 /* 1ST CONCURRENT USED        */~
            awcu2%(490),                 /* 2ND CONCURRENT USED        */~
            awcu3%(490),                 /* 3RD CONCURRENT USED        */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            phfact(101),                                                 ~
                               /* THE ABOVE ELEMENTS CANNOT BE CHANGED */~
            cpart$   (1000)25,                                           ~
            intagnr$ (1000)19,                                           ~
            outtagnr$(1000)19,                                           ~
            rte$     (1000)3,            /* WHICH ROUTE TO USE         */~
            bom$     (1000)3,            /* WHICH BOM TO USE           */~
            ed%      (1000),                                             ~
            sd%      (1000),                                             ~
            parline% (1000),             /* PARENT LINE NUMBER         */~
            action%  (1000),             /* ACTION TO TAKE             */~
            ldt%     (1000),             /* LEADTIME ARRAY             */~
            moq%     (1000),             /* MOQ                        */~
            type%    (1000),             /* PART TYPE                  */~
            qtyu     (1000),             /* QTY USED (NEEDED)          */~
            qtyp     (1000),             /* QTY TO PROCURE             */~
                               /* THE ABOVE ELEMENTS ARE THE MATERIALS */~
            wc$(2000)4,                  /* WORK CENTER                */~
            wl$(2000)1,                  /* JUST FOR SUMMARY REPORT    */~
            wa$(2000)1,                  /* IN CASE OF ARRAY OVERFLOW  */~
            ws$(2000)5,                  /* WC STEP #                  */~
            wl%(2000),                   /* LINE STACK                 */~
            du%(2000),                   /* DATE USED ARRAY STACK      */~
            au%(2000),                   /* AMOUNT USED ARRAY STACK    */~
            su%(2000),                   /* SET UP TIME TODAY          */~
            wa%(2000),                   /* WC ALTERNATE SEQ NO.       */~
                               /* THESE ARE THE WORK CENTER ARRAYS     */~
            rtestep$(255)200,            /* THE STEP AS A STRING       */~
            step$   (255)5,              /* STEP                       */~
            yld     (255),               /* STEP YIELD                 */~
            pd%     (255),               /* WC STEP START DATE(SPL PCK)*/~
            mmx%    (255),               /* START OF RTE STEP          */~
            xsd%    (255)                /* START OF RTE STEP          */~
                               /* THESE ARE FOR ROUTE STEPS            */~

        dim                                                              ~
            apprv$1,                     /* Approved                   */~
            ar_flag%(12),                 /* Flag for Asterisk         */~
            begcat$4,                    /* Part Category              */~
            begclass$4,                  /* Part Class                 */~
            begpart$25,                  /* Part Number                */~
            begtype$3,                   /* Part Type                  */~
            bfac$(12)1,                  /* Field Attribute Characters */~
            box$(12)1,                   /* For select screen          */~
            cat$4,                       /* Category for report        */~
            cd%(12),                     /* Completion Date            */~
            company$50,                  /* For Report Header          */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            day1$6,                      /* DATE FOR PIPIN             */~
            date_hdr$7,                  /* Screen header              */~
            disporder$26,                /* Display Priority Sequence  */~
            dmndtype_hdr$1,              /* Screen header              */~
            dc$16,                       /* For PLANSUB                */~
            dl$3,                        /* FOR PLANSUB                */~
            dp$(12)1,                    /* FOR PLANSUB                */~
            dt$(12)1,                    /* Demand type                */~
            duein$10,                    /* PIPIN qty for report       */~
            edtmessage$79,               /* Edit screen message        */~
            endcat$4,                    /* Ending category            */~
            endclass$4,                  /* Ending Class               */~
            endpart$25,                  /* Ending part                */~
            endtype$3,                   /* Ending type                */~
            eoq(12),                     /* Economic order Qty         */~
            eoq$10,                      /* Economic order qty for rpt */~
            eoq$(12)10,                  /* For PLANSUB                */~
            errorline$79,                /* For PLANSUB                */~
            errormsg$79,                 /* Error message              */~
            firstcat$4,                  /* First category             */~
            firstclass$4,                /* First class                */~
            firstpart$25,                /* First part to plow         */~
            firsttype$3,                 /* First type                 */~
/*EWD001*/  firstven$9,                  /* First Vendor Code          */~
/*EWD001*/  firstbuy$3,                  /* First Buyer  Code          */~  
            from$4,                      /* Literal FROM for screen    */~
            hits$4,                      /* # selected for planning    */~
            hitsplnd$4,                  /* # actually planned         */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lastcat$4,                   /* For testing range          */~
            lastclass$4,                 /* For testing range          */~
            lastpart$25,                 /* For testing range          */~
            lasttype$3,                  /* For testing range          */~
/*EWD001*/  lastven$9,                   /* For testing range          */~
/*EWD001*/  lastbuy$3,                   /* For testing range          */~            
            late$1,                      /* Planned Late               */~
            leadtime$3,                  /* Leadtime                   */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            msg1$64,                     /* For database error         */~
            oh$10,                       /* On hand for report         */~
            oh$(12)10,                   /* On hand                    */~
            onhand_hdr$10,               /* Screen header              */~
            part$(12)25,                 /* Part number                */~
            partdescr$32,                /* Part Description (report)  */~
            part_descr$(12)30,           /* Part Description (screen)  */~
            part_dsply$(12)30,           /* Part/Description variable  */~
            part_hdr$30,                 /* Screen header              */~
            pclass$4,                    /* Part Class for report      */~
            pdate1$8,                    /* Date field for report      */~
            pdate2$8,                    /* Date field for report      */~
            plandate$8,                                                  ~
            plandate$(12)8,              /* Starting date              */~
            pf7$20,                      /* PF1 Screen Literal         */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf10$43,                     /* PF 3 Screen Literal        */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            pf8$13,                      /* PF 8 Screen Literal        */~
            pfkeys$21,                   /* Valid PF keys              */~
            pfkeys1$21,                  /* Valid PF keys for 1st Scrn */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            plowkey2$99,                 /* Miscellaneous Read/Plow Key*/~
            porder$26,                   /* Priority Sequence          */~
            priority$1,                  /* Demand Priority            */~
            ponum$19,                    /* PO NUMBER FROM PIPIN       */~
            pipqty$8,                    /* QTY FROM PIPIN             */~
            pipdate$8,                   /* DATE FROM PIPIN            */~
            poarray$(50,3)19,            /* ARRAY FOR PO, QTY, DATE    */~
            priority_hdr$1,              /* Screen header              */~
            qty_hdr$10,                  /* Screen header              */~
            quan(1),                     /* For HNYTOTSB               */~
            rec$100,                     /* Hold area for ROPHOLD      */~
            readkey$99,                  /* Miscellaneous Read Key     */~
            rec_nbr%(12),                /* Record Number              */~
            recs$8,                      /* # of recs to open ROPHOLD  */~
            rop$10,                      /* Re-order point for report  */~
            rop$(12)10,                  /* Re-order point             */~
            rop_hdr$10,                  /* Screen header              */~
            to$,                         /* Literal TO for screen      */~
            totpart$6,                   /* Total parts printed        */~
            type$3,                      /* Part Type for report       */~
            userid$3,                    /* Current User Id            */~
/*EWD001*/  vendor_code$9,               /* Read Vendor Code           */~
/*EWD001*/  begven$9,                    /* Begin Selection Vendor Code*/~
/*EWD001*/  endven$9,                    /* End Selection Vendor Code  */~
/*EWD001*/  buy_code$3,                  /* Read Buyer  Code           */~
/*EWD001*/  begbuy$3,                    /* Begin Selection Buyer  Code*/~
/*EWD001*/  endbuy$3                     /* End Selection Buyer  Code  */

        dim f2%(32),                     /* = 0 if the file is open    */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            axd$(64)4,                   /* For OPENFILE               */~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.01.00 07/01/88 General Release R5.01.00        "
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
            * # 1 ! HNYMASTR ! Inventory Master File                    *~
            * # 2 ! ROPHNY   ! File containing part specific ROP data   *~
            * # 3 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * # 4 ! PIPIN    ! Planned inventory additions detail       *~
            * # 6 ! HNYALTRS ! For PLANSUB                              *~
            * # 7 ! BOMMASTR ! For PLANSUB                              *~
            * # 8 ! PIPMASTR ! For PLANSUB                              *~
            * # 9 ! RTEMASTR ! For PLANSUB                              *~
            * #10 ! JBCROSS2 ! For PLANSUB                              *~
            * #11 ! WCMASTR  ! For PLANSUB                              *~
            * #12 ! WCOUT    ! For PLANSUB                              *~
            * #13 ! PIPOUT   ! For PLANSUB                              *~
            * #14 ! SFMASTR2 ! For PLANSUB                              *~
            * #15 ! SFCUM2   ! For PLANSUB                              *~
            * #16 ! PIPCROSS ! For PLANSUB                              *~
            * #17 ! ENGMASTR ! For PLANSUB                              *~
            * #18 ! BOMSPEC  ! For PLANSUB                              *~
            * #19 ! JBPIPXRF ! For PLANSUB                              *~
            * #20 ! RTEALTRS ! For PLANSUB                              *~
            * #21 ! WORK1    ! For PLANSUB                              *~
            * #22 ! WORK2    ! For PLANSUB                              *~
            * #23 ! SYSFILE2 ! For PIPINDEX                             *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select # 2, "ROPHNY",                                        ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  104, keylen =   4, dup

            select # 4, "PIPIN",                                         ~
                        varc,     indexed,  recsize =   60,              ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48

            select # 6, "HNYALTRS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =    1, keylen =  33

            select # 7, "BOMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56

            select #8,  "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26

            select #9,  "RTEMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  400,                                  ~
                        keypos =    5, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  35

            select #10, "JBCROSS2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   94,                                  ~
                        keypos =   29, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  47,         ~
                            key  2, keypos =   48, keylen =  47

            select #11, "WCMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6

            select #12, "WCOUT",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   68,                                  ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =    1, keylen =  27

            select #13, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37

            select #14, "SFMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    1, keylen =  25

            select #15, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =  25

            select #16, "PIPCROSS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =  71,                     ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33

            select #17, "ENGMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos =    1, keylen =  29

            select #18, "BOMSPEC",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =   26, keylen =  54,                     ~
                        alt key  1, keypos =   57, keylen =  23

            select #19, "JBPIPXRF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   63,                                  ~
                        keypos =    1, keylen =  63,                     ~
                        alt key  1, keypos =   45, keylen =  19

            select #20, "RTEALTRS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =  34

            select #21, "WORK1",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos = 1, keylen = 34

            select #22, "WORK2",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 120,                                   ~
                        keypos = 31, keylen = 8,                         ~
                        alt key 1, keypos = 1, keylen = 29,              ~
                            key 2, keypos = 35, keylen= 4,               ~
                            key 3, keypos = 30, keylen= 9

            select #23, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

           select #24, "CALMASTR", varc, indexed,                        ~
                     recsize = 1962, keypos = 1   , keylen = 2

           select #25, "ROPHOLD", varc, indexed,                         ~
                     recsize = 100,  keypos = 3   , keylen = 4,          ~
                     alt key 1, keypos = 1, keylen = 6,                  ~
                         key 2, keypos = 2, keylen = 5

            select #26, "DEMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  123,                                  ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28

            select #27, "WORKFILE",                                      ~
                        varc, indexed, recsize = 153,                    ~
                        keypos =  1, keylen = 19                         ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1),100%, rslt$( 1))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4),100%, rslt$( 4))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6),100%, rslt$( 6))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7),100%, rslt$( 7))
            call "OPENCHCK" (# 8, fs%( 8), f2%( 8),100%, rslt$( 8))
            call "OPENCHCK" (# 9, fs%( 9), f2%( 9),100%, rslt$( 9))
            call "OPENCHCK" (#10, fs%(10), f2%(10),100%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11),100%, rslt$(11))
            call "OPENCHCK" (#12, fs%(12), f2%(12),100%, rslt$(12))
            call "OPENCHCK" (#13, fs%(13), f2%(13),100%, rslt$(13))
            call "OPENCHCK" (#14, fs%(14), f2%(14),100%, rslt$(14))
            call "OPENCHCK" (#15, fs%(15), f2%(15),100%, rslt$(15))
            call "OPENCHCK" (#16, fs%(16), f2%(16),100%, rslt$(16))
            call "OPENCHCK" (#17, fs%(17), f2%(17),100%, rslt$(17))
            call "OPENCHCK" (#18, fs%(18), f2%(18),100%, rslt$(18))
            call "OPENCHCK" (#19, fs%(19), f2%(19),100%, rslt$(19))
            call "OPENCHCK" (#20, fs%(20), f2%(20),100%, rslt$(20))
            call "OPENCHCK" (#23, fs%(20), f2%(20),100%, rslt$(20))
            call "OPENCHCK" (#24, fs%(24), f2%(24),100%, rslt$(24))
            call "OPENCHCK" (#26, fs%(26), f2%(26), 100%, " ")

            call "OPENCHCK" (#25, fs%(25), f2%(25), 0%, rslt$(25))
            if fs%(25) <> 0 then call "FILEBGON" addr(#25)
               fs%(25) = 0

            if fs%(24) < 0 then L65000
            rslt$(2) = "REQUIRED"
            f2%(2) = 1
            call "OPENFILE" (# 2, "IO   ", f2%( 2), rslt$( 2), axd$(2))
            if f2%(2) <> 0 then exit_program
            get rslt$(2) using L04046, recs%
L04046:     FMT POS(17), BI(4)
            recs% = recs% / 10
            convert recs% to recs$, pic (00000000)
            hit = 0
            gosub loadcalendar
            if hit > 0 then L65000

            if min(fs%()) < 0% then exit_program

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

            from$ = "From"
            to$ = "To"
            str(line2$,62%) = "ROPSRCH : " & str(cms2v$,,8%)
            msg1$ = "Part master record does not exist for:"
            part_hdr$ = "Part Number"
            str(onhand_hdr$,4,7) = "On Hand"
            str(rop_hdr$,8,3) = "ROP"
            priority_hdr$ = "P"
            dmndtype_hdr$ = "T"
            date_hdr$ = "Date In"
            str(qty_hdr$,4,7) = "EOQ QTY"
            stdate% = 0
            porder$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            ar_flag% = 0%

            call "READ100" (#23, "PLANNING SYSTEM FLAG", f1%(23))
            if f1%(23)=0 then exit_program
            get #23, using L09270, str(planflags$(),1,480)
L09270:         FMT XX(20), CH(480)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$ = " " : pf5$ = " " : pf16$ = "(16)Exit Program"
            pfkeys$ = hex(0001ff04ff02ffffffff0bff0d0f10)
            pfkeys1$ = hex(08090a0d0f10)
            gosub check_rophold
            gosub L29000

            gosub initial_screen
            if keyhit%  = 16 then exit_program
            if keyhit%  = 10 then L10400
               if tot_rec% < 1 then L10211
L10160:           kh% = 0
                  call "ASKUSER" (kh%, "Proceed?", "Results of the previo~
        ~us search will be destroyed.", "Press RETURN to continue or PF16 ~
        ~to abort.")
                  if kh% = 16 then inputmode
                  if kh% <> 0 then L10160
L10211:     if keyhit% <> 8 then L10260
               mode = 1
               str(line2$,1,16) = "Interactive Mode"
               goto input_parameters

L10260:     if keyhit% <>  9 then L10400
               mode = 2
               str(line2$,1,16) = "Report Mode     "
               goto input_parameters

L10400:     mode = 3
            str(line2$,1,16) = "Approval Mode   "
            goto main_logic

        input_parameters
            errormsg$ = " " : pf16$ = "(16)Return"
            for fieldnr% = 1 to  6
L10470:         if fieldnr% > 1 then L10476
                pf4$ = "                 " : str(pfkeys$,4,1) = hex(ff)
                  goto L10480
L10476:         pf4$ = "(4)Previous Field" : str(pfkeys$,4,1) = hex(04)
L10480:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10600
L10500:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10580
L10530:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if fieldnr% = 1% then L10470
                         if enabled% = 1% then L10500
                         goto L10530
L10580:               if keyhit% = 16 and fieldnr% = 1 then inputmode
                      if keyhit% <>  0 then       L10500
L10600:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10500
            next fieldnr%
            goto editpg1

        check_rophold
            call "OPENCHCK" (#25, fs%(25), f2%(25), 0%, rslt$(25))
            if fs%(25) < 1 then L10735
               if tot_rec% > 0 then L10700
                   tot_rec% = val(str(rslt$(25),17), 4)
L10700:            pf10$ = "PF10) Manage Results of Previous Search   "
                   str(pfkeys1$,3,1) = hex(0a)
                   return

L10735:        pf10$ = " "
               str(pfkeys1$,3,1) = hex(ff)
               return

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            pf4$  = " "
            pf5$  = " "
            pf16$ = "(16)Search"
            if mode = 2 then pf16$ = "(16)Print Report"
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       main_logic
                  if keyhit% <>  0 then       editpg1
            fieldnr% = cursor%(1) - 7
            if fieldnr% < 1 or fieldnr% >  6 then editpg1
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
                  pf4$, pf5$, pf16$ = " "
L11220:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11220
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11220
                  lastfieldnr% = fieldnr%
            goto editpg1

        REM *************************************************************~
            *                    M A I N   L O G I C                    *~
            *************************************************************~

        main_logic
            errflag = 0
            on mode gosub interactive_mode ,                             ~
                          report_mode,                                   ~
                          approval_mode

            goto inputmode

        interactive_mode
            gosub search_file
            if tot_rec% > 0 then gosub approval_mode
            return

        report_mode
            totpart, page% = 0
            runtime$ = " "
            call "TIME" (runtime$)
            call "COMPNAME" (12%, company$, u3%)
            select printer(134)
            call "SETPRNT" ("ROP005", " ", 0%, 0%)
            gosub search_file
            print
            call "CONVERT" (totpart, 0.0, totpart$)
            print using L35090, totpart$
            print
            print "** END REPORT **"
            call "SETPRNT" ("ROP005", " ", 0%, 1%)
            close printer
            return

        approval_mode
            gosub display_control
            if tot_rec% > 0 then L12183
               call "FILEBGON" (#25)
               return
L12183:     keyhit% = 99%
            gosub global_change
            return

        call_pipindex
            call "PIPINDEX" (#23, date, today%, ret%)
            if ret% = 0 then return
               errormsg$ = "Today's date is not within the planning calen~
        ~dar"
               return

        search_file
            call "FILEBGON" (#25)
            gosub call_pipindex
            plowkey$ = str(firstpart$)
            tot_rec% = 0
            call "SHOSTAT" ("Searching for ROP intrusions...")
            gosub find_rop_intrusions
            if tot_rec% > 0 then L12400
               call "FILEBGON" (#25)
               errflag = 1
               errormsg$ = "No Re-order Point Intrusions were found."
               return
L12400:     close #25
            return

        find_rop_intrusions
               ar_flag% = 0%
               init(" ") poarray$()
               cnt% = 0%
L12440:        call "PLOWNEXT" (#2, plowkey$, 0%, f1%(2))
               if f1%(2) <> 1 then return
               if str(plowkey$,1,25) <= lastpart$ then L12480
                  f1%(2) = 0
L12480:        get #2 using L12490, rop, eoq
L12490:        FMT POS(26), 2*PD(14,4)
               if eoq = 0 then find_rop_intrusions
               call "READ100" (#1, plowkey$, f1%(1))
               if f1%(1) = 1 then L12580
                  msg1$ = "PART MASTER RECORD DOES NOT EXIST FOR:" &     ~
                          str(plowkey$,1,25)
                  gosub database_error
                  call "SHOSTAT" ("Searching for ROP intrusions...")
                  goto find_rop_intrusions

L12580:        get #1 using L12600, partdescr$, cat$, vendor_code$,     ~
                                   pclass$, leadtime$, type$, buy_code$,~
                                   priority$
L12600:        FMT POS(26),CH(30),POS(90),CH(4),POS(102),CH(9),POS(133),~
                   CH(4),POS(170), CH(03), POS(180), CH(3), POS(200),   ~
                   CH(03), POS(334), CH(1)
               if type$ > lasttype$ then L12440
               if type$ < firsttype$ then L12440
               if cat$ > lastcat$ then L12440
               if cat$ < firstcat$ then L12440
               if pclass$ > lastclass$  then L12440
               if pclass$ < firstclass$ then L12440
/*EWD001 - B*/ if vendor_code$ > lastven$  then L12440
               if vendor_code$ < firstven$ then L12440
               if buy_code$ > lastbuy$  then L12440
/*EWD001 - E*/ if buy_code$ < firstbuy$ then L12440
               convert leadtime$ to leadtime
               call "CONVERT" (leadtime, 0.0, leadtime$)
               convert leadtime$ to leadtime%
               store$ = " "
               lot$ = " "
               sw% = 1                             /* (AWD002)  */
               call "HNYTOTSB" (str(plowkey$,1,25),store$,lot$,quan(),sw%)
               tempqty = quan(1)
               init (hex(00)) plowkey2$
               str(plowkey2$,1,25) = plowkey$

        plow_pipin
               call "PLOWALTS" (#4, plowkey2$, 1%, 25%, f1%(4))
               if f1%(4) <> 1 then check_rop
               get #4 using L13040, pipdate%, ponum$, pipqty
L13040:        FMT POS(26), BI(4), CH(19), PD(14,4)

        REM   MODIFIED FOR APC TO SHOW PO'S ON SCREEN AND REPORT



                gosub write_array
                tempqty = tempqty + pipqty
                goto plow_pipin

        check_rop
               if rop = 0 and tempqty = 0 then find_rop_intrusions
               if tempqty > rop then find_rop_intrusions
         REM   IF (TEMPQTY > ROP) AND (AR_FLAG% = 0%)                    ~
                     THEN FIND_ROP_INTRUSIONS
               if tot_rec% = 0 then gosub open_rophold
               tot_rec% = tot_rec% + 1
               if priority$ = " " then priority$ = "A"
               call "DATE" addr("G+", date, leadtime%, plandate$, ret%)
               write #25 using L13320, " ", priority$, tot_rec%, plowkey$,~
                                      plandate$, eoq, rop, quan(1),      ~
                                      leadtime$, "8", partdescr$,        ~
                                      ar_flag%, " ", eod goto L13382
L13320:        FMT CH(1), CH(1), BI(4), CH(25), CH(6), 3*PD(14,4), CH(3),~
                   CH(1), CH(30), BI(1), CH(4)
               if mode = 2 then gosub print_record
               goto find_rop_intrusions

L13382: REM TEST ERROR
               stop "(Error) - Duplicate Write of ------> " & plowkey$
               close ws
               str(partdescr$,24%,7%) = "(Error)"
               gosub print_record
               goto find_rop_intrusions

        open_rophold
               rslt$(25) = "OUTPTN" & str(recs$) & "100" & "100"
               f2%(25) = 1
               call "OPENFILE" (#25,"OUTPT", f2%(25), rslt$(25), axd$(25))
               if f2%(25) = 0 then return
               errormsg$ = "File ROPHOLD could not be opened successfully"
               return clear all
               goto inputmode

        database_error
L13600:        kh% = 2
               call "ASKUSER" (kh%, "DATABASE ERROR", msg1$, "Press RETUR~
        ~N to ignore this record and continue - or -", "Press PF16 to CANC~
        ~EL search.")
               if kh% <> 16 then L13740
                  return clear all
                  goto inputmode
L13740:        if kh% <> 0 then L13600
               return

        display_control
            f2%(25) = 1
            call "OPENFILE" (#25,"IO   ", f2%(25), rslt$(25), axd$(25))
            if f2%(25) = 0 then L13940
               errormsg$ = "Unable to open file ROPHOLD successfully."
               return clear all
               goto inputmode
L13940:     inc% = 1
            start% = 1
            finish% = 12
            rec_nbr%, lmode = 0
            pf7$ = "(7)Part Descriptions"
            pf8$ = " "
            pf9$ = "(9/25)Select All"
            pf10$ = "(10/26)Deselect"
L14010:     gosub load_screen
            pfkeys$ = hex(000203ffff0607ff090a0b0d0f1012131415191a01)
            pfac$ = hex(9c)
            pf16$ = "(16)Proceed..."
            if rec_nbr%(rec_ctr%) < tot_rec% then L14100
               pf5$ = " "
               str(pfkeys$,5,1) = hex(ff)
               goto L14120
L14100:     pf5$ = "(5/21)Next/Last"
            str(pfkeys$,5,1) = hex(05)
L14120:     if rec_nbr%(1) > 12 then L14160
               pf4$ = " "
               str(pfkeys$,4,1) = hex(ff)
               goto L14180
L14160:     pf4$ = "(4/20)Prev/First"
            str(pfkeys$,4,1) = hex(04)
L14180:     inpmessage$ = "Make Selections by placing an 'X' in the appro~
        ~priate box."
L14200:     gosub display_screen
            errormsg$ = " "
            if keyhit% <> 0 then L14250
               gosub test_fields
               goto L14200
L14250:     if keyhit% <>  7 then L14350
               if str(pf7$,9,1) <> "D" then L14310
                  pf7$ = "(7)Part Numbers     "
                  part_hdr$ = "Part Description"
                  for i% = 1 to rec_ctr%
                      part_dsply$(i%) = part_descr$(i%)
                  next i%
                  goto L14200
L14310:        pf7$ = "(7)Part Descriptions"
               part_hdr$ = "Part Number     "
               for i% = 1 to rec_ctr%
               if ar_flag% = 1%                                          ~
                          then str(part_dsply$(i%),27,3) = "***"
                   str(part_dsply$(i%),1,25) = part$(i%)
               next i%
               goto L14200
L14350:     if keyhit% <>  2 then L14400
               for i% = 1 to rec_ctr%
                   dt$(i%) = "7"
               next i%
               goto L14200
L14400:     if keyhit% <>  3 then L14450
               for i% = 1 to rec_ctr%
                   dt$(i%) = "8"
               next i%
               goto L14200
L14450:     if keyhit% <>  5 then L14510
               gosub test_fields
               if errormsg$ <> " " then L14200
                  gosub write_selections
                  rec_nbr% = rec_nbr%(12)
                  goto L14010
L14510:     if keyhit% <>  4 then L14570
               gosub test_fields
               if errormsg$ <> " " then L14200
                  gosub write_selections
                  rec_nbr% = rec_nbr%(1)
                  goto L14010
L14570:     if keyhit% <> 6 then L15130
               inpmessage$ = "Enter priority sequence A thru Z."
               for i% = 1 to rec_ctr%
                   lfac$(i%), bfac$(i%) = hex(8c)
               next i%
               pfac$ = hex(81)
               disporder$ = porder$
L15050:        gosub display_screen
               errormsg$ = " "
               if keyhit% <> 0 then L15050
                  porder$ = disporder$
                  gosub test_priority_sequence
                  if errormsg$ <> " " then L15050
                     pfac$ = hex(9c)
                     goto L14200
L15130:     if keyhit% <> 1 then L15160
               errflag = 1
               goto startover
L15160:     if keyhit% <> 9 then L15190
               gosub select_all
               goto L14200
L15190:     if keyhit% <> 10 then L15240
               deselect = 1
               gosub select_all
               deselect = 0
               goto L14200
L15240:     if keyhit% <> 11 then L15270
               call "PLNFLSUB" (planflags$(), err%)
               goto L14200
L15270:     if keyhit% <> 16 then L15340
               gosub test_fields
               if errormsg$ <> " " then L14200
                  gosub write_selections
               if first_rec% > 0 then summary_display_control
                  errflag = 1
                  return
L15340:     if keyhit% <> 18 and keyhit% <> 19 then L15360
               gosub global_change
               goto L14200
L15360:     if keyhit% <> 20 then L15420
               gosub test_fields
               if errormsg$ <> " " then L14200
                  gosub write_selections
               rec_nbr% = 0
               goto L14010
L15420:     if keyhit% <> 21 then L15480
               gosub test_fields
               if errormsg$ <> " " then L14200
                  gosub write_selections
               rec_nbr% = tot_rec% - mod(tot_rec%,12)
               goto L14010
L15480:     if keyhit% <> 25 then L15550
               gosub global_change
               first_rec% = 1%
               last_rec% = tot_rec%
               goto L14200
L15550:     if keyhit% <> 26 then L14200
               gosub global_change
               first_rec%, last_rec% = 0%
               goto L14200

        summary_display_control
               pf8$ = "(8)Plan Items"
               str(pfkeys$,8,1) = hex(08)
               pf9$ = "(9)Re-select    "
               pf10$ = " "
               str(pfkeys$,10,1) = hex(ff)
               pf16$ = "(16)Save/Exit   "
               inpmessage$ = " "
               lmode = 1
               init (hex(9c)) bfac$()
               rec_nbr% = first_rec% - 1
L15670:        gosub load_screen
               if box_ctr% = 0 then return
               if rec_nbr%(1) > first_rec% then L15740
                  pf4$ = " "
                  str(pfkeys$,4,1) = hex(ff)
                  str(pfkeys$,18,1) = hex(ff)
                  goto L15770
L15740:        pf4$ = "(4)Previous"
               str(pfkeys$,4,1) = hex(04)
               str(pfkeys$,18,1) = hex(14)
L15770:        if rec_nbr%(box_ctr%) < last_rec% then L16005
                  pf5$ = " "
                  str(pfkeys$,5,1) = hex(ff)
                  str(pfkeys$,19,1) = hex(ff)
                  goto L16020
L16005:        pf5$ = "(5)Next"
               str(pfkeys$,5,1) = hex(05)
               str(pfkeys$,19,1) = hex(15)
L16020:        gosub display_screen
               errormsg$ = " "
               if keyhit% <> 0 then L16045
                  gosub test_fields
                  goto L16020
L16045:     if keyhit% <>  7 then L16105
               if str(pf7$,9,1) <> "D" then L16085
                  pf7$ = "(7)Part Numbers     "
                  part_hdr$ = "Part Description"
                  for i% = 1 to box_ctr%
                      part_dsply$(i%) = part_descr$(i%)
                  next i%
                  goto L16020
L16085:        pf7$ = "(7)Part Descriptions"
               part_hdr$ = "Part Number     "
               for i% = 1 to box_ctr%
                   part_dsply$(i%) = part$(i%)
               next i%
               goto L16020
L16105:     if keyhit% <> 4 then L16135
               gosub test_fields
               if errormsg$ <> " " then L16020
               gosub write_selections
               rec_nbr% = rec_nbr%(1)
               goto L15670
L16135:     if keyhit% <> 5 then L16165
               gosub test_fields
               if errormsg$ <> " " then L16020
               gosub write_selections
               rec_nbr% = rec_nbr%(12%)
               goto L15670
L16165:     if keyhit% <> 2 then L16190
               for i% = 1 to box_ctr%
                   dt$(i%) = "7"
               next i%
               goto L16020
L16190:     if keyhit% <> 3 then L16215
               for i% = 1 to box_ctr%
                   dt$(i%) = "8"
               next i%
               goto L16020
L16215:     if keyhit% <> 6 then L16290
               inpmessage$ = "Enter priority sequence A thru Z."
               for i% = 1 to rec_ctr%
                   lfac$(i%) = hex(8c)
               next i%
               pfac$ = hex(81)
               disporder$ = porder$
L16250:        gosub display_screen
               errormsg$ = " "
               if keyhit% <> 0 then L16250
               porder$ = disporder$
               gosub test_priority_sequence
               if errormsg$ <> " " then L16250
               pfac$ = hex(9c)
               goto L16020
L16290:     if keyhit% <> 1 then L16305
               errflag = 1
               goto startover
L16305:     if keyhit% <> 8 then L16365
               gosub test_fields
               if errormsg$ <> " " then L16020
               gosub write_selections
               gosub plan_items
               if errflag = 1 then L16350
                  errormsg$ = "All Items Planned Successfully."
                  return

L16350:        errormsg$ = "Listed Items Were Not Planned Successfully"
               goto summary_display_control

L16365:     if keyhit% <> 9 then L16405
               errflag = 0
               goto L13940
L16405:     if keyhit% <> 11 then L16420
               call "PLNFLSUB" (planflags$(), err%)
               goto summary_display_control
L16420:     if keyhit% <> 16 then L16435
               errflag = 1
               return
L16435:     if keyhit% = 18 then L16510
            if keyhit% = 19 then L16510
            if keyhit% <> 20 then L16475
               gosub test_fields
               if errormsg$ <> " " then L16020
               gosub write_selections
               rec_nbr% = tot_rec% - mod(tot_rec%,12)
               goto L15670
L16475:     if keyhit% <> 21 then L16505
               gosub test_fields
               if errormsg$ <> " " then L16020
               gosub write_selections
               rec_nbr% = last_rec% + 1
               goto L15670
L16505:     if keyhit% <> 25 then L16516
L16510:        gosub global_change
               first_rec% = 1
               last_rec% = tot_rec%
               goto L16020
L16516:     if keyhit% <> 26 then L16020
               gosub global_change
               first_rec%, last_rec% = 0%
               goto L16020

        write_selections
            for i% = 1% to rec_ctr%
                plowkey$ = bin(rec_nbr%(i%),4)
                call "READ101" (#25, plowkey$, f1%(25))
                if f1%(25) = 1 then L16580
               msg1$ = "Unable to read ROPHNY for part: " & str(part$(i%))
                   gosub database_error
                   return clear
                   goto L17220
L16580:         if box$(i%) <> "X" then L17045
                   if first_rec% > 0 then L17000
                      first_rec% = rec_nbr%(i%)
                      goto L17020
L17000:            if rec_nbr%(i%) < first_rec% then first_rec% =        ~
                      rec_nbr%(i%)
L17020:            if rec_nbr%(i%) > last_rec% then last_rec% =          ~
                      rec_nbr%(i%)
                   goto L17080
L17045:         if rec_nbr%(i%) = first_rec% then first_rec% = 0
                if rec_nbr%(i%) = last_rec% then last_rec% = 0
L17080:         call "DATUNFMT" (plandate$(i%))
                put #25 using L17120, box$(i%), dp$(i%), plandate$(i%),   ~
                                     eoq(i%), dt$(i%)
L17120:            FMT CH(1), CH(1), POS(32), CH(6), PD(14,4), POS(65),  ~
                       CH(1)
                rewrite #25
L17220:     next i%
            return

        load_screen
            rec_ctr%, box_ctr% = 0
            if keyhit% <> 4 then L17311
                inc% = -1
                start% = 12
                finish% = 1
                goto L17320
L17311:     inc% = 1
            start% = 1
            finish% = 12
L17320:     for i% = start% to finish% step inc%
L17340:         if keyhit% = 4 then rec_nbr% = rec_nbr% - 1  else        ~
                   rec_nbr% = rec_nbr% + 1
                plowkey$ = bin(rec_nbr%,4)
                call "READ100" (#25, plowkey$, f1%(25))
                if f1%(25) <> 1 then L17800
                get #25 using L17431, box$
L17431:         FMT CH(1)
                if lmode = 1 then L17540
                   rec_ctr% = rec_ctr% + 1
                   goto L17620
L17540:         if box$ <> "X" then L17340
                   box_ctr% = box_ctr% + 1
                   if first_rec% = 0 then first_rec% = rec_nbr%(i%)
                   if last_rec% = 0 then last_rec% = rec_nbr%(i%)
L17620:        get #25 using L17660, box$(i%),dp$(i%),part$(i%),plandate$,~
                                    eoq, rop, oh, dt$(i%),               ~
                                    part_descr$(i%), ar_flag%(i%)
L17660:        FMT CH(1), CH(1), POS(7), CH(25), CH(6), 3*PD(14,4),      ~
                   POS(65), CH(1), CH(30), BI(1)
                rec_nbr%(i%) = val(str(plowkey$,1),4)
                gosub convert_data
                plandate$(i%) = plandate$
                if str(pf7$,9,1) = "D" then part_dsply$(i%) = part$(i%)  ~
                     else goto L17780
                if ar_flag%(i%) = 1% then                                ~
                     str(part_dsply$(i%),27,3) = "***"
                goto L17790
L17780:         part_dsply$(i%) = part_descr$(i%)
L17790:     next i%
L17800:     if lmode = 1 then rec_ctr% = box_ctr%
            return

        convert_data
               call "CONVERT" (oh, 2.2, oh$(i%))
               call "CONVERT" (rop, 2.2, rop$(i%))
               call "CONVERT" (eoq, 2.2, eoq$(i%))
               call "CONVERT" (rop, 2.2, rop$(i%))
               call "DATEFMT" (plandate$)
               return

        select_all
            for i% = 1 to 12
                if deselect = 1 then box$(i%) = " " else box$(i%) = "X"
            next i%
            return


        plan_items
            print at(4,1,80);hex(84); "Planning Selected Items..."
            call "WORKOPEN" (#27%, "IO", 100%, f2%(27))
            errflag, pctr%, hits%, hitsplnd% = 0
            str(plowkey$,2,4) = bin(first_rec% - 1%,4)
            str(planflags$(),40,18) =                                    ~
               hex(ffffffffffffffffffffffffffffffffffff)
            gosub get_priority
            page% = 0
            runtime$ = " "
            call "TIME" (runtime$)
            call "COMPNAME" (12%, company$, u3%)
            select printer(134)
            call "SETPRNT" ("ROP006", " ", 0%, 0%)
            init (hex(00)) plowkey$
L18130:     call "PLOWNEXT" (#27, plowkey$, 0%, f1%(27))
            if f1%(27) = 0 then L18165
               get #27 using L19101, dc$, dl$, dt$(1), dp$(1), apprv$,    ~
                                    part$(1), eoq$, pdate1$, pdate2$,    ~
                                    late$, errorline$
               gosub print_plnrpt
               goto L18130
L18165:     print
            convert hits% to hits$, pic (####)
            convert hitsplnd% to hitsplnd$, pic (####)
            print using L36110, hits$
            print using L36120, hitsplnd$
            print
            print "** END REPORT **"
            call "SETPRNT" ("ROP006", " ", 0%, 1%)
            close printer
            select crt
            call "FILEBGON" (#27)
            return

        get_priority
            for pctr% = 1 to 26
                if str(porder$,pctr%,1) = " " then L18245
                plowkey$ = str(porder$,pctr%,1) & bin(first_rec% - 1%,4)
                gosub get_next_item
            next pctr%
L18245:     gosub reorg_file
            return

        get_next_item
            call "PLOWALTS" (#25, plowkey$, 2%, 1%, f1%(25))
            if f1%(25) <> 1 then return
               get #25 using L18285, box$(1), dp$(1), rec_nbr%(1),        ~
                       part$(1), plandate$, eoq, rop, dt$(1)
L18285:        FMT CH(1), CH(1), BI(4), CH(25), CH(6),2*PD(14,4),POS(65),~
                   CH(3), CH(1)
               if box$(1) <> "X" then get_next_item
               call "PIPINDEX" (#23, plandate$, cd%(1), ret%)
               time$ = time
               dl$ = str(time$,6,3)
               dc$ = "PCROP" & date & time
               call "CONVERT" (eoq, 2.2, eoq$)
            put #26, using L19500, " ", dt$(1), dp$(1), plandate$,        ~
                                  dc$, dl$, part$(1), eoq$,              ~
                " ", " ", " ", " ", " ", " ",                            ~
                " ", " ", " ", hex(00000000000000000000)
                  write #26

               call "PLANSUB" (dc$,dl$,dt$(1),dp$(1),                    ~
                      part$(1),eoq,cd%(1)," ", " ", " ",                 ~
                      errorline$,stdate%,endate%,today%,100001%,#6,      ~
                      #7,#8,#9,#10,#11,#12,#4,#13,#14,#15,#16,#17,#18,   ~
                      #19,#20,#21,#22)

               hits% = hits% + 1
               pdate1$ = plandate$
               call "DATEFMT" (pdate1$)
               readkey$ = dt$(1) & dp$(1) & plandate$ & dc$ & dl$
               call "READ101" (#26, readkey$, f1%(26))
               if f1%(26) = 1 then L19050
                  msg1$ = "Unable to read DEMMASTR file."
                  gosub database_error
                  goto get_next_item
L19050:        if endate% < 1 then L19175
               if endate% > 490 then L19175
                  if endate% > cd%(1) then stat$ = "7" else stat$ = "9"
                  if endate% > cd%(1) then late$ = "Y" else late$ = "N"
                  pdate2$ = yymmdd$(endate%)
                  call "DATEFMT" (pdate2$)
                  apprv$ = "Y"
                  put #27 using L19101, dc$, dl$, dt$(1), dp$(1), apprv$, ~
                                       part$(1), eoq$, pdate1$,          ~
                                       pdate2$, late$, errorline$
L19101:           FMT CH(16), CH(3), 3*CH(1), CH(25), CH(10), CH(8),     ~
                      CH(8), CH(1), CH(79)
                  write #27
                  hitsplnd% = hitsplnd% + 1
                  put #26 using L19115, stat$, yymmdd$(stdate%),          ~
                                       yymmdd$(endate%)
L19115:           FMT CH(1), POS(77), 2*CH(6)
                  rewrite #26
                  readkey$ = bin(rec_nbr%(1),4)
                  call "READ101" (#25, readkey$, f1%(25))
                  if f1%(25) = 1 then L19155
                     msg1$ = "Unable to delete planned item from ROPHOLD."
                     gosub database_error
                     goto get_next_item
L19155:           delete #25
                  tot_rec% = tot_rec% - 1
                  goto get_next_item

L19175:        errflag = 1
               delete #26
               late$ = " "
               apprv$ = "N"
               pdate2$ = " "
               put #27 using L19101, dc$, dl$, dt$(1), dp$(1), apprv$,    ~
                                    part$(1), eoq$, pdate1$,             ~
                                    pdate2$, late$, errorline$
               write #27

               goto get_next_item

        reorg_file
               if tot_rec% = 0% then return
               plowkey$ = all(hex(00))
               box_ctr%, first_rec% = 0
               for i% = 1% to tot_rec%
                   call "PLOWNXT1" (#25, plowkey$, 0%, f1%(25))
                   get #25 using L19250, rec$
L19250:            FMT CH(100)
                   delete #25
                   if str(rec$,1,1) <> "X" then L19280
                      if first_rec% = 0 then first_rec% = i%
                      last_rec%  = i%
                      box_ctr% = box_ctr% + 1
L19280:            str(rec$,3,4) = bin(i%,4)
                   write #25 using L19250, rec$
               next i%
               return

        clear_tables
            if rec_ctr% = 12 then return
            for i% = rec_ctr% + 1 to 12
                lfac$(i%), bfac$(i%) = hex(9c)
                box$(i%),part$(i%),oh$(i%),rop$(i%),part_dsply$(i%) = " "
            next i%
            return

        global_change
            svrec_nbr% = rec_nbr%(1) - 1
            init (hex(00)) plowkey$
L19360:     call "PLOWNXT1" (#25, plowkey$, 0%, f1%(25))
            if f1%(25) <> 1 then L19465
            if lmode <> 1 then L19390
               get #25 using L19380, box$
L19380:        FMT CH(1)
               if box$ <> "X" then L19360
L19390:     if keyhit% <> 18 then L19410
               put #25 using L19400, "7"
L19400:        FMT POS(65), CH(1)
               goto L19450
L19410:     if keyhit% <> 19 then L19425
               put #25 using L19400, "8"
               goto L19450
L19425:     if keyhit% = 25 then L19440
               put #25 using L19445, " "
               goto L19450
L19440:     put #25 using L19445, "X"
L19445:     FMT CH(1)
L19450:     rewrite #25
            goto L19360

L19465:     if keyhit% = 99% then return
            rec_nbr% = svrec_nbr%
            inc% = 1
            start% = 1
            finish% = 12
            gosub load_screen
            return

L19500:     FMT CH( 1),                       /* RECORD STATUS         */~
                CH( 1),                       /* DEMAND TYPE           */~
                CH( 1),                       /* PRIORITY              */~
                CH( 6),                       /* REQ COMPLETION DATE   */~
                CH(16),                       /* DEMAND CODE           */~
                CH( 3),                       /* DEMAND LINE           */~
                CH(25),                       /* PART NEEDED           */~
                CH(10),                       /* QUANTITY              */~
                CH( 4),                       /* WC (FOR PM ONLY)      */~
                CH( 3),                       /* WHICH BOM REQD?       */~
                CH( 3),                       /* WHICH WCROUTE REQD?   */~
                CH( 3),                       /* DEL TO/SHIP FROM WHSE */~
                CH( 6),                       /* DATE LAST PLANNED     */~
                CH( 6),                       /* PLANNED COMPL DATE    */~
                CH(09),                       /* CUSTOMER IF TYPE 1    */~
                CH(01),                       /* APPROVAL STATUS       */~
                CH(15),                       /* APPROVAL BY           */~
                CH(10)                        /* APPROVAL ON MMDD (BIN)*/

        write_array              /* SUB TO CONVERT DATE/COLLECT PO INFO */
            cnt% = cnt% + 1%         /* BASED ON LEAD TIME  */
            if today% + leadtime% <= pipdate% then ar_flag% = 1%

            call "READ100" (#23, "MONTHS OPEN", f23%)
            if f23% = 0 then end
                get #23, using L19660, day1$
L19660:              FMT XX(32), CH(6)

            pipdate% = pipdate% - 1%
            call "DATE" addr("G+", day1$, pipdate%, pipdate$, ret%)
            call "DATEFMT" (pipdate$)
            call "CONVERT" (pipqty, 2.2, pipqty$)
            poarray$(cnt%,1) = ponum$
            poarray$(cnt%,2) = pipqty$
            poarray$(cnt%,3) = pipdate$
        return
        due_in                /* SUB TO CONVERT DATE/COLLECT PO INFO */
            cnt% = cnt% + 1%     /* BASED ON DUE IN AMT <> 0  */
            call "READ100" (#23, "MONTHS OPEN", f23%)
            if f23% = 0 then end
                get #23, using L19810, day1$
L19810:              FMT XX(32), CH(6)

            pipdate% = pipdate% - 1%
            call "DATE" addr("G+", day1$, pipdate%, pipdate$, ret%)
            call "DATEFMT" (pipdate$)
            call "CONVERT" (pipqty, 2.2, pipqty$)
            poarray$(cnt%,1) = ponum$
            poarray$(cnt%,2) = pipqty$
            poarray$(cnt%,3) = pipdate$
        return
        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20160,         /* Part Number        */    ~
                              L20210,         /* Part Type          */    ~
                              L20260,         /* Part Category      */    ~
                              L20310,         /* Exposure Class     */    ~
                              L20350,         /* Vendor Code        */    ~
                              L20370,         /* Buyer Code         */                                  
            return

L20160: REM Def/Enable Part Number                 BEGPART$
            if begpart$ = " " then begpart$ = "ALL"
            inpmessage$ = "Enter Part Number Range."
            return

L20210: REM Def/Enable Part Type
            if begtype$ <> " " then L20230
               begtype$ = "200"
               endtype$ = "999"
L20230:     inpmessage$ = "Enter Part Type Range."
            return

L20260: REM Def/Enable Part Category               BEGCAT$
            if begcat$ = " " then begcat$ = "ALL"
            inpmessage$ = "Enter Part Category Range."
            return

L20310: REM Def/Enable Exposure Class              CLASS$
            if begclass$ = " " then begclass$ = "ALL"
            inpmessage$ = "Enter Part Class."
            return

L20350: REM Def/Enable Vendor Code                 VENDOR_CODE$  (EWD001)
            if begven$ = " " then begven$ = "ALL"
            inpmessage$ = "Enter Vendor Code."
            return

L20370: REM Def/Enable Buyer  Code                 BUY_CODE$  (EWD001)
            if begbuy$ = " " then begbuy$ = "ALL"
            inpmessage$ = "Enter Buyer Code."
            return            
            
L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") inpmessage$,                                       ~
                      begpart$               , /* Part Number        */  ~
                      begcat$                , /* Part Category      */  ~
                      begclass$              , /* Exposure Class     */  ~
                      begtype$               , /* Part Type          */  ~
/* (EWD001) */        begven$                , /* Vendor Code        */  ~
/* (EWD001) */        begbuy$                , /* Buyer Code         */  ~
                      endpart$               , /* Ending Part        */  ~
                      endcat$                , /* Ending Category    */  ~
                      endclass$              , /* Ending Class Code  */  ~
                      endtype$               , /* Ending Type        */  ~
/* (EWD001) */        endven$                , /* Ending Vendor Code */  ~
/* (EWD001) */        endbuy$                , /* Ending Buyer Code  */  ~
                      str(line2$,1,16)       , /* Mode Constant      */  ~
                      poarray$()               /* PO array           */
            cnt% = 0%
            mat ar_flag% = zer
            ar_flag% = 0%
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        print_record
            if page% = 0 or line% > 50 then gosub print_hdr
            call "CONVERT" (rop, 2.2, rop$)
            call "CONVERT" (eoq, 2.2, eoq$)
            call "CONVERT" (quan(1), 2.2, oh$)
            duein = tempqty - quan(1)
            call "CONVERT" (duein, 2.2, duein$)
            if ar_flag% = 1% then                                        ~
            print using L35085, plowkey$, partdescr$,cat$, type$, pclass$,~
                               leadtime$, rop$, oh$, duein$, eoq$        ~
            else                                                         ~
            print using L35070, plowkey$, partdescr$,cat$, type$, pclass$,~
                               leadtime$, rop$, oh$, duein$, eoq$
            for i% = 1% to cnt%                /* PRINT PO INFO ON RPT */
            if poarray$(i%,1) = " " then L30047

            print using L35082, poarray$(i%,1), poarray$(i%,2),           ~
                               poarray$(i%,3)
            line% = line% + 1%
L30047:     next i%

            totpart = totpart + 1
            if ar_flag% = 0% then line% = line% + 1%
            ar_flag% = 0%
            cnt% = 0%
            init(" ") poarray$()
            return

        print_hdr
            page% = page% + 1
            print page
            print using L35000, date$, runtime$, company$
            print using L35020, page%
            print
            print using L35030
            print using L35042
            print using L35050
            print
            line% = 7
            return

        print_plnrpt
            if page% = 0 or line% > 55 then gosub print_plnhdr
            call "DESCRIBE" (#1, part$(1), partdescr$, 0%, f1%(2))
            if f1%(2) = 0 then partdescr$ = " "
            print using L36070, dc$, dl$, dt$(1), dp$(1), apprv$,         ~
                  part$(1), partdescr$, eoq$, pdate1$, pdate2$,          ~
                  late$
            print using L36090, errorline$
            print
            line% = line% + 3
        return

        print_plnhdr
            print page
            page% = page% + 1
            print using L36015, date$, runtime$, company$
            print using L36000, page%
            print
            print using L36020
            print using L36050
            print
            line% = 6
            return

L35000: %RUN DATE: ######## ########             ########################~
        ~####################################               ROPSRCH:ROP005
L35020: %                                                  RE-ORDER POINT~
        ~ INTRUSION REPORT                                     PAGE: ###
L35030: % PART NUMBER                PART DESCRIPTION                 CAT~
        ~  TYPE  CLASS   LT         ROP     ON HAND      DUE IN     EOQ QT~
        ~Y
L35042: %                                   PO NUMBER      LN     QTY    ~
        ~PO DATE
L35050: % -------------------------  ------------------------------  ----~
        ~  ----  -----  ---  ----------  ----------  ----------  ---------~
        ~-
L35070: % #########################  ##############################  ####~
        ~   ###    #    ###  ##########  ##########  ##########  #########~
        ~#
L35082: %                                 ###################  ########  ~
        ~########
L35085: % #########################  ##############################  ####~
        ~   ###    #    ###  ##########  ##########  ##########* #########~
        ~#
L35090: % TOTAL PARTS = ######

L36000: %                                            MULTIPLE DEMAND PLAN~
        ~NING STATUS REPORT                                    PAGE: ###
L36015: %RUN DATE: ######## ########             ########################~
        ~####################################               ROPSRCH:ROP006
L36020: %DEMAND           LINE TYPE  PRIORITY  APPROVED PART NUMBER      ~
        ~         DESCRIPTION              QUANTITY DUE DATE  PLANNED  LAT~
        ~E
L36050: %---------------- ---- ----  --------  -------- -----------------~
        ~-------- ---------------------- ---------- -------- --------  ---~
        ~-
L36070: %################  ###   #       #        #     #################~
        ~######## ###################### ########## ######## ########    #
L36090: %################################################################~
        ~################
L36110: %#### SELECTED
L36120: %#### PLANNED
        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40180,         /* Part Number       */   ~
                                L40190,         /* Part Type         */   ~
                                L40180,         /* Part Category     */   ~
                                L40180,         /* Exposure Class    */   ~
                                L40180,         /* Vendor Code       */   ~
                                L40180          /* Buyer  Code       */
              goto L40210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40190:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40210:     accept                                                       ~
               at (01,02),                                               ~
                  "Re-order Point Search",                               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)), from$                  , ch(25),~
               at (06,56), fac(hex(ac)), to$                    , ch(25),~
                                                                         ~
               at (08,02), "Part Number",                                ~
               at (08,30), fac(lfac$( 1)), begpart$             , ch(25),~
               at (08,56), fac(lfac$( 1)), endpart$             , ch(25),~
                                                                         ~
               at (09,02), "Part Type",                                  ~
               at (09,30), fac(lfac$( 2)), begtype$             , ch(03),~
               at (09,56), fac(lfac$( 2)), endtype$             , ch(03),~
                                                                         ~
               at (10,02), "Part Category",                              ~
               at (10,30), fac(lfac$( 3)), begcat$              , ch(04),~
               at (10,56), fac(lfac$( 3)), endcat$              , ch(04),~
                                                                         ~
               at (11,02), "Part Class",                                 ~
               at (11,30), fac(lfac$( 4)), begclass$            , ch(04),~
               at (11,56), fac(lfac$( 4)), endclass$            , ch(04),~
                                                                         ~
/*EWD001 - B*/ at (12,02), "Vendor Code",                                ~
               at (12,30), fac(lfac$( 5)), begven$              , ch(09),~
               at (12,56), fac(lfac$( 5)), endven$              , ch(09),~
                                                                         ~
               at (13,02), "Buyer Code",                                 ~
               at (13,30), fac(lfac$( 6)), begbuy$              , ch(03),~
/*EWD001 - E*/ at (13,56), fac(lfac$( 6)), endbuy$              , ch(03),~
                                                                         ~               
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L40650
                  call "MANUAL" ("ROPSRCH ")
                  goto L40210

L40650:        if keyhit% <> 15 then L40690
                  call "PRNTSCRN"
                  goto L40210

L40690:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        initial_screen

L41150:     accept                                                       ~
               at (01,02),                                               ~
                  "Re-order Point Search",                               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Select Search Function:",                    ~
                                                                         ~
               at (08,10), "PF 8) Interactive Display of ROP Intrusions",~
               at (09,10), "PF 9) Print Report of ROP Intrusions",       ~
               at (10,10), fac(hex(8c)), pf10$                  , ch(42),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(pfkeys1$),                                           ~
               key (keyhit%)

               if keyhit% <> 13 then L41580
                  call "MANUAL" ("ROPSRCH ")
                  goto L41150

L41580:        if keyhit% <> 15 then L41620
                  call "PRNTSCRN"
                  goto L41150

L41620:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        display_screen
            if keyhit% = 6  then L42035
            if errflag <> 0 then L42025
               for i% = 1 to rec_ctr%
                   lfac$(i%) = hex(81)
                   if lmode <> 1 then bfac$(i%) = hex(81)
               next i%
L42025:      gosub clear_tables

L42035:     accept                                                       ~
               at (01,02),                                               ~
                  "Re-order Point Search",                               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,04), fac(hex(ac)), part_hdr$              , ch(30),~
               at (05,35), fac(hex(ac)), onhand_hdr$            , ch(10),~
               at (05,46), fac(hex(ac)), rop_hdr$               , ch(10),~
               at (05,57), fac(hex(ac)), priority_hdr$          , ch(01),~
               at (05,59), fac(hex(ac)), dmndtype_hdr$          , ch(01),~
               at (05,61), fac(hex(ac)), date_hdr$              , ch(08),~
               at (05,70), fac(hex(ac)), qty_hdr$               , ch(10),~
                                                                         ~
               at (07,02), fac(bfac$(1)),box$(1)                , ch(01),~
               at (07,04), fac(hex(8c)), part_dsply$(1)         , ch(30),~
               at (07,35), fac(hex(8c)), oh$(1)                 , ch(10),~
               at (07,46), fac(hex(8c)), rop$(1)                , ch(10),~
               at (07,57), fac(lfac$(1)),dp$(1)                 , ch(01),~
               at (07,59), fac(lfac$(1)),dt$(1)                 , ch(01),~
               at (07,61), fac(lfac$(1)),plandate$(1)           , ch(08),~
               at (07,70), fac(lfac$(1)),eoq$(1)                , ch(10),~
                                                                         ~
               at (08,02), fac(bfac$(2)),box$(2)                , ch(01),~
               at (08,04), fac(hex(8c)), part_dsply$(2)         , ch(30),~
               at (08,35), fac(hex(8c)), oh$(2)                 , ch(10),~
               at (08,46), fac(hex(8c)), rop$(2)                , ch(10),~
               at (08,57), fac(lfac$(2)),dp$(2)                 , ch(01),~
               at (08,59), fac(lfac$(2)),dt$(2)                 , ch(01),~
               at (08,61), fac(lfac$(2)),plandate$(2)           , ch(08),~
               at (08,70), fac(lfac$(2)),eoq$(2)                , ch(10),~
                                                                         ~
               at (09,02), fac(bfac$(3)),box$(3)                , ch(01),~
               at (09,04), fac(hex(8c)), part_dsply$(3)         , ch(30),~
               at (09,35), fac(hex(8c)), oh$(3)                 , ch(10),~
               at (09,46), fac(hex(8c)), rop$(3)                , ch(10),~
               at (09,57), fac(lfac$(3)),dp$(3)                 , ch(01),~
               at (09,59), fac(lfac$(3)),dt$(3)                 , ch(01),~
               at (09,61), fac(lfac$(3)),plandate$(3)           , ch(08),~
               at (09,70), fac(lfac$(3)),eoq$(3)                , ch(10),~
                                                                         ~
               at (10,02), fac(bfac$(4)),box$(4)                , ch(01),~
               at (10,04), fac(hex(8c)), part_dsply$(4)         , ch(30),~
               at (10,35), fac(hex(8c)), oh$(4)                 , ch(10),~
               at (10,46), fac(hex(8c)), rop$(4)                , ch(10),~
               at (10,57), fac(lfac$(4)),dp$(4)                 , ch(01),~
               at (10,59), fac(lfac$(4)),dt$(4)                 , ch(01),~
               at (10,61), fac(lfac$(4)),plandate$(4)           , ch(08),~
               at (10,70), fac(lfac$(4)),eoq$(4)                , ch(10),~
                                                                         ~
               at (11,02), fac(bfac$(5)),box$(5)                , ch(01),~
               at (11,04), fac(hex(8c)), part_dsply$(5)         , ch(30),~
               at (11,35), fac(hex(8c)), oh$(5)                 , ch(10),~
               at (11,46), fac(hex(8c)), rop$(5)                , ch(10),~
               at (11,57), fac(lfac$(5)),dp$(5)                 , ch(01),~
               at (11,59), fac(lfac$(5)),dt$(5)                 , ch(01),~
               at (11,61), fac(lfac$(5)),plandate$(5)           , ch(08),~
               at (11,70), fac(lfac$(5)),eoq$(5)                , ch(10),~
                                                                         ~
               at (12,02), fac(bfac$(6)),box$(6)                , ch(01),~
               at (12,04), fac(hex(8c)), part_dsply$(6)         , ch(30),~
               at (12,35), fac(hex(8c)), oh$(6)                 , ch(10),~
               at (12,46), fac(hex(8c)), rop$(6)                , ch(10),~
               at (12,57), fac(lfac$(6)),dp$(6)                 , ch(01),~
               at (12,59), fac(lfac$(6)),dt$(6)                 , ch(01),~
               at (12,61), fac(lfac$(6)),plandate$(6)           , ch(08),~
               at (12,70), fac(lfac$(6)),eoq$(6)                , ch(10),~
                                                                         ~
               at (13,02), fac(bfac$(7)),box$(7)                , ch(01),~
               at (13,04), fac(hex(8c)), part_dsply$(7)         , ch(30),~
               at (13,35), fac(hex(8c)), oh$(7)                 , ch(10),~
               at (13,46), fac(hex(8c)), rop$(7)                , ch(10),~
               at (13,57), fac(lfac$(7)),dp$(7)                 , ch(01),~
               at (13,59), fac(lfac$(7)),dt$(7)                 , ch(01),~
               at (13,61), fac(lfac$(7)),plandate$(7)           , ch(08),~
               at (13,70), fac(lfac$(7)),eoq$(7)                , ch(10),~
                                                                         ~
               at (14,02), fac(bfac$(8)),box$(8)                , ch(01),~
               at (14,04), fac(hex(8c)), part_dsply$(8)         , ch(30),~
               at (14,35), fac(hex(8c)), oh$(8)                 , ch(10),~
               at (14,46), fac(hex(8c)), rop$(8)                , ch(10),~
               at (14,57), fac(lfac$(8)),dp$(8)                 , ch(01),~
               at (14,59), fac(lfac$(8)),dt$(8)                 , ch(01),~
               at (14,61), fac(lfac$(8)),plandate$(8)           , ch(08),~
               at (14,70), fac(lfac$(8)),eoq$(8)                , ch(10),~
                                                                         ~
               at (15,02), fac(bfac$(9)),box$(9)                , ch(01),~
               at (15,04), fac(hex(8c)), part_dsply$(9)         , ch(30),~
               at (15,35), fac(hex(8c)), oh$(9)                 , ch(10),~
               at (15,46), fac(hex(8c)), rop$(9)                , ch(10),~
               at (15,57), fac(lfac$(9)),dp$(9)                 , ch(01),~
               at (15,59), fac(lfac$(9)),dt$(9)                 , ch(01),~
               at (15,61), fac(lfac$(9)),plandate$(9)           , ch(08),~
               at (15,70), fac(lfac$(9)),eoq$(9)                , ch(10),~
                                                                         ~
               at (16,02), fac(bfac$(10)),box$(10)              , ch(01),~
               at (16,04), fac(hex(8c)), part_dsply$(10)        , ch(30),~
               at (16,35), fac(hex(8c)), oh$(10)                , ch(10),~
               at (16,46), fac(hex(8c)), rop$(10)               , ch(10),~
               at (16,57), fac(lfac$(10)),dp$(10)               , ch(01),~
               at (16,59), fac(lfac$(10)),dt$(10)               , ch(01),~
               at (16,61), fac(lfac$(10)),plandate$(10)         , ch(08),~
               at (16,70), fac(lfac$(10)),eoq$(10)              , ch(10),~
                                                                         ~
               at (17,02), fac(bfac$(11)),box$(11)              , ch(01),~
               at (17,04), fac(hex(8c)), part_dsply$(11)        , ch(30),~
               at (17,35), fac(hex(8c)), oh$(11)                , ch(10),~
               at (17,46), fac(hex(8c)), rop$(11)               , ch(10),~
               at (17,57), fac(lfac$(11)),dp$(11)               , ch(01),~
               at (17,59), fac(lfac$(11)),dt$(11)               , ch(01),~
               at (17,61), fac(lfac$(11)),plandate$(11)         , ch(08),~
               at (17,70), fac(lfac$(11)),eoq$(11)              , ch(10),~
                                                                         ~
               at (18,02), fac(bfac$(12)),box$(12)              , ch(01),~
               at (18,04), fac(hex(8c)), part_dsply$(12)        , ch(30),~
               at (18,35), fac(hex(8c)), oh$(12)                , ch(10),~
               at (18,46), fac(hex(8c)), rop$(12)               , ch(10),~
               at (18,57), fac(lfac$(12)),dp$(12)               , ch(01),~
               at (18,59), fac(lfac$(12)),dt$(12)               , ch(01),~
               at (18,61), fac(lfac$(12)),plandate$(12)         , ch(08),~
               at (18,70), fac(lfac$(12)),eoq$(12)              , ch(10),~
                                                                         ~
               at (20,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (21,02), "(1)Startover",                               ~
               at (21,25), fac(hex(8c)), pf7$                   , ch(20),~
               at (21,48), fac(hex(8c)), pf9$                   , ch(16),~
               at (21,65), "(13)Instructions",                           ~
               at (22,02), "(2/18)Type 7",                               ~
               at (22,15), fac(hex(8c)), pf4$                           ,~
               at (22,48), fac (hex(8c)), pf10$                 , ch(15),~
               at (22,65), "(15)Print Screen",                           ~
               at (23,02), "(3/19)Type 8",                               ~
               at (23,15), fac(hex(8c)), pf5$                           ,~
               at (23,32), fac(hex(85)), pf8$                           ,~
               at (23,48), "(11)Behavior SW"                            ,~
               at (23,65), fac(hex(8c)), pf16$                          ,~
               at (24,02), "(6)Priority Sequence"                       ,~
               at (24,25), fac(pfac$),disporder$                , ch(26),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L43440
                  call "MANUAL" ("ROPSRCH ")
                  goto L42035

L43440:        if keyhit% <> 15 then L43480
                  call "PRNTSCRN"
                  goto L42035

L43480:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50140,         /* Part Number       */     ~
                              L50190,         /* Part Type         */     ~
                              L50240,         /* Part Category     */     ~
                              L50290,         /* Exposure Class    */     ~
                              L50320,         /* Vendor Code       */     ~
                              L50350          /* Buyer  Code       */                              
            return

L50140: REM Test for Part Number                  BEGPART$
            call "TESTRNGE" (begpart$, endpart$, firstpart$, lastpart$,  ~
                             errormsg$)
            return

L50190: REM Test for Part Type                    BEGTYPE$
            call "TESTRNGE" (begtype$, endtype$, firsttype$, lasttype$,  ~
                             errormsg$)
            return

L50240: REM Test for Part Category                BEGCAT$
            call "TESTRNGE" (begcat$, endcat$, firstcat$, lastcat$,      ~
                             errormsg$)
            return

L50290: REM Test for Part Class                   CLASS$
            call "TESTRNGE" (begclass$,endclass$,firstclass$,lastclass$, ~
                             errormsg$)
            return

L50320: REM Test for Vendor Code                  BEGVEN$
            call "TESTRNGE" (begven$,endven$,firstven$,lastven$, ~
                             errormsg$)
            return

L50350: REM Test for Buyer  Code                  BEGBUY$
            call "TESTRNGE" (begbuy$,endbuy$,firstbuy$,lastbuy$, ~
                             errormsg$)
            return

        test_fields
            for i% = 1 to rec_ctr%
                gosub test_demand_type
                if errormsg$ = " " then L50450
                   init (hex(8c)) lfac$()
                   lfac$(i%), bfac$(i%) = hex(81)
                   return
L50450:     next i%
            return

        test_demand_type
            if dt$(i%) = "7" or dt$(i%) = "8" then test_plandate
               errormsg$ = "Demand Type Must Be '7' or '8'."
               return

        test_plandate
            call "DATEOK" (plandate$(i%), 0%, errormsg$)
            if errormsg$ = " " then test_qty
            return

        test_qty
            call "NUMTEST" (eoq$(i%), 0, 9e7, errormsg$, -2.2, eoq(i%))
            if errormsg$ = " " then test_priority
            return

        test_priority
            if dp$(i%) = " " then return
            if dp$(i%) < "A" or dp$(i%) > "Z" then L50670
                return
L50670:     errormsg$ = "Demand Priority Must Be 'A' thru 'Z'."
            return

        test_priority_sequence
                     for i% = 1 to 26
                         if str(porder$,i%,1) = " " then L50830
                         if str(porder$,i%,1) < "A" then L50880
                         if str(porder$,i%,1) > "Z" then L50880
                      a% = pos(-porder$ = str(porder$,i%,1))
                         if a% > pos(porder$ = str(porder$,i%,1)) then   ~
                            goto L50900
                      next i%
L50830:               pfac$ = hex(9c)
                      for i% = 1 to rec_ctr%
                          lfac$(i%) = hex(81)
                          if lmode <> 1 then bfac$(i%) = hex(81)
                      next i%
                   return
L50880:            errormsg$ = "Invalid Priority Code"
                   return
L50900:            errormsg$ = "Duplicate Code Not Allowed In Priority Se~
        ~quence."
                   return


        rem**************************************************************~
            *      l o a d   c a l m a s t r    d a t a                 *~
            *************************************************************

        loadcalendar

L64060:         FMT XX(2), CH(1470)
                FMT XX(2), 490*BI(4)
                FMT XX(2), 490*CH(3)

            call "READ100" (#24,"10", f1%(24))
                if f1%(24) = 0 then goto L64520
            get #24, using L64060, str(yymmdd$(),1,1470)

            call "READ100" (#24,"11", f1%(24))
                if f1%(24) = 0 then goto L64520
            get #24, using L64060, str(yymmdd$(),1471,1470)

            return

L64520:         hit = 1
                return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
