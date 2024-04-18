        REM *************************************************************~
            *                                                           *~
            *  V   V  BBBB   K   K  U   U  PPPP   DDDD   TTTTT  EEEEE   *~
            *  V   V  B   B  K  K   U   U  P   P  D   D    T    E       *~
            *  V   V  BBBB   KKK    U   U  PPPP   D   D    T    EEE     *~
            *   V V   B   B  K  K   U   U  P      D   D    T    E       *~
            *    V    BBBB   K   K   UUU   P      DDDD     T    EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VBKUPDTE - Posts On Order quantities to inventory         *~
            *            quantity file. Runs as a background task.      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/18/85 ! ORIGINAL (From VBKPOST1, VBKFILE1)       ! ERN *~
            * 02/17/86 ! Corrected bug in PIPIN of requestiond KAB/ HES *~
            * 06/01/86 ! Line for line Processing (no Arrays) to  !     *~
            *          ! Allow seq # to go to 999.                ! kab *~
            * 02/04/87 ! Changed HNYQUAN Format                   ! KAB *~
            * 08/06/87 ! Changed ST-DATE in PIPIN to reflect order!     *~
            *          ! date instead of DUE-DATE.                ! DAW *~
            * 02/24/89 ! Added Last Mod Date & USERID update for  ! MJB *~
            *          !  changed PO.                             !     *~
            * 01/19/90 ! Add Alt Key to VBKLINES select statement ! SID *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 03/15/93 ! New PORT/MESSAGE subs now in place.      ! KAB *~
            * 10/05/93 ! Changes for Purchase Jobs.               ! JBK *~
            * 06/28/94 ! Purchasing Contracts- added updating of  ! ERN *~
            *          !    VPCXREF file and placement of PO# on  !     *~
            *          !    VBKVSA records                        !     *~
            * 10/26/95 ! PRR -Stop Message Port hang in DATE_ERROR! RJH *~
            * 10/17/97 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            advice$8,                    /* Vendor Service Advice #    */~
            askpf1$80,                   /* Ask User Text              */~
            askpf2$80,                   /* Ask User Text              */~
            auto%(2),                    /* Update Next Purchase Job # */~
            basedate$8,                  /* Planning calendar base date*/~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bomid$3,                     /* BOM Id for Purchase Job    */~
            comment$1,                   /* Comment Line? (Y/N)        */~
            contract$20,                 /* Purchasing Contract & Line */~
            ctl_op$1,                    /* Pur Job Control # Option   */~
            control$19,                  /* Pur Job Control Number     */~
            defkitstr$3,                 /* Deft Kit Store for Pur Job */~
            diskkey$50,                  /* Diskkey for Misc porpoises */~
            diskkey$(2)50,               /* Diskkey for Misc porpoises */~
            diskkeyh$(2)50,              /* Diskkey for Misc porpoises */~
            due$6,                       /* Line Item Due Date         */~
            errmsg$79,                   /* Error Message              */~
            jnlid$3,                     /* Journal Id                 */~
            job$8,                       /* Job / Project Number       */~
            jobname$30,                  /* Purchased Job Name         */~
            kitcomp$1,                   /* Purchase Job Kit Complete  */~
            kitstore$3,                  /* Kit Store for Purchase Job */~
            lot$6,                       /* Lot Number                 */~
            message$10,                  /* Messaging work variable    */~
            modno$2,                     /* Module Number              */~
            newhdr$(11)100,              /* Buffer copy of PO Header   */~
            newhdr1$(11)100,             /* Buffer copy of PO Header   */~
            nextpokey$50,                /* Next PO in buffer file     */~
            npj_no$8,                    /* Next Purchased Job No      */~
            oldhdr$(11)100,              /* Master copy of PO Header   */~
            oldhdr1$(11)100,             /* Master copy of PO Header   */~
            ord_date$6,                  /* Order Date  of PO Header   */~
            part$25,                     /* Part Number                */~
            pj_job$1,                    /* Flag for Purchased Job     */~
            pj_no$8,                     /* Purchased Job Number       */~
            plowkey$99,                  /* Miscellaneous Plowkey      */~
            pipcross$100,                /* PIPCROSS Key               */~
            pipinkey$20,                 /* PIPIN Key                  */~
            piptag$19,                   /* PIP Tag for PO buffer file */~
            poact$1,                     /* PO Hdr updte done A/ /C/D  */~
            poacth$1,                    /* PO Hdr updte last write    */~
            ponumber$16,                 /* Purchase Order Number      */~
            poprnt$1,                    /* PO Print Flag              */~
            porlsekey$100,               /* PORLSE keys                */~
            poreltag$19,                 /* PORLSE Backward PIP tag    */~
            port$4,                      /* Message Port ID            */~
            prtoptions$4,                /* Purchase Job Print Options */~
            prtstamp$7,                  /* Stamp for PO print file    */~
            seq$3,                       /* Sequence Number            */~
            sfcdate$6,                   /* Shop floor posting date    */~
            stamp$7,                     /* Date(3), Time(4) Stamp     */~
            status$1,                    /* P.O. Line Status           */~
            store$3,                     /* Store Number (line)        */~
            tag$19,                      /* Work PIP tag               */~
            temp$8,                      /* Temp work variable         */~
            update$1,                    /* Line update performd A/C/D */~
            userid$3,                    /* User posting PO            */~
            vencode$9,                   /* Vendor Code                */~
            vpcxref_key$50,              /* VPCXREF key                */~
            z_roes$(111)8                /* Zeros for JBMASTR2 Records */

        dim newline$(3)250,              /* NEW Line (one at a time)   */~
            newpart$25,                  /*     Part Codes Array       */~
            newseq$3,                    /*     Sequence Number Array  */~
            newstore$3,                  /*     Store Number Array     */~
            newlot$6,                    /*     Lot Number Array       */~
            newjob$8,                    /*     Job/Project Array      */~
            newonordstr$3,               /*     Store for On Order Qty */~
            newdue$6,                    /*     Due Dates Array        */~
            newst$1,                     /*     P.O. Line Status       */~
            newtag$19,                   /*     PIP tag for line       */~
            oldline$(3)250,              /* OLD Line (one at a time)   */~
            oldpart$25,                  /*     Part Codes Array       */~
            oldseq$3,                    /*     Sequence Number Array  */~
            oldstore$3,                  /*     Store Number Array     */~
            oldlot$6,                    /*     Lot Number Array       */~
            oldjob$8,                    /*     Job/Project Array      */~
            oldonordstr$3,               /*     Store for On Order Qty */~
            olddue$6,                    /*     Due Dates Array        */~
            oldst$1,                     /*     P.O. Line Status       */~
            oldtag$19                    /*     PIP tag for line       */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !          D E S C R I P T I O N           *~
            *-----+----------+------------------------------------------*~
            * # 1 ! HNYQUAN  ! INVENTORY QUANTITY FILE                  *~
            * # 2 ! HNYMASTR ! INVENTORY MASTR FILE                     *~
            * # 3 ! SYSFILE2 ! SYSTEM CONTROL FILE                      *~
            * # 4 ! VBKVSA   ! Vendor Service Advices                   *~
            * # 5 ! VBKMASTR ! BACKLOG MAIN HEADER FILE.                *~
            * # 6 ! VBKLINES ! BACKLOG LINE ITEMS                       *~
            * # 7 ! VPCXREF  ! Purchasing Contract X-Ref                *~
            * # 9 ! VBKBUFFR ! BACKLOG BUFFER FOR PO HEADERS            *~
            * #10 ! VBKBUF2  ! BUFFER FOR LINE ITEMS                    *~
            * #15 ! TXTFILE  ! System Text File                         *~
            * #16 ! VBKCHNGH ! Purchase Order Changes File - Headers    *~
            * #17 ! VBKCHNGL ! Purchase Order Changes File - Line Items *~
            * #20 ! VBKPOPRT ! Purchase Order Print File                *~
            * #21 ! PIPIN    ! PIP In                                   *~
            * #22 ! PORLSE   ! Requistions File                         *~
            * #23 ! PIPMASTR ! PIP Master                               *~
            * #24 ! SFCUM2   ! Forecasts File                           *~
            * #25 ! PIPOUT   ! PIP Out                                  *~
            * #26 ! PIPCROSS ! PIP Cross Reference                      *~
            * #27 ! JBMASTR2 ! Job Master file                          *~
            * #28 ! WCOUT    ! Job Master file                          *~
            * #29 ! WCMASTR  ! Work center master file                  *~
            * #30 ! RTEMASTR ! Standard & alt work center routings      *~
            * #31 ! JBCROSS2 ! Job rte/bom used cross ref.              *~
            * #32 ! BOMMASTR ! Bills of materials structures            *~
            * #33 ! ENGMASTR ! Bom and Rte Effectivity dates            *~
            * #34 ! JBPIPXRF ! Option part pegging                      *~
            * #35 ! POPIPXRF ! Buy Advices/Releases Cross Reference     *~
            * #36 ! USERINFO ! User posting dates                       *~
            *************************************************************

            select  #1, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 650,                                   ~
                        keypos = 17, keylen = 44,                        ~
                        alternate key 1, keypos =  1, keylen = 44

            select  #2, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 900,                                   ~
                        keypos = 1, keylen = 42,                         ~
                        alternate key 1, keypos = 109, keylen = 9, dup,  ~
                                  key 2, keypos = 90, keylen = 4, dup

            select  #3, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #04, "VBKVSA",                                        ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos = 5,    keylen = 8,                       ~
                        alt key  1, keypos =    1, keylen =  12,         ~
                            key  2, keypos =    2, keylen =  11,         ~
                            key  3, keypos =   13, keylen =  12, dup,    ~
                            key  4, keypos =   29, keylen =   6, dup,    ~
                            key  5, keypos =   41, keylen =  13, dup,    ~
                            key  6, keypos =   50, keylen =   4, dup

            select  #5, "VBKMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1030,                                  ~
                        keypos = 1, keylen = 25,                         ~
                        alternate key 1, keypos = 10, keylen = 16

            select #6, "VBKLINES",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 28,                         ~
                        alternate key 1, keypos = 333, keylen = 20, dup

            select #7, "VPCXREF",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 133,                                   ~
                        keypos = 1, keylen = 49,                         ~
                        alternate key 1, keypos = 21, keylen = 49, dup

            select #9,  "VBKBUFFR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1044,                                  ~
                        keypos = 1, keylen = 10,                         ~
                        alternate key 1, keypos =  4, keylen =  7, dup,  ~
                                  key 3, keypos = 24, keylen = 16

            select #10, "VBKBUF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 28

            select #15, "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos  = 1, keylen = 11

            select #16, "VBKCHNGH",                                      ~
                        varc, indexed,                                   ~
                        recsize = 1163,                                  ~
                        keypos = 1, keylen = 32

            select #17, "VBKCHNGL",                                      ~
                        varc, indexed,                                   ~
                        recsize = 736,                                   ~
                        keypos = 1, keylen = 35

            select #20, "VBKPOPRT",                                      ~
                        varc, indexed,                                   ~
                        recsize = 36,                                    ~
                        keypos = 13, keylen = 16,                        ~
                        alt key  1, keypos =    4, keylen =  25,         ~
                            key  2, keypos =    1, keylen =  28

            select #21, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select #22, "PORLSE",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 492,                                   ~
                        keypos =   1, keylen =  66,                      ~
                        alt key  1, keypos =   48, keylen =  19, dup,    ~
                            key  2, keypos =    5, keylen =  62, dup,    ~
                            key  3, keypos =   14, keylen =  53, dup,    ~
                            key  4, keypos =   39, keylen =  28, dup     ~

            select #23, "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =   2, keylen =  25,                      ~
                        alt key  1, keypos =    1, keylen =  26

            select #24, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =   1, keylen =  25

            select #25, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select #26, "PIPCROSS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =  71,                     ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33          ~

            select #27, "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1300,                                  ~
                        keypos = 1, keylen = 8,                          ~
                        alt key  1, keypos = 1120, keylen = 19, dup,     ~
                            key  2, keypos =   58, keylen = 25, dup

           select #28, "WCOUT",                                          ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   68,                                  ~
                        keypos = 9, keylen = 23,                         ~
                        alt key 1, keypos = 1, keylen = 27

           select #29,  "WCMASTR",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos =   2 , keylen = 5,                      ~
                         alt key 1, keypos =  1 , keylen = 6

            select #30, "RTEMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 400,                                  ~
                         keypos =   5, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 35

            select #31, "JBCROSS2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize =  94,                                  ~
                         keypos =29, keylen = 19,                        ~
                         alternate key 1, keypos = 1 , keylen = 47,      ~
                                   key 2, keypos = 48, keylen = 47

            select #32, "BOMMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos =  26, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 56

            select #33, "ENGMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2015,                                 ~
                         keypos = 1, keylen = 29

            select #34, "JBPIPXRF",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize =   63,                                 ~
                         keypos = 1, keylen = 63,                        ~
                         alt key 1, keypos = 45, keylen = 19

            select #35, "POPIPXRF",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 100,                                  ~
                         keypos = 1, keylen = 58

            select #36, "USERINFO",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos = 1, keylen = 3

        call "SHOSTAT" ("Updating Purchase Orders")
            call "OPENCHCK" (# 1, 0%, f2%( 1%),   0%, " ")
            call "OPENCHCK" (# 2, 0%, f2%( 2%),   0%, " ")
            call "OPENCHCK" (# 3, 0%, f2%( 3%),   0%, " ")
            call "OPENCHCK" (# 4, 0%, f2%( 4%),   0%, " ")
            call "OPENCHCK" (# 5, 0%, f2%( 5%), 100%, " ")
            call "OPENCHCK" (# 6, 0%, f2%( 6%), 200%, " ")
            call "OPENCHCK" (# 7, 0%, f2%( 6%), 100%, " ")
            call "OPENCHCK" (# 9, 0%, f2%( 9%),   0%, " ")
            call "OPENCHCK" (#10, 0%, f2%(10%),   0%, " ")
            call "OPENCHCK" (#20, 0%, f2%(20%), 100%, " ")
            call "OPENCHCK" (#21, 0%, f2%(21%), 200%, " ")
            call "OPENCHCK" (#22, 0%, f2%(22%),   0%, " ")
            call "OPENCHCK" (#23, 0%, f2%(23%),   0%, " ")
            call "OPENCHCK" (#24, 0%, f2%(24%),   0%, " ")
            call "OPENCHCK" (#25, 0%, f2%(25%), 200%, " ")
            call "OPENCHCK" (#26, 0%, f2%(26%),   0%, " ")
            call "OPENCHCK" (#27, 0%, f2%(27%), 100%, " ")
            call "OPENCHCK" (#28, 0%, f2%(28%),   0%, " ")
            call "OPENCHCK" (#29, 0%, f2%(29%),   0%, " ")
            call "OPENCHCK" (#30, 0%, f2%(30%),   0%, " ")
            call "OPENCHCK" (#31, 0%, f2%(31%),   0%, " ")
            call "OPENCHCK" (#32, 0%, f2%(32%),   0%, " ")
            call "OPENCHCK" (#33, 0%, f2%(33%),   0%, " ")
            call "OPENCHCK" (#34, 0%, f2%(34%),   0%, " ")
            call "OPENCHCK" (#35, 0%, f2%(35%),   0%, " ")
            call "OPENCHCK" (#36, 0%, f2%(36%),   0%, " ")

        REM *************************************************************~
            *                 I N I T I A L I Z A T I O N               *~
            * --------------------------------------------------------- *~
            * INITIALIZES KEYS, THAT SORT OF THING.                     *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("TT", tt$)

            call "MSGCREAT" (#3, port$, "PORT.ID.VBKUPDTE", return%)
              if return% <> 0% then end

*        Get Base Date for Planning Calendar
            call "READ100" (#3, "MONTHS OPEN", f1%(3))
            if f1%(3) = 1% then L09170
L09160:         askpf1$ = "Error in P.I.P. Base Date."
                askpf2$ = "Press Any PF Key to Exit Program."
                goto date_error
L09170:     get #3, using L09180, basedate$
L09180:         FMT XX(32),CH(6)
            call "DATEOK" (basedate$, err%, errmsg$)
                if errmsg$ <> " " then L09160
            call "DATUNFMT" (basedate$)

*        Open VBK Changes Files (per switch).
            call "VBKSWTCH" ("POHISTRY", message$, idle, u3%)
            if message$ = "Y" then changes% = 1%
               if changes% = 0% then L09330
            call "OPENCHCK" (#16, 0%, f2%(16), 200%, " ")
            call "OPENCHCK" (#17, 0%, f2%(17), 200%, " ")
               if f2%(16) <> 0% or f2%(17) <> 0% then changes% = 0%

L09330
*        Remove any left over 'Kill me' tranactions.
            nextpokey$ = " "
            call "DELETE" (#9, nextpokey$, 3%)

*        Get Switch to see how long update can remain idle.
            call "VBKSWTCH" ("IDLETIME", message$, idle, u3%)
               if idle < 10 then idle = 10
               message$ = " "

*        Check for Purchased Jobs
            postflag% = 1%
            plowkey$ = "SWITCHS.SFC"
            call "READ100" (#3, plowkey$, pj_on%)
                if pj_on% <> 1% then L10000
            get #3 using L09470, ctl_op$, defkitstr$, npj_no$
L09470:         FMT POS(36), CH(1), CH(3), POS(115), CH(8)
            if npj_no$ = " " then pj_on% = 0%
            if pj_on% = 0% then L10000
            init (hex(00))  z_roes$()

            modno$ = "03"
            jnlid$ = "MPR"
            call "EXTRACT" addr ("ID", userid$) /* GET CURRENT USER    */
            call "READ100" (#36, userid$, f1%(36%))
                 if f1%(36%) = 0% then L09620
                 get #36, using L09580, sfcdate$
L09580:     FMT XX(33), CH(6)

            call "WHICHMON" (#3, sfcdate$, return%)
            if return% > 0% then L10000
L09620:         askpf1$ = "Error in SFC Posting Date."
                askpf2$ = "Press Any PF Key to Exit Program."
                goto date_error

L10000: REM *************************************************************~
            *                   M A I N   P R O G R A M                 *~
            * --------------------------------------------------------- *~
            * 1) Wait and look for a PO to become available.            *~
            *                                                           *~
            * 2) Replace the Old order in the Master with the New one   *~
            *    from the buffer file. Toss lines and kill from buffer  *~
            *    as they process to facilitate restarting.              *~
            *    Note- VBKINPUT writes ALL records pulled from Master   *~
            *          back to buffer file; therefore if a line is not  *~
            *          found in the buffer it has already been updated. *~
            *************************************************************

        next_trans        /* Looking into buffer for a PO to update.   */
            nextpokey$ = all(hex(00))
            call "PLOWALTS" (#9, nextpokey$, 1%, 0%, f1%(9))
            if f1%(9) = 1% then process_trans
                if message$  = "CANCEL" then L65000
                if inactive% > idle     then L65000
                call "MSGBFGND"(" ",                                     ~
                                port$, message$, 10%, inactive%, u3%)
                if u3% = 16 then L65000   /* Something's Whacko */
                if postflag% <> 0% then L10230
                     call "JBJNLCLS" ("J1", userid$, modno$, jnlid$,     ~
                                                             pstseq%, 0%)
                     postflag% = 1%
L10230:         goto next_trans

*       ****************[ PROCESS TRANSACTION ]************************
        process_trans     /* Update Purchase Order                     */

            call "SHOSTAT" ("PROCESSING")
            inactive% = 0% : init (" ") newhdr$(), oldhdr$(), poacth$
            get #9 using L10570, userid$, stamp$, poprnt$, vencode$,      ~
                                ponumber$, ord_date$
L10570:         FMT CH(3), CH(7), CH(1), XX(3), CH(9), CH(16), POS(465), ~
                    CH(6)
            if userid$ = " " then L65000            /* Kill Update Task */
            stamp$     = stamp$ bool6 all(hex(ff)) /* Reverses sort    */
            get #9 using L10610, str(newhdr$())
L10610:         FMT XX(14), CH(1030)
            poact$ = " "
            str(newhdr$(),593,9) = date & userid$

            vpcxref_key$ = "P" & str(vencode$,,9) & str(ponumber$,,16) & ~
                                                                  hex(00)
        vpcxref_delete_loop
            call "PLOWAL1" (#7, vpcxref_key$, 1%, 26%, f1%(7%))
            if f1%(7%) = 0% then L10650
                delete #7
                goto vpcxref_delete_loop

L10650:     call "READ100" (#5, str(newhdr$(),,25), f1%(5))
            if f1%(5) = 0% then L10720
                get #5 using L10680, str(oldhdr$())
L10680:             FMT CH(1030)
                poact$ = "D"
                str(newhdr$(),593,9) = str(oldhdr$(),593,9)
                str(newhdr$(),602,9) = date & userid$

L10720:     init (" ") diskkey$()
            str(diskkey$(1),,25) = str(vencode$,,9) & str(ponumber$,,14)
            str(diskkey$(2),,25) = str(vencode$,,9) & str(ponumber$,,14)
            diskkeyh$(1) = diskkey$(1)
            diskkeyh$(2) = diskkey$(2)

        process_lines     /* Update Purchase Order Lines               */
            gosub L30000   /* Load Buffer Lines, One at a Time */
            if i% = 0% then L11500
                if newpart$ = " " and oldpart$ = " " then                ~
                                      comment$ = "Y" else comment$ = "N"

             /* Branching is based on the following table--            */
             /*  NEWSEQ !OLDSEQ !BRANCH!       COMMENTS                */
             /*  -------+-------+------+------------------------------ */
             /*   blank ! blank ! Next ! No line - skip                */
             /*   blank !  ###  ! Next ! Already updated - skip        */
             /*    $FF$ ! blank !Inline! In/Out in Input pgm. Clear    */
             /*         !       !      ! text up only.                 */
             /*    $FF$ !  ###  !Delete! Line deleted                  */
             /*    ###  ! blank ! Add  ! New line item.                */
             /*    ###  !  ###  !Change! Line item changed.            */
             /*  -------+-------+------+------------------------------ */

                if newseq$  = " " then nextline
                if newseq$ <> hex(ff) then L11125
                     if oldseq$ <> " " then delete_line
                          call "TXTFUTIL" (#15, f2%(15), "DELE",         ~
                                                   str(newline$(),306,4))
                          call "DELETE" (#10, str(newline$(),,28), 28%)
                          goto nextline
L11125:         if oldseq$ = " " then add_line else change_line

            add_line
                update$ = "A"  :  if comment$ = "Y" then L11160
                oldonordstr$ = oldstore$  :  newonordstr$ = newstore$
                if str(newjob$,1%,2%) = "PJ" then oldonordstr$,          ~
                                                  newonordstr$ = "001"
                gosub onorder            /* Clean Up ON-ORDER          */
                tag$ = newtag$  :  gosub delete_pipin
                pipquan = newquan
                tag$ = newtag$  :  gosub create_pipin
                gosub update_porlse      /* Update PORLSE & PIPCROSS   */
                if pj_job$ = "Y" then gosub purchase_job_create
                gosub vbkvsa_update
L11160:         goto  toss_and_save

            change_line
                update$ = "C"  :  if comment$ = "Y" then L11220
                gosub vbkvsa_update
                if abs(oldquan - newquan) < .0001 then L11200
L11185:            tag$ = oldtag$  :  gosub delete_pipin
                   pipquan = newquan
                   if str(oldjob$,1%,2%) = "PJ" then pipquan = pipquan + ~
                                                      qrcvh + qqc + qqch
                   tag$ = newtag$  :  gosub create_pipin
L11190:            oldonordstr$ = oldstore$  :  newonordstr$ = newstore$
                   if str(oldjob$,1%,2%) = "PJ" then oldonordstr$ = "001"
                   if str(newjob$,1%,2%) = "PJ" then newonordstr$ = "001"
                   gosub onorder         /* Update ON ORDER           */
                   gosub purchase_job_update
                   goto toss_and_save
L11200:         if olddue$   <> newdue$   then L11185
                if oldstore$ <> newstore$ then L11185
                if oldjob$   <> newjob$   then L11185
                if oldst$    <> newst$    then L11185
                if oldlot$   <> newlot$   then L11190
L11220:         goto toss_and_save

            delete_line
                update$ = "D"  :  if comment$ = "Y" then L11260
                oldonordstr$ = oldstore$  :  newonordstr$ = newstore$
                if str(oldjob$,1%,2%) = "PJ" then oldonordstr$ = "001"
                if str(newjob$,1%,2%) = "PJ" then newonordstr$ = "001"
                gosub onorder            /* Clean Up ON-ORDER          */
                tag$ = oldtag$  :  gosub delete_pipin
                gosub dele_pipcross      /* Mark PIPCROSS as Cancelled */
                gosub vbkvsa_update

L11260
*        Toss and Changes History Update
            toss_and_save
                if update$ = "D" then call "TXTFUTIL"(#15,f2%(15),"DELE",~
                                                   str(oldline$(),306,4))
                call "DELETE" (#6, str(oldline$(),,28), 28%)
                if update$ = "D" then L11300
                     str(newline$(),222%,66%) = " "
                     str(newline$(),534%,27%) = " "
                     if pj_on% = 1% and pj_job$ = "Y" then               ~
                          str(newline$(),166%,8%) = pj_no$
                     write #6, str(newline$(),,700)
                        if poact$ = " " then poact$ = "A"
                        if poact$ = "D" then poact$ = "C"
                        gosub update_vpcxref
L11300:         call "DELETE" (#10, str(newline$(),,28), 28%)
                gosub changes_history_line
            nextline : goto process_lines

L11500
*        Now finish off toss of headers, etc....
            diskkey$ = str(newhdr$(),,25) & hex(00) /* See if any lines*/
            call "PLOWNEXT" (#6, diskkey$, 25%, f1%(6)) /* any lines?  */
               if f1%(6) = 0% then L11570
                  if poact$ = " " then poact$ = "A"
                  if poact$ = "D" then poact$ = "C"

L11570:     if str(newhdr$(),584,6) <= date then L11660 /* Don't kill   */
                if poact$ = " " then poact$ = "A"      /* hdr before   */
                if poact$ = "D" then poact$ = "C"      /* Lst Chng dte */

L11660:     call "DELETE" (#5, str(oldhdr$(),,25), 25%)
            if pos("ACT" = poact$) <> 0%    then                         ~
                                           write #5, str(newhdr$(),,1030)
            if poact$ = " " or poact$ = "D" then                         ~
                call "TXTFUTIL" (#15,f2%(15),"DELE",str(newhdr$(),562,4))

            diskkey$ = str(userid$) & str(newhdr$(),,25)
            prtstamp$ = stamp$
            call "READ101" (#20, ponumber$, f1%(20))  /* PO Print file */
            if f1%(20) = 0% then L11790
                get #20 using L11770, prtstamp$  /* Keep first one that */
L11770:              FMT XX(29), CH(7)          /* is on file.         */
                delete #20
L11790:     if poprnt$ = "N" then L11830
              write #20 using L11810, str(diskkey$,,28), poprnt$, stamp$
L11810:             FMT CH(28), CH(1), CH(7)

L11830:     diskkey$ = " "  :  get #9, str(diskkey$,,10)
            call "DELETE" (#9, diskkey$, 10%)      /* Kill buffer hdr  */

            gosub changes_history_master

        goto next_trans


        REM *************************************************************~
            *            U P D A T E   S U B R O U T I N E S            *~
            *************************************************************

        onorder      /* Update QUANTITY ON ORDER                       */
            if oldpart$ = " " then L12110
            if pos("HC" = oldst$) <> 0% then L12110
                call "HNYPST1" (oldpart$, oldonordstr$, oldlot$,         ~
                                0, 0, -oldquan, 0, 0, #1, #2, #3,        ~
                                f2%(1), f2%(2), f2%(3), 0%, err%)

L12110:     if newpart$ = " " then return
            if pos("HC" = newst$) <> 0% then return
                call "HNYPST1" (newpart$, newonordstr$, newlot$,         ~
                                0, 0, newquan, 0, 0, #1, #2, #3,         ~
                                f2%(1), f2%(2), f2%(3), 0%, err%)
            return



        delete_pipin                /* Clean house and reset PIP files. */
            if tag$ = " " then return

          /* Clean out OLD Stuff, regardless     */
            call "READ101" (#21, tag$, f1%(21%))
                if f1%(21%) = 0% then return
            get #21, using L12270, part$, idx%, q
L12270:         FMT CH(25), BI(4), XX(19), PD(14,4)
            delete #21
            call "PIPFLAGS" (part$, 1%, idx%, -q, #23, #24)
            return

        create_pipin
          /* Now Implement NEW Stuff               */
            if newpart$ = " " then return
            if pos("HC" = newst$) <> 0% then return
            call "READ100" (#23, newpart$, f1%(23%)) /* PIPMASTR    */
            if f1%(23%)       = 0%           then return
            if newstore$ = " "          then L12410  /* Do it anyhu */
            if str(newstore$,1,1) < "0" then return
            if str(newstore$,1,1) > "9" then return
L12410:     if pipquan < .0001          then return

                call "PIPINDEX" (#3, newdue$, idx%, err%)
                call "PIPINDEX" (#3, ord_date$, ids%, err%)
                write #21, using L12470, newpart$, idx%, tag$, pipquan,   ~
                                        ids%
L12470:              FMT CH(25), BI(4), CH(19), PD(14,4), BI(4)
                call "PIPFLAGS" (newpart$, 1%, idx%, pipquan, #23, #24)
            return


        update_vpcxref
            if contract$ = " " then return
                write #7 using L12560, contract$, "P", vencode$,          ~
                                      ponumber$, seq$, contract$, " "
L12560:              FMT CH(20), CH(1), CH(9), CH(16), CH(3), CH(20),    ~
                         CH(64)
                return


        vbkvsa_update
            if update$ = "A" then vbkvsa_add
            if update$ = "C" then vbkvsa_change
            if update$ = "D" then vbkvsa_delete
                return

        vbkvsa_add
            if str(newpart$,,10) <> "ACTIVITY: " then return
                advice$ = str(newpart$,17,8)
                call "READ101" (#4, advice$, f1%(4%))
                if f1%(4%) = 0% then return
                     put #4 using L12730, ponumber$, newseq$
L12730:                   FMT POS(74), CH(16), CH(3)
                     rewrite #4
                     return

        vbkvsa_change
            gosub vbkvsa_delete
            gosub vbkvsa_add

        vbkvsa_delete
            if str(oldpart$,,10) <> "ACTIVITY: " then return
                advice$ = str(oldpart$,17,8)
                call "READ101" (#4, advice$, f1%(4%))
                if f1%(4%) = 0% then return
                     put #4 using L12830, " "
L12830:                   FMT POS(74), CH(19)
                     rewrite #4
                     return


        update_porlse    /* Clean out PORLSE (NEW LINE) & OLD PIPINS;  */
                         /* Update PIPCROSS                            */
            if str(porlsekey$,,47%) = " " then return

            call "READ101" (#22, porlsekey$, f1%(22%))
                if f1%(22%) = 0% then return
            get #22 using L12930, poreltag$
L12930:         FMT POS(242), CH(19)
            delete #22                   /* Kill Header Record         */
            if poreltag$ = " " then L12980
                tag$ = poreltag$  :  gosub delete_pipin

L12980:     str(porlsekey$,48%,1%) = "R"
            init (hex(00))  str(porlsekey$,49%)
L13000:     call "PLOWNXT1" (#22, porlsekey$, 48%, f1%(22%))
                if f1%(22%) = 0% then return
            get #22, using L13030, pipinkey$, poreltag$
L13030:         FMT XX(47), CH(19), POS(242), CH(19)
            delete #22                 /*  Kill line item record      */
            tag$ = str(pipinkey$,1%,19%)  :  gosub delete_pipin
            tag$ = poreltag$              :  gosub delete_pipin

            init (hex(00)) pipcross$
            str(pipcross$,1%,19%) = "B" & str(pipinkey$,2%,18%)
            call "PLOWAL1" (#26, pipcross$, 1%, 19%, f1%(26%))
                if f1%(26%) = 0% then L13170
            delete #26
            put #26 using L13140, newtag$
L13140:         FMT POS(20), CH(19)
            write #26

L13170:     if poreltag$ = " " then L13000
                call "DELETE" (#35, poreltag$, 19%)
            goto L13000

        dele_pipcross     /* Deleted Line Update of PIPCROSS           */
            init (hex(00)) pipcross$
            convert i% to seq$, pic(###)
            str(pipcross$,1,19) = "PO" & str(ponumber$,1,14) &           ~
                                                          str(seq$,1,3)
L13260:     call "PLOWAL1" (#26, pipcross$, 1%, 19%, f1%(26))
            if f1%(26) = 0% then return
                delete #26
                put #26 using L13300, " CANCELLED       "
L13300:              FMT POS(22), CH(17)
                write #26
                goto L13260

        changes_history_line   /* Update Changes History for Lines     */
            if changes% <> 1%  then return
            if update$   = "A" then L13700     /* Don't write adds      */
            if comment$ <> "Y" then L13410     /* Comment Line          */
                if update$ = "C" and str(oldline$(),57,32) =             ~
                            str(newline$(),57,32) then return

L13410:     put #17 using L13430, str(oldline$(),,28), stamp$, update$,   ~
                                 str(oldline$())
L13430:         FMT CH(28), CH(7), CH(1), CH(700)
            if update$ = "D" then L13680       /* Always write deletes  */
         /* For changes, blank fields that don't imply a change, then  */
         /* test if anything has really changed.                       */
            init(" ")                                                    ~
             str(oldline$(), 26, 3), str(newline$(), 26, 3), /* Item   */~
             str(oldline$(),125, 8), str(newline$(),125, 8), /* Extn   */~
             str(oldline$(),148, 6), str(newline$(),148, 6), /* Rcv dte*/~
             str(oldline$(),177, 1), str(newline$(),177, 1), /* OH Post*/~
             str(oldline$(),181,16), str(newline$(),181,16), /* Pckng# */~
             str(oldline$(),222,66), str(newline$(),222,66), /* PORLSE */~
             str(oldline$(),306, 4), str(newline$(),306, 4), /* TextID */~
             str(oldline$(),373),    str(newline$(),373)     /* RcvQtys*/~
                                                             /* HnyCsts*/

            get str(oldline$()) using L13600, oldrtd, oldopn
            get str(newline$()) using L13600, newrtd, newopn
L13600:         FMT XX(100), 2*PD(14,4) /* Rcvd-to-Date, Open          */
            oldord = oldrtd + oldopn
            neword = newrtd + newopn

            if abs(oldord - neword) >= .01        then L13680
            if str(oldline$()) <> str(newline$()) then L13680
                return                   /* Just a Toss      */

L13680:     write #17, eod goto L13700    /* EOD assumes restarting     */

L13700: changes_history_master /* Update Changes History for Header    */

            if changes% <> 1% then return
            if poact$   = " " then return   /* Add & Delete in Input   */
            if poacth$  = poact$ then return

            if poact$ <> "C" then L13920  /* Always write A/D           */

            str(oldhdr1$()) = str(oldhdr$())
            str(newhdr1$()) = str(newhdr$())

            init (" ")                                                   ~
             str(oldhdr1$(),235,16),str(newhdr1$(),235,16),/* Last Inv */~
             str(oldhdr1$(),471, 6),str(newhdr1$(),471, 6),/* Due Date */~
             str(oldhdr1$(),480, 6),str(newhdr1$(),480, 6),/* Rcv Date */~
             str(oldhdr1$(),486, 3),str(newhdr1$(),486, 3),/* Dflt Byr */~
             str(oldhdr1$(),562, 4),str(newhdr1$(),562, 4),/* Text ID  */~
             str(oldhdr1$(),590, 3),str(newhdr1$(),590, 3),/* Last Seq */~
             str(oldhdr1$(),593,18),str(newhdr1$(),593,18),/* Audit    */~
             str(oldhdr1$(),611,16),str(newhdr1$(),611,16) /* PO Amts  */
            if str(oldhdr1$()) = str(newhdr1$()) then poact$ = "T"

L13920:     diskkey$ = str(newhdr$(),,25) & stamp$
            call "READ101" (#16, diskkey$, f1%(16))
               if f1%(16) = 0% then L13990
            get #16, using L13960, poacth$
L13960:         FMT POS(33), CH(1)
            if poacth$  = poact$ then return

L13990:     put #16 using L14010, str(newhdr$(),,25), stamp$, poact$,     ~
                                 str(oldhdr$()), " "
L14010:         FMT CH(25), CH(7), CH(1), CH(1030), CH(100)

            if f1%(16) = 0% then write #16 else rewrite #16
            poacth$ = poact$
            return


L29000: REM *************************************************************~
            *  INITIALIZE THE WHOLE MESS                                *~
            *************************************************************

            init (" ") newline$(), newpart$,  newseq$,                   ~
                       newstore$, newlot$, newjob$, newdue$, newtag$,    ~
                       porlsekey$, oldline$(), pj_job$,                  ~
                       oldpart$, oldseq$, oldstore$, oldjob$, oldtag$,   ~
                       oldlot$, olddue$, comment$, tag$, jobname$

            newquan = 0
            oldquan = 0
            pipquan, qrcvh, qqc, qqch = 0
            return

L30000: REM *************************************************************~
            *             L O A D   T R A N S A C T I O N               *~
            * --------------------------------------------------------- *~
            *  Load lines from buffer and all of PO from Master.        *~
            *  Determine how many lines need to be dealt with.          *~
            *************************************************************

            i% = 0%:gosub L29000
            call "PLOWNEXT" (#10, diskkey$(1), 25%, f1%(10))
            call "PLOWNEXT" (#6,  diskkey$(2), 25%, f1%( 6))

            if f1%(6) = 0% and f1%(10) = 0% then return  /* All Done */

            if f1%(6) <> 0% then L30180
                  diskkeyh$(1) = diskkey$(1)    /* Process Buffer */
                  goto L30370

L30180:     if f1%(10) <> 0% then L30230
                  diskkeyh$(2) = diskkey$(2)    /* Process Line   */
                  goto L30370

L30230:     if diskkey$(1) <> diskkey$(2) then L30280
               diskkeyh$(1) = diskkey$(1)       /* All O.K. in Step */
               diskkeyh$(2) = diskkey$(2)       /* Process Both   */
               goto L30370

L30280:     if diskkey$(1) < diskkey$(2) then L30330
               diskkeyh$(2) = diskkey$(2)       /* Process Line   */
               f1%(10) = 0%                     /* Put Buffer on Hold */
               goto L30370

L30330:        diskkeyh$(1) = diskkey$(1)       /* Process Buffer */
               f1%(6) = 0%                      /* Put Line on Hold */
               goto L30370

L30370:     i% = 999%
            if f1%(10) = 0% then L30590
                get #10, using L30410, seq$, part$, quan, due$, lot$,     ~
                                      job$, store$, porlsekey$, status$, ~
                                      piptag$, kitcomp$, prtoptions$,    ~
                                      bomid$, contract$
L30410:             FMT XX(25), CH(3), XX(3), CH(25), XX(52), PD(14,4),  ~
                        XX(25), CH(6), XX(12), CH(6), CH(8), CH(3),      ~
                        XX(45), CH(66), XX(22), CH(1), POS(534), CH(19), ~
                        CH(1), CH(4), CH(3), CH(20)
                convert seq$ to i%, data goto L30450 /* Not Expected */
L30450:         get #10, using L30460, newline$(1),newline$(2),newline$(3)
L30460:              FMT CH(250), CH(250), CH(200)

                if str(newline$(1%),57%,1%) = hex(ff) then L30480
                if pj_on% <> 1% then L30468
                     if job$ = "PJ" then gosub new_purchase_job
L30468:         gosub determine_tag
                if piptag$ = " " then L30480
                if tag$ <> piptag$ then gosub re_tag_everything

L30480:         newseq$      = seq$
                newpart$     = part$
                newquan      = quan
                newdue$      = due$
                newlot$      = lot$
                newjob$      = job$
                newstore$    = store$
                newst$       = status$
                newtag$      = tag$
                if str(newline$(1),57,1) = hex(ff) then                  ~
                            newseq$ = hex(ff) /* Deleted Line Flag   */

L30590
*        Now load in Line Item from Master (old guys)

                if f1%(6%) = 0% then L30760

                get #6,  using L30642, seq$, part$, quan, due$, lot$,     ~
                                      job$, store$, status$, qrcvh, qqc, ~
                                      qqch
L30642:             FMT XX(25), CH(3), XX(3), CH(25), XX(52), PD(14,4),  ~
                        XX(25), CH(6), XX(12), CH(6), CH(8), CH(3),      ~
                        XX(45), XX(66), XX(22), CH(1), POS(373),         ~
                        3*PD(14,4)
                convert seq$ to i%, data goto L30660 /* Not Expected */
L30660:         get #6,  using L30460, oldline$(1),oldline$(2),oldline$(3)

                gosub determine_tag

                oldseq$      = seq$
                oldpart$     = part$
                oldquan      = quan
                oldlot$      = lot$
                oldjob$      = job$
                oldstore$    = store$
                olddue$      = due$
                oldst$       = status$
                oldtag$      = tag$

L30760:         diskkey$(1) = diskkeyh$(1)  /* Set Key to find correct */
                diskkey$(2) = diskkeyh$(2)  /* 'Next' Records          */

                return

        new_purchase_job
            if pj_on% <>   1% then return
            if job$   <> "PJ" then return

*        Get New Purchase Job #
L34050:     plowkey$ = "SWITCHS.SFC"
            call "READ101" (#3, plowkey$, f1%(3%))
                if f1%(3%) = 0% then return
            get #3, using L34090, npj_no$, pj_incr%
L34090:         FMT POS(115), CH(8), BI(4)
            pj_no$, temp$ = npj_no$
                for j% = 1% to 8%
                     if str(temp$,j%,1%) < "0" or str(temp$,j%,1%) > "9" ~
                          then str(temp$,j%,1%) = " "
                next j%
            temp% = pos(-temp$ <> " ")
            auto%(1%) = pos(-str(temp$,1%,temp%) = " ") + 1%
            auto%(2%) = (temp% - auto%(1%)) + 1%
            convert str(temp$, auto%(1%), auto%(2%)) to job%
            job% = job% + pj_incr%
            convert job% to temp$, pic(00000000)
            str(npj_no$, auto%(1%), auto%(2%)) =                         ~
                                    str(temp$, 8% - auto%(2%) + 1%)
            put #3 using L34240, npj_no$
L34240:         FMT POS(115), CH(8)
            rewrite #3

*        Now Test the Purchase Job Number for Uniqueness
            call "READ100" (#27, pj_no$, f1%(27%))
                if f1%(27%) = 1% then L34050
            jobname$ = "Pur Job-PO " & str(ponumber$,1%,14%) & " " &     ~
                       str(seq$,1%,3%)
            pj_job$ = "Y"
            return

        determine_tag
            tag$ = " "
            tag$ = "PO" & str(ponumber$,1%,14%) & str(seq$,1%,3%)
            if pj_on% <> 1% then return
            if job$ <> "PJ" and str(job$,1%,2%) <> "PJ" then return
                if job$ = "PJ" then tag$ = "JOB ORDER: " & pj_no$
                if job$ <> "PJ" and str(job$,1%,2%) = "PJ" then          ~
                                    tag$ = "JOB ORDER: " & job$
            return

        re_tag_everything
            call "JBRETAG"               /* Change PIP Tags from PO    */~
                       (piptag$,         /* TAG CURRENTLY ON FILE      */~
                        tag$,            /* TAG IT WILL BECOME         */~
                        #31,             /* JBCROSS2                   */~
                        #23,             /* PIPMASTR                   */~
                        #28,             /* WCOUT                      */~
                        #21,             /* PIPIN                      */~
                        #25,             /* PIPOUT                     */~
                        #26,             /* PIPCROSS                   */~
                        #34)             /* JBPIPXRF                   */
            return

        purchase_job_create
*        Now create the new Job (JBMASTR2)
            control$ = " "
            if ctl_op$ = "C" then L35000
                plowkey$ = newtag$
                call "PLOWALTS" (#26, plowkey$, 1%, 19%, f1%(26%))
                     if f1%(26%) = 0% then L35000
                get #26 using L34880, control$
L34880:              FMT CH(19)

L35000:     write #27 using L35100, pj_no$, jobname$, newtag$, newpart$,  ~
                                   newquan, 0, " ",                      ~
                                   str(newline$(),1,28%), " ",           ~
                                   str(newhdr$(),451%,6%), blankdate$,   ~
                                   str(newline$(),133%,9%),              ~
                                   str(newhdr$(),451%,6%), newdue$,      ~
                                   newquan, " ", str(z_roes$(),,20%),    ~
                                   str(z_roes$()), control$, 0,          ~
                                   str(z_roes$(),,104%), " "

L35100:         FMT  CH(8), CH(30), CH(19),                              ~
                     CH(25), 2*PD(14,4), CH(9), CH(28), CH(11), CH(6),   ~
                     CH(6), CH(9), CH(6), CH(6), PD(14,4), CH(24),       ~
                     CH(20), CH(888),                                    ~
                     CH(19), PD(14,4), CH(104), CH(50)

            call "CDANPOST" (#27, "A")

*        Now Check for Print options
            if str(prtoptions$,1%,1%) <> "Y" then L35220   /* Traveller */
                call "JBTRAVEL" (newpart$, pj_no$, newquan, #30, #29,    ~
                                #28, #31, #2, #32, #33, #3, #27)
L35220:     if str(prtoptions$,2%,1%) <> "Y" then L35250   /* Pick List */
                call "JBPICKSL" (pj_no$, pj_no$, #27, #25, #28, #2, #1,  ~
                                #3, 0%, basedate$, 490%)
L35250:     if str(prtoptions$,3%,1%) <> "Y" then L35280  /* By-Product */
                call "JBPICKBP" (pj_no$, pj_no$, #27, #25, #28, #2,      ~
                                #1, #3, 0%, basedate$, 490%)
L35280:     if str(prtoptions$,4%,1%) <> "Y" then L35330  /* Sin Lvl BOM */
                call "BOMSLSUB" (newpart$, bomid$, "N", "N", "N", "N",   ~
                                #32, #2, #3, #3, #33, #3, " ", " ", " ", ~
                                pj_no$)

L35330:     close printer

            REM All thats left is to kit, if requested...
            if kitcomp$ <> "Y" then L35700

            kitstore$ = defkitstr$
            if kitstore$ = " " then kitstore$ = newstore$

            if postflag% = 0 then L35480
                call "JNLINFO" (modno$, jnlid$, pstseq%, " ", " ",       ~
                                             sfcdate$, #3, f2%(3%), 0%)
                postflag% = 0

L35480:     REM Kit It Complete...
            call "JB2TIF"               /* Writes to Transaction Image */~
                 ("J1",                 /* Send transaction to JBPOST1 */~
                  1%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  3%,                   /* Transaction type (1 = kit)  */~
                  hex(40),              /* Priority                    */~
                  pj_no$,               /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  sfcdate$,             /* G/L posting Date            */~
                  " ",                  /* Inventory Part Code         */~
                  kitstore$,            /* Inventory Store Code        */~
                  newlot$,              /* Inventory Lot Id.           */~
                  0,                    /* Quantity to process         */~
                  " ",                  /* Not used                    */~
                  " ",                  /* Not used                    */~
                  " ")                  /* Not used                    */


L35700
*        Check for JBCROSS2
            call "READ100" (#31, newtag$, f1%(31%))
                if f1%(31%) = 1% then return
            write #31 using L35750, newpart$, " ", newtag$, newpart$,     ~
                                   bomid$, newtag$
L35750:         FMT CH(25), CH(3), CH(19), CH(25), CH(3), CH(19)
            return

        purchase_job_update
            if newjob$ = " " then return
            if str(newjob$,1%,2%) <> "PJ" then return
            if oldquan = newquan and olddue$ = newdue$ then return
                call "READ101" (#27, newjob$, f1%(27%))
                     if f1%(27%) = 0% then return
                get #27 using L35870, jbqtycomp
L35870:              FMT POS(91), PD(14,4)
                jbqtymake = newquan + jbqtycomp + qrcvh + qqc + qqch
                if jbqtymake < 0 then jbqtymake = 0
                put #27 using L35920, jbqtymake, newdue$
L35920:              FMT POS(83), PD(14,4), POS(174), CH(6)
                rewrite #27
            return

        REM *************************************************************~
            * ASK USER ERROR ACKNOWLEDGEMNTS AND BAIL-OUTS              *~
            *************************************************************

        date_error:


            if tt$ = "F" then gp$ = "I " else gp$ = "O "
            call "GETPARM" addr(gp$, "A", "VBKUPDTE", " ", "0001",       ~
                                "VBKUPD", 2%, askpf1$, 50%, askpf2$, 50%)

            goto L65000

L65000: REM *************************************************************~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * CLOSES ALL THE FILES CURRENTLY OPEN.                      *~
            *************************************************************

            if postflag% = 0% then call "JBJNLCLS"                       ~
                           ("J1", userid$, modno$, jnlid$, pstseq%, 0%)
            close printer

            call "MESSAGE" addr("DE", port$, return%)
            end
