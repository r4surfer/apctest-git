        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  RRRR    CCC   V   V  U   U  PPPP   DDDD   TTTTT  EEEEE   *~
            *  R   R  C   C  V   V  U   U  P   P  D   D    T    E       *~
            *  RRRR   C      V   V  U   U  PPPP   D   D    T    EEEE    *~
            *  R   R  C   C   V V   U   U  P      D   D    T    E       *~
            *  R   R   CCC     V     UUU   P      DDDD     T    EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * RCVUPDTE - DRIVER FOR POSTING AND UPDATING RECEIVER FILES *~
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
            * 06/13/86 ! ORIGINAL                                 ! KAB *~
            * 05/18/87 ! Standard Costing Enhancements            ! ERN *~
            * 10/09/87 ! Fixed SELECT Statement for JBVALUE2      ! DAW *~
            * 06/14/90 ! Fixed PRR's 10518.                       ! SID *~
            *          !    HNYPROC File received date field will !     *~
            *          !    now contain the user input received   !     *~
            *          !    date and not the defaulted received   !     *~
            *          !    date from the USERINFO File.          !     *~
            * 08/29/90 ! G/L Export file modifications            ! RAC *~
            * 09/22/93 ! Purchase Jobs - extend RCVHNYTF/DS       ! KAB *~
            * 07/14/94 ! Passed file channel for RCVTIF to UPSUB  ! ERN *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim diskkey$100,                 /* DISK KEY FOR PLOW ROUTINES */~
            nextpokey$50,                /* NEXT PO TO PLOW IN BUFFER  */~
            oldreadkey$100,              /* PLOW KEY                   */~
            userid$3,                    /* USERID OF THIS USER        */~
            header$255,                  /* HOLDS ENTIRE RECORD        */~
            hnydate$6,                   /* HNYPOSTING DATE            */~
            rcvddate$6,                  /* REVEIVED DATE              */~
            modno$2,                     /* PUR JOB MODULE             */~
            jnlid$3                      /* PUR JOB JOURNAL            */~

        dim f2%(64),                     /* FILE STATUS FLAGS          */~
            fs%(64),                     /* FILE STATUS FLAGS          */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20                  /* RETURN CODE FROM "FILEOPEN"*/~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !          D E S C R I P T I O N           *~
            *-----+----------+------------------------------------------*~
            * # 1 ! HNYQUAN  ! INVENTORY QUANTITY FILE                  *~
            * # 2 ! HNYDETAL ! INVENTORY TRANSACTION DETAIL FILE        *~
            * # 3 ! SYSFILE2 ! SYSTEM CONTROL FILE                      *~
            * # 5 ! VBKMASTR ! BACKLOG MAIN HEADER FILE.                *~
            * #11 ! VBKLINES ! BACKLOG LINE ITEMS                       *~
            * # 6 ! HNYMASTR ! Inventory Master File                    *~
            * #15 ! USERINFO ! USER INFORMATION FILE                    *~
            * #14 ! POGLBUFF ! INTERFACE FILE FOR PO TO GL              *~
            * #16 ! HNYPOOLS ! INVENTORY POOL FILES                     *~
            * #18 ! PIPMASTR ! PLANNED INV. POSITION MASTER             *~
            * #19 ! PIPIN    ! PIP INCOMING DETAIL FILE                 *~
            * #20 ! PIPOUT   ! PIP OUTGOING DETAIL FILE                 *~
            * #21 ! SFCUM2   ! SFCUM2                                   *~
            * #23 ! JOBMASTR ! JOB MASTER FILE                          *~
            * #22 ! JBMASTR2 ! PROJECT MASTER FILE                      *~
            * #24 ! JBVALUE2 ! JOB VALUE DETAIL FILE                    *~
            * #25 ! JOBPURCH ! PROJECT VALUE DETAIL FILE                *~
            * #26 ! GLMAIN   ! General Ledger Main File                 *~
            * #27 ! GLDETAIL ! General ledger detail file               *~
            * #28 ! HNYPROC  ! Inventory where procurred from detail fi *~
            * #29 ! RCVJRNTF ! HNY ADJUSTMENTS GL JOURNAL FILE          *~
            * #3x ! RCVxxxxx ! ..........                               *~
            * #40 ! TXTFILE  ! TEXT FILE                                *~
            * #41 ! SERTIF   ! Additions buffer for inventory S/N's     *~
            * #42 ! SERMASTR ! Serial Number Tracking Master File       *~
            *************************************************************

            select  #1, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 650,                                   ~
                        keypos = 17, keylen = 44,                        ~
                        alternate key 1, keypos =  1, keylen = 44

            select  #2, "HNYDETAL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 42,                         ~
                        alternate key 1, keypos = 43, keylen = 6, dup,   ~
                                  key 2, keypos = 49, keylen = 2, dup

            select  #3, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select # 6, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #11, "VBKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 28

            select  #15, "USERINFO",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1,  keylen = 3

            select #16,  "HNYPOOL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 38

            select #18,  "PIPMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos = 2, keylen = 25,                        ~
                         alternate key 1, keypos = 1, keylen = 26

            select #19,  "PIPIN",                                        ~
                          varc,                                          ~
                          indexed,                                       ~
                          recsize = 60,                                  ~
                          keypos =30, keylen = 19,                       ~
                          alt key 1, keypos = 01, keylen = 48

            select #20,  "PIPOUT",                                       ~
                          varc,                                          ~
                          indexed,                                       ~
                          recsize = 64,                                  ~
                          keypos = 1, keylen = 56,                       ~
                          alt key 1, keypos = 20, keylen = 37

            select #21,  "SFCUM2",                                       ~
                          varc,                                          ~
                          indexed,                                       ~
                          recsize = 1985,                                ~
                          keypos = 1, keylen = 25

            select #22, "JBMASTR2",                                      ~
                          varc,                                          ~
                          indexed,                                       ~
                          recsize = 1300,                                ~
                          keypos = 1, keylen = 8

            select #23,  "JOBMASTR",                                     ~
                          varc,                                          ~
                          indexed,                                       ~
                          recsize =  700,                                ~
                          keypos = 1, keylen = 8

            select #24,  "JBVALUE2",                                     ~
                          varc,                                          ~
                          indexed,                                       ~
                          recsize = 300,                                 ~
                          keypos = 1, keylen = 23

            select #25,  "JOBPURCH",                                     ~
                          varc,                                          ~
                          indexed,                                       ~
                          recsize = 200,                                 ~
                          keypos = 1, keylen = 16

            select #26, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =   9                      ~

            select #27, "GLDETAIL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  160,                                  ~
                        keypos =    1, keylen =  26                      ~

            select #28, "HNYPROC",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  134,                                  ~
                        keypos =   32, keylen =  40,                     ~
                        alt key  1, keypos =    7, keylen =  65,         ~
                            key  2, keypos =    1, keylen =  40, dup,    ~
                            key  3, keypos =   41, keylen =  31, dup     ~

            select #29, "RCVJRNTF",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 670,                                  ~
                         keypos = 10, keylen = 10                        ~

            select #30, "RCVMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos= 1, keylen = 16                           ~

            select #31, "RCVLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 800,                                   ~
                        keypos= 26, keylen = 52,                         ~
                        alt key 1, keypos =  1, keylen = 69,             ~
                            key 2, keypos = 42, keylen = 36,             ~
                            key 3, keypos =128, keylen = 24              ~

            select #32, "RCVHNYDS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos= 1, keylen = 86,                          ~
                        alt key 1, keypos = 45, keylen = 42              ~

            select #33, "PAYLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 541,                                   ~
                        keypos = 36, keylen = 28,                        ~
                        alternate key 1, keypos = 1, keylen = 63,        ~
                                  key 2, keypos = 17, keylen = 47

            select #34, "RCVHNYRP",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 400,                                   ~
                        keypos= 29, keylen = 55,                         ~
                        alt key 1, keypos =  1, keylen = 83              ~

            select #35, "RCVTIF",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos= 12, keylen = 16,                         ~
                        alt key 1, keypos = 1, keylen = 11               ~

            select #36, "RCVTIF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 800,                                   ~
                        keypos= 26, keylen = 52,                         ~
                        alt key 1, keypos =  1, keylen = 69,             ~
                            key 2, keypos = 42, keylen = 36,             ~
                            key 3, keypos =128, keylen = 24              ~

            select #37, "RCVHNYTF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos= 1, keylen = 86,                          ~
                        alt key 1, keypos = 45, keylen = 42              ~

            select #38, "RCVQCREJ",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos= 26, keylen = 17,                         ~
                        alt key 1, keypos = 1, keylen = 42               ~

            select #40, "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos= 1, keylen = 11                           ~

            select #41, "SERTIF",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos = 1, keylen = 62

            select #42, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            call "SHOSTAT"  ("Opening Files; One Moment Please.")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1),   0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2),   0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3),   0%, rslt$( 3))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6),   0%, rslt$( 6))
            call "OPENCHCK" (#11, fs%(11), f2%(11),   0%, rslt$(11))
            call "OPENCHCK" (#15, fs%(15), f2%(15),   0%, rslt$(15))
            call "OPENCHCK" (#16, fs%(16), f2%(16),   0%, rslt$(16))
            call "OPENCHCK" (#18, fs%(18), f2%(18),   0%, rslt$(18))
            call "OPENCHCK" (#19, fs%(19), f2%(19),   0%, rslt$(19))
            call "OPENCHCK" (#20, fs%(20), f2%(20),   0%, rslt$(20))
            call "OPENCHCK" (#21, fs%(21), f2%(21),   0%, rslt$(21))
            call "OPENCHCK" (#22, fs%(22), f2%(22),   0%, rslt$(22))
            call "OPENCHCK" (#23, fs%(23), f2%(23),   0%, rslt$(23))
            call "OPENCHCK" (#26, fs%(26), f2%(26),   0%, rslt$(26))
            call "OPENCHCK" (#27, fs%(27), f2%(27),   0%, rslt$(27))
            call "OPENCHCK" (#28, fs%(28), f2%(28),   0%, rslt$(28))
            call "OPENCHCK" (#29, fs%(29), f2%(29), 300%, rslt$(29))
            call "OPENCHCK" (#30, fs%(30), f2%(30), 100%, rslt$(30))
            call "OPENCHCK" (#31, fs%(31), f2%(31), 300%, rslt$(31))
            call "OPENCHCK" (#32, fs%(32), f2%(32), 300%, rslt$(32))
            call "OPENCHCK" (#33, fs%(33), f2%(33),   0%, rslt$(33))
            call "OPENCHCK" (#34, fs%(34), f2%(34), 300%, rslt$(34))
            call "OPENCHCK" (#35, fs%(35), f2%(35),   0%, rslt$(35))
            call "OPENCHCK" (#36, fs%(36), f2%(36),   0%, rslt$(36))
            call "OPENCHCK" (#37, fs%(37), f2%(37),   0%, rslt$(37))
            call "OPENCHCK" (#38, fs%(38), f2%(38), 100%, rslt$(38))
            call "OPENCHCK" (#40, fs%(40), f2%(40),   0%, rslt$(40))
            call "OPENCHCK" (#41, fs%(41), f2%(41),   0%, rslt$(41))
            call "OPENCHCK" (#42, fs%(42), f2%(42),   0%, rslt$(42))

        REM *************************************************************~
            *                 I N I T I A L I Z A T I O N               *~
            *                                                           *~
            * INITIALIZES KEYS, THAT SORT OF THING.                     *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            nextpokey$ = all(hex(00)):str(nextpokey$,,3) = userid$

            REM RETRIEVE INVENTORY DATE FOR THIS USER
                call "READ100" (#15, userid$, f1%(15))
                      if f1%(15) = 0 then L65000
                get #15, using L09150, hnydate$
L09150:                 FMT XX(9), CH(6)
                call "WHICHMON" (#3, hnydate$, whichmonth%)
                     if whichmonth% < 1 or whichmonth% > 3 then L65000

        REM *************************************************************~
            *                                                           *~
            *                   M A I N   P R O G R A M                 *~
            *                                                           *~
            *************************************************************

            call "SHOSTAT" ("Updating Vendor Backlog")

L10080:     call "PLOWALTS" (#35, nextpokey$, 1%, 3%, f1%(35))
                if f1%(35) = 0% then L65000
            get #35, using L10101, str(header$,,139)
L10101:         FMT POS(12), CH(139)
            init (" ") str(header$,140)
            oldreadkey$ = str(header$,,16)
            rcvddate$ = str(header$,86,6)   /* User input Received Date */
            gosub process_lines

            diskkey$ = str(header$,,16)
            call "DELETE" (#30, diskkey$, 16%)
            call "DELETE" (#35, diskkey$, 16%)
            init (hex(00)) str(diskkey$,17)
            call "PLOWNEXT" (#31, diskkey$, 16%, f1%(31))
                if f1%(31) = 0% then L10080
            write #30, using L10181, str(header$,,150)
L10181:           FMT CH(150)
            goto L10080

        REM *************************************************************~
            *                                                           *~
            *  LINE ITEM PROCESSING HAPPENS HERE (NO ARRAYS)            *~
            *                                                           *~
            *************************************************************

        process_lines
            call "RCVUPSUB"                                              ~
                       (oldreadkey$, 16%, rcvddate$,                     ~
                        modno$, jnlid$, pstseq%,                         ~
                        #1,              /* HNYQUAN  */                  ~
                        #2,              /* HNYDETAL */                  ~
                        #3,              /* SYSFILE2 */                  ~
                        #6,              /* HNYMASTR */                  ~
                       #11,              /* VBKLINES */                  ~
                       #15,              /* USERINFO */                  ~
                       #16,              /* HNYPOOL  */                  ~
                       #18,              /* PIPMASTR */                  ~
                       #19,              /* PIPIN    */                  ~
                       #20,              /* PIPOUT   */                  ~
                       #21,              /* SFCUM2   */                  ~
                       #22,              /* JBMASTR2 */                  ~
                       #23,              /* JOBMASTR */                  ~
                       #24,              /* JBVALUE2 */                  ~
                       #25,              /* JOBPURCH */                  ~
                       #26,              /* GLMAIN   */                  ~
                       #27,              /* GLDETAIL */                  ~
                       #28,              /* HNYPROC  */                  ~
                       #29,              /* RCVJRNTF */                  ~
                       #31,              /* RCVLINES */                  ~
                       #32,              /* RCVHNYDS */                  ~
                       #33,              /* PAYLINES */                  ~
                       #34,              /* RCVHNYRP */                  ~
                       #35,              /* RCVTIF   */                  ~
                       #36,              /* RCVTIF2  */                  ~
                       #37,              /* RCVHNYTF */                  ~
                       #38,              /* RCVQCREJ */                  ~
                       #40,              /* TXTFILE  */                  ~
                       #41,              /* SERTIF   */                  ~
                       #42)              /* SERMSTR  */                  ~

            return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            if pstseq% <> 0% then call "JBJNLCLS" ("J2", userid$,        ~
                                       modno$, jnlid$, pstseq%, f1%(64))
            end
