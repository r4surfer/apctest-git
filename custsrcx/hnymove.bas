        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  H   H  N   N  Y   Y  M   M   OOO   V   V  EEEEE          *~
            *  H   H  NN  N  Y   Y  MM MM  O   O  V   V  E              *~
            *  HHHHH  N N N   YYY   M M M  O   O  V   V  EEEE           *~
            *  H   H  N  NN    Y    M   M  O   O   V V   E              *~
            *  H   H  N   N    Y    M   M   OOO     V    EEEEE          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYMOVE  - ALLOWS DIRECT MOVEMENT OF INVENTORY FROM ONE   *~
            *            STORE LOT TO ANOTHER STORE LOT ONLY IF THE     *~
            *            AVAILABLE - COMMITTED QUANTITY IN THE MOVE FROM*~
            *            STORE LOT IS ADEQUATE.  SHOWS QUANTITIES       *~
            *            FOR ALL STORES LOTS.                           *~
            *----------------------------------------------------------Q*~
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
            * 06/07/82 ! ORIGINAL                                 ! GLW *~
            * 07/13/83 ! CALL TO 'MANUAL' ADDED                   ! HES *~
            * 07/13/83 ! CALLS TO 'FILEOPEN' CHANGED TO 'OPENFILE'! HES *~
            * 08/31/84 ! ADDED INIT BLOCK & RENUMBERED MESS       ! KAB *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION - ! RAC *~
            *          ! CHANGES INCLUDED GLPOST, HNYPOST, JNLINFO!     *~
            *          !(NEW),GLPRTSUB(NEW), INCREASE IN GL BUFFER!     *~
            *          ! INTERFACE FILE RECORD SIZE FOR PASSING   !     *~
            *          ! MODULE, JOURNAL, AND POSTING SEQUENCE    !     *~
            * 02/05/87 ! Lot Tracking Enhancements.  Also redid   ! ERN *~
            *          !  screens and report. Added Serial Nos.   !     *~
            * 03/04/87 ! Removed Quantity Committed from display  ! JRH *~
            * 05/07/87 ! Fixed bug- not recording serial numbers  ! ERN *~
            *          !   after first posting occurred.          !     *~
            * 05/12/87 ! Std cost mods to files, screen & CALLs   ! JIM *~
            * 03/22/89 ! Now allows move from Store/Lot if result ! RJM *~
            *          !   is a Negative Lot IF Neg. Lots are     !     *~
            *          !   allowed for Part. Askuser warning appears.   *~
            * 06/06/89 ! Changed name of Return Code in call to   ! MJB *~
            *          !  JNLINFO so that it would increment seq# !     *~
            *          !  It was being reset in HNYPST2 call.     !     *~
            * 01/03/90 ! Modified code so like accounts will be   ! LAB *~
            *          ! combine for gl posting - Accountancy I   !     *~
            * 10/05/90 ! Merge A/C 1 and GL Export option.        ! JDH *~
            * 05/21/91 ! Added Lot Number checking for Mandatory  ! JDH *~
            *          !  Lot Tracked Parts. Thanks Marty.        !     *~
            *          !  Test to stop reads if GL Export is off. !     *~
            * 07/19/91 ! Added PF8 access to HNYLCSUB permitting  ! MLJ *~
            *          !  Location control.                       !     *~
            * 08/06/91 ! Expanded HNYLCSUB argument list for 'TO'.! JDH *~
            * 12/17/91 ! Ensured that JNL Seq# same for all moves.! JDH *~
            *          !  PRR 12099. Added UOM to screen.         !     *~
            * 12/24/91 ! Merry Xmas.  Top Str/Lot/Qty display was ! JDH *~
            *          !   lost after processing.  Fixed it.      !     *~
            * 12/23/92 ! Merry Xmas one year later.  PRR 12735    ! JDH *~
            *          !   matched variable names for GL Export.  !     *~
            *          ! Change text passed to HNYPST2.           !     *~
            * 09/21/93 ! Couldn't wait for Xmas!  Added warning   ! JDH *~
            *          !   if to lot was not the same as from lot.!     *~
            *          !   PRRs 12419 & 13027.                    !     *~
            * 01/14/94 ! Added to GLCMBSUB argument list.         ! JDH *~
            * 08/14/06 ! (AWD001) Mod to add GLORTRAN             ! CMG *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim date$8, hnydate$6, hnydatef$8, runtime$8, company$30,        ~
            gltext$100, jnlid$3, modno$2, summary$1, title$40,           ~
            errormsg$79, inpmessage$79, line2$79, mm$50,                 ~
            part$25, descr$32, newacct$9, oldacct$9,                     ~
            store$(10)3, lot$(10)6, q$(10,5)10, mq$12,                   ~
            fstore$3, flot$6, tstore$3, tlot$6, lfac$1,                  ~
            plowkey$99, readkey$99, userid$3,                            ~
            sn_location$30, sn_trankey$, cost(12), acct$(4)9, adjacct$16,~
            glamount(4), hnyflg$1, passedin_acct$(50)109, text$109,      ~
            passedin_dbcr(50,2), passedin_type$(50)2

        dim                              /* G/L Export Posting Info    */~
            export_on$1,                 /* G/L Export File processing?*/~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            partcat$4,                   /* Part Category code         */~
            partclass$4,                 /* Part Class code            */~
            partgen$16,                  /* Part Generic code          */~
            parttype$3,                  /* Part Type code             */~
            tran_type$5,                 /* G/L Transaction type       */~
            uom$4, uom_descr$32          /* Part Unit of measure       */

        dim division$3,                  /* division number (AWD001)   */~
            schema$8                     /* schema          (AWD001)   */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            fs%(32)                      /* = 1 if file open, -1 if it */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! SYSTEM INFORMATION FILE (INVENTORY DATE) *~
            * # 2 ! STORNAME ! STORE MASTER FILE                        *~
            * # 3 ! USERINFO ! USER INFORMATION FILE                    *~
            * # 4 ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * # 5 ! GENCODES ! General Codes File                       *~
            * # 6 ! HNYDETAL ! INVENTORY TRANSACTION DETAIL FILE        *~
            * # 7 ! HNYQUAN  ! INVENTORY QUANTITY INFORMATION FILE      *~
            * # 8 ! SERTIF   ! Additions buffer for inventory S/N's     *~
            * # 9 ! SERWORK  ! Temporary Serial #'s Work File           *~
            * #10 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #11 ! HNYPOOL  ! INVENTORY POOL FILES                     *~
            * #13 ! PIPMASTR ! PLANNED INV. POSITION MASTER             *~
            * #14 ! HNYADJPF ! INVENTORY ADJUSTMENT PRINT JOURNAL       *~
            * #15 ! HNYLOCNS ! Location Quantity Detail File            *~
            * #16 ! LOCATION ! Location Master File                     *~
            * #19 ! GLMAIN   ! General Ledger Main File                 *~
            * #20 ! GLDETAIL ! General ledger detail file               *~
            * #21 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #22 ! GLORTRAN ! GL Oracle transmit file     (AWD001)     *~
            * #23 ! GENCODES ! GENERAL CODES MASTER FILE   (AWD001)     *~
            * #50 ! WRKFILE  ! Hnypst2 workfile                         *~
            *************************************************************

            select  #1, "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            select  #2, "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 3

            select  #3, "USERINFO",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos = 1, keylen = 3

            select  #4, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90,  keylen = 4, dup

            select  #5,  "GENCODES",                                     ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select  #6, "HNYDETAL",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize =  150,                                 ~
                         keypos = 1, keylen = 42,                        ~
                         alternate key 1, keypos = 43, keylen = 6, dup,  ~
                                   key 2, keypos = 49, keylen = 2, dup

            select #7,  "HNYQUAN",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 650,                                  ~
                         keypos= 17, keylen = 44,                        ~
                         alternate key 1, keypos =  1, keylen = 44

            select #8,  "SERTIF",                                        ~
                        varc, indexed,  recsize =  100,                  ~
                        keypos = 1, keylen = 62

            select #9,  "SERWORK",                                       ~
                        varc, indexed,  recsize =  48,                   ~
                        keypos = 1, keylen = 23

            select #10, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #11,  "HNYPOOL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 38

            select #13,  "PIPMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos = 2, keylen = 25,                        ~
                         alternate key 1, keypos = 1, keylen = 26

            select #14,  "HNYADJPF",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 160,                                  ~
                         keypos = 1, keylen = 19                         ~

            select #15,  "HNYLOCNS",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 700,                                  ~
                         keypos = 1, keylen = 42,                        ~
                         alternate key 1, keypos = 443, keylen = 42,     ~
                                   key 2, keypos = 485, keylen = 42,     ~
                                   key 3, keypos = 527, keylen = 42,     ~
                                   key 4, keypos = 590, keylen = 42      ~

            select #16,  "LOCATION",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 400,                                  ~
                         keypos = 1, keylen = 11,                        ~
                         alternate key 1, keypos = 4, keylen = 11        ~

            select #19, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =   9                      ~

            select #20, "GLDETAIL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  160,                                  ~
                        keypos =    1, keylen =  26                      ~

            select #21, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =  25                      ~



/*(AWD001)*/
            select #22, "GLORTRAN",                                       ~
                        varc,     indexed, recsize = 512,                 ~
                        keypos = 1,    keylen = 33,                       ~
                        alt key 1, keypos = 31, keylen = 47,              ~
                            key 2, keypos = 81, keylen = 26


            select #23, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24
/*(AWD001)*/


            select #50, "WRKFILE",                                       ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 19                          ~

        call "SHOSTAT"  ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%( 1), f2%( 1),   0%, rslt$( 1))
            call "OPENCHCK" (#2,  fs%( 2), f2%( 2),   0%, rslt$( 2))
            call "OPENCHCK" (#3,  fs%( 3), f2%( 3),   0%, rslt$( 3))
            call "OPENCHCK" (#4,  fs%( 4), f2%( 4),   0%, rslt$( 4))
            call "OPENCHCK" (#5,  fs%( 5), f2%( 5),   0%, rslt$( 5))
            call "OPENCHCK" (#6,  fs%( 6), f2%( 6),   0%, rslt$( 6))
            call "OPENCHCK" (#7,  fs%( 7), f2%( 7),   0%, rslt$( 7))
            call "OPENCHCK" (#11, fs%(11), f2%(11),   0%, rslt$(11))
            call "OPENCHCK" (#13, fs%(13), f2%(13),   0%, rslt$(13))
            call "OPENCHCK" (#14, fs%(14), f2%(14),   0%, rslt$(14))
            call "OPENCHCK" (#15, fs%(15), f2%(15),   0%, rslt$(15))
            call "OPENCHCK" (#16, fs%(16), f2%(16),   0%, rslt$(16))
            call "OPENCHCK" (#19, fs%(19), f2%(19),   0%, rslt$(19))
            call "OPENCHCK" (#20, fs%(20), f2%(20),   0%, rslt$(20))
            call "OPENCHCK" (#21, fs%(21), f2%(21),   0%, rslt$(21))
/*(AWD001)*/
            call "OPENCHCK" (#22, fs%(22), f2%(22), 100%, rslt$(22))
            call "OPENCHCK" (#23, fs%(23), f2%(23),   0%, rslt$(23))
/*(AWD001)*/
            call "WORKOPEN" (#50, "IO", 100%, f2%(50))

            prtline = 100
            select printer(134)
            call "SETPRNT" ("HNY037", " ", 0%, 0%)

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

                                                          /* (AWD001)  */

            schema_err%, schema% = 0%
            init(" ") schema$, division$
            call "SCHEMA" (schema$, schema%, #23, schema_err%)


            if schema% = 1% then division$ = "036"   /* NC */
            if schema% = 2% then division$ = "080"   /* NE */
            if schema% = 3% then division$ = "035"   /* AES*/


            date$ = date : call "DATEFMT" (date$)
            str(line2$,63) = "HNYMOVE: " & str(cms2v$,,8)
            call "COMPNAME" (02%, company$, u3%) : u3% = u3%
            call "TIME" (runtime$)
            ll% = 6%

*        Get Inventory Date Information.
            call "EXTRACT" addr ("ID", userid$)
            call "READ100" (#3, userid$, f1%(3))
            if f1%(3) = 1% then L09200
                call "ASKUSER" (0%, "***ERROR***",                       ~
                     "Unable to locate your Inventory Posting Date!",    ~
                     " ", "Press (RETURN) to acknowledge and Exit.")
                goto exit_program
L09200:     get #3, using L09210, hnydate$
L09210:         FMT XX(27), CH(6)
            call "WHICHPER" (#1, hnydate$, thismonth%)
            if thismonth% > 0% then L09300
                call "ASKUSER" (0%, "INVALID POSTING DATE",              ~
                     "Your Inventory Posting Date is not within the " &  ~
                     "posting window.",                                  ~
                     "Please change your posting date & try again.",     ~
                     "Press (RETURN) to acknowledge and Exit.")
                goto exit_program
L09300:     hnydatef$ = hnydate$  :  call "DATEFMT"(hnydatef$)

*        See if G/L Export is on
            export_on$ = "N"
            call "READ100" (#1, "SWITCHS.GL", f1%(1))
            if f1%(1) = 1% then get #1 using L09307, export_on$
L09307:         FMT POS(22), CH(1)

            REM GET SYSFILE2 RECORD TO DETERMINE ACCOUNT FOR VARIANCE POST
                call "READ100" (#1, "SWITCHS.HNY", f1%(1))
                      if f1%(1) = 0 then exit_program
                get #1, using L09314, hnyflg$
L09314:               FMT POS(96), CH(1)

*        Journal Data elements
            jnlid$ = "IMV"
            modno$ = "04"
            jnlreturn% = 1%
            call "JNLINFO" (modno$, jnlid$, pstseq%, summary$,           ~
                            title$, hnydate$, #1, f2%(1), jnlreturn%)
            call "STRING" addr("RJ", title$, 40%)
            postflag% = 1%

L10000
*       ********   TOP OF LOOP   ***************************************

        init (" ")  errormsg$, part$, descr$, newacct$, oldacct$,        ~
                    store$(), lot$(), q$(), fstore$, flot$, tstore$,     ~
                    tlot$, readkey$, plowkey$, mm$, mq$, uom$, uom_descr$

        totalcost, mq, avail = 0 : mat cost = zer

        inpmessage$ = "Enter the Part number to move quantities for."
L10070: accept                                                           ~
           at(01,02), "Move Inventory Quantities from one Store/Lot to an~
        ~other",                                                          ~
           at(01,67), "Post: ",                                          ~
           at(01,73), fac(hex(8c)), hnydatef$,                           ~
           at(02,02), fac(hex(ac)), line2$                      , ch(79),~
           at(03,02), fac(hex(94)), errormsg$                   , ch(79),~
                                                                         ~
           at(04,02), "Part: ",                                          ~
           at(04,08), fac(hex(81)), part$                       , ch(25),~
           at(21,02), fac(hex(a4)), inpmessage$,                         ~
           at(22,65), "(13)Instructions",                                ~
           at(23,65), "(15)Print Screen",                                ~
           at(24,65), "(16)Exit Program",                                ~
                keys(hex(000d0f10)), key(keyhit%)

            if keyhit% <> 13 then L10260  :  call "MANUAL" ("HNYMOVE")
                                            goto L10070

L10260:     if keyhit% <> 15 then L10290  :  call "PRNTSCRN"
                                            goto L10070

L10290:     if keyhit% = 16 then goto L65000

*        Load Part Info
            call "GETCODE" (#4, part$, descr$, 0%, 0, f1%(4))
            if f1%(4) = 1% then L10350
                 errormsg$ = "Part not on file"  :  goto L10070
L10350:     call "LOTENABL" (part$, enable%, ll%, #1, #4)
            lfac$ = hex(81) : if enable% = 0% then lfac$ = hex(8c)
            gosub load_part_info

L10380:     errormsg$ = " "
            plowkey$  = str(part$,1,25) & hex(00)
L10400:     init(" ") store$(), lot$(), q$(), passedin_acct$() :  i = 1
            mat passedin_dbcr = zer : init(" ") passedin_type$()
            cntr% = 0%
            init(" ") oldacct$, newacct$

L10420:     call "PLOWNEXT" (#7, plowkey$, 25%, f1%(7))
            if f1%(7) = 1% then L10490
                if i > 1 then goto L10610
                if keyhit% = 5 then L10380
                errormsg$ = "End of Store/Lot records for this Part"
                goto L10070

L10490:     get #7 using L10510, store$(i), lot$(i), qty1, qty2, qty3,    ~
                                                    qty4, qty5
L10510:         FMT POS(42), CH(3), CH(6), POS(69), 5*PD(14,4)
           convert qty1 to q$(i,1), pic(-######.##)
           convert qty2 to q$(i,2), pic(-######.##)
           convert qty3 to q$(i,3), pic(-######.##)
           convert qty4 to q$(i,4), pic(-######.##)
           convert qty5 to q$(i,5), pic(-######.##)
           i = i + 1
           if i <= 10 then L10420

L10610: inpmessage$ = "Enter movement information, then press (RETURN)" &~
                      " to SAVE."
L10630: accept                                                           ~
           at(01,02), "Move Inventory Quantities from one Store/Lot to an~
        ~other",                                                          ~
           at(01,67), "Post: ",                                          ~
           at(01,73), fac(hex(8c)), hnydatef$                   , ch(08),~
           at(02,02), fac(hex(ac)), line2$                      , ch(79),~
           at(03,02), fac(hex(94)), errormsg$                   , ch(79),~
                                                                         ~
           at(04,02), "Part: ",                                          ~
           at(04,08), fac(hex(84)), part$                       , ch(25),~
           at(04,37), fac(hex(84)), descr$                      , ch(32),~
           at(05,02), "UOM:  ",                                          ~
           at(05,08), fac(hex(8c)), uom$                        , ch(04),~
           at(05,13), fac(hex(8c)), uom_descr$                  , ch(32),~
           at(06,02), "Move",                                            ~
           at(06,07), fac(hex(81)), mq$                         , ch(12),~
           at(06,20), "units FROM Store/Lot ",                           ~
           at(06,41), fac(hex(81)), fstore$, ch(3),                      ~
           at(06,45), fac(lfac$  ), str(flot$,,ll%),                     ~
           at(06,52), "TO Store/Lot",                                    ~
           at(06,65), fac(hex(81)), tstore$, ch(3),                      ~
           at(06,69), fac(lfac$  ), str(tlot$,,ll%),                     ~
                                                                         ~
           at(08,02), "Store  Lot         On-Hand   Open S.O.   On-Order ~
        ~     In Q.C.            ",                                       ~
           at(09,03), fac(hex(8d)), store$( 1), ch(3),                   ~
           at(09,09), fac(hex(8d)), lot$( 1), ch(6),                     ~
           at(09,18), fac(hex(8d)), q$(1, 1), ch(10),                    ~
           at(09,30), fac(hex(8d)), q$(1, 2), ch(10),                    ~
           at(09,41), fac(hex(8d)), q$(1, 3), ch(10),                    ~
           at(09,54), fac(hex(8d)), q$(1, 5), ch(10),                    ~
           at(10,03), fac(hex(8d)), store$( 2), ch(3),                   ~
           at(10,09), fac(hex(8d)), lot$( 2), ch(6),                     ~
           at(10,18), fac(hex(8d)), q$(2, 1), ch(10),                    ~
           at(10,30), fac(hex(8d)), q$(2, 2), ch(10),                    ~
           at(10,41), fac(hex(8d)), q$(2, 3), ch(10),                    ~
           at(10,54), fac(hex(8d)), q$(2, 5), ch(10),                    ~
           at(11,03), fac(hex(8d)), store$( 3), ch(3),                   ~
           at(11,09), fac(hex(8d)), lot$( 3), ch(6),                     ~
           at(11,18), fac(hex(8d)), q$(3, 1), ch(10),                    ~
           at(11,30), fac(hex(8d)), q$(3, 2), ch(10),                    ~
           at(11,41), fac(hex(8d)), q$(3, 3), ch(10),                    ~
           at(11,54), fac(hex(8d)), q$(3, 5), ch(10),                    ~
           at(12,03), fac(hex(8d)), store$( 4), ch(3),                   ~
           at(12,09), fac(hex(8d)), lot$( 4), ch(6),                     ~
           at(12,18), fac(hex(8d)), q$(4, 1), ch(10),                    ~
           at(12,30), fac(hex(8d)), q$(4, 2), ch(10),                    ~
           at(12,41), fac(hex(8d)), q$(4, 3), ch(10),                    ~
           at(12,54), fac(hex(8d)), q$(4, 5), ch(10),                    ~
           at(13,03), fac(hex(8d)), store$( 5), ch(3),                   ~
           at(13,09), fac(hex(8d)), lot$( 5), ch(6),                     ~
           at(13,18), fac(hex(8d)), q$(5, 1), ch(10),                    ~
           at(13,30), fac(hex(8d)), q$(5, 2), ch(10),                    ~
           at(13,41), fac(hex(8d)), q$(5, 3), ch(10),                    ~
           at(13,54), fac(hex(8d)), q$(5, 5), ch(10),                    ~
           at(14,03), fac(hex(8d)), store$( 6), ch(3),                   ~
           at(14,09), fac(hex(8d)), lot$( 6), ch(6),                     ~
           at(14,18), fac(hex(8d)), q$(6, 1), ch(10),                    ~
           at(14,30), fac(hex(8d)), q$(6, 2), ch(10),                    ~
           at(14,41), fac(hex(8d)), q$(6, 3), ch(10),                    ~
           at(14,54), fac(hex(8d)), q$(6, 5), ch(10),                    ~
           at(15,03), fac(hex(8d)), store$( 7), ch(3),                   ~
           at(15,09), fac(hex(8d)), lot$( 7), ch(6),                     ~
           at(15,18), fac(hex(8d)), q$(7, 1), ch(10),                    ~
           at(15,30), fac(hex(8d)), q$(7, 2), ch(10),                    ~
           at(15,41), fac(hex(8d)), q$(7, 3), ch(10),                    ~
           at(15,54), fac(hex(8d)), q$(7, 5), ch(10),                    ~
           at(16,03), fac(hex(8d)), store$( 8), ch(3),                   ~
           at(16,09), fac(hex(8d)), lot$( 8), ch(6),                     ~
           at(16,18), fac(hex(8d)), q$(8, 1), ch(10),                    ~
           at(16,30), fac(hex(8d)), q$(8, 2), ch(10),                    ~
           at(16,41), fac(hex(8d)), q$(8, 3), ch(10),                    ~
           at(16,54), fac(hex(8d)), q$(8, 5), ch(10),                    ~
           at(17,03), fac(hex(8d)), store$( 9), ch(3),                   ~
           at(17,09), fac(hex(8d)), lot$( 9), ch(6),                     ~
           at(17,18), fac(hex(8d)), q$(9, 1), ch(10),                    ~
           at(17,30), fac(hex(8d)), q$(9, 2), ch(10),                    ~
           at(17,41), fac(hex(8d)), q$(9, 3), ch(10),                    ~
           at(17,54), fac(hex(8d)), q$(9, 5), ch(10),                    ~
           at(18,03), fac(hex(8d)), store$(10), ch(3),                   ~
           at(18,09), fac(hex(8d)), lot$(10), ch(6),                     ~
           at(18,18), fac(hex(8d)), q$(10,1), ch(10),                    ~
           at(18,30), fac(hex(8d)), q$(10,2), ch(10),                    ~
           at(18,41), fac(hex(8d)), q$(10,3), ch(10),                    ~
           at(18,54), fac(hex(8d)), q$(10,5), ch(10),                    ~
           at(21,02), fac(hex(ac)), inpmessage$                 , ch(79),~
           at(22,02), "(1)Select another Part",                          ~
           at(22,28), "(2)First Store/Lot Info",                         ~
           at(23,28), "(5)Next  Store/Lot Info",                         ~
           at(24,28), "(8)Locations",                                    ~
           at(22,65), "(13)Instructions",                                ~
           at(23,65), "(15)Print Screen",                                ~
           at(24,65), "(16)Exit Program",                                ~
                keys(hex(00010205080d0f10)),  key(keyhit%)

            if keyhit% <> 2 then L11670  :  errormsg$ = " "
                                           goto L10380

L11670:     if keyhit% <> 5 then L11710  :  errormsg$ = " "
                                           if i <= 10 then L10610
                                           goto L10400

L11710:    if keyhit% <> 13 then L11740  :  call "MANUAL" ("HNYMOVE")
                                           goto L10610

L11740:    if keyhit% <> 15 then L11770  :  call "PRNTSCRN"
                                           goto L10610

L11770:    if keyhit% =  1 or keyhit% = 16% then                         ~
                                  call "SERSTOVR" (0%, "2", "2", #10, #9)
           if keyhit% =  1 then L10000
           if keyhit% = 16 then L65000
           if keyhit% <> 0 and keyhit% <> 8 then L10610

*        Test Data and effect changes indicated.
        errormsg$ = " "

        call "NUMTEST" (mq$, 0, 9e7, errormsg$, 0.2, mq)
            if errormsg$ <> " " then L10610

        if fstore$ <> tstore$ or flot$ <> tlot$ then L11906
            errormsg$ = "'FROM' Store/Lot can't be the same as 'TO' Sto"&~
                        "re/Lot"
            goto L10610
L11906:     if enable% < 2% then L11930
            if tlot$ <> " " then L11918
                errormsg$ = "This is a Lot Tracked Part - 'TO' "      &  ~
                            "Lot Number is REQUIRED"
            goto L10610

L11918:     if tlot$ = flot$ then L11930
L11920:         keyhit1% = 2%
                call "ASKUSER" (keyhit1%, "*** LOTS NOT SAME ***",       ~
                     "The 'To' Lot is NOT the Same as the 'From' Lot.",  ~
                     "Press RETURN to Reenter Lot   -or-",               ~
                     "Press PF16 to Continue Processing as is.")
                if keyhit1% = 16% then L11930
                if keyhit1% <> 0% then L11920
                     errormsg$ = "To and From Lot must be the same."
                     goto L10610

L11930: readkey$ = str(part$,1,25) & str(tstore$,1,3) & str(tlot$,1,6)
        call "READ100" (#7, readkey$, f1%(7))
        if f1%(7) = 1% then L12030
            if tlot$ = "?" and enable% <> 2% then tlot$ = " "
            call "READ100" (#2, tstore$, f1%(2))
            if f1%(2) = 1% then goto L12000
                errormsg$ = "'TO' Store/Lot does not exist."
                goto L10610
L12000:     call "LOTVALID" (part$, tstore$, tlot$, #1, #4, #7,errormsg$)
            if errormsg$ <> " " then L10610

L12030: call "HNYAVAIL" (#4, #7, part$, fstore$, flot$, errormsg$, mq,   ~
                avail, return%) : avail = avail
        if errormsg$ <> " " then L10610
        if return% <> 1% then L12170
            keyhit1% = 2%
            get #4, using L12090, neg_lots_flag$
L12090:         FMT POS(132), CH(1)

L12100:      if neg_lots_flag$ = "Y" then                                ~
                    errormsg$ = "You may not take more than you have."   ~
             else                                                        ~
                    call "ASKUSER" (keyhit1%, " *** WARNING *** ",       ~
              "This Movement will result in a Negative Quantity in the", ~
              "FROM Store/Lot.  Since Negative Lots are allowed on this" ~
                  & " Part,",                                            ~
              "Press <RETURN> to Continue, OR PF-16 to Abort.")

             if keyhit1% <> 16% then L12170
                keyhit1% = 0%
                neg_lots_flag$ = "Y"
                goto L12100

L12170: if return% = 99% then                                            ~
                         errormsg$ = "There is no such 'FROM' Store/Lot."
        if errormsg$ <> " " then L10610

        if keyhit% <> 8 then L12191 : gosub locations : goto L10630

L12191:         if hnyflg$ <> "A" then L12210
                     readkey$ = str(part$) & str(tstore$) & tlot$
                     call "READ100" (#7, readkey$, f1%(7))
                     if f1%(7) = 0% then  L12198
                     get #7, using L12196, adjacct$
L12196:                   FMT POS(286), CH(9)
                     if adjacct$ <> " " then L12210
L12198:              readkey$ = part$
                     call "READ100" (#4, readkey$, f1%(4))
                     if f1%(4) = 0% then  L12204
                     get #4, using L12202, adjacct$
L12202:                   FMT POS(380), CH(9)
                     if adjacct$ <> " " then L12210
L12204:              call "READ100" (#1, "FISCAL DATES", f1%(1))
                     if f1%(1) = 0% then  L12210
                     get #1, using L12207, adjacct$
L12207:                   FMT POS(416), CH(16)

L12210
*        Lastly, get serial numbers if so required
            sn_location$ = str(fstore$,,3) & flot$
            sn_trankey$  = " "
            call "SERSELCT" (part$, sn_location$, mq, 1%       , 1%,     ~
                             "IS", sn_trankey$, "2", "2", errormsg$,     ~
                             #1, #4, #10, #9)
            if errormsg$ <> " " then L10610

        totalcost = 0 : mat cost = zer
        call "SHOSTAT" ("Posting Transaction...")
        call "HNYHOLD" (#7, part$, fstore$, flot$, mq, return%)
        if postflag% = 0 then L12370
            call "JNLINFO" (modno$, jnlid$, pstseq%, summary$, title$,   ~
                                    hnydate$, #1, f2%(1), jnlreturn%)
            postflag% = 0%

L12370:     mm$ = "MOVED TO STORE:" & tstore$ & " LOT:" & tlot$
            gltext$ = "TO STR: " & str(tstore$) & " LOT: " & str(tlot$)
            str(gltext$,31) = str(part$) & str(fstore$) & str(flot$)
            str(gltext$,69,32) = "TRANSFER"
            call "HNYPST2" (part$, fstore$, flot$, -mq, 0,0,0,0, cost(), ~
                            totalcost,0,0, hnydate$, "IS", mm$, oldacct$,~
                            " ", 3%, 0%, modno$, " ", pstseq%, gltext$,  ~
                            userid$, #7, #6, #1, #11, #04, #13, #21, #19,~
                            #20, #50, 1%, returncode%)
            mm$ = "MOVED FROM STR:" & fstore$ & " LOT:" & flot$
            this% = 0%
            gltext$ = "TO STR: " & str(tstore$) & " LOT: " & str(tlot$)
            str(gltext$,31) = str(part$) & str(fstore$) & str(flot$)
            str(gltext$,69,32) = "TRANSFER"
            call "HNYPST2" (part$, tstore$, tlot$, mq, 0,0,0,0, cost(),  ~
                            totalcost,0,0, hnydate$, "IS", mm$, newacct$,~
                            " ", 3%, 0%, modno$, " ", pstseq%, gltext$,  ~
                            userid$, #7, #6, #1, #11, #04, #13, #21, #19,~
                            #20, #50, this%, returncode%)

            call "LOTTRACK"                                              ~
                 ("H",             /* FROM FLAG  H=INVNTORY,V=VEN,C=CUS*/~
                  part$,           /* PART BEING MOVED                 */~
                  flot$,           /* FROM LOT                         */~
                  fstore$,         /* FROM STORE                       */~
                  " ",             /* FROM MISC.                       */~
                  "H",             /* TO FLAG H=INVNTORY, V=VEN, C=CUS */~
                  part$,           /* TO PART (IF BLANK THEM SAME)     */~
                  tlot$,           /* TO LOT                           */~
                  tstore$,         /* TO STORE                         */~
                  " ",             /* TO MISC.                         */~
                  mq,              /* Quantity being moved             */~
                  #4,              /* 'HNYMASTR' FILE                  */~
                  #1)              /* 'SYSFILE2' FILE                  */~

            sn_trankey$ = str(tstore$,,3) & tlot$
            call "SERSAVE" (1%, "IS", sn_trankey$, 1%, part$, userid$,   ~
                            "2", "2", 1%, #1, #8, #10, #9)

            tran_type$ = "IMV"
            if export_on$ = "Y" then gosub load_gl_info
            movecost = round(totalcost * mq, 2%)
            gltext$ = "TO STR: " & str(tstore$) & " LOT: " & str(tlot$)
            str(gltext$,31) = str(part$) & str(fstore$) & str(flot$)
            str(gltext$,69,32) = "TRANSFER"
            passedin_acct$(1) = str(oldacct$) & gltext$
            passedin_dbcr(1,1) = 0
            passedin_dbcr(1,2) = movecost
            passedin_type$(1) = "01"
            gosub set_up_posting /* 1st just do FROM store inv asset */

            passedin_acct$(1) = str(newacct$) & gltext$
            passedin_dbcr(1,1) = movecost
            passedin_dbcr(1,2) = 0
            passedin_type$(1) = "01"
            cntr% = 1%

            gosub consolidate_array_and_post

        if prtline > 55 then gosub header
        print using L13020, part$, descr$, mq, fstore$, flot$, tstore$,   ~
                                                              tlot$
        prtline = prtline + 1

        totalcost, mq = 0 : mat cost = zer
        init(" ") fstore$, tstore$, flot$, tlot$, mq$
        goto L10380


L12980: % RUN DATE: ######## ########               #####################~
        ~#########                 HNYMOVE-HNY037
L13000: %POST DATE: ########  BY: ###              INVENTORY MOVEMENTS AU~
        ~DIT LISTING
L13020: %######################### ################################ -###,~
        ~###,###.##   ###   ######   ###   ######
L13040: %PART NUMBER               PART DESCRIPTION                      ~
        ~  QUANTITY  STORE  LOT NR  STORE  LOT NR
L13060: %                                                                ~
        ~            -- F R O M --  ---- T O ----

        header
            print page
            print using L12980, date$, runtime$, company$
            print using L13000, hnydatef$, userid$
            print
            print using L13060
            print using L13040
            prtline = 5
            return

        REM *************************************************************~
            * CALL TO CONSOLIDATE                                       *~
            *************************************************************

        consolidate_array_and_post
                first% = 1%
                plowkey$ = all (hex(00))
L14050:         call "PLOWNXT1" (#50, plowkey$, 0%, f1%(50))
                     if f1%(50) = 0 and first% = 1% then set_up_posting
                     if f1%(50) = 0 then L14210
                     first% = 0%
                get #50, using L14080, text$, dbamt, cramt
L14080:         FMT XX(25), CH(109), 2*PD(14,4)
                if hnyflg$ <> "A" then L14140
                    chkacct$ = str(text$,1,9)
                    if chkacct$ = oldacct$ or chkacct$ = newacct$ then   ~
                                                                    L14140
                    text$ = str(adjacct$,1,9) & str(text$,10,100)
L14140:         cntr% = cntr% + 1%
                passedin_acct$(cntr%) = text$
                passedin_dbcr(cntr%,1%) = dbamt
                passedin_dbcr(cntr%,2%) = cramt
                     passedin_type$(cntr%) = "01"
                     if str(passedin_acct$(cntr%),,9)= newacct$ then L14185
                     if hnyflg$ = "A" then L14182
                     if str(text$,76,1) = "H" then L14185
                     passedin_type$(cntr%) = "03"
                     if str(text$,74,1) = "S" then L14185
L14182:              passedin_type$(cntr%) = "02"
L14185:         delete #50
                goto L14050

L14210:         call "GLCMBSUB" (passedin_acct$(), passedin_dbcr(),      ~
                                 passedin_type$(), #1, cntr%)

        set_up_posting
            if export_on$ = "Y" then gosub load_gl_info
            for x% = 1% to 50%
                if passedin_acct$(x%) = " " then return
                acct$(1) = str(passedin_acct$(x%),,9)
                acct$(2) = acct$(1)
                glamount(1) = passedin_dbcr(x%,1)
                glamount(2) = passedin_dbcr(x%,2)
                temp1 = glamount(1) - glamount(2)
                str(tran_type$,4,2) = passedin_type$(x%)
                temp$ = tstore$ : temp2 = abs(mq)
                if acct$(1) <> oldacct$ then L14298
                     temp$ = fstore$ : temp2 = abs(mq) * (-1)
L14298:         put gl_post_info$() using L14300, tran_type$, temp1,      ~
                                                 temp2, temp$
L14300:            FMT CH(5), POS(18), 2*PD(15,4), POS(164), CH(3)
                gltext$ = str(passedin_acct$(x%),10)
                gosub post_gl
            next x%

                return

        REM *************************************************************~
            *         C O M M O N   G / L   P O S T   L O G I C         *~
            *                                                           *~
            * ACCT$(), GLTEXT$, and GLAMOUNT must be set prior to this. *~
            *************************************************************

        post_gl

            REM Account in ACCT$(1) is debited...

            if glamount(1) = 0 then L14660

            call "GLPOST2" (acct$(1),    /* ACCOUNT TO BE UPDATED      */~
                      glamount(1),       /* DEBIT AMOUNT (0 IF CREDIT) */~
                      0,                 /* CREDIT AMOUNT (0 IF DEBIT) */~
                      hnydate$,          /* DATE OF MODULE             */~
                      0%,                /*                            */~
                      "04",              /* TYPE CODE OF TRANSACTION   */~
                      gltext$,           /* REFERENCE TEXT (100 CHARS) */~
                      jnlid$,            /* JOURNAL ID                 */~
                      pstseq%,           /* POSTING SEQUENCE NUMBER    */~
                      userid$,           /* WHO                        */~
                      division$,         /* (AWD001) division          */~
                      #19,               /* UFB ADDRESS OF G/L MAIN    */~
                      #20,               /* UFB ADDRESS OF G/L DETAILS */~
                      #1,                /* UFB ADDRESS OF SYSFILE2    */~
                      #22,               /* (AWD001) GLORTRAN          */~
                      err%,              /* ERROR RETURN FROM SUBROUTIN*/~
                      " ",               /* Filler                     */~
                      gl_post_info$())   /* GLEXPORT Information       */

            if gllog$ = "N" then L14660
            call "GLPRTSUB" (modno$, jnlid$, pstseq%, userid$, hnydate$, ~
                         acct$(1), gltext$, glamount(1), 0, #14, f2%(14))

L14660:     REM Account in ACCT$(2), ...  is credited...
            for credit% = 2% to 4%
               if acct$(credit%) = " " then L14900
               if glamount(credit%) = 0 then L14900

            call "GLPOST2" (acct$(credit%),   /* ACCOUNT TO BE UPDATED */~
                      0,                 /* DEBIT AMOUNT (0 IF CREDIT) */~
                      glamount(credit%), /* CREDIT AMOUNT (0 IF DEBIT) */~
                      hnydate$,          /* DATE OF MODULE             */~
                      0%,                /*                            */~
                      "04",              /* TYPE CODE OF TRANSACTION   */~
                      gltext$,           /* REFERENCE TEXT (100 CHARS) */~
                      jnlid$,            /* JOURNAL ID                 */~
                      pstseq%,           /* POSTING SEQUENCE NUMBER    */~
                      userid$,           /* WHO                        */~
                      division$,         /* (AWD001) division          */~
                      #19,               /* UFB ADDRESS OF G/L MAIN    */~
                      #20,               /* UFB ADDRESS OF G/L DETAILS */~
                      #1,                /* UFB ADDRESS OF SYSFILE2    */~
                      #22,               /* (AWD001) GLORTRAN          */~
                      err%,              /* ERROR RETURN FROM SUBROUTIN*/~
                      " ",               /* Filler                     */~
                      gl_post_info$())   /* GLEXPORT Information       */

            if gllog$ = "N" then L14900
            call "GLPRTSUB" (modno$, jnlid$, pstseq%, userid$, hnydate$, ~
             acct$(credit%), gltext$, 0, glamount(credit%), #14, f2%(14))

L14900:     next credit%

        return

        load_gl_info

            put str(gl_post_info$(),,) using L15500,                      ~
                tran_type$,              /* Transaction Type CH(5)     */~
                " ",                     /* Currency code CH(4)        */~
                0,                       /* Currency Units per Book    */~
                0,                       /* Functional Currency amount */~
                0,                       /* Unit amount                */~
                " ",                     /* Customer code CH(9)        */~
                " ",                     /* Sales Order number CH(16)  */~
                " ",                     /* BOL number CH(3)           */~
                " ",                     /* Customer Type CH(2)        */~
                " ",                     /* State CH(2)                */~
                " ",                     /* Country CH(3)              */~
                " ",                     /* ZIP CH(9)                  */~
                " ",                     /* Sales Region CH(4)         */~
                " ",                     /* Sales Tax code CH(10)      */~
                " ",                     /* Shipping Region CH(4)      */~
                " ",                     /* Salesman code CH(4)        */~
                " ",                     /* Invoice Number CH(8)       */~
                part$,                   /* Part Number CH(25)         */~
                partcat$,                /* Part Category CH(4)        */~
                partclass$,              /* Part Class CH(4)           */~
                partgen$,                /* Part Generic code CH(16)   */~
                parttype$,               /* Part Type CH(3)            */~
                uom$,                    /* Part UOM CH(4)             */~
                " ",                     /* Store Number CH(3)         */~
                " ",                     /* Check Receipt Number CH(8) */~
                " ",                     /* Vendor code CH(9)          */~
                " ",                     /* Vendor type CH(4)          */~
                " ",                     /* Purchase Order CH(16)      */~
                " ",                     /* Receiver Number CH(16)     */~
                " ",                     /* Vendor Invoice CH(16)      */~
                " ",                     /* Check Payment Number CH(8) */~
                " ",                     /* Project code CH(8)         */~
                " ",                     /* Job number CH(8)           */~
                " ",                     /* Work Center CH(4)          */~
                " ",                     /* Activity code CH(4)        */~
                " ",                     /* Employee number CH(12)     */~
                " ",                     /* Department code CH(4)      */~
                " ",                     /* Cost Center CH(4)          */~
                " ",                     /* Earnings Type CH(12)       */~
                " ",                     /* Deduction Type CH(12)      */~
                " ",                     /* P/R Category CH(4)         */~
                " ",                     /* Labor class CH(4)          */~
                " "                      /* Filler                     */

            return

L15500: FMT     CH(5),                   /* Transaction Type CH(5)     */~
                CH(4),                   /* Currency code CH(4)        */~
                PD(15,7),                /* Currency Units per Book    */~
                PD(15,4),                /* Functional Currency amount */~
                PD(15,4),                /* Unit amount                */~
                CH(9),                   /* Customer code CH(9)        */~
                CH(16),                  /* Sales Order number CH(16)  */~
                CH(3),                   /* BOL number CH(3)           */~
                CH(2),                   /* Customer Type CH(2)        */~
                CH(2),                   /* State CH(2)                */~
                CH(3),                   /* Country CH(3)              */~
                CH(9),                   /* ZIP CH(9)                  */~
                CH(4),                   /* Sales Region CH(4)         */~
                CH(10),                  /* Sales Tax code CH(10)      */~
                CH(4),                   /* Shipping Region CH(4)      */~
                CH(4),                   /* Salesman code CH(4)        */~
                CH(8),                   /* Invoice Number CH(8)       */~
                CH(25),                  /* Part Number CH(25)         */~
                CH(4),                   /* Part Category CH(4)        */~
                CH(4),                   /* Part Class CH(4)           */~
                CH(16),                  /* Part Generic code CH(16)   */~
                CH(3),                   /* Part Type CH(3)            */~
                CH(4),                   /* Part UOM CH(4)             */~
                CH(3),                   /* Store Number CH(3)         */~
                CH(8),                   /* Check Receipt Number CH(8) */~
                CH(9),                   /* Vendor code CH(9)          */~
                CH(4),                   /* Vendor type CH(4)          */~
                CH(16),                  /* Purchase Order CH(16)      */~
                CH(16),                  /* Receiver Number CH(16)     */~
                CH(16),                  /* Vendor Invoice  CH(16)     */~
                CH(8),                   /* Check Payment Number CH(8) */~
                CH(8),                   /* Project code CH(8)         */~
                CH(8),                   /* Job number CH(8)           */~
                CH(4),                   /* Work Center CH(4)          */~
                CH(4),                   /* Activity code CH(4)        */~
                CH(12),                  /* Employee number CH(12)     */~
                CH(4),                   /* Department code CH(4)      */~
                CH(4),                   /* Cost Center CH(4)          */~
                CH(12),                  /* Earnings Type CH(12)       */~
                CH(12),                  /* Deduction Type CH(12)      */~
                CH(4),                   /* P/R Category CH(4)         */~
                CH(4),                   /* Labor class CH(4)          */~
                CH(191)                  /* Filler                     */

        load_part_info

            call "READ100" (#4, part$, f1%(4))
                 if f1%(4) = 0% then return /* Shouldn't happen */
            get #4 using L16000, partgen$, uom$, partcat$, partclass$,    ~
                                parttype$
L16000:     FMT POS(58), CH(16), CH(4), POS(90), CH(4), POS(133), CH(4), ~
                POS(180), CH(3)
            plowkey$ = "UOM      " & uom$
            call "DESCRIBE" (#5, plowkey$, uom_descr$, 1%, f1%(5))
            return

        REM *************************************************************~
            *    A C C E S S   L O C A T I O N   M A N A G E M E N T    *~
            *-----------------------------------------------------------*~
            *  Call HNYLCSUB passing Part, Store, Lot and Qty invoking  *~
            *  the transfer facility within the subroutine.             *~
            *************************************************************
        locations
            if mq$ <> " " then L17120
                mq = 0
                goto L17140

L17120:     convert mq$ to mq

L17140:     call "HNYLCSUB" (part$,                                      ~
                             fstore$,                                    ~
                             flot$,                                      ~
                             mq,                                         ~
                             2%,       /*   Action = TRANSFER          */~
                             #1,       /*   SYSFILE2                   */~
                             #2,       /*   STORNAME                   */~
                             #3,       /*   USERINFO                   */~
                             #4,       /*   HNYMASTR                   */~
                             #15,      /*   HNYLOCNS                   */~
                             #7,       /*   HNYQUAN                    */~
                             #16,      /*   LOCATION                   */~
                             tstore$,  /*   TO STORE                   */~
                             tlot$)    /*   TO LOT                     */
            return

L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC
        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "SETPRNT" ("HNY037", " ", 0%, 1%)
            call "FILEBGON" (#9)
            call "FILEBGON" (#50)
            call "JNLCLOSE" (modno$, jnlid$, pstseq%, returncode%)
            end
