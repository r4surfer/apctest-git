        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB   RRRR   W   W  K   K  H   H  N   N  Y   Y   *~
            *    J    B   B  R   R  W   W  K  K   H   H  NN  N  Y   Y   *~
            *    J    BBBB   RRR    W   W  KK     HHHHH  N N N   YYY    *~
            *  J J    B   B  R  R   W W W  K  K   H   H  N  NN    Y     *~
            *   J     BBBB   R   R   W W   K   K  H   H  N   N    Y     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBRWKHNY - TRANSFER MATERIAL TO BE REWORKED DIRECTLY TO   *~
            *            A JOB OR RETURN MATERIAL FROM JOB TO INVENTORY.*~
            *            QTY IN ADDS TO 'QTY-TO-MAKE', QTY OUT LESSENS. *~
            *            PGM NOT USED TO REPORT COMPLETION OF REWORK.   *~
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
            * 05/23/84 ! ORIGINAL (THANK YOU KENNY FOR 'JBTOHNY2' ! ERN *~
            *          !   FOR WHICH I WAS IMPELLED TO CLONE.)    !     *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION - ! RAC *~
            * 11/01/85 ! CHANGED CALL TO JBMTPOST                 ! HES *~
            * 02/05/87 ! Lot Tracking Enhancements                ! ERN *~
            * 03/11/87 ! Added Serial Numbers                     ! ERN *~
            * 06/15/87 ! File format changes; Std cost changes;   ! JIM *~
            *          !   HNYPROC, JBCREDIT files removed.       !     *~
            * 10/28/87 ! Corrected Write to JBMATER2              ! HES *~
            * 10/28/88 ! Corrected Erroneous Cost Postings[I hope]! KAB *~
            * 01/03/90 ! Modified code to combine like accounts   ! LAB *~
            *          ! for gl postings.  Accountancy Phase I    !     *~
            * 10/15/90 ! Merge GL Export Option & A/C I.          ! JDH *~
            * 05/21/91 ! Added PF8 access to HNYLCSUB. Also added ! JDH *~
            *          !   HNYLOCNS and LOCATION files. Thnx MLJ. !     *~
            * 06/04/91 ! PRR 10639 Modified Error Messages for    ! SID *~
            *          !   the transfer quantity.(Pos./Neg)       !     *~
            * 02/24/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 05/24/93 ! Core Ledger Posting                      ! KAB *~
            *          ! RWK PIPIN & On Order via Flag            !     *~
            * 12/22/93 ! PRR 13071.  Fixed inv asset account get. ! JDH *~
            * 01/14/94 ! Added to GLCMBSUB argument list.         ! JDH *~
            * 10/17/97 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            acct$(4)9,                   /* used in GLCMBSUB           */~
            aqty(100),                                                   ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            blankline$79,                                                ~
            cmbtext$109,                 /* TEXT FOR GLCMBSUB          */~
            coreactive$1,                /* Core Flag                  */~
            coreflg$1,                   /* Core Flag                  */~
            corepart$25,                 /* Core Part Code             */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dfcore_var$9,                /* System Default Core Var.   */~
            dfcore_fga$9,                /* System Default Core FGA    */~
            dfcore_wip$9,                /* System Default Core WIP    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            glamount(4),                 /* USED IN GLCMBSUB           */~
            gltext$100,                  /* GL TEXT STRING             */~
            hnyacct$9,                   /* INVENTORY ASSET ACCOUNT    */~
            hnyqty$10,                                                   ~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            jbnr$8,                      /* JOB NUMBER                 */~
            jbnrdescr$32,                /* JOB NUMBER                 */~
            jbpart$25,                   /* PART                       */~
            jbpartdescr$34,              /* PART                       */~
            jbquantity$10,               /* QUANTITY TO BUILD          */~
            jbavailable$10,                                              ~
            jbcomplete$10,                                               ~
            jbcorewip$9,                 /* JOB CORE WIP ACCOUNT       */~
            jnlid$3,                     /* JOURNAL ID                 */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lfa$(10,5)1,                                                 ~
            line2$79,                                                    ~
            lot$6, lotmsg$32,            /* LOT                        */~
            lot$(100)6,                                                  ~
            matcost(12), matcost$(100)104, matdisp$(100)10, matcost$96,  ~
            /* Above are costs from the Job Material Ledger- JBMATER2  */~
            mtkey$(100)22,                                               ~
            modno$2,                     /* MODULE NUMBER              */~
            moduleno$20,                 /* MODULE ID                  */~
            part$25,                     /* PART #                     */~
            partdescr$34,                /* PART #                     */~
            parttype$3,                  /* Part Type                  */~
            passedin_acct$(50)109,       /* ARRAY PASSED TO GLCMBSUB   */~
            passedin_dbcr(50,2),         /* ARRAY PASSED TO GLCMBSUB   */~
            passedin_type$(50)2,         /* ARRAY PASSED TO GLCMBSUB   */~
            pf16$16,                     /* PF-16 Prompt               */~
            plowwrk$99,                  /* General Plowkey            */~
            postdate$8,                  /* USERS INVENTORY POST DATE  */~
            posttext$40,                 /* JBVLPOST posting text      */~
            quantity$10,                 /* QUANTITY                   */~
            qty(100),                                                    ~
            readkey$100, plowkey$100,                                    ~
            rwkpip$1,                    /* Rework PIP Flag            */~
            sfcdate$6,                   /* USERS INVENTORY POST DATE  */~
            sn_loc$30,                   /* Serial Number Variables    */~
            sn_source$1,                 /*           "                */~
            sn_status$1,                 /*           "                */~
            sn_tran_type$2,              /*           "                */~
            stdcost(12),                 /* Std costs from STCCOSTS    */~
            store$3,                     /* STORE                      */~
            store$(100)3,                                                ~
            storedescr$32,               /* STORE                      */~
            summary$1,                   /* SUMMARY INDICATOR          */~
            temp2$25,                                                    ~
            temp(12),                                                    ~
            tempe$8,                     /* Temporary Variable         */~
            tempf$8,                     /* Temporary Variable         */~
            temps$8,                     /* Temporary Variable         */~
            text$50,                                                     ~
            title$40,                    /* JOURNAL TITLE$             */~
            tqty(100),                                                   ~
            userid$3,                    /* WHO                        */~
            wipacct$9                    /* WORK IN PROCESS  ACCT      */

        dim export_on$1,                 /* G/L Export File processing?*/~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            partcat$4,                   /* Part Category code         */~
            partclass$4,                 /* Part Class code            */~
            partgen$16,                  /* Part Generic code          */~
            tran_type$5,                 /* G/L transaction type       */~
            uom$4                        /* Part unit of measure       */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* RETURN CODE FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * # 1 ! USERINFO ! SECURITY FILE                            *~
            * # 2 ! SYSFILE2 ! LAST LEVEL OF DEFAULT                    *~
            * # 3 ! JBMASTR2 ! JOB MASTER FILE                          *~
            * # 4 ! HNYMASTR ! INVENETORY MASTER FILE                   *~
            * # 5 ! HNYQUAN  ! INVENTORY QUANTITY & GL DEFAULTS         *~
            * # 6 ! GLMAIN   ! TO TEST SAME                             *~
            * # 7 ! CALMASTR ! CALANDAR MASTER FILE FOR DATES           *~
            * # 8 ! PIPIN    ! GOING TO MAKE IT, LET SOMEONE KNOW!!     *~
            * # 9 ! PIPMASTR ! PLANNED POSITION MASTER                  *~
            * #11 ! HNYPOOL  ! INVENTORY POOLS                          *~
            * #12 ! HNYDETAL ! INVENTORY DETAIL                         *~
            * #13 ! JBVALUE2 ! JOB VALUE ADDED LEDGER                   *~
            * #14 ! JBMATER2 ! JOB MATERIAL LEDGER                      *~
            * #15 ! STORNAME ! STORES FOR VALIDATION                    *~
            * #16 ! GLDETAIL ! GENERAL LEDGER DETAIL                    *~
            * #18 ! SERMASTR ! Serial Number Master File                *~
            * #19 ! SERWORK  !               Work File                  *~
            * #20 ! SERTIF   !               Transaction Image File     *~
            * #21 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #22 ! JBJNLPF  ! JOB GL PRINT JOURNAL FILE                *~
            * #23 ! HNYLOCNS ! Location Quantity Detail File            *~
            * #24 ! LOCATION ! Location Master File                     *~
            * #40 ! COREXREF ! Core Cross Reference File                *~
            * #41 ! JBMASTRC ! JBMASTR2 Core Appendix                   *~
            * #50 ! WRKFILE  ! Hnypst2 work file                        *~
            *************************************************************

            select  #1, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

            select  #2, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select  #3, "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1300,                                  ~
                        keypos = 1, keylen = 8

            select  #4, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 900,                                   ~
                        keypos = 1, keylen = 25,                         ~
                        alternate key 1, keypos = 102, keylen = 9, dup,  ~
                                  key 2, keypos = 90,  keylen = 4, dup

            select  #5, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 650,                                   ~
                        keypos = 17, keylen = 44,                        ~
                        alternate key 1, keypos =  1, keylen = 44

            select  #6, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select  #7, "CALMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos = 1, keylen = 2

            select  #8, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 60,                                    ~
                        keypos = 30, keylen = 19,                        ~
                        alternate key 1, keypos = 1, keylen = 48

            select  #9, "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos = 2, keylen = 25,                         ~
                        alternate key 1, keypos = 1, keylen = 26

            select #11, "HNYPOOL",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 38

            select #12, "HNYDETAL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 42,                         ~
                        alt key 1, keypos = 43, keylen = 6, dup,         ~
                            key 2, keypos = 49, keylen = 2, dup

            select  #13, "JBVALUE2",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 23

            select #14, "JBMATER2",                                      ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 400,                                    ~
                       keypos = 1, keylen = 22,                          ~
                       alternate key 1, keypos = 23, keylen = 48

            select #15, "STORNAME",                                      ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 300,                                    ~
                       keypos = 1, keylen = 3

            select #16, "GLDETAIL",                                      ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 160,                                    ~
                       keypos = 1, keylen = 26

            select #18, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #19, "SERWORK",                                       ~
                        varc,     indexed,  recsize =  48,               ~
                        keypos = 1, keylen = 23

            select #20, "SERTIF",                                        ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos = 1, keylen = 62

            select #21, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =  25                      ~

            select # 22, "JBJNLPF",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 160,                                  ~
                         keypos = 1, keylen = 19                         ~

            select #23,  "HNYLOCNS",                                     ~
                          varc,       indexed,    recsize =  700,        ~
                          keypos =  1,  keylen =  42,                    ~
                          alt key 1,  keypos = 443,  keylen = 42,        ~
                              key 2,  keypos = 485,  keylen = 42,        ~
                              key 3,  keypos = 527,  keylen = 42,        ~
                              key 4,  keypos = 527,  keylen = 42

            select #24,  "LOCATION",                                     ~
                          varc,       indexed,    recsize =  400,        ~
                          keypos =  1,  keylen =  11,                    ~
                          alt key 1,  keypos =   4,  keylen = 11

            select #40, "COREXREF",                                      ~
                        varc, indexed, recsize = 500,                    ~
                        keypos = 26, keylen = 50,                        ~
                         alternate key 1, keypos =  1, keylen = 50,      ~
                                   key 2, keypos = 76, keylen = 25, dup

            select #41, "JBMASTRC",                                      ~
                        varc, indexed, recsize = 600,                    ~
                        keypos =  1, keylen =  8

            select #50, "WRKFILE",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 160,                                  ~
                         keypos = 1, keylen = 19                         ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1),   0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2),   0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3),   0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4),   0%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5),   0%, rslt$( 5))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6),   0%, rslt$( 6))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7),   0%, rslt$( 7))
            call "OPENCHCK" (# 8, fs%( 8), f2%( 8),   0%, rslt$( 8))
            call "OPENCHCK" (# 9, fs%( 9), f2%( 9),   0%, rslt$( 9))
            call "OPENCHCK" (#11, fs%(11), f2%(11),   0%, rslt$(11))
            call "OPENCHCK" (#12, fs%(12), f2%(12),   0%, rslt$(12))
            call "OPENCHCK" (#13, fs%(13), f2%(13),   0%, rslt$(13))
            call "OPENCHCK" (#14, fs%(14), f2%(14),   0%, rslt$(14))
            call "OPENCHCK" (#15, fs%(15), f2%(15),   0%, rslt$(15))
            call "OPENCHCK" (#16, fs%(16), f2%(16),   0%, rslt$(16))
            call "OPENCHCK" (#21, fs%(21), f2%(21),   0%, rslt$(21))
            call "OPENCHCK" (#22, fs%(22), f2%(22),   0%, rslt$(22))
            call "OPENCHCK" (#23, fs%(23), f2%(23), 100%, rslt$(23))
            call "OPENCHCK" (#24, fs%(24), f2%(24), 100%, rslt$(24))

            call "OPENCHCK" (#40, fs%(40), f2%(40),   0%, rslt$(40))
            call "OPENCHCK" (#41, fs%(41), f2%(41),   0%, rslt$(41))

            call "WORKOPEN" (#50, "IO", 100%, f2%(50))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)
            str(line2$,62) = "JBRWKHNY: " & str(cms2v$,,8)
            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."
            ll% = 6%

*        See if G/L Export is on
            export_on$ = "N"  :  plowwrk$ = "SWITCHS.GL"
            call "READ100" (#2, plowwrk$, f1%(2%))
            if f1%(2) = 1% then get #2 using L09122, export_on$
L09122:         FMT POS(22), CH(1)

*        See if Core is on
            plowkey$ = "SWITCHS.COR"
            call "READ100" (#2, plowkey$, corebank%)
                if corebank% = 0% then L09138
            get #2 using L09131, dfcore_var$, dfcore_fga$, dfcore_wip$,    ~
                    coreactive$
L09131:         FMT POS(30), CH(9), POS(84), CH(9), POS(119), CH(9),     ~
                    POS(135), CH(1)
            if coreactive$ <> "Y" then corebank% = 0%

L09138:     call "EXTRACT" addr("ID",userid$)
            call "READ100" (#1, userid$, f1%(1))
            if f1%(1)=0 then L65000
            get #1, using L09170, sfcdate$
L09170:         FMT XX(33), CH(6)
            postdate$ = sfcdate$
            call "WHICHMON" (#2, postdate$, whichmonth%)
            if whichmonth% = 0 then L65000 else call "DATEFMT" (postdate$)

            tran_type$ = "MRW  "
            modno$ = "03"
            jnlid$ = "MRW"
            returncode% = 0        /* Get Post Sequence */
            moduleno$ = modno$
            call "JNLINFO" (moduleno$, jnlid$, pstseq%, summary$, title$,~
                            sfcdate$, #2, f2%(2), returncode%)

            readkey$ = "SWITCHS.SFC"
            call "READ100" (#2, readkey$, f1%(2%))
               if f1%(2%) = 0% then L10000
            get #2 using L09330, rwkpip$
L09330:         FMT POS(70), CH(1)

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, blankline$, hnyqty$, jbnr$,~
                      hnyacct$, jbnrdescr$, jbpart$, jbpartdescr$, lot$, ~
                      jbquantity$, jbavailable$, jbcomplete$, mtkey$(),  ~
                      lot$(), part$, partdescr$, quantity$, readkey$,    ~
                      store$, store$(), storedescr$, wipacct$, lotmsg$,  ~
                      matdisp$()
            init (hex(00)) matcost$(), matcost$
            hnyqty, netqty, quantity, matcost, stdcost = 0
            mat qty  = zer : mat tqty = zer : mat aqty    = zer
            mat stdcost = zer : mat matcost = zer

            maxline%, sign%, line%, sn_enable% = 0%

            for fieldnr% = 1% to 4%
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10400
L10360:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10360
L10400:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10360
                next fieldnr%

            if quantity < 0 then L12000

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Edit data to transfer FROM Inventory TO Job. (SIGN% = +1) *~
            *************************************************************

L11080:     gosub'111(0%) /* INVENTORY --> JOB               */
                if keyhit%  =  1% then gosub startover
                if keyhit%  = 16% then       datasave1
                if keyhit% <>  0% then       L11080

            fieldnr% = 4%

L11150:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11150
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11150
                  goto L11080

L12000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Edit for transfer FROM Job TO Inventory.  (SIGN% = -1).   *~
            *************************************************************

*        First time thru get ledger selections (serial numbers in).
            fieldnr% = 2%
            goto L12260

L12100:     gosub totals
            pf16$ = "(16)Save Data" : if netqty <> 0 then pf16$ = " "
            gosub'112(0%)              /* Display and get what to edit */
                if keyhit% =  1% then gosub startover
                if keyhit% =  2% then line% = 0%
                if keyhit% =  3% then line% = maxline% - 5%
                if keyhit% =  4% then line% = line%- 8%
                if keyhit% =  5% then line% = min(line%+8, maxline%-5)
                if keyhit% =  6% then line% = line% - 1%
                if keyhit% =  7% then line% = min(line%+1, maxline%-5)
                                      line% = max(0, line%)
                if keyhit% = 16 and netqty  = 0 then datasave2
                if keyhit% <> 0% then L12100

            if cursor%(1) <= 11% then fieldnr% = 1% else fieldnr% = 2%

L12260:     gosub totals
L12270:     gosub'112(fieldnr%)        /* Get entries                  */
                if keyhit% =  1% then gosub startover
                if keyhit% =  2% then line% = 0%
                if keyhit% =  3% then line% = maxline% - 5%
                if keyhit% =  4% then line% = line%- 8%
                if keyhit% =  5% then line% = min(line%+8, maxline%-5)
                if keyhit% =  6% then line% = line% - 1%
                if keyhit% =  7% then line% = min(line%+1, maxline%-5)
                                      line% = max(0, line%)
                if keyhit% <> 0% then L12260

                if fieldnr% <> 1% then L12100
                     gosub'151(4%)
                     fieldnr% = 1%
                     if errormsg$ = " " then L12100 else L12270

        totals
            netqty = -quantity                /* QUANTITY IS < 0       */
            for i% = 1% to maxline%           /* Loop to edit entries  */
                if tqty(i%) = 0  then L12530   /* TQTY() = Transfer Qty */
                if i% = maxline% then L12500
                     tqty(i%) = max(0,min(tqty(i%),aqty(i%)))
                     goto L12520
L12500:         gosub'254(i%)
                if matcost < 0 then tqty(i%)=0
                tqty(i%) = max(0,tqty(i%))
L12520:         netqty   = netqty - tqty(i%)
L12530:     next i%
            return

        deffn'254(jrh%)
            get matcost$(jrh%) using L12580, matcost, matcost()
L12580:         FMT  13*PD(14,4)
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 0
            on fieldnr%  gosub      L20100,         /* JOB NUMBER       */~
                                    L20400,         /* STORE            */~
                                    L20500,         /* LOT              */~
                                    L20600          /* QUANTITY         */
            return

L20100
*        Default/Enable for JOB NUMBER
            inpmessage$ = "Enter Rework Job Number."
            enabled% = 1%
            return

L20400
*        Default/Enable for STORE
            inpmessage$ = "Enter Store to transfer to/from."
            enabled% = 1%
            return

L20500
*        Default/Enable for LOT
            inpmessage$ = "Enter Lot to transfer to/from."
            call "LOTENABL" (part$, lotenbl%, ll%, #2, #4)
            if lotenbl% > 0% then enabled% = 1%
            return

L20600
*        Default/Enable for QUANTITY
            inpmessage$ = "Positive to transfer to Job, Negative"  &     ~
                          " to transfer from Job."
            enabled% = 1%
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            * --------------------------------------------------------- *~
            * Gives the user the ability to Start Over when he wants to *~
            * else return to the menu.  Notice that he has to push TWO  *~
            * DIFFERENT BUTTONS to start over--isn't that slick!        *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
                return clear all
                if sn_enable% = 0% or sign% = 0% then L29980
                     call "SERSTOVR" (0%, sn_status$, sn_source$,        ~
                                      #18, #19)
L29980:         goto inputmode

L30000: REM *************************************************************~
            * LOAD MATERIALS IN THIS JOB FOR TRANSFER OUT               *~
            *-----------------------------------------------------------*~
            *  Retrieve any issues made to the Job from the materials   *~
            *  Ledger.                                                  *~
            *************************************************************

            maxline% = 0%
            init(hex(00)) readkey$
            str(readkey$,,8) = jbnr$

L30110:     call "PLOWNEXT" (#14, readkey$, 8%, f1%(14)) /*JBMATER2*/
            if f1%(14) = 0% then return

            get #14, using L30150, temp2$
L30150:         FMT XX(22), CH(25)
            if temp2$ <> part$ then L30110

                maxline% = maxline% + 1%
                get #14, using L30230,                                    ~
                    mtkey$(maxline%), store$(maxline%), lot$(maxline%),  ~
                    qty(maxline%), matcost$(maxline%), aqty(maxline%)
L30230:                   FMT CH(22), XX(25), CH(3), CH(6), XX(14),      ~
                              PD(14,4), CH(104), POS(330), PD(14,4)
                aqty(maxline%) = max(0, qty(maxline%)-aqty(maxline%))
                gosub'254(maxline%)
                call "CONVERT" (matcost, 2.4, matdisp$(maxline%))
                goto L30110


        REM *************************************************************~
            * SAVE DATA- WITHDRAWAL FROM INVENTORY  (QTY IS +)          *~
            *-----------------------------------------------------------*~
            *  Withdraw from inventory and issue to Job.                *~
            *************************************************************
        datasave1
            call "SHOSTAT" ("Transferring Inventory to Job")
            if quantity = 0 then inputmode
            qty   = -quantity
            text$ ="MATERIAL ISSUED TO RWRK JOB: " & jbnr$
            gosub call_hnypost
            gosub save_serial_numbers
            gltext$ = jbnr$
            str(gltext$,31) = str(part$) & str(store$) & str(lot$)
            str(gltext$,69) = "MATERIAL MOVED TO REWORK JOB"
            if export_on$ = "Y" then gosub load_gl_info

            call "LOTTRACK"                                              ~
                 ("H",             /* FROM FLAG  H=INVNTORY,V=VEN,C=CUS*/~
                  part$,           /* PART BEING MOVED                 */~
                  lot$,            /* FROM LOT                         */~
                  store$,          /* FROM STORE                       */~
                  " ",             /* FROM MISC.                       */~
                  "J",             /* TO FLAG H=INVNTORY, V=VEN, C=CUS */~
                  part$,           /* TO PART (IF BLANK THEM SAME)     */~
                  "*" & str(jbnr$,,5),                     /* TO LOT   */~
                  str(jbnr$,6,3),  /* TO STORE                         */~
                  " ",             /* TO MISC.                         */~
                  quantity,        /* QUANTITY                         */~
                  #4,              /* 'HNYMASTR'                       */~
                  #2)              /* 'SYSFILE2'                       */~


            glamount = round(quantity * matcost, 2%)

            gosub check_for_core
            call "JBMTPOST"  (jbnr$,     /* JOB  TO BE UPDATED         */~
                      part$,             /* PART NUMBER TO POST        */~
                      store$,            /* STORE NUMBER               */~
                      lot$,              /* LOT NUMBER                 */~
                      quantity,          /* QUANTITY MOVED TO JOB      */~
                      matcost(),         /* MATERIAL COSTS             */~
                      store$,            /* STORE NUMBER               */~
                      lot$,              /* LOT NUMBER                 */~
                      sfcdate$,          /* POSTING DATE               */~
                      userid$,           /* WHO                        */~
                      coreflg$,          /* CORE FLAG                  */~
                      #3,                /* UFB ADDRESS OF JBMASTR2    */~
                      #14,               /* UFB ADDRESS OF JBMATER2    */~
                      #2,                /* UFB ADDRESS OF SYSFILE2    */~
                      #41,               /* UFB ADDRESS OF JBMASTRC    */~
                      returncode%)       /* ERROR RETURN FROM SUBROUTIN*/~
                                         /* 0  = RECORD POSTED         */~
                                         /* 99 = RECORD *NOT* POSTED   */~

            passedin_acct$(1) = str(wipacct$,1,9) & gltext$
            if coreflg$ = " " then L31560
               if jbcorewip$ = " " then gosub set_jb_core_wip
               passedin_acct$(1) = str(jbcorewip$,1,9) & gltext$
L31560:     passedin_acct$(2) = str(hnyacct$,1,9) & gltext$
            passedin_dbcr(1,1) = glamount
            passedin_dbcr(1,2) = 0
            passedin_dbcr(2,1) = 0
            passedin_dbcr(2,2) = glamount
            passedin_type$(1) = "02"
            passedin_type$(2) = "01"
            cntr% = 2%

            gosub consolidate_array_and_post

            gosub core_shadow

            goto inputmode

        REM *************************************************************~
            * SAVE DATA FOR WITHDRAWAL FROM JOB TO HNY   (QTY IS -)     *~
            *-----------------------------------------------------------*~
            *  MOVE UN-REWORKED PRODUCT BACK INTO INVENTORY.            *~
            *************************************************************
        datasave2
            if quantity = 0 or maxline% = 0% then inputmode
            call "SHOSTAT" ("Transferring Inventory from Job")
            gosub save_serial_numbers
            quantity, matcost = 0 : mat matcost = zer
            for i% = 1% to maxline%  /* Loop thru Matl ledger & adjust */
                if tqty(i%) <= 0 then L32240
                     call "READ101" (#14, mtkey$(i%), f1%(14))
                        if f1%(14) = 0% then L32240
                     aqty(i%) = qty(i%) - aqty(i%) + tqty(i%)
                     put #14 using L32160, aqty(i%)
L32160:                   FMT POS(330), PD(14,4)
                     get #14 using L32180, temp, temp()
L32180:                  FMT POS(79), 13*PD(14,4)
                     rewrite #14
                     matcost = matcost + (tqty(i%) * temp / qty(i%))
                     mat temp = (tqty(i%)/qty(i%)) * temp
                     mat matcost = matcost + temp
                     quantity = quantity + tqty(i%)
L32240:     next i%

            quantity = round(quantity, 2)   /* Total Returned to HNY */
            if quantity = 0 then inputmode
            temp = 1/quantity
            mat matcost = (temp) * matcost
            glamount = round(matcost, 2%)
            matcost = round(matcost / quantity, 4)

            gosub check_for_core
            call "JBMTPOST" (jbnr$,      /* JOB  TO BE UPDATED         */~
                      part$,             /* PART NUMBER TO POST        */~
                      store$,            /* STORE NUMBER               */~
                      lot$,              /* LOT NUMBER                 */~
                     -quantity,          /* QUANTITY MOVED TO JOB      */~
                      matcost(),         /* STANDARD MATERIAL          */~
                      store$,            /* STORE NUMBER               */~
                      lot$,              /* LOT NUMBER                 */~
                      sfcdate$,          /* POSTING DATE               */~
                      userid$,           /* WHO                        */~
                      coreflg$,          /* CORE FLAG                  */~
                      #3,                /* UFB ADDRESS OF JBMASTR2    */~
                      #14,               /* UFB ADDRESS OF JBMATER2    */~
                      #2,                /* UFB ADDRESS OF SYSFILE2    */~
                      #41,               /* UFB ADDRESS OF JBMASTRC    */~
                      returncode%)       /* ERROR RETURN FROM SUBROUTIN*/~
                                         /* 0  = RECORD POSTED         */~
                                         /* 99 = RECORD *NOT* POSTED   */~

            qty   = quantity
            text$ = "MATRL RETURNED FROM RWK JOB: " & jbnr$
            gosub call_hnypost
            gltext$ = jbnr$
            str(gltext$,31) = str(part$) & str(store$) & str(lot$)
            str(gltext$,69) = "INV. BACKED OUT OF REWORK JOB"
            if export_on$ = "Y" then gosub load_gl_info


            call "LOTTRACK"                                              ~
                 ("J",             /* FROM FLAG  H=INVNTORY,V=VEN,C=CUS*/~
                  jbpart$,         /* PART BEING MOVED                 */~
                  "*" & str(jbnr$,,5),                     /* FROM LOT */~
                  str(jbnr$,6,3),  /* FROM STORE                       */~
                  " ",             /* FROM MISC.                       */~
                  "H",             /* TO FLAG H=INVNTORY, V=VEN, C=CUS */~
                  part$,           /* TO PART (IF BLANK THEM SAME)     */~
                  lot$,            /* TO LOT                           */~
                  store$,          /* TO STORE                         */~
                  " ",             /* TO MISC.                         */~
                  quantity,        /* QUANTITY MOVED                   */~
                  #4,              /* 'HNYMASTR' FILE                  */~
                  #2)              /* 'SYSFILE2' FILE                  */


            passedin_acct$(1) = str(hnyacct$,1,9) & gltext$
            passedin_acct$(2) = str(wipacct$,1,9) & gltext$
            if coreflg$ = " " then L32790
               if jbcorewip$ = " " then gosub set_jb_core_wip
               passedin_acct$(2) = str(jbcorewip$,1,9) & gltext$
L32790:     passedin_dbcr(1,1) = glamount
            passedin_dbcr(1,2) = 0
            passedin_dbcr(2,1) = 0
            passedin_dbcr(2,2) = glamount
            passedin_type$(1) = "01"
            passedin_type$(2) = "02"
            cntr% = 2%

            gosub consolidate_array_and_post

            gosub core_shadow

            goto inputmode

        REM *************************************************************~
            *   C O M M O N   U P D A T E   S U B - R O U T I N E S     *~
            *-----------------------------------------------------------*~
            *  ODDS AND ENDS TO KEEP SOME SKIN LEFT ON THE OL' DIGITS.  *~
            *************************************************************

        call_hnypost

            hnyacct$ = " "
            if qty > 0 then L34110
               call "HNYHOLD" (#5, part$, store$, lot$, -qty, returncode%)

L34110:     call "HNYPST2"   (part$,     /* PART TO BE UPDATED         */~
                      store$,            /* STORE CODE                 */~
                      lot$,              /* LOT NUMBER                 */~
                      qty, 0, 0, 0, 0,   /* ON-HAND                    */~
                      matcost(),         /* UNIT COST                  */~
                      matcost,           /* TOTAL COST                 */~
                      0,                 /* UNIT PRICE                 */~
                      0,                 /* EXTENSION OF PRICE         */~
                      sfcdate$,          /* MODULE DATE (YYMMDD)       */~
                      "JR",              /* TRANSACTION TYPE (HNYDETAL)*/~
                      text$,             /* REFERENCE TEXT STRING      */~
                      hnyacct$,          /* ACCOUNT                    */~
                      wipacct$,          /* ACCOUNT                    */~
                      3%,                /*                            */~
                      6%,                /*                            */~
                      modno$,            /* MODULE NUMBER              */~
                      " ",               /* JOURNAL ID                 */~
                      pstseq%,           /* POSTING SEQUENCE NUMBER    */~
                      gltext$,           /* GL TEXT STRING             */~
                      userid$,           /* WHO                        */~
                      #5,                /* UFB ADDRESS OF HNYQUAN     */~
                      #12,               /* UFB ADDRESS OF HNYDETAL    */~
                      #2,                /* UFB ADDRESS OF SYSFILE2    */~
                      #11,               /* UFB ADDRESS OF HNYPOOLS    */~
                      #4,                /* UFB ADDRESS OF HNYMASTR    */~
                      #9,                /* UFB ADDRESS OF PIPMASTR    */~
                      #21,               /* UFB ADDRESS OF SFCUM2      */~
                      #6,                /* UFB ADDRESS OF GLMAIN      */~
                      #16,               /* UFB ADDRESS OF GLDETAIL    */~
                      #50,               /* UFB ADDRESS OF HNYADJPF    */~
                      0%,                /* WHETHER OR NOT TO PRINT AN */~
                                         /*  EXCEPTION REPORT WHEN A   */~
                                         /*  NEW HNYQUAN REC CREATED-  */~
                                         /*  1= PRINT, 0 = DON'T PRINT */~
                      returncode%)       /* ERROR RETURN FROM SUBROUTIN*/~

          /* Delete Old PIPIN, If Any              */
            readkey$ = "JOB ORDER: " & jbnr$ : temp = 0
            call "READ101" (#8, readkey$, f1%(8))
                if f1%(8) = 0 then L34600
            get #8 using L34550, temp%, temp
L34550:         FMT XX(25), BI(4), XX(19), PD(14,4)
            delete #8
            call "PIPFLAGS" (part$, 1%, temp%, -temp, #9, #21)

L34600:   /* Rewrite JOB MASTER to Adjust QTY to Make        */
            call "READ101" (#3, jbnr$, f1%(3))
            get #3 using L34660, temp1, temp2         /* Make, Complete */
            temp3 = max(temp1 - qty, temp2)
            temp3 = round(temp3, 2)
            put #3 using L34660, temp3, temp2         /* Make, Complete */
L34660:         FMT POS(83), 2*PD(14,4)
            rewrite #3

          /* Write a PIPIN, If Called For          */
            temp4 = 0
            if str(jbnr$,,2%) <> "RW" then L34740
            if rwkpip$ <> "A" then L34750
L34740:        temp4 = temp3 - temp2
L34750:     if temp4 = 0 then L34800
            write #8 using L34770, part$, endate%, readkey$, temp4, stdate%
L34770:           FMT CH(25), BI(4), CH(19), PD(14,4), BI(4)
            call "PIPFLAGS" (part$, 1%, endate%, temp4, #9, #21)

L34800:     temp5 = temp4 - temp
            if temp5 = 0 then return

          /* Last, Adjust HNYQUAN On Order, if indicated  */
            call "HNYPST1" (part$,       /* PART TO BE UPDATED         */~
                      "001",             /* STORE CODE                 */~
                      "      ",          /* LOT NUMBER                 */~
                      0, 0, -qty, 0, 0,  /* ON-ORDER                   */~
                      #5,                /* UFB ADDRESS OF HNYQUAN     */~
                      #4,                /* UFB ADDRESS OF HNYMASTR    */~
                      #2,                /* UFB ADDRESS OF SYSFILE2    */~
                      f2%(5),            /* STATUS FLAG FOR HNYQUAN    */~
                      f2%(4),            /* STATUS FLAG FOR HNYMASTR   */~
                      f2%(2),            /* STATUS FLAG FOR SYSFILE2   */~
                      0%,                /* WHETHER OR NOT TO PRINT EXC*/~
                      returncode%)       /* ERROR RETURN FROM SUBROUTIN*/~

            return

        save_serial_numbers
            if sn_enable% = 0% then return

            if sign% = 1% then sn_trankey$ = jbnr$ else                  ~
                                sn_trankey$ = str(store$,,3) & lot$
            call "SERSAVE" (1%, sn_tran_type$, sn_trankey$, 5%, part$,   ~
                            userid$, sn_status$, sn_source$, 1%,         ~
                            #2, #20, #18, #19)
            return

        REM *************************************************************~
            * CALL TO CONSOLIDATE                                       *~
            *************************************************************

        consolidate_array_and_post
                first% = 1%
                plowwrk$ = all (hex(00))
L35550:         call "PLOWNXT1" (#50, plowwrk$, 0%, f1%(50))
                     if f1%(50) = 0 and first% = 1% then L35690
                     if f1%(50) = 0 then L35660
                     first% = 0%
                get #50, using L35580, cmbtext$, dbamt, cramt
L35580:         FMT XX(25), CH(109), 2*PD(14,4)
                cntr% = cntr% + 1%
                passedin_acct$(cntr%) = str(cmbtext$,,9)
                passedin_dbcr(cntr%,1%) = dbamt
                passedin_dbcr(cntr%,2%) = cramt
                passedin_type$(cntr%) = "01"
                if str(cmbtext$,76,1) = "H" then L35630
                passedin_type$(cntr%) = "04"
                if str(cmbtext$,74,1) = "S" then L35630
                passedin_type$(cntr%) = "03"
L35630:         delete #50
                goto L35550

L35660:         call "GLCMBSUB" (passedin_acct$(), passedin_dbcr(),      ~
                                 passedin_type$(), #2, cntr%)
        REM POST GL
L35690:     for x% = 1% to 50%
                if passedin_acct$(x%) = " " then goto L35780
                acct$(1) = str(passedin_acct$(x%),,9)
                acct$(2) = acct$(1)
                gltext$  = str(passedin_acct$(x%),10,100)
                glamount(1) = passedin_dbcr(x%,1)
                glamount(2) = passedin_dbcr(x%,2)
                str(tran_type$,4,2) = passedin_type$(x%)
                iamt = glamount(1) - glamount(2)
                iqty = abs(qty)
                if iamt < 0 then iqty = iqty * (-1)
                gosub post_gl
            next x%

L35780:         return

        REM *************************************************************~
            *         C O M M O N   G / L   P O S T   L O G I C         *~
            *                                                           *~
            * ACCT$(), GLTEXT$, and GLAMOUNT must be set prior to this. *~
            *************************************************************

        post_gl

            put gl_post_info$() using L35874, tran_type$, iamt, iqty
L35874:         FMT CH(5), POS(18), 2*PD(15,4)

            REM Account in ACCT$(1) is debited...

            if glamount(1) = 0 then L36110

            call "GLPOST2" (acct$(1),    /* ACCOUNT TO BE UPDATED      */~
                      glamount(1),       /* DEBIT AMOUNT (0 IF CREDIT) */~
                      0,                 /* CREDIT AMOUNT (0 IF DEBIT) */~
                      sfcdate$,          /* DATE OF MODULE             */~
                      0%,                /*                            */~
                      modno$,            /* TYPE CODE OF TRANSACTION   */~
                      gltext$,           /* REFERENCE TEXT (100 CHARS) */~
                      jnlid$,            /* JOURNAL ID                 */~
                      pstseq%,           /* POSTING SEQUENCE NUMBER    */~
                      userid$,           /* WHO                        */~
                      #6,                /* UFB ADDRESS OF G/L MAIN    */~
                      #16,               /* UFB ADDRESS OF G/L DETAILS */~
                      #2,                /* UFB ADDRESS OF SYSFILE2    */~
                      return%,           /* ERROR RETURN FROM SUBROUTIN*/~
                      jbnr$, gl_post_info$())

*          IF GLLOG$ = "N" THEN 36110
            call "GLPRTSUB" (modno$, jnlid$, pstseq%, userid$, sfcdate$, ~
                         acct$(1), gltext$, glamount(1), 0, #22, f2%(22))

L36110:     REM Account in ACCT$(2), ...  is credited...
            for credit% = 2% to 4%
               if acct$(credit%) = " " then L36350
               if glamount(credit%) = 0 then L36350

            call "GLPOST2" (acct$(credit%),   /* ACCOUNT TO BE UPDATED */~
                      0,                 /* DEBIT AMOUNT (0 IF CREDIT) */~
                      glamount(credit%), /* CREDIT AMOUNT (0 IF DEBIT) */~
                      sfcdate$,          /* DATE OF MODULE             */~
                      0%,                /*                            */~
                      modno$,            /* TYPE CODE OF TRANSACTION   */~
                      gltext$,           /* REFERENCE TEXT (100 CHARS) */~
                      jnlid$,            /* JOURNAL ID                 */~
                      pstseq%,           /* POSTING SEQUENCE NUMBER    */~
                      userid$,           /* WHO                        */~
                      #6,                /* UFB ADDRESS OF G/L MAIN    */~
                      #16,               /* UFB ADDRESS OF G/L DETAILS */~
                      #2,                /* UFB ADDRESS OF SYSFILE2    */~
                      return%,           /* ERROR RETURN FROM SUBROUTIN*/~
                      jbnr$, gl_post_info$())

*          IF GLLOG$ = "N" THEN 36350
            call "GLPRTSUB" (modno$, jnlid$, pstseq%, userid$, sfcdate$, ~
             acct$(credit%), gltext$, 0, glamount(credit%), #22, f2%(22))

L36350:     next credit%

        return

        load_gl_info

            gosub load_part_info

            put str(gl_post_info$(),,) using L37510,                      ~
                " ",                     /* Transaction Type CH(5)     */~
                " ",                     /* Currency code CH(4)        */~
                0,                       /* Transaction Currency amount*/~
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
                uom$,                    /* Part UOM CH(4)           4 */~
                store$,                  /* Store Number CH(3) 4       */~
                " ",                     /* Check Receipt Number CH(8) */~
                " ",                     /* Vendor code CH(9)          */~
                " ",                     /* Vendor type CH(4)          */~
                " ",                     /* Purchase Order CH(16)      */~
                " ",                     /* Receiver Number CH(16)     */~
                " ",                     /* Vendor Invoice CH(16)      */~
                " ",                     /* Check Payment Number CH(8) */~
                " ",                     /* Project code CH(8)         */~
                jbnr$,                   /* Job number CH(8)           */~
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

L37510: FMT     CH(5),                   /* Transaction Type CH(5)     */~
                CH(4),                   /* Currency code CH(4)        */~
                PD(15,7),                /* Currency units per book    */~
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
                if f1%(4) = 0% then return
            get #4 using L37992, partgen$, uom$, partcat$, partclass$,    ~
                         parttype$
L37992:     FMT POS(58), CH(16), CH(4), POS(90), CH(4), POS(133), CH(4), ~
                POS(180), CH(3)
            return

        REM *************************************************************~
            *   A C C E S S    L O C A T I O N    M A N A G E M E N T   *~
            *___________________________________________________________*~
            * Call HNYLCSUB and pass the Part, Store, Lot and Quantity  *~
            * so that the user does not have to re-enter them.          *~
            *************************************************************

         locations
            if quantity$ <> " " then L38024
               qty = 0
               goto L38028

L38024:     convert quantity$ to qty

L38028:     if qty >= 0 then mode% = 4% else        /* Inv Withdrawal  */~
                             mode% = 3%             /* Inv Addition    */
            if mode% = 3% then qty = abs(qty)

            call "HNYLCSUB"  (jbpart$,                                   ~
                              store$,                                    ~
                              str(lot$,,ll%),                            ~
                              qty,                                       ~
                              mode%,                /*  Action Type    */~
                              #2,                   /*  SYSFILE2       */~
                              #15,                  /*  STORNAME       */~
                              #1,                   /*  USERINFO       */~
                              #4,                   /*  HNYMASTR       */~
                              #23,                  /*  HNYLOCNS       */~
                              #5,                   /*  HNYQUAN        */~
                              #24)                  /*  LOCATION       */
            return

        REM *************************************************************~
            * Core Handling Subroutines                                 *~
            *************************************************************

        set_jb_core_wip
            jbcorewip$ = dfcore_wip$
            plowkey$ = jbpart$
            call "PLOWNEXT" (#40, plowkey$, 25%, f1%(40%))
               if f1%(40%) = 0% then L39120
            get #40 using L39100, jbcorewip$
L39100:         FMT POS(146), CH(9)
            if jbcorewip$ = " " then jbcorewip$ = dfcore_wip$
L39120:     call "READ101" (#3, jbnr$, f1%(3))
               if f1%(3%) = 0% then return /* UNLIKELY */
            put #3 using L39150, jbcorewip$
L39150:         FMT POS(99), CH(9)
            rewrite #3
            return

        check_for_core
               coreflg$ = " "
               if corebank% = 0% then return
               plowkey$ = " " : str(plowkey$,26%) = jbpart$
               call "REDALT0" (#40, plowkey$, 0%, f1%(40%))
                  if f1%(40%) = 0% then return
               coreflg$ = "CORE"
               return

        core_shadow
            if corebank% = 0% then return
            quan = abs(qty)
            plowkey$ = " " : str(plowkey$,,25%) = jbpart$
            call "PLOWALTS" (#40, plowkey$, 0%, 25%, f1%(40%))
               if f1%(40%) = 0% then return
            corepart$ = str(plowkey$,26%,25%)
            call "STCCOSTS" (corepart$, " ", #2, 2%, matcost, matcost())
                  if matcost = 0 then return
                     mat matcost = (quan) * matcost
                     call "PACKZERO" (matcost(), matcost$)
                     glamount(1) = abs(round(quan * matcost, 2%))
                     glamount(2) = glamount(1)
               get #40 using L39550, acct$(1)
L39550:            FMT POS(137), CH(9)
               if acct$(1) <> " " then L39651
                  plowkey$ = " " : str(plowkey$,26%) = corepart$
                  call "REDALT0" (#40, plowkey$, 0%, f1%(40%))
                     if f1%(40%) = 0% then L39640
                  get #40 using L39550, acct$(1)
                  if acct$(1) <> " " then L39651
L39640:              acct$(1) = dfcore_fga$

L39651:        if jbcorewip$ = " " then gosub set_jb_core_wip
               acct$(2) = jbcorewip$
               if qty > 0 then L39658
                  acct$(2) = acct$(1)
                  acct$(1) = jbcorewip$
                  goto L39660

L39658:        mat matcost = (-1) * matcost

L39660
*        Post to Job & G/L
            posttext$ = "CVA: " & corepart$
            call "CONVERT" (-qty, 2.2, str(posttext$,31%,10%))
            gltext$ = jbnr$
            str(gltext$,31) = str(part$) & str(store$) & str(lot$)
            str(gltext$,65%) = "    CVA: " & corepart$

            call "JBVLPOST"              /*                            */~
                      (#3,               /* UFB ADDRESS OF JBMASTR2    */~
                       #13,              /* UFB ADDRESS OF JBVALUE2    */~
                       #41,              /* UFB ADDRESS OF JBMASTRC    */~
                       returncode%,      /* ERROR RETURN FROM SUBROUTIN*/~
                       jbnr$,            /* JOB  TO BE UPDATED         */~
                       5%,               /* TYPE - CORE SHADOW         */~
                       sfcdate$,         /* DATE WORK WAS PERFORMED    */~
                                         /* OR DIRECT TRANSACTION DATE */~
                       sfcdate$,         /* POSTING DATE OF USER       */~
                       userid$,          /* WHO                        */~
                       posttext$,        /* POSTING TEXT               */~
                       matcost())        /* COSTS PASSED IN            */

            gosub post_gl
            return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40170,         /* JOB NUMBER       */~
                                    L40170,         /* STORE            */~
                                    L40170,         /* LOT              */~
                                    L40200          /* QUANTITY         */
                     goto L40240

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40200:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40240:     accept                                                       ~
               at (01,02), "TRANSFER REWORK MATERIAL TO/FROM JOB",       ~
               at (01,67), "Post: ", at (1,73), fac(hex(8c)),postdate$,  ~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), "Job Number",                                 ~
               at (05,16), fac(lfac$( 1)), jbnr$                , ch(08),~
               at (05,25), fac(hex(8c)),   jbnrdescr$           , ch(32),~
               at (05,55), "Rework Qty:",                                ~
               at (05,67), fac(hex(8c)),   jbquantity$          , ch(10),~
               at (06,02), "Rework Part",                                ~
               at (06,16), fac(hex(84)),   jbpart$              , ch(25),~
               at (06,57), "Qty Done: ",                                 ~
               at (06,67), fac(hex(8c)),   jbcomplete$          , ch(10),~
               at (07,25), fac(hex(8c)),   jbpartdescr$         , ch(34),~
               at (08,02), "To/From Store",                              ~
               at (08,16), fac(lfac$( 2)), store$               , ch(03),~
               at (08,25), fac(hex(8c)),   storedescr$          , ch(32),~
               at (09,02), "        Lot",                                ~
               at (09,16), fac(lfac$( 3)), str(lot$,,ll%),               ~
               at (09,25), fac(hex(8c)),   lotmsg$              , ch(32),~
               at (11,02),                                               ~
                  "Transfer Qty (+Into, -From) Job:",                    ~
               at (11,35), fac(lfac$( 4)), quantity$            , ch(10),~
               at (11,47), "Lot's Quantity",                             ~
               at (11,62), fac(hex(8c)), hnyqty$                , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,37), "(8)Locations",                               ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                   keys(hex(0001080d0f10)), key (keyhit%)

               if keyhit% <> 8% then L40580
                  gosub locations
                  goto L40240

L40580:        if keyhit% <> 13 then L40620
                  call "MANUAL" ("JBRWKHNY")
                  goto L40240

L40620:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40240

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41170,         /* JOB NUMBER       */~
                                    L41170,         /* STORE            */~
                                    L41170,         /* LOT              */~
                                    L41200          /* QUANTITY         */
                     goto L41240

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L41200:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41240:     accept                                                       ~
               at (01,02), "TRANSFER REWORK MATERIAL TO JOB",            ~
               at (01,67), "Post: ", at (1,73), fac(hex(8c)),postdate$,  ~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), "Job Number",                                 ~
               at (05,16), fac(lfac$( 1)), jbnr$                , ch(08),~
               at (05,25), fac(hex(8c)),   jbnrdescr$           , ch(32),~
               at (05,55), "Rework Qty:",                                ~
               at (05,67), fac(hex(8c)),   jbquantity$          , ch(10),~
               at (06,02), "Rework Part",                                ~
               at (06,16), fac(hex(84)),   jbpart$              , ch(25),~
               at (06,57), "Qty Done: ",                                 ~
               at (06,67), fac(hex(8c)),   jbcomplete$          , ch(10),~
               at (07,25), fac(hex(8c)),   jbpartdescr$         , ch(34),~
               at (08,02), "From Store   ",                              ~
               at (08,16), fac(lfac$( 2)), store$               , ch(03),~
               at (08,25), fac(hex(8c)),   storedescr$          , ch(32),~
               at (09,02), "     Lot",                                   ~
               at (09,16), fac(lfac$( 3)), str(lot$,,ll%),               ~
               at (09,25), fac(hex(8c)),   lotmsg$              , ch(32),~
               at (11,02),                                               ~
                  "Transfer Qty (+INTO,      ) JOB:",                    ~
               at (11,35), fac(hex(81)),   quantity$            , ch(10),~
               at (11,35), fac(lfac$( 4)), quantity$            , ch(10),~
               at (11,47), "Lot's Quantity",                             ~
               at (11,62), fac(hex(8c)), hnyqty$                , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Save Data",                              ~
                   keys(hex(00010d0f10)), key (keyhit%)

               if keyhit% <> 13 then L41630
                  call "MANUAL" ("JBRWKHNY")
                  goto L41240

L41630:        if keyhit% <> 15 then L41670
                  call "PRNTSCRN"
                  goto L41240

L41670:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            * --------------------------------------------------------- *~
            * Job to Inventory Screen.                                  *~
            *************************************************************

        deffn'112(fieldnr%)     /*  0=Display; 1=QTY;  2=Ledger   */
            init (hex(86)) lfac$()
            on fieldnr% + 1% goto L42170, L42100, L42130

L42100:         init (hex(8c)) lfac$() : lfac$(9) = hex(82)
                goto L42170

L42130:         init (hex(82)) lfac$()
                init (hex(9c)) str(lfac$(), min(9, maxline%-line%+1))
                lfac$(9) = hex(8c)

L42170:     init (hex(84)) lfa$()        /* Materials Ledger        */
            for i% = 1% to 8%
                if aqty(line%+i%) <= 0 then lfac$(i%) = hex(9c)
            next i%
            i% = maxline% - line%
            init (hex(9c)) str(lfa$(), min(41, 5*(maxline%-line%)+1))

L42240:     accept                                                       ~
               at (01,02),                                               ~
                  "TRANSFER REWORK MATERIAL FROM JOB TO INVENTORY",      ~
               at (01,67), "Post: ", at (1,73), fac(hex(8c)),postdate$,  ~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02),                                               ~
                  "Job Number",                                          ~
               at (05,16), fac(hex(84)),   jbnr$                , ch(08),~
               at (05,25), fac(hex(8c)),   jbnrdescr$           , ch(32),~
               at (05,55), "Rework Qty:",                                ~
               at (05,67), fac(hex(8c)),   jbquantity$          , ch(10),~
               at (06,02), "Rework Part",                                ~
               at (06,16), fac(hex(84)),   jbpart$              , ch(25),~
               at (06,57), "Qty Done: ",                                 ~
               at (06,67), fac(hex(8c)),   jbcomplete$          , ch(10),~
               at (07,25), fac(hex(8c)),   jbpartdescr$         , ch(34),~
               at (08,02), "To Store",                                   ~
               at (08,16), fac(hex(8c)),   store$               , ch(03),~
               at (08,25), fac(hex(8c)),   storedescr$          , ch(32),~
               at (09,02), "   Lot",                                     ~
               at (09,16), fac(hex(8c)),   str(lot$,,ll%),               ~
               at (09,25), fac(hex(8c)),   lotmsg$              , ch(25),~
               at (09,55), "Lot's Qty:",                                 ~
               at (09,67), fac(hex(8c)),   hnyqty$              , ch(10),~
                                                                         ~
               at (11,02),                                               ~
                  "Transfer Qty (       -FROM) Job:",                    ~
               at (11,35), fac(hex(81)),  quantity$             , ch(10),~
               at (11,35), fac(lfac$(9)), quantity$             , ch(10),~
               at (11,47), "Net Quantity",                               ~
               at (11,62), fac(hex(84)), netqty        , pic(-######.##),~
                                                                         ~
               at (12,02),                                               ~
                  "STR----LOT-------COST-------------------------ORIGINAL~
        ~--AVALIABLE----TRANSFER--",                                      ~
        /*         234567890123456789012345678901234567890123456789012345~
        6789012345678901234567890                                      */~
               at (13,02), fac(hex(84)), store$(line%+1)        , ch(03),~
               at (14,02), fac(hex(84)), store$(line%+2)        , ch(03),~
               at (15,02), fac(hex(84)), store$(line%+3)        , ch(03),~
               at (16,02), fac(hex(84)), store$(line%+4)        , ch(03),~
               at (17,02), fac(hex(84)), store$(line%+5)        , ch(03),~
               at (18,02), fac(hex(84)), store$(line%+6)        , ch(03),~
               at (19,02), fac(hex(84)), store$(line%+7)        , ch(03),~
               at (20,02), fac(hex(84)), store$(line%+8)        , ch(03),~
                                                                         ~
               at (13,06), fac(hex(84)), lot$(line%+1)          , ch(06),~
               at (14,06), fac(hex(84)), lot$(line%+2)          , ch(06),~
               at (15,06), fac(hex(84)), lot$(line%+3)          , ch(06),~
               at (16,06), fac(hex(84)), lot$(line%+4)          , ch(06),~
               at (17,06), fac(hex(84)), lot$(line%+5)          , ch(06),~
               at (18,06), fac(hex(84)), lot$(line%+6)          , ch(06),~
               at (19,06), fac(hex(84)), lot$(line%+7)          , ch(06),~
               at (20,06), fac(hex(84)), lot$(line%+8)          , ch(06),~
                                                                         ~
               at (13,13),fac(lfa$(1,1)), matdisp$(line%+1)     , ch(10),~
               at (14,13),fac(lfa$(2,1)), matdisp$(line%+2)     , ch(10),~
               at (15,13),fac(lfa$(3,1)), matdisp$(line%+3)     , ch(10),~
               at (16,13),fac(lfa$(4,1)), matdisp$(line%+4)     , ch(10),~
               at (17,13),fac(lfa$(5,1)), matdisp$(line%+5)     , ch(10),~
               at (18,13),fac(lfa$(6,1)), matdisp$(line%+6)     , ch(10),~
               at (19,13),fac(lfa$(7,1)), matdisp$(line%+7)     , ch(10),~
               at (20,13),fac(lfa$(8,1)), matdisp$(line%+8)     , ch(10),~
                                                                         ~
               at (13,46),fac(lfa$(1,4)),  qty(line%+1), pic(-######.##),~
               at (14,46),fac(lfa$(2,4)),  qty(line%+2), pic(-######.##),~
               at (15,46),fac(lfa$(3,4)),  qty(line%+3), pic(-######.##),~
               at (16,46),fac(lfa$(4,4)),  qty(line%+4), pic(-######.##),~
               at (17,46),fac(lfa$(5,4)),  qty(line%+5), pic(-######.##),~
               at (18,46),fac(lfa$(6,4)),  qty(line%+6), pic(-######.##),~
               at (19,46),fac(lfa$(7,4)),  qty(line%+7), pic(-######.##),~
               at (20,46),fac(lfa$(8,4)),  qty(line%+8), pic(-######.##),~
                                                                         ~
               at (13,57),fac(lfa$(1,5)), aqty(line%+1), pic(-######.##),~
               at (14,57),fac(lfa$(2,5)), aqty(line%+2), pic(-######.##),~
               at (15,57),fac(lfa$(3,5)), aqty(line%+3), pic(-######.##),~
               at (16,57),fac(lfa$(4,5)), aqty(line%+4), pic(-######.##),~
               at (17,57),fac(lfa$(5,5)), aqty(line%+5), pic(-######.##),~
               at (18,57),fac(lfa$(6,5)), aqty(line%+6), pic(-######.##),~
               at (19,57),fac(lfa$(7,5)), aqty(line%+7), pic(-######.##),~
               at (20,57),fac(lfa$(8,5)), aqty(line%+8), pic(-######.##),~
                                                                         ~
               at (13,69), fac(lfac$(1)), tqty(line%+1), pic(-######.##),~
               at (14,69), fac(lfac$(2)), tqty(line%+2), pic(-######.##),~
               at (15,69), fac(lfac$(3)), tqty(line%+3), pic(-######.##),~
               at (16,69), fac(lfac$(4)), tqty(line%+4), pic(-######.##),~
               at (17,69), fac(lfac$(5)), tqty(line%+5), pic(-######.##),~
               at (18,69), fac(lfac$(6)), tqty(line%+6), pic(-######.##),~
               at (19,69), fac(lfac$(7)), tqty(line%+7), pic(-######.##),~
               at (20,69), fac(lfac$(8)), tqty(line%+8), pic(-######.##),~
                                                                         ~
               at (21,02), fac(hex(a4)),   blankline$           , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over    (2)First   (5)Next                   ~
        ~        (13)Instructions",                                       ~
               at (23,02),                                               ~
                  "                 (3)Last    (6)Down                   ~
        ~        (15)Print Screen",                                       ~
               at (24,02),                                               ~
                  "                 (4)Prev    (7)Up                     ~
        ~                        ",                                       ~
               at (24,64), fac(hex(8c)), pf16$,                          ~
                   keys(hex(00010203040506070d0f10)), key (keyhit%)

               if keyhit% <> 13 then L43480
                  call "MANUAL" ("JBRWKHNY")
                  goto L42240

L43480:        if keyhit% <> 15 then L43520
                  call "PRNTSCRN"
                  goto L42240

L43520:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50140,         /* JOB NUMBER       */~
                                    L50500,         /* STORE            */~
                                    L50560,         /* LOT              */~
                                    L50780          /* QUANTITY         */
                     return

L50140
*        Test data for JOB NUMBER
            if jbnr$ <> " " then L50220
                str(jbnr$,,2) ="RW"
                jbnrdescr$ = hex(06) & "Existing 'RW' Jobs:"
                call "PLOWCODE" (#3,jbnr$,jbnrdescr$,2%,.3, f1%(3))
                if f1%(3) <> 0% then L50220
                     errormsg$ = hex(00)
                     return
L50220:     if str(jbnr$,,2) ="RW" then goto L50250
                errormsg$ = "You can only address Rework Jobs"
                return
L50250:     call "DESCRIBE" (#3, jbnr$, jbnrdescr$, 1%, f1%(3))
            if f1%(3) <> 0% then L50290
                errormsg$ = "Rework Job Not On File:" & jbnr$
                return
L50290:     get #3, using L50300, jbpart$, jbqty, jbavl, jbcorewip$,      ~
                                 temps$, tempe$, wipacct$, tempf$
L50300:         FMT POS(58), CH(25), 2*PD(14,4), CH(9), POS(147),        ~
                    2*CH(6), CH(9), POS(174), CH(6)
            if tempe$ = " " or tempe$ = blankdate$ then L50340
                call "DATEFMT" (tempe$)
                errormsg$="JOB CLOSED:" & tempe$
                return
L50340:     call "CONVERT" (jbqty, .2, jbquantity$)
            call "CONVERT" (jbavl, .2, jbcomplete$)
            jbavl = jbqty - jbavl
            call "CONVERT" (jbavl, .2, jbavailable$)

            call "PIPINDEX" (#2, str(temps$,,6) , stdate%, u3%)
            call "PIPINDEX" (#2, str(tempf$,,6) , endate%, u3%)

            call "DESCRIBE" (#4, jbpart$, jbpartdescr$, 1%, f1%(4))
            if f1%(4) <> 0% then L50430
                errormsg$ = "Part not on file:" & jbpart$
                return
L50430:     part$ = jbpart$ : partdescr$ = jbpartdescr$
            call "STCCOSTS" (part$, " ", #2, 2%, stdcost, stdcost())
            call "SERENABL" (part$, sn_enable%, u3%, #2, #4)
            return

L50500
*        TEST DATA FOR STORE
            call "GETCODE" (#15, store$, storedescr$, 1%, 0, f1%(15))
            if f1%(15) <> 0% then return
                errormsg$ = "Store not on file:" & store$
                return

L50560
*        Test Data for LOT
            if lotenbl% <> 2% or lot$ <> " " then L50600
                errormsg$ = "Lot Number required for this Part."
                return
L50600:     hnyqty = 0 : lotmsg$ = " "
            call "LOTVALID" (part$, store$, lot$, #2, #4, #5,errormsg$)
            readkey$ = str(part$,,25) & str(store$,,3) & lot$
            call "READ100" (#5, readkey$, f1%(5))
            if f1%(5) = 0% then L50710
                get #5, using L50650, hnyqty
L50650:           FMT POS(69), PD(14,4)

L50710:     call "HNYGLGET" (part$, store$, lot$, hnyacct$, 3%, #4, #5)
            if f1%(5%) = 0% then lotmsg$ = "(Lot not on file)"

            call "CONVERT" (hnyqty, 2.4, hnyqty$)
            return

L50780
*        Test QUANTITY. Neg Means JB To HNY, Pos Means HNY To JB
            low = -jbavl : high = hnyqty : tempqty = 0
            if sign% =  1% then low  =  0   /* Once qty is in may not  */
            if sign% = -1% then high =  0   /* change its sign!        */
            call "NUMTEST" (quantity$, low, high, errormsg$,.2, quantity)
              if quantity < 0 then tempqty = -(quantity)                 ~
                              else tempqty =   quantity
              if quantity >= 0 then L50826
                 if tempqty > jbavl then errormsg$ =                     ~
                    "The Transfer Qty is greater than what is available" ~
                    & " in the Job: " & jbnr$ : goto L50830
L50826:          if tempqty > hnyqty then errormsg$ =                    ~
                    "The Transfer Qty is greater than what is available" ~
                    & " in the Inventory."
L50830:     if errormsg$ <> " " then return
            if quantity  <> 0   then L50880
                errormsg$ = "Quantity cannot be equal to zero"
                return

L50880:     if quantity < 0 then L50930
                call "HNYAVAIL" (#4, #5, part$, store$, lot$,            ~
                                 errormsg$, quantity, temp, returncode%)
                if errormsg$ <> " " then return else L50980

L50930:         if maxline% = 0% then gosub L30000 /* Load Matls Ledger */
                if maxline% > 0% then L50980
                    errormsg$ = "No material has been issued to job yet"
                    return

L50980:     if quantity > 0 then sign% = 1 else sign% = -1%
            if sn_enable% = 0% then return

            sn_qty = abs(quantity)
            if sign% = -1% then L51120

            /* Set arguments for Inventory to Job            */
                sn_loc$       = str(store$,,3) & lot$
                sn_tran_type$ = "JK"
                sn_trankey$   = jbnr$
                sn_status$    = "1"
                sn_source$    = "2"
                goto L51190

L51120:     /* Set arguments for Job to Inventory            */
                sn_loc$       = jbnr$
                sn_tran_type$ = "JR"
                sn_trankey$   = jbnr$
                sn_status$    = "2"
                sn_source$    = "1"

L51190:     call "SERSELCT" (part$, sn_loc$, sn_qty, 1%, 5%,             ~
                             sn_tran_type$, sn_trankey$, sn_status$,     ~
                             sn_source$, errormsg$, #2, #4, #18, #19)
            return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please")
            call "JNLCLOSE" (modno$, jnlid$, pstseq%, returncode%)
            call "FILEBGON" (#50)
            end
