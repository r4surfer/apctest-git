        REM THISPROGRAMWASGENERATEDUSINGTHEGENMENUPROGRAMWHICHISAPROPRIET~
            *                                                           *~
            *  JJJJJ  BBBB   PPPP    OOO    SSS   TTTTT    1            *~
            *    J    B   B  P   P  O   O  S        T     11            *~
            *    J    BBBB   PPPP   O   O   SSS     T      1            *~
            *  J J    B   B  P      O   O      S    T      1            *~
            *   JJ    BBBB   P       OOO    SSS     T    11111          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBPOST1 - Issue components to jobs, in various manors.    *~
            *           One of multiple background posting tasks        *~
            *           servicing SFC. Runs in foreground just as well. *~
            *----------------------------------------------------------Q*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/27/85 ! ORIGINAL                                 ! HES *~
            * 02/13/87 ! Changes for Serial Number Handling       ! LDJ *~
            *          ! Added error messages describing causes   !     *~
            *          ! for various cancel conditions.           !     *~
            * 06/11/87 ! Standard Costing Enahancements           ! HES *~
            * 06/15/90 ! Changed print flag in call to HNYPST2 for! JDH *~
            *          !  withdrawls.                             !     *~
            * 09/10/90 ! G/L Export file modifications.           ! RAC *~
            * 05/21/91 ! Add an Alt Key to JBMASTR2.  Thanks Sid. ! JDH *~
            *          ! Shortened text passed to HNYPST2 so that !     *~
            *          !   Job number is visible in HNYDDISP.     !     *~
            *          !   Thanks Will.                           !     *~
            *          ! No extra reads or puts if GL Export off. !     *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 04/07/92 ! PRR12352 Fixed so that Kit Complete and  ! SID *~
            *          !  Report Complete is now possible when    !     *~
            *          !  SAVETIF$ = "N".                         !     *~
            * 07/28/92 ! MPS/PFM - Added call to HNYUSESB.        ! MLJ *~
            * 02/09/93 ! Minor mods for DEC. Short read key prob. ! JDH *~
            * 03/08/93 ! First things first.  Cover some holes in ! KAB *~
            *          !  KIT COMPLETE logic.  Flag if not 100%   !     *~
            *          !  successful.  Put the brakes on report   !     *~
            *          !  complete to follow (if any). via FLAG   !     *~
            *          !  Write Bad Trans to reflect problems     !     *~
            *          ! Dont save Close Journal Trans            !     *~
            *          ! Write HNYPST2 adjustments to aux. journal!     *~
            *          !  if keeping it.                          !     *~
            * 09/07/94 ! PRR 13264. Fixed GLexport for Kit Complt.! JDH *~
            * 08/13/96 ! Changes for the year 2000.               ! DXL *~
            * 08/14/06 ! (AWD001) Mod to add GLORTRAN             ! CMG *~
            ARYPRODUCTOFCAELUSASSOCIATESSPOKANEWAALLRIGHTSRESERVEDGENMENU

        dim                                                              ~
            acct$(2)9,                   /* Debit/Credit Resp.         */~
            comp_serial$20,              /* Component Serial number    */~
            corepart$25,                 /* Core Part Number           */~
            coreactive$1,                /* Core Active                */~
            costs(12), costs$96,         /* Inventory Costs            */~
            ccosts(12), ccosts$96,       /* Core Shadow Costs          */~
            curr_trans_key$18,           /* Current Transaction Key    */~
            curr_trans_key_hold$18,      /* Current Transaction Key    */~
            datetime$7,                  /* System Date & Time Stamp   */~
            dfcore_var$9,                /* System Default Core Var.   */~
            dfcore_fga$9,                /* System Default Core FGA    */~
            dfcore_wip$9,                /* System Default Core WIP    */~
            errortext$20,                /* Bad Trans text             */~
            gltext$100, gltext1$100,     /* G/L Text string            */~
            gllog$1,                     /* Write Gl Supp. Log?        */~
            hnyacct$9,                   /* Inventory source account   */~
            jbpart$25,                   /* Job part to build          */~
            jnlid$3,                     /* Journal id                 */~
            job$8,                       /* Job number                 */~
            lastjob$8,                   /* Job number last read       */~
            location$30,                 /* Serial Number Location     */~
            lot$16, kitlot$16,           /* Inventory lot number       */~
            message$10,                  /* Messaging work variable    */~
            misskey$50,                  /* Misc Panic Key             */~
            modno$2,                     /* Module id                  */~
            parent_serial$20,            /* Parent / Job Serial Number */~
            part$25,                     /* Inventory part number      */~
            pipout$64,                   /* Pipouts                    */~
            plowkey$100, plowkey1$100,   /* Misc Read & Plow key       */~
            port$4,                      /* Message port id            */~
            postdate$6,                  /* Module posting date        */~
            posttext$40,                 /* Module posting text        */~
            priority$1,                  /* Transaction priority       */~
            r_a$1,                       /* 'R'equested,'A'ctual usage */~
            readkey$100,                 /* Work variable              */~
            record$(7)50,                /* Work variable              */~
            savetif$1,                   /* Flag to Save TIF Record    */~
            ser_lot$16,                  /* Lot S/N withdrawn from     */~
            ser_store$3,                 /* Store S/N withdrawn from   */~
            text$100,                    /* For HNYDETAL history rcrds */~
            store$3,                     /* Warehouse                  */~
            wipacct$9,                   /* Work in process account    */~
            work1$40,                    /* Multiple uses              */~
            usedate$8,                   /* Usage Capture Trans Date   */~
            useseq$3,                    /* Usage Capture Line Seq #   */~
            useso$16,                    /* Usage Capture SO Number    */~
            usetype$5,                   /* Usage Capture Type Code    */~
            userid$3                     /* Stamp of the infamous user */

        dim export_on$1,                 /* G/L Export File processing?*/~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            partcat$4,                   /* Part Category code         */~
            partclass$4,                 /* Part Class code            */~
            partgen$16,                  /* Part Generic code          */~
            tran_type$5,                 /* G/L transaction type       */~
            uom$4                        /* Part unit of measure       */~

        dim division$3,                  /* division number (AWD001)   */~
            schema$8                     /* schema          (AWD001)   */


        dim                                                              ~
            f1%(64),                     /* Record status flags        */~
            f2%(64),                     /* File status flags          */~
            rslt$(64)20,                 /* Text from file opening     */~
            axd$(64)4                    /* Alt key pointer from OPEN  */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #2  ! PIPMASTR ! Planned inv. position master             *~
            * #3  ! HNYMASTR ! Inventory master (descriptions)          *~
            * #4  ! JBMASTR2 ! Job master file                          *~
            * #5  ! JBMATER2 ! Job material ledger                      *~
            * #6  ! JBVALUE2 ! Job value added ledger                   *~
            * #10 ! JBTIF    ! Shop Floor Transaction Image File        *~
            * #12 ! GLMAIN   ! General ledger master file               *~
            * #13 ! GLORTRAN ! GL Oracle transmit file     (AWD001)     *~
            * #14 ! GENCODES ! GENERAL CODES MASTER FILE   (AWD001)     *~
            * #20 ! USERINFO ! Users posting dates                      *~
            * #21 ! WORKFILE ! Inventory adjustment print journal       *~
            * #22 ! JBJNLPF  ! Job GL print journal                     *~
            * #34 ! PIPOUT   ! Planned position out                     *~
            * #40 ! COREXREF ! Core Cross Reference File                *~
            * #41 ! JBMASTRC ! JBMASTR2 Core Appendix                   *~
            * #52 ! HNYQUAN  ! Inventory quantity  file                 *~
            * #54 ! SYSFILE2 ! Inventory default values                 *~
            * #55 ! HNYPOOL  ! Lifo/Fifo pools- Part/Str/Lot/rev seq    *~
            * #56 ! HNYDETAL ! Inventory detail file                    *~
            * #57 ! GLDETAIL ! G/L Detail file                          *~
            * #58 ! SFCUM    ! Sales forecasts                          *~
            * #61 ! SERTIF   ! Serial Numbers Transactions Buffer       *~
            * #62 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #63 ! SERDETAL ! Serial Number Tracking Relationships file*~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #2,   "PIPMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos = 2, keylen = 25,                        ~
                         alternate key 1, keypos = 1, keylen = 26

            select #3, "HNYMASTR",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 900,                                    ~
                       keypos = 1, keylen = 25

            select #4, "JBMASTR2",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 1300,                                   ~
                       keypos = 1, keylen = 8,                           ~
                       alternate key 1, keypos = 1120, keylen = 19, dup, ~
                                 key 2, keypos =   58, keylen = 25, dup

            select #5, "JBMATER2",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 400,                                    ~
                       keypos = 1, keylen = 22,                          ~
                       alternate key 1, keypos = 23, keylen = 48

            select  #6, "JBVALUE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 23

            select #10, "JBTIF",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos =    9, keylen =  18,                     ~
                        alt key  1, keypos =  1, keylen =  26

            select  #12, "GLMAIN",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 9

/*(AWD001)*/
            select #13, "GLORTRAN",                                       ~
                        varc,     indexed, recsize = 512,                 ~
                        keypos = 1,    keylen = 33,                       ~
                        alt key 1, keypos = 31, keylen = 47,              ~
                            key 2, keypos = 81, keylen = 26

            select #14, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24
/*(AWD001)*/

            select #20, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

            select #21, "WORKFILE",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 160,                                  ~
                         keypos = 1, keylen = 19                         ~

            select #22, "JBJNLPF",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 160,                                  ~
                         keypos = 1, keylen = 19                         ~

            select #34, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  64,                                   ~
                        keypos = 1, keylen = 56,                         ~
                        alternate key 1, keypos = 20, keylen = 37

            select #52, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 650,                                   ~
                        keypos= 17, keylen = 44,                         ~
                        alternate key 1, keypos =  1, keylen = 44

            select #54, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #40, "COREXREF",                                      ~
                        varc, indexed, recsize = 500,                    ~
                        keypos = 26, keylen = 50,                        ~
                         alternate key 1, keypos =  1, keylen = 50,      ~
                                   key 2, keypos = 76, keylen = 25, dup

            select #41, "JBMASTRC",                                      ~
                        varc, indexed, recsize = 600,                    ~
                        keypos =  1, keylen =  8

            select #55, "HNYPOOL",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 38

            select #56, "HNYDETAL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 42,                         ~
                         alternate key 1, keypos = 43, keylen = 6, dup,  ~
                                   key 2, keypos = 49, keylen = 2, dup

            select #57, "GLDETAIL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 26

            select #58, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos = 1, keylen = 25

            select #61, "SERTIF",                                        ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos = 1, keylen = 62

            select #62, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #63, "SERDETAL",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =   46, keylen =  48,                     ~
                        alt key  1, keypos =    1, keylen =  93

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (# 2, "SHARE", f2%( 2), rslt$( 2), axd$( 2))
            call "OPENFILE" (# 3, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            call "OPENFILE" (# 4, "SHARE", f2%( 4), rslt$( 4), axd$( 4))
            call "OPENFILE" (# 5, "SHARE", f2%( 5), rslt$( 5), axd$( 5))
            call "OPENFILE" (# 6, "SHARE", f2%( 6), rslt$( 6), axd$( 6))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))

/*(AWD001)*/
            call "OPENCHCK" (#13, fs%(13), f2%(13), 100%, rslt$(13))
            call "OPENCHCK" (#14, fs%(14), f2%(14),   0%, rslt$(14))
/*(AWD001)*/

            call "OPENFILE" (#20, "SHARE", f2%(20), rslt$(20), axd$(20))
            call "WORKOPEN" (#21, "IO", 100%, f2%(21))
            call "OPENFILE" (#22, "SHARE", f2%(22), rslt$(22), axd$(22))
            call "OPENFILE" (#34, "SHARE", f2%(34), rslt$(34), axd$(34))
            call "OPENFILE" (#52, "SHARE", f2%(52), rslt$(52), axd$(52))
            call "OPENFILE" (#54, "SHARE", f2%(54), rslt$(54), axd$(54))
            call "OPENFILE" (#55, "SHARE", f2%(55), rslt$(55), axd$(55))
            call "OPENFILE" (#56, "SHARE", f2%(56), rslt$(56), axd$(56))
            call "OPENFILE" (#57, "SHARE", f2%(57), rslt$(57), axd$(57))
            call "OPENFILE" (#58, "SHARE", f2%(58), rslt$(58), axd$(58))
            call "OPENFILE" (#61, "SHARE", f2%(61), rslt$(61), axd$(61))
            call "OPENFILE" (#62, "SHARE", f2%(61), rslt$(61), axd$(61))

            REM If JBMATER2 not open, then create it
                if f2%(5) = 0 then L04030
                call "OPENFILE" (#5, "OUTPT", f2%(5), rslt$(5), axd$(5))
                close #5
                call "OPENFILE" (#5, "SHARE", f2%(5), rslt$(5), axd$(5))

L04030:     REM If JBMASTR2 not open, then create it
                if f2%( 4) = 0 then L04090
                call "OPENFILE" (# 4,"OUTPT",f2%( 4),rslt$( 4),axd$( 4))
                close # 4
                call "OPENFILE" (# 4,"SHARE",f2%( 4),rslt$( 4),axd$( 4))

L04090:     REM If JBTIF Not open, then create it
                if f2%(10) = 0% then L04150
                call "OPENFILE" (#10,"OUTPT",f2%(10),rslt$(10),axd$(10))
                close #10
                call "OPENFILE" (#10,"SHARE",f2%(10),rslt$(10),axd$(10))

L04150:         if f2%(20) <> 0 then exit_program

        REM *************************************************************~
            *              I N I T I A L I Z A T I O N                  *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************




                                                          /* (AWD001)  */

            schema_err%, schema% = 0%
            init(" ") schema$, division$
            call "SCHEMA" (schema$, schema%, #14, schema_err%)


            if schema% = 1% then division$ = "036"   /* NC */
            if schema% = 2% then division$ = "080"   /* NE */
            if schema% = 3% then division$ = "035"   /* AES*/


            call "MSGCREAT" (#54, port$, "PORT.ID.JBPOST1", return%)
              if return% <> 0% then end

            REM Get Idle Timeout if specified...
                timeout% = 240%
                call "READ100" (#54, "SWITCHS.SFC", f1%(54))
                     if f1%(54) = 0% then L09114
                get #54, using L09110, timeout%, savetif$, gllog$,         ~
                         comperrkit$
L09110:              FMT XX(23), BI(2), POS(29), 2*CH(1), POS(55), 2*CH(1)

L09114
*        See if G/L Export is on
            export_on$ = "N"
            call "READ100" (#54, "SWITCHS.GL", f1%(54))
            if f1%(54) = 1% then get #54 using L09122, export_on$
L09122:         FMT POS(22), CH(1)

            REM Clear any old control transactions...
            call "DELETE" (#10, " J1" & hex(0100), 4%)
            call "DELETE" (#10, " J1" & hex(ff00), 4%)
            call "SHOSTAT"                                               ~
                 ("Shop Floor Control Posting Task #1 In Progress")

            plowkey$ = "SWITCHS.COR"
            call "READ100" (#54, plowkey$, corebank%)     /* SYSFILE2 */
            if corebank% = 0% then goto L10000

*        OK, grab the necessary system defaults from SWITCHS.COR.
            get #54 using L09420, dfcore_var$, dfcore_fga$, dfcore_wip$,   ~
                    coreactive$
L09420:         FMT POS(30), CH(9), POS(84), CH(9), POS(119), CH(9),     ~
                    POS(135), CH(1)
            if coreactive$ <> "Y" then corebank% = 0%

            call "OPENCHCK" (#40, f1%(40%), f2%(40), 0%, " ")

L10000: REM *************************************************************~
            *                    M A I N   P R O G R A M                *~
            *-----------------------------------------------------------*~
            * Processing begins here.                                   *~
            *************************************************************

        next_trans
            if function% = 7% then call "TASKUP" ("J2", 0%)
            call "PLOWNEXT" (#10, " J1" & hex(0000), 3%, f1%(10))
                if f1%(10) = 0% then L10400
            inactive% = 0% : mat costs = zer : bad_trans_flag% = 0%
            get #10,using L10500, job$, priority$, function%, userid$,    ~
                                 modno$, jnlid$, pstseq%, postdate$,     ~
                                 part$, store$, lot$, quan, pipout$,     ~
                                 work1$, costs$
            curr_trans_key$ = key(#10)
            call "SHOSTAT" ("Processing Job: " & job$ & " Part: " & part$~
                      & " Str: " & store$ & " Lot: " & lot$)

            on function% gosub L11000,    /* Kit component to job       */~
                               L11000,    /* Move materials in, no pip  */~
                               L20000,    /* Kit Complete Logic         */~
                    transfer_sns_to_job, /* Job to Job Kitting of S/N's*/~
                                    ,    /* Not Used                   */~
                                    ,    /* Not Used                   */~
                               L23000,    /* Release Report Complete    */~
                               L23000,    /* Release Report Complete    */~
                               L23000,    /* Release Report Complete    */~
                               L23000,    /* Release Report Complete    */~
                               L35000,    /* Close G/L batch            */~
                               L23000     /* To Job Blocker             */

            if function% = 99% then exit_program /* End task gracefully*/
            gosub good_trans             /* clear 'RETURN' stack */
            goto next_trans

L10400
*       *** No Hit on File, Check Status, wait for a while & continue
            if message$ = "CANCEL" then exit_program
            if inactive% >= timeout% then exit_program  /* Inactive Tst*/

            REM Go into stand by mode           60 seconds
            call "MSGBFGND"                                              ~
                       ("Shop Floor Posting Task #1 Standing By",        ~
                        port$, message$, 10%, inactive%, u3%)
                if u3% = 16% then exit_program  /* Somethings Whacko */
            function% = 0%
            goto next_trans

L10500:     FMT CH(8),                   /* Job Number                 */~
                XX(1),                   /* Record Status              */~
                XX(2),                   /* Port Id  (trans type)      */~
                CH(1),                   /* Priority                   */~
                XX(6),                   /* Date                       */~
                XX(8),                   /* Time (hhmmss100th)         */~
                BI(1),                   /* Function                   */~
                CH(3),                   /* User Id                    */~
                CH(2),                   /* Module                     */~
                CH(3),                   /* Journal Id                 */~
                BI(4),                   /* Posting Sequence           */~
                CH(6),                   /* Posting Date               */~
                CH(25),                  /* Component Part             */~
                CH(3),                   /* Store                      */~
                CH(6),                   /* Lot                        */~
                PD(14,4),                /* Quantity                   */~
                CH(56),                  /* PIPOUT Key, or ?           */~
                CH(40),                  /* varies                     */~
                CH(96)                   /* Costs                      */

        transfer_sns_to_job
            gosub get_jbpart
            gosub get_kit_part
            gosub kit_serial_numbers
            return

L11000: REM *************************************************************~
            *               I S S U E   M A T E R I A L S               *~
            *-----------------------------------------------------------*~
            * Move component parts into job, possibly effecting PIPOUT  *~
            *************************************************************

            gosub get_jbpart /* Gets JBPART$, JBQTY, & WIPACCT$ ...    */
            gosub get_kit_part
            if abs(quan) < .0001 then L12510  /* if QUAN = zero then ...*/

*        Adjust inventory for quan withdrawn...
            gltext$ = job$
            str(gltext$,31%) = str(part$) & str(store$) & lot$
            str(gltext$,69%), text$ = "MATERIAL ISSUED TO JOB"
            if export_on$ = "Y" then gosub load_gl_info
            if function% <> 1% then str(gltext$,69%), text$ =            ~
                                      "ISSUED MANUALLY TO JOB"
            text$ = text$ & ": " & job$
            hnyacct$ = " "
            call "HNYPST2"  (part$,      /* Part to be updated         */~
                      store$,            /* Store code                 */~
                      lot$,              /* Lot number                 */~
                      -quan,0,0,quan,0,  /* Post to on hand, committed */~
                      costs(),           /* Unit costs                 */~
                      cost,              /* Total cost per unit        */~
                      0,                 /* Unit price                 */~
                      0,                 /* Extension of price         */~
                      postdate$,         /* Module date (yymmdd)       */~
                      "JK",              /* Transaction type (HNYDETAL)*/~
                      text$,             /* Reference text string      */~
                      hnyacct$,          /* Account                    */~
                      wipacct$,          /* Account                    */~
                      3%,                /*                            */~
                      6%,                /*                            */~
                      modno$,            /* Module id                  */~
                      " ",               /* Journal id                 */~
                      pstseq%,           /* Posting sequence number    */~
                      gltext$,           /* GL Text string             */~
                      userid$,           /* Who did the dirty deed     */~
                      #52,               /* UFB Address of HNYQUAN     */~
                      #56,               /* UFB Address of HNYDETAL    */~
                      #54,               /* UFB Address of SYSFILE2    */~
                      #55,               /* UFB Address of HNYPOOLS    */~
                      #3,                /* UFB Address of HNYMASTR    */~
                      #2,                /* UFB Address of PIPMASTR    */~
                      #58,               /* UFB Address of SFCUM2      */~
                      #12,               /* UFB Address of GLMAIN      */~
                      #57,               /* UFB Address of GLDETAIL    */~
                      #21,               /* UFB Address of HNYADJPF    */~
                      1%,                /* Whether or not to print an */~
                                         /*  exception report when a   */~
                                         /*  new HNYQUAN rec created-  */~
                                         /*  1= print, 0 = don't print */~
                      returncode%)       /* Error return from subroutin*/~
                                         /* 0  = Record posted         */~
                                         /* <0 = Posted, pool short    */~
                                         /* by amount of returncode/100*/~
                                         /* 99 = Record *not* posted   */
            if returncode% <= 0% then L11583
               put errortext$, using L11570, returncode%
L11570:        %HNYPST2 Returned:###
               goto bad_trans

L11583:     acct$(1) = wipacct$
            acct$(2) = hnyacct$
            plowkey$ = " "
L11586:     call "PLOWNEXT" (#21, plowkey$, 0%, f1%(21))
               if f1%(21) = 0% then L11630
               get #21 using L11592, glacct$, gltext1$, damt, camt
L11592:        FMT XX(25), CH(9), CH(100), 2*PD(14,4)
               if export_on$ <> "Y" then L11606
               tran_type$ = "MPR01"
               if str(gltext1$,67,1) = "H" then L11604
               str(tran_type$,4,2) = "04"
               if str(gltext1$,65,1) = "S" then L11604
               str(tran_type$,4,2) = "03"
L11604:        put gl_post_info$() using L16560, tran_type$,(damt-camt),0
L11606:        call "GLPOST2" (glacct$, damt, camt, postdate$, 0%,       ~
                               modno$, gltext1$, jnlid$, pstseq% ,       ~
                               userid$, division$, #12, #57, #54, #13,   ~
                               return%, job$, /* (AWD001) */             ~
                               gl_post_info$())
               if gllog$ = "N" then L11615
                  call "GLPRTSUB" (modno$, jnlid$, pstseq%, userid$,     ~
                                   postdate$, hnyacct$, gltext1$, damt,  ~
                                   camt, #22, f2%(22))
L11615:        call "READ101" (#21, str(plowkey$,1%,19%), f1%(21%))
               delete #21
               goto L11586

*        Check For Core
L11630:        coreflg$ = " "
               if corebank% = 0% then L11639
               plowkey$ = " " : str(plowkey$,26%) = part$
               call "REDALT0" (#40, plowkey$, 0%, f1%(40%))
                  if f1%(40%) = 0% then L11639
               if jbcorewip$ = " " then gosub set_jb_core_wip
               acct$(1) = jbcorewip$
               coreflg$ = "CORE"

L11639
*        Update Job's Material Ledger
         call "JBMTPOST"(job$,           /* Job to be updated          */~
                      part$,             /* Part number to post        */~
                      store$,            /* Store number               */~
                      lot$,              /* Lot number                 */~
                      quan,              /* Quantity moved to job      */~
                      costs(),           /* Inventory Costs per unit   */~
                      store$,            /* Store number               */~
                      lot$,              /* Lot number                 */~
                      postdate$,         /* Posting date               */~
                      userid$,           /* Who did the dirty deed     */~
                      coreflg$,          /* Core Flag                  */~
                      #4,                /* UFB Address of JBMASTR2    */~
                      #5,                /* UFB Address of JBMATER2    */~
                      #54,               /* UFB Address of SYSFILE2    */~
                      #41,               /* UFB Address of JBMASTRC    */~
                      returncode%)       /* Error return from sub      */
            if returncode% = 0% then L11960
                put errortext$, using L11780, returncode%
L11780:              %JBMTPOST Return:###

                call "LOTTRACK"                                          ~
                      ("H",             /* From flag H=Inventory       */~
                       part$,           /* Part being moved            */~
                       lot$,            /* From lot                    */~
                       store$,          /* From store                  */~
                       " ",             /* From misc                   */~
                       "D",             /* To flag                     */~
                       "Err-Withdrawal from Job: ",     /* Description */~
                       str(job$,,6),    /* TO LOT                      */~
                       str(job$,7,2),   /* TO STORE                    */~
                       " ",             /* To misc                     */~
                       quan,            /* Quantity moved              */~
                       #3,              /* 'HNYMASTR' File             */~
                       #54)             /* 'SYSFILE2' File             */
                goto bad_trans

L11960:     call "LOTTRACK"                                              ~
                 ("H",             /* From flag  H=Invntory,V=Ven,C=Cus*/~
                  part$,           /* Part being moved                 */~
                  lot$,            /* From lot                         */~
                  store$,          /* From store                       */~
                  " ",             /* From misc                        */~
                  "J",             /* To flag H=Invntory, V=Ven, C=Cus */~
                  jbpart$,         /* To part (if blank them same)     */~
                  "*" & str(job$,,5),    /* to lot                     */~
                  str(job$,6,3),   /* To store                         */~
                  " ",             /* To misc                          */~
                  quan,            /* Quantity moved                   */~
                  #3,              /* 'HNYMASTR' File                  */~
                  #54)             /* 'SYSFILE2' File                  */

            glamt = round(quan * cost, 2%)
            gosub post_gl_common
            gosub usage_capture            /* Now that its a done deal */

L12510
*        Modify PIPOUT if required...
            if function% <> 1% then kit_serial_numbers
            adjust=0 : convert str(work1$,,10) to adjust, data goto L12540
L12540:     if adjust <> 0 then L12560
            if quan = 0 then kit_serial_numbers
L12560:     call "READ101" (#34,str(pipout$,,56), f1%(34))
                if f1%(34) = 0% then kit_serial_numbers
                                     /* Above Shouldn't happen */
            get #34, using L12600, day%, old_qty
L12600:         FMT XX(44), BI(4), XX(8), PD(14,4)
            left = max(0, round(old_qty - (quan + adjust), 2%))
            old_qty = old_qty - left
            if abs(left) > .001 then L12660  /* If not equal to zero ...*/
                delete #34
                goto L12690
L12660:     put #34, using L12680, left
            rewrite #34
L12680:     FMT POS(57), PD(14,4)
L12690:     call "PIPFLAGS" (part$, 1%, day%, old_qty, #2, #58)

        kit_serial_numbers
            plowkey$ = "JK" & str(job$) & str(work1$,38%,3%)
        next_serial_record
            call "PLOWNEXT" (#61, plowkey$, 13%, f1%(61))
            if f1%(61) = 0% then core_shadow
            get #61 using L13740,                                         ~
            parent_serial$, /* Parent S/N the below Component was      */~
                            /* kitted to.                              */~
            comp_serial$,   /* Component Serial Number Kitted OR blank */~
                            /* if only kitted from Store & Lot         */~
            part$,          /* Component Part Code                     */~
                            /* Part this S/N is associated with.       */~
            qty_kitted      /* Quantity Kitted.                        */~
                            /* Quantity = 1 if S/N field contains a    */~
                            /* real S/N. If S/N field contains         */~
                            /* Store&Lot then = Qty Issued.            */

            if comp_serial$ = " " then update_serdetal
            REM *** Change Status in SERMASTR of Component S/N ***
            readkey$ = str(part$) & comp_serial$
            call "READ101" (#62, readkey$, f1%(62))
            if f1%(62) = 0% then delete_from_sertif
            location$ = str(job$) & parent_serial$
            call "GETDTTM" addr(datetime$)
            put #62 using L13880,                                         ~
            "3",            /* Current Status Of a Serial Numbered     */~
                            /* Part.                                   */~
            location$,      /* Current Location Of a Serial Number     */~
            datetime$       /* Date & Time Stamp                       */

            rewrite #62

        update_serdetal
            if p_enabled% = 0% then delete_from_sertif
            readkey$ = str(part$) & str(comp_serial$) & str(jbpart$) &   ~
                       str(parent_serial$) & hex(000000)
L13070:     call "PLOWAL1" (#63, readkey$, 1%, 90%, f1%(63))
            if f1%(63) = 0% then add_serdetal
            if comp_serial$ > " " then L13120
               get #63 using L13190, ser_store$, ser_lot$
               if ser_store$ <> store$ or ser_lot$ <> lot$ then L13070
L13120:     get #63 using L13200, qty_in, qty_out, cur_qty
            if qty_kitted < 0 then qty_out = qty_out + abs(qty_kitted)   ~
                              else qty_in = qty_in + qty_kitted
            cur_qty = cur_qty + qty_kitted
            put #63 using L13200, qty_in, qty_out, cur_qty
            rewrite #63
            goto delete_from_sertif
L13190:        FMT POS(94), CH(3), CH(16)
L13200:        FMT POS(121), 3*PD(14,4)

        add_serdetal
            call "SERMKWRK" (#54, 0%, #63)
            REM *** Find Last Sequence Number Used ***
            readkey$ = str(jbpart$) & str(parent_serial$) & hex(000000)
            call "PLOWNEXT" (#63, readkey$, 45%, f1%(63))
            if f1%(63%) = 0% then seq% = 2000000%                        ~
            else seq% = val(str(readkey$,46%,3%),3) - 1%
            qty_in, qty_out = 0
            if qty_kitted < 0 then qty_out = abs(qty_kitted)             ~
                              else qty_in = qty_kitted
            cur_qty = qty_kitted
            put #63 using L14000,                                         ~
            part$,          /* Component Part Number.                  */~
                            /* Start of Alt Key 1                      */~
            comp_serial$,   /* Component Serial Number                 */~
                            /* May be blank.                           */~
            jbpart$,        /* Parent Part Number                      */~
                            /* Start of Primary Key                    */~
            parent_serial$, /* Parent Serial Number                    */~
            seq%,           /* Reversed Sequence Number                */~
                            /* End of Primary Key, Alt Key 1.          */~
            store$,         /* Warehouse or Store                      */~
                            /* Component Store Number (If drawn from   */~
                            /* Inventory - otherwise blank).           */~
            lot$,           /* Which lot in inventory - always used    */~
                            /* with STORE                              */~
                            /* Component Lot Number (If drawn from     */~
                            /* Inventory -otherwise blank).            */~
            " ",            /* Job Number                              */~
                            /* Component Job Number (If drawn from WIP */~
                            /* -otherwise blank).                      */~
            qty_in,         /* quantity moved into job to date         */~
                            /* Component quantity kitted to make the   */~
                            /* parent part.                            */~
            qty_out,        /* Quantity of a subcomponent withdrawn    */~
                            /* from a job                              */~
                            /* Comp Qty returned to Inventory (not     */~
                            /* used and/or replaced).                  */~
            cur_qty,        /* Actual Quantity of a sub-component used */~
                            /* in a job.                               */~
                            /* Actual Quantity of this component used  */~
                            /* in this part. Used = Qty Kitted - Qty   */~
                            /* Returned.                               */~
            " "             /* Filler (Internal, unused space)         */

            write #63, eod goto add_serdetal

        delete_from_sertif
            call "DELETE" (#61, plowkey$, 62%)
            goto next_serial_record


L13740: FMT                 /* FILE: SERTIF                            */~
            POS(14),        /* Position for Field PARENT SERIAL NUMBER */~
            CH(20),         /* Parent S/N the below Component was      */~
                            /* kitted to.                              */~
            POS(43),        /* Position for Field SERIAL NUMBER        */~
            CH(20),         /* Component Serial Number Kitted OR the   */~
                            /* Store & Lot Kitted from.                */~
            CH(25),         /* Component Part Code                     */~
                            /* Part this S/N is associated with.       */~
            PD(14,4)        /* Quantity Kitted.                        */~
                            /* Quantity = 0 if S/N field contains a    */~
                            /* real S/N. If S/N field contains         */~
                            /* Store&Lot then = Qty Issued.            */

L13880: FMT                 /* FILE: SERMASTR                          */~
            CH(1),          /* Current Status Of a Serial Numbered     */~
                            /* Part.                                   */~
                            /* Start of Alt Key 2                      */~
            CH(30),         /* Current Location Of a Serial Numbered   */~
                            /* Part.                                   */~
            POS(202),       /* Position for Field LAST-MODIFY-DATE     */~
            CH(7)           /* The system date a file or record was    */~
                            /* last modified                           */~
                            /* The System Time when a transaction was  */~
                            /* entered                                 */~

L14000: FMT                 /* FILE: SERDETAL                          */~
            CH(25),         /* Component Part Number.                  */~
                            /* Start of Alt Key 1                      */~
            CH(20),         /* Component Serial Number                 */~
                            /* May be blank.                           */~
            CH(25),         /* Parent Part Number                      */~
                            /* Start of Primary Key                    */~
            CH(20),         /* Parent Serial Number                    */~
            BI(3),          /* Reversed Sequence number                */~
                            /* End of Primary Key, Alt Key 1.          */~
            CH(3),          /* Warehouse or Store                      */~
                            /* Component Store Number (If drawn from   */~
                            /* Inventory - otherwise blank).           */~
            CH(16),         /* Which lot in inventory - always used    */~
                            /* with STORE                              */~
                            /* Component Lot Number (If drawn from     */~
                            /* Inventory -otherwise blank).            */~
            CH(8),          /* Job Number                              */~
                            /* Component Job Number (If drawn from WIP */~
                            /* -otherwise blank).                      */~
            PD(14,4),       /* quantity moved into job to date         */~
                            /* Component quantity kitted to make the   */~
                            /* parent part.                            */~
            PD(14,4),       /* Quantity of a subcomponent withdrawn    */~
                            /* from a job                              */~
                            /* Comp Qty returned to Inventory (not     */~
                            /* used and/or replaced).                  */~
            PD(14,4),       /* Actual Quantity of a sub-component used */~
                            /* in a job.                               */~
                            /* Actual Quantity of this component used  */~
                            /* in this part. Used = Qty Kitted - Qty   */~
                            /* Returned.                               */~
            CH(56)          /* Filler (Internal, unused space)         */

        REM *************************************************************~
            * HANDLE CORE SHADOW VALUE POSTING                          *~
            *************************************************************
        core_shadow
            if corebank% = 0% then return

*        Check For Core
               plowkey$ = " " : str(plowkey$,,25%) = part$
               call "PLOWALTS" (#40, plowkey$, 0%, 25%, f1%(40%))
                  if f1%(40%) = 0% then return
               corepart$ = str(plowkey$,26%,25%)
               ccost = 0 : mat ccosts = zer
               call "STCCOSTS" (corepart$, " ", #54, 2%, ccost, ccosts())
                  if ccost = 0 then return
                     mat ccosts = (quan) * ccosts
                     call "PACKZERO" (ccosts(), ccosts$)
                     glamt = round(ccost * quan, 4)
               get #40 using L15190, acct$(2)
L15190:            FMT POS(137), CH(9)
               if jbcorewip$ = " " then gosub set_jb_core_wip
               acct$(1) = jbcorewip$
               if acct$(2) <> " " then L15280
                  plowkey$ = " " : str(plowkey$,26%) = corepart$
                  call "REDALT0" (#40, plowkey$, 0%, f1%(40%))
                     if f1%(40%) = 0% then L15260
                  get #40 using L15190, acct$(2)
                  if acct$(2) <> " " then L15280
L15260:              acct$(2) = dfcore_fga$

L15280
*        Post to Job & G/L
            posttext$ = "CVA: " & corepart$
            call "CONVERT" (quan, 2.2, str(posttext$,31%,10%))
            str(gltext$,69%) = "CVA: " & corepart$

            call "JBVLPOST"              /*                            */~
                      (#4,               /* UFB ADDRESS OF JBMASTR2    */~
                       #6,               /* UFB ADDRESS OF JBVALUE2    */~
                       #41,              /* UFB ADDRESS OF JBMASTRC    */~
                       returncode%,      /* ERROR RETURN FROM SUBROUTIN*/~
                       job$,             /* JOB  TO BE UPDATED         */~
                       5%,               /* TYPE - CORE SHADOW         */~
                       postdate$,        /* DATE WORK WAS PERFORMED    */~
                                         /* OR DIRECT TRANSACTION DATE */~
                       postdate$,        /* POSTING DATE OF USER       */~
                       userid$,          /* WHO                        */~
                       posttext$,        /* POSTING TEXT               */~
                       ccosts())         /* COSTS PASSED IN            */

            gosub post_gl_common   /* NOTE - could just fall through  */
            return                 /* but just for you 'purists'...   */

        REM *************************************************************~
            * Common G/L Posing block.  Fallen into from CORE_SHADOW,   *~
            *   GOSUB'd from main routine between Lot track & Pip       *~
            *                                                           *~
            *************************************************************
        post_gl_common
            if export_on$ <> "Y" then L16090
            put gl_post_info$() using L16560, "MPR01", -glamt, -quan

L16090:  call "GLPOST2" (acct$(2),       /* Account to be updated      */~
                      0,                 /* Debit amount (0 if credit) */~
                      glamt,             /* Credit amount (0 if debit) */~
                      postdate$,         /* Date of module             */~
                      0%,                /* Which of 3 open months     */~
                      modno$,            /* Type code of transaction   */~
                      gltext$,           /* Reference text (100 chars) */~
                      jnlid$,            /* Journal id                 */~
                      pstseq%,           /* Posting seq number         */~
                      userid$,           /* Who did the dirty deed     */~
                      division$,         /* (AWD001) Division          */~
                      #12,               /* UFB Address of G/L MAIN    */~
                      #57,               /* UFB Address of G/L DETAILS */~
                      #54,               /* UFB Address of SYSFILE2    */~
                      #13,               /* (AWD001) GLORTRAN          */~
                      returncode%,       /* Error return from subroutin*/~
                      " ",               /* Currency code document ID  */~
                      gl_post_info$())   /* G/L export posting info    */

            if gllog$ = "N" then L16300
            call "GLPRTSUB" (modno$, jnlid$, pstseq%, userid$, postdate$,~
                             acct$(2), gltext$, 0, glamt, #22, f2%(22))

L16300:     if export_on$ <> "Y" then L16330
            put gl_post_info$() using L16560, "MPR01", glamt, quan

L16330:  call "GLPOST2" (acct$(1),       /* Account to be updated      */~
                      glamt,             /* Debit amount (0 if credit) */~
                      0,                 /* Credit amount (0 if debit) */~
                      postdate$,         /* Date of module             */~
                      0%,                /* Which of 3 open months     */~
                      modno$,            /* Type code of transaction   */~
                      gltext$,           /* Reference text (100 chars) */~
                      jnlid$,            /* Journal id                 */~
                      pstseq%,           /* Posting seq number         */~
                      userid$,           /* Who did the dirty deed     */~ 
                      division$,         /* (AWD001) Division          */~
                      #12,               /* UFB Address of G/L MAIN    */~
                      #57,               /* UFB Address of G/L DETAILS */~
                      #54,               /* UFB Address of SYSFILE2    */~
                      #13,               /* (AWD001) GLORTRAN          */~
                      returncode%,       /* Error return from subroutin*/~
                      " ",               /* Currency code document ID  */~
                      gl_post_info$())   /* G/L export posting info    */

            if gllog$ = "N" then L16540
            call "GLPRTSUB" (modno$, jnlid$, pstseq%, userid$, postdate$,~
                             acct$(1), gltext$, glamt, 0, #22, f2%(22))

L16540:     return

L16560:     FMT CH(5), POS(18), 2*PD(15,4)

L20000: REM *************************************************************~
            *                K I T   C O M P L E T E                    *~
            *-----------------------------------------------------------*~
            * Routine that can be used to kit (rest) of job complete    *~
            * Note that generic parts are simply not kitted, they must  *~
            * be dealt with manually. (Also true for other conditions   *~
            * such as serial numbered parts ...).                       *~
            *************************************************************
            gosub get_jbpart
*          IF P_ENABLED% = 1% THEN RETURN /* Can't Auto-Kit if Job Part
*                                            to build is Serial Nbred */
*          But Can Kit Most of it!
            quan = quan/10000  /*accrcy*/
            if quan < .0001 then pct = 1 else pct = quan
*        Find pipouts (read kit list)...
            readkey$ = "JOB ORDER: " & str(job$)
            init(hex(00)) str(readkey$,20), costs$ : init ("0") work1$
L20160:     call "PLOWNEXT" (#34, readkey$, 19%, f1%(34))
                if f1%(34) <> 0% then L20180
                   if bad_trans_flag% <> 0% then L21500
                   return
L20180:     get #34, using L20190, part$, quan
L20190:         FMT XX(19), CH(25), XX(12), PD(14,4)
            if quan < 0 then L20160             /* Byproduct or Tool    */
            quan = round(quan * pct, 2)
               if quan <= 0 then L20160         /* No Effective request */

            call "READ100" (#3, part$, f1%(3))
                if f1%(3) = 0% then L20160
            get #3, using L20270, type$
L20270:         FMT XX(179), CH(3)
            convert type$ to type%, data goto L20160 /* No idea, here */
            if type% > 489% and type% < 500% then L20160  /* Tool */
            if type% > 789% and type% < 800% then L20160  /* Tool */
*       ** Anything from here down is an autokit error and will be dealt
*       ** with severely.
            auto_miss% = 1% : kitlot$ = lot$
            if type% = 0% then auto_miss      /* Can't do these         */
            auto_miss% = auto_miss% + 1%
            call "SERENABL" (part$, c_enabled%, u3%, #54, #3)
            if c_enabled% = 1% then auto_miss /* No AutoKit if part is  */
                                              /* Serial Numbered part   */
            auto_miss% = auto_miss% + 1%
            call "LOTENABL" (part$, l_enabled%, u3%, #54, #3)
               if l_enabled% > 0% then L20400  /* Memo or REAL           */
                  kitlot$ = " " : goto L20460  /* Not this part          */
L20400:        kitlot$ = lot$
               if l_enabled% < 2% then L20460  /* Just a Memo            */
               if p_enabled% = 1% then auto_miss  /* Parent Serialized  */
                  auto_miss% = auto_miss% + 1%
                  if kitlot$ = " " then auto_miss /* All Done here      */
                  gltext$ = "LOT-CHECK"
                  call "LOTVALID" (part$, store$, kitlot$, #54, #3, #52, ~
                                   gltext$)
                  if gltext$ <> " " then auto_miss /* No way, Jose      */

L20460:     auto_miss% = 5%
            call "HNYAVAIL" (#3, #52, part$, store$, kitlot$, gltext$,   ~
                               quan, avail, return%)
            if gltext$ <> " " then auto_miss  /* Insufficient Quantity  */

            call "HNYHOLD" (#52, part$, store$, kitlot$, quan, return%)

            call "JB2TIF"               /* Writes to Transaction Image */~
                 ("J1",                 /* Send transaction to JBPOST1 */~
                  0%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  1%,                   /* Transaction type (1 = kit)  */~
                  priority$,            /* Priority                    */~
                  job$,                 /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Originating user            */~
                  postdate$,            /* G/L posting Date            */~
                  part$,                /* Inventory Part Code         */~
                  store$,               /* Inventory Store Code        */~
                  kitlot$,              /* Inventory Lot Id.           */~
                  quan,                 /* Quantity to process         */~
                  str(readkey$,,56),    /* PIPOUT tag if kitting       */~
                  work1$, " ")          /* Not used here               */
            goto L20160


        auto_miss
            on auto_miss% goto  L21100,  /* Type 000 Component          */~
                                L21110,  /* Ser# Component              */~
                                L21120,  /* Comp Lot/ Par Serial        */~
                                L21130,  /* Invalid Lot                 */~
                                L21140   /* Quantity Violation          */
            errortext$ = "Couldn't Kit cmplete"
            goto L21300

L21100:     errortext$ = "Type 000 Component  " : goto L21300
L21110:     errortext$ = "Ser #'ed Component  " : goto L21300
L21120:     errortext$ = "Ser# Par/Lot Tr Comp" : goto L21300
L21130:     errortext$ = "Invalid Comp Lot Cde" : goto L21300
L21140:     errortext$ = "Quantity:           "
              call "CONVERT" (avail, -2.2, str(errortext$,11,10))

L21300:     write #10 using L21400, job$, "X", "J1", priority$, date,     ~
                                   time, 1%, userid$, modno$, jnlid$,    ~
                                   pstseq%, postdate$, part$, store$,    ~
                                   kitlot$, quan, str(readkey$,,56%),    ~
                                   work1$, costs$, errortext$, " ",      ~
                                   eod goto L21300

            bad_trans_flag% = 1%
            goto L20160

L21400:     FMT CH(8),                   /* Job Number                 */~
                CH(1),                   /* Record Status              */~
                CH(2),                   /* Port Id  (trans type)      */~
                CH(1),                   /* Priority                   */~
                CH(6),                   /* Date                       */~
                CH(8),                   /* Time (hhmmss100th)         */~
                BI(1),                   /* Function                   */~
                CH(3),                   /* User Id                    */~
                CH(2),                   /* Module                     */~
                CH(3),                   /* Journal Id                 */~
                BI(4),                   /* Posting Sequence           */~
                CH(6),                   /* Posting Date               */~
                CH(25),                  /* Component Part             */~
                CH(3),                   /* Store                      */~
                CH(6),                   /* Lot                        */~
                PD(14,4),                /* Quantity                   */~
                CH(56),                  /* PIPOUT Key, or ?           */~
                CH(40),                  /* varies                     */~
                CH(96),                  /* Costs                      */~
                CH(20),                  /* Error Text                 */~
                CH(51)                   /* Filler                     */

L21500:     errortext$ = "Kit Complete Failed "
            if comperrkit$ = "N" then return
            misskey$ = str(job$,,8%) & " J1" & priority$ & hex(ff)
L21530:     call "PLOWALTS" (#10, misskey$, 1%, 11%, f1%(10%))
               if f1%(10%) = 0% then return
            get #10 using L21560, u3%, temptojob$, temp$
L21560:         FMT POS(27), BI(1), POS(88), CH(8), POS(144), CH(1)
            if u3% <  7% then L21530
            if u3% > 10% then L21530
            if comperrkit$ <> "B" then L21590
            if str(temp$,1%,1%) = "S" then L21530 /* No Need to block if*/
            if str(temp$,1%,1%) = "Z" then L21530 /* Not based on Act.  */
L21590:        curr_trans_key_hold$ = curr_trans_key$
               curr_trans_key$ = str(misskey$,9%,18%)
               gosub load_trans
*             STR(RECORD$(),10,2) = "J2"
*             STR(RECORD$(),27,1) = HEX(01)  /* Set fnct to rpt comp */
*             IF U3% > 8% THEN STR(RECORD$(),27,1) = HEX(09)
               str(record$(),9,1) = "X"       /* Mark as 'Bad'*/
               str(record$(),280,20) = "Flagged by Kit Comp "
               put str(record$()) using L21700, costs$
L21700:            FMT POS(184), CH(96)
               write #10, using L31730, record$()
               if temptojob$ = " " then L21724
               plowkey1$ = str(temptojob$,,8%) & str(misskey$,9%,4%)
               init (hex(00)) str(plowkey1$,13%)
L21714:        call "PLOWALTS" (#10, plowkey1$, 1%, 11%, f1%(10%))
                  if f1%(10%) = 0% then L21724
               get #10 using L21560, u3%
                   FMT POS(27), BI(1)
               if u3% <> 12% then L21714
               curr_trans_key$ = str(plowkey1$,9%,18%)
               gosub load_trans  /* AND GONE */
               goto L21714

L21724:        curr_trans_key$ = curr_trans_key_hold$
               if p_enabled% = 1% then gosub reverse_serial_numbers
               goto L21530

        reverse_serial_numbers

            plowkey$ = "JC" & str(job$) & str(record$(),181%,3%)

L21840:     call "PLOWNEXT" (#61, plowkey$, 13%, f1%(61))
               if f1%(61%) <> 0% then L21900
                  init (hex(00)) str(plowkey$,14%)
                  call "DELETE" (#61, plowkey$, 13%)
                  return

L21900:     get #61 using L21920, str(plowkey1$,26%,20%),                 ~
                                 str(plowkey1$, 1%,25%)
L21920:         FMT POS(43), CH(20), CH(25)
            call "READ101" (#62, plowkey1$, f1%(62))
               if f1%(62%) = 0% then L21840
            put #62 using L21960, "1", "WP"
L21960:         FMT POS(1), CH(1), POS(216), CH(2)
            rewrite #62
            goto L21840

L23000: REM *************************************************************~
            *      R E L E A S E   P E N D I N G   C O M P   R P T      *~
            *-----------------------------------------------------------*~
            * This simply takes the transaction and changes it from     *~
            * a 'J1' to a 'J2' type so it can be posted by JBPOST2. It  *~
            * had a 'J1' to insure that all materials are issued before *~
            * the other task records completion info. This only comes   *~
            * into play with the 'kit AND report complete' option in    *~
            * JBCMPSUB, but does represent a technique to 'hold' trans  *~
            * for one post pgm until trans in another post pgm are done.*~
            *************************************************************

*        Get trans, change port id.
            gosub load_trans
            str(record$(),10,2) = "J2"
            if function% = 12% then L23180
            str(record$(),27,1) = hex(01)  /* Set function to rpt comp */
            if function% >  8% then str(record$(),27,1) = hex(09)
            call "TASKUP" ("J2", 0%)
L23180:     goto good_trans_special

        REM *************************************************************~
            *         M I S C E L L A N E O U S   R O U T I N E S       *~
            *-----------------------------------------------------------*~
            * Some common routines that are shared by trans types.      *~
            *************************************************************

        get_jbpart
            if job$= lastjob$ and job$<>" " then return /* please note */
            jbqty = 0
            errortext$ = "Invalid Job Number  "
            call "READ100" (#4, job$, f1%(4))
                if f1%(4) = 0 then bad_trans
            get #4, using L31260, jbpart$, jbqty, jbcorewip$, wipacct$
            lastjob$ = job$
            call "SERENABL" (jbpart$, p_enabled%, u3%, #54, #3)
            return

        get_kit_part
            if export_on$ <> "Y" then return
               init (" ") partgen$, uom$, partcat$, partclass$, parttype$
               call "READ100" (#3, part$, f1%(3))
                  if f1%(3) = 0% then return
               get #3 using L31220, partgen$, uom$, partcat$, partclass$, ~
                                   parttype$
            return

L31220:     FMT POS(58), CH(16), CH(4), POS(90), CH(4), POS(133), CH(4), ~
                POS(180), CH(3)

L31260:     FMT POS(58), CH(25), PD(14,4), POS(99), CH(9), POS(159), CH(9)

        set_jb_core_wip
            jbcorewip$ = dfcore_wip$
            plowkey$ = jbpart$
            call "PLOWNEXT" (#40, plowkey$, 25%, f1%(40%))
               if f1%(40%) = 0% then L31360
            get #40 using L31340, jbcorewip$
L31340:         FMT POS(146), CH(9)
            if jbcorewip$ = " " then jbcorewip$ = dfcore_wip$
L31360:     call "READ101" (#4, job$, f1%(4))
               if f1%(4%) = 0% then return /* UNLIKELY */
            put #4 using L31390, jbcorewip$
L31390:         FMT POS(99), CH(9)
            rewrite #4
            return

        bad_trans
            gosub load_trans
            str(record$(),9,1) = "X"   /* Mark as 'Bad'*/
            str(record$(),280,20) = errortext$
            goto good_trans_special

        good_trans
            if bad_trans_flag% <> 0% then bad_trans
            gosub load_trans
            str(record$(),9,1) = "O"   /* Mark as 'Old'*/
            if savetif$  = "N" then goto_next_trans
            if function% > 10% then goto_next_trans
        good_trans_special
                call "PACKZERO" (costs(), costs$)
                put str(record$()) using L31640, costs$
L31640:              FMT POS(184), CH(96)
            write #10, using L31730, record$()
        goto_next_trans
            return clear all
            goto next_trans

        load_trans
            call "READ101" (#10, curr_trans_key$, f1%(10))
                if f1%(10) = 0 then goto_next_trans   /* Can't happen */
            get #10, using L31730, record$()
L31730:         FMT 7*CH(50)
            delete #10
            return

*       ** G/L Export Text Routine
        load_gl_info

            put str(gl_post_info$(),,) using L32500,                      ~
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
                uom$,                    /* Part UOM CH(4)             */~
                store$,                  /* Store Number CH(3)         */~
                " ",                     /* Check Receipt Number CH(8) */~
                " ",                     /* Vendor code CH(9)          */~
                " ",                     /* Vendor type CH(4)          */~
                " ",                     /* Purchase Order CH(16)      */~
                " ",                     /* Receiver Number CH(16)     */~
                " ",                     /* Vendor Invoice CH(16)      */~
                " ",                     /* Check Payment Number CH(8) */~
                " ",                     /* Project code CH(8)         */~
                job$,                    /* Job number CH(8)           */~
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

L32500: FMT     CH(5),                   /* Transaction Type CH(5)     */~
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
L35000: REM *************************************************************~
            *               C L O S E  G / L  B A T C H                 *~
            *                                                           *~
            *                                                           *~
            *************************************************************

            call "JNLCLOSE" (modno$, jnlid$, pstseq%, return%)
            return

        usage_capture             /* Post Quantity Used to Usage Files */
            r_a$ = "A"                             /* Actual Usage     */
            usetype$ = "PROD"                      /* Production Usage */
            usedate$ = postdate$
            call "DATEOK" (usedate$, 0%, " ")
            call "DATUNFMT" (usedate$)
            call "HNYUSESB" (useso$, useseq$, store$, part$, r_a$,       ~
                            usedate$, usetype$, quan)
            return

        REM THISPROGRAMWASGENERATEDBYGENMENUAPROPRIETARYPRODUCTOFCCAELUS*~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENMENUGENMENUG

        exit_program
            call "MESSAGE" addr("DE", port$, return%)
            end /* Later on Doogan */
