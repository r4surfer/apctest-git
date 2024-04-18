        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  RRRR    CCC   V   V  U   U  PPPP    SSS   U   U  BBBB    *~
            *  R   R  C   C  V   V  U   U  P   P  S      U   U  B   B   *~
            *  RRRR   C      V   V  U   U  PPPP    SSS   U   U  BBBB    *~
            *  R   R  C   C   V V   U   U  P          S  U   U  B   B   *~
            *  R   R   CCC     V     UUU   P       SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * RCVUPSUB - Sub for posting and updating receiver files.   *~
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
            * 05/20/87 ! Standard Costing Enhancements            ! ERN *~
            * 08/06/87 ! Added DATE_ST% to PIPIN file to reflect  !     *~
            *          ! Order Date, instead of Due Date.         ! DAW *~
            * 11/11/87 ! Fixed Price/Cost Variance Posting Logic  ! HES *~
            * 11/13/87 ! Fixed Price/Cost Var. Pstng (RTV/REV PPL)! KAB *~
            * 12/22/87 ! Multi-currency. Added VBKLNCUR & RCVLNCUR! JIM *~
            * 10/05/88 ! Scrap Store Postings error               ! JDH *~
            * 06/09/89 ! Deleted lines that converted PO Price &  ! JDH *~
            *          !  Extention to transaction currency       !     *~
            * 08/21/89 ! Corrected GL postings for freight changes! JDH *~
            * 08/29/90 ! G/L Export file modifications            ! RAC *~
            * 05/23/91 ! Conditioned Execution of G/L Export code.! JBK *~
            * 09/18/91 ! PRR 11800.  RCVHNYTF record properly     ! JDH *~
            *          !  deleted when no change non-stocked part.!     *~
            * 03/19/93 ! Added Core Value Coding. Modified two    ! JBK *~
            *          !  GL_EXPORT Qty Values.  Added Channel #45!     *~
            *          !  for JBMASTRC file.                      !     *~
            * 07/14/94 ! For receipts to a 'VEND' step with an    ! ERN *~
            *          !  associated VBKVSA record, writes a log  !     *~
            *          !  record in VSAOUTIN                      !     *~
            * 04/25/95 ! PRR 12026,12664,11945. PO# to HNYPROCU.  ! JDH *~
            * 07/19/96 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "RCVUPSUB" (oldreadkey$, key%, hnydate$,                     ~
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
                       #42)              /* SERMASTR */                  ~

        dim acct$(4)9, oldacct$(4)9,     /* Rcvr accounts; Last Posted */~
            acctposted%(4),              /* Flag for all ready done    */~
            adj_ap_costs(12),            /* Core Adjusted AP_COSTS     */~
            ap_costs(12),                /* Paybles Costs              */~
            apst(12), apst$(13)9,        /* APST Amounts & Accounts    */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bol$30,                      /* BOL #                      */~
            cfg_acct$9,                  /* Core Finished Goods Acct   */~
            core_inv_flag$1,             /* Core Inv Trans Flag        */~
            core_part$25,                /* Core Part for Reman Part   */~
            costs(12),                   /* Array with costs           */~
            costs$96,                    /* Packed costs for writes    */~
            curr$1,                      /* Is currency in effect?     */~
            currency$4,                  /* Currency code              */~
            cur_eff_date$6,              /* Curr conversion eff date   */~
            date_time$14,                /* Date-Time Stamp            */~
            diskkey$100, oldreadkey$100, /* Disk key for plow routines */~
            job_part$25,                 /* The part made by a job     */~
            oldstorelot$9, newstorelot$9,/* Scrap Store Lot (Old/New)  */~
            oldjbcrkey$30, newjbcrkey$30,/* Scrap JBCREDIT Key(Old/New)*/~
            part$25, pippart$25,         /* Part Number                */~
            pc_var_acct$9,               /* Price/Cost Variance Acct   */~
            pipkey$20,                   /* PIP Keys                   */~
            pldate$6,                    /* Begin Planning Date        */~
            plowkey$100,                 /* Disk key for plow routines */~
            ponumber$16,                 /* Purchase Order Number      */~
            procsrce$26,                 /* Source of Procurement      */~
            readkey$100,                 /* Disk key                   */~
            rcvext(5),                   /* Receiver Extensions        */~
            newqty(7),                   /* New quantities             */~
            oldqty(7),                   /* Old quantities             */~
            netqty(7),                   /* Net quantity changes       */~
            shipper$30,                  /* Carrier Description        */~
            vbkqty(6),                   /* Dist Qty on VBKLINES       */~
            totalcost(12),               /* Extended Costs             */~
            statutory$4,                 /* Statutory currency code    */~
            store$3,                     /* Store number               */~
            userid$3,                    /* Userid of this user        */~
            vbkvsa$(2)150                /* VBKVSA Record in a string  */

        dim                                                              ~
            datedue$8,                   /* Date due print             */~
            gltext$100,                  /* GL Text string             */~
            gltype$2,                    /* GL Posting type            */~
            hny_acct$9,                  /* Purchases (line item) Acct */~
            hnydate$6,                   /* HNYPOSTing date            */~
            lot$6,                       /* Used for HNYPOSTing        */~
            record$(16)50,               /* Line Item                  */~
            save_gltext$100,             /* Save GL Text for Core Pst  */~
            save_part$25,                /* Save Reman Part for Core Pt*/~
            textdate$8,                  /* Date used in HNYDETAL text */~
            text$40,                     /* HNYDETAL Text              */~
            vencode$9,                   /* Vendor code                */~
            wipacct$                     /* WIP Account from JBMASTR   */~

        dim storelot$(202)9, storelot$9, /* Inventory Store/Lots       */~
            oldquan(202),                /* Old quantity posted        */~
            newquan(202),                /* New quantity               */~
            failed(202),                 /* Pur Job Failed last time   */~
            jbcrkey$(202)30, jbcrkey$30, /* JBCREDIT Key               */~
            rcvhnyds$200,                /* Record toss                */~
            loc%(1)                      /* Search receiver            */~

        dim                              /* G/L Export Posting Info    */~
            export_on$1,                 /* G/L Export File processing?*/~
            gl_job$8,                    /* Project Number             */~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            glstore$3,                   /* G/L Store Number           */~
            partcat$4,                   /* Part Category Code         */~
            partclass$4,                 /* Part Class Code            */~
            partgen$16,                  /* Part Generic Code          */~
            parttype$3,                  /* Part Type Code             */~
            rcvnbr$16,                   /* Receiver Number CH(16)     */~
            tran_type$5,                 /* G/L Transaction Type       */~
            uom$4,                       /* Part Unit of Measure       */~
            ventype$4                    /* VENDOR TYPE CODE           */~

        dim                              /* Puchase Jobs               */~
            kstr$3,                      /* Kit Complete Store         */~
            klot$3,                      /* Kit Complete Lot           */~
            costmethod$2,                /* Completion Cost Method     */~
            modno$2,                     /* G/L Module                 */~
            jnlid$2,                     /* G/L Journal Id             */~
            priority$1,                  /* Transaction Priority       */~
            task$2,                      /* SFC Background Task        */~
            work$56,                     /* Work String                */~
            work1$40,                    /* Work String                */~
            work2$25                     /* Work String                */

        dim f2%(64),                     /* FILE STATUS FLAGS          */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */~

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
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #43 ! WORKFILE ! General Ledger Workfile                  *~
            * #44 ! VENDOR   ! Vendor Master file                       *~
            * #45 ! JBMASTRC ! Production job Core Value Added File     *~
            * #50 ! VBKVSA   ! Vendor Service Advices                   *~
            * #51 ! VSAOUTIN ! Vendor Service Shipment Log              *~
            * #60 ! VBKLNCUR ! Currency Line Item Information (Vendor)  *~
            * #61 ! RCVLNCUR ! Currency Line Item Information (Receiver)*~
            *************************************************************~

            select #43,  "WORKFILE",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 160,                                  ~
                         keypos = 10, keylen = 19                        ~

            select #44, "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos=1, keylen=9,                              ~
                        alt key 1, keypos = 10, keylen = 30, dup

            select #45, "JBMASTRC",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize =  600,                                 ~
                         keypos = 1, keylen = 8

            select #50, "VBKVSA",                                        ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos = 5,    keylen = 8,                       ~
                        alt key  1, keypos =    1, keylen =  12,         ~
                            key  2, keypos =    2, keylen =  11,         ~
                            key  3, keypos =   13, keylen =  12, dup,    ~
                            key  4, keypos =   29, keylen =   6, dup,    ~
                            key  5, keypos =   41, keylen =  13, dup,    ~
                            key  6, keypos =   50, keylen =   4, dup

            select #51, "VSAOUTIN",                                      ~
                        varc, indexed, recsize =  316,                   ~
                        keypos =   1, keylen =  26,                      ~
                        alt key    1, keypos =  44, keylen =  8, dup,    ~
                            key    2, keypos =  52, keylen = 23, dup

            select #60, "VBKLNCUR",                                      ~
                        varc, indexed, recsize = 100,                    ~
                        keypos = 5, keylen = 28,                         ~
                        alt key 1, keypos = 1, keylen = 32

            select #61, "RCVLNCUR",                                      ~
                        varc, indexed, recsize = 100,                    ~
                        keypos = 5, keylen = 52,                         ~
                        alt key 1, keypos = 1, keylen = 56

*        Open G/L workfile
            call "WORKOPEN" (#43, "IO   ", 100%, f2%(43))


            if been_here_before% <> 0% then goto L10000
                been_here_before% = 1% : postflag% = 1%

*        Check for Multi-Currency
                curr$ = "N" : statutory$ = " "
                call "READ100" (#03, "SWITCHS.CUR", f1%(3))
                    if f1%(3) = 0% then L02760
                get #03 using L02710, curr$, statutory$
L02710:             FMT POS(21), CH(1), CH(4)
                if curr$ <> "Y" then goto L02760
                    call "OPENCHCK" (#60, 0%, f2%(60),   0%, " ")
                    call "OPENCHCK" (#61, 0%, f2%(61), 300%, " ")

L02760
*        See if G/L Export is on
            export_on$ = "N"
            call "READ100" (#3, "SWITCHS.GL", f1%(3))
            if f1%(3) = 1% then get #3 using L02800, export_on$
L02800:         FMT POS(22), CH(1)
            if export_on$ <> "Y" then L02840
                call "OPENCHCK" (#44, 0%, f2%(44),   0%, " ")

L02840
*        See if Core is on
            plowkey$ = "SWITCHS.COR"
            call "READ100" (#3, plowkey$, core_on%)
                if core_on% <> 1% then L02930
            get #3 using L02890, core_inv_flag$
L02890:         FMT POS(134), CH(1)
                if core_inv_flag$ = "Y" then L02930
            core_on% = 0%

L02930
*        Open VSA files
            call "OPENCHCK" (#50, 0%, f2%(50),   0%, " ")
            if f2%(50%) <> 0% then L09000
                call "OPENCHCK" (#51, 0%, f2%(51), 200%, " ")


L09000: REM *************************************************************~
            *                 I N I T I A L I Z A T I O N               *~
            *-----------------------------------------------------------*~
            * Initializes keys, that sort of thing.                     *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            if userid$ = " " then call "EXTRACT" addr("ID", userid$)

*        Retrieve Plan Start Date from SYSFILE2 for PIP records
            if pldate$ <> " " and dateclosed$ <> blankdate$ ~
                          then L10000
                call "READ100" (#3, "MONTHS OPEN", f1%(3))
                    if f1%(3) = 0% then L65000
                get #3, using L09130, pldate$
L09130:             FMT XX(32), CH(6)

L10000: REM *************************************************************~
            *                                                           *~
            *  Line item processing happens here (no arrays)            *~
            *                                                           *~
            *************************************************************

            init (hex(00)) str(oldreadkey$, key% + 1%)
L10035:     mat newqty = zer : mat oldqty = zer : mat netqty = zer

            call "PLOWNEXT" (#36, oldreadkey$, key%, f1%(36)) /*RCVTIF2*/
            if f1%(36) = 0% then L65000

            get #36, using L10065, str(record$())
L10065:         FMT CH(800)
            oldrecd, newrecd, vbkopen, vbkorigopen, apqtyinv = 0
            newfailed = 0
            datedue$, job$, pc_var_acct$ = " "
            diskkey$  = str(record$(),26,44)

            get str(record$()), using L10125,                             ~
                part$, rcvnbr$, vencode$, ponumber$,                     ~
                datedue$, newqty(), rcvext(), onordadj,                  ~
                acct$(), store$, newstorelot$, ap_price,                 ~
                gl_job$, ap_cost, ap_costs()

L10125:         FMT CH(25), CH(16), CH(9), CH(16), POS(116), CH(6),      ~
                    POS(156), 7*PD(14,4), POS(248), 5*PD(14,4), PD(14,4),~
                    4*CH(9), CH(3), POS(341), CH(9), POS(364), PD(14,7), ~
                    POS(400), CH(8), POS(512), 13*PD(14,4)

            newrecd = newqty(1)
            scpstore$ = str(newstorelot$,1,3)

            if export_on$ = "Y" then gosub load_gl_info

*        Get old RCVLINES info
            mat oldacct$ = acct$
            init (hex(00)) str(diskkey$,45)
            call "PLOWNEXT" (#31, diskkey$, 44%, f1%(31)) /* RCVLINES */
            if f1%(31) = 0% then L10240
                get #31 using L10210, oldqty(), oldacct$(), oldstorelot$, ~
                                     oldjbcrkey$, oldfailed
L10210:              FMT POS(156), 7*PD(14,4), POS(296), 4*CH(9),        ~
                         POS(341), CH(9), POS(616), CH(22), PD(14,4)

            if oldfailed > 9e9 then oldfailed = 0
            oldrecd = oldqty(1)

L10240:     mat netqty = newqty - oldqty

*        Check PAYLINES for 'certified' prices
            gosub update_paylines  /* Over-rides A/P price and costs   */

*        Check for Core and set-up variables
*          GOSUB CHECK_FOR_CORE

*        Update VBKLINES with new information
            netrecd  = newrecd - oldrecd
            diskkey$ = str(record$(),42,28)
            call "READ100" (#60, diskkey$, f1%(60))   /* VBKLNCUR */
            call "READ101" (#11, diskkey$, f1%(11))   /* VBKLINES */
            if f1%(11) = 0% then L10585
                get #11, using L10320, vbkrecd, vbkorigopen, datedue$,    ~
                                      job$, vbkqty()
L10320:              FMT POS(101), 2*PD(14,4), POS(142), CH(6),          ~
                         POS(166), CH(8), POS(373), 6*PD(14,4)
                if costin% = 0% then get #11 using L10335, pc_var_acct$
L10335:              FMT POS(525), CH(9)
                vbkrecd   = vbkrecd + netrecd
                vbkopen   = max(0, vbkorigopen - netrecd - onordadj)
                vbkqty(1) = vbkqty(1) + netqty(2)
                vbkqty(2) = vbkqty(2) + netqty(3)
                vbkqty(3) = vbkqty(3) + netqty(4)
                vbkqty(4) = vbkqty(4) + netqty(7)
                vbkqty(5) = vbkqty(5) + netqty(6)
                vbkqty(6) = vbkqty(6) + netqty(5)
                REM Retain old packslip & recv date if line not recvd...
                if abs(netrecd) + abs(onordadj) <> 0 then L10410
                if abs(netqty(2)) + abs(netqty(3)) <> 0 then L10410
                if abs(netqty(4)) + abs(netqty(5)) <> 0 then L10410
                if abs(netqty(6)) + abs(netqty(7)) <> 0 then L10410
                     goto L10585  /* line not effected */
L10410:         put #11, using L10425, vbkrecd, vbkopen,                  ~
                                      str(record$(),122, 6),             ~
                                      str(record$(),128,16), vbkqty()
L10425:              FMT POS(101), 2*PD(14,4), POS(148), CH(6),          ~
                         POS(181), CH(16), POS(373), 6*PD(14,4)
                rewrite #11

*        Update VBKOUTIN if applicable...
            if str(part$,,10) <> "ACTIVITY: " then L10585
            call "READ100" (#50, str(part$,17,8), f1%(50%))  /* VBKVSA */
            if f1%(50) = 0% then L10585               /* Not applicable */
                get #50, str(vbkvsa$(),,300)
                call "READ100" (#22, str(vbkvsa$(),13,8), f1%(22%))
                if f1%(22%) = 1% then get #22 using L10475, job_part$
L10475:              FMT POS(58), CH(25)
                get #35 using L10477, shipper$, bol$
L10477:              FMT POS(37), 2*CH(30)
L10480:         date_time$ =  date & time
                write #51 using L10565,                                   ~
                     str(vbkvsa$(), 13,  12),  /* Job, Rte step        */~
                     date_time$,               /* Date and Time Stamp  */~
                     "R", 0,                   /* Trans Type, Qty Out  */~
                     netrecd,                  /* Net Qty Received     */~
                     str(vbkvsa$(),  5,   8),  /* Advice Number        */~
                     str(vbkvsa$(), 41,   9),  /* Vendor               */~
                     date_time$,               /* Date and Time Stamp  */~
                     job_part$,                /* Job Part             */~
                     str(vbkvsa$(), 50,   4),  /* Activity Code        */~
                     shipper$,                 /* Shipper              */~
                     bol$,                     /* Shipment Doc #       */~
                     "RCVINPUT",               /* Srce-User-Comment    */~
                     date,                     /* Shipment Date        */~
                     " ",                      /* Filler               */~
                     eod goto L10480
L10565:                   FMT CH(12), CH(14), CH(1), 2*PD(14,4), CH(8),  ~
                              CH(9), CH(14), CH(25), CH(4), CH(30),      ~
                              CH(20), CH(51), CH(6), CH(106)

L10585
*        Check for Core and set-up variables
            gosub check_for_core

        REM *************************************************************~
            * Prior to the update chain we pay special attention to     *~
            * freight charges and possible A/P ramifications            *~
            *************************************************************

            if costin% <> 0% then acct$(1) = oldacct$(1)
            if costin% <> 0% then L10790 /* Don't mess with pre-payables*/
            if f1%(31)  = 0% then L10790  /* if A/P has already reversed*/
                hold_price = ap_price
                get #31, using L10650, ap_price
L10650:              FMT POS(364), PD(14,7)
                if str(record$(),67,3) <> "   " then L10790

        REM *************************************************************~
            * Now we know it is a freight charge, (not invoiced)        *~
            * and a previous record has been posted.  Since the         *~
            * here responds only to net change or account changes,      *~
            * some extra work is needed in case the $ amount changes.   *~
            *************************************************************

            if hold_price = ap_price then L10790 /* At most acct chgs */
                holdoldqty = oldqty(1)
                holdnewqty = newqty(1)
                newqty(1), newqty(7) = hold_price
                oldqty(1), oldqty(7) = ap_price
                hold_cost = ap_cost
                hold_adj_ap_cost = adj_ap_cost
                ap_price, ap_cost, adj_ap_cost = 1

                gosub rev_ppl_acct       /* Pre-Paid Liabilities */
                gosub rev_hnya_acct      /* Expense Posting      */

                newqty(1), newqty(7) = holdnewqty
                oldqty(1), oldqty(7) = holdoldqty
                ap_price = hold_price : ap_cost = hold_cost
                adj_ap_cost = hold_adj_ap_cost
                goto L10825     /* Done all that's needed for Frght */

L10790:     gosub gl_journal
            gosub balance_pips
            gosub post_on_order
            gosub post_inventory
            gosub movement_journal
            gosub rejection_journal

L10825:     diskkey$=str(record$(),26,52)
            init (hex(00)) str(diskkey$,45)
            call "DELETE" (#31, diskkey$, 44%)  /* RCVLINES */
            call "DELETE" (#61, diskkey$, 44%)  /* RCVLNCUR */
            call "DELETE" (#36, diskkey$, 44%)  /* RCVTIF2  */
            call "TXTFUTIL" (#40, f2%(40), "TOSS", str(record$(),388,4))
            call "TXTFUTIL" (#40, f2%(40), "TOSS", str(record$(),392,4))
            if newrecd = 0 then L10950
                apopenqty = newqty(1) - apqtyinv - newqty(6)
                rcvext(2) = apopenqty * ap_price
                rcvext(3) = apqtyinv  * ap_price
                rcvext(5) = newqty(6) * ap_price
                rcvext(4) = rcvext(2) + rcvext(3) + rcvext(5) - rcvext(1)
                call "PACKZERO" (ap_costs(), costs$)
                put str(record$()) using L10910, rcvext(), 0, apopenqty,  ~
                                                ap_price, ap_cost,costs$,~
                                                newjbcrkey$, newfailed
L10910:              FMT POS(248), 5*PD(14,4), PD(14,4), POS(356),       ~
                         PD(14,4), PD(14,7), POS(512), PD(14,4), CH(96), ~
                         POS(616), CH(22), PD(14,4)
                gosub rcv_currency_line_file
                write #31, using L10935, str(record$(),,800)
L10935:              FMT CH(800)
                goto L10035     /* Get next transaction */

L10950:     call "TXTFUTIL" (#40, f2%(40), "DELE", str(record$(),388,4))
            call "TXTFUTIL" (#40, f2%(40), "DELE", str(record$(),392,4))
            goto L10035         /* Get next transaction */

        rcv_currency_line_file
            if curr$ <> "Y" then return
            if f1%(60) = 0% then return
                get #60 using L10995, currency$, price, ext, cur_factor1, ~
                    cur_factor2, cur_eff_date$
L10995:             FMT CH(4), POS(33), PD(14,7), PD(14,4), 2*PD(14,7),  ~
                        CH(6)

                if currency$ = " " then return
                if currency$ = statutory$ then return
                write #61 using L11035, currency$, str(record$(),26,52),  ~
                    price, ext, cur_factor1, cur_factor2, cur_eff_date$, ~
                    " "
L11035:             FMT CH(4), CH(52), PD(14,7), PD(14,4), 2*PD(14,7),   ~
                        CH(6), CH(6)

            return

        REM *************************************************************~
            * Update the appropriate GL accounts                        *~
            *************************************************************
        gl_journal
            mat acctposted% = zer
            if acct$(1) = oldacct$(1) then L12035
                gosub rev_ppl_acct  :  acctposted%(1) = 1%
L12035:     if acct$(2) = oldacct$(2) then L12045
                gosub rev_rcvh_acct :  acctposted%(2) = 1%
L12045:     if acct$(3) = oldacct$(3) then L12055
                gosub rev_qch_acct  :  acctposted%(3) = 1%
L12055:     if acct$(4) = oldacct$(4) then L12070
                gosub rev_hnya_acct :  acctposted%(4) = 1%

L12070:     netqty(3)  = netqty(3) + netqty(4)  /* Totals in QC */
            if netqty(1) <> 0 then gosub pre_pay_account
            if netqty(2) <> 0 then gosub rcv_hld_account
            if netqty(3) <> 0 then gosub qcp_hld_account
            if netqty(6) <> 0 then gosub rtv_ppl_account
            if netqty(5) <> 0 then gosub rwk_hld_account
            if netqty(7) <> 0 then gosub hny_stk_account
            netqty(3)  = netqty(3) - netqty(4)  /* Undo above   */
            return

        pre_pay_account
          if acctposted%(1) <> 0% then return
            glamtc  = round(netqty(1) * ap_price, 2)  : glamtd  = 0
            glacct$ = acct$(1)
            str(gltext$,56,13) = " "
            str(gltext$,80,21) = "INT. LIABILITY       "
            tran_type$ = "PXX01" : postqty = netqty(1)
            glstore$ = store$
            gltype$ = "CP"  :  gosub post_to_gl

            invcost = round(netqty(1) * p_c_var_ap_cost, 2)
            glamtd = round(netqty(1) * ap_price, 2) - invcost
            if glamtd = 0 then return
                glamtc = 0
                glacct$ = pc_var_acct$
                str(gltext$,56,13) = " "
                str(gltext$,80,21) = "PRICE/COST VARIANCE  "
                tran_type$ = "PXX02" : postqty = netqty(1)
                glstore$ = store$
                gltype$ = "CV"  :  gosub post_to_gl
                return

        rcv_hld_account
          if acctposted%(2) <> 0% then return
            glamtc  = 0  :  glamtd  = round(netqty(2) * ap_cost, 2)
            glacct$ = acct$(2)
            str(gltext$,56,13) = " "
            str(gltext$,80,21) = "RCVR. HOLDING        "
            tran_type$ = "PXX03" : postqty = netqty(2)
            glstore$ = store$
            gltype$ = "DR"  :  gosub post_to_gl
            return

        qcp_hld_account
          if acctposted%(3) <> 0% then return
            glamtc  = 0  :  glamtd  = round(netqty(3) * ap_cost, 2)
            glacct$ = acct$(3)
            str(gltext$,56,13) = " "
            str(gltext$,80,21) = "PLACED IN Q.C.       "
            tran_type$ = "PXX04" : postqty = netqty(3)
            glstore$ = store$
            gltype$ = "DQ"  :  gosub post_to_gl
            return

        rtv_ppl_account
          if acctposted%(1) <> 0% then return
            glamtc  = 0  :  glamtd  = round(netqty(6) * ap_price, 2)
            glacct$ = acct$(1)
            str(gltext$,56,13) = " "
            str(gltext$,80,21) = "RETURN/REJECT        "
            tran_type$ = "PXX05" : postqty = netqty(6)
            glstore$ = store$
            gltype$ = "DP"  :  gosub post_to_gl

            invcost = round(netqty(6) * p_c_var_ap_cost, 2)
            glamtc = round(netqty(6) * ap_price, 2) - invcost
            if glamtc = 0 then return
                glamtd = 0
                glacct$ = pc_var_acct$
                str(gltext$,56,13) = " "
                str(gltext$,80,21) = "REV PRICE/COST VAR.  "
                gltype$ = "DV"
                tran_type$ = "PXX02" : postqty = netqty(6)
                glstore$ = store$
                gosub post_to_gl
                return

        rwk_hld_account
          if acctposted%(4) <> 0% then return
            glamtc  = 0  :  glamtd  = round(netqty(5) * adj_ap_cost, 2)
            glacct$ = acct$(4)
            str(gltext$,56,9)  = str(record$(),341,9)
            str(gltext$,65,4)  = " "
            str(gltext$,80,21) = "SCRAP/REWORK         "
            tran_type$ = "PXX06" : postqty = netqty(5)
            glstore$ = scpstore$
            gltype$ = "DS"  :  gosub post_to_gl
            if reman% <> 0% then gosub rwk_hld_account_core_value
            return

        hny_stk_account
          if acctposted%(4) <> 0% then return
            glamtc  = 0  :  glamtd  = round(netqty(7) * adj_ap_cost, 2)
            glacct$ = acct$(4)
            str(gltext$,56,9)  = str(record$(),332,9)
            str(gltext$,65,4)  = " "
            str(gltext$,80,21) = "ON-HAND INV.         "
            if f1%(6) = 1% then tran_type$ = "PXX11" else                ~
                        tran_type$ = "PXX07"
            postqty = netqty(7)
            glstore$ = store$
            gltype$ = "DH"  :  gosub post_to_gl
            if reman% <> 0% then gosub hny_stk_account_core_value
            return

        rev_ppl_acct
            glamtc  = round(newqty(1) * ap_price, 2)  :  glamtd  = 0
            glacct$ = acct$(1)
            str(gltext$,56,13) = " "
            str(gltext$,80,21) = "INT. LIABILITY       "
            tran_type$ = "PXX01" : postqty = newqty(1)
            glstore$ = store$
            gltype$ = "CP"  :  gosub post_to_gl

            glamtc  = 0  :  glamtd  = round(oldqty(1) * ap_price, 2)
            glacct$ = oldacct$(1)
            str(gltext$,65,4) = "REV"
            postqty = oldqty(1)
            gosub post_to_gl

            invcost = round(netqty(1) * p_c_var_ap_cost, 2)
            glamtd = round(netqty(1) * ap_price, 2) - invcost
            if glamtd = 0 then L12585
                glamtc = 0
                glacct$ = pc_var_acct$
                str(gltext$,56,13) = " "
                str(gltext$,80,21) = "PRICE/COST VARIANCE  "
                gltype$ = "CV"
                tran_type$ = "PXX02" : postqty = netqty(1)
                glstore$ = store$
                gosub post_to_gl

L12585:     glamtc  = 0  :  glamtd  = round(newqty(6) * ap_price, 2)
            glacct$ = acct$(1)
            str(gltext$,56,13) = " "
            str(gltext$,80,21) = "RETURN/REJECT        "
            tran_type$ = "PXX05" : postqty = newqty(6)
            glstore$ = store$
            gltype$ = "DP"  :  gosub post_to_gl

            glamtc  = round(oldqty(6) * ap_price, 2)  :  glamtd  = 0
            glacct$ = oldacct$(1)
            str(gltext$,65,4)  = "REV"
            postqty = oldqty(6)
            gosub post_to_gl

            invcost = round(netqty(6) * p_c_var_ap_cost, 2)
            glamtc = round(netqty(6) * ap_price, 2) - invcost
            if glamtc = 0 then return
                glamtd = 0
                glacct$ = pc_var_acct$
                str(gltext$,56,13) = " "
                str(gltext$,80,21) = "PRICE/COST VARIANCE  "
                gltype$ = "DV" : glstore$ = store$
                tran_type$ = "PXX02" : postqty = netqty(6)
                gosub post_to_gl
            return

        rev_rcvh_acct
            glamtc  = 0  :  glamtd  = round(newqty(2) * ap_cost, 2)
            glacct$ = acct$(2)
            str(gltext$,56,13) = " "
            str(gltext$,80,21) = "RCVR. HOLDING        "
            tran_type$ = "PXX03" : postqty = newqty(2)
            glstore$ = store$
            gltype$ = "DR"  :  gosub post_to_gl

            glamtc = round(oldqty(2) * ap_cost, 2)  :  glamtd = 0
            glacct$ = oldacct$(2)
            str(gltext$,65,4)  = "REV"
            postqty = netqty(2)
            gosub post_to_gl
            return

        rev_qch_acct
            tempnew = newqty(3) + newqty(4)
            glamtc  = 0  :  glamtd  = round(tempnew * ap_cost, 2)
            glacct$ = acct$(3)
            str(gltext$,56,13) = " "
            str(gltext$,80,21) = "PLACED IN Q.C.       "
            tran_type$ = "PXX04" : postqty = tempnew
            glstore$ = store$
            gltype$ = "DQ"  :  gosub post_to_gl

            tempold = oldqty(3) + oldqty(4)
            glamtc  = round(tempold * ap_cost, 2)  :  glamtd  = 0
            glacct$ = oldacct$(3)
            str(gltext$,65,4)  = "REV"
            postqty = tempold
            gosub post_to_gl
            return

        rev_hnya_acct
            glamtc  = 0  :  glamtd  = round(newqty(7) * adj_ap_cost, 2)
            glacct$ = acct$(4)
            str(gltext$,56,9)  = str(record$(),332,9)
            str(gltext$,65,4)  = " "
            str(gltext$,80,21) = "ON-HAND INV.         "
            if f1%(6) = 1% then tran_type$ = "PXX11" else                ~
                        tran_type$ = "PXX07"
            postqty = newqty(7) : glstore$ = store$
            gltype$ = "DH"  :  gosub post_to_gl

            glamtc  = round(oldqty(7) * adj_ap_cost, 2)  :  glamtd  = 0
            glacct$ = oldacct$(4)
            str(gltext$,65,4)  = "REV"
            postqty = oldqty(7)
            gosub post_to_gl

            glamtc  = 0  :  glamtd  = round(newqty(5) * adj_ap_cost, 2)
            glacct$ = acct$(4)
            str(gltext$,56,9)  = str(record$(),341,9)
            str(gltext$,65,4)  = " "
            str(gltext$,80,21) = "SCRAP/REWORK         "
            tran_type$ = "PXX06" : postqty = newqty(5)
            glstore$ = scpstore$
            gltype$ = "DS"  :  gosub post_to_gl

            glamtc  = round(oldqty(5) * adj_ap_cost, 2)  :  glamtd  = 0
            glacct$ = oldacct$(4)
            str(gltext$,65,4) = "REV"
            postqty = oldqty(5)
            gosub post_to_gl
            if reman% <> 0% then gosub rev_hnya_acct_core_value
            return

        post_to_gl
            if abs(glamtc) <> 0 then L12930
                if abs(glamtd)  = 0 then return
L12930:     str(gltext$, 1,30) = str(record$(),42,25) &                  ~
                                 str(record$(),26, 5)
            str(gltext$,31,25) = str(record$(), 1,25)
            str(gltext$,69,11) = str(record$(),31,11)
            postamt = glamtd - glamtc
            if export_on$ = "Y" then gosub load_gl_tran_type
            call "GLPSTSUB" (gltype$, "   ", 0%, userid$, hnydate$,      ~
                         glacct$, gltext$, glamtd, glamtc, #29, f2%(29%),~
                         gl_post_info$())
            return

        REM *************************************************************~
            *             Common PIP In/Out handler                     *~
            *           Here be the DRAGONS and OGRES                   *~
            *************************************************************

        balance_pips

*        First balance PO-Type PIPS with current on-order qty

            if pur_job%   <> 0% then L13210
            if planned%    = 0% then L13210

            store$ = str(record$(),332,3)
            if store$ = " " then L13190    /* same as VBKPOST1 */
            if str(store$,1,1) < "0" then L13210
            if str(store$,1,1) > "9" then L13210

L13190:     call "DATE" addr("G-", pldate$, datedue$, date_in%, err%)
            if err% = 0% then L13230
L13210:         date_in% = 0%
                goto L13260
L13230:     date_in% = date_in% + 1%
            if date_in% < 1% or date_in% > 490% then L13210

L13260:     pipkey$= "PO" & str(record$(),51,14) & str(record$(),67,3)
            pipqty = - min(netqty(1) + onordadj, vbkorigopen)
            gosub post_pips

*        Last, balance QC-type PIPs with V2PLINE array

            if pur_job%   <> 0% then L13730
            if planned% = 0% then L13730

            if store$ = " " then L13710    /* same as vbkpost1 */
            if str(store$,1,1) < "0" then L13730
            if str(store$,1,1) > "9" then L13730

L13710:     call "DATE" addr("G-",pldate$,datedue$,date_in%,err%)
                if err% = 0% then L13750
L13730:            date_in% = 0%
                   goto L13780
L13750:     date_in% = date_in% + 1%
            if date_in% < 1% or date_in% > 490% then L13730

L13780:     pipkey$= "QC" & str(record$(),51,14) & str(record$(),67,3)
            pipqty =  netqty(2) + netqty(3) + netqty(4)

        post_pips
            pqty = 0
            call "READ101" (#19, pipkey$, f1%(19))
               if f1%(19) = 0% then L13900
            get #19 using L13860, pippart$, in%, pqty, date_st%
L13860:         FMT CH(25), BI(4), XX(19), PD(14,4), BI(4)
            delete #19
            call "PIPFLAGS" (pippart$, 1%, in%, -pqty, #18, #21)

L13900:     if date_in% = 0% then L13980
            pqty = pqty + pipqty
            if pqty < .0001 then L13980

            write #19 using L13950,part$,date_in%,pipkey$,pqty, date_st%
L13950:           FMT CH(25), BI(04), CH(19), PD(14,4), BI(04)
            call "PIPFLAGS" (part$, 1%, date_in%, pqty, #18, #21)

L13980:     return

        REM *************************************************************~
            *             Common HNYQUAN update                         *~
            *************************************************************

        post_on_order
            part$  = str(record$(),  1,25)
            store$ = str(record$(),332, 3)
            lot$   = str(record$(),335, 6)
            if stocked%  = 0% then return
            if pur_job% <> 0% then return
            ooquan = - min(netqty(1) + onordadj, vbkorigopen)
            qcquan = netqty(3) + netqty(4)

            call "HNYPST1" (part$, store$, lot$, 0, 0, ooquan, 0, qcquan,~
                            #1, #6, #3, f2%(1), f2%(6), f2%(3), 0%, 0%)

            return

        REM *************************************************************~
            * Handle on-hand receipts                                   *~
            *************************************************************

        post_inventory

*        Decide what to do
            if stocked%  = 0% then post_to_non_stocked
            if pur_job% <> 0% then gosub submit_job_value_added_adj

            init (" ") storelot$(), jbcrkey$()
            mat oldquan = zer
            mat newquan = zer
            mat failed  = zer
            count% = 0%

            diskkey$ = str(record$(),26,44) & str(record$(),,25)
            init (hex(00)) str(diskkey$,70)

L16200:     call "PLOWNXT1" (#32, diskkey$, 69%, f1%(32)) /* RCVHNYDS */
            if f1%(32) = 0% then L16360
                get #32, using L16230, storelot$, oldquan, jbcrkey$, failed
L16230:              FMT POS(70), CH(9), POS(87), PD(14,4),              ~
                         POS(151), CH(22), PD(14,4)
                delete #32
                if failed > 9e9 then failed = 0
                search str(storelot$())=str(storelot$,,9) to loc%() step 9
                loc%(1) = (loc%(1) + 8%)/9%
                if loc%(1) = 0% then count%, loc%(1) = count% + 1%
                storelot$(loc%(1)) = storelot$
                oldquan  (loc%(1)) = oldquan(loc%(1)) + oldquan
                jbcrkey$ (loc%(1)) = jbcrkey$
                if failed(loc%(1)) = 0 then failed(loc%(1)) = failed
                get #32 using L16310, rcvhnyds$
L16310:              FMT CH(200)
                quan = -oldquan
                if pur_job% = 0% then gosub post_to_lot_track
                goto L16200

L16360:     init (hex(00)) str(diskkey$,70)
L16370:     call "PLOWNXT1" (#37, diskkey$, 69%, f1%(37))
            if f1%(37) = 0% then L16580
                get #37, using L16400, storelot$, newquan
L16400:              FMT POS(70), CH(9), POS(87), PD(14,4)
                delete #37
                search str(storelot$())=str(storelot$,,9) to loc%() step 9
                loc%(1) = (loc%(1) + 8%)/9%
                if loc%(1) = 0% then count%, loc%(1) = count% + 1%
                storelot$(loc%(1)) = storelot$
                newquan  (loc%(1)) = newquan(loc%(1)) + newquan
                get #37 using L16480, rcvhnyds$
L16480:              FMT CH(200)
                put str(rcvhnyds$) using L16510, ap_price, ap_cost,       ~
                                   acct$(4), hnydate$, jbcrkey$(loc%(1)),~
                                   0
L16510:         FMT POS(95), 2*PD(14,4), POS(119), CH(9), CH(6),         ~
                    POS(151), CH(22), PD(14,4)
                write #32, using L16480, rcvhnyds$
                if pur_job% = 0% then gosub process_serial_numbers
                quan = newquan
                if pur_job% = 0% then gosub post_to_lot_track
                goto L16370

L16580:     if oldqty(5) = 0 or oldstorelot$ = " " then L16700
            search str(storelot$())=str(oldstorelot$,,9) to loc%() step 9
            loc%(1) = (loc%(1) + 8%)/9%
            if loc%(1) = 0% then count%, loc%(1) = count% + 1%
            storelot$(loc%(1)) = oldstorelot$
            oldquan  (loc%(1)) = oldquan(loc%(1)) + oldqty(5)
            jbcrkey$ (loc%(1)) = oldjbcrkey$
            if failed(loc%(1)) = 0 then failed (loc%(1)) = oldfailed
            rcvhnyds$ = " "
            str(rcvhnyds$, 1,44) = str(record$(),26,44)
            str(rcvhnyds$,45,25) = str(record$(),1,25)
            str(rcvhnyds$,70, 9) = oldstorelot$
            quan = -oldqty(5)
            if pur_job% = 0% then gosub post_to_lot_track

L16700:     if newqty(5) = 0 or newstorelot$ = " " then L16820
            search str(storelot$())=str(newstorelot$,,9) to loc%() step 9
            loc%(1) = (loc%(1) + 8%)/9%
            if loc%(1) = 0% then count%, loc%(1) = count% + 1%
            storelot$(loc%(1)) = newstorelot$
            newquan(loc%(1))   = newquan(loc%(1)) + newqty(5)
            rcvhnyds$ = " "
            str(rcvhnyds$, 1,44) = str(record$(),26,44)
            str(rcvhnyds$,45,25) = str(record$(),1,25)
            str(rcvhnyds$,70, 9) = oldstorelot$
            quan = newqty(5)
            if pur_job% = 0% then gosub post_to_lot_track

L16820:     if count% = 0% then return

            mat newquan = newquan - oldquan
            mat newquan = newquan + failed
            part$ = str(record$(),,25)

            for i% = 1% to count%
                if newquan(i%) = 0 then L16930
                     quan     = newquan(i%)   /* Net Quantity */
                     store$   = str(storelot$(i%),1,3)
                     lot$     = str(storelot$(i%),4,6)
                     jbcrkey$ = jbcrkey$(i%)
                     if pur_job% = 0% then gosub post_to_on_hand         ~
                                      else gosub submit_job_completion
L16930:     next i%

            return

        REM *************************************************************~
            * Set PAYLINES OK. Find A/P Price                           *~
            * Tag prepays for NEWRECD <> 0                              *~
            * Release to prepay status if NEWRECD = 0                   *~
            *************************************************************

        update_paylines
            diskkey$ = str(record$(),26,16) & str(record$(),51,19) &     ~
                                                    str(record$(),42,9)
            init (hex(00)) str(diskkey$,45) : costin% = 0%

            if newrecd <> 0 then L17240

*        Check PAYLINES for 'certified' prices (received qty = 0)
L17140:     call "PLOWAL1" (#33, diskkey$, 1%, 44%, f1%(33))
            if f1%(33) = 0% then return
                if costin% <> 0% then L17200
                     get #33 using L17500, ap_price, ap_cost, ap_costs(), ~
                                          pc_var_acct$
                     costin% = 1%
L17200:         put #33 using L17510, " "
                rewrite #33
                goto L17140

L17240
*        Check PAYLINES for 'certified' prices (received qty <> 0)
L17250:     call "PLOWALTS" (#33, diskkey$, 1%, 44%, f1%(33))
            if f1%(33) = 0% then L17350  /* might be pre-pays */
                if costin% <> 0% then L17310
                     get #33 using L17500, ap_price, ap_cost, ap_costs(), ~
                                          pc_var_acct$
                     costin% = 1%
L17310:         get #33 using L17520, invoiceqty
                apqtyinv = apqtyinv + invoiceqty
                goto L17250

L17350:     init (hex(00)) str(diskkey$,45)
            init (hex(20)) str(diskkey$,1,16)
L17370:     call "PLOWAL1" (#33, diskkey$, 1%, 44%, f1%(33))
            if f1%(33) = 0% then return  /* All Done */
                if costin% <> 0% then L17430
                     get #33 using L17500, ap_price, ap_cost, ap_costs(), ~
                                          pc_var_acct$
                     costin% = 1%
L17430:         get #33 using L17520, invoiceqty
                apqtyinv = apqtyinv + invoiceqty
                put #33 using L17510, str(record$(),26,16)
                rewrite #33
                goto L17370


L17500:     FMT POS(131), PD(14,7), POS(183), 13*PD(14,4), CH(9)
L17510:     FMT POS(  1), CH(16)
L17520:     FMT POS( 98), PD(14,4)

        REM *************************************************************~
            * Take care of Lot Tracking                                 *~
            *************************************************************
         post_to_lot_track

            if str(rcvhnyds$,134,9) = " " then no_vendor_lot

            call "LOTTRACK"        /* RECORD OF MOVEMENT               */~
                 ("P",             /* FROM FLAG  H=INVNTORY,V=VEN,C=CUS*/~
              str(rcvhnyds$,17,25),/* VENDOR/PO#                       */~
              str(rcvhnyds$,42,3), /* VENDOR/PO#/SEQ                   */~
                  " ",             /* FROM STORE                       */~
                  " ",             /* FROM MISC.                       */~
                  "X",             /* TO FLAG H=INVNTORY, V=VEN, C=CUS */~
              str(rcvhnyds$,45,25),/* PART                             */~
              str(rcvhnyds$,17,6), /*  VEND (OR)                       */~
              str(rcvhnyds$,23,3), /* (VEND) OR                        */~
              str(rcvhnyds$,134,9),/* VENDOR LOT                       */~
                  quan,            /* QUANTITY                         */~
                  #6,              /* 'HNYMASTR' FILE                  */~
                  #3)              /* 'SYSFILE2' FILE                  */~

            call "LOTTRACK"        /* RECORD OF MOVEMENT               */~
                 ("X",             /* FROM FLAG  H=INVNTORY,V=VEN,C=CUS*/~
              str(rcvhnyds$,45,25),/* PART                             */~
              str(rcvhnyds$,17,6), /*  VEND (OR)                       */~
              str(rcvhnyds$,23,3), /* (VEND) OR                        */~
              str(rcvhnyds$,134,9),/* VENDOR LOT                       */~
                  "H",             /* TO FLAG H=INVNTORY, V=VEN, C=CUS */~
              str(rcvhnyds$,45,25),/* PART                             */~
              str(rcvhnyds$,73,6), /* LOT                              */~
              str(rcvhnyds$,70,3), /* STORE                            */~
                  " ",             /* VENDOR LOT                       */~
                  quan,            /* QUANTITY                         */~
                  #6,              /* 'HNYMASTR' FILE                  */~
                  #3)              /* 'SYSFILE2' FILE                  */~

            return

        no_vendor_lot

            call "LOTTRACK"        /* RECORD OF MOVEMENT               */~
                 ("P",             /* FROM FLAG  H=INVNTORY,V=VEN,C=CUS*/~
              str(rcvhnyds$,17,25),/* VENDOR/PO#                       */~
              str(rcvhnyds$,42,3), /* VENDOR/PO#/SEQ                   */~
                  " ",             /* FROM STORE                       */~
                  " ",             /* FROM MISC.                       */~
                  "H",             /* TO FLAG H=INVNTORY, V=VEN, C=CUS */~
              str(rcvhnyds$,45,25),/* PART                             */~
              str(rcvhnyds$,73,6), /* LOT                              */~
              str(rcvhnyds$,70,3), /* STORE                            */~
                  " ",             /* VENDOR LOT                       */~
                  quan,            /* QUANTITY                         */~
                  #6,              /* 'HNYMASTR' FILE                  */~
                  #3)              /* 'SYSFILE2' FILE                  */~

            return

        REM *************************************************************~
            *             Serial Number Handling                        *~
            *************************************************************

        process_serial_numbers
                temp% = val(str(rcvhnyds$,143,3),3)
                if temp% > 2000000% then return
                if temp% = 0% then return
                plowkey$ = "PO" & str(rcvhnyds$,,16) &                   ~
                           str(rcvhnyds$,26,19) & str(rcvhnyds$,143,3)

        process_serial_numbers_loop
            call "PLOWNEXT" (#41, plowkey$, 42%, f1%(41))
            if f1%(41) = 0% then return
            serial$ = str(plowkey$,43%)
            readkey$ = str(part$) & serial$
            call "READ101" (#42, readkey$, f1%(42))
            call "GETDTTM" addr(datetime$)
            put #42 using L19580,                                         ~
            "2",                             /* Current Status Of Part */~
            str(rcvhnyds$,70, 3),            /* Warehouse or Store     */~
            str(rcvhnyds$,73, 6),            /* Which lot in inventory */~
            " ",                             /* Not used               */~
            serial$,                         /* Serial Number          */~
            str(rcvhnyds$,45,25),            /* Part code              */~
            serial$,                         /* Serial number          */~
            " ",                             /* Job Number             */~
            str(rcvhnyds$,17, 9),            /* Vendor code            */~
            str(rcvhnyds$, 1,16),            /* Receiver Control Number*/~
            str(rcvhnyds$,26,16),            /* Purchase Order Number  */~
            str(rcvhnyds$,42, 3),            /* Purchase Line Sequence */~
            str(rcvhnyds$,134,9),            /* Vendor Lot             */~
            " ",                             /* The specific BOM       */~
            " ",                             /* The specific routing   */~
            " ",                             /* Date job started       */~
            hnydate$,                        /* Date Added to System   */~
            str(rcvhnyds$,70, 3),            /* Warehouse or Store     */~
            str(rcvhnyds$,73, 6),            /* Which lot in inventory */~
            datetime$,                       /* Date & Time            */~
            userid$,                         /* user-id                */~
            hex(ffffffff),                   /* Internal TXT ID        */~
            "  ",                            /* Transaction Type Code  */~
            " ",                             /* Transaction Key        */~
            " "                              /* Filler                 */~

            if f1%(42) = 0% then write #42 else rewrite #42

            goto process_serial_numbers_loop

L19580: FMT                 /* FILE: SERMASTR                          */~
            CH(1),          /* Current Status Of a Serial Numbered Part*/~
            CH(3),          /* Inventory Store                         */~
            CH(16),         /* Inventory Lot                           */~
            CH(11),         /* Filler                                  */~
            CH(20),         /* Serial Number                           */~
            CH(25),         /* Part code                               */~
            CH(20),         /* Serial Number                           */~
            CH(8),          /* Job Number                              */~
            CH(9),          /* Vendor code                             */~
            CH(16),         /* Receiver Control Number                 */~
            CH(16),         /* Purchase Order Number                   */~
            CH(3),          /* Purchase Line Sequence Number (not ITEM */~
            CH(16),         /* Lot Number                              */~
            CH(3),          /* The specific BOM identifier for a Bill o*/~
            CH(3),          /* The specific routing to use for a Bill. */~
            CH(6),          /* Date production job actually started    */~
            CH(6),          /* Date Job Completed                      */~
            CH(3),          /* Warehouse or Store                      */~
            CH(16),         /* Which lot in inventory - always used wit*/~
            CH(7),          /* Date & Time record was setup on the syst*/~
            CH(3),          /* user-id of specific user                */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(2),          /* Transaction Type Code                   */~
            CH(40),         /* Transaction or Document Line Key value  */~
            CH(43)          /* Filler (Internal, unused space)         */~

        REM *************************************************************~
            *             Do on hand updating here                      *~
            *************************************************************
         post_to_on_hand
            mat costs = zer
            hny_acct$ = " "
            textdate$ = str(record$(),122,6)
            call "DATEFMT" (textdate$)
            text$ = "P.O." & str(record$(),51,15) & textdate$

            str(gltext$, 1,30) = str(record$(),42,25) &                  ~
                                 str(record$(),26,5)
            str(gltext$,31,25) = str(record$(),1,25)
            str(gltext$,69,11) = str(record$(),31,11)

            if quan < 0 then L21190
                mat costs = adj_ap_costs
                text$ = text$ & " RECEIPT"
                goto L21210
L21190:     text$ = text$ & " REVERSAL"

L21210:     call "HNYPST2" (part$, store$, lot$, quan, 0, 0, 0, 0,       ~
                            costs(), total_cost,                         ~
                            0, 0, hnydate$, "PO", text$, hny_acct$,      ~
                            " ", 3%, 1%, "HD", "   ", 0%,                ~
                            gltext$, userid$, #1, #2, #3, #16, #6,       ~
                            #18, #21, #26, #27, #43, 0%, u3%)

L21280:     call "PLOWNXT1" (#43, hex(0000), 0%, f1%(43))
               if f1%(43) = 0% then L21380
               get #43 using L21310, glacct$, gltext$, damt, camt
L21310:        FMT XX(25), CH(9), CH(100), 2*PD(14,4)
                postqty = quan : postamt = damt - camt
                tran_type$ = "PXX11" : glstore$ = store$
                if str(gltext$,67,1) = "H" then L21317
                tran_type$ = "PXX09"
                if str(gltext$,65,1) = "S" then L21317
                tran_type$ = "PXX08"
L21317:         if export_on$ = "Y" then gosub load_gl_tran_type
                call "GLPSTSUB" ("HD", "   ", 0%, userid$, hnydate$,     ~
                               glacct$, gltext$,damt,camt, #29, f2%(29%),~
                               gl_post_info$())
               delete #43
               goto L21280

L21380:     total_cost = total_cost
            vencode$   = str(record$(),42,9)
            datedue$   = str(record$(),116,6)

            if hny_acct$ = acct$(4) then L21720
             /* Accounts changed.  Better adjust this!       */
                glamt = round(quan * adj_ap_cost, 2%)
                str(gltext$,56,9)  = str(record$(),332,9)
                str(gltext$,65,4)  = "ADJ"
                str(gltext$,80,21) = "INV. ASSET ADJUSTMENT"

                gltype$ = "CA"
                glacct$ = acct$(4)
                glamtd  = 0
                glamtc  = glamt
                postamt = -glamt
                tran_type$ = "PXX11"
                glstore$ = store$
                if export_on$ = "Y" then gosub load_gl_tran_type
                gosub post_to_gl

                gltype$ = "DA"
                glacct$ = hny_acct$
                glamtc  = 0
                glamtd  = glamt
                postamt = glamt
                if export_on$ = "Y" then gosub load_gl_tran_type
                gosub post_to_gl

                init (hex(00)) diskkey$
                str(diskkey$,,78) = str(record$(),26,44) &               ~
                                    str(record$(), 1,25) &               ~
                                    str(store$,1,3) & str(lot$,1,6)
L21650:         call "PLOWNXT1" (#32, diskkey$, 78%, f1%(32))/*RCVHNYDS*/
                if f1%(32) = 0% then L21720
                     put #32, using L21680, hny_acct$
L21680:                   FMT POS(119), CH(9)
                     rewrite #32
                     goto L21650

L21720:     gosub save_procurment_history
            if quan >= 0 then return

        REM *************************************************************~
            *     Process reversal for on hand quantity here            *~
            *************************************************************

            mat apst = adj_ap_costs - costs
            qtyadj   = quan

            call "HNYAPST" (part$, store$, lot$, qtyadj, apst(), apst$(),~
                            #1, #3, #16, #6, u3%)

            if u3% <> 1% and u3% <> 2% then return

            apst = 0
            for  a% = 1% to 12%
                apst(a%) = round(apst(a%) * qtyadj, 2%)
                apst     = apst + apst(a%)
            next a%

            str(gltext$, 1,30) = str(record$(),42,25) &                  ~
                                 str(record$(),26,5)
            str(gltext$,31,34) = str(part$,,25) & str(store$,,3) & lot$
            str(gltext$,65, 4) = "ADJ"
            str(gltext$,69,11) = str(record$(),31,11)
            str(gltext$,80,21) = "WITHDRAWAL VAR.      "
            if abs(apst) < .01 then L22290
                glamt = apst : glacct$ = hny_acct$
                postqty = qtyadj : postamt = - glamt
                tran_type$ = "PXX11"
                if export_on$ = "Y" then gosub load_gl_tran_type
                call "GLPSTSUB" ("HA", "   ", 0%, userid$, hnydate$,     ~
                               glacct$, gltext$, 0, glamt, #29, f2%(29%),~
                               gl_post_info$())

L22290:     a1% = 1% : a2% = 12% : ao% = 1%
            tran_type$ = "PXX09"
            if u3% <> 1% then L22330
                a1%, a2% = 1%    : ao% = 0%
                tran_type$ = "PXX08"
                apst(1)  = apst
L22330:     for  a% = a1% to a2%
                if abs(apst(a%)) < .01 then goto L22380
                     glamt = apst(a%) : glacct$ = apst$(a%+ao%)
                     postamt = glamt
                     if export_on$ = "Y" then gosub load_gl_tran_type
                     call "GLPSTSUB" ("HV", "   ", 0%, userid$, hnydate$,~
                               glacct$, gltext$, glamt, 0, #29, f2%(29%),~
                               gl_post_info$())
L22380:     next a%
            return

        REM *************************************************************~
            *          Non-stocked posting happens here                 *~
            *************************************************************
        post_to_non_stocked

            nsqty = netqty(7)
            if pur_job% <> 0% then nsqty = nsqty + netqty(5)

            totalcost = round(nsqty * ap_cost, 4)
            mat totalcost = (nsqty) * ap_costs
            call "PACKZERO" (totalcost(), costs$)

            diskkey$ = str(record$(),26,44) & str(record$(),1,25)
            init (hex(00)) str(diskkey$,70)
            vencode$ = str(record$(),42,9)
            quan = netqty(7)

            if pur_job% <> 0% then L23255

            call "DELETE" (#32, diskkey$, 69%)

            init (hex(00)) str(diskkey$,70)
L23150:     call "PLOWNXT1" (#37, diskkey$, 69%, f1%(37))
            if f1%(37) = 0% then L23255
                get #37 using L23180, rcvhnyds$
L23180:              FMT CH(150)
                delete #37
                put str(rcvhnyds$) using L23220, ap_price, ap_cost,       ~
                                                acct$(4), hnydate$
L23220:              FMT POS(95), 2*PD(14,4), POS(119), CH(9), CH(6)
                write #32, using L23180, rcvhnyds$
                goto L23150

L23255:     if nsqty = 0 then return
            if pur_job% = 0% then gosub save_procurment_history

                on jobflag% goto L23310, L23530
                return

L23310:     text$ = "PO:" & ponumber$
            text$ = text$ & " P/N:" & part$
            if pur_job% = 0% then L23320
                    text$ = "PO:" & ponumber$
                    text$ = text$ & " PURCHASE JOB VALUE ADDED"

L23320:     call "JBVLPOST" (#22, #24, #45, return%, job$, 3%, hnydate$, ~
                             hnydate$, userid$, text$, totalcost())

            if return% <> 0% or abs(totalcost) < .01 then return
                call "READ100" (#22, job$, f1%(22))
                if f1%(22) = 0% then return
                     get #22 using L23390, wipacct$
L23390:                   FMT XX(158), CH(9)
                gltext$ = " "
                str(gltext$, 1,30) = job$
                str(gltext$,31,34) = str(vencode$,1,9) & ponumber$
                str(gltext$,65, 4) = " "
                str(gltext$,80,21) = "N/STOCK TO JOB"
                if pur_job% = 0% then L23450
                   str(gltext$,80,21) = "PUR/JOB VALUE ADDED"
L23450:         str(gltext$,26, 5) = str(record$(),26,5)
                str(gltext$,69,11) = str(record$(),31,11)
                postqty = quan : postamt = -totalcost
                tran_type$ = "PXX07" : glstore$ = store$
                if export_on$ = "Y" then gosub load_gl_tran_type
                call "GLPSTSUB" ("WC", "   ", 0%, userid$, hnydate$,     ~
                           acct$(4), gltext$, 0, totalcost, #29, f2%(29),~
                           gl_post_info$())
                tran_type$ = "PXX10" : postamt = totalcost
                if export_on$ = "Y" then gosub load_gl_tran_type
                call "GLPSTSUB" ("WD", "   ", 0%, userid$, hnydate$,     ~
                           wipacct$, gltext$, totalcost, 0, #29, f2%(29),~
                           gl_post_info$())
                return

L23530:     call "JPURPOST" (job$, vencode$, " ", hnydate$, ponumber$,   ~
                             part$, quan, totalcost, acct$(4),           ~
                             #23, #25, f2%(23), f2%(25), return%)
            return

        save_procurment_history
            procsrce$ = str(vencode$) & "P" & str(ponumber$)
            call "HNYPROCU" (part$, procsrce$, hnydate$, quan, ap_cost,  ~
                             datedue$, #6, #3)
        return

        REM *************************************************************~
            * Create record for material movements journal if applicable*~
            *************************************************************
        movement_journal
            if str(record$(),67,3) = "   " then return /* freight! */
            for i% = 1% to 7%
              if netqty(i%) <> 0 then L25105
            next i%
            return        /* nothing moved */

L25105:     init (hex(00)) temp$ : call "GETDTTM" addr(str(temp$,2,7))
            put   #34, using L25180, userid$, str(record$(),1,25),        ~
              /* RCVHNYRP */        userid$, str(record$(),26,44),       ~
                                    temp$, str(record$(),78,170),        ~
                                    oldqty(), str(record$(),332,56),     ~
                                    " "
            put   #34, using L25200, str(record$(), 408,8),               ~
                                    str(record$(), 512,8)
            write #34, eod goto L25105
            return

L25180:     FMT CH(3), CH(25), CH(3), CH(44), CH(8), CH(170),            ~
                7*PD(14,4), CH(56), CH(35)
L25200:     FMT POS(226), CH(8), POS(350), CH(8)

        REM *************************************************************~
            * Write to rejections journal, if needed                    *~
            *************************************************************
        rejection_journal

            if netqty(6) = 0 then return /* no change this time */

L26070:     init (hex(00)) temp$ : call "GETDTTM" addr(str(temp$,2,7))
            write #38, using L26200, str(record$(),1,25),                 ~
                                    str(record$(),42,9), temp$,          ~
                                    str(record$(),26,16),                ~
                                    str(record$(),51,19),                ~
                                    newqty(), netqty(6),                 ~
                                    str(record$(),236,12),               ~
                                    hnydate$,                            ~
                                    str(record$(),350,6),                ~
                                    " ", eod goto L26070

            return

L26200:     FMT CH(25), CH(9), CH(8), CH(16), CH(19), 7*PD(14,4),        ~
                PD(14,4), CH(12), CH(6), CH(6), CH(35)

        REM *************************************************************~
            *     L O A D  G / L  P O S T  I N F O R M A T I O N        *~
            *************************************************************

        load_gl_info

            gosub get_post_info

            put str(gl_post_info$(),,) using L27530,                      ~
                " ",                     /* Transaction Type CH(5)     */~
                currency$,               /* Currency code CH(4)        */~
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
                " ",                     /* Store Number CH(3)         */~
                " ",                     /* Check Receipt Number CH(8) */~
                vencode$,                /* Vendor code CH(9)          */~
                ventype$,                /* Vendor type CH(4)          */~
                ponumber$,               /* Purchase Order CH(16)      */~
                rcvnbr$,                 /* Receiver Number CH(16)     */~
                " ",                     /* Vendor Invoice CH(16)      */~
                " ",                     /* Check Payment Number CH(8) */~
                gl_job$,                 /* Project code CH(8)         */~
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

L27530: FMT     CH(5),                   /* Transaction Type CH(5)     */~
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

        get_post_info
            call "READ100" (#44, vencode$, f1%(44))
                if f1%(44) = 0% then L28020
            get #44 using L28010, ventype$
L28010:     FMT POS(477), CH(4)
L28020:     partcat$, partclass$, partgen$, parttype$, uom$ = " "
            call "READ100" (#6, part$, f1%(6))
                if f1%(6) = 0% then return
            get #6 using L28070, partgen$, uom$, partcat$, partclass$,    ~
                                parttype$
L28070:     FMT POS(58), CH(16), CH(4), POS(90), CH(4), POS(133), CH(4), ~
                POS(180), CH(3)
            return

        load_gl_tran_type
            put str(gl_post_info$(),1,33) using L28190,                   ~
                tran_type$,              /* Transaction Type CH(5)     */~
                currency$,               /* Currency code CH(4)        */~
                cur_factor2,             /* Transaction Currency amount*/~
                postamt,                 /* Functional Currency amount */~
                postqty                  /* Unit amount                */

            str(gl_post_info$(),164,3) = glstore$
            return


L28190: FMT     CH(5),                   /* Transaction Type CH(5)     */~
                CH(4),                   /* Currency code CH(4)        */~
                PD(15,7),                /* Currency units per book    */~
                PD(15,4),                /* Functional Currency amount */~
                PD(15,4)                 /* Unit amount                */

        REM *************************************************************~
            * Check for Reman Part and get Core Data                    *~
            *************************************************************

        check_for_core
            adj_ap_cost      = ap_cost
            p_c_var_ap_cost  = ap_cost
            mat adj_ap_costs = ap_costs
            reman%           = 0%
            jobflag%         = 0%
            stocked%         = 0%
            planned%         = 0%
            pur_job%         = 0%

            part$ = str(record$(),,25%)
            if str(record$(),67,3) = "   " then return
            if part$ = " " then return

            call "READ100" (#6, part$, stocked%)
               if stocked% = 0% then L29110
            call "READ100" (#18, part$, planned%)

L29110:     if job$  = " " then L29190
                call "READ100" (#22, job$, f1%(22)) /* JBMASTR2 */
                if f1%(22) = 0% then L29155
                     get #22 using L29130, dateclosed$
L29130:                   FMT XX(152), CH(6)
                     if dateclosed$ <> " " and dateclosed$ <> blankdate$ ~
                          then L29190
                          jobflag% = 1%
                          goto L29190

L29155:         call "READ100" (#23, job$, f1%(23)) /* JOBMASTR */
                if f1%(23) = 0% then L29190
                     get #23 using L29170, dateclosed$
L29170:                   FMT XX(44), CH(6)
                     if dateclosed$ <> " " and dateclosed$ <> blankdate$ ~
                          then L29190
                          jobflag% = 2%

L29190:     if planned%    = 0% then L29210
            if jobflag%    = 1% then pur_job% = 1%
            if pur_job%   <> 0% then return

L29210:     if core_on%   <> 1% then return
            if stocked% = 0% then return

            call "CORCSTSB" (part$, " ", " ", ap_cost, ap_costs(),       ~
                             core_part$, cfg_acct$, core_std_cost,       ~
                             adj_ap_cost, adj_ap_costs(),                ~
                             p_c_var_ap_cost, #3, #1, #6, #29, ret%)
            if ret% <> 0% then return
            if core_std_cost = 0 then return
            reman% = 1%
            return

        hny_stk_account_core_value
            if core_on%      <> 1% then return
            if reman%        <> 1% then return
            if core_std_cost =  0  then return

            glamtc  = 0  :  glamtd = round(netqty(7%) * core_std_cost, 2%)
            glacct$ = cfg_acct$         :  save_gltext$ = gltext$
            str(gltext$,56%,9%)  = " "  :  str(gltext$,65%,4%)  = " "
            str(gltext$,80,21) = "ON-HAND INV.-CORE VAL"
            save_part$ = part$          :  part$ = core_part$
            if export_on$ = "Y" then gosub load_gl_info
            if f1%(6%) = 1% then tran_type$ = "PXX12" else               ~
                        tran_type$ = "PXX13"
            postqty  = netqty(7%)       :  glstore$ = " "
            gltype$  = "DH"             :  gosub post_to_gl
            gltext$  = save_gltext$     :  part$ = save_part$
            glstore$ = store$
            if export_on$ = "Y" then gosub load_gl_info
            return

        rwk_hld_account_core_value
            if core_on%      <> 1% then return
            if reman%        <> 1% then return
            if core_std_cost =  0  then return

            glamtc  = 0  :  glamtd = round(netqty(5%) * core_std_cost, 2%)
            glacct$ = cfg_acct$         :  save_gltext$ = gltext$
            str(gltext$,56%,9%)  = " "  :  str(gltext$,65%,4%)  = " "
            str(gltext$,80%,21%) = "SCRAP/REWORK-CORE VAL"
            save_part$ = part$          :  part$ = core_part$
            if export_on$ = "Y" then gosub load_gl_info
            tran_type$ = "PXX14"
            postqty  = netqty(5%)
            glstore$ = " "
            gltype$  = "DS"             :  gosub post_to_gl
            gltext$  = save_gltext$     :  part$ = save_part$
            glstore$ = store$
            if export_on$ = "Y" then gosub load_gl_info
            return

        rev_hnya_acct_core_value
            if core_on%      <> 1% then return
            if reman%        <> 1% then return
            if core_std_cost =  0  then return

            save_gltext$ = gltext$
            save_part$ = part$          :  part$ = core_part$
            if export_on$ = "Y" then gosub load_gl_info
            str(gltext$,56%,9%) = " "   :  str(gltext$,65%,4%) = " "
            glacct$ = cfg_acct$         :  glstore$ = " "

            glamtc  = 0  :  glamtd = round(newqty(7%) * core_std_cost, 2%)
            str(gltext$,80%,21%) = "ON-HAND INV.-CORE VAL"
            if f1%(6%) = 1% then tran_type$ = "PXX12" else               ~
                         tran_type$ = "PXX13"
            postqty = newqty(7%)
            gltype$ = "DH"              :  gosub post_to_gl

            glamtc = round(oldqty(7%) * core_std_cost, 2%)  :  glamtd = 0
            str(gltext$,65%,4%)  = "REV"
            postqty = oldqty(7%)
            gosub post_to_gl

            glamtc = 0  :  glamtd = round(newqty(5%) * core_std_cost, 2%)
            str(gltext$,65%,4%)  = " "
            str(gltext$,80%,21%) = "SCRAP/REWORK-CORE VAL"
            tran_type$ = "PXX14" :  postqty = newqty(5%)
            gltype$ = "DS"       :  gosub post_to_gl

            glamtc = round(oldqty(5%) * core_std_cost, 2%)  :  glamtd = 0
            str(gltext$,65%,4%) = "REV"
            postqty = oldqty(5%)
            gosub post_to_gl

            gltext$  = save_gltext$     :  part$ = save_part$
            glstore$ = store$
            if export_on$ = "Y" then gosub load_gl_info
            return

        REM *************************************************************~
            * Fancy Steps here to make JBPOSTx take care of P. Jobs     *~
            *************************************************************

        submit_job_value_added_adj

            if postflag% = 0 then L30060
                modno$ = "03" : jnlid$ = "MPC" : ret% = 0%
                call "JNLINFO" (modno$, jnlid$, pstseq%, summary$,       ~
                                " ", hnydate$, #3, postflag%, ret%)
                postflag% = 0 : summary$ = summary$

L30060:     REM Job Quantity Adjustment?
            adjquan = netqty(6) + onordadj
            if abs(adjquan) < .001 then L30180

            call "JB2TIF"               /* Writes to transaction Image */~
                 ("J2",                 /* Task to pass trans to       */~
                  0%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  10%,                  /* Transaction type            */~
                  hex(30),              /* Priority (within 'J2' only) */~
                  job$,                 /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  hnydate$,             /* G/L posting sequence number */~
                  part$,                /* Inventory Part Code         */~
                  " ",                  /* Inventory Store Code        */~
                  " ",                  /* Inventory Lot Id.           */~
                  adjquan,              /* Quantity to process         */~
                  " ",                  /* used here for passing value */~
                  " ",                  /* used here for passing value */~
                  " ")                  /* Costs                       */

L30180:     nsqty = netqty(5) + netqty(7)
            if nsqty = 0 then L30355

            mat totalcost = (nsqty) * ap_costs
            totalcost = round(nsqty * ap_cost, 4)
            call "PACKZERO" (totalcost(), costs$)
            if pos(costs$ > hex(00)) = 0% then L30355

            temp = totalcost/nsqty

            put work$ using L30235, " ", temp, totalcost, 0, " "
L30235:       FMT CH(20), 3*PD(14,4), CH(9)
            work2$ = hex(03)
            work1$ = "P/J:"
            str(work1$, 6%) = str(record$(),42%, 9%)
            str(work1$,16%) = str(record$(),51%,19%)


            call "JB2TIF"               /* Writes to transaction Image */~
                 ("J2",                 /* Task to pass trans to       */~
                  0%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  5%,                   /* Transaction type (7=special)*/~
                  hex(30),              /* Priority (within 'J2' only) */~
                  job$,                 /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  hnydate$,             /* G/L posting sequence number */~
                  work2$,               /* More Trans Info             */~
                  str(acct$(4),1,3),    /* Inventory Store Code        */~
                  str(acct$(4),4,6),    /* Inventory Lot Id.           */~
                  nsqty,                /* Quantity to process         */~
                  work$,                /* not used                    */~
                  work1$,               /* Carries S/N's Index Pointer */~
                  costs$)               /* Value Added                 */

L30355:     call "TASKUP" ("J2", 0%)  /* Gets to J2 one way or another */

        REM Kit It Complete?

            if kstr$ = " " then return
            nsqty = 0

            call "JB2TIF"               /* Writes to Transaction Image */~
                 ("J1",                 /* Send transaction to JBPOST1 */~
                  0%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  3%,                   /* Transaction type (3=kit cmp)*/~
                  hex(35),              /* Priority                    */~
                  job$,                 /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  hnydate$,             /* G/L posting sequence number */~
                  " ",                  /* Inventory Part Code         */~
                  kstr$,                /* Inventory Store Code        */~
                  klot$,                /* Inventory Lot Id.           */~
                  nsqty,                /* Quantity to process         */~
                  " ",                  /* Not used                    */~
                  " ",                  /* Not used                    */~
                  " ")                  /* Not used                    */~

            call "TASKUP" ("J1", 0%)  /* Gets J1 alive */

            return

        submit_job_completion

*       ** Actual completion Report

        REM Record completion of finished goods...

        REM TRANKEY$ = STR(JOB$) & BIN(3%,3)                             ~
            CALL "SERSAVE" (                                             ~
                        3%,              /* Line Item Pointer.         */~
                        "JC",            /* Source Transaction Type    */~
                        TRANKEY$,        /* Source Transaction Key     */~
                        1%,              /* # Trans to Create File for */~
                        PART$,           /* Part Code                  */~
                        USERID$,         /* Current User ID            */~
                        STATUS$(3),      /* Change Status to ...       */~
                        "1",             /* Change Status from ...     */~
                        0%,              /* Clear TIF after Save (NO)  */~
                        #54,             /* SYSFILE2 UFB               */~
                        #61,             /* SERTIF UFB                 */~
                        #62,             /* SERMASTR UFB               */~
                        #63)             /* SERWORK  UFB               */

        REM Kit It Complete?
            task$ = "J2" : priority$ = hex(30) : function% = 1%
*       *****
            costmethod$ = "SS"       /* Temporary */
*       *****
            put work1$, using L31250, costmethod$, " ", 3%
L31250:         FMT CH(2), CH(35), BI(3)
            str(work1$,3%,19%) = str(record$(),51%,19%)
            work$ = " "
            str(work$, 9%,22%) = jbcrkey$
            str(work$,31%,25%) = str(record$(),26%,25%)
            if str(costmethod$,1,1) = "A" then str(work1$,1,1) = "B"
            if str(costmethod$,2,1) = "A" then str(work1$,2,1) = "B"

            if kstr$ = " " then L31600

            task$ = "J1":priority$ = hex(36):function% = 7%

L31600:     call "JB2TIF"               /* Writes to transaction Image */~
                 (task$,                /* Task to pass trans to       */~
                  0%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  function%,            /* Transaction type (7=special)*/~
                  priority$,            /* Priority (within 'J2' only) */~
                  job$,                 /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  hnydate$,             /* G/L posting sequence number */~
                  part$,                /* Inventory Part Code         */~
                  store$,               /* Inventory Store Code        */~
                  lot$,                 /* Inventory Lot Id.           */~
                  quan,                 /* Quantity to process         */~
                  work$,                /* not used                    */~
                  work1$,               /* Carries S/N's Index Pointer */~
                  " ")                  /* not used                    */

            if task$ = "J1" then call "TASKUP" ("J1", 0%)
            call "TASKUP" ("J2", 0%)  /* Gets to J2 one way or another */

            if str(newstorelot$,1%,3%) <> str(store$,1%,3%) then L31880
            if str(newstorelot$,4%,6%) <> str(lot$  ,1%,6%) then L31880
               newjbcrkey$ = jbcrkey$
               newfailed   = quan

L31880:     diskkey$ = str(record$(),26,44) & str(record$(),,25) &       ~
                       str(store$,,3) & str(lot$,,6)
            init (hex(00)) str(diskkey$,79)
L31910:     call "PLOWNXT1" (#32, diskkey$, 78%, f1%(32))
               if f1%(32%) = 0% then return
            put #32 using L31940, jbcrkey$, quan
L31940:         FMT POS(151), CH(22), PD(14,4)
            rewrite #32
            goto L31910

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Closes all the files currently open, and also displays    *~
            * a message (only if in foreground) while linking to the    *~
            * next program.                                             *~
            *************************************************************

            call "FILEBGON" (#43)
            end
