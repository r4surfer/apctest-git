        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  H   H  N   N  Y   Y   AAA   PPPP    SSS   TTTTT          *~
            *  H   H  NN  N  Y   Y  A   A  P   P  S        T            *~
            *  HHHHH  N N N   YYY   AAAAA  PPPP    SSS     T            *~
            *  H   H  N  NN    Y    A   A  P          S    T            *~
            *  H   H  N   N    Y    A   A  P       SSS     T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYAPST  - Subroutine to adjust existing LIFO/FIFO records*~
            *            for cost variations without actual materials   *~
            *            movement.  Will return proper GL adj. accounts *~
            *            and may in fact do nothing to the pools (tell  *~
            *            you to post it all to variance or adjustments).*~
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
            * 08/31/84 ! ORIGINAL - CLONED FROM HNYPOST           ! KEN *~
            * 05/10/85 ! FIXED ACCOUNT NUMBERS RETURNED FOR STAND-! RAC *~
            *          ! COST METHOD                              !     *~
            * 07/09/85 ! FINAL CLEANUP & GL CODE FMT CONTROL      ! KEN *~
            *          ! PROGRAM STANDARD IS INTERNAL GL CODE     ! KEN *~
            * 01/13/87 ! Fix to NOT allow Negative Costs to be    ! LDJ *~
            *          !   introduced.                            !     *~
            * 05/13/87 ! Increased Standard Costs                 ! KEN *~
            * 06/01/88 ! Corrected short record write             ! MJB *~
            * 10/10/89 ! Added Cost Type 'B'                      ! KAB *~
            * 10/23/92 ! PRR 12655 - For Inv. Value methods X & Y ! RJH *~
            *          !  post ot inv.adj.acct. not adj. pool     !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   HANDLES VARIOUS TYPES OF COSTING AS SELECTED BY USER -  *~
            *        A - AVERAGE COSTING, WITH A KINKY. IF THE INVENTORY*~
            *            IS OVERDRAWN, AND AN ADDITION IS MADE WHICH RE-*~
            *            SULTS IN A ZERO QUANTITY A CORRECTION POSTING  *~
            *            IS MADE TO HNYASSETS AND HNYADJ. ONE POOL REC. *~
            *            THE CORRECTION IS TO ALLOW THE HNYASSET ACCT TO*~
            *            KEEP IN BALANCE WITH THE PHYSICAL INVENTORY.   *~
            *        B - Modified Average.  Acts like 'L' when recover- *~
            *            from Negative On-Hand.                         *~
            *        S - STANDARD COSTING WITH LIFO. INVENTORY ADDITIONS*~
            *            ARE ALWAYS AT CURRENT STANDARD (BETTER BE SURE *~
            *            THEY ARE SET) WITH VARIANCE POSTING.  FORMER   *~
            *            POOL RECORDS RETAIN THEIR RECORDED VALUE.      *~
            *        T - AS ABOVE WITH FIFO                             *~
            *        F - FIXED STANDARD, OR MANUAL COSTING. ONE POOL    *~
            *            RECORD, ALWAYS REFLECTING PRESENT STANDARD COST*~
            *            UPDATED BY ANY MOVEMENT AND POSTING VARIANCE   *~
            *            CORRECTIONS.                                   *~
            *        R - ACTUAL COST VIA THE POOL RECORDS. IF AN OVER-  *~
            *            DRAW OCCURS ANY ADJUSTMENT WILL POST TO THE    *~
            *            ACCOUNTS WHICH CAUSED THE SHORTAGE, IF POSSIBLE*~
            *        P - AS ABOVE WITH FIFO                             *~
            *        X - ACTUAL COSTING , BUT WITH OVERDRAW ADJUSTMENTS *~
            *            POSTING TO THE ADJUSTMENT ACCOUNT.             *~
            *        Y - AS ABOVE WITH FIFO                             *~
            *        L - LAST COST (FOR WHOMEVER WANTS IT). ONE POOL    *~
            *            RECORD CONTINUALLY UPDATED TO SHOW LAST ENTERED*~
            *            COST, WITH ADJUSTMENTS TO HNYADJ.              *~
            *        M - MANUAL COSTING WITH COSTS TAKEN AS IS FROM THE *~
            *            HNYQUAN RECORD                                 *~
            * NOTE THAT ANY OF THE AUTOMATIC CORRECTIONS FEATURES CAN   *~
            * BE BYPASSED BY SETTING THE VARAIANCE ACCOUNTS, ADJUSTMENT *~
            * ACCOUNT AND HNY ASSET ACCOUNT EQUAL, IN WHICH CASE RADIO  *~
            * SHACK CAN PROBABLY DO JUST AS GOOD A JOB AT INVENTORY CON-*~
            * TROL.                                                     *~
            *************************************************************~

        sub "HNYAPST"(part$,             /* PART TO BE UPDATED         */~
                      store$,            /* STORE CODE                 */~
                      lotno$,            /* LOT NUMBER                 */~
                      qtyadj,            /* QUANTITY OF ADJUSTMENT     */~
                      costadj(),         /* UNIT COST ADJ              */~
                      acct$(),           /* HNY ASSET/ADJ ACCOUNT      */~
                                         /* OR VAR ACCOUNTS            */~
                      #1,                /* UFB ADDRESS OF HNYQUAN     */~
                      #3,                /* UFB ADDRESS OF SYSFILE2    */~
                      #4,                /* UFB ADDRESS OF HNYPOOLS    */~
                      #5,                /* UFB ADDRESS OF HNYMASTR    */~
                      returncode%)       /* ERROR RETURN FROM SUBROUTIN*/~
                                         /* 0  = RECORD POSTED/ASSET   */~
                                         /* 1  = NO ACTION ADJ ACCT RET*/~
                                         /* 2  = NO ACTION VAR ACCT RET*/~
                                         /* 99 = PART NOT ON FILE      */~
                                         /* 98 = INVALID COST TYPE     */~

            dim                                                          ~
                acct$(13)9,                                              ~
                hnyacct$(18)9,                                           ~
                cat$4,                                                   ~
                costtype$1,                                              ~
                cost(12),                /* COST ADJUSTMENTS           */~
                costadj(12),             /* COST ADJUSTMENTS           */~
                hnycost(12),             /* COST AVERAGES              */~
                hnycost$96,              /* COST AVERAGES (PACKZERO)   */~
                holdcost$96,             /* COST AVERAGES              */~
                lot$16,                  /* LOT NUMBER                 */~
                lotno$16,                /* LOT NUMBER                 */~
                part$25,                 /* PART NUMBER TO POST TO     */~
                readkey$50,              /* HNYPOOL READKEY            */~
                nextreadkey$50,          /* HNYPOOL READKEY            */~
                lastreadkey$50,          /* HNYPOOL READKEY            */~
                store$3,                 /* STORE (WAREHOUSE) CODE     */~
                text$40,                 /* HNY POOL TEXT              */~
                totalcost(1),            /* COST TOTAL                 */~
                work(1,12)               /* WORK ARRAY                 */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "
        REM *************************************************************

            init (" ") acct$()
                lot$ = lotno$
                mat cost = costadj
                qtyoh    = qtyadj

            returncode% = 99             /* SET SO GOOD RESETS IT.     */

            REM CHECK INCOMING DATA
                if abs(qtyoh) > .0001 then L02130
                   returncode% = 0%
                   end
L02130:         if qtyoh > 0 then L02190
                   mat cost = (-1) * cost
                   qtyoh = -qtyoh

L02190:         call "READ100" (#5, part$, f1%)
                if f1%=0 then end
                get #5, using L02220, cat$, costtype$, hnyacct$()
L02220:          FMT POS(90), CH(4), POS(307), CH(1), POS(335), 18*CH(9)
                if costtype$=" " then costtype$="R"

                returncode% = 98%

        REM *************************************************************~
           *   M A I N   C O N T R O L   S E C T I O N                  *~
            *************************************************************

                readkey$ = part$
                str(readkey$, 26, 3) = store$
                str(readkey$, 29, 16) = lot$
                call "READ101" (#1, readkey$, f1%)
                   if f1% = 0 then  L03180
                get #1, using L03090, qty, totalcost(), hnycost(),         ~
                                    hnyacct$(), costtype$
L03090:             FMT POS(69), PD(14,4), POS(117), PD(14,4),           ~
                        12*PD(14,4), POS(241), 18*CH(9), CH(1)
                     if costtype$ = " " then costtype$ = "R"
                if pos("RXABSFLMPYT"=costtype$) = 0% then end
                if pos("SFLMTXY"=costtype$) <> 0% then L03210
                if qty < 0 then L03210
                if qty - qtyoh < 0 then L03210
                goto L04000

L03180:         call "GLHNYQN" (part$, store$, lot$, cat$, costtype$,    ~
                                                              hnyacct$())

L03210: REM NO HNY QUAN OR INSUFFICIENT QUANTITY ON HAND
                call "STRTRLSE" (#1)
                if pos("RXABSFLMPYT"=costtype$) = 0% then end
                if pos("SFT"=costtype$)<>0% then L03290
                acct$(1) = hnyacct$(6)
                returncode% = 1%
                end

L03290: REM STANDARDS
                str(acct$(),10) = str(hnyacct$(),55)
                returncode% = 2%
                end

L04000: rem**************************************************************~
           *         p o s t   hnyquan                                  *~
           *                                                            *~
           **************************************************************

            acct$(1) = hnyacct$(3)

            gosub L05000

            if min(hnycost()) < 0 then L03210

            returncode% = 0%
            put #1, using L04110, totalcost(), hnycost$
L04110:         FMT POS(117), PD(14,4), CH(96)
            rewrite #1

            holdcost$ = hnycost$                  /* JUST IN CASE...*/
            holdqty   = qty
            holdtotal = totalcost(1)

            goto L08000

L05000: rem**************************************************************~
           *         common cost update                                 *~
           **************************************************************

            for i% = 1% to 12%
               hnycost(i%) =                                             ~
                      round(((hnycost(i%)*qty) + (cost(i%)*qtyoh))/qty,4)
            next i%

            mat work = con:mat totalcost = work * hnycost
            call "PACKZERO" (hnycost(), hnycost$)

            return

L08000: rem**************************************************************~
           *         p o s t   hnypools                                 *~
           **************************************************************

           init(hex(00)) readkey$, /* NOTE THE KEY TO THE HNYPOOL FILE */~
                     nextreadkey$, /* THE SEQ IS AN INTEGER SO MUST    */~
                     lastreadkey$  /* INIT TO HEX(00) NOT TO BLANK     */

           str(readkey$,,34)=str(part$,,25) & str(store$,,3) & lot$
           if pos("AB" = costtype$) <> 0% then gosub L10000
           if pos("PY"=costtype$)<>0% then L08400

        REM HERE WE DEAL WITH LIFO POOLS, NOT BAD

L08220:    call "PLOWNXT1" (#4, readkey$, 34%, f1%)
                     if f1% = 0 then gosub L10000
           gosub L09000
           goto L08220

L08400: REM HERE WE DEAL WITH FIFO POOLS, CURSE THEM

L08420:    init(hex(00)) str(readkey$,35)

L08440:    call "PLOWNEXT" (#4, readkey$, 34%, f1%)
                if f1% = 0 then L08500
                if readkey$ = lastreadkey$ then L08540
                   nextreadkey$=readkey$
                   goto L08440

L08500:         if str(nextreadkey$,1,1) = hex(00)                       ~
                                         then gosub L10000     /* EMPTY */
                nextreadkey$ = readkey$
                goto L08550

L08540:    if nextreadkey$ = lastreadkey$ then gosub L10000  /* LOOPING */
L08550:    readkey$, lastreadkey$ = nextreadkey$
           call "READ101" (#4, readkey$, f1%)
               if f1% = 0 then end
           gosub L09000
           goto L08420

L09000: REM COMMON POOL RECORD HANDLER

           get   #4, using L09080,                                         ~
                     qty,                /* AMOUNT LEFT IN  POOL       */~
                     totalcost(),        /* TOTAL COST                 */~
                     hnycost()           /* COST ELEMENTS              */~


L09080:         FMT POS(39), PD(14,4), POS(55), PD(14,4), 12*PD(14,4)

            if qty < 0 then L10000                              /* OOPS */

            if qty <= qtyoh then L09180
               gosub L05000
            goto L09220

L09180:     mat hnycost = hnycost + cost
            mat work = con: mat totalcost = work * hnycost
            call "PACKZERO" (hnycost(), hnycost$)

L09220:     if min(hnycost()) < 0 then L10000

            put #4, using L09270, totalcost(), hnycost$
L09270:         FMT POS(55), PD(14,4), CH(96)
            rewrite #4

            qtyoh = qtyoh - qty
            if qtyoh > 0 then return

            return clear
            end

L10000: REM *************************************************************~
            * COMPRESS TO ONE POOL RECORD.  ALWAYS VALID FOR AVERAGE,   *~
            * BUT MAY BE NEEDED IF A COST GOES NEGATIVE.                *~
            *************************************************************

            init (hex(00)) str(readkey$,35)
            call "DELETE" (#4, readkey$, 34%) /* SO LONG, RECORDS */

            if pos("AB" = costtype$) <> 0%                               ~
                               then text$ = "Average Pool Record"        ~
                               else text$ = "Consolidated Pool Record"

            revseq% = 10000%

            write #4, using L10280,                                       ~
                      part$,             /* PART                       */~
                      store$,            /* WAREHOUSE                  */~
                      lot$,              /* LOT                        */~
                      revseq%,           /* REVERSE SEQ NUMBER         */~
                      holdqty,           /* AMOUNT LEFT IN  POOL       */~
                      holdqty,           /* AMT ORIG PUT INTO POOL     */~
                      holdtotal,         /* COST TOTAL                 */~
                      holdcost$,         /* COST ELEMENTS              */~
                      date,              /* TRANSACTIONS DATE          */~
                      acct$(3),          /* HNY ASSET ACCOUNT          */~
                      acct$(6),          /* HNY ADJUSTMENT ACCOUNT     */~
                      text$,             /* POSTING TEXT               */~
                      " "                /* Filler                     */

L10280:     FMT CH(25), CH(3), CH(6), BI(4), 3*PD(14,4), CH(96), CH(6),  ~
                2*CH(9), CH(40), CH(78)

            return clear
            end

        REM ENDOFPROGRAMENDOFPROGRAMENDOFPROGRAMENDOFPROGRAMENDOFPROGRAM
