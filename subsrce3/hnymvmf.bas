        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  H   H  N   N  Y   Y  M   M  V   V  M   M  FFFFF          *~
            *  H   H  NN  N  Y   Y  MM MM  V   V  MM MM  F              *~
            *  HHHHH  N N N   YYY   M M M  V   V  M M M  FFF            *~
            *  H   H  N  NN    Y    M   M   V V   M   M  F              *~
            *  H   H  N   N    Y    M   M    V    M   M  F              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYMVMF  - USER CONTROL OF WHAT IS RECORDED AS INVENTORY  *~
            *            MOVEMENT HISTORY.  YOU COULD EVEN MAINTAIN A   *~
            *            SEPARATE HISTORY FILE (OR FILES) IF YOU DON'T  *~
            *            LIKE WHAT IS IN HNYQUAN.  IF THE ARGUMENT LIST *~
            *            IS LACKING, MODIFY HNYPOST IN ONE (1) PLACE.   *~
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
            * 02/25/86 ! ORIGINAL                                 ! KAB *~
            * 02/11/87 ! Added Last Activity Months               ! HES *~
            * 08/23/88 ! Corrected period postings                ! JDH *~
            * 11/21/90 ! Corrected period postings for successive ! KAB *~
            *          ! CALLS from HNYPST2                       !     *~
            * 02/19/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 03/23/92 ! Added support for Transaction Code 'JR'. ! JDH *~
            * 09/17/93 ! Added support for Transaction Code 'IZ'. ! WPH *~
            * 07/08/97 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC
                 
            sub "HNYMVMF"  (part$,       /* PART CODE                  */~
                            store$,      /* STORE/WAREHOUSE            */~
                            lot$,        /* LOT                        */~
                            date$,       /* POSTING DATE               */~
                            type$,       /* POSTING TYPE               */~
                            userid$,     /* WHO TO BLAME               */~
                            qtyoh,       /* CHANGE TO ON HAND          */~
                            tcost,       /* TOTAL COST                 */~
                            cost(),      /* COST BREAKDOWN             */~
                            price,       /* PRICE (HNYPST2 CALLER)     */~
                            monthin%,    /* GL Period                  */~
                            #1)          /* SYSFILE2                   */

        dim                                                              ~
            cost(12),                    /* COST BREAKDOWN             */~
            dates$(17)8,                 /* Fiscal Calendar            */~
            date$6,                      /* POSTING DATE               */~
            f1%(32),                                                     ~
            fsclyr$4,                    /* Fiscal calendar year       */~
            lot$6,                       /* LOT                        */~
            month$(20)1,                 /* Last used month, by type   */~
            part$25,                     /* PART CODE                  */~
            readkey$100,                 /* G. P. READKEY              */~
            search%(2),                  /* Work Variable              */~
            store$3,                     /* STORE/WAREHOUSE            */~
	    tdate$8,                     /* Temp Date                  */~
            type$2,                      /* POSTING TYPE               */~
            userid$3,                    /* WHO TO BLAME               */~
            zero$208,                    /* NULL                       */~
            zero1$208                    /* NULL                       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************

            dim cms2v$50
REM         cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select # 2, "HNYHSTRF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  32                       

            f2% = 0
            if fs% > 0% then L09000
            call "OPENCHCK" (#2, fs%, f2%, 200%, " ")
              if fs% < 0% then L65000 /* Couldn't Open or Create */

L09000: REM *************************************************************~
            *                                                           *~
            *  UPDATE HISTORY                                           *~
            *                                                           *~
            *************************************************************

            if abs(qtyoh) < .01 then L65000

            factor = 1 : x% = 1% : month% = monthin%
            if month% > 17% then L65000
            if month% < 1%  then L65000
            if month% < 14% then L10000
               month% = month% - 13%
               x% = 14%

L10000: REM **************************************************************

            /* Transaction Type (HNYDETAL)  No. Proc/Usage  Status     */~
            /*  IA = Inventory Addition      1      P       Active     */~
            /*  IS = Inter-store movement    2      P       Active     */~
            /*  IC = Cycle count adjust.     3      P       Obs.       */~
            /*  IP = Physical adjustment     4      P       Active     */~
            /*  IW = Inventory Withdrawal    5      P       Active     */~
            /*  JB = Job Byproduct           6      P       Active     */~
            /*  JC = Job Completion          7      P       Active     */~
            /*  JK = Job Kitting             8      U       Active     */~
            /*  JT = Job Scrap               9      P       Active     */~
            /*  PR = PO/QC to Rework        10      P       Active     */~
            /*  PO = PO receipt             11      P       Active     */~
            /*  PQ = QC to On-Hand          12      P       Obs.?      */~
            /*  RT = A/R transaction        13      U       Active     */~
            /*  VT = A/P transaction        14      P       Active     */~
            /*  IQ = Inventory to Project   15      U       Active     */~
            /*  JR = To/From Rework Job     16      P       Active     */~
            /*  IZ = Part to Part Conversion17      P       Active     */~

            REM Get Numeric Offset For Transaction Type...
            REM Do NOT change the order of these!! You can add types to
            REM blank space at end of literal.
*                   1 2 3 4 5 6 7 8 9 A B C D E F1011
            search "IAISICIPIWJBJCJKJTPRPOPQRTVTIQJRIZ      " =          ~
                                        str(type$,,2) to search%() step 2
            type% = (search%(1)+1)/2
            if search%(1) = 0 then type% = 0%

            on type% goto hny_addition,   /* IA = Inventory Addition   */~
                          hny_addition,   /* IS = Inter-store movement */~
                          hny_addition,   /* IC = Cycle count adjust.  */~
                          hny_addition,   /* IP = Physical adjustment  */~
                          hny_addition,   /* IW = Inventory Withdrawal */~
                          hny_addition,   /* JB = Job Byproduct        */~
                          hny_addition,   /* JC = Job Completion       */~
                          hny_withdraw,   /* JK = Job Kitting          */~
                          hny_addition,   /* JT = Job Scrap            */~
                          hny_addition,   /* PR = PO/QC to Rework      */~
                          hny_addition,   /* PO = PO receipt           */~
                          hny_addition,   /* PQ = QC to On-Hand        */~
                          hny_withdraw,   /* RT = A/R transaction      */~
                          hny_addition,   /* VT = A/P transaction      */~
                          hny_withdraw,   /* IQ = Inventory to Project */~
                          hny_addition,   /* JR = To/From Rework Job   */~
                          hny_addition    /* IZ = Part to Part Conv.   */

            goto L65000

        hny_withdraw

            month% = month% + 13%
            factor = -1

        hny_addition
            spot% = 1% + (8%*(month%-1%))
            call "READ100" (#1, "FISCAL DATES", f1%(1))
            if f1%(1) = 0 then L65000
            get #1, using L11064, dates$()

            for x1% = 1% to 17%
	       tdate$ = dates$( x1% )
               call "DATEFMT" (tdate$, 0%, dates$( x1% ) )
            next x1%

L11064: FMT POS(23), 17*CH(08)
            u3% = 0%
            convert str( dates$( x% ), 1%, 6% ) to u3%, data goto noconv
noconv
	    put fsclyr$ using fsclyr_fmt, u3%
            readkey$ = str(part$,,25) & str(store$,,3) & fsclyr$
fsclyr_fmt: FMT BI(4)

L11090:     call "READ101" (#2, readkey$, f1%(2))
                if f1%(2) <> 0% then L11180

            init (hex(00)) zero$, zero1$, month$()
            write #2, using L11150, part$, store$, fsclyr$,       zero$,  ~
                                   zero1$, month$(), " "
L11150:     FMT CH(25), CH(3), CH(4), CH(208), CH(208), 20*CH(1), CH(32)
            goto L11090

L11180:     get #2, using L11190, zero$, zero1$, month$()
L11190:         FMT POS(33), CH(208), CH(208), 20*CH(1)
            get str(zero$, spot%, 8%) using L11210, quantity
L11210:         FMT PD(14,4)
            quantity = round(quantity + factor*qtyoh, 2)
            if quantity = 0 then  str(zero$, spot%, 8%) = all(hex(00))   ~
                     else put str(zero$, spot%, 8%) using L11210, quantity

            get str(zero1$, spot%, 8%) using L11270, cost
L11270:         FMT PD(14,4)
            cost = round(cost + (factor * qtyoh * tcost), 2)
            if cost = 0 then  str(zero1$, spot%, 8%) = all(hex(00))      ~
                     else put str(zero1$, spot%, 8%) using L11270, cost

            REM Set Respective Last Activity Month...
            if month% > 13% then month% = month% - 13%
            if val(month$(1),1) > 12% then month$() = all(hex(00))
            if type% > 0% and val(month$(type%),1) < month% then         ~
                                            month$(type%) = bin(month%,1)
            put #2, using L11190, zero$, zero1$, month$()
            rewrite #2

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
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end