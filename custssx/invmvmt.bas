        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *-----------------------------------------------------------*~
            * INVMVMT  - USER CONTROL OF WHAT IS RECORDED AS INVENTORY  *~
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
            * 02/03/06 ! ORIGINAL                                 ! CMG *~
           CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "INVMVMT"  (part$,       /* PART CODE                  */~
/* PAR000  */               subp$,       /* Subpart number             */~
                            store$,      /* STORE/WAREHOUSE            */~
                            lot$,        /* LOT                        */~
                            date$,       /* POSTING DATE               */~
                            type$,       /* POSTING TYPE               */~
                            userid$,     /* WHO TO BLAME               */~
                            qtyoh,       /* CHANGE TO ON HAND          */~
                            tcost,       /* TOTAL COST                 */~
                            cost(),      /* COST BREAKDOWN             */~
                            price)       /* PRICE (HNYPST2 CALLER)     */~

        dim                                                              ~
	    ccyy$2,                      /* Binary 2  CCYY             */~
	    ccyymmdd$8,                  /* ccyymmdd date              */~
            cost(12),                    /* COST BREAKDOWN             */~
            date$6,                      /* POSTING DATE               */~
            lot$6,                       /* LOT                        */~
            month$(20)1,                 /* Last used month, by type   */~
            part$25,                     /* PART CODE                  */~
            readkey$100,                 /* G. P. READKEY              */~
/* PAR000*/ subp$20,                     /* Subpart Number             */~
            search%(2),                  /* Work Variable              */~
            store$3,                     /* STORE/WAREHOUSE            */~
	    tdate$8,                     /* Temp. Date                 */~
            type$2,                      /* POSTING TYPE               */~
            userid$3,                    /* WHO TO BLAME               */~
            zero$192,                    /* NULL                       */~
            zero1$192                    /* NULL                       */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto L01980
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

L01980: REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select # 1, "INVHSTRY",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  50 

            if fs% > 0% then L09000
            call "OPENCHCK" (#1, fs%, f1%, 200%, " ")
               if fs% < 0% then L65000 /* Couldn't Open or Create */

L09000: REM *************************************************************~
            *                                                           *~
            *  UPDATE HISTORY                                           *~
            *                                                           *~
            *************************************************************

	    tdate$    = date$
	    ccyymmdd$ = " "
	    call "DATEFMT" (tdate$, 0%, ccyymmdd$)

            if abs(qtyoh) < .01 then L65000

            month% = 0% : factor = 1
            convert str(ccyymmdd$,5%,2%) to month%, data goto L65000
            if month% > 12% then L65000
            if month% < 1%  then L65000

        REM **************************************************************

            /* Transaction Type (INVDETAL)  No. Proc/Usage  Status     */~
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

            month% = month% + 12%
            factor = -1

        hny_addition
            spot% = 1% + (8%*(month%-1%))
	    convert str(ccyymmdd$,1%,4%) to ccyy%
            put ccyy$ using ccyy_fmt, ccyy%
ccyy_fmt:   FMT BI(2)
            readkey$ = str(part$,,25) & str(subp$,,20) & str(store$,,3) & ccyy$

L11090:     call "READ101" (#1, readkey$, f1%)
                if f1% <> 0% then L11180

            init (hex(00)) zero$, zero1$, month$()
            convert str(ccyymmdd$, 1%, 4%) to date%
            write #1, using L11150, part$, subp$, store$, date%, zero$,  ~
                                   zero1$, month$(), " "
L11150:     FMT CH(25), CH(20), CH(3), BI(2), CH(192), CH(192), 20*CH(1), CH(16)
            goto L11090

L11180:     get #1, using L11190, zero$, zero1$, month$()
L11190:         FMT POS(51), CH(192), CH(192), 20*CH(1)
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
            if month% > 12% then month% = month% - 12%
            if val(month$(1),1) > 12% then month$() = all(hex(00))
            if type% > 0% and val(month$(type%),1) < month% then         ~
                                            month$(type%) = bin(month%,1)
            put #1, using L11190, zero$, zero1$, month$()
            rewrite #1

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

            end


