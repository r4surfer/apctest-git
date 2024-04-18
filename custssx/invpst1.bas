        REM *************************************************************~
           *                                                           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            * INVPST1  - Posts quantity transactions to the INVQUAN     *~
            *            file only.  These may be Memo Type transactions*~
            *            only.  Changes to the on-hand position is not  *~
            *            allowed (see HNYPOST).                         *~
            *-----------------------------------------------------------*~
            *                 M O D I F I C A T I O N S                 *~
            *---WHEN---+-------------------WHAT-------------------+-WHO-*~
            * 02/09/06 ! Original for subpart                     ! CMG *~
            *************************************************************

        sub "INVPST1"(part$,             /* PART TO BE UPDATED         */~
/*PAR000*/            subp$,             /* Subpart                    */~
                      store$,            /* STORE CODE                 */~
                      lotno$,            /* LOT NUMBER                 */~
                      qtyoh,             /* QUANTITY + OR -            */~
                      qtybo,             /*  1 = POST TO ON HAND       */~
                      qtyoo,             /*  2 =         B'ORD         */~
                      qtycm,             /*  3 =         ON ORDER      */~
                      qtyqc,             /*  4 =         COMMITTED     */~
                                         /*  5 =         IN PROCESS    */~
                      #1,                /* UFB ADDRESS OF INVQUAN     */~
                      #2,                /* UFB ADDRESS OF INVMASTR    */~
                      #3,                /* UFB ADDRESS OF SYSFILE2    */~
                      f21%,              /* STATUS FLAG FOR HNYQUAN    */~
                      f22%,              /* STATUS FLAG FOR HNYMASTR   */~
                      f23%,              /* STATUS FLAG FOR SYSFILE2   */~
                      prtflag%,          /* WHETHER OR NOT TO PRINT AN */~
                                         /*  EXCEPTION REPORT WHEN A   */~
                                         /*  NEW HNYQUAN REC CREATED-  */~
                                         /*  1= PRINT, 0 = DON'T PRINT */~
                      returncode%)       /* ERROR RETURN FROM SUBROUTIN*/~
                                         /* 0  = RECORD POSTED         */~
                                         /* 99 = RECORD *NOT* POSTED   */~

            dim                                                          ~
                acct$(18)9,                                              ~
                blankdate$8,             /* blank unfmt date           */~
                cost(12),                                                ~
                cost$96,                                                 ~
                lot$16,                  /* LOT NUMBER                 */~
                lotno$16,                /* LOT NUMBER                 */~
                part$25,                 /* PART NUMBER TO POST TO     */~
                subp$20,                 /* subpart PAR000             */~
                partreadkey$100,         /* KEY FOR READING FILE       */~
                qty(4),                  /* QUANTITIES ON STORE FILE   */~
                store$3,                 /* STORE (WAREHOUSE) CODE     */~
                binloc$8                 /* BIN LOCATION               */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 02/09/06 Year 2000 Compliancy            "
        REM *************************************************************

            blankdate$ = " "
            call "DATUNFMT" (blankdate$)

            returncode% = 99%            /* Assume Trouble */
            REM CHECK INCOMING DATA
                if abs(qtyoh) > .0001 then end
                if abs(qtybo) > .0001 then L02090
                if abs(qtyoo) > .0001 then L02090
                if abs(qtycm) > .0001 then L02090
                if abs(qtyqc) > .0001 then L02090
                end
L02090:     returncode%=98%

            if open2% = 0% then call "OPENCHCK"(#2, open2%, f22%, 0%, " ")
                if open2% < 1% then end
            if open3% = 0% then call "OPENCHCK"(#3, open3%, f23%, 0%, " ")
                if open3% < 1% then end

            returncode%=97%

        REM *** Check to See if we can save a read ***
REM         if fs(#2) <"10" and part$ <> " " and part$ = key(#2) then L02310
* PAR000
            init(" ") partreadkey$
            str(partreadkey$, 1,25) = part$
            str(partreadkey$,26,20) = subp$

            call "READ100" (#2, partreadkey$, f1%)
                if f1% = 0% then end

L02310: REM *** Open / Create Files if Needed ***
            returncode% = 94%

        REM Open HNYQUAN File
            if open1% =0% then call "OPENCHCK" (#1,open1%,f21%,300%," ")

        rem**************************************************************~
           *               p o s t    h n y q u a n                     *~
           *                                                            *~
           **************************************************************

            lot$ = lotno$
* PAR000
            partreadkey$ = str(part$,,25) & str(subp$,,20) &           ~
                                           str(store$,,3) & str(lot$)

            call "READ101" (#1, partreadkey$, f1%)
                if f1% = 1% then L04140
                   gosub create_invquan          /* PAR000 */
                      call "READ101" (#1, partreadkey$, f1%)

L04140: REM *** Extract Pertinent Information ***
            get #1, using L04160, qty()
L04160:         FMT POS(97), 4*PD(14,4)      /* PAR000 */

        REM *** Update Desired Quantity ***

            qty(1) = max(round(qty(1) + qtybo, 2%), 0)
            qty(2) = max(round(qty(2) + qtyoo, 2%), 0)
            qty(3) = max(round(qty(3) + qtycm, 2%), 0)
            qty(4) = max(round(qty(4) + qtyqc, 2%), 0)

            put #1, using L04260, qty()
L04260:         FMT POS(97), 4*PD(14,4)          /* PAR000 */

            rewrite #1

            returncode%=0
            end


        rem**************************************************************~
           * create a hnyquan record since it is not already there.     *~
           * set the default values you want to employ here.  notice    *~
           * that the system will print an exception report when a new  *~
           * record is created only if the arguement 'PRTFLAG' = 1.     *~
           **************************************************************

        create_invquan

        REM *** Set Default Information Here ***
           get #2, using L05110 , cat$, binloc$, costtype$, acct$()
L05110:         FMT POS(1100), CH(4), POS(175), CH(8), POS(327), CH(1),    ~
                  /* PAR000 */                    POS(355), 18*CH(9)

                if costtype$=" " then costtype$="R"

        REM GL ACCOUNT SUBROUTINE CALL GOES HERE
* Not Used
           call "GLHNYQN" (part$, store$, lot$, cat$, costtype$, acct$())

           mat qty  = zer
           mat cost = zer
           tcost = 0

           if pos("SFT"=costtype$) = 0% then L05280
              call "STCCOSTS" (part$, " ", #3, 2%, tcost, cost())
L05280:    call "PACKZERO" (cost(), cost$)

           write #1, using L05670 ,                                        ~
            lot$,           /* Lot Number                              */~
            part$,          /* Part Number                             */~
            subp$,          /* Subpart PAR000                          */~
            store$,         /* Store or Warehouse Code                 */~
            lot$,           /* Lot Number                              */~
            binloc$,        /* Stock location                          */~
            0,              /* quantity on-hand, back-ordered,         */~
            qty(),          /*          on order, committed,           */~
            0,              /*          in process, pending            */~
            tcost,          /* Total Cost                              */~
            cost$,          /* Cost fields                             */~
            "0",            /* minimum on-hand                         */~
            "0",            /* maximum on-hand                         */~
            acct$(),        /* General Ledger Account Numbers          */~
            costtype$,      /* inventory costing method                */~
            blankdate$,     /* Date 'something' expires                */~
            1,              /* Lot Potency Factor                      */~
            hex(ffffffff),  /* Internal ID to text in TXTFILE.         */~
            " ",            /* Variable Fields Data Area               */~
            " ",            /* User who changed Expiration Date        */~
            blankdate$,     /* Date Expiration Date Changed            */~
            " ",            /* User who changed Potency Factor         */~
            blankdate$,     /* Date Potency Factor Changed             */~
            " ",            /* Filler (Internal, unused space)         */~
            eod goto L05650

L05650:    return

L05670: FMT                 /* FILE: INVQUAN                           */~
            CH(16),         /* Lot Number                              */~
            CH(25),         /* Part Number                             */~
            CH(20),         /* Subpart Number         PAR000           */~
            CH(3),          /* Store or Warehouse Code                 */~
            CH(16),         /* Lot Number                              */~
            CH(8),          /* Stock location                          */~
            6*PD(14,4),     /* quantities                              */~
            PD(14,4),       /* cost total                              */~
            CH(96),         /* cost fields                             */~
            2*CH(10),       /* minimum, maximum on-hand                */~
            18*CH(9),       /* General Ledger Account Numbers          */~
            CH(1),          /* inventory costing method                */~
            CH(6),          /* Date 'something' expires                */~
            PD(14,4),       /* Lot Potency Factor                      */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(200),        /* Variable Fields Data Area               */~
            CH(3),          /* User who changed Expiration Date        */~
            CH(6),          /* Date Expiration Date Changed            */~
            CH(3),          /* User who changed Potency Factor         */~
            CH(6),          /* Date Potency Factor Changed             */~
            CH(11)          /* Filler (Internal, unused space)         */~

        REM ENDOFPROGRAMENDOFPROGRAMENDOFPROGRAMENDOFPROGRAMENDOFPROGRAM



