        REM *************************************************************~
            *                                                           *~
            *   H   H  N   N  Y   Y  PPPP    SSS   TTTTT   11           *~
            *   H   H  NN  N  Y   Y  P   P  S        T    1 1           *~
            *   HHHHH  N N N   YYY   PPPP    SSS     T      1           *~
            *   H   H  N  NN    Y    P          S    T      1           *~
            *   H   H  N   N    Y    P       SSS     T    11111         *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            * HNYPST1  - Posts quantity transactions to the HNYQUAN     *~
            *            file only.  These may be Memo Type transactions*~
            *            only.  Changes to the on-hand position is not  *~
            *            allowed (see HNYPOST).                         *~
            *-----------------------------------------------------------*~
            *                 M O D I F I C A T I O N S                 *~
            *---WHEN---+-------------------WHAT-------------------+-WHO-*~
            * 08/02/83 ! MEMO ONLY VERSION of HNYPOST             ! KEN *~
            * 01/07/86 ! No longer writes ABC class to HNYQUAN    ! LDJ *~
            *          !   record, Bin location expanded in same, !     *~
            *          !   reorder quantity in same now filler,   !     *~
            *          !   next post seq # in same now filler(PD).!     *~
            * 05/13/87 ! Increased Cost Fields                    ! KEN *~
            * 12/17/91 ! PRR 12114 Changed the request flag for   ! SID *~
            *          !   the call to STCCOSTS from 1% to 2% to  !     *~
            *          !   include the folded cost when writing to!     *~
            *          !   HNYQUAN file.                          !     *~
            * 11/03/97 ! Write Expiration Date as Blankdate.      ! JDH *~
            *************************************************************

        sub "HNYPST1"(part$,             /* PART TO BE UPDATED         */~
                      store$,            /* STORE CODE                 */~
                      lotno$,            /* LOT NUMBER                 */~
                      qtyoh,             /* QUANTITY + OR -            */~
                      qtybo,             /*  1 = POST TO ON HAND       */~
                      qtyoo,             /*  2 =         B'ORD         */~
                      qtycm,             /*  3 =         ON ORDER      */~
                      qtyqc,             /*  4 =         COMMITTED     */~
                                         /*  5 =         IN PROCESS    */~
                      #1,                /* UFB ADDRESS OF HNYQUAN     */~
                      #2,                /* UFB ADDRESS OF HNYMASTR    */~
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
                partreadkey$50,          /* KEY FOR READING FILE       */~
                qty(4),                  /* QUANTITIES ON STORE FILE   */~
                store$3,                 /* STORE (WAREHOUSE) CODE     */~
                binloc$8                 /* BIN LOCATION               */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            if fs(#2) <"10" and part$ <> " " and part$ = key(#2) then L02310

            call "READ100" (#2, part$, f1%)
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
            partreadkey$ = str(part$,,25) & str(store$,,3) & str(lot$)

            call "READ101" (#1, partreadkey$, f1%)
                if f1% = 1% then L04140
                   gosub create_hnyquan
                      call "READ101" (#1, partreadkey$, f1%)

L04140: REM *** Extract Pertinent Information ***
            get #1, using L04160, qty()
L04160:         FMT POS(77), 4*PD(14,4)

        REM *** Update Desired Quantity ***

            qty(1) = max(round(qty(1) + qtybo, 2%), 0)
            qty(2) = max(round(qty(2) + qtyoo, 2%), 0)
            qty(3) = max(round(qty(3) + qtycm, 2%), 0)
            qty(4) = max(round(qty(4) + qtyqc, 2%), 0)

            put #1, using L04260, qty()
L04260:         FMT POS(77), 4*PD(14,4)

            rewrite #1

            returncode%=0
            end


        rem**************************************************************~
           * create a hnyquan record since it is not already there.     *~
           * set the default values you want to employ here.  notice    *~
           * that the system will print an exception report when a new  *~
           * record is created only if the arguement 'PRTFLAG' = 1.     *~
           **************************************************************

        create_hnyquan

        REM *** Set Default Information Here ***
           get #2, using L05110 , cat$, binloc$, costtype$, acct$()
L05110:         FMT POS(90), CH(4), POS(155), CH(8), POS(307), CH(1),    ~
                                                  POS(335), 18*CH(9)

                if costtype$=" " then costtype$="R"

        REM GL ACCOUNT SUBROUTINE CALL GOES HERE

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

L05670: FMT                 /* FILE: HNYQUAN                           */~
            CH(16),         /* Lot Number                              */~
            CH(25),         /* Part Number                             */~
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
