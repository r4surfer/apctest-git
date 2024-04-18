        REM *************************************************************~
            *                                                           *~
            * INVPST2  - Updates inventory files to reflect changes     *~
            *            to onhand, on order, backordered, committed,   *~
            *            and in process (QC).  Handles several          *~
            *            different costing methods, and will create     *~
            *            the files it needs to update.                  *~
            *                                                           *~
            *   Handles various types of costing as selected by user -  *~
            *        A - Average costing, with a kinky. If the inventory*~
            *            is overdrawn, and an addition is made which re-*~
            *            sults in a zero quantity a correction posting  *~
            *            is made to INVASSETS and INVADJ. One pool rec. *~
            *            The correction is to allow the INVASSET acct to*~
            *            keep in balance with the Physical Inventory.   *~
            *        B - Modified Average.  Acts like 'L' when recover- *~
            *            from Negative On-Hand.                         *~
            *        S - Standard costing with LIFO. Inventory additions*~
            *            are always at current standard (better be sure *~
            *            they are set) with variance posting.  Former   *~
            *            Pool records retain their recorded value.      *~
            *        T - As above with FIFO.                            *~
            *        F - Fixed Standard, or manual costing. One pool    *~
            *            record, always reflecting present standard cost*~
            *            updated by any movement and posting variance   *~
            *            corrections.                                   *~
            *        R - Actual cost via the pool records. If an over-  *~
            *            draw occurs any adjustment will post to the    *~
            *            accounts which caused the shortage, if possible*~
            *        P - As above with FIFO.                            *~
            *        X - Actual Costing, but with overdraw adjustments  *~
            *            posting to the adjustment account.             *~
            *        Y - As above with FIFO.                            *~
            *        L - Last cost (for whomever wants it). One pool    *~
            *            record continually updated to show last entered*~
            *            cost, with adjustments to INVADJ.              *~
            *        M - Manual costing with costs taken as is from the *~
            *            INVQUAN record.                                *~
            * Note that any of the automatic corrections features can   *~
            * be bypassed by setting the variance accounts, adjustment  *~
            * account and INV Asset account equal, in which case Radio  *~
            * Shack can probably do just as good a job at inventory     *~
            * control!                                                  *~
            *************************************************************~
            *                M O D I F I C A T I O N S                  *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/24/06 ! ORIGINAL PAR000  CR347                   ! CMG *~
            *PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED*

        sub "INVPST2"(part$,             /* Part to be updated         */~
/* PAR000  */         subp$,             /* Subpart number             */~
                      store$,            /* Store code                 */~
                      lotno$,            /* Lot number                 */~
                      qtyoh,             /* Quantity + or -            */~
                      qtybo,             /*  1 = Post to On Hand       */~
                      qtyoo,             /*  2 =         Backordered   */~
                      qtycm,             /*  3 =         On Order      */~
                      qtyqc,             /*  4 =         Committed     */~
                                         /*  5 =         In Process    */~
                      cst(),             /* Unit Costs                 */~
                      tcst,              /* Total Cost/Unit            */~
                      price,             /* Unit Price                 */~
                      extension,         /* Extension of Price         */~
                      date$,             /* Module Date (YYMMDD)       */~
                      type$,             /* Transaction Type (INVDETAL)*/~
                                         /*  IA = Inventory Addition   */~
                                         /*  IS = Inter-store movement */~
                                         /*  IC = Cycle count adjust.  */~
                                         /*  IP = Physical adjustment  */~
                                         /*  IW = Inventory Withdrawal */~
                                         /*  JC = Job Completion       */~
                                         /*  JK = Job Kitting          */~
                                         /*  PR = PO/QC to Rework      */~
                                         /*  PO = PO receipt           */~
                                         /*  PQ = QC to On-Hand        */~
                                         /*  RT = A/R transaction      */~
                                         /*  VT = A/P transaction      */~
                      text$,             /* Reference Text String      */~
                      acct1in$,          /* INV Asset Account          */~
                      acct2in$,          /* Offset Account             */~
                                         /* If blank, will be returned */~
                                         /* via GL1%, GL2%             */~
                      gl1%,              /* 1st Account Level          */~
                      gl2%,              /* 2nd Account Level          */~
                                         /* 1 = SRCE Account (WIP),    */~
                                         /* 2 = SRCE Account (Pur),    */~
                                         /* 3 = INV  Account,          */~
                                         /* 4 = COGS Account,          */~
                                         /* 5 = SALE Account,          */~
                                         /* 6 = ADJ  Account,          */~
                                         /* 7 - 18 = STC Variances     */~
                                         /* DEF, GL1%=2, GL2%=1 OR 3   */~
                                         /*                PROC    WDWL*/~
                  glmod$,                /* G/L Module Posting Inventor*/~
                  jnlid$,                /* Journal ID                 */~
                  pstseq%,               /* Posting Sequence No        */~
                  gltextpassed$,         /* G/L Text passed from Progrm*/~
                  userid$,               /* Who did the dirty deed     */~
                      #1,                /* UFB Address of INVQUAN     */~
                      #2,                /* UFB Address of INVDETAL    */~
                      #3,                /* UFB Address of SYSFILE2    */~
                      #4,                /* UFB Address of INVPOOLS    */~
                      #5,                /* UFB Address of INVMASTR    */~
                      #6,                /* UFB Address of PIPMASTR    */~
                      #7,                /* UFB Address of SFCUM2      */~
                      #8,                /* UFB Address of GLMAIN      */~
                      #9,                /* UFB Address of GLDETAIL    */~
                      #10,               /* UFB Address of HNYADJPF    */~
                      cstflag%,          /* Costing, if a 'Procurement'*/~
                                         /*  and cost is not available,*/~
                                         /*  then use 'Best Guess'  ?  */~
                                         /*  0% = Don't, <> 0% = Do It.*/~
                                         /* Costs are returned         */~
                                         /* as a bonus. If passed as a */~
                                         /* variable will return THIS% */~
                                         /* (Month Posted).            */~
                      returncode%)       /* Error Code Returned;       */~
                                         /* <=0 Means Record Posted    */~
                                         /* BONUS -(PLANDAY EFFECTED)  */~
                                         /* 99 = Record *NOT* Posted   */~
                                         /* 98 = INVMASTR not found    */~
                                         /* 97 = Part not on file      */~
                                         /* 96 = Open month error      */~
                                         /* 95 = Plan Date Error       */~
                                         /* 94 = File Opening Module   */~
                                         /* 93 = Invalid Cost Type     */~
                                         /* 08 = Invalid Plan Date Data*/~
                                         /* 04 = Invalid ""   "" Result*/~

            dim                                                          ~
                acct1$9,                 /* INV Asset Account          */~
                acct2$9,                 /* Offset Account             */~
                acct1in$9,               /* INV Asset Account          */~
                acct2in$9,               /* Offset Account             */~
                acct$(18)9,              /* G/L Accounts from INVQUAN  */~
                blankdate$8,             /* blank unfmt date           */~
                binloc$8,                /* Bin Location               */~
                cat$4,                   /* Category Code              */~
                cal$1,                   /* History Type Flag          */~
                cost(12),                /* Matl,Labr,OH Cost Averages */~
                cst(12),                 /* Matl,Labr,OH Cost Averages */~
                cost$96,                 /* PACKZERO Cost String       */~
                costtype$1,              /* Costing Method             */~
                date$6,                  /* Module Posting Date        */~
                exdate$6,                /* Date 'something' expires   */~
                f1%(32),                                                 ~
                gltext$100,              /* G/L Posting Text           */~
                gltextpassed$100,        /* G/L Text passed from Progrm*/~
                glmod$2,                 /* G/L Module Posting to INV  */~
                holdcst(12),             /* Work Variable              */~
                jnlid$3,                 /* Journal ID                 */~
                lastdate$6,              /* Last Module Posting Date   */~
                lot$16,                  /* Lot Number                 */~
                lotno$,                  /* LOT NUMBER                 */~
                mgtrpt_on$1,             /* Is Management Reporting on?*/~
                oldacct1$9,              /* Inventory Asset Account    */~
                oldacct2$9,              /* Inventory Adjustment Acct  */~
                part$25,                 /* Part Number to Post to     */~
                partreadkey$50,          /* Key for Reading file       */~
                pcst(12),                /* Pool Cost Adjustment       */~
                pcost(12),               /* Pool Cost                  */~
                pwork(12),               /* Pool Cost Work             */~
                qty(6),                  /* Quantities on Store File   */~
                std(12),                 /* Standard Unit Costs        */~
                readkey$58,              /* INVPOOL Readkey            */~
                store$3,                 /* Store (Warehouse) Code     */~
/* PAR000 */    subp$20,                 /* Subpart Number             */~
                sysdate$6,               /* System Date                */~
                text$50,                 /* Free Text Field            */~
                tcost(1),                /* Total Cost                 */~
                trndate$6,               /* Old Transaction Date       */~
                twork(1,12),             /* Totals Work Array          */~
                type$2                   /* Sales, Backlog, VBK, etc   */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            blankdate$ = " "
            call "DATUNFMT" (blankdate$)

            if been_here_before% = 1% then L02000
                been_here_before% = 1%
                /* See if Management Reporting is on */
                call "READ100" (#3, "SWITCHS.GL", f1%(3))
                if f1%(3) = 1% then get #3 using L01994, mgtrpt_on$
L01994:             FMT POS(59), CH(1)

L02000:     returncode% = 99%            /* Assume Trouble */
            onhand% = 0%:lot$=lotno$:acct1$=acct1in$:acct2$=acct2in$
            gltext$ = gltextpassed$
            REM CHECK INCOMING DATA
                if abs(qtyoh) > .0001 then L02100
                if abs(qtybo) > .0001 then L02110
                if abs(qtyoo) > .0001 then L02110
                if abs(qtycm) > .0001 then L02110
                if abs(qtyqc) > .0001 then L02110
                end
L02100:     onhand% = 1%
L02110:     returncode%=98%

REM in OPENCHECK stmt if file sucessfully opened then open5% with be 1% 
            if open5% = 0% then call "OPENCHCK" (#5, open5%, 0%, 0%, " ")
                if open5% < 1% then end
            if open3% = 0% then call "OPENCHCK" (#3, open3%, 0%, 0%, " ")
                if open3% < 1% then end

            if sysdate$ = " " or sysdate$ = blankdate$ then sysdate$ = date

            returncode%=97%
        REM *** Check to See if we can save a read ***
REM  PAR000 - begin
REM  take out the fs(x) because fs is NOT reference anywhere else in this subroutine

REM         if fs(#5) <"10" and part$ <> " " and part$ = key(#5) then L02302
            init(" ") partreadkey$
            partreadkey$ = part$
            str(partreadkey$, 26, 20%) = subp$
REM  PAR000 - end

            call "READ100" (#5, partreadkey$, f1%)
                if f1% = 0% then end
L02302:     get #5, using L02304, ptype$
L02304:         FMT POS(200), CH(3)            /* PAR000 */
            call "STCCOSTS" (part$, " ", #3, 2%, std, std())
            mdmc = -1.0
            if mgtrpt_on$ = "Y" then call "MDMCCOST" (part$, " ", #3,    ~
                                                                    mdmc)

            returncode% = 96%

        REM *** Get Which Month to Post if On Hand is Changing ***
        REM *** But First check to see if post date = last one ***

            if onhand% = 0% then L02580
            if date$ <> " " and date$ <> blankdate$ and ~
               date$ = lastdate$ then L02580
                lastdate$ = " "
                call "DATUNFMT" (lastdate$)
                day%, this% = 0%
                call "WHICHPER" (#3, date$, this%)
                     if this% = 0 then L02560

                call "PIPINDEX" (#3, date$, day%, returncode%)
                     if returncode% <> 0% then L02560
                returncode% = 95%
                   lastdate$ = date$
                   goto L02580

                REM *** Error in System Dates ***
L02560:         end

L02580: REM *** Open / Create Files if Needed ***
            returncode% = 94%

        REM Open INVQUAN File
            if open1% =0% then call "OPENCHCK" (#1, open1%, 0%, 300%," ")

            if onhand% = 0% then L02710  /* NOT 'TIL WE NEED THEM */

        REM Open INVDETAL File
            if open2% =0% then call "OPENCHCK" (#2, open2%, 0%, 300%," ")

        REM Open INVPOOLS File
            if open4% =0% then call "OPENCHCK" (#4, open4%, 0%, 300%," ")


L02710:     returncode%=93%

        REM Determine Type Of History They Are Keeping...
            if was_i_here% <> 0% then L03000
            was_i_here% = 1%
            call "READ100" (#3, "SWITCHS.HNY", f1%(3))
               if f1%(3) = 1 then get #3 using L02780, cal$
L02780:     FMT POS(95), CH(1)

L03000: REM *************************************************************~
           *   M a i n   C o n t r o l   S e c t i o n                  *~
            *************************************************************
* PAR000
            partreadkey$ = part$
            str(partreadkey$, 26, 20%) = subp$
            str(partreadkey$, 46%, 3%) = store$
            str(partreadkey$, 49%) = lot$

L03090:     call "READ100" (#1, partreadkey$, f1%)
                if f1% <> 0% then L03110
                   gosub create_invquan : goto L03090
L03110:     get #1, using L03120, qty(1),cost(),acct$(),costtype$,exdate$
L03120:       FMT POS(89), PD(14,4), POS(145), 12*PD(14,4), POS(261),    ~
                  18*CH(9), CH(1), CH(6)          /* PAR000 */

                if costtype$ = " " then costtype$="R"
                if pos("RXABSFLMPYT"=costtype$) = 0% then end

*          Here we make concessions for 'reasonable' averages.
                if costtype$ <> "B" then L03180
                   if qtyoh <= 0 then L03180
                      if qty(1) < 0 then costtype$ = "L"
*          To Do, or Not To Do?  Ask a Beancounter?

L03180:         if acct1$ <> " " then L03210
                if gl1% > 0% and gl1% < 18% then acct1$ = acct$(gl1%)
                if acct1$ = " " then acct1$ = acct$(3%)  /* INV ASSET */

L03210:         if acct2$ <> " " then L03260
                if gl2% > 0% and gl2% < 18% then acct2$ = acct$(gl2%)
                          /* DEFAULT TO SOURCE OR COGS */
                if acct2$ <> " " then L03260

                if qtyoh >= 0 then L03233
                   acct2$ = acct$(4%)
                   goto L03260

L03233:            if ptype$ < "500" then acct2$ = acct$(1%)
                   if ptype$ = "000" then acct2$ = acct$(2%)
                   if acct2$ = " "   then acct2$ = acct$(2%)

L03260:     acct1in$ = acct1$ : acct2in$ = acct2$

            if onhand% = 0% then L03350

               gosub post_invpools   /* PAR000 */
               gosub post_invquan    /* PAR000 */
               if ptype$ = "000" then L03300
               if ptype$ < "200" then L03320
L03300:        if str(store$,,1) > "9" or str(store$,,1) < "0" then L03320
              REM  gosub post_pipmastr        /* PAR000 */
L03320:        gosub post_invdetal
            goto L03370

L03350:        mat twork = con : mat tcost = twork * cst
               tcst = round(tcost(1), 4)
               gosub post_invquan
L03370:     returncode% = -day%
            cstflag%=this%
            end

        rem**************************************************************~
           *               p o s t    h n y q u a n                     *~
           *                                                            *~
           **************************************************************

        post_invquan

L04070:     call "READ101" (#1, partreadkey$, f1%)
                if f1% = 1% then L04130
                   gosub create_invquan  /* NOT LIKELY */
                      goto L04070

L04130: REM *** Extract Pertinent Information ***
* PAR000
            get #1, using L04180, qty(), tcost(), cost()

L04180:     FMT POS(89), 6*PD(14,4), PD(14,4), 12*PD(14,4)

        REM *** Now Update INVQUAN Accumulating Costs for Withdrawal**
            if onhand% = 0% then L04540
                tqty=round(qty(1)+qtyoh,2)
                if pos("FL"=costtype$)=0% then L04270
                   mat cost = std
                   goto L04316
L04270:         if abs(tqty) < .00001 then L04316
                   mat cost  = (qty(1)) * cost
                   mat pwork = (qtyoh)  * cst
                   mat cost  = cost     + pwork
                   mat cost  = cost     + pcst
                   mat cost  = (1/tqty) * cost

L04316:     call "PACKZERO" (cost(), cost$) /*Rounds buckets before Sum*/
            qty(1) =  tqty
            mat twork = con : mat tcost = twork * cost
            tcost(1) = round(tcost(1),4)
            if qtyoh < 0 then qty(6) = max(0, qty(6) + qtyoh)

L04540: REM *** Update Desired Quantity ***

            qty(2) =max(round(qty(2) + qtybo,2%), 0)
            qty(3) =max(round(qty(3) + qtyoo,2%), 0)
            qty(4) =max(round(qty(4) + qtycm,2%), 0)
            qty(5) =max(round(qty(5) + qtyqc,2%), 0)
            call "PACKZERO" (cost(), cost$)
* PAR000
            put #1, using L04620, qty(), tcost(), cost$
L04620:         FMT POS(89), 6*PD(14,4), PD(14,4), CH(96)

            rewrite #1

            if onhand% = 0% then return
* PAR000 do not update pip information
REM            call "HNYEXSUB" (lot$, store$, part$, exdate$, qty(1))

        REM CUSTOM MOVEMENT HISTORY CALL AFTER THE REWRITE!!!!

* !!! still do do need to create custom movement routines
            if cal$ = " " then return
            if cal$ = "F" then L04792   /* Fiscal?*/

            call "INVMVMT" (part$,       /* PART CODE                  */~
/*PAR000*/                  subp$,       /* Subpart                    */~
                            store$,      /* STORE/WAREHOUSE            */~
                            lot$,        /* LOT                        */~
                            date$,       /* POSTING DATE               */~
                            type$,       /* POSTING TYPE               */~
                            userid$,     /* WHO TO BLAME               */~
                            qtyoh,       /* CHANGE TO ON HAND          */~
                            tcst,        /* TOTAL COST                 */~
                            cst(),       /* COST BREAKDOWN             */~
                            price)       /* PRICE (HNYPST2 CALLER)     */~

            if cal$ = "G" then return /* Gregs? */
L04792:     call "INVMVMF"  (part$,      /* PART CODE                  */~
/*PAR000*/                   subp$,      /* Subpart                    */~
                             store$,     /* STORE/WAREHOUSE            */~
                             lot$,       /* LOT                        */~
                             date$,      /* POSTING DATE               */~
                             type$,      /* POSTING TYPE               */~
                             userid$,    /* WHO TO BLAME               */~
                             qtyoh,      /* CHANGE TO ON HAND          */~
                             tcst,       /* TOTAL COST                 */~
                             cst(),      /* COST BREAKDOWN             */~
                             price,      /* PRICE (HNYPST2 CALLE)      */~
                             this%,      /* GL Period                  */~
                             #3)         /* SYSFILE2                   */
            return

        rem**************************************************************~
           * create a invquan record since it is not already there.     *~
           * set the default values you want to employ here.            *~
           **************************************************************

        create_invquan

        REM *** Set Default Information Here ***
* PAR000 INVMASTR
           get #5, using L05090 , cat$, binloc$, costtype$, acct$()
L05090:         FMT POS(110), CH(4), POS(175), CH(8), POS(327), CH(1),    ~
                                                  POS(355), 18*CH(9)

                if costtype$=" " then costtype$="R"

        REM GL ACCOUNT SUBROUTINE CALL GOES HERE
* PAR000 note is subroutine was turned off anyway; 
* at beginning of routine it directly gotos end
REM           call "GLHNYQN" (part$, store$, lot$, cat$, costtype$, acct$())

           mat qty   = zer
           mat cost  = zer
           mat tcost = zer

           if pos("SFT"=costtype$) = 0% then L05240
              call "STCCOSTS" (part$, " ", #3, 2%, tcost(1), cost())
L05240:    call "PACKZERO" (cost(), cost$)

           write #1, using L05500 ,                                        ~
            lot$,           /* Lot Number                              */~
            part$,          /* Part Number                             */~
/*PAR000*/  subp$,          /* Sub Part number                         */~
            store$,         /* Store or Warehouse Code                 */~
            lot$,           /* Lot Number                              */~
            binloc$,        /* Stock location                          */~
            qty(),          /* quantity on-hand, back-ordered,         */~
                            /*          on order, committed,           */~
                            /*          in process, pending            */~
            tcost(1),       /* Total Cost                              */~
            cost$,          /* Cost fields                             */~
            "0",            /* minimum on-hand                         */~
            "0",            /* maximum on-hand                         */~
            acct$(),        /* General Ledger Account Numbers          */~
            costtype$,      /* inventory costing method                */~
            blankdate$,     /* Date 'something' expires                */~
            1,              /* Lot Potency Factor                      */~
            hex(ffffffff),  /* Internal ID to text in TXTFILE.         */~
            " ",            /* Variable Fields Data Area               */~
            " ",            /* Filler (Internal, unused space)         */~
            eod goto L05480

L05480:    return

L05500: FMT                 /* FILE: INVQUAN                           */~
            CH(16),         /* Lot Number                              */~
            CH(25),         /* Part Number                             */~
/*PAR000*/  CH(20),         /* Sub Part Number                         */~
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
/*PAR000*/  CH(109)         /* Filler (Internal, unused space)         */~

        rem**************************************************************~
           *         p o s t   p i p m a s t r                          *~
           *                                                            *~
           **************************************************************
REM        post_pipmastr
REM            call "READ101" (#6, part$, f1%)
REM                  if f1% = 0% then return
REM                get #6, using  L06080 , pipoh
REM L06080             FMT POS(1987), PD(14,4)
REM                pipoh = round(pipoh + qtyoh, 2%)
REM                put #6, using  L06080 , pipoh
REM                rewrite #6
* PAR000 so can take out file channel #7
REM            call "PIPFLAGS" (part$, day% , day% , qtyoh, #6, #7)
            return

        rem**************************************************************~
           *         p o s t   h n y d e t a l                          *~
           *                                                            *~
           **************************************************************

        post_invdetal                   /* INVDETAL   */

            price     = round(price, 4%)
            extension = round(extension, 4%)
L07110:     call "POSTDTTM" addr(date$, str(seqnr$,2,7))
            str(seqnr$,1,1) = "C"
*PAR000
            write #2, using L07220 , part$, subp$, store$, lot$, seqnr$, ~
                                   date$,   ~
                                   type$, qtyoh, tcst, price, extension, ~
                                   text$, userid$, sysdate$, std, mdmc,  ~
                                   " ", eod goto L07110
            return

L07220:         FMT CH(25),              /* PART NUMBER                */~
/*PAR000*/          CH(20),              /* SUBPART NUMBER             */~
                    CH(3),               /* STORE NUMBER               */~
                    CH(6),               /* LOT NUMBER                 */~
                    CH(8),               /* SEQUENCE NUMBER            */~
                    CH(6),               /* MODULE DATE POSTED         */~
                    CH(2),               /* TYPE CODE                  */~
                    PD(14,4),            /* QUANTITY                   */~
                    PD(14,4),            /* COST                       */~
                    PD(14,4),            /* PRICE                      */~
                    PD(14,4),            /* EXTENSION OF PRICE         */~
                    CH(40),              /* FREE TEXT FIELDS FOR AUDIT */~
                    CH(3),               /* OPERATOR ID CREATING THIS  */~
                    CH(6),               /* CALENDAR DATE WRITTEN      */~
                    PD(14,4),            /* CURRENT STANDARDS SUMMED   */~
                    PD(14,4),            /* Management DMC             */~
/*PAR000*/          CH(89)               /* FILLER                     */


        rem**************************************************************~
           *         p o s t   h n y p o o l                            *~
           * warning - to keep code to a minimum, the same code is used *~
           * by several of the costing methods.  be extremely careful   *~
           * when changing the routines because of this overlap.        *~
           **************************************************************

        post_invpools

            if qtyoh >= 0 then L08100
               mat cst = zer
                 goto L08140
L08100:        call "PACKZERO" (cst(), cost$)
                  if pos(cost$ > hex(00)) <> 0% then L08180
                  if cstflag% = 0% then L08180
L08140:     if pos("SFT"=costtype$) = 0% then L08170
               mat cst = std
                  goto L08210
L08170:        mat cst = cost
L08180:     if costtype$ <> "L" then L08210
               mat std = cst

L08210:     gosub L09000

            if qtyoh > 0 then L08290
               mat cst  = (1/qtyoh) * pcst
               mat pcst = zer
                  goto L08320

L08290:        mat pwork = (qtyoh) * cst
               mat pcst = pcst - pwork

L08320:        mat twork = con : mat tcost = twork * cst
               tcst = round(tcost(1), 4)
               return

L09000:    init(hex(00)) readkey$  /* Note the Key to the HNYPOOL file.*/
                                   /* The Seq is an integer so must    */
                                   /* init to HEX(00) not blank.       */

           mat pcst = zer
           qty = qtyoh

        REM FIRST WE WORK WITH EXISTING POOL RECORDS
REM PAR000
           str(readkey$,,54) = str(part$,,25) & str(subp$,,20) &      ~
                               str(store$,,3) & lot$
           call "PLOWNXT1" (#4, readkey$, 54%, f1%)  /* PAR000 */
              if f1% = 0% then L11010

L09130:    get   #4, using  L09220,                                        ~
                     lifoleft,           /* AMOUNT LEFT IN  POOL       */~
                     pcost(),            /* PER UNIT COSTS             */~
                     trndate$,           /* TRANSACTIONS DATE          */~
                     oldacct1$,                                          ~
                     oldacct2$
REM      PAR000 
L09220:    FMT POS(59), PD(14,4), POS(83), 12*PD(14,4), CH(6), 2*CH(9)

           if abs(lifoleft) > .00001 then L09340   /* CLEAN UP LOOP */
                delete #4
L09300:         call "READNXT1" (#4, f1%)
                   if f1% = 0% then L11010            /* PAR000 */
                if str(key(#4),,54%) <> str(readkey$,,54%) then L11010
                   goto L09130

        REM  IS THIS AN AVERAGE POOL???
L09340:    if pos("AB" = costtype$) = 0%     /* HANDLE AVERAGE HERE */   ~
                                  then L09590  /* HANDLE AVERAGE HERE */
                if qtyoh < 0 then L09870       /* WITHDRAWAL          */
                   mat pcst = (qty) * cst
                   qty   = round(qty+lifoleft,2)
                      if abs(qty) > .00001 then L09520

            REM  OOPS, NO UNITS LEFT.  CLEAN UP EXCESS $$$ IN HNY ASSET
                mat pwork = (lifoleft) * pcost
                mat pwork = pwork + pcst
                mat twork = con:mat tcost = twork * pwork
                adjust    = round(-tcost(1), 4)

                oldacct1$=acct$(2)
                oldacct2$=acct$(8)
                delete #4
                   gosub L12070
                      goto L10140

            REM  GOT SOME LEFT, SO PREP FOR REWRITE.
L09520:         mat pcost = (lifoleft) * pcost
                mat pcost = pcost + pcst
                mat pcost = (1/qty) * pcost
                lifoleft=qty
                trndate$=date$
                   goto L09950                                 /* REWRITE */

        REM  IS THIS ANOTHER 'SINGLE POOL TYPE'???
L09590:    if pos("FLM"=costtype$)=0% then L09760

              mat holdcst = cst                         /* SAVE */

           if costtype$ <> "M" then L09640             /* SET 'NEW' STDS  */
              mat cst = cost
                 goto L09650
L09640:       mat cst = std
L09650:       aqty= -lifoleft                        /* CURRENT POOL QTY*/
                 gosub L12030                         /* POST ADJUSTMENTS*/
                 call "READ101" (#4,readkey$,f1%)

              mat cst = holdcst                       /* RESTORE */

           if costtype$ <> "M" then L09720             /* SET POOL 'NEW'  */
             mat pcost = cost : trndate$=date$
                 goto L09870                                   /* REWRITE */
L09720:      mat pcost = std : trndate$=date$
                 goto L09870                                   /* REWRITE */

        REM PROCESS 'MULTIPLE POOL RECORD' TYPES
L09760:    if lifoleft*qty > 0 then L11010    /* SAME SIGN, NEW RECORD   */
              if abs(lifoleft)-abs(qty) >= 0 then L09870  /* THIS POOL OK */
                     qty  = round(qty + lifoleft,2%)
                     mat pwork = (lifoleft) * pcost
                     mat pcst  = pcst - pwork
                     delete #4
                     aqty= -lifoleft
                     gosub post_adjustments
                        goto L09300                       /* NEXT POOL REC*/

        REM REWRITE POOL RECORD WITH 'AT LEAST' TWO ENTRY POINTS
L09870:    mat pwork = (qty) * pcost
           mat pcst  = pcst + pwork
           lifoleft = round(lifoleft + qty,2%)
           if abs(lifoleft) > .00001 then L09950
           delete #4
              goto L10140

L09950:    call "PACKZERO" (pcost(), cost$)
           mat twork = con : mat tcost = twork * pcost
           tcost(1)  = round(tcost(1), 4)

           put #4, using L10060,                                          ~
                     lifoleft,           /* AMOUNT LEFT IN  POOL       */~
                     tcost(1),           /* COST                       */~
                     cost$,              /* COSTS                      */~
                     trndate$            /* TRANSACTIONS DATE          */~
* PAR000
L10060:    FMT POS(59), PD(14,4), POS(75), PD(14,4), CH(96), CH(6)

           rewrite  #4

L10140:    aqty=qty
           if pos("AB" = costtype$) = 0% then gosub post_adjustments
              qty=0
              return

        REM HERE WE BUILD NEW POOL RECORDS WHENEVER REQUIRED
L11010:    if abs(qty)<.00001 then L11740     /* GET OUT FAST           */

           if costtype$="M" then L11160
           if pos("SFLT"=costtype$)=0% then L11100  /* STANDARD COSTS?? */
              mat pcost = std
                 goto L11200

L11100:    if qtyoh < 0 then L11160
              mat pcost = cst
                 goto L11200

L11160:    mat pcost = cost

L11200:    qty = round (qty,2%)

* PAR000
           init(hex(00)) str(readkey$,55)
           revseq% = 10000%
           call "PLOWNEXT" (#4, readkey$, 54% , f1%)

           if pos("PTY"=costtype$) <> 0% then L11380     /* RATS, FIFO */

              if f1% = 0% then L11340
                 get #4, using L11330,  revseq%
* PAR000
L11330:              FMT POS(55), BI(4)
L11340:       revseq% = revseq% - 1%
              goto L11470

            REM FIND END OF CURRENT CHAIN
L11380:       if f1% = 0% then L11450
* PAR000
              if str(key(#4),,54%) <> str(readkey$,,54%) then L11450
                 get #4, using L11410,  revseq%
L11410:              FMT POS(55), BI(4)        /*PAR000*/
                 call "READNEXT" (#4, f1%)
                    goto L11380

L11450:       revseq% = revseq% + 1%

L11470:    call "PACKZERO" (pcost(), cost$)
           mat twork = con : mat tcost = twork * pcost
           tcost(1)  = round(tcost(1), 4)

           write #4, using L11660,                                        ~
                     part$,              /* PART                       */~
                     subp$,              /* Sub part PAR000            */~
                     store$,             /* WAREHOUSE                  */~
                     lot$,               /* LOT                        */~
                     revseq%,            /* REVERSE SEQ NUMBER         */~
                     qty,                /* AMOUNT LEFT IN  POOL       */~
                     qty,                /* AMT ORIG PUT INTO POOL     */~
                     tcost(1),           /* TOTAL COST                 */~
                     cost$,              /* COSTS                      */~
                     sysdate$,           /* TRANSACTIONS DATE          */~
                     acct1$,                                             ~
                     acct2$,                                             ~
                     text$, " "          /* POSTING TEXT, FILLER       */

* PAR000
L11660:    FMT CH(25), CH(20), CH(3), CH(6), BI(4), 3*PD(14,4), CH(96),  ~ 
               CH(6),  2*CH(9), CH(40), CH(58)

           mat pwork = (qty) * pcost
           mat pcst  = pcst + pwork
           aqty= qty

           if pos("SFLMT"=costtype$)<>0% then gosub post_adjustments
L11740:       qty = 0
              return

        post_adjustments

            if qtyoh<0 then return

        REM ENTRY TO REVALUE 'SINGLE POOL' TYPES
L12030:     if pos("SFT"=costtype$)<>0% then L12500

            mat pwork = pcost - cst
            mat pwork = (aqty) * pwork
            mat twork = con : mat tcost = twork * pwork
            adjust = round(tcost(1), 4)

L12070: REM ENTRY FOR FLAT AMOUNT ADJUSTMENT (AVERAGE COSTS)
            if abs(adjust)<.00001 then return
            str(gltext$,65%,4%) = "DAJ"
            if pos("LM"=costtype$)<>0% then                              ~
                          str(gltext$,65%,4%) = "CAJ"

            if pos("XLABYM"=costtype$)<>0% then cacct$=acct$(6)          ~
                                            else cacct$=oldacct2$

L12160: REM ENTRY FOR STANDARD ADJUSTMENTS BELOW (TRICKY ISN'T IT)

            str(gltext$,31%,34%)=str(part$,,25%) & str(store$,,3%) & lot$
            str(gltext$,80%,21%)="(INVENTORY AUTO. ADJ)"

            if oldacct1$=" " or pos("RP"=costtype$)=0%                   ~
                       then aacct$=acct$(3) else aacct$=oldacct1$

        REM IF AACCT$=CACCT$ THEN RETURN /* WHY POST NULL ENTRIES? */
*          IF JNLID$ = " " THEN 12330   /* PROGRAM USING THIS AS TIF */
*          IF PSTSEQ% <> 0% THEN 12310
*              RETURNCODE1% = 0%
*              CALL "JNLINFO" (GLMOD$, JNLID$, PSTSEQ%, " ",            ~
*                              " ", #3, F3%, RETURNCODE1%)

        REM CALL "GLPOST2" (CACCT$, 0, ADJUST, DATE$, 0%,GLMOD$,         ~
                GLTEXT$, JNLID$, PSTSEQ%, USERID$, #8, #9, #3, RETURN%)

* Call to write to work file for credit
            call "GLPRTSUB" (glmod$, jnlid$, pstseq%, userid$, date$,    ~
                             cacct$, gltext$, 0, adjust, #10, f10%)

        REM IF JNLID$ = " " THEN 12400   /* PROGRAM USING THIS AS TIF */

            str(gltext$, 67, 1) = "H"
        REM CALL "GLPOST2"  (AACCT$, ADJUST, 0, DATE$, 0%,GLMOD$,        ~
                GLTEXT$, JNLID$, PSTSEQ%, USERID$, #8, #9, #3, RETURN%)


* Call to write to work file for debit
            call "GLPRTSUB" (glmod$, jnlid$, pstseq%, userid$, date$,    ~
                             aacct$, gltext$, adjust, 0, #10, f10%)

            return

L12500: REM *** Standard Control Section ***

            for i% = 1% to 12%
               adjust=round(aqty * (pcost(i%) - cst(i%)), 2)
               if abs(adjust)<.00001 then L12570
                  cacct$=acct$(i% + 6%)
                  str(gltext$,65, 4) = "SAJ"
                  gosub L12160
L12570:     next i%

            return



        REM ENDOFPROGRAMENDOFPROGRAMENDOFPROGRAMENDOFPROGRAMENDOFPROGRAM

