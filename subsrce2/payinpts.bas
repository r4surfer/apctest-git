        REM *************************************************************~
            *                                                           *~
            *  PPPP    AAA   Y   Y  IIIII  N   N  PPPP   TTTTT   SSS    *~
            *  P   P  A   A  Y   Y    I    NN  N  P   P    T    S       *~
            *  PPPP   AAAAA   YYY     I    N N N  PPPP     T     SSS    *~
            *  P      A   A    Y      I    N  NN  P        T        S   *~
            *  P      A   A    Y    IIIII  N   N  P        T     SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PAYINPTS - A subroutine exclusive to PAYINPUT, created    *~
            *            12/23/87 to off-load some code and allow it    *~
            *            (PAYINPUT) to compile. This s/r contains the   *~
            *            screen handling routines as well as DATASAVE.  *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+------------------WHAT--------------------+-WHO-*~
            * 12/23/87 ! ORIGINAL (pulled from PAYINPUT)          ! JIM *~
            * 06/12/89 ! Reset Header after PF8 see Recurring Invs! JDH *~
            * 06/13/89 ! Fixed PF4 for both modes of use - Scrn 1 ! JDH *~
            * 07/07/89 ! Changed literal to note that inv cost is ! JDH *~
            *          !  displayed in statutory currency amount  !     *~
            * 05/31/90 ! Currency isn't blank unless MC is off.   ! JDH *~
            * 09/27/90 ! Disabled PF28 'Delete All' if no line itm! JDH *~
            * 05/14/91 ! Added PF14)See Receipt.  To show PO #,   ! SID *~
            *          !  Rcv Hld, QC, QC Hld, Store # and Lot #. !     *~
            * 01/29/92 ! PRR 10715 - Added ability to exclude QC  ! MLJ *~
            *          !  Qty from Default Qty to Pay and See     !     *~
            *          !  Receipts in Edit mode.                  !     *~
            * 02/03/93 ! Added CH(20) variable for "SWITCHS.VBK". ! MLJ *~
            * 07/20/92 ! Added '%' as needed to reduce object size! MLJ *~
            * 03/24/93 ! Added Variables to COM statement to align! JBK *~
            *          !  with PAYINPUT.  Change due to Core Value!     *~
            * 09/28/93 ! Fix Up Common block for 6.03.00 Mods     ! KAB *~
            * 07/07/94 ! Add Contract ID,&Line to Line Item Screen! RJH *~
            *          !  Add PF25 key for Vend Contract Inquiry  ! RJH *~
            * 07/18/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        sub "PAYINPTS" (payinpts%,                                       ~
                #3,  /* VENDOR  - VENDOR MASTER INFORMATION */           ~
                #5,  /* PAYMASTR- PAYABLES MAIN HEADER FILE */           ~
                #6,  /* PAYLINES- PAYABLES LINE ITEMS FILE */            ~
                #7,  /* SYSFILE2- SYSTEM INFO (DEFAULT PAY DATES) */     ~
                #9,  /* PAYBUFFR- PAYABLES INVOICE HEADER BUFFER */      ~
                #10, /* PAYBUF2 - PAYABLES INVOICE LINE ITEM BUFFER */   ~
                #12, /* HNYQUAN - INVENTORY QUANTITY FILE */             ~
                #17, /* PAYTSKNG- PAYINPUT Tasking Control File */       ~
                #22, /* CSHMASTR- 1099 POSTING NEEDS IT */               ~
                #23, /* CSHLINES- Cash Disbursements File */             ~
                #24, /* CSH1099 - 1099 DETAIL FILE */                    ~
                #25, /* TXTFILE - System Text File */                    ~
                #26, /* SERMASTR- Serial Number Master File */           ~
                #27, /* SERWORK - Work File */                           ~
                #28, /* SERTIF  - Transaction Image File */              ~
                #29, /* SERHOLD - Vendor Invoice Restore */              ~
                #41, /* PAYLNCUR- Currency-specific PAY line items */    ~
                #42, /* PAYLNBF2- Currency-specific PAYBUF2 line items */~
                #43) /* RCVLINES- Reveiver Line Items File (Purchasing)*/

        com /* Any changes made here need to be made in PAYINPUT, also */~
            r41603fix$16,                /* FOR FIX LOGIC INTEGRITY    */~
            acct$(100)16,                /* ACCOUNT NUMBERS            */~
            acctdescr$32,                /* ACCOUNT DESCRIPTION        */~
            base%,                                                       ~
            c%,                                                          ~
            contract_id$(100)16,         /* Purchase Contract ID       */~
            contract_descr$32,           /* Purchase Contract Descriptn*/~
            contract_line$(100)4,        /* Purchase Contract Line #   */~
            core_on%,                                                    ~
            core,                        /* Core Std Cost              */~
            core_inv_flag$1,             /* Core Value Inv Trans Flag  */~
            cost(12), cost$(100)96,      /* Inventory cost hold areas  */~
            costtype$(100)1,             /* Inventory item Cost Type   */~
            convdate$6, conveqv, convunt,/* Currency conversion fields */~
            currkey$50,                  /* Currency lines read key    */~
            curr$1, currtype$1,          /* SYSFILE2 Currency codes    */~
            currency$4, currdesc$32,     /* Currency code& description */~
            cursor%(2),                  /* CURSOR LOACATIONS FOR EDIT */~
            d%,                                                          ~
            date$8,                      /* SCREEN DATE STRING STUFF   */~
            date1$8,                     /* DATE FOR PAY DATE COMPUTING*/~
            date2$6,                     /* ANOTHER DATE...            */~
            datetime$7,                  /* DATE TIME STAMP            */~
            defaultacct$16,              /* DEFAULT ACCOUNT FOR INPUT  */~
            defstr$3,                    /* DEFAULT STORE NUMBR INPUT  */~
            delmax%,                                                     ~
            delpart$(100)25,             /* DELETED PART               */~
            delstore$(100)25,            /* DELETED PART'S STORE       */~
            dellot$(100)25,              /* DELETED PART'S STORE'S LOT */~
            delqty(100),                 /* DELETED PART'S HNYHOLD     */~
            delhold(100),                /* DELETED PART'S HNYHOLD     */~
            descr_m(16),                 /* Descr Map For PlowCode     */~
            dfac$(20)1,                  /* SUMMARY SCREEN FAC'S       */~
            disc_amt$9, disc_amt,        /* DISCOUNT AMOUNT            */~
            disc_pct$10, disc_pct,       /* DISCOUNT PERCENT           */~
            discdate$8,                  /* PAY TAKING DISCOUNT DATE   */~
            editmode%,                                                   ~
            errormsg$79,                 /* ERROR MESSAGE TEXT LINE    */~
            ext$(100)10, ext,            /* EXTENSIONS AND STUFF       */~
            factor$10,                   /* UOM Conversion Factor      */~
            factor(100),                 /* UOM Conversion Factor      */~
            fieldnr%,                                                    ~
            file%, lcur%,                                                ~
            freetext$20,                 /* FREE TEXT INFORMATION      */~
            groupdescr$32,               /* Cutover Group Description  */~
            header$(3)79,                /* SCREEN TITLE               */~
            hold$1,                      /* Invoice On Hold Flag       */~
            i$(24)80,                    /* JUNK SCREEN IMAGE (NOT USED*/~
            inc(2),                      /* For The Elusive "PLOWCODE" */~
            inc$(2)28,                   /* For The Elusive "PLOWCODE" */~
            infomsg$79,                  /* DESCRIPTIVE MESSAGE LINE   */~
            invamt,                                                      ~
            invcost(100),invcost$10,     /* Total Inventory cost       */~
            invdate$8,                   /* SCREEN INVOICE DATE        */~
            invnet$10,                   /* INVOICE TOTAL LESS DISCOUNT*/~
            invoicenr$16,                /* INVOICE NUMBER             */~
            invtype$,                    /* INVOICE TYPE (INTERNAL)    */~
            invtotal$10,                 /* INVOICE TOTAL              */~
            job$(100)8,                  /* JOB NUMBERS FOR LINES      */~
            jobdescr$32,                 /* JOB NUMBER DESCRIPTION     */~
            jobtext$33,                  /* JOB NUMBER POST FROM RCV   */~
            keyhit%,                                                     ~
            l%(2),                       /* Invoice Field Prompt Lenght*/~
            ll%,                                                         ~
            lastinvoice$16,              /* LAST INVOICE NUMBER INPUT  */~
            lastvendor$9,                /* LAST VENDOR PROCESSED      */~
            lfac$(20)1,                  /* LINEAR INPUT FAC'S         */~
            linemode%,                                                   ~
            lot$(100)6,                  /* LOT OF AP LINE ITEM        */~
            manual$8,                    /* For call to 'MANUAL'       */~
            maxlines%,                                                   ~
            message$79,                  /* INPUT MESSAGE              */~
            mode$3,                      /* "YES" Indicates Normal Mode*/~
            nethold, newhold,                                            ~
            nondisc$10, nondisc,         /* NON-DISCOUNTABLE AMOUNT    */~
            oldhold,                                                     ~
            oldseq$(100)3,               /* OLD SEQUENCE NUMBER        */~
            origdate$6,                  /* DATE OF ORIGINAL INVOICE   */~
            origuserid$3,                /* USERID OF ORIGINAL INVOICE */~
            part$(100)25,                /* PART NUMBERS FOR LINES     */~
            partdescr$34,                /* PART NUMBER DESCRIPTION    */~
            payacct$16,                  /* PAY ACCOUNT NUMBER         */~
            payacctdescr$32,             /* PAYABLES ACCOUNT DESCRIPTN */~
            payaccttype$1,               /* PAYABLES ACCOUNT TYPE      */~
            paydate$8,                   /* PAYABLES DATE INFORMATION  */~
            pfdescr$(3)79,               /* FUNCTION KEYS ENABLED LISTS*/~
            pfkeys$32,                   /* FUNCTION KEYS ENABLED LISTS*/~
            plowkey$90,                  /* KEY FOR PLOW ROUTINES      */~
            po$16,                       /* PO Number For Auto Generate*/~
            po$(100)16,                  /* PO Number For Line         */~
            poline$(100)3,               /* PO Line For Line!          */~
            prog$8,                      /* Program name               */~
            postatstdcost$1,             /* Post Cost at Std Cost      */~
            puracct$16,                  /* PURCHASES ACCOUNT NUMBER   */~
            puracctdescr$32,             /* PURCHASES ACCT DESCRIPTION */~
            price(100),                  /* CALCULATED PRICES FOR AP LN*/~
            price$(100)10,               /* CALCULATED PRICES FOR AP LN*/~
            prompt$(6)29,                /* Floating Screen Prompts    */~
            qty(100),                    /* QUANTITIES FOR EACH LINE   */~
            qtyhold(100,2),              /* OLD QUANTITIES FOR HNYHOLD */~
            qty$(100)10,                 /* QUANTITIES FOR EACH LINE   */~
            qty_inqc(100),               /* QTY IN QC      - EACH LINE */~
            qtytopay$1,                  /* QTY TO PAY SWITCH          */~
            qty_qchold(100),             /* QTY IN QC HOLD - EACH LINE */~
            rcv$(100)16,                 /* Reveiver Number For Line   */~
            readkey$90,                  /* KEY FOR PLOW ROUTINES      */~
            receiver$16,                 /* DEFAULT REVEIVER NUMBER    */~
            regulardate$8,               /* REGULAR DATE INFORMATION   */~
            reman%,                                                      ~
            remanpart$50,                /* Reman Part Key             */~
            return%,                                                     ~
            scr%(3,17), set%(255),       /* Enable arrays              */~
            search%(2),                  /* CURSOR LOCATIONS FOR EDIT  */~
            seq$(100)3,                  /* SEQUENCE NUMBERS FOR SCREEN*/~
            seqnr$3,                     /* SEQUENCE NUMBER FLAG       */~
                                         /* SERIAL NUMBER VARIABLES    */~
            sn_config$(100)36,           /*  Who were they for?        */~
                tmp_config$36,           /*                            */~
            sn_delex%(999), sn_delex%,   /*  Deleted Indices           */~
            sn_dpart$(999)25,            /*                            */~
            sn_index%(100),              /*  Unique Index for line-SNs */~
            sn_loc$30,                   /*  Current Location          */~
            sn_mastr$(2)150,             /*  Entire SERMASTR record    */~
            sn_source$1,                 /*  Source                    */~
            sn_status$1,                 /*  Save 'To' Status          */~
            sn_trankey$40,               /*  Transaction Key           */~
            sn_used%(100),               /*  Gone but not forgotten    */~
            sn_used_msg$30,              /*                            */~
                                                                         ~
            statutory$4,                 /* Statutory currency code    */~
            store$(100)3,                /* STORE # FOR AP LINE ITEMS  */~
            strdescr$32,                 /* JOB NUMBER DESCRIPTION     */~
            sumhdr$79,                   /* Line summary header        */~
            sysacct$(6)9,                /* SYSTEM DEFAULT ACCOUNTS    */~
            ten99$4,                     /* 1099 CATEGORY              */~
            ten99descr$32,               /* 1099 CATEGORY DESCRIPTION  */~
            text$4,                      /* Document Text Id. Number   */~
            textmsg$79,                  /* Text Rtn Message           */~
            text$(113,1)70,              /* Free Text Array            */~
            tfac$(20)1,                  /* SUMMARY SCREEN FAC'S       */~
            this%,                                                       ~
            topline$79,                  /* Floating Program Title     */~
            userid$3,                    /* USERID THIS USER           */~
            var$20,                      /* SWITCHS.VBK Variable       */~
            vbkkey$100,                  /* MISC READKEY               */~
            varacct$(100)16,             /* Price cost variance acct # */~
            varacctdescr$32,             /* Description of above       */~
            varmsg$34,                   /* Price cost variance Warning*/~
            venaddr$(6)30,               /* ADDRESS THIS VENDOR        */~
            vencode$9,                   /* VENDOR CODE THIS INVOICE   */~
            vendescr$32,                 /* VENDOR DESCRIPTION         */~
            venpart$(100)25,             /* Vendor's Part Number       */~
            venprice$10,                 /* Vendor Price For Line Item */~
            vpc_open%,                   /* VBKVSA Open Flag (1=Y,-1=N)*/~
            vpc_temp$16,                 /*    Contract Line for Query */~
            vpc_temp1$4,                 /*    Contract Line for Query */~
            venqty$10,                   /* Vendor Qty For Line Item   */~
            vpc_code$25,                 /* Vend Purchs Cntarct ItemCd */~
            vpc_potyp$1,                 /* PO Parts VPC Type (P,A,M,H)*/~
            vpc_type$1,                  /* Vend Purchs Cntarct Type   */~
            vpc_vendor$9,                /* Vend Purchs Cntarct Vendor */~
            xixacct$16,                  /* ACCOUNT WORK VARIABLE      */~
                                                                         ~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            f2%(64),                     /* FILE STATUS FLAGS          */~
            fs%(64),                     /* FILE STATUS FLAGS          */~
            rslt$(64)20                  /* RETURNED FROM OPENFILE     */

         dim                                                             ~
            blankdate$8                  /* Blank Date for Comparison. */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *        M A I N   P R O C E S S I N G   L O G I C          *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            if payinpts% = 999% then goto  datasave
            if payinpts% = 201% then gosub deffn_201
            if payinpts% = 211% then gosub deffn_211
            if payinpts% = 202% then gosub deffn_202
            if payinpts% = 212% then gosub deffn_212
            if payinpts% = 103% then gosub deffn_103
            if payinpts% = 113% then gosub deffn_113
            if payinpts% = 115% then gosub deffn_115
            if payinpts% = 125% then gosub deffn_125
            goto L65000

        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *************************************************************

        set_up_descriptor
            if manual$ = prog$                                           ~
               then str(header$(1%),62%) = "PAYINPTS: " & cms2v$         ~
               else str(header$(1%),54%) = manual$ & ": " & str(prog$) & ~
                    ": " & cms2v$
            return

        set_up_line3
            header$(2%) = "Inv: " & str(invoicenr$) & " Tot: " &         ~
                str(invtotal$) & " Disc: " & str(disc_amt$) & " Net: " & ~
                invnet$
            return

        REM *************************************************************~
            *          W R I T E   D A T A   T O   B U F F E R          *~
            *                                                           *~
            * THIS ROUTINE DELETES THE OLD INVOICE FROM THE BUFFER IF   *~
            * THERE WAS ONE.  THEN IT GOES AND CALLS THE ROUTINE THAT   *~
            * WRITES THE NEW ONE OUT THERE.                             *~
            *************************************************************

        datasave
            REM First Try To Delete The Old Invoice From Buffer...
                readkey$ = vencode$
                str(readkey$,10%) = invoicenr$
                call "REDALT1" (#9, readkey$, 1%, f1%(9%))
                      if f1%(9) <> 0% then delete #9    /* PAYBUFFR */
                call "DELETE" (#10, readkey$, 25%)      /* PAYBUF2  */
                call "DELETE" (#42, readkey$, 25%)      /* PAYLNBF2 */

                if mode$ <> "REC" then L19220
                     REM Recurring Mode Writes Straight To Master Files
                     call "DELETE" (#5, readkey$, 25%)  /* PAYMASTR */
                     call "DELETE" (#6, readkey$, 25%)  /* PAYLINES */
                     call "DELETE" (#41, readkey$, 25%) /* PAYLNCUR */

L19220:     REM Set Last Document Input Information And Write Invoice.
                gosub L32000
                lastvendor$ = vencode$
                lastinvoice$ = str(invoicenr$,l%(1%),l%(2%))
                goto L65000

L32000: REM *************************************************************~
            *       W R I T E   I N V O I C E   T O   B U F F E R       *~
            * --------------------------------------------------------- *~
            * TOSSES THE CURRENT INVOICE TO THE BUFFER.  THE DATASAVE   *~
            * ROUTINE DELETED THE OLD INVOICE, SO NOW WE WRITE THE NEW  *~
            * ONE IN ITS PLACE.                                         *~
            *************************************************************

*        Write line item information to file.
            if mode$ <> "REC" then L32120
                call "SHOSTAT" ("Saving Recurring Control Invoice")
                file% = 6% : lcur% = 41% /* PAYLNCUR shadows PAYLINES */
                goto L32180
L32120:     file% = 10% : lcur% = 42% /* PAYLNBF2 shadows PAYBUF2 */
            if mode$ <> "OFF" then L32160
                call "SHOSTAT" ("Saving Invoice")
                goto L32180
L32160:     call "SHOSTAT" ("Writing Invoice To Buffer")

L32180:     invamt = 0
            if currency$ = " " then currency$ = statutory$
            if maxlines% = 0% then L32720
                for c% = 1% to maxlines%
                    convert c% to seqnr$, pic(###)
                    ext = 0
                    convert ext$(c%) to ext, data goto L32250
        REM               INVAMT = ROUND(INVAMT + EXT, 2)
L32250:             invcost(c%) = round(invcost(c%), 4)
                    call "GLUNFMT" (acct$(c%))
                    call "GLUNFMT" (varacct$(c%))
                    get cost$(c%) using L32290, cost()
L32290:                   FMT 12*PD(14,4)
                    call "PACKZERO" (cost(), cost$(c%))

                    if curr$ <> "Y" then goto L32430
                    if currency$ = statutory$ then goto L32430
                    write #lcur% using L32380, currency$, vencode$,       ~
                        invoicenr$, seqnr$, ext, price(c%), convdate$,   ~
                        conveqv, convunt, " "

L32380:                 FMT CH(4), CH(9), CH(16), CH(3), PD(14,4),       ~
                            PD(14,7), CH(6), 2*PD(14,7), CH(30)

                    ext       = ext       * conveqv
                    price(c%) = price(c%) * conveqv
L32430:             invamt = round(invamt + ext, 2)
                    write #file%, using L33830, rcv$(c%), po$(c%),        ~
                          poline$(c%), vencode$, invoicenr$, seqnr$,     ~
                          acct$(c%), part$(c%), qty(c%), ext,            ~
                          job$(c%), store$(c%), lot$(c%), price(c%),     ~
                          oldseq$(c%), factor(c%), " ", venpart$(c%),    ~
                          sn_index%(c%), invcost(c%), cost$(c%),         ~
                          varacct$(c%), contract_id$(c%),                ~
                          contract_line$(c%), " "
                     if sn_index%(c%) = 0% then L32610
                          sn_trankey$ = str(vencode$) & str(invoicenr$)
                          convert sn_index%(c%) to                       ~
                                       str(sn_trankey$,26%,4%), pic(0000)
                          sn_status$ = "2"
                          if qty(c%) < 0% then sn_status$ = "0"
                          call "SERSAVE" (sn_index%(c%), "VT",           ~
                                          sn_trankey$, 25%, part$(c%),   ~
                                          userid$, sn_status$, "2", 0%,  ~
                                          #7, #28, #26, #27)
L32610:              if mode$   = "OFF" or mode$    = "REC" then L32690
                     if po$(c%) <> " "  or rcv$(c%) <> " "  then L32690
                        oldhold = max(0, qtyhold(c%,1%) - qtyhold(c%,2%))
                        newhold = max(0, qtyhold(c%,1%)- qty(c%))
                        nethold = newhold - oldhold
                        if nethold = 0 then L32690
                            call "HNYHOLD"(#12, part$(c%), store$(c%),   ~
                                           lot$(c%), nethold, return%)
L32690:         next c%
                goto L33130

L32720
*        Now clear out deleted serial number indices
            if sn_delex% = 0% then L32850
                for d% = 1% to sn_delex%
                     if sn_delex%(d%) = 0% then L32830
                          sn_trankey$ = str(vencode$) & str(invoicenr$)
                          convert sn_delex%(d%) to                       ~
                                       str(sn_trankey$,26%,4%), pic(0000)
                          call "SERSAVE" (sn_index%(c%), "VT",           ~
                                          sn_trankey$, 25%,              ~
                                          sn_dpart$(d%), userid$,        ~
                                          "2", "2", 1%,#7, #28, #26, #27)
L32830:         next d%

L32850:     if maxlines% > 0% then L33130

*        If DELETED Invoice Isn't In Master, Save Nothing...
            call "TXTFUTIL" (#25, 0%, "XOUT", text$)
            if mode$ = "OFF" then L33130 /* Go thru paytoss  */
            if mode$ = "REC" then L33500 /* Bye-Bye Recurry! */
                readkey$ = str(vencode$) & str(invoicenr$)
                call "READ100" (#5, readkey$, f1%(5%))
                     if f1%(5) = 0% then L33410

*        Is in master, allow cancelling of changes. (Aka buffer delete)
L32960:     ask% = 0%
            call "ASKUSER" (ask%, "*** DELETE OPTION ***",               ~
                "Press RETURN to cancel changes made to this invoice",   ~
                "---- OR ----",                                          ~
                "Press PF-16 to DELETE this invoice entirely")
            if ask% =  16% then L33130    /* Delete Invoice   */
            if ask% <>  0% then L32960
                gosub restore_serial_numbers
                if delmax%  = 0% then L33130
                   for i% = 1% to delmax%
                       nethold = delqty(i%) + delhold(i%)
                       if nethold = 0 then L33100
                       call "HNYHOLD" (#12, delpart$(i%), delstore$(i%), ~
                                       dellot$(i%), - nethold, return%)
L33100:            next i%
                goto L33410

L33130
*        WRITE HEADER INFORMATION TO FILE
            call "DATUNFMT" (invdate$)
            call "DATUNFMT" (regulardate$)
            call "DATUNFMT" (discdate$)
            call "GLUNFMT" (puracct$)
            call "GLUNFMT" (payacct$)
            if origdate$ = " " or origdate$ = blankdate$ then origdate$ = date
            if origuserid$ = " " then origuserid$ = userid$
            convert nondisc$ to nondisc
               if curr$ <> "Y" then goto L33220
               if currency$ = statutory$ then goto L33220
                  nondisc  = nondisc  * conveqv
                  disc_amt = disc_amt * conveqv
L33220:     if mode$ = "REC" then hold$ = "Y"
            if invtype$ = " " and mode$ = "REC" then invtype$ = "R"
            if invtype$ = " " and mode$ = "OFF" then invtype$ = "O"
            if invtype$ = " " then invtype$ = "N"

            put str(lot$(),,340%), using L33550, vencode$, invoicenr$,    ~
                            receiver$, invdate$, puracct$, payacct$,     ~
                            payaccttype$, regulardate$, discdate$,       ~
                            nondisc, " ", origdate$, origuserid$, date,  ~
                            userid$, invamt, 0, freetext$, disc_pct,     ~
                            disc_amt, ten99$, hold$, invtype$, text$,    ~
                            currency$, " "

            if mode$ = "REC" then L33470
              /* Write Out Invoice For Posting...                      */
L33370:         call "GETDTTM" addr (datetime$)
                write #9, using L33400, userid$, datetime$, str(lot$()),  ~
                                    str(lot$(),201%,140%), eod goto L33370
L33400:               FMT CH(3), CH(7), CH(200), CH(140)
L33410:         call "TXTFUTIL" (#25, 0%, "SAVE", text$)
                if mode$ = "OFF" then gosub toss_it  /* Straight In */
                readkey$ = str(vencode$) & str(invoicenr$)
                call "DELETE" (#17, readkey$, 25%)
                return

L33470:       /* Write Out 'Control' Invoice Straight To Master...     */
                write #5, using L33490, str(lot$()),str(lot$(),201%), " "
L33490:         FMT CH(200), CH(140), CH(10)
L33500:         call "TXTFUTIL" (#25, 0%, "SAV2", text$)
                readkey$ = str(vencode$) & str(invoicenr$)
                call "DELETE" (#17, readkey$, 25%)
                return

L33550:     FMT /* File #5 (PAYMASTR) & file #9 (PAYBUFFR) output rec  */~
                CH(9),                   /* VENDOR CODE                */~
                CH(16),                  /* INVOICE NUMBER             */~
                CH(16),                  /* RECEIVER NUMBER            */~
                CH(6),                   /* INVOICE DATE               */~
                CH(9),                   /* PURCHASES ACCOUNT          */~
                CH(9),                   /* PAYABLES ACCOUNT           */~
                CH(1),                   /* PAYABLES ACCOUNT TYPE      */~
                CH(6),                   /* PAY W/O DISCOUNT DATE      */~
                CH(6),                   /* PAY W/DISCOUNT DATE        */~
                PD(14,4),                /* NON-DISCOUNTABLE AMOUNT    */~
                CH(6),                   /* LAST POSTING DATE          */~
                CH(6),                   /* ORIGINAL INPUT DATE        */~
                CH(3),                   /* ORIGINAL INPUT BY          */~
                CH(6),                   /* LAST MOD DATE              */~
                CH(3),                   /* LAST MOD BY                */~
                PD(14,4),                /* INVOICE AMOUNT             */~
                PD(14,4),                /* INVOICE OPEN AMOUNT        */~
                CH(20),                  /* FREE TEXT FIELD            */~
                PD(14,4),                /* DISC PERCENT               */~
                PD(14,4),                /* DISCOUNT AMOUNT            */~
                CH(4),                   /* 1099 CATEGORY              */~
                CH(1),                   /* HOLD STATUS                */~
                CH(1),                   /* INVOICE TYPE (INTERNAL)    */~
                CH(4),                   /* TEXT ID NUMBER             */~
                CH(4),                   /* Currency code              */~
                CH(164)                  /* FILLER                     */

L33830:     FMT /*  File #6 or #10 (#FILE%)- PAYLINES or PAYBUF2 output*/~
                CH(16),                  /* RECEIVER NUMBER            */~
                CH(16),                  /* PO NUMBER                  */~
                CH(03),                  /* PO LINE NUMBER             */~
                CH(9),                   /* VENDOR CODE                */~
                CH(16),                  /* INVOICE NUMBER             */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                CH(9),                   /* EXPENSE ACCOUNT NUMBER     */~
                CH(25),                  /* PART NUMBER                */~
                PD(14,4),                /* QUANTITY                   */~
                PD(14,4),                /* EXTENSION                  */~
                CH(8),                   /* JOB NUMBER                 */~
                CH(03),                  /* STORE NUMBER               */~
                CH(06),                  /* LOT NUMBER                 */~
                PD(14,7),                /* UNIT PRICE                 */~
                CH(3),                   /* OLD SEQUENCE NUMBER        */~
                PD(14,4),                /* Quantity Per Vendor Unit   */~
                CH(4),                   /* Unit Of Measure            */~
                CH(25),                  /* Vendors Part Number        */~
                BI(4),                   /* Serial Number Index        */~
                PD(14,4),                /* Total Inventory cost       */~
                CH(96),                  /* Inventory cost array       */~
                CH(9),                   /* Cost variance account      */~
                CH(16),                  /* Contract Id                */~
                CH(04),                  /* Contract Line Number       */~
                CH(226)                  /* FILLER                     */

        toss_it  /* Offline Invoices */
            if o22% = 0% then call "OPENCHCK" (#22, o22%, 0%, 0%, " ")
            if o23% = 0% then call "OPENCHCK" (#23, o23%, 0%, 0%, " ")
            if o24% = 0% then call "OPENCHCK" (#24, o24%, 0%, 0%, " ")
            call "DATUNFMT" (paydate$)
            call "PAYTOSS"  (paydate$,this%,#5,#6,#9,#10,#3,#23,#22,#24, ~
                                            #28, #26, #29)
            call "DATEFMT"  (paydate$)
        return

        restore_serial_numbers
*        Reload Serial Numbers from SERHOLD and Write to SERMASTR & TIF
            plowkey$ = "VT" & str(vencode$) & str(invoicenr$)
            call "DELETE" (#28, plowkey$, 27%) /* TIF- just in case    */

            plowkey$ = str(vencode$) & str(invoicenr$) & hex(00)
L34240:     call "PLOWNEXT" (#29, plowkey$, 25%, f1%(29%))
            if f1%(29) = 0% then L34360
                get #29 using L34270, str(sn_mastr$())
L34270:              FMT XX(33), CH(300)
                call "DELETE" (#26, str(sn_mastr$(),52%,45%), 45%)
                write #26 using L34300, str(sn_mastr$())
L34300:              FMT CH(300)
                write #28 using L34330, str(sn_mastr$(),216%,42%),        ~
                                       str(sn_mastr$(), 32%,45%), 0, " "
L34330:              FMT CH(42), CH(45), PD(14,4), CH(5)
                goto L34240

L34360:     plowkey$ = str(vencode$) & str(invoicenr$)
            call "DELETE" (#29, plowkey$, 25%)
            return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn_201
                init(hex(8c)) lfac$()
                pfdescr$(1%) = "(1)Start Over     (4)Previous Field      ~
        ~                      (13)Instructions"
                pfdescr$(2%) = "                  (8)Display Existing Rec~
        ~urring Invoices       (15)Print Screen"
                pfdescr$(3%) = "(17)Auto Generate Invoice From P.O.      ~
        ~                      (16)Exit Program"
                pfkeys$ = hex(0001040d0f100811)

                REM Turn off appropriate fields...
                header$(1%) = " "
                if mode$ <> "REC" and mode$ <> "OFF" then L40210
                   str(pfdescr$(3%),,35%) = " " /* Shut Off Auto Gen   */
                   str(pfkeys$,8%,1%) = hex(ff)
L40210:         if mode$ = "REC" and fieldnr% = 2% then L40240
                   str(pfdescr$(2%),19%,40%) = " " /* Shut Off Search  */
                   str(pfkeys$,7%,1%) = hex(ff)
L40240:         if fieldnr% > 3% and mode$ <> "REC" then L40270
                if fieldnr% > 5% and mode$ = "REC" then L40270
                str(pfdescr$(1%),19%,19%) = " " /* Shut Off Prev Field */
                str(pfkeys$,3%,1%) = hex(ff)
L40270:         if fieldnr% > 2% then L40310
                if lastvendor$ <> " " then header$(1) = "Last Vendor: " &~
                     lastvendor$ & ", Last Invoice: " & lastinvoice$
                goto L40550
L40310:         pfdescr$(3%) = " "              /* Shut Off Exit       */
                str(pfkeys$,6%,1%),str(pfkeys$,8%,1%) = hex(ff)
                goto L40550

            deffn_211
                REM Editmode logic...
                pfdescr$(1%) = "(1)Start Over                            ~
        ~                      (13)Instructions"
                pfdescr$(2%) = "(2)Line Items      (5)Next Page          ~
        ~                      (15)Print Screen"
                pfdescr$(3%) = "(25)Free Text                            ~
        ~                      (16)Save Data"
                pfkeys$ = hex(000102050d0f1019)
                if manual$ = "PROC22" then str(pfkeys$,9%,1%) = hex(1d)
                header$(1) = "Inv Total: " & str(invtotal$) & " Disc: " &~
                     str(disc_amt$) & " Net: " & invnet$
                init(hex(8c)) lfac$()
                init(hex(86)) str(lfac$(),3%)
                if fieldnr% = 0% then L40550
                     str(pfdescr$(2%),,63%), pfdescr$(3%) = " "
                     str(pfkeys$,3%,2%), str(pfkeys$,7%,3%) = hex(ffffff)
                     init(hex(8c)) lfac$()

L40550:           str(pfdescr$(3%),62%,1%) = hex(84)
                  gosub set_up_descriptor
                  if fieldnr% > 0% then lfac$(fieldnr%) = hex(81)

L40770:     accept                                                       ~
               at (01,02), fac(hex(8c)),   topline$             , ch(79),~
               at (02,02), fac(hex(ac)),   header$(1%)          , ch(79),~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (06,02), "Vendor Code",                                ~
               at (06,30), fac(lfac$(1%)), vencode$             , ch(09),~
               at (06,49), fac(hex(8c)),  vendescr$             , ch(32),~
               at (07,02), fac(hex(8c)),  prompt$(1%)           , ch(29),~
               at (07,30), fac(lfac$(2%)),str(invoicenr$,l%(1%), l%(2%)),~
               at (07,49), fac(hex(8c)),  groupdescr$           , ch(32),~
               at (09,02), "Vendor Name",                                ~
               at (09,30), fac(hex(8c)),  venaddr$(1%)          , ch(30),~
               at (10,02), "   Address (Line 1)",                        ~
               at (10,30), fac(hex(8c)),  venaddr$(2%)          , ch(30),~
               at (11,02), "           (Line 2)",                        ~
               at (11,30), fac(hex(8c)),  venaddr$(3%)          , ch(30),~
               at (12,02), "           (Line 3)",                        ~
               at (12,30), fac(hex(8c)),  venaddr$(4%)          , ch(30),~
               at (13,02), "           (Line 4)",                        ~
               at (13,30), fac(hex(8c)),  venaddr$(5%)          , ch(30),~
               at (14,02), "   City, State, Zip",                        ~
               at (14,30), fac(hex(8c)),  venaddr$(6%)          , ch(30),~
               at (15,02), fac(hex(8c)),   prompt$(2%)          , ch(29),~
               at (15,30), fac(lfac$(3%)),  invdate$            , ch(08),~
               at (16,02), fac(hex(8c)),   prompt$(3%)          , ch(29),~
               at (16,30), fac(lfac$(4%)),  regulardate$        , ch(08),~
               at (17,02), fac(hex(8c)),   prompt$(4%)          , ch(29),~
               at (17,30), fac(lfac$(5%)),  discdate$           , ch(08),~
               at (18,02), "Discount Percent",                           ~
               at (18,30), fac(lfac$(6%)),  disc_pct$           , ch(10),~
               at (18,41), "And Amount",                                 ~
               at (18,52), fac(lfac$(7%)),  disc_amt$           , ch(10),~
               at (19,02), "Non-Discountable Amount",                    ~
               at (19,30), fac(lfac$(8%)),  nondisc$            , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfdescr$(1%)         , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(2%)         , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(3%)         , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 8% then L41340
                  if mode$ <> "REC" or fieldnr% <> 2% then L41340
                  readkey$ = str(vencode$) & "#"
                  errormsg$ = hex(06) & "Existing Recurring Invoices"
                  header$(), inc$() = " "
                  mat inc = zer
                  inc(1) = 15.11
                  call "PLOWCODE" (#5, readkey$, errormsg$, 5010%,  0,   ~
                                 f1%(5%), header$(), 0, 0, inc(), inc$())
                     if f1%(5%) <> 0% then invoicenr$ = str(readkey$,10%)
                  errormsg$ = " "
                  gosub set_up_descriptor
                  goto L40770

L41340:        if keyhit% <> 13% then L41380
                  call "MANUAL" (manual$)
                  goto L40770

L41380:        if keyhit% <> 15% then L41420
                  call "PRNTSCRN"
                  goto L40770

L41420:        if editmode% = 0% then return
               close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   2      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn_202
                init(hex(8c)) lfac$()
                header$(1%) = " "
                pfdescr$(1%) = "(1)Start Over     (4)Previous Field      ~
        ~                      (13)Instructions"
                pfdescr$(2%) = "                                         ~
        ~                      (15)Print Screen"
                pfdescr$(3%) = "                                         ~
        ~                                      "
                pfkeys$ = hex(0001040d0f)

                REM Turn off appropriate fields...
                if fieldnr% > 1% then L42190
                str(pfdescr$(1%),19%,19%) = " "
                str(pfkeys$,3%,2%) = hex(ffff)
                goto L42190

            deffn_212
                REM Editmode logic...
                pfdescr$(1%) = "(1)Start Over                            ~
        ~                      (13)Instructions"
                pfdescr$(2%) = "(2)Line Items      (4)Prev Page          ~
        ~                      (15)Print Screen"
                pfdescr$(3%) = "(25)Free Text                            ~
        ~                      (16)Save Data"
                pfkeys$ = hex(000102040d0f1019)
                if manual$ = "PROC22" then str(pfkeys$,9%,1%) = hex(1d)
                init(hex(86)) lfac$()
                if fieldnr% = 0% then L42190
                     str(pfdescr$(2%),,63%), pfdescr$(3%) = " "
                     str(pfkeys$,3%,2%), str(pfkeys$,7%,3%) = hex(ffffff)
                     init(hex(8c)) lfac$()

L42190:           str(pfdescr$(3%),62%,1%) = hex(84)
                  header$(1%) = "Vendor: " & vencode$ & ", Invoice: " &  ~
                                            str(invoicenr$,l%(1%),l%(2%))
                  gosub set_up_descriptor
                  if fieldnr% > 0% then lfac$(fieldnr%) = hex(81)

L42300:     accept                                                       ~
               at (01,02), fac(hex(8c)),   topline$             , ch(79),~
               at (02,02), fac(hex(ac)),   header$(1%)          , ch(79),~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (06,02), "Free Text Field",                            ~
               at (06,30), fac(lfac$(1%)), freetext$            , ch(20),~
               at (07,02), "1099 Category Code",                         ~
               at (07,30), fac(lfac$(2%)), ten99$               , ch(04),~
               at (07,49), fac(hex(8c)),     ten99descr$        , ch(30),~
               at (08,02), "Payables Account",                           ~
               at (08,30), fac(lfac$(3%)), payacct$             , ch(12),~
               at (08,49), fac(hex(8c)),     payacctdescr$      , ch(32),~
               at (09,02), fac(hex(8c)),   prompt$(5%)          , ch(29),~
               at (09,30), fac(lfac$(4%)), hold$                , ch(01),~
               at (10,02), "Default Purchases Account",                  ~
               at (10,30), fac(lfac$(5%)), puracct$             , ch(12),~
               at (10,49), fac(hex(8c)),     puracctdescr$      , ch(32),~
               at (11,02), fac(hex(8c)),   prompt$(6%)          , ch(29),~
               at (11,30), fac(lfac$(6%)), receiver$            , ch(16),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfdescr$(1%)         , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(2%)         , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(3%)         , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13% then L42465
                  call "MANUAL" (manual$)
                  goto L42300

L42465:        if keyhit% <> 15% then L42485
                  call "PRNTSCRN"
                  goto L42300

L42485:        if editmode% = 0% then return
               close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   3      *~
            * --------------------------------------------------------- *~
            * Input/Edit of First Line Item Screen.                     *~
            *************************************************************

            deffn_103
                init(hex(8c)) lfac$()
                pfdescr$(1%) = "(1)Start Invoice Over                  "&~
                               "                       (13)Instructions"
                pfdescr$(2%) = "(2)Restart Line     (4)Prev. Field     "&~
                               "                       (15)Print Screen"
                pfdescr$(3%) = "                    (6)Same as Prev Lin"&~
                               "e (25)Vendor Contracts (16)Edit Mode"
                pfkeys$ = hex(00010204060dff0f1019)
                if fieldnr% > 4% then L43200
                if fieldnr% = 2% and curr$ <> "Y" then L43170
                if fieldnr% > 1% then L43230
L43170:           str(pfdescr$(2%),,37%) = " " /* Shut Off Prev Field */
                  str(pfkeys$,3%,2%) = hex(ffff)
                  goto L43230

L43200:         str(pfdescr$(3%),63%) = " "   /* Shut Off Edit Mode  */
                str(pfkeys$,8%,1%) = hex(ff)

                readkey$ = str(part$(c%)) & str(rcv$(c%)) &              ~
                 str(vencode$) & str(po$(c%)) & str(poline$(c%)) & hex(00)
                call "REDALT0" (#43, readkey$, 1%, f1%(43%))
                if f1%(43) = 0% then L43230
                   str(pfdescr$(1%),41%,16%) = "(14)See Receipts"
                   str(pfkeys$,7%,1%) = hex(0e)

L43230:         if vpc_open% = 1% then L43500    /* Set Up Header Screen */
                   str(pfdescr$(3%),42%,21%) = " " /*Shut Off Vend Cntrc*/
                   str(pfkeys$,10%,1%) = hex(ff)

                goto L43500

            deffn_113
                init(hex(86)) lfac$()
                if fieldnr% <> 0% then init(hex(8c)) lfac$()
                pfdescr$(1%) = "(1)Start Over                          "&~
                               " (14) See Receipts      (13)Instructions"
                pfdescr$(2%) = "(2)First Line             (6)Prev Line "&~
                               "  (9)Header             (15)Print Screen"
                pfdescr$(3%) = "(3)Last Line              (7)Next Line "&~
                               " (25)Vendor Contracts   (16)Line Summary"
                pfkeys$ = hex(000102060307090d0e0f10ff19)
                if manual$ = "PROC22" then str(pfkeys$,12%,1%) = hex(1d)

                REM Turn Off Appropriate Fields. Are we editing a field?
                if fieldnr% = 0% then L43420  /* no */
                     str(pfdescr$(2%),,63%), pfdescr$(3%) = " "
                     init(hex(ff)) str(pfkeys$,3%,5%),str(pfkeys$,11%,2%)
                     init(hex(8c)) lfac$()
                     goto L43485
L43420:         REM Display Mode...
                if c% > 1% then L43460
                  str(pfdescr$(2%),,39%)  = " " /* Shut Off Prev Stuff */
                  str(pfkeys$,3%,2%) = hex(ffff)
L43460:         if c% < maxlines% then L43485
                  str(pfdescr$(3%),,39%)  = " " /* Shut Off Prev Stuff */
                  str(pfkeys$,5%,2%) = hex(ffff)

L43485:         if vpc_open% = 1% then L43500    /* Set Up Header Screen */
                   str(pfdescr$(3%),41%,21%) = " " /*Shut Off Vend Cntrc*/
                   str(pfkeys$,13%,1%) = hex(ff)

L43500
*        Set Up header Portion Of Screen...
            str(pfdescr$(3%),62%,1%) = hex(84)
            header$(1%) = "Vendor: " & vencode$ & " " & vendescr$
            gosub set_up_descriptor
            gosub set_up_line3
            convert c% to str(header$(2%),76%,3%), pic(###)
            str(header$(2%),73%,3%) = "Ln#"

            if fieldnr% > 0% then lfac$(fieldnr%) = hex(81)

            if fieldnr% = 13% then return /* No screen for Inv cost    */

L43860:     accept                                                       ~
               at (01,02), "Manage Invoice Line Items",                  ~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(8c)), header$(1%)            , ch(79),~
               at (03,02), fac(hex(ac)), header$(2%)            , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(94)), infomsg$               , ch(79),~
                                                                         ~
               at (06,02), "Currency code",                              ~
               at (06,22), fac(lfac$(1%)), currency$            , ch(04),~
               at (06,48), fac(hex(8c)),   currdesc$            , ch(32),~
               at (07,02), "Purchase Order #",                           ~
               at (07,22), fac(lfac$(2%)), po$(c%)              , ch(16),~
               at (07,39), "PO Line:",                                   ~
               at (07,48), fac(lfac$(2%)), poline$(c%)          , ch(03),~
               at (08,02), "Receiver Number",                            ~
               at (08,22), fac(lfac$(3%)), rcv$(c%)             , ch(16),~
               at (08,48), fac(hex(8c)),   jobtext$             , ch(33),~
               at (09,02), "Part Number",                                ~
               at (09,22), fac(lfac$(4%)), part$(c%)            , ch(25),~
               at (09,48), fac(hex(8c))  , partdescr$           , ch(33),~
               at (10,02), "Store Number",                               ~
               at (10,22), fac(lfac$(5%)), store$(c%)           , ch(03),~
               at (10,26), fac(hex(8c))  , strdescr$            , ch(32),~
               at (10,60), "Lot Number",                                 ~
               at (10,72), fac(lfac$(6%)), str(lot$(c%),,ll%),           ~
               at (11,02), "Vendor Part #",                              ~
               at (11,22), fac(lfac$(7%)), venpart$(c%)         , ch(25),~
               at (12,02), "Purchase Contract ID",                       ~
               at (12,24), fac(lfac$( 8%)), contract_id$(c%)  ,   ch(16),~
               at (12,42), "Line",                                       ~
               at (12,48), fac(lfac$( 8%)), contract_line$(c%),   ch( 4),~
               at (12,54), fac(hex(8c))  , contract_descr$      , ch(28),~
               at (13,02), "Vendor Qty/Unit",                            ~
               at (13,22), fac(lfac$(9%)), factor$              , ch(10),~
               at (13,36), "  INTERNAL",                                 ~
               at (14,02), "Quantity",                                   ~
               at (14,22), fac(lfac$(10%)), venqty$             , ch(10),~
               at (14,36), fac(lfac$(10%)), qty$(c%)            , ch(10),~
               at (14,48), fac(hex(8c))  , sn_used_msg$         , ch(30),~
               at (15,02), "Price",                                      ~
               at (15,22), fac(lfac$(11%)), venprice$           , ch(10),~
               at (15,36), fac(lfac$(11%)), price$(c%)          , ch(10),~
               at (16,02), "Extension",                                  ~
               at (16,22), fac(lfac$(12%)), ext$(c%)            , ch(10),~
               at (17,02), "Inventory cost (statutory)",                 ~
               at (17,36), fac(lfac$(13%)), invcost$            , ch(10),~
               at (17,47), fac(hex(8c))  , varmsg$              , ch(34),~
               at (18,02), "Purchase account",                           ~
               at (18,22), fac(lfac$(14%)), acct$(c%)           , ch(12),~
               at (18,39), fac(hex(8c))  , acctdescr$           , ch(32),~
               at (19,02), "Cost Variance Acct",                         ~
               at (19,22), fac(lfac$(15%)), varacct$(c%)        , ch(12),~
               at (19,39), fac(hex(8c))  , varacctdescr$        , ch(32),~
               at (20,02), "Job/Project #",                              ~
               at (20,22), fac(lfac$(16%)), job$(c%)            , ch(08),~
               at (20,39), fac(hex(8c))  , jobdescr$            , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,02), fac(hex(8c)), pfdescr$(1%)           , ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(2%)           , ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(3%)           , ch(79),~
                    keys(pfkeys$),                                       ~
                    key(keyhit%)

               infomsg$ = " "
               if keyhit% <> 13% then L44420
                  call "MANUAL" (manual$)
                  goto L43860

L44420:        if keyhit% <> 15% then L44460
                  call "PRNTSCRN"
                  goto L43860

L44460:        if linemode% = 0% then return
                  close ws
                  call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *       L I N E   S U M M A R Y   S C R E E N               *~
            * --------------------------------------------------------- *~
            * Line Item Summary Screen. (#5 if you're counting).        *~
            *************************************************************

            deffn_115
                init (hex(8e)) tfac$() : init (hex(84)) lfac$()
                init (hex(8c)) dfac$()
                pfdescr$(1%) = "(1)Start Over                            ~
        ~       (11)Insert     (13)Instructions"
                pfdescr$(2%) = "(2)First Line   (4)Prev   (6)Down One (9)~
        ~Header (12)Delete     (15)Print Screen"
                pfdescr$(3%) = "(3)Last Lines   (5)Next   (7)Up One   (25~
        ~)Text  (28)Delete All (16)Save Data"
                pfkeys$ = hex(000102040603050708090b0c0d0f10191c)

                REM Flip Off Appropriate Fields
                if maxlines% > 0% then L45210
                  str(pfdescr$(2%),49%,10%) = " " /* Shut Off Delete */
                  str(pfdescr$(3%),49%,14%) = " "/* Shut Off Delete All */
                  str(pfkeys$,12%,1%), str(pfkeys$,17%,1%) = hex(ff)
L45210:         if base% > 0% then L45240
                  str(pfdescr$(2%),,37%)  = " " /* Shut Off Prev Stuff */
                  str(pfkeys$,3%,3%) = hex(ffffff)
L45240:         if base%+13% < maxlines% then L45270
                  str(pfdescr$(3%),,37%)  = " " /* Shut Off Prev Stuff */
                  str(pfkeys$,6%,3%) = hex(ffffff)
L45270:         goto L45420

            deffn_125      /* Delete Lines From Memory */
                init (hex(8c)) lfac$(), tfac$(), dfac$()
                pfdescr$(1%) = "(1)Cancel Delete Request                 ~
        ~                      (13)Instructions"
                pfdescr$(2%) = "                                         ~
        ~                      (15)Print Screen"
                pfdescr$(3%) = "(ENTER) Delete Flashing Line(s)"
                pfkeys$ = hex(00010d0f10)
                if d% = 0% then init(hex(94)) lfac$(), tfac$(), dfac$()  ~
                           else lfac$(d%), tfac$(d%), dfac$(d%) = hex(94)

L45420:         header$(1%) = "Vendor: " & vencode$ & " " & vendescr$
                gosub set_up_descriptor
                gosub set_up_line3
                str(header$(2%),71%) = "Cur: " & currency$
                str(tfac$(), min(20, (maxlines%-base%)+1)) = all(hex(9c))
                str(pfdescr$(3%),63%,1%) = hex(84)

L45480:     accept                                                       ~
               at (01,02), "Invoice Line Item Summary",                  ~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(8c)), header$(1%)            , ch(79),~
               at (03,02), fac(hex(ac)), header$(2%)            , ch(79),~
               at (05,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), fac(hex(ae)), sumhdr$                , ch(79),~
                                                                         ~
               at (07,02), fac(tfac$( 1%)), seq$  (base%+ 1%)   , ch(03),~
               at (08,02), fac(tfac$( 2%)), seq$  (base%+ 2%)   , ch(03),~
               at (09,02), fac(tfac$( 3%)), seq$  (base%+ 3%)   , ch(03),~
               at (10,02), fac(tfac$( 4%)), seq$  (base%+ 4%)   , ch(03),~
               at (11,02), fac(tfac$( 5%)), seq$  (base%+ 5%)   , ch(03),~
               at (12,02), fac(tfac$( 6%)), seq$  (base%+ 6%)   , ch(03),~
               at (13,02), fac(tfac$( 7%)), seq$  (base%+ 7%)   , ch(03),~
               at (14,02), fac(tfac$( 8%)), seq$  (base%+ 8%)   , ch(03),~
               at (15,02), fac(tfac$( 9%)), seq$  (base%+ 9%)   , ch(03),~
               at (16,02), fac(tfac$(10%)), seq$  (base%+10%)   , ch(03),~
               at (17,02), fac(tfac$(11%)), seq$  (base%+11%)   , ch(03),~
               at (18,02), fac(tfac$(12%)), seq$  (base%+12%)   , ch(03),~
               at (19,02), fac(tfac$(13%)), seq$  (base%+13%)   , ch(03),~
                                                                         ~
               at (07,06), fac(lfac$( 1%)), part$ (base%+ 1%)   , ch(25),~
               at (08,06), fac(lfac$( 2%)), part$ (base%+ 2%)   , ch(25),~
               at (09,06), fac(lfac$( 3%)), part$ (base%+ 3%)   , ch(25),~
               at (10,06), fac(lfac$( 4%)), part$ (base%+ 4%)   , ch(25),~
               at (11,06), fac(lfac$( 5%)), part$ (base%+ 5%)   , ch(25),~
               at (12,06), fac(lfac$( 6%)), part$ (base%+ 6%)   , ch(25),~
               at (13,06), fac(lfac$( 7%)), part$ (base%+ 7%)   , ch(25),~
               at (14,06), fac(lfac$( 8%)), part$ (base%+ 8%)   , ch(25),~
               at (15,06), fac(lfac$( 9%)), part$ (base%+ 9%)   , ch(25),~
               at (16,06), fac(lfac$(10%)), part$ (base%+10%)   , ch(25),~
               at (17,06), fac(lfac$(11%)), part$ (base%+11%)   , ch(25),~
               at (18,06), fac(lfac$(12%)), part$ (base%+12%)   , ch(25),~
               at (19,06), fac(lfac$(13%)), part$ (base%+13%)   , ch(25),~
                                                                         ~
               at (07,32), fac(lfac$( 1%)), qty$  (base%+ 1%)   , ch(10),~
               at (08,32), fac(lfac$( 2%)), qty$  (base%+ 2%)   , ch(10),~
               at (09,32), fac(lfac$( 3%)), qty$  (base%+ 3%)   , ch(10),~
               at (10,32), fac(lfac$( 4%)), qty$  (base%+ 4%)   , ch(10),~
               at (11,32), fac(lfac$( 5%)), qty$  (base%+ 5%)   , ch(10),~
               at (12,32), fac(lfac$( 6%)), qty$  (base%+ 6%)   , ch(10),~
               at (13,32), fac(lfac$( 7%)), qty$  (base%+ 7%)   , ch(10),~
               at (14,32), fac(lfac$( 8%)), qty$  (base%+ 8%)   , ch(10),~
               at (15,32), fac(lfac$( 9%)), qty$  (base%+ 9%)   , ch(10),~
               at (16,32), fac(lfac$(10%)), qty$  (base%+10%)   , ch(10),~
               at (17,32), fac(lfac$(11%)), qty$  (base%+11%)   , ch(10),~
               at (18,32), fac(lfac$(12%)), qty$  (base%+12%)   , ch(10),~
               at (19,32), fac(lfac$(13%)), qty$  (base%+13%)   , ch(10),~
                                                                         ~
               at (07,43), fac(lfac$( 1%)), price$ (base%+ 1%)  , ch(10),~
               at (08,43), fac(lfac$( 2%)), price$ (base%+ 2%)  , ch(10),~
               at (09,43), fac(lfac$( 3%)), price$ (base%+ 3%)  , ch(10),~
               at (10,43), fac(lfac$( 4%)), price$ (base%+ 4%)  , ch(10),~
               at (11,43), fac(lfac$( 5%)), price$ (base%+ 5%)  , ch(10),~
               at (12,43), fac(lfac$( 6%)), price$ (base%+ 6%)  , ch(10),~
               at (13,43), fac(lfac$( 7%)), price$ (base%+ 7%)  , ch(10),~
               at (14,43), fac(lfac$( 8%)), price$ (base%+ 8%)  , ch(10),~
               at (15,43), fac(lfac$( 9%)), price$ (base%+ 9%)  , ch(10),~
               at (16,43), fac(lfac$(10%)), price$ (base%+10%)  , ch(10),~
               at (17,43), fac(lfac$(11%)), price$ (base%+11%)  , ch(10),~
               at (18,43), fac(lfac$(12%)), price$ (base%+12%)  , ch(10),~
               at (19,43), fac(lfac$(13%)), price$ (base%+13%)  , ch(10),~
                                                                         ~
               at (07,54), fac(lfac$( 1%)), ext$  (base%+ 1%)   , ch(10),~
               at (08,54), fac(lfac$( 2%)), ext$  (base%+ 2%)   , ch(10),~
               at (09,54), fac(lfac$( 3%)), ext$  (base%+ 3%)   , ch(10),~
               at (10,54), fac(lfac$( 4%)), ext$  (base%+ 4%)   , ch(10),~
               at (11,54), fac(lfac$( 5%)), ext$  (base%+ 5%)   , ch(10),~
               at (12,54), fac(lfac$( 6%)), ext$  (base%+ 6%)   , ch(10),~
               at (13,54), fac(lfac$( 7%)), ext$  (base%+ 7%)   , ch(10),~
               at (14,54), fac(lfac$( 8%)), ext$  (base%+ 8%)   , ch(10),~
               at (15,54), fac(lfac$( 9%)), ext$  (base%+ 9%)   , ch(10),~
               at (16,54), fac(lfac$(10%)), ext$  (base%+10%)   , ch(10),~
               at (17,54), fac(lfac$(11%)), ext$  (base%+11%)   , ch(10),~
               at (18,54), fac(lfac$(12%)), ext$  (base%+12%)   , ch(10),~
               at (19,54), fac(lfac$(13%)), ext$  (base%+13%)   , ch(10),~
                                                                         ~
               at (07,65), fac(dfac$( 1%)), po$   (base%+ 1%)   , ch(16),~
               at (08,65), fac(dfac$( 2%)), po$   (base%+ 2%)   , ch(16),~
               at (09,65), fac(dfac$( 3%)), po$   (base%+ 3%)   , ch(16),~
               at (10,65), fac(dfac$( 4%)), po$   (base%+ 4%)   , ch(16),~
               at (11,65), fac(dfac$( 5%)), po$   (base%+ 5%)   , ch(16),~
               at (12,65), fac(dfac$( 6%)), po$   (base%+ 6%)   , ch(16),~
               at (13,65), fac(dfac$( 7%)), po$   (base%+ 7%)   , ch(16),~
               at (14,65), fac(dfac$( 8%)), po$   (base%+ 8%)   , ch(16),~
               at (15,65), fac(dfac$( 9%)), po$   (base%+ 9%)   , ch(16),~
               at (16,65), fac(dfac$(10%)), po$   (base%+10%)   , ch(16),~
               at (17,65), fac(dfac$(11%)), po$   (base%+11%)   , ch(16),~
               at (18,65), fac(dfac$(12%)), po$   (base%+12%)   , ch(16),~
               at (19,65), fac(dfac$(13%)), po$   (base%+13%)   , ch(16),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfdescr$(1%)         , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(2%)         , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(3%)         , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key(keyhit%)

               if keyhit% <> 13% then L46620
                  call "MANUAL" (manual$)
                  goto L45480

L46620:        if keyhit% <> 15% then L46660
                  call "PRNTSCRN"
                  goto L45480

L46660:        close ws
               call "SCREEN" addr ("C", 3%, "I", i$(), cursor%())
               return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * Exits back to caller (PAYINPUT).                          *~
            *************************************************************

            end
