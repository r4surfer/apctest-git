************************************************************************~
*                           ( As of 08/15/92 - RHH )                   *~
*        APCEDISO - Print a Hardcopy of a Sales Order                  *~
*                                                                      *~
*                                                                      *~
*                                                                      *~
*       1/29/98 - Y2K Conversions                                  djd *~
*                                                                      *~
************************************************************************



        dim cust$9,                      /* APC Customer Code          */~
            so$8,                        /* APC Sales Order Number     */~
            ln$3,                        /* Sales Order Line Item No.  */~
            bck_key$25,                  /* BCKMASTER AND LINES KEY    */~
            sav_key$16,                  /*                            */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3,                    /* Current User Id            */~
            readkey$50                   /* Generic Lookup Key         */

        dim a01$20,         /* User ID, Date/Time, PGM or Print Flag   */~
            a02$9,          /* Customer Code                           */~
            a03$16,         /* Sales Order Mumber                      */~
            a04$16,         /* Purchase Order Number                   */~
            a05$(6)30,      /* Ship-To Name and Address                */~
            a06$(6)30,      /* Sold-To Name and Address                */~
            a07$20,         /* Payment Terms                           */~
            a08$20,         /* How Ship Information                    */~
            a09$20,         /* F.O.B. Information                      */~
            a10$(2)50,      /* Shipping Instructions                   */~
            a11$9,          /* Sales account number                    */~
            a12$9,          /* Discounts Account                       */~
            a13$(3)4,       /* Salesman Codes                          */~
            a14%(3),        /* Percentage of Sale credited to salesman.*/~
            a15$(3)6,       /* Percentage of Sale credited to salesman.*/~
            a16$4,          /* Region code                             */~
            a17$200,        /* Variable Fields                         */~
            a18$4,          /* Internal ID to text in TXTFILE.         */~
            a19$3,          /* Store Code                              */~
            a20$6,          /* Order Date                              */~
            a21$6,          /* Cancellation Date                       */~
            a22$6,          /* Due Date default                        */~
            a23$6,          /* Date Released                           */~
            a24$6,          /* Originally Input On (date)              */~
            a25$3,          /* Originally Input By (user)              */~
            a26$6,          /* Last Modified On (date)                 */~
            a27$3,          /* Last Modified By (user)                 */~
            a28$9,          /* Adjustment Reason Code                  */~
            a29$1,          /* Export Flag                             */~
            a30$1,          /* Pricing Code                            */~
            a31$10,         /* Order Discount Percent                  */~
            a32$10,         /* Open Order Amount                       */~
            a33$1,          /* Credit Hold Flag                        */~
            a34$10,         /* Last Sequence Number Used               */~
            a35$10,         /* Next BOL Number                         */~
            a36$2,          /* Customer Type Code                      */~
            a37$9,          /* Account X-Ref                           */~
            a38$4,          /* Currency code                           */~
            a39$104         /* Filler                                  */


        dim b01$9,          /* Customer Code                           */~
            b02$16,         /* Sales Order number                      */~
            b03$3,          /* Sequence Number                         */~
            b04$3,          /* Item Number                             */~
            b05$25,         /* Part Number                             */~
            b06$32,         /* Part Number description                 */~
            b07$4,          /* Category code                           */~
            b08$10,         /* Order Quantity                          */~
            b09$10,         /* Quantity Shipped (total)                */~
            b10$10,         /* Quantity Open                           */~
            b11$10,         /* Quantity scheduled for Shipment         */~
            b12$10,         /* Quantity Allocated                      */~
            b13$10,         /* Quantity Pre-Invoiced                   */~
            b14$10,         /* Unit Price @ Stocking UOM               */~
            b15$4,          /* Stocking UOM                            */~
            b16$4,          /* Pricing Unit of Measure                 */~
            b17$10,         /* Conversion Factor (Pricing to Stkng)    */~
            b18$10,         /* Unit Price                              */~
            b19$10,         /* Pricing Discount Percent                */~
            b20$1,          /* Taxable (y/n) indicator                 */~
            b21$9,          /* Sales Account number                    */~
            b22$9,          /* Discounts Account                       */~
            b23$6,          /* Due Date - Original                     */~
            b24$6,          /* Due Date - Current                      */~
            b25$6,          /* Required Ship Date                      */~
            b26$6,          /* Lot Number                              */~
            b27$8,          /* Code/# of a Project                     */~
            b28$8,          /* Filler                                  */~
            b29$1,          /* Demand Type                             */~
            b30$1,          /* Priority Code                           */~
            b31$4,          /* Internal ID to text in TXTFILE.         */~
            b32$1,          /* Allocation Flag                         */~
            b33$54          /* Filler                                  */

        dim f2%(64)                      /* File Open & Read Statuses  */


            select #1,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #2,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

             call "SHOSTAT" ("Opening Files, One Moment Please")
                  call "OPENCHCK" (#1, 0%, f2%(1),   0%, " ")
                  call "OPENCHCK" (#2, 0%, f2%(2),   0%, " ")

L10000: REM *************************************************************~
            *  S P E C I F Y   S A L E S   O R D E R   T O   P R I N T  *~
            *-----------------------------------------------------------*~
            * Report Selection Screen                                   *~
            *************************************************************

            u3% = 0%
            init(" ") userid$, rpt_time$, a01$, cust$, so$, ln$

            inpmessage$ = "Enter the Customer Code, Sales Order No., and ~
        ~Line Item No."
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                (12)Delete S.O.         " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffff0cff0e0f1000)


L10200:     accept                                                       ~
               at (01,02),                                               ~
                  "Specify Sales Order for Printing Utility",            ~
               at (01,66), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Enter APC Customer Code  :",                 ~
               at (05,30), fac(hex(81)), cust$                  , ch(09),~
                                                                         ~
               at (06,02), "Enter Sales Order Number :",                 ~
               at (06,30), fac(hex(81)), so$                    , ch(08),~
                                                                         ~
               at (07,02), "Enter Sales Order Line No.:",                ~
               at (07,30), fac(hex(81)), ln$                    , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L10470
                  call "PRNTSCRN"
                  goto L10200

L10470:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

            if keyhit% =  1% then goto L10000
            if keyhit% = 16% then goto exit_program

               errormsg$ = " "

               convert so$ to so%, data goto L10200

               convert so% to so$, pic(00000000)
               convert ln$ to ln%, data goto L10200

L10600:        convert ln% to ln$, pic(###)

            if keyhit% <> 12% then goto L10600
               gosub delete_so
            if keyhit% <> 14% then goto L10200

               gosub read_bckmastr
               if bckmastr% <> 1% then goto L10200

               gosub read_bcklines
               if bcklines% <> 1% then goto L10200

               gosub select_printer
                     gosub print_bckmastr
                     gosub print_bcklines
               gosub close_printer
            goto L10000                             /* Start Over Again */

        print_bckmastr
             print page
             print using L41680, cust$, "Header"
             print using L41690, so$  , date$
             print using L41700, ln$  , rpt_time$
             print
             print

        REM PRINT USING 41510,                                           ~
        REM          "User ID, Date/Time, PGM or Print Flag  ", A01$
            print using L41650,                                           ~
                     "Customer Code                          ", a02$
            print using L41650,                                           ~
                     "Sales Order Mumber                     ", a03$
            print using L41650,                                           ~
                     "Purchase Order Number                  ", a04$
            print using L41650,                                           ~
                     "Ship-To Name and Address               ", a05$(1%)
            print using L41650,                                           ~
                     "Sold-To Name and Address               ", a06$(1%)
            print using L41650,                                           ~
                     "Payment Terms                          ", a07$
            print using L41650,                                           ~
                     "How Ship Information                   ", a08$
            print using L41650,                                           ~
                     "F.O.B. Information                     ", a09$
            print using L41650,                                           ~
                     "Shipping Instructions                  ", a10$(1%)
            print using L41650,                                           ~
                     "Sales account number                   ", a11$
            print using L41650,                                           ~
                     "Discounts Account                      ", a12$
            print using L41650,                                           ~
                     "Salesman Codes                         ", a13$(1%)
            print using L41650,                                           ~
                     "Percentage of Sale credited to salesman", a15$(1%)
            print using L41650,                                           ~
                     "Region code                            ", a16$
            print using L41650,                                           ~
                     "Variable Fields                        ",          ~
                                                         str(a17$,1%,30%)
            print using L41650,                                           ~
                     "Internal ID to text in TXTFILE.        ", a18$
            print using L41650,                                           ~
                     "Store Code                             ", a19$
            print using L41650,                                           ~
                     "Order Date                             ", a20$
            print using L41650,                                           ~
                     "Cancellation Date                      ", a21$
            print using L41650,                                           ~
                     "Due Date default                       ", a22$
            print using L41650,                                           ~
                     "Date Released                          ", a23$
            print using L41650,                                           ~
                     "Originally Input On (date)             ", a24$
            print using L41650,                                           ~
                     "Originally Input By (user)             ", a25$
            print using L41650,                                           ~
                     "Last Modified On (date)                ", a26$
            print using L41650,                                           ~
                     "Last Modified By (user)                ", a27$
            print using L41650,                                           ~
                     "Adjustment Reason Code                 ", a28$
            print using L41650,                                           ~
                     "Export Flag                            ", a29$
            print using L41650,                                           ~
                     "Pricing Code                           ", a30$
            print using L41650,                                           ~
                     "Order Discount Percent                 ", a31$
            print using L41650,                                           ~
                     "Open Order Amount                      ", a32$
            print using L41650,                                           ~
                     "Credit Hold Flag                       ", a33$
            print using L41650,                                           ~
                     "Last Sequence Number Used              ", a34$
            print using L41650,                                           ~
                     "Next BOL Number                        ", a35$
            print using L41650,                                           ~
                     "Customer Type Code                     ", a36$
            print using L41650,                                           ~
                     "Account X-Ref                          ", a37$
            print using L41650,                                           ~
                     "Currency code                          ", a38$
            print using L41650,                                           ~
                     "Filler                                 ", a39$

        return

        print_bcklines

            print page
            print using L41680, cust$, "Detail"
            print using L41690, so$  , date$
            print using L41700, ln$  , rpt_time$
            print
            print

            print using L41650,                                           ~
                     "Customer Code                          ", b01$
            print using L41650,                                           ~
                     "Sales Order number                     ", b02$
            print using L41650,                                           ~
                     "Sequence Number                        ", b03$
            print using L41650,                                           ~
                     "Item Number                            ", b04$
            print using L41650,                                           ~
                     "Part Number                            ", b05$
            print using L41650,                                           ~
                     "Part Number description                ", b06$
            print using L41650,                                           ~
                     "Category code                          ", b07$
            print using L41650,                                           ~
                     "Order Quantity                         ", b08$
            print using L41650,                                           ~
                     "Quantity Shipped (total)               ", b09$
            print using L41650,                                           ~
                     "Quantity Open                          ", b10$
            print using L41650,                                           ~
                     "Quantity scheduled for Shipment        ", b11$
            print using L41650,                                           ~
                     "Quantity Allocated                     ", b12$
            print using L41650,                                           ~
                     "Quantity Pre-Invoiced                  ", b13$
            print using L41650,                                           ~
                     "Unit Price @ Stocking UOM              ", b14$
            print using L41650,                                           ~
                     "Stocking UOM                           ", b15$
            print using L41650,                                           ~
                     "Pricing Unit of Measure                ", b16$
            print using L41650,                                           ~
                     "Conversion Factor (Pricing to Stkng)   ", b17$
            print using L41650,                                           ~
                     "Unit Price                             ", b18$
            print using L41650,                                           ~
                     "Pricing Discount Percent               ", b19$
            print using L41650,                                           ~
                     "Taxable (y/n) indicator                ", b20$
            print using L41650,                                           ~
                     "Sales Account number                   ", b21$
            print using L41650,                                           ~
                     "Discounts Account                      ", b22$
            print using L41650,                                           ~
                     "Due Date - Original                    ", b23$
            print using L41650,                                           ~
                     "Due Date - Current                     ", b24$
            print using L41650,                                           ~
                     "Required Ship Date                     ", b25$
            print using L41650,                                           ~
                     "Lot Number                             ", b26$
            print using L41650,                                           ~
                     "Code/# of a Project                    ", b27$
            print using L41650,                                           ~
                     "Filler                                 ", b28$
            print using L41650,                                           ~
                     "Demand Type                            ", b29$
            print using L41650,                                           ~
                     "Priority Code                          ", b30$
            print using L41650,                                           ~
                     "Internal ID to text in TXTFILE.        ", b31$
            print using L41650,                                           ~
                     "Allocation Flag                        ", b32$
            print using L41650,                                           ~
                     "Filler                                 ", b33$

        return

L41650: % ########################################     ##################~
        ~############

L41680: % APC Customer Code            : #########   Rec Type: (########)
L41690: % APC Sales Order Number       : ########    Date    :  ##########
L41700: % APC Sales Order Line Item No.: ###         Time    :  ########

        read_bckmastr
            call "SHOSTAT" ("Reading (BCKMASTR) Header Info.")

            init(" ") readkey$ : bckmastr% = 0%
            str(readkey$,1%,9%)   = cust$
            str(readkey$,10%,16%) = so$
            read #1,key = readkey$, eod goto L50560
               get #1, using L60190, a02$,                                ~
                                    a03$,                                ~
                                    a04$,                                ~
                                    a05$(),             /* (6) AT (30) */~
                                    a06$(),             /* (6) AT (30) */~
                                    a07$,                                ~
                                    a08$,                                ~
                                    a09$,                                ~
                                    a10$(),             /* (2) AT (50) */~
                                    a11$,                                ~
                                    a12$,                                ~
                                    a13$(),             /* (3) AT (4)  */~
                                    a14%(),                              ~
                                    a16$,                                ~
                                    a17$,                                ~
                                    a18$,                                ~
                                    a19$,                                ~
                                    a20$,                                ~
                                    a21$,                                ~
                                    a22$,                                ~
                                    a23$,                                ~
                                    a24$,                                ~
                                    a25$,                                ~
                                    a26$,                                ~
                                    a27$,                                ~
                                    a28$,                                ~
                                    a29$,                                ~
                                    a30$,                                ~
                                    a31,                                 ~
                                    a32,                                 ~
                                    a33$,                                ~
                                    a34%,                                ~
                                    a35%,                                ~
                                    a36$,                                ~
                                    a37$,                                ~
                                    a38$,                                ~
                                    a39$

           convert a14%(1%) to a15$(1%), pic(###.##)
           convert a14%(2%) to a15$(2%), pic(###.##)
           convert a14%(3%) to a15$(3%), pic(###.##)

           convert a31 to a31$, pic(######.##-)
           convert a32 to a32$, pic(######.##-)

           convert a34% to a34$, pic(######)
           convert a35% to a35$, pic(######)
/* <<<<<<<<<< Y2K >>>>>>>>>> */
	   /* Order header */
	   call "DATEFMT" (a20$)			/* Order Date        */
	   call "DATEFMT" (a21$)			/* Cancellation Date */
 	   call "DATEFMT" (a22$)			/* Due Date          */
	   call "DATEFMT" (a23$)			/* Date Released     */
           call "DATEFMT" (a24$)			/* Input Date        */
           call "DATEFMT" (a26$)			/* Modified Date     */

	   /* Details */
	   call "DATEFMT" (b23$)			/* Due Date (Orig)   */
	   call "DATEFMT" (b24$)			/* Due Date (Curr)   */
	   call "DATEFMT" (b25$)			/* Reqd Ship Date    */

/* <<<<<<<<<< Y2K >>>>>>>>>> */
           bckmastr% = 1%
        return
L50560:    errormsg$="(Error)-Reading Sales Order-"&str(readkey$,1%,25%)
        return

        read_bcklines
            call "SHOSTAT" ("Reading (BCKLINES) Detail Info.")
            bcklines% = 0%
            init(" ") readkey$
            str(readkey$,1%,16%) = so$
            str(readkey$,17%,3%) = ln$

            read #2,key = readkey$, eod goto L51150
               get #2, using L60580, b01$,                                ~
                                    b02$,                                ~
                                    b03$,                                ~
                                    b04$,                                ~
                                    b05$,                                ~
                                    b06$,                                ~
                                    b07$,                                ~
                                    b08,                                 ~
                                    b09,                                 ~
                                    b10,                                 ~
                                    b11,                                 ~
                                    b12,                                 ~
                                    b13,                                 ~
                                    b14,                                 ~
                                    b15$,                                ~
                                    b16$,                                ~
                                    b17,                                 ~
                                    b18,                                 ~
                                    b19,                                 ~
                                    b20$,                                ~
                                    b21$,                                ~
                                    b22$,                                ~
                                    b23$,                                ~
                                    b24$,                                ~
                                    b25$,                                ~
                                    b26$,                                ~
                                    b27$,                                ~
                                    b28$,                                ~
                                    b29$,                                ~
                                    b30$,                                ~
                                    b31$,                                ~
                                    b32$,                                ~
                                    b33$

           convert b08 to b08$, pic(######.##-)
           convert b09 to b09$, pic(######.##-)
           convert b10 to b10$, pic(######.##-)
           convert b11 to b11$, pic(######.##-)
           convert b12 to b12$, pic(######.##-)
           convert b13 to b13$, pic(######.##-)
           convert b14 to b14$, pic(######.##-)


           convert b17 to b17$, pic(######.##-)
           convert b18 to b18$, pic(######.##-)
           convert b19 to b19$, pic(######.##-)
           bcklines% = 1%
        return
L51150:    errormsg$="(Error)-Reading Line Item - "&str(readkey$,1%,19%)
        return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            call "SHOSTAT" ("Printing a Sales Order")
            call "TIME" (rpt_time$)
            date$ = date
/* <<<<<<<<<< Y2K >>>>>>>>>> */
            call "DATFMTC" (date$)
/* <<<<<<<<<< Y2K >>>>>>>>>> */
            call "SETPRNT" ("APCSO", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCSO", " ", 0%, 1%)
        return

        REM CH(20),         /* User ID, Date/Time, PGM or Print Flag   */~

L60190: FMT                 /* FILE #1  -- BCKBUFFR                    */~
            CH(9),          /* Customer Code                           */~
            CH(16),         /* Sales Order Mumber                      */~
            CH(16),         /* Purchase Order Number                   */~
            6*CH(30),       /* Ship-To Name and Address                */~
            6*CH(30),       /* Sold-To Name and Address                */~
            CH(20),         /* Payment Terms                           */~
            CH(20),         /* How Ship Information                    */~
            CH(20),         /* F.O.B. Information                      */~
            2*CH(50),       /* Shipping Instructions                   */~
            CH(9),          /* Sales account number                    */~
            CH(9),          /* Discounts Account                       */~
            3*CH(4),        /* Salesman Codes                          */~
            3*BI(1),        /* Percentage of Sale credited to salesman.*/~
            CH(4),          /* Region code                             */~
            CH(200),        /* Variable Fields                         */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(3),          /* Store Code                              */~
            CH(6),          /* Order Date                              */~
            CH(6),          /* Cancellation Date                       */~
            CH(6),          /* Due Date default                        */~
            CH(6),          /* Date Released                           */~
            CH(6),          /* Originally Input On (date)              */~
            CH(3),          /* Originally Input By (user)              */~
            CH(6),          /* Last Modified On (date)                 */~
            CH(3),          /* Last Modified By (user)                 */~
            CH(9),          /* Adjustment Reason Code                  */~
            CH(1),          /* Export Flag                             */~
            CH(1),          /* Pricing Code                            */~
            PD(14,4),       /* Order Discount Percent                  */~
            PD(14,4),       /* Open Order Amount                       */~
            CH(1),          /* Credit Hold Flag                        */~
            BI(2),          /* Last Sequence Number Used               */~
            BI(4),          /* Next BOL Number                         */~
            CH(2),          /* Customer Type Code                      */~
            CH(9),          /* Account X-Ref                           */~
            CH(4),          /* Currency code                           */~
            CH(104)         /* Filler                                  */

L60580: FMT                 /* FILES #2        -- BCKBUF2 AND BCKLINES */~
            CH(9),          /* Customer Code                           */~
            CH(16),         /* Sales Order number                      */~
            CH(3),          /* Sequence Number                         */~
            CH(3),          /* Item Number                             */~
            CH(25),         /* Part Number                             */~
            CH(32),         /* Part Number description                 */~
            CH(4),          /* Category code                           */~
            PD(14,4),       /* Order Quantity                          */~
            PD(14,4),       /* Quantity Shipped (total)                */~
            PD(14,4),       /* Quantity Open                           */~
            PD(14,4),       /* Quantity scheduled for Shipment         */~
            PD(14,4),       /* Quantity Allocated                      */~
            PD(14,4),       /* Quantity Pre-Invoiced                   */~
            PD(14,4),       /* Unit Price @ Stocking UOM               */~
            CH(4),          /* Stocking UOM                            */~
            CH(4),          /* Pricing Unit of Measure                 */~
            PD(14,7),       /* Conversion Factor (Pricing to Stkng)    */~
            PD(14,4),       /* Unit Price                              */~
            PD(14,4),       /* Pricing Discount Percent                */~
            CH(1),          /* Taxable (y/n) indicator                 */~
            CH(9),          /* Sales Account number                    */~
            CH(9),          /* Discounts Account                       */~
            CH(6),          /* Due Date - Original                     */~
            CH(6),          /* Due Date - Current                      */~
            CH(6),          /* Required Ship Date                      */~
            CH(6),          /* Lot Number                              */~
            CH(8),          /* Code/# of a Project                     */~
            CH(8),          /* Filler                                  */~
            CH(1),          /* Demand Type                             */~
            CH(1),          /* Priority Code                           */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(1),          /* Allocation Flag                         */~
            CH(54)          /* Filler                                  */

        exit_program

        end

        delete_so
           call "SHOSTAT" ("DELETING S.O. (" &so$& ")" )
           stop " STILL CAN ABORT"
           bck_key$ = " "
           str(bck_key$,1%,9%)   = cust$
           str(bck_key$,10%,16%) = so$
           read #1,hold,key = bck_key$, eod goto L61180
              delete #1

           bck_key$ = " "
           str(bck_key$,1%,16%) = so$
           sav_key$ = str(bck_key$,1%,16%)

L61100:    read #2,hold,key > bck_key$, using L61120, bck_key$,           ~
                                                     eod goto L61160
L61120:       FMT POS(10), CH(19)
           if str(bck_key$,1%,16%) <> sav_key$ then goto L61160
              delete #2
              goto L61100
L61160: return clear all
        goto L10200
L61180:    stop " S.O. NOT DELETED -----> " & so$
        return clear all
        goto L10200
