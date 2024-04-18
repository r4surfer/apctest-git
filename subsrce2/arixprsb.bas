        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   AAA   RRRR   IIIII  X   X  PPPP   RRRR    SSS   BBBB    *~
            *  A   A  R   R    I     X X   P   P  R   R  S      B   B   *~
            *  AAAAA  RRRR     I      X    PPPP   RRRR    SSS   BBBB    *~
            *  A   A  R   R    I     X X   P      R   R      S  B   B   *~
            *  A   A  R   R  IIIII  X   X  P      R   R   SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARIXPRSB - Prints customer EXPORT invoices based on the   *~
            *            data passed from the calling program.          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/08/87 ! Original  (Clone of ARIPRSUB)            ! MJB *~
            * 12/23/87 ! Multi-currency print mods                ! MJB *~
            * 08/25/88 ! Invoice totals expressed in tran currency! JIM *~
            * 09/29/88 ! Price now not formatted via CURRFMT      ! JDH *~
            * 10/06/89 ! MC 'off' mods to suppress currency labels! JDH *~
            *          !  Also, fixed 1st line of pmnt schedule,  !     *~
            *          !  fmted Zip, & fixed serial # printing.   !     *~
            * 10/19/89 ! Discounts & Linextension now print on 2nd! MLJ *~
            *          !  detail line(s). (Same as ARIPRSUB)      !     *~
            * 02/27/90 ! MC 'off' mods.  Revisited.               ! JDH *~
            * 05/03/90 ! Added CUSTOMER file to enable program    ! MJB *~
            *          !  to print 'Sold To' address if other     !     *~
            *          !  'Ship To'.                              !     *~
            * 11/06/91 ! PRRs 10356,11272. Honor ARMTERMS Flag for! JDH *~
            *          !  Printing Terms Description on Invoice.  !     *~
            * 03/09/93 ! PRR 12233 Fix header shift with respect  ! JIM *~
            *          !   to ARIPRSUB (avoid alignment problems).!     *~
            * 12/08/94 ! PRR 13323.  Price printed honoring flag  ! JDH *~
            *          !  pertaining to discounts.                !     *~
            *          ! PRR 13327.  Fixed rpt id sent to text prt!     *~
            * 03/14/95 ! PRR 13187 - Print Xref Parts if exist &  ! RJH *~
            *          !   BCK Flags indicate to print.           !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "ARIXPRSB" (dup%, prt%, abend%, cust_code$, invoice$, rptid$)

                /* DUP% equal to 1 indicates a duplicate invoice       */
                /* PRT% is set to 1 if any invoice is printed          */
                /* ABEND% is set to 1 if errors suggest 'ABORT'        */
                /* CUST_CODE$ is the passed customer code              */
                /* INVOICE$ is the associated Invoice # to print       */
                /* RPTID$ is the passed report ID                      */

        REM *************************************************************~
            *This subroutine will print a customer invoice based on the *~
            *customer code and invoice number passed to it.             *~
            *************************************************************

        dim                                                              ~
            amtdue$10,                   /* Edited amount due          */~
            aribuf2_key$20,              /* Key to ARIBUF2             */~
            aribuffr_key$17,             /* Key to ARIBUFFR            */~
            arimscur_key$17,             /* Key to ARIMSCUR            */~
            bol$3,                       /* Bill of lading fr ARIBUFFR */~
            billxref$9,                  /* Bill-to Cross reference    */~
            billship$(6)31,              /* Bill-to XREF Ship Address  */~
            billsold$(6)31,              /* Bill-to XREF Sold Address  */~
            crmemo$24,                   /* Credit memo text           */~
            currency$4, currdesc$32,     /* Currency code              */~
            curr$1,                      /* Currency on flag           */~
            cust_code$9,                 /* Customer code from ARIINVRF*/~
            desc$57,                     /* Descriptions for totals    */~
            disc$7,                      /* Edited percentages         */~
            discount$10,                 /* Edited line discount amount*/~
            discsw$1,                    /* Controls discount functions*/~
            duplicate$24,                /* Duplicate invoice text     */~
            fmttotal$15,                 /* Formatted amounts for print*/~
            fob$20,                      /* F.O.B. from ARIBUFFR       */~
            frt_bill$20,                 /* Frt bill from ARIBUFFR     */~
            how_ship$20,                 /* How to ship from ARIBUFFR  */~
            inv_date$8,                  /* Invoice date from ARIBUFFR */~
            inv_print$1,                 /* Print ARMTERMS Descr?      */~
            invoice$8,                   /* Invoice # from ARIINVRF    */~
            mask$85,                     /* Text mask                  */~
            lineid$4,                    /* ARIBUF2  Line ID for S/N's */~
            linextension$12,             /* Formatted line extension   */~
            part_nmbr$25,                /* Part number from ARIBUF2   */~
            part_desc$32,                /* Part descr from ARIBUF2    */~
            po$16,                       /* PO number from ARIBUFFR    */~
            price$10,                    /* Edited unit price          */~
            price_uom$4,                 /* Pricing UOM from ARIBUF2   */~
            prog$8,                      /* Program name               */~
            qty_ordr$10,                 /* Edited open quantity       */~
            qty_ship$10,                 /* Edited order quantity      */~
            rptid$6,                     /* Report ID from caller      */~
            seqnr$3,                     /* ARI Sequence Number        */~
            ser$6,                       /* Serial # literal           */~
            serprint$76,                 /* Serial # print area        */~
            serial$20,                   /* Serial # from SERMASTR     */~
            serkey$99,                   /* SERMASTR key               */~
            ship_to$(6)31,               /* Ship to from ARIBUFFR      */~
            so$16,                       /* Sales Order # from ARIBUFFR*/~
            sold_to$(6)31,               /* Sold to from ARIBUFFR      */~
            stax$12,                     /* Edited sales tax base amt  */~
            stock_uom$4,                 /* Stocking UOM from ARIBUF2  */~
            stor_code$3,                 /* Store # from ARIBUFFR      */~
            taxable$3,                   /* Taxable code description   */~
            term_amt(30),                /* Terms amount due (ARIBUFFR)*/~
            term_code$20,                /* Terms code from ARIBUFFR   */~
            term_date$(30)8,             /* Terms date from ARIBUFFR   */~
            term_desc$41,                /* Terms descripion           */~
            term_hdr$31,                 /* Terms descripion (header)  */~
            term_net$(30)8,              /* Terms net date fr ARIBUFFR */~
            term_pct(30),                /* Terms discount % (ARIBUFFR)*/~
            txbl$1,                      /* Taxable code from ARIBUF2  */~
            txtid$4,                     /* Text ID- ARIBUFFR/ARIBUF2  */~
            xref_cus$1,                  /* Print Customer Xref Part   */~
            xref_mnf$1,                  /* Print Manufactur Xref Part */~
            xref_descr$32,               /* Xref Part # Description    */~
            xref_part$25,                /* Xref Part Number           */~
            xref_type$1                  /* Xref Part Type(Cust or Mnf)*/


        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #02 ! SYSFILE2 ! Caelus Management System General Informa *~
            * #4  ! ARIBUFFR ! Invoice Buffer file                      *~
            * #5  ! ARIBUF2  ! Invoice Line Items Buffer file           *~
            * #6  ! TXTFILE  ! System text file                         *~
            * #7  ! HNYMASTR ! Inventory Master File                    *~
            * #8  ! ARMTERMS ! A/R Payment Terms                        *~
            * #9  ! SERTIF   ! Serial Number Tracking  TIF   File       *~
            * #10 ! ARILNCUR ! Currency-specific ARI lines              *~
            * #11 ! CURMASTR ! Multi-Currency Master file               *~
            * #12 ! ARIMSCUR ! Currency-specific ARI Master             *~
            * #13 ! CUSTOMER ! Customer Master File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #02,  "SYSFILE2",                                     ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #4,  "ARIBUFFR",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  17,                     ~
             alt key 1, keypos = 2001, keylen =  24

            select #5,  "ARIBUF2",                                       ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =   1, keylen =  20

            select #6,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =   1, keylen =  11

            select #7, "HNYMASTR", varc, indexed, recsize = 900,         ~
                        keypos =   1, keylen =  25,                      ~
                    alt key 1, keypos = 102, keylen =  9, dup,           ~
                        key 2, keypos =  90, keylen =  4, dup

            select #8, "ARMTERMS", varc, indexed, recsize = 100,         ~
                        keypos =   1,  keylen = 20

            select #09, "SERTIF",                                        ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos = 1, keylen = 62

            select #10, "ARILNCUR",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   5,  keylen = 20,                      ~
                        alt key  1, keypos =   1, keylen =  24

            select #11, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            select #12, "ARIMSCUR",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =   5,  keylen = 17,                      ~
                        alt key  1, keypos =   1, keylen =  21

            select #13,  "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            * Initialization code is done one time only.                *~
            *************************************************************

            if been_here_before% <> 0% then goto L10000
            been_here_before% = 1%
            abend% = 1% /* Abnormal terminate if errors during OPENs */
            rslt$(4 ) = "REQUIRED"
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))
                if fs%(4) < 0 then exit_program
            rslt$(5 ) = "REQUIRED"
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ), 0%, rslt$(5 ))
                if fs%(5) < 0 then exit_program
            rslt$(13) = "REQUIRED"
            call "OPENCHCK" (#13,  fs%(13), f2%(13), 0%, rslt$(13))
                if fs%(13) < 0 then exit_program
            call "OPENCHCK" (#02, 0%, f2%(2 ),   0%, " ")
                curr$ = "N"   /* Test for multi-currency */
                call "READ100" (#02, "SWITCHS.CUR", f1%(2))
                if f1%(2) <> 0% then get #02 using L09200, curr$
L09200:             FMT POS(21), CH(1)
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ), 0%, rslt$(6 ))
            call "OPENCHCK" (#7,  fs%(7 ), f2%(7 ), 0%, rslt$(7 ))
            call "OPENCHCK" (#8,  fs%(8 ), f2%(8 ), 0%, rslt$(8 ))
            call "OPENCHCK" (#9,  fs%(9 ), f2%(9 ), 0%, rslt$(9 ))
            if curr$ <> "Y" then L09290
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
            call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, rslt$(12))

L09290:     call "EXTRACT" addr("CF", prog$)
            select printer (87)
            call "SETPRNT" (rptid$, prog$, 0%, 0%)

            max_lines% = 35%
            call "BCKSWTCH" ("BCK", "DISCS   ", discsw$, disc1or2, comp%)
            discsw$ = "N"
            if disc1or2 = 2 then discsw$ = "Y"

            call "READ100" (#02, "SWITCHS.BCK", f1%(02%))
            if f1%(02%) = 1% then get #02 using L09410, xref_cus$, xref_mnf$
L09410:         FMT POS(63), CH(1), CH(1)

        REM CHECK XREF PRINT SETTINGS
            call "READ100" (#13, cust_code$, f1%(13%))
            if f1%(13%) = 0% then L09980     /* Shouldn't happen */
            get #13 using L09470 , temp_cus$, temp_mnf$
L09470:         FMT POS(1092), CH(1), CH(1)

            if temp_cus$ = " " then L09520
                if pos("YNB" = temp_cus$) <> 0% then xref_cus$ = temp_cus$

L09520:     if temp_mnf$ = " " then L09980
                if pos("YNB" = temp_mnf$) <> 0% then xref_mnf$ = temp_mnf$

L09980:     call "SHOSTAT" ("Printing EXPORT Customer Invoices")

L10000: REM *************************************************************~
            *             M A I N   P R O G R A M   L O O P             *~
            *-----------------------------------------------------------*~
            *  The following code is executed once for each call of this*~
            *  subroutine -- which is equivalent to a single invoice.   *~
            *************************************************************

            abend% = 0% /* Indicate normal (no error) processing    */
            invtaxable = 0
            duplicate$, currdesc$ = " "
            if dup% <> 0% then duplicate$ = "THIS IS A DUPLICATE COPY"

            arimscur_key$, aribuffr_key$ = str(cust_code$,,9) & invoice$
            call "READ100" (#4, aribuffr_key$, f1%(4))
            if f1%(4) = 0% then goto exit_program
            get #4 using L10460, po$, so$, bol$, ship_to$(), sold_to$(),  ~
                how_ship$, fob$, frt_bill$, inv_date$, invgrossamt,      ~
                invdiscpct, invdiscamt, invfrtamt, invstaxamt, invnetamt,~
                billxref$, stor_code$, invstaxpct, txtid$, term_code$,   ~
                term_amt(), term_pct(), term_date$(), term_net$(),       ~
                currency$

        REM Partial record layout for file 'ARIBUFFR' *******************
L10460:         FMT  POS(18), CH(16),    /* Purchase Order number      */~
                     POS(34), CH(16),    /* Sales Order number         */~
                     POS(50), CH( 3),    /* Bill of lading             */~
                     POS(53), 6*CH(30),  /* Ship-to name & address     */~
                     POS(233), 6*CH(30), /* Sold-to name & address     */~
                     POS(419), CH(20),   /* How shipped (shipped via)  */~
                     POS(439), CH(20),   /* FOB                        */~
                     POS(481), CH(20),   /* Freight bill number        */~
                     POS(521), CH( 6),   /* Invoice date               */~
                     POS(793), PD(14,4), /* Gross invoice amount       */~
                     POS(801), PD(14,4), /* Invoice discount percent   */~
                     POS(809), PD(14,4), /* Invoice discount amount    */~
                     POS(817), PD(14,4), /* Freight amount             */~
                     POS(825), PD(14,4), /* Sales tax amount           */~
                     POS(833), PD(14,4), /* Net invoice amount         */~
                     POS(849), CH( 9),   /* Bill-to Cross Reference #  */~
                     POS(870), CH( 3),   /* Store code                 */~
                     POS(883), PD(14,4), /* Sales tax percent          */~
                     POS(901), CH( 4),   /* Text ID (header)           */~
                     POS(908), CH(20),   /* Terms code                 */~
                     POS(928), 30*PD(14,4),/* Terms amounts due        */~
                     POS(1168), 30*PD(14,4),/* Terms cash discount pct */~
                     POS(1408), 30*CH(6),/* Terms discount due dates   */~
                     POS(1588), 30*CH(6),/* Terms net due              */~
                     POS(1779), CH(4)    /* Currency Code              */

            if term_code$ = "DATED" then L11060
              grace% = 0%
              call "ARMTCDTE" (term_code$, inv_date$, term_pct(1),       ~
                               term_date$(1), term_net$(1), grace%)
L11060:       if currency$ = " " or curr$ <> "Y" then goto L11140
                call "DESCRIBE" (#11, currency$, currdesc$, 1%, f1%(11))
                if currdesc$ = " " then currdesc$ = "(" & currency$ & ")"

L11140: REM Now replace invoice totals with transaction currency amounts,~
            if header record exists in ARIMSCUR.
            if curr$ <> "Y" then L11340
            call "READ100" (#12, arimscur_key$, f1%(12))
                if f1%(12) = 0% then goto L11340
            get #12 using L11300, invgrossamt, invdiscamt, invfrtamt,     ~
                invstaxamt, invnetamt, term_amt()

L11300:         FMT POS(22), 5*PD(14,4), POS(92), 30*PD(14,4)

L11340:     prt% = 1% /* Indicate print activity */
            call "DATEFMT" (inv_date$)
            term_hdr$ = "SEE SCHEDULE BELOW"
            if term_code$ = "DATED" then goto do_addresses
                amtdue$, term_desc$ = " " : t% = 1%
                call "READ100" (#8, term_code$, f1%(8))
                if f1%(8) <> 1% then L11440
                     get #8 using L11428, term_desc$, inv_print$
L11428:                   FMT POS(21), CH(30), POS(77), CH(1)
                     if inv_print$ = "Y" then L11460
                term_desc$ = " "
L11440:         gosub term_descriptor
L11460:         call "STRING" addr("LJ", term_desc$, 41%)
                term_hdr$ = term_desc$
        do_addresses
            if sold_to$(1) = " " then sold_to$() = ship_to$()
            if sold_to$(1) <> "BILL-TO" then compress_address
            call "READ100" (#13, billxref$, f1%(13))
                if f1%(13) = 1% then L11511
                     sold_to$() = ship_to$()
                     goto compress_address
L11511:         get #13 using L11512, billsold$(), billship$()
L11512:              FMT POS(40), 6*CH(30), POS(253), 6*CH(30)
                if billsold$(1) = " " or billsold$(1) = "BILL-TO" then   ~
                   sold_to$() = billship$() else sold_to$() = billsold$()

        compress_address
            if str(ship_to$(6),17,1) <> " " or str(ship_to$(6),16,1)     ~
               <> " " or pos(str(ship_to$(6),27,4) = " ") > 0% then L11620
                  temp$ = str(ship_to$(6),27,4)
                  str(ship_to$(6),28,4) = temp$
                  str(ship_to$(6),27,1) = "-"
L11620:     call "LINSMASH" (ship_to$())
            if str(sold_to$(6),17,1) <> " " or str(sold_to$(6),16,1)     ~
               <> " " or pos(str(sold_to$(6),27,4) = " ") > 0% then L11740
                  temp$ = str(sold_to$(6),27,4)
                  str(sold_to$(6),28,4) = temp$
                  str(sold_to$(6),27,1) = "-"
L11740:     call "LINSMASH" (sold_to$())

            page_nbr% = 0%
            gosub page_heading
            txt% = (page_nbr% * 100%) + nbr_lines%
            gosub'200(txtid$)
            if txt% = (page_nbr% * 100%) + nbr_lines% then L11920
               REM Text printed, so add blank line after it...
               print : nbr_lines% = nbr_lines% + 1%
L11920:     aribuf2_key$ = key(#4) /* Customer code and Invoice number*/
            str(aribuf2_key$,18) = all(hex(00))

        aribuf2_loop
            call "PLOWNEXT" (#5, aribuf2_key$, 17%, f1%(5))
            if f1%(5) = 0% then goto end_of_lines
            get #5 using L12130, seqnr$, part_nmbr$, part_desc$, qty_ordr,~
                qty_ship, stock_prc, stock_uom$, price_uom$, disc,       ~
                lindiscamt, linextension, txbl$, txtid$, lineid$

        REM Partial record layout for file 'ARIBUF2' ********************
L12130:         FMT  POS(18), CH( 3),    /* ARI Sequence Number        */~
                     POS(24), CH(25),    /* Part number                */~
                     POS(49), CH(32),    /* Part description           */~
                     POS(85), PD(14,4),  /* Original order quantity    */~
                     POS(93), PD(14,4),  /* Quantity shipped           */~
                     POS(109), PD(14,4), /* Unit price @ stocking      */~
                     POS(117), CH( 4),   /* Stocking UOM               */~
                     POS(121), CH( 4),   /* Pricing UOM                */~
                     POS(141), PD(14,4), /* Line discount %            */~
                               PD(14,4), /* Line discount amount       */~
                     POS(157), PD(14,4), /* Line extension             */~
                     POS(165), CH( 1),   /* Taxable code               */~
                     POS(190), CH( 4),   /* Text ID (detail line)      */~
                     POS(626), CH( 4)    /* S/N Line ID                */

            if curr$ <> "Y" then L12520
            call "READ100" (#10, aribuf2_key$, f1%(10))
            if f1%(10) <> 0% then get #10 using L12480, stock_prc,        ~
                                          lindiscamt, linextension
L12480:         FMT POS(25), PD(14,4), POS(41), PD(14,4), PD(14,4)

L12520:     if qty_ship = 0 then goto aribuf2_loop
            if so$ = " " then qty_ordr = qty_ship
            if discsw$ = "Y" then L12560
                stock_prc = stock_prc - ((stock_prc * disc) / 100)
L12560:     call "CONVERT" (qty_ordr, 2.2, qty_ordr$)
            call "CONVERT" (qty_ship, 2.2, qty_ship$)
            call "CONVERT" (stock_prc, 2.4, price$)
            if curr$ = "Y" then call "CURRFMT"                           ~
                (linextension, currency$, linextension$, "N")            ~
                else call "CONVERT" (linextension, 2.2, linextension$)
            discount$ = " "
            if discsw$ = "N"  then L12780
            if lindiscamt = 0 then L12780
            if curr$ = "Y" then                                          ~
                call "CURRFMT" (lindiscamt, currency$, discount$, "N")   ~
                else call "CONVERT" (lindiscamt, 2.2, discount$)
L12780:     taxable$ = "NO"
            if txbl$ <> "Y" then goto L12846
                taxable$ = "YES"
                invtaxable = invtaxable + linextension
                invtaxable = invtaxable + linextension

L12846:      /* Get Xref Part Number, if available */
             call "PTUSEDSB" ( "R", "ARI ",                              ~
                              str(aribuf2_key$,,17%), seqnr$,            ~
                              xref_part$, xref_descr$, xref_type$, ret%)
             if ret% = 0% then xref_part$,xref_descr$,xref_type$ = " "

            if max_lines% - nbr_lines% < 2% then gosub page_heading
            print using L62600, part_nmbr$, stock_uom$, qty_ordr$,        ~
                price_uom$
            print using L62400, part_desc$, taxable$, qty_ship$, price$,  ~
                discount$, linextension$
            nbr_lines% = nbr_lines% + 2%

            if xref_type$ = "M" and xref_mnf$ = "B" then L12963 else L12966
L12963:         print using L63240, "Manufactor Part No.:", xref_part$,   ~
                                    "(" & xref_descr$ & ")"
                goto L12969             /* Iterate */
L12966:     if xref_type$ = "C" and xref_cus$ = "B" then L12967 else L12980
L12967:         print using L63240, "Customer Part No.:", xref_part$,     ~
                                    "(" & xref_descr$ & ")"
L12969:         nbr_lines% = nbr_lines% + 1%

L12980:     gosub serial_number_print
            gosub'200(txtid$)

        REM Print the part text (if any) from the 'HNYMASTR' file *******
            call "READ100" (#7, part_nmbr$, f1%(7))
                if f1%(7) = 0% then L13140
            get #7 using L13120, txtid$ : gosub'200(txtid$)
L13120:     FMT POS(98), CH(4)      /* HNYMASTR part text ID      */
L13140:     print : nbr_lines% = nbr_lines% + 1%
            goto aribuf2_loop

        end_of_lines
            if nbr_lines% > max_lines% then gosub page_heading
            print using L63210
            nbr_lines% = nbr_lines% + 1%
            if curr$ = "Y" then                                          ~
                desc$ = currency$ & " " & currdesc$ & " INVOICE TOTAL:"  ~
                else desc$ = " INVOICE TOTAL:"
            amount = invgrossamt
            if curr$ = "Y" then                                          ~
                call "CURRFMT" (amount, currency$, fmttotal$, "Y")       ~
                else call "CONVERT" (amount, 2.2, fmttotal$)
            gosub total_printer
            if invdiscamt = 0 then goto try_sales_tax
                convert invdiscpct to disc$, pic (-###.##)
                desc$ = disc$ & "% INVOICE DISCOUNT:"
                amount = invdiscamt
                if curr$ = "Y" then                                      ~
                    call "CURRFMT" (amount, currency$, fmttotal$, "N")   ~
                    else call "CONVERT" (amount, 2.2, fmttotal$)
                gosub total_printer
        try_sales_tax
            if invstaxamt = 0 then goto try_freight
                taxabledsc = round(invtaxable * invdiscpct * .01, 2)
                invtaxable = round(invtaxable - taxabledsc, 2)
                convert invstaxpct to disc$, pic (-##.###)
                if curr$ = "Y" then                                      ~
                    call "CURRFMT" (invtaxable, currency$, stax$, "N")   ~
                    else call "CONVERT" (invtaxable, 2.2, stax$)
                amount = invstaxamt
                desc$ = disc$ & "% SALES TAX ON " &                      ~
                     str(stax$, pos(stax$ <> " ")) & ":"
                if curr$ = "Y" then                                      ~
                    call "CURRFMT" (amount, currency$, fmttotal$, "N")   ~
                    else call "CONVERT" (amount, 2.2, fmttotal$)
                gosub total_printer
        try_freight
            if invfrtamt = 0 then goto try_net_amount
                amount = invfrtamt
                if curr$ = "Y" then                                      ~
                    call "CURRFMT" (amount, currency$, fmttotal$, "N")   ~
                    else call "CONVERT" (amount, 2.2, fmttotal$)
                desc$ = "FREIGHT CHARGES:"
                gosub total_printer
        try_net_amount
            if invnetamt = invgrossamt then goto exit_program
                amount = invnetamt
                if curr$ = "Y" then                                      ~
                    call "CURRFMT" (amount, currency$, fmttotal$, "Y")   ~
                    else call "CONVERT" (amount, 2.2, fmttotal$)
                desc$ = "INVOICE NET AMOUNT:"
                gosub total_printer
            goto exit_program

        page_heading
            page_nbr% = page_nbr% + 1% : nbr_lines% = 0%
            if page_nbr% > 1% then print using L62710,                    ~
                "     >>>>>  C O N T I N U E D  <<<<<    ", " *CONTINUED*"
            print page
            print skip(2)
            print using L60050, crmemo$
            print using L60050, duplicate$
            print skip (2)
            print using L60070, " ", invoice$, inv_date$, page_nbr%
            print skip(3) /* SKIP TO SOLD-TO, SHIP-TO PORTION OF FORM */
            for s% = 1% to 6%
                print using L60090, sold_to$(s%), ship_to$(s%)
            next s%
            if curr$ <> "Y"                                              ~
                then print using L61000, " ", "CUSTOMER NUMBER: " &       ~
                     cust_code$                                          ~
                else print using L61000, " CURRENCY: " & currency$ & " " &~
                     currdesc$, "CUSTOMER NUMBER: " & cust_code$
            print skip(2)
            print using L62000, term_hdr$, po$, so$, bol$
            print skip(2)
            print using L62200, how_ship$, fob$, frt_bill$
            print skip(2) /* SKIP TO BODY OF FORM */
            if term_code$ <> "DATED" then return
            if page_nbr% > 1% then return
            term_hdr$ = "SEE SCHEDULE ON PAGE 1"
            print using L62310, "----------- PAYMENT SCHEDULE ------------"

            nbr_lines% = nbr_lines% + 1%
            for t% = 1% to 30%
                if term_amt(t%) = 0% then goto L15040
                   if curr$ = "Y" then call "CURRFMT"                    ~
                       (term_amt(t%), currency$, amtdue$, "N")           ~
                       else call "CONVERT" (term_amt(t%), 2.2, amtdue$)
                   term_desc$ = amtdue$
                   gosub term_descriptor
                   print using L62310, term_desc$
                   nbr_lines% = nbr_lines% + 1%
L15040:     next t%
            term_desc$ = all("-")
            print using L62310, term_desc$
            nbr_lines% = nbr_lines% + 1%
            ser$ = "S/N'S:"
            return

        term_descriptor
            if term_pct(t%) = 0 then goto net_terms
               convert term_pct(t%) to disc$, pic (-###.##)
               call "DATEFMT" (term_date$(t%))
               term_desc$ = amtdue$ & disc$ & "% " & term_date$(t%) & ","

        net_terms
            call "DATEFMT" (term_net$(t%))
            str(term_desc$, len(term_desc$)+1%) = " NET " & term_net$(t%)
            return

        total_printer
            if nbr_lines% > max_lines% then gosub page_heading
            call "STRING" addr ("RJ", desc$, len(str(desc$)))
            print using L63000, desc$, fmttotal$
            nbr_lines% = nbr_lines% + 1%
            return

        serial_number_print
            ser$ = "S/N'S:" : serkey$, serprint$ = " " : ser% = 0%
            serkey$ = "RT" & str(cust_code$) & str(invoice$) &           ~
                str(lineid$) & hex(00)
        plow_serial_tif
            call "PLOWALTS" (#9, serkey$, 0%, 23%, f1%(9))
            if f1%(9) = 0% then goto plow_serial_done
                get #9 using L15700, serial$
L15700:              FMT  /* SERTIF #9  */ POS(43), CH(20)
                if len(serprint$) + len(serial$) + ser% >                ~
                     len(str(serprint$)) then gosub serial_print
                str(serprint$, len(serprint$) + ser%) = serial$
                ser% = 2%
                goto plow_serial_tif

        plow_serial_done
            if ser% <> 0% then gosub serial_print
            return

        serial_print
            if nbr_lines% >= max_lines% then gosub page_heading
            print using L63110, ser$, serprint$
            ser$, serprint$ = " " : ser% = 0%
            nbr_lines% = nbr_lines% + 1%
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            *      PRINT TEXT FOR EITHER THE HEADER OR DETAIL LINES     *~
            *************************************************************

            deffn'200(txtid$)
            if txtid$ = hex(ffffffff) then return
            if txtid$ = " " then return
                comp% = 0%
L50090:         call "TXTPRINT" (#6, f2%(6),  87%, txtid$, rptid$, 5%,   ~
                     nbr_lines%, max_lines%, "N", mask$, comp%)
                if comp% = 0% then return
                     gosub page_heading
                     goto L50090

        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************

L60050: %                                                          ######~
        ~##################
L60070: %          ##############################                  ######~
        ~####### ######## ###
L60090: %          ###############################            ###########~
        ~####################
L61000: % ##################################################  ###########~
        ~####################
L62000: % ###############################  ################   ###########~
        ~##### - ############
L62200: % ##############################   ####################   #######~
        ~####################
L62310: % #########################################
L62400: % ################################ #### ########## ########## ###~
        ~####### -####,###.##

L62600: % ################################ #### ########## ########## ###~
        ~#

L62710: % ###############################################################~
        ~####    ############

L63000: % ###############################################################~
        ~#### ###############

L63110: % ###### ########################################################~
        ~####################

L63210: %                                                                ~
        ~     ---------------

L63240: %        ################### #########################  #########~
        ~#########################

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            end
