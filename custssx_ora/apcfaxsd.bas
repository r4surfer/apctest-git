        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - APCFAXSD                             *~
            *  Creation Date     - 11/07/96                             *~
            *  Last Modified Date- 03/16/06                             *~
            *  Written By        - Roy H. Hoffman                       *~
            *                                                           *~
            *  Description       - Send a Sales Order Acknowledgement   *~
            *                      FAX to an APC Customer.              *~
            *                                                           *~
            *        Note        - Called from (BCKFASTR) and (JBPOST2) *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCFAXSD - Prints an acknowledgement for any order sent to*~
            *            this routine from Order Entry.  Resulting S.O. *~
            *            Acknowlegdement is routed to Faximum.          *~
            *                                                           *~
            *          - ERROR% = 0% - Fax Ok and Sent                  *~
            *                     1% - Could Not Open Fax Print File    *~
            *                     2% - No Fax Phone No. for Customer    *~
            *                     3% - No Sales Order on File           *~
            *                     4% - Shell Script Error               *~
            *                     5% - Print Error Creating Fax         *~
            *                     6% - Could Not Reset Table Flag       *~
            *                                                           *~
            * Subroutine - Called by BCKFASTR                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/10/96 ! Original                                 ! JBF *~
            * 04/03/97 ! Mod for New Series Naming using Subroutin! RHH *~
            *          !   APCPRZSB. Replaced ( by APCPRYSB )     !     *~
            * 11/13/97 ! Mod for Upgrade to New Release R6.04.03  ! RHH *~
            *          !                                          !     *~
            * 03/31/98 ! Y2K modifications                        ! ERN *~
            * 10/02/98 ! Mods for /*RightFax*/                DJD & BWS *~
            * 10/12/98 ! (EWD001) Mod <TONAME> to include S.O. No.! BWS *~
            * 11/10/98 ! (EWD002) Mod for Private Label           ! RHH *~  
            * 05/07/02 ! (EWD003) Mods to get description from    ! CMG *~
            *          !          Oracle.                         !     *~
            * 03/04/04 ! (EWD004) Mods to for the new version of  ! CMG *~
            *          !          Rightfax                        !     *~
            * 09/20/04 ! (AWD005) Mod for new Energy Surcharge    ! CMG *~
            * 03/16/06 ! (AWD006) - modification for North East   ! CMG *~
            * 07/06/06 ! (AWD007) - modification for ESC fix      ! CMG *~
            * 10/06/06 ! (AWD008) - mod to print text on ack if   ! CMG *~
            *          !   any, currently only prints if more than!     *~
            *          !   five characters                        !     *~
            *************************************************************

        sub "APCFAXSD" (customer_no$,    /* Customer No.               */~
                        so$,             /* S.O. No.                   */~
                        userid$,         /* User Who Entered Order     */~
                        error%,          /* Error Flag from File Open  */~
                        #1,              /* GENCODES Channel           */~
                        #4,              /* BCKMASTR Channel           */~
                        #5,              /* BCKLINES Channel           */~
                        #6,              /* TXTFILE  Channel           */~
                        #7,              /* CUSTOMER Channel           */~
                        #8,              /* ARMTERMS Channel           */~
                        #9,              /* HNYMASTR Channel           */~
                        #11)             /* BOMSPEC  Channel           */

        dim                                                              ~
            a$80,                        /* Print Line on S.O. Ack     */~
            apc$(3%)30,                  /* Descriptions (1) thru (3)  */~
            apc_scr$120,                 /* Screen Description         */~
            apc_prt$60,                  /* Print Description          */~
            apc_sze$20,                  /* Size Description           */~
            grp_due$10,                  /* GROUP DUE DATE             */~
            bck_key$25,                  /* Key to BCKMASTR            */~
            bcklines_key$19,             /* Key to BCKLINES            */~
            bill_to$(6%)31,              /* Bill-to name from CUSTOMER */~
            billto_xref$9,               /* Bill-to xref from BCKMASTR */~
            comp_pn$25,                  /* COMPONENT PART NUMBER      */~
            currency$4,                  /* Currency Code              */~
            customer_no$9,               /* Customer code              */~
            cust_name$30,                /* Customer name              */~
            date$10,                     /* Date for display & print   */~
            desc$64,                     /* Descriptions for totals    */~
            discount$10,                 /* Edited line discount amount*/~
            discsw$1,                    /* Controls discount functions*/~
            due_curr$8,                  /* Original due date- BCKLINES*/~
            extension$10,                /* Edited line extension amt  */~
            energy$14,                   /* Energy Surcharge  (AWD005) */~
            fax_code$2,                  /* GENCODES Code for Time     */~
            fax_key$24,                  /* Save File Key              */~
            fax_phone$11,                /* CUSTOMER Phone No.         */~
            fax_style$15,                /* Fax Style String           */~
            fax_time$12,                 /* Fax Send Time String       */~
            file$8,                      /* ACK Print File             */~
            fob$20,                      /* F.O.B. from BCKMASTR       */~
            fmtamount$14,                /* Formatted currency amount  */~
            how_ship$20,                 /* How to ship from BCKMASTR  */~
            library$8,                   /* Library Name = APCDATA     */~
            linedisc$30,                 /* Edited line discount       */~
            optkey$90,                   /* Work Variable              */~
            optkey1$83,                  /* Work Variable              */~
            optkey2$90,                  /* Work Variable              */~
            orderdate$10,                /* Order date from BCKMASTR   */~
            orderdisc$14,                /* Edited order discount      */~
            orddisclit$24,               /* Order Discount Literal     */~
            page_nbr$4,                  /* Page number                */~
            part_nmbr$25,                /* Part number from BCKLINES  */~
            part_desc$250,               /* Part descr from BCKLINES   */~
            po$16,                       /* PO number from BCKMASTR    */~
            po_line$3,                   /* PO line # from BCKLINES    */~
            price$10,                    /* Edited unit price          */~
            price_uom$4,                 /* Pricing UOM from BCKMASTR  */~
            qty_open$10,                 /* Edited open quantity       */~
            qty_ordr$10,                 /* Edited order quantity      */~
            readkey$24,                  /* Used by CHECK_PRINT Routine*/~
            s_23m$3,                     /* Model Code for Series      */~
            s_23$8,                      /* New Series Code            */~
            s_so$8,                      /* Sales order Code   (EWD002)*/~
            s_ln$3,                      /* S. O.              (EWD002)*/~
            s_prv$30,                    /* Private Label Name (EWD002)*/~
            s_1$2,                       /* Private Label Code (EWD002)*/~     
            script$8,                    /* FAX SHELL SCRIPT           */~
            ship_to$(6%)31,              /* Ship to from BCKMASTR      */~
            shipinstr$(2%)50,            /* Ship instructions- BCKMASTR*/~
            shipping$9,                  /* Shipping literal           */~
            so$16,                       /* Sales Order # from BCKMASTR*/~
            so_bck$16,                   /* Sales Order # from BCKLINES*/~
            so_seqnr$3,                  /* SO sequence # from BCKLINES*/~
            sv_seqnr$3,                  /* Save SO sequence number    */~
            sold_to$(6%)31,              /* Sold to from BCKMASTR      */~
            stock_uom$4,                 /* Stocking UOM from BCKMASTR */~
            stor_code$3,                 /* Store # from BCKMASTR      */~
            term_code$20,                /* Terms code from BCKMASTR   */~
            term_desc$30,                /* Terms descrip from ARMTERMS*/~
            text_fields$(2%)20,          /* Variable Text from CUSTOMER*/~
            txtid$4, text$(2%)70,        /* Common Text ID for Lookup  */~
            userid$3,                    /* User ID                    */~
            volume$6                     /* DISK VOLUME = CARLOS       */

        dim f2%(20%),                    /* = 0 if the file is open    */~
            f1%(20%),                    /* = 1 if READ was successful */~
            fs%(20%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(20%)20                 /* Text from file opening     */

        dim                              /*  (EWD003)                  */~
            opt_seqnr$3,                 /* Sequence Num for OPT       */~
            error$256,                   /* Error String               */~
            field$256,                   /* Query Return Field         */~
            stmt1$250,                   /* First Query String         */~
            stmt2$250,                   /* Second Query String        */~
            fields$(4%)100               /* String of Oracle Info      */

        dim schema$8                     /* (AWD006) schema            */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Fax Sales Order Acknowledgement   "
            pname$ = "APCFAXSD - Rev: R6.04"

        REM *************************************************************
            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! GENCODES ! Master Code Table File                   *~
            * #4  ! BCKMASTR ! Backlog Master File (Get Store Number)   *~
            * #5  ! BCKLINES ! Back Log Line Item File                  *~
            * #6  ! TXTFILE  ! System Text File                         *~
            * #7  ! CUSTOMER ! Customer Master File                     *~
            * #8  ! ARMTERMS ! A/R Payment Terms Codes                  *~
            * #9  ! HNYMASTR ! Inventory Master File                    *~
            * #10 ! HNYOPTNS ! Option Parts Replacements List           *~
            * #11 ! BOMSPEC  ! Options selected file                    *~
            * #12 ! HNYOPTN2 ! Option Parts Replacements List II        *~
            * #15 ! AMTBOMIF ! Master Validity File For BOM Generator   *~
            *************************************************************~
            * #20 ! FAXSOA?? ! Faximum Print File For S.O.Acknowlegement*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #10, "HNYOPTNS",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos = 1, keylen = 54

            select #12, "HNYOPTN2",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos = 26, keylen = 58,                        ~
                        alternate key 1, keypos = 112, keylen = 56, dup

            select #15, "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =  1,   keylen =  32

            select #20, "ROYAL", consec,  recsize =  80

            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%), 0%, rslt$(12%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%), 0%, rslt$(15%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            error%          =  0% : counter% = 0%
            nbr_lines%      =  0% : ora_lines% = 0%
            date$           =  date
            discsw$         = "N"
            /* EWD RIGHTFAX max_lines%      =  38% */
 	    max_lines%      = 33%
            library$        = "APCDATA "
            volume$         = "CARLOS"
            call "DATFMTC"  (date$)
            call "BCKSWTCH" ("BCK", "DISCS   ", discsw$, disc1or2, comp%)
                if disc1or2 = 2               then discsw$ = "Y"

* (AWD006) Next 3 lines
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #1, schema_err%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        REM INPUTMODE
            init(" ") due_curr$, po_line$, price_uom$, so_bck$, stock_uom$
            gosub set_file_name
                if error% <> 0%               then exit_sub
            gosub open_file
            gosub begin_process

            gosub setup_fax                         /* (EWD004)  */
            goto exit_sub

        set_file_name
REM            file$ = "TSTFAX"
REM            return

            init(" ") fax_key$, desc$, file$, script$
            str(fax_key$,1%,9%)   = "PLAN FAXF"
            str(fax_key$,10%,15%) = "FAXSOA"

        check_next_file
            read #1,hold,key > fax_key$, using L10210, fax_key$, desc$,   ~
                eod goto check_next_done
L10210:         FMT CH(24), CH(30)
            if str(fax_key$,1%,15%) <> "PLAN FAXFFAXSOA" then            ~
                    goto check_next_done
            if str(desc$,1%,1%) <> "-"                   then            ~
                    goto check_next_file
            put #1, using L10280, "*"
            rewrite #1
L10280:         FMT POS(25), CH(1)

            file$       = str(fax_key$,10%,8%)
            script$     = str(desc$,3%,8%)
        return

        check_next_done
            counter%    = counter% + 1%
            if counter% < 4% then goto set_file_name
            error%      = 1%                       /* EXIT TRY LATER */
        return

        open_file
            open nodisplay #20, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return

        REM *************************************************************~
            *                P R O C E S S   D A T A                    *~
            *************************************************************
        begin_process
            init(" ") bck_key$
            config% = 0%
            str(bck_key$, 1%, 9) = customer_no$
            str(bck_key$,10%,16%) = so$
            read #4, key = bck_key$, using L35030, customer_no$, so$, po$,~
                ship_to$(), sold_to$(), term_code$, how_ship$, fob$,     ~
                shipinstr$(), txtid$, stor_code$, orderdate$, grp_due$,  ~
                odiscpercent, billto_xref$, currency$, energy$,          ~
                     eod goto no_order

            call "DATFMTC" (orderdate$)
            call "DATFMTC" (grp_due$)                       /* (EWD003) */
            energy = 0.00
            if str(energy$,1%,14%) = " " then goto no_energy
 
               get str(energy$,1%,8%) using L19170, energy
L19170:                 FMT PD(14,4)
         no_energy
            gosub check_print


            init(" ") cust_name$, text_fields$()
            read #7,key = customer_no$, using L19180,                     ~
                cust_name$, text_fields$(), eod goto no_fax
L19180:         FMT POS(10), CH(30), POS(820), 2*CH(20)

            if str(text_fields$(2%),1%,2%) <> "00"  then prep_fax
        no_fax
                error% = 2% : goto exit_sub
        no_order
                error% = 3% : goto exit_sub

        prep_fax
REM         gosub setup_fax                              /*  (EWD004)  */

            if  sold_to$()    = " "           then                       ~
                sold_to$()    =  ship_to$()
            if  sold_to$(1%) <> "BILL-TO"     then smasher
            if  billto_xref$  =  customer_no$ then smasher
            read #7,key = billto_xref$, using L19350,                     ~
                sold_to$(), bill_to$(),   eod goto smasher
L19350:         FMT POS(40), 6*CH(30), POS(253), 6*CH(30)

            if  sold_to$()    = " "           then                       ~
                sold_to$()    =  bill_to$()
            if  sold_to$(1%)  = "BILL-TO"     then                       ~
                sold_to$()    =  bill_to$()

        smasher
            if  str(ship_to$(6%),27%) <> " "  then ship_to$(6%) =        ~
                str(ship_to$(6%),,26%) & "-" & str(ship_to$(6%),27%,4%)
            call "SPCESMSH" (ship_to$(6%), 2%)
            if  str(sold_to$(6%),27%) <> " "  then sold_to$(6%) =        ~
                str(sold_to$(6%),,26%) & "-" & str(sold_to$(6%),27%,4%)
            call "SPCESMSH" (ship_to$(6%), 2%)
            call "LINSMASH" (ship_to$())
            call "LINSMASH" (sold_to$())
            read #8,key = term_code$, using L19530, term_desc$,           ~
                eod goto use_term_code
L19530:         FMT POS(21), CH(30)

            if term_desc$ <> " "              then bckmastr_continue

        use_term_code
            term_desc$ = term_code$

        bckmastr_continue
            page_nbr%  = 0%
            ordertotal = 0
            gosub page_heading
            gosub'200(txtid$)

            gosub oracle_flush
            init(" ") stmt1$, stmt2$, txtid$
            str(stmt1$,1%,24%)   = "CALL MSSQL.RPT_DISPLAY('" 
            str(stmt1$,25%,20%)   = so$  & "', '" & userid$ & "')"

            gosub oracle_exec                              /* (EWD003) */
            if oci_err% <> 0% then goto delete_fax            

            init(" ") stmt1$
            str(stmt1$,1%,40%)   = "SELECT * FROM MSSQL.DISPLAY WHERE SALESO" 
            str(stmt1$,41%,32%)  = "RDER = '"&str(so$,1%,8%)&"' AND USERID = '" 
            str(stmt1$,73%,34%)  =  userid$ & "' ORDER BY DISPLAYORDER ASC, LI"
            str(stmt1$,107%,40%) = "NENUMBER ASC, CONFIG DESC, UNITID ASC"

            gosub oracle_flush
            gosub oracle_query                             /* (EWD003) */



        bcklines_loop
            if config% <> 0% then goto line_continue
            gosub oracle_fetch                             /* (EWD003) */
            if oci_err% > 0% and oci_err% < 100% then goto delete_fax
            if oci_err% = 100% then goto end_of_lines
        line_continue
            qty_ordr, qty_open, price_prc, extension = 0.00
            init(" ") qty_ordr$, qty_open$, price$, extension$

            gosub load_newline

            convert str(fields$(),368%,12%) to extension, data goto L19535
L19535:

            linedisc$         = " "
            discount          =  0
            if discsw$        = "N"           then net_extension
            if ldiscpercent   =  0            then bcklines_continue
            convert ldiscpercent to linedisc$, pic(-###.##)

            str(linedisc$,8%) = "% DISCOUNT"
            discount          = -(extension * ldiscpercent) / 100
            call "CURRFMT" (discount, currency$, discount$, "N")
            goto bcklines_continue

        net_extension
            extension = extension - ((extension * ldiscpercent) / 100)

        bcklines_continue
            part_desc$ = str(fields$(),32%,250%)
            convert str(fields$(),26%,3%) to qty_ordr, data goto L19540
L19540:
                                                   /* Sub Quantities */
            convert str(fields$(),29%,3%) to qty_open, data goto L19550
L19550:
            convert str(fields$(),356%,12%) to price_prc, data goto L19560
L19560:
            call "CONVERT" (qty_ordr, 0, qty_ordr$)
            call "CONVERT" (qty_open, 0, qty_open$)
            call "CONVERT" (price_prc, 2.4, price$)
            call "CONVERT" (extension, 2.2, extension$)

            if  linedisc$   = " "                                        ~
            and max_lines%  - nbr_lines% < 4% then gosub page_heading
            if  linedisc$  <> " "                                        ~
            and max_lines%  - nbr_lines% < 5% then gosub page_heading
            call "STRING" addr ("LJ", so_seqnr$,  len(str(so_seqnr$)))
            call "STRING" addr ("RJ", qty_ordr$,  len(str(qty_ordr$)))
            call "STRING" addr ("RJ", qty_open$,  len(str(qty_open$)))
            call "STRING" addr ("RJ", price$,     len(str(price$)))
            call "STRING" addr ("RJ", extension$, len(str(extension$)))
            a$, txtid$      = all(hex(20))              /* (EWD003) */
            str(a$, 1%, 3%) = so_seqnr$
            apc_prt$ = str(fields$(),282%,50%)
            gosub print_desc

            if str(fields$(),4%,1%) = "0" then goto print_detail_text
               config% = 1%

               gosub get_config





        print_detail_text
            gosub print_options

            spacer%         = 0%
            txtid$ = str(fields$(),380%,4%)
            gosub'200(txtid$)


            ordertotal      = ordertotal + extension + discount

            goto skip_text
            read #9,key     = part_nmbr$, using L20610, txtid$,           ~
                eod goto skip_text
L20610:         FMT POS(98), CH(4)      /* HNYMASTR part text ID      */

            gosub'200(txtid$)

        skip_text
            a$              = all(hex(20))
            if oci_err%     > 0%               then goto end_of_lines
            if spacer%     <> 0%               then bcklines_loop
            gosub print_line


            goto bcklines_loop

        end_of_lines
REM -- Trying to fix blank faxes
            so_seqnr% = 0%
            convert so_seqnr$ to so_seqnr%, data goto delete_fax

            if str(so_seqnr$,1%,3%) = " " then goto delete_fax
            if nbr_lines% = 0% and page_nbr% <= 1% then goto delete_fax
            if ora_lines% = 0% and page_nbr% <= 1% then goto delete_fax
REM -- Trying to fix blank faxes
/*  (AWD005)  - BEGIN   */
            if prt% = 0% then goto do_discount
               if energy <= 0.00 then goto do_total
               energytotal = ordertotal * (energy / 100)
               call "CURRFMT" (energytotal, currency$, energy$, "Y")
               desc$       = "ENERGY UPCHAGE COST:" 
               call "STRING" addr ("RJ", desc$,      len(str(desc$)))
               call "STRING" addr ("RJ", fmtamount$, len(str(energy$)))
               if max_lines%   - nbr_lines% < 2%  then gosub page_heading
               a$              = all(hex(20))
               str(a$, 2%,64%) = desc$
               str(a$,66%,14%) = energy$            
               gosub print_line
               ordertotal = ordertotal + energytotal
        do_total
/*  (AWD005)  -  END    */

            call "CURRFMT" (ordertotal, currency$, fmtamount$, "Y")
            desc$           = "SALES ORDER TOTAL :"
            if prt%         = 0%               then do_discount
            call "STRING" addr ("RJ", desc$,      len(str(desc$)))
            call "STRING" addr ("RJ", fmtamount$, len(str(fmtamount$)))
            if max_lines%   - nbr_lines% < 2%  then gosub page_heading
            a$              = all(hex(20))
            str(a$,66%,14%) = "______________"      /*EWD RightFax*/
            gosub print_line

            a$              = all(hex(20))
            str(a$, 2%,64%) = desc$
            str(a$,66%,14%) = fmtamount$            /*EWD RightFax*/
            gosub print_line

            a$              = all(hex(20))

        do_discount
            if odiscpercent = 0             then return
            discount = -round((ordertotal * odiscpercent) / 100, 2)
            convert odiscpercent to orddisclit$, pic(-###.##)

            str(orddisclit$,8) = "% ORDER DISCOUNT:"
            call "CURRFMT" (discount, currency$, orderdisc$, "N")
            ordtotal           = ordertotal + discount
            call "CURRFMT" (ordtotal, currency$, fmtamount$, "Y")
            desc$              = "NET SALES ORDER:"
            call "STRING" addr ("RJ", desc$,      len(str(desc$)))
            call "STRING" addr ("RJ", fmtamount$, len(str(fmtamount$)))
            call "STRING" addr ("RJ", orderdisc$, len(str(orderdisc$)))
            if prt%         = 0%            then return
            if nbr_lines% > max_lines%      then gosub page_heading
            a$              = all(hex(20))
            str(a$,41%,24%) = orddisclit$
            str(a$,66%,14%) = orderdisc$            /*EWD RightFax*/
            gosub print_line

            if nbr_lines%   > max_lines%    then gosub page_heading
            a$              = all(hex(20))
            str(a$, 2%,64%) = desc$
            str(a$,66%,14%) = fmtamount$            /*EWD RightFax*/
            gosub print_line

        return

        get_config
            init(" ") sv_seqnr$
            sv_seqnr$ = so_seqnr$
        next_config
            so_seqnr% = 0%
            gosub oracle_fetch
            if oci_err% > 0% then goto no_config

            if max_lines%  - nbr_lines% < 4% then gosub page_heading
            field_num% = 5%
            gosub oracle_getfield
            str(so_seqnr$,1%,3%) = str(field$,1%,3%)

            convert so_seqnr$ to so_seqnr%, data goto delete_fax

            if sv_seqnr$ <> so_seqnr$ then goto no_config
            init(" ") part_desc$, qty_open$, apc_prt$
            a$ = all(hex(20))

            field_num% = 9%
            gosub oracle_getfield
            str(qty_open$,1%,3%) = str(field$,1%,3%)
            convert qty_open$ to qty_open, data goto L20150
L20150:
            call "STRING" addr ("RJ", qty_open$,  len(str(qty_open$)))

            field_num% = 10%
            gosub oracle_getfield
            part_desc$ = str(field$,1%,250%)
            field_num% = 11%
            gosub oracle_getfield
            apc_prt$ = str(field$,1%,50%)

            gosub print_desc

            a$              = all(hex(20))
            if config% = 0% then str(a$,10%,50%) = apc_prt$ ~
            else str(a$,12%,50%) = apc_prt$
            if str(a$,1%,80%) <> " " then gosub print_line

            field_num% = 16%
            gosub oracle_getfield
            text_lne% = 0%
            convert str(field$,1%,len(field$)) to text_lne%, data goto next_lne_config
            str(field$,1%,4%) = BIN(text_lne%,4)
        next_lne_config
            gosub'200(field$)

            goto next_config
            
        no_config
        return

        print_desc
REM            if config% <> 0% then            ~
                   call "STRING" addr ("LJ", qty_open$,  len(str(qty_open$)))
            if config% = 0% then str(a$,4%, 3%)  = str(qty_ordr$,8%,3%)  ~
            else                 str(a$,7%, 3%)  = str(qty_open$,8%,3%)      

            if config% = 0% then str(a$,10%,45%) = str(part_desc$,1%,45%) ~
            else                 str(a$,12%,45%) = str(part_desc$,1%,45%)

            if config% = 0% then str(a$,55%,1%)  = "-"  ~
            else                 str(a$,57%,1%)  = "-"  

            if prt% <> 1%  or config% = 1%          then L20160
            str(a$,60%, 9%) = str(price$,2%,9%)
            str(a$,71%,10%) = extension$
L20160:     gosub print_line
            ora_lines% = ora_lines% + 1%

            pos% = 46%
            for i% = 1% to 5%
                a$              = all(hex(20))
                if config% = 0% then str(a$,10%,45%) = str(part_desc$,pos%,45%) ~
                else str(a$,12%,45%) = str(part_desc$,pos%,45%)
                if config% = 0% then str(a$,55%,1%)  = "-"  ~
                else str(a$,57%,1%)  = "-"
                if str(a$,10%,45%) <> " " then gosub print_line
                if str(a$,10%,45%) <> " " then ora_lines% = ora_lines% + 1%
                pos% = pos% + 45%
            next i%

REM         SIZE GOES HERE
            if str(fields$(),4%,1%) <> "0" then goto L20430
            a$              = all(hex(20))
            if config% = 0% then str(a$,10%,50%) = apc_prt$ ~
            else str(a$,12%,50%) = apc_prt$
            if str(a$,1%,80%) <> " " then gosub print_line
            if str(a$,1%,80%) <> " " then ora_lines% = ora_lines% + 1%

L20430:     a$              = all(hex(20))
            if linedisc$    = " " then return
            call "STRING" addr ("RJ", discount$, len(str(discount$)))
            str(a$, 8%,30%) = linedisc$
            str(a$,71%,10%) = discount$
            gosub print_line
            ora_lines% = ora_lines% + 1%

            a$              = all(hex(20))

        return

        deffn'200(txtid$)
            if txtid$       = hex(ffffffff)      then return
            if txtid$       = " "                then return
            txt%            = (page_nbr% * 100%) + nbr_lines%
            comp%           = 0%
            call "APCPLTXT" (#6, txtid$, text$(), comp% )
            if comp%        = 0%                 then L21380
   /* (AWD008) */
                if len(text$(1%))   <= 0         then L21310
                     a$     = all(hex(20))
                     str(a$,8%,70%) = "TXT: " & str(text$(1%),1%,65%)
                     gosub print_line

                     if nbr_lines%  > max_lines% then gosub page_heading
   /* (AWD008) */
L21310:         if len(text$(2%))   <= 0         then L21380
                     a$             = all(hex(20))
                     str(a$,8%,70%) = "TXT: " & str(text$(2%),1%,65%)
                     gosub print_line

                     if nbr_lines% > max_lines%  then gosub page_heading

L21380:     if txt% = (page_nbr% * 100%) + nbr_lines% then return
            a$ = all(hex(20))
            gosub print_line

            spacer%         = 1%
        return

REM        build_print                     /* PRINT DESCRIPTIONS       */
            err%            = 0%
            if len(part_nmbr$) < 19%        then err% = 1%
            if err%         = 1%            then return
            init(" ") apc$(), apc_scr$, apc_prt$, apc_sze$
            read #9,key = part_nmbr$,using L21510, apc$(), eod goto L21550
L21510:         FMT POS(606), 2*CH(30), CH(20)

            apc_sze$        = apc$(3%)
            goto L21590
L21550:     call "APCDESCR"  (part_nmbr$, apc_scr$, apc_prt$, apc_sze$,  ~
                              #15, err%)
            apc$(1%)        = str(apc_prt$, 1%,30%)
            apc$(2%)        = str(apc_prt$,31%,30%)
L21590:     apc$(3%)        = "WIDTH: "   & str(apc_sze$, 1%,7%) &       ~
                              " HEIGHT: " & str(apc_sze$,11%,6%)
            s_23% = 0%                         /* (EWD002) - Begin     */
            s_23m$ = str(part_nmbr$,1%,3%)
            s_so$  = str(so$,1%,8%)
            s_ln$  = so_seqnr$
            init(" ") s_prv$, s_23$                   /* s_1$ Passed In  */
            prv% = 1%                                 /* Use BCKLINES    */
            call "APCPRZSB" (prv%, s_1$, customer_no$, s_23m$, s_so$,      ~
                                   s_ln$, s_prv$, s_23$, s_23%,            ~
                                   #7, #1, #5, #5, x_er% )
            if x_er% <> 0% then return
               str(apc$(1%),1%,8%) = s_23$
        return                                  /* (EWD002) - End      */

        check_print
            readkey$ = " " : prt% = 0%
            str(readkey$,1%,9%)   = "APCACKPRT"
            str(readkey$,10%,15%) = str(customer_no$,1%,2%) & "XXXX"
            read #1,key = readkey$, eod goto L21700

            goto L21730
L21700:     str(readkey$,10%,15%) = customer_no$
            read #1,key = readkey$, eod goto L21740

L21730:     prt% = 1%
L21740: return

        setup_fax
	    /* EWD RIGHTFAX */
	    a$ = all(hex(20))
/*EWD001*/  a$ = "<TONAME: " & str(cust_name$) & " / " & so$ & ">"        
            gosub print_line
 
	    a$ = all(hex(20))
     /* a$ = "<TOFAXNUM: 764-1567>"        EWD Fax No. for Testing */
        fax_phone$ =  str(text_fields$(1%),1%,11%)     
        call "SPCSMASH" (fax_phone$)                   
        if len(fax_phone$) = 10 then fax_phone$ = "1" & fax_phone$
        if userid$ = "CMG" then  fax_phone$ = "13367641567"
REM          fax_phone$ = "18005223981" 
REM          fax_phone$ = "13367641501" 
REM          fax_phone$ = "19999999999"
        a$ = "<TOFAXNUM: " & fax_phone$ & ">"
	    gosub print_line                               
	

	    a$ = all(hex(20))
 	    a$ = "<NOCOVER>"
	    gosub print_line


	    a$ = all(hex(20))
 	    if schema% = 1% then a$ = "<FORMTYPE: ACKTST>"
 	    if schema% = 2% then a$ = "<FORMTYPE: NEACK>"
	    gosub print_line

rem     a$ = all(hex(20))
rem     a$ = "<PREVIEW>"
rem     gosub print_line


	    return

	    /* EWD RIGHTFAX */

	    rem This is the OLD way

            fax_phone$            =  str(text_fields$(1%),1%,11%)
            fax_code$             =  str(text_fields$(2%),1%,2%)
            fax_style$            = "style=Sales_Ack"
            fax_time$             = "time="
            str(fax_time$,6%,7%)  = "       "
            a$                    =  all(hex(20))
            str(a$, 1%,11%)       = "//-FAX(fax="
            str(a$,12%,11%)       =  fax_phone$
            str(a$,23%, 1%)       = ";"
            str(a$,24%,15%)       =  fax_style$
            str(a$,39%, 1%)       = ";"
            str(a$,40%,12%)       =  fax_time$
            str(a$,52%, 1%)       = ")"
            gosub print_line

        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
L35030:     FMT  CH(09),                 /* Customer Code              */~
                 CH(16),                 /* Sales Order number         */~
                 CH(16),                 /* Purchase Order number      */~
                 6*CH(30),               /* Ship-to name & address     */~
                 6*CH(30),               /* Sold-to name & address     */~
                 CH(20),                 /* Terms code                 */~
                 CH(20),                 /* How to ship                */~
                 CH(20),                 /* FOB                        */~
                 2*CH(50),               /* Shipping instructions      */~
                 XX(9),                  /* Sales account number       */~
                 XX(9),                  /* Discount account number    */~
                 XX(12),                 /* Salesman codes             */~
                 XX(3),                  /* Commission split           */~
                 XX(4),                  /* Region code                */~
                 XX(200),                /* Variable fields            */~
                 CH(4),                  /* Text ID                    */~
                 CH(3),                  /* Store code                 */~
                 CH(6),                  /* Order date (original)      */~
                 XX(6),                  /* Cancel date                */~
                 CH(6),                  /* Default due date           */~
                 XX(6),                  /* Ship date                  */~
                 XX(6),                  /* Date originally entered    */~
                 XX(3),                  /* User ID                    */~
                 XX(6),                  /* Date last changed          */~
                 XX(3),                  /* User ID                    */~
                 XX(9),                  /* Reason code for adjustment */~
                 XX(1),                  /* Sales analysis period      */~
                 XX(1),                  /* Price code                 */~
                 PD(14,4),               /* Discount percent           */~
                 XX(8),                  /* Gross open amount          */~
                 XX(1),                  /* Credit hold flag           */~
                 XX(2),                  /* General sequence number    */~
                 XX(4),                  /* Next BOL number            */~
                 XX(2),                  /* Customer type              */~
                 CH(9),                  /* Account cross-reference    */~
                 CH(4),                  /* Currency code              */~
		 XX(1),                  /* How Field (AWD007)         */~
		 XX(1),                  /* PMETAL SO FLAG  (AWD007)   */~
		 XX(1),                  /* PMETAL INV FLAG (AWD007)   */~
                 CH(08),                 /* Energy Surcharge  (AWD005) */~
                 XX(96)                 /* Filler                     */

L35410:     FMT  XX(9),                  /* Customer code              */~
                 CH(16),                 /* Sales Order number         */~
                 CH(3),                  /* Sales Order sequence number*/~
                 CH(3),                  /* Purchase Order line number */~
                 CH(25),                 /* Part number                */~
                 CH(32),                 /* Part description           */~
                 XX(4),                  /* Category code              */~
                 PD(14,4),               /* Order quantity             */~
                 XX(8),                  /* Quantity shipped           */~
                 PD(14,4),               /* Open order quantity        */~
                 XX(8),                  /* Quantity scheduled         */~
                 XX(16),                 /* Filler                     */~
                 PD(14,4),               /* Unit price @ stocking UOM  */~
                 CH(4),                  /* Stocking UOM               */~
                 CH(4),                  /* Pricing UOM                */~
                 XX(8),                  /* Conversion factor          */~
                 PD(14,4),               /* Unit price @ pricing UOM   */~
                 PD(14,4),               /* Discount percent           */~
                 XX(1),                  /* Taxable code               */~
                 XX(9),                  /* Sales account number       */~
                 XX(9),                  /* Discount account number    */~
                 XX(6),                  /* Original Due date          */~
                 CH(6),                  /* Current Due date           */~
                 XX(6),                  /* Ship date                  */~
                 XX(6),                  /* Lot number                 */~
                 XX(10),                 /* Filler                     */~
                 XX(6),                  /* Project code               */~
                 XX(1),                  /* Demand type                */~
                 XX(1),                  /* Priority                   */~
                 CH(4),                  /* Text ID                    */~
                 XX(1),                  /* Allocation flag            */~
                 XX(8),                  /* Invoice number             */~
                 XX(27),                 /* Skip (EWD002)              */~
                 CH(2),                  /* Private Label Code (EWD002)*/~          
                 XX(17)                  /* Filler (EWD002)            */

        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************
L55030:     FMT CH(80)

        REM *************************************************************~
            *           S P E C I A L   R O U T I N E S                 *~
            *************************************************************
        print_options
            init(" ") opt_seqnr$
            opt_seqnr$ = so_seqnr$  
            call "STRING" addr ("RJ", opt_seqnr$,  len(str(opt_seqnr$)))
            str(optkey$,1%,16%) = so$
            str(optkey$,17%,3%) = opt_seqnr$
            str(optkey$,20%)  = all(hex(00))

        read_alts
            read #11,key 1%   > optkey$, using L60100,                    ~
                optkey1$, optkey$, comp_pn$, eod goto L60560
L60100:         FMT CH(56), CH(23), CH(25)         /* Option Selected */

            if str(optkey$,1%,19%) <> bcklines_key$ then L60560
            optkey2$          = str(optkey1$,26%,28%) /* Assy + BOM */
            str(optkey2$,29%) = str(optkey1$,,25%) /* Orig Comp */
L60150:     str(optkey2$,54%) = hex(00)

        readnext_loop
            read #10,key      > optkey2$, using L60200,                   ~
                optkey1$, print_flag$,          eod goto readnext_check
L60200:         FMT POS(55), CH(25), CH(1)

            goto readnext_skip

        readnext_check
            if str(optkey2$,,28%) = " "             then read_alts
            str(optkey2$,54%,3%)  = str(optkey1$,54%,3%)
            read #12,key 1%       = optkey2$, using L60290,               ~
                optkey1$, print_flag$,          eod goto L60320
L60290:         FMT POS(84), CH(25), POS(184), CH(1)

            goto readnext_skip
L60320:     str(optkey2$,,28%)    = " "                  /* Any Assy */
            goto L60150

        readnext_skip
            if optkey1$    <> comp_pn$              then readnext_loop
            if pos("YD"=print_flag$) = 0            then read_alts
            read #9,key     = optkey1$, using L60400,                     ~
                part_desc$,                     eod goto read_alts
L60400:         FMT POS(26), CH(32)

            temp%           =  2%
            if print_flag$  = "D"                   then temp% = 1%
            if max_lines%   -  nbr_lines% < temp% then gosub page_heading
            if print_flag$  = "D"                   then L60500
            a$              =  all(hex(20))
            str(a$,8%,25%)  =  comp_pn$
            gosub print_line

L60500:     a$              = all(hex(20))
            str(a$,8%,32%)  = part_desc$
            gosub print_line

            a$              = all(hex(20))
            goto read_alts                        /* Go Get Next Opt*/
L60560: return

        page_heading
            page_nbr%       = page_nbr% + 1%
            rem if page_nbr%    = 1%                    then L60720

	    /* EWD RIGHTFAX */
	    if page_nbr%    = 1%                    then L60730

            a$              = all(hex(20))
            str(a$, 8%,58%) = ">>> C O N T I N U E D <<<"
            str(a$,71%,10%) = "CONTINUED"
            gosub print_line

            a$              = all(hex(20))  
            for pp%         = 1% to (38% - nbr_lines%)
                gosub print_line        /*^EWD RightFax*/

            next pp%
    

/*L60720:*/ a$              = all(hex(20))
	    /* EWD RIGHTFAX for bl%         = 1% to 2%		 */
            
L60730:
            for bl%         = 1% to 2%         /*  (EWD004) */
                gosub print_line
            next bl%	                       /*  (EWD004) */
            /* EWD RIGHTFAX */
	    a$ = all(hex(20))

            str(a$,47%,16%) = so$               /*EWD RightFax*/
            str(a$,63%,10%) = date$             /*EWD RightFax*/
            convert page_nbr% to page_nbr$, pic(####)

            str(a$,76%, 4%) = page_nbr$
            gosub print_line

            a$              = all(hex(20))
            for bl%         = 1% to 2%
                gosub print_line

            next bl%

            if currency$    = " "                   then L60950
            str(a$, 1%,10%) = "CURRENCY: "
            str(a$,11%, 4%) =  currency$
            str(a$,16%,32%) = " "
L60950:     str(a$,44%,16%) =  po$              /*EWD RightFax*/
            str(a$,61%, 9%) =  customer_no$     /*EWD RightFax*/
            str(a$,70%,10%) =  orderdate$       /*EWD RightFax*/
            gosub print_line

            a$              = all(hex(20))
            for bl%         = 1% to 3%
                gosub print_line

            next bl%

            for st%             = 1% to 6%
                a$              = all(hex(20))
                str(a$, 9%,31%) = sold_to$(st%)
                str(a$,49%,31%) = ship_to$(st%)
                gosub print_line

            next st%

            a$              = all(hex(20))
            for bl%         = 1% to 3%
                gosub print_line

            next bl%

            str(a$, 3%,28%) = term_desc$
            str(a$,30%,20%) = how_ship$
            str(a$,51%,15%) = fob$              /*EWD RightFax*/
            str(a$,67%,10%) = grp_due$
            gosub print_line

            a$              = all(hex(20))
            for bl%         = 1% to 3%
                gosub print_line

            next bl%

            nbr_lines%      =  0%
            if page_nbr%    >  1%                   then return
            if shipinstr$() = " "                   then return
            shipping$       = "SHIPPING:"

            for si%                =  1% to 2%
                a$                 =  all(hex(20))
                if shipinstr$(si%) = " "            then next_si
                str(a$, 8%, 9%)    =  shipping$
                str(a$,20%,50%)    =  shipinstr$(si%)
                gosub print_line

                shipping$          = " "
        next_si
            next si%

        return

        print_line
            nbr_lines% = nbr_lines% + 1%
            str(a$,80%,1%) = hex(0D)
            write #20, using L55030, a$, eod goto L61550

        return
L61550:     error% = 5%
        return clear all

                                                           /* (EWD003) - BEG */

        oracle_query
            oci_err% = 0%
            call "QUERY" (stmt1$, stmt2$, oci_err%)
        return

        oracle_exec
            oci_err% = 0%
            call "EXEC" (stmt1$, stmt2$, oci_err%)
        return

        oracle_flush
            oci_err% = 0%
            call "FLUSH" (oci_err%)
        return

        oracle_fetch
            oci_err% = 0%
            no_fields% = 0%
            call "FETCH" (no_fields%, oci_err%)
REM            if oci_err% = 0% then return
REM               call "ERROR" (error$)
REM               call "SHOSTAT" ("ERROR ERROR RETURN VALUE --> " & error$)
REM               stop               
        return

        oracle_getfield
            oci_err% = 0%
            field_len% = 0%
            init(" ") field$, name$
            call "FIELDINF" (field_num%, field$, name$, field_len%, oci_err%)
        return
 
        load_newline
             config%, text_lne%, so_seqnr% = 0%
             init(" ") fields$()
             pos% = 1%
             field_num% = 5%
             gosub oracle_getfield
             so_seqnr$ = field$
     
             convert so_seqnr$ to so_seqnr%, data goto delete_fax

             for field_num% = 6% to 15%
                 gosub oracle_getfield
                 str(fields$(),pos%, field_len%) = field$
                 pos% = pos% + field_len%
             next field_num%

             field_num% = 16%
             gosub oracle_getfield
             convert str(field$,1%,len(field$)) to text_lne%, data goto next_lne
             str(fields$(),pos%,4%) = BIN(text_lne%,4)

        next_lne
             pos% = pos% + 4%             
             if str(fields$(),1%,3%) <> " " then goto end_newline
                 gosub oracle_fetch
                 if oci_err% > 0% and oci_err% < 100% then goto delete_fax
                 if oci_err% = 100% then goto end_of_lines
                 goto load_newline
        end_newline
        return

        oracle_delete
             init(" ") stmt1$
             str(stmt1$,1%,40%)   = "DELETE FROM MSSQL.DISPLAY WHERE SALESORD" 
             str(stmt1$,41%,30%)  = "ER = '"&str(so$,1%,8%)&"' AND USERID = '"
             str(stmt1$,71%,10%)  = userid$ & "'"
             gosub oracle_flush
             gosub oracle_exec
        return
                                                           /* (EWD003) - END */

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************
        exit_sub
            counter%       = 0% : fx1% = 0% : fx2% = 0%
            if error%      = 1% then end
            close #20
REM            call "SHOSTAT" ("I am Closed")  stop
                if error% <> 0% then delete_fax
REM            script$ = "TSTFAX"

            call "LINK" addr(script$, fx1%, fx2%)
                if fx1%    > 0% then error% = 4%

        delete_fax
            gosub oracle_delete
            call "FILEBGON" (#20)          /* Scratch Fax File        */
L65140:     read    #1,hold,key = fax_key$, eod goto L65190
            put     #1, using L10280, "-"
            rewrite #1

        end
L65190:     counter%       = counter% + 1%
            if counter%    < 4% then L65140
            error%         = 6%
        end


