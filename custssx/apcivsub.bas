        REM *************************************************************~
            *                ( As of November, 1997 new index ARIMASTR  *~
            *                                                           *~
            * APCIVSUB - Update Product Quantities and Sales for Monthly*~
            *            Buckets for each Model.                        *~
            *                                                           *~
            *     Note - Only Process Data for Sales Order Month if the *~
            *            Sales Order was 'Invoiced' during the Year.    *~
            *                                                           *~
            *          - Fix Problem with a Bad Credit (Six Digit)      *~
            *            Invoice Number. No Leading Zero's.             *~
            *                                                           *~
            * RECORD LAYOUT FOR - (APCIVAVG) Same as (APCSOAVG)         *~
            *   1. CH(3)       -  Product (MODEL$)    (Primary Key)     *~
            *   2. CH(1)       -  Product (COLOR$)    (Primary Key)     *~
            *   3. 12*PD(14,4) -  Total QTY Product by Month (APC_QTY())*~
            *   4. 12*PD(14,4) -  Total Price Product by Mon (APC_PC() )*~
            *   5. 12*PD(14,4) -  Total QTY Zero Product     (APC_QTZ())*~
            *   6. CH(6)       -  Date Last Order Entered ( DTE$ )      *~
            *   7. CH(3)       -  Filler - Total = 300                  *~
            *                                                           *~
            * SKIP_PART ( Special Routine )                             *~
            *   Screen (SC$) - Skip Parts with Code (4,5,6)             *~
            *                  TSO, BSO, FSO                            *~
            *   Glass  (GL$) - Skip Parts with a Glass Code Greater     *~
            *                  than '30'. Those are Special Parts       *~
            *                  Only skip parts with gls over 29 and less*~
            *                  than 89, 99 or starts with Z   (AWD003)  *~
            *                                                           *~
            *   Skip Parts That the Length is less than (19). Those are *~
            *   Components.                                             *~
            *                                                           *~
            *   Calling Program (APCIVAVG) Creates the Report.          *~
            *                                                           *~
            *************************************************************~
            * 03/31/98  ERN  Y2K modifications                          *~
            * 04/07/00 !Mods on how avg cost is calc for EFFICIENCY !CMG*~
            *          ! (EWD0001)                                  !TM2*~
            * 09/17/02 ! (EWD002) Fix for Special Shapes Grid Code  !CMG*~
            * 08/16/05 ! (AWD003) Mod to fix average price parts    !CMG*~
            *          !           (CR357)                          !   *~
            * 10/31/05 ! (AWD004) Mod for sub part CR347            !CMG*~
            * 05/01/08 ! (AWD005) mod for casing and sdl            !CMG*~
            * 02/03/15 ! (AWD006) mod for New APCPCMST record size  !PWW*~
            * 02/03/2021! CR2768  Missing parameters to apcprsub    !RDB*~
            *************************************************************


            sub "APCIVSUB" (yr$, rpt_month%, chk_inv$, stat$, #4)

            /* yr$ passed as YYYY */

        dim                                                              ~
            stat$1,                      /* (Y)es OR (N)o - CLEAR DATA */~
            gl$2,                        /* GLASS CODE                 */~
            sc$1,                        /* SCREEN CODE                */~
            hdr_key$25,                  /* BCKMASTR - Primary Key     */~
            bck_key$19,                  /* BCKLINES - Primary Key     */~
            bck_cust$9,                  /* Customer Number            */~
            bck_ord$16,                  /* S.O. Number                */~
            invoice_type$1,              /* Type of invoice            */~
            inv_reason$9,                /* Invoice Reason code        */~
            bck_part$25, prc_part$25,    /* Part Number                */~
            partno1$20,                  /* Sub Part Number    (AWD001)*/~
            dte$6,                       /* Date S.O. Entered          */~
            apc_dte$6,                   /* Date S.O. Entered          */~
            yr$4,                        /* Current Year               */~
            chk_yr$4,                    /* Check Year Value           */~
            chk_mt$2,                    /* CHECK MONTH                */~
            chk_inv$8,                   /* Starting Invoice Number    */~
            date$6,                      /* Todays Date                */~
            model$3,                     /* Part Model Code            */~
            color$1,                     /* Part Color                 */~
            sys_key$20,                  /* SYSFILE2 KEY               */~
            temp1$10, temp2$10,          /* Temporary storage          */~
            ar_key$8,                    /* (ARIMASTR) Alt Key (1)     */~
            save_key$17,                 /* Save key                   */~
            ari_key$20,                  /* (ARILINES) key             */~
            apc_inv$8,                   /* INVOICE NUMBER             */~
            save_inv$8,                  /* LAST INVOICE NO USED UPD   */~
            apc_key$4,                   /* Cost Primary Key           */~
            apc_qty(12%),                /* 12 Months of Quantities    */~
            apc_qtz(12%),                /* 12 Months Zero Price Qty's */~
            apc_pc(12%),                 /* 12 Months of Prices        */~
            wood_model$3,                /* model                      */~
            vmodel$16,                   /* CR2768                     */~
            tpp$2,                       /* CR2768                     */~
            nfrc$2,                      /* CR2768                     */~
            forcedfoam$2,                /* CR2768                     */~ 
            sash5_0$2,                   /* CR2768                     */~
            apc_fil$2                    /* FILLER                     */

        dim                              /* Pricing Variables          */~
            size$1,                      /* (O)pening, (E)xact         */~
            pc(36%),                     /* Calc Dealer Price Catalog  */~
            upd$1, cd$1,                 /* Update Prices Y or N       */~
            ref$(15%)2,                  /* Ref Type Codes Catalog     */~
            ref1$(15%)2,                 /* Ref Type Codes Spec. Cat.  */~
            ref_p(15%),                  /* Ref Prices APC CAtalog     */~
            ref_p1(15%)                  /* Ref Prices Spec. Cat.      */   
/* (AWD005) */
        dim                                                              ~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4,            /* Additional Info Fields     */~
            sub_part$20,                 /* Subpart Number             */~
            info_part$20,                /* info part number           */~
            sav_color$1                  /* Save Color                 */
            
        dim logIt$256

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Calculate Average Price of Product"
            pname$ = "APCIVSUB - Rev: R6.04"

        REM *************************************************************

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! SYSFILE2 ! SYSTEM MASTER FILE                       *~
            * #02 ! BCKMASTR ! S.O. MASTER HEADER FILE                  *~
            * #03 ! BCKLINES ! S.O. LINE DETAIL FILE                    *~
            * #04 ! APCIVAVG ! Month Average Unit Cost File. Invoiced   *~
            * #05 ! ARIMASTR ! Invoice Master File                      *~
            * #63 ! BCKSUBPT ! New Sub Part Number File         (AWD005)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #2,   "BCKMASTR",                                     ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #3,   "BCKLINES",                                     ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =   10, keylen =   19

            select #5,  "ARIMASTR",                                      ~
                        varc, indexed, recsize = 2000,                   ~
                        keypos = 1, keylen =  17,                        ~
                        alt key 1, keypos = 10, keylen =  8, dup,        ~
                            key 2, keypos = 18, keylen = 16, dup,        ~
                            key 3, keypos = 34, keylen = 16, dup,        ~
                            key 4, keypos =1783, keylen = 26

            select #6,  "ARILINES",                                      ~
                        varc, indexed, recsize = 750,                    ~
                        keypos = 1, keylen = 20                          

            select #7,  "APCPCMST"                                       ~
/*AWD006 */             varc,     indexed,  recsize = 128,               ~
                        keypos = 9,    keylen =  53,                     ~
                        alt key  1, keypos  =     1, keylen = 8

            select #8, "APCPCMSK"                                       ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos = 1,    keylen =   5

            select #9, "APCPCMSD"                                       ~
/*AWD004*/              varc,     indexed,  recsize =  768,              ~
                        keypos = 1,    keylen =   9

            select #10, "APCSKUNO"                                       ~
                        varc,     indexed,  recsize =  73,               ~
                        keypos = 1,    keylen =  28,                     ~
                        alt key  1, keypos  =    29, keylen = 28, dup

            select #11, "CPRPRICE"                                       ~
                        varc,     indexed,  recsize = 700,               ~
                        keypos = 1,    keylen =  47

            select #12, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =  1,   keylen =  9,                      ~
                        alt key  1, keypos  =    10, keylen = 30, dup,   ~
                            key  2, keypos  =   424, keylen =  9, dup,   ~
                            key  3, keypos  =   771, keylen =  9, dup,   ~
                            key  4, keypos  =   780, keylen =  9, dup

            select #13,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =  24

                                                        /* (AWD005)     */


            select #63, "BCKSUBPT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =  12, keylen =  11, dup,     ~
                            key  2, keypos =  23, keylen =  45, dup

            call "OPENCHCK" (#1,  fs%(1%),  f2%(1%),   0%, rslt$(1%))
            call "OPENCHCK" (#2,  fs%(2%),  f2%(2%),   0%, rslt$(2%))
            call "OPENCHCK" (#3,  fs%(3%),  f2%(3%),   0%, rslt$(3%))
            call "OPENCHCK" (#5,  fs%(5%),  f2%(5%),   0%, rslt$(5%))
            call "OPENCHCK" (#6,  fs%(6%),  f2%(6%),   0%, rslt$(6%))
            call "OPENCHCK" (#7,  fs%(7%),  f2%(7%),   0%, rslt$(7%))
            call "OPENCHCK" (#8,  fs%(8%),  f2%(8%),   0%, rslt$(8%))
            call "OPENCHCK" (#9,  fs%(9%),  f2%(9%),   0%, rslt$(9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%),  0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%),  0%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%),  0%, rslt$(12%))
            call "OPENCHCK" (#13, fs%(13%), f2%(13%),  0%, rslt$(13%))
            call "OPENCHCK" (#63, fs%(63%), f2%(63%),  0%, rslt$(63%))


            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

        REM *************************************************************~
            *             C H E C K   O R D E R   S T A T U S           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

            model$ = " "
            init(" ") partno1$                         /* (AWD004) */
            date$ = date
            wood% = 0%
   
*       RHH YR$   = STR(DATE$,1%,2%)    /* Processing Year obtained  */
                                         /* from Calling Program      */
            init(" ")temp1$,temp2$
            temp% = 0%
            temp1$ = date
            call "DATEOKC" (temp1$, temp%, temp2$)
            chk_mt$ = str(temp1$,1%,2%)   /* Save Current Month        */
            apc_dte$ = " "
            count% = 0%
            apc_inv$ = all(hex(00))
            if stat$ = "N" then goto L01460
               gosub clear_data
                                   /* Look-up the Last Invoice Checked */
L01460:     sys_key$ = "**APC INVOICE UNIT**"
             read #1,key = sys_key$,using L01480  ,apc_inv$, eod goto L01540
L01480:        FMT XX(20),CH(8)
*       RHH                              /* Set for January of the    */
             if chk_inv$ <> "99999999" then apc_inv$ = chk_inv$

L01540:     call "SHOSTAT" ("Scanning Current S.O. Invoiced")
            save_inv$ = " "
            ar_key$ = all(hex(00))
REM            apc_inv$ = "05793483"       /* Current Year (2021)       */

            convert apc_inv$ to apc_inv%, data goto L01610

            convert apc_inv% to apc_inv$, pic(00000000)
            ar_key$ = apc_inv$
L01610:     read #5,key 1% > ar_key$, using L01670 , bck_cust$, apc_inv$,  ~
                                      bck_ord$, dte$, invoice_type$, inv_reason$, ~
                                      eod goto finished
            goto L01680
        next_invoice
            read #5, using L01670 , bck_cust$, apc_inv$, bck_ord$, dte$,   ~
                                    invoice_type$, inv_reason$,  eod goto finished
L01670:        FMT CH(9), CH(8), XX(16), CH(16), POS(521), CH(6), POS(891), CH(01), CH(09)

            if apc_inv$ = "03000092" then call "SHOSTAT" ( "INVOICE " & apc_inv$)
REM         if apc_inv$ = "03000092" then stop
            
L01680:     temp1$ = dte$
            call "DATFMTC" (temp1$)
            chk_yr$ = str(temp1$,7%,4%)

            count% = count% + 1%
            if mod(count%,50%) <> 0 then goto L01750
               convert count% to count$, pic(######)
               call "SHOSTAT" ("Invoices Scanned ("& count$ &")")

L01750:     month$ = str(temp1$,1%,2%)          /* Post Month of Invoice */
       
            month% = 1%
            convert month$ to month%, data goto L01780
L01780:
            if yr$ <> chk_yr$ then goto next_invoice
            
            if rpt_month% <> month% then goto next_invoice
            
            if invoice_type$ = "A"  then goto get_credit  /* (EWD0001) */
            if invoice_type$ = "C"  then goto get_credit  /* ADJUST FOR CREDITS */

                                       /* Skip Prior Yr                */
                                       /* Do not Skip any Current Year */
                                       /* Finished 1st Finance Invoice */
            convert apc_inv$ to apc_inv%, data goto finished

            if len(apc_inv$) < 8 then goto L01870
                save_inv$ = apc_inv$
L01870:     gosub read_header
            goto next_invoice

        read_header
            if len(bck_ord$) < 8 then return
            if str(bck_ord$,1%,1%) = " " then return
            hdr_key$ = all(hex(00))
            str(hdr_key$,1%,9%)   = bck_cust$
            str(hdr_key$,10%,16%) = bck_ord$
            read #2,key = hdr_key$, using L01980 , ord_disc,eod goto L02010

L01980:        FMT POS(859), PD(14,4)
            gosub scan_order
        return
L02010:     call "SHOSTAT" ("Error-Unable to Read S.O.-"&bck_ord$ )
REM            stop
        return

        scan_order
            bck_key$ = all(hex(00))
            str(bck_key$,1%,8%) = bck_ord$
            read #3, key > bck_key$, eod goto scan_done
            goto L02120
        scan_next                           /* Look-up - (BCKLINES)    */
            read #3, eod goto scan_done
L02120:        get #3, using L02140 , bck_key$, bck_part$, unit_qty,       ~
                                                      unit_price, ln_disc
L02140:        FMT XX(9), CH(19), XX(03), CH(25), XX(32),                ~
                   XX(04), PD(14,4), POS(165), 2*PD(14,4)
            if str(bck_key$,1%,8%) <> str(bck_ord$,1%,8%) then           ~
                                                           goto scan_done

            gosub skip_part
            if skip% <> 0% then goto scan_next
            gosub check_samples               /* if ss = 1 then has to be */
            if ss% = 1% then goto scan_next   /* part                     */

            gosub check_wood                  /* (EWD0001)  BEGIN */ 
            if wood% <> 1% then L02150
               unit_price = wood_price         /* (EWD0001) END */
               
L02150:     gosub lookup_subpart               /* (AWD005) */
            gosub check_casing_sdl             /* (AWD005) */


            total_price =  round(unit_price * unit_qty, 2)
                                                     /* CALC LINE DISC */
            discamt = round(total_price * ln_disc * .01, 2)
            total_price = round(total_price - discamt, 2)
            gosub update_cost
            goto scan_next
        scan_done
        return

        skip_part
            skip% = 1%
            if len(bck_part$) < 19 then return
               sc$ = str(bck_part$,11%,1%)
               if sc$ = "4" or sc$ = "5" or sc$ = "6" then return
                  gl% = 0%
                  gl$ = str(bck_part$,5%,2%)
                  convert gl$ to gl%, data goto L02370
L02370:

/* (AWD003) - BEGIN */
                  if gl% > 29% and gl% < 89% then return
                  if gl% = 99% then return
                  if str(gl$,1%,1%) = "Z" then return
/* (AWD003) - END */ 
                  skip% = 0%
        return

        update_cost
            model$ = str(bck_part$,1%,3%)
            if str(model$,1,1) = "9" and wood% <> 1% then gosub logIt
REM         gosub check_wood
            color$ = str(bck_part$,4%,1%)
REM         model% = 0%
REM         convert model$ to model%, data goto L02790
                                         /* CALC ORDER DISCOUNT */
            if wood% <> 1% then goto L02380   /* (EWD0001) */
               model$ = wood_model$           /* (EWD0001) */
               wood% = 0%                     /* (EWD0001) */
L02380:     discamt = round(total_price * ord_disc * .01, 2)
            total_price = round(total_price - discamt, 2)

            apc_key$ = model$ & color$
            read #4,hold,key = apc_key$, using L02550 ,                    ~
                            apc_qty(), apc_pc(), apc_qtz(), apc_dte$,    ~
                                                 apc_fil$, eod goto L02660
L02550:        FMT POS(5), 12*PD(14,4), 12*PD(14,4), 12*PD(14,4), CH(6), ~
                                                                  CH(2)
            apc_qty(month%) = round(apc_qty(month%) + unit_qty, 2)
            apc_pc(month%)  = round(apc_pc(month%) + total_price, 2)
            if total_price < .01 then              /* ZERO PRICE */      ~
                   apc_qtz(month%) = round(apc_qtz(month%) + unit_qty, 2)

            rewrite #4, using L02550 , apc_qty(), apc_pc(), apc_qtz(),     ~
                                                dte$,     apc_fil$
            goto L02790

L02660:       mat apc_qty = zer          /* TOTAL UNITS - ALL PRICES */
              mat apc_qtz = zer          /* TOTAL UNITS - ZERO PRICE */
              mat apc_pc  = zer
              apc_fil$ = " "
              apc_qty(month%) = round(apc_qty(month%) + unit_qty, 2)
              apc_pc(month%)  = round(apc_pc(month%) + total_price, 2)
              if total_price < .01 then            /* ZERO PRICE */      ~
                   apc_qtz(month%) = round(apc_qtz(month%) + unit_qty, 2)

              write #4, using L02770 , model$, color$, apc_qty(),          ~
                                     apc_pc(), apc_qtz(), dte$, apc_fil$
L02770:        FMT CH(3), CH(1), 12*PD(14,4), 12*PD(14,4), 12*PD(14,4),  ~
                                                           CH(6), CH(2)
L02790: return

        clear_data
           call "SHOSTAT" ("Clearing All Previous Data")
           call "DELETE" (#4, " ", 0%)

           sys_key$ = "**APC INVOICE UNIT**"
           read #1,hold,key = sys_key$,using L02880  ,sys_key$, apc_inv$,  ~
                                                          eod goto L02900
L02880:          FMT CH(20),CH(8)
            delete #1
L02900:     apc_inv$ = all(hex(00))
            write #1, using L02880 , sys_key$, apc_inv$
        return
                                                       /* (EWD001)      */
        check_samples
            ss% = 0%
            if len(bck_part$) < 20 then return        /* Quick Test      */
            if str(bck_part$,1%,1%) = "9" then return   /* Bay/Bow       */
            convert str(bck_part$,20%,3%) to ss%, data goto LS1

            if ss% < 1% or ss% > 80% then goto LS1   /* Not Samp/Disp   */
                                                     /* if code 0 - 79  */
                                                     /* has to be part  */
                                                     /* or samp/display */
                                                     /*   (EWD002)      */
            if str(bck_part$,7%,2%) > "99" then goto LS1
            ss% = 1%                                 
        return                                       /* Code Found      */
LS1:        convert str(bck_part$,23%,3%) to ss%, data goto LS2

            if ss% < 1% or ss% > 80% then goto LS2
                                                  /* Code Found      */ 
            ss% = 1%
LS2:    return                                    /* (EWD001)      */

 /* (EWD0001) BEGIN */
        check_wood
            ss% = 0%
            sav_price, total_price, wood_price = 0.0
            if len(bck_part$) < 20 then return        /* Quick Test      */
            convert str(bck_part$,20%,3%) to ss%, data goto LS3

            if len(bck_part$) < 23 then return        /* Quick Test      */
            convert str(bck_part$,23%,3%) to ss%, data goto LS4
        return                                 
LS3:      wood_model$ = str(bck_part$,20%,3%)
          str(wood_model$,2%,2%) = "00"      /* has to be set to '00' */
          prc_part$ = bck_part$              /* b/c look for it in EFFICITA */
          gosub calc_price             
          sav_price = total_price
          prc_part$ = str(bck_part$,1%,19%)
          gosub calc_price
          if sav_price < .01 then goto LS8
          wood_price = sav_price - total_price
LS8:      unit_price = unit_price - wood_price

          total_price =  round(  unit_price * unit_qty, 2)
                                                     /* CALC LINE DISC */
          discamt = round(total_price * ln_disc * .01, 2)
          total_price = round(total_price - discamt, 2)
          

          gosub update_cost
          wood% = 1%

        return
LS4:      wood_model$ = str(bck_part$,23%,3%)
          str(wood_model$,2%,2%) = "00"
          prc_part$ = bck_part$
          gosub calc_price
          sav_price = total_price
          prc_part$ = str(bck_part$,1%,22%)
          gosub calc_price
          if sav_price < .01 then goto LS7
          wood_price = sav_price - total_price
LS7:      unit_price = unit_price - wood_price

          total_price =  round(  unit_price * unit_qty, 2)
                                                     /* CALC LINE DISC */
          discamt = round(total_price * ln_disc * .01, 2)
          total_price = round(total_price - discamt, 2)
          gosub update_cost
          wood% = 1%
        return

        get_credit
          inv_reason% = 0%
          if str(bck_cust$,1,6) = "EM0100" then goto next_invoice
          
          convert inv_reason$ to inv_reason%, data goto LS6

LS6:      REM if inv_reason% < 14% then goto next_invoice
          total_price = 0
          init(" ") ari_key$
          str(ari_key$,1%,9%)   = bck_cust$
          str(ari_key$ ,10%,8%) = apc_inv$
          save_key$ = ari_key$

        get_credit_next

          read #6, key > ari_key$, using LS5, ari_key$, bck_part$, unit_qty, ~
                         unit_price, eod goto next_invoice

          if str(ari_key$,1%,17%) <> save_key$ then goto next_invoice

LS5:     FMT CH(20), POS(24), CH(25), POS(85), PD(15,4), POS(133), PD(15,4)


         gosub check_wood                  /* (EWD0001)  BEGIN */
         if wood% <> 1% then creditWood
            unit_price = wood_price         /* (EWD0001) END */

creditWood:
          total_price = unit_price * unit_qty
          gosub update_cost
          goto get_credit_next



        calc_price
          mat pc     = zer
          mat ref_p  = zer
          mat ref_p1 = zer
          p1    = 0.0      /* Spc Customer Price, Dealer Catalog Always*/
          sp%   = 0%       /* (0%) Dealer Price Catalog (Only)         */
                           /* (1%) Spc Customer Catalog Price in (P1)  */
                           /* (2%) Spc Customer EDI Price in     (P1)  */
                           /* Note - Information from 'CUS PRICE' Table*/
          size$ = "E"      /* Always Exact Size                        */
          upd$ = "N"       /* Do Not Update Price Sheets               */
          err%  = 0%       /* Clear Error Flag                         */
          
          init(" ") sash5_0$, tpp$, nfrc$, forcedfoam$, vmodel$   /* CR2768 */
          vmodel$ = model$
/* CR2768 updated call routine */              
          call "APCPRSUB" ( prc_part$,   /* Part Number                */~ 
                            partno1$,    /* Sub Part Number    (AWD004)*/~
                            size$,       /* (O)pen,(E)xact,(F)No Deduct*/~
                            pc(),        /* Calc Dealer Price Catalog  */~
                            p1,          /* Special Customer Price     */~
                            sp%,         /* Special Price Code 0,1,2   */~
                            upd$,        /* Update Price Sheet Y or N  */~
                            bck_cust$,   /* Customer Code              */~
                            err%,        /* Error Return Codes         */~
                            ref$(),      /* Ref. Type Codes Catalog    */~
                            ref1$(),     /* Ref. Type Codes Spec Cat.  */~
                            ref_p(),     /* Ref Prices APC Catalog     */~
                            ref_p1(),    /* Ref Prices Special Cat.    */~
                            sash5_0$,    /* (AWD020) 5/0 Sash option   */~
                            vmodel$,     /* Model                      */~
                            tpp$,        /* TriplePanePackage (SR67154)*/~
                            nfrc$,       /* NRFC 2016 req     (SR71583)*/~
                            forcedfoam$, /* Forced Foam                */~
                            #7,          /* Channel of (APCPCMST) File */~
                            #8,          /* Channel of (APCPCMSK) File */~
                            #9,          /* Channel of (APCPCMSD) File */~
                            #13,         /* Channel of (GENCODES) File */~
                            #11,         /* Channel of (CPRPRICE) File */~
                            #12,         /* Channel of (CUSTOMER) File */~
                            #10 )        /* Channel of (APCSKUNO) File */
                                         /* After Call PART$ Always    */
                                         /* has Exact Size             */
REM           if err% <> 0% then apc_err% = 8%   /* Pricing Error         */
REM           if apc_err% <> 0% then apc_err%(apc_err%) = 8%

            srce2% = 0%
            read #12,key = bck_cust$, eod goto L51150
                get #12, using L51080, cd$
L51080:     FMT POS(525), CH(1)
            if cd$ >= "A" and cd$ <= "Z" then                            ~
               srce2% = val(cd$) - 64%          /* (01) Thru (26) */     ~
               else  srce2% = val(cd$) - 21%    /* (27) Thru (36) */

L51150:    total_price = pc(srce2%)
           if sp% > 0% and p1 > 0.1 then total_price = p1  /* Spec Prce */
        return
/* (EWD0001)  END */

        finished
            if save_inv$ = " " then goto exit_program
            sys_key$ = "**APC INVOICE UNIT**"
            read #1,hold,key = sys_key$,using L02990  ,sys_key$, apc_inv$, ~
                                                          eod goto L03010
L02990:          FMT CH(20),CH(8)
            delete #1
L03010:     apc_inv$ = save_inv$
            write #1, using L02990 , sys_key$, apc_inv$
        exit_program
        end

/* (AWD005) */
        lookup_subpart
            init(" ") bcksubpt_rec$, flds$(), info_flds$(), sub_part$,~
                      info_part$, so_inv$, item_no$

            so_inv$  = str(bck_key$,1,8)
            item_no$ = str(bck_key$,17,3)

            flag$ = "0"                  /* Sales Order Info         */
            pgm$  = "1" 
            err1% = 0%

            convert so_inv$ to so_inv%, data goto convert_alpha

            convert so_inv% to so_inv$, pic(00000000)

            goto order_converted

convert_alpha:
            convert str(so_inv$,2%,7%) to so_inv%, data goto sub_part1
sub_part1:
            convert so_inv% to str(so_inv$,2%,7%), pic(0000000)

order_converted:
            convert item_no$ to item_no%, data goto sub_part2
sub_part2:
            convert item_no% to item_no$, pic(###)

        call "AWDBKSUB"   (flag$,        /* Flag 0=SalesOrder 1=Invoice*/~
                          pgm$,          /* Calling Program 0=BCKUPDTE */~
                                         /* 1=Any Other 2=Delete       */~
                                         /* 3=Invoice                  */~
                          so_inv$,       /* SO or Invoice Num to lookup*/~
                          item_no$,      /* Item Number                */~
                          bcksubpt_rec$, /* Record If BCKUPDTE then    */~
                                         /* pass in else pass out      */~
                          flds$(),       /* Part Number Fields         */~
                          info_flds$(),  /* Information Fields         */~
                          #63,           /* BCKSUBPT File              */~
                          err1%)         /* Error Code                 */
           
            if err1% <> 0% then str(bcksubpt_rec$,48%,20%) = "00000               "

           sub_part$ = str(bcksubpt_rec$,48%,20%)

           info_part$ = str(bcksubpt_rec$,132%,9%) & " " 


            if err1% = 0% then return

       return

       check_casing_sdl
           casing%       = 0%
           casing_wood%  = 0%
           sdl%          = 0%
           sdl_wood%     = 0%
           cas_sdl%      = 0%
           cas_sdl_wood% = 0%

           if str(sub_part$,6,1) <> "0" then casing% = 1%
           if str(sub_part$,1,3) = "418" then sdl% = 1%
           if casing% = 1% and sdl% = 1% then cas_sdl% = 1%
           if wood% = 1% and casing% = 1% then casing_wood% = 1%
           if wood% = 1% and sdl% = 1% then sdl_wood% = 1%
           if wood% = 1% and casing% = 1% and sdl% = 1% then ~ 
                             cas_sdl_wood% = 1%

           if casing% = 1% or sdl% = 1% or cas_sdl% = 1%  ~
                         then goto price_cas_sdl
           if casing_wood% = 1% or sdl_wood% = 1% or      ~
              cas_sdl_wood% = 1% then goto price_cas_sdl
       return
       price_cas_sdl
          sav_price, new_price = 0.00
          prc_part$ = bck_part$
          partno1$  = sub_part$
          gosub calc_price
          sav_price = total_price
          prc_part$ = str(bck_part$,1%,22%)
          str(partno1$,1,20) = " "
          gosub calc_price
          if sav_price < .01 then goto not_price
          new_price = sav_price - total_price
not_price:
          unit_price = unit_price - wood_price

          total_price =  round(  unit_price * unit_qty, 2)
                                                     /* CALC LINE DISC */
          discamt = round(total_price * ln_disc * .01, 2)
          total_price = round(total_price - discamt, 2)
          if casing% = 1% then model$ = "001"
          if sdl% = 1% then model$ = "002"
          if cas_sdl% = 1% then model$ = "003"
          init(" ") sav_color$
          sav_color$ = color$
          color$ = "Z"
          gosub update_cost
          color$ = sav_color$
        return

           
/* (AWD005) end */

        logIt
         call "SHOSTAT" (" HERE AT LOGIT ")
         init(" ") logIt$
         str(logIt$,1,18)  = apc_inv$ & "  " & str(bck_ord$,1,8)
         str(logIt$,21,28) = bck_part$ & "  "  & invoice_type$
         str(logIt$,51,10) = str(bck_key$,17,3) & "  " & str(ari_key$,18,3)

         convert unit_qty to str(logIt$,61,15), pic(#########.####-)

         convert total_price to str(logIt$,76,15), pic(#########.####-)

         call "LOGFILE" (logIt$)

      return
        

