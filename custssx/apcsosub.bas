        REM *************************************************************~
            * APCSOSUB - UPDATE PRODUCT QUANTITIES AND SALES FOR MONTHLY*~
            *            BUCKETS FOR EACH MODEL                         *~
            *                                                           *~
            * RECORD LAYOUT FOR - (APCSOAVG)                            *~
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
            *                  than '19'. Those are Special Parts       *~
            *                                                           *~
            *   Skip Parts That the Length is less than (19). Those are *~
            *   Components.                                             *~
            *                                                           *~
            *************************************************************

            sub "APCSOSUB" (stat$,#4)

        dim                                                              ~
            stat$1,                      /* (Y)es OR (N)o - CLEAR DATA */~
            gl$2,                        /* GLASS CODE                 */~
            sc$1,                        /* SCREEN CODE                */~
            hdr_key$25,                  /* BCKMASTR - Primary Key     */~
            bck_key$19,                  /* BCKLINES - Primary Key     */~
            bck_cust$9,                  /* Customer Number            */~
            bck_ord$16,                  /* S.O. Number                */~
            bck_part$25,                 /* Part Number                */~
            save_so$16,                  /* S.O. Number                */~
            dte$6,                       /* Date S.O. Entered          */~
            apc_dte$6,                   /* Date S.O. Entered          */~
            yr$4,                        /* Current Year               */~
            chk_yr$4,                    /* Check Year Value           */~
            chk_mt$2,                    /* CHECK MONTH                */~
            date$10,                     /* Todays Date                */~
            model$3,                     /* Part Model Code            */~
            color$1,                     /* Part Color                 */~
            sys_key$20,                  /* SYSFILE2 KEY               */~
            apc_key$4,                   /* Cost Primary Key           */~
            apc_so$8,                    /* S.O. SAVE KEY              */~
            apc_qty(12),                 /* 12 Months of Quantities    */~
            apc_qtz(12),                 /* 12 Months Zero Price Qty's */~
            apc_pc(12),                  /* 12 Months of Prices        */~
            apc_fil$2,                   /* FILLER                     */~
            temp1$10, temp2$10           /* Working storage            */

        dim f2%(5),                      /* = 0 if the file is open    */~
            f1%(5),                      /* = 1 if READ was successful */~
            fs%(5),                      /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5)20                   /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Update Monthly Analysis Buckets   "
            pname$ = "APCSOSUB - Rev: R6.04"

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
            * #04 ! APCSOAVG ! MONTH AVERAGE UNIT COST FILE             *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #02,  "BCKMASTR",                                     ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #03,  "BCKLINES",                                     ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =   10, keylen =   19


            call "OPENCHCK" (#01, fs%(01), f2%(01),   0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02),   0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03),   0%, rslt$(03))

            f1%(1), f1%(2), f1%(3), f1%(4) = 0

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
                                         /* 1ST FIND LAST SALES ORDER */
            model$ = " "
            temp1$ = date
            call "DATFMTC" (temp1$, temp%, date$)
            yr$   = str(date$,1%,4%)     /* SAVE CURRENT YEAR         */
            chk_mt$ = str(date$,5%,2%)   /* SAVE CURRENT MONTH        */
            apc_dte$ = " "
            count% = 0%
            apc_so$ = all(hex(00))
            if stat$ = "N" then goto L10150
               gosub clear_data
                                      /* Look-up the Last S.O. Checked */
L10150:     sys_key$ = "**APC AVERAGE UNIT**"
             read #1,key = sys_key$,using L10170  ,apc_so$, eod goto L10190
L10170:        FMT XX(20),CH(8)

L10190:     call "SHOSTAT" ("Scanning Current Sales Orders")
            bck_key$ = all(hex(00))         /* Start Scan at Last S.O. */
            str(bck_key$,1%,8%) = apc_so$
            read #3, key > bck_key$, eod goto read_orders_done
            goto L10260
        read_orders                         /* Look-up - (BCKLINES)    */
            read #3, eod goto read_orders_done
L10260:        get #3, using L10280, bck_cust$, bck_ord$, bck_part$,      ~
                                    unit_qty, unit_price, ln_disc
L10280:        FMT CH(9), CH(16), XX(03), XX(03), CH(25), XX(32),        ~
                   XX(04), PD(14,4), POS(165), 2*PD(14,4)

            gosub skip_part
            if skip% <> 0% then goto read_orders

            total_price =  round(  unit_price * unit_qty, 2)
                                                     /* CALC LINE DISC */
            discamt = round(total_price * ln_disc * .01, 2)
            total_price = round(total_price - discamt, 2)

            if save_so$ = bck_ord$ then goto L10370
               count% = count% + 1%
               save_so$ = bck_ord$
               gosub read_header
L10370:     if yr$  <> chk_yr$ then goto read_orders   /* SKIP PRIOR YR */
        REM IF "92" <> CHK_YR$ THEN GOTO READ_ORDERS_DONE
            if month$ = chk_mt$ then goto read_orders_done
               gosub update_cost
            goto read_orders
        read_orders_done
            if model$ = " " then goto exit_program
            sys_key$ = "**APC AVERAGE UNIT**"
            read #1,hold,key = sys_key$,using L10450 ,sys_key$, apc_so$,  ~
                                                          eod goto L10470
L10450:          FMT CH(20),CH(8)
            delete #1
L10470:     apc_so$ = str(bck_ord$,1%,8%)
            write #1, using L10450, sys_key$, apc_so$
        goto exit_program

        skip_part
            skip% = 1%
            if len(bck_part$) < 19 then return
               gl$ = str(bck_part$,5%,2%)
               sc$ = str(bck_part$,11%,1%)
               if sc$ = "4" or sc$ = "5" or sc$ = "6" then return
                  gl% = 0%
                  convert gl$ to gl%, data goto L10590
L10590:
                  if gl% > 19% then return
            skip% = 0%
        return

        read_header                 /* CHK_YR$ = Year Order Entered    */
                                    /* MONTH$  = Month the Order Entere*/
            if mod(count%,50) <> 0 then goto L60050
               convert count% to count$, pic(000000)
               call "SHOSTAT" ("Number of Orders Scanned = "&count$)

L60050:     month% = 1%
            hdr_key$ = all(hex(00))
            str(hdr_key$,1%,9%)   = bck_cust$
            str(hdr_key$,10%,16%) = bck_ord$
            read #2,key = hdr_key$, using L60110, dte$, ord_disc,         ~
                                                       eod goto L60170
L60110:        FMT POS(830), CH(6), POS(859), PD(14,4)
            temp1$ = dte$
            call "DATFMTC" (temp1$, temp%, temp2$)
            chk_yr$ = str(dte$,1%,4%)
            month$  = str(dte$,5%,2%)
            convert month$ to month%, data goto L60150
L60150:
        return
L60170:     stop " UNABLE TO READ HEADER FOR ORDER --> "&bck_ord$
        return

        update_cost
            model$ = str(bck_part$,1%,3%)
            color$ = str(bck_part$,4%,1%)
            model% = 0%
            convert model$ to model%, data goto L60580
                                         /* CALC ORDER DISCOUNT */
            discamt = round(total_price * ord_disc * .01, 2)
            total_price = round(total_price - discamt, 2)

            apc_key$ = model$ & color$
            read #4,hold,key = apc_key$, using L60330,                    ~
                            apc_qty(), apc_pc(), apc_qtz(), apc_dte$,    ~
                                                 apc_fil$, eod goto L60440
L60330:        FMT POS(5), 12*PD(14,4), 12*PD(14,4), 12*PD(14,4), CH(6), ~
                                                                  CH(2)
            apc_qty(month%) = round(apc_qty(month%) + unit_qty, 2)
            apc_pc(month%)  = round(apc_pc(month%) + total_price, 2)
            if total_price < .01 then              /* ZERO PRICE */      ~
                   apc_qtz(month%) = round(apc_qtz(month%) + unit_qty, 2)

            rewrite #4, using L60330, apc_qty(), apc_pc(), apc_qtz(),     ~
                                                dte$,     apc_fil$
            goto L60580

L60440:       mat apc_qty = zer          /* TOTAL UNITS - ALL PRICES */
              mat apc_qtz = zer          /* TOTAL UNITS - ZERO PRICE */
              mat apc_pc  = zer
              apc_fil$ = " "
              apc_qty(month%) = round(apc_qty(month%) + unit_qty, 2)
              apc_pc(month%)  = round(apc_pc(month%) + total_price, 2)
              if total_price < .01 then            /* ZERO PRICE */      ~
                   apc_qtz(month%) = round(apc_qtz(month%) + unit_qty, 2)

              write #4, using L60560, model$, color$,                     ~
                                     apc_qty(), apc_pc(), apc_qtz(),     ~
                                                dte$,     apc_fil$
L60560:        FMT CH(3), CH(1), 12*PD(14,4), 12*PD(14,4), 12*PD(14,4),  ~
                                                           CH(6), CH(2)
L60580: return

        clear_data
           call "SHOSTAT" ("Clearing All Previous Data")
           call "DELETE" (#4, " ", 0%)

           sys_key$ = "**APC AVERAGE UNIT**"
           read #1,hold,key = sys_key$,using L60670 ,sys_key$, apc_so$,   ~
                                                          eod goto L60690
L60670:          FMT CH(20),CH(8)
            delete #1
L60690:     apc_so$ = all(hex(00))
            write #1, using L60670, sys_key$, apc_so$
        return

        exit_program

        end
