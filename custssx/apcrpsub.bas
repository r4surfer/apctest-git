        REM *************************************************************~
            *                  ( As Of - 11/12/97 - djd )               *~
            *                  (   USED IN - (APCRPSLS) )               *~
            *                  ( ALSO USED BY APCRPBLD  )               *~
            * APCRPSUB - Update Product Quantities and Sales for Monthly*~
            *            Buckets for each Model.                        *~
            *                                                           *~
            *     Note - Only Process Data for Sales Order If the       *~
            *            Sales Order was 'Invoiced'. Correction to      *~
            *            Invoice Date at (1650).                        *~
            *                                                           *~
            *     Note - (LOOKUP_CUSTOMER) Routine needs to be changed  *~
            *            if the ROUTE CODE is Modified.                 *~
            *                                                           *~
            * RECORD LAYOUT FOR - (APCRPTSA)                            *~
            *   1. CH(2)       -  Year Sales Order Invoiced             *~
            *   2. CH(15)      -  Customer Route Code                   *~
            *   3. CH(9)       -  Customer Code                         *~
            *   4. CH(3)       -  Product Model Code                    *~
            *   5. CH(1)       -  Product Color Code                    *~
            *   6. CH(2)       -  Year Sales Order Entered              *~
            *   7. CH(3)       -  Product Model Code                    *~
            *   8. CH(1)       -  Product Color Code                    *~
            *   9. CH(2)       -  Year Sales Order Invoiced             *~
            *  10. CH(9)       -  Customer Code                         *~
            *  11. CH(3)       -  Product Model Code                    *~
            *  12. CH(1)       -  Product Color Code                    *~
            *  13. 13*BI(2)    -  No. Sales Orders by Month, (13)YTD    *~
            *  14. 13*BI(2)    -  Product Qty by Month, (13)YTD         *~
            *  15. 13*PD(14,4) -  Product Sales Dollars by Month,(13)YTD*~
            *  16. CH(49)      -  Filler Area                           *~
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
            *          !                                          !     *~
            * 11/12/97 ! Revision Update For 60403                ! DJD *~
            *          ! Also added key for ARIMASTR              !     *~
            * 03/31/98 ! Y2K modifications                        ! ERN *~
            *************************************************************

            sub "APCRPSUB" (#4,#6)

        dim                                                              ~
            hdr$40,                      /* ASKUSER Header Text        */~
            msg$(3)79,                   /* ASKUSER Message Text       */~
            cnt$10,                      /* Counter                    */~
            gl$2,                        /* GLASS CODE                 */~
            sc$1,                        /* SCREEN CODE                */~
            hdr_key$25,                  /* BCKMASTR - Primary Key     */~
            bck_key$19,                  /* BCKLINES - Primary Key     */~
            bck_cust$9,                  /* Customer Number            */~
            bck_ord$16,                  /* S.O. Number                */~
            bck_part$25,                 /* Part Number                */~
            dte$6,                       /* Date S.O. Entered          */~
            sys_key$20,                  /* SYSFILE2 KEY               */~
            ar_key$8,                    /* (ARIMASTR) Alt Key (1)     */~
            apc_inv$8,                   /* Starting Invoice to Scan   */~
            apc_inv1$8,                  /* Start Previous Run         */~
            save_inv$8,                  /* LAST INVOICE NO USED UPD   */~
            temp1$10, temp2$10           /* Working storage            */

        dim f2%(6),                      /* = 0 if the file is open    */~
            f1%(6),                      /* = 1 if READ was successful */~
            fs%(6),                      /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(6)20                   /* Text from file opening     */

        dim sa_key$30,                   /* Primary Key                */~
            sa_yr$2,                     /* Year Sales Order Invoiced  */~
                                         /*  BINARY                    */~  
            sa_route$15,                 /* Customer Route Code        */~
            sa_cust$9,                   /* Customer Code              */~
            sa_model$3,                  /* Product Model Code         */~
            sa_color$1,                  /* Product Color Code         */~
            sa_orders%(13),              /* Product Sales Orders       */~
            sa_qty%(13),                 /* Product Quantity           */~
            sa_sales(13),                /* Product Sales Dollars      */~
            sa_filler$49,                /* Filler Area                */~
            dummy$15                     /* Dummy Variable - for Route */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 12/14/92 Pre-Release Version            "
        REM *************************************************************

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! SYSTEM MASTER FILE                       *~
            * #2  ! BCKMASTR ! S.O. MASTER HEADER FILE                  *~
            * #3  ! BCKLINES ! S.O. LINE DETAIL FILE                    *~
            * #4  ! APCRPTSA ! Master Sales File by Year/Month/Model/Col*~
            * #5  ! ARIMASTR ! Invoice Master File                      *~
            * #6  ! CUSTOMER ! Master File                              *~
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
                            key 4, keypos = 1783, keylen = 26

            call "OPENCHCK" (#1, fs%(1), f2%(1),   0%, rslt$(1))
            call "OPENCHCK" (#2, fs%(2), f2%(2),   0%, rslt$(2))
            call "OPENCHCK" (#3, fs%(3), f2%(3),   0%, rslt$(3))
            call "OPENCHCK" (#5, fs%(5), f2%(5),   0%, rslt$(5))

            f1%(1), f1%(2), f1%(3), f1%(4) = 0

        REM *************************************************************~
            *             C H E C K   O R D E R   S T A T U S           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************
                                         /* Update (APCRPTSA) File    */
            cnt% = 0% : save_inv$ = " "
            apc_inv$  = all(hex(00))           /* Last Invoice Scanned */
            apc_inv1$ = all(hex(00))     /* Prev. Scan Starting Invoice*/
                                   /* Look-up the Last Invoice Checked */
            sys_key$ = "**APC SALES  UNITS**"

            read #1,key = sys_key$,using L01490  ,apc_inv$, apc_inv1$,     ~
                                                          eod goto L01500
L01490:        FMT XX(20),CH(8), CH(8)
L01500:     ar_key$ = all(hex(00))
            apc_inv% = 0%
            convert apc_inv$ to apc_inv%, data goto L01560
                                                /* Only for Start-Up   */
                ar_key$   = apc_inv$
                apc_inv1$ = apc_inv$            /* Save Prev. Start Inv*/
L01560:     read #5,key 1% > ar_key$, using L01640 , bck_cust$, apc_inv$,  ~
                                      bck_ord$, dte$, eod goto finished
            call "SHOSTAT" ("Starting Scan with Inv ("& apc_inv$ &")")
            cnt% = apc_inv%
            goto L01660
        next_invoice
            read #5, using L01640 , bck_cust$, apc_inv$, bck_ord$, dte$,   ~
                                                      eod goto finished
L01640:        FMT CH(9), CH(8), XX(16), CH(16), POS(521), CH(6)
        REM SPECIAL CORRECTION
L01660:     if apc_inv$ < "00053411" or apc_inv$ > "00053932" then       ~
                                                                goto L01700
               temp1$ = "19920908"
               CALL "DATECONV"(temp1$)
               dte$ = temp1$  

L01700:     cnt% = cnt% + 1%
            print at(05,35);hex(84);"[";cnt%;"]"
                                           /* Exit 1st Finance Invoice */
            convert apc_inv$ to apc_inv%, data goto finished

            save_inv$ = apc_inv$
            gosub read_header         /* Calculate S.O. Line Item Data */
        REM IF MOD(CNT%, 5000) <> 0 THEN GOTO NEXT_INVOICE
        REM    GOSUB PROMPT_USER
        REM    IF COMP% = 0% THEN GOTO FINISHED

        goto next_invoice

        read_header
                                      /* BCK_CUST$ - S.O Customer Code */
                                      /* BCK_ORD$  - S.O. Number       */
                                      /* DTE$      - Date S.O. Invoiced*/
            if len(bck_ord$) < 8 then return
            if str(bck_ord$,1%,1%) = " " then return
            temp1$ = dte$
            call "DATFMTC" (temp1$, temp%, temp2$)
            convert str(temp2$,1,4) to temp%, data goto L01925
L01925:     sa_yr$ = bin(temp%, 2)
        REM sa_yr$ = str(dte$,1%,2%)             /* S.O. Invoice Year  */
        REM sa_mn$ = str(dte$,3%,2%)             /* S.O. Invoice Month */
            sa_mn$ = str(temp2$,5,2)
            sa_mn% = 1%
            convert sa_mn$ to sa_mn%, data goto L01930
L01930:
            hdr_key$ = all(hex(00))           /* Read (BCKMASTR) Header*/
            str(hdr_key$,1%,9%)   = bck_cust$ /* Need Discount for S.O.*/
            str(hdr_key$,10%,16%) = bck_ord$
            read #2,key = hdr_key$, using L01990 , ord_disc,eod goto L02030

L01990:        FMT POS(859), PD(14,4)
            order% = 1%                    /* Set S.O. Header Flag     */
            gosub scan_order               /* Calculate Each Line Item */
        return
L02030: REM STOP " Unable to Read Header for S.O. --> "&BCK_ORD$
        return

        scan_order
            gosub lookup_customer          /* Only once per S.O.-Route */
            bck_key$ = all(hex(00))
            str(bck_key$,1%,8%) = bck_ord$
            read #3, key > bck_key$, using  L02150, bck_key$,              ~
                                                       eod goto scan_done
            goto L02170
        scan_next                           /* Look-up - (BCKLINES)    */
            read #3, using  L02150, bck_key$, eod goto scan_done
L02150:        FMT POS(10), CH(19)

L02170:     if str(bck_key$,1%,8%) <> str(bck_ord$,1%,8%) then           ~
                                                           goto scan_done
               get #3, using L02210 , bck_part$, unit_qty, unit_price,     ~
                                                                  ln_disc
L02210:           FMT POS(32), CH(25), XX(32), XX(04), PD(14,4),         ~
                                                     POS(165), 2*PD(14,4)
            gosub skip_part
            if skip% <> 0% then goto scan_next
            total_price =  round(  unit_price * unit_qty, 2)
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
                  convert gl$ to gl%, data goto L02420
L02420:
                  if gl% > 19% then return
            skip% = 0%
        return

        update_cost                       /* Update File for Each Line */
            update% = 0%                  /* Item on S.O. When Applic  */
            sa_model$ = str(bck_part$,1%,3%)
            sa_color$ = str(bck_part$,4%,1%)
            sa_model% = 1%
                                          /* Skip Special Credits with */
                                          /* Invalid Model Code        */
            convert sa_model$ to sa_model%, data goto L03060
                                         /* Calc S.O. Discount from    */
                                         /* Header Data (Discount)     */
            discamt = round(total_price * ord_disc * .01, 2)
            total_price = round(total_price - discamt, 2)

            sa_key$ = all(hex(00))
            str(sa_key$,1%,2%)  = sa_yr$
            str(sa_key$,3%,15%) = sa_route$
            str(sa_key$,18%,9%) = sa_cust$
            str(sa_key$,27%,3%) = sa_model$
            str(sa_key$,30%,1%) = sa_color$

            read #4,hold,key = sa_key$, using L02700 , sa_orders%(),       ~
                                        sa_qty%(), sa_sales(),sa_filler$,~
                                                           eod goto L02740
L02700:        FMT POS(52), 13*BI(2), 13*BI(2), 13*PD(14,4), CH(49)
            update% = 1%                          /* Data Record Found */
            goto L02790
                                           /* New Entry in File (Zero) */
L02740:        mat sa_orders% = zer        /* Sales Orders by Month    */
               mat sa_qty%    = zer        /* Product QTY by Month     */
               mat sa_sales   = zer        /* Product Dollars by Month */
               sa_filler$ = " "

L02790:     if order% = 0% then goto L02840   /* Only Update for Headers */
               sa_orders%(sa_mn%) = sa_orders%(sa_mn%) + 1%
               sa_orders%(13%)    = sa_orders%(13%) + 1%
               order% = 0%

L02840:     sa_qty%(sa_mn%)    = sa_qty%(sa_mn%) + int(unit_qty)
            sa_qty%(13%)       = sa_qty%(13%) + int(unit_qty)

            sa_sales(sa_mn%)   = round(sa_sales(sa_mn%) + total_price, 2)
            sa_sales(13%)      = round(sa_sales(13%) + total_price, 2)

            if update% = 0% then goto L02960
               put #4, using L02700 , sa_orders%(), sa_qty%(), sa_sales(), ~
                                    sa_filler$
               rewrite #4
               goto L03060

L02960:     write #4, using L03030 , sa_yr$, sa_route$, sa_cust$,          ~
                                   sa_model$, sa_color$,                 ~
                                   sa_yr$, sa_model$, sa_color$,         ~
                                   sa_yr$, sa_cust$, sa_model$,          ~
                                   sa_color$, sa_orders%(),              ~
                                   sa_qty%(), sa_sales(), sa_filler$,    ~
                                   eod goto L03070
L03030:        FMT CH(2), CH(15), CH(9), CH(3), CH(1), ch(2), CH(3),     ~
                   CH(1), CH(2), CH(9), CH(3), CH(1),                    ~
                   13*BI(2), 13*BI(2), 13*PD(14,4), CH(49)
L03060: return
L03070:     stop "(Error) Updating SA Index --> "& sa_key$
        return

        finished
            if save_inv$ = " " then goto exit_program
            sys_key$ = "**APC SALES  UNITS**"
            read #1,hold,key = sys_key$,using L03150  ,sys_key$, apc_inv$, ~
                                                  apc_inv$, eod goto L03170
L03150:          FMT CH(20), CH(8), CH(8)
            delete #1
L03170:     apc_inv$ = save_inv$
            write #1, using L03150 , sys_key$, apc_inv$, apc_inv1$
            goto exit_program

        lookup_customer
            init(" ") sa_route$, sa_cust$
            sa_cust$ = bck_cust$
            read #6,key = sa_cust$, using L03250 , dummy$, eod goto L03270
L03250:        FMT POS(980), CH(15)
            sa_route$ = dummy$
L03270:     if len(sa_route$) < 5% then sa_route$ = "00000"
        return

        prompt_user
            convert cnt% to cnt$, pic(000000)

            comp% = 2%
            hdr$ = "*** Pause *** at (  "& cnt$ &"  ) "
            msg$(1) = " **********  Do You Wish to Continue  ********** "
            msg$(2) = "Press <RETURN> To 'Exit Process', or Press Any   "
            msg$(3) = "(PF) Key To Continue...........                  "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        exit_program
            close #1
            close #2
            close #3
            close #5
        end
