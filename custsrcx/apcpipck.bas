        REM *************************************************************~
            * (APCPIPCK) - Utility Program - As Of - 11/13/97           *~
            *                                                           *~
            * Note - (Delete Pip's) Will only be Available When Logged  *~
            *        in with Userid (RHH) This is to clear Hung Loads.  *~
            *                                                           *~
            *************************************************************

        dim readkey$56,                  /*                            */~
            errormsg$79,                 /* Error Message Text         */~
            inpmessage$79,               /* Data Input Prompt          */~
            hdr$40,                      /* Askuser                    */~
            msg$(3%)79,                  /* Askuser                    */~
            rpt_time$8,                  /* Time of Day                */~
            company$40,                  /* Company Name               */~
            print_title$50,              /* Report Title               */~
            txt$79,                      /* DISPLAY HEADER             */~
            tag$(100%)79,                /* DISPLAY SCREEN             */~
            inv_on_hand$5,               /* On-Hand Quantity (Inventory*/~
            sel$1,                       /* DELETE SELECTION           */~
            value$8,                     /* S.O. OR LOAD NUMBER        */~
            userid$3,                    /* USER ID                    */~
            pf$(3%)79,                   /* Pfkey Descriptions         */~
            i$(24%)80,                   /* Screen Display Text        */~
            cursor%(2%),                 /* Cursor Position Row/Col    */~
            pfkeys$32                    /* PFKEY On or OFF            */

        dim pull_rec$64,                 /* = PULL STOCK RECORD        */~
            pull_cust$9,                 /* = CUSTOMER CODE            */~
            pull_so$8,                   /* = SALES ORDER NUMBER       */~
            pull_ln$2,                   /* = S.O. LINE ITEM NO.       */~
            pull_part$25,                /* = S.O. STOCK PART NUMBER   */~
            save_part$25,                /* = S.O. STOCK PART NUMBER   */~
            t_part$25,                   /* = S.O. STOCK PART NUMBER   */~
            pull_desc$32,                /* = STOCK PART DESC          */~
            t_desc$32,                   /* = STOCK PART DESC          */~
            pull_date$6,                 /* = DATE CREATED             */~
            pull_dte$8,                  /* =   "     "     FORMATTED  */~
            pull_qty$5,                  /* = PULL STOCK QTY           */~
            t_pull_qty$5,                /* = TOTAL FOR SEL            */~
            pull_oh$5,                   /* ON-HAND AT TIME OF PULL    */~
            t_pull_oh$5,                 /* = TOTAL FOR SEL            */~
            pull_fil$5,                  /* = FILLER AREA              */~
            pull_key$25                  /* KEY                        */

        dim f2%(9%),                     /* = 0 if the file is open    */~
            f1%(9%),                     /* = 1 if READ was successful */~
            fs%(9%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(9%)20                  /* Text from file opening     */


            select #1,  "APCPULLS",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =  10,  keylen =  10,                     ~
                        alt key  1, keypos  =     1, keylen = 19,        ~
                            key  2, keypos  =    20, keylen = 25, dup

            select #5,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #7,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =   650,             ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #8,  "APCSTOCK",                                      ~
                        varc,     indexed,  recsize =  70,               ~
                        keypos =    1, keylen =  32,                     ~
                        alt key  1, keypos =   8, keylen = 32

            select #9,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  1,   keylen =  25,                     ~
                        alt key  1, keypos  =   102, keylen =  9, dup,   ~
                            key  2, keypos  =    90, keylen =  4, dup,   ~
                            key  3, keypos  =    26, keylen = 32, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#7, fs%(7%), f2%(7%),  0%, rslt$(7%))
            call "OPENCHCK" (#8, fs%(8%), f2%(8%),  0%, rslt$(8%))
            call "OPENCHCK" (#9, fs%(9%), f2%(9%),  0%, rslt$(9%))

            dim apc$40, pname$21
            apc$   = "(EWD) Pull from Stock Utility Program   "
            pname$ = "APCPIPCK - Rev: R6.04"

            call "EXTRACT" addr("ID", userid$)
        begin
            init(" ") pull_cust$, pull_so$, pull_ln$, pull_part$,        ~
                      pull_date$, pull_dte$, pull_load$, pull_qty$,      ~
                      pull_oh$, pull_fil$, readkey$, pull_key$,          ~
                      t_pull_qty$, t_pull_oh$, date$, tag$(),            ~
                      inv_on_hand$, sel$, value$, t_bal_qty$
            txt$ =                                                       ~
               "Customer  S.O. No. Ln  <----- Part Number -----> Load "
            txt$ = txt$ & " Pul QTY OH-Hand ENT  DTE"
                                                   /* Cust - (1%,9%)   */
                                                   /* S.O. - (11%,8%)  */
                                                   /* Ln   - (20%,2%)  */
                                                   /* Part - (24%,25%) */
                                                   /* Load - (50%,5%)  */
                                                   /* PQTY - (57%,5%)  */
                                                   /* OH   - (65%,5%)  */
                                                   /* Date - (71%,8%)  */
            p%, p_max% = 0%
            date$ = date
            date% = 0%
            flg%  = 0%                             /* ALPHA LOAD FLAG  */
            call "DATEOK" (date$, date%, errormsg$)
        begin_next
            gosub display_screen

            if keyhit% = 1% then goto begin
            if keyhit% = 9% then goto begin_process
            if keyhit% = 12% then gosub delete_pulls
            if keyhit% = 14% then gosub generate_report
            if keyhit% = 16% then goto exit_program
            gosub check_data
            if errormsg$ <> " " then goto begin_next
            goto begin_next

        begin_process
            call "SHOSTAT" ("Checking Pull From Stock Detail")
            if pull_part$ <> " " then goto L01320
               goto begin_next

L01320:     gosub calc_hnyquan_data
            gosub scan_pulls
        goto begin_next

        check_data
           errormsg$ = " "
           if pull_part$ <> " " then goto L01410
              gosub lookup_part
                                                   /* CHECK PART NUMBER */
L01410:    if len(pull_part$) > 18 then goto L01440
              errormsg$ = "(ERROR) Invalid Part Number (Required)."
              pull_part$ = " "
L01440: return

        calc_hnyquan_data
          readkey$ = all(hex(00)) : inv_on_hand = 0.0
          str(readkey$,1%,25%) = pull_part$
          read #7,key > readkey$, using L01540, readkey$, on_hand,         ~
                                                            eod goto L01580
          goto L01550
        calc_next
          read #7, using L01540, readkey$, on_hand
L01540:       FMT POS(17), CH(44), POS(69), PD(14,4)
L01550:   if str(readkey$,1%,25%) <> pull_part$ then goto L01580
             inv_on_hand = inv_on_hand + on_hand
             goto calc_next
L01580:   inv_on_hand% = int(inv_on_hand)
          convert inv_on_hand% to inv_on_hand$, pic(####-)
          gosub get_description
        return

        scan_pulls
            t_pull_qty%, t_pull_oh% = 0%
            init(" ") tag$(), save_part$
            save_part$ = pull_part$
            pull_key$ = all(hex(00))
            read #1,key > pull_key$ using L01730, pull_rec$,               ~
                                                       eod goto scan_done
            goto L01740
        scan_next
            read #1, using L01730, pull_rec$, eod goto scan_done
L01730:        FMT CH(64)
L01740:     if str(pull_rec$,20%,25%) <> save_part$ then goto scan_next

               pull_cust$ = str(pull_rec$,1%,9%)
               pull_so$   = str(pull_rec$,10%,8%)
               pull_ln$   = str(pull_rec$,18%,2%)
               pull_part$ = str(pull_rec$,20%,25%)
               pull_date$ = str(pull_rec$,45%,6%)
               pull_load$ = str(pull_rec$,51%,5%)
               pull_dte$  = pull_date$
               call "DATEFMT" (pull_dte$)
               get str(pull_rec$,56%,4%), using  L01860, pull_qty%,        ~
                                                               pull_oh%
L01860:           FMT 2*BI(2)
               convert pull_qty% to pull_qty$, pic(####-)
               convert pull_oh%  to pull_oh$,  pic(####-)

               t_pull_qty% = t_pull_qty% + pull_qty%
               t_pull_oh%  = t_pull_oh% + pull_oh%
               gosub load_display
               goto scan_next
        scan_done
               convert t_pull_qty% to t_pull_qty$, pic(####-)
               convert t_pull_oh%  to t_pull_oh$,  pic(####-)
               t_bal_qty% = inv_on_hand% - t_pull_qty%
               convert t_bal_qty% to t_bal_qty$, pic(####-)

          p% = 0%
        return

        lookup_part
           init(" ") errormsg$, pull_part$, pull_desc$
           pull_desc$ = hex(06) & "Select a Valid Part"
           call "GETCODE" (#9, pull_part$, pull_desc$, 0%, 1.30, f1%(9))
           pull_desc$ = " "
           if f1%(9) = 0 then pull_part$ = " "
        return

        get_description
           read #9,key = pull_part$, using L02130,pull_desc$,eod goto L02150
L02130:        FMT XX(25), CH(32)
        return
L02150:    pull_desc$ = " "
        return


        load_display
            p% = p% + 1%
            str(tag$(p%),1%,9%)   = pull_cust$
            str(tag$(p%),11%,8%)  = pull_so$
            str(tag$(p%),20%,2%)  = pull_ln$
            str(tag$(p%),24%,25%) = pull_part$
            str(tag$(p%),50%,5%)  = pull_load$
            str(tag$(p%),57%,5%)  = pull_qty$
            str(tag$(p%),65%,5%)  = pull_oh$
            str(tag$(p%),71%,8%)  = pull_dte$
            p_max% = p_max% + 1%
        return

        REM *************************************************************~
            *                   F O R M A T S                           *~
            *************************************************************
                                         /* (APCPULLS) - FILE          */
            FMT CH(09),                  /* Customer Code              */~
                CH(08),                  /* Sales Order Number         */~
                CH(02),                  /* S.O. Line Item Number      */~
                CH(25),                  /* Stock Part Number          */~
                CH(06),                  /* Date Record Created        */~
                CH(05),                  /* Original Load              */~
                BI(2),                   /* Pull Quantity              */~
                BI(2),                   /* On-Hand Qty at Time of Pull*/~
                CH(05)                   /* Filler Area                */

        REM *************************************************************~
            *               D I S P L A Y   S C R E E N S               *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        display_screen
              gosub set_pf_keys

L60090:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Stock Part No. (Required):",                 ~
               at (03,30), fac(hex(81)),   pull_part$           , ch(25),~
                                                                         ~
               at (04,02), "Stock Pull Qty: ",                           ~
               at (04,20), fac(hex(84)),  t_pull_qty$           , ch(05),~
                                                                         ~
               at (04,30), "Current On-Hand Qty: ",                      ~
               at (04,52), fac(hex(84)),  inv_on_hand$          , ch(05),~
                                                                         ~
               at (04,65), "Balance: ",                                  ~
               at (04,75), fac(hex(84)),  t_bal_qty$            , ch(05),~
                                                                         ~
               at (06,02), fac(hex(a4)), txt$                   , ch(79),~
               at (07,02), fac(hex(84)), tag$(p% +  1%)         , ch(79),~
               at (08,02), fac(hex(84)), tag$(p% +  2%)         , ch(79),~
               at (09,02), fac(hex(84)), tag$(p% +  3%)         , ch(79),~
               at (10,02), fac(hex(84)), tag$(p% +  4%)         , ch(79),~
               at (11,02), fac(hex(84)), tag$(p% +  5%)         , ch(79),~
               at (12,02), fac(hex(84)), tag$(p% +  6%)         , ch(79),~
               at (13,02), fac(hex(84)), tag$(p% +  7%)         , ch(79),~
               at (14,02), fac(hex(84)), tag$(p% +  8%)         , ch(79),~
               at (15,02), fac(hex(84)), tag$(p% +  9%)         , ch(79),~
               at (16,02), fac(hex(84)), tag$(p% + 10%)         , ch(79),~
               at (17,02), fac(hex(84)), tag$(p% + 11%)         , ch(79),~
               at (18,02), fac(hex(84)), tag$(p% + 12%)         , ch(79),~
               at (19,02), fac(hex(84)), tag$(p% + 13%)         , ch(79),~
               at (20,02), fac(hex(84)), tag$(p% + 14%)         , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L60550
                  p% = 0%
                  goto display_screen

L60550:        if keyhit% <> 3% then goto L60590
L60560:           x% = int( p_max%/14.0 )
                  p% = x%
                  goto display_screen
L60590:        if keyhit% <> 5% then goto L60640
                  p% = p% + 14%
                  if p% > p_max% then goto L60560
                  goto display_screen

L60640:        if keyhit% <> 13% then goto L60680
                  gosub lookup_part
                  goto display_screen

L60680:        if keyhit% <> 15 then goto L60720
                  call "PRNTSCRN"
                  goto L60090

L60720:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf_keys
            u3% = 0%
            inpmessage$ = "Enter a Valid APC Stock Part Number.        "
            pf$(1) = "(1)Start Over          (5)Next          " &        ~
                     "(9)Load Detail         (14)Print Report"
            pf$(2) = "(2)First               (12)Delete Pull's" &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(3)Last                (13)Lookup Part  " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(010203ff05ffffff09ffff0c0d0e0f1000)
            if p_max% <> 0% then goto L60900
               str(pf$(1),24%,10%) = " " : str(pfkeys$,5%,1%) = "FF"
               str(pf$(2), 1%,10%) = " " : str(pfkeys$,2%,1%) = "FF"
               str(pf$(3), 1%,10%) = " " : str(pfkeys$,3%,1%) = "FF"
L60900:     if p_max% > 14% then goto L60930
               str(pf$(2), 1%,10%) = " " : str(pfkeys$,2%,1%) = "FF"
               str(pf$(3), 1%,10%) = " " : str(pfkeys$,3%,1%) = "FF"
L60930:     if (p_max% - p%) > 14% then goto L60960
               str(pf$(1),24%,10%) = " " : str(pfkeys$,5%,1%) = "FF"
               str(pf$(3), 1%,10%) = " " : str(pfkeys$,3%,1%) = "FF"
L60960:     if pull_part$ <> " " then goto L60980
               str(pf$(1),41%,20%) = " " : str(pfkeys$,9%,1%) = "FF"
L60980:     return

        delete_screen
            gosub set_keys

L61030:     accept                                                       ~
               at (01,02),                                               ~
                  "Delete Pull's from Stock, By 'Load' or 'Sales Order'",~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,10), "Selection : ",                               ~
               at (06,30), fac(hex(81)),   sel$                 , ch(01),~
                                                                         ~
               at (08,10), "Delete Value : ",                            ~
               at (08,30), fac(hex(81)),  value$                , ch(08),~
                                                                         ~
               at (10,10), "(1) - Enter a Sales Order Number.",          ~
               at (11,10), "(2) - Enter a Valid Load Number.",           ~
                                                                         ~
               at (15,10), "Note: All Pull's From Stock Will be ",       ~
               at (16,10), "      Purged for the Applicable Selection",  ~
               at (17,10), "      Entered.                           ",  ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L61340
                  call "PRNTSCRN"
                  goto L61030

L61340:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_keys
            u3% = 0%
            inpmessage$ = "Enter a Valid Selection (1), or (2).        "
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Purge Data  "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffff0c0d0e0f1000)
            return


        select_printer
            page_no% = 0%
            lcnt%    = 99%

         print_title$ = "Pull From Stock Report (As of To Day)"
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCSTK", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCSTK", " ", 0%, 1%)
        return

        generate_report
            rpt_load$ = "ALL  "
L61680:     gosub report_screen
            if keyhit% = 1% then goto begin
            if keyhit% = 16% then goto exit_program
            if keyhit% <> 14% then goto L61680

            save_part$ = " "
            call "SHOSTAT" ("Printing Report ..........")
            gosub select_printer
            pull_key$ = all(hex(00))
            read #1,key 2% > pull_key$, using L61860, pull_rec$,          ~
                                                   eod goto generate_done
            save_part$ = str(pull_rec$,20%,25%)
            pull_part$ = save_part$
            gosub calc_hnyquan_data

            goto L61875
        generate_next
            read #1, using L61860, pull_rec$, eod goto generate_done
L61860:      FMT CH(64)

L61875:        pull_load$ = str(pull_rec$,51%,5%)
               if str(rpt_load$,1%,3%) = "ALL" then goto L61880
                  if pull_load$ <> rpt_load$ then goto generate_next

L61880:        pull_cust$ = str(pull_rec$,1%,9%)
               pull_so$   = str(pull_rec$,10%,8%)
               pull_ln$   = str(pull_rec$,18%,2%)
               pull_part$ = str(pull_rec$,20%,25%)
               pull_date$ = str(pull_rec$,45%,6%)
               pull_dte$  = pull_date$

               call "DATEFMT" (pull_dte$)
               get str(pull_rec$,56%,4%), using  L62000, pull_qty%,       ~
                                                       pull_oh%
L62000:           FMT 2*BI(2)
               convert pull_qty% to pull_qty$, pic(####-)
               convert pull_oh%  to pull_oh$,  pic(####-)

            gosub print_detail
            goto generate_next
        generate_done
            gosub print_totals
            print using L62680
            gosub close_printer
        return clear all
        goto begin

        print_header
            if lcnt% <> 99% then print using L62680
            page_no% = page_no% + 1%
            print page
            print using L62620, date$, rpt_time$, company$, page_no%
            print
            print using L62650, print_title$
            print
            print using L62680
            print using L62740
            lcnt% = 6%
            t_part$ = pull_part$
            t_desc$ = pull_desc$
        return

        print_detail
            if lcnt% > 60% then gosub print_header
            if save_part$ = pull_part$ then goto L62360
               gosub print_totals

L62360:     print using L62800
            print using L62770, t_part$, t_desc$, pull_cust$,             ~
                        pull_so$, pull_ln$, pull_load$, pull_qty$,       ~
                        pull_oh$, pull_dte$
            lcnt% = lcnt% + 2%
            t_pull_qty% = t_pull_qty% + pull_qty%
            t_part$, t_desc$ = " "
        return

        print_totals
            error$ = " "
            if t_pull_qty% > inv_on_hand% then error$ = "* PROBLEM **"

            convert t_pull_qty% to t_pull_qty$, pic(####-)
            t_bal_qty% = inv_on_hand% - t_pull_qty%
            convert t_bal_qty% to t_bal_qty$, pic(####-)
            print using L62800
            print using L62830, t_bal_qty$,t_pull_qty$,inv_on_hand$,error$
            lcnt% = lcnt% + 2%
            save_part$ = pull_part$
            gosub calc_hnyquan_data
            t_pull_qty% = 0%
            t_part$ = pull_part$
            t_desc$ = pull_desc$
        return

L62620: % ######## @ ########                        ####################~
        ~#####################                                  PAGE: ###

L62650: %                                        ########################~
        ~####################################

L62680: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+

        %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!

L62740: %!<---- Part Number ------>!<------ Part Description ------>!Cust~
        ~omer Cd! S.O. No. !Line!Load No!Pull Stk ! On_Hand ! Date Sched.!

L62770: %!#########################!################################! ###~
        ~###### ! ######## ! ## ! ##### !  #####  !  #####  !  ########  !

L62800: %!-------------------------!--------------------------------!----~
        ~-------!----------!----!-------!---------!---------!------------!

L62830: %! Totals for Part  -----> !( Remaining Balance : ##### )   !    ~
        ~       !          !    !       !  #####  !  #####  !############!


        delete_pulls
           cnt1%, cnt2% = 0%
           init(" ") cnt1$, cnt2$
           gosub delete_screen
           if keyhit% = 1% then goto delete_done
           if keyhit% = 16% then goto exit_program
              convert sel$ to sel%, data goto L63290

              convert value$ to value%, data goto L63290

              convert value% to value$, pic(00000000)

              if sel% < 1% or sel% > 2% then goto L63290

L63010:    if keyhit% <> 14% then goto delete_pulls

           if flg% = 0% then goto L63060
              stop "DELETING DATA FOR ALPHA LOAD ---> " & value$

L63060:     call "SHOSTAT" ("Deleting Pull's From Stock")
            pull_key$ = all(hex(00))
        delete_next
            read #1,hold,key > pull_key$ using L63110  , pull_rec$,       ~
                                                     eod goto delete_done
L63110:        FMT CH(64)
            cnt1% = cnt1% + 1%
            pull_key$ = str(pull_rec$,10%,10%)
            if sel$ <> "1" then goto L63190
                                                         /* CHECK S.O. */
               if value$ <> str(pull_rec$,10%,8%) then goto delete_next
                  goto L63210
                                                         /* CHECK LOAD */
L63190:     if str(value$,4%, 5%) <> str(pull_rec$,51%,5%) then          ~
                                                         goto delete_next
L63210:        delete #1
               cnt2% = cnt2% + 1%
               goto delete_next
        delete_done
            gosub prompt_done
        return clear all
        goto begin

L63290:     if sel% <> 2% then goto L63390
               if str(value$,1%,1%) <> "A" and str(value$,4%,1%) <> "A"  ~
                                               then goto L63390
                  if str(value$,1%,1%) = "A" then                        ~
                                       value$ = "000" & str(value$,1%,5%)
                  if str(value$,4%,1%) = "A" then                        ~
                                       value$ = "000" & str(value$,4%,5%)
                  flg% = 1%
                  goto L63010

L63390:     errormsg$ = "(ERROR) - INVALID SELECTION OR SELECTION VALUE."
            init(" ") sel$, value$
            goto delete_pulls

        prompt_done
            convert cnt1% to cnt1$, pic(00000)
            convert cnt2% to cnt2$, pic(00000)
            comp% = 2%
            hdr$ = "** Pull From Stock Delete ***"
            msg$(1) = " Total Records Scanned = " & cnt1$
            msg$(2) = " Total Records Deleted = " & cnt2$
            msg$(3) = "Press Any Key To Continue......................."
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return


        report_screen
            gosub set_keys_report

L63580:     accept                                                       ~
               at (01,02),                                               ~
                  "Select Options for Printing Report",                  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Enter Load Number or (ALL):",                ~
               at (06,30), fac(hex(81)),   rpt_load$            , ch(05),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L63790
                  call "PRNTSCRN"
                  goto L63580

L63790:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_keys_report
            u3% = 0%
            inpmessage$ = "Enter a Valid Load, Including Leading Zero's"
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffff0c0d0e0f1000)
            return

        exit_program

        end

