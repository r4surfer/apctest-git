        REM *-----------------------------------------------------------*~
            *                                                           *~
            * AAAAAA  PPPP   CCCC  EEEE  DDDD  IIIIII   0000   77777    *~
            * AA  AA  PP  P CC     EE    DD  D   II    0    0     77    *~
            * AAAAAA  PPPP  CC     EEEE  DD  D   II    0    0    77     *~
            * AA  AA  PP    CC     EE    DD  D   II    0    0   77      *~
            * AA  AA  PP     CCCC  EEEE  DDDD  IIIIII   0000   77       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCEDI07 - Utility To View and Modify Invoice Records     *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/04/94 ! Original                                 ! VRW *~
            *          !                                          !     *~
            * 01/27/98 ! y2k compliance                           ! djd *~
            *-----------------------------------------------------------*

        dim stat_code$ 01,          /*                                 */~
            rec_type$  01,          /*                                 */~
            proc_dte$  04,          /*                                 */~
            edi_id$    03,          /*                                 */~
            edi_inv$   08,          /*                                 */~
            edi_seq$   03,          /*                                 */~
            rec_code$  02,          /*                                 */~
            inv$       08,          /*                                 */~
            inv_dte$   06,          /*                                 */~
            inv_amt$   10,          /*                                 */~
            ship_dte$  06,          /*                                 */~
            po_dte$    06,          /*                                 */~
            nd_dte$    06,          /*                                 */~
            dd_dte$    06,          /*                                 */~
            freight$   08,          /*                                 */~
            disc_per$  08,          /*                                 */~
            sales_tax$ 08,          /*                                 */~
            contl_nbr$ 09,          /*                                 */~
            tran_dte$  06,          /*                                 */~
            cust$      09,          /*                                 */~
            ship_via$  20,          /*                                 */~
            po_num$    16,          /*                                 */~
            terms$     20,          /*                                 */~
            line_nbr$  06,          /*                                 */~
            unit_pri$  08,          /*                                 */~
            quantity$  10,          /*                                 */~
            uom$       02,          /*                                 */~
            buy_prt$   25,          /*                                 */~
            ven_prt$   25,          /*                                 */~
            item_desc$ 32,          /*                                 */~
            contact$   35,          /*                                 */~
            phone$     21,          /*                                 */~
            errormsg$  79,          /*                                 */~
            sav_cust$  09,          /*                                 */~
            hdr_key$   13,          /*                                 */~
            li_key$    13,          /*                                 */~
            old_li_key$22,          /*                                 */~
            old_hdr_key$22,         /*                                 */~
            old_nme_key$22,         /*                                 */~
            hdr_lin1$  79,          /*                                 */~
            hdr_lin2$  79,          /*                                 */~
            hdr_lin3$  79,          /*                                 */~
            hdr_lin4$  79,          /*                                 */~
            line1$     79,          /*                                 */~
            line2$     79,          /*                                 */~
            line3$     79,          /*                                 */~
            line4$     79,          /*                                 */~
            line5$     79,          /*                                 */~
            line6$     79,          /*                                 */~
            rslt$(1)   20,          /*                                 */~
            f2%(1),                 /*                                 */~
            fs%(1),                 /*                                 */~
            type_name$ 02,          /*                                 */~
            ori_id$    17,          /*                                 */~
            add_nme$   30,          /*                                 */~
            add_city$  18,          /*                                 */~
            add_state$ 02,          /*                                 */~
            add_zip$   05,          /*                                 */~
            add_lin1$  30,          /*                                 */~
            add_lin2$  30,          /*                                 */~
            nme_key$   13           /*                                 */~


	dim blankdate$8,            /* Used for empty date compares   */~
            today$10,               /* displaying todays date         */~
            wrk_invdte$10,          /* Input Date For invoice date    */~
            wrk_shpdte$10,          /* Input date for ship date       */~
            wrk_podte$10,           /* Input date for PO date         */~
            wrk_nddte$10,           /* Input date for net due date    */~
            wrk_dddte$10,           /* Input date for discount due    */~
            wrk_trndte$10           /* Input date for transact. date  */

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCEDIMC ! APC EDI Master Control History File      *~
            *************************************************************~
            *              File Selection and Open Calls                *~
            *************************************************************

            select #1, "APCEDIMC",                                       ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   1 , keylen =  22,                     ~
                        alt key    1 , keypos =  10, keylen =  13, dup,  ~
                            key    2 , keypos = 233, keylen =  06, dup

            call "SHOSTAT" ("Opening EDI Files...")
            call "OPENCHCK" (#1, fs%(1), f2%(1),     0%, rslt$(1))

        hdr_lin1$ = "EDI Invoice Editor                                  ~
        ~             APCEDI07"
        hdr_lin2$ = "EDI Invoice Editor - HEADER                         ~
        ~             APCEDI07"
        hdr_lin3$ = "EDI Invoice Editor - LINE ITEM                      ~
        ~             APCEDI07"
        hdr_lin4$ = "EDI Invoice Editor - NAME                           ~
        ~             APCEDI07"
        line1$ = "Modify Required Data and Press <Return>."
        line2$ = "PF1 - Start Over      PF5 - Next Line Item             ~
        ~     PF15 - Print Screen"
        line3$ = "                                                       ~
        ~     PF16 - Exit"
        line4$ = "Enter Invoice and Customer and Press <Return>."
        line5$ = "PF1 - Start Over                                       ~
        ~     PF15 - Print Screen"
        line6$ = "PF1 - Start Over      PF5 - Name Record                ~
        ~     PF15 - Print Screen"
        pfkeys$ = hex(0001050f10)
        today$=date
        call "DATEFMT" (today$)

        show_screen1
        accept                                                           ~
               at (01,65), "TODAY",                                      ~
               at (01,71), fac(hex(84)), today$     , ch(10),            ~
               at (02,02), fac(hex(a4)), hdr_lin1$  , ch(79),            ~
               at (03,02), fac(hex(94)), errormsg$  , ch(79),            ~
               at (04,03), "Invoice Nbr",                                ~
               at (04,15), fac(hex(82)), inv$       , ch(08),            ~
               at (04,27), "Customer",                                   ~
               at (04,36), fac(hex(81)), cust$      , ch(09),            ~
               at (21,02), fac(hex(a4)), line4$     , ch(79),            ~
               at (22,02), fac(hex(84)), line5$     , ch(79),            ~
               at (23,02), fac(hex(84)), line3$     , ch(79),            ~
               keys(pfkeys$), key(keyhit%)

               errormsg$ = " "

               if keyhit% <>0% then L01470
                     hdr_key$=hex(00)
                     str(hdr_key$,1,8)=inv$
                     gosub load_header
                     if errormsg$<>" " then show_screen1
                  goto show_screen2

L01470:        if keyhit% <> 1%  then L01510
                  init(" ") inv$, cust$
                  goto show_screen1

L01510:        if keyhit% = 15%  then call "PRNTSCRN"

               if keyhit% = 16%  then exit_program
           goto show_screen1

        show_screen2

        /* Y2K */
        gosub fix_dates

        accept                                                          ~
               at (01,65), "TODAY",                                      ~
               at (01,71), fac(hex(84)), today$     , ch(10),            ~
               at (02,02), fac(hex(a4)), hdr_lin2$  , ch(79),            ~
               at (03,02), fac(hex(94)), errormsg$  , ch(79),            ~
               at (04,03), "Status Code",                                ~
               at (04,15), fac(hex(81)), stat_code$ , ch(01),            ~
               at (04,24), "Record Type",                                ~
               at (04,36), fac(hex(81)), rec_type$  , ch(01),            ~
               at (04,47), "Proc Dte",                                   ~
               at (04,56), fac(hex(81)), proc_dte$  , ch(04),            ~
               at (04,65), "EDI ID",                                     ~
               at (04,72), fac(hex(81)), edi_id$    , ch(03),            ~
               at (05,03), "EDI Invoice",                                ~
               at (05,15), fac(hex(84)), edi_inv$   , ch(08),            ~
               at (05,24), "EDI Sequnce",                                ~
               at (05,36), fac(hex(84)), edi_seq$   , ch(03),            ~
               at (05,47), "Rec Code",                                   ~
               at (05,56), fac(hex(84)), rec_code$  , ch(02),            ~
               at (05,65), "Inv.",                                       ~
               at (05,72), fac(hex(84)), inv$       , ch(08),            ~
               at (06,03), "Inv Dte",                                    ~
               at (06,15), fac(hex(81)), wrk_invdte$, ch(10),            ~
               at (06,24), "Inv Amt",                                    ~
               at (06,36), fac(hex(81)), inv_amt$   , ch(10),            ~
               at (06,47), "Ship Dte",                                   ~
               at (06,56), fac(hex(81)), wrk_shpdte$, ch(10),            ~
               at (06,65), "PO Dte",                                     ~
               at (06,72), fac(hex(81)), wrk_podte$ , ch(10),            ~
               at (07,03), "Net Due Dte",                                ~
               at (07,15), fac(hex(81)), wrk_nddte$ , ch(10),            ~
               at (07,24), "Disc Date",                                  ~
               at (07,36), fac(hex(81)), wrk_dddte$ , ch(10),            ~
               at (07,47), "Freight",                                    ~
               at (07,56), fac(hex(81)), freight$   , ch(08),            ~
               at (07,65), "Disc %",                                     ~
               at (07,72), fac(hex(81)), disc_per$  , ch(08),            ~
               at (08,03), "Sales Tax",                                  ~
               at (08,15), fac(hex(81)), sales_tax$ , ch(08),            ~
               at (08,24), "Contl Nbr",                                  ~
               at (08,36), fac(hex(81)), contl_nbr$ , ch(09),            ~
               at (08,47), tran Dte",                                   ~
               at (08,56), fac(hex(81)), wrk_trndte$, ch(10),            ~
               at (08,65), "Cust",                                       ~
               at (08,72), fac(hex(81)), cust$      , ch(09),            ~
               at (10,10), "Ship Via",                                   ~
               at (10,22), fac(hex(81)), ship_via$  , ch(20),            ~
               at (10,49), "PO Number",                                  ~
               at (10,59), fac(hex(81)), po_num$    , ch(16),            ~
               at (11,10), "Terms Desc",                                 ~
               at (11,22), fac(hex(81)), terms$     , ch(20),            ~
               at (21,02), fac(hex(a4)), line1$     , ch(79),            ~
               at (22,02), fac(hex(84)), line6$     , ch(79),            ~
               at (23,02), fac(hex(84)), line3$     , ch(79),            ~
               keys(pfkeys$), key(keyhit%)

               errormsg$=" "

               if keyhit% <>0% then L02190
                  gosub update_header
                  goto show_screen2

L02190:        if keyhit% <> 5% then L02290
                  nme_key$=hex(00)
                  str(nme_key$,1,8)=inv$
                  gosub load_name
                  if errormsg$=" " then show_screen4
                     str(li_key$,1,8)=inv$
                     str(li_key$,9,3)="  0"
                     gosub load_line_item
                  goto show_screen3

L02290:        if keyhit% = 1 then goto show_screen1

               if keyhit% = 15 then call "PRNTSCRN"

               if keyhit% = 16 then exit_program
           goto show_screen2

        show_screen3
        accept                                                           ~
               at (01,65), "TODAY",                                      ~
               at (01,71), fac(hex(84)), today$     , ch(10),            ~
               at (02,02), fac(hex(a4)), hdr_lin3$  , ch(79),            ~
               at (03,02), fac(hex(94)), errormsg$  , ch(79),            ~
               at (04,03), "Status Code",                                ~
               at (04,15), fac(hex(81)), stat_code$ , ch(01),            ~
               at (04,26), "Record Type",                                ~
               at (04,38), fac(hex(81)), rec_type$  , ch(01),            ~
               at (04,51), "Proc Dte",                                   ~
               at (04,60), fac(hex(81)), proc_dte$  , ch(04),            ~
               at (04,65), "EDI ID",                                     ~
               at (04,72), fac(hex(81)), edi_id$    , ch(03),            ~
               at (05,03), "EDI Invoice",                                ~
               at (05,15), fac(hex(84)), edi_inv$   , ch(08),            ~
               at (05,26), "EDI Sequnce",                                ~
               at (05,38), fac(hex(84)), edi_seq$   , ch(03),            ~
               at (05,51), "Rec Code",                                   ~
               at (05,60), fac(hex(84)), rec_code$  , ch(02),            ~
               at (05,65), "Line #",                                     ~
               at (05,72), fac(hex(84)), line_nbr$  , ch(06),            ~
               at (06,03), "Unit Price",                                 ~
               at (06,15), fac(hex(81)), unit_pri$  , ch(08),            ~
               at (06,26), "Quantity",                                   ~
               at (06,38), fac(hex(81)), quantity$  , ch(10),            ~
               at (06,51), "UOM",                                        ~
               at (06,60), fac(hex(81)), uom$       , ch(02),            ~
               at (06,65), "Cust",                                       ~
               at (06,72), fac(hex(81)), cust$      , ch(09),            ~
               at (07,03), tran Dte",                                   ~
               at (07,15), fac(hex(81)), wrk_trndte$, ch(10),            ~
               at (07,26), "Contl Nbr",                                  ~
               at (07,38), fac(hex(81)), contl_nbr$ , ch(09),            ~
               at (09,05), "Buyer Part",                                 ~
               at (09,16), fac(hex(81)), buy_prt$   , ch(25),            ~
               at (09,43), "Vendor Part",                                ~
               at (09,55), fac(hex(81)), ven_prt$   , ch(25),            ~
               at (10,21), "Item Desc",                                  ~
               at (10,32), fac(hex(81)), item_desc$ , ch(32),            ~
               at (21,02), fac(hex(a4)), line1$     , ch(79),            ~
               at (22,02), fac(hex(84)), line2$     , ch(79),            ~
               at (23,02), fac(hex(84)), line3$     , ch(79),            ~
               keys(pfkeys$), key(keyhit%)

               errormsg$ = " "

               if keyhit% <>0% then L02870
                  gosub update_line_item
                  goto show_screen3

L02870:        if keyhit% <> 5% then L02950
                  gosub load_line_item
                  if errormsg$=" " then show_screen3
                     hdr_key$=hex(00)
                     str(hdr_key$,1,8)=inv$
                     gosub load_header
                  goto show_screen2

L02950:        if keyhit% = 1 then goto show_screen1

               if keyhit% = 15 then call "PRNTSCRN"

               if keyhit% = 16 then exit_program
           goto show_screen3

        show_screen4
        accept                                                           ~
               at (01,65), "TODAY",                                      ~
               at (01,71), fac(hex(84)), today$     , ch(10),            ~
               at (02,02), fac(hex(a4)), hdr_lin4$  , ch(79),            ~
               at (03,02), fac(hex(94)), errormsg$  , ch(79),            ~
               at (04,03), "Status Code",                                ~
               at (04,15), fac(hex(81)), stat_code$ , ch(01),            ~
               at (04,24), "Record Type",                                ~
               at (04,36), fac(hex(81)), rec_type$  , ch(01),            ~
               at (04,47), "Proc Dte",                                   ~
               at (04,56), fac(hex(81)), proc_dte$  , ch(04),            ~
               at (04,65), "EDI ID",                                     ~
               at (04,72), fac(hex(81)), edi_id$    , ch(03),            ~
               at (05,03), "EDI Invoice",                                ~
               at (05,15), fac(hex(84)), edi_inv$   , ch(08),            ~
               at (05,24), "EDI Sequnce",                                ~
               at (05,36), fac(hex(84)), edi_seq$   , ch(03),            ~
               at (05,47), "Rec Code",                                   ~
               at (05,60), fac(hex(84)), rec_code$  , ch(02),            ~
               at (21,02), fac(hex(a4)), line1$     , ch(79),            ~
               at (22,02), fac(hex(84)), line2$     , ch(79),            ~
               at (23,02), fac(hex(84)), line3$     , ch(79),            ~
               keys(pfkeys$), key(keyhit%)

               errormsg$=" "

               if keyhit% <>0% then L03330
                  gosub update_name
                  goto show_screen4

L03330:        if keyhit% <> 5% then L03430
                  str(li_key$,1,8)=inv$
                  str(li_key$,9,5)="  01N"
                  gosub load_line_item
                  if errormsg$=" " then show_screen3
                     hdr_key$=hex(00)
                     str(hdr_key$,1,8)=inv$
                     gosub load_header
                  goto show_screen2

L03430:        if keyhit% = 1 then goto show_screen1

               if keyhit% = 15 then call "PRNTSCRN"

               if keyhit% = 16 then exit_program
           goto show_screen4

        load_header
            read #1, key 1 > hdr_key$, using L03530, hdr_key$, sav_cust$,  ~
                 eod goto L03710
L03530:       FMT POS(10), CH(13), POS(248), CH(09)
              if str(hdr_key$,1,8)<>inv$ then L03710
              if str(hdr_key$,13,1)<>"H" then L03730
              if sav_cust$<>cust$ then load_header
              get #1, using L03630 , stat_code$, rec_type$, proc_dte$,     ~
                  edi_id$, edi_inv$, edi_seq$, rec_code$, inv$,          ~
                  inv_dte$, inv_amt$, ship_dte$, ship_via$, po_num$,     ~
                  po_dte$, nd_dte$, dd_dte$, terms$, contact$, phone$,   ~
                  freight$, sales_tax$, pc$, disc_per$, tran_dte$,       ~
                  contl_nbr$, cust$
L03630:       FMT CH(01), CH(01), CH(04), CH(03), CH(08), CH(03),        ~
                  CH(02), CH(08), CH(06), CH(10), CH(06), CH(20),        ~
                  CH(16), CH(06), CH(06), CH(06), CH(20), CH(35),        ~
                  CH(21), CH(08), CH(08), CH(01), CH(08), POS(233),      ~
                  CH(06), CH(09), CH(09)
              old_hdr_key$ = stat_code$ & rec_type$ & proc_dte$ &        ~
                             edi_id$ & edi_inv$ & edi_seq$ & rec_code$
        return
L03710:    errormsg$ = "Invoice Not On For This Customer"
        return
L03730:    errormsg$ = "Header Record Not On File For This Invoice"
        return

        update_header
            gosub pack_dates
            call "SHOSTAT" ("Updating Header..")
            read #1, hold, key = old_hdr_key$, eod goto L03930
              delete #1
              put #1, using L03860 , stat_code$, rec_type$, proc_dte$,     ~
                  edi_id$, edi_inv$, edi_seq$, rec_code$, inv$,          ~
                  inv_dte$, inv_amt$, ship_dte$, ship_via$, po_num$,     ~
                  po_dte$, nd_dte$, dd_dte$, terms$, contact$, phone$,   ~
                  freight$, sales_tax$, pc$, disc_per$, tran_dte$,       ~
                  contl_nbr$, cust$
L03860:       FMT CH(01), CH(01), CH(04), CH(03), CH(08), CH(03),        ~
                  CH(02), CH(08), CH(06), CH(10), CH(06), CH(20),        ~
                  CH(16), CH(06), CH(06), CH(06), CH(20), CH(35),        ~
                  CH(21), CH(08), CH(08), CH(01), CH(08), POS(233),      ~
                  CH(06), CH(09), CH(09)
              write #1, eod goto L03930
        return
L03930:     errormsg$ = "Header Record Update Unsuccessful!!"
        return

        load_name
            read #1, key 1 > nme_key$, using L03990 , nme_key$ ,sav_cust$, ~
                 eod goto L04160
L03990:       FMT POS(10), CH(13), POS(248), CH(09)
              goto L04030
        read_nxt
            read #1, using L03990, nme_key$,  sav_cust$,  eod goto L04160
L04030:       if str(nme_key$,1,8)<>inv$ then L04160
              if sav_cust$<>cust$ then L04160
              if str(nme_key$,13,1)<>"N" then read_nxt
              get #1, using L04100 , stat_code$, rec_type$, proc_dte$,     ~
                  edi_id$, edi_inv$, edi_seq$, rec_code$, type_name$,    ~
                  ori_id$, add_nme$, add_city$, add_state$, add_zip$,    ~
                  add_lin1$, add_lin2$, tran_dte$, contl_nbr$, cust$
L04100:       FMT CH(01), CH(01), CH(04), CH(03), CH(08), CH(03),        ~
                  CH(02), CH(02), CH(17), CH(30), CH(18), CH(02),        ~
                  CH(05), CH(30), CH(30), POS(233), CH(06), CH(09), CH(09)
              old_nme_key$ = stat_code$ & rec_type$ & proc_dte$ &        ~
                             edi_id$ & edi_inv$ & edi_seq$ & rec_code$
        return
L04160:    errormsg$ = "No Name Address For This Customer"
        return

        update_name
            gosub pack_dates      
            call "SHOSTAT" ("Updating Name..")
            read #1, hold, key = old_nme_key$, eod goto L04320
              delete #1
              put #1, using L04270 , stat_code$, rec_type$, proc_dte$,     ~
                  edi_id$, edi_inv$, edi_seq$, rec_code$, type_name$,    ~
                  ori_id$, add_nme$, add_city$, add_state$, add_zip$,    ~
                  add_lin1$, add_lin2$, tran_dte$, contl_nbr$, cust$
L04270:       FMT CH(01), CH(01), CH(04), CH(03), CH(08), CH(03),        ~
                  CH(02), CH(02), CH(17), CH(30), CH(18), CH(02),        ~
                  CH(05), CH(30), CH(30), POS(233), CH(06), CH(09), CH(09)
              write #1, eod goto L04320
        return
L04320:     errormsg$ = "Name Record Update Unsuccessful!!"
        return

        load_line_item
            read #1, key 1 > li_key$, using L04380 , li_key$ ,sav_cust$,   ~
              eod goto L04520
L04380:       FMT POS(10), CH(13), POS(248), CH(09)
              if str(li_key$,1,8)<>inv$ then L04520
              if str(li_key$,13,1)<>"I" then load_line_item
              if sav_cust$<>cust$ then load_line_item
              get #1 using L04460 ,  stat_code$, rec_type$, proc_dte$,     ~
                  edi_id$, edi_inv$, edi_seq$, rec_code$, line_nbr$,     ~
                  buy_prt$, ven_prt$, quantity$, uom$, unit_pri$,        ~
                  item_desc$, tran_dte$, contl_nbr$, cust$
L04460:       FMT CH(01), CH(01), CH(04), CH(03), CH(08), CH(03),        ~
                  CH(02), CH(06), CH(25), CH(25), CH(10), CH(02),        ~
                  CH(08), CH(32), POS(233), CH(06), CH(09), CH(09)
              old_li_key$ = stat_code$ & rec_type$ & proc_dte$ &         ~
                             edi_id$ & edi_inv$ & edi_seq$ & rec_code$
        return
L04520:    errormsg$ = "No More Line Items For This Invoice"
        return

        update_line_item
            gosub pack_dates
            call "SHOSTAT" ("Updating Line Item..")
            read #1, hold, key = old_li_key$, eod goto L04680
              delete #1
              put #1 using L04630 ,  stat_code$, rec_type$, proc_dte$,     ~
                  edi_id$, edi_inv$, edi_seq$, rec_code$, line_nbr$,     ~
                  buy_prt$, ven_prt$, quantity$, uom$, unit_pri$,        ~
                  item_desc$, tran_dte$, contl_nbr$, cust$
L04630:       FMT CH(01), CH(01), CH(04), CH(03), CH(08), CH(03),        ~
                  CH(02), CH(06), CH(25), CH(25), CH(10), CH(02),        ~
                  CH(08), CH(32), POS(233), CH(06), CH(09), CH(09)
              write #1, eod goto L04680
        return
L04680:     errormsg$ = "Line Item Record Update Unsuccessful!!"
        return

fix_dates
/* <<<<<<<<<< Y2K >>>>>>>>>> */

        /* this sub will convert all the packed dates into a displayable */
        /* and editable value                                            */

        wrk_invdte$ = inv_dte$                          /* Invoice Date */
        call "DATEFMT" (wrk_invdte$)

        wrk_shpdte$ = ship_dte$                         /* Ship Date    */
        call "DATEFMT" (wrk_shpdte$)

        wrk_podte$ = po_dte$                            /* PO Date      */
        call "DATEFMT" (wrk_podte$)

        wrk_nddte$ = nd_dte$                            /* Net Due Date */
        call "DATEFMT" (wrk_nddte$)

        wrk_dddte$ = dd_dte$                            /* Disc. Due    */
        call "DATEFMT" (wrk_dddte$)


        wrk_trndte$ = tran_dte$                         /* Tran Date    */
        call "DATEFMT" (wrk_trndte$)
        return
/* <<<<<<<<<< Y2K >>>>>>>>>> */

pack_dates
/* <<<<<<<<<< Y2K >>>>>>>>>> */

        /* This sub will put the viewable dates back into their paced   */
        /* form in the vars that get written to the disk                */

        call "DATUNFMT" (wrk_invdte$)
        str(inv_dte$,1%,6%) = str(wrk_invdte$,1%,6%)    /* Invoice Date */

        call "DATUNFMT" (wrk_shpdte$)
        str(ship_dte$,1%,6%) = str(wrk_shpdte$,1%,6%)   /* Shp Date     */

        call "DATUNFMT" (wrk_podte$) 
        str(po_dte$,1%,6%) = str(wrk_podte$,1%,6%)      /* PO  Date     */

        call "DATUNFMT" (wrk_nddte$) 
        str(nd_dte$,1%,6%) = str(wrk_nddte$,1%,6%)      /* Net Due Date */

        call "DATUNFMT" (wrk_dddte$) 
        str(dd_dte$,1%,6%) = str(wrk_dddte$,1%,6%)      /* Discount Due */

        call "DATUNFMT" (wrk_trndte$) 
        str(tran_dte$,1%,6%) = str(wrk_trndte$,1%,6%)   /* Tran Date    */
        return
/* <<<<<<<<<< Y2K >>>>>>>>>> */


        exit_program
           close #1
           end
