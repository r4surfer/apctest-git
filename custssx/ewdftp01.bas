        REM *************************************************************~
            *                                                           *~
            * EEEEEE W    W DDDD  FFFFFF TTTTT PPPPP    0000     1      *~
            * E      W    W D   D F        T   P    P  0    0   11      *~
            * EEEE   W WW W D   D FFFFF    T   PPPPPP  0    0  1 1      *~
            * E      WW  WW D   D F        T   P       0    0    1      *~
            * EEEEEE W    W DDDD  F        T   P        0000   1111     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDFTP01 - Subroutine To Create Invoice Edi Transmit File *~
            *    Special Note - Subroutine is used by the Programs      *~
            *                   (ARIUPDTE)                              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/31/00 ! Original                                 ! CMG *~
            * 08/10/01 ! (EWD001) - Mod to add new code 'H' for   ! CMG *~
            *          !     Warranty Line Items.                 !     *~
            * 09/13/01 ! (EWD002) - Mods to add new codes for     ! CMG *~
            *          !     'J' = Screen Ship Sep.               !     *~
            *          !     'K' = Factory Prep                   !     *~
            *          !     'L' = Mull, Twin, Triple             !     *~
            * 02/10/03 ! (EWD003) - Mods for WW Line Number       ! CMG *~
            * 04/07/05 ! (AWD004) - mods for NE schema            ! CMG *~
            *09/26/2013! (AWD005) - modes for norandex part number! CMG *~
            *07/17/2014! (AWD006) - mod for private label         ! CMG *~
            *-----------------------------------------------------------*

        sub "EWDFTP01" (#1,                /* ARIBUFFR File - Headers */ ~
                        #2,                /* ARIBUFF2 File - Details */ ~
                        #3,                /* CUSTOMER Master File    */ ~
                        #4,                /* GENCODES File           */ ~
                        #7,                /* BCKLINES File           */ ~
                        #6,                /* EWDFTPMR File           */ ~
                        #9,                /* HNYMASTR File           */ ~
                        #10,               /* AMTBOMIF File           */ ~
                        #11,               /* TXTFILE File            */ ~
                        edi_cust$,         /* Customer Number         */ ~
                        edi_inv$,          /* Invoice Number          */ ~
                        edi_seq$,          /* Invoice Sequence Number */ ~
                        schema%      )



        dim aribuf_key$17,               /* ARIBUFFR File Key          */~
            aribuf2_key$20,              /* ARIBUF2 File Key           */~
            bcklne_key$19,               /* BCKLINES File Key          */~
            txt_key$11,                  /* TXTFILE File key           */~
            textid$4,                    /* Text ID                    */~
            text$70,                     /* Text Line                  */~
            sku_key$24,                  /* Sku Key                    */~
            edi_part$10,                 /* Norandex/Reynolds Part Num */~
            cde_key$24,                  /* GENCODES File Key          */~
            cde_desc$30,                 /* GENCODES Description (AWD005)*/~
            del_key$22,                  /* EWDFTPMR Delete key        */~
            edi_cust$9,                  /* Customer Number            */~
            edi_inv$8,                   /* Invoice Number             */~
            edi_seq$3,                   /* Invoice Line Item          */~
            edi_ww_seq$3,                /* WW Line Item         EWD003*/~
            edi_type$2,                  /* What Part Of EDI is Used   */~
            edi_so$8,                    /* EDI Sales Order Number     */~
            edi_width$20,                /* EDI Part Number Width      */~
            edi_height$20,               /* EDI Part Number Height     */~
            edi_sze$20,                  /* Part Number Sizing Info    */~
            hld_key$22,                  /* EWDFTPMR Temp Delete Key   */~
            edi_desc$39,                 /* EDI Description            */~
            inv_date$6,                  /* Invoice Date               */~
            edi_po$16,                   /* Purchase Order Number      */~
            edi_qty$7,                   /* Quantity Ordered           */~
            valid_flag$5,                /* Valid Flag - 'EDIOK'       */~
            blankdate$6,                 /* Empty Date for compares    */~
            xx$10, errormsg$79,          /*                            */~
            ven_in$25,                   /* Vendor Item Number         */~
            edi_hows$2,                  /* How Ship Code              */~
            edi_code$1,                  /* Send Code for Nor/Rey      */~
            edi_cd_desc$30,              /* Send Code Description      */~
/*AWD004*/  vendor$6,                    /* NE or NC vendor code       */~
/*AWD006*/  privateLbl$02                /* Private Label              */



        REM *************************************************************~
            *       M A I N  P R O C E S S  S E C T I O N                ~
            *************************************************************~

            gosub init_data                /* Initialize Data Variable */

            gosub validate_edi_id          /* Validate EDI Customer    */

            if valid_flag$ <> "EDIOK" then goto exit_program

            gosub update_edirn             /* Update EDI Invoice File  */

            goto exit_program                          /* Exit Program */


        init_data

            init (" ") aribuf_key$, aribuf2_key$, cde_key$, del_key$,     ~
                       edi_type$, inv_date$, edi_desc$, edi_qty$,         ~
                       edi_po$, sku_key$, valid_flag$, ven_in$, hld_key$, ~
                       edi_id$, date$, edi_so$, edi_width$, edi_height$,  ~
                       edi_sze$, edi_hows$, edi_code$, edi_cd_desc$,      ~
                       privateLbl$


            init (" ") vendor$         /* (AWD004) */
            vendor$ = "064300"
            if schema% = 2% then vendor$ = "021657"

           qty_ord, edi_cpu = 0

           date$  = date                           /* Set Today's Date */

           call "DATUFMTC" (blankdate$)

           inv_date$ = blankdate$



        return

        validate_edi_id                /* Get EDI Id and Customer Data */
                                       /* from the Customer Master File*/
           read #3,key = edi_cust$,using  L01390, edi_id$, eod goto L01610

L01390:         FMT POS(1000), CH(03)

                if len(edi_id$) < 2 then return

                        /* Determine If Customer Is A Valid EDI Partner*/
           cde_key$ = " "
           str(cde_key$,1%,9%)   = "PARTNERS1"
           str(cde_key$,10%,15%) = edi_id$
           read #4,key = cde_key$, eod goto L01620
                      /* Determine What Part Of EDI This Customer Uses */
                      /* 00 - NA, 01 - Invoices Only, 02 - PO's Only   */
                      /* 03 - Both Invoices and PO's                   */
                      /* Pos (25) 'K' = Kleinsmidth, 'P' = PC anywhere */
           str(cde_key$,1%,9%)   = "EDI TRADE"
           str(cde_key$,10%,15%) = edi_id$
           read #4,key = cde_key$,using  L01570,edi_type$, eod goto L01640
L01570:       FMT POS(25), CH(02)
           if edi_type$ <> "01" and edi_type$ <> "03" then return
              valid_flag$="EDIOK"

L01610: return
L01620:    call "SHOSTAT" ("(Error)-Validity - 'PARTNERS1' ") : stop
        return
L01640:    call "SHOSTAT" ("(Error)-Validity - 'EDI TRADE' ") : stop
        return

        update_edirn
            gosub get_aribuffr
            gosub get_aribuf2
            gosub get_width_height

                 str(del_key$,1%,1%)  = "S"    /* Set EWDFTPMR Del Key */
                 str(del_key$,2%,1%)  = "I"
                 str(del_key$,3%,9%)  = edi_cust$
                 str(del_key$,12%,8%) = edi_inv$
                 str(del_key$,20%,3%) = edi_seq$
            read #6,hold,key = del_key$, eod goto delete_line_done

                 delete #6

        delete_line_done
              put #6,using  L03210, inv_date$, /* EDI Invoice Date      */~
                                    "S",       /* STATUS CODE (S/T)     */~
                                    "I",       /* Record Type (I/C/A)   */~
                                  edi_cust$,   /* EDI Customer          */~
                                  edi_inv$,    /* Invoice Number        */~
                                  edi_seq$,    /* Invoice Line Item     */~
                                  edi_part$,   /* Nor/Rey Part Number   */~
                                  edi_qty$,    /* Line Item Qty         */~
                                  "EA",        /* EDI Unit Of Measure   */~
                                  edi_cpu,     /* EDI Cost Per Unit     */~
                                  edi_desc$,   /* Part Desc-Ellison Desc*/~
                                  vendor$,     /* Vendor Code (AWD004)  */~
                                  edi_po$,     /* Purchase Order Number */~
                                  ven_in$,     /* Ellison Part Number   */~
                                  edi_width$,  /* EDI Part Width        */~
                                  edi_height$, /* EDI Part Height       */~
                                  edi_so$,     /* EDI Sales Order       */~
                                  edi_code$,   /* EDI Code              */~
                                  edi_cd_desc$,/* EDI Code Description  */~
                                  edi_ww_seq$  /* (EWD003) WW Line Num  */

L03210:       FMT CH(6), CH(1), CH(1), CH(9), CH(8), CH(3), CH(10), CH(7),  ~
                  CH(2), PD(14,5), CH(39), CH(6), CH(16), CH(25), CH(20),   ~
                  CH(20), CH(08), CH(01), CH(30), CH(3)

              write #6, eod goto L03270  /*Write EWDFTPMR   */
L03260: return
L03270:   call "SHOSTAT" ("(Error) - Dup Record for ("&edi_inv$&")")
          stop
        return

        get_aribuffr
                            /* Add Invoice Header Items To EDI RN File */
              str(aribuf_key$,1%,9%)  = edi_cust$
              str(aribuf_key$,10%,8%) = edi_inv$
              read #1,key = aribuf_key$,using  L01750, edi_po$, edi_so$,   ~
                                      edi_hows$,inv_date$, eod goto L03260
L01750:            FMT POS(18), CH(16), CH(16), POS(419), CH(02), POS(521), CH(6)

              init(" ") xx$          :  date% = 0%
              xx$ = inv_date$
              call "DATEOKC" (xx$, date%, errormsg$)
              call "DATUFMTC" (xx$)

              init(" ") inv_date$
              inv_date$ = str(xx$,1%,6%)

              if str(edi_po$,1%,1%)=" " then call "SPCSMASH" (edi_po$)
              gosub check_hows
        return

        get_aribuf2
                             /* Notes - 'VEN_IN$' - EWD Part Number    */
                             /*         'BUYIN$'  - Customer's Part No.*/
              str(aribuf2_key$,1%,9%)  = edi_cust$
              str(aribuf2_key$,10%,8%) = edi_inv$
              str(aribuf2_key$,18%,3%) = edi_seq$

              read #2,key = aribuf2_key$,using  L03390,ven_in$, edi_desc$,~
                                       qty_ord, edi_cpu, linedisc,        ~
                                                          eod goto L03970

L03390:       FMT POS(24), CH(25), CH(32), POS(93), PD(14,4), POS(133),  ~
                  PD(14,4), PD(14,4)

              convert qty_ord to edi_qty$, pic(-######)

L03970:
              gosub extreme   /* (AWD005) */
              str(edi_part$,1%,10%) = "9043100000"
              str(sku_key$,1%,9%)   = "PLAN NEWC"
              str(sku_key$,10%,15%)   = str(ven_in$,1%,3%)

              read #4,key = sku_key$,eod goto not_new_c

                   str(edi_part$,1%,10%) = "9044100000"
        not_new_c

                                          /* Unit Price after */
                                          /*     Line Item Discount      */
              edi_cpu = round(edi_cpu * (1 - (linedisc/100.0)), 2)
                                          /*  Line Item Disc   */

              edi_cpu = round( edi_cpu, 4)        /* Round Unit Price */

              if str(ven_in$,1%,4%) <> "0052" and                   ~
                 str(ven_in$,1%,4%) <> "0062" then goto L03980
                 edi_code$ = "F"
                 edi_cd_desc$ = "Freight Charges       "
                 if str(ven_in$,1%,4%) = "0062" then edi_code$ = "G"
                 if str(ven_in$,1%,4%) = "0062" then              ~
                             edi_cd_desc$ = "Drop Shipment         "

                                                    /*  (EWD001)  */
L03980:      if str(ven_in$,5%,4%) <> "WARR" then  goto L03990
                 edi_code$ = "H"
                 edi_cd_desc$ = "Warranty Line         "

                                                  /*  (EWD002) - Begin */
L03990:      if str(ven_in$,6%,4%) <> "SCRN" then  goto L03400
                 edi_code$ = "J"
                 edi_cd_desc$ = "Screen Ship Sep       "
                 str(edi_part$,1%,10%) = "9043100000"
L03400:

/* (AWD005) */
/* (AWD006) */
            if privateLbl$ <> "14" then goto not_Norandex_Brand
            if extreme% = 1% then str(edi_part$,1%,10%) = "9212100000"
            if extreme% = 2% then str(edi_part$,1%,10%) = "9210100000"
            if extreme% = 3% then str(edi_part$,1%,10%) = "9214100000"
            if extreme% = 4% then str(edi_part$,1%,10%) = "9216100000"
not_Norandex_Brand:
/*(\AWD006)*/            
/* (\AWD005) */
            ss% = 0%
            if len(ven_in$) < 20 then return        /* Quick Test      */
            if str(ven_in$,1%,1%) = "9" then return   /* Bay/Bow       */

            convert str(ven_in$,20%,3%) to ss%, data goto LS1
            goto LS2

LS1:        if str(ven_in$,20%,1%) <> "A" then goto LS4
               if str(ven_in$,20%,3%) < "A02" or         ~
                    str(ven_in$,20%,3%) > "A15" then goto LS4
                edi_code$ = "K"
                edi_cd_desc$ = "Factory Prep          "
        return
LS2:        if len(ven_in$) < 23 then return        /* Quick Test      */
            convert str(ven_in$,23%,3%) to ss%, data goto LS3
            return

LS3:        if str(ven_in$,20%,1%) <> "A" then goto LS4
               if str(ven_in$,20%,3%) < "A02" or         ~
                    str(ven_in$,20%,3%) > "A15" then goto LS4
                edi_code$ = "K"
                edi_cd_desc$ = "Factory Prep          "
        return
LS4:          edi_code$ = "L"
              edi_cd_desc$ = "Mull, Twin, Triple    "
        return
                                                  /*  (EWD002) - End   */

        get_width_height
            p%, err% = 0%
            init(" ") edi_width$, edi_height$
            if len(ven_in$) < 19% then return

            call "APCDESCR" (ven_in$, " ", " ", edi_sze$, #10, err% )

            if err% <> 0% then return

            p% = pos(edi_sze$ = "X")
            edi_width$  = str(edi_sze$, 1%, (p% - 1%))
            edi_height$ = str(edi_sze$, (p% + 1%), 20%)

        return

        check_hows
            init(" ") edi_code$, edi_cd_desc$
            p% = 0%
            p% = pos(edi_po$ = "-")

            if str(edi_po$,p%+7,1%) = "R" then edi_code$ = "A"
            if str(edi_po$,p%+7,1%) = "." then edi_code$ = "B"
            if str(edi_po$,p%+7,1%) = "F" then edi_code$ = "C"

            if edi_hows$ = "24"                     then edi_code$ = "A"
            if edi_hows$ = "20" or edi_hows$ = "21" then edi_code$ = "B"
            if edi_hows$ = "23"                     then edi_code$ = "C"
            if edi_hows$ = "04" or edi_hows$ = "05" then edi_code$ = "D"
            if edi_hows$ = "06" or edi_hows$ = "22" then edi_code$ = "D"
            if edi_hows$ = "34"                     then edi_code$ = "D"
            if edi_hows$ = "32"                     then edi_code$ = "E"

            if edi_code$ = "A" then edi_cd_desc$ = "Replacement Parts     "
            if edi_code$ = "B" then edi_cd_desc$ = "Back Ordered          "
            if edi_code$ = "C" then edi_cd_desc$ = "Order Changed (Fix-it)"
            if edi_code$ = "D" then edi_cd_desc$ = "Sample/Display        "
            if edi_code$ = "E" then edi_cd_desc$ = "Literature            "

REM            if str(edi_code$,1%,1%) = "C" then gosub get_line_item
            gosub get_line_item                      /*  (EWD003)        */

        return

        get_line_item
           bcklne_key$, txt_key$, textid$, text$ = all(hex(00))
           init(" ") edi_ww_seq$                     /*  (EWD003)        */
           edi_ww_seq% = 0%
           str(bcklne_key$,1%,16%) = edi_so$
           str(bcklne_key$,17%,3%) = edi_seq$

/* (AWD006) ~ add privateLbl$ */           
           read #7, key = bcklne_key$, using L04500, textid$, privateLbl$, ~
                          edi_ww_seq$, eod goto no_line

L04500:     FMT POS(242), CH(04), POS(282), CH(02), CH(03)

                                                     /*  (EWD003)  BEG  */
           convert edi_ww_seq$ to edi_ww_seq%, data goto L04550

           convert edi_ww_seq% to edi_ww_seq$, pic(##0)

L04550:
           if edi_ww_seq% = 0% then                                      ~
                    str(edi_ww_seq$,1%,3%) = str(edi_seq$,1%,3%)

           if str(edi_code$,1%,1%) <> "C" then return

                                                     /*  (EWD003)  END  */
           str(txt_key$,1%,1%)  = "M"
           str(txt_key$,2%,3%)  = " "
           str(txt_key$,5%,4%)  = textid$

        read_txt_next
           read #11, key > txt_key$, using L04600, txt_key$, text$, ~
                                                   eod goto no_line

L04600:     FMT CH(11), POS(65), CH(70)

           if str(txt_key$,5%,4%) > str(textid$,1%,4%) then goto no_line
           if str(txt_key$,5%,4%) <> str(textid$,1%,4%) then goto read_txt_next
           pp%, text% = 0%
           pp% = pos(text$ = "-")
           if pp% = 0% then goto no_line
           convert str(text$,(p%+1%),2%) to text%, data goto no_line

           convert text% to str(edi_cd_desc$,23%,2%), pic(00)
        no_line
        return
/* (AWD005) */
        extreme
         extreme% = 0%
           init(" ") cde_key$, cde_desc$
           str(cde_key$,1%,9%)   = "EXTREME  "
           str(cde_key$,10%,15%) = str(ven_in$,1%,3%)
           read #4,key = cde_key$,using  L01580, cde_desc$, eod goto notExtreme

              if str(cde_desc$,1%,3%) = "X  " then extreme% = 1%
              if str(cde_desc$,1%,3%) = "XX " then extreme% = 2%
              if str(cde_desc$,1%,3%) = "200" then extreme% = 3%
              if str(cde_desc$,1%,3%) = "500" then extreme% = 4%
        notExtreme
        return
L01580:       FMT POS(25), CH(30)
/* (\AWD005) */

        exit_program
         end








