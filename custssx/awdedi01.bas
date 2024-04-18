        REM *************************************************************~
            *                                                           *~
            * AAAAAA  W   W DDDD   EEEE  DDDD  IIIIII   0000    111     *~
            * AA  AA  W   W DD  D  EE    DD  D   II    0    0  1111     *~
            * AAAAAA  W W W DD  D  EEEE  DD  D   II    0    0    11     *~
            * AA  AA  WW WW DD  D  EE    DD  D   II    0    0    11     *~
            * AA  AA  W   W DDDD   EEEE  DDDD  IIIIII   0000   1111     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * AWDEDI01 - Subroutine To Create Invoice Edi Transmit File *~
            *            to send to PLYMART                             *~
            *    Special Note - Subroutine is used by the Programs      *~
            *                   (ARIUPDTE) and (        )               *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/01/04 ! Original                                 ! CMG *~
            *-----------------------------------------------------------*

        sub "AWDEDI01" (#1,                /* ARIBUFFR File - Headers */ ~
                        #2,                /* ARIBUFF2 File - Details */ ~
                        #3,                /* CUSTOMER Master File    */ ~
                        #4,                /* GENCODES File           */ ~
                        #5,                /* BCKMASTR File           */ ~
                        #6,                /* AWDEDIPL File           */ ~
                        #7,                /* APCSKUNO File           */ ~
                        #8,                /* ARMTERMS File           */ ~
                        #9,                /* HNYMASTR File           */ ~ 
                        edi_cust$,         /* Customer Number         */ ~
                        edi_inv$,          /* Invoice Number          */ ~
                        dsc_due_dt$,       /* Discount Due Date       */ ~
                        net_due_dt$,       /* Net Due Date            */ ~
                        edi_seq$,          /* Invoice Sequence Number */ ~
                        lh_flag$,          /* Line Item / Header Flag */ ~
                        energy )           /* ESC Surcharge           */


        dim aribuf_key$17,               /* ARIBUFFR File Key          */~
            aribuf2_key$20,              /* ARIBUF2 File Key           */~
            baddr1$30,                   /* Address Line 1  Bill-To    */~
            baddr2$30,                   /* Address Line 2  Bill-To    */~
            bcity$18,                    /* Address City    Bill-To    */~
            bck_key$25,                  /* BCKMASTR File Key          */~
            bname$30,                    /* Address Name    Bill-To    */~
            bstate$2,                    /* Address State   Bill-To    */~
            buy_in$25,                   /* Buyers Item Number         */~
            bzip$5,                      /* Zip Code        Bill-To    */~
            cde_key$24,                  /* GENCODES File Key          */~
            cont_nme$35,                 /* Contact Name               */~
            cont_phn$21,                 /* Contact Phone              */~
            del_key$22,                  /* APCEDIMC Delete key        */~
            desc$32,                     /* GENCODES Description        */~        
            dsc_due_dt$6,                /* Discount Due Date          */~
            dsc_per$5,                   /* Discount Percent           */~
            edi_cust$9,                  /* Customer Number            */~
            edi_cust1$9,                 /* Customer Number            */~
            edi_id$3,                    /* EDI Identification Number  */~
            edi_inv$8,                   /* Invoice Number             */~
            edi_ln$6,                    /* Invoice Line Item          */~
            edi_seq$3,                   /* Invoice Line Item          */~
            edi_type$2,                  /* What Part Of EDI is Used   */~
            frt$8,                       /* Freight                    */~
            frt_pc$1,                    /* Freight Paid/Collected     */~
            hld_key$22,                  /* APCEDIMC Temp Delete Key   */~
            howship$20,                  /* Control 'Our Truck'/S.O.   */~
            inv_date$6,                  /* Invoice Date               */~
            inv_tot$10,                  /* Invoice Total              */~
            item_desc$32,                /* Item Description           */~
            lh_flag$1,                   /* 'L'=LINE, 'H'=HEADER       */~
            net_due_dt$6,                /* Net Due Date               */~
            old_skuno$25,                /* Customer's Part No         */~
            ori_id$17,                   /* Originator ID              */~
            po_date$6,                   /* Purchase Order Date        */~
            po_num$16,                   /* Purchase Order Number      */~
            qty_ord$10,                  /* Quantity Ordered           */~
            readkey$50,                  /* Gencodes Key               */~
            saddr1$30,                   /* Address Line 1  Ship-To    */~
            saddr2$30,                   /* Address Line 2  Ship-To    */~
            sales_tax$8,                 /* Sales Tax                  */~
            scity$18,                    /* Address City    Ship-To    */~
            ship_date$6,                 /* Date Shipped               */~
            sku_key$28,                  /* SKU Parts File             */~
            sname$30,                    /* Address Name    Ship-To    */~
            so_no$16,                    /* Sales Order Number         */~
            str_id$4,                    /* Store Id                   */~
            sstate$2,                    /* Address State   Ship-To    */~
            szip$5,                      /* Zip Code        Ship-To    */~
            terms$20,                    /* Terms File Key             */~
            terms_desc$20,               /* Terms Description          */~
            tr_dte$6,                    /* Create Date                */~
            today$4,                     /* Today's Date (MM/DD)       */~
            unit_pri$8,                  /* Unit Price                 */~
            upc_code$12,                 /* UPC Code                   */~
            valid_flag$5,                /* Valid Flag - 'EDIOK'       */~
            xx$10, errormsg$79,          /*                            */~
            ven_in$25                    /* Vendor Item Number         */

        dim blankdate$6,                 /* Empty Date for compares    */~
            workdate$8,                  /* Used for working with date */~
            work_date$8,                 /*                            */~
            yy$4, mm$2, dd$2,            /*                            */~ 
            cnet_due_dt$8,               /* Net due date calc          */~
            cdsc_due_dt$8                /* Disc due date              */

        REM *************************************************************~
            *       M A I N  P R O C E S S  S E C T I O N                ~
            *************************************************************~

            gosub init_data                /* Initialize Data Variable */

            gosub validate_edi_id          /* Validate EDI Customer    */

            if valid_flag$ <> "EDIOK" then goto exit_program

            gosub update_edimc             /* Update EDI Invoice File  */

            goto exit_program                          /* Exit Program */


        init_data

            init (" ") aribuf_key$, aribuf2_key$, baddr1$, baddr2$,      ~
                       bcity$, bck_key$, bname$, bstate$, buy_in$, bzip$,~
                       cde_key$, cont_nme$, cont_phn$, del_key$,         ~
                       dsc_per$, edi_ln$, edi_type$, frt$,               ~
                       frt_pc$, inv_date$, inv_tot$, item_desc$,         ~
                       ori_id$, qty_ord$, po_date$, po_num$,             ~
                       saddr1$, saddr2$, sales_tax$, scity$, ship_date$, ~
                       sku_key$, sname$, so_no$, sstate$, szip$,         ~
                       terms_desc$, today$, unit_pri$, valid_flag$,      ~
                       ven_in$, terms$, hld_key$, str_id$, edi_id$, edi_cust1$
           frt_amt = 0
           inv_amt, qty_ord, unit_pri = 0

           date$  = date                       

           workdate$ = date$

           call "DATUFMTC" (blankdate$)

           inv_date$ = blankdate$                           

           call "EWDMDY" (workdate$, 1%, mm%,mm$,dd%,dd$,yy%,yy$)

           str(today$,1%,2%) = mm$                           /* SET MONTH  */
           str(today$,3%,2%) = dd$                           /* SET DAY    */


           tr_dte$ = date                                /* Create Date*/
           /* Y2K tr_dte$ - does not have to be altered since it is */
           /* written to the disk and is not displayed or checked   */

           ship_date$ = blankdate$                   
           po_date$   = blankdate$                   

        return

        validate_edi_id                /* Get EDI Id and Customer Data */
                                       /* from the Customer Master File*/
           edi_cust1$ = edi_cust$       


           read #3,key = edi_cust$,using  L01390, bname$, baddr1$,         ~
                baddr2$, bcity$, bstate$, bzip$, sname$, saddr1$,        ~
                saddr2$, scity$, sstate$, szip$, terms_desc$, edi_id$,   ~
                eod goto L01610
L01390:         FMT POS(150), CH(30), CH(30), CH(30), POS(190), CH(18),  ~
                    CH(02), POS(211), CH(05), POS(253), CH(30), CH(30),  ~
                    CH(30), POS(403), CH(18), CH(2), POS(424), CH(5),    ~
                    POS(543), CH(20), POS(1000), CH(03)
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

        update_edimc
           if lh_flag$ <> "H" then L03310
                            /* Add Invoice Header Items To EDI MC File */
              str(aribuf_key$,1%,9%)  = edi_cust$
              str(aribuf_key$,10%,8%) = edi_inv$
              read #1,key = aribuf_key$,using  L01750,str_id$, po_num$,    ~
                   so_no$, ship_date$, inv_date$, hq_amt, frt_amt,       ~
                   inv_amt, terms$, eod goto L03260
L01750:            FMT POS(3), CH(4), POS(18), CH(16), CH(16), POS(413), ~
                       CH(6), POS(521), CH(6), POS(793), PD(14,4),       ~
                       POS(817), PD(14,4), POS(833), PD(14,4),           ~
                       POS(908), CH(20)



              init(" ") xx$
              xx$ = ship_date$
              call "DATEOKC" (xx$, date%, errormsg$)
              if date% = 0% then init(" ") xx$
              if date% = 0% then xx$ = inv_date$
              call "DATUFMTC" (XX$)
              init(" ") ship_date$
              ship_date$ = str(xx$,1%,6%)      
 
              if str(po_num$,1%,1%)=" " then call "SPCSMASH" (po_num$)

                                 /* Don't Write Invoice If Amount Zero */
                                 /* Delete Previous Written Line Items */
              if inv_amt <> 0 then goto update_header
                 str(del_key$,1%,1%)  = "S"    /* Set APCEDIMC Del Key */
                 str(del_key$,2%,1%)  = "I"
                 str(del_key$,3%,4%)  = today$
                 str(del_key$,7%,3%)  = edi_id$
                 str(del_key$,10%,8%) = edi_inv$
                 str(del_key$,18%,3%) = "   "
                 str(del_key$,21%,2%) = "  "
        read_line_items
              read #6,hold,key > del_key$,using  L01940,hld_key$,          ~
                                                eod goto delete_line_done
L01940:          FMT CH(22)
              if str(del_key$,1%,17%) <> str(hld_key$,1%,17%) then       ~
                                                    goto delete_line_done
                 del_key$ = hld_key$
                 delete #6                   /* Delete Zero Line Items */
                 goto read_line_items
              return
        delete_line_done
        return
        update_header
                     /* Set Invoice Due Date & Disc Due Date For Lowes */
              gosub set_contract_due_dates

              str(bck_key$,1%,9%)   = edi_cust$ /* Purchase Order Date */
              str(bck_key$,10%,16%) = so_no$
              init(" ") howship$, po_date$
              po_date$ = blankdate$                              /* Y2K */
              read #5,key = bck_key$,using  L02120, howship$, po_date$,    ~
                                                           eod goto L02130
L02120:          FMT POS(422), CH(20), POS(830), CH(6)

              /* Y2K */
L02130:       if len(po_date$) < 6 or po_date$ = blankdate$ then  po_date$=date

              disc_per = 0                /* Get TERMS Discount % !!!!!*/
              read #8,key = terms$,using  L02170,disc_per, eod goto L02190
L02170:            FMT POS(51), PD(14,4)
*       RHH - Special Mod to EDI for Freight Charges - 03/20/95
L02190:       if frt_amt < .01 then goto L02250     /* No Freight Charge */
                 frt_amt = round(frt_amt, 2)
        REM      INV_AMT = ROUND(INV_AMT - FRT_AMT, 2)
                 frt_amt = 100 * frt_amt
                 convert frt_amt to frt$, pic (-#######)

L02250:       if energy < .01 then goto L02260
                 energy = round(energy, 2)
                 energy = 100 * energy
                 convert energy to frt$, pic(-#######)


L02260:       inv_amt  = round(inv_amt, 2)        /* Round All Amounts */
              disc_amt = round((disc_per * inv_amt), 2)


              inv_amt  = 100 * inv_amt /* Convert Numeric Data To Ascii */
        REM   DISC_AMT = 100 * DISC_AMT
              convert inv_amt to inv_tot$, pic (-#########)

              convert disc_amt to sales_tax$, pic (########)

              str(del_key$,1%,1%)  = "S" /*Set APCEDIMC File Delete Key*/
              str(del_key$,2%,1%)  = "I"
              str(del_key$,3%,4%)  = today$
              str(del_key$,7%,3%)  = edi_id$
              str(del_key$,10%,8%) = edi_inv$
              str(del_key$,18%,3%) = edi_seq$
              str(del_key$,21%,2%) = " H"
              read #6,hold,key = del_key$, eod goto L02500
                   delete #6                      /* Delete Dup Header */
L02500:       put #6,using  L02780, "S",        /* STATUS CODE (S/T/R)   */~
                                  "I",        /* Record Type (S/I/A)   */~
                                  today$,     /* Today's Date (MM/DD)  */~
                                  edi_id$,    /* EDI Identifer         */~
                                  edi_inv$,   /* Invoice Number        */~
                                  edi_seq$,   /* Invoice Line Item     */~
                                  " H",       /* Record Code (i.e. H/I)*/~
                                  edi_inv$,   /* Invoice Number        */~
                                  inv_date$,  /* Invoice Date          */~
                                  inv_tot$,   /* Invoice Total         */~
                                  ship_date$, /* Date Shipped          */~
                                  howship$,   /* Ship Via              */~
                                  po_num$,    /* Purchase Order Number */~
                                  po_date$,   /* Purchase Order Date   */~
                                  net_due_dt$,/* Net Due Date          */~
                                  dsc_due_dt$,/* Discount Due Date     */~
                                  terms_desc$,/* Terms Description     */~
                                  cont_nme$,  /* Contact Name          */~
                                  cont_phn$,  /* Contact Phone         */~
                                  frt$,       /* Freight               */~
                                  sales_tax$, /* Sales Tax             */~
                                  frt_pc$,    /* Freight Paid/Collected*/~
                                  dsc_per$,   /* Discount Percent      */~
                                  " ",        /* Filler Spaces         */~
                                  tr_dte$,    /* Create Date           */~
                                  " ",        /* Filler Spaces         */~
                                  edi_cust1$   /* Customer Number EWD006*/

L02780:       FMT CH(1), CH(1), CH(4), CH(3), CH(8), CH(3), CH(2), CH(8),~
                  CH(6), CH(10), CH(6), CH(20), CH(16), CH(6), CH(6),    ~
                  CH(6), CH(20), CH(35), CH(21), CH(8), CH(8), CH(1),    ~
                  CH(5), CH(28), CH(6), CH(9), CH(9)

              write #6, eod goto L02850    /* Write APCEDIMC File Header */
              goto L02880
L02850:          call "SHOSTAT" ("(Error) - DUP Hdr --> "& del_key$ )
                 stop

L02880:       str(ori_id$,1%,4%) = str_id$

              str(del_key$,1%,1%)  = "S"   /* APCEDIMC File Delete Key */
              str(del_key$,2%,1%)  = "I"
              str(del_key$,3%,4%)  = today$
              str(del_key$,7%,3%)  = edi_id$
              str(del_key$,10%,8%) = edi_inv$
              str(del_key$,18%,3%) = edi_seq$
              str(del_key$,21%,2%) = "1N"
              read #6,hold,key = del_key$, eod goto L03000
                   delete #6                    /*Delete Dup ST Address*/
                              /* Build Ship-To Name And Address Record */
L03000:       put #6,using  L03210, "S",       /* STATUS CODE (S/T/R)    */~
                                  "I",        /* Record Type (S/I/A)   */~
                                  today$,     /* Today's Date (MM/DD)  */~
                                  edi_id$,    /* EDI Identifer         */~
                                  edi_inv$,   /* Invoice Number        */~
                                  edi_seq$,   /* Invoice Line Item     */~
                                  "1N",       /* Record Code (i.e. H/I)*/~
                                  "ST",       /* Type Name Address Rec */~
                                  ori_id$,    /* Originator ID         */~
                                  sname$,     /* Address Name          */~
                                  scity$,     /* Address City          */~
                                  sstate$,    /* Address State         */~
                                  szip$,      /* Zip Code              */~
                                  saddr1$,    /* Address Line 1        */~
                                  saddr2$,    /* Address Line 2        */~
                                  " ",        /* Filler Spaces         */~
                                  tr_dte$,    /* Create Date           */~
                                  " ",        /* Filler Spaces         */~
                                  edi_cust1$   /* Customer Number EWD006*/


L03210:       FMT CH(1), CH(1), CH(4), CH(3), CH(8), CH(3), CH(2), CH(2),~
                  CH(17), CH(30), CH(18), CH(2), CH(5), CH(30), CH(30),  ~
                  CH(76), CH(06), CH(9), CH(9)

              write #6, eod goto L03270  /*Write APCEDIMC Bill TO Address*/
L03260: return
L03270:   call "SHOSTAT" ("(Error) - Dup Name Addr for ("&edi_inv$&")")
          stop
        return

L03310:    if lh_flag$ <> "L" then return  /* Create Line Item Records */
                             /* Notes - 'VEN_IN$' - EWD Part Number    */
                             /*         'BUYIN$'  - Customer's Part No.*/
              str(aribuf2_key$,1%,9%)  = edi_cust$
              str(aribuf2_key$,10%,8%) = edi_inv$
              str(aribuf2_key$,18%,3%) = edi_seq$   
                                                    /*    (linedisc)      */ 
              read #2,key = aribuf2_key$,using  L03390,ven_in$, item_desc$,~
                                       qty_ord, unit_pri, linedisc,        ~
                                                          eod goto L03970

L03390:       FMT POS(24), CH(25), CH(32), POS(93), PD(14,4), POS(133),  ~
                  PD(14,4), PD(14,4)

REM              gosub lookup_mfg
              if hit% = 0% then goto L03400
                 buy_in$ = old_skuno$
                 goto L03490

L03400:       goto L03470      /* Make Plymart Sku Number AWD Part Number */
              str(sku_key$,1%,3%)  = edi_id$   /*Get Buyer Sku Part NO. */
              str(sku_key$,4%,25%) = str(ven_in$,1%,25%)
              read #7,key 1% = sku_key$,using  L03450,buy_in$,eod goto L03470
L03450:          FMT POS(4), CH(25)
                 goto L03490                /* Found Their Sku Number   */
L03470:       REM buy_in$ = ven_in$             /* Set our Product to Theirs*/

              str(buy_in$,1%,25%) = str(ven_in$,1%,25%) 
                                            

L03490:       str(edi_ln$,1%,3%) = "   "    /* Set Line Number Sequence */
              str(edi_ln$,4%,3%) = edi_seq$
                                          
                                          
              unit_pri = round(unit_pri * (1 - (linedisc/100.0)), 2)

        
              unit_pri = round( unit_pri, 4)        /* Round Unit Price */

*        VRW MUST CHANGE BACK TO 100    /*Convert Numeric Data To Ascii*/
              unit_pri = 10000 * unit_pri
              convert unit_pri to unit_pri$, pic (########)
              qty_ord = abs(qty_ord)
              convert qty_ord to qty_ord$, pic (##########)

              str(del_key$,1%,1%)  = "S" /*Set APCEDIMC File Delete Key*/
              str(del_key$,2%,1%)  = "I"
              str(del_key$,3%,4%)  = today$
              str(del_key$,7%,3%)  = edi_id$
              str(del_key$,10%,8%) = edi_inv$
              str(del_key$,18%,3%) = edi_seq$
              str(del_key$,21%,2%) = " I"
              read #6,hold,key = del_key$,eod goto L03700
                   delete #6                  /* Delete Dup Line Item  */
                                              /* Build Line Item Record*/
L03700:       put #6,using  L03890, "S",        /* STATUS CODE (S/T/R)   */~
                                  "I",        /* Record Type (S/I/A)   */~
                                  today$,     /* Today's Date (MM/YY)  */~
                                  edi_id$,    /* EDI Identifer         */~
                                  edi_inv$,   /* Invoice Number        */~
                                  edi_seq$,   /* Invoice Line Item     */~
                                  " I",       /* Record Code (i.e. H/I)*/~
                                  edi_ln$,    /* Invoice Line Item     */~
                                  buy_in$,    /* Buyers Item Number    */~
                                  ven_in$,    /* Vendor Item Number    */~
                                  qty_ord$,   /* Quantity Ordered      */~
                                  "EA",       /* Unit Of Measure Code  */~
                                  unit_pri$,  /* Unit Price            */~
                                  item_desc$, /* Item Description      */~
                                  " ",        /* Filler Spaces         */~
                                  tr_dte$,    /* Create Date           */~
                                  " ",        /* Filler Spaces         */~
                                  edi_cust1$   /* Customer Number  EWD006*/

L03890:       FMT CH(1), CH(1), CH(4), CH(3), CH(8), CH(3), CH(2), CH(6),~
                  CH(25), CH(25), CH(10), CH(2), CH(8), CH(32), CH(102), ~
                  CH(6), CH(9), CH(9)

              write #6, eod goto L03950       /* Write APCEDIMC Line Item*/
        return
L03950:       call "SHOSTAT" ("(Error)-Dup Line Item for ("&edi_inv$&")")
              stop
L03970: return

        set_contract_due_dates              /* Only Set for Lowe's Store*/
        return                              

/* Changes to this subroutine are made to handle the string dates */
/* to include century.  Two new vars are being used to calculate  */
/* the due dates but are string$10 so they can include centry     */
/* the string$10 vars are then dateconv'ed back to pd(11,1)       */
/* and stuffed back into their original vars (net_due_dt$, and    */
/* dsc_due_dt$).                                                  */


REM            if edi_id$ <> "002" and edi_id$ <> "003" and            ~
               edi_id$ <> "005" then return             


              work_date$ = inv_date$
              call "EWDMDY" (work_date$, 1%, mm%, mm$, dd%, dd$, yy%, yy$)
              rem convert str(inv_date$,1%,2%) to yy%, data goto L04500
              rem convert str(inv_date$,3%,2%) to mm%, data goto L04500
              rem convert str(inv_date$,5%,2%) to dd%, data goto L04500


              init(" ")dsc_due_dt$, net_due_dt$, cnet_due_dt$, cdsc_due_dt$


              net_due_dt$    = blankdate$
              dsc_due_dt$    = blankdate$


              if dd% <= 15% then goto L04160    /* CHECK INV DAY <= 15TH */
                 mm% = mm% + 1%
                 if mm% <= 12% then L04120
                    mm% = 1% : yy% = yy% + 1%
 L04120:         convert mm% to mm$, pic(00)
                 convert yy% to yy$, pic(0000)
                 cnet_due_dt$ = yy$&mm$&"16"
                 cdsc_due_dt$ = yy$&mm$&"15"
                 goto L04470


L04160:          workdate$ = inv_date$
                 call "EWDMDY" (workdate$, 1%, mm%,mm$,dd%,dd$,yy%,yy$)
                 rem yy$ = str(inv_date$,1%,2%)
                 rem mm$ = str(inv_date$,3%,2%)


                 if mm% <> 2% then L04260
                    cdsc_due_dt$ = yy$&mm$&"28"
                    zz = yy%
                    if mod(zz,4) = 0 then dsc_due_dt$ = yy$&mm$&"29"
                    mm$ = "03"
                    cnet_due_dt$ = yy$&mm$&"01"
                    goto L04470
L04260:          if mm% <> 4% then L04310
                    cdsc_due_dt$ = yy$&mm$&"30"
                    mm$ = "05"
                    cnet_due_dt$ = yy$&mm$&"01"
                    goto L04470
L04310:          if mm% <> 6% then L04360
                    cdsc_due_dt$ = yy$&mm$&"30"
                    mm$ = "07"
                    cnet_due_dt$ = yy$&mm$&"01"
                    goto L04470
L04360:          if mm% <> 9% then L04410
                    cdsc_due_dt$ = yy$&mm$&"30"
                    mm$ = "10"
                    cnet_due_dt$ = yy$&mm$&"01"
                    goto L04470
L04410:          if mm% <> 11% then L04460
                    cdsc_due_dt$ = yy$&mm$&"30"
                    mm$ = "12"
                    cnet_due_dt$ = yy$&mm$&"01"
                    goto L04470
L04460:          cdsc_due_dt$ = yy$&mm$&"30"
                 cnet_due_dt$ = yy$&mm$&"31"


L04470:     call "DATECONV" (cnet_due_dt$)
            call "DATECONV" (cdsc_due_dt$)
            str(net_due_dt$,1%,6%) = str(cnet_due_dt$,1%,6%)
            str(dsc_due_dt$,1%,6%) = str(cdsc_due_dt$,1%,6%)


            return

        lookup_mfg                                  
           init(" ") upc_code$, old_skuno$, readkey$, desc$
           hit% = 0%
           read #9,key = ven_in$,using L04600, upc_code$, eod goto L04700
L04600:       FMT POS(566), CH(12)

           str(readkey$,1%,9%)   = "PLANLABSP"
           str(readkey$,10%,15%) = upc_code$
           read #4,key = readkey$, using L04650, desc$, eod goto L04700
L04650:        FMT POS(25), CH(30)
              old_skuno$ = str(desc$,6%,25%)
              hit% = 1%
L04700: return
                                                    
        exit_program
         end
