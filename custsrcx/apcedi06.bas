        REM *-----------------------------------------------------------*~
            *                                                           *~
            *  AAAAA   PPPP     CCCC   EEEE   DDDD   IIIIII   000   666 *~
            *  AA AA   PP  P   CC      EE     DD  D    II    0   0 66   *~
            *  AAAAA   PPPPP   CC      EEEE   DD  D    II    0   0 6666 *~
            *  AA AA   PP      CC      EE     DD  D    II    0   0 66  6*~
            *  AA AA   PP       CCCC   EEEE   DDDD   IIIIII   000   666 *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCEDI06 - Create Flat File To Update APC PO's            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/01/93 ! Original                                 ! VRW *~
            * 03/13/95 ! Added Creation of Comment File           ! RMT *~
            * 01/23/98 ! y2k compliance                           ! DJD *~
            * 11/02/01 ! (EWD001) - Mod to turn OFF ACK           ! CMG *~
            *-----------------------------------------------------------*

        dim ack_str$80,                  /* Acknowledgement Record     */~
            axd$(10)4,                   /*                            */~
            buy_in$21,                   /* Customer Sku Part Number   */~
            date$8,                      /* Today's Date               */~
            dun$15,                      /* Customer DUNS Number       */~
            dun_sav$15,                  /* DUNS Number from GENCODES  */~
            del_key$28,                  /* APCEDIMC File Key          */~
            edi_crtl$9,                  /* EDI Control Number         */~
            edi_dun$15,                  /* Customer DUNS Number       */~
            edi_desc$15,                 /* Customer Description/Name  */~
            edi_inv$8,                   /* Customer Invoice Number    */~
            edi_key$41,                  /* APCEDI File Key            */~
            edi_seq$3,                   /* Invoice Sequence/Record Cnt*/~
            edi_store$6,                 /* Customer Store Number      */~
            apc_store$6,                 /* APC Cust Number Code       */~
            f2%(10),                     /* = 0 if the file is open    */~
            fs%(10),                     /* = 1 Open, -1 doesn't exist */~
                                         /*   0 if not checked         */~
            format_id$80,                /* Header Or Trailer Record   */~
            ic_array$(1000)61,           /* Interchange #, Store, PO   */~
            line_num$6,                  /* PO Line Item Number        */~
            po_num$16,                   /* Purchase Order Number      */~
            prev_inter$9,                /* Previous Interchange #     */~
            prev_store$6,                /* Previous Store             */~
            prev_po$16,                  /* Previous Purchase Order #  */~
            prev_dun$15,                 /* Previous Duns Number       */~
           prev_cust$15,                /* Previous Customer Number   */~
            qty_ord$10,                  /* Quantity Order             */~
            rec_code$1,                  /* Record Type Code           */~
            rec_cnt$5,                   /* Record Count Sent          */~
            rslt$(64)20,                 /* Text From File Opening     */~
            sav_po$16,                   /* Held Purchase Order Number */~
            store$4,                     /* Customer Store Number      */~
            time$8,                      /* Acknowledgement Time Stamp */~
            today$4,                     /* TODAY'S DATE (MM/DD)       */~
            xref_key$21,                 /* APCEDIRF File Key          */~
            comm_str$80,                 /* Input Comment Record       */~
            comm_rec$(10)128,            /* Output Comment Record      */~
            tot_comm$2,                  /* # of Comment Records       */~
            readprt$12,                  /* Trading Part. Read Key     */~
            readstore$21,                /* APCEDIRF Store # Read Key  */~
            trpart_code$3                /* Trading Partner Code       */

/* <<<<<<<<<< Y2K >>>>>>>>>> */
        dim blankdate$6,                 /* empty date format          */~
            tr_dte$6,                    /* Transaction date (today)   */~
            workdate$8,                  /* for working with dates     */~
            workdate%                    /* for testing dates          */
/* <<<<<<<<<< Y2K >>>>>>>>>> */

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCEDI   ! APC EDI Master Control Purchase Orders   *~
            * #2  ! APCEDIRV ! APC EDI PO Recieve File                  *~
            * #3  ! GENCODES ! GENCODES Table File                      *~
            * #4  ! APCEDIMC ! APC EDI Master Control Send/Ack File     *~
            * #5  ! APCEDIRF ! APC EDI Customer Cross Reference File    *~
            * #6  ! APCEDICM ! APC EDI PO Comment File                  *~
            *************************************************************~
            *              File Selection and Open Calls                *~
            *************************************************************

            select #1, "APCEDI",                                         ~
                        varc,     indexed,  recsize = 102,               ~
                        keypos =   10, keylen =  41,                     ~
                        alt key  1, keypos  = 1, keylen = 9, dup

            select #2, "APCEDIRV",                                       ~
                        varc,     consec,   recsize = 80

            select #3, "GENCODES",                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   1, keylen =  24

            select #4, "APCEDIMC",                                       ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   1, keylen =  22,                      ~
                        alt key    1 , keypos =  10, keylen =  13, dup,  ~
                            key    2 , keypos = 233, keylen =  06, dup

            select #5, "APCEDIRF",                                       ~
                        varc,     indexed,  recsize = 32,                ~
                        keypos =   1, keylen =  21,                      ~
                        alt key 1, keypos = 22, keylen =  9

            select #6, "APCEDICM",                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   1, keylen = 33,                       ~
                        alt key 1, keypos = 01, keylen = 06, dup,        ~
                            key 2, keypos = 07, keylen = 03, dup,        ~
                            key 3, keypos = 10, keylen = 06, dup,        ~
                            key 4, keypos = 16, keylen = 16, dup

            select printer (134)
            call "SETPRNT" (" ", "ICRP", 0%, 0%)

            call "SHOSTAT" ("Opening EDI Files...")
            call "OPENCHCK" (#1, fs%(1 ), f2%(1 ),  100%, rslt$(1 ))
            call "OPENFILE" (#2, "INPUT", f2%(2 ), rslt$(2 ), axd$(2) )
            call "OPENCHCK" (#3, fs%(3 ), f2%(3 ),    0%, rslt$(3 ))
            call "OPENCHCK" (#4, fs%(4 ), f2%(4 ), 1000%, rslt$(4 ))
            call "OPENCHCK" (#5, fs%(5 ), f2%(5 ),  100%, rslt$(5 ))
            call "OPENCHCK" (#6, fs%(6 ), f2%(6 ),  100%, rslt$(6 ))

        REM *************************************************************~
            *       M A I N  P R O C E S S  S E C T I O N                ~
            *************************************************************~

        call "SHOSTAT" ("Updating PO Master File..")
        gosub initialize_data               /* Initialize Data Variable */
        gosub build_po_mc                 /* Create Recieve APCEDI File */
        print using                                                      ~
        L55170 , grd_tot%           /* Print Audit Grand Total */
        gosub print_ic_report               /* Print Interchange Report */
        goto  exit_program                              /* Exit Program */

        initialize_data
            init (" ") ack_str$, buy_in$,  date$, dun$, edi_crtl$,       ~
                       edi_dun$, edi_inv$, edi_seq$, edi_store$,         ~
                       format_id$, line_num$, po_num$, qty_ord$,         ~
                       rec_code$, store$, today$, xref_key$, time$,      ~
                       ic_array$(), prev_inter$, prev_store$, prev_po$,  ~
                       prev_dun$, prev_cust$, trpart_code$, comm_rec$()

/* <<<<<<<<<< Y2K >>>>>>>>>> */            
            date$=date
	    call "DATFMTC" (date$)              /* Convert to mm/dd/yy */
            today$=str(date$,1,2)               /* Get MM              */
            str(today$,3,2)=str(date$,4,2)      /* Get DD              */
/* <<<<<<<<<< Y2K >>>>>>>>>> */
            time$ = time

/* Y2K */
            tr_dte$=date                        /* Dont unpack this var! */

            rem call "DATEFMT" (date$)
            call "TIME"(rpt_time$)
            call "EXTRACT" addr("ID", user$)
/* Y2K */
            call "DATUFMTC" (blankdate$)

            rec_cnt%, prec_cnt%, li_num%, sub%  =  0%
            grd_tot%, page_no% = 0% : lcnt%=99%
            gosub init_data
        return

        init_data
           tot_li%, tot_ack%, tot_poa%, tot_head%, tot_nme%, tot_hdtr%=0%
           tot_rec%, tot_comm% = 0%
        return

        build_po_mc

        read_next
           read #2,using L10430,rec_code$, eod goto L10970
L10430:         FMT CH(1)
                grd_tot%=grd_tot%+1% : tot_rec%=tot_rec%+1%
                if rec_code$ <> "I" then L10540
                   tot_li%=tot_li%+1%
                   get #2, using L10480 , line_num$, buy_in$, qty_ord$
L10480:            FMT POS(2), CH(6), CH(25), CH(10)
                   if line_num$<>"000000" then put_records
                      li_num%=li_num%+1%
                      convert li_num% to line_num$, pic (000000)

                   goto put_records
L10540:         if rec_code$<>"A" then L10590    /* Test For Invoice Ack */
                   tot_ack%=tot_ack%+1%
                   get #2, using L10570 , ack_str$
L10570:            FMT CH(80)
                   goto put_records
L10590:         if rec_code$<>"@" then L10650     /* Test For PO Acknow. */
                   tot_poa%=tot_poa%+1%
                   format_id$="#APCBUI-POACK"
                   get #2, using L10630 , ack_str$
L10630:            FMT POS(3), CH(78)          /*  (EWD001)           */
                   format_id$="#850-DATA"
                   goto read_next              /* Mod to turn off ACK */
                                               /* for testing ctrl num*/
                                               /* to Lowe's           */
                   goto put_records
L10650:         if rec_code$<>"H" then L10730  /* Test For PO Header Rec */
                   tot_head%=tot_head%+1%
                   get #2, using L10680 , po_num$
L10680:            FMT POS(2), CH(16)
                   if sav_po$=po_num$ then read_next
                      sav_po$=po_num$
                      li_num%=0%
                   goto read_next
L10730:         if rec_code$<>"C" then L10790  /* Test For Comment Rec */
                   txtnum% = txtnum% + 1
                   tot_comm% = tot_comm% + 1
                   get #2, using L10770 , comm_str$
L10770:            FMT POS(2), CH(79)
                   goto build_comments
L10790:         if rec_code$<>"N" then L10930  /* Test For Address Rec. */
                   tot_nme%=tot_nme%+1%
                   get #2, using L10820 , store$
L10820:            FMT POS(5), CH(04)
                   edi_store$="00"
                   str(edi_store$,3,4)=store$
                   if txtnum% > 0% then gosub put_comments
                   sub% = sub% + 1
                   ic_array$(sub%)            = edi_ctrl$
                   str(ic_array$(sub%),10,6)  = edi_store$
                   str(ic_array$(sub%),16,16) = po_num$
                   str(ic_array$(sub%),32,15) = edi_dun$
                   str(ic_array$(sub%),47,15) = edi_desc$
                   goto read_next
L10930:         if rec_code$<>"#" then read_next  /*Test For Head/Trail*/
                   tot_hdtr%=tot_hdtr%+1%
                   gosub header_trailer
                   goto read_next
L10970: return

        put_records
          if str(format_id$,1,9)<>"#850-DATA"  then L11320
             put #1, using                                               ~
        L11120, edi_ctrl$,      /* Edi Control Number */                  ~
                                "N",             /* Process Flag (Y/N) */~
                                edi_dun$,        /* Customer DUNS #    */~
                                edi_store$,      /* Customer Store     */~
                                po_num$,         /* Purchase Order #   */~
                            str(line_num$,4,3),  /* PO Line Item #     */~
                                buy_in$,         /* Buyer's SKU Part   */~
                                qty_ord$,        /* Quantity Ordered   */~
                                " "              /* Spaces             */

L11120:       FMT CH(09), CH(1), CH(15), CH(6), CH(16), CH(3), CH(25),   ~
                  CH(10), CH(17)

              write #1, eod goto L11190   /* Write APCEDI Detail Records */
              rec_cnt%=rec_cnt%+1%          /* Increment Record Counter */
              goto read_next                /* Get Next APCEDIRV Record */

L11190:       edi_key$="N"                /* Set APCEDI File Delete Key */
              str(edi_key$,2,15)=edi_dun$
              str(edi_key$,17,6)=edi_store$
              str(edi_key$,23,16)=po_num$
              str(edi_key$,39,3)=str(line_num$,4,3)
              read #1,hold,key = edi_key$,eod goto read_next
              delete #1                            /* Delete Dup Header */
              write #1, eod goto L11280       /* Write APCEDI LI Rrecord */
              goto read_next
L11280:         stop " UPDATE APCEDI "
                close ws
              goto read_next                /* Get Next APCEDIRV Record */

L11320:   if str(format_id$,1,12)<>"#APCBUI-ACKS"  then L11720
                                            /* Increment Record Counter */
             convert tot_ack% to edi_seq$, pic (###)

/* <<<<<<<<<< Y2K >>>>>>>>>> */

             edi_inv$ = str(ack_str$,41,8)    /* Get Customer Number */
             put #4, using L11500,"R",         /* STATUS CODE (S/T/R)   */~
                                 "I",         /* Record Type (S/I)     */~
                                 bi_today%,   /* Today's Date (MM/DD)  */~
                                 edi_id$,     /* EDI Identifier        */~
                                 edi_inv$,    /* Invoice Number        */~
                                 edi_seq$,    /* Record Count          */~
                                 " ",         /* Spaces                */~
                                 ack_str$,    /* Acknowledgement Record*/~
                                 " ",         /* Spaces                */~
                                 tr_dte$,     /* Recieve Date          */~
                                 " "          /* Spaces                */~

L11500:       FMT CH(1), CH(1), BI(4), CH(3), CH(8), CH(3), CH(1),       ~
                  CH(80), CH(131), CH(6), CH(18)

/* <<<<<<<<<< Y2K >>>>>>>>>> */

              write #4, eod goto L11570 /* Write APCEDIMC Detail Records */
              rec_cnt% = rec_cnt% + 1%      /* Increment Record Counter */
              goto read_next                /* Get Next APCEDIRV Record */

L11570:       del_key$="R"              /* Set APCEDIMC File Delete Key */
              str(del_key$,2,1)="I"
              str(del_key$,3,4)=today$
              str(del_key$,7,9)=edi_id$
              str(del_key$,16,8)=edi_inv$
              str(del_key$,24,3)=edi_seq$
              str(del_key$,27,2)=" A"
              read #4,hold,key = del_key$,eod goto read_next
              delete #4                       /* Delete Inv ACK. Record */
              write #4 , eod goto L11680   /* Write APCEDIMC File Header */
              goto read_next
L11680:          stop " STORE --> " & edi_id$ & " INV ---> " & edi_inv$
                 close ws
              goto read_next                /* Get Next APCEDIRV Record */

L11720:   if format_id$<>"#APCBUI-POACK"  then read_next
             prec_cnt%=prec_cnt%+1%         /* Increment Record Counter */
             convert prec_cnt% to edi_seq$, pic (###)
             time$=time
             put #4, using L11890, "S",        /* STATUS CODE (S/T/R )  */~
                                  "S",        /* Record Type (I/S)     */~
                                  today$,     /* Today's Date (MM/DD)  */~
                                  edi_id$,    /* EDI Identifier        */~
                                  time$,      /* Time Sequence         */~
                                  edi_seq$,   /* Acknowledge. Sequence */~
                                  " A",       /* Record Code i.e. A/H/I*/~
                                  ack_str$,   /* Acknowledgement Record*/~
                                  " " ,       /* Spaces                */~
                                  tr_dte$,    /* Create Date           */~
                                  " "         /* Spaces                */

L11890:          FMT CH(1), CH(1), CH(4), CH(3), CH(8), CH(3), CH(2),    ~
                     CH(78), CH(132), CH(6), CH(18)

                 write #4, eod goto L11960 /*Write APCEDIMC PO Ack Record*/
                 format_id$="#850-DATA"
                 goto read_next             /* Get Next APCEDIRV Record */

L11960:          del_key$="S"           /* Set APCEDIMC File Delete Key */
                 str(del_key$,2,1)="S"
                 str(del_key$,3,4)=today$
                 str(del_key$,7,9)=edi_id$
                 str(del_key$,16,8)=time$
                 str(del_key$,24,3)=edi_seq$
                 str(del_key$,27,2)=" A"
                 read #4,hold,key = del_key$,eod goto read_next
                 delete #4                     /* Delete PO ACK. Record */
                 write #4, eod goto L12070 /* Write APCEDIMC File Header */
                 goto read_next
L12070:            stop "EDI ID ACK  ---> "& edi_id$
                   close ws
                 goto read_next             /* Get Next APCEDIRV Record */
        return

        build_comments
          if str(format_id$,1,9)<>"#850-DATA" then goto L12290
          readprt$ = "PARTNERS1" & "002"
          read #3,key = readprt$,using L12160,dun_sav$, eod goto L12290
L12160:     FMT POS(25), CH(15)
          if dun_sav$ = dun$ then goto L12200
            trpart_code$ = "001"
            goto L12210
L12200:   trpart_code$ = "002"
L12210:   convert txtnum% to tot_comm$, pic(00)
          str(comm_rec$(txtnum%),1%,6%)=po_date$       /* PO DATE */
          str(comm_rec$(txtnum%),7%,3%)=trpart_code$   /* EDI ID  */
          str(comm_rec$(txtnum%),10%,6%)="000000"      /* STORE # */
          str(comm_rec$(txtnum%),16%,16%)=po_num$      /* PO NUM  */
          str(comm_rec$(txtnum%),32%,2%)=tot_comm$     /* # OF COMMENTS */
          str(comm_rec$(txtnum%),34%,79%)=comm_str$    /* COMMENT */
          goto read_next
L12290:   call "SHOSTAT" ("ERROR WITH COMMENT RECORDS") : stop
          goto read_next
        return

        put_comments
          readstore$ = edi_dun$ & "                "
          str(readstore$,16%,6%) = edi_store$
          read #5,key = readstore$,using L12380,apc_store$,eod goto L12460
L12380:      FMT POS(22), CH(6)
          for i% = 1% to txtnum%
             str(comm_rec$(i%),10%,6%)=apc_store$
             write #6, using L12420, comm_rec$(i%), eod goto L12480
L12420:      FMT CH(128)
          next i%
          txtnum% = 0%
        return
L12460:   call "SHOSTAT" ("ERROR WITH STORE" & edi_store$) : stop
        return
L12480:   call "SHOSTAT" ("DUPLICATE COMMENT RECORD, WILL SKIP") : stop
          txtnum% = 0%
          goto read_next
        return

        header_trailer
          spccnt%, prevpos%, strlen%=0%
          get #2, using L12560  , format_id$
L12560:       FMT CH(80)            /* Translate Header/Trailer Records */
          if str(format_id$,1,4)="#EOT" then L12770
             if str(format_id$,1,1)<>"#" then return
                for i% = 1% to 80%
                    if str(format_id$,i%,1)<>" " then L12760
                       spccnt%=spccnt%+1%
                       if spccnt%<>2% and spccnt%<>5% and spccnt%<>7%    ~
                       then L12650
                          prevpos%=i%+1%
L12650:                if spccnt%<>3% then L12680
                          strlen%=(i%-prevpos%)
                          edi_dun$=str(format_id$, prevpos%, strlen%)
L12680:                if spccnt%<>6% then L12710
                          strlen%=(i%-prevpos%)
                          po_date$=str(format_id$, prevpos%, strlen%)

/* <<<<<<<<<< Y2K >>>>>>>>>> */
          		  str(workdate$,1%,6%) = str(po_date$,1%,6%)
          		  convert str(workdate$,1%,2%) to workdate%

                          /* If the date is less than 89 then put a 20 in front */
	                  /* otherwise - put a 20 in front                      */
	                  if workdate% > 89 then goto Handle1900
          
          		  str(workdate$,1%,8%) = "20" & str(workdate$,1%,6%)
          		  goto skip1900
Handle1900
          		  str(workdate$,1%,8%) = "19" & str(workdate$,1%,6%)

Skip1900
          		  call "DATECONV" (workdate$)
                          str(po_date$,1%,6%) = str(workdate$,1%,6%)
	  
/* <<<<<<<<<< Y2K >>>>>>>>>> */


L12710:                if spccnt%<>8% then L12740
                          strlen%=(i%-prevpos%)
                          edi_ctrl$=str(format_id$, prevpos%, strlen%)
L12740:                if spccnt%<>9% then L12760
                          goto L12920
L12760:         next i%
L12770:   if str(format_id$,1,4)<>"#EOT" then L12920
             gosub print_audit_report
             gosub init_data
             for i% = 1% to 80%
                 if str(format_id$,i%,1)<>" " then L12900
                    spccnt%=spccnt%+1%
                    if spccnt%<>2% then L12850
                       prevpos%=i%+1%
L12850:             if spccnt%<>3% then L12880
                       strlen%=(i%-prevpos%)
                       rec_cnt$=str(format_id$, prevpos%, strlen%)
L12880:             if spccnt%<>4% then L12900
                       return
L12900:      next i%

L12920:      cde_key$="PARTNERS1"                 /* Get EDI Identifier */
             str(cde_key$,10,3)=hex(00)
        read_partner
             read #3,key > cde_key$,using L12970,cde_key$,dun$, edi_desc$,~
                                                          eod goto L13010
L12970:           FMT CH(12), POS(25), CH(15), POS(41), CH(15)
             if str(cde_key$,1,9)<>"PARTNERS1" then return
             if dun$<>edi_dun$ then read_partner
                edi_id$=str(cde_key$,10,3)
L13010: return

        print_header
          page_no% = page_no% + 1%
          print page
          print using L55040  , date$, rpt_time$, user$, page_no%
          print
          print using L55060
          lcnt% = 5%
        return

        print_audit_report
          lcnt% = lcnt% + 9%
          if lcnt% > 55% then gosub print_header
          print using L55080
          print using L55100  , edi_dun$, edi_desc$, tot_head%
          print using L55110  , tot_li%
          print using L55120  , tot_nme%
          print using L55130  , tot_comm%
          print using L55140  , tot_poa%
          print using L55150  , tot_ack%
          print using L55160  , tot_hdtr%
          print
          print using L55170  , tot_rec%
        return

        print_header1
          page_no% = page_no% + 1%
          print page
          print using L55240, date$, rpt_time$, user$, page_no%
          print
          print using L55260
          print using L55080
          lcnt% = 4%
        return

        print_ic_report
          lcnt% = 56%
          page_no% = 0%
          call "SORT" addr(ic_array$(), sub%, 61%)
          for row% = 1% to sub%
              lcnt% = lcnt% + 1%
              if lcnt% > 55% then gosub print_header1
              if str(ic_array$(row%),1,9) =  prev_inter$ then            ~
                 str(ic_array$(row%),1,9) = " "          else            ~
                 prev_inter$ = str(ic_array$(row%),1,9)

              if str(ic_array$(row%),10,6) = prev_store$  then           ~
                 str(ic_array$(row%),10,6) = " "          else           ~
                 prev_store$ = str(ic_array$(row%),10,6)

              if str(ic_array$(row%),16,16) = prev_po$  then             ~
                 str(ic_array$(row%),16,16) = " "       else             ~
                 prev_po$    = str(ic_array$(row%),16,16)

              if str(ic_array$(row%),32,15) = prev_dun$  then            ~
                 str(ic_array$(row%),32,15) = " "        else            ~
                 prev_dun$   = str(ic_array$(row%),32,15)

              if str(ic_array$(row%),47,15) = prev_cust$ then            ~
                 str(ic_array$(row%),47,15) = " "        else            ~
                 prev_cust$  = str(ic_array$(row%),47,15)

              print using L55280, str(ic_array$(row%),1,9),               ~
                str(ic_array$(row%),10,6), str(ic_array$(row%),16,16),   ~
                str(ic_array$(row%),32,15), str(ic_array$(row%),47,15)
          next row%
        return

*       *****************************************************************
*        FORMAT STATEMENTS FOR AUDIT REPORT
*       ****************************************************************

L55040: %######## ######## ID: ###     APC EDI DATA RECEIVED AUDIT REPORT~
        ~      PAGE: ####
L55060: %DUNS NUMBER     CUSTOMER        ITEM TYPE              TRANSMIT ~
        ~COUNT
L55080: %----------------------------------------------------------------~
        ~-----
L55100: %############### ############### PURCHASE ORDER HEADERS      #####
L55110: %                                PURCHASE LINE ITEMS         #####
L55120: %                                PURCHASE NAME/ADDRESS       #####
L55130: %                                PO COMMENTS                 #####
L55140: %                                PO ACKNOWLEDGEMENTS         #####
L55150: %                                INVOICE ACKOWLEDGEMENTS     #####
L55160: %                                ACCOUNT HEADERS/TRAILERS    #####
L55170: %TOTAL RECORDS                                              ######
        %GRAND TOTAL RECORDS                                        ######

*       *****************************************************************
*        FORMAT STATEMENTS FOR INTERCHANGE REPORT
*       ****************************************************************

L55240: %######## ######## ID: ###     APC EDI PURCHASE ORDER INTERCHANGE~
        ~ REPORT  PAGE: ####
L55260: %INTERCHANGE NBR   STORE   PO NUMBER         DUNS NUMBER         ~
        ~CUSTOMER
L55280: %########          ######  ################  ###############     ~
        ~###############

        
        exit_program
         close #1                                /* Close APCEDI   File */
         close #2                                /* Close APCEDIRV File */
         close #3                                /* Close GENCODES File */
         close #4                                /* Close APCEDIMC File */
         close #5                                /* Close APCEDIRF File */
         close #6                                /* Close APCEDIRF File */
         end
