        REM *-----------------------------------------------------------*~
            *                                                           *~
            * AAAAAA  W   W DDDD   EEEE  DDDD  IIIIII   0000   77777    *~
            * AA  AA  W   W DD  D  EE    DD  D   II    0    0      7    *~
            * AAAAAA  W W W DD  D  EEEE  DD  D   II    0    0     7     *~
            * AA  AA  WW WW DD  D  EE    DD  D   II    0    0    7      *~
            * AA  AA  W   W DDDD   EEEE  DDDD  IIIIII   0000    7       *~
            *-----------------------------------------------------------*~
            * AWDEDI06 - Create Flat File To Send To IPNET              *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN-----+--------------WHAT----------------------+-WHO-*~
            * 06/04/2006 ! Copied from awdedi04                   ! DES *~
            * 01/18/2007 ! AWD001 split sku 042 & 043             ! DES *~
            * 01/18/2010 ! AWD002 mod to get approval number from ! CMG *~
            *            !  ordermaster                           !     *~
            *06/03/2011  ! (AWD003) - add orcl usr & pswd lookup  ! CMG *~
            *-----------------------------------------------------------*


        dim apcmc_key$22,                /* ARIEDIMC File Key          */~
            axd$(64%)4,                  /*                            */~
            cde_key$12,                  /* GENCODES File Key          */~
            cde_str$56,                  /* GENCODES Rewrite String    */~
            detail_send_item$226,        /* Flat File Detail String    */~
            detail_send_unpk$226,        /*   with dates unpacked      */~ 
            edi_id$3,                    /* Cutomer Trading ID         */~
            edi_cust$9,                  /* Customer Number            */~
            edi_desc$15,                 /* Customer Description       */~
            edi_duns$15,                 /* Customer DUNS Number       */~
            f2%(64%),                    /* = 0 if the file is open    */~
            fs%(64%),                    /* = 1 Open, -1 doesn't exist */~
            f1%(25%),                    /* = 1 if READ was successful */~
            hd_str$39,                   /* Header String              */~
                                         /*   0 if not checked         */~
            new_cntl$9,                  /* Next Control Number        */~
            old_cntl$9,                  /* Old Control Number         */~
            part1$4,                     /* Record Part Of APCEDIMC    */~
            part2$12,                    /* Record Part Of APCEDIMC    */~
            rec_tot$5,                 /* Record Counter             */~
            rec_type$1,                  /* Record Type                */~
            rslt$(64%)20,                /* Text From File Opening     */~
            readkey$24, desc$30,         /* Gencodes Primary Key       */~
            save_id$3,                 /* Save EDI Identifier        */~
            save_042$3,                 /* Save EDI Identifier        */~
            save_043$3,                 /* Save EDI Identifier        */~
            sav_type$2,                  /* Transaction Type Code      */~
            tr_str$20,                   /* Trailer String             */~
            trans_date$8,                /* Transmit date              */~
            type$2,                      /* Record Type                */~
            work_date1$10, work_date2$10 /* Temp dates                 */    

        dim run_042$1,run_043$1,     /* AWD001 split edi sku's     */~
/*AWD003*/  userid$3                     /* USERID                     */



        dim file$8,                      /* IPNet Print File           */~
            library$8,                   /* Library Name = APCDATA     */~
            volume$6                     /* DISK VOLUME = CARLO2       */

/* (AWD002)  */
        dim                                                              ~
            salesorder$8,                /* Sales Order Number         */~
            approvalnumber$20,           /* Approval Number            */~
            server$25,                   /* Connection String          */~
            user$25,                     /* User name to Connect       */~
            pass$25,                     /* Password to Connect        */~
            stmt1$250,                   /* First Query String         */~
            stmt2$250,                   /* Second Query String        */~
            stmt3$250,                   /* Second Query String        */~
            stmt4$250,                   /* Second Query String        */~
            stmt5$250,                   /* Second Query String        */~
            stmt6$250,                   /* Second Query String        */~
            stmt7$250,                   /* Second Query String        */~
            stmt8$250,                   /* Second Query String        */~
            error$256,                   /* Error String               */~
            field$256,                   /* Query Return Field         */~
            name$50                      /* Field Name Returned        */
         
        dim schema$8                     /* Schema                     */            


            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCEDIGU ! AWD EDI Master IBSA File                 *~
            * #3  ! GENCODES ! GENCODES Table File                      *~
            * #4  ! AWD42SEN ! EWD IPNet Send Flat File SKU 042         *~
            * #5  ! AWD43SEN ! EWD IPNet Send Flat File SKU 043         *~
            * #6  ! SYSFILE2 ! Caelus Management System General Informa *~
            *************************************************************~
            *              File Selection and Open Calls                *~
            *************************************************************

            select #1, "APCEDIGU",                                       ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   1 , keylen =  22,                     ~
                        alt key    1 , keypos =  10, keylen =  13, dup,  ~
                            key    2 , keypos = 233, keylen =  06, dup


            select #3, "GENCODES",                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   1 , keylen =  24

            select #4, "AWD42SEN",                                       ~
                        varc,     consec,   recsize = 230
            select #5, "AWD43SEN",                                       ~
                        varc,     consec,   recsize = 230

            select #6,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            call "SETPRNT" ("IBSA", "IBSA", 0%, 0%)
            select printer (134)

            edi_sn% = 0%             

            call "SHOSTAT" ("Opening EDI Files...")
            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 1000%, rslt$(1%))


            call "OPENCHCK" (#3, fs%(3%), f2%(3%),    0%, rslt$(3%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),    0%, rslt$(6%))



            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #3, schema_err%)
                        
            init(" ") file$, library$, volume$
            init("N") run_042$, run_043$

            edi_id$ = "042"
            run_042$ = "Y"
            gosub check_process
            if str(desc$,1,2)="00" then run_042$ = "N"

            edi_id$ = "043"
            run_043$ = "Y"
            gosub check_process
            if str(desc$,1,2)="00" then run_043$ = "N"

            if run_042$ = "N" then goto SKIP042
            run_sku$ = "042"
            file$     = "AWD42SEN"         
            library$  = "FTPGUARD"
REM            volume$   = "CARLO2"

            if schema% = 1% then volume$ = "CARLO2"
            if schema% = 2% then volume$ = "NE2"  
            gosub open_file

SKIP042:
            if run_043$ = "N" then goto SKIP043
            run_sku$ = "043"
            file$     = "AWD43SEN"         
            library$  = "FTPGUARD"
REM            volume$   = "CARLO2"

            if schema% = 1% then volume$ = "CARLO2"
            if schema% = 2% then volume$ = "NE2"  
            gosub open_file

SKIP043:
        REM *************************************************************~
            *       M A I N  P R O C E S S  S E C T I O N                ~
            *************************************************************~

        call "SHOSTAT" ("Creating EDI Transmit File...")
        gosub initialize_data               /* Initialize Data Variable */
/*(AWD002)*/
        gosub connectOracle
        if oci_err% < 0 then goto exit_program

        gosub build_invoice_recs /* Write Invoice Recs To APCEDISN File */
        gosub build_poack_recs   /* Write PO Ack. Recs To APCEDISN File */
        gosub print_audit_report
        goto  exit_program                              /* Exit Program */

        initialize_data
            init (" ") apcmc_key$, cde_key$, cde_str$, detail_send_item$,~
                       edi_cust$, edi_duns$, new_cntl$, old_cntl$,       ~
                       rec_tot$, type$, edi_desc$, tr_str$,    ~
                       hd_str$, save_id$, save_042$, save_043$
            rec_042_tot% = 0%
            rec_043_tot% = 0%

            edi_cntl, new_cntl%, old_cntl% = 0%
            grnd_tot%, page_no% = 0%
            lcnt% = 99%

            
       REM  trans_date$ = date                                              /* Y2K */
            work_date1$ = date                                              /* Y2K */
            call "DATFMTC" (work_date1$, wd%, trans_date$)                  /* Y2K */
            trans_date$ = str(trans_date$, 3, 6)                            /* Y2K */

            date$ = date
            call "DATEFMT" (date$)
            time$ = time
            call "TIME"(rpt_time$)

            call "EXTRACT" addr("ID", userid$)

            gosub init_counters

/* (AWD002) */
            init(" ") server$, user$, pass$, stmt1$, stmt2$, stmt3$,     ~
                      stmt4$, stmt5$, stmt6$, stmt7$, stmt8$, error$,    ~
                      field$, name$

        return

        init_counters
            head_tot% = 0% : name_tot% = 0% : line_tot% = 0%
            po_tot%   = 0% : rec_tot%  = 0% : hdtr_tot% = 0%
        return

        check_process                          /* Check Process Flag   */
           edi% = 1%                           /* 'K' = Kleinsmidth    */
           init(" ") readkey$, desc$           /* 'P' = PC Anywhere    */
           str(readkey$,1%,9%)   = "EDI TRADE"
           str(readkey$,10%,15%) = edi_id$
           run_sku$=edi_id$
           read #3,key = readkey$, using L01240 , desc$, eod goto L01270
L01240:       FMT POS(25), CH(30)
           if str(desc$,25%,1%) = "P" then edi% = 0%  /* Skip Invoice  */
        return
L01270:    edi% = 0%
        return

        build_invoice_recs
                            /* Start File At First Invoice Send Record */
           apcmc_key$="SI"
           str(apcmc_key$,3,20)=hex(00)
           read #1,hold,key > apcmc_key$,using  L01370, type$, part1$,     ~
                edi_id$, part2$, detail_send_item$, edi_cust$,           ~
                eod goto L01780
L01370:         FMT CH(2), CH(4), CH(3), CH(12), CH(226), CH(9)
                goto L01450

        read_next_inv
           read #1,hold,key > apcmc_key$,using  L01370, type$, part1$,     ~
                edi_id$, part2$, detail_send_item$, edi_cust$,           ~
                eod goto L01750
L01450:
          run_sku$ = edi_id$
/* AWD001 */ if run_042$ = "N" and edi_id$ = "042" then goto read_next_inv
/* AWD001 */ if run_043$ = "N" and edi_id$ = "043" then goto read_next_inv

           str(apcmc_key$,1%,2%)   = type$
           str(apcmc_key$,3%,4%)   = part1$
           str(apcmc_key$,7%,3%)   = edi_id$
           str(apcmc_key$,10%,12%) = part2$
           str(apcmc_key$,22%,1%)  = str(detail_send_item$,1%,1%)
REM        if edi_id$ <> run_sku$ then goto L01750
           if type$<>"SI" then L01750
REM           gosub check_process
              if edi% = 0% then goto read_next_inv   /* Skip Invoice    */
          /* if not edi sku being processed, skip */

              rec_type$=str(detail_send_item$,1,1)   /* Set Record Type */
              sav_type$="TI"
              gosub reset_transmit_flag
REM           if edi_id$=save_id$ then L01610
/* AWD001 */  if edi_id$ = "042" and save_042$ <> " " then L01610            
/* AWD001 */  if edi_id$ = "043" and save_043$ <> " " then L01610            
REM           if save_id$ <> " " then L01610
                 gosub create_header_trailer
REM              save_id$=edi_id$
/* AWD001 */  if edi_id$ = "042" then save_042$ = edi_id$            ~
          else save_043$ = edi_id$
L01610:          gosub unpack_invoice_dates           
                 gosub detail_edi                 
                 gosub detail_ipn                 

*       *******************************************
*        Add Up All Counters For The Audit Report *
*       *******************************************
REM           rec_tot%=rec_tot%+1%  /* Account Record Total */
/* AWD001 */  if edi_id$ = "043" then rec_043_tot% = rec_043_tot% + 1%  ~
          else rec_042_tot% = rec_042_tot% + 1%
              grnd_tot%=grnd_tot%+1%             /* Grand Total Records */
              if rec_type$ = "B" then head_tot%=head_tot%+1% /* Bill to */
              if rec_type$ = "H" then head_tot%=head_tot%+1%    /*Header*/
              if rec_type$ = "I" then line_tot%=line_tot%+1% /*Line Item*/
              if rec_type$ = "N" then name_tot%=name_tot%+1% /* Address */
              goto read_next_inv            /* Get Next APCEDIMC Record */

L01750: 
REM        if rec_tot%=0% then goto L01780     /* Create Trailer Record */
/* AWD001 */ if rec_042_tot% = 0% then goto skip_042_trailer
              run_sku$ = "042"
              edi_id$ = "042"
              save_042$="EOD"
              save_id$="EOD"
              gosub create_header_trailer

skip_042_trailer:
/* AWD001 */ if rec_043_tot% = 0% then return               
              run_sku$ = "043"
              edi_id$ = "043"
              save_043$="EOD"
              save_id$="EOD"
              gosub create_header_trailer

L01780: return


        unpack_invoice_dates    /* Unpack dates before they're sent.       */

            init(" ") detail_send_unpk$                      
            detail_send_unpk$ = detail_send_item$

            if str(detail_send_unpk$, 1, 1) <> "H" then goto L01800

                work_date1$ = str(detail_send_unpk$,  10, 6)    /* EDI Date     */
                gosub unpack_date
                str(detail_send_unpk$,  10, 6) = work_date2$

                work_date1$ = str(detail_send_unpk$,  26, 6)    /* Date Shipped */
                gosub unpack_date
                str(detail_send_unpk$,  26, 6) = work_date2$

                work_date1$ = str(detail_send_unpk$,  68, 6)    /* PO Date      */
                gosub unpack_date
                str(detail_send_unpk$,  68, 6) = work_date2$
                work_date1$ = str(detail_send_unpk$,  74, 6)    /* Net Due Date */
                gosub unpack_date

                str(detail_send_unpk$,  74, 6) = work_date2$
                work_date1$ = str(detail_send_unpk$,  80, 6)    /* Disc Due Dte */
                gosub unpack_date

                str(detail_send_unpk$,  80, 6) = work_date2$
/* (AWD002) */
                gosub getApprovalNumber



L01800:     work_date1$ = str(detail_send_unpk$, 212, 6)        /* EDI Create   */
            gosub unpack_date
            str(detail_send_unpk$, 212, 6) = work_date2$

        return


        unpack_date         /* in - work_date1$ - PD(11,1)              */
                            /* out- work_date1$ - formatted             */
                            /*      work_date2$ - YYDDMM                */

            call "DATFMTC" (work_date1$, wd%, work_date2$)
            work_date2$ = str(work_date2$, 3, 6)
        return
       



        build_poack_recs
           if edi_sn% <> 1% then return    
                             /* Start File At First PO ACK. Send Record */
           apcmc_key$="SS"
           str(apcmc_key$,3,20)=hex(00)
           read #1,hold,key > apcmc_key$,using L01870, type$, part1$,      ~
                edi_id$, part2$, detail_send_item$, edi_cust$,           ~
                eod goto L02200
L01870:         FMT CH(2), CH(4), CH(3), CH(12), CH(226), CH(9)
                sav_id$=edi_id$
                goto L01950

        read_next_po
           read #1,hold,key > apcmc_key$,using  L01870, type$, part1$,     ~
                edi_id$, part2$, detail_send_item$, edi_cust$,           ~
                eod goto L02200
L01950:    if type$<>"SS" then L02200
              run_sku$=edi_id$
              rec_type$=str(detail_send_item$,1,1)   /* Set Record Type */
              sav_type$="TS"
              gosub reset_transmit_flag
              if edi_id$=sav_id$ then L02070
                 sav_id$=edi_id$
                 lcnt%=lcnt%+2%
                 if lcnt%>55% then gosub print_header
                 print
                 print using L03420, edi_duns$, edi_desc$, po_tot%
                 po_tot% = 0%

L02070:       gosub get_edi_duns

              detail_send_unpk$ = detail_send_item$                    
              work_date1$ = str(detail_send_unpk$, 212, 6)    /* EDI Date */
              gosub unpack_date
              str(detail_send_unpk$, 212, 6) = work_date2$             

              put #2, using L02090, str(detail_send_unpk$,2,225), " "
L02090:       FMT CH(225), CH(1)
              write #2                 /* Write APCEDISN Detail Records */

*       *******************************************
*        Add Up All Counters For The Audit Report *
*       *******************************************
              grnd_tot%=grnd_tot%+1%             /* Grand Total Records */
              if rec_type$ = "A" then po_tot%=po_tot%+1%  /* Acknowledge*/

              goto read_next_po             /* Get Next APCEDIMC Record */

L02200:       if po_tot% = 0% then L02250
                 lcnt%=lcnt%+2%
                 if lcnt%>55% then gosub print_header
                 print
                 print using L03420, edi_duns$, edi_desc$, po_tot%
L02250:       lcnt%=lcnt%+2%
              if lcnt%>55% then gosub print_header
              print
              print using L03450 , grnd_tot%
        return

        get_edi_duns
           cde_key$="PARTNERS1"            /* Get Customers Duns Number */
           str(cde_key$,10,3)=edi_id$
           read #3,key = cde_key$,using  L02360,edi_duns$, edi_desc$,      ~
                                                            eod goto L02370
L02360:         FMT POS(25), CH(15), POS(41), CH(15)
L02370: return

        create_header_trailer
/*AWD001*/ if edi_id$ = "042" and save_id$ <> "EOD" then save_id$ = " " 
/*AWD001*/ if edi_id$ = "043" and save_id$ <> "EOD" then save_id$ = " " 
           gosub get_edi_duns

           cde_key$="EDI TRADE"          /* Get Next EDI Control Number */
           str(cde_key$,10,3)=edi_id$
           read #3,hold,key = cde_key$,using L02470 , cde_str$, edi_cntl,  ~
                eod goto L02560
L02470:         FMT CH(56), PD(09)
                edi_cntl = 0
                if edi_cntl > 999999999 then edi_cntl = 0
                old_cntl%=edi_cntl
                if save_id$<>"EOD" then edi_cntl=edi_cntl+1
                new_cntl%=edi_cntl
                put #3 using L02470 , cde_str$, edi_cntl
                rewrite #3  /*Rewrite Updated Control Number To GENCODES*/

                                       /* Convert Numeric Data To Ascii */
L02560:         convert old_cntl% to old_cntl$, pic(#########)
                convert new_cntl% to new_cntl$, pic(#########)

                if save_id$<=" " then L02770     /* Create Trailer Record */
REM                rec_tot% = rec_tot% + 1% 
/* AWD001 */  if edi_id$ = "043" then rec_043_tot% = rec_043_tot% + 1%  ~
          else rec_042_tot% = rec_042_tot% + 1%
          if edi_id$ = "043" then rec_tot% = rec_043_tot%        ~
          else rec_tot% = rec_042_tot%
                   convert rec_tot%  to rec_tot$,  pic(#####)
                   tr_str$ = "#EOT "
                   str(tr_str$,6,9)=old_cntl$
                   str(tr_str$,16,5)=rec_tot$
                   call "SPCESMSH" (tr_str$,1%)
                   gosub trailer_edi              
                   gosub trailer_ipn              
                   hdtr_tot% = hdtr_tot%+1%       /* Add Total Trailers */
                   grnd_tot% = grnd_tot%+1%
REM                gosub print_audit_report
REM                gosub init_counters

L02770:
                if save_id$="EOD" then return   /* Create Header Record */
xL02770:           hd_str$=edi_duns$                  /* Destination Id */
                   str(hd_str$,17,8)=trans_date$
                   str(hd_str$,26,4)=time$
                   str(hd_str$,31,9)=new_cntl$  /* Next Control Number */
                   call "SPCESMSH" (hd_str$,1%)
                   gosub header_edi                                
                   gosub header_ipn                                
                   hdtr_tot% = hdtr_tot%+1%        /* Add Total Headers */
REM                rec_tot% = rec_tot% + 1% 
/* AWD001 */  if edi_id$ = "043" then rec_043_tot% = rec_043_tot% + 1%  ~
          else rec_042_tot% = rec_042_tot% + 1%
                   grnd_tot% = grnd_tot%+1%
        return

                         
        trailer_edi
          if edi_sn% <> 1% then return
          put #2, using L02680, tr_str$, /* Trailer String      */~
                                " "      /* End Of Record       */
          write #2         /* Write Trailer Record To APCEDISN */
        return

        trailer_ipn
        if run_sku$ = "043" then fl% = 5% else fl% = 4%
          put #fl%, using L02680, tr_str$, /* Trailer String      */~
                                " "      /* End Of Record       */
L02680:            FMT CH(20), CH(210)
          write #fl%       /* Write Trailer Record To EWDIPNSN */
        return

        header_edi
          if edi_sn% <> 1% then return
          put #2, using L02890, "#INVDATA",  /* Format Id       */~
                                " FROM ",    /* Fixed Language  */~
                                "APCBUI",    /* Origination Id  */~
                                " TO ",      /* Fixed Language  */~
                                hd_str$,     /* Header String   */~
                                " "          /* Spaces          */

           write #2          /* Write Header Record To APCEDISN */

        return

        header_ipn
        if run_sku$ = "043" then fl% = 5% else fl% = 4%
          put #fl%, using L02890, "#INVDATA",  /* Format Id       */~
                                " FROM ",    /* Fixed Language  */~
                                "APCBUI",    /* Origination Id  */~
                                " TO ",      /* Fixed Language  */~
                                 hd_str$,     /* Header String   */~
                                 " "          /* Spaces          */

L02890:            FMT CH(8), CH(6), CH(6), CH(4), CH(39), CH(167)
          write #fl%        /* Write Header Record To EWDIPNSN */

        return

        detail_edi
          if edi_sn% <> 1% then return
          put #2, using L01620, detail_send_unpk$

          write #2              /* Write APCEDISN Detail Records */
        return

        detail_ipn
          str(detail_send_unpk$,218%,4%) = "    "
        if run_sku$ = "043" then fl% = 5% else fl% = 4%
          put #fl%, using L01620, detail_send_unpk$
L01620:          FMT CH(226)
          write #fl%           /* Write EWDIPNSN Detail Records */
        return

                                                         

        reset_transmit_flag
              delete #1
              tr_dte$=date
                                          /* Reset Transmit Flag To 'T' */
              put #1, using L03030, sav_type$, part1$, edi_id$, part2$,    ~
                                  detail_send_item$, tr_dte$, old_cntl$, ~
                                  edi_cust$
L03030:       FMT CH(2), CH(4), CH(3), CH(12), CH(211), CH(6), CH(9),    ~
                  CH(9)
              write #1
        return

                                     
        open_file
        if run_sku$ = "043" then fl% = 5% else fl% = 4%
            open nodisplay #fl%, output, space = 100%,                     ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return

        print_header
          page_no% = page_no% + 1%
          print page
          print using L03330 , date$, rpt_time$, userid$, page_no%
          print
          print using L03350
          lcnt% = 5%
        return

        print_audit_report
          lcnt% = lcnt% + 7%
          if lcnt% > 55% then gosub print_header
          print using L03370
          print using L03390 , edi_duns$, edi_desc$, head_tot%
          print using L03400 , name_tot%
          print using L03410 , line_tot%
          print using L03430 , hdtr_tot%
          print
          print using L03440 , 042, rec_042_tot%
          print using L03440 , 043, rec_043_tot%
        return

        connectOracle
            init(" ") user$, pass$, server$
REM            user$   = "MSSQL"
REM            pass$   = "MSSQL"
            gosub get_user_pswd       /* (AWD003) */

            oci_err% = 0%
            call "CONNECT" (user$, pass$, server$, oci_err%)
        return

/* (AWD003) beg */
        get_user_pswd
            call "READ100" (#6, "ORACLE PASSWORD", f1%(6%))   /* SYSFILE2 */
            if f1%(6%) <> 0% then get #6 using ORCL_PSWD, user$, pass$
ORCL_PSWD:         FMT POS(21), CH(50), POS(50)

        return

/* (AWD003) END */

        getApprovalNumber
           init(" ") salesorder$, approvalnumber$, stmt1$, stmt2$
           salesorder$ = str(detail_send_unpk$,184,8)
           if salesorder$ = " " then return

 
           str(stmt1$,1,47)  = "select orderid, approvalcode from ordermaster "
           str(stmt1$,48,29) = "where salesorder = '" & salesorder$ & "'"

            gosub oracle_flush
            gosub oracle_query
            gosub oracle_fetch
             if oci_err% > 0% then return   /* Can not find sales order */

            field_num% = 2%
            gosub oracle_getfield
            approvalnumber$ = field$
            str(detail_send_unpk$,184%,20%) = approvalnumber$
        return

        oracle_query
            oci_err% = 0%
            call "QUERY" (stmt1$, stmt2$, oci_err%)
        return

        oracle_flush
            oci_err% = 0%
            call "FLUSH" (oci_err%)
        return

        oracle_exec
            oci_err% = 0%
            call "EXEC" (stmt1$, stmt2$, oci_err%)
        return     

        oracle_fetch
            oci_err% = 0%
            no_fields% = 0%
            call "FETCH" (no_fields%, oci_err%)
        return

        oracle_getfield
            oci_err% = 0%
            field_len% = 0%
            init(" ") field$, name$

            call "FIELDINF" (field_num%, field$, name$, field_len%, oci_err%)
        return

*       *****************************************************************
*        FORMAT STATEMENTS FOR AUDIT REPORT
*       ****************************************************************

L03330: %######## ######## ID: ###     APC EDI TRANSMISSION AUDIT REPORT ~
        ~      PAGE: ####
L03350: %DUNS NUMBER     CUSTOMER        ITEM TYPE              TRANSMIT ~
        ~COUNT
L03370: %----------------------------------------------------------------~
        ~-----
L03390: %############### ############### INVOICE HEADER              #####
L03400: %                                INVOICE NAME & ADDRESS      #####
L03410: %                                INVOICE LINES ITEMS         #####
L03420: %############### ############### PO ACKNOWLEDGEMENTS         #####
L03430: %                                ACCOUNT HEADERS/TRAILERS    #####
L03440: %TOTAL RECORDS                   SKU ###                    ######
L03450: %GRAND TOTAL RECORDS                                        ######

        exit_program

         call "SETPRNT" ("LOWE", "LOWE", 0%, 1%)

         close #1                                /* Close APCEDIMC File */

         close #3                                /* Close GENCODES File */
         end



