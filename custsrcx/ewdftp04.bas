        REM *-----------------------------------------------------------*~
            *                                                           *~
            * EEEEEE W    W DDDD  FFFFFF TTTTT PPPPP    0000   4   4    *~
            * E      W    W D   D F        T   P    P  0    0  4   4    *~
            * EEEE   W WW W D   D FFFFF    T   PPPPP   0    0  44444    *~
            * E      WW  WW D   D F        T   P       0    0      4    *~
            * EEEEEE W    W DDDD  F        T   P        0000       4    *~
            *                                                           *~            
            *-----------------------------------------------------------*~
            * EWDFTP04 - Create Flat File To Send To Atrium.            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/29/00 ! Original                                 ! CMG *~
            * 12/04/01 ! (EWD001) - Mod to use first part of PO   ! CMG *~
            *          ! instead of Customer for Branch           !     *~
            * 1/31/05  ! Get Totals from Files                    ! TM  *~
            * 03/19/06 ! (AWD002) mods for North east             ! CMG *~
            * 06/27/06 ! (AWD003) mods for division number        ! CMG *~
            *-----------------------------------------------------------*

        dim ewdrn_key$9,                 /* Customer File Key          */~
            arimas_key$17,               /* ARIMASTR File Key          */~
            armtrial_key$21,             /* ARMTRIAL File Key          */~
            trans_key$20,                /* ARILINES                   */~
            or_key$25,                   /* apcplnor                   */~
            f2%(64%),                    /* = 0 if the file is open    */~
            fs%(64%),                    /* = 1 Open, -1 doesn't exist */~
            rslt$(64%)20,                /* Text From File Opening     */~
            asofu$6,                     /* As of Date                 */~
            bals(3),                     /* Balances - Statutory       */~
            qty$12,                      /* Quantity                   */~
            cbals(3),                    /* balances                   */~
            bal$12,                      /* Balance                    */~
            currency$4,                  /* Currency RETURNED          */~
            readkey$24,                  /* Gencodes                   */~
            desc$30,                     /* Gencodes                   */~
            desc1$30,                    /* Gencodes                   */~
            cdate$6,                     /* ARMCBLNC - RETURNED        */~
            number$20,                   /* phone number               */~
            work_date1$10, work_date2$10,/* Temp dates                 */~
            work_date3$10, work_date5$10,/* dates                      */~
            work_date6$10, work_date4$10,/* dates                      */~
            work1$10, work2$10, work3$10,/* dates                      */~
            work8$10,                    /* dates                      */~
            work4$10, work7$10           /* dates                      */
    

        dim cuscode$9,                   /* CUSTOMER CODE              */~
            ship_to_cuscode$9,           /* Ship To Customer Code      */~
            sortname$30,                 /* SORT NAME                  */~
            sold_to$(3%)30,              /* SOLD TO & ADDRESS          */~
            cusbillc$18,                 /* CITY                       */~
            cusbills$2,                  /* STATE                      */~
            cusbillz$9,                  /* ZIPCODE                    */~
            country$3,                   /* Country                    */~
            phone_number$10,             /* PHONE NUMBER               */~
            fax_number$10,               /* Fax Number                 */~
            cus_type$2,                  /* CUSTOMER TYPE              */~
            crlimit$7,                   /* CREDIT LIMIT               */~
            slsmcode$4,                  /* SALESMAN CODE              */~
            cusbuyer$20,                 /* Contact Name               */~
            contacttitle$20,             /* contact title              */~
            lst_inv$6,                   /* Last Tran Date             */~
            lst_pymt$6,                  /* Last Payment Date          */~
            lst_change$6,                /* Last inv change date       */~
            division$4,                  /* Division                   */~
            cus_bal$12,                  /* AR balance                 */~
            rep$20,                      /* representative             */~
            add$30,                      /* address                    */~
            bill_to_xref$9,              /* parent cust                */~
            invoice$8,                   /* invoice                    */~
            set_code$12,                 /* Settlement Code            */~
            ship_to_addr$(6%)30,         /* Shp to & address           */~
            so_no$11,                    /* Order number               */~
            or_date$8,                   /* order date                 */~
            store_no$3,                  /* Warehouse                  */~
            terms$20,                    /* terms desc                 */~
            howship$12,                  /* shp via                    */~
            or_status$2,                 /* order status               */~
            or_load$5,                   /* Shipment No                */~
            disc_amt$12,                 /* Discount Amount            */~
            disc_terms$3,                /* Discount Days              */~
            salesman$4,                  /* Sales Person               */~
            fob$20,                      /* F.O.B                      */~
            terms_descr$30,              /* terms description          */~
            ship_date$6,                 /* Ship date                  */~
            inv_date$6,                  /* invoice date               */~
            post_date$6,                 /* post date                  */~
            net_terms$3,                 /* net due days               */~
            gross_invoice$12,            /* Orig Inv Amt               */~
            po_no$16,                    /* PO Number                  */~
            sales_tax_amount$10,         /* Sales Tax                  */~
            freight_amount$10,           /* freight                    */~
            other$10,                    /* other charge               */~
            invoice_type$1,              /* Trans type                 */~
            seqnr$3,                     /* Inventory Item #           */~
            partdesc$32,                 /* item desc                  */~
            part_no$25,                  /* part number                */~
            line_discount$7,             /* sales disc perc            */~
            unitpric$12,                 /* unit price                 */~
            qty_shipped$12,              /* qty shipped                */~
            extension$12,                /* total extended price       */~
            get_seqnr$7,                  /* Get Paid Seqnr Number      */~
            custotal$12,                  /* Total Customer             */~
            mastotal$12,                  /* Total Master               */~
            mastercustomer$9,             /* Customer                   */~
            trancustomer$9,               /* Tran Customer              */~
            grossinvtot$12,               /* gross total                */~
            trantotal$12,                 /* Total Tran                 */~
/*AWD003*/  plant$3                       /* plant  division number     */~


        dim schema$8,                     /* SCHEMA            (AWD002) */~
            volume$8                      /* VOLUME to create file (AWD002)*/
            
      
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! ARIMASTR ! Invoice Master File                      *~
            * #2  ! ARILINES ! Invoice Line Items File                  *~
            * #3  ! GENCODES ! GENCODES Table File                      *~
            * #4  ! CUSTOMER ! Customer Master File                     *~
            * #5  ! CCRMASTR ! Customer Credit Master File              *~
            * #6  ! APCPLNOR ! New Planning S.O. Header                 *~
            * #7  ! ARCUST   ! Customer Send Flat File                  *~
            * #8  ! ARMAST   ! Transaction Send Flat File               *~
            * #9  ! AREXTI   ! Extended Transaction Send Flat File      *~
            * #10 ! ARTRAN   ! Transaction Line Item Send Flat File     *~
            * #11 ! AREXPO   ! Export Description Send Flat File        *~           
            * #12 ! ARMTERMS ! A/R Payment Terms Codes                  *~
            * #13 ! ARMTRIAL ! Accounts Receivable Trial Balance        *~
            * #14 ! ARMTBCEX ! Multi-Currency Trial Balance Exposure    *~
            * #15 ! CRCBUF2  ! Cash Receipts Buffer-Distribution        *~
            * #16 ! SYSFILE2 ! System File                              *~
            * #17 ! ARMTBCRC ! Multi-Currency Invoice Settlement        *~
            * #18 ! ARCUSTOT ! Customer Total                           *~
            * #19 ! ARMASTOT ! Master Total                             *~
            * #20 ! ARTRANTO ! Tran Total                               *~ 
            *************************************************************~
            *              File Selection and Open Calls                *~
            *************************************************************

            select #1, "ARIMASTR",                                       ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =   1 , keylen =  17,                     ~
                        alt key    1 , keypos =  10, keylen =  8, dup,   ~
                            key    2 , keypos =  18, keylen = 16, dup,   ~
                            key    3 , keypos =  34, keylen = 16, dup,   ~
                            key    4 , keypos =1783, keylen = 26  
                        
            select #2, "ARILINES",                                       ~
                        varc,     indexed,   recsize = 750,              ~
                        keypos =   1 , keylen = 20

            select #3, "GENCODES",                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   1 , keylen =  24

            select #4, "CUSTOMER",                                       ~
                        varc, indexed, recsize = 1200,                   ~
                        keypos = 1, keylen =   9,                        ~
                        alt key  1, keypos =  10, keylen =  30, dup,     ~
                            key  2, keypos = 424, keylen =   9, dup,     ~
                            key  3, keypos = 771, keylen =   9, dup,     ~
                            key  4, keypos = 780, keylen =   9, dup


            select #5, "CCRMASTR",                                      ~
                        varc, indexed,  recsize = 200,                   ~
                        keypos = 1 , keylen = 9                         

            select #6, "APCPLNOR",                                       ~
                        varc, indexed,  recsize = 170,                   ~
                        keypos = 1, keylen = 51,                         ~
                        alt key  1, keypos = 27, keylen = 25,            ~
                            key  2, keypos = 70, keylen =  8,   dup,     ~
                            key  3, keypos = 78, keylen =  8,   dup,     ~
                            key  4, keypos = 52, keylen =  8,            ~
                            key  5, keypos = 36, keylen = 16,   dup       
                        

            select #7, "ARCUST",                                        ~
                        varc,        consec,          recsize = 448


            select #8, "ARMAST",                                        ~
                        varc,        consec,          recsize = 163

            select #9, "AREXTI",                                        ~
                        varc,        consec,          recsize = 366

            select #10, "ARTRAN",                                       ~
                        varc,        consec,          recsize = 133

            select #11, "AREXPO",                                       ~
                        varc,        consec,          recsize = 9


            select #12, "ARMTERMS",                                     ~
                        varc, indexed,  recsize = 100,                  ~
                        keypos = 1 , keylen = 20



            select #13, "ARMTRIAL",                                     ~
                        varc,     indexed,  recsize = 256,              ~
                        keypos =  1,   keylen = 21 


            select #14, "ARMTBCEX",                                     ~
                        varc,     indexed,  recsize = 100,              ~
                        keypos =    5, keylen = 21,                     ~
                        alt key  1, keypos =  1, keylen =  25

            select #16,  "SYSFILE2",                                    ~
                        varc,     indexed,  recsize = 500,              ~
                        keypos =  1,   keylen = 20 

            select #17, "ARMTBCRC",                                     ~
                        varc,     indexed,  recsize = 100,              ~
                        keypos =    5, keylen = 21,                     ~
                        alt key  1, keypos =  1, keylen =  25

            select #18, "ARCUSTOT",                                     ~
                        varc,       consec,          recsize = 126

           
            select #19, "ARMASTOT",                                     ~
                        varc,       consec,          recsize = 126

            select #20, "ARTRANTO",                                     ~
                        varc,       consec,          recsize = 126                                                  


            call "SETPRNT" ("FTPR", "FTPR", 0%, 0%)
            select printer (134)




REM         call "OPENFILE" (#7, "OUTPT", f7%(7%), rslt$(7%), axd$(7%))
REM         call "OPENFILE" (#8, "OUTPT", f8%(8%), rslt$(8%), axd$(8%))
REM         call "OPENFILE" (#9, "OUTPT", f9%(9%), rslt$(9%), axd$(9%))
REM         call "OPENFILE" (#10, "OUTPT", f10%(10%), rslt$(10%), axd$(10%))
REM         call "OPENFILE" (#11, "OUTPT", f11%(11%), rslt$(11%), axd$(11%))

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),    0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),    0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),    0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),    0%, rslt$(4%))            
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),    0%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),    0%, rslt$(6%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%),    0%, rslt$(12%))
            call "OPENCHCK" (#13, fs%(13%), f2%(13%),    0%, rslt$(13%))
            call "OPENCHCK" (#14, fs%(14%), f2%(14%),    0%, rslt$(14%))
            call "OPENCHCK" (#16, fs%(16%), f2%(16%),    0%, rslt$(16%))
            call "OPENCHCK" (#17, fs%(17%), f2%(17%),    0%, rslt$(17%))



* (AWD002) Begin
            call "SHOSTAT" ("Find Volume ...")
            schema_err%, schema% = 0%
            init(" ") schema$, volume$
            call "SCHEMA" (schema$, schema%, #3, schema_err%)


            if schema% = 1% then volume$ = "CARLO2"
            if schema% = 2% then volume$ = "NE2"

            call "SHOSTAT" ("Opening EDI Files...")
            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 1000%, rslt$(1%))

            open nodisplay #7, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = "BARCUST",          ~
                library = "FTPGETPD", volume = volume$, blocks = 5%

            open nodisplay #8, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = "BARMAST",          ~
                library = "FTPGETPD", volume = volume$, blocks = 5%

            open nodisplay #9, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = "BAREXTI",          ~
                library = "FTPGETPD", volume = volume$, blocks = 5%    

            open nodisplay #10, output, space = 100%,                   ~
                dpack   = 100%, ipack = 100%, file = "BARTRAN",          ~
                library = "FTPGETPD", volume = volume$, blocks = 5%    

            open nodisplay #11, output, space = 100%,                   ~
                dpack   = 100%, ipack = 100%, file = "BAREXPO",          ~
                library = "FTPGETPD", volume = volume$, blocks = 5%    
              
            open nodisplay #18, output, space = 100%,                   ~
                dpack  = 100%, ipack = 100%, file = "BARCUSTOT",        ~
                library = "FTPGETPD", volume = volume$, blocks = 5%

            open nodisplay #19, output, space = 100%,                   ~
                dpack = 100%, ipack = 100%, file = "BARMASTOT",         ~
                library = "FTPGETPD", volume = volume$, blocks = 5%

            open nodisplay #20, output, space = 100%,                   ~
                dpack = 100%, ipack = 100%, file = "BARTRANTOT",         ~
                library = "FTPGETPD", volume = volume$, blocks = 5%

* (AWD002) END
            
        REM *************************************************************~
            *       M A I N  P R O C E S S  S E C T I O N                ~
            *************************************************************~

        call "SHOSTAT" ("Creating EDI Transmit File...")

        gosub initialize_data        /* Initialize Data Variable */
        gosub build_customer_recs    /* Write customer Recs To ARCUST File */
REM        gosub build_master_recs      /* Write master recs  to ARMAST  File */
        gosub build_master_armtrial   /* Write master recs  to ARMAST  File */
        gosub build_expo_rec         /* write date/time stamp */
        goto  exit_program           /* Exit Program */

        initialize_data
            init (" ") ewdrn_key$, arimas_key$, trans_key$, cuscode$,         ~
                       sortname$, sold_to$(), cusbillc$, cusbills$,           ~
                       cusbillz$, country$, phone_number$, fax_number$,       ~
                       cus_type$, crlimit$, slsmcode$, cusbuyer$,             ~
                       contacttitle$, lst_inv$, lst_pymt$, lst_change$,       ~
                       division$, cus_bal$, rep$, add$, bill_to_xref$,        ~
                       part_no$, invoice$, ship_to_addr$(), so_no$, or_date$, ~
                       store_no$, other$, terms$, howship$, or_status$,       ~
                       or_load$, disc_amt$, terms_descr$, disc_terms$,        ~
                       salesman$, fob$, ship_date$, inv_date$, post_date$, bal$,~
                       net_terms$, gross_invoice$, po_no$, sales_tax_amount$, ~
                       freight_amount$, invoice_type$, seqnr$, partdesc$,     ~
                       line_discount$, unitpric$, qty_shipped$, extension$,    ~
                       custotal$, mastotal$, trantotal$, mastercustomer$,      ~
                       trancustomer$, grossinvtot$     
                             
            sav_bill_to_xref$ = " "
            total_amt = 0.00
       REM  trans_date$ = date                                              /* Y2K */
            work_date1$ = date
            work_date2$ = date                                              /* Y2K */
            work_date3$ = date
            work_date4$ = date                                              /* Y2K */
            work_date5$ = date
            work_date6$ = date                                              /* Y2K */
            work1$      = date                                              /* Y2K */
            work3$      = date                                              /* Y2K */
            work7$      = date                                              /* Y2k */
            work2$      = date 
            work4$      = date
            work8$      = date
REM            call "DATFMTC" (work_date1$, wd%, trans_date$)                  /* Y2K */
REM            call "DATFMTC" (work_date3$, wd%, trans_date$)                  /* Y2K */
REM            call "DATFMTC" (work_date5$, wd%, trans_date$)                  /* Y2K */
REM            call "DATFMTC" (work1$, wd%, trans_date$)                       /* Y2K */
REM            call "DATFMTC" (work3$, wd%, trans_date$)                       /* Y2K */
            trans_date$ = str(trans_date$, 3, 6)                            /* Y2K */

            date$ = date
            call "DATEFMT" (date$)
            rpt_time$ = time
            call "TIME"(rpt_time$)

/*(AWD003) - begin */            
            init(" ") plant$
            plant$ = "036"
            if schema% = 2% then plant$ = "080"
/*(AWD003) - end   */            
        return


        build_customer_recs
                            /* Start File At First Invoice Send Record */
          
              custotal = 0.0
              init(" ") ewdrn_key$
REM              str(ewdrn_key$, 1%, 6%) = "840084"
           
              read #4, key > ewdrn_key$,using  L01370, cuscode$,          ~
                              sold_to$(), cusbillc$, cusbills$, cusbillz$,    ~
                              country$, phone_number$, fax_number$, cus_type$,~
                              crlimit, slsmcode$, cusbuyer$, division$,       ~
                              bill_to_xref$,  eod goto L01480
           
                                  
L01370:            FMT CH(09), POS(40), 3*CH(30), POS(190), CH(18), POS(208), ~
                       CH(02), POS(211), CH(09), POS(1073), CH(03),           ~
                       POS(453), CH(10), POS(228), CH(10), POS(1023), CH(02), ~ 
                       POS(526), PD(15,4), POS(714), CH(04), POS(433), CH(20),~
                       POS(1069), CH(04), POS(780), CH(09) 
                   goto L01450

        read_next_cus
              custotal = 0.0
              read #4,using  L01370, cuscode$,       ~
                              sold_to$(), cusbillc$, cusbills$, cusbillz$,    ~
                              country$, phone_number$, fax_number$, cus_type$,~
                              crlimit, slsmcode$, cusbuyer$, division$,       ~
                              bill_to_xref$,  eod goto L01480
           
L01450:          str(ewdrn_key$,1%,9%)   = cuscode$
                 gosub custype

                 gosub dates
              
              
                 gosub unpack_dates
                 init(" ") number$
                 if phone_number$ <> " "  then                                ~
                    number$ = str(phone_number$,1%,3%) & "/" &                ~
                    str(phone_number$,4%,3%) & "-" & str(phone_number$,7%,12%)
                 custotal = cus_bal + custotal
                 convert crlimit to crlimit$, pic(0000000)	
                 convert cus_bal to cus_bal$, pic(-00000000.00)
                 convert custotal to custotal$, pic(-00000000.00)         /*Tina*/
                                             /*Tina*/
                 if cus_bal = 0.00 then goto read_next_cus
              

                                                             /* (AWD003) */
                 put #7, using L01455, str(ewdrn_key$, 1%, 6%) & "." & plant$,~
                        sold_to$(1%), sold_to$(2%), sold_to$(3%), cusbillc$,  ~
                        cusbills$, cusbillz$, country$,number$, " ", " ", " ",~
                        crlimit$, slsmcode$, cusbuyer$, " ", work_date4$,     ~
/*(AWD003)*/            work_date2$, plant$, cus_bal$,                        ~
                        " ", " ", bill_to_xref$, work_date6$ 

                  write #7              /* Write EWDFTPSN Detail Records */
                  put #18, using L01453, str(ewdrn_key$, 1%, 6%), custotal$               /*Tina */

L01453:            FMT CH(20), CH(12)                                    /*Tina*/
                                   
                                
                              
L01455:            FMT CH(20), CH(35), CH(30), CH(30), CH(20), CH(10), CH(10), CH(15), CH(20),    ~
                       CH(20), CH(10), CH(65), CH(07), CH(10), CH(20), CH(20), CH(08), CH(08),  ~
                       CH(10), CH(12), CH(20), CH(30), CH(10), CH(08)          
                     
                      
                   
                   write #18             /* Write EWDFTPSN Total          */  /*Tina*/
                  
              
                   goto read_next_cus            /* Get Next EWDFTPMR Record */

L01480: return

                 qty = qty + cus_bal
                   convert qty to qty$, pic(000000000.00)
                   put #7, using L01481, qty$
L01481:            FMT CH(12)
                   write # 7 
       


        unpack_dates    /* Unpack dates before they're sent.        Y2K */
        
            
              work_date1$ = str(lst_inv$)                   /* Invoice Date     */
              work_date3$ = str(lst_pymt$)
              work_date5$ = str(lst_change$)
                
              gosub unpack_date

              return


        unpack_date         /* in - work_date1$ - PD(11,1)          Y2K */
                            /* out- work_date1$ - formatted             */
                            /*      work_date2$ - YYDDMM                */
                            /* convert work_date2$ - MMDDYY             */                            
            
              call "DATEFMT" (work_date2$)
REM            work_date2$ = str(work_date2$, 3, 6)                  
REM            work_date2$ = str(work_date2$,3%,2%) & str(work_date2$,5%,2%) & ~
REM                          str(work_date2$,1%,2%)


              call "DATEFMT" (work_date4$)
REM            work_date4$ = str(work_date4$, 3, 6)                  
REM            work_date4$ = str(work_date4$,3%,2%) & str(work_date4$,5%,2%) & ~
REM                          str(work_date4$,1%,2%)



              call "DATEFMT" (work_date6$)
REM            work_date6$ = str(work_date6$, 3, 6)                  
REM            work_date6$ = str(work_date6$,3%,2%) & str(work_date6$,5%,2%) & ~
REM                          str(work_date6$,1%,2%)

 
                          
               return

        custype
             
              init(" ") readkey$, desc$
              str(readkey$,1%,9%)    = "CUS TYPES"
              str(readkey$,10%,15%)  = cus_type$
              read #3, key = readkey$, using L01482, desc$, eod goto L01483

L01482:       FMT POS(25), CH(30)
              return

L01483:       REM edi% = 0%

        return

        
        dates
       
               read #5, key = cuscode$, using L01485, lst_inv$, lst_pymt$,    ~
                                        lst_change$, cus_bal, eod goto L01490
  

L01485:        FMT   POS(90), 3*CH(6), POS(130), PD(15,4)
  

REM edi% = 1%

L01490: return

REM       edi% = 0%


               
REM L01493         fax_number$ = " "
REM               return

        build_master_armtrial
          call "SHOSTAT" ("BUILDING ARMTRIAL")
           mastotal, grossinvtot = 0.0
           trantotal = 0.0
          init(" ") armtrial_key$
          
REM          str(armtrial_key$, 1%, 9%) = "HO0375"
REM          str(armtrial_key$,10%, 8%) = "01814601"    

        build_armtrial_next
         
          init(" ") part_no$, partdesc$,cuscode$, po_no$, so_no$,            ~
                    work7$, net_terms$, gross_invoice$, sales_tax_amount$,   ~
                    freight_amount$, line_discount$, unitpric$, qty_shipped$,~
                    extension$, other$, work8$
          
          read #13, key > armtrial_key$, using L02000, bill_to_xref$,        ~
                                         set_code$, post_date$,              ~
                                         ship_to_cuscode$, eod goto L02010

L02000:   FMT CH(9), CH(12), POS(97), CH(6), POS(109), CH(9)
       
               str(armtrial_key$,1%,9%)   = bill_to_xref$
               str(armtrial_key$,10%,12%) = set_code$
REM               str(armtrial_key$,1%,9%)   = "HO0375"
REM               str(armtrial_key$,10%,8%) = "01814601"
               

               if str(set_code$,11%,2%) <> "00" then goto build_armtrial_next


               gosub balances
               convert bals(3%) to bal$, pic(-00000000.00)
               
               if bals(3%) = 0.00 then goto build_armtrial_next
               work8$ = str(post_date$)
                  call "DATEFMT" (work8$)
               if str(set_code$,1%,1%) <> "U" then goto L02020
                  total_amt = total_amt + bals(3%)
                  invoice$ = str(set_code$,1%,8%)
REM                  cuscode$ = str(ship_to_cuscode$,1%,9%)
                  
                  gosub write_master
                  gosub write_trans
                  goto build_armtrial_next                  

L02020:         gosub build_master_recs

               goto build_armtrial_next
L02010: return



        build_master_recs
          
          init(" ") arimas_key$, invoice$, work7$, net_terms$, gross_invoice$,~
                    po_no$, sales_tax_amount$, freight_amount$,~
                    other$, invoice_type$

          str(arimas_key$, 1%, 9%) = str(ship_to_cuscode$, 1%,9%)
          str(arimas_key$,10%, 8%) = str(set_code$,1%,8%)
REM             str(arimas_key$, 1%, 9%) = "HO0375"
REM             str(arimas_key$,10%, 8%) = "01814601"

          read #1, key = arimas_key$, using L01500, cuscode$, invoice$,  ~
                   po_no$, so_no$, ship_to_addr$(), ship_date$, howship$,      ~
                   fob$, salesman$, inv_date$, gross_invoice,  disc_amt,       ~
                   freight_amount, sales_tax_amount, store_no$, invoice_type$, ~
                   terms$, bill_to_xref$,                                      ~        
                eod goto L01555

L01500:   FMT CH(9), CH(8), CH(16), CH(16), POS(53), 6*CH(30), POS(413), CH(6),~ 
              CH(20), CH(20), POS(501), CH(4), POS(521), CH(6), POS(793),      ~
              PD(15,4), POS(809), PD(15,4), PD(15,4), PD(15,4), POS(870),      ~
              CH(3), POS(891), CH(1), POS(908), CH(20), POS(1783), CH(9)    


          str(arimas_key$, 1%, 9%) = cuscode$
          str(arimas_key$,10%, 8%) = invoice$
             
REM          if str(bill_to_xref$,1%,9%) <> str(sav_bill_to_xref$,1%,9%)      ~
REM                    then gosub write_extra
              
          total_amt = total_amt + bals(3%)

          
          if invoice_type$ = "O" then  invoice_type$ = "I"
          if invoice_type$ = "C" then  invoice_type$ = "C"
          if invoice_type$ = "A" and gross_invoice >= 0 then invoice_type$ = "D"
          if invoice_type$ = "A" and gross_invoice < 0 then invoice_type$ = "C"
             

          gosub terms2
              
          work7$ = str(inv_date$)
          
          call "DATEFMT" (work7$)
          convert gross_invoice to gross_invoice$,  pic(000000000.00)
          
          convert sales_tax_amount to sales_tax_amount$, pic(0000000.00)
          convert freight_amount to freight_amount$, pic(0000000.00)
          convert other to other$, pic(0000000.00)


        


L01555: write_master

            if str(set_code$,1%,1%) = "U" then  work7$ = work8$
            if str(work7$) = " " then work7$ = work8$
            put #8, using L01520, str(set_code$,1%,8%),                    ~
                   bill_to_xref$ & "." & plant$,                           ~
                   work7$, bal$, net_terms$, gross_invoice$, " ", po_no$,  ~
                   " ",  sales_tax_amount$, freight_amount$, other$,       ~
                   invoice_type$

            write #8
            if mastercustomer$ = " " then mastercustomer$ = bill_to_xref$
                    if mastercustomer$ = bill_to_xref$ then goto L01530
          convert mastotal to mastotal$, pic(-00000000.00)   /*tina*/    
          convert grossinvtot to grossinvtot$, pic(-00000000.00)   /*tina*/                   

            put #19, using L01556, mastercustomer$, mastotal$, grossinvtot$ /*Tina*/

L01556:     FMT CH(9), CH(12), CH(12)                                     /*Tina*/

L01520:     FMT CH(12), CH(20), CH(8), CH(12), CH(3), CH(12), CH(30),  ~
                CH(25), CH(10), CH(10),CH(10), CH(10), CH(1)

            
            write #19                    /*Tina*/
            mastercustomer$ = bill_to_xref$
             mastotal,grossinvtot = 0.0
L01530:

          mastotal = bals(3%) + mastotal         /*Tina*/

          if str(invoice$,1%,1%) <> "U" then     ~
              grossinvtot = gross_invoice + grossinvtot


            if str(set_code$,1%,1%) = "U" then return

            gosub build_extend_recs
                   
        return

        write_extra
            call "SHOSTAT" ("WRITING EXTRA" ) 
            convert total_amt to total_amt$,  pic(-00000000.00)

                                                 /* (AWD003) */
            put #8, using L01520, " ", sav_bill_to_xref$ & "." & plant$,  ~
                   " ", total_amt$, " ", " ", " ", " ", " ",              ~
                   sales_tax_amount$, freight_amount$, other$,            ~
                   invoice_type$
            put #19, using L01556, mastotal$               /*Tina*/

            write #8   
            write #19                                     /*Tina*/


            str(sav_bill_to_xref$,1%,9%) = str(bill_to_xref$,1%,9%)
            total_amt = 0.00
        return


     build_extend_recs
            init (" ") or_key$
            str(or_key$, 1%, 9%) = cuscode$
            str(or_key$, 10%, 16%) = po_no$ 
            read #6, key 1 = or_key$, using L01600, or_status$, or_load$, ~
                                                 or_date$, eod goto L01752

L01600:    FMT POS(60), CH(02), POS(94), CH(05), POS(127), CH(06)
       

            gosub status
            gosub unpackdates1
       
            if terms_descr$ = "CASH                         " then       ~
                                                       disc_terms$ = " 00"
            if terms_descr$ = "NET 30 DAYS                  " then       ~
                                                       disc_terms$ = " 00"
            if terms_descr$ = "NET 60 DAYS                  " then       ~
                                                       disc_terms$ = " 00"
            if terms_descr$ = "NET 10TH PROX                " then      ~
                                                       disc_terms$ = " 00"
            if terms_descr$ = "NET 45 DAYS                  " then      ~
                                                       disc_terms$ = " 00"
            if terms_descr$ = "PREPAID                      " then      ~
                                                      disc_terms$ = " 00"
              
            convert disc_amt to disc_amt$, pic(000000000.00)
         
                                                  /* (AWD003) */
            put #9, using L01751, bill_to_xref$ & "." & plant$,          ~
                      str(arimas_key$,10%,8%), ship_to_addr$(1%),        ~
                      ship_to_addr$(2%), ship_to_addr$(3%),              ~     
                      ship_to_addr$(6%), " ", " ",  so_no$, work2$,      ~ 
                      store_no$, terms_descr$, howship$, desc1$,         ~
                      or_load$, disc_amt$, disc_terms$, salesman$,       ~
                      fob$, work4$


L01751:     FMT   CH(20), CH(12), CH(35), CH(30), CH(30), CH(30), CH(30),  ~
                  CH(30), CH(11), CH(8), CH(15), CH(20), CH(12), CH(15),   ~
                  CH(5), CH(12), CH(3), CH(10), CH(30), CH(8)

            write #9

            
L01752: 
               gosub build_trans_recs
        return

        status

            init(" ") readkey$, desc1$
            str(readkey$,1%,9%)    = "PLAN STAT"
            str(readkey$,10%,15%)  =  or_status$
            read #3, key = readkey$, using L01760, desc1$, eod goto L01765

L01760:     FMT POS(25), CH(30)

L01765: return

REM     edi% = 0%
REM       return    

        terms2
 
           read #12, key = terms$, using L01820, terms_descr$, disc_terms$,   ~
                                                 net_terms$, eod goto L01830

L01820:    FMT POS(21), CH(30), POS(59), CH(8), POS(67), CH(8)

REM           edi% = 1
L01830: return
REM    edi% = 0%

          

REM     dates1
           

        terms


           read #12, key = terms$, using L01860, disc_terms$, eod goto L01862

L01860:    FMT POS(59), CH(8)

REM           edi% = 1%

L01862: return

REM    edi% = 0%



        balances
           
           asofu$  = date
           
           call "ARMCBLNC" (str(armtrial_key$,1%,9%),str(armtrial_key$,10%,12%),~
                            asofu$, 10%, "N", #13, #4, bals(), #14, #17, #16,   ~
                            currency$, cdate$, coneqv,conunt, cbals())
            
        return                                                                                     



        

        unpackdates1
            
              work2$ = str(or_date$)
              work4$ = str(ship_date$)
              gosub unpackdates2

        return


        unpackdates2        /* in - Work1$ - PD(15,1)          Y2K */
                            /* out- work1$ - formatted             */
                            /*      work2$ - YYYYMMDD              */
                            /* convert work2$ - MMDDYYYY           */    

            
             call "DATEFMT" (work2$)

             call "DATEFMT" (work4$)

        return     
 
        build_trans_recs

             init(" ") trans_key$, part_no$, partdesc$
          
             str(trans_key$,1%, 9%) = str(ship_to_cuscode$, 1%,9%)
REM             str(trans_key$,1%, 9%) = str(bill_to_xref$, 1%,9%)
             str(trans_key$,10%, 8%) = str(set_code$,1%,8%)
REM                str(trans_key$,1%, 9%) = "HO0375"
REM                str(trans_key$,10%,8%) = "01814601"
             read #2, key > trans_key$, using L01870, cuscode$,               ~
                                 invoice$, seqnr$, part_no$, partdesc$,       ~ 
                                 qty_shipped, unitpric, line_discount,        ~
                                 extension, eod goto L01890

L01870:      FMT CH(9), CH(8), CH(3), POS(24), CH(25), POS(49), CH(32),       ~
                 POS(93), PD(15,4), POS(133), PD(15,4),POS(141), CH(7),       ~
                 POS(157), PD(14,4) 
             
             goto L01874

        read_next_trans
             init(" ") part_no$, partdesc$, get_seqnr$
             get_seqnr% = 0%
             read #2, using L01870, cuscode$, invoice$, seqnr$, part_no$,     ~
                                 partdesc$, qty_shipped, unitpric,            ~
                                 line_discount, extension, eod goto L01890

                   
           
L01874:         str(trans_key$, 1%, 9%) = cuscode$
                str(trans_key$, 10%, 8%) = invoice$
                str(trans_key$, 18%, 3%) = seqnr$
 
REM                if str(arimas_key$,1%,17%) <> str(trans_key$,1%,17%)       ~
                                             then goto read_next_cus2

                if str(arimas_key$,1%,17%) <> str(trans_key$,1%,17%) then return
                                      /*Tina*/
                convert line_discount to line_discount$, pic(000.000)
                convert unitpric to unitpric$, pic(000000000.00)
                convert qty_shipped to qty_shipped$, pic(00000000.000)
                convert extension to extension$, pic(000000000.00)
               
                
REM ** CMG **
                convert seqnr$ to get_seqnr%, data goto L01878

L01878:

                convert get_seqnr% to get_seqnr$, pic(0000000)
L01877:      write_trans
                    
                    put #10, using L01880, invoice$, " ", partdesc$,      ~
                           line_discount$, unitpric$, qty_shipped$,       ~
                           extension$, bill_to_xref$ & "." & plant$,      ~
                            get_seqnr$                 /* (AWD003)  */

                    write #10
                    

                    if trancustomer$ = " " then trancustomer$ = bill_to_xref$
                    if trancustomer$ = bill_to_xref$ then goto L01891
                    convert trantotal to trantotal$, pic(-00000000.00)        /*Tina*/
                   
                    put #20, using L01886, trancustomer$, trantotal$      /*tina*/

L01886:             FMT CH(9), CH(12)                                     /*tina*/

L01880:             FMT CH(12), CH(16), CH(35), CH(7), CH(12), CH(12), CH(12), CH(20), CH(7)

        
                    
                    write #20                                   /*Tina*/
                    trancustomer$ = bill_to_xref$
                    trantotal = 0.0
                                 /*Tina*/

L01891:
                     
                      if str(invoice$, 1%, 1%) <> "U" then ~
                      trantotal = extension + trantotal


                    put #10, using L01885, invoice$, "WIDECOMMENT", " ",       ~
                           "PART#:", " ", part_no$,                            ~
                           bill_to_xref$ & "." & plant$, " "
                                                 /* (AWD003) */

L01885:             FMT CH(12), CH(11), CH(5), CH(5), CH(2), CH(71), CH(20), CH(7)

                    write #10
 
            
            if str(set_code$,1%,1%) = "U" then return
            goto read_next_trans

L01890: return


        build_expo_rec

                date$ = date
                time$ = time
                call "DATEFMT" (date$)
                call "TIME" (time$)

                  put #11, using L01895, date$, time$

L01895:           FMT CH(8), CH(5) 
    
                  write #11

        return
   
        exit_program
         
             close #7                                /* Close EWDFTPMR File */
             close #8                                /* Close EWDEDISR File */
             close #9                                /* Close GENCODES File */
             close #10
             close #11
        end


