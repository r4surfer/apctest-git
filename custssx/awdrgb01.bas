        REM *************************************************************~
            *                  (MFGSHIP - Turned On)                    *~
            *  Subroutine Name   - AWDRGB01                             *~
            *  Creation Date     - 04/04/05                             *~
            *  Last Modified Date- 01/01/06                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Modifications By  - Royal H. Hoffman                     *~ 
            *                                                           *~
            *  Description       - Write records to a file to print a   *~
            *                      AES Barcode Label.                   *~
            *                                                           *~
            *                      Print File  = MFGRGB                 *~
            *                      Script File = MFGRGB                 *~
            *                                                           *~
            *                      Program supports both old and new    *~
            *                      Zebra label format.                  *~
            *                      'load_label' - for 220 XI3 Zebra     *~
            *                      'new_label'  - for 3844 Zebra        *~
            *                       Comment Out 'goto new_label' to     *~
            *                       Switch formats.                     *~
            *                                                           *~    
            *-----------------------------------------------------------*~
            * AWDRGB01 - Generates the label format and data to print   *~
            *            AES Barcode labels. The resulting file is      *~
            *            routed to the label printer via a script.      *~
            *                                                           *~
            *          - ERROR% = 0% - Lbl Ok and Sent                  *~
            *                     1% - Could Not Open Lbl Print File    *~
            *                     4% - Shell Script Error               *~
            *                     5% - Print Error Creating Label       *~
            *                     6% - Could Not Reset Table Flag       *~
            *                     7% - Label Data Format Error          *~
            *                     8% - No Data for Label                *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/04/05 ! Original - New RGA Label Program.        ! RHH *~
            * 05/18/05 ! (AWD001) Change the print size of the    ! RHH *~
            *          !    Serial Number under the barcode.      !     *~
            * 01/01/06 ! (PAR000) - CR347 Mod for new Sub Part No.! RHH *~
            *          !            Also Change for new RGA Printer!    *~
            *10/16/2007! (AWD002) Remove the Sales Order number   ! DES *~
            *02/25/2019! CR1941 Change script so not scanner script!RDB *~
            *************************************************************

        sub "AWDRGB01" (been_here%,      /* Zero (Only 1st Time)       */~
                        rga_rec$(),      /* RGA Database File          */~
                        #1,              /* (GENCODES)                 */~
                        error%)          /* Return Code                */

        dim                                                              ~
            a$90, b$90,                  /* Print Lines for Label      */~
            lbl$(40%)60,                 /* Label Data Array           */~
            readkey$24, descr$30,        /* Use for Gencodes           */~ 
            hdr$40,                      /* ASKUSER Header Text        */~
            msg$(3%)79,                  /* ASKUSER Info Text          */~
            fs$3,                        /* End of Lbl Records         */~
            file$8,                      /* Lbl Print File             */~
            library$8,                   /* Library Name = APCDATA     */~
            script$8,                    /* Lbl SHELL SCRIPT           */~
            volume$6                     /* DISK VOLUME = CARLOS       */

        dim rga_rec$(2%)256,             /* Purchase Order Label Data  */~
            yy$(90%)90,                  /* Buffer                     */~ 
            xx$(90%)90,                  /* Buffer                     */~
            lb_serial$8,                 /* RGA Serail Number          */~
            lb_serial_txt$8,             /* RGA Serial Number Text     */~
            lb_warranty_id$8,            /* Production Warranty Id     */~
            lb_production_barcode$18,    /* Production Barcode         */~
            lb_complaint_no$8,           /* Complaint Number           */~
            lb_rga_no$4,                 /* RGA Number                 */~
            lb_sales_order$8,            /* Sales Order Number         */~
            lb_rga_stat_desc$34,         /* RGA Status Code Description*/~
            lb_rga_reas_desc$34,         /* RGA Reson Code Description */~
            lb_part$25,                  /* MFG Part Number            */~
            lb_sub_part$20,              /* Sub Part Number    (PAR000)*/~     
            lb_part_desc$30,             /* MFG Part Number Description*/~
            lb_rga_entry_dte$10,         /* RGA Entry Date into System */~
            lb_orig_dte$10,              /* Original Sales Order Date  */~
            lb_rga_comment$32,           /* Additional Comment         */~
            lb_trailer_loc$6,            /* Trailer / Location         */~
            lb_comp$8                    /* Use to unpack              */ 


        dim tmp_sales_order$8            /* Sales Order Number         */
        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~ 
            axd$4,                       /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21                        
            apc$   = "(AWD) Generate RGA Barcode Labels       "
            pname$ = "AWDRGB01 - 01/01/2006"

        REM *************************************************************
            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #5  ! MFGRGA   ! Print File For RGA Barcode Labels        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #5, "ROYAL", varc, consec, recsize =   90


                                             
                                                              
            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            sel%       = error%
            error%     = 0%  
            nbr_lines% = 0%
            fs$ = "^FS"


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
            init(" ") xx$()
            if been_here% > 0% then goto L01000
               gosub load_label           /* Build Label Format yy$() */ 
               gosub set_file_name        /* Create Label Print File  */
               gosub open_file            /* Open Label Print file    */

L01000:        if sel% = 99% then goto exit_print
                  been_here% = been_here% + 1%
                  copy yy$() to xx$()
                  gosub begin_process

                  goto exit_sub

        set_file_name
            init(" ") file$, script$
            file$   = "MFGRGB"       /* CR1941 */
            script$ = "MFGRGB"       /* CR1941 */
      
        REM    file$   = "MFGSHIP"  /* For Now Use Shipping Printer 'B' */ 
        REM    script$ = "MFGSHIP" 

        REM    file$   = "MFGTEST"
        REM    script$ = "MFGTEST"
        return

        open_file
            library$        = "APCDATA "
            volume$         = "CARLOS"
            call "OPENFILE" (#5, "IO   ", f2%(5%), rslt$(5%), axd$ )
            if f2%(5%) <> 0% then goto L01100
               gosub file_exists         
               if comp% <> 16% then goto exit_sub
                  call "FILEBGON" (#5)

L01100:    open nodisplay #5, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return


        REM *************************************************************~
            *                P R O C E S S   D A T A                    *~
            *************************************************************
        begin_process  
                                          /* Load data from (AESPRDLB) */
            init (" ") lbl$(), lb_serial$, lb_serial_txt$, lb_warranty_id$,  ~
                       lb_production_barcode$, lb_complaint_no$, lb_rga_no$, ~
                       lb_sales_order$, lb_rga_stat_desc$, lb_rga_reas_desc$,~
                       lb_part$, lb_part_desc$, lb_rga_entry_dte$,           ~
                       lb_orig_dte$, lb_rga_comment$, lb_trailer_loc$,       ~
                       lb_comp$, lb_sub_part$                
                                                          /* (PAR000) */     

                                          /* RGA Serial Number        */       
            lb_serial$ = str(rga_rec$(),10%,8%) 
            lbl$(01%) = lb_serial$           & fs$
                                          /* RGA Serial Number Text   */       
            lb_serial_txt$ = str(rga_rec$(),10%,8%) 
            lbl$(02%) = lb_serial$           & fs$
                                          /* Production Warranty Id   */
            lb_warranty_id$ = str(rga_rec$(),18%,8%)
            lbl$(03%) = lb_warranty_id$      & fs$
                                          /* Production Barcode       */
            lb_production_barcode$ = str(rga_rec$(),26%,18%)
            lbl$(04%) = lb_production_barcode$ & fs$
                                          /* Complaint Number         */
            str(lb_comp$,1%,4%) = str(rga_rec$(),44%,5%)
            gosub unpack_complaint
            lbl$(05%) = lb_complaint_no$       & fs$
                                          /* RGA Number Assigned      */
            lb_rga_no$ = str(rga_rec$(),49%,4%)
            lb_rga_no_ln$= str(rga_rec$(),53%,2%)
            lbl$(06%) = lb_rga_no$ & lb_rga_no_ln$ & fs$
                                          /* Sales Order Number       */
            lb_sales_order$ = str(rga_rec$(),55%,8%)
/* <AWD002> */
REM         lbl$(07%) = lb_sales_order$        & fs$
REM         lbl$(07%) = fs$
	    tmp_sales_order$ = lb_sales_order$
	    x% = 0%
	    for y% = 1% to 8%
	       if str(tmp_sales_order$,y%,1%) > " " then x% = x% + 1%
	    next y%
	    for y% = 1% to (x% - 4%)
		str(tmp_sales_order$,y%,1%) = "X"
	    next y%
            lbl$(07%) = tmp_sales_order$        & fs$

/* </AWD002> */
                                          /* RGA Stat Description     */
            gosub lookup_rga_status
            lbl$(08%) = lb_rga_stat_desc$     & fs$
                                          /* RGA Reason Description   */
            gosub lookup_rga_reason
            lbl$(09%) = lb_rga_reas_desc$      & fs$
                                          /* MFG Part Number          */
                                          /* (PAR000)                 */ 
            lb_part$     = str(rga_rec$(),66%,25%)
            lb_sub_part$ = str(rga_rec$(),266%,20%)
            lbl$(10%) = lb_part$               & fs$
                                          /* MFG Part Description     */
            lb_part_desc$ = str(rga_rec$(),91%,30%)
            lbl$(11%) = lb_part_desc$          & fs$
                                          /* RGA Entry Date           */
            str(lb_rga_entry_dte$,1%,6%) = str(rga_rec$(),1%,6%)
            call "DATFMTC" (lb_rga_entry_dte$)
            lbl$(12%) = lb_rga_entry_dte$      & fs$
                                          /* Original Sales Order Date*/
            str(lb_orig_dte$,1%,6%) = str(rga_rec$(),233%,6%)
            call "DATFMTC" (lb_orig_dte$)
            lbl$(13%) = lb_orig_dte$           & fs$
                                          /* RGA Additional Comment   */
            lb_rga_comment$ = str(rga_rec$(),140%,32%)
            lbl$(14%) = lb_rga_comment$        & fs$
                                          /* RGA Trailer-Loc code     */
            lb_trailer_loc$ = str(rga_rec$(),131%,6%)
            lbl$(15%) = lb_trailer_loc$        & fs$

         

    read_loop
        init(" ") a$
        b$ = all(hex(00))
        nbr_lines% = nbr_lines% + 1%
        a$ = xx$(nbr_lines%)
        if a$ = " " then end_process
        a_len% = len(a$)                   /* Calc Length of Data String */
        str(b$,1%,a_len%) = str(a$,1%,a_len%) /* Put into b$ Data from a$ */
        convert str(a$,1%,2%) to ln%, data goto skip_data
                                           /* Look for a Field Number    */ 
        l_len% = len(lbl$(ln%))            /* Find Length of Data Element*/
                                           /* in the Label data array    */
        b_len% = (a_len% - 2%) + l_len%    /* Adjust for 2 digit field No*/

        b$ = all(hex(00))                  /* Initialize Print string    */
                                           /* 1st set Font data for print*/
                                           /* 2nd append Actual Data that*/
                                           /*     will be printed        */    
        str(b$,1%,b_len%) = str(a$,3%,a_len%-2%)                           ~
            & str(lbl$(ln%),1%,l_len%)
      skip_data
                                           /* (AWD002)                  */
        if nbr_lines% = 1% and been_here% = 1% then                        ~
                               b$ = hex(7e) & str(a$,2%,a_len%)

        if nbr_lines% = 1% and been_here% > 1% then                        ~
                               goto read_loop

  
        gosub print_line
        if a$ = "^XZ" then end_process       /* Last Line */

        REM if nbr_lines% = 55% then end_process /* Last Line */ 
        goto read_loop

    end_process
        if nbr_lines% = 0% then error% = 8%
        return


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                                       /* (EWD001)     */     

        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************
L55030:     FMT CH(90)

        REM *************************************************************~
            *           S P E C I A L   R O U T I N E S                 *~
            *************************************************************

        print_line
            write #5, using L55030, b$, eod goto L61550
        return

L61550:     error% = 5%
        return clear all


        unpack_complaint
            lb_comp% = 0%
            get str(lb_comp$,1%,4%), using PACK_2, lb_comp%
PACK_2:             FMT BI(4)
            convert lb_comp% to lb_complaint_no$, pic(00000000)

        return 

        lookup_rga_status                          /* RGA Status Code      */
            init(" ") readkey$, descr$
            str(readkey$,1%,9%)  = "RGASTATUS"
            str(readkey$,10%,3%) = str(rga_rec$(),7%,3%) 
            read #1,key = readkey$, using L61600, descr$, eod goto L61610
L61600:        FMT POS(25), CH(30)
L61610:     lb_rga_stat_desc$ = str(rga_rec$(),7%,3%) & " " & descr$
        return

        lookup_rga_reason                          /* RGA reason Code      */
            init(" ") readkey$, descr$
            str(readkey$,1%,9%)  = "RGAREASON"
            str(readkey$,10%,3%) = str(rga_rec$(),137%,3%) 
            read #1,key = readkey$, using L61600, descr$, eod goto L61620
L61620:     lb_rga_reas_desc$ = str(rga_rec$(),137%,3%) & " " & descr$
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************
        exit_sub


        end

        exit_print
            lb1% = 0% : lb2% = 0%

            close #5

            call "LINK" addr(script$, lb1%, lb2%)
            if lb1%    > 0% then error% = 4%

            call "FILEBGON" (#5)          /* Scratch 'MFGAES'         */
        end

        file_exists
          comp% = 2%     
          hdr$ = "***  New AES Barcode Label ***"
          msg$(1%) = "       The File (MFGRGB) Already Exists.         "
          msg$(2%) = "        RGA B A R C O D E   L a b e l s          "
          msg$(3%) = "Press <RETURN> To Exit Prog, or PF(16) to Delete."
          call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                    
        load_label

           init(" ") yy$() 

           yy$( 1%) = "^JO"                          /* This format is used*/
                                                     /* for the planning   */
                                                     /* Zebra Printers     */
                                                     /*  220 XI3 Printer.  */ 
           yy$( 2%) = "^XA^EG^XZ"
 
           yy$( 3%) = "^XA"
           yy$( 4%) = "^PMN"
           yy$( 5%) = "^MNY"
           yy$( 6%) = "^MMT"                          /* Back Feed Off */
                                                      /* R=Off, T=On   */                   
           yy$( 7%) = "^MTT"
           yy$( 8%) = "^MD0"
           yy$( 9%) = "^LH0,0"
           yy$(10%) = "^LL813"
           yy$(11%) = "^PR3,4,8"                      /* PR Label Print Speed*/
                                                      /* a = 3 Print Speed   */
                                                      /* b = 4 Slew  SPeed   */
                                                      /* c = 8 Back Feed Spee*/ 
           yy$(12%) = "^JMA"

           yy$(13%) = "^COY,362"                      /* 256 + 128 Mem */
                                                      /* (-) 22k Intern*/
                                                      /* Serial Number Barcode */
           yy$(14%) = "01^FO1089,282^BY2,2.0,98^BCR,98,N,N,N^FR^FD>:"
                                                      /* Warranty Id   */
           yy$(15%) = "^FO983,22^CI0^A0R,41,41^FR^FDWarranty ID:^FS"
           yy$(16%) = "03^FO983,368^CI0^A0R,41,41^FR^FD"
                                                      /* Production Barcode    */
           yy$(17%) = "^FO914,22^CI0^A0R,41,41^FR^FDProduction Barcode:^FS"
           yy$(18%) = "04^FO914,368^CI0^A0R,41,41^FR^FD"
                                                      /* Complaint Number      */
           yy$(19%) = "^FO845,22^CI0^A0R,41,41^FR^FDComplaint No:^FS"
           yy$(20%) = "05^FO845,368^CI0^A0R,41,41^FR^FD"
                                                      /* RGA Number            */
           yy$(21%) = "^FO774,22^CI0^A0R,41,41^FR^FDRGA No:^FS"
           yy$(22%) = "06^FO774,368^CI0^A0R,41,41^FR^FD"
                                                      /* Sales Order Number    */
           yy$(23%) = "^FO705,22^CI0^A0R,41,41^FR^FDSales Order:^FS"
           yy$(24%) = "07^FO705,368^CI0^A0R,41,41^FR^FD"
                                                      /* RGA Status Description*/
           yy$(25%) = "^FO636,22^CI0^A0R,41,37^FR^FDRGA Stat:^FS"
           yy$(26%) = "08^FO636,217^CI0^A0R,41,37^FR^FD"
                                                      /* RGA Reason Description*/
           yy$(27%) = "^FO567,22^CI0^A0R,41,37^FR^FDRGA Reason:^FS"
           yy$(28%) = "09^FO567,217^CI0^A0R,41,37^FR^FD"
                                                      /* MFG Description       */
           yy$(29%) = "^FO427,22^CI0^A0R,41,37^FR^FDDescr:^FS"
           yy$(30%) = "11^FO427,217^CI0^A0R,41,37^FR^FD"
                                                      /* RGA Entry Date        */
           yy$(31%) = "^FO358,22^CI0^A0R,41,41^FR^FDRGA Entry Date:^FS"
           yy$(32%) = "12^FO358,368^CI0^A0R,41,41^FR^FD"
                                                      /* Orig S.O. Date        */
           yy$(33%) = "^FO289,22^CI0^A0R,41,41^FR^FDOrig. Order Date:^FS"
           yy$(34%) = "13^FO289,368^CI0^A0R,41,41^FR^FD"
                                                      /* Additional Information*/
           yy$(35%) = "^FO217,22^CI0^A0R,41,41^FR^FDRGA Comment:^FS"
           yy$(36%) = "^FO146,22^CI0^A0R,41,41^FR^FDRGA Trailer / Loc :^FS"
           yy$(37%) = "14^FO217,368^CI0^A0R,41,41^FR^FD"
                                                      /* Trailer - Loc         */
           yy$(38%) = "15^FO146,368^CI0^A0R,41,41^FR^FD"
                                                      /* Part Number           */
           yy$(39%) = "10^FO496,217^CI0^A0R,41,37^FR^FD"

           yy$(40%) = "^FO496,22^CI0^A0R,41,37^FR^FDPart No:^FS"
                                                      /* Serial Barcode Text   */
           yy$(41%) = "02^FO1038,325^CI0^A0R,41,41^FR^FD"     /* (AWD001)      */
           yy$(42%) = "^PQ1"
           yy$(43%) = "^XZ"


        return 
