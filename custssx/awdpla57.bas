        REM *************************************************************~
            *  Subroutine Name   - AWDPLA57                             *~
            *  Creation Date     - 09/10/07                             *~
            *  Written By        - David Speight                        *~
            *                                                           *~
            *  Description       - Write records to a file to print a   *~
            *                      AES Barcode Label.                   *~
            *                                                           *~
            *                      Print File  = MFGDES                 *~
            *                      Script File = MFGDES                 *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * AWDPLA57 - Generates the label format and data to print   *~
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
            *09/10/2007! Original - New Label Program.            ! DES *~
            *************************************************************

        sub "AWDPLA57" (pan%,            /* Number of unique Labels    */~
                        been_here%,      /* Zero (Only 1st Time)       */~
                        lb_part$,        /* part number                */~
                        lb_mat_des$,     /* material description       */~
                        pg_nbr$,         /* Page number    of          */~
                        max_page$,       /* total pages                */~
                        sub_inv$,        /* sub inventory number       */~
                        locator$,        /* warehouse (locator)        */~
                        qty_amt$,        /* quantity                   */~
                        qty_um$,         /* unit of measure            */~
                        pour$,           /* POUR                       */~
                        aes_prt_flg$,    /* Printer Flag '1' or '2'    */~
                        error%)          /* Return Code                */
        dim                                                              ~
            a$256, b$256,                 /* Print Lines for Label      */~
            lbl$(40%)60,                 /* Label Data Array           */~
            hdr$40,                      /* ASKUSER Header Text        */~
            msg$(3%)79,                  /* ASKUSER Info Text          */~
            fs$3,                        /* End of Lbl Records         */~
            file$8,                      /* Lbl Print File             */~
            library$8,                   /* Library Name = APCDATA     */~
            script$8,                    /* Lbl SHELL SCRIPT           */~
            volume$6                     /* DISK VOLUME = CARLOS       */

        dim aes_rec$256,                 /* Purchase Order Label Data  */~
            aes_prt_flg$1,               /* AES Prt Flag 1 or 2(AWD002)*/~
            yy$(90%)255,                 /* Buffer                     */~ 
            xx$(90%)255                  /* Buffer                     */ 

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~ 
            axd$4,                       /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        dim tmp_amt$10

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21                        
            apc$   = "(AWD) Generate AES Barcode Labels       "
            pname$ = "AWDPLA07 - 08/04/2006"               /* (AWD002) */

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
            * #5  ! MFGDES   ! Print File For AES Barcode Labels        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #5, "NOMAD", varc, consec, recsize =  155


                                             
                                                              
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
               file$   = "MFGDES"      
               script$ = "MFGDES"      
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
            pan% = 0%

            /* Raw material Part Number */       
            lbl$(01%) = lb_part$              & fs$
            /* material description     */ 
            lbl$(02%) = lb_mat_des$           & fs$
            /* page number              */ 
            lbl$(03%) = pg_nbr$ & " OF " & max_page$ & fs$
            /* sub inventory # (100)    */ 
            lbl$(04%) = sub_inv$           & fs$
            /* locator (whse)           */ 
            lbl$(05%) = locator$           & fs$
            /* qty amt                  */ 
	    wrk_amt$ = " "
	    for m = 1 to 10 
	       if str(qty_amt$,m,1) > " " then wrk_amt$ = wrk_amt$ & str(qty_amt$,m,1)
            next m
            lbl$(06%) = wrk_amt$           & fs$
            /* qty label w/ UM          */ 
            lbl$(07%) = wrk_amt$ & "  " & qty_um$ & fs$
            /* pour                     */ 
            lbl$(08%) = pour$              & fs$
            /* sub inventory # first 3 char */
            lbl$(09%) = str(sub_inv$,1,3)  & fs$

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
                                           /* (AWD002)                 */         
        if a$ = "^XZ" then end_process       /* Last Line */

                                           /* (AWD002)                 */ 
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
L55030:     FMT CH(155)

        REM *************************************************************~
            *           S P E C I A L   R O U T I N E S                 *~
            *************************************************************

        print_line
                                                  /* (RHHTEST)          */
            write #5, using L55030, b$, eod goto L61550
        return

L61550:     error% = 5%
        return clear all

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************
        exit_sub


        end

        exit_print
            lb1% = 0% : lb2% = 0%

            close #5
            script$ = "MFGDES"      
            if aes_prt_flg$ = "B" then                              ~
               script$ = "MFGDESB"      
            call "LINK" addr(script$, lb1%, lb2%)
            if lb1%    > 0% then error% = 4%

            call "FILEBGON" (#5)          /* Scratch 'MFGDES'         */
        end

        file_exists
          comp% = 2%     
          hdr$ = "***  New AES Barcode Label ***"
          msg$(1%) = "       The File (MFGDES) Already Exists.         "
          msg$(2%) = "     New  AES B A R C O D E   L a b e l s        "
          msg$(3%) = "Press <RETURN> To Exit Prog, or PF(16) to Delete."
          call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        load_label
	/* positioning is 200/inch in both the X & Y axis */
        /* make sure you use font 0 (zero) not O          */

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
           yy$(10%) = "^LL600"  
           yy$(11%) = "^PR4"                          /* (AWD002)            */
        REM         = "^PRa,b,c"                      /* PR Label Print Speed*/
                                                      /* a = 3 Print Speed   */
                                                      /* b = 4 Slew  SPeed   */
                                                      /* c = 8 Back Feed Spee*/ 
           yy$(12%) = "^JMA"
           yy$(13%) = "03^FO951,25^CI0^A0N,95,45,FR^FD"
           yy$(14%) = "01^FO455,25^CI0^A0N,50,85,FR^FD"
           yy$(15%) = "01^FO455,75^BY3,2.0,100^BCN,100,N,N,N^FR^FD>:"
           yy$(16%) = "02^FO525,187^CI0^A0N,50,20,FR^FD"
           yy$(17%) = "^FO225,400^CI0^A0N,75,45,FR^FDSUB INV^FS"
           yy$(18%) = "09^FO195,475^BY3,2.0,80^BCN,80,N,N,N^FR^FD>:"     
           yy$(19%) = "04^FO265,560^CI0^AON,25,15,FR^FD" 
           yy$(20%) = "07^FO595,270^CI0^A0N,75,30,FR^FD" 
           yy$(21%) = "06^FO525,350^BY3,2.0,80^BCN,80,N,N,N^FR^FD>:"      
           yy$(22%) = "^FO887,400^CI0^A0N,75,45,FR^FDPOUR^FS"
           yy$(23%) = "08^FO815,475^BY3,2.0,80^BCN,80,Y,N,N^FR^FD>:"      
           yy$(24%) = "^PQ1"
           yy$(25%) = "^XZ"
        return




