        REM *************************************************************~
	    *                  (MFGTEST - Turned OFF)          (RHHTEST)*~
            *  Subroutine Name   - AWDPLA07                             *~
            *  Creation Date     - 09/14/03                             *~
            *  Last Modified Date- 08/04/06                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Modifications By  - Royal H. Hoffman                     *~ 
            *                                                           *~
            *  Description       - Write records to a file to print a   *~
            *                      AES Barcode Label.                   *~
            *                                                           *~
            *                      Print File  = MFGAES                 *~
            *                      Script File = MFGAES                 *~
            *                                                           *~
            *                      Program supports both old and new    *~
            *                      Zebra label format.                  *~
            *                      'load_label' - for 220 XI3 Zebra     *~
            *                                              (AWD002)     *~
            *                       New Switch to change between the    *~
            *                       two printers.  aes_prt_flg$         *~
            *                       'A' =        'B' =                  *~ 
            *                                                           *~    
            *-----------------------------------------------------------*~
            * AWDPLA07 - Generates the label format and data to print   *~
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
            * 12/15/04 ! Original - New Label Program.            ! RHH *~
            * 03/01/06 ! (AWD001) Mod to add the Quantity to the  ! RHH *~
            *          !    two tear off Barcode Labels.          !     *~
            * 08/04/06 ! (AWD002) Mod for Printer Selection Flag  ! RHH *~
            *          !    '1' = 3844Z (Small) '2' = 220XI (Big) !     *~
            *06/01/2007! (AWD003) label mod.                      ! DES *~ 
            *10/13/2010! (AWD004) Add time and shift code         ! DES *~ 
            *************************************************************

        sub "DESPLA07" (pan%,            /* Number of unique Labels    */~
                        been_here%,      /* Zero (Only 1st Time)       */~
                        aes_rec$,        /* Purchase Order Label Data  */~
                        aes_prt_flg$,    /* Printer Flag '1' or '2'    */~
                        error%,          /* Return Code                */~
                        aes_shift$,      /* Shift Code                 */~
                        aes_shiftd$,      /* Shift Code                 */~
                        aes_userid$)     /* Shift Code                 */
                                         /* (AWD002)                   */
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
            xx$(90%)255,                 /* Buffer                     */~
            qty$4, qty2$4,               /* Temp Quantity      (AWD001)*/~
            lb_part$14,                  /* Raw Material Part Number   */~
            lb_barcode$30,               /* AES Barcode Number-Serial  */~
            lb_barcode_txt$30,           /* Serial No Text String      */~
            lb_id$8, pan$3,              /* Unique Pan Identifier      */~
            lb_delivery$6,               /* Product Delivery Date      */~
            lb_deliv$10,                 /* Delivery Date Formatted    */~
            lb_color$8,                  /* Product Color              */~
            lb_cut$8,                    /* Raw Material Cut Length    */~
            lb_qty$10,                   /* Label Quantity             */~
            lb_po$16,                    /* Purchase Order Number      */~
            lb_loc$8,                    /* Raw Material Location      */~
            lb_descr$32,                 /* Raw Material Description   */~
            lb_ord_qty$10,               /* Label Order Quantity       */~
            lb_qty1$6,                   /* Top Right Qty      (AWD001)*/~
            lb_qty2$6,                   /* Bot Right Qty      (AWD001)*/~
            lb_pin$3                     /* AES Pin Identifier Profile */


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
            * #5  ! MFGAES   ! Print File For AES Barcode Labels        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #5, "ROYAL", varc, consec, recsize =  155


                                             
                                                              
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
               file$   = "MFGAES"      
               script$ = "MFGAES"      
                                           /* (RHHTEST)                */ 
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
            init (" ") lbl$(), lb_part$, lb_barcode$, lb_barcode_txt$, lb_id$,   ~
                       pan$, lb_delivery$, lb_color$, lb_cut$, lb_qty$, lb_po$,  ~
                       lb_loc$, lb_descr$, lb_pin$, lb_deliv$, qty$, lb_qty1$,   ~
                       lb_qty2$
                                          /* (AWD001)                  */
            pan% = 0%

                                          /* Raw material Part Number */       
            lb_part$ = str(aes_rec$,37%,14%) 
            lbl$(01%) = lb_part$              & fs$
            lbl$(15%) = "I: " & lb_part$              & fs$
                                          /* AES Barcode (Serial No.) */
            lb_barcode$ = str(aes_rec$,1,30)
            lb_barcode_txt$ = lb_barcode$ /* Print Image              */
  
            lbl$(02%) = lb_barcode$           & fs$
            lbl$(16%) = "L: " & lb_barcode$           & fs$
                                          /* Item No. with Rack Id    */
            lb_id$ = str(aes_rec$,17%,3%) & "-" & str(aes_rec$,20%,3%)

            lbl$(03%) = lb_id$                & fs$
                                          /* Delivery Date            */
            lb_delivery$ = str(aes_rec$,31%,6%)
            lb_deliv$ = lb_delivery$
            call "DATFMTC" (lb_deliv$)  
            lbl$(04%) = lb_deliv$             & fs$

                                          /* Raw Material Color       */
            lb_color$ = str(aes_rec$,135%,8%)
            a = len(lb_part$)
            if len(lb_part$) <> 7 then goto L01150
            lb_color$ = "        "
            lb_descr$ = str(aes_rec$,62%,32%)
            if str(lb_descr$,1,6) = "BEIGE " then lb_color$ = "BEIGE"
            if str(lb_descr$,1,6) = "WHITE " then lb_color$ = "WHITE"
            if str(lb_descr$,1,5) = "CLAY "  then lb_color$ = "CLAY"

L01150:     lbl$(05%) = lb_color$             & fs$
                                          /* Raw Material Cut Length  */
            lb_cut$ = str(aes_rec$,143%,8%)
            lbl$(06%) = lb_cut$               & fs$
                                          /* label Quantity           */
            lb_qty = 0.0 
            get str(aes_rec$,103%,8%), using L01200, lb_qty
L01200:     FMT  PD(15,4)            
REM         convert lb_qty to lb_qty$, pic(######.##-)
            convert lb_qty to lb_qty$, pic(######-)

            lbl$(07%) = lb_qty$               & fs$
                                          /* P.O. PO Number           */
            lb_po$ = str(aes_rec$,1%,16%)
            lbl$(08%) = lb_po$                & fs$
                                          /* Raw Material Location    */
            lb_loc$ = str(aes_rec$,151%,8%)
            lbl$(09%) = lb_loc$               & fs$
                                          /* Raw Material Description */
            lb_descr$ = str(aes_rec$,62%,32%)
            lbl$(10%) = lb_descr$             & fs$
                                          /* AES P/N Mumber           */
            lb_pin$ = str(aes_rec$,169%,3%)
            lbl$(11%) = lb_pin$               & fs$
                                          /* Label Ord Qty            */
            get str(aes_rec$,111%,8%), using L01200, lb_ord_qty
            convert lb_ord_qty to lb_ord_qty$, pic(####.####-)
            
REM         lbl$(12%) = lb_ord_qty$           & fs$
                                          /* (AWD001)                 */
            convert lb_qty to qty$, pic(0000)
            lbl$(12%) = qty$           & fs$

                                          /* Top Right Quantity       */
            convert lb_qty to qty2$, pic(####)
            lb_qty1$  = "Q:" & qty2$
            lbl$(13%) = lb_qty1$              & fs$
                                          /* Bot Left Quantity        */
REM         lb_qty2$  = lb_qty1$ 
REM         lbl$(14%) = lb_qty2$              & fs$
                                          /* (AWD001)                 */
         
/* <AWD004> */ 
            time$ = " "
            call "TIME" (time$)
            lbl$(17%) = time$ & fs$
            lbl$(18%) = aes_shift$ & fs$
/* </AWD004> */ 

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
            script$ = "MFGAES"      
            if aes_prt_flg$ = "B" then                              ~
               script$ = "MFGAESB"      
            call "LINK" addr(script$, lb1%, lb2%)
            if lb1%    > 0% then error% = 4%

            call "FILEBGON" (#5)          /* Scratch 'MFGAES'         */
        end

        file_exists
          comp% = 2%     
          hdr$ = "***  New AES Barcode Label ***"
          msg$(1%) = "       The File (MFGAES) Already Exists.         "
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
           yy$(10%) = "^LL2400"
           yy$(11%) = "^PR4"                          /* (AWD002)            */
        REM         = "^PRa,b,c"                      /* PR Label Print Speed*/
                                                      /* a = 3 Print Speed   */
                                                      /* b = 4 Slew  SPeed   */
                                                      /* c = 8 Back Feed Spee*/ 
           yy$(12%) = "^JMA"

           yy$(13%) = "^FO50,2200^CI0^A0B,122,85^FR^FDP/N:^FS"
                                                      /* Raw Material Number */
                                                      /* Change Original     */ 
           yy$(14%) = "01^FO50,1500^CI0^A0B,122,85,FR^FD"
                                                      /* Cut Length Text     */ 
           yy$(15%) = "^FO150,1147^CIO^A0B,76,42^FR^FDCut Ln:^FS"
                                                      /* Quantity Text       */
           yy$(16%) = "^FO300,2200^CI0^A0B,76,42^FR^FDQuantity:^FS"
                                                      /* Location Text       */
           yy$(17%) = "^FO250,1110^CI0^A0B,76,42^FR^FDLocation:^FS"
                                                      /* Description Text    */
           yy$(18%) = "^FO400,2245^CI0^A0B,76,42^FR^FDDescr:^FS"
                                                      /* AES P/N Text        */
                                                      /* Change Original     */
           yy$(19%) = "^FO350,1002^CI0^A0B,102,71^FR^FDAES P/N:^FS"
                                                      /* AES Pin Id          */
                                                      /* Change Original     */
           yy$(20%) = "11^FO350,0750^CI0^A0B,102,71^FR^FD"
                                                      /* Raw Material Color  */
           yy$(21%) = "05^FO200,1500^CI0^A0B,76,42^FR^FD"
                                                      /* Raw Material Cut Len*/
           yy$(22%) = "06^FO150,0750^CI0^A0B,76,42^FR^FD"
                                                      /* P.O./Rack Quantity  */
                                                      /* Fix  12 - 07        */
           yy$(23%) = "07^FO300,1500^CI0^A0B,76,42^FR^FD"
                                                      /* Raw Material Loc.   */
           yy$(24%) = "09^FO250,0750^CI0^A0B,76,42^FR^FD"
                                                      /* Raw Material Descrp */
           yy$(25%) = "10^FO400,1500^CI0^A0B,76,42^FR^FD"
                                                      /* Delivery Date       */
           yy$(26%) = "04^FO50,0750^CI0^A0B,102,71^FR^FD"
                                                      /* Text                */
           yy$(27%) = "^FO50,1118^CI0^A0B,102,71^FR^FDDate:^FS"
                                                       /* Text                */
           yy$(28%) = "^FO200,2250^CI0^A0B,76,42^FR^FDColor:^FS"
                                                      /* P/N BAR CODE        */
           yy$(29%) = "01^FO12,050^BY3,2.0,142^BCB,142,N,N,Y^FR^FD>:"
                                                      /* Strip (2) Raw Mat   */
           yy$(30%) = "15^FO162,100^CI0^A0B,32,32^FR^FD"
                                                      /* Serial No. Bar      */ 
           yy$(31%) = "02^FO625,0050^BY3,2.0,100^BCB,100,N,N,Y^FR^FD>:"
                                                      /* Strip (1) Raw Mat   */
           yy$(32%) = "16^FO750,1375^CI0^A0B,26,26^FR^FD"
           yy$(33%) = "16^FO750,0125^CI0^A0B,26,26^FR^FD"
                                                      /* Strip (2) Barcode   */
                                                      /* Serial No. Bar      */ 
           yy$(34%) = "02^FO625,1275^BY3,2.0,100^BCB,100,N,N,Y^FR^FD>:"
                                                      /* Top Barcode         */
                                                      /* Serial No. Bar      */
           yy$(35%) = "12^FO737,1613^BY4,2.0,100^BCB,100,N,N,Y^FR^FD>:"
           yy$(36%) = "13^FO687,1725^CI0^A0B,26,26^FR^FD"
                                                      /* Strip (2)           */
                                                      /* Serial No. Text     */
           yy$(37%) = "12^FO737,0400^BY4,2.0,100^BCB,100,N,N,N^FR^FD>:"
           yy$(38%) = "13^FO687,0500^CI0^A0B,26,26^FR^FD"
                                                      /* Strip (1) Barcode   */
                                                      /* Serial No. Barcode  */
           yy$(39%) = "01^FO625,1875^BY3,2.0,100^BCB,100,N,N,Y^FR^FD>:"
                                                      /* Strip (1)           */
                                                      /* Serial No. Text     */
           yy$(40%) = "01^FO625,0670^BY3,2.0,100^BCB,100,N,N,Y^FR^FD>:"
                                                      /* (AWD002) TTTTTTT    */
                                                      /* Top Tear Off Strip  */
                                                      /* Quantity Top Right  */
           yy$(41%) = "15^FO750,2075^CI0^A0B,26,26^FR^FD"

           yy$(42%) = "15^FO750,0875^CI0^A0B,26,26^FR^FD"

                                                      /* (AWD002) UUUUUU    */
                                                      /* Bot Tear Off Strip */
                                                      /* Quantity Bot Right */
           yy$(43%) = "02^FO212,050^BY3,2.0,142^BCB,142,N,N,Y^FR^FD>:"
     /* (AWD001)            */
           yy$(44%) = "16^FO362,100^CI0^A0B,32,32^FR^FD"
           yy$(45%) = "12^FO412,075^BY4,2.0,142^BCB,142,N,N,Y^FR^FD>:"
           yy$(46%) = "13^FO562,112^CI0^A0B,32,32^FR^FD"
           yy$(47%) = "^FO460,1102^CI0^A0B,122,85^FR^FDLOT:^FS"
           yy$(48%) = "02^FO460,750^CI0^A0B,122,85,FR^FD"
/* <AWD004> */ 
           yy$(49%) = "^FO500,2255^CI0^A0B,76,42^FR^FDTime:^FS"
           yy$(50%) = "17^FO500,1800^CI0^A0B,76,42^FR^FD"
           yy$(51%) = "^FO500,1600^CI0^A0B,76,42^FR^FDShift:^FS"
           yy$(52%) = "18^FO500,1500^CI0^A0B,76,42^FR^FD"
/* </AWD004> */ 
           yy$(53%) = "^PQ1"
           yy$(54%) = "^XZ"
        return




