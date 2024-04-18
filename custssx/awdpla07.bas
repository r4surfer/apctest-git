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
            *                      'new_label'  - for 3844 Zebra        *~
            *                                              (AWD002)     *~
            *                       New Switch to change between the    *~
            *                       two printers.  aes_prt_flg$         *~
            *                       '1' = 3844Z  '2' = 220 XI3          *~ 
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
            *          !                                          !     *~ 
            *************************************************************

        sub "AWDPLA07" (pan%,            /* Number of unique Labels    */~
                        been_here%,      /* Zero (Only 1st Time)       */~
                        aes_rec$,        /* Purchase Order Label Data  */~
                        aes_prt_flg$,    /* Printer Flag '1' or '2'    */~
                        error%)          /* Return Code                */
                                         /* (AWD002)                   */
        dim                                                              ~
            a$90, b$90,                  /* Print Lines for Label      */~
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
            yy$(90%)90,                  /* Buffer                     */~ 
            xx$(90%)90,                  /* Buffer                     */~
            qty$4,                       /* Temp Quantity      (AWD001)*/~
            lb_part$14,                  /* Raw Material Part Number   */~
            lb_barcode$8,                /* AES Barcode Number-Serial  */~
            lb_barcode_txt$8,            /* Serial No Text String      */~
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
                                          /* AES Barcode (Serial No.) */
            lb_barcode$ = str(aes_rec$,23%,8%)
            lb_barcode_txt$ = lb_barcode$ /* Print Image              */
  
            lbl$(02%) = lb_barcode$           & fs$
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
            lbl$(05%) = lb_color$             & fs$
                                          /* Raw Material Cut Length  */
            lb_cut$ = str(aes_rec$,143%,8%)
            lbl$(06%) = lb_cut$               & fs$
                                          /* label Quantity           */
            lb_qty = 0.0 
            get str(aes_rec$,103%,8%), using L01200, lb_qty
L01200:     FMT  PD(15,4)            
            convert lb_qty to lb_qty$, pic(######.##-)

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
            
            lbl$(12%) = lb_ord_qty$           & fs$
                                          /* (AWD001)                 */
            convert lb_qty to qty$, pic(####)

                                          /* Top Right Quantity       */
            lb_qty1$  = "Q:" & qty$
            lbl$(13%) = lb_qty1$              & fs$
                                          /* Bot Left Quantity        */
            lb_qty2$  = lb_qty1$ 
            lbl$(14%) = lb_qty2$              & fs$
                                          /* (AWD001)                 */
         

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
        if a$ = "^XZ" and aes_prt_flg$ = "2" then end_process       /* Last Line */

        if nbr_lines% = 57% and aes_prt_flg$ = "1" then end_process /* Last Line */ 
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
L55030:     FMT CH(90)

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
           init(" ") yy$()
                                                     /* (AWD002)           */
           if aes_prt_flg$ = "1" then goto new_label /* 3844 Zebra Small   */
                                                     /* (AWD002)           */
  
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
           yy$(10%) = "^LL1422"
           yy$(11%) = "^PR4"                          /* (AWD002)            */
        REM         = "^PRa,b,c"                      /* PR Label Print Speed*/
                                                      /* a = 3 Print Speed   */
                                                      /* b = 4 Slew  SPeed   */
                                                      /* c = 8 Back Feed Spee*/ 
           yy$(12%) = "^JMA"

           yy$(13%) = "^COY,362"                      /* 256 + 128 Mem */
                                                      /* (-) 22k Intern*/
                                                      /* Change Original     */
        REM   yy$(14%) = "^FO39,37^CI0^A0N,148,49^FR^FDP/N?^FS"
           yy$(14%) = "^FO39,37^CI0^A0N,102,71^FR^FDP/N:^FS"
                                                      /* Raw Material Number */
                                                      /* Change Original     */ 
        REM   yy$(15%) = "01^FO185,37^CI0^A0N,148,49^FR^FD"
           yy$(15%) = "01^FO185,37^CI0^A0N,102,71^FR^FD"
                                                      /* Cut Length Text     */ 
           yy$(16%) = "^FO26,467^CI0^A0N,89,36^FR^FDCut Ln:^FS"
                                                      /* Quantity Text       */
           yy$(17%) = "^FO24,534^CI0^A0N,89,36^FR^FDQuantity:^FS"
                                                      /* P.O. Text           */
           yy$(18%) = "^FO24,604^CI0^A0N,89,36^FR^FDPO #:^FS"
                                                      /* Location Text       */
           yy$(19%) = "^FO24,671^CI0^A0N,89,36^FR^FDLocation:^FS"
                                                      /* Description Text    */
           yy$(20%) = "^FO24,738^CI0^A0N,89,35^FR^FDDescr:^FS"
                                                      /* AES P/N Text        */
                                                      /* Change Original     */
        REM   yy$(21%) = "^FO173,1093^CI0^A0N,116,58^FR^FDAES P/N?^FS"
           yy$(21%) = "^FO136,1120^CI0^A0N,81,81^FR^FDAES P/N:^FS"
                                                      /* AES Pin Id          */
                                                      /* Change Original     */
        REM   yy$(22%) = "11^FO514,1093^CI0^A0N,116,58^FR^FD"
           yy$(22%) = "11^FO473,1120^CI0^A0N,81,81^FR^FD"
                                                      /* Raw Material Color  */
           yy$(23%) = "05^FO215,400^CI0^A0N,89,34^FR^FD"
                                                      /* Raw Material Cut Len*/
           yy$(24%) = "06^FO215,467^CI0^A0N,89,36^FR^FD"
                                                      /* P.O./Rack Quantity  */
                                                      /* Fix  12 - 07        */
           yy$(25%) = "07^FO215,534^CI0^A0N,89,35^FR^FD"
                                                      /* P.O. Number         */
           yy$(26%) = "08^FO215,604^CI0^A0N,89,36^FR^FD"
                                                      /* Raw Material Loc.   */
           yy$(27%) = "09^FO215,671^CI0^A0N,89,39^FR^FD"
                                                      /* Raw Material Descrp */
           yy$(28%) = "10^FO185,738^CI0^A0N,89,36^FR^FD"
                                                      /* xxx-xxx Pan-Item    */
           yy$(29%) = "03^FO561,400^CI0^A0N,89,36^FR^FD"
                                                      /* Delivery Date       */
           yy$(30%) = "04^FO561,467^CI0^A0N,89,36^FR^FD"
                                                      /* Text                */
           yy$(31%) = "^FO179,1016^CI0^A0N,89,37^FR^FD____________________^FS"
                                                      /* Text                */
           yy$(32%) = "^FO24,1016^CI0^A0N,89,37^FR^FDExt #^FS"
                                                      /* Text                */
           yy$(33%) = "^FO179,941^CI0^A0N,89,37^FR^FD____________________^FS"
                                                      /* Text                */
           yy$(34%) = "^FO24,941^CI0^A0N,89,34^FR^FDOper:^FS"
                                                      /* Text                */
           yy$(35%) = "^FO179,874^CI0^A0N,89,37^FR^FD____________________^FS"
                                                      /* Text                */
           yy$(36%) = "^FO24,874^CI0^A0N,89,36^FR^FDTime:^FS"
                                                      /* Text                */
           yy$(37%) = "^FO179,807^CI0^A0N,89,37^FR^FD____________________^FS"
                                                      /* Text                */
           yy$(38%) = "^FO24,807^CI0^A0N,89,36^FR^FDDate:^FS"
                                                      /* Text                */
           yy$(39%) = "^FO26,400^CI0^A0N,89,36^FR^FDColor:^FS"
                                                      /* Strip (2) Raw Mat   */
           yy$(40%) = "01^FO311,1337^CI0^A0N,73,36^FR^FD"
                                                      /* Strip (1) Raw Mat   */
           yy$(41%) = "01^FO313,1235^CI0^A0N,73,36^FR^FD"
                                                      /* Strip (2) Barcode   */
                                                      /* Serial No. Bar      */ 
           yy$(42%) = "02^FO93,1329^BY2,2.0,51^BCN,51,N,N,N^FR^FD>;"
                                                      /* Top Barcode         */
                                                      /* Serial No. Bar      */
           yy$(43%) = "02^FO325,150^BY2,2.0,142^BCN,142,N,N,N^FR^FD>;"
                                                      /* Top Barcode         */
                                                      /* Serial No. Text     */
           yy$(44%) = "02^FO293,311^CI0^A0N,89,43^FR^FD"
                                                      /* Strip (2)           */
                                                      /* Serial No. Text     */
           yy$(45%) = "02^FO126,1390^CI0^A0N,26,13^FR^FD"
                                                      /* Strip (1) Barcode   */
                                                      /* Serial No. Barcode  */
           yy$(46%) = "02^FO93,1229^BY2,2.0,51^BCN,51,N,N,N^FR^FD>;"
                                                      /* Strip (1)           */
                                                      /* Serial No. Text     */
           yy$(47%) = "02^FO126,1288^CI0^A0N,26,13^FR^FD"
                                                      /* (AWD002) TTTTTTT    */
                                                      /* Top Tear Off Strip  */
                                                      /* Quantity Top Right  */
           yy$(48%) = "13^FO636,1233^CI0^A0N,73,36^FR^FD"

                                                      /* (AWD002) UUUUUU    */
                                                      /* Bot Tear Off Strip */
                                                      /* Quantity Bot Right */
           yy$(49%) = "14^FO636,1337^CI0^A0N,73,36^FR^FD"
                                                      /* (AWD001)            */
           yy$(50%) = "^PQ1"
           yy$(51%) = "^XZ"

        return


       new_label
                                                /* This a different label    */
                                                /* format from all the other */
                                                /* Zebra's (3844 - Printer   */
          init(" ") yy$() 

          yy$( 1%) = "^XA"
          yy$( 2%) = "^MCY"
          yy$( 3%) = "^XZ"
          yy$( 4%) = "^XA"
          yy$( 5%) = "^FWN^CFD,24^PW1200^LH0,0"
          yy$( 6%) = "^CI0^PR2^MNY^MMT^MD0^PON^PMN^LRN"
          yy$( 7%) = "^XZ"
          yy$( 8%) = "^XA"
          yy$( 9%) = "^MCY"
          yy$(10%) = "^XZ"
          yy$(11%) = "^XA"
          yy$(12%) = "^DFR:TEMP_FMT.ZPL"
          yy$(13%) = "^LRN"
                                                /* Label Text                */
          yy$(14%) = "^A0N,150,104^FO57,54^FDP/N:^FS"
                                                /* Raw Material Number       */
          yy$(15%) = "01^A0N,150,104^FO273,54^FD"
                                                /* Cut Length Text           */
          yy$(16%) = "^A0N,90,74^FO39,690^FDCut Ln:^FS"
                                                /* Quantity Text             */
          yy$(17%) = "^A0N,90,74^FO36,789^FDQuantity:^FS"
                                                /* P.O. Text                 */
          yy$(18%) = "^A0N,90,74^FO36,891^FDPO #:^FS"
                                                /* Location Text             */
          yy$(19%) = "^A0N,90,74^FO36,990^FDLocation:^FS"
                                                /* Description Text          */    
          yy$(20%) = "^A0N,90,74^FO36,1089^FDDescr:^FS"
                                                /* AES P/N Text              */
          yy$(21%) = "^A0N,116,118^FO255,1614^FDAES P/N:^FS"
                                                /* AES PIN ID                */
          yy$(22%) = "11^A0N,116,116^FO759,1614^FD"
                                                /* Raw Material Color        */ 
          yy$(23%) = "05^A0N,90,72^FO318,591^FD"
                                                /* Raw Material Cut Length   */
          yy$(24%) = "06^A0N,90,76^FO318,690^FD"
                                                /* P.O/Rack Quantity         */
                                                /* Fix  12 -- 07             */ 
          yy$(25%) = "07^A0N,90,74^FO318,789^FD"
                                                /* P.O. Number               */
          yy$(26%) = "08^A0N,90,74^FO318,891^FD"
                                                /* Raw Material Location     */
          yy$(27%) = "09^A0N,90,78^FO318,990^FD"
                                                /* Raw Material Descriptiom  */
          yy$(28%) = "10^A0N,90,74^FO273,1089^FD"
                                                /* xxx-xxx Pan-Item   ?????  */ 
          yy$(29%) = "03^A0N,90,74^FO828,591^FD"
                                                /* Delivery Date ?????       */
          yy$(30%) = "04^A0N,90,74^FO828,690^FD"
                                                /* User Data Line            */ 
          yy$(31%) = "^A0N,90,76^FO264,1500^FD____________________^FS"
                                                /* User Data Text            */
          yy$(32%) = "^A0N,90,74^FO33,1500^FDExt #^FS"
                                                /* User Data Line            */
          yy$(33%) = "^A0N,90,76^FO261,1389^FD____________________^FS"
                                                /* User Data Text            */
          yy$(34%) = "^A0N,90,74^FO36,1389^FDOper:^FS"
                                                /* User Data Line            */
          yy$(35%) = "^A0N,90,76^FO264,1290^FD____________________^FS"
                                                /* User Data Text            */
          yy$(36%) = "^A0N,90,74^FO36,1290^FDTime:^FS"
                                                /* User Data Line            */ 
          yy$(37%) = "^A0N,90,76^FO261,1191^FD____________________^FS"
                                                /* Label Text                */
          yy$(38%) = "^A0N,90,74^FO36,1191^FDDate:^FS"
                                                /* Label Text                */ 
          yy$(39%) = "^A0N,90,74^FO39,591^FDColor:^FS"
                                                /* Raw Material Bottom Right */ 
          yy$(40%) = "01^A0N,75,74^FO459,1974^FD"
                                                /* Raw Material Bottom Left  */
          yy$(41%) = "01^A0N,75,74^FO462,1824^FD"
                                                /* Barcode Top               */
          yy$(42%) = "02^BY3^FO480,222^BCN,210,N,N,N^FD>;"
                                                /* Barcode Text Top          */
          yy$(43%) = "02^A0N,90,90^FO432,459^FD"
                                                /* Barcode Bottom Left       */
          yy$(44%) = "02^BY3^FO138,1815^BCN,75,N,N,N^FD>;"
                                                /* Barcode Bottom Left Text  */ 
          yy$(45%) = "02^A0N,27,30^FO186,1902^FD"
                                                /* Barcode Bottom Right Text */ 
          yy$(46%) = "02^A0N,27,30^FO186,2052^FD"
                                                /* Barcode Bottom Right      */
          yy$(47%) = "02^BY3^FO138,1962^BCN,75,N,N,N^FD>;"
                                                /* (AWD001)                  */
          yy$(48%) = "13^A0N,75,74^FO939,1821^FD"

          yy$(49%) = "14^A0N,75,74^FO939,1974^FD"

                                                /* (AWD001)                  */
                                                /* End of Data               */
          yy$(50%) = "^XZ"
          yy$(51%) = "^XA"
          yy$(52%) = "^XFR:TEMP_FMT.ZPL"
          yy$(53%) = "^PQ1,0,1,Y"
          yy$(54%) = "^XZ"
          yy$(55%) = "^XA"
          yy$(56%) = "^IDR:TEMP_FMT.ZPL"
          yy$(57%) = "^XZ"

        return

