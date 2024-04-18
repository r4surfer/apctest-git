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
            *04/07/2011! (AWD005) Add Thermal changes             ! DES *~
            *09/27/2011! (AWD006) changes for FT uom              ! DES *~
            *10/20/2011! (AWD007) changes external color code     ! DES *~
            *04/30/2012! (AWD008) help desk 15925                 ! DES *~
            *02/22/2017! SR79706  Added logic to handle TX AES.   ! PWW *~
            *02/23/2017! SR79713  Had to Close GENCODES.          ! PWW *~
            *02/24/2017! (SR79721) add user IVT                   ! CMN *~
            *04/05/20181 (CR1437) add 2 badge entries and print   ! RDB *~
            *************************************************************

        sub "DSDPLA07" (pan%,            /* Number of unique Labels    */~
                        been_here%,      /* Zero (Only 1st Time)       */~
                        aes_rec$,        /* Purchase Order Label Data  */~
                        aes_rec2$,       /* Purchase Order Label CR14137*/~
                        aes_prt_flg$,    /* Printer Flag '1' or '2'    */~
                        error%,          /* Return Code                */~
                        aes_shift$,      /* Shift Code                 */~
                        aes_shiftd$,      /* Shift Code                 */~
                        aes_userid$,     /* Shift Code                 */~
                        tm_prt$,        /* thermal part number        */~
                        org_qty$,       /* standard quantity          */~
                        tm_uom$)        /* stocking unit of measure   */
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

        dim aes_rec$254,                 /* Purchase Order Label Data  */~
            aes_rec2$30,                 /* Purchase Order Label CR1437*/~
            aes_prt_flg$1,               /* AES Prt Flag 1 or 2(AWD002)*/~
            yy$(90%)255,                 /* Buffer                     */~
            xx$(90%)255,                 /* Buffer                     */~
            qty$6, qty2$6,               /* Temp Quantity      (AWD001)*/~
            lb_part$14,                  /* Raw Material Part Number   */~
            lb_barcode$30,               /* AES Barcode Number-Serial  */~
            lb_barcode_txt$30,           /* Serial No Text String      */~
            lb_id$8, pan$3,              /* Unique Pan Identifier      */~
            lb_delivery$6,               /* Product Delivery Date      */~
            lb_deliv$10,                 /* Delivery Date Formatted    */~
            lb_color$8,                  /* Product Color              */~
            lb_cut$8,                    /* Raw Material Cut Length    */~
            lb_qty$8,                   /* Label Quantity             */~
            org_qty$8,                   /* Label Quantity             */~
            lb_po$16,                    /* Purchase Order Number      */~
            lb_loc$8,                    /* Raw Material Location      */~
            lb_descr$32,                 /* Raw Material Description   */~
            lb_ord_qty$10,               /* Label Order Quantity       */~
            lb_qty1$8,                   /* Top Right Qty      (AWD001)*/~
            lb_qty2$6,                   /* Bot Right Qty      (AWD001)*/~
            lb_pin$3                     /* AES Pin Identifier Profile */


        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            axd$4,                       /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            schema$8,                    /* SR79706  Schema Switch     */~
            rslt$(5%)20                  /* Text from file opening     */

         dim message$156

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

            select #1,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24



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
            filename$ = "GENCODES" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
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
            err%    = 0%
/*SR79706*/ call "SCHEMA" (schema$,      /* What switch 1-NC 2-NE      */~
                           schema%,      /* Schema                     */~
                           #1,           /* GENCODES                   */~
                           err% )        /* error                      */
/*SR79706 + */
            if err% = 0% then goto SS_1
               errormsg$ = "(Error) Schema Lookup Error)"
               gosub error_prompt
               error% = 9%
               end

SS_1:
            if aes_userid$ = "RDB" or aes_userid$ = "CMG" or ~
               aes_userid$ = "RBN"    then goto TT_1        /* CR1437 testing */
            if schema% <> 1% then goto SS_2
               file$   = "MFGAES"
               script$ = "MFGAES"

               library$ = "APCDATA "
               volume$  = "CARLOS"
               goto SS_3
TT_1:                                                       /* CR1437 testing */
               file$   = "MFGTEST"
               script$ = "MFGTEST"
               library$ = "APCDATA "
               volume$  = "CARLOS"
               goto SS_3
SS_2:
               file$    = "MFGAES"
               script$  = "MFGAESC"

               library$ = "NEDATA  "
               volume$  = "NE    "

SS_3:
/*SR79706 - */
        return

        open_file
/*SR79706   library$        = "APCDATA "                                 */
/*SR79706   volume$         = "CARLOS"                                   */
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
          init (" ") lbl$(), lb_part$, lb_barcode$, lb_barcode_txt$,    ~
                     lb_id$, pan$, lb_delivery$, lb_color$, lb_cut$,    ~
                     lb_qty$, lb_po$, lb_loc$, lb_descr$, lb_pin$,      ~
                     lb_deliv$, qty$, lb_qty1$, lb_qty2$, message$
                                          /* (AWD001)                  */
          pan% = 0%

                                          /* Raw material Part Number */
          lb_part$ = str(aes_rec$,37%,14%)
          lbl$(01%) = lb_part$              & fs$
          lbl$(15%) = "I: " & lb_part$              & fs$
                                        /* AES Barcode (Serial No.) */
          lb_barcode$ = "WE" & str(aes_rec$,3,28)
          if aes_userid$ = "TX1" then lb_barcode$ = "TE" & str(aes_rec$,3,28)
/*(SR79721)*/
          if aes_userid$ = "IVT" then lb_barcode$ = "WT" & str(aes_rec$,3,28)
          lb_barcode_txt$ = lb_barcode$ /* Print Image              */

          lbl$(02%) = lb_barcode$  & fs$
          lbl$(16%) = "LPN: " & lb_barcode$ & fs$
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
REM A = LEN(LB_PART$)
          if len(lb_part$) <> 7 then goto L01150
REM LB_COLOR$ = "        "
          lb_descr$ = str(aes_rec$,62%,32%)
REM IF STR(LB_DESCR$,1,6) = "BEIGE " THEN LB_COLOR$ = "BEIGE"
REM IF STR(LB_DESCR$,1,6) = "WHITE " THEN LB_COLOR$ = "WHITE"
REM IF STR(LB_DESCR$,1,5) = "CLAY "  THEN LB_COLOR$ = "CLAY"

L01150:   lbl$(05%) = lb_color$             & fs$
                                          /* Raw Material Cut Length  */
          lb_cut$ = str(aes_rec$,143%,8%)

          for l = 1 to 4
            if str(lb_cut$,l,1) = "0" then str(lb_cut$,l,1) = " "
             if str(lb_cut$,l,1) <> "0" then l=4
          next l

          lbl$(06%) = lb_cut$  & bin(34%,1)   & fs$
                                          /* label Quantity           */
          lb_qty = 0.0
          get str(aes_rec$,103%,8%), using L01200, lb_qty
L01200:     FMT  PD(15,4)
REM CONVERT LB_QTY TO LB_QTY$, PIC(######.##-)
          convert lb_qty to lb_qty$, pic(######-)

/* <AWD006> */
REM LBL$(07%) = LB_QTY$               & FS$
REM IF LB_QTY$ = ORG_QTY$ THEN GOTO NOT_SAME_QTY
          lbl$(07%) = "(Std " & org_qty$ & ")" & fs$
          lbl$(23%) = "(Pcs " & lb_qty$ & ")" & fs$
REM NOT_SAME_QTY
/* </AWD006> */
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
          convert lb_ord_qty to lb_ord_qty$, pic(#####.####-)

REM LBL$(12%) = LB_ORD_QTY$           & FS$
/* <AWD006> */

          if tm_uom$ <> "FT" then goto not_ft
          tmp_nbr$ = "0"
          init(" ") des$
          des$ = lb_cut$
REM    RDGT & DECSW ARE FOR AWD008
          rdgt = 0
          decsw$ = "0"
          for l% = 1% to len(lb_cut$)
           if str(lb_cut$,l%,1%) >= "0" and                      ~
              str(lb_cut$,l%,1%) <= "9" then                     ~
              tmp_nbr$ = tmp_nbr$ & str(lb_cut$,l%,1%)
           if decsw$ = "1" then rdgt = rdgt + 1
           if str(lb_cut$,l%,1%) = "." then                      ~
                decsw$ = "1"
          next l%
          tm_cut$ = lb_cut$
          convert tmp_nbr$ to tmp_nbr, data goto bad_nbr
          if decsw$ = "1" then tmp_nbr = tmp_nbr / (10 ** rdgt)
          lb_cut = tmp_nbr
          des = 0
          des = lb_qty
          lb_qty = ((lb_qty * lb_cut) / 12.00 ) + .500

/*
            MESSAGE$ = "[_____.__][_____.__][_____.__][##]"
            CONVERT LB_QTY TO STR(MESSAGE$,2,8), PIC(#####.##)
            CONVERT LB_CUT TO STR(MESSAGE$,12,8), PIC(#####.##)
            CONVERT DES    TO STR(MESSAGE$,22,8), PIC(#####.##)
            CONVERT RDGT TO STR(MESSAGE$,32,2), PIC(##)
REM            CALL "LOGFILE" (MESSAGE$)
*/
not_ft:
/* </AWD006> */
                                          /* (AWD001)                 */
                                          /* Top Right Quantity       */
          convert lb_qty to qty2$, pic(######) /*mve to correct barcode*/
          convert lb_qty to qty$, pic(000000)
          lb_qty1$  = "Q:" & qty2$
          lbl$(12%) = qty$           & fs$
          lbl$(13%) = lb_qty1$       & fs$
          lbl$(24%) = qty2$          & fs$
                                          /* Bot Left Quantity        */
REM LB_QTY2$  = LB_QTY1$
REM LBL$(14%) = LB_QTY2$       & FS$
                                          /* (AWD001)                 */
/* CR1437 + */
          if tm_prt$ > " " then goto skip_badge
          lbl$(25%) = "Badge1:" & fs$
          lbl$(26%) = str(aes_rec2$,1%,5%) & fs$
          lbl$(27%) = "Badge2:" & fs$
          lbl$(28%) = str(aes_rec2$,6%,5%) & fs$
skip_badge:
/* CR1437 - */
/* <AWD004> */
          time$ = " "
          call "TIME" (time$)
          lbl$(17%) = time$ & fs$
          lbl$(18%) = aes_shift$ & fs$
/* </AWD004> */
/* <AWD005> */
          lbl$(19%) = "              "      & fs$
          lbl$(20%) = "              "      & fs$
          lbl$(21%) = tm_uom$ & fs$
          lbl$(22%) = "UOM: " & tm_uom$ & fs$
          if tm_prt$ <= "                " then goto skip_thermal

          tmp_nbr$ = "0"
          for l% = 1% to len(lb_cut$)
             if str(lb_cut$,l%,1%) >= "0" and                      ~
                str(lb_cut$,l%,1%) <= "9" then                     ~
             tmp_nbr$ = tmp_nbr$ & str(lb_cut$,l%,1%)
          next l%
          tm_cut$ = lb_cut$
          convert tmp_nbr$ to tmp_nbr, data goto bad_nbr
          tmp_nbr = tmp_nbr / 12
          convert tmp_nbr to tm_cut$, pic (#0)
          tm_cut$ = tm_cut$ & "'"
bad_nbr:
          lbl$(06%) = tm_cut$               & fs$
          lbl$(19%) = "Thermal P/N = "      & fs$
          lbl$(20%) = tm_prt$               & fs$
skip_thermal:
/* </AWD005> */

      read_loop
          init(" ") a$
          b$ = all(hex(00))
          nbr_lines% = nbr_lines% + 1%
          a$ = xx$(nbr_lines%)
          if a$ = " " then end_process
          a_len% = len(a$)                 /* Calc Length of Data String */
          str(b$,1%,a_len%) = str(a$,1%,a_len%) /*Put into b$ Data from a$*/
          convert str(a$,1%,2%) to ln%, data goto skip_data
                                           /* Look for a Field Number    */
          l_len% = len(lbl$(ln%))          /* Find Length of Data Element*/
                                           /* in the Label data array    */
          b_len% = (a_len% - 2%) + l_len%  /* Adjust for 2 digit field No*/

          b$ = all(hex(00))                /* Initialize Print string    */
                                           /* 1st set Font data for print*/
                                           /* 2nd append Actual Data that*/
                                           /*     will be printed        */
          str(b$,1%,b_len%) = str(a$,3%,a_len%-2%) & str(lbl$(ln%),1%,l_len%)
      skip_data
                                           /* (AWD002)                  */
        if nbr_lines% = 1% and been_here% = 1% then                       ~
                               b$ = hex(7e) & str(a$,2%,a_len%)

        if nbr_lines% = 1% and been_here% > 1% then                       ~
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
/*SR79713*/ close #1
/* CR1437 testing */
            if aes_userid$ = "RDB" or aes_userid$ = "CMG" or ~
               aes_userid$ = "RBN"    then goto ss_8

            script$ = "MFGAES"  

            if aes_prt_flg$ = "B" then                              ~
               script$ = "MFGAESB"
            if aes_prt_flg$ = "C" then                              ~
               script$ = "MFGAESC"
            goto ss_9
            
SS_8:      script$ = "MFGTEST"
                           
SS_9:       call "LINK" addr(script$, lb1%, lb2%)
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
        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
        open_error
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return


        load_label
         /* positioning is 200/inch in both the X & Y axis */
         /* make sure you use font 0 (zero) not O          */

           init(" ") yy$()

           yy$( 1%) = "^JO"                        /* This format is used*/
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
           yy$(11%) = "^PR4"                      /* (AWD002)            */
        REM         = "^PRa,b,c"                  /* PR Label Print Speed*/
                                                  /* a = 3 Print Speed   */
                                                  /* b = 4 Slew  SPeed   */
                                                  /* c = 8 Back Feed Spee*/
           yy$(12%) = "^JMA"

           yy$(13%) = "^FO50,2200^CI0^A0B,122,85^FR^FDP/N:^FS"
                                                  /* Raw Material Number */
                                                  /* Change Original     */
           yy$(14%) = "01^FO50,1500^CI0^A0B,122,85,FR^FD"
                                                  /* Cut Length Text     */
           yy$(15%) = "^FO150,872^CIO^A0B,76,42^FR^FDCut Ln:^FS"
                                                  /* Quantity Text       */
           yy$(16%) = "^FO300,2286^CI0^A0B,76,42^FR^FDQty:^FS"
                                                  /* Description Text    */
           yy$(17%) = "^FO400,2245^CI0^A0B,76,42^FR^FDDescr:^FS"
                                                  /* AES P/N Text        */
                                                  /* Change Original     */
           yy$(18%) = "^FO350,1002^CI0^A0B,102,71^FR^FDAES P/N:^FS"
                                                  /* AES Pin Id          */
                                                  /* Change Original     */
           yy$(19%) = "11^FO350,0750^CI0^A0B,102,71^FR^FD"
                                                  /* Raw Material Color  */
           yy$(20%) = "05^FO200,1500^CI0^A0B,76,42^FR^FD"
                                                  /* Raw Material Cut Len*/
           yy$(21%) = "06^FO150,0750^CI0^A0B,76,42^FR^FD"
                                                  /* P.O./Rack Quantity  */
                                                  /* Fix  12 - 07        */
           yy$(22%) = "07^FO300,1500^CI0^A0B,76,42^FR^FD"
                                                  /* Raw Material Descrp */
           yy$(23%) = "10^FO400,1500^CI0^A0B,76,42^FR^FD"
                                                  /* Delivery Date       */
           yy$(24%) = "04^FO50,0750^CI0^A0B,102,71^FR^FD"
                                                  /* Text                */
           yy$(25%) = "^FO50,1118^CI0^A0B,102,71^FR^FDDate:^FS"
                                                  /* Text                */
           yy$(26%) = "^FO200,2250^CI0^A0B,76,42^FR^FDColor:^FS"
                                                  /* P/N BAR CODE        */
           yy$(27%) = "01^FO212,050^BY3,2.0,142^BCB,142,N,N,Y^FR^FD>:"
                                                  /* Strip (2) Raw Mat   */
           yy$(28%) = "15^FO362,100^CI0^A0B,32,32^FR^FD"
                                                  /* Serial No. Bar      */
           yy$(29%) = "02^FO635,0050^BY4,2.0,100^BCB,100,N,N,Y^FR^FD>:"
                                                  /* Strip (1) Raw Mat   */
           yy$(30%) = "16^FO755,1500^CI0^A0B,26,26^FR^FD"
           yy$(31%) = "16^FO755,0250^CI0^A0B,26,26^FR^FD"
                                                  /* Strip (2) Barcode   */
                                                  /* Serial No. Bar      */
           yy$(32%) = "02^FO635,1275^BY4,2.0,100^BCB,100,N,N,Y^FR^FD>:"
                                                  /* Top Barcode         */
                                                  /* Serial No. Bar      */
           yy$(33%) = "13^FO687,2210^CI0^A0B,35,35^FR^FD"
                                                  /* Strip (2)           */
                                                  /* Serial No. Text     */
           yy$(34%) = "13^FO687,1010^CI0^A0B,35,35^FR^FD"
                                                  /* (AWD002) TTTTTTT    */
                                                  /* Top Tear Off Strip  */
                                                  /* Quantity Top Right  */
           yy$(35%) = "15^FO750,2095^CI0^A0B,35,35^FR^FD"

           yy$(36%) = "15^FO750,0895^CI0^A0B,35,35^FR^FD"

                                                   /* (AWD002) UUUUUU    */
                                                   /* Bot Tear Off Strip */
                                                   /* Quantity Bot Right */
           yy$(37%) = "02^FO12,050^BY4,2.0,142^BCB,142,N,N,Y^FR^FD>:"
/* (AWD001) */
           yy$(38%) = "16^FO170,100^CI0^A0B,32,32^FR^FD"
           yy$(39%) = "12^FO412,075^BY4,2.0,142^BCB,142,N,N,Y^FR^FD>:"
           yy$(40%) = "13^FO562,112^CI0^A0B,32,32^FR^FD"
           yy$(41%) = "^FO460,1130^CI0^A0B,122,71^FR^FDLOT:^FS"
           yy$(42%) = "02^FO460,750^CI0^A0B,122,85,FR^FD"
/* <AWD004> */
           yy$(43%) = "^FO500,2255^CI0^A0B,76,42^FR^FDTime:^FS"
           yy$(44%) = "17^FO500,1800^CI0^A0B,76,42^FR^FD"
           yy$(45%) = "^FO500,1600^CI0^A0B,76,42^FR^FDShift:^FS"
           yy$(46%) = "18^FO500,1500^CI0^A0B,76,42^FR^FD"
/* </AWD004> */
/* <AWD005>  Thermal PN area, same as badge area */
           yy$(47%) = "19^FO250,990^CI0^A0B,76,42^FR^FD"
           yy$(48%) = "20^FO250,750^CI0^A0B,76,42^FR^FD"
/* </AWD005> */
/* <AWD006> */
           yy$(49%) = "^FO150,1172^CIO^A0B,76,42^FR^FDUOM:^FS"
           yy$(50%) = "21^FO150,1097^CI0^A0B,76,42^FR^FD"
           yy$(51%) = "22^FO687,1910^CI0^A0B,35,35^FR^FD"
           yy$(52%) = "22^FO687,0710^CI0^A0B,35,35^FR^FD"
           yy$(53%) = "23^FO300,1750^CI0^A0B,76,42^FR^FD"
           yy$(54%) = "24^FO300,2100^CI0^A0B,76,42^FR^FD"
/* CR1437 New badge data shared with thermal pn */ 
           yy$(55%) = "25^FO250,1155^CIO^A0B,76,42^FR^FD"
           yy$(56%) = "26^FO250,1040^CI0^A0B,76,42^FR^FD"
           yy$(57%) = "27^FO250,872^CIO^A0B,76,42^FR^FD"
           yy$(58%) = "28^FO250,755^CI0^A0B,76,42^FR^FD"
/* </AWD006> */
           yy$(59%) = "^PQ1"
           yy$(60%) = "^XZ"
        return




