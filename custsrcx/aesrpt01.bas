        REM *************************************************************~
            *                                                           *~
            *   AAA   EEEEE   SSS   RRRR   PPPP   TTTTT   000     1     *~
            *  A   A  E      S   S  R   R  P   P    T    0   0   11     *~
            *  AAAAA  EEEE    S     RRRRR  PPPP     T    0   0    1     *~
            *  A   A  E          S  R  R   P        T    0   0    1     *~
            *  A   A  EEEEE  SSSS   R   R  P        T     000   11111   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * AESRPT01 - AES Rack Raw Material Variance Report          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/05/05 ! New Program for (AES) - Last Mod Date    ! RHH *~
            *          !                                          !     *~
            *          !                                          !     *~
            *************************************************************

        dim                                                              ~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            dte1$10, beg_dte$10,         /* Beg     Sales Order Date   */~
            dte2$10, end_dte$10,         /* End     Sales Order Date   */~
            po1$16,                      /* Beg Model Code             */~
            po2$16,                      /* End Model Code             */~
            verify_po_no$16,             /* VBKMASTR Label Database    */~
            aes_key3$31,                 /* Alt Key 3 AESPRDLB         */~
            aes_rec$256,                 /* AESPRDLB Record            */~
            aes_po$12,                   /* Purchase Order Numer       */~
            aes_item$2,                  /* Purchase Order Line Item   */~
            aes_pan_no$2,                /* Rack Id Assoc. With Line It*/~
            aes_raw$25,                  /* Raw Material               */~
            aes_serial$8,                /* Rack Serial Number         */~
            aes_scan_dte$8,              /* AES Scan Date into 500     */~
            aes_scan_tme$5,              /* AES Time Rac Scanned       */~
            aes_scan_qty$8,              /* AES Qty Scanned into 500   */~
            aes_scan_usr$3,              /* AES User Who Scanned Prod  */~
            aes_awd_dte$8,               /* AWD Date Prod Scanned 100  */~
            aes_awd_tme$5,               /* AWD Time rack scanned 100  */~
            aes_awd_qty$8,               /* AWD Qty Scanned into 100   */~
            aes_awd_usr$3,               /* AWD User Who Scanned Prod  */~
            aes_delivery$8,              /* Date Rack label Created    */~
            aes_delivery_sav$8,          /* Save Date                  */~
            aes_po_sav$12,               /* Save Raw Material          */~
            aes_recv$1,                  /* Label Qty Status 0, 1, 2   */~
            aes_qty_avail$8,             /* Line Item Qty Available    */~
            aes_id$5,                    /* Line Item - Pan Number     */~ 
            raw_p$14,                    /* Print Value                */~
            aes_status$5,                /* Rack Status                */~    
            wrk_key$44,                  /* Work File Key              */~
            company$40,                  /* For Report Company Name    */~
            print_title$40,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$35, pname$21            
            apc$   = "AES-Raw Material Serial No Analysis"
            pname$ = "AESRPT01 - 01/05/2005"

        REM *************************************************************

            mat f2% = con

            mat f1% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! AESPRDLB ! AES Inventory Barcode Labels File        *~
            * #02 ! VBKMASTR ! Purchase Order Header                    *~
            * #04 ! GENCODES ! System Master Code Table Files           *~
            * #10 ! APCRPTWK ! Report Work File                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #01,  "AESPRDLB",                                     ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  30,                     ~
                        alt key  1, keypos =   23, keylen =   8,         ~
                            key  2, keypos =   37, keylen =  25, dup,    ~
                            key  3, keypos =   31, keylen =  31, dup,    ~
                            key  4, keypos =  239, keylen =  14, dup  


            select #02,  "VBKMASTR",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1030,                                  ~
                        keypos = 1, keylen = 25,                         ~
                        alt key 1, keypos = 10, keylen = 16

            select #03,  "GENCODES",                                     ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


            select #10, "AESRPTWK",                                      ~
                        varc,     indexed,  recsize =   300,             ~
                        keypos =    1, keylen =  44

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#02, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#03, fs%(3%), f2%(3%),  0%, rslt$(3%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATFMTC" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 2%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 2% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_report
            gosub open_work_file

            gosub generate_report
        return clear all
        REM GOTO INPUTMODE
        goto exit_program

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Beginning and Ending Rack Production Date?           ",~
         "Enter a Beginning and Ending PUrchae Order Number or 'ALL'?  "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, dte1$, dte2$, beg_dte$,    ~
                      end_dte$, po1$, po2$, verify_po_no$


        return

        REM *************************************************************~
            *************************************************************


        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        REM DATALOAD
        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT
        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40180,         /* Beg/End Date      */   ~
                                L40180          /* Beg/End PO Number */

              goto L40210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40210:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,63), "Today:",                                     ~
               at (01,70), fac(hex(8c)), date$                  , ch(10),~
               at (01,24), fac(hex(a4)), apc$                   , ch(35),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Beg/End Rack Prod Date:",                    ~
               at (06,26), fac(lfac$(1%)), dte1$                , ch(10),~
               at (06,45), fac(lfac$(1%)), dte2$                , ch(10),~
                                                                         ~
               at (07,02), "Beg/End P.O. Number   :",                    ~
               at (07,26), fac(lfac$(2%)), po1$                 , ch(16),~
               at (07,45), fac(lfac$(2%)), po2$                 , ch(16),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L40560
                  call "PRNTSCRN"
                  goto L40210

L40560:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40750     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40710
                str(pf$(3%),64%)    = " "  :  str(pfkeys$,16%,1%) = hex(ff)
L40710:     if fieldnr% > 1% then L40730
                str(pf$(2%),18%,26%) = " "  :  str(pfkeys$,4%,1%) = hex(ff)
L40730:     return

L40750: if fieldnr% > 0% then L40840  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (14)Print Report"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40840:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                                       "
            pf$(2%) = "                                        " &       ~
                     "                                       "
            pf$(3%) = "                                        " &       ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50150,         /* Beg/End Rack Prod Date*/ ~
                              L50200          /* Beg/End P.O. Number   */
            return


L50150: REM Beginning and Ending Rack Prod Date       DTE1$,DTE2$
            if str(dte1$,1%,3%) = "ALL" then goto L50190
            if dte1$ <> " " then goto L50160
               goto L50190

L50160:        date% = 0%
               call "DATEOKC" (dte1$, date%, errormsg$)
               if errormsg$ <> " " then goto L50185
               if dte2$ <> " " then goto L50180
                  dte2$ = dte1$

L50180:           call "DATEOKC" (dte2$, date%, errormsg$)
                  if errormsg$ <> " " then L50185
               beg_dte$ = dte1$
               end_dte$ = dte2$
               call "DATUFMTC" (beg_dte$)
               call "DATUFMTC" (end_dte$)
               if beg_dte$ > end_dte$ then goto L50195

        return
L50185:        errormsg$ = "(Error) Invalid Order Date."
               init(" ") dte1$, dte2$, beg_dte$, end_dte$
               gosub error_prompt
        return
L50190:     str(dte1$,1%,3%) = "ALL"
            str(dte2$,1%,3%) = "ALL"
            beg_dte$ = "ALL" : end_dte$ = "ALL"
        return
L50195:        errormsg$ = "(Error) Invalid beginning date?"
               init(" ") dte1$, dte2$, beg_dte$, end_dte$
               gosub error_prompt
        return

L50200: REM Beginning and Ending PO Number            po1$, po2$
            if str(po1$,1%,3%) = "ALL" then goto L50250
            if po1$ <> " "  then goto L50220
               goto L50250
L50220:     if po2$ <> " " then goto L50230
               po2$ = po1$

L50230:     init(" ") verify_po_no$ 
            verify_po_no$ = po1$
            gosub verify_po
            if check% = 0% then goto L50260

            init(" ") verify_po_no$ 
            verify_po_no$ = po2$
            gosub verify_po
            if check% = 0% then goto L50270


            if po1$ > po2$ then goto L50280
        return
L50250:     str(po1$,1%,3%) = "ALL" 
            str(po2$,1%,3%) = "ALL"

        return
L50260:
            errormsg$ = "(Error) Invalid Beginning P.O. Number?"
            init(" ") po1$, po2$
            gosub error_prompt
        return
L50270:
            errormsg$ = "(Error) Invalid Ending P.O. Number?"
            init(" ") po1$, po2$
            gosub error_prompt
        return
L50280:
            errormsg$ = "(Error) Beginning P.O. MUST be Greater than Ending?"
            init(" ") po1$, po2$
            gosub error_prompt
        return

        verify_po
           check% = 0% 
           read #02,key 1% = verify_po_no$, eod goto L50310

               check% = 1%
L50310:
        return


        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L55050: %!########## ########                           #################~
        ~#######################                                 AESRPT01:~
        ~!

L55090: %!Rack  DTE Beg: ##########     End: ##########  ################~
        ~########################                              Page: #####~
        ~!

L55130: %!PO Number Beg: ############## End: ##############              ~
        ~                                                                 ~
        ~!

                                                   /* Customer Header */
L55210: %!RackDate!P.O. Number !Ln-Id!Qty Aval!Rack Material !SerialNo!AE~
        ~S SDte!AESTm!AES Rqty!AES!AWD SDte!AWDTm!AWD RQty!AWD!Stat ! Rec ~
        ~!

L55250: %!--------!------------!-----!--------!--------------!--------!--~
        ~------!-----!--------!---!--------!-----!--------!---!-----!-----~
        ~!

L55290: %!########!############!#####!########!##############!########!##~
        ~######!#####!########!###!########!#####!########!###!#####!  #  ~
        ~!

L55320: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~+

L55360: %!---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~!

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            call "SETPRNT" ("AESRPT", " ",25000%, 0%)
            select printer (134)

            page_no% = 0%
            lcnt%    = 99%

            print_title$ = "AES Rack Raw Material Variance Report"
            call "FMTTITLE" (print_title$, " ", 12%)
            date$ = date  :  call "DATFMTC" (date$)
            call "TIME" (rpt_time$)
            company$ = "ATRIUM Windows and Doors"
            call "FMTTITLE" (company$, " ", 12%)
        return

        close_printer
            call "SETPRNT" ("APCRPT", " ",0%, 1%)
        return

        generate_report
                                         /* Check label file for data */ 
            gosub selecting_data
                                         /* Print sorted report       */

            call "SHOSTAT" ("PRINTING REPORT")
            CALL "PAUSE" ADDR(50%)

            gosub select_printer

            init(" ") aes_po_sav$, aes_delivery_sav$
            count% = 0%
            wrk_key$ = all(hex(00))

        generate_report_next
            init(" ") aes_rec$, aes_raw$, aes_serial$, aes_scan_dte$,  ~
                      aes_scan_tme$, aes_scan_qty$, aes_scan_usr$,     ~
                      aes_awd_dte$, aes_awd_tme$, aes_awd_qty$,        ~
                      aes_awd_usr$, aes_delivery$, aes_status$, aes_po$,~
                      aes_item$, aes_qty_avail$, aes_recv$, aes_id$,   ~
                      aes_pan_no$

            read #10,key > wrk_key$, using GEN_1, wrk_key$, aes_rec$,  ~
                                                  eod goto generate_done
GEN_1:         FMT CH(44), CH(256)
            count% = count% + 1%

            aes_delivery$ = str(aes_rec$,31%,6%)
            call "DATEFMT" (aes_delivery$)
                                               /* Purchae Order No   */
            aes_po$ = str(aes_rec$,1%,12%)
                                               /* P.O. Line Item     */
            aes_item$ = str(aes_rec$,18%,2%)
                                               /* Rack Id for Line Item */
            aes_pan_no$ = str(aes_rec$,21%,2%)
                                               /* Rack Line Id       */
            aes_id$ = aes_item$ & "-" & aes_pan_no$
        
                                               /* Raw Material Number*/ 
            aes_raw$ = str(aes_rec$,37%,25%)

                                               /* Rack Serial Number */
            aes_serial$ = str(aes_rec$,23%,8%)
                                               /* PO Line Item Qty   */
            aes_ord_qty = 0.0
            get str(aes_rec$,111%,8%), using GEN_2, aes_ord_qty

                                               /* PO Line Qty Used   */
            aes_used_qty = 0.0
            get str(aes_rec$,119%,8%), using GEN_2, aes_used_qty

            aes_qty_avail = 0.0
            aes_qty_avail = aes_ord_qty - aes_used_qty
            convert aes_qty_avail to aes_qty_avail$, pic(####.##-)
 
                                               /* AES Scan Date      */
            aes_scan_dte$ = str(aes_rec$,182%,6%)
            if aes_scan_dte$ <> " " then call "DATEFMT" (aes_scan_dte$)

                                               /* AES Scan Time      */
            aes_scan_tme$ = str(aes_rec$,188%,2%) & ":" &             ~
                            str(aes_rec$,190%,2%)

                                               /* AES Scan Qty       */
            get str(aes_rec$,192%,8%), using GEN_2, aes_scan_qty
GEN_2:         FMT PD(15,4)
            convert aes_scan_qty to aes_scan_qty$, pic(####.##-)
                                               /* AES_Scan User      */
            aes_scan_usr$ = str(aes_rec$,200%,3%)

                                               /* AWD Scan Date      */
            aes_awd_dte$ = str(aes_rec$,203%,6%)
            if aes_awd_dte$ <> " " then call "DATEFMT" (aes_awd_dte$)

                                               /* AWD Scan Time      */
            aes_awd_tme$ = str(aes_rec$,209%,2%) & ":" &              ~
                           str(aes_rec$,211%,2%)
 
                                               /* AWD Scan Qty       */
            get str(aes_rec$,213%,8%), using GEN_2, aes_awd_qty

            convert aes_awd_qty to aes_awd_qty$, pic(####.##-)
                                               /* AWD Scan User      */
            aes_awd_usr$ = str(aes_rec$,221%,3%)
                                               /* Date Received      */
            aes_recv$ = " "
            aes_recv$ = str(aes_rec$,239%,1%)

            gosub check_aes_status

            gosub print_detail

            goto generate_report_next
        generate_done
            print using L55320

            gosub close_printer
            call "FILEBGON" addr(#10)
        return

        check_aes_status
            if aes_scan_usr$ = "???" and aes_awd_usr$ = "???"  then  ~
               aes_status$ = "O AES "

            if aes_scan_usr$ <> "???" and aes_awd_usr$ = "???"  Then ~
               aes_status$ = "O AWD"

            if aes_scan_usr$ <> "???" and aes_awd_usr$ <> "???" then ~
               aes_status$ = "CLOSE"

            if str(aes_rec$,94%,6%) = "DELETE" then                  ~
               aes_status$ = "DELETE"

        return

        print_header                        /* Page Header       */
          page_no% = page_no% + 1%
          if lcnt% <> 99% then print using L55320
          lcnt% = 0%
          print page
          print using L55320
          print using L55050, date$, rpt_time$, company$
          print using L55090, dte1$, dte2$, print_title$, page_no%
          print using L55130, po1$, po2$
          print using L55360
          print using L55210
          lcnt% = lcnt% + 6%
        return

        print_detail                        /* Line Item Detail  */

          if lcnt% > 57% then gosub print_header
                                            /* Print Columns     */
          print using L55250
                                            /* print Detail      */
          raw_p$ = str(aes_raw$, 1%,14%)    /* Print length      */

          if aes_delivery$ <> aes_delivery_sav$ then goto print_a

          if aes_delivery$ = aes_delivery_sav$ and                           ~
             aes_po$ = aes_po_sav$ then goto print_B  
                                            /* Default          */
          goto print_a 
 
        return

        print_a                         /* 1st time for Date and Material */
          print using L55290, aes_delivery$, aes_po$, aes_id$,     ~
                aes_qty_avail$, raw_p$  , aes_serial$,             ~
                aes_scan_dte$, aes_scan_tme$, aes_scan_qty$,       ~
                aes_scan_usr$, aes_awd_dte$, aes_awd_tme$,         ~
                aes_awd_qty$, aes_awd_usr$, aes_status$, aes_recv$
          lcnt% = lcnt% + 2%
          aes_delivery_sav$ = aes_delivery$
          aes_po_sav$       = aes_po$
        return

        print_b                         /* Date and PO the Same          */
          print using L55290, " "       , " "       , aes_id$,     ~
                aes_qty_avail$, raw_p$  , aes_serial$,             ~
                aes_scan_dte$, aes_scan_tme$, aes_scan_qty$,       ~
                aes_scan_usr$, aes_awd_dte$, aes_awd_tme$,         ~
                aes_awd_qty$, aes_awd_usr$, aes_status$, aes_recv$
          lcnt% = lcnt% + 2%
          aes_delivery_sav$ = aes_delivery$
          aes_po_sav$       = aes_po$
        return


        selecting_data
             call "SHOSTAT" ("Selecting Rack Labels")
            
             count% = 0%

             aes_key3$ = all(hex(00))

             if str(dte1$,1%,3%) <> "ALL"                                    ~
                              then str(aes_key3$,1%,6%) = str(beg_dte$,1%,6%)

             read #1,key 3% > aes_key3$, using SEL_1, aes_rec$,              ~
                                                 eod goto selecting_data_done
SEL_1:          FMT CH(256)

             goto SEL_3
        selecting_data_next
             if mod(count%,25%) <> 0 then goto SEL_2
                convert count% to count$, pic(######)
                call "SHOSTAT" ("Purchase Orders Scanned ("&count$&")")

SEL_2:       read #1, using SEL_1, aes_rec$, eod goto selecting_data_done

SEL_3:       count% = count% + 1%
             convert count% to rhh$, pic(###)
        
        REM    call "SHOSTAT" ("Found ---> " & rhh$)
        REM    stop

                                         /* Check Rack Production Date */
             if str(dte1$,1%,3%) = "ALL" then goto SEL_4
                if str(aes_rec$,31%,6%) > str(end_dte$,1%,6%) then       ~
                                               goto selecting_data_done
                                         /* Check PO Number            */
SEL_4:       if str(po1$,1%,3%) = "ALL" then goto SEL_5
                if str(aes_rec$,1%,16%) < po1$ or                        ~
                   str(aes_rec$,1%,16%) > po2$ then                      ~
                                              goto selecting_data_next

SEL_5: 
        REM     call "SHOSTAT" ("In Report ----> " & rhh$ )
        REM     stop

               init(" ") wrk_key$
                                        /* Set Production Date        */
               str(wrk_key$,1%,6%)  = str(aes_rec$,31%,6%) 
                                        /* Purchase Order Number      */
               str(wrk_key$,7%,16%) = str(aes_rec$,1%,16%)
                                        /* PO Line Item               */
               str(wrk_key$,23%,3%) = str(aes_rec$,17%,3%)
                                        /* Rack Identifier            */
               str(wrk_key$,26%,3%) = str(aes_rec$,20%,3%)
                                        /* Set Serial Number (Unique) */
               str(wrk_key$,29%,8%) = str(aes_rec$,23%,8%)

               gosub update_work
               goto selecting_data_next
        selecting_data_done
              if count% = 0% then goto SEL_6

        REM      convert count% to count$, pic(####)

        REM      call "SHOSTAT" ("Rack records found----> " & count$)
        REM      stop

        return
SEL_6:
            errormsg$ = "No Data Found"
            gosub error_prompt
        return 

        update_work

            write #10, using UPD_1, wrk_key$, aes_rec$, eod goto UPD_2

UPD_1:         FMT CH(44), CH(256)
        return
UPD_2:
            call "SHOSTAT" ("(Error) Serial Number - " & str(wrk_key$,26%,8%) )
            stop
        return

        open_work_file
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 500%, rslt$(10%))
        return

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program

            end
