        REM *************************************************************~
            *                                                           *~
            *   AAA   EEEEE   SSS   RRRR   PPPP   TTTTT   000    222    *~
            *  A   A  E      S   S  R   R  P   P    T    0   0  2   2   *~
            *  AAAAA  EEEE    S     RRRRR  PPPP     T    0   0    2     *~
            *  A   A  E          S  R  R   P        T    0   0   2      *~
            *  A   A  EEEEE  SSSS   R   R  P        T     000   22222   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * AESRPT02 - AES Rack Raw Material Variance Report          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/17/05 ! New Program for (AES) - Last Mod Date    ! RHH *~
            *          !                                          !     *~
            *          !                                          !     *~
            *************************************************************

        dim                                                              ~
            rhh$3, mode$5,               /* Debug Counter              */~
            cnt$6, unitprice$10,         /* Rack Counter               */~
            dte_prompt$15,               /* Date Prompt Text from Sort */~
            prt_date$8,                  /* Date Description           */~
            prt_txt$8, prt_value$16,     /* Break total Text           */~                            
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            scr$(12%)40,                 /* Screen Display             */~
            sc_type$1, sc_typed$25,      /* Report Type Code           */~
            sc_sort$1, sc_sortd$25,      /* Report Sort Code           */~
            raw1$15, raw2$15,            /* Begin/End Raw Material No. */~  
            dte1$10, beg_dte$10,         /* Beg     Sales Order Date   */~
            dte2$10, end_dte$10,         /* End     Sales Order Date   */~
            po1$16,                      /* Beg Model Code             */~
            po2$16,                      /* End Model Code             */~
            verify_po_no$16,             /* VBKMASTR Label Database    */~
            aes_key3$31,                 /* Alt Key 3 AESPRDLB         */~
            aes_rec$256,                 /* AESPRDLB Record            */~
            aes_vendor$9,                /* PO Vendor Code             */~
            aes_po$12,                   /* Purchase Order Numer       */~
            aes_item$2,                  /* Purchase Order Line Item   */~
            aes_pan_no$2,                /* Rack Id Assoc. With Line It*/~
            aes_raw$25,                  /* Raw Material               */~
            aes_serial$8,                /* Rack Serial Number         */~
            aes_scan_dte$8,              /* AES Scan Date into 500     */~
            aes_scan_tme$5,              /* AES Time Rac Scanned       */~
            aes_scan_qty$6,              /* AES Qty Scanned into 500   */~
            aes_scan_usr$3,              /* AES User Who Scanned Prod  */~
            aes_awd_dte$8,               /* AWD Date Prod Scanned 100  */~
            aes_awd_tme$5,               /* AWD Time rack scanned 100  */~
            aes_awd_qty$6,               /* AWD Qty Scanned into 100   */~
            aes_awd_usr$3,               /* AWD User Who Scanned Prod  */~
            aes_delivery$8,              /* Date Rack label Created    */~
            aes_recv$1,                  /* Label Qty Status 0, 1, 2   */~
            aes_rcv_dte$8,               /* Date received into store 100*/~
            aes_qty_avail$6,             /* Line Item Qty Available    */~
            aes_id$5,                    /* Line Item - Pan Number     */~ 
            raw_p$14,                    /* Print Value                */~
            raw_d$30,                    /* Raw Material Description   */~
            aes_status$5,                /* Rack Status                */~
            aes_status1$4,               /* Rack Status - Summary      */~    
            wrk_key$44,                  /* Work File Key              */~
            sav_wrk_key$44,              /* Use for Total Break        */~
            aes_qty$6, awd_qty$6,        /* Piece quantites            */~
            aes_price$11,                /* Price Total                */~
            aes_qty_tot$6, awd_qty_tot$6,/* Report Piece Total         */~
            aes_price_tot$11,            /* Report Price Total         */~
            aes_ln_price$11,             /* Detail Line Price          */~
            aes_ln_price1$10,            /* Summary Price              */~
            aes_qty1$6,                  /* Piece Quantity             */~
            lab_qty$6,                   /* Piece Quantites            */~
            lab_qty_tot$6,               /* Report Label Total         */~  
            po_key$28,                   /* VBKLINES Primary Key       */~
            hny_key$25,                  /* HNYMASTR Primary Key       */~
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
            pname$ = "AESRPT02 - 06/17/2005"

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
            * #03 ! GENCODES ! System Master Code Table Files           *~
            * #04 ! VBKLINES ! Purchase Order Line Item File            *~
            * #05 ! HNYMASTR ! Imventory master file                    *~
            * #10 ! AESRPTWK ! Report Work File                         *~
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

            select #04,  "VBKLINES",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 28

            select #05,  "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #10, "AESRPTWK",                                      ~
                        varc,     indexed,  recsize =   300,             ~
                        keypos =    1, keylen =  44

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#02, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#03, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#04, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#05, fs%(5%), f2%(5%),  0%, rslt$(5%))

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


            scr$( 1%) = "****************************************"
            scr$( 2%) = "*       Report Sort Selections         *"
            scr$( 3%) = "* 1. By Rack Date, P.O., Line Item No. *"
            scr$( 4%) = "* 2. By Rack Date, Raw Material Number *"
            scr$( 5%) = "* 3. By AES Scan Date, P.O. Line Item  *"
            scr$( 6%) = "* 4. By AES Scan Date, Raw Material    *"
            scr$( 7%) = "* 5. By AWD Scan Date, P.O. Line Item  *"
            scr$( 8%) = "* 6. By AWD Scan Date, Raw Material    *"
            scr$( 9%) = "* 7. By Posting Date , P.O., Line Item *"
            scr$(10%) = "* 8. By Posting Date , Raw Material    *"
            scr$(11%) = "****************************************"
        


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 5%
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
                  if keyhit%  = 12% then gosub print_report
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 5% then editpg1
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
            detail% = 0%                             /* Detail Report  */
            if keyhit% = 14% then detail% = 1%       /* Summary Report */
            mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work

       REM     gosub open_work_file

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
         "Enter Report Type: 0=AES Open,1=AWD Open,2=Closed,A=All Data?",~
         "Enter a Valid Report Sort Code 1, 2, 3, 4, 5, 6, 7, 8?       ",~
         "Enter a Beginning and Ending Rack Production Date?           ",~
         "Enter a Beginning and Ending Purchase Order Number or 'ALL'? ",~
         "Enter a Beginning and Ending Raw Material Number or 'ALL'?   "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, dte1$, dte2$, beg_dte$,    ~
                      end_dte$, po1$, po2$, verify_po_no$, sc_type$,     ~
                      sc_typed$, sc_sort$, sc_sortd$, raw1$, raw2$

            dte_prompt$ = "Rack Prod Date:"

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
              on fieldnr% gosub L40180,         /* Report Type Code  */  ~
                                L40180,         /* Report Sort Code  */  ~
                                L40180,         /* Beg/End Date      */  ~
                                L40180,         /* Beg/End PO Number */  ~
                                L40180          /* Beg/End Raw Mat No*/

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
               at (03,02), "Report Type (0,1,2,A) :",                    ~
               at (03,26), fac(lfac$(1%)), sc_type$             , ch(01),~
               at (03,45), fac(hex(84)), sc_typed$              , ch(25),~
                                                                         ~
               at (04,02), "Report Sort Code      :",                    ~
               at (04,26), fac(lfac$(2%)), sc_sort$             , ch(01),~
               at (04,45), fac(hex(84)), sc_sortd$              , ch(25),~
                                                                         ~
               at (05,02), "Beg/End",                                    ~
               at (05,10), fac(hex(84)),dte_prompt$             , ch(15),~ 
               at (05,26), fac(lfac$(3%)), dte1$                , ch(10),~
               at (05,45), fac(lfac$(3%)), dte2$                , ch(10),~
                                                                         ~
               at (06,02), "Beg/End P.O. Number   :",                    ~
               at (06,26), fac(lfac$(4%)), po1$                 , ch(16),~
               at (06,45), fac(lfac$(4%)), po2$                 , ch(16),~
                                                                         ~
               at (07,02), "Beg/End Raw Mat. No.  :",                    ~
               at (07,26), fac(lfac$(5%)), raw1$                , ch(15),~
               at (07,45), fac(lfac$(5%)), raw2$                , ch(15),~
                                                                         ~
               at (09,21), fac(hex(84)), scr$( 1%)              , ch(40),~
               at (10,21), fac(hex(84)), scr$( 2%)              , ch(40),~
               at (11,21), fac(hex(84)), scr$( 3%)              , ch(40),~
               at (12,21), fac(hex(84)), scr$( 4%)              , ch(40),~
               at (13,21), fac(hex(84)), scr$( 5%)              , ch(40),~
               at (14,21), fac(hex(84)), scr$( 6%)              , ch(40),~
               at (15,21), fac(hex(84)), scr$( 7%)              , ch(40),~
               at (16,21), fac(hex(84)), scr$( 8%)              , ch(40),~
               at (17,21), fac(hex(84)), scr$( 9%)              , ch(40),~
               at (18,21), fac(hex(84)), scr$(10%)              , ch(40),~
               at (19,21), fac(hex(84)), scr$(11%)              , ch(40),~
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
                     "(12)Detail Report                      "
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "(14)Summary Report     (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffff0cff0e0f1000)
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
            on fieldnr% gosub L50100,         /* Report Type Code      */~
                              L50200,         /* Report Sort Code      */~
                              L50300,         /* Beg/End Rack Prod Date*/~
                              L50400,         /* Beg/End P.O. Number   */~
                              L50500          /* Beg/End Raw Mat No.   */
            return

L50100: REM Report Type Code                          sc_type$, sc_typed$
            if sc_type$ <> " " then goto L50110
               sc_type$ = "A"
L50110:
            p% = pos("012A" = sc_type$)
            if p% = 0% then goto L50120

            if sc_type$ = "0" then sc_typed$ = "Open - AES Not Scanned   "
            if sc_type$ = "1" then sc_typed$ = "Open - AWD Not Scanned   "
            if sc_type$ = "2" then sc_typed$ = "Closed - Scanned AES/AWD "
            if sc_type$ = "A" then sc_typed$ = "All - AES Rack Data      "

        return 
L50120:     errormsg$ = "(Error) - Invalid report type code? (0,1,2,A) "
            init(" ") sc_type$, sc_typed$
            gosub error_prompt
        return

L50200: REM Report Sort Code                          sc_sort$, sc_sortd$
            if sc_sort$ <> " " then goto L50210
               sc_sort$ = "1"
L50210:
            p% = pos("12345678" = sc_sort$)
            if p% = 0% then goto L50220

            if sc_sort$ = "1" then sc_sortd$ = "Rack Dte, PO, Line Item  "
            if sc_sort$ = "2" then sc_sortd$ = "Rack Dte, Raw Material No"
            if sc_sort$ = "3" then sc_sortd$ = "AES Dte, PO, Line Item   "
            if sc_sort$ = "4" then sc_sortd$ = "AES Dte,Raw Material No  "
            if sc_sort$ = "5" then sc_sortd$ = "AWD Dte, PO, Line Item   "
            if sc_sort$ = "6" then sc_sortd$ = "AWD Dte,Raw Material No  "
            if sc_sort$ = "7" then sc_sortd$ = "PostDte, PO, Line Item   "
            if sc_sort$ = "8" then sc_sortd$ = "PostDte,Raw Material No  "
                                               /* Date Screen Prompts   */
            if sc_sort$ = "1" or sc_sort$ = "2" then dte_prompt$ = "Rack Prod Date:"

            if sc_sort$ = "3" or sc_sort$ = "4" then dte_prompt$ = "AES Scan Date :"

            if sc_sort$ = "5" or sc_sort$ = "6" then dte_prompt$ = "AWD Scan Date :"

            if sc_sort$ = "7" or sc_sort$ = "8" then dte_prompt$ = "Posting  Date :"

                                               /* Report Column Header */
            if sc_sort$ = "1" or sc_sort$ = "2" then prt_date$ = "RackDate"

            if sc_sort$ = "3" or sc_sort$ = "4" then prt_date$ = "AES Date"

            if sc_sort$ = "5" or sc_sort$ = "6" then prt_date$ = "AWD Date"
         
            if sc_sort$ = "7" or sc_sort$ = "8" then prt_date$ = "PostDate"

                                              /* Break total Print Text */
            if sc_sort$ = "1" or sc_sort$ = "3" or sc_sort$ = "5" or sc_sort$ = "7"~
                                             then prt_txt$ = "P.O. No."

            if sc_sort$ = "2" or sc_sort$ = "4" or sc_sort$ = "6" or sc_sort$ = "8"~
                                             then prt_txt$ = "Raw Mat."
        return
L50220:     errormsg$ = "(Error) - Invalid Report Sort Code? (1,2,3,4,5,6)"
            init(" ") sc_sort$, sc_sortd$
            gosub error_prompt
        return
 
L50300: REM Beginning and Ending Rack Prod Date       DTE1$,DTE2$

            if str(dte1$,1%,3%) = "ALL" then goto L50390
            if dte1$ <> " " then goto L50360
               goto L50390

L50360:        date% = 0%
               call "DATEOKC" (dte1$, date%, errormsg$)
               if errormsg$ <> " " then goto L50385
               if dte2$ <> " " then goto L50380
                  dte2$ = dte1$

L50380:           call "DATEOKC" (dte2$, date%, errormsg$)
                  if errormsg$ <> " " then L50385
               beg_dte$ = dte1$
               end_dte$ = dte2$
               call "DATUFMTC" (beg_dte$)
               call "DATUFMTC" (end_dte$)
               if beg_dte$ > end_dte$ then goto L50395

        return
L50385:        errormsg$ = "(Error) Invalid Order Date."
               init(" ") dte1$, dte2$, beg_dte$, end_dte$
               gosub error_prompt
        return
L50390:     str(dte1$,1%,3%) = "ALL"
            str(dte2$,1%,3%) = "ALL"
            beg_dte$ = "ALL" : end_dte$ = "ALL"
        return
L50395:        errormsg$ = "(Error) Invalid beginning date?"
               init(" ") dte1$, dte2$, beg_dte$, end_dte$
               gosub error_prompt
        return

L50400: REM Beginning and Ending PO Number            po1$, po2$
            if str(po1$,1%,3%) = "ALL" then goto L50450
            if po1$ <> " "  then goto L50420
               goto L50450
L50420:     if po2$ <> " " then goto L50430
               po2$ = po1$

L50430:     init(" ") verify_po_no$ 
            verify_po_no$ = po1$
            gosub verify_po
            if check% = 0% then goto L50460

            init(" ") verify_po_no$ 
            verify_po_no$ = po2$
            gosub verify_po
            if check% = 0% then goto L50470


            if po1$ > po2$ then goto L50480
        return
L50450:     str(po1$,1%,3%) = "ALL" 
            str(po2$,1%,3%) = "ALL"

        return
L50460:
            errormsg$ = "(Error) Invalid Beginning P.O. Number?"
            init(" ") po1$, po2$
            gosub error_prompt
        return
L50470:
            errormsg$ = "(Error) Invalid Ending P.O. Number?"
            init(" ") po1$, po2$
            gosub error_prompt
        return
L50480:
            errormsg$ = "(Error) Beginning P.O. MUST be Greater than Ending?"
            init(" ") po1$, po2$
            gosub error_prompt
        return

L50500: REM Beg/End Raw Material Number                 raw1$, raw2$
           if str(raw1$,1%,3%) = "ALL" then goto L50550
            if raw1$ <> " "  then goto L50520
               goto L50550
L50520:     if raw2$ <> " " then goto L50530
               raw2$ = raw1$

L50530:     init(" ") hny_key$ 
            hny_key$ = raw1$
            gosub raw_material_number
            if check% = 0% then goto L50560

            init(" ") hny_key$ 
            hny_key$ = raw2$
            gosub raw_material_number
            if check% = 0% then goto L50570


            if raw1$ > raw2$ then goto L50580
        return
L50550:     str(raw1$,1%,3%) = "ALL" 
            str(raw2$,1%,3%) = "ALL"


        return
L50560:    errormsg$ = "(Error) Invalid Beginning Raw Material Number?"
           init(" ") raw1$, raw2$
           gosub error_prompt
        return

L50570:    errormsg$ = "(Error) Invalid Ending Raw Material Number?"
           init(" ") raw1$, raw2$
           gosub error_prompt
        return

L50580:    errormsg$ = "(Error) Beginning Raw Material MUST be Greater than Ending?"
           init(" ") raw1$, raw2$
           gosub error_prompt
        return

                                             /* (HNYMASTR)             */
        raw_material_number     
           check% = 0%
           read #5,key 0% = hny_key$, eod goto L50710
              check% = 1%
L50710:
        return
                                             /* (VBKMASTR)             */
        verify_po
           check% = 0% 
           read #02,key 1% = verify_po_no$, eod goto L50810
              check% = 1%
L50810:
        return


        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L55050: %!########## ########                            ################~
        ~####################### Typ: #########################  AESRPT02:~
        ~!

L55090: %! ######## Beg: ##########     End: ##########  ################~
        ~####################### Srt: #########################  Page:####~
        ~!

L55130: %!PO Number Beg: ################ End: ################          ~
        ~          Raw Material Beg: ############### End: ############### ~
        ~!

                                                   /* Detail Header    */
L55210: %!########!P.O. Number !Ln-Id!QtyAvl!Rack Material !SerialNo!AES ~
        ~SDte!AESTm!AESQty!AES!AWD SDte!AWDTm!AWDQty!AWD!Stat ! Mat Price ~
        ~!

L55250: %!--------!------------!-----!------!--------------!--------!----~
        ~----!-----!------!---!--------!-----!------!---!-----!-----------~
        ~!

L55290: %!########!############!#####!######!##############!########!####~
        ~####!#####!######!###!########!#####!######!###!#####!###########~
        ~!

L55320: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~+

L55360: %!---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~!
                                                  /* Detail Break Totals  */

L55380: %!  ########: ################   ###### Racks                    ~
        ~          !######!                  !######!         !###########~
        ~!

                                                  /* Detail Report Totals */

L55390: %!  Report Totals for            ###### Racks                    ~
        ~          !######!                  !######!         !###########~
        ~!


L55400: %!---------------------------------------------------------------~
        ~----------!------!------------------!------!---------!-----------~
        ~!

                                                   /* Summary Header   */
L55410: %!########!P.O. Number !Ln-Id!Rack Material !<----- Description -~
        ~--------->!SerialNo!LabQty!AESQty!AWDQty!PostDate!Stat! Mat Price~
        ~!

L55420: %!--------!------------!-----!--------------!--------------------~
        ~----------!--------!------!------!------!--------!----!----------~
        ~!

L55430: %!########!############!#####!##############!####################~
        ~##########!########!######!######!######!########!####!##########~
        ~!

                                               /* Summary Break Totals  */

L55440: %!  ########: ################   ###### Racks                    ~
        ~                   !######!######!######!             !##########~
        ~!


L55445: %!---------------------------------------------------------------~
        ~-------------------!------!------!------!-------------!----------~
        ~!
                                               /* Summary Report Totals */

L55450: %!  Report Totals for            ###### Racks                    ~
        ~                   !######!######!######!             !##########~
        ~!

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            call "SETPRNT" ("AESRPT", " ",25000%, 0%)
            select printer (134)

            page_no% = 0%
            lcnt%    = 99%
            if detail% = 0% then print_title$ = "AES Rack Detail Raw Material Report"  ~
                            else print_title$ = "AES Rack Summary Raw Material Report" 

            call "FMTTITLE" (print_title$, " ", 12%)
            date$ = date  :  call "DATFMTC" (date$)
            call "TIME" (rpt_time$)
            company$ = "ATRIUM Windows and Doors"
            call "FMTTITLE" (company$, " ", 12%)
        return

        close_printer
            call "SETPRNT" ("AESRPT", " ",0%, 1%)
        return

        generate_report
                                         /* Check label file for data */ 
            gosub selecting_data
                                         /* Print sorted report       */

            call "SHOSTAT" ("PRINTING REPORT")
            CALL "PAUSE" ADDR(50%)

            gosub select_printer

            init(" ") sav_wrk_key$
            count% = 0% : cnt% = 0%
            lab_qty   = 0.0              /* Label Qty Counter Break  */ 
            aes_qty   = 0.0              /* AES Piece Counter  Break */   
            awd_qty   = 0.0              /* AWD Piece Counter  Break */
            aes_price = 0.0              /* AES Total Price at Break */

            lab_qty_tot   = 0.0          /* Report Lab Piece Total   */
            aes_qty_tot   = 0.0          /* Report AES Piece Total   */
            awd_qty_tot   = 0.0          /* Report AWD Piece Total   */
            aes_price_tot = 0.0          /* Report AES Price Total   */     

            wrk_key$ = all(hex(00))

        generate_report_next
            init(" ") aes_rec$, aes_raw$, aes_serial$, aes_scan_dte$,  ~
                      aes_scan_tme$, aes_scan_qty$, aes_scan_usr$,     ~
                      aes_awd_dte$, aes_awd_tme$, aes_awd_qty$,        ~
                      aes_awd_usr$, aes_delivery$, aes_status$, aes_po$,~
                      aes_item$, aes_qty_avail$, aes_recv$, aes_id$,   ~
                      aes_pan_no$, aes_vendor$, aes_rcv_dte$, aes_status1$,~
                      raw_d$

            read #10,key > wrk_key$, using GEN_1, wrk_key$, aes_rec$,  ~
                                                  eod goto generate_done
GEN_1:         FMT CH(44), CH(256)
            count% = count% + 1%

            if count% = 1% then sav_wrk_key$ = wrk_key$
                                               /* Rack Date          */   
            if sc_sort$ = "1" or sc_sort$ = "2" then                   ~
                                    aes_delivery$ = str(aes_rec$,31%,6%)

                                               /* AES Scan Date      */   
            if sc_sort$ = "3" or sc_sort$ = "4" then                   ~
                                    aes_delivery$ = str(aes_rec$,182%,6%)

                                               /* AWD Scan Date      */   
            if sc_sort$ = "5" or sc_sort$ = "6" then                   ~
                                    aes_delivery$ = str(aes_rec$,203%,6%)

                                               /* AWD Scan Date      */   
            if sc_sort$ = "7" or sc_sort$ = "8" then                   ~
                                    aes_delivery$ = str(aes_rec$,240%,6%)

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
                                               /* Raw Material Descr */
            raw_d$   = str(aes_rec$,62%,30%)
                                               /* Vendor Code        */
            aes_vendor$ = str(aes_rec$,94%,9%)
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
            convert aes_qty_avail to aes_qty_avail$, pic(######)

                                               /* Label Quantity     */
            aes_qty1 = 0.0
            get str(aes_rec$,103%,8%), using GEN_2, aes_qty1
            convert aes_qty1 to aes_qty1$, pic(######)

                                               /* AES Scan Date      */
            aes_scan_dte$ = str(aes_rec$,182%,6%)
            if aes_scan_dte$ <> " " then call "DATEFMT" (aes_scan_dte$)

                                               /* AES Scan Time      */
            aes_scan_tme$ = str(aes_rec$,188%,2%) & ":" &             ~
                            str(aes_rec$,190%,2%)

                                               /* AES Scan Qty       */
            get str(aes_rec$,192%,8%), using GEN_2, aes_scan_qty
GEN_2:         FMT PD(15,4)
            convert aes_scan_qty to aes_scan_qty$, pic(######)
                                               /* AES_Scan User      */
            aes_scan_usr$ = str(aes_rec$,200%,3%)

            gosub calculate_price 

                                               /* AWD Scan Date      */
            aes_awd_dte$ = str(aes_rec$,203%,6%)
            if aes_awd_dte$ <> " " then call "DATEFMT" (aes_awd_dte$)

                                               /* AWD Scan Time      */
            aes_awd_tme$ = str(aes_rec$,209%,2%) & ":" &              ~
                           str(aes_rec$,211%,2%)
 
                                               /* AWD Scan Qty       */
            get str(aes_rec$,213%,8%), using GEN_2, aes_awd_qty

            convert aes_awd_qty to aes_awd_qty$, pic(######)
                                               /* AWD Scan User      */
            aes_awd_usr$ = str(aes_rec$,221%,3%)
                                               /* Date Received      */
            aes_recv$ = " "
            aes_recv$ = str(aes_rec$,239%,1%)
                                               /* Date Received      */
            aes_rcv_dte$ = str(aes_rec$,240%,6%)
            if aes_rcv_dte$ <> " " then call "DATEFMT" (aes_rcv_dte$)

            gosub check_aes_status

            gosub print_detail

            goto generate_report_next
        generate_done

            gosub print_total_break

            gosub print_report_total

            print using L55320

            gosub close_printer

            gosub delete_work

        REM    call "FILEBGON" addr(#10)
        return

        check_aes_status
                                              /* For Detail Report */
            if aes_scan_usr$ = "???" and aes_awd_usr$ = "???"  then  ~
               aes_status$ = "O AES"

            if aes_scan_usr$ <> "???" and aes_awd_usr$ = "???"  Then ~
               aes_status$ = "O AWD"

            if aes_scan_usr$ <> "???" and aes_awd_usr$ <> "???" then ~
               aes_status$ = "CLOSE"

            if str(aes_rec$,94%,6%) = "DELETE" then                  ~
               aes_status$ = "DELET"

                                              /* For Summary Report */
            if aes_scan_usr$ = "???" and aes_awd_usr$ = "???"  then  ~
               aes_status1$ = "OAES"

            if aes_scan_usr$ <> "???" and aes_awd_usr$ = "???"  Then ~
               aes_status1$ = "OAWD"

            if aes_scan_usr$ <> "???" and aes_awd_usr$ <> "???" then ~
               aes_status1$ = "CLOS"

            if str(aes_rec$,94%,6%) = "DELETE" then                  ~
               aes_status1$ = "DELE"

        return

        print_header                        /* Page Header       */
          page_no% = page_no% + 1%
          if lcnt% <> 99% then print using L55320
          lcnt% = 0%
          print page
          print using L55320
          print using L55050, date$, rpt_time$, company$, sc_typed$
          print using L55090, prt_date$, dte1$, dte2$, print_title$,~ 
                              sc_sortd$, page_no%
          print using L55130, po1$, po2$, raw1$, raw2$
          print using L55360
          if detail% = 0% then print using L55210, prt_date$        ~
                          else print using L55410, prt_date$
 
          lcnt% = lcnt% + 6%
        return

        print_detail                        /* Line Item Detail  */

                                            /* Check for Break   */
          if sc_sort$ = "1" and str(sav_wrk_key$,1%,25%) <>        ~
                                str(wrk_key$,1%,25%) then          ~
                                gosub print_total_break

          if sc_sort$ = "2" and str(sav_wrk_key$,1%,31%) <>        ~
                                str(wrk_key$,1%,31%) then          ~
                                gosub print_total_break

          if sc_sort$ = "3" and str(sav_wrk_key$,1%,25%) <>        ~
                                str(wrk_key$,1%,25%) then          ~
                                gosub print_total_break

          if sc_sort$ = "4" and str(sav_wrk_key$,1%,31%) <>        ~
                                str(wrk_key$,1%,31%) then          ~
                                gosub print_total_break

          if sc_sort$ = "5" and str(sav_wrk_key$,1%,25%) <>        ~
                                str(wrk_key$,1%,25%) then          ~
                                gosub print_total_break

          if sc_sort$ = "6" and str(sav_wrk_key$,1%,31%) <>        ~
                                str(wrk_key$,1%,31%) then          ~
                                gosub print_total_break

          if sc_sort$ = "7" and str(sav_wrk_key$,1%,25%) <>        ~
                                str(wrk_key$,1%,25%) then          ~
                                gosub print_total_break

          if sc_sort$ = "8" and str(sav_wrk_key$,1%,31%) <>        ~
                                str(wrk_key$,1%,31%) then          ~
                                gosub print_total_break

                                            /* Count Racks         */
          cnt% = cnt% + 1%
                                            /* Label Quantity      */
          lab_qty = lab_qty + aes_qty1
                                            /* AES Quantity        */
          aes_qty = aes_qty + aes_scan_qty
                                            /* AWD Quantity        */
          awd_qty = awd_qty + aes_awd_qty
                                            /* AES Price Break Tot */
          aes_price = aes_price + aes_ln_price

                                            /* Label Quantity Total*/
          lab_qty_tot = lab_qty_tot + aes_qty1
                                            /* AES Quantity Total  */
          aes_qty_tot = aes_qty_tot + aes_scan_qty
                                            /* AWD Quantity total  */
          awd_qty_tot = awd_qty_tot + aes_awd_qty
                                            /* AES Price Total     */
          aes_price_tot = aes_price_tot + aes_ln_price


          if detail% = 0% then gosub print_a                        ~
                          else gosub print_b

        return

        print_a                      /* Detail Report Line Print */

          if lcnt% > 57% then gosub print_header
                                            /* Print Columns     */
          print using L55250

          raw_p$ = str(aes_raw$, 1%,14%)    /* Print length      */


          print using L55290, aes_delivery$, aes_po$, aes_id$,     ~
                aes_qty_avail$, raw_p$  , aes_serial$,             ~
                aes_scan_dte$, aes_scan_tme$, aes_scan_qty$,       ~
                aes_scan_usr$, aes_awd_dte$, aes_awd_tme$,         ~
                aes_awd_qty$, aes_awd_usr$, aes_status$, aes_ln_price$
          lcnt% = lcnt% + 2%
        return

        print_b                     /* Summary Report Line Print */

          if lcnt% > 57% then gosub print_header
                                            /* Print Columns     */
          print using L55420

          raw_p$ = str(aes_raw$, 1%,14%)    /* Print length      */


          print using L55430, aes_delivery$, aes_po$, aes_id$,     ~
                raw_p$, raw_d$, aes_serial$, aes_qty1$, aes_scan_qty$,~
                aes_awd_qty$, aes_rcv_dte$, aes_status1$, aes_ln_price1$
          lcnt% = lcnt% + 2%
        return

        print_total_break
                                           /* Check for Summary  */
          if detail% = 1% then goto print_total_break_b

          if lcnt% > 57% then gosub print_header

          init(" ") aes_qty$, awd_qty$, aes_price$, prt_value$, cnt$

          prt_value$ = str(sav_wrk_key$,7%,16%)

          convert cnt% to cnt$, pic(######)

          convert aes_qty to aes_qty$, pic(######)

          convert awd_qty to awd_qty$, pic(######)

          convert aes_price to aes_price$, pic($###,###.##)

          print using L55400

          print using L55380, prt_txt$, prt_value$, cnt$, aes_qty$, awd_qty$,~
                                    aes_price$

          lcnt% = lcnt% + 2%

          cnt%      = 0%
          aes_qty   = 0.0
          awd_qty   = 0.0
          aes_price = 0.0
          sav_wrk_key$  = wrk_key$
        return


        print_total_break_b

          if lcnt% > 57% then gosub print_header

          init(" ") aes_qty$, awd_qty$, aes_price$, prt_value$, cnt$, lab_qty$

          prt_value$ = str(sav_wrk_key$,7%,16%)

          convert cnt% to cnt$, pic(######)

          convert lab_qty to lab_qty$, pic(######)

          convert aes_qty to aes_qty$, pic(######)

          convert awd_qty to awd_qty$, pic(######)

          convert aes_price to aes_price$, pic(###,###.##)

          print using L55445

          print using L55440, prt_txt$, prt_value$, cnt$, lab_qty$, aes_qty$,~
                                                       awd_qty$, aes_price$

          lcnt% = lcnt% + 2%

          cnt%      = 0%
          lab_qty   = 0.0
          aes_qty   = 0.0
          awd_qty   = 0.0
          aes_price = 0.0
          sav_wrk_key$  = wrk_key$
        return

        print_report_total
                                          /* Check for Summary    */  
          if detail% = 1% then goto print_report_total_b

          if lcnt% > 57% then gosub print_header

          init(" ") aes_qty_tot$, awd_qty_tot$, aes_price_tot$, count$

          convert aes_qty_tot to aes_qty_tot$, pic(######)

          convert awd_qty_tot to awd_qty_tot$, pic(######)

          convert aes_price_tot to aes_price_tot$, pic($###,###.##)

          convert count% to count$, pic(######)

          print using L55400

          print using L55390, count$, aes_qty_tot$, awd_qty_tot$, ~
                                      aes_price_tot$

          lcnt% = lcnt% + 2%

        return


        print_report_total_b

          if lcnt% > 57% then gosub print_header

          init(" ") lab_qty_tot$, aes_qty_tot$, awd_qty_tot$,      ~
                                            aes_price_tot$, count$

          convert lab_qty_tot to lab_qty_tot$, pic(######)

          convert aes_qty_tot to aes_qty_tot$, pic(######)

          convert awd_qty_tot to awd_qty_tot$, pic(######)

          convert aes_price_tot to aes_price_tot$, pic(###,###.##)

          convert count% to count$, pic(######)

          print using L55445

          print using L55450, count$, lab_qty_tot$, aes_qty_tot$, ~
                                       awd_qty_tot$, aes_price_tot$

          lcnt% = lcnt% + 2%

        return

        selecting_data
             call "SHOSTAT" ("Selecting AES Rack Data")
            
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
                call "SHOSTAT" ("AES Rack Data Scanned ("&count$&")")

SEL_2:       read #1, using SEL_1, aes_rec$, eod goto selecting_data_done

SEL_3:       count% = count% + 1%
             convert count% to rhh$, pic(###)

             aes_recv$ = str(aes_rec$,239%,1%)
        
        REM    call "SHOSTAT" ("Found ---> " & rhh$)
        REM    stop
                                                  /* Check Report Type        */
             if sc_type$ = "A" then goto SEL_4    /* All AES Rack Data        */
                                                  /* Check Not Scanned AES    */
                if sc_type$ = "0" and aes_recv$ <> "0" then goto selecting_data_next
                                                  /* Check Not Scanned AWD    */
                if sc_type$ = "1" and aes_recv$ <> "1" then goto selecting_data_next
                                                  /* Check for Closed         */
                if sc_type$ = "2" and aes_recv$ <> "2" then goto selecting_data_next

SEL_4:
                                         /* Check Rack Production Date */
             if str(dte1$,1%,3%) = "ALL" then goto SEL_5
                                         /* New Date Logic             */
                if str(aes_rec$,31%,6%) > str(end_dte$,1%,6%) then       ~
                                               goto selecting_data_done



                                         /* new Date Logic             */
                                         /* Check PO Number            */
SEL_5:       if str(po1$,1%,3%) = "ALL" then goto SEL_6
                if str(aes_rec$,1%,16%) < po1$ or                        ~
                   str(aes_rec$,1%,16%) > po2$ then                      ~
                                              goto selecting_data_next
                                         /* Check Raw Material Number  */
SEL_6:       if str(raw1$,1%,3%) = "ALL" then goto SEL_7
                if str(aes_rec$,37%,25%) < raw1$ or                      ~
                   str(aes_rec$,37%,25%) > raw2$ then                    ~
                                              goto selecting_data_next 
SEL_7:  
        REM     call "SHOSTAT" ("In Report ----> " & rhh$ )
        REM     stop

        REM    Build sort key based on selection
               init(" ") wrk_key$
                                        /* 1st Rack Date, P.O No., Line Item   */
               if sc_sort$ <> "1" then goto SEL_7A
                  str(wrk_key$,1%,6%)   = str(aes_rec$,31%,6%) /* Rack Date    */
                  str(wrk_key$,7%,16%)  = str(aes_rec$,1%,16%) /* P.O. Number  */
                  str(wrk_key$,23%,3%)  = str(aes_rec$,17%,3%) /* PO Line Item */
                  str(wrk_key$,26%,3%)  = str(aes_rec$,20%,3%) /* Rack Id.     */
                  str(wrk_key$,29%,8%)  = str(aes_rec$,23%,8%) /* Serial Number*/
                  goto SEL_7H 

SEL_7A:                                 /* 2nd Rack Date, Raw Material Number  */       
               if sc_sort$ <> "2" then goto SEL_7B
                  str(wrk_key$,1%,6%)   = str(aes_rec$,31%,6%) /* Rack Date    */
                  str(wrk_key$,7%,25%)  = str(aes_rec$,37%,25%)/* Raw Material */
                  str(wrk_key$,32%,8%)  = str(aes_rec$,23%,8%) /* Serial Number*/
                  goto SEL_7H 

SEL_7B:                                 /* 3rd AES Scan Date, PO No, Line Item */
               if sc_sort$ <> "3" then goto SEL_7C
                  str(wrk_key$,1%,6%)   = str(aes_rec$,182%,6%)/* AES Scan Date*/
                  str(wrk_key$,7%,16%)  = str(aes_rec$,1%,16%) /* P.O. Number  */
                  str(wrk_key$,23%,3%)  = str(aes_rec$,17%,3%) /* PO Line Item */
                  str(wrk_key$,26%,3%)  = str(aes_rec$,20%,3%) /* Rack Id.     */
                  str(wrk_key$,29%,8%)  = str(aes_rec$,23%,8%) /* Serial Number*/
                  goto SEL_7H 
SEL_7C:                                 /* 4th AES Scan Date, Raw Material No. */ 
               if sc_sort$ <> "4" then goto SEL_7D
                  str(wrk_key$,1%,6%)   = str(aes_rec$,240%,6%)/* AES Scan Date*/
                  str(wrk_key$,7%,25%)  = str(aes_rec$,37%,25%)/* Raw Material */
                  str(wrk_key$,32%,8%)  = str(aes_rec$,23%,8%) /* Serial Number*/
                  goto SEL_7H 
SEL_7D:                                 /* 5th AWD Scan Date, PO No, Line Item */ 
               if sc_sort$ <> "5" then goto SEL_7E
                  str(wrk_key$,1%,6%)   = str(aes_rec$,203%,6%)/* AWD Scan Date*/
                  str(wrk_key$,7%,16%)  = str(aes_rec$,1%,16%) /* P.O. Number  */
                  str(wrk_key$,23%,3%)  = str(aes_rec$,17%,3%) /* PO Line Item */
                  str(wrk_key$,26%,3%)  = str(aes_rec$,20%,3%) /* Rack Id.     */
                  str(wrk_key$,29%,8%)  = str(aes_rec$,23%,8%) /* Serial Number*/
                  goto SEL_7H 
SEL_7E:                                 /* 6th AWD Scan Date, Raw Material No. */
               if sc_sort$ <> "6" then goto SEL_7F 
                  str(wrk_key$,1%,6%)   = str(aes_rec$,203%,6%)/* AWD Scan Date*/
                  str(wrk_key$,7%,25%)  = str(aes_rec$,37%,25%)/* Raw Material */
                  str(wrk_key$,32%,8%)  = str(aes_rec$,23%,8%) /* Serial Number*/
                  goto SEL_7H
SEL_7F:                                 /* 7th Posting Date, PO No, Line Item  */ 
               if sc_sort$ <> "7" then goto SEL_7G
                  str(wrk_key$,1%,6%)   = str(aes_rec$,240%,6%)/* Post Date    */
                  str(wrk_key$,7%,16%)  = str(aes_rec$,1%,16%) /* P.O. Number  */
                  str(wrk_key$,23%,3%)  = str(aes_rec$,17%,3%) /* PO Line Item */
                  str(wrk_key$,26%,3%)  = str(aes_rec$,20%,3%) /* Rack Id.     */
                  str(wrk_key$,29%,8%)  = str(aes_rec$,23%,8%) /* Serial Number*/
                  goto SEL_7H 
SEL_7G:                                 /* 8th Posting Date, Raw Material No.  */
                  str(wrk_key$,1%,6%)   = str(aes_rec$,240%,6%)/* Post Date    */
                  str(wrk_key$,7%,25%)  = str(aes_rec$,37%,25%)/* Raw Material */
                  str(wrk_key$,32%,8%)  = str(aes_rec$,23%,8%) /* Serial Number*/


 
SEL_7H:             
               gosub update_work
               goto selecting_data_next
        selecting_data_done
              if count% = 0% then goto SEL_8

        REM      convert count% to count$, pic(####)

        REM      call "SHOSTAT" ("Rack records found----> " & count$)
        REM      stop

        return
SEL_8:
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

        REM open_work_file
        REM     call "OPENCHCK" (#10, fs%(10%), f2%(10%), 500%, rslt$(10%))
        REM return

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        calculate_price                       /* (VBKLINES)          */
           aes_ln_price = 0.0
           init(" ") po_key$, unitprice$
           str(po_key$,1%,9%)   = aes_vendor$
           str(po_key$,10%,16%) = str(aes_rec$,1%,16%)
           str(po_key$,26%,3%)  = str(aes_rec$,17%,3%)
           read #4,key 0% = po_key$, using CAL_1, unitprice,            ~
                                                 eod goto CAL_2
CAL_1:        FMT POS(117), PD(15,7)

           convert unitprice to unitprice$, pic(######.##-)

           aes_ln_price = round(aes_scan_qty * unitprice, 2)

           convert aes_ln_price to aes_ln_price$, pic($###,###.##)

           convert aes_ln_price to aes_ln_price1$, pic(###,###.##)

CAL_2:  return


       open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#10,mode$, 500%, f2%)
            if f2% <> 0% then goto WORK_1
        return
WORK_1:     call "SHOSTAT" ("Error - Cannot Open (AESRPTWK)") : stop
        return
        delete_work
            call "FILEBGON" (#10)
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program

            end
