        REM *************************************************************~
            *  Program Name      - EWDPLP43                             *~
            *  Creation Date     - 04/24/2021                           *~
            *                                                           *~
            *  Description       - Production Stock Bulk Label Printng  *~
            *                                                           *~
            *  Code Tables Used  - PLAN DEPT, PLAN 100, PLANPARTS       *~
            *                                                           *~
            *  Subroutine Used   - EWDPLP43 (Print Bulk Label)          *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *04/24/2021! (New) Program -                          | RDB *~
            *************************************************************

        dim                                                              ~
            sc_sel$02,                   /* Display                    */~
            sc_sku$10,                   /* Screen SKU                 */~
            sc_desc$30,                  /* SKU Description 30 chars   */~
            sc_grid$30,                  /* Part grid display          */~
            sc_color$30,                 /* Color display              */~
            sc_qty$5,                    /* Screen Quantity to print   */~
                                                                         ~
            sku_key$16,                  /* Sku Key                    */~
            part$25,                     /* Part number                */~
            model$3,                     /* Sku Model                  */~
            upc$12,                      /* UPC                        */~
            l_desc$70,                   /* label desc                 */~
                                                                         ~
            lb_key$35,                   /* Record Key from LB         */~
            lb_rec$(4%)256,              /* LB Record Data             */~
            lb_prddate$10,               /* Prod. Date from LB         */~
            lb_dept$3,                   /* Dept. No. from LB          */~
            lb_seq$5,                    /* Sequence number from LB    */~
            lb_shift$2,                  /* Shift Code from LB         */~
            lb_load$5,                   /* Load Number from LB        */~
            lb_barcode$18,               /* Production Barcode         */~
            lb_foam$1,                   /* Foam Flag Y,N,0,1          */~
            lb_part$25,                  /* Part number                */~
                                                                         ~
            dt_key$23,                   /* Primary Key                */~
            tmp_bar$18,                  /* DT barcode                 */~
                                                                         ~
            filename$8,                  /* Used by EWDOPEN            */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            lb_wd_nrw$10,                /* Wide or Narrow Lowes label */~
            time$8,                      /* System time                */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            currdate$6,                  /* Current system date        */~
            st_date$6,                   /* Start -60 days find barcode*/~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3,                    /* Current User Id            */~
            sc_sel_p$1                   /* Printer Number             */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21                           
            apc$   = "Generate Bulk Stock Lowes Labels "
            pname$ = "EWDPLP43 - Rev: R1.00"

        REM *************************************************************


                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNDT ! Production Master Detail File            *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            * #5  ! EWDPRDLB ! Production Label Data File               *~
            * #6  ! BCKLINES ! Back Log Line Item File                  *~
            * #7  ! AWDSKUXR ! sku x-ref file                           *~
            * #8  ! APCPCMST !                                          *~
            * #63 ! BCKSUBPT ! New Sub Prt Number File                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  24,  keylen =  23,                     ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #5, "EWDPRDLB",                                       ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =   35,                    ~
                        alt key  1, keypos = 278, keylen =  23

            select #6, "BCKLINES",                                       ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #7,  "AWDSKUXR",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    1, keylen =  16,                     ~
                        alt key  1, keypos =  17, keylen =  20,          ~
                            key  2, keypos =  37, keylen =  45, dup
/*
 sku#       1- 16 ch(16)
 upc#      17- 36 ch(20)
 part      37- 61 ch(25)
 sub part  62- 81 ch(20)
 model     82- 87 ch(06)
 desc      88-157 ch(70)
filler    158-256 ch(99)
*/
            select #8,  "AWDPCMST",                                     ~
                        varc,     indexed,  recsize =   128,            ~
                        keypos =    9, keylen = 53,                     ~
                        alt key  1, keypos =   1, keylen =  8

            select #63, "BCKSUBPT",                                     ~
                        varc,     indexed,  recsize =  256,             ~
                        keypos =    1, keylen =  11,                    ~
                        alt key  1, keypos =  12, keylen =  11, dup,    ~
                            key  2, keypos =  23, keylen =  45, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")
            filename$ = "APCPLNDT" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDPRDLB" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDSKUXR" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDPCMST" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            currdate$ = date
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            call "TIME" (time$)
            
            extsearch% = 0%         /* flag for extending sales order search */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   2%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  2% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 2%)
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
                  if keyhit%  = 16% then       dataload
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% <> 1% then fieldnr% = 2%
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
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            convert sc_sel$ to sc_sel%, data goto L11180
L11180:     sc_sel_p$ = "1"
            if sc_sel% > 1 then sc_sel_p$ = "2"
            enabled% = 1%
        return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
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
         "Enter a SKU number?                                          ",~
         "Enter a Number of labels to be printed?                      "

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sc_sku$, sc_qty$, sc_desc$, ~
                      sc_grid$, sc_color$          
            
            lbl% = 0%
            days% = -60%
            
        return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload                                        
            call "SHOSTAT" ("Printing Production Labels...")
            cnt% = 0%
            call "DATE" addr("G+", currdate$, days%, st_date$, err%)
            call "DATUFMTC" (st_date$)

            been_here% = 0%

            lb_key$ = all(hex(00))
            str(lb_key$,1%,6%) = st_date$
          load_next_rec
            read #5, key > lb_key$, using L35040, lb_rec$(),            ~
                                                      eod goto load_done

            lb_key$ = str(lb_rec$(),1%,35%)
                                                    
            lb_prddate$ = str(lb_key$,1%,6%)
            lb_dept$    = str(lb_key$,12%,3%)
            lb_shift$   = str(lb_key$,15%,2%)
            lb_load$    = str(lb_key$,22%,5%)
            lb_seq$     = str(lb_rec$(),311%,5%)     
                                                    
            lb_barcode$ = str(lb_rec$(),278%,18%)
                                                    
            lb_foam$    = str(lb_rec$(),601%,1%)  
            lb_part$    = str(lb_rec$(),523%,25%)            

/* confirm model, color and grid match with sku table  */            
            if str(lb_part$,1%,4%) <> str(part$,1%,4%) then goto load_next_rec
            if str(lb_part$,7%,2%) <> str(part$,7%,2%) then goto load_next_rec
            
            gosub check_repair
            if lb_foam% = 1% then goto load_next_rec  /* Skip       */
            
            gosub lookup_sku
            if so% = 0% then goto load_next_rec

            cnt% = cnt% + 1%

            call "EWDPLO43" (been_here%, lb_rec$(), #4, #1, #6, #63, #7,    ~
                 #8, sc_sku$, upc$, sc_qty%, lb_wd_nrw$, err%)
                if err% <> 0% then gosub print_error
                
        load_done
            if cnt% = 0% then goto L29000
            gosub label_wide_narrow
                                                /* Return to run script */
            call "EWDPLO43" (been_here%, lb_rec$(), #4, #1, #6, #63, #7,    ~
                 #8, sc_sku$, upc$, sc_qty%, ld_wd_nrw$, 99%)
                if err% <> 0% then gosub print_error
                
            gosub load_results
            goto inputmode
L29000:
           errormsg$ = "(Error) SKU Order not found"
           gosub error_so_prompt

        return
            
        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
        check_repair                            /* (EWD014)             */
            lb_foam% = 0%
            if lb_foam$ = "Y" or lb_foam$ = "N" then goto L30110

            lb_foam% = 1%                       /* Skip Label Print     */
L30110: return
                                                
        lookup_sku
            init(" ") dt_key$, dt_sku$, dt_seq$, tmp_bar$
            so% = 0%

            str(dt_key$,1%,18%) = lb_barcode$
            str(dt_key$,19%,3%) = "000"
            str(dt_key$,22%,2%) = "00"
            read #1, key >= dt_key$, using L31000, tmp_bar$, dt_seq$, dt_sku$, ~
                                 eod goto L31999
L31000:      FMT POS(24), CH(18), POS(111), CH(05), POS(245), CH(09)

            if tmp_bar$ <> lb_barcode$ then goto L31999
            
            if dt_sku$ <> sc_sku$ then goto L31999
            
            so% = 1%
L31999:          
        return


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                                         /* (EWDPRDLB) */
L35040:     FMT 4*CH(256)

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
              on fieldnr% gosub L40160,          /* SKU                */~
                                L40170           /* Quantity           */


              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40170:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

/* CR2563  replace shift code with seq range */
L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "SKU Number :",                               ~
               at (03,25), fac(lfac$(1%)), sc_sku$              , ch(10),~
               at (03,40), fac(hex(84)),   sc_desc$             , ch(30),~
               at (04,40), fac(hex(84)),   sc_grid$             , ch(30),~
               at (05,40), fac(hex(84)),   sc_color$            , ch(30),~
                                                                         ~                                                           
               at (06,02), "Quantity to print :",                        ~
               at (06,25), fac(lfac$(2%)), sc_qty$              , ch(05),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L40420
                  call "PRNTSCRN"
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                      (15)Print Screen "
            pf$(3%) = "                                        " &        ~
                      "                      (16)PRINT LABELS "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                                       "
            pf$(3%) = "                                        " &        ~
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
            on fieldnr% gosub L50010,        /* SKU                    */~
                              L50050         /* Quantity               */

            return
                                                  /* (EWD011)         */
L50010: Rem Enter a SKU           
            gosub check_sku

            if sku% = 0% then goto L50015
            sc_desc$ = l_desc$
            
            if str(part$,7%,2%) = "00" then sc_grid$ = "No Grid" ~
                                       else sc_grid$ = "Grid   "
                                       
            gosub lookup_color

            return
            
L50015:    errormsg$ = "(Error) Invalid SKU"
           gosub error_prompt
           init(" ") sc_sku$, sc_desc$, sc_grid$, sc_color$
        return


L50050: Rem Enter a Valid Quantiy

           convert sc_qty$ to sc_qty%, data goto L50070
           if sc_qty% > 101 then goto L50070
        return

L50070:     errormsg$ = "(Error) Quantity too large"
            gosub error_prompt
            init(" ") sc_qty$
        return

        REM *************************************************************~
            *       V A L I D A T I O N   S U B R O U T I N E S         *~
            *************************************************************
        check_sku
          init(" ") sku_key$
          sku% = 0%

          sku_key$ = "X_LO" & sc_sku$ & "   "

          read #7, key = sku_key$, using L60000, model$, upc$, part$,~
                                           l_desc$,   eod goto L61000
L60000:     FMT POS(82), CH(03), POS(21), CH(12), POS(37), CH(25),   ~
                  POS(88), CH(70)
              sku% = 1%  
L61000:
        return              
             
        lookup_color
        
            init(" ") readkey$
            readkey$ = "COLOR    " & str(part$,4%,1%)
            read #4,key = readkey$, using L62025, sc_color$, eod goto L62050
L62025:           FMT POS(25), CH(30)            
       
L62050:        
        return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        error_so_prompt
           comp% = 2%
           hdr$     = "******* Searching for Sales Order  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press F16 To Expand Search, <ENTER> to Exit."
           
           if extsearch% = 1% then  ~
             msg$(3%) = "Press <ENTER> to Exit.                    "
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
           
           if comp% <> 0% and comp% <> 16% then goto error_so_prompt
           
           if comp% = 0% or extsearch% = 1% then goto L70000
           
           extsearch% = 1%
           days% = -365%
           goto dataload
        return
L70000:
           extsearch% = 0%           
           goto inputmode
        return
        
        open_error                                    /* (EWD004)        */
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return
                                                      /* (EWD004)        */
        load_results
           k% = 2%
           lbl% = sc_qty%
           hdr$     = "***** Label Generation Results *****"
           msg$(1%) = "This run generated xxxxx label(s)."
           msg$(2%) = "---"
           msg$(3%) = "Press <ENTER> to Acknowledge & Continue"
           convert lbl% to str(msg$(1%),20%,5%), pic(####0)
           if lbl% <> 0% then L64100
               msg$(1%) = "NO LABELS GENERATED!!!"
               str(msg$(3%),,13%) = " Press <PF16>"
L64100:    call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
           if lbl% <> 0% and k% <>  0% then load_results
           if lbl%  = 0% and k% <> 16% then load_results
           lbl% = 0%
        return

        label_wide_narrow
           k% = 2%
           lbl% = sc_qty%
           hdr$     = "***** Label Type Is *****"
           msg$(1%) = lb_wd_nrw$
           msg$(2%) = "---"
           msg$(3%) = "Press <ENTER> to Acknowledge & Continue"
           call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
           if lbl% <> 0% and k% <>  0% then load_results
           if lbl%  = 0% and k% <> 16% then load_results
        return
        
        print_error
            hdr$     = "***** Label Printing Error *****"
            msg$(1%) = "ERROR OCCURRED WHILE PRINTING LABELS!!!"
            msg$(2%) = "Return Code (EWDPLP43) = "
            msg$(3%) = "Press <ENTER> to Continue, <PF16> to Exit"

            convert err% to str(msg$(2%),26%,2%), pic(#0)
L64550:     k% = 2%
            call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
                if k%  =  0% then goto L64600
                if k% <> 16% then goto L64550
L64600:     return clear all
            goto inputmode

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program

        end

