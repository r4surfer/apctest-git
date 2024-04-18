        REM *************************************************************~
            *                                                           *~
            *   AAA   W   W  DDDD   RRRR   PPPP   TTTTT   000     1     *~
            *  A   A  W   W  D   D  R   R  P   P    T    0   0   11     *~
            *  AAAAA  W w W  D   D  RRRRR  PPPP     T    0   0    1     *~
            *  A   A  Ww wW  D   D  R  R   P        T    0   0    1     *~
            *  A   A  W   W  DDDD   R   R  P        T     000   11111   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * AWDRPT01 - AWD Custom Glass Receiving Report              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/20/04 ! New Program for (AES) - Last Mod Date    ! RHH *~
            * 03/29/05 ! (AWD001) Add Custom Glass Price to Report! RHH *~
            * 05/09/05 ! (AWD002) Fix Report window (2)           ! RHH *~
            * 01/01/06 ! (PAR000) CR347 Mod for new Sub Part No.  ! RHH *~
            *05/18/2015! (IM8022) mod for glass remakes           ! CMG *~
            *10/29/2019! CR2304  EWDGLSXX not used                ! RDB *~
            *************************************************************

        dim                                                              ~
            readkey$25, desc$30,         /* Gencodes lookup            */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            dte1$10, beg_dte$10,         /* Beg     Sales Order Date   */~
            dte2$10, end_dte$10,         /* End     Sales Order Date   */~
            scr_wind$1,                  /* Scanning Window for Report */~
            scr_wind_d$32,               /* Scanning Window Description*/~
            scan_date$6,                 /* Date Glass Scanned Complete*/~
            scan_time$4,                 /* Time Glass Scanned Complete*/~
            prod_date$10,                /* Glass Production Date      */~
            prod_seq$5,                  /* Glass production Seq. No.  */~
            glass_scan_date$10,          /* Glass Scan Date            */~
            glass_scan_time$8,           /* Glass scan time            */~
            glass_scan_usr$3,            /* User that scanned glass    */~
            glass_remake_no$3,           /* Glass remake number        */~
            glass_barcode$9,             /* Glass barcode - Short      */~ 
            prod_load$5,                 /* Load Number                */~
            prod_so$8,                   /* Sale Order Number          */~
            prod_model$3,                /* Production Model           */~
            prod_cl$1, prod_cl_d$6,      /* Window Color               */~
            prod_grid$2, prod_grid_d$5,  /* Glass Grid Code            */~
            prod_gls$2,                  /* Glass Type Code            */~
            glass_width$7,               /* Window Width               */~
            glass_height$6,              /* Window Height              */~
            reason$2,                    /* Remake Reson Code          */~
            reason_d$16,                 /* Reason Code Description    */~
            rm_key$23,                   /* Primary Key        (PAR000)*/~   
            rm_key1$27,                  /* Remake Alt 1 Key           */~
            rm_rec$(2%)192,              /* Glass Remake Record(PAR000)*/~
            sub_part$20,                 /* New Sub Part Number(PAR000)*/~
            wrk_key$44,                  /* Work File Key              */~
            contour$1,                   /* Contour Blank or 'C'       */~
            fgo$1,                       /* FGO Blank or 'F'           */~
            sh_type$1,                   /* Type of Grid 1,2,3         */~
            ct_key$12,                   /* Custom Glass Data Key      */~
            ct_price$10,                 /* Custom Glass Price (AWD001)*/~
            ct_price_tot$11,             /* custom Glass Total (AWD001)*/~
            company$40,                  /* For Report Company Name    */~
            print_title$40,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            custom$4, rm_tot$5,          /* Custom total an Non Custom */~
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
            apc$   = "(AWD)Custom Glass Receiving Report "
            pname$ = "AWDRPT01 - 01/01/2006"

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
            * #01 ! APCPLNGR ! Glass Sched/Remake File          (PAR000)*~
            * #03 ! GENCODES ! System Master Code Table Files           *~
            * #05 ! AWDGLSCT ! Custom Glass Detail File         (AWD001)*~
            * #10 ! AWDRPTWK ! Report Work File                 (PAR000)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

                                                       /* (PAR000)      */
            select #01,  "APCPLNGR",                                     ~
/*AWD083*/              varc,     indexed,  recsize =  512,              ~
                        keypos = 22,   keylen =  12,                     ~
                        alt key  1, keypos  =     7, keylen = 27,        ~
                            key  2, keypos  =     1, keylen = 33,        ~
                            key  3, keypos  =    13, keylen = 21

            select #03,  "GENCODES",                                     ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24
                                                            /* (AWD001) */
            select #05, "AWDGLSCT",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    1, keylen =   23,                    ~
                        alt key  1, keypos =   12, keylen =  12,         ~
                            key  2, keypos  = 154, keylen =  29
                                                            /* (AWD001) */
                                                            /* (PAR000) */
            select #10, "AWDRPTWK",                                      ~
                        varc,     indexed,  recsize =   428,             ~
                        keypos =    1, keylen =  44
                                                            /* (PAR000) */
            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#03, fs%(3%), f2%(3%),  0%, rslt$(3%))
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
         "Enter a Beginning and Ending Glass Scanning Date for Receiving Glass?",~
         "Enter a Valid Scanning Window (0 Thru 4) or <Return> = '0' ?         " 

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, dte1$, dte2$, beg_dte$,    ~
                      end_dte$, scr_wind$, scr_wind_d$, ct_price_tot$
                                                        /* (AWD001)     */

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
              on fieldnr% gosub L40180,      /* Beg/End Scanning Date */ ~
                                L40180       /* Valid Scanning Window */

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
               at (06,02), "Beg/End Scanning Date :",                    ~
               at (06,26), fac(lfac$(1%)), dte1$                , ch(10),~
               at (06,40), fac(lfac$(1%)), dte2$                , ch(10),~
                                                                         ~
               at (07,02), "Scanning Window (0-4) :",                    ~
               at (07,26), fac(lfac$(2%)), scr_wind$            , ch(01),~
               at (07,40), fac(lfac$(3%)), scr_wind_d$          , ch(32),~
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
            on fieldnr% gosub L50150,         /* Beg/End Scanning Date*/ ~
                              L50200          /* Valid Scanning Window*/
            return


L50150: REM Beginning and Ending Scanning Date             DTE1$,DTE2$
            if dte1$ <> " " then goto L50160
               dte1$ = date

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
L50185:        errormsg$ = "(Error) Invalid Scanning Date."
               init(" ") dte1$, dte2$, beg_dte$, end_dte$
               gosub error_prompt
        return
L50195:        errormsg$ = "(Error) Invalid beginning date?"
               init(" ") dte1$, dte2$, beg_dte$, end_dte$
               gosub error_prompt
        return

L50200: REM Valid Scanning window                          scr_wind$,scr_wind_d$

           init(" ") readkey$, scr_wind_d$, beg_wind$, end_wind$ 
           beg_wind%, end_wind% = 0%

           if scr_wind$ <> " " then goto L50205
              scr_wind$ = "0"
L50205:         

           str(readkey$,1%,9%)   = "SHAPESCAN"
           str(readkey$,10%,15%) = scr_wind$
           read #3,key = readkey$, using L50210, scr_wind_d$, eod goto L50250
L50210:        FMT POS(25), CH(32)

              beg_wind$ = str(scr_wind_d$,1%,4%)
              end_wind$ = str(scr_wind_d$,6,4%)

              convert beg_wind$ to beg_wind%, data goto L50250

              convert end_wind$ to end_wind%, data goto L50250

        return
L50250:   errormsg$ = "(Error) Invalid scanning window (0 Thru 4)? "
          gosub error_prompt
          init(" ") scr_wind$, scr_wind_d$   
        return           

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L55050: %!########## ########                        ####################~
        ~####################                                    AWDRPT01:~
        ~!

L55090: %!Scan DTE Beg: ##########      End: ##########  ################~
        ~########################                              Page: #####~
        ~!

L55130: %!Scan Window: #  ##############################                 ~
        ~                                                                 ~
        ~!

                                                   /* Detail Header      */
L55210: %!Prod Date !SeqNo! Barcode !Rmk!Mod!SalesOrd!Color !GD!Size !GS!~
        ~C!F!Load !Win Wid!Win HT!Scan  Date!ScanTime!USR!Cd!Custom  Price~
        ~!

L55250: %!----------!-----!---------!---!---!--------!------!--!-----!--!~
        ~-!-!-----!-------!------!----------!--------!---!--!-------------~
        ~!

L55290: %!##########!#####!#########!###!###!########!######!##!#####!##!~
        ~#!#!#####!#######!######!##########!########!###!##! ##########  ~
        ~!

L55320: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~+

L55360: %!---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~!
                                                   /* End of Report      */
                                                   /* (AWD001)           */

L55400: %! Atrium Windows & Doors Receiving Total:########### Signature: ~
        ~ ________________________________________  Date: ___/___/______  ~
        ~!

L55410: %! Custom Glass Received: ####    Non: ####           Signature: ~
        ~ ________________________________________  Date: ___/___/______  ~
        ~!

L55420: %!                                                               ~
        ~                                                                 ~
        ~!

 
        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            call "SETPRNT" ("AWDRPT", " ",2500%, 0%)
            select printer (134)

            page_no% = 0%
            lcnt%    = 99%
                                             
            print_title$ = "Custom Glass Truck Receiving Report  "
            call "FMTTITLE" (print_title$, " ", 12%)
            date$ = date  :  call "DATFMTC" (date$)
            call "TIME" (rpt_time$)
            company$ = "ATRIUM Windows and Doors"
            call "FMTTITLE" (company$, " ", 12%)
        return

        close_printer
            call "SETPRNT" ("AWDRPT", " ",0%, 1%)
        return

        generate_report
                                         /* Check label file for data */ 
            gosub selecting_data
                                         /* Print sorted report       */

            call "SHOSTAT" ("PRINTING REPORT")
            CALL "PAUSE" ADDR(50%)

            gosub select_printer

            custom% = 0%                 /* Custom Glass Count        */
            rm_tot% = 0%                 /* Non-Custom glass          */
            wrk_key$ = all(hex(00))

        generate_report_next
            init(" ") prod_date$, prod_seq$, glass_scan_date$, glass_scan_time$,  ~
                      glass_scan_usr$, glass_remake_no$, glass_barcode$, prod_so$,~
                      prod_model$, prod_cl$, prod_cl_d$, prod_grid$, prod_grid_d$,~
                      prod_gls$, glass_width$, glass_height$, prod_load$,    ~
                      contour$, fgo$, reason$, reason_d$, ct_price$, rm_rec$()
                                               /* (PAR000)            */  
                                               /* (AWD001)            */            
            read #10,key > wrk_key$, using GEN_1, wrk_key$, rm_rec$(), ~
                                                  eod goto generate_done
GEN_1:         FMT CH(44), 2*CH(192)

                                               /* For Glass received  */
                                               /* from Custom         */
            if str(rm_rec$(),65%,1%) = "7" then custom% = custom% + 1%  ~
                                            else rm_tot% = rm_tot% + 1%

                                               /* Production Date     */ 
            prod_date$ = str(rm_rec$(),1%,6%)
            call "DATFMTC" (prod_date$)
                                               /* Production Seq. No. */ 
            prod_seq$ = str(rm_rec$(),242%,5%)
                                               /* Glass Scan Date     */
            glass_scan_date$ = str(rm_rec$(),7%,6%)
            call "DATFMTC" (glass_scan_date$)
                                               /* Glass Scan Time     */
            glass_scan_time$ = str(rm_rec$(),14%,8%)
                                               /* Glass Scan Userid   */
            glass_scan_usr$  = str(rm_rec$(),58%,3%)
                                               /* Glass Barcode       */
            glass_barcode$   = str(rm_rec$(),22%,9%)
                                               /* Glass Remake No.    */
            glass_remake_no$ = str(rm_rec$(),31%,3%)
                                               /* Production Sales Ord*/
            prod_so$         = str(rm_rec$(),163%,8%)
                                               /* Production Model    */
            prod_model$      = str(rm_rec$(),72%,3%)
                                               /* Production Load     */
            prod_load$ = str(rm_rec$(),67%,5%)
                                               /* Production Color    */
            prod_cl$         = str(rm_rec$(),128%,1%)
            gosub get_color
                                               /* Get Grid Code       */
            gosub get_grid_size
            if sh_type$ = "1" then prod_grid_d$ = " 1    "
            if sh_type$ = "2" then prod_grid_d$ = " 3/4  "
            if sh_type$ = "3" then prod_grid_d$ = " 5/8  "

            if sh_type$ = "5" then prod_grid_d$ = " 18m  "
 
                                               /* Get Glass Code      */
            prod_gls$        = str(rm_rec$(),77%,2%)
                                               /* Window Width        */
            glass_width$     = str(rm_rec$(),150%,7%) 
                                               /* Window Height       */  
            glass_height$    = str(rm_rec$(),157%,6%)
                                               /* Remake Reason Code  */
            reason$ = str(rm_rec$(),34%,2%)
            gosub get_reason

            if str(rm_rec$(),65%,1%) = "7" then str(prod_seq$,1%,1%)  = "C" ~
                                            else str(prod_seq$,1%,1%) = "N"

            if scr_wind$ <> "0" and str(rm_rec$(),65%,1%) <> "7" then        ~
                                            generate_report_next
            gosub lookup_price 

            gosub print_detail

            goto generate_report_next
        generate_done

                                             /* (AWD001)             */
            convert ct_price_tot to ct_price_tot$, pic($##,###.##-)
  
            convert custom% to custom$, pic(####)

            convert rm_tot% to rm_tot$, pic(####)

                                             /* End of Report Totals */
            print using L55320
            print using L55420
            print using L55400, ct_price_tot$
            print using L55420
            print using L55410, custom$, rm_tot$
            print using L55320

            gosub close_printer
            call "FILEBGON" addr(#10)

        return
                                            /* (PAR000)              */

        print_header                        /* Page Header       */
          page_no% = page_no% + 1%
          if lcnt% <> 99% then print using L55320
          lcnt% = 0%
          print page
          print using L55320
          print using L55050, date$, rpt_time$, company$
          print using L55090, dte1$, dte2$, print_title$, page_no%
          print using L55130, Scr_wind$, scr_wind_d$
          print using L55360
          print using L55210
          lcnt% = lcnt% + 6%
        return

        print_detail                        /* Line Item Detail  */

          if lcnt% > 56% then gosub print_header
                                            /* Print Columns     */
          print using L55250
                                            /* print Detail      */
          gosub print_a
 
        return

        print_a                             /* Detail Data Line   */
          print using L55290, prod_date$, prod_seq$, glass_barcode$,   ~
                              glass_remake_no$, prod_model$, prod_so$, ~
                              prod_cl_d$, prod_grid$, prod_grid_d$,    ~
                              prod_gls$, contour$, fgo$, prod_load$,   ~
                              glass_width$, glass_height$, glass_scan_date$,~
                              glass_scan_time$, glass_scan_usr$,       ~
                              reason$, ct_price$

          lcnt% = lcnt% + 2%
        return

        selecting_data
             call "SHOSTAT" ("Selecting Glass Received Data")

             count% = 0%

             rm_key1$ = all(hex(00))
                                         /* (PAR000)                  */
             str(rm_key1$,1%,6%) = str(beg_dte$,1%,6%)
             str(rm_key1$,7%,1%) = "2"
SEL_1:
             read #1,key 1% > rm_key1$, using SEL_2, rm_rec$(),        ~
                                                eod goto selecting_data_done
SEL_2:          FMT 2*CH(192)
                                         /* Set Alt Key               */
             rm_key1$ = str(rm_rec$(),7%,27%)
             if mod(count%,25%) <> 0 then goto SEL_3
                convert count% to count$, pic(######)
                call "SHOSTAT" ("Glass Panels Checked ("&count$&")")

SEL_3:       if str(rm_key1$,1%,6%) > str(end_dte$,1%,6%) then        ~
                                               goto selecting_data_done
                                         /* Only Completed Glass      */   
             if str(rm_rec$(),13%,1%) <> "2" then goto SEL_1

             count% = count% + 1%
                                         /* Only Department (043)     */
             if str(rm_rec$(),249%,3%) <> "043" then goto SEL_1

                                         /* Check Scan Window         */
               gosub check_scan_window
                                         /* Check Next Panel          */
               if scan_wind% = 0% then goto SEL_1  

               init(" ") wrk_key$
                                        /* Set Production Date        */
               str(wrk_key$,1%,6%)  = str(rm_rec$(),1%,6%) 
                                        /* Set Sequence Number        */
               str(wrk_key$,7%,5%)  = str(rm_rec$(),242%,5%)
                                        /* Remake Barcode             */
               str(wrk_key$,26%,9%) = str(rm_rec$(),22%,9%)  
                                        /* Remake Number              */ 
               str(wrk_key$,35%,3%) = str(rm_rec$(),31%,3%)
                                        /* Set Scan Date              */
               str(wrk_key$,12%,6%) = str(rm_rec$(),7%,6%)
                                        /* Set Scan time              */
               str(wrk_key$,18%,8%) = str(rm_rec$(),14%,8%)               

               gosub update_work
               goto SEL_1
        selecting_data_done
              if count% = 0% then goto SEL_6

        return
SEL_6:
            errormsg$ = "No Data Found"
            gosub error_prompt
        return 
                                         /* (PAR000)                  */
        update_work

            write #10, using UPD_1, wrk_key$, rm_rec$(), eod goto UPD_2

UPD_1:         FMT CH(44), 2*CH(192)
        return
UPD_2:
            call "SHOSTAT" ("(Error) Barcode Number - " & str(wrk_key$,26%,12%) )
            stop
        return
                                         /* (PAR000)                 */
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
                                               /* (PAR000)            */
        check_scan_window
           init(" ") scan_time$, scan_date$
           scan_wind% = 0%
           scan_time% = 0%

           scan_date$ = str(rm_rec$(),7%,6%)
           scan_time$ = str(rm_rec$(),14%,2%) & str(rm_rec$(),17%,2%)

           convert scan_time$ to scan_time%, data goto CHECK_1
CHECK_1:
           if str(rm_rec$(),20%,2%) = "PM" then scan_time% = scan_time% + 1200%
                                                      /* Special Test  */
                                                      /* for Scan Wind */
                                                      /* (4)           */

                                                      /* Adjustment for*/
                                                      /* after Midnight*/
                                                      /* before 01:00AM*/
           if str(rm_rec$(),20%,2%) = "AM" and scan_time% > 1159% then      ~
                                          scan_time% = scan_time% - 1200%
                                                      /* New Test      */ 
           if str(rm_rec$(),20%,2%) = "PM" and scan_time% > 2359% then      ~
                                          scan_time% = scan_time% -1200%

           if scr_wind$ = "4" then goto CHECK_3

           if scan_time% < beg_wind% then goto CHECK_2 /* Not in Window */

           if scan_time% > end_wind% then goto CHECK_2 /* Not in Window */
              
              scan_wind% = 1%                         /* In window     */
CHECK_2:

        return 
CHECK_3:

          if scan_date$ <> str(beg_dte$,1%,6%) then goto CHECK_4
                                                     /* Check begin    */
                                                     /* Time Window    */
             if scan_time% < beg_wind% then return   /* Not in Window  */

             goto CHECK_5                            /* Within Window  */ 
CHECK_4:
                                                     /* Check End      */
                                                     /* Time Window    */
          if scan_date$ <> str(end_dte$,1%,6%) then return
             if scan_time% > end_wind% then return   /* not in Window  */

         
CHECK_5:                                             /* Within Window  */
              scan_wind% = 1%
        return
                                                     /* (PAR000)       */ 
        get_color
           init(" ") readkey$, desc$
           str(readkey$, 1%,9%)  = "COLOR    "
           str(readkey$,10%,15%) = prod_cl$
           read #3,key = readkey$, using GET_1, desc$,                  ~
                                       eod goto get_color_done
GET_1:        FMT POS(25), CH(30)
           prod_cl_d$ = str(desc$,6%,6%)                                  
        get_color_done
 
        return        

        get_reason
           init(" ") readkey$, desc$
           str(readkey$, 1%,9%)  = "PLAN REMK"
           str(readkey$,10%,15%) = reason$
           read #3,key = readkey$, using GET_1, desc$,                  ~
                                       eod goto get_reason_done
           reason_d$ = str(desc$,1%,16%)                                  
        get_reason_done
 
        return        
                                       /* (PAR000)        */
        get_grid_size
                                       /* Check for 'FGO' */
           fgo$ = " "
           if str(rm_rec$(),135%,1%) = "6" then fgo$ = "F"

                                       /* Grid Code       */
           prod_grid$ = str(rm_rec$(),131%,2%)

           gosub lookup_sub_part       /* (PAR000)        */

           sh_type$ = "3"              /* 5/8 Inch Grid   */ 

                                       /* 3/4 Inch Grid   */
           if str(sub_part$,2%,1%) = "2" then sh_type$ = "2"

                                       /* 1 Inch Grid     */
/* (AWD003) */
REM           if str(sub_part$,1%,1%) = "2" then sh_type$ = "1"
           if str(sub_part$,1%,1%) = "2" and               ~
                   str(sub_part$,2%,1%) = "3"  then sh_type$ = "1"
           if str(sub_part$,1%,1%) = "2" and               ~
                   str(sub_part$,2%,1%) = "1"  then sh_type$ = "5"

                                       /* Contour Grid Y/N*/
           contour$ = "N"
           if sh_type$ = "1" then contour$ = "Y"

        return                         /* (PAR000)        */

                                       /* (AWD001)        */  
        lookup_price
           init(" ") ct_key$, ct_price$
           ct_price = 0.0
           str(ct_key$,1%,9%)  = glass_barcode$
           str(ct_key$,10%,3%) = glass_remake_no$

           read #05, key 1% = ct_key$, using PR1, ct_price, eod goto PR2
PR1:          FMT POS(127), PD(14,4)

PR2:
           convert ct_price to ct_price$, pic($#,###.##-)
           
           ct_price_tot = ct_price_tot + ct_price 
 
        return
                                       /* (AWD001)        */
                                       /* (PAR000)        */
        lookup_sub_part
           init(" ") rm_key$, sub_part$
           str(rm_key$,1%,9%) = glass_barcode$       /* Glass Barcode      */
           str(rm_key$,10%,3%)= glass_remake_no$     /* Remake Number      */
           read #1,key 0% = rm_key$, using PR3, sub_part$,                  ~
                                                            eod goto PR4
PR3:            FMT POS(255), CH(20)
           return
PR4:
           sub_part$ = "00000                    "
        return
                                       /* (PAR000)        */              
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program

            end
