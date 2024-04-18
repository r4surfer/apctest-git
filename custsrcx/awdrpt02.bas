        REM *************************************************************~
            *                                                           *~
            *   AAA   W   W  DDDD   RRRR   PPPP   TTTTT   000    222    *~
            *  A   A  W   W  D   D  R   R  P   P    T    0   0  2   2   *~
            *  AAAAA  W w W  D   D  RRRRR  PPPP     T    0   0    2     *~
            *  A   A  Ww wW  D   D  R  R   P        T    0   0   2      *~
            *  A   A  W   W  DDDD   R   R  P        T     000   22222   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * AWDRPT02 - AWD Custom Glass Receiving Report              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/29/04 ! New Program for (AWD) - Last Mod Date    ! RHH *~
            * 01/01/06 ! (PAR000) Mod New Sub Part No Change      ! RHH *~
            *          !                                          !     *~
            *************************************************************

        dim                                                              ~
            readkey$25,                  /* Gencodes lookup            */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            dte1$10, beg_dte$10,         /* Beg     Sales Order Date   */~
            dte2$10, end_dte$10,         /* End     Sales Order Date   */~
            sort_code$1, sort_code_d$25, /* Sort Code Selection/Descr  */~
            shape_code$2, shape_code_d$25,/* shape Code Selection      */~
            shape_pct$2, ct_pct_flg$9,   /* Custom Glass Pct Flag      */~  
            sh$(30%)8,                   /* Shape Abbreviations        */~
            s$(20%)60, ss$(20%)60,       /* Special Shape Selections   */~ 
            prod_date$10,                /* Glass Production Date      */~
            prod_seq$5,                  /* Glass production Seq. No.  */~
            ct_barcode$9,                /* Glass Barcode              */~
            ct_rnum$3,                   /* Glass Remake Number        */~
            ct_mod$3,                    /* MFG Model Code             */~
            ct_so$8,                     /* Sales Order Number         */~
            ct_shape$3, ct_shp$3,        /* Shape Code and Shape Abbrev*/~
            ct_shp1$2,                   /* Lookup Shape Code          */~ 
            ct_grid$2,                   /* Glass Grid Code            */~
            ct_grid_sze$3,               /* Glass Grid Size            */~
            ct_glass$2,                  /* Glass Code                 */~
            ct_width$9,                  /* window Width               */~
            ct_height$9,                 /* window Height              */~
            ct_wind_prc$10,              /* Window Price               */~
            ct_price$10,                 /* Custom Glass/Grid Price    */~
            ct_pct$7,                    /* Price Pct of Sale          */~
            ct_wood$1,                   /* wood Surround (Y/N)        */~
            ct_fgo$1, ct_fgo1$3,         /* FGO Product                */~
            ct_descr$13,                 /* Pricing Description        */~
            ct_tot1$10, ct_tot2$10,      /* Pricing totals             */~
            ct_tot3$7, count$4,          /* percent of total for report*/~  
            ct_key$23,                   /* Primary key                */~    
            ct_rec$256,                  /* Custom Detail Record       */~
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
            apc$   = "Custom Glass Pricing Analysis Rpt  "
            pname$ = "AWDRPT02 - 10/29/2004"

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
            * #01 ! AWDGLSCT ! Custom Glass Detail Data                 *~
            * #03 ! GENCODES ! System Master Code Table Files           *~
            * #10 ! AWDGLSWK ! Report Work File                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #01, "AWDGLSCT",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    1, keylen =   23,                    ~
                        alt key  1, keypos =   12, keylen =  12,         ~
                            key  2, keypos  = 154, keylen =  29

            select #03,  "GENCODES",                                     ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

                                          /* Sort = 44, Data Rec = 256 */
            select #10, "AWDGLSWK",                                      ~
                        varc,     indexed,  recsize =   300,             ~
                        keypos =    1, keylen =  44

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(1%), f2%(1%),  0%, rslt$(1%))
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

            sh$( 1%) = "00 - PW "     :     sh$(11%) = "51 - CIR"
            sh$( 2%) = "01 - RPT"     :     sh$(12%) = "60 - EYE"
            sh$( 3%) = "02 - LPT"     :     sh$(13%) = "63 - CA "
            sh$( 4%) = "03 - RTP"     :     sh$(14%) = "64 - HR "
            sh$( 5%) = "04 - LTP"     :     sh$(15%) = "64 - ELP" 
            sh$( 6%) = "05 - RTG"     :     sh$(16%) = "66 - RQR"
            sh$( 7%) = "06 - LTG"     :     sh$(17%) = "67 - LQR"
            sh$( 8%) = "07 - TRI"     :     sh$(18%) = "70 - REB"
            sh$( 9%) = "15 - DPT"     :     sh$(19%) = "71 - LEB"
            sh$(10%) = "25 - OCT"     :     sh$(20%) = "73 - RCA" 
                                      :     sh$(21%) = "74 - LCA"

            sh_max% = 21% 
                                                                           
            ss$( 1%) = "00 - Picture Window     51 - Circle                 "
            ss$( 2%) = "01 - Irreg Right Pent   60 - Eyebrow                "
            ss$( 3%) = "02 - Irreg Left Pent    63 - Colonial Arch          "
            ss$( 4%) = "03 - Trapezoid Right    64 - Half Rnd/Elliptical    "
            ss$( 5%) = "04 - Trapezoid Left     66 - Right Quarter Round    " 
            ss$( 6%) = "05 - Right Triangle     67 - Left Quarter Round     "
            ss$( 7%) = "06 - Left Triangle      70 - 1/2 Right Eyebrow      "
            ss$( 8%) = "07 - ISOC Triangle      71 - 1/2 Left Eyebrow       "
            ss$( 9%) = "15 - Doghouse Pent      73 - 1/2 Right Colonial Arch"
            ss$(10%) = "25 - Octagon            74 - 1/2 Left Colonial Arch " 

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 4%
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
            if fieldnr% < 1% or fieldnr% > 4% then editpg1
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
         "Enter a Beginning and Ending Production Date for Custom Glass?",~
         "Enter Sort Code, 1=Date/Seq., 2=Shape Code, 3=Pct of Price?   ",~
         "Enter Special Shape Selection, 'AL' or Special Shapes Code?   ",~
         "Flag Custom Prices Greater than or Equal to XX Percent, Default 99?"  

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, dte1$, dte2$, beg_dte$,    ~
                      end_dte$, sort_code$, sort_code_d$, shape_code$,   ~
                      shape_code_d$, shape_pct$, s$()


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
                                L40180,      /* Sort Code Selection   */ ~
                                L40180,      /* Shape Code Selection  */ ~
                                L40180       /* Shape Percent Flag    */ 

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
               at (06,02), "Beg/End Production Date :",                  ~
               at (06,30), fac(lfac$(1%)), dte1$                , ch(10),~
               at (06,50), fac(lfac$(1%)), dte2$                , ch(10),~
                                                                         ~
               at (07,02), "Sort Code (1,2, or 3)   :",                  ~
               at (07,30), fac(lfac$(2%)), sort_code$           , ch(01),~
               at (07,50), fac(hex(84)), sort_code_d$           , ch(25),~
                                                                         ~
               at (08,02), "Shape Sel 'AL' or Code  :",                  ~
               at (08,30), fac(lfac$(3%)), shape_code$          , ch(02),~
               at (08,50), fac(hex(84)), shape_code_d$          , ch(25),~
                                                                         ~
               at (09,02), "Flag Custom Price >= XX:",                   ~
               at (09,30), fac(lfac$(4%)), shape_pct$           , ch(02),~
               at (09,50), "Percent Greater Than or Equal",              ~
                                                                         ~
               at (11,02), fac(hex(84)), s$(1%)                 , ch(60),~
               at (12,02), fac(hex(84)), s$(2%)                 , ch(60),~
               at (13,02), fac(hex(84)), s$(3%)                 , ch(60),~
               at (14,02), fac(hex(84)), s$(4%)                 , ch(60),~
               at (15,02), fac(hex(84)), s$(5%)                 , ch(60),~
               at (16,02), fac(hex(84)), s$(6%)                 , ch(60),~
               at (17,02), fac(hex(84)), s$(7%)                 , ch(60),~
               at (18,02), fac(hex(84)), s$(8%)                 , ch(60),~
               at (19,02), fac(hex(84)), s$(9%)                 , ch(60),~
               at (20,02), fac(hex(84)), s$(10%)                , ch(60),~
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
            on fieldnr% gosub L50150,        /* Beg/End Production Date*/~
                              L50200,        /* Sort Selection 1,2,3   */~
                              L50300,        /* Special Shape Selection*/~
                              L50400         /* Custom Price Percent Flg*/ 

            return


L50150: REM Beginning and Ending Production Date             DTE1$,DTE2$
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

L50200: REM Valid Sort Selection                      sort_code$, sort_code_d$

           init(" ") sort_code_d$

           if sort_code$ = " " then sort_code$ = "1"

           if sort_code$ < "1" or sort_code$ > "3" then goto L50250

           sort_code% = 0%
           convert sort_code$ to sort_code%, data goto L50250

           if sort_code$ = "1" then sort_code_d$ = "Sort By Date Sequence    "
           if sort_code$ = "2" then sort_code_d$ = "Sort By Shape Code       "
           if sort_code$ = "3" then sort_code_d$ = "Sort By Pct of Price S-L "  

           copy ss$() to s$()
            
        return
L50250:   errormsg$ = "(Error) Invalid Sort Selection (1,2, or 3)? "
          gosub error_prompt
          init(" ") sort_code$, sort_code_d$   
        return           

L50300: REM Special Shapes Selection Code          shape_code$, shape_code_d$
           init(" ") readkey$, shape_code_d$
           if shape_code$ <> " " then goto L50305
              shape_code$ = "AL"
L50305: 
           if shape_code$ <> "AL" then goto L50310
              shape_code_d$ = "(ALL) Special Shape Codes "
              init(" ") s$()

              return
L50310:
           str(readkey$,1%,9%)   = "PLNCONFIG"
           str(readkey$,10%,15%) = shape_code$
           read #3,key = readkey$, using L50320, shape_code_d$,eod goto L50350
L50320:        FMT POS(25), CH(32)

           init(" ") s$()

        return
L50350:   errormsg$ = "(Error) Invalid Shape code Selection? "
          gosub error_prompt
          init(" ") shape_code$, shape_code_d$   
        return           
 
L50400: REM Special Shapes Flag Percentage Value   shape_pct$
           if shape_pct$ <> "  " then goto L50410
              shape_pct$ = "99"
L50410: 
           shape_pct = 0.0
           convert shape_pct$ to shape_pct, data goto L50420

        return
L50420:   errormsg$ = "(Error) Invalid Flag Percentage Value, (00-99)?"
          gosub error_prompt
          init(" ") shape_pct$   
        return           
 
        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L55050: %!########## ########                        ####################~
        ~####################                                    AWDRPT02 ~
        ~!

L55090: %!Prod DTE Beg: ##########      End: ##########  ################~
        ~########################                             Page: ##### ~
        ~!

L55130: %!Sort Code: #  #########################        Special Shape Co~
        ~de: ## #########################       Flag Percentage Value: ## ~
        ~!

                                                   /* Detail Header      */
L55210: %!Prod Date !SeqNo! Barcode !Rmk!Mod!SalesOrd!Shp!GD!Sze!GS!Win W~
        ~idth!WinHeight!Window Prc!Custom Prc! Gls Pct !W/S!FGO!   TEXT   ~
        ~!

L55250: %!----------!-----!---------!---!---!--------!---!--!---!--!-----~
        ~----!---------!----------!----------!---------!---!---!----------~
        ~!

L55290: %!##########!#####!#########!###!###!########!###!##!###!##!#####~
        ~####!#########!##########!##########!#########! # !###!##########~
        ~!

L55320: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~+

L55340: %!Number of Shapes: ####    Report Totals                        ~
        ~              !##########!##########! ####### !                  ~
        ~!

L55360: %!---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~!


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            call "SETPRNT" ("AWDRPT", " ",2500%, 0%)
            select printer (134)

            page_no% = 0%
            lcnt%    = 99%
                                                                
            print_title$ = "Custom Glass Pricing Analysis Report "
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
                                         /* Check Custom Detail File  */ 
            gosub selecting_data
                                         /* Print sorted report       */

            call "SHOSTAT" ("PRINTING REPORT")
            CALL "PAUSE" ADDR(50%)
            
            ct_wind_prc_tot = 0.0
            ct_price_tot    = 0.0
            count%          = 0%

            gosub select_printer

            wrk_key$ = all(hex(00))

        generate_report_next
            init(" ") prod_date$, prod_seq$, ct_barcode$, ct_rnum$, ct_mod$,~
                      ct_so$, ct_shape$, ct_shp$, ct_grid$, ct_grid_sze$,   ~
                      ct_glass$, ct_width$, ct_height$, ct_wind_prc$,       ~
                      ct_price$, ct_pct$, ct_pct_flg$, ct_wood$, ct_fgo$,   ~
                      ct_fgo1$, ct_descr$,ct_tot1$, ct_tot2$, ct_tot3$

            read #10,key > wrk_key$, using GEN_1, wrk_key$, ct_rec$,   ~
                                                  eod goto generate_done
GEN_1:         FMT CH(44), CH(256)

                                               /* Production Date     */ 
            prod_date$ = str(ct_rec$,1%,6%)
            call "DATFMTC" (prod_date$)
                                               /* Production Seq. No. */ 
            prod_seq$   = str(ct_rec$,7%,5%)
                                               /* Glass Barcode       */
            ct_barcode$ = str(ct_rec$,12%,9%)
                                               /* Glass Remake No.    */
            ct_rnum$    = str(ct_rec$,21%,3%)
                                               /* Production Sales Ord*/
            ct_so$      = str(ct_rec$,24%,8%)
                                               /* Production Model    */
            ct_mod$     = str(ct_rec$,32%,3%)
                                               /* Get Grid Code       */
            ct_grid$    = str(ct_rec$,47%,2%)
                                               /* Get Grid size       */
            ct_grid_sze$= str(ct_rec$,44%,3%)
                                               /* Glass Code          */
            ct_glass$   = str(ct_rec$,49%,2%)
                                               /* Window width        */
            ct_width$   = str(ct_rec$,51%,9%) 
                                               /* Window Height       */
            ct_height$  = str(ct_rec$,60%,9%)
                                               /* Special Shape Name  */
            gosub get_shape_abbrev

                                               /* Get Window Price    */
            get str(ct_rec$,138%,8%), using GEN_2, ct_wind_prc
GEN_2:          FMT PD(14,4)
            convert ct_wind_prc to ct_wind_prc$, pic(######.##-)

            ct_wind_prc_tot = ct_wind_prc_tot + ct_wind_prc

                                               /* Custom Price        */ 
            get str(ct_rec$,127%,8%), using GEN_2, ct_price

            convert ct_price to ct_price$, pic(######.##-)
                                               
            ct_price_tot = ct_price_tot + ct_price
 
                                               /* Price Percent       */
            get str(ct_rec$,146%,8%), using GEN_2, ct_pct

            convert ct_pct to ct_pct$, pic(###.##-)
                                              /* Wood Surround Y/N    */
            ct_wood$    = str(ct_rec$,184%,1%)
                                              /* Pricing Description  */
            ct_descr$   = str(ct_rec$,185%,10%)
            if ct_pct < .01 then ct_descr$ = "No  Charge"

                                              /* FGO product          */
            ct_fgo$ = str(ct_rec$,97%,1%)
            ct_fgo1$ = "   "
            if ct_fgo$ = "F" then ct_fgo1$ = "FGO"
                                              /* Check Percentage Flag*/
            str(ct_pct_flg$,1%,1%) = " "
            str(ct_pct_flg$,2%,7%) = ct_pct$
            str(ct_pct_flg$,9%,1%) = " "
            if ct_pct < shape_pct then goto GEN_3
                                              /* Flag Custom Price    */
               str(ct_pct_flg$,1%,1%) = "*"
               str(ct_pct_flg$,2%,7%) = ct_pct$
               str(ct_pct_flg$,9%,1%) = "*"
GEN_3:

            gosub print_detail

            goto generate_report_next
        generate_done

            convert ct_wind_prc_tot to ct_tot1$, pic(######.##-)

            convert ct_price_tot to ct_tot2$,    pic(######.##-)

            ct_tot3 = round( (ct_price_tot/ct_wind_prc_tot) * 100, 2)

            convert ct_tot3 to ct_tot3$, pic(###.##-)

            convert count% to count$, pic(####)

            print using L55360
            print using L55340, count$, ct_tot1$, ct_tot2$, ct_tot3$
            print using L55320

            gosub close_printer
            call "FILEBGON" addr(#10)

        return

        print_header                        /* Page Header       */
          page_no% = page_no% + 1%
          if lcnt% <> 99% then print using L55320
          lcnt% = 0%
          print page
          print using L55320
          print using L55050, date$, rpt_time$, company$
          print using L55090, dte1$, dte2$, print_title$, page_no%
          print using L55130, sort_code$, sort_code_d$, shape_code$,~
                              shape_code_d$, shape_pct$
          print using L55360
          print using L55210
          lcnt% = lcnt% + 6%
        return

        print_detail                        /* Line Item Detail  */
          count% = count% + 1%

          if lcnt% > 54% then gosub print_header
                                            /* Print Columns     */
          print using L55250
                                            /* print Detail      */
          gosub print_a
 
        return

        print_a                             /* Detail Data Line   */
          print using L55290, prod_date$, prod_seq$, ct_barcode$,  ~
                              ct_rnum$, ct_mod$, ct_so$, ct_shp$,  ~
                              ct_grid$, ct_grid_sze$, ct_glass$,   ~
                              ct_width$, ct_height$, ct_wind_prc$, ~
                              ct_price$, ct_pct_flg$, ct_wood$,    ~
                              ct_fgo1$, ct_descr$

          lcnt% = lcnt% + 2%
        return

        selecting_data
             call "SHOSTAT" ("Selecting Custom Detail Data")

             count% = 0%

             ct_key$=all(hex(00))

             str(ct_key$,1%,6%) = str(beg_dte$,1%,6%)
SEL_1:
             read #1,key > ct_key$, using SEL_2, ct_rec$,             ~
                                          eod goto selecting_data_done
SEL_2:          FMT CH(256)
                                         /* Set Alt Key               */
             ct_key$ = str(ct_rec$,1%,23%)
             if mod(count%,25%) <> 0 then goto SEL_3
                convert count% to count$, pic(####)
                call "SHOSTAT" ("Glass Detail Checked ("&count$&")")

SEL_3:       if str(ct_key$,1%,6%) > str(end_dte$,1%,6%) then        ~
                                               goto selecting_data_done
             count% = count% + 1%
                                         /* Check selections          */
             gosub check_sort_data
                                         /* Check Next Panel          */
             if sort_select% = 0% then goto SEL_1  


               gosub update_work
               goto SEL_1
        selecting_data_done
              if count% = 0% then goto SEL_6

        return
SEL_6:
            errormsg$ = "No Data Found"
            gosub error_prompt
        return 

        update_work

            write #10, using UPD_1, wrk_key$, ct_rec$, eod goto UPD_2

UPD_1:         FMT CH(44), CH(256)
        return
UPD_2:
            call "SHOSTAT" ("(Error) Barcode Number - " & str(ct_rec$,12%,12%) )
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

        check_sort_data
           wrk_key$ = all(hex(00))

           sort_select% = 0%
           if sort_code$ <> "1" then goto CHK_1
                                              /* Sort Selection (1)  */
                                              /* Production Date/Seq.*/
              str(wrk_key$,1%,23%) = str(ct_rec$,1%,23%)
              goto CHK_3
 
CHK_1:
           if sort_code$ <> "2" then goto CHK_2
                                              /* Sort Selection (2)  */
                                              /* By Special Shape Code*/
              str(wrk_key$,1%,3%)  = str(ct_rec$,41%,3%)
              str(wrk_key$,4%,23%) = str(ct_rec$,1%,23%)
              goto CHK_3

CHK_2:
                                              /* Sort Selection (3)   */
                                              /* Pct of Price from the*/
                                              /* Smallest to Largest  */
          ct_pct = 0.0
          get str(ct_rec$,146%,8%), using GEN_2, ct_pct

          convert ct_pct to str(wrk_key$,1%,8%), pic(###.###-)

          str(wrk_key$,9%,23%) = str(ct_rec$,1%,23%)
                                             /* Check Shape Selection*/
CHK_3:  
          if shape_code$ = "AL" then goto CHK_4
          
          gosub get_shape_abbrev
                                             /* Skip Shape           */
          if shape_code$ <> ct_shp1$ then return
                                             /* Selected Shape       */
CHK_4:
              sort_select% = 1%
        return

        get_shape_abbrev
            init(" ") ct_shp$, ct_shape$, ct_shp1$

  
            ct_shape$ = str(ct_rec$,41%,3%)
 
            ct_shp1% = 0%
            convert ct_shape$ to ct_shp1%, data goto CT_SHP1
CT_SHP1:
            convert ct_shp1% to ct_shp1$, pic(00)

            for jj% = 1% to sh_max%

                if ct_shp1$ = str(sh$(jj%),1%,2%) then goto CT_SHP2

            next jj%
        return
CT_SHP2:
        ct_shp$ = str(sh$(jj%),6%,3%)

        if str(ct_grid$,1%,1%) = "C" then ct_shp$ = "ELP"

        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program

            end
