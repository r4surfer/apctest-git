        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDPLN82                             *~
            *  Creation Date     - 02/01/2011                           *~
            *                                                           *~
            *  Description       - SCREEN WARRENTY LOOKUP               *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN-----+--------------WHAT----------------------+-WHO-*~
            * 02/01/2011 | New Program                            ! DES *~
            *************************************************************

        dim                                                              ~
            sc_search$8,                 /* Search Value               */~
            sc_key$12, sc_rec$(2%)256,   /* Re-make key and record     */~
            screensz$23,                 /* Last Scann Date            */~
            h1$10, h2$4, h3$25, h4$20,   /* Display Headers            */~
            h5$4, h6$8, h7$8, h8$8,      /*                            */~
            h9$7, h10$3, title$44,       /*                            */~
            wandchar$1,color$30,         /* Wand Character - Scanner   */~
            genkey$24,                   /* Search text                */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8, sub_part$20,         /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            dsp_msg$79,                  /* Screen Display Message     */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim dt$(24%)160, tt$(7%)21,       /* Analysis Display           */~
            cc$(24%)1                    /* Selection                  */
 
        dim f2%(17%),                    /* = 0 if the file is open    */~
            f1%(17%),                    /* = 1 if READ was successful */~
            fs%(17%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(20%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = " Screen Warranty/Look-Up Routine "
            pname$ = "EWDPLN82 - Rev: R1.00"

        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            * #17 ! AWDPLNSR ! Screen File                              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #17, "AWDPLNSR",                                       ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =   42, keylen =  12,                     ~
                        alt key  1, keypos =   7, keylen =  47,          ~
                            key  2, keypos = 163, keylen =  13,          ~
                            key  3, keypos =   1, keylen =  53,          ~
                            key  4, keypos = 205, keylen =  12, dup 

             call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#17, fs%(17%), f2%(17%),  0%, rslt$(17%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   1%
                gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
/*                    if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160  */
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
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
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = 1%
REM         fieldnr% = cursor%(1%) - 2%
REM         if fieldnr% < 1% or fieldnr% > 2% then editpg1
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
            *             P R O C E S S   D A T A                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************
                                                 /* Search for Glass   */ 
        process_data                           
            dt% = 0% 
            call "SHOSTAT" ("Searching for Screen Data")
      
             sc_key$ = str(sc_rec$(),42,12)
            if str(sc_key$,1,8) <> sc_search$ then goto process_data_done 
        process_data_next
/* ???? */  if str(sc_rec$(),34,5) = "99999" then goto read_again        
            dt% = dt% + 1%
            if dt% > 7% then dt% = 7%
            sc_key$ = str(sc_rec$(),42,12)
            
            get #17, using datefmt, dat
datefmt:    FMT POS(01), PD(11,1)
REM         convert dat to dat$, pic (0000/00/00)
            convert dat to dat$, pic (00000000)
            dat$ = str(dat$,5,2) & "/" & str(dat$,7,2) & "/" & str(dat$,1,4)
REM         screensz$ = str(sc_rec$(),118,10) & " x " & str(sc_rec$(),128,10)
            screensz$ = " "
            test1$ = str(sc_rec$(),118,10)
            test2$ = str(sc_rec$(),128,10)
            for l% = 1% to 10%
             x% = 117% + l%
             y% = 118% + l%
             if str(sc_rec$(),x%,1) <> " " then                   ~
              screensz$ = screensz$ & str(sc_rec$(),x%,1)
             if str(sc_rec$(),x%,1) > " " and str(sc_rec$(),y%,1) = " " then ~
              screensz$ = screensz$ & "@"                     
           next l%      
           screensz$ = screensz$ & "x@"                     
           for l% = 1% to 10%
             x% = 127% + l%
             y% = 128% + l%
             if str(sc_rec$(),x%,1) <> " " then                   ~
              screensz$ = screensz$ & str(sc_rec$(),x%,1)
             if str(sc_rec$(),x%,1) = " " and str(sc_rec$(),y%,1) > " " then ~
             screensz$ = screensz$ & "@"                     
           next l%      
           for l% = 1% to 20%
             if str(screensz$,l%,1) = "@" then str(screensz$,l%,1) = " "
           next l%
           screensz$ = str(screensz$,2,20)
           if str(screensz$,1,1) = "x" then init(" ") screensz$
           init(" ") mesh$
           mesh$ = "None "
           sub_part$ = str(sc_rec$(),278,20)
           if str(sub_part$,15,1) = "1" then mesh$ = "Std. "   
           if str(sub_part$,15,1) = "2" then mesh$ = "WIRED"   
            init(" ") dt$(dt%)
            if dat$ = "00/00/0000" then dat$ = "  /  /    "
            str(dt$(dt%),01,10) = dat$ 
            str(dt$(dt%),12,05) = str(sc_rec$(),54,5)   /* seq        */
            str(dt$(dt%),18,25) = str(sc_rec$(),138,25) /* part       */
            str(dt$(dt%),123,20) = screensz$             /* sreen size */
            str(dt$(dt%),65,05) = mesh$                 /* mesh type  */
            str(dt$(dt%),71,08) = str(sc_rec$(),177,8)  /* S.O.       */
/*----------------------------------------------------------------------*/
            screensz$ = " "
            test1$ = str(sc_rec$(),098,10)
            test2$ = str(sc_rec$(),108,10)
            for l% = 1% to 10%
             x% = 97% + l%
             y% = 98% + l%
             if str(sc_rec$(),x%,1) <> " " then                   ~
              screensz$ = screensz$ & str(sc_rec$(),x%,1)
             if str(sc_rec$(),x%,1) > " " and str(sc_rec$(),y%,1) = " " then ~
              screensz$ = screensz$ & "@"                     
            next l%      
            screensz$ = screensz$ & "x@"                     
            for l% = 1% to 10%
             x% = 107% + l%
             y% = 108% + l%
             if str(sc_rec$(),x%,1) <> " " then                   ~
              screensz$ = screensz$ & str(sc_rec$(),x%,1)
             if str(sc_rec$(),x%,1) = " " and str(sc_rec$(),y%,1) > " " then ~
              screensz$ = screensz$ & "@"                     
            next l%      
            for l% = 1% to 20%
             if str(screensz$,l%,1) = "@" then str(screensz$,l%,1) = " "
            next l%
            screensz$ = str(screensz$,2,20)
            if str(screensz$,1,1) = "x" then init(" ") screensz$
            str(dt$(dt%),44,20) = screensz$             /* sreen size */

            get #17, using datefmt2, dat
datefmt2:   FMT POS(299), PD(11,1)
REM         convert dat to dat$, pic (0000/00/00)
            convert dat to dat$, pic (00000000)
            dat$ = str(dat$,5,2) & "/" & str(dat$,7,2) & "/" & str(dat$,1,4)
            if dat$ = "00/00/0000" then dat$ = "  /  /    "
            str(dt$(dt%),80,10) = dat$ 

            str(dt$(dt%),91,05) = "     "                                  
REM         str(dt$(dt%),93,01) = str(sc_rec$(),62,1)   /* color      */
            genkey$ = "COLOR    " & str(sc_rec$(),62,1)
            read #4,key = genkey$,using genfmt, color$,             ~
              eod goto genfmt
            str(dt$(dt%),97,25) = str(color$,1,30)
genfmt:     FMT pos(30), CH(25)
REM         str(dt$(dt%),150,09) = str(sc_rec$(),234,9)   /* sku        */
            str(dt$(dt%),92,1) = str(sc_rec$(),64,1)   /* type       */
            str(dt$(dt%),94,1) = str(sc_rec$(),65,1)   /* hf         */
read_again:
            read #17, key > sc_key$, using L50005, sc_rec$(),       ~
             eod goto process_data_done
            sc_key$ = str(sc_rec$(),42,12)
            get #17, using datefmt, dat
            if str(sc_key$,1,8) = sc_search$ then goto process_data_next
            
        process_data_done

            gosub display_analysis
        return clear all
        goto inputmode


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
         "Enter a Valid Search Value?                                  "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sc_search$, dt$(), cc$(), ~
                      tt$()

        return

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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
REM           on fieldnr% gosub L40120,          /* Search Selection   */~
REM                             L40120           /* Search Code        */
              gosub L40120
              goto L40130

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40120:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40130:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), fac(hex(84)), "Enter Warranty Id.    :",      ~
               at (03,27), fac(lfac$(1%)), sc_search$           , ch(08),~
               at (03,38), fac(lfac$(2%)), wandchar$            , ch(01),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40450
                  call "PRNTSCRN"
                  goto L40130

L40450: close ws
        call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf1

        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(2),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1),19,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (10)Search Data        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffff0affffff0e0f1000)
            return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       " 
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return


        REM *************************************************************~
            *           D I S P L A Y   S U M M A R Y   S C R E E N     *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_analysis
            k% = 0%
L41000:     gosub set_pf2   
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
                                                                         ~
               at (02,66), fac(hex(84)), pageno$                , ch(14),~
                                                                         ~
               at (03,02), fac(hex(84)), "Warranty Id. ",      ~
               at (03,17), fac(hex(84)), sc_search$           ,~
               at (03,34), fac(hex(84)), title$                 , ch(44),~
                                                                         ~
               at (04,13), fac(hex(a4))  , h2$                 , ch(4), ~
               at (05,02), fac(hex(a4))  , h1$                  , ch(10),~
               at (05,13), fac(hex(a4))  , h2b$                 , ch(6), ~
               at (05,19), fac(hex(a4))  , h3$                  , ch(25),~
               at (05,45), fac(hex(a4))  , h4$                  , ch(20),~
               at (05,66), fac(hex(a4))  , h5$                  , ch(4), ~
               at (05,72), fac(hex(a4))  , h6$                  , ch(8), ~
                                                                         ~
               at (06,02), fac(hex(84))  ,str(dt$(k% + 1%),01,79), ch(79),~
               at (07,02), fac(hex(84))  ,str(dt$(k% + 1%),80,79), ch(79),~
               at (08,02), fac(hex(84))  ,str(dt$(k% + 2%),01,79), ch(79),~
               at (09,02), fac(hex(84))  ,str(dt$(k% + 2%),80,79), ch(79),~
               at (10,02), fac(hex(84))  ,str(dt$(k% + 3%),01,79), ch(79),~
               at (11,02), fac(hex(84))  ,str(dt$(k% + 3%),80,79), ch(79),~
               at (12,02), fac(hex(84))  ,str(dt$(k% + 4%),01,79), ch(79),~
               at (13,02), fac(hex(84))  ,str(dt$(k% + 4%),80,79), ch(79),~
               at (14,02), fac(hex(84))  ,str(dt$(k% + 5%),01,79), ch(79),~
               at (15,02), fac(hex(84))  ,str(dt$(k% + 5%),80,79), ch(79),~
               at (16,02), fac(hex(84))  ,str(dt$(k% + 6%),01,79), ch(79),~
               at (17,02), fac(hex(84))  ,str(dt$(k% + 6%),80,79), ch(79),~
               at (18,02), fac(hex(84))  ,str(dt$(k% + 7%),01,79), ch(79),~
               at (19,02), fac(hex(84))  ,str(dt$(k% + 7%),80,79), ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   dsp_msg$             , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~  
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L41040             /* First    */
L41020:           k% = 0%
                  goto L41000

L41040:        if keyhit% <> 3% then goto L41080             /* Last      */
L41060:           x% = int(val_max% / 14%)
                  k% = (x%*14%)
                  if (k% + 1%) > val_max% then k% = k% - 14% 
                  goto L41000

L41080:        if keyhit% <> 4% then goto L41100             /* Previous */
                  if k% < 15% then goto L41020
                  k% = k% - 14%
                  if k% <= 1% then goto L41020
                  goto L41000

L41100:        if keyhit% <> 5% then goto L41150             /* Next     */
                  k% = k% + 14%
                  if k% < val_max% then goto L41000
                  goto L41060

L41150:        if keyhit% <> 15 then goto L41155
                  call "PRNTSCRN"
                  goto L41000

L41155:        if keyhit% <> 16% then goto L41000

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return clear all
        goto inputmode

        set_pf2
            init(" ") h1$, h2$, h3$, h4$, h5$, h6$, h7$, h8$,h9$, h10$
            dsp_msg$=                                                     ~
             "                                                               "
            str(dsp_msg$,65%,15%) = "Total [ xxxxx ]"
            convert dt% to str(dsp_msg$,73%,5%), pic(#####)

            title$ = " Display of Screen Warranty records "
            h1$="Date/Orig."                 
            h2$="Seq. "                      
            h2b$="Typ/Hf"                     
            h3$="Part Number/Color"   
            h4$="Screen Size/CB Size"      
            h5$="Mesh"                 
            h6$="S.O.    "                  

            pageno$ = "              "             /* k% = Array Values */
  

            pf$(1) = "                                        " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Display" 
            pfkeys$ = hex(ffffffffffffffffffffffffff0f1000)
            gosub check_screen
            return

        check_screen
            if val_max% > 14% then goto L41860
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L41860:      if k% >= 14% then goto L41870
                gosub no_first
                gosub no_prev
L41870:      if (k% + 14%) <= val_max% then goto L41880
                gosub no_last
L41880:      if k% <= (val_max% - 14%) then goto L41900
                gosub no_next
L41900: return
        no_first
            str(pf$(1%),1%, 9%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
            str(pf$(1%),20%, 9%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last
            str(pf$(2%),1%,9%)   = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
            str(pf$(3%),1%,12%)  = " " : str(pfkeys$,4%,1%) = hex(ff)
        return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            init(" ") errormsg$
REM         on fieldnr% gosub L50000,                 /* sc_status$   */  ~
REM                           L50100                  /* sc_type$     */
            gosub L50000
            return

L50000: Rem Search Warranty Number                     sc_search$
            init(" ") sc_key$
        str(sc_key$,1,8) = sc_search$
            read #17, key >= sc_key$, using L50005, sc_rec$(), eod goto L50010 
L50005:    FMT 2*CH(256)
            sc_key$ = str(sc_rec$(),42,12)
            if str(sc_key$,1,8) <> sc_search$ then goto L50010
            gosub process_data                           
        return
L50010:     init(" ") sc_search$
            errormsg$ = "(Error) Invalid Warranty Id Code?"
            gosub error_prompt
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

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

     
        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
          
        end

