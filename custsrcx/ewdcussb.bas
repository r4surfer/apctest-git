        REM *************************************************************~
            *           Hidden Key (PF(30)) to clear buckets            *~
            *                                                           *~      
            *  Program Name      - EWDCUSSB                             *~
            *  Creation Date     - 11/29/99                             *~
            *  Last Modified Date- 12/29/00                             *~
            *  Written By        - Roy H. Hoffman                       *~
            *  Modifications By  - Christie Gregory                     *~ 
            *                                                           *~
            *  Description       - New Subroutine to create additional  *~
            *                      customer information. New Info.      *~
            *                      related to Marketing.                *~
            *                      New File (CUSTLINK)                  *~
            *                                                           *~
            *  Code Tables Used  - ELLISON01 - Private Label Definition *~
            *                      APC WOOD  - Mull Code Table          *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/29/99 ! (New) Program -                          ! RHHS *~
            * 12/29/00 ! Mod to zero out all buckets for beginning! CMG *~
            *          !     of new year. (EWD001)                !     *~
            * 12/18/02 ! Mod to allow to delete customers.(EWD002)! CMG *~
            *************************************************************

        dim                                                              ~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            lk_priv$2, lk_priv_d$30,     /* Private Label Code         */~ 
            lk_cuscode$9, lk_cuscode_d$30,/* Customer Code             */~
            lk_ad_allowance$10,          /* Advertising Allowance      */~
            lk_samples_u$8,              /* Samples/Displays Tot Units */~
            lk_samples$10,               /* Samples/Displays Tot $$$$  */~
            lk_marketing_u$8,            /* Literature/Apparel Tot Units*/~
            lk_marketing$10,             /* Literature/Apparel Tot $$$$*/~       
            lk_samples_up$8,             /* Samp/Disp Tot Units Purched*/~
            lk_samples_p$10,             /* Samp/Disp Tot $$$ Purchased*/~
            lk_marketing_up$8,           /* Lit/App Tot Units Purchased*/~
            lk_marketing_p$10,           /* Lit/App Tot $$$$ Purchased */~       
            lk_samples%(80%),            /* Sample Units by Code       */~
            lk_samples(80%),             /* Sample Dollars by Code     */~
            lk_marketing%(40%),          /* Marketing Units by Code    */~
            lk_marketing(40%),           /* Marketing Dollars by Code  */~
            lk_filler1$250,              /* Filler Area (1)            */~
            lk_filler2$250,              /* Filler Area (2)            */~
            lk_filler3$250,              /* Filler Area (3)            */~
            lk_filler4$35,               /* Filler Area (4)            */~
            screen_msg$45,               /* Private Label Text         */~
            cd$(80%)2, cd_d$(80%)30,     /* Code Table Values          */~
            val1$(80%)8, val2$(80%)10,   /* Units and $$'s             */~
            pageno$16,                   /* Page Number                */~
            header$42,                   /* Display Header             */~ 
            lk_key$11,                   /* CUSTLINK Key               */~
            filename$8,                  /* Used by EWDOPEN            */~ 
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            time$8,                      /* System time                */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            fs%(5%),                     /*                            */~
            f2%(5%),                     /*                            */~ 
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "Addit. Customer Sales and Marketing Info"
            pname$ = "EWDCUSSB - Rev: R7.00"

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
            * #1  ! GENCODES ! System Master Code Table Files           *~
            * #2  ! CUSTOMER ! Customer Master File                     *~
            * #3  ! CUSTLINK ! Addit Customer Sales and Marketing Info  *~
            * #4  ! CUSDETAL ! Customer Detail for Samp/Disp/Lit        *~  
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #2,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key 1,  keypos = 10,   keylen =  30, dup,    ~
                            key 2,  keypos = 424,  keylen =   9, dup,    ~
                            key 3,  keypos = 771,  keylen =   9, dup,    ~
                            key 4,  keypos = 780,  keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #3,  "CUSTLINK",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    3, keylen =   9,                     ~
                        alt key 1,  keypos =  1,   keylen =  11

            select #4,  "CUSDETAL",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  28,                     ~
                        alt key  1, keypos =   35, keylen =  11,         ~
                            key  2, keypos =   29, keylen =  17

            call "SHOSTAT" ("Opening Files, One Moment Please")
                                 
            filename$ = "GENCODES" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "CUSTOMER" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error

            call "OPENCHCK" (#3,  fs%(3%), f2%(3%), 100%, " ")
                                                    /* Use to Create   */
            call "OPENCHCK" (#4,  fs%(4%), f2%(4%), 100%, " ")


        REM    filename$ = "CUSTLINK"
        REM    call "EWDOPEN" (#2, filename$, err%)
        REM    if err% <> 0% then gosub open_error

        REM    filename$ = "CUSDETAL"
        REM    call "EWDOPEN" (#4, filename$, err%)
        REM    if err% <> 0% then gosub open_error

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
            call "TIME" (time$)

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
                      if keyhit%  = 30% then gosub clear_values   /*(EWD001) */
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
                  if keyhit%  = 12% then       delete_it   /* (EWD001) */
                  if keyhit%  = 16% then       dataput
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 3%
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
         "Enter a Valid Customer Code?                                 ",~
         "Enter the applicable Advertising Allowance Assoc. with Cust? "

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
            init(" ") errormsg$, inpmessage$, lk_priv$, lk_priv_d$,      ~
                      lk_cuscode$, lk_cuscode_d$, lk_ad_allowance$,      ~
                      lk_samples_u$, lk_samples$, lk_marketing_u$,       ~
                      lk_marketing$, lk_filler1$, lk_filler2$, lk_filler3$,~
                      lk_filler4$, screen_msg$, lk_samples_up$,          ~
                      lk_samples_p$, lk_marketing_up$, lk_marketing_p$
 
            mat lk_samples%   = zer
            mat lk_samples    = zer
            mat lk_marketing% = zer
            mat lk_marketing  = zer

            lk_samples%   = 0%
            lk_samples    = 0.0
            lk_marketing% = 0%
            lk_marketing  = 0.0

            rec% = 0%
        return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload

            mat lk_samples%   = zer
            mat lk_samples    = zer
            mat lk_marketing% = zer
            mat lk_marketing  = zer

            lk_samples%       = 0%
            lk_samples        = 0.0
            lk_marketing%     = 0%
            lk_marketing      = 0.0
 
            lk_ad_allowance = 0.0
            rec% = 0%
            init(" ") lk_key$
            lk_key$ = lk_cuscode$
            read #3,key = lk_key$, eod goto L30000

        data_load
            get #3, using L35040,lk_priv$,       /* Private Label Code */~
                                 lk_cuscode$,    /* customer Code      */~
                                 lk_ad_allowance,/* Advertising Allowan*/~
                                 lk_samples%(),  /* Sample/Display Unit*/~
                                 lk_samples(),   /* Sample/Display $$$ */~
                                 lk_marketing%(),/* Literature/App Unit*/~
                                 lk_marketing(), /* Literature/App $$'s*/~
                                 lk_samples%,    /* Samp/Disp Un Purch */~
                                 lk_samples,     /* Samp/Disp $$$ Purch*/~
                                 lk_marketing%,  /* Lit/App Units Purch*/~
                                 lk_marketing,   /* Lit/App $$$ Purchas*/~
                                 lk_filler1$,    /* Filler Area        */~
                                 lk_filler2$,    /* Filler Area        */~
                                 lk_filler3$,    /* Filler Area        */~
                                 lk_filler4$     /* Filler Area        */    

            rec% = 1%
            gosub lookup_private_label
            convert lk_ad_allowance to lk_ad_allowance$, pic(######.##-)

            convert lk_samples%(80%) to lk_samples_u$,   pic(#######-)
 
            convert lk_samples(80%) to lk_samples$,      pic(######.##-)
 
            convert lk_marketing%(40%) to lk_marketing_u$, pic(#######-)

            convert lk_marketing(40%) to lk_marketing$,  pic(######.##-)

                                                        /* Temporary  */
            lk_samples%       = 0%
            lk_samples        = 0.0
            lk_marketing%     = 0%
            lk_marketing      = 0.0
                                                        /* Temporary  */ 

            convert lk_samples% to lk_samples_up$,       pic(#######-)
 
            convert lk_samples to lk_samples_p$,         pic(######.##-)
 
            convert lk_marketing% to lk_marketing_up$,   pic(#######-)

            convert lk_marketing to lk_marketing_p$,    pic(######.##-)

L30000: return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        delete_it                                        /*  (EWD001)   */
        dataput
            init(" ") lk_key$, lk_filler1$, lk_filler2$, lk_filler3$,   ~
                      lk_filler4$
 
            lk_key$ = lk_cuscode$
            read #3,hold,key = lk_key$, eod goto L31000
               delete #3

            if keyhit% = 12% then goto delete_done      /*  (EWD001)    */

        data_put
L31000:     put #3, using L35040,lk_priv$,       /* Private Label Code */~
                                 lk_cuscode$,    /* customer Code      */~
                                 lk_ad_allowance,/* Advertising Allowan*/~
                                 lk_samples%(),  /* Sample/Display Unit*/~
                                 lk_samples(),   /* Sample/Display $$$ */~
                                 lk_marketing%(),/* Literature/App Unit*/~
                                 lk_marketing(), /* Literature/App $$'s*/~
                                 lk_samples%,    /* Samp/Disp Un Purch */~
                                 lk_samples,     /* Samp/Disp $$$ Purch*/~
                                 lk_marketing%,  /* Lit/App Units Purch*/~
                                 lk_marketing,   /* Lit/App $$$ Purchas*/~
                                 lk_filler1$,    /* Filler Area        */~
                                 lk_filler2$,    /* Filler Area        */~
                                 lk_filler3$,    /* Filler Area        */~
                                 lk_filler4$     /* Filler Area        */    

            write #3, eod goto L31010

            if keyhit% = 30% then return
        delete_done                                     /*  (EWD001)   */
        return clear all
        goto inputmode

L31010:     errormsg$ = "(Error) - Unable to Update 'CUSTLINK'?"
            gosub error_prompt
        return  


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35040:     FMT CH(02),             /* Private Label Code (CUSTLINK )  */~
                CH(09),             /* Customer Code                   */~
                PD(14,4),           /* Advertising Allowance           */~
                80*BI(2),           /* Sample/Display Unit             */~
                80*PD(14,4),        /* Sample/Display $$$              */~
                40*BI(2),           /* Literature/App Unit             */~
                40*PD(14,4),        /* Literature/App $$'s             */~
                BI(2),              /* Sample Display Units Purchased  */~
                PD(14,4),           /* Sample Display Dollars Purchased*/~
                BI(2),              /* Literature/App Units Purchased  */~
                PD(14,4),           /* Literature/App Dollars Purchased*/~ 
                CH(250),            /* Filler Area (1)                 */~
                CH(250),            /* Filler Area (2)                 */~
                CH(250),            /* Filler Area (3)                 */~
                CH(35)              /* Filler Area (4)                 */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L40100:       gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40160,          /* lk_cuscode$        */~
                                L40160           /* lk_ad_allowance$   */

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,16), fac(hex(94)), screen_msg$            , ch(45),~
                                                                         ~
                                                                         ~
               at (04,02), "Customer Code        :",                     ~
               at (04,25), fac(lfac$(1%)), lk_cuscode$          , ch(09),~
               at (04,40), fac(hex(84)),   lk_cuscode_d$        , ch(30),~
                                                                         ~
               at (05,02), "Advertising Allowance:",                     ~
               at (05,25), fac(lfac$(2%)), lk_ad_allowance$     , ch(10),~
                                                                         ~
               at (10,02), "Total for Samples & Displays  :",            ~
               at (10,35), fac(hex(84)),   lk_samples_u$        , ch(08),~
               at (10,50), fac(hex(84)),   lk_samples$          , ch(10),~
                                                                         ~
               at (12,02), "Total for Literature & Apparel:",            ~
               at (12,35), fac(hex(84)),   lk_marketing_u$      , ch(08),~
               at (12,50), fac(hex(84)),   lk_marketing$        , ch(10),~
                                                                         ~
               at (15,02), "Total for Samp/Disp Purchased :",            ~
               at (15,35), fac(hex(84)),   lk_samples_up$       , ch(08),~
               at (15,50), fac(hex(84)),   lk_samples_p$        , ch(10),~
                                                                         ~
               at (17,02), "Total for Lit/App Purchased   :",            ~
               at (17,35), fac(hex(84)),   lk_marketing_up$     , ch(08),~
               at (17,50), fac(hex(84)),   lk_marketing_p$      , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 7% then goto L40200
                  gosub display_samples
                  goto L40100

L40200:        if keyhit% <> 8% then goto L40210
                  gosub display_marketing
                  goto L40100

L40210:        if keyhit% <> 15% then goto L40420
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
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f10001E)
            if fieldnr% = 1% then L40570
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over         (7)Display Samples" &        ~
                      "                      (12)Delete Cust  "  /* (EWD001) */
            pf$(2%) = "                      (8)Display Marketi" &        ~
                      "ng                    (15)Print Screen "
            pf$(3%) = "                                        " &        ~
                      "                      (16)Save Data    "
            pfkeys$ = hex(01ffffffffff0708ffffff0cffff0f1000)
            if rec% = 1% then return
               str(pf$(1%),23%,22%) = " " : str(pfkeys$,7%,1%) = hex(ff)
               str(pf$(2%),23%,22%) = " " : str(pfkeys$,8%,1%) = hex(ff)
                                                                 /* (EWD001) */
               str(pf$(1%),62%,17%) = " " : str(pfkeys$,8%,1%) = hex(ff)
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
            *           D I S P L A Y   V A L U E S                     *~
            *-----------------------------------------------------------*~
            * Display Values                                            *~
            *************************************************************

        display_values
L41000:     gosub set_pf2
            accept                                                       ~
               at (01,17), fac(hex(84)), header$                , ch(42),~
               at (01,62), fac(hex(84)), pageno$                , ch(16),~
                                                                         ~
               at (04,02), "Code   Description                   ",      ~
               at (05,02), "----   ------------------------------",      ~
                                                                         ~
               at (04,40), "Units    Sales Dollars",                     ~
               at (05,40), "-------- -------------",                     ~
                                                                         ~
               at (06,03), fac(hex(84))  , cd$(k% + 1%)         , ch(02),~
               at (06,09), fac(hex(84))  , cd_d$(k% + 1%)       , ch(30),~
                                                                         ~
               at (06,40), fac(hex(84))  , val1$(k% + 1%)       , ch(08),~
               at (06,51), fac(hex(84))  , val2$(k% + 1%)       , ch(10),~
                                                                         ~
               at (07,03), fac(hex(84))  , cd$(k% + 2%)         , ch(02),~
               at (07,09), fac(hex(84))  , cd_d$(k% + 2%)       , ch(30),~
                                                                         ~
               at (07,40), fac(hex(84))  , val1$(k% + 2%)       , ch(08),~
               at (07,51), fac(hex(84))  , val2$(k% + 2%)       , ch(10),~
                                                                         ~
               at (08,03), fac(hex(84))  , cd$(k% + 3%)         , ch(02),~
               at (08,09), fac(hex(84))  , cd_d$(k% + 3%)       , ch(30),~
                                                                         ~
               at (08,40), fac(hex(84))  , val1$(k% + 3%)       , ch(08),~
               at (08,51), fac(hex(84))  , val2$(k% + 3%)       , ch(10),~
                                                                         ~
               at (09,03), fac(hex(84))  , cd$(k% + 4%)         , ch(02),~
               at (09,09), fac(hex(84))  , cd_d$(k% + 4%)       , ch(30),~
                                                                         ~
               at (09,40), fac(hex(84))  , val1$(k% + 4%)       , ch(08),~
               at (09,51), fac(hex(84))  , val2$(k% + 4%)       , ch(10),~
                                                                         ~
                at (10,03), fac(hex(84))  , cd$(k% + 5%)         , ch(02),~
               at (10,09), fac(hex(84))  , cd_d$(k% + 5%)       , ch(30),~
                                                                         ~
               at (10,40), fac(hex(84))  , val1$(k% + 5%)       , ch(08),~
               at (10,51), fac(hex(84))  , val2$(k% + 5%)       , ch(10),~
                                                                         ~
               at (11,03), fac(hex(84))  , cd$(k% + 6%)         , ch(02),~
               at (11,09), fac(hex(84))  , cd_d$(k% + 6%)       , ch(30),~
                                                                         ~
               at (11,40), fac(hex(84))  , val1$(k% + 6%)       , ch(08),~
               at (11,51), fac(hex(84))  , val2$(k% + 6%)       , ch(10),~
                                                                         ~
               at (12,03), fac(hex(84))  , cd$(k% + 7%)         , ch(02),~
               at (12,09), fac(hex(84))  , cd_d$(k% + 7%)       , ch(30),~
                                                                         ~
               at (12,40), fac(hex(84))  , val1$(k% + 7%)       , ch(08),~
               at (12,51), fac(hex(84))  , val2$(k% + 7%)       , ch(10),~
                                                                         ~
               at (13,03), fac(hex(84))  , cd$(k% + 8%)         , ch(02),~
               at (13,09), fac(hex(84))  , cd_d$(k% + 8%)       , ch(30),~
                                                                         ~
               at (13,40), fac(hex(84))  , val1$(k% + 8%)       , ch(08),~
               at (13,51), fac(hex(84))  , val2$(k% + 8%)       , ch(10),~
                                                                         ~
               at (14,03), fac(hex(84))  , cd$(k% + 9%)         , ch(02),~
               at (14,09), fac(hex(84))  , cd_d$(k% + 9%)       , ch(30),~
                                                                         ~
               at (14,40), fac(hex(84))  , val1$(k% + 9%)       , ch(08),~
               at (14,51), fac(hex(84))  , val2$(k% + 9%)       , ch(10),~
                                                                         ~
               at (15,03), fac(hex(84))  , cd$(k% + 10%)        , ch(02),~
               at (15,09), fac(hex(84))  , cd_d$(k% + 10%)      , ch(30),~
                                                                         ~
               at (15,40), fac(hex(84))  , val1$(k% + 10%)      , ch(08),~
               at (15,51), fac(hex(84))  , val2$(k% + 10%)      , ch(10),~
                                                                         ~
               at (16,03), fac(hex(84))  , cd$(k% + 11%)        , ch(02),~
               at (16,09), fac(hex(84))  , cd_d$(k% + 11%)      , ch(30),~
                                                                         ~
               at (16,40), fac(hex(84))  , val1$(k% + 11%)      , ch(08),~
               at (16,51), fac(hex(84))  , val2$(k% + 11%)      , ch(10),~
                                                                         ~
               at (17,03), fac(hex(84))  , cd$(k% + 12%)        , ch(02),~
               at (17,09), fac(hex(84))  , cd_d$(k% + 12%)      , ch(30),~
                                                                         ~
               at (17,40), fac(hex(84))  , val1$(k% + 12%)      , ch(08),~
               at (17,51), fac(hex(84))  , val2$(k% + 12%)      , ch(10),~
                                                                         ~
               at (18,03), fac(hex(84))  , cd$(k% + 13%)        , ch(02),~
               at (18,09), fac(hex(84))  , cd_d$(k% + 13%)      , ch(30),~
                                                                         ~
               at (18,40), fac(hex(84))  , val1$(k% + 13%)      , ch(08),~
               at (18,51), fac(hex(84))  , val2$(k% + 13%)      , ch(10),~
                                                                         ~
               at (19,03), fac(hex(84))  , cd$(k% + 14%)        , ch(02),~
               at (19,09), fac(hex(84))  , cd_d$(k% + 14%)      , ch(30),~
                                                                         ~
               at (19,40), fac(hex(84))  , val1$(k% + 14%)      , ch(08),~
               at (19,51), fac(hex(84))  , val2$(k% + 14%)      , ch(10),~
                                                                         ~
               at (20,03), fac(hex(84))  , cd$(k% + 15%)        , ch(02),~
               at (20,09), fac(hex(84))  , cd_d$(k% + 15%)      , ch(30),~
                                                                         ~
               at (20,40), fac(hex(84))  , val1$(k% + 15%)      , ch(08),~
               at (20,51), fac(hex(84))  , val2$(k% + 15%)      , ch(10),~
                                                                         ~
              at (23,02), fac(hex(a4)), pf$(1%)                 , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L41020             /* First    */
L41010:           k% = 0%
                  goto L41000

L41020:        if keyhit% <> 3% then goto L41040             /* Last      */
L41030:           x% = int(val_max% / 15%)
                  k% = (x%*15%)
                  goto L41000

L41040:        if keyhit% <> 4% then goto L41050             /* Previous */
                  if k% < 16% then goto L41010
                  k% = k% - 15%
                  if k% <= 1% then goto L41010
                  goto L41000

L41050:        if keyhit% <> 5% then goto L41060             /* Next     */
                  k% = k% + 15%
                  if k% < val_max% then goto L41000
                  goto L41030

L41060:        if keyhit% <> 15 then goto L41070
                  call "PRNTSCRN"
                  goto L41000

L41070:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
            pageno$ = "Page: XXX of XXX"
            xx% = (val_max% / 15%) + 1%
            convert xx% to str(pageno$,14%,3%), pic(###)


            xx% = (k% / 15%) + 1%
            convert xx% to str(pageno$,7%,3%), pic(###)

            pf$(1) = "(2)First     (3)Last     (4)Previous    " &        ~
                     " (5)Next (15)Print Screen <Return> Cont"
            pfkeys$ = hex(ff02030405ffffffffffffffffff0f1000)
            gosub check_screen
            return

        check_screen
            if val_max% > 15% then goto L42000
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L42000:      if k% >= 15% then goto L42010
                gosub no_first
                gosub no_prev
L42010:      if (k% + 15%) <= val_max% then goto L42020
                gosub no_last
L42020:      if k% <= (val_max% - 15%) then goto L42030
                gosub no_next
L42030: return
        no_first
            str(pf$(1%),1%, 9%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
            str(pf$(1%),41%, 9%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last
            str(pf$(1%),14%,9%)  = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
            str(pf$(1%),26%,12%) = " " : str(pfkeys$,4%,1%) = hex(ff)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50010,        /* Customer Code          */~
                              L50100         /* Advertising Allowance  */

            return

L50010: Rem Enter a customer Code                     lk_cuscode$
            if lk_cuscode$ <> " " then goto L50015
                call "GETCODE" (#2, lk_cuscode$, " ", 0%, 1.30, rhh%)
                if rhh% = 0% then goto L50070

L50015:     read #2,key = lk_cuscode$, using L50020, lk_cuscode_d$,      ~
                                       lk_priv$, eod goto L50070
L50020:        FMT POS(10), CH(30), POS(960), CH(2)

            gosub lookup_private_label
            gosub dataload
            if rec% = 1% then fieldnr% = 2%
  
        return
L50070:     errormsg$ = "(Error) Invalid Customer code????"
            gosub error_prompt
            init(" ") lk_cuscode$, lk_cuscode_d$, lk_priv$, lk_priv_d$
        return


L50100: Rem Enter Advertising Allowance        lk_ad_allowance$
            lk_ad_allowance = 0.0
            convert lk_ad_allowance$ to lk_ad_allowance, data goto L50110

L50110:     convert lk_ad_allowance to lk_ad_allowance$, pic(######.##-)

        return

        lookup_private_label
            init(" ") readkey$, desc$, screen_msg$
            str(readkey$,1%,9%)   = "ELLISON01"
            str(readkey$,10%,15%) = lk_priv$
            read #1,key = readkey$, using L51000, desc$, eod goto L51010
L51000:        FMT POS(25), CH(30)
            lk_priv_d$ = desc$
            screen_msg$ = "Private Label: " & lk_priv_d$
L51010: return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        display_samples
            init(" ") cd$(), cd_d$(), val1$(), val2$(), readkey$, desc$
                i% = 0%
                str(readkey$,1%,9%) = "APC WOOD"
L60000:         read #1,key > readkey$, using L60010, readkey$, desc$,  ~
                                                       eod goto L60020
L60010:            FMT CH(24), CH(30)
                if str(readkey$,1%,9%) <> "APC WOOD" then goto L60020
                   yy% = 0%
                   convert str(readkey$,10%,3%) to yy%, data goto L60020
 
                   if yy% = 0% then goto L60000
                   cd$(yy%)   = str(readkey$,11%,2%)
                   cd_d$(yy%) = desc$
                   convert lk_samples%(yy%) to val1$(yy%), pic(#######-)
             
                   convert lk_samples(yy%) to val2$(yy%), pic(######.##-)
         
                   i% = i% + 1%
                   goto L60000

L60020:    val_max% = i%
           k% = 0%
           header$ = "***  Samples and Displays Information  ***"
           gosub display_values
        return

        display_marketing
            init(" ") cd$(), cd_d$(), val1$(), val2$()

                i% = 0%
                init(" ") readkey$, desc$
                str(readkey$,1%,9%) = "ELLISON06"
L60100:         read #1,key > readkey$, using L60110, readkey$, desc$,  ~
                                                      eod goto L60130
L60110:            FMT CH(24), CH(30)
                if str(readkey$,1%,9%) <> "ELLISON06" then goto L60130
                   yy% = 0%
                   convert str(desc$,1%,2%) to yy%, data goto L60130

                   cd$(yy%)   = str(readkey$,10%,1%)

                   cd_d$(yy%) = str(desc$,4%,30%)

                   convert lk_marketing%(yy%) to val1$(yy%), pic(#######-)
             
                   convert lk_marketing(yy%) to val2$(yy%), pic(######.##-)
                   i% = i% + 1%
                   goto L60100

L60130:    val_max% = i%
           k% = 0%
           header$ = "Marketing Literature & Apparel Information"
           gosub display_values
        return

        error_prompt                               /* (EWD001)          */
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_error                                    /* (EWD001)        */
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return

        clear_values                               /* (EWD001)          */
           if userid$ <> "CMG" and userid$ <> "DES" and userid$ <> "CG1" then return
           init(" ") lk_key$
        clear_values_next    
            read #3, hold, key > lk_key$, using L60200, lk_key$, eod goto all_done

L60200:        FMT POS(03), CH(09)

            gosub data_load

            mat lk_samples%   = zer
            mat lk_samples    = zer
            mat lk_marketing% = zer
            mat lk_marketing  = zer

            lk_samples%       = 0%
            lk_samples        = 0.0
            lk_marketing%     = 0%
            lk_marketing      = 0.0
 
            lk_ad_allowance = 0.0

            delete #3
            gosub data_put

            goto clear_values_next
        all_done
        goto exit_program                           /* (EWD001)          */
        
        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            end
