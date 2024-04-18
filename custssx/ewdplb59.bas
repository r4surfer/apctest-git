        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDPLB59                             *~
            *  Creation Date     - 01/25/99                             *~
            *  Last Modified Date- 01/25/99                             *~
            *  Written By        - Brian W. Sanders                     *~
            *                                                           *~
            *  Description       - Rapid entry *ONLY* of tube winding   *~
            *                      and coil data. Winds & coils are     *~
            *                      are entered for a Maximum Width &    *~
            *                      Maximum Height combination.          *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/25/99 ! (New) Program - Copied & Mod EWDPLN59.   ! BWS *~
            * 12/19/01 ! (EWD001) - Mod to incr by decimal number.! CMG *~
            *************************************************************

            sub "EWDPLB59" (#1, #4)      /* #1-APCPLNWC, #4-GENCODES   */

        dim                                                              ~
            readkey$50, desc$30,         /* GENCODES Lookup & Descr    */~
            sav_key$15,                  /* Use for Loading Table      */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            hdr2$13,                     /* Column Header              */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */~

        dim height$5,                    /* Max. Height for Winds/Coil */~
            hgt_beg$5, hgt_end$5,        /* Max. Height Range          */~
            hgt_inc$4,                   /* Max. Height Increment      */~
            gls$1,                       /* Glass Type                 */~
            gls_d$30,                    /* Glass Type Description     */~
            gtdesc$(6)30,                /* Glass Type Descr Lookup    */~
            model$3,                     /* Model No.                  */~
            model_d$30,                  /* Model Description          */~
            lkupcd$2,                    /* Lkup Diameter Code         */~
            lkupcd_d$30,                 /* Lkup Diameter Code Descrptn*/~
            windcl$(6)6,                 /* Winds/Coil Data Array      */~
            windcl$6,                    /* Entry Winds/Coil           */~
            wcflg$3,                     /* Blank Winds/Coil Flag      */~
            tubedi$(6)2,                 /* Tube Diameter for Wnds/Coil*/~
            tubedi$2,                    /* Entry Tube Diameter        */~
            tdflg$1,                     /* Tube Diameter Change Flag  */~
            width$5,                     /* Max. Width for Winds/Coil  */~
            wdt_beg$5, wdt_end$5,        /* Max. Width Range           */~
            wdt_inc$4,                   /* Max. Width Increment       */~
            cur_rec$96,                  /* Entire Current WC Record   */~
            prv_rec$96,                  /* Entire Previous WC Record  */~
            prv_tubedi$2                 /* Previous Tube Diameter     */~

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Tube Windings/Coil Rapid Entry"
            pname$ = "EWDPLB59 - Rev: R7.00"

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
            * #1  ! APCPLNWC ! Production Windings & Coils File         *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            *************************************************************~
 
            init (" ") rslt$()
            mat f1% = zer
            mat fs% = zer

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
            actvflds% = 7%        /* No. of Active "Input Mode" Fields */

            gtdesc$(1%) = "Single Strength Glass         "
            gtdesc$(2%) = "Double Strength Glass         "
            gtdesc$(3%) = "Laminate Glass                "
            gtdesc$(4%) = "                              "
            gtdesc$(5%) = "                              "
            gtdesc$(6%) = "                              "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  actvflds%
L10110:         gosub'051(fieldnr%,1%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)       /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%,1%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%,1%)  /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
*               NON-STANDARD EDIT MODE -- 2 fields are saved upon pressing
*                                         ENTER. Repeated until PF16-Exit
*                                         or wdt_end is exceeded.
        editpg1
            fieldnr% = 8%              /* Display Screen - Fld #8 Only */ 
            gosub bump_height
            gosub'051(fieldnr%,2%)      /* Check Enables, Set Defaults */
                  if enabled% =  0% then       exit_program
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then goto  exit_program
                  if keyhit% <>  0% then       editpg1
            gosub'151(fieldnr%,2%)      /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  gosub dataload
                  gosub dataput
            goto editpg1


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%, edit%)
            enabled% = 1%
            x% = pos(windcl$ = "-")
            if x% <> 0% and wcflg$ = "On" then windcl$ = " "
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
         "Enter Model Number                                           ",~
         "Enter Lookup Diameter/Coil Code                              ",~
         "Enter Maximum Width Range (1/8 inch) for Winds/Coil Data     ",~
         "Enter Maximum Height Range (1/8 inch) for Winds/Coil Data    ",~
         "Enter Width Increment (whole inch) for Winds/Coil Data       ",~
         "Enter Height Increment (whole inch) for Winds/Coil Data      ",~
         "Enter Glass Type Code ('S'ingle/'D'ouble/'L'aminate)         ",~
         "Enter Winds/Coils & Tube Diameter for Displayed Width/Height ",~
         "                                                             ",~
         "                                                             "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, hdr2$, windcl$, tubedi$,   ~
                tdflg$, cur_rec$, prv_rec$, prv_tubedi$, model_d$,       ~
                lkupcd_d$, gls_d$, width$, height$

            wcflg$ = "On"                             
            onfile%, j% = 0%

        return

        REM *************************************************************~
            *************************************************************

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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        DATALOAD
            onfile% = 0%
            init(" ") windcl$(), tubedi$()
            sav_key$ = str(model$) & str(lkupcd$) & str(width$)          ~ 
                & str(height$)
            read #1, hold, key = sav_key$, using L30100, windcl$(),      ~
                tubedi$(), eod goto L30998
L30100:     fmt pos(16), 6*ch(6), 6*ch(02)
            onfile% = 1%


L30998:     return


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        DATAPUT
            windcl$(j%) = windcl$
            tubedi$(j%) = tubedi$
            put #1, using L35050, model$, lkupcd$, width$, height$,      ~
                windcl$(), tubedi$(), " "
            get #1, using L31100, cur_rec$
L31100:         fmt ch(96)
            if onfile% = 1% then rewrite #1, data goto write_err         ~
                else write #1, data goto write_err, eod goto write_err
            if str(cur_rec$,16%,81%) = str(prv_rec$,16%,81%)             ~
                then gosub delt_prv_rec
            prv_rec$ = cur_rec$
            prv_tubedi$ = tubedi$
            return

            
            delt_prv_rec
                read #1, hold, key = str(prv_rec$,,15%), eod goto L31300
                delete #1
L31300:         return


            write_err
                errormsg$ = "Error writing to APCPLNWC. Data NOT saved."
                gosub error_prompt
                return clear all
                goto INPUTMODE



        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35050:     FMT                         /* File: APCPLNWC              */~
                CH(03),                 /* Model No.                   */~
                CH(02),                 /* Lkup Diameter Code          */~
                CH(05),                 /* Width                       */~
                CH(05),                 /* Height                      */~
                6*CH(06),               /* Winds/Coil Data             */~
                6*CH(02),               /* Tube Diameter Codes         */~
                CH(33)                  /* Filler                      */

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
              on fieldnr% gosub L40160,          /* model$             */~
                                L40160,          /* lkupcd$            */~
                                L40170,          /* wdt_beg$, wdt_end$ */~
                                L40170,          /* hgt_beg$, hgt_end$ */~
                                L40170,          /* wdt_inc$           */~
                                L40170,          /* hgt_inc$           */~
                                L40160,          /* gls$               */~
                                L40160           /* windcl$/tubedi$    */
              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40170:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     hdr2$ = width$ & " / " & height$
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Model Number:",                              ~
               at (03,30), fac(lfac$(1%)), model$               , ch(03),~
               at (03,40), fac(hex(84)),   model_d$             , ch(30),~
                                                                         ~
               at (04,02), "Lookup Diameter Code:",                      ~
               at (04,30), fac(lfac$(2%)), lkupcd$              , ch(02),~
               at (04,40), fac(hex(84)),   lkupcd_d$            , ch(30),~
                                                                         ~
               at (05,02), "Max. Width Range:",                          ~
               at (05,30), fac(lfac$(3%)), wdt_beg$             , ch(05),~
               at (05,40), fac(lfac$(3%)), wdt_end$             , ch(05),~
                                                                         ~
               at (06,02), "Max. Height Range:",                         ~
               at (06,30), fac(lfac$(4%)), hgt_beg$             , ch(05),~
               at (06,40), fac(lfac$(4%)), hgt_end$             , ch(05),~
                                                                         ~
               at (07,02), "Width Increment:",                           ~
/*EWD001*/     at (07,30), fac(lfac$(5%)), wdt_inc$             , ch(04),~
                                                                         ~
               at (08,02), "Height Increment:",                          ~
/*EWD001*/     at (08,30), fac(lfac$(6%)), hgt_inc$             , ch(04),~
                                                                         ~
               at (09,02), "Glass Type Code:",                           ~
               at (09,30), fac(lfac$(7%)), gls$                 , ch(01),~
               at (09,40), fac(hex(84)),   gls_d$               , ch(30),~
                                                                         ~
               at (11,02), "Current Ending Width/Height =",              ~
               at (11,31), fac(hex(a4)), hdr2$                  , ch(13),~
               at (11,50), "Blank W/C Data Flag = ",                     ~
               at (11,72), fac(hex(8c)),   wcflg$               , ch(03),~
                                                                         ~
               at (13,02), "Winds/Coil:",                                ~
               at (13,15), fac(lfac$(8%)), windcl$              , ch(06),~
               at (13,25), "Tube Diameter:",                             ~
               at (13,41), fac(lfac$(8%)), tubedi$              , ch(02),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <>  5 then goto L40400
                  gosub next_col
                  goto L40190

L40400:        if keyhit% <>  9 then goto L40410
                  if wcflg$ = "On" then wcflg$ = "Off" else wcflg$ = "On"
                  goto L40190

L40410:        if keyhit% <> 15 then goto L40420
                  call "PRNTSCRN"
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (5)Next Column         " &        ~
                     "(9)Toggle W/C Flag     (15)Print Screen"
            pf$(3) = "(ENTER)Save Data                        " &        ~
                     "                       (16)Exit Program" 
            pfkeys$ = hex(01ffffff05ffffff09ffffffffff0f1000)
            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%,edit%)
            errormsg$ = " "
            on fieldnr% gosub L50010,        /* Model Number           */~
                              L50020,        /* Lookup Diameter Code   */~
                              L50030,        /* Max.Wdth Range for W/C */~
                              L50040,        /* Max.Hght Range for W/C */~
                              L50050,        /* Width Increment for W/C*/~
                              L50060,        /* Height Incremnt for W/C*/~
                              L50070,        /* Glass Type Code        */~
                              L50080         /* Winds/Coils & Tube Diam*/

            return

L50010: rem Enter Model Number                            model$
        readkey$ = "MODEL    " & model$
        gosub test_model
        return

    test_model
        call "DESCRIBE" (#4, readkey$, model_d$, 0%, f1%(4))
        if f1%(4) = 0% then errormsg$ = "Invalid Model No."
        return

L50020: rem Enter Lookup Diameter Code                    lkupcd$
        readkey$ = "PLAN DIAM" & lkupcd$
        gosub test_lookup
        return

    test_lookup
        call "DESCRIBE" (#4, readkey$, lkupcd_d$, 0%, f1%(4))
            if f1%(4) = 0% then errormsg$="Invalid Lookup Diameter Code"
        return

L50030: rem Enter Max. Width Range for Winds/Coil         wdt_beg/end$
        if wdt_beg$ = " " or wdt_end$ = " " then goto bad_width
        convert wdt_beg$ to wdt_beg, data goto bad_width
        if wdt_beg < 0 or wdt_beg > 999.99 then goto bad_width
        convert wdt_beg to wdt_beg$, pic(000.0)
        convert wdt_end$ to wdt_end, data goto bad_width
        if wdt_end < 0 or wdt_end > 999.99 then goto bad_width
        convert wdt_end to wdt_end$, pic(000.0)
        if wdt_end < wdt_beg then goto bad_width
        return

    bad_width
        errormsg$ = "Invalid data for Maximum Width Range"
        return

L50040: rem Enter Max. Height Range for Winds/Coil        hgt_beg/end$
        convert hgt_beg$ to hgt_beg, data goto bad_height
        if hgt_beg < 0 or hgt_beg > 999.99 then goto bad_height
        convert hgt_beg to hgt_beg$, pic(000.0)
        convert hgt_end$ to hgt_end, data goto bad_height
        if hgt_end < 0 or hgt_end > 999.99 then goto bad_height
        convert hgt_end to hgt_end$, pic(000.0)
        if hgt_end < hgt_beg then gosub bad_height
        return

    bad_height
        errormsg$ = "Invalid data for Maximum Height Range"
        return

L50050: rem Enter Width Increment                         wdt_inc$   EWD001
        convert wdt_inc$ to wdt_inc, data goto bad_increment
        if wdt_inc < 0 or wdt_inc > 99.9 then goto bad_increment
        convert wdt_inc to wdt_inc$, pic(00.0)
        width = wdt_beg - wdt_inc       /* Init width for 'bump'. */
        return

L50060: rem Enter Height Increment                        hgt_inc$   EWD001
        convert hgt_inc$ to hgt_inc, data goto bad_increment
        if hgt_inc < 0 or hgt_inc > 99.9 then goto bad_increment
        convert hgt_inc to hgt_inc$, pic(00.0)
        height = hgt_beg - hgt_inc      /* Init height for 'bump'. */
        return


    bad_increment
        errormsg$ = "Invalid data for Increment Value"
        return


L50070: rem Enter Glass Type Code                         gls$      
        j% = pos("SDL" = gls$)                                      
        if j% = 0% then errormsg$ = "Invalid Glass Type Code"
        if j% <> 0% then gls_d$ = gtdesc$(j%)
        return

L50080: rem Enter Winds/Coil & Tube Diameter              windcl$/tubedi$
        readkey$ = "PLAN DIAM" & tubedi$
        call "DESCRIBE" (#4, readkey$, desc$, 0%, f1%(4))
            if f1%(4) = 0% then errormsg$="Invalid Tube Diameter Code"
        if tubedi$ <> prv_tubedi$ and height$ <> hgt_beg$ then tdflg$ = "Y"  
        if height$ = hgt_beg$ then tdflg$ = "N"
        if windcl$ = " " then errormsg$ = "Invalid data for Winds/Coil"
        return


        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        bump_height
            height = height + hgt_inc
            if height > hgt_end or width < wdt_beg then gosub next_col
            convert height to height$, pic(000.0)
            return

          next_col                      /* Called from deffn'101 Also */
            width = width + wdt_inc
            if width > wdt_end then goto L64500
            convert width to width$, pic(000.0)
            height = hgt_beg
            if tdflg$ = "Y" then tubedi$ = "**"
            windcl$ = " "
            return

L64500:     return clear all
            goto inputmode
           

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
           if prv_rec$ = " " then end   /* Done Nothing or All Done */
           comp% = 2%
           hdr$     = "******* Exit Rapid Entry Mode??  *******"
           msg$(1%) = "Press <ENTER> to Exit Rapid Entry Mode"
           msg$(2%) = "--- OR ---"
           msg$(3%) = "Press <PF1> to Resume"
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
           if comp%  = 1% then goto L11170
           if comp% <> 0% then goto exit_program
           end
            
