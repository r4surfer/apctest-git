        REM *************************************************************~    
            *                                                           *~
            *  Program Name      - EWDPLP72                             *~ 
            *  Creation Date     - 12/09/2015                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Paul W Williamsn                     *~
            *  Modifications By  -                                      *~ 
            *                                                           *~
            *  Description       - Generate Glass Barcode Label         *~
            *                                                           *~
            *  Process Selection (1) = 'Single Label Format'            *~    
            *                    (2) = 'Production Labels'              *~
            *                    (3) = 'Re-Make Label Format'           *~
            *                    (4) = 'Special Glass Labels'           *~
            *                    (5) = 'Special Shapes Labels'          *~
            *                    (6) = 'Tempered Glass Labels'          *~
            *                    (7) = 'Temp Re-Make Glass Labels'      *~
            *                    (8) = 'Pre-Cut Glass Labels'  (AWD008) *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *      (EWDPLF72)    - (1%) Single Label Print (Glass)      *~
            *      (EWDPLE72)    - (2%) Production Labels               *~
            *      (EWDPLF72)    - (3%) Re-Make Glass Labels            *~
            *      (EWDPLG72)    - (4%) Special Glass Labels (EWDPLN66) *~
            *      (EWDPLH72)    - (5%) Special Shapes Glass Labels     *~
            *      (EWDPLE72)    - (6%) Tempered Glass Production Labels*~
            *      (EWDPLF72)    - (7%) Tempered Glass Re-Make Labels   *~
            *      (EWDPLE72)    - (8%) Pre-Cut Glass Labels (AWD008)   *~
            *                                                           *~
            *  Special Comments      - Error Codes                      *~
            *          - ERROR% = 0% - Lbl Ok and Sent                  *~
            *                     1% - Could Not Open Lbl Print File    *~
            *                     4% - Shell Script Error               *~
            *                     5% - Print Error Creating Label       *~
            *                     6% - Could Not Reset Table Flag       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/11/99 ! (New) Program -                          ! RHH *~
            * 12/20/99 ! (EWD001) Mod for new 12 Digit Barcode for! RHH *~
            *          !          glass label                     !     *~
            * 08/20/01 ! (EWD002) Mod to print Special Shapes     ! RHH *~ 
            *          !          glass label.                    !     *~
            * 07/30/03 ! (EWD003) Mod to improve speed of Labels. ! RHH *~
            * 10/01/03 ! (EWD004) Mod to add Process codes 6 for  ! RHH *~
            *          !          Tempered Glass, and 7 for Temp. !     *~
            *          !          Re-Make Glass.                  !     *~
            * 02/23/04 ! (EWD005) Mod for New Shape System        ! RHH *~
            * 05/20/04 ! (EWD006) Mod to put Glass Code on the    ! RHH *~
            *          !          Special Shape Glass label       !     *~
            * 08/11/04 ! (EWD007) Mod to put Grid Size on the     ! RHH *~
            *          !          Special Shape Labels. And       !     *~
            *          !          correct Glass Barcode to 12 Digit!    *~    
            * 02/14/05 ! (AWD008) Mod for Pre-Cut Glass Labels    ! CMG *~
            * 01/01/06 ! (PAR000) CR347 Mod for New Sub Part No.  ! RHH *~
            * 03/27/06 ! (PAR001) Mod to print labels in the      ! RHH *~
            *          !          North East. All 4 Subs changed. !     *~
            *          !          EWDPLA72,EWDPLF72,EWDPLC72,     !     *~
            *          !          EWDPLD72.                       !     *~
            * 04/10/06 ! (PAR002) Mod for NE Test label script in ! RHH *~
            *          !          EWDPLA72 and EWDPLD72.          !     *~  
            * 03/24/08 ! (AWD009) mod for tempered glass labels   ! CMG *~ 
            *05/18/2015! (IM8022) mod for laminate                ! CMG *~
            *12/09/2015! SR67154  Cloned from EWDPLN72 for new    ! PWW *~
            *          !          larger glass labels. We now use !     *~
            *          !          subs EWDPLE72,EWDPLF72,EWDPLG72 !     *~
            *          !          and EWDPLH72. This way we leave !     *~
            *          !          the original programs intact so !     *~
            *          !          that smaller original labels can!     *~
            *          !          be used until gone.             !     *~
            *10/25/2019! CR2304 Low-e barcode changes Expand file ! RDB *~   
            *10/05/2021! CR2913 Issue with invalid written record ! RDB *~            
            *************************************************************
                         
        dim                                                              ~
            rm_key$12, rm_rec$(2%)192,   /* Remake Data       (PAR000) */~
            gl_date$10, gl_dte$10,       /* Prod Date Formatted, an Un */~
            gl_key$32, gl_rec$256,       /* Print Glass Labels         */~
            gl_ky2$12,                   /* Print Glass Label key      */~
            gl_reb$128,                  /* CR2304 Expand file         */~
            gl_batch$20, gl_sav$27,      /* Prod Batch Name            */~
            gl_beg$9, gl_end$9,          /* Range Beg/End Barcode      */~   
            sel$1, sel_d$30,             /* Selection Code (1 thru 7)  */~ 
            barcode$9, barcode_d$30,     /* Glass Barcode              */~
            line2$24, line3$24, line4$24,/* Screen entry Text          */~
            entry2$10, entry3$20,        /* Screen Entry Values        */~
            entry4$9, entry5$9,          /* Screen Entry Beg/End Barcod*/~
            scr$(15%)65,                 /* Screen Display             */~
            error$1,                     /* Error Code Display         */~ 
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            time$8,                      /* System time                */~
            filename$8,                  /* Used by EWDOPEN            */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21 
            apc$   = "(AWD) Generate Glass/Remake Labels    "
            pname$ = "EWDPLP72 - PAR: 01.10"

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
            * #1  ! APCPLNGR ! Master Glass Remake File         (PAR000)*~
            * #2  ! GENCODES ! System Master Code Table Files           *~
            * #3  ! TXTFILE  ! Master Text File                         *~
            * #4  ! EWDGLSXX ! Glass Label Database             (EWD005)*~
            * #5  ! AWDPLNGR ! Master Glass Remake File   TEMP  (AWD009)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
                                                            /* (PAR000) */   
            select #1,  "APCPLNGR",                                      ~
                        varc,     indexed,  recsize =  512, /*(IM8022)*/ ~
                        keypos = 22,   keylen =  12,                     ~
                        alt key  1, keypos  =     7, keylen = 27,        ~
                            key  2, keypos  =     1, keylen = 33,        ~
                            key  3, keypos  =    13, keylen = 21
                                                            /* (PAR000) */
            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #3,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =   1, keylen =  11
                                                   /* (EWD005)          */
           select  #4,  "EWDGLSXX",                                      ~
                        varc,     indexed,  recsize = 384,               ~
                        keypos =  1,   keylen =  32,                     ~
                        alt key  1, keypos  =    33, keylen = 12


            call "SHOSTAT" ("Opening Files, One Moment Please")

            filename$ = "APCPLNGR" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "TXTFILE"  : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
                                                    /* (EWD005)        */
            filename$ = "EWDGLSXX" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error


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
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:               gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
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
                  if keyhit%  =  7% then gosub print_single_label
                  if keyhit%  = 14% then gosub print_labels
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2% 
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
                                                         /* (EWD002)     */
                                                         /* (EWD004)     */   
        scrn1_msg  :  data                                                ~
         "Enter a Valid Glass Print Selection? (1) thru (7)             ",~
         "Enter a Glass (1)Barcode, or (2)Prod Date, Bat Name, Beg/End Barcode?"

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
        initialize_variables                                /* (PAR000) */  
            init(" ") errormsg$, inpmessage$, barcode$, rm_key$, rm_rec$(),~
                      sel$, sel_d$, gl_date$, gl_dte$, gl_batch$, gl_key$, ~
                      gl_rec$, line2$, line3$, line4$, entry2$, entry3$,   ~
                      entry4$, entry5$, gl_reb$ 

            sel% = 0%
                                                       /* (EWD004)      */ 
            scr$( 1%) = "****************************************************"~
                    & "************"
            scr$( 2%) = "*                    Glass Label Print Options      "~
                    & "           *"
            scr$( 3%) = "*                                                   "~
                    & "           *"
            scr$( 4%) = "* 1 - Glass Single Label Print        9 - Laminate G"~
                    & "lass Labels*"
            scr$( 5%) = "* 2 - Glass Production Labels                       "~
                    & "           *"
            scr$( 6%) = "* 3 - Glass Re-Make Labels Print                    "~
                    & "           *"
            scr$( 7%) = "* 4 - Special Glass Labels Print                    "~
                    & "           *"
            scr$( 8%) = "* 5 - Special Shapes Glass Label Print              "~
                    & "           *"
            scr$( 9%) = "* 6 - Tempered Glass Labels Print                   "~
                    & "           *"
            scr$(10%) = "* 7 - Tempered R-Make Glass Label Print             "~
                    & "           *"
            scr$(11%) = "* 8 - Pre-Cut Glass Labels                          "~
                    & "           *"
            scr$(12%) = "****************************************************"~
                    & "************"
                                                      /* (EWD002)       */
        return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************


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
              on fieldnr% gosub L40170,       /* Process Selection    */~
                                L40170        /* Barcode or Prod Date */

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40170:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     gosub set_pf1
                                                        /* (EWD004)   */
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,23), fac(hex(a4)), apc$                   , ch(35),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Process Selection (1-7):",                   ~
               at (05,30), fac(lfac$(1%)), sel$                 , ch(01),~
               at (05,45), fac(hex(84)), sel_d$                 , ch(30),~
                                                                         ~   
               at (06,02), fac(hex(84)), line2$                 , ch(24),~
               at (06,30), fac(lfac$(2%)), entry2$              , ch(10),~
                                                                         ~   
               at (07,02), fac(hex(84)), line3$                 , ch(24),~
               at (07,30), fac(lfac$(3%)), entry3$              , ch(20),~
                                                                         ~   
               at (08,02), fac(hex(84)), line4$                 , ch(24),~
               at (08,30), fac(lfac$(4%)), entry4$              , ch(09),~
               at (08,45), fac(lfac$(5%)), entry5$              , ch(09),~
                                                                         ~   
               at (09,05), fac(hex(84)), scr$( 1%)              , ch(65),~
               at (10,05), fac(hex(84)), scr$( 2%)              , ch(65),~
               at (11,05), fac(hex(84)), scr$( 3%)              , ch(65),~
               at (12,05), fac(hex(84)), scr$( 4%)              , ch(65),~
               at (13,05), fac(hex(84)), scr$( 5%)              , ch(65),~
               at (14,05), fac(hex(84)), scr$( 6%)              , ch(65),~
               at (15,05), fac(hex(84)), scr$( 7%)              , ch(65),~
               at (16,05), fac(hex(84)), scr$( 8%)              , ch(65),~
               at (17,05), fac(hex(84)), scr$( 9%)              , ch(65),~
               at (18,05), fac(hex(84)), scr$(10%)              , ch(65),~
               at (19,05), fac(hex(84)), scr$(11%)              , ch(65),~
               at (20,05), fac(hex(84)), scr$(12%)              , ch(65),~
                                                                         ~   
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

 
               if keyhit% <> 15 then goto L40200
                  call "PRNTSCRN"
                  goto L40190

L40200: close ws
        call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

set_pf1:
        if sel% <> 0% then goto L40300
           init(" ") line2$, line3$, entry2$, entry3$   /* Initial Input */
           init(" ") line4$, entry4$, entry5$
           lfac$(2%) = hex(84) 
           lfac$(3%) = hex(84)
           lfac$(4%) = hex(84)
           lfac$(5%) = hex(84)
           goto L40500

L40300: if sel% <> 1% then goto L40400
           init(" ") line3$, entry3$                    /* Single Label  */
           init(" ") line4$, entry4$, entry5$ 
           line2$ = "Barcode Value          :"
           lfac$(2%) = hex(80)
           lfac$(3%) = hex(84)
           lfac$(4%) = hex(84) 
           lfac$(5%) = hex(84)
           goto L40500

L40400: line2$ = "Glass Production Date  :"             /* Selections    */
        line3$ = "Glass Batch Name       :"             /*  (2) thru (7) */
        line4$ = "Glass Beg/End Barcode  :"             /* (EWD004)      */
        lfac$(2%) = hex(81)
        lfac$(3%) = hex(81)
        lfac$(4%) = hex(81)  
        lfac$(5%) = hex(81)


L40500:     if edit% = 2% then L40610     /*  Input Mode             */
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
                      "                      (14)Print Labels "
            pf$(2%) = "                   (7)Print Single Label" &        ~
                      "                      (15)Print Screen "
            pf$(3%) = "                                        " &        ~
                      "                                       "
            pfkeys$ = hex(01ffffffffff07ffffffffffff0e0f1000)
            if sel% = 1% then goto L40620
               str(pf$(2%),18%,25%) = " " : str(pfkeys$,7%,1%) = hex(ff)
               return
L40620:     str(pf$(1%),60%) = " " : str(pfkeys$,14%,1%) = hex(ff) 

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
            on fieldnr% gosub L50010,       /* Selection Code          */~
                              L50100        /* Glass Barcode or Prod   */
                                            /* Date and Batch Name     */

            return

L50010: REM Selection Code                   sel$, sel_d$
            if sel$ <> " " then goto L50015
                sel$ = "1"

L50015:     convert sel$ to sel%, data goto L50020
                                                   /* (EWD002)         */
                                                   /* (EWD004)         */
L50020:     if sel% < 1% or sel% > 9% then goto L50025   /* (AWD008) */
               sel_d$ = str(scr$(sel% + 3%),7%,30%)
        return
L50025:     errormsg$ = "(Error) Invalid Print Selection entered?(1 thru 8)"
            gosub error_prompt
            init(" ") sel$, sel_d$
        return
   
L50100: REM Check Glass Barcode                barcode$
            if sel% <> 1% then goto L50200
               f1% = 0%
               barcode$ = entry2$
               if barcode$ <> " " then goto L50115

               barcode_d$ = " "
               barcode_d$ = hex(06) & "Select a Valid Glass Barcode?"
               call "PLOWCODE" (#1,barcode$, barcode_d$, 0%, 0.00, f1%)
                                                 /* (PAR000)           */
L50115:    init(" ") rm_key$, rm_rec$(), errormsg$
           str(rm_key$,1%,9%)  = barcode$        /* Glass Barcode      */
           read #1,key > rm_key$, using L50120 , rm_rec$(),             ~
                                                 eod goto L50130
L50120:       FMT 2*CH(192) 
        REM - check for Valid Barcode for Glass
              if str(rm_rec$(),22%,9%) <> barcode$ then goto L50130
 
              entry2$ = barcode$
        return
L50130:    errormsg$ = "(Error) - Invalid Glass Barcode???"
           gosub error_prompt
           init(" ") barcode$, entry2$
        return
                                                  /* (PAR000)          */
L50200: REM Glass Prod Date and Batch Name       gl_date$, gl_dte$
            init(" ") gl_dte$, gl_date$
            gl_date$ = entry2$
            if gl_date$ <> " " then goto L50210
               goto L50220
L50210:     call "DATEOKC" (gl_date$, 0%, errormsg$)
            gl_dte$ = gl_date$

            call "DATUFMTC" (gl_dte$)
            entry2$ = gl_date$
            goto L50300                       /* Batch Name         */
        return
L50220:     errormsg$ ="(Error)= Production Date is Required?"
            gosub error_prompt
            init(" ") gl_date$, gl_dte$, entry2$
        return

L50300: REM Glass Production Batch Name        gl_batch$
            gl_batch$ = entry3$

            gl_key$ = all(hex(00))
            str(gl_key$,1%,1%)  = sel$ 
            str(gl_key$,2%,6%)  = str(gl_dte$,1%,6%)
            str(gl_key$,8%,20%) = gl_batch$

            read #4,key > gl_key$, using L50305, gl_key$, eod goto L50310
L50305:        FMT CH(32)

            if str(gl_key$,2%,6%) <> str(gl_dte$,1%,6%) then goto L50315

            if str(gl_key$,8%,20%) <> gl_batch$ then goto L50320
               entry3$ = gl_batch$
 
               goto L50400
        return
L50310:     errormsg$ = "(Error) No Data for Production Date/Batch Name?"
            gosub error_prompt
            init(" ") gl_batch$, gl_date$, gl_dte$, entry2$, entry3$
        return
L50315:     errormsg$ = "(Error) Invalid Production Date?"
            gosub error_prompt
            init(" ") gl_date$, gl_dte$, gl_batch$, entry2$, entry3$
        return
L50320:     errormsg$ = "(Error) Invalid Batch Name?"
            gosub error_prompt
            init(" ") gl_date$, gl_dte$, gl_batch$, entry2$, entry3$
        return
  
L50400: REM Glass Beginning and Ending Barcode   gl_beg$, gl_end$
            if len(entry4$) < 8 then entry4$ = "ALL      "
            if len(entry4$) > 8 and len(entry5$) < 8 then                ~
                                                        entry5$ = entry4$
 
            if len(entry5$) < 8 then entry5$ = "ALL      "
            gl_beg$ = entry4$
            gl_end$ = entry5$
  
        return
  
        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_single_label                        /* (EWD001) - Mod    */
            init (" ") gl_rec$, gl_reb$           /* CR2304 */
            Call "SHOSTAT" ("Printing Glass Barcode 'Single Label'")

            gl_rec$ = "00000000000000000000000000000000000000000000000000"~
                    & "00000000000000000000000000000000000000000000000000"~
                    & "00000000000000000000000000000000000000000000000000"~
                    & "00000000000000000000000000000000000000000000000000"
                                                  /* Dummy Label             */
                                                  /* (PAR000) Channel #1 Add */
            call "EWDPLF72" (gl_rec$, gl_reb$, sel%, #1, #2, #3, er%)  
            init(" ") gl_rec$, gl_reb$                     /* (PAR000) CR2304 */
            
            if schema% = 2% then goto L00600  
            str(gl_ky2$,1%,9%) = str(entry2$,1%,9%)
            str(gl_ky2$,10%,2%) = "000"            
            read #4,key 1 > gl_ky2$, using L00500, gl_reb$,        ~
                                             eod goto L00600
L00500:       FMT POS(257), CH(128)    
L00600:         
            gl_rec$ = str(rm_rec$(),1%,256%)      /* Use Re-Make data for    */
                                                  /* Single Label Print      */
 
            call "EWDPLF72" (gl_rec$, gl_reb$, sel%, #1, #2, #3, er%)
               /* CR2304 Real Label     */
            if er% <> 0% then gosub print_error 
                 
            init (" ") gl_rec$
            gl_rec$ = "00000000000000000000000000000000000000000000000000"~
                    & "00000000000000000000000000000000000000000000000000"~
                    & "00000000000000000000000000000000000000000000000000"~
                    & "00000000000000000000000000000000000000000000000000"

                                                 /* (PAR000) Channel #1 Add */
            call "EWDPLF72" (gl_rec$, gl_reb$, sel%, #1, #2, #3, er%)
                 /* Dummy Label  */

        return clear all
        goto inputmode

        print_labels
           call "SHOSTAT" ("Printing " & sel_d$ & " for " & gl_date$)
            
           count% = 0%
 
           init(" ") gl_key$, gl_sav$
           str(gl_key$,1%,1%) = sel$
           str(gl_key$,2%,6%) = str(gl_dte$,1%,6%)
           str(gl_key$,8%,20%) = gl_batch$
           gl_sav$ = str(gl_key$,1%,27%)
        print_labels_next
           read #4,key > gl_key$, using L50500, gl_rec$, gl_reb$,        ~
                                             eod goto print_labels_done  
L50500:       FMT CH(256), CH(128) 
           gl_key$ = str(gl_rec$,1%,32%)
           if gl_sav$ <> str(gl_key$,1%,27%) then goto print_labels_done

/* CR2913 */ 
           if str(gl_rec$,54%,8%) = " " then print_labels_next
           
           if str(gl_beg$,1%,3%) = "ALL" then goto L50510
              if str(gl_rec$,33%,9%) < gl_beg$ or                      ~
                 str(gl_rec$,33%,9%) > gl_end$ then goto print_labels_next  

L50510:    
 
                                               /* (PAR000) Channel #1 Add */
                                               /* (PAR001) (PAR002)       */
           if sel% = 2% then                                           ~
               call "EWDPLE72" (gl_rec$, gl_reb$, sel%, #1, #2, er%) 
                                               /* Production  CR2304 */
                                               /* (PAR000) Channel #1 Add */
           if sel% = 3% then                                           ~
               call "EWDPLF72" (gl_rec$, gl_reb$, sel%, #1, #2, #3, er%)
                                                /* Re-Make   CR2304 */
                                               /* (PAR000) Channel #1 Add */
           if sel% = 4% then                                           ~
               call "EWDPLG72" (gl_rec$, gl_reb$, sel%, #1, #2, #3, er%)
                                                            /* Special  CR2304*/
                                                            /* (EWD002)   */ 
                                                            /* (EWD006)   */ 
                                                            /* (EWD007)   */ 
                                               /* (PAR000) Channel #1 Add */
           if sel% = 5% then                                           ~
               call "EWDPLH72" (gl_rec$, gl_reb$, sel%, #1, #2, er%) 
                                                            /* Spec Shapes*/
                                                            /* CR2304     */
                                                            /* (EWD002)   */
                                                            /* (EWD006)   */
                                                            /* (EWD007)   */
                                                            /* (EWD004)   */  
                                               /* (PAR000) Channel #1 Add */
                                               /* (PAR001) (PAR002)       */
                                               /* (AWD009) channel #5     */
           if sel% = 6% then                   /* (IM8022) replaced #5 for #1*/~
               call "EWDPLE72" (gl_rec$, gl_reb$, sel%, #1, #2, er%) 
                                               /* Tempered  CR2304  */

                                               /* (PAR000) Channel #1 Add */
                                               /* (AWD009) channel #5     */
           if sel% = 7% then                   /*(IM8022)*/                ~
               call "EWDPLF72" (gl_rec$, gl_reb$, sel%, #1, #2, #3, er%)
                                                            /* Re-Mak Temp*/
                                                            /* CR2304     */
                                                            /* (EWD004)   */ 

                                                            /* (AWD008)   */  
                                               /* (PAR000) Channel #1 Add */
                                               /* (PAR001) (PAR002)       */
           if sel% = 8% then                                           ~
               call "EWDPLE72" (gl_rec$, gl_reb$, sel%, #1, #2, er%) 
                                                            /* Pre-Cut    */
                                                            /* CR2304     */

           if sel% = 9% then                                           ~
               call "EWDPLE72" (gl_rec$, gl_reb$, sel%, #1, #2, er%) 
                                                            /* Pre-Cut    */
                                                            /* CR2304     */


           if er% <> 0% then gosub print_error
                                                       /* (EWD003)    */
           count% = count% + 1%                        /* (RHHTEST)   */

        REM   if count% > 50% then goto print_labels_done

                                                       /* (EWD003)    */ 
           goto print_labels_next                      /* (RHHTEST)   */
        print_labels_done
                                              /* (PAR000) Channel #1 Add */
                                              /* (PAR001) (PAR002)       */
           if sel% = 2% then                                           ~
               call "EWDPLE72" (gl_rec$, gl_reb$, 99%, #1, #2, er%) 
                                                           /* Production */
                                                           /* CR2304     */
                                                           /* (EWD002)   */
                                                           /* (EWD006)   */
                                                           /* (EWD007)   */
                                              /* (PAR000) Channel #1 Add */
           if sel% = 5% then                                           ~
               call "EWDPLH72" (gl_rec$, gl_reb$, 99%, #1, #2, er%) 
                                                           /* Spec Shape */
                                                           /* CR2304     */
                                                           /* (EWD006)   */
                                                           /* (EWD007)   */

                                                           /* (EWD004)   */  
                                              /* (PAR000) Channel #1 Add */
                                              /* (PAR001) (PAR002)       */
           if sel% = 6% then                                           ~
               call "EWDPLE72" (gl_rec$, gl_reb$, 99%, #1, #2, er%) 
                                                           /* Tempered   */
                                                           /* CR2304     */
                                                           /* (AWD008)   */  
                                              /* (PAR000) Channel #1 Add */
                                              /* (PAR001) (PAR002)       */
           if sel% = 8% then                                           ~
               call "EWDPLE72" (gl_rec$, gl_reb$, 99%, #1, #2, er%) 
                                                           /* Pre-Cut    */
                                                           /* CR2304     */
               
           if sel% = 9% then                                           ~
               call "EWDPLE72" (gl_rec$, gl_reb$, 99%, #1, #2, er%) 
                                                           /* Pre-Cut    */
                                                           /* CR2304     */
                                                           /* (EWD004)   */ 
                                                           /* (EWD002)   */
        return clear all
        goto inputmode

        print_error
           convert er% to error$, pic(#)

           if sel% = 1% then                                           ~
               errormsg$ = "(Error) Printing Single Glass Label? " &   ~
               str(gl_rec$,33%,12%) & " (" & error$ & ")"

           if sel% = 2% then                                           ~
               errormsg$ = "(Error) Printing Production Glass? " &     ~
               str(gl_rec$,33%,12%) & " (" & error$ & ")"

           if sel% = 3% then                                           ~
               errormsg$ = "(Error) Printing Re-Make Glass? "  &       ~
               str(gl_rec$,33%,12%) & " (" & error$ & ")"
 
           if sel% = 4% then                                           ~
               errormsg$ = "(Error) Printing Special Glass? "  &       ~
               str(gl_rec$,33%,12%) & " (" & error$ & ")"
                                                  /* (EWD002)          */
           if sel% = 5% then                                           ~
               errormsg$ = "(Error) Printing Special Shapes?"  &       ~
               str(gl_rec$,33%,12%) & " (" & error$ & ")"
                                                  /* (EWD002)          */
                                                  /* (EWD004)          */
           if sel% = 6% then                                           ~
               errormsg$ = "(Error) Printing Tempered Glass? " &       ~
               str(gl_rec$,33%,12%) & " (" & error$ & ")"

           if sel% = 7% then                                           ~
               errormsg$ = "(Error) Printing Remake Temp Glass?" &     ~
               str(gl_rec$,33%,12%) & " (" & error$ & ")"
                                                  /* (EWD004)          */
  
          gosub error_prompt
        return
                                                  /* (EWD001)          */      
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
                              
        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            end

