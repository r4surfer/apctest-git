        REM *************************************************************~
            *  Program Name      - AWDSCRLB                             *~
            *  Creation Date     - 01/19/2010                           *~
            *                                                           *~
            *  Description       - This Program Creates the Screen      *~
            *                      labels replacing the cyberquery      *~
            *                      label reports.                       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *01/19/2010! New Program                              ! DES *~
            *************************************************************

        dim                              /* FILE = APCPLNDT            */~
	     scr_name$35,                 ~
	     scr_addr1$29,                ~
	     scr_addr2$20,                ~
	     scr_item$30,                 ~
	     scr_part$30,                 ~
	     tmp_str$35,                  ~
	     blank$35,                    ~
	     scr_city$20, scr_state$5, scr_attn$30, scr_tel$30

        dim                              /* (Program Variables)        */~
            hdr$40, msg$(3%)79,          /* ASKUSER TEXT               */~
            readkey$30,                  /* GENCODES Look-Up Key       */~
            scr_dte$8,                   /* Screen Completion Date FORM*/~
            scr_dte1$8,                  /* Screen Comp. Date Unform   */~
            scr_code$1,                  /* Screen Report Selection    */~
            scr_msg$30,                  /* Screen - Report Selection  */~
            scr_msg1$30, l_txt$25,       /* Screen - Product Line      */~
            scr_desc$30,                 /* Screen - Load Number       */~
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

        dim f2%(20%),                    /* = 0 if the file is open    */~
            fs%(20%),                    /* = 1 if file open, -1 if it */~
            rslt$(20%)20                 /* Text from file opening     */

        dim a$256, b$256

        dim xx$(90%)255,lbl$(40)252
        dim l1$(90%)255, l2$(90%)255 ,l3$(90)255
        dim l4$(90%)255, l5$(90%)255,                                  ~      
            file$8,                      /* Lbl Print File             */~
            library$8,                   /* Library Name = APCDATA     */~
            script$8,                    /* Lbl SHELL SCRIPT           */~
            volume$6                     /* DISK VOLUME = CARLOS       */

        dim mon$(12)3


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$41, pname$21
            apc$   = "(New)Planning Screen Processing Utility  "
            pname$ = "SCRLABEL - Rev: 01.00"

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
            * #2 ! HNYMASTR !                                           *~
            * #3 ! GENCODES ! Master System Table File                  *~
            * #4 ! CUSTOMER ! Master Customer File                      *~
            * #5 ! MFGSCX   ! Print File for AES Screen Labels          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #2,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #4,   "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #5,  "MFGSCX", varc, consec, recsize = 256



            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#2,  fs%(2%),  f2%(2%),   0%, rslt$(2%))
            call "OPENCHCK" (#3,  fs%(3%),  f2%(3%),   0%, rslt$(3%))
            call "OPENCHCK" (#4,  fs%(4%),  f2%(4%),   0%, rslt$(4%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            time$ = time
           hr$ = str(time$,1%,2%) 
           mm$ = str(time$,3%,2%)
           a_m$ = "AM"
           hr% = 0%
           convert hr$ to hr%, data goto t_1
T_1:
           if hr% >= 12% then a_m$ = "PM"
           if hr% >= 12% then hr% = hr% - 12%
           convert hr% to hr$, pic(00)

            call "DATEFMT" (date$)
	    fs$ = "^FS"
	    been_here% = 0%
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            mon$(01) = "JAN"
            mon$(02) = "FEB"
            mon$(03) = "MAR"
            mon$(04) = "APR"
            mon$(05) = "MAY"
            mon$(06) = "JUN"
            mon$(07) = "JUL"
            mon$(08) = "AUG"
            mon$(09) = "SEP"
            mon$(10) = "OCT"
            mon$(11) = "NOV"
            mon$(12) = "DEC"
	    blank$ = "                                   "
            gosub load_label
            gosub open_file

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

	    scr_code% = 0%
	    scr_nbr$ = "01"
	    scr_nbr% = 1%
            for fieldnr% = 1% to  1%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%
                  if keyhit% = 0% and scr_code$ = "1" then goto inputmode2
                  if keyhit% = 0% and scr_code$ = "2" then goto inputmode3
                  if keyhit% = 0% and scr_code$ = "3" then goto inputmode4
                  if keyhit% = 0% and scr_code$ = "4" then goto inputmode5
                  if keyhit% = 0% and scr_code$ = "5" then goto inputmode6
            goto editpg1

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
REM               if keyhit%  = 16% then gosub begin_process
                  if keyhit%  = 16% then goto exit_program        
                  if keyhit% = 0% and scr_code$ = "1" then goto inputmode2
                  if keyhit% = 0% and scr_code$ = "2" then goto inputmode3
                  if keyhit% = 0% and scr_code$ = "3" then goto inputmode4
                  if keyhit% = 0% and scr_code$ = "4" then goto inputmode5
                  if keyhit% = 0% and scr_code$ = "5" then goto inputmode6
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 1% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
                  if keyhit% = 16% and scr_code$ = "1" then goto inputmode2
                  if keyhit% = 16% and scr_code$ = "2" then goto inputmode3
                  if keyhit% = 16% and scr_code$ = "3" then goto inputmode4
                  if keyhit% = 16% and scr_code$ = "4" then goto inputmode5
                  if keyhit% = 16% and scr_code$ = "5" then goto inputmode6
            goto L11120

        inputmode2
            gosub initialize_variables2

	    scr_code% = 1%
            for fieldnr% = 1% to  4%
L12000:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L12040
L12010:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L12030
L12020:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L12010
                         if fieldnr% = 1% then L12000
                         goto L12020
L12030:               if keyhit% = 16% and fieldnr% = 1% then inputmode  
                      if keyhit% <> 0% then       L12010
L12040:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12010
            next fieldnr%
            goto editpg2

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub print_screen_cart_label
                  if keyhit% <>  0% then       editpg2
L12060:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 4% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L12070:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12070
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12070
                  lastfieldnr% = fieldnr%
            goto L12060


        inputmode3
            gosub initialize_variables3

	    scr_code% = 2%
            for fieldnr% = 1% to  5%
L13000:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L13040
L13010:         gosub'103(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L13030
L13020:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L13010
                         if fieldnr% = 1% then L13000
                         goto L13020
L13030:               if keyhit% = 16% and fieldnr% = 1% then inputmode  
                      if keyhit% <> 0% then       L13010
L13040:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L13010
            next fieldnr%
            goto editpg3

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg3
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub print_patio_door_label
                  if keyhit% <>  0% then       editpg3
L13060:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 5% then editpg3
            if fieldnr% = lastfieldnr% then    editpg3
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg3
L13070:     gosub'103(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13070
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13070
                  lastfieldnr% = fieldnr%
            goto L13060

        inputmode4
            gosub initialize_variables4

	    scr_code% = 3%
            for fieldnr% = 1% to  6%
L14000:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L14040
L14010:         gosub'104(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L14030
L14020:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L14010
                         if fieldnr% = 1% then L14000
                         goto L14020
L14030:               if keyhit% = 16% and fieldnr% = 1% then inputmode  
                      if keyhit% <> 0% then       L14010
L14040:         gosub'154(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L14010
            next fieldnr%
            goto editpg4

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg4
            lastfieldnr% = 0%
            gosub'104(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub print_shipping_label  
                  if keyhit% <>  0% then       editpg4
L14060:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 6% then editpg4
            if fieldnr% = lastfieldnr% then    editpg4
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg4
L14070:     gosub'104(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L14070
            gosub'154(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L14070
                  lastfieldnr% = fieldnr%
            goto L14060


        inputmode5
            gosub initialize_variables5

	    scr_code% = 4%
            for fieldnr% = 1% to 11%
L15000:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L15040
L15010:         gosub'105(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L15030
L15020:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L15010
                         if fieldnr% = 1% then L15000
                         goto L15020
L15030:               if keyhit% = 16% and fieldnr% = 1% then inputmode  
                      if keyhit% <> 0% then       L15010
L15040:         gosub'155(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L15010
            next fieldnr%
            goto editpg5

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg5
            lastfieldnr% = 0%
            gosub'105(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub print_packing_label    
                  if keyhit% <>  0% then       editpg5
L15060:     fieldnr% = cursor%(1%) / 2%
            if cusrsor%(1%) = 8% and  cusrsor%(2%) > 49 then          ~
			     fieldnr% = fieldnr% + 1%
            if cusrsor%(1%) = 8% and  cusrsor%(2%) > 62 then          ~
			     fieldnr% = fieldnr% + 1%
            if cusrsor%(1%) > 8 then fieldnr% = fieldnr% + 2%
            if fieldnr% < 1% or fieldnr% > 11% then editpg5
            if fieldnr% = lastfieldnr% then    editpg5
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg5
L15070:     gosub'105(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L15070
            gosub'155(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L15070
                  lastfieldnr% = fieldnr%
            goto L15060


        inputmode6
            gosub initialize_variables6

	    scr_code% = 5%
            for fieldnr% = 1% to  2%
L16000:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L16040
L16010:         gosub'106(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L16030
L16020:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L16010
                         if fieldnr% = 1% then L16000
                         goto L16020
L16030:               if keyhit% = 16% and fieldnr% = 1% then inputmode  
                      if keyhit% <> 0% then       L16010
L16040:         gosub'156(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L16010
            next fieldnr%
            goto editpg6

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg6
            lastfieldnr% = 0%
            gosub'106(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub print_roll_form_label 
                  if keyhit% <>  0% then       editpg6
L16060:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 2% then editpg6
            if fieldnr% = lastfieldnr% then    editpg6
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg6
L16070:     gosub'106(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L16070
            gosub'156(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L16070
                  lastfieldnr% = fieldnr%
            goto L16060

        REM *************************************************************~
            *             P R O C E S S   D A T A                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************


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
            if scrnr% = 2% then restore line = scrn2_msg, fieldnr%
            if scrnr% = 3% then restore line = scrn3_msg, fieldnr%
            if scrnr% = 4% then restore line = scrn4_msg, fieldnr%
            if scrnr% = 5% then restore line = scrn5_msg, fieldnr%
            if scrnr% = 6% then restore line = scrn6_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Selection Code                                         "

        scrn2_msg  :  data                                               ~
         "Enter Line                                                  ", ~
         "Enter Day (1=Mon, 2=Tue, 3=Wed, 4=Thu, 5=Fri, 6-Sat, 7-Sun) ", ~
         "Enter Date                                                    "

        scrn3_msg  :  data                                               ~
         "Enter Item Number                                             ", ~
         "Enter Quantity                                                ", ~
         "Enter P.O. Number                                             ", ~
         "Enter Due Date                                                "

        scrn4_msg  :  data                                               ~
         "Enter Part Number                                             ", ~
         "Enter Description                                             ", ~
         "Enter Color                                                   ", ~
         "Enter Sub Inv                                                 ", ~
         "Enter a Customer Name                                         "

        scrn5_msg  :  data                                               ~
         "Enter a Customer Number (or blank)                            ", ~
         "Enter a Customer Name                                         ", ~
         "Enter a Address Line 1                                        ", ~
         "Enter a Address Line 2                                        ", ~
         "Enter a City                                                  ", ~
         "Enter a State                                                 ", ~
         "Enter a Zip Code                                              ", ~
         "Enter a Country                                               ", ~
         "Enter a Contact Name                                          ", ~
         "Enter a Contact Telephone Number                              "

        scrn6_msg  :  data                                               ~
         "Enter a Starting Sequence number                              ", ~
         "Enter a Ending Sequence number                              "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, scr_code$, scr_msg$,       ~
                      scr_dte$, scr_dte1$, scr_msg1$
        return

        initialize_variables2
            init(" ") errormsg$, inpmessage$, scr_code$, scr_msg$,       ~
                      scr_dte$, scr_dte1$, scr_msg1$,                    ~
                      scr_line$, desc$, scr_day$, doy$, scr_date$ 
        return

        initialize_variables3
            init(" ") errormsg$, inpmessage$, scr_code$, scr_msg$,       ~
                      scr_dte$, scr_dte1$, scr_msg1$,                    ~
                      scr_item$, scr_qty$, scr_po$, scr_date$
        return

        initialize_variables4
            init(" ") errormsg$, inpmessage$, scr_code$, scr_msg$,       ~
                      scr_dte$, scr_dte1$, scr_msg1$, scr_name$,         ~
                      scr_part$, scr_desc$, scr_color$, scr_sub_inv$  
        return

        initialize_variables5
            init(" ") errormsg$, inpmessage$, scr_code$, scr_msg$,       ~
                      scr_dte$, scr_dte1$, scr_msg1$,                    ~
                      scr_cust$, scr_name$, scr_addr1$, scr_addr2$,      ~
                      scr_city$, scr_state$, scr_zip$, scr_country$,     ~
                      scr_attn$, scr_tel$
        return

        initialize_variables6
            init(" ") errormsg$, inpmessage$, scr_code$, scr_msg$,       ~
                      scr_dte$, scr_dte1$, scr_msg1$, scr_start$, scr_end$
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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40210          /* Screen's Selection*/  

              goto L42100

L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40210:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42100:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,20), fac(hex(a4)), apc$                   , ch(41),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (07,02), "Screen Selections (1-5) :",                  ~
               at (07,30), fac(lfac$(1%)), scr_code$            , ch(01),~
               at (07,40), fac(hex(84)), scr_msg$               , ch(30),~
                                                                         ~
               at (10,21), "**************************************",     ~
               at (11,21), "*                                    *",     ~
               at (12,21), "*  Selection Codes                   *",     ~
               at (13,21), "*  1  =  Screen Cart Label           *",     ~
               at (14,21), "*  2  =  Patio Door Label            *",     ~
               at (15,21), "*  3  =  Shipping Label              *",     ~
               at (16,21), "*  4  =  Packing Label               *",     ~
               at (17,21), "*  5  =  Roll Form Labels            *",     ~
               at (18,21), "*                                    *",     ~
               at (19,21), "**************************************",     ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L42110
                  call "PRNTSCRN"
                  goto L42100

L42110:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
           l_txt$ = "Production Load or Blank:"
           if  scr_code% = 3% then l_txt$ = "Schedule No. for Lineal :"

        if edit% = 2% then L42140     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L42120
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L42120:     if fieldnr% > 1% then L42130
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L42130:     return

L42140: if fieldnr% > 0% then L42150  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Print Labels"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return
L42150:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                 (9)Display Dept's      " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffff09ffffffffffffff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'050(2%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
                                                         /* (AWD002)   */
              on fieldnr% gosub L40200,     /* Line                    */ ~
                                L40210,     /* Day                     */ ~
                                L40210,     /* Date                    */ ~
                                L40210      /* Nbr Lbls                */

REM ---- screen cart label ------
REM  01 = line     i.e. 411
REM  02 = day      i.e. 4  
REM  03 = date     i.e. APR 10/03

L42200:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Line: ",                                     ~
               at (03,30), fac(lfac$(1%)), scr_line$             , ch(03),~
               at (03,36), fac(hex(8c)), desc$                  , ch(30),~
                                                                         ~
               at (05,02), "Day (1-7): ",                                ~
               at (05,30), fac(lfac$(2%)), scr_day$              , ch(01),~
               at (05,36), fac(hex(8c)), doy$                   , ch(30),~
                                                                         ~
               at (07,02), "Date: ",                                     ~
               at (07,30), fac(lfac$(3%)), scr_date$             , ch(10),~
                                                                         ~
               at (09,02), "# Labels: ",                                     ~
               at (09,30), fac(lfac$(4%)), scr_nbr$              , ch(02),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L42210
                  call "PRNTSCRN"
                  goto L42200

L42210:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L42250     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L42230
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L42230:     if fieldnr% > 1% then L42240
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L42240:     return

L42250: if fieldnr% > 0% then L42260  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                   (15)Print Screen    "
            pf$(3%) = "                                        " &        ~
                      "                   (16)Print Label     "
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)


            return
L42260:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                                       "
            pf$(3%) = "                                        " &        ~
                      "                                       " 
            pfkeys$ = hex(01ffff04ffffffffffffffffffffffff00)
            return

          REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
              gosub'050(3%, fieldnr%)
              gosub set_pf3
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
                                                         /* (AWD002)   */
              on fieldnr% gosub L40200,     /* Item                    */ ~
                                L40210,     /* Qty                     */ ~
                                L40200,     /* PO                      */ ~
                                L40210,     /* Date                    */ ~
                                L40210      /* Nbr                     */

REM ---- patio door label -------
REM  04 = item #   i.e. KINRO-GA36 7/8 X77 7/8WHTPD
REM  05 = qty      i.e. 10 
REM  06 = PO       i.e. 59433
REM  07 = due date i.e. 5/20/08

L42300:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Item Number: ",                              ~
               at (03,25), fac(lfac$(1%)), scr_item$             , ch(30),~
                                                                         ~
               at (05,02), "Qty: ",                                      ~
               at (05,25), fac(lfac$(2%)), scr_qty$              , ch(06),~
               at (07,02), "P.O.:",                                      ~
               at (07,25), fac(lfac$(3%)), scr_po$               , ch(08),~
                                                                         ~
               at (09,02), "Due Date:",                                  ~
               at (09,25), fac(lfac$(4%)), scr_date$             , ch(08),~
                                                                         ~
               at (11,02), "# Labels: ",                                     ~
               at (11,30), fac(lfac$(5%)), scr_nbr$              , ch(02),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L42310
                  call "PRNTSCRN"
                  goto L42300

L42310:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
        if edit% = 2% then L42340     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            return

L42340: if fieldnr% > 0% then L42350  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                   (15)Print Screen    "
            pf$(3%) = "                                        " &        ~
                      "                   (16)Print Label     "
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)


            return
L42350:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                                       "
            pf$(3%) = "                                        " &        ~
                      "                                       " 
            pfkeys$ = hex(01ffff04ffffffffffffffffffffffff00)
            return
  
        REM *************************************************************~
            *               S C R E E N   P A G E   4                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'104(fieldnr%, edit%)
              gosub'050(4%, fieldnr%)
              gosub set_pf4
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
                                                         /* (AWD002)   */
              on fieldnr% gosub L40200,     /* Part Number             */ ~
                                L40200,     /* Description             */ ~
                                L40200,     /* Color                   */ ~
                                L40200,     /* Sub Inv                 */ ~
                                L40200,     /* Customer                */ ~
                                L40210      /* Nbr Lines               */ 

REM ---- shipping label ---------
REM  08 = desc     i.e. 7/16 x 3/4 SCREEN FRAME    
REM  09 = PART #   i.e. 507861440
REM  10 = color    i.e. WHITE
REM  11 = SUB INV  i.e. 100
REM  12 = date stamp i.e. 18-01-10    1.44 PM

L42400:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Part Number: ",                              ~
               at (03,30), fac(lfac$(1%)), scr_part$             , ch(25),~
                                                                         ~
               at (05,02), "Description: ",                              ~
               at (05,30), fac(lfac$(2%)), scr_desc$             , ch(30),~
                                                                         ~
               at (07,02), "Color: ",                                    ~
               at (07,30), fac(lfac$(3%)), scr_color$            , ch(08),~
                                                                         ~  
               at (09,02), "Sub Inv.: ",                                 ~
               at (09,30), fac(lfac$(4%)), scr_sub_inv$          , ch(08),~
                                                                         ~
               at (11,02), "Customer: ",                                 ~
               at (11,30), fac(lfac$(5%)), scr_name$             , ch(35),~
                                                                         ~
               at (13,02), "# Labels: ",                                     ~
               at (13,30), fac(lfac$(6%)), scr_nbr$              , ch(02),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L42410
                  call "PRNTSCRN"
                  goto L42400

L42410:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf4
        if edit% = 2% then L42440     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L42420
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L42420:     if fieldnr% > 1% then L42430
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L42430:     return

L42440: if fieldnr% > 0% then L42450  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                   (15)Print Screen    "
            pf$(3%) = "                                        " &        ~
                      "                   (16)Print Label     "
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)


            return
L42450:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                                       "
            pf$(3%) = "                                        " &        ~
                      "                                       " 
            pfkeys$ = hex(01ffff04ffffffffffffffffffffffff00)
            return

          REM *************************************************************~
            *               S C R E E N   P A G E   5                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'105(fieldnr%, edit%)
              gosub'050(5%, fieldnr%)
              gosub set_pf5
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
                                                         /* (AWD002)   */
              on fieldnr% gosub L40200,     /* Customer Number         */ ~
                                L40200,     /* Customer Name           */ ~
                                L40200,     /* Addr1                   */ ~
                                L40200,     /* Addr2                   */ ~
                                L40200,     /* City                    */ ~
                                L40200,     /* State                   */ ~
                                L40200,     /* Zip                     */ ~
                                L40200,     /* Country                 */ ~
                                L40200,     /* Attn                    */ ~
                                L40200,     /* Tel                     */ ~
                                L40210      /* Nbr Lbls                */

REM ---- packing label ----------
REM  13 = addr1    i.e. ATRIUM
REM  14 = addr2    i.e. 100
REM  15 = addr3    i.e.    
REM  16 = city, st i.e. welcome, nc
REM  17 = zip      i.e. 27110
REM  18 = country  i.e.    
REM  19 = attn     i.e. CHRISTY
REM  20 = tel      i.e. 336-764-6400

L42500:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Customer Number: ",                          ~
               at (03,25), fac(lfac$(1%)), scr_cust$             , ch(09),~
                                                                         ~
               at (05,02), "Customer Name: ",                            ~
               at (05,25), fac(lfac$(2%)), scr_name$             , ch(35),~
                                                                         ~
               at (07,02), "Addr1: ",                                    ~
               at (07,25), fac(lfac$(3%)), scr_addr1$            , ch(29),~
                                                                         ~
               at (09,02), "Addr2: ",                                    ~
               at (09,25), fac(lfac$(4%)), scr_addr2$            , ch(20),~
                                                                         ~
               at (11,02), "City: ",                                     ~
               at (11,25), fac(lfac$(5%)), scr_city$             , ch(25),~
               at (11,52), "State: ",                                    ~
               at (11,59), fac(lfac$(6%)), scr_state$            , ch(05),~
               at (11,67), "Zip: ",                                      ~
               at (11,72), fac(lfac$(7%)), scr_zip$              , ch(07),~
                                                                         ~
               at (13,02), "Country: ",                                  ~
               at (13,25), fac(lfac$(8%)), scr_country$          , ch(08),~
                                                                         ~
               at (15,02), "Attn: ",                                     ~
               at (15,25), fac(lfac$(9%)), scr_attn$            , ch(25),~
                                                                         ~
               at (17,02), "Tel: ",                                      ~
               at (17,25), fac(lfac$(10%)), scr_tel$             , ch(25),~
                                                                         ~
               at (19,02), "# Labels: ",                                     ~
               at (19,30), fac(lfac$(11%)), scr_nbr$             , ch(02),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L42510
                  call "PRNTSCRN"
                  goto L42500

L42510:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf5
        if edit% = 2% then L42540     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L42520
                str(pf$(1%),60%) = " " : str(pfkeys$,10%,1%) = hex(ff)
L42520:     if fieldnr% > 1% then L42530
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L42530:     return

L42540: if fieldnr% > 0% then L42550  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                   (15)Print Screen    "
            pf$(3%) = "                                        " &        ~
                      "                   (16)Print Label     "
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)


            return
L42550:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                                       "
            pf$(3%) = "                                        " &        ~
                      "                                       " 
            pfkeys$ = hex(01ffff04ffffffffffffffffffffffff00)
            return
  
        REM *************************************************************~
            *               S C R E E N   P A G E   6                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'106(fieldnr%, edit%)
              gosub'050(6%, fieldnr%)
              gosub set_pf6
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
                                                         /* (AWD002)   */
              on fieldnr% gosub L40210,     /* Serial Number Start     */ ~
                                L40210      /* Serial Number End       */

REM ---- roll form label --------
REM  21 = seq #    i.e. 00000000088755
REM  22 = seq #    i.e. 00000000088756
REM  23 = seq #    i.e. 00000000088757
REM  24 = seq #    i.e. 00000000088758
REM  25 = seq #    i.e. 00000000088759

L42600:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Start Sequence Number: ",                    ~
               at (03,30), fac(lfac$(1%)), scr_start$            , ch(14),~
                                                                         ~
               at (05,02), "End Sequence Number: ",                    ~
               at (05,30), fac(lfac$(2%)), scr_end$              , ch(14),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L42610
                  call "PRNTSCRN"
                  goto L42600

L42610:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf6
        if edit% = 2% then L42640     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L42620
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L42620:     if fieldnr% > 1% then L42630
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L42630:     return

L42640: if fieldnr% > 0% then L42650  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                   (15)Print Screen    "
            pf$(3%) = "                                        " &        ~
                      "                   (16)Print Label(s)  "
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)


            return
L42650:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                                       "
            pf$(3%) = "                                        " &        ~
                      "                                       " 
            pfkeys$ = hex(01ffff04ffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50150          /* Screen's Selection    */  
            return

        deffn'152(fieldnr%)
REM ---- screen cart label ------
REM  01 = line     i.e. 411
REM  02 = day      i.e. 4  
REM  03 = date     i.e. APR 10/03
            errormsg$ = " "
            on fieldnr% gosub L50160,         /* Line    */ ~
                              L50170,         /* Day     */ ~
                              L50340,         /* Date    */ ~
                              L50380          /* # Lbl   */ 
            return

        deffn'153(fieldnr%)
REM ---- patio door label -------
REM  04 = item #   i.e. KINRO-GA36 7/8 X77 7/8WHTPD
REM  05 = qty      i.e. 10 
REM  06 = PO       i.e. 59433
REM  07 = due date i.e. 5/20/08
            errormsg$ = " "
            on fieldnr% gosub L50180,         /* Item    */ ~
                              L50190,         /* Qty     */ ~
                              L50200,         /* PO      */ ~
                              L50340,         /* Date    */ ~
                              L50380          /* # Lbl   */ 
            return

        deffn'154(fieldnr%)
REM ---- shipping label ---------
REM  08 = desc     i.e. 7/16 x 3/4 SCREEN FRAME    
REM  09 = PART #   i.e. 507861440
REM  10 = color    i.e. WHITE
REM  11 = SUB INV  i.e. 100
REM  12 = date stamp i.e. 18-01-10    1.44 PM
            errormsg$ = " "
            on fieldnr% gosub L50210,         /* Part    */  ~
                              L50330,         /* Desc    */ ~
                              L50220,         /* Color   */ ~
                              L50230,         /* Sub Inv */ ~
                              L50240,         /* Cust    */ ~
                              L50380          /* # Lbl   */ 
            return

        deffn'155(fieldnr%)
REM ---- packing label ----------
REM  13 = addr1    i.e. ATRIUM
REM  14 = addr2    i.e. 100
REM  15 = addr3    i.e.    
REM  16 = city, st i.e. welcome, nc
REM  17 = zip      i.e. 27110
REM  18 = country  i.e.    
REM  19 = attn     i.e. CHRISTY
REM  20 = tel      i.e. 336-764-6400
            errormsg$ = " "
            on fieldnr% gosub L50350,         /* Nbr   */  ~               
                              L50240,         /* Name  */  ~               
                              L50250,         /* Addr1 */  ~                
                              L50260,         /* Addr2 */  ~                
                              L50270,         /* City  */  ~                
                              L50280,         /* State */  ~                
                              L50290,         /* Zip   */  ~                
                              L50300,         /* Country */  ~                
                              L50310,         /* Attn  */  ~                
                              L50320,         /* Phone */   ~
                              L50380          /* # Lbl   */ 
            return

        deffn'156(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50360,         /* Screen's Selection    */  ~
                              L50370          /* Screen's Selection    */  
            return

L50150: REM Screen's Selection                    SCR_CODE$
            scr_code% = 0%
            if scr_code$ <> " " then goto L50152
               scr_code$ = "1"
L50152:     convert scr_code$ to scr_code%, data goto L50157
            if scr_code% < 1% or scr_code% > 5% then L50157
        return
L50157:     errormsg$ = "(Error) - Invalid Screen's Selection?"
            gosub error_prompt
            init(" ") scr_code$, scr_msg$
        return

L50160: REM Line                                  SCR_LINE$
            if scr_line$ = " " then goto L50167
           init(" ") readkey$, desc$               
REM        str(readkey$, 1%,9%)  = "MODEL    "
           str(readkey$, 1%,9%)  = "PLAN DEPT"
           str(readkey$,10%,15%) = scr_line$
           read #3,key = readkey$, using L50162, desc$,eod goto L50167
L50162:        FMT POS(25), CH(30)

        return
L50167:  
        return
            errormsg$ = "(Error) - Invalid Line?              "
            gosub error_prompt
            init(" ") scr_line$, scr_msg$
        return

L50170: REM Day (of week)                         SCR_DAY$ 
            scr_day% = 0%
            if scr_day$ = " " then goto L50177
            convert scr_day$ to scr_day%, data goto L50177
            if scr_day% < 1% or scr_day% > 7% then L50177
	    if scr_day% = 1% then doy$ = "Monday"
	    if scr_day% = 22 then doy$ = "Tuesday"
	    if scr_day% = 3% then doy$ = "Wednesday"
	    if scr_day% = 4% then doy$ = "Thursday"
	    if scr_day% = 5% then doy$ = "Friday"
	    if scr_day% = 6% then doy$ = "Saturday"
	    if scr_day% = 7% then doy$ = "Sunday"
        return
L50177:     errormsg$ = "(Error) - Invalid Day?               "
            gosub error_prompt
            init(" ") scr_day$, scr_msg$
        return

L50180: REM Item                                  SCR_ITEM$
            if scr_item$ = " " then goto L50187
        return
L50187:     errormsg$ = "(Error) - Invalid Item?              "
            gosub error_prompt
            init(" ") scr_item$, scr_msg$
        return

L50190: REM QTY                                   SCR_QTY$ 
            scr_qty% = 0%
            if scr_qty$ = " " then goto L50197
            convert scr_qty$ to scr_qty%, data goto L50197
        return
L50197:     errormsg$ = "(Error) - Invalid Qty?               "
            gosub error_prompt
            init(" ") scr_qty$, scr_msg$
        return

L50200: REM PO                                    SCR_PO$  
            if scr_po$ = " " then goto L50207
        return
L50207:     errormsg$ = "(Error) - Invalid PO?                "
            gosub error_prompt
            init(" ") scr_po$, scr_msg$
        return

L50210: REM Part                                  SCR_PART$
            if scr_part$ = " " then goto L50217
            read #2, key = scr_part$, eod goto L50211
            get #2, using HNYMASTR, scr_desc$        
HNYMASTR: FMT POS(26), CH(32)
L50211:
        return
L50217:     errormsg$ = "(Error) - Invalid Part?              "
            gosub error_prompt
            init(" ") scr_part$, scr_msg$
        return
         
L50220: REM Color                                 SCR_COLOR$
            if scr_color$ = " " then goto L50227
        return
L50227:     errormsg$ = "(Error) - Invalid Color?             "
            gosub error_prompt
            init(" ") scr_color$, scr_msg$
        return

L50230: REM SUB INV                               SCR_SUB_INV$  
            if scr_sub_inv$ = " " then goto L50237
        return
L50237:     errormsg$ = "(Error) - Invalid SUB INV?           "
            gosub error_prompt
            init(" ") scr_sub_inv$, scr_msg$
        return

L50240: REM Name                                  SCR_NAME$   
            if scr_name$ = " " then goto L50247
        return
L50247:     errormsg$ = "(Error) - Invalid Name?              "
            gosub error_prompt
            init(" ") scr_name$, scr_msg$
        return

L50250: REM Addr1                                 SCR_ADDR1$  
        return

L50260: REM Addr2                                 SCR_ADDR2$  
        return

L50270: REM City                                  SCR_CITY$  
        return

L50280: REM State                                 SCR_STATE$  
        return

L50290: REM Zip                                   SCR_ZIP$    
        return

L50300: REM Country                               SCR_COUNTRY$
        return

L50310: REM Attn                                  SCR_ATTN$   
        return

L50320: REM Phone                                 SCR_Phone$  
        return

L50330: REM Desc                                  SCR_DEC$    
        return

L50340: REM Planned Production Date               SCR_DTE$, SCR_DTE1$
           date% = 0%
           call "DATEOK" (scr_date$, date%, errormsg$ )

        return

L50350: REM Name                                  SCR_NAME$   
           init(" ") readkey$, desc$
	   if scr_cust$ <= "         " then L50357
           readkey$ = scr_cust$
           read #4,key = readkey$, using CUSTOMER,                  ~
	        scr_name$, scr_addr1$, scr_addr2$, scr_city$, scr_state$,  ~
	        scr_zip$, scr_attn$, scr_phone$, eod goto L50357
CUSTOMER:  FMT POS(253), CH(30), CH(30), CH(30), POS(403), CH(18), CH(2), ~
	       POS(424), CH(9), CH(20), CH(10) 
           scr_tel$ = "(" & str(scr_phone$,1,3) & ") " & str(scr_phone$,4,3) & ~
		      "-" & str(scr_phone$,7,4)
        return
L50357:                                                        
            init(" ") scr_msg$, scr_name$, scr_addr1$, scr_addr2$, scr_city$, ~
		      scr_state$, scr_zip$, scr_attn$, scr_tel$
        return

L50360: REM Start Seq #                           SCR_START$  
        convert scr_start$ to start%, data goto L50365
	convert start% to scr_start$, pic (00000000000000)
        return

L50365:     errormsg$ = "(Error) - Invalid Sequence Number    "
            gosub error_prompt
            init(" ") scr_name$, scr_msg$
        return

L50370: REM End Seq #                             SCR_END$    
        convert scr_end$ to end%, data goto L50376
	convert end%   to scr_end$,   pic (00000000000000)
        return

L50376:     errormsg$ = "(Error) - Invalid Sequence Range     "
            gosub error_prompt
            init(" ") scr_name$, scr_msg$
        return

L50380: REM # labels                              SCR_NBR$    
        return

print_screen_cart_label
      init (" ") lbl$()
      xx$() = l1$()
      l1$(14%) = "01^FO175,0500^CI0^A0B,300,175^FR^FD"
      if scr_line$ <> "007" and                                ~
         scr_line$ <> "049" and                                ~
         scr_line$ <> "005" then multi_line                        
      l1$(14%) = "01^FO175,0425^CI0^A0B,300,175^FR^FD"
multi_line:
      if scr_line$ = "007" then scr_line$ = "007A"
      if scr_line$ = "049" then scr_line$ = "049B"
      if scr_line$ = "005" then scr_line$ = "005C"
      lbl$(01) = scr_line$ & fs$      
      lbl$(02) = scr_day$ & fs$      
REM  03 = date     i.e. APR 10/03
      mon% = 13%
      convert str(scr_date$,1,2) to mon%, data goto bad_date
bad_date:
    lbl$(03) = mon$(mon%) & " " & str(scr_date$,4,2) & "/" & str(scr_date$,7,2)
      gosub print_label
      return clear all
      goto inputmode2

print_patio_door_label
      init (" ") lbl$()
      xx$() = l2$()
      lbl$(04) = scr_item$ & fs$
      lbl$(05) = scr_qty$ & fs$
      lbl$(06) = scr_po$ & fs$
      lbl$(07) = scr_date$ & fs$
      gosub print_label
      return clear all
      goto inputmode3

print_shipping_label  
      init (" ") lbl$()
      xx$() = l3$()
      x = len(scr_desc$)
      y = (31 - x) / 2
      tmp_str$ = str(blank$,1,y) & scr_desc$ 
      lbl$(08) = tmp_str$  & fs$ 
REM   lbl$(08) = scr_desc$ & fs$ 
      x = len(scr_part$)
      y = (21 - x) / 2
      tmp_str$ = str(blank$,1,y) & scr_part$ 
      lbl$(09) = tmp_str$  & fs$ 
REM   lbl$(09) = scr_part$ & fs$ 
      x = len(scr_color$)
      y = 18 - x
      tmp_str$ = str(blank$,1,y) & scr_color$
      lbl$(10) = tmp_str$   & fs$ 
REM   lbl$(10) = scr_color$ & fs$ 
      x = len(scr_sub_inv$)
      y = 10 - x
      tmp_str$ = str(blank$,1,y) & scr_sub_inv$
      lbl$(11) = tmp_str$     & fs$ 
REM   lbl$(11) = scr_sub_inv$ & fs$ 
      date$ = date
      call "DATEFMT" (date$)
      str(date$,3,1) = "-" 
      str(date$,6,1) = "-" 
      lbl$(12) = date$ & "    " & hr$ & ":" & mm$ & " " & a_m$ & fs$ 
REM  12 = date stamp i.e. 18-01-10    1.44 PM
      lbl$(13) = scr_name$  & fs$
      gosub print_label
      return clear all
      goto inputmode4

print_packing_label    
      init (" ") lbl$()
      xx$() = l4$()
      x = len(scr_name$)
      y = (29 - x) / 2
      tmp_str$ = str(blank$,1,y) & scr_name$ 
      lbl$(13) = tmp_str$   & fs$
REM   lbl$(13) = scr_name$  & fs$
      x = len(scr_addr1$)
      y = (29 - x) / 2
      tmp_str$ = str(blank$,1,y) & scr_addr1$
      lbl$(14) = tmp_str$   & fs$ 
REM   lbl$(14) = scr_addr1$ & fs$ 
      x = len(scr_addr2$)
      y = (29 - x) / 2
      tmp_str$ = str(blank$,1,y) & scr_addr2$
      lbl$(15) = tmp_str$   & fs$ 
REM   lbl$(15) = scr_addr2$ & fs$ 
      x = len(scr_city$) +  len(scr_state$) + 2
      y = (27 - x) / 2
      lbl$(16) = str(blank$,1,y) & scr_city$  & ", " &   ~
		 scr_state$ & fs$ 
REM   lbl$(16) = scr_city$  & ", " & scr_state$ & fs$ 
      lbl$(17) = scr_zip$   & fs$ 
      lbl$(18) = scr_country$ & fs$ 
      lbl$(19) = scr_attn$  & fs$ 
     
     if len(scr_tel$) <> 10% then goto not_10
      tel$ = "(" & str(scr_tel$,1,3) & ") " & str(scr_tel$,4,3) & "-" & ~
		   str(scr_tel$,7,4)
     goto lbl20
not_10:
     tel$ = scr_tel$
lbl20: lbl$(20) = tel$  & fs$ 
      gosub print_label
      return clear all
      goto inputmode5

print_roll_form_label 
      init (" ") lbl$()
      xx$() = l5$()
	for l% = start% to end% step 5%
	     convert l%      to seq1$, pic (00000000000000)
	     convert l% + 1% to seq2$, pic (00000000000000)
	     convert l% + 2% to seq3$, pic (00000000000000)
	     convert l% + 3% to seq4$, pic (00000000000000)
	     convert l% + 4% to seq5$, pic (00000000000000)
             lbl$(21) = seq1$   & fs$ 
             lbl$(22) = seq2$   & fs$ 
             lbl$(23) = seq3$   & fs$ 
             lbl$(24) = seq4$   & fs$ 
             lbl$(25) = seq5$   & fs$ 
	     l0% = (l% - 1%) / 5%
             lx% = l% - (l0% * 5%)
	     if (l% + 1%) > end% then gosub'000(lx% + 1%)
	     if (l% + 2%) > end% then gosub'000(lx% + 2%)
	     if (l% + 3%) > end% then gosub'000(lx% + 3%)
	     if (l% + 4%) > end% then gosub'000(lx% + 4%)
             gosub print_label
	next l%

        return clear all
        goto inputmode6

  deffn'000(line%)             
        ln% = line% * 2% + 10%
        xx$(ln% + 1%) = xx$(23%)
        xx$(ln% + 2%) = xx$(24%)
        xx$(ln% + 3%) = "               "
        return

load_label:
	/* positioning is 200/inch in both the X & Y axis */
        /* make sure you use font 0 (zero) not O          */

           init(" ") l1$(), l2$(), l3$(), l4$(), l5$()
  
REM ---- screen cart label ------
REM  01 = line     i.e. 411
REM  02 = day      i.e. 4  
REM  03 = date     i.e. APR 10/03
REM ---- patio door label -------
REM  04 = item #   i.e. KINRO-GA36 7/8 X77 7/8WHTPD
REM  05 = qty      i.e. 10 
REM  06 = PO       i.e. 59433
REM  07 = due date i.e. 5/20/08
REM ---- shipping label ---------
REM  08 = desc     i.e. 7/16 x 3/4 SCREEN FRAME    
REM  09 = PART #   i.e. 507861440
REM  10 = color    i.e. WHITE
REM  11 = SUB INV  i.e. 100
REM  12 = date stamp i.e. 18-01-10    1.44 PM
REM ---- packing label ----------
REM  13 = addr1    i.e. ATRIUM
REM  14 = addr2    i.e. 100
REM  15 = addr3    i.e.    
REM  16 = city, st i.e. welcome, nc
REM  17 = zip      i.e. 27110
REM  18 = country  i.e.    
REM  19 = attn     i.e. CHRISTY
REM  20 = tel      i.e. 336-764-6400
REM ---- roll form label --------
REM  21 = seq #    i.e. 00000000088755
REM  22 = seq #    i.e. 00000000088756
REM  23 = seq #    i.e. 00000000088757
REM  24 = seq #    i.e. 00000000088758
REM  25 = seq #    i.e. 00000000088759



           l1$( 1%) = "^JO"                          /* This format is used*/
           l1$( 2%) = "^XA^EG^XZ"
           l1$( 3%) = "^XA"
           l1$( 4%) = "^PMN"
           l1$( 5%) = "^MNY"
           l1$( 6%) = "^MMT"                          /* Back Feed Off */
           l1$( 7%) = "^MTT"
           l1$( 8%) = "^MD0"
           l1$( 9%) = "^LH0,0"
           l1$(10%) = "^LL2400"
           l1$(11%) = "^PR4"                          /* (AWD002)            */
           l1$(12%) = "^JMA"
           l1$(13%) = "^FO100,0700^CI0^A0B,60,55,FR^FDPRODUCTION LINE:^FS"
           l1$(14%) = "01^FO175,0500^CI0^A0B,300,175^FR^FD"
           l1$(15%) = "^FO475,0955^CI0^A0B,085,065^FR^FDDAY:^FS"
           l1$(16%) = "02^FO475,0870^CI0^A0B,085,065^FR^FD"
           l1$(17%) = "^FO625,0950^CI0^A0B,085,065^FR^FDDATE:^FS"
           l1$(18%) = "03^FO625,0625^CI0^A0B,085,065^FR^FD"
           l1$(19%) = "^PQ1"
           l1$(20%) = "^XZ"

           l2$( 1%) = "^JO"                          /* This format is used*/
           l2$( 2%) = "^XA^EG^XZ"
           l2$( 3%) = "^XA"
           l2$( 4%) = "^PMN"
           l2$( 5%) = "^MNY"
           l2$( 6%) = "^MMT"                          /* Back Feed Off */
           l2$( 7%) = "^MTT"
           l2$( 8%) = "^MD0"
           l2$( 9%) = "^LH0,0"
           l2$(10%) = "^LL2400"
           l2$(11%) = "^PR4"                          /* (AWD002)            */
           l2$(12%) = "^JMA"
           l2$(13%) = "^FO100,0765^CI0^A0B,120,60,FR^FDITEM NUMBER:^FS"
           l2$(14%) = "04^FO220,0265^CI0^A0B,120,60^FR^FD"
           l2$(15%) = "^FO475,1050^CI0^A0B,100,065^FR^FDQTY:^FS"
           l2$(16%) = "05^FO475,0975^CI0^A0B,100,065^FR^FD"
           l2$(17%) = "^FO475,0655^CI0^A0B,100,065^FR^FDPO:^FS"
           l2$(18%) = "06^FO475,0450^CI0^A0B,100,065^FR^FD"
           l2$(19%) = "^FO625,0875^CI0^A0B,085,065^FR^FDDUE DATE:^FS"
           l2$(20%) = "07^FO625,0650^CI0^A0B,085,065^FR^FD"
           l2$(21%) = "^PQ1"
           l2$(22%) = "^XZ"

           l3$( 1%) = "^JO"                          /* This format is used*/
           l3$( 2%) = "^XA^EG^XZ"
           l3$( 3%) = "^XA"
           l3$( 4%) = "^PMN"
           l3$( 5%) = "^MNY"
           l3$( 6%) = "^MMT"                          /* Back Feed Off */
           l3$( 7%) = "^MTT"
           l3$( 8%) = "^MD0"
           l3$( 9%) = "^LH0,0"
           l3$(10%) = "^LL2400"
           l3$(11%) = "^PR4"                          /* (AWD002)            */
           l3$(12%) = "^JMA"
           l3$(13%) = "08^FO620,0140^CI0^A0R,150,65,FR^FD"
           l3$(14%) = "09^FO480,0140^CI0^A0R,150,95^FR^FD"
           l3$(15%) = "10^FO390,0620^CI0^A0R,100,065^FR^FD"
           l3$(16%) = "11^FO290,0820^CI0^A0R,100,065^FR^FD"
           l3$(17%) = "^FO180,0460^CI0^A0R,030,025^FR^FDCustomer: ^FS"
           l3$(18%) = "13^FO180,0580^CI0^A0R,040,025^FR^FD"
           l3$(19%) = "^FO160,0060^CI0^A0R,030,025^FR^FDATRIUM GREENSBORO^FS"
 l3$(20%) = "^FO130,0060^CI0^A0R,030,025^FR^FD4501 GREEN PORT DRIVE, SUITE 104^FS"
           l3$(21%) = "^FO100,0060^CI0^A0R,030,025^FR^FDGREENSBORO, NC 27410^FS"
           l3$(22%) = "^FO070,0060^CI0^A0R,030,025^FR^FD336-605-8080^FS"
           l3$(23%) = "12^FO050,0800^CI0^A0R,040,025^FR^FD"
           l3$(24%) = "^PQ1"
           l3$(25%) = "^XZ"


           l4$( 1%) = "^JO"                          /* This format is used*/
           l4$( 2%) = "^XA^EG^XZ"
           l4$( 3%) = "^XA"
           l4$( 4%) = "^PMN"
           l4$( 5%) = "^MNY"
           l4$( 6%) = "^MMT"                          /* Back Feed Off */
           l4$( 7%) = "^MTT"
           l4$( 8%) = "^MD0"
           l4$( 9%) = "^LH0,0"
           l4$(10%) = "^LL2400"
           l4$(11%) = "^PR4"                          /* (AWD002)            */
           l4$(12%) = "^JMA"
           l4$(13%) = "13^FO630,0220^CI0^A0R,130,057,FR^FD"             
           l4$(14%) = "14^FO510,0220^CI0^A0R,130,057^FR^FD"   
           l4$(15%) = "15^FO390,0220^CI0^A0R,130,057^FR^FD"   
           l4$(16%) = "16^FO275,0340^CI0^A0R,130,057^FR^FD"
           l4$(17%) = "17^FO160,0640^CI0^A0R,130,057^FR^FD"
           l4$(18%) = "18^FO160,0900^CI0^A0R,130,057^FR^FD"
           l4$(19%) = "^FO095,0780^CI0^A0R,060,025^FR^FDATTN:^FS"
           l4$(20%) = "19^FO095,0860^CI0^A0R,060,020^FR^FD"
           l4$(21%) = "^FO040,0780^CI0^A0R,060,025^FR^FDTEL:^FS"
           l4$(22%) = "20^FO040,0860^CI0^A0R,060,020^FR^FD"
           l4$(23%) = "^FO160,0060^CI0^A0R,030,025^FR^FDATRIUM GREENSBORO^FS"
           l4$(24%) = "^FO130,0060^CI0^A0R,030,025^FR^FD4501 GREEN PORT DRIVE, SUITE 104^FS"
           l4$(25%) = "^FO100,0060^CI0^A0R,030,025^FR^FDGREENSBORO, NC 27410^FS"
           l4$(26%) = "^FO070,0060^CI0^A0R,030,025^FR^FD336-605-8080^FS"
           l4$(27%) = "^PQ1"
           l4$(28%) = "^XZ"

           l5$( 1%) = "^JO"                          /* This format is used*/
           l5$( 2%) = "^XA^EG^XZ"
           l5$( 3%) = "^XA"
           l5$( 4%) = "^PMN"
           l5$( 5%) = "^MNY"
           l5$( 6%) = "^MMT"                          /* Back Feed Off */
           l5$( 7%) = "^MTT"
           l5$( 8%) = "^MD0"
           l5$( 9%) = "^LH0,0"
           l5$(10%) = "^LL2400"
           l5$(11%) = "^PR4"                          /* (AWD002)            */
           l5$(12%) = "^JMA"
           l5$(13%) = "21^FO125,0275^CI0^A0N,25,25^FR^FD"
           l5$(14%) = "21^FO350,0275^BY2,2.0,40^BCN,40,N,N,N^FR^FD>:"
           l5$(15%) = "22^FO125,0450^CI0^A0N,25,25^FR^FD"
           l5$(16%) = "22^FO350,0450^BY2,2.0,40^BCN,40,N,N,N^FR^FD>:"
           l5$(17%) = "23^FO125,0625^CI0^A0N,25,25^FR^FD"
           l5$(18%) = "23^FO350,0625^BY2,2.0,40^BCN,40,N,N,N^FR^FD>:"
           l5$(19%) = "24^FO125,0800^CI0^A0N,25,25^FR^FD"
           l5$(20%) = "24^FO350,0800^BY2,2.0,40^BCN,40,N,N,N^FR^FD>:"
           l5$(21%) = "25^FO125,0975^CI0^A0N,25,25^FR^FD"
           l5$(22%) = "25^FO350,0975^BY2,2.0,40^BCN,40,N,N,N^FR^FD>:"
           l5$(23%) = "^PQ1"
           l5$(24%) = "^XZ"


           return

print_label    

        nbr_lines% = 0%

    read_loop
        init(" ") a$
        b$ = all(hex(00))
        nbr_lines% = nbr_lines% + 1%
        a$ = xx$(nbr_lines%)
        if a$ = " " then end_process
        a_len% = len(a$)                   /* Calc Length of Data String */
        str(b$,1%,a_len%) = str(a$,1%,a_len%) /* Put into b$ Data from a$ */
        convert str(a$,1%,2%) to ln%, data goto skip_data
                                           /* Look for a Field Number    */ 
        l_len% = len(lbl$(ln%))            /* Find Length of Data Element*/
                                           /* in the Label data array    */
        b_len% = (a_len% - 2%) + l_len%    /* Adjust for 2 digit field No*/

        b$ = all(hex(00))                  /* Initialize Print string    */
                                           /* 1st set Font data for print*/
                                           /* 2nd append Actual Data that*/
                                           /*     will be printed        */    
        str(b$,1%,b_len%) = str(a$,3%,a_len%-2%)                           ~
            & str(lbl$(ln%),1%,l_len%)
      skip_data
                                           /* (AWD002)                  */
        if nbr_lines% = 1% and been_here% = 1% then                        ~
                               b$ = hex(7e) & str(a$,2%,a_len%)

        if nbr_lines% = 1% and been_here% > 1% then                        ~
                               goto read_loop

  
        gosub print_line
                                           /* (AWD002)                 */         
        if a$ = "^XZ" then end_process       /* Last Line */

                                           /* (AWD002)                 */ 
        goto read_loop

    end_process
	been_here% = been_here% + 1%
        close #5
	scr_nbr% = 1%
	convert scr_nbr$ to scr_nbr%, data goto bad_nbr
bad_nbr:
        for l% = 1% to scr_nbr%
            call "LINK" addr(script$, lb1%, lb2%)
        next l%
        call "FILEBGON" (#5)        
        gosub open_file
        return

        print_line
                                                  /* (RHHTEST)          */
            write #5, using L55030, b$, eod goto L55030
L55030: FMT CH(256)
        return

        open_file
            library$        = "APCDATA "
            volume$         = "CARLOS"
            file$           = "MFGSCX"
            script$         = "MFGSCX"
	    axd$            = "   "
            call "OPENFILE" (#5, "IO   ", f2%(5%), rslt$(5%), axd$ )
            if f2%(5%) <> 0% then goto L01100
               gosub file_exists         
               if comp% <> 16% then goto exit_program
                  call "FILEBGON" (#5)

L01100:    open nodisplay #5, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return

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
           hdr$ = "***** (Error) (Error) (Error)  *****"
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
            call "SHOSTAT" ("One Moment Please")
            lb1% = 0% : lb2% = 0%

            close #5
REM         call "LINK" addr(script$, lb1%, lb2%)

            call "FILEBGON" (#5)        
        end
