        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN62                             *~
            *  Creation Date     - 03/10/05                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M Gregory                   *~
            *                                                           *~
            *  Description       - Entry & modification of balance      *~
            *                      location calculations.               *~
            *                                                           *~
            *  Code Tables Used  - MODEL                                *~
            *                                                           *~
            *  Subroutine Used   - AWDPLA62 - Report                    *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/10/05 ! (New) Program                            ! CMG *~
            *************************************************************

        dim                                                              ~
            readkey$50,                  /* GENCODES Lookup & Descr    */~
            sav_key$27,                  /* Use for Loading Table      */~
            f_sav_key$5,                  /* Use for Loading Table      */~
            t_sav_key$5,                  /* Use for Loading Table      */~
            s_sav_key$5,                  /* Use for Loading Table      */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            hdr2$10,                     /* Column Header              */~
            copytxt$9,                   /* Screen Text for Table Copy */~
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

        dim model$3,                     /* Model  Code                */~
            f_model$3,                   /* Model  Code                */~
            t_model$3,                   /* Model  Code                */~
            s_model$3,                   /* Model  Code                */~
            mdl_desc$30,                 /* Model  Desc                */~
            rec$256,                     /* Top or Bottom              */~
            t_b$1,                       /* Top or Bottom              */~
            f_t_b$1,                     /* Top or Bottom              */~
            s_t_b$1,                     /* Top or Bottom              */~
            unit_type$1,                 /* Standard, Cottage, Oriel   */~
            f_unit_type$1,                 /* Standard, Cottage, Oriel   */~
            s_unit_type$1,                 /* Standard, Cottage, Oriel   */~
            value1$9,                    /* Value 1                    */~ 
            value2$9,                    /* Value 2                    */~
            value3$9,                    /* Value 3                    */~
            value4$9,                    /* Value 4                    */~
            value$9,                     /* Variable to test values    */~
            fixed$1,                     /* Is there a static value    */~
            fixed_value$9,               /* Fixed Value                */~
            cott_oriel$9                 /* Cottage/Oriel Multiplier   */



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
            apc$   = "(AWD) Balance Location Data Entry"
            pname$ = "AWDPLN62 - Rev: R7.00"

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
            * #1  ! AWDPLNBC ! Balance Location File                    *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "AWDPLNBC",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,   keylen =  5 

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24
	

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),500%, rslt$(1%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
 
            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
REM   gosub dataput
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            actvflds% = 10%          /* No. of Active Fields on-screen  */

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

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  8% then goto  delete_record
                  if keyhit%  = 16% then goto  dataput
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
REM            if fieldnr% > 4% then fieldnr% = fieldnr% - 1%
            if fieldnr% <= 3% then fieldnr% = 0%
            if fieldnr% < 1% or fieldnr% > actvflds% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%,2%)      /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%,2%)      /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%, edit%)
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
         "Enter Model Code  (MODEL)                                    ",~
         "Enter 'T'op or 'B'ottom                                      ",~
         "Enter Unit Type 'S'tandard, 'C'ottage, 'O'riel, 'A'LL        ",~
         "Enter Height Limit                                           ",~
         "Enter Factor Under Limit                                     ",~
         "Enter Factor Over Limit                                      ",~
         "Is there a static value? 'N'one, 'U'pper, 'L'ower            ",~
         "Static Value                                                 ",~
         "Enter Multiplier for Cottage and Oriel                       ",~
         "Enter Balance Cover Factor                                   "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, model$, mdl_desc$,         ~
                      unit_type$, value$, value1$, value2$, value3$,     ~
                      t_b$, fixed$, fixed_value$, cott_oriel$, value4$

            value1, value2, value3, fixed_value, cott_oriel, value4  = 0.00


            onfile%, del% = 0%

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
        dataload
            onfile% = 0%
            init(" ") sav_key$
            sav_key$ = str(model$) & str(t_b$) & str(unit_type$)

            read #1, hold, key = sav_key$, using L30100, value1, value2, ~
                                         value3, fixed$, fixed_value,   ~ 
                                         cott_oriel, value4,eod goto L30998
L30100:     fmt pos(6), 3*PD(14,4), CH(1), PD(14,4), PD(14,4)


            convert value1 to value1$, pic(-00.0000)

            convert value2 to value2$, pic(-00.0000)

            convert value3 to value3$, pic(-00.0000)

            convert fixed_value to fixed_value$, pic(-00.0000)

            convert cott_oriel to cott_oriel$, pic(-00.0000)

            convert value4 to value4$, pic(-00.0000)

            onfile% = 1%



L30998:     return



        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            onfile% = 0%
            init(" ") sav_key$
            sav_key$ = str(model$) & str(t_b$) & str(unit_type$)

            read #1, hold, key = sav_key$,  eod goto L31998

                     onfile% = 1%
                     if del% = 1% then return

L31998:


            put #1, using L35050, model$, t_b$, unit_type$, value1, ~
                 value2, value3, fixed$, fixed_value, cott_oriel,   ~
                 value4

            if onfile% = 1% then rewrite #1, data goto write_err          ~
                else write #1, data goto write_err, eod goto write_err
            goto inputmode

            write_err
                errormsg$ = "Error writing to AWDPLNBC. Data NOT saved."
                gosub error_prompt
                goto INPUTMODE


        delete_record
            del% = 1%
            gosub dataput
REM         gosub confirm_delete

            delete #1

            del% = 0%
            goto INPUTMODE


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35050:     FMT                         /* File: AWDPLNBC              */~
                CH(03),                 /* Model No.                   */~
                CH(01),                 /* Top or Bottom               */~
                CH(01),                 /* Unit Type                   */~
                PD(14,4),               /* First Value                 */~
                PD(14,4),               /* Second Value                */~
                PD(14,4),               /* Third Value                 */~
                CH(1),                  /* Fixed Value ?               */~
                PD(14,4),               /* Actual Fixed Value          */~
                PD(14,4),               /* Cottage / Oriel             */~
                PD(14,4)                /* Forth Value                 */

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
              on fieldnr% gosub L40160,          /* Model              */~
                                L40160,          /* Top or Bottom      */~
                                L40160,          /* Unit Type          */~
                                L40170,          /* first value        */~
                                L40170,          /* second value       */~
                                L40170,          /* third value        */~ 
                                L40160,          /* Fixed ?            */~
                                L40170,          /* Fixed Value        */~
                                L40170,          /* Cottage / Oriel    */~
                                L40170           /* Fourth Value       */


              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40170:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Model       :",                              ~
               at (03,30), fac(lfac$(1%)), model$               , ch(03),~
               at (03,40), fac(hex(84)),   mdl_desc$            , ch(30),~
                                                                         ~
               at (04,02), "Top or Bottom       :",                      ~
               at (04,30), fac(lfac$(2%)), t_b$                 , ch(01),~
                                                                         ~
               at (05,02), "Unit Type S, C, O, A:",                      ~
               at (05,30), fac(lfac$(3%)), unit_type$           , ch(01),~
                                                                         ~
               at (06,02), "Value 1   Height Lim:",                      ~
               at (06,30), fac(lfac$(4%)), value1$              , ch(09),~
                                                                         ~
               at (07,02), "Value 2   Factor Und:",                      ~
               at (07,30), fac(lfac$(5%)), value2$              , ch(09),~
                                                                         ~
               at (08,02), "Value 3   Facto Over:",                      ~
               at (08,30), fac(lfac$(6%)), value3$              , ch(09),~
                                                                         ~
               at (09,02), "Static Value N,U,L  :",                      ~
               at (09,30), fac(lfac$(7%)), fixed$               , ch(01),~
                                                                         ~
               at (10,02), "Value 4   Fixed Valu:",                      ~
               at (10,30), fac(lfac$(8%)), fixed_value$         , ch(09),~
                                                                         ~
               at (11,02), "Value 4   Cott/Oriel:",                      ~
               at (11,30), fac(lfac$(9%)), cott_oriel$          , ch(09),~
                                                                         ~
               at (12,02), "Value 4   Bal Cover :",                      ~
               at (12,30), fac(lfac$(10%)), value4$             , ch(09),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9% then goto L40410
                  call "AWDPLA62" (#1,#4)
                  goto L40190
L40410:
               if keyhit% <> 13% then goto L40415
		   goto inputmode2
L40415:
               if keyhit% <> 15 then goto L40420
                  call "PRNTSCRN"
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        hdr$ = "GENCODES Xref"
        hdr2$ = "Tube Diam."
        copytxt$ = " "
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                                       "
            pf$(2) = "                 (9) Print Report       " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (13)Copy/Delete Range  " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffff09ffffff0d0e0f1000)

            if fieldnr% = 1% then L40570
                str(pf$(2),12,25) = " "  :  str(pfkeys$, 9,1) = hex(ff)

                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "           (8)Delete Record             " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffff08ffffffffffff0f1000)
            if onfile% = 1% then L40690
                str(pf$(2),12,16) = " "  :  str(pfkeys$, 8,1) = hex(ff)
L40690:     return
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
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%,edit%)
            errormsg$ = " "
            on fieldnr% gosub L50100,        /* Model  Code            */~
                              L50200,        /* Top or Bottom          */~
                              L50250,        /* Unit Type              */~
                              L50300,        /* First Value            */~
                              L50340,        /* Second Value           */~
                              L50380,        /* Thrid Value            */~
                              L50400,        /* Static Value?          */~
                              L50500,        /* Fixed Value            */~
                              L50600,        /* Cottage / Oriel        */~
                              L50700         /* Fourth Value           */


            return

L50100: rem Enter Model  Code                             model$, mdl_desc$
            readkey$ = "MODEL    " & model$
            gosub test_model 
            if errormsg$ <> " " then return

        return

    test_model 
        call "DESCRIBE" (#4, readkey$, mdl_desc$, 0%, f1%(4))
        if f1%(4) = 0% then errormsg$ = "Invalid Model "
        return

L50200: rem Enter Top or Bottom                           t_b$
            if t_b$ = " " then t_b$ = "B"
            if t_b$ <> "T" and t_b$ <> "B" then goto top_bot_err

        return
   top_bot_err
        errormsg$ = "Invalid Value for Top or Bottom"
        return

L50250: rem Enter Unit Type S, C, O                       unit_type$
            if unit_type$ = " " then unit_type$ = "S"
            if unit_type$ <> "S" and unit_type$ <> "C"  and    ~
               unit_type$ <> "O" and unit_type$ <> "A"         ~
                        then goto unit_type_err

            gosub dataload
            if onfile% = 1% then edit% = 1%
            if onfile% = 1% then fieldnr% = actvflds%

        return
   unit_type_err
        errormsg$ = "Invalid Value for Unit Type"
        return

L50300: rem Enter Value 1                                 value1$
        init(" ") value$ 
        value$ = value1$
        gosub check_value
           if val% <> 1% then goto bad_val1
        value1 = value
        value1$ = value$

        return

    bad_val1
        errormsg$ = "Invalid data for First Value"
        return

L50340: rem Enter Value 2                                 value2$
        init(" ") value$ 
        value$ = value2$
        gosub check_value
           if val% <> 1% then goto bad_val2
        value2 = value
        value2$ = value$

        return

    bad_val2
        errormsg$ = "Invalid data for Second Value"
        return

L50380: rem Enter Value 3                                 value3$   
        init(" ") value$ 
        value$ = value3$
        gosub check_value
           if val% <> 1% then goto bad_val3
        value3 = value
        value3$ = value$

        return

    bad_val3
        errormsg$ = "Invalid data for Third Value"
        return

L50400: rem Enter Fixed Value?  N, U, L                 fixed$
            if fixed$ = " " then fixed$ = "N"
            if fixed$ <> "N" and fixed$ <> "U"  and    ~
               fixed$ <> "L" then goto static_err
        return
   static_err
        errormsg$ = "Invalid Value for Fixed/Static Value"
        return


L50500: rem Enter Value 4  Static Value                   fixed_value$
        init(" ") value$
        value$ = fixed_value$
        gosub check_value
           if val% <> 1% then goto bad_val4
        fixed_value = value       
        fixed_value$ = value$

        return

    bad_val4
        errormsg$ = "Invalid data for Fourth Value"
        return


L50600: rem Enter Value 1                                 cott_oriel$
        init(" ") value$ 
        value$ = cott_oriel$
        gosub check_value
           if val% <> 1% then goto bad_val5
        cott_oriel = value
        cott_oriel$ = value$

        return

    bad_val5
        errormsg$ = "Invalid data for Fifth Value"
        return

L50700: rem Enter Value 4                                 value4$
        init(" ") value$ 
        value$ = value4$
        gosub check_value
           if val% <> 1% then goto bad_val6
        value4 = value
        value4$ = value$

        return

    bad_val6
        errormsg$ = "Invalid data for Balance Cover Value"
        return

      check_value
        val%  = 0%
        value = 0.00
        convert value$ to value, data goto bad_value
REM     if value < 0 or value > 999.99 then goto bad_value
        convert value to value$, pic(-000.0000)

        val% = 1%
      bad_value
      return


REM +-------------------------------------------------------------------+
REM | Copy / Delete range                                               |
REM +-------------------------------------------------------------------+

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode2
            gosub initialize_variables2

            for fieldnr% = 1% to 7%
L60110:         gosub'051(fieldnr%,2%)        /* Default / Enables */
                      if enabled% = 0% then L60230
L60130:         gosub'102(fieldnr%, 1%)       /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover2
                      if keyhit%  =  6% then goto  delete_range 
                      if keyhit% <>  4% then       L60215
L60160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%,1%)
                         if enabled% = 1% then L60130
                         if fieldnr% = 1% then L60110
                         goto L60160
L60215:               if keyhit% = 16% and fieldnr% = 1% then inputmode   
                      if keyhit% <> 0% then       L60130
L60230:         gosub'152(fieldnr%,1%)  /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L60130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg2
            gosub'102(fieldnr%, 3%)           /*  Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover2
                  if keyhit%  =  6% then goto  delete_range 
                  if keyhit%  =  7% then goto  copy_range   
                  if keyhit%  = 16% then goto  dataput
                  if keyhit% <>  0% then       editpg2
L61120:     fieldnr% = cursor%(1%) - 7%
            col% = (cursor%(2%) - 30%) / 15%                                
	    if col% = 1%  then fieldnr% = 4%
	    if col% > 1%  then fieldnr% = fieldnr% + 4%
            if fieldnr% < 1% or fieldnr% > 5% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'051(fieldnr%, 2%)      /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L61170:     gosub'102(fieldnr%, 3%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover2
                  if keyhit% <>  0% then L61170
            gosub'152(fieldnr%,2%)      /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L61170
                  lastfieldnr% = fieldnr%
            goto L61120

delete_range:            
            func$ = "D"
            err% = 0%
            gosub check_keys
REM	    if err% > 0% then goto range_error
	    gosub calc_count
            if err% = 0% then gosub process_it
            goto startover2
        return

copy_range:
            func$ = "C"
            err% = 0%
            gosub check_keys
	    if err% > 0% then goto range_error
            gosub check_to
	    if err% > 0% then goto range_error
	    gosub calc_count
            if err% = 0% then gosub process_it
            goto startover2

        return

check_keys
            init(" ") sav_key$, t_sav_key$
            sav_key$ = str(f_model$) & str(f_t_b$) & str(f_unit_type$)
            f_sav_key$ = str(f_model$) & str(f_t_b$) & str(f_unit_type$) 
            t_sav_key$ = str(t_model$) & str(f_t_b$) & str(f_unit_type$) 
	    num_dgt% = 1  
	    s_num_dgt% = 1  
	    if str(f_model$,1,1) < "0" or str(f_model$,1,1) > "9" or  ~
	       str(t_model$,1,1) < "0" or str(t_model$,1,1) > "9" or  ~
	       str(s_model$,1,1) < "0" or str(s_model$,1,1) > "9" then  ~
	        num_dgt% = 2  
	    if str(s_model$,1,1) < "0" or str(s_model$,1,1) > "9" then  ~
	        s_num_dgt% = 2  
	    if str(f_model$,2,1) < "0" or str(f_model$,2,1) > "9" or  ~
	       str(t_model$,2,1) < "0" or str(t_model$,2,1) > "9" or  ~
	       str(s_model$,2,1) < "0" or str(s_model$,2,1) > "9" then  ~
	        num_dgt% = 3  
	    if str(s_model$,2,1) < "0" or str(s_model$,2,1) > "9" then  ~
	        s_num_dgt% = 3  
	    if str(f_model$,3,1) < "0" or str(f_model$,3,1) > "9" or  ~
	       str(t_model$,3,1) < "0" or str(t_model$,3,1) > "9" or  ~
	       str(s_model$,3,1) < "0" or str(s_model$,3,1) > "9" then  ~
	        num_dgt% = 0  
	    if str(s_model$,3,1) < "0" or str(s_model$,3,1) > "9" then  ~
	        s_num_dgt% = 0  
	    if num_dgt% = 0% then err% = 4%
	    return
        
check_to 
            init(" ") s_sav_key$
            s_sav_key$ = str(s_model$) & str(s_t_b$) & str(s_unit_type$) 
	    err% = 1%
	    if str(s_sav_key$) = " " then no_start
	    if f_sav_key$ < s_sav_key$ and                 ~
	       t_sav_key$ > s_sav_key$ then no_start
	    err% = 0%
no_start:
        return

calc_count
        err% = 0%
        cnt% = 0%
	if f_t_b$ = "*" then str(sav_key$,4,1) = "B"
	if f_t_b$ = "*" then str(t_sav_key$,4,1) = "T"
	if f_unit_type$ = "*" then str(sav_key$,5,1) = "A"
	if f_unit_type$ = "*" then str(t_sav_key$,5,1) = "S"
        read #1, key >= sav_key$, using F00001, sav_key$, eod goto fini   
	goto first_read
readnext: read #1, key > sav_key$, using F00001, sav_key$, eod goto fini   
first_read:
	if f_t_b$ = "B" and str(sav_key$,4,1) = "T" then goto readnext
	if f_t_b$ = "T" and str(sav_key$,4,1) = "B" then goto readnext
	if f_unit_type$ = "S" and str(sav_key$,5,1) <> "S" then goto readnext
	if f_unit_type$ = "C" and str(sav_key$,5,1) <> "C" then goto readnext
	if f_unit_type$ = "O" and str(sav_key$,5,1) <> "O" then goto readnext
	if f_unit_type$ = "A" and str(sav_key$,5,1) <> "A" then goto readnext
        if sav_key$ > t_sav_key$ then fini 
        cnt% = cnt% + 1%
	goto readnext

fini:  
        errormsg$ = "##### Records to Copy  "
        if func$ = "D" then errormsg$ = "##### Records to Delete"
	convert cnt% to str(errormsg$,1,5), pic (####0)
        call "SHOSTAT" (errormsg$)                         
        call "PAUSE" (100)
        if func$ = "D" then return                                 

	err% = 6%
	convert str(s_model$,s_num_dgt%) to tmp%, data goto range_error 
	if s_t_b$ <> "*" AND F_T_B$<> "*" then cnt% = cnt% * 2%
	tmp% = tmp% + cnt% - 1%
	if s_num_dgt% = 1% and tmp% > 999% then goto range_error
	if s_num_dgt% = 2% and tmp% >  99% then goto range_error
	if s_num_dgt% = 3% and tmp% >   9% then goto range_error
	err% = 0%
        return

range_error
        errormsg$ = "Copy/Delete Range Error..............."
        if err% = 2% then errormsg$ = "Invalid To Key........................"
        if err% = 3% then errormsg$ = "Duplicate Key on Copy Encountered....."
        if err% = 4% then errormsg$ = "Key Must End With Numerics............"
        if err% = 5% then errormsg$ = "Model Incrament Error................."
        if err% = 6% then errormsg$ = "'To' Key Model Overflow..............."
	errormsg$ = errormsg$ & sav_key$ & " : " & s_sav_key$
        gosub error_prompt
	return

process_it
        sav_key$ = str(f_model$) & str(f_t_b$) 
	if f_t_b$ = "*" then str(sav_key$,4,1) = "B"
	if f_t_b$ = "*" then str(t_sav_key$,4,1) = "T"
	if f_unit_type$ = "*" then str(sav_key$,5,1) = "A"
	if f_unit_type$ = "*" then str(t_sav_key$,5,1) = "S"
        read #1, hold, key >= sav_key$, using F00001, sav_key$, eod goto e_o_f  
	goto firstread
readnxt: read #1, hold, key > sav_key$, using F00001, sav_key$, eod goto e_o_f  
firstread:
F00001: FMT CH(5)
F00002: FMT CH(256)
        if sav_key$ > t_sav_key$ then e_o_f
	if f_t_b$ = "B" and str(sav_key$,4,1) = "T" then goto readnxt
	if f_t_b$ = "T" and str(sav_key$,4,1) = "B" then goto readnxt
	if f_unit_type$ = "S" and str(sav_key$,5,1) <> "S" then goto readnxt
	if f_unit_type$ = "C" and str(sav_key$,5,1) <> "C" then goto readnxt
	if f_unit_type$ = "O" and str(sav_key$,5,1) <> "O" then goto readnxt
	if f_unit_type$ = "A" and str(sav_key$,5,1) <> "A" then goto readnxt
        if func$ = "D" then delete_it
	get #1, using F00002, rec$
	if str(sav_key$,1,3) <> f_model$ then gosub inc_model
	f_model$ = str(sav_key$,1,3)
	str(s_sav_key$,4,1) = str(sav_key$,4,1)       
	str(s_sav_key$,5,1) = str(sav_key$,5,1)       
	if s_t_b$ <> "*" then str(s_sav_key$,4,1) = s_t_b$                  
	if s_unit_type$ <> "*" then str(s_sav_key$,5,1) = s_unit_type$      
        str(rec$,1,4)  = str(s_sav_key$,1,4)
        str(rec$,1,5)  = str(s_sav_key$,1,5)
        write #1, using F00002, rec$, eod goto bad_write
        goto readnxt

bad_write
        err% = 3%
        gosub range_error
        goto readnxt

inc_model:
        err% = 5%
        convert str(s_model$,num_dgt%,4% - num_dgt%) to model%, ~
                                                  data goto range_error
	model% = model% + 1%  
	if num_dgt% = 1% and model% > 999% then goto range_error
	if num_dgt% = 2% and model% > 99%  then goto range_error
	if num_dgt% = 3% and model% > 9%   then goto range_error
        if num_dgt% = 1% then convert model% to str(s_model$,1,3), pic (000)  
        if num_dgt% = 2% then convert model% to str(s_model$,2,2), pic (00)    
        if num_dgt% = 3% then convert model% to str(s_model$,3,1), pic (0)    
        s_sav_key$ = str(s_model$) & str(s_t_b$) 
        return

delete_it:
        delete #1
        goto readnxt
e_o_f:
	 return

REM         *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************


        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'052(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L78110
                inpmessage$ = edtmessage$
                return

L78110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 2% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter Model Number                                           ",~
         "Enter 'T'op, 'B'ottom or '*' All                             ",~
         "Enter S/C/O/A or '*' All                                     ",~
         "Enter Model Number                                           ",~
         "Enter Model Number                                           ",~
         "Enter 'T'op, 'B'ottom to Override or '*' to Keep Value       ",~
         "Enter S/C/O/A to Override or '*' to Keep Value               "  

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables2
            init(" ") errormsg$, inpmessage$, f_model$, t_model$,        ~
                      s_model$, f_t_b$, s_t_b$, f_unit_type$,            ~
		      s_unit_type$

        return

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************

        startover2
            return clear all
            goto inputmode2

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'052(2%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              if fieldnr% > 0% then lfac$(fieldnr%) = hex(81) /* Upper Only */

L90190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (05,30), "*-- COPY / DELETE ---*",   ~
               at (06,30), "S T A R T      E N D          T O     ",     ~
               at (07,30), "---------      --------       --------",     ~
                                                                         ~
               at (08,02), "Model               :",                      ~
               at (08,30), fac(lfac$(1%)), f_model$             , ch(03),~
               at (08,45), fac(lfac$(4%)), t_model$             , ch(03),~
               at (08,60), fac(lfac$(5%)), s_model$             , ch(03),~
                                                                         ~
               at (09,02), "Top or Bottom       :",                      ~
               at (09,30), fac(lfac$(2%)), f_t_b$               , ch(01),~
               at (09,60), fac(lfac$(6%)), s_t_b$               , ch(01),~
                                                                         ~
               at (10,02), "Unit Type S, C, O, A:",                      ~
               at (10,30), fac(lfac$(3%)), f_unit_type$         , ch(01),~
               at (10,60), fac(lfac$(7%)), s_unit_type$         , ch(01),~
                                                                         ~
               at (12,30), "T = Just Top, B = Just Bottom, * = All",     ~
               at (13,30), "S/C/O/A or  * = All                   ",     ~
               at (14,30), "For To, '*' = Keep Value, Else Override",    ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L90420
                  call "PRNTSCRN"
                  goto L90190

L90420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

set_pf2
        hdr$ = "GENCODES Xref"
        hdr2$ = "Tube Diam."
        copytxt$ = " "
   	if fieldnr% > 4% and edit% = 1% then edit% = 2%

        if edit% >= 2% then L90610     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffff09ffffffff0e0f1000)

L90590:     return

L90610: if edit% = 2% then L90705  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "           (6) Delete Range             " &        ~
                     "                      (15) Print Screen"
            pf$(3) = "           (7) Copy Range               " &        ~
                     "                      (16) Return      "
            pfkeys$ = hex(01ff0607ffffffffffffffffffff0f1000)
L90690:     return
L90705:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "           (6) Delete Range             " &        ~
                     "                      (15) Print Screen"
            pf$(3) = "                                        " &        ~
                     "                      (16) Return      "
            pfkeys$ = hex(01ff06ffffffffffffffffffffff0f1000)
            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'152(fieldnr%,edit%)
            errormsg$ = " "
            on fieldnr% gosub L90100,        /* Model                  */~
                              L90200,        /* Top or Bottom          */~
                              L90300,        /* Unit of Measure        */~
                              L90400,        /* Model                  */~
                              L90500,        /* Model                  */~
                              L90600,        /* Top or Bottom          */~
                              L90700         /* Unit of Measure        */


            return

L90100: rem Enter Vendor Code                vendor$, ven_desc$
            model$ = f_model$
            t_model$ = f_model$
            s_model$ = f_model$
            readkey$ = "MODEL    " & model$
            if errormsg$ <> " " then return

        return

L90200: rem Enter Top or Bottom                           t_b$
            t_b$ = f_t_b$
            s_t_b$ = f_t_b$
            if t_b$ = " " then t_b$ = "*"
            if t_b$ <> "T" and t_b$ <> "B" and t_b$ <> "*" then top_bot_err

        return

L90300: rem Enter Unit Type S, C, O                       unit_type$
            s_unit_type$ = f_unit_type$
            if f_unit_type$ = " " then f_unit_type$ = "*"
            if f_unit_type$ <> "S" and f_unit_type$ <> "C"  and    ~
               f_unit_type$ <> "O" and f_unit_type$ <> "A"  and    ~
               f_unit_type$ <> "*"                                 ~
                        then goto unit_type_err
            return

L90400: rem Enter Vendor Code                vendor$, ven_desc$
            model$ = t_model$
            s_model$ = t_model$
            readkey$ = "MODEL    " & model$
            if errormsg$ <> " " then return

        return

L90500: rem Enter Vendor Code                vendor$, ven_desc$
            model$ = s_model$
            readkey$ = "MODEL    " & model$
            if errormsg$ <> " " then return

        return

L90600: rem Enter Top or Bottom                           t_b$
            t_b$ = s_t_b$
            if t_b$ = " " then t_b$ = "*"
            if t_b$ <> "T" and t_b$ <> "B" and t_b$ <> "*" then top_bot_err

        return

L90700: rem Enter Unit Type S, C, O                       unit_type$
            if s_unit_type$ = " " then s_unit_type$ = "*"
            if s_unit_type$ <> "S" and s_unit_type$ <> "C"  and    ~
               s_unit_type$ <> "O" and s_unit_type$ <> "A"  and    ~
               s_unit_type$ <> "*"                                 ~
                        then goto unit_type_err
            return

REM +-------------------------------------------------------------------+



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
            
