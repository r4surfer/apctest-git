        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCSHAPE                             *~
            *  Creation Date     - 01/24/96                             *~
            *  Last Modified Date- 01/24/96                             *~
            *  Description       - This Program provides computed data  *~
            *                      for Special Shapes setup.            *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/24/96 ! New Program for (APC) - Last Mod Date    ! JBF *~
            *          !                                          !     *~
            * 11/12/97 ! Revision Update For 60403                ! DJD *~
            *************************************************************
        dim                                                              ~
            shape_code$2,                /* Shape Code                 */~
            shape_desc$29,               /* Shape Description          */~
            height$3,                    /* Height Entry from Part #   */~
            width$4,                     /* Width  Entry from Part #   */~
            leg$4,                       /* Leg    Entry from Part #   */~
            ht$8,                        /* Height Display (Decimal)   */~
            wd$8,                        /* Width  Display (Decimal)   */~
            lg$8,                        /* Leg    Display (Decimal)   */~
            cut_length$8,                /* Cut Length Display         */~
            setting$8,                   /* Setting Display            */~
            angle1$8,                    /* Angle 1 Display            */~
            angle2$8,                    /* Angle 2 Display            */~
            type$1,                      /* Input Required Type        */~
            readkey$25,                  /* Gencodes File Read Key     */~
            desc$32,                     /* Gencodes Description       */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$60,                 /* Error message              */~
            inpmessage$79,               /* Informational Message      */~
            cursor%(2%),                 /*                            */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            progid$18,                   /* Screen Line #2 Program ID  */~
            line11$79,                   /* Screen Line #11            */~
            i$(24)80,                    /* Screen Detail Area         */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(1%),                     /* = 0 if the file is open    */~
            f1%(1%),                     /* = 1 if READ was successful */~
            fs%(1%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(1%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/12/97 Special Shapes Computations    "
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
            * #01 ! GENCODES ! System Code Table File                   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =   24

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" ( #1, fs%(1%), f2%( 1%),   0%, rslt$(1%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)

            date$ = date
            call "DATEFMT" (date$)

            select degrees
            progid$     = "APCSHAPE: " & str(cms2v$,,8)
            line11$     = "Cut Length      Setting       Angle 1      "& ~
                           "Angle 2                             "
            edtmessage$ = "To Modify Displayed Values, Position Cursor"& ~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 4%
L10090:         gosub'051(fieldnr%)        /* Default / Enables */
                     if enabled% = 0% then L10250

L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                     if keyhit% =  1% then gosub startover
                     if keyhit% <> 4% then L10220 /* Previous Field */
L10150:                   fieldnr% = max(1%, fieldnr% - 1%)

                          gosub'051(fieldnr%)
                               if enabled% = 1% then L10120
                               if fieldnr% = 1% then L10090
                               goto L10150

L10220:              if keyhit% = 16% and  fieldnr% = 1% then exit_program
                     if keyhit% <> 0% then L10120

L10250:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " " then L10120

                if fieldnr% = 2% and type$ =  "1" then fieldnr% = 4%
                if fieldnr% = 3% and type$ <> "6" then fieldnr% = 4%

            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                if keyhit% =  1% then gosub startover
                if keyhit% = 14% then gosub compute_screen
                if keyhit% = 16% then exit_program
                if keyhit% <> 0% then editpg1

L11130:     fieldnr%    = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% > 4% then editpg1
            if fieldnr% = lastfieldnr%        then editpg1

            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled%  =  0%            then editpg1

L11200:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit%   =  1%            then gosub startover
                if keyhit%   <> 0%            then L11200

            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " "           then L11200
                lastfieldnr% = fieldnr%
            goto L11130

        REM *************************************************************~
            *     P R O C E S S    D A T A                              *~
            *-----------------------------------------------------------*~
            * Performs Computations from Input Data for Display.        *~
            *************************************************************
        compute_screen
            if shape_code$ = "00" then compute_00
            if shape_code$ = "01" then compute_01
            if shape_code$ = "02" then compute_02
            if shape_code$ = "03" then compute_03
            if shape_code$ = "04" then compute_04
            if shape_code$ = "05" then compute_05
            if shape_code$ = "06" then compute_06
            if shape_code$ = "07" then compute_07
            if shape_code$ = "08" then compute_08
            if shape_code$ = "09" then compute_09

        compute_00                                 /* Full Round       */
            cut_length = pi * (wd / 2) + 10
            setting    = wd / 2
            angle1     = 0 : angle2 = 0
            goto convert_display

        compute_01                                 /* Quarter Round    */
            cut_length = (((pi * ht) + 10) / 2)
            setting    = 0 : angle1 = 0 : angle2 = 0
            goto convert_display

        compute_02                                 /* Half Round       */
            cut_length = (pi * ht) + 10
            setting    = (wd / 2)
            angle1     = 0 : angle2 = 0
            goto convert_display

        compute_03                                 /* Triangle         */
            cut_length = sqr(wd^2 + ht^2)
            setting    = 0
            angle1     = (arctan(wd / ht)) / 2
            angle2     = (arctan(ht / wd)) / 2
            goto convert_display

        compute_04                                 /* Trapezoid        */
            delta_x    = ht - lg
            cut_length = sqr(delta_x^2 + wd^2)
            setting    = 0
            angle1     = (arctan(wd / delta_x)) / 2
            angle2     = ((arctan(delta_x / wd)) + 90) / 2
            goto convert_display

        compute_05                                 /* Ellipse          */
            cut_length = pi * (sqr(2 * (ht^2 + (wd / 2)^2)))
            setting    = ((wd / 2)^2 + ht^2) / (2 * ht)
            angle1     = 0 : angle2 = 0
            goto convert_display

        compute_06                                 /* Hexagon          */
            cut_length = (ht / 2) / .866
            setting    = 0 : angle1 = 60 : angle2 = 0
            goto convert_display

        compute_07                                 /* Colonial Arch    */
            cut_length = pi * (wd / 2) + 10
            setting    = wd / 2
            angle1     = ht - (wd / 2)
            angle2     = 0
            goto convert_display

        compute_08                                 /* Octagon          */
            cut_length = ((ht / 2) / 1.207) + .125
            setting    = 0 : angle1 = 67.5 : angle2 = 0
            goto convert_display

        compute_09                                 /* Eyebrow          */
            delta_x    = ht - lg
            cut_length = pi * (sqr(2 * (delta_x^2 + (wd / 2)^2)))
            setting    = ((wd / 2)^2 + delta_x^2) / (2 * delta_x)
            angle1     = 0 : angle2 = 0

        convert_display
            calc       = cut_length
            gosub convert_sixteen

            cut_length = a% + round((b% / 16), 4)
            convert cut_length to cut_length$,   pic(###.####)

            calc       = setting
            gosub convert_sixteen

            setting    = a% + round((b% / 16), 4)
            if setting = 0 then setting$ = " N/A" else                   ~
                convert setting to setting$,     pic(###.####)

            angle1     = round(angle1, 4)
            if angle1  = 0 then angle1$  = " N/A" else                   ~
                convert angle1  to angle1$,      pic(###.####)

            angle2     = round(angle2, 4)
            if angle2  = 0 then angle2$  = " N/A" else                   ~
                convert angle2  to angle2$,      pic(###.####)

        return

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

*        Define the Input Message for the Screen/Field Indicated
L28110:     if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
                read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
            "Enter a Valid Special Shape Code ?                        ",~
            "Enter a Valid Height Dimension ?                          ",~
            "Enter a Valid Width Dimension ?                           ",~
            "Enter a Valid Leg Dimension ?                             "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, shape_code$, shape_desc$,  ~
                height$, width$, leg$, cut_length$, setting$, angle1$,   ~
                angle2$, type$, ht$, wd$, lg$

            lfac$(1%) = hex(82)
            lfac$(2%) = hex(84)
            lfac$(3%) = hex(84)
            lfac$(4%) = hex(84)
        return

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

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT

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

            if fieldnr% > 0% then accept_screen                          ~
                             else init(hex(86)) lfac$()

        accept_screen
            accept                                                       ~
                at (01,02),                                              ~
                   "APC Special Shapes Computations"            ,        ~
                                                                         ~
                at (01,66), "Today:"                            ,        ~
                at (01,73), fac(hex(8c)),   date$               , ch(08),~
                                                                         ~
                at (02,02), fac(hex(94)),   errormsg$           , ch(60),~
                at (02,63), fac(hex(8c)),   progid$             , ch(18),~
                                                                         ~
                at (05,02), "Shape Code:"                       ,        ~
                at (05,16), fac(lfac$(1%)), shape_code$         , ch(02),~
                at (05,25), fac(hex(84)),   shape_desc$         , ch(29),~
                                                                         ~
                at (06,02), "    Height:"                       ,        ~
                at (06,15), fac(lfac$(2%)), height$             , ch(03),~
                at (06,25), fac(hex(84)),   ht$                 , ch(08),~
                                                                         ~
                at (07,02), "     Width:"                       ,        ~
                at (07,14), fac(lfac$(3%)), width$              , ch(04),~
                at (07,25), fac(hex(84)),   wd$                 , ch(08),~
                                                                         ~
                at (08,02), "       Leg:"                       ,        ~
                at (08,14), fac(lfac$(4%)), leg$                , ch(04),~
                at (08,25), fac(hex(84)),   lg$                 , ch(08),~
                                                                         ~
                at (11,02), fac(hex(ac)),   line11$             , ch(60),~
                                                                         ~
                at (13,03), fac(hex(84)),   cut_length$         , ch(08),~
                at (13,18), fac(hex(84)),   setting$            , ch(08),~
                at (13,31), fac(hex(84)),   angle1$             , ch(08),~
                at (13,44), fac(hex(84)),   angle2$             , ch(08),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1)              , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2)              , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3)              , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                if keyhit% <> 15% then L40570
                     call "PRNTSCRN"

                     goto accept_screen
L40570:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

            return

        set_pf1
        if edit% = 2% then L40760     /*  Input Mode             */
            pf$(1) = "(1)Start Over                            " &       ~
                     "                                      "
            pf$(2) = "                                         " &       ~
                     "                      (15)Print Screen"
            pf$(3) = "                                         " &       ~
                     "                      (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)

            if fieldnr% = 1% then L40740
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40740:     return

L40760: if fieldnr% > 0% then L40880  /*  Edit Mode - Select Field */
            pf$(1) = "(1)Start Over                            " &       ~
                     "                      (14)Compute Data"
            pf$(2) = "                                         " &       ~
                     "                      (15)Print Screen"
            pf$(3) = "                                         " &       ~
                     "                      (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)

            return

                                     /*  Edit Mode - Enabled    */
L40880:     pf$(1) = "(1)Start Over                            " &       ~
                     "                                      "
            pf$(2) = "                                         " &       ~
                     "                      (15)Print Screen"
            pf$(3) = "                                         " &       ~
                     "                                      "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0fff00)

            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************
        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50140,     /* Shape Code                 */~
                              L50500,     /* Height                     */~
                              L50720,     /* Width                      */~
                              L50940      /* Leg                        */
        return

        REM Shape Code                          SHAPE_CODE$
L50140:     convert shape_code$ to shape_code,  data goto L50450

            convert shape_code  to shape_code$, pic(00)

            gosub lookup_shape
                if errormsg$ <> " " then return

            lfac$(1%) = hex(84)
            if type$  = "1"         then type_1
            if type$  = "2"         then type_2
            if type$  = "3"         then type_3
            if type$  = "6"         then type_6

        type_1
            lfac$(2%) = hex(82)
        return

        type_2
            lfac$(3%) = hex(82)
        return

        type_3
            lfac$(2%) = hex(82)
            lfac$(3%) = hex(82)
        return

        type_6
            lfac$(2%) = hex(82)
            lfac$(3%) = hex(82)
            lfac$(4%) = hex(82)
        return
L50450:     errormsg$ = "(Error) - Invalid Shape Code (Required)?"
            init(" ") shape_code$
        return

        REM Height                              HEIGHT$
L50500:     if type$ = "2"  then L50690
            convert height$ to height,  data goto L50670

            if height < 0   then L50670
            convert height  to height$, pic(###)

            ht1$ = str(height$,1%,2%)
            ht2$ = str(height$,3%,1%)
            convert ht1$    to ht1,     data goto L50670

            convert ht2$    to ht2,     data goto L50670

            ht2 = round((ht2 / 8), 4)
            ht  = ht1 + ht2
            convert ht      to ht$,     pic(00#.####)

        return
L50670:     errormsg$ = "(Error) - Invalid Height (Required)?"
            init(" ") height$
L50690: return

        REM Width                               WIDTH$
L50720:     if type$ = "1" then L50910
            convert width$ to width,    data goto L50890

            if width < 0   then L50890
            convert width  to width$,   pic(####)

            wd1$ = str(width$,1%,3%)
            wd2$ = str(width$,4%,1%)
            convert wd1$   to wd1,      data goto L50890

            convert wd2$   to wd2,      data goto L50890

            wd2 = round((wd2 / 8), 4)
            wd  = wd1 + wd2
            convert wd     to wd$,      pic(00#.####)

        return
L50890:     errormsg$ = "(Error) - Invalid Width (Required)?"
            init(" ") width$
L50910: return

        REM Leg                                 LEG$
L50940:     if type$ <> "6" then L51130
            convert leg$    to leg,     data goto L51110

            if leg < 0      then L51110
            convert leg     to leg$,    pic(####)

            lg1$ = str(leg$,1%,3%)
            lg2$ = str(leg$,4%,1%)
            convert lg1$    to lg1,     data goto L51110

            convert lg2$    to lg2,     data goto L51110

            lg2 = round((lg2 / 8), 4)
            lg  = lg1 + lg2
            convert lg      to lg$,     pic(00#.####)

        return
L51110:     errormsg$ = "(Error) - Invalid Leg (Required)?"
            init(" ") leg$
L51130: return

        REM *************************************************************~
            *           S p e c i a l   S u b r o u t i n e s           *~
            *************************************************************
        lookup_shape
            readkey$ = all(hex(00))
            str(readkey$,1%,9%)   = "APC SHAPE"
            str(readkey$,10%,15%) = shape_code$
            read #1,key = readkey$, using L60080, desc$, eod goto L60130
L60080:         FMT POS(25), CH(30)

            shape_desc$ = str(desc$,1%,29%)
            type$       = str(desc$,30%,1%)
        return
L60130:     errormsg$ = "(Error) - Invalid Shape Code Lookup?"
            init(" ") shape_code$
        return

        convert_sixteen
            calc   = round(calc, 4)
            a%     = int(calc)
            b%     = int((calc - a%) * 10000)
            if  b% = 0%             then L60270         /****************/
                d% = int(b%/625)                       /* Conversion of*/
                if mod(b%,625) <> 0 then d% = d% + 1%  /* Decimals to  */
                     b% = d%                           /*  Sixteen's   */
                     if b% <> 16%   then L60270         /****************/
                          a% = a% + 1% : b% = 0%    /* A% = WHOLE PART */
L60270: return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*
        exit_program
            call "SHOSTAT" ("One Moment Please")

        end
