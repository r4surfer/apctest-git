        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN59                             *~
            *  Creation Date     - 11/11/04                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M Gregory                   *~
            *                                                           *~
            *  Description       - Entry & modification of tube winding *~
            *                      and coil data. Winds & coils are     *~
            *                      are entered by Vendor, Balance Type, *~
            *                      and up to four different combinations*~
            *                      of specifications                    *~
            *                                                           *~
            *  Code Tables Used  - BAL VENDOR                           *~
            *                      BAL TYPES                            *~
            *                                                           *~
            *  Subroutine Used   - EWDPLA59 - Data File Listing Rpts    *~
            *                      EWDPLB59 - 'Rapid Entry' Routine     *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/11/04 ! (New) Program                            ! CMG *~
            * 06/24/14 !AWD001 Add Purchase to Order              ! PWW *~
            *************************************************************

        dim                                                              ~
            readkey$50,                  /* GENCODES Lookup & Descr    */~
            sav_key$27,                  /* Use for Loading Table      */~
            f_sav_key$27,                /* Use for Loading Table      */~
            t_sav_key$27,                /* Use for Loading Table      */~
            s_sav_key$27,                /* Use for Loading Table      */~
            rec$156,                     /* Use for Loading Table      */~
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

        dim vendor$1,                    /* Vendor Code                */~
            f_vendor$1,                  /* Vendor Code                */~
            t_vendor$1,                  /* Vendor Code                */~
            s_vendor$1,                  /* Vendor Code                */~
            ven_desc$30,                 /* Vendor Desc                */~
            bal_type$1,                  /* Balance Type               */~
            f_bal_type$1,                /* Balance Type               */~
            t_bal_type$1,                /* Balance Type               */~
            s_bal_type$1,                /* Balance Type               */~
            bal_type_desc$30,            /* Balance Type Desc          */~
            t_b$1,                       /* Top or Bottom              */~
            f_t_b$1,                     /* Top or Bottom              */~
            t_t_b$1,                     /* Top or Bottom              */~
            s_t_b$1,                     /* Top or Bottom              */~
            bal$8,                       /* Balance                    */~
            f_bal$8,                     /* Balance                    */~
            t_bal$8,                     /* Balance                    */~
            s_bal$8,                     /* Balance                    */~
            bal1$8,                      /* Balance  1                 */~
            f_bal1$8,                    /* Balance  1                 */~
            t_bal1$8,                    /* Balance  1                 */~
            s_bal1$8,                    /* Balance  1                 */~
            bal2$8,                      /* Balance  2                 */~
            f_bal2$8,                    /* Balance  1                 */~
            t_bal2$8,                    /* Balance  1                 */~
            s_bal2$8,                    /* Balance  1                 */~
            value1$8,                    /* Value 1                    */~ 
            value2$8,                    /* Value 2                    */~
            value3$8,                    /* Value 3                    */~
            value4$8,                    /* Value 4                    */~
            value$,                      /* Variable to test values    */~
            td$2,                        /* Tube Diameter              */~
/*<AWD001>*/purch_order$1                /* Purchase to Order          */


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
            apc$   = "(AWD) Tube Windings/Coil Data Entry"
            pname$ = "AWDPLN59 - Rev: R7.00"

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
            * #1  ! AWDPLNWC ! Production Windings & Coils File         *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "AWDPLNWC",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,   keylen = 27,                       ~
                        alt key  1, keypos =    1, keylen =  62,         ~
                            key  2, keypos =   28, keylen =  35

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
/*          actvflds% = 11%            No. of Active Fields on-screen  */
/*<AWD001>*/actvflds% = 12%         /* No. of Active Fields on-screen  */

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
         "Enter Vendor Code  (BAL VENOR)                               ",~
         "Enter Balance Type  (BAL TYPES)                              ",~
         "Enter 'T'op or 'B'ottom                                      ",~
         "Enter Balance Code                                           ",~
         "Enter Balance Code  1                                        ",~
         "Enter Balance Code  2                                        ",~
         "Enter First Max Value(1/8 inch) for Winds/Coil Data          ",~
         "Enter Second Max Value (1/8 inch) for Winds/Coil Data        ",~
         "Enter Third Max Value (1/8 inch) for Winds/Coil Data         ",~
         "Enter Fourth Max Value (1/8 inch) for Winds/Coil Data        ",~  
         "Enter Tube Diameter                                          ",~
         "Enter Purchase to Order (Y/N)                                "
/*<AWD001>^  */
        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, vendor$, ven_desc$,        ~
                      bal_type$, bal_type_desc$, bal$, value$, value1$,  ~
                      value2$, value3$, value4$, td$, bal1$, bal2$, t_b$,~
/*<AWD001>*/          purch_order$
            value1, value2, value3, value4  = 0.00


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
            sav_key$ = str(vendor$) & str(bal_type$) & str(t_b$) &        ~
                                       str(bal$) & str(bal1$) & str(bal2$)

            read #1, hold, key = sav_key$, using L30100, value1, value2, ~
                                         value3, value4, td$,            ~
/*<AWD001>*/                             purch_order$, eod goto L30998
/*<AWD001>  L30100:     fmt pos(31), 4*PD(14,4), CH(02), 6*PD(14,4)   */
L30100:     fmt pos(31), 4*PD(14,4), CH(02), CH(01)   /*<AWD001>*/


            convert value1 to value1$, pic(000.0000)

            convert value2 to value2$, pic(000.0000)

            convert value3 to value3$, pic(000.0000)

            convert value4 to value4$, pic(000.0000)

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
            sav_key$ = str(vendor$) & str(bal_type$) & str(t_b$) &        ~
                                       str(bal$) & str(bal1$) & str(bal2$)

            read #1, hold, key = sav_key$,  eod goto L31998

                     onfile% = 1%
                     if del% = 1% then return

L31998:


            put #1, using L35050, vendor$, bal_type$, t_b$, bal$, bal1$, ~
                bal2$, vendor$, bal_type$, t_b$, value1, value2, value3, ~
                 value4, td$, purch_order$                /*<AWD001>*/   

            if onfile% = 1% then rewrite #1, data goto write_err          ~
                else write #1, data goto write_err, eod goto write_err
            goto inputmode

            write_err
                errormsg$ = "Error writing to AWDPLNWC. Data NOT saved."
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

L35050:     FMT                         /* File: AWDPLNWC              */~
                CH(01),                 /* Model No.                   */~
                CH(01),                 /* Lkup Diameter Code          */~
                CH(01),                 /* Top or Bottom               */~
                CH(08),                 /* Balance                     */~
                CH(08),                 /* Balance 1                   */~
                CH(08),                 /* Balance 2                   */~
                CH(01),                 /* Vendor                      */~
                CH(01),                 /* Balance Type                */~
                CH(01),                 /* Top or Bottom               */~
                PD(14,4),               /* First Value                 */~
                PD(14,4),               /* Second Value                */~
                PD(14,4),               /* Third Value                 */~
                PD(14,4),               /* Fourth Value                */~
                CH(02),                 /* Tube Diameter               */~
/*<AWD001>*/    CH(01)                  /* Purchase to Order           */

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
              on fieldnr% gosub L40160,          /* Vendor             */~
                                L40160,          /* Balance Type       */~
                                L40160,          /* Top or Bottom      */~
                                L40160,          /* Balance            */~
                                L40160,          /* Balance   1        */~
                                L40160,          /* Balance   2        */~
                                L40170,          /* first value        */~
                                L40170,          /* second value       */~
                                L40170,          /* third value        */~
                                L40170,          /* fourth value       */~
                                L40160,          /* Tube Diameter      */~
                                L40160           /* Purch to Order     */

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
               at (03,02), "Vendor      :",                              ~
               at (03,30), fac(lfac$(1%)), vendor$              , ch(01),~
               at (03,40), fac(hex(84)),   ven_desc$            , ch(30),~
                                                                         ~
               at (04,02), "Balance Type        :",                      ~
               at (04,30), fac(lfac$(2%)), bal_type$            , ch(01),~
               at (04,40), fac(hex(84)),   bal_type_desc$       , ch(30),~
                                                                         ~
               at (05,02), "Top or Bottom       :",                      ~
               at (05,30), fac(lfac$(3%)), t_b$                 , ch(01),~
                                                                         ~
               at (06,02), "Balance/Turns       :",                      ~
               at (06,30), fac(lfac$(4%)), bal$                 , ch(08),~
                                                                         ~
               at (07,02), "Balance  Heights    :",                      ~
               at (07,30), fac(lfac$(5%)), bal1$                , ch(08),~
                                                                         ~
               at (08,02), "Balance   N/A       :",                      ~
               at (08,30), fac(lfac$(6%)), bal2$                , ch(08),~
                                                                         ~
               at (09,02), "Value 1   Sash Wgt L:",                      ~
               at (09,30), fac(lfac$(7%)), value1$              , ch(08),~
                                                                         ~
               at (10,02), "Value 2   Sash Wgt U:",                      ~
               at (10,30), fac(lfac$(8%)), value2$              , ch(08),~
                                                                         ~
               at (11,02), "Value 3  Glass Hgt L:",                      ~
               at (11,30), fac(lfac$(9%)), value3$              , ch(08),~
                                                                         ~
               at (12,02), "Value 4  Glass Hgt U:",                      ~
               at (12,30), fac(lfac$(10%)), value4$              , ch(08),~
                                                                         ~
               at (13,02), "Tube Diameter       :",                      ~
               at (13,30), fac(lfac$(11%)), td$                 , ch(02),~
                                                                         ~
/*<AWD001>*/   at (14,02), "Purchase to Order   :",                      ~
/*<AWD001>*/   at (14,30), fac(lfac$(12%)), purch_order$        , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9% then goto L40410
                  call "AWDPLA59" (#1,#4)
                  goto L40190
L40410:
               if keyhit% <> 13 then goto L40415
               goto inputmode2
REM               goto L40190
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
            pf$(3) = "                 (13) Copy/Delete Range " &        ~
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
            on fieldnr% gosub L50100,        /* Vendor Code            */~
                              L50200,        /* Balance Type           */~
                              L50250,        /* Top or Bottom          */~
                              L50300,        /* Balance                */~
                              L50340,        /* Balance 1              */~
                              L50380,        /* Balance 2              */~
                              L50400,        /* First Value            */~
                              L50500,        /* Second Value           */~
                              L50600,        /* Third Value            */~
                              L50700,        /* Fourth Value           */~
                              L50800,        /* Tube Diameter          */~
/*<AWD001>*/                  L50900,        /* Purchase to Order      */~


            return

L50100: rem Enter Vendor Code                             vendor$, ven_desc$
            readkey$ = "BAL VENOR" & vendor$
            gosub test_vendor
            if errormsg$ <> " " then return

        return

    test_vendor
        call "DESCRIBE" (#4, readkey$, ven_desc$, 0%, f1%(4))
        if f1%(4) = 0% then errormsg$ = "Invalid Vendor"
        return

L50200: rem Enter Balance Type                            bal_type$, bal_type_desc$

            readkey$ = "BAL TYPES" & bal_type$
            gosub test_lookup
            if errormsg$ <> " " then return

        return

    test_lookup
        call "DESCRIBE" (#4, readkey$, bal_type_desc$, 0%, f1%(4))
            if f1%(4) = 0% then errormsg$="Invalid Balance Type        "
        return

L50250: rem Enter Top or Bottom                           t_b$
            if t_b$ = " " then t_b$ = "B"
            if t_b$ <> "T" and t_b$ <> "B" then goto top_bot_err

        return
   top_bot_err
        errormsg$ = "Invalid Value for Top or Bottom"
        return
        

L50300: rem Enter Balance                                 bal$      
            if str(bal$,1%,8) = " " then bal_err

        return

    bal_err
        errormsg$ = "Invalid Value for Balance"
        return

L50340: rem Enter Balance 1                               bal1$
        

        return

L50380: rem Enter Balance 2                              bal2$

            gosub dataload
            if onfile% = 1% then edit% = 1%
            if onfile% = 1% then fieldnr% = actvflds%

        return

L50400: rem Enter Value 1                                 value1$
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

L50500: rem Enter Value 2                                 value2$
        init(" ") value$ 
        value$ = value2$
        gosub check_value
           if val% <> 1% then goto bad_val2
        value2 = value
        if value2 < value1 then goto bad_val2
        value2$ = value$

        return

    bad_val2
        errormsg$ = "Invalid data for Second Value"
        return

L50600: rem Enter Value 3                                 value3$   
        if value3$ <> " " then goto L50655
           str(value3$,1%,8%) = "000.0000"
           return
 
L50655: if str(value3$,1%,8%) = "000.0000" then return
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

L50700: rem Enter Value 4                                 value4$   
        if value4$ <> " " then goto L50755
           str(value4$,1%,8%) = "000.0000"
           return
 
L50755: if str(value4$,1%,8%) = "000.0000" then return
        init(" ") value$ 
        value$ = value4$
        gosub check_value
           if val% <> 1% then goto bad_val4
        value4 = value
        if value4 < value3 then goto bad_val4
        value4$ = value$

        return

    bad_val4
        errormsg$ = "Invalid data for Fourth Value"
        return

L50800: rem Enter Tube Diamter                            td$               
            readkey$ = "PLAN DIAM" & td$     
            gosub test_td_diam
            if errormsg$ <> " " then return

        return
        
/*<AWD001>+ */
L50900: rem Enter Purchase to Order               purch_order$             
        if purch_order$ <> "Y" then purch_order$ = "N"
        return
/*<AWD001>- */

    test_td_diam
        call "DESCRIBE" (#4, readkey$, " ",       0%, f1%(4))
        if f1%(4) = 0% then errormsg$ = "Invalid Tube Diameter"
        return


      check_value
        val%  = 0%
        value = 0.00
        convert value$ to value, data goto bad_value
        if value < 0 or value > 999.99 then goto bad_value
        convert value to value$, pic(000.0000)

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

            for fieldnr% = 1% to  12%
L60110:         gosub'051(fieldnr%,2%)        /* Default / Enables */
                      if enabled% = 0% then L60230
L60130:         gosub'102(fieldnr%, 1%)       /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover2
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
L61120:     fieldnr% = cursor%(1%) - 5%
            col% = (cursor%(2%) - 30%) / 15%                                
	    if col% = 1%  then fieldnr% = fieldnr% + 3%
	    if col% >= 2% then fieldnr% = fieldnr% + 9%
            if fieldnr% < 1% or fieldnr% > 12% then editpg2
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
	    if err% = 1% then goto range_error
	    gosub calc_count
            if err% = 0% then gosub process_it
            goto startover2
        return

copy_range:
            func$ = "C"
            err% = 0%
            gosub check_keys
	    if err% = 1% then goto range_error
            gosub check_to
	    if err% = 2% then goto range_error
	    gosub calc_count
            if err% = 0% then gosub process_it
            goto startover2

        return

check_keys
            init(" ") sav_key$, t_sav_key$
            sav_key$ = str(f_vendor$) & str(f_bal_type$) & str(f_t_b$) &  ~
                       str(f_bal$) & str(f_bal1$) & str(f_bal2$)           

            t_sav_key$ = str(t_vendor$) & str(t_bal_type$) & str(t_t_b$) &  ~
                       str(t_bal$) & str(t_bal1$) & str(t_bal2$)           
 
            if t_sav_key$ < sav_key$ then err% = 1%
	    return
        
check_to 
            init(" ") s_sav_key$
            s_sav_key$ = str(s_vendor$) & str(s_bal_type$) & str(s_t_b$) &  ~
                       str(s_bal$) & str(s_bal1$) & str(s_bal2$)           
	    err% = 1%
	    if str(s_sav_key$) = " " then no_start
	    if s_sav_key$ >= str(sav_key$,1,3)  and                        ~
	       s_sav_key$ <= str(t_sav_key$,1,3) then no_start
	    err% = 0%
no_start:
        return

calc_count
        cnt% = 0%
        read #1, key >= sav_key$, using F00001, sav_key$, eod goto fini   
	goto first_read
readnext: read #1, key > sav_key$, using F00001, sav_key$, eod goto fini   
first_read:
        if sav_key$ > t_sav_key$ then fini 
        cnt% = cnt% + 1%
	goto readnext

fini:  
        errormsg$ = "##### Records to Copy  "
        if func$ = "D" then errormsg$ = "##### Records to Delete"
	convert cnt% to str(errormsg$,1,5), pic (####0)
        call "SHOSTAT" (errormsg$)                         
        call "PAUSE" (100)
        return

range_error
        errormsg$ = "Copy/Delete Range Error..............."
        if err% = 2% then errormsg$ = "Invalid To Key........................"
        if err% = 3% then errormsg$ = "Duplicate Key on Copy Encountered....."
	errormsg$ = errormsg$ & sav_key$
        gosub error_prompt
	return

process_it
        sav_key$ = str(f_vendor$) & str(f_bal_type$) & str(f_t_b$) &  ~
                       str(f_bal$) & str(f_bal1$) & str(f_bal2$)           
        read #1, hold, key >= sav_key$, using F00001, sav_key$, eod goto e_o_f  
	goto firstread
readnxt: read #1, hold, key > sav_key$, using F00001, sav_key$, eod goto e_o_f  
firstread:
F00001: FMT CH(27)
F00002: FMT CH(256)
        if sav_key$ > t_sav_key$ then e_o_f
        if func$ = "D" then delete_it
	get #1, using F00002, rec$
        str(rec$,1,3)  = str(s_sav_key$,1,3)
        str(rec$,28,3) = str(s_sav_key$,1,3)
        write #1, using F00002, rec$, eod goto bad_write
        goto readnxt

bad_write
        err% = 3%
        gosub range_error
        goto readnxt

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
         "Enter Vendor Code  (BAL VENOR)                               ",~
         "Enter Balance Type  (BAL TYPES)                              ",~
         "Enter 'T'op or 'B'ottom                                      ",~
         "Enter Balance Code                                           ",~
         "Enter Balance Code  1                                        ",~
         "Enter Balance Code  2                                        ",~
         "Enter Balance Code                                           ",~
         "Enter Balance Code  1                                        ",~
         "Enter Balance Code  2                                        ",~
         "Enter Vendor Code  (BAL VENOR)                               ",~
         "Enter Balance Type  (BAL TYPES)                              ",~
         "Enter 'T'op or 'B'ottom                                      "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables2
            init(" ") errormsg$, inpmessage$, f_vendor$, t_vendor$,      ~
		      s_vendor$, f_bal_type$, t_bal_type$,               ~
                      s_bal_type$, f_bal$, t_bal$, s_bal$,               ~
                      f_t_b$, t_t_b$, s_t_b$,                            ~
                      f_bal1$, t_bal1$, s_bal1$, f_bal2$, t_bal2$, s_bal2$

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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload2
            onfile% = 0%
            init(" ") sav_key$
            sav_key$ = str(vendor$) & str(bal_type$) & str(t_b$) &        ~
                                       str(bal$) & str(bal1$) & str(bal2$)

            read #1, hold, key = sav_key$, using L80100, value1, value2, ~
                                         value3, value4, td$, eod goto L30998
L80100:     fmt pos(31), 4*PD(14,4), CH(02), 6*PD(14,4)


            convert value1 to value1$, pic(000.0000)

            convert value2 to value2$, pic(000.0000)

            convert value3 to value3$, pic(000.0000)

            convert value4 to value4$, pic(000.0000)

            onfile% = 1%



L80998:     return



        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput2
            onfile% = 0%
            init(" ") sav_key$
            sav_key$ = str(vendor$) & str(bal_type$) & str(t_b$) &        ~
                                       str(bal$) & str(bal1$) & str(bal2$)

            read #1, hold, key = sav_key$,  eod goto L31998

                     onfile% = 1%
                     if del% = 1% then return

L81998:


            put #1, using L35050, vendor$, bal_type$, t_b$, bal$, bal1$, ~
                bal2$, vendor$, bal_type$, t_b$, value1, value2, value3, ~
                 value4, td$, purch_order$                /*<AWD001>*/   

            if onfile% = 1% then rewrite #1, data goto write_err2         ~
                else write #1, data goto write_err2, eod goto write_err2
            goto inputmode2

            write_err2
                errormsg$ = "Error writing to AWDPLNWC. Data NOT saved."
                gosub error_prompt
                goto INPUTMODE2


        delete_record2
            del% = 1%
            gosub dataput2
REM         gosub confirm_delete

            delete #1

            del% = 0%
            goto INPUTMODE2 


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
               at (03,30), "*-- COPY / DELETE ---*",   ~
               at (04,30), "S T A R T      E N D          T O     ",     ~
               at (05,30), "---------      --------       --------",     ~
                                                                         ~
               at (06,02), "Vendor              :",                      ~
               at (06,30), fac(lfac$(1%)), f_vendor$            , ch(01),~
               at (06,45), fac(hex(8c)), t_vendor$           , ch(01),~
               at (06,60), fac(lfac$(10%)), s_vendor$           , ch(01),~
                                                                         ~
               at (07,02), "Balance Type        :",                      ~
               at (07,30), fac(lfac$(2%)), f_bal_type$          , ch(01),~
               at (07,45), fac(hex(8c)), t_bal_type$          , ch(01),~
               at (07,60), fac(lfac$(11%)), s_bal_type$         , ch(01),~
                                                                         ~
               at (08,02), "Top or Bottom       :",                      ~
               at (08,30), fac(lfac$(3%)), f_t_b$               , ch(01),~
               at (08,45), fac(hex(8c)), t_t_b$               , ch(01),~
               at (08,60), fac(lfac$(12%)), s_t_b$              , ch(01),~
                                                                         ~
               at (09,02), "Balance/Turns       :",                      ~
               at (09,30), fac(lfac$(4%)), f_bal$               , ch(08),~
               at (09,45), fac(lfac$(7%)), t_bal$              , ch(08),~
                                                                         ~
               at (10,02), "Balance  Heights    :",                      ~
               at (10,30), fac(lfac$(5%)), f_bal1$              , ch(08),~
               at (10,45), fac(lfac$(8%)), t_bal1$             , ch(08),~
                                                                         ~
               at (11,02), "Balance   N/A       :",                      ~
               at (11,30), fac(lfac$(6%)), f_bal2$              , ch(08),~
               at (11,45), fac(lfac$(9%)), t_bal2$             , ch(08),~
                                                                         ~
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
REM	if fieldnr% > 9% then edit% = 3%

        if edit% >= 2% then L90610     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffff09ffffffff0e0f1000)

L90590:     return

L90610: if edit% = 2% then L90700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "           (6) Delete Range             " &        ~
                     "                      (15) Print Screen"
            pf$(3) = "           (7) Copy Range               " &        ~
                     "                      (16) Return      "
            pfkeys$ = hex(01ff0607ffffffffffffffffffff0f1000)
L90690:     return
L90700:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L90100,        /* Vendor Code            */~
                              L90200,        /* Balance Type           */~
                              L90250,        /* Top or Bottom          */~
                              L90300,        /* Balance                */~
                              L90340,        /* Balance 1              */~
                              L90380,        /* Balance 2              */~ 
                              L90305,        /* Balance                */~
                              L90345,        /* Balance 1              */~
                              L90385,        /* Balance 2              */~ 
                              L90105,        /* Vendor Code            */~
                              L90205,        /* Balance Type           */~
                              L90255         /* Top or Bottom          */


            return

L90100: rem Enter Vendor Code                vendor$, ven_desc$
            vendor$ = f_vendor$
            t_vendor$ = f_vendor$
            readkey$ = "BAL VENOR" & vendor$
            gosub test_vendor
            if errormsg$ <> " " then return

        return

L90200: rem Enter Balance Type               bal_type$, bal_type_desc$
            bal_type$ = f_bal_type$
            t_bal_type$ = f_bal_type$
            readkey$ = "BAL TYPES" & bal_type$
            gosub test_lookup
            if errormsg$ <> " " then return

        return

L90250: rem Enter Top or Bottom                           t_b$
            t_b$ = f_t_b$
            t_t_b$ = f_t_b$
            if t_b$ = " " then t_b$ = "B"
            if t_b$ <> "T" and t_b$ <> "B" then goto top_bot_err

        return

L90300: rem Enter Balance                                 bal$      
            bal$ = f_bal$
            if str(bal$,1%,8) = " " then bal_err

        return

L90340: rem Enter Balance 1                               bal1$
        

        return

L90380: rem Enter Balance 2                              bal2$

 REM        gosub dataload2
 REM        if onfile% = 1% then edit% = 1%
 REM        if onfile% = 1% then fieldnr% = 18%          

        return

L90105: rem Enter Vendor Code                vendor$, ven_desc$
            vendor$ = s_vendor$
            readkey$ = "BAL VENOR" & vendor$
            gosub test_vendor
            if errormsg$ <> " " then return

        return

L90205: rem Enter Balance Type               bal_type$, bal_type_desc$
            bal_type$ = s_bal_type$
            readkey$ = "BAL TYPES" & bal_type$
            gosub test_lookup
            if errormsg$ <> " " then return

        return

L90255: rem Enter Top or Bottom                           t_b$
            t_b$ = s_t_b$
            if t_b$ = " " then t_b$ = "B"
            if t_b$ <> "T" and t_b$ <> "B" then goto top_bot_err

        return

L90305: rem Enter Balance                                 bal$      
            bal$ = t_bal$
            if str(bal$,1%,8) = " " then bal_err

        return

L90345: rem Enter Balance 1                               bal1$
        

        return

L90385: rem Enter Balance 2                              bal2$

 REM        gosub dataload2
 REM        if onfile% = 1% then edit% = 1%
 REM        if onfile% = 1% then fieldnr% = 18%          

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
            
