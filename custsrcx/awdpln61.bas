        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN61                             *~
            *  Creation Date     - 11/11/04                             *~
            *  Last Modified Date- 07/08/2009                           *~
            *  Written By        - Christie M Gregory                   *~
            *                                                           *~
            *  Description       - Entry & modification of tube winding *~
            *                      and coil data. Winds & coils are     *~
            *                      are entered by Vendor, Balance Type, *~
            *                      and up to four different combinations*~
            *                      of specifications                    *~
            *                                                           *~
            *  Code Tables Used  - MODEL                                *~
            *                      BAL TYPES                            *~
            *                      BAL VENOR                            *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/11/04 ! (New) Program                            ! CMG *~
            *07/08/2009!(AWD001) mod to all Perimeter Plus        ! CMG *~
            *          !  Weight value to screen                  !     *~
            * 06/24/14 !AWD002 Add Secondary Vendor & Balance Type! PWW *~
            * 07/14/14 !AWD003 Add Max Weight along w/AWD002      !     *~
            * 05/11/15 !IM7691 mod for litelift balances          ! PWW *~
            *************************************************************

        dim                                                              ~
            readkey$50,                  /* GENCODES Lookup & Descr    */~
            sav_key$4,                   /* Use for Loading Table      */~
            t_sav_key$4,                 /* Use for Loading Table      */~
            f_sav_key$4,                 /* Use for Loading Table      */~
            s_sav_key$4,                 /* Use for Loading Table      */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            hdr2$10,                     /* Column Header              */~
            copytxt$9,                   /* Screen Text for Table Copy */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(22%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim model$3,                     /* Model  Code                */~
            f_model$3,                   /* Model  Code                */~
            t_model$3,                   /* Model  Code                */~
            s_model$3,                   /* Model  Code                */~
            f_t_b$1,                     /* Top / Bottom               */~
            t_t_b$1,                     /* Top / Bottom               */~
            s_t_b$1,                     /* Top / Bottom               */~
            mod_desc$30,                 /* Model Description          */~
            top_bot$1,                   /* Top / Bottom               */~
            vendor$1,                    /* Vendor Code                */~
            ven_desc$30,                 /* Vendor Description         */~
            bal_type$1,                  /* Balance Type               */~
            bal_type_desc$30,            /* Balance Type Description   */~
            friction$8,                  /* Variable to test values    */~
            weight$8,                    /* Weight of Vinyl            */~
            perimeter$8,                 /*(AWD001) Perimeter Plus     */~
/*<AWD002>+ */                                                           ~
            vendor2$1,                    /* Vendor Code               */~
            ven_desc2$30,                 /* Vendor Description        */~
            bal_type2$1,                  /* Balance Type              */~
            bal_type_desc2$30,            /* Balance Type Description  */~
            max_weight$8,                 /* Max Weight                */~
/*<AWD002>- */                                                           ~
/*<IM7691>+ */                                                           ~
            range_max1$8,                 /* Range #1 Max              */~
            range_max2$8,                 /* Range #2 Max              */~
            range_max3$8,                 /* Range #3 Max              */~
            range_max4$8,                 /* Range #4 Max              */~
            range_max1_wt$8,              /* Range #1 Max Weight       */~
            range_max2_wt$8,              /* Range #2 Max Weight       */~
            range_max3_wt$8,              /* Range #3 Max Weight       */~
            range_max4_wt$8               /* Range #4 Max Weight       */
/*<IM7691>- */
            
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
            pname$ = "AWDPLN61 - Rev: R7.00"

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
            * #1  ! AWDPLNBL ! Production Balance Information           *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "AWDPLNBL",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,   keylen = 4 

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
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
/*<AWD002>  actvflds% = 7%             No. of Active Fields on-screen  */
/*<AWD002>*/actvflds% = 10%         /* No. of Active Fields on-screen  */
/*<IM7691>*/actvflds% = 18%         /* No. of Active Fields on-screen  */

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
            row% = cursor%(1%) : col% = cursor%(2%)
L11120:     fieldnr% = cursor%(1%) - 2%
REM         if fieldnr% > 4% then fieldnr% = fieldnr% - 1%
            if fieldnr% <= 10% then goto edit_cont
            on (row% - 13%) goto Range_1, Range_2, Range_3, Range_4
            goto editpg1
            
        Range_1
            if col% >= 8%  and col% <= 15% then fieldnr% = 11%
            if col% >= 23%  and col% <= 30% then fieldnr% = 12%
            goto edit_cont
        Range_2
            if col% >= 8%  and col% <= 15% then fieldnr% = 13%
            if col% >= 23%  and col% <= 30% then fieldnr% = 14%
            goto edit_cont
        Range_3
            if col% >= 8%  and col% <= 15% then fieldnr% = 15%
            if col% >= 23%  and col% <= 30% then fieldnr% = 16%
            goto edit_cont
        Range_4
            if col% >= 8%  and col% <= 15% then fieldnr% = 17%
            if col% >= 23%  and col% <= 30% then fieldnr% = 18%

        edit_cont
            if fieldnr% <= 2% then fieldnr% = 0%
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
         "Enter Model Number (MODEL)                                   ",~
         "Enter Top / Bottom (T or B)                                  ",~
         "Enter Vendor Code (BAL VENOR)                                ",~
         "Enter Balance Type (BAL TYPES)                               ",~
         "Enter Friction Factor                                        ",~
         "Enter Weight of Vinyl                                        ",~
         "Enter Perimeter Plus Weight Value                            ",~
/*<AWD002>+ */                                                           ~
         "Enter Secondary Vendor Code (BAL VENOR)                      ",~
         "Enter Secondary Balance Type (BAL TYPES)                     ",~
         "Enter Max Weight                                             ",~
/*<AWD002>- */                                                           ~
/*<IM7691>+ */                                                           ~
         "Enter #1 Max Window Height(in)                         ",~
         "Enter #1 Max Window Weight(lbs)                        ",~
         "Enter #2 Max Window Height(in)                         ",~
         "Enter #2 Max Window Weight(lbs)                        ",~
         "Enter #3 Max Window Height(in)                         ",~
         "Enter #3 Max Window Weight(lbs)                        ",~
         "Enter #4 Max Window Height(in)                         ",~
         "Enter #4 Max Window Weight(lbs)                        "
/*<IM7691>- */

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, model$, top_bot$, friction$, ~
                      vendor$, ven_desc$, bal_type$, bal_type_desc$,       ~
                      mod_desc$, weight$, perimeter$,                      ~
/*<AWD002>+ */        vendor2$, ven_desc2$, bal_type2$, bal_type_desc2$,   ~
                      max_weight$, range_max1$, range_max2$, range_max3$,  ~
                      range_max4$, range_max1_wt$, range_max2_wt$,         ~
                      range_max3_wt$, range_max4_wt$        /*IM7691 */

                      range_min1, range_max1, range_min2, range_max2,      ~
                      range_min3, range_max3, range_min4, range_max4,      ~
                      range_max1_wt, range_max2_wt,                        ~
                      range_max3_wt, range_max4_wt = 0.0    /*IM7691 */

            onfile%, del% = 0%
/*<AWD002>*/max_weight = 0

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
            model$, top_bot$, friction$, vendor$, ven_desc$, bal_type$,   ~
            bal_type_desc$, weight$, perimeter$ = " "
/*<AWD002>*/vendor2$, ven_desc2$, bal_type2$, bal_type_desc2$,            ~
            max_weight$ = " "
/*IM7691 + */
            range_max1$, range_max2$, range_max3$, range_max4$,           ~
            range_max1_wt$, range_max2_wt$,                               ~
            range_max3_wt$, range_max4_wt$ = " "
/*IM7691 - */

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
            sav_key$ = str(model$) & str(top_bot$) 

            read #1, hold, key = sav_key$, using L30100, vendor$, bal_type$, ~
                                friction, weight, perimeter,                 ~
/*<AWD002>*/                    vendor2$, bal_type2$, max_weight,            ~
/*IM7691 + */                   range_max1, range_max2, range_max3,          ~
                                range_max4, range_max1_wt, range_max2_wt,    ~
/*IM7691 - */                   range_max3_wt, range_max4_wt, eod goto L30998
L30100:     fmt pos(05), CH(01), CH(01), PD(14,4), PD(14,4), PD(14,4),       ~
/*<AWD002>*/             CH(01), CH(01), PD(14,4),                           ~
/*IM7691 + */            PD(14,4), PD(14,4), PD(14,4), PD(14,4),   ~
                         PD(14,4), PD(14,4), PD(14,4), PD(14,4)

             
            if range_max1 >= 0 then                        ~
            convert range_max1  to range_max1$,  pic(000.0000)~
            else                                         ~
            convert range_max1  to range_max1$,  pic(00.0000-)
             
            if range_max2 >= 0 then                        ~
            convert range_max2  to range_max2$,  pic(000.0000)~
            else                                         ~
            convert range_max2  to range_max2$,  pic(00.0000-)
             
            if range_max3 >= 0 then                        ~
            convert range_max3  to range_max3$,  pic(000.0000)~
            else                                         ~
            convert range_max3  to range_max3$,  pic(00.0000-)
             
            if range_max4 >= 0 then                        ~
            convert range_max4  to range_max4$,  pic(000.0000)~
            else                                         ~
            convert range_max4  to range_max4$,  pic(00.0000-)
             
            if range_max1_wt >= 0 then                        ~
            convert range_max1_wt  to range_max1_wt$,  pic(000.0000)~
            else                                         ~
            convert range_max1_wt  to range_max1_wt$,  pic(00.0000-)
             
            if range_max2_wt >= 0 then                        ~
            convert range_max2_wt  to range_max2_wt$,  pic(000.0000)~
            else                                         ~
            convert range_max2_wt  to range_max2_wt$,  pic(00.0000-)
             
            if range_max3_wt >= 0 then                        ~
            convert range_max3_wt  to range_max3_wt$,  pic(000.0000)~
            else                                         ~
            convert range_max3_wt  to range_max3_wt$,  pic(00.0000-)
             
            if range_max4_wt >= 0 then                        ~
            convert range_max4_wt  to range_max4_wt$,  pic(000.0000)~
            else                                         ~
            convert range_max4_wt  to range_max4_wt$,  pic(00.0000-)
             
/*IM7691 - */
            if friction >= 0 then                        ~
            convert friction  to friction$,  pic(000.0000)~
            else                                         ~
            convert friction  to friction$,  pic(00.0000-)

            if weight >= 0  then                         ~
            convert weight    to weight$,    pic(000.0000)~
            else                                         ~
            convert weight    to weight$,    pic(00.0000-)

/*(AWD001)*/
            if perimeter >=0 then                        ~
            convert perimeter to perimeter$, pic(000.0000)~
            else                                         ~
            convert perimeter to perimeter$, pic(00.0000-)

/*(AWD002)*/
            if max_weight > 99.99 or max_weight < -99.99 then max_weight = 0
            if bal_type2$ < "0" or bal_type2$ > "9" then bal_type2$ = " "
            if vendor2$ < "0" or vendor2$ > "9" then vendor2$ = " "
            if max_weight >=0 then                        ~
            convert max_weight to max_weight$, pic(000.0000)~
            else                                         ~
            convert max_weight to max_weight$, pic(00.0000-)

            onfile% = 1%
            readkey$ = "BAL VENOR" & vendor$
            gosub test_vendor
            readkey$ = "BAL TYPES" & bal_type$
            gosub test_types
/*<AWD002>+ */
            readkey$ = "BAL VENOR" & vendor2$
            gosub test_vendor2
            readkey$ = "BAL TYPES" & bal_type2$
            gosub test_types2
/*<AWD002>- */
L30998:     return


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            onfile% = 0%
            init(" ") sav_key$
            sav_key$ = str(model$) & str(top_bot$)           

            read #1, hold, key = sav_key$,  eod goto L31998

                     onfile% = 1%
                     if del% = 1% then return

L31998:
            put #1, using L35050, model$, top_bot$, vendor$, bal_type$,  ~
                friction, weight, perimeter,         /*(AWD001)*/        ~
                vendor2$, bal_type2$, max_weight,    /*(AWD002)*/        ~
/*IM7691 + */   range_max1, range_max2, range_max3, range_max4,          ~
                range_max1_wt, range_max2_wt, range_max3_wt,             ~
/*IM7691 - */   range_max4_wt
            if onfile% = 1% then rewrite #1, data goto write_err         ~
                else write #1, data goto write_err, eod goto write_err
            goto inputmode

            write_err
                errormsg$ = "Error writing to AWDPLNWC. Data NOT saved."
                gosub error_prompt
                goto INPUTMODE


        delete_record
            del% = 1%
            gosub dataput


            delete #1

            del% = 0%
            goto INPUTMODE

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35050:     FMT                         /* File: AWDPLNBL              */~
                CH(03),                 /* Model No.                   */~
                CH(01),                 /* Top / Bottom                */~
                CH(01),                 /* Vendor                      */~
                CH(01),                 /* Balance Type                */~
                PD(14,4),               /* Friction Factor             */~
                PD(14,4),               /* Weight   Factor             */~
                PD(14,4),               /*(AWD001) Perimeter Plus      */~
/*<AWD002>+*/   CH(01),                 /* Vendor2                     */~
                CH(01),                 /* Balance Type2               */~
/*<AWD002>-*/   PD(14,4),               /* Max Weight                  */~
/*IM7691 + */   PD(14,4),               /* Range #1 Max Window Height(in)  */~
                PD(14,4),               /* Range #1 Max Window Weight(lbs) */~
                PD(14,4),               /* Range #2 Max Window Height(in)  */~
                PD(14,4),               /* Range #2 Max Window Weight(lbs) */~
                PD(14,4),               /* Range #3 Max Window Height(in)  */~
                PD(14,4),               /* Range #3 Max Window Weight(lbs) */~
                PD(14,4),               /* Range #4 Max Window Height(in)  */~
/*IM7691 - */   PD(14,4)                /* Range #4 Max Window Weight(lbs) */

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
              on fieldnr% gosub L40160,       /* Model                 */~
                                L40160,       /* Top Bottom            */~
                                L40160,       /* Vendor                */~
                                L40160,       /* Balance Type          */~
                                L40170,       /* Friction Factor       */~
                                L40170,       /* Weight Factor         */~
                                L40170,       /*(AWD001) Perimeter     */~
                                L40160,       /* Vendor2 (AWD002)      */~
                                L40160,       /* Balance Type2(AWD002) */~
                                L40170,       /* Max Weight   (AWD002) */~
/*IM7691 + */                   L40170, /* Range #1 Max Window Height(in)  */~
                                L40170, /* Range #1 Max Window Weight(lbs) */~
                                L40170, /* Range #2 Max Window Height(in)  */~
                                L40170, /* Range #2 Max Window Weight(lbs) */~
                                L40170, /* Range #3 Max Window Height(in)  */~
                                L40170, /* Range #3 Max Window Weight(lbs) */~
                                L40170, /* Range #4 Max Window Height(in)  */~
/*IM7691 - */                   L40170  /* Range #4 Max Window Weight(lbs) */

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
               at (03,02), "Model               :",                      ~
               at (03,30), fac(lfac$(1%)), model$               , ch(03),~
               at (03,40), fac(hex(84)),   mod_desc$            , ch(30),~
                                                                         ~
               at (04,02), "Top / Bottom        :",                      ~
               at (04,30), fac(lfac$(2%)), top_bot$             , ch(01),~
                                                                         ~
               at (05,02), "Vendor              :",                      ~
               at (05,30), fac(lfac$(3%)), vendor$              , ch(01),~
               at (05,40), fac(hex(84)),   ven_desc$            , ch(30),~
                                                                         ~
               at (06,02), "Balance Type        :",                      ~
               at (06,30), fac(lfac$(4%)), bal_type$            , ch(01),~
               at (06,40), fac(hex(84)),   bal_type_desc$       , ch(30),~
                                                                         ~
               at (07,02), "Friction Factor     :",                      ~
               at (07,30), fac(lfac$(5%)), friction$            , ch(08),~
                                                                         ~
               at (08,02), "Vinyl Weight        :",                      ~
               at (08,30), fac(lfac$(6%)), weight$              , ch(08),~
/*(AWD001) */                                                            ~
               at (09,02), "Perimeter Plus      :",                      ~
               at (09,30), fac(lfac$(7%)), perimeter$           , ch(08),~
/*(AWD002)+*/                                                            ~
               at (10,02), "2nd Vendor          :",                      ~
               at (10,30), fac(lfac$(8%)), vendor2$             , ch(01),~
               at (10,40), fac(hex(84)),   ven_desc2$           , ch(30),~
                                                                         ~
               at (11,02), "2nd Balance Type    :",                      ~
               at (11,30), fac(lfac$(9%)), bal_type2$           , ch(01),~
               at (11,40), fac(hex(84)),   bal_type_desc2$      , ch(30),~
                                                                         ~
               at (12,02), "Max Weight          :",                      ~
               at (12,30), fac(lfac$(10%)), max_weight$          , ch(08),~
/*(AWD002)-*/                                                            ~
/*IM7691 + */  at (13,02), "     Window Height ",                        ~
               at (13,22), "Max Weight Limit of B&T",                ~
               at (14,04), "#1",                                          ~
               at (15,04), "#2",                                          ~
               at (16,04), "#3",                                          ~
               at (17,04), "#4",                                          ~
               at (14,08), fac(lfac$(11%)), range_max1$          , ch(08),~
               at (14,23), fac(lfac$(12%)), range_max1_wt$       , ch(08),~
               at (15,08), fac(lfac$(13%)), range_max2$          , ch(08),~
               at (15,23), fac(lfac$(14%)), range_max2_wt$       , ch(08),~
               at (16,08), fac(lfac$(15%)), range_max3$          , ch(08),~
               at (16,23), fac(lfac$(16%)), range_max3_wt$       , ch(08),~
               at (17,08), fac(lfac$(17%)), range_max4$          , ch(08),~
/*IM7691 - */  at (17,23), fac(lfac$(18%)), range_max4_wt$       , ch(08),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9 then goto L40410
                  call "AWDPLA61" (#1,#4)

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
                     "                       (9)Print Report "
            pf$(2) = "                 (13)Delete/Copy Range  " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffff09ffffff0dff0f1000)
            if fieldnr% = 1% then L40570
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
            on fieldnr% gosub L50010,        /* Model                  */~
                              L50020,        /* Top Bottom             */~
                              L50025,        /* Vendor                 */~
                              L50030,        /* Balance Type           */~
                              L50040,        /* Friction               */~
                              L50050,        /* Weight                 */~
                              L50060,        /*(AWD001) Perimeter Plus */~
                              L50070,        /*(AWD002) Vendor2        */~
                              L50080,        /*(AWD002) Balance Type2  */~
                              N50050,        /* Max Weight             */~ 
/*<IM7691> + */               N50070,        /* Range #1 Max Window Height */~
                              N50080,        /* Range #1 Max Window Weight */~
                              N50100,        /* Range #2 Max Window Height */~
                              N50110,        /* Range #2 Max Window Weight */~
                              N50130,        /* Range #3 Max Window Height */~
                              N50140,        /* Range #3 Max Window Weight */~
                              N50160,        /* Range #4 Max Window Height */~
/*<IM7691> - */               N50170         /* Range #4 Max Window Weight */

            return

L50010: rem Enter Model  Code                             model$, mod_desc$
            readkey$ = "MODEL    " & model$
            gosub test_model 
            if errormsg$ <> " " then return

        return

    test_model 
        call "DESCRIBE" (#4, readkey$, mod_desc$, 0%, f1%(4))
        if f1%(4) = 0% then errormsg$ = "Invalid Model "
        return

L50020: rem Enter Top / Bottom                            top_bot$                       
            if top_bot$ <> "T" and top_bot$ <> "B" then goto L50024
            gosub dataload

            if onfile% = 1% then fieldnr% = actvflds%
        return
L50024: errormsg$ = "Invalid Top / Bottom Code"
        return

L50025: rem Enter Balance                                 vendor$, ven_desc$
            readkey$ = "BAL VENOR" & vendor$
            gosub test_vendor
            if errormsg$ <> " " then return            

        return

    test_vendor
        call "DESCRIBE" (#4, readkey$, ven_desc$, 0%, f1%(4))
            if f1%(4) = 0% then errormsg$="Invalid Vendor              "
        return

L50030: rem Enter Balance Type                            bal_type$, bal_type_desc$
            readkey$ = "BAL TYPES" & bal_type$
            gosub test_types 
            if errormsg$ <> " " then return

        return

    test_types 
        call "DESCRIBE" (#4, readkey$, bal_type_desc$, 0%, f1%(4))
        if f1%(4) = 0% then errormsg$ = "Invalid Types Desc "
        return


L50040: rem Enter Friction Fraction                       friction$
        init(" ") value$ 
        value$ = friction$
        gosub check_value
           if val% <> 1% then goto bad_val1
        friction = value
        friction$ = value$

        return

    bad_val1
        errormsg$ = "Invalid data for Friction Value"
        return



L50050: rem Enter Weight Value                            weight$   
        init(" ") value$ 
        value$ = weight$   
        gosub check_value
           if val% <> 1% then goto bad_val2
        weight = value
        weight$ = value$

        return

    bad_val2
        errormsg$ = "Invalid data for Weight Value"
        return

/*(AWD001)*/
L50060: rem Enter Perimeter Plus Weight                   perimeter$
        init(" ") value$ 
        value$ = perimeter$
        gosub check_value
           if val% <> 1% then goto bad_val3
        perimeter = value
        perimeter$ = value$

        return

    bad_val3
        errormsg$ = "Invalid data for Perimeter Plus Weight Value"
        return

/*(\AWD001)*/

      check_value
        val%  = 0%
        value = 0.00
        convert value$ to value, data goto bad_value
REM        if value < 0 or value > 999.99 then goto bad_value
        if value > 99.99 then goto bad_value        

        if value < 0 then ~
        convert value to value$, pic(00.0000-)
        if value >= 0 then ~
        convert value to value$, pic(000.0000)

        val% = 1%
      bad_value
      return

/*<AWD002>+ */
L50070: rem Enter Vendor 2                             vendor2$, ven_desc2$
            if vendor2$ = " " then return
            readkey$ = "BAL VENOR" & vendor2$
            gosub test_vendor2
            if errormsg$ <> " " then return            

        return

    test_vendor2
        if vendor2$ = " " then return
        call "DESCRIBE" (#4, readkey$, ven_desc2$, 0%, f1%(8))
            if f1%(8) = 0% then errormsg$="Invalid 2nd Vendor          "
        return

L50080: rem Enter Balance Type2                   bal_type2$, bal_type_desc2$
            if bal_type2$ = " " then return
            readkey$ = "BAL TYPES" & bal_type2$
            gosub test_types2 
            if errormsg$ <> " " then return

        return

    test_types2 
        if bal_type2$ = " " then return
        call "DESCRIBE" (#4, readkey$, bal_type_desc2$, 0%, f1%(9))
        if f1%(9) = 0% then errormsg$ = "Invalid 2nd Types Desc "
        return
        
N50050: rem Enter Max Weight Value                 Max weight$   
        init(" ") value$ 
        value$ = max_weight$   
        gosub check_value
           if val% <> 1% then goto bad_val4
        max_weight = value
        max_weight$ = value$

        return

    bad_val4
        errormsg$ = "Invalid data for Weight Value"
        return

/*<AWD002>- */
/*<IM7691 + */

N50070: rem Enter Range #1 Max Window Height(in)   Range_max1$   
        init(" ") value$ 
        value$ = range_max1$   
        gosub check_value     
           if val% <> 1% then goto bad_val6
        range_max1 = value
        range_max1$ = value$

        return
        
    bad_val6 
        errormsg$ = "Invalid data for Range #1 Max Window Height"
        return

N50080: rem Enter Range #1 Max Window Weight(in)   Range_max1_wt$
        init(" ") value$ 
        value$ = range_max1_wt$
        gosub check_value     
           if val% <> 1% then goto bad_val7
        range_max1_wt = value
        range_max1_wt$ = value$

        return

    bad_val7 
        errormsg$ = "Invalid data for Range #1 Max Window Weight"
        return

N50100: rem Enter Range #2 Max Window Height(in)   Range_max2$   
        init(" ") value$ 
        value$ = range_max2$   
        gosub check_value     
           if val% <> 1% then goto bad_val9
        range_max2 = value
        range_max2$ = value$

        return
        
    bad_val9 
        errormsg$ = "Invalid data for Range #2 Max Window Height"
        return

N50110: rem Enter Range #2 Max Window Weight(in)   Range_max2_wt$
        init(" ") value$ 
        value$ = range_max2_wt$
        gosub check_value     
           if val% <> 1% then goto bad_val10
        range_max2_wt = value
        range_max2_wt$ = value$

        return

    bad_val10
        errormsg$ = "Invalid data for Range #2 Max Window Weight"
        return

N50130: rem Enter Range #3 Max Window Height(in)   Range_max3$   
        init(" ") value$ 
        value$ = range_max3$   
        gosub check_value     
           if val% <> 1% then goto bad_val12
        range_max3 = value
        range_max3$ = value$

        return
        
    bad_val12
        errormsg$ = "Invalid data for Range #3 Max Window Height"
        return

N50140: rem Enter Range #3 Max Window Weight(in)   Range_max3_wt$
        init(" ") value$ 
        value$ = range_max3_wt$
        gosub check_value     
           if val% <> 1% then goto bad_val13
        range_max3_wt = value
        range_max3_wt$ = value$

        return
        
    bad_val13
        errormsg$ = "Invalid data for Range #3 Max Window Weight"
        return

N50160: rem Enter Range #4 Max Window Height(in)   Range_max4$   
        init(" ") value$ 
        value$ = range_max4$   
        gosub check_value     
           if val% <> 1% then goto bad_val15
        range_max4 = value
        range_max4$ = value$

        return

    bad_val15
        errormsg$ = "Invalid data for Range #4 Max Window Height"
        return

        N50170: rem Enter Range #4 Max Window Weight(in)   Range_max4_wt$
        init(" ") value$ 
        value$ = range_max4_wt$
        gosub check_value     
           if val% <> 1% then goto bad_val16
        range_max4_wt = value
        range_max4_wt$ = value$

        return

    bad_val16
        errormsg$ = "Invalid data for Range #4 Max Window Weight"
        return
/*<IM7691> - */
        
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

            for fieldnr% = 1% to 5%
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
	    if col% = 1%  then fieldnr% = 3%
	    if col% > 1%  then fieldnr% = fieldnr% + 3%
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
            sav_key$ = str(f_model$) & str(f_t_b$) 
            f_sav_key$ = str(f_model$) & str(f_t_b$) 
            t_sav_key$ = str(t_model$) & str(f_t_b$) 
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
            s_sav_key$ = str(s_model$) & str(s_t_b$) 
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
        read #1, key >= sav_key$, using F00001, sav_key$, eod goto fini   
	goto first_read
readnext: read #1, key > sav_key$, using F00001, sav_key$, eod goto fini   
first_read:
	if f_t_b$ = "B" and str(sav_key$,4,1) = "T" then goto readnext
	if f_t_b$ = "T" and str(sav_key$,4,1) = "B" then goto readnext
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
        read #1, hold, key >= sav_key$, using F00001, sav_key$, eod goto e_o_f  
	goto firstread
readnxt: read #1, hold, key > sav_key$, using F00001, sav_key$, eod goto e_o_f  
firstread:
F00001: FMT CH(27)
F00002: FMT CH(256)
        if sav_key$ > t_sav_key$ then e_o_f
	if f_t_b$ = "B" and str(sav_key$,4,1) = "T" then goto readnxt
	if f_t_b$ = "T" and str(sav_key$,4,1) = "B" then goto readnxt
        if func$ = "D" then delete_it
	get #1, using F00002, rec$
	if str(sav_key$,1,3) <> f_model$ then gosub inc_model
	f_model$ = str(sav_key$,1,3)
	str(s_sav_key$,4,1) = str(sav_key$,4,1)       
	if s_t_b$ <> "*" then str(s_sav_key$,4,1) = s_t_b$                  
        str(rec$,1,4)  = str(s_sav_key$,1,4)
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
         "Enter Model Number                                           ",~
         "Enter 'T'op, 'B'ottom or '*' All                             ",~
         "Enter Model Number                                           ",~
         "Enter 'T'op or 'B'ottom                                      "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables2
            init(" ") errormsg$, inpmessage$, f_model$, t_model$,        ~
                      s_model$, f_t_b$, s_t_b$

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
               at (08,02), "Vendor              :",                      ~
               at (08,30), fac(lfac$(1%)), f_model$             , ch(03),~
               at (08,45), fac(lfac$(3%)), t_model$             , ch(03),~
               at (08,60), fac(lfac$(4%)), s_model$             , ch(03),~
                                                                         ~
               at (09,02), "Top or Bottom       :",                      ~
               at (09,30), fac(lfac$(2%)), f_t_b$               , ch(01),~
               at (09,60), fac(lfac$(5%)), s_t_b$               , ch(01),~
                                                                         ~
               at (11,30), "T = Just Top, B = Just Bottom, * = All",     ~
               at (12,30), "For To, '*' = Keep Value, Else Override",    ~
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
   	if fieldnr% > 3% and edit% = 1% then edit% = 2%

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
            on fieldnr% gosub L90100,        /* Model                  */~
                              L90200,        /* Top or Bottom          */~
                              L90300,        /* Model                  */~
                              L90500,        /* Model                  */~
                              L90600         /* Top or Bottom          */


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
            if t_b$ <> "T" and t_b$ <> "B" and t_b$ <> "*" then goto L50024

        return

L90300: rem Enter Vendor Code                vendor$, ven_desc$
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
            if t_b$ <> "T" and t_b$ <> "B" and t_b$ <> "*" then goto L50024

        return

REM +-------------------------------------------------------------------+
REM +-------------------------------------------------------------------+
        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

*       display_codes
*           call "APCPLN1B" (tab%, #4)
*       return

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
            
