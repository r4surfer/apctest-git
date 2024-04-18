
        REM *************************************************************~
            *                                                           *~
            *  EEEEE  FFFFF  FFFFF  IIIII   CCCC  IIIII  TTTTT   AAA    *~
            *  E      F      F        I    C   C    I      T    A   A   *~
            *  EEEE   FFFF   FFFF     I    C        I      T    AAAAA   *~
            *  E      F      F        I    C   C    I      T    A   A   *~
            *  EEEEE  F      F      IIIII   CCCC  IIIII    T    A   A   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EFFICITA - Monthly Saved Values for the New Efficiency    *~
            *            Report                                         *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/07/00 + New Program                              + TBM *~
            * 07/21/04 ! (EWD001) Mod to make dept '054' calc unit! CMG *~
            *          !            just like '044'               !     *~
            * 01/09/08 ! (AWD002) mods for new dept 074           ! CMG *~
            * 02/20/2009! (AWD003) mod for ASM screen 029 like 000! CMG *~
            *10/27/2010! (AWD004) modifications to store avg price! CMG *~
            *          !   by quarter                             !     *~
			*01/20/2023! CR3230  mod to add RDB to approval list  ! RDB *~
            *************************************************************

        dim ex_key$12,                    /* Read Key                   */~
            eff_year$4,                  /* Production Year            */~
            eff_dept$3,                  /* Department                 */~
            eff_model$3,                 /* Production Model           */~
            model$(300)3,                /* Production Model           */~
            ind_dept$(40%),              /* Indirect Department Values */~
            eff_units(12%),              /* Prod. Planning UPMH        */~
            eff_scrapa(12%),             /* Average weight             */~
            eff_scrapb(12%),             /* In Process weight          */~
            eff_price(12%),              /* Average Price              */~
            eff_matrl(12%),              /* Material Cost/Lbs          */~
            eff_labor(12%),              /* Labor Dollar Goal/Unit    */~
            eff_lbs(12%),                /* Lbs/Unit Goal             */~
            eff_scrapa$14,               /* Average weight             */~
            eff_scrapb$14,               /* In Process Weight          */~
            eff_unit$2,                  /* Planning Unit              */~
            eff_units$14,                /* Planning UPMH              */~
            eff_price$14,                /* Average Selling Price      */~
            eff_matrl$14,                /* Average weight             */~
            eff_labor$14,                /* Labor Dollar Goal/Unit    */~
            eff_lbs$14,                  /* Lbs/Unit Goal             */~
            unitkey$5,                   /* units per man Hour         */~
            savekey$9,                   /* save key                   */~
            modkey$50,                   /* model key                  */~
            mod_desc$30,                 /* model description          */~
            descript$30,                 /* description                */~
            scrkey$12%,                  /* scrap A key                */~
            scrkey1$12%,                 /* scrap B key                */~
            readkey$55,                  /* read key                   */~
            readkey1$24,                 /* read key1                  */~
            readkey2$24,                 /* read key2                  */~
            readkey3$24,                 /* read key3                  */~
            readkey4$24,                 /* read key2                  */~
            savekey1$9,                  /* save key1                  */~
            savekey2$12,                  /* save key2                  */~
            savekey3$9,                  /* save key3                  */~
            pl_key$15,                   /* Planning Read Key          */~
            pl_unt$(3%)3,                /* Planning units             */~
            apc_qty(12%), apc_qtz(12%),  /* Total Qty's, TOT an Zero To*/~
            apc_pc(12%),                 /* Total Selling Price by Mon */~
            avg_key$4                    /* Average Pric Read Key      */

       dim                              /* Extra Variables            */~
            f2%(8%),                     /* = 0 if the file is open    */~
            f1%(8%),                     /* = 1 if READ was successful */~
            fs%(8%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(11%)20,                /* Text from file opening     */~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(297%)1,                /* Field Attribute Characters */~
            lfac1$1,                     /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$40,                   /* PF Key Hex Values          */~
            msg$(3%)79,                  /* ASKUSER TEXT               */~
            hh$40,                       /* Open Error Header Msg      */~
            userid$3                     /* Current User Id            */
            
        dim scrn_message$80,             /* Screen Message (AWD004)    */~
            calendar$10,                 /* Calendar                   */~
            calendar$(12%)10,            /* Calendar Month Array       */~
            quarter$(4)10,               /* Calendar Quarter Array     */~
            dft_price(12),               /* Default Price for Creating Rec*/~
            qtr_price(4),                /* AVG by Quarter             */~
            dpt_number$(999)3,           /* Department Numbers         */~
            dpt_price(999),              /* Department Quarterly Price */~
            savModel$3,                  /* Save Model Number          */~
            savDept$3                    /* Save Department Number     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Efficiency Table/Average Storage"
            pname$ = "EFFICITA - Rev: 1.00"

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
            * #1  ! APCPLNDP ! Planning master Department File          *~
            * #2  ! GENCODES ! System Master Code Table Files           *~
            * #3  ! APCIVAVG ! Monthly Invoice Sales Average            *~
            * #4  ! EWDEFFEX ! Efficiency Table/Average Storage         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "APCPLNDP",                                     ~
                        varc,     indexed,  recsize = 32,                ~
                        keypos =    11, keylen =   12,                   ~
                        alt key 1, keypos = 9, keylen = 14,              ~
                            key 2, keypos = 4, keylen = 12,              ~
                            key 3, keypos = 1, keylen = 15              
                      
            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #3,   "APCIVAVG",                                     ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen =   4                      ~

            select #4,   "EWDEFFEX",                                     ~
                         varc,    indexed,  recsize = 1024,              ~
                         keypos =   1, keylen =   12

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
	          call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  500%, rslt$(4%))
 
            mat f1% = zer

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

            calendar$( 1%) = "January  "
            calendar$( 2%) = "February "
            calendar$( 3%) = "March    "
            calendar$( 4%) = "April    "
            calendar$( 5%) = "May      "
            calendar$( 6%) = "June     "
            calendar$( 7%) = "July     "
            calendar$( 8%) = "August   "
            calendar$( 9%) = "September"
            calendar$(10%) = "October  "
            calendar$(11%) = "November "
            calendar$(12%) = "December "
            calendar$(14%) = "January  "
            calendar$(15%) = "February "
            calendar$(16%) = "March    "
            calendar$(17%) = "April    "
/* (AWD004) */
            quarter$( 1%) = "1st Qtr"
            quarter$( 2%) = "2nd Qtr"
            quarter$( 3%) = "3rd Qtr"
            quarter$( 4%) = "4th Qtr"

            ind_dept$(1%)  = "012" : ind_dept$(2%)  = "013"
            ind_dept$(3%)  = "015" : ind_dept$(4%)  = "022"
            ind_dept$(5%)  = "024" : ind_dept$(6%)  = "030"
            ind_dept$(7%)  = "030" : ind_dept$(8%)  = "031"
            ind_dept$(9%)  = "032" : ind_dept$(10%) = "034"
            ind_dept$(11%) = "035" : ind_dept$(12%) = "037"
            ind_dept$(13%) = "038" : ind_dept$(14%) = "039"
            ind_dept$(15%) = "045" : ind_dept$(16%) = "061"
            ind_dept$(17%) = "010" : ind_dept$(18%) = "001"
            ind_dept$(19%) = "071" : ind_dept$(20%) = "041"

            ind_dept$(21%) = "060" : ind_dept$(22%) = "057"
            ind_dept$(23%) = "   " : ind_dept$(24%) = "   "
            ind_dept$(25%) = "   " : ind_dept$(26%) = "   "
            ind_dept$(27%) = "   " : ind_dept$(28%) = "   "
            ind_dept$(29%) = "   " : ind_dept$(30%) = "   "
            ind_dept$(31%) = "   " : ind_dept$(32%) = "   "
            ind_dept$(33%) = "   " : ind_dept$(34%) = "   "
            ind_dept$(35%) = "   " : ind_dept$(36%) = "   "
            ind_dept$(37%) = "   " : ind_dept$(38%) = "   "
            ind_dept$(39%) = "   " : ind_dept$(40%) = "   "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            for fieldnr% = 1% to 2%
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
                  if keyhit%  = 14% then goto inputmode_report
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
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
         "Enter a Valid Reporting Period ( 01 thru 12)                  ",~
         "Enter a Valid Processing Year.                                "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, eff_year$, eff_model$,     ~
                      eff_price$, eff_unit$, eff_scrapa$, eff_scrapb$,   ~
                      readkey$, savekey$, unitkey$, savekey1$, modkey$,  ~
                      mod_desc$, scrkey$, scrkey1$, period$,       ~
                      calendar$, yr$, eff_units$, eff_lbs$, eff_matrl$,  ~
                      eff_labor$, readkey1$, readkey2$, savekey2$,       ~
                      readkey3$, savekey3$, model$()

              scrn_qtr% = 0%                                   /* (AWD004)  */
              scrn_message$ = "Price Analysis Report Month  :" /* (AWD004) */
            return

       initialize_rec
            init(" ") mod_desc$, descript$, modkey$, eff_scrapa$,       ~
                      eff_scrapb$, ex_key$, eff_price$,      ~
                      eff_lbs$, eff_labor$, eff_matrl$

            eff_scrapa, eff_scrapb, eff_matrl, eff_lbs, eff_labor = 0.0
            
            mat eff_price  = zer
            mat eff_units  = zer
            mat eff_scrapa = zer
            mat eff_scrapb = zer
            mat eff_labor  = zer
            mat eff_lbs    = zer
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
       dataload
            init(" ") ex_key$
            str(ex_key$,1%,4%) = eff_year$
            str(ex_key$,5%,3%) = eff_dept$
            str(ex_key$,8%,3%) = eff_model$
            str(ex_key$,11%,2%) = eff_unit$

           read #4, hold, key = ex_key$, using L30080, eff_year$, eff_dept$, eff_model$, ~
                                      eff_unit$, eff_price(), eff_units(),    ~
                                      eff_scrapa(), eff_scrapb(), eff_labor(),~
                                      eff_lbs(), eff_matrl(), eod goto L30060

L30060:     calendar$ = " "
            period% = 0%
            if period$ <> " " then goto L25000
               period$ = str(date$,1%,2%)
L25000:     convert period$ to period%, data goto L50270

            convert period% to period$, pic(00)

            calendar$ = calendar$(period%)

            gosub convert_screen

            goto display_values

       REM *************************************************************~
           *          S T U F F   D A T A   I N T O   F I L E          *~
           *-----------------------------------------------------------*~
           * Stuffs data from Program Variables into File Record Area. *~
           *************************************************************

        dataput
           call "SHOSTAT" ("Updating Table Values")
           init(" ") ex_key$
           str(ex_key$,1%,4%)   = eff_year$
           str(ex_key$,5%,3%)   = eff_dept$
           str(ex_key$,8%,3%)   = eff_model$
           str(ex_key$,11%,2%)   = eff_unit$

           read #4, hold, key = ex_key$, using L30080, eff_year$, eff_dept$, eff_model$, ~
                                      eff_unit$, eff_price(), eff_units(),    ~
                                      eff_scrapa(), eff_scrapb(), eff_labor(),~
                                      eff_lbs(), eff_matrl(), eod goto L30065

                delete #4


L30065:    for i% = period% to 12%

                convert eff_price$  to eff_price(i%),  data goto L60030

L60030:         convert eff_units$  to eff_units(i%),  data goto L60040

L60040:         convert eff_scrapa$ to eff_scrapa(i%), data goto L60050

L60050:         convert eff_scrapb$ to eff_scrapb(i%), data goto L60060

L60060:         convert eff_labor$  to eff_labor(i%),  data goto L60070

L60070:         convert eff_lbs$  to eff_lbs(i%),  data goto L60080

L60080:         convert eff_matrl$  to eff_matrl(i%),  data goto L60090

L60090:   
           next i%

                                                                             
           put #4, using L30080, eff_year$,    /* Production Year           */~
                   eff_dept$,                  /* Department                */~
                   eff_model$,                 /* Production Model          */~
                   eff_unit$,                  /* Planning Unit code        */~
                   eff_price(),                /* Efficiency average price  */~
                   eff_units(),                /* Planning UPMH             */~
                   eff_scrapa(),               /* average weight            */~
                   eff_scrapb(),               /* mistake scrap             */~
                   eff_labor(),                /* Labor Dollar Goal/Unit    */~
                   eff_lbs(),                  /* Lbs/Unit Goal             */~
                   eff_matrl()                 /* Material Cost/Lb          */

L30080:    FMT  CH(4), CH(3), CH(3), CH(2), 84*PD(14,4)

           write #4, eod goto L30097
         return 
L30097:  errormsg$ = "(Error) - Unable to update the Table/Average File"
             gosub error_prompt
         return    

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
              on fieldnr% gosub L40190,         /* Report Period       */ ~
                                L40180          /* Processing Year     */ 
              goto L40210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40190:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40210:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,24), fac(hex(a4)), apc$                   , ch(38),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), fac(hex(8c)), scrn_message$          , ch(30),~
               at (06,35), fac(lfac$(1%)), period$              , ch(02),~
               at (06,40), fac(hex(84)),   calendar$            , ch(09),~
                                                                         ~
               at (07,02), "Price Analysis Report Year   :",             ~
               at (07,35), fac(lfac$(2%)), yr$                  , ch(04),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~                              
               keys(pfkeys$), key(keyhit%)
               
/* (AWD004) */
               if keyhit% <>  3% then goto L40505
                  if scrn_qtr% = 0% then goto setQtrMsg
                     scrn_qtr% = 0%
                     scrn_message$ = "Price Analysis Report Month  :"
                     goto L40210
setQtrMsg:
                  scrn_qtr% = 1%
                  scrn_message$ = "Price Analysis Report Quarter:"
                  goto L40210
                  
/*(/AWD004)*/

L40505:        if keyhit% <> 10% then goto L40510
                  goto display_values

L40510:        if keyhit% <> 15% then goto L40520
                  call "PRNTSCRN"
                  goto L40210

L40520:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40710     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "(3)Quarter AVG   (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ff0304ffffffffff0Affffffff0f1000)
            if fieldnr% = 1% then L40670
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L40670:     if fieldnr% > 1% then L40690
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40690:     return

L40710: if fieldnr% > 0% then L40800  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Efficiency"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40800:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

       REM**************************************************************~
          *             R E P O R T  S C R E E N                       *~
          *------------------------------------------------------------*~
          * Document Input and Edit Screen.                            *~
          **************************************************************
       display_values    
/* CR3230 Add RDB */
              if userid$ = "RRH" or userid$ = "CMG" or             ~
                 userid$ = "TM2" or userid$ = "TNR" or             ~
                 userid$ = "KH1" or userid$ = "ES1" or             ~
				 userid$ = "RDB"                                   ~
                                 then lfac1$ = hex(81)             ~
                                 else lfac1$ = hex(84)
             gosub set_pf2

L42270:     accept                                                           ~
                at (01,02),                                                  ~ 
                   "New Efficiency Table/Average-Report Screen - (EFFICITA)",~
                at (01,66), "Today:",                                        ~
                at (01,73), fac(hex(8c)), date$                   , ch(08),  ~
                at (02,02), fac(hex(94)), errormsg$               , ch(79),  ~
                                                                             ~
                at (03,02), "Production Year              :",                ~
                at (03,35), fac(lfac1$), eff_year$                , ch(04),  ~
                                                                             ~
                at (04,02), "Department                   :",                ~
                at (04,35), fac(lfac1$), eff_dept$                , ch(03),  ~    
                                                                             ~
                at (05,02), "Price Analysis Report Month  :",                ~
                at (05,35), fac(lfac1$), period$                  , ch(02),  ~
                at (05,40), fac(hex(84)),   calendar$             , ch(09),  ~
                                                                             ~
                at (06,02), "Production Model             :",                ~
                at (06,35), fac(lfac1$), eff_model$               , ch(03),  ~
                at (06,45), fac(hex(84))  , mod_desc$             , ch(30),  ~
                                                                             ~
                at (07,02), "Planning Unit code           :",                ~
                at (07,35), fac(lfac1$), eff_unit$                , ch(02),  ~
                                                                             ~
                at (08,02), "Efficiency Average Price     :",                ~
                at (08,35), fac(lfac1$), eff_price$               , ch(14),  ~
                                                                             ~
                at (09,02), "Planning UPMH                :",                ~
                at (09,35), fac(lfac1$), eff_units$               , ch(14),  ~
                                                                             ~
                at (10,02), "Average weight               :",                ~
                at (10,35), fac(lfac1$), eff_scrapa$              , ch(14),  ~
                                                                             ~
                at (11,02), "Mistake scrap                :",                ~
                at (11,35), fac(lfac1$), eff_scrapb$              , ch(14),  ~
                                                                             ~
                at (12,02), "Material Cost Per Lb.        :",                ~
                at (12,35), fac(lfac1$), eff_matrl$               , ch(14),  ~
                                                                             ~
                at (13,02), "Labor Dollar Goal/Unit       :",                ~
                at (13,35), fac(lfac1$), eff_labor$               , ch(14),  ~
                                                                             ~
                at (14,02), "Lbs/Unit Goal                :",                ~
                at (14,35), fac(lfac1$), eff_lbs$                 , ch(14),  ~
                                                                             ~
                at (15,02), fac(hex(a4)),   inpmessage$           , ch(79),  ~
                at (15,02), fac(hex(8c)),   pf$(1%)               , ch(79),  ~
                                                                             ~
                keys(pfkeys$), key(keyhit%)

                if keyhit% <> 1% then goto L43010
                   goto startover

L43010:         if keyhit% <> 15% then goto L43020
                   call "PRNTSCRN"
                   goto L42270

L43020:         if keyhit% <> 16% then goto L43040
                   gosub dataput
                   goto inputmode

L43040:         if keyhit% <> 14% then goto L43030
                goto dataload

L43030:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

            set_pf2
               pf$(1%)= "(1)Start Over      (15)Print Screen     "&         ~
                        "(14)Dataload           (16)Exit Program"
               pfkeys$ = hex(01ffffffffffffffffffffffffff0f10000E)
               inpmessage$ = "Press Return to Continue."

            return
                             

       REM *************************************************************~
           *                     T E S T   D A T A                     *~
           *-----------------------------------------------------------*~
           * Test data for the items on Screen 1.                      *~
           *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50140,         /* Reporting Period      */ ~
                              L50310          /* Reporting Year        */ 
            return

L50140: REM Price Analysis Period               PERIOD$
            begMth%, endMth% = 0%
            if scrn_qtr% = 1% then goto validateQtr  /* (AWD004) */
            month% = 1% : calendar$ = " "
            if period$ <> " " then goto L50180
               period$ = str(date$,1%,2%)
L50180:     convert period$ to period%, data goto L50270

            convert period% to period$, pic(00)

            if period% < 1% or period% > 12% then goto L50270
            month% = period%
            calendar$ = calendar$(period%)
        return
L50270:     errormsg$ = "Must Enter a Valid Pricing Month."
            init(" ") period$, calendar$
        return
L50280:     errormsg$ = "Must Enter a Valid Pricing Quarter."
            init(" ") period$, calendar$
        return
validateQtr:
            convert period$ to period%, data goto L50280
            
            convert period% to period$, pic(00)
            
            if period% < 1% or period% > 4% then goto L50280
            
            calendar$ = quarter$(period%)
            
            if period% = 1% then begMth% = 1%
            if period% = 1% then endMth% = 3%
            
            if period% = 2% then begMth% = 4%
            if period% = 2% then endMth% = 6%
            
            if period% = 3% then begMth% = 7%
            if period% = 3% then endMth% = 9%
            
            if period% = 4% then begMth% = 10%
            if period% = 5% then endMth% = 12%
            
        return

L50310: REM Price Analysis Processing Year      YR$
            if yr$ <> " " then goto L50340
               yr$ = str(date$,7%,4%)
L50340:     convert yr$ to yr%, data goto L50400

            convert yr% to yr$, pic(####)

            if yr$ > str(date$,7%,4%) then goto L50400
            if yr% < 1990             then goto L50400
        return
L50400:     errormsg$ = "(Error) - Invalid Processing Year?"
            init(" ") yr$
        return

        inputmode_report
            if scrn_qtr% = 1% then goto inputmode_report_qtr
            p_no% = 0%
            eff_year$ = yr$
            init(" ") pl_key$, savekey$, readkey1$, savekey1$,  ~
                        readkey2$, savekey2$, readkey3$, savekey3$
            readkey1$ = "APC EFFDP"
            savekey1$ = str(readkey1$,1%,9%)
        read_pl_nxt
            gosub initialize_rec
            init(" ") eff_dept$
            indirect% = 0%
            p_no% = 0%
            read #2, key > readkey1$, using L52015, readkey1$,           ~
                                      eod goto read_pl_done
            if str(readkey1$,1%,9%) <> str(savekey1$,1%,9%) then         ~
                                        goto inputmode_done

        read_pl_done

            eff_dept$ = str(readkey1$,13%,3%)
            if eff_dept$ = "LLL" or eff_dept$ = "SSS" then goto read_pl_nxt
            if eff_dept$ = "DDD" then goto inputmode_done
            for k% = 1% to 40%
              if eff_dept$ = ind_dept$(k%) then indirect% = 1%
            next k%
            init(" ") readkey2$
            str(readkey2$,1%,9%) = "APC EFFSC"
            str(readkey2$,10%,3%) = eff_dept$
            str(savekey2$,1%,12%) = readkey2$
         read_model_next2
            read #2, key > readkey2$, using L52015, readkey2$,             ~
                                      eod goto read_model_done

            if str(readkey2$,1%,12%) <> str(savekey2$,1%,12%) then         ~
                                        goto read_model_done
            if eff_dept$ = "044" and str(readkey2$,14%,2%) <> "01" then ~
                                         goto read_model_next2
                                                    /*  (EWD001)  */
            if eff_dept$ = "054" and str(readkey2$,14%,2%) <> "01" then ~
                                         goto read_model_next2
                                                    /*  (AWD002)  */
REM            if eff_dept$ = "074" and str(readkey2$,14%,2%) <> "01" then ~
                                         goto read_model_next2
            p_no% = p_no% + 1%
            model$(p_no%) = str(readkey2$,13%,3%)
            goto read_model_next2
        read_model_done
         for j% = 1% to p_no%
            eff_model$ = model$(j%)
            gosub write_extras
            init(" ") pl_key$
            str(pl_key$,1%,3%) = eff_model$
            str(pl_key$,4%,2%) = "01"
            str(pl_key$,6%,3%) = eff_model$
            str(pl_key$,9%,2%) = "01"
            str(pl_key$,11%,3%) = eff_dept$
            str(pl_key$,14%,2%) = "01"

            read #1, key 3% = pl_key$, using L52010, eff_units, eod goto L52020
                                                  
L52010:         FMT POS(23), PD(14,4)
L52015:         FMT CH(24) 
           
L52020:     if eff_dept$ = "000" then gosub screen_upmh
                                       /* (AWD003) */
            if eff_dept$ = "029" then gosub screen_upmh
            if indirect% = 1% then goto read_in_nxt
L52030:     convert eff_units to eff_units$, pic(-00000000.00##)

             if str(eff_model$,1%,2%) = "A0" then read_pl_nxt2
             if str(eff_model$,1%,2%) = "B0" then read_pl_nxt2
             if str(eff_model$,1%,2%) = "C0" then read_pl_nxt2
             if str(eff_model$,1%,2%) = "D0" then read_pl_nxt2
             if str(eff_model$,1%,2%) = "E0" then read_pl_nxt2
             if str(eff_model$,1%,2%) = "F0" then read_pl_nxt2
             if str(eff_model$,1%,2%) = "G0" then read_pl_nxt2 
             if str(eff_model$,1%,2%) = "H0" then read_pl_nxt2
             if str(eff_model$,1%,2%) = "J0" then read_pl_nxt2

L52070:     gosub calc_avg_price
            if indirect% = 1% then eff_price(period%) = 0.0

            convert eff_price(period%) to eff_price$, pic(-00000000.00##)
          
REM            if eff_unit$ <> "01" and eff_dept$ <> "000" then goto L52040
/* (AWD003) */
            if eff_unit$ <> "01" and eff_dept$ <> "000" and             ~
                                     eff_dept$ <> "029" then goto L52040
            init(" ") readkey$
            str(readkey$,1%,9%) = "APC EFFSC"
            str(readkey$,10%,3) = eff_dept$
            str(readkey$,13%,3%) = eff_model$
            read #2, key = readkey$, using L52000, eff_scrapa$, eff_scrapb$, ~
                                                   eod goto L52040
L52000:     FMT POS(25), CH(7), CH(10)

            convert eff_scrapa$ to eff_scrapa, data goto L60100
L60100:
            convert eff_scrapb$ to eff_scrapb, data goto L60110
L60110:
L52040:     convert eff_scrapa to eff_scrapa$, pic(-00000000.00##)

            convert eff_scrapb to eff_scrapb$, pic(-00000000.00##)


            init(" ") modkey$, eff_matrl$, descript$
            eff_matrl, eff_labor, eff_lbs = 0.0
            modkey$ = "APC EFFMC"  &  eff_dept$
            call "DESCRIBE" (#2, modkey$, descript$, 0%, f1%(2%))
            eff_matrl$ = str(descript$,1%,7%)
            eff_labor$ = str(descript$,11%,6%)
            eff_lbs$ = str(descript$,21%,6%)
            convert eff_matrl$ to eff_matrl, data goto L60000

L60000:     convert eff_labor$ to eff_labor, data goto L60010

L60010:     convert eff_lbs$ to eff_lbs, data goto L60020

L60020
            convert eff_matrl to eff_matrl$, pic(-00000000.00##)

            convert eff_labor to eff_labor$, pic(-00000000.00##)

            convert eff_lbs to eff_lbs$, pic(-00000000.00##)
            
REM            gosub display_values
            gosub dataput
            eff_unit$ = "01"
         next j%
         goto  read_pl_nxt
        inputmode_done
          eff_dept$ = "043"
          eff_model$ = "SSS"
          init(" ") readkey4$, descript$, eff_units$
          str(readkey4$,1%,9%) = "APC EFFID"
          str(readkey4$,10%,3%) = eff_dept$
          str(readkey4$,13%,3%) = eff_model$
          gosub initialize_rec
          read #2, key = readkey4$, using L52060, descript$,  ~
                                    eod goto L60125
             eff_units$ =  str(descript$,24%,6%)
L60125: 
          gosub dataput
        return clear all
        goto inputmode


        read_in_nxt
             init(" ") readkey4$
             str(readkey4$,1%,9%) = "APC EFFID"
             str(readkey4$,10%,3%) = eff_dept$
             str(readkey4$,13%,3%) = eff_model$
             gosub initialize_rec
             read #2, key = readkey4$, using L52060, descript$,  ~
                                       eod goto L52030
L52060:      FMT XX(24), CH(30)

             convert str(descript$,24%,6%) to eff_units, data goto L60120
L60120:
             goto L52030


          read_pl_nxt2
             gosub initialize_rec
             init(" ")readkey3$
             str(readkey3$,1%,9%) = "PLAN UNIT"
             str(readkey3$,10%,2%) = eff_model$

             read #2, key = readkey3$, using L52065, readkey3$, descript$,  ~
                                       eod goto L52070
L52065:      FMT CH(24), CH(30)

             convert str(descript$,16%,6%) to eff_units, data goto L52070
             convert eff_units to eff_units$, pic(-00000000.00##)
             goto L52070

        screen_upmh
          pl_unt$(1%) = "19"
          pl_unt$(2%) = "21"
          pl_unt$(3%) = "17"
          
        for k% = 1% to 3%
         init(" ") pl_key$
         eff_units = 0.0
         str(pl_key$,1%,3%) = eff_dept$
         str(pl_key$,4%,2%) = "01"
         str(pl_key$,6%,2%) = "01"
         str(pl_key$,8%,3%) = eff_model$
         str(pl_key$,11%,2%) = pl_unt$(k%)

         read #1, key = pl_key$, using L52010, eff_units, eod goto L00005

L00005:  convert eff_units to eff_units$, pic(-00000000.00##)
         convert eff_price to eff_price$,   pic(-00000000.00##)
         convert eff_scrapa to eff_scrapa$, pic(-00000000.00##)
         convert eff_scrapb to eff_scrapb$, pic(-00000000.00##)
         convert eff_labor to eff_labor$, pic(-00000000.00##)
         convert eff_lbs to eff_lbs$, pic(-00000000.00##)
         convert eff_matrl to eff_matrl$, pic(-00000000.00##)

         eff_unit$ = pl_unt$(k%)
         gosub dataput
        next k%
       return

        convert_screen

             convert eff_price(period%) to eff_price$,   pic(-00000000.00##)

             convert eff_units(period%) to eff_units$,   pic(-00000000.00##)

             convert eff_scrapa(period%) to eff_scrapa$, pic(-00000000.00##)

             convert eff_scrapb(period%) to eff_scrapb$, pic(-00000000.00##)

             convert eff_labor(period%) to eff_labor$, pic(-00000000.00##)

             convert eff_lbs(period%) to eff_lbs$, pic(-00000000.00##)

             convert eff_matrl(period%) to eff_matrl$, pic(-00000000.00##)
        return


        table_error
           errormsg$ = "Invalid table value in table " & readkey$ & eff_model$
           gosub error_prompt
        return clear all
        goto inputmode


        table_error2
           errormsg$ = "Invalid table value in table " & readkey2$ & eff_dept$
           gosub error_prompt
        return clear all
        goto inputmode

        conv_error
           errormsg$ = "Invalid Screen value!! " 
           gosub error_prompt
        return clear all
        goto inputmode

        calc_avg_price
         avg, ex_avg, ex_pc, mdl_price = 0.0
         mat eff_price = zer
         init(" ") avg_key$, savekey$
         str(avg_key$,1%,3%) = eff_model$
         if eff_dept$ = "044" then str(avg_key$,2%,2%) = "00"
                                                               /*  (EWD001) */
         if eff_dept$ = "054" then str(avg_key$,2%,2%) = "00"
                                                               /*  (AWD002) */
REM         if eff_dept$ = "074" then str(avg_key$,2%,2%) = "00"
         str(savekey$,1%,3%) =  avg_key$
        read_next
        read #3, key > avg_key$, using L52080, avg_key$, apc_qty(), apc_pc(),    ~
                                               apc_qtz(), eod goto L52090
L52080:   FMT CH(4), 12*PD(14,4), 12*PD(14,4), 12*PD(14,4)
        if str(avg_key$,1%,3%) <> str(savekey$,1%,3%) then goto avg_price_done

         if scrn_qtr% = 1% then goto avg_qtr
         avg = abs(apc_qty(month%))
         ex_avg = ex_avg + avg
         ex_pc = ex_pc + apc_pc(month%)
         goto read_next
/* (AWD004) */
avg_qtr:
         for m% = begMth% to endMth%
           avg = abs(apc_qty(m%))
           ex_avg = ex_avg + avg
           ex_pc = ex_pc + apc_pc(m%)
           
           dpt_avg = dpt_avg + avg
           dpt_pc  = dpt_pc + apc_pc(m%)
         next m%
         
         goto read_next
      avg_price_done
         gosub get_disc
         if ex_avg < .01 then goto L52090
         if scrn_qtr% <> 1% then eff_price(period%) = round(ex_pc / ex_avg, 4)
         if scrn_qtr% = 1%  then mdl_price = round(ex_pc / ex_avg, 4)
/* Do not allow negative quarterly price  */
         if scrn_qtr% = 1%  and mdl_price < 0  then mdl_price = 0.00
L52090: return

        get_disc
            init(" ") modkey$, descript$
            eff_disc = 0.0
            str(modkey$,1%,9%) = "APC EFFMN"
            str(modkey$,10%,4%) = yr$
            str(modkey$,14%,2%) = period$
            call "DESCRIBE" (#2, modkey$, descript$, 0%, f1%(2%))

            convert str(descript$,25%,6%) to eff_disc, data goto L52100
L52100:     
            ex_pc = round(ex_pc * eff_disc, 4)
        return
       
        write_extras
          pl_unt$(1%) = "03"
          pl_unt$(2%) = "11"
          pl_unt$(3%) = "15"

        for k% = 1% to 3%
         init(" ") pl_key$
         eff_units = 0.0
         str(pl_key$,1%,3%) = eff_dept$
         str(pl_key$,4%,2%) = "01"
         str(pl_key$,6%,2%) = "01"
         str(pl_key$,8%,3%) = eff_model$
         str(pl_key$,11%,2%) = pl_unt$(k%)

         read #1, key = pl_key$, using L52010, eff_units, eod goto L00003

L00003:  convert eff_units to eff_units$, pic(-00000000.00##)
         convert eff_price to eff_price$,   pic(-00000000.00##)
         convert eff_scrapa to eff_scrapa$, pic(-00000000.00##)
         convert eff_scrapb to eff_scrapb$, pic(-00000000.00##)
         convert eff_labor to eff_labor$, pic(-00000000.00##)
         convert eff_lbs to eff_lbs$, pic(-00000000.00##)
         convert eff_matrl to eff_matrl$, pic(-00000000.00##)


           eff_unit$ = pl_unt$(k%)
           gosub dataput
         next k%
           eff_unit$ = "01"
         return


        error_prompt
           comp% = 2%
           hh$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%))
        return
        
        inputmode_report_qtr
            init(" ") ex_key$, eff_model$, dpt_number$()
            init("ZZZ") savModel$, savDept$
            dpt_count% = 0%
            dpt_avg, dpt_pc = 0.00

            mat qtr_price = zer
            mat dpt_price = zer
            str(ex_key$,1%,4%) = yr$
        report_qtr_next
            read #4, hold, key > ex_key$, using EFFQTRFMT, ex_key$, qtr_price(),~
                                                    eod goto report_qtr_done
EFFQTRFMT:             FMT CH(12), POS(685), 4*PD(14,4)

            if str(ex_key$,1,4) <> yr$ then goto report_qtr_done
            if savDept$ = "ZZZ" then savDept$ = str(ex_key$,5%,3%)
REM            if savModel$ = "ZZZ" then savModel$ = str(ex_key$,8%,3%)

REM if different dept then update overall dept price
            if savDept$ <> str(ex_key$,5%,3%) then gosub updateDptPrices
REM if same model just update data don't recalculate
            if savModel$ = str(ex_key$,8%,3%) then goto updateQtrPrices

            eff_model$ = str(ex_key$,8%,3%)
            savModel$  = str(ex_key$,8%,3%)
            gosub calc_avg_price
            goto updateQtrPrices


        report_qtr_done
          gosub updateDptPrices
          
          gosub writeDptPrices
          
        return clear all
        goto inputmode
        
        updateQtrPrices
            qtr_price(period%) = mdl_price
            
            rewrite #4, using EFFWRITEFMT, qtr_price()
EFFWRITEFMT:        FMT POS(685), 4*PD(14,4)

            goto report_qtr_next
            
         updateDptPrices
REM           call "SHOSTAT" ("Recording Dept --> " & savDept$ )  stop
           dpt_count% = dpt_count% + 1%
           dpt_number$(dpt_count%) = savDept$
           dpt_price(dpt_count%) = round(dpt_pc / dpt_avg, 4)
/* Do not allow negative quarterly price  */
           if dpt_price(dpt_count%) < 0 then dpt_price(dpt_count%) = 0.00
           
           savDept$ = str(ex_key$,5%,3%)
           dpt_pc, dpt_avg = 0.00
         return
         
         writeDptPrices
REM           call "SHOSTAT" ("Write Dept Prices")  stop
           mat dft_price = zer
           init(" ") ex_key$
           str(ex_key$,1%,4%) = yr$
            for i% = 1% to dpt_count%
             str(ex_key$,5%,3%)  = dpt_number$(i%)
             str(ex_key$,8%,3%)  = "ZZZ"             /* Model for Dept     */
             str(ex_key$,11%,2%) = "ZZ"              /* Plan Unit for Dept */

             rec% = 0%
             read #4,hold, key = ex_key$, using EFFQTRFMT, ex_key$, qtr_price(),~
                                                eod goto createDptPrice
                                                
                   rec% = 1%
createDptPrice:
                   qtr_price(period%) = dpt_price(i%)
             
                   if rec% = 1% then rewrite #4, using EFFWRITEFMT, qtr_price() ~
                   else write #4, using EFFCREATEFMT, ex_key$, dft_price(), ~
                     dft_price(), dft_price(), dft_price(), dft_price(),    ~
                     dft_price(), dft_price(), qtr_price()
EFFCREATEFMT:      FMT CH(12), 12*PD(14,4), 12*PD(14,4), 12*PD(14,4), 12*PD(14,4), ~
                       12*PD(14,4), 12*PD(14,4), 12*PD(14,4), 4*PD(14,4)
            next i%
         return
        
       REM *************************************************************~
           *                          E X I T                          *~
           *-----------------------------------------------------------*~
           * Terminates execution (files closed automatically).        *~
           *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
