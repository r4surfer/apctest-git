        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDPLN59                             *~
            *  Creation Date     - 08/11/98                             *~
            *  Last Modified Date- 08/28/98                             *~
            *  Written By        - Brian W. Sanders                     *~
            *                                                           *~
            *  Description       - Entry & modification of tube winding *~
            *                      and coil data. Winds & coils are     *~
            *                      are entered for a Maximum Width &    *~
            *                      Maximum Height combination.          *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *                                                           *~
            *  Subroutine Used   - EWDPLA59 - Data File Listing Rpts    *~
            *                      EWDPLB59 - 'Rapid Entry' Routine     *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/11/98 ! (New) Program                            ! BWS *~
            * 02/02/99 ! Add functionality for EWDPLB59.    EWD001! BWS *~
            * 04/20/99 ! Mods for Lamint Glass & Rpt Selctn.EWD002! BWS *~
            *************************************************************

        dim                                                              ~
            readkey$50, desc$30,         /* GENCODES Lookup & Descr    */~
            sav_key$15,                  /* Use for Loading Table      */~
            new_key$15,                  /* Key for Copying Table      */~
            wrkrec$81,                   /* Work Record for Copying Tbl*/~
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

        dim height$5,                    /* Max. Height for Winds/Coil */~
            model$3, model_cpy$3,        /* Model No.                  */~
            model_d$30,                  /* Model Description          */~
            lkupcd$2, lkupcd_cpy$2,      /* Lkup Diameter Code         */~
            lkupcd_d$30,                 /* Lkup Diameter Cd Descriptn */~
            windcl$(6)6,                 /* Winds/Coil Data Array      */~
            tubedi$(6)2,                 /* Tube Diameter for Wnds/Coil*/~
            width$5                      /* Max. Width for Winds/Coil  */~

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
            apc$   = "(EWD) Tube Windings/Coil Data Entry"
            pname$ = "EWDPLN59 - Rev: R7.00"

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
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCPLNWC",                                      ~
                        varc,     indexed,  recsize =  96,               ~
                        keypos = 1,   keylen = 15

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
/*EWD002*/  actvflds% = 7%          /* No. of Active Fields on-screen  */

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
            if fieldnr% > 4% then fieldnr% = fieldnr% - 1%
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
            enabled% = 0%
            if edit% = 1% or onfile% = 0% or fieldnr% > 4%               ~
                then enabled% = 1%
            if enabled% = 1% and fieldnr% = 2% then lkupcd$ = "00"
            if edit% = 2% or fieldnr% < 6% then goto L26998
                windcl$(fieldnr% - 4%) = windcl$(fieldnr% - 5%)
                tubedi$(fieldnr% - 4%) = tubedi$(fieldnr% - 5%)
L26998: return

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
         "Enter Model Number(s)                                        ",~
         "Enter Lookup Diameter/Coil Code(s)                           ",~
         "Enter Maximum Width (1/8 inch) for Winds/Coil Data           ",~
         "Enter Maximum Height (1/8 inch) for Winds/Coil Data          ",~
         "Enter Normal Winds/Coil & Tube Diameter Code                 ",~
/*EWD*/  "Enter Double Strength Winds/Coil & Tube Diameter Code        ",~
/*002->*/"Enter Laminate Winds/Coil & Tube Diameter Code               ",~
         "                                                             ",~
         "                                                             ",~
         "                                                             "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, height$, model_d$,         ~
                lkupcd_d$, windcl$(), model_cpy$, lkupcd_cpy$
            for x% = 2% to 6%
                tubedi$(x%) = " "
            next x%

            onfile%, copy% = 0%

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
            model$, lkupcd$, width$, windcl$(), tubedi$() = " "
            return clear all
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        DATALOAD
            sav_key$ = str(model$) & str(lkupcd$) & str(width$)          ~ 
                & str(height$)
            read #1, hold, key = sav_key$, using L30100, windcl$(),      ~
                tubedi$(), eod goto L30998
L30100:     fmt pos(16), 6*ch(6), 6*ch(02)
            onfile% = 1%
            return clear all
            goto editpg1


L30998:     return


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        DATAPUT
            put #1, using L35050, model$, lkupcd$, width$, height$,      ~
                windcl$(), tubedi$(), " "
            if onfile% = 1% then rewrite #1, data goto write_err         ~
                else write #1, data goto write_err, eod goto write_err
            GOTO INPUTMODE

            write_err
                errormsg$ = "Error writing to APCPLNWC. Data NOT saved."
                gosub error_prompt
                goto INPUTMODE


        delete_record
            gosub confirm_delete
            delete #1
            goto INPUTMODE

* ----------------------------------------------------------------------

        copy_delt_recs
            count% = 0%
            gosub confirm_action
            on copy% gosub copy_recs, delt_recs
            gosub show_results
            return clear all 
            goto inputmode

          copy_recs
            call "SHOSTAT" ("Copying Records.....")
            if model_cpy$ = model$ and lkupcd_cpy$ = lkupcd$ then L34910
            sav_key$ = all(hex(00))
            str(sav_key$,,5) = str(model$) & str(lkupcd$)
L34890:     read #1, key > sav_key$, using L34900, sav_key$, wrkrec$,    ~
                eod goto L34910
L34900:           fmt ch(15), ch(81)
            if str(sav_key$,,3) <> model$ or str(sav_key$,4,2) <> lkupcd$~
                then goto L34910
            new_key$ = sav_key$
            str(new_key$,,3) = model_cpy$
            str(new_key$,4,2) = lkupcd_cpy$
            write #1, using L34900, new_key$, wrkrec$, data goto L34890, ~
                eod goto L34890
            count% = count% + 1%
            goto L34890

L34910:     return

          delt_recs
            call "SHOSTAT" ("Deleting Records.....")  
            sav_key$ = all(hex(00))
            str(sav_key$,,5) = str(model$) & str(lkupcd$)
L34920:     read #1, hold, key > sav_key$, using L34930, sav_key$,       ~
                eod goto L34940
L34930:           fmt ch(15)          
            if str(sav_key$,,3) <> model$ or str(sav_key$,4,2) <> lkupcd$~
                then goto L34940
            delete #1
            count% = count% + 1%
            goto L34920

L34940:     return

           confirm_action
             comp% = 2%
             hdr$     = "******* WARNING WARNING WARNING  *******"
             msg$(1%) = "Confirm Selected Action -- All records for"
             msg$(2%) = "the above Model/Lookup Code will be XXXXXXX."
             msg$(3%) = "Press <PF25> to Continue, <PF1> to Abort."
             if copy% = 1% then str(msg$(2),37,7) = "COPIED "
             if copy% = 2% then str(msg$(2),37,7) = "DELETED"
             call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
             if comp% = 25% then return
             if comp% <> 1% then goto confirm_action
             return clear all  
             goto inputmode

           show_results
             comp% = 2%
             hdr$     = "******* COPY/DELETE RESULTS *******"
             msg$(1%) = "- - - - - FINISHED - - - - -"
             msg$(2%) = "Total Records Copied or Deleted = #####."
             msg$(3%) = "Press <ENTER> to Continue."
             convert count% to str(msg$(2%),35,5), pic(####0)
             call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
             return


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
L40120:       gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40160,          /* model$             */~
                                L40160,          /* lkupcd$            */~
                                L40170,          /* width$             */~
                                L40170,          /* height$            */~
                                L40160,          /* windcl$/tubedi$(1) */~
                                L40160,          /* windcl$/tubedi$(2) */~
                                L40160,          /* windcl$/tubedi$(3) */~
                                L40160,          /* windcl$/tubedi$(4) */~
                                L40160,          /* windcl$/tubedi$(5) */~
                                L40160           /* windcl$/tubedi$(6) */ 
              if copy% <> 1% or edit% <> 1% then goto L40150
                  lfac$(fieldnr% + 10%) = hex(81)
                  copytxt$ = "^Copy To^"
L40150:       goto L40190

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
               at (03,02), "Model Number:",                              ~
               at (03,30), fac(lfac$(1%)), model$               , ch(03),~
               at (03,40), fac(hex(84)),   model_d$             , ch(30),~
               at (03,72), fac(lfac$(11%)),model_cpy$           , ch(03),~
                                                                         ~
               at (04,02), "Lookup Diameter Code:",                      ~
               at (04,30), fac(lfac$(2%)), lkupcd$              , ch(02),~
               at (04,40), fac(hex(84)),   lkupcd_d$            , ch(30),~
               at (04,72), fac(lfac$(12%)),lkupcd_cpy$          , ch(02),~
                                                                         ~
               at (05,02), "Max. Width for Winds/Coil:",                 ~
               at (05,30), fac(lfac$(3%)), width$               , ch(05),~
               at (05,69), fac(hex(84)),   copytxt$             , ch(09),~
                                                                         ~
               at (06,02), "Max. Height for Winds/Coil:",                ~
               at (06,30), fac(lfac$(4%)), height$              , ch(05),~
                                                                         ~
               at (07,51), fac(hex(a4)), hdr$                   , ch(13),~
               at (07,39), fac(hex(a4)), hdr2$                  , ch(10),~
                                                                         ~
               at (08,02), "Normal Winds/Coil:",                         ~
               at (08,30), fac(lfac$(5%)), windcl$(1)           , ch(06),~
               at (08,43), fac(lfac$(5%)), tubedi$(1)           , ch(02),~
                                                                         ~
               at (09,02), "Dbl Strength Winds/Coil:",                   ~
               at (09,30), fac(lfac$(6%)), windcl$(2)           , ch(06),~
               at (09,43), fac(lfac$(6%)), tubedi$(2)           , ch(02),~
               at (09,52), "(PLAN DBLE)",                                ~
                                                                         ~
/*EWD002*/     at (10,02), "Laminate Winds/Coil:",                       ~
               at (10,30), fac(lfac$(7%)), windcl$(3)           , ch(06),~
               at (10,43), fac(lfac$(7%)), tubedi$(3)           , ch(02),~
/*EWD002*/     at (10,52), "(PLAN LAMN)",                                ~
                                                                         ~
               at (11,02), "******** Winds/Coil:",                       ~
               at (11,30), fac(lfac$(8%)), windcl$(4)           , ch(06),~
               at (11,43), fac(lfac$(8%)), tubedi$(4)           , ch(02),~
               at (11,52), "(  )",                                       ~
                                                                         ~
               at (12,02), "******** Winds/Coil:",                       ~
               at (12,30), fac(lfac$(9%)), windcl$(5)           , ch(06),~
               at (12,43), fac(lfac$(9%)), tubedi$(5)           , ch(02),~
               at (12,52), "(  )",                                       ~
                                                                         ~
               at (13,02), "******** Winds/Coil:",                       ~
               at (13,30), fac(lfac$(10%)), windcl$(6)          , ch(06),~
               at (13,43), fac(lfac$(10%)), tubedi$(6)          , ch(02),~
               at (13,52), "(  )",                                       ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9  then goto L40360
                  copy% = copy% + 1%
                  if copy% > 2% then copy% = 0%
                  goto L40120

L40360:        if keyhit% <> 10 then goto L40370        /*EWD002*/
 /*EWD002*/       call "EWDPLA59" (model$, " ", #1, #4)
                  return clear all                  /* Must do because  */
                  goto inputmode                    /* no record lock.  */

L40370:        if keyhit% <> 11 then goto L40380    /* EWD002 - New */
                  call "EWDPLA59" (model$, lkupcd$, #1, #4)
                  return clear all                  /* Must do because  */
                  goto inputmode                    /* no record lock.  */

L40380:        if keyhit% <> 12 then goto L40390
 /*EWD002*/       call "EWDPLA59" ("ALL", " ", #1, #4)  
                  return clear all                  /* Must do because  */
                  goto inputmode                    /* no record lock.  */

L40390:        if keyhit% <> 14 then goto L40400    /* EWD001 - New */
                  call "EWDPLB59" (#1, #4)  
                  return clear all                  
                  goto inputmode                                          

L40400:        if keyhit% <> 15 then goto L40420
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
/*EWD001*/           "                       (14)Rapid Entry "
            pf$(2) = "           (9)Toggle Copy/Delt: OFF     " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
/*EWD001*/  pfkeys$ = hex(01ffff04ffffffff09ffffffff0e0f1000)
            if copy% = 1% then str(pf$(2),33,4) = "COPY"
            if copy% = 2% then str(pf$(2),33,4) = "DELT"
            if fieldnr% = 1% then L40570
                str(pf$(2),12,25) = " "  :  str(pfkeys$, 9,1) = hex(ff)
/*EWD001*/      str(pf$(1),64)    = " "  :  str(pfkeys$,14,1) = hex(ff)
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
/*EWD002*/  pf$(1) = "(1)Start Over                 (10)Print " &        ~
/*   |  */           "Rpt for This Model                     "
/*   |  */  pf$(2) = "           (8)Delete Record   (11)Print " &        ~
/*EWD002*/           "Rpt for Model/Lkup     (15)Print Screen"
            pf$(3) = "                              (12)Print " &        ~
                     "Rpt for ALL Models     (16)Save Data   "
/*EWD002*/  pfkeys$ = hex(01ffffffffffff08ff0a0b0cffff0f1000)
            if onfile% = 1% then L40690
                str(pf$(2),12,16) = " "  :  str(pfkeys$, 8,1) = hex(ff)
/*EWD002*/      str(pf$(1),31,28) = " "  :  str(pfkeys$,10,1) = hex(ff)
/*EWD002*/      str(pf$(2),31,28) = " "  :  str(pfkeys$,11,1) = hex(ff)
                str(pf$(3),31,28) = " "  :  str(pfkeys$,12,1) = hex(ff)
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
            on fieldnr% gosub L50010,        /* Model Number           */~
                              L50020,        /* Lookup Diameter Code   */~
                              L50030,        /* Max.Wdth for Winds/Coil*/~
                              L50040,        /* Max.Hght for Winds/Coil*/~
                              L50050,        /* Normal Winds/Coil & TD */~
                              L50060,        /* Dbl St Winds/Coil & TD */~
/*EWD002*/                    L50070,        /* Laminate Winds/Coil&TD */~
                              L50080,        /* ******** Winds/Coil&TD */~
                              L50090,        /* ******** Winds/Coil&TD */~
                              L50100         /* ******** Winds/Coil&TD */

            return

L50010: rem Enter Model Number                            model$/_cpy$
        if copy% <> 1% then goto L50015
            readkey$ = "MODEL    " & model_cpy$
            gosub test_model
            if errormsg$ <> " " then return
L50015: readkey$ = "MODEL    " & model$
        gosub test_model
        return

    test_model
        call "DESCRIBE" (#4, readkey$, model_d$, 0%, f1%(4))
        if f1%(4) = 0% then errormsg$ = "Invalid Model No."
        return

L50020: rem Enter Lookup Diameter Code                    lkupcd$/_cpy$
        if copy% <> 1% then goto L50025
            readkey$ = "PLAN DIAM" & lkupcd_cpy$
            gosub test_lookup
            if errormsg$ <> " " then return
L50025: readkey$ = "PLAN DIAM" & lkupcd$
        gosub test_lookup
        if copy% > 0% then goto copy_delt_recs
        return

    test_lookup
        call "DESCRIBE" (#4, readkey$, lkupcd_d$, 0%, f1%(4))
            if f1%(4) = 0% then errormsg$="Invalid Lookup Diameter Code"
        return

L50030: rem Enter Max. Width for Winds/Coil               width$
        convert width$ to w, data goto bad_width
        if w < 0 or w > 999.99 then goto bad_width
        convert w to width$, pic(000.0)
        return

    bad_width
        errormsg$ = "Invalid data for Maximum Width"
        return

L50040: rem Enter Max. Height for Winds/Coil              height$
        convert height$ to h, data goto bad_height
        if h < 0 or h > 999.99 then goto bad_height
        convert h to height$, pic(000.0)
        if edit% = 1% then gosub dataload
        return

    bad_height
        errormsg$ = "Invalid data for Maximum Height"
        return

L50050: rem Enter Normal Winds/Coil & Tube Diameter       windcl$(1)
        j% = 1%                                     /* tubedi$(1) */
        gosub test_diam
        return

L50060: rem Enter Dbl Strength Winds/Coil & Tube Diameter windcl$(2)
        j% = 2%                                     /* tubedi$(2) */
        gosub test_diam
        return

L50070: rem Enter Laminate Winds/Coil & Tube Diameter     windcl$(3)
        j% = 3%                                     /* tubedi$(3) */
        gosub test_diam
        return

L50080: rem Enter ******** Winds/Coil & Tube Diameter     windcl$(4)
        j% = 4%                                     /* tubedi$(4) */
        gosub test_diam
        return

L50090: rem Enter ******** Winds/Coil & Tube Diameter     windcl$(5)
        j% = 5%                                     /* tubedi$(5) */
        gosub test_diam
        return

L50100: rem Enter ******** Winds/Coil & Tube Diameter     windcl$(6)
        j% = 6%                                     /* tubedi$(6) */
        gosub test_diam
        return 



    test_diam
        if windcl$(j%) = " " then skip_test     /*EWD001*/
        readkey$ = "PLAN DIAM" & tubedi$(j%)
        call "DESCRIBE" (#4, readkey$, desc$, 0%, f1%(4))
            if f1%(4) = 0% then errormsg$="Invalid Tube Diameter Code"
        return

      skip_test                                 /* EWD001 - New */
        tubedi$(j%) = " "
        return


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

       confirm_delete
           comp% = 2%
           hdr$     = "******* WARNING WARNING WARNING  *******"
           msg$(1%) = " - - - - - - Confirm Deletion - - - - - - "
           msg$(2%) = "The above record will be deleted."
           msg$(3%) = "Press <PF16> to Continue, <PF1> to Abort."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
           if comp% = 16% then return
           if comp% <> 1% then goto confirm_delete
           return clear all  
           goto editpg1 


        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            end
            
