        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCSLS01                             *~
            *  Creation Date     - 01/16/95                             *~
            *  Last Modified Date- 11/11/97                             *~
            *  Description       - This Program Defines APC Salesman    *~
            *                      Commission Rates.                    *~
            *                      (1) APC Price Code(s) Assigned to a  *~
            *                          Salesman.                        *~
            *                      (2) APC Products Assigned to Price   *~
            *                          Code.                            *~
            *                      (3) Commission Rates Assigned to an  *~
            *                          APC Product.                     *~
            *                                                           *~
            *  Code Tables Used  - (XXXXX XXX) - Code Table             *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/15/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/11/97 ! Mod for New Release Upgrade to R6.04.03  ! RHH *~
	        * 03/19/98 ! Y2K modifications			              ! ERN *~
            *************************************************************
                    
        dim                                                              ~
            blankdate$6,                 /* PD null date               */~
            slsman$4, slsname$30,        /* Salesman Code and Name     */~
            prc_code$1, prc_desc$30,     /* Price Code and Description */~
            model$3, model_desc$30,      /* Model Code and Description */~
            c_comm$8,                    /* Current Commission Rate    */~
            p_date$10, p_dte$8,          /* Pending Commission Date    */~
            p_comm$8,                    /* Pending Commission Rate    */~
            sls_fil$2,                   /* SLS Free Area              */~
            sls_key$10, sls_rec$32,      /* Sales Comm. Primary Key    */~
            readkey$24, desc$32,         /* Gencodes Key and Descript  */~
            scr$(10%)40, s_mod$(15%)30,  /* Screen Text, Spec Model Txt*/~
            price$50,                    /* Store all Valid Price Codes*/~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            tempdate1$8,                 /* Temporary work var         */~
            userid$3, x$10               /* Current User Id            */~

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        dim bg_sls$4, ed_sls$4,          /* Report Salesman Codes      */~
            bg_name$30, ed_name$30,      /* Report Salesman Names      */~
            bg_prc$1, ed_prc$1,          /* Report Price Codes         */~
            bg_desc$30, ed_desc$30,      /* Report Price Descriptions  */~
            xtime$9, sav_key$8,          /* Report Run Time            */~
            title$40,                    /* Report Title               */~
            sav_slsman$4, sav_prc$1      /* Report Break Control       */

        dim fr_sls$4, to_sls$4,          /* Copy   Salesman Codes      */~
            fr_name$30, to_name$30,      /* Copy   Salesman Names      */~
            cp_prc$1,                    /* Copy   Price Codes         */~
            cp_desc$30,                  /* Copy   Price Descriptions  */~
            copy_key$8                   /* Copy Lookup Key            */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Salesman Commission Utility       "
            pname$ = "APCSLS01 - Rev: R6.04"

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
            * #1  ! SLMMASTR ! Salesman master file                     *~
            * #2  ! APCSLCOM ! Salesman Commission Percentage File      *~
            * #3  ! GENCODES ! System Code Table File                   *~
            * #5  ! APCSLCOM ! Same as (2). Use for Copying Data.       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4

            select #2,  "APCSLCOM",                                      ~
                        varc,     indexed,  recsize = 32,                ~
                        keypos = 1,    keylen = 8

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24

            select #5,  "APCSLCOM",                                      ~
                        varc,     indexed,  recsize = 32,                ~
                        keypos = 1,    keylen = 8

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),   0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%), 100%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),   0%, rslt$(3%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),   0%, rslt$(5%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATFMTC" (date$)
            call "DATUFMTC" (blankdate$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."


            scr$( 1%) = "****************************************"
            scr$( 2%) = "* Code    Description          Priority*"
            scr$( 3%) = "* -----   -------------------- ------- *"
            scr$( 4%) = "* (000) - All Models Code        (3)   *"
            scr$( 5%) = "* (?XX) - ? = Prod Line  (7XX)   (2)   *"
            scr$( 6%) = "* (YYY) - Specific Model (712)   (1)   *"
            scr$( 7%) = "*                                      *"
            scr$( 8%) = "****************************************"

            s_mod$(1%) = "(000) - All Models            "
            s_mod$(2%) = "(0XX) - All Aluminum Frame Scr"
            s_mod$(3%) = "(1XX) - All Storm Windows     "
            s_mod$(4%) = "(2XX) - All Storm Doors       "
            s_mod$(5%) = "(3XX) - All Patio Doors       "
            s_mod$(6%) = "(4XX) - All Parts             "
            s_mod$(7%) = "(5XX) - All Aluminum Prime    "
            s_mod$(8%) = "(6XX) - All Vinyl Windows     "
            s_mod$(9%) = "(7XX) - All Vinyl Prime Wind's"
            s_mod$(10%)= "(8XX) - All Casement Windows  "
            s_mod$(11%)= "(9XX) - ALL Bay Bow Windows   "

            price$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            fld% = 1%
            goto L10150
        inputmode_a
            init(" ") model$, model_desc$, c_comm$, p_comm$, p_date$
            p_dte$ = blankdate$
            c_comm, p_comm = 0.0

L10150:     for fieldnr% = fld% to  6%
L10160:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10310
L10180:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10260
L10210:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10180
                         if fieldnr% = 1% then L10160
                         goto L10210
L10260:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% = 14% and fieldnr% = 1% then inp_rpt
                      if keyhit% =  9% and fieldnr% = 1% then            ~
                                                          gosub copy_sls
                      if keyhit% <> 0% then       L10180
L10310:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10180
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
                  if keyhit%  = 12% then gosub delete_rate
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% >  6% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11180
                  lastfieldnr% = fieldnr%
            goto L11130

        REM *************************************************************~
            *       I N P U T   M O D E   R E P O R T                    ~
            *************************************************************

        inp_rpt
            gosub initialize_variables
            for fieldnr% = 1% to  4%
L12070:         gosub'061(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L12190
L12090:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L12170
L12120:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'061(fieldnr%)
                         if enabled% = 1% then L12090
                         if fieldnr% = 1% then L12070
                         goto L12120
L12170:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L12090
L12190:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12090
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   R E P O R T                    *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16  then gosub gen_rpt
                  if keyhit% <>  0% then       editpg2
L13100:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% >  4% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'061(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L13150:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13150
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13150
                  lastfieldnr% = fieldnr%
            goto L13100

        REM *************************************************************~
            *             P r o c e s s   D a t a                       *~
            *************************************************************

        gen_rpt
            gosub select_printer
            gosub generate_report
            close printer
        return clear all
        goto inputmode

        select_printer
            title$ = "* Salesman Commission Rate Definitions *"
            pageno% = 0%
            lcnt%   = 99%
            date$ = date
            call "DATFMTC" (date$)
            call "TIME" (xtime$)
            call "SETPRNT" (" ","SCOM",0%,0%)
            select printer(134)
        return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            return

        deffn'061(fieldnr%)
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
         "Enter a Valid Four (4) Digit Salesman Code?                  ",~
         "Enter a Valid (APC) Price Code ( A thru Z ) or ( 0 thru 9 )? ",~
         "Enter a Valid (APC) Product/Model Code? (000) = All Models?  ",~
         "Enter a Valid (APC) Commission Rate Assoc. with Product?     ",~
         "Enter a Pending (APC) Commission Rate Date? or None?         ",~
         "Enter a Valid (APC) Pending Commission Rate Assoc. with Prod?"

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28290
                inpmessage$ = edtmessage$
                return

L28290
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid Beginning Salesman Code or 'ALL'?              ",~
         "Enter a Valid Ending Salesman Code?                          ",~
         "Enter a Valid Beginning Price Code or '*' for ALL?           ",~
         "Enter a Valid Ending Price Code?                             "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      slsman$, slsname$, prc_code$, prc_desc$, model$,   ~
                      model_desc$, c_comm$, p_date$, p_comm$,            ~
                      sls_key$, readkey$, desc$, sls_fil$, bg_sls$,      ~
                      ed_sls$, bg_name$, ed_name$, bg_prc$, ed_prc$,     ~
                      bg_desc$, ed_desc$, xtime$, title$, fr_sls$,       ~
                      to_sls$, fr_name$, to_name$, cp_prc$, cp_desc$,    ~
                      copy_key$, sls_rec$, sav_slsman$, sav_prc$
            p_dte$ = blankdate$
            c_comm, p_comm = 0.0
            rec% = 0%
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

        dataload
            rec% = 0%
            sls_key$ = all(hex(00))
            str(sls_key$,1%,4%) = slsman$
            str(sls_key$,5%,1%) = prc_code$
            str(sls_key$,6%,3%) = model$
            read #2,hold,key = sls_key$, eod goto L30260
               get #2, using L35040, slsman$, prc_code$, model$,          ~
                                    c_comm, p_comm, p_dte$, sls_fil$
            convert c_comm to c_comm$, pic(##.####-)

            convert p_comm to p_comm$, pic(##.####-)

            p_date$ = p_dte$
            if p_dte$ = str(blankdate$,,6) then p_date$ = "**None**"      ~
                               else call "DATFMTC" (p_date$)
            gosub lookup_salesman
            gosub lookup_pricecode
            gosub lookup_model
        rec% = 1%
L30260: return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        delete_rate
        dataput
            call "SHOSTAT" ( "Updating Data for ("& slsman$ &")" )
            sls_key$ = all(hex(00))
            str(sls_key$,1%,4%) = slsman$
            str(sls_key$,5%,1%) = prc_code$
            str(sls_key$,6%,3%) = model$
            read #2,hold,key = sls_key$, eod goto L31170
               delete #2
            if keyhit% = 12% then L31260

L31170:     put #2, using L35040, slsman$, prc_code$, model$, c_comm,     ~
                                 p_comm, p_dte$, sls_fil$
            write #2, eod goto L31240
            if rec% = 1% then goto L31260
            fld% = 3%
        return clear all
        goto inputmode_a
L31240:     stop "(Error) - Updating Salesman -->  "&sls_key$
        return
L31260: return clear all
        goto inputmode

        REM *************************************************************~
            *            D a t a   F o r m a t                          *~
            *************************************************************

L35040:  FMT CH(4),                            /* Salesman Code        */~
             CH(1),                            /* APC Price Code       */~
             CH(3),                            /* APC Model Code       */~
             PD(14,4),                         /* Current Commission % */~
             PD(14,4),                         /* Pending Commission % */~
             CH(6),                            /* Effective Date for   */~
             CH(2)                             /*   Pending Commission */

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
              on fieldnr% gosub L40200,         /* Salesman Code     */   ~
                                L40200,         /* Price Code        */   ~
                                L40200,         /* Model Code        */   ~
                                L40210,         /* Current Commission*/   ~
                                L40180,         /* Pending Date      */   ~
                                L40190          /* Pending Comm Rate */
              goto L40230
L40180:
L40190:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40210:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,24), fac(hex(a4)), apc$                   , ch(38),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "(APC) Salesman Code     :",                  ~
               at (05,30), fac(lfac$(1%)), slsman$              , ch(04),~
               at (05,40), fac(hex(84)),   slsname$             , ch(30),~
                                                                         ~
               at (06,02), "(APC) Price Code (A-9)? :",                  ~
               at (06,30), fac(lfac$(2%)), prc_code$            , ch(01),~
               at (06,40), fac(hex(84)),   prc_desc$            , ch(30),~
                                                                         ~
               at (07,02), "(APC) Model/Product Code:",                  ~
               at (07,30), fac(lfac$(3%)), model$               , ch(03),~
               at (07,40), fac(hex(84)),   model_desc$          , ch(30),~
                                                                         ~
               at (08,02), "(APC) Assoc. Comm. Rate%:",                  ~
               at (08,30), fac(lfac$(4%)), c_comm$              , ch(08),~
                                                                         ~
               at (09,02), "(APC) Pending Comm Date :",                  ~
               at (09,30), fac(lfac$(5%)), p_date$              , ch(10),~
                                                                         ~
               at (10,02), "(APC) Pending Comm Rate%:",                  ~
               at (10,30), fac(lfac$(6%)), p_comm$              , ch(08),~
                                                                         ~
               at (12,21), fac(hex(84)),   scr$( 1%)            , ch(40),~
               at (13,21), fac(hex(84)),   scr$( 2%)            , ch(40),~
               at (14,21), fac(hex(84)),   scr$( 3%)            , ch(40),~
               at (15,21), fac(hex(84)),   scr$( 4%)            , ch(40),~
               at (16,21), fac(hex(84)),   scr$( 5%)            , ch(40),~
               at (17,21), fac(hex(84)),   scr$( 6%)            , ch(40),~
               at (18,21), fac(hex(84)),   scr$( 7%)            , ch(40),~
               at (19,21), fac(hex(84)),   scr$( 8%)            , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then L40710
                  call "PRNTSCRN" : goto L40230

L40710:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf1
        if edit% = 2% then L40920     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Report      "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (9)Copy Data           " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffff09ffffffff0e0f1000)
            if fieldnr% = 1% then L40880
               str(pf$(3%),18%,20%) = " " : str(pfkeys$,9%,1%) = hex(ff)
               str(pf$(1%),64%) = " " : str(pfkeys$,14%,1%) = hex(ff)
               str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40880:     if fieldnr% > 2% then L40900
               str(pf$(2%),18%,20%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40900:     return

L40920: if fieldnr% > 0% then L41030  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (12)Delete Rate        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffff0cffff0f1000)
            if rec% <> 0% then goto L41020
               str(pf$(2%),18%,20%) = " " : str(pfkeys$,12%,1%) = hex(ff)
L41020:     return
L41030:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               R E P O R T   S C R E E N                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42200,         /* Beg Sales Code    */   ~
                                L42200,         /* End Sales Code    */   ~
                                L42200,         /* Beg Price Code    */   ~
                                L42200          /* End Price Code    */
              goto L42230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Beg Salesman Code :",                        ~
               at (05,30), fac(lfac$(1%)), bg_sls$              , ch(04),~
               at (05,40), fac(hex(84)),   bg_name$             , ch(30),~
                                                                         ~
               at (06,02), "End Salesman Code :",                        ~
               at (06,30), fac(lfac$(2%)), ed_sls$              , ch(04),~
               at (06,40), fac(hex(84)),   ed_name$             , ch(30),~
                                                                         ~
               at (07,02), "Beg Price Code    :",                        ~
               at (07,30), fac(lfac$(3%)), bg_prc$              , ch(01),~
               at (07,40), fac(hex(84)),   bg_desc$             , ch(30),~
                                                                         ~
               at (08,02), "End Price Code    :",                        ~
               at (08,30), fac(lfac$(4%)), ed_prc$              , ch(01),~
               at (08,40), fac(hex(84)),   ed_desc$             , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then L42570
                  call "PRNTSCRN" : goto L42230

L42570:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf2
        if edit% = 2% then L42760     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L42720
               str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L42720:     if fieldnr% > 2% then L42740
               str(pf$(2%),18%,20%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L42740:     return

L42760: if fieldnr% > 0% then L42850  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return
L42850:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *       S a l e s m a n   C o p y   S c r e e n             *~
            *************************************************************

        deffn'103(fr_sls$,to_sls$,cp_prc$)
              inpmessage$ =                                              ~
           "Enter a Valid Copy 'From' and 'To' Salesman and Pricecode?"
              gosub set_pf3

L44090:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Copy From Salesman:",                        ~
               at (05,30), fac(hex(81)),   fr_sls$              , ch(04),~
               at (05,40), fac(hex(84)),   fr_name$             , ch(30),~
                                                                         ~
               at (06,02), "Copy To Salesman  :",                        ~
               at (06,30), fac(hex(81)),   to_sls$              , ch(04),~
               at (06,40), fac(hex(84)),   to_name$             , ch(30),~
                                                                         ~
               at (07,02), "Copy Price Code   :",                        ~
               at (07,30), fac(hex(81)),   cp_prc$              , ch(01),~
               at (07,40), fac(hex(84)),   cp_desc$             , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then L44390
                  call "PRNTSCRN" : goto L44090

L44390:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf3
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Copy Data   "
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            if copy% = 1% then L44530
               str(pf$(1%),64%) = " " : str(pfkeys$,14%,1%) = hex(ff)
L44530: return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50160,         /* Salesman               */~
                              L50280,         /* Price Code             */~
                              L50400,         /* Model/Product Code     */~
                              L50530,         /* Current Commission     */~
                              L50630,         /* Pending Commission Date*/~
                              L50800          /* Pending Commission Rate*/
            return

L50160: REM Test for Salesman                     SLSMAN$
            if slsman$ <> " " then goto L50210
               desc$ = hex(06) & "Select a Valid Salesman Code"
               call "PLOWCODE" (#1, slsman$, desc$, 0%, .30, f1%(1))
               if f1%(1%) = 0 then goto L50240
L50210:     gosub lookup_salesman
            if len(slsname$) < 2 then goto L50240
        return
L50240:     errormsg$ = "(Error) - Salesman Not on File or Invalid?"
            init(" ") slsman$, slsname$
        return

L50280: REM Test for Price Code                   PRC_CODE$
            p = pos(price$ = prc_code$)
            if p <> 0 then goto L50320
               goto L50350
L50320:     gosub lookup_pricecode
            if len(prc_desc$) < 2 then goto L50350
        return
L50350:     errormsg$ = "Invalid Price Code - Code Must be 'A' - 'Z'" &  ~
                           " or '0' - '9'"
            init(" ") prc_code$, prc_desc$
        return

L50400: REM Test for Valid Model Code             MODEL$
            if model$ <> " " then goto L50430
               model$ = "000"
L50430:     gosub lookup_model
            if len(model_desc$) < 2 then goto L50490
            if edit% <> 1% then return
               gosub dataload
               if rec% = 1% then fieldnr% = 6%
        return
L50490:     errormsg$ = "(Error) - Invalid Model Code?"
            init(" ") model$, model_desc$
        return

L50530: REM Test for Current Commission %           C_COMM$
            convert c_comm$ to c_comm, data goto L50590

            convert c_comm to c_comm$, pic(##.####-)

        return
L50590:     errormsg$ = "(Error) Invalid Commission Rate Entered?"
            c_comm$ = " " : c_comm = 0.0
        return

L50630: REM Test for Pending Commission Date      P_DATE$
            if len(p_date$) < 2 then goto L50730
               if str(p_date$,1%,1%) = "*" then goto L50730
                call "DATEOKC" (p_date$, date%, errormsg$)
                if date% = 0% then goto L50720
                    call "DATUFMTC" (p_date$)   /* CCYYMMDD     */
                    p_dte$ = p_date$
                    call "DATECONV" (p_dte$)    /* PD           */
                    call "DATFMTC" (p_date$)    /* MM/DD/CCYY   */
L50720:     return

L50730:     p_date$ = "**None**"
            p_dte$ = blankdate$
            p_comm$ = "0.0" : p_comm = 0.0
            if edit% <> 1% then return
               fieldnr% = 6%
            return

L50800: REM Test for Pending Commission Rate      P_COMM$
            if p_dte$ = blankdate$ then p_comm$ = "0.0"
               convert p_comm$ to p_comm, data goto L50880

               convert p_comm to p_comm$, pic(##.####-)

            if p_dte$ <> blankdate$ and p_comm = 0 then goto L50880
        return
L50880:      errormsg$ = "(Error) - Invalid Pending Commission Rate?"
             init(" ") p_comm$
        return

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51000,         /* Beg Salesman Code      */~
                              L51140,         /* End Salesman Code      */~
                              L51300,         /* Beg Price Code         */~
                              L51490          /* End Price Code         */
            return

L51000: REM Beginning Salesman Code               BG_SLS$
            if bg_sls$ <> " " then goto L51050
               bg_sls$ = "ALL "
               bg_name$ = "(ALL) SALESMAN "
               return
L51050:     slsman$ = bg_sls$ : slsname$ = " "
            gosub lookup_salesman
            if len(slsname$) < 2 then goto L51100
            bg_name$ = slsname$
        return
L51100:     errormsg$ = "(Error) - Invalid Beginning Salesman Code?"
            init(" ") bg_sls$, bg_name$, slsman$, slsname$
        return

L51140: REM Ending Salesman Code                  ED_SLS$
            if ed_sls$ <> " " then goto L51190
L51160:        ed_sls$  = bg_sls$
               ed_name$ = bg_name$
               return
L51190:     if bg_sls$ = "ALL " then goto L51160
               slsman$ = ed_sls$ : slsname$ = " "
               gosub lookup_salesman
               if len(slsname$) < 2 then goto L51260
               ed_name$ = slsname$
               if bg_sls$ > ed_sls$ then goto L51260
        return
L51260:     errormsg$ = "(Error) - Invalid Ending Salesman Code?"
            init(" ") ed_sls$, ed_name$, slsman$, slsname$
        return

L51300: REM Beginning Price Code                  BG_PRC$
            if bg_prc$ <> "*" and bg_prc$ <> " " then goto L51360
               bg_prc$  = "*"
               bg_desc$ = "(All) Valid Price Codes"
               return

L51360:     p = pos(price$ = bg_prc$)
            if p <> 0 then goto L51390
               goto L51440
L51390:     prc_code$ = bg_prc$ : prc_desc$ = " "
            gosub lookup_pricecode
            if len(prc_desc$) < 2 then goto L51440
            bg_desc$ = prc_desc$
        return
L51440:     errormsg$ = "Invalid Price Code - Code Must be 'A' - 'Z'" &  ~
                           " or '0' - '9'"
            init(" ") prc_code$, prc_desc$, bg_prc$, bg_desc$
        return

L51490: REM Ending Price Code                     ED_PRC$
            if bg_prc$ <> "*" and ed_prc$ <> " " then goto L51550
               ed_prc$  = bg_prc$
               ed_desc$ = bg_desc$
               return

L51550:     p = pos(price$ = ed_prc$)
            if p <> 0 then goto L51580
               goto L51640
L51580:     prc_code$ = ed_prc$ : prc_desc$ = " "
            gosub lookup_pricecode
            if len(prc_desc$) < 2 then goto L51640
            ed_desc$ = prc_desc$
            if bg_prc$ > ed_prc$ then goto L51640
        return
L51640:     errormsg$ = "Invalid Price Code - Code Must be 'A' - 'Z'" &  ~
                           " or '0' - '9'"
            init(" ") prc_code$, prc_desc$, ed_prc$, ed_desc$
        return

        REM *************************************************************~
            *      R e p o r t   F o r m a t   S t a t e m e n t s      *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+
L55060: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!
                                                      /* Header Format */
L55090: %!                    ########################################   ~
        ~                                                                !
L55110: %!########## @ ########                                          ~
        ~                                                      Page: ### !
                                                      /* Detail Header */
L55140: %!Slsm!<---------- Name ------------>!Prc!<--- Description -->!Mo~
        ~d!<--------- Description -------! Curr Rate! Pend Date!Pend Rate!

L55170: %!####!##############################! # !####################!##~
        ~#!##############################! ######## !##########! ########!

L55200: %!----!------------------------------!---!--------------------!--~
        ~-!------------------------------!----------!----------!---------!

        REM *************************************************************~
            *            S p e c i a l   S u b r o u t i n e s          *~
            *************************************************************

        prt_header
            if lcnt% <> 99% then print using L55040
            pageno% = pageno% + 1%
            print page
            print using L55040
            print using L55090, title$
            print using L55110, date$, xtime$, pageno%
            print using L55060
            print using L55140
            lcnt% = 5%
        return

        print_detail
            if lcnt% > 57% then gosub prt_header
            print using L55200
            if sav_slsman$ = slsman$ then goto L60260
               sav_slsman$ = slsman$
               sav_prc$    = prc_code$
               print using L55170, slsman$, slsname$, prc_code$,          ~
                  str(prc_desc$,1%,20%), model$, model_desc$, c_comm$,   ~
                  p_date$, p_comm$
               goto L60360

L60260:     if sav_prc$ = prc_code$ then goto L60330
               sav_prc$ = prc_code$
               print using L55170, " ", " ", prc_code$,                   ~
                  str(prc_desc$,1%,20%), model$, model_desc$, c_comm$,   ~
                  p_date$, p_comm$
               goto L60360

L60330:        print using L55170, " ", " ", " ", " ",                    ~
                         model$, model_desc$, c_comm$, p_date$, p_comm$

L60360:     lcnt% = lcnt% + 2%
        return

        lookup_salesman
            read #1,key = slsman$, using L60410, slsname$, eod goto L60430
L60410:        FMT POS(5), CH(30)
        return
L60430:     slsname$ = " "
        return

        lookup_pricecode
            readkey$ = " "
            str(readkey$,1%,9%)   = "PRICECODE"
            str(readkey$,10%,15%) = prc_code$
            read #3,key = readkey$, using L60510, prc_desc$,eod goto L60530
L60510:        FMT POS(25), CH(30)
        return
L60530:     prc_desc$ = " "
        return

        lookup_model
            if model$ <> "000" then goto L60600
               model_desc$ = s_mod$(1%)
               return
L60600:     if str(model$,2%,2%) <> "XX" then goto L60650
               convert str(model$,1%,1%) to x%, data goto L60720

               model_desc$ = s_mod$(x%+2%)
               return
L60650:     readkey$ = " "
            str(readkey$,1%,9%)  = "MODEL    "
            str(readkey$,10%,15%)= model$
            read #3,key = readkey$, using L60700, model_desc$,            ~
                                                 eod goto L60720
L60700:        FMT POS(25), CH(30)
        return
L60720:     model_desc$ = " "
        return

        generate_report
            call "SHOSTAT" ("Creating Salesman Report")
            init(" ") sav_key$, sav_slsman$, sav_prc$
            if bg_sls$ = "ALL " then goto generate_next
               str(sav_key$,1%,4%) = bg_sls$
        generate_next
            read #2,key > sav_key$, using L60830, sav_key$,               ~
                                                   eod goto generate_done
L60830:        FMT CH(8)
            slsman$   = str(sav_key$,1%,4%)
            prc_code$ = str(sav_key$,5%,1%)
            model$    = str(sav_key$,6%,3%)
            if bg_sls$ = "ALL " then goto L60890
               if str(sav_key$,1%,4%) > ed_sls$ then goto generate_done
L60890:     if bg_prc$ = "*" then goto L60920
               if str(sav_key$,5%,1%) < bg_prc$ then goto generate_next
               if str(sav_key$,5%,1%) > ed_prc$ then goto generate_next
L60920:     gosub dataload
            gosub print_detail
            goto generate_next
        generate_done
            print using L55040

        return

        copy_sls
            copy% = 0%
L61020:     gosub'103(fr_sls$,to_sls$,cp_prc$)
            if keyhit% = 14% then goto copy_utility
            if keyhit% = 16% then goto exit_program
            if keyhit% = 1%  then gosub startover
            slsman$ = fr_sls$
            gosub lookup_salesman
            if len(slsname$) < 2 then goto L61200
            fr_name$ = slsname$
            slsman$ = to_sls$
            gosub lookup_salesman
            if len(slsname$) < 2 then goto L61230
            to_name$ = slsname$
            prc_code$ = cp_prc$
            gosub lookup_pricecode
            if len(prc_desc$) < 2 then goto L61260
            cp_desc$ = prc_desc$
            copy% = 1%
            goto L61020
L61200:  errormsg$ = "(Error) - Invalid Copy 'From' Salesman Code?   "
         goto copy_sls

L61230:  errormsg$ = "(Error) - Invalid Copy 'To' Salesman Code?     "
         goto copy_sls

L61260:  errormsg$ = "(Error) - Invalid (APC) Price Code?            "
         goto copy_sls

        copy_utility
            init(" ") sls_key$
            str(sls_key$,1%,4%) = fr_sls$
            str(sls_key$,5%,1%) = cp_prc$
            sav_key$ = sls_key$
        copy_next
             call "SHOSTAT" ( "Copying Data for ("& fr_sls$ &")" )
             read #2,hold,key > sls_key$, using L61380, sls_rec$,         ~
                                                       eod goto copy_done
L61380:         FMT CH(32)
             sls_key$ = str(sls_rec$,1%,8%)
             if str(sav_key$,1%,5%) <> str(sls_key$,1%,5%) then          ~
                                                           goto copy_done
                str(sls_rec$,1%,4%) = to_sls$
                copy_key$ = str(sls_rec$,1%,8%)
                read #5,hold,key = copy_key$, eod goto L61460
                   delete #5
L61460:         put #5, using L61380, sls_rec$
                write #5, eod goto L61530
                goto copy_next
        copy_done
        return clear all
        goto inputmode

L61530:      stop "(Error) - Writing Data for -->  "&sls_key$
          goto copy_next

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

        end
