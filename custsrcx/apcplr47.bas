REM         *************************************************************~
            *  Program Name      - APCPLR47                             *~
            *  Creation Date     - 07/24/2008                           *~
            *  Last Modified Date- 07/24/2008                           *~
            *  Written By        - David Speight   	                *~
            *  Modified From     - APCPLN47                             *~
            *                                                           *~
            *  Description       - This Program Creates a Screen        *~
            *                      Audit Report.                        *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *07/24/2008! New program, copied from apcpln47 for    ! DES *~
            *          ! New Report (from new file scrnaudt)      !     *~
            *************************************************************

        dim                              /* FILE = AMTBOMCD            */~
            ht2$9,                       /* Center Bar Location        */~
            phantom$25                   /* Phantom Designator         */

	    dim scrn_rec$160,scrn_key$16 /* AWD013 */
	    dim temp1$64, temp2$61       /* AWD013 */

        dim                              /* FILE = APCPLNDT            */~
            dt_rec$256,                  /* Detail Record              */~
            dt_key1$57,                  /* Alt - Bar Code Key         */~
            dt_dept$3,                   /* Department Code            */~
            dt_proc$2,                   /* proc                       */~
            dt_date$8,                   /* date                       */~
            dt_barcode$18,               /* barcode                    */~
            dt_load$5,                   /* Load Number                */~
            dt_part$25, dt_wood$3,       /* Prod/Comp Seq. (0) or (1)  */~
            dt_desc$30, dt_so$8,         /* Prod/Comp Date (0) or (1)  */~
            dt_seq$5, dt_index$30,       /* Production Sort Index      */~
            dt_shft$2, scr_shft$2,       /* DESCRIPTION                */~
            dt_special$10,               /* S.O. Special Flags         */~
            dt_ref$8,                    /* REF / WARRANTY             */~
            dt_samp$1,                   /* 0=NO, 1=SAMP, 2=DISP       */~
            scr_shft_d$30                /*                            */

        dim                              /* FILE - (APCPLNW1)          */~
            wd$7,                        /* Actual Width               */~
            ht$6,                        /* Actual Height              */~
            scr$(10)60,                  /* Actual Height              */~
            txt_flag$1,                  /* Special Text Exists        */~
            report_date$8,               /* Report Date                */~
            rpt_dte$10,                  /* Report Date                */~
            sav_part$25                  /* Save Part Number           */


        dim                              /* (Program Variables)        */~
            wt$3,                        /* Window Type Code           */~
            hdr$40, msg$(3%)79,          /* ASKUSER TEXT               */~
            qty$5,                       /* QUANTITY                   */~
            grid$25,                     /* Description of Grid        */~
            screen_dte$8,                /* Screen Comp Date Formated  */~
            screen_dte1$8,               /* Screen Comp Date Unformated*/~
            sze$(10%)3,                  /* Save Eights                */~
            sze1$(20%)5,                 /* Save Sixteenths            */~
            wd1$9,                       /* Calculated Width           */~
            wd2$9,                       /* CLMR FOR SCREEN            */~
            ht1$8,                       /* Calculated Height          */~
            sav_wd1$9,                   /* Calculated Width     EWD009*/~
            sav_ht1$8,                   /* CLMR FOR SCREEN      EWD009*/~
            readkey$30,                  /* GENCODES Look-Up Key       */~
            descr$30,   stk_so$8,        /* Use for GENCODES Look-Up   */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            rpt_time$8,                  /* Report Time                */~
            title$25,                    /* Report Title               */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3,                    /* Current User Id            */~
/*(EWD002)*/script$8,                    /* Script to send file to Bayfm*/~
/*(EWD005)*/script_rte$8                 /* Script to send file to RteSn*/

        dim f2%(20%),                    /* = 0 if the file is open    */~
            f1%(20%),                    /* = 1 if READ was successful */~
            fs%(20%),                    /* = 1 if file open, -1 if it */~
            rslt$(20%)20                 /* Text from file opening     */

        dim                              /* FILE - (TEXTFILE)          */~
            textid$4                     /* S.O. TEXTID$               */

        dim                              /* FILE - (NEW LABEL VARS)    */~
            model$3,                     /* MODEL CODE                 */~
            cl$1, cl_l$2, color$6,       /* COLOR CODE                 */~
            gl$2,                        /* GLASS CODE                 */~
            lt$2, co_or$8,               /* LITING CODE                */~
            hg$2, hnge$20, hg_l$8, hh$8,  /* HINGE CODE                 */~
            sc$1, sc_l$4, sc_r$20,       /* SCREEN CODES               */~
            lk$1,                        /* LOCK CODES                 */~
            width$4,                     /* WIDTH                      */~
            height$3,                    /* HEIGHT                     */~
            clmr$3,                      /* CENTER LINE MEETING RAIL   */~
            wallw$3                      /* WALL WIDTH                 */

        dim                              /* FILE - Screen Explosion    */~
            cut$(4%)62,                  /* Max (4) Screens            */~
            txt$(3%)50                   /* Screen '102 Header Text    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$41, pname$21
            apc$   = "(New)Planning Screen Processing Utility  "
            pname$ = "APCPLN47 - Rev: 06.04"

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
            * #3 ! GENCODES ! Master System Table File                  *~
            * #18! SCRNAUDT ! Screen Audit File (AWD013)                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


            select #18,  "SCRNAUDT",                                     ~
                        varc,     indexed,  recsize = 160,               ~
                        keypos =    1, keylen =   16,                    ~
                        alt key  1, keypos =   17, keylen =  36, dup,    ~
                            key  2, keypos  =  40, keylen =  13, dup,    ~
                            key  3, keypos  =   9, keylen =   8, dup      
/* @@@@@
key         field    column
1           date     1-8          ch(8)
1 4         dept     9-11         ch(3) 
1 4         seq     12-16         ch(5) 
2           barcode 17-34         ch(18)
2           dept2   35-37         ch(3) 
2           proc    38-39         ch(2)
2 3         S.O.    40-47         ch(8)
2 3         S.O. line # 48-53     ch(2)
            model$  53-55         ch(3)
            cl_l    56-57         ch(2)         
            sc_l    58-61         ch(4)
            wd1     62-70         ch(9)
	    ht1     71-78         ch(8)
            cb      79-87         ch(9)
	    wd      88-94         ch(7)
	    ht      95-100        ch(6)
            received 101-101      ch(1) N/Y
            in house 102-102      ch(1) I/B/R
            filler 103-128        ch(26)
*/
            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#3,  fs%(3%),  f2%(3%),   0%, rslt$(3%))
            call "OPENCHCK" (#18, fs%(18%), f2%(18%), 500%, rslt$(18%))

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

            init(" ") scr$()
REM         scr$( 1%) = "****************************************"
REM         scr$( 2%) = "*        Consolidated Screen           *"
REM         scr$( 3%) = "*                                      *"
REM         scr$( 4%) = "* (1) - Screen Audit Report            *"
REM         scr$( 5%) = "* (2) -                                *"
REM         scr$( 6%) = "* (3) -                                *"
REM         scr$( 7%) = "*                                      *"
REM         scr$( 8%) = "*                                      *"
REM         scr$( 9%) = "****************************************"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 2% to  3%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(2%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 2% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% < 3% then exit_program
                      if keyhit% <> 0% then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%
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
                  if keyhit%  = 16% then gosub begin_process
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 2% or fieldnr% > 3% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 1%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *             P R O C E S S   D A T A                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        begin_process

L19110:      call "SHOSTAT" ("Creating "& scr_msg$)
        init(" ") scrn_key$
        init(" ") date$, rpt_time$, title$
        lcnt% = 99%   :  pageno% = 0%
        date$ = date  :  call "DATEFMT" (date$)
        call "TIME" (rpt_time$)
        call "SETPRNT" (" ","SC02", 0%, 0%)
        select printer (134)
        call "COMPNAME" (12%, title$, 0%)
        call "SHOSTAT" ("Printing Center Bar Report")
        str(scrn_key$,1,8) = report_date$
        read #18,key > scrn_key$, using SCRNAUDT, scrn_rec$, eod goto end_rpt
	goto process_record

read_next:
        read #18,key > scrn_key$, using SCRNAUDT, scrn_rec$, eod goto end_rpt
SCRNAUDT: FMT CH(160)

process_record:
        dept$ = str(scrn_rec$,09,3)
        scrn_key$ = str(scrn_rec$,1,16)
        if str(scrn_rec$,1,8) > report_date$ then goto end_rpt
	if (scr_dept$ <> "ALL" and scr_dept$ <> dept$) then goto read_next 
        seq$  = str(scrn_rec$,12,5)
        cl_l$ = str(scrn_rec$,56,2)
        wd1$  = str(scrn_rec$,62,9)
        ht1$  = str(scrn_rec$,71,8)
        sc_l$ = str(scrn_rec$,58,2)
        wd2$  = str(scrn_rec$,79,9)
        sc$   = str(scrn_rec$,126,1)
        hg$   = str(scrn_rec$,124,2)
	part$ = str(scrn_rec$,116,25)
        wd$   = str(scrn_rec$,88,7)
        ht$   = str(scrn_rec$,95,6)
        ht2$  = str(scrn_rec$,103,9)
        hg$   = str(scrn_rec$,125,2)
        model$ = str(scrn_rec$,53,3)
        init(" ") grid$                      /* Set Center Bar Loc */
        grid$ = "      *** **/**        "  /* APC MOD - 08/26/97 */
        if len(ht2$) > 1 then str(grid$,7%,9%) = ht2$
        gosub lookup_screen
        gosub lookup_hinge 
        gosub print_detail
	goto read_next

end_rpt:
        return clear all
        goto inputmode

        lookup_hinge                                  /* Look Up Hinge */
            triple% = 0%
            init(" ") readkey$, descr$, hnge$, hg_l$, hh$
            str(readkey$,1%,9%)   = "HINGE    "
            str(readkey$,10%,15%) = hg$
            read #3,key = readkey$, using L60755, descr$, eod goto L60865
L60755: FMT POS(25), CH(30)
            p% = pos(descr$ = "-")
            hnge$ = str(descr$,p%+2%)
            if str(descr$,p%+2%,2%) = "  " then                          ~
                    hnge$ = str(descr$,1%,p%-1%)
            hg_l$ = str(descr$,1%,p%-2%)
            if str(descr$,p%-4%,3%) = "1/3" then triple% = 1%
            if str(hg_l$,1%,2%) = "CO" then hh$ = "COTTAGE"
            if str(hg_l$,1%,2%) = "OR" then hh$ = "ORIEL  "
        return
L60865:     hnge$ = "(Error) - Invalid Hinge Code?"
        return

        lookup_screen                                /* Look Up SCREEN */
            init(" ") readkey$, descr$, sc_l$, sc_r$
            str(readkey$,1%,9%)   = "SCREEN   "
            str(readkey$,10%,15%) = sc$
            read #3,key = readkey$, using L60755, descr$, eod goto L60925
            p% = pos(descr$ = "-")
            sc_r$ = str(descr$,1%,20%)
            sc_l$ = str(descr$,1%,p%-2%)
        return
L60925:     sc_r$ = "(error) - Invalid Screen Code?"
        return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return

        deffn'052(fieldnr%)
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
         "Enter (1)Calculate Screen Sizes, (2)Screen Report/Labels.    ",~
         "Enter the Planned Production Date Associated with Screens.   ",~
         "Enter a Valid Dept Code or All.                              "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28280
                inpmessage$ = edtmessage$
                return

L28280
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid Stock or Manufactured Part Number?              "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, scr_code$, scr_msg$,       ~
                      scr_dte$, scr_dte1$, scr_dept$, scr_msg1$,         ~
                      screen_dte$, screen_dte1$, scr_load$, scr_desc$,   ~
                      dt_part$, dt_desc$, scr_shft$, scr_shft_d$, cut$(),~
                      dt_wood$
            txt$(1) =                                                    ~
                "**************************************************"
            txt$(2) =                                                    ~
                "* (Calculate) Screen Size for Manufactured Parts *"
            txt$(3) =                                                    ~
                "**************************************************"
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
        REM DATALOAD
        REM RETURN

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Update Store Data and Part Data                           *~
            *************************************************************

        REM DATAPUT
        REM RETURN

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
              on fieldnr% gosub L40210,         /* Screen's Selection*/   ~
                                L40200,         /* Production Date   */   ~
                                L40200          /* Department Code   */   

              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40210:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

REM line 6 removed for now, if needed copy from apcpln47.bas
L40230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(08),~
               at (02,20), fac(hex(a4)), apc$                   , ch(41),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (07,02), "Planned Production Date :",                  ~
               at (07,30), fac(lfac$(2%)), scr_dte$             , ch(08),~
               at (07,42), "MMDDYYYY                 ",                  ~
               at (08,02), "Department Code, or All :",                  ~
               at (08,30), fac(lfac$(3%)), scr_dept$            , ch(03),~
               at (08,40), fac(hex(84)), scr_msg1$              , ch(30),~
                                                                         ~
               at (12,21), fac(hex(84)), scr$(1%)               , ch(60),~
               at (13,21), fac(hex(84)), scr$(2%)               , ch(60),~
               at (14,21), fac(hex(84)), scr$(3%)               , ch(60),~
               at (15,21), fac(hex(84)), scr$(4%)               , ch(60),~
               at (16,21), fac(hex(84)), scr$(5%)               , ch(60),~
               at (17,21), fac(hex(84)), scr$(6%)               , ch(60),~
               at (18,21), fac(hex(84)), scr$(7%)               , ch(60),~
               at (19,21), fac(hex(84)), scr$(8%)               , ch(60),~
               at (20,21), fac(hex(84)), scr$(9%)               , ch(60),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9% then goto L40600
                  gosub display_departments
                  goto L40230

L40600:        if keyhit% <> 15 then goto L40640
                  call "PRNTSCRN"
                  goto L40230

L40640:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
           l_txt$ = "Production Load or Blank:"
           if  scr_code% = 3% then l_txt$ = "Schedule No. for Lineal :"

        if edit% = 2% then L40830     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                 (9)Display Dept's      " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffff09ffffffffff0f1000)
L40790:     if fieldnr% > 2% then L40810
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40810:     return

L40830: if fieldnr% > 0% then L40920  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                 (9)Display Dept's      " &       ~
                      "                       (16)Print Data  "
            pfkeys$ = hex(01ffffffffffffff09ffffffffff0f1000)
            return
L40920:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                 (9)Display Dept's      " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffff09ffffffffffffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50150,         /* Screen's Selection    */ ~
                              L50340,         /* Production Date       */ ~
                              L50550          /* Department Code       */ 
            return

L50150: REM Screen's Selection                    SCR_CODE$
            scr_code% = 0%
            if scr_code$ <> " " then goto L50190
               scr_code$ = "1"
L50190:     convert scr_code$ to scr_code%, data goto L50290

            if scr_code% < 1% or scr_code% > 3% then goto L50290
            scr_msg$ = str(scr$(scr_code% + 3%),9%,30%)
        return
L50290:     errormsg$ = "(Error) - Invalid Screen's Selection?"
            gosub error_prompt
            init(" ") scr_code$, scr_msg$
        return

L50340:         /* Production Date  MMDDYYYY     */ 
            if len(scr_dte$) = 7 then scr_dte$ = "0" & scr_dte$
            if len(scr_dte$) <> 8 then goto L50350
            convert str(scr_dte$,1,2) to mm%, data goto L50350
            convert str(scr_dte$,3,2) to dd%, data goto L50350
            convert str(scr_dte$,5,4) to yyyy%, data goto L50350
            if mm% < 01% or mm% > 12% then goto L50350
            if dd% < 01% or dd% > 31% then goto L50350
            if yyyy% < 2005% or yyyy% > 2099% then goto L50350
            if mm% = 2% and dd% > 29% then goto L50350
	    /* need leap year check for feb 29 */
            if mm% = 1% or mm% = 3% or mm% = 5% or mm% = 7% or mm% = 8% ~
	    or mm% =  10% or mm% = 12% then goto L50345
            if dd% > 30% then goto L50350
L50345:
	    report_date$ = "yyyymmdd"
	    convert yyyy% to str(report_date$,1,4), pic (0000)
	    convert mm%   to str(report_date$,5,2), pic (00)
	    convert dd%   to str(report_date$,7,2), pic (00)
	    rpt_dte$ = str(report_date$,5,2) & "/" &                ~
	               str(report_date$,7,2) & "/" &                ~
	               str(report_date$,1,4)

       return
L50350:     errormsg$ = "(Error) - Invalid Date Entered!      "
            gosub error_prompt
            init(" ") scr_code$, scr_msg$
        return

L50550:         /* Department Code       */ 
       if scr_dept$ = "   " then scr_dept$ = "ALL"
       if scr_dept$ <> "ALL" then gosub lookup_dept
       return

        display_departments
           readkey$ = " "
           str(readkey$,1%,9%) = "PLAN DEPT"
           descr$ =hex(06) & "Dept Batch/Sort/Descr Info"
           call "PLOWCODE" (#3, readkey$, descr$, 9%, .30, f1%(2%))
        return

        lookup_dept
           dept% = 0%
           init(" ") readkey$, descr$
           if scr_dept$ = "066" then return      /* (EWD001)        */
                                                 /* Skip Literature */     

           str(readkey$,1%,9%) = "PLAN DEPT"
           str(readkey$,10%,15%) = scr_dept$
           read #3,key = readkey$, using L51910, descr$
L51910:       FMT POS(25), CH(30)
           scr_msg1$ = descr$                    
           dept% = 1%
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                      /* Report Header */
L55050: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~-+

L55090: %!########@########                                  ############~
        ~#############                                           Page: ###~
        ~ !

L55130: %! Production Date: ##########                      Screen Consol~
        ~idation Report                               Department Code: ###~
        ~ !

        
L55210: %!                                                               ~
        ~                                                                 ~
        ~ !

L55250: %!---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~-!
                                                              /* Screen*/
L55290: %!Dpt! Seq !Mdl!Cl!  Width       Height !<----- Type ------->!Cb Cut ~
        ~Ln!<Center Bar Cut Loc.>!<------ Hinge ----->!  Window Size  ~
        ~ !
                                                              /* SCREEN*/
L55330: %!---!-----!---!--!---------------------!--------------------!-------~
        ~--!---------------------!--------------------!---------------~
        ~-!
                                                              /* SCREEN*/
L55370: %!###!#####!###!##!######### BY ########!####################!#######~
        ~##!#####################!####################!####### X #####~
        ~#!
                                                              /* Totals*/
        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_header                         /* GENERIC REPORT HEADING */
	  if dept$ <> hold_dept$ then lcnt% = 99%
          if lcnt% <> 99% then print using L55050
	  hold_dept$ = dept$
          pageno% = pageno% + 1%
          print page
          print using L55050
          print using L55090, date$, rpt_time$, title$, pageno%
          print using L55130, rpt_dte$, dept$   
          print using L55250
          print using L55290
          lcnt% = 5%
        return

        print_detail
          if lcnt% > 60% then gosub print_header

             print using L55330
             print using L55370, dept$, seq$, model$, cl_l$, wd1$, ht1$, ~
                             sc_r$, wd2$, grid$, hnge$, wd$, ht$
             lcnt% = lcnt% + 2%
        return

        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

exit_program:

end_program:

        end
        
        
            
