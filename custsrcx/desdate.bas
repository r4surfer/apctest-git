        dim                                                              ~
            key$24, rec$128,             /* GENCODES Lookup            */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            inpmessage$79,               /* Edit screen message        */~
            week_range$(54)25,           /* Error message              */~
            days_in_month(12),           /* Error message              */~
            errormsg$79,                 /* Error message              */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(10%),                    /* = 0 if the file is open    */~
                                         /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */ 

                                         /* (AESPRDLB) - Label File    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21                                              
            apc$   = "(AWD) Generate AES Barcode Labels     "
            pname$ = "AWDPLN07 - 03/01/2006"

        REM *************************************************************


                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

            mat f2% = con


            select #1,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24
                                                     /* (AWD002)        */        

            call "SHOSTAT" ("Opening Files, One Moment Please")

            filename$ = "GENCODES" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "                                           "&~
                           "                                   "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

        inputmode_1 
	    edit_sw = 0
            for fieldnr% = 1% to   1%
L10110:         gosub'051(fieldnr%)        /* Default - Enables */
                      if enabled% = 0% then L10240
L10130:         gosub'101(fieldnr%, 1%)    /* Display - Accept  */
                      if keyhit%  =  1% then gosub startover
L10215:               if keyhit% = 16% then exit_program
                      if keyhit% <> 0% then       L10130
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
	    edit_sw = 1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit% = 16% then exit_program
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = 1%
            goto editpg1
REM         if fieldnr% = lastfieldnr% then    editpg1
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
                                                             
        scrn1_msg  :  data                                               ~
         "Enter a starting date in YYYYMMDD format.                    ",~
         "Enter number of weeks 52/53                                  "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28310
                inpmessage$ = edtmessage$
                return

L28310
*        Define the Input Message for the Screen/Field Indicated
            read inpmessage$      /* Read Input Message */
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
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      start_date$, week_range$()                                   
        return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        REM dataload
 
        REM return
 
        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        dataput   
            if str(week_range$(1),1,1) = " " then goto no_calc 
            for l = 1 to 54
                if str(week_range$(l),1,1) = " " then goto skip_rec
                key$ = "WEEKS                   "
                str(key$,10,6) = str(week_range$(l),10,6)
                read #1,key = key$,hold,using F00001,rec$,eod goto notfnd
                str(rec$,25,8) = str(week_range$(l),1,8)
                str(rec$,43,8) = str(week_range$(l),17,8)
                rewrite #1, using F00001, rec$
                goto skip_rec
notfnd:         init(" ") rec$ 
                str(rec$,38,2) = "--"
                str(rec$,1,24) = key$
                str(rec$,25,8) = str(week_range$(l),1,8)
                str(rec$,43,8) = str(week_range$(l),17,8)
                write #1, using F00001,rec$
F00001:     FMT CH(128)
skip_rec:   next l
        return

no_calc:
        errormsg$ = "Must calculate values first"
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
                                                         
              on fieldnr% gosub L40160,     /* start date              */~
                                L40160      /* number of weeks 52/53   */

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low    */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only  */
L40160:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric     */
                                                       
L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
                                                                         ~
               at (03,02), "Starting Date (YYYYMMDD):",                  ~
               at (03,25), fac(lfac$(1%)), start_date$          , ch(08),~
                                                                         ~
               at (04,01), fac(hex(84)), week_range$(01)        , ch(25),~     
               at (04,27), fac(hex(84)), week_range$(19)        , ch(25),~
               at (04,54), fac(hex(84)), week_range$(37)        , ch(25),~
                                                                         ~
               at (05,01), fac(hex(84)), week_range$(02)        , ch(25),~
               at (05,27), fac(hex(84)), week_range$(20)        , ch(25),~
               at (05,54), fac(hex(84)), week_range$(38)        , ch(25),~
                                                                         ~
               at (06,01), fac(hex(84)), week_range$(03)        , ch(25),~
               at (06,27), fac(hex(84)), week_range$(21)        , ch(25),~
               at (06,54), fac(hex(84)), week_range$(39)        , ch(25),~
                                                                         ~
               at (07,01), fac(hex(84)), week_range$(04)        , ch(25),~
               at (07,27), fac(hex(84)), week_range$(22)        , ch(25),~
               at (07,54), fac(hex(84)), week_range$(40)        , ch(25),~
                                                                         ~
               at (08,01), fac(hex(84)), week_range$(05)        , ch(25),~
               at (08,27), fac(hex(84)), week_range$(23)        , ch(25),~
               at (08,54), fac(hex(84)), week_range$(41)        , ch(25),~
                                                                         ~
               at (09,01), fac(hex(84)), week_range$(06)        , ch(25),~
               at (09,27), fac(hex(84)), week_range$(24)        , ch(25),~
               at (09,54), fac(hex(84)), week_range$(42)        , ch(25),~
                                                                         ~
               at (10,01), fac(hex(84)), week_range$(07)        , ch(25),~
               at (10,27), fac(hex(84)), week_range$(25)        , ch(25),~
               at (10,54), fac(hex(84)), week_range$(43)        , ch(25),~
                                                                         ~
               at (11,01), fac(hex(84)), week_range$(08)        , ch(25),~
               at (11,27), fac(hex(84)), week_range$(26)        , ch(25),~
               at (11,54), fac(hex(84)), week_range$(44)        , ch(25),~
                                                                         ~
               at (12,01), fac(hex(84)), week_range$(09)        , ch(25),~
               at (12,27), fac(hex(84)), week_range$(27)        , ch(25),~
               at (12,54), fac(hex(84)), week_range$(45)        , ch(25),~
                                                                         ~
               at (13,01), fac(hex(84)), week_range$(10)        , ch(25),~
               at (13,27), fac(hex(84)), week_range$(28)        , ch(25),~
               at (13,54), fac(hex(84)), week_range$(46)        , ch(25),~
                                                                         ~
               at (14,01), fac(hex(84)), week_range$(11)        , ch(25),~
               at (14,27), fac(hex(84)), week_range$(29)        , ch(25),~
               at (14,54), fac(hex(84)), week_range$(47)        , ch(25),~
                                                                         ~
               at (15,01), fac(hex(84)), week_range$(12)        , ch(25),~
               at (15,27), fac(hex(84)), week_range$(30)        , ch(25),~
               at (15,54), fac(hex(84)), week_range$(48)        , ch(25),~
                                                                         ~
               at (16,01), fac(hex(84)), week_range$(13)        , ch(25),~
               at (16,27), fac(hex(84)), week_range$(31)        , ch(25),~
               at (16,54), fac(hex(84)), week_range$(49)        , ch(25),~
                                                                         ~
               at (17,01), fac(hex(84)), week_range$(14)        , ch(25),~
               at (17,27), fac(hex(84)), week_range$(32)        , ch(25),~
               at (17,54), fac(hex(84)), week_range$(50)        , ch(25),~
                                                                         ~
               at (18,01), fac(hex(84)), week_range$(15)        , ch(25),~
               at (18,27), fac(hex(84)), week_range$(33)        , ch(25),~
               at (18,54), fac(hex(84)), week_range$(51)        , ch(25),~
                                                                         ~
               at (19,01), fac(hex(84)), week_range$(16)        , ch(25),~
               at (19,27), fac(hex(84)), week_range$(34)        , ch(25),~
               at (19,54), fac(hex(84)), week_range$(52)        , ch(25),~
                                                                         ~
               at (20,01), fac(hex(84)), week_range$(17)        , ch(25),~
               at (20,27), fac(hex(84)), week_range$(35)        , ch(25),~
               at (20,54), fac(hex(84)), week_range$(53)        , ch(25),~
                                                                         ~
               at (21,01), fac(hex(84)), week_range$(18)        , ch(25),~
               at (21,27), fac(hex(84)), week_range$(36)        , ch(25),~
               at (21,54), fac(hex(84)), week_range$(54)        , ch(25),~
                                                                         ~
                                                                         ~
               at (22,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

L40420:        if keyhit% <> 15% then goto L40440
                  gosub dataput  
		  inpmessage$ = errormsg$
		  if errormsg$ <> " " then goto L40190
                  gosub startover
                  goto L40190

L40440:        if keyhit% <> 10% then goto L40450
                  gosub calc_data
                  goto L40190

L40450:        close ws
REM            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                       (10)Calculate    "
            pf$(2%) = "(15)Save Data                           " &        ~
                      "                       (16)Exit Program"
            pf$(3%) = "                                        " &        ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffff0affffff0e0f1000)

L40590:     return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "

            on fieldnr% gosub L50100,        /* Purchase Order Number */~
                              L50200         /* Raw Material Rack Qty */

            return
                                             /* (VBKMASTR)             */

L50100: Rem Raw Material Rack Quantity          sc_rack_qty$
        if edit_sw = 0 then return
            convert str(start_date$,1,4) to start_year%, data goto L50190
            convert str(start_date$,5,2) to start_month%, data goto L50190
            convert str(start_date$,7,2) to start_day%, data goto L50195
       days_in_month(01) = 31
       days_in_month(02) = 28
       days_in_month(03) = 31
       days_in_month(04) = 30
       days_in_month(05) = 31
       days_in_month(06) = 30
       days_in_month(07) = 31
       days_in_month(08) = 31
       days_in_month(09) = 30
       days_in_month(10) = 31
       days_in_month(11) = 30
       days_in_month(12) = 31
       run_year% = start_year%
       if start_month% > 6% then run_year% = run_year% + 1%
       tmp_year% = run_year% / 4%
       if tmp_year% * 4% <> run_year% then goto not_leap
       tmp_year% = run_year% / 100%
       if (tmp_year% * 100%) <> run_year% then goto leap_year
       tmp_year% = run_year% / 400%
       if (tmp_year% * 400%) <> run_year% then goto not_leap
leap_year: days_in_month(02) = 29
not_leap:
            if start_month% < 01% or start_month% > 12% then goto L50180
            if start_day% < 01% or start_day% > 31% then goto L50190
            if start_day% > days_in_month(start_month%) then goto L50190
date_fini:
        return

L50180:     errormsg$ = "(Error) Invalid Month?"
            goto L50195
L50190:     errormsg$ = "(Error) Invalid Day?"
L50195:     gosub error_prompt
            init(" ") sc_rack_qty$, aes_qty$, sc_available$
           edit_sw = 0 
        return                                        /* (AWD001)      */

L50200: Rem Raw Material Rack Quantity          sc_rack_qty$

        return

L50290:     errormsg$ = "(Error) Invalid AES Rack Quantity?"
            gosub error_prompt
            init(" ") sc_rack_qty$, aes_qty$, sc_available$
        return

calc_data:
        if edit_sw = 0 then return
	gosub L50100
       num_weeks% = 54%                
   data_err:
       for l = 1 to num_weeks%
           week_range$(l) = "            --           "
	   if l < 53 then goto good_week
	   if l > 53 then goto skip_mon_inc2
           if start_month% = 1 then goto skip_mon_inc2
           if start_day% > 28 then goto skip_mon_inc2
good_week:
	   l% = l
           convert run_year%  to str(week_range$(l),10,4), pic (0000)
           convert l%         to str(week_range$(l),14,2), pic (00)
           convert start_year%  to str(week_range$(l),1,4), pic (0000)
           convert start_month% to str(week_range$(l),5,2), pic (00)
           convert start_day%   to str(week_range$(l),7,2), pic (00)
           start_day% = start_day% + 6%
	   if start_day% <= days_in_month(start_month%) then goto skip_mon_inc
           start_day% = start_day% - days_in_month(start_month%)
           start_month% = start_month% + 1%
           if start_month% <= 12% then goto skip_mon_inc
           start_month% = start_month% - 12%
           start_year% = start_year% + 1%
  skip_mon_inc:
           convert start_year%  to str(week_range$(l),17,4), pic (0000)
           convert start_month% to str(week_range$(l),21,2), pic (00)
           convert start_day%   to str(week_range$(l),23,2), pic (00)
           start_day% = start_day% + 1%
	   if start_day% <= days_in_month(start_month%) then goto skip_mon_inc2
           start_day% = start_day% - days_in_month(start_month%)
           start_month% = start_month% + 1%
           if start_month% <= 12% then goto skip_mon_inc2
           start_month% = start_month% - 12%
           start_year% = start_year% + 1%
  skip_mon_inc2:
       next l
       return

        set_keys
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
        return


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

