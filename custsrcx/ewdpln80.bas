        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - EWDPLN80                             *~
            *  Creation Date     - 02/04/02                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie Gregory                     *~
            *                                                           *~
            *  Description       - Program to close BA111 customer      *~
            *                      orders for specific Date range.      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/04/02 ! Original                                 ! CMG *~
            *************************************************************

        dim                                                              ~
            cnt$28,                      /* Number Records Purged      */~
            cnt_mst$28,                  /* Number Records Checked     */~
            filename$8,                  /* Filename                   */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            end_date$10,                 /* End Date for Screen        */~
            end_dte$10,                  /* End Date for Purge         */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim or_key$8,                    /* APCPLNOR Readkey           */~
            or_rec$170,                  /* APCPLNOR Record            */~
            sc_key$10,                   /* APCPLNSC Readkey           */~
            sc_rec$128,                  /* APCPLNSC Record            */~
            dt_key$23,                   /* APCPLNDT Readkey           */~
            mst_key$25,                  /* BCKMASTR Readkey           */~
            mst_dte$8,                   /* BCKMASTR Order Date        */~
            lne_key$19                   /* BCKLINES Readkey           */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNDT ! (NEW) Planning Tracking File             *~
            * #3  ! BCKMASTR ! Backlog master file                      *~
            * #4  ! BCKLINES ! Back Log Line Item File                  *~
            * #5  ! APCPLNAD ! (New) Planning Master Audit File         *~
            * #6  ! APCPLNSC ! Planning Master Schedule-Old APCLINES    *~
            * #7  ! APCPLNOR ! Planning S.O. Header History-Old APCORDER*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 24,   keylen = 23,                      ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup


            select #3,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #4, "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #5,  "APCPLNAD",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos = 19,   keylen = 33,                      ~
                        alt key 1, keypos =  1, keylen = 33


            select #6, "APCPLNSC",                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   24, keylen =   10,                    ~
                        alt key  1, keypos =    7, keylen =  27,         ~
                            key  2, keypos =    1, keylen =  33


            select #7, "APCPLNOR",                                      ~
                        varc,     indexed,  recsize = 170,               ~
                        keypos =    1, keylen =   51,                    ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos =   70, keylen =   8, dup,    ~
                            key  3, keypos =   78, keylen =   8, dup,    ~
                            key  4, keypos =   52, keylen =   8,         ~
                            key  5, keypos =   36, keylen =  16, dup


            call "SHOSTAT" ("Opening Files, One moment Please?")

            filename$ = "APCPLNDT" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKMASTR" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
	    filename$ = "BCKLINES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNAD" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSC" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNOR" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "  Close Bay/Bow Customer Orders  "
            pname$ = "EWDPLN80 - Rev: R6.04"

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  1%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
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
                  if keyhit%  = 14% then gosub correct_orders
                  if keyhit% <>  0% then       editpg1
L11140:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 1% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11190:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11190
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11190
                  lastfieldnr% = fieldnr%
            goto L11140


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

        scrn1_msg  :  data                                                   ~
         "Enter a Valid End Purge Date to clear Bay/Bow Orders?"

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
            init(" ") errormsg$, inpmessage$, end_dte$, end_date$

        return


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
              gosub L40170

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     gosub set_pf1
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,23), fac(hex(a4)), apc$                   , ch(35),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "End Purge Date (BAY/BOW):",                  ~
               at (05,30), fac(lfac$(1%)), end_date$            , ch(10),~
                                                                         ~   
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

 
               if keyhit% <> 15 then goto L40200
                  call "PRNTSCRN"
                  goto L40190

L40200: close ws
        call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

set_pf1:
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                      (14)Purge Data   "
            pf$(2%) = "                                        " &        ~
                      "                      (15)Print Screen "
            pf$(3%) = "                                        " &        ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)

            return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                                       "
            pf$(3%) = "                                        " &        ~
                      "                                       " 
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
                on fieldnr% gosub L50010   /* End Purge Date - APCPLNDT */

            return

L50010: REM End Purgre Date                  END_DATE$, END_DTE$   
            if end_date$ <= " " then goto L50020
         
           date% = 0%
           call "DATEOKC" (end_date$, date%, errormsg$ )
           if errormsg$ <> " " then goto L50020
              end_dte$ = end_date$
              call "DATUFMTC" (end_dte$)                                                
        return
L50020:     errormsg$ = "(Error) Invalid Purge Date?"
            gosub error_prompt
            init(" ") end_date$
        return


   
        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        correct_orders
            cnt$ = "Records Deleted = [xxxxxxxx]"
            cnt_mst$ = "Records Checked = [xxxxxxxx]"
            cnt%, cnt_mst% = 0%
            gosub correct_bckmastr
            
        return clear all
        goto editpg1


        correct_bckmastr
           init(" ") mst_key$
           str(mst_key$,1%,9%) = "BA111"
 	correct_bckmastr_nxt
           init(" ") mst_dte$
           open_amt, mst_open = 0.00
           
           read #3,hold,key > mst_key$, using L35075, mst_key$, mst_dte$, ~
                                   mst_open, eod goto correct_bckmastr_done

L35075:           FMT CH(25), POS(806), CH(6), POS(867), PD(15,4)

               if mod(cnt_mst%,50%) <> 0 then goto L35000
               convert cnt_mst% to str(cnt_mst$,20%,8%), pic(########)
               print at(03,26);hex(84);cnt_mst$;

L35000:       cnt_mst% = cnt_mst% + 1% 
              if str(mst_key$,1%,9%) <> "BA111" then goto correct_bckmastr_done

              if str(mst_dte$,1%,6%) > str(end_dte$,1%,6%)                ~
                                            then goto correct_bckmastr_nxt
              if mst_open <= 0.00 then goto L35040

              put #3, using L35070, open_amt
L35070:           FMT POS(867), PD(15,4)
              rewrite #3

L35040:       gosub correct_apcplnor              
              goto correct_bckmastr_nxt
        correct_bckmastr_done
           convert cnt_mst% to str(cnt_mst$,20%,8%), pic(########)
           call "SHOSTAT" (cnt_mst$) : stop
           convert cnt% to str(cnt$,20%,8%), pic(########)
           call "SHOSTAT" (cnt$) : stop
        return

	correct_apcplnor
           init(" ") or_key$, or_rec$
           or_key$ = str(mst_key$,10%,8%)
        correct_apcplnor_nxt
           
           read #7,hold,key 4% = or_key$, using L35060, or_rec$,      ~
                                             eod goto correct_apcplnor_done
L35060:     FMT CH(170)

           str(or_key$,1%,8%) = str(or_rec$,52%,8%)

           if str(or_rec$,27%,5%) <> "BA111" then goto correct_apcplnor_done

           if str(or_rec$,60%,2%) >= "16" then goto correct_apcplnor_done

           if str(or_rec$,127%,6%) > str(end_dte$,1%,6%)                 ~
                                            then goto correct_apcplnor_done
      
              str(or_rec$,60%,2%) = "16"
              str(or_rec$,62%,8%) = date
              put #7, using L35060, or_rec$
              rewrite #7

        correct_apcplnor_done
	      gosub correct_apcplnsc
        return

        correct_apcplnsc
            init(" ") sc_key$, sc_rec$
            str(sc_key$,1%,8%) = str(mst_key$,10%,8%)
        correct_apcplnsc_nxt
            read #6,hold,key  > sc_key$, using L35080, sc_rec$,        ~
                                           eod goto correct_apcplnsc_done
L35080:     FMT CH(128)                  /* (APCPLNSC) - FILE          */

            sc_key$ = str(sc_rec$,24%,10%)
            if str(mst_key$,10%,8%) <> str(sc_rec$,24%,8%) then        ~
                                              goto correct_apcplnsc_done

            if str(sc_rec$,110%,2%) >= "16" then goto correct_apcplnsc_nxt

            
               
            str(sc_rec$,110%,2%) = "16"
            str(sc_rec$,112%,8%) = date
	    put #6, using L35060, sc_rec$
            rewrite #6
            
            goto correct_apcplnsc_nxt
        correct_apcplnsc_done
            gosub correct_bcklines
            gosub delete_apcplndt
        return

        correct_bcklines
            init(" ") lne_key$
            lne_open = 0.00
            str(lne_key$,1%,16%) = str(mst_key$,10%,8%)
        correct_bcklines_nxt
            read #4,hold,key > lne_key$, using L35095, lne_key$,      ~
                                     lne_open, eod goto correct_bcklines_done
L35095:         FMT POS(10), CH(19), POS(109), PD(15,4) 
 
            if str(lne_key$,1%,8%) <> str(mst_key$,10%,8%)            ~
                                       then goto correct_bcklines_done 

            if lne_open <= 0.00 then goto correct_bcklines_nxt

	    put #4, using L35090, open_amt
L35090:           FMT POS(109), PD(15,4) 
            rewrite #4

            goto correct_bcklines_nxt
        correct_bcklines_done
        return

        delete_apcplndt
            call "SHOSTAT" ("Purging Data in APCPLNDT")
            init(" ") dt_key$
            str(dt_key$,1%,10%) = str(mst_key$,10%,8%)
        delete_apcplndt_nxt
            read #1, hold, key > dt_key$, using L36050, dt_key$,     ~
                                          eod goto delete_apcplndt_done

L36050:        FMT POS(24), CH(23)

            if mod(cnt%,50%) <> 0 then goto L36100
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(04,26);hex(84);cnt$;

L36100:     if str(dt_key$,1%,8%) <> str(mst_key$,10%,8%) then       ~
                                             goto delete_apcplndt_done

            
                delete #1
                cnt% = cnt% + 1% 
                goto delete_apcplndt_nxt  
        delete_apcplndt_done
        return


        open_error                                    
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
            err% = 0%
        return

        error_prompt
           comp% = 2%
           hdr$ = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return


        exit_program
          end


