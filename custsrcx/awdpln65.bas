        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN65                             *~
            *  Creation Date     - 11/06/2012                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie Gregory                     *~
            *                                                           *~
            *  Description       - This Program Calculates the daily    *~
            *                      completions against daily production *~
            *                      schedule                             *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *11/06/2012! New Program for (AWD) - Last Mod Date    ! CMG *~
            *02/15/2018! SR1111 Add new depts and times           ! RDB *~
            *04/18/2018! CR1464 Add TX hours, skips and supports  ! RDB *~
            *01/06/2020! CR2374 Change NC dept 049 and skip parts ! RDB *~
            *07/26/2021! CR2862 Time changes for Chad             ! RDB *~
            *11/17/2021! CR2957 New Dept 006 for sliders          ! RDB *~            
            *************************************************************


        dim hdr$40, msg$(3%)79,          /* Shostat Error Messages     */~
            filename$8,                  /* Used By EWDOPEN            */~
            date$10,                     /* Todays Date                */~
            bg_prd$10, bg_dte$10,        /* Beg/End Production Date    */~
            ed_prd$10, ed_dte$10,        /*                            */~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */


        dim f2%(25%),                     /* = 0 if the file is open    */~
            f1%(25%),                     /* = 1 if READ was successful */~
            fs%(25%),                     /* = 1 if file open, -1 if it */~
                                          /*   doesn't exist, or 0 if   */~
                                          /*   not yet checked (OPENCHCK*/~
            rslt$(25%)20                  /* Text from file opening     */


        dim due_date$6,                   /* Schedule Adherence Due Date*/~
            due_time$4,                   /* Schedule Adherence Due Time*/~
            am_pm$2,                      /* AM_PM                      */~
            adj_time$4,                   /* Adjusted complete time     */~
            skipDept$3,                   /* Depts to Skip              */~
            cnt$28                        /* Count Display              */

        dim dt_rec$256,                   /* APCPLNDT Record            */~
            dt_key1$57,                   /* Apcplndt readkey 1         */~
            dt_bar$18,                    /* Apcplndt barcode           */~
            dt_dept$3,                    /* Apcplndt department        */~
            dt_st$2,                      /* Apcplndt status            */~
            dt_date$6,                    /* Apcplndt prod date         */~
            dt_part$25                    /* Apcplndt part number CR2374*/


        dim ad_rec$64,                    /* APCPLNAD Record            */~
            ad_key1$33,                   /* Apcplnad readkey 1         */~
            ad_dept$3,                    /* Apcplnad department        */~
            ad_st$2,                      /* Apcplnad status            */~
            ad_st1$2,                      /* Apcplnad test status       */~
            ad_time$8                     /* Apcplnad scanned time      */
            
        dim schema$8                     /* Schema                     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$36, pname$21
            apc$   = "  Schedule Adherence Report  "
            pname$ = "AWDPLN65 - Rev: R6.04"

        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNAD ! Planning Master Audit File               *~
            * #2  ! APCPLNDT ! Production Master Detail File            *~
            * #3  ! GENCODES ! Master Code Tables File                  *~
            * #10 ! AWDSCHAD ! Work File for ALL Departments            *~
            * #12 ! WORKFILE ! Work File to Sort Data                   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,  "APCPLNAD",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos = 19,   keylen = 33,                      ~
                        alt key 1, keypos =  1, keylen = 33


            select #2,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 24,   keylen = 23,                      ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,    keylen = 24

            select #10, "AWDSCHAD",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,    keylen = 27


            filename$ = "APCPLNAD" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDT" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error

            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
            if fs%(10%) < 0% then goto create_sort_file


                 call "FILEBGON" (#10)


create_sort_file:

            call "OPENCHCK" (#10, fs%(10%), f2%(10%),500%, rslt$(10%))

            
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #3, schema_err%)
                    

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 1% 
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10240
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
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
                  if keyhit%  =  8% then gosub report_analysis

                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg1
L11160:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 1% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11210:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11210
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11210
                  lastfieldnr% = fieldnr%
            goto L11160

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
         "Enter a Valid Beginning / Ending Production Date ?           "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") bg_prd$, ed_prd$, bg_dte$, ed_dte$

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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub set_pf1

              gosub'050(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40200          /* Beg/End Prod Date    */

              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,24), fac(hex(a4)), apc$                   , ch(38),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Begin Prod Date      :",                     ~
               at (03,25), fac(lfac$(1%)), bg_dte$              , ch(10),~
                                                                         ~
               at (03,40), "Ending Prod Date     :",                     ~
               at (03,63), fac(lfac$(1%)), ed_dte$              , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40990     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40950
               str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40950:     if fieldnr% > 1% then L40970
               str(pf$(1%),18%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40970:     return

L40990: if fieldnr% > 0% then L41100  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "(8)Schedule Analysis                   "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffff08ffffffffffff0f1000)
            return
L41100:
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
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
            on fieldnr% gosub L50150          /* Beg/End Prod Date     */

            return

L50150: REM Beginning Prod Date                   BG_PRD$, ED_PRD$, BG_DTE$, ED_DTE$
            if bg_dte$ > " " then goto L50190
               bg_dte$ = date$

L50190:     date% = 0%
            call "DATEOKC" (bg_dte$, date%, errormsg$)
            if date% = 0% then goto date_error
            bg_prd$ = bg_dte$
            call "DATUFMTC" (bg_prd$)
REM END
            if len(ed_dte$) < 6 then ed_dte$ = bg_dte$
            date% = 0%
            call "DATEOKC" (ed_dte$, date%, errormsg$)
            if date% = 0% then goto date_error
            ed_prd$ = ed_dte$
            call "DATUFMTC" (ed_prd$)
            if bg_prd$ > ed_prd$ then goto L50330
        return
L50330:     errormsg$ = "(Error) - Invalid Due Date Range??"
        date_error
            gosub error_prompt
            init(" ") bg_prd$, bg_dte$, ed_prd$, ed_dte$
        return



        REM *************************************************************~
            *           S p e c i a l   S u b r o u t i n e s           *~
            *************************************************************

        report_analysis
          cnt% = 0%
          init(" ") dt_rec$, dt_key1$, dt_bar$, dt_dept$, dt_st$, dt_date$, ~
                    dt_part$   /* CR2374 */
          dt_key1$ = all(hex(00))
          str(dt_key1$,1%,6%) = bg_prd$

        rpt_analysis_next
          read #2, key 1% > dt_key1$, using DT_FMT, dt_rec$,              ~
                                               eod goto rpt_analysis_done
DT_FMT:      FMT CH(256)

             cnt% = cnt% + 1%
             dt_key1$ = str(dt_rec$,47%,57%)
             dt_date$ = str(dt_rec$,47%,6%)
             dt_dept$ = str(dt_rec$,59%,4%)
             dt_bar$  = str(dt_rec$,24%,18%)
             dt_st$   = str(dt_rec$,64%,2%)
             dt_part$ = str(dt_rec$,189%,25%)   /* CR2374 */          
             
             goto noSHOSTAT

             bar% = 0%
             gosub check_barcode
             if bar% = 0% then goto noSHOSTAT
                    call "SHOSTAT" ("BARCODE --> " & dt_bar$)
                    stop   
             
noSHOSTAT:             
             
             if mod(cnt%,50%) <> 0% then goto notDisplay
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;
             
notDisplay:

             if dt_date$ > str(ed_prd$,1%,6%) then goto rpt_analysis_done
             skipDept$ = dt_dept$
             gosub isSkip
             if skip% = 1% then goto rpt_analysis_next
             gosub isSupport
             numToAdd% = 0%
             if support% = 1% then gosub get_day
             if schema% = 1% and ~
                (dt_dept$ = "005" or dt_dept$ = "049")  then ~
                  gosub get_day
             due_date$ = dt_date$
             due_time$ = "1700"   /* SR1111 */

REM schema% = 1% -> NC
REM schema% = 2% -> TX             
                          
             if schema% <> 1% then goto skipNCTimeCheck
                if dt_dept$ = "002" then due_time$ = "1700"
                if dt_dept$ = "008" then due_time$ = "1700"
                if dt_dept$ = "014" then due_time$ = "1700"
                if dt_dept$ = "047" then due_time$ = "1700"
                if dt_dept$ = "043" then due_time$ = "1700"
                
                if dt_dept$ = "044" or dt_dept$ = "054" or dt_dept$ = "074"  ~
                        then due_time$ = "1500"  /* SR1111 */
                        
                if dt_dept$ = "020" or dt_dept$ = "023" or dt_dept$ = "049"  ~
                        then due_time$ = "2300"  /* SR1111  CR2374 */
                
                if dt_dept$ = "036" then due_time$ = "1700"  /* SR1111 */
                
                if dt_dept$ = "000" then due_time$ = "2359"  /* CR2862 */

                gosub nc_nxt_day       /* CR2862 */
                goto prcSupprt
skipNCTimeCheck:             
             if schema% = 2% then due_time$ = "1800"
             if dt_dept$ = "021" or dt_dept$ = "044" or dt_dept$ = "074"  ~
                        then due_time$ = "1200"  /* CR1464 */
             if dt_dept$ = "015" or dt_dept$ = "045" ~
                        then due_time$ = "0300"  /* CR1464 */
prcSupprt:             
             if support% = 1% then                   ~
                call "DATE" addr("G+", dt_date$, +numToAdd%, due_date$,err%) 
                        
REM          IF SUPPORT% = 1% THEN DUE_TIME$ = "2300"

             gosub isComplete
             gosub updateCyberFile
               goto rpt_analysis_next
        rpt_analysis_done     
        return clear all
        goto inputmode

*****
        /* CR2862 next day due for NC departments */
        nc_nxt_day
           if schema% <> 1% then return
           if dt_dept$ <> "005" and dt_dept$ <> "049" then return

           gosub get_day
           call "DATE" addr("G+", dt_date$, +numToAdd%, due_date$,err%) 
           due_time$ = "0200"
        return
        
*****
** Also Dept 020 & 023 added to support like WS
****
        
        isSupport
          support% = 1%
          if schema% = 2% then goto L60000            /* CR1464 */
/* NC support departments */
          if dt_dept$ = "044" then return
          if dt_dept$ = "054" then return
          if dt_dept$ = "074" then return
/*          if dt_dept$ = "020" then return    SR1111 */
/*          if dt_dept$ = "023" then return    SR1111 */
             support% = 0%
             goto L69999
/* TX support departments CR1464 */
L60000:
          if dt_dept$ = "015" or dt_dept$ = "021" or dt_dept$ = "044" or ~
             dt_dept$ = "045" or dt_dept$ = "074"  then return
               support% = 0%
L69999: return
        
        isSkip
          skip% = 1%
          if schema% = 2% then L70000                 /* CR1464 */
/* NC skip departments */
REM CR2957       if skipDept$ = "001" or skipDept$ = "006" then return
          if skipDept$ = "001" then return
          if skipDept$ = "011" or skipDept$ = "021" then return
          if skipDept$ = "032" or skipDept$ = "066" then return
          if skipDept$ = "095" or skipDept$ = "099" then return
          if skipDept$ = "100" or skipDept$ = "102" then return
REM 2862          if len(dt_part$) < 19% then return          /* CR2374 */
             skip% = 0%
             goto L79999
L70000:  
/* TX skip departments CR1464*/
          if skipDept$ = "001" or skipDept$ = "011" then return 
          if skipDept$ = "071" or skipDept$ = "101" then return 
          if skipDept$ = "102" or skipDept$ = "104" then return 
             skip% = 0%
L79999:  return   
        
        get_day
          day% = 0%
          numToAdd% = 1%
          call "DAY" addr(dt_date$, day%)
          day% = day% - 1%    /* So Monday = 1, etc.  */
          if day% = 5% then numToAdd% = 3%
          if day% = 6% then numToAdd% = 2%   /* SR1111 Saturday planning */
        return                  

        isComplete
          init(" ") ad_rec$, ad_key1$, ad_dept$, ad_st$, ad_time$
          ad_key1$ = all(hex(00))
          str(ad_key1$,1%,18%) = dt_bar$

        complete_next
          read #1, key 1% > ad_key1$, using AD_FMT, ad_rec$,             ~
                                                eod goto complete_done
AD_FMT:      FMT CH(64)

             ad_key1$ = str(ad_rec$,1%,33%)
             
             if str(ad_rec$,1%,18%) <> dt_bar$ then goto complete_done
REM             skipDept$ = str(ad_rec$,25%,3%)
REM             gosub isSkip
REM             if skipDept% = 1% then goto complete_next

             if str(ad_rec$,25%,3%) <> dt_dept$ then goto complete_next
             ad_st1$ = str(ad_rec$,32%,2%)
/* Want completed status or the closest status to complete */             
REM if ad_st$ = "12" and ad_st1$ > "12" then goto complete_done
REM if ad_st1$ < ad_st$ and ad_st$ <> " " and ad_st1$ <> "12" then goto complete_next 
REM if ad_st1$ > "12" and ad_st$ <> " " then goto complete_next
              if ad_st$ = " " then gosub isStatus
              if ad_st1$ = "12" then gosub isStatus
                            
              if ad_st$ < ad_st1$                          ~
                 and ad_st1$ <= "14"                       ~
                 and ad_st$ <> "12" then gosub isStatus

              if ad_st$ < ad_st1$ and ad_st1$ = "16" and   ~
                  len(dt_part$) < 19%   then gosub isStatus
              
               goto complete_next

        complete_done
          onTime% = 1%
          if ad_st$ = "04" then onTime% = 0%
          
          if str(ad_dte$,1%,6%) > str(due_date$,1%,6%) and   ~
              adj_time$ > due_time$ then onTime% = 0%
          if str(ad_dte$,1%,6%) = str(due_date$,1%,6%) and   ~
              adj_time$ > due_time$ then onTime% = 0%              
          if str(ad_dte$,1%,6%) > str(due_date$,1%,6%) then onTime% = 0%
         
REM          if onTime% = 1% and adj_time$ > due_time$ then onTime% = 0%
        return
        
        isStatus
         hr%,  min% = 0%
         ad_st$   = str(ad_rec$,32%,2%)
         ad_time$ = str(ad_rec$,52%,8%)
         ad_dte$  = str(ad_rec$,19%,6%)
         am_pm$   = str(ad_time$,7%,2%)
         convert str(ad_time$,1%,2%) to hr%, data goto badHr
         
badHr:
         convert str(ad_time$,4%,2%) to min%, data goto badMin
         
badMin:

         if am_pm$ = "PM" and hr% <> 12% then hr% = hr% + 12%
         convert hr%  to str(adj_time$,1%,2%), pic(00)
         convert min% to str(adj_time$,3%,2%), pic(00)         
        return

        updateCyberFile
             put #10, using SCHEDULE_FMT,                     ~
                      dt_date$,                               ~
                      dt_dept$,                               ~
                      dt_bar$,                                ~
                      dt_st$,                                 ~
                      due_date$,                              ~
                      due_time$,                              ~
                      ad_st$,                                 ~
                      ad_time$,                               ~
                      ad_dte$,                                ~
                      adj_time$,                              ~
                      onTime%                               
                                            
             write #10, eod goto updateDone         

        updateDone     
        return
SCHEDULE_FMT:   FMT CH(06), CH(03), CH(18), CH(02), CH(06),   ~
                    CH(04), CH(02), CH(08), CH(06), CH(04),   ~
                    BI(01)

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
        
        check_barcode
          if dt_dept$ = "001" then return
          if dt_dept$ = "011" then return          
REM          if dt_dept$ = "044" then return      
          if dt_bar$ = "038904960200010003" then bar% = 1%
          if dt_bar$ = "038904960200030003" then bar% = 1%
          if dt_bar$ = "038873270200020002" then bar% = 1%

        return


        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program

         end

