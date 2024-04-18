        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDCLEA2                             *~
            *  Creation Date     - 07/26/99                             *~
            *  Last Modified Date- 04/01/2017                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Last Modified By  - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Master Purge Utility for down sizing *~
            *                      specific Database files.             *~
            *                                                           *~
            *  DATABASE Files    - (1) SHPAUDIT                         *~
            *                      (2) ORADESCR                         *~
            *                      (3) ORADESC2                         *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/28/21 ! (New) Program -  Copy of EWDCLEAN        ! RDB *~
            * 06/29/21 ! (CR2857) - New files added               ! RDB *~
            *************************************************************

        dim                                                              ~
            sel$(18%)1, sel_d$(30%)40,   /* Purge Selection Data       */~
            sel2$8, sel3$8, cnt$28,      /* Sales order Number         */~
            lookup_key$30,               /* S.O. Lookup Key            */~
            ss$(30%)40,                  /* Purge Descriptive Data     */~
            pg_dte$6,                    /* Today's Date               */~
            pg_dte1$6,                   /* Seven Days Old             */~
            pg_dte2$6,                   /* Fourteen Days Old          */~
            pg_dte3$6,                   /* Thirty Days Old            */~
            pg_dte4$6,                   /* 120 Days                   */~
            pg_dte5$6,                   /* 21 Days Old        (AWD001)*/~
            pg_dte6$6,                   /* 365 Days Old        CR2735 */~
            purge_dte$6,                 /* Read Date for Purge        */~
            purge_key$60,                /* For Purge Utilities        */~
            close_date$6,                /* Close date field    CR2857 */~
            filename$8,                  /* Filename                   */~
            so$(10%)8,                   /* Array of sales orders      */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            time$8,                      /* System time                */~
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

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Master Clean-Up/Purge Utility  "
            pname$ = "EWDCLEA2 - Rev: 01.00"

        REM *************************************************************


                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SHPAUDIT ! Lowe's Special Label Database            *~
            * #2  ! ORADESCR ! Master DATABASE for Prod Labels  (AWD003)*~
            * #3  ! ORADESC2 ! WW Description File                      *~
            * #4  ! AWDCART  ! Cart File                        CR2857  *~
            * #5  ! AWDTKCRT ! Truck Load with Carts File       CR2857  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1, "SHPAUDIT",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  1,   keylen = 20   
                        
            select #2, "ORADESCR",                                       ~
                        varc,     indexed,  recsize =  1024,             ~
                        keypos =    1, keylen =  14,                     ~
                        alt key 1, keypos = 15, keylen = 14 
                        
            select #3, "ORADESC2",                                       ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =   1,   keylen = 11
/* CR2857 */
            select #4, "AWDCART",                                        ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    7, keylen =  35,                     ~
                        alt key  1, keypos =    1, keylen =  41,         ~
                            key  2, keypos =   24, keylen =  18,         ~
                            key  3, keypos =   44, keylen =   5, dup
                            
            select #5, "AWDTKCRT",                                       ~
                        varc,     indexed,  recsize =  75,               ~
                        keypos =   1, keylen =    31,                    ~
                        alt key 1, keypos = 9,   keylen = 9, dup


            call "SHOSTAT" ("Opening Files, One Moment Please")

            filename$ = "SHPAUDIT" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ORADESCR" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ORADESC2" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDCART" :  call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDTKCRT" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error            
                        
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
            call "TIME" (time$)
            pg_dte$ = date
                                               /* More than 7 Days Old  */
            call "DATE" addr("G+",pg_dte$, -7%,pg_dte1$,err%)
                                               /* More than 14 Days Old */
            call "DATE" addr("G+",pg_dte$,-14%,pg_dte2$,err%)
                                               /* More than 30 Days Old */
            call "DATE" addr("G+",pg_dte$,-30%,pg_dte3$,err%)
                                               /* More than 120 Days Old */
            call "DATE" addr("G+",pg_dte$,-120%,pg_dte4$,err%)
                                               /* More than 14 Days Old */
            call "DATE" addr("G+",pg_dte$,-14%,pg_dte5$,err%)
                                               /* More than 365 Days Old */
            call "DATE" addr("G+",pg_dte$, -365%,pg_dte6$,err%)
            
            ss$(1%)  = "(SHPAUDIT)-Shipping Audit Data  (365+)  "
            ss$(2%)  = "(ORADESCR)-WW Descriptions Zero SO      "
            ss$(3%)  = "(ORADESC2)-WW Descriptions SO 0,A,B,C,D "
            ss$(4%)  = "(AWDCART )-Carting Data  (365+)         "
            ss$(5%)  = "(AWDTKCRT)-Truck Carting Data  (365+)   "
                                    
        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   5%
L10110:         gosub'051(fieldnr%)        /* Default / Eables */
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
L10230:               gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
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
                  if keyhit%  = 14% then gosub purge_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 5% then editpg1
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
         "Enter a Valid Purge Selection for (SHPAUDIT) Shipping Audit(Y/N)?",~
         "Enter a Valid Purge Selection for (ORADESCR) Zero S.O. No. Entry?",~
         "Enter a Valid Purge Selection for (ORADESC2) Zero S.O. No. Entry?",~
         "Enter a Valid Purge Selection for (AWDCART)  Cart (Y/N)?         ",~
         "Enter a Valid Purge Selection for (ORADESC2) Truck Carts (Y/N)?  "
                  
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
            init(" ") errormsg$, inpmessage$, sel$(), sel_d$(), sel2$,   ~
                      sel3$, lookup_key$, purge_key$
        return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************


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
              gosub L40170

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

/* CR2735 Layout adjusted for new files */
L40190:     gosub set_pf1
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,23), fac(hex(a4)), apc$                   , ch(35),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Clean-Up (SHPAUDIT) Y/N:",                   ~
               at (03,30), fac(lfac$(1%)), sel$(1%)             , ch(01),~
               at (03,40), fac(hex(84)), sel_d$(1%)             , ch(40),~
                                                                         ~
               at (04,02), "Clean-Up (ORADESCR)S.O.:",                   ~
               at (04,30), fac(lfac$(2%)), sel2$                , ch(08),~
               at (04,40), fac(hex(84)), sel_d$(2%)             , ch(40),~
                                                                         ~
               at (05,02), "Clean-Up (ORADESC2)S.O.:",                   ~
               at (05,30), fac(lfac$(3%)), sel3$                , ch(08),~
               at (05,40), fac(hex(84)), sel_d$(3%)             , ch(40),~
                                                                         ~
               at (06,02), "Clean-Up (AWDCART) Y/N:",                    ~
               at (06,30), fac(lfac$(4%)), sel$(4%)             , ch(08),~
               at (06,40), fac(hex(84)), sel_d$(4%)             , ch(40),~
                                                                         ~
               at (07,02), "Clean-Up (AWDTKCRT) Y/N:",                   ~
               at (07,30), fac(lfac$(5%)), sel$(5%)             , ch(08),~
               at (07,40), fac(hex(84)), sel_d$(5%)             , ch(40),~
                                                                         ~
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
            if fieldnr% = 1% then gosub L50010 /* Selection Code Y or N   */
            if fieldnr% = 2% then gosub L50100
            if fieldnr% = 3% then gosub L50200
            if fieldnr% = 4% then gosub L50010 /* Selection Code Y or N   */
            if fieldnr% = 5% then gosub L50010 /* Selection Code Y or N   */
                        
            return

L50010: REM Selection Code                   sel$, sel_d$
            if sel$(fieldnr%) <> " " then goto L50015
                sel$(fieldnr%) = "N"

L50015:     if sel$(fieldnr%) <> "Y" and sel$(fieldnr%) <> "N"        ~
                                         then goto L50020
            if sel$(2%) = "Y" then sel$(3%) = "Y"

            sel_d$(fieldnr%) = str(ss$(fieldnr%),12%)
        return
L50020:     errormsg$ = "(Error) Purge Selection? (Y)es or (N)o"
            gosub error_prompt
            init(" ") sel$(fieldnr%), sel_d$(fieldnr%)
        return

L50100: REM Selection for ORADESCR                     sel2$
            lookup_key$ = all(hex(00))
            if sel2$ <> " " then goto L50120
               sel2$ = "00000000"
               goto L50140
L50120:     if sel2$ = "00000000" then goto L50140
            lookup_key$ = sel2$
            read #2,key >= lookup_key$, using L50130, lookup_key$,     ~
                                                eod goto L50150
L50130:        FMT CH(23)
            call "SHOSTAT" ("Lookup Key = " & lookup_key$ & "  Sel = " &sel2$)
            stop

            if sel2$ <> str(lookup_key$,1%,8%) then goto L50150
L50140:     sel_d$(2%) = "(ORADESCR) Purge < or = " & sel2$

        return
L50150:     errormsg$ = "(Error) Invalid Sales Order Entry?"
            gosub error_prompt
            init(" ") sel2$, sel_d$(fieldnr%)
        return


L50200: REM Selection for ORADESC2                     sel3$
            lookup_key$ = all(hex(00))
            if sel3$ <> " " then goto L50220
               sel3$ = "00000000"
               goto L50240
L50220:     if sel3$ = "00000000" then goto L50240
            lookup_key$ = sel3$
            read #3,key >= lookup_key$, using L50230, lookup_key$,     ~
                                                eod goto L50250
L50230:        FMT CH(23)
            call "SHOSTAT" ("Lookup Key = " & lookup_key$ & "  Sel = " &sel3$)
            stop

            if sel3$ <> str(lookup_key$,1%,8%) then goto L50250
L50240:     sel_d$(3%) = "(ORADES2) Purge < or = " & sel3$

        return
L50250:     errormsg$ = "(Error) Invalid Sales Order Entry?"
            gosub error_prompt
            init(" ") sel3$, sel_d$(fieldnr%)
        return


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        purge_data
            cnt$ = "Records Deleted = [xxxxxxxx]"
            if sel$(1%) = "Y" then gosub purge_shpaudit
            if sel2$ <> "00000000" then gosub purge_oradescr
            if sel3$ <> "00000000" then gosub purge_oradesc2
            if sel$(4%) = "Y" then gosub purge_awdcart      /* CR2857 */
            if sel$(5%) = "Y" then gosub purge_awdtkcrt     /* CR2857 */
            
        return clear all
        goto editpg1

/* Purge shpaudit file */
        purge_shpaudit
            call "SHOSTAT" ("Purging Data in " & str(ss$(1%),1%,10%) )
            purge_key$ = all(hex(00))
            cnt% = 0%
        purge_shpaudit_nxt
            read #1,hold,key > purge_key$, using L20000, purge_key$, ~
                                           eod goto purge_shpaudit_done
L20000:        FMT CH(20)

            purge_dte$ = str(purge_key$,1%,6%)
            if mod(cnt%,50%) <> 0 then goto L20010
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(02,26);hex(84);cnt$;
                                                /* Only Keep (365) Days */
L20010:     if purge_dte$ > pg_dte6$ then goto purge_shpaudit_done
                delete #1
                cnt% = cnt% + 1%
                goto purge_shpaudit_nxt
                
        purge_shpaudit_done
           sel_d$(1%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(1%),21%,8%), pic(########)

        return

/* Purge WW description file */        
        purge_oradescr
            cnt$ = "Records Deleted = [xxxxxxxx]"
            call "SHOSTAT" ("Purging Data in " & str(ss$(8%),1%,10%) )
            purge_key$ =all(hex(00))
            cnt% = 0%

        purge_oradescr_nxt

             read #2, hold, key >= purge_key$, using L20020, purge_key$, ~
                                             eod goto purge_oradescr_done
L20020:      FMT CH(14)

            if mod(cnt%,50%) <> 0 then goto L20030
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(02,26);hex(84);cnt$;
                                               
L20030:     if str(purge_key$,1%,8%) > sel2$ then goto purge_oradescr_done
                delete #2
                cnt% = cnt% + 1%
                goto purge_oradescr_nxt
                
        purge_oradescr_done
           sel_d$(2%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(2%),21%,8%), pic(########)    
        
        return

/* Purge WW second description file */           
        purge_oradesc2
            init(" ") so$()
            cnt$ = "Records Deleted = [xxxxxxxx]"
            call "SHOSTAT" ("Purging Data in " & str(ss$(8%),1%,10%) )
            purge_key$ =all(hex(00))
            cnt% = 0%
            
            so$(1%) = sel3$ : so$(2%) = sel3$ : so$(3%) = sel3$ 
            so$(4%) = sel3$ : so$(5%) = sel3$ :
            str(so$(2%),1%,1%) = "A"
            str(so$(3%),1%,1%) = "B"
            str(so$(4%),1%,1%) = "C"
            str(so$(5%),1%,1%) = "D"
            maxso% = 5%
            for i% = 1% to maxso%
            
              purge_key$ =all(hex(00))
              str(purge_key$,1%,1%) = str(so$(i%),1%,1%)
              
        purge_ordesc2_nxt:

             read #3, hold, key >= purge_key$, using L20040, purge_key$, ~
                                             eod goto purge_next_so
L20040:      FMT CH(11)

            if mod(cnt%,50%) <> 0 then goto L20050
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(02,26);hex(84);cnt$;
                   
L20050:     if str(purge_key$,1%,8%) > so$(i%) then purge_next_so 
             
              delete #3
              cnt% = cnt% + 1%
 
            goto purge_ordesc2_nxt
           
        purge_next_so:           
           next i%   
           
        purge_oradesc2_done
           sel_d$(3%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(3%),21%,8%), pic(########)         
        return        

/* CR2857 */
        purge_awdcart
            call "SHOSTAT" ("Purging Data in " & str(ss$(4%),1%,10%) )
            purge_key$ = all(hex(00))
            cnt% = 0%
        purge_awdcart_nxt
            read #4,hold,key 1% > purge_key$, using L30000, purge_key$, ~
                                           eod goto purge_awdcart_done
L30000:        FMT CH(41)

            purge_dte$ = str(purge_key$,1%,6%)
            if mod(cnt%,50%) <> 0 then goto L30010
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(02,26);hex(84);cnt$;
                                                /* Only Keep (365) Days */
L30010:     if purge_dte$ > pg_dte6$ then goto purge_awdcart_done
                delete #4
                cnt% = cnt% + 1%
                goto purge_awdcart_nxt
                
        purge_awdcart_done
           sel_d$(4%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(4%),21%,8%), pic(########)
       
        return
        
        purge_awdtkcrt
            call "SHOSTAT" ("Purging Data in " & str(ss$(5%),1%,10%) )
            purge_key$ = all(hex(00))
            cnt% = 0%
        purge_awdtkcrt_nxt
            read #5,hold,key > purge_key$, using L30020, purge_key$, ~
                                           close_date$,              ~             
                                           eod goto purge_awdtkcrt_done
L30020:        FMT CH(31), CH(06)

            purge_dte$ = close_date$
            if mod(cnt%,50%) <> 0 then goto L30030
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(02,26);hex(84);cnt$;
                                                /* Only Keep (365) Days */
L30030:     if purge_dte$ > pg_dte6$ then goto purge_awdtkcrt_nxt
            if purge_dte$ = " " then goto purge_awdtkcrt_nxt
                delete #5
                cnt% = cnt% + 1%
                goto purge_awdtkcrt_nxt
                
        purge_awdtkcrt_done
           sel_d$(5%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(5%),21%,8%), pic(########)
               
        return
        
/* error prompt section */
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

