        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN37                             *~
            *  Creation Date     - 10/13/06                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie Gregory                     *~
            *                                                           *~
            *  Description       - Utility to change status of apcplnor,*~
            *                      apcplnsc to status 84 (Cross Dock    *~
            *                      Issues) to close out the order.      *~
            *                      will also delete order detail from   *~
            *                      apcplnsd.                            *~
            *                      Will only let planning run to begin  *~
            *                      - Status must be less than 02 to     *~
            *                        process                            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/13/06 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            *************************************************************



        dim                                                              ~
            filename$8,                  /* Used By EWDOPEN            */~
            sd_key$23,                   /* APCPLNSD Key               */~
            or_key4$8,                   /* APCPLNOR Key               */~
            sc_key$10                    /* APCPLNSC Key               */

        dim scr_so$8,                    /* Screen Sales Order         */~
            or_status$2,                 /* APCPLNOR Status            */~
            or_cuscode$9,                /* APCPLNOR Customer          */~
            cus_desc$30,                 /* Customer Description       */~
            readkey$100,                 /* Generic Readkey            */~
            descr$30,                    /* Generic Description        */~
            hdr$40, msg$(3%)79,          /* Askuser - Var's            */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
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
            dim apc$35, pname$21            
            apc$   = "Sales Order Update - Cross Dock Status"
            pname$ = "AWDPLN37 - 10/13/2006"

        REM *************************************************************



        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNOR ! (NEW) S.O. Header Histroy                *~
            * #2  ! GENCODES ! Master Code Tables File                  *~
            * #3  ! APCPLNSC ! Planning Master Schedule File            *~
            * #4  ! APCPLNSD ! S.O. Scheduling Dept. Detail             *~
            * #5  ! CUSTOMER ! Customer Master FILE                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos = 1,    keylen = 51,                      ~
                        alt key 1, keypos = 27, keylen = 25,             ~
                            key 2, keypos = 70, keylen =  8, dup,        ~
                            key 3, keypos = 78, keylen =  8, dup,        ~
                            key 4, keypos = 52, keylen =  8,             ~
                            key 5, keypos = 36, keylen = 16, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24

            select #3,  "APCPLNSC",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 24,   keylen = 10,                      ~
                        alt key 1, keypos =  7, keylen = 27,             ~
                            key 2, keypos =  1, keylen = 33

            select #4,  "APCPLNSD",                                      ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  23

            select #5,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =  1,   keylen =  9,                      ~
                        alt key  1, keypos  =    10, keylen = 30, dup,   ~
                            key  2, keypos  =   424, keylen =  9, dup,   ~
                            key  3, keypos  =   771, keylen =  9, dup,   ~
                            key  4, keypos  =   780, keylen =  9, dup


            filename$ = "APCPLNOR" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSC" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSD" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "CUSTOMER" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error



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



        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 1%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10220
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
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
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
                  if keyhit%  = 14% then gosub updatedata
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 1% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11240:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11240
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11240
                  lastfieldnr% = fieldnr%
            goto L11130

        REM *************************************************************~
            *             U P D A T E     D A T A                       *~
            *************************************************************

            updatedata
                gosub update_sc
                gosub update_or
REM                gosub update_sd

            return clear all
            goto inputmode

            update_sc
                init(" ") sc_key$
                str(sc_key$,1%,8%) = scr_so$
            next_sc
                read #3, hold, key > sc_key$, using SC_FMT, sc_key$, ~
                                              eod goto sc_done

SC_FMT:              FMT POS(24), CH(10)

                       if str(sc_key$,1%,8%) <> scr_so$ then goto sc_done
  
                       put #3, using SC_FMT1, "84"
SC_FMT1:                     FMT POS(110), CH(02)

                       rewrite #3

                        goto next_sc

            sc_done
            return

            update_or
                init(" ") or_key4$
                str(or_key4$,1%,8%) = scr_so$
  
                read #1, hold, key 4% = or_key4$, eod goto or_done                 

                    put #1, using OR_FMT1, "84", date, userid$

OR_FMT1:              FMT POS(60), CH(02), POS(135), CH(06), ~
                          POS(143), CH(03)

                    rewrite #1
            or_done
            return

            update_sd
                init(" ") sd_key$
                str(sd_key$,1%,8%) = scr_so$
            next_sd
                read #4, hold, key > sd_key$, using SD_FMT, sd_key$, ~ 
                                      eod goto sd_done
SD_FMT:              FMT CH(23)

                     if str(sd_key$,1%,8%) <> scr_so$ then goto sd_done

                        delete #4

                         goto next_sd         
                           
            sd_done
            return

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
         "Enter a Valid Sales Order to Update????                      "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sc_key$, or_key4$, sd_key$,~ 
                      scr_so$, readkey$, or_status$, or_cuscode$, descr$,~
                      cus_desc$


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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40160          /* Entry Selection      */

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,13), fac(hex(94)), errormsg$              , ch(55),~
               at (03,02), "* Sales Order Number:",                      ~
               at (03,25), fac(lfac$(1%)), scr_so$              , ch(08),~
               at (03,40), fac(hex(8c)), or_cuscode$            , ch(09),~
               at (04,40), fac(hex(8c)), or_status$             , ch(02),~
               at (03,50), fac(hex(8c)), cus_desc$              , ch(30),~
               at (04,50), fac(hex(8c)), descr$                 , ch(30),~
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
        if edit% = 2% then L41560     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffffffff1000)
            if fieldnr% = 1% then L41540
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L41540: return

L41560: if fieldnr% > 0% then L41830  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Update Data "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0eff1000)
        return
L41830:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
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
            on fieldnr% gosub L50150          /* Beg/End Rack Prod Date*/ 
 
            return

L50150: REM Sales Order Number                        SCR_SO$   
            if scr_so$ = " " then goto L50190
            gosub validate_so

              if or_status$ = " " then goto L50190
              if or_status$ >= "02" then goto L50190
              gosub get_cusdesc
              gosub get_statdesc

        return
L50190:        errormsg$ = "(Error) Invalid Sales Order"
               init(" ") scr_so$                            
               gosub error_prompt
        return



        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************


        validate_so
            init(" ") or_key4$, or_status$, or_cuscode$
            or_key4$ = scr_so$

            read #1, key 4% = or_key4$, eod goto no_validate

                  get #1, using APCPLNOR4_FMT, or_cuscode$, or_status$
APCPLNOR4_FMT:            FMT POS(27), CH(09), POS(60), CH(02)

        return
        no_validate
        return

        get_cusdesc
            init(" ") cus_desc$
            read #5, key = or_cuscode$, using CUSTOMER_FMT, cus_desc$,~ 
                                          eod goto no_cus_desc
CUSTOMER_FMT:   FMT POS(10), CH(30)


        no_cus_desc
        return

        get_statdesc
            init(" ") readkey$, descr$
            str(readkey$,1%,9%)  = "PLAN STAT" 
            str(readkey$,10%,2%) = or_status$

            read #2, key = readkey$, using GENCODES_FMT, descr$, ~
                                       eod goto no_statdesc
GENCODES_FMT:      FMT POS(25), CH(30)

        no_statdesc
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
           comp% = 2%
           hdr$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program

            end

                 
