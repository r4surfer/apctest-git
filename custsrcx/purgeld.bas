        REM *************************************************************~
            *                                                           *~
            *  Program Name      - PURGELD                              *~
            *  Creation Date     - 03/01/2017                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie Norman                      *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New program to reset load number     *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *  Spec. Comm (Screen 1) -                                  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *03/01/2017! New Program for (EWS) - Last Mod Date    ! CMN *~
            *************************************************************


        dim                              /* (Program) - Variables      */~
            filename$8,                  /* Used By EWDOPEN            */~
            hdr$45, msg$(3%)79,          /* Askuser - Var's            */~
            date$8,                      /* REPORT TITLE               */~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3,                    /* Current User Id            */~
            cnt$45                       /* Cnt display message        */

        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */


        dim beg_ld$5,                    /* Screen Load Number     */~
            end_ld$5,                    /* Screen Load Number     */~
            ld$5,                        /* Lookup Key             */~
            ld_d$30,                     /* Lookup descrition      */~
            beg_ld_d$30,                 /* Load Description       */~
            end_ld_d$30,                 /* Load Description       */~
            ld_key$,                     /* Load Readkey           */~
            ld_rec$128                   /* Load Record            */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Appian File Extract Utility  "
            pname$ = "AWDPLN02 - Rev: R6.04"

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
            * #1  ! AWDPLNLD ! Appian Load Master File                  *~
            * #2  ! APCPLNLD ! Load Master File                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "AWDAPPLD",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   12, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =  16,         ~
                            key  2, keypos =    2, keylen =  15,         ~
                            key  3, keypos =   17, keylen =  15

            select #2,  "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 11,   keylen =  5,                      ~
                        alt key 1, keypos =  3, keylen = 13,             ~
                            key 2, keypos =  1, keylen = 15


            call "SHOSTAT" ("Initialization")


            filename$ = "AWDAPPLD" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNLD" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #2, schema_err%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 2%
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
                  if keyhit%  =  9% then gosub delete_data
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg1
L11150:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 2% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11200:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11200
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11200
                  lastfieldnr% = fieldnr%
            goto L11150

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        delete_data
          call "SHOSTAT" ("Delete AWDAPPLD Data")

          init(" ") ld_key$, ld_rec$
          ld_key$ = beg_ld$
          cnt% = 0%
          cnt$ = "APP Rec Deleted = [xxxxxxxx] Load [xxxxx]"
        appld_nxt
            read #1, hold, key >= ld_key$, using LOAD_FMT, ld_rec$, ~
                                     eod goto no_appld
LOAD_FMT:           FMT CH(128)


              ld_key$ = str(ld_rec$,12%,5%)
              if ld_key$ > end_ld$ then goto no_appld

               if mod(cnt%,50%) <> 0% then goto L44320
                convert cnt% to str(cnt$,20%,8%), pic(########)

                str(cnt$,36%,5%) = ld_key$

                print at(03,50);hex(84);cnt$;

L44320:

                delete #1

                cnt% = cnt% + 1%
              goto appld_nxt

no_appld:
          call "SHOSTAT" ("Delete APCPLNLD Data")

          init(" ") ld_key$, ld_rec$
          ld_key$ = beg_ld$
          cnt% = 0%
          cnt$ = "APC Rec Deleted = [xxxxxxxx] Load [xxxxx]"
        apcld_nxt
            read #2, hold, key >= ld_key$, using LOAD_FMT, ld_rec$, ~
                                     eod goto no_apcld


              ld_key$ = str(ld_rec$,11%,5%)
              if ld_key$ > end_ld$ then goto no_apcld

               if mod(cnt%,50%) <> 0% then goto L44325
                convert cnt% to str(cnt$,20%,8%), pic(########)

                str(cnt$,36%,5%) = ld_key$

                print at(03,50);hex(84);cnt$;

L44325:

                delete #2

                cnt% = cnt% + 1%
              goto apcld_nxt


no_apcld:
        return clear all
        goto inputmode



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
         "Enter a Valid Beginning Load Number ?                        ",~
         "Enter a Valid Ending Load Number ?                           "



        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") beg_ld$, end_ld$, ld_rec$, beg_ld_d$, end_ld_d$, ~
                      ld_d$, cnt$, ld$

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
REM        DATALOAD
REM        RETURN

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT
        REM RETURN

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* APCPLNOR - New File Layout */


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
              on fieldnr% gosub L40200,          /* Screen Load Number  */~
                                L40200

              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Beg Load Number:",                           ~
               at (04,25), fac(lfac$(1%)), beg_ld$              , ch(05),~
               at (04,35), fac(hex(84)),   beg_ld_d$            , ch(30),~
                                                                         ~
               at (05,02), "End Load Number:",                           ~
               at (05,25), fac(lfac$(2%)), end_ld$              , ch(05),~
               at (05,35), fac(hex(84)),   end_ld_d$            , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 15 then goto L40790
                  call "PRNTSCRN"


L40790:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40990     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            return

L40990: if fieldnr% > 0% then L41100  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(9)Delete Load Data                     " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffff09ffffffffff0f1000)
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
            on fieldnr% gosub L50500,         /* Screen Load Number   */~
                              L50700          /* Screen Load Number   */
            return

L50500: REM Screen  Load Number             BEG_LD$
            if beg_ld$ = " " then goto beg_ld_error

            if str(beg_ld$,1%,1%) <> "P" then goto beg_ld_error

            convert str(beg_ld$,2%,4%) to beg_ld%, data goto beg_ld_error

            convert beg_ld% to str(beg_ld$,2%,4%), pic(0000)

            ld$ = beg_ld$
            gosub lookup_load_desc
            beg_ld_d$ = ld_d$
        return
        beg_ld_error
          errormsg$="(Error) - Invalid Load Number, Not Defined?"
          gosub error_prompt
          init(" ") beg_ld$, beg_ld_d$
        return


L50700: REM Screen  Load Number             END_LD$
            if end_ld$ = " " then goto end_ld_error

            if str(end_ld$,1%,1%) <> "P" then goto end_ld_error

            convert str(end_ld$,2%,4%) to end_ld%, data goto end_ld_error

            convert end_ld% to str(end_ld$,2%,4%), pic(0000)

            ld$ = end_ld$
            gosub lookup_load_desc
            end_ld_d$ = ld_d$
        return
        end_ld_error
          errormsg$="(Error) - Invalid Load Number, Not Defined?"
          gosub error_prompt
          init(" ") end_ld$, end_ld_d$
        return

        lookup_load_desc
REM - LOOK-UP LOAD
            read #2,key = ld$, using L51220, ld_d$, eod goto load_error
L51220:        FMT POS(16), CH(30)

        load_error
        return


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************


        open_error
           comp% = 2%
           hdr$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return


        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return


        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
