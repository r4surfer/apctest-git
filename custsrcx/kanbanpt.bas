        REM *************************************************************~
            *                                                           *~
            *  K   K   AAA   N   N  BBBB    AAA   N   N  PPPP   TTTTT   *~
            *  K KK   A   A  NN  N  B   B  A   A  NN  N  P   P    T     *~
            *  KK     AAAAA  N N N  BBBB   AAAAA  N N N  PPPP     T     *~
            *  K KK   A   A  N  NN  B   B  A   A  N  NN  P        T     *~
            *  K   K  A   A  N   N  BBBB   A   A  N   N  P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * KANBANPT - Add / Change Records Kanban Part Number        *~
            *-----------------------------------------------------------*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *03/08/2010! ORIGINAL                                 ! CMG *~
            *************************************************************


        dim                                                              ~
            hdr$40, msg$(3%)79,          /* ASKUSER TEXT               */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(40%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */ 


        dim kanbanptKey$40,              /* Kanbanpt Readkey           */~
            scrSeq$10,                   /* Part Sequence Number       */~
            scrPart$45                   /* Screen Part Number         */


        dim                                                              ~
            readkey$30,                  /* GENCODES Look-Up Key       */~
            descr$30                     /* Use for GENCODES Look-Up   */


        dim                                                              ~
            f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$41, pname$21
            apc$   = "  Kanban Master MFG Part Utility  "
            pname$ = "KANBANPT "

        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * #1  ! KANBANPT ! Master Kanban Mfg Part File              *~
            *************************************************************

            select #1,  "KANBANPT",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =  1,    keylen = 10,                     ~
                        alt key  1, keypos =   11, keylen =  45


            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1%),  f2%(1%), 100%, rslt$(1%))
            call "OPENCHCK" (#3,  fs%(3%),  f2%(3%),   0%, rslt$(3%))

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


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            call "ALLFREE"
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
L10200:               if keyhit% = 16% and fieldnr% = 1% then exitProgram
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
            fieldnr% = 0%
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub dataSave
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 3%
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
         "Enter Sequence Number                                        ",~
         "Enter Mfg Part Number                                        "



        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
          init(" ") errormsg$, scrSeq$, scrPart$, kanbanptKey$
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
            dataLoad 
              rec% = 0%

              read #1, key key% = kanbanptKey$, eod goto noDataLoad

                  get #1, using kanbanptFMT, scrSeq$,  /* Sequence Number */~
                                             scrPart$  /* Part Number     */

                   rec% = 1%

            noDataLoad
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Update Store Data and Part Data                           *~
            *************************************************************
            dataSave
                if scrSeq$ = "NEW" then gosub assignNumber


                read #1, hold, key = scrSeq$, eod goto noRec
                      delete #1

noRec:
                put #1, using kanbanptFMT, scrSeq$,  /* Sequence Number */~
                                           scrPart$  /* Part Number     */


                write #1
            return clear all
            goto inputmode


            deleteItem
                read #1, hold, key = scrSeq$, eod goto noRecDelete
                      delete #1

noRecDelete:
            return clear all
            goto inputmode


        assignNumber
           init(" ") readkey$, descr$             :    seq% = 0%
           str(readkey$,1,9)   = "SEQUENCE"
           str(readkey$,10,15) = "KANBANPT"
      
           read #3, hold, key = readkey$, using L51910, descr$, ~
                                                eod goto noSeq
L51910:       FMT POS(25), CH(30)

                   convert str(descr$,1,10) to seq%, data goto noSeq

                   seq% = seq% + 1%

                   convert seq% to scrSeq$,pic(0000000000)

                   descr$ = scrSeq$

                   put #3, using L51910, descr$
                   rewrite #3

        return
        noSeq
           errormsg$ = "(ERROR)-Cannot assign sequence Number-GENCODES ~
                       ~SEQUENCE!"
           gosub error_prompt
        return



        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
kanbanptFMT:   FMT CH(10),                          /* Sequence Number */~
                   CH(45)                           /* Part Number     */



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
              on fieldnr% gosub L40210,          /* Sequence          */ ~
                                L40200           /* PartNumber        */

              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40210:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,20), fac(hex(a4)), apc$                   , ch(41),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Seq  Number : ",                             ~
               at (04,20), fac(lfac$(1%)), scrSeq$              , ch(10),~
                                                                         ~
               at (05,02), "Part Number : ",                             ~
               at (05,20), fac(lfac$(2%)), str(scrPart$,1,25)   , ch(25),~
               at (05,50), fac(lfac$(2%)), str(scrPart$,26,20)  , ch(20),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 12 then goto L40620
                  gosub deleteItem

L40620:        if keyhit% <> 15 then goto L40640
                  call "PRNTSCRN"
                  goto L40230

L40640:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1

        if edit% = 2% then L40830     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffff07ff09ffffffffff0f1000)
            if fieldnr% = 1% then L40790
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40790:     if fieldnr% > 1% then L40810
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40810:     return

L40830: if fieldnr% > 0% then L40920  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (12)Delete Item "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Data Save   "
            pfkeys$ = hex(01ffffffffffffff09ffff0cffff0f1000)
            return
L40920:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
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
            on fieldnr% gosub L50150,         /* Sequence Number       */ ~
                              L50350          /* Part Number           */
            return


L50150: REM Sequence Number                       SCRSEQ$
            if scrSeq$ =  " " then scrSeq$ = "NEW"

            if scrSeq$ = "NEW" then return
            
            scrSeq% = 0%
            convert scrSeq$ to scrSeq%, data goto L50290
            convert scrSeq% to scrSeq$, pic(0000000000)
            key% = 0%
            init(" ") kanbanptKey$
            kanbanptKey$ = scrSeq$
            if scrSeq$ <> "NEW" then gosub dataLoad
REM No Record Found
            if rec% = 0% then goto L50290

            fieldnr% = 2%
        return
L50290:     errormsg$ = "(Error) - Invalid Sequence Number?"
            gosub error_prompt
            init(" ") scrPart$
        return

L50350: REM Part Number                           SCRPART$
            for i% = 1% to 19%
              if str(scrPart$,i%,1%) =  " " then goto L50390
            next i%

            for i% = 26% to 45%
              if str(scrPart$,i%,1%) =  " " then goto L50390
            next i%

            key% = 1%
            init(" ") kanbanptKey$
            kanbanptKey$ = scrPart$
            gosub dataLoad
            if rec% = 1% then fieldnr% = 2%
             
        return
L50390:     errormsg$ = "(Error) - Invalid Part Number?"
            gosub error_prompt
REM            init(" ") scrPart$
        return


        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return



        exitProgram
            end






