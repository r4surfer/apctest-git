        REM *************************************************************~
            * AWDSKU                                                    *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *03/05/2009! Original                                 ! CMG *~
            *10/06/2020! CR2695  Add insert date for new SKUs     ! RDB *~
            *************************************************************


        dim                                                              ~
            sku$16,                             /* SKU Number          */~
            upc$20,                             /* UPC Code            */~
            part$25,                            /* Part Number         */~
            sub_part$20,                        /* Sub Part Number     */~
            model$6,                            /* Model Number        */~
            desc$(2%)35,                        /* Desc                */~
            fillerx$38,                         /* Filler CR2695       */~
            readkey$100                         /* Readkey             */


        dim                                                              ~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(23%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */       


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(AWD) Master SKU Number Maintanence "
            pname$ = "AWDSKU - Rev: R6.04"

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
            * #1  ! AWDSKUXR ! Lowes sku cross refference file          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "AWDSKUXR",                                     ~
                        varc,     indexed,  recsize = 256,              ~
                        keypos =    1, keylen =  16,                    ~
                        alt key  1, keypos =  17, keylen =  20,         ~
                            key  2, keypos =  37, keylen =  45, dup


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 0%, rslt$(1%))

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
            gosub initialize_variables

            for fieldnr% = 1% to  6%
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
                  if keyhit%  = 12% then gosub delete_it
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then goto  editpg1
L11140:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  6% then editpg1
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
         "Enter Sku Number to Enter or Lookup                          ",~
         "Enter UPC Code                                               ",~
         "Enter Part Number                                            ",~
         "Enter Sub Part Number                                        ",~
         "Enter Model Number                                           ",~
         "Enter Description                                            "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") readkey$, sku$, upc$, part$, sub_part$, model$, ~
                      desc$(), fillerx$            /* CR2695 */
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
            *                 D A T A   L O A D                         *~
            *************************************************************

        dataload
            rec% = 0%
            read #1,key = readkey$, eod goto L30160
                get #1 using L35040, sku$, upc$, part$, sub_part$, model$,~
                                   desc$()

            rec% = 1%
L30160: return


        dataload1
            rec% = 0%
            read #1,key 1% = readkey$, eod goto noDataload1
                get #1 using L35040, sku$, upc$, part$, sub_part$, model$,~
                                   desc$()

            rec% = 1%
        noDataload1
        return





        REM *************************************************************~
            *              P U T   D A T A   I N T O   F I L E          *~
            *************************************************************

        dataput
            init(" ") readkey$
            readkey$ = sku$
            str(fillerx$,1%,8%) = date$          /* CR2695 */

            read #1,hold,key = readkey$, eod goto L31080
                delete #1

L31080:     put #1 using L35040, sku$, upc$, part$, sub_part$, model$,~
                                   desc$(), fillerx$          /* CR2695 */

            write #1


        return clear all
        goto inputmode

        delete_it
            init(" ") readkey$
            readkey$ = sku$

            read #1,hold,key = readkey$, eod goto L19250
                delete #1
L19250: return clear all
        goto inputmode

        REM *************************************************************~
            *              F O R M A T   S T A T E M E N T S            *~
            *************************************************************

L35040: FMT                 /* FILE: AWDSKUXR                          */~
            CH(16),         /* Sku Number                              */~
            CH(20),         /* UPC Number                              */~
            CH(25),         /* Part Number                             */~
            CH(20),         /* Sub Part Number                         */~
            CH(06),         /* Model                                   */~ 
            2*CH(35),       /* Description                             */~
            CH(99)          /* Filler space  CR2695                    */


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L40070:       gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40220,         /* Sku               */   ~
                                L40220,         /* UPC               */   ~
                                L40220,         /* Part              */   ~
                                L40220,         /* Sub Part          */   ~
                                L40220,         /* Model             */   ~
                                L40220          /* Description        */   
              goto L40250

L40210:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40220:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40230:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40250:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Sku         ",                               ~
               at (06,30), fac(lfac$(1%)), sku$                 , ch(16),~
                                                                         ~
               at (07,02), "UPC         ",                               ~
               at (07,30), fac(lfac$(2%)), upc$                 , ch(20),~
                                                                         ~
               at (08,02), "Part Number  ",                              ~
               at (08,30), fac(lfac$(3%)), part$                , ch(25),~
                                                                         ~
               at (09,02), "Sub-Part Number",                            ~
               at (09,30), fac(lfac$(4%)), sub_part$            , ch(20),~
                                                                         ~
               at (10,02), "Model         ",                             ~
               at (10,30), fac(lfac$(5%)), model$               , ch(06),~
                                                                         ~
               at (11,02), "Description    ",                            ~
               at (11,30), fac(lfac$(6%)), desc$(1%)            , ch(35),~
               at (12,30), fac(lfac$(6%)), desc$(2%)            , ch(35),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 15 then L40640
                  call "PRNTSCRN" : goto L40070

L40640:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40830     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffff09ffffffffff0f1000)
            if fieldnr% = 1% then L40790
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40790:     if fieldnr% > 2% then L40810
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40810:     return

L40830: if fieldnr% > 0% then L40950  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (12)Delete      "
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffff0cffff0f1000)
            if rec% = 1% then return
               str(pf$(1%),64%) = " " : str(pfkeys$,12%,1%) = hex(ff)
            return
L40950:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0fff00)
            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50180,         /* SKU                    */~
                              L50490,         /* UPC                    */~
                              L50740,         /* Part                   */~
                              L51130,         /* SubPart                */~
                              L51250,         /* Model                  */~
                              L51290          /* Desc                   */
            return

L50180: REM Test for Sku Number                   SKU$  
           init(" ") readkey$
           str(readkey$,1,16) = sku$
           gosub dataload

           if rec% = 1% and edit% = 1% then fieldnr% = 6%
        return

L50490: REM Test for UPC Number                   UPC$
           init(" ") readkey$
           str(readkey$,1,16) = upc$
           gosub dataload1

           if rec% = 1% and edit% = 1% then fieldnr% = 6%

        return

L50740: REM Test for Part Number                  PART$ 
        return

L51130: REM Test for Sub Part Number              SUB_PART$
        return

L51250: REM Test for Model Number                 MODEL$
        return

L51290: REM Test for Description                  DESCR$
        return


        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
