        REM *************************************************************~
            *                                                           *~
            * APCVEND  - Vendor Mailing Labels Print Program            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/06/97 ! New Program for (APC) - Last Mod Date    ! JBF *~
            *          !                                          !     *~
            * 11/13/97 ! Changed Program Version ID To 60403      ! DJD *~
            *          !                                          !     *~
            *************************************************************

        dim                                                              ~
            vendor_no$9,                 /* Vendor Number              */~
            vendor_name$30,              /* Vendor Name                */~
            beg_no$9, beg_desc$30,       /* Beginning Vendor Number    */~
            end_no$9, end_desc$30,       /* Ending Vendor Number       */~
            vendor_add$(5%)30,           /* Vendor Billing Address (5) */~
            vendor_desc$30,              /* Vendor Description         */~
            ven_key$9,                   /* VENDOR File Read Key       */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(1),                      /* = 0 if the file is open    */~
            f1%(1),                      /* = 1 if READ was successful */~
            fs%(1),                      /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(1)20                   /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/13/97 Vendor Mailing Labels Utility  "
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
            * #1  ! VENDOR   ! Vendor Master File                       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "VENDOR",                                       ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =    9,                    ~
                        alt key  1, keypos =   10, keylen =  30, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, fs%(1), f2%(1),  0%, rslt$(1))

            f1%(1) = 0%

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

            for fieldnr% = 1% to 2%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
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
                  if keyhit%  = 14% then gosub begin_process
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
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
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        begin_process
            call "SHOSTAT" ("Creating Vendor Mailing Labels")
            ven_key$ = all(hex(00))
            read #1,key > ven_key$, using L19103, vendor_no$,             ~
                vendor_desc$, vendor_name$, vendor_add$(),               ~
                eod goto process_done
L19103:         FMT CH(09), CH(30), CH(30), 5*CH(30)

            goto L19160
        begin_next
            read #1, using L19103, vendor_no$, vendor_desc$, vendor_name$,~
                vendor_add$(), eod goto process_done

L19160:     if beg_no$    = "ALL      " then goto L19260
            if vendor_no$ <  beg_no$    then goto begin_next
            if vendor_no$ >  end_no$    then goto process_done

L19260:     call "APCVENLB" ( vendor_no$,           /* Vendor No.      */~
                              vendor_name$,         /* Vendor Name     */~
                              vendor_desc$,         /* Vendor Descr.   */~
                              vendor_add$() )       /* Vendor Bill Addr*/

            init(" ") vendor_no$, vendor_name$, vendor_desc$,            ~
                      vendor_add$()
            goto begin_next
        process_done
            vendor_no$ = "END LABEL"
            call "APCVENLB" ( vendor_no$,           /* Vendor No.      */~
                              vendor_name$,         /* Vendor Name     */~
                              vendor_desc$,         /* Vendor Descr.   */~
                              vendor_add$() )       /* Vendor Bill Addr*/

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
         "Enter a Valid Beginning Vendor Number or (ALL).              ",~
         "Enter a Valid Ending Vendor Number or Leave Blank.           "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, beg_no$, end_no$, ven_key$,~
                      beg_desc$, end_desc$, vendor_no$, vendor_name$,    ~
                      vendor_desc$, vendor_add$()
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
        dataload
            rec% = 0%
            read #1,key = ven_key$, using L19103,                         ~
                vendor_no$, vendor_desc$, vendor_name$, vendor_add$(),   ~
                eod goto L30130

            rec% = 1%
L30130: return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT
        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE

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
              on fieldnr% gosub L40170,         /* Beginning Vendor No. */~
                                L40170          /* Ending    Vendor No. */

              goto L40200

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
               at (01,02),                                               ~
                  "Create Vendor Mailing Labels   ",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Beginning Vendor No.:",                      ~
               at (06,25), fac(lfac$( 1)), beg_no$              , ch(09),~
               at (06,40), fac(hex(84)), beg_desc$              , ch(30),~
                                                                         ~
               at (07,02), "Ending    Vendor No.:",                      ~
               at (07,25), fac(lfac$( 2)), end_no$              , ch(09),~
               at (07,40), fac(hex(84)), end_desc$              , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40460
                  call "PRNTSCRN"
                  goto L40200

L40460:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40650     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40610
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40610:     if fieldnr% > 1% then L40630
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40630:     return

L40650: if fieldnr% > 0% then L40740  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Labels"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40740:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
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
            on fieldnr% gosub L50120,         /* Beginning Vendor No.  */ ~
                              L50360          /* Ending    Vendor No.  */
            return

L50120: REM Beg Vendor Number                   BEG_NO$
            if beg_no$  <> " " then goto L50190
               beg_no$   = "ALL      "
               beg_desc$ = "Print Labels for All Vendors"
               end_no$, end_desc$ = " "
               fieldnr% = 2%
               return
L50190:     convert beg_no$ to beg_no%, data goto L50320

            convert beg_no% to beg_no$, pic(000000000)
            ven_key$  = beg_no$
            gosub dataload
            if rec%   = 0% then goto L50320
            beg_desc$ = vendor_desc$
        return
L50320:     errormsg$ = "Invalid Beginning Vendor Number."
            beg_no$, beg_desc$ = " "
        return

L50360: REM Ending Vendor Number                END_NO$
            if end_no$ <> " "     then goto L50410
            if beg_no$  = "ALL  " then return
               end_no$  =  beg_no$

L50410:     convert end_no$ to end_no%, data goto L50540

            convert end_no% to end_no$, pic(000000000)
            ven_key$ = end_no$
            gosub dataload
            if rec% = 0% then goto L50540
            end_desc$ = vendor_desc$
        return
L50540:     errormsg$ = "Invalid Ending Vendor Number."
            end_no$, end_desc$ = " "
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
