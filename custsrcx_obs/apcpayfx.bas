        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPAYFX                             *~
            *  Creation Date     - 08/17/95                             *~
            *  Last Modified Date- 11/12/97                             *~
            *  Description       - This Program Correct Payables Data   *~
            *                      in PAYMASTR and PAYLINES.            *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/16/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            *          !                                          !     *~
            * 11/11/97 ! Revision Update For 60403                ! DJD *~
            *************************************************************

        dim                                                              ~
            apc_ven$9, apc_ven_d$30,     /* Vendor Code                */~
            apc_inv$16,                  /* Invoice Number             */~
            apc_line$3, apc_line_po$3,   /* Invoice Line Item Number   */~
            apc_rcv$16,                  /* Receiver Number            */~
            apc_part$25,                 /* APC Raw Mat'l Part No.     */~
            apc_rec1$(2%)200,            /* Paymaster Record           */~
            apc_rec2$(3%)200,            /* Paylines Record            */~
            apc_key1$25,                 /* Paymaster Key              */~
            apc_key2$28,                 /* Paylines Key               */~
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

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/12/97 APC PayaBLES Fix               "
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
            * #01 ! PAYMASTR ! Payables Main File for Invoices          *~
            * #02 ! PAYLINES ! Payables Line Item File                  *~
            * #03 ! VENDOR   ! Vendor Master File                       *~
            * #04 ! RCVMASTR ! Receiver Master File                     *~
            * #05 ! RCVLINES ! Receiver Line Item File                  *~
            * #06 !          !                                          *~
            * #07 !          !                                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "PAYMASTR",                                     ~
                        varc,     indexed,  recsize =  350,              ~
                        keypos =    1, keylen =   25

            select #2,  "PAYLINES",                                      ~
                        varc,     indexed,  recsize = 541,               ~
                        keypos =   36, keylen =   28,                    ~
                        alt key  1, keypos =    1, keylen =  63,         ~
                            key  2, keypos =   17, keylen =  47

            select #3,  "VENDOR",                                        ~
                        varc,     indexed,  recsize =   600,             ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup

            select #4,  "RCVMASTR",                                      ~
                        varc,     indexed,  recsize =   150,             ~
                        keypos =    1, keylen =  16

            select #5,  "RCVLINES",                                      ~
                        varc,     indexed,  recsize = 800,               ~
                        keypos =   26, keylen =   52,                    ~
                        alt key  1, keypos =    1, keylen =  69,         ~
                            key  2, keypos =   42, keylen =  36,         ~
                            key  3, keypos =  128, keylen = 24

            call "SHOSTAT" ("Opening File, One Moment Please")
            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))

            mat f1% = zer

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

            for fieldnr% = 1% to 6%
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
                  if keyhit%  = 14% then gosub process_data
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 6% then editpg1
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

        process_data
            call "SHOSTAT" ("Updating PAYMASTR and PAYLINES Data")

            read #1,hold,key = apc_key1$, eod goto L19280
               delete #1
            str(apc_rec1$(1%),26%,16%) = apc_rcv$
            put #1, using L19130, apc_rec1$()
L19130:       FMT CH(200), CH(150)
            write #1, eod goto L19320

            read #2,hold,key = apc_key2$, eod goto L19360
               delete #2
            str(apc_rec2$(1%),1%,16%)  = apc_rcv$
            str(apc_rec2$(1%),33%,3%)  = apc_line_po$
            str(apc_rec2$(1%),73%,25%) = apc_part$
            put #2, using L19220, apc_rec2$()
L19220:       FMT 2*CH(200), CH(141)
            write #2, eod goto L19400

        return clear all
        goto inputmode

L19280:     call "SHOSTAT" ("Error PAYMASTR - READ") : stop
        return clear all
        goto inputmode

L19320:     call "SHOSTAT" ("Error PAYMASTR - WRITE") : stop
        return clear all
        goto inputmode

L19360:     call "SHOSTAT" ("Error PAYLINES - READ") : stop
        return clear all
        goto inputmode

L19400:     call "SHOSTAT" ("Error PAYLINES - WRITE") : stop
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
         "Enter a Valid APC Vendor Code?                               ",~
         "Enter a Valid APC Invoice Number?                            ",~
         "Enter a Valid Detail Invoice Line Item Number?               ",~
         "Enter The Correct APC Receiver Number?                       ",~
         "Enter the Correct Raw Material Part Number?                  ",~
         "Enter the Correct P.O. Line Item No. for Raw Mat'l Part?     "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, apc_key1$, apc_key2$,      ~
                      apc_ven$, apc_ven_d$, apc_line$, apc_inv$,         ~
                      apc_rcv$, apc_part$, apc_rec1$(), apc_rec2$(),     ~
                      apc_line_po$
        return

        REM *************************************************************~
            *************************************************************

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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        REM DATALOAD
        REM RETURN

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

              on fieldnr% gosub L40210,         /* Vendor Code        */  ~
                                L40210,         /* Invoice Number     */  ~
                                L40210,         /* Line Item          */  ~
                                L40210,         /* Receiver No.       */  ~
                                L40210,         /* Part Number        */  ~
                                L40210          /* P.O. Line Item No. */
              goto L40240

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40210:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40240:     accept                                                       ~
               at (01,02),                                               ~
                  "APC Payables Fix Utiltiy Program",                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "APC Vendor Code   :",                        ~
               at (06,25), fac(lfac$(1%)), apc_ven$             , ch(09),~
               at (06,40), fac(hex(84)),   apc_ven_d$           , ch(30),~
                                                                         ~
               at (07,02), "APC Invoice Number:",                        ~
               at (07,25), fac(lfac$(2%)), apc_inv$             , ch(16),~
                                                                         ~
               at (08,02), "APC Invoice Line  :",                        ~
               at (08,25), fac(lfac$(3%)), apc_line$            , ch(03),~
                                                                         ~
               at (09,02), "APC Receiver No.  :",                        ~
               at (09,25), fac(lfac$(4%)), apc_rcv$             , ch(16),~
                                                                         ~
               at (10,02), "APC Part Number   :",                        ~
               at (10,25), fac(lfac$(5%)), apc_part$            , ch(25),~
                                                                         ~
               at (11,02), "APC P.O. Line Item:",                        ~
               at (11,25), fac(lfac$(6%)), apc_line_po$         , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40610
                  call "PRNTSCRN"
                  goto L40240

L40610:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40760     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return

L40760: if fieldnr% > 0% then L40850  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Update Data "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40850:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50160,         /* Vendor Code           */ ~
                              L50270,         /* Invoice Number        */ ~
                              L50400,         /* Invoice Line Item No. */ ~
                              L50600,         /* Receiver Code         */ ~
                              L50640,         /* Raw Mat'l Part No.    */ ~
                              L50680          /* P.O. Line Item No.    */
            return

L50160: REM Vendor Code                          APC_VEN$, APC_VEN_D$
            if apc_ven$ <> " " then goto L50190
               goto L50230
L50190:     read #3,key = apc_ven$, using L50210, apc_ven_d$,             ~
                                                           eod goto L50230
L50210:        FMT POS(10), CH(30)
        return
L50230:     errormsg$ = "(Error) - Invalid Vendor Code (Required)?"
            init(" ") apc_ven$, apc_ven_d$
        return

L50270: REM Invoice Number                       APC_INV$
            if apc_inv$ <> " " then goto L50300
               goto L50360
L50300:     str(apc_key1$,1%,9%)   = apc_ven$
            str(apc_key1$,10%,16%) = apc_inv$
            read #1,key = apc_key1$,using L50340, apc_rec1$(),            ~
                                                          eod goto L50360
L50340:        FMT CH(200), CH(150)
        return
L50360:     errormsg$ = "(Error) - Invalid Invoice Number?"
            init(" ") apc_inv$, apc_key1$, apc_rec1$()
        return

L50400: REM Invoice Line Item Number             APC_LINE$
            if apc_line$ <> " " then goto L50430
               goto L50560
L50430:     convert apc_line$ to apc_line%, data goto L50560

            convert apc_line% to apc_line$,pic(###)
            str(apc_key2$,1%,9%)   = apc_ven$
            str(apc_key2$,10%,16%) = apc_inv$
            str(apc_key2$,26%,3%)  = apc_line$
            read #2,key = apc_key2$,using L50510, apc_rec2$(),            ~
                                                          eod goto L50560
L50510:        FMT 2*CH(200), CH(141)
            apc_rcv$     = str(apc_rec2$(1%),1%,16%)
            apc_part$    = str(apc_rec2$(1%),73%,25%)
            apc_line_po$ = str(apc_rec2$(1%),33%,3%)
        return
L50560:     errormsg$ = "(Error) - Invalid Invoice Line Item?"
            init(" ") apc_line$, apc_key2$, apc_rec2$(), apc_line_po$
        return

L50600: REM APC Receiver Number                  APC_RCV$

        return

L50640: REM APC Raw Mat'l Part Number            APC_PART$

        return

L50680: REM APC P.O. Line Item No. for Part      APC_LINE_PO$
            if apc_line_po$ <> " " then goto L50710
               goto L50750
L50710:     convert apc_line_po$ to apc_line_po%, data goto L50750

            convert apc_line_po% to apc_line_po$,pic(###)
        return
L50750:     errormsg$ = "(Error) - Invalid P.O. Line Item No.?"
            apc_line_po$ = " "
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
