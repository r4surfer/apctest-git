        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   CCC   H   H  K   K   SSS   TTTTT   AAA   TTTTT          *~
            *  C   C  H   H  K  K   S        T    A   A    T            *~
            *  C      HHHHH  KKK     SSS     T    AAAAA    T            *~
            *  C   C  H   H  K  K       S    T    A   A    T            *~
            *   CCC   H   H  K   K   SSS     T    A   A    T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CHKSTAT  - Monitor status of CHKBUFFR.  Can reset control *~
            *            numbers for reprint, output 'void' markers for *~
            *            destroyed forms, and handles program exit      *~
            *            options.                                       *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/29/89 ! Original                                 ! KEN *~
            * 03/03/94 ! PRR 13086 - Added Check for Max # of Void! JBK *~
            *          !   Checks that can be created in CHKBUFFR.!     *~
            *          !   Miscellaneous add % to integers.       !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

            sub "CHKSTAT" (#1, #2, #3, #4, return%)

        dim                                                              ~
            checkkey$50,                 /* Checkkey                   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            high%(2), low%(2),           /* Buffer Status Info         */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lastno$8,                    /* Last Good Check #          */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            maxvoids$4,                  /* Maximum # of Void Checks   */~
            message$(2)79,               /* Status Messages            */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            number$(2,2)8,               /* Numbers Used               */~
            userid$3,                    /* Current User Id            */~
            voidfrom$8,                  /* Void From (Including)      */~
            voidto$8                     /* Void Thru (Including)      */~

        dim f1%(64)                      /* = 1 if READ was successful */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! CHKBUFFR ! CASH DISBURSEMENTS CHECK GEN BUFFER      *~
            * #02 ! CHKBUF2  ! CASH DISBURSEMENTS CHECK GEN DETAIL BUFF *~
            * #03 ! CHKBUFFR ! CASH DISBURSEMENTS MANUAL CHECK BUFFER   *~
            * #04 ! CHKMASTR ! CASH DISBURSEMENTS CHECK MASTER FILE     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

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

            str(line2$,62%) = "CHKSTAT : " & str(cms2v$,,8%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            gosub dataload
            if message$(2%) <> " " then L10130
            if message$(1%) <> " " then L10110
            call "ASKUSER" (2%, "** ACKNOWLEDGE**", "You Have NO Transact~
        ~ions to Process", " ", "Press Any Key to EXIT")
               goto exit_program
L10110:        return% = -1%
L10120:        goto exit_program
L10130:     gosub'101(0%,0%)
                if keyhit% =  0% then L10180
                if keyhit% =  9% then L10120
                if keyhit% = 10% then L10110
                if keyhit% =  8% then L10170
                if keyhit% =  1% then gosub startover
                   goto L10130
L10170:            return% = 1%
                   goto exit_program
L10180:     for fieldnr% = 1% to  2%
L10190:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10310
L10210:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10300
L10240:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10210
                         if fieldnr% = 1% then L10190
                         goto L10240
L10300:               if keyhit% <> 0% then       L10210
L10310:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10210
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
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  2% then editpg1
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
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Last Good Check #      */~
                              L20200          /* Void Thru              */
            return
L20100: REM Def/Enable Last Good Check #           LASTNO$
            if lastno$ <> " " then return
            if number$(2%,2%) <> " " then lastno$ = number$(2%,2%)
            return

L20200: REM Def/Enable Void Thru (Including)       VOIDTO$
            if voidfrom$ = " " then voidfrom$ = "NONE"
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28055
            if edit% > 0% then inpmessage$ = edtmessage$ else            ~
               inpmessage$ = "Press ENTER to Continue or Select Processin~
        ~g Option Below"
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Last Good Check #, or NONE.                            ",~
         "Enter Range to Void (Inclusive), or NONE.                    "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, message$(), number$(),     ~
                      lastno$, voidto$, voidfrom$
            mat high% = con
            mat low%  = (999999999) * high%
            mat high% = (-1)*high%
            return% = 0%

            maxvoids% = 9999%
            init (hex(00))  plowkey$
            call "PLOWALTS" (#1, plowkey$, 1%, 0%, f1%(1%))
                if f1%(1%) = 0% then L29130
            get #1 using L29100, highcheck$
L29100:         FMT POS(4), CH(4)
            convert highcheck$ to highcheck%, data goto L29140
            maxvoids% = min(maxvoids%, 9999% - highcheck%)
L29130:     convert maxvoids% to maxvoids$, pic(###0)
L29140:     return


        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
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
        dataload
            plowkey$ = userid$

L30080:     call "PLOWNEXT" (#1, plowkey$, 3%, f1%(1%))
               if f1%(1%) = 0% then set_messages
            get #1 using L30110, checkkey$
L30110:         FMT POS(15), CH(17)
            if str(checkkey$,10%,1%) = "X" then unassigned

*       ASSIGNED
            convert str(checkkey$,10%,8%) to temp%
            if temp% < low%(2%) then low%(2%) = temp%
            if temp% > high%(2%) then high%(2%) = temp%
            goto L30080

        unassigned
            convert str(checkkey$,11%,7%) to temp%
            if temp% < low%(1%) then low%(1%) = temp%
            if temp% > high%(1%) then high%(1%) = temp%
            goto L30080

        set_messages
            if high%(1%) < low%(1%) then L30300
            init ("X") str(number$(),1%,16%)
            convert low%(1%) to str(number$(1%,1%),2%,7%), pic(0000000)
            convert high%(1%) to str(number$(1%,2%),2%,7%), pic(0000000)
            put message$(1%) using L30290, number$(1%,1%), number$(1%,2%)
L30290: % Control Numbers ######## thru ######## are Unassigned.
L30300:     if high%(2%) < low%(2%) then return
            convert low%(2%) to str(number$(2%,1%),1%,8%), pic(00000000)
            convert high%(2%) to str(number$(2%,2%),1%,8%), pic(00000000)
            put message$(2%) using L30320, number$(2%,1%), number$(2%,2%)
L30320: % Check Numbers ######## thru ######## have been Assigned.
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            plowkey$ = userid$

L31080:     call "PLOWNXT1" (#1, plowkey$, 3%, f1%(1%))
               if f1%(1%) = 0% then delete_marker
            get #1 using L31110, checkkey$
L31110:         FMT POS(15), CH(17)
            if str(checkkey$,10%,1%) = "X" then L31080

            convert str(checkkey$,10%,8%) to temp%
            if temp% <= lastgood% then L31080
            if str(checkkey$,1%,9%) <> "**VOID** " then L31200
               delete #1
               goto L31080

L31200:     str(temp$,,8%) = "X000" & str(key(#1),4%,4%)
            put #1 using L31220, temp$
L31220:         FMT POS(24), CH(8)
            rewrite #1

            str(checkkey$,18%) = " "
L31260:     call "PLOWNXT1" (#2, checkkey$, 17%, f1%(2%))
               if f1%(2%) = 0% then L31080
            delete #2
            put #2 using L31300, temp$
L31300:         FMT POS(10), CH(8)
            write #2
            goto L31260

        delete_marker
            if voidfrom% > voidthru% then return
               convert voidfrom% to temp$, pic(00000000)

            call "REDALT0" (#4, temp$, 3%, f1%(4%))
               if f1%(4%) <> 0% then L31590
            call "REDALT0" (#3, temp$, 3%, f1%(3%))
               if f1%(3%) <> 0% then L31590
            call "REDALT0" (#1, temp$, 2%, f1%(1%))
               if f1%(1%) <> 0% then L31590

            str(temp$,1%,1%) = "M"
            call "REDALT0" (#4, temp$, 3%, f1%(4%))
               if f1%(4%) <> 0% then L31590
            call "REDALT0" (#3, temp$, 3%, f1%(3%))
               if f1%(3%) <> 0% then L31590
            str(temp$,1%,1%) = "0"

                voidkey$, voidrkey$ = " "
                call "FMTKEYCK" (#1,voidkey$,voidrkey$)

                write  #1, using L31620,                                  ~
                           voidkey$, voidrkey$, "**VOID** ", temp$,      ~
                           date, 0, " ", " ", 0, " "

L31590:     voidfrom% = voidfrom% + 1%
            goto delete_marker

L31620:                         FMT /* TO USE IN WRITING HEADER        */~
                                    2*CH(07),      /* KEY + REVERSE KEY*/~
                                    CH(9),         /* VENDOR CODE      */~
                                    CH(8),         /* CHECK NUMBER     */~
                                    CH(6),         /* CHECK DATE       */~
                                    PD(14,4),      /* DISCOUNT AMOUNT  */~
                                    2*CH(9),       /* DISCOUNT,CASH ACC*/~
                                    PD(14,4),      /* TOTAL CHECK AMT  */~
                                    CH(29)         /* FILL IT OUT      */

            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

        FMT                 /* FILE: CHKBUFFR                          */~
            CH(3),          /* User-ID of specific user                */~
            CH(4),          /* check-no for use as key on buffers      */~
            CH(7),          /* chkbuffr reverse key                    */~
            CH(9),          /* Vendor code                             */~
            CH(8),          /* check number                            */~
            CH(6),          /* check date                              */~
            PD(14,4),       /* discount amount                         */~
            CH(9),          /* discounts taken account                 */~
            CH(9),          /* cash in bank account                    */~
            PD(14,4),       /* net check amount                        */~
            CH(29)          /* Unused Space                            */~

        FMT                 /* FILE: CHKBUF2                           */~
            CH(9),          /* Vendor Code                             */~
            CH(8),          /* check number                            */~
            CH(3),          /* chkbuf2 sequence number                 */~
            CH(16),         /* Invoice number                          */~
            CH(9),          /* debit account                           */~
            CH(1),          /* debit account type                      */~
            PD(14,4),       /* debit amount                            */~
            PD(14,4),       /* discount taken this invoice             */~
            CH(6),          /* invoice date                            */~
            CH(4),          /* 1099 Category Code                      */~
            CH(1),          /* Status of this line, modifiable or froze*/~
            CH(27)          /* Unused Space                            */~

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
              on fieldnr% gosub L40160,         /* Last Good Check # */   ~
                                L40160          /* Void Thru         */
              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02),                                               ~
                  "Accounts Payable Check Print Status",                 ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Last Good Check #",                          ~
               at (06,30), fac(lfac$( 1%)), lastno$             , ch(08),~
                                                                         ~
               at (07,02), "Void Check # Range",                         ~
               at (07,30), fac(lfac$( 2%)), voidfrom$           , ch(08),~
               at (07,40), "to",                                         ~
               at (07,44), fac(lfac$( 2%)), voidto$             , ch(08),~
                                                                         ~
               at (10,02), fac(hex(84)),   message$(1%)         , ch(79),~
               at (11,02), fac(hex(84)),   message$(2%)         , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40480
                  call "MANUAL" ("CHKPRINT") : goto L40190

L40480:        if keyhit% <> 15% then L40510
                  call "PRNTSCRN" : goto L40190

L40510:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40870     /*  Input Mode             */
        if fieldnr% > 0% then L40740
            pf$(1%) = "(1)Start Over          (8)Exit/Post Tran" &       ~
                      "sactions               (13)Instructions"
            pf$(2%) = "                       (9)Exit/No Postin" &       ~
                      "g                      (15)Print Screen"
            pf$(3%) = "                       (10)Exit to Check" &       ~
                      "# Assignment                           "
            pfkeys$ = hex(01ffffffffffff08090affff0dff0fff00)
            if message$(1%) <> " " then return
                str(pf$(3%),24%)  = " "  :  str(pfkeys$,10%,1%) = hex(ff)
            return

L40740:     pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0fff00)
            if fieldnr% = 1% then L40830
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L40830:     if fieldnr% > 2% then L40850
                str(pf$(2%),18%,26%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L40850:     return

L40870: if fieldnr% > 0% then L40970  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return

L40970:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* Last Good Check #      */~
                              L50200          /* Void Thru              */
            return
L50100: REM Test for Last Good Check #            LASTNO$
            if lastno$ = " " then lastno$ = "NONE"
            if lastno$ <> "NONE" then L50140
               lastgood% = 0%
               return
L50140:     convert lastno$ to lastgood%, data goto L50170
            convert lastgood% to lastno$, pic(00000000)
            return
L50170:     errormsg$ = "INVALID NUMERIC ENTRY"
            return

L50200: REM Test for Void Thru (Including)        VOIDTO$
            if voidfrom$ <> "NONE" then L50230
               voidfrom% = -1% : voidto$ = " " : voidthru% = -2 : return
L50230:     convert voidfrom$ to voidfrom%, data goto L50360
            if voidto$ = " " then voidto$ = voidfrom$
            convert voidto$ to voidthru%, data goto L50360
            if voidfrom% > voidthru% then L50340
            if voidthru% - voidfrom% <= maxvoids% then L50310
               errormsg$ = "Maximum number of Checks that can be "     & ~
                           "voided is " & maxvoids$
               return
L50310:     convert voidfrom% to voidfrom$, pic(00000000)
            convert voidthru% to voidto$, pic(00000000)
            return
L50340:        errormsg$ = "Invalid Range Entry"
               return
L50360:     errormsg$ = "Invalid Numeric Entry"
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            end
