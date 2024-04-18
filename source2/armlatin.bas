        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M  L       AAA   TTTTT  IIIII  N   N   *~
            *  A   A  R   R  MM MM  L      A   A    T      I    NN  N   *~
            *  AAAAA  RRRR   M M M  L      AAAAA    T      I    N N N   *~
            *  A   A  R   R  M   M  L      A   A    T      I    N  NN   *~
            *  A   A  R   R  M   M  LLLLL  A   A    T    IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMLATIN - Allows User to enter Late Notice text.         *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/22/86 ! Original                                 ! ERN *~
            * 03/23/92 ! Rehost.  Text file may allow 1 - 28 lines! KAB *~
            *          !   of text per record.  Monitors record   !     *~
            *          !   length to determine how many.          !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            days%(5), days$(5)3,         /* Days                       */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            hdrs$(2)70,                  /* Screen Headers             */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            pf16$16, pf25$16, pf32$18,   /* PF Key Literals            */~
            readkey$50,                  /* Miscellaneous Read/Plow Key*/~
            txta$(140,1)70, textid$(5)4, /* Text Routine Elements      */~
            text$(5,3)70,   textmsg$60,  /* Text for Screen Display    */~
            textstr$(4)70,               /* Text work Area             */~
            userid$3                     /* Current User Id            */~

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.04 05/19/92 UNIX Compatibility Changes      "
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
            * # 1 ! TXTFILE  ! System Text File                         *~
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11                      ~

            select # 2, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

        call "SHOSTAT" ("Opening Files, One Moment Please")
            rslt$(1) = "REQUIRED" : rslt$(2) = "REQUIRED"
            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            if min(fs%()) < 0% then exit_program
               call "GETUFBRS" addr(#1, readkey$)
               textsize% = val(str(readkey$,,2), 2)
               textsize% = textsize% - 64%
               textsize% = min(textsize%, 210%)

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date  :  call "DATEFMT" (date$)

            edtmessage$  = "Press RETURN to change Days; Position Curso"&~
                           "r and Press PF-25 to Manage Text."

            hdrs$(1) = "After..."
            hdrs$(2) = "Text which appears (First three lines):"


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * No Input Mode - Just Handle Inits and Load Data.          *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, days$(), text$()
            call "TXTFUTIL" (#1, f2%(1), "INTL", textid$(1))
            gosub load_data


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            pf16$ = "(16)Save Data"
            pf25$ = "(25)Manage Text"
            pf32$ = "(32)Exit w/o Save"
            inpmessage$ = edtmessage$
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 25 then gosub manage_text
                  if keyhit%  = 32 then       nosave
                  if keyhit% <>  0 then       editpg1
            gosub'051                   /* Set Input Message           */
                  pf16$, pf25$, pf32$ = " "
L12190:     gosub'101(1%)               /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12190
            gosub'151                   /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12190
                  goto editpg1

        manage_text
            d% = (cursor%(1) - 2%) / 3%
            if d% < 1% or d% > 5% then return

            textmsg$ = "Late Notice Text to Appear After " & days$(d%) & ~
                       " Days"
            call "TXTINSUB" (#1, f2%(1), "018", textmsg$, textid$(d%),   ~
                                                                 txta$())
            gosub load_text_display
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto  exit_program

        nosave
            call "TXTFUTIL" (#1, f2%(1), "INTL", textid$(1))
            goto  exit_program


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051
            inpmessage$ = "Enter Days after which Text is to appear."
            return


        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
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
        load_data
            readkey$ = "ARM.LATE.NOTICE"
            init (hex(ff)) textid$()
            for d% = 1% to 5%
                days%(d%) = d% * 30%
            next d%
            call "READ100" (#2, readkey$, f1%(2))
            if f1%(2) = 1% then get #2 using L30130, days%(), textid$()
L30130:         FMT XX(20), 5*BI(4), 5*CH(4)

            for d% = 1% to 5%
                convert days%(d%) to days$(d%), pic(##0)
                call "TXTFUTIL" (#1, f2%(1), "LOAD", textid$(d%))
                gosub load_text_display
            next d%
            return


        load_text_display   /* Load screen display for D%    */
            if textid$(d%) <> hex(ffffffff) then L30280
L30250:         text$(d%,1) = "** No Text Entered **"
                text$(d%,2), text$(d%,3) = " "
                return
L30280:     readkey$ = "W" & str(userid$) & str(textid$(d%)) & "1" &     ~
                                                                hex(0000)
            init (" ") textstr$()

            call "PLOWALTS" (#1, readkey$, 0%, 9%, f1%(1))
               if f1%(1) = 0% then L30250
            get #1, str(textstr$(4),,64), /* Get at least one          */~
                    str(textstr$(),,textsize%)
            if textsize% >= 210% then L30500
            call "PLOWALTS" (#1, readkey$, 0%, 9%, f1%(1))
               if f1%(1) = 0% then L30500
            get #1, str(textstr$(4),,64), /* Get 2nd only or 3rd O/L 4 */~
                    str(textstr$(), textsize% + 1%, textsize%)
            if textsize% >= 140% then L30500
            call "PLOWALTS" (#1, readkey$, 0%, 9%, f1%(1))
               if f1%(1) = 0% then L30500
            get #1, str(textstr$(4),,64), /* Get 3rd one               */~
                    str(textstr$(), (2%*textsize%) + 1%, textsize%)

L30500:     text$(d%,1) = textstr$(1%)
            text$(d%,2) = textstr$(2%)
            text$(d%,3) = textstr$(3%)
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            readkey$ = "ARM.LATE.NOTICE"
            call "READ101" (#2, readkey$, f1%(2))
            put #2 using L31090, readkey$, days%(), textid$(), " ", " "
L31090:         FMT CH(20), 5*BI(4), 5*CH(4), CH(240), CH(200)
            if f1%(2) = 0% then write #2 else rewrite #2
            call "TXTFUTIL" (#1, f2%(1), "SAV2", textid$(1))
            return


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              str(line2$,62%) = "ARMLATIN: " & str(cms2v$,,8%)
              if fieldnr% = 0% then init(hex(8e)) lfac$()                ~
                               else init(hex(82)) lfac$()

L40110:     accept                                                       ~
               at (01,02), "Late Notice Text Input",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), fac(hex(ac)), hdrs$(1)               , ch(08),~
               at (04,11), fac(hex(ac)), hdrs$(2)               , ch(70),~
               at (05,06), "Days",                                       ~
                                                                         ~
               at (05,02), fac(lfac$( 1)), days$(1)             , ch(03),~
               at (05,11), fac(hex(8c))  , text$(1,1)           , ch(70),~
               at (06,11), fac(hex(8c))  , text$(1,2)           , ch(70),~
               at (07,11), fac(hex(ac))  , text$(1,3)           , ch(70),~
                                                                         ~
               at (08,02), fac(lfac$( 2)), days$(2)             , ch(03),~
               at (08,11), fac(hex(8c))  , text$(2,1)           , ch(70),~
               at (09,11), fac(hex(8c))  , text$(2,2)           , ch(70),~
               at (10,11), fac(hex(ac))  , text$(2,3)           , ch(70),~
                                                                         ~
               at (11,02), fac(lfac$( 3)), days$(3)             , ch(03),~
               at (11,11), fac(hex(8c))  , text$(3,1)           , ch(70),~
               at (12,11), fac(hex(8c))  , text$(3,2)           , ch(70),~
               at (13,11), fac(hex(ac))  , text$(3,3)           , ch(70),~
                                                                         ~
               at (14,02), fac(lfac$( 4)), days$(4)             , ch(03),~
               at (14,11), fac(hex(8c))  , text$(4,1)           , ch(70),~
               at (15,11), fac(hex(8c))  , text$(4,2)           , ch(70),~
               at (16,11), fac(hex(ac))  , text$(4,3)           , ch(70),~
                                                                         ~
               at (17,02), fac(lfac$( 5)), days$(5)             , ch(03),~
               at (17,11), fac(hex(8c))  , text$(5,1)           , ch(70),~
               at (18,11), fac(hex(8c))  , text$(5,2)           , ch(70),~
               at (19,11), fac(hex(ac))  , text$(5,3)           , ch(70),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf25$,                          ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,40), fac(hex(8c)), pf32$,                          ~
               at (24,65), fac(hex(8c)), pf16$,                          ~
                     keys(hex(00010d0f101920)),  key(keyhit%)

               if keyhit% <> 13 then L40590
                  call "MANUAL" ("ARMLATIN")
                  goto L40110

L40590:        if keyhit% <> 15 then L40630
                  call "PRNTSCRN"
                  goto L40110

L40630:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151
            errormsg$ = " "



*        Test Number of Days                         DAYS$
            for d% = 1% to 5%
                if days$(d%) = " " then days$(d%) = "0"
                convert days$(d%) to days%(d%), data goto L50160
                goto L50180
L50160:              errormsg$ = "Invalid entry."
                     goto error_exit
L50180:         if days%(d%) >= 0% and days%(d%) <= 999% then L50210
                     errormsg$ = "Days must be between 0 and 999."
                     goto error_exit
L50210:         if d% = 1% then L50250
                     if days%(d%) > days%(d%-1%) then L50250
                          errormsg$ = "Must be in ascending order."
                          goto error_exit
L50250:         convert days%(d%) to days$(d%), pic(##0)
            next d%
            return

            error_exit
                errormsg$ = "Day #: " & errormsg$
                convert d% to str(errormsg$,5,1), pic(0)
                return


        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
