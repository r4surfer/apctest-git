        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  ZZZZZ  CCC   M   M   SSS   M   M   AAA   IIIII  N   N    *~
            *     Z  C   C  MM MM  S      MM MM  A   A    I    NN  N    *~
            *    Z   C      M M M   SSS   M M M  AAAAA    I    N N N    *~
            *   Z    C   C  M   M      S  M   M  A   A    I    N  NN    *~
            *  ZZZZZ  CCC   M   M   SSS   M   M  A   A  IIIII  N   N    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ZCMSMAIN - Allows A/C/D of Module Administrator List for  *~
            *            Module specified. SSL sub called by CMSMAINP.  *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/26/85 ! ORIGINAL                                 ! ERN *~
            * 10/28/92 ! Change GETUFBF1 to GETIFBS1.             ! JDH *~
            * 04/14/94 ! PRR 12147,12601,12824,10326. CMS2V$ fixed! JDH *~
            * 09/23/96 ! Millie Date conversion - USERLCMS to 600 ! DER *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "ZCMSMAIN" (module$, moduledescr$)

        dim                                                              ~
            date$8,                      /* Date for Screen Display    */~
            errormsg$79,                 /* Error Message              */~
            hdr1$4, hdr2$30,             /* Column Headings            */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Screen FACs                */~
            module$3,                    /* Module to maintain list for*/~
            moduledescr$30,              /* Module Description         */~
            readkey$20,                  /* Read Key                   */~
            scrn2$79,                    /* Second Line on screen      */~
            syslib$8,                    /* System Library Name        */~
            sysvol$6,                    /* System Volume Name         */~
            user$(15)33,                 /* User ID(3), Name(30)       */~
            userid$3                     /* User ID of current user    */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            * #2  ! USERLCMS ! Program Access Control User Info file    *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            select #2,  "USERLCMS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos =    1, keylen = 3,                       ~
                        alt key 1, keypos =  4, keylen = 30, dup

*        See if this person has any right in here before we get going
            call "CMSMACHK" (module$, user$(1), user$(2))
            if user$(1) = "Y" or user$(2) = "Y" then L02260 else L65000

L02260:     call "OPENCHCK" (#1, 0%, f2%(1),   0%, " ")

            call "EXTRACT" addr("ID", userid$,"XL",syslib$,"XV",sysvol$)
            call "READFDR" addr("USERLCMS", syslib$, sysvol$, 0%, u3%)
            if u3% <> 0% then L02395
            call "PUTNAMES" addr(#2, "USERLCMS", syslib$, sysvol$)
            call "WORKOPN2" (#2, "SHARE", 0%, f2%(2))
        REM     OPEN NOGETPARM, #2, SHARED, FILE    = "USERLCMS",        ~
                                            LIBRARY = SYSLIB$,           ~
                                            VOLUME  = SYSVOL$
         /* See if USERLCMS really got opened    */
            call "GETUFBS1" addr(#2, open%)
            if f2%(2) = 0% then L09000
            if open%  = 1% then L09000
L02395:         close ws
                call "ZASKUSER" (keyhit%, "FILE OPEN", " ",              ~
                     "Unable to open file USERLCMS.  Press RETURN.", " ")
                goto L65000


L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date : call "DATEFMT" (date$)

            scrn2$ = "Module: " & moduledescr$ & " (" & module$ & ")"
            str(scrn2$, 62%) = "CMSMAINP: " & cms2v$
            hdr1$ = "User" : hdr2$ = "User's Name"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * Justs loads 'em up - Edit Mode in full screen only.       *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$

            gosub L30000


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

L11060:     gosub'101(0%, 0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 32 then       L65000
                  if keyhit% <>  0 then       L11060
            fieldnr% = 0%

L11130:     gosub'101(fieldnr%, 1%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11130
            gosub'151
                  if errormsg$ <> " " then L11130
            goto L11060

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            * --------------------------------------------------------- *~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub L31000
            goto  L65000

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            * --------------------------------------------------------- *~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%  /* PUT MSG AREA AT BOTTOM OF SCREEN  */
            call "ZSTARTOV" (keyhit1%)
                if keyhit1% = 1% then return

                return clear all
                goto inputmode

L30000: REM *************************************************************~
            *              L O A D   D A T A                            *~
            * --------------------------------------------------------- *~
            * Get current list from file and stick names with IDs.      *~
            *************************************************************

            init (" ") user$()
            readkey$ = "MODULE_ADMIN." & module$
            call "READ100" (#1, readkey$, f1%(1))
            if f1%(1) = 0% then return

                get #1 using L30120, user$()
L30120:              FMT XX(20), 15*CH(3)

            for x% = 1% to 15%
                if user$(x%) = " " then L30200
                     call "READ100" (#2, user$(x%), f1%(2))
                     if f1%(2) = 0% then L30200
                          get #2 using L30190, str(user$(x%),4)
L30190:                        FMT XX(3), CH(30)
L30200:     next x%

            return


L31000: REM *************************************************************~
            *                S A V E   D A T A                          *~
            * --------------------------------------------------------- *~
            * Puts the list on disk.                                    *~
            *************************************************************

            readkey$ = "MODULE_ADMIN." & module$
            call "READ101" (#1, readkey$, f1%(1))
            put #1 using L31090, readkey$, user$(), " ", " "
L31090:         FMT CH(20), 15*CH(3), CH(218), CH(217)
            if f1%(1) = 1% then rewrite #1 else write #1
            return


        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            * --------------------------------------------------------- *~
            * Inputs Document for the first time.                       *~
            *************************************************************

        deffn'101(fieldnr%, mode%)   /* MODE%- 0 display, 1 edit   */
         if mode% > 0% then L40110
            init(hex(84)) lfac$()
            inpmessage$  = "To EDIT list Press RETURN.  Press PF-32" &   ~
                           " to EXIT maintenance."
            goto L40220

L40110:  if fieldnr% > 0% then L40160
            init(hex(81)) lfac$()
            inpmessage$  = "Make modifications and Press RETURN."
            goto L40220

L40160:     init(hex(8c)) lfac$()
            lfac$(fieldnr%) = hex(81)
            inpmessage$ = "Please make error correction then press" &    ~
                          " RETURN."


L40220:   accept                                                         ~
            at (01,02), "Manage Module Administrator List",              ~
            at (01,67), "Date:",                                         ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), scrn2$                    , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
            at (05,20), fac(hex(ac)), str(hdr1$)                , ch(04),~
            at (05,27), fac(hex(ac)), str(hdr2$)                , ch(30),~
                                                                         ~
            at (06,20), fac(lfac$( 1)), str(user$(01),,3)       , ch(03),~
            at (06,27), fac(  hex(8c)), str(user$(01), 4)       , ch(30),~
            at (07,20), fac(lfac$( 2)), str(user$(02),,3)       , ch(03),~
            at (07,27), fac(  hex(8c)), str(user$(02), 4)       , ch(30),~
            at (08,20), fac(lfac$( 3)), str(user$(03),,3)       , ch(03),~
            at (08,27), fac(  hex(8c)), str(user$(03), 4)       , ch(30),~
            at (09,20), fac(lfac$( 4)), str(user$(04),,3)       , ch(03),~
            at (09,27), fac(  hex(8c)), str(user$(04), 4)       , ch(30),~
            at (10,20), fac(lfac$( 5)), str(user$(05),,3)       , ch(03),~
            at (10,27), fac(  hex(8c)), str(user$(05), 4)       , ch(30),~
            at (11,20), fac(lfac$( 6)), str(user$(06),,3)       , ch(03),~
            at (11,27), fac(  hex(8c)), str(user$(06), 4)       , ch(30),~
            at (12,20), fac(lfac$( 7)), str(user$(07),,3)       , ch(03),~
            at (12,27), fac(  hex(8c)), str(user$(07), 4)       , ch(30),~
            at (13,20), fac(lfac$( 8)), str(user$(08),,3)       , ch(03),~
            at (13,27), fac(  hex(8c)), str(user$(08), 4)       , ch(30),~
            at (14,20), fac(lfac$( 9)), str(user$(09),,3)       , ch(03),~
            at (14,27), fac(  hex(8c)), str(user$(09), 4)       , ch(30),~
            at (15,20), fac(lfac$(10)), str(user$(10),,3)       , ch(03),~
            at (15,27), fac(  hex(8c)), str(user$(10), 4)       , ch(30),~
            at (16,20), fac(lfac$(11)), str(user$(11),,3)       , ch(03),~
            at (16,27), fac(  hex(8c)), str(user$(11), 4)       , ch(30),~
            at (17,20), fac(lfac$(12)), str(user$(12),,3)       , ch(03),~
            at (17,27), fac(  hex(8c)), str(user$(12), 4)       , ch(30),~
            at (18,20), fac(lfac$(13)), str(user$(13),,3)       , ch(03),~
            at (18,27), fac(  hex(8c)), str(user$(13), 4)       , ch(30),~
            at (19,20), fac(lfac$(14)), str(user$(14),,3)       , ch(03),~
            at (19,27), fac(  hex(8c)), str(user$(14), 4)       , ch(30),~
            at (20,20), fac(lfac$(15)), str(user$(15),,3)       , ch(03),~
            at (20,27), fac(  hex(8c)), str(user$(15), 4)       , ch(30),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,03), "(1)Start Over",                                 ~
            at (22,65), "(13)Instructions",                              ~
            at (23,65), "(15)Print Screen",                              ~
            at (24,65), "(16)SAVE DATA   ",                              ~
            at (24,45), "(32)Exit Routine",                              ~
                                                                         ~
                keys(hex(00010d0f1020)),                                 ~
                key (keyhit%)

                if keyhit% <> 13 then L40650
                     close ws
                     call "ZMANUAL" ("CMSMAINP")
                     goto L40220

L40650:         if keyhit% <> 15 then return
                     close ws
                     call "ZPRNTSCR"
                     goto L40220

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Page 1.                        *~
            *************************************************************

            deffn'151
                errormsg$ = " "

            for fieldnr% = 1% to 15%
                x% = fieldnr%
                if str(user$(x%),,3) <> " " then L50300
                     user$(x%) = " "
                     goto L50360
L50300:         call "READ100" (#2, str(user$(x%),,3), f1%(2))
                if f1%(2) = 1% then L50340
                     errormsg$ = "Invalid User ID. Please re-enter."
                     return
L50340:         get #2 using L50350, str(user$(x%),4)
L50350:              FMT XX(3), CH(30)
L50360:     next fieldnr%


*         Now make the display presentable.
            for x% = 1% to 15%
                if user$(x%) = " " then user$(x%) = hex(ff)
            next x%

            call "SORT" addr(str(user$()), 15%, 33%)

            for x% = 1% to 15%
                if user$(x%) = hex(ff) then user$(x%) = " "
            next x%

            return


L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * Closes all the files currently open, and also displays    *~
            * a message (ONLY if in foreground) while linking to the    *~
            * next program.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            for u3% = 1 to 64
                if f2%(u3%) = 0 then close # u3%
            next u3%
            close ws
            end
