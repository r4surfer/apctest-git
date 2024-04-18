        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   SSS    AAA    SSS   U   U  M   M  RRRR   IIIII  ZZZZZ   *~
            *  S      A   A  S      U   U  MM MM  R   R    I       Z    *~
            *   SSS   AAAAA   SSS   U   U  M M M  RRRR     I      Z     *~
            *      S  A   A      S  U   U  M   M  R  R     I     Z      *~
            *   SSS   A   A   SSS    UUU   M   M  R   R  IIIII  ZZZZZ   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SASUMRIZ - Allows the user to specify the SASUMRY# file(s)*~
            *            to fix/restore/rebuild, etc. from the SADETAIL *~
            *            file. Criteria are selected here and the       *~
            *            resulting SADETAIL records are passed to the   *~
            *            subroutine SAPOSTFX (a clone of SAPOSTSB).     *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/21/91 ! Original                                 ! JIM *~
            * 08/21/96 ! Century date conversion                  ! DER *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            blankdate$8,                 /* blank unfmt date           */~
            date$8,                      /* Date for screen display    */~
            daterng$(4)10,               /* SADETAIL Date Range        */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            filedescrs$(10)35,           /* File Descriptions          */~
            filefix$(10)1,               /* SASUMRY# File to Fix (1-10)*/~
            files$(10)2,                 /* Summary File Definitions   */~
            groups$(10)14,               /* Grouping Descriptions      */~
            hdr2$(4)35,                  /* Second Screen Headings     */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            sadetail_rec$(2)150,         /* SADETAIL records           */~
            userid$3                     /* Current User Id            */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * #01 ! SADETAIL ! Sales Analysis Detail Transactions       *~
            * #02 ! SYSFILE2 ! Caelus Management System Information     *~
            * #03 ! HNYMASTR ! Inventory Master File                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SADETAIL",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   8

            select #02, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #03, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 0%, rslt$(02%))
            call "OPENCHCK" (#03, fs%(03%), f2%(03%), 0%, rslt$(03%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date : call "DATEFMT" (date$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            edtmessage$ = "To modify displayed values, position cursor "&~
                "to desired value & press (RETURN)."
            str(line2$,62%) = "SASUMRIZ: " & str(cms2v$,,8%)

*        See if this User is a Data Base or Module Administrator.
            call "CMSMACHK" ("SA", lfac$(1%), lfac$(2%))
            if lfac$(1%) = "Y" or lfac$(2%) = "Y" then L09200
                call "ASKUSER" (0%, "*** SECURITY CHECK ***", " ",       ~
                     "You must be a Data Base or A/R Module Administrat"&~
                     "or to run this program.", " ")
                goto exit_program

L09200
*        Set some variables.
            groups$( 1%) = "Part Number"
            groups$( 2%) = "Part Category"
            groups$( 3%) = "Account"
            groups$( 4%) = "Ship-to"
            groups$( 5%) = "Customer Type"
            groups$( 6%) = "Store"
            groups$( 7%) = "Sales Region"
            groups$( 8%) = "Salesman"

            hdr2$(1%) = "FILE"
            hdr2$(2%) = "GROUPS"
            hdr2$(3%) = "FILE DESCRIPTION"
            hdr2$(4%) = "    G R O U P S"

            call "READ100" (#02, "SWITCHS.SA", f1%(2%))    /* SYSFILE2 */
            if f1%(2%) <> 0% then goto L09420
                call "ASKUSER" (0%, "*** SYSFILE2 RECORD MISSING ***",   ~
                     "The S/A Definition record is missing from SYSFILE"&~
                     "2.", " ", "Press (RETURN) to acknowledge.")
                goto exit_program

L09420
*        Get S/A definition data and set up for screen.
            get #02 using L09440, files$()                   /* SYSFILE2 */
L09440:         FMT POS(308), 10*CH(2)
            if str(files$()) <> " " then goto L09520
                call "ASKUSER" (0%, "*** NO FILES DEFINED ***",          ~
                     "There are NO SASUMRY# files defined!.", "Run SAFL"&~
                     "AGS to define a set of files; then re-run this pr"&~
                     "ogram.", "Press (RETURN) to exit to menu.")
                goto exit_program

L09520:     for i% = 1% to 10%
                filedescrs$(i%) = "* * * U N D E F I N E D * * *"
                if files$(i%) = " " then L09600
                     convert str(files$(i%),1%,1%) to i1%, data goto L09600
                     filedescrs$(i%) = groups$(i1%)
                     convert str(files$(i%),2%,1%) to i1%, data goto L09600
                     filedescrs$(i%) = filedescrs$(i%) & ", "            ~
                          & groups$(i1%)
L09600:     next i%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 2%
                gosub'051(fieldnr%)               /* Default / Enables */
                     if enabled% = 0% then L10160
L10120:         gosub'101(fieldnr%, 1%)           /* Display / Accept  */
                     if keyhit% =  1% then gosub startover
                     if keyhit% = 16% and fieldnr% = 1% then exit_program
                     if keyhit% <> 0% then L10120
L10160:         gosub'151(fieldnr%)      /* Edit Field for Valid Entry */
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
                if keyhit%  = 16% then process_sadetail
                if keyhit% <>  0% then editpg1
L11120:     if cursor%(1) <  6% then editpg1
            if cursor%(1) > 18% then editpg1
            if cursor%(1) =  6% then fieldnr% = 1% else fieldnr% = 2%
            if lastfieldnr% = fieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled% =  0% then editpg1
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " " then L11180
                lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            * Main processing occurs here.                              *~
            *************************************************************

        process_sadetail
            call "SHOSTAT" ("Processing SADETAIL; Fixing SASUMRY file(s"&~
                "). Please stand by.")
            plowkey$ = xor plowkey$ /* Start SADETAIL at the beginning */
            primed% = 0%                         /* Start with READ102 */

L12100
*        PLOW the SADETAIL file and call the subroutine as appropriate.
            if primed% = 0%                                              ~
                then call "READ102" (#01, plowkey$, f1%(1%))             ~
                else call "READNEXT" (#01, f1%(1%))
            primed% = 1%                       /* READNEXT from now on */
            if f1%(1%) = 0% then goto inputmode/* All done-next please */
            get #01, str(sadetail_rec$()) /* Pass the w-h-o-l-e record */

*        Try to eliminate the record based on the Posting Date.
            if str(sadetail_rec$(),9%,6%) < daterng$(3%) then goto L12100
            if str(sadetail_rec$(),9%,6%) > daterng$(4%) then goto L12100

*        Can't eliminate it. Pass it to the subroutine for fixing.
*        Pass the SADETAIL record once per selected SASUMRY# file.
            for poo% = 1% to 10%
                if filefix$(poo%) <> " " then call "SAPOSTFX"            ~
                     (sadetail_rec$(), #02, #03, poo%)
            next poo%
            goto L12100

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20120,         /* SADETAIL Date Range    */~
                              L20160          /* SASUMRY# File(s) to fix*/
            return

L20120: REM Def/Enable SADETAIL Date Range         DATERNG$()
            if str(daterng$()) = " " or ~
               str(daterng$()) = blankdate$ then daterng$(1%) = "ALL"
            return

L20160: REM Def/Enable SASUMRY# File to Fix (1-10) FILEFIX$()
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
         "Enter SADETAIL Posting Date Range, 'FIRST', 'LAST' or 'ALL'. ",~
         "Select the SASUMRY# File(s) with a non-blank. Undefined files w~
        ~ill be ignored."

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$, filefix$()
            for i% = 1% to 4%
               daterng$(i%) = blankdate$
            next i%
            call "DATFMTC" (daterng$(1%))
            call "DATFMTC" (daterng$(2%))

            call "ALLFREE"
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            gosub'050(1%, fieldnr%)
            gosub set_pf1
            if fieldnr% > 0%                                             ~
                then init(hex(8c)) lfac$()                               ~
                else init(hex(86)) lfac$()
            on fieldnr% gosub L40170,         /* SADETAIL Date Range  */  ~
                              L40170          /* SASUMRY# File to fix */
            goto L40200

            lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:     lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
            lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
                at (01,02), "S/A Summary File(s) Construction Criteria", ~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (04,02), fac(hex(94)), errormsg$             , ch(79),~
                                                                         ~
                at (06,02), "SADETAIL Date Range",                       ~
                at (06,30), fac(lfac$( 1%)), daterng$(1%)       , ch(10),~
                at (06,42), "to",                                        ~
                at (06,45), fac(lfac$( 1%)), daterng$(2%)       , ch(10),~
                                                                         ~
                at (08,02), fac(hex(ac)), str(hdr2$(1%),, 4%),           ~
                at (08,07), fac(hex(ac)), str(hdr2$(2%),, 6%),           ~
                at (08,14), fac(hex(ac)), str(hdr2$(3%),,35%),           ~
                at (08,55), "!",                                         ~
                at (08,58), fac(hex(ac)), str(hdr2$(4%),,22%),           ~
                at (09,02), "0 ", at (10,02), "1 ", at (11,02), "2 ",    ~
                at (12,02), "3 ", at (13,02), "4 ", at (14,02), "5 ",    ~
                at (15,02), "6 ", at (16,02), "7 ", at (17,02), "8 ",    ~
                at (18,02), "9 ",                                        ~
                at (09,05), fac(lfac$( 2%)), filefix$( 1%)      , ch(01),~
                at (10,05), fac(lfac$( 2%)), filefix$( 2%)      , ch(01),~
                at (11,05), fac(lfac$( 2%)), filefix$( 3%)      , ch(01),~
                at (12,05), fac(lfac$( 2%)), filefix$( 4%)      , ch(01),~
                at (13,05), fac(lfac$( 2%)), filefix$( 5%)      , ch(01),~
                at (14,05), fac(lfac$( 2%)), filefix$( 6%)      , ch(01),~
                at (15,05), fac(lfac$( 2%)), filefix$( 7%)      , ch(01),~
                at (16,05), fac(lfac$( 2%)), filefix$( 8%)      , ch(01),~
                at (17,05), fac(lfac$( 2%)), filefix$( 9%)      , ch(01),~
                at (18,05), fac(lfac$( 2%)), filefix$(10%)      , ch(01),~
                at (09,55), "!", at (10,55), "!",  at (11,55), "!",      ~
                at (12,55), "!", at (13,55), "!",  at (14,55), "!",      ~
                at (15,55), "!", at (16,55), "!",  at (17,55), "!",      ~
                at (18,55), "!",                                         ~
                at (09,58), " 1", at (10,58), " 2", at (11,58), " 3",    ~
                at (12,58), " 4", at (13,58), " 5", at (14,58), " 6",    ~
                at (15,58), " 7", at (16,58), " 8",                      ~
                at (09,62), fac(hex(8c)), groups$(1%),                   ~
                at (10,62), fac(hex(8c)), groups$(2%),                   ~
                at (11,62), fac(hex(8c)), groups$(3%),                   ~
                at (12,62), fac(hex(8c)), groups$(4%),                   ~
                at (13,62), fac(hex(8c)), groups$(5%),                   ~
                at (14,62), fac(hex(8c)), groups$(6%),                   ~
                at (15,62), fac(hex(8c)), groups$(7%),                   ~
                at (16,62), fac(hex(8c)), groups$(8%),                   ~
                                                                         ~
                at (09,08), fac(hex(8c)), str(files$(1%),1%,1%) , ch(01),~
                at (09,11), fac(hex(8c)), str(files$(1%),2%,1%) , ch(01),~
                at (10,08), fac(hex(8c)), str(files$(2%),1%,1%) , ch(01),~
                at (10,11), fac(hex(8c)), str(files$(2%),2%,1%) , ch(01),~
                at (11,08), fac(hex(8c)), str(files$(3%),1%,1%) , ch(01),~
                at (11,11), fac(hex(8c)), str(files$(3%),2%,1%) , ch(01),~
                at (12,08), fac(hex(8c)), str(files$(4%),1%,1%) , ch(01),~
                at (12,11), fac(hex(8c)), str(files$(4%),2%,1%) , ch(01),~
                at (13,08), fac(hex(8c)), str(files$(5%),1%,1%) , ch(01),~
                at (13,11), fac(hex(8c)), str(files$(5%),2%,1%) , ch(01),~
                at (14,08), fac(hex(8c)), str(files$(6%),1%,1%) , ch(01),~
                at (14,11), fac(hex(8c)), str(files$(6%),2%,1%) , ch(01),~
                at (15,08), fac(hex(8c)), str(files$(7%),1%,1%) , ch(01),~
                at (15,11), fac(hex(8c)), str(files$(7%),2%,1%) , ch(01),~
                at (16,08), fac(hex(8c)), str(files$(8%),1%,1%) , ch(01),~
                at (16,11), fac(hex(8c)), str(files$(8%),2%,1%) , ch(01),~
                at (17,08), fac(hex(8c)), str(files$(9%),1%,1%) , ch(01),~
                at (17,11), fac(hex(8c)), str(files$(9%),2%,1%) , ch(01),~
                at (18,08), fac(hex(8c)), str(files$(10%),1%,1%), ch(01),~
                at (18,11), fac(hex(8c)), str(files$(10%),2%,1%), ch(01),~
                                                                         ~
                at (09,14), fac(hex(8c)), filedescrs$( 1%)      , ch(30),~
                at (10,14), fac(hex(8c)), filedescrs$( 2%)      , ch(30),~
                at (11,14), fac(hex(8c)), filedescrs$( 3%)      , ch(30),~
                at (12,14), fac(hex(8c)), filedescrs$( 4%)      , ch(30),~
                at (13,14), fac(hex(8c)), filedescrs$( 5%)      , ch(30),~
                at (14,14), fac(hex(8c)), filedescrs$( 6%)      , ch(30),~
                at (15,14), fac(hex(8c)), filedescrs$( 7%)      , ch(30),~
                at (16,14), fac(hex(8c)), filedescrs$( 8%)      , ch(30),~
                at (17,14), fac(hex(8c)), filedescrs$( 9%)      , ch(30),~
                at (18,14), fac(hex(8c)), filedescrs$(10%)      , ch(30),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then L41090
                call "MANUAL" ("SASUMRIZ") : goto L40200

L41090:     if keyhit% <> 15% then L41120
                call "PRNTSCRN" : goto L40200

L41120:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf1
        if edit% = 2% then L41290     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L41270
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L41270:     return

L41290: if fieldnr% > 0% then L41380  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Summarize   "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L41380:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
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
            on fieldnr% gosub L50120,         /* SADETAIL Date Range    */~
                              L50330          /* SASUMRY# File to fix   */
            return

L50120: REM Test for SADETAIL Date Range          DATERNG$()
            if daterng$(1%) = "ALL" then goto L50230
            if daterng$(1%) = "FIRST" then goto L50180
                call "DATEOKC" (daterng$(1%), u3%, errormsg$)
                if errormsg$ <> " " then return
L50180:     if daterng$(2%) = "LAST" then goto L50230
                call "DATEOKC" (daterng$(2%), u3%, errormsg$)
                if errormsg$ <> " " then return

L50230
*        Test to make sure there is an actual FROM - TO range.
            if daterng$(1%) <> "ALL" and daterng$(1%) <> "FIRST" then    ~
               call "DATUFMTC" (daterng$(1%))

            if daterng$(1%) <> "ALL" and daterng$(2%) <> "LAST"  then    ~
               call "DATUFMTC" (daterng$(2%))

            call "TESTRNGE" (str(daterng$(1%),,6%),str(daterng$(2%),,6%),~
                str(daterng$(3%),,6%), str(daterng$(4%),,6%), errormsg$)

            if daterng$(1%) <> "ALL" and daterng$(1%) <> "FIRST" then    ~
               call "DATFMTC" (daterng$(1%))

            if daterng$(1%) <> "ALL" and daterng$(2%) <> "LAST"  then    ~
               call "DATFMTC" (daterng$(2%))
            return

L50330: REM Test for SASUMRY# File to Fix (1-10)  FILEFIX$()
            for i% = 1% to 10% /* De-Select any undefined SASUMRY files */
                if filefix$(i%) = " " then goto L50370
                     if files$(i%) = " " then filefix$(i%) = " "
L50370:     next i%
            if str(filefix$()) <> " " then return
                errormsg$ = "You must select at least one SASUMRY file "&~
                     "to fix."
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
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
