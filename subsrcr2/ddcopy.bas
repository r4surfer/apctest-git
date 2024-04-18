        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  DDDD   DDDD    CCC    OOO   PPPP   Y   Y                 *~
            *  D   D  D   D  C   C  O   O  P   P  Y   Y                 *~
            *  D   D  D   D  C      O   O  PPPP    YYY                  *~
            *  D   D  D   D  C   C  O   O  P        Y                   *~
            *  DDDD   DDDD    CCC    OOO   P        Y                   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DDCOPY   - This subroutine provides the capability to COPY*~
            *            or RENAME one or many System Element Structures*~
            *            The presence of a user supplied Parent Code    *~
            *            will control whether or not a single parent or *~
            *            many parents within the same S.E.S. library    *~
            *            will be copied/renamed. The presence of a      *~
            *            specific Version ID will cause only parents    *~
            *            with that Version ID to be copied/renamed.     *~
            *            A blank Version ID will cause only the latest  *~
            *            version of each parent to be copied (regardless*~
            *            of what version that is).  Entering ALL for    *~
            *            Version ID will copy/rename all versions of    *~
            *            each structure to copy/rename.                 *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1984, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/25/84 ! ORIGINAL                                 ! LDJ *~
            * 12/06/84 ! Changed to accomodate new binary sequence! LDJ *~
            *          !   number, ergonomic screen and name      !     *~
            *          !   changes (Custom to Library), added the !     *~
            *          !   ability to create a new element in the !     *~
            *          !   dictionary on the fly, and integrated  !     *~
            *          !   the Complex Text Management Text Master!     *~
            *          !   file to be included in any COPY or     !     *~
            *          !   RENAME operation.                      !     *~
            * 07/29/85 ! Corrected bug in Text Master File Copies ! LDJ *~
            * 10/15/85 ! Corrected bug in COPY (dupe versions)    ! LDJ *~
            *          !   reorganized screen layout while at it. !     *~
            * 04/12/86 ! Minor Change to Screen Header.           ! LDJ *~
            * 06/30/86 ! Corrected bug in SINGLE_COPY, missing    ! LDJ *~
            *          !   STR around variable in string concaten-! LDJ *~
            *          !   ation.  Caused FSB 22 when SES Library !     *~
            *          !   length less than 6 characters (some-   !     *~
            *          !   times).                                !     *~
            * 10/21/86 ! Added ALL option for Versions.           ! LDJ *~
            * 03/23/90 ! No test From Version if From Parent = ALL! MJB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

            sub "DDCOPY" (#1,            /* INTDOC01 File              */~
                          #2,            /* INTDOC02 File              */~
                          #3,            /* TEXTMSTR File              */~
                          mode$)         /* Mode = RENAME or COPY      */

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dupe$7,                      /* Duplicate structures ?     */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fromlibrary$6,               /* 'From' SES Library         */~
            fromlibrary_descr$50,        /* 'From' SES Library descr   */~
            from_parent$16,              /* 'From' Parent code         */~
            from_parentdescr$50,         /* 'From' Parent code         */~
            from_version$6,              /* 'From' release version     */~
            hdr$(2)79,                   /* PLOWCODE Arguments         */~
            incl(1),                     /* PLOWCODE Arguments         */~
            incl$(1)1,                   /* PLOWCODE Arguments         */~
            i$(24)80,                    /* Screen image               */~
            inpmessage$79,               /* Input message              */~
            lfac$(20)1,                  /* Field attribute characters */~
            line2$79,                    /* Second line of screen hdr  */~
            mask$2,                      /* Alt key mask               */~
            mode$6,                      /* Literal 'COPY' or 'RENAME' */~
            new_code$1,                  /* New element code flag      */~
            pf16$12,                     /* PF 16 literal              */~
            plowkey$100,                 /* Miscell plow variable      */~
            plow$100,                    /* Miscell plow variable      */~
            readkey$100,                 /* Miscell read variable      */~
            record$(1)256,               /* INTDOC02 Record holders    */~
            revseq$4,                    /* Reversed document seq nbr  */~
            save_from_parent$16,         /* Save specified from parent */~
            save_from_version$6,         /* Save specified from version*/~
            save_to_version$6,           /* Save specified to version  */~
            tolibrary$6,                 /* 'TO'   SES Library         */~
            tolibrary_descr$50,          /* 'TO'   SES Library descr   */~
            to_parent$16,                /* 'TO'   Parent code         */~
            to_parentdescr$32,           /* 'TO'   Parent code         */~
            to_version$6,                /* 'TO'   Release version     */~
            version$6                    /* Latest version of structure*/~

        dim f1%(04)                      /* = 1 if read was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
        REM *************************************************************


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! INTDOC01 ! S.E.S. Element Dictionary File           *~
            * #02 ! INTDOC02 ! S.E.S. Structure Relationships File      *~
            * #03 ! TEXTMSTR ! Master Text Blocks file                  *~
            *************************************************************~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."
            call "GETUFBS1" addr(#3, ufbf1%)

            init(" ")fromlibrary$, tolibrary$,fromlibrary_descr$,        ~
                     tolibrary_descr$

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            nbrcopy%, nbrskip%, nbrrepl%, textcopy%, textrepl% = 0%
            init(" ") errormsg$, inpmessage$, from_parent$, to_parent$,  ~
                      to_parentdescr$,          from_version$,           ~
                      to_version$, dupe$, from_parentdescr$, new_code$

            pf16$ =   "(16)Return"

            for fieldnr% = 1 to  7
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10290
L10170:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then L10240
L10200:                  fieldnr% = max(1%, fieldnr%-1%)
                         gosub'051(fieldnr%)
                         if enabled% = 0% then L10200
                         errormsg$ = " "
                         goto L10170
L10240:               if keyhit%  = 16 and fieldnr% = 1 then exit_program
                      if keyhit%  = 32 then call "RETURN" addr(2%,u3%)
                      if keyhit% <>  0 then       L10170
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10170
L10290:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of edit mode for data entry screens.    *~
            *************************************************************
L11050:     pf16$="(16)" & mode$
            inpmessage$ = edtmessage$
            gosub'101(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 32 then call "RETURN" addr(2%,u3%)
                  if keyhit% <>  0 then       L11050
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  7 then L11050
            gosub'051(fieldnr%)
                  if enabled% = 0% then L11050
                  pf16$ = " "
L11130:     gosub'101(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11130
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11130
            goto L11050

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Perform COPY / RENAME function                            *~
            * If FROM_PARENT$  > " " then do single parent copy else    *~
            * If FROM_VERSION$ > " " then copy-rename that version only *~
            * If FROM_VERSION$ = " " then    ""    latest versions only *~
            * If FROM_VERSION$= "ALL"then    ""    all versions         *~
            *************************************************************

        datasave
            if from_parent$ <> "ALL" and from_version$ <> "ALL" then     ~
               flag% = 0%
            if from_parent$ <> "ALL" and from_version$=" " then flag%=1%
            if from_parent$ <> "ALL" and from_version$ = "ALL" then      ~
               flag% = 2%
            if from_parent$ = "ALL" and from_version$ > " " and          ~
               from_version$ <> "ALL" then flag% = 3%
            if from_parent$="ALL" and from_version$ = " " then flag% = 4%
            if from_parent$="ALL" and from_version$="ALL" then flag% = 5%
            on flag% + 1% gosub single_copy,/* 1 parent, 1 version     */~
                                single_copy,/* 1 parent, latest version*/~
                                multi_copy, /* 1 parent, all versions  */~
                                multi_copy, /* all parents, 1 version  */~
                                multi_copy, /* all parents, latest vers*/~
                                multi_copy  /* all parents, all version*/

            inpmessage$=mode$ & " complete.  Please press ENTER to acknow~
        ~lege results above"

            str(line2$,,64) = "RESULTS of " & mode$ & " are listed below"
            str(line2$,65) = "DDCOPY:" & str(cms2v$,,8%)
L19112:
            accept                                                       ~
               at (01,02), fac(hex(8c)), mode$                  , ch(06),~
               at (01,09),                                               ~
                  "System Element Structures / Text",                    ~
               at (01,66),                                               ~
                  "Today:",                                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "'FROM' Parent Code    :",                             ~
               at (06,30), fac(hex(8c))  , from_parent$         , ch(16),~
               at (06,49), fac(hex(8c)),   from_parentdescr$    , ch(32),~
               at (07,02),                                               ~
                  "'TO'   Parent Code    :",                             ~
               at (07,30), fac(hex(8c))  , to_parent$           , ch(16),~
               at (07,49), fac(hex(8c)),   to_parentdescr$      , ch(32),~
               at (08,02),                                               ~
                  "'FROM' SES Library    :",                             ~
               at (08,30), fac(hex(8c))  , fromlibrary$         , ch(06),~
               at (08,46), fac(hex(8c)),   fromlibrary_descr$   , ch(35),~
               at (09,02),                                               ~
                  "'TO'   SES Library    :",                             ~
               at (09,30), fac(hex(8c))  , tolibrary$           , ch(06),~
               at (09,46), fac(hex(8c)),   tolibrary_descr$     , ch(35),~
               at (10,02),                                               ~
                  "'FROM' Release Version:",                             ~
               at (10,30), fac(hex(8c))  , from_version$        , ch(06),~
               at (11,02),                                               ~
                  "'TO'   Release Version:",                             ~
               at (11,30), fac(hex(8c))  , to_version$          , ch(06),~
               at (12,02),                                               ~
                  "Duplicate Structures ?:",                             ~
               at (12,30), fac(hex(8c))  , dupe$                , ch(07),~
               at (14,10),                                               ~
                  "Results of",                                          ~
               at (14,21), fac(hex(84)),   mode$                , ch(06),~
               at (15,04),                                               ~
                  "Number of Structures Copied   =",                     ~
               at (15,36), fac(hex(84)), nbrcopy%            ,pic(#####),~
               at (16,04),                                               ~
                  "Number of Structures Skipped  =",                     ~
               at (16,36), fac(hex(84)), nbrskip%            ,pic(#####),~
               at (17,04),                                               ~
                  "Number of Structures Replaced =",                     ~
               at (17,36), fac(hex(84)), nbrrepl%            ,pic(#####),~
               at (18,04),                                               ~
                  "Number of Text Blocks Copied  =",                     ~
               at (18,36), fac(hex(84)), textcopy%           ,pic(#####),~
               at (19,04),                                               ~
                  "Number of Text Blocks Replaced=",                     ~
               at (19,36), fac(hex(84)), textrepl%           ,pic(#####),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
                                                                         ~
               keys(hex(000d0f)),                                        ~
               key (key%)

               if key% <> 13 then L19388
                  call "MANUAL" ("DDCOPY  ")
                  goto L19112

L19388:        if key% <> 15 then L19400
                  call "PRNTSCRN"
                  goto L19112
L19400:        goto inputmode

        single_copy
            mask$ = hex(6000)
            version$ = from_version$
            call "SHOSTAT" ("Now " & mode$ & "ing " & from_parent$)
            if to_parent$ = " " then to_parent$ = from_parent$
            if version$ = " " then gosub find_latest_version
            if to_version$ = " " then to_version$ = version$
            if new_code$ = "Y" then gosub add_dictionary_entry
        REM *************************************************************~
            *       Check for duplicates first and decide how to handle *~
            *************************************************************
            readkey$ = str(to_parent$) & str(to_version$) &              ~
                       str(tolibrary$) & hex(000000)
            call "PLOWNEXT" (#2, readkey$, 28%, f1%(2))
            if f1%(2) = 0% then L19560    /* No duplicate found         */
            if dupe$ = "SKIP" then nbrskip% = nbrskip% + 1%
            if dupe$ = "SKIP" then return
            if dupe$ = "REPLACE" then nbrrepl% = nbrrepl% + 1%
            if dupe$ = "REPLACE" then gosub delete_text_block
            if dupe$ = "REPLACE" then call "DELETE" (#2, readkey$, 28%)
            if dupe$ = "REPLACE" then L19560
            get #2 using L19496, mod_date1$
L19496:     FMT POS(208), CH(06)
            plowkey$ = str(from_parent$) & str(version$) &               ~
                       str(fromlibrary$) & hex(000000)
            call "PLOWNEXT" (#2, plowkey$, 28%, f1%(2))
            if f1%(2) = 0% then return
            get #2, using L19496, mod_date2$
            if mod_date1$ >= mod_date2$ then nbrskip% = nbrskip% + 1%
            if mod_date1$ >= mod_date2$ then return
            gosub delete_text_block
            call "DELETE" (#2, readkey$, 28%)
            nbrrepl% = nbrrepl% + 1%

        REM *************************************************************~
            * Here's the actual copy process.  The rename works by      *~
            * copying the old structure to the new name & then delete   *~
            * the original.  Not fast but it's safe.                    *~
            *************************************************************
L19560:     plowkey$ = str(from_parent$) & str(version$) &               ~
                       str(fromlibrary$) & hex(000000)
L19568:     call "PLOWNEXT" (#2, plowkey$, 28%, f1%(2))
            if f1%(2) = 0% then delete_old_structure
            get #2, using L19580, record$(1)
L19580:     FMT CH(256)
            put record$(1), using L19592, to_parent$, tolibrary$,         ~
                            to_parent$, to_version$, tolibrary$
L19592:     FMT POS(23), CH(16), CH(6), CH(16), CH(6), CH(6)
            put #2, using L19580, record$(1)
            write #2, mask=mask$
            mask$ = hex(8000)
            goto L19568

        add_dictionary_entry
            call "READ100" (#1, from_parent$, f1%(1))
            if f1%(1) = 0% then return
            key(#1) = to_parent$
            write #1
            return

        find_latest_version:   /* Finds latest version of a parent     */
            plowkey$ = str(from_parent$) & str(fromlibrary$) &           ~
                       str(from_parent$)
L19656:     call "PLOWALTS" (#2, plowkey$, 2%, 38%, f1%(2))
            if f1%(2) = 0% then return   /*Should never happen 1st plow*/
            version$ = str(plowkey$,39,6)
            goto L19656

        delete_old_structure:  /* If function is rename then kill the old~
                                  structure now that it's been copied  */
            nbrcopy% = nbrcopy% + 1%
            gosub copy_text_block /*copy or rename text block (if there*/
            if mode$ <> "RENAME" then return
            plowkey$ = str(from_parent$) & str(version$) &               ~
                       str(fromlibrary$) & hex(000000)
            call "DELETE" (#2, plowkey$, 28%)
            return

        REM *************************************************************~
            * Copy/rename specific version only from one SES library to *~
            * another or upgrade/downgrade a specific version within the*~
            * same SES library else copy latest version only of each    *~
            * parent else copy ALL versions.                            *~
            *************************************************************~

        multi_copy
            save_to_version$ = to_version$
            save_from_version$ = from_version$
            save_from_parent$ = from_parent$
            plow$ = fromlibrary$
            break% = 6%
            if save_from_parent$ = "ALL" then L19769
               break% = 22%
               str(plow$,7%) = save_from_parent$
L19769:     call "PLOWALTS" (#2, plow$, 3%, break%, f1%(2))
            if f1%(2) = 0% then return
            if flag% = 3% and str(plow$,23%,6%) <> save_from_version$    ~
               then L19769
            from_parent$ = str(plow$, 7%, 16%)
            if flag%=2% or flag%=5% then from_version$=str(plow$,23%,6%)
            if flag% = 4% then from_version$ = " "
            gosub single_copy
            to_version$=save_to_version$
            from_version$=save_from_version$
            to_parent$ = " "
*          STR(PLOW$,,22%) = ADDC HEX(01)
*          STR(PLOW$,23%) = ALL(HEX(00))
            goto L19769

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for the page 1 of input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1
                  on fieldnr% gosub L20320,         /* From SES Library */~
                                    L20420,         /* From Parent      */~
                                    L20500,         /* From Version     */~
                                    L20580,         /* To SES Library   */~
                                    L20660,         /* To Parent        */~
                                    L20820,         /* To Version       */~
                                    L20900          /* Duplicates ?     */
                     return
L20320:     REM Default/enable for 'from' ses library
                inpmessage$="Enter the SES Library of the structure(s) to~
        ~ be "     & mode$ & "'d "
                if fromlibrary$ = " " then fromlibrary$ = "CAELUS"
                return
L20420:     REM Default/enable for 'from' parent code
                inpmessage$="Enter the Parent Code of the Structure to " ~
                & mode$ & " or ALL to copy all parents"
                return
L20500:     REM Default/enable for 'from' release version
                inpmessage$="Blank to " & mode$ & " the latest version," ~
                   & " ALL for all versions, or a specific version"
                return
L20580:     REM Default/enable for 'to'   ses library
                inpmessage$="Enter the SES Library of the structure(s) to~
        ~ be "     & mode$ & "'d TO"
                if tolibrary$ = " " then tolibrary$ = fromlibrary$
                return
L20660:     REM Default/enable for 'to'   parent code
                enabled%=0%
                if from_parent$ = "ALL" then return
                enabled%=1%
                if to_parent$=" " then to_parent$=from_parent$
                inpmessage$="Enter the Parent Code of the Structure to " ~
                & mode$ & " TO "
                return
L20820:     REM Default/enable for 'to'   release version
                inpmessage$="Enter desired Version ID or leave blank to r~
        ~etain same version as 'From' structure(s)"
                if from_version$ <> "ALL" then return
                to_version$ = " "
                enabled% = 0%
                return
L20900:     REM Default/enable for duplicate structures ?
                inpmessage$ =                                            ~
                "How do you wish to handle duplicate structures found dur~
        ~ing the " & mode$ & " process"
                if dupe$ = " " then dupe$ = "SKIP"
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1984, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the user the ability to start over when he wants to *~
            * or will return user back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            k% = 2%
            call "STARTOVR" (k%)
               on k% + 1% goto L29942, L29948
               return

L29942:        REM START OVER            (ENTER)
                   mode$ = " "
                   end
L29948:        REM RETURN TO DISPLAY.    (P.F. KEY 1)
                   return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *-----------------------------------------------------------*~
            * Inputs document for first time.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
            if ufbf1% = 1% then                                          ~
            line2$ = "(Any Text Blocks/Documents will also be included)"
            str(line2$,64)="DDCOPY: " & str(cms2v$,,8%)
                  if errormsg$ > " " then print at(1,1); bell
                  on fieldnr% gosub L40200,         /* From SES Library */~
                                    L40200,         /* From Parent      */~
                                    L40200,         /* From Version     */~
                                    L40200,         /* To SES Library   */~
                                    L40200,         /* To Parent        */~
                                    L40200,         /* To Version       */~
                                    L40200          /* Duplicates ?     */
                  if errormsg$ > " " and fieldnr% > 0% then              ~
                     lfac$(fieldnr%) = or hex(10)
                     goto L40270

                  REM Set fac's for upper/lower case input
                      lfac$(fieldnr%) = hex(80)
                      return
L40200:           REM Set fac's for upper case only input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set fac's for numeric only input
                      lfac$(fieldnr%) = hex(82)
                      return

L40270:     accept                                                       ~
               at (01,02), fac(hex(8c)), mode$                  , ch(06),~
               at (01,09),                                               ~
                  "System Element Structures / Text",                    ~
               at (01,66),                                               ~
                  "Today:",                                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "'FROM' SES Library    :",                             ~
               at (06,30), fac(lfac$( 1)), fromlibrary$         , ch(06),~
               at (06,38), fac(hex(8c)),   fromlibrary_descr$   , ch(43),~
               at (07,02),                                               ~
                  "       Parent Code    :",                             ~
               at (07,30), fac(lfac$( 2)), from_parent$         , ch(16),~
               at (07,47), fac(hex(8c)),   from_parentdescr$    , ch(34),~
               at (08,02),                                               ~
                  "       Release Version:",                             ~
               at (08,30), fac(lfac$( 3)), from_version$        , ch(06),~
               at (09,02),                                               ~
                  "'TO'   SES Library    :",                             ~
               at (09,30), fac(lfac$( 4)), tolibrary$           , ch(06),~
               at (09,38), fac(hex(8c)),   tolibrary_descr$     , ch(43),~
               at (10,02),                                               ~
                  "       Parent Code    :",                             ~
               at (10,30), fac(lfac$( 5)), to_parent$           , ch(16),~
               at (10,47), fac(hex(8c)),   to_parentdescr$      , ch(34),~
               at (11,02),                                               ~
                  "       Release Version:",                             ~
               at (11,30), fac(lfac$( 6)), to_version$          , ch(06),~
               at (12,02),                                               ~
                  "Duplicate Structures ?:",                             ~
               at (12,30), fac(lfac$( 7)), dupe$                , ch(07),~
               at (14,04),                                               ~
                  "Enter: SKIP    to skip duplicate structures",         ~
               at (15,04),                                               ~
                  "       REPLACE to replace the existing structure with ~
        ~the FROM structure",                                             ~
               at (16,04),                                               ~
                  "       UPDATE  to",                                   ~
               at (16,22), fac(hex(8c)), mode$                  , ch(06),~
               at (16,29),                                               ~
                  "only those structures with a more recent mod date ",  ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,02),                                               ~
                  "(1)Start Over     (4)Previous Field ",                ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), fac(hex(84)), pf16$                  , ch(12),~
               at (24,02),                                               ~
                  "(32)Quit",                                            ~
                                                                         ~
               keys(hex(0001040d0f1020)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L40790
                  call "MANUAL" ("DDCOPY  ")
                  goto L40270

L40790:        if keyhit% <> 15 then L40820
                  call "PRNTSCRN"
                  goto L40270

L40820:        if fieldnr% > 0% then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return
               u3%=u3%

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Tests data for the items on page 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50080,         /* From SES Library */~
                                    L50195,         /* From Parent      */~
                                    L50280,         /* From Version     */~
                                    L50385,         /* To SES Library   */~
                                    L50495,         /* To Parent        */~
                                    L50560,         /* To Version       */~
                                    L50630          /* Duplicates ?     */
                     return
L50080:     REM Test data for 'From' SES library
                readkey$ = "LIBRARY " & fromlibrary$
                fromlibrary_descr$=hex(06) & "Select Library to " & mode$
                f1%(1) = -6%
                call "PLOWCODE" (#1, readkey$, fromlibrary_descr$,       ~
                                 8%,1.5,f1%(1))
                fromlibrary$ = str(readkey$,9%)
                if f1%(1)=0% then errormsg$ = "SES Library " &           ~
                   fromlibrary$ & " not defined"
                if f1%(1) = 0% or mode$ <> "RENAME" then return
                readkey$ = fromlibrary$
                call "PLOWALTS" (#2, readkey$,3%,6%,f1%(2))
                if f1%(2) > 0% then L50160
                   errormsg$="Not currently used as a Library for any str~
        ~ucture"
                return
L50160: REM *** Does current user have write access to this ses library***
            call "SESACESS" (#1,         /* INTDOC01                   */~
                             #2,         /* INTDOC02                   */~
                         fromlibrary$,   /* S.E.S. LIBRARY             */~
                             " ",        /* CURRENT PARENT             */~
                             errormsg$)  /* ERROR MESSAGE RETURNED     */
                return
L50195:     REM Test data for 'from' parent code
                if from_parent$ = "ALL" then to_parent$ = " "
                if from_parent$ = "ALL" then return
                readkey$ = str(fromlibrary$) & from_parent$
                hdr$(1) = "  Parent Code        Description"
                from_parentdescr$=hex(06) & "Select PARENT to " & mode$
                f1%(2) = -7%
                call "PLOWCODE" (#2, readkey$, from_parentdescr$,8006%,  ~
                     3.5, f1%(2), hdr$(), 16,0,incl(),incl$()," "," ",#1)
                from_parent$ = str(readkey$,7%,16%)
                from_parentdescr$ = " "
                if f1%(2) = 0% then L50270
                call "READ100" (#1, from_parent$, f1%(1))
                if f1%(1) = 1% then from_parentdescr$ = key(#1,3)
                return
L50270:         errormsg$="Parent code not found in specified Library!"
                return
L50280:     REM Test data for 'from' release version
                if from_parent$ ="ALL" then return
*              IF FROM_PARENT$ ="ALL" AND FROM_VERSION$ = "ALL" THEN    ~
*                 RETURN
*              IF FROM_PARENT$ ="ALL" AND FROM_VERSION$ = "   " THEN    ~
*                 RETURN
                if from_version$ = "ALL" then L50350
                if from_version$ = " " then L50350
                readkey$ = str(fromlibrary$) &                           ~
                           str(from_parent$) & from_version$
                f1%(2) = -8%
                call "PLOWCODE" (#2, readkey$, " ", 22%, 3.5, f1%(2))
                from_version$ = str(readkey$,23%,6%)
                if f1%(2) > 0% then return
                errormsg$="Sorry, this version of " & from_parent$ & " do~
        ~es not exist in library " & fromlibrary$
                return
L50350:         readkey$ = str(from_parent$) & str(fromlibrary$) &       ~
                           str(from_parent$)
                call "PLOWALTS" (#2, readkey$, 2%, 38%, f1%(2))
                if f1%(2) = 0% then errormsg$ =                          ~
                   "Sorry, but " & from_parent$ & " is not a structure un~
        ~der library " & fromlibrary$
                return
L50385:     REM Test data for 'to'   ses library
                readkey$ = "LIBRARY " & tolibrary$
                f1%(1) = -9%
                call "PLOWCODE" (#1, readkey$, tolibrary_descr$,         ~
                                 8%,1.5,f1%(1))
                tolibrary$ = str(readkey$,9%)
                if f1%(1)=0% then errormsg$ =        "SES Library " &    ~
                   tolibrary$ & " not defined in Element Dictionary"
                if f1%(1)=0% then return
        REM *** Does current user have write access to this ses library***
            call "SESACESS" (#1,         /* INTDOC01                   */~
                             #2,         /* INTDOC02                   */~
                             tolibrary$, /* S.E.S. Library             */~
                             " ",        /* Current parent             */~
                             errormsg$)  /* Error message returned     */
                if errormsg$ > " " then return
                if from_parent$ <> "ALL" then return
                if fromlibrary$ <> tolibrary$ then return
                if from_version$ <> "ALL" then return
                errormsg$ = "You Cannot " & mode$ &                      ~
                  " an entire Library to itself (Call for Help!)"
                return
L50495:     REM Test data for 'to'   parent code
                if from_parent$ = "ALL" then to_parent$ = " "
                if from_parent$ = "ALL" then return
                new_code$ = " "
                f1%(1) = -10%
                call "PLOWCODE" (#1, to_parent$, to_parentdescr$, 0%,    ~
                                  0.50, f1%(1))
                if f1%(1) > 0% then L50545
                to_parentdescr$=hex(a4) & "NEW DICTIONARY CODE !"
                new_code$ = "Y"
L50545:         if to_parent$ <> from_parent$ or                         ~
                   tolibrary$ <> fromlibrary$ or                         ~
                   from_version$ <> "ALL" then return
                errormsg$ = "You Cannot " & mode$ & " all the Versions" &~
                  " of a Parent to themselves (Call for Help!)"
                return
L50560:     REM Test data for 'to'   release version
                if from_version$ = "ALL" then to_version$ = " "
                if from_version$ = "ALL" then return
                if from_parent$ <> "ALL" then L50565
                if from_parent$ = "ALL" and to_version$ = " " then L50590
L50565:         if to_version$ = " " then L50585
                if len(to_version$) = 6% then L50585
                errormsg$="Release Version must be full 6 Characters"
                return
L50585:         if from_parent$ <> to_parent$ then return
L50590:         if fromlibrary$ <> tolibrary$ then return
                if to_version$ = " " then errormsg$ =                    ~
                   "Sorry, in this instance You MUST supply a Version #"
                if to_version$ = " " then return
                if to_version$ <> from_version$ then return
                errormsg$ = "Sorry, you can't " & mode$ & " something "  ~
                          & "to itself - look again!"
                return
L50630:     REM Test data for duplicate structures ?
                if dupe$ = "SKIP" then return
                if dupe$ = "REPLACE" then return
                if dupe$ = "UPDATE" then return
                errormsg$ = "Invalid option, please see list BELOW !"
                return

        delete_text_block
            plowkey$=str(to_parent$) & str(tolibrary$) & str(to_version$)
            call "REDALT0" (#3, plowkey$, 1%, f1%(3))
            if f1%(3) = 0% then return
            plowkey$ = key(#3)
            str(plowkey$,5) = all(hex(00))
            call "DELETE" (#3, plowkey$, 4%)
            textrepl% = textrepl% + 1%
            return

        copy_text_block
            if mode$ <> "RENAME" then L51145

        REM *********** GET CURRENT DOCUMENT SEQ NUMBER *************
            plowkey$ = str(from_parent$) & str(fromlibrary$) & version$
            call "REDALT1" (#3, plowkey$, 1%, f1%(3))
            if f1%(3) = 0% then return
            plowkey$ = key(#3)
            document_seq_nbr% = val(str(plowkey$,,4%),4)
            revseq$ = key(#3,2)
            goto L51270

L51145: REM ******* GET NEXT AVAILABLE DOCUMENT SEQ NUMBER **********
            document_seq_nbr% = 1%
            init(hex(00))plowkey$
            call "PLOWAL1" (#3, plowkey$, 2%, 0%, f1%(3))
            if f1%(3) = 0% then L51220
            get #3 using L51210 , document_seq_nbr%
            document_seq_nbr% = document_seq_nbr% + 1%
L51210:     FMT POS(33), BI(4)
L51220:     revseq$ = bin(document_seq_nbr%,4) bool6 all(hex(ff))
        REM *********************************************************

            plowkey$ = str(from_parent$) & str(fromlibrary$) & version$
            call "REDALT1" (#3, plowkey$, 1%, f1%(3))
L51250:     if f1%(3) = 0% then return
            plowkey$ = key(#3)
L51270:     put #3 using L51360, to_parent$,                              ~
                                tolibrary$,                              ~
                                to_version$,                             ~
                                revseq$,                                 ~
                                document_seq_nbr%
            if mode$ = "COPY" then L51320
            if str(plowkey$,5,2)=hex(0001) then rewrite #3,mask=hex(c000)~
            else rewrite #3, mask=hex(0000)
            goto L51322
L51320:     if str(plowkey$,5,2) = hex(0001) then write #3,mask=hex(c000)~
            else write #3, mask=hex(0000)
L51322:     if str(plowkey$,5,2) = hex(0001) then textcopy%=textcopy%+1%
            call "PLOWNXT1" (#3, plowkey$, 4%, f1%(3))
            goto L51250

L51360:     FMT                          /* FILE; TEXTMSTR             */~
                               CH(16),   /* DOCUMENT ID                */~
                               CH(6),    /* LIBRARY                    */~
                               CH(6),    /* VERSION                    */~
                               CH(4),    /* REVERSED DOCUMENT SEQ NBR  */~
                               BI(4)     /* DOCUMENT SEQUENCE NUMBER   */

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1984, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

        exit_program
            end
