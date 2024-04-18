        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   CCC    OOO   RRRR   X   X  RRRR   FFFFF   SSS   BBBB    *~
            *  C   C  O   O  R   R   X X   R   R  F      S      B   B   *~
            *  C      O   O  RRRR     X    RRRR   FFFF    SSS   BBBB    *~
            *  C   C  O   O  R   R   X X   R   R  F          S  B   B   *~
            *   CCC    OOO   R   R  X   X  R   R  F       SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CORXRFSB - Subroutine permits input/edit/maintenance of   *~
            *            the Core Tracking Cross-Reference file.        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/16/92 ! Original                                 ! JIM *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "CORXRFSB" (passdpart$,      /* Ignored if FUNCTION% = 0%  */~
                                         /* Do NOT modify PASSDPART$!  */~
                       function%)        /* 0- Full Input/Edit/Maint   */
                                         /* 1- Inp/Edit PASSDPART$ only*/
                                         /* 2- Display only (no edit)  */
                                         /* Do NOT modify FUNCTION%!   */

        dim                                                              ~
            corecharg$12, cchgdescr$32,  /* Core Charge/Price & Descr  */~
            corecrnsp$25, cnspdescr$34,  /* Core CR Non-Stock Part #   */~
            core_part$25, cprtdescr$34,  /* Core Part Number           */~
            core_wip$12, cwipdescr$32,   /* Core WIP G/L Acct #        */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dflt_drop$3,                 /* Default Core Drop-Off Days */~
            dmap(8),                     /* PLOWCODE Display Map       */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fginvntry$12, fgindescr$32,  /* Core F/G Inventory G/L     */~
            hdr$(3)79,                   /* PLOWCODE Headers           */~
            i$(24)80,                    /* Screen Image               */~
            inclexcl(1), inclexcl$(1)25, /* PLOWCODE Include/Exclude   */~
            inpmessage$79,               /* Informational Message      */~
            interimar$12, inardescr$32,  /* Core Bank Interim A/R G/L  */~
            intrmcogs$12, incgdescr$32,  /* Interim COGS G/L Acct      */~
            intrmsale$12, insldescr$32,  /* Interim Sales G/L Acct     */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            liability$12, liabdescr$32,  /* Core Deposit Liability G/L */~
            line2$79,                    /* Screen Line #2             */~
            mod_user$3, mod_date$10,     /* Last modified by ... on ...*/~
            passdpart$25, /* Part # passed in, if any (DO NOT MODIFY!) */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$33,                   /* PF Key Hex Values          */~
            plowkey$99, pmsg$79,         /* Miscellaneous Read/Plow Key*/~
            pricecode$1, pcdedescr$32,   /* Price Code                 */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            remanpart$25, rprtdescr$34,  /* Reman Part Number          */~
            temp$25,                     /* Temporary scalar           */~
            textid$4, textmsg$79, text$(196,1)70, /* Text fields       */~
            userid$3,                    /* Current User Id            */~
            varacct$12, vardescr$32      /* Core Variance Acct         */

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
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

*          PRINT PAGE
*          INPUT "FUNCTION, PART", FUNCTION%, PASSDPART$

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! COREXREF ! Core Tracking Cross-Reference file       *~
            * #02 ! HNYMASTR ! Inventory Master file                    *~
            * #03 ! GLMAIN   ! General Ledger Chart of Accounts file.   *~
            * #04 ! SYSFILE2 ! Caelus Management System Information     *~
            * #05 ! TXTFILE  ! System Text File                         *~
            * #06 ! GENCODES ! General Codes File                       *~
            * #07 ! COREXREF ! Core Tracking X-Ref (Alternate channel)  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "COREXREF",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =   26, keylen =  50,                     ~
                        alt key  1, keypos =    1, keylen =  50,         ~
                            key  2, keypos =   76, keylen =  25, dup

            select #02, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #03, "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            select #04, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #05,  "TXTFILE",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =   11

            select #06, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #07, "COREXREF",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =   26, keylen =  50,                     ~
                        alt key  1, keypos =    1, keylen =  50,         ~
                            key  2, keypos =   76, keylen =  25, dup

            if been_here_before% > 0% then goto L02570
                been_here_before% = 1%
                call "OPENCHCK" (#04, fs%(4%), f2%(4%),   0%, rslt$(4%))
                call "READ100" (#04, "SWITCHS.COR", f1%(4%))/* SYSFILE2 */

L02570
*        Subroutine is disabled if there is no 'SWITCHS.COR' record ...
            if f1%(4%) = 0% then goto exit_program
*        ... or for an invalid FUNCTION% ...
            if function% < 0% or function% > 2% then goto exit_program
*        ... or if required part # is missing.
            if function% > 0% and passdpart$ = " " then goto exit_program

            if been_here_before% > 1% then goto L09110       /* OK, fine */
*        One-time-only initializations begin here.
                been_here_before% = 2%
                call "OPENCHCK" (#01, fs%(1%), f2%(1%), 100%, rslt$(1%))
                call "OPENCHCK" (#02, fs%(2%), f2%(2%),   0%, rslt$(2%))
                call "OPENCHCK" (#03, fs%(3%), f2%(3%),   0%, rslt$(3%))
                call "OPENCHCK" (#05, fs%(5%), f2%(5%),   0%, rslt$(5%))
                call "OPENCHCK" (#06, fs%(6%), f2%(6%),   0%, rslt$(6%))
                call "OPENCHCK" (#07, fs%(7%), f2%(7%),   0%, rslt$(7%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

*        One-time-only initializations continue here.
            call "EXTRACT" addr("ID", userid$)
            date$ = date : call "DATEFMT" (date$)
            str(line2$,62%) = "CORXRFSB: " & str(cms2v$,,8%)

L09110
*        Perform the following initializations each & every access.
            if function% <> 1% then L09170 /* Change to = 0 if you don't */
               remanpart$ = passdpart$
               gosub describe_reman_part /* want tacky message for     */
               if f1%(2%) = 0% then exit_program  /* display either    */

L09170:     if function% <> 2%                                           ~
                then edtmessage$ = "To modify displayed values, positio"&~
                     "n cursor to desired value & press (RETURN)."       ~
                else edtmessage$ = "Display only. Editing not permitted."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            if function% <> 2% then L10090
               gosub describe_reman_part
               gosub dataload
               if f1%(1%) = 0% then                                      ~
                  errormsg$ = "Part has no Core on file"
               goto editpg1   /* Could Exit Here on Error, Too */

L10090:     for fieldnr% = 1% to 13%
                if fieldnr% = 1% and function% = 1% then goto L10300
L10120:         gosub'051(fieldnr%)        /* Default / Enables */
                     if enabled% = 0% then goto L10300
L10140:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                     if keyhit%  =  1% then gosub startover
                     if keyhit%  =  3% then gosub copy_cross_ref
                     if keyhit% <>  4% then goto L10221
L10180:                   fieldnr% = max(1%, fieldnr% - 1%)
                          gosub'051(fieldnr%)
                          if enabled% = 1% then goto L10140
                          if fieldnr% = 1% then goto L10120
                          goto L10180
L10221:              if fieldnr% < 3% then L10230
                        if keyhit% <> 7% then L10230
                           proceed% = 1%
                           keyhit%  = 0%
                           goto L10300
L10230:              if keyhit% =  7% and fieldnr% = 1% then             ~
                                               gosub select_core_master
                     if keyhit% =  8% and fieldnr% = 1% then             ~
                                               gosub select_by_reman
                     if keyhit% =  9% and fieldnr% = 1% then             ~
                                               gosub select_by_core
                     if keyhit% = 10% and fieldnr% = 1% then             ~
                                               gosub select_by_nsp
                     if keyhit% = 16% and fieldnr% = 1% then exit_program
                     if keyhit% = 16% and fieldnr% = 2% then exit_program
                     if keyhit% <> 0% then goto L10140
L10300:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " " then goto L10140
            next fieldnr%
            proceed% = 0%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                if function% <> 2% then goto L11130    /* Display only? */
                     if keyhit%  = 16% then goto exit_program
                     if keyhit%  = 25% then gosub manage_text
                     goto editpg1
L11130:         if keyhit%  =  1% then gosub startover
                if keyhit%  = 12% then goto delete_cross_ref
                if keyhit%  = 16% then goto datasave
                if keyhit%  = 32% then goto exit_program
                if keyhit%  = 25% then gosub manage_text
                if keyhit% <>  0% then goto editpg1
L11190:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% <  2% or fieldnr% > 15% then goto editpg1
            if fieldnr% =  4% then fieldnr% = 5%
            if fieldnr% >  3% then fieldnr% = fieldnr% - 1%
            if fieldnr% = lastfieldnr% then goto editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled% =  0% then goto editpg1
L11280:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  3% then goto L11390
L11310:              u3% = 2%                      /* Window at bottom */
                     call "ASKUSER" (u3%, "*** CONFIRM COPY OVER DATA *"&~
                          "**", "Press PF(16) to copy over the data in "&~
                          "this record.", "--OR--", "Press PF(1) to abo"&~
                          "rt copy.")
                     if u3% =   1% then goto L11390
                     if u3% <> 16% then goto L11310
                          gosub copy_cross_ref
L11390:         if keyhit% <>  0% then goto L11280
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " " then goto L11280
                lastfieldnr% = fieldnr%
            goto L11190

        REM *************************************************************~
            *  M I S C E L L A N E O U S   S U B R O U T I N E S        *~
            *************************************************************

        init_plowcode    /* Initialize extended argumants for PLOWCODE */
            plowkey$ = xor plowkey$
            init (" ") hdr$(), inclexcl$(), pmsg$, errormsg$
            mat inclexcl = zer
            mat dmap     = zer
            return

        modify_user_and_date        /* Update 'modified by ... on ...' */
            mod_user$ = userid$
            mod_date$ = date
            call "DATEFMT" (mod_date$)
            return

        manage_text
            if function% <> 2% then goto L15250
*        User may only view text.
                textmsg$ = "Display of text for " & remanpart$ & " " &   ~
                     rprtdescr$
                call "TXTDSPLY" (#05, f2%(5), "025", textmsg$, textid$,  ~
                     text$())
                return
L15250
*        User has full input/edit capability over text.
            textmsg$ = "Enter/edit text for " & remanpart$ & " " &       ~
                rprtdescr$
            call "TXTINSUB" (#05, f2%(5), "025", textmsg$, textid$,      ~
                text$())
            gosub modify_user_and_date /* Update 'modified by...on...' */
            return

        delete_cross_ref /* User wants to delete a Cross-Reference rec */
            u3% = 2%                               /* Window at bottom */
            call "ASKUSER" (u3%, "*** CONFIRM DELETION ***",             ~
                "Press PF(12) to DELETE this Cross Reference record.",   ~
                "--OR--", "Press PF(1) to cancel deletion.")
            if u3% =   1% then goto editpg1
            if u3% <> 12% then goto delete_cross_ref
            readkey$ = str(remanpart$) & core_part$
            call "DELETE" (#01, readkey$, 50%)             /* COREXREF */
            call "TXTFUTIL" (#05, f2%(5%), "DELE", textid$) /* TXTFILE */
            if function% = 0% then goto inputmode else goto exit_program

*        REMAN_VS_CORE
*        Selected reman part number may NOT also be a core part number.
*        Using Reman Part #, read COREXREF on Alt Channel, Alt Key 1.
            readkey$ = remanpart$
            call "PLOWALTS"(#07, readkey$, 1%, 25%, f1%(7%))/* COREXREF */
            if f1%(7%) = 0% then goto test_same_part       /* OK, fine */
                errormsg$ = "Reman Part # may not ALSO be a Core Part #."
                return clear
                return

*        CORE_VS_REMAN
*        Selected core part number may NOT also be a reman part number.
*        Using Core Part #, read COREXREF on Alt Channel, Primary Key.
            readkey$ = core_part$
            call "PLOWALTS"(#07, readkey$, 0%, 25%, f1%(7%))/* COREXREF */
            if f1%(7%) = 0% then goto test_same_part       /* OK, fine */
                errormsg$ = "Core Part # may not ALSO be a Reman Part #."
                return clear
                return

        test_same_part /* Reman & Core Part Numbers may not be the same*/
            if remanpart$ <> core_part$ then return        /* OK, fine */
                errormsg$ = "The Reman Part and Core Part may NOT be th"&~
                     "e SAME part #."
                return clear
                return

        copy_cross_ref      /* Get data fields from an existing record */
            gosub init_plowcode
            hdr$(1%) = "  Reman Part Number         Core Part Number   "&~
                "       Part NSP Number"
            dmap(1%) = 26.25 : dmap(2%) =  1
            dmap(3%) =  1.25 : dmap(4%) = 27
            dmap(5%) = 76.25 : dmap(6%) = 53
            if function% = 0% then goto L15810
                inclexcl(1%) = -26.25 : inclexcl$(1%) = " "/*Excl Mstrs*/
L15810:     gosub'200("record", 3%)
            call "PLOWCODE" (#01, plowkey$, pmsg$, 9000%, .75, f1%(1%),  ~
                hdr$(), 0, 0, inclexcl(), inclexcl$(), "d", " ", #01,    ~
                dmap())                                    /* COREXREF */
            if f1%(1%) <> 0% then goto L15880
                errormsg$ = hex(00)
                return
L15880
*        User has selected an existing record to copy.
            get #01 using L35040, temp$, junk$, junk$, corecrnsp$,        ~
                interimar$, liability$, intrmsale$, intrmcogs$,          ~
                fginvntry$, core_wip$, corecharg, pricecode$, dflt_drop%,~
                textid$, junk$, junk$, mod_user$, mod_date$/* COREXREF */
            if fieldnr% = 2% then core_part$ = temp$    /* Copy Core # */
            if textid$ = " " or textid$ = hex(00000000) or               ~
                textid$ = hex(ffffffff) then goto L16050     /* No text */
L15960:         u3% = 2%                           /* Window at bottom */
                call "ASKUSER" (u3%, "*** COPY TEXT ALSO? ***",          ~
                     "Press PF(25) to INCLUDE text in the copy.",        ~
                     "--OR--", "Press PF(1) to NOT copy text.")
                if u3% <> 1% then goto L16030
                     textid$ = hex(ffffffff)
                     goto L16050
L16030:         if u3% <> 25% then goto L15960 /* Invalid user response */
                     call "TXTFUTIL" (#05, f2%(5), "COPY", textid$)
L16050:     gosub dataload_3               /* Describe the data fields */
            gosub modify_user_and_date /* Overlay the modified-by data */
            return clear all
            goto editpg1

        describe_reman_part
            if remanpart$ <> " " then goto L16140
                rprtdescr$ = "(Core Part Master Record)"
                return
L16140:     call "DESCRIBE" (#02, remanpart$, rprtdescr$, 1%, f1%(2%))
            if f1%(2%) = 0% then rprtdescr$ = "(Unknown Part Number)"
            return

        deffn'200(word$, verb%)
            pmsg$ = hex(06) & "Select a Cross-Ref " & word$ & " to"
            on verb% goto L16210, L16220, L16230
L16210:         pmsg$ = pmsg$ & " edit"                  : goto L16240
L16220:         pmsg$ = pmsg$ & " view"                  : goto L16240
L16230:         pmsg$ = pmsg$ & " copy data fields FROM" : goto L16260
L16240:     if verb% = 0% then pmsg$ = pmsg$ & " -OR- press PF(16) to c"&~
                "reate a new " & word$
L16260:     pmsg$ = pmsg$ & "."
            return

        REM Selects for Reman & Core Part Numbers

        select_by_reman
*        Now PLOW and permit selection.
            gosub init_plowcode
            rprtdescr$, cprtdescr$ = " "
            hdr$(1%) = "  Reman Part Number         Part Description   "&~
                "              Core Part #"
            dmap(1%) =  26.25 : dmap(2%) =  1
            dmap(3%) = -26.32 : dmap(4%) = 27
            dmap(5%) =   1.25 : dmap(6%) = 60
            gosub'200("record", function%)
            if remanpart$ = "?" then remanpart$ = " "
*          IF FUNCTION% = 0% THEN GOTO 53260
                inclexcl(1%) = -26.25 : inclexcl$(1%) = " "/*Excl Mstrs*/
            plowkey$ = str(remanpart$) & hex(00)
            call "PLOWCODE" (#01, plowkey$, pmsg$, 9000%, .5, f1%(1%),   ~
                hdr$(), 0, .0026, inclexcl(), inclexcl$(), "d", " ",     ~
                #02, dmap()) /* PLOW COREXREF. Alt channel is HNYMASTR */
            if f1%(1%) = 0% then return
            goto select_load_and_edit

        select_core_master
*        Select from COREXREF file on AK1/Core #, concat with spaces.
            gosub init_plowcode
            rprtdescr$, cprtdescr$ = " "
            hdr$(1%) = "  Core Part Number          Description        "&~
                "              Core NSP Number"
            dmap(1%) =   1.25 : dmap(2%) =  1
            dmap(3%) = -26.32 : dmap(4%) = 27
            dmap(5%) =  76.25 : dmap(6%) = 60
            gosub'200("Master", function%)
            if core_part$ = "?" then core_part$ = " "
            remanpart$ = " "
            inclexcl(1%) = 26.25 : inclexcl$(1%) = " " /* Masters only */
            plowkey$ = str(core_part$) & str(remanpart$) & hex(00)
            call "PLOWCODE" (#01, plowkey$, pmsg$, 9000%, 1.75, f1%(1%), ~
                hdr$(), 0, 0, inclexcl(), inclexcl$(), "d", " ", #02,    ~
                dmap())      /* PLOW COREXREF. Alt channel is HNYMASTR */
                if f1%(1%) = 0% then return     /* Hit/Selection made? */
            gosub describe_reman_part
            goto select_load_and_edit

        select_by_core      /* Select from COREXREF file on AK1/Core # */
            gosub init_plowcode
            rprtdescr$, cprtdescr$ = " "
            hdr$(1%) = "  Core Part Number          Reman Part Number  "&~
                "       Core NSP Number"
            dmap(1%) =   1.25 : dmap(2%) =  1
            dmap(3%) =  26.25 : dmap(4%) = 27
            dmap(5%) =  76.25 : dmap(6%) = 53
            gosub'200("record", function%)
            if core_part$ = "?" then core_part$ = " "
            if remanpart$ = "?" then remanpart$ = " "
            if function% = 0% then goto L17560
                inclexcl(1%) = -26.25 : inclexcl$(1%) = " "/*Excl Mstrs*/
L17560:     plowkey$ = str(core_part$) & str(remanpart$) & hex(00)
            call "PLOWCODE" (#01, plowkey$, pmsg$, 9000%, 1.75, f1%(1%), ~
                hdr$(), 0, 0, inclexcl(), inclexcl$(), "d", " ", #01,    ~
                dmap())                                    /* COREXREF */
            if f1%(1%) = 0% then return
            goto select_load_and_edit

        select_by_nsp      /* Select from COREXREF file on AK2/Core NSP */
            gosub init_plowcode
            rprtdescr$, cprtdescr$ = " "
            hdr$(1%) = "  Core NSP Number           Reman Part Number  "&~
                "       Core Part Number"
            dmap(1%) =  76.25 : dmap(2%) =  1
            dmap(3%) =  26.25 : dmap(4%) = 27
            dmap(5%) =   1.25 : dmap(6%) = 53
            if function% = 0% then goto L17730
                inclexcl(1%) = -26.25 : inclexcl$(1%) = " "/*Excl Mstrs*/
L17730:     gosub'200("record", function%)
            call "PLOWCODE" (#01, plowkey$, pmsg$, 9000%, 2.75, f1%(1%), ~
                hdr$(), 0, 0, inclexcl(), inclexcl$(), "d", " ", #01,    ~
                dmap())                                    /* COREXREF */
            if f1%(1%) = 0% then return
            goto select_load_and_edit

        select_load_and_edit
            gosub dataload_2
            return clear all
            goto editpg1

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if remanpart$ = " " then L19070
               readkey$ = remanpart$
               call "DELETE" (#01, readkey$, 25%)
               /* Just in case we're changing Cores */
L19070:     readkey$ = str(remanpart$) & core_part$
            call "READ101" (#01, readkey$, f1%(1%))        /* COREXREF */
            call "GLUNFMT" (core_wip$)
            call "GLUNFMT" (fginvntry$)
            call "GLUNFMT" (interimar$)
            call "GLUNFMT" (intrmcogs$)
            call "GLUNFMT" (intrmsale$)
            call "GLUNFMT" (liability$)
            call "GLUNFMT" (varacct$)
            call "DATUNFMT" (mod_date$)
            put #01 using L35040, core_part$, remanpart$, core_part$,     ~
                corecrnsp$, interimar$, liability$, intrmsale$,          ~
                intrmcogs$, fginvntry$, core_wip$, corecharg, pricecode$,~
                dflt_drop%, textid$, varacct$,                           ~
                " ", " ", mod_user$, mod_date$
            if f1%(1%) = 0% then write #01 else rewrite #01
            call "TXTFUTIL" (#05, f2%(5%), "SAV2", textid$)
            if function% = 0% then goto inputmode else goto exit_program

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            if proceed% <> 0% then enabled% = 0%
            on fieldnr% gosub L20220,         /* Reman & Core Part #s   */~
                              L20240,         /* Core Part Number       */~
                              L20270,         /* Core CR Non-Stock Part */~
                              L20300,         /* Core Bank Interim G/L  */~
                              L20330,         /* Core Deposit Liab G/L  */~
                              L20360,         /* Interim Sales G/L      */~
                              L20390,         /* Interim COGS G/L       */~
                              L20420,         /* Core F/G Inventory G/L */~
                              L20450,         /* Core WIP G/L           */~
                              L20472,         /* Core VAR G/L           */~
                              L20480,         /* Core Charge/Price      */~
                              L20510,         /* Price Code             */~
                              L20550          /* Default Core Drop Days */
            return

L20220: REM Def/Enable Reman & Core Part Numbers   REMANPART$ & CORE_PART$

L20240: REM Def/Enable Core Part Number            CORE_PART$
            return

L20270: REM Def/Enable Core CR Non-Stock Part #    CORECRNSP$
            return

L20300: REM Def/Enable Core Bank Interim A/R       INTERIMAR$
            return

L20330: REM Def/Enable Core Deposit Liability      LIABILITY$
            return

L20360: REM Def/Enable Interim Sales               INTRMSALE$
            return

L20390: REM Def/Enable Interim COGS                INTRMCOGS$
            return

L20420: REM Def/Enable Core F/G Inventory          FGINVNTRY$
            return

L20450: REM Def/Enable Core WIP                    CORE_WIP$
            return

L20472: REM Def/Enable Core Variance Acct          CORE_WIP$
            return

L20480: REM Def/Enable Core Charge/Price           CORECHARG$
            return

L20510: REM Def/Enable Price Code                  PRICECODE$
            return

L20550: REM Def/Enable Default Core Drop-Off Days  DFLT_DROP$
            if dflt_drop$ = " " then dflt_drop$ = "  0"
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
         "Enter Part #(s), partial or blanks, then press (RETURN), PF(8),~
        ~ PF(9) or PF(10).",                                              ~
         "Enter Core Part Number or '?' to see list.                   ",~
         "Enter Core Credit Non-Stock Part Number.                     ",~
         "Enter Core Bank Interim A/R G/L Account Number, partial or blan~
        ~k to see list.",                                                 ~
         "Enter Core Deposit Liability G/L Account Number, partial or bla~
        ~nk to see list.",                                                ~
         "Enter Interim Sales G/L Account Number, partial or blank to see~
        ~ list.",                                                         ~
         "Enter Interim COGS G/L Account Number, partial or blank to see ~
        ~list.",                                                          ~
         "Enter Finished Goods Inventory G/L Account Number, partial or b~
        ~lank to see list.",                                              ~
         "Enter Core WIP G/L Account Number, partial or blank to see list~
        ~.",                                                              ~
         "Enter Core Variance Account Number, partial or blank to see lis~
        ~t.",                                                             ~
         "Enter Core Charge/Price for flat rate. Zeroes = CMS pricing. ",~
         "Enter CMS Price Code (A - Q or 0 - 8).                       ",~
         "Enter Default Core Drop-Off Days (1 - 999) for Invoicing.    "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$, corecharg$, cchgdescr$,    ~
                corecrnsp$, cnspdescr$, core_part$, cprtdescr$,          ~
                core_wip$, cwipdescr$, dflt_drop$, fginvntry$,           ~
                fgindescr$, interimar$, inardescr$, intrmcogs$,          ~
                incgdescr$, intrmsale$, insldescr$, liability$,          ~
                liabdescr$, pricecode$, pcdedescr$, remanpart$,          ~
                rprtdescr$, varacct$, vardescr$, mod_user$, mod_date$
            corecharg = 0
            retrieved%, dflt_drop%, proceed% = 0%
            if function% <> 0% then remanpart$ = passdpart$
            textid$ = hex(ffffffff)
            call "TXTFUTIL" (#05, f2%(5%), "INTL", textid$)
            call "ALLFREE"
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
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
            readkey$ = remanpart$
            call "PLOWNEXT" (#01, readkey$, 25%, f1%(1%))  /* COREXREF */
               if f1%(1%) = 0% then return        /* It's a Reman Part */

        dataload_2
            get #01 using L35040, core_part$, remanpart$, junk$,          ~
                corecrnsp$, interimar$, liability$, intrmsale$,          ~
                intrmcogs$, fginvntry$, core_wip$, corecharg, pricecode$,~
                dflt_drop%, textid$, varacct$,                           ~
                junk$, junk$, mod_user$, mod_date$
            retrieved% = 1%/* Indicate 'record retrieved from COREXREF' */
        dataload_3
            gosub describe_reman_part
            call "DESCRIBE" (#02, core_part$, cprtdescr$, 1%, f1%(2%))
            if f1%(2%) = 0% then cprtdescr$ = "(Unknown Part Number)"
            gosub describe_nsp
            call "DESCRIBE" (#03, interimar$, inardescr$, 1%, f1%(3%))
            call "DESCRIBE" (#03, liability$, liabdescr$, 1%, f1%(3%))
            call "DESCRIBE" (#03, intrmsale$, insldescr$, 1%, f1%(3%))
            call "DESCRIBE" (#03, intrmcogs$, incgdescr$, 1%, f1%(3%))
            call "DESCRIBE" (#03, fginvntry$, fgindescr$, 1%, f1%(3%))
            call "DESCRIBE" (#03, core_wip$ , cwipdescr$, 1%, f1%(3%))
            call "DESCRIBE" (#03, varacct$  , vardescr$ , 1%, f1%(3%))
            gosub describe_core_charge
            gosub describe_price_code
            call "GLFMT" (core_wip$)
            call "GLFMT" (fginvntry$)
            call "GLFMT" (interimar$)
            call "GLFMT" (intrmcogs$)
            call "GLFMT" (intrmsale$)
            call "GLFMT" (liability$)
            call "GLFMT" (varacct$)
            call "DATEFMT" (mod_date$)
            call "CONVERT" (corecharg, 2.4, corecharg$)
            convert dflt_drop% to dflt_drop$, pic (##0)
            call "TXTFUTIL" (#05, f2%(5%), "LOAD", textid$)
            return

        REM *************************************************************~
            *               R E C O R D   L A Y O U T S                 *~
            *************************************************************

L35040:     FMT                    /* File #01- COREXREF record layout */~
                CH(25),                  /* Core Part #   AK1          */~
                CH(25),                  /* Reman Part #   "   PK      */~
                CH(25),                  /* Core Part # again   "      */~
                CH(25),                  /* Core Cr NSP # AK2          */~
                CH(09),                  /* Core Bank - Interim A/R    */~
                CH(09),                  /* Core Deposit Liability     */~
                CH(09),                  /* Interim Sales              */~
                CH(09),                  /* Interim COGS               */~
                CH(09),                  /* Core F/G Inventory         */~
                CH(09),                  /* Core WIP                   */~
                PD(14,4),                /* Core Charge/Price          */~
                CH(01),                  /* Price code                 */~
                BI(02),                  /* Default Drop-Off Days      */~
                CH(04),                  /* Text file pointer          */~
                CH(09),                  /* Core Var Acct              */~
                CH(113),                 /* Filler                     */~
                CH(200),                 /* Filler/Reserved for VarFlds*/~
                CH(03),                  /* Last Modified User         */~
                CH(06)                   /* Last Modified Date         */

            FMT POS(166), CH(4)     /* File #01- COREXREF Text Pointer */

L35260:     FMT CH(25), CH(32)/* File #02- HNYMASTR Part & Description */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            gosub'050(1%, fieldnr%)
            gosub set_pf1
            init(hex(8c)) lfac$()
            if function% = 2% then goto L40330
            if fieldnr% <> 0% then goto L40140
                init(hex(86)) lfac$()
                lfac$(1%), lfac$(2%) = hex(8c)
L40140:     on fieldnr% gosub L40280,         /* Reman & Core Part #s   */~
                              L40300,         /* Core Part Number       */~
                              L40300,         /* Core CR Non-Stock Part */~
                              L40300,         /* Core Bank Interim G/L  */~
                              L40300,         /* Core Deposit Liab G/L  */~
                              L40300,         /* Interim Sales G/L      */~
                              L40300,         /* Interim COGS G/L       */~
                              L40300,         /* Core F/G Inventory G/L */~
                              L40300,         /* Core WIP G/L           */~
                              L40300,         /* Core Variance Acct     */~
                              L40310,         /* Core Charge/Price      */~
                              L40300,         /* Price Code             */~
                              L40310          /* Default Core Drop Days */
            goto L40330

L40280:     lfac$(1%), lfac$(2%) = hex(81) : return  /* Field # 1 & 2 */
            lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40300:     lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40310:     lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40330:     accept                                                       ~
                at (01,02), "Core Tracking Cross-Reference Maintenance", ~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (03,02), fac(hex(94)), errormsg$             , ch(79),~
                                                                         ~
                at (05,02), "Reman Part Number",                         ~
                at (05,20), fac(lfac$( 1%)), remanpart$         , ch(25),~
                at (05,46), fac(hex(8c)),    rprtdescr$         , ch(34),~
                                                                         ~
                at (06,02), "Core Part Number",                          ~
                at (06,20), fac(lfac$( 2%)), core_part$         , ch(25),~
                at (06,46), fac(hex(8c)),    cprtdescr$         , ch(34),~
                                                                         ~
                at (07,02), "Core CR NSP Part#",                         ~
                at (07,20), fac(lfac$( 3%)), corecrnsp$         , ch(25),~
                at (07,46), fac(hex(8c)),    cnspdescr$         , ch(34),~
                                                                         ~
                at (08,02), "G/L Accounts:",                             ~
                                                                         ~
                at (09,04), "Core Bank Interim A/R",                     ~
                at (09,30), fac(lfac$( 4%)), interimar$         , ch(12),~
                at (09,46), fac(hex(8c)),    inardescr$         , ch(32),~
                                                                         ~
                at (10,04), "Core Deposit Liability",                    ~
                at (10,30), fac(lfac$( 5%)), liability$         , ch(12),~
                at (10,46), fac(hex(8c)),    liabdescr$         , ch(32),~
                                                                         ~
                at (11,04), "Interim Sales - Cores",                     ~
                at (11,30), fac(lfac$( 6%)), intrmsale$         , ch(12),~
                at (11,46), fac(hex(8c)),    insldescr$         , ch(32),~
                                                                         ~
                at (12,04), "Interim COGS - Cores",                      ~
                at (12,30), fac(lfac$( 7%)), intrmcogs$         , ch(12),~
                at (12,46), fac(hex(8c)),    incgdescr$         , ch(32),~
                                                                         ~
                at (13,04), "Finished Goods Inventory",                  ~
                at (13,30), fac(lfac$( 8%)), fginvntry$         , ch(12),~
                at (13,46), fac(hex(8c)),    fgindescr$         , ch(32),~
                                                                         ~
                at (14,04), "Core WIP",                                  ~
                at (14,30), fac(lfac$( 9%)), core_wip$          , ch(12),~
                at (14,46), fac(hex(8c)),    cwipdescr$         , ch(32),~
                                                                         ~
                at (15,04), "Core Variance",                             ~
                at (15,30), fac(lfac$(10%)), varacct$           , ch(12),~
                at (15,46), fac(hex(8c)),    vardescr$          , ch(32),~
                                                                         ~
                at (16,02), "Core Charge/Price",                         ~
                at (16,30), fac(lfac$(11%)), corecharg$         , ch(12),~
                at (16,46), fac(hex(8c)),    cchgdescr$         , ch(32),~
                                                                         ~
                at (17,02), "Price Code",                                ~
                at (17,30), fac(lfac$(12%)), pricecode$         , ch(01),~
                at (17,46), fac(hex(8c)),    pcdedescr$         , ch(32),~
                                                                         ~
                at (18,02), "Default Core Drop-Off Days",                ~
                at (18,30), fac(lfac$(13%)), dflt_drop$         , ch(03),~
                                                                         ~
                at (20,02), "Last modified by",                          ~
                at (20,19), fac(hex(8c)),   mod_user$           , ch(03),~
                at (20,23), "on",                                        ~
                at (20,26), fac(hex(8c)),   mod_date$           , ch(10),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then L41040
                call "MANUAL" ("CORXRFSB") : goto L40330

L41040:     if keyhit% <> 15% then L41070
                call "PRNTSCRN" : goto L40330

L41070:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf1
            init (hex(ff)) pfkeys$
        if edit% = 2% then L41360     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field   (8)" &       ~
                      "Select by Reman Part   (13)Instructions"
            pf$(2%) = "                                     (9)" &       ~
                      "Select by Core Part    (15)Print Screen"
            pf$(3%) = "(3)Copy       (7)Core Part Masters  (10)" &       ~
                      "Select by Core NSP     (16)Exit Program"
            str(pfkeys$,,17%) = hex(01ff0304ffff0708090affff0dff0f1000)
            if function% = 0% then goto L41230
                str(pf$(3%),15%,20%) = " " : str(pfkeys$, 7%,1%) = hex(ff)
L41230:     if fieldnr% = 1% then L41280
                str(pf$(3%),15%,20%) = " " : str(pfkeys$, 7%,1%) = hex(ff)
                str(pf$(1%),38%,23%) = " " : str(pfkeys$, 8%,1%) = hex(ff)
                str(pf$(2%),38%,23%) = " " : str(pfkeys$, 9%,1%) = hex(ff)
                str(pf$(3%),37%,22%) = " " : str(pfkeys$,10%,1%) = hex(ff)
L41280:     if fieldnr% < 3% then L41291
                str(pf$(3%),64%)     = " " : str(pfkeys$,16%,1%) = hex(ff)
L41291:     if fieldnr% < 4% then L41300
                str(pf$(3%),18%,20%) = "(7)Proceed to Edit"
                                             str(pfkeys$, 7%,1%) = hex(07)
L41300:     if fieldnr% = 2% or fieldnr% = 3% then L41320
                str(pf$(3%), 1%, 7%) = " " : str(pfkeys$, 3%,1%) = hex(ff)
L41320:     if fieldnr% > 2% then L41340
                str(pf$(1%),18%,17%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L41340:     return

L41360: if fieldnr% > 0% then L41610  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                       (12)" &       ~
                      "DELETE Record          (13)Instructions"
            pf$(2%) = "                                    (25)" &       ~
                      "Manage Text            (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Save Data   "
            str(pfkeys$,,18%) = hex(01ffffffffffffffffffff0c0dff0f101900)
            if retrieved% <> 0% then goto L41460
                str(pf$(1%),37%,17%) = " " : str(pfkeys$,12%,1%) = hex(ff)
L41460:     if textid$ <> " " and textid$ <> hex(00000000) and           ~
                textid$ <> hex(ffffffff)                                 ~
                     then str(pf$(2%),36%,1%) = hex(84)                  ~
                     else str(pf$(2%),36%,1%) = hex(8c)
            str(pf$(2%),52%,1%) = hex(8c)
            if function% <> 1% then goto L41550
                str(pf$(3%),37%) = "(32)Exit without Saving"
                str(pf$(3%),64%) = "(16)Save & Exit "
                str(pfkeys$,32%,1%) = hex(20)
L41550:     if function% <> 2% then goto L41600
                str(pf$(1%), 1%,13%) = " " : str(pfkeys$, 1%,1%) = hex(ff)
                str(pf$(1%),37%,17%) = " " : str(pfkeys$,12%,1%) = hex(ff)
                str(pf$(2%),37%,15%) = "(25)View Text"
                str(pf$(3%),64%)     = "(16)Return"
L41600:     return
L41610:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(3)Copy                                 " &       ~
                      "                                       "
            str(pfkeys$,,17%) = hex(01ff03ffffffffffffffffff0dff0fff00)
            if fieldnr% = 2% or fieldnr% = 3% then L41710
                str(pf$(3%), 1%, 7%) = " " : str(pfkeys$, 3%,1%) = hex(ff)
L41710:     return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            if function% <> 2% then gosub modify_user_and_date
            on fieldnr% gosub L50230,         /* Reman & Core Part #s   */~
                              L51670,         /* Core Part Number       */~
                              L51810,         /* Core CR Non-Stock Part */~
                              L51940,         /* Core Bank Interim G/L  */~
                              L52030,         /* Core Deposit Liab G/L  */~
                              L52120,         /* Interim Sales G/L      */~
                              L52210,         /* Interim COGS G/L       */~
                              L52300,         /* Core F/G Inventory G/L */~
                              L52390,         /* Core WIP G/L           */~
                              L52471,         /* Core Var Acct          */~
                              L52480,         /* Core Charge/Price      */~
                              L52600,         /* Price Code             */~
                              L52720          /* Default Core Drop Days */
            return

L50230: REM Test for Reman & Core Part Numbers    REMANPART$ & CORE_PART$
            if remanpart$ <> " " then L50430
            pmsg$ = hex(06) & "Select a Core Part Number from HNYMASTR."
            if core_part$ = "?" then core_part$ = " "
            plowkey$ = str(core_part$) & hex(00)
            call "PLOWCODE" (#02, plowkey$, pmsg$, 0%, .32, f1%(2%))
            if f1%(2%) <> 0% then goto L50330    /* Hit/Selection made? */
                errormsg$ = "You must select a Core Part Number that ex"&~
                     "ists in HNYMASTR."
                return
L50330:     get #02 using L35260, core_part$, cprtdescr$
            fieldnr% = 2%
            call "PUTPAREN" (cprtdescr$)
*          GOSUB CORE_VS_REMAN
            readkey$ = str(remanpart$,,25) & core_part$
            call "REDALT0" (#01, readkey$, 0%, f1%(1%))
               if f1%(1%) = 0% then return
            gosub dataload_2
            return clear all
            goto editpg1

L50430: REM Test for Reman Part Number             REMANPART$
            pmsg$ = hex(06) & "Select a Reman Part Number from HNYMASTR."
            if remanpart$ = "?" then remanpart$ = " "
            plowkey$ = str(remanpart$) & hex(00)
            call "PLOWCODE" (#02, plowkey$, pmsg$, 0%, .32, f1%(2%))
            if f1%(2%) <> 0% then goto L50520    /* Hit/Selection made? */
                errormsg$ = "You must select a Part Number that exists "&~
                     "in HNYMASTR."
                return
L50520:     get #02 using L35260, remanpart$, rprtdescr$
            gosub dataload
            if f1%(1%) <> 0% then goto L50720/* Editing an X-Ref record */
*              GOSUB REMAN_VS_CORE
                call "PUTPAREN" (rprtdescr$)
                if core_part$ = " " then return
                     pmsg$ = hex(06) & "Select a Core Part Number from "&~
                          "HNYMASTR."
                     if core_part$ = "?" then core_part$ = " "
                     plowkey$ = str(core_part$) & hex(00)
                     call "PLOWCODE" (#02, plowkey$, pmsg$, 0%, .32,     ~
                          f1%(2%))
                     if f1%(2%) <> 0% then goto L50680/* Hit/Selection? */
                          errormsg$ = "You must select a Part Number th"&~
                               "at exists in HNYMASTR."
                          return
L50680:              get #02 using L35260, core_part$, cprtdescr$
                     call "PUTPAREN" (cprtdescr$)
                     fieldnr% = 2%
                     return
L50720:     return clear all
            goto editpg1

L51670: REM Test for Core Part Number             CORE_PART$
            pmsg$ = hex(06) & "Select a Core Part Number from HNYMASTR."
            if core_part$ = "?" then core_part$ = " "
            plowkey$ = str(core_part$) & hex(00)
            call "PLOWCODE" (#02, plowkey$, pmsg$, 0%, .32, f1%(2%))
            if f1%(2%) <> 0% then goto L51760    /* Hit/Selection made? */
                errormsg$ = "You must select a Core Part Number that ex"&~
                     "ists in HNYMASTR."
                return
L51760:     get #02 using L35260, core_part$, cprtdescr$
            call "PUTPAREN" (cprtdescr$)
*          GOSUB CORE_VS_REMAN
            return

L51810: REM Test for Core CR Non-Stock Part #     CORECRNSP$
            cnspdescr$ = " "
            if corecrnsp$ <> " " then goto L51860
                errormsg$ = "Core NSP Part # may not be blank."
                return
L51860:     call "READ100" (#02, corecrnsp$, f1%(2%))
            if f1%(2%) = 0% then goto L51910
                errormsg$ = "Core NSP Part # must NOT exist in HNYMASTR."
                return
        describe_nsp
L51910:     cnspdescr$ = "(Non-Stocked Part)"
            return

L51940: REM Test for Core Bank Interim A/R        INTERIMAR$
            inardescr$ = " "
            if interimar$ = " " then return
            if interimar$ = "?" then interimar$ = " "
            call "GETCODE" (#03, interimar$, inardescr$, 1%, .3, f1%(3%))
            if f1%(3%) <> 0% then return
                inardescr$, interimar$ = " "
                return

L52030: REM Test for Core Deposit Liability       LIABILITY$
            liabdescr$ = " "
            if liability$ = " " then return
            if liability$ = "?" then liability$ = " "
            call "GETCODE" (#03, liability$, liabdescr$, 1%, .3, f1%(3%))
            if f1%(3%) <> 0% then return
                liabdescr$, liability$ = " "
                return

L52120: REM Test for Interim Sales                INTRMSALE$
            insldescr$ = " "
            if intrmsale$ = " " then return
            if intrmsale$ = "?" then intrmsale$ = " "
            call "GETCODE" (#03, intrmsale$, insldescr$, 1%, .3, f1%(3%))
            if f1%(3%) <> 0% then return
                insldescr$, intrmsale$ = " "
                return

L52210: REM Test for Interim COGS                 INTRMCOGS$
            incgdescr$ = " "
            if intrmcogs$ = " " then return
            if intrmcogs$ = "?" then intrmcogs$ = " "
            call "GETCODE" (#03, intrmcogs$, incgdescr$, 1%, .3, f1%(3%))
            if f1%(3%) <> 0% then return
                incgdescr$, intrmcogs$ = " "
                return

L52300: REM Test for Core F/G Inventory           FGINVNTRY$
            fgindescr$ = " "
            if fginvntry$ = " " then return
            if fginvntry$ = "?" then fginvntry$ = " "
            call "GETCODE" (#03, fginvntry$, fgindescr$, 1%, .3, f1%(3%))
            if f1%(3%) <> 0% then return
                fgindescr$, fginvntry$ = " "
                return

L52390: REM Test for Core WIP                     CORE_WIP$
            cwipdescr$ = " "
            if core_wip$ = " " then return
            if core_wip$ = "?" then core_wip$ = " "
            call "GETCODE" (#03, core_wip$, cwipdescr$, 1%, .3, f1%(3%))
            if f1%(3%) <> 0% then return
                cwipdescr$, core_wip$ = " "
                return

L52471: REM Test for Core WIP                     CORE_WIP$
            vardescr$ = " "
            if varacct$ = " " then return
            if varacct$ = "?" then varacct$ = " "
            call "GETCODE" (#03, varacct$, vardescr$, 1%, .3, f1%(3%))
            if f1%(3%) <> 0% then return
                varacct$, vardescr$ = " "
                return

L52480: REM Test for Core Charge/Price            CORECHARG$
            cchgdescr$ = " "
            if corecharg$ = " " then corecharg$ = "0"
            call "NUMTEST" (corecharg$, 0, 9e9, errormsg$, -2.2,         ~
                corecharg)
            if errormsg$ <> " " then return
        describe_core_charge
            if corecharg = 0                                             ~
                then cchgdescr$ = "(Defaults or CMS Pricing)"            ~
                else cchgdescr$ = "(Flat Rate)"
            return

L52600: REM Test for Price Code                   PRICECODE$
            pcdedescr$ = " "
            if pricecode$ = " " then return
            if (pricecode$ >= "A" and pricecode$ <= "Q") or              ~
                (pricecode$ >= "0" and pricecode$ <= "8")                ~
                then goto describe_price_code
                     errormsg$ = "Price Code must be 'A'-'Q' or '0'-'8'."
                     return
        describe_price_code
            call "DESCRIBE" (#06, "PRICECODE" & pricecode$, pcdedescr$,  ~
                1%, f1%(6%))
            return

L52720: REM Test for Default Core Drop-Off Days   DFLT_DROP$
            call "NUMTEST" (dflt_drop$, 0, 999, errormsg$, 0, temp)
            if errormsg$ <> " " then return
                dflt_drop% = int(temp)
                convert dflt_drop% to dflt_drop$, pic (##0)
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
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            end
