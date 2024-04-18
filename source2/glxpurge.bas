        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   GGG   L      X   X  PPPP   U   U  RRRR    GGG   EEEEE   *~
            *  G      L       X X   P   P  U   U  R   R  G      E       *~
            *  G GGG  L        X    PPPP   U   U  RRRR   G GGG  EEEE    *~
            *  G   G  L       X X   P      U   U  R   R  G   G  E       *~
            *   GGG   LLLLL  X   X  P       UUU   R   R   GGG   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLXPURGE - Purges records from the GLBATCH & GLEXPORT     *~
            *            files.  Records associated with Posted Batches *~
            *            are purged in a date range.                    *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11-28-90 ! Original                                 ! JDH *~
            * 05-13-91 ! Mods for R6.01.00 release.               ! JDH *~
            * 09/18/92 ! PRRs 12395,12518  Fixed Report.          ! JDH *~
            *          ! Added Posting Date to report.            !     *~
            * 10/29/92 ! To date now can't be in the future.      ! JDH *~
	    * 06/25/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            backgnd_process$1,           /* Background Processing Activ*/~
            company$50,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            datec$10,                    /* Date for TO default        */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            export_on$1,                 /* Is GL Export Active?       */~
            from_date$10,                /* Date Range to Purge        */~
            from_date_range$10,          /* Date Range to Purge Unfmtd */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            mgtrpt_on$1,                 /* Management Reportng Active?*/~
            mgt_status$1,                /* Management Status          */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99, readkey$99,      /* Miscellaneous Read/Plow Key*/~
            post_date$8,                 /* Posting Date from GLBATCH  */~
            print$1,                     /* Print Listing? (Y/N)       */~
            rptid$6,                     /* Report ID                  */~
            seq$10,                      /* Batch Posting Sequence #   */~
            to_date$10,                  /* Date Range to Purge        */~
            to_date_range$10,            /* Date Range to Purge Unfmtd */~
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
            * #02 ! SYSFILE2 ! Caelus Management System Information     *~
            * #03 ! GLBATCH  ! General Ledger Batch File                *~
            * #04 ! GLEXPORT ! General Ledger Export file               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #02, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #03, "GLBATCH",                                       ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    5, keylen =   9,                     ~
                        alt key  1, keypos =    1, keylen =  13,         ~
                            key  2, keypos =   22, keylen =   8, dup     ~

            select #04, "GLEXPORT",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  32                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 0%, rslt$(02%))
            call "OPENCHCK" (#03, fs%(03%), f2%(03%), 0%, rslt$(03%))
            call "OPENCHCK" (#04, fs%(04%), f2%(04%), 0%, rslt$(04%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date : datec$ = date$
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62%) = "GLXPURGE: " & str(cms2v$,,8%)

            linectr% = 999% : page% =  0%
            call "COMPNAME" (12%, company$, u3%)
            rptid$ = "G/L011"

*        Check if Background Processing is active.
            backgnd_process$ = "N" : temp$ = " "
            call "READ100" (#02, "SWITCHS.GL", f1%(2%))
            if f1%(2%) = 1% then get #02 using L09212, temp$
L09212:         FMT POS(23), CH(8)
            if temp$ <> " " then backgnd_process$ = "Y"

*        See if GL Export or Management Reporting is on
            mgtrpt_on$ = "N" : export_on$ = "N"
            if f1%(2%) = 1% then get #2 using L09260, export_on$, mgtrpt_on$
L09260:         FMT POS(22), CH(1), POS(59), CH(1)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  2%
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
                  if keyhit%  = 16% then       purgatory
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
            *             P U R G E   D A T A   H E R E                 *~
            *-----------------------------------------------------------*~
            * Purges data in GLBATCH & GLEXPORT Files.                  *~
            *************************************************************

        purgatory
            gosub make_sure
*        First we PLOW the GLBATCH file to find completed batches
            call "SHOSTAT" ("PURGING Files...")
            plowkey$ = all(hex(00))

            /* Without background processing, the batch status must be */
            /* a '2', which means a closed GL batch.  If the customer  */
            /* is using a custom background processor of the GLEXPORT  */
            /* file, purging only happens if the batch status is '6'.  */
            /* This status means successfully updated and must be set  */
            /* by the background processor.  Status '3' thru '5' are   */
            /* for custom processing indicators as required by user.   */
            if backgnd_process$ = "Y" then str(plowkey$,1%,1%) = "6"     ~
                                      else str(plowkey$,1%,1%) = "2"
            if print$ = "Y" then gosub setup_print
        plow_batch_loop
            call "PLOWALTS" (#03, plowkey$, 1%, 1%, f1%(3%))
                if f1%(3%) <> 0% then L19170
                if print$ = "Y" then shutdown_print else exit_program
L19170:     get #03 using L19180, post_date$, mgt_status$
L19180:         FMT POS(16), CH(6), POS(62), CH(1)
            if post_date$ < from_date_range$ or                          ~
               post_date$ > to_date_range$ then plow_batch_loop
            if mgtrpt_on$ = "Y" and mgt_status$ < "1" then plow_batch_loop
            call "DATEFMT" (post_date$)

*        Now delete GLEXPORT records
            if export_on$ <> "Y" then L19260
                if print$ = "Y" then purge_and_print
                    call "DELETE" (#04, key(#03), 9%)

L19260
*        And delete GLBATCH record
            if print$ = "N" then delete_batch_record
L19264:         get str(plowkey$,10%,4%) using L19266, seq%
L19266:              FMT BI(4)
                convert seq% to seq$, pic(#####)
                print using L60110, str(plowkey$,5%,2%) ,                 ~
                               str(plowkey$,7%,3%), seq$, post_date$, " "
                linectr% = linectr% + 1%
                if linectr% > 54% then gosub print_header
                printed_something% = 1%
        delete_batch_record
            if printed_something% <> 1% and print$ = "Y" then L19264
                call "DELETE" (#03, key(#03), 9%)
                goto plow_batch_loop  /* Back for more */

*        Here we PLOW, PRINT, DELETE,   PLOW, PRINT, DELETE.....
        purge_and_print
            print_batch_info% = 1%
            readkey$ = str(plowkey$,5%,9%) & hex(00)
            if linectr% > 54% then gosub print_header
        plow_export_loop
            call "PLOWNXT1" (#04, readkey$, 9%, f1%(4%))
            if f1%(4%) = 0% then delete_batch_record
            temp$ = str(readkey$,10%,16%)
            if len(temp$) < 15% then temp$ = "  " & temp$
            if print_batch_info% <> 1% then L19480
            if header_just_printed% = 1% then L19420
                print : linectr% = linectr% + 1%
L19420:     get #04 using L19430, seq%, temp_date$
L19430:         FMT POS(6), BI(4), POS(545), CH(6)
            convert seq% to seq$, pic(#####)
            call "DATEFMT" (temp_date$)
            print using L60110, str(readkey$,1%,2%), str(readkey$,3%,3%), ~
                               seq$, temp_date$, temp$
            printed_something% = 1%
            goto L19485
L19480:     if str(readkey$,10%,16%) = " " then L19490
            print using L60120, temp$
L19485:     linectr% = linectr% + 1%
L19490:     print_batch_info% = 0%
            header_just_printed% = 0%
            if linectr% > 54% then gosub print_header
            delete #04
            goto plow_export_loop

        print_header
            if linectr% <> 999% then print page
            page% = page% + 1%
            print using L60010, date$, company$, rptid$
            print using L60040, time$, page%
            print using L60070, from_date$, to_date$
            print
            if export_on$ = "Y" then print using L60090                   ~
                                else print using L60095
            if export_on$ = "Y" then print using L60100                   ~
                                else print using L60105
            linectr% = 7%
            print_batch_info% = 1%
            header_just_printed% = 1%
            return

        setup_print
            time$ = " "  :  call "TIME" (time$)
            select printer(134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            gosub print_header
            return

        shutdown_print
            print
            time$ = " "  :  call "TIME" (time$)
            print using L60140, time$
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            close printer
            goto exit_program

*        Make sure that purge is to proceed
        make_sure
            u3% = 2%
            call "ASKUSER" (u3%, "*** PURGE ABOUT TO TAKE PLACE ***",    ~
                            " ", "Press RETURN to ABORT Purge  -OR-  P" &~
                            "ress PF16 to PURGE.", " ")
            if u3% = 16% then return
            return clear all
            goto editpg1

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Date Range             */~
                              L20300          /* Print Listing?         */
            return
L20100: REM Def/Enable Date Range to Purge         FROM_DATE$
            if from_date$ <> " " then L20200
                from_date$ = "19010101"
                call "DATFMTC" (from_date$)

L20200: REM Def/Enable to                          TO_DATE$
            if to_date$ <> " " then return
                to_date$ = datec$
                call "DATEOKC" (to_date$, temp%, errormsg$) : temp% = 0%
                call "DATEOKC" (datec$,   temp%, errormsg$) : temp% = 0%
                call "DATUFMTC" (datec$)
                return

L20300: REM Def/Enable Print Listing? (Y/N)        PRINT$
            if print$ = " " then print$ = "N"
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Date Range to Purge                                    ",~
         "Enter Print Listing? (Y/N)                                   "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      from_date$, print$, to_date$, from_date_range$,    ~
                      to_date_range$,  post_date$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
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
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40080,         /* Date Range        */   ~
                                L40085          /* Print Listing?    */
              goto L40100

L40080:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40085:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40100:     accept                                                       ~
               at (01,02),                                               ~
                  "Purge Completed Batch Control Records",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Date Range to Purge:   from             to", ~
               at (06,30), fac(lfac$( 1)), from_date$           , ch(10),~
                                                                         ~
               at (06,45), fac(lfac$( 1)), to_date$             , ch(10),~
                                                                         ~
               at (07,02), "Print Listing? (Y/N)",                       ~
               at (07,30), fac(lfac$( 2)), print$               , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40240
                  call "MANUAL" ("GLXPURGE") : goto L40100

L40240:        if keyhit% <> 15 then L40255
                  call "PRNTSCRN" : goto L40100

L40255:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40350     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                 (4)Previous Field      " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40330
                str(pf$(3%),64%)    = " "  : str(pfkeys$,16%,1%) = hex(ff)
L40330:     if fieldnr% > 1% then L40340
                str(pf$(2%),18%,26%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L40340:     return

L40350: if fieldnr% > 0% then L40395  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)PURGE Data  "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40395:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,         /* Date Range             */~
                              L50300          /* Print Listing?         */
            return
L50100: REM Test for Date Range to Purge
            call "DATEOKC" (from_date$, fr_date%, errormsg$)
                if errormsg$ <> " " then return
            call "DATEOKC" (to_date$, to_date%, errormsg$)
                if errormsg$ <> " " then return
            if fr_date% <= to_date% then L50170
                errormsg$ = "'From' Date can't be later than 'To' Date."
                return
L50170:     from_date_range$ = from_date$
            to_date_range$   = to_date$
            call "DATUFMTC" (from_date_range$)
            call "DATUFMTC" (to_date_range$)
            if to_date_range$ <= datec$ then L50290
                errormsg$ = "'To' Date cannot be greater than Today."
L50290:     return

L50300: REM Test for Print Listing? (Y/N)         PRINT$
            if print$ = "Y" or print$ = "N" then return
                errormsg$ = "Please enter 'Y' or 'N'."
            return

        REM The USINGs
L60010: %DATE: ######## #################################################~
        ~ GLXPURGE:######

L60040: %TIME: ########            GLEXPORT BATCH PURGE LISTING          ~
        ~      PAGE: ####

L60070: %                 FOR THE PERIOD: ########## through ##########

L60090: %      Module ID     Journal ID     Posting Seq     Posting Date ~
        ~  Document ID
L60095: %      Module ID     Journal ID     Posting Seq     Posting Date
L60100: %      ---------     ----------     -----------     ------------ ~
        ~  -----------
L60105: %      ---------     ----------     -----------     ------------
L60110: %         ##            ###            #####          ########   ~
        ~################
L60120: %                                                                ~
        ~################

L60140: %  *** END OF REPORT (########) ***

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
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
