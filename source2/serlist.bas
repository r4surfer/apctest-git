        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   EEEEE  RRRR   L      IIIII   SSS   TTTTT          *~
            *  S      E      R   R  L        I    S        T            *~
            *   SSS   EEEE   RRRR   L        I     SSS     T            *~
            *      S  E      R   R  L        I        S    T            *~
            *   SSS   EEEEE  R   R  LLLLL  IIIII   SSS     T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SERLIST  - Prints selected listings of serial numbers by  *~
            *            part numbers selected by the operator.         *~
            *----------------------------------------------------------Q*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/02/87 ! Original                                 ! JIM *~
            * 05/12/87 ! Made report presentation more presentable! LDJ *~
            * 06/09/87 ! HNYMASTR Format change                   ! MJB *~
            * 08/26/91 !(NO PRR) Added the PF(14) function to call! RJB *~
            *          !   'PICKYEAR' to allow for listing of the !     *~
            *          !   Archieved Serial Number Files. Added   !     *~
            *          !   call to 'ALLFREE'.  Brought the DEFAULT!     *~
            *          !   -ENABLE and TEST sections up to Stand. !     *~
            * 09/05/91 !QC-FIXES Correct Branch for STATCD in the ! RJB *~
            *          !   DEFAULT-ENABLES section.               !     *~
            * 10/20/92 ! Removed FACs from page zero.             ! JIM *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            choice$4,                    /* Archieve Year Selected     */~
            compname$46,                 /* Company name               */~
            currentfile$23,              /* Serial # Master in use     */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fileid$4,                    /* File Prefix to Find 'SERM' */~
            frpart$25, frdesc$34,        /* From Part Number & Descript*/~
            hipart$25,                   /* Last part # to plow        */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            lopart$25,                   /* First part # to plow       */~
            part$25, desc$34,            /* 'Generic' part # & descrip */~
            pf14$23,                     /* PF 14 Screen Literal       */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rptid$6,                     /* Report ID                  */~
            runtime$8,                   /* Time of day                */~
            savpart$25,                  /* For part # breaks          */~
            serdata$30,                  /* Part loc info- SERMASTR    */~
            serial$20,                   /* Serial #- SERMASTR         */~
            sermprname$8,                /* Serial # Master File Name  */~
            stat40$1,                    /* Status code desired OR'd 40*/~
            statcd$1,                    /* Status code desired        */~
            status$1,                    /* Status code-SERMASTR       */~
            status$(10)40,               /* S/N Status Descriptions    */~
            statbf$1,                    /* Status code-SERMASTR AND BF*/~
            topart$25, todesc$34,        /* Thru Part Number & Descript*/~
            trantype$2,                  /* Transaction Type           */~
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
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "
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
            * # 1 ! SERMASTR ! Serial Number Tracking Master File       *~
            * # 2 ! HNYMASTR ! Inventory Master File                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select # 2, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (2%, compname$, u3%)
            rptid$ = "SER004"
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            status$(01) = "Issued out of Inventory"
            status$(02) = "Currently being Built (WIP)"
            status$(03) = "Currently in Inventory"
            status$(04) = "S/N is being Used in A Job (WIP Component)"
            status$(05) = "S/N has been Sold/Shipped"
            status$(06) = "S/N Now a component of another Part"
            status$(07) = "S/N Temporarily Assigned/Created"
            status$(08) = "Temporarily reserved by User/Transaction"
            status$(09) = "Not Used"
            status$(10) = "Serial Number Issued but not used."

            currentfile$ = "Current DataBase File."
            str(line2$,1,40) = "Archive File Year: " & currentfile$
            str(line2$,62%) = "SERLIST: " & str(cms2v$,,8%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$, pf5$ = " "
            pf14$ = "(14)Select Archive Year"
            pf16$ = "(16)Exit Program"
            gosub L29000

            for fieldnr% = 1 to  3
                if fieldnr% > 1 then pf4$ = "(4)Previous Field"
L10120:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10240
L10140:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10220
L10170:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10140
                         if fieldnr% = 1% then L10120
                         goto L10170
L10220:               if keyhit% = 16% then exit_program
                      if keyhit% <>  0 then       L10140
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10140
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            pf4$, pf5$ = " "
            pf16$ = "(16)Print List"
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       dataprint
                  if keyhit% <>  0 then       editpg1
L11160:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  3 then editpg1
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
                  pf4$, pf5$, pf16$ = " "
L11220:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11220
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11220
                  lastfieldnr% = fieldnr%
            goto L11160

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        dataprint
            call "TIME" (runtime$)
            if statcd$ = " " then str(i$(8),15) = "ALL"
            plowkey$ = lopart$

L19050:     call "PLOWNEXT" (#1, plowkey$, 0%, f1%(1))
            if f1%(1) = 0% then end_report
            get #1 using L19065, status$, serdata$,serial$,part$,trantype$
L19065:         FMT  CH(1), CH(30), CH(20), CH(25), POS(216),CH(2)
            if part$ > hipart$ then end_report
            if statcd$ = " " then L19095
                if status$ =  statcd$ then L19095
                if status$ <> stat40$ then L19050

L19095:     if prt% <> 0% then L19165
                call "SHOSTAT" ("Printing requested items")
                gosub page_0_head
                print skip (4)
L19111:         i% = pos(str(i$()) > hex(7f))
                if i% = 0% then goto L19115
                     str(i$(), i%, 1%) = " "
                     goto L19111
L19115:         print using L19670, "   ---- SELECTION CRITERIA ----"
                print
                print using L19670, str(line2$,1,40)
                print
                for n% = 6% to 12%
                    print using L19670, i$(n%)
                next n%
                print " Legend:   Status Code  Meaning"
                for n% = 1% to 10%
                    print "              "; n%-1%; "       "; status$(n%)
                next n%

L19165:     if part$ = savpart$ then L19190
                savpart$ = part$
                call "DESCRIBE" (#2, part$, desc$, 1%, f1%(2))
                if line% > 53% then gosub print_headers                  ~
                               else gosub print_part
L19190:     if line% > 53% then gosub print_headers
            statbf$ = status$ and hex(bf)
            s% = pos("0123456789" = statbf$)
            on s% goto L19225, L19240, L19260, L19280, L19300, L19330, L19345,  ~
                       L19345, L19360,      , L19375
            s% = 9%

L19225:     print using L19660, serial$, statbf$, serdata$         /* 0 */
            goto L19390

L19240:     print using L19660, serial$, statbf$, "Job: " &        /* 1 */~
                               str(serdata$,,8)
            goto L19390

L19260:     print using L19660, serial$, statbf$, "Store: " &      /* 2 */~
                        str(serdata$,,3) & "  Lot: " & str(serdata$,4,16)
            goto L19390

L19280:     print using L19660, serial$, statbf$, "Job: " &        /* 3 */~
                     str(serdata$,,8) & "  Parent: " & str(serdata$,9,20)
            goto L19390

L19300:     print using L19660, serial$, statbf$, "Cust: " &       /* 4 */~
                      str(serdata$,,9) & "  Inv: " & str(serdata$,10,8) &~
                     "  Line: " & str(serdata$,18,4) & "/" &             ~
                                  str(serdata$,22,4)
            goto L19390

L19330:     print using L19660, serial$, statbf$, "Parent: " & serdata$
            goto L19390

L19345:     print using L19660, serial$, statbf$, serdata$         /* 6 */
            goto L19390

L19360:     print using L19660, serial$, statbf$, serdata$         /* 7 */
            goto L19390

L19375:     print using L19660, serial$, statbf$, serdata$         /* 9 */
            goto L19390

L19390:     line% = line% + 1%
            if status$ < hex(40) and status$ >= "0" then L19050
                print using L19685, trantype$
                line% = line% + 1%
                goto L19050

        print_headers
            page% = page% + 1%
            line% = 0%
            prt% = 1%  /*  INDICATE 'SOMETHING PRINTED'  */
        page_0_head
            print page
            print using L19610, date$, compname$, "-" & rptid$
            print using L19620, runtime$, page%
            if prt% = 0% then return
            gosub print_part
            return

        print_part
            print skip (2)
            print using L19630, part$, desc$
            print
            print using L19640
            print using L19650
            line% = line% + 6%
            return

        printer_closer
            if prt% = 0% then L19550
                print
                print using L19680
L19550:     close printer
            call "SETPRNT" (rptid$, " ", 1%, 1%)
            return

        end_report
            if prt% <> 0% then inputmode
                call "ASKUSER" (2%, "NOTHING SELECTED",                  ~
                                "No records were found within the run",  ~
                                "parameters entered.",                   ~
                                "Press (RETURN) to continue...")
                goto inputmode

L19610: %RUN DATE: ######## #############################################~
        ~# SERLIST#######
L19620: %    TIME: ########         SERIAL NUMBER LISTING BY PART        ~
        ~       PAGE ####
L19630: %Part: ######################### ################################~
        ~##
L19640: %  Serial Number         Stat  Current Location / Usage

L19650: %  --------------------  ----  ----------------------------------~
        ~----------------
L19660: %  ####################    #   ##################################~
        ~################
L19670: %################################################################~
        ~################
L19680: %                    *** End of Report ***
L19685: %                              *** Currently held by a ## Transac~
        ~tion ***

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20140,         /* From Part Number   */    ~
                              L20190,         /* Thru Part Number   */    ~
                              L20240          /* Status code        */
            return

L20140: REM Def/Enable From Part Number            FRPART$
            if frpart$ = " " then frpart$ = "ALL"
            inpmessage$ = "Starting Part Number, 'ALL', 'FIRST', or '?' "~
                        & "for available Part Numbers."
            return

L20190: REM Def/Enable Thru Part Number            TOPART$
            if frpart$ <> "ALL" then L20220
                enabled% = 0%
                goto L20230
L20220:     inpmessage$ = "Ending Part Number, 'LAST', '?' for available"~
                        & " Part Numbers or Blank."
L20230:     return

L20240: REM Def/Enable Status code                 STATCD$
            inpmessage$ = "Enter Status Code (0 - 9) or blank for 'ALL'"
            return

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            call "ALLFREE"
            init(" ") errormsg$, inpmessage$, desc$, frpart$, frdesc$,   ~
                      hipart$, lopart$, part$, desc$, runtime$, topart$, ~
                      todesc$, savpart$, statcd$, stat40$


            if print_on% <> 0% then gosub printer_closer
            select printer (134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            prt%, page% = 0%
            line% = 99%
            print_on% = 1%
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40160,         /* From Part Number  */   ~
                                L40160,         /* Thru Part Number  */   ~
                                L40170          /* Status Code       */
              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40170:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02),                                               ~
                  "Serial Number Listing(s) by Part",                    ~
               at (01,66), "Date:",                                      ~
               at (01,72), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "From Part #:",                               ~
               at (06,15), fac(lfac$( 1)), frpart$              , ch(25),~
               at (06,42), fac(hex(8c)),   frdesc$              , ch(34),~
                                                                         ~
               at (07,02), "Thru Part #:",                               ~
               at (07,15), fac(lfac$( 2)), topart$              , ch(25),~
               at (07,42), fac(hex(8c)),   todesc$              , ch(34),~
                                                                         ~
               at (08,02), "Status Code:",                               ~
               at (08,15), fac(lfac$( 3)), statcd$              , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,40), fac(hex(8c)), pf14$                          ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(0100000405000000000000000d0e0f10)),              ~
               key (keyhit%)

               if keyhit% <> 13 then L40511
                  call "MANUAL" ("SERLIST ")
                  goto L40190

L40511:        if keyhit% <> 14% then L40520
                  gosub select_year
                  goto L40190

L40520:        if keyhit% <> 15 then L40560
                  call "PRNTSCRN"
                  goto L40190

L40560:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50130,         /* From Part Number  */     ~
                              L50300,         /* Thru Part Number  */     ~
                              L50460          /* Status code       */
            return

L50130: REM Test for From Part Number             FRPART$
            if frpart$ <> "ALL" then L50155
                topart$, todesc$, frdesc$ = " "
                init(hex(00)) lopart$ : init(hex(ff)) hipart$
                goto L50265
L50155:     if frpart$ <> "FIRST" then L50175
                todesc$, frdesc$ = " "
                topart$ = "LAST"
                goto L50250
L50175:     if frpart$ <> " " then L50190
L50180:         errormsg$ = "The Starting Part Number CANNOT be Blank."
                goto L50265
L50190:     if frpart$ <> "?" then L50200
                frpart$ = " "
L50200:     frdesc$ = hex(06) & "Select a beginning Part Number"
            plowkey$ = str(frpart$,,25)
            call "PLOWCODE" (#1, plowkey$, frdesc$, -8025%, -.32, f1%(1),~
                            null$(), 0, 0, null(), null$(), " ", " ", #2)
                if f1%(1) = 1% then goto L50240
                     if frpart$ = " " then L50180
                          frdesc$ = " "
                          goto L50250
L50240:         frpart$ = str(plowkey$,,25)
                call "PUTPAREN" (frdesc$)
L50250:     if topart$ = " " then L50265
                call "TESTRNGE" (frpart$, topart$, lopart$, hipart$,     ~
                                                               errormsg$)
L50265:     return

L50300: REM Test for Thru Part Number             TOPART$
            if frpart$ <> "ALL" then L50325
                frdesc$, topart$, todesc$ = " "
                init(hex(00)) lopart$ : init(hex(ff)) hipart$
                goto L50440
L50325:     if topart$ <> frpart$ then L50340
                todesc$ = frdesc$
                goto L50430
L50340:     if topart$ <> "LAST" then L50355
                todesc$ = " "
                goto L50430
L50355:     if topart$ = " " then L50405
            if str(topart$,1,1) <> "?" then L50370
                topart$ = " "
L50370:     todesc$ = hex(06) & "Select an ending Part Number"
            plowkey$ = str(topart$,,25)
            call "PLOWCODE" (#1, plowkey$, todesc$, -8025%, -.32, f1%(1),~
                            null$(), 0, 0, null(), null$(), " ", " ", #2)
                if f1%(1) = 1% then L50420
                    todesc$ = " "
                if topart$ <> " " then L50430
L50405:              topart$ = frpart$
                     todesc$ = frdesc$
                     goto L50430
L50420:         topart$ = str(plowkey$,,25)
                call "PUTPAREN" (todesc$)
L50430:     call "TESTRNGE" (frpart$, topart$, lopart$, hipart$,         ~
                                                               errormsg$)
L50440:     return

L50460: REM Test for Status Code                  STATCD$
            stat40$ = " " : if statcd$ = " " then return
            if pos("0123456789" = statcd$) <> 0% then L50520
                errormsg$ = "Status Code must be 0, 1, 2, 3, 4, 5, 6, 7"&~
                     ", 8, 9, or blank for 'ALL'"
                return
L50520:     stat40$ = statcd$ or hex(40)
            return

        REM *************************************************************~
            *         S E L E C T    A R C H I V E    Y E A R           *~
            *-----------------------------------------------------------*~
            * Allows User to select from existing Archive Files         *~
            *************************************************************

        select_year
            fileid$ = "SERM"
            call "PICKYEAR" (fileid$, choice$)
             if f2%(1) = 0% then close #1
             if choice$ = "CURR" or choice$ = " "  then L55120 else L55130
L55120:      sermprname$="SERMASTR" : goto L55150
L55130:      sermprname$="SERM" & choice$
L55150:      call "PUTPRNAM" addr(#1, sermprname$)
             call "OPENCHCK"(#1, fs%(1), f2%(1), 0%, rslt$(1))
            if choice$ = "CURR" or choice$ = " "                         ~
                        then str(line2$,20,25) = "Current DataBase File."~
                        else str(line2$,20,25) = choice$ & hex(00)
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
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")
            if print_on% <> 0% then gosub printer_closer
            end
