        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y   CCC    CCC    SSS   PPPP    GGG    *~
            *  H   H  NN  N  Y   Y  C   C  C   C  S      P   P  G       *~
            *  HHHHH  N N N   YYY   C      C       SSS   PPPP   G GGG   *~
            *  H   H  N  NN    Y    C   C  C   C      S  P      G   G   *~
            *  H   H  N   N    Y     CCC    CCC    SSS   P       GGG    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYCCSPG - This program creates a report of a Session in  *~
            *            the DetailFile and has the option to PURGE the *~
            *            reported Session if that session is not Active.*~
            *            The report format is basically a dump of data. *~
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
            * 02/19/92 ! Original                                 ! RJ1 *~
            * 08/08/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            company$60,                  /* Company or Division Name   */~
            costsource$1,                /* Source of Cost Info        */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr_m(6),                  /* Descr Map For PlowCode     */~
            descr$42,                    /* Descr For PlowCode         */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            header$(3)79,                /* For PLOWCODE Call          */~
            i$(24)80,                    /* Screen Image               */~
            inc(2),                      /* Plowcode Arguments         */~
            inc$(2)16,                   /* Plowcode Arguments         */~
            inpmessage$79,               /* Informational Message      */~
            key$99,                      /* Miscellaneous Read/Plow Key*/~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pf16$16,                     /* PF(16) Screen Literals     */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            purgeline$25,                /* Accept Purge Desc Line     */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rptid$6,                     /* Report ID                  */~
            rpttitle$60,                 /* Report Title               */~
            rpttitle2$80,                /* Report Title Second Line   */~
            session$12,                  /* Session Name               */~
            testkey$99,                  /* Test Plowkey               */~
            time$8,                      /* System Time                */~
            userid$3                     /* Current User Id            */

            dim     /* ** Cycle Count Variables **                     */~
            actflag$1,                   /* Active Session Flag        */~
            actflagmsg$12,               /* Active Session Message     */~
            cntby$12,                    /* Who Counted the Item       */~
            cntdate$10,                  /* Date PI Started            */~
            cnttlernper$10,              /* Count Tolerance in Percent */~
            cnttlernqty$10,              /* Count Tolerance Quantity   */~
            costvar$10,                  /* Value Variance             */~
            deleteflag$1,                /* Records been purged Flag   */~
            enterby$12,                  /* Who Entered Count Amount   */~
            lastcntdate$10,              /* Date Part was Last Counted */~
            lastval1$44,                 /* Previous 1st Value in PrKey*/~
            lot$6,                       /* Lot Number of Part         */~
            part$25,                     /* Part Number                */~
            printtoltype$1,              /* Print Flag wrt Tolerance   */~
            purgemsg$60,                 /* Purge Print Message        */~
            purgeflag$1,                 /* Purge Flag                 */~
            qty$10,                      /* PI Quantity Count          */~
            sessiondesc$30,              /* Session Description        */~
            sourceflag$1,                /* Flag How Part Came to Sessn*/~
            store$3,                     /* Store/warehouse            */~
            unitvar$10,                  /* Unit Variance              */~
            varreason$6                  /* Variance Reason Code       */

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
            * #04 ! HNYCCMST ! Cycle Count Master File                  *~
            * #04 ! HNYCCDTL ! Cycle Count Session Detail File          *~
            * #06 ! HNYCCSYS ! Cycle Count System Control File          *~
            * #55 ! DUMMY    ! For PLOWCODE Extended Arguments          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #03, "HNYCCMST",                                      ~
                        varc,     indexed,  recsize = 796,               ~
                        keypos =    1,  keylen = 44,                     ~
                        alt key  1, keypos =   45, keylen =   6, dup,    ~
                            key  2, keypos =   81, keylen =   7, dup,    ~
                            key  3, keypos =   73, keylen =  15, dup     ~

            select #04, "HNYCCDTL",                                      ~
                        varc,     indexed,  recsize =  436,              ~
                        keypos =    1, keylen =  57,                     ~
                        alt key  2, keypos =   14, keylen =  44, dup,    ~
                            key  1, keypos =   13, keylen =  45, dup     ~

            select #06, "HNYCCSYS",                                      ~
                        varc,     indexed,  recsize =  249,              ~
                        keypos =    1, keylen =  12,                     ~
                        alt key  1, keypos =   43, keylen =  13, dup,    ~
                            key  2, keypos =   56, keylen =  10

            select #55, "DUMMY",                                         ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  42

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
            call "OPENCHCK" (#06, fs%(06), f2%(06), 0%, rslt$(06))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, ret%)
            ret% = ret%
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "          Cycle Count Session Print/Purge" &    ~
                        " Report                       "

            rptid$ = "HNY048"

            str(line2$,62) = "HNYCCSPG: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
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

            if purgeflag$ = "Y" and aukey% = 16% then extract_data

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles EDIT MODE for range selection screen.             *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       extract_data
                  if keyhit% <>  0% then       editpg1
            if cursor%(1%) <> 6% then L11124
                fieldnr% = 1%  :  goto  L11130
L11124:     if cursor%(1%) =  8% then fieldnr% = 2%                      ~
                                 else fieldnr% = 0%

L11130:     if fieldnr% < 1% or fieldnr% >  2% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11124

        REM *************************************************************~
            *           E X T R A C T   R E P O R T   D A T A           *~
            *-----------------------------------------------------------*~
            * Data Extraction section for report.                       *~
            *************************************************************
        extract_data
            gosub set_rpttitle2
            goto generate_report
          /* END Extract Data Section */

        set_actflag
            readkey$ = plowkey$
            call "READ100" (#06, readkey$, f1%(06))
            if f1%(06) = 0% then inputmode
            get #6 using  L13200, actflag$
            return

L13200:     FMT POS(43), CH(1)    /* HNYCCSYS  for Session Active Flag */

        set_rpttitle2
            rpttitle2$ = "          SESSION -            - IS "  :
            pos% = 39%
            purgemsg$ = " "
            str(rpttitle2$,21,12) = plowkey$
            if purgeflag$ <> "Y" then L13340
            pos% = 38%
            purgemsg$ = " AND HAS BEEN PURGED FROM THE FILE."
L13340:     if actflag$ <> "C" then L13350
                actflagmsg$ = "CLOSED"     : posplus% = 6%  : goto L13375
L13350:     if actflag$ <> "A" then L13360
                actflagmsg$ = "ACTIVE"     : posplus% = 6%  : goto L13375
L13360:     if actflag$ <> "P" then L13395
                actflagmsg$ = "PRE-ACTIVE" : posplus% = 10% : goto L13375

L13375:     str(rpttitle2$, pos%,  ) = actflagmsg$
            str(rpttitle2$, pos% + posplus%,  ) = purgemsg$
            return

L13395:     str(rpttitle2$,34,  ) = "HAS NO ACTIVITY FLAG SETTING"
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Session Name/Date      */~
                              L20300          /* Purge Flag             */
            return
L20100: REM Def/Enable Session Name            SESSION$


            return

L20300: REM Def/Enable Purge Flag                  PURGEFLAG$
            purgeline$ = "Purge Session Records"
            if actflag$ <> "C" and actflag$ <> "P"                       ~
                     then L20320
                purgeflag$ = "N"
*              PURGELINE$ = "Purge Session Records"
                return
L20320:     enabled% = 0%  :  purgeflag$ = "N"
            pf16$ = "(16)Print Record"
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Cycle Count Session Name                               ",~
         "Enter 'Y' to PURGE Session After Report                      "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, lastval1$,                 ~
                      costsource$, printtoltype$, session$, sessiondesc$,~
                      purgeline$, purgeflag$
            printflag% = 0%  :  deleteflag$ = " "
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
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************
        generate_report
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("RPTID", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */

            if lcntr% > 56% then gosub page_head

            call "SHOSTAT" ("PRINTING REPORT NOW")
            if pcntr% = 1% then gosub column_head

*         ** Start Report Loop **
            str(plowkey$,13) = all(hex(00))
            testkey$ = str(plowkey$,,12)
            str(testkey$,13) = all(hex(ff))

            call "READ104" (#04, plowkey$, f1%(04))
            if f1%(04) = 0% then  goto delete_session
            printflag% = 1%  :  goto L30310

        loop_session  /* HNYCCDTL Session Selection Loop */
            call "READNEXT" (#04, f1%(04))
            if  f1%(04) = 0% then  goto delete_session

L30310:     key$ = key(#04, 0%)
            if str(key$,,12) > testkey$ then goto delete_session

            get #04 using L35030, session$, actflag$, part$,              ~
                               store$, lot$, sourceflag$, cntdate$,      ~
                               enterby$, qty, unitcost, unitvar, costvar,~
                               cnttlernper, cnttlernqty, lastcntdate$,   ~
                               lastcntperiod$,varreason$, cntby$

            call "CONVERT" (qty        , 2.2, qty$)
            call "CONVERT" (unitcost   ,-2.2, unitcost$)
            call "CONVERT" (unitvar    ,-2.2, unitvar$)
            call "CONVERT" (costvar    ,-2.2, costvar$)
            call "CONVERT" (cnttlernper, 2.2, cnttlernper$)
            call "CONVERT" (cnttlernqty, 2.2, cnttlernqty$)
            call "DATEFMT" (lastcntdate$)
            call "DATEFMT" (cntdate$)

            if purgeflag$ = "Y" then gosub change_ccmaster_actflag

            lcntr% = lcntr% + 3%
            if lcntr% < 57 then L30540
                gosub page_head   :   gosub column_head

L30540:     print using L60310, part$, store$, lot$, cntdate$, qty$,      ~
                               cntby$, lastcntperiod$, cnttlernper$,     ~
                               cnttlernqty$, lastcntdate$, actflag$

            print using L60350, unitvar$, unitcost$, costvar$, varreason$,~
                               sourceflag$, enterby$
            print
            goto loop_session

        delete_session
            if purgeflag$ <> "Y" then goto end_report
                if actflag$ <> "C" and actflag$ <> "P"                   ~
                     then goto end_report
                /* **ELSE Delete the Record *** */
                call "DELETE" (#04, testkey$, 12%)
                call "DELETE" (#06, testkey$, 12%)
                /* End Delete Record unit */

            goto end_report
            /* **** End Print Loop Section **** */

        change_ccmaster_actflag
          /* Session Records are being purged so update CC Master File */
            if actflag$ = "A" then return  /*No Purge on Active records*/
            readkey$ = str(part$) & str(store$) & lot$
            call "READ101" (#3, readkey$, f1%(3))
            if f1%(3) = 0 then return          /* Should never do this */
            actflag$ = "C"
            put #3 using  L30820, actflag$             /* Set it Closed */
            rewrite #3

L30820:     FMT POS(81), CH(1)

            return

        end_report                   /* Report Ending Routine */
            if printflag% <> 0% then L33910
                print
                print using L60390    /* No Records Printed Message */

L33910:     print skip(2)
            time$ = " "   :   call "TIME" (time$)
            print using L64990, time$          /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)

            if deleteflag$ <> "Y" then L33950
                call "READ105" (#6, testkey$, f1%(6))
                if f1%(6) = 0% then L33950
                     delete #6      /* Remove the Session Record */
L33950:     goto inputmode

        page_head                   /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            if pcntr% = 0% then gosub print_params
            print page              /* Top of Form */
            print using L60070, date$, time$, company$, "HNYCCSPG", rptid$
            print using L60110, rpttitle$, pcntr%
            print using L60150, rpttitle2$
            print
            lcntr% = 4%
            return


        column_head
            print using L60190
            print using L60230
            print using L60270
            lcntr% = lcntr% + 3%
            return

        print_params           /* Print Page Zero */
            /* Onle one selection so no page zero printed */
            pcntr% = pcntr% + 1%
            return


        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: HNYCCDTL                          */~
            CH(12),         /* Session Name                            */~
            CH(1),          /* Active Session Flag                     */~
            CH(25),         /* Part Number                             */~
            CH(3),          /* Store                                   */~
            CH(16),         /* Lot                                     */~
            CH(1),          /* Flag Show How Part Got Into Session     */~
            CH(6),          /* Count Date                              */~
            CH(3),          /* Entered Count Quantity By               */~
            PD(14,4),       /* Number of Units Counted                 */~
            PD(14,4),       /* Unit Value                              */~
            PD(14,4),       /* Unit Variance                           */~
            PD(14,4),       /* Value Variance                          */~
            PD(14,4),       /* Count Tolerance Percentage              */~
            PD(14,4),       /* Count Tolerance Quantity                */~
            CH(6),          /* Last Count Date                         */~
            CH(3),          /* Last Count Period                       */~
            POS(131),       /*                                         */~
            CH(6),          /* Variance Reason Code                    */~
            CH(12)          /* Item Counted By                         */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40085,         /* Session Name/Date */   ~
                                L40085          /* Purge Flag        */
              goto L40110

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40085:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */



L40110:     accept                                                       ~
               at (01,02),                                               ~
                  "Print/Purge Cycle Count Session Detail",              ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Session Name        ",                       ~
               at (06,23), fac(lfac$( 1)),   session$           , ch(12),~
               at (06,40), fac(hex(8c)), sessiondesc$           , ch(30),~
                                                                         ~
               at (08,02), fac(hex(8c)),   purgeline$           , ch(22),~
               at (08,25), fac(lfac$( 2)),   purgeflag$         , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L41140
                  call "MANUAL" ("HNYCCSPG") : goto L40110

L41140:        if keyhit% <> 15 then L41170
                  call "PRNTSCRN" : goto L40110

L41170:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41360     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L41330
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L41340
L41330:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41340:     return

L41360: if fieldnr% > 0% then L41450  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        "
            str(pf$(3),64,16) =  pf16$
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L41450:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
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
            on fieldnr% gosub L50100,         /* Session Name/Date      */~
                              L50410          /* Purge Flag             */
            return

L50100: REM Test for Session Name                 SESSION$
            if session$ = " " or session$ = "?" then                     ~
                plowkey$ = all(hex(20)) else  plowkey$ = session$

            mat descr_m = zer : mat inc = zer : init(" ") inc$()
            header$(3) = hex(80) & "Select Session Name"
            header$(1) = "  Session     Act Flag   Description"

            descr_m(01) =     1.12  : descr_m(02) = 0001.0
            descr_m(03) =    43.01  : descr_m(04) = 0016.0
            descr_m(05) =    13.30  : descr_m(06) = 0024.0

            call "PLOWCODE" (#06, plowkey$, descr$, 9000%, 0.42, f1%(06),~
                             header$(), 0, 0, inc(), inc$(), "D", " ",   ~
                             #55, descr_m())

            if f1%(06) = 1% then L50350
                errormsg$ = "Session Not On File" : return
L50350:     session$ = plowkey$
            call "DESCRIBE" (#06, plowkey$, sessiondesc$, 0%, f1%(06))

            gosub set_actflag  /*Set the Acitive Flag from Session file*/

            return

L50410: REM Test for Purge Flag           PURGEFLAG$
            if enabled% <> 0% then L50440
                purgeflag$ = "N"  :  return
L50440:     p% = pos("YN" = purgeflag$)
            if p% = 0% then goto L50550
            if p% = 2% then goto L50540
L50470:     call "ASKUSER" (aukey%, "** PURGE CYCLE COUNT SESSION **  ", ~
                            "PRESS PF(16) TO ALLOW PURGE OF          " & ~
                            "              SESSION RECORDS",             ~
                            "             -- OR --",                     ~
                            "PRESS (RETURN) TO PRINT SESSION RECORDS ")
            if aukey% = 16% then goto L50560
            if aukey% <> 0% then goto L50470
L50540:     purgeflag$ = "N"  :   pf16$ = "(16)Print Record"  :  return
L50550:     errormsg$ = "'Y' or 'N' Please."                  :  return
L50560:     pf16$ = "(16)Purge Record"                        :  return
          /* ** End Purge Test ** */

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:######

*       * Header Line 2
L60110: %                                     ###########################~
        ~#################################                     PAGE: ####

*       * Header Line 3
L60150: %                                     ###########################~
        ~################################################################

*       * Column Header Line 1
L60190: %                                          COUNT       COUNT     ~
        ~  COUNT         COUNT    TOLERANCE  TOLERANCE   LAST        ACT


*       * Column Header Line 2
L60230: %PART NUMBER                STORE  LOT     DATE        QUANTITY  ~
        ~  BY            PERIOD   PERCENT    QUANTITY    COUNT DATE  FLAG


*       * Column Header Line 3
L60270: %-------------------------  -----  ------  ----------  ----------~
        ~  ------------  -------  ---------  ----------  ----------  ----


        %** Report Line 1
L60310: %#########################   ###   ######   ########   ##########~
        ~  ############     ###   #########  ##########   ########     #


        %** Report Line 2
L60350: %    VAR QUAN: ##########  UNIT VAL: ##########  VAR VAL: #######~
        ~###  VAR REASON: ######   CYCLE SOURCE: #  ENTERED BY: ###

        %** Null Printing Message
L60390: %                                       * * * No Records Printed ~
        ~* * *

        %** Report Title for page 0
        %############################################################

L64990:         %                          * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   @   ########   * * * * * * * * * *

        REM THISPROGRAMWASGENERATEDBYGENRPPGMAPROPRIETRYPRODUCTOFCAELUS**~
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
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
