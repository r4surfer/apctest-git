        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  BBBB    OOO   M   M  FFFFF  L       AAA    GGG    SSS    *~
            *  B   B  O   O  MM MM  F      L      A   A  G      S       *~
            *  BBBB   O   O  M M M  FFFF   L      AAAAA  G GGG   SSS    *~
            *  B   B  O   O  M   M  F      L      A   A  G   G      S   *~
            *  BBBB    OOO   M   M  F      LLLLL  A   A   GGG    SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMFLAGS - Set Bill of Material Behavior Switches         *~
            *            (Including BOM Approvals)                      *~
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
            * 01/04/88 ! ORIGINAL                                 ! TLJ *~
            * 04/02/91 ! Corrected Write to SYSFILE2 (PRR # 11740)! RJB *~
            *          ! Added call to "ALLFREE" per new standards! RJB *~
            * 06/17/91 ! QC - Moved ALLFREE to the right place.   ! RJB *~
            * 03/12/93 ! PRR 10641 - Flag for BOMINPUT to enable  ! RJH *~
            *          !   or not the Location Screen.            !     *~
            * 06/10/94 ! Added switches for Formula Calculation   ! LDJ *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC
        dim                                                              ~
            apprflag$1,                  /* Aprroval Flag              */~
            apprmethod$1,                /* Aprroval Method            */~
            apprby$15,                   /* Approved By                */~
            appron$6,                    /* Approved On                */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            errormsg$79,                 /* Error message              */~
            fillone$250,                 /* SYSFILE2 unused space one  */~
            filltwo$228,                 /* SYSFILE2 unused space two  */~
            form_calc_flag$1,            /* Formula Calculations Used? */~
            form_calc_mod$1,             /* Values modifiable @ runtime*/~
            i$(24)80,                    /* Screen Image               */~
            initappr$1,                  /* Initial Approval Flag      */~
            initmeth$1,                  /* Initial Approval Method    */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second screen line         */~
            locscreen_flag$1,            /* Location Screen Enable Flag*/~
            material_usage$10,           /* Material Usage %           */~
            methdescr$39,                /* Method Description"        */~
            pf$(3)79,                    /* PF Key Prompts             */~
            pfk$10,                      /* PF Keys Available          */~
            readkey$99,                  /* Read key for PLOWCODE      */~
            switchkey$20,                /* Key for SYSFILE2           */~
            userid$3                     /* User ID                    */

        dim f2%(04),                     /* = 0 if the file is open    */~
            f1%(04)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            * #2  ! BOMMASTR ! BOM Master File                          *~
            *************************************************************~
            *       FILE SELECTION AND OPEN CALLS                       *~
            *************************************************************~

            select #1,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =     1, keylen =  20

            select #2,  "BOMMASTR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56          ~

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, 0%, f2%(1%), 100%, " ")
            call "OPENCHCK" (#2, 0%, f2%(2%), 100%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date  :  call "DATEFMT" (date$)
            switchkey$ = "SWITCHS.BOM"

*        See if this User is a Data Base or Module Administrator
            call "CMSMACHK" ("BOM", lfac$(1%), lfac$(2%))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then L09160
            pf$(1) = "You must be a Data Base or BOM Module Administrato"~
                     & "r to run this program."
            call "ASKUSER" (0%, "SECURITY CHECK", " ", pf$(1%), " ")
            goto exit_program

L09160
*        Set some variables
            str(line2$,62) = "BOMFLAGS: " & cms2v$

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * No real input mode - just load defaults from disk.        *~
            *************************************************************

        inputmode:
            call "ALLFREE"
            init(" ") errormsg$, inpmessage$, apprflag$, apprmethod$,    ~
                      fillone$, filltwo$, initappr$, initmeth$,          ~
                                      locscreen_flag$,                   ~
                      form_calc_flag$, form_calc_mod$, material_usage$
            gosub load_data


L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editpg1:
            gosub'051(0%)                /* Set Input Messages         */
L11060:     gosub'111(0%)                /* Edit Screen                */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  9 then       mod_admin
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 32 then       exit_program
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% then fieldnr% = 1%
            if fieldnr% > 6% then fieldnr% = 3%

            gosub'051(fieldnr%)          /* Set Input Messages         */
                  if enabled% = 0% then L11060
L11160:     gosub'111(fieldnr%)          /* Edit Screen                */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11160
            gosub'151(fieldnr%)          /* Test Data Entered          */
                  if errormsg$ <> " " then L11160
            goto editpg1


        mod_admin    /* Allow maintenance of BOM Module Administrators */
            call "CMSMAINP" ("BOM", "Bills of Materials")
            goto L11000


        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            * --------------------------------------------------------- *~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave:
            if initappr$ <> "Y" or apprflag$ <> "N" then L19370
            n% = 2%
            call "ASKUSER" (n%, "WARNING",                               ~
                             "You have modified the BOM APPROVAL flag " &~
                             "from 'Y' to 'N'.",                         ~
                             "ALL BOMs will be UNAPPROVED.",             ~
                             "Press RETURN to acknowledge or PF1 to "   &~
                             "reset to 'Y'.")
            if n% = 0% then L19190
                apprflag$ = "Y"
                apprmethod$ = initmeth$
                goto editpg1
L19190:     call "SHOSTAT" ("ALL BOMS ARE BEING UNAPPROVED.")
            readkey$ = all(hex(00))
            unapprove_loop:
                call "PLOWNXT1" ( #2%, readkey$, 0%, f1%(2) )
                if f1%(2) = 0% then L19370
                if (str(readkey$,29,3)) = "  0" then L19250
                  put #2 using L19242, " "
L19242:               FMT   POS(103), CH(3)
                  rewrite #2
                  goto unapprove_loop
L19250:         get #2 using L19260, appron$
L19260:            FMT    POS(115), CH(6)
                if appron$ = " " then unapprove_loop /* NOT APPROVED */

                appron$, apprby$ = all(" ")
                put #2 using L19310, appron$, apprby$, date, userid$
L19310:            FMT    POS(115), CH(6), CH(15), CH(6), CH(3)

                rewrite #2
            goto unapprove_loop

L19370:     gosub L31000                  /* Save Data                  */
            goto  exit_program


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * --------------------------------------------------------- *~
            * Sets DEFAULTS and ENABLES fields for the Page 1 of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            if fieldnr% <> 0% then L20110
                inpmessage$ = "Place cursor at field to modify and" &    ~
                " Press (RETURN); PF-32 to EXIT program."
                return
L20110:     enabled% = 1%

            on fieldnr% gosub L20170,                /* Approval Flag   */~
                              L20290,                /* Approval Method */~
                              L20400,                /* Loc Screen Flag */~
                              L20500,                /* Form Calc  Flag */~
                              L20600,                /* Form Calc Mod   */~
                              L20700                 /* Material Usage %*/
               return

L20170
*        Default/Enable for BOM APPROVAL             APPRVFLAG$
            call "READ101" (#1, "PLANNING SYSTEM FLAG", f1%(1))
            if f1%(1) = 1%  or initappr$ = "Y"  then L20270
L20200:     call "ASKUSER" (n%, "ERROR",                                 ~
                             "The PLANNING SYSTEM FLAG record does "    &~
                             "not exist.",                               ~
                             "The APPROVAL FLAG cannot be set.",         ~
                             "Press RETURN to acknowledge.")
            if n% <> 0% then L20200
L20270:     inpmessage$ = "Enter 'Y' or 'N'."
            return

L20290
*        Default/Enable for APPROVAL METHOD           APPRMETHOD$
            if apprflag$ = "Y" then L20310
              enabled% = 0%
              errormsg$="Method is valid only if Approval Required "    &~
                                                  "Flag is set to 'Y'."
              return
L20310:     inpmessage$ = "(A)Auto Apprv Components, (D)Don't Apprv Unles~
        ~s Components Approved, (P)Prompt"
            return

L20400
*        Default/Enable for Location Screen Enable    LOCSCREEN_FLAG$
            if locscreen_flag$ = "Y" then L20440 else L20420
L20420:         inpmessage$ = "Enter 'Y' to Enable the Location Screen" &~
                              " in BOMINPUT."
                    return
L20440:         inpmessage$ = "Enter 'N' to Disable the Location Screen"&~
                              " in BOMINPUT."
                    return

L20500
*        Default/Enable for FORMULA CALCULATION FLAG  FORM_CALC_FLAG$
            inpmessage$ = "'N'=Formula Calculation Disabled; 'Y'=Enabled"
            if form_calc_flag$ = " " then form_calc_flag$ = "N"
            return

L20600
*        Default/Enable for FORMULA CALC MODIFICATION FORM_CALC_MOD$
            if form_calc_flag$ = "Y" then L20650
                enabled% = 0%
                form_calc_mod$ = " "
                return
L20650:     inpmessage$ = "'Y'=Formula Variables modifiable; 'D'=Displaye~
        ~d only; 'N'=No Display"
            if form_calc_mod$ = " " then form_calc_mod$ = "N"
            return

L20700
*        Default/Enable for MATERIAL USAGE %          MATERIAL_USAGE$
            inpmessage$ = "Enter a Material Usage % Factor for use in BOM~
        ~ Formula Calculation"
            return

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
            call "STARTOVR" (keyhit1%)
                if keyhit1% = 1% then return

                return clear all
                goto inputmode

        REM *************************************************************~
            *                  L O A D   D A T A                        *~
            * --------------------------------------------------------- *~
            * Pull data from switch's string and format.                *~
            *************************************************************
        load_data:
            call "READ100" (#1, switchkey$, f1%(1))
            if f1%(1) = 1% then L30100
                apprflag$ = "N"
                init(" ") apprmethod$, fillone$, filltwo$, form_calc_mod$
                form_calc_flag$ = "N"
                goto L30120
L30100:     get #1 using L30110, apprflag$, apprmethod$, locscreen_flag$, ~
                                form_calc_flag$, form_calc_mod$,         ~
                                material_usage$,                         ~
                                fillone$, filltwo$
L30110:          FMT POS(21),  5*CH(01), CH(10), CH(238), CH(227)
L30120:     initappr$ = apprflag$
            initmeth$ = apprmethod$
            if form_calc_flag$ = " " then form_calc_flag$ = "N"
            if apprflag$ = " " then apprflag$ = "N"
            if locscreen_flag$ = " " then locscreen_flag$ = "Y"
            return

        get_method_descr:
            methdescr$ = " "
            if apprmethod$ = "A" then methdescr$ =                       ~
        "Automatically Approve Components"
            if apprmethod$ = "P" then methdescr$ =                       ~
        "Prompt for Automatic Approval"
            if apprmethod$ = "D" then methdescr$ =                       ~
        "Don't Approve Unless Comp. are Approved"
            return

L31000: REM *************************************************************~
            *                  S A V E   D A T A                        *~
            * --------------------------------------------------------- *~
            * Write switchs back to file                                *~
            *************************************************************
            call "READ101" (#1, switchkey$, f1%(1%))
            put #1 using L31070,  switchkey$, apprflag$, apprmethod$,     ~
                                 locscreen_flag$,                        ~
                                 form_calc_flag$, form_calc_mod$,        ~
                                 material_usage$,                        ~
                                 fillone$, filltwo$
L31070:         FMT CH(20), 5*CH(01), CH(10), CH(238), CH(227)
            if f1%(1%) = 0% then write #1 else rewrite #1

            call "READ101" (#1, "PLANNING SYSTEM FLAG", f1%(1))
            if f1%(1) = 0% then return
            put #1 using L31110, "PLANNING SYSTEM FLAG", apprflag$
L31110:         FMT CH(20), POS(226), CH(1)
            rewrite #1
            return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            * --------------------------------------------------------- *~
            * Screen for Editing Page 1 of document.                    *~
            *************************************************************

        deffn'111(fieldnr%)
            if fieldnr% = 0% then init(hex(86)) lfac$()  else            ~
                                  init(hex(8c)) lfac$()
            gosub set_pf_keys

                  on fieldnr% gosub L41230,        /* Aprroval Flag     */~
                                    L41230,        /* Aprroval Method   */~
                                    L41230,        /* Location Screen   */~
                                    L41230,        /* Form Calc Flag    */~
                                    L41230,        /* Form Calc Mod     */~
                                    L41285         /* Material Usage %  */
                     goto L41330

L41230:           REM SET FAC'S FOR UPPER CASE INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L41285:           lfac$(fieldnr%) = hex(82) : return   /* Numeric     */

L41330:   accept                                                         ~
            at (01,02), "Manage BOM Module Behavior Switches",           ~
            at (01,67), "Date:",                                         ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (06,02),                                                  ~
             "BOM Approval Required?",                                   ~
            at (06,39), fac(lfac$(1%)), apprflag$               , ch(01),~
                                                                         ~
            at (07,02),                                                  ~
             "Approval Method",                                          ~
            at (07,39), fac(lfac$(2%)), apprmethod$             , ch(01),~
            at (07,41), fac(hex(8c)), methdescr$                , ch(39),~
                                                                         ~
            at (08,02),                                                  ~
             "Enable Location Screen in BOMINPUT?",                      ~
            at (08,39), fac(lfac$( 3%)), locscreen_flag$        , ch(01),~
                                                                         ~
            at (09,02),                                                  ~
             "Formula Calculation Enabled?",                             ~
            at (09,39), fac(lfac$(4%)), form_calc_flag$         , ch(01),~
                                                                         ~
            at (10,02),                                                  ~
             "Display/Modify Formula Variables?",                        ~
            at (10,39), fac(lfac$( 5%)), form_calc_mod$         , ch(01),~
                                                                         ~
            at (11,02),                                                  ~
             "Formula Calculation Material Usage %",                     ~
            at (11,39), fac(lfac$( 6%)), material_usage$        , ch(10),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)), pf$(1)                     ,ch(79),~
            at (23,02), fac(hex(8c)), pf$(2)                     ,ch(79),~
            at (24,02), fac(hex(8c)), pf$(3)                     ,ch(79),~
                                                                         ~
                keys(pfk$),                                              ~
                key (keyhit%)

                if keyhit% <> 13 then L41860
                     call "MANUAL" ("BOMFLAGS")
                     goto L41330

L41860:         if keyhit% <> 15 then L41900
                     call "PRNTSCRN"
                     goto L41330

L41900:         close ws
                call "SCREEN" addr("C", 0%, "I", i$(), cursor%())
                return

        set_pf_keys
          gosub get_method_descr
          if fieldnr% <> 0% then L42102
            pf$(1) = "(1)Start Over       (9)Maintain BOM Modu" &        ~
                     "le Administrators      (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                    (16/32)Save/Exit   "
            str(pf$(3),60,1) = hex(84)
            pfk$ = hex(000102090d0f1020ffffff)
            return

L42102:     pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfk$ = hex(00010d0fffffffffffff)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Page 1.                        *~
            *************************************************************

        deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50110,         /* Approval Flag   */ ~
                                    L50500,         /* Approval Method */ ~
                                    L50600,         /* Location Screen */ ~
                                    L50650,         /* Form Calc Flag  */ ~
                                    L50700,         /* Form Calc Mods? */ ~
                                    L50760          /* Material Usage %*/
                  return

L50110
*        Test Data for APPROVAL FLAG
            if apprflag$ = "Y" or apprflag$ = "N" then L50170
                  errormsg$ = "Enter 'Y' or 'N'."
                  return

L50170:     if initappr$ <> apprflag$ then L50180
                  apprmethod$ = initmeth$
                  return

L50180:     n% = 2%

            if apprflag$ = "Y" then                                      ~
              pf$(1) = "If saved, BOMs will have to be APPROVED "       &~
                       "prior to planning."                              ~
            else                                                         ~
              pf$(1) = "If saved all approved BOMs will be UNAPPROVED."

L50250:     call "ASKUSER" (n%, "WARNING",                               ~
                             "You have modified the BOM APPROVAL flag " &~
                             "from its initial value.",                  ~
                             pf$(1),                                     ~
                             "Press RETURN to acknowledge or PF1 to "   &~
                             "reset to initial value.")
            if n% = 0% then L50330 : if n% <> 1% then L50250
            apprflag$ = initappr$ : return
L50330:     if apprflag$ = "Y" then apprmethod$="D" else apprmethod$=" "
            return

L50500
*        Test Data for APPROVAL METHOD            APPRMETHOD$
            if apprmethod$ = "A" or apprmethod$ = "D" or                 ~
                                             apprmethod$ = "P" then L50540
               errormsg$ = "ENTER 'A', 'D' or 'P'"
               goto L50560
L50540:     initmeth$ = apprmethod$
L50560:     return

L50600
*        Test Data for Location Screen Flag       LOCSCREEN_FLAG$
            if locscreen_flag$ = "Y" or locscreen_flag$ = "N" then return
                  errormsg$ = "Enter 'Y' or 'N'."
                  return

L50650
*        Test Data for Formula Calculation Flag   FORM_CALC_FLAG$
            if form_calc_flag$ = "N" then form_calc_mod$ = "N"
            if form_calc_flag$ = "Y" and form_calc_mod$ = " " then       ~
                  form_calc_mod$ = "N"
            if form_calc_flag$ = "Y" or form_calc_flag$ = "N" then return
                  errormsg$ = "Enter 'Y' or 'N'."
                  return

L50700
*        Test Data for Formula Calculation Mods   FORM_CALC_MOD$
            if form_calc_mod$ = "Y" or form_calc_mod$ = "N" then return
            if form_calc_mod$ = "D" then return
                  errormsg$ = "Enter 'Y', 'D', or 'N'."
                  return

L50760
*        Test Data for Material Usage %           MATERIAL_USAGE$
            call "NUMTEST" (material_usage$, -999,999,errormsg$,.8,mu)
            return
            mu = mu  /* get rid of annoying warning */

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
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
        exit_program:
            call "SHOSTAT" ("One Moment Please")

            end
