        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   CCC    OOO   RRRR   PPPP    AAA   RRRR    SSS   BBBB    *~
            *  C   C  O   O  R   R  P   P  A   A  R   R  S      B   B   *~
            *  C      O   O  RRRR   PPPP   AAAAA  RRRR    SSS   BBBB    *~
            *  C   C  O   O  R   R  P      A   A  R   R      S  B   B   *~
            *   CCC    OOO   R   R  P      A   A  R   R   SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CORPARSB - This subroutine allows the user to maintain the*~
            *            Core Deposit Tracking Customer appendix/shadow *~
            *            file, CORPARNT, which contains core parents    *~
            *            for the CUSTOMER file.                         *~
            *                                                           *~
            *            If the incoming Customer number is blank, the  *~
            *            user is permitted free access to the entire    *~
            *            CORPARNT file. If the code is non-blank, he/she*~
            *            is restricted to ONLY that customer.           *~
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
            * 04/30/92 ! Original                                 ! JIM *~
            * 07/28/92 ! Fields/Defaults Added                    ! KAB *~
            *          ! to test stand-alone REM sub @ 280  &     ! KAB *~
            *          ! two EXIT_PROGRAMS @ DATASAVE/DELETE      ! KAB *~
            * 09/23/92 ! Blank XUSTOMER$ = free use of CORPARNT.  ! JIM *~
            * 11/05/92 ! Drop-off to Ship-to or Parent            ! KAB *~
            * 08/04/93 ! Support for Pre_bill & Credit Memoes     ! KAB *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "CORPARSB" (xustomer$)       /* Customer Code from caller. */
                                         /* Do not modify XUSTOMER$!   */

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            cusparnt$9, parname$32,      /* Core Parent Customer Code  */~
            customer$9, cusname$32,      /* Customer Code              */~
            cmflag$1,                    /* Credit Memo Flag           */~
            date$8,                      /* Date for screen display    */~
            dmap(10),                    /* PLOWCODE Display Map       */~
            drop_off$3,                  /* Default Core Drop-Off Days */~
            dsorp$1,                     /* Drop-off to Ship or Prnt   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            extra_line$1,                /* Extra Line at Invoicing    */~
            hdr$(3)79,                   /* PLOWCODE Headers           */~
            i$(24)80,                    /* Screen Image               */~
            inex(4), inex$(4)16,         /* PLOWCODE Include/Exclude   */~
            inpmessage$79,               /* Informational Message      */~
            junk$16,                     /* Avoid the Noid             */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            mod_user$3, mod_date$10,     /* Last modified by ... on ...*/~
            pc$1, pcdescr$32,            /* Price Code For Core Prcing */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pf16msg$16,                  /* Prompt for PF(16)EDITMODE  */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99, pmsg$79,         /* Miscellaneous Read/Plow Key*/~
            postunap$1,                  /* Unapplied Cores Acct       */~
            readkey$99,                  /* GP Work Area               */~
            type$4, typedescr$32,        /* Cus Type For Core Pricing  */~
            unapacct$12, unapdescr$32,   /* Unapplied Cores Acct       */~
            userid$3                     /* Current User Id            */

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
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
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
            * #01 ! CORPARNT ! Core Deposit Tracking Core Parent file.  *~
            * #02 ! CUSTOMER ! Customer Master File                     *~
            * #03 ! GLMAIN   ! G/L Master File                          *~
            * #04 ! GENCODES ! G/P Codes File                           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "CORPARNT",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   10, keylen =   9,                     ~
                        alt key  1, keypos = 1, keylen =  18

            select #02, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  6, keypos = 1189, keylen =   3, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  1, keypos =   10, keylen =  30, dup

            select #03, "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9                      ~

            select #04, "GENCODES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos =    1,  keylen = 24

            if been_here_before% <> 0% then goto L09160
*        One-time-only initializations begin here.
                if xustomer$ = " " then fullfile% = 1%  /* full range? */
                been_here_before% = 1%
                call "OPENCHCK" (#01, fs%(1%), f2%(1%), 100%, rslt$(1%))
                call "OPENCHCK" (#02, fs%(2%), f2%(2%),   0%, rslt$(2%))
                call "OPENCHCK" (#03, fs%(3%), f2%(3%),   0%, rslt$(3%))
                call "OPENCHCK" (#04, fs%(4%), f2%(4%),   0%, rslt$(4%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

*        One-time-only initializations continue here.
            if fullfile% = 0%                                            ~
                then pf16msg$ = "(16)Save & Exit"                        ~
                else pf16msg$ = "(16)Save Data"
            call "EXTRACT" addr("ID", userid$)
            date$ = date : call "DATEFMT" (date$)
            edtmessage$ = "To modify displayed values, position cursor "&~
                "to desired value & press (RETURN)."
            str(line2$,62%) = "CORPARSB: " & str(cms2v$,,8%)

L09160
*        Any necessary EVERY-time initializations can be placed here.

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 10%
                if fullfile% = 0% and fieldnr% = 1% then goto L10270
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                     if enabled% = 0% then goto L10270
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <>  4% then goto L10210
L10160:                   fieldnr% = max(1%, fieldnr% - 1%)
                          gosub'051(fieldnr%)
                          if enabled% = 1% then goto L10130
                          if fieldnr% = 1% then goto L10110
                          goto L10160
L10210:              if fullfile% = 1% and keyhit% = 8%                  ~
                          then gosub select_by_parent
                     if fullfile% = 1% and keyhit% = 9%                  ~
                          then gosub select_by_customer
                     if keyhit% = 16% then goto exit_program
                     if keyhit% <> 0% then goto L10130
L10270:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " " then goto L10130
            next fieldnr%
            goto editpg1

        init_plowcode   /* Initialize PLOWCODE extended args for calls */
            plowkey$ = xor plowkey$
            init (" ") hdr$(), inex$(), pmsg$
            mat inex = zer
            mat dmap = zer
            return

        select_by_parent
            gosub init_plowcode
            hdr$(1%) = "  Parent Code & Name                       Cust"&~
                "omer"
            dmap(1%) =   1.09  : dmap(2%) = 1
            dmap(3%) = -10.30  : dmap(4%) = 11
            dmap(5%) =  10.09  : dmap(6%) = 43
            pmsg$ = hex(06) & "Select a Customer/Parent Relationship to"&~
                " edit."
            plowkey$ = xor plowkey$
*        PLOW CORPARNT on Alt Key 1.
            call "PLOWCODE" (#01, plowkey$, pmsg$, 9000%, 1.5, f1%(1%),  ~
                hdr$(), 0, 0, inex(), inex$(), "d", " ", #02, dmap())
            if f1%(1%) = 0% then return
                get #01, using L10540, customer$            /* CORPARNT */
L10540:              FMT POS(10), CH(9)
                gosub dataload
                return clear all
                goto editpg1

        select_by_customer
            gosub init_plowcode
            hdr$(1%) = "  Customer Code & Name                     Pare"&~
                "nt"
            dmap(1%) =  10.09  : dmap(2%) = 1
            dmap(3%) = -10.30  : dmap(4%) = 11
            dmap(5%) =   1.09  : dmap(6%) = 43
            pmsg$ = hex(06) & "Select a Customer/Parent Relationship to"&~
                " edit."
            plowkey$ = xor plowkey$
*        PLOW CORPARNT on Primary Key.
            call "PLOWCODE" (#01, plowkey$, pmsg$, 9000%, .5, f1%(1%),   ~
                hdr$(), 0, 0, inex(), inex$(), "d", " ", #02, dmap())
            if f1%(1%) = 0% then return
                get #01, using L10740, customer$            /* CORPARNT */
L10740:              FMT POS(10), CH(9)
                gosub dataload
                return clear all
                goto editpg1

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                if keyhit%  =  1% then gosub startover
                if keyhit%  = 12% then goto delete_core_parent
                if keyhit%  = 16% then goto datasave
                if keyhit%  = 32% then goto exit_program
                if keyhit% <>  0% then goto editpg1
L11140:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 2% or fieldnr% > 10% then goto editpg1
            if fieldnr% = lastfieldnr% then goto editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled% =  0% then goto editpg1
L11190:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then goto L11190
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " " then goto L11190
                lastfieldnr% = fieldnr%
            goto L11140

        delete_core_parent
            u3% = 2%                               /* Window at bottom */
            call "ASKUSER" (u3%, "*** CONFIRM DELETION ***",             ~
                "Press PF(12) to DELETE this Core Parent record.",       ~
                "--OR--", "Press PF(1) to cancel deletion.")
            if u3% =   1% then goto editpg1
            if u3% <> 12% then goto delete_core_parent
            call "DELETE" (#01, customer$, 9%)
            if fullfile% = 0%                                            ~
                then goto exit_program                                   ~
                else goto inputmode

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "READ101" (#01, customer$, f1%(1%))
            call "DATUNFMT" (mod_date$)
            call "GLUNFMT"  (unapacct$)
            put #01 using L35040, cusparnt$, customer$, drop_off%,        ~
                unapacct$, postunap$, cmflag$, pc$, type$, dsorp$,       ~
                extra_line$,                                             ~
                " ", mod_user$, mod_date$
            if f1%(1%) = 0% then write #01 else rewrite #01
            if fullfile% = 0%                                            ~
                then goto exit_program                                   ~
                else goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20180,         /* Customer Code          */~
                              L20210,         /* Core Parent Customer   */~
                              L20240,         /* Default Core Drop Days */~
                              L20320,         /* Price Code             */~
                              L20430,         /* Customer Type          */~
                              L20540,         /* Unappl. Core Acct      */~
                              L20640,         /* Post Unappl. Core      */~
                              L20710,         /* Drop to Ship or Prnt   */~
                              L20780,         /* Extra Line             */~
                              L20850          /* Credit Memo Flag       */
            return

L20180: REM Def/Enable Customer Code               CUSTOMER$
            return

L20210: REM Def/Enable Core Parent Customer Code   CUSPARNT$
            return

L20240: REM Def/Enable Default Core Drop-Off Days  DROP_OFF$
            if drop_off$ <> " " then return
            if dflt% = 0% then return
               get #01 using L20280, drop_off%
L20280:            FMT POS(19), BI(2)
            convert drop_off% to drop_off$, pic(##0)
            return

L20320: REM Def/Enable Price Code                  PC$
            if pc$ <> " " then return
            if dflt% = 0% then return
               get #01 using L20360, pc$
L20360:            FMT POS(32), CH(1)
            pcdescr$ = " "
            if pc$ = " " then return
            readkey$ = "PRICECODE" & pc$
            call "GETCODE" (#04, readkey$, pcdescr$, 1%, 99, f1%(4%))
            return

L20430: REM Def/Enable Customer Type               TYPE$
            if type$ <> " " then return
            if dflt% = 0% then return
               get #01 using L20470, type$
L20470:            FMT POS(33), CH(2)
            type$ = " "
            if type$ = " " then return
            readkey$ = "CUS TYPES" & type$
            call "GETCODE" (#04, readkey$, typedescr$, 1%, 99, f1%(4%))
            return

L20540: REM Def/Enable Unapplied Cores Account     UNAPACCT$
            if unapacct$ <> " " then return
            if dflt% = 0% then return
               get #01 using L20580, unapacct$
L20580:            FMT POS(21), CH(9)
            if unapacct$ = " " then return
            call "GLFMT" (unapacct$)
            call "GETCODE" (#03, unapacct$, unapdescr$, 1%, 99, f1%(3))
            return

L20640: REM Def/Enable Post Unapplied Cores        POSTUNAP$
            if postunap$ <> " " then return
            if dflt% = 0% then return
               get #01 using L20680, postunap$
L20680:            FMT POS(30), CH(1)
            return

L20710: REM Def/Enable Drop to Ship or Parent      DSORP$
            return

L20780: REM Def/Enable Extra Line                  EXTRA_LINE$
            if extra_line$ <> " " then return
            if dflt% = 0% then return
               get #01 using L20820, extra_line$
L20820:            FMT POS(36), CH(1)
            return

L20850: REM Def/Enable Credit Memo Flag            CMFLAG$
            if cmflag$ <> " " then return
            if dflt% = 0% then return
               get #01 using L20890, cmflag$
L20890:            FMT POS(31), CH(1)
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
         "Enter Ship-To Customer Code, partial or '?' to see list.     ",~
         "Enter Core Parent Customer Code, partial or '?' to see list. Bl~
        ~ank = Customer.",                                                ~
         "Enter Default Core Drop-Off Days (1 - 999) [0 for Default].  ",~
         "Enter Default Customer Specific Price Code for Core Pricing. ",~
         "Enter Default Customer Specific Type Code for Core Pricing.  ",~
         "Enter Pre-billed or other COGS holding Account (overrides syste~
        ~m).",                                                            ~
         "'Y' implies Pre-Billed or other; 'N' implies potential A/R (Dro~
        ~p-off billing).",                                                ~
         "Enter 'Drop-off to Ship-to or Parent' (' ', S or P).         ",~
         "Allow Creation Of Core Line at Invoicing. (' ', Y or N).     ",~
         "Enter Create Credit Memo Option - A = Auto., R = Review, N = No~
        ~ Issue or ' '."

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$, cusparnt$, parname$,       ~
                customer$, cusname$, drop_off$, mod_user$, mod_date$,    ~
                postunap$, cmflag$, unapacct$, unapdescr$, pc$, type$,   ~
                pcdescr$, typedescr$, dsorp$, extra_line$
            drop_off% = 0%
            if fullfile% = 0% then customer$ = xustomer$
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
            call "READ100" (#01, customer$, f1%(1%))
            if f1%(1%) = 0% then return
            get #01 using L35040, cusparnt$, customer$, drop_off%,        ~
                unapacct$, postunap$, cmflag$, pc$, type$, dsorp$,       ~
                extra_line$,                                             ~
                junk$, mod_user$, mod_date$

            call "GETCODE" (#02, customer$, cusname$, 1%, 99, f1%(2%))
            if f1%(2%) = 0% then cusname$ = "(Unknown Customer Code)"
            call "GETCODE" (#02, cusparnt$, parname$, 1%, 99, f1%(2%))
            if f1%(2%) = 0% then parname$ = "(Unknown Parent Code)"
            convert drop_off% to drop_off$, pic (##0)
            call "GLFMT" (unapacct$)
            call "GETCODE" (#03, unapacct$, unapdescr$, 1%, 99, f1%(3))
            call "DATEFMT" (mod_date$)
            readkey$ = "PRICECODE" & pc$
            call "GETCODE" (#04, readkey$, pcdescr$, 1%, 99, f1%(4%))
            readkey$ = "CUS TYPES" & type$
            call "GETCODE" (#04, readkey$, typedescr$, 1%, 99, f1%(4%))

        dfltload
            dflt% = 0%
            if customer$ = cusparnt$ then return

            readkey$ = str(cusparnt$,,9%) & cusparnt$
            call "REDALT0" (#01, readkey$, 1%, dflt%)
            return

        REM *************************************************************~
            *               R E C O R D   L A Y O U T S                 *~
            *************************************************************

L35040:     FMT   /* File #01- CORPARNT (Core Parent Customer Appendix)*/~
                CH(09),  /*   1/9        /* Core Parent Customer Code  */~
                CH(09),  /*  10/9        /* CUSTOMER Master file Code  */~
                BI(02),  /*  19/2        /* Default Drop-Off Days      */~
                CH(09),  /*  21/9        /* Customer Spec. Unapp. Core */~
                CH(01),  /*  30/1        /* Post Unapp Core Acct       */~
                CH(01),  /*  31/1        /* Credit Memo Flag           */~
                CH(01),  /*  32/1        /* Price Code                 */~
                CH(02),  /*  33/2        /* Customer Type              */~
                CH(01),  /*  35/1        /* Drop-off to Ship or Prnt   */~
                CH(01),  /*  36/1        /* Extra Line                 */~
                CH(55),  /*  37/55       /* Filler                     */~
                CH(03),  /*  92/3        /* Last Modified User         */~
                CH(06)   /*  95/6        /* Last Modified Date         */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            gosub'050(1%, fieldnr%)
            gosub set_pf1
            if fieldnr% = 0% then goto L40120
                init(hex(8c)) lfac$()
                goto L40140
L40120:     init(hex(86)) lfac$()
            lfac$(1%) = hex(8c)
L40140:     on fieldnr% gosub L40250,         /* Customer Code          */~
                              L40250,         /* Core Parent Customer   */~
                              L40260,         /* Default Core Drop Days */~
                              L40250,         /* Price Code             */~
                              L40250,         /* Customer Type          */~
                              L40250,         /* Unappl. Core Acct      */~
                              L40250,         /* Post Unappld Core Acct */~
                              L40250,         /* Drop to Ship or Prnt   */~
                              L40250,         /* Extra Line             */~
                              L40250          /* Credit Memo Flag       */
            goto L40280

            lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40250:     lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40260:     lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40280:     accept                                                       ~
                at (01,02), "Core Customer/Parent Input/Edit/Maintenance"~
        ,                                                                ~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (04,02), fac(hex(94)), errormsg$             , ch(79),~
                                                                         ~
                at (06,02), "Customer Code",                             ~
                at (06,30), fac(lfac$( 1%)), customer$          , ch(09),~
                at (06,46), fac(hex(8c)),   cusname$            , ch(32),~
                                                                         ~
                at (07,02), "Core Parent Customer Code",                 ~
                at (07,30), fac(lfac$( 2%)), cusparnt$          , ch(09),~
                at (07,46), fac(hex(8c)),   parname$            , ch(32),~
                                                                         ~
                at (08,02), "Default Core Drop-Off Days",                ~
                at (08,30), fac(lfac$( 3%)), drop_off$          , ch(03),~
                                                                         ~
                at (09,02), "Price Code for Core Items ",                ~
                at (09,30), fac(lfac$( 4%)), pc$                , ch(01),~
                at (09,46), fac(hex(8c)),   pcdescr$            , ch(32),~
                                                                         ~
                at (10,02), "Customer Type (Pricing)   ",                ~
                at (10,30), fac(lfac$( 5%)), type$              , ch(02),~
                at (10,46), fac(hex(8c)), typedescr$            , ch(32),~
                                                                         ~
                at (11,02), "Pre-bill COGS Hold Acct",                   ~
                at (11,30), fac(lfac$( 6%)), unapacct$          , ch(12),~
                at (11,46), fac(hex(8c)   ), unapdescr$         , ch(32),~
                                                                         ~
                at (12,02), "Core Billed w/ Re-man Invc?",               ~
                at (12,30), fac(lfac$( 7%)), postunap$          , ch(01),~
                                                                         ~
                at (13,02), "Drop-off to Par. or Shp-to?",               ~
                at (13,30), fac(lfac$( 8%)), dsorp$             , ch(01),~
                                                                         ~
                at (14,02), "Allow Extra Line on Invoice",               ~
                at (14,30), fac(lfac$( 9%)), extra_line$        , ch(01),~
                                                                         ~
                at (15,02), "Credit Memo Flag",                          ~
                at (15,30), fac(lfac$(10%)), cmflag$            , ch(01),~
                                                                         ~
                at (19,02), "Last modified by",                          ~
                at (19,19), fac(hex(8c)),   mod_user$           , ch(03),~
                at (19,23), "on",                                        ~
                at (19,26), fac(hex(8c)),   mod_date$           , ch(10),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then L40800
                call "MANUAL" ("CORPARSB") : goto L40280

L40800:     if keyhit% <> 15% then L40830
                call "PRNTSCRN" : goto L40280

L40830:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf1
        if edit% = 2% then L41030     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                 (8)Select from Parents " &       ~
                      "on file                (15)Print Screen"
            pf$(3%) = "                 (9)Select from Customer" &       ~
                      "s on file              (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffff0809ffffff0dff0f1000)
            if fullfile% = 1% and fieldnr% = 1% then L40990
                str(pf$(2%),18%,30%) = " " : str(pfkeys$,8%,1%) = hex(ff)
                str(pf$(3%),18%,32%) = " " : str(pfkeys$,9%,1%) = hex(ff)
L40990:     if fieldnr% > 2% then L41010
                str(pf$(1%),18%,17%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L41010:     return

L41030: if fieldnr% > 0% then L41140  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                       (12)" &       ~
                      "DELETE Record          (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                    (32)" &       ~
                      "Exit without Saving    " & pf16msg$
            pfkeys$ = hex(01ffffffffffffffffffff0c0dff0f100020ff)
            if fullfile% = 0% then L41130
                str(pf$(3%),37%,23%) = " " : str(pfkeys$,18%,1%) = hex(ff)
L41130:     return
L41140:                              /*  Edit Mode - Enabled    */
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
            mod_user$ = userid$
            mod_date$ = date
            call "DATEFMT" (mod_date$)
            on fieldnr% gosub L50210,         /* Customer Code          */~
                              L50360,         /* Core Parent Customer   */~
                              L50490,         /* Default Core Drop Days */~
                              L50560,         /* Price Code             */~
                              L50670,         /* Customer Type          */~
                              L50820,         /* Unapplied Cores Account*/~
                              L50900,         /* Test for Post Unapplied*/~
                              L50970,         /* Drop to Ship or Prnt   */~
                              L51040,         /* Extra Line             */~
                              L51110          /* Credit Memo Flag       */
            return

L50210: REM Test for Customer Code                CUSTOMER$
            cusname$ = " "
*          IF CUSTOMER$ <> " " THEN GOTO 50210
*              ERRORMSG$ = "Customer Code may NOT be blank."
*              RETURN
            if customer$ = "?" then customer$ = " "
            call "GETCODE" (#02, customer$, cusname$, 1%, .3, f1%(2%))
            if f1%(2%) = 0% then goto L50330
                gosub dataload
                if f1%(1%) = 0% then return
                     return clear all
                     goto editpg1
L50330:     errormsg$ = "You must enter a Customer Code."
            return

L50360: REM Test for Core Parent Customer Code    CUSPARNT$
            parname$ = " "
            if cusparnt$ <> " " then goto L50430
                cusparnt$ = customer$
                parname$ = cusname$
L50410:         gosub dfltload
                return
L50430:     if cusparnt$ = "?" then cusparnt$ = " "
            call "GETCODE" (#02, cusparnt$, parname$, 1%, .3, f1%(2%))
            if f1%(2%) <> 0% then L50410
                errormsg$ = "You must enter a Core Customer Parent Code."
                return

L50490: REM Test for Default Core Drop-Off Days   DROP_OFF$
            call "NUMTEST" (drop_off$, 0, 999, errormsg$, 0, temp)
            if errormsg$ <> " " then return
                drop_off% = int(temp)
                convert drop_off% to drop_off$, pic (##0)
                return

L50560
*        Standard Price Code                   PC$
            pcdescr$ = " "
            if pc$   = " " then return
            if (pc$ >= "A" and pc$ <= "Q") or                            ~
               (pc$ >= "0" and pc$ <= "8") then L50630
                errormsg$ = "Price Code must be 'A'-'Q' or '0'-'8'."
                return
L50630:     readkey$ = "PRICECODE" & pc$
            call "GETCODE" (#04, readkey$, pcdescr$, 1%, 99, f1%(4%))
            return

L50670
*        Customer Type Code                    TYPE$
            typedescr$ = " "
            if type$   = " " then return
                readkey$ = "CUS TYPES" & type$
                call "PLOWCODE" (#04, readkey$, typedescr$, 9%, .30,     ~
                                                                 f1%(4%))
                if f1%(4%) = 1% then L50760
                     errormsg$ = "Customer Type Code not on file."
                     return
L50760:         type$ = str(readkey$,10)
                if typedescr$ = " " then return
                typedescr$ = "(" & typedescr$
                typedescr$ = typedescr$ & ")"
                return

L50820: REM Test for Unapplied Cores Account      UNAPACCT$
            unapdescr$ = " "
            if unapacct$ = " " then return
            call "GETCODE" (#03, unapacct$, unapdescr$, 0%, 0, f1%(3))
              if f1%(3) <> 0% then return
            errormsg$ = "Enter Valid Account or leave blank."
            return

L50900: REM Test for Post Unapplied Cores         POSTUNAP$
            if postunap$ = "Y" then return
            if postunap$ = "N" then return
            if postunap$ = " " then return
               errormsg$ = "Valid Responses are 'Y', 'N' or ' ' (blank)"
               return

L50970: REM Test for Drop to Ship or Parent       DSORP$
            if dsorp$ = "S" then return
            if dsorp$ = "P" then return
            if dsorp$ = " " then return
               errormsg$ = "Valid Responses are 'S', 'P' or ' ' (blank)"
               return

L51040: REM Test for Extra Line on Invoice        EXTRA_LINE$
            if extra_line$ = "Y" then return
            if extra_line$ = "N" then return
            if extra_line$ = " " then return
               errormsg$ = "Valid Responses are 'Y', 'N' or ' ' (blank)"
               return

L51110: REM Test for Credit Memo Flag             CMFLAG$
            if cmflag$ = " " then return
            if cmflag$ = "N" then return
            if cmflag$ = "A" then return
            if cmflag$ = "R" then return
               errormsg$ = "You must enter 'A'uto., 'R'eview, 'N'o"  &   ~
                           " Issue, or blank for this code."
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
