        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  PPPP   RRRR   L      W   W   222   M   M  N   N  TTTTT   *~
            *  P   P  R   R  L      W   W      2  MM MM  NN  N    T     *~
            *  PPPP   RRRR   L      W   W     2   M M M  N N N    T     *~
            *  P      R   R  L      W W W    2    M   M  N  NN    T     *~
            *  P      R   R  LLLLL   W W   2222   M   M  N   N    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLW2MNT - Maintain data in the W-2 Extract file before   *~
            *             generating the W-2 forms.                     *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/21/89 ! Original                                 ! MJB *~
            * 01/03/90 ! Added all 8 boxes in box 5 and deferred  ! MJB *~
            *          !  Compensation.                           !     *~
            * 01/12/90 ! Added Employer State ID and changed      ! MJB *~
            *          !  PRLWYYYY rec length to 330.             !     *~
            * 12/10/90 ! 1990 W2 Requirements                     ! KAB *~
            * 12/06/91 ! PRR 12142 - Added Medicare tax and wage  ! JBK *~
            * 12/03/91 ! for 1991 W2 Requirements.                ! JBK *~
            *          ! PRR 12159 - For supplemental W2 screen   !     *~
            *          ! all fields are open for edit (per KAB)   !     *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 11/29/93 ! Changed Screen Layouts, Box Numbers,     ! JBK *~
            *          !  Variable Names, Test Section Code to    !     *~
            *          !  conform to '93 W-2 Changes.             !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            aeic$10,                     /* Advance EIC                */~
            box$(8)1,                    /* Boxes in Box 15 & Void     */~
            box13$(4)1,                  /* Box 13 Identifiers         */~
            box13a$(4)10,                /* Box 13 Amounts             */~
            box14$(2)12,                 /* Box 14 Descriptions        */~
            box14a$(2)10,                /* Box 14 Amounts             */~
            box13(4),                    /* Box 13 Amounts             */~
            box14(2),                    /* Box 14 Amounts             */~
            date$8,                      /* Date for screen display    */~
            dcass$10,                    /* Dependent Care Assistance  */~
            delkey$50,                   /* Delete Key                 */~
            edtmessage$79,               /* Edit screen message        */~
            empaddr$(3)30,               /* Employee Address           */~
            empcity$20,                  /* Employee Address           */~
            empstate$2,                  /* Employee Address           */~
            empcode$12,                  /* Employee Number            */~
            empname$30,                  /* Employee Name              */~
            empzip$9,                    /*          Zip Code          */~
            error$79,                    /* Error message short        */~
            errormsg$79,                 /* Error message              */~
            fedtax$10,                   /* Federal Tax                */~
            fedwages$10,                 /* Federal Taxable Income     */~
            ficatax$10,                  /* FICA Tax                   */~
            ficatips$10,                 /* FICA Tips                  */~
            ficawages$10,                /* FICA Income                */~
            fringe$10,                   /* Fringe Benefits            */~
            hdr$(3)30,                   /* PLOWCODE Argument          */~
            inpmessage$79,               /* Informational Message      */~
            medicaretax$10,              /* FICA Medicare Tax          */~
            medicarewages$10,            /* FICA Medicare Income       */~
            lc$8, lc2$,                  /* Local ID Code              */~
            lfac$(50)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            ltax$10, ltax2$10,           /* Local Tax                  */~
            lwages$10, lwages2$10,       /* Local Taxable Income       */~
            msg_text$(2)70,              /* Text for GETPARM           */~
            nmn$80,                      /* PLOWCODE Prompt            */~
            nq457$10,                    /* Non Qualified sec 457      */~
            nqn457$10,                   /* Non Qualified non 457      */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            sid$10, sid2$10,             /* Employers State ID #       */~
            ssn$11,                      /* Social Security Number     */~
            st$2, st2$2,                 /* State ID                   */~
            stax$10, stax2$10,           /* State Tax                  */~
            swages$10, swages2$10,       /* State Taxable Income       */~
            tipsalloc$10,                /* Tips allocated             */~
            w2year$4                     /* Year for file definition   */


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
            cms2v$ = "R6.02.05 12/06/93 1993 Year End Payroll Patch     "
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
            * #01 ! PRLWYYYY ! Payroll W2 Extract File for Year YYYY    *~
            * #02 ! PERMASTR ! Personell Master File                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "PRLWYYYY",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos = 1,    keylen = 13,                      ~
                        alt key  1, keypos = 40, keylen = 12,            ~
                            key  2, keypos = 14, keylen =  38

            select #2, "PERMASTR",                                       ~
                        varc,     indexed,  recsize = 950,               ~
                        keypos =  39,  keylen = 12,                      ~
                        alt key  1, keypos =   28, keylen =  23,         ~
                            key  2, keypos =    2, keylen =  49,         ~
                            key  3, keypos =    1, keylen =  50


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

        set_getparm
            pfmask$ = hex(00010000)  :  pfkey$ = "@"
            msg_text$(1%) = " "
            msg_text$(2%) = hex(84) & "Enter the Reporting" &            ~
                            " Year for the W2 Extract File"
            msg_text$(3%) = " "
            msg_text$(4%) = hex(84) & "Press RETURN to Continue "  &     ~
                                     "or PF-16 to EXIT"
            str(msg_text$(1%),69%) = hex(0d)
            str(msg_text$(2%),69%) = hex(0d)
            str(msg_text$(3%),69%) = hex(0d)
            str(msg_text$(4%),69%) = hex(0d)

            w2year$ = "    "
L02090:     call "GETPARM" addr("I ", "R", "  Year  ", pfkey$,           ~
                           "W202", "W2MNT ", msg_text$(), 350%,          ~
                            "K", "W2YEAR  ", w2year$, 4%,                ~
                            "A", 14%, "A", 45%, "A",                     ~
                            "T", "W2 Reporting Year ", 17%,              ~
                            "A", 14%, "A", 20%, "P", pfmask$, "E")

            if pfkey$ = "P" then L65000
            if pfkey$ <> "@" then L02090

            prname$ = "PRLW" & str(w2year$)
            call "PUTPRNAM" addr(#1, prname$)

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 0%, rslt$(02%))
            if f2%(1%) = 0% then L09000

L02270:     ask% = 2%
            call "ASKUSER" (ask%, "*** NO FILE ***",                     ~
                 "The W-2 File for " & w2year$ & " Cannot be Found",     ~
                 "    Press PF-1 to Re-Select the Reporting Year",       ~
                 " or Press PF-16 to EXIT This Program")
            if ask% = 1% then set_getparm
            if ask% = 16% then L65000
            goto L02270

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62%) = "PRLW2MNT: " & str(cms2v$,,8%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
        inputmode0
            m% = 1%
L10180:     gosub'101(m%)              /* Accept Emp#  */
                if keyhit% =  1% then gosub startover
                if keyhit% =  2% then load_first
                if keyhit% =  5% then load_next
                if keyhit% = 21% then load_next
                if keyhit% = 16% then exit_program
                if keyhit% <> 0% then L10180
            gosub'151                 /* Check Employee Number */
                if errormsg$ <> " " then L10180
                goto dataload


L10320: inputmode1
            m% = 2%
          inputmode2
L10360:     gosub'101(m%)              /* Display / Accept  */
                if keyhit% =  1% then gosub startover
                if keyhit% <> 16% then L10320

            gosub'152               /* Edit Fields for Valid Entry */
                if errormsg$ = " " then editpg1
                m% = 4%
                goto L10360


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            m% = 3%
L11080:     gosub'101(m%)           /* Display Screen - No Entry  */
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  2% then       load_first
                if keyhit%  =  5% then       load_next
                if keyhit%  = 21% then       load_next
                if keyhit%  =  8% then       delete_empl
                if keyhit%  = 11% then       add_extra
                if keyhit%  = 16% then       datasave
                if keyhit% <>  6% then       L11080

*          IF SEQ% = 1% THEN M% = 2% ELSE M% = 5%
*          IF M% = 5% THEN SCREEN% = 1%
            m% = 2%
L11150:     gosub'101(m%)           /* Full Screen Edit            */
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  1% then gosub startover
                if keyhit% <> 16% then       L11150
            gosub'152                   /* Validate Full Screen    */
                if errormsg$ = " " then editpg1
                m% = 4%
                goto L11150


        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto inputmode

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, last$, first$, middle$,    ~
                      empaddr$(), empname$, empzip$, fedtax$, fedwages$, ~
                      ficatax$, ficawages$, ficatips$, fringe$, aeic$,   ~
                      tipsalloc$, dcass$, nq457$, nqn457$, box$(), sid$, ~
                      box13$(), box13a$(), box14$(), box14a$(), sid2$,   ~
                      lc$, ltax$, lwages$, ssn$, st$, stax$, swages$,    ~
                      lc2$, ltax2$, lwages2$, st2$, stax2$, swages2$,    ~
                      medicaretax$, medicarewages$
                      seq% = 0%
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
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
            plowkey$ = " "  :  errormsg$ = " " :  seq% = 1%
            put plowkey$ using L30065, empcode$, seq%
L30065:         FMT CH(12), BI(1)
            call "READ100" (#1, plowkey$, f1%(1%))
                if f1%(1%) = 0% then inputmode1
L30090:     get #1 using L30140, empcode$, seq%, last$, first$, middle$,  ~
                                ssn$, seq%, empname$, empaddr$(),        ~
                                empzip$, fedtax, fedwages, ficatax,      ~
                                ficawages, ficatips, fringe, swages,     ~
                                stax, st$, lwages, ltax, lc$, box$(),    ~
                                sid$, box13$(), box14$(), medicaretax,   ~
                                medicarewages, tipsalloc,                ~
                                aeic, dcass, nq457, nqn457, box14(),     ~
                                box13(), swages2, stax2, st2$, sid2$,    ~
                                lwages2, ltax2, lc2$

L30140:         FMT CH(12), BI(1), CH(15), CH(10), CH(1), CH(11), BI(1), ~
                    CH(30), 3*CH(30), CH(9), 8*PD(14,4), CH(2),          ~
                    2*PD(14,4), CH(8), XX(20), 8*CH(1), CH(10), 4*CH(1), ~
                    2*CH(12), 2*PD(14,4), XX(2),                         ~
                    5*PD(14,4), 2*PD(14,4), 4*PD(14,4),                  ~
                    2*PD(14,4), CH(2), CH(10), 2*PD(14,4), CH(8)

            call "CONVERT" (fedwages,      2.2, fedwages$)
            call "CONVERT" (fedtax,        2.2, fedtax$)
            call "CONVERT" (ficawages,     2.2, ficawages$)
            call "CONVERT" (ficatax,       2.2, ficatax$)
            call "CONVERT" (ficatips,      2.2, ficatips$)
            call "CONVERT" (fringe,        2.2, fringe$)
            call "CONVERT" (tipsalloc,     2.2, tipsalloc$)
            call "CONVERT" (aeic,          2.2, aeic$)
            call "CONVERT" (dcass,         2.2, dcass$)
            call "CONVERT" (nq457,         2.2, nq457$)
            call "CONVERT" (nqn457,        2.2, nqn457$)
            call "CONVERT" (medicaretax,   2.2, medicaretax$)
            call "CONVERT" (medicarewages, 2.2, medicarewages$)

            for i% = 1% to 4%
*            IF BOX13(I%) <= 0% THEN 30330
                 call "CONVERT" (box13(i%), 2.2, box13a$(i%))
            next i%

            for i% = 1% to 2%
*            IF BOX14(I%) <= 0% THEN 30380
                 call "CONVERT" (box14(i%), 2.2, box14a$(i%))
            next i%

            call "CONVERT" (swages, 2.2, swages$)
            call "CONVERT" (stax, 2.2, stax$)
            call "CONVERT" (lwages, 2.2, lwages$)
            call "CONVERT" (ltax, 2.2, ltax$)
            call "CONVERT" (swages2, 2.2, swages2$)
            call "CONVERT" (stax2, 2.2, stax2$)
            call "CONVERT" (lwages2, 2.2, lwages2$)
            call "CONVERT" (ltax2, 2.2, ltax2$)
*          IF SEQ% > 1% THEN SCREEN% = 1%
            goto editpg1

        load_next
            plowkey$ = " "  :  errormsg$ = " "
            if keyhit% = 5% then seq% = -1%
            if keyhit% = 5% then screen% = 0%
            put plowkey$ using L30725, empcode$, seq%
            call "READ102" (#1, plowkey$, f1%(1))
L30540:        if f1%(1) = 1% then L30090
            errormsg$ = "The Last Employee is Currently Displayed"
            init(" ") fedtax$, fedwages$, ficatax$, ficawages$,          ~
                      ficatips$, fringe$, aeic$, tipsalloc$, dcass$,     ~
                      nq457$, nqn457$, box$(), box13$(), box13a$(),      ~
                      box14$(), box14a$(), lc$, ltax$, lwages$, st$,     ~
                      stax$, swages$, sid$, lc2$, ltax2$, lwages2$,      ~
                      st2$, stax2$, swages2$, sid2$, medicaretax$,       ~
                      medicarewages$
                      if seq% = 1% then screen% = 0%
             goto inputmode0


        load_first
            errormsg$ = " ": screen% = 0%
            init(hex(00)) plowkey$
            call "READ104" (#1, plowkey$, f1%(1%))
*          GOTO 30090
            goto L30540

        add_extra
            gosub dataput
            errormsg$ = " "
            seq% = seq% + 1%
            plowkey$ = " "
            put plowkey$ using L30725, empcode$, seq%
L30725:         FMT CH(12), BI(1)
            call "READ100" (#1, plowkey$, f1%(1%))
            if f1%(1%) = 0% then L30800
                errormsg$ = "Additional W-2 Record Already Exists" &     ~
                            " as Shown, Press PF-11 to ADD Another"
                nextwarning% = 1% : /* SCREEN% = 1% */
                goto L30090

L30800:     init(" ") fedtax$, fedwages$, ficatax$, ficawages$,          ~
                      ficatips$, fringe$, aeic$, tipsalloc$, dcass$,     ~
                      nq457$, nqn457$, box$(), box13$(), box13a$(),      ~
                      box14$(), box14a$(), lc$, ltax$, lwages$, st$,     ~
                      stax$, swages$, sid$, lc2$, ltax2$, lwages2$,      ~
                      st2$, stax2$, swages2$, sid2$, medicaretax$,       ~
                      medicarewages$

*          M% = 5% : SCREEN% = 1%
*          GOTO INPUTMODE2
            m% = 2%
            goto inputmode2

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            plowkey$ = " "
            put plowkey$ using L31053, empcode$, seq%
L31053:         FMT CH(12), BI(1)
            call "READ101" (#1, plowkey$, f1%(1%))

            put #1 using L31160, empcode$, seq%, last$, first$, middle$,  ~
                                ssn$, seq%, empname$, empaddr$(),        ~
                                empzip$, fedtax, fedwages, ficatax,      ~
                                ficawages, ficatips, fringe, swages,     ~
                                stax, st$, lwages, ltax, lc$, " ",       ~
                                0, box$(), sid$, box13$(), box14$(),     ~
                                medicaretax, medicarewages, " ",         ~
                                tipsalloc, aeic, dcass, nq457, nqn457,   ~
                                box14(), box13(), swages2, stax2, st2$,  ~
                                sid2$, lwages2, ltax2, lc2$, " "

            if f1%(1%) = 1% then rewrite #1 else write #1
            return

L31160:         FMT CH(12),              /* Employee Code              */~
                    BI(1),               /* Sequence No.               */~
                    CH(15),              /* Last Name                  */~
                    CH(10),              /* First Name                 */~
                    CH(1),               /* Middle Initial             */~
                    CH(11),              /* SSAN                       */~
                    BI(1),               /* Sequence No.               */~
                    CH(30),              /* Employee Name              */~
                    3*CH(30),            /* Employee Address           */~
                    CH(9),               /* Employee Zip Code          */~
                    6*PD(14,4),          /* Amounts - FIT, Wages,      */~
                                         /*         - FICA, FICA Wages,*/~
                                         /*         - Tips, Fringes    */~
                    2*PD(14,4),          /* State Amounts - Wages, Tax */~
                    CH(2),               /* State Abbreviation         */~
                    2*PD(14,4),          /* Local Amounts - Wages, Tax */~
                    CH(8),               /* Locality                   */~
                    CH(12),              /* Old Box 16 Label  (filler) */~
                    PD(14,4),            /* Old Deferred Comp (filler) */~
                    8*CH(1),             /* W-2 Check Off Boxes        */~
                    CH(10),              /* State Id Number            */~
                    4*CH(1),             /* Box 13 Identifiers         */~
                    2*CH(12),            /* Box 14 Labels              */~
                    PD(14,4),            /* Medicare Tax               */~
                    PD(14,4),            /* Medicare Wages             */~
                    CH(2),               /* Filler                     */~
                    PD(14,4),            /* Allocated Tips             */~
                    PD(14,4),            /* A.E.I.C.                   */~
                    PD(14,4),            /* Dep Care Assistance        */~
                    PD(14,4),            /* Non Qualified 457          */~
                    PD(14,4),            /* Non Qualified non 457      */~
                    2*PD(14,4),          /* Box 14 Amounts             */~
                    4*PD(14,4),          /* Box 13 Amounts             */~
                    2*PD(14,4),          /* State Amounts - Wages, Tax */~
                    CH(2),               /* State Abbreviation         */~
                    CH(10),              /* State Id Number            */~
                    2*PD(14,4),          /* Local Amounts - Wages, Tax */~
                    CH(8),               /* Locality                   */~
                    CH(6)                /* Filler                     */


        delete_empl
            ask% = 2%
            call "ASKUSER" (ask%, "***** CONFIRM DELETE *****",          ~
                 "PRESS PF-24 to DELETE Employee # " & empcode$  &       ~
                 " from the " & w2year$ & " W-2 File",                   ~
                 "PRESS PF-28 to DELETE only ADDITIONAL Records.",       ~
                 "Press PF-1 to RETURN to EDIT MODE")
            if ask%  =  1% then editpg1
            if ask%  = 28% then L32080
            if ask% <> 24% then delete_empl

L32080:     init (hex(00)) delkey$
              if ask% = 24% then seq% = 1% else seq% = max(seq%, 2%)
            put str(delkey$,,13%) using L32083, empcode$, seq% - 1%
L32083:       FMT CH(12), BI(1)
*          CALL "DELETE" (#1, DELKEY$, 12%)
*          GOTO INPUTMODE
L32110:     call "PLOWNXT1" (#1, delkey$, 12%, f1%(1%))
              if f1%(1%) = 0% then inputmode
            delete #1
            goto L32110

        pf1315
            if keyhit% <> 13% then L34650
                call "MANUAL" ("PRLW2MNT")
                keyhit% = 15%
                return
L34650:     if keyhit% <> 15% then return
                call "PRNTSCRN"
                return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(mode%)
            if nextwarning% = 0% then L40100
            if mode% = 3% then L40100
               errormsg$ = " " : nextwarning% = 0%
L40100:     gosub set_pf1
            if seq% = 0% then str(line2$,,60%) = " "
            if seq% = 1% then str(line2$,,60%) =                         ~
                      "Standard Employee W-2 Form for Federal Return"
            if seq% > 1% then str(line2$,,60%) =                         ~
                      "Additional Employee W-2 Form for State or Local"
            on mode% gosub   L40240,     /* Input Employee Number Only */ ~
                             L40300,     /* Full Screen Input          */ ~
                             L40400,     /* Edit - Display only        */ ~
                             L40460,     /* Error Screen               */ ~
                             L40500      /* Add Additional W-2         */
            goto L41000

*       ** Input for Code Filed Only
L40240:     inpmessage$ = "Enter Employee Number"
            init(hex(8c)) lfac$()
            lfac$(1%) = hex(81)
            return

*       ** Full Screen Input/Edit
L40300:     inpmessage$ = "Enter or Modify ALL Data as Required and " &  ~
                          "Press PF-16 to Validate"
            init(hex(82)) lfac$()        /* Numeric    */
            lfac$(1%) = hex(84)          /* Exceptions */
            init (hex(81)) lfac$(4%), lfac$(6%), lfac$(19%), lfac$(23%), ~
                           lfac$(24%), lfac$(27%), lfac$(28%),           ~
                           lfac$(21%), lfac$(31%), lfac$(34%)
            init (hex(80)) lfac$(2%), lfac$(5%)

            return

*       ** Edit Mode Display Only
L40400:     inpmessage$ = "Press PF-6 for Full Screen Edit - or - "   &  ~
                          "PF-16 to Save Data"
            init(hex(8c)) lfac$()
            return

*       ** Error Correction Screen
L40460:     inpmessage$ = "Correct All Bright Fields and Press PF-16" &  ~
                          " to Validate Entries"
            return

L40500
*       ** Add Additional W-2 Screen
            inpmessage$ = "Enter State and/or Local Data for "        &  ~
                          "Additional W-2 for Employee " & empcode$
            init(hex(8c)) lfac$()
            init(hex(82)) lfac$(20%), lfac$(22%), lfac$(25%), lfac$(26%),~
                          lfac$(29%), lfac$(30%), lfac$(32%), lfac$(33%),~
                          lfac$(35%), lfac$(36%)
            init(hex(81)) lfac$(19%), lfac$(23%), lfac$(24%), lfac$(27%),~
                          lfac$(28%), lfac$(21%), lfac$(31%), lfac$(34%)
*          INIT(HEX(80)) LFAC$(21%), LFAC$(31%), LFAC$(34%)
            return

L41000
*       ** Here Starts the ACCEPTs

            on screen% goto L43000

            accept                                                       ~
               at (01,02),                                               ~
                  "Employee W-2 Data Maintenance",                       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Employee Number",                            ~
               at (04,20), fac(lfac$( 1%)), empcode$            , ch(12),~
               at (04,37), "Name",                                       ~
               at (04,47), fac(lfac$( 2%)), empname$            , ch(30),~
                                                                         ~
               at (05,02), "Social Security #",                          ~
               at (05,20), fac(lfac$( 3%)), ssn$                , ch(11),~
                                                                         ~
               at (05,37), "Address",                                    ~
               at (05,47), fac(lfac$( 5%)), empaddr$(1%)        , ch(30),~
               at (06,02), "Sort Name - Last",                           ~
               at (06,20), fac(lfac$( 4%)), last$               , ch(15),~
               at (06,47), fac(lfac$( 5%)), empaddr$(2%)        , ch(30),~
               at (07,02), "            First",                          ~
               at (07,20), fac(lfac$( 4%)), first$              , ch(10),~
               at (07,47), fac(lfac$( 5%)), empaddr$(3%)        , ch(30),~
               at (08,02), "            M.I.",                           ~
               at (08,20), fac(lfac$( 4%)), middle$             , ch(01),~
               at (08,37), "Zip Code",                                   ~
               at (08,47), fac(lfac$( 5%)), empzip$             , ch(09),~
                                                                         ~
               at (10,02), "(15)Statutory Empl ",                        ~
               at (10,23), fac(lfac$( 6%)), box$(1%)            , ch(01),~
               at (10,31), "(15)Deceased    ",                           ~
               at (10,51), fac(lfac$( 6%)), box$(2%)            , ch(01),~
               at (10,57), "(15)Pension Plan",                           ~
               at (10,79), fac(lfac$( 6%)), box$(3%)            , ch(01),~
                                                                         ~
               at (11,02), "(15)Legal Rep  ",                            ~
               at (11,23), fac(lfac$( 6%)), box$(4%)            , ch(01),~
               at (11,31), "(15)942 Employee",                           ~
               at (11,51), fac(lfac$( 6%)), box$(5%)            , ch(01),~
               at (11,57), "(15)Deferred Comp",                          ~
               at (11,79), fac(lfac$( 6%)), box$(7%)            , ch(01),~
                                                                         ~
               at (13,02), "( 1)Federal Wages",                          ~
               at (13,32), fac(lfac$( 7%)), fedwages$           , ch(10),~
               at (13,48), "( 2)Federal Tax",                            ~
               at (13,69), fac(lfac$( 8%)), fedtax$             , ch(10),~
                                                                         ~
               at (14,02), "( 3)FICA Wages ",                            ~
               at (14,32), fac(lfac$( 9%)), ficawages$          , ch(10),~
               at (14,48), "( 4)FICA Paid  ",                            ~
               at (14,69), fac(lfac$(10%)), ficatax$            , ch(10),~
                                                                         ~
               at (15,02), "( 5)Medicare Wages",                         ~
               at (15,32), fac(lfac$(11%)), medicarewages$      , ch(10),~
               at (15,48), "( 6)Medicare Paid",                          ~
               at (15,69), fac(lfac$(12%)), medicaretax$        , ch(10),~
                                                                         ~
               at (16,02), "( 7)FICA Tips     ",                         ~
               at (16,32), fac(lfac$(13%)), ficatips$           , ch(10),~
               at (16,48), "( 8)Allocated Tips",                         ~
               at (16,69), fac(lfac$(14%)), tipsalloc$          , ch(10),~
                                                                         ~
               at (17,02), "( 9)Advance EIC",                            ~
               at (17,32), fac(lfac$(15%)), aeic$               , ch(10),~
               at (17,48), "(10)Dep Care Asst ",                         ~
               at (17,69), fac(lfac$(16%)), dcass$              , ch(10),~
                                                                         ~
               at (18,02), "(11)Nonqual'd Plan - sec 457",               ~
               at (18,32), fac(lfac$(17%)), nq457$              , ch(10),~
               at (18,48), "(12)Benefits-Box 10",                        ~
               at (18,69), fac(lfac$(18%)), fringe$             , ch(10),~
                                                                         ~
               at (19,02), "                   - sec 457",               ~
               at (19,32), fac(lfac$(17%)), nqn457$             , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               gosub pf1315
               if keyhit%  = 15% then L41000
               if keyhit% <>  9% then return
                  screen%  =  1%
                  goto L41000

L43000
*       ** Here Starts the ACCEPTs

            accept                                                       ~
               at (01,02),                                               ~
                  "Employee W-2 Data Maintenance",                       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Employee Number",                            ~
               at (04,20), fac(lfac$( 1%)), empcode$            , ch(12),~
               at (04,37), "Name",                                       ~
               at (04,47), fac(lfac$( 2%)), empname$            , ch(30),~
                                                                         ~
               at (05,02), "Social Security #",                          ~
               at (05,20), fac(lfac$( 3%)), ssn$                , ch(11),~
                                                                         ~
               at (05,37), "Address",                                    ~
               at (05,47), fac(lfac$( 5%)), empaddr$(1%)        , ch(30),~
               at (06,02), "Sort Name - Last",                           ~
               at (06,20), fac(lfac$( 4%)), last$               , ch(15),~
               at (06,47), fac(lfac$( 5%)), empaddr$(2%)        , ch(30),~
               at (07,02), "            First",                          ~
               at (07,20), fac(lfac$( 4%)), first$              , ch(10),~
               at (07,47), fac(lfac$( 5%)), empaddr$(3%)        , ch(30),~
               at (08,02), "            M.I.",                           ~
               at (08,20), fac(lfac$( 4%)), middle$             , ch(01),~
               at (08,37), "Zip Code",                                   ~
               at (08,47), fac(lfac$( 5%)), empzip$             , ch(09),~
                                                                         ~
               at (10,02), "(13)Code/Amount a)",                         ~
               at (10,21), fac(lfac$(19%)), box13$(1%)          , ch(01),~
               at (10,23), fac(lfac$(20%)), box13a$(1%)         , ch(10),~
                                                                         ~
               at (10,39), "(14)Box 14 Label a)",                        ~
               at (10,60), fac(lfac$(21%)), box14$(1%)          , ch(12),~
                                                                         ~
               at (11,02), "                b)",                         ~
               at (11,21), fac(lfac$(19%)), box13$(2%)          , ch(01),~
               at (11,23), fac(lfac$(20%)), box13a$(2%)         , ch(10),~
                                                                         ~
               at (11,39), "          Amount a)",                        ~
               at (11,60), fac(lfac$(22%)), box14a$(1%)         , ch(10),~
                                                                         ~
               at (12,02), "                c)",                         ~
               at (12,21), fac(lfac$(19%)), box13$(3%)          , ch(01),~
               at (12,23), fac(lfac$(20%)), box13a$(3%)         , ch(10),~
                                                                         ~
               at (12,39), "           Label b)",                        ~
               at (12,60), fac(lfac$(21%)), box14$(2%)          , ch(12),~
                                                                         ~
               at (13,39), "          Amount b)",                        ~
               at (13,60), fac(lfac$(22%)), box14a$(2%)         , ch(10),~
                                                                         ~
               at (15,02), "St & Employer's St ID",                      ~
                                                                         ~
               at (16,02), fac(lfac$(23%)), st$                 , ch(02),~
               at (16,07), fac(lfac$(24%)), sid$                , ch(10),~
               at (16,26), "(17a)State Wages",                           ~
               at (16,43), fac(lfac$(25%)), swages$             , ch(10),~
               at (16,54), "(18a)State Tax",                             ~
               at (16,71), fac(lfac$(26%)), stax$               , ch(10),~
                                                                         ~
               at (17,02), fac(lfac$(27%)), st2$                , ch(02),~
               at (17,07), fac(lfac$(28%)), sid2$               , ch(10),~
               at (17,26), "(17b)         ",                             ~
               at (17,43), fac(lfac$(29%)), swages2$            , ch(10),~
               at (17,54), "(18b)         ",                             ~
               at (17,71), fac(lfac$(30%)), stax2$              , ch(10),~
                                                                         ~
               at (18,02), "(19a)Locality",                              ~
               at (18,16), fac(lfac$(31%)), lc$                 , ch(08),~
               at (18,26), "(20a)Local Wages",                           ~
               at (18,43), fac(lfac$(32%)), lwages$             , ch(10),~
               at (18,54), "(21a)Local Tax",                             ~
               at (18,71), fac(lfac$(33%)), ltax$               , ch(10),~
                                                                         ~
               at (19,02), "(19b)        ",                              ~
               at (19,16), fac(lfac$(34%)), lc2$                , ch(08),~
               at (19,26), "(20b)         ",                             ~
               at (19,43), fac(lfac$(35%)), lwages2$            , ch(10),~
               at (19,54), "(21b)           ",                           ~
               at (19,71), fac(lfac$(36%)), ltax2$              , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               gosub pf1315
               if keyhit% = 15% then L43000

               if keyhit% <> 9% then return
                  screen%  = 0%
                  goto L41000

        set_pf1
        if mode% > 1% then L45110      /* Input - Key Field      */
            pf$(1%) = "(1)Start Over     (5)Next Employee      " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "(2)First Employee (21)Next W-2          " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(0102ff1505ffffffffffffff0dff0f1000)
            return

L45110: if mode% <> 2% and mode% <> 4% then L45220  /* Full Screen */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                       (" &       ~
                      "9)Toggle Screen        (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Continue    "
            pfkeys$ = hex(01ffffffffffffff09ffffff0dff0f10ff)
            str(pf$(3%),63%,1%) = hex(84)
            return

L45220: if mode% = 5% then L45330     /*  Edit Mode - Display    */
            pf$(1%) = "(1)Start Over     (5)Next Employee     (" &       ~
                      "6)Full Screen Edit     (13)Instructions"
            pf$(2%) = "(2)First Employee (21)Next W-2         (" &       ~
                      "9)Toggle Screen        (15)Print Screen"
            pf$(3%) = "                  (8)DELETE Employee   (" &       ~
                      "11)Save & Add Addl W-2 (16)Save Data   "
            pfkeys$ = hex(0102ff150506ff0809ff0bff0dff0f10ff)
            str(pf$(3%),63%,1%) = hex(84)
            return

L45330:                              /*  Add Additional W-2     */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Continue    "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f10ff)
            str(pf$(3%),63%,1%) = hex(84)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the Employee Number                         *~
            *************************************************************

        deffn'151
            errormsg$ = " "
            init(hex(8c)) lfac$()

        REM Test for Employee Number              EMPCODE$

            plowkey$ = " "
            put plowkey$ using L50140, empcode$, 1%
L50140:         FMT CH(12), BI(1)
*          CALL "DESCRIBE" (#1, PLOWKEY$, NMN$, 0%, F1%(1))
            nmn$ = hex(06) & "Select Employee from W2 Image File. Press P~
        ~F16 to Cancel Selection"
            call "PLOWCODE" (#1, empcode$, nmn$, -3012%, -0.30,          ~
                                 f1%(1%), hdr$(), 0, 52)
            if f1%(1%) = 0% then L50200
               return clear all
               goto dataload  /* And Never */ : return

L50200:     nmn$ = hex(06) & "Select Record from Personnel File.  Press P~
        ~F16 to Cancel Selection"
            call "PLOWCODE" (#2, empcode$, nmn$, -3012%, -0.26,          ~
                                 f1%(2%), hdr$(), 0, 2)
            if f1%(2%) = 1% then L50270  /* Picked a good one? */
                if empcode$ <> " " then return
                errormsg$ = "You MUST Enter an Employee Number"
*              ERRORMSG$ = "You MUST Enter A Valid Employee Number"
                lfac$(1%) = hex(81)
                return

L50270:     plowkey$ = " "
            put plowkey$ using L50290, empcode$, 1%
L50290:         FMT CH(12), BI(1)
            call "DESCRIBE" (#1, plowkey$, nmn$, 0%, f1%(1%))
            if f1%(1%) = 0% then L50350
               return clear all
               goto dataload  /* And Never */ : return

L50350:     get #2, using L50380, last$, first$, middle$, ssn$, empcode$, ~
                                 empaddr$(1), empaddr$(2), empcity$,     ~
                                 empstate$, empzip$
L50380:         FMT XX(1), CH(15), CH(10), CH(1), CH(11), CH(12), XX(10),~
                    2*CH(30), CH(20), XX(20), CH(2), CH(9)

            if empcity$ = " " then L50411
            empaddr$(3%) = empcity$
            if empstate$ = " " then L50411
            empaddr$(3%) = empaddr$(3%) & ", " & empstate$ & "."
*          EMPADDR$(3%) = EMPCITY$ & ", " & EMPSTATE$ & "."
L50411:     empname$ = last$
            if first$ = " " then L50460
            empname$ = first$ & " " & last$
            if middle$ = " " then L50460
            empname$ = first$ & " " & middle$ & ". " & last$

L50460:     return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the Rest of the Screen                      *~
            *************************************************************

        deffn'152
            errormsg$ = " "
            init(hex(8c)) lfac$()

        REM Test for Employee Name                EMPNAME$
            if empname$ <> " " then L51600
                if errormsg$ <> " " then L51550
                   errormsg$ = "Employee Name CANNOT be Blank"
*                 SCREEN% = 0%
L51550:            lfac$(2%) = hex(80)

L51600: REM Test for Social Security Number       SSN$
            if ssn$ = " " then L51630
            if str(ssn$,4%,1%) = "-" and str(ssn$,7%,1%) = "-" then L51670
L51630:         if errormsg$ <> " " then L51750
                   errormsg$ =                                           ~
                   "SSN Must be entered and formatted as xxx-xx-xxxx"
                   goto L51740
L51670:     plowkey$ = str(ssn$,,11%) & hex(00)
            call "PLOWALTS" (#1, plowkey$, 1%, 11%, f1%(1%))
            if f1%(1%) = 0% then L51800
            if str(key(#1,0%),,12%) = str(empcode$,,12%) then L51800
                if errormsg$ <> " " then L51750
                   errormsg$ =                                           ~
                   "SSN on file for Employee Code: " & str(key(#1,0%),,12)
L51740
*                 SCREEN% = 0%
L51750:            lfac$(3%) = hex(82)

L51800: REM Test for Sort Name                    LAST$, FIRST$, MIDDLE$
            if last$   <> " " then L51900
            if first$  <> " " then L51900
            if middle$ <> " " then L51900
                if errormsg$ <> " " then L51870
                   errormsg$ = "Sort Name CANNOT be Blank"
*                 SCREEN% = 0%
L51870:            lfac$(4%) = hex(81)

L51900: REM Test for Address                      EMPADDR$()

        REM Test for Check off boxes              BOX$
            for i% = 1% to 7%
                if i% = 6% then L52040
                if box$(i%) <> " " then box$(i%) = "X"
L52040:     next i%

        REM Test for Federal Taxable Income       FEDWAGES$
            if fedwages$ = " " then fedwages$ = "0"
            call "NUMTEST" (fedwages$, 0, 9e7, error$, -2.2, fedwages)
            if error$ = " " then L52200
                if errormsg$ <> " " then L52170
                   errormsg$ = "Invalid Number entered for Federal Wages"
                   screen% = 0%
L52170:            lfac$(7%) = hex(82)

L52200: REM Test for FED Tax                      FEDTAX$
            if fedtax$ = " " then fedtax$ = "0"
            call "NUMTEST" (fedtax$, 0, 9e7, error$, -2.2, fedtax)
            if error$ = " " then L52300
                if errormsg$ <> " " then L52270
                   errormsg$ = "Invalid Number entered for Federal Tax"
                   screen% = 0%
L52270:            lfac$(8%) = hex(82)

L52300: REM Test for FICA Wages                   FICAWAGES$
            if ficawages$ = " " then ficawages$ = "0"
            call "NUMTEST" (ficawages$, 0, 9e7, error$, -2.2, ficawages)
            if error$ = " " then L52400
                if errormsg$ <> " " then L52370
                   errormsg$ = "Invalid Number entered for FICA Wages"
                   screen% = 0%
L52370:            lfac$(9%) = hex(82)

L52400: REM Test for FICA Tax                     FICATAX$
            if ficatax$ = " " then ficatax$ = "0"
            call "NUMTEST" (ficatax$, 0, 9e7, error$, -2.2, ficatax)
            if error$ = " " then L52500
                if errormsg$ <> " " then L52470
                   errormsg$ = "Invalid Number entered for FICA Tax"
                   screen% = 0%
L52470:            lfac$(10%) = hex(82)

L52500: REM Test for Medicare Wages               MEDICAREWAGES$
            if medicarewages$ = " " then medicarewages$ = "0"
            call "NUMTEST" (medicarewages$, 0, 9e7, error$, -2.2,        ~
                            medicarewages)
            if error$ = " " then L52600
                if errormsg$ <> " " then L52580
                   errormsg$ = "Invalid Number entered for Medicare Wages"
                   screen% = 0%
L52580:            lfac$(11%) = hex(82)

L52600: REM Test for Medicare Tax                 MEDICARETAX$
            if medicaretax$ = " " then medicaretax$ = "0"
            call "NUMTEST" (medicaretax$, 0, 9e7, error$, -2.2,          ~
                            medicaretax)
            if error$ = " " then L52700
                if errormsg$ <> " " then L52680
                   errormsg$ = "Invalid Number entered for Medicare Tax"
                   screen% = 0%
L52680:            lfac$(12%) = hex(82)

L52700: REM Test for FICA Tips                    FICATIPS$
            if ficatips$ = " " then ficatips$ = "0"
            call "NUMTEST" (ficatips$, 0, 9e7, error$, -2.2, ficatips)
            if error$ = " " then L52800
                if errormsg$ <> " " then L52770
                   errormsg$ = "Invalid Number entered for FICA Tips"
                   screen% = 0%
L52770:            lfac$(13%) = hex(82)

L52800: REM Test for Allocated Tips               TIPSALLOC$
            if tipsalloc$ = " " then tipsalloc$ = "0"
            call "NUMTEST" (tipsalloc$, 0, 9e7, error$, -2.2, tipsalloc)
            if error$ = " " then L52900
                if errormsg$ <> " " then L52870
                   errormsg$ = "Invalid Number entered for Allocated Tips"
                   screen% = 0%
L52870:            lfac$(14%) = hex(82)

L52900: REM Test for Advance EIC                  AEIC$
            if aeic$ = " " then aeic$ = "0"
            call "NUMTEST" (aeic$, 0, 9e7, error$, -2.2, aeic)
            if error$ = " " then L53000
                if errormsg$ <> " " then L52970
                   errormsg$ = "Invalid Number entered for Advance EIC"
                   screen% = 0%
L52970:            lfac$(15%) = hex(82)

L53000: REM Test for Dependent Care Assistance    DCASS$
            if dcass$ = " " then dcass$ = "0"
            call "NUMTEST" (dcass$, 0, 9e7, error$, -2.2, dcass)
            if error$ = " " then L53100
                if errormsg$ <> " " then L53080
                   errormsg$ =                                           ~
                   "Invalid Number entered for Dependent Care Assistance"
                   screen% = 0%
L53080:            lfac$(16%) = hex(82)

L53100: REM Test for Non Qualified Plans          NQ457$, NQN457$
            if nq457$ = " " then nq457$ = "0"
            if nqn457$ = " " then nqn457$ = "0"
            call "NUMTEST" (nq457$, 0, 9e7, error$, -2.2, nq457)
               if error$ <> " " then L53170
            call "NUMTEST" (nqn457$, 0, 9e7, error$, -2.2, nqn457)
            if error$ = " " then L53300
L53170:         if errormsg$ <> " " then L53210
                   errormsg$ =                                           ~
                         "Invalid Number entered for Non Qualified Plans"
                   screen% = 0%
L53210:            lfac$(17%) = hex(82)

L53300: REM Test for Fringe Benefits              FRINGE$
            if fringe$ = " " then fringe$ = "0"
            call "NUMTEST" (fringe$, 0, 9e7, error$, -2.2, fringe)
            if error$ = " " then L53400
                if errormsg$ <> " " then L53380
                   errormsg$ =                                           ~
                    "Invalid Number entered for Box 12 Benefits"
                   screen% = 0%
L53380:            lfac$(18%) = hex(82)

L53400: REM Test for Box 13 Identifiers           BOX13$
            for i% = 1% to 4%
                if box13$(i%) = " " then L53480
                if pos("ABCDEFGH JKLMN" = box13$(i%)) <> 0% then L53480
                   if errormsg$ <> " " then L53470
                      errormsg$ ="Invalid Identification Code for Box 13"
                      screen% = 1%
L53470:               lfac$(19%) = hex(81)
L53480:     next i%

        REM Box 13 Amounts                        BOX13A$
            for i% = 1% to 4%
            if box13a$(i%) = " " then box13a$(i%) = "0"
            call "NUMTEST" (box13a$(i%), 0, 9e7, error$, -2.2, box13(i%))
               if error$ = " " then L53600
                   if errormsg$ <> " " then L53590
                      errormsg$ =                                        ~
                       "Invalid Number entered for Box 13 Amount"
                      screen% = 1%
L53590:               lfac$(20%) = hex(82)  :  lfac$(19%) = hex(81)
L53600:     if box13(i%) > 0 and box13$(i%) = " "  then L53610 else L53660
L53610:            if errormsg$ <> " " then L53650
                      errormsg$ =                                        ~
                       "Box 13 Amounts Must have an Identification Code"
                      screen% = 1%
L53650:               lfac$(19%) = hex(81)  :  lfac$(20%) = hex(82)
L53660:     next i%

        REM Box 14 Descriptions                   BOX14$

        REM Box 14 Amounts                        BOX14A$
            for i% = 1% to 2%
            if box14a$(i%) = " " then box14a$(i%) = "0"
            call "NUMTEST" (box14a$(i%), 0, 9e7, error$, -2.2, box14(i%))
               if error$ = " " then L53820
                   if errormsg$ <> " " then L53810
                      errormsg$ =                                        ~
                       "Invalid Number entered for Box 14 Amount"
                      screen% = 1%
L53810:               lfac$(22%) = hex(82)  :  lfac$(21%) = hex(81)
L53820:     if box14(i%) > 0 and box14$(i%) = " "  then L53830 else L53880
L53830:            if errormsg$ <> " " then L53870
                      errormsg$ =                                        ~
                       "Box 14 Amounts Must have a Description"
                      screen% = 1%
L53870:               lfac$(21%) = hex(81)  :  lfac$(22%) = hex(82)
L53880:     next i%

        REM Test for State ID Number              SID$
            /* No edit Performed  */
            /* LFAC$(24%)          */

        REM Test for State Taxable Income         SWAGES$
            if swages$ = " " then swages$ = "0"
            call "NUMTEST" (swages$, 0, 9e7, error$, -2.2, swages)
            if error$ = " " then L54100
                if errormsg$ <> " " then L54070
                   errormsg$ = "Invalid Number entered for State Wages(a)"
                   screen% = 1%
L54070:            lfac$(25%) = hex(82)

L54100: REM Test for State Tax                    STAX$
            if stax$ = " " then stax$ = "0"
            call "NUMTEST" (stax$, 0, 9e7, error$, -2.2, stax)
            if error$ = " " then L54200
                if errormsg$ <> " " then L54170
                   errormsg$ = "Invalid Number entered for State Taxes(a)"
                   screen% = 1%
L54170:            lfac$(26%) = hex(82)

L54200: REM Test for State ID                     ST$
            if swages = 0 and stax = 0 and st$ <> " " then st$ = " "
            if swages = 0 and stax = 0 and st$ = " " then L54300
            if st$ <> " " then L54300
                if errormsg$ <> " " then L54280
                   errormsg$ =                                           ~
                   "You MUST Enter a State Code if Reporting State Wages"
                   screen% = 1%
L54280:            lfac$(23%) = hex(81)

L54300: REM Test for State ID Number              SID2$
            /* No edit Performed  */
            /* LFAC$(28%)          */

        REM Test for State Taxable Income         SWAGES2$
            if swages2$ = " " then swages2$ = "0"
            call "NUMTEST" (swages2$, 0, 9e7, error$, -2.2, swages2)
            if error$ = " " then L54500
                if errormsg$ <> " " then L54470
                   errormsg$ = "Invalid Number entered for State Wages(b)"
                   screen% = 1%
L54470:            lfac$(29%) = hex(82)

L54500: REM Test for State Tax                    STAX2$
            if stax2$ = " " then stax2$ = "0"
            call "NUMTEST" (stax2$, 0, 9e7, error$, -2.2, stax2)
            if error$ = " " then L54600
                if errormsg$ <> " " then L54570
                   errormsg$ = "Invalid Number entered for State Taxes(b)"
                   screen% = 1%
L54570:            lfac$(30%) = hex(82)

L54600: REM Test for State ID                     ST2$
            if swages2 = 0 and stax2 = 0 and st2$ <> " " then st2$ = " "
            if swages2 = 0 and stax2 = 0 and st2$ = " " then L54800
            if st2$ <> " " then L54800
                if errormsg$ <> " " then L54680
                   errormsg$ =                                           ~
                   "You MUST Enter a State Code if Reporting State Wages"
                   screen% = 1%
L54680:            lfac$(27%) = hex(81)

L54800: REM Test for Local Taxable Income         LWAGES$
            if lwages$ = " " then lwages$ = "0"
            call "NUMTEST" (lwages$, 0, 9e7, error$, -2.2, lwages)
            if error$ = " " then L54900
                if errormsg$ <> " " then L54870
                   errormsg$ = "Invalid Number entered for Local Wages(a)"
                   screen% = 1%
L54870:            lfac$(32%) = hex(82)

L54900: REM Test for Local Tax                    LTAX$
            if ltax$ = " " then ltax$ = "0"
            call "NUMTEST" (ltax$, 0, 9e7, error$, -2.2, ltax)
            if error$ = " " then L55000
                if errormsg$ <> " " then L54970
                   errormsg$ = "Invalid Number entered for Local Taxes(a)"
                   screen% = 1%
L54970:            lfac$(33%) = hex(82)

L55000: REM Test for Local ID Code                LC$
            if lwages = 0 and ltax = 0 and lc$ <> " " then lc$ = " "
            if lwages = 0 and ltax = 0 and lc$ = " " then L55100
            if lc$ <> " " then L55100
                if errormsg$ <> " " then L55080
                   errormsg$ =                                           ~
                   "You MUST Enter a Local Code if Reporting Local Wages"
                   screen% = 1%
L55080:            lfac$(31%) = hex(81)

L55100: REM Test for Local Taxable Income         LWAGES2$
            if lwages2$ = " " then lwages2$ = "0"
            call "NUMTEST" (lwages2$, 0, 9e7, error$, -2.2, lwages2)
            if error$ = " " then L55200
                if errormsg$ <> " " then L55170
                   errormsg$ = "Invalid Number entered for Local Wages(b)"
                   screen% = 1%
L55170:            lfac$(35%) = hex(82)

L55200: REM Test for Local Tax                    LTAX2$
            if ltax2$ = " " then ltax2$ = "0"
            call "NUMTEST" (ltax2$, 0, 9e7, error$, -2.2, ltax2)
            if error$ = " " then L55300
                if errormsg$ <> " " then L55270
                   errormsg$ = "Invalid Number entered for Local Taxes(b)"
                   screen% = 1%
L55270:            lfac$(36%) = hex(82)

L55300: REM Test for Local ID Code                LC2$
            if lwages2 = 0 and ltax2 = 0 and lc2$ <> " " then lc2$ = " "
            if lwages2 = 0 and ltax2 = 0 and lc2$ = " " then L55400
            if lc2$ <> " " then L55400
                if errormsg$ <> " " then L55380
                   errormsg$ =                                           ~
                   "You MUST Enter a Local Code if Reporting Local Wages"
                   screen% = 1%
L55380:            lfac$(34%) = hex(81)

L55400: /* And finally */:return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
