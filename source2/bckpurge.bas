        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    CCC   K   K  PPPP   U   U  RRRR    GGG   EEEEE   *~
            *  B   B  C   C  K  K   P   P  U   U  R   R  G      E       *~
            *  BBBB   C      KKK    PPPP   U   U  RRRR   G GGG  EEEE    *~
            *  B   B  C   C  K  K   P      U   U  R   R  G   G  E       *~
            *  BBBB    CCC   K   K  P       UUU   R   R   GGG   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKPURGE - Purges sales orders from regular BCK files     *~
            *            and optionally writes purged orders to BCK     *~
            *            History files. Only closed orders can be       *~
            *            purged/moved.                                  *~
            *-----------------------------------------------------------*~
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
            * 12/28/87 ! Original                                 ! JDH *~
            * 05/12/88 ! Create BCKHxxxx files to 1/2 of BCKxxxxx ! JDH *~
            * 12/27/89 ! No creation of BCKHCLNS if no BCKLNCUR.  ! JDH *~
            * 03/08/94 ! Changed RL for BOMSPEC and added BOMSPHDR! WPH *~
            * 01/16/95 ! Add Precious Metal Surcharges Removal    ! RJH *~
            * 04/03/95 ! PRR 13187 - XREF Shadow records removal  ! RJH *~
            * 04/28/95 ! On delete of BOMSPEC Prime the key better! RJH *~
            * 05/01/95 ! Also Delete Orphane BOMSPEC Records      ! RJH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            filler$(20)25,               /* Scalar Filler > 255 Char.  */~
            history$1,                   /* Write to History (Y/N)?    */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line$(3)100,                 /* Scalar Line Record         */~
            line2$79,                    /* Screen Line #2             */~
            master$(10)100,              /* Scalar Master Record       */~
            masterkey$25,                /* Primary Key - BCKMASTR     */~
            oldpurge$8,                  /* Last Used Purge-to Date    */~
            olduser$3,                   /* Last USERID from SYS-2     */~
            oldwrite$1,                  /* Last Purge: Write to Hist? */~
            options$1,                   /* Purge Options (Y/N)?       */~
            orderdate$6,                 /* Date of Order from BCKMASTR*/~
            orphans$1,                   /* Delete Orphans (Y/N/Only)? */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            purgedate$8,                 /* Purge-to Date              */~
            readkey$90,                  /* Miscellaneous Read/Plow Key*/~
            reedkey$90,                  /* Miscellaneous Read/Plow Key*/~
            shadow$100,                  /* Shadow Record              */~
            text$1,                      /* Purge Text (Y/N)?          */~
            textid$4,                    /* Master File Text ID        */~
            textid1$4,                   /* Line  File Text ID         */~
            userid$3,                    /* Current User Id            */~
            xref_descr$32,               /* Xref Part # Description    */~
            xref_part$25,                /* Xref Part Number           */~
            xref_type$1                  /* Xref Part Type(Cust or Mnf)*/

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
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
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
            * # 1 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 2 ! TXTFILE  ! System Text File                         *~
            * # 3 ! BCKMASTR ! BACKLOG MASTER FILE (GET STORE NUMBER)   *~
            * # 4 ! BCKLINES ! BACK LOG LINE ITEM FILE                  *~
            * # 5 ! BOMSPEC  ! options selected file                    *~
            * # 6 ! DEMMASTR ! Demand Master File                       *~
            * # 7 ! BCKLNCUR ! Currency specific counterpart to BCKLINE *~
            * # 8 ! BCKHMSTR ! BCK History Master File                  *~
            * # 9 ! BCKHLNES ! BCK History Line Item File               *~
            * #10 ! BCKHCLNS ! BCK History Currency Line File           *~
            * #11 ! BOMSPHDR ! Header file for options                  *~
            * #12 ! BCKPMSLD ! Precious Metal SO Shadow File            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select # 2, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11                      ~

            select # 3, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup     ~

            select # 4, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19                      ~

            select # 5, "BOMSPEC",                                       ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  54,                     ~
                        alt key  1, keypos =   57, keylen =  23          ~

            select # 6, "DEMMASTR",                                      ~
                        varc,     indexed,  recsize =  123,              ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28          ~

            select # 7, "BCKLNCUR",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    5, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  23          ~

            select # 8, "BCKHMSTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup     ~

            select # 9, "BCKHLNES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19                      ~

            select #10, "BCKHCLNS",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    5, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  23          ~

            select #11, "BOMSPHDR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  53,                     ~
                        alt key  1, keypos =   35, keylen =  19

            select #12, "BCKPMSLD",                                      ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos = 1,    keylen =  29,                     ~
                         alternate key 1, keypos = 30, keylen = 25, dup, ~
                                   key 2, keypos = 73, keylen =  9, dup, ~
                                   key 3, keypos = 20, keylen = 10, dup
            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
              rec3% = val(str(rslt$(3), 17, 4), 4)
              rec3% = max(int(rec3% / 2), 100%)
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
              rec4% = val(str(rslt$(4), 17, 4), 4)
              rec4% = max(int(rec4% / 2), 100%)
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 0%, rslt$( 5))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6), 0%, rslt$( 6))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7), 0%, rslt$( 7))
            call "OPENCHCK" (# 8, fs%( 8), f2%( 8), rec3%, rslt$( 8))
            call "OPENCHCK" (# 9, fs%( 9), f2%( 9), rec4%, rslt$( 9))
            if fs%(7) <> 1% then L02770
                rec7% = val(str(rslt$(7), 17, 4), 4)
                rec7% = max(int(rec7% / 2), 100%)
                call "OPENCHCK" (#10, fs%(10), f2%(10), rec7%, rslt$(10))
L02770:     call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
            call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, rslt$(12))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "BCKPURGE: " & str(cms2v$,,8)

            call "DATE" addr ("G+", date, -90%, mindate$, err%)
                if err% <> 0 then exit_program
            call "DATEOK" (mindate$, mindate%, errormsg$)
                if errormsg$ <> " " then exit_program

L09200:     call "READ101" (#1, "BCK.PURGE.CTL", f1%(1))
                if f1%(1) = 0 then write_new
              get #1, using L09240, olduser$, purgedate$, history$, text$, ~
                                  options$, orphans$
L09240:       FMT POS(21), CH(3), CH(6), CH(1), CH(1), CH(1), CH(1)
                oldpurge$ = purgedate$
                oldwrite$ = history$
              call "DATEFMT" (oldpurge$)
              str(line2$,,35) = "Last Purge-to Date Used:   " & oldpurge$

            REM TESTING FOR RESTARTING & MULTI-TASKING
              if olduser$ = " " then write_sys2
            call "STRTRLSE" addr(#1)
              if olduser$ = userid$ then L09390
L09330:     u3% = 0%
            call "ASKUSER" (u3%, "BCKPURGE ALREADY IN PROGRESS", " ",    ~
                            "Press <RETURN> to Exit", " ")
              if u3% <> 0% then L09330
            goto L65210

L09390:     u3% = 0%
            call "ASKUSER" (u3%, "*** RESTART ***", " ", "Press <RETURN"&~
                  "> to Restart ", " ")
              if u3% <> 0% then L09390
            goto startpurge

        REM Write 1st BCK.PURGE.CTL Record to SYS2
        write_new
            put#1, using L09540, "BCK.PURGE.CTL", " ", "010101", "N", "Y", ~
                            "Y", "N", "010101", str(filler$(), 1, 461)
L09540:     FMT CH(20), CH(3), CH(6), CH(1), CH(1), CH(1), CH(1), CH(6), ~
                      CH(461)
            write #1
            goto L09200

        REM Write to SYS2 when NOT restarting
        write_sys2
            put #1, using L09630, userid$
L09630:     FMT POS(21), CH(3)
            rewrite #1
            goto inputmode

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  5%
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
                  if keyhit%  = 16% then       safeguard
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  5% then editpg1
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
            *             S T A R T   P U R G E                         *~
            *-----------------------------------------------------------*~
            * START PURGING DATA FROM BCK FILES                         *~
            *************************************************************

        startpurge

            call "SHOSTAT" ("Purge of Data in Process")


            if orphans$ = "O" then L19990

L19130:         gosub get_mastr
                  if f1%(3) = 0% then L19990

                gosub purge_xref_shadow

                gosub purge_lines

                gosub purge_shadow

                gosub purge_pmetal_shadow

                gosub purge_master

                goto L19130

L19990:         goto  del_orphans

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Purge-to Date          */~
                              L20200,         /* Write to History       */~
                              L20300,         /* Purge Text (Y/N)?      */~
                              L20400,         /* Purge Options          */~
                              L20500          /* Delete Orphans         */
            return
L20100: REM Def/Enable Purge-to Date               PURGEDATE$
            purgedate$ = mindate$
            return

L20200: REM Def/Enable Write to History (Y/N)?     HISTORY$
            history$ = "Y"
            return

L20300: REM Def/Enable Purge Text (Y/N)?           TEXT$
            text$ = "Y"
            return

L20400: REM Def/Enable Purge Options (Y/N)?        OPTIONS$
            options$ = "Y"
            return

L20500: REM Def/Enable Delete Orphans (Y/N/Only)?  ORPHANS$
            orphans$ = "N"
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
         "Enter Purge-to Date                                          ",~
         "Enter Write to History (Y/N)?                                ",~
         "Enter Purge Text (Y/N)?                                      ",~
         "Enter Purge Options (Y/N)?                                   ",~
         "Enter Delete Orphans (Y/N/Only)?                             "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, plowkey$,                  ~
                      history$, options$, orphans$, purgedate$, text$
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
            *          G E T   A   M A S T E R    R E C O R D           *~
            *************************************************************
        get_mastr

L30060:     call "READNXT1" (#3, f1%(3))
            if f1%(3) = 0% then return
                get #3, using L30090, readkey$, textid$, orderdate$
L30090:              FMT XX(9), CH(16), POS(799), CH(4), POS(806), CH(6)
                if orderdate$ > purgedate$ then L30060

            /* Check for lines with Open Qtys      */
                init (hex(00)) plowkey$
                str(plowkey$,1,16) = str(readkey$,,16)       /* SO#    */
                call "PLOWNEXT" (#4, plowkey$, 16%, f1%(4))
                goto L30190

L30180:         call "READNEXT" (#4, f1%(4))
L30190:         if f1%(4) = 0% then L30260       /* And away he goes!  */
                if str(key(#4),,16) <> str(readkey$,,16) then L30260
                     get #4 using L30220, openqty
L30220:                   FMT POS(109), PD(14,4)
                     if abs(openqty) > .0001 then L30060 /* Next Order */
                     goto L30180

L30260:     get #3 using L30270, str(master$(),,1000)
L30270:          FMT CH(1000)
            get #3 using L30276, masterkey$
L30276:          FMT CH(25)
            return


        REM *************************************************************~
            *           P U R G E     L I N E S                         *~
            *************************************************************
        purge_lines

            plowkey$ = str(readkey$,,16) & hex(00)
L31060:     call "PLOWNXT1" (#4, plowkey$, 16%, f1%(4))
            if f1%(4) = 0% then return
                get #4 using L31074, str(line$(),,300)
L31074:              FMT CH(300)
                get #4 using L31090, textid1$
L31090:              FMT POS(242), CH(4)

                if history$ = "N" then L31100
                write #9 using L31096, str(line$(),,300), eod goto L31100
L31096:              FMT CH(300)

L31100:       call "DELETE" (#4, plowkey$, 19%)

                if text$ = "N" then L31125
                call "TXTFUTIL" (#2, f2%(2), "DELE", textid1$)

L31125:         if options$ = "N" then L31060
                call "REDALT0" (#6, plowkey$, 1%,f1%(6)) /* DEMMASTR */
                if f1%(6) = 1% then L31060
*                   REEDKEY$ = STR(PLOWKEY$,,19) & HEX(00)
                     reedkey$ = str(plowkey$,,19) & hex(00000000)
*        Retain BOMSPEC records if header record exists in BOMSPHDR
                call "REDALT1"(#11, reedkey$, 1%, f1%(11))  /* header */
                   if f1%(11) = 0% then L31160
                   goto L31060  /* next line */

L31160:              call "PLOWAL1" (#5, reedkey$, 1%, 19%, f1%(5))
                     if f1%(5) = 0% then L31060
                          delete #5     /* Bye Bye BOMSPEC */
                          goto L31160

        REM *************************************************************~
            *           P U R G E     S H A D O W                       *~
            *************************************************************
        purge_shadow

            plowkey$ = str(readkey$,,16) & hex(00)
L32060:     call "PLOWNXT1" (#7, plowkey$, 16%, f1%(7))
            if f1%(7) = 0% then return
                get #7 using L32090, shadow$
L32090:              FMT CH(100)

                if history$ = "N" then L32170
                write #10 using L32150, shadow$, eod goto L32170
L32150:              FMT CH(100)

L32170:       call "DELETE" (#7, plowkey$, 19%)
                goto L32060
        REM *************************************************************~
            *           P U R G E     M A S T E R                       *~
            *************************************************************
        purge_master

                if history$ = "N" then L33090
              write #8 using L33070, str(master$(),,1000), eod goto L33090
L33070:              FMT CH(1000)

L33090:       call "DELETE" (#3, masterkey$, 25%)

                if text$ = "N" then return
                call "TXTFUTIL" (#2, f2%(2), "DELE", textid$)

            return


        REM *************************************************************~
            *           P U R G E   P M E T A L   S H A D O W           *~
            *************************************************************
        purge_pmetal_shadow
              plowkey$ = str(readkey$,,16%) & hex(00)
              call "DELETE" (#12, plowkey$, 16%)
              return

        REM *************************************************************~
            *              P U R G E   X R E F    S H A D O W           *~
            *************************************************************
        purge_xref_shadow
              if history$ <> "N" then L33320
              call "PTUSEDSB" ("D", "BCK ", str(readkey$,,16%), "ALL",   ~
                               " ", " ", " ", ret%)/* Del Xref Shadow */
              return
L33320:
              plowkey$ = str(readkey$,,16%) & hex(00)
L33340:       call "PLOWNXT1" (#4, plowkey$, 16%, f1%(4%))
              if f1%(4%) = 0% then return

              /* Get Xref Part Number, if available */
              call "PTUSEDSB" ( "R", "BCK ",                             ~
                               str(plowkey$,,16%), str(plowkey$,17%,3%), ~
                               xref_part$, xref_descr$, xref_type$, ret%)
              if ret% = 0% then L33340     /* Try for next one */

              /* Save to History Xref Part Number  */
              call "PTUSEDSB" ( "W", "BCKH",                             ~
                               str(plowkey$,,16%), str(plowkey$,17%,3%), ~
                               xref_part$, xref_descr$, xref_type$, ret%)

              /* Delete the old record */
              call "PTUSEDSB" ( "D", "BCK ",                             ~
                               str(plowkey$,,16%), str(plowkey$,17%,3%), ~
                               " ", " ", " ", ret%)
              goto L33340                  /* Try for next one */

        REM *************************************************************~
            *            P U R G E    O R P H A N S                     *~
            *************************************************************
        del_orphans

            if orphans$ = "N" then exit_program

          REM Find orphan lines
                init (hex(00)) plowkey$, readkey$
L39090:         call "PLOWNEXT" (#4, readkey$, 0%, f1%(4))
                if f1%(4) = 0% then L39190
                     get #4 using L39120, plowkey$
L39120:                   FMT CH(25)
                     call "READ100"  (#3, plowkey$, f1%(3))
                     if f1%(3) <> 0% then L39160
                          call "DELETE" (#4, readkey$, 16%)
                REM Get rid of shadow too
                          call "DELETE" (#7, readkey$, 16%)
L39160:              init (hex(ff)) str(readkey$,17)
                     goto L39090

L39190
*        ** Check for BOMSPEC Orphans ** *
          init (hex(00))  plowkey$, readkey$
L39210:   call "PLOWAL1" (#5, plowkey$, 1%, 0%, f1%(5%))
            if f1%(5%) = 0% then L39360
*        Retain BOMSPEC records if BCKLINES
          readkey$ = str(plowkey$,,19%)
          call "READ100" (#4, readkey$, f1%(4%))    /* BCK Lines File */
            if f1%(4%) = 1% then L39210
*        Retain BOMSPEC records associated with a DEMMASTR
          readkey$ = str(plowkey$,,19%)
          call "REDALT0" (#6, readkey$, 1%,f1%(6%)) /* DEMMASTR */
            if f1%(6%) = 1% then L39210    /* Back for another BOMSPEC */
*        Retain BOMSPEC records if header record exists in BOMSPHDR
          readkey$ = str(plowkey$,,19%) & hex(00000000)
          call "REDALT0"(#11, readkey$, 1%, f1%(11%))  /* header */
            if f1%(11%) = 1% then L39210
*        Retain BOMSPEC records if BCKHLINS
          readkey$ = str(plowkey$,,19%)
          call "READ100" (#9, readkey$, f1%(9%))    /* BCK History File */
            if f1%(9%) = 1% then L39210

          delete #5
          goto L39210       /* Back for another BOMSPEC */

L39360:   REM OK, DONE WITH ORPHANS
            goto exit_program

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
              on fieldnr% gosub L40095,         /* Purge-to Date     */   ~
                                L40095,         /* Write to History  */   ~
                                L40095,         /* Purge Text (Y/N)? */   ~
                                L40095,         /* Purge Options     */   ~
                                L40095          /* Delete Orphans    */
              goto L40110

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40095:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40110:     accept                                                       ~
               at (01,02),                                               ~
                  "Purge Sales Orders",                                  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Purge-to Date",                              ~
               at (06,30), fac(lfac$( 1)), purgedate$           , ch(08),~
                                                                         ~
               at (07,02), "Write to History (Y/N)?",                    ~
               at (07,30), fac(lfac$( 2)), history$             , ch(01),~
                                                                         ~
               at (08,02), "Purge Text (Y/N)?",                          ~
               at (08,30), fac(lfac$( 3)), text$                , ch(01),~
                                                                         ~
               at (09,02), "Purge Options (Y/N)?",                       ~
               at (09,30), fac(lfac$( 4)), options$             , ch(01),~
                                                                         ~
               at (10,02), "Delete Orphans (Y/N/Only)?",                 ~
               at (10,30), fac(lfac$( 5)), orphans$             , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40280
                  call "MANUAL" ("BCKPURGE") : goto L40110

L40280:        if keyhit% <> 15 then L40295
                  call "PRNTSCRN" : goto L40110

L40295:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40390     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40370
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40370:     if fieldnr% > 2% then L40380
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40380:     return

L40390: if fieldnr% > 0% then L40435  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Purge Data  "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40435:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *           S A F E G U A R D   S C R E E N S               *~
            *-----------------------------------------------------------*~
            *     User input to really do this process!!                *~
            *************************************************************

        safeguard

L49080:     u3% = 0%
            call "ASKUSER" (u3%, "PURGE ABOUT TO START", "Are you SURE?",~
                            "Press <RETURN> to continue.",               ~
                            "Press PF(16) to return to Input Screen")
                if u3% = 16% then editpg1
                if u3% <> 0% then L49080

            if oldwrite$ = "N" then L49500
            if history$ = "Y" then L49500
L49170:     u3% = 0%
            call "ASKUSER" (u3%, " *** ATTENTION *** ", "This Purge wil"&~
             "l NOT write to History;  Last Purge DID write to History!",~
                             "Press <RETURN> to continue.",              ~
                             "Press PF(16) to return to Input Screen")
                if u3% = 16% then editpg1
                if u3% <> 0% then L49170

L49500: REM *************************************************************~
            *           W R I T E   T O   S Y S 2 - B C K               *~
            *-----------------------------------------------------------*~
            *   Writes parameters to SYS2-BCK for next purge & restart  *~
            *************************************************************

            call "READ101" (#1, "BCK.PURGE.CTL", f1%(1))
                if f1%(1) = 0 then write_new
                  if orphans$ = "O" then purgedate$ = oldpurge$
                  if orphans$ = "O" then history$ = oldwrite$
                    call "DATUNFMT" (purgedate$)
              put #1, using L49580, userid$, purgedate$, history$, text$, ~
                                  options$, orphans$
L49580:       FMT POS(21), CH(3), CH(6), CH(1), CH(1), CH(1), CH(1)
              rewrite #1
              goto startpurge

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* Purge-to Date          */~
                              L50200,         /* Write to History       */~
                              L50300,         /* Purge Text (Y/N)?      */~
                              L50400,         /* Purge Options          */~
                              L50500          /* Delete Orphans         */
            return
L50100: REM Test for Purge-to Date                PURGEDATE$
                call "DATEOK" (purgedate$, date%, errormsg$)
                if errormsg$ <> " "      then return
                if date%     <= mindate% then return
                     errormsg$ = "Date for Purge Must Be ON or BEFORE " &~
                                 mindate$ & "! "
            return

L50200: REM Test for Write to History (Y/N)?      HISTORY$
              if history$ = "Y" or history$ = "N" then return
            errormsg$ = "'Y' for Yes;  'N' for No."
            return

L50300: REM Test for Purge Text (Y/N)?            TEXT$
              if text$ = "Y" or text$ = "N" then return
            errormsg$ = "'Y' for Yes;  'N' for No."
            return

L50400: REM Test for Purge Options (Y/N)?         OPTIONS$
              if options$ = "Y" or options$ = "N" then return
            errormsg$ = "'Y' for Yes;  'N' for No."
            return

L50500: REM Test for Delete Orphans (Y/N/Only)?   ORPHANS$
              if orphans$ = "Y" or orphans$ = "N" or orphans$ = "O"      ~
                 then return
            errormsg$ = "'Y' for Yes;  'N' for No;  'O' for Only."
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
            call "READ101" (#1, "BCK.PURGE.CTL", f1%(1))
                if f1%(1) = 0% then L65210
            put #1, using L65185, " "
L65185:     FMT POS(21), CH(3)
            rewrite #1
L65210:     end
