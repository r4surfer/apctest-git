        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  V   V  BBBB   K   K  H   H  PPPP   RRRR    GGG   EEEEE   *~
            *  V   V  B   B  K  K   H   H  P   P  R   R  G      E       *~
            *  V   V  BBBB   KKK    HHHHH  PPPP   RRRR   G GGG  EEEE    *~
            *   V V   B   B  K  K   H   H  P      R   R  G   G  E       *~
            *    V    BBBB   K   K  H   H  P      R   R   GGG   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VBKHPRGE - Purges purchase orders from the VBK History    *~
            *            files.  Will purge to a date by vendor, store, *~
            *            and/or purchase order #.  Optionally prints    *~
            *            facsimile purchase orders of those that are    *~
            *            deleted.                                       *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/18/91 ! Original                                 ! JDH *~
            * 09/02/97 ! Chnages for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            company$50,                  /* Company header             */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            duedate$8,                   /* Due Date                   */~
            edtmessage$79,               /* Edit screen message        */~
            evend$9,                     /* End Vendor for Plow        */~
            endvend$9,                   /* End Vendor for Screen      */~
            endpo$16,                    /* End Purch Order for Screen */~
            endstore$3,                  /* End Store for Screen       */~
            errormsg$79,                 /* Error message              */~
            epo$16,                      /* End Purchase Order for Plow*/~
            estore$3,                    /* End Store for Plow         */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            masterkey$25,                /* Plowkey for PO$            */~
            mindate$8,                   /* PURGEDATE Limit            */~
            orderdate$8,                 /* Order date                 */~
            part$25,                     /* Part Number                */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            po$16,                       /* Purchase Order number      */~
            pocopy$1,                    /* Paper Copy of Purges?      */~
            purgedate$10,                /* Purge-to Date              */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            svend$9,                     /* Start Vendor for Plow      */~
            spo$16,                      /* Start Purch Order for Plow */~
            sstore$3,                    /* Start Store for Plow       */~
            startvend$9,                 /* Start Vendor for Screen    */~
            startpo$16,                  /* Start Purch Order for Scrn */~
            startstore$3,                /* Start Store for Screen     */~
            store$4,                     /* Store number               */~
            textid$4,                    /* Master File Text ID        */~
            textid1$4,                   /* Line  File Text ID         */~
            titl$(3)25,                  /* Screen title line          */~
            userid$3,                    /* Current User Id            */~
            vendname$30,                 /* Vendor Description         */~
            vendor$9                     /* Vendor code                */

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
            * # 1 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 2 ! VBKHMSTR ! BACKLOG MASTER FILE (GET STORE NUMBER)   *~
            * # 3 ! VBKHLNES ! BACK LOG LINE ITEM FILE                  *~
            * # 4 ! VBKHCLNS ! Currency specific counterpart to VBKLINE *~
            * # 5 ! TXTFILE  ! System Text File                         *~
            * # 6 ! VBKCHNGH ! PO Changes History - Headers             *~
            * # 7 ! VBKCHNGL ! PO Changes History - Line Items          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select # 2, "VBKHMSTR",                                      ~
                        varc,     indexed,  recsize = 1030,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   10, keylen =  16

            select # 3, "VBKHLNES",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28

            select # 4, "VBKHCLNS",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    5, keylen =  28,                     ~
                        alt key  1, keypos =    1, keylen =  32

            select # 5, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11

            select # 6, "VBKCHNGH",                                      ~
                        varc, indexed,                                   ~
                        recsize = 1163,                                  ~
                        keypos = 1, keylen = 32

            select # 7, "VBKCHNGL",                                      ~
                        varc, indexed,                                   ~
                        recsize = 736,                                   ~
                        keypos = 1, keylen = 35

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 0%, rslt$( 5))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6), 0%, rslt$( 6))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7), 0%, rslt$( 7))

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

            str(line2$,62) = "VBKHPRGE: " & str(cms2v$,,8)

            flag% = 0%            /* Print Flag */

            titl$(1) = "Selection"
            titl$(2) = "Beginning Selection"
            titl$(3) = "Ending Selection"

            call "DATE" addr ("G+", date, -90%, mindate$, err%)
                if err% <> 0 then exit_program
            call "DATEOK" (mindate$, mindate%, errormsg$)
                if errormsg$ <> " " then exit_program

            call "READ101" (#1, "VBK.PURGE.CTL", f1%(1))
                if f1%(1) = 0 then write_new
              get #1, using L09270, olduser$, purgedate$, startvend$,      ~
                                  endvend$, startstore$, endstore$,      ~
                                  startpo$, endpo$, pocopy$

L09270:           FMT POS(21), CH(3), POS(34), CH(6), CH(9), CH(9),      ~
                      CH(3), CH(3), CH(16), CH(16), XX(4), XX(4), CH(1)
              oldpurge$ = purgedate$
              call "DATEFMT" (oldpurge$)
              str(line2$,,35) = "Last Purge-to Date Used:   " & oldpurge$

            REM TESTING FOR RESTARTING & MULTI-TASKING
              if olduser$ = " " then write_sys2
            call "ALLFREE"
              if olduser$ = userid$ then L09430
L09370:     u3% = 0%
            call "ASKUSER" (u3%, "VBKHPRGE ALREADY IN PROGRESS", " ",    ~
                            "Press <RETURN> to Exit", " ")
              if u3% <> 0% then L09370
            goto L65210

L09430:     u3% = 0%
            call "ASKUSER" (u3%, "*** RESTART ***", " ", "Press <RETURN"&~
                  "> to Restart ", " ")
              if u3% <> 0% then L09430
            goto startpurge

        REM Write 1st VBK.PURGE.CTL Record to SYS2
        write_new
L09505:     u3% = 0%
            call "ASKUSER" (u3%, "*** ATTENTION ***",                    ~
                    "NO Records have been written to History Files, Yet",~
                    " ", "Press <RETURN> to Exit.")
            if u3% <> 0% then L09505
            goto L65210

        REM Write to SYS2 when NOT restarting
        write_sys2
            put #1, using L09600, userid$
L09600:     FMT POS(21), CH(3)
            rewrite #1
            goto inputmode

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 5%
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
L11120:     fieldnr% = cursor%(1%) - 6%
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
            * START PURGING DATA FROM VBK FILES                         *~
            *************************************************************

        startpurge

            call "SHOSTAT" ("Purge of Data in Process")

            if pocopy$ = "Y" then gosub set_printer

L19100:     gosub get_master
                if f1%(2) = 0% then exit_program

            gosub purge_lines

            gosub purge_shadow

            gosub purge_master

            goto L19100

        REM SET UP PRINTER
          set_printer
            call "COMPNAME" (12%, company$, u3%)
            page% = 0%
            line% = 999%
            select printer(134)
            call "SETPRNT" ("VBK007", " ", 0%, 0%)
            gosub check_page
            return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Purge-to Date          */~
                              L20200,         /* Vendor Range           */~
                              L20300,         /* Store Range            */~
                              L20400,         /* Purch Order Range      */~
                              L20600          /* Paper Copy?            */
            return
L20100: REM Def/Enable Purge-to Date               PURGEDATE$
            purgedate$ = mindate$
            call "DATEOKC" (purgedate$, 0%, " ")
            return

L20200: REM Def/Enable Vendor Range                STARTVEND$
            startvend$ = "ALL"
            return

L20300: REM Def/Enable Store Range                 STARTSTORE$
            startstore$ = "ALL"
            return

L20400: REM Def/Enable Purchase Order Range        STARTPO$
            startpo$ = "ALL"
            return

L20600: REM Def/Enable Paper Copy of Purges?       POCOPY$
            pocopy$ = "Y"
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
         "Enter Beginning and Ending Vendor Code or 'ALL'              ",~
         "Enter Beginning and Ending Store Code or 'ALL'               ",~
         "Enter Beginning and Ending Purchase Order # or 'ALL'         ",~
         "Enter Paper Copy of Purges (Y/N)                             "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      purgedate$, pocopy$, startvend$, startpo$,         ~
                      startstore$, endvend$, part$, endpo$, endstore$,   ~
                      vendor$, po$, store$, orderdate$, duedate$
            rcvdqty, orderqty, price = 0
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
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
        get_master

L30060:     call "READNXT1" (#2, f1%(2))
            if f1%(2) = 0% then return
                get #2, using L30090, vendor$, po$, vendname$, textid$,   ~
                                     store$, orderdate$, duedate$
L30090:             FMT CH(9), CH(16), CH(30), POS(562), CH(4), POS(477),~
                        CH(3), POS(451), CH(6), POS(471), CH(6)
            readkey$ = str(vendor$) & po$

                if orderdate$ > purgedate$ then L30060

                if startvend$ = "ALL" then L30150
                if vendor$ < svend$ or vendor$ > evend$ then L30060

L30150:         if startstore$ = "ALL" then L30180
                if store$ < sstore$ or store$ > estore$ then L30060

L30180:         if startpo$ = "ALL" then L30280
                if po$ < spo$ or po$ > epo$ then L30060

L30280:     get #2 using L30290, masterkey$
L30290:          FMT CH(25)
            flag% = 1%
            call "DATEFMT" (orderdate$)
            call "DATEFMT" (duedate$)
            if pocopy$ = "N" then return
                print : line% = line% + 1%
                return

        REM *************************************************************~
            *           P U R G E     L I N E S                         *~
            *************************************************************
        purge_lines

            plowkey$ = str(readkey$,,25) & hex(00)
L30390:     call "PLOWNXT1" (#3, plowkey$, 25%, f1%(3))
            if f1%(3) = 0% then return

             get #3 using L30493, part$, orderqty, rcvdqty, price, textid1$
L30493:           FMT POS(32), CH(25), POS(93), PD(14,4), PD(14,4),      ~
                      POS(117), PD(14,4), POS(306), CH(4)

              gosub print_po

              delete #3

              call "TXTFUTIL" (#5, f2%(5), "DELE", textid1$)

                 goto L30390

        REM *************************************************************~
            *           P U R G E     S H A D O W                       *~
            *************************************************************
        purge_shadow

            plowkey$ = str(readkey$,,25) & hex(00)
L31060:     call "PLOWNXT1" (#4, plowkey$, 25%, f1%(4))
            if f1%(4) = 0% then return

              call "DELETE" (#4, plowkey$, 28%)
                goto L31060

        REM *************************************************************~
            *           P U R G E     M A S T E R                       *~
            *************************************************************
        purge_master

              call "DELETE" (#2, masterkey$, 25%)
              call "DELETE" (#6, masterkey$, 25%)
              call "DELETE" (#7, masterkey$, 25%)

              call "TXTFUTIL" (#5, f2%(5), "DELE", textid$)
            return

        REM *************************************************************~
            *     P R I N T     P U R C H A S E    O R D E R            *~
            *************************************************************
        print_po

            if pocopy$ = "N" then return

            if flag% = 0% then L32300
            gosub check_page
            print using L33020, vendor$, po$, vendname$, store$,          ~
                               orderdate$, duedate$, part$, orderqty,    ~
                               rcvdqty, price
            flag% = 0% : line% = line% + 1%
            return

L32300:     gosub check_page
            print using L33050, part$, orderqty, rcvdqty, price
            line% = line% + 1%
            return


        check_page
            if line% <= 56% then return
            page% = page% + 1% : line% = 6%
            print page
            print using L33090, date$, company$
            print using L33120, page%
            print
            print
            print using L33150
            print using L33180
            return


        REM PRINT FORMATS

L33020: %######### ################ ############################## ### ##~
        ~###### ######## ######################### ######## ######## -####~
        ~.##
L33050: %                                                                ~
        ~                ######################### ######## ######## #####~
        ~.##

L33090: %RUN DATE: ########                      ########################~
        ~####################################                VBKHPRGE:VBK0~
        ~07
L33120: %                                              PURCHASE ORDER HIS~
        ~TORY PURGE REPORT                                         PAGE: #~
        ~##
L33150: %Vendor    Purchase Order   Vendor Name                    Str Or~
        ~d Date Due Date Part Number               Qty Ordr Qty Ship    Pr~
        ~ice
L33180: %--------- ---------------- ------------------------------ --- --~
        ~------ -------- ------------------------- -------- -------- -----~
        ~---

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
              on fieldnr% gosub L40100,         /* Purge-to Date     */   ~
                                L40100,         /* Vendor Range      */   ~
                                L40100,         /* Store Range       */   ~
                                L40100,         /* Purch Order Range */   ~
                                L40100          /* Paper Copy?       */
              goto L40115

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40100:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40115:     accept                                                       ~
               at (01,02),                                               ~
                  "Purge Purchase Order History Files",                  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), fac(hex(ac)), titl$(1)               , ch(17),~
               at (06,22), fac(hex(ac)), titl$(2)               , ch(25),~
               at (06,50), fac(hex(ac)), titl$(3)               , ch(25),~
                                                                         ~
               at (07,02), "Purge-to Date",                              ~
               at (07,50), fac(lfac$( 1)), purgedate$           , ch(10),~
                                                                         ~
               at (08,02), "Vendor Range",                               ~
               at (08,30), fac(lfac$( 2)), startvend$           , ch(09),~
               at (08,50), fac(lfac$( 2)), endvend$             , ch(09),~
                                                                         ~
               at (09,02), "Store Range",                                ~
               at (09,30), fac(lfac$( 3)), startstore$          , ch(03),~
               at (09,50), fac(lfac$( 3)), endstore$            , ch(03),~
                                                                         ~
               at (10,02), "Purchase Order Range",                       ~
               at (10,30), fac(lfac$( 4)), startpo$             , ch(16),~
               at (10,50), fac(lfac$( 4)), endpo$               , ch(16),~
                                                                         ~
               at (11,02), "Paper Copy of Purges?",                      ~
               at (11,30), fac(lfac$( 5)), pocopy$              , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40300
                  call "MANUAL" ("VBKHPRGE") : goto L40115

L40300:        if keyhit% <> 15 then L40315
                  call "PRNTSCRN" : goto L40115

L40315:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40410     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous            " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40390
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40390:     if fieldnr% = 2% or fieldnr% > 2% then L40400
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40400:     return

L40410: if fieldnr% > 0% then L40455  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Purge Data  "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40455:                              /*  Edit Mode - Enabled    */
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


        REM *************************************************************~
            *           W R I T E   T O   S Y S 2 - V B K               *~
            *-----------------------------------------------------------*~
            *   Writes parameters to SYS2-VBK for next purge & restart  *~
            *************************************************************

            call "READ101" (#1, "VBK.PURGE.CTL", f1%(1))
                if f1%(1) = 0 then write_new
                    call "DATUFMTC" (purgedate$)
              put #1, using L49380, userid$, purgedate$, svend$, evend$,  ~
                                   sstore$, estore$, spo$, epo$, " ",    ~
                                   " ", pocopy$

L49380:       FMT POS(21), CH(3), POS(34), CH(6), CH(9), CH(9), CH(3),   ~
                    CH(3), CH(16), CH(16), CH(4), CH(4), CH(1)
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
                              L50200,         /* Vendor Range           */~
                              L50300,         /* Store Range            */~
                              L50400,         /* Purch Order Range      */~
                              L50600          /* Paper Copy?            */
            return
L50100: REM Test for Purge-to Date                PURGEDATE$
                call "DATEOKC" (purgedate$, date%, errormsg$)
                if errormsg$ <> " "      then return
                if date%     <= mindate% then return
                     errormsg$ = "Date for Purge Must Be ON or BEFORE " &~
                                 mindate$ & "! "
            return

L50200: REM Test for Vendor Range                 STARTVEND$
            call "TESTRNGE" (startvend$, endvend$, svend$, evend$,       ~
                             errormsg$)
            return

L50300: REM Test for Store Range                  STARTSTORE$
            call "TESTRNGE" (startstore$, endstore$, sstore$, estore$,   ~
                             errormsg$)
            return

L50400: REM Test for Purchase Order Range         STARTPO$
            call "TESTRNGE" (startpo$, endpo$, spo$, epo$,               ~
                             errormsg$)
            return

L50600: REM Test for Paper Copy of Purges?        POCOPY$
            if pocopy$ = "Y" or pocopy$ = "N" then return
            errormsg$ = "'Y' for Yes;     'N' for No."
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
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "READ101" (#1, "VBK.PURGE.CTL", f1%(1))
                if f1%(1) = 0% then L65210
            put #1, using L65180, " "
L65180:     FMT POS(21), CH(3)
            rewrite #1

            if pocopy$ = "N" then L65210
                if page% = 0% then L65196
                    print
                    print "** END OF REPORT **"
L65196:         call "SETPRNT" ("VBK007", " ", 0%, 1%)
                close printer

L65210:     end
