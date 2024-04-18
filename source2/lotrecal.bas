        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  L       OOO   TTTTT  RRRR   EEEEE   CCC    AAA   L       *~
            *  L      O   O    T    R   R  E      C   C  A   A  L       *~
            *  L      O   O    T    RRRR   EEEE   C      AAAAA  L       *~
            *  L      O   O    T    R   R  E      C   C  A   A  L       *~
            *  LLLLL   OOO     T    R   R  EEEEE   CCC   A   A  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * LOTRECAL - This program will trace a Part/Lot's movemenets*~
            *            to the ShipTo customer and write a report or   *~
            *            write information to a user welected File.  Up *~
            *            to 100 Parts of 100 Lots each may be reported. *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/17/93 ! Original                                 ! RJ1 *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            address$(6)30,               /* Ship To Address            */~
            arcyear$7,                   /* Archive Year Literal       */~
            blankline$79,                /* Line for input screen      */~
            choice$4,                    /* PICKYEAR return choice     */~
            company$60,                  /* Company Name               */~
            component$44,                /* Component for Explosion    */~
            cursor%(2),                  /* Cursor location for edit   */~
            customer$9,                  /* Customer Code              */~
            cus_prev$9,                  /* Customer Code Previous     */~
            date$8,                      /* Date for screen display    */~
            descr$(100)32,               /* Part Description           */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fileid$4,                    /* File ID for PICKYEAR       */~
            getback$9,                   /* (1)Return on Filename Scrn.*/~
            hdr_scrn_2$79,               /* Screen 2 Part Header       */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            invoice$8,                   /* Invoice Number             */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lot$(100,100)6,              /* Lot                        */~
            lot$6,                       /* Lot                        */~
            lotmprname$8,                /* Current LOTMVMNT File Name */~
            outfile$8,                   /* Output File Name           */~
            part$(100)25,                /* Part Number                */~
            part$25,                     /* Part Number                */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            pick$(100)1,                 /* Summary Screen Pick        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            plowkey1$99,                 /* Miscellaneous Read/Plow Key*/~
            po_number$16,                /* Purchase Order Number      */~
            p%(30),                      /* Dup Counter                */~
            p$(30)88,                    /* Part Number to Track       */~
            qty$10,                      /* Quantity moved             */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            report_type$1,               /* Report to Printer or File  */~
            sales_order$16,              /* Sales Order Number         */~
            seqnr$3,                     /* Part # array position      */~
            sort_msg$31,                 /* Screen Sort Message        */~
            sort_type$1,                 /* Report Sort Type           */~
            store$3,                     /* Store                      */~
            telephone$12,                /* Telephone Number           */~
            tele_prev$12,                /* Telephone Number Previous  */~
            time$8,                      /* Time Stamp                 */~
            trandate$8,                  /* Transaction Date           */~
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
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! HNYMASTR ! Inventory Master File                    *~
            * #03 ! LOTMVMNT ! LOT MOVEMENT FILE                        *~
            * #04 ! ARIMASTR ! Invoice Master File                      *~
            * #05 ! BCKMASTR ! S.O. Header Master File                  *~
            * #06 ! CUSTOMER ! Customer Master File                     *~
            * #07 ! XXXXXXXX ! Report Output File                       *~
            * #50 ! WORKFILE ! Report Print Workfile                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #02, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  3, keypos =   26, keylen =  32, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  1, keypos =  102, keylen =   9, dup     ~

            select #03, "LOTMVMNT",                                      ~
                        varc,     indexed,  recsize =  160,              ~
                        keypos =    1, keylen =  88,                     ~
                        alt key  1, keypos =   45, keylen =  88          ~

            select #04, "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17,                     ~
                        alt key  4, keypos = 1783, keylen =  26,         ~
                            key  3, keypos =   34, keylen =  16, dup,    ~
                            key  2, keypos =   18, keylen =  16, dup,    ~
                            key  1, keypos =   10, keylen =   8, dup     ~

          /* Available for Extracting Custom Phone Number */
*          SELECT #05, "BCKMASTR",                                      ~
*                      VARC,     INDEXED,  RECSIZE = 1000,              ~
*                      KEYPOS =    1, KEYLEN =  25,                     ~
*                      ALT KEY  1, KEYPOS =   26, KEYLEN =  16, DUP     ~

            select #06, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  5, keypos = 1049, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  1, keypos =   10, keylen =  30, dup     ~

            select #07, "LOTEPORT",                                      ~
                        varc,     indexed,  recsize =  350,              ~
                        keypos =    1, keylen =  59

            select #50, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =  350,              ~
                        keypos =    1, keylen =  59       ,              ~
                        alt key  1, keypos = 35, keylen = 9, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
*          CALL "OPENCHCK" (#05, FS%(05), F2%(05), 0%, RSLT$(05))
            call "OPENCHCK" (#06, fs%(06), f2%(06), 0%, rslt$(06))
            call "WORKOPEN" (#50, "IO", 100%, f2%(50))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            call "COMPNAME" (12%, company$, u3%)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62%) = "LOTRECAL: " & str(cms2v$,,8%)
            hdr_scrn_2$     = "Part Number                 Description"
            if arcyear$     = " " then arcyear$ = "Current"
            outfile$        = "LOTEPORT"
            sort_msg$       = "Print Report Sorted by LOT"
            seqnr%, max_seqnr% = 0%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

L10060:     gosub set_next_part
            gosub initialize_variables

        inputmode

            for fieldnr% = 1% to  2%
                gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10220
L10140:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  2% then       partover
                      if keyhit%  =  9% then       summary_scrn
                      if keyhit%  = 10% then gosub select_archive_year
                      if keyhit% <> 16% or fieldnr% <> 1% then L10210
                          goto exit_program
L10210:               if keyhit% <> 0% then        L10140
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then     L10140
            next fieldnr%

            goto editpg1

        set_next_part
            if part$(max_seqnr%) = " " and seqnr% <> 0%                  ~
                                       then L10360  /* Last OK for Data */
            max_seqnr% = max_seqnr% + 1%
L10360:     seqnr%     = max_seqnr%
            return

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
L11080:     gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       partover
                  if keyhit%  =  4% then gosub do_prev_part
                  if keyhit%  =  5% then gosub do_next_part
*                IF KEYHIT%  =  9% THEN       SUMMARY_SCRN
                  if keyhit%  = 10% then gosub select_archive_year
                  if keyhit%  = 11% then       append_part
                  if keyhit%  = 12% then       delete_part
                  if keyhit%  = 16% then       summary_scrn
                  if keyhit% <>  0% then       editpg1
L11170:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% > 2% then fieldnr% = 2%
            if fieldnr% < 1% or fieldnr% >  2% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11230:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11230
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11230
                  lastfieldnr% = fieldnr%
            goto L11170


        do_prev_part
            if seqnr% > 1% then seqnr% = seqnr% - 1%
            return

        do_next_part
            if seqnr% <  max_seqnr% then  seqnr% = seqnr% + 1%
            return

        append_part
            gosub set_next_part
            goto inputmode

        delete_part
L11450:     ask% = 2%
            call "ASKUSER" (ask%, "** DELETE PART **",                   ~
                                  "Are You Sure",                        ~
                                  "Press PF (1) to Return to Display",   ~
                                  "Press PF(12) to Delete Part " )

            if ask% =   1% then L11660
            if ask% <> 12% then L11450

            part$(seqnr%)    = " "
            descr$(seqnr%)   = " "
            call "LINSMASH" (part$())
            call "LINSMASH" (descr$())
            for j% = 1% to 100% : lot$(seqnr%, j%) = " " : next j%
            for i% = seqnr% to max_seqnr%
                for j% = 1% to 100%
                    lot$(i%, j%) = lot$(i% + 1%, j%)
                next j%
            next i%
            if max_seqnr% > 1% then max_seqnr% = max_seqnr% - 1%
            if seqnr% > max_seqnr% then seqnr% = max_seqnr%
L11660:     if part$(seqnr%) = " " then inputmode else editpg1

        summary_scrn
            if part$(seqnr%) <> " " then L11710
                descr$(seqnr%), errormsg$ = " "
L11710:     fieldnr% = 1%
                gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L11880
L11740:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  4% then gosub do_prev_page
                      if keyhit%  =  5% then gosub do_next_page
                      if keyhit%  =  9% then gosub toggle_report
                      if keyhit%  = 10% then gosub select_archive_year
                      if keyhit% <> 0% then       L11860
                          part_pos% = cursor%(1%) - 5%
                          if lfac$(part_pos%) = hex(81) then L11840
                              errormsg$ = "Cursor Not on a Part."
                              goto L11740
L11840:                   seqnr% = part_pos% + dln%
                          goto editpg1
L11860:               if keyhit%  = 11% then append_part
                      if keyhit% = 14% then run_report
L11880:               if keyhit% = 30% then run_report
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
                      goto L11740

        do_next_page
            if dln% + 15%  > max_seqnr% then return
                scr% = scr% + 1%
                dln% = scr% * 14%
            return

        do_prev_page
            if scr%  < 1% then return
                scr% = scr% - 1%
                dln% = scr% * 14%

            return

        toggle_report
            if sort_type$ = "C" then L12100
                sort_type$ = "C"
                str(sort_msg$, 24%, 8%) = "CUSTOMER"
                return
L12100:     sort_type$ = "L"
            str(sort_msg$, 24%, 8%) = "LOT"
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Part Number            */~
                              L20200          /* Lot                    */
            return
L20100: REM Def/Enable Part Number                 PART$
            return

L20200: REM Def/Enable Lot                         LOT$
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L21100          /* Part Number            */
            return
L21100: REM Def/Enable Part Number                 PART2$
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
            if scrnr% = 2% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Part Number.                                           ",~
         "Enter Lots.                                                  "

        scrn2_msg  :  data                                               ~
         "Place Cursor on desired Part Number and Press Return.        "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      lot$(), part$(), descr$()
            something_done%, tothit%, scr%, dln% = 0%
            max_seqnr%, seqnr% = 1%
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
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
            goto L10060

        partover
L29710:     ask% = 2%
            call "ASKUSER" (ask%, "** RESTART PART **",                  ~
                                  "Press PF (1) to Return to Display",   ~
                                  "- OR -" ,                             ~
                                  "Press RETURN to do Part Over without" ~
                                & " Saving the Current Entry" )
            if ask% =  0% then L29755
            if ask% <> 1% then L29710
                if edit% = 1% then L10140 else L11080 /* Input or Edit */
L29755:     errormsg$      = " "
            part$(seqnr%)  = " "
            descr$(seqnr%) = " "
            for j% = 1% to 100% : lot$(seqnr%, j%) = " " : next j%
            goto inputmode

*       ASK_TO_EXIT
*          IF SOMETHING_DONE% = 1% THEN RETURN
*          ASK% = 2%
*          CALL "ASKUSER" (ASK%, "** EXIT PROGRAM **",                  ~
*                                "There exists multiple Parts to "      ~
*                              & "Report upon" ,                        ~
*                                "Press PF (1) to Return to Display",   ~
*                                "Press PF(16) to Exit Program" )
*
*          IF ASK% =   1% THEN RETURN
*          IF ASK% <> 16% THEN 29810
*          GOTO EXIT_PROGRAM

        report_nothing_done
            ask% = 2%
            call "ASKUSER" (ask%, "** NOTHING TO REPORT **",             ~
                                  " ",                                   ~
                                  "There were NO Customers for your "    ~
                                & "Selection Criteria",                  ~
                                  "Press Any Key to Continue" )
            if report_type$ = "F" then call "FILEBGON" (#07)

            return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        run_report
            something_done% = 1%
            if keyhit% <> 14% then report_type$ = "F"                    ~
                              else report_type$ = "P"
            if report_type$ = "P"                                        ~
                then  gosub set_up_printer                               ~
                else  gosub set_up_file

            l%, page%, path% = 0%
            p$() = " "
            mat p% = zer

            for seqnr% = 1% to max_seqnr%
                for j% = 1% to 100%
                    hit% = 0%
                    if lot$(seqnr%, j%) = " " then L30320     /* Next J */
                    plowkey$ = "H" & str(part$(seqnr%))    &             ~
                                     str(lot$(seqnr%, j%)) & hex(00)
L30190:             call "PLOWNEXT" (#03, plowkey$, 32%, f1%(3%))
                       if f1%(3%) <> 0% then L30250
                          if hit% <> 0% then L30320
                          gosub report_no_results
                          goto L30320                         /* Next J */

L30250:             /* ** Follow this Part/Lot to it's Customers ** */
                    /* ** and Report the results ** */
                    from_loop% = 1%
                    hit% = hit% + 1%  : tothit% = tothit% + 1%
                    gosub'9(plowkey$)
                    str(plowkey$,45%,1%) = hex(ff)
                    goto L30190                /* Plow for Next Store */
L30320:         next j%
            next seqnr%

            if report_type$ = "P"                                        ~
                 then   gosub finish_printing                            ~
                 else   gosub close_up_file

            if tothit% = 0% then gosub report_nothing_done
            tothit% = 0%
            goto summary_scrn

            /* ** Trace Part/Lot's Movements to Customer ** */

        deffn'9(plowkey1$)
                  l% = l% + 1
                  p$(l%) = str(plowkey1$,,44)
                  if l% = 1% then L30530
                     search str(p$(),1%,88%*(l%-1%)) = str(p$(l%),1%,44%)~
                            to f1%() step 88
                     if f1%(1%) = 0% then L30530        /* NO LOOP HERE */
                        gosub report_redundancy
                        goto end_present_level

L30530:      /*  Read the Selected Keys */
L30540:         call "PLOWALTS" (#03, p$(l%), path%, 44%, f1%(3%))
                if f1%(3%) = 0% then end_present_level

                  component$ = str(p$(l%),45)
                  if str(p$(l%),,1)  = "C" then L30610
                  if str(p$(l%),,1) <> "D" then L30660

L30610:           if l% = 1% then L30640
                    if str(component$,2,25) = str(p$(l%-1%),2,25)        ~
                                                 then L30670 else L30540
L30640:             if str(component$,2,25) <> part$(seqnr%) then L30540

L30660:           if str(component$,1%,1%) <> "C"  then L30730
L30670:               get #03, using L30680, qty, trandate$
L30680:                   FMT POS(133), PD(14,4), POS(147), CH(6)
                      call "DATEFMT" (trandate$)
                      call "CONVERT" (qty, 0.2, qty$)
                      gosub report_customer /*Valid Cust, so lets Do It */

L30730:           p%(l%) = p%(l%) + 1
                  if l% < 30% then gosub'9(component$) /* Recursive !! */
                                         /* DO COMPS IF NOT AT TOP     */
                  goto L30530

        end_present_level         /* End this level Gracefully. If 1st */
                  p%(l%) = 0      /*  level returns from subroutine or */
                  l% = l% - 1     /*  returns from recursive DEFFN'9.  */
                  return

        REM *************************************************************~
            * P R I N T   O R   S T U F F   D A T A   I N T O   F I L E *~
            *-----------------------------------------------------------*~
            * Outputs to printer or file Customer data                  *~
            *************************************************************

        report_customer

            address$(), telephone$, invoice$, po_number$, customer$,     ~
                sales_order$ = " "
            gosub get_ari_data  /* Get PO, Sale Order, & Address */
*          GOSUB GET_BCK_DATA  /* Get Special Telephone         */
            gosub get_cus_data  /* Get Regular Telephone         */

            if report_type$ = "P" then L31155
                gosub file_report
                return
L31155:     if sort_type$ = "L" then gosub print_report_by_lot           ~
                                else gosub workfile_for_print_by_cus
            return

        print_report_by_lot
            if tothit% < 2% then call "SHOSTAT" ("Printing Report")
            line% = line% + 1%
            if line% > 56% then gosub print_header
            if from_loop% = 1% then cus_prev$, tele_prev$  = " "
            gosub print_part_header
            if cus_prev$ = customer$ then L31380  /* Don't do Address */
            tele_prev$ = telephone$
            print using L39750, customer$, invoice$, address$(1%),        ~
                               telephone$, sales_order$, po_number$,     ~
                               qty$, trandate$

            for address% = 2% to 6%
                if address$(address%) = " " then L31350
                     print using L39780, address$(address%)
                     line% = line% + 1%
L31350:         next address%
            goto  L31410

L31380:     if tele_prev$ = telephone$ then telephone$ = " "             ~
                                       else tele_prev$ = telephone$
            print using L39750, " "      , invoice$, " "         ,        ~
                               telephone$, sales_order$, po_number$,     ~
                               qty$, trandate$
L31410:     cus_prev$ = customer$
            return

        print_header
                print page
                page% = page% + 1%
                print using L39520, date$, time$, company$
                print using L39590, page%
                print using L39560, arcyear$
                line% = 3%
            return

        print_part_header
            if line% > 48% then gosub print_header  /* New Page */
            if from_loop% = 0%  then L31610
                print
                print using L39630, part$(seqnr%), lot$(seqnr%, j%),      ~
                                                     str(plowkey$,33%,3%)
                goto L31650

L31610:     if line% > 3% then  return
                print
                print using L39660, part$(seqnr%), lot$(seqnr%, j%),      ~
                                                     str(plowkey$,33%,3%)
L31650:         print using L39690
                print using L39720
                line% = line% + 4%
                from_loop% = 0%
            return

        file_report
L31720:     time$ = time
            put #7 using L31810, part$(seqnr%),lot$(seqnr%,j%),           ~
                            str(plowkey$,33%,3), customer$, invoice$,    ~
                            time$, address$(), telephone$, sales_order$, ~
                            po_number$, qty$, trandate$, " "

            write #7, eod  goto L31720
            return

L31810: FMT                    /* Output Report File */                  ~
                CH(25),                /* Part Number                  */~
                CH( 6),                /* Lot                          */~
                CH( 3),                /* Store                        */~
                CH( 9),                /* Customer Code                */~
                CH( 8),                /* Invoice Number               */~
                CH( 8),                /* Time Stamp                   */~
              6*CH(30),                /* Address (if <> ARI then CUS) */~
                CH(12),                /* Telephone                    */~
                CH(16),                /* Sales Order Number           */~
                CH(16),                /* PO Number                    */~
                CH(10),                /* Quantity Shipped             */~
                CH( 8),                /* Transaction Date             */~
                CH(49)                 /* Filler                       */

        workfile_for_print_by_cus
L31955:     if tothit% < 2% then call "SHOSTAT" ("Writing to Workfile")
            put #50 using L31810, part$(seqnr%),lot$(seqnr%,j%),          ~
                            str(plowkey$,33%,3), customer$, invoice$,    ~
                            time$, address$(), telephone$, sales_order$, ~
                            po_number$, qty$, trandate$, " "

            write #50, eod  goto L31955
            return

        get_ari_data
            customer$ = str(component$,02%,9%)
            invoice$ = str(component$,11%,8%)

            readkey$ = str(customer$) & invoice$
            call "READ100" (#04, readkey$, f1%(4%))
                if f1%(4%) <> 0% then L32090
*                   PO_NUMBER$ = "NO PO ON RECORD"
                     return
L32090:     get #04 using L32120, po_number$, sales_order$, address$()
            if address$() <> " " then return
                get #04 using L32130, address$() /* Try for Sold to */


L32120:     FMT POS(18), CH(16), CH(16), POS(53), 6*CH(30)  /* Ship to */
L32130:     FMT POS(233), 6*CH(30)                          /* Sold to */

*       GET_BCK_DATA   /* Get Special Phone from variable field in    */
*                      /*  BCKMASTR File.  Custom Area for some Users.*/
*          READKEY$ = CUSTOMER$ & SALES_ORDER$
*          CALL "READ100" (#05, READKEY$, F1%(5%))
*              IF F1%(5%) <> 0% THEN 32220
*                   TELEPHONE$ = "NO SPECIAL PHONE"
*                   RETURN
*          GET #05 USING 32260, TELEPHONE$
*          GOSUB SET_TELEPHONE
*          RETURN
*
*          FMT POS(599), CH(12)

        get_cus_data      /* Get Regular Phone */
            readkey$ = customer$
            call "READ100" (#06, readkey$, f1%(6%))
                if f1%(6%) <> 0% then L32350
                     address$(1%) = "NO ADDRESS ON FILE"
                     return
L32350:     get #06 using L32410, telephone$
            gosub set_telephone
            if address$() <> " " then return
                get #06 using L32420, address$()
            return

L32410:     FMT POS(453), CH(10)         /* Phone # */
L32420:     FMT POS(253), 6*CH(30)       /* Ship to */

        set_telephone
            if len(telephone$) <> 10% then L32530
                str(temp$,9%,4%) = str(telephone$,7%,4%)
                str(temp$,8%,1%) = "-"
                str(temp$,5%,3%) = str(telephone$,4%,3%)
                str(temp$,4%,1%) = "-"
                str(temp$,1%,3%) = str(telephone$,1%,3%)
                telephone$       = temp$
                return
L32530:     if len(telephone$) <>  7% then return
                str(temp$,5%,4%) = str(telephone$,4%,4%)
                str(temp$,4%,1%) = "-"
                str(temp$,1%,3%) = str(telephone$,1%,3%)
                telephone$       = temp$
            return

        report_redundancy
           /* No Report or Redundance at this time */

        return

        report_no_results
            if report_type$ = "F" then return
            if sort_type$   = "C" then return
            from_loop% = 1%
            gosub print_part_header
            print "NO CUSTOMERS FOR THIS PART/LOT "
            line% = line% + 1%
            return

        set_up_printer
            time$ = " " : call "TIME" (time$)
            select printer(134)
            call "SETPRNT" ("HNY057", " ", 0%, 0%)
            line% = 1000%
            path% = 0%
            return

        set_up_file
L32920:     gosub set_filename_screen
                if keyhit%  = 0% then L32970
                if keyhit% <> 1% then L32920
                    errormsg$ = " "
                    goto summary_scrn
L32970:     errormsg$ = " "
            call "SHOSTAT" ("Creating and Opening Report File")
            call "PUTPRNAM" addr(#7, outfile$)
          /* Does File already exist ?? */
            call "OPENCHCK"(#7, fs%(7%), f2%(7%), 0%  , rslt$(7%))
                 if f2%(7%) = 0% then L33080
            call "OPENCHCK"(#7, fs%(7%), f2%(7%), 100%, rslt$(7%))
                 if f2%(7%) = 0% then return
            errormsg$ = "Unable to Open File. Please Try Again."
            goto L32920

L33080:     errormsg$ = "File Already Exists. Please Reenter."
            close #07
            goto L32920

        close_up_printer
            if page% > 0% then L33150
                goto L33180              /* Nothing done */
L33150:     time$ = " "  :  call "TIME" (time$)
            print skip(2)
            print using L39800, time$        /* End of report line */
L33180:     close printer
            call "SETPRNT" ("HNY057", " ", 0%, 1%)
            return                       /* ALL DONE */

        close_up_file
            close #7
            return

        finish_printing
            if sort_type$ = "L" then goto close_up_printer
            /* Print Report Sorted by Customer in Workfile */
            call "SHOSTAT" ("Printing Report")
            hit%, page%  = 0%
            line% = 99%
            cus_prev$, tele_prev$ = " "
            plowkey$ = all(hex(00))
            call "REDALT4" (#50, plowkey$, 1%, f1%(50%))
                goto L33340
        work_loop
            call "READNEXT" (#50, f1%(50%))
L33340:        if f1%(50%) = 0% then goto close_up_printer
            hit% = 1%
            get #50 using L31810, part$,lot$, store$, customer$, invoice$,~
                            time$, address$(), telephone$, sales_order$, ~
                            po_number$, qty$, trandate$

            line% = line% + 2%
            if line% < 57% then L33440
                gosub print_header
                gosub print_customer_header
L33440:     if cus_prev$ = customer$ then L33570  /* Don't do Address */
*          TELE_PREV$ = TELEPHONE$
            print
            print using L39830, customer$, address$(1%)
            for address% = 2% to 6%
                if address$(address%) = " " then L33540
                     print using L39780, address$(address%)
                     line% = line% + 1%
L33540:         next address%

L33570:     if tele_prev$ = telephone$ then telephone$ = " "             ~
                                       else tele_prev$ = telephone$
            print using L39850, part$, store$, lot$, invoice$, qty$,      ~
                               telephone$, sales_order$, po_number$,     ~
                               trandate$
            cus_prev$ = customer$
            goto work_loop

        print_customer_header
                print
                print using L39880
                print using L39910
            line% = line% + 3%
            return

*                  **** Archive Year Selection **** *

        select_archive_year
            fileid$ = "LOTM"
            call "PICKYEAR" (fileid$, choice$)
              if f2%(3%) = 0% then close #03/* Close Curr. Detail File */
              if choice$ = "CURR" or choice$ = " " then L39070 else L39090
L39070:       lotmprname$ = "LOTMVMNT"
                  goto L39110
L39090:       lotmprname$ = "LOTM" & choice$
L39110:       call "PUTPRNAM" addr(#03, lotmprname$)
              call "OPENCHCK" (#03, fs%(3%), f2%(3%), 0%, rslt$(3%))
              if choice$ = "CURR" or choice$ = " "                       ~
               then arcyear$ = "Current" else arcyear$ = choice$ & "   "
              keyhit% = 10%        /* Belt and Galisters */
            return

*                  **** Print Image Section **** *

L39520: %RUN DATE: ######## @ ########       ############################~
        ~################################                     LOTRECAL-HNY~
        ~057

L39560: %                                                       ARCHIVE Y~
        ~EAR: #######

L39590: %                                      C U S T O M E R    S H I P~
        ~ M E N T    R E P O R T                                    PAGE #~
        ~###

L39630: %PART: #########################  LOT: ######  STORE: ###        ~
        ~                                       NET          SHIP

L39660: % - CONTINUED - PART: #########################  LOT: ######  STO~
        ~RE: ###                                NET          SHIP

L39690: %CUSTOMER   INVOICE   ADDRESS                         TELEPHONE  ~
        ~   SALES ORDER       PO NUMBER         QUANTITY     DATE

L39720: %---------  --------  ------------------------------  -----------~
        ~-  ----------------  ----------------  ----------   --------

L39750: %#########  ########  ##############################  ###########~
        ~#  ################  ################  ##########   ########

L39780: %                     ##############################

L39800:         %                          * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   @   ########   * * * * * * * * * *

L39830: %CUSTOMER: #########  ##############################

L39850: %  #########################  ###  ######  ########  ##########  ~
        ~############  ################  ################  ########

L39880: %  PART                       STR  LOT     INVOICE   QUANTITY    ~
        ~TELEPHONE     SALES ORDER       PO NUMBER         DATE

L39910: %  -------------------------  ---  ------  --------  ----------  ~
        ~------------  ----------------  ----------------  --------

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              convert seqnr% to seqnr$, pic(000)
              str(line2$,20%, 32%) = "Add/Edit Part     Sequence: "   &  ~
                                                                   seqnr$
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40095,         /* Part Number       */   ~
                                L40095          /* Lot               */
              goto L40200

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40095:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
               at (01,02),                                               ~
                  "Recall List Report",                                  ~
               at (01,40), "Archive Year:",fac(hex(84)),arcyear$, ch(07),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Part Number",                                ~
               at (06,15), fac(lfac$( 1%)), part$(seqnr%)       , ch(25),~
               at (06,41), "Description",                                ~
               at (06,55), fac(hex(8c))   , descr$(seqnr%)      , ch(24),~
                                                                         ~
               at (07,02), "Lots:",                                      ~
               at (08,02), fac(lfac$( 2%)), lot$(seqnr%,  1%)   , ch(06),~
               at (08,10), fac(lfac$( 2%)), lot$(seqnr%,  2%)   , ch(06),~
               at (08,18), fac(lfac$( 2%)), lot$(seqnr%,  3%)   , ch(06),~
               at (08,26), fac(lfac$( 2%)), lot$(seqnr%,  4%)   , ch(06),~
               at (08,34), fac(lfac$( 2%)), lot$(seqnr%,  5%)   , ch(06),~
               at (08,42), fac(lfac$( 2%)), lot$(seqnr%,  6%)   , ch(06),~
               at (08,50), fac(lfac$( 2%)), lot$(seqnr%,  7%)   , ch(06),~
               at (08,58), fac(lfac$( 2%)), lot$(seqnr%,  8%)   , ch(06),~
               at (08,66), fac(lfac$( 2%)), lot$(seqnr%,  9%)   , ch(06),~
               at (08,74), fac(lfac$( 2%)), lot$(seqnr%, 10%)   , ch(06),~
                                                                         ~
               at (09,02), fac(lfac$( 2%)), lot$(seqnr%, 11%)   , ch(06),~
               at (09,10), fac(lfac$( 2%)), lot$(seqnr%, 12%)   , ch(06),~
               at (09,18), fac(lfac$( 2%)), lot$(seqnr%, 13%)   , ch(06),~
               at (09,26), fac(lfac$( 2%)), lot$(seqnr%, 14%)   , ch(06),~
               at (09,34), fac(lfac$( 2%)), lot$(seqnr%, 15%)   , ch(06),~
               at (09,42), fac(lfac$( 2%)), lot$(seqnr%, 16%)   , ch(06),~
               at (09,50), fac(lfac$( 2%)), lot$(seqnr%, 17%)   , ch(06),~
               at (09,58), fac(lfac$( 2%)), lot$(seqnr%, 18%)   , ch(06),~
               at (09,66), fac(lfac$( 2%)), lot$(seqnr%, 19%)   , ch(06),~
               at (09,74), fac(lfac$( 2%)), lot$(seqnr%, 20%)   , ch(06),~
                                                                         ~
               at (10,02), fac(lfac$( 2%)), lot$(seqnr%, 21%)   , ch(06),~
               at (10,10), fac(lfac$( 2%)), lot$(seqnr%, 22%)   , ch(06),~
               at (10,18), fac(lfac$( 2%)), lot$(seqnr%, 23%)   , ch(06),~
               at (10,26), fac(lfac$( 2%)), lot$(seqnr%, 24%)   , ch(06),~
               at (10,34), fac(lfac$( 2%)), lot$(seqnr%, 25%)   , ch(06),~
               at (10,42), fac(lfac$( 2%)), lot$(seqnr%, 26%)   , ch(06),~
               at (10,50), fac(lfac$( 2%)), lot$(seqnr%, 27%)   , ch(06),~
               at (10,58), fac(lfac$( 2%)), lot$(seqnr%, 28%)   , ch(06),~
               at (10,66), fac(lfac$( 2%)), lot$(seqnr%, 29%)   , ch(06),~
               at (10,74), fac(lfac$( 2%)), lot$(seqnr%, 30%)   , ch(06),~
                                                                         ~
               at (11,02), fac(lfac$( 2%)), lot$(seqnr%, 31%)   , ch(06),~
               at (11,10), fac(lfac$( 2%)), lot$(seqnr%, 32%)   , ch(06),~
               at (11,18), fac(lfac$( 2%)), lot$(seqnr%, 33%)   , ch(06),~
               at (11,26), fac(lfac$( 2%)), lot$(seqnr%, 34%)   , ch(06),~
               at (11,34), fac(lfac$( 2%)), lot$(seqnr%, 35%)   , ch(06),~
               at (11,42), fac(lfac$( 2%)), lot$(seqnr%, 36%)   , ch(06),~
               at (11,50), fac(lfac$( 2%)), lot$(seqnr%, 37%)   , ch(06),~
               at (11,58), fac(lfac$( 2%)), lot$(seqnr%, 38%)   , ch(06),~
               at (11,66), fac(lfac$( 2%)), lot$(seqnr%, 39%)   , ch(06),~
               at (11,74), fac(lfac$( 2%)), lot$(seqnr%, 40%)   , ch(06),~
                                                                         ~
               at (12,02), fac(lfac$( 2%)), lot$(seqnr%, 41%)   , ch(06),~
               at (12,10), fac(lfac$( 2%)), lot$(seqnr%, 42%)   , ch(06),~
               at (12,18), fac(lfac$( 2%)), lot$(seqnr%, 43%)   , ch(06),~
               at (12,26), fac(lfac$( 2%)), lot$(seqnr%, 44%)   , ch(06),~
               at (12,34), fac(lfac$( 2%)), lot$(seqnr%, 45%)   , ch(06),~
               at (12,42), fac(lfac$( 2%)), lot$(seqnr%, 46%)   , ch(06),~
               at (12,50), fac(lfac$( 2%)), lot$(seqnr%, 47%)   , ch(06),~
               at (12,58), fac(lfac$( 2%)), lot$(seqnr%, 48%)   , ch(06),~
               at (12,66), fac(lfac$( 2%)), lot$(seqnr%, 49%)   , ch(06),~
               at (12,74), fac(lfac$( 2%)), lot$(seqnr%, 50%)   , ch(06),~
                                                                         ~
               at (13,02), fac(lfac$( 2%)), lot$(seqnr%, 51%)   , ch(06),~
               at (13,10), fac(lfac$( 2%)), lot$(seqnr%, 52%)   , ch(06),~
               at (13,18), fac(lfac$( 2%)), lot$(seqnr%, 53%)   , ch(06),~
               at (13,26), fac(lfac$( 2%)), lot$(seqnr%, 54%)   , ch(06),~
               at (13,34), fac(lfac$( 2%)), lot$(seqnr%, 55%)   , ch(06),~
               at (13,42), fac(lfac$( 2%)), lot$(seqnr%, 56%)   , ch(06),~
               at (13,50), fac(lfac$( 2%)), lot$(seqnr%, 57%)   , ch(06),~
               at (13,58), fac(lfac$( 2%)), lot$(seqnr%, 58%)   , ch(06),~
               at (13,66), fac(lfac$( 2%)), lot$(seqnr%, 59%)   , ch(06),~
               at (13,74), fac(lfac$( 2%)), lot$(seqnr%, 60%)   , ch(06),~
                                                                         ~
               at (14,02), fac(lfac$( 2%)), lot$(seqnr%, 61%)   , ch(06),~
               at (14,10), fac(lfac$( 2%)), lot$(seqnr%, 62%)   , ch(06),~
               at (14,18), fac(lfac$( 2%)), lot$(seqnr%, 63%)   , ch(06),~
               at (14,26), fac(lfac$( 2%)), lot$(seqnr%, 64%)   , ch(06),~
               at (14,34), fac(lfac$( 2%)), lot$(seqnr%, 65%)   , ch(06),~
               at (14,42), fac(lfac$( 2%)), lot$(seqnr%, 66%)   , ch(06),~
               at (14,50), fac(lfac$( 2%)), lot$(seqnr%, 67%)   , ch(06),~
               at (14,58), fac(lfac$( 2%)), lot$(seqnr%, 68%)   , ch(06),~
               at (14,66), fac(lfac$( 2%)), lot$(seqnr%, 69%)   , ch(06),~
               at (14,74), fac(lfac$( 2%)), lot$(seqnr%, 70%)   , ch(06),~
                                                                         ~
               at (15,02), fac(lfac$( 2%)), lot$(seqnr%, 71%)   , ch(06),~
               at (15,10), fac(lfac$( 2%)), lot$(seqnr%, 72%)   , ch(06),~
               at (15,18), fac(lfac$( 2%)), lot$(seqnr%, 73%)   , ch(06),~
               at (15,26), fac(lfac$( 2%)), lot$(seqnr%, 74%)   , ch(06),~
               at (15,34), fac(lfac$( 2%)), lot$(seqnr%, 75%)   , ch(06),~
               at (15,42), fac(lfac$( 2%)), lot$(seqnr%, 76%)   , ch(06),~
               at (15,50), fac(lfac$( 2%)), lot$(seqnr%, 77%)   , ch(06),~
               at (15,58), fac(lfac$( 2%)), lot$(seqnr%, 78%)   , ch(06),~
               at (15,66), fac(lfac$( 2%)), lot$(seqnr%, 79%)   , ch(06),~
               at (15,74), fac(lfac$( 2%)), lot$(seqnr%, 80%)   , ch(06),~
                                                                         ~
               at (16,02), fac(lfac$( 2%)), lot$(seqnr%, 81%)   , ch(06),~
               at (16,10), fac(lfac$( 2%)), lot$(seqnr%, 82%)   , ch(06),~
               at (16,18), fac(lfac$( 2%)), lot$(seqnr%, 83%)   , ch(06),~
               at (16,26), fac(lfac$( 2%)), lot$(seqnr%, 84%)   , ch(06),~
               at (16,34), fac(lfac$( 2%)), lot$(seqnr%, 85%)   , ch(06),~
               at (16,42), fac(lfac$( 2%)), lot$(seqnr%, 86%)   , ch(06),~
               at (16,50), fac(lfac$( 2%)), lot$(seqnr%, 87%)   , ch(06),~
               at (16,58), fac(lfac$( 2%)), lot$(seqnr%, 88%)   , ch(06),~
               at (16,66), fac(lfac$( 2%)), lot$(seqnr%, 89%)   , ch(06),~
               at (16,74), fac(lfac$( 2%)), lot$(seqnr%, 90%)   , ch(06),~
                                                                         ~
               at (17,02), fac(lfac$( 2%)), lot$(seqnr%, 91%)   , ch(06),~
               at (17,10), fac(lfac$( 2%)), lot$(seqnr%, 92%)   , ch(06),~
               at (17,18), fac(lfac$( 2%)), lot$(seqnr%, 93%)   , ch(06),~
               at (17,26), fac(lfac$( 2%)), lot$(seqnr%, 94%)   , ch(06),~
               at (17,34), fac(lfac$( 2%)), lot$(seqnr%, 95%)   , ch(06),~
               at (17,42), fac(lfac$( 2%)), lot$(seqnr%, 96%)   , ch(06),~
               at (17,50), fac(lfac$( 2%)), lot$(seqnr%, 97%)   , ch(06),~
               at (17,58), fac(lfac$( 2%)), lot$(seqnr%, 98%)   , ch(06),~
               at (17,66), fac(lfac$( 2%)), lot$(seqnr%, 99%)   , ch(06),~
               at (17,74), fac(lfac$( 2%)), lot$(seqnr%,100%)   , ch(06),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40525
                  call "MANUAL" ("LOTRECAL") : goto L40200

L40525:        if keyhit% <> 15% then L40540
                  call "PRNTSCRN" : goto L40200

L40540:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40645     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                      (10)A" &       ~
                     "rchive Year            (13)Instructions"
            pf$(2%) = "(2)Restart Part                         " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                        (9)Summary      " &       ~
                     "                       (16)Exit Program "
            pfkeys$ = hex(0102ffffffffffff090affff0dff0f1000)
            if fieldnr% = 1% then L40615
                str(pf$(3%),64%)     = " " : str(pfkeys$,16%,1%) = hex(ff)
L40615:     if fieldnr% > 1% then L40630
                str(pf$(2%), 1%,16%) = " " : str(pfkeys$, 2%,1%) = hex(ff)
            if part$(1%) <> " "  then  return
L40630:         str(pf$(3%),25%,10%) = " " : str(pfkeys$,9%,1%) = hex(ff)
            return

L40645: if fieldnr% > 0% then L40735  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over   (4)Prev            (10)A" &       ~
                     "rchive Year            (13)Instructions"
            pf$(2%) = "(2)Restart Part (5)Next            (11)A" &       ~
                     "ppend                  (15)Print Screen"
            pf$(3%) = "                                   (12)D" &       ~
                     "elete                  (16)Summary Scrn"
            pfkeys$ = hex(0102ff0405ffffffff0a0b0c0dff0f1000)
            if seqnr% <> max_seqnr% then L40695
                str(pf$(2%),17%, 8%) = " " : str(pfkeys$, 5%,1%) = hex(ff)
L40695:     if seqnr%  > 1%         then L40730
                str(pf$(1%),17%, 8%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
            if part$(1%) > " " then return
                str(pf$(3%),24%,   ) = " " : str(pfkeys$, 9%,1%) = hex(ff)
                                             str(pfkeys$,12%,1%) = hex(ff)
                                             str(pfkeys$,16%,1%) = hex(ff)

L40730:     return
L40735:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "(2)Restart Part                         " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'050(2%, fieldnr%)

              str(line2$,20%, 32%) = "Summary Screen              "

              gosub set_pf2
              init(hex(8c))  lfac$()
              for i% =  1% to 15%
                   if part$(dln% + i%) <> " " then L41070
                       i% = 16%
                       goto  L41080
L41070:            lfac$(i%) = hex(81)                  /* Upper Only */
L41080:       next i%

L41090:     accept                                                       ~
               at (01,02),                                               ~
                  "Recall List Report",                                  ~
               at (01,40), "Archive Year:",fac(hex(84)),arcyear$, ch(07),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(84)), sort_msg$              , ch(31),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,04), fac(hex(ac))   , hdr_scrn_2$         , ch(77),~
                                                                         ~
               at (06,02), fac(lfac$( 1%)), pick$ (dln% +  1%)  , ch(01),~
               at (06,04), fac(hex(84))   , part$ (dln% +  1%)  , ch(25),~
               at (06,32), fac(hex(84))   , descr$(dln% +  1%)  , ch(32),~
                                                                         ~
               at (07,02), fac(lfac$( 2%)), pick$ (dln% +  2%)  , ch(01),~
               at (07,04), fac(hex(84))   , part$ (dln% +  2%)  , ch(25),~
               at (07,32), fac(hex(84))   , descr$(dln% +  2%)  , ch(32),~
                                                                         ~
               at (08,02), fac(lfac$( 3%)), pick$ (dln% +  3%)  , ch(01),~
               at (08,04), fac(hex(84))   , part$ (dln% +  3%)  , ch(25),~
               at (08,32), fac(hex(84))   , descr$(dln% +  3%)  , ch(32),~
                                                                         ~
               at ( 9,02), fac(lfac$( 4%)), pick$ (dln% +  4%)  , ch(01),~
               at ( 9,04), fac(hex(84))   , part$ (dln% +  4%)  , ch(25),~
               at ( 9,32), fac(hex(84))   , descr$(dln% +  4%)  , ch(32),~
                                                                         ~
               at (10,02), fac(lfac$( 5%)), pick$ (dln% +  5%)  , ch(01),~
               at (10,04), fac(hex(84))   , part$ (dln% +  5%)  , ch(25),~
               at (10,32), fac(hex(84))   , descr$(dln% +  5%)  , ch(32),~
                                                                         ~
               at (11,02), fac(lfac$( 6%)), pick$ (dln% +  6%)  , ch(01),~
               at (11,04), fac(hex(84))   , part$ (dln% +  6%)  , ch(25),~
               at (11,32), fac(hex(84))   , descr$(dln% +  6%)  , ch(32),~
                                                                         ~
               at (12,02), fac(lfac$( 7%)), pick$ (dln% +  7%)  , ch(01),~
               at (12,04), fac(hex(84))   , part$ (dln% +  7%)  , ch(25),~
               at (12,32), fac(hex(84))   , descr$(dln% +  7%)  , ch(32),~
                                                                         ~
               at (13,02), fac(lfac$( 8%)), pick$ (dln% +  8%)  , ch(01),~
               at (13,04), fac(hex(84))   , part$ (dln% +  8%)  , ch(25),~
               at (13,32), fac(hex(84))   , descr$(dln% +  8%)  , ch(32),~
                                                                         ~
               at (14,02), fac(lfac$( 9%)), pick$ (dln% +  9%)  , ch(01),~
               at (14,04), fac(hex(84))   , part$ (dln% +  9%)  , ch(25),~
               at (14,32), fac(hex(84))   , descr$(dln% +  9%)  , ch(32),~
                                                                         ~
               at (15,02), fac(lfac$(10%)), pick$ (dln% + 10%)  , ch(01),~
               at (15,04), fac(hex(84))   , part$ (dln% + 10%)  , ch(25),~
               at (15,32), fac(hex(84))   , descr$(dln% + 10%)  , ch(32),~
                                                                         ~
               at (16,02), fac(lfac$(11%)), pick$ (dln% + 11%)  , ch(01),~
               at (16,04), fac(hex(84))   , part$ (dln% + 11%)  , ch(25),~
               at (16,32), fac(hex(84))   , descr$(dln% + 11%)  , ch(32),~
                                                                         ~
               at (17,02), fac(lfac$(12%)), pick$ (dln% + 12%)  , ch(01),~
               at (17,04), fac(hex(84))   , part$ (dln% + 12%)  , ch(25),~
               at (17,32), fac(hex(84))   , descr$(dln% + 12%)  , ch(32),~
                                                                         ~
               at (18,02), fac(lfac$(13%)), pick$ (dln% + 13%)  , ch(01),~
               at (18,04), fac(hex(84))   , part$ (dln% + 13%)  , ch(25),~
               at (18,32), fac(hex(84))   , descr$(dln% + 13%)  , ch(32),~
                                                                         ~
               at (19,02), fac(lfac$(14%)), pick$ (dln% + 14%)  , ch(01),~
               at (19,04), fac(hex(84))   , part$ (dln% + 14%)  , ch(25),~
               at (19,32), fac(hex(84))   , descr$(dln% + 14%)  , ch(32),~
                                                                         ~
               at (20,02), fac(lfac$(15%)), pick$ (dln% + 15%)  , ch(01),~
               at (20,04), fac(hex(84))   , part$ (dln% + 15%)  , ch(25),~
               at (20,32), fac(hex(84))   , descr$(dln% + 15%)  , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L41490
                  call "MANUAL" ("LOTRECAL") : goto L41090

L41490:        if keyhit% <> 15% then L41505
                  call "PRNTSCRN" : goto L41090

L41505:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
            /*  Select Mode             */
            pf$(1) = "(1)Start Over (4)Prev Page   (10)Archive" &        ~
                     " Year                  (13)Instructions"
            pf$(2) = "              (5)Next Page   (11)Append " &        ~
                     "Part                   (15)Print Screen"
            pf$(3) = "              (9)Sort Toggle (14)Print R" &        ~
                     "eport (30)File Report  (16)Exit Program"
            pfkeys$ = hex(01ffff0405ffffff090a0bff0d0e0f10001e)
            if max_seqnr% >  dln% + 15% then L41580
                str(pf$(2%),15%,12%) = " " : str(pfkeys$,05%,1%) = hex(ff)
L41580:     if dln%       >  0% then     L41590
                str(pf$(1%),15%,12%) = " " : str(pfkeys$,04%,1%) = hex(ff)
L41590:     return


        REM *************************************************************~
            *            R E P O R T   F I L E   N A M E                *~
            * --------------------------------------------------------- *~
            * Input File Name for outrput File.                         *~
            *                                                           *~
            *************************************************************
        set_filename_screen
            getback$            = "(1)Return"

L42100:     accept                                                       ~
               at (01,02),                                               ~
                  "Recall List Report            "              ,        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (08,23), "************* FILE NAME **************",     ~
               at (09,23), "*                                    *",     ~
               at (10,23), "* Enter the Report Output File Name  *",     ~
               at (11,23), "*                                    *",     ~
               at (12,23), "*                                    *",     ~
               at (13,23), "*                                    *",     ~
               at (14,23), "*                                    *",     ~
               at (15,23), "**************************************",     ~
                                                                         ~
               at (12,36), fac(hex(81)), outfile$               , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)), blankline$             , ch(79),~
                                                                         ~
               at (22,02), fac(hex(84)), getback$               ,        ~
               at (22,65), "(13)Instructions"                   ,        ~
               at (23,65), "(15)Print Screen"                   ,        ~
                                                                         ~
               keys(hex(00010d0f)),                                      ~
               key (keyhit%)

               if keyhit% <> 13% then L42420
                  call "MANUAL" ("LOTARCHV")
                  goto L42100

L42420:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L42100

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* Part Number            */~
                              L50300          /* Lot                    */
            return
L50100: REM Test for Part Number                  PART$
            call "GETCODE" (#02, part$(seqnr%), descr$(seqnr%), 0%, 0,   ~
                                                                 f1%(2%))
               if f1%(2%) <> 0% then L50155   /* Test for already there */
                  descr$(seqnr%) = "* * Part Not on File * *"
                  if part$(seqnr%) = "?" or part$(seqnr%) = " "          ~
                      then  errormsg$ = "Please Enter a Part Number."
L50155:     for i% = 1% to max_seqnr%
                if i% = seqnr% then L50260          /* Don't Check Self */
                if part$(i%) <> part$(seqnr%) then  L50260
L50175:             ask% = 2%
                    call "ASKUSER" (ask%, "** DUPLICATE PART **",        ~
                                 "You have Previously Entered this Part",~
                                  "Press PF (2) to Reenter the Part ",   ~
                                  "Press ENTER  to Review Part " )
                    if ask% =   0% then L50235
                    if ask% <>  2% then L50175
                        errormsg$  = "Duplicate Part Number."
                        goto L50255
L50235:              part$(seqnr%), descr$(seqnr%) = " "
                     seqnr% = i%    /* Go to existing Part's Screen */
                     if seqnr% < max_seqnr%                              ~
                         then max_seqnr% = max_seqnr% - 1%
L50255:              i% = 101%

L50260:     next i%
            return

L50300: REM Test for Lot                          LOT$
            hit% = 0%
            for j% = 1% to  99%
                if lot$(seqnr%, j%) = " " then L50360  /* Skip Blanks */
                hit% = 1%
                for k% =  j% + 1%  to  100%
                    if lot$(seqnr%, j%) <> lot$(seqnr%, k%) then L50355
                        errormsg$ = "Lot: " & lot$(seqnr%, j%) &         ~
                                    " is duplicated."
                        k% = 101%
                        j% = 101%               /* Force End of Loop */
L50355:        next k%
L50360:     next j%

            if hit% = 0% then errormsg$ = "Please Enter a Lot Code."
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
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")
            if arcyear$ = "Current" then L65210
                lotmprname$ = "LOTMVMNT" : close #03
                call "PUTPRNAM" addr(#03, lotmprname$)
                call "OPENCHCK" (#03, fs%(3%), f2%(3%), 0%, rslt$(3%))
                arcyear$ = "Current"
L65210:     end
