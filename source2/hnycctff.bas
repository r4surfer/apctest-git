        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y   CCC    CCC   TTTTT  FFFFF  FFFFF   *~
            *  H   H  NN  N  Y   Y  C   C  C   C    T    F      F       *~
            *  HHHHH  N N N   YYY   C      C        T    FFFF   FFFF    *~
            *  H   H  N  NN    Y    C   C  C   C    T    F      F       *~
            *  H   H  N   N    Y     CCC    CCC     T    F      F       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYCCTFF - This program will act against the contents of  *~
            *            the Cycle Count Master file,  will read the    *~
            *            HNYDETAL file for each selected part and       *~
            *            calculate the # of transactions (additions     *~
            *            and withdrawals) that have occurred during     *~
            *            a particular time period.                      *~
            *     Note-> This program currently does not take the       *~
            *            transaction types in HNYDETAL into a           *~
            *            consideration in determining the Transaction   *~
            *            Frequence Factor.                              *~
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
            * 04/17/92 ! Original                                 ! SID *~
            * 05/05/93 ! Now considers Archived HNYDETAL files.   ! JDH *~
            * 08/08/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            arch_beg$6,                  /* Archive file starting date */~
            arch_end$6,                  /* Archive file ending date   */~
            archname$8,                  /* Archive file name HNYDYYYY */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cntr$7,                      /* # of rec wrote to HNYCCMST */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            date_tff_changed$8,          /* Date Trans Freq Fctr Change*/~
            descr$42,                    /* Descr For PlowCode         */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fmccgroup$6,                 /* Cycle Count Group Range    */~
            fmdate$10,                   /* Date Range                 */~
            from$8,                      /* Literal for Input Screen   */~
            header$55,                   /* Header Literal for HNYCCRNG*/~
            hnydate$8,                   /* Posting Date from HNYDETAL */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lot$16,                      /* Lot Code                   */~
            part$25,                     /* Part Code                  */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pf16$16,                     /* PF16 Literals for HNYCCRNG */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            plowkey1$99,                 /* Read/Plow Key for HNYDETAL */~
            r$(24)80,                    /* Screen Image from HNYCCRNG */~
            store$3,                     /* Store Code                 */~
            transfrefctrflag$,           /* Trans Freq Fctr Lock Flag  */~
            to$8,                        /* Literal for Input Screen   */~
            todate$10,                   /* Date Range                 */~
            toccgroup$6,                 /* CC Group Range TESTRNGE    */~
            unfdate$10,                  /* Unformat Today's Date      */~
            unffmdate$10,                /* Unformat Input Date Range  */~
            unftodate$10,                /* Unformat Input Date Range  */~
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
            * #01 ! HNYCCMST ! Cycle Count Master File                  *~
            * #02 ! HNYCCDTL ! Cycle Count Session Detail File          *~
            * #03 ! HNYCCGRP ! Cycle Count Group File                   *~
            * #04 ! HNYCCSYS ! Cycle Count System Control File          *~
            * #05 ! HNYMASTR ! Inventory Master File                    *~
            * #06 ! HNYDETAL ! Inventory Master File                    *~
            * #07 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * #08 ! CATEGORY ! Iventory Category Codes File             *~
            * #09 ! STORNAME ! Store Information File                   *~
            * #10 ! ARCHVREC ! Archived History File                    *~
            * #11 ! HNYDYYYY ! Inventory Details Archived File          *~
            * #50 ! WORKFILE ! Temporary System Workfile                *~
            * #51 ! WORKFIL2 ! Temporary System Workfile Number 2       *~
            * #52 ! WORKFIL3 ! List of Archived HNYDETAL files          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYCCMST",                                      ~
                        varc,     indexed,  recsize =  796,              ~
                        keypos =    1, keylen =  44,                     ~
                        alt key  1, keypos =   45, keylen =   6, dup,    ~
                            key  2, keypos =   81, keylen =   7, dup,    ~
                            key  3, keypos =   73, keylen =  15, dup


            select #02, "HNYCCDTL",                                      ~
                        varc,     indexed,  recsize =  436,              ~
                        keypos =    1, keylen =  57,                     ~
                        alt key  1, keypos =   13, keylen =  45, dup,    ~
                            key  2, keypos =   14, keylen =  44, dup


            select #03, "HNYCCGRP",                                      ~
                        varc,     indexed,  recsize =  616,              ~
                        keypos =    1, keylen =   6                      ~

            select #04, "HNYCCSYS",                                      ~
                        varc,     indexed,  recsize =  249,              ~
                        keypos =    1, keylen =  12,                     ~
                        alt key  1, keypos =   43, keylen =   7, dup,    ~
                            key  2, keypos =   50, keylen =  10


            select #05, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup     ~

            select #06, "HNYDETAL",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =   43, keylen =   6, dup,    ~
                            key  2, keypos =   49, keylen =   2, dup

            select #07, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #08, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos  =   1, keylen = 4

            select #09, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3

            select #10, "ARCHVREC",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  30

            select #11, "HNYDYYYY",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =   43, keylen =   6, dup,    ~
                            key  2, keypos =   49, keylen =   2, dup

            select #50, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =   44,              ~
                        keypos =    1, keylen =  44

            select #51, "WORKFIL2",                                      ~
                        varc,     indexed,  recsize =  52,               ~
                        keypos =  1,   keylen = 44

            select #52, "WORKFIL3",                                      ~
                        varc,     indexed,  recsize =  08,               ~
                        keypos =  1,   keylen = 08

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
                 get rslt$(01) using L04025, rec%
L04025:              FMT POS(17), BI(4)
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))
            call "OPENCHCK" (#06, fs%(06), f2%(06), 0%, rslt$(06))
            call "OPENCHCK" (#07, fs%(07), f2%(07), 0%, rslt$(07))
            call "OPENCHCK" (#08, fs%(08), f2%(08), 0%, rslt$(08))
            call "OPENCHCK" (#09, fs%(09), f2%(09), 0%, rslt$(09))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$, date_tff_changed$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "HNYCCTFF: " & str(cms2v$,,8)

            from$ = "From" : to$ = "To"

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
                      if keyhit% <>  4% then       L10205
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10205:               if keyhit% = 8% then select_ranges
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
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
                  if keyhit%  =  8% then select_ranges
                  if keyhit%  = 16% then       process_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 2% or fieldnr% >  3% then editpg1
            if fieldnr% = 2% then fieldnr% = 1% else fieldnr% = 2%
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  8% then select_ranges
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        process_data
            gosub look_for_archives
            init(" ") plowkey$
            call "SHOSTAT" ("Processing .......")
            call "FILEBGON" (#51) /* Destroy Work File */
            call "WORKOPEN" (#51, "IO", rec%/2%, f2%(51))

            if (fmccgroup$ = "FIRST" and toccgroup$ = "LAST") or         ~
                         fmccgroup$ = "ALL" then L19087
            str(plowkey$,1,6) = str(fmccgroup$) addc all(hex(ff))

L19087:     call "PLOWALTS" (#01, plowkey$, 1%, 0%, f1%(01))/* HNYCCMST */
             goto L19090
L19089:     call "READNEXT" (#01, f1%(01))
L19090:      if f1%(01) = 0% then update_hnyccmst

            if (fmccgroup$ = "FIRST" and toccgroup$ = "LAST") or         ~
                    fmccgroup$ = "ALL" then L19140 /* Test Group Range */
            if key(#01,1) > toccgroup$ then update_hnyccmst

L19140:        get #01 using L19150, part$, store$, lot$,                 ~
                                    transfrefctrflag$, cur_transfrefctr
L19150:            FMT CH(25), CH(3), CH(16), POS(138), CH(1), PD(14,4)

               if transfrefctrflag$ = "N" then L19170 /* If not lock */
                if cur_transfrefctr = 0 then L19170 /* Still Update if 0 */
                goto L19089  /* Get next HNYCCMST */

L19170:        gosub plow_hnydetal
               goto L19089   /* Get next HNYCCMST */

        /* Cal. Cycle Count Trans. Freq Fctr thru Range Selection */
        select_ranges
            call "SHOSTAT" ("Opening Work Files, One Moment Please")
            call "FILEBGON" (#50) /* Destroy Work File */
            call "WORKOPEN" (#50, "IO", rec%/2%, f2%(50))
            cntr%, count% = 0% : pf16$ = "(16)Proceed    "
            header$ = "Calculate Cycle Count Transaction Frequency Factor"
            call "HNYCCRNG" ("Y", 2%, #05, #07, #08, #09, #01, #50,      ~
                             count%, header$, pf16$, r$())

            if count% = 0% then inputmode

            call "SHOSTAT" ("Processing .... ")
            call "FILEBGON" (#51) /* Destroy Work File */
            call "WORKOPEN" (#51, "IO", rec%/2%, f2%(50))

            /* Now Plow Thru the WORKFILE #50 and keeps track of the     ~
               # of occurrence per STORE/PART/LOT in HNYDETAL and        ~
               Calculate/Update HNYCCMST Transaction Frequence Factor   */
            init(" ") plowkey$
L19610:     call "PLOWNEXT" (#50, plowkey$, 0%, f1%(50))
                if f1%(50) = 1% then L19660
                goto update_hnyccmst

L19660:     call "READ101" (#01, str(plowkey$,,44), f1%(01)) /*HNYCCMST*/
               if f1%(01) = 0% then L19610 /* This shouldn't happen */

            get #01 using L19700, part$, store$, lot$, transfrefctrflag$, ~
                                 cur_transfrefctr
L19700:               FMT CH(25), CH(3), CH(16), POS(138), CH(1), PD(14,4)

            if transfrefctrflag$ = "N" then L19720
             if cur_transfrefctr =  0  then L19720
             goto L19610 /* Get Next WORKFILE record */

L19720:     gosub plow_hnydetal
            goto L19610 /* Get next record from WORKFILE to process */

        look_for_archives
            call "SHOSTAT" ("Looking for Archived files...")
            call "FILEBGON" (#52)
            call "WORKOPEN" (#52, "IO", 100%, f2%(52%))
            plowkey$ = "HNYDETAL" : nmbr_archives% = 0%
L19820:     call "PLOWNEXT" (#10, plowkey$, 8%, f1%(10%))
                if f1%(10%) = 0% then return

            get #10 using L19850, archname$, arch_beg$, arch_end$,        ~
                                 arch_recs%
L19850:          FMT POS(9), CH(8), POS(31), 2*CH(6), POS(46), BI(4)
            call "READ100" (#52, archname$, f1%(52%))
                if f1%(52%) = 1% then L19820  /* Already got this one */
            if arch_recs% = 0% then L19820    /* No Records */
            if (unffmdate$ >= arch_beg$ and unffmdate$ <= arch_end$) or  ~
               (unftodate$ >= arch_beg$ and unftodate$ <= arch_end$)     ~
                then got_an_archive
            if (arch_beg$ >= unffmdate$ and arch_beg$ <= unftodate$) or  ~
               (arch_end$ >= unffmdate$ and arch_end$ <= unftodate$)     ~
                then got_an_archive
            goto L19820 /* Archive file not in range of dates */
        got_an_archive
            call "PUTPRNAM" addr(#11, archname$)
            call "OPENFILE"(#11, "VALID", f2%(11%), rslt$(11%), " ")
                if f2%(11%) = 0% then write_archive_file_name

L19940:     u3% = 2%
            call "ASKUSER" (u3%, "*** ARCHIVED FILE MISSING ***",        ~
                "Archived file " & archname$ & " is not on disk!",       ~
                "Press PF1 to ABORT Processing  -or-",                   ~
                "Press PF16 to Continue Without File.")
            if u3% = 1% then startover_2
            if u3% <> 16% then L19940

        write_archive_file_name
            write #52, archname$, eod goto L19820
            nmbr_archives% = nmbr_archives% + 1%
            goto L19820

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Date Range             */~
                              L20200          /* Cycle Count Group      */
            return

L20100: REM Def/Enable Date Range                  FMDATE$/TODATE$
            if fmdate$ <> " " and fmdate$ <> blankdate$ and ~
               todate$ <> " " and todate$ <> blankdate$ then return
            if fmdate$ = " " or fmdate$ = blankdate$ then fmdate$ = date
            if todate$ = " " or todate$ = blankdate$ then todate$ = date
            call "DATE" addr("G+", fmdate$, -90%, fmdate$, ret%)
            call "DATFMTC" (fmdate$)
            call "DATFMTC" (todate$)
            return

L20200: REM Def/Enable Cycle Count Group Range     FMCCGROUP$/TOCCGROUP$
            if fmccgroup$ = " " then fmccgroup$ = "ALL"
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
         "Enter Date Range                                             ",~
         "Enter Cycle Count Group Range                                "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, plowkey$,                  ~
                      fmccgroup$, toccgroup$, fmdate$, todate$,          ~
                      part$, store$, lot$

            count%, nbr_of_trans, new_transfrefctr, cntr% = 0
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
        startover_2
            return clear all
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        display_result
            convert cntr% to cntr$, pic(#######)
            keyhit% = 2% /* Window in the Bottom */
            call "ASKUSER" (keyhit%, "*** RESULTS ****",                 ~
               "There are a total of " & hex(80) & cntr$ & " record(s)", ~
               "updated to the HNYCCMST file", "Press Any key to RETURN")
            goto inputmode

        update_hnyccmst
            date_tff_changed$ = date$
            call "DATUNFMT" (date_tff_changed$)
            init(" ") plowkey$ : cntr% = 0%
L32040:     call "PLOWNEXT" (#51, plowkey$, 0%, f1%(51))
                if f1%(51) = 1% then L32080
                   goto display_result

L32080:     call "READ101" (#01, plowkey$, f1%(01))
               if f1%(01) = 0% then L32040  /* Not Happen Until EOF */
            new_transfrefctr, cur_transfrefctr = 0
            get #01 using L32120, cur_transfrefctr
L32120:         FMT POS(139), PD(14,4)
            get #51 using L32140, new_transfrefctr
L32140:             FMT POS(45), PD(14,4)
            put #01 using L32170, new_transfrefctr, cur_transfrefctr,     ~
                                 userid$, date_tff_changed$
L32170:             FMT POS(139), 2*PD(14,4), POS(171), CH(3), CH(6)
            rewrite #01, eod goto L32200
            cntr% = cntr% + 1%
L32200:     goto L32040

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        plow_hnydetal
            /* Plow thru HNYDETAL via Part/Store/Lot for # of Freq */
            nbr_of_trans, new_transfrefctr = 0
            c% = 6% /* HNYDETAL */
L35020:     init(" ") plowkey1$
            str(plowkey1$,,34) = str(part$) & str(store$) & str(lot$,,6)
L35030:     call "PLOWNEXT" (#c%, plowkey1$, 34%, f1%(c%)) /* HNYDETAL */
                if f1%(c%) = 1% then L35170

            /* Set up for reading through archived files.         */
            if nmbr_archives% = 0% then L35080
                if c% <> 11% then L35048
                     close #11 : goto L35054
L35048:         temp$ = hex(0000000000000000)
                call "READ102" (#52, temp$, f1%(52%))
                     goto L35056
L35054:         call "READNEXT" (#52, f1%(52%))
L35056:         if f1%(52%) = 0% then L35080
                     get #52, archname$
                     call "PUTPRNAM" addr(#11, archname$)
                     f2%(11%) = 1%
                     call "OPENCHCK" (#11, fs%(11%), f2%(11%), 0%, " ")
                     if f2%(11%) <> 0% then L35054
                          c% = 11%
                          goto L35020

L35080:     /* Calculate Cycle Count Transaction Frequency Factor */
                if nbr_of_trans = 0 then L35090 /* No HNYDETAL Record */
                new_transfrefctr = nbr_of_trans / daysdiff%
L35090:         put #51 using L35110, part$, store$, lot$, new_transfrefctr
L35110:              FMT CH(25), CH(3), CH(16), PD(14,4)

                write #51, eod goto L35140
L35140:         nbr_of_trans, new_transfrefctr = 0
                return /* Get next HNYCCMST */

L35170:     init(" ") hnydate$ : hnydate$ = key(#c%,1)
            if unffmdate$ > hnydate$ or unftodate$ < hnydate$ then L35030
            nbr_of_trans = nbr_of_trans + 1

            goto L35030 /* Get next HNYDETAL */


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
              on fieldnr% gosub L40075,         /* Date Range        */   ~
                                L40080          /* Cycle Count Group */
              goto L40095

L40075:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40080:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40095:     accept                                                       ~
               at (01,02),                                               ~
                  "Calculate Cycle Count Transaction Frequency Factor",  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)), from$                  , ch(08),~
               at (06,42), fac(hex(ac)), to$                    , ch(08),~
                                                                         ~
               at (07,02), "Date Range",                                 ~
               at (07,30), fac(lfac$( 1)), fmdate$              , ch(10),~
               at (07,42), fac(lfac$( 1)), todate$              , ch(10),~
                                                                         ~
               at (08,02), "Cycle Count Group Range",                    ~
               at (08,30), fac(lfac$( 2)), fmccgroup$           , ch(06),~
               at (08,42), fac(lfac$( 2)), toccgroup$           , ch(06),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40220
                  call "MANUAL" ("HNYCCTFF") : goto L40095

L40220:        if keyhit% <> 15 then L40235
                  call "PRNTSCRN" : goto L40095

L40235:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40330     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40310
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40310:     if fieldnr% > 1% then L40316
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40316:     if fieldnr% <> 2% then L40320
                str(pf$(3),41,16) = "(8)Select Ranges"
                str(pfkeys$, 8,1) = hex(08)
L40320:     return

L40330: if fieldnr% > 0% then L40375  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Process     "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40375:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            if fieldnr% <> 2% then return
              str(pf$(3),41,16) = "(8)Select Ranges"
              str(pfkeys$, 8, 1) = hex(08)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* Date Range             */~
                              L50200          /* Cycle Count Group      */
            return
L50100: REM Test for Date Range                   FMDATE$/TODATE$
            init(" ") unffmdate$, unftodate$
            call "DATEOKC" (fmdate$, 0%, errormsg$)
                if errormsg$ <> " " then return
            call "DATEOKC" (todate$, 0%, errormsg$)
                if errormsg$ <> " " then return
            unfdate$ = date$ : call "DATUNFMT" (unfdate$)
            unffmdate$ = fmdate$ : unftodate$ = todate$
            call "DATUFMTC" (unffmdate$) : call "DATUFMTC" (unftodate$)
            if unftodate$ > unfdate$ then                                ~
               errormsg$ = "To Date Can Not be Greater than Today's Date"
               if errormsg$ <> " " then return
            if unftodate$ < unffmdate$ then                              ~
               errormsg$ = "To Date Can Not be Greater than From Date"
               if errormsg$ <> " " then return
            call "DATE" addr("G-",unffmdate$, unftodate$, daysdiff%, ret%)
            if daysdiff% = 0% then daysdiff% = 1%
            return

L50200: REM Test for Cycle Count Group Range      FMCCGROUP$/TOGROUP$
            if (fmccgroup$ = "FIRST" and toccgroup$ = "LAST") or         ~
                fmccgroup$ = "ALL" then return
            if fmccgroup$ = " " and toccgroup$ = " " then to% = 1%       ~
                                                     else to% = 0%
            if toccgroup$ <> " " then to% = 1%

            descr$ = hex(06) & "Select FROM Group Range"
            call "PLOWCODE" (#03, fmccgroup$, descr$, 0%, 0.30, f1%(03))
               if (f1%(03) = 0% or f1%(03) = 1%) and to% = 1% then L50262
               if (f1%(03) = 0% or f1%(03) = 1%) and to% = 0% then L50354
L50262:     descr$ = hex(06) & "Select TO Group Range"
            call "PLOWCODE" (#03, toccgroup$, descr$, 0%, 0.30, f1%(03))
               if f1%(03) = 0% then L50284
                  if toccgroup$ < fmccgroup$ then L50394 else return
L50284:     if fmccgroup$ = " " and toccgroup$ = " " then                ~
                                                     fmccgroup$ = "ALL"
            if fmccgroup$ = " " and toccgroup$ <> " " then               ~
                                                fmccgroup$ = toccgroup$
            if fmccgroup$ <> " " and toccgroup$ = " " then               ~
                                                toccgroup$ = fmccgroup$
            return
L50354:     if fmccgroup$ = "?" and toccgroup$ = " " then L50384
            toccgroup$ = fmccgroup$
            return
L50384:     errormsg$ = "Group Code Does Not Exist." : return
L50394:     errormsg$ = "FROM Group Code" & fmccgroup$ &                 ~
                        " may not be greater than TO Group Code" &       ~
                        toccgroup$                   : return

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
            call "FILEBGON" (#50)
            call "FILEBGON" (#51)
            call "FILEBGON" (#52)
            call "SHOSTAT" ("One Moment Please")
            end
