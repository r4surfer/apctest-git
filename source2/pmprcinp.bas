        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  PPPP   M   M  PPPP   RRRR    CCC   IIIII  N   N  PPPP    *~
            *  P   P  MM MM  P   P  R   R  C   C    I    NN  N  P   P   *~
            *  PPPP   M M M  PPPP   RRRR   C        I    N N N  PPPP    *~
            *  P      M   M  P      R  R   C   C    I    N  NN  P       *~
            *  P      M   M  P      R   R   CCC   IIIII  N   N  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PMPRCINP - Input the Cust/ Precious Metal Item Pricing    *~
            *            table data.                                    *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/17/94 ! Original                                 ! RJH *~
            * 08/23/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            company$60,                  /* Company Name               */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9,                   /* Customer code              */~
            cuscode_org$9,               /* Customer code              */~
            cus_descr$30,                /* Description                */~
            copy_cust$9,                 /* Customer code              */~
            date$8,                      /* Date for screen display    */~
            descr$(100)30,               /* Accept Screen Variable     */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            in_cuscode$9,                /* Customer Code from caller  */~
            lastcuscode$9,               /* Previous Customer Code     */~
            lfac$(12,20)1,               /* Field Attribute Characters */~
            lfac1$(2)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lines_hdr$79,                /* Screen Lines header string */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            pmcode$(100)10, pmcode$10,   /* Precious Metal             */~
            pmcode_last$10,              /* Precious Metal             */~
            pmdescr$(100)30, pmdescr$30, /* PM Description             */~
            pmprice$(100)10, pmprice$10, /* PM Effective Price         */~
            pmprice(100),                /* PM Effective Price         */~
            pmdate$(100)8, pmdate$8,     /* PM Effective Date          */~
            pmlastprc$(100)10,           /* PM Last Price              */~
            pmlastprc(100),              /* PM Last Price              */~
            pmlastdate$(100)8,           /* PM Last Date               */~
            temp_pmlastprc$(100)10,      /* PM Last Price(Temp)        */~
            temp_pmlastdate$(100)8,      /* PM Last Date(Temp)         */~
            temp_cuscode$9,              /* Customer Code (temp)       */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rpttitle$80,                 /* Report title block         */~
            sdate$8,                     /* Save Date for File         */~
            tempcuscode$9,               /* Customer    for printing   */~
            tempdescr$32,                /* Cust Descr  for printing   */~
            uom$(100)4, uom$4,           /* UOM                        */~
            userid$3,                    /* Current User Id            */~
            warnmsg$79                   /* No PM Lines Message        */

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
            * #01 ! HNYPMPRC ! Precious Metal Price table               *~
            * #02 ! PMCODES  ! Precious metal codes file                *~
            * #03 ! CUSTOMER ! Customer Master File                     *~
            * #04 ! BCKPMSLD ! Precious Metal SO Shadow File            *~
            * #05 ! MLQPMSLD ! Precious Metal MLQ Shadow File           *~
            * #50 ! WORKFILE ! Workfile                                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYPMPRC",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos = 1,    keylen = 25

            select #02, "PMCODES",                                       ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos = 1,    keylen =  10                      ~

            select #03, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup


            select #04, "BCKPMSLD",                                      ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos = 1,    keylen = 29,                      ~
                        alt key  1, keypos =   30, keylen =  25, dup,    ~
                            key  2, keypos =   73, keylen =   9, dup,    ~
                            key  3, keypos =   20, keylen =  10, dup

            select #05, "MLQPMSLD",                                      ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos = 1,    keylen = 21,                      ~
                        alt key  1, keypos =   22, keylen =  25, dup,    ~
                            key  2, keypos =   65, keylen =   9, dup,    ~
                            key  3, keypos =   12, keylen =  10, dup

            select #50, "WORKFILE",                                      ~
                        varc, indexed, recsize = 9  ,                    ~
                        keypos =    1,  keylen =  9

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 100%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 100%, rslt$(02%))
            call "OPENCHCK" (#03, fs%(03%), f2%(03%),   0%, rslt$(03%))
            call "OPENCHCK" (#04, fs%(04%), f2%(04%),   0%, rslt$(04%))
            call "OPENCHCK" (#05, fs%(05%), f2%(05%),   0%, rslt$(05%))
            call "WORKOPEN" (#50, "IO",               100%, rslt$(50%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            sdate$ = date$
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "PMPRCINP: " & str(cms2v$,,8)
            lines_hdr$ =                                                 ~
                         "PM Code    Description                  Price" ~
                       & "      Eff Date Prev Price PrevDate"

            mxl% = 100%    /* Max Number of PM Prices per Customer */
            sum% =  10%    /* Max Number of PM Prices per Screen */

            gosub load_customer_list

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            if in_cuscode$ = " " then L10105
                 cuscode$ = in_cuscode$
                 goto editpg1

L10105:     for fieldnr% = 1% to 2%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%, 0%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  <> 4% then L10190
                          if fieldnr% = 1% then L10190
                          fieldnr% = fieldnr% - 1%
                          goto L10110
L10190:               if keyhit%  = 28% then gosub delete_customer
                      if keyhit%  = 14% then gosub print_pm_list
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130

            next fieldnr%
            fieldnr% = 0%

            gosub load_prices
            goto pm_price_lines_input

        pm_price_lines_input

            for dln% = 1% to c%
              for fn% = 1% to 2%
L10530:         gosub'052(fn%, dln%)        /* Default / Enables */
                      if enabled% = 0% then L10600
                gosub compute_zln
L10550:         gosub'101(fn%, 1%, 2%)       /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10570
                          gosub backup_one
                          goto L10530
L10570:               if keyhit%  = 14% then gosub print_pm_list
                      if keyhit%  = 12% then goto  copy_price_table
                      if keyhit%  = 11% then gosub switch_descr_display
                      if keyhit%  = 16% and fieldnr% = 1%                ~
                                          then display_pm_lines
                      if keyhit% <> 0% then       L10550
L10600:         gosub'152(fn%, dln%,0%)  /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10550

              next fn%
            next dln%

            goto display_pm_lines
        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            goto display_pm_lines
L11090:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% >  2% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
L11120:     gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11140:     gosub'101(fieldnr%, 2%, 0%) /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11140
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11140
                  lastfieldnr% = fieldnr%
                  if get_prices% = 1% then gosub load_prices
            goto L11090


        display_pm_lines
            d% = 1%                    /* Begin at the first line item */
        display_pm_lines_2
            lastfieldnr% = 0%
            gosub'101(0%, 2%, 2%)
            if keyhit% =  1% then gosub startover
            if keyhit% =  2% then d% = 1%                /* First page */
            if keyhit% =  3% then d% = max(1%, p% - sum% + 1)/*Last pg */
            if keyhit% =  4% then d% = max(1%, d% - sum%)   /* Prev pg */
            if keyhit% =  5% then                                        ~
                d% = max(1%, min(p% - sum% +1%, d% + sum%)) /* Next pg */
            if keyhit% =  6% then d% = max(1%, d% - 1%)        /* Down */
            if keyhit% =  7% then                                        ~
                d% = max(1%, min(p% - sum% +1%, d% + 1%))   /* Next pg */
            if keyhit% =  9% then goto editpg1          /* Edit Header */
            if keyhit% = 11% then gosub switch_descr_display
            if keyhit% = 14% then gosub print_pm_list
            if keyhit% = 16% then goto datasave
            if keyhit% <> 0% then goto display_pm_lines_2
L11410
*        User wants to edit an existing line item, based on DLN%.
*          GOSUB CALL_SCREEN
            fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 3% then goto L11120    /* Header Edit */
            dx% = cursor%(1%) -  8%             /* Screen line to edit */
            if dx% < 1% or dx% > sum% then goto display_pm_lines_2
            if dx% + d% - 1% > p% then goto display_pm_lines_2
            if dx% = lastfieldnr% then goto display_pm_lines_2
               fn% = 2%
               if cursor%(2%) < 53% then fn% = 1%
            zln% = dx%
            dln% = dx% + d% - 1%   /* Array Element # to Edit */
L11560:     gosub'052(fn%, dln%)
L11570:     gosub'101(fn%, 2%, 2%)
            if keyhit% =  1% then gosub startover
            if keyhit% =  4% then goto L11620   /* Validate & Prev Line */
            if keyhit% =  5% then goto L11620   /* Validate & Next Line */
            if keyhit% <> 0% then goto L11570
L11620:     gosub'152(fn%, dln%, 2%)  /* Edit Fields for Valid Entry  */
            if errormsg$ <> " " then goto L11570
            if date_dflt% = 1% then L11570  /* Date Defaulted in */
            lastfieldnr% = dx%
            if keyhit% <> 4% then goto L11700  /* Validate & Prev Line? */
                dln% = max(1%, dln% - 1%) /* Array element to edit/val */
                if dln% < d% then d% = max(1%, d% - 1%)/* Scroll down? */
                dx% = dln% - d% + 1% /* Screen line to enable for edit */
                goto L11560
L11700:     if keyhit% <> 5% then goto L11750  /* Validate & Next Line? */
                dln% = min(p%, dln% + 1%) /* Array element to edit/val */
                if dln% > sum%+d%-1% then d%=min(zln%,d%+1%)/* Scr up? */
                dx% = dln% - d% + 1% /* Screen line to enable for edit */
                goto L11560
L11750:     goto L11410

        REM *************************************************************~
            *     B I G  D E A L  O T H E R   S U B R O U T I N E S     *~
            *************************************************************

        print_pm_list
            cuscode_org$ = cuscode$
            gosub generate_report
            tempcuscode$ = "FIRSTTIME"
            pmcode_last$ = " "
            plowkey$  = hex(00)
L14540:     call "PLOWNEXT" (#01, plowkey$, 0%, f1%(1%))
            if f1%(1%) = 0% then L14590
            gosub print_lines
            goto L14540

L14590:     cuscode$ = cuscode_org$
            gosub  end_report
            return

        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *************************************************************

        compute_zln

            d%   = max(1%, dln% - sum% + 1%)  /* SUM% is set above */
            zln% = dln% - d% + 1%
            return

*       set_rev_date
            call "DATREVRS" ( eff_date$, rev_date$, errormsg$ )
            return

        set_descr_screen
            if descrflag% <> 1% then L15320
                mat descr$ = pmdescr$
                str(lines_hdr$,12%,11%) = "Description"
                return
L15320:     for i% = 1% to c%
                descr$(i%)             = all(hex(20))
                str(descr$(i%),2%,4%)  = uom$(i%)
            next i%
            str(lines_hdr$,12%,11%) = "UOM        "
            return

        switch_descr_display
            if descrflag% = 0% then descrflag% = 1% else descrflag% = 0%
            return

        backup_one
            if fn% <> 1% then L15470       /* FN% can only be 1 or 2 */
                if dln% = 1% then return  /* At 1st position */
                    fn%  = 2%
                    dln% = max(1%, dln% - 1%)
                    if pmprice$(dln%) = " " then fn% = 1%
                    return
L15470:     fn% = 1%
            return

        copy_price_table
            copy_cust$ = "DEFAULT"
            ask% = 2%
            call "ASKSTRNG" (ask%, " *** COPY PRICE TABLE ***",          ~
                             "Enter Customer to Copy Price Table From:", ~
                             "  Enter 'DEFAULT' for Default Prices",     ~
                             "Customer", copy_cust$, 9%, #50, f1%(50%))

            if copy_cust$ <> "DEFAULT" then L15570
                 copy_cust$ = " "  :  goto  L15590
L15570:     if ask% = 1% then goto  pm_price_lines_input

           /* Ok lets copy Cust/Prices to New array */
L15590:     temp_cuscode$ = cuscode$
            cuscode$ = copy_cust$
            mat temp_pmlastdate$ = pmlastdate$
            mat temp_pmlastprc$  = pmlastprc$
            gosub load_prices

            if c% = 0% then goto no_copy_done

            cuscode$        = temp_cuscode$

            mat pmdate$     = pmlastdate$
            mat pmprice$    = pmlastprc$
            mat pmlastdate$ = temp_pmlastdate$
            mat pmlastprc$  = temp_pmlastprc$

            gosub compute_zln
            goto display_pm_lines

        no_copy_done
            ask% = 2%
            call "ASKUSER" (ask%, " ** NO COPY DONE **",                 ~
                            "No Precioous Metal Prices found for : " &   ~
                             copy_cust$, " ", "Press Any Key to Continue")
            goto L10550      /* Return to Input Mode */

        load_customer_list
            plowkey$ = hex(00)
L15860:     call "READ102" (#01, plowkey$, f1%(1%))
                if f1%(1%) = 0% then return
            get #01 using L15940,   plowkey$
            if plowkey$ = " " then L15970
            call "READ100" (#50, plowkey$, f1%(50%))
            put #50 using L15940, str(plowkey$,1%,9%)
L15940:         FMT CH(9)
            if f1%(50%) = 0% then write #50

L15970:     str(plowkey$,10%,10%) = hex(ff)
            goto L15860

        delete_customer

            readkey$ = cuscode$
            call "REDALT1" (#04, readkey%, 2%, f1%(4%))
            if f1%(4%) = 0% then L16080
                askmsg$ = "WARNING, " & cuscode$                         ~
                                   & " exists in the Sales Shadow File."
                goto L16180
L16080:     call "REDALT1" (#05, readkey%, 2%, f1%(5%))
            if f1%(5%) = 0% then L16135
                askmsg$ = "WARNING, " & cuscode$                         ~
                                   & " exists in the MLQ Shadow File."
                goto L16180
            /* Delete Customer/Price Records */
L16135:     call "SHOSTAT" (cuscode$ & " being Removed from Table ")
L16140:     call "DELETE" (#01, readkey$, 9%)

            goto inputmode

L16180:     /* Check with user if we should continue with delete */
L16190:     ask% = 2%
            call "ASKUSER" (ask%, " **** DELETE WARNING **** ",          ~
                            askmsg$,                                     ~
                            "Press PF(1) to STOP Delete Procedure ",     ~
                            "Press PF(16) to Delete " & cuscode$ &       ~
                            " from Price Table")
             if ask% = 1% then goto L10130 /* back inside input loop */
             if ask% <> 16% then L16190
             goto L16140     /* Delete Records */

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Customer Code          */~
                              L20150          /* Default Date           */
            return

L20100: REM Def/Enable Customer Code               CUSCODE$

            return

L20150: REM Def/Enable Default Date                DEF_DATE$
            if def_date$ = " " or def_date$ = blankdate$ then def_date$ = date$
            return

        deffn'052(fieldnr%, apos%)
            enabled% = 1%
            on fieldnr% gosub L20300,         /* Precious Metal Price   */~
                              L20400          /* Precious Metal Date    */

            return
L20300: REM Def/Enable Precious Metal Price        PMPRICE$
            return

L20400: REM Def/Enable PM Effective Date           PMDATE$
            if pmprice$(apos%) <> " " then L20450
                pmdate$(apos%) = " "
                enabled% = 0%
            return
L20450:     if pmdate$(apos%) = " " or ~
               pmdate$(apos%) = blankdate$ then pmdate$(apos%) = def_date$
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
         "Enter Customer Code (Blank for Default Pricing)              ",~
         "Enter Default PM Effective Start Date                      "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28180
                inpmessage$ = edtmessage$
                return

L28180
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter PM Effective Price                                     ",~
         "Enter PM Effective Start Date                                "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      cuscode$, cus_descr$, pmcode$(), pmdescr$(),       ~
                      pmprice$(), pmdate$(), pmlastprc$(), pmlastdate$(),~
                      uom$(), warnmsg$, def_date$
            lastcuscode$ = "FIRSTTIME"
            mat pmprice      =  zer
            d% = 1%
            descrflag% = 1%
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
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
        load_prices
            init(" ") pmcode$(), pmdescr$(), pmprice$(), pmdate$(),      ~
                      pmlastprc$(), pmlastdate$(), uom$(), warnmsg$

            mat pmprice      =  zer
            mat pmlastprc    =  zer
            d% = 1%
            c%,p%  = 0%

            readkey$ =  hex(00)
            call "READ102" (#02, readkey$, f1%(2%))
            goto L30180
         loop_pmcodes
            call "READNEXT" (#02, f1%(2%))
L30180:        if f1%(2%) = 0% then L30320     /* Return */
            c% = c% + 1%
            get #02 using L30210, pmcode$(c%), pmdescr$(c%), uom$(c%)
L30210:         FMT CH(10), CH(30), CH(4)
            if pmcode$(c%) <> " " then L30220
                 c% = c% - 1%
                 goto loop_pmcodes
L30220:     plowkey$ = str(cuscode$) & str(pmcode$(c%)) & all(hex(00))
            call "PLOWNEXT"(#01, plowkey$, 19%, f1%(1%))
            if f1%(1%) = 0% then loop_pmcodes
            get #01 using L30260,pmlastdate$(c%),pmlastprc(c%)
L30260:        FMT POS(26), CH( 6), PD(14,7)
            call "CONVERT" (pmlastprc(c%), 4.7, pmlastprc$(c%))
            call "DATEFMT" (pmlastdate$(c%))

            goto loop_pmcodes

L30320:     p% = c%
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput

            if p% < 1% then return   /* Nothing to do */
            for i% = 1% to p%
               /* Check if we should Update Price Table */
                if pmprice$(i%) = " " then L31270  /* Next I */
                if pmdate$(i%)  = " " or ~
                   pmdate$(i%)  = blankdate$ then L31270  /* Next I */
                convert pmprice$(i%) to pmprice,  data goto L31270
                eff_date$ = pmdate$(i%)
                call "DATUNFMT" (eff_date$)

               /* Update the Precious Metal Price/Customer Table */

                rev_date$ = eff_date$ xor hex(ffffffffffff)
                readkey$ = str(cuscode$) & str(pmcode$(i%)) & rev_date$
                call "READ101" (#01, readkey$, f1%(1%))

                put #01 using L35030, cuscode$, pmcode$(i%), rev_date$,   ~
                                  eff_date$, pmprice, uom$(i%), userid$, ~
                                  sdate$, " "
                if f1%(1%) = 0% then write #01 else rewrite #01

L31270:     next i%

            /* Add to Customer Workfile */
            readkey$ = str(cuscode$)
            call "READ101" (#50, readkey$, f1%(50%))
            if f1%(50%) = 1% then return   /* Already there */
            write #50 using L31340, readkey$
L31340:         FMT CH(9)

            return


        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: HNYPMPRC                          */~
            CH( 9),         /* Customer Code                           */~
            CH(10),         /* Precious Metal Item Code                */~
            CH(6),          /* Reverse effective date 100000 - yymmdd  */~
            CH(6),          /* Effective date                          */~
            PD(14,7),       /* Precious metal effective price          */~
            CH(4),          /* Unit of Measure                         */~
            CH(3),          /* Definition of Type USER (user ids)      */~
            CH(6),          /* Date record last changed                */~
            CH(48)          /* Unused Space                            */~

        FMT                 /* FILE: PMCODES                           */~
            CH(10),         /* Precious Metal Item Code                */~
            CH(30),         /* Precious Metal Item Description         */~
            CH(04),         /* Precious Metal Unit of Measure          */~
            CH(56)          /* Filler                                  */

        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************
        generate_report
            call "SHOSTAT" ("Report Generation in Progress")
            call "COMPNAME" (12%, company$, 0%)
            rpttitle$ = "Precious Metal Pricing Table"
            call "FMTTITLE" (rpttitle$, " ", 12%)
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("HNY061", " ", 0%, 0%)
            pcntr% =  0% : lcntr% = 99% /* Page & Line Counters */
            return

        print_lines
            get #01 using L36200, cuscode$, pmcode$, pmdate$, pmprice, uom$

L36200:     FMT CH(09), CH(10), POS(26), CH(6), PD(14,7), CH(4)

            call "DESCRIBE" (#02, pmcode$, pmdescr$, 0%, f1%(2%))
            call "DATEFMT" (pmdate$)
            call "CONVERT" (pmprice, 4.7, pmprice$)

            if cuscode$ = tempcuscode$ then L36330
                if lcntr% > 54% then gosub page_head
                if cuscode$ <> " " then L36270
                    tempdescr$ = " Default Customer PM Prices"
                    goto L36275
L36270:         call "DESCRIBE" (#03, cuscode$, tempdescr$, 0%, f1%(3%))
L36275:         print
                print using L60470, cuscode$ , tempdescr$
                print using L60270                  /* Sub-header line 1 */
                print using L60380                  /* Sub-header line 2 */
                print using L60400                  /* Sub-header line 3 */
                lcntr%    = lcntr% + 5%
                pm_cntr%  = 0%
                tempcuscode$ = cuscode$

L36330:     if lcntr% < 61% then L36390
                gosub page_head
                print
                print using L60470, "Continued ...", " "
                print using L60270                  /* Sub-header line 1 */
                print using L60380                  /* Sub-header line 2 */
                print using L60400                  /* Sub-header line 3 */
                lcntr%    = lcntr% + 5%

L36390:     if pm_cntr% > 0% and pmcode$ <> pmcode_last$ then L36400      ~
                                                         else L36415
L36400:         print
                lcntr% = lcntr% + 1%
L36415:     print using L60430, pmcode$, pmdescr$, pmprice$, pmdate$, uom$
            lcntr% = lcntr% + 1%
            pm_cntr% = pm_cntr% + 1%
            pmcode_last$ = pmcode$
            return

        end_report                /* Report Ending Routine */
            if pcntr% <> -1 then L36570
                keyhit% = 2%
                call "ASKUSER" (keyhit%, "****",                         ~
                    "SORRY, no records for Reporting were found",        ~
                    " ",                                                 ~
                    "Press RETURN to Acknowledge and Exit.")
                return
L36570:     print skip(2)
            print using L60310     /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            return

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "PMPRCINP"
            print using L60110, rpttitle$, pcntr%
            print
            lcntr% = 4%
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%, mod%)
              if mod% > 0% then L40080
                  gosub'050(1%, fieldnr%)
                  goto L40100
L40080:       gosub'060(1%, fieldnr%)

L40100:       gosub set_pf1
              gosub set_descr_screen
              if fieldnr% > 0% then init(hex(8c)) lfac$(), lfac1$()      ~
                               else init(hex(86)) lfac$(), lfac1$()
              if mod% > 0% or fieldnr% = 0%  then L40210
              on fieldnr% gosub L40190,         /* Customer          */   ~
                                L40190          /* PM Effective Date */
              goto L40300

L40190:          lfac1$(fieldnr%) = hex(81): return  /* Upper Only */

L40210:       on fieldnr% gosub L40280,         /* PM Price          */   ~
                                L40270          /* PM Date           */

              goto L40300

                  lfac$(zln%,fieldnr%) = hex(80): return  /* Up / Low   */
L40270:           lfac$(zln%,fieldnr%) = hex(81): return  /* Upper Only */
L40280:           lfac$(zln%,fieldnr%) = hex(82): return  /* Numeric    */

L40300:     accept                                                       ~
               at (01,02),                                               ~
                  "Precious Metal Pricing Table",                        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Customer Code",                              ~
               at (05,17), fac(lfac1$(1%)), cuscode$            , ch(09),~
               at (05,43), fac(hex(8c))  , cus_descr$           , ch(30),~
                                                                         ~
               at (06,02), "Default Date ",                              ~
               at (06,17), fac(lfac1$(2%)), def_date$           , ch(08),~
                                                                         ~
               at (07,43), fac(hex(84)), warnmsg$               , ch(79),~
                                                                         ~
               at (08,02), fac(hex(ac)), lines_hdr$             , ch(79),~
                                                                         ~
               at (09,02), fac(hex(8c))      , pmcode$  (d%    ), ch(10),~
               at (09,13), fac(hex(8c))      ,   descr$ (d%    ), ch(28),~
               at (09,42), fac(lfac$( 1%,1%)), pmprice$ (d%    ), ch(10),~
               at (09,53), fac(lfac$( 1%,2%)), pmdate$  (d%    ), ch( 8),~
               at (09,62), fac(hex(8c)),    pmlastprc$  (d%    ), ch(10),~
               at (09,73), fac(hex(8c)),    pmlastdate$ (d%    ), ch( 8),~
                                                                         ~
               at (10,02), fac(hex(8c))      , pmcode$  (d% +1%), ch(10),~
               at (10,13), fac(hex(8c))      ,   descr$ (d% +1%), ch(28),~
               at (10,42), fac(lfac$( 2%,1%)), pmprice$ (d% +1%), ch(10),~
               at (10,53), fac(lfac$( 2%,2%)), pmdate$  (d% +1%), ch( 8),~
               at (10,62), fac(hex(8c)),    pmlastprc$  (d% +1%), ch(10),~
               at (10,73), fac(hex(8c)),    pmlastdate$ (d% +1%), ch( 8),~
                                                                         ~
               at (11,02), fac(hex(8c))      , pmcode$  (d% +2%), ch(10),~
               at (11,13), fac(hex(8c))      ,   descr$ (d% +2%), ch(28),~
               at (11,42), fac(lfac$( 3%,1%)), pmprice$ (d% +2%), ch(10),~
               at (11,53), fac(lfac$( 3%,2%)), pmdate$  (d% +2%), ch( 8),~
               at (11,62), fac(hex(8c)),    pmlastprc$  (d% +2%), ch(10),~
               at (11,73), fac(hex(8c)),    pmlastdate$ (d% +2%), ch( 8),~
                                                                         ~
               at (12,02), fac(hex(8c))      , pmcode$  (d% +3%), ch(10),~
               at (12,13), fac(hex(8c))      ,   descr$ (d% +3%), ch(28),~
               at (12,42), fac(lfac$( 4%,1%)), pmprice$ (d% +3%), ch(10),~
               at (12,53), fac(lfac$( 4%,2%)), pmdate$  (d% +3%), ch( 8),~
               at (12,62), fac(hex(8c)),    pmlastprc$  (d% +3%), ch(10),~
               at (12,73), fac(hex(8c)),    pmlastdate$ (d% +3%), ch( 8),~
                                                                         ~
               at (13,02), fac(hex(8c))      , pmcode$  (d% +4%), ch(10),~
               at (13,13), fac(hex(8c))      ,   descr$ (d% +4%), ch(28),~
               at (13,42), fac(lfac$( 5%,1%)), pmprice$ (d% +4%), ch(10),~
               at (13,53), fac(lfac$( 5%,2%)), pmdate$  (d% +4%), ch( 8),~
               at (13,62), fac(hex(8c)),    pmlastprc$  (d% +4%), ch(10),~
               at (13,73), fac(hex(8c)),    pmlastdate$ (d% +4%), ch( 8),~
                                                                         ~
               at (14,02), fac(hex(8c))      , pmcode$  (d% +5%), ch(10),~
               at (14,13), fac(hex(8c))      ,   descr$ (d% +5%), ch(28),~
               at (14,42), fac(lfac$( 6%,1%)), pmprice$ (d% +5%), ch(10),~
               at (14,53), fac(lfac$( 6%,2%)), pmdate$  (d% +5%), ch( 8),~
               at (14,62), fac(hex(8c)),    pmlastprc$  (d% +5%), ch(10),~
               at (14,73), fac(hex(8c)),    pmlastdate$ (d% +5%), ch( 8),~
                                                                         ~
               at (15,02), fac(hex(8c))      , pmcode$  (d% +6%), ch(10),~
               at (15,13), fac(hex(8c))      ,   descr$ (d% +6%), ch(28),~
               at (15,42), fac(lfac$( 7%,1%)), pmprice$ (d% +6%), ch(10),~
               at (15,53), fac(lfac$( 7%,2%)), pmdate$  (d% +6%), ch( 8),~
               at (15,62), fac(hex(8c)),    pmlastprc$  (d% +6%), ch(10),~
               at (15,73), fac(hex(8c)),    pmlastdate$ (d% +6%), ch( 8),~
                                                                         ~
               at (16,02), fac(hex(8c))      , pmcode$  (d% +7%), ch(10),~
               at (16,13), fac(hex(8c))      ,   descr$ (d% +7%), ch(28),~
               at (16,42), fac(lfac$( 8%,1%)), pmprice$ (d% +7%), ch(10),~
               at (16,53), fac(lfac$( 8%,2%)), pmdate$  (d% +7%), ch( 8),~
               at (16,62), fac(hex(8c)),    pmlastprc$  (d% +7%), ch(10),~
               at (16,73), fac(hex(8c)),    pmlastdate$ (d% +7%), ch( 8),~
                                                                         ~
               at (17,02), fac(hex(8c))      , pmcode$  (d% +8%), ch(10),~
               at (17,13), fac(hex(8c))      ,   descr$ (d% +8%), ch(28),~
               at (17,42), fac(lfac$( 9%,1%)), pmprice$ (d% +8%), ch(10),~
               at (17,53), fac(lfac$( 9%,2%)), pmdate$  (d% +8%), ch( 8),~
               at (17,62), fac(hex(8c)),    pmlastprc$  (d% +8%), ch(10),~
               at (17,73), fac(hex(8c)),    pmlastdate$ (d% +8%), ch( 8),~
                                                                         ~
               at (18,02), fac(hex(8c))      , pmcode$  (d% +9%), ch(10),~
               at (18,13), fac(hex(8c))      ,   descr$ (d% +9%), ch(28),~
               at (18,42), fac(lfac$(10%,1%)), pmprice$ (d% +9%), ch(10),~
               at (18,53), fac(lfac$(10%,2%)), pmdate$  (d% +9%), ch( 8),~
               at (18,62), fac(hex(8c)),    pmlastprc$  (d% +9%), ch(10),~
               at (18,73), fac(hex(8c)),    pmlastdate$ (d% +9%), ch( 8),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L41290
                  call "MANUAL" ("PMPRCINP") : goto L40300

L41290:        if keyhit% <> 15% then L41320
                  call "PRNTSCRN" : goto L40300

L41320:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41610     /*  Input Mode             */
            pf$(1) = "(1)Start Over (4)Prev Field (11)Toggle D" &        ~
                     "escr                   (13)Instructions"
            pf$(2) = "                            (12)Copy Pri" &        ~
                     "ces Table              (15)Print Screen"
            pf$(3) = "                             (14)Print Pr" &       ~
                     "ices                  (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffff0b0c0d0e0f1000)
            if mod% <> 2% then L41480
                str(pf$(3%),64%) = "(16)Edit Mode  "
                str(pf$(3%),29%,18%)= " ":str(pfkeys$,14%,1%) = hex(ff)
L41480:     if mod% <> 0% then L41530
                str(pf$(2%),29%,21%) = " " : str(pfkeys$,12%,1%) = hex(ff)
                str(pf$(1%),29%,21%) = " " : str(pfkeys$,11%,1%) = hex(ff)
                str(pf$(2%),29%,21%) = " (28)Delete Customer"
                str(pfkeys$,28%,1%) = hex(1c)
                if fieldnr% = 1% then L41530
                    str(pf$(3%),63%,17%) = " "
                    str(pfkeys$,16%,1%) = hex(ff)
L41530:     if fieldnr% > 1% or mod% > 0% then L41570
                str(pf$(1%),15%,13%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
                str(pf$(1%),29%,16%) = " " : str(pfkeys$,11%,1%) = hex(ff)
                str(pf$(2%),29%,21%) = " " : str(pfkeys$,12%,1%) = hex(ff)
L41570:     if dln% < 2% then L41590
                str(pf$(2%),29%,21%) = " " : str(pfkeys$,12%,1%) = hex(ff)
L41590:         return

L41610:     if fieldnr% = 0% then L41720
*         Edit Mode - Enabled
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

L41720
*        Edit Mode - Select Field.
            pf$(1%) = "(1)Start Over (4)Prev Page  (11)Toggle D" &       ~
                      "escr                   (13)Instructions"
            pf$(2%) = "(2)First Page (5)Next Page              " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(3)Last Page  (6)Down (7)Up (14)Print Pr" &       ~
                      "ices                   (16)Save Data   "
            pfkeys$ = hex(01020304050607ffffff0bff0d0e0f10ff00)
*        If we're at the 'top' of the line items, DISable PF(2), 4 & 6.
            if d% <> 1% then goto L41860
                str(pf$(2%),,13%), str(pf$(1%),15%,12%),                 ~
                     str(pf$(3%),15%,7%) = " "
                str(pfkeys$,2%,1%), str(pfkeys$,4%,1%),                  ~
                     str(pfkeys$,6%,1%) = hex(ff)
L41860
*        If we're at the 'end' of the line items, DISable PF(3), 5 & 7.
            if d% <> zln% then goto L41930
            if p% > sum%  then goto L41930
                str(pf$(3%),,12%), str(pf$(2%),15%,12%),                 ~
                     str(pf$(3%),23%,5%) = " "
                str(pfkeys$,3%,1%), str(pfkeys$,5%,1%),                  ~
                     str(pfkeys$,7%,1%) = hex(ff)
L41930
*        No APPEND if we already have the maximum # of line items.
            if p% < mxl% then goto L41960
                str(pf$(3%),29%,19%) = " " : str(pfkeys$,11%,1%) = hex(ff)
L41960:     return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50080,         /* Customer Code          */~
                              L50180          /* PM Default Date        */
            return

L50080: REM Test for Customer Code                CUSCODE$
            get_prices% = 0%
            if cuscode$ = lastcuscode$ then return
            if cuscode$ = " " then  L50140          /* Load Defaults */
            if cuscode$ = "?" then  cuscode$ = " "

            cus_descr$ = hex(06) & "Select Customer for Precious Metals"
            call "GETCODE" (#3, cuscode$, cus_descr$, 0%, 0.32, f1%(3%))
            if f1%(3%) = 1% then L50150
L50140:         cus_descr$ = " Default Customer PM Prices"
L50150:     lastcuscode$ = cuscode$
            get_prices% = 1%
            return

        REM Test for PM Effective Date            PMDATE$
L50180:     if def_date$ = " " or def_date$ = blankdate$ then return
            call "DATEOK" (def_date$, date%, errormsg$)

            return

        deffn'152(fieldnr%, apos%, editmode%)
            errormsg$ = " "
            on fieldnr% gosub L50300,         /* Precious Metal Price   */~
                              L50400          /* Precious Metal Date    */
            return


L50300: REM Test for Precious Metal Price         PMPRICE$
            date_dflt% = 0%
            if pmprice$(apos%) = " " then return
            call "NUMTEST" (pmprice$(apos%), 0.0, 9999999, errormsg$,    ~
                                                     -4.7, pmprice(apos%))
            if errormsg$ <> " " then return
            if editmode% <> 2% then return
                if pmdate$(apos%) <> " " and ~
                   pmdate$(apos%) <> blankdate$ then return
                     date_dflt% = 1%
                     fieldnr%, fn% = 2%
                     pmdate$(apos%) = def_date$
            return

L50400: REM Test for PM Effective Date            PMDATE$
            date_dflt% = 0%
            if pmdate$(apos%) <> " " and ~
               pmdate$(apos%) <> blankdate$ then L50425
                if editmode% <> 2% then return
                if pmprice$(apos%) = " " then return
                errormsg$ = "Date Can't be blank" : return
L50425:     call "DATEOK"(pmdate$(apos%), date%, errormsg$)

            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************~

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########: PM001

*       * Header Line 2
L60110: %                           #####################################~
        ~#################################                     PAGE: ####

L60270: %----------------------------------------------------------------~
        ~----------------
        %                         ---------------------------------------~
        ~-----------------------------------------------
L60310: %                                  * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   * * * * * * * * * *

*       * Precious Metal Price Lines

L60380: %    PM Code      PM Description                    Price   Eff D~
        ~ate   UOM
L60400: %    ----------   --------------------------   ----------   -----~
        ~---   ----

L60430: %    ##########   ##########################   ##########   #####~
        ~###   ####

L60470: % Customer Code: #########################  Descr: ##############~
        ~################
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
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            call "FILEBGON" (#50)

            end
