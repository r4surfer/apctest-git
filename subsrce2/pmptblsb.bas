        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  PPPP   M   M  PPPP   TTTTT  BBBB   L       SSS   BBBB    *~
            *  P   P  MM MM  P   P    T    B   B  L      S      B   B   *~
            *  PPPP   M M M  PPPP     T    BBBB   L       SSS   BBBB    *~
            *  P      M   M  P        T    B   B  L          S  B   B   *~
            *  P      M   M  P        T    BBBB   LLLLL   SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PMPTBLSB - Input the Part/ Precious Metal Item association*~
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
            * 11/17/94 ! Original                                 ! RJ1 *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**


        sub "PMPTBLSB" (in_part$,        /* CMS Part                   */~
                        #3,              /* HNYMASTR File              */~
                        #6,              /* GENCODES File              */~
                        mode$)           /* 'D'elete, 'S'tndrd Add/View*/

        dim                                                              ~
            askmsg1$79, askmsg2$79,      /* Askuser Message            */~
            askmsg3$79,                  /* Askuser Message            */~
            company$60,                  /* Company Name               */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            in_part$25,                  /* Part Number from caller    */~
            lastpart$25,                 /* Part Number                */~
            lfac$(12,20)1,               /* Field Attribute Characters */~
            lfac1$1,                     /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lines_hdr$79,                /* Screen Lines header string */~
            part$25,                     /* Part Number                */~
            part_org$25,                 /* Part Number(1st for report)*/~
            partdescr$32,                /* Description                */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            plowdescr$79,                /*  Plowcode Description      */~
            pmcode$(100)10, pmcode$10,   /* Precious Metal             */~
            pmdescr$(100)30, pmdescr$30, /* PM Description             */~
            pmfactor$(100)10,pmfactor$10,/* Factor                     */~
            pmfactor(100),               /* Factor                     */~
            pmqty$(100)10,pmqty$10,      /* Quantity per               */~
            pmqty(100),                  /* Quantity per               */~
            protect_uom%(100),           /* UOM Protection Flag        */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rpttitle$80,                 /* Report title block         */~
            sdate$8,                     /* Save Date for File         */~
            so_line$19,                  /* Sales Order Number and line*/~
            temppart$25,                 /* Part Number for printing   */~
            tempdescr$32,                /* Part Descr  for printing   */~
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
            * #01 ! HNYPMTBL ! Precious metal code table                *~
            * #02 ! PMCODES  ! Precious metal codes file                *~
            * #03 ! HNYMASTR ! Inventory Master File                    *~
            * #04 ! BCKPMSLD ! Precious Metal SO Shadow File            *~
            * #05 ! MLQPMSLD ! Precious Metal MLQ Shadow File           *~
            * #06 ! GENCODES ! General Codes File                       *~
            * #07 ! BCKLINES ! Back Log Line Item File                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYPMTBL",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos = 1,    keylen = 35,                      ~
                        alt key  1, keypos =   26, keylen =  10, dup

            select #02, "PMCODES",                                       ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos = 1,    keylen =  10                      ~

*          SELECT #03, "HNYMASTR",                                      ~
*                      VARC,     INDEXED,  RECSIZE =  900,              ~
*                      KEYPOS =    1, KEYLEN =  25,                     ~
*                      ALT KEY  1, KEYPOS =  102, KEYLEN =   9, DUP,    ~
*                          KEY  2, KEYPOS =   90, KEYLEN =   4, DUP,    ~
*                          KEY  3, KEYPOS =   26, KEYLEN =  32, DUP

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

*          SELECT #06, "GENCODES",                                      ~
*                      VARC, INDEXED, RECSIZE = 128,                    ~
*                      KEYPOS =    1,  KEYLEN = 24

            select #07,  "BCKLINES",                                     ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            if in_part$ = " " then                                       ~
                call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 100%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 100%, rslt$(02%))
            call "OPENCHCK" (#04, fs%(04%), f2%(04%),   0%, rslt$(04%))
            call "OPENCHCK" (#05, fs%(05%), f2%(05%),   0%, rslt$(05%))
            call "OPENCHCK" (#07, fs%(07%), f2%(07%),   0%, rslt$(07%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            sdate$ = date$
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "PMPTBLSB: " & str(cms2v$,,8)
            lines_hdr$ =                                                 ~
                         "  PM Code     Description                     "~
                       & "UOM     Quantity Per  # Parts"

            mxl% = 100%    /* Max Number of PM Items per Part */
            sum% =  10%    /* Max Number of PM Items per Screen */

            if mode$ = "D" then goto delete_part_reference

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            if in_part$ = " " then L10105
                 part$ = in_part$
                 gosub load_part
                 goto editpg1

L10105:     fieldnr% = 1%
                gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%, 0%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  8% then gosub select_part_from_table
                           if selected_part% = 1% then L10230
                      if keyhit%  = 14% then gosub print_pm_list
                      if keyhit% = 16%  then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130

            fieldnr% = 0%
            inpmessage$ = " "

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            goto display_pm_lines
L11090:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% >  1% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
L11120:     gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11140:     gosub'101(fieldnr%, 2%, 0%) /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <> 28% then L11165
                      gosub delete_part_from_table
                      if ok% = 1% then goto inputmode
L11165:           if keyhit% <>  0% then L11140
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11140
                  lastfieldnr% = fieldnr%
            goto L11090


        display_pm_lines
            d% = 1%                    /* Begin at the first line item */
            gosub compute_zln
        display_pm_lines_2
            lastfieldnr% = 0%
            gosub'101(0%, 2%, 2%)
            if keyhit% =  1% then gosub startover
            if keyhit% =  2% then d% = 1%                /* First page */
            if keyhit% =  3% then d% = zln%               /* Last page */
            if keyhit% =  4% then d% = max(1%, d% - sum%)   /* Prev pg */
            if keyhit% =  5% then d% = min(zln%, d% + sum%) /* Next pg */
            if keyhit% =  6% then d% = max(1%, d% - 1%)        /* Down */
            if keyhit% =  7% then d% = min(zln%, d% + 1%)        /* Up */
            if keyhit% =  9% then goto editpg1          /* Edit Header */
            if keyhit% = 11% then goto append_pm_items
            if keyhit% = 12% then goto delete_pm_line_or_pmcode
            if keyhit% = 14% then gosub print_pm_list
            if keyhit% <> 28% then L11390
                gosub delete_part_from_table
                  if ok% = 1% then goto inputmode
L11390:     if keyhit% = 16% then goto datasave
            if keyhit% <> 0% then goto display_pm_lines_2
L11410
*        User wants to edit an existing line item, based on DLN%.
            gosub call_screen
            fieldnr% = cursor%(1%) - 4%
            if fieldnr% = 1% then goto L11120    /* Header Edit */
            dx% = cursor%(1%) -  7%             /* Screen line to edit */
            if dx% < 1% or dx% > sum% then goto display_pm_lines_2
            if dx% + d% - 1% > p% then goto display_pm_lines_2
            if dx% = lastfieldnr% then goto display_pm_lines_2
               fn% = 5%
               if cursor%(2%) < 65% then fn% = 4%
               if cursor%(2%) < 52% then fn% = 3%
               if cursor%(2%) < 45% then fn% = 2%
               if cursor%(2%) < 15% then fn% = 1%
            zln% = dx%
            dln% = dx% + d% - 1%   /* Array Element # to Edit */
L11560:     gosub'052(fn%, dln%)
L11570:     gosub'101(fn%, 1%, 2%)
            if keyhit% =  1% then gosub startover
            if keyhit% =  4% then goto L11620   /* Validate & Prev Line */
            if keyhit% =  5% then goto L11620   /* Validate & Next Line */
            if keyhit% <> 0% then goto L11570
L11620:     gosub'152(fn%, dln%)      /* Edit Fields for Valid Entry  */
            if errormsg$ <> " " then goto L11570
            lastfieldnr% = dx%
            save_lines% = 1%        /* Something has been done so save */
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

        append_pm_items
            if p% >= mxl% then goto append_pm_items_exit_2   /* Max'd? */
            p% = p% + 1%   /* No- Get the array element # for the item */
            gosub  compute_zln
            warnmsg$ = " "

        append_pm_items_2                   /* Restart Line comes here */
            gosub clear_pm_item

            for fieldnr% = 1% to 5%
L11870:         gosub'052(fieldnr%, p%)    /* Default / Enables */
                     if enabled% = 0% then goto L12010
L11890:         gosub'101(fieldnr%, 1%, 2%)/* Display / Accept  */
                     if keyhit%  = 1% then gosub startover
                     if keyhit%  = 2% then goto append_pm_items_2
                     if keyhit% <> 4% then goto L11980
L11930:                   fieldnr% = max(1%, fieldnr% - 1%)
                          gosub'052(fieldnr%, p%)
                          if enabled% = 1% then goto L11980
                          if fieldnr% = 1% then goto L11870
                          goto L11930
L11980:              if keyhit% =  16% and fieldnr% = 1%                 ~
                          then goto append_pm_items_exit
                     if keyhit% <>  0% then goto L11890
L12010:         gosub'152(fieldnr%, p%) /* Edit Field for Valid Entry */
                     if errormsg$ <> " " then goto L11890
            next fieldnr%
            save_lines% = 1%        /* Something has been done so save */
            goto append_pm_items

        append_pm_items_exit
            gosub clear_pm_item
            p% = p% - 1%
        append_pm_items_exit_2
            gosub compute_zln
            errormsg$ = " "
            goto display_pm_lines

        REM *************************************************************~
            *     B I G  D E A L  O T H E R   S U B R O U T I N E S     *~
            *************************************************************

        delete_pm_line_or_pmcode
            gosub call_screen
            dx% = cursor%(1%) -  7%             /* Screen line to edit */
            if dx% < 1% or dx% > sum% then goto display_pm_lines_2
            if dx% + d% - 1% > p% then goto display_pm_lines_2
            dln% = dx% + d% - 1%          /* Array Element # to Delete */

            gosub check_pm_inuse
            if ok% = 1% then L14140 else goto display_pm_lines_2

L14140:     call "SHOSTAT" ("Deleting " &  pmcode$(dln%) & " from Table")
            plowkey$ = str(part$) & pmcode$(dln%)
            call "DELETE" (#01, plowkey$, 35%)  /* Remove from PM Table */
            if ask% <> 28% then L14180    /* Not Asked to delete PM Code */
                 plowkey$ =  pmcode$(dln%)
                 call "DELETE" (#02, plowkey$, 10%)/*Remove from PMCodes*/

L14180:     if dln% >= p% then goto L14280 /* Must be last element */
            for i% =  dln% to p%
                 pmcode$(i%)    = pmcode$  (i% + 1%)
                 pmdescr$(i%)   = pmdescr$ (i% + 1%)
                 pmqty$(i%)     = pmqty$   (i% + 1%)
                 pmqty (i%)     = pmqty    (i% + 1%)
                 pmfactor$(i%)  = pmfactor$(i% + 1%)
                 uom$(i%)       = uom$     (i% + 1%)
            next i%

L14280:     gosub clear_pm_item
            p% = p% - 1%
            save_lines% = 1%        /* Something has been done so save */

            goto display_pm_lines_2

        delete_part_from_table
            ok% = 0%

            gosub check_pm_inuse
            if ok% = 0% then return

            call "DELETE" (#01, part$, 25%)
            return

        print_pm_list
            part_org$ = part$
            gosub generate_report
            temppart$ = " "
            plowkey$  = hex(00)
L14470:     call "PLOWNEXT" (#01, plowkey$, 0%, f1%(1%))
            if f1%(1%) = 0% then L14520
            gosub print_lines
            goto L14470

L14520:     part$ = lastpart$
            gosub  end_report
            part$ = part_org$
            return

        check_pm_inuse
            gosub check_pm_used_at_all
            ok% = 0% : so_conflict% = 0%
            askmsg3$ = "Press PF(16) to Continue Delete Procedure "
            readkey$ = part$
            call "REDALT0" (#04, readkey%, 1%, f1%(4%)) /* BCKPMSLD */
            if f1%(4%) = 0% then L14680
                askmsg1$ = "WARNING, " & part$                           ~
                                   & " exists in the Sales Shadow File."
                askmsg2$ =  "Press PF(1) to STOP Delete Procedure "  &   ~
                            "Press PF(8) to Check SO's for Open Qty"
                so_conflict% = 1%
                goto L14820
L14680:     call "REDALT0" (#05, readkey%, 2%, f1%(5%))   /* MLQPMSLD */
            if f1%(5%) = 0% then L14750    /* Must be OK To Delete */
                askmsg1$ = "WARNING, " & part$                           ~
                                   & " exists in the MLQ Shadow File."
                askmsg2$ =  "Press PF(1) to STOP Delete Procedure "
                goto L14820

L14750:     askmsg1$ = "Last Chance, Are You Sure You Want to Delete Part~
        ~ from PM Table."
            if keyhit% = 12% then askmsg1$ = "Last Chance, Are You Sure Y~
        ~ou Want to Delete PM Item from List"
            askmsg2$ = "Press PF(1) to STOP Delete Procedure "
            if pm_used_somewhere% =  1% then  L14820 /* Allow Line Delete*/
                askmsg3$ = "PF(16) to Delete PM Line Item or PF(28) to De~
        ~lete PM Code Completely."
            goto L14820

L14820:     /* Check with user if we should continue with delete */
L14830:     ask% = 3%   /* Middle of Screen */
            call "ASKUSER" (ask%, " **** DELETE WARNING **** ",          ~
                            askmsg1$, askmsg2$, askmsg3$)

             if ask% = 1% then return
             if ask% = 8% and so_conflict% = 1% then L14940 /* Check SO's*/
             if ask% = 28% then L14910
             if ask% <> 16% then L14830
L14910:      ok% = 1%
             return

L14940:     /* Loop thru SO's to check for Open Qty Conflicts */
            so_conflict% = 0%
            readkey$ = part$
            call "REDALT0" (#04, readkey$, 1%, f1%(4%))
L14980:         if f1%(4%) = 0% then L15130              /* No Open Qty */
                if key(#04, 1%) <> readkey$ then L15130  /* No Open Qty */
            get #04 using L15010, so_line$
L15010:        FMT CH(19)
            call "READ100" (#07, so_line$, f1%(7%))
                if f1%(7%) = 0% then L15100    /* No hit, try again */
            get #07 using L15050, openqty
L15050:        FMT POS(109), PD(14,4)
            if openqty = 0.0 then L15100    /* Not a problem, try again */
                so_conflict% = 1%      /* Problem w/ open qty */
                goto L15130

L15100:     call "READNEXT" (#04, f1%(4%))
            goto L14980

L15130:     /* Check with user if we should continue with delete */
            if so_conflict% = 0% then L15190
                askmsg1$ = " **** DELETE WARNING **** "
                askmsg2$ = "Open Quantity exists on at least one " &     ~
                              " Sales Order"
                goto L15220
L15190:     askmsg1$ = " **** DELETE MESSAGE **** "
            askmsg2$ = "Delete OK, No Open Quantity exists on any SO's"

L15220:     ask% = 3%   /* Middle of Screen */
            call "ASKUSER" (ask%, askmsg1$, askmsg2$,                    ~
                            "Press PF(1) to Stop Delete Procedure ",     ~
                            "Press PF(16) Continue Delete Procedure ")

             if ask% = 1% then return
             if ask% <> 16% then L15220
             ok% = 1%

             return

        check_pm_used_at_all
            pm_used_somewhere% = 0%
            readkey$ = pmcode$(dln%)
            call "REDALT0" (#4, readkey$, 3%, f1%(4%)) /* BCKPMSLD */
            if f1%(4%) = 0% then L15390
L15380:         pm_used_somewhere% = 1%  :  return
L15390:     call "REDALT0" (#5, readkey$, 3%, f1%(5%)) /* MLQPMSLD */
            if f1%(5%) = 0% then return else L15380

        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *************************************************************

        clear_pm_item                     /* Clear 1 element of arrays */
            init(" ") errormsg$, inpmessage$, pmcode$(p%), pmdescr$(p%), ~
                pmfactor$(p%), pmqty$(p%), uom$(p%)
            pmfactor(p%), pmqty(p%) = 0
            return

        compute_zln
            dln% = p%    /* Array Elemnent */
            d%   = max(1%, p% - sum% + 1%)   /* SUM% is Set in 9000's  */
            zln% = p% - d% + 1%
            return

        call_screen
            call "GETSCRN" ("C", " ", cursor%(), 0%)
            return

        select_part_from_table
            selected_part% = 0%
            plowkey$ = str(part$)  &  hex(00)
            plowdescr$ = "Select Precious Metal Part Number."
            call "PLOWCODE" (#01, plowkey$, plowdescr$,                  ~
                                                   -25%, -0.001, f1%(1%))
*          CALL "GETCODE" (#01, PLOWKEY$, PLOWDESCR$, 0, 0.0, F1%(1%))
            if f1%(1%) = 0% then return
            part$ = plowkey$
            selected_part% = 1%
            return

        delete_part_reference    /* Straight delete from table with no */
            part$ = in_part$                         /* User interface */
            if part$ = " " then exit_program
            gosub delete_part_from_table
            goto exit_program

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            if in_part$ <> " " then exit_program   /* Restricted to IN */
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100          /* Part Number            */

            return
L20100: REM Def/Enable Part Number                 PART$
            return

        deffn'052(fieldnr%, dln%)
            enabled% = 1%
            on fieldnr% gosub L20300,         /* Precious Metal         */~
                              L20400,         /* PM Description         */~
                              L20500,         /* UOM                    */~
                              L20600,         /* Quantity per           */~
                              L20700          /* Factor                 */
            return
L20300: REM Def/Enable Precious Metal              PMCODE$
            return

L20400: REM Def/Enable PM Description              PMDESCR$
            return

L20500: REM Def/Enable UOM                         UOM$
            if protect_uom%(dln%) = 1% then enabled% = 0%
            return

L20600: REM Def/Enable Quantity per                PMQTY$
            return

L20700: REM Def/Enable Factor                      PMFACTOR$
            if pmfactor$(dln%) = " " then pmfactor$(dln%) = "1000"
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
         "Enter Part Number (? for Master List, PF8 for PM List)       ",~
         "Enter Precious Metal                                         ",~
         "Enter PM Description                                         ",~
         "Enter UOM                                                    ",~
         "Enter Quantity per                                           ",~
         "Enter Factor                                                 "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      part$, partdescr$, pmcode$(), pmdescr$(),          ~
                      pmfactor$(), pmqty$(), uom$(), warnmsg$, lastpart$

            mat pmfactor     =  con   /* Set to one */
            mat pmqty        =  zer
            d%               =  1%
            save_lines%      =  0%    /* PMs changed or appended Flag */

            selected_part% = 0%
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
        load_part
            init(" ") pmcode$(), pmdescr$(), pmfactor$(), pmqty$(),      ~
                      uom$(), warnmsg$, lastpart$

            mat pmfactor     =  con   /* Set to one */
            mat pmqty        =  zer
            d% = 1%
            c%,p%  = 0%

            plowkey$ = str(part$) & hex(00)
L30080:     call "PLOWNEXT" (#01, plowkey$, 25%, f1%(1%))
            if f1%(1%) <> 0% then L30105
               if c% > 0% then L30200
                   warnmsg$ =                                            ~
                      "No Precious Metal Items associated with this Part."
                   return

L30105:     c% = c% + 1%
            get #01 using L30130, pmcode$(c%), pmqty(c%), pmfactor(c%),   ~
                                 uom$(c%)
L30130:     FMT POS(26), CH(10), PD(14,7), PD(14,7), CH(4)
            if pmcode$(c%) <> " " then L30145
                c% = c% - 1%
                goto L30080   /* Back for more */
L30145:     call "DESCRIBE" (#02, pmcode$(c%), pmdescr$(c%), 0%, f1%(2%))
            call "CONVERT" (pmqty(c%), 4.7, pmqty$(c%))
            call "CONVERT" (pmfactor(c%), 4.7, pmfactor$(c%))

            goto L30080   /* Back for more */

           /* Table exists for part */
L30200:        p% = c%

               plowkey$ = str(part$) & hex(00)
               call "REDALT0" (#04, plowkey$, 1%, f1%(4%))  /* BCK's */
                  if f1%(4%) <> 0% then L30280          /* Warning msg */
               call "REDALT0" (#05, plowkey$, 1%, f1%(5%))  /* MLQ's */
                  if f1%(4%) <> 0% then L30280          /* Warning msg */
               return

L30280:        warnmsg$ =                                                ~
                    "WARNING - Part is used in a Sales Order or Quote."
               return


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput

            if p% < 1% then return   /* Nothing to do */
            if save_lines% = 0% then return /* Nothing to do */
            for i% = 1% to p%
               /* Update the Precious Metal Codes File */
                call "READ101" (#02, pmcode$(i%), f1%(2%))
                put #02 using L35200, pmcode$(i%), pmdescr$(i%), uom$(i%),~
                                     " "
                if f1%(2%) = 0% then write #02 else rewrite #02

               /* Update the Precious Metal/Part Table */
                pmqty = 0   :   pmfactor = 1

                plowkey$ = str(part$) & pmcode$(i%)
                call "READ101" (#01, plowkey$, f1%(1%))
                convert pmqty$(i%) to pmqty,  data goto L31160
L31160:         convert pmfactor$(i%) to pmfactor,  data goto L31170

L31170:         put #01 using L35030, part$, pmcode$(i%), pmqty, pmfactor,~
                                      uom$(i%), userid$, sdate$, " "
                if f1%(1%) = 0% then write #01 else rewrite #01

            next i%

            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: HNYPMTBL                          */~
            CH(25),         /* Part Number (Product Code)              */~
            CH(10),         /* Precious Metal Item Code                */~
            PD(14,7),       /* Precious metal quantity                 */~
            PD(14,7),       /* Multiplier to set PM Price per xxx Parts*/~
            CH(4),          /* Unit of Measure                         */~
            CH(3),          /* Definition of Type USER (user ids)      */~
            CH(6),          /* Date record last changed                */~
            CH(36)          /* Unused Space                            */~

L35200: FMT                 /* FILE: PMCODES                           */~
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
            rpttitle$ = "Precious Metal Part Table"
            call "FMTTITLE" (rpttitle$, " ", 12%)
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("HNY060", " ", 0%, 0%)
            pcntr% =  0% : lcntr% = 99% /* Page & Line Counters */
            return

        print_lines
            get #01 using L36200, part$, pmcode$, pmqty, pmfactor, uom$

L36200:     FMT CH(25), CH(10), PD(14,7), PD(14,7), CH(4)

            call "DESCRIBE" (#02, pmcode$, pmdescr$, 0%, f1%(2%))
            call "CONVERT" (pmqty, 4.7, pmqty$)
            call "CONVERT" (pmfactor, 2.2, pmfactor$)

            if part$ = temppart$ then L36390
                if lcntr% > 54% then gosub page_head
                call "DESCRIBE" (#03, part$, tempdescr$, 0%, f1%(3%))
                print
                print using L60470, part$ , tempdescr$
                print using L60270                  /* Sub-header line 1 */
                print using L60380                  /* Sub-header line 2 */
                print using L60400                  /* Sub-header line 3 */
                lcntr%    = lcntr% + 5%
                temppart$ = part$

L36390:     if lcntr% < 55% then L36410
                gosub page_head
                print
                print using L60470, "Part Continued...", " "
                print using L60270                  /* Sub-header line 1 */
                print using L60380                  /* Sub-header line 2 */
                print using L60400                  /* Sub-header line 3 */
                lcntr%    = lcntr% + 5%

L36410:     print using L60430, pmcode$, pmdescr$, pmqty$, pmfactor$, uom$
            lcntr% = lcntr% + 1%
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
            print using L60070, date$, time$, company$, "PMPTBLSB"
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
              if mod% > 0% then L40070
                  gosub'050(1%, fieldnr%)
                  goto L40090
L40070:       gosub'051(fieldnr%)

L40090:       gosub set_pf1

              if fieldnr% > 0% then init(hex(8c)) lfac$(), lfac1$        ~
                               else init(hex(86)) lfac$(), lfac1$
              if mod% > 0% or fieldnr% = 0%  then L40160
                  lfac1$ = hex(81):  goto L40270    /* Upper Only */

L40160:       on fieldnr% gosub L40240,         /* Precious Metal    */   ~
                                L40240,         /* PM Description    */   ~
                                L40240,         /* UOM               */   ~
                                L40250,         /* Quantity per      */   ~
                                L40250          /* Factor            */
              goto L40270

                  lfac$(zln%,fieldnr%) = hex(80): return  /* Up / Low   */
L40240:           lfac$(zln%,fieldnr%) = hex(81): return  /* Upper Only */
L40250:           lfac$(zln%,fieldnr%) = hex(82): return  /* Numeric    */

L40270:     accept                                                       ~
               at (01,02),                                               ~
                  "Precious Metal / Part Number Table",                  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Part Number",                                ~
               at (05,17), fac(lfac1$    ), part$               , ch(25),~
               at (05,43), fac(hex(8c))  , partdescr$           , ch(32),~
                                                                         ~
               at (05,43), fac(hex(84)), warnmsg$               , ch(79),~
                                                                         ~
               at (07,02), fac(hex(ac)), lines_hdr$             , ch(79),~
                                                                         ~
               at (08,04), fac(lfac$( 1%,1%)), pmcode$  (d%    ), ch(10),~
               at (08,16), fac(lfac$( 1%,2%)), pmdescr$ (d%    ), ch(30),~
               at (08,48), fac(lfac$( 1%,3%)), uom$     (d%    ), ch(04),~
               at (08,54), fac(lfac$( 1%,4%)), pmqty$   (d%    ), ch(10),~
               at (08,67), fac(lfac$( 1%,5%)), pmfactor$(d%    ), ch(10),~
                                                                         ~
               at (09,04), fac(lfac$( 2%,1%)), pmcode$  (d%+ 1%), ch(10),~
               at (09,16), fac(lfac$( 2%,2%)), pmdescr$ (d%+ 1%), ch(30),~
               at (09,48), fac(lfac$( 2%,3%)), uom$     (d%+ 1%), ch(04),~
               at (09,54), fac(lfac$( 2%,4%)), pmqty$   (d%+ 1%), ch(10),~
               at (09,67), fac(lfac$( 2%,5%)), pmfactor$(d%+ 1%), ch(10),~
                                                                         ~
               at (10,04), fac(lfac$( 3%,1%)), pmcode$  (d%+ 2%), ch(10),~
               at (10,16), fac(lfac$( 3%,2%)), pmdescr$ (d%+ 2%), ch(30),~
               at (10,48), fac(lfac$( 3%,3%)), uom$     (d%+ 2%), ch(04),~
               at (10,54), fac(lfac$( 3%,4%)), pmqty$   (d%+ 2%), ch(10),~
               at (10,67), fac(lfac$( 3%,5%)), pmfactor$(d%+ 2%), ch(10),~
                                                                         ~
               at (11,04), fac(lfac$( 4%,1%)), pmcode$  (d%+ 3%), ch(10),~
               at (11,16), fac(lfac$( 4%,2%)), pmdescr$ (d%+ 3%), ch(30),~
               at (11,48), fac(lfac$( 4%,3%)), uom$     (d%+ 3%), ch(04),~
               at (11,54), fac(lfac$( 4%,4%)), pmqty$   (d%+ 3%), ch(10),~
               at (11,67), fac(lfac$( 4%,5%)), pmfactor$(d%+ 3%), ch(10),~
                                                                         ~
               at (12,04), fac(lfac$( 5%,1%)), pmcode$  (d%+ 4%), ch(10),~
               at (12,16), fac(lfac$( 5%,2%)), pmdescr$ (d%+ 4%), ch(30),~
               at (12,48), fac(lfac$( 5%,3%)), uom$     (d%+ 4%), ch(04),~
               at (12,54), fac(lfac$( 5%,4%)), pmqty$   (d%+ 4%), ch(10),~
               at (12,67), fac(lfac$( 5%,5%)), pmfactor$(d%+ 4%), ch(10),~
                                                                         ~
               at (13,04), fac(lfac$( 6%,1%)), pmcode$  (d%+ 5%), ch(10),~
               at (13,16), fac(lfac$( 6%,2%)), pmdescr$ (d%+ 5%), ch(30),~
               at (13,48), fac(lfac$( 6%,3%)), uom$     (d%+ 5%), ch(04),~
               at (13,54), fac(lfac$( 6%,4%)), pmqty$   (d%+ 5%), ch(10),~
               at (13,67), fac(lfac$( 6%,5%)), pmfactor$(d%+ 5%), ch(10),~
                                                                         ~
               at (14,04), fac(lfac$( 7%,1%)), pmcode$  (d%+ 6%), ch(10),~
               at (14,16), fac(lfac$( 7%,2%)), pmdescr$ (d%+ 6%), ch(30),~
               at (14,48), fac(lfac$( 7%,3%)), uom$     (d%+ 6%), ch(04),~
               at (14,54), fac(lfac$( 7%,4%)), pmqty$   (d%+ 6%), ch(10),~
               at (14,67), fac(lfac$( 7%,5%)), pmfactor$(d%+ 6%), ch(10),~
                                                                         ~
               at (15,04), fac(lfac$( 8%,1%)), pmcode$  (d%+ 7%), ch(10),~
               at (15,16), fac(lfac$( 8%,2%)), pmdescr$ (d%+ 7%), ch(30),~
               at (15,48), fac(lfac$( 8%,3%)), uom$     (d%+ 7%), ch(04),~
               at (15,54), fac(lfac$( 8%,4%)), pmqty$   (d%+ 7%), ch(10),~
               at (15,67), fac(lfac$( 8%,5%)), pmfactor$(d%+ 7%), ch(10),~
                                                                         ~
               at (16,04), fac(lfac$( 9%,1%)), pmcode$  (d%+ 8%), ch(10),~
               at (16,16), fac(lfac$( 9%,2%)), pmdescr$ (d%+ 8%), ch(30),~
               at (16,48), fac(lfac$( 9%,3%)), uom$     (d%+ 8%), ch(04),~
               at (16,54), fac(lfac$( 9%,4%)), pmqty$   (d%+ 8%), ch(10),~
               at (16,67), fac(lfac$( 9%,5%)), pmfactor$(d%+ 8%), ch(10),~
                                                                         ~
               at (17,04), fac(lfac$(10%,1%)), pmcode$  (d%+ 9%), ch(10),~
               at (17,16), fac(lfac$(10%,2%)), pmdescr$ (d%+ 9%), ch(30),~
               at (17,48), fac(lfac$(10%,3%)), uom$     (d%+ 9%), ch(04),~
               at (17,54), fac(lfac$(10%,4%)), pmqty$   (d%+ 9%), ch(10),~
               at (17,67), fac(lfac$(10%,5%)), pmfactor$(d%+ 9%), ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L41130
                  call "MANUAL" ("PMPTBLSB") : goto L40270

L41130:        if keyhit% <> 15% then L41160
                  call "PRNTSCRN" : goto L40270

L41160:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41390     /*  Input Mode             */
            pf$(1) = "(1)Start Over (4)Prev Field             " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                              (8)Select "  &       ~
                     "PM Part                (15)Print Screen"
            pf$(3) = "                             (14)Print PM" &       ~
                     " List                 (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffff08ffffffff0d0e0f1000)
            if mod% <> 2% then L41330
                str(pf$(3%),64%) = "(16)Exit Append "
                str(pf$(3%),29%,18%)= " ":str(pfkeys$,14%,1%) = hex(ff)
                str(pf$(2%),29%,19%)= " ":str(pfkeys$, 8%,1%) = hex(ff)
L41330:     if fieldnr% = 1% then L41360
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L41370
L41360:         str(pf$(1%),15%,13%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L41370:     return

L41390: if fieldnr% = 0% then L41500
*         Edit Mode - Enabled
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

L41500
*        Edit Mode - Select Field.
            pf$(1%) = "(1)Start Over (4)Prev Page  (11)Append P" &       ~
                      "M Items                (13)Instructions"
            pf$(2%) = "(2)First Page (5)Next Page  (12/28)Delet" &       ~
                      "e PM Line/Part         (15)Print Screen"
            pf$(3%) = "(3)Last Page  (6)Down (7)Up (14)Print PM" &       ~
                      " List                  (16)Save Data   "
            if save_lines% = 1% then L41570
                str(pf$(3%),64%,13%) = "(16)Return"
L41570:     pfkeys$ = hex(01020304050607ffffff0b0c0d0e0f10ff001c)
*        If we're at the 'top' of the line items, DISable PF(2), 4 & 6.
            if d% <> 1% then goto L41640
                str(pf$(2%),,13%), str(pf$(1%),15%,12%),                 ~
                     str(pf$(3%),15%,7%) = " "
                str(pfkeys$,2%,1%), str(pfkeys$,4%,1%),                  ~
                     str(pfkeys$,6%,1%) = hex(ff)
L41640
*        If we're at the 'end' of the line items, DISable PF(3), 5 & 7.
            if d% = zln% then goto L41660
            if d% + sum% - 1% < p% then L41700
L41660:         str(pf$(3%),,12%), str(pf$(2%),15%,12%),                 ~
                     str(pf$(3%),23%,5%) = " "
                str(pfkeys$,3%,1%), str(pfkeys$,5%,1%),                  ~
                     str(pfkeys$,7%,1%) = hex(ff)
L41700
*        No APPEND if we already have the maximum # of line items.
            if p% < mxl% then goto L41730
                str(pf$(3%),29%,20%) = " " : str(pfkeys$,11%,1%) = hex(ff)
L41730:     return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50080          /* Part Number            */
            return

L50080: REM Test for Part Number                  PART$
            if part$ = lastpart$ and part$ <> " "  then return
            if part$ = "?" then  part$ = " "

            partdescr$ = hex(06) & "Select Part for Precious Metals"
            call "GETCODE" (#3, part$, partdescr$, 0%, 0.32, f1%(3%))
            if f1%(3%) = 1% then L50160
                errormsg$ = "Sorry, Part Not on File.  Please Re-enter."
                return
L50160:     gosub load_part
            lastpart$ = part$
            return

        deffn'152(fieldnr%, dln%)
            errormsg$ = " "
            on fieldnr% gosub L50300,         /* Precious Metal         */~
                              L50500,         /* PM Description         */~
                              L50560,         /* UOM                    */~
                              L50650,         /* Quantity per           */~
                              L50700          /* Factor                 */
            return


L50300: REM Test for Precious Metal               PMCODE$
            if pmcode$(dln%) = "?" then pmcode$(dln%) = " "
            call "GETCODE" (#02, pmcode$(dln%), pmdescr$(dln%), 0%, 0,   ~
                                                               f1%(2%))
            if pmcode$(dln%) <> " "  then L50340
                errormsg$ = "Blank Code not allowed"
                return
L50340:     if f1%(2%) =  0% then L50380
            get #02 using L50350, uom$(dln%)
L50350:        FMT POS(41), CH(4)
            readkey$ = str(part$) & pmcode$(dln%)
            call "READ100" (#1, readkey$, f1%(1%))
            if f1%(1%) = 0% then L50425
                errormsg$ = "Precious Metal already associated with Part"
                uom$(dln%) = " "
                return
L50380:     if p% = 1% then L50425   /* Dont bother with loop test */
                for i% = 1% to p%
                     if dln% = i% then L50410  /* Next I */
                     if pmcode$(i%) <> pmcode$(dln%) then L50410/* Next I*/
                         errormsg$ = "Precious Metal already being used"
                         i% = p% + 1%  :  uom$(dln%) = " "
L50410:         next i%
                if errormsg$ <> " " then return

L50425:     if uom$(dln%) = " " then protect_uom%(dln%) =  0%            ~
                                else protect_uom%(dln%) =  1%
            return

L50500: REM Test for PM Description               PMDESCR$
            return

L50560: REM Test for UOM                          UOM$
                if uom$(dln%) = "?" then uom$(dln%) = " "
                readkey$ = "UOM      " & uom$(dln%)
                uomdescr$ = hex(06) & "SELECT Unit of Measure"
                f1%(06%) = -9%
                call "PLOWCODE" (#06, readkey$, uomdescr$, 9%, 0.3,      ~
                                                                 f1%(06%))
                if f1%(06%) = 0% then L50610
                     uom$(dln%) = str(readkey$,10)
                     return
L50610:         errormsg$ = "Unit of Measure not on file: " & uom$(dln%)
                return

L50650: REM Test for Quantity per                 PMQTY$
            call "NUMTEST" (pmqty$(dln%), 0.0, 9999999, errormsg$, -4.7, ~
                                                             pmqty(dln%))
            return

L50700: REM Test for Factor                       PMFACTOR$
            call "NUMTEST" (pmfactor$(dln%), 0.0, 9999999, errormsg$,    ~
                                                   -2.2, pmfactor(dln%))
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************~

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########: PM002

*       * Header Line 2
L60110: %                       #########################################~
        ~#################################                     PAGE: ####

L60270: %----------------------------------------------------------------~
        ~--------------
        %                         ---------------------------------------~
        ~-----------------------------------------------
L60310: %                                  * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   * * * * * * * * * *

*       * Precious Metal Codes Lines

L60380: %    PM Code     PM Description                Quantity Per No. o~
        ~f Parts  UOM
L60400: %    ----------  --------------------------  ----------     -----~
        ~-------  ----

L60430: %    ##########  ##########################  ##########      ####~
        ~#######  ####

L60470: % Part Number: #########################  Descr: ################~
        ~##############
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
            if in_part$ = " " then  call "SHOSTAT" ("One Moment Please")

            end
