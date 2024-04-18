        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  DDDD   EEEEE  M   M  RRRR    AAA   PPPP   IIIII  DDDD    *~
            *  D   D  E      MM MM  R   R  A   A  P   P    I    D   D   *~
            *  D   D  EEEE   M M M  RRRR   AAAAA  PPPP     I    D   D   *~
            *  D   D  E      M   M  R   R  A   A  P        I    D   D   *~
            *  DDDD   EEEEE  M   M  R   R  A   A  P      IIIII  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DEMRAPID - Allows quick entry of non-sales order demands  *~
            *            by either demand code or by part code.         *~
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
            * 05/19/88 ! Original                                 ! MJB *~
            * 01/26/89 ! Don't Clobber Back Half of DEMMASTR      ! KAB *~
            * 04/25/89 ! Fixed FS95 when mod Type or Priority     ! MJB *~
            *          !   Added Default SO Priority fm HNYMASTR  !     *~
            * 07/26/90 ! Array (IDX%) Overflow                    ! KAB *~
            * 06/05/91 !(PRR 11739)                               ! RJB *~
            *          !    1. Replaced hardcoded Values for the  !     *~
            *          !       Max # of Entries & Starting Screen !     *~
            *          !       Values with (ML% & MI%) which are  !     *~
            *          !       set in the 'INITALIZATION' section !     *~
            *          !    2. Corrected setting of 'MAXLINES$ &  !     *~
            *          !       IDX%' in the 'APPEND_LINE' section !     *~
            *          !       to not exceed the max Array sizes  !     *~
            *          !    3. Added call to "ALLFREE" (New Stand)!     *~
            * 06/25/91 !QC-FIXES Added maxline checks to 'LOAD_BY ! RJB *~
            *          !    DEMAND' and 'SET_PF_KEYS' sections.   !     *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 11/11/92 ! Part input message now correct.          ! RJH *~
            *          ! PRR 12666 - Enable Line # field when     !     *~
            *          !  Inputing Part demands & then check for  !     *~
            *          !  duplicate demands before accepting.     !     *~
            *          !  If two unprocessed lines duplicate      !     *~
            *          !  demand, ErrorMsg is 'DuplicateDemLine". !     *~
            *          ! Some implied integers explicitly set.    !     *~
            * 01/19/93 ! PRR's 11433, 11628 -Added PF(3)Copy Above! RJH *~
            *          !  for Full Screen Input, Insert, & Append !     *~
            *          !  modes.                                  !     *~
            *          ! Fixed problems with Inserting&Appending--!     *~
            *          ! When editing lines for a Demand Code,    !     *~
            *          !  Append is the method of adding Parts.   !     *~
            *          ! When editing Demand Codes for a Part No.,!     *~
            *          !  Insert is the method of adding Demands. !     *~
            * 01/22/93 ! PLUS...Fix other misc. weirdnesses.      ! RJH *~
            * 03/31/93 ! PRR 12766 Invalid SEQNO$ returns to READ ! JIM *~
            *          !   instead of terminating program.        !     *~
            * 02/ 8/94 ! PRR 13104 - Null list of demands will not! RJH *~
            *          !   be processed for a Part Number.        !     *~
            * 08/05/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for comparison  */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            datereq$(100)8,              /* Required Due Date          */~
            demcode$(100)16,             /* Demand Code                */~
            demcodeselect$16,            /* Demand Code for select     */~
            demline$(100)3,              /* Demand Line Number         */~
            demstatus$(100)1,            /* Current Demand Status      */~
            edtmessage$79,               /* Edit screen message        */~
            error$1,                                                     ~
            error$(100)17,               /* ERROR MESSAGE              */~
            errormsg$79,                 /* Error message              */~
            ff$(100,8)1,                 /* FAC Array for display      */~
            fillspc$(100)50,             /* Filler - spaces            */~
            fillhex$(100)10,             /* Filler - HEX(00)           */~
            header$(2)79,                /* Column Headers             */~
            hnytype$10,                  /* Inventory part type        */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$(4)79,            /* Informational Message      */~
            insert_lines%(100),          /* Insert line numbers        */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            okflag%(100),                /* Entry Valid Flag           */~
            orig_datereq$(100)6,         /* Date Required Due          */~
            orig_qty(100),                                               ~
            orig_typ$(100)1,             /* Demand Type                */~
            orig_pri$(100)1,             /* Demand Priority            */~
            qty$(100)10,                 /* Input demand quantity      */~
            part$25,                     /* Part for read test         */~
            part$(100)25,                /* Demand Part Number         */~
            partselect$25,               /* Part Number                */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            pldate$6,                                                    ~
            prio$(100)1,                 /* Planning Priority Code     */~
            readkey$50,                  /* File read key              */~
            reqtype$1,                   /* Requested Demand Type      */~
            hnyprio$1,                   /* SO Priority fm HNYMASTR    */~
            smasharray$(100)64,          /* Array to delete blank lines*/~
            templine$(100)3,             /* Temp Dem Lines Array(Smash)*/~
            testdate$8,                  /* Date for testing           */~
            today$8,                     /* Todays Date                */~
            type$(100)1,                 /* Demand Type                */~
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
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! DEMMASTR ! Demand Master File                       *~
            * # 2 ! HNYMASTR ! Inventory Master File                    *~
            * # 3 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * # 4 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "DEMMASTR",                                      ~
                        varc,     indexed,  recsize =  123,              ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28,         ~
                            key  3, keypos =   29, keylen =  25, dup

            select # 2, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select # 3, "PIPMASTR",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26

            select # 4, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20


            call "SHOSTAT" ("Opening Files, One Moment Please")          ~

            call "OPENCHCK" (# 1, fs%( 1%), f2%( 1%), 0%, rslt$( 1%))
            call "OPENCHCK" (# 2, fs%( 2%), f2%( 2%), 0%, rslt$( 2%))
            call "OPENCHCK" (# 3, fs%( 3%), f2%( 3%), 0%, rslt$( 3%))
            call "OPENCHCK" (# 4, fs%( 4%), f2%( 4%), 0%, rslt$( 4%))

            if f2%(1%)+f2%(2%)+f2%(3%)+f2%(4%)  = 0 then initialization
            call "SHOSTAT" ("Data File(s) not found... Call your System S~
        ~ecurity Administrator")
            goto exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

        initialization
            call "EXTRACT" addr("ID", userid$)
            today$, date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62%) = "DEMRAPID: " & str(cms2v$,,8)
            maxlines% = 0%
            mat orig_qty = zer

            call "READ100" (#4, "MONTHS OPEN", f1%(4%))
                if f1%(4%) = 0% then month_open_err
            get #4, using L09200, pldate$
L09200:         FMT XX(32), CH(6)

            ml% = dim(part$(),1)   /* VARIABLE FOR MAX SCREEN ENTRIES */
            mi% = ml% - 14%        /* VARIABLE FOR MAX STARTING LINE  */

        REM *************************************************************~
            *       I N P U T   M O D E   S E L E C T I O N             *~
            *-----------------------------------------------------------*~
            * Input mode to function by demand or by part.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

L10090:     gosub'101(1%)                    /* Display / Accept  */
                if keyhit% =  1% then gosub startover
                if keyhit% = 16% then exit_program
                if keyhit% <> 0% then L10090

            gosub'151              /* Edit Fields for Valid Entry */
                if errormsg$ <> " " then L10090

            if mode% = 1% then input_by_demand
            if mode% <> 2% then L10240
                gosub load_by_demand
                if maxlines% <> 0% then L10220
                    mode% = 1%
                    goto input_by_demand

L10220:         goto edit_by_demand

L10240:     gosub load_by_part
                if mode% = 3% then input_by_part
                goto edit_by_part


        REM *************************************************************~
            *       I N P U T   M O D E   B Y   D E M A N D             *~
            *-----------------------------------------------------------*~
            * Input mode By Demand                                      *~
            *************************************************************

        input_by_demand
            idx% = 1%
L11080:     gosub'102(1%)                 /* Display / Accept  */
                if keyhit% =  1% then gosub startover
                if keyhit% <> 5% then L11132
                    if part$(idx% + 14) = " " then L11080
                        idx% = min(idx% + 15%, mi%)
                        goto L11080
L11132:         if keyhit% <> 3% then L11140
                    gosub copy_line_above_by_demand_inputmode
                    goto  L11080
L11140:         if keyhit% <> 0% then L11080

L11160:     gosub'152(1%)        /* Test For 1st time entry    */
            if maxlines% = 0% then L11080

            for j% = 1 to maxlines%
                if okflag%(j%) <> 0% then set_up_errors
            next j%
            goto edit_by_demand

        set_up_errors
            if maxlines% > 15% then idx% = j% else idx% = 1%
               idx% = min(idx%, mi%)
L11260:     gosub'102(2%)                /* Error Display & Input */
                if keyhit% =  1% then gosub startover
                if keyhit% =  2% then idx% = 1%
                if keyhit% =  3% then idx%=max(1%,min(mi%,maxlines%-14%))
                if keyhit% =  4% then idx% = max(1%,idx%-14%)
                if keyhit% =  5% then idx% =                             ~
                              max(1%, min(mi%,idx%+14%,maxlines%-14%))
                if keyhit% <> 0% then L11260
                goto L11160

        REM *************************************************************~
            *        E D I T   B Y   D E M A N D   C O D E              *~
            *-----------------------------------------------------------*~
            * Handles Edit Mode by Demand Code                          *~
            *************************************************************

        edit_by_demand
            edit% = 3%
L13080:     idx% = 1%
L13090:     gosub'102(edit%)
                errormsg$ = " "
                if keyhit% =  1% then gosub startover
                if keyhit% =  2% then idx% = 1%
                if keyhit% =  3% then idx%=max(1%,min(mi%,maxlines%-14%))
                if keyhit% =  4% then idx% = max(1%,idx%-14%)
                if keyhit% =  5% then idx% =                             ~
                              max(1%, min(mi%,idx%+14%,maxlines%-14%))
                if keyhit% < 10% or keyhit% > 12% then L13370
                    line% = cursor%(1) - 5%
                    workline% = min(line% + idx% - 1%, maxlines%)
                    if line% > 0% or keyhit% = 11% then L13240
                        errormsg$ = "Please Position Cursor First"
                        goto L13090
L13240:             if keyhit% > 10% then L13270
                        gosub reactivate
                        goto L13380
L13270:             if keyhit% > 11% then L13310
                        gosub append_line  /* Always in Append Mode */
                        goto L13540
L13310:             if onfile% <> 1% then L13350
                        if okflag%(workline%) <> 3% then L13320
                            errormsg$ = "Cannot Delete PLANNED Demand"
                            goto L13090
L13320:                 if type$(workline%) < hex(39) then L13350
                            errormsg$ = "Already Flagged For Delete"
                            goto L13090
L13350:             gosub delete_line
                    goto L13090
L13370:         if keyhit% = 16% then datasave
L13380:         if keyhit% <>  0% then L13090
                    if edit% = 3% then L13420
                    if edit% = 4% then L13540

L13420
*       ** Set up for Full Screen Modify
            for j% = 1% to maxlines%
                if okflag%(j%) = 3% or type$(j%) > hex(40) then L13480
                    str(ff$(),j%*8%-4%,1%) = all(hex(82))   /* 4      */
                    str(ff$(),j%*8%-3%,1%) = all(hex(81))   /* 5      */
                    str(ff$(),j%*8%-2%,1%) = all(hex(82))   /* 6      */
                    str(ff$(),j%*8%-6%,2%) = all(hex(81))   /* 2 & 3  */
                    str(ff$(),j%*8%-1%,1%) =    (hex(81))   /* 7      */
L13480:     next j%
            top% = maxlines%
            edit% = 4%          /* Full Screen Edit Mode       */
            idx% = 1%
            goto L13090

L13540:     gosub'152(2%)               /* Test for Edit Mode          */
            edit% = 3%  :  idx% = 0%
            for j% = 1% to maxlines%
                if okflag%(j%) = 0% or okflag%(j%) = 3% then L13600
                if maxlines% > 15% and idx% = 0% then idx% = j%
                   idx% = min(idx%, mi%)
                edit% = 4%          /* Full Screen Edit Mode       */
L13600:     next j%
            if idx% = 0% then L13080 else L13090

        REM *************************************************************~
            *       I N P U T   M O D E   B Y   P A R T                 *~
            *-----------------------------------------------------------*~
            * Input mode By Part Number                                 *~
            *************************************************************

        input_by_part
            idx% = 1%
L15080:     gosub'103(1%)    /* Display / Accept  */
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  5% then L15122
                    if demline$(idx% + 14%) = " " then L15080
                        idx% = min(idx% + 15%, mi%)
                        goto L15080
L15122:         if keyhit% <> 3% then L15130
                    gosub copy_line_above_by_part_inputmode
                    goto  L15080
L15130:         if keyhit% <> 0% then L15080

L15150:     gosub'153(1%)           /* Test data for Input Mode */
                if maxlines% = 0% then L15080
            for j% = 1% to maxlines%
                if okflag%(j%) <> 0% then set_up_error2
            next j%
            errormsg$ = " "
            goto edit_by_part

        set_up_error2
            if maxlines% > 15% then idx% = j% else idx% = 1%
               idx% = min(idx%, mi%)
L15250:     gosub'103(2%)                /* Error Display & Input */
                if keyhit% =  1% then gosub startover
                if keyhit% =  2% then idx% = 1%
                if keyhit% =  3% then idx%=max(1%,min(mi%,maxlines%-14%))
                if keyhit% =  4% then idx% = max(1%,idx%-14%)
                if keyhit% =  5% then idx% =                             ~
                              max(1%, min(mi%,idx%+14%,maxlines%-14%))
                if keyhit% <> 0% then L15250
                goto L15150

        REM *************************************************************~
            *        E D I T   B Y   P A R T   N U M B E R              *~
            *-----------------------------------------------------------*~
            * Handles Edit Mode by Part Number                          *~
            *************************************************************

        edit_by_part
            edit% = 3%
L16080:     idx% = 1%
L16090:     gosub'103(edit%)
                errormsg$ = " "
                if keyhit% =  1% then gosub startover
                if keyhit% =  2% then idx% = 1%
                if keyhit% =  3% then idx%=max(1%,min(mi%,maxlines%-14%))
                if keyhit% =  4% then idx% = max(1%,idx%-14%)
                if keyhit% =  5% then idx% =                             ~
                              max(1%, min(mi%,idx%+14%,maxlines%-14%))
                if keyhit% < 10% or keyhit% > 12% then L16380
                    line% = cursor%(1) - 5%
                    workline% = min(line% + idx% - 1%, maxlines%)
                    if line% > 0%  then L16240
                        if keyhit% = 11% then L16230
                            errormsg$ = "Please Position Cursor First"
                            goto L16090
L16230:                 workline% = maxlines%
L16240:             if keyhit% > 10% then L16270
                        gosub reactivate
                        goto L16390
L16270:             if keyhit% > 11% then L16320
                        gosub insert_line
                        goto L16550
L16320:             if onfile% <> 1% then L16360
                        if okflag%(workline%) <> 3% then L16330
                            errormsg$ = "Cannot DELETE Planned Demand"
                            goto L16090
L16330:                 if type$(workline%) < hex(39) then L16360
                            errormsg$ = "Already Flagged For Delete"
                            goto L16090
L16360:             gosub delete_line
                    goto L16090
L16380:         if keyhit% = 16% then datasave
L16390:         if keyhit% <>  0% then L16090
                    if edit% = 3% then L16430
                    if edit% = 4% then L16550

L16430
*       ** Set up for Full Screen Modify
            for j% = 1% to maxlines%
                if okflag%(j%) = 3% or type$(j%) > hex(40) then L16490
                    str(ff$(),j%*8%-4%,1%) = all(hex(82)) /* 4        */
                    str(ff$(),j%*8%-3%,1%) = all(hex(82)) /* 5        */
                    str(ff$(),j%*8%-2%,1%) = all(hex(82)) /* 6        */
                    str(ff$(),j%*8%-7%,2%) = all(hex(81)) /* 1, 2 & 3 */
                    str(ff$(),j%*8%-1%,1%) =    (hex(81)) /* 7        */
L16490:     next j%
            top% = maxlines%
            edit% = 4%          /* Full Screen Edit Mode       */
            idx% = 1%
            goto L16090

L16550:     gosub'153(2%)               /* Test for Edit Mode          */
            edit% = 3%  :  idx% = 0%
            for j% = 1% to maxlines%
                if okflag%(j%) = 0% or okflag%(j%) = 3% then L16610
                if maxlines% > 15% and idx% = 0% then idx% = j%
                   idx% = min(idx%, mi%)
                edit% = 4%          /* Full Screen Edit Mode       */
L16610:     next j%
            if idx% = 0% then L16080 else L16090


        REM *************************************************************~
            *        S U B R O U T I N E   S E C T I O N                *~
            *-----------------------------------------------------------*~
            * Various subroutines reside here                           *~
            *************************************************************
*       *** Insert a line
        insert_line
            init(hex(8c)) ff$()
            if maxlines% < ml% then L17110
                errormsg$ = "Cannot Insert, already have 100 lines"
                return
L17110:     roll% = -1%
            if workline% >= 15% then idx% = min(workline% - 13%, mi%)
            workline% = min(workline% + 1%, ml%)
            maxlines% = min(maxlines% + 1%, ml%)
            if workline% = maxlines% then L17220
            for temp% = maxlines%       to  workline% step -1
                gosub roll_lines
            next temp%

*       ** Now set up to input the line
L17220:     c% = workline%
            if workline% <> maxlines% then gosub clearline
            str(ff$(),workline%*8%-7%,3%) = all(hex(81))  /* 1, 2 & 3 */
            str(ff$(),workline%*8%-4%,1%) = all(hex(82))  /* 4        */
            str(ff$(),workline%*8%-3%,1%) = all(hex(81))  /* 5        */
            str(ff$(),workline%*8%-2%,1%) = all(hex(82))  /* 6        */
            str(ff$(),workline%*8%-1%,1%) = all(hex(81))  /* 7        */

L17280:     gosub'103(5%)
                if keyhit% <> 3% then L17290
                    gosub copy_line_above_by_part_appendmode
                    goto L17280

L17290:         if keyhit% <> 0% then L17300
                    if qty$(workline%) <> " "                            ~
                        or (datereq$(workline%) <> " "                   ~
                        and datereq$(workline%) <> blankdate$)           ~
                        or type$(workline%) <> " " then L17297 else L17450
L17297:                 insert_lines%(workline%) = 1%  /* Set Flag */
                        goto insert_line

L17300:         if keyhit% <> 1% then L17280
                    goto startover

*              WORKLINE% = WORKLINE% - 1%  :  MAXLINES% = MAXLINES% - 1%
*              RETURN

        delete_line
            str(ff$(),workline%*8%-7%,7%) = all(hex(94))  /* 1 thru 7 */
            if mode% < 3% then gosub'102(7%) else gosub'103(7%)
                if keyhit% > 0% then L17470
                    if onfile% <> 1% then L17450
                        type$(workline%) = type$(workline%) or hex(40)
                        error$(workline%) = "To Be Deleted"
                        del% = del% + 1%
                        goto L17470
L17450:             here% = workline%
                    gosub roll_back
L17470:         edit% = 3%
                str(ff$(),workline%*8%-7%,7%) = all(hex(8c)) /*1 thru 7*/
                return

        roll_back
            roll% = 1%
            for temp% = here% to maxlines% - 1%
                gosub roll_lines
            next temp%
            c% = maxlines%
            gosub clearline
            init (hex(8c)) ff$()
            maxlines% = maxlines% - 1%
            demline$(maxlines% + 1%) = " "
            return

        roll_lines
            demcode$(temp%) = demcode$(temp% + roll%)
            demline$(temp%) = demline$(temp% + roll%)
            part$   (temp%) = part$   (temp% + roll%)
            qty$    (temp%) = qty$    (temp% + roll%)
            datereq$(temp%) = datereq$(temp% + roll%)
            type$   (temp%) = type$   (temp% + roll%)
            prio$   (temp%) = prio$   (temp% + roll%)
            error$  (temp%) = error$  (temp% + roll%)
            fillspc$(temp%) = fillspc$(temp% + roll%)
            fillhex$(temp%) = fillhex$(temp% + roll%)
            return

        clearline
            part$(c%), qty$(c%), datereq$(c%), type$(c%), error$(c%),    ~
                prio$(c%), demcode$(c%), demline$(c%), fillspc$(c%) = " "
            init (hex(00)) fillhex$(c%)
            return

        append_line
            if edit% <> 6% and maxlines% >= 15% then                     ~
                                           idx% = min(maxlines% -0%, mi%)
*          IF MOD(WORKLINE%, 15%) = 0% THEN IDX% = MIN(MAXLINES%, MI%)
            if workline% >= 15% then idx% = idx% + 1%
            workline%, maxlines% = min(maxlines% + 1%, ml%)
            ff$(workline%,2), ff$(workline%,3), ff$(workline%,7) = hex(81)
            str(ff$(),workline%*8%-4%,1%) = all(hex(82))
            str(ff$(),workline%*8%-3%,1%) = all(hex(81))
            str(ff$(),workline%*8%-2%,1%) = all(hex(82))
            hiline = workline%
            if workline% < 2% then  L17860
                convert demline$(workline% - 1%) to hiline
                hiline = hiline + 1
L17860:     convert hiline to demline$(workline%),pic(##0)

L17890:     gosub'102(6%)
                if keyhit% =  1% then goto startover
                if keyhit% <> 3% then L17910
                    gosub copy_line_above_by_demand_appendmode
                    goto L17890
L17910:         if keyhit% <> 0% then L17890
                    if maxlines% = ml% then L18000
                    if          qty$(workline%) <> " "                   ~
                        or (datereq$(workline%) <> " "                   ~
                        and datereq$(workline%) <> blankdate$)           ~
                        or     type$(workline%) <> " " then L17920
                    goto L17940
L17920:             str(ff$(),workline%*8%-7%,7%) = all(hex(8c))
                    goto append_line
L17940:         str(ff$(),maxlines%*8%-7%,7%) = all(hex(8c))
                hiline = hiline - 1
                demline$(maxlines%) = " "
                c% = maxlines%
                    gosub clearline
                maxlines% = maxlines% - 1%
L18000:         top% = maxlines%
                return

        reactivate
            if type$(workline%) > hex(40) then L18040
                errormsg$ = "Not flagged for Delete"
                return
L18040:     type$(workline%) = type$(workline%) and hex(3f)
            error$(workline%) = " "
            del% = del% - 1%
            return

        copy_line_above_by_demand_inputmode
            if part$(1%)    =  " " and     qty$(1%) =  " "        and   ~
              (datereq$(1%) =  " " or  datereq$(1%) = blankdate$) and   ~
                  type$(1%) =  " " and    prio$(1%) =  " "   then  return
            for workline% = 2% to 100%           /*Stops at first blank */
              if part$(workline%) <> " " and qty$(workline%) <> " " and ~
                (datereq$(workline%) <> " " and ~
                 datereq$(workline%) <> blankdate$) and ~
                type$(workline%) <> " " and prio$(workline%) <> " " then  L18220
               pline% = workline% - 1%
               gosub set_next_demand_line

               workline% = 101%
L18220:     next workline%
            return

        copy_line_above_by_demand_appendmode
            pline% = workline% - 1%
            if pline% < 1% then return
               gosub set_next_demand_line
               return

        set_next_demand_line
               part$(workline%)    = part$(pline%)
               qty$(workline%)     = qty$(pline%)
               datereq$(workline%) = datereq$(pline%)
               type$(workline%)    = type$(pline%)
               prio$(workline%)    = prio$(pline%)
            return

        copy_line_above_by_part_inputmode
            if demcode$(1%) =  " "  and demline$(1%) =  " "              ~
                                    and     qty$(1%) =  " " and          ~
              (datereq$(1%) =  " "  or  datereq$(1%) = blankdate$) and   ~
                  type$(1%) =  " "  and prio$(1%)    =  " " then return
            for workline% = 2% to 100%         /*Stops at first blank */
               if demcode$(workline%) =  " " and                         ~
                  demline$(workline%) =  " " and                         ~
                  qty$(workline%)     =  " " and                         ~
                 (datereq$(workline%) =  " " or                          ~
                  datereq$(workline%) =  blankdate$) and                 ~
                  type$(workline%)    =  " " and                         ~
                  prio$(workline%)    =  " "     then  L18500 else L18540
L18500:        pline% = workline% - 1%
               gosub set_next_part_line

               workline% = 101%
L18540:     next workline%
            return

        copy_line_above_by_part_appendmode
            pline% = workline% - 1%
            if pline% < 1% then return
               gosub set_next_part_line
               return

        set_next_part_line
               demcode$(workline%) = demcode$(pline%)
               demline$(workline%) = demline$(pline%)
               qty$(workline%)     = qty$(pline%)
               datereq$(workline%) = datereq$(pline%)
               type$(workline%)    = type$(pline%)
               prio$(workline%)    = prio$(pline%)
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if mode% < 3% then gosub save_demand else gosub save_part
            goto inputmode

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************~

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$(1) = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            if scrnr% = 2% then restore line = scrn2_msg, fieldnr%
            if scrnr% = 3% then restore line = scrn3_msg, fieldnr%
            read inpmessage$(1)   /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Demand Code -or- a Part Number and Demand Type, then pr~
        ~ess RETURN "

        scrn2_msg  :  data                                               ~
         "Enter Part, Qty, Date, Type (or blank) and Priority (or blank) ~
        ~and press RETURN"

        scrn3_msg  :  data                                               ~
         "Enter Demand, Line, Qty, Date, Type(or blank) & Priority(or bla~
        ~nk) then RETURN"


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            call "ALLFREE"
            init(" ") errormsg$, inpmessage$(), prio$(), demcodeselect$, ~
                      partselect$, demline$(), part$(), qty$(), type$(), ~
                      error$(), fillspc$(), error$, datereq$(), reqtype$,~
                      demcode$(), orig_datereq$(), orig_typ$(),          ~
                      orig_pri$()
            init(hex(00)) str(fillhex$())
            init("1") demstatus$()
            mat insert_lines% = zer
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        load_by_demand
            onfile%, maxlines% = 0%  :  errormsg$ = " "
            mat okflag% = zer  :  i% = 1%  :  hiline = 0
            init (hex(00)) readkey$
            str(readkey$,,16) = demcodeselect$
            call "SHOSTAT" ("Loading Demand Lines, one moment please")

        read_loop
                call "PLOWALTS" (#1, readkey$, 1%, 16%, f1%(1%))
                    if f1%(1%) = 0 then L30510
                    if i% > ml% then L30542
                get #1, using L30170, type$, seqno$
L30170:             FMT POS(2), CH(1), POS(26), CH(3)
                    convert seqno$ to seq, data goto read_loop
                    if seq > hiline then hiline = seq
                if type$ <> "3" and type$ <> "4" and type$ <> "5" and    ~
                   type$ <> "7" and type$ <> "8" then read_loop

                get #1,using L30260, demstatus$(i%), type$(i%), prio$(i%),~
                    datereq$(i%), demline$(i%),  part$(i%),  qty$(i%),   ~
                    fillspc$(i%), fillhex$(i%)

L30260:         FMT CH( 1),                   /* Record status         */~
                    CH( 1),                   /* Demand type           */~
                    CH( 1),                   /* Priority              */~
                    CH( 6),                   /* Req completion date   */~
                    XX(16),                   /* Demand code           */~
                    CH( 3),                   /* Demand line           */~
                    CH(25),                   /* Part needed           */~
                    CH(10),                   /* Quantity              */~
                    CH(50),                   /* Unused data           */~
        /*          CH(16),                   /* Filler - spaces       */~
                    CH(10)                    /* Filler - HEX(00)      */


                if demstatus$(i%) < "2" then L30420
                    okflag%(i%) = 3%
                    error$(i%) = "Demand Planned"
L30420:         call "NUMTEST" (qty$(i%), 0, 9e7, error$, -2.2, qty)
                orig_qty(i%) = qty
                orig_typ$(i%) = type$(i%)
                orig_datereq$(i%) = datereq$(i%)
                orig_pri$(i%) = prio$(i%)
                call "DATEFMT" (datereq$(i%))
                maxlines% = maxlines% + 1
                onfile% = 1%
                del% = 0%
                i% = i% + 1%
                goto read_loop
L30510:     if maxlines% <> 0% then return
                if reqtype$ = " " then  errormsg$ = hex(84)  &           ~
                 "There are no type 3, 4, 5, 7 or 8 lines on this demand"~
                                  else  errormsg$ = hex(84)  &           ~
                 "There are no type " & reqtype$ & " lines on this demand"
            return

L30542:     errormsg$ = "There are more than 100 demands on file for this~
        ~ Part.  The 1st 100 are shown"
            return

        load_by_part
            onfile%, maxlines% = 0%  :  errormsg$ = " "
            mat okflag% = zer  :  i% = 1%  :  mode% = 3%
            call "SHOSTAT" ("Loading Demands, one moment please")
            call "REDALT0" (#1, partselect$, 3%, f1%(1%))
            goto L30650

        read_loop2
            call "READNEXT" (#1, f1%(1%))
L30650:         if f1%(1%) = 0% then L31010
                if i% > ml% then L31060
            get #1 using L30680, type$, part$
L30680:         FMT POS(2), CH(1), POS(29), CH(25)
            if part$ <> partselect$ then L31010
            if reqtype$ = " " then L30720
                if type$ <> reqtype$ then read_loop2
L30720:     if type$ <> "3" and type$ <> "4" and type$ <> "5" and        ~
               type$ <> "7" and type$ <> "8" then read_loop2

            get #1, using L30780, demstatus$(i%), type$(i%), prio$(i%),   ~
                      datereq$(i%), demcode$(i%), demline$(i%),          ~
                      qty$(i%), fillspc$(i%), fillhex$(i%)

L30780:     FMT CH( 1),                       /* Record status         */~
                CH( 1),                       /* Demand type           */~
                CH( 1),                       /* Priority              */~
                CH( 6),                       /* Req completion date   */~
                CH(16),                       /* Demand code           */~
                CH( 3),                       /* Demand line           */~
                XX(25),                       /* Part needed           */~
                CH(10),                       /* Quantity              */~
                CH(50),                       /* Unused data           */~
                CH(10)                        /* Filler HEX(00)        */

            if demstatus$(i%) < "2" then L30930
                okflag%(i%) = 3%
                error$(i%) = "Demand Planned"
L30930:     call "NUMTEST" (qty$(i%), 0, 9e7, error$, -2.2, qty)
            orig_qty(i%)      = qty
            orig_datereq$(i%) = datereq$(i%)
            call "DATEFMT" (datereq$(i%))
            maxlines% = maxlines% + 1%
            onfile% = 1% : mode% = 4% : i% = i% + 1%
            del% = 0%
            goto read_loop2

L31010:     if maxlines% <> 0% then L31040
                if reqtype$ = " "  then errormsg$ = hex(84)  &           ~
               "There are no type 3, 4, 5, 7 or 8 demands for this Part" ~
                                  else errormsg$ = hex(84)  &            ~
               "There are no type " & reqtype$ & " demands for this Part"

L31040:     oldmax% = maxlines%
            return

L31060:     errormsg$ = "There are more than 100 demands on file for this~
        ~ Part.  The 1st 100 are shown"
            return


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        save_demand
            call "SHOSTAT" ("Saving Demands, one moment please")
            init(hex(00)) readkey$
            str(readkey$,,16) = demcodeselect$
            for i% = 1% to maxlines%
                str(readkey$,17,3) = demline$(i%)
                call "REDALT1" (#1, readkey$, 1%, f1%(1%))
                if type$(i%) < hex(40) then L35150
                    if f1%(1%) <> 0% then delete #1
                    goto L35250

L35150:         call "DATUNFMT" (datereq$(i%))
                if datereq$(i%) = orig_datereq$(i%) and                  ~
                          type$(i%) = orig_typ$(i%) and                  ~
                          prio$(i%) = orig_pri$(i%) then L35180
                    if f1%(1%) <> 0% then delete #1
L35180:         put #1, using L35280, demstatus$(i%), type$(i%),          ~
                        prio$(i%), datereq$(i%), demcodeselect$,         ~
                        demline$(i%), part$(i%), qty$(i%),               ~
                        fillspc$(i%), fillhex$(i%)
                if f1%(1%) = 0% or datereq$(i%) <> orig_datereq$(i%)     ~
                                or type$(i%) <> orig_typ$(i%)            ~
                                or prio$(i%) <> orig_pri$(i%)            ~
                    then write #1 else rewrite #1

L35250:     next i%
            return

L35280:         FMT 3*CH(1), CH(6), CH(16), CH(3), CH(25), CH(10),       ~
                             CH(50), CH(10)

        save_part
            init(hex(00)) readkey$
            call "SHOSTAT" ("Saving Demands, one moment please")
            for i% = 1% to maxlines%
                str(readkey$,,16%) = demcode$(i%)
                str(readkey$,17%,3%) = demline$(i%)
                call "REDALT1" (#1, readkey$, 1%, f1%(1%))
                if type$(i%) < hex(40) then L37110
                    if f1%(1%) <> 0% then delete #1
                    goto L37250

L37110:         call "DATUNFMT" (datereq$(i%))
                if datereq$(i%) = orig_datereq$(i%) and                  ~
                          type$(i%) = orig_typ$(i%) and                  ~
                          prio$(i%) = orig_pri$(i%) then L37160
                    if f1%(1%) <> 0% then delete #1
L37160:         put #1, using L35280, demstatus$(i%), type$(i%),          ~
                        prio$(i%), datereq$(i%), demcode$(i%),           ~
                        demline$(i%), partselect$, qty$(i%),             ~
                        fillspc$(i%), fillhex$(i%)
                if f1%(1%) = 0% or datereq$(i%) <> orig_datereq$(i%)     ~
                                or type$(i%) <> orig_typ$(i%)            ~
                                or prio$(i%) <> orig_pri$(i%)            ~
                    then write #1 else rewrite #1

L37250:     next i%
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(edit%)
              gosub'050(1%, 1%)
              gosub set_pf1
              init(hex(81)) lfac$()

L40110:     accept                                                       ~
               at (01,02),                                               ~
                  "Rapid Entry or Review of Non-SO Demands",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Demand Code",                                ~
               at (06,20), fac(lfac$( 1)), demcodeselect$       , ch(16),~
               at (08,02), "----- OR ----------------------------------",~
               at (10,02), "Part Number",                                ~
               at (10,20), fac(lfac$( 1)), partselect$          , ch(25),~
               at (11,06), "and",                                        ~
               at (12,02), "Demand Type",                                ~
               at (12,20), fac(lfac$( 1)), reqtype$             , ch(01),~
               at (12,25), "'3' = Requisition Demand",                   ~
               at (13,25), "'4' = Nettable Forecast",                    ~
               at (14,25), "'5' = Non-Nettable Forecast",                ~
               at (15,25), "'7' = Force Procurement with jump",          ~
               at (16,25), "'8' = Force Procurement without jump",       ~
               at (17,25), "' ' = All of the above",                     ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$(1)       , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40420
                  call "MANUAL" ("DEMRAPID") : goto L40110

L40420:        if keyhit% <> 15% then L40450
                  call "PRNTSCRN" : goto L40110

L40450:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)

            return

        REM *************************************************************~
            *   I N P U T  /  E D I T   F O R   B Y   D E M A N D       *~
            *-----------------------------------------------------------*~
            * Input/Edit Screen for By Demand option                    *~
            *************************************************************

        deffn'102(edit%)
            on edit% gosub L48010,        /* Input Mode                 */~
                           L48150,        /* Error correction input     */~
                           L48190,        /* Display Only               */~
                           L48240,        /* Full Screen Edit           */~
                           L48290,        /* Insert Line on new demand  */~
                           L48290,        /* Append Line on old demand  */~
                           L48330         /* Delete Line from demand    */
            goto L41160

L41160:     gosub set_pf_keys
L41170:     accept                                                       ~
               at (01,02),                                               ~
                  "Rapid Entry or Review of Non-SO Demands",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,03), "Demand Code:",                               ~
               at (04,17), fac(hex(84)), demcodeselect$, ch(16),         ~
                                                                         ~
               at (05,02), fac(hex(ac)),  header$(1)     , ch(79),       ~
               at (06,02), fac(ff$(idx%   ,1)),demline$(idx%   ) ,ch(03),~
               at (07,02), fac(ff$(idx%+ 1,1)),demline$(idx%+ 1) ,ch(03),~
               at (08,02), fac(ff$(idx%+ 2,1)),demline$(idx%+ 2) ,ch(03),~
               at (09,02), fac(ff$(idx%+ 3,1)),demline$(idx%+ 3) ,ch(03),~
               at (10,02), fac(ff$(idx%+ 4,1)),demline$(idx%+ 4) ,ch(03),~
               at (11,02), fac(ff$(idx%+ 5,1)),demline$(idx%+ 5) ,ch(03),~
               at (12,02), fac(ff$(idx%+ 6,1)),demline$(idx%+ 6) ,ch(03),~
               at (13,02), fac(ff$(idx%+ 7,1)),demline$(idx%+ 7) ,ch(03),~
               at (14,02), fac(ff$(idx%+ 8,1)),demline$(idx%+ 8) ,ch(03),~
               at (15,02), fac(ff$(idx%+ 9,1)),demline$(idx%+ 9) ,ch(03),~
               at (16,02), fac(ff$(idx%+10,1)),demline$(idx%+10) ,ch(03),~
               at (17,02), fac(ff$(idx%+11,1)),demline$(idx%+11) ,ch(03),~
               at (18,02), fac(ff$(idx%+12,1)),demline$(idx%+12) ,ch(03),~
               at (19,02), fac(ff$(idx%+13,1)),demline$(idx%+13) ,ch(03),~
               at (20,02), fac(ff$(idx%+14,1)),demline$(idx%+14) ,ch(03),~
               at (06,07), fac(ff$(idx%   ,3)),part$   (idx%   ) ,ch(25),~
               at (07,07), fac(ff$(idx%+ 1,3)),part$   (idx%+ 1) ,ch(25),~
               at (08,07), fac(ff$(idx%+ 2,3)),part$   (idx%+ 2) ,ch(25),~
               at (09,07), fac(ff$(idx%+ 3,3)),part$   (idx%+ 3) ,ch(25),~
               at (10,07), fac(ff$(idx%+ 4,3)),part$   (idx%+ 4) ,ch(25),~
               at (11,07), fac(ff$(idx%+ 5,3)),part$   (idx%+ 5) ,ch(25),~
               at (12,07), fac(ff$(idx%+ 6,3)),part$   (idx%+ 6) ,ch(25),~
               at (13,07), fac(ff$(idx%+ 7,3)),part$   (idx%+ 7) ,ch(25),~
               at (14,07), fac(ff$(idx%+ 8,3)),part$   (idx%+ 8) ,ch(25),~
               at (15,07), fac(ff$(idx%+ 9,3)),part$   (idx%+ 9) ,ch(25),~
               at (16,07), fac(ff$(idx%+10,3)),part$   (idx%+10) ,ch(25),~
               at (17,07), fac(ff$(idx%+11,3)),part$   (idx%+11) ,ch(25),~
               at (18,07), fac(ff$(idx%+12,3)),part$   (idx%+12) ,ch(25),~
               at (19,07), fac(ff$(idx%+13,3)),part$   (idx%+13) ,ch(25),~
               at (20,07), fac(ff$(idx%+14,3)),part$   (idx%+14) ,ch(25),~
               at (06,33), fac(ff$(idx%   ,4)),qty$    (idx%   ) ,ch(10),~
               at (07,33), fac(ff$(idx%+ 1,4)),qty$    (idx%+ 1) ,ch(10),~
               at (08,33), fac(ff$(idx%+ 2,4)),qty$    (idx%+ 2) ,ch(10),~
               at (09,33), fac(ff$(idx%+ 3,4)),qty$    (idx%+ 3) ,ch(10),~
               at (10,33), fac(ff$(idx%+ 4,4)),qty$    (idx%+ 4) ,ch(10),~
               at (11,33), fac(ff$(idx%+ 5,4)),qty$    (idx%+ 5) ,ch(10),~
               at (12,33), fac(ff$(idx%+ 6,4)),qty$    (idx%+ 6) ,ch(10),~
               at (13,33), fac(ff$(idx%+ 7,4)),qty$    (idx%+ 7) ,ch(10),~
               at (14,33), fac(ff$(idx%+ 8,4)),qty$    (idx%+ 8) ,ch(10),~
               at (15,33), fac(ff$(idx%+ 9,4)),qty$    (idx%+ 9) ,ch(10),~
               at (16,33), fac(ff$(idx%+10,4)),qty$    (idx%+10) ,ch(10),~
               at (17,33), fac(ff$(idx%+11,4)),qty$    (idx%+11) ,ch(10),~
               at (18,33), fac(ff$(idx%+12,4)),qty$    (idx%+12) ,ch(10),~
               at (19,33), fac(ff$(idx%+13,4)),qty$    (idx%+13) ,ch(10),~
               at (20,33), fac(ff$(idx%+14,4)),qty$    (idx%+14) ,ch(10),~
               at (06,45), fac(ff$(idx%   ,5)),datereq$(idx%   ) ,ch(08),~
               at (07,45), fac(ff$(idx%+ 1,5)),datereq$(idx%+ 1) ,ch(08),~
               at (08,45), fac(ff$(idx%+ 2,5)),datereq$(idx%+ 2) ,ch(08),~
               at (09,45), fac(ff$(idx%+ 3,5)),datereq$(idx%+ 3) ,ch(08),~
               at (10,45), fac(ff$(idx%+ 4,5)),datereq$(idx%+ 4) ,ch(08),~
               at (11,45), fac(ff$(idx%+ 5,5)),datereq$(idx%+ 5) ,ch(08),~
               at (12,45), fac(ff$(idx%+ 6,5)),datereq$(idx%+ 6) ,ch(08),~
               at (13,45), fac(ff$(idx%+ 7,5)),datereq$(idx%+ 7) ,ch(08),~
               at (14,45), fac(ff$(idx%+ 8,5)),datereq$(idx%+ 8) ,ch(08),~
               at (15,45), fac(ff$(idx%+ 9,5)),datereq$(idx%+ 9) ,ch(08),~
               at (16,45), fac(ff$(idx%+10,5)),datereq$(idx%+10) ,ch(08),~
               at (17,45), fac(ff$(idx%+11,5)),datereq$(idx%+11) ,ch(08),~
               at (18,45), fac(ff$(idx%+12,5)),datereq$(idx%+12) ,ch(08),~
               at (19,45), fac(ff$(idx%+13,5)),datereq$(idx%+13) ,ch(08),~
               at (20,45), fac(ff$(idx%+14,5)),datereq$(idx%+14) ,ch(08),~
               at (06,55), fac(ff$(idx%   ,6)),type$   (idx%   ) ,ch(01),~
               at (07,55), fac(ff$(idx%+ 1,6)),type$   (idx%+ 1) ,ch(01),~
               at (08,55), fac(ff$(idx%+ 2,6)),type$   (idx%+ 2) ,ch(01),~
               at (09,55), fac(ff$(idx%+ 3,6)),type$   (idx%+ 3) ,ch(01),~
               at (10,55), fac(ff$(idx%+ 4,6)),type$   (idx%+ 4) ,ch(01),~
               at (11,55), fac(ff$(idx%+ 5,6)),type$   (idx%+ 5) ,ch(01),~
               at (12,55), fac(ff$(idx%+ 6,6)),type$   (idx%+ 6) ,ch(01),~
               at (13,55), fac(ff$(idx%+ 7,6)),type$   (idx%+ 7) ,ch(01),~
               at (14,55), fac(ff$(idx%+ 8,6)),type$   (idx%+ 8) ,ch(01),~
               at (15,55), fac(ff$(idx%+ 9,6)),type$   (idx%+ 9) ,ch(01),~
               at (16,55), fac(ff$(idx%+10,6)),type$   (idx%+10) ,ch(01),~
               at (17,55), fac(ff$(idx%+11,6)),type$   (idx%+11) ,ch(01),~
               at (18,55), fac(ff$(idx%+12,6)),type$   (idx%+12) ,ch(01),~
               at (19,55), fac(ff$(idx%+13,6)),type$   (idx%+13) ,ch(01),~
               at (20,55), fac(ff$(idx%+14,6)),type$   (idx%+14) ,ch(01),~
               at (06,59), fac(ff$(idx%   ,7)),prio$   (idx%   ) ,ch(01),~
               at (07,59), fac(ff$(idx%+ 1,7)),prio$   (idx%+ 1) ,ch(01),~
               at (08,59), fac(ff$(idx%+ 2,7)),prio$   (idx%+ 2) ,ch(01),~
               at (09,59), fac(ff$(idx%+ 3,7)),prio$   (idx%+ 3) ,ch(01),~
               at (10,59), fac(ff$(idx%+ 4,7)),prio$   (idx%+ 4) ,ch(01),~
               at (11,59), fac(ff$(idx%+ 5,7)),prio$   (idx%+ 5) ,ch(01),~
               at (12,59), fac(ff$(idx%+ 6,7)),prio$   (idx%+ 6) ,ch(01),~
               at (13,59), fac(ff$(idx%+ 7,7)),prio$   (idx%+ 7) ,ch(01),~
               at (14,59), fac(ff$(idx%+ 8,7)),prio$   (idx%+ 8) ,ch(01),~
               at (15,59), fac(ff$(idx%+ 9,7)),prio$   (idx%+ 9) ,ch(01),~
               at (16,59), fac(ff$(idx%+10,7)),prio$   (idx%+10) ,ch(01),~
               at (17,59), fac(ff$(idx%+11,7)),prio$   (idx%+11) ,ch(01),~
               at (18,59), fac(ff$(idx%+12,7)),prio$   (idx%+12) ,ch(01),~
               at (19,59), fac(ff$(idx%+13,7)),prio$   (idx%+13) ,ch(01),~
               at (20,59), fac(ff$(idx%+14,7)),prio$   (idx%+14) ,ch(01),~
               at (06,63), fac(ff$(idx%   ,8)),error$  (idx%   ) ,ch(17),~
               at (07,63), fac(ff$(idx%+ 1,8)),error$  (idx%+ 1) ,ch(17),~
               at (08,63), fac(ff$(idx%+ 2,8)),error$  (idx%+ 2) ,ch(17),~
               at (09,63), fac(ff$(idx%+ 3,8)),error$  (idx%+ 3) ,ch(17),~
               at (10,63), fac(ff$(idx%+ 4,8)),error$  (idx%+ 4) ,ch(17),~
               at (11,63), fac(ff$(idx%+ 5,8)),error$  (idx%+ 5) ,ch(17),~
               at (12,63), fac(ff$(idx%+ 6,8)),error$  (idx%+ 6) ,ch(17),~
               at (13,63), fac(ff$(idx%+ 7,8)),error$  (idx%+ 7) ,ch(17),~
               at (14,63), fac(ff$(idx%+ 8,8)),error$  (idx%+ 8) ,ch(17),~
               at (15,63), fac(ff$(idx%+ 9,8)),error$  (idx%+ 9) ,ch(17),~
               at (16,63), fac(ff$(idx%+10,8)),error$  (idx%+10) ,ch(17),~
               at (17,63), fac(ff$(idx%+11,8)),error$  (idx%+11) ,ch(17),~
               at (18,63), fac(ff$(idx%+12,8)),error$  (idx%+12) ,ch(17),~
               at (19,63), fac(ff$(idx%+13,8)),error$  (idx%+13) ,ch(17),~
               at (20,63), fac(ff$(idx%+14,8)),error$  (idx%+14) ,ch(17),~
               at (21,02), fac(hex(a4)),   inpmessage$(1)        ,ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)                ,ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)                ,ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)                ,ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L42430
                  call "MANUAL" ("DEMRAPID") : goto L41170

L42430:        if keyhit% <> 15% then L42460
                  call "PRNTSCRN" : goto L41170

L42460:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *      I N P U T  /  E D I T   F O R   B Y   P A R T        *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(edit%)
            on edit% gosub L48010,        /* Input Mode                 */~
                           L48150,        /* Error correction input     */~
                           L48190,        /* Display Only               */~
                           L48240,        /* Full Screen Edit           */~
                           L48290,        /* Insert Line on new demand  */~
                           L48290,        /* Append Line on old demand  */~
                           L48330         /* Delete Line from demand    */
            goto L45160

L45160:     gosub set_pf_keys
L45170:     accept                                                       ~
               at (01,02),                                               ~
                  "Rapid Entry or Review of Non-SO Demands",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), "Demands for Part: ",                         ~
               at (04,20), fac(hex(8c)), partselect$            , ch(25),~
               at (05,02), fac(hex(ac)), header$(1)             , ch(79),~
               at (06,23), fac(ff$(idx%   ,1)),demline$(idx%   ) ,ch(03),~
               at (07,23), fac(ff$(idx%+ 1,1)),demline$(idx%+ 1) ,ch(03),~
               at (08,23), fac(ff$(idx%+ 2,1)),demline$(idx%+ 2) ,ch(03),~
               at (09,23), fac(ff$(idx%+ 3,1)),demline$(idx%+ 3) ,ch(03),~
               at (10,23), fac(ff$(idx%+ 4,1)),demline$(idx%+ 4) ,ch(03),~
               at (11,23), fac(ff$(idx%+ 5,1)),demline$(idx%+ 5) ,ch(03),~
               at (12,23), fac(ff$(idx%+ 6,1)),demline$(idx%+ 6) ,ch(03),~
               at (13,23), fac(ff$(idx%+ 7,1)),demline$(idx%+ 7) ,ch(03),~
               at (14,23), fac(ff$(idx%+ 8,1)),demline$(idx%+ 8) ,ch(03),~
               at (15,23), fac(ff$(idx%+ 9,1)),demline$(idx%+ 9) ,ch(03),~
               at (16,23), fac(ff$(idx%+10,1)),demline$(idx%+10) ,ch(03),~
               at (17,23), fac(ff$(idx%+11,1)),demline$(idx%+11) ,ch(03),~
               at (18,23), fac(ff$(idx%+12,1)),demline$(idx%+12) ,ch(03),~
               at (19,23), fac(ff$(idx%+13,1)),demline$(idx%+13) ,ch(03),~
               at (20,23), fac(ff$(idx%+14,1)),demline$(idx%+14) ,ch(03),~
               at (06,02), fac(ff$(idx%   ,2)),demcode$(idx%   ) ,ch(16),~
               at (07,02), fac(ff$(idx%+ 1,2)),demcode$(idx%+ 1) ,ch(16),~
               at (08,02), fac(ff$(idx%+ 2,2)),demcode$(idx%+ 2) ,ch(16),~
               at (09,02), fac(ff$(idx%+ 3,2)),demcode$(idx%+ 3) ,ch(16),~
               at (10,02), fac(ff$(idx%+ 4,2)),demcode$(idx%+ 4) ,ch(16),~
               at (11,02), fac(ff$(idx%+ 5,2)),demcode$(idx%+ 5) ,ch(16),~
               at (12,02), fac(ff$(idx%+ 6,2)),demcode$(idx%+ 6) ,ch(16),~
               at (13,02), fac(ff$(idx%+ 7,2)),demcode$(idx%+ 7) ,ch(16),~
               at (14,02), fac(ff$(idx%+ 8,2)),demcode$(idx%+ 8) ,ch(16),~
               at (15,02), fac(ff$(idx%+ 9,2)),demcode$(idx%+ 9) ,ch(16),~
               at (16,02), fac(ff$(idx%+10,2)),demcode$(idx%+10) ,ch(16),~
               at (17,02), fac(ff$(idx%+11,2)),demcode$(idx%+11) ,ch(16),~
               at (18,02), fac(ff$(idx%+12,2)),demcode$(idx%+12) ,ch(16),~
               at (19,02), fac(ff$(idx%+13,2)),demcode$(idx%+13) ,ch(16),~
               at (20,02), fac(ff$(idx%+14,2)),demcode$(idx%+14) ,ch(16),~
               at (06,30), fac(ff$(idx%   ,4)),qty$(idx%   )     ,ch(10),~
               at (07,30), fac(ff$(idx%+ 1,4)),qty$(idx%+ 1)     ,ch(10),~
               at (08,30), fac(ff$(idx%+ 2,4)),qty$(idx%+ 2)     ,ch(10),~
               at (09,30), fac(ff$(idx%+ 3,4)),qty$(idx%+ 3)     ,ch(10),~
               at (10,30), fac(ff$(idx%+ 4,4)),qty$(idx%+ 4)     ,ch(10),~
               at (11,30), fac(ff$(idx%+ 5,4)),qty$(idx%+ 5)     ,ch(10),~
               at (12,30), fac(ff$(idx%+ 6,4)),qty$(idx%+ 6)     ,ch(10),~
               at (13,30), fac(ff$(idx%+ 7,4)),qty$(idx%+ 7)     ,ch(10),~
               at (14,30), fac(ff$(idx%+ 8,4)),qty$(idx%+ 8)     ,ch(10),~
               at (15,30), fac(ff$(idx%+ 9,4)),qty$(idx%+ 9)     ,ch(10),~
               at (16,30), fac(ff$(idx%+10,4)),qty$(idx%+10)     ,ch(10),~
               at (17,30), fac(ff$(idx%+11,4)),qty$(idx%+11)     ,ch(10),~
               at (18,30), fac(ff$(idx%+12,4)),qty$(idx%+12)     ,ch(10),~
               at (19,30), fac(ff$(idx%+13,4)),qty$(idx%+13)     ,ch(10),~
               at (20,30), fac(ff$(idx%+14,4)),qty$(idx%+14)     ,ch(10),~
               at (06,43), fac(ff$(idx%   ,5)),datereq$(idx%   ) ,ch(08),~
               at (07,43), fac(ff$(idx%+ 1,5)),datereq$(idx%+ 1) ,ch(08),~
               at (08,43), fac(ff$(idx%+ 2,5)),datereq$(idx%+ 2) ,ch(08),~
               at (09,43), fac(ff$(idx%+ 3,5)),datereq$(idx%+ 3) ,ch(08),~
               at (10,43), fac(ff$(idx%+ 4,5)),datereq$(idx%+ 4) ,ch(08),~
               at (11,43), fac(ff$(idx%+ 5,5)),datereq$(idx%+ 5) ,ch(08),~
               at (12,43), fac(ff$(idx%+ 6,5)),datereq$(idx%+ 6) ,ch(08),~
               at (13,43), fac(ff$(idx%+ 7,5)),datereq$(idx%+ 7) ,ch(08),~
               at (14,43), fac(ff$(idx%+ 8,5)),datereq$(idx%+ 8) ,ch(08),~
               at (15,43), fac(ff$(idx%+ 9,5)),datereq$(idx%+ 9) ,ch(08),~
               at (16,43), fac(ff$(idx%+10,5)),datereq$(idx%+10) ,ch(08),~
               at (17,43), fac(ff$(idx%+11,5)),datereq$(idx%+11) ,ch(08),~
               at (18,43), fac(ff$(idx%+12,5)),datereq$(idx%+12) ,ch(08),~
               at (19,43), fac(ff$(idx%+13,5)),datereq$(idx%+13) ,ch(08),~
               at (20,43), fac(ff$(idx%+14,5)),datereq$(idx%+14) ,ch(08),~
               at (06,54), fac(ff$(idx%   ,6)),type$(idx%   )    ,ch(01),~
               at (07,54), fac(ff$(idx%+ 1,6)),type$(idx%+ 1)    ,ch(01),~
               at (08,54), fac(ff$(idx%+ 2,6)),type$(idx%+ 2)    ,ch(01),~
               at (09,54), fac(ff$(idx%+ 3,6)),type$(idx%+ 3)    ,ch(01),~
               at (10,54), fac(ff$(idx%+ 4,6)),type$(idx%+ 4)    ,ch(01),~
               at (11,54), fac(ff$(idx%+ 5,6)),type$(idx%+ 5)    ,ch(01),~
               at (12,54), fac(ff$(idx%+ 6,6)),type$(idx%+ 6)    ,ch(01),~
               at (13,54), fac(ff$(idx%+ 7,6)),type$(idx%+ 7)    ,ch(01),~
               at (14,54), fac(ff$(idx%+ 8,6)),type$(idx%+ 8)    ,ch(01),~
               at (15,54), fac(ff$(idx%+ 9,6)),type$(idx%+ 9)    ,ch(01),~
               at (16,54), fac(ff$(idx%+10,6)),type$(idx%+10)    ,ch(01),~
               at (17,54), fac(ff$(idx%+11,6)),type$(idx%+11)    ,ch(01),~
               at (18,54), fac(ff$(idx%+12,6)),type$(idx%+12)    ,ch(01),~
               at (19,54), fac(ff$(idx%+13,6)),type$(idx%+13)    ,ch(01),~
               at (20,54), fac(ff$(idx%+14,6)),type$(idx%+14)    ,ch(01),~
               at (06,59), fac(ff$(idx%   ,7)),prio$   (idx%   ) ,ch(01),~
               at (07,59), fac(ff$(idx%+ 1,7)),prio$   (idx%+ 1) ,ch(01),~
               at (08,59), fac(ff$(idx%+ 2,7)),prio$   (idx%+ 2) ,ch(01),~
               at (09,59), fac(ff$(idx%+ 3,7)),prio$   (idx%+ 3) ,ch(01),~
               at (10,59), fac(ff$(idx%+ 4,7)),prio$   (idx%+ 4) ,ch(01),~
               at (11,59), fac(ff$(idx%+ 5,7)),prio$   (idx%+ 5) ,ch(01),~
               at (12,59), fac(ff$(idx%+ 6,7)),prio$   (idx%+ 6) ,ch(01),~
               at (13,59), fac(ff$(idx%+ 7,7)),prio$   (idx%+ 7) ,ch(01),~
               at (14,59), fac(ff$(idx%+ 8,7)),prio$   (idx%+ 8) ,ch(01),~
               at (15,59), fac(ff$(idx%+ 9,7)),prio$   (idx%+ 9) ,ch(01),~
               at (16,59), fac(ff$(idx%+10,7)),prio$   (idx%+10) ,ch(01),~
               at (17,59), fac(ff$(idx%+11,7)),prio$   (idx%+11) ,ch(01),~
               at (18,59), fac(ff$(idx%+12,7)),prio$   (idx%+12) ,ch(01),~
               at (19,59), fac(ff$(idx%+13,7)),prio$   (idx%+13) ,ch(01),~
               at (20,59), fac(ff$(idx%+14,7)),prio$   (idx%+14) ,ch(01),~
               at (06,63), fac(ff$(idx%   ,8)),error$(idx%   )   ,ch(17),~
               at (07,63), fac(ff$(idx%+ 1,8)),error$(idx%+ 1)   ,ch(17),~
               at (08,63), fac(ff$(idx%+ 2,8)),error$(idx%+ 2)   ,ch(17),~
               at (09,63), fac(ff$(idx%+ 3,8)),error$(idx%+ 3)   ,ch(17),~
               at (10,63), fac(ff$(idx%+ 4,8)),error$(idx%+ 4)   ,ch(17),~
               at (11,63), fac(ff$(idx%+ 5,8)),error$(idx%+ 5)   ,ch(17),~
               at (12,63), fac(ff$(idx%+ 6,8)),error$(idx%+ 6)   ,ch(17),~
               at (13,63), fac(ff$(idx%+ 7,8)),error$(idx%+ 7)   ,ch(17),~
               at (14,63), fac(ff$(idx%+ 8,8)),error$(idx%+ 8)   ,ch(17),~
               at (15,63), fac(ff$(idx%+ 9,8)),error$(idx%+ 9)   ,ch(17),~
               at (16,63), fac(ff$(idx%+10,8)),error$(idx%+10)   ,ch(17),~
               at (17,63), fac(ff$(idx%+11,8)),error$(idx%+11)   ,ch(17),~
               at (18,63), fac(ff$(idx%+12,8)),error$(idx%+12)   ,ch(17),~
               at (19,63), fac(ff$(idx%+13,8)),error$(idx%+13)   ,ch(17),~
               at (20,63), fac(ff$(idx%+14,8)),error$(idx%+14)   ,ch(17),~
               at (21,02), fac(hex(a4)),   inpmessage$(1)       , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L46420
                  call "MANUAL" ("DEMRAPID") : goto L45170

L46420:        if keyhit% <> 15% then L46450
                  call "PRNTSCRN" : goto L45170

L46450:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf_keys
            header$(1) = "Lin  Part Number                 Quantity" &   ~
                         "  Req-Date Typ Pri  Error Message"
            if mode% > 2% then                                           ~
                header$(1%) = "Demand Code         Line      Quantit" &  ~
                             "y   Req-Date  Typ  Pri  Error Message"

        if edit% > 1% then L47190     /* this is Input Mode      */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "(3)Copy Above                           " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                 (5)More Input          " &       ~
                     "                                       "
            pfkeys$ = hex(01ff03ff05ffffffffffffff0dff0fff00)
            if idx% < mi% then return
                pf$(3%) = " "  :  str(pfkeys$,5,1) = hex(ff)
            return

L47190: if edit% > 2% then L47300     /* Error Display & Input   */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "(2)First Screen  (4)Previous Screen     " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "(3)Last Screen   (5)Next Screen         " &       ~
                     "                                       "
            pfkeys$ = hex(0102030405ffffffffffffff0dff0fff00)
            gosub turn_on_off
            return

L47300: if edit% > 3% then L47460     /* Edit Display Only       */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "(2)First Screen  (4)Previous Screen    (" &       ~
                     "11)Insert Line         (15)Print Screen"
            pf$(3%) = "(3)Last Screen   (5)Next Screen        (" &       ~
                     "12)Delete Line         (16)Save Data   "
            pfkeys$ = hex(0102030405ffffffffff0b0c0dff0f1000)
            if mode% > 2% then L47430
                if maxlines% < ml% then L47390
                     str(pf$(2),40,16) = " "
                     str(pfkeys$,11,1) = hex(ff)
                     goto L47430
L47390:         str(pf$(2),44,16) = "Append Line"
                if del% = 0% then L47430
                    str(pf$(1),17,30) = "(10)Reinstate Deleted Line"
                    str(pfkeys$,10,1) = hex(0a)
L47430:     gosub turn_on_off
            return

L47460:     if edit% > 4% then L47580      /* Full Screen Edit        */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "(2)First Screen  (4)Previous Screen     " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "(3)Last Screen   (5)Next Screen         " &       ~
                     "                                       "
            pfkeys$ = hex(0102030405ffffffffffffff0dff0fff00)
            gosub turn_on_off
            return

*       ** Insert Lines
L47580:     if edit% > 6% then L47670
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "(3)Copy Above                           " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = " "
            pfkeys$ = hex(01ff03ffffffffffffffffff0dff0fff00)
            return

L47670
*       ** Delete lines
            pf$(1%) = "(1)Cancel Delete                        " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = " "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        turn_on_off
            if maxlines% > 15% then L47810     /* Less than 16 lines    */
                str(pf$(2%),,35%), str(pf$(3%),,35%) = " "
                str(pfkeys$,2%,4%) = hex(ffffffff)
                return
L47810
*       ** Now find out where we are
            if idx% <> 1% then L47860
                str(pf$(2%),,35%) = " "
                str(pfkeys$,2%,1%) = hex(ff): str(pfkeys$,4%,1%) = hex(ff)
                return
L47860:     if idx% + 14% < maxlines% then return
                str(pf$(3%),,35%) = " "
                str(pfkeys$,3%,1%) = hex(ff): str(pfkeys$,5%,1%) = hex(ff)
                return

*       **** Set up Input Messages and FACs for the screens!!!!!!
L48010
*       ** Set up for Input Mode                  (EDIT% = 1%)
            if mode% = 3% then  gosub'050(3%, fieldnr%)                  ~
                          else  gosub'050(2%, fieldnr%)
            init (hex(8c)) ff$()
            if mode% = 3% then L48075
                for k% = 1 to 100                   /* Input by Demand */
                    str(ff$(),k%*8%-4%,1%) = all(hex(82))  /* 4        */
                    str(ff$(),k%*8%-3%,1%) = all(hex(81))  /* 5        */
                    str(ff$(),k%*8%-2%,1%) = all(hex(82))  /* 6        */
                    str(ff$(),k%*8%-6%,2%) = all(hex(81))  /* 2 & 3    */
                    str(ff$(),k%*8%-1%,1%) =    (hex(81))  /* 7        */
                    convert k% to demline$(k%), pic(##0)
                next k%
                return
L48075:     for k% = 1 to 100                      /* Input by Part */
                str(ff$(),k%*8%-4%,1%) = all(hex(82))   /* 4        */
                str(ff$(),k%*8%-3%,1%) = all(hex(81))   /* 5        */
                str(ff$(),k%*8%-2%,1%) = all(hex(82))   /* 6        */
                str(ff$(),k%*8%-7%,3%) = all(hex(81))   /* 1, 2 & 3 */
                str(ff$(),k%*8%-1%,1%) =    (hex(81))   /* 7        */
            next k%
            return

L48150
*       ** Set up for Error Correction Input      (EDIT% = 2%)
            inpmessage$(1) = "Make Corrections and Press RETURN"
            return

L48190
*       ** Set up for Display Only                (EDIT% = 3%)
            inpmessage$(1) = "Press RETURN for Full Screen Edit Mode"
            init(hex(8c)) ff$()
            return

L48240
*       ** Set up for Full Screen Edit Mode       (EDIT% = 4%)
            inpmessage$(1) = "Make Desired Changes and press RETURN T" & ~
                             "o Verify Data and return to display"
            if mode% = 4% then L48260
                for i% = 1% to min(oldmax%, maxlines%)
                     if reqtype$ <> " " then ff$(i%,6%) = hex(8c)
                next i%
                return
L48260:     for i% = 1% to  maxlines%
                 if insert_lines%(i%) <> 1% then                         ~
                                         ff$(i%,1%), ff$(i%,2%) = hex(8c)
                 if reqtype$ <> " " then ff$(i%,6%) = hex(8c)
            next i%
            return

L48290
*       ** Set up for Insert line on new demand   (EDIT% = 5% or 6%)
            inpmessage$(1) = "Enter data and press RETURN"
            return

L48330
*       ** Set up for Delete line from demand     (EDIT% = 7%)
            inpmessage$(1) = "Press RETURN to DELETE Flashing Line"
            return

        REM *************************************************************~
            *       T E S T   D A T A   F O R   I N P U T   M O D E     *~
            *-----------------------------------------------------------*~
            * Test data for Input Mode - by part or by demand           *~
            *************************************************************

        deffn'151
            errormsg$ = " "
            onfile% = 0%

        REM Test for Demand Code
            if demcodeselect$ <> " " and partselect$ <> " " then L50300
            if demcodeselect$ = " " then L50180
                reqtype$ = " "
                if demcodeselect$ = "?" then demcodeselect$ = " "
                call "PLOWCODE"(#1,demcodeselect$, " ", -16%, -1, f1%(1%))
                if demcodeselect$ = " " then L50330
                    if f1%(1%) = 0% then L50162
                        mode% = 2%
                        return
L50162:             call "BCKPREFX" (demcodeselect$, errormsg$)
                    if errormsg$ <> " " then return
                    mode% = 1%
                    return

L50180: REM Test for Part Number                  PARTSELECT$
            if partselect$ = " " then L50330
            call "GETCODE" (#2, partselect$, " ", 0%, 0, f1%(2%))
                if f1%(2%) = 0% then L50270
            get #2, using L50214, hnytype$, hnyprio$
L50214:         FMT POS(180), CH(10), POS(334), CH(01)
            if hnytype$ < "200" then L50390
                mode% = 3%
                if reqtype$  = " " then return
                if reqtype$ <> "3" and reqtype$ <> "4" and               ~
                   reqtype$ <> "5" and reqtype$ <> "7" and               ~
                   reqtype$ <> "8" then L50360
                return

L50270:     errormsg$ = "Part Number not found on file.  Enter Valid Part"
            return

L50300:     errormsg$ = "Enter Demand Number or Part, not both"
            return

L50330:     errormsg$ = "Enter either a Demand code or a Part Number"
            return

L50360:     errormsg$ = "Invalid Demand Type, see list below"
            return

L50390:     errormsg$ = "Part has Invalid Part Type for Planning, "  &   ~
                        "Please Enter a Different Part"
            return
        REM *************************************************************~
            *  T E S T   D A T A   F O R   F U L L   S C R E E N        *~
            *-----------------------------------------------------------*~
            * Test data for Entry/Edit by Demand Code                   *~
            *************************************************************
*       ** Start here for Entry by Demand
        deffn'152(test%)
            if test% > 1% then L51120
                top% = ml%
                mat okflag% = zer
                init (" ") error$()
                gosub array_smash

L51120:     init (hex(8c)) ff$()
            call "SHOSTAT" ("Validating Entries, one moment please")
            for idx% = 1% to top%
                if okflag%(idx%) = 3% then next_idx
                if type$(idx%) < hex(40) then error$(idx%) = " "
                if okflag%(idx%) <> 3% then okflag%(idx%) = 0%

*       ** Test data for Part Number
                if      part$(idx%) <> " " or      qty$(idx%) <> " " or     ~
                    (datereq$(idx%) <> " " and datereq$(idx%) <> blankdate$)~
                     or type$(idx%) <> " " then L51230
                    maxlines% = idx% - 1%
                    goto L52110
L51230:         if part$(idx%) <> " " then L51280
                    error$(idx%) = "Must Enter Part  "
                    ff$(idx%,3) = hex(81)
                    okflag%(idx%) = 1%
                    goto L51440
L51280:         call "GETCODE" (#2, part$(idx%), " ", 0%, 99, f1%(2%))
                if f1%(2%) <> 0 then L51340
                    error$(idx%) = "Invalid Part     "
                    ff$(idx%,3%) = hex(81)
                    okflag%(idx%) = 1%
                    goto L51440
L51340:         get #2, using L51350, hnytype$, hnyprio$
L51350:             FMT POS(180), CH(10), POS(334), CH(01)
                if hnytype$ >= "200" then L51410
L51370:             error$(idx%) = "Wrong Part Type  "
                    ff$(idx%,3%) = hex(81)
                    okflag%(idx%) = 1%
                    goto L51440
L51410:          call "READ100" (#3, part$(idx%), f1%(3%))
                 if f1%(3%) = 0 then L51370

L51440
*       ** Test for Quantities Entered
                if qty$(idx%) <> " " then L51510
                    if error$(idx%) = " " then                           ~
                                    error$(idx%) = "Must Enter Qty   "
                        ff$(idx%,4) = hex(82)
                        okflag%(idx%) = 1%
                        goto L51580
L51510:         call "NUMTEST" (qty$(idx%), .1, 9e7, err$, -2.2, qty)
                    if err$ = " " then L51580
                        if error$(idx%) = " " then                       ~
                                    error$(idx%) = "Invalid Number   "
                        ff$(idx%,4) = hex(82)
                        okflag%(idx%) = 1%

L51580
*       ** Test for Required Date Entered
                call "DATEOK" (datereq$(idx%), u3%, error$)
                if error$ = " " then L51670
                    if error$(idx%) = " " then                           ~
                                    error$(idx%) = "Invalid Date     "
                    ff$(idx%,5) = hex(80)
                    okflag%(idx%) = 1%
                    goto L51870

L51670:         testdate$ = datereq$(idx%)
                call "DATUNFMT" (testdate$)
                if testdate$ > today$ then L51770
                    if error$(idx%) = " " then                           ~
                                    error$(idx%) = "Must be > Today  "
                    ff$(idx%,5) = hex(80)
                    okflag%(idx%) = 1%
                    goto L51870

L51770:         call "DATE" addr("G-", pldate$, testdate$, today%, err%)
                    if err% <> 0 then calc_date_err
                today% = today% + 1%
                if today% >= 1% and today% <= 490% then L51870
                    if error$(idx%) = " " then                           ~
                                    error$(idx%) = "Date out of Range"
                    ff$(idx%,5) = hex(80)
                    okflag%(idx%) = 1%

L51870
*       ** Test for Demand Type Entered
                if type$(idx%) = " " then type$(idx%) = reqtype$
                if type$(idx%) = " " then type$(idx%) = "4"
                if (type$(idx%) >= "3" and type$(idx%) <= "5") or        ~
                   (type$(idx%) >= "s" and type$(idx%) <= "u") then L51970
                if (type$(idx%) >= "7" and type$(idx%) <= "8") or        ~
                   (type$(idx%) >= "w" and type$(idx%) <= "x") then L51970
                    if error$(idx%) = " " then                           ~
                                    error$(idx%) = "Invalid Dem Type"
                    ff$(idx%,6) = hex(82)
                    okflag%(idx%) = 1%

L51970
*       ** Test for Demand Priority Code Entered
                if prio$(idx%) = " " then prio$(idx%) = hnyprio$
                if prio$(idx%) = " " then prio$(idx%) = "A"
                if prio$(idx%) >= "A" and prio$(idx%) <= "Z" then L52050
                    if error$(idx%) = " " then                           ~
                                    error$(idx%) = "Priority A - Z"
                    ff$(idx%,7) = hex(81)
                    okflag%(idx%) = 1%

L52050:     if okflag%(idx%) <> 0% then ff$(idx%,8) = hex(84)
        next_idx
            next idx%

L52070:     if test% > 1% then return

            if maxlines% = 0% then maxlines% = idx%

L52110:     for j% = maxlines%+1% to ml%
                if j% <= ml% then demline$(j%) = " "
            next j%

            return




        REM *************************************************************~
            *  T E S T   D A T A   F O R   F U L L   S C R E E N        *~
            *-----------------------------------------------------------*~
            * Test data for Entry/Edit by Part Number                   *~
            *************************************************************
        deffn'153(test%)
            if test% > 1% then L53110
                mat okflag% = zer
                top% = ml%
                init(" ") error$()
                gosub array_smash

L53110:     init (hex(8c)) ff$()
            call "SHOSTAT" ("Validating Entries, one moment please")
            for idx% = 1% to top%
                if okflag%(idx%) = 3% then next_idx
                if type$(idx%) < hex(40) then error$(idx%) = " "
                if okflag%(idx%) <> 3% then okflag%(idx%) = 0%
*       ** Test data for Demand Code
                if demcode$(idx%) <> " " or     qty$(idx%)  <> " " or        ~
                  (datereq$(idx%) <> " " and datereq$(idx%) <> blankdate$)   ~
                   or type$(idx%) <> " " or        ~
                   demline$(idx%) <> " " then L53220
                   maxlines% = idx% - 1%
                   if maxlines% <> 0% then L52070
                        errormsg$ = " At Least One Demand Must Be Entered"
                        return
L53220:         if demcode$(idx%) <> " " then L53260
                    error$(idx%) = "Enter Demand Code"
                    ff$(idx%,2) = hex(81)
                    okflag%(idx%) = 1%
L53260:         call "BCKPREFX" (demcode$(idx%), errormsg$)
                if errormsg$ = " " then L53330
                    errormsg$ = " "
                    if error$(idx%) = " " then error$(idx%) =            ~
                       "Can't start w/ " & str(demcode$(idx%),,2)
                    ff$(idx%,2) = hex(81)
                    okflag%(idx%) = 1%
*        ** Test Line Numbers
L53330:         if demline$(idx%) <> " " then L53378
                    if error$(idx%) = " " then                           ~
                                    error$(idx%) = "Enter Demand Line #"
                    ff$(idx%,1) = hex(81)
                    okflag%(idx%) = 1%
L53378:         convert  demline$(idx%) to demline, data goto L53385
                if pos(demline$(idx%) = ".") = 0% and demline > 0.0      ~
                                                               then L53390
L53385:             if error$(idx%) = " " then                           ~
                                    error$(idx%) = "Invalid Line #  "
                    ff$(idx%,1%) = hex(81)
                    okflag%(idx%) = 1%
                    goto L53394
L53390:         convert demline to demline$(idx%), pic(##0)
L53394:         call "STRING" addr("RJ", demline$(idx%), 3%)
                if idx% <= oldmax% and     /* No New lines */            ~
                   insert_lines%(idx%) = 0%  /* Not an Inserted Line */  ~
                        then L51440   /* So don't test for duplication */
                    if error$(idx%) = "Duplicate Demand" then            ~
                        error$(idx%) = " "
                    init (hex(00)) readkey$
                    str(readkey$,,16) = str(demcode$(idx%),,16)
                    str(readkey$,17,3) = str(demline$(idx%),,3)
                    call "REDALT0" (#1, readkey$, 1%, f1%(1%))
                        if f1%(1%) = 0% then L53505
                    if error$(idx%) = " " then                           ~
                                    error$(idx%) = "Duplicate Demand"
                        goto L53570
*        ** Test for Unprocessed  Duplication
L53505:             if error$(idx%) = "DuplicateDemLine" then            ~
                        error$(idx%) = " "
                    dupflag% = 0%
                    for i% = 1% to maxlines%
                        if i% = idx% then L53540    /* Don't check self */
                        if demcode$(idx%) <> demcode$(i%) or             ~
                           demline$(idx%) <> demline$(i%) then  L53540
                            dupflag% = 1% :  i% = 101%
L53540:             next i%
                    if dupflag% = 0% then L53595
                    if error$(idx%) = " " then                           ~
                                    error$(idx%) = "DuplicateDemLine"
L53570:             ff$(idx%,1%), ff$(idx%,2%) = hex(81)
                    okflag%(idx%) = 1%
L53595:         goto L51440
            next idx%  /* Dummy NEXT */

        array_smash
            if mode% = 1% then templine$() = " "                         ~
                          else mat templine$ = demline$
            for i% = 1% to ml%
                smasharray$(i%) = str(demcode$(i%))  &  str(part$(i%)) & ~
                                  str(datereq$(i%))  &  str(qty$(i%))  & ~
                                  str(type$(i%))     &  str(prio$(i%)) & ~
                                  str(templine$(i%))
            next i%
            call "LINSMASH" (smasharray$())
            for i% = 1% to ml%
                demcode$ (i%) = str(smasharray$(i%),   , 16%)
                part$    (i%) = str(smasharray$(i%),17%, 25%)
                datereq$ (i%) = str(smasharray$(i%),42%,  8%)
                qty$     (i%) = str(smasharray$(i%),50%, 10%)
                type$    (i%) = str(smasharray$(i%),60%,  1%)
                prio$    (i%) = str(smasharray$(i%),61%,  1%)
                templine$(i%) = str(smasharray$(i%),62%,  3%)
            next i%
            if mode% <> 1% then  mat  demline$ = templine$
            return

        calc_date_err
            ask% = 2%
            call "ASKUSER" (ask%, "***** DATE CALC ERROR *****",         ~
                            "Cannot Calculate Planning Date",            ~
                            "Contact the Database Administrator",        ~
                            "Press RETURN to exit program")
            goto exit_program

        month_open_err
            ask% = 2%
            call "ASKUSER" (ask%, "***** MONTH OPEN ERROR *****",        ~
                            "Cannot Establish Planning Calendar Start" & ~
                            " Date", "Notify the Database Administrator",~
                            "Press RETURN to Exit Program")
            goto exit_program

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
            end

