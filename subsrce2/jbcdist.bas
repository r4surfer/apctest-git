        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  JJJJJ  BBBB    CCC   DDDD   IIIII   SSS   TTTTT          *~
            *    J    B   B  C   C  D   D    I    S        T            *~
            *    J    BBBB   C      D   D    I     SSS     T            *~
            *  J J    B   B  C   C  D   D    I        S    T            *~
            *   J     BBBB    CCC   DDDD   IIIII   SSS     T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBCDIST  - DISTRIBUTE COSTS TO A JOB.  CLONED FROM        *~
            *            HNYCDIST.                                      *~
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
            * 06/16/87 ! ORIGINAL                                 ! KEN *~
            * 10/20/92 ! PRR 12642 - Dim var BUCKETDESCR$() to 12 ! RJH *~
            *          !  to match dim of called Sub STCSETID     !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "JBCDIST"                                                    ~
                    (mode$,              /* Mode (D = Display Only)    */~
                                         /*      (N = Total not Edited)*/~
                     std%,               /* Which Standard Element?    */~
                                         /*   1 = BOM                  */~
                                         /*   2 = RTE (WC and Labor)   */~
                                         /*   3 = MISC                 */~
                                         /*   4 = Folded Standard      */~
                                         /*   5 = Rolled Standard      */~
                                         /*   6 = This Level (2 + 3)   */~
                     part$,              /* Part Code                  */~
                     partdescr$,         /* Part Description           */~
                     line$,              /* Line 2 (up to 60 char)     */~
                     #1,                 /* SYSFILE2 Channel           */~
                     cost$,              /* Costs Passed (12*PD(14,4)) */~
                     total$,             /* Total (Returned)           */~
                                         /* I Mode = Value to Dist.    */~
                     total,              /* Total (Returned)           */~
                                         /* In = Job Quantity          */~
                     jbcost$,            /* Current Job Costs          */~
                     jbtotal)            /* Current Job Total          */~

        dim                                                              ~
            addcost$10,                  /* ADD cost                   */~
            addcost$(12)10,              /* ADD cost                   */~
            addcost(12),                 /* ADD cost                   */~
            addcosthold(12),             /* ADD cost                   */~
            addpct$(12)6,                /* ADD pct                    */~
            bomcost(12),                 /* BOM Costs                  */~
            bucketdescr$(12)20,          /* Bucket Descr               */~
            bucketid$(12)10,             /* bucket id                  */~
            bucketnr$(12)2,              /* bucket number              */~
            cost$96,                     /* Costs (Mod.) 12*PD(14,4)   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            header$79,                   /* header                     */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            jbcost$96,                   /* Job Costs (Current)        */~
            jbcost(12),                  /* Job Costs (Current)        */~
            jbcosts$(12)10,              /* Job Costs (Display)        */~
            jbpct$(12)6,                 /* Job Pct   (Display)        */~
            jbtotal$10,                  /* Job Total                  */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line$60,                     /* Second Line (Passed Part)  */~
            line2$79,                    /* Second Line of Screen Headr*/~
            misccost(12),                /* Misc. Costs                */~
            mode$1,                      /* Mode, Display or Edit      */~
            part$25,                     /* Part                       */~
            partdescr$34,                /* Part  Description          */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rtecost(12),                 /* RTE Costs                  */~
            setid$8,                     /* Current Cost Set Id        */~
            stdcost$10,                  /* std cost                   */~
            stdcost$(12)10,              /* std cost                   */~
            stdcost(12),                 /* std cost                   */~
            stdpct$(12)6,                /* std pct                    */~
            total(1),                    /* Total Inventory Cost       */~
            total$10,                    /* Total Cost (returned)      */~
            twork(1,12),                 /* Work Array                 */~
            userid$3                     /* Current User Id            */~

        dim f2%(1)                       /* = 0 if the file is open    */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "
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
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "RETURN to Edit."                 &           ~
                           "  PF8 to Prorate (Std. Cost %)." &           ~
                           "  PF10 to Prorate (Job Cost %)."

            header$ = " "
            str(header$, 3,11) = hex(ac) & "Bucket ID "
            str(header$,14,11) = hex(ac) & " Cost Amt."
            str(header$,25,11) = hex(ac) & "% of Total"
            str(header$,36,11) = hex(ac) & " Job Costs"
            str(header$,47,11) = hex(ac) & "% of Total"
            str(header$,58,11) = hex(ac) & " xxxx Std."
            str(header$,69,11) = hex(ac) & "% of Total"

            if buckets% <> 0% then L09225

*        Get Cost Bucket Descriptors
            buckets% = 3%
            call "STCSETID" (buckets%, #1, setid$, addcost$, bucketid$(),~
                             bucketdescr$(), readkey$)
                if buckets% > 0% then L09155
            setid$ = " ":def% = 1%
            goto L09190

L09155:     get #1, using L09160, def%
L09160:         FMT POS(442), BI(1)
            if def% < 1% or def% > 12% then def% = 1%

                if buckets% = 12% then L09190
            temp% = 10% * buckets% + 1%:str(bucketid$(),temp%)    = " "
            temp% = 20% * buckets% + 1%:str(bucketdescr$(),temp%) = " "
L09190:     if buckets% = 0% then buckets% = 12%
            str(bucketnr$()) = " "

            for i% = 1% to buckets%
                convert i% to bucketnr$(i%), pic(##)
            next i%

L09225:     init (" ") jbcosts$(),jbpct$(),stdcost$(),stdpct$(),readkey$
            mat stdcost = zer: stdcost = 0
            mat bomcost = zer: mat rtecost = zer: mat misccost = zer

            get str(jbcost$) using L09250, jbcost()
L09250:         FMT 12*PD(14,4)
            if setid$ = " " then L09280

            call "STCCOSTS" (part$, setid$, #1, 3%, stdcost, stdcost(),  ~
                             bomcost(), rtecost(), misccost())

L09280:     on std% goto L09295, L09300, L09305, L09335, L09315, L09331
               goto L09335

L09295:     mat stdcost = bomcost :str(header$,60,4) = " BOM":goto L09345
L09300:     mat stdcost = rtecost :str(header$,60,4) = " RTE":goto L09345
L09305:     mat stdcost = misccost:str(header$,60,4) = "MISC":goto L09345

L09315:     mat stdcost = bomcost + rtecost
            mat stdcost = stdcost + misccost
            str(header$,60,4) = "Rld.":goto L09345

L09331:     mat stdcost = rtecost + misccost
            str(header$,60,4) = "T.L.":goto L09345

L09335:     str(header$,60,4) = "Fld."

L09345:     mat stdcost = (total) * stdcost
            mat twork = con:mat total = twork*stdcost:stdcost = total(1)

            for i% = 1% to buckets%
                call "CONVERT" (stdcost(i%), 4.4, stdcost$(i%))
                call "CONVERT" (jbcost(i%),  4.4, jbcosts$(i%))
                    if stdcost = 0 then L09385
                       temp = stdcost(i%) * 100 / stdcost
                       call "CONVERT" (temp, 2.2, stdpct$(i%))
L09385:             if jbtotal = 0 then L09400
                       temp = jbcost(i%) * 100 / jbtotal
                       call "CONVERT" (temp, 2.2, jbpct$(i%))
L09400:     next i%

            call "CONVERT" (stdcost, 4.4, stdcost$)
            call "CONVERT" (jbtotal, 4.4, jbtotal$)

            get str(cost$) using L09430, addcost()
L09430:         FMT 12*PD(14,4)

        inputmode_restart

            total = 0: convert total$ to total, data goto L09450

L09450:     mat addcosthold = addcost : errormsg$ = " "
            fieldnr% = 1%

            mat twork = con : mat total = twork * addcost
            addcost = total(1)
            if mode$ = "N" then addcost = total

            call "CONVERT" (addcost, 4.4, addcost$)
            addpct$() = " "
            for i% = 1% to buckets%
                call "CONVERT" (addcost(i%), 4.4, addcost$(i%))
                    if addcost = 0 then L09515
                       temp = addcost(i%) * 100 / addcost
                       call "CONVERT" (temp, 2.2, addpct$(i%))
L09515:     next i%

            if mode$ = "D" then editpg1

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode

                pf16$ = " "
                gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10240
L10140:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  0 then       L10140
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10140

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            pf16$ = "(16)Exit Dist."
            inpmessage$ = edtmessage$
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       exit_program
            if mode$ = "D" then editpg1
                  if keyhit% <>  0 then       L11180
                     fieldnr% = 1%
                     goto inputmode
L11180:           if keyhit% <>  8 then       L11220
                     if stdcost  =  0 then       editpg1
                        fieldnr% = 2%
                        goto inputmode
L11220:           if keyhit% <> 10 then       editpg1
                     if jbtotal  =  0 then       editpg1
                        fieldnr% = 3%
                        goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20100,             /* Costs              */~
                              L20200,             /* Prorate STD        */~
                              L20300              /* Prorate JOB        */~

            return

L20100: REM Def/Enable Costs
            inpmessage$ = "Enter Cost Distribution (per Part)."
            return

L20200: REM Def/Enable Prorate
            inpmessage$ = "Enter Total Cost for Distribution (per Part)."
            inpmessage$ = inpmessage$ & " (Blank = Standard)"
            if mode$ = "N" then enabled% = 0%
            return

L20300: REM Def/Enable Prorate
            inpmessage$ = "Enter Total Cost for Distribution (per Part)."
            inpmessage$ = inpmessage$ & " (Blank = Std - Job)"
            if mode$ = "N" then enabled% = 0%
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
            mat addcost = addcosthold
            goto inputmode_restart

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              str(line2$,1%,60%) = line$
              str(line2$,62%) = " JBCDIST: " & str(cms2v$,,8%)
              init(hex(84)) lfac$():lfac$(12) = hex(a4)
              on fieldnr% gosub L40160,            /* Costs             */~
                                L40190,            /* Prorate           */~
                                L40190             /* Prorate           */~

              goto L40210

L40160:           init (hex(82)) str(lfac$(),1,buckets%)
                  if buckets% = 12% then lfac$(12) = hex(a2)
                     return
L40190:           lfac$(20) = hex(82):return

L40210:     accept                                                       ~
               at (01,02),                                               ~
                  "Distribute Job Costs",                                ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Part:",                                      ~
               at (04,08), fac(hex(84)), part$                  , ch(25),~
               at (04,34), "Desc:",                                      ~
               at (04,40), fac(hex(84)), partdescr$             , ch(34),~
                                                                         ~
               at (06,02), fac(hex(8c)), header$                , ch(79),~
                                                                         ~
               at (07,02), fac(hex(8c)), bucketnr$( 1)          , ch(02),~
               at (08,02), fac(hex(8c)), bucketnr$( 2)          , ch(02),~
               at (09,02), fac(hex(8c)), bucketnr$( 3)          , ch(02),~
               at (10,02), fac(hex(8c)), bucketnr$( 4)          , ch(02),~
               at (11,02), fac(hex(8c)), bucketnr$( 5)          , ch(02),~
               at (12,02), fac(hex(8c)), bucketnr$( 6)          , ch(02),~
               at (13,02), fac(hex(8c)), bucketnr$( 7)          , ch(02),~
               at (14,02), fac(hex(8c)), bucketnr$( 8)          , ch(02),~
               at (15,02), fac(hex(8c)), bucketnr$( 9)          , ch(02),~
               at (16,02), fac(hex(8c)), bucketnr$(10)          , ch(02),~
               at (17,02), fac(hex(8c)), bucketnr$(11)          , ch(02),~
               at (18,02), fac(hex(8c)), bucketnr$(12)          , ch(02),~
                                                                         ~
               at (07,05), fac(hex(8c)), bucketid$( 1)          , ch(10),~
               at (08,05), fac(hex(8c)), bucketid$( 2)          , ch(10),~
               at (09,05), fac(hex(8c)), bucketid$( 3)          , ch(10),~
               at (10,05), fac(hex(8c)), bucketid$( 4)          , ch(10),~
               at (11,05), fac(hex(8c)), bucketid$( 5)          , ch(10),~
               at (12,05), fac(hex(8c)), bucketid$( 6)          , ch(10),~
               at (13,05), fac(hex(8c)), bucketid$( 7)          , ch(10),~
               at (14,05), fac(hex(8c)), bucketid$( 8)          , ch(10),~
               at (15,05), fac(hex(8c)), bucketid$( 9)          , ch(10),~
               at (16,05), fac(hex(8c)), bucketid$(10)          , ch(10),~
               at (17,05), fac(hex(8c)), bucketid$(11)          , ch(10),~
               at (18,05), fac(hex(ac)), bucketid$(12)          , ch(10),~
                                                                         ~
               at (19,05), "* Totals *",                                 ~
                                                                         ~
               at (07,16), fac(lfac$( 1)), addcost$( 1)         , ch(10),~
               at (08,16), fac(lfac$( 2)), addcost$( 2)         , ch(10),~
               at (09,16), fac(lfac$( 3)), addcost$( 3)         , ch(10),~
               at (10,16), fac(lfac$( 4)), addcost$( 4)         , ch(10),~
               at (11,16), fac(lfac$( 5)), addcost$( 5)         , ch(10),~
               at (12,16), fac(lfac$( 6)), addcost$( 6)         , ch(10),~
               at (13,16), fac(lfac$( 7)), addcost$( 7)         , ch(10),~
               at (14,16), fac(lfac$( 8)), addcost$( 8)         , ch(10),~
               at (15,16), fac(lfac$( 9)), addcost$( 9)         , ch(10),~
               at (16,16), fac(lfac$(10)), addcost$(10)         , ch(10),~
               at (17,16), fac(lfac$(11)), addcost$(11)         , ch(10),~
               at (18,16), fac(lfac$(12)), addcost$(12)         , ch(10),~
                                                                         ~
               at (19,16), fac(lfac$(20)), addcost$             , ch(10),~
                                                                         ~
               at (07,29), fac(hex(8c)), addpct$( 1)            , ch(06),~
               at (08,29), fac(hex(8c)), addpct$( 2)            , ch(06),~
               at (09,29), fac(hex(8c)), addpct$( 3)            , ch(06),~
               at (10,29), fac(hex(8c)), addpct$( 4)            , ch(06),~
               at (11,29), fac(hex(8c)), addpct$( 5)            , ch(06),~
               at (12,29), fac(hex(8c)), addpct$( 6)            , ch(06),~
               at (13,29), fac(hex(8c)), addpct$( 7)            , ch(06),~
               at (14,29), fac(hex(8c)), addpct$( 8)            , ch(06),~
               at (15,29), fac(hex(8c)), addpct$( 9)            , ch(06),~
               at (16,29), fac(hex(8c)), addpct$(10)            , ch(06),~
               at (17,29), fac(hex(8c)), addpct$(11)            , ch(06),~
               at (18,29), fac(hex(8c)), addpct$(12)            , ch(06),~
                                                                         ~
               at (07,38), fac(hex(8c)), jbcosts$( 1)           , ch(10),~
               at (08,38), fac(hex(8c)), jbcosts$( 2)           , ch(10),~
               at (09,38), fac(hex(8c)), jbcosts$( 3)           , ch(10),~
               at (10,38), fac(hex(8c)), jbcosts$( 4)           , ch(10),~
               at (11,38), fac(hex(8c)), jbcosts$( 5)           , ch(10),~
               at (12,38), fac(hex(8c)), jbcosts$( 6)           , ch(10),~
               at (13,38), fac(hex(8c)), jbcosts$( 7)           , ch(10),~
               at (14,38), fac(hex(8c)), jbcosts$( 8)           , ch(10),~
               at (15,38), fac(hex(8c)), jbcosts$( 9)           , ch(10),~
               at (16,38), fac(hex(8c)), jbcosts$(10)           , ch(10),~
               at (17,38), fac(hex(8c)), jbcosts$(11)           , ch(10),~
               at (18,38), fac(hex(ac)), jbcosts$(12)           , ch(10),~
                                                                         ~
               at (19,38), fac(hex(8c)), jbtotal$               , ch(10),~
                                                                         ~
               at (07,51), fac(hex(8c)), jbpct$ ( 1)            , ch(06),~
               at (08,51), fac(hex(8c)), jbpct$ ( 2)            , ch(06),~
               at (09,51), fac(hex(8c)), jbpct$ ( 3)            , ch(06),~
               at (10,51), fac(hex(8c)), jbpct$ ( 4)            , ch(06),~
               at (11,51), fac(hex(8c)), jbpct$ ( 5)            , ch(06),~
               at (12,51), fac(hex(8c)), jbpct$ ( 6)            , ch(06),~
               at (13,51), fac(hex(8c)), jbpct$ ( 7)            , ch(06),~
               at (14,51), fac(hex(8c)), jbpct$ ( 8)            , ch(06),~
               at (15,51), fac(hex(8c)), jbpct$ ( 9)            , ch(06),~
               at (16,51), fac(hex(8c)), jbpct$ (10)            , ch(06),~
               at (17,51), fac(hex(8c)), jbpct$ (11)            , ch(06),~
               at (18,51), fac(hex(8c)), jbpct$ (12)            , ch(06),~
                                                                         ~
               at (07,60), fac(hex(8c)), stdcost$( 1)           , ch(10),~
               at (08,60), fac(hex(8c)), stdcost$( 2)           , ch(10),~
               at (09,60), fac(hex(8c)), stdcost$( 3)           , ch(10),~
               at (10,60), fac(hex(8c)), stdcost$( 4)           , ch(10),~
               at (11,60), fac(hex(8c)), stdcost$( 5)           , ch(10),~
               at (12,60), fac(hex(8c)), stdcost$( 6)           , ch(10),~
               at (13,60), fac(hex(8c)), stdcost$( 7)           , ch(10),~
               at (14,60), fac(hex(8c)), stdcost$( 8)           , ch(10),~
               at (15,60), fac(hex(8c)), stdcost$( 9)           , ch(10),~
               at (16,60), fac(hex(8c)), stdcost$(10)           , ch(10),~
               at (17,60), fac(hex(8c)), stdcost$(11)           , ch(10),~
               at (18,60), fac(hex(ac)), stdcost$(12)           , ch(10),~
                                                                         ~
               at (19,60), fac(hex(8c)), stdcost$               , ch(10),~
                                                                         ~
               at (07,73), fac(hex(8c)), stdpct$( 1)            , ch(06),~
               at (08,73), fac(hex(8c)), stdpct$( 2)            , ch(06),~
               at (09,73), fac(hex(8c)), stdpct$( 3)            , ch(06),~
               at (10,73), fac(hex(8c)), stdpct$( 4)            , ch(06),~
               at (11,73), fac(hex(8c)), stdpct$( 5)            , ch(06),~
               at (12,73), fac(hex(8c)), stdpct$( 6)            , ch(06),~
               at (13,73), fac(hex(8c)), stdpct$( 7)            , ch(06),~
               at (14,73), fac(hex(8c)), stdpct$( 8)            , ch(06),~
               at (15,73), fac(hex(8c)), stdpct$( 9)            , ch(06),~
               at (16,73), fac(hex(8c)), stdpct$(10)            , ch(06),~
               at (17,73), fac(hex(8c)), stdpct$(11)            , ch(06),~
               at (18,73), fac(hex(8c)), stdpct$(12)            , ch(06),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(0001080a0d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L41610
                  call "MANUAL" ("JBCDIST")
                  goto L40210

L41610:        if keyhit% <> 15 then L41650
                  call "PRNTSCRN"
                  goto L40210

L41650:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,              /* Costs             */~
                              L50200,              /* Prorate (Std)     */~
                              L50500               /* Prorate (Job)     */~

            return
L50100: REM Test for Costs
            for i% = 1% to buckets%
            call "NUMTEST" (addcost$(i%), -9e7, 9e7, errormsg$, -4.4,    ~
                                                             addcost(i%))
                if errormsg$ <> " " then return
            next i%
            gosub L50800
            if mode$ <> "N" then return
            if addcost = total then return
               errormsg$ = "Distribution Incomplete:"
                 call "CONVERT" (total - addcost, -4.4,                  ~
                                 str(errormsg$,len(errormsg$) + 2%, 10%))
               addcost = total
               call "CONVERT" (addcost, 4.4, addcost$)
               return

L50200: REM Test for Prorate (STD)
            if addcost$ = " " then addcost$ = stdcost$
            call "NUMTEST" (addcost$, -9e7, 9e7, errormsg$, -4.4, addcost)
                if errormsg$ <> " " then return
            if addcost <> stdcost then L50290
               mat addcost  = stdcost
               mat addcost$ = stdcost$
               mat addpct$  = stdpct$
               return
L50290:     mat addcost  = zer
            if stdcost <> 0 then L50350
               addcost(def%) = addcost
               str(addcost$()) = " "
               call "CONVERT" (addcost(def%), 4.4, addcost$(def%))
               goto L50390
L50350:     for i% = 1% to buckets%
                addcost(i%) = round(addcost*stdcost(i%)/stdcost,4)
                call "CONVERT" (addcost(i%), 4.4, addcost$(i%))
            next i%
L50390:     addcosthold = addcost
            gosub L50800
            if addcost = addcosthold then return
               fieldnr% = 1%
               return clear all
                 errormsg$ = "CAUTION: Rounding Error:"
                 call "CONVERT" (addcosthold - addcost, -4.4,            ~
                                 str(errormsg$,len(errormsg$) + 2%, 10%))
                 goto inputmode


L50500: REM Test for Prorate (Job)
            if addcost$ <> " " then L50570
               mat addcost = stdcost - jbcost
                  for i% = 1% to buckets%
                     call "CONVERT" (addcost(i%), 4.4, addcost$(i%))
                  next i%
               goto L50100
L50570:     call "NUMTEST" (addcost$, -9e7, 9e7, errormsg$, -4.4, addcost)
                if errormsg$ <> " " then return
            mat addcost  = zer
            if jbtotal <> 0 then L50650
               addcost(def%) = addcost
               str(addcost$()) = " "
               call "CONVERT" (addcost(def%), 4.4, addcost$(def%))
               goto L50690
L50650:     for i% = 1% to buckets%
                addcost(i%) = round(addcost*jbcost(i%)/jbtotal,4)
                call "CONVERT" (addcost(i%), 4.4, addcost$(i%))
            next i%
L50690:     addcosthold = addcost
            gosub L50800
            if addcost = addcosthold then return
               fieldnr% = 1%
               return clear all
                 errormsg$ = "CAUTION: Rounding Error:"
                 call "CONVERT" (addcosthold - addcost, -4.4,            ~
                                 str(errormsg$,len(errormsg$) + 2%, 10%))
                 goto inputmode


L50800:     mat twork = con:mat total = twork * addcost
            addcost = total(1)
            call "CONVERT" (addcost, 4.4, addcost$)
            addpct$() = " "
            if addcost = 0 then return
            for i% = 1% to buckets%
               temp = 100 * addcost(i%)/addcost
               call "CONVERT" (temp, 2.2, addpct$(i%))
            next i%
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

            call "PACKZERO" (addcost(), cost$)
            total$ = addcost$ : total = addcost

            end
