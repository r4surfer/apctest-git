        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y   CCC   DDDD   IIIII   SSS   TTTTT   *~
            *  H   H  NN  N  Y   Y  C   C  D   D    I    S        T     *~
            *  HHHHH  N N N   YYY   C      D   D    I     SSS     T     *~
            *  H   H  N  NN    Y    C   C  D   D    I        S    T     *~
            *  H   H  N   N    Y     CCC   DDDD   IIIII   SSS     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYCDIST - Enter cost for each of the (up to) 12 cost     *~
            *            distribution buckets.  Note that this is       *~
            *            strictly entry/edit & possibly display only.   *~
            *            No balancing act is required here.             *~
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
            * 04/20/87 ! Original                                 ! KEN *~
            * 05/12/88 ! Honor Specific Bucket for Pur. Costs     ! KAB *~
            * 06/02/89 ! Fudge the invcost bucket totals if there ! JDH *~
            *          !  is a rounding error of .0001            !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "HNYCDIST"                                                   ~
                    (mode$,              /* Mode (D = Display Only)    */~
                                         /*      (I = Initialize)      */~
                                         /* RET  (E = Init forced Edit)*/~
                     part$,              /* Part Code                  */~
                     partdescr$,         /* Part Description           */~
                     line$,              /* Line 2 (up to 60 char)     */~
                     #1,                 /* SYSFILE2 Channel           */~
                     cost$,              /* Costs Passed (12*PD(14,4)) */~
                     total$,             /* Total (Returned)           */~
                                         /* I Mode = Value to Dist.    */~
                     total)              /* Total (Returned)           */~

        dim                                                              ~
            bucketdescr$(12)20,          /* bucket descr               */~
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
            invcost$10,                  /* inv cost                   */~
            invcost$(12)10,              /* inv cost                   */~
            invcost(12),                 /* inv cost                   */~
            invcosthold(12),             /* inv cost                   */~
            invpct$(12)6,                /* inv pct                    */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line$60,                     /* Second Line (Passed Part)  */~
            line2$79,                    /* Second Line of Screen Headr*/~
            mode$1,                      /* Mode, Display or Edit      */~
            part$25,                     /* Part                       */~
            partdescr$34,                /* Part  Description          */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
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
            cms2v$ = "R6.00.00 01/19/90 CMS2 / CMS-I Merge              "
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
            edtmessage$  = "RETURN to Edit Costs."  &                    ~
                  "  PF8 to Enter Total Cost to Prorate via Std. Cost %."

            header$ = " "
            str(header$, 3,11) = hex(ac) & "Bucket ID"
            str(header$,14,21) = hex(ac) & "Bucket Description"
            str(header$,35,11) = hex(ac) & " Inv. Cost"
            str(header$,46,11) = hex(ac) & "% of Total"
            str(header$,57,11) = hex(ac) & " Std. Cost"
            str(header$,68,12) = hex(ac) & "% of Total" & hex(8c)

            if buckets% <> 0% then L09460

*        Get Cost Bucket Descriptors
            buckets% = 3%
            call "STCSETID" (buckets%, #1, setid$, invcost$, bucketid$(),~
                             bucketdescr$(), readkey$)
                if buckets% > 0% then L09290
            setid$ = " ":def% = 1%
            goto L09390

L09290:     get #1, using L09300, def%
L09300:         FMT POS(442), BI(1)
            if def% < 1% or def% > 12% then def% = 1%

                if buckets% = 12% then L09390
            temp% = 10% * buckets% + 1%:str(bucketid$(),temp%)    = " "
            temp% = 20% * buckets% + 1%:str(bucketdescr$(),temp%) = " "
L09390:     if buckets% = 0% then buckets% = 12%
            str(bucketnr$()) = " "

            for i% = 1% to buckets%
                convert i% to bucketnr$(i%), pic(##)
            next i%

L09460:     init (" ") stdcost$(), stdpct$(), readkey$
            mat stdcost = zer: stdcost = 0
            if setid$ = " " then L09490

            pdef% = 2%
            call "STCCOSTS" (part$, setid$, #1, pdef%, stdcost, stdcost())

L09490:     for i% = 1% to buckets%
                call "CONVERT" (stdcost(i%), 4.4, stdcost$(i%))
                    if stdcost = 0 then L09540
                       temp = stdcost(i%) * 100 / stdcost
                       call "CONVERT" (temp, 2.2, stdpct$(i%))
L09540:     next i%

            call "CONVERT" (stdcost, 4.4, stdcost$)

            get str(cost$) using L09590, invcost()
L09590:         FMT 12*PD(14,4)

        inputmode_restart

            mat invcosthold = invcost : errormsg$ = " "
            fieldnr% = 1%

            mat twork = con : mat total = twork * invcost
            invcost = total(1)

            call "CONVERT" (invcost, 4.4, invcost$)
            invpct$() = " "
            for i% = 1% to buckets%
                call "CONVERT" (invcost(i%), 4.4, invcost$(i%))
                    if invcost = 0 then L09760
                       temp = invcost(i%) * 100 / invcost
                       call "CONVERT" (temp, 2.2, invpct$(i%))
L09760:     next i%

            if mode$ = "D" then editpg1

            if mode$ <> "I" then inputmode
               if pdef% < 0% then L09840
                 if pdef% = 0% then pdef% = def%
                    invcost$() = " ":invcost$(pdef%) = total$
                    gosub'151(1%)
                 if errormsg$ = " " then exit_program
                    goto inputmode
L09840:        pdef% = def%:invcost$ = total$:gosub'151(2%)
                 if errormsg$ = " " then exit_program

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
                if mode$ = "I" then mode$ = "E"
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
            if mode$ = "D" then inpmessage$ = " "                        ~
                           else inpmessage$ = edtmessage$
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       exit_program
            if mode$ = "D" then editpg1
                  if keyhit% <>  0 then       L11180
                     fieldnr% = 1%
                     goto inputmode
L11180
*                IF STDCOST  =  0 THEN       EDITPG1
                  if keyhit% <>  8 then       editpg1
                     fieldnr% = 2%
                     goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Costs              */    ~
                              L20200          /* Prorate            */
            return
L20100: REM Def/Enable Costs
            inpmessage$ = "Enter Cost Distribution (per Part)."
            return
L20200: REM Def/Enable Prorate
            inpmessage$ = "Enter Total Cost for Distribution (per Part)."
            inpmessage$ = inpmessage$ & " (Blank = Standard)"
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
            mat invcost = invcosthold
            goto inputmode_restart

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              str(line2$,1%,60%) = line$
              str(line2$,62%) = "HNYCDIST: " & str(cms2v$,,8%)
              init(hex(84)) lfac$():lfac$(12) = hex(a4)
              on fieldnr% gosub L40210,         /* Costs             */   ~
                                L40230          /* Prorate           */
              goto L40250

L40210:           init (hex(82)) str(lfac$(),1,buckets%)
                  if buckets% = 12% then lfac$(12) = hex(a2)
                     return
L40230:           lfac$(20) = hex(82):return

L40250:     accept                                                       ~
               at (01,02),                                               ~
                  "Distribute Inventory Costs",                          ~
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
               at (18,05), fac(hex(8c)), bucketid$(12)          , ch(10),~
                                                                         ~
               at (07,16), fac(hex(8c)), bucketdescr$( 1)       , ch(20),~
               at (08,16), fac(hex(8c)), bucketdescr$( 2)       , ch(20),~
               at (09,16), fac(hex(8c)), bucketdescr$( 3)       , ch(20),~
               at (10,16), fac(hex(8c)), bucketdescr$( 4)       , ch(20),~
               at (11,16), fac(hex(8c)), bucketdescr$( 5)       , ch(20),~
               at (12,16), fac(hex(8c)), bucketdescr$( 6)       , ch(20),~
               at (13,16), fac(hex(8c)), bucketdescr$( 7)       , ch(20),~
               at (14,16), fac(hex(8c)), bucketdescr$( 8)       , ch(20),~
               at (15,16), fac(hex(8c)), bucketdescr$( 9)       , ch(20),~
               at (16,16), fac(hex(8c)), bucketdescr$(10)       , ch(20),~
               at (17,16), fac(hex(8c)), bucketdescr$(11)       , ch(20),~
               at (18,16), fac(hex(ac)), bucketdescr$(12)       , ch(20),~
                                                                         ~
               at (19,16), "* * Totals * *",                             ~
                                                                         ~
               at (07,37), fac(lfac$( 1)), invcost$( 1)         , ch(10),~
               at (08,37), fac(lfac$( 2)), invcost$( 2)         , ch(10),~
               at (09,37), fac(lfac$( 3)), invcost$( 3)         , ch(10),~
               at (10,37), fac(lfac$( 4)), invcost$( 4)         , ch(10),~
               at (11,37), fac(lfac$( 5)), invcost$( 5)         , ch(10),~
               at (12,37), fac(lfac$( 6)), invcost$( 6)         , ch(10),~
               at (13,37), fac(lfac$( 7)), invcost$( 7)         , ch(10),~
               at (14,37), fac(lfac$( 8)), invcost$( 8)         , ch(10),~
               at (15,37), fac(lfac$( 9)), invcost$( 9)         , ch(10),~
               at (16,37), fac(lfac$(10)), invcost$(10)         , ch(10),~
               at (17,37), fac(lfac$(11)), invcost$(11)         , ch(10),~
               at (18,37), fac(lfac$(12)), invcost$(12)         , ch(10),~
                                                                         ~
               at (19,37), fac(lfac$(20)), invcost$             , ch(10),~
                                                                         ~
               at (07,50), fac(hex(8c)), invpct$( 1)            , ch(06),~
               at (08,50), fac(hex(8c)), invpct$( 2)            , ch(06),~
               at (09,50), fac(hex(8c)), invpct$( 3)            , ch(06),~
               at (10,50), fac(hex(8c)), invpct$( 4)            , ch(06),~
               at (11,50), fac(hex(8c)), invpct$( 5)            , ch(06),~
               at (12,50), fac(hex(8c)), invpct$( 6)            , ch(06),~
               at (13,50), fac(hex(8c)), invpct$( 7)            , ch(06),~
               at (14,50), fac(hex(8c)), invpct$( 8)            , ch(06),~
               at (15,50), fac(hex(8c)), invpct$( 9)            , ch(06),~
               at (16,50), fac(hex(8c)), invpct$(10)            , ch(06),~
               at (17,50), fac(hex(8c)), invpct$(11)            , ch(06),~
               at (18,50), fac(hex(8c)), invpct$(12)            , ch(06),~
                                                                         ~
               at (07,59), fac(hex(8c)), stdcost$( 1)           , ch(10),~
               at (08,59), fac(hex(8c)), stdcost$( 2)           , ch(10),~
               at (09,59), fac(hex(8c)), stdcost$( 3)           , ch(10),~
               at (10,59), fac(hex(8c)), stdcost$( 4)           , ch(10),~
               at (11,59), fac(hex(8c)), stdcost$( 5)           , ch(10),~
               at (12,59), fac(hex(8c)), stdcost$( 6)           , ch(10),~
               at (13,59), fac(hex(8c)), stdcost$( 7)           , ch(10),~
               at (14,59), fac(hex(8c)), stdcost$( 8)           , ch(10),~
               at (15,59), fac(hex(8c)), stdcost$( 9)           , ch(10),~
               at (16,59), fac(hex(8c)), stdcost$(10)           , ch(10),~
               at (17,59), fac(hex(8c)), stdcost$(11)           , ch(10),~
               at (18,59), fac(hex(ac)), stdcost$(12)           , ch(10),~
                                                                         ~
               at (19,59), fac(hex(8c)), stdcost$               , ch(10),~
                                                                         ~
               at (07,72), fac(hex(8c)), stdpct$( 1)            , ch(06),~
               at (08,72), fac(hex(8c)), stdpct$( 2)            , ch(06),~
               at (09,72), fac(hex(8c)), stdpct$( 3)            , ch(06),~
               at (10,72), fac(hex(8c)), stdpct$( 4)            , ch(06),~
               at (11,72), fac(hex(8c)), stdpct$( 5)            , ch(06),~
               at (12,72), fac(hex(8c)), stdpct$( 6)            , ch(06),~
               at (13,72), fac(hex(8c)), stdpct$( 7)            , ch(06),~
               at (14,72), fac(hex(8c)), stdpct$( 8)            , ch(06),~
               at (15,72), fac(hex(8c)), stdpct$( 9)            , ch(06),~
               at (16,72), fac(hex(8c)), stdpct$(10)            , ch(06),~
               at (17,72), fac(hex(8c)), stdpct$(11)            , ch(06),~
               at (18,72), fac(hex(8c)), stdpct$(12)            , ch(06),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(0001080d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L40750
                  call "MANUAL" ("HNYCDIST")
                  goto L40250

L40750:        if keyhit% <> 15 then L40790
                  call "PRNTSCRN"
                  goto L40250

L40790:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* Costs             */     ~
                              L50500          /* Prorate           */
            return
L50100: REM Test for Costs
            for i% = 1% to buckets%
            call "NUMTEST" (invcost$(i%), 0, 9e7, errormsg$, -4.4,       ~
                                                             invcost(i%))
                if errormsg$ <> " " then return
            next i%
            goto L50800

L50500: REM Test for Prorate
            if invcost$ = " " then invcost$ = stdcost$
            call "NUMTEST" (invcost$, 0, 9e7, errormsg$, -4.4, invcost)
                if errormsg$ <> " " then return
            if invcost <> stdcost then L50590
               mat invcost  = stdcost
               mat invcost$ = stdcost$
               mat invpct$  = stdpct$
               return
L50590:     mat invcost  = zer
            if stdcost <> 0 then L50600
               if pdef% < 1% or pdef% > buckets% then pdef% = def%
               invcost(pdef%) = invcost
               str(invcost$()) = " "
               call "CONVERT" (invcost(pdef%), 4.4, invcost$(def%))
               goto L50640
               str(invcost$(),11) = " "
L50600:     for i% = 1% to buckets%
                invcost(i%) = round(invcost*stdcost(i%)/stdcost,4)
                call "CONVERT" (invcost(i%), 4.4, invcost$(i%))
            next i%
L50640:     invcosthold = invcost
            gosub L50800
            if invcost = invcosthold then return
               fieldnr% = 1%
               return clear all
                 errormsg$ = "CAUTION: Rounding Error:"
                 call "CONVERT" (invcosthold - invcost, -4.4,            ~
                                 str(errormsg$,len(errormsg$) + 2%, 10%))
                 goto inputmode

L50800:     mat twork = con:mat total = twork * invcost
            invcost = total(1)
            if abs(invcost - invcosthold) > .0005 then L51040
                fudge = invcosthold - invcost
                if fudge = 0 then L51040
                invcost = invcost + fudge
                hi% = 1%
                for i% = 2% to buckets%
                     if invcost(i%) > invcost(hi%) then hi% = i%
                next i%
                invcost(hi%) = invcost(hi%) + fudge
                call "CONVERT" (invcost(hi%), 4.4, invcost$(hi%))
L51040:     call "CONVERT" (invcost, 4.4, invcost$)
            invpct$() = " "
            if invcost = 0 then return
            for i% = 1% to buckets%
               temp = 100 * invcost(i%)/invcost
               call "CONVERT" (temp, 2.2, invpct$(i%))
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

            call "PACKZERO" (invcost(), cost$)
            total$ = invcost$ : total = invcost

            end
