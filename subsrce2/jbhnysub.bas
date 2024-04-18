        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB   H   H  N   N  Y   Y   SSS   U   U  BBBB    *~
            *    J    B   B  H   H  NN  N  Y   Y  S      U   U  B   B   *~
            *    J    BBBB   HHHHH  N N N   YYY    SSS   U   U  BBBB    *~
            * J  J    B   B  H   H  N  NN    Y        S  U   U  B   B   *~
            *  JJ     BBBB   H   H  N   N    Y     SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBHNYSUB - Provides the ability to move (return) component*~
            *            parts from a job back into inventory or        *~
            *            directly into another Job bypassing Inventory. *~
            *            (Presumably these parts were not used by the   *~
            *            Job).                                          *~
            *----------------------------------------------------------Q*~
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
            * 07/20/84 ! ORIGINAL                                 ! BLT *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION - ! RAC *~
            * 10/10/85 ! Posting moved to background, changed call! HES *~
            * 02/09/87 ! Added Enhanced Lot Tracking.             ! ERN *~
            * 02/27/87 ! Original (Rewrite).                      ! LDJ *~
            *          ! Removed functionality for moving         !     *~
            *          ! component parts into a job (handled      !     *~
            *          ! better by JBKITSUB), also removed ability!     *~
            *          ! to move a parent/job part out of a job   !     *~
            *          ! into inventory (JBCMPSUB does it better),!     *~
            *          ! Removed functionality for moving Parent/ !     *~
            *          ! Job parts from inventory back into a job !     *~
            *          ! and stuck it into a new routine called   !     *~
            *          ! JBHNYJBS.                                !     *~
            *          ! Pulled functionality for Job to Job      !     *~
            *          ! Material Transfers out of JBMTVLJB and   !     *~
            *          ! stuffed into here since most of the      !     *~
            *          ! logic involved is virtually identical.   !     *~
            * 03/16/87 ! Removed obsolete subroutines.            ! ERN *~
            * 06/10/87 ! Standard Costing Changes                 ! ERN *~
            * 07/27/87 ! Changed 'Inventory Source' Display.      ! ERN *~
            * 01/27/88 ! Bug Fix.  PLOWCODE call on Component Part! LDJ *~
            *          ! number took "a long time" with large     !     *~
            *          ! file.  File structure prevents a good fix!     *~
            *          ! but an adequate patch has been made.     !     *~
            * 06/27/88 ! Initialize ORG_QTY fixes average cost dsp! KAB *~
            * 08/12/88 ! Deleted Closed Job restriction           ! TLJ *~
            *          ! If serialized may de-kit remaining only  !     *~
            *          ! Else may de-kit any qty.  If qty out > in!     *~
            *          ! can not distr. from ledger.              !     *~
            * 03/23/89 ! Reinstated test to prevent access to     ! RJM *~
            *          ! Closed Jobs and issue a Warning Message. !     *~
            * 02/12/90 ! Added PF8 access to HNYLCSUB permitting  ! MLJ *~
            *          !   Location control.                      !     *~
            * 03/09/90 ! Changed LOCATION from 200 - 400.         ! MLJ *~
            * 09/04/90 ! G/L Export file modifications            ! RAC *~
            * 04/14/92 ! PRR 12108. Flag Job as In-use.           ! JDH *~
            * 09/30/92 ! Corrected errormsg check that caused     ! JDH *~
            *          !   unissused parts to be restricted.      !     *~
            *          ! Ensured that job hold is relieved @ exit.!     *~
            * 05/28/93 ! Block To Job via JBTIF                   ! KAB *~
            *          ! Clean Up JBINUSE use                     !     *~
            *          ! Check to job Quantity. (Requires PIPOUT) !     *~
            *          ! Hang on to 'NOT KITTED' Message          !     *~
            *          ! Display & Enable PF keys for scrolling   !     *~
            *          !   (Logic was all ready there, but...)    !     *~
            *          ! Erroneous Ave. Cost if AQTY = 0          !     *~
            *          ! Don't Enable Transfer QTY if AQTY = 0    !     *~
            *          !    NUMTEST Doesn't Validate RANGE 0,0    !     *~
            *          ! PRR's 12717, 12424, 12439, 12635, 12942  !     *~
            * 07/17/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "JBHNYSUB" (modno$, jnlid$,  /* GL Posting directives      */~
                         #1,             /* USERINFO  File UFB         */~
                         #2,             /* SYSFILE2  File UFB         */~
                         #3,             /* JBMASTR2  File UFB         */~
                         #4,             /* HNYMASTR  File UFB         */~
                         #5,             /* HNYQUAN   File UFB         */~
                         #6,             /* JBMATER2  File UFB         */~
                         #7,             /* STORNAME  File UFB         */~
                         #8,             /* SERTIF    File UFB         */~
                         #9,             /* SERMASTR  File UFB         */~
                         #10,            /* SERWORK   File UFB         */~
                         #34,            /* PIPOUT    File UFB         */~
                         jobnr$,         /* Passed In Job (Optional)   */~
                         trantype$,      /* RJ = Return to Inventory   */~
                                         /*      from Job              */~
                                         /* JJ = Transfer Job to Job   */~
                         f21%)           /* OPEN Status of SYSFILE2    */

        dim                                                              ~
            ask$(3)80,                   /* ASK USER                   */~
            aqty(100),                   /* Distribution Available Qty */~
            avgcost$10,                  /* Total Average Cost         */~
            avgcosts(12), avgcosts$96,   /* Avg Cost Breakdown         */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            comp_part$25,                /* Component Part to Move Out */~
            comp_partdescr$32,           /* Component Part to Move Out */~
            cost$(100)10,                /* Distribution Total Costs   */~
            costs(12),                   /* Work array for costs       */~
            costs$(100)96,               /* Inventory Costs            */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            date$(100)8,                 /* Distribution Date          */~
            date_closed$8,               /* Date production job closed */~
            descr_map(18),               /* PLOWCODE Argument          */~
            distribute$3,                /* Distribute From Job Ledger?*/~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            f$(100)1,                    /* Screen FACs                */~
            header$(3)80,                /* PLOWCODE Argument          */~
            i(2),                        /* PLOWCODE Argument          */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            jbcomplete$10,               /* Qty Completed in Job       */~
            jbquantity$10,               /* Quantity to build in Job   */~
            jnlid$3,                     /* G/L Journal ID             */~
            job$8,                       /* Transfer Material FROM Job */~
            jobdescr$32,                 /* Transfer Material FROM Job */~
            jobnr$8,                     /* Passed In Job Number       */~
            l$(100)1,                    /* Screen FACs                */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            line7$79,                    /* Column Header              */~
            lot$16,                      /* Transfer TO Lot            */~
            lot$(100)16,                 /* Distribution Lot           */~
            lotno$16,                    /* Transfer TO Lot or JOB     */~
            mode$1,                      /* Mode for HNYCDIST          */~
            modno$2,                     /* G/L Module ID              */~
            moduleno$2,                  /* G/L Module ID              */~
            mtkey$(100)22,               /* JBMATER2 Keys              */~
            nokitmsg$80,                 /* Not Kitted Message         */~
            part$25,                     /* Job Part                   */~
            partdescr$32,                /* Job Part Description       */~
            pf16$16,pf6$20,pf7$20,       /* PF Key Prompts             */~
            pf2$22,pf3$20,pf4$20,pf5$20, /* PF Key Prompts             */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            postdate$8,                  /* Users inventory post date  */~
            qty(100),                    /* Distribution Orig Qty In   */~
            quantity$10,                 /* Quantity to Transfer       */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            sfcdate$6,                   /* Users shop floor post date */~
            source$(100)23,              /* Inventory Source           */~
            summary$1,                   /* Summary indicator - G/L    */~
            title$40,                    /* Journal title     - G/L    */~
            store$3,                     /* Transfer TO Store          */~
            storedescr$32,               /* Transfer TO Store          */~
            store$(100)3,                /* Distribution Store         */~
            tojob$8, ctojob$8,           /* Transfer Material TO   Job */~
            tojobdescr$32,               /* Transfer Material TO   Job */~
            topart$25,                   /* TO Job Part                */~
            tqty$(100)10,                /* Distribution Transfer Qty  */~
            tqty(100),                   /* Distribution Transfer Qty  */~
            trantype$2,                  /* Transaction Type           */~
            work$56,                     /* General Purpose ...        */~
            userid$3                     /* Current User Id            */~

        dim f1%(12),                     /* = 1 if READ was successful */~
            f2%(64),                     /* Record-On-File Flags       */~
            rslt$(63)20,                 /* Return Code from 'OPENFILE'*/~
            axd$(64)4                    /* AXD Pointer from 'OPENFILE'*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                    /* The variables F2%() AND AXD$() should not be    */
                    /* modified.  They are an intrinsic part of the    */
                    /* file open subroutine.                           */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * #11 ! HNYLOCNS ! Location Quantity Detail File            *~
            * #12 ! LOCATION ! Location Master File                     *~
            *************************************************************

            select #11, "HNYLOCNS",                                      ~
                         varc,  indexed,  recsize =  700,                ~
                         keypos =  1,  keylen =  42,                     ~
                         alt key 1, keypos =  443, keylen =  42,         ~
                             key 2, keypos =  485, keylen =  42,         ~
                             key 3, keypos =  527, keylen =  42,         ~
                             key 4, keypos =  590, keylen =  42

            select #12, "LOCATION",                                      ~
                         varc,  indexed,  recsize =  400,                ~
                         keypos =  1,  keylen =  11,                     ~
                         alt key 1, keypos =    4, keylen =  11

            call "OPENFILE" (#11, "SHARE", f2%(11), rslt$(11), axd$(11))
            call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))

            if f2%(11) = 0% then L02050
               call "OPENFILE" (#11,"OUTPT",f2%(11),rslt$(11),axd$(11))
               close #11
               call "OPENFILE" (#11,"SHARE",f2%(11),rslt$(11),axd$(11))
L02050:     if f2%(12) = 0% then L09000
               call "OPENFILE" (#12,"OUTPT",f2%(12),rslt$(12),axd$(12))
               close #12
               call "OPENFILE" (#12,"SHARE",f2%(12),rslt$(12),axd$(12))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            ll% = 6%      /* Default Lot Length */
            call "READ100" (#1, userid$, f1%(1))
            if f1%(1) = 1% then L09190
               call "ASKUSER"  (0%, "*** MISSING USER POSTING DATES ***",~
                "Unable to Locate your Posting Date Record in SYSFILE2.",~
                "Please check your Database (INLIB) and then try again.",~
                "Press RETURN to Acknowlege and EXIT.")
               goto exit_routine
L09190:     get #1, using L09200, sfcdate$
L09200:         FMT POS(34), CH(6)
            postdate$ = sfcdate$
            call "WHICHMON" (#2, postdate$, whichmonth%)
            call "DATEFMT" (postdate$)
            if whichmonth% > 0% then L09290
               call "ASKUSER"  (0%, "*** INVALID POSTING DATE ***",      ~
                "Your Shop Floor Posting Date, " & postdate$ & ", is not"~
              & " in an Open Period.",                                   ~
                "See your Administrator or run SYSDATES to correct the " ~
              & "problem & then try again.",                             ~
                "Press RETURN to Acknowlege and EXIT.")
               goto exit_routine
L09290:

            postflag% = 1%

        line7$ = "Source of Inventory    Date      Unit Cost   Original  ~
        ~Available   Transfer"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables  /* Clear Screen Variables     */
            if jobnr$ > " " then lf% = 2% else lf% = 1%

            for fieldnr% = 1% to  7%
                mode$ = "I"
                gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
L10130
*                    IF ENABLED% = 0% THEN 10320
L10140:               if fieldnr% > lf% then pf4$ = "(4)Previous Field"  ~
                                        else pf4$ = " "
                      if fieldnr% < lf%+1% then pf16$= "(16)Exit/Return "~
                                        else pf16$= " "
                      if enabled% = 0% then L10320
L10180:         if trantype$ <> "JJ" then                                ~
                   gosub'101(fieldnr%)  /* Display for Job to HNY     */ ~
                else                                                     ~
                   gosub'103(fieldnr%)  /* Display Screen for Job to Job*/
                      if keyhit% =  1% then gosub startover
                      if keyhit% <> 4% then       L10300
L10240:                  if fieldnr% = 5% then gosub clear_serial_numbers
                         fieldnr% = max(lf%, fieldnr% - 1%)
                         if fieldnr% = 1% then call "JBINUSE" (job$, 1%)
                         if fieldnr% = 3% and trantype$ = "JJ"           ~
                                          then call "JBINUSE" (tojob$, 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10140
                         if fieldnr% = lf% then L10130
                         goto L10240
L10300:               if keyhit% = 16% and fieldnr%<=lf% then exit_routine
                      if keyhit% <> 0% then       L10180
L10320:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ > " "     then L10180
                      if errormsg$ = hex(00) then L10180 /* Job in Use */
            next fieldnr%

            if distribute$ = "NO" then editpg1
               if maxline% <> 1% then editpg2
                  cursor%(1%) = 8% : lastfieldnr% = 0%
                  goto enter_transfer_qty

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            pf4$  = " "
            if distribute$ = "YES" then pf2$ = "(2)Distribution Detail"  ~
                                   else pf2$ = " "
            pf16$ = "(16)Save Data"
            mode$ = "E"
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            if trantype$ <> "JJ" then                                    ~
               gosub'101(0%)  /* Display Screen for Job to HNY */        ~
            else                                                         ~
               gosub'103(0%)  /* Display Screen for Job to Job */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% and pf2$ > " " then editpg2
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11220:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 3% or fieldnr% >  7% then editpg1
            if distribute$ = "YES" and fieldnr% > 6% then editpg1
            if t_enabled% > 0% and fieldnr% = 3% then editpg1
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
                  pf4$, pf2$, pf16$ = " "
L11300:     if trantype$ <> "JJ" then                                    ~
               gosub'101(fieldnr%)  /* Display Screen for Job to HNY */  ~
            else                                                         ~
               gosub'103(fieldnr%)  /* Display Screen for Job to Job */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11300
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11300
                  if fieldnr% = 6% and distribute$ ="YES" then editpg2
                  if fieldnr% = 5% and distribute$ ="YES" then           ~
                     check_distribution
                  lastfieldnr% = fieldnr%
            goto L11220

        editpg2
            pf2$  = "(2)First"    : pf3$ = "(3)Last"
            pf4$  = "(4)Previous" : pf5$ = "(5)Next"
            pf6$  = "(6)Down"     : pf7$ = "(7)Up"
            pf16$ = "(16)Return"
            if o% <= 0% then pf2$,pf4$,pf6$ = " "
            if o% >= maxline% - 9% then pf3$,pf5$,pf7$ = " "
            gosub'052(0%)               /* Calculate Remainder         */
            inpmessage$ = "To Enter Distribution, Position Cursor to th"&~
                           "e Transfer Qty Field & Press RETURN"
            lastfieldnr% = 0%
            gosub'102(0%)               /* Display Screen - No Entry   */
                errormsg$ = " "
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  2% then o% = 0%
                if keyhit%  =  3% then o% = max(0%,maxline%-9%)
                if keyhit%  =  4% then o% = max(0%,o%-8%)
                if keyhit%  =  5% then o% = max(0%,min(o%+8%,maxline%-9%))
                if keyhit%  =  6% then o% = max(0%,o%-1%)
                if keyhit%  =  7% then o% = max(0%,min(o%+1%,maxline%-9%))
                if keyhit%  = 16% then       check_distribution
                if keyhit% <>  0% then       editpg2
        enter_transfer_qty
            fieldnr% = cursor%(1%) - 7%
            if fieldnr% < 1% or fieldnr% >  9% then editpg2
            if fieldnr% = lastfieldnr% then editpg2
            line% = o% + fieldnr%
            gosub'052(line%)            /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg2
                  pf2$,pf3$,pf4$,pf5$,pf6$,pf7$,pf16$ = " "
L11730:     gosub'102(line%)            /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11730
            gosub'152(line%)            /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11730
                  lastfieldnr% = fieldnr%
            goto enter_transfer_qty

        check_distribution
            gosub'052(0%)
            if remainder = 0 then editpg1
            errormsg$ = "Quantity Distributed not equal to Qty to Return:"
            convert quantity - remainder to                              ~
                str(errormsg$,len(errormsg$)+2%), pic(-######.##)
            gosub'051(5%)
            pf2$ = "(2)Distribution Detail" : pf16$ = " "
L11890:     if trantype$ <> "JJ" then                                    ~
               gosub'101(5%)  /* Display Screen for Job to HNY */        ~
            else                                                         ~
               gosub'103(5%)  /* Display Screen for Job to Job */
            if keyhit% = 1% then gosub startover
            if keyhit% = 2% then editpg2
            if keyhit% <>0% then L11890
            gosub'151(5%)
            if errormsg$ <> " " then L11890
            goto check_distribution

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
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20200,         /* From Job               */~
                              L20270,         /* Component Part         */~
                              L20310,         /* Transfer TO Store / Job*/~
                              L20390,         /* Transfer TO Lot        */~
                              L20470,         /* Quantity to Move       */~
                              L20510,         /* Distribute?            */~
                              L20620,         /* Total Average Cost     */~
                              L20690,         /* Unused                 */~
                              L20730          /* Unused                 */
            return

L20200: REM Def/Enable Transfer Material FROM Job  JOB$
            inpmessage$ = "Enter the Job Number to Move Material Ou"  &  ~
                          "t of or blank to find an existing Job   "
            if jobnr$ = " " then return
               enabled% = 1%
               job$ = jobnr$
            return
L20270: REM Def/Enable Component Part to Move Out  COMP_PART$
            inpmessage$ = "Enter the Part Code to Transfer or blank"  &  ~
                          " to list of parts kitted to the Job     "
            return
L20310: REM Def/Enable Transfer TO Store           STORE$
            if trantype$ = "JJ" then L20360
            inpmessage$ = "Enter the Store to Move Material To   (b"  &  ~
                          "lank to find an existing store)         "
            return
L20360:     inpmessage$ = "Enter the Job to Move Material To (b"  &      ~
                          "lank to find an existing Job)"
            ctojob$ = tojob$
            return
L20390: REM Def/Enable Transfer TO Lot             LOT$
            call "LOTENABL" (comp_part$, lot_enabl%, ll%, #2, #4)
            if trantype$ <> "JJ" then L20440
               enabled% = 0%
               return
L20440:     inpmessage$ = "Enter the Lot to Move Material To"
            enabled% = lot_enabl%
            return
L20470: REM Def/Enable Quantity to Transfer        QUANTITY$
            inpmessage$ = "Enter the Qty to Transfer Out of the Job"
            call "CONVERT" (quantity,-.2,quantity$)
            return

L20510
*        Def/Enable Distribute From Job Ledger? DISTRIBUTE$
            inpmessage$ = "Enter YES to Distribute from the Job Mat"  &  ~
                          "erial Ledger or NO"
            if c_enabled% = 0% and p_enabled% = 0% then L20536
              distribute$ = "YES"
              enabled% = 0%
              return
L20536:     if quantity > tot_qty then L20550
              distribute$ = "YES"
              enabled% = 1%
              return
L20550:     distribute$ = "NO"
            enabled% = 0%
            return

L20620: REM Def/Enable Total Average Cost          AVGCOST$
            if distribute$ = "YES" then enabled% = 0%
            if enabled% = 0% then return
            inpmessage$ = "Modify the Job's Average Material Cost f"  &  ~
                          "or this Component or accept & continue"
            return

L20690: REM Def/Enable UNUSED                      ???????$
            enabled% = 0%
            return

L20730: REM Def/Enable UNUSED                      ???????$
            enabled% = 0%
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(line%)
            inpmessage$ = " "
            enabled% = 0%
            if line% > maxline% then return
            if maxline% = 0% then return
            if line% = 0% then L21180
            if mtkey$(line%) = " " then return
            if aqty(line%) >= .01 then L21130
               errormsg$ = "No Remaining Quantity to Transfer, Reselect."
               return
L21130:     if tqty$(line%) > " " then                                   ~
               call "CONVERT" (tqty(line%),-0.2, tqty$(line%))
            enabled% = 1%
            inpmessage$ = "Enter the quantity to distribute from this " &~
                          "Material Ledger Entry"
L21180:     remainder = quantity
            for x% = 1% to maxline%
                if x% = line% then L21220
                remainder = remainder - tqty(x%)
L21220:     next x%
            return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            pf2$, pf3$, pf4$, pf5$, pf6$, pf7$, pf16$ = " "
            org_qty, quantity, tot_qty, qty_remaining, o%   = 0
            init (hex(00)) avgcosts$, costs$()
            init(" ") errormsg$, inpmessage$, nokitmsg$,                 ~
                      jobdescr$              , /* From Job             */~
                      job$                   , /* From Job             */~
                      tojob$, ctojob$        , /* To   Job             */~
                      tojobdescr$            , /* To   Job             */~
                      comp_partdescr$        , /* Component Part       */~
                      comp_part$             , /* Component Part       */~
                      storedescr$            , /* Transfer TO Store    */~
                      store$                 , /* Transfer TO Store    */~
                      lot$                   , /* Transfer TO Lot      */~
                      quantity$              , /* Quantity to Move     */~
                      distribute$            , /* Distribute?          */~
                      avgcost$               , /* Total Average Cost   */~
                      quantity$              , /* Quantity to Dist     */~
                      store$()               , /* Distribution Stor    */~
                      lot$()                 , /* Distribution Lot     */~
                      date$()                , /* Distribution Date    */~
                      tqty$()                , /* Dist Transfer Qty    */~
                      source$()
                      mat tqty = zer
                      mat costs = zer
                      mat avgcosts = zer
            call "ALLFREE"
            call "JBINUSE" (" ", 1%)  /* Clears In-use Flag for User */
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
            gosub clear_serial_numbers
            goto inputmode

        clear_serial_numbers
            if c_enabled% = 0% then L29580
            call "SERSTOVR" (0%, "2", "3",  #9, #10) /* clear all  */
            return

L29580:     if lot_enabl% = 0% then return
            call "SERKIT"   (part$,      /* Part code to Build in Job  */~
                             comp_part$, /* Part code of Component     */~
                             " ",        /* Store to Select From/Issue */~
                                         /* To.                        */~
                             " ",        /* Lot to Select From/Issue To*/~
                                         /* (If TRANTYPE$ = JJ then    */~
                                         /*  this arg should be the    */~
                                         /*  Xfer To Job code).        */~
                             jbqty,      /* Qty to Build in Job        */~
                             0,          /* Qty to Move / Transfer     */~
                             0%,         /* Pointer For Work File Use  */~
                             10%,        /* Average # Lines per Documnt*/~
                             trantype$,  /* Source Transaction Type.   */~
                             job$,       /* Source Transaction Key.    */~
                             " ",        /* Status to Change S/N to.   */~
                             " ",        /* Status to Select/Chg from  */~
                             2%,         /* Operation to Perform       */~
                                         /* 2% = Start Over, ALL.      */~
                             errormsg$,  /* Returned Error Message     */~
                             #2,         /* SYSFILE2 UFB               */~
                             #4,         /* HNYMASTR UFB               */~
                             #9 ,        /* SERMASTR UFB               */~
                             #8 ,        /* SERTIF   UFB               */~
                             #10)        /* SERWORK  UFB               */

            return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            get #3, using L35035,                                         ~
               part$,       /* Part code                               */~
               jbqty,       /* Quantity to make                        */~
               jbcomplete,  /* Quantity completed to date              */~
               date_closed$ /* Date production job actually ended      */

            call "CONVERT" (jbqty, -0.2, jbquantity$)
            call "CONVERT" (jbcomplete, -0.2, jbcomplete$)
            call "GETCODE" (#4, part$, partdescr$, 0%, 99, f1%(4))
            if f1%(4) <> 0% then L30190
               errormsg$ = "Part Not On File: " & part$
               goto L30194
L30190:     call "SERENABL" (part$, p_enabled%, u3%, #2, #4)
            if date_closed$ = " " or date_closed$ = blankdate$ then return
               call "DATEFMT" (date_closed$)
               errormsg$ = "JOB CLOSED ON: " & date_closed$
L30194:        call "JBINUSE" (job$, 1%)  /* Clears In-use for Job */
               return

        load_material_ledger
          call "SERENABL" (comp_part$, c_enabled%, u3%, #2, #4)
          maxline% = 0% : tot_qty = 0
          readkey$ = str(job$) & hex(0000000000000000)

L30270:   call "PLOWNEXT" (#6, readkey$, 8%, f1%(6))
          if f1%(6) = 0% then L30570
          if dim(mtkey$(),1) <= maxline% then L30570
          if str(key(#6,1),,25%) <> comp_part$ then L30270
            maxline% = maxline% + 1%
            get #6, using L35280,                                         ~
                mtkey$(maxline%),   /* PRIMARY KEY TO RECORD           */~
                                    /* Job #, Post Date & System Time  */~
                store$(maxline%),   /* Store inventory was issued from */~
                lot$(maxline%),     /* Lot # inventory was issued from */~
                date$(maxline%),    /* Post Date                       */~
                qty(maxline%),      /* Qty moved to(+)/from(-) Job.    */~
                cost,               /* Total inv cost for qty available*/~
                costs$(maxline%),   /* Total inventory costs for qty   */~
                                    /* remaining on record.            */~
                aqty(maxline%)      /* Quantity withdrawn from Job.    */

            if qty(maxline%) >= 0 then L30480
               maxline% = maxline% - 1%
               goto L30270

L30480:     aqty(maxline%) = max(0, qty(maxline%)-aqty(maxline%))

            if qty(maxline%) <> 0 then                                   ~
                                   cost = round(cost / qty(maxline%), 4)
            call "CONVERT" (cost, 2.4, cost$(maxline%))
            tot_qty = tot_qty + aqty(maxline%)
            org_qty = org_qty +  qty(maxline%)
            call "DATEFMT" (date$(maxline%))
            if str(store$(maxline%),,1) = "*" then                       ~
                source$(maxline%) = "Job: " & str(store$(maxline%),2)    ~
                                            & lot$(maxline%)             ~
                               else                                      ~
                source$(maxline%) = "Str: " & str(store$(maxline%))   &  ~
                                   " Lot: " & lot$(maxline%)
          goto L30270

          qty_remaining = tot_qty

L30570:   mat avgcosts = zer   :  avgcost = 0
          if tot_qty  <= 0 then L30740
          if maxline% < 1% then L30740
            for x% = 1% to maxline%
                get costs$(x%) using L30620, costs()
L30620:              FMT 12*PD(14,4)
                mat avgcosts = avgcosts + costs      /* Total Costs  */
                for b% = 1% to 12%
                     costs(b%) = costs(b%) /  qty(x%)  /* Per unit */
                next b%
                call "PACKZERO" (costs(), costs$(x%)) /* Now per unit */
            next x%

            for b% = 1% to 12%
                avgcosts(b%) = avgcosts(b%) / org_qty
                avgcost = avgcost + avgcosts(b%)
            next b%
L30740:     call "CONVERT" (avgcost, 2.4, avgcost$)

            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "SHOSTAT" ("Updating Transaction Image File(s)")

            if postflag% = 0% then L31075
                returncode% = 0%
                moduleno$   = modno$
                call "JNLINFO" (moduleno$, jnlid$, pstseq%, summary$,    ~
                                title$, sfcdate$, #2, f21%, returncode%)
                postflag% = 0%

L31075:     REM *** First Save (DeKit) any Serial Numbers ***
            if maxline% = 0% then L31175  /* Part never Kitted to Job */
            if p_enabled% = 0% then L31175 /* Job Part Not Serialized */
            if c_enabled% = 0% then L31175 /* Comp Part Not Serialized */

            call "SERSAVE" (                                             ~
                        1%,              /* Line Item Pointer.         */~
                        "RJ",            /* Source Transaction Type    */~
                        job$,            /* Source Transaction Key     */~
                        1%,              /* # Trans to Create File for */~
                        comp_part$,      /* Part Code                  */~
                        userid$,         /* Current User ID            */~
                        "2",             /* Change Status to ...       */~
                        "3",             /* Change Status from ...     */~
                        0%,              /* Save to TIF,               */~
                        #2,              /* SYSFILE2 UFB               */~
                        #8 ,             /* SERTIF UFB                 */~
                        #9 ,             /* SERMASTR UFB               */~
                        #10)             /* SERWORK  UFB               */

L31175: REM *** Now Send the Real Work to JBPOST2 ***
            if trantype$ = "JJ" then function% = 2% else function% = 4%
            if distribute$ = "YES" then L31315
            if quantity <= 0 then return
            mtkey$(1%) = all(hex(ff))
            call "PACKZERO" (avgcosts(), avgcosts$)
            put work$ using L31880, tojob$, mtkey$(1%), 0, 0, 0, 1%

            call "JB2TIF" ("J2",        /* Send transaction to JBPOST2 */~
                           0%,          /* Wake up task flag 0,1,2,9999*/~
                           0%,          /* Not used if Wake flag = 0   */~
                           function%,   /* Transaction type (4=cmp out)*/~
                           hex(15),     /* Priority (within 'J2' only) */~
                           job$,        /* From Job number affected    */~
                           modno$,      /* G/L module to post          */~
                           jnlid$,      /* G/L journal to post         */~
                           pstseq%,     /* G/L posting sequence number */~
                           userid$,     /* Who                         */~
                           sfcdate$,    /* Posting Date                */~
                           comp_part$,  /* Inventory Part Code         */~
                           store$,      /* Inventory Store Code        */~
                           lot$,        /* Inventory Lot Id.           */~
                           quantity,    /* Quantity to process         */~
                           work$,       /* To job, MTkey, and ?        */~
                           " ",         /* Special Area                */~
                           avgcosts$)   /* Average Costs Breakdown     */
            goto L31840

        REM *** Detail Distribution Handling ***
L31315:     for x% = 1% to maxline%
                if tqty(x%) <= 0 then L31830
                if lot_enabl% * p_enabled% = 0% then L31475
                if c_enabled% = 1% then L31475
                REM *** Save DeKitting to SERTIF File ***
                call "SERKIT" (part$,    /* Part code to Build in Job  */~
                             comp_part$, /* Part code of Component     */~
                             store$(x%), /* Store to Select From/Issue */~
                                         /* To.  (N/A)                 */~
                             lot$(x%),   /* Lot to Select From/Issue To*/~
                                         /* (If TRANTYPE$ = JJ then    */~
                                         /*  this arg should be the    */~
                                         /*  Xfer To Job code).        */~
                             jbqty,      /* Qty to Build in Job        */~
                             tqty(x%),   /* Qty to Move / Transfer     */~
                             x%+1%,      /* Pointer For Work File Use  */~
                             10%,        /* Average # Lines per Documnt*/~
                             trantype$,  /* Source Transaction Type.   */~
                             job$,       /* Source Transaction Key.    */~
                             " ",        /* Status to Change S/N to.   */~
                             " ",        /* Status to Select/Chg from  */~
                             1%,         /* Operation to Perform       */~
                                         /* 1% = Save to Tif           */~
                             errormsg$,  /* Returned Error Message     */~
                             #2,         /* SYSFILE2 UFB               */~
                             #4,         /* HNYMASTR UFB               */~
                             #9 ,        /* SERMASTR UFB               */~
                             #8 ,        /* SERTIF   UFB               */~
                             #10)        /* SERWORK  UFB               */

L31475:         put work$, using L31880, tojob$, mtkey$(x%), 0, 0, 0,     ~
                                                                    x%+1%
                if trantype$ <> "JJ" then L31490
                   store$ = store$(x%) : lot$ = lot$(x%)

L31490:         call "JB2TIF" (         /* Writes to transaction Image */~
                             "J2",      /* Send transaction to JBPOST2 */~
                             0%,        /* Wake up task flag 0,1,2,9999*/~
                             0%,        /* Not used if Wake flag = 0   */~
                             function%, /* Transaction type (4=cmp out)*/~
                             hex(15),   /* Priority (within 'J2' only) */~
                             job$,      /* From Job number affected    */~
                             modno$,    /* G/L module to post          */~
                             jnlid$,    /* G/L journal to post         */~
                             pstseq%,   /* G/L posting sequence number */~
                             userid$,   /* Who                         */~
                             sfcdate$,  /* Posting Date                */~
                             comp_part$,/* Inventory Part Code         */~
                             store$,    /* Inventory Store Code        */~
                             lot$,      /* Inventory Lot Id.           */~
                             tqty(x%),  /* Quantity to process         */~
                             work$,     /* To job, MTkey, and ?        */~
                             " ",       /* Special Area                */~
                             costs$(x%))/* Per unit inventory costs    */
                if trantype$ <> "JJ" or t_enabled% = 0% then L31830
                if p_enabled% = 0% or c_enabled% = 1% then L31830

                REM *** Save Kitting to SERTIF File ***
                call "SERKIT" (topart$,  /* Part code to Build in Job  */~
                             comp_part$, /* Part code of Component     */~
                             store$(x%), /* Store to Select From/Issue */~
                                         /* To.  (N/A)                 */~
                             lot$(x%),   /* Lot to Select From/Issue To*/~
                                         /* (If TRANTYPE$ = JJ then    */~
                                         /*  this arg should be the    */~
                                         /*  Xfer To Job code).        */~
                             tojbqty,    /* Qty to Build in Job        */~
                             tqty(x%),   /* Qty to Move / Transfer     */~
                             x%+1001%,   /* Pointer For Work File Use  */~
                             10%,        /* Average # Lines per Documnt*/~
                             "JK",       /* Source Transaction Type.   */~
                             tojob$,     /* Source Transaction Key.    */~
                             " ",        /* Status to Change S/N to.   */~
                             " ",        /* Status to Select/Chg from  */~
                             1%,         /* Operation to Perform       */~
                                         /* 1% = Save to Tif           */~
                             errormsg$,  /* Returned Error Message     */~
                             #2,         /* SYSFILE2 UFB               */~
                             #4,         /* HNYMASTR UFB               */~
                             #9 ,        /* SERMASTR UFB               */~
                             #8 ,        /* SERTIF   UFB               */~
                             #10)        /* SERWORK  UFB               */

                work$ = " "
                str(work$,38%,3%) = bin(x%+1001%,3)

                call "JB2TIF" (         /* Writes to transaction Image */~
                             "J1",      /* Send transaction to JBPOST1 */~
                             0%,        /* Wake up task flag 0,1,2,9999*/~
                             0%,        /* Not used if Wake flag = 0   */~
                             4%,        /* Transaction type (4=Kit S/N)*/~
                             hex(80),   /* Priority                    */~
                             tojob$,    /* From Job number affected    */~
                             modno$,    /* G/L module to post          */~
                             jnlid$,    /* G/L journal to post         */~
                             pstseq%,   /* G/L posting sequence number */~
                             userid$,   /* Who                         */~
                             sfcdate$,  /* Posting Date                */~
                             comp_part$,/* Inventory Part Code         */~
                             store$(x%),/* Inventory Store Code        */~
                             lot$(x%),  /* Inventory Lot Id.           */~
                             tqty(x%),  /* Quantity to process         */~
                             " ",       /* Not used                    */~
                             work$,     /* S/N Index pointer only      */~
                             " ")       /* Ignored for this trans type */
L31830:     next x%
L31840:     gosub L31900        /* Block To Job */
            call "JB2TIF" ("J2", 2%, u3%) /* Send Hullo to BG Task */
            if trantype$ <> "JJ" or p_enabled% = 0% then return
            if distribute$ <> "YES" then return
            call "JB2TIF" ("J1", 2%, u3%) /* Send Hullo to BG Task */
            return
L31880:              FMT CH(8), CH(22), 3*PD(14,4), BI(2)

L31900:     if tojob$ = " " then return
            if function% <> 2% then return
            put work$ using L31880, job$, " ", 0, 0, 0, 0%
                call "JB2TIF" (         /* Writes to transaction Image */~
                             "J2",      /* Send transaction to JBPOST1 */~
                             0%,        /* Wake up task flag 0,1,2,9999*/~
                             0%,        /* Not used if Wake flag = 0   */~
                             12%,       /* Transaction type (4=Kit S/N)*/~
                             hex(15),   /* Priority                    */~
                             tojob$,    /* From Job number affected    */~
                             modno$,    /* G/L module to post          */~
                             jnlid$,    /* G/L journal to post         */~
                             pstseq%,   /* G/L posting sequence number */~
                             userid$,   /* Who                         */~
                             sfcdate$,  /* Posting Date                */~
                             comp_part$,/* Inventory Part Code         */~
                             " ",       /* Inventory Store Code        */~
                             " ",       /* Inventory Lot Id.           */~
                             0,         /* Quantity to process         */~
                             work$,     /* Not used                    */~
                             " ",       /* Not used                    */~
                             " ")       /* Ignored for this trans type */
            return

        REM *************************************************************~
            *   A C C E S S    L O C A T I O N    M A N A G E M E N T   *~
            *___________________________________________________________*~
            * Call HNYLCSUB and pass the Part, Store, Lot and Quantity  *~
            * so that the user does not have to re-enter them.          *~
            *************************************************************

         locations
            if quantity$ <> " " then L33120
               qty = 0
               goto L33140

L33120:     convert quantity$ to qty

L33140:     call "HNYLCSUB"  (comp_part$,                                ~
                              store$,                                    ~
                              str(lot$,,ll%),                            ~
                              qty,                                       ~
                              3%,  /*  Additions Mode                  */~
                              #2,  /*  SYSFILE2                        */~
                              #7,  /*  STORNAME                        */~
                              #1,  /*  USERINFO                        */~
                              #4,  /*  HNYMASTR                        */~
                              #11, /*  HNYLOCNS                        */~
                              #5,  /*  HNYQUAN                         */~
                              #12) /*  LOCATION                        */
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35035: FMT                 /* FILE: JBMASTR2                          */~
            POS(58),        /* Position for Field PART                 */~
            CH(25),         /* Part code                               */~
            PD(14,4),       /* Quantity to make                        */~
            PD(14,4),       /* Quantity completed to date              */~
            POS(153),       /* Position for Field DATE-CLOSED          */~
            CH(6)           /* Date production job actually closed     */

L35280: FMT                 /* FILE: JBMATER2                          */~
            CH(22),         /* PRIMARY KEY. JOB, POST DATE, TIME.      */~
            POS(48),        /* Position for Field STORE                */~
            CH(3),          /* Warehouse or Store                      */~
                            /* Source Store in Inventory Material was  */~
                            /* issued from.                            */~
            CH(6),          /* Which lot in inventory - always used    */~
                            /* with STORE                              */~
                            /* Source Lot in Inventory Material was    */~
                            /* issued from.                            */~
            CH(6),          /* Date record written.                    */~
            POS(71),        /* Position for Field QTYTOJOB             */~
            PD(14,4),       /* quantity moved to job                   */~
                            /* If positive this is orig qty moved into */~
                            /* Job from this store/lot. If < 0 = qty   */~
                            /* moved out of job.                       */~
            PD(14,4),       /* Total inventory cost for Qty Avaialable */~
            CH(96),         /* Total inventory costs for Qty Avaialable*/~
            POS(330),       /* Position for Field QTY-FROM             */~
            PD(14,4)        /* Quantity of a subcomponent withdrawn    */~
                            /* from a job                              */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              line2$ = "Transactions Posting Date: " & postdate$
              str(line2$,62%) = "JBHNYSUB: " & str(cms2v$,,8%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              if fieldnr% = 0% then lfac$(1), lfac$(2) = hex(84)
              on fieldnr% gosub L40230,         /* From Job             */~
                                L40230,         /* Component Part       */~
                                L40230,         /* Transfer TO Store    */~
                                L40230,         /* Transfer TO Lot      */~
                                L40240,         /* Quantity to Move     */~
                                L40230,         /* Distribute?          */~
                                hnycdist,      /* Average Cost Total   */~
                                L40240,         /* Unused               */~
                                L40240          /* Unused               */
              goto L40360
                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low    */
L40230:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only  */
L40240:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric     */

            hnycdist      /* Call sub to get average cost distribution */
                return clear
                call "PACKZERO" (avgcosts(), avgcosts$)
                call "HNYCDIST" (mode$, comp_part$, comp_partdescr$,     ~
                                 str(line2$,,60), #2, avgcosts$,         ~
                                 avgcost$, avgcost)
                get avgcosts$ using L40330, avgcosts()
L40330:              FMT 12*PD(14,4)
                return

L40360:     accept                                                       ~
               at (01,02),                                               ~
                  "Transfer Material From Job to Inventory",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Transfer Material FROM Job",                 ~
               at (06,30), fac(lfac$( 1)), job$                 , ch(08),~
               at (06,49), fac(hex(8c)),   jobdescr$            , ch(32),~
                                                                         ~
               at (07,02), "Component Part to Move Out",                 ~
               at (07,30), fac(lfac$( 2)), comp_part$           , ch(25),~
               at (07,56), fac(hex(8c)),   comp_partdescr$      , ch(25),~
                                                                         ~
               at (08,02), "Transfer TO Store",                          ~
               at (08,30), fac(lfac$( 3)), store$               , ch(03),~
               at (08,49), fac(hex(8c)),   storedescr$          , ch(32),~
                                                                         ~
               at (09,02), "Transfer TO Lot",                            ~
               at (09,30), fac(lfac$( 4)), str(lot$,,ll%)               ,~
                                                                         ~
               at (10,02), "Quantity to Transfer",                       ~
               at (10,30), fac(lfac$( 5)), quantity$            , ch(10),~
               at (10,44), "Total Qty Issued to Job",                    ~
               at (10,68), fac(hex(8c)), org_qty    , pic(-#######.##),  ~
                                                                         ~
               at (11,02), "Distribute From Job Ledger?",                ~
               at (11,30), fac(lfac$( 6)), distribute$          , ch(03),~
               at (11,44), "Qty Remaining in JOB",                       ~
               at (11,68), fac(hex(8c)), tot_qty    , pic(-#######.##),  ~
                                                                         ~
               at (12,02), "Total Average Cost",                         ~
               at (12,30), fac(lfac$( 7)), avgcost$             , ch(10),~
                                                                         ~
               at (14,02), fac(hex(8c)), nokitmsg$              , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,20), "(8)Locations",                               ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (24,02), fac(hex(8c)), pf2$                           ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(00010204080d0f10)),                              ~
               key (keyhit%)

               if keyhit% <>  8% then L40870
                  gosub locations
                  goto L40360

L40870:        if keyhit% <> 13% then L40910
                  call "MANUAL" ("JBHNYSUB")
                  goto L40360

L40910:        if keyhit% <> 15% then L40950
                  call "PRNTSCRN"
                  goto L40360

L40950:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(line%)
              line2$ = "Distribute From Job Material Ledger"
              str(line2$,62%) = "JBHNYSUB: " & str(cms2v$,,8%)
              init(hex(9c))f$(),l$() : str(f$(),,maxline%) = all(hex(8c))
              if line% > 0% then init(hex(ac)) str(l$(),,maxline%)       ~
                            else init(hex(a6)) str(l$(),,maxline%)
              if line% > 0% then l$(line%) = hex(a1)

L41070:     accept                                                       ~
               at (01,02),                                               ~
                  "Transfer Material From Job to Inventory",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Quantity to Distribute:",                    ~
               at (05,26), fac(hex(8c)),   quantity$            , ch(10),~
               at (05,40), "Remainder to Distribute:",                   ~
               at (05,65), fac(hex(8c)),   remainder, pic(-#######.##),  ~
                                                                         ~
               at (07,02), fac(hex(ac)), line7$                 , ch(79),~
                                                                         ~
               at (08,02), fac(f$(o%+1%)),source$(o%+01%)       , ch(23),~
               at (08,25), fac(f$(o%+1%)),date$(o%+01%)         , ch(08),~
               at (08,34), fac(f$(o%+1%)),cost$(o%+01%)         , ch(10),~
               at (08,45), fac(f$(o%+1%)),qty  (o%+01%), pic(-######.##),~
               at (08,56), fac(f$(o%+1%)),aqty (o%+01%), pic(-######.##),~
               at (08,67), fac(l$(o%+1%)),tqty$(o%+01%)         , ch(10),~
                                                                         ~
               at (09,02), fac(f$(o%+2%)),source$(o%+02%)       , ch(23),~
               at (09,25), fac(f$(o%+2%)),date$(o%+02%)         , ch(08),~
               at (09,34), fac(f$(o%+2%)),cost$(o%+02%)         , ch(10),~
               at (09,45), fac(f$(o%+2%)),qty  (o%+02%), pic(-######.##),~
               at (09,56), fac(f$(o%+2%)),aqty (o%+02%), pic(-######.##),~
               at (09,67), fac(l$(o%+2%)),tqty$(o%+02%)         , ch(10),~
                                                                         ~
               at (10,02), fac(f$(o%+3%)),source$(o%+03%)       , ch(23),~
               at (10,25), fac(f$(o%+3%)),date$(o%+03%)         , ch(08),~
               at (10,34), fac(f$(o%+3%)),cost$(o%+03%)         , ch(10),~
               at (10,45), fac(f$(o%+3%)),qty  (o%+03%), pic(-######.##),~
               at (10,56), fac(f$(o%+3%)),aqty (o%+03%), pic(-######.##),~
               at (10,67), fac(l$(o%+3%)),tqty$(o%+03%)         , ch(10),~
                                                                         ~
               at (11,02), fac(f$(o%+4%)),source$(o%+04%)       , ch(23),~
               at (11,25), fac(f$(o%+4%)),date$(o%+04%)         , ch(08),~
               at (11,34), fac(f$(o%+4%)),cost$(o%+04%)         , ch(10),~
               at (11,45), fac(f$(o%+4%)),qty  (o%+04%), pic(-######.##),~
               at (11,56), fac(f$(o%+4%)),aqty (o%+04%), pic(-######.##),~
               at (11,67), fac(l$(o%+4%)),tqty$(o%+04%)         , ch(10),~
                                                                         ~
               at (12,02), fac(f$(o%+5%)),source$(o%+05%)       , ch(23),~
               at (12,25), fac(f$(o%+5%)),date$(o%+05%)         , ch(08),~
               at (12,34), fac(f$(o%+5%)),cost$(o%+05%)         , ch(10),~
               at (12,45), fac(f$(o%+5%)),qty  (o%+05%), pic(-######.##),~
               at (12,56), fac(f$(o%+5%)),aqty (o%+05%), pic(-######.##),~
               at (12,67), fac(l$(o%+5%)),tqty$(o%+05%)         , ch(10),~
                                                                         ~
               at (13,02), fac(f$(o%+6%)),source$(o%+06%)       , ch(23),~
               at (13,25), fac(f$(o%+6%)),date$(o%+06%)         , ch(08),~
               at (13,34), fac(f$(o%+6%)),cost$(o%+06%)         , ch(10),~
               at (13,45), fac(f$(o%+6%)),qty  (o%+06%), pic(-######.##),~
               at (13,56), fac(f$(o%+6%)),aqty (o%+06%), pic(-######.##),~
               at (13,67), fac(l$(o%+6%)),tqty$(o%+06%)         , ch(10),~
                                                                         ~
               at (14,02), fac(f$(o%+7%)),source$(o%+07%)       , ch(23),~
               at (14,25), fac(f$(o%+7%)),date$(o%+07%)         , ch(08),~
               at (14,34), fac(f$(o%+7%)),cost$(o%+07%)         , ch(10),~
               at (14,45), fac(f$(o%+7%)),qty  (o%+07%), pic(-######.##),~
               at (14,56), fac(f$(o%+7%)),aqty (o%+07%), pic(-######.##),~
               at (14,67), fac(l$(o%+7%)),tqty$(o%+07%)         , ch(10),~
                                                                         ~
               at (15,02), fac(f$(o%+8%)),source$(o%+08%)       , ch(23),~
               at (15,25), fac(f$(o%+8%)),date$(o%+08%)         , ch(08),~
               at (15,34), fac(f$(o%+8%)),cost$(o%+08%)         , ch(10),~
               at (15,45), fac(f$(o%+8%)),qty  (o%+08%), pic(-######.##),~
               at (15,56), fac(f$(o%+8%)),aqty (o%+08%), pic(-######.##),~
               at (15,67), fac(l$(o%+8%)),tqty$(o%+08%)         , ch(10),~
                                                                         ~
               at (16,02), fac(f$(o%+9%)),source$(o%+09%)       , ch(23),~
               at (16,25), fac(f$(o%+9%)),date$(o%+09%)         , ch(08),~
               at (16,34), fac(f$(o%+9%)),cost$(o%+09%)         , ch(10),~
               at (16,45), fac(f$(o%+9%)),qty  (o%+09%), pic(-######.##),~
               at (16,56), fac(f$(o%+9%)),aqty (o%+09%), pic(-######.##),~
               at (16,67), fac(l$(o%+9%)),tqty$(o%+09%)         , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (23,02), fac(hex(8c)), pf2$                   , ch(12),~
               at (24,02), fac(hex(8c)), pf3$                   , ch(12),~
               at (23,20), fac(hex(8c)), pf4$                   , ch(12),~
               at (24,20), fac(hex(8c)), pf5$                   , ch(12),~
               at (23,40), fac(hex(8c)), pf6$                   , ch(12),~
               at (24,40), fac(hex(8c)), pf7$                   , ch(12),~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(00010203040506070d0f10)),                        ~
               key (keyhit%)

               if keyhit% <> 13% then L41570
                  call "MANUAL" ("JBHNYSUB")
                  goto L41070

L41570:        if keyhit% <> 15% then L41590
                  call "PRNTSCRN"
                  goto L41070

L41590:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%)
              line2$ = "Transactions Posting Date: " & postdate$
              str(line2$,62%) = "JBHNYSUB: " & str(cms2v$,,8%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              if fieldnr% = 0% then lfac$(1), lfac$(2) = hex(84)
              on fieldnr% gosub L42240,         /* From Job             */~
                                L42240,         /* Component Part       */~
                                L42240,         /* Transfer TO Job      */~
                                     ,         /* Not Used             */~
                                L42250,         /* Quantity to Move     */~
                                L42240,         /* Distribute?          */~
                                hnycdist,      /* Total Average Cost   */~
                                L42250,         /* Not Used             */~
                                L42250          /* Not Used             */
              goto L42270

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low    */
L42240:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only  */
L42250:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric     */

L42270:     accept                                                       ~
               at (01,02),                                               ~
                  "Transfer Material From Job to Job",                   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Transfer Material FROM Job",                 ~
               at (06,30), fac(lfac$( 1)), job$                 , ch(08),~
               at (06,49), fac(hex(8c)),   jobdescr$            , ch(32),~
                                                                         ~
               at (07,02), "Component Part to Move Out",                 ~
               at (07,30), fac(lfac$( 2)), comp_part$           , ch(25),~
               at (07,56), fac(hex(8c)),   comp_partdescr$      , ch(25),~
                                                                         ~
               at (08,02), "Transfer TO Job",                            ~
               at (08,30), fac(lfac$( 3)), tojob$               , ch(08),~
               at (08,49), fac(hex(8c)),   tojobdescr$          , ch(32),~
                                                                         ~
               at (10,02), "Quantity to Transfer",                       ~
               at (10,30), fac(lfac$( 5)), quantity$            , ch(10),~
               at (10,44), "Total Qty Issued to Job",                    ~
               at (10,68), fac(hex(8c)), org_qty    , pic(-#######.##),  ~
                                                                         ~
               at (11,02), "Distribute From Job Ledger?",                ~
               at (11,30), fac(lfac$( 6)), distribute$          , ch(03),~
               at (11,44), "Qty Remaining in JOB",                       ~
               at (11,68), fac(hex(8c)), tot_qty    , pic(-#######.##),  ~
                                                                         ~
               at (12,02), "Total Average Cost",                         ~
               at (12,30), fac(lfac$( 7)), avgcost$             , ch(10),~
                                                                         ~
               at (14,02), fac(hex(8c)), nokitmsg$              , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (24,02), fac(hex(8c)), pf2$                           ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(000102040d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13% then L42820
                  call "MANUAL" ("JBHNYSUB")
                  goto L42270

L42820:        if keyhit% <> 15% then L42860
                  call "PRNTSCRN"
                  goto L42270

L42860:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50090,         /* From Job               */~
                              L50140,         /* Component Part         */~
                              L50350,         /* Transfer TO Store / JOB*/~
                              L50535,         /* Transfer TO Lot        */~
                              L50595,         /* Quantity to Move       */~
                              L50720,         /* Distribute?            */~
                              L50740,         /* Total Average Cost     */~
                              L50755,         /* Unused                 */~
                              L50770          /* Unused                 */
            return
L50090: REM Test for Transfer Material FROM Job   JOB$
            call "GETCODE" (#3, job$, jobdescr$, 0%, 0, f1%(3))
            if f1%(3) <> 0% then L50115
               errormsg$ = "Please Enter An Existing Job Number"
               return
L50115:     u3% = 2%   /* Check In-use & Write Flag if Free */
            call "JBINUSE" (job$, u3%)
            if u3% = 0% then dataload
                errormsg$ = hex(00) /* Job in use */
                return

L50140: REM Test for Component Part to Move Out   COMP_PART$
            nokitmsg$ = " "
            maxline%,o%,tot_qty,qty_remaining,org_qty = 0
            if comp_part$ = " " or comp_part$ = "?" then L50158
            plowkey$ = str(job$) & all(hex(00))
L50151:     call "PLOWNEXT" (#6, plowkey$, 8%, f1%(6%))
            if f1%(6%) = 0% then L50158
            get #6 using L50245, comp_partdescr$
            if comp_part$ <> part$ and comp_part$ = comp_partdescr$      ~
                                                               then L50240
            goto L50151
L50158:     REM *** The Direct Approach didn't work, lets try being Subtle
            plowkey$ = str(job$) & all(hex(00))
            mat i = zer
            i(1) =-23.25 : i$() = part$ /* but not the Parent Part! */
            descr_map(01) = 09.061 : descr_map(02) = 01    /* Date  */
            descr_map(03) = 15.082 : descr_map(04) = 10    /* Time  */
            descr_map(05) = 23.25  : descr_map(06) = 20    /* Part# */
            descr_map(07) = 48.03  : descr_map(08) = 46    /* Store */
            descr_map(09) = 51.06  : descr_map(10) = 51    /* Lot   */
            descr_map(11) = 71.08  : descr_map(12) = 58.104 /* Orig Qty*/
            descr_map(13) =330.08  : descr_map(14) = 70.084 /* Qty Out*/
            descr_map(15) =-26.32  : descr_map(16) =1001   /*Part Descr*/
            descr_map(17) =287.40  : descr_map(18) =1038   /*Post Text */
            header$(1%) = hex(a4) &                                      ~
                           " Tran Date & Time   Component Part Number    ~
        ~Store Lot        Qty In   Qty Out"
            header$(3%) = hex(a4) & "Below is the Material Register From ~
        ~Which You May Transfer Parts Out of The Job."
            call "PLOWCODE" (#6, plowkey$, comp_partdescr$, 9008%,  0.34,~
                             f1%(6), header$(), 0,-23,i(), i$(), "D"," ",~
                             #4, descr_map())
            i(1) = 0
            if f1%(6) = 0% then L50280
L50240:     get #6 using L50245, comp_part$
L50245:     FMT POS(23), CH(25)
            call "DESCRIBE" (#4, comp_part$, comp_partdescr$, 0%, f1%(4))
            gosub load_material_ledger
            if c_enabled% = 0% or trantype$ <> "JJ" then return
            errormsg$ = "Sorry, but Job to Job Transfer of Serialized "  ~
                      & "Components is not supported!"
            return
L50280:     if comp_part$ <> part$ then L50300
            errormsg$ = "Sorry, you cannot report Completion, Scr"  &    ~
                        "ap, or Rework of the Job Part here!"
            return
L50300:     call "GETCODE" (#4, comp_part$, comp_partdescr$, 0%,99,f1%(4))
            if f1%(4) = 1% then L50335
            errormsg$ = "Sorry, part does not exist in either the"  &    ~
                        " Job Material Ledger or the Master File"
            return
            REM *** We let them by here BUT, if this is flagged as a S/N ~
                    item they won't get past the Serial Number Selection!
L50335
*          ERRORMSG$ = HEX(00) & "WARNING>>> Part was never reported as"~
*                              & " being issued to this Job!"
            nokitmsg$ = hex(84) & "WARNING>>> Part was never reported as"~
                                & " being issued to this Job!"
            return
L50350: REM Test for Transfer TO Store / Job      STORE$ / TOJOB$
            if trantype$ = "JJ" then L50385

            call "PLOWCODE" (#7, store$, storedescr$, 0%, .30, f1%(7))
            if f1%(7) = 1% then return
            errormsg$ = "Store Not On File!  Please ReEnter or Select "  ~
                      & "from the displayed List"
            return

L50385:     if ctojob$ = " " then L50389
               call "JBINUSE" (ctojob$, 1%)
               ctojob$ = " "
L50389:     call "GETCODE" (#3, tojob$, tojobdescr$, 0%, 0, f1%(3))
            if f1%(3) = 1% then L50405
               errormsg$ = "Please Enter An Existing Job Number"
               return
L50405:     if tojob$ <> job$ then L50416
               errormsg$ = "The FROM and TO Jobs can't be the same!"
               return
L50416:     checkqty = quantity
            gosub check_tojob_pip
            u3% = 2%   /* Check In-use & Write Flag if Free */
            call "JBINUSE" (tojob$, u3%)
               if u3% = 0% then L50430
                  errormsg$ = hex(00) /* Job in use */
                  return
L50430:     get #3, using L35035,                                         ~
               topart$,     /* Part code                               */~
               tojbqty,     /* Quantity to make                        */~
               tojbcomplete,/* Quantity completed to date              */~
               date_closed$ /* Date production job actually ended      */
            if topart$ <> comp_part$ then L50468
                errormsg$ = "Can't transfer the To Job's End Item"
                call "JBINUSE" (tojob$, 1%)  /* Clears In-use for Job */
                return
L50468:     if date_closed$ = " " or date_closed$ = blankdate$ then L50475
                call "DATEFMT" (date_closed$)
                errormsg$ = "JOB CLOSED ON: " & date_closed$  &          ~
                            " - Select another Job"
                call "JBINUSE" (tojob$, 1%)  /* Clears In-use for Job */
                return
L50475:     call "SERENABL" (topart$, t_enabled%, u3%, #2, #4)

            if tojbqty - tojbcomplete  > 0 or p_enabled% = 0% then L50505
               errormsg$ = "Sorry, all parts to build are "             &~
                           "SERIALIZED and have been reported COMPLETE."
               call "JBINUSE" (tojob$, 1%)  /* Clears In-use for Job */
               return
L50505:     ctojob$ = tojob$
            return

L50535: REM Test for Transfer TO Lot              LOT$
            if trantype$ = "JJ" then return
            if lot$ = "?" then lot$ = " "
            if lot_enabl% <> 2% or lot$ <> " " then L50565
                errormsg$ = "Lot Number required for this Part."
                return
L50565:     readkey$ = str(comp_part$,,25%) & str(store$) & lot$
            call "READ100" (#5, readkey$, f1%(5))
            if f1%(5) = 1% then return
                call "LOTVALID" (comp_part$, store$, lot$, #2, #4, #5,   ~
                                 errormsg$)
            return
L50595: REM Test for Quantity to Transfer         QUANTITY$
            if maxline% > 0% and (c_enabled%=1% or p_enabled%=1%) then   ~
             call"NUMTEST"(quantity$,.01,tot_qty,errormsg$,-2.2,quantity)~
            else                                                         ~
             call "NUMTEST" (quantity$,.01,9e7,errormsg$,-2.2,quantity)
            if errormsg$ <> " " then return
            checkqty = quantity
            if mode$ = "E" then L50624
               if tojobqty <= 0 then L50625   /* Been Told Once Already */
L50624:     gosub check_tojob_pip
L50625:     qty_remaining = tot_qty - quantity
            if c_enabled% = 0% then return
            REM *** Select Serial Numbers to Transfer ***
            errormsg$ = "*Enter the Component S/N(s) to De-Kit"
            call "SERSELCT" (comp_part$, /* Part code                  */~
                             job$,       /* S/N Location to Select from*/~
                             quantity,   /* Qty to Assign S/N's To     */~
                             1%,         /* Pointer For Work File Use  */~
                             1%,         /* Average # Lines per Documnt*/~
                             trantype$,  /* Source Transaction Type.   */~
                             job$,       /* Source Transaction Key.    */~
                             "2",        /* Status to Change S/N to.   */~
                             "3",        /* Status to Select/Chg from  */~
                             errormsg$,  /* Returned Error Message     */~
                             #2,         /* SYSFILE2 UFB               */~
                             #4,         /* HNYMASTR UFB               */~
                             #9 ,        /* SERMASTR UFB               */~
                             #10)        /* SERWORK  UFB               */

            return
L50720: REM Test for Distribute From Job Ledger?  DISTRIBUTE$
            if distribute$ = "NO" or distribute$ = "YES" then return
            errormsg$ = "Must be YES or NO"
            return
L50740: REM Test for Total Average Cost           AVGCOST$
            return

L50755: REM Test for UNUSED                       ???????$
            return

L50770: REM Test for UNUSED                       ???????$
            return

        check_tojob_pip
            tojobqty = 0
            if tojob$ = " " then return

            readkey$ = "JOB ORDER: " & str(tojob$,,8%) & comp_part$
            init(hex(00)) str(readkey$,45)

L50840:     call "PLOWNEXT" (#34, readkey$, 44%, f1%(34%))
               if f1%(34%) = 0% then L50880
            get #34 using L50855, temp
L50855:         FMT POS(57), PD(14,4)
            if temp  <= 0 then L50840
            tojobqty = tojobqty + temp
            goto L50840

L50880:     if tojobqty <= 0 then L50905
            if checkqty = 0 then return
            if checkqty <= tojobqty then return

L50890: %Movement of ######################### to job: ########
L50895: %Kit List Requires: ########.## : movement is for ########.##
L50896: %Kit List Requires: ########.##

L50905:     ask$() = " "
            put ask$(1) using L50890, comp_part$, tojob$
            if checkqty = 0 then put ask$(2) using L50896, tojobqty       ~
                     else put ask$(2) using L50895, tojobqty, checkqty
            ask$(3) = "Press PF8 to Continue. Press PF1 to Re-enter."

L50930:     u3% = 2%
            call "ASKUSER" (u3%, "*** To Job Quantity Warning ***",      ~
                            ask$(1), ask$(2), ask$(3))
            if u3%  = 8% then return
            if u3% <> 1% then L50930
               errormsg$ = "Movement Quantity/To Job Requirement is" &   ~
                           " inconsistent."
               return clear
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(line%)
            errormsg$ = " "

        REM Test for Distribution Transfer Qty    TQTY$(100)
            call "NUMTEST" (tqty$(line%), 0, aqty(line%),                ~
                            errormsg$, -0.2, tqty(line%))
            if errormsg$ > " " then return
            if p_enabled% = 0% then return
            if c_enabled% = 1% then return
            if lot_enabl% = 0% then return
            if trantype$ = "JJ" then lotno$ = tojob$                     ~
                                else lotno$ = lot$(line%)
            REM *** Have the User tell us What Parent S/N's to Pull From
            errormsg$ = "*Enter the Parent S/N(s) this "                 ~
                      & "Component is being De-Kitted From"
            call "SERKIT" (part$,        /* Part code to Build in Job  */~
                           comp_part$,   /* Part code of Component     */~
                           store$(line%),/* Orig Store Issued from     */~
                           lotno$,       /* Orig Lot   Issued from     */~
                                         /* (If TRANTYPE$ = JJ then    */~
                                         /*  this arg should be the    */~
                                         /*  Transfer To Job code).    */~
                           jbqty,        /* Qty to Build in From Job   */~
                           tqty(line%),  /* Qty to Move / Transfer     */~
                           line%+1%,     /* Pointer For Work File Use  */~
                           10%,          /* Average # Lines per Documnt*/~
                           trantype$,    /* Source Transaction Type.   */~
                           job$,         /* Source Transaction Key.    */~
                           " ",          /* Status to Change S/N to.   */~
                           " ",          /* Status to Select/Chg from  */~
                           0%,           /* Operation to Perform       */~
                                         /* 0% = Normal Input/Edit.    */~
                           errormsg$,    /* Returned Error Message     */~
                           #2,           /* SYSFILE2 UFB               */~
                           #4,           /* HNYMASTR UFB               */~
                           #9 ,          /* SERMASTR UFB               */~
                           #8 ,          /* SERTIF   UFB               */~
                           #10)          /* SERWORK  UFB               */

            if errormsg$ > " " then return
            if trantype$ <> "JJ" then return
            if t_enabled% = 0% then return

            REM *** Have the User tell us What Parent S/N's to KIT TO
            errormsg$ = "*Enter the Parent S/N(s) this "                 ~
                      & "Component is being Kitted TO"
            call "SERKIT" (topart$,      /* Part code to Build in Job  */~
                           comp_part$,   /* Part code of Component     */~
                           store$(line%),/* Orig Store Issued from     */~
                           lot$(line%),  /* Orig Lot   Issued from     */~
                                         /* (If TRANTYPE$ = JJ then    */~
                                         /*  this arg should be the    */~
                                         /*  Transfer To Job code).    */~
                           tojbqty,      /* Qty to Build in From Job   */~
                           tqty(line%),  /* Qty to Move / Transfer     */~
                           line%+1001%,  /* Pointer For Work File Use  */~
                           10%,          /* Average # Lines per Documnt*/~
                           "JK",         /* Source Transaction Type.   */~
                           tojob$,       /* Source Transaction Key.    */~
                           "1",          /* Status to Select S/N's from*/~
                           " ",          /* N/A                        */~
                           0%,           /* Operation to Perform       */~
                                         /* 0% = Normal Input/Edit.    */~
                           errormsg$,    /* Returned Error Message     */~
                           #2,           /* SYSFILE2 UFB               */~
                           #4,           /* HNYMASTR UFB               */~
                           #9,           /* SERMASTR UFB               */~
                           #8,           /* SERTIF   UFB               */~
                           #10)          /* SERWORK  UFB               */

            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Return to Caller.                                         *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_routine
            if postflag% = 0% then call "JBJNLCLS" ("J2", userid$,       ~
                              moduleno$, jnlid$, pstseq%, returncode%)
            call "JBINUSE" (" ", 1%)  /* Clears In-use Flag for User */
            end
