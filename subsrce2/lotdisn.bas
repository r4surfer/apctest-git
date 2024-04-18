        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  L       OOO   TTTTT  DDDD   IIIII   SSS   N   N ( Y   Y )*~
            *  L      O   O    T    D   D    I    S      NN  N (  Y Y  )*~
            *  L      O   O    T    D   D    I     SSS   N N N (   Y   )*~
            *  L      O   O    T    D   D    I        S  N  NN (   Y   )*~
            *  LLLLL   OOO     T    DDDD   IIIII   SSS   N   N (   Y   )*~
            *                                                           *~
            *-----------------------------------------------------------*~
            * LOTDISN  - Handle Distribution of a Quantity over Store   *~
            *            Lot Combinations.                              *~
            *            (With possible distribution to Serial Numbers  *~
            *            if they get involved ...).                     *~
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
            * 01/22/87 ! Original                                 ! KEN *~
            * 02/12/87 ! Added Serial Number handling             ! LDJ *~
            * 05/28/87 ! Intermittent False Error Message         ! KAB *~
            * 06/08/87 ! Standard Cost Changes                    ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "LOTDISN" (part$,            /* Part Code                  */~
                       str$(),           /* Passed Stores              */~
                       ltt$(),           /* Passed Lots                */~
                       qtt$(),           /* Passed Quantities          */~
                       errormsg$,        /* Oops                       */~
                       function%,        /* 1= W/D; 2=Addns            */~
                       passmax%,         /* Current Number of Lots     */~
                       maxallowed%,      /* Maximum Number of Lots     */~
                       qtydst,           /* Quantity to Distribute     */~
                       index%(),         /* Component S/N's Index ptrs */~
                       parent_part$,     /* Parent part to distribute  */~
                                         /* to otherwise blank if not  */~
                                         /* applicable.                */~
                       parent_qty,       /* Qty of Parent S/N's to     */~
                                         /* distribute to or zero.     */~
                       index%,           /* Current Line or S/N Index  */~
                                         /* pointer for current line in*/~
                                         /* caller.                    */~
                       avglines%,        /* Average # Lines per Documnt*/~
                       trantype$,        /* Source Transaction Type.   */~
                       trankeyin$,       /* Source Transaction Key     */~
                       status$,          /* Status of Parent S/N's to  */~
                                         /* Distribute To              */~
                       source$,          /* Status of Component S/N's  */~
                                         /* to issue from.             */~
                       dest$,            /* Destination Status of      */~
                                         /* Component S/N's.           */~
                       #1,               /* HNYMASTR UFB               */~
                       #2,               /* HNYQUAN  UFB               */~
                       #3,               /* STORNAME UFB               */~
                       #4,               /* SYSFILE2 UFB               */~
                       #5,               /* SERMASTR UFB               */~
                       #6,               /* SERTIF   UFB               */~
                       #7)               /* SERWORK  UFB               */~

*        If only one store is allowed then set first element of STR$()
*        to HEX(FF) and second equal to the store code. Note that the
*        store array is the array used for determining sizes.

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dest$,                       /* Destination Status of S/N's*/~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            header$79,                   /* Header                     */~
            i$(24)80,                    /* Screen Image               */~
            index%(100),                 /* S/N's index pointers       */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            location$30,                 /* S/N Location to Select from*/~
            lot$(100)6,                  /* Lot                        */~
            onestore$3,                  /* Set if only 1 store allowed*/~
            openqty$10,                  /* Quantity Remaining         */~
            parent_part$25,              /* Part code to Kit To or " " */~
            part$25,                     /* Part Code                  */~
            partdescr$32,                /* Part Code                  */~
            pfmsg$(3)79,                 /* PF Screen Literal          */~
            pfkey$32,                    /* PF Keys Enabled            */~
            qty$10,                      /* Quantity to Distribute     */~
            qty$(100)10,                 /* Quantity                   */~
            readkey$50,                  /* A key to read by           */~
            savelot$6,                   /* Lot field before Edit      */~
            savestore$3,                 /* Store field before Edit    */~
            source$1,                    /* Status of Component S/N's  */~
            status$1,                    /* Status of Parent S/N's     */~
            store$(100)3,                /* Store                      */~
            text$(100)40,                /* Line Text Field            */~
            tran$80,                     /* Line Trans Field           */~
            trankey$40,                  /* Source Transaction Key     */~
            trankeyin$40,                /* Source Transaction Key     */~
            trantype$2                   /* Transaction Type Code      */~

        dim                                                              ~
            str$(100)3,                  /* Passed Stores              */~
            ltt$(100)6,                  /* Passed Lots                */~
            qtt$(100)10                  /* Passed Quantities          */~

        dim f2%(1)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.00 09/08/87 Standard cost to 12             "
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
            * # 1 ! HNYMASTR ! Inventory Master File                    *~
            * # 2 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * # 3 ! STORNAME ! STORE INFORMATION FILE                   *~
            * # 4 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            line2$ = errormsg$: errormsg$, onestore$ = " "
            trankey$ = trankeyin$
            ll% = 6%      /* Default Lot Length */
            if str$(1) <> hex(ff) then L09120
                onestore$ = str$(2)
                init(" ") str$()

L09120:     if part$ = " " then L09140   /* Will ask for if blank!!!     */
                 if qtydst = 0 then exit_routine
L09140:     date$ = date  :  call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            header$ = "Distribution:  Quantity Store Lot"
            header$ = header$ & "   ('*' indicates lot is not on file)"

            init(hex(00)) tran$
            init(hex(01)) str(tran$, 1)
            init(hex(02)) str(tran$,25)
            init(hex(03)) str(tran$,31)

            linemax% = dim(str$(),1)

            on function% goto L09320, L09340
                     errormsg$ = "Invalid sub-routine function"
                     goto exit_routine
L09320:     if line2$ <> " " then L09360
                line2$ = "Inventory Withdrawals" : goto L09360
L09340:     if line2$ <> " " then L09360
                line2$ = "Inventory Additions  "
L09360:     str(line2$,62%) = "LOTDISN: " & str(cms2v$,,8%)

            call "SERENABL" (parent_part$,    /* Part Number to Check  */~
                             p_enabled%,      /* Enable Flag to Set    */~
                             u3%,             /* Maximum S/N Field Len */~
                             #4,              /* SYSFILE2 UFB          */~
                             #1)              /* HNYMASTR UFB          */

            call "SERENABL" (part$,           /* Component Part        */~
                             c_enabled%,      /* Enable Flag to Set    */~
                             u3%,             /* Maximum S/N Field Len */~
                             #4,              /* SYSFILE2 UFB          */~
                             #1)              /* HNYMASTR UFB          */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 2%
L10100:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10220
L10120:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16 and fieldnr% = 1 then exit_routine
                      if keyhit% <>  0 then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%

            if maxlines% <> 1% then L10310
                c%, s%, fieldnr% = 1%           /* Test if only 1 guy */
                if index%(c%) =  0% then        /* passed in.         */ ~
                                   index%(c%) =  0% - index% * 1000% - c%
                goto L10420
L10310:     if maxlines% > 0% then editpg1
            if linemax% > 0%  then L10350
               errormsg$ = "Distribution Array Full.  Please Return."
               goto editpg1
L10350:     s% = s% + 1%
            if s% < 11% then L10390
               l% = l% + 1%
               s% = 10%
L10390:     c% = l% + s%
            if c% > linemax% then editpg1
               index%(c%) =  0% - index% * 1000% - c%
L10420:        gosub input_lines
                  if keyhit% = 16 then editpg1
               maxlines% = maxlines% + 1%
               if qtyrem <= 0 then editpg1
               goto L10350

        input_lines
            for fieldnr% = 1% to  3%
L10500:         gosub'052(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10720
L10520:         gosub'102(fieldnr%, s%) /* Display & Accept Screen    */
                      if keyhit%  = 1% then gosub startover
                      if keyhit% <> 4% then       L10630
L10550:                  if fieldnr% < 2% or fieldnr% > 3% then L10580
                            savestore$ = store$(c%) : savelot$ = lot$(c%)
                            gosub clear_ser_distr
L10580:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L10520
                         if fieldnr% = 1% then L10500
                         goto L10550
L10630:               if keyhit% <>  6% then       L10700
                         if c% = 1% then L10700

                      if fieldnr% = 1% then qty$(c%) = qty$(c%-1%)
                      if fieldnr% = 2% then store$(c%) = store$(c%-1%)
                      if fieldnr% = 3% then lot$(c%) = lot$(c%-1%)

L10700:               if keyhit% = 16 and fieldnr% = 1 then L10770
                      if keyhit% <>  0 then       L10520
L10720:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10520
            next fieldnr%
            return

L10770:     init(" ") store$(c%), lot$(c%), qty$(c%), text$(c%)
            call "CONVERT" (qtyrem, 0.2, openqty$)
            index%(c%) = 0%
            return

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            edit% = 1%
            inpmessage$ = edtmessage$
            lasts%, lastfieldnr% = 0%
            gosub'102(0%, 0%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then l% = 0%
                  if keyhit%  =  3 then l% = maxlines% - 10%
                  if keyhit%  =  4 then l% = l% - 10
                  if keyhit%  =  5 then l% = min(l%+10%, maxlines%-10%)
                  if keyhit%  =  6 then l% = l%- 1%
                  if keyhit%  =  7 then l% = min(l%+ 1%, maxlines%-10%)
                                        l% = max(0%, l%)
                  if keyhit%  = 11 then       append_mode
                  if keyhit%  = 12 then       clear_dist
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editpg1
L11230:     s% = cursor%(1) - 9
            if s% < 1 or s% >  10 then editpg1
            fieldnr% = val(str(tran$,cursor%(2),1))
            if fieldnr% = 0% then editpg1
            if fieldnr% = lastfieldnr% and lasts% = s% then editpg1
            c% = l% + s%
            if qty$(c%) = " " then editpg1
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
L11310:     gosub'102(fieldnr%,s%)      /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11310
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11310
                  lastfieldnr% = fieldnr% : lasts% = s%
            goto L11230

L11390: append_mode
            c% = maxlines% + 1%
            if c% > linemax% then editpg1
            l% = max(0%, maxlines% - 9%)
            s% = c% - l%
            index%(c%) = 0% - index% * 1000% - c%
            edit% = 0%
            gosub input_lines
                if keyhit% <> 16 then L11500
                   l% = max(0%, l% - 1%)
                   goto editpg1
L11500:     maxlines% = maxlines% + 1%
            if qtyrem <= 0 then editpg1
            goto L11390

        clear_dist

            u3% = 2%
            inpmessage$ =                                                ~
            "You will be allowed to UNDO the Effects of this CLEAR via " ~
               & "PF1."
            if p_enabled% = 1% or c_enabled% = 1% then inpmessage$ =     ~
            "You will NOT be allowed to UNDO the Effects of this CLEAR."
            call "ASKUSER" (u3%, "* * * WARNING * * *",                  ~
            "The current distribution is about to be cleared.",          ~
            inpmessage$,                                                 ~
            "Press PF1 to CLEAR  -or-  Press RETURN to resume EDIT.")
            inpmessage$ = " "
            if u3% = 0% then editpg1
            if u3% <> 1% then clear_dist

            for c% = 1% to maxlines%
                gosub clear_ser_distr
            next c%

            init(" ") errormsg$, inpmessage$, qty$(), store$(), lot$(),  ~
                      text$()

            edit%, maxlines%, l%, s%, c% = 0%
            mat index% = zer

            qtyrem = qtydst

            call "CONVERT" (qtyrem, 0.2, openqty$)

            goto editpg1

        clear_ser_distr
            REM *** Clear if PF4 Pressed or Start Over Condition ***
            if p_enabled% = 0% and c_enabled% = 0% then return
            savestore$ = store$(c%) : savelot$ = lot$(c%)
            if p_enabled% = 0% then L12240  /* Component Selection Only */

            call "SERKIT" (parent_part$, /* Part code to Issue To      */~
                           part$,        /* Comp Part to Issue From    */~
                           store$(c%),   /* S/N Location to Select from*/~
                           lot$(c%),     /* (Store and Lot Codes).     */~
                           parent_qty,   /* Qty of Parent to Issue To  */~
                           0,            /* Comp.  Qty to Issue        */~
                           index%(c%),   /* Pointer For Work File Use  */~
                           avglines%,    /* Average # Lines per Documnt*/~
                           trantype$,    /* Source Transaction Type.   */~
                                         /* (JK = Job Kitting).        */~
                           trankey$,     /* Source Transaction Key     */~
                           status$,      /* Status of Parent S/N's to  */~
                                         /* Issue To.                  */~
                           source$,      /* Status of Component S/N's  */~
                                         /* to Issue From.             */~
                           2%,           /* Operation to Perform       */~
                                         /* 0% = Normal Input/Edit,    */~
                                         /*      by INDEX%,            */~
                                         /* 1% = Save Data of INDEX%   */~
                                         /* 2% = Start Over, if INDEX% */~
                                         /*      > 0% then only the    */~
                                         /*      line/data pointed to  */~
                                         /*      by INDEX% is cleared, */~
                                         /*      else ALL is cleared.  */~
                           errormsg$,    /* Returned Error Message     */~
                           #4,           /* SYSFILE2 UFB               */~
                           #1,           /* HNYMASTR UFB               */~
                           #5,           /* SERMASTR UFB               */~
                           #6,           /* SERTIF   UFB               */~
                           #7)           /* SERWORK  UFB               */
            return

L12240:     REM *** Clear Selections ***
            call "SERSTOVR" (index%(c%), /* Line Item Pointer.  If = 0 */~
                                         /* all entries in workfile    */~
                                         /* will be processed/scratched*/~
                                         /* If > 0 then only entries   */~
                                         /* for that item will be      */~
                                         /* processed, WorkFile will   */~
                                         /* NOT be scratched in that   */~
                                         /* instance.                  */~
                             dest$,      /* Status to Change back from */~
                             source$,    /* Status to Restore To       */~
                             #5,         /* SERMASTR UFB               */~
                             #7)         /* SERWORK  UFB               */
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if qtyrem = 0 then L19130
               errormsg$ = "Distribution Not Complete: "
               call "CONVERT" (qtydst - qtyrem, -0.2,                    ~
                               str(errormsg$, len(errormsg$) + 2%, 10%))
               errormsg$ = errormsg$ & " Distributed."

L19130:     init (" ") str$(), ltt$(), qtt$()
            passmax% = 0%

            if maxlines% = 0% then exit_routine

            for i% = 1% to maxlines%
                convert qty$(i%) to temp, data goto L19260
                if temp = 0 then L19260
                     passmax% = passmax% + 1%
                     qtt$(passmax%) = qty$(i%)
                     str$(passmax%) = store$(i%)
                     ltt$(passmax%) = lot$(i%)
                     index%(passmax%)=index%(i%)
L19260:     next i%

            goto exit_routine

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Part Code          */    ~
                              L20200          /* Dist. Quantity     */    ~

            return

L20100: REM Def/Enable Part Code                   PART$
            call "GETCODE" (#1, part$, partdescr$, 0%, 99, f1%)
            if f1% <> 0% then enabled% = 0%
            return

L20200: REM Def/Enable Quantity to Distribute      QTY$
            call "CONVERT" (qtydst, 0.2, qty$)
            if qtydst > 0 then enabled% = 0%
            call "STRING" addr("LJ", qty$, 10%)
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L21160,         /* Quantity           */    ~
                              L21220,         /* Store              */    ~
                              L21320          /* Lot                */

            return


L21160: REM Def/Enable Quantity                    QTY$(1)
            temp = 0 : convert qty$(c%) to temp, data goto L21180
L21180:     if keyhit% = 0% then qtyrem = qtyrem + temp
            call "STRING" addr("LJ", qty$(c%), 10%)
            return

L21220: REM Def/Enable Store                       STORE$(1)
            savestore$ = store$(c%)
            if store$(c%) = " " and c% > 1% then                         ~
                                             store$(c%) = store$(c%-1%)
            if onestore$ = " " then return
                enabled%   = 0%
                store$(c%) = onestore$
                savestore$ = store$(c%)
            return

L21320: REM Def/Enable Lot                         LOT$(1)
            savelot$ = lot$(c%)
            inpmessage$ = "Enter the Lot from which the Material was "   ~
                        & "Issued"
            call "LOTENABL" (part$, enabled%, ll%, #4, #1)
            return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, qty$, openqty$, qty$(),    ~
                      store$(), lot$(), text$()

            keyhit%, edit%, maxlines%, l%, s%, c% = 0%

            mat qty$   = qtt$
            mat store$ = str$
            mat lot$   = ltt$

            maxlines% = passmax%
            linemax%  = min(maxallowed%, linemax%, dim(str$(),1))
            qtyrem    = qtydst
            if maxlines% = 0% then L29350

            for i% = 1% to maxlines%
                convert qty$(i%) to temp, data goto L29340
                qtyrem = qtyrem - temp
                if onestore$ <> " " then store$(i%) = onestore$
                readkey$ = str(part$,,25) & str(store$(i%),,3) & lot$(i%)
                call "READ100" (#2, readkey$, f1%)
                if f1% = 0% then text$(i%) = "*"
                call "GETCODE" (#3, store$(i%), str(text$(i%),4),        ~
                                                           0%, 99, f1%)
L29340:     next i%
L29350:     call "CONVERT" (qtyrem, 0.2, openqty$)
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
            if c_enabled% = 0% and p_enabled% = 0% then undo
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            init(" ") str$(), ltt$(), qtt$()
            passmax% = 0%
            for c% = 1% to maxlines%
                gosub clear_ser_distr
            next c%
            mat index% = zer
            goto inputmode

        undo
            u3% = 2%
            call "ASKUSER" (u3%, "*** UNDO ***",                         ~
                 "Press PF (1) to Return to Display", "- OR -",          ~
            "Press RETURN to UnDo the Effects of The Current Entries")
            if u3% = 1% then return
            if u3% <> 0% then undo
            return clear all
            goto inputmode

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              init (hex(9c)) fac$()
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()

              pfmsg$(1) = "(1)Start Over"
              str(pfmsg$(1),64) = "(13)Instructions"

              pfmsg$(2) = " "
              str(pfmsg$(2),64) = "(15)Print Screen"

              pfmsg$(3) = " "
              str(pfmsg$(3),64) = "(16)Exit Program"

              pfkey$ = hex(0001ff0d0f10)

              if fieldnr% > 1% then L40270
                 str(pfmsg$(1),20,18) = " "
                 str(pfkey$,3,1) = hex(ff)
                 goto L40300

L40270:          str(pfmsg$(1),20,18) = "(4)Previous Field"
                 str(pfkey$,3,1) = hex(04)

L40300:       on fieldnr% gosub L40360,         /* Part Code         */   ~
                                L40370          /* Dist. Quantity    */   ~

              goto L42000

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40360:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40370:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, screen%)
              init (hex(84)) lfac$()
              if fieldnr% > 0% then L41220
                 init(hex(86)) fac$()

              pfmsg$(1) = "(1)Start Over                          (10)See~
        ~ Stores/Lots     (13)Instructions"
              pfmsg$(2) = "(2)First  (4)Prev Page  (6)Prev Line   (11)App~
        ~end Lines        (15)Print Screen"
              pfmsg$(3) = "(3)Last   (5)Next Page  (7)Next Line   (12)Cle~
        ~ar Distribution  (16)Return"

              pfkey$ = hex(00010203040506070a0b0c0d0f10)

                 goto L42000

L41220:       if p_enabled% = 1% or c_enabled% = 1% then                 ~
                 pfmsg$(1) = "(1)Start Over"                             ~
              else                                                       ~
                 pfmsg$(1) = "(1)UnDo Changes"
              str(pfmsg$(1),40) = "(10)See Stores/Lots"
              str(pfmsg$(1),64) = "(13)Instructions"

              pfmsg$(2) = " "
              str(pfmsg$(2),64) = "(15)Print Screen"

              pfmsg$(3) = " "
              str(pfmsg$(3),64) = "(16)Edit Data"

              pfkey$ = hex(0001ffffff0a0d0f10)

              init(hex(84)) fac$()
              if edit% <> 0% then L41600
L41390:          if fieldnr% > 1% then L41460
                    str(pfmsg$(1),20,18) = " "
                    str(pfkey$,3,1) = hex(ff)
                    str(pfmsg$(3),64) = "(16)Edit Data"
                    str(pfkey$,9,1) = hex(10)
                      goto L41510

L41460:             str(pfmsg$(1),20,18) = "(4)Previous Field"
                    str(pfkey$,3,1) = hex(04)
                    str(pfmsg$(3),64) = " "
                    str(pfkey$,9,1) = hex(ff)

L41510:          if c% > 1% then L41560
                    str(pfmsg$(2),20,18) = " "
                    str(pfkey$,4,1) = hex(ff)
                       goto L41660

L41560:             str(pfmsg$(2),20,18) = "(6)Previous Line"
                    str(pfkey$,4,1) = hex(06)
                       goto L41660

L41600:       if c% <= maxlines% then L41630
                 goto L41390

L41630:             str(pfmsg$(3),64) = " "
                    str(pfkey$,9,1) = hex(ff)

L41660:       on fieldnr% gosub L41730,         /* Quantity          */   ~
                                L41720,         /* Store             */   ~
                                L41720          /* Lot               */
              goto L42000

                fac$(screen%,fieldnr%) = hex(80):return  /* Up / Low   */
L41720:         fac$(screen%,fieldnr%) = hex(81):return  /* Upper Only */
L41730:         fac$(screen%,fieldnr%) = hex(82):return  /* Numeric    */

L42000: REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

L42060:     accept                                                       ~
               at (01,02),                                               ~
                  "Distribute Quantities Over Store/Lot Combinations",   ~
               at (01,65), "Today:",                                     ~
               at (01,72), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Part Code:",                                 ~
               at (06,20), fac(lfac$( 1)), part$                , ch(25),~
               at (06,48), fac(hex(8c))  , partdescr$           , ch(32),~
                                                                         ~
               at (07,02), "Quantity to Distribute:",                    ~
               at (07,30), fac(lfac$( 2)), qty$                 , ch(10),~
                                                                         ~
               at (07,50), "Quantity Remaining:",                        ~
               at (07,70), fac(hex(84))  , openqty$             , ch(10),~
                                                                         ~
               at (09,02), fac(hex(ac))  , header$              , ch(79),~
                                                                         ~
               at (10,15), fac(fac$( 1,1)), qty$  (l%+ 1%)      , ch(10),~
               at (11,15), fac(fac$( 2,1)), qty$  (l%+ 2%)      , ch(10),~
               at (12,15), fac(fac$( 3,1)), qty$  (l%+ 3%)      , ch(10),~
               at (13,15), fac(fac$( 4,1)), qty$  (l%+ 4%)      , ch(10),~
               at (14,15), fac(fac$( 5,1)), qty$  (l%+ 5%)      , ch(10),~
               at (15,15), fac(fac$( 6,1)), qty$  (l%+ 6%)      , ch(10),~
               at (16,15), fac(fac$( 7,1)), qty$  (l%+ 7%)      , ch(10),~
               at (17,15), fac(fac$( 8,1)), qty$  (l%+ 8%)      , ch(10),~
               at (18,15), fac(fac$( 9,1)), qty$  (l%+ 9%)      , ch(10),~
               at (19,15), fac(fac$(10,1)), qty$  (l%+10%)      , ch(10),~
                                                                         ~
               at (10,27), fac(fac$( 1,2)), store$(l%+ 1%)      , ch(03),~
               at (11,27), fac(fac$( 2,2)), store$(l%+ 2%)      , ch(03),~
               at (12,27), fac(fac$( 3,2)), store$(l%+ 3%)      , ch(03),~
               at (13,27), fac(fac$( 4,2)), store$(l%+ 4%)      , ch(03),~
               at (14,27), fac(fac$( 5,2)), store$(l%+ 5%)      , ch(03),~
               at (15,27), fac(fac$( 6,2)), store$(l%+ 6%)      , ch(03),~
               at (16,27), fac(fac$( 7,2)), store$(l%+ 7%)      , ch(03),~
               at (17,27), fac(fac$( 8,2)), store$(l%+ 8%)      , ch(03),~
               at (18,27), fac(fac$( 9,2)), store$(l%+ 9%)      , ch(03),~
               at (19,27), fac(fac$(10,2)), store$(l%+10%)      , ch(03),~
                                                                         ~
               at (10,32), fac(fac$( 1,3)), str(lot$(l%+ 1%),1,lot%),    ~
               at (11,32), fac(fac$( 2,3)), str(lot$(l%+ 2%),1,lot%),    ~
               at (12,32), fac(fac$( 3,3)), str(lot$(l%+ 3%),1,lot%),    ~
               at (13,32), fac(fac$( 4,3)), str(lot$(l%+ 4%),1,lot%),    ~
               at (14,32), fac(fac$( 5,3)), str(lot$(l%+ 5%),1,lot%),    ~
               at (15,32), fac(fac$( 6,3)), str(lot$(l%+ 6%),1,lot%),    ~
               at (16,32), fac(fac$( 7,3)), str(lot$(l%+ 7%),1,lot%),    ~
               at (17,32), fac(fac$( 8,3)), str(lot$(l%+ 8%),1,lot%),    ~
               at (18,32), fac(fac$( 9,3)), str(lot$(l%+ 9%),1,lot%),    ~
               at (19,32), fac(fac$(10,3)), str(lot$(l%+10%),1,lot%),    ~
                                                                         ~
               at (10,40), fac(hex(8c))   , text$ (l%+ 1%)      , ch(30),~
               at (11,40), fac(hex(8c))   , text$ (l%+ 2%)      , ch(30),~
               at (12,40), fac(hex(8c))   , text$ (l%+ 3%)      , ch(30),~
               at (13,40), fac(hex(8c))   , text$ (l%+ 4%)      , ch(30),~
               at (14,40), fac(hex(8c))   , text$ (l%+ 5%)      , ch(30),~
               at (15,40), fac(hex(8c))   , text$ (l%+ 6%)      , ch(30),~
               at (16,40), fac(hex(8c))   , text$ (l%+ 7%)      , ch(30),~
               at (17,40), fac(hex(8c))   , text$ (l%+ 8%)      , ch(30),~
               at (18,40), fac(hex(8c))   , text$ (l%+ 9%)      , ch(30),~
               at (19,40), fac(hex(8c))   , text$ (l%+10%)      , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)), pfmsg$(1%)             , ch(79),~
               at (23,02), fac(hex(8c)), pfmsg$(2%)             , ch(79),~
               at (24,02), fac(hex(8c)), pfmsg$(3%)             , ch(79),~
                                                                         ~
               keys(pfkey$),                                             ~
               key (keyhit%)


               if keyhit% <> 13 then L42740
                  call "MANUAL" ("LOTDISN ")
                  goto L42060

L42740:        if keyhit% <> 10 then L42780
                  call "HNYQDISP" (part$, #1, #2, #2, #4)
                  goto L42060

L42780:        if keyhit% <> 15 then L42820
                  call "PRNTSCRN"
                  goto L42060

L42820:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50130,         /* Part Code         */     ~
                              L50220          /* Dist. Quantity    */     ~

            return

L50130: REM Test for Part Code                    PART$
            call "GETCODE" (#1, part$, partdescr$, 0%, 0, f1%)
            if f1% = 1% then L50180
                errormsg$ = "Part code is not on File."
                return
L50180:     call "LOTENABL" (part$, lot_enable%, lot%, #4, #1)
            return


L50220: REM Test for Quantity to Distribute       QTY$
            call "NUMTEST" (qty$, 0, 9e7, errormsg$, -0.2, temp)
            if errormsg$ <> " " then return
                if maxlines% = 0% then qtyrem = temp
                call "CONVERT" (qtyrem, 0.2, openqty$)
                return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51170,         /* Quantity          */     ~
                              L51230,         /* Store             */     ~
                              L51310          /* Lot               */

            if fieldnr% <> 1% or errormsg$ = " " then return
               temp = 0 : convert qty$(c%) to temp, data goto L51140
L51140:        qtyrem = qtyrem - temp
               call "CONVERT" (qtyrem, 0.2, openqty$)
               return

L51170: REM Test for Quantity                     QTY$()
            call "NUMTEST" (qty$(c%), 0, 9e7, errormsg$, -0.2, temp)
            if errormsg$ <> " " then return
            if edit% = 0% then return
                savestore$ = store$(c%)

L51230: REM Test for Store                        STORE$()
            if savestore$ = " " or savestore$ = store$(c%) then L51260
               gosub clear_ser_distr
L51260:     call "GETCODE" (#3, store$(c%), str(text$(c%),4,30),0%,0,f1%)
            if f1% = 0% then errormsg$ = "Store Must Be on File."
            if edit% = 0% then return
                savelot$ = lot$(c%)

L51310: REM Test for Lot                          LOT$()
            if lot$(c%) = "?" then lot$(c%) = " "
            if savelot$ = " " or savelot$ = lot$(c%) then L51340
                gosub clear_ser_distr
L51340:     if lot_enable% < 2% or lot$(c%) <> " " then L51370
                errormsg$ = "Lot Number required for this Part."
                return
L51370:     readkey$ = str(part$,,25) & str(store$(c%),,3) & lot$(c%)
            call "READ100" (#2, readkey$, f1%)
            if f1% = 0% then str(text$(c%),,1) = "*" else                ~
                             str(text$(c%),,1) = " "
            if function% = 2% then L51670

*        Test for withdrawls. Lot must exist.
            if f1% <> 0% then L51470
               errormsg$ = "Store/Lot not on file."
               return
L51470:     temp1 = 0 : convert qty$(c%) to temp1, data goto L51550
            for test% = 1% to maxlines%
                if test% = c% then L51540
                if store$(test%) <> store$(c%) then L51540
                if lot$(test%) <> lot$(c%) then L51540
                   temp = 0:convert qty$(test%) to temp, data goto L51530
L51530:            temp1 = temp1 + temp
L51540:     next test%
L51550:     call "HNYAVAIL" (#1, #2, part$, store$(c%), lot$(c%),        ~
                                         errormsg$, temp1, temp, f1%)
            if errormsg$ > " " then return
            if p_enabled% = 1% then gosub issue_to_parent
            if c_enabled% = 1% and p_enabled% = 0% then                  ~
               gosub select_serial_numbers
            if errormsg$ > " " then return
L51620:     temp = 0 : convert qty$(c%) to temp, data goto L51630
L51630:     qtyrem = qtyrem - temp
            call "CONVERT" (qtyrem, 0.2, openqty$)
            return

L51670
*        Test for Additions.
            if f1% = 1% then L51720
                call "LOTVALID" (part$, store$(c%), lot$(c%), #4, #1, #2,~
                                 errormsg$)
                if errormsg$ > " " then return
L51720:     if c_enabled% = 1% then gosub select_serial_numbers
            if errormsg$ = " " then L51620
            return


        issue_to_parent
            temp1 = 0 : convert qty$(c%) to temp1, data goto L51790
L51790:     call "SERKIT" (parent_part$, /* Part code to Issue To      */~
                           part$,        /* Comp Part to Issue From    */~
                           store$(c%),   /* S/N Location to Select from*/~
                           lot$(c%),     /* (Store and Lot Codes).     */~
                           parent_qty,   /* Qty of Parent to Issue To  */~
                           temp1,        /* Comp.  Qty to Issue        */~
                           index%(c%),   /* Pointer For Work File Use  */~
                           avglines%,    /* Average # Lines per Documnt*/~
                           trantype$,    /* Source Transaction Type.   */~
                                         /* (JK = Job Kitting).        */~
                           trankey$,     /* Source Transaction Key     */~
                           status$,      /* Status of Parent S/N's to  */~
                                         /* Issue To.                  */~
                           source$,      /* Status of Component S/N's  */~
                                         /* to Issue From.             */~
                           0%,           /* Operation to Perform       */~
                                         /* 0% = Normal Input/Edit,    */~
                                         /*      by INDEX%,            */~
                                         /* 1% = Save Data of INDEX%   */~
                                         /* 2% = Start Over, if INDEX% */~
                                         /*      > 0% then only the    */~
                                         /*      line/data pointed to  */~
                                         /*      by INDEX% is cleared, */~
                                         /*      else ALL is cleared.  */~
                           errormsg$,    /* Returned Error Message     */~
                           #4,           /* SYSFILE2 UFB               */~
                           #1,           /* HNYMASTR UFB               */~
                           #5,           /* SERMASTR UFB               */~
                           #6,           /* SERTIF   UFB               */~
                           #7)           /* SERWORK  UFB               */
            return

        select_serial_numbers
            location$ = str(store$(c%)) & lot$(c%)
            temp1 = 0 : convert qty$(c%) to temp1, data goto L52140
L52140:     call "SERSELCT" (part$,      /* Part code                  */~
                             location$,  /* S/N Location to Select from*/~
                             temp1,      /* Qty to Assign S/N's To     */~
                             index%(c%), /* Pointer For Work File Use  */~
                             avglines%,  /* Average # Lines per Documnt*/~
                             trantype$,  /* Source Transaction Type.   */~
                             trankey$,   /* Source Transaction Key.    */~
                             dest$,      /* Status to Change S/N to.   */~
                             source$,    /* Status to Select/Chg from  */~
                             errormsg$,  /* Returned Error Message     */~
                             #4,         /* SYSFILE2 UFB               */~
                             #1,         /* HNYMASTR UFB               */~
                             #5,         /* SERMASTR UFB               */~
                             #7)         /* SERWORK  UFB               */
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

        exit_routine
            end
