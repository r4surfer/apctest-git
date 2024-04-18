        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    OOO   M   M   AAA   N   N   AAA   L              *~
            *  B   B  O   O  MM MM  A   A  NN  N  A   A  L              *~
            *  BBBB   O   O  M M M  AAAAA  N N N  AAAAA  L              *~
            *  B   B  O   O  M   M  A   A  N  NN  A   A  L              *~
            *  BBBB    OOO   M   M  A   A  N   N  A   A  LLLLL          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMANAL  - PROVIDES COMPLETE ANALYSIS AND VERIFICATION FOR*~
            *            ALL STRUCTURES IN THE BOM.  CAN BE RUN FOR A   *~
            *            RANGE OF PARTS INCLUDING 'ALL'. CHECKS         *~
            *            STRUCTURE FOR REDUNDANCY AND REPORTS ON PART   *~
            *            USEAGE, # TIMES PARENT, COMPONENT, ETC.        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/23/82 ! ORIGINAL                                 ! GLW *~
            * 03/14/84 ! CORRECT BOMID PROCESSING, COUNT ALL COMPS! ECR *~
            * 12/04/85 ! CORRECT BLANK BOMID LOOP                 ! MJB *~
            * 09/22/86 ! BOMMASTR FORMAT CHANGED                  ! LKM *~
            * 05/13/87 ! HNYMASTR record length mod for Std Cost  ! JIM *~
            * 06/22/87 ! Changed Range select to 1 part or ALL    ! MJB *~
            *    --    !  and corrected infinite loop             !  -  *~
            * 03/28/89 ! Modified BOM ID edit, blanked display of ! MLJ *~
            *          !  verification msg on STARTOVER           !     *~
            * 11/06/91 ! CMS/DEC 'MASK' Project                   ! SID *~
            * 11/12/91 ! Added PLOWCODE to the BOM ID Field and   ! SID *~
            *          !  changed so that Verify 'ALL' Part Number!     *~
            *          !  would print properly.                   !     *~
            * 03/24/94 ! Infinite Loop at 30343 if Sp. Bom Used   ! KAB *~
            *          ! Fix up Plowcode so it doesn't die til    !     *~
            *          !   end of file reached.                   !     *~
            *          ! Minor code out of sequence at 30060 &    !     *~
            *          !   [RETURN CLEAR GOTO 19162] = RETURN!!!  !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            bom$(490)3,                  /* BOM LIST                   */~
            bomid$3,                     /* WHICH ALT BOM?             */~
            bomid1$3,                    /* WHICH ALT BOM HAS REDUNDCY */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            component$25,                /* THE COMPONENT FOUND        */~
            componentkey$28,             /* COMPONENT - BOM FOUND      */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            effdate$8,                   /* EFFECTIVITY DATE           */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            firstpart$25,                /* FROM PART                  */~
            fpart$25,                    /* FROM PART FOR DISPLAY      */~
            fpartdescr$32,               /* FROM PART DESCRIPTION      */~
            hdr$(3)79,                   /* PLOWCODE Argument          */~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED)    */~
            inbom$3,                     /* Selected BOMID             */~
            inbomdescr$30,               /* Selected BOMID Description */~
            incl(2),                     /* PLOWCODE Argument          */~
            incl$(2)25,                  /* PLOWCODE Argument          */~
            index%(1),                   /* RECVR  FOR EFFDATE SEARCH  */~
            inpmessage$79,               /* INPUT SCREEN MESSAGE       */~
            lastpart$25,                 /* TO PART                    */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* Screen line #2             */~
            lockey$100,                  /* Location File Key          */~
            m$79,                        /* LAST VERIFICATION REPORT   */~
            msg$60,                      /* Message for SHOSTAT        */~
            nct$6,                       /* Total Number of Components */~
            nextpart$25,                 /* NEXT PART TO VERIFY        */~
            nextpartkey$31,              /* NEST PART - BOM TO VERIFY  */~
            nextbomid$3,                 /* BOM OF NEXT PART TO VERIFY */~
            mm$132,                      /* PRINT FIELD                */~
            p$(1000)31,                  /* PLOWNEXT KEY ARRAY         */~
            ppart$25,                    /* PRINT FIELD FOR P$(J%)     */~
            pseq$3,                      /* PRINT FIELD                */~
            pl$4,                        /* PRINT FIELD                */~
            part$25,                     /* USED IN SUB 50             */~
            partkey$28,                  /* USED IN SUB 50             */~
            ppart1$25,                   /* PRINT FIELD FOR P$(L%)     */~
            readkey$99,                  /* PLOWNEXT KEY               */~
            readkey1$56,                 /* PLOWALTS KEY               */~
            readkey2$29,                 /* ENGMASTR KEY               */~
            red$1,                       /* = 'Y' IF REDUNDANCY FOUND  */~
            reporttype$1,                /* FULL 'F' OR EXCEPTION 'E'  */~
            seq$3,                       /* BOM SEQUENCE               */~
            spbomid$3,                   /* Specific BOMID             */~
            testpart$31,                 /* A READ NEXT KEY            */~
            yymmdd$(490)6                /* PRODUCTION CALENDAR        */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "OPENFILE"*/~
            axd$(64)4                    /* AXD POINTER FROM "OPENFILE"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 4 ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * # 5 ! BOMMASTR ! BILL OF MATERIALS RELATIONSHIP FILE      *~
            * #10 ! CALMASTR ! PRODUCTIN CALENDAR                       *~
            * #11 ! ENGMASTR ! ENGINEERING MASTER FILE                  *~
            *************************************************************

            select  #4, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90,  keylen = 4, dup

            select # 5, "BOMMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos =  26, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 56

           select #10, "CALMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos = 1, keylen = 2

           select #11, "ENGMASTR" ,                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos = 1, keylen = 29

            call "SHOSTAT" ("Opening Files, One Moment Please.")
            call "OPENFILE" (# 4, "SHARE", f2%( 4), rslt$( 4), axd$( 4))
            call "OPENFILE" (# 5, "SHARE", f2%( 5), rslt$( 5), axd$( 5))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#11, "SHARE", f2%(11), rslt$(11), axd$(11))

            if f2%(5) = 0 then L09000
               call "OPENFILE" (#5, "OUTPT", f2%( 5), rslt$( 5), axd$( 5))
               close #5
               call "OPENFILE" (#5, "SHARE", f2%( 5), rslt$( 5), axd$( 5))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************
            date$ = date
            call "DATEFMT" (date$)
            str(line2$,62) = " BOMANAL: " & str(cms2v$,,8)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

        REM GET PRODUCTION CALENDAR IN YYMMDD FORMAT
            call "READ100" (#10, "10", f1%(10))
                get #10, using L09230, str(yymmdd$(),    1, 1470)
            call "READ100" (#10, "11", f1%(10))
                get #10, using L09230, str(yymmdd$(), 1471, 1470)
L09230:             FMT XX(2), CH(1470)

            effdate$ = date$
            call "DATUNFMT" (effdate$)
            search yymmdd$() = str(effdate$,,6) to index%() step 6
            if index%(1) <> 0% then dateindex% = (index%(1) +5) / 6      ~
                               else dateindex% = 0%


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, fpart$, nextpart$, inbom$, ~
                   component$, red$, testpart$, readkey$, readkey1$,     ~
                   p$(), fpartdescr$, mm$, bomid$, effdate$, inbomdescr$,~
                   reporttype$

           lin , count = 0

            for fieldnr% = 1 to 3
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10212
L10152:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then L65000
                      if keyhit% <>  0 then       L10152
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10152
L10212:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************

L11060:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 3 then L11060

L11125:     gosub'051(fieldnr%)
                  if enabled% = 0% then L11060
                  edtmessage$ = inpmessage$
L11130:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11130
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11130
            if fpart$ <> "ALL" then L11200
               inbom$, inbomdescr$ = " " : goto L11060
L11200:     if fieldnr% <> 1% then L11220
               fieldnr% = 2% : goto L11125
L11220:        fieldnr% = 0%
               edtmessage$ = "To Modify Displayed Values, Position "     ~
                           & " Cursor To Desired Value And Press RETURN."
               goto L11060

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
           if fpart$ <> "ALL" then msg$ = "Verifying BOM '" & inbom$ &   ~
                                          "' for part '" & fpart$ & "' " ~
                              else msg$ = "Verifying the ENTIRE Bill " & ~
                                          "of Materials Master File"
           call "SHOSTAT" (msg$)
           init(" ") m$
           select printer(134)
            gosub newpage
            gosub L30000        /* TO PERFORM THE VERIFICATION          */
            if red = 1 then goto  L19067
               nu, nc, nct = 0
               print using L31010
               goto L19072
L19067:     print using L31200
L19072:     close printer

           if fpart$ = "ALL" then L19180

           if red = 1 then m$ = "Verification of BOM '" & inbom$ &       ~
                    "' for part '" & fpart$ & "' Found Redundancies"

           if red = 0 then m$ = "Verification of BOM '" & inbom$ &       ~
                    "' for part '" & fpart$ & "' Found No Redundancies"
           goto L19240

L19180:    if red = 1 then m$ = "Verification of the Bill of Materials" &~
                                " Master File Found Redundancies"

           if red = 0 then m$ = "Verification of the Bill of Materials" &~
                                " Master File Found NO Redundancies"

L19240:    red = 0
           goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  inpmessage$ = " "
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* Part Number      */~
                                    L20200,         /* BOM ID           */~
                                    L20300          /* REPORT TYPE      */

                     return
L20100: REM Default/enable for verify Part Number
            inpmessage$ = "Enter a part number, 'ALL' or '?' to see a lis~
        ~t of part numbers."
            return

L20200: REM Default/enable for BOMID
            if fpart$ = "ALL" then enabled% = 0%
            inpmessage$ = "Enter the BOM ID for part " & fpart$ &        ~
                          " you like to verify OR '?' to see a list."

            return

L20300: REM Default/enable for report type
            reporttype$ = "F"
            inpmessage$ = "Enter 'F' for full detailed report, or 'E' "  ~
                           & "to report only Exceptions."
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 0%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            m$ = " "
            goto inputmode


L30000: REM *************************************************************~
            *       V E R I F Y   T H E   B O M                         *~
            * CHECK FOR REDUNDANCY AND COUNT HOW MANY TIMES USED AS A   *~
            * COMPONENT AND HOW MANY COMPONENTS EACH PARENT HAS.        *~
            *************************************************************

        REM FIRST PART, FIRST BILL STRUCTURE
           testpart$ = str(firstpart$, 1, 25) & str(inbom$,1,3) & hex(00)

           if fpart$ = "ALL" then break% = 0% else break% = 28%

        REM Get 1st BOM Record
L30055:     call "PLOWNEXT" (#5, testpart$, break%, f1%(5))
                if f1%(5) = 0 then return
            if str(testpart$,29,3) = "  0" then L30055   /* Header */

           get #5, using L30085, nextpart$, nextbomid$
L30085:    FMT XX(25), CH(25), CH(3)

        REM Test for range ALL
           if nextpart$ >= firstpart$  and                               ~
              nextpart$ <= lastpart$  then  L30150
           return

L30150:              l% = 0
                     maxlevel = 0
                     nc   = 0
                     nu   = 0
                     nct  = 0

        REM ******* FIRST COUNT THE NUMBER OF COMPONENTS ****************

           readkey$ = str(nextpart$,1,25) & str(nextbomid$,1,3) & " "
L30190:              call "PLOWNEXT" (#5, readkey$, 28%, f1%(5))
                     if f1%(5) = 0 then goto L30215
                     if str(readkey$,29,3) = "  0" then L30190 /* Header */
                     nc = nc + 1
                     goto L30190

L30215: REM ****** NOW COUNT HOW MANY TIMES USED AS COMPONENT ************

           readkey1$ = str(nextpart$,1,25) & " "
L30230:              call "PLOWALTS" (#5, readkey1$, 1%, 25%, f1%(5))
                     if f1%(5) = 0 then goto  L30255
                     if str(readkey1$,54,3) = "  0" then L30230
                     nu = nu + 1
                     goto L30230

L30255: REM ******* NOW CHECK BILL STRUCTURE FOR REDUNDANCY *************

           nextpartkey$ = str(nextpart$, 1, 25) & str(nextbomid$, 1, 3)
           gosub'50(nextpartkey$)  /* PERFORM VERIFICATION */
L30275:    gosub processlastpart   /* COLLECT REPORT DATA  */
           testpart$ = str(nextpart$,1,25)
           str(testpart$,26,3) = str(nextbomid$,,3)
           init(hex(ff)) str(testpart$,29,3)
           goto L30055              /* GET THE NEXT PART    */

         deffn'50(partkey$)

           l% = l% + 1
           if l% > maxlevel then maxlevel = l%
           p$(l%) = str(partkey$, 1, 28) & " "
L30325:    call "PLOWNEXT" ( #5, p$(l%), 28%, f1%(5))
                     if f1%(5) = 0 then goto L30435
           if str(p$(l%),29,3) = "  0" then L30325
           get #5, using L30340, component$, seq$, spbomid$
L30340:              FMT CH(25), POS(54), CH(3), POS(92), CH(3)
           if spbomid$ = " " then L30345
                str(bomid$,1,3) = spbomid$
                goto L30350
L30345:    gosub'200(component$)                   /* GET BOM FOR PART */
L30350:    componentkey$ = str(component$, 1, 25) & str(bomid$, 1, 3)
           nct = nct + 1                 /* Total number of components */
        REM CHECK FOR REDUNDANCY
           for j% = 1 to l%
               if component$ = str(p$(j%), 1, 25) then goto L30395
           next j%
           goto L30425

        REM REDUNDANT PART - SET FLAG AND PRINT
L30395:              red$ = "Y"
                     red = 1
                     for k% = 1% to l%
                         return clear
                     next k%
                     goto L30275

        REM NO REDUNDANCY  - GO TO NEXT LEVEL
L30425:    if l% < 1000% then gosub'50(componentkey$)
                     goto L30325
L30435:              l% = l% - 1
                     return

        processlastpart:
           if reporttype$ = "F" then goto L30465
           if red$ <> "Y"  then return
L30465:    count = count + 1
           call "CONVERT" (nct, -0.2, nct$)

           if red$ <> "Y" then L30500
              ppart1$ = str(p$(l%), 1, 25)
              bomid1$ = str(p$(l%),26,  3)
              ppart$ =  str(p$(j%), 1, 25)
              convert l%   to pl$, pic(####)
              pseq$ = seq$

L30500:  REM PRINT OUT A REPORT LINE
           lin  = lin  + 1
           if lin  > 55 then gosub newpage

           print using L31180, count, nextpart$, nextbomid$,(maxlevel-1), ~
                       nc, nct$, nu, pl$, pseq$, ppart1$, bomid1$, ppart$
           init(" ") ppart$, pseq$, pl$, red$, ppart1$, bomid1$
           l%, j%, maxlevel, nc, nu, nct = 0
           return

L31010: %+---------------------------------------------------------------~
        ~--+----- THERE ARE NO REDUNDANCIES IN YOUR BOM ------------------~
        ~--+
L31040: %!###############################################################~
        ~#################################################################~
        ~##!
L31140: %!   PARENT PART TESTED            BOM  # OF   # COMPTS    X'S-US~
        ~ED! AT  THE COMPONENT                 BOM IS A REDUNDANT         ~
        ~  !
L31160: %!   IN PART CODE ORDER            ID  LEVELS  LVL1/TOTL    AS CO~
        ~MP! LVL SEQ OF PART                   ID  PART CODE              ~
        ~  !
L31180: %!#####) ######################### ###  ##### #### / ###### #####~
        ~# !#### ### ######################### ### #######################~
        ~##!
L31200: %+---------------------------------------------------------------~
        ~--+--------------------------------------------------------------~
        ~--+

          newpage
           print page
           print using L31200
           if fpart$ = "ALL" then L31270
                mm$ = "BOM ANALYSIS REPORT FOR PART '" & fpart$ &        ~
                      "' USING BOM ID '" & inbom$ & "' "
              goto L31300
L31270:    mm$ = "BOM ANALYSIS REPORT FOR ALL ASSEMBLIES IN THE BILL " & ~
                 "OF MATERIALS MASTER FILE"

L31300:    call "STRING" addr("CT", mm$, 132%)

           print using L31040, mm$
           print using L31200
           print using L31140, "#", "#"
           print using L31160
           print using L31200
           lin  = 5
                     return


        REM *************************************************************~
            *      G E T   E F F E C T I V E   B O M                    *~
            *                                                           *~
            * GETS THE CURRENT BOM FOR PASSED PART BASED ON DATEINDEX%  *~
            *************************************************************

            deffn'200(part$)
                 bomid$ = "   "
                 if dateindex% = 0 then return   /* OFF PROD CALENDER? */
                    readkey2$ = str(part$, 1, 25) & "1" & " "
                    call "READ102" (#11, readkey2$, f1%(11))
                    if f1%(11) <> 1 then return
                       get #11, using L32130, readkey2$, bom$()
L32130:                         FMT CH(29), 490 * CH(3)
                       if str(readkey2$,,25)<>str(part$,,25) then return
                       if bom$(dateindex%) = " " then return
                       bomid$ = bom$(dateindex%)
                       return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  str(line2$,,60) = "Input Part Number Selection"
                  on fieldnr% gosub L40150,         /* Part Number      */~
                                    L40150,         /* Bom ID           */~
                                    L40150          /* REPORT TYPE      */
                     goto L40220

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40150:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40220:     accept                                                       ~
               at (01,02), "Bill of Materials Analysis and Verification",~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(84)), m$                     , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Verify Part Number",                                  ~
               at (06,22), fac(lfac$( 1)), fpart$               , ch(25),~
               at (06,48), fac(hex(84)), fpartdescr$            , ch(32),~
               at (07,02), "BOM ID:",                                    ~
               at (07,22), fac(lfac$( 2)), inbom$               , ch(03),~
               at (07,48), fac(hex(84)), inbomdescr$            , ch(30),~
               at (08,02), "Report Desired",                             ~
               at (08,22), fac(lfac$( 3)), reporttype$          , ch(1), ~
                                                                         ~
               at (11,02), "WARNING! Verification of the entire file may ~
        ~take a LONG time.",                                              ~
                                                                         ~
               at (13,02), "The BOM for the part specified will be analyz~
        ~ed for:",                                                        ~
               at (14,02), "   (1) How many components each parent has", ~
               at (15,02), "   (2) How often it is used as a component", ~
               at (16,02), "   (3) How many levels are below the parent",~
               at (17,02), "   (4) Whether the structure is redundant",  ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40690
                  call "MANUAL" ("BOMANAL")
                  goto L40220

L40690:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40220

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41150,         /* Part Number      */~
                                    L41150,         /* BOM ID           */~
                                    L41150          /* REPORT TYPE      */
                     goto L41220

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41150:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41220:     accept                                                       ~
               at (01,02), "Bill of Materials Analysis and Verification",~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(84)), m$                     , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Verify Part Number",                                  ~
               at (06,22), fac(lfac$( 1)), fpart$               , ch(25),~
               at (06,48), fac(hex(84)), fpartdescr$            , ch(32),~
               at (07,02), "BOM ID:",                                    ~
               at (07,22), fac(lfac$( 2)), inbom$               , ch(03),~
               at (07,48), fac(hex(84)), inbomdescr$            , ch(30),~
               at (08,02), "Report Desired",                             ~
               at (08,22), fac(lfac$( 3)), reporttype$          , ch(1), ~
                                                                         ~
               at (11,02), "WARNING! Verification of the entire file may ~
        ~take a LONG time.",                                              ~
                                                                         ~
               at (13,02), "The BOM for the part specified will be analyz~
        ~ed for:",                                                        ~
               at (14,02), "   (1) How many components each parent has", ~
               at (15,02), "   (2) How often it is used as a component", ~
               at (16,02), "   (3) How many levels are below the parent",~
               at (17,02), "   (4) Whether the structure is redundant",  ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
                                                                         ~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Verify BOM",                             ~
                                                                         ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41510
                  call "MANUAL" ("BOMANAL")
                  goto L41220

L41510:        if keyhit% <> 15 then L41550
                  call "PRNTSCRN"
                  goto L41220

L41550:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$, m$ = " "
                  on fieldnr% gosub L50100,         /* Part Number      */~
                                    L50200,         /* BOM ID           */~
                                    L50300          /* REPORT TYPE      */
                     return
L50100: REM Test data for verify Part Number
            if fpart$ <> "ALL" then L50110
               init(hex(00)) firstpart$
               init(hex(ff)) lastpart$
               inbom$, inbomdescr$ = " "
               fpartdescr$ = "Verify ALL assemblies in file"
               return

L50110:     if fpart$  <> " " then L50140
                errormsg$ = "Part Number Cannot Be Blank!!"
                return
L50140:     call "GETCODE" (#4, fpart$, fpartdescr$, 0%, 0, f1%(4))
                if f1%(4) = 0 then L50180
                firstpart$, lastpart$ = fpart$
                return

L50180:         errormsg$ = "Part Not On File: " & fpart$
                return

L50200: REM Test data for BOM ID
            i% = pos(inbom$ = " ")
            if i% > 0% then str(inbom$,i%) = " "
            readkey$ = str(fpart$,1,25) & str(inbom$,1,3) & "  0"
            call "READ100" (#5, readkey$, f1%(5))
            if f1%(5) = 1 then L50272
              hdr$(1), hdr$(3) = "  "
              hdr$(2) = "  Assembly Number             BOM"  &           ~
                        " Description"
              lockey$ = hex(06) & "Select the Bill of Materials to" &    ~
                      " Verify.  Use PF-16 to Cancel Selection"

*            INCL(1) = 26.25 : INCL$(1) = FPART$
*            CALL "PLOWCODE" (#5, READKEY$, LOCKEY$, -5028%, -.30,      ~
*                             F1%(5), HDR$(), 0, 0, INCL(), INCL$())

              incl(1) = 54.03 : incl$(1) = "  0"
              call "PLOWCODE" (#5, readkey$, lockey$,  5025%, -.30,      ~
                               f1%(5), hdr$(), 0, 0, incl(), incl$())

              if f1%(5) = 1% then L50264
L50248:          errormsg$ = "Bill of Materials " & inbom$ & " does not" ~
                           & " exist or no BOM selected."
                 return

L50264:       fpart$ = str(readkey$,1%,25%):inbom$ = str(readkey$,26%,3%)
              readkey$ = str(fpart$,1,25) & str(inbom$,1,3) & "  0"
              call "READ100" (#5, readkey$, f1%(5))
                 if f1%(5) = 0% then L50248
L50272:       get #5 using L50276, inbomdescr$
L50276:                   FMT POS(57), CH(30)
              call "GETCODE" (#4, fpart$, fpartdescr$, 0%, 99, f1%(4))
                if f1%(4) <> 0% then return
              fpartdescr$ = "Part Not On File"
              return

L50300: REM Test data for report type
            if reporttype$ = "F" or reporttype$ = "E" then return
            errormsg$ = "Report Type must Be 'F' or 'E', please respecify"
            return


L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
