        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  RRRR   TTTTT  EEEEE  PPPP   RRRR   IIIII  N   N  TTTTT   *~
            *  R   R    T    E      P   P  R   R    I    NN  N    T     *~
            *  RRRR     T    EEEE   PPPP   RRRR     I    N N N    T     *~
            *  R   R    T    E      P      R   R    I    N  NN    T     *~
            *  R   R    T    EEEEE  P      R   R  IIIII  N   N    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * RTEPRINT - Program to print  one, many or all Routings as *~
            *             requested.  Includes Alternates and Text.     *~
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
            * 11/19/85 ! ORIGINAL                                 ! MJB *~
            * 09/17/86 ! RTEMASTR file format change              ! HES *~
            * 10/30/87 ! Fixed Printing of Move Queue Days        ! KAB *~
            * 07/20/88 ! Fixed Printing of Text on last line of pg! JDH *~
            * 04/25/91 !(PRR 11560) Added call to SETPRNT         ! RJB *~
            *          !(PRR 10151) Expanded Assembly Description !     *~
            *          !     Variable ASSYDESCR$ and its picture  !     *~
            *          !     statement to 32 characters.          !     *~
            *          !(PRR 10173) Expanded Test Section for the !     *~
            *          !     Starting and Ending Assy # to include!     *~
            *          !     a GETCODE and added displaying the   !     *~
            *          !     description to the screens.          !     *~
            *          !(PRR 10092) Expanded Test Section for the !     *~
            *          !     Starting and Ending Route Id's to    !     *~
            *          !     include a PLOWCODE.                  !     *~
            * 06/20/91 !QC-FIXES Rewrote Screens, DEF-ENABLES, &  ! RJB *~
            *          !     TEST SECTIONS to bring them up to the!     *~
            *          !     current standards and the link the   !     *~
            *          !     Part # to the Route ID.              !     *~
            * 06/25/91 !QC-FIXES Added Program/Revision to Screen !     *~
            *          !     , put PF Desc. in right place, Corr. !     *~
            *          !     the Plowcode's handling of a '?'.    !     *~
            * 07/09/91 !QC_FIXES Corrected postioning of Date and !     *~
            *          !     Line2 on the screens.                !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**
        dim                                                              ~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            endpart$25,                  /* Ending Assembly #          */~
            endrteid$3,                  /* Ending Route ID            */~
            epartdesc$34,                /* Ending Assembly Descrip.   */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* Second Line of Header      */~
            prttxt$1,                    /* Text Print Flag            */~
            spartdesc$34,                /* Starting Assembly Descip.  */~
            startpart$25,                /* Beginning Assembly #       */~
            startrteid$3,                /* Beginning Route ID         */~
            warnmsg$79,                  /* Part Master Status Message */~
            lowpn$25,                    /* Beginning part number      */~
            lowid$3,                     /* Beginning route ID         */~
            rtekey$31,                   /* Key for RTEMASTR           */~
            altkey$34,                   /* Key for RTEALTRS           */~
            assy$25,                     /* Assembly Number for print  */~
            assydescr$32,                /* Assembly Description       */~
            headline$50,                 /* Header line for route text */~
            wc$4,                        /* Work Center #              */~
            rteid$3,                     /* Route ID for print         */~
            seqno$3,                     /* Step sequence number       */~
            aseq$3,                      /* Step sequence for Alternate*/~
            mq$6,                        /* Move queue days            */~
            su$4,                        /* Setup units                */~
            run$6,                       /* Run units                  */~
            cmp$3,                       /* Completed parts            */~
            yld$3,                       /* % Yield here               */~
            actdescr$30,                 /* Activity Description       */~
            textidh$4,                   /* Header text ID             */~
            textids$4,                   /* Step   text ID             */~
            ccwc1$4,                     /* 1st concurrent WC          */~
            ccwc2$4,                     /* 2nd concurrent WC          */~
            ccwc3$4,                     /* 3rd concurrent WC          */~
            ccwc1n$6,                    /* 1st Normalizer             */~
            ccwc2n$6,                    /* 2nd Normalizer             */~
            ccwc3n$6,                    /* 3rd Normalizer             */~
            actcode$4,                   /* Activity Code              */~
            shft$7,                      /* Shift differential         */~
            opt$1                        /* MQ Option                  */~

        dim f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! RTEMASTR ! Production routing master file           *~
            * #2  ! HNYMASTR ! Inventory Master File                    *~
            * #3  ! RTEALTRS ! Alternate Routing file                   *~
            * #4  ! TXTFILE  ! Text File                                *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "RTEMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  400,                                  ~
                        keypos =    5, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  35          ~

            select #2,  "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #3,  "RTEALTRS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =  34                      ~

            select #4,  "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    1, keylen =  11                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#2, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#3, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#4, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            str(line2$,62) = "RTEPRINT: " & str(cms2v$,,8)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            select printer(134)
            call "SETPRNT" ("RET001", " ", 0%, 0%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, endpart$, endrteid$,       ~
                      epartdesc$, prttxt$, spartdesc$, startpart$,       ~
                      startrteid$, warnmsg$
            close printer
            call "SETPRNT" ("RET001", " ", 0%, 0%)

            for fieldnr% = 1 to 3
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10190
L10130:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10130
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10130
L10190:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

L11060:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       gen_report
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1) - 7
            if cursor%(1) >  8% then fieldnr% = fieldnr% - 4%
            if cursor%(1) > 13% then fieldnr% = fieldnr% - 2%
            if fieldnr% < 1 or fieldnr% >  3 then L11060

L11130:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11130
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11130
            goto L11060

        REM *************************************************************~
            *       R E P O R T   G E N E R A T I O N   S E C T I O N   *~
            *************************************************************

        gen_report
            call "SHOSTAT" ("Printing Production Routings")
            init (hex(00)) rtekey$, altkey$
            if startpart$ = "ALL" then init(hex(ff)) endpart$, endrteid$
            if startpart$ = "ALL" then init(hex(00)) lowpn$, lowid$
            if startrteid$ = "ALL" or startrteid$ = " " then             ~
                                                     init(hex(00)) lowid$
            if endrteid$ = "ALL" or endrteid$ = " " then                 ~
                                                  init(hex(ff)) endrteid$
            str(rtekey$,1,25) = lowpn$
            str(rtekey$,26,3) = lowid$
            pcntr% = 0%

        REM Now get the first route record
L19160:     call "PLOWNEXT" (#1, rtekey$, 0%, f1%(1))
              if f1%(1) = 1% then L19180
                if pcntr% > 0% then L19175
                  call "ASKUSER" (keyhit1%, "A Report was NOT Generated",~
                     "No Records Found Matching the Parameters Entered", ~
                     "Press Enter to continue")
L19175:         goto inputmode
L19180:     gosub'180
            if assy$ > endpart$ then inputmode
            if assy$ = endpart$ and rteid$ > endrteid$ then inputmode
            if seqno$ = "  1" and wc$ = "VEND" and                       ~
               actdescr$ = "UNSPECIFIED" then check_purch_part
            call "DESCRIBE" (#2, assy$, assydescr$, 0%, f1%(2))
            if f1%(2) = 0% then assydescr$ = "**** PART NOT ON FILE ****"
            if prttxt$ = "Y" then gosub header_text
            gosub'160
            gosub'185
            gosub'165
            gosub check_alt

L19310:     call "PLOWNEXT" (#1, rtekey$, 28%, f1%(1))
                if f1%(1) = 0% then end_route
L19330:     get #1, using L35060, wc$, assy$, rteid$, seqno$, mq, su, run,~
                    ccwc1$, nm1, ccwc2$, nm2, ccwc3$, nm3, shift, opt$,  ~
                    yield, step$, cost, actcode$, actdescr$, textidh$,   ~
                    textids$
            gosub'185
            gosub'165
            gosub check_alt
            goto L19310

        end_route
            if lcntr% > 56 then gosub'160
            print skip(2)
            print using L55230
            goto L19160

        check_purch_part

            call "PLOWNEXT" (#1, rtekey$, 28%, f1%(1))
                if f1%(1) = 0 then L19160
            call "DESCRIBE" (#2, assy$, assydescr$, 0%, f1%(2))
            if f1%(2) = 0% then assydescr$ = "**** PART NOT ON FILE ****"
            if prttxt$ = "Y" then gosub header_text
            gosub'160
            gosub'185
            gosub'165
            gosub check_alt
            goto L19330

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20140,         /* Starting Range   */~
                                    L20220,         /* Ending Range     */~
                                    L20310          /* Text print flag  */
                     return

L20140:     REM DEFAULT/ENABLE FOR Begin Range - STARTPART$, STARTRTEID$
                startpart$ = "ALL"
                inpmessage$ = "Enter Assembly Number/Route ID, ALL, or " ~
                            & "a '?' for Listing."
                return

L20220:     REM DEFAULT/ENABLE FOR Ending Range - ENDPART$, ENDRTEID$
                if startpart$ = "ALL" then enabled% = 0%
                inpmessage$ = "Enter Assembly Number/Route ID or a '?' " ~
                            & "for Listing."
                return

L20310:     REM DEFAULT/ENABLE FOR          Text Include
                prttxt$ = "N"
                inpmessage$ = "Enter 'Y' to include Routing Text"
                return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

        REM *************************************************************~
            *      Loads data from first read for a route               *~
            *************************************************************
        deffn'180
            get #1, using L35060, wc$, assy$, rteid$, seqno$, mq, su, run,~
                    ccwc1$, nm1, ccwc2$, nm2, ccwc3$, nm3, shift, opt$,  ~
                    yield, step$, cost, actcode$, actdescr$, textidh$,   ~
                    textids$
            str(rtekey$,1,25) = assy$
            str(rtekey$,26,3) = rteid$
            return

        REM ***** Let's right justify all this stuff
        deffn'185
            call "CONVERT" (mq,    0.0, mq$)
            call "CONVERT" (su,    0.0, su$)
            call "CONVERT" (run,   0.4, run$)
            call "CONVERT" (cost,  0.0, cmp$)
            call "CONVERT" (yield, 0.0, yld$)
            call "CONVERT" (shift, 5.5, shft$)
            ccwc1n$, ccwc2n$, ccwc3n$ = " "
            if ccwc1$ <> " " then call "CONVERT" (nm1, 0.4, ccwc1n$)
            if ccwc2$ <> " " then call "CONVERT" (nm2, 0.4, ccwc2n$)
            if ccwc3$ <> " " then call "CONVERT" (nm3, 0.4, ccwc3n$)
            return

        REM *************************************************************~
            *               Page Heading Print Routine                  *~
            *************************************************************
        deffn'160    /* PAGE HEADER ROUTINE */
            print page
            pcntr% = pcntr% + 1%
            print using L55030, date$, pcntr%
            print
            print using L55060, assy$, rteid$
            print using L55080, assydescr$
            print
            print using L55100
            print using L55130
            print
            lcntr% = 8%
            return

        REM *************************************************************~
            *               Detali Line  Print Routine                  *~
            *************************************************************
        deffn'165    /*  SET & PRINT DETAIL LINE     */
            if lcntr% > 54 then gosub'160

            print using L55170, step$, wc$, mq$, su$, run$, cmp$, yld$,   ~
                  actcode$, actdescr$, opt$, shft$, ccwc1$, ccwc1n$,     ~
                  ccwc2$, ccwc2n$, ccwc3$, ccwc3n$

            lcntr% = lcntr% + 1%
            if prttxt$ <> "Y" then L31180
L31115:     lchk% = lcntr%
            call "TXTPRINT" (#4, f4%, 134%, textids$, "RTE001", 46%,     ~
                             lcntr%, 56%, "N", " ", stat%)
            if stat% = 0% then L31170
            gosub'160
            goto L31115
L31170:     if lcntr% = lchk% then L31180
            print  :  lcntr% = lcntr% + 1%
L31180:     return

        REM *****  PRINT THE ROUTE HEADER TEXT ON THE FIRST PAGE
        header_text
           if textidh$ = hex(20202020) or                                ~
              textidh$ = hex(00000000) or                                ~
              textidh$ = hex(ffffffff) then return
L31550:    lcntr% = 5%
           print page
           print skip(2)
           headline$ = "ASSEMBLY NUMBER  " & assy$ & " RTEID " & rteid$
           print using L55260, headline$
           print skip(2)
           call "TXTPRINT" (#4, f4%, 134%, textidh$, "RTE001",  5%,      ~
                             lcntr%, 56%, "Y", " ", stat%)
           if stat% = 0% then L31670
           goto L31550

L31670:    return

        REM *************************************************************~
            *    CHECK ALTERNATE ROUTE STEPS & PRINT IF REQ'D           *~
            *************************************************************
        check_alt
            init(hex(00)) altkey$
            init(" ") aseq$
            str(altkey$,1,25) = assy$
            str(altkey$,26,3) = rteid$
            str(altkey$,29,3) = seqno$
L32090:     call "PLOWNEXT" (#3, altkey$, 31%, f1%(3))
                if f1%(3) = 0 then L32220
            get #3, using L35330, aseq$, wc$, mq, su, run, ccwc1$, nm1,   ~
                                 ccwc2$, nm2, ccwc3$, nm3, shift, opt$
            if lcntr% > 56 then gosub'160
            if aseq$ = "  1" then print using L55210
            gosub'185
            print using L55310, aseq$, wc$, mq$, su$, run$, opt$, shft$,  ~
               ccwc1$, ccwc1n$, ccwc2$, ccwc2n$, ccwc3$, ccwc3n$

            lcntr% = lcntr% + 1%
            goto L32090

L32220:     if aseq$ = " " then return
            print using L55290
            lcntr% = lcntr% + 1%
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

L35060: FMT                 /* FILE: RTEMASTR                          */~
            CH(04),         /* Work Center                             */~
            CH(25),         /* PART                                    */~
            CH(3),          /* WC ROUTE                                */~
            CH(3),          /* SEQUENCE NUMBER                         */~
            BI(4),          /* MOVE QUEUE TIME IN DAYS                 */~
            BI(2),          /* SET UP TIME IN HOURS                    */~
            PD(14,4),       /* RUN TIME IN HOURS                       */~
            CH(4),          /* Concurrent work center # 1              */~
            PD(7,4),        /* Concurrent work center # 1 normalizer   */~
            CH(4),          /* Concurrent work center # 2              */~
            PD(7,4),        /* Concurrent work center # 2 normalizer   */~
            CH(4),          /* Concurrent work center # 3              */~
            PD(7,4),        /* Concurrent work center # 3 normalizer   */~
            PD(7,6),        /* Work center shift differential          */~
            CH(1),          /* MOVE QUE OPTION                         */~
            BI(1),          /* PROCESS YIELD EXPECTED %                */~
            XX(8),          /* Effective yield (internal)              */~
            CH(4),          /* STEP CODE                               */~
            BI(1),          /* % OF COSTS INCURRED TO HERE             */~
            CH(4),          /* ACTIVITY CODE                           */~
            CH(30),         /* ACTIVITY PERFORMED                      */~
            CH(4),          /* Header Text ID                          */~
            CH(4),          /* Line Text ID                            */~
            CH(16)          /* Filler                                  */

L35330: FMT                 /* FILE: RTEALTRS                          */~
            XX(31),         /* Skip the first part                     */~
            CH(3),          /* Alternate Route sequence                */~
            CH(4),          /* Primary alternate work center           */~
            BI(4),          /* Alternate Move Queue time               */~
            BI(2),          /* Alternate work center set up time       */~
            PD(14,4),       /* Alternate work center run time          */~
            CH(4),          /* Concurrent work center # 1              */~
            PD(7,4),        /* Concurrent work center # 1 normalizer   */~
            CH(4),          /* Concurrent work center # 2              */~
            PD(7,4),        /* Concurrent work center # 2 normalizer   */~
            CH(4),          /* Concurrent work center # 3              */~
            PD(7,4),        /* Concurrent work center # 3 normalizer   */~
            PD(7,6),        /* Alternate work center shift differential*/~
            CH(1)           /* Generic option field                    */


        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40190,         /* Starting Range   */~
                                    L40190,         /* Ending Range     */~
                                    L40190          /* Text print flag  */
                     goto L40241

                  lfac$(fieldnr%) = hex(80) : return   /* UPPER/LOWER  */
L40190:           lfac$(fieldnr%) = hex(81) : return   /* UPPER ONLY   */
                  lfac$(fieldnr%) = hex(82) : return   /* NUMERIC ONLY */

L40241:     if warnmsg$ <> " " then errormsg$ = warnmsg$
L40250:     accept                                                       ~
               at (01,02), "Input Selection Criteria",                   ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Beginning Range of :",                       ~
               at (08,07), "Assembly #",                                 ~
               at (08,20), fac(lfac$( 1)), startpart$           , ch(25),~
               at (08,50), "Route ID",                                   ~
               at (08,60), fac(lfac$( 1)), startrteid$          , ch(03),~
               at (09,17), fac(hex(8c)), spartdesc$             , ch(34),~
               at (11,02), "Ending Range of :",                          ~
               at (13,07), "Assembly #",                                 ~
               at (13,20), fac(lfac$( 2)), endpart$             , ch(25),~
               at (13,50), "Route ID",                                   ~
               at (13,60), fac(lfac$( 2)), endrteid$            , ch(03),~
               at (14,17), fac(hex(8c)), epartdesc$             , ch(34),~
               at (16,02), "Include Route Text?",                        ~
               at (16,30), fac(lfac$( 3)), prttxt$              , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40680
                  call "MANUAL" ("RTEPRINT")
                  goto L40250

L40680:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40250

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41190,         /* Starting Range   */~
                                    L41190,         /* Ending Range     */~
                                    L41190          /* Text Print Flag  */
                     goto L41245

                  lfac$(fieldnr%) = hex(80) : return   /* UPPER/LOWER  */
L41190:           lfac$(fieldnr%) = hex(81) : return   /* UPPER ONLY   */
                  lfac$(fieldnr%) = hex(82) : return   /* NUMERIC ONLY */

L41245:     if warnmsg$ <> " " then errormsg$ = warnmsg$
L41250:     accept                                                       ~
               at (01,02), "Input Selection Criteria",                   ~
               at (01,67), "Date:",                                      ~
               at (01,72), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Beginning Range of :",                       ~
               at (08,07), "Assembly #",                                 ~
               at (08,20), fac(lfac$( 1)), startpart$           , ch(25),~
               at (08,50), "Route ID",                                   ~
               at (08,60), fac(lfac$( 1)), startrteid$          , ch(03),~
               at (09,17), fac(hex(8c)), spartdesc$             , ch(34),~
               at (11,02), "Ending Range of :",                          ~
               at (13,07), "Assembly #",                                 ~
               at (13,20), fac(lfac$( 2)), endpart$             , ch(25),~
               at (13,50), "Route ID",                                   ~
               at (13,60), fac(lfac$( 2)), endrteid$            , ch(03),~
               at (14,17), fac(hex(8c)), epartdesc$             , ch(34),~
               at (16,02), "Include Route Text?",                        ~
               at (16,30), fac(lfac$( 3)), prttxt$              , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Print Report",                           ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41680
                  call "MANUAL" ("RTEPRINT")
                  goto L41250

L41680:        if keyhit% <> 15 then L41720
                  call "PRNTSCRN"
                  goto L41250

L41720:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$, warnmsg$ = " "
                  on fieldnr% gosub L50060,         /* Starting Range   */~
                                    L50215,         /* Ending Range     */~
                                    L50355          /* Text Print Flag  */
                     return
L50060: REM TEST BEGINNING RANGE DATA             STARTPART$, STARTRTEID$
            init(hex(00)) lowpn$, lowid$
            if startpart$ = "ALL" then L50145
                if startrteid$ = "ALL" then L50125
                     if startpart$ = "?" then startpart$ = " "
                     rtekey$ = str(startpart$,1,25) &                    ~
                               str(startrteid$,1,3) & "   "
                     call "PLOWCODE" (#1, rtekey$," ",-28%,-.001,f1%(1))
                     if f1%(1) = 1% then L50115
                          if str(rtekey$,1,25) <> " " then L50100
                               startpart$  = str(rtekey$,1,25)
                               startrteid$ = str(rtekey$,26,3)
                               goto L50165
L50100:                   warnmsg$ = "WARNING: Starting Assembly/Route " ~
                                   & " is NOT on the Route Master File."
                          goto L50125
L50115:              startpart$  = str(rtekey$,1,25)
                     startrteid$ = str(rtekey$,26,3)
L50125:         call "DESCRIBE" (#2, startpart$, spartdesc$, 1%, f1%(2))
                if f1%(2) = 1% then L50160
                     spartdesc$ = "(NOT on Inventory Master)"
                     goto L50160
L50145:     if startrteid$ = " " then startrteid$ = "ALL"
            spartdesc$, endpart$, epartdesc$, endrteid$ = " "
            goto L50206
L50160:     if startrteid$ = " " then startrteid$ = "ALL"
L50165:     if endpart$ = " " then L50195
                if endrteid$ = "ALL" then endrteid$ = startrteid$
                if startrteid$ = "ALL" then endrteid$ = "ALL"
                if startpart$ <= endpart$ then L50206
                     errormsg$ = "Starting Range's Assembly Number is "  ~
                               & "Greater then the Ending Range's."
                     goto L50206
L50195:     endpart$   = startpart$
            epartdesc$ = spartdesc$
            endrteid$  = startrteid$
L50206:     lowpn$ = startpart$ : lowid$ = startrteid$
            return

L50215: REM TEST ENDING RANGE DATA                    ENDPART$, ENDRTEID$
            if startpart$ <> "ALL" then L50235
                endpart$, epartdesc$, endrteid$ = " "
                goto L50340
L50235:     if endpart$ <> " " then L50256
                endpart$   = startpart$
                epartdesc$ = spartdesc$
                endrteid$  = startrteid$
                goto L50340
L50256:     if startrteid$= "ALL" and endrteid$= " " then endrteid$= "ALL"
            if endrteid$ = "ALL" then L50305
                if endpart$ = "?" then endpart$ = " "
                rtekey$ = str(endpart$,1,25) & str(endrteid$,1,3) & "   "
                call "PLOWCODE" (#1, rtekey$, " ", -28%, -.001, f1%(1))
                if f1%(1) = 1% then L50277
                     if str(rtekey$,1,25) <> " " then L50280
L50277:                   endpart$  = str(rtekey$,1,25)
                          endrteid$ = str(rtekey$,26,3)
                          goto L50320
L50280:              warnmsg$ = "WARNING: Ending Assembly/Route is NOT " ~
                              & "on the Route Master File."
                     goto L50305
                endpart$  = str(rtekey$,1,25)
                endrteid$ = str(rtekey$,26,3)
L50305:     call "DESCRIBE" (#2, endpart$, epartdesc$, 1%, f1%(2))
            if f1%(2) = 1% then L50320
                epartdesc$ = "(NOT on Inventory Master)"
L50320:     if endpart$ >= startpart$ then L50340
                errormsg$ = "Ending Range's Assembly Number CANNOT be "  ~
                          & "less then the Starting Range's."
            if startrteid$ = "ALL" then endrteid$ = "ALL"
L50340:     return


L50355:     REM TEST FOR TEXT PRINT FLAG          PRTTXT$
                if prttxt$ = "Y" or prttxt$ = "N" then return
                errormsg$ = " MUST be 'Y' or 'N' ONLY."
                return

        REM *************************************************************~
            *        AND HERE COME THE IMAGE STATEMENTS                 *~
            *************************************************************
L55030: %DATE: ########                                  P R O D U C T I ~
        ~O N   R O U T I N G                                     PAGE ###

L55060: %ASSEMBLY NUMBER: ######################### ROUTE ID: ###

L55080: %    DESCRIPTION: ################################

L55100: %STEP WORK     MQ   SET UP    RUN    %    %   --- ACTIVITY ------~
        ~----------------  MQ  SHFT

L55130: %NBR  CTR     DAYS   UNITS   UNITS  COMP YLD  CODE DESCRIPTION   ~
        ~                 OPT  DIFF    ------ CONCURRENT WORK CENTERS ----~
        ~--

L55170: %#### ####  ######  ######  ######  ###  ###  #### ##############~
        ~################  # #######   #### ######  #### ######  #### ####~
        ~##

L55210: %  ***** ALTERNATES *****

L55230: %                                        ********** E N D   O F  ~
        ~ R O U T I N G **********

L55260: %ROUTING TEXT FOR ###############################################~
        ~###

L55290: %   **********************

L55310: %  ### #### ######  ######  ######                               ~
        ~                  # #######   #### ######  #### ######  #### ####~
        ~##

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
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please")
            end
