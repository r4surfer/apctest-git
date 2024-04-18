        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    OOO   M   M  RRRR   EEEEE  PPPP   L      RRRR    *~
            *  B   B  O   O  MM MM  R   R  E      P   P  L      R   R   *~
            *  BBBB   O   O  M M M  RRRR   EEEE   PPPP   L      RRRR    *~
            *  B   B  O   O  M   M  R   R  E      P      L      R   R   *~
            *  BBBB    OOO   M   M  R   R  EEEEE  P      LLLLL  R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMREPLR - THIS PROGRAM ALLOWS THE USER TO REPLACE ONE    *~
            *            PART CODE WITH ANOTHER IN ENGINEERING BOM'S.   *~
            *            THEY CAN SELECT REPLACE WHEN USED AS A PARENT  *~
            *            OR WHEN USED AS A COMPONENT OR BOTH.  THE BOM  *~
            *            MARKERS CAN BE RETAINED OR CHANGED ALSO.       *~
            *            ALSO REPLACES REFERENCE INFO WHERE IT APPLIES. *~
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
            * 09/21/82 ! ORIGINAL                                 ! GLW *~
            * 07/28/83 ! ADD REFERENCE REPLACEMENT                ! HES *~
            * 11/27/85 ! Corrected eff. check & allowed assy repl.! MJB *~
            * 09/18/86 ! BOMMASTR FORMAT CHANGED                  ! LKM *~
            * 05/13/87 ! HNYMASTR record length mod for Std Cost  ! JIM *~
            * 06/11/87 ! Disallow BOM updates if in Std Cost Set  ! JIM *~
            * 07/20/95 ! Loop thru all BOM Markers at validation. ! RJH *~
            *          !   Plus some screen erganomics.           !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**
        dim                                                              ~
            bomid$3,                     /* BOM STRUCTURE ID           */~
            bommkr$2,                    /* THE BOM MARKER             */~
            comp$1,                      /* CHANGE IF COMPONENT FLAG   */~
            change$1,                    /* KEEP THE SAME MARKERS FLAG */~
            cflag$5,                     /* COMPONENT CHANGES          */~
            fflag$5,                     /* COMPONENT HITS             */~
            component$25,                /* USED IN SUB 7              */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            effective$(490)3,            /* BOM EFFECTIVE DATES        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            freetext$76,                 /* BOM FREE TEXT              */~
            from$(4)2,                   /* CHANGE MARKER FROM         */~
            i$(24)80,                    /* FOR SCREEN PROCESSING      */~
            inpmessage$79,               /* INPUT SCREEN MESSAGE       */~
            keyarea$56,                                                  ~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            mkrmsg$(20)3,                                                ~
            np$25,                       /* USED IN SUB 7              */~
            newpart$25,                  /* CHANGE TO PART #           */~
            newpartdescr$32,             /* CHANGE TO PART DESCR       */~
            m$79,                        /* COMPLETION MESSAGE         */~
            mm$79,                       /* NUMBER OF HITS MESSAGE     */~
            marker$(20)2,                /* VALID BOM MARKERS          */~
            mkrkey$10,                                                   ~
            op$25,                       /* USED IN SUB 7              */~
            oldparn$25,                  /* THE OLD PARENT PART #      */~
            oldpart$25,                  /* CHANGE FROM PART #         */~
            oldpart1$25,                 /* USED IN SUB 7              */~
            oldpartdescr$32,             /* CHANGE FROM PART DESCR     */~
            p$(100)31,                   /* USED IN SUB 7              */~
            part$25,                     /* USED IN SUB 7              */~
            plowkey$50,                  /* PLOWNEXT READKEY FOR BOM   */~
            readkey$31,                  /* PLOWNEXT READKEY FOR BOM   */~
            readkey1$56,                 /* PLOWALTS READKEY FOR BOM   */~
            redalt1key$56,               /* REDALT1 READKEY FOR BOM    */~
            redalt1ref$59,               /* REDALT1 READKEY FOR REFEREN*/~
            reference$100,               /* STORAGE FOR REFERENCE RECRD*/~
            ref_key$60,                  /* READKEY FOR REFERENCE FILE */~
            seq$3,                       /* SEQ NUMBER IN BOM          */~
            to$(4)2                      /* CHANGE MARKER TO           */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "OPENFILE"*/~
            axd$(64)4                    /* AXD POINTER FROM "OPENFILE"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
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
            * # 3 ! STCBOMXF ! Standard Cost Set / BOM-RTE X-Ref        *~
            * # 4 ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * # 5 ! BOMMASTR ! BILL OF MATERIALS RELATIONSHIP FILE      *~
            * # 6 ! BOMREFER ! REFERENCE FILE                           *~
            * # 9 ! JBCROSS2 !                                          *~
            * #10 ! ENGMASTR ! ENGINEERING MASTER FILE                  *~
            *************************************************************

            select  #3, "STCBOMXF",                                      ~
                        varc, indexed, recsize = 72,                     ~
                        keypos = 29, keylen = 33,                        ~
                        alt key 1, keypos =  1, keylen = 36,             ~
                            key 2, keypos = 37, keylen = 36

            select  #4, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90,  keylen = 9, dup

            select # 5, "BOMMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos =  26, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 56,            ~
                         ioerr goto L16570

            select # 6, "BOMREFER",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 100,                                  ~
                         keypos =  26, keylen = 34,                      ~
                         alt key  1, keypos = 1, keylen = 59

            select # 9,"JBCROSS2",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize =  94,                                    ~
                       keypos =29, keylen = 19,                          ~
                       alternate key 1, keypos = 1 , keylen = 47,        ~
                                 key 2, keypos = 48, keylen = 47

            select #10, "ENGMASTR" ,                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos = 1, keylen = 29

            select #11, "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos  =   1, keylen =  20

            call "SHOSTAT" ("Opening files, one moment please")

            call "OPENFILE" (# 3, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            call "OPENFILE" (# 4, "SHARE", f2%( 4), rslt$( 4), axd$( 4))
            call "OPENFILE" (# 5, "SHARE", f2%( 5), rslt$( 5), axd$( 5))
            call "OPENFILE" (# 9, "SHARE", f2%( 9), rslt$( 9), axd$( 9))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#11, "SHARE", f2%(11), rslt$(11), axd$(11))

            if f2%(5) = 0 then L02890
               call "OPENFILE" (#5, "OUTPT", f2%( 5), rslt$( 5), axd$( 5))
               close #5
               call "OPENFILE" (#5, "SHARE", f2%( 5), rslt$( 5), axd$( 5))

L02890:     call "OPENFILE" (#6, "SHARE", f2%( 6), rslt$( 6), axd$( 6))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            init(" ") mkrmsg$()
            mkrctr% = 0%
            mkrkey$ = "TABLE01:" & hex(00)
            for i% = 1% to 20%
            call "PLOWNEXT" (#11%,mkrkey$, 8%, f1%(11))
            if f1%(11) = 0% then L03196
            get #11, using L03170, marker$(i%)
L03170:     FMT POS(9), CH(2)
            mkrctr% = mkrctr% + 1%
            next i%
L03196:     comp$ = "Y"
            if mkrctr% < 1 then L03230
            for i% = 1% to mkrctr%
            str(mkrmsg$(i%),1,2) = marker$(i%)
            if i% = 20% then L03206
            str(mkrmsg$(i%),3,1) = ","
L03206:     next i%

L03230:     edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, oldpart$, newpart$, fflag$,~
                           change$, from$(), to$(), readkey$, readkey1$, ~
                     oldpartdescr$, newpartdescr$, cflag$, freetext$

           l% = 0%
           flag, fflag, cflag = 0

            for fieldnr% = 1 to 9
                gosub'051(fieldnr%)
                      if enabled% = 0 then L03810
L03690:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L52000
                      if keyhit% <>  0 then       L03690
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L03690
L03810:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************

                     init(" ") m$, mm$

L04010:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L04010
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  9 then L04010

L04150:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L04150
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L04150
            goto L04010

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub L06750
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L04890 ,         /* OLD PART         */~
                                    L04950 ,         /*    DESCRIPTION   */~
                                    L05030 ,         /* NEW PART         */~
                                    L05090 ,         /*    DESCRIPTION   */~
                                    L05330 ,         /* KEEP MARKERS?    */~
                                    L05410 ,         /* CHANGE FROM/TO   */~
                                    L05530 ,         /* CHANGE FROM/TO   */~
                                    L05650 ,         /* CHANGE FROM/TO   */~
                                    L05770           /* CHANGE FROM/TO   */
                     return
L04890:     REM DEFAULT/ENABLE FOR EXISTING PART CODE
                     enabled% = 1
                     return
L04950:     REM DEFAULT/ENABLE FOR    DESCRIPTION
                     enabled% = 0
            call "DESCRIBE" (#4, oldpart$, oldpartdescr$, 0%, f1%(4))
                     return
L05030:     REM DEFAULT/ENABLE FOR REPLACE WITH PART CODE
                     enabled% = 1
                     return
L05090:     REM DEFAULT/ENABLE FOR    DESCRIPTION
                     enabled% = 0
            call "DESCRIBE" (#4, newpart$, newpartdescr$, 0%, f1%(4))
                     return
L05330:     REM DEFAULT/ENABLE FOR KEEP THE SAME BOM MARKERS?
                     if oldpart$ = newpart$ then enabled% = 0%           ~
                                            else enabled% = 1%
                     if oldpart$ = newpart$ then change$ = "N"           ~
                                            else change$ = "Y"
                     return
L05410:     REM DEFAULT/ENABLE FOR CHANGE FROM/TO
                     if change$ = "Y" then return
                     enabled% = 1
                     from$(1) = "  "
                     to$(1) = "  "
                     return
L05530:     REM DEFAULT/ENABLE FOR CHANGE FROM/TO
                     if from$(1) = " " then to$(1) = " "
                     if from$(1) =  " " then return
                     from$(2) = "  "
                     to$(2) = "  "
                     enabled% = 1
                     return
L05650:     REM DEFAULT/ENABLE FOR CHANGE FROM/TO
                     if from$(2) = " " then to$(2) = " "
                     if from$(2) =  " " then return
                     from$(3) = "  "
                     to$(3) = "  "
                     enabled% = 1
                     return
L05770:     REM DEFAULT/ENABLE FOR CHANGE FROM/TO
                     if from$(3) = " " then to$(3) = " "
                     if from$(3) =  " " then return
                     from$(4) = "  "
                     to$(4) = "  "
                     enabled% = 1
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
            init(" ") m$, mm$

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

L06750: REM *************************************************************~
            *       M A K E   B O M   C H A N G E S                     *~
            * CHANGE WHEN PART IS A COMPONENT ONLY                      *~
            *                    FIRST MAKE SURE THAT NEITHER PART IS   *~
            * A COMPONENT OF THE OTHER.  THEN MAKE THE CHANGES REQUESTED*~
            *************************************************************

        REM SEE IF EITHER IS A COMPONENT OF THE OTHER *****************
                     np$ = newpart$
                     op$ = oldpart$
                     flag = 0
            gosub'7(np$, op$, flag)
            if flag = 0 then goto L07170
            m$ = str(oldpart$,1,len(oldpart$)) & " IS USED IN " &        ~
                     str(newpart$,1,len(newpart$)) & " RESPECIFY"
                     return
L07170:              np$ = newpart$
                     op$ = oldpart$
                     flag = 0
            gosub'7(op$, np$, flag)
            if flag = 0 then goto L08910
            m$ = str(newpart$,1,len(newpart$)) & " IS USED IN " &        ~
                     str(oldpart$,1,len(oldpart$)) & " RESPECIFY"
                     return

            deffn'7(part$, oldpart1$, flag)
                  l% = l% + 1
                  p$(l%) = str(part$,1,25) & " "

L07470:           call "PLOWNEXT" (#5, p$(l%), 25%, f1%(5))
                       if f1%(5) = 0 then L07750
            get    #5, using L07550 ,  keyarea$

L07550:     FMT   CH(56)                 /* COMPONENT PART NUMBER      */
                  if str(keyarea$,54,3) = "  0" then L07470
                  component$ = str(keyarea$,1,25)

                  if oldpart1$ <> component$ then goto L07670
                  flag = 1
           return

L07670:           if l% < 100% then gosub'7(component$, oldpart1$, flag)
                                         /* DO COMPS IF NOT AT BOTTOM  */
                  goto L07470

L07750:           REM END ROUTINE GRACEFULLY.
                      l% = l% - 1
                      return

L07820:     FMT CH(25),                  /* COMPONENT PART NUMBER      */~
                CH(25),                  /* ASSEMBLY PART NUMBER       */~
                CH(3),                   /* BOM STRUCTURE ID           */~
                CH(3),                   /* SEQUENCE NO.               */~
                XX(32),                  /*                            */~
                CH(2)                    /* BOM MARKER                 */

L07910:     FMT CH(100)                  /* REFERENCE RECORD           */~

L08910: REM NOW DO COMPONENT CHANGE IF REQUIRED *************************

           readkey1$ = str(oldpart$,1,25) & " "  /* READKEY1 = 53 CHAR */
           ref_key$  = str(oldpart$,1,25) & " "  /* REF_KEY  = 56 CHAR */

L09050:     call "PLOWALTS" (#5, ref_key$, 1%, 25%, f1%(5))
                 if f1%(5) = 0 then L09470

            get #5, using L07820, oldpart$, oldparn$, bomid$, seq$, bommkr$
            if seq$ = "  0" then L09050
            fflag = fflag + 1

         /* CHECK FOR RESTRICTIONS ON BILL MODIFICATION */
            part$ = oldparn$
            gosub check_usage
            if used% = 0 then L09170
                str(readkey1$,54,3) = hex(ffffff)
                goto L09050

L09170:   cflag = cflag + 1
          redalt1key$ = str(oldpart$,1,25) & str(oldparn$,1,25)          ~
                      & str(bomid$,1,3)    & str(seq$,1,3)
                     if change$ = "Y" then goto L09350
                     for i = 1 to 4
                     if bommkr$ = from$(i) then goto   L09330
                     next i
                     goto  L09350
L09330:              bommkr$ = to$(i)
L09350:     call "REDALT1" (#5, redalt1key$, 1%, f1%(5))
            put #5, using L09390, newpart$, bommkr$
L09390:         FMT POS(1), CH(25), POS(89), CH(02)
            rewrite #5
            goto L09050

L09470: REM         ****** TAKE CARE OF REFERENCE FILE ******

L09510:     call "PLOWALTS" (#6, ref_key$, 1%, 25%, f1%(6))
                    if f1%(6) = 0 then L09730
               get    #6, using L07910 , reference$

               redalt1ref$ = reference$
               call "REDALT1" (#6, redalt1ref$, 1%, f1%(6))
               str(reference$,,25) = newpart$
               delete #6
               write  #6, using L07910 , reference$
            goto L09510

L09730: REM THE CHANGES HAVE BEEN MADE, SO NOTIFY USER AND START AGAIN **
           if comp$ <> "Y" then return
           convert cflag to cflag$, pic(#####)
           convert fflag to fflag$, pic(#####)
           m$ = "Part Number " & oldpart$ & " was used " & fflag$ & " tim~
        ~es, "
           mm$ = "     And was replaced by Part Number " & newpart$ &    ~
                  ", " &  cflag$ & " times."

           return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L10430,         /* OLD PART         */~
                                    L10370,         /*    DESCRIPTION   */~
                                    L10430,         /* NEW PART         */~
                                    L10370,         /*    DESCRIPTION   */~
                                    L10430,         /* KEEP MARKERS?    */~
                                    L10430,         /* CHANGE FROM/TO   */~
                                    L10430,         /* CHANGE FROM/TO   */~
                                    L10430,         /* CHANGE FROM/TO   */~
                                    L10430          /* CHANGE FROM/TO   */
                     goto L10570

L10370:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L10430:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L10570:     accept                                                       ~
               at (01,02),                                               ~
                  "BOM FILE SEARCH AND REPLACE: INPUT THE CHANGE DESIRED ~
        ~AND PRESS (ENTER)",                                              ~
               at (02,02), fac(hex(84)),                                 ~
                  m$                                            , ch(62),~
               at (03,02), fac(hex(84)), mm$                    , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "EXISTING PART CODE",                                  ~
               at (06,30), fac(lfac$( 1)), oldpart$             , ch(25),~
               at (07,02),                                               ~
                  "   DESCRIPTION",                                      ~
               at (07,30), fac(lfac$( 2)), oldpartdescr$        , ch(34),~
               at (08,02),                                               ~
                  "REPLACE WITH PART CODE",                              ~
               at (08,30), fac(lfac$( 3)), newpart$             , ch(25),~
               at (09,02),                                               ~
                  "   DESCRIPTION",                                      ~
               at (09,30), fac(lfac$( 4)), newpartdescr$        , ch(34),~
               at (10,02),                                               ~
                  "KEEP THE SAME BOM MARKERS?",                          ~
               at (10,30), fac(lfac$( 5)), change$              , ch(01),~
               at (11,02),                                               ~
                  "CHANGE MARKER FROM",                                  ~
               at (11,21), fac(lfac$( 6)), from$(1)             , ch(02),~
               at (11,25), "TO",                                         ~
               at (11,30), fac(lfac$( 6)), to$(1)               , ch(02),~
               at (12,02),                                               ~
                  "CHANGE MARKER FROM",                                  ~
               at (12,21), fac(lfac$( 7)), from$(2)             , ch(02),~
               at (12,25), "TO",                                         ~
               at (12,30), fac(lfac$( 7)), to$(2)               , ch(02),~
               at (13,02),                                               ~
                  "CHANGE MARKER FROM",                                  ~
               at (13,21), fac(lfac$( 8)), from$(3)             , ch(02),~
               at (13,25), "TO",                                         ~
               at (13,30), fac(lfac$( 8)), to$(3)               , ch(02),~
               at (14,02),                                               ~
                  "CHANGE MARKER FROM",                                  ~
               at (14,21), fac(lfac$( 9)), from$(4)             , ch(02),~
               at (14,25), "TO",                                         ~
               at (14,30), fac(lfac$( 9)), to$(4)               , ch(02),~
               at (18,02), "This search and replace function is very powe~
        ~rful and should be used with care.",                             ~
               at (19,02), "It will make the changes shown unless the EXI~
        ~STING and REPLACE parts are used",                               ~
               at (20,02), "in each other.  To change only the marker, en~
        ~ter the same part in both places,",                              ~
               at (21,02), "answer KEEP THE SAME BOM MARKERS? 'N', and en~
        ~ter the marker changes desired",                                 ~
                                                                         ~
                                                                         ~
               at (22,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L12090
                  call "MANUAL" ("BOMREPLR")
                  goto L10570

L12090:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L10570

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L12650,         /* OLD PART         */~
                                    L12590,         /*    DESCRIPTION   */~
                                    L12650,         /* NEW PART         */~
                                    L12590,         /*    DESCRIPTION   */~
                                    L12650,         /* KEEP MARKERS?    */~
                                    L12650,         /* CHANGE FROM/TO   */~
                                    L12650,         /* CHANGE FROM/TO   */~
                                    L12650,         /* CHANGE FROM/TO   */~
                                    L12650          /* CHANGE FROM/TO   */
                     goto L12790

L12590:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L12650:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L12790:     accept                                                       ~
               at (01,02),                                               ~
                  "BOM FILE SEARCH AND REPLACE: EDIT THE ENTRIES AS DESIR~
        ~ED",                                                             ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,75),                                               ~
                  "PAGE 1",                                              ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "EXISTING PART CODE",                                  ~
               at (06,30), fac(lfac$( 1)), oldpart$             , ch(25),~
               at (07,02),                                               ~
                  "   DESCRIPTION",                                      ~
               at (07,30), fac(lfac$( 2)), oldpartdescr$        , ch(34),~
               at (08,02),                                               ~
                  "REPLACE WITH PART CODE",                              ~
               at (08,30), fac(lfac$( 3)), newpart$             , ch(25),~
               at (09,02),                                               ~
                  "   DESCRIPTION",                                      ~
               at (09,30), fac(lfac$( 4)), newpartdescr$        , ch(34),~
               at (10,02),                                               ~
                  "KEEP THE SAME BOM MARKERS?",                          ~
               at (10,30), fac(lfac$( 5)), change$              , ch(01),~
               at (11,02),                                               ~
                  "CHANGE MARKER FROM",                                  ~
               at (11,21), fac(lfac$( 6)), from$(1)             , ch(02),~
               at (11,25), "TO",                                         ~
               at (11,30), fac(lfac$( 6)), to$(1)               , ch(02),~
               at (12,02),                                               ~
                  "CHANGE MARKER FROM",                                  ~
               at (12,21), fac(lfac$( 7)), from$(2)             , ch(02),~
               at (12,25), "TO",                                         ~
               at (12,30), fac(lfac$( 7)), to$(2)               , ch(02),~
               at (13,02),                                               ~
                  "CHANGE MARKER FROM",                                  ~
               at (13,21), fac(lfac$( 8)), from$(3)             , ch(02),~
               at (13,25), "TO",                                         ~
               at (13,30), fac(lfac$( 8)), to$(3)               , ch(02),~
               at (14,02),                                               ~
                  "CHANGE MARKER FROM",                                  ~
               at (14,21), fac(lfac$( 9)), from$(4)             , ch(02),~
               at (14,25), "TO",                                         ~
               at (14,30), fac(lfac$( 9)), to$(4)               , ch(02),~
               at (18,02), "WHEN YOU PRESS PF-16 YOUR BOM FILE WILL BE CH~
        ~ANGED AS DIRECTED ABOVE",                                        ~
               at (19,02), "MAKE SURE THE DIRECTIONS ABOVE ARE CORRECT BE~
        ~FORE YOU PROCEED !",                                             ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,48),                                               ~
                  "(16) MAKE THE CHANGES SHOWN ABOVE",                   ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L14270
                  call "MANUAL" ("BOMREPLR")
                  goto L12790

L14270:        if keyhit% <> 15 then L14350
                  call "PRNTSCRN"
                  goto L12790

L14350:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L14830,         /* OLD PART         */~
                                    L14990,         /*    DESCRIPTION   */~
                                    L15030,         /* NEW PART         */~
                                    L15290,         /*    DESCRIPTION   */~
                                    L15490,         /* KEEP MARKERS?    */~
                                    L15570,         /* CHANGE FROM/TO   */~
                                    L15810,         /* CHANGE FROM/TO   */~
                                    L16050,         /* CHANGE FROM/TO   */~
                                    L16290          /* CHANGE FROM/TO   */
                     return
L14830:     REM TEST DATA FOR EXISTING PART CODE
                if oldpart$  <> " " then L14910
                   errormsg$ = "EXISTING Part Number may not be blank"
                   return
L14910:         call "GETCODE" (#4, oldpart$, oldpartdescr$, 0%,0, f1%(4))
                     if f1%(4) <> 0 then return
                errormsg$ = "Part Not On File: " & oldpart$
                return
L14990:     REM TEST DATA FOR    DESCRIPTION
                     return
L15030:     REM TEST DATA FOR REPLACE WITH PART CODE
                if newpart$  <> " " then L15110
                   newpart$  = oldpart$
L15110:         call "GETCODE" (#4, newpart$, newpartdescr$, 0%,0, f1%(4))
                     if f1%(4) <> 0 then return
                errormsg$ = "Part Not On File: " & newpart$
                return
L15290:     REM TEST DATA FOR    DESCRIPTION
                     return
L15490:     REM TEST DATA FOR KEEP THE SAME BOM MARKERS?
                if oldpart$ = newpart$ then change$ = "N"
                if  change$ = "Y" or change$ = "N" then return
                errormsg$ = "KEEP SAME BOM MARKER MUST BE 'Y' OR 'N' "
                     return
L15570:     REM TEST DATA FOR CHANGE FROM/TO
                     if from$(1) = " " then return
                     for i = 1 to mkrctr%
                     if from$(1) = marker$(i) then goto L15710
                     next i
                errormsg$ = "CHANGE FROM MUST BE " & mkrmsg$()
                     return
L15710:              for i = 1 to mkrctr%
                     if   to$(1) = marker$(i) then goto L15790
                     next i
                errormsg$ = "CHANGE TO MUST BE " & mkrmsg$()
L15790:              return
L15810:     REM TEST DATA FOR CHANGE FROM/TO
                     if from$(2) = " " then return
                     for i = 1 to mkrctr%
                     if from$(2) = marker$(i) then goto L15950
                     next i
                errormsg$ = "CHANGE FROM MUST BE " & mkrmsg$()
                     return
L15950:              for i = 1 to mkrctr%
                     if   to$(2) = marker$(i) then goto L16030
                     next i
                errormsg$ = "CHANGE TO MUST BE " & mkrmsg$()
L16030:              return
L16050:     REM TEST DATA FOR CHANGE FROM/TO
                     if from$(3) = " " then return
                     for i = 1 to mkrctr%
                     if from$(3) = marker$(i) then goto L16190
                     next i
                errormsg$ = "CHANGE FROM MUST BE " & mkrmsg$()
                     return
L16190:              for i = 1 to mkrctr%
                     if   to$(3) = marker$(i) then goto L16270
                     next i
                errormsg$ = "CHANGE TO MUST BE " & mkrmsg$()
L16270:              return
L16290:     REM TEST DATA FOR CHANGE FROM/TO
                     if from$(4) = " " then return
                     for i = 1 to mkrctr%
                     if from$(4) = marker$(i) then goto L16430
                     next i
                errormsg$ = "CHANGE FROM MUST BE " & mkrmsg$()
                     return
L16430:              for i = 1 to mkrctr%
                     if   to$(4) = marker$(i) then goto L16510
                     next i
                errormsg$ = "CHANGE TO MUST BE " & mkrmsg$()
L16510:              return


L16570: REM *************************************************************~
            *        I O   E R R O R   P R O C E S S I N G              *~
            *                                                           *~
            * IO ERROR CAUSED PROGRAM TO BRANCH HERE...                 *~
            * PERHAPS  A PART WAS BEING CHANGE TO ANOTHER NAME THAT     *~
            * ALREADY EXISTED ENDING UP WITH SAME KEY.                  *~
            *************************************************************

            errormsg$ = "A COMPONENT ALREADY EXISTS WITH THIS PARENT- BOM~
        ~, INTEGRITY OF OLD BOM IN DOUBT"
            return

        REM *************************************************************~
            *                   C H E C K____U S A G E                  *~
            *                                                           *~
            *                IS IT OK TO CHANGE THIS BILL?              *~
            *************************************************************

        check_usage
            plowkey$ = str(part$,1,25) &  str(bomid$,1,3)
            call "PLOWALTS" (#9, plowkey$, 2%,28%, used%) /*IN JBCROSS2?*/
            if used% <> 0 then return
            plowkey$ = str(part$,1,25) & "1001"
            call "READ100" (#10,str(plowkey$,,29),f1%(10))/*IN ENGMASTR?*/
            if f1%(10) = 0 then goto L33230
            get #10, using L33190, effective$()
L33190:     FMT XX(29), 490*CH(3)
            search effective$() = str(bomid$,,3) to cursor%() step 3
            if cursor%(1) <> 0 then used% = 1
            if used% <> 0 then return
L33230
*        And last see if Bill referenced by Standard Costing (STCBOMXF)
            plowkey$ = str(part$,,25) & str(bomid$,,3) & hex(00)
            call "PLOWALTS" (#3, plowkey$, 1%, 28%, used%)
            if used% = 0% then return
            errormsg$ = "Used by Standard Cost Set " & str(plowkey$,29,8)
            return

L52000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
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

            call "SHOSTAT" ("One moment please")
            end
