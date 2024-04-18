        REM *************************************************************~
            *                                                           *~
            *   GGG   N   N  JJJJJ  IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  G      NN  N    J      I    NN  N  P   P  U   U    T     *~
            *  G GGG  N N N    J      I    N N N  PPPP   U   U    T     *~
            *  G   G  N  NN  J J      I    N  NN  P      U   U    T     *~
            *   GGG   N   N   J     IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GNJINPUT - INPUTS GENERAL JOURNAL ENTRIES.                *~
            *-----------------------------------------------------------*~
            *                 M O D I F I C A T I O N S                 *~
            *---WHEN---+-----------------WHAT---------------------+-WHO-*~
            * 05/31/80 ! ORIGINAL (FROM CRCINPUT)                 ! BCW *~
            * 11/23/82 ! CORRECTED *BUG* IN INSERTMODE (15230)    ! BEV *~
            * 11/10/83 ! NOW ALLOWS INPUT TO *ANY* JOURNAL, CAN   ! HES *~
            *          ! SET JOURNALS POSTABLES BASED ON USERID   !     *~
            * 01/21/85 ! CORRECTED *BUG* IN DELETE LINE ITEM      ! RAC *~
            *          ! ADDED ERRORMSG IF LINE ITEM > 800        !     *~
            *          ! & ADDED PF 4 IN INSERT MODE              !     *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION - ! RAC *~
            *          ! CHANGES INCLUDED GLPOST, HNYPOST, JNLINFO!     *~
            *          !(NEW),GLPRTSUB(NEW), INCREASE IN GL BUFFER!     *~
            *          ! FILE RECORD SIZE FOR PASSING MODULE,     !     *~
            *          ! JOURNAL, AND POSTING SEQUENCE            !     *~
            * 07/28/85 ! COMBINED GLDIRECT & THIS INTO ONE PGM    ! HES *~
            * 08/08/85 ! Modified Ref1 and 2 entry; GLDETAIL      ! ERN *~
            *          !   layout changes and general cleanup.    !     *~
            * 04/13/87 ! Corrected array overflow on screen       ! MJB *~
            * 07/27/87 ! Bugs fixed regarding handling of blank   ! ERN *~
            *          !   accts (passed from other programs).    !     *~
            * 09/22/87 ! Minor modifications & modernizations.    ! JIM *~
            * 10/14/88 ! Bug fixed, show right period # if posting! JDH *~
            *          !   to the last period in fiscal year.     !     *~
            * 11/21/88 ! Bring Unposted STJ Journals to light.    ! JDH *~
            * 06/07/89 ! Merge of CMS2 & CMS-I                    ! MJB *~
            *          !  Includes GLFMT before GETCODE in DATA   !     *~
            *          !   LOAD Section (KAB)                     !     *~
            *          !   And No More out of Balance for Honchos !     *~
            *          !   (KAB)                                  !     *~
            * 10/05/89 ! Hid PF16 prompt.  Bah.                   ! JDH *~
            * 12/14/90 ! Changed Call to JNLINFO for GL Batches   ! RAC *~
            * 06/26/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim acct$(804)16,                /* ACCOUNT NUMBERS FOR ENTRY  */~
            blankdate$8,                 /* Blank date for comparison  */~
            blankline$79,                /* FOR UNDERLINING            */~
            credit$(804)10,              /* CREDIT AMOUNTS THIS ENTRY  */~
            cursor%(2),                  /* CURSOR LOCATION            */~
            debit$(804)10,               /* DEBIT AMOUNTS THIS ENTRY   */~
            descr$(804,2)32,             /* ACCOUNT DESCRIPTIONS       */~
            description$32,              /* DESCRIPTION OF ENTRY       */~
            edtcoltran$(2)80,            /* AND ALSO WHICH COLUMN(FIELD*/~
            edtmessage$79,               /* "TO MODIFY DISPLAYED.."TEXT*/~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            fac$(20,6)1,                 /* FAC'S FOR TABULAR INPUT    */~
            fc2$(10)1,                   /* FAC'S FOR TABULAR INPUT    */~
            gldate$8,                    /* THIS USER'S GL MODULE DATE */~
            hdr$79,                      /* LINE ITEM SCREEN HEADER    */~
            header$100,                  /* HEADER THIS ENTRY - 1 FIELD*/~
            i$(24)80,                    /* JUNK VARIABLE--HOLDS SCREEN*/~
            infomsg$79,                  /* INFORMATIVE MESSAGE        */~
            jnlid$3,                     /* JOURNAL ID                 */~
            line2$79,                    /* Second Screen Line         */~
            linefac$(10)1,               /* FAC'S FOR LINEAR MODE      */~
            lastentry$10,                /* LAST ENTRY TO PRINT        */~
            module_name$(10)35,          /* POSTABLE JOURNAL LIST      */~
            mod_descr$25,                /* DESCRIPTION                */~
            module$2,                    /* MODULE # FROM GNJBUFFR     */~
            name$10,                     /* JOURNAL ENTRY NAME         */~
            nfac$(3)1,                   /* FAC'S FOR PRINT SCRN       */~
            oldjnlid$3,                  /* ORIGINAL JOURNAL ID        */~
            pfkeys$(4)17,                /* TABULAR FUNCTION KEY LISTS */~
            pf16$30,                     /* PF16 PROMPT                */~
            pfkey$17,                    /* PF KEYS                    */~
            postdate$8,                  /* DATE TO POST TRANS TO      */~
            postdescr$32,                /* DATE TO POST TRANS TO      */~
            readkey$50,                  /* KEY FOR PLOW ROUTINE       */~
            refinpopt$1,                 /* ENTER REFERENCE FIELDS?    */~
            ref1dflt$30, ref2dflt$34,    /* REF 1, 2 DEFAULT VALUES    */~
            ref1$(804)30,                /* REFERENCE 1                */~
            ref2$(804)34,                /* REFERENCE 2                */~
            ruserid$3,                   /* USERID OF INPUT RECORD     */~
            scrn_head$50,                /* Screen Header              */~
            tdate$8,                     /* Temp. Date                 */~
            title$(4,2)64,               /* TABULAR PF KEY TITLES      */~
            u$(17)8,                     /* DATE ARRAY                 */~
            userid$3                     /* USERID OF CURRENT USER     */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "OPENFILE"*/~
            axd$(64)4                    /* AXD POINTER FROM "OPENFILE"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * #01 ! USERINFO ! SYSTEM USER INFORMATION...MODULE DATES...*~
            * #02 ! GLMAIN   ! GENERAL LEDGER MAIN FILE (ACCT DESCR'S)  *~
            * #03 ! SYSFILE2 ! SYSTEM CONTROL FILE                      *~
            * #09 ! GNJBUFFR ! GENERAL JOURNAL HEADER BUFFER AREA       *~
            * #10 ! GNJBUF2  ! GENERAL JOURNAL DETAIL BUFFER AREA       *~
            *************************************************************

            select #01, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1 , keylen = 3

            select  #02, "GLMAIN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select  #03, "SYSFILE2",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select  #09, "GNJBUFFR",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 4, keylen = 10,                         ~
                        alt key 1, keypos= 1, keylen= 13

            select #10, "GNJBUF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 13

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENFILE" (#01, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
            call "OPENFILE" (#02, "SHARE", f2%( 2), rslt$( 2), axd$( 2))
            call "OPENFILE" (#03, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            call "OPENFILE" (#09, "SHARE", f2%( 9), rslt$( 9), axd$( 9))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))

            REM OPEN BUFFER FILES IF THEY ARE NOT ALREADY.
                if f2%(9) = 0 then L04130
                   call"OPENFILE"(#09, "OUTPT", f2%(9), rslt$(9), axd$(9))
                   close #09
                   call"OPENFILE"(#09, "SHARE", f2%(9), rslt$(9), axd$(9))

L04130:         if f2%(10)= 0 then L09000
                   call"OPENFILE"(#10,"OUTPT",f2%(10),rslt$(10),axd$(10))
                   close #10
                   call"OPENFILE"(#10,"SHARE",f2%(10),rslt$(10),axd$(10))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * INITIALIZES VARIOUS PROMPTS USED IN SCREEN DISPLAYS.      *~
            *************************************************************

*        Do a GETPARM to find JNLID$
L09035:     call "GETPARM" addr ("I ", "R", "JNLID   ",  " ", "0001",    ~
                          "PRLTOG", "INPUT THE JOURNAL ID TO POST THRU ",~
                          34%, "K", "JNLID   ", jnlid$, 3%, 5%, 32%, "A")
            if jnlid$ = " " then L09035
            scrn_head$ = "Enter your Journal Entries"
            if jnlid$ <> "GAJ" then L09071
                any_date% = 1%
                scrn_head$ = "Enter Adjusting Journal Entries"
L09071:     if jnlid$ <> "STC" then L09073
                scrn_head$ = "Enter Standard Cost Journal Entries"
L09073:     if jnlid$ <> "GYE" then L09080
                scrn_head$ = "Enter Year End Journal Entries"
                any_date% = 1%

L09080:     edtmessage$ = "To Modify Displayed Values, Position Cursor" &~
                          " to Desired Line And Press (ENTER)."

            init(hex(00)) edtcoltran$()
            init(hex(01)) str(edtcoltran$(1),  1)
            init(hex(02)) str(edtcoltran$(1), 16)
            init(hex(03)) str(edtcoltran$(1), 57)
            init(hex(04)) str(edtcoltran$(1), 71)
            init(hex(04)) str(edtcoltran$(2),  1, 50)
            init(hex(05)) str(edtcoltran$(2), 51, 14)
            init(hex(06)) str(edtcoltran$(2), 65, 16)

            date$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)
            str(line2$,62) = "GNJINPUT: " & str(cms2v$,,8)

            call "READ100" (#03, "FISCAL DATES", f1%(3))
            if f1%(3) = 1% then L09200
                call "ASKUSER" (2%, scrn_head$,                          ~
                          "Unable to find G/L Fiscal Calendar.",         ~
                          "Please correct problem and re-run program",   ~
                          "Press (RETURN) to return to menu...")
                goto L65000
L09200:     get #3, using L09205, periods%, u$()
L09205:     FMT XX(20), BI(2), 17*CH(8)

*        Check if G/L date one of the months open...
            call "READ100" (#01, userid$, f1%(1))
            if f1%(1) = 1% then L09260
                call "ASKUSER" (2%, scrn_head$,                          ~
                          "Unable to find User's Module Date.",          ~
                          "Please correct problem and re-run program",   ~
                          "Press (RETURN) to return to menu...")
                goto L65000

L09260
*        Is the month open?
            get #01, using L09270 , gldate$
L09270:         FMT XX(21), CH(6)
            call "WHICHPER" (#03, gldate$, thismonth%)
            if thismonth% <> 0% then L09310
                call "ASKUSER" (2%, scrn_head$,                          ~
                     "Your G/L Module Date is in a month that is no",    ~
                     "longer open.  Press RETURN to acknowledge this",   ~
                     "message and continue...")

L09310:     call "DATEFMT" (gldate$)
            title$(1,1) = "(1)Start Over(2)Col 1(4)Line Above(13)Instrs(1~
        ~5)Prt Scrn"
            title$(1,2) = "(16)Return   "
            title$(2,1) = "(1)StartOvr(2)First(3)Last(4)Prev(5)Next(6)Dow~
        ~n(7)Up(8)Descr"
            title$(2,2) = "(9)Hdr(10)FlipScrn(11)Ins(12)Del(13)Instrs(15)~
        ~Print(16)SAVE"
            title$(3,1) = "Supply requested items or (4)Prev and (ENTER)"
            title$(3,2) = "or (1)To Exit Insert Mode"
            title$(4,1) = "Press (ENTER) to DELETE flashing line or (1) t~
        ~o EXIT delete."
            blankline$ = " "

            pfkeys$(1) = hex(000102040d0f10ffffffffffffffffffff)
            pfkeys$(2) = hex(000102030405060708090a0b0c0d0f10ff)
            pfkeys$(3) = hex(0001040d0fffffffffffffffffffffffff)
            pfkeys$(4) = hex(00010d0fffffffffffffffffffffffffff)

            i% = 0
            readkey$ = "MODULENO:0000"
L09415:     call "PLOWNEXT" (#03, readkey$, 11%, f1%(3))
                if f1%(3) = 0% then L09485
            module$ = str(readkey$,12,2) : gosub test_module
            if flag% <> 0% then L09415 : i% = i% + 1%
            str(module_name$(i%),,3) = module$ & ")"
            call "DESCRIBE" (#03, readkey$, str(module_name$(i%),6),0%,0%)
*          IF MODULE$ = "99" THEN CLOSING_ALLOWED% = 1%
            goto L09415

        test_module
            flag% = 2%
            call "JNLINFO" (module$, jnlid$, 0%, " ", " ", " ", #03,     ~
                            f2%(3), flag%)
        return

L09485:     if module_name$(1) <> " " then L10000
                call "ASKUSER" (2%, scrn_head$,                          ~
                          "Journal ID " & jnlid$ & " is not set up for", ~
                          "ANY Module.  Please correct problem.",        ~
                          "Press (RETURN) to return to menu...")
                goto L65000

L10000: REM *************************************************************~
            *                  I N P U T   H E A D E R                  *~
            * --------------------------------------------------------- *~
            * INPUTS JOURNAL ENTRY MNEMONIC AND ENTRY DESCRIPTION.      *~
            *************************************************************

        inputmode
            init(" ") name$, description$, acct$(), descr$(), module$,   ~
                      debit$(), credit$(), errormsg$, infomsg$,postdate$,~
                      postdescr$, ref1$(), ref2$(), edtmessage$,         ~
                      refinpopt$, ref1dflt$, ref2dflt$
            total = 0
L10110:     editmode% = 0

            for fieldnr% = 1 to 7
            if fieldnr% = 3 then postdate$ = gldate$
                gosub'161(fieldnr%)
                      if fieldnr% = 5 then linescrn% = 2% else           ~
                                           linescrn% = 1%
                      if enabled% = 0 then L10230
L10170:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 12 then delete_entry
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10170
L10230:         gosub'150(fieldnr%)
                      if errormsg$ <> " " then L10170
                if fieldnr% = 3% then fieldnr% = 5%
                next fieldnr%

        REM *************************************************************~
            *              I N P U T   L I N E   I T E M S              *~
            * --------------------------------------------------------- *~
            * INPUTS LINE ITEMS AND TESTS FOR VALIDITY.                 *~
            *************************************************************

            maxlines%, screenline%, currentline% , line% = 0

L11080:     screenline% = screenline% + 1
            currentline% = currentline% + 1
            if screenline% <= 20 then L11150
               screenline% = 1
               line% = line% + 20
               if line% = 800  then editmode

L11150:     infomsg$ = " "
            for fieldnr% = 1 to 5
                linescrn% = 1
                if fieldnr% = 5 then linescrn% = 2
                gosub'160(fieldnr%)
                      if enabled% = 0 then L11280
L11190:         gosub'203(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  2 then gosub columnone
                      if keyhit%  =  4 then gosub lineabove
                      if keyhit%  = 16 and fieldnr% = 1 then editmode
                      if keyhit% <>  0 then       L11190
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L11190
L11280:         next fieldnr%
            maxlines% = maxlines% + 1
            gosub L25000
            if maxlines% = 800 then editmode
            goto  L11080

        REM *************************************************************~
            *       E D I T   H E A D E R   I N F O R M A T I O N       *~
            * --------------------------------------------------------- *~
            * EDITS HEADER INFORMATION.                                 *~
            *************************************************************

        editmode
            init(" ") errormsg$, infomsg$
            editmode% = 1  :  linescrn% = 1

L12100:     gosub'202(0%)
                  if keyhit% =  1 then gosub startover
                  if keyhit% =  2 then       editlines
                  if keyhit% = 16 then       datasave
                  if keyhit% = 28 then       delete_journal
                  if keyhit% <> 0 then       L12100
            fieldnr% = max(0, cursor%(1) - 5)
            if fieldnr% < 2 or fieldnr% > 7 then L12100
            if fieldnr% >= 3 and fieldnr% <= 5 then fieldnr% = 3

L12180:     gosub'202(fieldnr%)
                  if keyhit% =  1 then gosub startover
                  if keyhit% <> 0 then       L12180
            gosub'150(fieldnr%)
                  if errormsg$ <> " " then L12180
            goto editmode

        delete_journal
L12260:     ask% = 2%
            call "ASKUSER" (ask%, "* * * DELETEION WARNING * * *",       ~
                            "The Entire Journal Is About To Be Deleted.",~
                            "Press PF1 To Return to Header.",            ~
                            "Press RETURN to Continue Deletion.")
            if ask% = 1% then editmode
            if ask% <> 0% then L12260
               maxlines% = 0%
               goto datasave

        REM *************************************************************~
            *               E D I T   L I N E   I T E M S               *~
            * --------------------------------------------------------- *~
            * EDITS LINE ITEMS, GOES AND SAVES DATA TO THE FILE, ETC.   *~
            *************************************************************

        editlines
            line%, currentline%, screenline% = 0

L13090:     gosub'213(0%)
                  if keyhit%  =  0 then       L13270
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       line%=0
                  if keyhit%  =  3 then       line%=max(0,maxlines%-18)
                  if keyhit%  =  4 then       line%=max(0,line%-18)
                  if keyhit%  =  5 then       line%=min(line%+18,        ~
                                                 max(0,maxlines%-18))
                  if keyhit%  =  6 then       line%=max(0,line%-1)
                  if keyhit%  =  7 then       line%=min(line%+1,         ~
                                                 max(0, maxlines%-18))
                  if keyhit% <>  8 then       L13200
                     if descrtype% = 1% then descrtype% = 2% else        ~
                        descrtype% = 1%
                     infomsg$ = " "
L13200:           if keyhit%  =  9 then       editmode
                  if keyhit% <> 10 then       L13210
                     if linescrn% = 1% then linescrn% = 2% else          ~
                        linescrn% = 1%
L13210:           if keyhit%  = 11 then gosub insertmode
                  if keyhit%  = 12 then gosub deletemode
                  if keyhit%  = 13 then call "MANUAL" ("GNJINPUT")
                  if keyhit%  = 16 then       datasave
                  goto L13090

L13270:     REM NOW FIGURE OUT WHICH FIELD HE HIT.
                screenline% = max(0%, cursor%(1) - 4%)
                if screenline%  =  0 then L13090
                currentline% = screenline% + line%
                if currentline% > maxlines% then L13090
                if linescrn% = 2% then fieldnr% = 5%
                if linescrn% = 2% then L13330
                fieldnr% = val(str(edtcoltran$(1),cursor%(2)))
L13330:         if fieldnr% = 0 then L13090

L13360:         gosub'213(fieldnr%)
                      on keyhit% gosub startover
                      if keyhit% <> 0 then L13360
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L13360

                gosub L25000
                goto L13090

        REM *************************************************************~
            *        C O L U M N   O N E ,   L I N E   A B O V E        *~
            * --------------------------------------------------------- *~
            * COLUMN ONE KEY AND LINE ABOVE KEY FUNCTIONS HANDLED HERE. *~
            *************************************************************

        columnone
            if fieldnr% = 1 then return
            acct$(currentline%), descr$(currentline%,1),                 ~
            descr$(currentline%,2),                                      ~
            ref1$(currentline%), ref2$(currentline%),                    ~
            debit$(currentline%), credit$(currentline%) = " "
            infomsg$ = " "
            fieldnr% = 1
            return

        lineabove
            if currentline% = 1 then return
            on fieldnr% gosub L15220,     /* ACCOUNT NUMBER             */~
                              L15230,     /* ACCOUNT DESCRIPTION        */~
                              L15240,     /* DEBIT AMOUNT THIS ACCOUNT  */~
                              L15250,     /* CREDIT AMOUNT              */~
                              L15252      /* ACCOUNT REF 1 & 2          */
            return

L15220:     acct$  (currentline%)   = acct$  (currentline%-1)   : return
L15230:     descr$ (currentline%,1) = descr$ (currentline%-1,1)
            descr$ (currentline%,2) = descr$ (currentline%-1,2) : return
L15240:     debit$ (currentline%)   = debit$ (currentline%-1)   : return
L15250:     credit$(currentline%)   = credit$(currentline%-1)   : return
L15252:     ref1$  (currentline%)   = ref1$  (currentline%-1)
            ref2$  (currentline%)   = ref2$  (currentline%-1)   : return

L16000: REM *************************************************************~
            *              I N S E R T   M O D E   C O D E              *~
            * --------------------------------------------------------- *~
            * HANDLES INSERTION OF A LINE ITEM INTO THE JOURNAL ENTRY   *~
            *************************************************************

        insertmode
            if maxlines% = 800 then return         /* ARRAY FULL, CAN'T*/
            linescrn% = 1%
            REM OTHERWISE, SET CURRENTLINE%, SCREENLINE%, AND COPY RIGHT
                screenline% = max(0,cursor%(1)-4)
                if screenline% > 0% then goto L16100
                    errormsg$ = "Position cursor where you wish to inse"&~
                        "rt a new line"
                    return
L16100:         if line% + screenline% < maxlines% then L16120
                   screenline% = maxlines% - line% /* TO INS AT END    */
L16120:         if screenline% <> 20 then L16150    /* BOTTOM OF PAGE   */
                   line% = line% + 1
                   screenline% = screenline% - 1
L16150:         currentline%, c% = screenline% + line%

            REM COPY ALL THE ELEMENTS UP ONE
                if c% >= maxlines% then L16260
                for temp% = maxlines% to c% step -1
                    acct$  (temp%+1)   = acct$  (temp%)
                    ref1$  (temp%+1)   = ref1$  (temp%)
                    ref2$  (temp%+1)   = ref2$  (temp%)
                    descr$ (temp%+1,1) = descr$ (temp%,1)
                    descr$ (temp%+1,2) = descr$ (temp%,2)
                    debit$ (temp%+1)   = debit$ (temp%)
                    credit$(temp%+1)   = credit$(temp%)
                    next temp%

L16260:         screenline% = screenline% + 1
                c%, currentline% = currentline% + 1

                init(" ") acct$(c%), descr$(c%,1), descr$(c%,2),         ~
                          debit$(c%), credit$(c%),  errormsg$, infomsg$, ~
                          ref1$(c%), ref2$(c%)

            REM NOW INPUT THE LINE, ENABLE CANCEL OUT OPTION
                infomsg$ = " "
                for fieldnr% = 1 to 5
                    if fieldnr% = 5 then linescrn% = 2% else             ~
                                         linescrn% = 1%
                    gosub'160(fieldnr%)
                          if enabled% = 0 then L16420
L16370:             gosub'223(fieldnr%)
                          if keyhit%  =  1 then L16490     /* END INSERT*/
                          if keyhit%  =  4 then gosub lineabove
                          if keyhit% <>  0 then L16370
                    gosub'151(fieldnr%)
                          if errormsg$ <> " " then L16370
L16420:             next fieldnr%

                maxlines%  = maxlines% + 1
                cursor%(1) = min(cursor%(1) + 1, 24)
                gosub L25000              /* RE# LINES, RECOMPUTE TOTAL */
                goto L16000

L16490:     REM THIS ROUTINE ABORTS INSERT MODE AND DESTROYS SCREENLINE%
                c% = currentline%
                if currentline% <= maxlines% then gosub L16620

                temp% = maxlines% + 1
                if temp% > 800 then temp% = 800
                init(" ") acct$(temp%), descr$(temp%,1), descr$(temp%,2),~
                          debit$(temp%), credit$(temp%), errormsg$,      ~
                          infomsg$, ref1$(temp%), ref2$(temp%)

                gosub L25000              /* RENUMBER & RETOTAL ENTRY   */
                if currentline% >= maxlines% and screenline% = 20        ~
                                          then line% = max(0%, line%- 1%)
            return

L16620:     for temp% = currentline% to maxlines%
                acct$   (temp%)   = acct$   (temp%+1)
                ref1$   (temp%)   = ref1$   (temp%+1)
                ref2$   (temp%)   = ref2$   (temp%+1)
                descr$  (temp%,1) = descr$  (temp%+1,1)
                descr$  (temp%,2) = descr$  (temp%+1,2)
                debit$  (temp%)   = debit$  (temp%+1)
                credit$ (temp%)   = credit$ (temp%+1)
                next temp%
            return

        REM *************************************************************~
            *              D E L E T E   M O D E   C O D E              *~
            * --------------------------------------------------------- *~
            * DELETES A LINE ITEM FROM THE JOURNAL ENTRY.               *~
            *************************************************************

        deletemode
            if maxlines% = 0 then return
            screenline% = cursor%(1)-4
            if screenline% < 1 then return
               currentline% = screenline% + line%
               if currentline% > maxlines% then return
               linescrn% = 1%

L17130:     gosub'233(screenline%)
                  if keyhit%  =  1 then       return
                  if keyhit% <>  0 then       L17130

            c% = currentline%
            if currentline% < maxlines% then gosub L16620
                                         /* ACTUALLY DELETE LINE @C%   */
            temp% = maxlines%
            init(" ") acct$(temp%), descr$(temp%,1), descr$(temp%,2),    ~
                      ref1$(temp%), ref2$(temp%), debit$(temp%),         ~
                      credit$(temp%), errormsg$, infomsg$
            maxlines% = maxlines% - 1
            gosub L25000                  /* RENUMBER & RETOTAL ENTRY   */
            return

        REM *************************************************************~
            *  D E L E T E  R E Q E U S T E D  B U F F E R  E N T R Y   *~
            * --------------------------------------------------------- *~
            * CHECKS TO MAKE SURE ENTRY TO DELETE IS IN BUFFER THEN ZAPS*~
            *************************************************************

        delete_entry

L18070:     for fieldnr% = 1 to 2
                gosub'054(fieldnr%)
                      if enabled% = 0 then L18154
                      if fieldnr% = 2 then edtmessage$ = hex(94) &       ~
                      "Press (RETURN) To Delete, 16 To Cancel"
L18100:         gosub'204(fieldnr%)
                      if keyhit% = 16 and fieldnr% = 2 then L18150
                      if keyhit% =  1 then gosub startover
                      if keyhit% = 16 and fieldnr% = 1 then inputmode
                      if keyhit% <> 0 and fieldnr% = 2 then L18100
L18150:         gosub'154(fieldnr%)
                      if errormsg$ <> " " then L18100
L18154:     next fieldnr%

            if keyhit% = 16 then L18070
            call "READ101" (#09, name$, f1%(9%))
                if f1%(9%) <> 0 then delete #09
            call "DELETE" (#10, name$, 10%)
            goto inputmode

        REM *************************************************************~
            *     C H E C K   S U M   A N D   W R I T E   E N T R Y     *~
            * --------------------------------------------------------- *~
            * CHECKS TO MAKE SURE THAT THE DEBITS BALANCE THE CREDITS   *~
            * IF THEY DON'T, USER GETS SCREEN TELLING HOW FAR OFF WE ARE*~
            *************************************************************

        datasave
            REM FIRST, TOTAL UP JOURNAL ENTRY.  IF NOT NET 0, ERROR.
                total = 0
                if maxlines% = 0% then L19180
                for temp% = 1 to maxlines%
                     debit, credit = 0
                     convert debit$ (temp%) to debit , data goto L19130
L19130:              convert credit$(temp%) to credit, data goto L19140
L19140:              total = round(total + debit - credit, 2)
                next temp%

L19180:         if abs(total) < .00001 then L19220
                   gosub L49000
                if jnlid$ <> "GAJ" then editmode
                if keyhit% <> 16 then editmode

L19220:     REM NOW SAVE THE INVOICE ON THE BUFFER.
                call "READ101" (#09, name$, f1%(9))
                      if f1%(9) <> 0 then delete #09
                call "DELETE" (#10, name$, 10%)
                gosub L31000

            REM SET NAME OF LAST ENTRY
                lastentry$ = name$
                goto inputmode

        REM *************************************************************~
            *   I N P U T   E N A B L E   H E A D E R   S C R E E N     *~
            * --------------------------------------------------------- *~
            * ENABLE INPUT OF EACH FIELD ON THE SCREEN.                 *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 1
                  on fieldnr% gosub L20140,         /* ENTRY NAME       */~
                                    L20160,         /* DESCRIPTION      */~
                                    L20175,         /* REF TEXT OPTION  */~
                                    L20175,         /* REF TEXT 1       */~
                                    L20175,         /* REF TEXT 2       */~
                                    L20180,         /* POSTING DATE     */~
                                    L20210          /* JOURNAL ID       */
                     return

L20140:     REM ENABLE FOR ENTRY NAME
                return
L20160:     REM ENABLE FOR DESCRIPTION
                return
L20175:     REM ENABLE FOR REFERENCE TEXT FIELD
                ref1dflt$ = name$
                return
L20180:     REM ENABLE FOR POSTING DATE
                enabled% = any_date%
                return
L20210:     REM ENABLE FOR JOURNAL ID
                module$ = "06"
                if module_name$(2) <> " " then return
                module$ = module_name$(1)
                enabled% = 0
                return

        REM *************************************************************~
            *   I N P U T   E N A B L E   T A B U L A R   F I E L D S   *~
            * --------------------------------------------------------- *~
            * ENABLE INPUT OF EACH FIELD IN THE TABLE.  THIS WILL MAKE  *~
            * DESCRIPTION ONLY PROCESSING SIMPLER THAN HARDWIRING IT.   *~
            *************************************************************

            deffn'160(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L21170,         /* ACCOUNT NUMBER   */~
                                    L21310,         /* DESCRIPTION      */~
                                    L21360,         /* DEBIT AMOUNT     */~
                                    L21410,         /* CREDIT AMOUNT    */~
                                    L21500          /* REFERENCE TEXT   */
                     return

L21170:     REM ENABLE FOR ACCOUNT NUMBER
                str(infomsg$,1,25)= "Enter Account Code"
                enabled% = 1
                return
L21310:     REM ENABLE FOR DESCRIPTION
                str(infomsg$,1,29)= "Enter Description"
                descr$(currentline%,1) = description$
                enabled% = 1  :  descrtype% = 1%
                return
L21360:     REM ENABLE FOR DEBIT AMOUNT (ENABLED ONLY FOR NON-BLANK ACCT)
                str(infomsg$,1,29)= "Enter Debit Amount"
                   enabled% = 1
                   return
L21410:     REM ENABLE FOR CREDIT (DITTO.)
*              IF ACCT$(CURRENTLINE%) = " " THEN RETURN
                if debit <> 0 then gosub'151(fieldnr%)
                if debit <> 0 then L21470
                str(infomsg$,1,29)= "Enter Credit Amount"
                   enabled% = 1
L21470:            return

            REM ENABLE FOR REFERENCE TEXT
L21500:         ref1$(currentline%) = ref1dflt$
                ref2$(currentline%) = ref2dflt$
                if refinpopt$ = "Y" then enabled% = 1%
                return

        REM *************************************************************~
            *    I N P U T  E N A B L E  F O R  D E L E T E  E N T R Y  *~
            * --------------------------------------------------------- *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR DELETE ENTRY SCREEN  *~
            *************************************************************

            deffn'054(fieldnr%)
                  enabled% = 1
                  on fieldnr% gosub L23120,    /* NAME$                 */~
                                    L23170     /* EDITMODE              */
            return

L23120:     REM DEFAULT/ENABLE FOR NAME$
                edtmessage$ ="Enter Journal Name to Delete or PF16 for In~
        ~put Screen"
                return

L23170:     REM DEFAULT/ENABLE FOR EDITMODE
               edtmessage$ = "Press ENTER to DELETE or PF16 for Editmode"
               return

L25000: REM *************************************************************~
            *    R E N U M B E R   A N D   R E T O T A L   E N T R Y    *~
            * --------------------------------------------------------- *~
            * RENUMBERS AND RETOTALS THE JOURNAL ENTRY.  THIS IS ITS    *~
            * OWN ROUTINE SINCE IT GETS USED ALL OVER THE PLACE.        *~
            *************************************************************

L25070:     total = 0
            for temp% = 1 to maxlines%
                debit, credit = 0
                convert debit$ (temp%) to debit , data goto L25110
L25110:         convert credit$(temp%) to credit, data goto L25120
L25120:         total = round(total + debit - credit, 2)
            next temp%

            return


        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            * --------------------------------------------------------- *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
L29090:     u3% = 2%
            call "STARTOVR" (u3%)
            if u3%  = 1% then return
            if u3% <> 0% then L29090
                return clear all
                goto inputmode

                                                                         ~
L30000: REM *************************************************************~
            *        L O A D   E N T R Y   F R O M   B U F F E R        *~
            * --------------------------------------------------------- *~
            * SEARCHES THE BUFFER FOR THE OLD JOURNAL ENTRY NAME AND    *~
            * LOADS IT FOR US.                                          *~
            *************************************************************

            oldentryonfile% = 0
            oldjnlid$ = jnlid$
            REM SEARCH BUFFER AND SEE IF IT'S ON FILE, RETURN IF NOT.
                call "READ100" (#09, name$, f1%(9))
                      if f1%(9) = 0 then return
                get #09, using L30140,ruserid$,header$,module$,postdate$, ~
                        oldjnlid$
L30140:                 FMT CH(03), CH(46), CH(2), CH(6), CH(3)
                if ruserid$ <> userid$ then L30180
                if oldjnlid$ = jnlid$ then L30220
*              IF OLDJNLID$ = "GYE" AND CLOSING_ALLOWED% = 1 THEN 30220
                   errormsg$ = "Invalid Journal for " & jnlid$ & ": "
                   errormsg$ = errormsg$ & oldjnlid$
                     goto L30190
L30180:            errormsg$ = "Journal Entry Name In Use By: " & ruserid$
L30190:            postdate$, module$ = " "
                   return

L30220:     REM LOAD AND FORMAT HEADER INFORMATION.
                call "SHOSTAT" ("Loading Journal Entry...")
                get header$, using L30550, description$
                call "DATEFMT" (postdate$)
                oldentryonfile% = 1
                refinpopt$ = "Y"
                ref1dflt$ = name$

            REM PLOW ROUTINE TO LOAD DATA FROM LINE ITEMS.
                readkey$ = name$
                maxlines% = 0

L30320:         call "PLOWNEXT" (#10, readkey$, 10%, f1%(10))
                     if f1%(10) = 0 then L30580
                if maxlines% = 800 then lineitems_error
                maxlines% = maxlines% + 1
                get #10, using L30460,                                    ~
                            acct$(maxlines%), ref1$(maxlines%),          ~
                            ref2$(maxlines%), descr$(maxlines%,1),       ~
                            debit, credit
                if debit <> 0 then                                       ~
                    call "CONVERT" (debit , 2.2, debit$ (maxlines%))
                if credit <> 0 then                                      ~
                    call "CONVERT" (credit, 2.2, credit$(maxlines%))
                call "GLFMT" (acct$(maxlines%))
                call "GETCODE" (#02, acct$(maxlines%),                   ~
                                    descr$(maxlines%, 2), 0%, 99, f1%(2))
                goto L30320

L30460:     FMT XX(3),                   /* USERID OF CURRENT USER     */~
                XX(10),                  /* JOURNAL ENTRY MNEMONIC     */~
                CH(9),                   /* ACCOUNT NUMBER             */~
                CH(30),                  /* REF 1                      */~
                CH(34),                  /* REF 2                      */~
                XX(4),                   /* SPECIAL POSTING FLAG       */~
                CH(32),                  /* DESCRIPTION OF THIS LINE   */~
                PD(14,4),                /* DEBIT AMOUNT               */~
                PD(14,4)                 /* CREDIT AMOUNT              */

L30550:     FMT XX(10),                  /* NAME OF JOURNAL ENTRY      */~
                CH(36)                   /* DESRIPTION OF THIS ENTRY   */~

L30580:     REM SET JOURNAL FOR SCREEN DISPLAY
                fc2$() = all (hex(8c))
                convert module$ to j%, data goto L30710
                convert j% to module$, pic(00)
                search module_name$() = module$ to cursor%() step 35
                    if cursor%(1) = 0 then L30710
                element% = (cursor%(1)+34)/ 35
                fc2$(element%) = hex(84)
                mod_descr$ = str(module_name$(element%),4)
                gosub L25070
                return
L30710:         errormsg$ = "Invalid entry for Journal - see list below a~
        ~nd re-enter"
                return

        lineitems_error
            errormsg$ = "Journal entry contains more than 800 line items"
            init (" ") name$, description$, acct$(), descr$(), module$,  ~
                       debit$(), credit$(), infomsg$, postdate$,         ~
                       postdescr$, ref1$(), ref2$()
            return clear
            goto L10110

L31000: REM *************************************************************~
            *            W R I T E   D A T A   T O   F I L E            *~
            * --------------------------------------------------------- *~
            * WRITE DATA TO FILE, AFTER HAVING OLD DELETED BY L. 19000  *~
            *************************************************************

            REM WRITE HEADER TO BUFFER FILE.
                if maxlines% = 0 then return
                call "DATUNFMT" (postdate$)
                write #09, using L31250, userid$, name$, description$,    ~
                                       module$, postdate$, oldjnlid$, " "

            REM WRITE LINE ITEMS TO FILE
                if maxlines% = 0 then return
                   for temp% = 1 to maxlines%
                       convert temp% to seqnr$, pic(###)
                       debit, credit = 0
                       convert debit$ (temp%) to debit , data goto L31180
L31180:                convert credit$(temp%) to credit, data goto L31181
L31181:                call "GLUNFMT" (acct$(temp%))
                       write #10, using L31310 ,                          ~
                                  name$, seqnr$, acct$(temp%),           ~
                                  ref1$(temp%), ref2$(temp%), " ",       ~
                                  descr$(temp%, 1%), debit, credit," "
                   next temp%
                return

L31250:     FMT CH(3),                   /* USERID                     */~
                CH(10),                  /* NAME OF ENTRY              */~
                CH(36),                  /* DESCRIPTION                */~
                CH(02),                  /* WHICH JOURNAL              */~
                CH(06),                  /* POSTING DATE               */~
                CH(03),                  /* SOURCE PROGRAM (JOURNAL)   */~
                CH(40)                   /* FILLER                     */

L31310:     FMT CH(10),                  /* ENTRY NAME                 */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                CH(9),                   /* ACCOUNT NUMBER             */~
                CH(30),                  /* REF 1                      */~
                CH(34),                  /* REF 2                      */~
                CH(4),                   /* SPECIAL POSTING FLAG       */~
                CH(32),                  /* DESCRIPTION THIS LINE      */~
                PD(14,4),                /* DEBIT AMOUNT               */~
                PD(14,4),                /* CREDIT AMOUNT              */~
                CH(22)                   /* FILLER                     */

        REM *************************************************************~
            *        G E T   J O U R N A L   E N T R Y   N A M E        *~
            * --------------------------------------------------------- *~
            * GETS NAME OF THIS JOURNAL ENTRY NAME AND A DESCRIPTION OF *~
            * IT.  THE NAME IS NECESSARY FOR RECALL.                    *~
            *************************************************************

            deffn'201(fieldnr%)
                  init(hex(8c)) linefac$()
                  str(line2$,,50) = "Last Entry: " &  lastentry$
                  on fieldnr% gosub L40210,         /* ENTRY NAME       */~
                                    L40180,         /* DESCRIPTION OF   */~
                                    L40210,         /* Enter Ref Text?  */~
                                    L40180,         /* Reference #1     */~
                                    L40180,         /* Reference #2     */~
                                    L40210,         /* POSTING DATE     */~
                                    L40240          /* WHICH JOURNAL    */
                  goto L40270

L40180:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      linefac$(fieldnr%) = hex(80)
                      return
L40210:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      linefac$(fieldnr%) = hex(81)
                      return
L40240:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      linefac$(fieldnr%) = hex(82)
                      return

L40270:        if fieldnr% = 1% then pf16$ = "(16)EXIT PROGRAM"          ~
                                else pf16$ = " "

L40280:     accept                                                       ~
               at (01,02), fac(hex(8c)), scrn_head$,                     ~
               at (01,66), "Today: ",                                    ~
               at (01,73), fac(hex(8c)),      date$             , ch(08),~
               at (02,02), fac(hex(ac)),      line2$            , ch(79),~
               at (04,02), fac(hex(94)),      errormsg$         , ch(79),~
               at (06,02),                                               ~
                  "Journal Entry Name",                                  ~
               at (06,30), fac(linefac$( 1)), name$             , ch(10),~
               at (07,02),                                               ~
                  "Title of Entry",                                      ~
               at (07,30), fac(linefac$( 2)), description$      , ch(32),~
               at (08,02), "Enter Reference Text? (Y/N)",                ~
               at (08,30), fac(linefac$( 3)), refinpopt$        , ch(01),~
               at (09,02), "Ref Text 1 Default",                         ~
               at (09,30), fac(linefac$(  3)), ref1dflt$        , ch(30),~
               at (10,02), "Ref Text 2 Default",                         ~
               at (10,30), fac(linefac$(  3)), ref2dflt$        , ch(34),~
               at (11,02),                                               ~
                  "Posting Date",                                        ~
               at (11,30), fac(linefac$( 6)), postdate$         , ch(08),~
               at (11,45), fac(hex(84)),      postdescr$        , ch(32),~
               at (12,02),                                               ~
                  "Module to Post To",                                   ~
               at (12,30), fac(linefac$( 7)), module$           , ch(02),~
               at (14,19), "** POSTABLE MODULES ARE LISTED BELOW **",    ~
               at (15,10), fac(hex(8c)), module_name$(01)       , ch(30),~
               at (16,10), fac(hex(8c)), module_name$(02)       , ch(30),~
               at (17,10), fac(hex(8c)), module_name$(03)       , ch(30),~
               at (18,10), fac(hex(8c)), module_name$(04)       , ch(30),~
               at (19,10), fac(hex(8c)), module_name$(05)       , ch(30),~
               at (15,50), fac(hex(8c)), module_name$(06)       , ch(30),~
               at (16,50), fac(hex(8c)), module_name$(07)       , ch(30),~
               at (17,50), fac(hex(8c)), module_name$(08)       , ch(30),~
               at (18,50), fac(hex(8c)), module_name$(09)       , ch(30),~
               at (19,50), fac(hex(8c)), module_name$(10)       , ch(30),~
               at (21,02), fac(hex(a4)),      blankline$        , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,18),                                               ~
                  "(12)Delete Entry",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (24,65),                                               ~
                  fac(hex(8c)), pf16$                           , ch(16),~
                                                                         ~
               keys(hex(00010c0d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L40820
                  call "MANUAL" ("GNJINPUT")
                  goto L40280

L40820:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

        REM *************************************************************~
            *       E D I T   H E A D E R   I N F O R M A T I O N       *~
            * --------------------------------------------------------- *~
            * EDITS HEADER INFORMATION FOR THE JOURNAL ENTRY.           *~
            *************************************************************

            deffn'202(fieldnr%)
                  init(hex(8c)) linefac$()
                  str(line2$,,50) = " "
                  on fieldnr% gosub L41160,         /* ENTRY NAME       */~
                                    L41130,         /* DESCRIPTION OF   */~
                                    L41160,         /* REF TEXT INP OPT */~
                                    L41160,         /* REF TEXT 1       */~
                                    L41160,         /* REF TEXT 2       */~
                                    L41160,         /* POSTING DATE     */~
                                    L41190          /* WHICH JOURNAL    */
                  goto L41230

L41130:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      linefac$(fieldnr%) = hex(80)
                      return
L41160:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      linefac$(fieldnr%) = hex(81)
                      return
L41190:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      linefac$(fieldnr%) = hex(82)
                      return

L41230:     accept                                                       ~
               at (01,02), fac(hex(8c)), scrn_head$,                     ~
               at (01,66), "Today: ",                                    ~
               at (01,73), fac(hex(8c)),      date$             , ch(08),~
               at (02,02), fac(hex(ac)),      line2$            , ch(79),~
               at (04,02), fac(hex(94)),      errormsg$         , ch(79),~
               at (06,02),                                               ~
                  "Journal Entry Name",                                  ~
               at (06,30), fac(linefac$( 1)), name$             , ch(10),~
               at (07,02),                                               ~
                  "Title of Entry",                                      ~
               at (07,30), fac(linefac$( 2)), description$      , ch(32),~
               at (08,02), "Enter Reference Text? (Y/N)",                ~
               at (08,30), fac(linefac$( 3)), refinpopt$        , ch(01),~
               at (09,02), "Ref Text 1 Default",                         ~
               at (09,30), fac(linefac$(  3)), ref1dflt$        , ch(30),~
               at (10,02), "Ref Text 2 Default",                         ~
               at (10,30), fac(linefac$(  3)), ref2dflt$        , ch(34),~
               at (11,02),                                               ~
                  "Posting Date",                                        ~
               at (11,30), fac(linefac$( 6)), postdate$         , ch(08),~
               at (11,45), fac(hex(8c)),      postdescr$        , ch(32),~
               at (12,02),                                               ~
                  "Module to Post To",                                   ~
               at (12,30), fac(linefac$( 7)), module$           , ch(02),~
               at (14,19), "** POSTABLE MODULES ARE LISTED BELOW **",    ~
               at (15,10), fac(fc2$(01)), module_name$(01)      , ch(30),~
               at (16,10), fac(fc2$(02)), module_name$(02)      , ch(30),~
               at (17,10), fac(fc2$(03)), module_name$(03)      , ch(30),~
               at (18,10), fac(fc2$(04)), module_name$(04)      , ch(30),~
               at (19,10), fac(fc2$(05)), module_name$(05)      , ch(30),~
               at (15,50), fac(fc2$(06)), module_name$(06)      , ch(30),~
               at (16,50), fac(fc2$(07)), module_name$(07)      , ch(30),~
               at (17,50), fac(fc2$(08)), module_name$(08)      , ch(30),~
               at (18,50), fac(fc2$(09)), module_name$(09)      , ch(30),~
               at (19,50), fac(fc2$(10)), module_name$(10)      , ch(30),~
               at (21,02), fac(hex(a4)) ,      edtmessage$      , ch(79),~
                                                                         ~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,02), "(2)Line Items",                              ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)SAVE ENTRY",                             ~
                                                                         ~
               keys(hex(0001020d0f101c)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L41720
                  call "MANUAL" ("GNJINPUT")
                  return

L41720:        if keyhit% <> 15 then L41760
                  call "PRNTSCRN"
                  return

L41760:        REM GET CURSOR LOCATION FOR EDIT MODE
                   close ws
                   call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                   return

        REM *************************************************************~
            *           D E L E T E  E N T R Y  S C R E E N             *~
            * --------------------------------------------------------- *~
            * HANDLES INPUT OF BUFFER ENTRY TO DELETE FOR INPUT MODE AND*~
            * EDIT MODE                                                 *~
            *************************************************************

            deffn'204(fieldnr%)

                  init(hex(84)) nfac$()
                  on fieldnr% gosub L43170,         /* NAME$            */~
                                    L43170          /* EDITMODE$        */
                     goto L43240

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      nfac$(fieldnr%) = hex(80)
                      return
L43170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      nfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      nfac$(fieldnr%) = hex(82)
                      return

L43240:     accept                                                       ~
               at (01,02),                                               ~
                  "****  DELETE JOURNAL ENTRY ****",                     ~
               at (01,66),                                               ~
                  "Today: ",                                             ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Journal Entry to Delete",                             ~
               at (06,30), fac(nfac$( 1)), name$                , ch(10),~
               at (21,02), fac(hex(ac)), edtmessage$            , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)RETURN",                                          ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L43530
                  call "MANUAL" ("STJINPUT")
                  goto L43240

L43530:        if keyhit% <> 15 then L43570
                  call "PRNTSCRN"
                  goto L43240

L43570:        return

        REM *************************************************************~
            *           T A B U L A R   M O D E   S C R E E N           *~
            * --------------------------------------------------------- *~
            * HANDLES THE INPUT OF FIELDS IN TABLE, IN EITHER INPUT,    *~
            * EDIT, INSERT, OR DELETE STYLE.                            *~
            *************************************************************

            deffn'203(fieldnr%)                    /* INPUT MODE       */
                  screen% = 1
                  goto L45290

            deffn'213(fieldnr%)                    /* EDIT MODE        */
                  screen% = 2
                  if fieldnr% <> 0 then L45290
                     init(hex(86)) fac$()
                     goto L45460

            deffn'223(fieldnr%)                    /* INSERT MODE      */
                  screen% = 3
                  goto L45290

            deffn'233(screenline%)                 /* DELETE MODE      */
                  screen% = 4
                  init(hex(8c)) fac$()
                  for temp% = 1 to 5
                      fac$(screenline%, temp%) = hex(94)
                      next temp%
                  goto L45460

L45290:           init(hex(8c)) fac$()
                  on fieldnr% gosub L45390,         /* ACCOUNT NUMBER   */~
                                    L45360,         /* ACCT DESCRIPTION */~
                                    L45420,         /* DEBIT AMOUNT     */~
                                    L45420,         /* CREDIT AMOUNT    */~
                                    L45390          /* REFERENCE TEXT   */
                  if fieldnr% = 2 then descrtype% = 1%
                  goto L45460

L45360:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(screenline%, fieldnr%) = hex(80)
                      return
L45390:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(screenline%, fieldnr%) = hex(81)
                      return
L45420:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      fac$(screenline%, fieldnr%) = hex(82)
                      return

L45460:     if errormsg$ <> " " then infomsg$ = errormsg$

            if linescrn%  = 2% then goto ref_text_screen
            hdr$ = "Acct Number   Posting Text"
            str(hdr$,58) = "Debit        Credit"
            if descrtype% = 1% or descrtype% = 2% then goto L45467 else   ~
               descrtype% = 1%
L45467:     d% = descrtype%
            if d% = 2 then str(hdr$, 15, 30) = "Account Description"
            accept                                                       ~
               at (01,02), fac(hex(8c)),     title$(screen%,1)  , ch(64),~
               at (02,02), fac(hex(8c)),     title$(screen%,2)  , ch(64),~
               at (03,02), fac(hex(84)),     infomsg$           , ch(60),~
               at (04,02), fac(hex(a4)),     hdr$               , ch(79),~
                                                                         ~
               at (01,66), "! BALANCE:    "                             ,~
               at (02,66), "!"                                          ,~
               at (03,66), "+-------------"                             ,~
               at (02,68), fac(hex(84)), total,       pic(-#########.##),~
                                                                         ~
               at (05,02), fac(fac$( 1,1)), acct$  (line%+ 1)   , ch(12),~
               at (06,02), fac(fac$( 2,1)), acct$  (line%+ 2)   , ch(12),~
               at (07,02), fac(fac$( 3,1)), acct$  (line%+ 3)   , ch(12),~
               at (08,02), fac(fac$( 4,1)), acct$  (line%+ 4)   , ch(12),~
               at (09,02), fac(fac$( 5,1)), acct$  (line%+ 5)   , ch(12),~
               at (10,02), fac(fac$( 6,1)), acct$  (line%+ 6)   , ch(12),~
               at (11,02), fac(fac$( 7,1)), acct$  (line%+ 7)   , ch(12),~
               at (12,02), fac(fac$( 8,1)), acct$  (line%+ 8)   , ch(12),~
               at (13,02), fac(fac$( 9,1)), acct$  (line%+ 9)   , ch(12),~
               at (14,02), fac(fac$(10,1)), acct$  (line%+10)   , ch(12),~
               at (15,02), fac(fac$(11,1)), acct$  (line%+11)   , ch(12),~
               at (16,02), fac(fac$(12,1)), acct$  (line%+12)   , ch(12),~
               at (17,02), fac(fac$(13,1)), acct$  (line%+13)   , ch(12),~
               at (18,02), fac(fac$(14,1)), acct$  (line%+14)   , ch(12),~
               at (19,02), fac(fac$(15,1)), acct$  (line%+15)   , ch(12),~
               at (20,02), fac(fac$(16,1)), acct$  (line%+16)   , ch(12),~
               at (21,02), fac(fac$(17,1)), acct$  (line%+17)   , ch(12),~
               at (22,02), fac(fac$(18,1)), acct$  (line%+18)   , ch(12),~
               at (23,02), fac(fac$(19,1)), acct$  (line%+19)   , ch(12),~
               at (24,02), fac(fac$(20,1)), acct$  (line%+20)   , ch(12),~
                                                                         ~
               at (05,16), fac(fac$( 1,2)), descr$ (line%+ 1,d%), ch(32),~
               at (06,16), fac(fac$( 2,2)), descr$ (line%+ 2,d%), ch(32),~
               at (07,16), fac(fac$( 3,2)), descr$ (line%+ 3,d%), ch(32),~
               at (08,16), fac(fac$( 4,2)), descr$ (line%+ 4,d%), ch(32),~
               at (09,16), fac(fac$( 5,2)), descr$ (line%+ 5,d%), ch(32),~
               at (10,16), fac(fac$( 6,2)), descr$ (line%+ 6,d%), ch(32),~
               at (11,16), fac(fac$( 7,2)), descr$ (line%+ 7,d%), ch(32),~
               at (12,16), fac(fac$( 8,2)), descr$ (line%+ 8,d%), ch(32),~
               at (13,16), fac(fac$( 9,2)), descr$ (line%+ 9,d%), ch(32),~
               at (14,16), fac(fac$(10,2)), descr$ (line%+10,d%), ch(32),~
               at (15,16), fac(fac$(11,2)), descr$ (line%+11,d%), ch(32),~
               at (16,16), fac(fac$(12,2)), descr$ (line%+12,d%), ch(32),~
               at (17,16), fac(fac$(13,2)), descr$ (line%+13,d%), ch(32),~
               at (18,16), fac(fac$(14,2)), descr$ (line%+14,d%), ch(32),~
               at (19,16), fac(fac$(15,2)), descr$ (line%+15,d%), ch(32),~
               at (20,16), fac(fac$(16,2)), descr$ (line%+16,d%), ch(32),~
               at (21,16), fac(fac$(17,2)), descr$ (line%+17,d%), ch(32),~
               at (22,16), fac(fac$(18,2)), descr$ (line%+18,d%), ch(32),~
               at (23,16), fac(fac$(19,2)), descr$ (line%+19,d%), ch(32),~
               at (24,16), fac(fac$(20,2)), descr$ (line%+20,d%), ch(32),~
                                                                         ~
               at (05,57), fac(fac$( 1,3)), debit$ (line%+ 1)   , ch(10),~
               at (06,57), fac(fac$( 2,3)), debit$ (line%+ 2)   , ch(10),~
               at (07,57), fac(fac$( 3,3)), debit$ (line%+ 3)   , ch(10),~
               at (08,57), fac(fac$( 4,3)), debit$ (line%+ 4)   , ch(10),~
               at (09,57), fac(fac$( 5,3)), debit$ (line%+ 5)   , ch(10),~
               at (10,57), fac(fac$( 6,3)), debit$ (line%+ 6)   , ch(10),~
               at (11,57), fac(fac$( 7,3)), debit$ (line%+ 7)   , ch(10),~
               at (12,57), fac(fac$( 8,3)), debit$ (line%+ 8)   , ch(10),~
               at (13,57), fac(fac$( 9,3)), debit$ (line%+ 9)   , ch(10),~
               at (14,57), fac(fac$(10,3)), debit$ (line%+10)   , ch(10),~
               at (15,57), fac(fac$(11,3)), debit$ (line%+11)   , ch(10),~
               at (16,57), fac(fac$(12,3)), debit$ (line%+12)   , ch(10),~
               at (17,57), fac(fac$(13,3)), debit$ (line%+13)   , ch(10),~
               at (18,57), fac(fac$(14,3)), debit$ (line%+14)   , ch(10),~
               at (19,57), fac(fac$(15,3)), debit$ (line%+15)   , ch(10),~
               at (20,57), fac(fac$(16,3)), debit$ (line%+16)   , ch(10),~
               at (21,57), fac(fac$(17,3)), debit$ (line%+17)   , ch(10),~
               at (22,57), fac(fac$(18,3)), debit$ (line%+18)   , ch(10),~
               at (23,57), fac(fac$(19,3)), debit$ (line%+19)   , ch(10),~
               at (24,57), fac(fac$(20,3)), debit$ (line%+20)   , ch(10),~
                                                                         ~
               at (05,71), fac(fac$( 1,4)), credit$(line%+ 1)   , ch(10),~
               at (06,71), fac(fac$( 2,4)), credit$(line%+ 2)   , ch(10),~
               at (07,71), fac(fac$( 3,4)), credit$(line%+ 3)   , ch(10),~
               at (08,71), fac(fac$( 4,4)), credit$(line%+ 4)   , ch(10),~
               at (09,71), fac(fac$( 5,4)), credit$(line%+ 5)   , ch(10),~
               at (10,71), fac(fac$( 6,4)), credit$(line%+ 6)   , ch(10),~
               at (11,71), fac(fac$( 7,4)), credit$(line%+ 7)   , ch(10),~
               at (12,71), fac(fac$( 8,4)), credit$(line%+ 8)   , ch(10),~
               at (13,71), fac(fac$( 9,4)), credit$(line%+ 9)   , ch(10),~
               at (14,71), fac(fac$(10,4)), credit$(line%+10)   , ch(10),~
               at (15,71), fac(fac$(11,4)), credit$(line%+11)   , ch(10),~
               at (16,71), fac(fac$(12,4)), credit$(line%+12)   , ch(10),~
               at (17,71), fac(fac$(13,4)), credit$(line%+13)   , ch(10),~
               at (18,71), fac(fac$(14,4)), credit$(line%+14)   , ch(10),~
               at (19,71), fac(fac$(15,4)), credit$(line%+15)   , ch(10),~
               at (20,71), fac(fac$(16,4)), credit$(line%+16)   , ch(10),~
               at (21,71), fac(fac$(17,4)), credit$(line%+17)   , ch(10),~
               at (22,71), fac(fac$(18,4)), credit$(line%+18)   , ch(10),~
               at (23,71), fac(fac$(19,4)), credit$(line%+19)   , ch(10),~
               at (24,71), fac(fac$(20,4)), credit$(line%+20)   , ch(10),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)
               linescrn% = 1%

L46450:        if keyhit% <> 13 then L46490
                  call "MANUAL" ("GNJINPUT")
                  goto L45460

L46490:        if keyhit% <> 15 then L46530
                  call "PRNTSCRN"
                  goto L45460

L46530:        if screen% <> 2 then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return


        ref_text_screen   /* 'Right side' of line item screen for      */
                          /* input or edit of reference text           */
            hdr$ = "Acct Number"
            str(hdr$,16) = "Reference Text #1"
            str(hdr$,47) = "Reference Text #2"

            accept                                                       ~
               at (01,02), fac(hex(8c)),     title$(screen%,1)  , ch(64),~
               at (02,02), fac(hex(8c)),     title$(screen%,2)  , ch(64),~
               at (03,02), fac(hex(84)),     infomsg$           , ch(60),~
               at (04,02), fac(hex(a4)),     hdr$               , ch(79),~
                                                                         ~
               at (01,66), "! BALANCE:    "                             ,~
               at (02,66), "!"                                          ,~
               at (03,66), "+-------------"                             ,~
               at (02,68), fac(hex(84)), total,       pic(-#########.##),~
                                                                         ~
               at (05,02), fac(fac$( 1,1)), acct$  (line%+ 1)   , ch(12),~
               at (06,02), fac(fac$( 2,1)), acct$  (line%+ 2)   , ch(12),~
               at (07,02), fac(fac$( 3,1)), acct$  (line%+ 3)   , ch(12),~
               at (08,02), fac(fac$( 4,1)), acct$  (line%+ 4)   , ch(12),~
               at (09,02), fac(fac$( 5,1)), acct$  (line%+ 5)   , ch(12),~
               at (10,02), fac(fac$( 6,1)), acct$  (line%+ 6)   , ch(12),~
               at (11,02), fac(fac$( 7,1)), acct$  (line%+ 7)   , ch(12),~
               at (12,02), fac(fac$( 8,1)), acct$  (line%+ 8)   , ch(12),~
               at (13,02), fac(fac$( 9,1)), acct$  (line%+ 9)   , ch(12),~
               at (14,02), fac(fac$(10,1)), acct$  (line%+10)   , ch(12),~
               at (15,02), fac(fac$(11,1)), acct$  (line%+11)   , ch(12),~
               at (16,02), fac(fac$(12,1)), acct$  (line%+12)   , ch(12),~
               at (17,02), fac(fac$(13,1)), acct$  (line%+13)   , ch(12),~
               at (18,02), fac(fac$(14,1)), acct$  (line%+14)   , ch(12),~
               at (19,02), fac(fac$(15,1)), acct$  (line%+15)   , ch(12),~
               at (20,02), fac(fac$(16,1)), acct$  (line%+16)   , ch(12),~
               at (21,02), fac(fac$(17,1)), acct$  (line%+17)   , ch(12),~
               at (22,02), fac(fac$(18,1)), acct$  (line%+18)   , ch(12),~
               at (23,02), fac(fac$(19,1)), acct$  (line%+19)   , ch(12),~
               at (24,02), fac(fac$(20,1)), acct$  (line%+20)   , ch(12),~
                                                                         ~
               at (05,16), fac(fac$( 1,5)), ref1$  (line%+ 1)   , ch(30),~
               at (06,16), fac(fac$( 2,5)), ref1$  (line%+ 2)   , ch(30),~
               at (07,16), fac(fac$( 3,5)), ref1$  (line%+ 3)   , ch(30),~
               at (08,16), fac(fac$( 4,5)), ref1$  (line%+ 4)   , ch(30),~
               at (09,16), fac(fac$( 5,5)), ref1$  (line%+ 5)   , ch(30),~
               at (10,16), fac(fac$( 6,5)), ref1$  (line%+ 6)   , ch(30),~
               at (11,16), fac(fac$( 7,5)), ref1$  (line%+ 7)   , ch(30),~
               at (12,16), fac(fac$( 8,5)), ref1$  (line%+ 8)   , ch(30),~
               at (13,16), fac(fac$( 9,5)), ref1$  (line%+ 9)   , ch(30),~
               at (14,16), fac(fac$(10,5)), ref1$  (line%+10)   , ch(30),~
               at (15,16), fac(fac$(11,5)), ref1$  (line%+11)   , ch(30),~
               at (16,16), fac(fac$(12,5)), ref1$  (line%+12)   , ch(30),~
               at (17,16), fac(fac$(13,5)), ref1$  (line%+13)   , ch(30),~
               at (18,16), fac(fac$(14,5)), ref1$  (line%+14)   , ch(30),~
               at (19,16), fac(fac$(15,5)), ref1$  (line%+15)   , ch(30),~
               at (20,16), fac(fac$(16,5)), ref1$  (line%+16)   , ch(30),~
               at (21,16), fac(fac$(17,5)), ref1$  (line%+17)   , ch(30),~
               at (22,16), fac(fac$(18,5)), ref1$  (line%+18)   , ch(30),~
               at (23,16), fac(fac$(19,5)), ref1$  (line%+19)   , ch(30),~
               at (24,16), fac(fac$(20,5)), ref1$  (line%+20)   , ch(30),~
                                                                         ~
               at (05,47), fac(fac$( 1,5)), ref2$  (line%+ 1)   , ch(34),~
               at (06,47), fac(fac$( 2,5)), ref2$  (line%+ 2)   , ch(34),~
               at (07,47), fac(fac$( 3,5)), ref2$  (line%+ 3)   , ch(34),~
               at (08,47), fac(fac$( 4,5)), ref2$  (line%+ 4)   , ch(34),~
               at (09,47), fac(fac$( 5,5)), ref2$  (line%+ 5)   , ch(34),~
               at (10,47), fac(fac$( 6,5)), ref2$  (line%+ 6)   , ch(34),~
               at (11,47), fac(fac$( 7,5)), ref2$  (line%+ 7)   , ch(34),~
               at (12,47), fac(fac$( 8,5)), ref2$  (line%+ 8)   , ch(34),~
               at (13,47), fac(fac$( 9,5)), ref2$  (line%+ 9)   , ch(34),~
               at (14,47), fac(fac$(10,5)), ref2$  (line%+10)   , ch(34),~
               at (15,47), fac(fac$(11,5)), ref2$  (line%+11)   , ch(34),~
               at (16,47), fac(fac$(12,5)), ref2$  (line%+12)   , ch(34),~
               at (17,47), fac(fac$(13,5)), ref2$  (line%+13)   , ch(34),~
               at (18,47), fac(fac$(14,5)), ref2$  (line%+14)   , ch(34),~
               at (19,47), fac(fac$(15,5)), ref2$  (line%+15)   , ch(34),~
               at (20,47), fac(fac$(16,5)), ref2$  (line%+16)   , ch(34),~
               at (21,47), fac(fac$(17,5)), ref2$  (line%+17)   , ch(34),~
               at (22,47), fac(fac$(18,5)), ref2$  (line%+18)   , ch(34),~
               at (23,47), fac(fac$(19,5)), ref2$  (line%+19)   , ch(34),~
               at (24,47), fac(fac$(20,5)), ref2$  (line%+20)   , ch(34),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)
               linescrn% = 2%

                goto L46450


L49000: REM *************************************************************~
            *       E R R O R   S C R E E N   F O R   T O T A L S       *~
            * --------------------------------------------------------- *~
            * THIS SCREEN DOES THE ERROR MESSAGE IN CASE THE TOTAL OF   *~
            * THE JOURNAL ENTRY DOES NOT BALANCE (IN WHICH CASE IT'D    *~
            * THROW THE GENERAL LEDGER OUT OF BALANCE)                  *~
            *************************************************************
            str(line2$,,50) = " "
            temp$ = "E R R O R ! !"
            pf16$ = " "  :  pfkey$ = hex(000f)
            if jnlid$ <> "GAJ" then L49100
               pf16$ = "(16)Post Out of Balance" : pfkey$ = hex(000f10)

L49100:     accept                                                       ~
               at (01,02), fac(hex(8c)), scrn_head$,                     ~
               at (01,66), "Today: ",                                    ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (08,21), "*****************************************",  ~
               at (09,21), "*                                       *",  ~
               at (10,21), "*                                       *",  ~
               at (11,21), "* The Total Amount of the Journal Entry *",  ~
               at (12,21), "* is not zero.  Press (ENTER) to return *",  ~
               at (13,21), "* to EDIT MODE.                         *",  ~
               at (14,21), "*****************************************",  ~
               at (09,35), fac(hex(94)), temp$                  , ch(13),~
                                                                         ~
               at (21,02), fac(hex(ac)), blankline$             , ch(79),~
               at (22,02),                                               ~
                  "(ENTER)RETURN TO EDIT MODE",                          ~
               at (23,55),                                               ~
                  "(15)Print screen",                                    ~
               at (24,55), fac(hex(8c)), pf16$                  , ch(23),~
                                                                         ~
               keys(pfkey$),                                             ~
               key (keyhit%)

               if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

        REM *************************************************************~
            *              T E S T   H E A D E R   D A T A              *~
            * --------------------------------------------------------- *~
            * HEADER DATA TEST, INCLUDING WHETHER OR NOT THE JOURNAL    *~
            * ENTRY NAME IS OUT IN THE BUFFER OR NOT.                   *~
            *************************************************************

            deffn'150(fieldnr%)
                  errormsg$, infomsg$ = " "
                  on fieldnr% gosub L50150,         /* ENTRY NAME       */~
                                    L50250,         /* DESCRIPTION      */~
                                    L50290,         /* REF TEXT OPTION  */~
                                    L50290,         /* REF TEXT OPTION  */~
                                    L50290,         /* REF TEXT OPTION  */~
                                    L50300,         /* POSTING DATE     */~
                                    L50610          /* JOURNAL TO POST  */
                  return

L50150:     REM TEST TO SEE THAT THE JOURNAL ENTRY IS NOT ON FILE.
                if name$ <> " " then goto L50210
                     readkey$ = userid$
                     call "PLOWCODE" (#09, readkey$, " ",3%,1.36, f1%(9))
                     if f1%(9) <> 0 then L50201
                     errormsg$ = hex(00)
                     return
L50201:           name$ = str(readkey$,4,10)
L50210:         gosub L30000
                if oldentryonfile% = 0 then return
                   return clear all
                   goto editmode
L50250:     REM TEST DESCRIPTION FOR OK VALUE.
                if description$ <> " " then return
                errormsg$ = "Please describe these entries"
                return

L50290:     REM TEST REFERENCE TEXT OPTION
                if refinpopt$ = "Y" or refinpopt$ = "N" then return
                     errormsg$ = "Enter 'Y' -or- 'N'."
                     return

L50300:     REM TEST POSTING DATE
                call "DATEOK" (postdate$, u3%, errormsg$)
                if errormsg$ <> " " then return
                init (" ") postdescr$
                if module$ = "99" then return

*       ****** FIGURE OUT WHICH PERIOD WE'RE TRYING TO POST TO *******
                call "DATUNFMT" (postdate$)
                temp% = 17
                for u3% = 1 to 17
                  if u$(u3%) = " " or u$(u3%) = blankdate$ then L50430
                     if postdate$ < u$(u3%) then temp% = u3% - 1
                     if postdate$ < u$(u3%) then L50440
L50430:         next u3%
L50440:         if temp% = 0 then L50490
                                  /* Post to Last Period, not Closing */
            if temp% = 13% and periods% = 12% then temp% = 12%

                tdate$ = u$(temp%)
                call "DATEFMT" (tdate$)
                put postdescr$ using L50470, temp%, tdate$
L50470:         %Period (##),  Beginning ########
                goto L50530
L50490:             sdate$ = u$(1)
                    call "DATEFMT" (sdate$)
                    errormsg$ = "Date Must Be On Or After: "&sdate$

L50530:         call "DATEFMT" (postdate$)
                gldate$ = postdate$
                call "DATUNFMT" (gldate$)
                if any_date% = 1% then return
                call "WHICHPER" (#03, gldate$, thismonth%)
                     if thismonth% = 0  then errormsg$ = "Date Is Outside~
        ~ The Posting Window."
                return
L50610:     REM TEST JOURNAL NUMBER
                fc2$() = all (hex(8c))
                convert module$ to j%, data goto L50710
                convert j% to module$, pic(00)
                search module_name$() = module$ to cursor%() step 35
                if cursor%(1) = 0 then L50710
                element% = (cursor%(1)+34)/ 35
                fc2$(element%) = hex(84)
                mod_descr$ = str(module_name$(element%),4)
                return
L50710:         errormsg$ = "Invalid entry for Journal - see list below a~
        ~nd re-enter"
                return

        REM *************************************************************~
            *      T E S T   D A T A   F O R   L I N E   I T E M S      *~
            * --------------------------------------------------------- *~
            * TESTS DATA FOR THE ACCOUNT ON FILE, AND THAT SORT OF THING*~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$, infomsg$ = " "
                  c% = currentline%
                  on fieldnr% gosub L51150,         /* ACCOUNT NUMBER   */~
                                    L51200,         /* DESCRIPTION      */~
                                    L51250,         /* DEBIT AMOUNT     */~
                                    L51290,         /* CREDIT AMOUNT    */~
                                    L51400          /* REFERENCE TEXT   */
                  return

L51150:     REM TEST FOR ACCOUNT ON FILE. RETURN IF DESCRIPTION ONLY LINE
                descr$(c%,2) = " "
                call"GETCODE"(#02, acct$(c%), descr$(c%,2), 0%, 0, f1%(2))
                str(infomsg$, 30) = "(" & descr$(c%,2) & ")"
                if f1%(2) = 0 then                                       ~
                     errormsg$="Account not on file: " & acct$(c%)
                return
L51200:     REM HAVE FUN WITH THE DESCRIPTION
                if descr$(c%,1) <> " " then return
                errormsg$ = "Sorry, the description may not be blank"
                return

L51250
*        Test DEBIT AMOUNT for valid number.
            if debit$(c%) = " " then debit$(c%) = "0"
            call "NUMTEST"(debit$(c%), -9e7, 9e7, errormsg$, -2.2, debit)
            if debit = 0 then debit$(c%) = " "
            return

L51290
*        Check out the CREDIT AMOUNT
            if credit$(c%) = " " then credit$(c%) = "0"
            call "NUMTEST" (credit$(c%),-9e7,9e7,errormsg$, -2.2, credit)
            if credit = 0 then credit$(c%) = " "
            return

L51400:     REM TEST REFERENCE TEST
                return

        REM *************************************************************~
            *         T E S T  D A T A  F O R  D E L E T E  E N T R Y   *~
            * --------------------------------------------------------- *~
            * TESTS ENTRY TO SEE IF IN BUFFER FILE                      *~
            *************************************************************

            deffn'154(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L53120,         /* NAME$            */~
                                    L53240          /* EDITMODE         */
            return

L53120: REM TEST FOR NAME$ IN BUFFER FILE
            if name$ = " " then L53210
            call "READ100" (#09, name$, f1%(9))
                  if f1%(9) = 0 then L53210
            get #09 using L53170, ruserid$, oldjnlid$
L53170:         FMT CH(3),XX(54),CH(03)
            if ruserid$ = userid$ then L53190
               errormsg$ = "Journal in use by " & ruserid$
               return
L53190:     if oldjnlid$ = jnlid$ then return
               errormsg$ = "Invalid Journal for " & jnlid$
               errormsg$ = errormsg$ & ": " & oldjnlid$
               return

L53210:     errormsg$ = "Journal Entry not in Buffer file"
            return

L53240: REM TEST FOR EDITMODE
            return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * CLOSES ALL OPEN FILES, DISPLAYS MESSAGE, AND SEES IF ANY  *~
            * DOCUMENTS ARE IN THE BUFFER FOR THIS USER, GENERATING AN  *~
            * APPROPRIATE RETURN CODE IF THERE ARE.                     *~
            *************************************************************

            REM NOW SET FLAG TO SEE IF ANYTHING THIS USER IN BUFFER.
                readkey$ = userid$
L65120:         call "PLOWALTS" (#09, readkey$, 1%, 3%, f1%(9))
                     if f1%(9) = 0 then L65180
                get #09, using L65150, oldjnlid$
L65150:         FMT POS(58), CH(3)
                if oldjnlid$ <> jnlid$ then L65120

L65180:     end f1%(9)
