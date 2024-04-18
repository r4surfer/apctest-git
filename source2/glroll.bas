        REM *************************************************************~
            *                                                           *~
            *   GGG   L      RRRR    OOO   L      L                     *~
            *  G   G  L      R   R  O   O  L      L                     *~
            *  G      L      RRRR   O   O  L      L                     *~
            *  G  GG  L      R  R   O   O  L      L                     *~
            *   GGG   LLLLL  R   R   OOO   LLLLL  LLLLL                 *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLROLL   - CLONED FROM MEGANUKE, ONLY THE GL DETAIL       *~
            *            PURGE IS HERE THOUGH.  ASLO ROLLS THE GL       *~
            *            PERIOD OPEN.                                   *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/15/83 ! ORIGINAL  (CLONED)                       ! HES *~
            * 07/12/84 ! CHANGED LITERAL CLOSE TO ROLL            ! BLT *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION - ! RAC *~
            *          ! CHANGES INCLUDED GLPOST, HNYPOST, JNLINFO!     *~
            *          !(NEW),GLPRTSUB(NEW), INCREASE IN GL BUFFER!     *~
            *          ! INTERFACE FILE RECORD SIZE FOR PASSING   !     *~
            *          ! MODULE, JOURNAL, AND POSTING SEQUENCE    !     *~
            * 03/04/86 ! Change for unformatted Fiscal Dates      ! ERN *~
            * 08/03/87 ! Changed screen text to indicate that data!     *~
            *          ! including date entered will be purged.   ! DAW *~
            * 09/17/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 08/10/88 ! Dual Books depends on a SYSFILE2 flag.   ! JIM *~
            * 11/08/88 ! Now purges both books if 'dual books' ON.! JDH *~
            * 12/09/92 ! PRR 12678  Corrected exits.              ! JDH *~
            *          ! PRR 12431  Corrected PURDATE$ update.    !     *~
            *          ! PRR 11280  No Longer Blanks Pur Rcvr Date!     *~
            * 07/15/97 ! Changes for the year 2000.               ! DXL *~
            * 09/09/97 ! Changed "Roll & Purge" conditionally.    ! JDH *~
            *************************************************************

        dim                                                              ~
            askmsg$40,                   /* ASKUSER Message            */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            cutoffdate$8,                /* CUTOFF DATE FOR THIS MODULE*/~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dates$(17)8,                 /* FISCAL DATE STRUCTURE      */~
            dateposted$8,                /* DATE POSTED THIS RECORD    */~
            dual_books$1,                /* Dual books in effect?      */~
            edates$(32)8,                /* PERIOD ENDIND DATES        */~
            edtmessage$79,               /* MESSAGE FOR EDIT MODE      */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            fac$(20)1,                   /* FIELD ATTRIBUTE CHARACTERS */~
            fiscalkey$20,                                                ~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED)    */~
            inpmessage$79,               /* INPUT SCREEN MESSAGE       */~
            lastmain$9,                  /* WORK VARIABLE              */~
            last_purged$8,               /* Formatted Last Purged Date */~
            lastpurkey$20,                                               ~
            line2$79,                                                    ~
            mindate$8, mindisp$8,        /* LATEST PURGE DATE ALLOWED  */~
            months$36,                   /*                            */~
            oldpur$(7)8,                 /* LAST PURGE DATE USED       */~
            pf16fac$1, pf16key$1, pf16msg$16,                            ~
            purdate$8,                   /* PURGE DATES TO USE THIS RUN*/~
            readkey$50,                  /* KEY TO READ DISK WITH      */~
            tdate$8,                     /* Temporary Date Variable    */~
            udate$8,                     /* Temporary Date Variable    */~
            warnmsg$79                   /* WARNING FOR SCREEN         */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            fs%(64),                     /* FILE OPEN FLAGS            */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20                  /* RETURN CODE FROM "FILEOPEN"*/

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
            *                    S E L E C T   F I L E S                *~
            *                                                           *~
            * FILES ARE OPENED AS THEY ARE NEEDED TO SPEED UP SYSTEM.   *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * #01 ! SYSFILE2 ! USED TO FIND THE OPEN MONTHS             *~
            * #11 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            * #12 ! GLDETAIL ! GENERAL LEDGER DETAIL ITEM FILE          *~
            * #21 ! GLMAIN2  ! G. L. chart of accounts for local auth.  *~
            * #22 ! GLDETAL2 ! G. L. detail records for local authority *~
            *************************************************************

            select  #01, "SYSFILE2",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #11, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select #12, "GLDETAIL",      /* GENERAL LEDGER DETAILS     */~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 26

            select #21, "GLMAIN2",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            select #22, "GLDETAL2",                                      ~
                        varc,     indexed,  recsize = 160,               ~
                        keypos = 1,    keylen = 26

            call "SHOSTAT" ("Opening files, one moment please")

            call "OPENCHCK" (#01%, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (#11%, fs%(11), f2%(11), 0%, rslt$(11))
            call "OPENCHCK" (#12%, fs%(12), f2%(12), 0%, rslt$(12))
            dual_books$ = "N"                        /* Default to 'no' */
            call "READ100" (#01, "SWITCHS.GL", f1%(1))
                if f1%(1) = 0% then goto L09000
            get #01 using L03771, dual_books$
L03771:         FMT POS(21), CH(1)
            if dual_books$ <> "Y" then goto L09000
                call "OPENCHCK" (#21%, fs%(21), f2%(21), 0%, rslt$(21))
                call "OPENCHCK" (#22%, fs%(22), f2%(22), 0%, rslt$(22))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES VARIABLES FOR SCREEN INPUT STUFF.             *~
            * INITIALIZATION FOR THE PURGE ROUTINES THEMSELVES IS       *~
            * HANDLED IN THE INDIVIDUAL PURGE ROUTINES.                 *~
            *************************************************************

            inpmessage$ = "To bypass purging of detail, leave the PURGE D~
        ~ate blank."

           edtmessage$="To modify displayed data, position cursor to de"&~
                "sired line and press (RETURN)."

            months$ = "JanFebMarAprMayJunJulAugSepOctNovDec"

            date$ = date
            call "DATEFMT" (date$)
            lastpurkey$ = "LAST PURGE DATES"
            fiscalkey$  = "FISCAL DATES"

            call "READ100" (#01, fiscalkey$, f1%(1))
                 if f1%(1) = 0 then L64000
            get #01, using L09220, dates$(), monthopen%, edates$(), adjacct$
L09220:             FMT XX(22), 17*CH(8), BI(2), 32*CH(8), CH(16)
            if monthopen% = 16 then warnmsg$ = "WARNING: The Year End Clo~
        ~sing MUST be performed during this new period!"

            if monthopen% = 17 then L64000

            t% = 1
L09290:     t% = t% + 1
            temp$, mindate$ = edates$((monthopen%+15)-t%)
            if mindate$ = " " or mindate$ = "CLOSING" then L09290
            mindisp$ = mindate$ : call "DATEFMT" (mindisp$)
	    call "DATEOK" (temp$, mindate%, errormsg$)
            if mindate% = 0 then L64000

            currentmonth$ = edates$(monthopen%+15)
            tdate$ = currentmonth$
            call "DATEFMT" ( tdate$, 0%, udate$ )
            convert str(udate$,5%,2%)to m%, data goto L64000
            m1% = m%*3%-2%
            temp$ = currentmonth$
            currentmonth$ = str(months$,m1%,3%) & " " &                   ~
                    str(udate$,7%,2%) & ", " & str(udate$,1%,4%)

            t% = 0
L09430:     t% = t% + 1
            if dates$(monthopen%+t%) = " " then L09430

            REM NOW SET RECORD AREA FOR #01 TO THE LAST DATES USED.
                call "READ100" (#01, lastpurkey$, f1%(1%))
                if f1%(1%) = 0% then L10000
                    get #01, using L09510, oldpur$()
L09510:                 FMT XX(20), 7*CH(6)
                     last_purged$ = oldpur$(6)
                     if last_purged$ <> " " then                         ~
                          call "DATEFMT" (last_purged$)

L10000: REM *************************************************************~
            *             I N P U T   P U R G E   D A T E S             *~
            *                                                           *~
            * INPUTS PURGE DATES FOR DETERMINING WHEN TO PURGE.  NOTE   *~
            * THAT A BLANK PURGE DATE SIGNIFIES THAT WE WILL NOT PURGE  *~
            * THIS FILE AT THIS TIME.                                   *~
            *************************************************************

            init(" ") errormsg$, purdate$
            pf16msg$="(16)EXIT PROGRAM":pf16fac$=hex(84):pf16key$=hex(10)
            main% = 11% : detl% = 12% /* Set defaults */

            for fieldnr% = 1% to 1%
                gosub'161(fieldnr%)
                    if enabled% = 0 then L10200
L10140:         gosub'201(fieldnr%)
                    if keyhit%  =  1 then gosub startover
                    if keyhit%  = 16 then exit_without_msg
                    if keyhit% <>  0 then L10140
                gosub'151(fieldnr%)
                    if errormsg$ <> " " then L10140
L10200:     next fieldnr%

        REM *************************************************************~
            *              E D I T   P U R G E   D A T E S              *~
            *                                                           *~
            * EDITS THE PURGE INFORMATION ALREADY INPUT.                *~
            *************************************************************

L11051:     pf16fac$=hex(84):pf16key$=hex(10)
            if purdate$ = " "  then pf16msg$ = "(16)ROLL"                ~
                               else pf16msg$ = "(16)ROLL & PURGE"
            gosub'201(0%)
                if keyhit%  =  1 then gosub startover
                if keyhit%  = 16 then       L19000
                if keyhit% <>  0 then       L11051
            fieldnr% = 0%
            if cursor%(1) =  7% then fieldnr% = 1%
            if fieldnr% = 0 then L11051
            pf16fac$=hex(9c) : pf16key$=hex(ff)
            gosub'161(fieldnr%)
                if enabled% = 0 then L11160
L11130:     gosub'201(fieldnr%)
                if keyhit%  =  1 then gosub startover
                if keyhit% <>  0 then       L11130
L11160:     gosub'151(fieldnr%)
                if errormsg$ <> " " then L11130
            goto L11051

L19000: REM *************************************************************~
            *     S H O W   L A S T   C H A N C E S   T H E N   G O     *~
            *************************************************************

            if purdate$ = " "  then                                      ~
                        askmsg$="Press PF(16) to Roll this G/L set"      ~
                               else                                      ~
                        askmsg$="Press PF(16) to Roll/Purge this G/L set"
            u3% = 2% /* Window at bottom */
            call "ASKUSER" (u3%, "*** VERIFY YOUR ENTRIES ***",          ~
                "PURGING is about to begin -- verify your input, above", ~
                 askmsg$,                                                ~
                "Press PF(1) to return to input/edit of fields")
            if u3%  =  1% then goto L10000
            if u3% <> 16% then goto L19000

            call "SHOSTAT" ("Roll/Purge in process ...")

        REM LOOP THAT CONTROLS WHAT GETS PURGED, ALSO UNFORMATS DATES
            if purdate$ = " " then L19220
                call "DATUNFMT" (purdate$)
                cutoffdate$ = purdate$
                oldpur$(6) = purdate$
                gosub L35000

L19220: REM NOW (RE)WRITE UNFORMATTED DATES TO THE SYSFILE2 FILE.
            call "READ101" (#01, lastpurkey$, f1%(1))
            put #01, using L19280, lastpurkey$, oldpur$(), " ", " "
L19280:         FMT CH(20), 7*CH(6), CH(250), CH(188)

            if f1%(1) = 0 then write #01 else rewrite #01

            call "READ101" (#01, fiscalkey$, f1%(1))

            get #01, using L19350, periods%, dates$(), monthopen%,        ~
                edates$(), adjacct$
L19350:         FMT XX(20), BI(2), 17*CH(8), BI(2), 32*CH(8), CH(16)
L19360:     monthopen% = monthopen% + 1
            if monthopen% = 13 and periods% = 12 then L19360
            rewrite #01, using L19400, fiscalkey$, periods%, dates$(),    ~
                monthopen%, edates$(), adjacct$, " "
L19400:         FMT CH(20),BI(2),17*CH(8),BI(2),32*CH(8),CH(16),CH(68)
            goto L65000

        REM *************************************************************~
            *   D E F A U L T / E N A B L E   I N P U T   F I E L D S   *~
            *                                                           *~
            * SETS DEFAULT DATES FOR THE FILES, AND LOADS FROM THE      *~
            * SYSFILE2 FILE THE DATE THAT FILE WAS LAST PURGED.         *~
            *************************************************************

        deffn'161(fieldnr%)
            enabled% = 0
            on fieldnr% gosub L20600
            return

L20600: REM DEFAULT/ENABLE FOR PURGE GENERAL LEDGER DETAILS
            enabled%  = 1
            inpmessage$ = "Enter PURGE date per above instructions."
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
            ask% = 2%
            call "STARTOVR" (ask%)
            if ask% = 1% then return
            return clear all
            goto L10000

L35000: REM *************************************************************~
            *  P U R G E   G E N E R A L   L E D G E R   D E T A I L S  *~
            *                                                           *~
            * PURGES GENERAL LEDGER DETAILS POSTED BEFORE THE GENERAL   *~
            * LEDGER DATE.                                              *~
            *************************************************************

            call "SHOSTAT"  ("Purging Old General Ledger Details")
L35080:     readkey$ = all(hex(00))
            lastmain$ = all(hex(00))

        next_item
        REM PLOW THROUGH LOOKING FOR GENERAL LEDGER DETAILS TO KILL.
            call "PLOWNEXT" (#detl%, readkey$, 0%, f1%(detl%))
                if f1%(detl%) = 0 then L35320
            get #detl%, using L35160, dateposted$, jtype$
L35160:         FMT XX(16), CH(6), XX(4), CH(2)
            if jtype$ = "99" then L35230 /* SKIP CLOSING ENTRIES */
            if dateposted$ > cutoffdate$ then L35230
            call "READ101" (#detl%, readkey$, f1%(detl%))
            if f1%(detl%) = 1 then delete #detl%
            goto next_item

L35230: REM CHECK FOR (AND KILL) ORPHANS.  (My favorite passtime)
            if lastmain$ = str(readkey$,,9) then next_item
            lastmain$ = str(readkey$,,9)
            call "READ100"  (#main%, lastmain$, f1%(main%))
                if f1%(main%) <> 0 then L35300
            str(readkey$,17) = all(hex(00))
            call "DELETE" (#detl%, readkey$, 9%)
L35300:     goto next_item

L35320:     if dual_books$ <> "Y" then return
            if main% = 21% then return        /* Let's not do it again */
            main% = 21% : detl% = 22%         /* Set Local Auth. books */
            goto L35080

        REM *************************************************************~
            *      I N P U T   P U R G E   D A T E S   S C R E E N      *~
            *                                                           *~
            * INPUTS THE DESIRED PURGE DATE ONTO THE SCREEN.            *~
            *************************************************************

        deffn'201(fieldnr%)
            init(hex(8c)) fac$()
            if fieldnr% <> 0% then goto L40120
                inpmessage$ = edtmessage$
                init (hex(84)) fac$()
                goto L40250
L40120:     on fieldnr% gosub L40180          /* GENERAL LEDGER   */
            goto L40250

            REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                fac$(fieldnr%) = hex(80)
                return
L40180:     REM SET FAC'S FOR UPPER CASE ONLY INPUT
                fac$(fieldnr%) = hex(81)
                return
            REM SET FAC'S FOR NUMERIC ONLY INPUT
                fac$(fieldnr%) = hex(82)
                return

L40250:     line2$ = "For the period ending " & currentmonth$
            str(line2$,64) = "GLROLL: " & str(cms2v$,,8)
L40270:     accept                                                       ~
               at (01,02), "G/L Period Rolling (and PURGE)",             ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,06), fac(hex(84)), warnmsg$               , ch(71),~
               at (07,09), "PURGE Date:",                                ~
               at (07,21), fac(fac$( 1)), purdate$              , ch(08),~
               at (07,40), "Last Purge Date:",                           ~
               at (07,57), fac(hex(ac)), last_purged$           , ch(08),~
                                                                         ~
               at (09,02),                                               ~
        "All detail records posted prior to and INCLUDING the above date ~
        ~will be PURGED!",                                                ~
               at (10,02),                                               ~
        "You may change this date if you wish. However, the date must be ~
        ~on or before:",                                                  ~
               at (11,37), fac(hex(8c)), mindisp$               , ch(08),~
               at (13,02),                                               ~
        "Purging of detail records has NO effect on account balances.",  ~
               at (14,02),                                               ~
         "If you don't want ANY detail purged, enter a blank date.",     ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(pf16fac$), pf16msg$              , ch(16),~
                                                                         ~
               keys(hex(00010d0f) & pf16key$), key (keyhit%)

               if keyhit% <> 13 then L40630
                  call "MANUAL" ("GLROLL")
                  goto L40270

L40630:        if keyhit% <> 15 then goto L40670
                  call "PRNTSCRN"
                  goto L40270

L40670:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TEST PURGE DATES AGAINST LATEST DATE, AND MAKE SURE THEY  *~
            * ARE O.K. SYNTACTICALLY.  LEAVE BLANKS ALONE; THEY ARE THE *~
            * SIGNAL TO NOT PURGE SOMETHING.                            *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50220
            return

L50220: REM TEST DATA FOR PURGE GENERAL LEDGER DETAILS
            if purdate$ = " " then return
                call "DATEOK" (purdate$, date%, errormsg$)
                if errormsg$ <> " " then return
                    if date% <= mindate% then return
                    errormsg$ = "Date for PURGE must be on or before " & ~
                        mindisp$ & "."
                    return

L64000: REM *************************************************************~
            *        C L O S I N G   A B O R T E D  M E S S A G E       *~
            *                                                           *~
            * TELLS/WARNS USER THAT MONTH END CLOSING HAS NOT BEEN DONE.*~
            *************************************************************

            u3% = 0%
            call "ASKUSER" (u3%, "*** FISCAL DATE ERROR ***",            ~
                "WARNING: G/L Closing can't be run. The fiscal date str"&~
                "ucture is INVALID.", "The current month is NOT closed.",~
                "Press (RETURN) to continue.")

        exit_without_msg
            call "SHOSTAT" (hex(94) & "CLOSING ABORTED")
            end 99

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("G/L Period-Ending file updates completed")
            end
