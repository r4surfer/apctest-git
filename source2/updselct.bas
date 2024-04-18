        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  U   U  PPPP   DDDD    SSS   EEEEE  L       CCC   TTTTT   *~
            *  U   U  P   P  D   D  S      E      L      C        T     *~
            *  U   U  PPPP   D   D   SSS   EEEE   L      C        T     *~
            *  U   U  P      D   D      S  E      L      C        T     *~
            *   UUU   P      DDDD    SSS   EEEEE  LLLLL   CCC     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * UPDSELCT - Allows specification of which sessions are to  *~
            *            be included in this update and marks those     *~
            *            session records with the User's Logon ID.      *~
            *            Program returns the following status codes-    *~
            *            0- No Update; 1- Update in foreground;         *~
            *            2- Update in background; 3- Restarting (update *~
            *            in foreground assumed.                         *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/22/86 ! Original                                 ! ERN *~
            * 05/08/91 ! PRR 11762  Fixed a Bad Branch            ! SID *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            errormsg$79,                 /* Error message              */~
            hdr1$6, hdr2$20, hdr3$8,     /* Summary Screen Headings    */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(100)1,                 /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            pf1$79, pf2$79, pf$(20)1,    /* PF Descriptors and Keys    */~
            plowkey$50,                  /* Multipurpose Plow Key      */~
            sfac$(100)1,                 /* Selection FACs             */~
            slct$(100)1,                 /* Sessions selected          */~
            status$1,                    /* Session Status             */~
            summary$(100)37,             /* Lines for Summary Screen   */~
            upd$8,                       /* Update Program Controlled  */~
            upddescr$30,                 /* Update Description         */~
            userid$3                     /* Current User Id            */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
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
            * #1  ! UPDSESSN ! Update Session Control File              *~
            *************************************************************~

            select #1,  "UPDSESSN",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =  4,   keylen = 17,                      ~
                        alt key  1, keypos =     1, keylen =  20

            call "OPENCHCK" (#1,  fs%(1), f2%(1), 100%, rslt$(1))


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date  :  call "DATEFMT" (date$)

            inpmessage$  = "Place a non-blank character next to Sessio" &~
                           "n(s) to Update."

*        Define Headings for Summary Screen
            hdr1$ = "  ID"
            hdr2$ = "Session Description"
            hdr3$ = "  Date"

*        Determine maximum number of sessions allowed.
            maxsessions% = dim(summary$(), 1)

*        Define what Update is being controlled here
            call "GETPARM" addr ("I ", "R", "UPDSELCT", " ", "0001",     ~
                                 "SELECT", "For Which Update?", 17%,     ~
                                 "K", "UPDATE  ", upd$, 8%, 0%, 0%, "A")

            if upd$ = "ARIUPDTE" then upddescr$ = "Invoice Update"
            if upd$ = "CRCUPDTE" then                                    ~
                             upddescr$ = "Cash Receipts Management      "
            if upddescr$ = " " then L65000

*        Now check to see that there are no sessions queued for this
*        User/Update.  If there are, we must assume a restart condition
*        exists.
            plowkey$ = str(userid$) & str(upd$)
            call "PLOWALTS" (#1, plowkey$, 1%, 9%, f1%(1))
            if f1%(1) = 0% then L09430
L09350:         ret% = 0%
                call "ASKUSER" (ret%, "UPDATE CONTROL",                  ~
                 "You already have sessions in-process for this update.",~
                 "Enter RETURN to EXIT without restarting the update;  ",~
                 "-OR- Enter PF-3 to RESTART the update in foreground.")
                if ret% = 0% or ret% = 3% then L65000 else goto L09350


L09430
*        Now make sure that User is not running another update.
            plowkey$ = userid$ & hex(00)
            call "PLOWALTS" (#1, plowkey$, 1%, 3%, f1%(1))
            if f1%(1) = 0% then L10000
                ret% = 0%
                call "ASKUSER" (ret%, "UPDATE CONTROL",                  ~
                 "You already have another update in-process.          ",~
                 "Complete it before running another update.           ",~
                 "Press RETURN to exit."                                )
                ret% = 0%
                goto L65000

L10000: REM *************************************************************~
            *         S E L E C T I O N   S C R E E N                   *~
            *-----------------------------------------------------------*~
            * Shows sessions available for update and allows selection. *~
            *************************************************************

        summary_screen
            init(" ") errormsg$
            ret% = 0%
            gosub load_summary
            if summary% > 0% then L10150
                call "ASKUSER" (0%, "UPDATE CONTROL",                    ~
                     "There are no sessions available to update.",       ~
                     "Press any PF Key to Exit", " " )
                goto L65000

L10150:   gosub'101
            if keyhit%  =  1 then gosub startover
            if keyhit%  =  2 then top% = 0%
            if keyhit%  =  3 then top% = max(0%,((summary%-1%)/30%)*30%)
            if keyhit%  =  4 then top% = max(0%,top%-30%)
            if keyhit%  =  5 then top% = min(top%+30%, summary%)
            if keyhit%  =  6 then top% = max(0%,top%-1%)
            if keyhit%  =  7 then top% = min(top%+1%,max(0%,summary%-30%))
            if keyhit%  <  8 then errormsg$ = " "

            if keyhit%  = 16 then exit_program
            if keyhit% <>  9 then L10150

*        Test that one or more sessions have been selected. If so,
*        away we go.
            gosub'152
                if errormsg$ <> " " then L10150

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Flags sessions for update and sets up for running.        *~
            *************************************************************

            gosub dataput

*          CALL "ASKUSER" (RET%, "Mode of Update",                      ~
*                          "Enter PF-1 to Update in Foreground,",       ~
*                          "      PF-2 to Update in Background.", " ")
*          IF RET% = 1% OR RET% = 2% THEN 65000 ELSE 19090
          ret% = 1%
          goto L65000

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
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
            goto summary_screen

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        load_summary
            summary%, top% = 0%
            init(" ") summary$(), slct$()
            plowkey$ = str(upd$) & hex(00)

L30100:     call "PLOWALTS" (#1, plowkey$, 0%, 8%, f1%(1))
            if f1%(1) = 0% then return
                get #1 using L30130, temp$, status$
L30130:              FMT CH(3), XX(43), CH(1)
                if temp$ = hex(ffffff) then L30100
                if status$ <> "C"      then L30100
                s%, summary% = summary% + 1%
                get #1 using L30200,                                      ~
                          str(summary$(s%), 1, 6),       /* Session    */~
                          str(summary$(s%), 8,20),       /* Descriptn  */~
                          str(summary$(s%),29, 6)        /* Post Date  */
L30200:              FMT XX(11), CH(6), XX(3), CH(20), CH(6)
                call "DATEFMT" (str(summary$(s%),29, 8))
                goto L30100


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            for i% = 1% to summary%
                if slct$(i%) = " " then L31140
                     plowkey$ = str(upd$) & str(summary$(i%),,6)
                     call "READ101" (#1, plowkey$, f1%(1))
                     if f1%(1) = 0% then L31140
                     put #1 using L31120, userid$, "U"
L31120:                   FMT CH(3), POS(47), CH(1)
                     rewrite #1
L31140:     next i%

            return


        REM *************************************************************~
            *               S U M M A R Y   S C R E E N                 *~
            *-----------------------------------------------------------*~
            * Summary Screen.                                           *~
            *************************************************************

        deffn'101
            line2$ = "  ** SELECT SESSIONS FOR UPDATE **"
            str(line2$,58) = "    UPDSELCT: " & cms2v$
            init(hex(81)) sfac$()  : init(hex(8c)) lfac$()
            if summary% <  maxsessions% then                             ~
                init(hex(8c)) str(sfac$(), summary%+1%)
            for i% = 1% to summary%
                if slct$(i%) <> " " then lfac$(i%) = hex(84)
            next i%
            gosub setpf_summary


L40180:     accept                                                       ~
               at (01,02), "Session Management for ",                    ~
               at (01,25), fac(hex(84)), upddescr$              , ch(30),~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,04), "Session                     Posting",        ~
               at (04,44), "Session                     Posting",        ~
               at (05,04), fac(hex(ac)), hdr1$,                          ~
               at (05,11), fac(hex(ac)), hdr2$,                          ~
               at (05,32), fac(hex(ac)), hdr3$,                          ~
               at (05,44), fac(hex(ac)), hdr1$,                          ~
               at (05,51), fac(hex(ac)), hdr2$,                          ~
               at (05,72), fac(hex(ac)), hdr3$,                          ~
                                                                         ~
               at (06,02), fac(sfac$(top% + 1%)),slct$(top% + 1%),ch(01),~
               at (07,02), fac(sfac$(top% + 2%)),slct$(top% + 2%),ch(01),~
               at (08,02), fac(sfac$(top% + 3%)),slct$(top% + 3%),ch(01),~
               at (09,02), fac(sfac$(top% + 4%)),slct$(top% + 4%),ch(01),~
               at (10,02), fac(sfac$(top% + 5%)),slct$(top% + 5%),ch(01),~
               at (11,02), fac(sfac$(top% + 6%)),slct$(top% + 6%),ch(01),~
               at (12,02), fac(sfac$(top% + 7%)),slct$(top% + 7%),ch(01),~
               at (13,02), fac(sfac$(top% + 8%)),slct$(top% + 8%),ch(01),~
               at (14,02), fac(sfac$(top% + 9%)),slct$(top% + 9%),ch(01),~
               at (15,02), fac(sfac$(top% +10%)),slct$(top% +10%),ch(01),~
               at (16,02), fac(sfac$(top% +11%)),slct$(top% +11%),ch(01),~
               at (17,02), fac(sfac$(top% +12%)),slct$(top% +12%),ch(01),~
               at (18,02), fac(sfac$(top% +13%)),slct$(top% +13%),ch(01),~
               at (19,02), fac(sfac$(top% +14%)),slct$(top% +14%),ch(01),~
               at (20,02), fac(sfac$(top% +15%)),slct$(top% +15%),ch(01),~
               at (06,42), fac(sfac$(top% +16%)),slct$(top% +16%),ch(01),~
               at (07,42), fac(sfac$(top% +17%)),slct$(top% +17%),ch(01),~
               at (08,42), fac(sfac$(top% +18%)),slct$(top% +18%),ch(01),~
               at (09,42), fac(sfac$(top% +19%)),slct$(top% +19%),ch(01),~
               at (10,42), fac(sfac$(top% +20%)),slct$(top% +20%),ch(01),~
               at (11,42), fac(sfac$(top% +21%)),slct$(top% +21%),ch(01),~
               at (12,42), fac(sfac$(top% +22%)),slct$(top% +22%),ch(01),~
               at (13,42), fac(sfac$(top% +23%)),slct$(top% +23%),ch(01),~
               at (14,42), fac(sfac$(top% +24%)),slct$(top% +24%),ch(01),~
               at (15,42), fac(sfac$(top% +25%)),slct$(top% +25%),ch(01),~
               at (16,42), fac(sfac$(top% +26%)),slct$(top% +26%),ch(01),~
               at (17,42), fac(sfac$(top% +27%)),slct$(top% +27%),ch(01),~
               at (18,42), fac(sfac$(top% +28%)),slct$(top% +28%),ch(01),~
               at (19,42), fac(sfac$(top% +29%)),slct$(top% +29%),ch(01),~
               at (20,42), fac(sfac$(top% +30%)),slct$(top% +30%),ch(01),~
                                                                         ~
               at (06,04), fac(lfac$(top% + 1%)),  summary$(top% +  1%), ~
               at (07,04), fac(lfac$(top% + 2%)),  summary$(top% +  2%), ~
               at (08,04), fac(lfac$(top% + 3%)),  summary$(top% +  3%), ~
               at (09,04), fac(lfac$(top% + 4%)),  summary$(top% +  4%), ~
               at (10,04), fac(lfac$(top% + 5%)),  summary$(top% +  5%), ~
               at (11,04), fac(lfac$(top% + 6%)),  summary$(top% +  6%), ~
               at (12,04), fac(lfac$(top% + 7%)),  summary$(top% +  7%), ~
               at (13,04), fac(lfac$(top% + 8%)),  summary$(top% +  8%), ~
               at (14,04), fac(lfac$(top% + 9%)),  summary$(top% +  9%), ~
               at (15,04), fac(lfac$(top% +10%)),  summary$(top% + 10%), ~
               at (16,04), fac(lfac$(top% +11%)),  summary$(top% + 11%), ~
               at (17,04), fac(lfac$(top% +12%)),  summary$(top% + 12%), ~
               at (18,04), fac(lfac$(top% +13%)),  summary$(top% + 13%), ~
               at (19,04), fac(lfac$(top% +14%)),  summary$(top% + 14%), ~
               at (20,04), fac(lfac$(top% +15%)),  summary$(top% + 15%), ~
               at (06,44), fac(lfac$(top% +16%)),  summary$(top% + 16%), ~
               at (07,44), fac(lfac$(top% +17%)),  summary$(top% + 17%), ~
               at (08,44), fac(lfac$(top% +18%)),  summary$(top% + 18%), ~
               at (09,44), fac(lfac$(top% +19%)),  summary$(top% + 19%), ~
               at (10,44), fac(lfac$(top% +20%)),  summary$(top% + 20%), ~
               at (11,44), fac(lfac$(top% +21%)),  summary$(top% + 21%), ~
               at (12,44), fac(lfac$(top% +22%)),  summary$(top% + 22%), ~
               at (13,44), fac(lfac$(top% +23%)),  summary$(top% + 23%), ~
               at (14,44), fac(lfac$(top% +24%)),  summary$(top% + 24%), ~
               at (15,44), fac(lfac$(top% +25%)),  summary$(top% + 25%), ~
               at (16,44), fac(lfac$(top% +26%)),  summary$(top% + 26%), ~
               at (17,44), fac(lfac$(top% +27%)),  summary$(top% + 27%), ~
               at (18,44), fac(lfac$(top% +28%)),  summary$(top% + 28%), ~
               at (19,44), fac(lfac$(top% +29%)),  summary$(top% + 29%), ~
               at (20,44), fac(lfac$(top% +30%)),  summary$(top% + 30%), ~
                                                                         ~
               at (22,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), fac(hex(8c)),   pf1$                 , ch(79),~
               at (24,02), fac(hex(8c)),   pf2$                 , ch(79),~
                                                                         ~
               keys(str(pf$())),                                         ~
               key (keyhit%)

               if keyhit% <> 13 then L41120
                  call "MANUAL" ("UPDSELCT")
                  goto L40180

L41120:        if keyhit% <> 15 then L41160
                  call "PRNTSCRN"
                  goto L40180

L41160:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        setpf_summary
            pf1$ = "(1)Start Over  (2)First (4)Prev (6)Down "   &        ~
                   "                       (15)Print Screen"
            pf2$ = "(9)UPDATE      (3)Last  (5)Next (7)Up   "   &        ~
                   "      (13)Instructions (16)Exit Program"
            str(pf$()) = hex(01020304050607090b0d0e0f1000ffffffffffff)
            if top% <> 0% then L41280
                str(pf1$,16,25) = " "  :  pf$(2), pf$(4), pf$(6) = hex(ff)
L41280:     if top% + 30% < summary% then return
                str(pf2$,16,25) = " "  :  pf$(3), pf$(5), pf$(7) = hex(ff)
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data. (Just make sure a selection has been made).    *~
            *************************************************************

        deffn'152
            errormsg$ = " "

            if str(slct$()) <> " " then return
                errormsg$ = "You must select at least one session to" &  ~
                            " continue with the Update."
                return


L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program

            end  ret%
