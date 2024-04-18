        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  JJJJJ  BBBB   RRRR   EEEEE  L       SSS   U   U  BBBB    *~
            *    J    B   B  R   R  E      L      S      U   U  B   B   *~
            *    J    BBBB   RRRR   EEEE   L       SSS   U   U  BBBB    *~
            *  J J    B   B  R   R  E      L          S  U   U  B   B   *~
            *   J     BBBB   R   R  EEEEE  LLLLL   SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBRELSUB - Review/Release Planned Job Orders.             *~
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
            * 06/06/83 ! ORIGINAL                                 ! KAB *~
            * 10/03/83 ! CHANGED TO SUBROUTINE                    ! HES *~
            * 06/12/84 ! ADDED ALTERNATE KEY PLOW                 ! JUDD*~
            * 07/23/84 ! ADDED FIELDS FOR ORIG. QUANTITY & STD CST! BLT *~
            * 08/30/84 ! ADDED BUYER/PLANNER LINK                 ! DSH *~
            * 08/10/85 ! MODIFIED FOR GLDETAIL FILE CHANGES       ! HES *~
            * 10/11/85 ! Several enhancements (kit, auto JB gen,) ! HES *~
            * 02/04/86 ! Revised 08/30/84 revisions               ! HES *~
            * 03/12/86 ! Added call to CDANPOST.                  ! LDJ *~
            * 10/20/86 ! Changed call to include BOM & ENGMASTRs  ! HES *~
            * 02/04/87 ! Changes to support Serial Number Tracking! LDJ *~
            * 06/10/87 ! HNYMASTR, JBMASTR2, RTEMASTR mods. Added ! JIM *~
            *          !   Back-flush flag, cost arrays, etc. to  !     *~
            *          !   JBMASTR2. Added JOBMASTR & Job/Project !     *~
            *          !   validation.                            !     *~
            * 11/19/87 ! Fix problem if over 16 selected to releas! HES *~
            * 05/10/80 ! Arg List in "JBTIF" at 32080, added 32085! KAB *~
            * 01/19/90 ! Change to honor the behavior switch      ! SID *~
            *          ! setting for Pegging features.            !     *~
            * 09/04/90 ! G/L Export file modifications.           ! RAC *~
            * 12/13/91 ! Corrections to Ctrl # and est. complete. ! JDH *~
            * 12/18/91 ! Made minor changes to how the control    ! SID *~
            *          !   number screen is being display and     !     *~
            *          !   corrected the program of not writting  !     *~
            *          !   the control number to JBMASTR2 file.   !     *~
            * 04/16/92 ! PRR 12354. Move Ctl # after schdler test.! JDH *~
            * 06/11/92 ! Added call to COMPCHCK via PF 9 on       ! WPH *~
            *          !  the first screen listing WO advices.    !     *~
            * 05/15/93 ! Allow Trying to Kit Complete, Even if    ! KAB *~
            *          !  serial Numbered.                        !     *~
            *          ! Search for Part feature added. (if load  !     *~
            *          !   is by part)                            !     *~
            *          ! Load Selectively by Demand Code / Line   !     *~
            *          ! Minor Bug in Call to GETDEM, which isn't !     *~
            *          !   even here according to Mod Block, or   !     *~
            *          !   SES which explains the miss.           !     *~
            *          ! Job # Increments rather than stopping    !     *~
            *          !    on errors.                            !     *~
            *          ! PF14 (See Jobs on file) is now visible   !     *~
            *          !    on Edit Screen.                       !     *~
            *          ! PF10 Trace (GETDEM) is now available     !     *~
            *          !    BEFORE selecting job to release,      !     *~
            *          !    rather than only AFTER, when its too  !     *~
            *          !    late. (Real tight on screen here)     !     *~
            * 07/26/93 ! Purchased Jobs                           ! MLJ *~
            *          !   1) Next job # info now in SWITCHS.SFC  !     *~
            *          !   2) If defaults exist, now loaded and   !     *~
            *          !      displayed for edit instead of input.!     *~
            *          !   3) ASKUSER to change defaults for this !     *~
            *          !      session only or all sessions.       !     *~
            *          ! PRR 12408 - displays print option default! MLJ *~
            *          !   for traveler, pick list, by-prod list &!     *~
            *          !   single level BOM for temp modification.!     *~
            *          !   Prog auto print thos items selected.   !     *~
            * 03/30/94 ! Added PFKey prompt and call to ECRINQSB. ! LDJ *~
            *          ! Fixed bug recently introduced wherein    !     *~
            *          ! an ARRAY overflow was occurring on LFAC$(!     *~
            *          ! ) - Used LFAC$(6) but didn't chg DIM!    !     *~
            * 06/24/94 ! Added creation of Vendor Servive Advices ! ERN *~
            * 07/13/94 ! Added Printing of Vendor Servive Sheets. ! RJH *~
            * 05/12/95 ! PF10 to accept defaults immediately      ! JDH *~
            *          !   loads WOs.                             !     *~
            *          ! PRR 13423. Fixed SYS2 update of next job#!     *~
            * 07/26/95 ! PRR 13478. Chngd read of DEMMASTR to     ! JDH *~
            *          !   ensure Control Number is written.      !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

          sub "JBRELSUB" (#2,                    /* PIPMASTR File UFB  */~
                          #3,                    /* HNYMASTR File UFB  */~
                          #4,                    /* JBMASTR2 File UFB  */~
                          #5,                    /* SERTIF   File UFB  */~
                          #6,                    /* SERMASTR File UFB  */~
                          #7,                    /* SERWORK  File UFB  */~
                          #8,                    /* SFCUM2   FILE UFB  */~
                          #10,                   /* RTEMASTR File UFB  */~
                          #11,                   /* WCMASTR  File UFB  */~
                          #12,                   /* GLMAIN   File UFB  */~
                          #20,                   /* USERINFO File UFB  */~
                          #23,                   /* WCOUT    File UFB  */~
                          #33,                   /* PIPIN    File UFB  */~
                          #34,                   /* PIPOUT   File UFB  */~
                          #45,                   /* JBCROSS2 File UFB  */~
                          #52,                   /* HNYQUAN  File UFB  */~
                          #54,                   /* SYSFILE2 File UFB  */~
                          #35,                   /* PIPCROSS File UFB  */~
                          #36,                   /* JBPIPXRF File UFB  */~
                          #59,                   /* STORNAME File UFB  */~
                          #18,                   /* BOMMASTR File UFB  */~
                          #19,                   /* ENGMASTR File UFB  */~
                          f2%())                 /* Entire F2% array   */

        dim                                                              ~
            date$8,                      /* Date for screen display    */~
            errormsg$79,                 /* Error message              */~
            f1%(64),                     /* Record-on-file flags       */~
            f2%(64)                      /* File status flags for      */

        dim                                                              ~
            advice$8,                    /* Vendor Service Advice #    */~
            auto%(2),                    /* Work array                 */~
            autojob$8,                   /* Next job number for auto ge*/~
            blankdate$8,                 /* Blank Date for Comparison  */~
            box$(400)1,                  /* Screen Selection Boxes     */~
            byprod$1,                    /* Print By-Prod List Flag    */~
            cancel$(400)1,               /* Demand Code Cancel Flag    */~
            codes$(100)3,                /* Part Production Schedr clas*/~
            cursor%(2),                  /* Cursor location for edit   */~
            defkitstr$3,                 /* Default store for kitting  */~
            delete$(400)1,               /* Demand Code Delete Flag    */~
            demcode$16, dline$3,         /* Demand Code/Line           */~
            demprompt$20,                /* Demand Code/Line prompt    */~
            demcodeline$(400)19,         /* Demand Code/Line           */~
            demand_opt$1,                /* Default Demand Code Option */~
            ecr$(400)1,                  /* ECR Flags Open, Closed, " "*/~
            ecrpfk$14,                   /* ECR Inquiry PFKey Prompt   */~
            header$79,                   /* Screen line                */~
            header$(2)80,                /* PLOWCODE Argument          */~
            i$(24)80,                    /* Screen Image               */~
            incl_excl(1),                /* PLOWCODE Argument          */~
            incl_excl$(1)1,              /* PLOWCODE Argument          */~
            increment$5,                 /* Job number increment       */~
            index%(400),                 /* Serial Numbers Index Ptrs  */~
            jnlid$3,                     /* Journal Id                 */~
            jobnr$(400)8,                /* User assigned job numbers  */~
            lastsort$1,                  /* Option selected last time  */~
            lfac$(8)1,                   /* Facs for screens           */~
            lfaca$(16)1,                 /* Facs for screens           */~
            lfacb$(16)1,                 /* Facs for screens           */~
            lfacc$(16,4)1,               /* FACS FOR SCREENS           */~
            line$(400)3,                 /* For reference on screen    */~
            line2$79,                    /* Second line of screen      */~
            lot$6,                       /* For kit complete           */~
            message$79,                  /* Screen message             */~
            modno$2,                     /* Module Number              */~
            newtag$19,                   /* Used in datasave           */~
            nextjob$8,                   /* Next job number for auto ge*/~
            partdescr$(400)32,           /* Part description           */~
            part_descr$(16)49,           /* For display only           */~
            part$(400)25,                /* Part job is building       */~
            pfdescr$(2)79,               /* Active PF keys Descriptions*/~
            pfkeys$32,                   /* Active PF keys             */~
            picklist$1,                  /* Print Pick List Flag       */~
            pipinkey$(400)19,            /* Work order pipin key       */~
            pipinplow$60,                /* General for plowing PIPIN  */~
            plowdem$19,                  /* Demand Code Plow Key       */~
            plowdemcode$71,              /* Demand Code Plow Key       */~
            plowkey$100,                 /* Miscellaneous Plow Key     */~
            plowkey2$50,                 /* Miscellaneous Plow Key     */~
            qty$(400)8,                  /* Job quantity to build      */~
            rqdate$(400)8,               /* Needed by date for job     */~
            savejob$8,                   /* Job # Comparison           */~
            search%(2),                  /* For searches               */~
            senabled%(400),              /* Serial Number Enabled Flag */~
            sfcdate$6,                   /* Shop floor posting date    */~
            sfcdatef$8,                  /* Shop floor date formatted  */~
            slbom$1,                     /* Print Single Level BOM Flag*/~
            sort$1,                      /* Sort order for display     */~
            sprompt$20,                  /* Part Search Capability     */~
            spart$25,sparth$25,          /* Part Search Capability     */~
            start$(25)1,                 /* Starting value for display */~
            startprompt$20,              /* Starting value selected    */~
            stdate$(400)8,               /* Start date for job         */~
            store$6,                     /* For kit complete           */~
            title$77,                    /* Screen header              */~
            title1$79,                   /* Screen header              */~
            traveler$1,                  /* Print Traveler Flag        */~
            userid$3,                    /* User's Logon Id.           */~
            vendsrvc$1,                  /* Vend Service Sheet Prt Flag*/~
            wipacct$(400)16,             /* Work in process acct by lin*/~
            z_roes$(111)8                /* Zeroes for JBMASTR2 arrays */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! JBRELUSR ! User Work Order Release Cross Reference  *~
            * #37 ! DEMMASTR ! Demand Master File                       *~
            * #38 ! BCKLINES ! Back Log Line Item File                  *~
            * #43 ! WORKFILE ! PIPCROSS Extract of PIPIN                *~
            * #60 ! JOBMASTR ! WIP/JC Job Master File                   *~
            * #61 ! VBKVSA   ! Vendor Service Advise File               *~
            *************************************************************

            select  #1, "JBRELUSR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 70,                                    ~
                        keypos =    1, keylen =   6,                     ~
                        alt key 1, keypos =  4, keylen = 6

            select #37, "DEMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  123,                                  ~
                        keypos =   2,  keylen = 27,                      ~
                        alt key  1, keypos = 10, keylen = 19,            ~
                            key  2, keypos =  1, keylen = 28,            ~
                            key  3, keypos = 29, keylen = 25, dup

            select #38, "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos = 1,   keylen = 10

            select #60, "JOBMASTR",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =   1,  keylen =  8

            select #43, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =   48,              ~
                        keypos =  30,  keylen =  19,                     ~
                        alt key  1, keypos =  1, keylen = 48

            select #61, "VBKVSA",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    5, keylen =   8,                     ~
                        alt key  6, keypos =   50, keylen =   4, dup,    ~
                            key  5, keypos =   41, keylen =  13, dup,    ~
                            key  4, keypos =   29, keylen =   6, dup,    ~
                            key  3, keypos =   13, keylen =  12, dup,    ~
                            key  2, keypos =    2, keylen =  11,         ~
                            key  1, keypos =    1, keylen =  12          ~

            if open1% <> 0 then L09000

            call "OPENCHCK" (#01, open1%, 0%, 0%, " ")
            call "OPENCHCK" (#05, open1%, 0%, 0%, " ")
            call "OPENCHCK" (#37, open1%, 0%, 0%, " ")
            call "OPENCHCK" (#38, open1%, 0%, 0%, " ")
            call "OPENCHCK" (#60, open1%, 0%, 0%, " ")
            call "WORKOPEN" (#43, "IO", 100%, f1%(43))
            call "OPENCHCK" (#61, fs%,    0%, 0%, " ")

L09000: REM *************************************************************~
            *             I N I T I A L I Z A T I O N                   *~
            *-----------------------------------------------------------*~
            * Get set up info as required.                              *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)
            modno$ = "03"
            jnlid$ = "MPR"
            call "EXTRACT" addr ("ID", userid$) /* GET CURRENT USER    */
            call "READ100" (#20, userid$, f1%(20))
                 if f1%(20) = 0 then L09171
                 get #20, using L09140, sfcdate$
L09140:     FMT XX(33), CH(6)

            call "WHICHMON" (#54, sfcdate$, return%)
            if return% > 0 then L09210
L09171:     call "ASKUSER" (0%, "*** INVALID POSTING DATE ***",          ~
                "Your Posting Date is out of the Posting Window, -OR- ,",~
                "Your Posting Dates have not been set at all!",          ~
            "Please Press (RETURN) to Acknowlege and Exit this function.")
            goto exit_routine

L09210:     REM See if operator is an administrator or not
            lfac$() = " "
            call "CMSMACHK" ("SFC", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then L09490

            REM Load Up His Release Codes For Cross Checking...
            pipinplow$ = all(hex(00))
            call "PLOWNEXT" (#1, pipinplow$, 0%, f1%(1))
                if f1%(1) = 0 then L09490  /* not using feature */

            c%, admin% = 0 : codes$() = " "
            pipinplow$ = str(userid$) & hex(00000000)
L09330:     call "PLOWNEXT" (#1, pipinplow$, 3%, f1%(1))
                if f1%(1) = 0 then L09410
            c% = c% + 1
            if c% = 100 then L09410
            get #1, using L09380, codes$(c%)
L09380:     FMT XX(3), CH(3)
            goto L09330

L09410:     REM What did we get?
            if codes$() <> " " then L09470
                call "ASKUSER" (0%, "Sorry", "You are not listed as a" & ~
                " valid Production Scheduler", "therefore you may not" & ~
                " release Work Order Advices", "Press (RETURN) to exit.")
                goto exit_routine
L09470:     if codes$() <> "ALL" then L09520

L09490:     REM This guy can do as he pleases...
            admin% = 1

L09520:     title$ = "Part Number (Description)                    ECR? S~
        ~tart By  Need By       Qty"

            message$ = "Press (RETURN) to create Jobs from marked Work Or~
        ~ders"
            call "READ100" (#54, "MONTHS OPEN", f1%(54))
            if f1%(54) = 0% then L09171
            get #54, using L09610, pldate$
L09610:     FMT XX(32), CH(6)

            REM Check Demand Code Option Flag in SYSFILE2 'A','B', or 'C'
            init("Y") cancel$(), delete$()
            call "READ100" (#54, "SWITCHS.SFC", f1%(54))
            if f1%(54) = 0% then L09630
            get #54, using L09624, demand_opt$, defkitstr$
L09624:                    FMT  POS(36), CH(1), CH(3)

L09630:     array% = dim(box$(),1)
            postflag% = 1
            if line$() <> " " then L09710
                for i% = 1 to array%
                     convert i% to line$(i%), pic(###)
                     call "STRING" addr ("LJ", line$(i%), 3%)
                next i%

L09710:     sfcdatef$ = sfcdate$
            call "DATEFMT" (sfcdatef$)
            header$ = "Select Work Orders To Process       Post Date: "  ~
                    &  sfcdatef$
            str(header$,62%) = "JBRELSUB: " & str(cms2v$,,8%)
            init (hex(00)) z_roes$()         /* For disk compaction */

L10000: REM *************************************************************~
            *             S E L E C T   S O R T   O R D E R             *~
            *-----------------------------------------------------------*~
            * Choose The Order in which you want to see the WO's.       *~
            *************************************************************

        inputmode
            errormsg$ = " "
        inputmode_special
            init(" ") startprompt$, start$(), autojob$, increment$,      ~
                      lastsort$, sort$, demprompt$, demcode$, dline$,    ~
                      spart$, savejob$, ecrpfk$
            store$ = defkitstr$
            increment% = 10% : search%, edt% = 0% : reset% = 1%
            call "READ100" (#54, "SWITCHS.SFC", f1%(54%))
                if f1%(54%) = 0% then L10160
            get #54, using L10140, autojob$, increment%, lastsort$,       ~
                     traveler$, picklist$, byprod$, slbom$, vendsrvc$
L10140:     FMT POS(72), CH(8), BI(4), CH(1), POS(127), 5*CH(1)
            sort$ = lastsort$
            convert increment% to increment$, pic(00000)
            savejob$ = autojob$  :  edt% = 1%
            gosub test_autojob

L10160: inputmode1
            init(" ") jobnr$(),part$(),partdescr$(), qty$(), stdate$(),  ~
                      pipinkey$(), rqdate$(), wipacct$(), box$(), ecr$()

            line%, maxlines%, base%, errorline%, index% = 0%
            mat index% = zer
            mat senabled% = zer
            pfdescr$(1) = "(1)Start Over                                 ~
        ~                 (15)Print Screen"
            pfdescr$(2) = "               (4)Edit Previous Field        (~
        ~13)Instructions  (16)Exit Program"
            pfkeys$ = hex(0001040d0e0f10)
            if edt% = 1% then editmode1

            for fieldnr% = 1% to 6%
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10420
L10291:               if fieldnr% > 1% then str(pfdescr$(2),16%,22%) =   ~
                                            "(4)Edit Previous Field"     ~
                                       else str(pfdescr$(2),16%,22%) =" "
                gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 or fieldnr% = 1 then L10380
L10330:                  fieldnr% = fieldnr% - 1%
                         if fieldnr% = 1% then L10000
                         gosub'051(fieldnr%)
                         if enabled% > 0 then L10291
                         goto L10330
L10380:               if keyhit%  = 16 and fieldnr% = 1 then exit_routine
                      if keyhit% <>  0 then       L10291
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10291
L10420:     next fieldnr%
            print at(04,02,78);hex(94);"Loading Planned Work Orders"
            goto L11000

        editmode1
            message$ = "Position Cursor & Press RETURN To Modify -OR- P"&~
                       "F10 To Accept Defaults."
            fieldnr% = 0%
            init(hex(86)) lfac$()
            str(pfdescr$(1%),16%,24%) = "(3)Review Used Numbers"
            str(pfdescr$(2%),16%,22%) = "(10)Accept Defaults"
            str(pfkeys$,8%,1%) = hex(0a) :  str(pfkeys$,3%,1%) = hex(ff)
            str(pfkeys$,9%,1%) = hex(03)

            gosub L40700
                if keyhit% =  1% then gosub startover
                if keyhit% = 16% then exit_routine
                if keyhit% = 10%                                         ~
                                 then goto L10810                         ~
                                 else fieldnr% = cursor%(1%) - 4%
                str(pfdescr$(2%),16%,22%) = "(4)Edit Previous Field"
                str(pfkeys$,3%,1%) = hex(04)
                str(pfkeys$,8%,1%) = hex(ff)

            for fieldnr% = fieldnr% to 6%
                gosub'051(fieldnr%)
                      if enabled% = 0% then L10800
L10680:         gosub'101(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% or fieldnr% = 1% then L10760
L10710:                  fieldnr% = fieldnr% - 1%
                         if fieldnr% = 1% then L10000
                         gosub'051(fieldnr%)
                         if enabled% > 0% then L10680
                         goto L10710
L10760:               if keyhit%  = 16% and fieldnr%= 1% then exit_routine
                      if keyhit% <>  0% then L10680
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10680
L10800:     next fieldnr%
L10810: REM Find Portion Of Job Number To Increment...
            temp$ = autojob$
            for i% = 1% to 8%
                if str(temp$,i%,1%) < "0" or str(temp$,i%,1%) > "9"      ~
                                              then str(temp$,i%,1%) = " "
            next i%
                temp% = pos(-temp$<>" ")
                     if temp% = 0% then L10955
                auto%(1%) = pos(-str(temp$,,temp%)=" ") + 1%
                auto%(2%) = (temp% - auto%(1%)) + 1%
                if auto%(2%) < 3% then L10940
                convert str(autojob$,auto%(1%),auto%(2%)) to job%,       ~
                                                        data goto L10940
                goto L10955
L10940:             errormsg$ = "Next Job Nmbr Must Have At Least 3 Con"&~
                                "secutive Nmbrs For Incrementing"
                    return

L10955:     print at(04,02,78);hex(94);"Loading Planned Work Orders"

L11000: REM *************************************************************~
            *                  B E G I N   P L O W                      *~
            *-----------------------------------------------------------*~
            * Order was chose, lets get to it.                          *~
            *************************************************************

            if demcode$ = " " then L11270
               init(hex(00)) plowkey$ : work% = 0%
               call "DELETE" (#43, plowkey$, 0%)
               plowkey$ = str(demcode$,,16%) & str(dline$,,3%) & "WO"
L11100:        call "PLOWNEXT" (#35, plowkey$, 21%, f1%(35%))
                  if f1%(35%) = 0% then L11200
               call "READ100" (#33, str(plowkey$,20%,19%), f1%(33%))
                  if f1%(33%) = 0% then L11100
               get #33 using L11150, plowdemcode$
L11150:            FMT CH(48)
               write #43 using L11150, plowdemcode$
               work% = work% + 1%
               goto L11100

L11200:        if work% > 0% then L11270
                  errormsg$ = "No Orders Were Found To Release via " &   ~
                              "this Demand Code/Line"
                  goto inputmode1

*       ** Set Up the Plowkey

L11270:     if sort$ = "P"  then L11440

        REM HERE WE HAVE CHOSEN PLOW BY START DATE
            bisdate% = 0%
            if start$() = " " or start$() = blankdate$ then L11350
            call "DATUNFMT" (str(start$()))
            call "DATE" addr("G-", pldate$,str(start$(),,6),bisdate%,0%)
                call "DATEFMT" (str(start$()))
L11350:         bisdate% = bisdate% + 1%
                bisdate% = max(1,min(bisdate%,490%))
            key% = 0%
            break% = 2%
            init(hex(00)) pipinplow$
            str(pipinplow$,1,2)="WO"
            convert bisdate% to str(pipinplow$,3,3), pic (###)
            goto show_wos

L11440: REM HERE WE HAVE CHOSEN PLOW BY PART NUMBER
            key% = 1%
            init(hex(00)) pipinplow$
            str(pipinplow$,,25) = start$()
            break% = 0%

        REM *************************************************************~
            *            R E L E A S E   J O B   O R D E R S            *~
            *-----------------------------------------------------------*~
            * Pick WOS to be released.                                  *~
            *************************************************************

            errormsg$ = " "

        show_wos

            gosub dataload  /* Load next page for display;processing */
            if maxlines% > 0% then L12150
                errormsg$ = "No Orders Were Found To Release."
                goto inputmode1

L12150:     base% = max(0%, min(maxlines%-16%, base%))
            if sort$ <> "P" then L12192
L12170:        errormsg$ = errormsg$ &                                   ~
                           hex(84) & " Press PF8 to Enable Part Search."

L12192:     message$ = "Mark WO's to Release & Press RETURN. Cur"     &  ~
                       "sor & PF9-Component Status. PF10-Trace."
            gosub'102
            errormsg$ = " "
            if keyhit% =  1% then gosub startover
            if keyhit% =  2% then base% = 0%
            if keyhit% =  4% then base% = max(base% - 12%, 0%)
            if keyhit% =  5% then base% = base% + 12%
            if sort$  <> "P" then L12300
            if keyhit% <> 8% then L12300
               search% = 1%
               message$ = "Enter Part Code (or partial) for search. "   &~
                          " [FIRST, LAST (viewed) are Keywords.]"
               gosub'102
               goto L12170
L12300:     if keyhit% <> 0% then show_wos

            if box$() <> " " then L12350
                errormsg$ = "Please Mark WOs to release."
                goto show_wos
L12350:     gosub squish_array
            goto review_and_edit

        squish_array
            REM Squish arrays to eliminate unwanted WOs...splurp...
L12520:     blank% = pos(box$() = " ")
            if blank% = 0% then L12730
            n% = pos(str(box$(),blank%) <> " ")
            if n% = 0% then L12730
                   n% = n% + (blank%-1%)   /* Offset POS function */
                   index%(blank%)       = index%(n%)
                   part$(blank%)        = part$(n%)
                   jobnr$(blank%)       = jobnr$(n%)
                   partdescr$(blank%)   = partdescr$(n%)
                   pipinkey$(blank%)    = pipinkey$(n%)
                   wipacct$(blank%)     = wipacct$(n%)
                   stdate$(blank%)      = stdate$(n%)
                   rqdate$(blank%)      = rqdate$(n%)
                   qty$(blank%)         = qty$(n%)
                   box$(blank%)         = box$(n%)
                   demcodeline$(blank%) = demcodeline$(n%)
                   ecr$(blank%)         = ecr$(n%)
                   part$(n%), pipinkey$(n%), wipacct$(n%), stdate$(n%),  ~
                   rqdate$(n%), qty$(n%), box$(n%), partdescr$(n%),      ~
                   jobnr$(n%), demcodeline$(n%), ecr$(n%) = " "
            goto L12520

L12730:     maxlines% = pos(box$() = " ") - 1%
            REM Clear off screen where we have to...
            if maxlines% >= dim(jobnr$(),1) then return
            for n% = maxlines% + 1% to dim(jobnr$(),1)
                   part$(n%), pipinkey$(n%), wipacct$(n%), stdate$(n%),  ~
                   rqdate$(n%), qty$(n%), jobnr$(n%), box$(n%),          ~
                   demcodeline$(n%) = " "
            next n%
            return

        review_and_edit
            pfdescr$(1) = "(1)Start Over     (12)Delete Line     (1"    &~
                          "4)See Jobs on File     "
            str(pfdescr$(1),64%) = "(13)Instructions"
            pfdescr$(2) = " "
            str(pfdescr$(2),64%) = "(15)Print Screen"
            lfac$, sfac$ = hex(9c)
            nextjob$, jobnr$(1) = autojob$
            for i% = 1% to maxlines%
                errormsg$ = " "
                base% = max(i%/16%*16%-1%,0) /* Screen Display Offset */
                if i% = 1% or autojob$ = " " then L13065
L13055:            gosub next_job_number
                   jobnr$(i%) = nextjob$
L13065:         if jobnr$(i%) = " " then L13080
L13070:         gosub'153(1%)  /* Edit Job Number  */
                if errormsg$ = " " then L13150
                   if autojob$  = " " then L13080
                   if nextjob$ <> " " then L13055
L13080:         message$ = "Enter the Job Number to Create/Release"
L13085:         gosub'103(1%)  /* Display Job Number Error   */
                if keyhit% = 1% then gosub startover
                if keyhit% = 0% then L13070
                if keyhit%<>12% then L13085
L13105:            box$(i%) = " "
                   call "SERSTOVR" (index%(i%), "6", " ",  #6, #7)
                   gosub squish_array
                   i% = i% - 1%
                   errormsg$ = " "
                   errorline% = 0%
                   if maxlines% < 1% or maxlines% < i% then inputmode1
                   goto L13235

L13150:         gosub'153(2%)  /* Edit WIP Account */
                if errormsg$ = " " then L13195
                message$ = "Enter the G/L Work In Progress Asset Account"
L13165:         gosub'103(2%)  /* Display WIP Account Error  */
                if keyhit% = 1% then gosub startover
                if keyhit% = 0% then L13150
                if keyhit% =12% then L13105
                goto L13165

L13195:         gosub'153(3%)  /* Check For / Assign Serial Numbers */
                if errormsg$ = " " then L13235
L13205:         gosub'103(3%)  /* Display Screen with Error Message */
                if keyhit% = 1% then gosub startover
                if keyhit% = 0% then L13195
                if keyhit% =12% then L13105
                goto L13205

L13235:     if i% >= maxlines% then i% = dim(jobnr$(),1)
            next i%
            base% = 0%

        show_wos_selected
            message$  = "To Modify Displayed Values, Position Cursor" &  ~
                           " to Desired Value & Press (RETURN)."
            pfdescr$(1) =                                                ~
        "(1)ReStrt (2)1st (4)Prev (5)Nxt (8)Cntrl # (9)Trace (12)Del (13)~
        ~Inst (15)PrtScr"
            pfdescr$(2) = "(10)Release Jobs AND Auto Kit: Store"
            str(pfdescr$(2),42%) = "Lot"
            str(pfdescr$(2),57%) = "(16)Release Jobs (Only)"
            lfac$, sfac$ = hex(81)
            if demand_opt$ = "C" then str(pfdescr$(1), 33, 10) = " "
L13555:     gosub'103(0%)
               errormsg$ = " "
               errorline% = 0
               if keyhit% <> 8 then L13585
                  if demand_opt$ = "C" then L13555
                  goto cntrl_field_selected
L13585:        if keyhit% =  1 then gosub startover
               if keyhit% =  2 then base% = 0%
               if keyhit% =  4 then base% = max(base%-12%, 0%)
               if keyhit% =  5 then                                      ~
                               base%=max(0,min(maxlines%-16%,base%+12%))
               if keyhit% = 10 then check_auto_kit
               if keyhit% = 16 then data_save
               oldi%, oldfield% = 0%
L13625:        if cursor%(1) < 5% or cursor%(1)>20% then show_wos_selected
               i% = cursor%(1) - 4% + base%
               if i% < 1% or i% > maxlines% then show_wos_selected
               if keyhit% <> 12% then L13710
                   errorline% = i%
                   errormsg$ = "Press RETURN to Remove this Line OR " &  ~
                               "Press PF1 to Cancel Delete."
L13660:            gosub'103(0%)
                   if keyhit% = 1% then show_wos_selected
                   if keyhit% <> 0% then L13660
                   box$(i%) = " "
                   call "SERSTOVR" (index%(i%), "6", " ",  #6, #7)
                   gosub squish_array
                   errormsg$ = " "
                   errorline% = 0%
                   if maxlines% < 1% then inputmode1
                   goto show_wos_selected
L13710:        if keyhit% <> 9% then L13730
                   call "GETDEM" (1%, pipinkey$(i%), #35, #37, #33,      ~
                                  " ", " ", 0%)
                   goto show_wos_selected
L13730:        if keyhit% <> 0% then show_wos_selected
               if cursor%(2) < 16% then fieldnr% = 1% else fieldnr% = 2%
               if cursor%(2) > 70% then fieldnr% = 3%
               if i% = oldi% and fieldnr% = oldfield% then               ~
                  show_wos_selected
               if fieldnr% <> 3% then L13760
*                IF ADMIN% = 0% THEN 13805
L13760:        pfdescr$(1) = "(1)Start Over                         (1" &~
                             "4)See Jobs on File     "
               str(pfdescr$(1),64%) = "(13)Instructions"
               pfdescr$(2) = " "
               str(pfdescr$(2),64%) = "(15)Print Screen"
               lfac$, sfac$ = hex(9c)
               message$ = " "
L13790:        gosub'103(fieldnr%)
                  if keyhit% = 1% then gosub startover
                  if keyhit% <> 0% then L13790
               gosub'153(fieldnr%)
                  if errormsg$ > " " then L13790
                  oldi% = i%
                  oldfield% = fieldnr%
                  goto L13625

        check_auto_kit
            gosub'152
               if errormsg$ <> " " then show_wos_selected
               goto L14050
        data_save
            store$ = " "
L14050:     if errormsg$ <> " " then show_wos_selected
            gosub datasave
            goto inputmode_special

        next_job_number
            REM Come up with next Job Number...
            job% = job% + increment%
            convert job% to temp$, pic(00000000)
            if auto%(2) < 8% then L14570
               if pos(temp$ = "#") > 0% then L14580
               goto L14590
L14570:     if pos(str(temp$,,8%-auto%(2))>"0") = 0% then L14590
L14580:        nextjob$ = " " : return
L14590:     str(nextjob$,auto%(1),auto%(2)) = str(temp$,8%-auto%(2)+1%)
            return

        test_autojob
            test% = 0%
            if autojob$ = " " then return
            call "READ100" (#4, autojob$, f1%(4))
               if f1%(4) = 0% then return
            if increment% < 1% then return
               temp = increment%
               call "CONVERT" (temp, -0.001, increment$)
            test% = 1%
            gosub'151(1%)
            test% = 0%
            if errormsg$ <> " " then return
L15110:     nextjob$ = autojob$
            gosub next_job_number
            if nextjob$ = " " then return
            autojob$  = nextjob$
            call "READ100" (#4, autojob$, f1%(4))
               if f1%(4) = 0% then return
            goto L15110

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for the page 1 of input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1
                  on fieldnr% gosub L20130,         /* NEXT JOB NUMBER  */~
                                    L20170,         /* INCREMENT        */~
                                    L20290,         /* SORT OPTION      */~
                                    L20340,         /* START VALUE      */~
                                    L20440          /* DEMAND CODE      */
                     return
L20130:     REM DEFAULT/ENABLE FOR NEXT JOB NUMBER
                message$ = "Enter Starting Value For Auto Generation Of J~
        ~ob Numbers.  Blank If Not Using."
                return
L20170:     REM DEFAULT/ENABLE FOR INCREMENT
                if autojob$ <> " " then L20210
                   enabled% = 0
                   return
L20210:         if increment$ <> " " then L20240
                   temp = increment%
                   call "CONVERT" (temp, -0.001, increment$)
L20240:         message$ = "Enter The Increment To Be Used When Assigning~
        ~ Job Number."
                str(pfkeys$,pos(pfkeys$ = hex(03)), 1) = hex(20)
                str(pfdescr$(1),16,24) = " "
                return
L20290:     REM DEFAULT/ENABLE FOR SORT OPTION
                if sort$ = " " then sort$ = lastsort$
                message$ = "Enter 'P' To See WOs In Part# Order, or 'D' T~
        ~o See In Start Date Order."
                return
L20340:     REM DEFAULT/ENABLE FOR START VALUE
                redim start$(25)1
                startprompt$ = "First Part Number"
                if sort$ <> "D" then L20390
                   str(start$(),9) = " "
                   redim start$(8)1
                   startprompt$ = "First Start Date"
L20390:         message$ = "Enter " & startprompt$
                message$ = message$ & " To Display. Blank For First In Fi~
        ~le."
                return

L20440:     REM DEFAULT/ENABLE FOR DEMAND CODE/LINE
                demprompt$ = "Via Demand Code/Line"
                message$ = "Enter Demand Code/Line if desired (or blank)"
                return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the user the ability to start over when he wants to *~
            * else return to the menu.  Notice that he has to push 2    *~
            * different buttons to start over--a little harder.         *~
            *************************************************************

        startover
            k1% = 2%
            call "STARTOVR" (k1%)
            if k1% = 1% then return
            return clear all
            call "SERSTOVR" (0%, "6", " ",  #6, #7)
            goto inputmode

        REM *************************************************************~
            *          L O A D   D A T A   F R O M   F I L E S          *~
            *-----------------------------------------------------------*~
            * Plow for work orders to be released.                      *~
            * Build data stacks and return.                             *~
            *************************************************************
        dataload
            REM Fill next Screen Of Arrays...?
            if search% = 1% then L30100
            if maxlines% >= base% + 16% then return

L30100:     REM Got room?
            if line% < array% then L30150
               errormsg$ = "Array full, Can't load anymore Work Advices."
               return

L30150:     if demcode$ = " " then L30230
L30160:     call "PLOWALTS" (#43, pipinplow$, key%, break%, f1%(43%))
               if f1%(43%) = 0% then L30900
            get #43 using L30190, plowkey$
L30190:         FMT POS(30), CH(19)
            call "READ100" (#33, plowkey$, f1%(33%))
               if f1%(33%) = 0% then L30160
            goto L30260

L30230:     call "PLOWALTS" (#33, pipinplow$, key%, break%, f1%(33))
                if f1%(33)=0 then L30900
            if key% = 1% and str(pipinplow$,30,2) <> "WO" then L30230
L30260:     maxlines%, line% = maxlines% + 1
            index% = index% + 1%
            get #33, using L30310, part$(line%),rqdate%,pipinkey$(line%), ~
                     quan, stdate%
            index%(line%) = index%
L30310:     FMT CH(25), BI(4), CH(19), PD(14,4), BI(4)

            partdescr$(line%) = hex(94) & "**NOT ON FILE**"
            call "READ100" (#3, part$(line%), f1%(3))
                if f1%(3)=0 then L30500
            get #3, using L30380, partdescr$(line%), partclass$,          ~
                wipacct$(line%)
L30380:     FMT POS(26), CH(32), POS(309), CH(3), POS(344), CH(9)
            call "GLFMT" (wipacct$(line%))
*          Check for ECRs
            ecrpfk$ = "(11)"
            call "ECRINQSB" ("C",        /* "C" to Check for ECR Info  */~
                            part$(line%),/* Part to Do Inquiry/Check on*/~
                            ecrpfk$,     /* IN:  PFKey # to Use        */~
                            #54,         /* SYSFILE2                   */~
                            #03)         /* HNYMASTR                   */
            if ecrpfk$ = " " then ecr$(line%) = " "
            if str(ecrpfk$,,1%) = hex(8c) then ecr$(line%) = "C"
            if str(ecrpfk$,,1%) = hex(84) then ecr$(line%) = "O"

            REM Can this guy release orders for this part?
            if admin% = 1 then L30500
            search codes$() = str(partclass$,,3) to search%() step 3
                if search%(1) <> 0 then L30500
                part$(line%), pipinkey$(line%), wipacct$(line%),         ~
                              ecr$(line%),        partdescr$(line%) = " "
                maxlines%, line% = maxlines% - 1
                goto L30150

L30500:     call "CONVERT" (quan, 0.2, qty$(line%))
            call "DATE" addr("G+", pldate$, stdate% - 1%,                ~
                                        str(stdate$(line%),,6), ret%)
            call "DATE" addr("G+", pldate$, rqdate% - 1%,                ~
                                        str(rqdate$(line%),,6), ret%)
            call "DATEFMT" (stdate$(line%))
            call "DATEFMT" (rqdate$(line%))

        REM Now get demand and demand line for the control number        ~
            If not found, don't write Demand Code(Control #) to JBMASTR2
            if demand_opt$ = "C" then L30850
                plowdemcode$ = xor plowdemcode$ : demcodeline$(line%)=" "
                cancel$(line%) = "Y" : delete$(line%) = "Y"
                str(plowdemcode$,,19) = str(pipinkey$(line%))
                call "PLOWALTS" (#35, plowdemcode$, 1%, 19%, f1%(35))
                  if f1%(35) = 0% then L30850
                    get #35 using L30670, demcodeline$(line%)
L30670:             FMT CH(19)
                    cancel$(line%) = "N"

        REM Check to see if Demand Code is deleted from DEMMASTR         ~
            If 'Y' don't write Demand Code(Control #) to JBMASTR2 File
               str(plowdem$,,19%) = demcodeline$(line%)
               call "REDALT0" (#37, plowdem$, 1%, f1%(37%))
                  if f1%(37%) = 1% then delete$(line%) = "N"

        /* TEST SECTION - MAKE ALL COUNT AS N + 1
            N% = 19%
            FOR TEST% = 1% TO N%
                MAXLINES%, LINE% = MAXLINES% + 1
                PART$       (LINE%) = PART$       (LINE% - 1%)
                PIPINKEY$   (LINE%) = PIPINKEY$   (LINE% - 1%)
                INDEX%      (LINE%) = INDEX%      (LINE% - 1%)
                PARTDESCR$  (LINE%) = PARTDESCR$  (LINE% - 1%)
                ECR$        (LINE%) = ECR$        (LINE% - 1%)
                WIPACCT$    (LINE%) = WIPACCT$    (LINE% - 1%)
                STDATE$     (LINE%) = STDATE$     (LINE% - 1%)
                RQDATE$     (LINE%) = RQDATE$     (LINE% - 1%)
                CANCEL$     (LINE%) = CANCEL$     (LINE% - 1%)
                DELETE$     (LINE%) = DELETE$     (LINE% - 1%)
                DEMCODELINE$(LINE%) = DEMCODELINE$(LINE% - 1%)
            NEXT TEST%                          /* END OF TEST SECTION */
L30850:
            if search% = 0% then dataload
            if part$(line%) <= sparth$ then dataload
               return

L30900:     if errormsg$=" " then errormsg$ =                            ~
                     hex(84) & "All Work Orders Have Been Shown."
            return

        REM *************************************************************~
            *            S A V E   D A T A   T O   F I L E S            *~
            *-----------------------------------------------------------*~
            * Now do the thrashing about with PIPIN, PIPOUT, WCOUT,     *~
            * and create JBMASTR2 records. Now does JBCROSS2, PIPCROSS, *~
            * and JBPIPXRF as well.                                     *~
            *************************************************************
        datasave
            bad% = 0 : jbpost1% = 0%
            if jobnr$() = " " then return
            nextjob$ = " "
            if store$ = " " then                                         ~
                 call "SHOSTAT"("Work Order Release In Progress")  else  ~
                 call "SHOSTAT"("Releasing Work Orders And Queuing" &    ~
                                                " Jobs For Kit Complete")

            for i%=1 to maxlines%
            if jobnr$(i%)=" " then next_wo
            REM Insure nobody else snuck in and mucked us up...
            call "READ100" (#4, jobnr$(i%), f1%(4))
                if f1%(4) <> 0 then L31220
            call "READ101" (#33, pipinkey$(i%), f1%(33))
                if f1%(33) = 1% then L31242
L31220:         bad% = bad% + 1
                call "SERSTOVR" (index%(i%), "6", " ",  #6, #7)
                goto next_wo
L31241:         FMT POS(26), BI(4), POS(49), PD(14,4)
L31242:     REM *** Check to See if Quantity Changed ***
            get #33 using L31241, plndate%, qty
            quan = 0 : convert qty$(i%) to quan, data goto L31245
L31245:     if qty = quan then L31260
            put #33 using L31241, plndate%, quan
            rewrite #33
            REM *** Update PIPMASTR ***
            adjqty = quan - qty
            call "PIPFLAGS" (part$(i%), 1%, plndate%, adjqty, #2, #8)

L31260:     newtag$="JOB ORDER: " & str(jobnr$(i%),,8)
            call "JBRETAG"               /* CHANGE PIP TAGS FROM WO    */~
                       (pipinkey$(i%),   /* TAG CURRENTLY ON FILE      */~
                        newtag$,         /* TAG IT WILL BECOME         */~
                        #45,             /* JBCROSS2                   */~
                        #2,              /* PIPMASTR                   */~
                        #23,             /* WCOUT                      */~
                        #33,             /* PIPIN                      */~
                        #34,             /* PIPOUT                     */~
                        #35,             /* PIPCROSS                   */~
                        #36)             /* JBPIPXRF                   */


            call "DATUNFMT" (stdate$(i%))
            call "DATUNFMT" (rqdate$(i%))
            call "GLUNFMT" (wipacct$(i%))

            if delete$(i%) = "Y" or cancel$(i%) = "Y" or                 ~
               demand_opt$ = "C" then demcodeline$(i%) = " "

            write #4, using L31490, jobnr$(i%), partdescr$(i%), newtag$,  ~
                     part$(i%), quan, 0, " ", date, " ",                 ~
                     wipacct$(i%), stdate$(i%), rqdate$(i%), quan, " ",  ~
                     str(z_roes$(),,20), str(z_roes$()),                 ~
                     demcodeline$(i%), 0, str(z_roes$(),,104), " ",      ~
                     eod goto L31220

            call "CDANPOST" (#4, "A")

L31490:         FMT  CH(8), CH(30), CH(19),                              ~
                     CH(25), 2*PD(14,4), CH(48), CH(6), CH(6),           ~
                     CH(9), CH(6), CH(6), PD(14,4), CH(24),              ~
                     CH(20), CH(888),                                    ~
                     CH(19), PD(14,4), CH(104), CH(50)

*        Write out any Vendor Service Advices...
            call "JBVSASUB" (#54, #23, "ADD", jobnr$(i%), part$(i%), quan)

            call "HNYPST1" (part$(i%), "001", " ",                       ~
                      0,0,quan,0,0,      /* POST TO ON ORDER           */~
                      #52,               /* UFB ADDRESS OF HNYQUAN     */~
                      #3,                /* UFB ADDRESS OF HNYMASTR    */~
                      #54,               /* UFB ADDRESS OF SYSFILE2    */~
                      f2%(52),           /* STATUS FLAG FOR HNYQUAN    */~
                      f2%(3),            /* STATUS FLAG FOR HNYMASTR   */~
                      f2%(54),           /* STATUS FLAG FOR SYSFILE2   */~
                      0%,                /* WHETHER OR NOT TO PRINT AN */~
                                         /*  EXCEPTION REPORT WHEN A   */~
                                         /*  NEW HNYQUAN REC CREATED-  */~
                                         /*  1= PRINT, 0 = DON'T PRINT */~
                      temp%)             /* ERROR RETURN FROM SUBROUTIN*/~

            REM *** Save Serial Numbers ***
            plowkey$ = jobnr$(i%)
            call "SERSAVE"                                               ~
                       (index%(i%),      /* Line Item Pointer.         */~
                        "WP",            /* Source Transaction Type    */~
                        plowkey$,        /* Source Transaction Key     */~
                        maxlines%,       /* # Trans to Create File for */~
                        part$(i%),       /* Part Code                  */~
                        userid$,         /* Current User ID            */~
                        "1",             /* Change Status to ...       */~
                        "6",             /* Change Status from ...     */~
                        1%,              /* Clear TIF after Save (YES) */~
                        #54,             /* SYSFILE2 UFB               */~
                        #5,              /* SERTIF UFB                 */~
                        #6,              /* SERMASTR UFB               */~
                        #7)              /* SERWORK  UFB               */

            if picklist$ <> "Y" then L31780
                call "JBPICKSL" (jobnr$(i%),jobnr$(i%), #4, #34, #23, #3,~
                                #52, #54, 0%, pldate$, 490%)
L31780:     if traveler$ <> "Y" then L31790
                call "JBTRAVEL" (part$(i%), jobnr$(i%), quan, #10, #11,  ~
                                #23, #45, #3, #18, #19, #54, #4)
L31790:     if byprod$ <> "Y" then L31800
                call "JBPICKBP" (jobnr$(i%),jobnr$(i%), #4, #34, #23, #3,~
                                #52, #54, 0%, pldate$, 490%)
L31800:     if slbom$ <> "Y" then L31830
                init(hex(00)) plowkey$
                bomid$ = " "
                str(plowkey$,1%) = "JOB ORDER: " & jobnr$(i%)
                call "READ100" (#45, plowkey$, f1%(45%))
                   if f1%(45%) = 0% then L31830
                       get #45 using L31814, bomid$
L31814:                    FMT POS(73), CH(3)
                call "BOMSLSUB" (part$(i%), bomid$, "N", "N", "N", "N",  ~
                                #18, #3, #1, #1, #19, #54, " ", " ", " ",~
                                jobnr$(i%))

L31830:     /* Lets print the Vendor Service Sheets */
            if vendsrvc$ <> "Y" or fs% <>  1%  then L31845
                plowkey2$ = jobnr$(i%)
L31833:         call "PLOWALTS" (#61, plowkey2$, 3%, 8%, f1%(61%))
                if f1%(61%) = 0% then L31845  /*Thats All for Now */
                get #61 using L31836, advice$
L31836:      FMT POS(5), CH(8)
                if advice$ = " "  then L31840
               /* We'll print the Sheet w/o any shipper info */
                call "VSASHTSB" (advice$, " ",  0 , " ", " ", " ")
L31840:         goto L31833    /* Loop for more steps into Vendor WC */

L31845:     REM All thats left is to kit, if requested...
            if store$ = " " then next_wo

            if postflag% = 0 then L31905
                call "JNLINFO" (modno$, jnlid$, pstseq%, " ", " ",       ~
                                             sfcdate$, #54, f2%(54), 0%)
                postflag% = 0

L31905:     REM Wake Up, Once per data save ...
            if jbpost1% <> 0% then L31910
               call "JB2TIF" ("J1", 2%, jbpost1%)
               jbpost1% = 1%

L31910:     REM Kit It Complete...
            call "JB2TIF"               /* Writes to Transaction Image */~
                 ("J1",                 /* Send transaction to JBPOST1 */~
                  0%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  3%,                   /* Transaction type (1 = kit)  */~
                  hex(40),              /* Priority                    */~
                  jobnr$(i%),           /* Job number effected         */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  sfcdate$,             /* G/L posting Date            */~
                  " ",                  /* Inventory Part Code         */~
                  store$,               /* Inventory Store Code        */~
                  lot$,                 /* Inventory Lot Id.           */~
                  0,                    /* Quantity to process         */~
                  " ",                  /* Not used                    */~
                  " ",                  /* Not used                    */~
                  " ")                  /* Not used                    */

        next_wo
            if jobnr$(i%) > nextjob$ then nextjob$ = jobnr$(i%)
            next i%

            REM Save next job number and return...
            if bad% = 0 then L32200
                errormsg$ = "XXX work order(s) may not have been released~
        ~ because of concurrency conflicts."
                convert bad% to str(errormsg$,,4), pic(###)
L32200:     if autojob$ = " " then L32260
            convert str(nextjob$,auto%(1),auto%(2)) to job%,             ~
                                                          data goto L32260
            gosub next_job_number
            goto L32270

L32260:     nextjob$ = " "
L32270:     if reset% <> 1% then return
            call "READ101" (#54, "SWITCHS.SFC", f1%(54%))
            put #54 using L32310, nextjob$, increment%, sort$
L32310:         FMT POS(72), CH(8), BI(4), CH(1)
            rewrite #54
            return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *-----------------------------------------------------------*~
            * Screen Page 1 entry.                                      *~
            *************************************************************~

        deffn'101(fieldnr%)
            if fieldnr% = 0% then init(hex(86)) lfac$()                  ~
                             else init(hex(8c)) lfac$()
            str(pfdescr$(1),16%,24%) = " "
            if pos(pfkeys$ = hex(03)) > 0% then                          ~
               str(pfkeys$,pos(pfkeys$ = hex(03)),1%) = hex(20)

            on fieldnr% gosub L40220,               /* NEXT JOB #       */~
                              L40220,               /* Increment        */~
                              L40220,               /* Sort Order       */~
                              L40220,               /* Start Value      */~
                              L40220,               /* Demand Code      */~
                              L40220                /* Print Options    */
            goto L40320

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40220:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      if fieldnr% <> 1 then L40260
                      str(pfdescr$(1),16,24) = "(3)Review Used Numbers"
                      str(pfkeys$,pos(pfkeys$ = hex(20)), 1) = hex(03)
L40260:               lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40320:     accept                                                       ~
               at (01,02), "Open Jobs - Release Planned Work Orders",    ~
               at (01,59), "Today's Date:", fac(hex(8c)), date$  ,ch(08),~
               at (02,02), fac(hex(ac)), header$                 ,ch(79),~
               at (04,02), fac(hex(94)), errormsg$               ,ch(79),~
                                                                         ~
               at (05,02), "Next Job Number (default)",                  ~
               at (05,35), fac(lfac$( 1)), autojob$              ,ch(08),~
               at (06,02), "Job Number Increment",                       ~
               at (06,35), fac(lfac$( 2)), increment$            ,ch(05),~
               at (07,02), "Sort Order For Display (P or D)",            ~
               at (07,35), fac(lfac$( 3)), sort$                 ,ch(01),~
               at (08,02), fac(hex(8c)), startprompt$,                   ~
               at (08,35), fac(lfac$( 4)), str(start$()),                ~
               at (09,02), fac(hex(8c)), demprompt$,                     ~
               at (09,35), fac(lfac$( 5)), demcode$              ,ch(16),~
               at (09,52), fac(lfac$( 5)), dline$                ,ch(03),~
               at (10,02), "Print -  Traveler:",                         ~
               at (10,21), fac(lfac$(6%)), traveler$             ,ch(01),~
               at (10,24), "Pick List:",                                 ~
               at (10,35), fac(lfac$(6%)), picklist$             ,ch(01),~
               at (10,38), "By-Product List:",                           ~
               at (10,55), fac(lfac$(6%)), byprod$               ,ch(01),~
               at (10,57), "Single Level BOM:",                          ~
               at (10,75), fac(lfac$(6%)), slbom$                ,ch(01),~
               at (11,02), "         Vendor Service Sheet:",             ~
               at (11,33), fac(lfac$(6%)), vendsrvc$             ,ch(01),~
                                                                         ~
               at (22,02), fac(hex(a4)),   message$              ,ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(1)           ,ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(2)           ,ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 3 then L40610
                  call "GETCODE" (#4, " ", " ", 0%, 0, f1%(4))
                  goto L40320

L40610:        if keyhit% <> 13 then L40650
                  call "MANUAL" ("JBRELSUB")
                  goto L40320

L40650:        if keyhit% <> 15% then L40674
                  call "PRNTSCRN"
                  goto L40320

L40674:        close ws
               call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
               return

        REM Display For Edit - Job Number, Increment & Last Sort...
            str(pfdescr$(2%),16%,24%) = "(3)Review Used Numbers"


L40700:     accept                                                       ~
               at (01,02), "Open Jobs - Release Planned Work Orders",    ~
               at (01,59), "Today's Date:", fac(hex(8c)), date$  ,ch(08),~
               at (02,02), fac(hex(ac)), header$                 ,ch(79),~
               at (04,02), fac(hex(94)), errormsg$               ,ch(79),~
                                                                         ~
               at (05,02), "Next Job Number (default)",                  ~
               at (05,35), fac(lfac$(1%)), autojob$              ,ch(08),~
               at (06,02), "Job Number Increment",                       ~
               at (06,35), fac(lfac$(2%)), increment$            ,ch(05),~
               at (07,02), "Sort Order For Display (P or D)",            ~
               at (07,35), fac(lfac$(3%)), sort$                 ,ch(01),~
               at (10,02), "Print -  Traveler:",                         ~
               at (10,21), fac(lfac$(4%)), traveler$             ,ch(01),~
               at (10,24), "Pick List:",                                 ~
               at (10,35), fac(lfac$(4%)), picklist$             ,ch(01),~
               at (10,38), "By-Product List:",                           ~
               at (10,55), fac(lfac$(4%)), byprod$               ,ch(01),~
               at (10,57), "Single Level BOM:",                          ~
               at (10,75), fac(lfac$(4%)), slbom$                ,ch(01),~
               at (11,02), "         Vendor Service Sheet:",             ~
               at (11,33), fac(lfac$(4%)), vendsrvc$             ,ch(01),~
                                                                         ~
               at (22,02), fac(hex(a4)),   message$              ,ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(1%)          ,ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(2%)          ,ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 3% then L40920
                   call "GETCODE" (#4, " ", " ", 0%, 0, f1%(4%))
                   goto L40700

L40920:        if keyhit% <> 13% then L40955
                   call "MANUAL" ("JBRELSUB")
                   goto L40700

L40955:        if keyhit% <> 15% then L40990
                   call "PRNTSCRN"
                   goto L40700

L40990:        close ws
               call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
               return

L41000: REM *************************************************************~
            *    S E L E C T   A D V I C E S   T O   P R O C E S S      *~
            *-----------------------------------------------------------*~
            * Screen Page 2 input ('X' the boxes...).                   *~
            *************************************************************

        deffn'102
            lfaca$() = all(hex(8c))
            line2$ = "Select Work Order Advices to Release"
            str(line2$,62%) = "JBRELSUB: " & str(cms2v$,,8%)
            part_descr$() = " "
            for i% = 1% to min(16%, maxlines%-base%)
              part_descr$(i%) = part$(base%+i%)&" ("&partdescr$(base%+i%)
              part_descr$(i%) = part_descr$(i%) & ")"
              if str(part_descr$(i%), 48%) > " " then                    ~
                 str(part_descr$(i%), 47%) = ")"
              str(part_descr$(i%),49%) = ecr$(base%+i%)
              if search% = 0% then lfaca$(i%) = hex(81)
            next i%
            sprompt$ = " " : srchfac$ = hex(9c)
            if search% = 0% then L41500
               sprompt$ = "Search for Part:"
               srchfac$ = hex(81)

L41500: accept                                                           ~
               at (01,02), "Open Jobs - Release Planned Work Orders",    ~
               at (01,59), "Today's Date:", fac(hex(8c)), date$ , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,04), fac(hex(ac)), title$                 , ch(77),~
                                                                         ~
               at (05,02), fac(lfaca$(01)), box$(base% + 01)    , ch(01),~
               at (05,04), fac(hex(8c)), part_descr$    (01)    , ch(49),~
               at (05,54), fac(hex(8c)), stdate$(base% + 01)    , ch(08),~
               at (05,63), fac(hex(8c)), rqdate$(base% + 01)    , ch(08),~
               at (05,73), fac(hex(84)), qty$   (base% + 01)    , ch(08),~
                                                                         ~
               at (06,02), fac(lfaca$(02)), box$(base% + 02)    , ch(01),~
               at (06,04), fac(hex(8c)), part_descr$    (02)    , ch(49),~
               at (06,54), fac(hex(8c)), stdate$(base% + 02)    , ch(08),~
               at (06,63), fac(hex(8c)), rqdate$(base% + 02)    , ch(08),~
               at (06,73), fac(hex(84)), qty$   (base% + 02)    , ch(08),~
                                                                         ~
               at (07,02), fac(lfaca$(03)), box$(base% + 03)    , ch(01),~
               at (07,04), fac(hex(8c)), part_descr$    (03)    , ch(49),~
               at (07,54), fac(hex(8c)), stdate$(base% + 03)    , ch(08),~
               at (07,63), fac(hex(8c)), rqdate$(base% + 03)    , ch(08),~
               at (07,73), fac(hex(84)), qty$   (base% + 03)    , ch(08),~
                                                                         ~
               at (08,02), fac(lfaca$(04)), box$(base% + 04)    , ch(01),~
               at (08,04), fac(hex(8c)), part_descr$    (04)    , ch(49),~
               at (08,54), fac(hex(8c)), stdate$(base% + 04)    , ch(08),~
               at (08,63), fac(hex(8c)), rqdate$(base% + 04)    , ch(08),~
               at (08,73), fac(hex(84)), qty$   (base% + 04)    , ch(08),~
                                                                         ~
               at (09,02), fac(lfaca$(05)), box$(base% + 05)    , ch(01),~
               at (09,04), fac(hex(8c)), part_descr$    (05)    , ch(49),~
               at (09,54), fac(hex(8c)), stdate$(base% + 05)    , ch(08),~
               at (09,63), fac(hex(8c)), rqdate$(base% + 05)    , ch(08),~
               at (09,73), fac(hex(84)), qty$   (base% + 05)    , ch(08),~
                                                                         ~
               at (10,02), fac(lfaca$(06)), box$(base% + 06)    , ch(01),~
               at (10,04), fac(hex(8c)), part_descr$    (06)    , ch(49),~
               at (10,54), fac(hex(8c)), stdate$(base% + 06)    , ch(08),~
               at (10,63), fac(hex(8c)), rqdate$(base% + 06)    , ch(08),~
               at (10,73), fac(hex(84)), qty$   (base% + 06)    , ch(08),~
                                                                         ~
               at (11,02), fac(lfaca$(07)), box$(base% + 07)    , ch(01),~
               at (11,04), fac(hex(8c)), part_descr$    (07)    , ch(49),~
               at (11,54), fac(hex(8c)), stdate$(base% + 07)    , ch(08),~
               at (11,63), fac(hex(8c)), rqdate$(base% + 07)    , ch(08),~
               at (11,73), fac(hex(84)), qty$   (base% + 07)    , ch(08),~
                                                                         ~
               at (12,02), fac(lfaca$(08)), box$(base% + 08)    , ch(01),~
               at (12,04), fac(hex(8c)), part_descr$    (08)    , ch(49),~
               at (12,54), fac(hex(8c)), stdate$(base% + 08)    , ch(08),~
               at (12,63), fac(hex(8c)), rqdate$(base% + 08)    , ch(08),~
               at (12,73), fac(hex(84)), qty$   (base% + 08)    , ch(08),~
                                                                         ~
               at (13,02), fac(lfaca$(09)), box$(base% + 09)    , ch(01),~
               at (13,04), fac(hex(8c)), part_descr$    (09)    , ch(49),~
               at (13,54), fac(hex(8c)), stdate$(base% + 09)    , ch(08),~
               at (13,63), fac(hex(8c)), rqdate$(base% + 09)    , ch(08),~
               at (13,73), fac(hex(84)), qty$   (base% + 09)    , ch(08),~
                                                                         ~
               at (14,02), fac(lfaca$(10)), box$(base% + 10)    , ch(01),~
               at (14,04), fac(hex(8c)), part_descr$    (10)    , ch(49),~
               at (14,54), fac(hex(8c)), stdate$(base% + 10)    , ch(08),~
               at (14,63), fac(hex(8c)), rqdate$(base% + 10)    , ch(08),~
               at (14,73), fac(hex(84)), qty$   (base% + 10)    , ch(08),~
                                                                         ~
               at (15,02), fac(lfaca$(11)), box$(base% + 11)    , ch(01),~
               at (15,04), fac(hex(8c)), part_descr$    (11)    , ch(49),~
               at (15,54), fac(hex(8c)), stdate$(base% + 11)    , ch(08),~
               at (15,63), fac(hex(8c)), rqdate$(base% + 11)    , ch(08),~
               at (15,73), fac(hex(84)), qty$   (base% + 11)    , ch(08),~
                                                                         ~
               at (16,02), fac(lfaca$(12)), box$(base% + 12)    , ch(01),~
               at (16,04), fac(hex(8c)), part_descr$    (12)    , ch(49),~
               at (16,54), fac(hex(8c)), stdate$(base% + 12)    , ch(08),~
               at (16,63), fac(hex(8c)), rqdate$(base% + 12)    , ch(08),~
               at (16,73), fac(hex(84)), qty$   (base% + 12)    , ch(08),~
                                                                         ~
               at (17,02), fac(lfaca$(13)), box$(base% + 13)    , ch(01),~
               at (17,04), fac(hex(8c)), part_descr$    (13)    , ch(49),~
               at (17,54), fac(hex(8c)), stdate$(base% + 13)    , ch(08),~
               at (17,63), fac(hex(8c)), rqdate$(base% + 13)    , ch(08),~
               at (17,73), fac(hex(84)), qty$   (base% + 13)    , ch(08),~
                                                                         ~
               at (18,02), fac(lfaca$(14)), box$(base% + 14)    , ch(01),~
               at (18,04), fac(hex(8c)), part_descr$    (14)    , ch(49),~
               at (18,54), fac(hex(8c)), stdate$(base% + 14)    , ch(08),~
               at (18,63), fac(hex(8c)), rqdate$(base% + 14)    , ch(08),~
               at (18,73), fac(hex(84)), qty$   (base% + 14)    , ch(08),~
                                                                         ~
               at (19,02), fac(lfaca$(15)), box$(base% + 15)    , ch(01),~
               at (19,04), fac(hex(8c)), part_descr$    (15)    , ch(49),~
               at (19,54), fac(hex(8c)), stdate$(base% + 15)    , ch(08),~
               at (19,63), fac(hex(8c)), rqdate$(base% + 15)    , ch(08),~
               at (19,73), fac(hex(84)), qty$   (base% + 15)    , ch(08),~
                                                                         ~
               at (20,02), fac(lfaca$(16)), box$(base% + 16)    , ch(01),~
               at (20,04), fac(hex(8c)), part_descr$    (16)    , ch(49),~
               at (20,54), fac(hex(8c)), stdate$(base% + 16)    , ch(08),~
               at (20,63), fac(hex(8c)), rqdate$(base% + 16)    , ch(08),~
               at (20,73), fac(hex(84)), qty$   (base% + 16)    , ch(08),~
                                                                         ~
               at (21,02), fac(hex(8c)), sprompt$               , ch(20),~
               at (21,23), fac(srchfac$), spart$                , ch(25),~
                                                                         ~
               at (22,02), fac(hex(a4)), message$               , ch(79),~
               at (23,02),                                               ~
        "(1)StartOver (2)1st WOs (4)Prev WOs (5)Next WOs (11)ECR (13)Inst~
        ~r (15)Prnt Scrn",                                                ~
               at (24,02),"(RETURN)To Assign Job #s To Marked WOs.  Next ~
        ~Job# Is", fac(hex(8c)), autojob$,                                ~
                                                                         ~
               keys(hex(000102040508090a0b0d0f)),                        ~
               key(keyhit%)

            if keyhit% <> 13% then L42700
               call "MANUAL" ("JBRELSUB")
               goto L41000

L42700:     if keyhit% <> 15% then L42740
               call "PRNTSCRN"
               goto L41000

L42740:     if search% = 0% then L42880
               if spart$ = " " then L42840
                  if spart$ <> "FIRST" then L42753
                     base% = 0% : goto L42840
L42753:           if spart$ <> "LAST"  then L42760
                     base% = max(0%, maxlines% - 16%) : goto L42840
L42760:           sparth$ = spart$
                  len% = 1% + len(sparth$)
                  if len% < 26% then str(sparth$, len%) = all(hex(ff))
                  if part$(maxlines%) <= sparth$ then gosub dataload
                  search str(part$()) = spart$ to search%() step 25
                  if search%(1%) = 0% then L42840
                  base% = search%(1%) / 25%
                  base% = max(0%, min(array% - 16%, base%))
L42840:        keyhit% = 99%
               search% = 0%
               return

L42880:     if keyhit%  =  9% then L42890
            if keyhit%  = 10% then L42890
            if keyhit% <> 11% then return
L42890:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            if keyhit%  = 10% then L42940
            if keyhit%  = 11% then L42965
               call "COMPCHCK"(pipinkey$(cursor%(1)-4%+base%),           ~
                                       #34, #3, #54, #33)
               goto L41000
L42940:        call "GETDEM" (1%, pipinkey$(cursor%(1)-4%+base%),        ~
                              #35, #37, #33, " ", " ", 0%)
               goto L41000
L42965:     REM *** Display ECR Info (if any) ***
            if ecr$(cursor%(1%)-4%+base%) = " " then L41000
            call "ECRINQSB" ("A",        /* "A" - Show ALL ECRs this   */~
                                         /*    part, open or closed.   */~
             part$(cursor%(1%)-4%+base%),/* Part to Do Inquiry/Check on*/~
                             ecrpfk$,    /* IN:  PFKey # to Use        */~
                             #54,        /* SYSFILE2                   */~
                             #03)        /* HNYMASTR                   */
            goto L41000

L43000: REM *************************************************************~
            *         A S S I G N   J O B   N U M B E R S               *~
            *-----------------------------------------------------------*~
            * Screen page 3 data entry.                                 *~
            * Enter Job Numbers, review Work In Process account.        *~
            *************************************************************

        deffn'103(fieldnr%)
            title1$= "    Job #     Part Number               Wip Account~
        ~  Start By  Need By      Qty"
            line2$ = "Assign Job Numbers and Work In Process Accounts"
            str(line2$,62%) = "JBRELSUB: " & str(cms2v$,,8%)
            lfacc$() = all(hex(8c))
            lfacb$() = all(hex(9c))
            if fieldnr% > 0% and fieldnr% < 4% then                      ~
               lfacc$(i%-base%,fieldnr%) = hex(81)
            str(lfacb$(),,min(16%,maxlines%)) = all(hex(8c))
            if fieldnr% = 0% and keyhit% <> 12% then                     ~
                str(lfacc$(),,min(16%,maxlines%)*4%) = all(hex(86))
            if errorline% = 0% then L43180
            if errorline% < base% or errorline% > base% + 16% then base%=~
                                max(0, min(errorline% - 1%, maxlines%-16))
            lfacb$(errorline%-base%) = hex(94)
            if keyhit% = 12% then lfacc$(errorline%-base%,1%) = hex(94)

L43180: accept                                                           ~
               at (01,02), "Open Jobs - Release Planned Work Orders",    ~
               at (01,59), "Today's Date:", fac(hex(8c)), date$ , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), fac(hex(ac)), title1$                , ch(79),~
                                                                         ~
               at (05,02), fac(lfacb$(01)),line$    (base%+ 01) , ch(03),~
               at (05,06), fac(lfacc$(01,1)),jobnr$(base% + 01) , ch(08),~
               at (05,16), fac(hex(8c)), part$     (base% + 01) , ch(25),~
               at (05,42), fac(lfacc$(01,2)),wipacct$(base%+ 01), ch(12),~
               at (05,55), fac(hex(8c)), stdate$   (base% + 01) , ch(08),~
               at (05,64), fac(hex(8c)), rqdate$   (base% + 01) , ch(08),~
               at (05,73), fac(lfacc$(01,3)),qty$  (base% + 01),  ch(08),~
                                                                         ~
               at (06,02), fac(lfacb$(02)),line$    (base%+ 02) , ch(03),~
               at (06,06), fac(lfacc$(02,1)),jobnr$ (base%+ 02) , ch(08),~
               at (06,16), fac(hex(8c)), part$     (base% + 02) , ch(25),~
               at (06,42), fac(lfacc$(02,2)),wipacct$(base%+ 02), ch(12),~
               at (06,55), fac(hex(8c)), stdate$   (base% + 02) , ch(08),~
               at (06,64), fac(hex(8c)), rqdate$   (base% + 02) , ch(08),~
               at (06,73), fac(lfacc$(02,3)),qty$  (base% + 02),  ch(08),~
                                                                         ~
               at (07,02), fac(lfacb$(03)),line$    (base%+ 03) , ch(03),~
               at (07,06), fac(lfacc$(03,1)),jobnr$ (base%+ 03) , ch(08),~
               at (07,16), fac(hex(8c)), part$     (base% + 03) , ch(25),~
               at (07,42), fac(lfacc$(03,2)),wipacct$(base%+ 03), ch(12),~
               at (07,55), fac(hex(8c)), stdate$   (base% + 03) , ch(08),~
               at (07,64), fac(hex(8c)), rqdate$   (base% + 03) , ch(08),~
               at (07,73), fac(lfacc$(03,3)),qty$  (base% + 03),  ch(08),~
                                                                         ~
               at (08,02), fac(lfacb$(04)),line$    (base%+ 04) , ch(03),~
               at (08,06), fac(lfacc$(04,1)),jobnr$ (base%+ 04) , ch(08),~
               at (08,16), fac(hex(8c)), part$     (base% + 04) , ch(25),~
               at (08,42), fac(lfacc$(04,2)),wipacct$(base%+ 04), ch(12),~
               at (08,55), fac(hex(8c)), stdate$   (base% + 04) , ch(08),~
               at (08,64), fac(hex(8c)), rqdate$   (base% + 04) , ch(08),~
               at (08,73), fac(lfacc$(04,3)),qty$  (base% + 04),  ch(08),~
                                                                         ~
               at (09,02), fac(lfacb$(05)),line$    (base%+ 05) , ch(03),~
               at (09,06), fac(lfacc$(05,1)),jobnr$ (base%+ 05) , ch(08),~
               at (09,16), fac(hex(8c)), part$     (base% + 05) , ch(25),~
               at (09,42), fac(lfacc$(05,2)),wipacct$(base%+ 05), ch(12),~
               at (09,55), fac(hex(8c)), stdate$   (base% + 05) , ch(08),~
               at (09,64), fac(hex(8c)), rqdate$   (base% + 05) , ch(08),~
               at (09,73), fac(lfacc$(05,3)),qty$  (base% + 05),  ch(08),~
                                                                         ~
               at (10,02), fac(lfacb$(06)),line$    (base%+ 06) , ch(03),~
               at (10,06), fac(lfacc$(06,1)),jobnr$ (base%+ 06) , ch(08),~
               at (10,16), fac(hex(8c)), part$     (base% + 06) , ch(25),~
               at (10,42), fac(lfacc$(06,2)),wipacct$(base%+ 06), ch(12),~
               at (10,55), fac(hex(8c)), stdate$   (base% + 06) , ch(08),~
               at (10,64), fac(hex(8c)), rqdate$   (base% + 06) , ch(08),~
               at (10,73), fac(lfacc$(06,3)),qty$  (base% + 06),  ch(08),~
                                                                         ~
               at (11,02), fac(lfacb$(07)),line$    (base%+ 07) , ch(03),~
               at (11,06), fac(lfacc$(07,1)),jobnr$ (base%+ 07) , ch(08),~
               at (11,16), fac(hex(8c)), part$     (base% + 07) , ch(25),~
               at (11,42), fac(lfacc$(07,2)),wipacct$(base%+ 07), ch(12),~
               at (11,55), fac(hex(8c)), stdate$   (base% + 07) , ch(08),~
               at (11,64), fac(hex(8c)), rqdate$   (base% + 07) , ch(08),~
               at (11,73), fac(lfacc$(07,3)),qty$  (base% + 07),  ch(08),~
                                                                         ~
               at (12,02), fac(lfacb$(08)),line$    (base%+ 08) , ch(03),~
               at (12,06), fac(lfacc$(08,1)),jobnr$ (base%+ 08) , ch(08),~
               at (12,16), fac(hex(8c)), part$     (base% + 08) , ch(25),~
               at (12,42), fac(lfacc$(08,2)),wipacct$(base%+ 08), ch(12),~
               at (12,55), fac(hex(8c)), stdate$   (base% + 08) , ch(08),~
               at (12,64), fac(hex(8c)), rqdate$   (base% + 08) , ch(08),~
               at (12,73), fac(lfacc$(08,3)),qty$  (base% + 08),  ch(08),~
                                                                         ~
               at (13,02), fac(lfacb$(09)),line$    (base%+ 09) , ch(03),~
               at (13,06), fac(lfacc$(09,1)),jobnr$ (base%+ 09) , ch(08),~
               at (13,16), fac(hex(8c)), part$     (base% + 09) , ch(25),~
               at (13,42), fac(lfacc$(09,2)),wipacct$(base%+ 09), ch(12),~
               at (13,55), fac(hex(8c)), stdate$   (base% + 09) , ch(08),~
               at (13,64), fac(hex(8c)), rqdate$   (base% + 09) , ch(08),~
               at (13,73), fac(lfacc$(09,3)),qty$  (base% + 09),  ch(08),~
                                                                         ~
               at (14,02), fac(lfacb$(10)),line$    (base%+ 10) , ch(03),~
               at (14,06), fac(lfacc$(10,1)),jobnr$ (base%+ 10) , ch(08),~
               at (14,16), fac(hex(8c)), part$     (base% + 10) , ch(25),~
               at (14,42), fac(lfacc$(10,2)),wipacct$(base%+ 10), ch(12),~
               at (14,55), fac(hex(8c)), stdate$   (base% + 10) , ch(08),~
               at (14,64), fac(hex(8c)), rqdate$   (base% + 10) , ch(08),~
               at (14,73), fac(lfacc$(10,3)),qty$  (base% + 10),  ch(08),~
                                                                         ~
               at (15,02), fac(lfacb$(11)),line$    (base%+ 11) , ch(03),~
               at (15,06), fac(lfacc$(11,1)),jobnr$ (base%+ 11) , ch(08),~
               at (15,16), fac(hex(8c)), part$     (base% + 11) , ch(25),~
               at (15,42), fac(lfacc$(11,2)),wipacct$(base%+ 11), ch(12),~
               at (15,55), fac(hex(8c)), stdate$   (base% + 11) , ch(08),~
               at (15,64), fac(hex(8c)), rqdate$   (base% + 11) , ch(08),~
               at (15,73), fac(lfacc$(11,3)),qty$  (base% + 11),  ch(08),~
                                                                         ~
               at (16,02), fac(lfacb$(12)),line$    (base%+ 12) , ch(03),~
               at (16,06), fac(lfacc$(12,1)),jobnr$ (base%+ 12) , ch(08),~
               at (16,16), fac(hex(8c)), part$     (base% + 12) , ch(25),~
               at (16,42), fac(lfacc$(12,2)),wipacct$(base%+ 12), ch(12),~
               at (16,55), fac(hex(8c)), stdate$   (base% + 12) , ch(08),~
               at (16,64), fac(hex(8c)), rqdate$   (base% + 12) , ch(08),~
               at (16,73), fac(lfacc$(12,3)),qty$  (base% + 12),  ch(08),~
                                                                         ~
               at (17,02), fac(lfacb$(13)),line$    (base%+ 13) , ch(03),~
               at (17,06), fac(lfacc$(13,1)),jobnr$ (base%+ 13) , ch(08),~
               at (17,16), fac(hex(8c)), part$     (base% + 13) , ch(25),~
               at (17,42), fac(lfacc$(13,2)),wipacct$(base%+ 13), ch(12),~
               at (17,55), fac(hex(8c)), stdate$   (base% + 13) , ch(08),~
               at (17,64), fac(hex(8c)), rqdate$   (base% + 13) , ch(08),~
               at (17,73), fac(lfacc$(13,3)),qty$  (base% + 13),  ch(08),~
                                                                         ~
               at (18,02), fac(lfacb$(14)),line$    (base%+ 14) , ch(03),~
               at (18,06), fac(lfacc$(14,1)),jobnr$ (base%+ 14) , ch(08),~
               at (18,16), fac(hex(8c)), part$     (base% + 14) , ch(25),~
               at (18,42), fac(lfacc$(14,2)),wipacct$(base%+ 14), ch(12),~
               at (18,55), fac(hex(8c)), stdate$   (base% + 14) , ch(08),~
               at (18,64), fac(hex(8c)), rqdate$   (base% + 14) , ch(08),~
               at (18,73), fac(lfacc$(14,3)),qty$  (base% + 14),  ch(08),~
                                                                         ~
               at (19,02), fac(lfacb$(15)),line$    (base%+ 15) , ch(03),~
               at (19,06), fac(lfacc$(15,1)),jobnr$ (base%+ 15) , ch(08),~
               at (19,16), fac(hex(8c)), part$     (base% + 15) , ch(25),~
               at (19,42), fac(lfacc$(15,2)),wipacct$(base%+ 15), ch(12),~
               at (19,55), fac(hex(8c)), stdate$   (base% + 15) , ch(08),~
               at (19,64), fac(hex(8c)), rqdate$   (base% + 15) , ch(08),~
               at (19,73), fac(lfacc$(15,3)),qty$  (base% + 15),  ch(08),~
                                                                         ~
               at (20,02), fac(lfacb$(16)),line$    (base%+ 16) , ch(03),~
               at (20,06), fac(lfacc$(16,1)),jobnr$ (base%+ 16) , ch(08),~
               at (20,16), fac(hex(8c)), part$     (base% + 16) , ch(25),~
               at (20,42), fac(lfacc$(16,2)),wipacct$(base%+ 16), ch(12),~
               at (20,55), fac(hex(8c)), stdate$   (base% + 16) , ch(08),~
               at (20,64), fac(hex(8c)), rqdate$   (base% + 16) , ch(08),~
               at (20,73), fac(lfacc$(16,3)),qty$  (base% + 16),  ch(08),~
                                                                         ~
               at (22,02), fac(hex(a4)), message$               , ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(01)           , ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(02)           , ch(79),~
               at (24,39), fac(sfac$),   store$                 , ch(03),~
               at (24,48), fac(lfac$),   lot$                   , ch(06),~
                                                                         ~
               keys(hex(000102040508090a0c0d0e0f10)),                    ~
               key(keyhit%)

            if keyhit% <> 14% then L44710
               call "GETCODE" (#4, " ", " ", 0%, 0, f1%(4))
               goto L43000

L44710:     if keyhit% <> 13% then L44750
               call "MANUAL" ("JBRELSUB")
               goto L43000

L44750:     if keyhit% <> 15% then L44790
               call "PRNTSCRN"
               goto L43000

L44790:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return
            u3% = u3%

        cntrl_field_selected
            message$  = "To Modify Displayed Values, Position Cursor" &  ~
                           " to Desired Value & Press (RETURN)."
            pfdescr$(1) =                                                ~
        "(1)ReStrt (2)1st (4)Prev (5)Nxt (8)Date/Qty         (12)Del (13)~
        ~Inst (15)PrtScr"
            pfdescr$(2) = "(10)Release Jobs AND Auto Kit: Store"
            str(pfdescr$(2),42%) = "Lot"
            str(pfdescr$(2),57%) = "(16)Release Jobs (Only)"
            lfac$, sfac$ = hex(81)
            gosub'104(0%)
               errormsg$ = " "
               errorline% = 0
               if keyhit% = 8 then show_wos_selected
               if keyhit% = 1 then gosub startover
               if keyhit% = 2 then base% = 0
               if keyhit% = 4 then base% = max(base%-12, 0)
               if keyhit% = 5 then base%=max(0,min(maxlines%-16,base%+12))
               if keyhit% = 10 then cntrl_field_check_auto_kit
               if keyhit% = 16 then cntrl_field_data_save
               oldi%, oldfield% = 0%
L46535:        if cursor%(1) < 5% or cursor%(1)>20% then                 ~
                                                    cntrl_field_selected
               i% = cursor%(1) - 4% + base%
               if i% < 1% or i% > maxlines% then cntrl_field_selected
               if keyhit% <> 12% then L46620
                   errorline% = i%
                   errormsg$ = "Press RETURN to Remove this Line OR " &  ~
                               "Press PF1 to Cancel Delete."
L46570:            gosub'104(0%)
                   if keyhit% = 1% then cntrl_field_selected
                   if keyhit% <> 0% then L46570
                   box$(i%) = " "
                   call "SERSTOVR" (index%(i%), "6", " ",  #6, #7)
                   gosub squish_array
                   errormsg$ = " "
                   errorline% = 0%
                   if maxlines% < 1% then inputmode1
                   goto cntrl_field_selected
L46620:        if keyhit% <> 0% then cntrl_field_selected
               if cursor%(2) < 16% then fieldnr% = 1% else fieldnr% = 2%
               if cursor%(2) > 54% then fieldnr% = 4%
               if i% = oldi% and fieldnr% = oldfield% then               ~
                   cntrl_field_selected
               if fieldnr% = 3% then L46695
               if fieldnr% = 4% and demand_opt$ <> "B" then L46695
               pfdescr$(1) = "(1)Start Over                         (1" &~
                             "4)See Jobs on File     "
               str(pfdescr$(1),64%) = "(13)Instructions"
               pfdescr$(2) = " "
               str(pfdescr$(2),64%) = "(15)Print Screen"
               lfac$, sfac$ = hex(9c)
               message$ = " "
L46680:        gosub'104(fieldnr%)
                  if keyhit% = 1% then gosub startover
                  if keyhit% <> 0% then L46680
L46695:        gosub'153(fieldnr%)
                  if errormsg$ > " " then L46680
                  oldi% = i%
                  oldfield% = fieldnr%
                  goto L46535

L46930: deffn'104(fieldnr%)
            title1$= "    Job #     Part Number               Wip Account~
        ~  Cntrl # (Demand Code/Line)"
            line2$ = "Assign Job Numbers, WIP Accounts, & Control #  "
            str(line2$,62%) = "JBRELSUB: " & str(cms2v$,,8%)
            lfacc$() = all(hex(8c))
            lfacb$() = all(hex(9c))


            if fieldnr% > 0% and fieldnr% < 5% then                      ~
               lfacc$(i%-base%,fieldnr%) = hex(81)
            if fieldnr% = 4% and demand_opt$ = "B"                       ~
               then lfacc$(i%-base%,fieldnr%) = hex(81)
            str(lfacb$(),,min(16%,maxlines%)) = all(hex(8c))
            if fieldnr% = 0% and keyhit% <> 12% then                     ~
                str(lfacc$(),,min(16%,maxlines%)*4%) = all(hex(86))
            if errorline% = 0% then L47110
            if errorline% < base% or errorline% > base% + 16% then base%=~
                                max(0, min(errorline% - 1%, maxlines%-16))
            lfacb$(errorline%-base%) = hex(94)
            if keyhit% = 12% then lfacc$(errorline%-base%,1%) = hex(94)

L47110: accept                                                           ~
               at (01,02), "Open Jobs - Release Planned Work Orders",    ~
               at (01,59), "Today's Date:", fac(hex(8c)), date$ , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), fac(hex(ac)), title1$                , ch(79),~
                                                                         ~
               at (05,02), fac(lfacb$(01)),line$    (base%+ 01) , ch(03),~
               at (05,06), fac(lfacc$(01,1)),jobnr$(base% + 01) , ch(08),~
               at (05,16), fac(hex(8c)), part$     (base% + 01) , ch(25),~
               at (05,42), fac(lfacc$(01,2)),wipacct$(base%+ 01), ch(12),~
           at (05,55), fac(lfacc$(01,4)),demcodeline$(base%+ 01), ch(19),~
                                                                         ~
               at (06,02), fac(lfacb$(02)),line$    (base%+ 02) , ch(03),~
               at (06,06), fac(lfacc$(02,1)),jobnr$ (base%+ 02) , ch(08),~
               at (06,16), fac(hex(8c)), part$     (base% + 02) , ch(25),~
               at (06,42), fac(lfacc$(02,2)),wipacct$(base%+ 02), ch(12),~
           at (06,55), fac(lfacc$(02,4)),demcodeline$(base%+ 02), ch(19),~
                                                                         ~
               at (07,02), fac(lfacb$(03)),line$    (base%+ 03) , ch(03),~
               at (07,06), fac(lfacc$(03,1)),jobnr$ (base%+ 03) , ch(08),~
               at (07,16), fac(hex(8c)), part$     (base% + 03) , ch(25),~
               at (07,42), fac(lfacc$(03,2)),wipacct$(base%+ 03), ch(12),~
           at (07,55), fac(lfacc$(03,4)),demcodeline$(base%+ 03), ch(19),~
                                                                         ~
               at (08,02), fac(lfacb$(04)),line$    (base%+ 04) , ch(03),~
               at (08,06), fac(lfacc$(04,1)),jobnr$ (base%+ 04) , ch(08),~
               at (08,16), fac(hex(8c)), part$     (base% + 04) , ch(25),~
               at (08,42), fac(lfacc$(04,2)),wipacct$(base%+ 04), ch(12),~
           at (08,55), fac(lfacc$(04,4)),demcodeline$(base%+ 04), ch(19),~
                                                                         ~
               at (09,02), fac(lfacb$(05)),line$    (base%+ 05) , ch(03),~
               at (09,06), fac(lfacc$(05,1)),jobnr$ (base%+ 05) , ch(08),~
               at (09,16), fac(hex(8c)), part$     (base% + 05) , ch(25),~
               at (09,42), fac(lfacc$(05,2)),wipacct$(base%+ 05), ch(12),~
           at (09,55), fac(lfacc$(05,4)),demcodeline$(base%+ 05), ch(19),~
                                                                         ~
               at (10,02), fac(lfacb$(06)),line$    (base%+ 06) , ch(03),~
               at (10,06), fac(lfacc$(06,1)),jobnr$ (base%+ 06) , ch(08),~
               at (10,16), fac(hex(8c)), part$     (base% + 06) , ch(25),~
               at (10,42), fac(lfacc$(06,2)),wipacct$(base%+ 06), ch(12),~
           at (10,55), fac(lfacc$(06,4)),demcodeline$(base%+ 06), ch(19),~
                                                                         ~
               at (11,02), fac(lfacb$(07)),line$    (base%+ 07) , ch(03),~
               at (11,06), fac(lfacc$(07,1)),jobnr$ (base%+ 07) , ch(08),~
               at (11,16), fac(hex(8c)), part$     (base% + 07) , ch(25),~
               at (11,42), fac(lfacc$(07,2)),wipacct$(base%+ 07), ch(12),~
           at (11,55), fac(lfacc$(07,4)),demcodeline$(base%+ 07), ch(19),~
                                                                         ~
               at (12,02), fac(lfacb$(08)),line$    (base%+ 08) , ch(03),~
               at (12,06), fac(lfacc$(08,1)),jobnr$ (base%+ 08) , ch(08),~
               at (12,16), fac(hex(8c)), part$     (base% + 08) , ch(25),~
               at (12,42), fac(lfacc$(08,2)),wipacct$(base%+ 08), ch(12),~
           at (12,55), fac(lfacc$(08,4)),demcodeline$(base%+ 08), ch(19),~
                                                                         ~
               at (13,02), fac(lfacb$(09)),line$    (base%+ 09) , ch(03),~
               at (13,06), fac(lfacc$(09,1)),jobnr$ (base%+ 09) , ch(08),~
               at (13,16), fac(hex(8c)), part$     (base% + 09) , ch(25),~
               at (13,42), fac(lfacc$(09,2)),wipacct$(base%+ 09), ch(12),~
           at (13,55), fac(lfacc$(09,4)), demcodeline$(base%+09), ch(19),~
                                                                         ~
               at (14,02), fac(lfacb$(10)),line$    (base%+ 10) , ch(03),~
               at (14,06), fac(lfacc$(10,1)),jobnr$ (base%+ 10) , ch(08),~
               at (14,16), fac(hex(8c)), part$     (base% + 10) , ch(25),~
               at (14,42), fac(lfacc$(10,2)),wipacct$(base%+ 10), ch(12),~
           at (14,55), fac(lfacc$(10,4)), demcodeline$(base%+10), ch(19),~
                                                                         ~
               at (15,02), fac(lfacb$(11)),line$    (base%+ 11) , ch(03),~
               at (15,06), fac(lfacc$(11,1)),jobnr$ (base%+ 11) , ch(08),~
               at (15,16), fac(hex(8c)), part$     (base% + 11) , ch(25),~
               at (15,42), fac(lfacc$(11,2)),wipacct$(base%+ 11), ch(12),~
           at (15,55), fac(lfacc$(11,4)), demcodeline$(base%+11), ch(19),~
                                                                         ~
               at (16,02), fac(lfacb$(12)),line$    (base%+ 12) , ch(03),~
               at (16,06), fac(lfacc$(12,1)),jobnr$ (base%+ 12) , ch(08),~
               at (16,16), fac(hex(8c)), part$     (base% + 12) , ch(25),~
               at (16,42), fac(lfacc$(12,2)),wipacct$(base%+ 12), ch(12),~
           at (16,55), fac(lfacc$(12,4)), demcodeline$(base%+12), ch(19),~
                                                                         ~
               at (17,02), fac(lfacb$(13)),line$    (base%+ 13) , ch(03),~
               at (17,06), fac(lfacc$(13,1)),jobnr$ (base%+ 13) , ch(08),~
               at (17,16), fac(hex(8c)), part$     (base% + 13) , ch(25),~
               at (17,42), fac(lfacc$(13,2)),wipacct$(base%+ 13), ch(12),~
           at (17,55), fac(lfacc$(13,4)), demcodeline$(base%+13), ch(19),~
                                                                         ~
               at (18,02), fac(lfacb$(14)),line$    (base%+ 14) , ch(03),~
               at (18,06), fac(lfacc$(14,1)),jobnr$ (base%+ 14) , ch(08),~
               at (18,16), fac(hex(8c)), part$     (base% + 14) , ch(25),~
               at (18,42), fac(lfacc$(14,2)),wipacct$(base%+ 14), ch(12),~
           at (18,55), fac(lfacc$(14,4)), demcodeline$(base%+14), ch(19),~
                                                                         ~
               at (19,02), fac(lfacb$(15)),line$    (base%+ 15) , ch(03),~
               at (19,06), fac(lfacc$(15,1)),jobnr$ (base%+ 15) , ch(08),~
               at (19,16), fac(hex(8c)), part$     (base% + 15) , ch(25),~
               at (19,42), fac(lfacc$(15,2)),wipacct$(base%+ 15), ch(12),~
           at (19,55), fac(lfacc$(15,4)), demcodeline$(base%+15), ch(19),~
                                                                         ~
               at (20,02), fac(lfacb$(16)),line$    (base%+ 16) , ch(03),~
               at (20,06), fac(lfacc$(16,1)),jobnr$ (base%+ 16) , ch(08),~
               at (20,16), fac(hex(8c)), part$     (base% + 16) , ch(25),~
               at (20,42), fac(lfacc$(16,2)),wipacct$(base%+ 16), ch(12),~
           at (20,55), fac(lfacc$(16,4)), demcodeline$(base%+16), ch(19),~
                                                                         ~
               at (22,02), fac(hex(a4)), message$               , ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(01)           , ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(02)           , ch(79),~
               at (24,39), fac(sfac$),   store$                 , ch(03),~
               at (24,48), fac(lfac$),   lot$                   , ch(06),~
                                                                         ~
               keys(hex(0001020405080a0c0d0e0f10)),                      ~
               key(keyhit%)

            if keyhit% <> 14% then L48590
               call "GETCODE" (#4, " ", " ", 0%, 0, f1%(4))
               goto L46930

L48590:     if keyhit% <> 13% then L48630
               call "MANUAL" ("JBRELSUB")
               goto L46930

L48630:     if keyhit% <> 15% then L48670
               call "PRNTSCRN"
               goto L46930

L48670:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return
            u3% = u3%

        cntrl_field_check_auto_kit
            gosub'152
               if errormsg$ <> " " then cntrl_field_selected
               goto L48760
        cntrl_field_data_save
            store$ = " "
L48760:     if errormsg$ <> " " then cntrl_field_selected
            gosub datasave
            goto inputmode_special

        REM *************************************************************~
            *                   T E S T   D A T A                       *~
            *-----------------------------------------------------------*~
            * Validate starting parameters on screen page 1.            *~
            *************************************************************

        deffn'151(fieldnr%)

                  errormsg$ = " "
                  on fieldnr% gosub L50150,         /* NEXT JOB NUMBER  */~
                                    L50340,         /* INCREMENT        */~
                                    L50390,         /* SORT OPTION      */~
                                    L50430,         /* START VALUE      */~
                                    L50500,         /* DEMAND CODE      */~
                                    L50700          /* Print Options    */
                  return

L50150:     REM TEST DATA FOR NEXT JOB NUMBER
                if autojob$ = " " then return
                if test% <> 0% then L50170
                call "READ100" (#4, autojob$, f1%(4))
                   if f1%(4) = 0% then L50170
                errormsg$="This Job Number has already been Assigned!"
                return

L50170:         REM Find Portion Of Job Number To Increment...
                temp$ = autojob$
                for i% = 1 to 8
                     if str(temp$,i%,1) < "0" or str(temp$,i%,1) > "9"   ~
                                               then str(temp$,i%,1) = " "
                next i%
                temp% = pos(-temp$<>" ")
                     if temp% = 0% then L50310
                auto%(1) = pos(-str(temp$,,temp%)=" ")+1
                auto%(2) = (temp% - auto%(1)) + 1
                if auto%(2%) < 3% then L50310
                convert str(autojob$,auto%(1),auto%(2)) to job%,         ~
                                                        data goto L50310
                if autojob$ = savejob$ then return
L50286:         keyhit% = 0%
                call "ASKUSER" (keyhit%, "*** RESET JOB DEFAULT ***",    ~
                     "RETURN - Reset Job Number For This Session Only",  ~
                     "PF16  - Replace Job Number Default",               ~
                     "PF1  - Return Without Resetting Or Replacing")
                if keyhit% = 1% then goto inputmode
                    if keyhit% <> 0% then L50300
                      reset% = 0% : return
L50300:                 if keyhit% <> 16% then L50286
                            reset% = 1%
                            keyhit% = 0%
                            return
L50310:         errormsg$ = "Next Job Nmbr Must Have At Least 3 Consecu"&~
                                "tive Nmbrs In It For Incrementing."
                return
L50340:     REM TEST DATA FOR INCREMENT
                if autojob$ = " " then return
                call "NUMTEST" (increment$, 1, 10000, errormsg$, 0, temp)
                increment% = temp
                return
L50390:     REM TEST DATA FOR SORT OPTION
                if pos("PD" = sort$) <> 0 then return
                errormsg$ = "Please Enter 'D' or 'P'"
                return
L50430:     REM TEST DATA FOR START VALUE
                if sort$ <> "D" then return
                if start$() = " " or start$() = blankdate$ then return
                call "DATEOK" (str(start$(),,8), 0%, errormsg$)
                return

L50500:     REM TEST DATA FOR DEMAND CODE
                if demcode$ <> " " then L50510
                   dline$ = " " : return
L50510:         call "STRING" addr("RJ", dline$, 3%)
                plowkey$ = str(demcode$,,16%) & dline$
                call "REDALT0" (#37, plowkey$, 1%, f1%(37%))
                   if f1%(37%) <> 0% then L50560
                errormsg$ = "Demand Code & Line not on file."
                return
L50560:         str(plowkey$,20%) = "WO"
                call "PLOWNEXT" (#35, plowkey$, 21%, f1%(35%))
                   if f1%(35%) <> 0% then return
                errormsg$ = "Demand Code & Line not linked to " &        ~
                            "unreleased advices."
                return

L50700: REM Test Data For Print Options...
            if pos("YN" = traveler$) <> 0% then L50730
                goto L50780
L50730:     if pos("YN" = picklist$) <> 0% then L50750
                goto L50780
L50750:     if pos("YN" = byprod$) <> 0% then L50770
                goto L50780
L50770:     if pos("YN" = slbom$) <> 0% then L50778
                goto L50780
L50778:     if pos("YN" = vendsrvc$) <> 0% then return
L50780:         errormsg$ = "Please Enter Y or N"
                return

        REM *************************************************************~
            *                   T E S T   D A T A                       *~
            *-----------------------------------------------------------*~
            *   Test data for Auto Kitting on Screen 3.                 *~
            *************************************************************
        deffn'152
            errorline% = 0%
            errormsg$ = " "
            call "GETCODE" (#59, store$, " ", 0%, 0, f1%(59))
                if f1%(59) = 0 then errormsg$ = "Invalid Store Number For~
        ~ Kit Complete."
            if errormsg$ > " " then return
*          IF MAX(SENABLED%()) = 0% THEN RETURN
*          ERRORMSG$ = "Cannot Auto-Kit Serial Numbered Parts"
*          FOR X% = 1% TO MAXLINES%
*              IF SENABLED%(X%) > 0% THEN ERRORLINE% = X%
*          NEXT X%
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            *  Tests data of items on screen page 3.                    *~
            *  Validates entered job numbers and checks WIP Account     *~
            *  numbers.                                                 *~
            *************************************************************
        deffn'153(fieldnr%)
            errorline% = 0%
            errormsg$ = " "
            on fieldnr% gosub L52150,     /* Job Number                 */~
                              L52340,     /* WIP Account                */~
                              L52430      /* Serial Numbers             */
            if errormsg$ > " " then errorline% = i%
            return

L52150: REM Edit Job Number Entry
            if jobnr$(i%) > " " then L52200
                errormsg$ = "Job Number Cannot be Blank! (Use PF12 if "  ~
                          & "You Don't want to Release this Advice)."
                return
L52200:     if str(jobnr$(i%),,2) <> "RW" then goto L52230
                errormsg$ = "You May Not Set Up Rework Jobs Here"
                return
L52230:     call "READ100" (#4, jobnr$(i%), f1%(4))
            if f1%(4) = 0% then L52270
                errormsg$="This Job Number has already been Assigned!"
                return
L52270:     search%(2) = 0%  /* SEARCH won't init this */
            temp$ = jobnr$(i%)
            search jobnr$() = str(temp$,,8) to search%() step 8
                if search%(2) = 0% then goto L52321
                errormsg$="You Are Trying To Assign This Job Number Twice"
                return
L52321:     call "READ100" (#60, jobnr$(i%), f1%(60))
            if f1%(60) = 0% then return
                errormsg$="Job Number may not also be a PROJECT Number"
                return

L52340: REM Edit Wip Account Entry
            incl_excl(1) = 40.01 : incl_excl$(1) = "A"
            header$(1) = "  Asset Acct  Description"
            call "PLOWCODE" (#12, wipacct$(i%), " ", 5000%, .30, f1%(12),~
                             header$(),0,0,incl_excl(),incl_excl$())
            if f1%(12) = 1% then return
                errormsg$ = "Work In Process Account is either "   &     ~
                            "not found or is not an Asset Account!"
                return

L52430: REM Test for Serial Numbered Parts
            least = .01 : most = 9e7
            call "SERENABL" (part$(i%),senabled%(i%), u3%, #54, #3)
               if senabled%(i%) = 0% then L52460
                  least = .01 : most = 1000
L52460:     call "NUMTEST" (qty$(i%), least, most, errormsg$, -.0001, qty)
               if errormsg$ > " " then return
*          CALL "SERENABL" (PART$(I%),SENABLED%(I%), U3%, #54, #3)
               if senabled%(i%) = 0% then return
            plowkey$ = jobnr$(i%)
            call "SERINSUB" (part$(i%)," "," ",qty, index%(i%),maxlines%,~
                             "WP", plowkey$, errormsg$, #54, #3, #6, #7)
            return


        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_routine
            if postflag% = 0% then call "JBJNLCLS"                       ~
                           ("J1", userid$, modno$, jnlid$, pstseq%, 0%)
            close printer
            call "FILEBGON" (#7)
            end
