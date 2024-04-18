        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  W   W   CCC   DDDD    SSS   PPPP   L      Y   Y          *~
            *  W   W  C   C  D   D  S      P   P  L       Y Y           *~
            *  W   W  C      D   D   SSS   PPPP   L        Y            *~
            *  W W W  C   C  D   D      S  P      L        Y            *~
            *   W W    CCC   DDDD    SSS   P      LLLLL    Y            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * WCDSPLY  - Display & Inquiry to Work Center Master File.  *~
            *            Also can review the units available and used   *~
            *            by day a month at a time.  Can also display    *~
            *            Work Center Loading.                           *~
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
            * 01/13/89 ! ORIGINAL (Stripped from WCINPUT)         ! MJB *~
            * 02/02/89 ! Changed Plant to Facility.  Dimensioned  ! TLJ *~
            *          ! INPMESSAGE$.  Formatted FMDATE$ and      !     *~
            *          ! TODATE$ when errors occurred.  Set       !     *~
            *          ! INPMESSAGE$ = EDITMSG$ when in edit_many.!     *~
            * 02/17/89 ! Changed Usage Display Based on date.     ! MJB *~
            * 04/10/90 ! Increased size of monthly usage fields.  ! JDH *~
            *          !  'PF5' more data for daily details screen!     *~
            *          !  now works.  Added page #s to report.    !     *~
            * 04/16/91 ! PRR 11796  Rounded the WCs Percent       ! SID *~
            *          !            Utilization.                  !     *~
            * 06/20/91 ! Added 'ALLFREE'                          ! SID *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 03/12/92 ! PRR 12318 Modified the Cursor Position   ! SID *~
            * 04/02/92 ! PRR 12324 - Fixed erroneous PFkey labels ! MLJ *~
            *          !   on display of Average Facility Loading !     *~
            *          !   graph.                                 !     *~
            * 07/02/92 ! Added PIPIN select & pass channel- GETDEM! WPH *~
            * 10/27/92 ! PRR11799 Standardized Report Screen.     ! SID *~
            *          !            Added report ID W/C001,W/C002.!     *~
            *          !          Show the actual % utilization.  !     *~
            *          !          Show the WC Description.        !     *~
            *          !          Show Utilization > 100% as      !     *~
            *          !            different from 100%           !     *~
            *          ! PRR11431 showing OverBooking % of Util.  !     *~
            * 02/28/94 ! Added Work Center Defaults functionality ! MLJ *~
            *          !   via new screen 2 and new file WCDFLTS. !     *~
            * 11/27/95 ! Fixed display of detail on 1st month.    ! JDH *~
            * 09/05/96 ! Millie date conversion                   ! DER *~
            * 09/04/97 ! More Millie Mods.                        ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            actkey$50,                   /*                            */~
            afac$(31)1,                  /* FACS for cap available     */~
            avail%(490),                 /* Cap Available              */~
            avl%(490),                   /*                            */~
            blankdate$8,                 /* blank unfmt date           */~
            blankline$79,                /*                            */~
            ccyymmdd$8,                  /* ccyymmdd                   */~
            columnttl$51,                /* Column titles line         */~
            colhead$78,                  /* Column header for screen   */~
            comp$3,                      /* % Complete through this stp*/~
            company$60,                  /* Company Name               */~
            cursor%(2),                  /* Cursor Location For Edit   */~
            date$8,                      /* Date For Screen Display    */~
            davail$(31)5,                /* Displayed Available        */~
            dd$(31)3,                    /* Displayed Day Of Week Name */~
            delete$79,                   /* Delete Message             */~
            demand$(490)19,              /* Demands                    */~
            dow$(490)3,                  /* Day Of Week Names          */~
            dpct$(31)6,                  /* Displayed Pct Used         */~
            dused$(31)6,                 /* Displayed Used             */~
            editmsg$79,                  /*                            */~
            errormsg$79,                 /* Error Message              */~
            firstplowkey$27,             /*                            */~
            fmdate$10,                   /* Date Range                 */~
            fmwc$4,                      /* Work Center Range          */~
            gdem$19,                     /* From WCCROSS               */~
            gpart$25,                    /* From WCCROSS               */~
            h$(490)1,                    /* Holidays                   */~
            handfactor$8,                /* Move Unit Conversion Factor*/~
            hdate$45,                    /*                            */~
            hdr$60,                      /* Header for ASKUSER         */~
            hiwc$4,                      /* Work Center Range          */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Input Message              */~
            line1$79,                    /* Screen line #1             */~
            line2$79,                    /* Screen line #2             */~
            line2x$(5)79,                /* Screen line #20 - 24       */~
            lastdate$(15)8,              /*                            */~
            lastpm$8,                    /* Last Pm Date               */~
            lfac$(31)1,                  /* Field Attribute Characters */~
            lowc$4,                      /* Work Center Range          */~
            message$79,                  /* Input Message              */~
            msg$(3)80,                   /* 3 lines for ASKUSER        */~
            mm%(490),                    /* Months                     */~
            modate$(12)9,                /* Month Names                */~
            mq$5,                        /* Move/Queue Time In Days    */~
            mqopt$1,                     /* Move/Queue After Step?     */~
            multibom$(100)3,             /*                            */~
            multicode$(100)4,            /*                            */~
            multidemand$(100)19,         /*                            */~
            multidscr$(100)30,           /*                            */~
            multihead$79,                /*                            */~
            multipart$(100)25,           /*                            */~
            multiquant$(100)6,           /*                            */~
            multirte$(100)3,             /*                            */~
            multistep$(100)4,            /*                            */~
            nuw$(7)5,                    /* Norm Units Worked          */~
            nuw%(7),                     /* Norm Units Worked (Numeric)*/~
            note$(31)7,                  /* For Holidays               */~
            overutil$3,                  /* WC Over Util Percentage    */~
            packed$245,                  /* For loading Holiday Schedle*/~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfdescr$(3,2)79,             /* PF Key Descriptions        */~
            pfkeys$(3)32,                /* PF Keys Active             */~
            plowkey$50,                  /* Misc Read Key              */~
            pmfreq$3,                    /* PM Frequency In Days       */~
            prnt_id$16,                  /* Report Program ID          */~
            rpttitle$65,                 /* Report Title               */~
            rucode$4,                    /* Run Activity Code          */~
            rudescr$30,                  /* Run Activity Code Descr    */~
            run$6,                       /* Run Time ( WCunits/Part )  */~
            runh$8,                      /* Run Time ( Hours/Part )    */~
            runp$8,                      /* Run Time ( Parts/Hour )    */~
            seepart$(490)25,             /* Parts Into Work Centers    */~
            seequant%(490),              /* Quants Into Work Centers   */~
            shown$(31)23,                /* For Screen Display         */~
            su$6,                        /* Setup Time ( Units)        */~
            sucode$4,                    /* Setup Activity Code        */~
            sudescr$32,                  /* Setup Activity Code Descr  */~
            suh$8,                       /* Setup Time ( Hours)        */~
            time$8,                      /* System Time                */~
            todate$10,                   /* For Date Ranges            */~
            towc$4,                      /* For WC Ranges              */~
            units$6,                     /* Units In A 24hr Period     */~
            unitsdescr$30,               /* Units In A 24hr Period     */~
            usd%(490),                   /*                            */~
            used%(490),                  /* Capacity Used              */~
            wc$4,                        /* Work Center Code           */~
            wccplowkey$27,               /* Alt Plow On WCCROSS        */~
            wcdescr$32,                  /* Work Center Description    */~
            wcline$(17)64,               /*                            */~
            wcline$132,                  /*                            */~
            wcmsg$79,                    /* No Defaults Message        */~
            wcpart$(15)25,               /*                            */~
            wcpart$25,                   /*                            */~
            wcq$(15)11,                  /*                            */~
            wctag$(15)35,                /*                            */~
            wctag$35,                    /*                            */~
            wcwc$(15)4,                  /*                            */~
            wcwc$4,                      /*                            */~
            workkey$80,                  /*                            */~
            yield$3,                     /* % Yield From This Step     */~
            yr$4,                        /*                            */~
            yy%(490),                    /* Years                      */~
            yymmdd$(490)6                /*                            */

        dim f1%(64)                      /* Record-On-File Flags       */

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
            * #1  ! SYSFILE2 ! System Records File                      *~
            * #11 ! WCMASTR  ! Work Center Master File                  *~
            * #12 ! CALMASTR ! Production Calendar                      *~
            * #13 ! WCDFLTS  ! Work Center Defaults File                *~
            * #14 ! GENCODES ! General Codes Validation File            *~
            * #17 ! RTEMASTR ! Route Master File                        *~
            * #23 ! WCOUT    ! Work Center Usage File                   *~
            * #25 ! JBCROSS2 ! Job/Bom/Rte Xref File                    *~
            * #26 ! PIPCROSS ! Hard Pegging File                        *~
            * #27 ! DEMMASTR ! Demand Master File                       *~
            * #28 ! PIPIN    ! Expected Inventory Additions file        *~
            * #60 ! WORKFILE ! Temp Work File For Detail Use Report     *~
            *************************************************************

            select #1,  "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            select #11, "WCMASTR",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos = 2  , keylen = 5,                       ~
                         alt key 1, keypos = 1, keylen = 6

            select #12, "CALMASTR",                                      ~
                         varc, indexed, recsize = 1962,                  ~
                         keypos = 1, keylen = 2

            select #13, "WCDFLTS",                                       ~
                        varc,     indexed,  recsize =   100,             ~
                        keypos =    1, keylen =   5

            select #14,  "GENCODES",                                     ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #17, "RTEMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 400,                                  ~
                         keypos =   5, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 35

            select #23, "WCOUT",                                         ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 68,                                   ~
                         keypos = 9, keylen = 23,                        ~
                         alt key 1, keypos = 1, keylen = 27

            select #25,  "JBCROSS2",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 94,                                   ~
                         keypos = 29, keylen = 19,                       ~
                         alt key   1, keypos =  1, keylen = 47,          ~
                             key   2, keypos = 48, keylen = 47

            select #26,  "PIPCROSS",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos = 1, keylen =  71,                       ~
                         alt key  1, keypos =  20, keylen =  52,         ~
                             key  2, keypos =  39, keylen =  33

            select #27,  "DEMMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 123,                                  ~
                         keypos = 2, keylen =  27,                       ~
                         alt key  1, keypos =  10, keylen =  19,         ~
                             key  2, keypos =   1, keylen =  28

            select #28, "PIPIN",                                         ~
                        varc,     indexed,  recsize =    60,             ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48

            select #60, "TEMP60",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 66,                                   ~
                         keypos = 1, keylen = 66


           call "SHOSTAT" ("Preparing For Work Center Review Functions")

           call "OPENCHCK" (#1, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#11, 0%, 0%, 100%, " ")
           call "OPENCHCK" (#12, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#13, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#14, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#17, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#23, 0%, 0%, 300%, " ")
           call "OPENCHCK" (#25, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#26, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#27, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#28, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$, nix%, ccyymmdd$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            call "COMPNAME" (12%, company$, ret%)
            time$ = " "  :  call "TIME" (time$)

            editmsg$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press RETURN."

           call "DATE" addr ("HD", hdate$)

            modate$(01) = "January  "
            modate$(02) = "February "
            modate$(03) = "March    "
            modate$(04) = "April    "
            modate$(05) = "May      "
            modate$(06) = "June     "
            modate$(07) = "July     "
            modate$(08) = "August   "
            modate$(09) = "September"
            modate$(10) = "October  "
            modate$(11) = "November "
            modate$(12) = "December "

        REM Get The Production Caledar
                gosub loadcal

        REM Load Holiday Schedule...
            h$() = all("0")
            call "REDALT0" (#1, "HOLIDAY SCHEDULE", 0%, f1%(1))
                if f1%(1) = 0 then L09400
            get #1, using L09350, packed$
L09350:     FMT XX(20), CH(245)
            hexunpack packed$ to str(h$())

            hdr$ = "*** CALENDAR ERROR ***"
            today% = 0%
L09400:     call "PIPINDEX" (#1, " ", today%, u3%)
            if u3% <> 1 then L09490
L09420:       ask% = 2%
              msg$(1) = "Can't Find Planning Calendar In SYSFILE2"
              msg$(2) ="Please Correct the Problem and Rerun This Program"
              msg$(3) = "Press RETURN to EXIT"
              call "ASKUSER" (ask%, hdr$, msg$(1), msg$(2), msg$(3))
              if ask% <> 0% then L09420
              goto exit_program
L09490:     if u3% = 0 then L09580
L09500:       ask% = 2%
              msg$(1) = "Todays Date Is Outside Planning Calendar"
              msg$(2) ="Please Correct the Problem and Rerun This Program"
              msg$(3) = "Press RETURN to EXIT"
              call "ASKUSER" (ask%, hdr$, msg$(1), msg$(2), msg$(3))
              if ask% <> 0% then L09500
              goto exit_program

L09580:     pfdescr$(1,1) = "(1)Start Over        (12)Review Multiple W/C~
        ~                   (15)Print Screen"
            pfdescr$(1,2) = "                                            ~
        ~                   (16)EXIT PROGRAM"

            pfdescr$(2,1) = "(1)Start Over                               ~
        ~                   (15)Print Screen"
            pfdescr$(2,2) = "(2)W/C Utilization                          ~
        ~                   (16)RETURN   "

            pfdescr$(3,1) = "(1)Start Over           (4)Previous Page    ~
        ~                   (15)Print Screen"
            pfdescr$(3,2) = "                                            ~
        ~                   (16)RETURN   "

            pfkeys$(1) = hex(00010c0d0f10)
            pfkeys$(2) = hex(000102ff0d0f10)
            pfkeys$(3) = hex(0001040d0f10)

            str(columnttl$, 1) = "Beginning Code"
            str(columnttl$,27) = "Ending Code"


            str(line2$,62) = " WCDSPLY: " & str(cms2v$,1,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Input Work Center Code for Review                         *~
            *************************************************************

        inputmode
            init(" ") errormsg$, delete$, wc$, wcdescr$, lastpm$, nuw$(),~
                      pmfreq$, units$, unitsdescr$, fmdate$, fmwc$,      ~
                      hiwc$, lowc$, todate$, towc$, mq$, mqopt$, suh$,   ~
                      su$, runh$, run$, runp$, yield$, sucode$, sudescr$,~
                      rucode$, rudescr$, comp$, handfactor$

            call "ALLFREE"

            page% = 0%  /* For reports */

            mat avail% = zer
            mat used% = zer
            mat nuw% = zer

            message$ = "Leave Blank & Press (RETURN) To Search For " &   ~
                       "Existing Work Center Code."
L10160:     gosub'101(1%)
                if keyhit%  =  1% then gosub startover
                if keyhit%  = 12% then sel_many
                if keyhit%  = 16% then exit_program
                if keyhit% <>  0% then L10160
            gosub'151
                if errormsg$ <> " " then L10160

        REM *************************************************************~
            *        D I S P L A Y   S I N G L E   W / C                *~
            *-----------------------------------------------------------*~
            * Display Detail of SINGLE Work Center                      *~
            *************************************************************

        display_mode
            message$ = " "
L10320:     gosub'111(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then see_capacities
                  if keyhit%  =  5% then see_defaults
                  if keyhit%  = 16% then inputmode
                  goto L10320

        see_defaults
L10400:     gosub'112
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then display_mode
                  if keyhit%  = 16% then inputmode
                  goto L10400

        REM *************************************************************~
            *             I N P U T   R A N G E                         *~
            *-----------------------------------------------------------*~
            * Input Mode For Range Selection                            *~
            *************************************************************
        sel_many
            init(" ") errormsg$, fmdate$, fmwc$, hiwc$, lowc$,           ~
                      todate$, towc$

            for fieldnr% = 1% to  2%
L11100:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L11220
L11120:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L11200
L11150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L11120
                         if fieldnr% = 1% then L11100
                         goto L11150
L11200:               if keyhit% = 16% and fieldnr% = 1% then inputmode
                      if keyhit% <> 0% then L11120
L11220:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L11120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles EDIT MODE for range selection screen.             *~
            *************************************************************~

        edit_many
            inpmessage$ = editmsg$
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                gosub call_screen
                if keyhit%  =  1% then gosub startover
                if keyhit%  <> 8% then L11410
                     gosub display_loading
                     goto edit_many
L11410:         if keyhit%  <> 9% then L11440
                     gosub display_usage_details
                     goto edit_many
L11440:         if keyhit%  <> 24% then L11470
                     gosub print_loading
                     goto edit_many
L11470:         if keyhit%  <> 25% then L11500
                     gosub print_usage_details
                     goto edit_many
L11500:         if keyhit%  = 16% then inputmode
                if keyhit% <>  0% then edit_many
L11520:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  2% then edit_many
            if fieldnr% = lastfieldnr% then edit_many
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled% =  0% then edit_many
L11570:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                gosub call_screen
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then L11570
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " " then L11570
                lastfieldnr% = fieldnr%
            goto L11520

        REM *************************************************************~
            * D I S P L A Y   U T I L I Z A T I O N   G R A P H         *~
            *************************************************************

        display_loading
            init (hex(00)) plow$
L12060:     str(plow$,,4) = lowc$
            line% = 1%
            call "READ104" (#11, plow$, f1%(11))
L12090:     goto L12130

        readloop2
            call "READNEXT" (#11, f1%(11))
L12130:         if f1%(11) <> 1% then L12502
            get #11, using L12150, str(wcline$,,5%), avl%(), usd%()
L12150:         FMT XX(1), CH(4), XX(54), 490*BI(2), 490*BI(2)
            if str(wcline$,,4%) > hiwc$ then L12502
                str(wcline$(line%),,5%) = str(wcline$,,5%)

                init(" ") str(wcline$(line%),5%)
                totavl%, totusd% = 0%
                for jj = fromsub% to tosub%
                    totavl% = totavl% + avl%(jj)
                    totusd% = totusd% + usd%(jj)
                next jj
                if totavl% = 0% then L12320
                abcde% = round(((totusd% * 50) / totavl%), 0)
                if abcde% < 50% then L12280
                 totusd = totusd% : totavl = totavl%
                 overutil = round(((totusd / totavl) * 100),0)
                 convert overutil to overutil$, pic(###)
                 abcde% = 50%
                 if overutil <= 100 then L12280
                 str(wcline$(line%),abcde%+7%,) = "... " & overutil$ & "%"
L12280:         if abcde% <= 0% then L12320
                init("*") str(wcline$(line%),7,abcde%)

L12320:     str(wcline$(line%),6,1) = "!"
            line% = line% + 1%
            if line% <= 15% then readloop2
            wcline$(16%) = "     +----1----2----3----4----5----6----7" & ~
                           "----8----9----+ Percent"
            wcline$(17%) = "          0    0    0    0    0    0    0" & ~
                           "    0    0       Usage "
            line2x$(3) = "                                          "  & ~
                         "                    (13)Instructions"
            line2x$(4) = "(3)First Screen                           "  & ~
                         "                    (15)Print Screen"
            line2x$(5) = "(5)Next Screen                            "  & ~
                         "                    (16)RETURN"
L12440:     gosub display_load
            init (" ") wcline$()
            if keyhit% = 16% then return
            if keyhit% = 3% then L12060
            if keyhit% = 15% then L12440
                line% = 1%
                go to L12090

L12502:     line2x$(3) = "                                          "  & ~
                         "                    (13)Instructions"
            line2x$(4) = "(3)First Screen                           "  & ~
                         "                    (15)Print Screen"
            line2x$(5) = "                                          "  & ~
                         "                    (16)RETURN"
            wcline$(line%) =   "     +----1----2----3----4----5----6" &  ~
                               "----7----8----9----+ Percent"
            wcline$(line%+1%)= "          0    0    0    0    0    0" &  ~
                               "    0    0    0       Usage "
L12570:
            gosub display_load
            init (" ") wcline$()
            if keyhit% = 16% then return
            if keyhit% = 3% then L12060
            go to L12570

        REM *************************************************************~
            * D I S P L A Y   U S A G E   D E T A I L                   *~
            *************************************************************

        display_usage_details
            gosub create_usage_workfile

        display_loop
            colhead$ = " Date    Wkctr  Units Used   Part/Desc       " & ~
                       "Job Or Advice/BOM RTE ACT"
            line2x$(3) =  "Position Cursor & Press RETURN to See Top " & ~
                          " Level Demand"
            line2x$(4) = "(3)First Screen                           "  & ~
                         "                    (15)Print Screen"
            line2x$(5) = "(5)Next Screen                            "  & ~
                         "                    (16)RETURN"
L13160:     init(" ") lastdate$(), wcwc$(), wcq$(), wcpart$(), wctag$(), ~
                      multibom$ (), multirte$ (), multistep$(),          ~
                      multicode$(), multidscr$(), lastdate$
            line% = 1%
            if keyhit% = 5% then readloop3  /* Load another screenful */
            init(hex(00)) workkey$
            call "READ104" (#60, workkey$, f1%(60))
            goto L13260

        readloop3
            call "READNEXT" (#60, f1%(60))
L13260:         if f1%(60) = 0% then L13560
            get #60, using L13280, workkey$
L13280:         FMT CH(66)
            if lastdate$ =  str(workkey$,,6) then L13340
                lastdate$ = str(workkey$,,6)
                lastdate$(line%) = lastdate$
                call "DATEFMT" (lastdate$(line%))

L13340:     get workkey$, using L13360, wcwc$(line%), wctag$(line%),      ~
                                       wcpart$(line%), wcq
L13360:         FMT XX(6), CH(4), CH(23), CH(25), PD(14,4)
            convert wcq to wcq$(line%), pic (########.##)

            call "WCACTSUB" (wctag$(line%),      /* WCOUT TAG          */~
                      multibom$ (line%),         /* BOM FROM JBCROSS2  */~
                      multirte$ (line%),         /* RTE FROM JBCROSS2  */~
                      multistep$(line%),         /* STEP FROM RTEMASTR */~
                      multicode$(line%),         /* ACTIVITY CODE      */~
                      multidscr$(line%),         /* ACTIVITY DESCR     */~
                      #23,                       /* WCOUT              */~
                      #25,                       /* JBCROSS2           */~
                      #17)                       /* RTEMASTR           */

            line% = line% + 1%
            if line% < 9% then readloop3
            gosub display_use
            if keyhit% = 16% then L13630
            if keyhit% =  3% then display_loop
            goto L13160

L13560:     line2x$(5) = "                                          "  & ~
                         "                    (16)RETURN"
L13580:     gosub display_use
            if keyhit% = 16% then L13630
            if keyhit% <> 3% then L13580
            goto display_loop

L13630:     call "FILEBGON" (#60)
            return

        REM *************************************************************~
            *     P R I N T   U T I L I Z A T I O N   G R A P H         *~
            *************************************************************

        print_heading
            rpttitle$  = " Average Facility Loading For the Period " &   ~
                         fmdate$ & " To " & todate$
            print page
            page% = page% + 1%
            time$ = " "  :  call "TIME" (time$)
            print using L60070, date$, time$, company$, prnt_id$
            print using L60110, rpttitle$, page%
            print using L60200, str(hdate$,1%,31%)
            print skip (1%)
            print using L60132 /* Print Column Headings    */
            print using L60133 /* Under Line */
            lcntr% = 6%
        return

        print_loading
            call "SHOSTAT" ("Printing Shop Utilization Graph")
            select printer(134)
            call "SETPRNT" ("W/C001", " ", 0%, 0%)
            prnt_id$ = " WCDSPLY: W/C001"
            gosub print_heading
            init(" ") wcline$
            call "READ104" (#11, lowc$, f1%(11))
            goto L14170

        readloop
            call "READNEXT" (#11, f1%(11))
L14170:         if f1%(11) <> 1% then L14470
            get #11, using L14190, str(wcline$,,5), avl%(), usd%()
L14190:         FMT XX(1), CH(4), XX(54), 490*BI(2), 490*BI(2)
            if str(wcline$,,4) > hiwc$ then L14470
            call "DESCRIBE" (#11, str(wcline$,,4%), str(wcline$,7%,30%), ~
                             0%, f1%(11%))
            totavl%, totusd% = 0%
            for jj = fromsub% to tosub%
                     totavl% = totavl% + avl%(jj)
                     totusd% = totusd% + usd%(jj)
            next jj
            init(" ") str(wcline$,37%)
            if totavl% = 0% then L14330
                abcde% = round(((totusd% * 50) / totavl%), 0)
                 totusd = totusd% : totavl = totavl%
                 overutil = round(((totusd / totavl) * 100),0)
                 convert overutil to overutil$, pic(###)
                 if abcde% < 50% then L14296
                    abcde% = 50% : str(wcline$,99%,2%) = "@@"
L14296:          str(wcline$,42%,6%) = overutil$
                if abcde% <= 0% then  L14330
                    init("*") str(wcline$,48%,abcde%)

L14330:     str(wcline$,47%,1%) = "!"
            print using L60220, wcline$
            lcntr% = lcntr% + 1%
            if lcntr% < 56% then readloop

            print using L60240
            print using L60260
            print using L60274
            gosub print_heading
            go to readloop

L14470:     print using L60240
            print using L60260
            print using L60274
            page% = 0%
            time$ = " "  :  call "TIME" (time$)
            print
            print using L60450, time$
            call "SETPRNT" (str(prnt_id$,11%,6%), " ", 0%, 1%)
            call "FILEBGON" (#60)
            return

        REM *************************************************************~
            *     P R I N T   U S A G E   D E T A I L                   *~
            *************************************************************

        print_usage_details
            gosub create_usage_workfile
            select printer(134)
            call "SETPRNT" ("W/C002", " ", 0%, 0%)
            prnt_id$ = " WCDSPLY: W/C002"
            call "SHOSTAT" ("Printing Daily Detail Report")
            init(hex(00)) lastdate$, workkey$
            page% = 0%
            gosub page_control
            call "READ104" (#60, workkey$, f1%(60))
            goto L15150

        readloop4
            call "READNEXT" (#60, f1%(60))
L15150:         if f1%(60) = 0 then L15470
            get #60, using L15170, workkey$
L15170:         FMT CH(66)
            if lastdate$ = str(workkey$,,6) then L15250
                if lcntr% > 50 then gosub page_control
                lastdate$ = str(workkey$,,6)
                call "DATEFMT" (lastdate$)
                print using L60400, lastdate$
                call "DATUNFMT" (lastdate$)
                lcntr% = lcntr% + 1
L15250:     get workkey$, using L15260, wcwc$, wctag$, wcpart$, wcq
L15260:         FMT XX(6), CH(4), CH(23), CH(25), PD(14,4)

            call "WCACTSUB" (wctag$,             /* WCOUT TAG          */~
                     multibom$ (1%),             /* BOM FROM JBCROSS2  */~
                     multirte$ (1%),             /* RTE FROM JBCROSS2  */~
                     multistep$(1%),             /* STEP FROM RTEMASTR */~
                     multicode$(1%),             /* ACTIVITY CODE      */~
                     multidscr$(1%),             /* ACTIVITY DESCR     */~
                        #23,                     /* WCOUT              */~
                        #25,                     /* JBCROSS2           */~
                        #17)                     /* RTEMASTR           */~

           print using L60420, wcwc$, wcq, str(wctag$,,19), wcpart$,      ~
                              multibom$ (1%), multirte$ (1%),            ~
                              multistep$(1%), multicode$(1%),            ~
                              multidscr$(1%)
            lcntr% = lcntr% + 1

            if lcntr% > 55 then gosub page_control
            goto readloop4

L15470:     time$ = " "  :  call "TIME" (time$)
            print
            print using L60450, time$
            call "SETPRNT" (str(prnt_id$,11%,6%), " ", 0%, 1%)
            call "FILEBGON" (#60)
            return

        REM *************************************************************~
           *    S E E   C A P A C I T I E S   B Y   M O N T H           *~
           **************************************************************
        see_capacities
            convert str(ccyymmdd$, 1%, 4%) to yr%
            todayyy% = yr%
            convert str(ccyymmdd$, 5%, 2%) to mo%
            todaymm% = mo%
            convert str(ccyymmdd$, 7%, 2%) to dd%

        see_capacities_again
            avail%, f% = 0%
            for i = 1 to 490
                if yy%(i) < yr% then L16160
                if yy%(i) > yr% then L16170
                if mm%(i) < mo% then L16160
                if mm%(i) > mo% then L16170
                if f% =  0 then f% = i
L16160:     next i
L16170:     l% = i - 1
            counter, counter1, j = 0
            convert yr% to yr$, pic(####)
            colhead$ = "   Day  Avail   Used  Usage Note      !  "  &    ~
                       "   Day  Avail   Used  Usage Note"
            line1$ = "Units Available and Used for W/C" &                ~
                      hex(84) & wc$ & hex(8c) & "for the Month of" &     ~
                      hex(84) & modate$(mo%) & " " & yr$
            init(" ")  dd$(), davail$(), dused$(), dpct$(), note$()
            init(hex(8c)) afac$()
            for i = f% to l%
                j = j + 1
                if avail%(i) = 0% then L16350
                if yy%(i) = todayyy% and mm%(i) = todaymm%               ~
                                     and j < dd% then L16340
                counter1 = counter1 + avail%(i)
                afac$(j) = hex(84)
L16340:         convert avail%(i) to davail$(j), pic(#####)
L16350:         if used%(i) = 0% then L16400
                if yy%(i) = todayyy% and mm%(i) = todaymm%               ~
                                         and j < dd% then L16390
                   counter = counter + used%(i)
L16390:            convert used%(i) to dused$(j), pic(######)
L16400:       if avail%(i) = 0 then L16430
                put dpct$(j), using L16420, round(100*used%(i)/avail%(i),0)
L16420:                                    %#####%
L16430:         dd$(j) = dow$(i)
                if h$(i) <> "0" then note$(j) = "Holiday"
            next i
L16460:     effic = 0
            if counter1 = 0 then L16530
                effic = 100 * counter / counter1
            if (yy%(i) < todayyy%)  or                                   ~
               (yy%(i) = todayyy% and mm%(i) < todaymm%)  then           ~
               init(hex(8c)) afac$()

            effic = round(effic,2)
L16530:     convert effic to eff$, pic(###.##)
            convert counter to ctr1$, pic(######)
            convert counter1 to ctr2$, pic(######)
            line2x$(1) = "Usage is" & hex(84) & eff$ & "%" & hex(8c) &   ~
                         ", Used" & hex(84) & ctr1$ & hex(8c) &          ~
                         "Units of" & hex(84) & ctr2$ & hex(8c) &        ~
                         "Units Still Available This Month"
            if (yy%(i) < todayyy%)  or                                   ~
               (yy%(i) = todayyy% and mm%(i) < todaymm%)  then           ~
                        line2x$(1) = "Work Center has" & hex(84) &       ~
                        ctr1$ & hex(8c) & "Scheduled Units that are " &  ~
                        "Unaccounted For"
            line2x$(2) = "Cursor to Day for PF12 or PF14"
L16660:     line2x$(3) = "(1)Start Over     (5)Next Month        (9)"  & ~
                         "Header              (13)Instructions"
            line2x$(4) = "(3)Current Month  (7)See Jobs in WC   (12)"  & ~
                         "See Daily Schedule  (15)Print Screen"
            line2x$(5) = "(4)Prior Month    (8)See Parts in WC  (14)"  & ~
                         "See Shop Loading    (16)RETURN"

L16730:     gosub display_wc_util

            if keyhit% =  1% then gosub startover
            if avail%  > 0% then test_edit
            if keyhit% = 16% then display_mode
            if keyhit% =  3% then see_capacities
            if keyhit% =  7% then demands
            if keyhit% =  8% then parts
            if keyhit% =  9% then display_mode

            if keyhit% = 12% or keyhit% = 14% then gosub get_cursor_pos
            if keyhit% <> 4% then L16890
                if f% = 1 then L16730
                mo% = mm%(f%-1)
                yr% = yy%(f%-1)
                goto see_capacities_again
L16890:     if keyhit% <> 5 then L16660
                if l% = 490 then L16730
                mo% = mm%(l%+1)
                yr% = yy%(l%+1)
                goto see_capacities_again

        REM *************************************************************~
           *    S E E   D E M A N D S   B Y   M O N T H                 *~
           **************************************************************
        seedemands
            convert str(ccyymmdd$, 1%, 4%) to yr%
            todayyy% = yr%
            convert str(ccyymmdd$, 5%, 2%) to mo%
            todaymm% = mo%
            convert str(ccyymmdd$, 7%, 2%) to dd%

        demands
            f% = 0%
            for i = 1 to 490
                if yy%(i) < yr% then L17140
                if yy%(i) > yr% then L17150
                if mm%(i) < mo% then L17140
                if mm%(i) > mo% then L17150
                if f% =  0 then f% = i
L17140:     next i
L17150:     j = 0  :  l% = i - 1
            init(" ")  dd$(), davail$(), shown$()
            convert yr% to yr$, pic(####)
            line1$ = "Jobs & Demands Currently Scheduled in W/C" &       ~
                      hex(84) & wc$ & hex(8c) & "for the Month of" &     ~
                      hex(84) & modate$(mo%) & " " & yr$
            colhead$ = "   Day Avail  Job/Demand Scheduled    !  "  &    ~
                       "   Day Avail  Job/Demand Scheduled"
            for i = f% to l%
                j = j + 1
                if avail%(i) = 0 then L17280
                    convert avail%(i) to davail$(j), pic(#####)
L17280:         shown$(j) = demand$(i)
                dd$(j) = dow$(i)
                if h$(i) <> "0" then str(shown$(j),17,7) = "Holiday"
            next i

L17390:     line2x$(1) = " "
            line2x$(2) = "Cursor to Day and Press RETURN to see Top "  & ~
                         "Level Demand or PF12 or PF14"
            line2x$(3) = "(3)Current Month                          "  & ~
                         "                    (13)Instructions"
            line2x$(4) = "(4)Prior Month     (8)Parts in W/C    (12)"  & ~
                         "Daily Schedule      (15)Print Screen"
            line2x$(5) = "(5)Next Month                         (14)"  & ~
                         "Facility Loading    (16)RETURN"

L17510:     gosub display_both

            if keyhit% = 16 then see_capacities_again
            if keyhit% = 3 then seedemands
            if keyhit% = 8 then parts
            if keyhit% <> 14% and keyhit% <> 12% then L17590
                gosub get_cursor_pos
                goto L17390
L17590:     if keyhit% <> 4 then L17640
                if f% = 1 then L17510
                    mo% = mm%(f%-1)
                    yr% = yy%(f%-1)
                    goto demands
L17640:     if keyhit% <> 5 then L17690
                if l% = 490 then L17510
                mo% = mm%(l%+1)
                yr% = yy%(l%+1)
                goto demands
L17690:     if keyhit% <> 15 then L17720
                call "PRNTSCRN"
                goto L17510
L17720:     if keyhit% <> 0 then L17510
                mat cursor% = zer : dem$, type$ = " " : r% = 0%
                gosub get_cursor_pos
                if edit% = 0 then L17510
            if str(shown$(edit%),,19) = "MULTIPLE ACTIVITIES" then L17510
            if str(shown$(edit%),,19) = " " then L17510
            call "GETDEM" (1%, str(shown$(edit%),,19), #26, #27,#28,dem$,~
                           type$, r%)
            goto L17510

        REM *************************************************************~
           *    S E E   P A R T S   &   Q T Y    B Y   M O N T H        *~
           **************************************************************

        seeparts
            convert str(ccyymmdd$, 1%, 4%) to yr%
            todayyy% = yr%
            convert str(ccyymmdd$, 5%, 2%) to mo%
            todaymm% = mo%
            convert str(ccyymmdd$, 7%, 2%) to dd%

        parts
L18080:     f% = 0%
            for i = 1 to 490
                if yy%(i) < yr% then L18150
                if yy%(i) > yr% then L18170
                if mm%(i) < mo% then L18150
                if mm%(i) > mo% then L18170
                if f% =  0 then f% = i
L18150:     next i

L18170:     l% = i - 1  :  j = 0
            init(" ")  dd$(), davail$(), shown$()
            convert yr% to yr$, pic(####)
            line1$ = "Parts Currently Scheduled in W/C" &                ~
                      hex(84) & wc$ & hex(8c) & "for the Month of" &     ~
                      hex(84) & modate$(mo%) & " " & yr$
            colhead$ = "   Day  Used  Part To Be Produced     !  "  &    ~
                       "   Day  Used  Part To Be Produced"

            for i = f% to l%
                j = j + 1
                if seequant%(i) = 0% then L18310
                convert seequant%(i) to davail$(j), pic(#####)
                shown$(j) = str(seepart$(i),,23)
L18310:         dd$(j) = dow$(i)
                if h$(i) <> "0" then str(shown$(j),17,7) = "Holiday"
            next i

L18350:     line2x$(1) = " "
            line2x$(2) = "Cursor to Day for PF12 or PF14"
            line2x$(3) = "(3)Current Month                          "  & ~
                         "                    (13)Instructions"
            line2x$(4) = "(4)Prior Month     (7)Jobs in W/C     (12)"  & ~
                         "Daily Schedule      (15)Print Screen"
            line2x$(5) = "(5)Next Month                         (14)"  & ~
                         "Facility Loading    (16)RETURN"

L18440:     gosub display_both

            if keyhit% = 16 then see_capacities_again
            if keyhit% = 3 then seeparts
            if keyhit% = 7 then demands
            if keyhit% <> 14% and keyhit% <> 12% then L18520
               gosub get_cursor_pos
               goto L18350
L18520:     if keyhit% <> 4 then L18570
                if f% = 1 then L18440
                mo% = mm%(f%-1)
                yr% = yy%(f%-1)
                goto L18080
L18570:     if keyhit% <> 5 then L18620
                if l% = 490 then L18440
                mo% = mm%(l%+1)
                yr% = yy%(l%+1)
                goto L18080
L18620:     if keyhit% <> 15 then L18440
                call "PRNTSCRN"
                goto L18440

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************~

        deffn'052(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20120,         /* Work Center Range      */~
                              L20180          /* Date Range             */
            return

L20120: REM Def/Enable Work Center Range           FMWC$
            if fmwc$               = " " then                            ~
               fmwc$               = "ALL"
            inpmessage$ = "Enter Work Center Range.  Use '?' to view "  &~
                          "list."
            return

L20180: REM Def/Enable Date Range                  FMDATE$
            if fmdate$             = " " then                            ~
               fmdate$             = date$
               call "DATEOKC" ( fmdate$, 0%, " " )
            inpmessage$ = "Enter Date Range"
            return
                                                                         ~
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
            goto inputmode

        REM *************************************************************~
           *         L O A D   W C   I N F O R M A T I O N              *~
           *                                                            *~
           * LOADS DATA OFF DISK                                        *~
           **************************************************************

        load_wc_info
           call "SHOSTAT" ("Gathering Work Center Data")
           call "REDALT0" (#11, wc$, 0%, f1%(11))
           get #11, using L30120, wc$, wcdescr$, nuw%(), pmfreq$,         ~
                                 lastpm$, avail%(), used%(), units

L30120:    FMT  XX(01),                  /* Unused                     */~
                CH(04),                  /* Work Center                */~
                XX(01),                  /* Filler = ' '               */~
                CH(30),                  /* WC Descr                   */~
                7*BI(02),                /* Weekly Units Avail, Mon 1st*/~
                CH(03),                  /* Pm Frequency In Days       */~
                CH(06),                  /* Last Pm Date               */~
                490*BI(02),              /* Capacity Available         */~
                490*BI(02),              /* Capacity Used              */~
                BI(2)                    /* Units In 24 Hours          */

           for i%=1% to 7%
               convert nuw%(i%) to nuw$(i%), pic(#####)
           next i%
           call "CONVERT" (units, 0.0, units$)
           call "WCUN2HRS" (#11, hex(00000000), units, 0, unitsdescr$)
           call "DATEFMT" (lastpm$)

           init(hex(00)) wccplowkey$
           init(" ") demand$(), seepart$()
           mat seequant% = zer

           str(wccplowkey$,,4) =  str(wc$)

L30360:    call "PLOWALTS" (#23, wccplowkey$, 1%, 4%, f1%(23))
                     if f1%(23) <> 1 then load_wc_defaults
           get #23, using L30390, gdem$, subscr%, su%, run%
L30390:    FMT XX(8), CH(19), BI(2), XX(2), 2*BI(4)
           gquant% = su% + run%
           if demand$(subscr%) <> " " then L30490
                call "READ100" (#25, gdem$, f1%(25))
                if f1%(25) <> 0 then get #25, gpart$ else gpart$ = " "
                demand$(subscr%) = gdem$
                seepart$(subscr%) = gpart$
                seequant%(subscr%) = gquant%
                   goto L30360

L30490:    demand$(subscr%) = "MULTIPLE ACTIVITIES"
           seepart$(subscr%) = "MULTIPLE PARTS"
           seequant%(subscr%) = 0
           init (hex(ff)) str(wccplowkey$,7)
                   goto L30360

        load_wc_defaults
            wcmsg$ = " "
            if wc$ = "VEND" then display_mode
                plowkey$ = str(wc$) & " "
            call "READ100" (#13, plowkey$, f1%(13%))
                if f1%(13%) <> 0% then L30630
                    wcmsg$ = "No Defaults Currently Exist For This Work"&~
                                " Center"
                    goto display_mode
L30630:     get #13 using L30650, mq, mqopt$, handfactor, su, run, yield, ~
                          sucode$, rucode$, rudescr$, comp
L30650:     FMT POS(6), BI(4), CH(1), PD(14,4), BI(4), PD(14,4), BI(4),  ~
                2*CH(4), CH(30), BI(4)
            if mq < 0 then L30680 else call "CONVERT" (mq, -0.0, mq$)
L30680:     if su < 0 then L30690 else call "CONVERT" (su, -0.0, su$)
                call "WCUN2HRS" (#11, wc$, 0, su, " ")
                call "CONVERT" (su, -2.4, suh$)
L30690:     if run < 0 then L30780 else call "CONVERT" (run, -0.0, run$)
                call "WCUN2HRS" (#11, wc$, 0, run, " ")
                call "CONVERT" (run, -4.6, runh$)
                    temp = 0
                    convert runh$ to temp, data goto L30770
                    if temp <> 0 then temp = 1/temp
L30770:             call "CONVERT" (temp, 0.6, runp$)
L30780:     call "CONVERT" (handfactor, -0.0, handfactor$)
            call "CONVERT" (yield, -0.0, yield$)
            if comp < 0 then L30810 else call "CONVERT" (comp, -0.0, comp$)
L30810:     if mqopt$ = " " then mqopt$ = "N"
            plowkey$ = "WC ACTVTY" & sucode$
            call "GETCODE" (#14, plowkey$, sudescr$, 0%, 99, f1%(14%))
            goto display_mode

        REM *************************************************************~
           *    L O A D   C A L E N D A R                               *~
           **************************************************************
        loadcal
            call "REDALT0" (#12, "30", 0%, f1%(12))
            if f1%(12) = 1 then L31130
L31060:       ask% = 2%
              msg$(1) = "Planning Calendar Has Not Been Defined"
              msg$(2) ="Please Correct the Problem and Rerun This Program"
              msg$(3) = "Press RETURN to EXIT"
              call "ASKUSER" (ask%, hdr$, msg$(1), msg$(2), msg$(3))
              if ask% <> 0% then L31060
                goto exit_program
L31130:     get #12, using L31140, mm%()
L31140:     FMT XX(2), 490*BI(4)

            call "REDALT0" (#12, "50", 0%, f1%(12))
                if f1%(12) = 0 then L31370
                get #12, using L31190, dow$()
L31190:         FMT XX(2), 490*CH(3)

            call "REDALT0" (#12, "20", 0%, f1%(12))
                if f1%(12) = 0 then L31370
                get #12, using L31240, yy%()
L31240:         FMT XX(2), 490*BI(4)

            call "REDALT0" (#12, "10", 0%, f1%(12))
                if f1%(12) = 0 then L31370
            get #12, using L31290, str(yymmdd$(),,1470)
L31290:         FMT XX(2), CH(1470)

            call "REDALT0" (#12, "11", 0%, f1%(12))
                if f1%(12) = 1 then L31400
L31330:       ask% = 2%
              msg$(1) = "PLANNING CALENDAR IS DAMAGED"
              msg$(2) ="Please Correct the Problem and Rerun This Program"
              msg$(3) = "Press RETURN to EXIT"
L31370:       call "ASKUSER" (ask%, hdr$, msg$(1), msg$(2), msg$(3))
              if ask% <> 0% then L31330
                goto exit_program
L31400:     get #12, using L31410, str(yymmdd$(),1471,1470)
L31410:         FMT XX(2), CH(1470)
           return

        get_cursor_pos
            close ws
            edit% = 0%
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                if cursor%(1%) < 4% or cursor%(1%) > 19% then return
                if cursor%(1%) > 18% and cursor%(2%) < 40% then return
            temp% = cursor%(1%) - 3%
            if cursor%(2%) > 40% then temp% = temp% + 15%
            edit% = temp%
            if edit% + f% - 1% > 490% or edit% + f% - 1% < 1% then return
            if keyhit% = 12% then seemultiple
            if keyhit% <> 14% then return

            fmdate$ = yymmdd$(edit% - 1% + f%)
            todate$, towc$ = " "
            fmwc$ = "ALL"
            call "TESTRNGE" (fmwc$, towc$, lowc$, hiwc$, errormsg$)
            gosub L51160
            gosub display_loading
            return

        test_edit
            for i% = 1 to 31
                avail% = f% + i% - 1
                if avail% > 490 then L32690
                if dd$(i%) = " " then L32690
                if davail$(i%) = " " then davail$(i%) = "0"
                convert davail$(i%) to temp%, data goto L16730
                if temp% < 0% then L16730
                if temp% > 65000% then L16730
                counter1 = counter1 - avail%(avail%)
                avail%(avail%) = temp%
                convert avail%(avail%) to davail$(i%), pic(#####)
                if davail$(i%) = "    0" then davail$(i%) = " "
                counter1 = counter1 + avail%(avail%)
                if avail%(avail%) = 0% then L32680
                put dpct$(i%), using L32670,                              ~
                               100*used%(avail%)/avail%(avail%)
L32670:             %#####%
L32680:         lfac$(i%) = hex(84)
L32690:     next i%
            avail% = 0
            goto L16460


        create_usage_workfile
           call "WORKOPEN" (#60, "IO   ", 500%, 1%)

           str(firstplowkey$,,4) = lowc$
L33040:    call "PLOWALTS"  (#23, firstplowkey$, 1%, 0%, f1%(23))
               if f1%(23) = 0% then return

           if str(firstplowkey$,,4) > hiwc$ then return

           init (hex(00)) str(firstplowkey$,5)
           str(firstplowkey$,5,2) = bin(fromsub%,2)

L33120:    call "PLOWALTS" (#23, firstplowkey$, 1%, 4%, f1%(23))
               if f1%(23) = 0% then L33210
           get #23, using L33150, wcwc$, wcdate%, wccplowkey$, su%, run%
L33150:        FMT CH(4), BI(2), XX(2), CH(19), XX(4), 2*BI(4)
            wcq = su% + run%
            call "READ100" (#25, wccplowkey$, f1%(25))
                if f1%(25) <> 0 then get #25, wcpart$ else wcpart$ = " "

           if wcdate% <= tosub% then L33240
L33210:       init (hex(ff)) str(firstplowkey$,5)
              goto L33040

L33240:    write #60, using L33260, yymmdd$(wcdate%), wcwc$, key(#23,0),  ~
                                   wcpart$, wcq
L33260:    FMT CH(6), CH(4), CH(23), CH(25), PD(14,4)
           goto L33120

        call_screen
            call "GETSCRN" ("C", " ", cursor%(), 0%)
            return

        pf1315
            if keyhit% <> 13% then L33590
                call "MANUAL" ("WCDSPLY")
                keyhit% = 15%
                return
L33590:     if keyhit% <> 15% then return
                call "PRNTSCRN"
                return

        pf1315_1
            if keyhit1% <> 13% then L33680
                call "MANUAL" ("WCDSPLY")
                keyhit1% = 15%
                return
L33680:     if keyhit1% <> 15% then return
                call "PRNTSCRN"
                return

        page_control
            rpttitle$  = "Detailed Facility Utilization Map Period " &   ~
                         fmdate$ & " To " & todate$
            page% = page% + 1%
            print page
            time$ = " "  :  call "TIME" (time$)
            print using L60070, date$, time$, company$, prnt_id$
            print using L60110, rpttitle$, page%
            print using L60200, str(hdate$,1%,31%)
            print skip(1)
            print using L60340
            print using L60370
            lcntr% = 6%
            return

        REM *************************************************************~
            *            S E E   M U L T I P L E   J O B S              *~
            *-----------------------------------------------------------*~
            * See Multiple Jobs in Work Center                          *~
            *************************************************************

        seemultiple
            mp% = edit% - 1 + f%
            multihead$ = "   Cap Part / Bom  Rte  Step)    "  &          ~
                         "Job Or Advice / Activity)"

L35110:     gosub getmultiple
            mpdate$ = yymmdd$(mp%)
            call "DATEFMT" (mpdate$)

L35150:     gosub display_multi

            if keyhit1% <>  0% then L35290
                mat cursor% = zer
                ret% = 0
                demand$, type$ = " "
                gosub call_screen
                field% = cursor%(1) - 2%
                if field% < 1% or field% > 20% then L35150
                f = (field% / 2) + .5 : field% = f
                if multidemand$(field%) = " " then L35150
                call "GETDEM"(1%, multidemand$(field%), #26,             ~
                                  #27, #28,  demand$, type$, ret%)
                goto L35150

L35290:     if keyhit1% = 16% then return
            if keyhit1% <> 3% then L35340
                mpl% = 0%
                goto L35150

L35340:     if keyhit1% <>  5% then L35380
                mpl% = max(0%, min(mpm%-10%, mpl%+10%))
                goto L35150

L35380:     if keyhit1% <> 9% then L35420
                mp% = max(1%, mp%-1%)
                goto L35110

L35420:     if keyhit1% <> 10% then L47040
                mp% = min(490%, mp%+1%)
                goto L35110


        getmultiple
            init(hex(00)) wccplowkey$
            init(" ") multidemand$(), multipart$(), multibom$(),         ~
                      multirte$(), multistep$(), multicode$(),           ~
                      multidscr$(), multiquant$()
            mpm%, mpl% = 0%
            multipart$(1) = "Nothing Now Scheduled"

            put str(wccplowkey$,,6) using L36090, str(wc$,,4), mp%
L36090:         FMT CH(4), BI(2)

L36110:     if mpm% > 99% then return
            call "PLOWALTS" (#23, wccplowkey$, 1%, 6%, f1%(23))
                if f1%(23) = 0 then return
            mpm% = mpm% + 1%
            get #23 using L36160, multidemand$(mpm%), su%, run%
L36160:         FMT XX(8), CH(19), XX(4), 2*BI(4)
            mquant% = su% + run%
            convert mquant% to multiquant$(mpm%), pic(######)
            call "READ100" (#25, multidemand$(mpm%), f1%(25))
            if f1%(25) <> 0 then get #25, multipart$(mpm%)               ~
                            else multipart$(mpm%) = " "

            get #23, using L36240, actkey$
L36240:         FMT XX(8), CH(23)

            call "WCACTSUB" (actkey$,            /* WCOUT Tag          */~
                             multibom$ (mpm%),   /* BOM from JBCROSS2  */~
                             multirte$ (mpm%),   /* RTE from JBCROSS2  */~
                             multistep$(mpm%),   /* Step from RTEMASTR */~
                             multicode$(mpm%),   /* Activity Code      */~
                             multidscr$(mpm%),   /* Activity Descr     */~
                             #23,                /* WCOUT              */~
                             #25,                /* JBCROSS2           */~
                             #17)                /* RTEMASTR           */

           goto L36110

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                screen% = 1
                init(hex(84)) lfac$()
                goto L40160

            deffn'111(fieldnr%)
                screen% = 2
                if wc$ <> "VEND" then L40125
                    str(pfdescr$(2%,1%),25%,13%) = " "
                    str(pfkeys$(2%),4%,1%) = hex(ff)
                    goto L40130
L40125:         str(pfdescr$(2%,1%),25%,13%) = "(5)Next Page"
                str(pfkeys$(2%),4%,1%) = hex(05)
L40130:         init(hex(84)) lfac$()
                if fieldnr% = 0 then init(hex(86)) lfac$()

L40160:           on fieldnr% gosub L40220          /* WORK CENTER CODE */
                  goto L40290

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40220:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40290:     accept                                                       ~
               at (01,02), "Review Work Center Capacity, Usage & Schedule~
        ~s",                                                              ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Work Center Code",                           ~
               at (06,30), fac(lfac$( 1)), wc$                  , ch(04),~
               at (07,02), "Work Center Description",                    ~
               at (07,30), fac(lfac$( 2)), wcdescr$             , ch(30),~
               at (08,02), "PM Frequency in Days",                       ~
               at (08,30), fac(lfac$( 3)), pmfreq$              , ch(03),~
               at (09,02), "Last PM Date",                               ~
               at (09,30), fac(lfac$( 4)), lastpm$              , ch(08),~
               at (10,02), "Units in 24 Hr. Period",                     ~
               at (10,30), fac(lfac$( 5)), units$               , ch(06),~
               at (10,40), fac(hex(8c)),   unitsdescr$          , ch(30),~
               at (12,02), "Usual Units Available On:",                  ~
               at (13,02), "   Mondays",                                 ~
               at (13,18), fac(lfac$( 6)), nuw$(1)              , ch(05),~
               at (14,02), "   Tuesdays",                                ~
               at (14,18), fac(lfac$( 7)), nuw$(2)              , ch(05),~
               at (15,02), "   Wednesdays",                              ~
               at (15,18), fac(lfac$( 8)), nuw$(3)              , ch(05),~
               at (16,02), "   Thursdays",                               ~
               at (16,18), fac(lfac$( 9)), nuw$(4)              , ch(05),~
               at (17,02), "   Fridays",                                 ~
               at (17,18), fac(lfac$(10)), nuw$(5)              , ch(05),~
               at (18,02), "   Saturdays",                               ~
               at (18,18), fac(lfac$(11)), nuw$(6)              , ch(05),~
               at (19,02), "   Sundays",                                 ~
               at (19,18), fac(lfac$(12)), nuw$(7)              , ch(05),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), fac(hex(8c)),   pfdescr$(screen%,1)  , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(screen%,2)  , ch(79),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)

               gosub pf1315
               if keyhit% = 15% then L40290
               return

        REM *************************************************************~
            *   W O R K   C E N T E R   D E F A U L T S   S C R E E N   *~
            *************************************************************~

            deffn'112
                init(hex(84)) lfac$()
                str(line2$,1%,61%) = "Work Center Defaults"

L40822:     accept                                                       ~
               at (01,02), "Manage Work Centers and Capacity",           ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(84)), wcmsg$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Work Center",                                ~
               at (05,25), fac(hex(84)),   wc$                  , ch(04),~
               at (05,41), fac(hex(8c)),   wcdescr$             , ch(32),~
                                                                         ~
               at (06,02), "Move/Queue Days",                            ~
               at (06,24), fac(lfac$(1%)), mq$                  , ch(05),~
               at (06,30), "After Step?",                                ~
               at (06,42), fac(lfac$(1%)), mqopt$               , ch(01),~
                                                                         ~
               at (08,02), "WC Setup Hours: xxxxxxxx        WC Units:",  ~
               at (08,18), fac(lfac$(2%)), suh$                 , ch(08),~
               at (08,44), fac(lfac$(2%)), su$                  , ch(06),~
                                                                         ~
               at (09,02), "Run Hours/Part: xxxxxxxx   WC Units/Part: xxx~
        ~xxx   Parts/Hr:xxxxxxxx",                                        ~
               at (09,18), fac(lfac$(3%)), runh$                , ch(08),~
               at (09,44), fac(lfac$(3%)), run$                 , ch(06),~
               at (09,63), fac(lfac$(3%)), runp$                , ch(08),~
                                                                         ~
               at (11,02), "%Yield From This Step",                      ~
               at (11,25), fac(lfac$(4%)), yield$               , ch(03),~
                                                                         ~
               at (12,02), "Setup Activity Code",                        ~
               at (12,25), fac(lfac$(5%)), sucode$              , ch(04),~
               at (12,41), fac(hex(8c)),   sudescr$             , ch(32),~
                                                                         ~
               at (13,02), "Run Activity Code",                          ~
               at (13,25), fac(lfac$(6%)), rucode$              , ch(04),~
                                                                         ~
               at (14,02), "Activity Description",                       ~
               at (14,25), fac(lfac$(7%)), rudescr$             , ch(30),~
                                                                         ~
               at (15,02), "%Complete At This Step",                     ~
               at (15,25), fac(lfac$(8%)), comp$                , ch(03),~
                                                                         ~
               at (16,02), "Move Unit Conv. Factor",                     ~
               at (16,25), fac(lfac$(9%)), handfactor$          , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), fac(hex(8c)),   pfdescr$(3%,1%),       ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(3%,2%),       ch(79),~
                                                                         ~
               keys(pfkeys$(3%)),                                        ~
               key(keyhit%)

               gosub pf1315
               if keyhit% = 15% then L40822
               return

        REM *************************************************************~
            *               R A N G E   I N P U T   S C R E E N         *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************~

        deffn'102(fieldnr%, edit%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L41150,         /* Work Center Range */   ~
                                L41150          /* Date Range        */
              goto L41180

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L41150:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41180:     accept                                                       ~
               at (01,02),                                               ~
                  "Input Range Selection Criteria",                      ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), "Work Center Range",                          ~
               at (07,30), fac(lfac$( 1)), fmwc$                , ch(04),~
               at (07,56), fac(lfac$( 1)), towc$                , ch(04),~
                                                                         ~
               at (08,02), "Date Range",                                 ~
               at (08,30), fac(lfac$( 2)), fmdate$              , ch(10),~
               at (08,56), fac(lfac$( 2)), todate$              , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$(3)), key(keyhit%)

               gosub pf1315
               if keyhit% = 15% then L41180
               return


        set_pf1
        if edit% = 2% then L41630     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)RETURN      "
            pfkeys$(3) = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L41600
                str(pf$(3),64)    = " "  : str(pfkeys$(3),16,1) = hex(ff)
            if fieldnr% > 1% then L41610
L41600:         str(pf$(2),18,18) = " "  : str(pfkeys$(3), 4,1) = hex(ff)
L41610:     return

L41630: if fieldnr% > 0% then L41720  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                   (8/24)Display/Print U" &        ~
                     "tilization Graph       (15)Print Screen"
            pf$(3) = "                   (9/25)Display/Print D" &        ~
                     "aily Details           (16)RETURN      "
            pfkeys$(3) = hex(01ffffffffffff08091819ff0dff0f1000)
            return
L41720:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$(3) = hex(01ffffffffffffffffffffff0dff0fff00)
            return
                                                                         ~

        REM *************************************************************~
            *    D I S P L A Y   W / C   U T I L I Z A T I O N          *~
            *-----------------------------------------------------------*~
            * Work Center Utilization Display Screen                    *~
            *************************************************************
        display_wc_util
        accept                                                           ~
               at (01,02), fac(hex(8c)), line1$                 , ch(79),~
               at (02,31), fac(hex(84)), wcdescr$               , ch(30),~
               at (03,03), fac(hex(ac)), colhead$               , ch(78),~
                                                                         ~
               at (04,03), "01",       at(04,41), "!  16",               ~
               at (05,03), "02",       at(05,41), "!  17",               ~
               at (06,03), "03",       at(06,41), "!  18",               ~
               at (07,03), "04",       at(07,41), "!  19",               ~
               at (08,03), "05",       at(08,41), "!  20",               ~
               at (09,03), "06",       at(09,41), "!  21",               ~
               at (10,03), "07",       at(10,41), "!  22",               ~
               at (11,03), "08",       at(11,41), "!  23",               ~
               at (12,03), "09",       at(12,41), "!  24",               ~
               at (13,03), "10",       at(13,41), "!  25",               ~
               at (14,03), "11",       at(14,41), "!  26",               ~
               at (15,03), "12",       at(15,41), "!  27",               ~
               at (16,03), "13",       at(16,41), "!  28",               ~
               at (17,03), "14",       at(17,41), "!  29",               ~
               at (18,03), "15",       at(18,41), "!  30",               ~
                                       at(19,41), "!  31",               ~
                                                                         ~
            at(04,10), fac(afac$(01)), davail$(01)              , ch(05),~
            at(05,10), fac(afac$(02)), davail$(02)              , ch(05),~
            at(06,10), fac(afac$(03)), davail$(03)              , ch(05),~
            at(07,10), fac(afac$(04)), davail$(04)              , ch(05),~
            at(08,10), fac(afac$(05)), davail$(05)              , ch(05),~
            at(09,10), fac(afac$(06)), davail$(06)              , ch(05),~
            at(10,10), fac(afac$(07)), davail$(07)              , ch(05),~
            at(11,10), fac(afac$(08)), davail$(08)              , ch(05),~
            at(12,10), fac(afac$(09)), davail$(09)              , ch(05),~
            at(13,10), fac(afac$(10)), davail$(10)              , ch(05),~
            at(14,10), fac(afac$(11)), davail$(11)              , ch(05),~
            at(15,10), fac(afac$(12)), davail$(12)              , ch(05),~
            at(16,10), fac(afac$(13)), davail$(13)              , ch(05),~
            at(17,10), fac(afac$(14)), davail$(14)              , ch(05),~
            at(18,10), fac(afac$(15)), davail$(15)              , ch(05),~
            at(04,51), fac(afac$(16)), davail$(16)              , ch(05),~
            at(05,51), fac(afac$(17)), davail$(17)              , ch(05),~
            at(06,51), fac(afac$(18)), davail$(18)              , ch(05),~
            at(07,51), fac(afac$(19)), davail$(19)              , ch(05),~
            at(08,51), fac(afac$(20)), davail$(20)              , ch(05),~
            at(09,51), fac(afac$(21)), davail$(21)              , ch(05),~
            at(10,51), fac(afac$(22)), davail$(22)              , ch(05),~
            at(11,51), fac(afac$(23)), davail$(23)              , ch(05),~
            at(12,51), fac(afac$(24)), davail$(24)              , ch(05),~
            at(13,51), fac(afac$(25)), davail$(25)              , ch(05),~
            at(14,51), fac(afac$(26)), davail$(26)              , ch(05),~
            at(15,51), fac(afac$(27)), davail$(27)              , ch(05),~
            at(16,51), fac(afac$(28)), davail$(28)              , ch(05),~
            at(17,51), fac(afac$(29)), davail$(29)              , ch(05),~
            at(18,51), fac(afac$(30)), davail$(30)              , ch(05),~
            at(19,51), fac(afac$(31)), davail$(31)              , ch(05),~
                                                                         ~
            at(04,17), fac(hex(84)), dused$(01)                 , ch(06),~
            at(05,17), fac(hex(84)), dused$(02)                 , ch(06),~
            at(06,17), fac(hex(84)), dused$(03)                 , ch(06),~
            at(07,17), fac(hex(84)), dused$(04)                 , ch(06),~
            at(08,17), fac(hex(84)), dused$(05)                 , ch(06),~
            at(09,17), fac(hex(84)), dused$(06)                 , ch(06),~
            at(10,17), fac(hex(84)), dused$(07)                 , ch(06),~
            at(11,17), fac(hex(84)), dused$(08)                 , ch(06),~
            at(12,17), fac(hex(84)), dused$(09)                 , ch(06),~
            at(13,17), fac(hex(84)), dused$(10)                 , ch(06),~
            at(14,17), fac(hex(84)), dused$(11)                 , ch(06),~
            at(15,17), fac(hex(84)), dused$(12)                 , ch(06),~
            at(16,17), fac(hex(84)), dused$(13)                 , ch(06),~
            at(17,17), fac(hex(84)), dused$(14)                 , ch(06),~
            at(18,17), fac(hex(84)), dused$(15)                 , ch(06),~
            at(04,58), fac(hex(84)), dused$(16)                 , ch(06),~
            at(05,58), fac(hex(84)), dused$(17)                 , ch(06),~
            at(06,58), fac(hex(84)), dused$(18)                 , ch(06),~
            at(07,58), fac(hex(84)), dused$(19)                 , ch(06),~
            at(08,58), fac(hex(84)), dused$(20)                 , ch(06),~
            at(09,58), fac(hex(84)), dused$(21)                 , ch(06),~
            at(10,58), fac(hex(84)), dused$(22)                 , ch(06),~
            at(11,58), fac(hex(84)), dused$(23)                 , ch(06),~
            at(12,58), fac(hex(84)), dused$(24)                 , ch(06),~
            at(13,58), fac(hex(84)), dused$(25)                 , ch(06),~
            at(14,58), fac(hex(84)), dused$(26)                 , ch(06),~
            at(15,58), fac(hex(84)), dused$(27)                 , ch(06),~
            at(16,58), fac(hex(84)), dused$(28)                 , ch(06),~
            at(17,58), fac(hex(84)), dused$(29)                 , ch(06),~
            at(18,58), fac(hex(84)), dused$(30)                 , ch(06),~
            at(19,58), fac(hex(84)), dused$(31)                 , ch(06),~
                                                                         ~
            at(04,24), fac(hex(8c)), dpct$(01)                  , ch(06),~
            at(05,24), fac(hex(8c)), dpct$(02)                  , ch(06),~
            at(06,24), fac(hex(8c)), dpct$(03)                  , ch(06),~
            at(07,24), fac(hex(8c)), dpct$(04)                  , ch(06),~
            at(08,24), fac(hex(8c)), dpct$(05)                  , ch(06),~
            at(09,24), fac(hex(8c)), dpct$(06)                  , ch(06),~
            at(10,24), fac(hex(8c)), dpct$(07)                  , ch(06),~
            at(11,24), fac(hex(8c)), dpct$(08)                  , ch(06),~
            at(12,24), fac(hex(8c)), dpct$(09)                  , ch(06),~
            at(13,24), fac(hex(8c)), dpct$(10)                  , ch(06),~
            at(14,24), fac(hex(8c)), dpct$(11)                  , ch(06),~
            at(15,24), fac(hex(8c)), dpct$(12)                  , ch(06),~
            at(16,24), fac(hex(8c)), dpct$(13)                  , ch(06),~
            at(17,24), fac(hex(8c)), dpct$(14)                  , ch(06),~
            at(18,24), fac(hex(8c)), dpct$(15)                  , ch(06),~
            at(04,65), fac(hex(8c)), dpct$(16)                  , ch(06),~
            at(05,65), fac(hex(8c)), dpct$(17)                  , ch(06),~
            at(06,65), fac(hex(8c)), dpct$(18)                  , ch(06),~
            at(07,65), fac(hex(8c)), dpct$(19)                  , ch(06),~
            at(08,65), fac(hex(8c)), dpct$(20)                  , ch(06),~
            at(09,65), fac(hex(8c)), dpct$(21)                  , ch(06),~
            at(10,65), fac(hex(8c)), dpct$(22)                  , ch(06),~
            at(11,65), fac(hex(8c)), dpct$(23)                  , ch(06),~
            at(12,65), fac(hex(8c)), dpct$(24)                  , ch(06),~
            at(13,65), fac(hex(8c)), dpct$(25)                  , ch(06),~
            at(14,65), fac(hex(8c)), dpct$(26)                  , ch(06),~
            at(15,65), fac(hex(8c)), dpct$(27)                  , ch(06),~
            at(16,65), fac(hex(8c)), dpct$(28)                  , ch(06),~
            at(17,65), fac(hex(8c)), dpct$(29)                  , ch(06),~
            at(18,65), fac(hex(8c)), dpct$(30)                  , ch(06),~
            at(19,65), fac(hex(8c)), dpct$(31)                  , ch(06),~
                                                                         ~
            at(04,06), fac(hex(8c)), dd$(01)                     , ch(3),~
            at(05,06), fac(hex(8c)), dd$(02)                     , ch(3),~
            at(06,06), fac(hex(8c)), dd$(03)                     , ch(3),~
            at(07,06), fac(hex(8c)), dd$(04)                     , ch(3),~
            at(08,06), fac(hex(8c)), dd$(05)                     , ch(3),~
            at(09,06), fac(hex(8c)), dd$(06)                     , ch(3),~
            at(10,06), fac(hex(8c)), dd$(07)                     , ch(3),~
            at(11,06), fac(hex(8c)), dd$(08)                     , ch(3),~
            at(12,06), fac(hex(8c)), dd$(09)                     , ch(3),~
            at(13,06), fac(hex(8c)), dd$(10)                     , ch(3),~
            at(14,06), fac(hex(8c)), dd$(11)                     , ch(3),~
            at(15,06), fac(hex(8c)), dd$(12)                     , ch(3),~
            at(16,06), fac(hex(8c)), dd$(13)                     , ch(3),~
            at(17,06), fac(hex(8c)), dd$(14)                     , ch(3),~
            at(18,06), fac(hex(8c)), dd$(15)                     , ch(3),~
            at(04,47), fac(hex(8c)), dd$(16)                     , ch(3),~
            at(05,47), fac(hex(8c)), dd$(17)                     , ch(3),~
            at(06,47), fac(hex(8c)), dd$(18)                     , ch(3),~
            at(07,47), fac(hex(8c)), dd$(19)                     , ch(3),~
            at(08,47), fac(hex(8c)), dd$(20)                     , ch(3),~
            at(09,47), fac(hex(8c)), dd$(21)                     , ch(3),~
            at(10,47), fac(hex(8c)), dd$(22)                     , ch(3),~
            at(11,47), fac(hex(8c)), dd$(23)                     , ch(3),~
            at(12,47), fac(hex(8c)), dd$(24)                     , ch(3),~
            at(13,47), fac(hex(8c)), dd$(25)                     , ch(3),~
            at(14,47), fac(hex(8c)), dd$(26)                     , ch(3),~
            at(15,47), fac(hex(8c)), dd$(27)                     , ch(3),~
            at(16,47), fac(hex(8c)), dd$(28)                     , ch(3),~
            at(17,47), fac(hex(8c)), dd$(29)                     , ch(3),~
            at(18,47), fac(hex(8c)), dd$(30)                     , ch(3),~
            at(19,47), fac(hex(8c)), dd$(31)                     , ch(3),~
                                                                         ~
            at(04,31), fac(hex(84)), note$(01)                  , ch(07),~
            at(05,31), fac(hex(84)), note$(02)                  , ch(07),~
            at(06,31), fac(hex(84)), note$(03)                  , ch(07),~
            at(07,31), fac(hex(84)), note$(04)                  , ch(07),~
            at(08,31), fac(hex(84)), note$(05)                  , ch(07),~
            at(09,31), fac(hex(84)), note$(06)                  , ch(07),~
            at(10,31), fac(hex(84)), note$(07)                  , ch(07),~
            at(11,31), fac(hex(84)), note$(08)                  , ch(07),~
            at(12,31), fac(hex(84)), note$(09)                  , ch(07),~
            at(13,31), fac(hex(84)), note$(10)                  , ch(07),~
            at(14,31), fac(hex(84)), note$(11)                  , ch(07),~
            at(15,31), fac(hex(84)), note$(12)                  , ch(07),~
            at(16,31), fac(hex(84)), note$(13)                  , ch(07),~
            at(17,31), fac(hex(84)), note$(14)                  , ch(07),~
            at(18,31), fac(hex(84)), note$(15)                  , ch(07),~
            at(04,72), fac(hex(84)), note$(16)                  , ch(07),~
            at(05,72), fac(hex(84)), note$(17)                  , ch(07),~
            at(06,72), fac(hex(84)), note$(18)                  , ch(07),~
            at(07,72), fac(hex(84)), note$(19)                  , ch(07),~
            at(08,72), fac(hex(84)), note$(20)                  , ch(07),~
            at(09,72), fac(hex(84)), note$(21)                  , ch(07),~
            at(10,72), fac(hex(84)), note$(22)                  , ch(07),~
            at(11,72), fac(hex(84)), note$(23)                  , ch(07),~
            at(12,72), fac(hex(84)), note$(24)                  , ch(07),~
            at(13,72), fac(hex(84)), note$(25)                  , ch(07),~
            at(14,72), fac(hex(84)), note$(26)                  , ch(07),~
            at(15,72), fac(hex(84)), note$(27)                  , ch(07),~
            at(16,72), fac(hex(84)), note$(28)                  , ch(07),~
            at(17,72), fac(hex(84)), note$(29)                  , ch(07),~
            at(18,72), fac(hex(84)), note$(30)                  , ch(07),~
            at(19,72), fac(hex(84)), note$(31)                  , ch(07),~
                                                                         ~
            at(20,02), fac(hex(8c)), line2x$(1)                 , ch(79),~
            at(21,02), fac(hex(ac)), line2x$(2)                 , ch(79),~
            at(22,02), fac(hex(8c)), line2x$(3)                 , ch(79),~
            at(23,02), fac(hex(8c)), line2x$(4)                 , ch(79),~
            at(24,02), fac(hex(8c)), line2x$(5)                 , ch(79),~
                                                                         ~
            keys(hex(01ff0304050708090c0d0e0f10)), key(keyhit%)

            gosub pf1315
            if keyhit% = 15% then display_wc_util
            return

        REM *************************************************************~
            *    D I S P L A Y   D E M A N D S   O R   P A R T S        *~
            *-----------------------------------------------------------*~
            * Display Demands or Parts in Work Center                   *~
            *************************************************************~

        display_both
            accept                                                       ~
                at (01,02), fac(hex(8c)), line1$                , ch(79),~
                at (02,31), fac(hex(84)), wcdescr$              , ch(30),~
                at (03,03), fac(hex(ac)), colhead$              , ch(78),~
                at (04,03), "01",         at(04,41), "!  16",            ~
                at (05,03), "02",         at(05,41), "!  17",            ~
                at (06,03), "03",         at(06,41), "!  18",            ~
                at (07,03), "04",         at(07,41), "!  19",            ~
                at (08,03), "05",         at(08,41), "!  20",            ~
                at (09,03), "06",         at(09,41), "!  21",            ~
                at (10,03), "07",         at(10,41), "!  22",            ~
                at (11,03), "08",         at(11,41), "!  23",            ~
                at (12,03), "09",         at(12,41), "!  24",            ~
                at (13,03), "10",         at(13,41), "!  25",            ~
                at (14,03), "11",         at(14,41), "!  26",            ~
                at (15,03), "12",         at(15,41), "!  27",            ~
                at (16,03), "13",         at(16,41), "!  28",            ~
                at (17,03), "14",         at(17,41), "!  29",            ~
                at (18,03), "15",         at(18,41), "!  30",            ~
                                          at(19,41), "!  31",            ~
                                                                         ~
                at(04,10), fac(hex(84)), davail$(01)             ,ch(05),~
                at(05,10), fac(hex(84)), davail$(02)             ,ch(05),~
                at(06,10), fac(hex(84)), davail$(03)             ,ch(05),~
                at(07,10), fac(hex(84)), davail$(04)             ,ch(05),~
                at(08,10), fac(hex(84)), davail$(05)             ,ch(05),~
                at(09,10), fac(hex(84)), davail$(06)             ,ch(05),~
                at(10,10), fac(hex(84)), davail$(07)             ,ch(05),~
                at(11,10), fac(hex(84)), davail$(08)             ,ch(05),~
                at(12,10), fac(hex(84)), davail$(09)             ,ch(05),~
                at(13,10), fac(hex(84)), davail$(10)             ,ch(05),~
                at(14,10), fac(hex(84)), davail$(11)             ,ch(05),~
                at(15,10), fac(hex(84)), davail$(12)             ,ch(05),~
                at(16,10), fac(hex(84)), davail$(13)             ,ch(05),~
                at(17,10), fac(hex(84)), davail$(14)             ,ch(05),~
                at(18,10), fac(hex(84)), davail$(15)             ,ch(05),~
                at(04,51), fac(hex(84)), davail$(16)             ,ch(05),~
                at(05,51), fac(hex(84)), davail$(17)             ,ch(05),~
                at(06,51), fac(hex(84)), davail$(18)             ,ch(05),~
                at(07,51), fac(hex(84)), davail$(19)             ,ch(05),~
                at(08,51), fac(hex(84)), davail$(20)             ,ch(05),~
                at(09,51), fac(hex(84)), davail$(21)             ,ch(05),~
                at(10,51), fac(hex(84)), davail$(22)             ,ch(05),~
                at(11,51), fac(hex(84)), davail$(23)             ,ch(05),~
                at(12,51), fac(hex(84)), davail$(24)             ,ch(05),~
                at(13,51), fac(hex(84)), davail$(25)             ,ch(05),~
                at(14,51), fac(hex(84)), davail$(26)             ,ch(05),~
                at(15,51), fac(hex(84)), davail$(27)             ,ch(05),~
                at(16,51), fac(hex(84)), davail$(28)             ,ch(05),~
                at(17,51), fac(hex(84)), davail$(29)             ,ch(05),~
                at(18,51), fac(hex(84)), davail$(30)             ,ch(05),~
                at(19,51), fac(hex(84)), davail$(31)             ,ch(05),~
                                                                         ~
                at(04,17), fac(hex(84)), shown$(01)              ,ch(23),~
                at(05,17), fac(hex(84)), shown$(02)              ,ch(23),~
                at(06,17), fac(hex(84)), shown$(03)              ,ch(23),~
                at(07,17), fac(hex(84)), shown$(04)              ,ch(23),~
                at(08,17), fac(hex(84)), shown$(05)              ,ch(23),~
                at(09,17), fac(hex(84)), shown$(06)              ,ch(23),~
                at(10,17), fac(hex(84)), shown$(07)              ,ch(23),~
                at(11,17), fac(hex(84)), shown$(08)              ,ch(23),~
                at(12,17), fac(hex(84)), shown$(09)              ,ch(23),~
                at(13,17), fac(hex(84)), shown$(10)              ,ch(23),~
                at(14,17), fac(hex(84)), shown$(11)              ,ch(23),~
                at(15,17), fac(hex(84)), shown$(12)              ,ch(23),~
                at(16,17), fac(hex(84)), shown$(13)              ,ch(23),~
                at(17,17), fac(hex(84)), shown$(14)              ,ch(23),~
                at(18,17), fac(hex(84)), shown$(15)              ,ch(23),~
                at(04,58), fac(hex(84)), shown$(16)              ,ch(23),~
                at(05,58), fac(hex(84)), shown$(17)              ,ch(23),~
                at(06,58), fac(hex(84)), shown$(18)              ,ch(23),~
                at(07,58), fac(hex(84)), shown$(19)              ,ch(23),~
                at(08,58), fac(hex(84)), shown$(20)              ,ch(23),~
                at(09,58), fac(hex(84)), shown$(21)              ,ch(23),~
                at(10,58), fac(hex(84)), shown$(22)              ,ch(23),~
                at(11,58), fac(hex(84)), shown$(23)              ,ch(23),~
                at(12,58), fac(hex(84)), shown$(24)              ,ch(23),~
                at(13,58), fac(hex(84)), shown$(25)              ,ch(23),~
                at(14,58), fac(hex(84)), shown$(26)              ,ch(23),~
                at(15,58), fac(hex(84)), shown$(27)              ,ch(23),~
                at(16,58), fac(hex(84)), shown$(28)              ,ch(23),~
                at(17,58), fac(hex(84)), shown$(29)              ,ch(23),~
                at(18,58), fac(hex(84)), shown$(30)              ,ch(23),~
                at(19,58), fac(hex(84)), shown$(31)              ,ch(23),~
                                                                         ~
                at(04,06), fac(hex(8c)), dd$(01)                 , ch(3),~
                at(05,06), fac(hex(8c)), dd$(02)                 , ch(3),~
                at(06,06), fac(hex(8c)), dd$(03)                 , ch(3),~
                at(07,06), fac(hex(8c)), dd$(04)                 , ch(3),~
                at(08,06), fac(hex(8c)), dd$(05)                 , ch(3),~
                at(09,06), fac(hex(8c)), dd$(06)                 , ch(3),~
                at(10,06), fac(hex(8c)), dd$(07)                 , ch(3),~
                at(11,06), fac(hex(8c)), dd$(08)                 , ch(3),~
                at(12,06), fac(hex(8c)), dd$(09)                 , ch(3),~
                at(13,06), fac(hex(8c)), dd$(10)                 , ch(3),~
                at(14,06), fac(hex(8c)), dd$(11)                 , ch(3),~
                at(15,06), fac(hex(8c)), dd$(12)                 , ch(3),~
                at(16,06), fac(hex(8c)), dd$(13)                 , ch(3),~
                at(17,06), fac(hex(8c)), dd$(14)                 , ch(3),~
                at(18,06), fac(hex(8c)), dd$(15)                 , ch(3),~
                at(04,47), fac(hex(8c)), dd$(16)                 , ch(3),~
                at(05,47), fac(hex(8c)), dd$(17)                 , ch(3),~
                at(06,47), fac(hex(8c)), dd$(18)                 , ch(3),~
                at(07,47), fac(hex(8c)), dd$(19)                 , ch(3),~
                at(08,47), fac(hex(8c)), dd$(20)                 , ch(3),~
                at(09,47), fac(hex(8c)), dd$(21)                 , ch(3),~
                at(10,47), fac(hex(8c)), dd$(22)                 , ch(3),~
                at(11,47), fac(hex(8c)), dd$(23)                 , ch(3),~
                at(12,47), fac(hex(8c)), dd$(24)                 , ch(3),~
                at(13,47), fac(hex(8c)), dd$(25)                 , ch(3),~
                at(14,47), fac(hex(8c)), dd$(26)                 , ch(3),~
                at(15,47), fac(hex(8c)), dd$(27)                 , ch(3),~
                at(16,47), fac(hex(8c)), dd$(28)                 , ch(3),~
                at(17,47), fac(hex(8c)), dd$(29)                 , ch(3),~
                at(18,47), fac(hex(8c)), dd$(30)                 , ch(3),~
                at(19,47), fac(hex(8c)), dd$(31)                 , ch(3),~
                at(20,02), fac(hex(8c)), line2x$(1)              ,ch(79),~
                at(21,02), fac(hex(ac)), line2x$(2)              ,ch(79),~
                at(22,02), fac(hex(8c)), line2x$(3)              ,ch(79),~
                at(23,02), fac(hex(8c)), line2x$(4)              ,ch(79),~
                at(24,02), fac(hex(8c)), line2x$(5)              ,ch(79),~
                                                                         ~
            keys(hex(0003040507080c0d0e0f10)), key(keyhit%)

            gosub pf1315
            if keyhit% = 15% then display_both
            return


        REM *************************************************************~
            *    D I S P L A Y   D A I L Y   U S A G E                  *~
            *-----------------------------------------------------------*~
            * Display Daily usage of Work Center                        *~
            *************************************************************~

        display_use
L45035:     accept                                                       ~
               at (01,02), "Detailed Facility Utilization Map for the Per~
        ~iod",                                                            ~
               at (01,51), fac(hex(8c)), fmdate$                , ch(10),~
               at (01,61), "to",                                         ~
               at (01,64), fac(hex(8c)), todate$                , ch(10),~
               at (02,02), "         as of",                             ~
               at (02,17), fac(hex(8c)), hdate$                 , ch(45),~
               at (04,02), fac(hex(ac)), colhead$               , ch(79),~
                                                                         ~
               at (05,02), fac(hex(85)), lastdate$(1%)          , ch(08),~
               at (07,02), fac(hex(85)), lastdate$(2%)          , ch(08),~
               at (09,02), fac(hex(85)), lastdate$(3%)          , ch(08),~
               at (11,02), fac(hex(85)), lastdate$(4%)          , ch(08),~
               at (13,02), fac(hex(85)), lastdate$(5%)          , ch(08),~
               at (15,02), fac(hex(85)), lastdate$(6%)          , ch(08),~
               at (17,02), fac(hex(85)), lastdate$(7%)          , ch(08),~
               at (19,02), fac(hex(85)), lastdate$(8%)          , ch(08),~
                                                                         ~
               at (05,12), fac(hex(85)), wcwc$(1%)              , ch(04),~
               at (07,12), fac(hex(85)), wcwc$(2%)              , ch(04),~
               at (09,12), fac(hex(85)), wcwc$(3%)              , ch(04),~
               at (11,12), fac(hex(85)), wcwc$(4%)              , ch(04),~
               at (13,12), fac(hex(85)), wcwc$(5%)              , ch(04),~
               at (15,12), fac(hex(85)), wcwc$(6%)              , ch(04),~
               at (17,12), fac(hex(85)), wcwc$(7%)              , ch(04),~
               at (19,12), fac(hex(85)), wcwc$(8%)              , ch(04),~
                                                                         ~
               at (05,17), fac(hex(85)), wcq$(1%)               , ch(11),~
               at (07,17), fac(hex(85)), wcq$(2%)               , ch(11),~
               at (09,17), fac(hex(85)), wcq$(3%)               , ch(11),~
               at (11,17), fac(hex(85)), wcq$(4%)               , ch(11),~
               at (13,17), fac(hex(85)), wcq$(5%)               , ch(11),~
               at (15,17), fac(hex(85)), wcq$(6%)               , ch(11),~
               at (17,17), fac(hex(85)), wcq$(7%)               , ch(11),~
               at (19,17), fac(hex(85)), wcq$(8%)               , ch(11),~
                                                                         ~
               at (05,31), fac(hex(85)), wcpart$(1%)            , ch(25),~
               at (07,31), fac(hex(85)), wcpart$(2%)            , ch(25),~
               at (09,31), fac(hex(85)), wcpart$(3%)            , ch(25),~
               at (11,31), fac(hex(85)), wcpart$(4%)            , ch(25),~
               at (13,31), fac(hex(85)), wcpart$(5%)            , ch(25),~
               at (15,31), fac(hex(85)), wcpart$(6%)            , ch(25),~
               at (17,31), fac(hex(85)), wcpart$(7%)            , ch(25),~
               at (19,31), fac(hex(85)), wcpart$(8%)            , ch(25),~
                                                                         ~
               at (05,57), fac(hex(85)), wctag$(1%)             , ch(19),~
               at (07,57), fac(hex(85)), wctag$(2%)             , ch(19),~
               at (09,57), fac(hex(85)), wctag$(3%)             , ch(19),~
               at (11,57), fac(hex(85)), wctag$(4%)             , ch(19),~
               at (13,57), fac(hex(85)), wctag$(5%)             , ch(19),~
               at (15,57), fac(hex(85)), wctag$(6%)             , ch(19),~
               at (17,57), fac(hex(85)), wctag$(7%)             , ch(19),~
               at (19,57), fac(hex(85)), wctag$(8%)             , ch(19),~
                                                                         ~
               at (06,61), fac(hex(8c)), multibom$   (1%)       , ch( 3),~
               at (08,61), fac(hex(8c)), multibom$   (2%)       , ch( 3),~
               at (10,61), fac(hex(8c)), multibom$   (3%)       , ch( 3),~
               at (12,61), fac(hex(8c)), multibom$   (4%)       , ch( 3),~
               at (14,61), fac(hex(8c)), multibom$   (5%)       , ch( 3),~
               at (16,61), fac(hex(8c)), multibom$   (6%)       , ch( 3),~
               at (18,61), fac(hex(8c)), multibom$   (7%)       , ch( 3),~
               at (20,61), fac(hex(8c)), multibom$   (8%)       , ch( 3),~
                                                                         ~
               at (06,65), fac(hex(8c)), multirte$   (1%)       , ch( 3),~
               at (08,65), fac(hex(8c)), multirte$   (2%)       , ch( 3),~
               at (10,65), fac(hex(8c)), multirte$   (3%)       , ch( 3),~
               at (12,65), fac(hex(8c)), multirte$   (4%)       , ch( 3),~
               at (14,65), fac(hex(8c)), multirte$   (5%)       , ch( 3),~
               at (16,65), fac(hex(8c)), multirte$   (6%)       , ch( 3),~
               at (18,65), fac(hex(8c)), multirte$   (7%)       , ch( 3),~
               at (20,65), fac(hex(8c)), multirte$   (8%)       , ch( 3),~
                                                                         ~
               at (06,70), fac(hex(8c)), multistep$  (1%)       , ch( 3),~
               at (08,70), fac(hex(8c)), multistep$  (2%)       , ch( 3),~
               at (10,70), fac(hex(8c)), multistep$  (3%)       , ch( 3),~
               at (12,70), fac(hex(8c)), multistep$  (4%)       , ch( 3),~
               at (14,70), fac(hex(8c)), multistep$  (5%)       , ch( 3),~
               at (16,70), fac(hex(8c)), multistep$  (6%)       , ch( 3),~
               at (18,70), fac(hex(8c)), multistep$  (7%)       , ch( 3),~
               at (20,70), fac(hex(8c)), multistep$  (8%)       , ch( 3),~
                                                                         ~
               at (06,15), fac(hex(8c)), multicode$  (1%)       , ch( 4),~
               at (08,15), fac(hex(8c)), multicode$  (2%)       , ch( 4),~
               at (10,15), fac(hex(8c)), multicode$  (3%)       , ch( 4),~
               at (12,15), fac(hex(8c)), multicode$  (4%)       , ch( 4),~
               at (14,15), fac(hex(8c)), multicode$  (5%)       , ch( 4),~
               at (16,15), fac(hex(8c)), multicode$  (6%)       , ch( 4),~
               at (18,15), fac(hex(8c)), multicode$  (7%)       , ch( 4),~
               at (20,15), fac(hex(8c)), multicode$  (8%)       , ch( 4),~
                                                                         ~
               at (06,20), fac(hex(8c)), multidscr$  (1%)       , ch(30),~
               at (08,20), fac(hex(8c)), multidscr$  (2%)       , ch(30),~
               at (10,20), fac(hex(8c)), multidscr$  (3%)       , ch(30),~
               at (12,20), fac(hex(8c)), multidscr$  (4%)       , ch(30),~
               at (14,20), fac(hex(8c)), multidscr$  (5%)       , ch(30),~
               at (16,20), fac(hex(8c)), multidscr$  (6%)       , ch(30),~
               at (18,20), fac(hex(8c)), multidscr$  (7%)       , ch(30),~
               at (20,20), fac(hex(8c)), multidscr$  (8%)       , ch(30),~
                                                                         ~
               at (22,02), fac(hex(ac)), line2x$(3)             , ch(79),~
               at (23,02), fac(hex(8c)), line2x$(4)             , ch(79),~
               at (24,02), fac(hex(8c)), line2x$(5)             , ch(79),~
                                                                         ~
               keys(hex(0003050d0f10)),                                  ~
               key(keyhit%)

            gosub pf1315
            if keyhit% = 15% then L45035


            if keyhit% = 5% then return
            if keyhit% <> 0% then return
            mat cursor% = zer : ret% = 0 :  demand$, type$ = " "
            gosub call_screen
            if cursor%(1) <  5 or cursor%(1) > 20 then L45035
            field% = cursor%(1) - 4

            field% = round((field%/2), 0)
            if wctag$(field%) = " " then L45035
            call "GETDEM"(1%, wctag$(field%), #26, #27, #28, demand$,    ~
                                     type$, ret%)
            goto L45035


        REM *************************************************************~
            *    D I S P L A Y   S H O P   L O A D I N G                *~
            *-----------------------------------------------------------*~
            * Display Daily Shop Loading for Work Centers               *~
            *************************************************************~

        display_load
L46070:     accept                                                       ~
               at (01,02), "Average Facility Loading For The Period",    ~
               at (01,43), fac(hex(8c)), fmdate$                , ch(10),~
               at (01,54), "to",                                         ~
               at (01,57), fac(hex(8c)), todate$                , ch(10),~
               at (02,08), "as of",                                      ~
               at (02,14), fac(hex(8c)), hdate$                 , ch(45),~
               at (04,06), fac(hex(85)), wcline$(1%)            , ch(64),~
               at (05,06), fac(hex(85)), wcline$(2%)            , ch(64),~
               at (06,06), fac(hex(85)), wcline$(3%)            , ch(64),~
               at (07,06), fac(hex(85)), wcline$(4%)            , ch(64),~
               at (08,06), fac(hex(85)), wcline$(5%)            , ch(64),~
               at (09,06), fac(hex(85)), wcline$(6%)            , ch(64),~
               at (10,06), fac(hex(85)), wcline$(7%)            , ch(64),~
               at (11,06), fac(hex(85)), wcline$(8%)            , ch(64),~
               at (12,06), fac(hex(85)), wcline$(9%)            , ch(64),~
               at (13,06), fac(hex(85)), wcline$(10%)           , ch(64),~
               at (14,06), fac(hex(85)), wcline$(11%)           , ch(64),~
               at (15,06), fac(hex(85)), wcline$(12%)           , ch(64),~
               at (16,06), fac(hex(85)), wcline$(13%)           , ch(64),~
               at (17,06), fac(hex(85)), wcline$(14%)           , ch(64),~
               at (18,06), fac(hex(85)), wcline$(15%)           , ch(64),~
               at (19,06), fac(hex(85)), wcline$(16%)           , ch(64),~
               at (20,06), fac(hex(85)), wcline$(17%)           , ch(64),~
                                                                         ~
               at (21,02), fac(hex(ac)), blankline$             , ch(79),~
               at (22,02), fac(hex(8c)), line2x$(3)             , ch(79),~
               at (23,02), fac(hex(8c)), line2x$(4)             , ch(79),~
               at (24,02), fac(hex(8c)), line2x$(5)             , ch(79),~
                                                                         ~
               keys(hex(0003050d0f10)),                                  ~
               key(keyhit%)

               gosub pf1315
               if keyhit% = 15% then L46070
               return


        REM *************************************************************~

        display_multi

L47040:    accept                                                        ~
            at (01,02), "W/C",                                           ~
            at (01,06), fac(hex(84)), wc$                       , ch(04),~
            at (01,11), fac(hex(84)), wcdescr$                  , ch(30),~
            at (01,50), "on",                                            ~
            at (01,53), fac(hex(84)), mpdate$                   , ch(08),~
            at (02,02), fac(hex(ac)), multihead$                , ch(79),~
                                                                         ~
            at (03,02), fac(hex(84)), multiquant$(mpl%+1)       , ch(06),~
            at (05,02), fac(hex(84)), multiquant$(mpl%+2)       , ch(06),~
            at (07,02), fac(hex(84)), multiquant$(mpl%+3)       , ch(06),~
            at (09,02), fac(hex(84)), multiquant$(mpl%+4)       , ch(06),~
            at (11,02), fac(hex(84)), multiquant$(mpl%+5)       , ch(06),~
            at (13,02), fac(hex(84)), multiquant$(mpl%+6)       , ch(06),~
            at (15,02), fac(hex(84)), multiquant$(mpl%+7)       , ch(06),~
            at (17,02), fac(hex(84)), multiquant$(mpl%+8)       , ch(06),~
            at (19,02), fac(hex(84)), multiquant$(mpl%+9)       , ch(06),~
            at (21,02), fac(hex(84)), multiquant$(mpl%+10)      , ch(06),~
                                                                         ~
            at (03,09), fac(hex(84)), multipart$(mpl%+1)        , ch(25),~
            at (05,09), fac(hex(84)), multipart$(mpl%+2)        , ch(25),~
            at (07,09), fac(hex(84)), multipart$(mpl%+3)        , ch(25),~
            at (09,09), fac(hex(84)), multipart$(mpl%+4)        , ch(25),~
            at (11,09), fac(hex(84)), multipart$(mpl%+5)        , ch(25),~
            at (13,09), fac(hex(84)), multipart$(mpl%+6)        , ch(25),~
            at (15,09), fac(hex(84)), multipart$(mpl%+7)        , ch(25),~
            at (17,09), fac(hex(84)), multipart$(mpl%+8)        , ch(25),~
            at (19,09), fac(hex(84)), multipart$(mpl%+9)        , ch(25),~
            at (21,09), fac(hex(84)), multipart$(mpl%+10)       , ch(25),~
                                                                         ~
            at (03,35), fac(hex(84)), multidemand$(mpl%+1)      , ch(19),~
            at (05,35), fac(hex(84)), multidemand$(mpl%+2)      , ch(19),~
            at (07,35), fac(hex(84)), multidemand$(mpl%+3)      , ch(19),~
            at (09,35), fac(hex(84)), multidemand$(mpl%+4)      , ch(19),~
            at (11,35), fac(hex(84)), multidemand$(mpl%+5)      , ch(19),~
            at (13,35), fac(hex(84)), multidemand$(mpl%+6)      , ch(19),~
            at (15,35), fac(hex(84)), multidemand$(mpl%+7)      , ch(19),~
            at (17,35), fac(hex(84)), multidemand$(mpl%+8)      , ch(19),~
            at (19,35), fac(hex(84)), multidemand$(mpl%+9)      , ch(19),~
            at (21,35), fac(hex(84)), multidemand$(mpl%+10)     , ch(19),~
                                                                         ~
            at (04,16), fac(hex(8c)), multibom$(mpl%+1)         , ch( 3),~
            at (06,16), fac(hex(8c)), multibom$(mpl%+2)         , ch( 3),~
            at (08,16), fac(hex(8c)), multibom$(mpl%+3)         , ch( 3),~
            at (10,16), fac(hex(8c)), multibom$(mpl%+4)         , ch( 3),~
            at (12,16), fac(hex(8c)), multibom$(mpl%+5)         , ch( 3),~
            at (14,16), fac(hex(8c)), multibom$(mpl%+6)         , ch( 3),~
            at (16,16), fac(hex(8c)), multibom$(mpl%+7)         , ch( 3),~
            at (18,16), fac(hex(8c)), multibom$(mpl%+8)         , ch( 3),~
            at (20,16), fac(hex(8c)), multibom$(mpl%+9)         , ch( 3),~
            at (22,16), fac(hex(8c)), multibom$(mpl%+10)        , ch( 3),~
                                                                         ~
            at (04,21), fac(hex(8c)), multirte$(mpl%+1)         , ch( 3),~
            at (06,21), fac(hex(8c)), multirte$(mpl%+2)         , ch( 3),~
            at (08,21), fac(hex(8c)), multirte$(mpl%+3)         , ch( 3),~
            at (10,21), fac(hex(8c)), multirte$(mpl%+4)         , ch( 3),~
            at (12,21), fac(hex(8c)), multirte$(mpl%+5)         , ch( 3),~
            at (14,21), fac(hex(8c)), multirte$(mpl%+6)         , ch( 3),~
            at (16,21), fac(hex(8c)), multirte$(mpl%+7)         , ch( 3),~
            at (18,21), fac(hex(8c)), multirte$(mpl%+8)         , ch( 3),~
            at (20,21), fac(hex(8c)), multirte$(mpl%+9)         , ch( 3),~
            at (22,21), fac(hex(8c)), multirte$(mpl%+10)        , ch( 3),~
                                                                         ~
            at (04,26), fac(hex(8c)), multistep$(mpl%+1)        , ch( 4),~
            at (06,26), fac(hex(8c)), multistep$(mpl%+2)        , ch( 4),~
            at (08,26), fac(hex(8c)), multistep$(mpl%+3)        , ch( 4),~
            at (10,26), fac(hex(8c)), multistep$(mpl%+4)        , ch( 4),~
            at (12,26), fac(hex(8c)), multistep$(mpl%+5)        , ch( 4),~
            at (14,26), fac(hex(8c)), multistep$(mpl%+6)        , ch( 4),~
            at (16,26), fac(hex(8c)), multistep$(mpl%+7)        , ch( 4),~
            at (18,26), fac(hex(8c)), multistep$(mpl%+8)        , ch( 4),~
            at (20,26), fac(hex(8c)), multistep$(mpl%+9)        , ch( 4),~
            at (22,26), fac(hex(8c)), multistep$(mpl%+10)       , ch( 4),~
                                                                         ~
            at (04,40), fac(hex(8c)), multicode$(mpl%+1)        , ch( 4),~
            at (06,40), fac(hex(8c)), multicode$(mpl%+2)        , ch( 4),~
            at (08,40), fac(hex(8c)), multicode$(mpl%+3)        , ch( 4),~
            at (10,40), fac(hex(8c)), multicode$(mpl%+4)        , ch( 4),~
            at (12,40), fac(hex(8c)), multicode$(mpl%+5)        , ch( 4),~
            at (14,40), fac(hex(8c)), multicode$(mpl%+6)        , ch( 4),~
            at (16,40), fac(hex(8c)), multicode$(mpl%+7)        , ch( 4),~
            at (18,40), fac(hex(8c)), multicode$(mpl%+8)        , ch( 4),~
            at (20,40), fac(hex(8c)), multicode$(mpl%+9)        , ch( 4),~
            at (22,40), fac(hex(8c)), multicode$(mpl%+10)       , ch( 4),~
                                                                         ~
            at (04,45), fac(hex(8c)), multidscr$(mpl%+1)        , ch(30),~
            at (06,45), fac(hex(8c)), multidscr$(mpl%+2)        , ch(30),~
            at (08,45), fac(hex(8c)), multidscr$(mpl%+3)        , ch(30),~
            at (10,45), fac(hex(8c)), multidscr$(mpl%+4)        , ch(30),~
            at (12,45), fac(hex(8c)), multidscr$(mpl%+5)        , ch(30),~
            at (14,45), fac(hex(8c)), multidscr$(mpl%+6)        , ch(30),~
            at (16,45), fac(hex(8c)), multidscr$(mpl%+7)        , ch(30),~
            at (18,45), fac(hex(8c)), multidscr$(mpl%+8)        , ch(30),~
            at (20,45), fac(hex(8c)), multidscr$(mpl%+9)        , ch(30),~
            at (22,45), fac(hex(8c)), multidscr$(mpl%+10)       , ch(30),~
                                                                         ~
            at (23,02), fac(hex(ac)), blankline$                , ch(79),~
            at (24,02),                                                  ~
        "(3)1st (5)Nxt (9)Prev Day (10)Nxt Day (15)Prnt (16)RTN (Cursor &~
        ~ENTER = DEMAND)",                                                ~
            keys(hex(000305090a0f10)), key(keyhit1%)

               gosub pf1315_1
               if keyhit1% = 15% then L47040
               return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Tests Data For The Items On Page 1.                       *~
            *************************************************************

            deffn'151
                errormsg$ = " "

        REM Test Data For Work Center Code
            call "GETCODE" (#11, wc$, " ", 0%, 0, wconfile%)
                if wconfile% <> 0% then L50160
                    if wc$ = " " then errormsg$ = hex(00)                ~
                    else errormsg$ = "Please Enter Valid Work Center"
                    return
L50160:     goto load_wc_info
            return


        REM *************************************************************~
            *         T E S T   R A N G E   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for Range Selections                            *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51120,         /* Work Center Range      */~
                              L51160          /* Date Range             */
            return

L51120: REM Test for Work Center Range            FMWC$
            if pos(fmwc$ = "?") = 0% then L51124
              fmwc$ = " "
              call "GETCODE" (#11, fmwc$, " ", 0%, 0, wconfile%)
L51124:     if pos(towc$ = "?") = 0% then L51130
              towc$ = " "
              call "GETCODE" (#11, towc$, " ", 0%, 0, wconfile%)
L51130:     call "TESTRNGE" (fmwc$, towc$, lowc$, hiwc$, errormsg$)
            return

L51160: REM Test for Date Range                   FMDATE$
            errormsg$ = " "
            call "DATEOKC" (fmdate$, fmdate%, errormsg$)
                if errormsg$ <> " " then return
            if todate$ = " " or todate$ = blankdate$ then todate$ = fmdate$
            call "DATEOKC" (todate$, fmdate%, errormsg$)
                if errormsg$ <> " " then return
            call "DATUFMTC" (fmdate$)
            call "DATUFMTC" (todate$)
            if fmdate$ <= todate$ then L51280
                errormsg$ = "Invalid Date Range"
                call "DATFMTC" (fmdate$)
                call "DATFMTC" (todate$)
                return
L51280:      call "PIPINDEX" (#1, fmdate$, fromsub%, u3%)
             if u3% = 0 then L51330
                errormsg$ = "From Date Is Outside Planning Calendar"
                call "DATFMTC" (fmdate$)
                call "DATFMTC" (todate$)
                return

L51330:     call "PIPINDEX" (#1, todate$, tosub%, u3%)
            if u3% = 0 then L51410
                errormsg$ = "'TO' Date Is Outside Planning Calendar"
                call "DATFMTC" (fmdate$)
                call "DATFMTC" (todate$)
                return

L51410:    call "DATFMTC" (fmdate$)
           call "DATFMTC" (todate$)
           return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1 for page 0
L60070: %RUN ########   ########              ###########################~
        ~#################################                   #############~
        ~###
*       * Header Line 2
L60110: %                                  ##############################~
        ~###################################                     PAGE:  ##~
        ~###
*       * Report Column Headings
L60132: % WC   Description                     % Util
L60133: %----  ------------------------------  ------

*       * Report Title for page 0                               : #####!
        %############################################################

*       * Report Lines for Facility Loading Graph Print
        % AVERAGE FACILITY LOADING FOR THE PERIOD ########## TO ##########

L60200: %                                                 As of #########~
        ~######################
L60220: %################################################################~
        ~#################################################################~
        ~###
L60240: %                                              +----1----2----3--~
        ~--4----5----6----7----8----9----+ PERCENT
L60260: %                                                   0    0    0  ~
        ~  0    0    0    0    0    0      USAGE
L60274: %   @@ - Work Center Utilization Over 100 Percent

*       * Print Lines for Daily Usage Report
        % DETAILED FACILITY UTILIZATION MAP FOR THE PERIOD ########## TO ##~
        ~########                                            Page: ###

        %          AS OF #############################################

L60340: %  DATE    WKCTR  UNITS USED   FOR JOB OR ADVICE    TO BUILD PART~
        ~              BOM RTE STEP  CODE ACTIVITY

L60370: % -------- -----  ----------   -------------------  -------------~
        ~------------  --- --- ----  ---- ------------------------------

L60400: % ########

L60420: %           ####  ##########   ###################  #############~
        ~############  ### ### ####  #### ##############################

L60450: %                                                *******  END OF ~
        ~REPORT  ########  *******
        REM *************************************************************
        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
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

        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" (#60)
            end
