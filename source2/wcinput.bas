        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  W   W   CCC   IIIII  N   N  PPPP   U   U  TTTTT          *~
            *  W   W  C   C    I    NN  N  P   P  U   U    T            *~
            *  W   W  C        I    N N N  PPPP   U   U    T            *~
            *  W W W  C   C    I    N  NN  P      U   U    T            *~
            *   W W    CCC   IIIII  N   N  P       UUU     T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * WCINPUT  - Maintain Routing Work Centers Master File.     *~
            *            User can input/modify the information stored   *~
            *            for a work center. Also can review the units   *~
            *            available and used by day a month at a time.   *~
            *            For new work centers the capacity is auto      *~
            *            generated.                                     *~
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
            * 12/14/82 ! ORIGINAL                                 ! GLW *~
            * 09/07/83 ! ADDED DELETE OPTION                      ! HES *~
            * 09/07/83 ! ADDED RESET FOR RANGE OF DAYS            ! GLW *~
            * 07/17/84 ! BYPASS VEND DURING USED REBUILD          ! KAB *~
            *          ! ALSO CHANGE QUANTITY TO UNITS USED       ! KAB *~
            * 08/21/84 ! FIX PROBLEMS WITH SCREEN DISPLAY         ! BLT *~
            * 05/10/85 ! IMPLEMENT ACTIVITY CODE DISPLAYS         ! KAB *~
            * 06/18/85 ! ADD CALL TO GETDEM TO FIND TOP DEMAND    ! WPH *~
            * 08/02/85 ! ADDED PLOWCODE TO SHOW LIST OF WC'S      ! WPH *~
            * 09/12/85 ! Added 'Numeber Of Units In One Day' Field! HES *~
            *          ! Moved 'Used' Info Onto 'Available' Record!     *~
            *          ! Considerable House Cleaning              !     *~
            * 03/12/86 ! Added Call to CDANPOST.                  ! LDJ *~
            * 10/25/86 ! Change WCOUT Format.                     ! HES *~
            * 05/14/87 ! Standard Costing Changes                 ! ERN *~
            * 11/09/88 ! Reload USED% After WCBUILD at Set/Reset  ! KAB *~
            * 02/20/89 ! Removed most display and print functions ! MJB *~
            *          !  to new WCDSPLY.  Also changed format of !     *~
            *          !  Usage line on Utilization line.         !     *~
            * 04/10/90 ! Increased monthly usage fields.          ! JDH *~
            * 06/20/91 ! PRR 11796  Rounded the WCs Percent of    ! SID *~
            *          !            Utilization line.             !     *~
            *          !     Added 'ALLFREE'                      !     *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 07/02/92 ! Added Select for PIPIN & pass to GETDEM  ! WPH *~
            * 10/26/92 ! PRR 12515, Added PF4)Previous            ! SID *~
            *          ! PRR 12573, Fixed the date format problem !     *~
            * 05/05/93 ! Added WORKCENTER GROUP field as CH(3)    ! WPH *~
            *          ! and GENCODES validation.                 ! WPH *~
            * 02/28/94 ! Added Work Center Defaults functionality ! MLJ *~
            *          !   via new screen 2 and new file WCDFLTS. !     *~
            * 03/24/95 ! PRR 13369  Modified edit for run time so ! JBK *~
            *          !   that a change to any of three fields   !     *~
            *          !   will cause a recalculation of the other!     *~
            *          !   two fields.                            !     *~
            *          ! Misc. - Modified edit for setup time so  !     *~
            *          !   that a change to either of two fields  !     *~
            *          !   will cause a recalculation of the other!     *~
            *          !   fields.  Tightened up edits of the     !     *~
            *          !   handling factor to not allow 0 as an   !     *~
            *          !   to entry.                              !     *~
            * 06/23/95 ! Fixed RETURN CLEAR ELSE. Unix Compatiblty! JDH *~
            * 01/15/96 ! Ergonomic chg - inputmode of default scrn! JDH *~
            * 09/05/96 ! Millie date conversion                   ! DER *~
            * 09/04/97 ! Change Rounding on Call to Convert (run$)! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            avail%(490),                 /* CAP AVAILABLE              */~
            blankline$79,                /* Blank line for display     */~
            can$1,                       /* GENCODES file exists flag  */~
            ccyymmdd$8,                  /* ccyymmdd                   */~
            comp$3,                      /* % Complete through this stp*/~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            davail$(31)5,                /* DISPLAYED AVAILABLE        */~
            dd$(31)3,                    /* DISPLAYED DAY OF WEEK NAME */~
            delete$79,                   /* DELETE MESSAGE             */~
            demand$(490)19,              /* DEMANDS                    */~
            dow$(490)3,                  /* DAY OF WEEK NAMES          */~
            dpct$(31)6,                  /* DISPLAYED PCT USED         */~
            dsyymmdd$8,                  /* Displayed fmt yymmdd()     */~
            dused$(31)6,                 /* DISPLAYED USED             */~
            end$10,                      /* END DATE FOR RESET         */~
            editmsg$79,                  /*                            */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            fmyymmdd$8,                  /* Formatted YYMMDD date      */~
            gdem$19,                     /* FROM WCCROSS               */~
            gpart$25,                    /* FROM WCCROSS               */~
            group$3,                     /* Workcenter Group           */~
            groupdescr$32,               /* Workcenter Group Descrip   */~
            h$(490)1,                    /* HOLIDAYS                   */~
            handfactor$8,                /* Move Unit Conversion Factor*/~
            hdate$45,                    /*                            */~
            hdr$60,                      /* Header for ASKUSER         */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            insert$79,                   /* Defalut Insert Message     */~
            line2$79,                    /* Screen line #2             */~
            line2x$79,                   /* Screen line #20            */~
            lits$(5)78,                  /* Screen Literals            */~
            lastpm$8,                    /* LAST PM DATE               */~
            lfac$(31)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            message$79,                  /* INPUT MESSAGE              */~
            msg$(3)80,                   /* 3 lines for ASKUSER        */~
            mm%(490),                    /* MONTHS                     */~
            modate$(12)9,                /* MONTH NAMES                */~
            mq$5,                        /* Move/Queue Time In Days    */~
            mqopt$1,                     /* Move/Queue After Step?     */~
            new$(7)5,                    /* NEW NORM UNITS WORKD STRING*/~
            new%(7),                     /* NEW NORM UNITS WORKED      */~
            nuw$(7)5,                    /* NORM UNITS WORKED          */~
            nuw%(7),                     /* NORM UNITS WORKED (NUMERIC)*/~
            note$(31)7,                  /* FOR HOLIDAYS               */~
            packed$245,                  /* For loading Holiday Schedle*/~
            pfdescr$(4,3)79,             /* PF Key Descriptions        */~
            pfkeys$(4)32,                /* PF Keys Active             */~
            plowkey$50,                  /* WORKA VARIABLE             */~
            pmfreq$3,                    /* PM FREQUENCY IN DAYS       */~
            readkey$99,                  /* Misc Read Key              */~
            rucode$4,                    /* Run Activity Code          */~
            rudescr$30,                  /* Run Activity Code Descr    */~
            run$6,                       /* Run Time ( WCunits/Part )  */~
            runh$8,                      /* Run Time ( Hours/Part )    */~
            runp$8,                      /* Run Time ( Parts/Hour )    */~
            savesuh$8,                   /* Work Variable              */~
            saverun$6,                   /* Work Variable              */~
            saverunh$8,                  /* Work Variable              */~
            seepart$(490)25,             /* PARTS INTO WORK CENTERS    */~
            seequant%(490),              /* QUANTS INTO WORK CENTERS   */~
            sfac$(31)1,                  /* Saved Faxs During Edit     */~
            shown$(31)23,                /* FOR SCREEN DISPLAY         */~
            start$10,                    /* START DATE FOR RESET       */~
            su$6,                        /* Setup Time ( Units)        */~
            sucode$4,                    /* Setup Activity Code        */~
            sudescr$32,                  /* Setup Activity Code Descr  */~
            suh$8,                       /* Setup Time ( Hours)        */~
            temp$8,                      /* Work Variable              */~
            toyymmdd$8,                  /* Formatted YYMMDD date      */~
            tempkey$50,                  /* GENCODES Temp key          */~
            unfend$10,                   /* unfmt end date             */~
            unfstart$10,                 /* unfmt start date           */~
            units$6,                     /* UNITS IN A 24hr PERIOD     */~
            unitsdescr$30,               /* UNITS IN A 24hr PERIOD     */~
            used%(490),                  /* CAPACITY USED              */~
            wc$4,                        /* WORK CENTER CODE           */~
            wccplowkey$27,               /* ALT PLOW ON WCCROSS        */~
            wcdescr$32,                  /* WORK CENTER DESCRIPTION    */~
            wcstat$1,                    /* Filler field               */~
            yield$3,                     /* Process Yield In %         */~
            yy%(490),                    /* YEARS                      */~
            yymmdd$(490)6                /*                            */

        dim f1%(64)                      /* RECORD-ON-FILE FLAGS       */

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
            * #1  ! SYSFILE2 ! SYSTEM ANYTHING/EVERYTHING FILE          *~
            * #2  ! GENCODES ! General Codes Validation File            *~
            * #11 ! WCMASTR  ! WORK CENTER MASTER FILE                  *~
            * #12 ! CALMASTR ! PRODUCTION CALENDAR, 490 CONSECUTIVE DAYS*~
            * #17 ! RTEMASTR ! STANDARD ROUTING FILE W/ALTERNATE ROUTES *~
            * #23 ! WCOUT    ! WORK CENTER USAGE CROSS REFERENCE        *~
            * #25 ! JBCROSS2 ! JOB/BOM/RTE XREF FILE                    *~
            * #26 ! PIPCROSS ! HARD PEGGING FILE                        *~
            * #27 ! DEMMASTR ! DEMAND MASTER FILE                       *~
            * #29 ! WCDFLTS  ! WORK CENTER DEFAULTS FILE                *~
            *************************************************************

            select #1,  "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            select #02,  "GENCODES",                                     ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #11, "WCMASTR",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos = 2  , keylen = 5,                       ~
                         alt key 1, keypos = 1, keylen = 6

            select #12, "CALMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 1962,                                 ~
                         keypos = 1, keylen = 2

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

            select #29, "WCDFLTS",                                       ~
                        varc,     indexed,  recsize =   100,             ~
                        keypos =    1, keylen =   5

           call "SHOSTAT" ("Preparing For Work Center Management")

           call "OPENCHCK" (#1, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#2, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#11, 0%, 0%, 100%, " ")
           call "OPENCHCK" (#12, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#17, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#23, 0%, 0%, 300%, " ")
           call "OPENCHCK" (#25, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#26, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#27, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#28, 0%, 0%, 0%, " ")
           call "OPENCHCK" (#29, 0%, 0%, 100%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************
            nix%  = 0%
            date$ = date
            call "DATEFMT" (date$, nix%, ccyymmdd$)
            pipidx_str% = 0%
            pipidx_end% = 0%

            editmsg$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press RETURN."

           call "DATE" addr ("HD", hdate$)

            modate$(01) = "JANUARY  "
            modate$(02) = "FEBRUARY "
            modate$(03) = "MARCH    "
            modate$(04) = "APRIL    "
            modate$(05) = "MAY      "
            modate$(06) = "JUNE     "
            modate$(07) = "JULY     "
            modate$(08) = "AUGUST   "
            modate$(09) = "SEPTEMBER"
            modate$(10) = "OCTOBER  "
            modate$(11) = "NOVEMBER "
            modate$(12) = "DECEMBER "


*        See if WCGROUPS file in place
            can$ = "N"
            readkey$ = "WCGROUPS "
            call "PLOWNEXT" (#02, readkey$, 9%, f1%(2%))
              if f1%(2%) = 1% then can$ = "Y"

            REM GET THE PRODUCTION CALEDAR
                gosub loadcal

            REM Load Holiday Schedule...
            h$() = all("0")
            call "REDALT0" (#1, "HOLIDAY SCHEDULE", 0%, f1%(1))
                if f1%(1) = 0% then L09380
            get #1, using L09350, packed$
L09350:     FMT XX(20), CH(245)
            hexunpack packed$ to str(h$())

            hdr$ = "*** CALENDAR ERROR ***"
L09380:     call "PIPINDEX" (#1, " ", today%, u3%)
            if u3% <> 1 then L09410
L09391:       ask% = 2%
              msg$(1) = "Can't Find Planning Calendar In SYSFILE2"
              msg$(2) ="Please Correct the Problem and Rerun This Program"
              msg$(3) = "Press RETURN to EXIT"
              call "ASKUSER" (ask%, hdr$, msg$(1), msg$(2), msg$(3))
              if ask% <> 0% then L09391
              goto L65000
L09410:     if u3% = 0 then L09440
L09412:       ask% = 2%
              msg$(1%) = "Todays Date Is Outside Planning Calendar"
              msg$(2%)="Please Correct the Problem and Rerun This Program"
              msg$(3%) = "Press RETURN to EXIT"
              call "ASKUSER" (ask%, hdr$, msg$(1), msg$(2), msg$(3))
              if ask% <> 0% then L09412
              goto L65000

L09440:     pfdescr$(1%,1%)="(1)Start Over                               ~
        ~                   (13)Instructions"
            pfdescr$(1%,2%)="                                            ~
        ~                   (15)Print Screen"
            pfdescr$(1%,3%)="                                            ~
        ~                   (16)Exit Program"


            pfdescr$(2%,1%)="(1)Start Over                               ~
        ~                   (13)Instructions"
            pfdescr$(2%,2%)="(2)Display Utilization  (8)RESET Capacity   ~
        ~                   (15)Print Screen"
            pfdescr$(2%,3%)="                       (12)Delete Center    ~
        ~                   (16)SAVE DATA   "

            pfdescr$(3%,1%)="(1)Start Over                               ~
        ~                   (13)Instructions"
            pfdescr$(3%,2%)="                                            ~
        ~                   (15)Print Screen"
            pfdescr$(3%,3%)="                                            ~
        ~                                   "

            pfdescr$(4%,1%)="(1)Start Over           (4)Previous Page    ~
        ~                   (13)Instructions"
            pfdescr$(4%,2%)="                                            ~
        ~                   (15)Print Screen"
            pfdescr$(4%,3%)="                                            ~
        ~                   (16)SAVE DATA"

            pfkeys$(1%) = hex(0001ff0d0f10)
            pfkeys$(2%) = hex(000102ff080c0d0f10)
            pfkeys$(3%) = hex(0001ffff0d0f10)
            pfkeys$(4%) = hex(0001040d0f10)

            str(line2$,62%) = " WCINPUT: " & str(cms2v$,1%,8%)

            lits$(1%)= "TAB To Day And Press (RETURN) To See Top " &     ~
                       "Level Demand"
            lits$(2%)= "   Day  Avail   Used  Usage Note      !  "  &    ~
                       "   Day  Avail   Used  Usage Note"
            lits$(3%)= "   Day Avail  Job/Demand Scheduled    !  "  &    ~
                       "   Day Avail  Job/Demand Scheduled"
            lits$(4%)= "   Day  Used  Part To Be Produced     !  "  &    ~
                       "   Day  Used  Part To Be Produced"
            lits$(5%)= "To Modify Units Available Press (RETURN)"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode

            call "ALLFREE" addr(#11)

            init(" ") errormsg$, delete$, wc$, wcdescr$, lastpm$, nuw$(),~
                      pmfreq$, units$, unitsdescr$, group$, groupdescr$, ~
                      mq$, mqopt$, suh$, su$, runh$, run$, runp$, yield$,~
                      sucode$, sudescr$, rucode$, rudescr$, comp$,       ~
                      handfactor$, savesuh$, saverun$, saverunh$,        ~
                      insert$, wcstat$

            mat avail% = zer  :  mat used% = zer  :  mat nuw% = zer
            wconfile%, wcbuild% = 0%
            mq, handfactor, su, run, yield, comp = 0

            for fieldnr% = 1% to 13%
L10140:         gosub'051(fieldnr%)
                      if enabled% = 0% then L10220
L10160:         gosub'101(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if fieldnr% = 1% then L10180
                         if keyhit% <>  4% then L10180
L10173:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10160
                         if fieldnr% = 1% then L10140
                         goto L10173
L10180:               if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit% <>  0% then       L10160
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10160
L10220:         next fieldnr%

            if wc$ = "VEND" then L11000    /* No Dflts - Outside Process */
            call "ALLFREE" addr(#29)
                gosub L42045
                if keyhit% =  1% then gosub startover
                if keyhit% = 16% then editmode
            for fieldnr% = 1% to 9%
                gosub'052(fieldnr%)
                      if enabled% = 0% then L10630
L10540:         gosub'201(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  <> 4% then       L10610
L10570:                   fieldnr% = max(1%, fieldnr% -1%)
                          gosub'052(fieldnr%)
                              if enabled% <> 0% then L10540
                              goto L10570
L10610:               if keyhit%  = 16% and fieldnr% = 1% then datasave
                      if keyhit% <>  0% then       L10540
L10630:         gosub'251(fieldnr%)
                      if errormsg$ <> " " then L10540
            next fieldnr%

L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************

        editmode
            message$ = editmsg$
L11080:     gosub'111(0%)
                  delete$ = " "
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then see_capacities
                  if keyhit%  =  5% then editpg2
                  if keyhit%  =  8% then gosub re_gen_capacity
                  if keyhit%  = 12% then delete_center
                  if keyhit%  = 16% then datasave
                  if keyhit% <>  0% then L11080
            fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 2% or fieldnr% > 14% then L11080
            if fieldnr% = 7% then L11080
            if fieldnr% > 7% then fieldnr% = fieldnr% - 1%

            gosub'051(fieldnr%)
                  if enabled% = 0% then editmode
L11230:     gosub'111(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11230
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11230
            goto editmode

        editpg2
            message$ = editmsg$
L11530:     gosub'211(0%)
                errormsg$ = " "
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  4% then editmode
                if keyhit%  = 11% then insertmode
                if keyhit%  = 16% then datasave
                if keyhit% <>  0% then L11530
                   if cursor%(1%) < 8%                                   ~
                       then fieldnr% = cursor%(1%) - 5%
                   if cursor%(1%) > 6% and cursor%(1%) < 11%             ~
                       then fieldnr% = cursor%(1%) - 6%
                   if cursor%(1%) > 9% and cursor%(1%) < 17%             ~
                       then fieldnr% = cursor%(1%) - 7%
                   if cursor%(1%) > 16% then fieldnr% = 1%
            gosub'052(fieldnr%)
                insert$ = " "
                if enabled% = 0% then editpg2
L11710:     gosub'211(fieldnr%)
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then L11710
            gosub'251(fieldnr%)
                if errormsg$ <> " " then L11710
            goto editpg2

        insertmode
            for fieldnr% = 1% to 9%
                gosub'052(fieldnr%)
                      if enabled% = 0% then L11910
L11820:         gosub'201(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  <> 4% then       L11900
L11850:                   fieldnr% = max(1%, fieldnr% -1%)
                          gosub'052(fieldnr%)
                              if enabled% <> 0% then L11820
                              goto L11850
L11900:               if keyhit% <>  0% then       L11820
L11910:         gosub'251(fieldnr%)
                      if errormsg$ <> " " then L11820
            next fieldnr%
            goto editpg2

        REM *************************************************************~
            *             D E L E T E   W O R K   C E N T E R           *~
            *                                                           *~
            * WILL DELETE THE WORK CENTER IF POSSIBLE.                  *~
            *************************************************************

        delete_center
            message$ = " "
            if wc$ = "VEND" then L15320
*        Check For WCOUTs...
            init (hex(00)) str(plowkey$,5)
            str(plowkey$,,4%) = wc$
L15120:     call "PLOWALTS" (#23, plowkey$, 1%, 4%, f1%(23))
            if f1%(23) = 0% then L15220
                get #23 using L15150, date%, su%, run%
L15150:              FMT XX(4), BI(2), XX(25), 2*BI(4)
                if su% = 0% and run% = 0% then L15120
                if date% < max(1,today%-4%) then L15120 /* 5 DAY FUDGE */
                   dsyymmdd$ = yymmdd$(date%)
                   call "DATEFMT" (dsyymmdd$)
                   delete$ = "Can't Delete Center: Planned Usage On "  ~
                                                         & dsyymmdd$
                   goto editmode

L15220
*        Check To See If Referenced In RTEMASTR...
            plowkey$ = all(hex(00))
            str(plowkey$,,4) = wc$
            call "PLOWALTS" (#17, plowkey$, 1%, 4%, f1%(17))
            if f1%(17) = 0% then L15320
                delete$ = "Can't Delete: Used in Route "    &            ~
                          str(plowkey$,30,3) & " for Part " &            ~
                          str(plowkey$,5,25)
                goto editmode

L15320:     gosub delete_ok  /* If not Ok never comes back */

*        Actually Delete The Record...
            call "READ101" (#11, wc$, f1%(11))
            if f1%(11%) = 0% then L15390
                delete #11
                call "CDANPOST" (#11, "D")

*        Now Delete WC Default Record if it exists...
             plowkey$ = str(wc$) & " "
             call "READ101" (#29, plowkey$, f1%(29%))
                 if f1%(29%) <> 1% then L15390
             delete #29

L15390:     if wc$ = "VEND" then inputmode
                init (hex(00)) plowkey$  /* Slay WCOUTS, 5^ each */
                str(plowkey$,,4) = str(wc$,,4)
L15420:         call "PLOWAL1" (#23, plowkey$, 1%, 4%, f1%(23))
                if f1%(23%) = 0% then inputmode
                     delete #23
                     goto L15420

        delete_ok   /* Allow User Opportunity To Abort Delete */
L15480:     u3% = 2%
            call "ASKUSER" (u3%, "***** DELETE WORK CENTER? *****",      ~
                            "Press RETURN to DELETE Work Center", "-OR-",~
                            "Press PF-1 to Abort Delete")
            if u3%  = 0% then return
            if u3% <> 1% then L15480
                   return clear
                   goto editmode

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave

           if lastpm$ <> " " then call "DATUNFMT" (lastpm$)
           units = 24
           convert units$ to units, data goto L19120
L19120:    mat nuw% = zer
           for i% = 1% to 7%
               convert nuw$(i%) to nuw%(i%), data goto L19150
L19150:    next i%

           call "REDALT1" (#11, wc$, 0%, f1%(11%))
           if f1%(11%) <> 1% then                                        ~
           write #11,using L19200,wcstat$, wc$, wcdescr$, nuw%(), pmfreq$,~
                                 lastpm$, avail%(), used%(), units,group$~
           else                                                          ~
           rewrite #11,using L19200,wcstat$,wc$,wcdescr$, nuw%(), pmfreq$,~
                                 lastpm$, avail%(), used%(), units,group$

           if wconfile% = 0% then call "CDANPOST" (#11, "A")             ~
                             else call "CDANPOST" (#11, "C")

L19200:    FMT  CH(01),                  /* UNUSED                     */~
                CH(05),                  /* WORK CENTER                */~
                CH(30),                  /* WC DESCR                   */~
                7*BI(02),                /* WEEKLY UNITS AVAIL, MON 1st*/~
                CH(03),                  /* PM FREQUENCY IN DAYS       */~
                CH(06),                  /* LAST PM DATE               */~
                490*BI(02),              /* CAPACITY AVAILABLE         */~
                490*BI(02),              /* CAPACITY USED              */~
                BI(2),                   /* UNITS IN 24 HOURS          */~
                CH(3)                    /* GROUP                      */


            if wcbuild% = 0% then L19500

        REM ********* Rebuild Used Record if Required **********
            call "WCBUILD" (wc$, #11, #23)

L19500: REM Save Work Center Defaults...
            if wc$ = "VEND" then inputmode  /* No Dflts, Outside Process*/

        REM Does user want to save on input w/o entering defaults?  ...
            if screen% = 3% and keyhit% = 16% then L19740

            if yield$ = " " then yield$ = "100"         /* Just in case */
            if handfactor$ = " " then handfactor$ = "1.00" /*  " " "    */
            if mq$ = " " then mq$ = "-1"
                convert mq$ to mq, data goto L19545
L19545:     convert handfactor$ to handfactor, data goto L19550
L19550:     if su$ = " " then su$ = "-1"
                convert su$ to su, data goto L19560
L19560:     if run$ = " " then run$ = "-1"
                convert run$ to run, data goto L19570
L19570:     convert yield$ to yield, data goto L19580
L19580:     if comp$ = " " then comp$ = "-1"
                convert comp$ to comp, data goto L19590
L19590:     if mqopt$ <> "Y" then mqopt$ = " "

            plowkey$ = all(hex(00))
            str(plowkey$,1%,5%) = wc$ & " "
            call "READ101" (#29, plowkey$, f1%(29%))
            put #29 using L19690, wc$, " ", mq, mqopt$, handfactor,       ~
                           su, run, yield, sucode$, rucode$, rudescr$,   ~
                           comp, " "
            if f1%(29%) = 0% then write #29 else rewrite #29
L19690:     FMT CH(4), CH(1), BI(4), CH(1), PD(14,4), BI(4), PD(14,4),   ~
                BI(4), 2*CH(4), CH(30), BI(4), CH(24)

L19740:     goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  message$ = " "
                  enabled% = 1%
                  on fieldnr% gosub L20250,         /* WORK CENTER CODE */~
                                    L20290,         /* WC DESCR         */~
                                    L20330,         /* PM FREQ          */~
                                    L20360,         /* LAST PM DATE     */~
                                    L20390,         /* UNITS IN A DAY   */~
                                    L20432,         /* GROUP            */~
                                    L20440,         /* MONDAY           */~
                                    L20460,         /* TUESDAY          */~
                                    L20490,         /* WEDNESDAY        */~
                                    L20520,         /* THURSDAY         */~
                                    L20550,         /* FRIDAY           */~
                                    L20580,         /* SATURDAY         */~
                                    L20610          /* SUNDAY           */
                     return
L20250:     REM DEFAULT/ENABLE FOR WORK CENTER CODE
                message$ = "Leave Blank & Press (RETURN) To Search For Ex~
        ~isting Work Center Code."
                return
L20290:     REM DEFAULT/ENABLE FOR WORK CENTER DESCRIPTION
                message$ = "Enter A Brief Description Of Center To Help Y~
        ~ou Identify It In The Future."
                return
L20330:     REM DEFAULT/ENABLE FOR PM FREQUENCY IN DAYS
                message$ = "Enter The Number Of Days Between PMs, If Any"
                return
L20360:     REM DEFAULT/ENABLE FOR LAST PM DATE
                message$ = "Enter The Date Of The Last PM"
                return
L20390:     REM DEFAULT/ENABLE FOR UNITS IN 24 HR. PERIOD
                if wconfile% = 0% then L20400
                   enabled% = 0%
                   return
L20400:         message$ = "Capacity and usage are expressed in units. Ho~
        ~w many units are there in 24 Hrs?"
                if units$ = " " then units$ = "24"
                if wconfile% <> 0% then enabled% = 0%
                return

L20432:     REM DEFAULT/ENABLE FOR GROUP
                message$ = "Enter the Group to which this WC belongs"
                return

L20440:     REM DEFAULT/ENABLE FOR NORM UNITS WORKED ON MONDAYS
                return
L20460:     REM DEFAULT/ENABLE FOR NORM UNITS WORKED ON TUE'S
                if nuw$(2%) = " " then nuw$(2%) = nuw$(1%)
                return
L20490:     REM DEFAULT/ENABLE FOR NORM UNITS WORKED ON WED'S
                if nuw$(3%) = " " then nuw$(3%) = nuw$(2%)
                return
L20520:     REM DEFAULT/ENABLE FOR NORM UNITS WORKED ON THURS'S
                if nuw$(4%) = " " then nuw$(4%) = nuw$(3%)
                return
L20550:     REM DEFAULT/ENABLE FOR NORM UNITS WORKED ON FRI'S
                if nuw$(5%) = " " then nuw$(5%) = nuw$(4%)
                return
L20580:     REM DEFAULT/ENABLE FOR NORM UNITS WORKED ON SAT'S
                if nuw$(6%) = " " then nuw$(6%) = "0"
                return
L20610:     REM DEFAULT/ENABLE FOR NORM UNITS WORKED ON SUN'S
                if nuw$(7%) = " " then nuw$(7%) = "0"
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *                                                           *~
            * Sets Defaults and Enables for page 2 of Input.            *~
            *************************************************************~

            deffn'052(fieldnr%)
                message$ = " "
                enabled% = 1%
                on fieldnr% gosub L22200,         /* Move/Queue Time    */~
                                  L22300,         /* Setup Time         */~
                                  L22400,         /* Run Time           */~
                                  L22500,         /* Exp Proc Yield     */~
                                  L22600,         /* S.U. Activity Code */~
                                  L22700,         /* Run Activity Code  */~
                                  L22800,         /* Run Activity Descr */~
                                  L22900,         /* % Comp at this step*/~
                                  L23000          /* Handling Factor    */
                return

L22200:     REM DEFAULT/ENABLE FOR MOVE/QUEUE       MQ$
                message$ = "Enter Move/Queue Days.  Put 'Y' in Box if"  &~
                           " M/Q is After Step Rather than Before."
                if mq$ = " " then mq$ = "0"
                call "STRING" addr("LJ", mq$, 5%)
                if mqopt$ = " " then mqopt$ = "N"
                return

L22300:     REM DEFAULT/ENABLE FOR SETUP UNITS      SU$
                message$ = "Enter in Hours or *whole* WC Units.  Enter" &~
                           " Hours as HH.DDDD, HH:MM or HH:MM:SS."
                call "STRING" addr("LJ", su$, 6%)
                call "STRING" addr("LJ", suh$, 8%)
                savesuh$ = suh$
                return

L22400:     REM DEFAULT/ENABLE FOR RUN UNITS        RUN$
                message$ = "Enter in Hours (HH.DDDD, HH:MM or HH:MM:SS)"&~
                           ", WC Units (U.UUUU), or Parts/Hour."
                call "STRING" addr("LJ", run$, 6%)
                call "STRING" addr("LJ", runh$, 8%)
                call "STRING" addr("LJ", runp$, 8%)
                saverun$ = run$
                saverunh$ = runh$
                return

L22500:     REM DEFAULT/ENABLE FOR EXPECTED PROCESS YIELD THIS STEP
                message$ = "Expected % Yield This Step (e.g. 100, 92.3)"
                if yield$ = " " then yield$ = "100"
                call "STRING" addr("LJ", yield$, 3%)
                return

L22600:     REM DEFAULT/ENABLE FOR SETUP ACTIVITY CODE
                message$ = "Enter Setup Activity Code."
                return

L22700:     REM DEFAULT/ENABLE FOR RUN ACTIVITY CODE
                message$ = "Enter Run Activity Code."
                return

L22800:     REM DEFAULT/ENABLE FOR ACTIVITY DESCRIPTION
                if run$ <> " " then L22840
                    tempkey$ = "WC ACTVTY" & run$
                    call "GETCODE"(#2, tempkey$, rudescr$,0%,99,f1%(2%))
L22840:             message$ = "Enter Description Of Activity To Be Per"&~
                               "formed."
                    return

L22900:     REM DEFAULT/ENABLE FOR % COMPLETE AT THIS STEP
                message$ = "% Product is Complete At This Step (Memo)."
                call "STRING" addr("LJ", comp$, 3%)
                return

L23000:     REM DEFAULT/ENABLE FOR HANDLING FACTOR
                message$ = "Enter Number Of Movement Units Per One Un"  &~
                           "it Of The Part Being Built In The Job."
                if handfactor$ = " " then handfactor$ = "1.00"
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
               startover% = 2%
               call "STARTOVR" (startover%)
                  if startover% <> 0% then return

               REM START OVER            (ENTER)
                   return clear
                   goto inputmode

        REM *************************************************************~
           *         L O A D   W C   I N F O R M A T I O N              *~
           *                                                            *~
           * LOADS DATA OFF DISK                                        *~
           **************************************************************

        load_wc_info
           call "SHOSTAT" ("Gathering Work Center Data")
           wconfile%, wcbuild% = 0%
           call "REDALT1" (#11, wc$, 0%, f1%(11))
              if f1%(11) <> 0% then L30088
                 errormsg$ = hex(00)
                 return
L30088:    wconfile% = 1%
           get #11, using L30120, wcstat$, wc$, wcdescr$, nuw%(), pmfreq$,~
                                 lastpm$, avail%(), used%(), units, group$

L30120:    FMT  CH(01),                  /* UNUSED                     */~
                CH(04),                  /* WORK CENTER                */~
                XX(01),                  /* RECORD TYPE 'A', 'U'       */~
                CH(30),                  /* WC DESCR                   */~
                7*BI(02),                /* WEEKLY UNITS AVAIL, MON 1st*/~
                CH(03),                  /* PM FREQUENCY IN DAYS       */~
                CH(06),                  /* LAST PM DATE               */~
                490*BI(02),              /* CAPACITY AVAILABLE         */~
                490*BI(02),              /* CAPACITY USED              */~
                BI(2),                   /* UNITS IN 24 HOURS          */~
                CH(3)                    /* GROUP                      */

           for i%=1% to 7%
               convert nuw%(i%) to nuw$(i%), pic(#####)
           next i%
           call "CONVERT" (units, 0.0, units$)
           call "WCUN2HRS" (#11, hex(00000000), units, 0, unitsdescr$)
           if lastpm$ <> " " then call "DATEFMT" (lastpm$)

           if group$ = " " then L30380
               readkey$ = "WCGROUPS " & group$
               call "DESCRIBE" (#02, readkey$, groupdescr$, 1%, f1%(02%))
L30380:    init(hex(00)) wccplowkey$
           init(" ") demand$(), seepart$()
           mat seequant% = zer

           str(wccplowkey$,,4%) =  str(wc$)

L30440:    call "PLOWALTS" (#23, wccplowkey$, 1%, 4%, f1%(23))
                     if f1%(23) <> 1 then L30700
           get #23, using L30480, gdem$, subscr%, su%, run%
L30480:    FMT XX(8), CH(19), BI(2), XX(2), 2*BI(4)
           gquant% = su% + run%
           if demand$(subscr%) <> " " then L30560
                call "READ100" (#25, gdem$, f1%(25))
                if f1%(25) <> 0% then get #25, gpart$ else gpart$ = " "
                demand$(subscr%) = gdem$
                seepart$(subscr%) = gpart$
                seequant%(subscr%) = gquant%
                   goto L30440

L30560:    demand$(subscr%) = "MULTIPLE ACTIVITIES"
           seepart$(subscr%) = "MULTIPLE PARTS"
           seequant%(subscr%) = 0%
           init (hex(ff)) str(wccplowkey$,7%)
                   goto L30440

L30700: REM Load Work Center Defaults (WCDFLTS)...
            if wc$ = "VEND" then goto editmode
            insert$ = " "
            plowkey$ = str(wc$) & " "
            call "READ100" (#29, plowkey$, f1%(29%))
                if f1%(29%) <> 0% then L30750
                    insert$ = "No Defaults Exist.  Press PF(11) to Inse"&~
                              "rt or RETURN to edit."
                    goto editmode

L30750:     get #29 using L30770, mq, mqopt$, handfactor, su, run, yield, ~
                          sucode$, rucode$, rudescr$, comp
L30770:     FMT POS(6), BI(4), CH(1), PD(14,4), BI(4), PD(14,4), BI(4),  ~
                2*CH(4), CH(30), BI(4)

            if mq < 0 then L30810 else call "CONVERT" (mq, 0.0, mq$)
L30810:     if su < 0 then L30840 else call "CONVERT" (su, 0.0, su$)
                call "WCUN2HRS" (#11, wc$, 0, su, " ")
                call "CONVERT" (su, 2.4, suh$)
L30840:     if run < 0 then L30910 else call "CONVERT" (run, 0.4, run$)
                call "WCUN2HRS" (#11, wc$, 0, run, " ")
                call "CONVERT" (run, 4.6, runh$)
                    temp = 0
                    convert runh$ to temp, data goto L30900
                    if temp <> 0 then temp = 1/temp
L30900:             call "CONVERT" (temp, 0.6, runp$)
L30910:     call "CONVERT" (handfactor, 0.0, handfactor$)
            call "CONVERT" (yield, 0.0, yield$)
            if comp < 0 then L30940 else call "CONVERT" (comp, 0.0, comp$)
L30940:     if mqopt$ = " " then mqopt$ = "N"
            tempkey$ = "WC ACTVTY" & sucode$
            call "GETCODE" (#2, tempkey$, sudescr$,  0%, 99, f1%(2%))
            goto editmode

        REM *************************************************************~
           *    L O A D   C A L E N D A R                               *~
           **************************************************************

        loadcal

            call "REDALT0" (#12, "30", 0%, f1%(12))
            if f1%(12) = 1 then L33110
L33072:       ask% = 2%
              msg$(1%) = "Planning Calendar Has Not Been Defined"
              msg$(2%)="Please Correct the Problem and Rerun This Program"
              msg$(3%) = "Press RETURN to EXIT"
              call "ASKUSER" (ask%, hdr$, msg$(1), msg$(2), msg$(3))
              if ask% <> 0% then L33072
                goto L65000
L33110:     get #12, using L33120, mm%()
L33120:     FMT XX(2), 490*BI(4)

            call "REDALT0" (#12, "50", 0%, f1%(12))
                if f1%(12%) = 0% then L33310
                get #12, using L33170, dow$()
L33170:         FMT XX(2), 490*CH(3)

            call "REDALT0" (#12, "20", 0%, f1%(12))
                if f1%(12%) = 0% then L33310
                get #12, using L33220, yy%()
L33220:         FMT XX(2), 490*BI(4)

            call "REDALT0" (#12, "10", 0%, f1%(12))
                if f1%(12%) = 0% then L33310
            get #12, using L33270, str(yymmdd$(),,1470)
L33270:         FMT XX(2), CH(1470)

            call "REDALT0" (#12, "11", 0%, f1%(12))
                if f1%(12%) = 1% then L33330
L33302:       ask% = 2%
              msg$(1%) = "PLANNING CALENDAR IS DAMAGED"
              msg$(2%)="Please Correct the Problem and Rerun This Program"
              msg$(3%)= "Press RETURN to EXIT"
L33310:       call "ASKUSER" (ask%, hdr$, msg$(1), msg$(2), msg$(3))
              if ask% <> 0% then L33302
                goto L65000
L33330:     get #12, using L33340, str(yymmdd$(),1471%,1470%)
L33340:         FMT XX(2), CH(1470)
           return

        REM *************************************************************~
           *    G E N E R A T E   C A P A C I T Y                       *~
           **************************************************************

        re_gen_capacity

            new$() = nuw$()
            start$ = yymmdd$(1%)
            end$   = yymmdd$(490%)
            pipidx_str% = 1%
            pipidx_end% = 490%
            unfstart$   = start$
            unfend$     = end$
            call "DATFMTC" (start$)
            call "DATFMTC" (end$)
L35110:     gosub'119(0%)
                  if key%  =  1% then gosub startover
                  if key%  =  8% then L35280
                  if key%  = 24% then L35280
                  if key%  = 16% then       return
                  if key% <>  0% then       L35110
            fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 09% then L35110

L35190:     gosub'119(fieldnr%)
                  if key%  =  1% then gosub startover
                  if key% <>  0% then L35190
            gosub'159(fieldnr%)
                  if errormsg$ <> " " then L35190
            if fieldnr% = 2% or fieldnr% = 9% then L35110
            fieldnr% = fieldnr% + 1%
            goto L35190

L35280:    for i% = 1% to 7%
               convert new$(i%) to new%(i%), data goto L35310
               goto L35320
L35310:           nuw%(i%) = 0%
L35320:    next i%

           if dow$(1%) = "SUN" then m%, mm% = 7%
           if dow$(1%) = "MON" then m%, mm% = 1%
           if dow$(1%) = "TUE" then m%, mm% = 2%
           if dow$(1%) = "WED" then m%, mm% = 3%
           if dow$(1%) = "THU" then m%, mm% = 4%
           if dow$(1%) = "FRI" then m%, mm% = 5%
           if dow$(1%) = "SAT" then m%, mm% = 6%

           for j% = 1% to 7%
               for i% = j% to 490% step 7%
                   if i% < pipidx_str% or i% > pipidx_end% then L35470
                      if key% <> 24% then L35450
                         avail%(i%) = new%(m%)
                         goto L35470
L35450:            if h$(i%) <> "0" then avail%(i%) = 0% else            ~
                                         avail%(i%) = new%(m%)
L35470:        next i%
               m% = m% + 1%
               if m% > 7% then m% = 1%
           next j%
           wcbuild% = 1%
           return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                screen% = 1%
                str(line2$,1%,61%) = " "
                init(hex(84)) lfac$()
                if fieldnr% < 3% then L40085
                     str(pfdescr$(1%,1%),20%,17%) = "(4)Previous Field"
                     str(pfkeys$(1%),3%,1%) = hex(04)
                     goto L40160
L40085:              str(pfdescr$(1%,1%),20%,17%) = " "
                     str(pfkeys$(1%),3%,1%) = hex(ff)
                goto L40160

            deffn'111(fieldnr%)
                screen% = 2%
                str(line2$,1%,61%) = " "
                if wc$ <> "VEND" then L40122
                    str(pfdescr$(2%,1%),25%,13%) = " "
                    str(pfkeys$(2%),4%,1%) = hex(ff)
                    goto L40130
L40122:         str(pfdescr$(2%,1%),25%,13%) = "(5)Next Page"
                str(pfkeys$(2%),4%,1%) = hex(05)
L40130:         init(hex(84)) lfac$()
                if fieldnr% = 0% then init(hex(86)) lfac$()

L40160:           on fieldnr% gosub L40360,         /* WORK CENTER CODE */~
                                    L40360,         /* WC DESCR         */~
                                    L40390,         /* PM FREQ          */~
                                    L40390,         /* LAST PM DATE     */~
                                    L40390,         /* UNITS IN 24 HRs  */~
                                    L40360,         /* GROUP            */~
                                    L40390,         /* MONDAY           */~
                                    L40390,         /* TUESDAY          */~
                                    L40390,         /* WEDNESDAY        */~
                                    L40390,         /* THURSDAY         */~
                                    L40390,         /* FRIDAY           */~
                                    L40390,         /* SATURDAY         */~
                                    L40390          /* SUNDAY           */
                     goto L40430

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40360:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40390:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40430:     accept                                                       ~
               at (01,02), "Manage Work Centers and Capacity",           ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), delete$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Work Center Code",                           ~
               at (06,30), fac(lfac$( 1%)), wc$                 , ch(04),~
               at (07,02), "Work Center Description",                    ~
               at (07,30), fac(lfac$( 2%)), wcdescr$            , ch(30),~
               at (08,02), "PM Frequency in Days",                       ~
               at (08,30), fac(lfac$( 3%)), pmfreq$             , ch(03),~
               at (09,02), "Last PM Date",                               ~
               at (09,30), fac(lfac$( 4%)), lastpm$             , ch(08),~
               at (10,02), "Units in 24 Hr. Period",                     ~
               at (10,30), fac(lfac$( 5%)), units$              , ch(06),~
               at (10,40), fac(hex(8c)),   unitsdescr$          , ch(30),~
               at (11,02), "Workcenter Group Code",                      ~
               at (11,30), fac(lfac$( 6%)), group$              , ch(03),~
               at (11,40), fac(hex(8c)),   groupdescr$          , ch(32),~
               at (12,02), "Usual Units Available On:",                  ~
               at (13,02), "   Mondays",                                 ~
               at (13,18), fac(lfac$( 7%)), nuw$(1%)            , ch(05),~
               at (14,02), "   Tuesdays",                                ~
               at (14,18), fac(lfac$( 8%)), nuw$(2%)            , ch(05),~
               at (15,02), "   Wednesdays",                              ~
               at (15,18), fac(lfac$( 9%)), nuw$(3%)            , ch(05),~
               at (16,02), "   Thursdays",                               ~
               at (16,18), fac(lfac$(10%)), nuw$(4%)            , ch(05),~
               at (17,02), "   Fridays",                                 ~
               at (17,18), fac(lfac$(11%)), nuw$(5%)            , ch(05),~
               at (18,02), "   Saturdays",                               ~
               at (18,18), fac(lfac$(12%)), nuw$(6%)            , ch(05),~
               at (19,02), "   Sundays",                                 ~
               at (19,18), fac(lfac$(13%)), nuw$(7%)            , ch(05),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfdescr$(screen%,1%) , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(screen%,2%) , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(screen%,3%) , ch(79),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)

               if keyhit% <> 13% then L41010
                  call "MANUAL" ("WCINPUT")
                  goto L40430

L41010:        if keyhit% <> 15% then L41050
                  call "PRNTSCRN"
                  goto L40430

L41050:        if fieldnr% <> 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   2      *~
            * --------------------------------------------------------- *~
            * Input/Edit of Work Center Defaults Screen.                *~
            *************************************************************

L42045:         screen% = 3%
                str(line2$,1%,61%) = "Work Center Defaults"
                insert$ = "Press RETURN to enter defaults, PF16 to Retu"&~
                           "rn without entering defaults."
                str(pfdescr$(3%,3%),64%,13%) = "(16)Return   "
                str(pfkeys$(3%),7%,1%) = hex(10)
                init(hex(84)) lfac$()
                goto L42410

            deffn'201(fieldnr%)
                screen% = 3%
                str(line2$,1%,61%) = "Work Center Defaults"
                insert$ = " "
                init(hex(84)) lfac$()
                str(pfdescr$(3%,1%),25%,17%) = " "
                str(pfkeys$(3%),3%,1%) = hex(ff)
                if fieldnr% < 2% then L42220
                    str(pfdescr$(3%,1%),25%,17%) = "(4)Previous Field"
                    str(pfkeys$(3%),3%,1%) = hex(04)
                    goto L42220

            deffn'211(fieldnr%)
                screen% = 4%
                str(line2$,1%,61%) = "Work Center Defaults"
                str(pfdescr$(4%,2%),25%,17%) = " "
                    str(pfkeys$(4%),4%,1%) = hex(ff)
                str(pfdescr$(4%,1%),25%,17%) = "(4)Previous Page"
                    str(pfkeys$(4%),3%,1%) = hex(04)
                str(pfdescr$(4%,3%),64%,13%) = "(16)SAVE DATA"
                    str(pfkeys$(4%),6%,1%) = hex(10)
                if fieldnr% = 0% then L42184
                    str(pfdescr$(4%,1%),25%,17%) = " "
                    str(pfkeys$(4%),3%,1%) = hex(ff)
                    str(pfdescr$(4%,3%),64%,13%) = " "
                    str(pfkeys$(4%),6%,1%) = hex(ff)
L42184:         if insert$ = " " then L42194
                    str(pfdescr$(4%,2%),25%,17%) = "(11)Insert Mode"
                    str(pfkeys$(4%),4%,1%) = hex(0b)
L42194:         init(hex(84)) lfac$()
                if fieldnr% = 0% then init(hex(86)) lfac$()

L42220:         on fieldnr% gosub L42370,         /* Move/Queue Days    */~
                                  L42370,         /* Setup Time         */~
                                  L42370,         /* Run Time           */~
                                  L42370,         /* Yield Percentage   */~
                                  L42370,         /* Setup Activity Code*/~
                                  L42370,         /* Run Activity Code  */~
                                  L42330,         /* Activity Descrip   */~
                                  L42370,         /* % Complete (memo  )*/~
                                  L42370          /* Handling Factor    */
                goto L42410

L42330:         REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                    lfac$(fieldnr%) = hex(80)
                    return

L42370:         REM SET FAC'S FOR UPPER CASE ONLY INPUT
                    lfac$(fieldnr%) = hex(81)
                    return

L42410:     accept                                                       ~
               at (01,02), "Manage Production Work Center Routings",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), insert$                , ch(79),~
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
               at (22,02), fac(hex(8c)), pfdescr$(screen%,1%)   , ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(screen%,2%)   , ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(screen%,3%)   , ch(79),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)

               if keyhit% <> 13% then L42904
                  call "MANUAL" ("WCINPUT")
                  goto L42410

L42904:        if keyhit% <> 15% then L42912
                  call "PRNTSCRN"
                  goto L42410

L42912:        if fieldnr% <> 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

            rem**********************************************************~
            *        r e s e t   c e n t e r s   c a p a c i t y        *~
            *************************************************************~

            deffn'119(fieldnr%)
                  init(hex(84)) lfac$()
                  if fieldnr% = 0% then L43600
                  if fieldnr% < 3% then gosub L43565 else   /* DATES */   ~
                          gosub L43580              /* MON - SUN        */
                  goto L43600

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L43565:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L43580:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L43600:     accept                                                       ~
               at (01,02),                                               ~
        "RESET W/C CAPACITY:",fac(hex(84)), wc$, fac(hex(84)), wcdescr$, ~
                                                                         ~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Starting From (Default is FIRST Day in Prod. Calendar)",~
               at (06,60), fac(lfac$( 1%)), start$              , ch(10),~
               at (07,02),                                               ~
                  "Ending With (Default is LAST Day in Prod. Calendar)", ~
               at (07,60), fac(lfac$( 2%)), end$                , ch(10),~
               at (08,02),                                               ~
                  "Units Worked on Mondays",                             ~
               at (08,30), fac(lfac$( 3%)), new$(1%)            , ch(05),~
               at (09,02),                                               ~
                  "Units Worked on Tuesdays",                            ~
               at (09,30), fac(lfac$( 4%)), new$(2%)            , ch(05),~
               at (10,02),                                               ~
                  "Units Worked on Wednesdays",                          ~
               at (10,30), fac(lfac$( 5%)), new$(3%)            , ch(05),~
               at (11,02),                                               ~
                  "Units Worked on Thursdays",                           ~
               at (11,30), fac(lfac$( 6%)), new$(4%)            , ch(05),~
               at (12,02),                                               ~
                  "Units Worked on Fridays",                             ~
               at (12,30), fac(lfac$( 7%)), new$(5%)            , ch(05),~
               at (13,02),                                               ~
                  "Units Worked on Saturdays",                           ~
               at (13,30), fac(lfac$( 8%)), new$(6%)            , ch(05),~
               at (14,02),                                               ~
                  "Units Worked on Sundays",                             ~
               at (14,30), fac(lfac$( 9%)), new$(7%)            , ch(05),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (23,16),                                               ~
                      "(8)SET/RESET WC Capacity For Date Range Shown",   ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,02),                                               ~
        "             (24)SET/RESET WC Capacity WITHOUT Holidays        (~
        ~16)Return",                                                      ~
                                                                         ~
               keys(hex(000108180d0f10)),                                ~
               key (key%)

               if key% <> 13% then L43855
                  call "MANUAL" ("WCINPUT")
                  goto L43600

L43855:        if key% <> 15% then L43875
                  call "PRNTSCRN"
                  goto L43600

L43875:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
           *    S E E   C A P A C I T I E S   B Y   M O N T H           *~
           **************************************************************

        see_capacities
            convert str(ccyymmdd$, 1%, 4%) to yr%
            convert str(ccyymmdd$, 5%, 2%) to mo%
            convert str(ccyymmdd$, 7%, 2%) to dd%
            todayyy% = yr%
            todaymm% = mo%

        see_capacities_again
            avail%, f% = 0%
            for i% = 1% to 490%
                if yy%(i%) < yr% then L44085
                if yy%(i%) > yr% then L44095
                if mm%(i%) < mo% then L44085
                if mm%(i%) > mo% then L44095
                if f% =  0% then f% = i%
L44085:     next i%

L44095:     l% = i% - 1%
            counter, counter1, j% = 0%
            init(" ")  dd$(), davail$(), dused$(), dpct$(), note$()
            init(hex(8c)) lfac$()
            for i% = f% to l%
                j% = j% + 1%
                if avail%(i%) = 0% then L44150
                if yy%(i%) = todayyy% and mm%(i%) = todaymm%             ~
                                     and j% < dd% then L44145
                counter1 = counter1 + avail%(i%)
                lfac$(j) = hex(84)
L44145:         convert avail%(i%) to davail$(j%), pic(#####)
L44150:         if used%(i%) = 0% then L44175
                if yy%(i%) = todayyy% and mm%(i%) = todaymm%             ~
                                         and j% < dd% then L44170
                   counter = counter + used%(i%)
                   lfac$(j) = hex(84)
L44170:            convert used%(i%) to dused$(j%), pic(######)
L44175:            if avail%(i%) = 0 then L44190
               put dpct$(j%),using L44185,round(100*used%(i%)/avail%(i%),0)
L44185:            %#####%
L44190:            dd$(j%) = dow$(i%)
                   if h$(i%) <> "0" then note$(j%) = "Holiday"
            next i%
L44199:     effic = 0
            if counter1 = 0 then L44204
                 effic = 100 * counter / counter1
L44204:     effic = round(effic,2) : convert effic to eff$, pic(###.##)
            convert counter to ctr1$, pic(######)
            convert counter1 to ctr2$, pic(######)
            line2x$    = "Usage is" & hex(84) & eff$ & "%" & hex(8c) &   ~
                         ", Used" & hex(84) & ctr1$ & hex(8c) &          ~
                         "Units of" & hex(84) & ctr2$ & hex(8c) &        ~
                         "Units Still Available This Month"
            if (yy%(i%) < todayyy%)  or                                  ~
               (yy%(i%) = todayyy% and mm%(i%) < todaymm%)  then         ~
                         line2x$    = "Work Center has" & hex(84) &      ~
                         ctr1$ & hex(8c) & "Scheduled Units that are " & ~
                         "Unaccounted For"
            if (yy%(i%) < todayyy%)  or                                  ~
               (yy%(i%) = todayyy% and mm%(i%) < todaymm%)  then         ~
               init(hex(8c)) lfac$()

L44280: accept                                                           ~
               at (01,02), "Units Available And Used For Work Center:",  ~
               at (01,43), fac(hex(84)), wc$                    , ch(04),~
               at (01,48), "Month Of:",                                  ~
               at (01,57), fac(hex(84)), modate$(mo%)           , ch(09),~
               at (01,67), fac(hex(84)), yr%                  ,pic(####),~
               at (02,31), fac(hex(84)), wcdescr$               , ch(30),~
               at (03,03), fac(hex(ac)), lits$(2)               , ch(78),~
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
            at(04,10), fac(lfac$(01)), davail$(01%)             , ch(05),~
            at(05,10), fac(lfac$(02)), davail$(02%)             , ch(05),~
            at(06,10), fac(lfac$(03)), davail$(03%)             , ch(05),~
            at(07,10), fac(lfac$(04)), davail$(04%)             , ch(05),~
            at(08,10), fac(lfac$(05)), davail$(05%)             , ch(05),~
            at(09,10), fac(lfac$(06)), davail$(06%)             , ch(05),~
            at(10,10), fac(lfac$(07)), davail$(07%)             , ch(05),~
            at(11,10), fac(lfac$(08)), davail$(08%)             , ch(05),~
            at(12,10), fac(lfac$(09)), davail$(09%)             , ch(05),~
            at(13,10), fac(lfac$(10)), davail$(10%)             , ch(05),~
            at(14,10), fac(lfac$(11)), davail$(11%)             , ch(05),~
            at(15,10), fac(lfac$(12)), davail$(12%)             , ch(05),~
            at(16,10), fac(lfac$(13)), davail$(13%)             , ch(05),~
            at(17,10), fac(lfac$(14)), davail$(14%)             , ch(05),~
            at(18,10), fac(lfac$(15)), davail$(15%)             , ch(05),~
            at(04,51), fac(lfac$(16)), davail$(16%)             , ch(05),~
            at(05,51), fac(lfac$(17)), davail$(17%)             , ch(05),~
            at(06,51), fac(lfac$(18)), davail$(18%)             , ch(05),~
            at(07,51), fac(lfac$(19)), davail$(19%)             , ch(05),~
            at(08,51), fac(lfac$(20)), davail$(20%)             , ch(05),~
            at(09,51), fac(lfac$(21)), davail$(21%)             , ch(05),~
            at(10,51), fac(lfac$(22)), davail$(22%)             , ch(05),~
            at(11,51), fac(lfac$(23)), davail$(23%)             , ch(05),~
            at(12,51), fac(lfac$(24)), davail$(24%)             , ch(05),~
            at(13,51), fac(lfac$(25)), davail$(25%)             , ch(05),~
            at(14,51), fac(lfac$(26)), davail$(26%)             , ch(05),~
            at(15,51), fac(lfac$(27)), davail$(27%)             , ch(05),~
            at(16,51), fac(lfac$(28)), davail$(28%)             , ch(05),~
            at(17,51), fac(lfac$(29)), davail$(29%)             , ch(05),~
            at(18,51), fac(lfac$(30)), davail$(30%)             , ch(05),~
            at(19,51), fac(lfac$(31)), davail$(31%)             , ch(05),~
                                                                         ~
            at(04,17), fac(hex(84)), dused$(01%)                , ch(06),~
            at(05,17), fac(hex(84)), dused$(02%)                , ch(06),~
            at(06,17), fac(hex(84)), dused$(03%)                , ch(06),~
            at(07,17), fac(hex(84)), dused$(04%)                , ch(06),~
            at(08,17), fac(hex(84)), dused$(05%)                , ch(06),~
            at(09,17), fac(hex(84)), dused$(06%)                , ch(06),~
            at(10,17), fac(hex(84)), dused$(07%)                , ch(06),~
            at(11,17), fac(hex(84)), dused$(08%)                , ch(06),~
            at(12,17), fac(hex(84)), dused$(09%)                , ch(06),~
            at(13,17), fac(hex(84)), dused$(10%)                , ch(06),~
            at(14,17), fac(hex(84)), dused$(11%)                , ch(06),~
            at(15,17), fac(hex(84)), dused$(12%)                , ch(06),~
            at(16,17), fac(hex(84)), dused$(13%)                , ch(06),~
            at(17,17), fac(hex(84)), dused$(14%)                , ch(06),~
            at(18,17), fac(hex(84)), dused$(15%)                , ch(06),~
            at(04,58), fac(hex(84)), dused$(16%)                , ch(06),~
            at(05,58), fac(hex(84)), dused$(17%)                , ch(06),~
            at(06,58), fac(hex(84)), dused$(18%)                , ch(06),~
            at(07,58), fac(hex(84)), dused$(19%)                , ch(06),~
            at(08,58), fac(hex(84)), dused$(20%)                , ch(06),~
            at(09,58), fac(hex(84)), dused$(21%)                , ch(06),~
            at(10,58), fac(hex(84)), dused$(22%)                , ch(06),~
            at(11,58), fac(hex(84)), dused$(23%)                , ch(06),~
            at(12,58), fac(hex(84)), dused$(24%)                , ch(06),~
            at(13,58), fac(hex(84)), dused$(25%)                , ch(06),~
            at(14,58), fac(hex(84)), dused$(26%)                , ch(06),~
            at(15,58), fac(hex(84)), dused$(27%)                , ch(06),~
            at(16,58), fac(hex(84)), dused$(28%)                , ch(06),~
            at(17,58), fac(hex(84)), dused$(29%)                , ch(06),~
            at(18,58), fac(hex(84)), dused$(30%)                , ch(06),~
            at(19,58), fac(hex(84)), dused$(31%)                , ch(06),~
                                                                         ~
            at(04,24), fac(hex(8c)), dpct$(01%)                 , ch(06),~
            at(05,24), fac(hex(8c)), dpct$(02%)                 , ch(06),~
            at(06,24), fac(hex(8c)), dpct$(03%)                 , ch(06),~
            at(07,24), fac(hex(8c)), dpct$(04%)                 , ch(06),~
            at(08,24), fac(hex(8c)), dpct$(05%)                 , ch(06),~
            at(09,24), fac(hex(8c)), dpct$(06%)                 , ch(06),~
            at(10,24), fac(hex(8c)), dpct$(07%)                 , ch(06),~
            at(11,24), fac(hex(8c)), dpct$(08%)                 , ch(06),~
            at(12,24), fac(hex(8c)), dpct$(09%)                 , ch(06),~
            at(13,24), fac(hex(8c)), dpct$(10%)                 , ch(06),~
            at(14,24), fac(hex(8c)), dpct$(11%)                 , ch(06),~
            at(15,24), fac(hex(8c)), dpct$(12%)                 , ch(06),~
            at(16,24), fac(hex(8c)), dpct$(13%)                 , ch(06),~
            at(17,24), fac(hex(8c)), dpct$(14%)                 , ch(06),~
            at(18,24), fac(hex(8c)), dpct$(15%)                 , ch(06),~
            at(04,65), fac(hex(8c)), dpct$(16%)                 , ch(06),~
            at(05,65), fac(hex(8c)), dpct$(17%)                 , ch(06),~
            at(06,65), fac(hex(8c)), dpct$(18%)                 , ch(06),~
            at(07,65), fac(hex(8c)), dpct$(19%)                 , ch(06),~
            at(08,65), fac(hex(8c)), dpct$(20%)                 , ch(06),~
            at(09,65), fac(hex(8c)), dpct$(21%)                 , ch(06),~
            at(10,65), fac(hex(8c)), dpct$(22%)                 , ch(06),~
            at(11,65), fac(hex(8c)), dpct$(23%)                 , ch(06),~
            at(12,65), fac(hex(8c)), dpct$(24%)                 , ch(06),~
            at(13,65), fac(hex(8c)), dpct$(25%)                 , ch(06),~
            at(14,65), fac(hex(8c)), dpct$(26%)                 , ch(06),~
            at(15,65), fac(hex(8c)), dpct$(27%)                 , ch(06),~
            at(16,65), fac(hex(8c)), dpct$(28%)                 , ch(06),~
            at(17,65), fac(hex(8c)), dpct$(29%)                 , ch(06),~
            at(18,65), fac(hex(8c)), dpct$(30%)                 , ch(06),~
            at(19,65), fac(hex(8c)), dpct$(31%)                 , ch(06),~
                                                                         ~
            at(04,06), fac(hex(8c)), dd$(01%)                    , ch(3),~
            at(05,06), fac(hex(8c)), dd$(02%)                    , ch(3),~
            at(06,06), fac(hex(8c)), dd$(03%)                    , ch(3),~
            at(07,06), fac(hex(8c)), dd$(04%)                    , ch(3),~
            at(08,06), fac(hex(8c)), dd$(05%)                    , ch(3),~
            at(09,06), fac(hex(8c)), dd$(06%)                    , ch(3),~
            at(10,06), fac(hex(8c)), dd$(07%)                    , ch(3),~
            at(11,06), fac(hex(8c)), dd$(08%)                    , ch(3),~
            at(12,06), fac(hex(8c)), dd$(09%)                    , ch(3),~
            at(13,06), fac(hex(8c)), dd$(10%)                    , ch(3),~
            at(14,06), fac(hex(8c)), dd$(11%)                    , ch(3),~
            at(15,06), fac(hex(8c)), dd$(12%)                    , ch(3),~
            at(16,06), fac(hex(8c)), dd$(13%)                    , ch(3),~
            at(17,06), fac(hex(8c)), dd$(14%)                    , ch(3),~
            at(18,06), fac(hex(8c)), dd$(15%)                    , ch(3),~
            at(04,47), fac(hex(8c)), dd$(16%)                    , ch(3),~
            at(05,47), fac(hex(8c)), dd$(17%)                    , ch(3),~
            at(06,47), fac(hex(8c)), dd$(18%)                    , ch(3),~
            at(07,47), fac(hex(8c)), dd$(19%)                    , ch(3),~
            at(08,47), fac(hex(8c)), dd$(20%)                    , ch(3),~
            at(09,47), fac(hex(8c)), dd$(21%)                    , ch(3),~
            at(10,47), fac(hex(8c)), dd$(22%)                    , ch(3),~
            at(11,47), fac(hex(8c)), dd$(23%)                    , ch(3),~
            at(12,47), fac(hex(8c)), dd$(24%)                    , ch(3),~
            at(13,47), fac(hex(8c)), dd$(25%)                    , ch(3),~
            at(14,47), fac(hex(8c)), dd$(26%)                    , ch(3),~
            at(15,47), fac(hex(8c)), dd$(27%)                    , ch(3),~
            at(16,47), fac(hex(8c)), dd$(28%)                    , ch(3),~
            at(17,47), fac(hex(8c)), dd$(29%)                    , ch(3),~
            at(18,47), fac(hex(8c)), dd$(30%)                    , ch(3),~
            at(19,47), fac(hex(8c)), dd$(31%)                    , ch(3),~
                                                                         ~
            at(04,31), fac(hex(84)), note$(01%)                 , ch(07),~
            at(05,31), fac(hex(84)), note$(02%)                 , ch(07),~
            at(06,31), fac(hex(84)), note$(03%)                 , ch(07),~
            at(07,31), fac(hex(84)), note$(04%)                 , ch(07),~
            at(08,31), fac(hex(84)), note$(05%)                 , ch(07),~
            at(09,31), fac(hex(84)), note$(06%)                 , ch(07),~
            at(10,31), fac(hex(84)), note$(07%)                 , ch(07),~
            at(11,31), fac(hex(84)), note$(08%)                 , ch(07),~
            at(12,31), fac(hex(84)), note$(09%)                 , ch(07),~
            at(13,31), fac(hex(84)), note$(10%)                 , ch(07),~
            at(14,31), fac(hex(84)), note$(11%)                 , ch(07),~
            at(15,31), fac(hex(84)), note$(12%)                 , ch(07),~
            at(16,31), fac(hex(84)), note$(13%)                 , ch(07),~
            at(17,31), fac(hex(84)), note$(14%)                 , ch(07),~
            at(18,31), fac(hex(84)), note$(15%)                 , ch(07),~
            at(04,72), fac(hex(84)), note$(16%)                 , ch(07),~
            at(05,72), fac(hex(84)), note$(17%)                 , ch(07),~
            at(06,72), fac(hex(84)), note$(18%)                 , ch(07),~
            at(07,72), fac(hex(84)), note$(19%)                 , ch(07),~
            at(08,72), fac(hex(84)), note$(20%)                 , ch(07),~
            at(09,72), fac(hex(84)), note$(21%)                 , ch(07),~
            at(10,72), fac(hex(84)), note$(22%)                 , ch(07),~
            at(11,72), fac(hex(84)), note$(23%)                 , ch(07),~
            at(12,72), fac(hex(84)), note$(24%)                 , ch(07),~
            at(13,72), fac(hex(84)), note$(25%)                 , ch(07),~
            at(14,72), fac(hex(84)), note$(26%)                 , ch(07),~
            at(15,72), fac(hex(84)), note$(27%)                 , ch(07),~
            at(16,72), fac(hex(84)), note$(28%)                 , ch(07),~
            at(17,72), fac(hex(84)), note$(29%)                 , ch(07),~
            at(18,72), fac(hex(84)), note$(30%)                 , ch(07),~
            at(19,72), fac(hex(84)), note$(31%)                 , ch(07),~
                                                                         ~
            at (20,02), fac(hex(8c)), line2x$                   , ch(79),~
                                                                         ~
            at (21,02), fac(hex(ac)), lits$(5)                  , ch(79),~
                                                                         ~
               at(22,02), "(1)Start Over      (3)Current Month    (7)See ~
        ~Jobs in WC",                                                     ~
               at(23,02), "(2)RESET Capacity  (4)Prev Month       (8)See ~
        ~Parts in WC",                                                    ~
               at(24,02), "                   (5)Next Month       (9)Head~
        ~er",                                                             ~
               at(23,64), "(15)Print Screen",                            ~
               at(24,64), "(16)Save Data",                               ~
                keys(hex(0001020304050708090f10)),                       ~
                key(keyhit%)

            if keyhit% =  1% then gosub startover
               if avail% > 0% then test_edit
            if keyhit% =  16% then datasave
            if keyhit% =   3% then see_capacities
            if keyhit% =   2% then gosub re_gen_capacity
            if keyhit% =   2% then see_capacities
            if keyhit% =   7% then demands
            if keyhit% =   8% then parts
            if keyhit% =   9% then editmode
            if keyhit% <>  0% then L45370
                sfac$() = lfac$()
                lfac$() = all (hex(82))
                avail% = 99%
                for i% = 2% to 31%
                    if dd$(i%) = " " then lfac$(i%) = hex(8c)
                next i%
                goto L44280

L45370:     if keyhit% <> 4% then L45395
                if f% = 1% then L44280
                mo% = mm%(f%-1%)
                yr% = yy%(f%-1%)
                goto see_capacities_again
L45395:     if keyhit% <> 5% then L45420
                if l% = 490% then L44280
                mo% = mm%(l%+1%)
                yr% = yy%(l%+1%)
                goto see_capacities_again
L45420:     if keyhit% <> 15% then L44280
                call "PRNTSCRN"
                goto L44280

        test_edit
            for i% = 1% to 31%
              avail% = f% + i% - 1%
              if avail% > 490% then L45530
              if dd$(i%) = " " then L45530
              if davail$(i%) = " " then davail$(i%) = "0"
              convert davail$(i%) to temp%, data goto L44280
              if temp% < 0% then L44280
              if temp% > 65000% then L44280
              if yy%(i%) = todayyy% and mm%(i%) = todaymm%               ~
                                   and i% < dd% then L45490
                  counter1 = counter1 - avail%(avail%)
L45490:       avail%(avail%) = temp%
              convert avail%(avail%) to davail$(i%), pic(#####)
              if davail$(i%) = "    0" then davail$(i%) = " "
              if yy%(i%) = todayyy% and mm%(i%) = todaymm%               ~
                                   and i% < dd% then L45510
                  counter1 = counter1 + avail%(avail%)
L45510:       if avail%(avail%) = 0% then L45530
              put dpct$(i%),using L45520, 100*used%(avail%)/avail%(avail%)
L45520:       %#####%
L45530:     next i%
            lfac$() = sfac$()
            avail% = 0%
            goto L44199

        REM *************************************************************~
           *    S E E   D E M A N D S         B Y   M O N T H           *~
           **************************************************************

        seedemands
            convert str(ccyymmdd$, 1%, 4%) to yr%
            convert str(ccyymmdd$, 5%, 2%) to mo%
        demands
L46040:     f% = 0%
            for i = 1 to 490
                if yy%(i) < yr% then L46075
                if yy%(i) > yr% then L46080
                if mm%(i) < mo% then L46075
                if mm%(i) > mo% then L46080
                if f% =  0 then f% = i
L46075:     next i
L46080:     l% = i - 1

            j = 0  : counter = 0 : counter1 = 0
            init(" ")  dd$(), davail$(), shown$()

            for i = f% to l%
                j = j + 1
                if avail%(i) = 0 then L46130
                   counter1 = counter1 + 1
                   convert avail%(i) to davail$(j), pic(#####)
L46130:         shown$(j) = demand$(i)
                if shown$(j) <> " " then counter = counter + 1
                dd$(j) = dow$(i)
                if h$(i) <> "0" then str(shown$(j),17,7) = "Holiday"
           next i
           if counter1 = 0 then L46165
              effic = 100 *   counter / counter1
L46165:    init(hex(86)) lfac$()

L46175: accept                                                           ~
               at (01,02), "Jobs & Demands Now Scheduled In Work Ctr:",  ~
               at (01,43), fac(hex(84)), wc$                    , ch(04),~
               at (01,48), "Month of:",                                  ~
               at (01,57), fac(hex(84)), modate$(mo%)           , ch(09),~
               at (01,67), fac(hex(84)), yr%                  ,pic(####),~
               at (02,31), fac(hex(84)), wcdescr$               , ch(30),~
               at (03,03), fac(hex(ac)), lits$(3)               , ch(78),~
               at (04,03), "01", at(04,41), "!  16",                     ~
               at (05,03), "02", at(05,41), "!  17",                     ~
               at (06,03), "03", at(06,41), "!  18",                     ~
               at (07,03), "04", at(07,41), "!  19",                     ~
               at (08,03), "05", at(08,41), "!  20",                     ~
               at (09,03), "06", at(09,41), "!  21",                     ~
               at (10,03), "07", at(10,41), "!  22",                     ~
               at (11,03), "08", at(11,41), "!  23",                     ~
               at (12,03), "09", at(12,41), "!  24",                     ~
               at (13,03), "10", at(13,41), "!  25",                     ~
               at (14,03), "11", at(14,41), "!  26",                     ~
               at (15,03), "12", at(15,41), "!  27",                     ~
               at (16,03), "13", at(16,41), "!  28",                     ~
               at (17,03), "14", at(17,41), "!  29",                     ~
               at (18,03), "15", at(18,41), "!  30",                     ~
                                 at(19,41), "!  31",                     ~
                                                                         ~
            at(04,10), fac(lfac$(01)), davail$(01)               ,ch(05),~
            at(05,10), fac(lfac$(02)), davail$(02)               ,ch(05),~
            at(06,10), fac(lfac$(03)), davail$(03)               ,ch(05),~
            at(07,10), fac(lfac$(04)), davail$(04)               ,ch(05),~
            at(08,10), fac(lfac$(05)), davail$(05)               ,ch(05),~
            at(09,10), fac(lfac$(06)), davail$(06)               ,ch(05),~
            at(10,10), fac(lfac$(07)), davail$(07)               ,ch(05),~
            at(11,10), fac(lfac$(08)), davail$(08)               ,ch(05),~
            at(12,10), fac(lfac$(09)), davail$(09)               ,ch(05),~
            at(13,10), fac(lfac$(10)), davail$(10)               ,ch(05),~
            at(14,10), fac(lfac$(11)), davail$(11)               ,ch(05),~
            at(15,10), fac(lfac$(12)), davail$(12)               ,ch(05),~
            at(16,10), fac(lfac$(13)), davail$(13)               ,ch(05),~
            at(17,10), fac(lfac$(14)), davail$(14)               ,ch(05),~
            at(18,10), fac(lfac$(15)), davail$(15)               ,ch(05),~
            at(04,51), fac(lfac$(16)), davail$(16)               ,ch(05),~
            at(05,51), fac(lfac$(17)), davail$(17)               ,ch(05),~
            at(06,51), fac(lfac$(18)), davail$(18)               ,ch(05),~
            at(07,51), fac(lfac$(19)), davail$(19)               ,ch(05),~
            at(08,51), fac(lfac$(20)), davail$(20)               ,ch(05),~
            at(09,51), fac(lfac$(21)), davail$(21)               ,ch(05),~
            at(10,51), fac(lfac$(22)), davail$(22)               ,ch(05),~
            at(11,51), fac(lfac$(23)), davail$(23)               ,ch(05),~
            at(12,51), fac(lfac$(24)), davail$(24)               ,ch(05),~
            at(13,51), fac(lfac$(25)), davail$(25)               ,ch(05),~
            at(14,51), fac(lfac$(26)), davail$(26)               ,ch(05),~
            at(15,51), fac(lfac$(27)), davail$(27)               ,ch(05),~
            at(16,51), fac(lfac$(28)), davail$(28)               ,ch(05),~
            at(17,51), fac(lfac$(29)), davail$(29)               ,ch(05),~
            at(18,51), fac(lfac$(30)), davail$(30)               ,ch(05),~
            at(19,51), fac(lfac$(31)), davail$(31)               ,ch(05),~
                                                                         ~
            at(04,17), fac(hex(84)), shown$(01)                  ,ch(23),~
            at(05,17), fac(hex(84)), shown$(02)                  ,ch(23),~
            at(06,17), fac(hex(84)), shown$(03)                  ,ch(23),~
            at(07,17), fac(hex(84)), shown$(04)                  ,ch(23),~
            at(08,17), fac(hex(84)), shown$(05)                  ,ch(23),~
            at(09,17), fac(hex(84)), shown$(06)                  ,ch(23),~
            at(10,17), fac(hex(84)), shown$(07)                  ,ch(23),~
            at(11,17), fac(hex(84)), shown$(08)                  ,ch(23),~
            at(12,17), fac(hex(84)), shown$(09)                  ,ch(23),~
            at(13,17), fac(hex(84)), shown$(10)                  ,ch(23),~
            at(14,17), fac(hex(84)), shown$(11)                  ,ch(23),~
            at(15,17), fac(hex(84)), shown$(12)                  ,ch(23),~
            at(16,17), fac(hex(84)), shown$(13)                  ,ch(23),~
            at(17,17), fac(hex(84)), shown$(14)                  ,ch(23),~
            at(18,17), fac(hex(84)), shown$(15)                  ,ch(23),~
            at(04,58), fac(hex(84)), shown$(16)                  ,ch(23),~
            at(05,58), fac(hex(84)), shown$(17)                  ,ch(23),~
            at(06,58), fac(hex(84)), shown$(18)                  ,ch(23),~
            at(07,58), fac(hex(84)), shown$(19)                  ,ch(23),~
            at(08,58), fac(hex(84)), shown$(20)                  ,ch(23),~
            at(09,58), fac(hex(84)), shown$(21)                  ,ch(23),~
            at(10,58), fac(hex(84)), shown$(22)                  ,ch(23),~
            at(11,58), fac(hex(84)), shown$(23)                  ,ch(23),~
            at(12,58), fac(hex(84)), shown$(24)                  ,ch(23),~
            at(13,58), fac(hex(84)), shown$(25)                  ,ch(23),~
            at(14,58), fac(hex(84)), shown$(26)                  ,ch(23),~
            at(15,58), fac(hex(84)), shown$(27)                  ,ch(23),~
            at(16,58), fac(hex(84)), shown$(28)                  ,ch(23),~
            at(17,58), fac(hex(84)), shown$(29)                  ,ch(23),~
            at(18,58), fac(hex(84)), shown$(30)                  ,ch(23),~
            at(19,58), fac(hex(84)), shown$(31)                  ,ch(23),~
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
               at (21,02), "WORK CENTER DAYS USED:",                     ~
               at (21,27), fac(hex(84)), effic , pic(###.##),            ~
               at(21,34), "%   (Used",                                   ~
               at(21,45), fac(hex(84)), counter, pic(##),                ~
               at(21,48), " Days Out Of",                                ~
               at(21,62), fac(hex(84)), counter1, pic(##),               ~
               at(21,65), "Prod Days Avail)",                            ~
               at(22,02), fac(hex(ac)), lits$(1),                 ch(79),~
               at (23,02), "(3)Current  (4)Prev Month",                  ~
               at (23,64), "(15)Print Screen",                           ~
               at (24,02), "            (5)Next Month    (8)Parts in WC",~
               at (24,64), "(16)RETURN",                                 ~
                                                                         ~
                keys(hex(00030405080f10)),                               ~
                key(keyhit%)

            if keyhit% = 16 then see_capacities_again
            if keyhit% = 3 then seedemands
            if keyhit% = 8 then parts

            if keyhit% <> 4 then L46950
                if f% = 1 then L46175
                mo% = mm%(f%-1)
                yr% = yy%(f%-1)
                goto L46040
L46950:     if keyhit% <> 5 then L46975
                if l% = 490 then L46175
                mo% = mm%(l%+1)
                yr% = yy%(l%+1)
                goto L46040
L46975:     if keyhit% <> 15 then L46995
                call "PRNTSCRN"
                goto L46175

L46995:     if keyhit% <> 0 then L46175
                mat cursor% = zer : dem$, type$ = " " : r% = 0%
                gosub get_cursor_pos
                if edit% = 0 then L46175

            if str(shown$(edit%),,19) = "MULTIPLE ACTIVITIES" then L46175
            if str(shown$(edit%),,19) = " " then L46175
            call "GETDEM" (1%, str(shown$(edit%),,19), #26, #27, #28,    ~
                                                         dem$, type$, r%)
            goto L46175

        REM *************************************************************~
           *    S E E   P A R T S   &   Q T Y    B Y   M O N T H        *~
           **************************************************************

        seeparts
            convert str(ccyymmdd$, 1%, 4%) to yr%
            convert str(ccyymmdd$, 5%, 2%) to mo%
        parts
L48080:     f% = 0%
            for i = 1 to 490
            if yy%(i) < yr% then L48150
            if yy%(i) > yr% then L48160
            if mm%(i) < mo% then L48150
            if mm%(i) > mo% then L48160
            if f% =  0 then f% = i
L48150:         next i
L48160:     l% = i - 1

                j = 0
            init(" ")  dd$(), davail$(), shown$()

            for i = f% to l%
                j = j + 1
            if seequant%(i) = 0% then L48250
            convert seequant%(i) to davail$(j), pic(#####)
L48250:     shown$(j) = str(seepart$(i),,23)
            dd$(j) = dow$(i)
            if h$(i) <> "0" then str(shown$(j),17,7) = "Holiday"
                next i
           init(hex(86)) lfac$()

L48310: accept                                                           ~
               at (01,02), "Parts Presently Scheduled Into Work Ctr:",   ~
               at (01,43), fac(hex(84)), wc$                    , ch(04),~
               at (01,48), "Month Of:",                                  ~
               at (01,57), fac(hex(84)), modate$(mo%)           , ch(09),~
               at (01,67), fac(hex(84)), yr%                  ,pic(####),~
               at (02,31), fac(hex(84)), wcdescr$               , ch(30),~
               at (03,03), fac(hex(ac)), lits$(4)               , ch(78),~
               at (04,03), "01", at(04,41), "!  16",                     ~
               at (05,03), "02", at(05,41), "!  17",                     ~
               at (06,03), "03", at(06,41), "!  18",                     ~
               at (07,03), "04", at(07,41), "!  19",                     ~
               at (08,03), "05", at(08,41), "!  20",                     ~
               at (09,03), "06", at(09,41), "!  21",                     ~
               at (10,03), "07", at(10,41), "!  22",                     ~
               at (11,03), "08", at(11,41), "!  23",                     ~
               at (12,03), "09", at(12,41), "!  24",                     ~
               at (13,03), "10", at(13,41), "!  25",                     ~
               at (14,03), "11", at(14,41), "!  26",                     ~
               at (15,03), "12", at(15,41), "!  27",                     ~
               at (16,03), "13", at(16,41), "!  28",                     ~
               at (17,03), "14", at(17,41), "!  29",                     ~
               at (18,03), "15", at(18,41), "!  30",                     ~
                                 at(19,41), "!  31",                     ~
               at (22,02), fac(hex(ac)), blankline$,              ch(79),~
               at (23,02), "(3)Current  (4)Prev Month",                  ~
               at (23,64), "(15)Print Screen",                           ~
               at (24,02), "            (5)Next Month    (7)JOBS in WC", ~
               at (24,64), "(16)RETURN",                                 ~
                                                                         ~
           at(04,10), fac(lfac$(01)), davail$(01)               , ch(05),~
           at(05,10), fac(lfac$(02)), davail$(02)               , ch(05),~
           at(06,10), fac(lfac$(03)), davail$(03)               , ch(05),~
           at(07,10), fac(lfac$(04)), davail$(04)               , ch(05),~
           at(08,10), fac(lfac$(05)), davail$(05)               , ch(05),~
           at(09,10), fac(lfac$(06)), davail$(06)               , ch(05),~
           at(10,10), fac(lfac$(07)), davail$(07)               , ch(05),~
           at(11,10), fac(lfac$(08)), davail$(08)               , ch(05),~
           at(12,10), fac(lfac$(09)), davail$(09)               , ch(05),~
           at(13,10), fac(lfac$(10)), davail$(10)               , ch(05),~
           at(14,10), fac(lfac$(11)), davail$(11)               , ch(05),~
           at(15,10), fac(lfac$(12)), davail$(12)               , ch(05),~
           at(16,10), fac(lfac$(13)), davail$(13)               , ch(05),~
           at(17,10), fac(lfac$(14)), davail$(14)               , ch(05),~
           at(18,10), fac(lfac$(15)), davail$(15)               , ch(05),~
           at(04,51), fac(lfac$(16)), davail$(16)               , ch(05),~
           at(05,51), fac(lfac$(17)), davail$(17)               , ch(05),~
           at(06,51), fac(lfac$(18)), davail$(18)               , ch(05),~
           at(07,51), fac(lfac$(19)), davail$(19)               , ch(05),~
           at(08,51), fac(lfac$(20)), davail$(20)               , ch(05),~
           at(09,51), fac(lfac$(21)), davail$(21)               , ch(05),~
           at(10,51), fac(lfac$(22)), davail$(22)               , ch(05),~
           at(11,51), fac(lfac$(23)), davail$(23)               , ch(05),~
           at(12,51), fac(lfac$(24)), davail$(24)               , ch(05),~
           at(13,51), fac(lfac$(25)), davail$(25)               , ch(05),~
           at(14,51), fac(lfac$(26)), davail$(26)               , ch(05),~
           at(15,51), fac(lfac$(27)), davail$(27)               , ch(05),~
           at(16,51), fac(lfac$(28)), davail$(28)               , ch(05),~
           at(17,51), fac(lfac$(29)), davail$(29)               , ch(05),~
           at(18,51), fac(lfac$(30)), davail$(30)               , ch(05),~
           at(19,51), fac(lfac$(31)), davail$(31)               , ch(05),~
                                                                         ~
           at(04,17), fac(hex(84))  , shown$ (01)           ,ch(23)     ,~
           at(05,17), fac(hex(84))  , shown$ (02)           ,ch(23)     ,~
           at(06,17), fac(hex(84))  , shown$ (03)           ,ch(23)     ,~
           at(07,17), fac(hex(84))  , shown$ (04)           ,ch(23)     ,~
           at(08,17), fac(hex(84))  , shown$ (05)           ,ch(23)     ,~
           at(09,17), fac(hex(84))  , shown$ (06)           ,ch(23)     ,~
           at(10,17), fac(hex(84))  , shown$ (07)           ,ch(23)     ,~
           at(11,17), fac(hex(84))  , shown$ (08)           ,ch(23)     ,~
           at(12,17), fac(hex(84))  , shown$ (09)           ,ch(23)     ,~
           at(13,17), fac(hex(84))  , shown$ (10)           ,ch(23)     ,~
           at(14,17), fac(hex(84))  , shown$ (11)           ,ch(23)     ,~
           at(15,17), fac(hex(84))  , shown$ (12)           ,ch(23)     ,~
           at(16,17), fac(hex(84))  , shown$ (13)           ,ch(23)     ,~
           at(17,17), fac(hex(84))  , shown$ (14)           ,ch(23)     ,~
           at(18,17), fac(hex(84))  , shown$ (15)           ,ch(23)     ,~
           at(04,58), fac(hex(84))  , shown$ (16)           ,ch(23)     ,~
           at(05,58), fac(hex(84))  , shown$ (17)           ,ch(23)     ,~
           at(06,58), fac(hex(84))  , shown$ (18)           ,ch(23)     ,~
           at(07,58), fac(hex(84))  , shown$ (19)           ,ch(23)     ,~
           at(08,58), fac(hex(84))  , shown$ (20)           ,ch(23)     ,~
           at(09,58), fac(hex(84))  , shown$ (21)           ,ch(23)     ,~
           at(10,58), fac(hex(84))  , shown$ (22)           ,ch(23)     ,~
           at(11,58), fac(hex(84))  , shown$ (23)           ,ch(23)     ,~
           at(12,58), fac(hex(84))  , shown$ (24)           ,ch(23)     ,~
           at(13,58), fac(hex(84))  , shown$ (25)           ,ch(23)     ,~
           at(14,58), fac(hex(84))  , shown$ (26)           ,ch(23)     ,~
           at(15,58), fac(hex(84))  , shown$ (27)           ,ch(23)     ,~
           at(16,58), fac(hex(84))  , shown$ (28)           ,ch(23)     ,~
           at(17,58), fac(hex(84))  , shown$ (29)           ,ch(23)     ,~
           at(18,58), fac(hex(84))  , shown$ (30)           ,ch(23)     ,~
           at(19,58), fac(hex(84))  , shown$ (31)           ,ch(23)     ,~
            at(04,06), fac(hex(8c)), dd$(01), ch(3),                     ~
            at(05,06), fac(hex(8c)), dd$(02), ch(3),                     ~
            at(06,06), fac(hex(8c)), dd$(03), ch(3),                     ~
            at(07,06), fac(hex(8c)), dd$(04), ch(3),                     ~
            at(08,06), fac(hex(8c)), dd$(05), ch(3),                     ~
            at(09,06), fac(hex(8c)), dd$(06), ch(3),                     ~
            at(10,06), fac(hex(8c)), dd$(07), ch(3),                     ~
            at(11,06), fac(hex(8c)), dd$(08), ch(3),                     ~
            at(12,06), fac(hex(8c)), dd$(09), ch(3),                     ~
            at(13,06), fac(hex(8c)), dd$(10), ch(3),                     ~
            at(14,06), fac(hex(8c)), dd$(11), ch(3),                     ~
            at(15,06), fac(hex(8c)), dd$(12), ch(3),                     ~
            at(16,06), fac(hex(8c)), dd$(13), ch(3),                     ~
            at(17,06), fac(hex(8c)), dd$(14), ch(3),                     ~
            at(18,06), fac(hex(8c)), dd$(15), ch(3),                     ~
            at(04,47), fac(hex(8c)), dd$(16), ch(3),                     ~
            at(05,47), fac(hex(8c)), dd$(17), ch(3),                     ~
            at(06,47), fac(hex(8c)), dd$(18), ch(3),                     ~
            at(07,47), fac(hex(8c)), dd$(19), ch(3),                     ~
            at(08,47), fac(hex(8c)), dd$(20), ch(3),                     ~
            at(09,47), fac(hex(8c)), dd$(21), ch(3),                     ~
            at(10,47), fac(hex(8c)), dd$(22), ch(3),                     ~
            at(11,47), fac(hex(8c)), dd$(23), ch(3),                     ~
            at(12,47), fac(hex(8c)), dd$(24), ch(3),                     ~
            at(13,47), fac(hex(8c)), dd$(25), ch(3),                     ~
            at(14,47), fac(hex(8c)), dd$(26), ch(3),                     ~
            at(15,47), fac(hex(8c)), dd$(27), ch(3),                     ~
            at(16,47), fac(hex(8c)), dd$(28), ch(3),                     ~
            at(17,47), fac(hex(8c)), dd$(29), ch(3),                     ~
            at(18,47), fac(hex(8c)), dd$(30), ch(3),                     ~
            at(19,47), fac(hex(8c)), dd$(31), ch(3),                     ~
                                                                         ~
                keys(hex(030405070f10)), key(keyhit%)

            if keyhit% = 16 then see_capacities_again
            if keyhit% = 3 then seeparts
            if keyhit% = 7 then demands

            if keyhit% <> 4 then L49710
                if f% = 1 then L48310
                mo% = mm%(f%-1)
                yr% = yy%(f%-1)
                goto L48080
L49710:     if keyhit% <> 5 then L49760
                if l% = 490 then L48310
                mo% = mm%(l%+1)
                yr% = yy%(l%+1)
                goto L48080
L49760:     if keyhit% <> 15 then L48310
                call "PRNTSCRN"
                goto L48310

        get_cursor_pos
            close ws
            edit% = 0
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                if cursor%(1) < 4 or cursor%(1) > 19 then return
                if cursor%(1) > 18 and cursor%(2) < 40 then return
            temp% = cursor%(1) - 3
            if cursor%(2) > 40 then temp% = temp% + 15
            if edit% + f% - 1 > 490 or edit% + f% - 1 < 1 then return
            edit% = temp%
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50250,         /* WORK CENTER CODE */~
                                    L50340,         /* WC DESCR         */~
                                    L50380,         /* PM FREQ          */~
                                    L50400,         /* LAST PM DATE     */~
                                    L50450,         /* UNITS IN 24 HRS  */~
                                    L50510,         /* GROUP            */~
                                    L50530,         /* MONDAY           */~
                                    L50530,         /* TUESDAY          */~
                                    L50530,         /* WEDNESDAY        */~
                                    L50530,         /* THURSDAY         */~
                                    L50530,         /* FRIDAY           */~
                                    L50530,         /* SATURDAY         */~
                                    L50530          /* SUNDAY           */
                     return

L50250:     REM TEST DATA FOR WORK CENTER CODE
                if wc$ <> " " then L50310
                call "GETCODE" (#11, wc$, " ", 0%, 0, wconfile%)
                    if wconfile% <> 0% then L50320
                       errormsg$ = hex(00)
                       return
L50310:         call "GETCODE" (#11, wc$, " ", 0%, 99, wconfile%)
L50320:             if wconfile% <> 0 then load_wc_info
                return
L50340:     REM TEST DATA FOR WORK CENTER DESCRIPTION
                if wcdescr$ <> " " then return
                    errormsg$ = "Description May Not Be Blank"
                    return
L50380:     REM TEST DATA FOR PM FREQUENCY IN DAYS
                return
L50400:     REM TEST DATA FOR LAST PM DATE
                if lastpm$ = " " then return
                call "DATEOK" (lastpm$, u3%, errormsg$)
                    if errormsg$ <> " " then return
                return
L50450:     REM TEST DATA FOR UNITS IN 24 hr PERIOD
                if units$ = " " then units$ = "24"
                call "NUMTEST" (units$, 1, 65000, errormsg$, -0.001, temp)
                   if errormsg$ <> " " then return
                REM describe units in terms of hours for users sanity
                call "WCUN2HRS" (#11, hex(00000000), temp, 0, unitsdescr$)
                return

L50510:     REM Test for Workcenter Group
            if group$ = " " then L50523
            if can$ <> "Y" then return
            groupdescr$ = hex(06) & "Select Workcenter Group Code"
            readkey$ = "WCGROUPS " & group$
            call "PLOWCODE" (#02, readkey$, groupdescr$, 9%, 0.3, f1%(2))
            if f1%(2) = 1% then L50519
                 errormsg$ = "Invalid Workcenter Group Code."
                 return
L50519:     group$ = str(readkey$,10)
            call "PUTPAREN" (groupdescr$)
            return

L50523:     groupdescr$ = " "
            return
L50530:     REM TEST DATA FOR NORM UNITS WORKED
            call"NUMTEST"(nuw$(fieldnr%-6%),0,65000,errormsg$,-.001,temp)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR RESET CENTERS CAPACITY.                    *~
            *************************************************************

            deffn'159(fieldnr%)
                  errormsg$ = " "
                  if fieldnr% > 2 then L52330
                  on fieldnr% gosub L52120,         /* START DATE       */~
                                    L52220          /* END DATE         */
                     return
L52120:     REM TEST DATA FOR START DATE
                   call "DATEOKC" (start$, u3%, errormsg$)
                   if errormsg$ <> " " then return
                   unfstart$ = start$
                   call "DATUFMTC" (unfstart$)
                   call "PIPINDEX" (#1, unfstart$, pipidx_str%, u3%)
                   fmyymmdd$ = yymmdd$(1)
                   toyymmdd$ = yymmdd$(490)
                   call "DATFMTC" (fmyymmdd$)
                   call "DATFMTC" (toyymmdd$)
                     if u3% <> 0 then errormsg$ = "Date Must Be Between "~
                                    & fmyymmdd$ & " and " & toyymmdd$
                   return
L52220:     REM TEST DATA FOR END DATE
                   call "DATEOKC" (end$, u3%, errormsg$)
                   if errormsg$ <> " " then return
                   unfend$ = end$
                   call "DATUFMTC" (unfend$)
                   call "PIPINDEX" (#1, unfend$, pipidx_end%, u3%)
                   toyymmdd$ = yymmdd$(490)
                   call "DATFMTC" (toyymmdd$)
                   if u3% <> 0 or unfstart$ > unfend$ then ~
                      errormsg$ = "Date Must Be Between " & start$ & ~
                      ~" and " & toyymmdd$
                   return
L52330:     REM TEST UNITS ENTERED
            call"NUMTEST"(new$(fieldnr%-2%),0,65000,errormsg$,-.001,temp)
            return


        REM *************************************************************~
            *      T E S T   D A T A   F O R   P A G E  2               *~
            *                                                           *~
            * Test Data For Items On Page 2 - Work Center Defaults      *~
            *************************************************************

            deffn'251(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L54230,         /* Move/Queue Time  */~
                                    L54300,         /* Setup Time       */~
                                    L54530,         /* Run Time         */~
                                    L54930,         /* Yield Percentage */~
                                    L54970,         /* Setup Activity   */~
                                    L55070,         /* Run Activity     */~
                                    L55170,         /* Activity Descr   */~
                                    L55200,         /* Completion Percnt*/~
                                    L55380          /* Handling Factor  */

                  return

L54230: REM Test Data For Move/Queue Time...
            call "NUMTEST" (mq$, 0, 999, errormsg$, -0.01, mq)
                if errormsg$ <> " " then return
            if mq = 0 then mqopt$ = " "
            if mqopt$ <> "N" then mqopt$ = "Y"
            return

L54300: REM Test Data For Setup Time...
            REM Decide which way the time was entered...
                if savesuh$ <> " " then L54323
                     if suh$ <> " " then L54380
                     goto L54330
L54323:         if savesuh$ <> suh$ then L54380

L54330:     REM Setup Entered In Wcunits...
                call "NUMTEST" (su$, 0, 65000, errormsg$, -0.01,test)
                    if errormsg$ <> " " then return
                call "WCUN2HRS" (#11, wc$, 0, test, " ")
                goto L54500
L54380:     REM Setup Entered In Hours...
                savesuh$ = suh$
                if pos(suh$ = ":") = 0% then L54440
                    temp$ = suh$
                    gosub L55320
                    suh$ = temp$
L54440:         temp% = 0%            /* Must be even mutiple of factor */
                call "NUMTEST" (suh$, 0, 65000, errormsg$,-2.4,test)
                savesuh$ = suh$
                    if errormsg$ <> " " then return
                gosub L55240
                call "CONVERT" (test1, 0.0, su$)
L54500:         call "CONVERT" (test, 2.4, suh$)
                return

L54530: REM Test Data For Run Time...
            REM Decide which way the time was entered...
                if saverunh$ <> " " or saverun$ <> " " then L54550
                    if runh$ <> " " then L54700
                    if run$  <> " " then L54640
                    goto L54570
L54550:         if saverunh$ <> runh$ then L54700
                    if saverun$ <> run$ then L54640

L54570:     REM Run Time Entered In Parts Per Hour...
                call "NUMTEST" (runp$, 0, 9e6, errormsg$, -0.6, runp)
                   if errormsg$ <> " " then return
                test = 0
                if runp <> 0 then test = 1/runp
                call "CONVERT" (test, -4.6, runh$)
                goto L54700
L54640:     REM Run Time Entered In Wcunits...
                call "NUMTEST" (run$, 0, 9e6, errormsg$, -0.4, test)
                saverun$ = run$
                   if errormsg$ <> " " then return
                call "WCUN2HRS" (#11, wc$, 0, test, " ")
                goto L54820
L54700:     REM Run Time Entered In Hours...
                saverunh$ = runh$
                if pos(runh$ = ":") = 0 then L54760
                    temp$ = runh$
                    gosub L55320
                    runh$ = temp$
L54760:         temp% = 4 /* Must be even mutiple of factor to 4 Places*/
                call "NUMTEST" (runh$, 0, 9e6, errormsg$, -4.6, test)
                saverunh$ = runh$
                    if errormsg$ <> " " then return
                gosub L55240
                call "CONVERT" (test1, 0.4, run$)
L54820:         call "CONVERT" (test, 4.6, runh$)
                runp = 0
                if test <> 0 then runp = 1/test
                call "CONVERT" (runp, 0.6, runp$)
                if wc$ <> "VEND" then return
                mq,su = 0
                convert mq$ to mq, data goto L54890
L54890:         convert su$ to su, data goto L54900
L54900:         if mq + su + test = 0 then mq$ = "1"
                return

L54930: REM Test Data For Yield percentage...
            call "NUMTEST" (yield$, 1, 100, errormsg$,-.01, 0)
            return

L54970: REM Test data for Setup Activity Code...
            if su$ = " " then L55000
            convert su$ to temp
            if sucode$ = " " and temp = 0 then return
L55000:         sudescr$ = hex(06) & "Select Set-Up Activity Code"
                tempkey$ = "WC ACTVTY" & sucode$
                call "PLOWCODE" (#2, tempkey$, sudescr$, 9%, 0.3, f1%(2%))
                if f1%(2%) <> 0% then L55030
                    if sucode$ = "?" then sucode$ = " "
                    goto L55040
L55030:         sucode$ = str(tempkey$,10%)
L55040:     if sucode$ = " " then sudescr$ = " "
            return

L55070: REM Test data for Run Activity Code...
            if run$ = " " then L55100
            convert run$ to temp
            if rucode$ = " " and temp = 0 then return
L55100:         rudescr$ = hex(06) & "Select Run Activity Code"
                tempkey$ = "WC ACTVTY" & rucode$
                call "PLOWCODE" (#2, tempkey$, rudescr$, 9%, 0.3, f1%(2%))
                if f1%(2%) <> 0% then L55130
                    if rucode$ = "?" then rucode$ = " "
                    goto L55140
L55130:         rucode$ = str(tempkey$,10%)
L55140:     if rucode$ = " " then rudescr$ = " "
            return

L55170: REM Test Data For Run Activity Description...
            return

L55200: REM Test Data For Percentage Complete...
            call "NUMTEST" (comp$, 0, 100, errormsg$, -.01, 0)
            return

L55240: REM Simulates Datasave Effect to Insure no surprizes later...
            convert units$ to units, data goto L55250
L55250:     adjfactor = .00000001
            call "WCUN2HRS" (#11, hex(00000000), units, 0, " ")
                if units <> 0 then adjfactor = 24/units
            test1 = round(test/adjfactor, temp%)
            call "WCUN2HRS" (#11, hex(00000000), 0, test, " ")
            return

L55320: REM Take Time Entered As HH;MM;SS and convert to decimal...
            call "TIMEOK" (temp$, test, errormsg$)
                if errormsg$ = " " then L55350
                     return clear :  return
L55350:         call "CONVERT" (test, 0.4, str(temp$,,8%))
            return

L55380: REM Test Data For Handling Factor...
            call "NUMTEST" (handfactor$, -9999999, 99999999,             ~
                            errormsg$, -2.2, temp1)
            if errormsg$ <> " " then return
            if temp1 <> 0 then  return
            errormsg$ = "The Factor Cannot Be 0."
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

            call "SHOSTAT" ("One Moment Please")
            end
