        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   L      N   N  RRRR    SSS   TTTTT  RRRR           *~
            *  P   P  L      NN  N  R   R  S        T    R   R          *~
            *  PPPP   L      N N N  RRRR    SSS     T    RRRR           *~
            *  P      L      N  NN  R   R      S    T    R   R          *~
            *  P      LLLLL  N   N  R   R   SSS     T    R   R          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PLNRSTR  - DELETES ALL PLANNED DEMANDS & ASSOCIATED DATA. *~
            *            PROGRAM FUNTIONS AS A DATA BASE CLEANUP IN THE *~
            *            EVENT THAT THING GET OUT OF CONTROL. ALL FILES *~
            *            ARE OPENED IN IO MODE.                         *~
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
            * 09/09/83 ! ORIGINAL                                 ! HES *~
            * 08/11/86 ! Misc minor repairs                       ! KAB *~
            * 10/22/86 ! WCout file format change                 ! HES *~
            * 02/04/87 ! Changed HNYQUAN Format                   ! KAB *~
            * 10/15/93 ! Purchase Jobs - Added Support for 'BW'.  ! JBK *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            askhdr$40,                                                   ~
            askpf1$80,                                                   ~
            askmid$80,                                                   ~
            askpf2$80,                                                   ~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            demand$122,                  /* DEMAND RECORD              */~
            do_it$3,                     /* TYPE IN 'YES' TO BEGIN REGE*/~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            file$8,                      /* FILE NAME FOR GETNAMES     */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lib$8,                       /* LIB  NAME FOR GETNAMES     */~
            line2$79,                    /* SCREEN LINE                */~
            pf16$20,                     /* PF 16 MESSAGE              */~
            part$25,                     /* PIP PART                   */~
            pipinpart$25,                /* PIP PART                   */~
            pipoutpart$25,               /* PIP PART                   */~
            plbase$6,                                                    ~
            p$25,                        /* PIP PART                   */~
            readkey$60,                  /* WORK VARIABLE              */~
            readkey1$60,                 /* WORK VARIABLE              */~
            rec1$60,                     /* WORKING STRING             */~
            rec2$16,                     /* WORKING STRING             */~
            stat$1,                      /* WORK VARIABLE              */~
            pip%(490),                   /*                            */~
            pip(490),                    /*                            */~
            vol$6                        /* VOL  NAME FOR GETNAMES     */~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! DEMMASTR ! Demand Master File                       *~
            * #2  ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #3  ! PIPIN    ! Planned inventory additions detail       *~
            * #4  ! PIPOUT   ! Planned inventory use detail rec         *~
            * #5  ! WCMASTR  ! Work center master file                  *~
            * #6  ! WCOUT    ! Planned work center use detail rec       *~
            * #7  ! SFMASTR2 ! Sales forecast master file               *~
            * #8  ! SFCUM2   ! Cumulative sales forecast file           *~
            * #9  ! JBCROSS2 !                                          *~
            * #10 ! PIPCROSS !                                          *~
            * #11 ! JBPIPXRF !                                          *~
            * #12 ! HNYQUAN  ! Inventory Quantity File                  *~
            * #14 ! SYSFILE2 ! System Records File                      *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "DEMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  123,                                  ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28

            select #2,  "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26

            select #3,  "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48

            select #4,  "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37

            select #5,  "WCMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6

            select #6,  "WCOUT",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   68,                                  ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =    1, keylen =  27

            select #7,  "SFMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    1, keylen =  25

            select #8,  "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =  25

            select #9,  "JBCROSS2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 94,                                    ~
                        keypos=29, keylen = 19,                          ~
                        alt key 1, keypos = 1 , keylen = 47,             ~
                            key 2, keypos = 48, keylen = 47

            select #10, "PIPCROSS", varc, indexed,                       ~
                     recsize = 150,  keypos =  1  , keylen =  71,        ~
                     alt key 1, keypos = 20, keylen = 52,                ~
                         key 2, keypos = 39, keylen = 33

           select #11, "JBPIPXRF",varc, indexed, recsize=63,             ~
                        keypos=1, keylen=63,                             ~
                        alt key 1, keypos=45, keylen=19

            select #12, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   650,                                 ~
                        keypos =    1, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #14, "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#1,  "IO   ", f2%(1 ), rslt$(1 ), axd$(1 ))
            call "OPENFILE" (#2,  "IO   ", f2%(2 ), rslt$(2 ), axd$(2 ))
            call "OPENFILE" (#3,  "IO   ", f2%(3 ), rslt$(3 ), axd$(3 ))
            call "OPENFILE" (#4,  "IO   ", f2%(4 ), rslt$(4 ), axd$(4 ))
            call "OPENFILE" (#5,  "IO   ", f2%(5 ), rslt$(5 ), axd$(5 ))
            call "OPENFILE" (#6,  "IO   ", f2%(6 ), rslt$(6 ), axd$(6 ))
            call "OPENFILE" (#7,  "IO   ", f2%(7 ), rslt$(7 ), axd$(7 ))
            call "OPENFILE" (#8,  "IO   ", f2%(8 ), rslt$(8 ), axd$(8 ))
            call "OPENFILE" (#9,  "IO   ", f2%(9 ), rslt$(9 ), axd$(9 ))
            call "OPENFILE" (#10, "IO   ", f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#11, "IO   ", f2%(11), rslt$(11), axd$(11))
            call "OPENFILE" (#12, "IO   ", f2%(12), rslt$(12), axd$(12))
            call "OPENFILE" (#14, "IO   ", f2%(14), rslt$(14), axd$(14))

            for u3% = 1% to 14%
                if u3% = 13% then L05210
                if f2%(u3%) <> 0% then file_conflict
L05210:     next u3%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

            call "READ100" (#14, "MONTHS OPEN", f1%(14%))
                if f1%(14%) = 0% then date_error
            get #14, using L09150, plbase$
L09150:         FMT XX(32), CH(6)

            call "DATE" addr("G-", plbase$, date, today%, err%)
                if err% <> 0% then date_error
            today% = min(490%, max(1%, today%))

            str(line2$,62%) = " PLNRSTR: " & str(cms2v$,,8%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, do_it$
            pf16$ = "    (16)EXIT PROGRAM"
            for fieldnr% = 1% to  1%
                gosub'051(fieldnr%)
                      if enabled% = 0% then L10180
L10120:         gosub'111(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit% <>  0% then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10180:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

L11060:     pf16$ = "(16)START PROCESSING"
            inpmessage$ = edtmessage$
L11080:     gosub'111(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       L11080
            fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  1% then L11080

            gosub'051(fieldnr%)
                  if enabled% = 0% then L11060
L11150:     gosub'111(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11150
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11150
            goto L11060

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub kill_sf_files
            gosub kill_wo_bo_pips
            gosub reset_demands
            gosub nuke_wc_sf_data
            gosub reset_pipmastr
            goto  L65000

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 0%
                  on fieldnr% gosub L20100          /* DO IT?           */
                     return
L20100:     REM DEFAULT/ENABLE FOR TYPE IN 'YES' TO BEGIN REGE
                enabled% = 1%
                inpmessage$ = "Enter 'YES' to Proceed.  Any other respons~
        ~e will EXIT PROGRAM."
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
L29690:     askkey% = 2%
            call "STARTOVR" (askkey%)
            if askkey% = 1% then return
            if askkey% <> 0% then L29690
               return clear all
               goto inputmode

        rem**************************************************************~
*       ***********    clear work & s/f files         ******************~
*       ****************************************************************~

        kill_sf_files
            call "SHOSTAT" ("Clearing Sales Forecast Detail files.")

            if f2%(7%) = 0% then close #7
            if f2%(8%) = 0% then close #8

            call "GETNAMES" addr (#7, file$, lib$, vol$)
            call "SCRATCH"  addr ("F", file$, lib$, vol$, "B", " ", u3%)
            call "GETNAMES" addr (#8, file$, lib$, vol$)
            call "SCRATCH"  addr ("F", file$, lib$, vol$, "B", " ", u3%)
        REM THAT TOOK CARE OF ALL THE SF DATA !!
            call "OPENFILE" (#7,  "OUTPT", f2%(7%), rslt$(7%), axd$(7%))
            call "OPENFILE" (#8,  "OUTPT", f2%(8%), rslt$(8%), axd$(8%))

            return

        rem**************************************************************~
*       ***********    clear work & buy order pips    ******************~
*       ****************************************************************~

        kill_wo_bo_pips
            call "SHOSTAT" ("Clearing PIP Detail files.")

            readkey$ = all(hex(00))
            str(readkey$,,2%) = "WO"
            call "DELETE" (#3, readkey$,2%)

            readkey$ = all(hex(00))
            str(readkey$,,2%) = "WO"
            call "DELETE" (#11, readkey$,2%)

            readkey$ = all(hex(00))
            str(readkey$,,2%) = "BO"
            call "DELETE" (#3, readkey$, 2%)

            readkey$ = all(hex(00))
            str(readkey$,,2%) = "WO"
            call "DELETE" (#4, readkey$, 2%)

            readkey$ = all(hex(00))
            str(readkey$,,2%) = "WO"
            call "DELETE" (#9, readkey$, 2%)

            readkey$ = all(hex(00))
            str(readkey$,,2%) = "BW"
            call "DELETE" (#3, readkey$, 2%)                  /* PIPIN */

            readkey$ = all(hex(00))
            str(readkey$,,2%) = "BW"
            call "DELETE" (#4, readkey$, 2%)                 /* PIPOUT */

            readkey$ = all(hex(00))
            str(readkey$,,2%) = "BW"
            call "DELETE" (#9, readkey$, 2%)               /* JBCROSS2 */

            readkey$ = all(hex(00))                /* Shouldn't be any */
            str(readkey$,,2%) = "BW"
            call "DELETE" (#11, readkey$, 2%)              /* JBPIPXRF */

            return

        rem**************************************************************~
*       ************    reset demand status to unplanned    ************~
*       ****************************************************************~

        reset_demands
            call "SHOSTAT" ("Resetting the status of demands.")
            readkey$ = all(hex(00))

L32080:     call "PLOWNXT1"  (#1, readkey$, 0%, f1%(1%))
                if f1%(1%) = 0% then return
            get #1, using L32400, stat$, demand$

            str(demand$,76%,12%) = " "  /* INITIALIZE LAST PLANNED & */
                                        /* PLANNED COMPLETION DATES  */

            if stat$ = " " or stat$ = "1" then L32170
            if pos("68" = stat$) = 0% then stat$ = "1"  else stat$ = " "
L32170:     delete #1
            write #1, using L32400, stat$, demand$
            readkey1$ = str(demand$,9%,19%) & hex(00000000)
            dd%=0%  :  qty=0
            call "DATE" addr("G-", plbase$, str(demand$,3%,6%), dd%, err%)
            dd% = dd% + 1%  :  dd% = min(490%,max(1%,dd%))
L32230:     call "PLOWNXT1" (#4, readkey1$, 19%, f1%(4%))
                     if f1%(4%) = 0% then L32300
            get #4, using L32260, p$, q
L32260:         FMT XX(19), CH(25), XX(4), XX(8), PD(14,4)
            delete #4
            qty = qty + q
            goto L32230
L32300:     call "READ101" (#3, str(demand$,9%,19%), f1%(3%))
               if f1%(3%) <> 0% then delete #3
            if abs(qty)<.00001 then L32370
            write #3, using L32340, p$, dd%, str(demand$,9%,19%), qty, dd%
L32340:         FMT CH(25),BI(4),CH(19),PD(14,4),BI(4)
            write #4, using L32360, str(demand$,9%,19%), p$, dd%, time, qty
L32360:         FMT CH(19), CH(25), BI(4), CH(8), PD(14,4)
L32370:     call "DELETE" (#10, str(demand$,9%,19%),19%)
            goto L32080

L32400: FMT                      /* FILE: DEMMASTR                     */~
            CH(1),               /* STATUS                             */~
            CH(122)              /* REST OF RECORD                     */

        rem**************************************************************~
*       ********    reset work center & sales forcast data    **********~
*       ****************************************************************~

        nuke_wc_sf_data

            call "SHOSTAT" ("Adjusting Work Center Capacity allocated.")
            readkey$ = all(hex(00))
            str(readkey$,,2%) = "WO"
            call "DELETE" (#6, readkey$, 2%)

        REM **** ADJUST FOR DEAD WCOUT RECORDS *****
            call "WCBUILD" ("ALL^", #5, #6)
            return

        rem**************************************************************~
*       ********    reset pipmaster after detail deletions    **********~
*       ****************************************************************~

        reset_pipmastr

            call "SHOSTAT" ("Rebuilding the PIPMASTR file.        (Last s~
        ~tep)")
            init (hex(00)) part$, pipinpart$, pipoutpart$
            f1%(3%), f1%(4%) = -1%  :  active%, hits% = 0%
            call "READ103" (#2, part$, f1%(2%))
                     if f1%(2%) = 0% then return
L34120:     get #2,using L34130,rec1$,part$,pip%(),oh,ss,mq,pz,tp%,rec2$
L34130:            FMT CH(1), CH(25), 490*BI(4), 4*PD(14,4), BI(2), CH(4)

            mat pip = zer
        REM GOSUB CALC_QTYOH
            pip(today%) = oh

            if f1%(3%) < 0% then L34220
            if str(pipinpart$,1%,1%) = hex(ff) then L34350
            goto L34270
L34220:     readkey$ = all(hex(00))
            str(readkey$,,25%) = part$
            call "PLOWALTS" (#3, readkey$, 1%, 0%, f1%(3%))
                     if f1%(3%) = 0 then L34330
L34260:     get #3, using L34940, pipinpart$, di%, qi
L34270:     if pipinpart$ < part$ then L34310
            if pipinpart$ > part$ then L34350
            pip(di%) = pip(di%) + qi
            active% = active% + 1%
L34310:     read #3, eod goto L34330
            goto L34260
L34330:     init (hex(ff)) pipinpart$

L34350:     if f1%(4%) < 0% then L34380
            if str(pipoutpart$,1%,1%) = hex(ff) then L34510
            goto L34430
L34380:     readkey$ = all(hex(00))
            str(readkey$,,25%) = part$
            call "PLOWALTS" (#4, readkey$, 1%, 0%, f1%(4%))
                     if f1%(4%) = 0% then L34490
L34420:     get #4, using L35010, pipoutpart$, do%, qo
L34430:     if pipoutpart$ < part$ then L34470
            if pipoutpart$ > part$ then L34510
            pip(do%) = pip(do%) - qo
            active% = active% + 1%
L34470:     read #4, eod goto L34490
            goto L34420
L34490:     init (hex(ff)) pipoutpart$

L34510: rem OH=ROUND(OH,2):ss=round(ss,2):mq=round(mq,2):pz=round(pz,2)
            if active% > 0% then L34620
               mat pip% = zer
                  if abs (oh) < .0001 then L34590
               oh% = sgn(oh)*int(abs(oh))
               for i% = today% to 490%
               pip%(i%) = oh%
               next i%
L34590:      atc% = pip%(today%)
             goto L34730

L34620:     for i% = 489% to 1% step -1%
            if abs(pip(i%)) < .0001 then L34670
               for j% = i%+1% to 490%
                 pip(j%) = pip(j%) + pip(i%)
               next j%
L34670:     next i%
            mat pip% = pip
            atc% = pip%(today%)
            for i% = today% to 490%
                if pip%(i%) < atc% then atc% = pip%(i%)
            next i%
L34730:     rec1$=" "
            if tp% = 0% or tp%>499% then L34790
            if atc% < ss then rec1$="5"
            if atc% < 0% then rec1$="9"
            if atc% > ss + mq then rec1$="3"
            goto L34830
L34790:     if atc% < ss then rec1$="4"
            if atc% < 0% then rec1$="8"
            if atc% > ss + mq then rec1$="2"

L34830:     put #2, using L34130, rec1$,part$,pip%(),oh,ss,mq,pz,tp%,rec2$
            rewrite #2

            active% = 0%
            if mod(hits%,100%) = 0% then                                 ~
                                  print at (19,02), part$, hits%, time
            hits% = hits% + 1%
            read #2, hold, eod goto L34920
            goto L34120
L34920:     return

L34940: FMT                      /* FILE: PIPIN                        */~
            CH(25),              /* Part code                          */~
            BI(4),               /* Date in subscript for PIP          */~
            XX(19),              /* Tag number in level 2 planning     */~
            PD(14,4),            /* Quantity of something in packed de */~
            XX(4)                /* Date to start as a date subscript  */~

L35010: FMT                      /* FILE: PIPOUT                       */~
            XX(19),              /* Tag number in level 2 planning     */~
            CH(25),              /* Part code                          */~
            BI(4),               /* Date out of PIP in date subscript  */~
            XX(8),               /* Time from the system clock         */~
            PD(14,4)             /* Quantity of something in packed de */~

        REM CALC_QTYOH

            init (hex(00)) readkey$
            str(readkey$,1%,25%) = str(part$,1%,25%)
L35120:     call "PLOWNEXT" (#12, readkey$, 25%, f1%(12%))
                if f1%(12%) = 0% then return
                if str(readkey$,26%,1%) < "0" then L35120
                if str(readkey$,26%,1%) > "9" then return
            get #12, using L35170, qty
L35170:         FMT XX(69), PD(14,4)
            oh = oh + qty
            goto L35120

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40140          /* DO IT?           */
                     goto L40210

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40140:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40210:     accept                                                       ~
               at (01,02),                                               ~
                  "Unplan All Demands",                                  ~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02),                                               ~
                 "Type in 'YES' to Begin Unplanning All Planned Demands",~
               at (06,58), fac(lfac$( 1)), do_it$               , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,61), fac(hex(84)), pf16$                  , ch(20),~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40530
                  call "MANUAL" ("PLNRSTR ")
                  goto L40210

L40530:        if keyhit% <> 15 then L40570
                  call "PRNTSCRN"
                  goto L40210

L40570:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100          /* DO IT?           */
                     return
L50100:     REM TEST DATA FOR TYPE IN 'YES' TO BEGIN REGE
                if do_it$ <> "YES" then L65000
                return

        REM *************************************************************~
            * ERROR ABORT SECTION                                       *~
            *************************************************************

        file_conflict

            askkey% = 0%
            askhdr$ = "* * * SORRY * * *"
            askpf1$ = "Exclusive access to the Planning System Files is R~
        ~equired."
            askmid$ = " "
            askpf2$ = "Press Any PF Key to Exit Program."

            call "ASKUSER" (askkey%, askhdr$, askpf1$, askmid$, askpf2$)
            goto L65000

        date_error

            askkey% = 0%
            askhdr$ = "* * * SORRY * * *"
            askpf1$ = "An Error Has been detected in the Planning System ~
        ~Base Date."
            askmid$ = " "
            askpf2$ = "Press Any PF Key to Exit Program."

            call "ASKUSER" (askkey%, askhdr$, askpf1$, askmid$, askpf2$)
            goto L65000

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
