        REM *************************************************************~
            *                                                           *~
            *  BBBB    OOO   M   M   OOO    AAA                         *~
            *  B   B  O   O  MM MM  O   O  A   A                        *~
            *  BBBB   O   O  M M M  O   O  AAAAA                        *~
            *  B   B  O   O  M   M  O   O  A   A                        *~
            *  BBBB    OOO   M   M   OOO   A   A                        *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMOA    - BILL OF MATERIALS ORDER ANALYSIS PROGRAM. THIS *~
            *            PROGRAM TAKES SINGLE OR MULTIPLE PARTS AND     *~
            *            PRINTS INDIVIDUAL OR CUMULATIVE ORDER ANALYSIS *~
            *            ON THEM.  SEE THE CODE FOR THE TRICKS OF IT.   *~
            *            THIS IS NON-TIME-PHASING MRP FOR LEVEL-II      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/18/81 ! ORIGINAL                                 ! BCW *~
            * 07/24/82 ! CHANGE WORDING & MAKE IT EASIER TO USE   ! GLW *~
            * 08/02/83 ! CORRECT HANDLING OF PHANTOM AND USE-UP   ! GLW *~
            * 12/02/85 ! Change CUM$ = NO Processing              ! MJB *~
            * 09/17/86 ! BOMMASTR FORMAT CHANGED                  ! LKM *~
            * 02/04/87 ! Changed HNYQUAN Format                   ! KAB *~
            * 05/13/87 ! HNYMASTR record length mod for Std Cost  ! JIM *~
            * 06/11/87 ! Install GETCODE for Part number          ! JIM *~
            * 02/18/88 ! Standardized report header, assigned a   ! RJM *~
            *          ! SES Report ID, Added Screen fields to    !     *~
            *          ! allow reporting against ATC instead of   !     *~
            *          ! Quantity On Hand from HNYQUAN.           !     *~
            * 02/22/88 ! Correct BOM ver. selection & Hard-Peg    ! RJM *~
            * 08/03/90 ! Changed Quantity calculation to include  ! MJB *~
            *          !  Added Overage and Fixed Quantity / run. !     *~
            *          ! Corrected screen header.                 !     *~
            * 04/07/92 ! PRR 12313. Fix non-stocked fields.       ! JDH *~
            *          ! PRR 11761. Added BOM IDs to report and   !     *~
            *          !   corrected header.                      !     *~
            * 09/16/93 ! Added Support for new 'NC' BOM Marker.   ! JDH *~
            *          ! Fixed test for 'RE' marker, was 'RF'.    !     *~
            * 09/16/93 ! Added Support for new 'BP' BOM Marker.   ! JDH *~
            * 03/29/94 ! Added Store Range & whether to ignore inv! JDH *~
            *          !   for top level parent part.             !     *~
            *************************************************************

        dim                                                              ~
            avail%(490),                 /* PIP ARRAY                  */~
            atc%(490),                   /* ATC ARRAY                  */~
            atc_date$8,                  /* ATC Date From Calendar     */~
            atc_datef$8,                 /* ATC DATE FOR DISPLAY       */~
            atc_flag$3,                  /* ATC or On Hand ?           */~
            atch_flag$3,                 /* ATC Horizon, Fixed or Var? */~
            bmkr$(20)2,                  /* BOM MARKER TABLE           */~
            bmkrdes$(20)10,              /* BOM MARKER DESCRIP. TABLE  */~
            bom$(490)3,                  /* ALL EFFECTIVE BOMS         */~
            bom_date$8,                  /* BOM EFFECECTIVE DATE       */~
            bom_datef$8,                 /* BOM DATE FOR DISPLAY       */~
            bomid$3,                                                     ~
            bomid$(20)3,                 /* BOM IDs for parts analysed */~
            company$60,                  /* COMPANY NAME FOR HEADING   */~
            component$25,                /* COMPONENT PART NUMBER      */~
            count%(16),                  /* INDENT INDEXES THIS LEVEL  */~
            cumulative$3,                /* CUMULATIVE ANALYSIS? FLAG  */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDITS  */~
            date$8,                      /* SCREEN DISPLAY DATE        */~
            demnum$3,                    /* Demand Sequence Number     */~
            descr$32,                    /* DESCRIPTION OF PART        */~
            diskkey$50,                  /* KEY FOR STORE QTY REC READ */~
            edtmessage$79,               /* EDIT MESSAGE TEXT INFO     */~
            edttran$80,                  /* TRANSLATION STRING FOR EDIT*/~
            errormsg$79,                 /* ERROR MESSAGE TEXT INFO    */~
            fac$(21,2)1,                 /* FIELD ATTRIBUTE CHARACTERS */~
            fmstore$3,                   /* FROM store code range      */~
            hard_bom$3,                  /* HARD PEG BOM ID FOR COMP.  */~
            hdrdate$45,                  /* DATE FOR REPORT PRINTING   */~
            hdrdescr$132,                /* BUILD XXX OF XXX MESSAGE   */~
            histore$3,                   /* High store code range      */~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED)    */~
            i2$(24)80,                   /* SCREEN IMAGE (NOT USED)    */~
            ignore$3,                    /* Ignore Inventory On Hand?  */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            inpmessage$79,               /* INPUT MESSAGE TEXT INFO    */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* Screen line #2             */~
            line10$79,                   /* Screen Line #10            */~
            lostore$3,                   /* Low store code range       */~
            maxlevels$2,                 /* MAXIMUM NUMBER OF LEVELS   */~
            mkr$2,                       /* BOM MARKER                 */~
            mkrdescr$10,                 /* BOM MARKER DESCRIPTION     */~
            op$1,                        /* BOM OPTION FLAG            */~
            pf4$19,                      /* PF 4                       */~
            pf5$18,                      /* PF 5                       */~
            pf16$18,                     /* PF 16                      */~
            p$(16)31,                    /* NEXT BOM REC EACH LEVEL    */~
            paa$4,                       /* Sequence Number            */~
            part$25,                     /* PART TO DO THIS LEVEL      */~
            part$(20)25,                 /* PART NUMBERS FOR MULTIPLE  */~
            part22$25,                   /* PART NUMBERS FOR MULTIPLE  */~
            print$(5)10,                 /* 5 NUMBERS, PRINT FORMATTED */~
            prtpart$100,                 /* PART NUMBER/DESCR INDENTED */~
            quantity$(20)10,             /* QUANTITIES FOR MULTIPLE OA */~
            r(16),                       /* QUANTITY REQUIRED TO HERE  */~
            readkey$100,                 /* KEY TO PROCESS IN READING  */~
            readkey2$100,                /* KEY TO PROCESS IN READING  */~
            store$3,                     /* WAREHOUSE OR STORE         */~
            summary$4,                   /* PRINT SUMMARY REPORT?      */~
            test$2,                      /* USED FOR BOM MARKER TEST   */~
            thispart$25,                 /* PART CODE FOR LOOKUP       */~
            time$8,                      /* TIME OF DAY FOR REPORT     */~
            tostore$3,                   /* TO store code range        */~
            type$3,                      /* Part Type                  */~
            usrid$3,                     /* USERS LOGON ID             */~
            uom$4                        /* UNIT OF MEASURE FOR PART   */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.01 07/28/94 CMS Patch Release R6.03.01      "
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
            * #03 ! STORNAME ! Store Information File                   *~
            * # 4 ! HNYMASTR ! Inventory Master File                    *~
            * # 5 ! BOMMASTR ! Bill Of Materials Relationship File      *~
            * # 6 ! HNYQUAN  ! Inventory Store Quantity Detail File     *~
            * # 7 ! PIPMASTR ! Pip Master File                          *~
            * # 9 ! WORKFILE ! Work File For Cumulative Analyzes        *~
            * #24 ! ENGMASTR ! Engineering Master File                  *~
            * #34 ! SYSFILE2 ! System File For Months Open              *~
            *************************************************************

            select #03, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3

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

            select #6,  "HNYQUAN",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 650,                                  ~
                         keypos=17, keylen = 44,                         ~
                         alternate key 1, keypos =  1, keylen = 44

            select #7,  "PIPMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos =    2, keylen =  25,                    ~
                         alt key  1, keypos =    1, keylen =  26


            select # 9, "SORTWORK",                                      ~
                         indexed,                                        ~
                         recsize = 108,                                  ~
                         keypos =   1, keylen =  25                      ~

           select #24, "ENGMASTR" ,                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos = 1, keylen = 29

            select #34, "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#03, "SHARE", f2%(3%), rslt$(3%), axd$(3%))
            call "OPENFILE" (# 4, "SHARE", f2%( 4), rslt$( 4), axd$( 4))
            call "OPENFILE" (# 5, "SHARE", f2%( 5), rslt$( 5), axd$( 5))
            call "OPENFILE" (# 6, "SHARE", f2%( 6), rslt$( 6), axd$( 6))
            call "OPENFILE" (#24, "SHARE", f2%(24), rslt$(24), axd$(24))
            call "OPENFILE" (#34, "SHARE", f2%(34), rslt$(34), axd$(34))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES FIELDS NECESSARY FOR PROCESSING THE REPORT.   *~
            *************************************************************

            line10$ = "Part Number              " & hex(ac) &            ~
                "  Quantity" & hex(8c)
            str(line10$,43) = hex(ac) & "Part Number              " &    ~
                hex(ac) & "  Quantity"
            date$ = date
            time$ = " " : call "TIME" (time$)
            call "COMPNAME" (12%, company$, ret%)
            call "DATEFMT" (date$)

            call "EXTRACT" addr ("ID", usrid$, 3%)

            REM SET UP STRINGS FOR EDIT TRANSLATION.
                init(hex(00)) str(edttran$,  1, 80)
                init(hex(01)) str(edttran$,  1, 27)
                init(hex(02)) str(edttran$, 28, 11)
                init(hex(01)) str(edttran$, 44, 26)
                init(hex(02)) str(edttran$, 70, 11)

            str(line2$,65) = "BOMOA: " & str(cms2v$,1,8)

        REM Load Up BOM Markers Table...
            readkey$ = "TABLE01:"
L09200:     call "PLOWNEXT" (#34, readkey$, 8%, f1%(34))
                if f1%(34) = 0% then L09270
            bm% = bm% + 1%
            get #34, using L09240, bmkr$(bm%), bmkrdes$(bm%)
L09240:     FMT XX(8), CH(2), XX(40), CH(10)
            if bm% < 20% then L09200

L09270:     edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *               I N P U T   Q U E S T I O N S               *~
            *                                                           *~
            * INPUTS THE QUESTIONS ABOUT ORDER ANALYSIS OPTIONS         *~
            *************************************************************

        inputmode1
            init(" ") cumulative$, summary$, maxlevels$, part$(), bomid$,~
                      quantity$(), errormsg$, infomsg$, inpmessage$,     ~
                      atc_flag$, atc_datef$, atc_date$, atch_flag$,      ~
                      bom_date$, bom_datef$, type$, bomid$(), hard_bom$, ~
                      fmstore$, tostore$, histore$, lostore$, ignore$
            maxpart%, c%, type%, bom_today% = 0%
            if anotherflag% = 1% then infomsg$ = "Your Analysis has Been ~
        ~Printed, Do You Wish Another?"
            pf5$ = hex(8c)

L10140:     for fieldnr% = 1% to 9%
                if fieldnr% = 1% then pf16$ = hex(84) &"(16)Exit Program"~
                                 else pf16$ = hex(8c)
                if fieldnr% = 1% then pf4$ = hex(8c)                     ~
                                 else pf4$ = hex(8c)&"(4)Previous Field"
                gosub'161(fieldnr%)
                      if enabled% =  0% then L10310
L10190:         gosub'201(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10270
L10220:                  fieldnr% = max(1%, fieldnr% - 1%)
                         if fieldnr% = 1% then L10140
                         gosub'161(fieldnr%)
                         if enabled% <> 0% then L10190
                         goto L10220
L10270:               if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit% <>  0% then       L10190
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10190
L10310:         next fieldnr%
            goto inputmode2

        REM *************************************************************~
            *                E D I T   R E S P O N S E S                *~
            *                                                           *~
            * EDITS THE HEADER SCREEN, ORDER ANALYSIS OPTION PARAMETERS *~
            *************************************************************
        editpg1
            init(" ") errormsg$, inpmessage$, infomsg$

L11081:     inpmessage$ = edtmessage$
            pf16$, pf4$ = hex(8c)
            pf5$ = hex(84) & "(5)Next Screen"
            if maxpart% > 0% then pf16$ = hex(84) & "(16)Print Report"

            gosub'201(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  5% then       inputmode2
                  if keyhit%  = 16% and maxpart% > 0% then L30000
                  if keyhit% <>  0% then       L11081
               fieldnr% = cursor%(1) - 5%
               if fieldnr% < 1% or fieldnr% > 9% then L11081
                  pf5$ = hex(9c)
                  gosub'161(fieldnr%)
                        if enabled% =  0% then L11081
L11170:           gosub'201(fieldnr%)
                        if keyhit%  =  1% then gosub startover
                        if keyhit% <>  0% then       L11170
                  gosub'151(fieldnr%)
                        if errormsg$  <> " " then L11170
                  infomsg$ = " "
                  goto L11081

        REM *************************************************************~
            *     I N P U T   P A R T S   &   Q U A N T I T I E S       *~
            *                                                           *~
            * INPUTS THE PART NUMBER INFORMATION                        *~
            * AND EXITS WHEN A BLANK PART NUMBER IS ENTERED.            *~
            *************************************************************

        inputmode2

            if maxpart% > 0% then editpg2
            init(" ") part$(), quantity$()
            pf5$ = hex(8c)

            for screenline% = 1% to 20%
             if screenline% = 1% then pf16$ = hex(84) &"(16)Exit Program"~
                                 else pf16$ = hex(8c)
             if screenline% = 1% then pf4$ = hex(8c)&"(4)Previous Screen"~
                                 else pf4$ = hex(8c)&"(4)Previous Field"
                c% = screenline%
L12190:         for fieldnr% = 1% to 2%
L12200:             if screenline% = 1% and fieldnr% = 2%                ~
                                 then pf4$ = hex(8c)&"(4)Previous Field"
                    gosub'163(fieldnr%)
                          if enabled% = 0% then L12470
L12240:             gosub'203(fieldnr%)
                         if keyhit%  =  1% then gosub startover
                         if keyhit% = 4% and screenline% = 1%            ~
                                and fieldnr% = 1% then editpg1
                         if keyhit% <>  4% then L12370
                            if fieldnr% > 1% then L12320
                                screenline% = max(1%, screenline% - 1%)
                                fieldnr% = 2%  : c% = screenline%
                                goto L12200
L12320:                     fieldnr% = max(1%, fieldnr% - 1%)
                            if fieldnr% = 1% then L12190
                            gosub'163(fieldnr%)
                            if enabled% <> 0% then L12240
                            goto L12320
L12370:                  if keyhit% = 16% and screenline% = 1% then L65000
                         if keyhit% <>  0% then       L12240
                    gosub'153(fieldnr%)
                          if c% > 1% and part$(c%) = " " then editpg2
                          if c% <> 1% or part$(c%) <> " " then L12440
                               errormsg$ = "You must enter at least one"&~
                                         " Part Number"
L12440:                   if errormsg$ <> " " then L12240
                next fieldnr%
                if part$(c%) <> " " then maxpart% = c%
L12470:     next screenline%

        REM *************************************************************~
            *                E D I T   R E S P O N S E S                *~
            *                                                           *~
            * EDITS THE LINE ITEMS RESPONSES FOR THE ORDER ANALYSIS     *~
            *  PARAMETERS.                                              *~
            *************************************************************

         editpg2

            init(" ") errormsg$, inpmessage$, infomsg$
L13080:  inpmessage$ = "PRESS 16 to perform analysis ... move cursor to l~
        ~ine & RETURN to edit"
            pf16$ = hex(84) & "(16)Print Report"
            pf4$  = hex(84) & "(4)Previous Screen"

L13120:     gosub'203(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then       editpg1
                  if keyhit%  = 16% then       L30000
                  if keyhit% <>  0% then       L13120
            REM EDIT TABULAR DATA FIELDS.
                if cursor%(1) < 7% or cursor%(1) > 16% then L13120
                   f% = val(str(edttran$, cursor%(2)))
                   if f% = 0% then L13120
                   c%, screenline% =                                     ~
                           (2%*(cursor%(1)-7%)+1%) + int(cursor%(2)/44%)
                   c%, screenline% = min(maxpart%+1%, c%)
                   if f% = 2% and part$(c%) = " " then L13120
                   pf4$  = hex(84) & "(4)Previous Field"
                   pf16$ = hex(84)
                   for fieldnr% = f% to 2%
                      gosub'163(fieldnr%)
                            if enabled% = 0% then L13385
L13340:               gosub'203(fieldnr%)
                            if keyhit%  =  1% then gosub startover
                            if keyhit% <>  4% then L13360
                                fieldnr% = 1%
L13360:                     if keyhit% <>  0% then       L13340
                      gosub'153(fieldnr%)
                            if part$(c%) = " " then L13080
                            if errormsg$ <> " " then L13340
L13385:            next fieldnr%
                   if part$(c%) <> " " then maxpart% = max(maxpart%, c%)

            goto L13080

        REM *************************************************************~
            *      D E F A U L T / E N A B L E   F O R   I N P U T      *~
            *                                                           *~
            * SETS DEFAULTS AND INPUTS QUESTIONS FOR LINEAR INPUT.      *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 1
                  inpmessage$, infomsg$  = " "
                  on fieldnr% gosub L20100,         /* CUMULATIVE       */~
                                    L20200,         /* MAX # OF LEVELS  */~
                                    L20900,         /* Store Range      */~
                                    L20800,         /* BOM EFF. DATE    */~
                                    L20300,         /* SUMMARY REPORT   */~
                                    L20400,         /* ATC or QHand     */~
                                    L20500,         /* ATC DATE         */~
                                    L20600,         /* ATC HORIZON      */~
                                    L20950          /* Ignore Inv OH?   */
                     return
L20100:     REM DEFAULT/ENABLE FOR CUMULATIVE
                if cumulative$ = " " then cumulative$ = "NO "
                inpmessage$ = "Cumulative Assumes Parts Used For One Orde~
        ~r Are Not Available For Next"
                infomsg$ = "The SUMMARY REPORT option is incompatible wit~
        ~h CUMULATIVE = NO."
                return
L20200:     REM DEFAULT/ENABLE FOR MAXIMUM NUMBER OF LEVELS
                if maxlevels$ = " " then maxlevels$ = "05"
                inpmessage$ = "Only 15 Levels Can Be Printed, Enter 1 to ~
        ~15"
            return
L20300:     REM DEFAULT/ENABLE FOR PRINT SUMMARY REPORT
                if cumulative$ <> "NO " then L20317
                    enabled% = 0
                    summary$ = "NO "
                    return
L20317:         if summary$ = " " then summary$ = "YES"
                inpmessage$ = "Summary Report Shows Total Changes To Inve~
        ~ntory FROM Cumulative Analysis"
                return
L20400:     REM DEFAULT/ENABLE FOR USE ATC OR QTY ON HAND
                if atc_flag$ = " " then atc_flag$ = "NO"
                inpmessage$ = "Report Against ATC Instead Of Using Curren~
        ~t Quantity On Hand."
            return
L20500:     REM DEFAULT/ENABLE FOR ATC DATE FROM PLANNING CALENDAR
                if atc_flag$ <> "NO " then L20550
                    enabled% = 0%
                    atc_datef$ = " "
                    return
L20550:         if atc_datef$ = " " then atc_datef$ = date$
                inpmessage$ = "ATC Date to Use from the Planning Calendar~
        ~"
                return
L20600:     REM DEFAULT/ENABLE FOR ATC HORIZON
                if atc_flag$ <> "NO " then L20700
                    enabled% = 0%
                    atch_flag$ = " "
                    return
L20700:         if atch_flag$ = " " then atch_flag$ = "YES"
                inpmessage$ = "Use ATC Horizon from Part Master, Otherwis~
        ~e Look to the End of the Calendar"
                return
L20800:     REM DEFAULT/ENABLE FOR BOM EFFECTIVITY DATE
                if bom_date$ = " " then bom_datef$ = date$
                inpmessage$ = "BOM Effectivity Date"
                return


L20900:     REM DEFAULT/ENABLE FOR Store Range
                if fmstore$ = " " then fmstore$ = "ALL"
                inpmessage$ = "Enter Store Code Range"
                return

L20950:     REM DEFAULT/ENABLE FOR Ignore Inventory On Hand?
                if atc_flag$ = "NO " then L20955
                     enabled% = 0%
                     ignore$ = " "
                     return
L20955:         if ignore$ = " " then ignore$ = "NO"
                inpmessage$ = "Enter 'YES' to NOT consider On Hand Invent~
        ~ory for Top Level Parent Part."
                return

        REM *************************************************************~
            *      D E F A U L T / E N A B L E   F O R   P A R T S      *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES INPUT FOR THE PART NUMBER SCREEN*~
            * A FIELD IS DISABLED IF THE PREVIOUS PART FIELD IS BLANK.  *~
            *************************************************************

            deffn'163(fieldnr%)
                  enabled% = 0
                  inpmessage$ = " "
                  on fieldnr% gosub L21100,         /* PART NUMBER      */~
                                    L21200          /* QUANTITY TO BUILD*/
                     return
L21100:     REM DEFAULT/ENABLE FOR PART NUMBER
             inpmessage$ = "Enter the desired Part # or partial to see "&~
                "list. Leave blank to Edit."
                   enabled% = 1
                   return
L21200:     REM DEFAULT/ENABLE FOR QUANTITY TO BUILD
             inpmessage$ = "Enter the quantity to build for this Part"
                   enabled% = 1
                   return

L29000: REM *************************************************************~
            *             P R I N T   P A G E   Z E R O                 *~
            *************************************************************
                print page
                print using L35000, date$, time$, company$
                if atc_flag$ = "YES"                                     ~
                      then print using L35052, usrid$, page%              ~
                      else print using L35040, usrid$, page%
                print skip(3)
                for i% = 6% to 19%
                     print tab(20);
                     for j% = 1% to 80%
                          if str(i$(i%), j%, 1%) > hex(7f) or            ~
                             str(i$(i%), j%, 1%) < hex(20) then          ~
                               str(i$(i%), j%, 1%) = " "
                          print str(i$(i%), j%, 1);
                     next j%
                     print
                next i%
                print skip(2)
                for i% = 6% to 19%
                     print tab(20);
                     for j% = 1% to 80%
                          if str(i2$(i%), j%, 1%) > hex(7f) then         ~
                               str(i2$(i%), j%, 1%) = " "
                          print str(i2$(i%), j%, 1);
                     next j%
                     print
                next i%
                return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode1

L30000: REM *************************************************************~
            *        P E R F O R M   O R D E R   A N A L Y S I S        *~
            *                                                           *~
            * PERFORMS AN ORDER ANALYSIS FOR THE PART(S) IN QUESTION.   *~
            * DUMPS ALL THE INFORMATION INTO THE WORK FILE AND GOES     *~
            * FROM THERE.  SEE THE DOCUMENTATION FOR THE EXACT ALGORITHM*~
            * WE USE.                                                   *~
            *                                                           *~
            * ACTUALLY, IT'S KIND OF SIMPLE--THERE ARE JUST TWO CASES.  *~
            * 1.)  IF THE PART IS AN ATOM (NO COMPONENTS), THEN THE     *~
            *      COMMITTED FOR THIS PART GOES UP AND CAN EXCEED THE   *~
            *      QUANTITY ON HAND IN THE INVENTORY FILE.              *~
            * 2.)  IF THE PART IS NOT AN ATOM, THEN COMMIT THE MIN OF   *~
            *      THE AVAILABLE PARTS (ON HAND - PREVIOUSLY COMMITTED) *~
            *      (UP TO THE QUANTITY ON HAND) AND DIG IN THE BOM FOR  *~
            *      THE REST TIL WE END UP AT ATOMS AND CAN OVERCOMMIT.  *~
            *                                                           *~
            * THE COMMITTED IS HAULED UP FROM THE INVENTORY FILE AND    *~
            * PUT INTO THE WORKFILE WHEN THAT PART NUMBER IS FIRST      *~
            * REFERENCED.  THEN THAT FIELD IS UPDATED WHEN WE COMMIT    *~
            * ADDITIONAL PARTS.  SINCE THE QUANTITY ON HAND FIGURE IS   *~
            * COPIED INTO THE WORK FILE AS WELL, WE CAN COMPUTE         *~
            * AVAILABLE AND USE THAT TO DETERMINE WHETHER OR NOT TO DIG *~
            * DEEPER.  WHEN WE'RE DONE WITH THE PART(S), WE CAN DUMP    *~
            * THE CONTENTS OF THE WORK FILE TO PAPER, GIVING THE SUMMARY*~
            * REPORT INFORMATION.                                       *~
            *                                                           *~
            * NOTE THAT THE COMPUTER NEEDS TO COMMIT ALL THE AVAILABLE  *~
            * STUFF FOR THE PARENT PART NUMBERS SO THAT WE CAN COMMIT   *~
            * ONLY THE PARTS NEEDED FOR UNBUILT ASSEMBLIES.  BY         *~
            * COMMITTING THE PARENT PARTS WE CAN PREVENT A SCENARIO     *~
            * WHERE WE BUILD COMPONENT B AND THEN BUILD SOME A'S WHICH  *~
            * INCORPORATE COMPONENT B'S.                                *~
            *   TO ACCOMPLISH THIS SIZEABLE FEAT OF LEGERDEMAIN, WE     *~
            * DO A "PUT" OF A FAKE BOM RECORD WITH THE PARENT PART # AS *~
            * THE COMPONENT, A BLANK PRIMARY KEY, SO PLOWNEXT RETURNS   *~
            * WITH AN INVALID RESULT, AND DUMMY QUANTITY AND SIZE EQUAL *~
            * TO THE QUANTITY REQUIRED FOR THIS PARENT PART.  THEN THE  *~
            * BOM PROCESSOR IS ENLARGED TO 16 LEVELS, TO ACCOMMODATE    *~
            * THIS TRICK.                                               *~
            *************************************************************

            page% = 0
            call "SHOSTAT" ("Order Analysis in Process")
            call "WORKOPEN" (# 9, "IO   ", 1000%, f2%(9))

            select printer (134)
            call "SETPRNT" ("BOM006", " ", 0%, 0%)

            gosub L29000                 /* PRINT PAGE 0 SELECTION INFO */
            for part% = 1% to 20%
                if str(part$(part%),1,25) = " " then L30600
                   l% = 0% : type% = 0%
                   line% = 1000%
                   convert quantity$(part%) to qtyreqd
*                 GOSUB'199(PART$(PART%))
        put #5, using L30510, part$(part%), " ","001"," ", qtyreqd, 1, 0, ~
                             0, " ", " ", " "
L30510:            FMT CH(25), CH(25), CH(3),CH(3), 4*PD(14,4), CH(2),   ~
                       CH(1), CH(3)
                   gosub L36000           /* FORMAT HDRDESCR$           */
                   gosub'7(" ", 1)       /* START WITH FAKE RECORD.    */
                   bomid$(part%) = bomid$
                   REM PRINT TAG LINE FOR THIS PART'S OA.
                   if summary$ = "ONLY" then L30600
                       if tagprinted% <> 0 then L30600
                          gosub L32800    /* PAGE CONTROL SUBROUTINE    */
                          if tagprinted% <> 0 then L30600
                             print using L35120
                             tagprinted% = 1
L30600:         next part%
            goto L34000                   /* SUMMARY REPORT IF NEEDED   */

            deffn'7(part$, r(l%+1))
                  l% = l% + 1
                  if l% > 1% then gosub'190 (part$)


                  p$(l%) = str(part$,1,25) & str(bomid$,1,3)
                  count%(l%) = 0         /* SEQUENCE NUMBER FOR PRINT  */
                  if l% = 1 then L30710   /* HANDLE FAKE REC FOR LEV 1  */

L30690:           call "PLOWNEXT" (#5, p$(l%), 28%, f1%(5))
                       if f1%(5) = 0 then L30850
                       if str(p$(l%),29,3) = "  0" then L30690
L30710:           gosub L60000            /* LOAD BOM RECORD & INFO.    */
                  if mkr$ = "RE" or mkr$ = "SP" then L30690

                  gosub'10(component$)   /* LOAD COMPONENT INFORMATION */
                  needed = (r(l%) * ((quantity * xused) + over)) + fixed

                  REM CHECK TO SEE IF THIS PART HAS COMPONENTS OR NOT
                      if type% = 0% or type% >= 500% then component% = 1%~
                                                     else component% = 0%


                  count%(l%) = count%(l%) + 1
                  if component% = 0 or l% = maxlevels% + 1               ~
                     then gosub L32000              /* ATOM CASE        */~
                     else gosub L31000              /* (SUB)ASSEMBLY    */
                  goto L30690

L30850:           REM END ROUTINE GRACEFULLY.
                      l% = l% - 1
                      return

            REM ROUTINE TO EXTRACT THE BOM DESCRIPTION FROM TABLE
        deffn'199(test$)
            for i9% = 1% to bm%
                if test$ = bmkr$(i9%) then L30955
            next i9%
            mkrdescr$ = " " : return
L30955:     mkrdescr$ = bmkrdes$(i9%)
            return

*       -------------------------------------------------------------
L31000:     REM ROUTINE TO COMMIT PARTS FOR THOSE THAT ARE MANUFACTURED
                dig% = 0%
                if mkr$ = "TL" then L31050
                gosub'190(component$)
                     if bomid$ = hex(000000) then L31050
                if needed > avail then dig% = 1%
L31050:         build  = max(0, needed - avail)
                if mkr$ = "UU" then goto L31090
                if mkr$ = "PH" or mkr$ = "PA" then goto L31090
                manuf = manuf + build
L31090
*              IF MKR$ = "PH" OR MKR$ = "PA" THEN GOTO 31150
                committed = committed + needed

                rewrite #9, using L33390, component$, descr$, onhand,     ~
                              committed ,buy, manuf, mkr$, uom$, atch%,  ~
                              type$

                REM PRINT AN ENTRY.
                    init(" ") print$(), mkrdescr$

                    if mkr$ = "ST" or mkr$ = "TL" or mkr$ = "AS" or      ~
                       mkr$ = "NC" or mkr$ = "NP" or mkr$ = "BP" then    ~
                                                                    L31235
                         gosub'199(mkr$)
                         print$(3) = mkrdescr$
L31235:             call "CONVERT" (needed, 2.4, print$(1))
                    if abs(avail) < .01                                  ~
                       then print$(2) = " "                              ~
                       else call "CONVERT" (avail, 2.4, print$(2))
                    if mkr$ = "PA" or mkr$ = "PH" then L31370
                    if abs(build) < .01                                  ~
                       then print$(3) = " "                              ~
                       else call "CONVERT" (build, 2.4, print$(3))

                     if mkr$ <> "UU" then goto L31370

                     if needed = 0 then print$(1) = "SEE BELOW "
                     if needed = 0 then print$(2) = "  NONE    "

L31370:             gosub L32700          /* PRINT INDENTED ENTRY.      */
                    if bomid$ <> hex(000000) then L31440
                        descr$ = "* * *  NO EFFECTIVE BOM FOUND * * *"
                        gosub L32740
                           /* ODD, jumps into middle of print routine  */

                REM NOW DIG IF ANY TO BUILD FROM COMPONENTS.
L31440:             if dig% = 0 then return
                       gosub'7 (component$, build)
                       return

*       --------------------------------------------------------------
L32000:     REM ROUTINE TO COMMIT PARTS FOR THOSE THAT ARE PURCHASED
                committed = committed + needed
                buyy   = max(0, needed - avail)
                buy = buy + buyy

                rewrite #9, using L33390, component$, descr$, onhand,     ~
                          committed , buy, manuf, mkr$,  uom$, atch%,    ~
                          type$

                REM PRINT AN ENTRY FOR ATOMS.
                    init(" ") print$()
                    call "CONVERT" (needed, 2.4, print$(1))
                    if abs(avail) < .01                                  ~
                       then print$(2) = " "                              ~
                       else call "CONVERT" (avail, 2.4, print$(2))
                    REM PRINT$(3), BUILD FROM COMPONENTS, STAYS BLANK.
                    if abs(buyy) < .01                                   ~
                       then print$(4) = " "                              ~
                       else call "CONVERT" (buyy, 2.4, print$(4))
                    if mkr$ = "ST" or mkr$ = "AS" or mkr$ = "NC" or      ~
                                    mkr$ = "NP" or mkr$ = "BP" then L32210
                         gosub'199(mkr$)
                         print$(3) = mkrdescr$
L32210:             gosub L32700
                return

L32700:         REM ROUTINE THAT INDENTS, FORMATS ENTRIES, AND PRINTS.
                if summary$ = "ONLY" then return
                    prtpart$ = " "
                    put str(prtpart$, l% * 3 - 2, 30), using L32775,      ~
                        count%(l%), component$
                    if op$ <> "Y" then op$ = " "
                    gosub L32800
                    if l% = 1% and ignore$ = "YES" then print$(2%) =     ~
                                                               "  IGNORE"
                    print using L35240, prtpart$, op$, print$(1), uom$,   ~
                                       print$(2), print$(3), print$(4)
                    tagprinted% = 0
L32740:             prtpart$ = " "
                    put str(prtpart$, l% * 3 - 2, 37), using L32780,      ~
                        descr$
                    gosub L32800
                    print using L35240, prtpart$, " ", " ", " ", " ", " ",~
                                                 " "
                    tagprinted% = 0
                    return
L32775:                 %###. #########################
L32780:                 %     ################################

L32800:         REM PAGE CONTROL ROUTINE FOR THE ORDER ANALYSIS
                    line% = line% + 1
                    if line% < 60 then return
                       if page% = 0 then L32840
                          if tagprinted% <> 0 then L32840
                             print using L35120
                             tagprinted% = 1
L32840:                page% = page% + 1
                       print page
                       if atc_flag$ = "YES" then hdrdate$ = atc_datef$   ~
                                            else hdrdate$ = date$
                       print using L35000, date$, time$, company$
                       if atc_flag$ = "YES"                              ~
                            then print using L35052, usrid$, page%        ~
                            else print using L35040, usrid$, page%
                       print
                       if atc_flag$ = "YES" then L32876
                            print using L35080  : print using L35100
                            print
L32876:                convert str(hdrdescr$,9,3) to temp%
                       gosub'190(part$(temp%))
                       hdrdescr$ = hdrdescr$ & "  BOM: " & bomid$
                       print using L35060 , hdrdescr$ & "  FOR "          ~
                                           & hdrdate$
                       print using L35120
                       if atc_flag$ = "YES" then L32920
                            print using L35150
                            print using L35180
                            print using L35210
                            goto L32935
L32920:                print using L35280
                       print using L35310
                       print using L35340
L32935:                print using L35120
                       if atc_flag$ = "YES" then line% = 9%              ~
                                            else line% = 11%
                       return


        REM *************************************************************~
            *        ROUTINE TO LOAD COMPONENT PART INFORMATION.        *~
            *************************************************************

         deffn'10(part$)
L33020:         call "READ101" (#9, part$, f1%(9))
                     if f1%(9) = 0 then L33100      /* MAKE NEW WF ENTRY*/
              get #9, using L33390, part$, descr$, onhand, committed,     ~
                     buy, manuf, mkr$, uom$, atch%, type$
                if l% = 1% and ignore$ = "YES" then onhand = 0

                convert type$ to type%
                if cumulative$ = "NO " then avail = max(0, onhand) else  ~
                avail = max(0, onhand - committed)
                return

L33100:         REM IF NOT FOUND, CREATE NEW WORK FILE ENTRY FOR THIS P/N.
                    toh, buy, manuf, onhand, committed = 0
                    descr$ = "Non-Stocked Part"
                    uom$   = " "
                    type$  = "200"
                    p_atch%, atch% = 999%

                    call "READ100" (#4, part$, f1%(4))
                        if f1%(4) = 0% then L33330
                    get #4, using L33500, descr$, uom$, p_atch%, type$

                    convert type$ to type%

                    if atc_flag$ <> "YES" then L33210
                          if atch_flag$ = "YES" then atch% = p_atch%     ~
                                                else atch% = 999%
                          gosub'99(part$)
                          toh = qavail
                          goto L33330

L33210:             diskkey$ = str(part$,1,25) & " "

L33230:             call "PLOWNEXT" (#6, diskkey$, 25%,  f1%(6))
                    if f1%(6) <> 1% then L33330
                    get #6, using L33320, store$, onhand
                    if l% = 1% and ignore$ = "YES" then onhand = 0
                    if fmstore$ = "ALL" then L33260
                    if store$ < fmstore$ or store$ > tostore$ then L33230
L33260:             if str(store$,1,1) > "9" then goto L33330
                    if str(store$,1,1) < "0" then goto L33230
          REM ONLY STORES WITH NUMERIC ID'S CAN SUPPLY PARTS TO OUR PLANT
                    toh = toh + onhand
                    goto L33230

L33320:             FMT /*#6-HNYQUAN*/  POS(42), CH(3), POS(69), PD(14,4)
L33330:             write #9, using L33390, part$, descr$, toh   , 0 , 0, ~
                               0, mkr$, uom$, atch%, type$

                    goto L33020

*       ----------------  FORMAT FOR WORKFILE
L33390:         FMT CH(25),              /* PART NUMBER KEY TO FILE    */~
                    CH(32),              /* PART DESCRIPTION           */~
                    PD(14,4),            /* QUANTITY ON HAND           */~
                    PD(14,4),            /* QUANTITY COMMITTED         */~
                    PD(14,4),            /* QUANTITY TO BUY            */~
                    PD(14,4),            /* QUANTITY TO MANUFACTURE    */~
                    CH(2),               /* BOM MARKER                 */~
                    CH(4),               /* UNIT OF MEASURE            */~
                    BI(2),               /* ATC HORIZON                */~
                    CH(3)                /* PART TYPE                  */


*       ----------------  FORMAT FOR PARTS MASTER FILE
L33500:         FMT XX(25),              /* PART NUMBER KEY            */~
                    CH(32),              /* PART DESCRIPTION           */~
                    XX(16),              /* GENERIC X-REF              */~
                    CH(4),               /* UNIT OF MEASURE            */~
                    XX(50),              /* SKIP A BUNCH OF FIELDS     */~
                    BI(2),               /* ATC HORIZON                */~
                    POS(180),                                            ~
                    CH(3)                /* PART TYPE                  */



L34000:     REM PRINT THE SUMMARY REPORT IF IT'S NEEDED.
                if summary$ <> "YES" and summary$ <> "ONLY" then L34630

                readkey$ = " "
                line% = 1000%

L34030:         call "PLOWNEXT" (#9, readkey$, 0%, f1%(9))
                     if f1%(9) = 0 then L34165      /* TOTAL REPORT     */
           get #9, using L33390, part$, descr$, onhand, committed, buy,   ~
                     manuf, mkr$, uom$, atch%, type$

                init(" ") print$()

                if mkr$ = "ST" then L34085
                gosub'199(mkr$)
                     print$(5) = mkrdescr$

L34085:         if abs(onhand)    < .01 then L34095
                call "CONVERT" (onhand, 2.4, print$(1))
L34095:         if abs(committed) < .01 then L34105
                call "CONVERT" (committed, 2.4, print$(2))
L34105:         if abs(buy)       < .01 then L34115
                call "CONVERT" (buy, 2.4, print$(3))
L34115:         if abs(manuf)     < .01 then L34122
                call "CONVERT" (manuf, 2.4, print$(4))
L34122:         for i% = 1% to 25%
                     if part$(i%) = " " then L34130
                     if part$ = part$(i%) then print$(5%) = "*Analyzed*"
                     if part$ = part$(i%) then L34130
                 next i%

L34130:         gosub L34800              /* PAGE CONTROL SUBROUTINE    */
                print using L35530, part$, descr$, type$, uom$, print$(1),~
                               print$(2), print$(3), print$(4), print$(5)
                tagprinted% = 0
                init(" ") print$()
                goto L34030

L34165:         REM PRINT TAG LINE FOR REPORT, EXIT PROGRAM
                    if tagprinted% <> 0 then L34180
                    print using L35550
L34180:             print using L35640 : print using L35660
                    print using L35680
                    if summary$ <> "YES" and summary$ <> "ONLY" then L34630

           line% = 1000%
           for part% = 1% to 20%
           if part$(part%) = " " then goto   L34240
           gosub L34265
               call "DESCRIBE" (#4, part$(part%), descr$, 0%, f1%(4))
           convert part% to paa$, pic(####)
           print using L34340, paa$, part$(part%), descr$, bomid$(part%), ~
                              quantity$(part%)

L34240:    next part%
           print using L34350
           goto L34630

                REM PAGE CONTROL SUBROUTINE FOR DEMAND REPORT.
L34265:             line% = line% + 1%
                    if line% < 60% then return
                       page% = page% + 1%
                       print page
                       print using L35000, date$, time$, company$
                       print using L34325, usrid$, page%
                       print skip(1)
                       print using L34350
                       print using L34360
                       print using L34350
                       line% = 6%
                       return

L34325: %BY: ###                     O R D E R   I N   W H I C H   D E M ~
        ~A N D S   W E R E   A N A L Y Z E D                   PAGE: ###

L34340: % !   #### !#########################!###########################~
        ~#####!###!##########!
L34350: % +--------+-------------------------+---------------------------~
        ~-----+---+----------+
L34360: % ! DEMAND !  P A R T   N E E D E D  !     D E S C R I P T I O N ~
        ~     !BOM! QUANTITY !
L34370: %                             * * * * *  E N D   O F   R E P O R ~
        ~T  (@ ########) * * * * *


L34630:        anotherflag% = 1%
               call "FILEBGON" (#9)
               print skip(2)
               time$ = " " : call "TIME" (time$)
               print using L34370, time$
               close printer
               call "SETPRNT" ("BOM006", " ", 0%, 1%)
               goto inputmode1

L34800:         REM PAGE CONTROL SUBROUTINE FOR SUMMARY REPORT.
                    line% = line% + 1
                    if line% < 60 then return
                       page% = page% + 1
                       print page
                       print using L35000, date$, time$, company$
                       print using L35500, usrid$, page%
                       print
                       if atc_flag$ = "YES" then L34850
                            print using L35080  : print using L35100
                            print
L34850:                print using L35550
                       if atc_flag$ = "YES" then L34880
                            print using L35570
                            print using L35590
                            print using L35610
                            goto L34895
L34880:                print using L35730
                       print using L35750
                       print using L35770

L34895:                print using L35550
                       if atc_flag$ = "YES" then line% = 8%              ~
                                            else line% = 10%
                       return

L35000: %RUN DATE: ######## @ ########     ##############################~
        ~##############################                         BOMOA: BOM~
        ~006

L35040: %BY: ###                A B I L I T Y   T O   F I L L   D E M A N~
        ~ D S   F R O M   O N H A N D   I N V E N T O R Y        PAGE: ###

L35052: %BY: ###                A B I L I T Y   T O   F I L L   D E M A N~
        ~ D S   F R O M   A . T . C .   I N V E N T O R Y        PAGE: ###

L35060: %        ########################################################~
        ~#################################################################
L35080: %          CAUTION: THIS ANALYSIS ONLY CONSIDERS CURRENT ON-HAND ~
        ~INVENTORY WITHOUT REGARD FOR EXISTING COMMITMENTS
L35100: %                   PLEASE USE "CHECK FEASIBILITY" FROM PRODUCTIO~
        ~N PLANNING TO AVOID INTERFERENCE WITH CURRENT PLANS.
L35120: %+---------------------------------------------------------------~
        ~---------------+-+----------+-----+----------+----------+--------~
        ~--+
L35150: %!                                                               ~
        ~               !O! REQUIRED !     ! AVAIL IN !  BUILD   ! PROCURE~
        ~  !
        %!                                                               ~
        ~               !O! REQUIRED !     ! IGNORING !  BUILD   ! PROCURE~
        ~  !
L35180: %!                  P A R T   C O D E   /   D E S C R I P T I O N~
        ~               !P! FOR THIS ! UOM ! CURRENT  !  IN OUR  ! FROM TH~
        ~E !
L35210: %!                                                               ~
        ~               !?!  PART    !     ! ON-HAND  !  PLANT   ! OUTSIDE~
        ~  !
L35240: %!###############################################################~
        ~###############!#!##########!#####!##########!##########!########~
        ~##!
        REM --------------  REPORT COLUMN HEADING FOR ATC
L35280: %!                                                               ~
        ~               !O! REQUIRED !     ! AVAILABLE!  BUILD   ! PROCURE~
        ~  !
L35310: %!                  P A R T   C O D E   /   D E S C R I P T I O N~
        ~               !P! FOR THIS ! UOM !    TO    !  IN OUR  ! FROM TH~
        ~E !
L35340: %!                                                               ~
        ~               !?!  PART    !     !  COMMIT  !  PLANT   ! OUTSIDE~
        ~  !

L35500: %BY: ###                S U M M A R Y   O F   A C T I O N S   R E~
        ~ Q U I R E D   F R O M   T H I S   A N A L Y S I S      PAGE: ###

L35530: %!#########################!################################!####~
        ~!#####!##########!##########!##########!##########!##########!
L35550: %+-------------------------+--------------------------------+----~
        ~+-----+----------+----------+----------+----------+----------+
L35570: %!                         !                                !    ~
        ~!     ! ON  HAND !  NEEDED  !  NEED    !   NEED   ! SPECIAL  !
L35590: %!  P A R T   N U M B E R  !     D E S C R I P T I O N      !PART~
        ~! UOM !  BEFORE  !  DURING  !   TO     !    TO    ! PART USE !
L35610: %!                         !                                !TYPE~
        ~!     ! ANALYSIS ! ANALYSIS !  BUY     !   BUILD  !  NOTES   !

L35640: %  COMPONENT PARTS OR ASSEMBLIES MAY BE USED DIFFERENTLY IN DIFFE~
        ~RENT BILL STRUCTURES.  THE 'SPECIAL PART USE NOTES'
L35660: %  INDICATE ANY NON-STANDARD USE ENCOUNTERED.  PARTS MARKED TO HA~
        ~VE A NON-STANDARD USE WERE SO SPECIFIED IN AT LEAST
L35680: %  ONE BOM.




L35730: %!                         !                                !    ~
        ~!     !  A.T.C.  !  NEEDED  !  NEED    !   NEED   ! SPECIAL  !
L35750: %!  P A R T   N U M B E R  !     D E S C R I P T I O N      !PART~
        ~! UOM !  BEFORE  !  DURING  !   TO     !    TO    ! PART USE !
L35770: %!                         !                                !TYPE~
        ~!     ! ANALYSIS ! ANALYSIS !  BUY     !   BUILD  !  NOTES   !


L36000:     REM FORMATS HDRDESCR$, WHICH TELLS HOW MANY OF WHAT WE BUILDIN
                readkey$ = " "
                call "DESCRIBE" (#4, part$(part%), readkey$, 1%, f1%(4))
                     if f1%(4) = 0 then readkey$ = "(PART NOT ON FILE)"
           convert part% to demnum$, pic(###)
            hdrdescr$ = "DEMAND: "& str(demnum$,1,3) & "  QTY: "  &      ~
            quantity$(part%) & "  PART: "   & part$(part%) & " " &       ~
            readkey$
                return

        REM *************************************************************~
            *        G E T   A T C   F R O M   P I P   M A S T E R      *~
            *                                                           *~
            * LOAD A.T.C. IF REQUESTED BY USER TO USE INSTEAD OF QTY    *~
            * ON HAND IN THE ORDER ANALYSIS.                            *~
            *************************************************************

         deffn'99(thispart$)

            mat avail% = zer  :  mat atc% = zer

            if f2%(7) = 1% then                                          ~
                 call "OPENFILE" (#7, "SHARE", f2%(7), rslt$(7), axd$(7))

            call "READ100" (#7, thispart$, f1%(7))
                if f1%(7) <> 0% then L37150
                     errormsg$ = "PART NOT FOUND IN PIP!"
                     return

L37150:     get #7, using L37500, avail%()

            mat atc% = avail%
            for zz% = min(489%, atc_today% + atch%)  to  atc_today%      ~
                                                                 step -1%
                atc%(zz%) = min(avail%(zz%), atc%(zz% + 1%))
            next zz%

            qavail = atc%(atc_today%)
            return

L37500:     FMT XX(26), 490*BI(4)


        REM *************************************************************~
            *             A N S W E R   Q U E S T I O N S               *~
            *                                                           *~
            * ANSWER QUESTIONS ABOUT ORDER ANALYSIS OPTIONS             *~
            * THE ORDER ANALYSIS BIT.                                   *~
            *************************************************************

            deffn'201(fieldnr%)
                  init(hex(84)) lfac$()
                  if fieldnr% <> 0% then L40090
                  init(hex(86)) lfac$()
L40090:           on fieldnr% gosub L40170,         /* CUMULATIVE?      */~
                                    L40200,         /* MAXIMUM # LEVELS */~
                                    L40170,         /* Store Range      */~
                                    L40170,         /* BOM EFF. DATE    */~
                                    L40170,         /* PRINT SUMMARY?   */~
                                    L40170,         /* ATC or Qty OnHand*/~
                                    L40170,         /* Date for ATC     */~
                                    L40170,         /* ATC Horizon?     */~
                                    L40170          /* Ignore Inv OH?   */
                     goto L40400

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40200:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40400:     accept                                                       ~
               at (01,02),                                               ~
               "Analyze Delivery Feasibility of Orders",                 ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(84)), infomsg$               , ch(79),~
               at (06,02),                                               ~
                  "Perform Cumulative Analysis?",                        ~
               at (06,40), fac(lfac$( 1)), cumulative$          , ch(03),~
               at (07,02),                                               ~
                  "Maximum Number of Levels",                            ~
               at (07,40), fac(lfac$( 2)), maxlevels$           , ch(02),~
               at (08,02),                                               ~
                  "Store Code Range",                                    ~
               at (08,40), fac(lfac$( 3)), fmstore$             , ch(03),~
               at (08,45), "To",                                         ~
               at (08,49), fac(lfac$( 3)), tostore$             , ch(03),~
               at (09,02),                                               ~
                  "BOM Effectivity Date",                                ~
               at (09,40), fac(lfac$( 4)), bom_datef$           , ch(08),~
               at (10,02),                                               ~
                  "Print Summary Report? (YES, NO, ONLY)",               ~
               at (10,40), fac(lfac$( 5)), summary$             , ch(04),~
               at (11,02),                                               ~
                  "Use ATC instead of Quantity on Hand?",                ~
               at (11,40), fac(lfac$( 6)), atc_flag$            , ch(03),~
               at (12,02),                                               ~
                  "ATC Date from Planning Calendar",                     ~
               at (12,40), fac(lfac$( 7)), atc_datef$           , ch(08),~
               at (13,02),                                               ~
                  "Use ATC Horizon from Part Master?",                   ~
               at (13,40), fac(lfac$( 8)), atch_flag$           , ch(03),~
               at (14,02),                                               ~
                  "Ignore Parent's Inventory On Hand?",                  ~
               at (14,40), fac(lfac$( 9)), ignore$              , ch(03),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,30), fac(str(pf4$,,1)), str(pf4$,2,18)    , ch(18),~
               at (22,64),                                               ~
                  "(13)Instructions",                                    ~
               at (23,30), fac(str(pf5$,,1)), str(pf5$,2,16)    , ch(16),~
               at (23,64),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,64), fac(str(pf16$,,1)), str(pf16$,2,16)  , ch(16),~
                                                                         ~
                                                                         ~
               keys(hex(000104050d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L41090
                  call "MANUAL" ("BOMOA")
                  goto L40400

L41090:        if keyhit% <> 15 then L41130
                  call "PRNTSCRN"
                  goto L40400

L41130:        REM GET CURSOR LOCATION FOR EDIT MODE COMPUTATIONS.
                   close ws
                   call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
                   return

        REM *************************************************************~
            *                     I N P U T   P A R T S                 *~
            *                                                           *~
            * INPUT PART NUMBER(S) TO PROCESS FOR THE ORDER ANALYSIS BIT*~
            *************************************************************

            deffn'203(fieldnr%)
                  init(hex(84)) fac$()
                  if fieldnr% <> 0% then L42080
                  init(hex(86)) str(fac$(),1,2%*maxpart%+1%)
L42080:           on fieldnr% gosub L42150,         /* PART NUMBER      */~
                                    L42180          /* QUANTITY TO MAKE */
                     goto L42220

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(screenline%, fieldnr%) = hex(80)
                      return
L42150:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(screenline%, fieldnr%) = hex(81)
                      return
L42180:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      fac$(screenline%, fieldnr%) = hex(82)
                      return

L42220:     accept                                                       ~
               at (01,02),                                               ~
               "Analyze Delivery Feasibility of Orders",                 ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(84)), infomsg$               , ch(79),~
                                                                         ~
               at (06,02), fac(hex(ac)),   line10$              , ch(79),~
               at (07,02), fac(fac$( 1,1)), part$    ( 1)       , ch(25),~
               at (07,28), fac(fac$( 1,2)), quantity$( 1)       , ch(10),~
               at (07,45), fac(fac$( 2,1)), part$    ( 2)       , ch(25),~
               at (07,71), fac(fac$( 2,2)), quantity$( 2)       , ch(10),~
               at (08,02), fac(fac$( 3,1)), part$    ( 3)       , ch(25),~
               at (08,28), fac(fac$( 3,2)), quantity$( 3)       , ch(10),~
               at (08,45), fac(fac$( 4,1)), part$    ( 4)       , ch(25),~
               at (08,71), fac(fac$( 4,2)), quantity$( 4)       , ch(10),~
               at (09,02), fac(fac$( 5,1)), part$    ( 5)       , ch(25),~
               at (09,28), fac(fac$( 5,2)), quantity$( 5)       , ch(10),~
               at (09,45), fac(fac$( 6,1)), part$    ( 6)       , ch(25),~
               at (09,71), fac(fac$( 6,2)), quantity$( 6)       , ch(10),~
               at (10,02), fac(fac$( 7,1)), part$    ( 7)       , ch(25),~
               at (10,28), fac(fac$( 7,2)), quantity$( 7)       , ch(10),~
               at (10,45), fac(fac$( 8,1)), part$    ( 8)       , ch(25),~
               at (10,71), fac(fac$( 8,2)), quantity$( 8)       , ch(10),~
               at (11,02), fac(fac$( 9,1)), part$    ( 9)       , ch(25),~
               at (11,28), fac(fac$( 9,2)), quantity$( 9)       , ch(10),~
               at (11,45), fac(fac$(10,1)), part$    (10)       , ch(25),~
               at (11,71), fac(fac$(10,2)), quantity$(10)       , ch(10),~
               at (12,02), fac(fac$(11,1)), part$    (11)       , ch(25),~
               at (12,28), fac(fac$(11,2)), quantity$(11)       , ch(10),~
               at (12,45), fac(fac$(12,1)), part$    (12)       , ch(25),~
               at (12,71), fac(fac$(12,2)), quantity$(12)       , ch(10),~
               at (13,02), fac(fac$(13,1)), part$    (13)       , ch(25),~
               at (13,28), fac(fac$(13,2)), quantity$(13)       , ch(10),~
               at (13,45), fac(fac$(14,1)), part$    (14)       , ch(25),~
               at (13,71), fac(fac$(14,2)), quantity$(14)       , ch(10),~
               at (14,02), fac(fac$(15,1)), part$    (15)       , ch(25),~
               at (14,28), fac(fac$(15,2)), quantity$(15)       , ch(10),~
               at (14,45), fac(fac$(16,1)), part$    (16)       , ch(25),~
               at (14,71), fac(fac$(16,2)), quantity$(16)       , ch(10),~
               at (15,02), fac(fac$(17,1)), part$    (17)       , ch(25),~
               at (15,28), fac(fac$(17,2)), quantity$(17)       , ch(10),~
               at (15,45), fac(fac$(18,1)), part$    (18)       , ch(25),~
               at (15,71), fac(fac$(18,2)), quantity$(18)       , ch(10),~
               at (16,02), fac(fac$(19,1)), part$    (19)       , ch(25),~
               at (16,28), fac(fac$(19,2)), quantity$(19)       , ch(10),~
               at (16,45), fac(fac$(20,1)), part$    (20)       , ch(25),~
               at (16,71), fac(fac$(20,2)), quantity$(20)       , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,30), fac(str(pf4$,,1)), str(pf4$,2,18)    , ch(18),~
               at (22,64),                                               ~
                  "(13)Instructions",                                    ~
               at (23,64),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,64), fac(str(pf16$,,1)), str(pf16$,2,16)  , ch(16),~
                                                                         ~
                                                                         ~
               keys(hex(0001040d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L43400
                  call "MANUAL" ("BOMOA")
                  goto L42220

L43400:        if keyhit% <> 15 then L43440
                  call "PRNTSCRN"
                  goto L42220

L43440:        REM GET CURSOR LOCATION FOR EDIT MODE COMPUTATIONS.
                   close ws
                   call "SCREEN" addr ("C", 0%, "I", i2$(), cursor%())
                   return

        REM *************************************************************~
            *    T E S T   D A T A   F O R   L I N E A R   I N P U T    *~
            *                                                           *~
            * TESTS DATA FOR LINEAR INPUT MODE.  CHECK THE PARAMETERS   *~
            * TO SEE THAT THEY ALL LINE UP.                             *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* CUMULATIVE?      */~
                                    L50200,         /* MAX # OF LEVELS  */~
                                    L50930,         /* Store Code Range */~
                                    L50800,         /* BOM EFF. DATE    */~
                                    L50300,         /* SUMMARY REPORT   */~
                                    L50400,         /* ATC?             */~
                                    L50500,         /* ATC DATE         */~
                                    L50700,         /* ATC HORIZON      */~
                                    L50960          /* Ignore Inv OH?   */
                 return
L50100:     REM TEST DATA FOR CUMULATIVE? FLAG
                if str(cumulative$, 1, 1) = "Y" then cumulative$ = "YES" ~
                                                else cumulative$ = "NO"
                if cumulative$ = "NO" then summary$ = "NO"
                return
L50200:     REM TEST DATA FOR MAXIMUM NUMBER OF LEVELS
                if maxlevels$ = " " then maxlevels$ = "15"
                convert maxlevels$ to maxlevels%, data goto L50230
                if maxlevels% < 1% or maxlevels% > 15% then L50241
                convert maxlevels% to maxlevels$, pic(##)
                return
L50230:            errormsg$ = "Illegal Entry For Maximum Number Of Level~
        ~s: " & maxlevels$
                   return
L50241:            errormsg$ = "Maximum Number Of Levels Must Be Between ~
        ~1 And 15: " & maxlevels$
                   return

L50300:     REM TEST DATA FOR PRINT SUMMARY REPORT?
                if str(summary$,1%,1%) = "Y" then summary$ = "YES"
                if str(summary$,1%,1%) = "N" then summary$ = "NO"
                if str(summary$,1%,1%) = "O" then summary$ = "ONLY"

                if cumulative$ = "NO" and summary$ <> "NO" then L50370
                if summary$ = "YES" or summary$ = "NO" or                ~
                   summary$ = "ONLY" then return
            errormsg$ = "Summary Report Must be 'YES', 'NO', or 'ONLY'."
            return
L50370:     errormsg$ = "Cannot Have Summary Report if Cumulative is 'NO'"
            return

L50400:     REM TEST DATA FOR ATC OR QTY ON HAND?
                if str(atc_flag$, 1, 1) = "Y" then atc_flag$ = "YES"     ~
                                              else atc_flag$ = "NO"
                if atc_flag$ = "YES" then ignore$ = " "
                return
L50500:    REM  TEST DATA FOR ATC DATE
                call "DATEOK" (atc_datef$, ret%, errormsg$)
                if errormsg$ <> " " then atc_datef$ = date$
                if errormsg$ <> " " then return
                     atc_date$ = atc_datef$
                     call "DATUNFMT" (atc_date$)
                     ret% = 0%
                call "PIPINDEX" (#34, atc_date$, atc_today%, ret%)
                     if ret% <> 1% then return
                errormsg$ = "Planning Calander Base Date not Found.  Exit~
        ~ & Correct."
                return
L50700:     REM TEST DATA FOR ATC HORIZON FLAG
                if str(atch_flag$, 1, 1) = "Y" then atch_flag$ = "YES"   ~
                                               else atch_flag$ = "NO"
                return

L50800:    REM  TEST DATA FOR BOM EFFECTIVITY DATE
                call "DATEOK" (bom_datef$, ret%, errormsg$)
                if errormsg$ <> " " then bom_datef$ = date$
                if errormsg$ <> " " then return
                     bom_date$ = bom_datef$
                     call "DATUNFMT" (bom_date$)
                     ret% = 0%
                call "PIPINDEX" (#34, bom_date$, bom_today%, ret%)
                     if ret% <> 1% then return
                errormsg$ = "Planning Calander Base Date not Found.  Exit~
        ~ & Correct."
                return

L50930:    REM TEST FOR Store Range
                call "TESTRNGE" (fmstore$, tostore$, lostore$, histore$, ~
                                 errormsg$, #03)
                return

L50960:    REM TEST FOR Ignore Inventory On Hand?
                if atc_flag$ <> "YES" then L50975
                     ignore$ = " " : return
L50975:         if str(ignore$,1%,1%) = "Y" then ignore$ = "YES"         ~
                                            else ignore$ = "NO "
                return

        REM *************************************************************~
            *      T E S T   T A B U L A R   I N F O R M A T I O N      *~
            *                                                           *~
            * TESTS TABULAR INFORMATION TO MAKE SURE THAT THE DATA IS   *~
            * OK.  ZAP QUANTITY IF BLANK PART NUMBER ENTERED.           *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51100,         /* PART NUMBER      */~
                                    L51200          /* QUANTITY TO BUILD*/
                     return
L51100:     REM TEST DATA FOR PART NUMBER
                if part$(c%) <> " " then L51120
                   quantity$(c%) = " "
                   goto L51500
L51120:         call "GETCODE" (#4, part$(c%), infomsg$, 1%, 0, f1%(4))
                     if f1%(4) = 0 then L51180
                gosub'190 (part$(c%))
                readkey$ = str(part$(c%),1,25) & str(bomid$,1,3) & "  0"
                call "READ100" (#5, readkey$, f1%(5))
                     if f1%(5) = 0 then L51190
        REM     IF QUANTITY$(C%) <> " " THEN RETURN
        REM        FIELDNR% = 2%
        REM        ERRORMSG$ = HEX(00)
                   return
L51180:         errormsg$ = "Part Number Not On File: " & part$(c%)
                return
L51190:         errormsg$ = "Part Has No Components Under BOM: " & bomid$
                return
L51200:     REM TEST DATA FOR QUANTITY TO BUILD
                if part$(c%) = " " then L51310
                convert quantity$(c%) to quantity, data goto L51245
                if quantity < 0 then L51255
                call  "CONVERT" (quantity, 2.2, quantity$(c%))
                return

L51245:         errormsg$ = "Invalid Entry For Quantity: " & quantity$(c%)
                return
L51255:         errormsg$ = "Quantity Must Be Greater Than Zero: "       ~
                                                          & quantity$(c%)
                return
L51310:         errormsg$ = "Cannot have Quantity Without part number!"
                return

L51500: REM CLEAN UP STACK
            if c% > maxpart% then return
            if c% = maxpart% then L51580
            if maxpart% < 2% then L51580
            for i% = c% to maxpart% - 1%
                part$    (i%) = part$    (i%+1%)
                quantity$(i%) = quantity$(i%+1%)
            next i%

L51580:     maxpart% = max(maxpart% - 1%, 0%)
            init (" ") part$(maxpart%+1%), quantity$(maxpart%+1%)
            return

L60000: REM *************************************************************~
            * L O A D   B I L L   O F   M A T E R I A L S   R E C O R D *~
            *                                                           *~
            * LOADS THE BILL OF MATERIALS RECORD FROM DISK.             *~
            *************************************************************

            get #5, using L60100, component$, bomid$, quantity, xused,    ~
                    fixed, over, mkr$, op$, hard_bom$
            return

L60100:     FMT CH(25),                  /* COMPONENT PART NUMBER      */~
                XX(25),                  /* ASSEMBLY PART NUMBER       */~
                CH(3),                   /* WHICH BOM                  */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                PD(14,4),                /* QUANTITY REQUIRED          */~
                PD(14,4),                /* TIMES USED (SIZE)          */~
                PD(14,4),                /* FIXED QUANTITY             */~
                PD(14,4),                /* ADDED OVERAGE              */~
                CH(2),                   /* BOM MARKER                 */~
                CH(1),                   /* OPTION FLAG                */~
                CH(3)                    /* HARD PEG BOM ID            */


        REM *************************************************************~
            *      G E T   E F F E C T I V E   B O M                    *~
            *                                                           *~
            * GETS THE CURRENT BOM FOR PASSED PART BASED ON DATEINDEX%  *~
            *************************************************************

            deffn'190(part22$)
                 bomid$ = " "
                 if type% > 0% and type% < 500% then return
                                                      /* PURCHASED PART */
                 bomid$ = hex(000000)                 /* SET FOR ERROR */
*               IF PART22$ = PART$(PART%) 61200
                 if type% > 789% and type% < 800% then return /* TOOL  */

                 if str(hard_bom$,1,1) <=  " " then L61280
                     bomid$ = hard_bom$
                     return
L61280:          if bom_today% = 0 then return   /* OFF PROD CALENDER? */
                    readkey2$ = str(part22$,,25) & "1"
                    call "READ102" (#24, readkey2$, f1%(24))
                       if f1%(24) <> 1 then return
                    get #24, using L61330, readkey2$, bom$()
L61330:                FMT CH(29), 490 * CH(3)
                    if str(readkey2$,,25) <> str(part22$,,25) then return
                    bomid$ = bom$(bom_today%)
                    if bomid$ = " " then bomid$ = hex(000000) /* ERROR */

           return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("Closing Files, One Moment Please")
            end
