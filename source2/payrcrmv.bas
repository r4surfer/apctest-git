         rem*************************************************************~
            *                                                           *~
            *  pppp    aaa   y   y  rrrr    ccc   rrrr   m   m  v   v   *~
            *  p   p  a   a  y   y  r   r  c   c  r   r  mm mm  v   v   *~
            *  pppp   aaaaa   yyy   rrrr   c      rrrr   m m m  v   v   *~
            *  p      a   a    y    r   r  c   c  r   r  m   m   v v    *~
            *  p      a   a    y    r   r   ccc   r   r  m   m    v     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * payrcrmv - creates payables invoices from recurring       *~
            *            control (dummy) invoices.  invoice dates are   *~
            *            defaulted same as in payinput. invoices        *~
            *            selection is based on 'cutover group' codes    *~
            *            rather then predetermined dates, putting more  *~
            *            responsibility on the user, but making process *~
            *            simple.                                        *~
            *-----------------------------------------------------------*~
            *                  m o d i f i c a t i o n s                *~
            *---when---+------------------what--------------------+-who-*~
            * 05/28/86 ! original                                 ! hes *~
            * 05/18/87 ! paybuf2 & paylines mods- std costs       ! jim *~
            * 09/30/88 ! now getting last vendor in range.        ! jdh *~
            * 06/12/91 ! prr 12015 modified so that users can     ! sid *~
            *          !     process a vendor with 2 or more      !     *~
            *          !     recurring invoice groups.            !     *~
            * 06/13/91 ! added call 'ALLFREE'                     ! sid *~
            * 04/01/94 ! condition for background posting         ! kab *~
            * 08/15/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cursor%(2),                  /* CURSOR LOACATIONS FOR EDIT */~
            date$8,                      /* SCREEN DATE STRING STUFF   */~
            date1$8,                     /* DATE FOR PAY DATE COMPUTING*/~
            date2$8,                     /* ANOTHER DATE...            */~
            datetime$7,                  /* DATE TIME STAMP            */~
            defstr$3,                    /* DEFAULT STORE NUMBR INPUT  */~
            descr$(5)32,                 /* Cutover Group Descriptions */~
            discdate$6,                  /* PAY TAKING DISCOUNT DATE   */~
            errormsg$79,                 /* ERROR MESSAGE TEXT LINE    */~
            group$(5)4,                  /* Cutover Groups             */~
            head$(50)7,                  /* Invoice Header Record      */~
            header$79,                   /* Screen Title               */~
            i$(24)80,                    /* JUNK SCREEN IMAGE (NOT USED*/~
            ldate$(5)8,                  /* Last Cutover Dates         */~
            lfac$(20)1,                  /* LINEAR INPUT FAC'S         */~
            line_a$250, line_b$250, line_c$41,  /* Invoice line item   */~
            manual$8,                    /* For call to 'MANUAL'       */~
            marker$(5)1,                 /* For update section         */~
            message$79,                  /* INPUT MESSAGE              */~
            paydate$8,                   /* PAYABLES DATE INFORMATION  */~
            pfdescr$(3)79,               /* FUNCTION KEYS ENABLED LISTS*/~
            pfkeys$32,                   /* FUNCTION KEYS ENABLED LISTS*/~
            postdate$6,                  /* PAYABLES DATE INFORMATION  */~
            readkey$90,                  /* KEY FOR PLOW ROUTINES      */~
            readkey1$90,                 /* KEY FOR PLOW ROUTINES      */~
            regulardate$6,               /* REGULAR DATE INFORMATION   */~
            search%(2),                  /* GUESS                      */~
            subheader$56,                /* Screen Title               */~
            sysacct$(5)9,                /* SYSTEM DEFAULT ACCOUNTS    */~
            tdate$8,                     /* Temporary Date Variable    */~
            userid$3,                    /* USERID THIS USER           */~
            vendor$(2)9                  /* VENDOR RANGE               */

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
            *FILE#!  PRNAME  !          D E S C R I P T I O N           *~
            *-----+----------+------------------------------------------*~
            * # 1 ! USERINFO ! USER INFORMATION (PAYABLES DATE)         *~
            * # 3 ! VENDOR   ! LOAD VENDOR MASTER INFORMATION HERE      *~
            * # 5 ! PAYMASTR ! PAYABLES MAIN HEADER FILE.               *~
            * # 6 ! PAYLINES ! PAYABLES INVOICE LINE ITEM FILE          *~
            * # 7 ! SYSFILE2 ! SYSTEM INFO (DEFAULT PAY DATES)          *~
            * # 9 ! PAYBUFFR ! PAYABLES INVOICE HEADER BUFFER           *~
            * #10 ! PAYBUF2  ! PAYABLES INVOICE LINE ITEM BUFFER.       *~
            * #20 ! GENCODES ! General Purpose Code File                *~
            *************************************************************

            select # 1, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

            select #3,  "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos=1, keylen=9,                              ~
                        alt key 1, keypos = 10, keylen = 30, dup

            select  #5, "PAYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos = 1, keylen = 25

            select  #6, "PAYLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 541,                                   ~
                        keypos = 36, keylen = 28,                        ~
                        alternate key 1, keypos = 1, keylen = 63

            select #7,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #9,  "PAYBUFFR",      /* INVOICE HEADER BUFFER      */~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos = 1, keylen = 10,                         ~
                        alternate key 1, keypos = 11, keylen = 25

            select #10, "PAYBUF2",       /* INVOICE LINE ITEM BUFFER   */~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 541,                                   ~
                        keypos = 36, keylen = 28,                        ~
                        alternate key 1, keypos = 1, keylen = 63

            select #20, "GENCODES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos = 1, keylen = 24

            call "SHOSTAT"  ("Opening Files, One Moment Please.")

            call "OPENCHCK" (#1, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#3, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#5, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#6, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#7, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#9, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#10, 0%, 0%, 200%, " ")
            call "OPENCHCK" (#20, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                 I N I T I A L I Z A T I O N               *~
            *                                                           *~
            * SETS USER ID, COMPUTES BUFFERSEQNR%, AND THAT SORT OF     *~
            * THING.                                                    *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "PAYBKCTL" (#9, ret%)
               if ret% <> 0% then end 0%

            date$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)
            manual$ = "PAYRCRMV"
            subheader$ = "     Group Description                      Las~
        ~t Cutover"

            REM Get Users A/P Posting Date...
            call "READ100" (#1, userid$, f1%(1))
                if f1%(1) = 1 then L09220
                call "ASKUSER" (keyhit%, "Sorry",                        ~
                  "You're Not Listed As A Valid User In This Data Base", ~
                                         " ",  "Press (RETURN) To Exit.")
                goto L65000

L09220:     get #1, using L09230, postdate$, defstr$
L09230:         FMT XX(9), CH(6), XX(48), CH(3)

            REM Validate Users Posting Date...
                call "WHICHMON" (#7, postdate$, this%)
                  if this% <> 0 then L09330
                call "ASKUSER" (keyhit%, "Sorry",                        ~
                      "Your Posting Date Is Outside The Posting Window", ~
                                         " ",  "Press (RETURN) To Exit.")
                goto L65000

L09330:         paydate$ = postdate$
                call "DATEFMT" (paydate$)

            REM Get A/P System Defaults...
                sysbillsdue%, sysdiscsdue% = 30
                call "READ100" (#7, "MODULE.DEFAULTS.AP  ", f1%(7))
                     if f1%(7) = 0 then L09430
                get #7, using L09410,sysbillsdue%, sysdiscsdue%, sysacct$()
L09410:         FMT XX(20), 2*BI(4), XX(8), 5*CH(9)

L09430:     REM Insure Buffer Is Clear For This Guy (Control Measure)...
                readkey$ = all(hex(00))
                str(readkey$,,3) = userid$
                call "PLOWNEXT" (#9, readkey$, 3%, f1%(9))
                     if f1%(9) = 0 then L10000
                     call "ASKUSER" (keyhit%, "Sorry",                   ~
                         "You Must Clear The A/P Invoice Buffer Of Your",~
                         "Invoices Before This Function Can Be Used.",   ~
                                               "Press (RETURN) To Exit.")
                     call "SHOSTAT" ("One Moment Please")
                     end 0%

L10000: REM *************************************************************~
            *     I N P U T   I N V O I C E   H E A D E R   I N F O     *~
            *                                                           *~
            * GETS INVOICE HEADER INFORMATION AND THAT SORT OF THING.   *~
            *************************************************************

        inputmode
            vendor$(), group$(), descr$(), ldate$(), errormsg$ = " "
            call "ALLFREE"

            for fieldnr% = 1 to 6
                gosub'161(fieldnr%)
                      if enabled% = 0 then L10110
L10065:         gosub'201(fieldnr%)
L10070:               if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then L10100
                         fieldnr% = max(1%, fieldnr%-1%)
                         gosub'161(fieldnr%)
                         if enabled% = 0 then L10070
                         goto L10065
L10100:               if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10065
L10110:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10065
                next fieldnr%

        REM *************************************************************~
            *           E D I T   I N V O I C E   H E A D E R           *~
            *                                                           *~
            * EDITS INVOICE HEADERS, PERMITTING ALL OF THE FIELDS TO BE *~
            * MODIFIED.                                                 *~
            *************************************************************

        editmode
            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press RETURN."
            gosub'211(0%)
                 if keyhit%  =  1 then gosub startover
                 if keyhit%  = 16 then       datasave
                 if keyhit% <>  0 then       editmode
            oldfieldnr% = 0
L11150:     fieldnr% = cursor%(1) - 5
            if fieldnr% = 1% then L11200
            if fieldnr% < 6% or fieldnr% > 10% then editmode
            fieldnr% = fieldnr% - 4%

L11200:     if fieldnr% = oldfieldnr% then editmode
            oldfieldnr% = fieldnr%
            gosub'161(fieldnr%)
                 if enabled% = 0 then editmode
L11240:     gosub'211(fieldnr%)
                 if keyhit%  =  1 then gosub startover
                 if keyhit% <>  0 then       L11240
            gosub'151(fieldnr%)
                 if errormsg$ <> " " then L11240
            goto L11150

        REM *************************************************************~
            *          W R I T E   D A T A   T O   B U F F E R          *~
            *                                                           *~
            * THIS ROUTINE DELETES THE OLD INVOICE FROM THE BUFFER IF   *~
            * THERE WAS ONE.  THEN IT GOES AND CALLS THE ROUTINE THAT   *~
            * WRITES THE NEW ONE OUT THERE.                             *~
            *************************************************************

        datasave
                if group$() = " " then L65000
                if vendor$(1) <> "ALL" then L19140
                   vendor$(1) = all(hex(00))
                   vendor$(2) = all(hex(ff))

L19140:     REM Seek Out Candidate Invoices...
                call "SHOSTAT" ("Invoice Generation In Progress")
                readkey$ = all(hex(00))
                str(readkey$,, 9) = vendor$(1)
                main_loop : call "PLOWNEXT" (#5, readkey$, 0%, f1%(5))
                     if f1%(5) = 0 then clean_up_and_get_out
                if str(readkey$,, 9)>vendor$(2) then clean_up_and_get_out
                if str(readkey$,10,1) < "#" then main_loop
                if str(readkey$,10,1) = "#" and                          ~
                                     str(readkey$,15,11) = " " then L19270
                str(readkey$,15) = all(hex(ff))
                goto main_loop

L19270:     REM Got A Candidate, See If He Qualifies...
                mat search% = zer
                search group$() = str(readkey$,11,4) to search%() step 4
                     if search%(1) = 0 then main_loop
                get #5, str(head$())
                if str(head$(),73,6) < date then invoice_expired

                REM Tweek Data, Toss To Buffer...
                if str(head$(),168,1)<>"R" then main_loop /* make SURE */
                gosub calc_dates
                str(head$(),42,6) =  postdate$
                str(head$(),67,6) =  regulardate$
                str(head$(),73,6) =  discdate$
                str(head$(),93,6) =  date
                str(head$(),99,3) =  userid$
                str(head$(),102,9) = " "
                str(head$(),167,1) = "N"
                str(head$(),168,1) = "N"

                REM Assign Invoice Number
                call "DATEFMT" ( date, 0%, udate$ )
                str(head$(),15,11) = str(udate$,3%,6%) /*Once Invoice Per Day*/
                readkey1$ = str(head$(),,25)
                call "READ100" (#5, readkey1$, f1%(5))
                      if f1%(5) <> 0 then main_loop  /*Could Be Trouble*/
                call "REDALT1" (#9, readkey1$, 1%, f1%(9))
                      if f1%(9) <> 0 then delete #9      /* Shouldn't */
                call "DELETE"  (#10, readkey1$, 25%)     /*  happen  */

                REM Write Out Invoice For Posting...
                marker$((search%(1)+3)/4) = "X"
L19570:         call "GETDTTM" addr (datetime$)
                write #9, using L19600, userid$, datetime$, str(head$()), ~
                                     str(head$(),201,140), eod goto L19570
L19600:         FMT CH(3), CH(7), CH(200), CH(140)

                REM Now Do Lines
                readkey1$ = readkey$
                str(readkey1$,26) = all(hex(00))
L19650:         call "PLOWNEXT" (#6, readkey1$, 25%, f1%(6))
                     if f1%(6) = 0 then main_loop
                get #6, using L19675, line_a$, line_b$, line_c$
L19675:              FMT CH(250), CH(250), CH(41)
                str(line_a$,45,16) = str(head$(),10,16)
                write #10, using L19675, line_a$, line_b$, line_c$
                goto L19650

        invoice_expired
            call "DELETE" (#5, readkey$, 25%)
            call "DELETE" (#6, readkey$, 25%)
            goto main_loop

        clean_up_and_get_out
            for i% = 1 to 5
                if marker$(i%) = " " then L19860
                readkey$ = "APRECGRPS" & group$(i%)
                call "READ101" (#20, readkey$, f1%(20))
                     if f1%(20) = 0% then L19860
                put #20, using L19840, date
L19840:         FMT POS(57), CH(6)
                rewrite #20
L19860:     next i%
            goto L65000

        REM *************************************************************~
            *  S E T   D E F A U L T S   F O R   H E A D E R   I N F O  *~
            *                                                           *~
            * SETS DEFAULTS FOR THE FIRST PAGE OF THE HEADER--ADDRESS,  *~
            * INVOICE DATE, DATE TO PAY WITH AND WITHOUT DISCOUNTS, AND *~
            * THAT SORT OF THING.                                       *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 1
                  message$ = " "
                  on fieldnr% gosub L20180,         /* Vendor Range     */~
                                    L20230,         /* Group Code 1     */~
                                    L20230,         /* Group Code 2     */~
                                    L20230,         /* Group Code 3     */~
                                    L20230,         /* Group Code 4     */~
                                    L20230          /* Group Code 5     */
                return
L20180:     REM ENABLE STUFF FOR VENDOR RANGE.
                if vendor$() = " " then vendor$() = "ALL"
                message$ = "Only Vendors In This Range Will Be Considered~
        ~ For Recurring Invoice Cutover."
                return
L20230:     REM INPUT ENABLE FOR INVOICE NUMBER
                message$ = "Enter Cutover Group(s) To Be Processed.  Ente~
        ~r a Partial Value To Search File."
                return

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
            if startover% = 1% then return

            return clear all
            goto inputmode

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                init(hex(8c)) lfac$()
                pfdescr$(1) = "(1)Start Over     (4)Previous Field       ~
        ~                     (13)Instructions"
                pfdescr$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfdescr$(3) = "                                          ~
        ~                     (16)Exit Program"
                pfkeys$ = hex(0001040d0f10)

                REM Turn off appropriate fields...
                header$ = " "
                if fieldnr% > 1% then L40220
                str(pfdescr$(1),19,19) = " "    /* Shut Off Prev Field */
                str(pfkeys$,3,2) = hex(ffff)
                goto L40420
L40220:         pfdescr$(3) = " "               /* Shut Off Exit       */
                str(pfkeys$,6,1) = hex(ff)
                goto L40420

            deffn'211(fieldnr%)
                REM Editmode logic...
                pfdescr$(1) = "(1)Start Over                             ~
        ~                     (13)Instructions"
                pfdescr$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfdescr$(3) = "                                          ~
        ~                     (16)Process"
                pfkeys$ = hex(00010d0f10)
                header$ = " "
                init(hex(86)) lfac$()
                if fieldnr% = 0% then L40420
                     pfdescr$(3) = " "
                     str(pfkeys$,5,1) = all(hex(ff))
                     init(hex(8c)) lfac$()

L40420:           str(pfdescr$(3),63,1) = hex(84)
                  str(header$,62) = str(manual$) & ": " & cms2v$
                  on fieldnr% gosub L40550,         /* Vendor Range     */~
                                    L40550,         /* Group Code 1     */~
                                    L40550,         /* Group Code 2     */~
                                    L40550,         /* Group Code 3     */~
                                    L40550,         /* Group Code 4     */~
                                    L40550          /* Group Code 5     */
                     goto L40620

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40550:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40620:     accept  "Cutover (Create) Invoices",                         ~
               at (01,02), "Cutover (Create) Invoices",                  ~
               at (01,38), "Post Date: XXXXXXXX  Today's Date: XXXXXXXX",~
               at (01,49), fac(hex(8c)),   paydate$             , ch(08),~
               at (01,73), fac(hex(8c)),   date$                , ch(08),~
               at (02,02), fac(hex(ac)),   header$              , ch(79),~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (06,02), "Vendor Range           thru",                ~
               at (06,15), fac(lfac$(1)), vendor$(1)            , ch(09),~
               at (06,30), fac(lfac$(1)), vendor$(2)            , ch(09),~
               at (08,02), "Select Up To Five Recurring A/P Cutover Group~
        ~s To Process This Run:",                                         ~
               at (10,14), fac(hex(ac)),  subheader$            , ch(56),~
               at (11,14), "1)",                                         ~
               at (11,19), fac(lfac$(2)),  group$(1)            , ch(04),~
               at (11,25), fac(hex(8c)),   descr$(1)            , ch(32),~
               at (11,60), fac(hex(8c)),   ldate$(1)            , ch(08),~
               at (12,14), "2)",                                         ~
               at (12,19), fac(lfac$(3)),  group$(2)            , ch(04),~
               at (12,25), fac(hex(8c)),   descr$(2)            , ch(32),~
               at (12,60), fac(hex(8c)),   ldate$(2)            , ch(08),~
               at (13,14), "3)",                                         ~
               at (13,19), fac(lfac$(4)),  group$(3)            , ch(04),~
               at (13,25), fac(hex(8c)),   descr$(3)            , ch(32),~
               at (13,60), fac(hex(8c)),   ldate$(3)            , ch(08),~
               at (14,14), "4)",                                         ~
               at (14,19), fac(lfac$(5)),  group$(4)            , ch(04),~
               at (14,25), fac(hex(8c)),   descr$(4)            , ch(32),~
               at (14,60), fac(hex(8c)),   ldate$(4)            , ch(08),~
               at (15,14), "5)",                                         ~
               at (15,19), fac(lfac$(6)),  group$(5)            , ch(04),~
               at (15,25), fac(hex(8c)),   descr$(5)            , ch(32),~
               at (15,60), fac(hex(8c)),   ldate$(5)            , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L41090
                  call "MANUAL" (manual$)
                  goto L40620

L41090:        if keyhit% <> 15 then L41130
                  call "PRNTSCRN"
                  goto L40620

L41130:        if fieldnr% <> 0 then return
               close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *         T E S T   D A T A   F R O M   H E A D E R         *~
            *                                                           *~
            * TESTS ALL THE DATA ON THE PAYABLES INVOICE HEADER.        *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50160,         /* Vendor Range     */~
                                    L50270,         /* Group Code 1     */~
                                    L50270,         /* Group Code 2     */~
                                    L50270,         /* Group Code 3     */~
                                    L50270,         /* Group Code 4     */~
                                    L50270          /* Group Code 5     */
                  return

L50160:     REM TEST FOR VENDOR RANGE.
                if vendor$(1) = "ALL" then vendor$(2) = " "
                if vendor$() = "ALL" then return
                if vendor$() = " " then L50240
                if vendor$(2) = " " then vendor$(2) = vendor$(1)
                if vendor$(1) <= vendor$(2) then return
                     errormsg$ = "Starting Vendor Can't Be After Ending."
                     return
L50240:              errormsg$ = "Enter 'ALL' To Select All Vendors."
                     return
                return
L50270:     REM TEST FOR GROUP CODES TO PROCESS
                test% = fieldnr% - 1
                descr$(test%), ldate$(test%) = " "
                if group$(test%) = " " then return
                readkey$ = "APRECGRPS" & group$(test%)
                descr$(test%)=hex(06) & "Select Cutover Group To Process"
                call"PLOWCODE"(#20,readkey$,descr$(test%),9%,.30,f1%(20))
                     if f1%(20) = 1% then L50370
                     errormsg$ = "Unknown Cutover Group Code"
                     return
L50370:         group$(test%) = str(readkey$,10)
                mat search% = zer
                search group$() = group$(test%) to search%() step 4
                     if search%(2) = 0 then L50430
                     errormsg$="Group: "&group$(test%)&" Already Entered"
                     return
L50430:         call "PUTPAREN" (descr$(test%))
                get #20, using L50450, ldate$(test%)
L50450:         FMT POS(57), CH(6)
                call "DATEFMT" (ldate$(test%))
                return

        REM *************************************************************~
            * D I S B U R S E M E N T   D A T E   C O M P U T A T I O N *~
            *                                                           *~
            * THESE TWO SUBROUTINES FIGURE OUT THE TWO DISBURSEMENT     *~
            * DATES FOR THE INVOICE USING THE VARIOUS DEFAULTS.         *~
            *-----------------------------------------------------------*~
            * FOR THE BILLS DUE WITHOUT DISCOUNT FIELD, THERE ARE 3 WAYS*~
            *     1.) IF THE VENDOR'S BILLS DUE FIELD = 0 THEN WE SET   *~
            *         THE DATE TO THE INVOICE DATE + THE SYSTEM DEFAULT *~
            *         BILLS DUE (DAYS) PARAMETER. (THIS IS COMMON...)   *~
            *     2.) IF THE VENDOR'S BILLS DUE FIELD > 0 THEN WE SET   *~
            *         THE DATE TO THE INVOICE DATE + THE VENDOR'S       *~
            *         DEFAULT BILLS DUE (DAYS) FIELD.                   *~
            *     3.) IF THE VENDOR'S BILLS DUE FIELD < 0 THEN WE SET   *~
            *         THE DATE ON THE PROX SYSTEM--DUE MONTH EQUAL TO   *~
            *         MONTH OF INVOICE + 1, CARRYING THE YEAR IF        *~
            *         APPROPRIATE.  THE PROXX DAY IS ALWAYS THE ABSOLUTE*~
            *         VALUE OF THE NUMBER RECALLED FROM THE VENDOR'S    *~
            *         BILLS DUE FIELD.                                  *~
            *-----------------------------------------------------------*~
            * FOR THE BILLS DUE WITH DISCOUNTS FIELD, THERE ARE 3 CASES *~
            *     1.) IF THE DISCOUNTS DUE FIELD = 0 THEN THE DISCOUNT  *~
            *         DATE FOR THE INVOICE GETS A NULL VALUE.           *~
            *     2.) IF THE DISCOUNTS DUE FIELD > 0 THEN THE DISCOUNT  *~
            *         DATE GETS THE INVOICE DATE + THE VENDOR'S DISCOUNT*~
            *         DUE FIELD.                                        *~
            *     3.) IF THE DISCOUNTS DUE FIELD < 0 THEN THE DISCOUNT  *~
            *         DATE IS COMPUTED ON THE "PROX" SYSTEM.  THE       *~
            *         CALCULATION IS AS IN (3) ABOVE.                   *~
            *************************************************************

        calc_dates
            billsdue% = 0
            call "READ100" (#3, str(readkey$,,9), f1%(3))
                if f1%(3) = 0 then L56380  /* ? */
            get #3, using L56360, billsdue%, discsdue%
L56360:     FMT XX(285), 2*PD(14,4)

L56380:     REM Routine that computes regulardate$
                get str(head$(),147,8), using L56400, discpercent
L56400:         FMT PD(14,4)
                date2$, date1$ = postdate$

                on sgn (billsdue%)+2 gosub L56570, L56470, L56530
                   regulardate$ = date2$
                   goto L56720

L56470:         REM Case 1--if bills due = 0 then use system default
                    if sysbillsdue% = 0% then return  /* Can't Happen */
                       billsdue% = sysbillsdue%
                       on sgn (billsdue%)+2 goto L56570,,L56530
                       return

L56530:         REM Case 2--if bills due > 0 then use inv date + days
                    call "DATE" addr("G+",date1$,billsdue%,date2$,err%)
                    return

L56570:         REM Case 3--if bills due < 0 then see explanation above
                    tdate$ = date1$
                    call "DATEFMT" (tdate$, 0%, date1$)
                    convert str(date1$,5%,2%) to month%
                    convert str(date1$,1%,4%) to year%
                    month% = month% + 1%
                    if month% < 13% then L56640      /* CARRY YEAR       */
                       year% = year% + 1%
                       month% = 1%
L56640:             day% = abs(billsdue%)
                    REM Convert dates back to regular date format.
                        convert year%  to str(date2$,1%,4%), pic(0000)
                        convert month% to str(date2$,5%,2%), pic(00)
                        convert day%   to str(date2$,7%,2%), pic(00)
                        call "DATECONV" (date1$)
                        call "DATECONV" (date2$)
                           return


L56720:     REM Routine that computes DISCDATE$
                date1$ = postdate$
                date2$ = blankdate$
                if discpercent = 0 then L56780

                   on sgn (discsdue%)+2 gosub L56910, L56810, L56870
L56780:               discdate$ = date2$
                      return

L56810:         REM Case 1--if bills due = 0 then use system default
                    if sysdiscsdue% = 0% then return
                       discsdue% = sysdiscsdue%
                       on sgn (discsdue%)+2 goto L56910,,L56870
                       return

L56870:         REM Case 2--if discounts due > 0 then use inv date + days
                    call "DATE" addr("G+",date1$,discsdue%,date2$,err%)
                    return

L56910:         REM Case 3--if discounts due < 0 then see explanation
                    tdate$ = date1$
                    call "DATEFMT" (tdate$, 0%, date1$)
                    convert str(date1$,5%,2%) to month%
                    convert str(date1$,1%,4%) to year%
                    month% = month% + 1%
                    if month% < 13% then L56980      /* CARRY YEAR       */
                       year% = year% + 1%
                       month% = 1%
L56980:             day% = abs(discsdue%)
                    REM Convert dates back to regular date format.
                        convert year%  to str(date2$,1%,4%), pic(0000)
                        convert month% to str(date2$,5%,2%), pic(00)
                        convert day%   to str(date2$,7%,2%), pic(00)
                        call "DATECONV" (date1$)
                        call "DATECONV" (date2$)
                           return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM. SET RETURN CODE FOR DOCUMENTS IN BUFFER.    *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")

            REM SET RETURN CODE IF INVOICES FOUND IN BUFFER.
                readkey$ = all(hex(00))
                str(readkey$,,3) = userid$
                call "PLOWNEXT" (#9, readkey$, 3%, f1%(9))
            end  f1%(9)
